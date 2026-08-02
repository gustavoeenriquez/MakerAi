# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

The Agents module implements a graph-based autonomous agent orchestration framework. It enables building workflows where multiple AI-powered nodes execute in sequence or parallel, with conditional routing and shared state management.

## Unit Structure

| Unit | Purpose |
|------|---------|
| `uMakerAi.Agents.pas` | Core framework: `TAIAgentManager`, `TAIAgentsNode`, `TAIAgentsLink`, `TAIBlackboard` |
| `uMakerAi.Agents.Attributes.pas` | RTTI attributes `TToolAttribute`, `TToolParameterAttribute` and `TSecretAttribute` (`[TSecret]`: la propiedad nunca se serializa a disco y los valores entrantes desde JSON se ignoran — credenciales solo en runtime) |
| `uMakerAi.Agents.EngineRegistry.pas` | Singleton registries for tool discovery (`TEngineRegistry`, `TAgentHandlerRegistry`) |
| `uMakerAi.Agents.GraphBuilder.pas` | `TGraphBuilder` parses JSON graph specs into runtime structures. `StrictValidation` (default True) raises `EAiGraphError` on structural defects (edge to missing node, undeclared port, >4 fanout outputs); set False for the legacy warn-and-drop behavior (fix M-02) |
| `uMakerAi.Agents.DmGenerator.pas` | `TDataModuleGenerator` generates Delphi DataModule code from JSON graphs |
| `uMakerAi.A2A.Server.pas` | `TAiA2AServer` (MVP spec A2A 1.0): expone un `TAIAgentManager` como agente A2A — Agent Card en `/.well-known/agent-card.json`, JSON-RPC `SendMessage`/`GetTask`/`CancelTask` (+ aliases 0.x); `esSuspended`→`TASK_STATE_INPUT_REQUIRED`; sin streaming (rechaza con UnsupportedOperationError) |
| `uMakerAi.A2A.Client.pas` | `TAiA2AClient`: consume agentes A2A remotos — `FetchAgentCard`, `SendText` (artifact text + `LastTaskId`/`LastState`), `GetTask`/`CancelTask` |

## Core Classes

**TAIAgentManager** - Orchestrates workflow execution via TThreadPool. Key properties: `StartNode`, `EndNode`, `MaxConcurrentTasks` (default 4). Use `Run(APrompt)` for sync execution, `Compile()` to validate before running. `Compile` is purely structural (validation + InEdges); it does NOT clear the Blackboard — a pre-seeded `Blackboard.AskMsg` survives the first `Run` (fix M-03). For an explicit clean slate between runs call `ResetExecutionState` (clears blackboard, node/link transient state; keeps graph structure).

**TAIAgentsNode** - Workflow vertex. Executes via `OnExecute` callback or attached `Tool: TAiToolBase`. Join modes: `jmAny` (first input triggers), `jmAll` (waits for all inputs).

**TAIAgentsLink** - Directed edge connecting nodes. Four routing modes:
- `lmFanout` - Broadcasts to all NextA/B/C/D slots
- `lmConditional` - Routes based on `Blackboard[ConditionalKey]` value
- `lmManual` - Targets specified programmatically via `ManualTargetsKey`
- `lmExpression` - Evaluates `ExpressionA/B/C/D` bindings against blackboard

**TAIBlackboard** - Thread-safe shared state (TDictionary with TCriticalSection). Standard accessors: `SetString/GetString`, `SetInteger/GetInteger`, `SetBoolean/GetBoolean`. Chat messages via `AskMsg`, `ResMsg` properties.

**TAiToolBase** - Abstract base for node tools. Inherit and implement `Execute(ANode, AInput, var AOutput)`.

**TAiToolParams** - Public RTTI mapper for tool parameters (fix M-04): `ToJSON`/`FromJSON`/`SchemaOf`. Excludes `AI_TOOL_RESERVED_PROPS` (`Name`, `Tag`, `ID`, `Description`) both ways, honors `[TSecret]` (never written; incoming values ignored; `SchemaOf` marks them `writeOnly` + `x-credential-type`). `FromJSON` accepts typed JSON values or their string representation (single mapper — `GraphBuilder.SetToolParameters` delegates here). Serialization notes: `TAIAgentManager.SaveToStream` raises `EAiGraphError` for nodes with `OnExecute` and no `Tool` unless `AllowPartialSerialization := True` (fix M-06).

## Creating Custom Tools

```pascal
uses uMakerAi.Agents, uMakerAi.Agents.Attributes, uMakerAi.Agents.EngineRegistry;

type
  [TToolAttribute('MyTool', 'Does something useful', 'Custom')]
  TMyTool = class(TAiToolBase)
  private
    [TToolParameterAttribute('API Key', 'Your API key', '')]
    FApiKey: string;
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string); override;
  published
    property ApiKey: string read FApiKey write FApiKey;
  end;

initialization
  TEngineRegistry.Instance.RegisterTool(TMyTool, 'uMyToolUnit');
```

## JSON Graph Format

GraphBuilder expects this structure:
```json
{
  "nodes": [
    {
      "id": "node_guid",
      "label": "NodeName",
      "toolClassName": "TMyTool",
      "parameters": { "ApiKey": "xxx" },
      "properties": {
        "engine": { "joinMode": "jmAny", "linkMode": "lmFanout" }
      }
    }
  ],
  "edges": [
    {
      "sourceNodeId": "node1_guid",
      "targetNodeId": "node2_guid",
      "sourceTerminal": "out_a"
    }
  ]
}
```

Port terminals: `out_a`, `out_b`, `out_c`, `out_d`, `out_failure` (maps to NextNo in ALL link modes, including `lmConditional` — fix M-07).

## Reusing a manager across runs

`Compile` validates structure only; it does **not** clear execution state (fix M-03 — clearing there freed the `AskMsg`/`ResMsg` that `Run` seeds beforehand). Since `Compile` also early-exits when `FCompiled` is already `True`, **consecutive `Run(APrompt)` calls on the same instance inherit the previous run's state**: blackboard keys, node `Input`/`Output`/`FError`/`FSuspended`, and link `NoCycles`.

That is the historic behavior and it is preserved. To get a clean run, use the seeding overload:

```pascal
Manager.Run('prompt',
  procedure(B: TAIBlackboard)
  begin
    B.SetString('cliente_id', '42');   // seeded AFTER the reset, so it survives
  end);
```

Guaranteed order: `Compile` -> `ResetExecutionState` -> `ASeed` -> seed `AskMsg`/`ResMsg` -> execute. Seeding before `Run` instead of inside `ASeed` would be wiped by the reset.

`ASeed` may assign `AskMsg` (it is not overwritten if already set). It need not assign `ResMsg` — `Run` always creates a fresh one for the run.

`ResetExecutionState` is also public if you prefer to call it explicitly.

> `TAIBlackboard.SetAskMsg`/`SetResMsg` free the previously stored message before replacing it. Without that, every repeated `Run` leaked the prior `ResMsg`, because `SetValue` only does `AddOrSetValue` and `Clear` was no longer being reached.

## Execution Status

`TAgentExecutionStatus`: `esUnknown`, `esRunning`, `esCompleted`, `esError`, `esTimeout`, `esAborted`, `esSuspended`

Access via `Blackboard.SetStatus()`/`GetStatus()`.

`esSuspended` is set when one or more nodes called `Node.Suspend(...)` during execution. Use `TAIAgentManager.ResumeThread(ThreadID, NodeName, Input)` to resume.

## Durable Execution (Checkpoint / Suspend-Resume)

**Key types** (in `uMakerAi.Agents.Checkpoint`):
- `IAiCheckpointer` — persistence contract; assign to `TAIAgentManager.Checkpointer`
- `TAiNullCheckpointer` — no-op (default behavior, no disk writes)
- `TAiFileCheckpointer` — JSON on disk (`<dir>/<GUID>.checkpoint.json`)
- `TAiCheckpointSnapshot` — serialized state (blackboard + node/link states + pending steps)

**Suspend a node from `OnExecute`:**
```pascal
procedure MyNodeExecute(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
  Input: string; var Output: string);
begin
  Output := Input; // pass-through
  Node.Suspend('Requiere aprobación', 'Contexto adicional');
end;
```

Or use the built-in `TAiWaitApprovalTool` (from `uMakerAi.Agents.Tools.Approval`).

**Resume after suspension:**
```pascal
// Detect suspended threads on app startup
var Threads := AgentManager.GetActiveThreads;
// Resume when user approves
AgentManager.ResumeThread(ThreadID, 'NombreDelNodo', 'Aprobado');
```

**Event for human-in-the-loop UI:**
```pascal
AgentManager.OnSuspend := procedure(Sender: TObject; const ThreadID, NodeName,
  Reason, Context: string) begin
  ShowMessage('Aprobación requerida: ' + Reason);
end;
```

## Threading Model

- Nodes execute in parallel via TThreadPool
- `MaxConcurrentTasks` limits concurrent executions
- TAIBlackboard operations are thread-safe
- Node callbacks (`OnExecute`) run on worker threads - use `TThread.Synchronize` for UI updates

## Dependencies

Internal: `uMakerAi.Chat`, `uMakerAi.Core`, `uMakerAi.Chat.Messages`

Framework: `System.Threading`, `System.Bindings.*`, `System.Rtti`, `System.JSON`, `System.SyncObjs`

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
