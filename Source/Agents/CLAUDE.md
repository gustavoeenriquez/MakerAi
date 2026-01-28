# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

The Agents module implements a graph-based autonomous agent orchestration framework. It enables building workflows where multiple AI-powered nodes execute in sequence or parallel, with conditional routing and shared state management.

## Unit Structure

| Unit | Purpose |
|------|---------|
| `uMakerAi.Agents.pas` | Core framework: `TAIAgentManager`, `TAIAgentsNode`, `TAIAgentsLink`, `TAIBlackboard` |
| `uMakerAi.Agents.Attributes.pas` | RTTI attributes `TToolAttribute` and `TToolParameterAttribute` for tool metadata |
| `uMakerAi.Agents.EngineRegistry.pas` | Singleton registries for tool discovery (`TEngineRegistry`, `TAgentHandlerRegistry`) |
| `uMakerAi.Agents.GraphBuilder.pas` | `TGraphBuilder` parses JSON graph specs into runtime structures |
| `uMakerAi.Agents.DmGenerator.pas` | `TDataModuleGenerator` generates Delphi DataModule code from JSON graphs |

## Core Classes

**TAIAgentManager** - Orchestrates workflow execution via TThreadPool. Key properties: `StartNode`, `EndNode`, `MaxConcurrentTasks` (default 4). Use `Run(APrompt)` for sync execution, `Compile()` to validate before running.

**TAIAgentsNode** - Workflow vertex. Executes via `OnExecute` callback or attached `Tool: TAiToolBase`. Join modes: `jmAny` (first input triggers), `jmAll` (waits for all inputs).

**TAIAgentsLink** - Directed edge connecting nodes. Four routing modes:
- `lmFanout` - Broadcasts to all NextA/B/C/D slots
- `lmConditional` - Routes based on `Blackboard[ConditionalKey]` value
- `lmManual` - Targets specified programmatically via `ManualTargetsKey`
- `lmExpression` - Evaluates `ExpressionA/B/C/D` bindings against blackboard

**TAIBlackboard** - Thread-safe shared state (TDictionary with TCriticalSection). Standard accessors: `SetString/GetString`, `SetInteger/GetInteger`, `SetBoolean/GetBoolean`. Chat messages via `AskMsg`, `ResMsg` properties.

**TAiToolBase** - Abstract base for node tools. Inherit and implement `Execute(ANode, AInput, var AOutput)`.

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

Port terminals: `out_a`, `out_b`, `out_c`, `out_d`, `out_failure` (maps to NextNo).

## Execution Status

`TAgentExecutionStatus`: `esUnknown`, `esRunning`, `esCompleted`, `esError`, `esTimeout`, `esAborted`

Access via `Blackboard.SetStatus()`/`GetStatus()`.

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
