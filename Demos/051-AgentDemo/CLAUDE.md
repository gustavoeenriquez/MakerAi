# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Delphi FMX demo application showcasing the **MakerAI Agents Framework** (`uMakerAi.Agents`). It demonstrates building AI agent workflows using visual graph-based components.

## Build Commands

```bash
# Compile with Delphi command-line compiler (Windows)
DCC64 AiAgentDemo.dpr

# Or use MSBuild
msbuild AiAgentDemo.dproj /p:Config=Release /p:Platform=Win64
```

## Architecture

### MakerAI Agents Framework Components

The demo uses three core visual components from `uMakerAi.Agents`:

- **TAIAgentManager**: Orchestrates agent graph execution. Configured with `StartNode`, `EndNode`, `TimeoutMs`, and events (`OnPrint`, `OnFinish`).

- **TAIAgentsNode**: Individual processing unit in the graph. Each node has:
  - `OnExecute` handler receiving `(Node, BeforeNode, Link, Input, Output)`
  - `Next` property pointing to a `TAIAgentsLink`
  - `JoinMode` (jmAny/jmAll) for parallel branch convergence
  - `Print()` method for logging

- **TAIAgentsLink**: Edges connecting nodes with routing logic:
  - `NextA`, `NextB`, `NextC`, `NextD`: Target nodes
  - `NextNo`: Fallback when condition fails
  - `Mode`: lmFanout (parallel), lmConditional, lmManual, lmExpression
  - `OnExecute` handler returns `IsOk` to control routing

### Graph Pattern Demonstrated

**Parallel fan-out + join** (`AIAgentsManager`): NodoInicio -> (TareaA || TareaB) -> NodoFinal

- `AIAgentsLink1` has `NextA = TareaA` and `NextB = TareaB`: both branches run in parallel on pool threads.
- **TareaA** calls the model (`AiOpenChat1`, `gpt-6-sol`, `@OPENAI_API_KEY`) with the prompt. Without the variable it says so instead of failing.
- **TareaB** does a local analysis (word and character count) while TareaA waits for the model.
- **NodoFinal** has `JoinMode = jmAll`: it runs once, with both outputs joined, and shows them in `Memo1`.

Cleaned up oct 22/2026: TareaA returned a fixed text (the model call was commented out), the unit carried seven handlers of a sequential code-generation graph that the form no longer has (`AiNodeExecute`, `ExcecuteNodeExecute`, `EvalNodeExecute`, `Link4Execute`...), plus an orphan node and a test label. The model was `gpt-5` (shut down Dec 2026). `TAIAgents` (deprecated alias) became `TAIAgentManager`. The form loads; the button run was not exercised in the automated check.

For a richer graph (fan-out, join, expression, conditional and a `TLLMNode`) see `../098-AgentGraphLadera`.

### Thread Safety

All UI updates from node handlers use `TThread.Synchronize` since agent execution runs asynchronously.

## Dependencies

Part of the MakerAI Suite. Requires:
- Source/Core units (`uMakerAi.Core`, `uMakerAi.Chat`, etc.)
- Source/Agents units
- Delphi 11 Alexandria or later

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
