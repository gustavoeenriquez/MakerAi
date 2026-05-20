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

- **TAIAgentManager** (`TAIAgents`): Orchestrates agent graph execution. Configured with `StartNode`, `EndNode`, `TimeoutMs`, and events (`OnPrint`, `OnFinish`).

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

### Graph Patterns Demonstrated

1. **Sequential Flow**: `AIGraph1` - StartNode → AiNode → ExecuteNode → EvalNode → EndNode
   - Generates Delphi console code via AI
   - Compiles with DCC64
   - Evaluates compilation result
   - Loops back to AiNode on error (Link4.NextNo)

2. **Parallel Fan-out**: `AIAgentsManager` - NodoInicio → (TareaA || TareaB) → NodoFinal
   - Uses `AIAgentsLink1` with both NextA and NextB set
   - NodoFinal has `JoinMode = jmAll` to wait for both branches

### Key Integration Points

- **TAiOpenChat**: OpenAI chat component for AI interactions
- **TAiPrompts**: Template-based prompt management with variable substitution (`GetTemplate('name', ['var=value'])`)
- **TUtilsSystem**: System utilities for command execution (`RunCommandLine`, `ExecuteCommandLine`)

### Thread Safety

All UI updates from node handlers use `TThread.Synchronize` since agent execution runs asynchronously.

## Dependencies

Part of the MakerAI Suite. Requires:
- Source/Core units (`uMakerAi.Core`, `uMakerAi.Chat`, etc.)
- Source/Agents units
- Delphi 11 Alexandria or later

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
