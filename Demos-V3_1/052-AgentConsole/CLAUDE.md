# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a console demo application for the **MakerAI Agents** framework, showcasing graph-based AI agent orchestration in Delphi. The demo demonstrates two flow patterns:
1. **Conditional Flow (IF/ELSE)**: Branching execution based on decisions stored in the Blackboard
2. **Parallel Flow (Fork/Join)**: Concurrent task execution with synchronization

## Build Commands

Build using Delphi's MSBuild from the command line:

```bash
# Debug build (Win32)
msbuild AgentConsoleDemo.dproj /p:Config=Debug /p:Platform=Win32

# Debug build (Win64)
msbuild AgentConsoleDemo.dproj /p:Config=Debug /p:Platform=Win64

# Release build
msbuild AgentConsoleDemo.dproj /p:Config=Release /p:Platform=Win64
```

Output binaries are placed in `.\$(Platform)\$(Config)\` (e.g., `.\Win64\Debug\AgentConsoleDemo.exe`).

## Architecture

### Core Components (from `uMakerAi.Agents`)

- **`TAIAgentManager`** (alias `TAIAgents`): The graph orchestrator managing nodes, links, and execution. Key methods:
  - `AddNode(Name, ExecuteProc)`: Adds a node with an execution callback
  - `AddEdge(Start, End)`: Creates a simple edge between nodes
  - `AddConditionalEdge(Start, LinkName, Conditions)`: Creates conditional routing
  - `SetEntryPoint(Name)` / `SetFinishPoint(Name)`: Defines flow endpoints
  - `Run(Input)`: Executes the graph

- **`TAIAgentsNode`**: Execution unit with:
  - `OnExecute` callback: `(Node, BeforeNode, Link, Input, var Output)`
  - `JoinMode`: `jmAny` (first input) or `jmAll` (wait for all parallel branches)
  - `Next`: Link to subsequent node(s)

- **`TAIAgentsLink`**: Edge connecting nodes. Supports:
  - `lmFanout`: Parallel execution (NextA, NextB, NextC, NextD)
  - `lmConditional`: Routes based on Blackboard key
  - `lmExpression`: Routes based on expression evaluation

- **`TAIBlackboard`**: Thread-safe shared state dictionary. Use `SetString(key, value)` and `GetString(key)` for conditional routing.

### Demo Flow Patterns

**Conditional Flow** (lines 196-249):
```
Start -> Work -> Decide --(ok)--> OkNode --\
                        \--(fail)--> FailNode --> Finish
```
Decision uses `Blackboard.SetString('next_route', decision)`.

**Parallel Flow** (lines 252-318):
```
Start --ForkLink--> TaskA --JoinLinkA--\
                \-> TaskB --JoinLinkB--> JoinNode (jmAll)
```
Manual link creation for fork/join patterns.

### Key Events

- `OnPrint`: Log output during execution
- `OnEnd`: Called when finish node completes
- `OnError`: Exception handling with abort option
- `OnConfirm`: User confirmation dialogs

### Threading Model

- Graph execution runs asynchronously via `TTask`
- Use `CheckSynchronize` in console apps for `TThread.Synchronize` calls
- `Busy` property indicates active execution

## Dependencies

Requires the MakerAI framework. Add these paths to Library Path:
- `../../Source/Agents`
- `../../Source/Chat`
- `../../Source/Core`

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
