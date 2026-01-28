# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Source Directory Overview

This is the main source code directory for MakerAI 3.2, an AI orchestration framework for Delphi. See individual module CLAUDE.md files for detailed guidance on specific subsystems.

## Building

### Package Compilation Order (Critical)
```text
1. Packages/MakerAI.dpk          - Runtime core (~98 units)
2. Packages/MakerAi.RAG.Drivers.dpk  - PostgreSQL connectors
3. Packages/MakerAi.UI.dpk       - FMX visual components
4. Packages/MakerAiDsg.dpk       - Design-time (requires VCL, DesignIDE)
```

Open `Packages/MakerAiGrp.groupproj` to compile all packages.

### Required Library Paths
All subdirectories (Agents, Chat, ChatUI, Core, Design, MCPClient, MCPServer, RAG, Resources, Tools, Utils) must be in Delphi Library Path.

## Module Quick Reference

| Module | Entry Point | Key Classes |
|--------|-------------|-------------|
| Core | `uMakerAi.Chat.pas` | `TAiChat` (abstract base), `TAiMediaFile`, `TAiChatMessage` |
| Chat | `uMakerAi.Chat.AiConnection.pas` | `TAiChatConnection` (universal connector) |
| RAG | `uMakerAi.RAG.Vectors.pas`, `uMakerAi.RAG.Graph.Core.pas` | `TAiVectorStore`, `TAiRagGraph` |
| Agents | `uMakerAi.Agents.pas` | `TAIAgentManager`, `TAIAgentsNode`, `TAIBlackboard` |
| MCPServer | `uMakerAi.MCPServer.Core.pas` | `TAiMCPServer`, `IAiMCPTool` |
| MCPClient | `uMakerAi.MCPClient.Core.pas` | `TMCPClientItem`, transport classes |
| Tools | `uMakerAi.Tools.Functions.pas` | `TAiFunctions`, `TFunctionActionItem` |
| ChatUI | `uMakerAi.UI.ChatList.pas` | `TChatList`, `TChatBubble`, `TChatInput` |

## Architecture Patterns

### Universal Connector
`TAiChatConnection` abstracts all LLM providers. Switch providers by setting `DriverName`:
```pascal
AiConnection.DriverName := 'OpenAI';  // or 'Claude', 'Gemini', 'Ollama', etc.
AiConnection.Model := 'gpt-5.1-turbo';
```

### Driver Registration
New LLM drivers must implement these class methods in `TAiChat` descendant:
```pascal
class function GetDriverName: string; override;
class procedure RegisterDefaultParams(Params: TStrings); override;
class function CreateInstance(Sender: TComponent): TAiChat; override;
```
Register in `uMakerAi.Chat.Initializations.pas`.

### Chat State Machine
All chat implementations follow: `acsIdle → acsConnecting → [acsReasoning] → acsWriting → [acsToolCalling] → acsFinished/acsError`

### Media File Handling
`TAiMediaFile` abstracts files across providers. Load with `LoadFromFile()`, attach to messages via `AddMessageAndRun(prompt, role, [mediaFile])`.

### Parameter Hierarchy
Three levels (lowest to highest priority):
1. Driver defaults (`RegisterDefaultParams`)
2. Model overrides (`TAiChatFactory.RegisterModelParam`)
3. User overrides (`TAiChatFactory.RegisterUserParam`)

Environment variables: Use `@VAR_NAME` syntax (e.g., `@OPENAI_API_KEY`).

## Common Tasks

| Task | Location |
|------|----------|
| Add LLM provider | `Chat/uMakerAi.Chat.*.pas` (inherit `TAiChat`) |
| Add MCP tool | `MCPServer/` (implement `IAiMCPTool`) |
| Add agent tool | `Agents/` (inherit `TAiToolBase`, register in `TEngineRegistry`) |
| Modify function calling | `Tools/uMakerAi.Tools.Functions.pas` |
| Change version/flags | `Core/uMakerAi.Version.inc` |

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for project overview, naming conventions, Delphi version compatibility, thread safety details, troubleshooting, and known issues.

### Module Documentation

| Module | Documentation |
|--------|---------------|
| [Core/](Core/CLAUDE.md) | Foundation classes, TAiChat, TAiMediaFile |
| [Chat/](Chat/CLAUDE.md) | LLM provider drivers |
| [Agents/](Agents/CLAUDE.md) | Agent orchestration framework |
| [RAG/](RAG/CLAUDE.md) | Retrieval-Augmented Generation |
| [MCPServer/](MCPServer/CLAUDE.md) | MCP server implementations |
| [MCPClient/](MCPClient/CLAUDE.md) | MCP client connector |
| [Tools/](Tools/CLAUDE.md) | Function calling, Shell, ComputerUse |
| [ChatUI/](ChatUI/CLAUDE.md) | FMX visual components |
| [Utils/](Utils/CLAUDE.md) | Voice monitor, diff updater |
| [Design/](Design/CLAUDE.md) | Design-time property editors |
| [Resources/](Resources/CLAUDE.md) | Embedded resources |
