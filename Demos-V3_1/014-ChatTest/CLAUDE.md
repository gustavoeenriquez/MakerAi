# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**MakerAI 3.x** - Enterprise Delphi AI Framework providing unified connectivity to multiple LLM providers (Claude, OpenAI GPT, Gemini, Ollama, LM Studio, Groq, DeepSeek, Mistral, Cohere) with RAG, MCP protocol support, autonomous agents, and native capability tools.

**This Demo (014-ChatTest)** - Console application testing core Claude chat functionality including text chat, JSON output, JSON Schema validation, web search, code interpretation, document extraction, vision/image analysis, PDF processing, and extended thinking mode.

## Build & Run

**IDE**: RAD Studio (Delphi 11 Alexandria or newer)

**Compile in IDE**: Open `ClaudeChatTest.dproj`, set target platform (Win32/Win64), build (F9)

**Output**: `.\[Platform]\[Config]\ClaudeChatTest.exe`

**Package Dependencies** (compile in this order from `Source/Packages/`):
1. `MakerAI.dpk` - Core framework
2. `MakerAi.RAG.Drivers.dpk` - RAG drivers
3. `MakerAi.UI.dpk` - UI components
4. `MakerAiDsg.dpk` - Design-time support

**Runtime Requirements**:
- API Key: Set `CLAUDE_API_KEY` environment variable or configure in code
- Network access to `https://api.anthropic.com/v1/`

## Architecture

**Layered Structure**:
```
Connection Layer (TAiChatConnection) - Universal LLM interface, API management
    ↓
Chat Management (TAiChat) - Message history, context, conversation threads
    ↓
Core Capabilities (TAiMediaFile, TAiMessage) - Attachments, message types
    ↓
Tool Framework (ChatTools) - Vision, code execution, web search, file extraction
    ↓
Knowledge Engines (RAGVector, RAGGraph) - Semantic search, knowledge relationships
```

**Key Modules**:
| Module | Path | Purpose |
|--------|------|---------|
| `uMakerAi.Chat.AiConnection` | Source/Chat/ | Main connector for all LLM operations |
| `uMakerAi.Chat.Claude` | Source/Chat/ | Anthropic Claude driver |
| `uMakerAi.Core` | Source/Core/ | Core types (TAiChat, TAiMediaFile, TAiMessage) |
| `uMakerAi.Chat.Tools` | Source/Core/ | Tool/function calling framework |
| `uMakerAi.RAG.Vectors` | Source/RAG/ | Vector embedding and search |
| `uMakerAi.Agents` | Source/Agents/ | Agent orchestration |
| `uMakerAi.MCPServer.Core` | Source/MCPServer/ | MCP protocol server |

## Code Conventions

**Unit Naming**: `uMakerAi.[Module].[SubModule].pas`

**Type Naming**:
- Classes: `TAi[Feature]` (e.g., `TAiChat`, `TAiMediaFile`)
- Interfaces: `IAi[Feature]`
- Events: `TOn[Event]` (e.g., `TOnChatModelChangeEvent`)

**Design Patterns**:
- Factory Pattern: Driver selection via `TAiChatConnection.DriverName`
- Adapter Pattern: Provider-specific implementations
- Component Pattern: `TComponent` inheritance for IDE integration

## Configuration Example

```pascal
procedure ConfigureBase(A: TAiChatConnection);
begin
  A.DriverName := 'Claude';
  A.Model := 'claude-sonnet-4-5-20250929';
  A.Params.Values['ApiKey'] := '@CLAUDE_API_KEY';  // @ prefix reads from env var
  A.Params.Values['Url'] := 'https://api.anthropic.com/v1/';
  A.Params.Values['Max_Tokens'] := '16000';
  A.Params.Values['Temperature'] := '0.7';
  A.Params.Values['ResponseTimeOut'] := '600000';
end;
```

## Demo Test Procedures

The main program includes these test functions that demonstrate different capabilities:
- `Test_Chat1()` - Basic text chat
- `Test_JSON1()` / `Test_JSONSchema1()` - JSON formatted responses
- `Test_WebSearch1()` - Web search integration
- `Test_CodeInterpreter1()` - Code execution
- `Test_ExtractText1()` / `Test_ExtractText2()` - File extraction
- `Test_VisionImage()` - Image analysis
- `Test_VisionPDF()` / `Test_VisionPDFJson()` - PDF processing
- `Test_Thinking()` - Extended thinking mode

## Related Demos

Other demos in `/Demos/` that provide additional context:
- `010-MinimalChat` - Simplest chat implementation
- `012-ChatAllFunctions` - Complete function reference
- `021-RAG+Postgres` / `022-RAG_SQLite` - RAG implementations
- `031-MCPServer` - MCP protocol server
- `051-AgentDemo` - Autonomous agents

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
