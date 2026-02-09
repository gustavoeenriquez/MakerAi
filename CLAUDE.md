# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MakerAI is an AI orchestration framework for Delphi developers (v3.2). It provides components for integrating multiple LLM providers (OpenAI, Claude, Gemini, Ollama, Groq, DeepSeek, Kimi, Grok, Mistral, Cohere, LM Studio), RAG systems (vector and graph-based), MCP servers, autonomous agents, and native ChatTools into Delphi applications. Supports Delphi 10.4 Sydney through 13 Florence (full support: 11 Alexandria+).

**Official Website:** https://makerai.cimamaker.com

## Building and Installation

### Package Compilation Order
Compile and install packages in this exact order:
1. `Source/Packages/MakerAI.dpk` - Runtime core (~98 units)
2. `Source/Packages/MakerAi.RAG.Drivers.dpk` - Database connectors (PostgreSQL)
3. `Source/Packages/MakerAi.UI.dpk` - FMX visual components
4. `Source/Packages/MakerAiDsg.dpk` - Design-time editors (requires VCL, DesignIDE)

Group project: `Source/Packages/MakerAiGrp.groupproj`

### Required Library Paths
Add these folders to Delphi Library Path (Tools > Options > Language > Delphi > Library):
- `Source/Agents`
- `Source/Chat`
- `Source/ChatUI`
- `Source/Core`
- `Source/Design`
- `Source/MCPClient`
- `Source/MCPServer`
- `Source/Packages`
- `Source/RAG`
- `Source/Resources`
- `Source/Tools`
- `Source/Utils`

### Running Demos
Open `Demos/DemosVersion31.groupproj` in Delphi IDE to access all demo projects.

### Testing MCP Servers
```bash
# Run demo server
AiMCPServerDemo.exe --protocol sse --port 8080

# Test via curl
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'
```

### Known Issues
- MCP SSE Server is experimental (intermittent connectivity)
- Linux compilation requires manual path adjustments
- macOS support is incomplete (`MAKERAI_SUPPORT_MACOS = False` in version.inc)

### Incomplete Features (code TODOs)
- **Android screen capture**: Not implemented (`uMakerAi.Utils.ScreenCapture.pas`)
- **Whisper streaming mode**: Not yet implemented
- **Async transcription**: OpenAI async transcription mode pending
- **OpenAI Audio streaming events**: `speech.audio.delta` and `transcript.text.delta` parsing not implemented
- **Mistral OCR annotations**: Feature stub present but unimplemented
- **Claude Citations (RAG nativo)**: Future implementation planned for native citation support
- **Gemini Speech cost estimation**: Token/cost tracking not yet implemented

## Architecture

### Overview Diagram

```text
┌─────────────────────────────────────────────────────────────┐
│  Application / Demos                                        │
└──────┬──────────────────┬──────────────────┬────────────────┘
       │                  │                  │
┌──────v──────┐  ┌────────v────────┐  ┌──────v──────────────┐
│  ChatUI     │  │  Agents         │  │  Design-Time (Dsg)  │
│  TChatList  │  │  TAIAgentManager│  │  Property editors   │
│  TChatInput │  │  TAIBlackboard  │  │  VCL + DesignIDE    │
└──────┬──────┘  └────────┬────────┘  └──────┬──────────────┘
       │                  │                  │
┌──────v──────────────────v──────────────────v────────────────┐
│  Chat Drivers                                               │
│  OpenAI│Claude│Gemini│Ollama│Groq│Mistral│DeepSeek│Kimi│...│
│  TAiChatConnection (universal connector via DriverName)     │
└──────────────────────────┬──────────────────────────────────┘
                           │
┌──────────────────────────v──────────────────────────────────┐
│  Core Layer                                                 │
│  TAiChat (abstract) │ TAiMediaFile │ TAiChatMessages        │
└──────┬───────────────────┬──────────────────┬───────────────┘
       │                   │                  │
┌──────v──────┐  ┌─────────v────────┐  ┌──────v──────────────┐
│  Tools      │  │  RAG             │  │  MCP                │
│  Functions  │  │  Vectors + Graph │  │  Client + Server    │
│  Shell      │  │  VQL + GQL       │  │  StdIO/HTTP/SSE     │
└─────────────┘  └──────────────────┘  └─────────────────────┘
```

### Core Layers

**Core (`Source/Core/`)**: Foundation classes
- `uMakerAi.Core.pas` - Base types: `TAiMediaFile`, `TAiFileCategory`, `TAiChatMediaSupport`, chat states
- `uMakerAi.Chat.pas` - Abstract `TAiChat` base class for all LLM drivers
- `uMakerAi.Chat.Messages.pas` - Message handling (`TAiChatMessage`, `TAiChatMessages`)
- `uMakerAi.Embeddings.pas` - Embedding generation

**Chat Drivers (`Source/Chat/`)**: LLM-specific implementations
- `uMakerAi.Chat.AiConnection.pas` - `TAiChatConnection` universal connector (switch providers via `DriverName` property)
- Provider-specific drivers: `TAiOpenChat`, `TAiClaudeChat`, `TAiGeminiChat`, `TAiOllamaChat`, `TAiGroqChat`, `TAiDeepSeekChat`, `TAiKimiChat`, `TAiGrokChat`, `TAiMistralChat`, `TCohereChat`, `TAiLMStudioChat`
- `uMakerAi.Chat.Initializations.pas` - Driver registration and default model parameters via `TAiChatFactory`

**Agents (`Source/Agents/`)**: Autonomous agent framework
- `uMakerAi.Agents.pas` - Graph-based agent orchestration with `TAIAgentManager`, `TAIAgentsNode`, `TAIAgentsLink`
- `TAIBlackboard` - Shared state between agent nodes
- Link modes: `lmFanout`, `lmConditional`, `lmManual`, `lmExpression`

**RAG (`Source/RAG/`)**: Retrieval-Augmented Generation
- `uMakerAi.RAG.Vectors.pas` - Vector-based RAG with reranking
- `uMakerAi.RAG.Graph.Core.pas` - Knowledge graph RAG engine
- `uMakerAi.RAG.Graph.GQL.pas` - Graph query language
- `uMakerAi.RAG.Vectors.VQL.pas` - Vector query language

**MCP (`Source/MCPClient/`, `Source/MCPServer/`)**: Model Context Protocol
- `uMakerAi.MCPClient.Core.pas` - MCP client for consuming external servers
- Server implementations: StdIO, HTTP, SSE, Direct

**Tools (`Source/Tools/`)**: Capabilities framework
- `uMakerAi.Tools.Functions.pas` - Function calling system (`TAiFunctions`, `TAiToolsFunction`)
- Provider tools: Whisper (STT), DALL-E, Sora (video), Veo (video via Gemini)
- Agent tools: `TAiShell`, `TAiTextEditorTool`, `TAiComputerUseTool`
- Tool interfaces: `IAiPdfTool`, `IAiVisionTool`, `IAiSpeechTool`, `IAiWebSearchTool`

**ChatUI (`Source/ChatUI/`)**: FMX visual components
- `TChatList` - Chat container with markdown rendering
- `TChatInput` - Text/voice/attachment input bar

### Key Design Patterns

**Universal Connector Pattern**: `TAiChatConnection` abstracts provider differences. Set `DriverName` property to switch between providers without code changes.

**Media File Abstraction**: `TAiMediaFile` handles images, PDFs, audio across all providers. Use `TAiFileCategory` to identify file types.

**Chat State Machine**: All chats follow states defined in `TAiChatState` (acsIdle -> acsConnecting -> acsWriting -> acsToolCalling -> acsReasoning -> acsFinished/acsError). Additional states: `acsWaitingForInput` for interactive flows.

**Driver Registration**: Chat drivers register via `uMakerAi.Chat.Initializations.pas` using `TAiChatFactory.Instance.RegisterUserParam()`. Custom drivers inherit from `TAiChat`.

**Model Capabilities Configuration**: Model capabilities (vision, tools, reasoning) are configured per-provider and per-model in `uMakerAi.Chat.Initializations.pas` using:
- `NativeInputFiles` - File types the model accepts natively (e.g., `[Tfc_Image]`)
- `ChatMediaSupports` - Logical capabilities (e.g., `[Tcm_Text, Tcm_Image, Tcm_Reasoning]`)
- `EnabledFeatures` - Features to enable for the model

### Agent Graph Execution Model

Agents use a directed graph with nodes (`TAIAgentsNode`) and links (`TAIAgentsLink`):
- **Link modes**: `lmFanout` (parallel), `lmConditional` (if/else), `lmManual`, `lmExpression` (binding expressions)
- **Join modes**: `jmAny` (first arrival wins), `jmAll` (wait for all inputs)
- **Execution status**: `esRunning`, `esCompleted`, `esError`, `esTimeout`, `esAborted`
- Nodes communicate via `TAIBlackboard` shared state dictionary

## Key Files Reference

| Task | Primary File |
|------|-------------|
| Add new LLM provider | `Source/Chat/uMakerAi.Chat.*.pas` (inherit from `TAiChat`) |
| Modify function calling | `Source/Tools/uMakerAi.Tools.Functions.pas` |
| RAG vector operations | `Source/RAG/uMakerAi.RAG.Vectors.pas` |
| RAG graph operations | `Source/RAG/uMakerAi.RAG.Graph.Core.pas` |
| Agent orchestration | `Source/Agents/uMakerAi.Agents.pas` |
| MCP server creation | `Source/MCPServer/uMakerAi.MCPServer.Core.pas` |
| Version/feature flags | `Source/Core/uMakerAi.Version.inc` |

## Naming Conventions

- Unit prefix: `uMakerAi.` (e.g., `uMakerAi.Chat.OpenAi.pas`)
- Class prefix: `TAi` or `TAI` (e.g., `TAiChat`, `TAIAgentManager`)
- Interface prefix: `IAi` (e.g., `IAiPdfTool`, `IAiVisionTool`)
- Driver naming: `TAi[Provider]Chat` (e.g., `TAiOpenChat`, `TAiClaudeChat`, `TAiGeminiChat`). Exception: `TCohereChat` does not follow this convention.

## Delphi Version Compatibility

### Compiler Version Reference

| CompilerVersion | Delphi Version | Support Level |
|-----------------|----------------|---------------|
| 34.0 | Delphi 10.4 Sydney | Minimum supported |
| 35.0 | Delphi 11 Alexandria | **Full support** (primary split point) |
| 36.0 | Delphi 12 Athens | Full support |
| 37.0 | Delphi 13 Florence | Latest tested |

### Key Conditional Split: CompilerVersion 35

The codebase has 79 conditional compilation directives. The primary split is at CompilerVersion 35 (Delphi 11):

- **< 35**: Uses `uJSONHelper.pas` for missing TJSONObject helper methods
- **>= 35**: Uses native `System.JSON` helpers, `TRESTClient.SynchronizeEvents := False`

```pascal
{$IF CompilerVersion >= 34}  // Delphi 10.4 Sydney+
  Client.SynchronizeEvents := False;
{$IFEND}

{$IF CompilerVersion < 35}
uses uJSONHelper;  // JSON helper for older Delphi versions
{$ENDIF}
```

### Feature Flags (uMakerAi.Version.inc)

| Flag | Value | Description |
|------|-------|-------------|
| `MAKERAI_HAS_OPENAI` | True | OpenAI provider support |
| `MAKERAI_HAS_WHISPER` | True | Speech-to-text |
| `MAKERAI_HAS_EMBEDDINGS` | True | Embedding generation |
| `MAKERAI_HAS_TOOL_CALLING` | True | Function calling framework |
| `MAKERAI_HAS_RAG_VECTOR` | True | Vector-based RAG |
| `MAKERAI_HAS_RAG_GRAPH` | True | Graph-based RAG |
| `MAKERAI_HAS_MCP` | True | Model Context Protocol |
| `MAKERAI_HAS_CHAT_CONNECTION` | True | TAiChatConnection universal connector |
| `MAKERAI_HAS_UI_COMPONENTS` | True | FMX visual chat components |
| `MAKERAI_HAS_AGENTS` | True | Agent orchestration |
| `MAKERAI_SUPPORT_WINDOWS` | True | Windows platform |
| `MAKERAI_SUPPORT_LINUX` | True | Linux platform |
| `MAKERAI_SUPPORT_MACOS` | **False** | macOS (incomplete) |
| `MAKERAI_SUPPORT_MOBILE` | True | Mobile (Android/iOS) |
| `MAKERAI_API_LEVEL` | 30 | Internal API compatibility level |

## Thread Safety Notes

### Critical Sections (TCriticalSection)

| Lock | Class | Protects |
|------|-------|----------|
| `FLock` | `TAiChatMessage` | Media file list operations |
| `FLock` | `TAIBlackboard` | Shared state dictionary |
| `FJoinLock` | `TAIAgentsLink` | Join counter for multi-input nodes |
| `FActiveTasksLock` | `TAIAgentManager` | Active task count tracking |
| `FOutputLock` | `TAiMCPServerStdio` | Stdout writes from multiple threads |
| `FSessionsLock` | `TAiMCPServerSSE` | SSE session list |
| `FCS` | `TAiVoiceMonitor` | Audio capture state |

### Thread Queuing

- `TThread.Queue(nil, proc)` -- used by all Chat drivers and UI components for non-blocking main-thread updates (40+ call sites)
- `TThread.Synchronize(nil, proc)` -- used by Agents (node callbacks) and Gemini live streaming for blocking synchronization
- `TThreadedQueue<TJSONObject>` -- async message buffering in MCP Client
- `TThreadedQueue<string>` -- SSE outbox in MCP Server

### Thread Pools

- `TAIAgentManager.FThreadPool: TThreadPool` -- executes agent node callbacks on worker threads
- Agent `OnExecute` handlers run on pool threads; use `TThread.Synchronize` to access UI

### Gemini Live Streaming

`TMonitor.Enter/Exit(FPendingScreenshots)` protects a shared screenshot list during Gemini's real-time video streaming.

## Adding a New LLM Provider

1. Create `Source/Chat/uMakerAi.Chat.[ProviderName].pas` inheriting from `TAiChat`
2. Implement required abstract methods for API communication
3. Register the driver in `uMakerAi.Chat.Initializations.pas`:
   ```pascal
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'Max_Tokens', '8000');
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'NativeInputFiles', '[Tfc_Image]');
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'ChatMediaSupports', '[Tcm_Text, Tcm_Image]');
   ```
4. Add the unit to `MakerAI.dpk` package

## Dependencies

### External Delphi Packages Required

| Dependency | Used By | Purpose |
|------------|---------|---------|
| `rtl` | All packages | Delphi runtime library |
| `RESTComponents` | All packages | HTTP REST client (TRESTClient, TRESTRequest) |
| `bindengine`, `bindcomp` | All packages | LiveBindings engine |
| `IndySystem`, `IndyCore`, `IndyProtocols` | MakerAI.dpk | Indy HTTP/TCP for MCP StdIO and streaming |
| `inet` | MakerAI.dpk | Internet utilities |
| `xmlrtl` | MakerAI.dpk, RAG.Drivers | XML parsing |
| `fmx` | MakerAi.UI.dpk | FireMonkey framework (UI components) |
| `FireDAC`, `FireDACCommon`, `FireDACCommonDriver` | MakerAi.RAG.Drivers.dpk | Database access for PostgreSQL RAG |
| `dbrtl` | RAG.Drivers, UI, Dsg | Database runtime |
| `vcl` | MakerAiDsg.dpk | VCL framework (design-time editors only) |
| `designide` | MakerAiDsg.dpk | Delphi IDE design-time integration |

### Package Dependency Chain

```text
MakerAI.dpk (standalone - no internal deps)
   ^
   |--- MakerAi.RAG.Drivers.dpk (requires MakerAI + FireDAC)
   |--- MakerAi.UI.dpk (requires MakerAI + FMX)
   |--- MakerAiDsg.dpk (requires MakerAI + VCL + DesignIDE)
```

## Environment Variables

API keys use the `@VAR_NAME` convention. When an API key property starts with `@`, the framework resolves it via `GetEnvironmentVariable()` at runtime. This is handled automatically in `TAiChat.ApiKey` getter and in the design-time `TParamsRegistry`.

### Required Variables by Provider

| Variable | Provider | Notes |
|----------|----------|-------|
| `OPENAI_API_KEY` | OpenAI (GPT, Whisper, DALL-E, Sora, Audio) | Default for all OpenAI endpoints |
| `CLAUDE_API_KEY` | Anthropic Claude | |
| `GEMINI_API_KEY` | Google Gemini (Chat, Video/Veo, Speech, WebSearch) | |
| `OLLAMA_API_KEY` | Ollama (local) | Usually empty for local setups |
| `GROQ_API_KEY` | Groq | |
| `DEEPSEEK_API_KEY` | DeepSeek | |
| `KIMI_API_KEY` | Kimi/Moonshot | |
| `GROK_API_KEY` | xAI Grok | |
| `MISTRAL_API_KEY` | Mistral AI | |
| `COHERE_API_KEY` | Cohere | |

### Usage

```pascal
// Automatic: set @-prefixed key, resolved at runtime
AiConnection.ApiKey := '@OPENAI_API_KEY';

// Manual: pass literal key directly
AiConnection.ApiKey := 'sk-...';
```

## Error Handling

### Custom Exception Classes

| Exception | Module | When Raised |
|-----------|--------|-------------|
| `EVGQLParserError` | RAG/VQL | Invalid VQL syntax (lexer/parser errors) |
| `EVGQLTranslationError` | RAG/VQL | VQL-to-SQL compilation failures |
| `EMCPClientException` | MCPClient | MCP transport or protocol errors |
| `EPythonArchitectureError` | Utils | Python architecture mismatch (x86 vs x64) |

### Error Patterns

- **Chat state machine**: Errors transition to `acsError` state; handle via `OnStateChange` event
- **Agent graph**: Invalid graph structure (missing nodes, circular deps) raises `Exception` during `Validate()`
- **API errors**: HTTP errors from providers are caught in `try/except` and surfaced via `OnError` events
- Most validation uses `Exception.Create(msg)` or `Exception.CreateFmt(msg, [args])` -- catch with `on E: Exception do`

## Component Selection Guide

### TAiChat vs TAiChatConnection

| Component | Use When | Avoid When |
|-----------|----------|------------|
| `TAiChatConnection` | Provider-agnostic code, switching providers at runtime via `DriverName` | You need provider-specific API features |
| `TAi[Provider]Chat` | Direct access to provider-specific features, fine-grained control | You want portable/switchable code |

### RAG: Vector vs Graph

| Approach | Use When | Avoid When |
|----------|----------|------------|
| `TAiRAGVector` (Vector RAG) | Document Q&A, similarity search, semantic retrieval | Relationship-heavy data, traversal queries |
| RAG Graph (`uMakerAi.RAG.Graph.Core`) | Knowledge graphs, entity relationships, traversal queries | Simple document similarity search |

### MCP Server Transports

| Transport | Use When | Protocol |
|-----------|----------|----------|
| StdIO | IDE integration, Claude Desktop, local tools | stdin/stdout |
| HTTP | REST API integration, request/response pattern | HTTP POST |
| SSE | Streaming responses, real-time updates (experimental) | Server-Sent Events |
| Direct | In-process usage, no IPC overhead | Direct method calls |

## Troubleshooting

**Package installation fails with BPL version conflict**

1. Close all Delphi instances
2. Delete existing BPL files from Delphi's BPL output directory
3. Recompile packages in the correct order (MakerAI -> RAG.Drivers -> UI -> Dsg)

**"Unit not found" compilation errors**

Verify all 12 library paths are added to Delphi Library Path (see [Required Library Paths](#required-library-paths)).

**API key not resolving**

Ensure the environment variable is set system-wide (not just in the current terminal). The `@VAR_NAME` syntax requires `GetEnvironmentVariable()` to find the value at runtime.

**JSON parsing errors on older Delphi versions**

Delphi < 11 (CompilerVersion < 35) requires `uJSONHelper.pas` in the uses clause. This is handled automatically via conditional compilation, but verify the unit is accessible in the library path.

**MCP SSE Server intermittent connectivity**

The SSE transport is experimental. For production use, prefer StdIO or HTTP transport.

## External Documentation

Detailed documentation is available in `Docs/Version 3/`:

| Document | Description |
|----------|-------------|
| `Manual Instalacion.docx` | Installation guide |
| `uMakerAi-ChatConnection.docx` | Complete TAiChatConnection guide with examples |
| `uMakerAi.Chat.docx` | Core chat system reference |
| `TAiChatClaude-Guia de Uso.docx` | Claude provider-specific guide |
| `TAiChatGemini-Guia de Uso.docx` | Gemini provider-specific guide |
| `uMakerAi-RAG.docx` | RAG system documentation |
| `uMakerAI-RAGGraph.docx` | Graph RAG documentation |
| `uMakerAi.ToolFuncions.docx` | Function calling / Tools documentation |
| `TAiAgentes-FLUENT GRAPH BUILDER.docx` | Agent graph builder guide |
| `uMakerAi-MCP.Server.EN.pdf` | MCP Server guide (English) |
| `uMakerAi-MCP.Server.ES.pdf` | MCP Server guide (Spanish) |
| `uMakerAi-Agents.ES.pdf` | Agents documentation (Spanish) |
| `uMakerAi-RAG.ES.pdf` | RAG documentation (Spanish) |

## Navigation

### Source Modules

| Directory | Documentation |
|-----------|---------------|
| [Source/](Source/CLAUDE.md) | Top-level source overview |
| [Source/Compatibility/](Source/Compatibility/CLAUDE.md) | Cross-platform compatibility layer (Delphi/FPC) |
| [Source/Core/](Source/Core/CLAUDE.md) | Foundation classes, TAiChat, TAiMediaFile |
| [Source/Chat/](Source/Chat/CLAUDE.md) | LLM provider drivers |
| [Source/Agents/](Source/Agents/CLAUDE.md) | Agent orchestration framework |
| [Source/RAG/](Source/RAG/CLAUDE.md) | Retrieval-Augmented Generation |
| [Source/MCPServer/](Source/MCPServer/CLAUDE.md) | MCP server implementations |
| [Source/MCPClient/](Source/MCPClient/CLAUDE.md) | MCP client connector |
| [Source/Tools/](Source/Tools/CLAUDE.md) | Function calling, Shell, ComputerUse |
| [Source/ChatUI/](Source/ChatUI/CLAUDE.md) | FMX visual components |
| [Source/Utils/](Source/Utils/CLAUDE.md) | Voice monitor, diff updater |
| [Source/Design/](Source/Design/CLAUDE.md) | Design-time property editors |
| [Source/Resources/](Source/Resources/CLAUDE.md) | Embedded resources |

### Config & Docs

| Directory | Documentation |
|-----------|---------------|
| [Config/](Config/CLAUDE.md) | Environment configuration (.env) |
| [Demos/](Demos/CLAUDE.md) | Demo projects overview |
| [Docs/](Docs/CLAUDE.md) | Documentation index |
