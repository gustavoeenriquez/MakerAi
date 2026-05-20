# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MakerAI is an AI orchestration framework for Delphi developers (v3.4). It provides components for integrating multiple LLM providers (OpenAI, Claude, Gemini, Ollama, Groq, DeepSeek, Kimi, Grok, Mistral, Cohere, LM Studio, GenericLLM), RAG systems (vector and graph-based), MCP servers, autonomous agents, and native ChatTools into Delphi applications. Supports Delphi 10.4 Sydney through 13 Florence (limited: 10.4 Sydney; full support: 11 Alexandria+).

**v3.4 highlights:** registro selectivo de drivers restaurado — `TAiChatConnection` ya no fuerza la carga de todos los providers. Cada driver se auto-registra solo cuando se importa explícitamente. Para cargar todos los drivers de una vez, agregar `uMakerAi.Chat.Initializations` al `uses`.

**v3.3 highlights:** nuevo sistema de orquestación `TAiCapabilities` (`ModelCaps`/`SessionCaps`) que unifica y simplifica la configuración de capacidades por modelo; soporte de modelos actualizado para todos los providers (Feb 2026).

**Official Website:** https://makerai.cimamaker.com

**Language note:** Code comments, variable names, and some documentation are in **Spanish**. When writing new code or comments, follow the existing style of the file being edited (Spanish if the file uses Spanish, English otherwise).

**Git workflow:** `master` is the main/release branch. `dev` is the active development branch. PRs target `master`.

**Testing:** There is no formal test suite or CI/CD pipeline. Testing is done manually via the 18+ demo projects in `Demos/`. When modifying core functionality, verify changes by running relevant demos in the Delphi IDE.

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
- `Source/Embeddings`
- `Source/MCPClient`
- `Source/MCPServer`
- `Source/Packages`
- `Source/RAG`
- `Source/Realtime`
- `Source/Resources`
- `Source/Tools`
- `Source/Utils`
- `Source/WebSocket`

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
- **Claude Citations (RAG nativo)**: Implementación parcial disponible; soporte completo pendiente
- **Gemini Speech cost estimation**: Token/cost tracking not yet implemented
- **Gemini Realtime STT**: `uMakerAi.Realtime.Gemini.pas` — stub implementado, conexión y envío de audio pendientes

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
- `uMakerAi.Core.pas` - Base types: `TAiMediaFile`, `TAiFileCategory`, `TAiChatMediaSupport`, `TAiCapability`, `TAiCapabilities`, chat states
- `uMakerAi.Chat.pas` - Abstract `TAiChat` base class; incluye `ModelCaps`, `SessionCaps`, `EnsureNewSystemConfig`; orquestador interno `RunNew` (privado — no llamar directamente; usar `AddMessageAndRun` o `Run`)
- `uMakerAi.Chat.Messages.pas` - Message handling (`TAiChatMessage`, `TAiChatMessages`)
- `uMakerAi.Embeddings.pas` - Embedding generation

**Chat Drivers (`Source/Chat/`)**: LLM-specific implementations
- `uMakerAi.Chat.AiConnection.pas` - `TAiChatConnection` universal connector (switch providers via `DriverName` property)
- Provider-specific drivers: `TAiOpenChat`, `TAiClaudeChat`, `TAiGeminiChat`, `TAiOllamaChat`, `TAiGroqChat`, `TAiDeepSeekChat`, `TAiKimiChat`, `TAiGrokChat`, `TAiMistralChat`, `TCohereChat`, `TAiLMStudioChat`, `TAiGenericChat`
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

**WebSocket (`Source/WebSocket/`)**: Standalone RFC 6455 WebSocket client (used by Realtime module)
- `uMakerAi.WebSocket.Client.pas` - `TAiWSClient` — pure-Pascal RFC 6455 + HTTP Upgrade + reader thread; uses `ITlsTransport` interface
- `uMakerAi.WebSocket.SChannel.pas` - `TSChannelTransport` — TLS via `secur32.dll` (Windows native, zero extra DLLs; tested ✅)
- `uMakerAi.WebSocket.OpenSSL.pas` - `TOpenSSLTransport` — TLS via `dlopen(libssl.so)` (Linux/macOS; compiles, not yet tested on real hardware)
- `uMakerAi.WebSocket.Android.pas` - `TAndroidSSLTransport` — TLS via `javax.net.ssl.SSLSocketFactory` (JNI; compiles, not yet tested on real hardware)

**Realtime (`Source/Realtime/`)**: Real-time audio streaming (STT via WebSocket)
- `uMakerAi.Realtime.pas` - Abstract base `TAiRealtimeBase` + `TAiRealtimeFactory`; resampler PCM16, VAD modes, thread-safe events
- `uMakerAi.Realtime.AiConnection.pas` - `TAiRealtimeConnection` universal connector (same pattern as `TAiChatConnection`)
- `uMakerAi.Realtime.OpenAI.pas` - `TAiOpenAiRealtimeSTT` — WebSocket to `wss://api.openai.com/v1/realtime`, 24 kHz PCM16; full implementation
- `uMakerAi.Realtime.Gemini.pas` - `TAiGeminiRealtimeSTT` — 16 kHz PCM16; **stub, pendiente implementación**
- `uMakerAi.Realtime.WebSocket.pas` - compatibility shim; re-exports `TAiRealtimeWSClient` → `TAiWSClient` (Source/WebSocket/)

**ChatUI (`Source/ChatUI/`)**: FMX visual components
- `TChatList` - Chat container with markdown rendering
- `TChatInput` - Text/voice/attachment input bar

### Key Design Patterns

**Universal Connector Pattern**: `TAiChatConnection` abstracts provider differences. Set `DriverName` property to switch between providers without code changes.

**Media File Abstraction**: `TAiMediaFile` handles images, PDFs, audio across all providers. Use `TAiFileCategory` to identify file types.

**Chat State Machine**: All chats follow states defined in `TAiChatState` (acsIdle -> acsConnecting -> acsWriting -> acsToolCalling -> acsReasoning -> acsFinished/acsError). Additional states: `acsWaitingForInput` for interactive flows.

**Driver Registration**: Chat drivers register via `uMakerAi.Chat.Initializations.pas` using `TAiChatFactory.Instance.RegisterUserParam()`. Custom drivers inherit from `TAiChat`.

**Model Capabilities Configuration (v3.3)**: Model capabilities are configured per-provider and per-model in `uMakerAi.Chat.Initializations.pas` usando el nuevo sistema unificado:
- `ModelCaps` (`TAiCapabilities`) — capacidades nativas del modelo vía completions (e.g., `[cap_Image, cap_Reasoning]`)
- `SessionCaps` (`TAiCapabilities`) — capacidades deseadas en la sesión; **Gap = SessionCaps − ModelCaps** activa bridges automáticamente
- `ThinkingLevel` — nivel de razonamiento (`tlLow`, `tlMedium`, `tlHigh`)
- `Tool_Active` — habilita function calling

**API eliminada (no usar en `RegisterUserParam`):** `NativeInputFiles`, `NativeOutputFiles`, `ChatMediaSupports`, `EnabledFeatures` fueron eliminados de la API pública. Existen internamente en el engine (calculados automáticamente desde `ModelCaps`/`SessionCaps` vía `SetModelCaps`/`EnsureNewSystemConfig`), pero NO deben usarse directamente.

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
| Configure model capabilities | `Source/Chat/uMakerAi.Chat.Initializations.pas` |
| Modify capability orchestration | `Source/Core/uMakerAi.Chat.pas` (`RunNew` privado, `EnsureNewSystemConfig`) |
| Add TAiCapability value | `Source/Core/uMakerAi.Core.pas` (`TAiCapability` enum) |
| Modify function calling | `Source/Tools/uMakerAi.Tools.Functions.pas` |
| RAG vector operations | `Source/RAG/uMakerAi.RAG.Vectors.pas` |
| RAG graph operations | `Source/RAG/uMakerAi.RAG.Graph.Core.pas` |
| Agent orchestration | `Source/Agents/uMakerAi.Agents.pas` |
| MCP server creation | `Source/MCPServer/uMakerAi.MCPServer.Core.pas` |
| Add Realtime STT provider | `Source/Realtime/uMakerAi.Realtime.pas` (inherit `TAiRealtimeBase`) |
| Realtime universal connector | `Source/Realtime/uMakerAi.Realtime.AiConnection.pas` |
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
| 34.0 | Delphi 10.4 Sydney | Limited (minimum supported) |
| 35.0 | Delphi 11 Alexandria | **Full support** |
| 36.0 | Delphi 12 Athens | **Full support** (primary conditional split) |
| 37.0 | Delphi 13 Florence | Full support (latest tested) |

### Key Conditional Split: CompilerVersion 36

The codebase has 79 conditional compilation directives. The primary split is at CompilerVersion 36 (Delphi 12):

- **< 35**: Uses `uJSONHelper.pas` for missing TJSONObject helper methods
- **>= 35**: Uses native `System.JSON` helpers, `TRESTClient.SynchronizeEvents := False`
- **< 35**: `TMultipartFormData.AddStream` without `AShareOwnership` parameter (Delphi 10.4 only)
- **>= 35**: `TMultipartFormData.AddStream` with `AShareOwnership: Boolean` parameter (Delphi 11+)
- **>= 36**: `TThread.ForceQueue` available (Delphi 12+); use `TThread.Queue` as fallback for D11

```pascal
{$IF CompilerVersion >= 34}  // Delphi 10.4 Sydney+
  Client.SynchronizeEvents := False;
{$IFEND}

{$IF CompilerVersion < 35}
uses uJSONHelper;  // JSON helper for older Delphi versions
{$ENDIF}
```

### Feature Flags (uMakerAi.Version.inc)

All `MAKERAI_HAS_*` feature flags are `True` by default (OpenAI, Whisper, Embeddings, Tool Calling, RAG Vector/Graph, MCP, Chat Connection, UI Components, Agents). Platform flags: Windows, Linux, Mobile are `True`; **macOS is `False`** (incomplete). `MAKERAI_API_LEVEL = 30`.

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
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'Max_Tokens',  '16000');
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'Tool_Active', 'True');
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'ModelCaps',   '[cap_Image]');
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'SessionCaps', '[cap_Image]');
   // Modelo con reasoning:
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'my-model', 'ModelCaps',    '[cap_Image, cap_Reasoning]');
   TAiChatFactory.Instance.RegisterUserParam('ProviderName', 'my-model', 'ThinkingLevel', 'tlMedium');
   ```
4. Add the unit to `MakerAI.dpk` package

## Dependencies

All packages require standard Delphi RTL, RESTComponents, LiveBindings, and Indy. Additional notable dependencies:

- **MakerAi.RAG.Drivers.dpk**: FireDAC (PostgreSQL database access)
- **MakerAi.UI.dpk**: FMX (FireMonkey visual framework)
- **MakerAiDsg.dpk**: VCL + `designide` (design-time IDE integration only)

### Package Dependency Chain

```text
MakerAI.dpk (standalone - no internal deps)
   ^
   |--- MakerAi.RAG.Drivers.dpk (requires MakerAI + FireDAC)
   |--- MakerAi.UI.dpk (requires MakerAI + FMX)
   |--- MakerAiDsg.dpk (requires MakerAI + VCL + DesignIDE)
```

## Environment Variables

API keys use the `@VAR_NAME` convention. When a key property starts with `@`, the framework resolves it via `GetEnvironmentVariable()` at runtime (handled in `TAiChat.ApiKey` getter and `TParamsRegistry`).

**Convention:** Each provider uses `[PROVIDER]_API_KEY` (e.g., `OPENAI_API_KEY`, `CLAUDE_API_KEY`, `GEMINI_API_KEY`, `GROQ_API_KEY`, `DEEPSEEK_API_KEY`, `KIMI_API_KEY`, `GROK_API_KEY`, `MISTRAL_API_KEY`, `COHERE_API_KEY`). `OLLAMA_API_KEY` is usually empty for local setups.

```pascal
AiConnection.ApiKey := '@OPENAI_API_KEY';  // Resolved via GetEnvironmentVariable
AiConnection.ApiKey := 'sk-...';           // Literal key
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

**This project uses a tiered CLAUDE.md system.** Each `Source/` subdirectory and each demo has its own `CLAUDE.md` with module-specific guidance. Read the relevant sub-CLAUDE.md before working on a specific module.

### Source Modules

| Directory | Documentation |
|-----------|---------------|
| [Source/](Source/CLAUDE.md) | Top-level source overview |
| [Source/Core/](Source/Core/CLAUDE.md) | Foundation classes, TAiChat, TAiMediaFile |
| [Source/Chat/](Source/Chat/CLAUDE.md) | LLM provider drivers |
| [Source/Agents/](Source/Agents/CLAUDE.md) | Agent orchestration framework |
| [Source/RAG/](Source/RAG/CLAUDE.md) | Retrieval-Augmented Generation |
| [Source/MCPServer/](Source/MCPServer/CLAUDE.md) | MCP server implementations |
| [Source/MCPClient/](Source/MCPClient/CLAUDE.md) | MCP client connector |
| [Source/Tools/](Source/Tools/CLAUDE.md) | Function calling, Shell, ComputerUse |
| [Source/ChatUI/](Source/ChatUI/CLAUDE.md) | FMX visual components |
| [Source/Realtime/](Source/Realtime/CLAUDE.md) | Real-time STT drivers (OpenAI WebSocket, Gemini stub) |
| [Source/WebSocket/](Source/WebSocket/CLAUDE.md) | RFC 6455 WebSocket client + TLS transports (SChannel/OpenSSL) |
| [Source/Utils/](Source/Utils/CLAUDE.md) | Voice monitor, diff updater |
| [Source/Design/](Source/Design/CLAUDE.md) | Design-time property editors |
| [Source/Resources/](Source/Resources/CLAUDE.md) | Embedded resources |

### Demos & Docs

| Directory | Documentation |
|-----------|---------------|
| [Demos/](Demos/CLAUDE.md) | Demo projects overview |
| [Docs/](Docs/CLAUDE.md) | Documentation index |
