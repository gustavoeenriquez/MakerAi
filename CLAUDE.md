# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**MakerAI Suite FPC Port** — Free Pascal port of the MakerAi Delphi library (v3.3). Provides AI orchestration for FPC applications: multi-provider chat (OpenAI, Claude, Gemini, Ollama, etc.), tool calling, embeddings, RAG (vector + graph), autonomous agents, and MCP protocol.

- **GitHub:** https://github.com/gustavoeenriquez/MakerAi
- **Original Delphi project (reference implementation):** `E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker`
- **Official site:** https://makerai.cimamaker.com
- **License:** MIT

**Language note:** Code comments and some identifiers in the original project are in **Spanish**. Follow the existing style of each file.

## Reference Implementation

Always consult the Delphi original at `E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker` when implementing or porting any feature. It contains:
- `Source/` — 86+ fully implemented `.pas` units
- `Demos/` — 26+ demo projects covering all features
- `Docs/Version 3/` — Detailed documentation (PDF/DOCX) per module
- Tiered `CLAUDE.md` files in every subdirectory with module-specific guidance

The Delphi project's `CLAUDE.md` at the root and in each `Source/*/CLAUDE.md` subdirectory contain authoritative patterns to follow when porting.

## Build & Development

This is a **source library** — no standalone build system. Units are included directly into FPC consumer projects via unit search paths.

**Required `{$mode objfpc}{$H+}` at the top of every unit.**

**Add to FPC project search path:**
```
-Fu<MakerAI_FPC_Path>/Source/Core
-Fu<MakerAI_FPC_Path>/Source/Tools
-Fu<MakerAI_FPC_Path>/Source/Design
-Fu<MakerAI_FPC_Path>/Source/Chat
-Fu<MakerAI_FPC_Path>/Source/Agents
-Fu<MakerAI_FPC_Path>/Source/RAG
-Fu<MakerAI_FPC_Path>/Source/MCPClient
-Fu<MakerAI_FPC_Path>/Source/MCPServer
-Fu<MakerAI_FPC_Path>/Source/Utils
```

**Feature flags** are in `Source/Core/uMakerAi.Version.inc` — include with `{$I uMakerAi.Version.inc}`, never hard-code constants.

**No formal test suite.** Verify changes by running relevant demo applications.

## Architecture

### Layers (mirrors the Delphi original)

```
Application
    ↓
TAiChatConnection (universal connector — switch providers via DriverName)
    ↓
Chat Drivers: TAiOpenChat, TAiClaudeChat, TAiGeminiChat, TAiOllamaChat, ...
    ↓                        (each inherits from TAiChat)
TAiChatMessages / TAiChatMessage     — message history, tool calls, citations
    ↓
Tool System (IAiToolContext)          — Speech, Vision, WebSearch, Code, etc.
    ↓
Core Utilities (uMakerAi.Core.pas)   — TAiMediaFile, MIME types, Base64, HTTP
    ↓
RAG / Agents / MCP                   — higher-level orchestration layers
```

### Key Files (FPC port)

| File | Status | Purpose |
|------|--------|---------|
| `Source/Core/uMakerAi.Core.pas` | Implemented | `TAiMediaFile`, `TAiCapability`, `TAiChatState`, `IAiHttpResponse` |
| `Source/Core/uMakerAi.Chat.Messages.pas` | Implemented | `TAiChatMessage`, `TAiToolsFunction`, citations, token tracking |
| `Source/Core/uMakerAi.Chat.Tools.pas` | Implemented | Tool interfaces, `TAiQueueHelper` (FPC async workaround) |
| `Source/Design/UMakerAi.ParamsRegistry.pas` | Implemented | `TAiChatFactory`, `TAiEmbeddingFactory` singletons |
| `Source/Core/uMakerAi.Utils.CodeExtractor.pas` | Implemented | Extract code blocks from Markdown LLM responses |
| `Source/Core/uMakerAi.Version.inc` | Implemented | Version constants and feature flags |
| `Source/Tools/uMakerAi.Tools.*.pas` | **Stubs** | Shell, ComputerUse, TextEditor, Functions — to be ported |
| `Source/Chat/` | **Empty** | All LLM provider drivers — to be ported |
| `Source/Agents/` | **Empty** | Agent orchestration — to be ported |
| `Source/RAG/` | **Empty** | Vector + Graph RAG — to be ported |
| `Source/MCPClient/` | **Empty** | MCP client — to be ported |
| `Source/MCPServer/` | **Empty** | MCP server — to be ported |

### Design Patterns (from Delphi original, apply same patterns in FPC)

**Universal Connector:** `TAiChatConnection` switches provider at runtime via `DriverName` property. Implemented in `Source/Chat/uMakerAi.Chat.AiConnection.pas` (reference Delphi original).

**Factory + Registry:** `TAiChatFactory.Instance.RegisterUserParam(Provider, Model, Key, Value)` — three-level hierarchy: Driver defaults < Model overrides < User overrides.

**Chat State Machine:** `acsIdle → acsConnecting → [acsReasoning] → acsWriting → [acsToolCalling] → [acsToolExecuting] → acsFinished/acsError`

**Model Capabilities (v3.3):** `ModelCaps` = what model natively supports; `SessionCaps` = what session needs. Gap (`SessionCaps − ModelCaps`) automatically activates tool bridges. Configured per-model in Initializations unit.

**TAiQueueHelper:** FPC replacement for Delphi's `TThread.Queue(nil, procedure begin...end)` anonymous closures. Used throughout Chat drivers for non-blocking main-thread updates.

**Thread Safety:** `TCriticalSection` on `TAiChatMessage` properties; `TAIBlackboard` dictionary; join counters in agent links.

### Agent Framework (to port from Delphi)

Graph-based multi-agent orchestration: `TAIAgentManager` → `TAIAgentsNode` (execution units) → `TAIAgentsLink` (edges with modes: `lmFanout`, `lmConditional`, `lmManual`, `lmExpression`) → `TAIBlackboard` (shared state). Supports human-in-the-loop suspension (`TAiWaitApprovalTool`) and durable execution (`TAiFileCheckpointer`).

### RAG (to port from Delphi)

Two engines: **Vector RAG** (`uMakerAi.RAG.Vectors.pas`) — HNSW + BM25 hybrid search with RRF fusion and reranking + VQL (SQL-like DSL); **Graph RAG** (`uMakerAi.RAG.Graph.Core.pas`) — entity/relationship knowledge graph + GQL (Cypher-like DSL). Both support PostgreSQL drivers.

### MCP (to port from Delphi)

Server: HTTP, SSE, StdIO, Direct transports. `TAiFunctions` bridge auto-exposes function collections as MCP tools. Client: `TMCPClientCustom` consumes external MCP servers. Auth: `@ApiKey` header + `OnValidate` event.

## FPC vs Delphi Translation Table

When porting any unit from the Delphi original, apply these translations:

| Delphi | FPC Equivalent |
|--------|----------------|
| `System.JSON` | `fpjson` + `jsonparser` |
| `System.NetEncoding` (Base64) | `EncdDecd` |
| `System.Net.HttpClient` | `fphttpclient` + `URIParser` |
| `IHTTPResponse` | `IAiHttpResponse` (defined in `uMakerAi.Core.pas`) |
| `TThread.Queue(nil, procedure begin...end)` | `TAiQueueHelper` (in `uMakerAi.Chat.Tools.pas`) |
| `TThread.Queue` / `TThread.Synchronize` with anonymous proc | `TAiQueueHelper` with method pointer |
| `{$IF CompilerVersion >= X}` | `{$IF FPC_FULLVERSION >= X}` |
| `TEncoding.UTF8` | Not needed — FPC native UTF-8 strings |
| `specialize TObjectList<T>` | Must use `specialize` keyword explicitly |
| String helpers (`.StartsWith`, `.IsEmpty`) | Manual: `Copy()`, `Length()` checks |
| `TJson.ObjectToJsonObject` | Manual serialization with `fpjson` |
| `TRESTClient`, `TRESTRequest` | `TFPHTTPClient` |
| `TJSONObject.GetValue<T>('key')` | `TJSONObject.Get('key', default)` / `fpjson` accessors |
| `System.Threading.TThreadPool` | FPC `TThreadPool` or manual thread management |
| `TMonitor.Enter/Exit` | `TCriticalSection.Enter/Leave` |

**FPC-specific:** All generic types require the `specialize` keyword: `specialize TObjectList<TAiMediaFile>`. Anonymous procedures in event handlers require `{$modeswitch nestedprocvars}` or method pointers instead.

## Naming Conventions

Follow the Delphi original exactly:
- Unit prefix: `uMakerAi.` (e.g., `uMakerAi.Chat.OpenAi.pas`)
- Class prefix: `TAi` or `TAI` (e.g., `TAiChat`, `TAIAgentManager`)
- Interface prefix: `IAi` (e.g., `IAiPdfTool`, `IAiVisionTool`)
- Driver naming: `TAi[Provider]Chat` (e.g., `TAiOpenChat`, `TAiClaudeChat`, `TAiGeminiChat`)

## API Keys

Use `@ENV_VAR` convention — resolved via `GetEnvironmentVariable()` at runtime:
```pascal
AiConnection.ApiKey := '@OPENAI_API_KEY';
AiConnection.ApiKey := '@CLAUDE_API_KEY';
AiConnection.ApiKey := '@GEMINI_API_KEY';
```
Standard variable names: `OPENAI_API_KEY`, `CLAUDE_API_KEY`, `GEMINI_API_KEY`, `GROQ_API_KEY`, `DEEPSEEK_API_KEY`, `KIMI_API_KEY`, `GROK_API_KEY`, `MISTRAL_API_KEY`, `COHERE_API_KEY`.

## Key Files Reference (Delphi original — consult when porting)

| Task | Primary File in Delphi original |
|------|--------------------------------|
| Add new LLM provider | `Source/Chat/uMakerAi.Chat.*.pas` (inherit from `TAiChat`) |
| Configure model capabilities | `Source/Chat/uMakerAi.Chat.Initializations.pas` |
| Capability orchestration logic | `Source/Core/uMakerAi.Chat.pas` (`RunNew`, `EnsureNewSystemConfig`) |
| Add `TAiCapability` value | `Source/Core/uMakerAi.Core.pas` |
| Function calling | `Source/Tools/uMakerAi.Tools.Functions.pas` |
| Vector RAG | `Source/RAG/uMakerAi.RAG.Vectors.pas` |
| Graph RAG | `Source/RAG/uMakerAi.RAG.Graph.Core.pas` |
| Agent orchestration | `Source/Agents/uMakerAi.Agents.pas` |
| MCP server | `Source/MCPServer/uMakerAi.MCPServer.Core.pas` |
| Version/feature flags | `Source/Core/uMakerAi.Version.inc` |

## Known Issues (from Delphi original — relevant for FPC port)

- MCP SSE transport is experimental (prefer StdIO or HTTP)
- macOS support incomplete (`MAKERAI_SUPPORT_MACOS = False`)
- Android screen capture not implemented
- Whisper streaming mode pending
- OpenAI async transcription pending
