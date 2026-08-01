# MakerAI Suite v3.4 — The AI Ecosystem for Delphi

🌐 **Official Website:** [https://makerai.cimamaker.com](https://makerai.cimamaker.com)
📖 **Manual:** [https://www.gustavoenriquez.com/book-makerai](https://www.gustavoenriquez.com/book-makerai) — available in English and Spanish

[![GitHub Stars](https://img.shields.io/github/stars/gustavoeenriquez/MakerAi?style=social)](https://github.com/gustavoeenriquez/MakerAi)
[![GitHub Issues](https://img.shields.io/github/issues/gustavoeenriquez/MakerAi)](https://github.com/gustavoeenriquez/MakerAi/issues)
[![License](https://img.shields.io/github/license/gustavoeenriquez/MakerAi)](LICENSE.txt)
[![Telegram](https://img.shields.io/badge/Join-Telegram%20Chat-blue.svg)](https://t.me/+7LaihFwqgsk1ZjQx)
[![Delphi Supported Versions](https://img.shields.io/badge/Delphi%20Support-11%20Alexandria%20to%2013%20Florence-blue.svg)](https://www.embarcadero.com/products/delphi)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2%2B-orange.svg)](https://github.com/gustavoeenriquez/MakerAi/tree/fpc)

> **Free Pascal / Lazarus port available** — Full port of MakerAI Suite for FPC 3.2+ (12 LLM drivers, RAG, Agents, MCP, Embeddings). See the [`fpc` branch](https://github.com/gustavoeenriquez/MakerAi/tree/fpc).

---

## MakerAI is more than an API wrapper

Most AI libraries for Delphi stop at wrapping REST calls. **MakerAI is different.**

Yes, MakerAI includes **native, provider-specific components** that give you direct, full-fidelity access to each provider's API — every model parameter, every response field, every streaming event, exactly as the provider defines it.

But on top of that, MakerAI is a **complete AI application ecosystem** that lets you build production-grade intelligent systems entirely in Delphi:

- **RAG pipelines** (vector and graph-based) with SQL-like query languages (VQL / GQL)
- **Autonomous Agents** with graph orchestration, checkpoints, and human-in-the-loop approval
- **MCP Servers and Clients** — expose or consume tools using the Model Context Protocol
- **Native ChatTools** — bridge AI reasoning with deterministic real-world capabilities (PDF, Vision, Speech, Web Search, Shell, Computer Use)
- **FMX Visual Components** — drop-in UI for multimodal chat interfaces
- **Universal Connector** — switch providers at runtime without changing your application code

Whether you need a simple one-provider integration or a multi-agent, multi-provider, retrieval-augmented production system, MakerAI covers the full stack — **natively in Delphi**.

---

## 🚀 What's New in v3.4

### Delphi 13.1 Florence Support

v3.4 is fully tested and compatible with **Delphi 13.1 Florence** (CompilerVersion 37.1), in addition to the existing range from Delphi 10.4 Sydney through Delphi 13 Florence.

### Selective Driver Registration

The biggest infrastructure change in v3.4: **`TAiChatConnection` no longer force-loads all providers at startup**. Each driver now self-registers only when explicitly imported, eliminating unnecessary initialization overhead:

```pascal
// Load only what you need
uses uMakerAi.Chat.AiConnection, uMakerAi.Chat.OpenAi, uMakerAi.Chat.Claude;

// Load all drivers at once (legacy behavior)
uses uMakerAi.Chat.Initializations;
```

### Real-Time STT — TAiRealtimeConnection

New universal connector for real-time speech-to-text via WebSocket:

- **`TAiRealtimeConnection`** — provider-agnostic STT connector; switch providers via `DriverName`
- **`TAiOpenAiRealtimeSTT`** — full OpenAI Realtime API implementation (24 kHz PCM16, VAD modes, streaming transcription)
- Pure-Pascal WebSocket client with native TLS via Windows SChannel — no extra DLLs required
- Thread-safe PCM16 resampler; supports push-based audio streaming from any source

### GPT-Transcribe — Next-Gen OpenAI Transcription (Whisper successors) 🆕

OpenAI's new transcription models (Aug 2026) are fully integrated — better accuracy on real-world audio, accents, numbers, specialized terminology and loud background noise:

| Model | Use case | Word Error Rate |
|-------|----------|-----------------|
| `gpt-live-transcribe` | Live low-latency STT (Realtime WebSocket) | 9.60% (vs 11.65% Whisper) |
| `gpt-transcribe` | Completed files and batch workloads | 8.98% (vs 15.21% Whisper) |

- **`TAiOpenAiRealtimeSTT`** now defaults to `gpt-live-transcribe`, with new context properties: `TranscriptionPrompt` (free-form topic), `TranscriptionKeywords` (domain terms), `Languages` (multi-language guided autodetection) and `LowDelay`
- **`TAiOpenAiAudio`** gains `tmGptTranscribe` / `tmGptLiveTranscribe` with `TranscriptionKeywords` + `TranscriptionLanguages` for REST/batch transcription
- Legacy models (`whisper-1`, `gpt-4o-transcribe`) remain available — they're still required for subtitles (SRT/VTT), word timestamps and diarization (`gpt-4o-transcribe-diarize`), which the new models don't support; the components degrade formats safely per model
- VoiceBridge demos (062–065) migrated: live channels use `gpt-live-transcribe` with contextual prompts and guided language detection; diarized channels stay on `gpt-4o-transcribe-diarize`

### Grok Voice — Real-Time Speech-to-Speech (xAI) 🆕

Full-duplex voice conversation with xAI's **Grok Voice** models (`grok-voice-think-fast-2.0`) over a single WebSocket — the user speaks, Grok listens, reasons and answers back with voice:

- **`TAiGrokRealtimeChat`** — complete driver for `wss://api.x.ai/v1/realtime` (OpenAI Realtime-compatible protocol, 24 kHz PCM16)
- **`TAiRealtimeVoiceBase`** — new base class for full-duplex voice drivers; adds `OnAssistantText`, `OnAssistantTextDelta`, `OnAudioChunk`, `OnAudioDone` (shared with `TAiMakerAiRealtimeChat`)
- Live user transcription (`OnTranscriptDelta` / `OnTranscriptCompleted`), server VAD, streamed assistant text and TTS audio
- **Function calling by voice**: assign a `TAiFunctions` component (local functions + MCP) and Grok invokes your Delphi code mid-conversation — the driver handles the whole round-trip (execution on worker threads, `function_call_output`, continuation)
- **xAI native tools**: `EnableWebSearch` / `EnableXSearch` — executed server-side by xAI
- Session options: `Voice` (eva, ara, rex, sal, leo or custom voice_id), `Instructions`, `ReasoningEffort` (high / none for lower latency), `OutputSpeed`, `Keyterms` (transcription biasing), `PronunciationReplace` (TTS corrections), automatic regional language hints (`es`→`es-MX`, `pt`→`pt-BR`)
- `ForceMessage()` — scripted TTS utterance bypassing the model (IVR prompts, disclosures)
- **Session resumption**: `EnableResumption` + `ConversationId` — reconnect and the server replays the cached turns (transcripts, tool calls and outputs; 30-min window)
- **Binary audio transport**: `BinaryAudio := True` — raw PCM over WebSocket binary frames, ~33% less bandwidth than base64
- **Ephemeral tokens** for mobile/browser clients: `MintEphemeralToken()` on your backend + `EphemeralToken` on the client — the API key never leaves the server
- **`file_search`** over xAI Collections (`FileSearchCollections`) and remote MCP servers via `CustomToolsJson`
- Works through `TAiRealtimeConnection` too — just set `DriverName := 'Grok'`

```pascal
uses uMakerAi.Realtime.AiConnection, uMakerAi.Realtime.Grok;

Voice := TAiRealtimeConnection.Create(nil);
Voice.DriverName   := 'Grok';
Voice.ApiKey       := '@GROK_API_KEY';
Voice.Language     := 'es';
Voice.OnTranscriptCompleted := HandleUserText;   // what the user said
Voice.OnAssistantText       := HandleGrokText;   // what Grok answered
Voice.OnAudioChunk          := HandleGrokAudio;  // Grok's voice (PCM16 24 kHz)
Voice.Connect;
// ... stream microphone audio via Voice.SendAudioChunk(Data) ...
// For file-based audio (non-continuous), close the turn explicitly:
// Voice.CommitAudio; TAiGrokRealtimeChat(Voice.Instance).CreateResponse;
```

### cmSmartDispatch — Intelligent Chat Routing

New `ChatMode` value for automatic two-pass routing:

- **Pass 1** — classifies the user intent and rewrites the prompt for the target capability (image generation, speech synthesis, web search, etc.)
- **Pass 2** — dispatches to the appropriate bridge or tool based on classification
- Works with all existing ChatTools (`IAiImageTool`, `IAiSpeechTool`, `IAiWebSearchTool`, etc.)

### Models Updated (May 2026)

| Provider | New / Updated Models |
|----------|----------------------|
| OpenAI | **gpt-5.4**, **gpt-5.4-mini**, **gpt-5.5**, gpt-image-1 |
| Claude | **claude-opus-4-7** (Adaptive Thinking), claude-sonnet-4-6, claude-haiku-4-5 |
| Gemini | **gemini-3.1-pro**, **gemini-3-flash**, gemini-3.1-flash-lite, gemini-3.1-flash-image |
| Grok | **grok-4-fast**, grok-3, grok-code-fast-1 |
| Mistral | magistral-medium/small, devstral, voxtral |
| Groq | llama-4-scout/maverick, kimi-k2, qwen3, compound-beta |
| Kimi | **kimi-k2**, kimi-k2.5, kimi-k2-thinking |
| Cohere | command-a-03-2025, command-a-reasoning, command-a-vision |

### Agent Improvements

- **`TAiAgentManager.Run`** declared `virtual` — proper subclassing now supported
- **jmAll join node fix** — `FJoinInputs` cleared after each execution; eliminates premature firing on retries and loops
- **`TChatInput.EnterAsSend`** — new property (default `False`): Enter sends the prompt, Shift+Enter / Ctrl+Enter inserts a line break
- **`TChatBubble`** — eliminated spurious vertical scrollbar (`ShowScrollBars := False`)

### Bug Fixes

- **Claude Opus 4.7 Adaptive Thinking** — temperature, top_p, top_k and the `thinking` block are now correctly omitted for `claude-opus-4-7` models. Anthropic manages sampling internally for these models; sending these parameters caused HTTP 400 errors.
- **`RegisterDefaultParams` — Max_Tokens key** — corrected in 10 drivers (Claude, Gemini, Mistral, Groq, DeepSeek, Grok, Kimi, LMStudio, GenericLLM, Ollama). The wrong key `MaxTokens` was never resolved by RTTI to the `Max_tokens` property, causing `Max_Tokens` to be silently ignored when set via `RegisterDefaultParams`.
- **`ApplyParamsToChat` — locale-independent float parsing** — `TryStrToFloat` now tries invariant format (dot decimal) first, then falls back to the system locale. Both `Temperature=0.7` and `Temperature=0,7` are valid regardless of regional settings.

### Bug Fixes (March 2026)

- **MCP concurrent tool calls — race condition** (`uMakerAi.MCPClient.Core.pas`): When a model responded with two or more tools from the same MCP server in a single turn, `ParseChat` launched all tool calls as parallel `TTask`s. Since `TMCPClientStdIo` shares a single process/pipe per instance (no synchronization), concurrent calls corrupted the JSON-RPC communication, causing intermittent failures. Fixed by adding `FCallLock: TCriticalSection` to `TMCPClientCustom` — calls to the same server are now serialized while calls to different servers still run in parallel.

- **`EAggregateException` on tool errors — Claude driver** (`uMakerAi.Chat.Claude.pas`): The local `_CreateTask` procedure in `TAiClaudeChat.ParseChat` lacked the `try/except` present in the base class. Any exception raised inside a tool task (MCP timeout, network error, etc.) escaped unhandled, causing `TTask.WaitForAll` to wrap it in an `EAggregateException` and crash the application. Fixed to match base class behavior: exceptions are caught, reported via `OnError`, and the tool receives an error response so the conversation can continue.

---

## 🏗️ Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│  Your Delphi Application                                         │
└────┬──────────────────┬─────────────────┬────────────────────────┘
     │                  │                 │
┌────▼────┐   ┌─────────▼──────────┐  ┌──▼────────────────────────┐
│ ChatUI  │   │ Agents             │  │ Design-Time               │
│ FMX     │   │ TAIAgentManager    │  │ Property Editors          │
│ Visual  │   │ TAIBlackboard      │  │ Object Inspector support  │
│ Comps   │   │ Checkpoint/Approve │  └───────────────────────────┘
└────┬────┘   └─────────┬──────────┘
     │                  │
┌────▼──────────────────▼──────────────────────────────────────────┐
│  TAiChatConnection  — Universal Connector                        │
│  Switch provider at runtime via DriverName property             │
└──────────────────────────────┬───────────────────────────────────┘
                               │
┌──────────────────────────────▼───────────────────────────────────┐
│  Native Provider Drivers  (direct API access, full fidelity)     │
│  OpenAI · Claude · Gemini · Grok · Mistral · DeepSeek · Kimi    │
│  Groq · Cohere · Ollama · LM Studio · GenericLLM                │
└──────────────────────────────┬───────────────────────────────────┘
                               │
     ┌─────────────────────────┼────────────────────────┐
     │                         │                        │
┌────▼────────┐   ┌────────────▼────────┐   ┌───────────▼─────────┐
│  ChatTools  │   │  RAG                │   │  MCP                │
│  PDF/Vision │   │  Vector (VQL)       │   │  Server (HTTP/SSE   │
│  Speech/STT │   │  Graph (GQL)        │   │  StdIO/Direct)      │
│  Web Search │   │  PostgreSQL/SQLite  │   │  Client             │
│  Shell      │   │  HNSW · BM25 · RRF  │   │  TAiFunctions bridge│
│  ComputerUse│   │  Rerank · Documents │   └─────────────────────┘
└─────────────┘   └─────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│  Realtime Voice — parallel WebSocket stack                       │
│  TAiRealtimeConnection · OpenAI STT · Grok Voice S2S · MakerAI   │
│  Pure-Pascal RFC 6455 + TLS (SChannel / OpenSSL / Android)       │
└──────────────────────────────────────────────────────────────────┘
```

---

## 📡 Supported AI Providers

MakerAI gives you **two ways to work with each provider**, which you can mix freely:

### Direct Provider Components
Full, provider-specific access to every API feature. Use when you need complete control:

| Component | Provider | Latest Models |
|-----------|----------|---------------|
| `TAiOpenChat` | OpenAI | gpt-5.4, gpt-5.4-mini, gpt-5.5, gpt-image-1 |
| `TAiClaudeChat` | Anthropic | claude-opus-4-7, claude-sonnet-4-6, claude-haiku-4-5 |
| `TAiGeminiChat` | Google | gemini-3.1-pro, gemini-3-flash, gemini-3.1-flash-lite |
| `TAiGrokChat` | xAI | grok-4-fast, grok-3, grok-code-fast-1 |
| `TAiMistralChat` | Mistral AI | magistral-medium, devstral, voxtral |
| `TAiDeepSeekChat` | DeepSeek | deepseek-reasoner, deepseek-chat |
| `TAiKimiChat` | Moonshot | kimi-k2, kimi-k2.5, kimi-k2-thinking |
| `TAiGroqChat` | Groq | llama-4-scout, llama-4-maverick, kimi-k2, qwen3 |
| `TCohereChat` | Cohere | command-a-03-2025, command-a-reasoning, command-a-vision |
| `TAiOllamaChat` | Ollama | Any local model |
| `TAiLMStudioChat` | LM Studio | Any local model |
| `TAiGenericChat` | OpenAI-compatible | Any OpenAI-API endpoint |

### Universal Connector
Provider-agnostic code. Switch models or providers by changing one property:

```pascal
AiConn.DriverName := 'OpenAI';
AiConn.Model := 'gpt-5.2';
AiConn.ApiKey := '@OPENAI_API_KEY';  // resolved from environment variable

// Switch to Gemini without changing anything else
AiConn.DriverName := 'Gemini';
AiConn.Model := 'gemini-3.0-flash';
AiConn.ApiKey := '@GEMINI_API_KEY';
```

---

## 📊 Feature Support Matrix

| Feature | OpenAI (gpt-5.2) | Claude (4.6) | Gemini (3.0) | Grok (4) | Mistral | DeepSeek | Ollama |
|:--------|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| Text Generation | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Streaming (SSE) | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Function Calling | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| JSON Mode / Schema | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Image Input | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| PDF / Files | ✅ | ✅ | ✅ | ⚠️ | ✅ | ❌ | ⚠️ |
| Image Generation | ✅ | ❌ | ✅ | ✅ | ❌ | ❌ | ❌ |
| Video Generation | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ |
| Extended Thinking | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ⚠️ |
| Speech (TTS/STT) | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ⚠️ |
| Realtime Voice (WebSocket) | ✅ STT | ❌ | ⚠️ | ✅ S2S | ❌ | ❌ | ❌ |
| Web Search | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ |
| Computer Use | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ |
| RAG (all modes) | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| MCP Client/Server | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Agents | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

> **Legend:** ✅ Native | ⚠️ Tool-Assisted bridge | ❌ Not Supported

---

## 🧩 Ecosystem Modules

### 🧠 RAG — Retrieval-Augmented Generation

Two complementary retrieval engines with their own query languages:

**Vector RAG** — semantic and hybrid search over document embeddings:
- HNSW index for approximate nearest-neighbor search
- BM25 lexical index for keyword matching
- Hybrid search with RRF (Reciprocal Rank Fusion) or weighted fusion
- Reranking and Lost-in-the-Middle reordering for LLM context
- **VQL** (Vector Query Language) — SQL-like DSL for complex retrieval queries:
  ```sql
  MATCH documents SEARCH 'machine learning'
  USING HYBRID WEIGHTS(semantic: 0.7, lexical: 0.3) FUSION RRF
  WHERE category = 'tech' AND date > '2025-01-01'
  RERANK 'neural networks' WITH REGENERATE
  LIMIT 10
  ```
- Drivers: PostgreSQL/pgvector, SQLite, in-memory

**Graph RAG** — knowledge graph with semantic search over entities and relationships:
- Nodes and edges with embeddings and metadata
- **MakerGQL** — Graph Query Language based on ISO/IEC 39075:2024 (GQL standard):
  ```gql
  MATCH (p:Person)-[r:WORKS_AT]->(c:Company)
  WHERE c.city = 'Madrid' DEPTH 2
  RETURN p, r, c
  ```
- Dijkstra shortest path, centrality analysis, hub detection
- Export to GraphViz DOT, GraphML (Gephi), native JSON format
- Document lifecycle management (ingest → chunk → embed → link)

### 🤖 Agents — Autonomous Orchestration

Graph-based multi-agent workflows with full thread safety:

- **`TAIAgentManager`** — executes directed graphs of AI nodes via thread pool
- **`TAIAgentsNode`** — single execution unit; runs an LLM call, a tool, or custom logic
- **`TAIBlackboard`** — thread-safe shared state dictionary between all nodes
- **Link modes:** `lmFanout` (parallel broadcast), `lmConditional` (routing), `lmExpression` (binding), `lmManual`
- **Join modes:** `jmAny` (first arrival wins), `jmAll` (wait for all inputs)
- **Durable execution:** `IAiCheckpointer` persists full agent state between process restarts; built-in implementations: `TAiFileCheckpointer` (JSON files) and `TAiDatabaseCheckpointer` (FireDAC — SQLite, PostgreSQL, Firebird, etc.)
- **Human-in-the-loop:** `Node.Suspend(Reason, Context)` pauses a node and saves the checkpoint; `TAiWaitApprovalTool` provides a drop-in approval tool; resume with `ResumeThread(ThreadID, NextNode, HumanInput)`
- Supports any LLM provider via `TAiChatConnection`

### 🔗 MCP — Model Context Protocol

Full implementation of the MCP standard for both consuming and exposing tools:

**MCP Server** — expose Delphi functions as MCP tools, callable by any MCP client (Claude Desktop, AI agents, etc.):
- Transports: **HTTP**, **SSE** (Server-Sent Events), **StdIO**, **Direct** (in-process)
- Bridge `TAiFunctions → IAiMCPTool` — any existing `TAiFunctions` component becomes an MCP server instantly
- API Key authentication, CORS configuration
- `TAiMCPResponseBuilder` for structured responses (text + files + media)
- RTTI-based automatic JSON Schema generation from parameter classes

**MCP Client** — consume any external MCP server from your Delphi app:
- Connect to Claude Desktop tools, filesystem servers, database tools, etc.
- Integrated into `TAiFunctions` component alongside native function definitions

### 🛠️ ChatTools — AI × Deterministic Capabilities

ChatTools bridge the gap between AI reasoning and real-world operations. They activate automatically based on gap analysis between `SessionCaps` and `ModelCaps`:

| Tool Interface | What it does | Implementations |
|----------------|-------------|-----------------|
| `IAiPdfTool` | Extract text from PDFs | Mistral OCR, Ollama OCR |
| `IAiVisionTool` | Describe / analyze images | Any vision model |
| `IAiSpeechTool` | Text-to-speech / speech-to-text | Whisper, Gemini Speech, OpenAI TTS |
| `IAiWebSearchTool` | Live web search | Gemini Web Search |
| `IAiImageTool` | Generate images | DALL-E 3, gpt-image-1, Gemini, Grok |
| `IAiVideoTool` | Generate video | Sora, Gemini Veo |
| `TAiShell` | Execute shell commands | Windows/Linux |
| `TAiTextEditorTool` | Read/write/patch files | Diff-based editing |
| `TAiComputerUseTool` | Control mouse and keyboard | Claude Computer Use, OpenAI |

Tools follow a common pattern: `SetContext(AiChat)` + `Execute*()`. They can run standalone, as function-call bridges, or as automatic capability bridges.

### 🎙️ Realtime Voice — WebSocket STT & Speech-to-Speech

A parallel component stack for live audio over WebSocket, with the same universal-connector pattern as chat (`TAiRealtimeConnection.DriverName`):

| Driver | Type | Endpoint |
|--------|------|----------|
| `TAiOpenAiRealtimeSTT` | STT only — streaming transcription (`gpt-live-transcribe` default) | OpenAI Realtime API |
| `TAiGrokRealtimeChat` | **Full-duplex speech-to-speech** — the user talks, Grok answers with voice | xAI `wss://api.x.ai/v1/realtime` |
| `TAiMakerAiRealtimeChat` | STT + LLM + TTS in one socket | MakerAI server |
| `TAiGeminiRealtimeSTT` | STT (planned) | Gemini Live |

- **`TAiRealtimeVoiceBase`** — shared base for full-duplex drivers: `OnAssistantText[Delta]`, `OnAudioChunk`, `OnAudioDone`, on top of the STT events (`OnTranscriptDelta/Completed`, `OnSpeechStarted/Stopped`)
- **Voice function calling** (Grok): plug a `TAiFunctions` component and the model invokes your Delphi functions mid-conversation
- **Session resumption, binary audio transport, ephemeral tokens** for mobile/browser clients (Grok)
- **Audio pipeline**: `TAIVoiceMonitor` (mic) → thread-safe PCM16 resampler → provider rate (24 kHz); push audio from any source via `SendAudioChunk`
- **Pure-Pascal WebSocket stack** (`TAiWSClient`, RFC 6455) with pluggable TLS: Windows SChannel (zero DLLs), OpenSSL (Linux/macOS), `javax.net.ssl` (Android)

### ⚙️ Model Capabilities — TAiCapabilities

Introduced in v3.3 and refined in v3.4, the `TAiCapabilities` system replaces all manual feature flags with two declarative sets:

- **`ModelCaps`** — what the model natively supports (e.g., `[cap_Image, cap_Reasoning]`)
- **`SessionCaps`** — what the session needs
- **Gap = SessionCaps − ModelCaps** — any missing capability activates an automatic ChatTool bridge; for example, a text-only model with `cap_GenImage` in `SessionCaps` automatically routes image generation requests through a DALL-E or Gemini bridge

```pascal
// Default capabilities for all models of a provider
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'ModelCaps',   '[cap_Image, cap_Pdf]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'SessionCaps', '[cap_Image, cap_Pdf, cap_GenImage]');

// Per-model override (e.g., a reasoning model)
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model', 'ModelCaps',    '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model', 'ThinkingLevel', 'tlMedium');
```

Available capabilities: `cap_Image`, `cap_Audio`, `cap_Video`, `cap_Pdf`, `cap_Reasoning`, `cap_WebSearch`, `cap_GenImage`, `cap_GenVideo`, `cap_TTS`, `cap_STT`, `cap_ComputerUse`

`ThinkingLevel` controls reasoning depth: `tlLow`, `tlMedium`, `tlHigh`.

### 🎨 FMX Visual Components

Two generations of FireMonkey components for building multimodal chat UIs:

**Next-generation (v3.4) — Skia-native, virtualized, zero FMX child controls:**

- **`TAIChatView`** — single-canvas virtualized conversation renderer; only visible messages are painted; supports multi-message text selection, dark/light theme, context menu, long-press (mobile), copy-button feedback with timer, and a scrollbar that doesn't interfere with content
- **`TAIChatInput`** — fully Skia-painted input bar with custom dropdown overlay (no `TPopupMenu` required), voice-mode indicator, file attachment chips, and `TAIVoiceMonitor` integration; layout adapts from 1 to N attachment chips automatically

**Classic components — FMX-layout-based, simpler to subclass:**

- **`TChatList`** — scrollable message container with Markdown rendering, code blocks, copy buttons
- **`TChatBubble`** — individual message bubble (user / assistant / tool)
- **`TChatInput`** — text input bar with voice recording, file attachment, and send button

Both sets are compatible with all providers and work with streaming responses.

### 📐 Design-Time Integration

Full Delphi IDE support via the `MakerAiDsg.dpk` design-time package:
- `DriverName` property shows a dropdown of all registered providers in the Object Inspector
- `Model` property lists all models for the selected provider
- MCP Client configuration editor with transport type selection
- Embedding connection editor
- Version/About dialog

---

## 📦 Installation

```bash
git clone https://github.com/gustavoeenriquez/MakerAi.git
```

### Step 1 — Add Library Paths

**Before compiling any package**, add all of these to **Tools > Options > Language > Delphi > Library**:

```
Source/Agents
Source/Chat
Source/ChatUI
Source/Core
Source/Design
Source/Embeddings
Source/MCPClient
Source/MCPServer
Source/Packages
Source/RAG
Source/Realtime
Source/Resources
Source/Tools
Source/Utils
Source/WebSocket
```

### Step 2 — Compile and Install Packages

Compile and install in this exact order:

1. `Source/Packages/MakerAI.dpk` — Runtime core (~100 units)
2. `Source/Packages/MakerAi.RAG.Drivers.dpk` — PostgreSQL/pgvector connector
3. `Source/Packages/MakerAi.UI.dpk` — FMX visual components
4. `Source/Packages/MakerAiDsg.dpk` — Design-time editors (requires VCL + DesignIDE)

Open `Source/Packages/MakerAiGrp.groupproj` to compile all packages at once.

### API Keys

API keys are resolved from environment variables using the `@VAR_NAME` convention:

```pascal
AiConn.ApiKey := '@OPENAI_API_KEY';    // reads OPENAI_API_KEY from environment
AiConn.ApiKey := '@CLAUDE_API_KEY';    // reads CLAUDE_API_KEY
AiConn.ApiKey := '@GEMINI_API_KEY';    // reads GEMINI_API_KEY
AiConn.ApiKey := '@GROK_API_KEY';      // reads GROK_API_KEY (xAI chat and Grok Voice)
AiConn.ApiKey := 'sk-...';             // or set a literal key directly
```

### Delphi Version Compatibility

| Delphi Version | Support |
|----------------|---------|
| 10.4 Sydney | Limited (minimum supported) |
| 11 Alexandria | **Full support** |
| 12 Athens | **Full support** |
| 13 Florence | **Full support** |
| 13.1 Florence | **Full support** (latest tested) |

---

## 🗂️ Demo Projects

Open `Demos/DemosVersion31.groupproj` to access all demos.

| Demo | Description |
|------|-------------|
| `010-Minimalchat` | Minimal chat with Ollama and TAiChatConnection |
| `012-ChatAllFunctions` | Full-featured multimodal chat (images, audio, streaming, tools) |
| `012-ChatWebList` | Chat with web-based content list |
| `021-RAG+Postgres-UpdateDB` | Build a vector RAG database with PostgreSQL/pgvector |
| `022-1-RAG_SQLite` | Lightweight vector RAG with SQLite |
| `023-RAGVQL` | VQL query language for semantic search |
| `025-RAGGraph` | Knowledge graph RAG with GQL queries |
| `026-RAGGraph-Basic` | Simplified graph RAG patterns |
| `027-DocumentManager` | Document ingestion and management |
| `031-MCPServer` | Multi-protocol MCP server (HTTP, SSE, StdIO) |
| `032-MCP_StdIO_FileManager` | File manager exposed via MCP StdIO |
| `032-MCPServerDataSnap` | MCP server using DataSnap transport |
| `034-MCPServer_Http_FileManager` | File manager via MCP HTTP |
| `035-MCPServerWithTAiFunctions` | TAiFunctions bridge to MCP |
| `036-MCPServerStdIO_AiFunction` | StdIO MCP server with AI functions |
| `041-GeminiVeo` | Video generation with Google Veo |
| `051-AgentDemo` | Visual agent graph builder and runner |
| `052-AgentConsole` | Console-based agent execution (conditional and parallel flows) |
| `053-DemoAgentesTools` | Agents with integrated tool use |
| `054-AgentCheckpointDB` | Durable agent execution: suspend/resume with `TAiDatabaseCheckpointer` (SQLite via FireDAC) |
| `060-AIChatUI` | Next-generation `TAIChatView` + `TAIChatInput` components — full multimodal demo |

---

## 🔄 Changelog

### Unreleased (dev)
- New: **Grok video generation** — `TAiGrokChat.InternalRunNativeVideoGeneration` implements the grok-imagine async video job (`POST /videos/generations` + polling + mp4 download as `TAiMediaFile`), with new `VideoDurationSeconds` property; activated via `cmVideoGeneration` or the `[cap_GenVideo]` gap. Runtime-tested (image generation also verified live)
- New: **xAI Grok Aug 2026** — full catalog turnover: `grok-4.3` (new driver default, 1M ctx, vision + always-on reasoning), `grok-4.5` (premium), `grok-build-0.1` (coding), `grok-imagine-image-quality` and `grok-imagine-video-1.5` registered; entire grok-3/grok-4-fast/4.1 families and grok-2 models retired with compatibility aliases (`grok-3`→`grok-4.3`, etc.). Runtime-tested 6/6
- New: **Groq Aug 2026** — `qwen/qwen3.6-27b` registered (replaces retired `qwen3-32b`, alias kept) plus `allam-2-7b`; retired entries removed (`llama-4-scout`, `moonshotai/kimi-k2-instruct(-0905)`). Fix: `openai/gpt-oss-120b` is text-only on Groq — `cap_Image` removed (no vision chat model on Groq currently). Runtime-tested 4/4
- New: **Cohere Aug 2026** — `command-a-plus-05-2026` flagship (436K ctx, vision + always-on reasoning) and `north-mini-code-1-0` registered; thinking-mode control via `cap_Reasoning` (blocks captured into `ReasoningContent`/`OnReceiveThinking`, non-streaming and streaming); Rerank v4.0 and tiny-aya noted; retired 8b Aya entries removed. Fix: synchronous tool-calling return was always empty (second round now reuses the same `ResMsg`). Runtime-tested 5/5
- New: **DeepSeek V4** — `deepseek-v4-flash` (new driver default) and `deepseek-v4-pro` (1M ctx, 384K output); explicit thinking-mode control (`cap_Reasoning` + `ThinkingLevel` → `thinking`/`reasoning_effort`, disabled otherwise since the API defaults to thinking ON); retired aliases `deepseek-chat`/`deepseek-reasoner` flagged (officially sunset Jul 24, 2026). Runtime-tested 4/4 including tool calling in thinking mode
- New: **Kimi K3 family** — `kimi-k3` (new driver default, 1M ctx, vision + reasoning), `kimi-k2.7-code`/`-highspeed` and `kimi-k2.6` registered; retired models (`kimi-k2`, `kimi-k2-thinking`) removed and Aug 31 sunsets flagged (`kimi-k2.5`, `moonshot-v1-*`). Fix: the new family rejects `top_p` (400) — removed from Kimi defaults. Runtime-tested 4/4 including K3 vision
- New: **Mistral Voxtral TTS** — `voxtral-mini-tts-2603` via `POST /v1/audio/speech` (`TtsVoice` from the `/v1/audio/voices` catalog + `TtsFormat`); activated by the `[cap_GenAudio]` gap; runtime-tested. Plus OCR 4 support: `OcrIncludeBlocks` (paragraph-level bounding boxes) and page-range syntax
- New: **Gemini 3.5/3.6 family registered** (`gemini-3.5-flash` + `gemini-flash-latest` alias, `gemini-3.6-flash`, `gemini-3.5-flash-lite`), Nano Banana GA image models (`gemini-3.1-flash-image`, `gemini-3-pro-image`, `gemini-3.1-flash-lite-image`), `gemini-omni-flash-preview` (video) and `gemini-embedding-2`; the driver omits deprecated sampling params (`temperature`/`topP`) on the 3.5+/3.6/omni family. Veo 2.0/3.0 profiles removed (shut down by Google Jun 30) and Imagen 4.0 shutdown (Aug 17) flagged. *Not runtime-tested — no Gemini API key available*
- New: Claude driver phase 2 — `FastMode` (`speed:"fast"`, Opus 5/4.8, research preview — requires org quota), mid-conversation `system` messages in the history (cache-preserving on Opus 5/4.8/Fable; auto-degraded to `<system-reminder>` user turns elsewhere), `EnableCompaction` (server-side compaction with compaction-block echo), `RefusalFallbackModel` (server-side fallback on refusals), and `aa_claude-sonnet-5-thinking` / `aa_claude-opus-5-thinking` / `aa_claude-opus-5-agent` profiles; runtime-tested 4/4 (Fast mode blocked only by org quota)
- New: **Claude 5 family support** — `claude-opus-5`, `claude-sonnet-5`, `claude-fable-5`, plus `claude-opus-4-7`/`claude-opus-4-8` registered; the driver now sends `thinking: {type: "adaptive"}` on the 4.6+ families and maps `ThinkingLevel` → `output_config.effort` (`budget_tokens`/sampling params return 400 on 4.7+ and are only sent on legacy models). Runtime-tested (sonnet-5, opus-4-6 adaptive, haiku legacy)
- Update: Claude driver — `output_format` migrated to `output_config.format`; web search upgraded to `web_search_20260209` (dynamic filtering) on 4.6+; `stop_reason: "refusal"` now parses `stop_details` and fires `OnError`
- New: **`TAiOpenAiRealtimeTranslate`** — streaming speech translation via `gpt-realtime-translate` (`wss://api.openai.com/v1/realtime/translations`); continuous stream without VAD/turns; emits translated text (`OnAssistantTextDelta`), translated TTS audio (`OnAudioChunk`) and optional source transcript (`SourceTranscription`); runtime-tested (es→en)
- New: demo **`071-VoiceBridgeTranslate`** — the 063 voice bridge refactored with `TAiOpenAiRealtimeTranslate`: one WebSocket per direction replaces the STT→LLM→TTS pipeline (lower latency, ~1/3 of the code)
- New: **GPT-5.6 family registered** (`gpt-5.6-sol` / `-terra` / `-luna` + `gpt-5.6` alias) — 1.05M ctx, vision + reasoning + tools; `gpt-5.6-luna` runtime-tested
- Update: Realtime session default model → **`gpt-realtime-2.1`** (better alphanumeric recognition and noise handling) in `TAiOpenAiRealtimeSTT` and demos 062–064
- Update: **`TAiDalle` / `TAiDalleImageTool` default model → `gpt-image-1`** — the `dall-e-2`/`dall-e-3` snapshots were deprecated by OpenAI (May 2026); both remain selectable while the API accepts them
- New: **OpenAI `gpt-transcribe` / `gpt-live-transcribe`** (Whisper successors, Aug 2026) — `TAiOpenAiAudio` gains `tmGptTranscribe`/`tmGptLiveTranscribe` with `TranscriptionKeywords` + `TranscriptionLanguages`; `TAiOpenAiRealtimeSTT` defaults to `gpt-live-transcribe` with new context props (`TranscriptionPrompt`, `TranscriptionKeywords`, `Languages`, `LowDelay`); both runtime-tested. Registry entries added
- Update: VoiceBridge demos (062–065) migrated to `gpt-live-transcribe` on live channels (contextual prompt + guided language autodetection); diarized channels stay on `gpt-4o-transcribe-diarize` (new models don't support diarization)
- New: **`TAiGrokRealtimeChat`** — xAI Grok Voice speech-to-speech driver (`wss://api.x.ai/v1/realtime`, OpenAI Realtime-compatible, 24 kHz PCM16); live user transcription + streamed assistant text and TTS audio; runtime-tested against the live API
- New: **Voice function calling for Grok Voice** — `AiFunctions` (`TAiFunctions`: local functions + MCP) declared as session tools; automatic tool round-trip (worker-thread execution, `function_call_output`, single continuation `response.create`); `OnCallToolFunction` fallback event; runtime-tested end-to-end
- New: Grok Voice extras — `EnableWebSearch` / `EnableXSearch` (xAI server-side tools), `OutputSpeed`, `Keyterms`, `PronunciationReplace`, `ForceMessage()` (scripted TTS)
- New: Grok Voice phase 3 — session resumption with turn replay (`EnableResumption` + `ConversationId`), binary audio transport (`BinaryAudio`), ephemeral tokens (`MintEphemeralToken` + `EphemeralToken`), `file_search` over Collections and remote MCP via `CustomToolsJson`; all runtime-tested except MCP declarations
- New: **`TAiRealtimeVoiceBase`** — shared base for full-duplex voice drivers (`OnAssistantText`, `OnAssistantTextDelta`, `OnAudioChunk`, `OnAudioDone`); `TAiMakerAiRealtimeChat` and `TAiRealtimeConnection` now inherit from it, so voice events flow through the universal connector

### v3.4 (May 2026)
- Tested with Delphi 13.1 Florence
- Selective driver registration — each driver self-registers only when imported
- New: **`TAIChatView`** — next-generation Skia-native virtualized chat renderer (single canvas, no FMX child controls, multi-message text selection, dark/light theme, mobile long-press)
- New: **`TAIChatInput`** — fully Skia-painted input bar with custom dropdown overlay, attachment chips, voice indicator (no `TPopupMenu` / no FMX buttons)
- New: `TAiRealtimeConnection` + `TAiOpenAiRealtimeSTT` — real-time STT via WebSocket (24 kHz PCM16, VAD, streaming transcription; pure-Pascal TLS via Windows SChannel)
- New: `cmSmartDispatch` chat mode — two-pass intelligent routing
- Models: claude-opus-4-7, gpt-5.4/5.5, gemini-3.1-pro, grok-4-fast, kimi-k2, groq llama-4
- Fix: Claude Opus 4.7 Adaptive Thinking — HTTP 400 eliminated (temperature/top_p/top_k + thinking block now omitted for claude-opus-4-7)
- Fix: AV on async abort — nil guard in `TAiChatConnection.OnInternalReceiveDataEnd`
- Fix: `TStringStream` leak in async HTTP requests — `FCurrentPostStream` lifetime now correctly tied to request completion
- Fix: `RegisterDefaultParams` `Max_Tokens` key corrected in 10 drivers
- Fix: `ApplyParamsToChat` `TryStrToFloat` now locale-independent
- Fix: Agent jmAll join node premature firing on retries
- Fix: `TChatBubble` spurious vertical scrollbar eliminated
- New: `TChatInput.EnterAsSend` property
- New: **`TAiDatabaseCheckpointer`** — FireDAC-based checkpoint persistence; works with SQLite, PostgreSQL, Firebird, MySQL, SQL Server, and any other FireDAC driver
- Fix: **D11 Alexandria compatibility** — `TInterlocked.Exchange(Boolean)` (D12-only) replaced with Integer-based atomic; `AddStream(AShareOwnership)` boundary corrected to `CompilerVersion >= 36`; `THashSet<T>` boundary corrected to `CompilerVersion >= 36`
- Fix: **`ModelCaps` / `SessionCaps` duplicated in the Object Inspector** — `TAiChat` published these at both the root level and inside `ModelConfig`, out of sync with each other. Now there's a single source of truth (`ModelConfig.ModelCaps` / `ModelConfig.SessionCaps`); the root shortcuts still work in code but were moved out of `published`, so the Object Inspector shows the property only once

### v3.3 (February 2026)
- New `TAiCapabilities` system (`ModelCaps` / `SessionCaps` / `ThinkingLevel`)
- Models updated: OpenAI gpt-5.2, Claude 4.6, Gemini 3.0, Grok 4, Mistral Magistral, DeepSeek-reasoner, Kimi k2.5
- Agents: durable execution (checkpoints), human-in-the-loop approval tool
- RAG: Graph Document management (`uMakerAi.RAG.Graph.Documents`)
- Fix: `reasoning_content` preserved in multi-turn tool calls (DeepSeek, Kimi, Groq)
- New: `TAiEmbeddingsConnection`, `TAiAudioPushStream`
- New demos: DocumentManager, ChatWebList

### v3.2 (January 2026)
- Native ChatTools framework (`IAiPdfTool`, `IAiVisionTool`, `IAiSpeechTool`, etc.)
- Unified deterministic tool orchestration and capability bridges

### v3.1 (November 2025)
- GPT-5.1, Gemini 3.0, Claude 4.5 initial support
- FMX multimodal UI components
- RAG Rerank + Graph RAG engine
- MCP Server framework (SSE, StdIO, HTTP)

### v3.0 (October 2025)
- Major architecture redesign
- Visual FMX chat components
- Graph-based vector database
- Delphi 10.4–13 compatible (limited: 10.4 Sydney; full support: 11 Alexandria+)

### v2.5 (August 2025)
- MCP Client/Server (Model Context Protocol)
- Agent graph orchestration
- Linux/POSIX full support

---

## 💬 Community & Support

- **Website:** [https://makerai.cimamaker.com](https://makerai.cimamaker.com)
- **Manual (EN/ES):** [https://www.gustavoenriquez.com/book-makerai](https://www.gustavoenriquez.com/book-makerai)
- **Telegram (Spanish):** [https://t.me/MakerAi_Suite_Delphi](https://t.me/MakerAi_Suite_Delphi)
- **Telegram (English):** [https://t.me/MakerAi_Delphi_Suite_English](https://t.me/MakerAi_Delphi_Suite_English)
- **Email:** gustavoeenriquez@gmail.com
- **GitHub Issues:** [https://github.com/gustavoeenriquez/MakerAi/issues](https://github.com/gustavoeenriquez/MakerAi/issues)

---

## 📜 License

MIT License — see `LICENSE.txt` for details.

Copyright © 2024–2026 Gustavo Enríquez — CimaMaker
