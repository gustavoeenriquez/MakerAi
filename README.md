# MakerAI Suite v4.0 — The AI Ecosystem for Delphi

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
- **MCP Servers and Clients** — expose or consume tools using the Model Context Protocol (dual-era: stateless spec 2026-07-28 + legacy handshake)
- **Native ChatTools** — bridge AI reasoning with deterministic real-world capabilities (PDF, Vision, Speech, Web Search, Shell, Computer Use)
- **Skills** — reusable instructions in the SKILL.md format (Agent Skills / PPM), loaded on demand by the model
- **A2A** — expose your agent graphs to other agents and call remote ones (Agent-to-Agent protocol 1.0)
- **Production controls** — tool-call guardrails, evals, OpenTelemetry tracing, persistent memory, and calibrated decisions with Jev
- **FMX Visual Components** — drop-in UI for multimodal chat interfaces
- **Universal Connector** — switch providers at runtime without changing your application code

Whether you need a simple one-provider integration or a multi-agent, multi-provider, retrieval-augmented production system, MakerAI covers the full stack — **natively in Delphi**.

---

## 🚀 What's New in v4.0

### October 2026 model refresh — checked against the live APIs
Every provider was compared with what its API actually serves today (not only its docs), and every model we could call was tested live from the Delphi drivers. Retired models stay usable as **aliases of their successors**, so forms and configurations saved with an old name keep working.

| Provider | New models | Retired → now an alias of |
|----------|------------|---------------------------|
| Anthropic | **`claude-haiku-5-5`** (1M context, $0.10/$0.50 up to 100K-token prompts) | `claude-sonnet-4-20250514` → `claude-sonnet-5-5`, `claude-opus-4-20250514` → `claude-opus-5-5` |
| OpenAI | **`gpt-6.1-sol`** ($2/$10), **`gpt-6.1-terra`** ($1/$5) | `o3-pro` → `gpt-5.5-pro`, `o3/o4-mini-deep-research` → `gpt-6.1-sol`, `dall-e-2/3` → `gpt-image-1` |
| Google | **`gemini-4-argon`**, **`gemini-nano-banana-2.1`**, `gemini-omni-1.1-flash` | `gemini-3.5-flash` → `3.6-flash`, `gemini-3.7-flash` → `3.8-flash`, `gemini-2.5-computer-use-preview` → `3.8-flash` |
| Moonshot | **`kimi-k3.1-pro`**, `kimi-k3.1-pro-highspeed` | `kimi-k2.5`, `moonshot-v1-*` → `kimi-k3`; `kimi-latest` → `kimi-k3.1-pro` |
| Zhipu / Z.ai | **`glm-5.3-flash`**, `glm-5.3-flashx` (multimodal, MIT weights) | — |

Fixes found on the way (each one returned HTTP 400 or used the wrong setting):
- **Claude Haiku 5.5** was sent down the legacy path (`budget_tokens` + `temperature`); it now uses adaptive thinking with `effort`, accepts mid-conversation `system` messages, and `RefusalFallbackModel` is skipped (Haiku 5.5 has no server-side fallback).
- **OpenAI:** `tlNone` is sent as `low` to `gpt-6-astra` and `gpt-6.1-sol` (they reject `none`); `gpt-5.x-pro` only accepts `medium`/`high`/`xhigh`, so lower levels go up to `medium` and `tlMax` down to `xhigh`.
- **Gemini 4 Argon** was routed to the Gemini 2.5 `thinkingBudget` path; the driver now picks `thinkingLevel` by model version.
- **Kimi:** every current model (k2.6, k2.7, k3, k3.1) accepts only `temperature = 1` and `top_p = 0.95`; the driver no longer sends them.
- **GLM:** `tlNone`/`tlMinimal`/`tlXHigh`/`tlMax` sent no `reasoning_effort`, so the API used `max`, the most expensive level.

Tested live: Claude 7/7, OpenAI 9/9, Kimi + GLM + DeepSeek 12/13 (`kimi-k3.1-pro-highspeed` answered *engine overloaded*). Gemini was checked against the official docs only (no account).

### MiniMax — new provider
`TAiMiniMaxChat` (`DriverName = 'MiniMax'`, `@MINIMAX_API_KEY`) on MiniMax's OpenAI-compatible endpoint: `MiniMax-M3` (default; 1M context, image and video input), `MiniMax-M3.1-Flash` (tunable reasoning depth) and the M2.x family. Reasoning comes back in `reasoning_content` (`reasoning_split` is always on) and is re-sent in the history, as MiniMax requires. Thinking follows `cap_Reasoning`, except on M3.1-Flash, which cannot turn it off; `reasoning_effort` is only sent to M3.1-Flash. *Written against the official docs; not runtime-tested yet (no API key).*

### Agents — a complete fluent API
`AddEdge` now adds targets to an existing link (before it replaced it, so a fluent fan-out only ran its last branch), and the builder gained `AddNode`, `AddToolNode`, `AddExpressionEdge`, `AddFallbackEdge` and `SetJoinMode`, with construction errors raised as `EAiGraphError`. `TLLMNode` runs on the new protected `ExecuteNode`, has its own `Guardrails` and is owned by the manager. Forms saved with older versions load again: properties that are no longer published are read and applied (or dropped) instead of raising `EReadError`.

### PPM — A2A agents as packages, and your own registry
A2A defines the Agent Card (`/.well-known/agent-card.json`) but not where to find an agent. The PPM registry now works as that catalog: a new package type **`agent`** holds only the agent's card and its URL (`agent-card.json` + `pai.package` with `[agent] url`) — no code, the agent keeps running as a service.

- **Publish** from the live agent: `ppm publish --desde-url http://host:port` reads its card and builds the package. The registry validates the card and indexes name, description and skills (`GET /v1/a2a/discover?q=`, `GET /v1/packages/:name/:version/card`).
- **In code:** `TAiFunctions.SearchPPMAgents`, `GetAgentCardFromPPM` and `InstallAgentFromPPM` — the agent becomes one more function of the `TAiFunctions` (a `TAiA2AAgentTool`).
- **AutoMCP:** with `uMakerAi.A2A.Client` in the project, `ppm_search` also lists agents, `ppm_install` recognises an `agent` package, and the model gets **`call_agent(agent_name, message)`**. The same `AutoMCPConfig.Allowed` / `Blocked`, `OnAutoMCPRequest` and guardrails apply.
- **Keys never travel in the package:** if the agent needs one, it is read from the environment of whoever installs it (`PPM_AGENT_KEY_<NAME>`).
- **Your own registry:** a company can run PPMServer in its own network with its own MCP servers, skills and agents, and point `AutoMCPConfig.RegistryUrl` at it. Ready-to-run example in `Demos/Libro/cap33-registry-corporativo/registro-local` (a local registry with an MCP server, a skill, a prompt and an A2A agent of *Café La Ladera*).
- **Agents are for private registries, by design.** An `agent` package is the URL of someone's running service, not code you can review, and a public registry cannot vouch for whoever operates it — with AutoMCP, installing one means sending your users' conversations there. So PPMServer accepts agents only when started with `PPM_ALLOW_AGENTS=1` (plus `migration_003_agent_type.sql`); **the public `registry.cimamaker.com` keeps them off** (publishing an agent returns 400, `/v1/a2a/discover` returns an empty list, `/v1/health` reports `"agents": false`).
- Demo: `093-A2AChatAgentTool --ppm` — the chat does not know the agent's URL; it finds it in the registry, installs it from its card and asks it over A2A.

### Chat UI
`TAIChatView` messages gain a **footer**, **action buttons** inside the bubble (`OnMessageAction`, e.g. *Approve / Reject*) and collapsible **task steps** with durations (`AddStep` / `FinishStep`), all persisted in the message JSON. Consecutive messages of the same role merge into one bubble (`MergeConsecutive`), `AddAssistantMessage` shows generated images, audio and files, and `TAIChatInput` has a voice-call button wired for realtime sessions.

### Computer Use runs the model's actions in order
With `computer_toolset_20260801` Claude sends several screen actions per turn that are steps of one sequence; the driver ran them in parallel, so two `type` actions mixed their letters in the same field. A batch with a screen action now runs serially, in arrival order — the same form took 14 actions and 26 s instead of 114 actions and 322 s. `key` honours `repeat`.

### Telemetry — one trace per turn
Tool calls run in their own tasks, and OpenAI streaming executes functions on the HTTP thread; none of them carried the turn's span, so every `execute_tool` (and an agent graph or MCP call behind it) showed up as a separate trace. `TAiTelemetry.ContextEnter` / `ContextLeave` carry the parent across threads: a turn that delegates to a graph went from 5 loose traces to one trace of 14 spans.

### Production fixes
- **Bridges actually reach the model:** the web-search result is injected as context (the model used to answer "I can't browse" with the search already done), the input bridges (audio, image, PDF) no longer fire `OnReceiveDataEnd` with the raw tool output, and `TAiOpenAiWebSearchTool` moved to the Responses API (`gpt-4o-mini-search-preview` was retired).
- **`TAiShell` security list** checks every segment of a chained command (`&`, `|`, `;`, new lines); before, `cd x && del /f /q *` passed an allow-list of `[cd, dir]`.
- **Prompt filter:** a message blocked by the sanitizer or the prompt guard no longer stays in the history (it reached the model on the next turn), and injection phrases are recognised in Spanish too.
- **OpenAI:** `apply_patch` works in streaming and can create files; strict schemas are enforced on nested objects and arrays, and tools with a free-form object are sent without `strict` (both were 400s); `PromptCacheMode` → `prompt_cache_options.mode`.
- **Concurrency:** a new question asked right after `OnReceiveDataEnd` waits for the previous request to close (intermittent *Stream read error*); `TAudioPushStream` no longer deadlocks the app when interrupted mid-playback.
- **Connections and costs:** a `Model` saved in the form is applied on load even when it is not the driver default; `ResetTokenCounters` (#135); `Cost_usd` accumulates what the MakerAI broker charges per call.
- **MCP:** the SSE client retries a dropped POST (except `tools/call`), reports `IsConnected`, and streams over `https`; freeing a running `TAiMCPSSEHttpServer` no longer raises an AV.
- **WebSocket:** `ws://` uses plain TCP (it opened TLS and hung against a plain HTTP server); `wss://` works on Android.
- **Evals:** the judge receives the case's question and starts a fresh chat for every verdict.
- **Build:** packages and the regression suite compile without the IDE Library Path (PR #132), and Delphi sources check out with CRLF, also in the GitHub ZIP (#133).

### Demos for the book's 4.0 edition
`Demos/Libro` holds one project per chapter of *IA con Delphi* 4.0 (chapters 3 to 37: from the first chat to agents, federation, voice, Computer Use and every bridge), and demos `093`–`105` follow the *Café La Ladera* video series (see **Demo Projects** below).

### English, Spanish and Portuguese
Chat UI texts, design-time editors, exception messages and status messages now follow the OS language (or `MAKERAI_LANG`, or `TAiLocale.Language := 'pt'` at runtime): English, Spanish and Brazilian Portuguese, 568 texts in `Source/Core/uMakerAi.Lang.*.pas`. Prompts sent to the model stay in English and ask it to answer in the user's language. An application can add its own language with `TAiLocale.RegisterTable`.

> **Portuguese speakers:** the Portuguese texts were not written by a native speaker. If something reads wrong, please open an issue or a pull request — every text is in `Source/Core/uMakerAi.Lang.Pt.pas`, one line per text (keep the key and the `%s`/`%d` markers as they are).
>
> **Para quem fala português:** os textos em português não foram escritos por um falante nativo. Se algo soar estranho, abra uma issue ou um pull request — todos os textos estão em `Source/Core/uMakerAi.Lang.Pt.pas`, uma linha por texto (mantenha a chave e os marcadores `%s`/`%d` como estão).

### ⚠️ Behaviour changes
- **`imagen-4.0-*` aliases now generate with `gemini-nano-banana-2.1`** (before `gemini-3.1-flash-image`, deprecated on 2026-10-06).
- **Kimi ignores `Temperature` and `Top_p`** — the API only accepts its fixed values, so any other value used to fail.
- **`gpt-5.x-pro` and `gpt-6-astra` / `gpt-6.1-sol` adjust the requested effort** to the nearest level the model accepts instead of failing.
- **`TAiShell` in allow-list mode rejects redirections and substitutions** (`>`, `<`, `` ` ``, `$( )`), and checks each segment of a chained command.
- **`TAiChat.Run` waits up to 15 s for the previous request to close** before starting a new one (it does not wait when called from inside `OnReceiveDataEnd`).
- **Setting a typed `ModelConfig` field to its current value now pins it** (`Tool_Active := True` was overwritten by the driver registry when it already was `True`).
- **Internal prompts are in English** and ask the model to answer in the user's language: the default `SystemPrompt` of a new chat component (*"You are a helpful assistant. Always answer in the same language the user writes in."*), the JSON-format hint, the AutoMCP instructions, the evals judge, the `TAiMemory` analyzer, the bridge prompts (vision, document, transcription), the context labels added to the user message (audio transcription, web search, memory, knowledge graph), and the descriptions and results of the built-in tools (Computer Use, text editor, memory MCP tools, A2A agent tool). A `SystemPrompt` saved in a form keeps its text. Code that searched for the old Spanish labels (e.g. `Contexto extraído del grafo`) must look for the English ones.
- **An MCP server without `ServerVersion` reports its executable's version** in `serverInfo` instead of `1.0.0`.
- **Chat UI and design-time texts follow the OS language** (`MAKERAI_LANG` or `TAiLocale.Language` to force one) instead of a fixed mix of English and Spanish.
- **Exception messages follow the OS language too**, so their text changed (several were in Spanish, others in English). Code that compared `E.Message` with a fixed text should check the exception class instead. For HTTP errors from a provider there is now `EAiHttpError` (`StatusCode`, `ResponseText`), and `TAiChat`/`TAiChatConnection` expose `LastHttpStatus` for synchronous callers; in English the message is still `Error Received: <code>, <body>`.

## Earlier versions

What came in each release from v3.4 to v3.9 (GPT-Live, Jev, Qwen, Skills, Computer Use on Linux, MCP 2026-07-28, OpenTelemetry, A2A 1.0, guardrails and evals, the full-duplex voice suite, selective driver registration…) is in [Docs/WHATS-NEW-3.x.md](Docs/WHATS-NEW-3.x.md). The *Changelog* at the end of this page lists every version.


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
│         │   │ A2A server/client  │
└────┬────┘   └─────────┬──────────┘
     │                  │
┌────▼──────────────────▼──────────────────────────────────────────┐
│  TAiChatConnection  — Universal Connector                        │
│  Switch provider at runtime via DriverName property              │
└──────────────────────────────┬───────────────────────────────────┘
                               │
┌──────────────────────────────▼───────────────────────────────────┐
│  Native Provider Drivers  (direct API access, full fidelity)     │
│  OpenAI · Claude · Gemini · Grok · Mistral · DeepSeek · Kimi     │
│  GLM · MiniMax · Qwen · Groq · Cohere · Ollama · LM Studio       │
│  GenericLLM (any OpenAI-compatible endpoint)                     │
└──────────────────────────────┬───────────────────────────────────┘
                               │
     ┌─────────────────────────┼────────────────────────┐
     │                         │                        │
┌────▼────────┐   ┌────────────▼────────┐   ┌───────────▼─────────┐
│  ChatTools  │   │  RAG                │   │  MCP                │
│  PDF/Vision │   │  Vector (VQL)       │   │  Server (HTTP/SSE   │
│  Speech/STT │   │  Graph (GQL)        │   │  StdIO/Direct)      │
│  Web Search │   │  PostgreSQL/SQLite  │   │  Client             │
│  Shell      │   │  SQL Server         │   │  TAiFunctions bridge│
│  ComputerUse│   │  HNSW · BM25 · RRF  │   └─────────────────────┘
│  Skills     │   │  Rerank · Documents │
└─────────────┘   └─────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│  Cross-cutting: Guardrails · Evals · OpenTelemetry · TAiMemory   │
│  Jev (calibrated decisions: routing, guards, rerank, labeling)   │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│  Realtime Voice — parallel WebSocket stack                       │
│  TAiRealtimeConnection · OpenAI STT/Translate · Grok Voice S2S   │
│  OpenAI GPT-Live (full-duplex voice that delegates the thinking) │
│  Qwen voice/STT/translate/TTS · MakerAI                          │
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
| `TAiOpenChat` | OpenAI | **gpt-6-sol** (default), gpt-6.1-sol, gpt-6.1-terra, gpt-6-luna, gpt-6-astra, gpt-5.6, gpt-image-2.5 |
| `TAiClaudeChat` | Anthropic | claude-opus-5-5, claude-sonnet-5-5, claude-fable-5-1, claude-haiku-5-5, **claude-haiku-4-5** (default) |
| `TAiGeminiChat` | Google | **gemini-3.8-flash** (default), gemini-4-argon, gemini-3.1-pro, gemini-3.8-flash-tts, Nano Banana 2.1 |
| `TAiGrokChat` | xAI | grok-4.3, grok-4.5, grok-build, grok-imagine (image/video) |
| `TAiMistralChat` | Mistral AI | mistral-large/medium/small, magistral, devstral, voxtral (STT/TTS) |
| `TAiDeepSeekChat` | DeepSeek | deepseek-flash, deepseek-v4-pro |
| `TAiKimiChat` | Moonshot | kimi-k3, kimi-k3.1-pro, kimi-k2.7-code, kimi-k2.6 |
| `TAiGLMChat` | GLM (Zhipu / Z.ai) | glm-4.7, glm-5.3, glm-5.3-flash, glm-5v-turbo, free tiers: glm-4.7-flash / glm-4.6v-flash |
| `TAiMiniMaxChat` | MiniMax | **MiniMax-M3** (default), MiniMax-M3.1-Flash, MiniMax-M2.7 |
| `TAiQwenChat` | Qwen (Alibaba Model Studio) | qwen3.8-flash, qwen3.8-max, qwen3.7-plus, qwq-plus, qwen3-coder-plus, qwen3-vl-flash |
| `TAiGroqChat` | Groq | openai/gpt-oss-20b, openai/gpt-oss-120b, qwen3.8, whisper-large-v3 |
| `TCohereChat` | Cohere | command-a-plus, command-a-03-2025, north-mini-code |
| `TAiOllamaChat` | Ollama | Any local model |
| `TAiLMStudioChat` | LM Studio | Any local model |
| `TAiGenericChat` | OpenAI-compatible | Any OpenAI-API endpoint |

### Universal Connector
Provider-agnostic code. Switch models or providers by changing one property:

```pascal
AiConn.DriverName := 'OpenAI';
AiConn.Model := 'gpt-6-sol';         // leave Model empty to get the driver's default
AiConn.Params.Values['ApiKey'] := '@OPENAI_API_KEY';  // resolved from the environment variable

// Switch to Gemini without changing anything else
AiConn.DriverName := 'Gemini';
AiConn.Model := 'gemini-3.8-flash';
AiConn.Params.Values['ApiKey'] := '@GEMINI_API_KEY';

// Or to GLM (Zhipu / Z.ai) — glm-4.7-flash is free
AiConn.DriverName := 'GLM';
AiConn.Model := 'glm-4.7-flash';
AiConn.Params.Values['ApiKey'] := '@GLM_API_KEY';
```

---

## 📊 Feature Support Matrix

| Feature | OpenAI (GPT-6) | Claude (5 / 5.5) | Gemini (3.8) | Grok (4.5) | Mistral | DeepSeek | Ollama |
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
| Computer Use ¹ | ✅ | ✅ | ⚠️ | ❌ | ❌ | ❌ | ❌ |
| RAG (all modes) | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| MCP Client/Server | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Agents | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

> **Legend:** ✅ Native | ⚠️ Tool-Assisted bridge | ❌ Not Supported

> ¹ **Computer Use is opt-in.** `cap_ComputerUse` hands the model the real mouse and
> keyboard, so no model enables it by default — you add the capability yourself (see
> the **Computer Use** section below). Native support: OpenAI
> `gpt-6-astra`; Claude `claude-opus-4-8` / `-opus-5` / `-sonnet-5` / `-fable-5`, and
> `claude-sonnet-5-5` (tested live on Xvfb). Gemini is marked ⚠️: the dedicated
> `gemini-2.5-computer-use-preview` model was shut down and is now an alias of
> `gemini-3.8-flash`, which lists computer use as a preview tool — not runtime-tested.
> The other Claude 5.5 / Fable 5.1 / Haiku 5.5 models use the same `computer_toolset`
> but have not been runtime-tested with it yet.

---

## 🎨 Component Palette

Every one of the 109 components has its own palette icon. The tile color identifies the
provider (maroon = MakerAI framework, black = OpenAI, blue = Gemini, purple = Qwen, teal = Jev…).

![MakerAI 3.8 component palette](Docs/Version%203/images/social/MakerAI-3.8-palette-1200x675.png)

The **[full sheet](Docs/Version%203/images/MakerAI-Component-Palette.png)** groups them by
category, with the class name and the palette tab each one lands on. Icons are generated by
`Source/Resources/gen_icons.py` and the sheet by `gen_palette_sheet.py`; see
[`Source/Resources/CLAUDE.md`](Source/Resources/CLAUDE.md) to add an icon for a new component.

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
  MATCH (documents) SEARCH 'machine learning'
  USING HYBRID WITH WEIGHTS(0.7, 0.3) FUSION RRF
  WHERE category = 'tech' AND date > '2025-01-01'
  RERANK 'neural networks' REGENERATE
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

Full **dual-era** implementation of the MCP standard for both consuming and exposing tools: supports the stateless **spec revision 2026-07-28** (per-request `_meta`, `server/discover`, MRTR elicitation) and interoperates automatically with legacy peers that still use the `initialize` handshake.

**MCP Server** — expose Delphi functions as MCP tools, callable by any MCP client (Claude Desktop, AI agents, etc.):
- Transports: **HTTP** (Streamable HTTP — stateless per spec 2026-07-28), **StdIO**, **Direct** (in-process), **SSE** (legacy — see deprecation note below)
- Dual-era per request: modern clients are served stateless (per-request `_meta` identity + `OnClientConnect` vetting); legacy clients keep the `initialize` handshake and `Mcp-Session-Id` session gating
- **MRTR** (Multi Round-Trip Requests): tools can pause and ask the user for confirmation or data via elicitation (`resultType: "input_required"` + opaque `requestState`) — working example in `Demos/031-MCPServer/uTool.ConfirmDemo.pas`
- Bridge `TAiFunctions → IAiMCPTool` — any existing `TAiFunctions` component becomes an MCP server instantly
- API Key authentication, CORS configuration
- `TAiMCPResponseBuilder` for structured responses (text + files + media)
- RTTI-based automatic JSON Schema generation from parameter classes

**MCP Client** — consume any external MCP server from your Delphi app:
- Dual-era probe: tries `server/discover` first and falls back to the legacy handshake automatically; the negotiated mode is exposed in `NegotiatedProtocol`
- `OnInputRequired` event resolves MRTR elicitations (retry loop with `requestState` echo; assigning the handler declares the `elicitation` capability)
- Connect to Claude Desktop tools, filesystem servers, database tools, etc.
- Integrated into `TAiFunctions` component alongside native function definitions

Quick way to try `TMCPClientHttp` against a public MCP server — this example uses [Parallel Search](https://search.parallel.ai/mcp), a **third-party commercial service** (Parallel Web Systems, not affiliated with MakerAI) that exposes `web_search` / `web_fetch` tools over MCP:

```pascal
var
  MCPClient: TMCPClientHttp;
begin
  MCPClient := TMCPClientHttp.Create(nil);
  try
    MCPClient.URL := 'https://search.parallel.ai/mcp';
    if MCPClient.Initialize then
      Writeln(MCPClient.Tools.Text);
  finally
    MCPClient.Free;
  end;
end;
```

> **Note:** at the time of writing (Aug 2026) Parallel offers a rate-limited free tier that works without an API key, but **pricing, limits and availability are set by Parallel and may change at any time** — check [their terms](https://parallel.ai/) before relying on it in production. Be aware that your search queries and any fetched URLs are sent to their servers. MakerAI has no relationship with this service; it is shown only as a convenient public endpoint for testing the MCP HTTP client, and any spec-compliant MCP server works the same way.

> **⚠️ SSE transport deprecation (spec 2026-07-28):** the classic HTTP+SSE transport (GET `/sse` + POST `/messages`) was formally moved to *Deprecated* state by MCP spec revision 2026-07-28 under the project's feature-lifecycle policy, which mandates a minimum 12-month window. Its **earliest possible removal from the spec is July 2027** — actual removal happens in the first spec revision published after that date, at the maintainers' discretion, and may come later. Removal deletes the transport from future spec revisions only: existing MakerAI SSE endpoints keep working between themselves, but third-party clients (Claude Desktop, official SDKs) will progressively drop it. **Use the HTTP or StdIO transports for anything new.** Note that SSE *as a streaming response format* survives inside Streamable HTTP — only the standalone HTTP+SSE transport is being retired.

### 🛠️ ChatTools — AI × Deterministic Capabilities

ChatTools bridge the gap between AI reasoning and real-world operations. They activate automatically based on gap analysis between `SessionCaps` and `ModelCaps`:

| Tool Interface | What it does | Implementations |
|----------------|-------------|-----------------|
| `IAiPdfTool` | Extract text from PDFs | Mistral OCR, Ollama OCR, native PDF parser (`TAiNativePdfTool`, in the `MakerAi.ChatTools` repo) |
| `IAiVisionTool` | Describe / analyze images | Any vision model |
| `IAiSpeechTool` | Text-to-speech / speech-to-text | OpenAI (`gpt-transcribe`, `gpt-4o-mini-tts`), Whisper (local server), Gemini Speech, ElevenLabs |
| `IAiWebSearchTool` | Live web search | OpenAI (Responses API), Gemini |
| `IAiImageTool` | Generate images | gpt-image-2.5, Gemini (Nano Banana), Grok Imagine, Qwen Image |
| `IAiVideoTool` | Generate video | Gemini Veo, Grok Imagine, Qwen Wan (OpenAI shut Sora down; its class stays only for compatibility) |
| `TAiShell` | Execute shell commands | Windows/Linux |
| `TAiTextEditorTool` | Read/write/patch files | Diff-based editing |
| `TAiComputerUseTool` | Control mouse and keyboard | Claude `computer_toolset`, OpenAI `computer` (gpt-6-astra) |

Tools follow a common pattern: `SetContext(AiChat)` + `Execute*()`. They can run standalone, as function-call bridges, or as automatic capability bridges.

### 🖱️ Computer Use — Driving the Desktop

`TAiComputerUseTool` lets a model look at the screen and drive the real mouse and
keyboard. One canonical action model is shared by every provider, so the same
executors and the same event handlers work regardless of who is driving:

```pascal
// Opt-in: no model ships with cap_ComputerUse enabled
TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-6-astra',
  'ModelCaps',   '[cap_Image, cap_Reasoning, cap_ComputerUse]');
TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-6-astra',
  'SessionCaps', '[cap_Image, cap_Reasoning, cap_ComputerUse]');

AiConn.ChatTools.ComputerUseTool := MyComputerUseTool;  // + OnExecuteAction / OnRequestScreenshot
```

| Provider | Tool declared | Action shape |
|----------|---------------|--------------|
| OpenAI (`gpt-6-astra`) | `computer` (no parameters) | one `computer_call` carrying an **array of actions** |
| Claude (opus-4-8 / family 5) | `computer_toolset_20260801` (no parameters) | **17 individually named tools**, several `tool_use` blocks per turn |
| Gemini | `computerUse` (`ENVIRONMENT_BROWSER`) | one function call per action, coordinates normalised 0–1000 |

The two APIs refreshed in 2026 (OpenAI and Claude) converged independently on the same
design: the tool declares **no screen dimensions** — the model infers them from the
screenshot — and coordinates come back as **pixels of the image you sent**.
`ScreenWidth`/`ScreenHeight` are therefore local-only now: they must match the image you
actually submit, because that is the divisor used to translate coordinates back to
physical pixels.

- **Safety**: `OnSafetyConfirmation` gates risky actions (human-in-the-loop); denying
  is the default when no handler is assigned
- **Executors**: Windows VCL and FMX (Win32 `SendInput`) and **Linux/X11**
  (`TAiLinuxExecutor`, xdotool + scrot), all runtime-tested. A macOS executor (CGEvent) is
  written but has not yet been compiled or tested on macOS. They are interchangeable: an
  executor is just the pair of handlers, so `TAiComputerUseTool` itself is platform-agnostic
- **Delegation**: fill `ToolCall.Response` from `OnCallToolFunction` and the driver will not
  execute locally — a headless process can forward the call to whoever owns a screen
- **Capture area**: `AreaLeft`/`AreaTop`/`AreaWidth`/`AreaHeight` select a sub-region;
  multi-monitor works but is still lightly tested
- **Demos**: `066-ComputerUseTest` (Windows, real desktop — `-provider=openai|claude|gemini`,
  `-prompt=...`, `-autorun`, `run.log` next to the executable), `082-ComputerUsePassthru`
  (who executes the call: delegated, missing tool, local — self-verifying, does not touch the
  screen) and `083-ComputerUseLinux` (real agentic loop on Xvfb)

### 🎙️ Realtime Voice — WebSocket STT & Speech-to-Speech

A parallel component stack for live audio over WebSocket, with the same universal-connector pattern as chat (`TAiRealtimeConnection.DriverName`):

| Driver | Type | Endpoint |
|--------|------|----------|
| `TAiOpenAiRealtimeSTT` | STT only — streaming transcription (`gpt-live-transcribe` default) | OpenAI Realtime API |
| `TAiOpenAiRealtimeTranslate` | **Simultaneous translation** — translated text and voice in one stream | OpenAI Realtime API |
| `TAiOpenAiLiveChat` | **GPT-Live full-duplex voice** (`gpt-live-1`) — can be interrupted, delegates the thinking to a Responses model or any `TAiChatConnection` (`DelegateChat`) | OpenAI Realtime API |
| `TAiGrokRealtimeChat` | **Full-duplex speech-to-speech** — the user talks, Grok answers with voice | xAI `wss://api.x.ai/v1/realtime` |
| `TAiQwenRealtimeChat` / `…STT` / `…Translate` | Voice conversation, live transcription and simultaneous translation (`DriverName` `Qwen` / `QwenSTT` / `QwenTranslate`) | Alibaba DashScope |
| `TAiQwenRealtimeTTS` | Streaming text-to-speech: speak an LLM answer while it is generated | Alibaba DashScope |
| `TAiMakerAiRealtimeChat` | STT + LLM + TTS in one socket | MakerAI server |
| `TAiGeminiRealtimeSTT` | STT (planned) | Gemini Live |

- **`DriverParams`** on `TAiRealtimeConnection` — driver-specific settings (`Voice`, `Instructions`, `TargetLanguage`…) as `Property=Value` lines, applied to whichever driver is active
- **`TAiRealtimeVoiceBase`** — shared base for full-duplex drivers: `OnAssistantText[Delta]`, `OnAudioChunk`, `OnAudioDone`, on top of the STT events (`OnTranscriptDelta/Completed`, `OnSpeechStarted/Stopped`)
- **Voice function calling** (Grok): plug a `TAiFunctions` component and the model invokes your Delphi functions mid-conversation
- **Session resumption, binary audio transport, ephemeral tokens** for mobile/browser clients (Grok)
- **Audio pipeline**: `TAIVoiceMonitor` (mic) → thread-safe PCM16 resampler → provider rate (24 kHz); push audio from any source via `SendAudioChunk`
- **Pure-Pascal WebSocket stack** (`TAiWSClient`, RFC 6455) with pluggable TLS: Windows SChannel (zero DLLs), OpenSSL (Linux/macOS), `javax.net.ssl` (Android). The POSIX transport verifies the server certificate — chain **and** hostname — with `InsecureSkipVerify` as an explicit opt-out

### 🌐 A2A — Agent-to-Agent Protocol

First Delphi implementation of **A2A 1.0** (Linux Foundation), certified against the official TCK (MUST 89/89, SHOULD 8/8):

- **`TAiA2AServer`** exposes any `TAIAgentManager` graph as an A2A agent: Agent Card at `/.well-known/agent-card.json`, `SendMessage` / `GetTask` / `CancelTask` / `ListTasks`, SSE streaming, push notifications, bearer auth, a pool of managers for concurrent tasks, and human-in-the-loop (a suspended node becomes `input-required`)
- **`TAiA2AClient`** consumes remote agents; **`TAiA2ARemoteAgentTool`** federates a graph node to a remote agent, and **`TAiA2AAgentTool`** exposes a remote agent to a chat as a function
- **Discovery through PPM:** agents published as `agent` packages are found and installed by name (`InstallAgentFromPPM`) or by the model itself with AutoMCP (`call_agent`) — see *PPM — A2A agents as packages* above
- Demos: `072-A2AFederation`, `074-A2AOrchestration`, `079-A2ASutAgent`, `080-A2APushNotifications`, `093-A2AChatAgentTool` (an agent as a chat tool) and `099-VozPedidoLadera` (voice order through federated agents)

### 📚 Skills — Reusable Instructions on Demand

Skills use the **SKILL.md** format (Claude Agent Skills / PPM registry): YAML frontmatter with a name and when to use it, plus Markdown instructions. The same file works in a chat, an agent node or a prompt:

- **`TAiSkills`** — plug it into `TAiFunctions` and the model sees only a one-line catalog in `use_skill`; it loads the instructions it needs, and reads supporting files with `read_skill_file` (confined to the skill folder). Works with any provider that supports function calling
- **`TAiSkill` + `TLLMNode.Skill`** — a skill as the fixed personality of an agent node
- **`TAiPrompts.ApplySkill`** — copy a skill into any `SystemPrompt`
- Load from a folder (`Skills\<name>\SKILL.md`), a file, code, or the PPM registry (`LoadFromPPM('skill-code-review')`)
- Guide: [`Docs/Version 3/uMakerAi-Skills.EN.md`](Docs/Version%203/uMakerAi-Skills.EN.md) · Demo: `091-Skills`

### 🛡️ Guardrails & Evals

- **`TAiGuardrails`** — assigned to `TAiFunctions.Guardrails`, it checks every tool call (local, MCP or AutoMCP) **before** it runs: allow/block lists with wildcards, forbidden argument patterns, a pluggable semantic `Classifier` and an `OnCheckToolCall` veto. A blocked call never executes; the model gets the reason and can replan
- **`ChatTools.PromptGuard`** — the input-side guardrail: checks the user's message before it reaches the model
- **`TAiEvalRunner`** — fluent test cases (`ExpectContains`, `ExpectRegex`, `ExpectEquals`, `ExpectJudge` with an LLM judge, `ExpectScore` with a calibrated scorer) against any target, with text and JSON reports. The framework's own regression suite (144 cases, no API keys) is built on it
- Demo: `073-GuardrailsEvals`

### 📈 Observability — OpenTelemetry

**`TAiTelemetry`** exports traces over OTLP/HTTP (collector on `localhost:4318`: Jaeger, Grafana Tempo, Langfuse, Arize Phoenix) following the OpenTelemetry **GenAI semantic conventions**: chat turns with token usage, tool executions, agent graphs and nodes, RAG searches, MCP and A2A requests, and skill loads. The W3C `traceparent` travels through MCP `_meta` and A2A, so a call that crosses processes stays in one trace. Zero overhead when disabled. Validated against a real Jaeger.

### 💾 Memory — TAiMemory

Persistent semantic memory on SQLite: FTS5 lexical search, optional embeddings with hybrid RRF fusion, importance and decay, TTL, links between memories, and **namespaces** that isolate agents or projects. `Context(prompt)` builds a memory block ready for the system prompt, and `uMakerAi.Memory.MCP` exposes it as MCP tools. Demo: `076-Memory`.

### 🧭 Jev — Calibrated Decisions

**`TAiJev`** wraps Jev (TypeSafe AI), a model that does not generate text: it answers typed questions (choice, score, yes/no) with **calibrated probabilities**, for the fast and cheap decisions that would otherwise cost a full LLM call. Adapters plug it into the framework: agent routing (`TAiJevRouterTool`), SmartDispatch, tool-call and input guardrails, eval scoring, RAG reranking with injection filtering, bulk labeling (`TAiJevBatchLabeler`) and model routing (`TAiJevModelRouter`, the cheapest model that can handle each request), with usage metering for billing. Needs `TYPESAFE_API_KEY`. Demos `084`–`088`.

### ⚙️ Model Capabilities — TAiCapabilities

Introduced in v3.3 and refined in v3.4, the `TAiCapabilities` system replaces all manual feature flags with two declarative sets:

- **`ModelCaps`** — what the model natively supports (e.g., `[cap_Image, cap_Reasoning]`)
- **`SessionCaps`** — what the session needs
- **Gap = SessionCaps − ModelCaps** — any missing capability activates an automatic ChatTool bridge; for example, a text-only model with `cap_GenImage` in `SessionCaps` automatically routes image generation requests through a `gpt-image` or Gemini (Nano Banana) bridge

```pascal
// Default capabilities for all models of a provider
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'ModelCaps',   '[cap_Image, cap_Pdf]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'SessionCaps', '[cap_Image, cap_Pdf, cap_GenImage]');

// Per-model override (e.g., a reasoning model)
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model', 'ModelCaps',    '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model', 'ThinkingLevel', 'tlMedium');
```

Available capabilities:
- **Input / understanding:** `cap_Image`, `cap_Audio`, `cap_Video`, `cap_Pdf`, `cap_WebSearch`, `cap_Reasoning`, `cap_CodeInterpreter`, `cap_Memory`, `cap_TextEditor`, `cap_ComputerUse`, `cap_Shell`
- **Output / generation** (a gap activates a ChatTool or a dedicated endpoint): `cap_GenImage`, `cap_GenAudio` (TTS), `cap_GenVideo`, `cap_GenReport`, `cap_ExtractCode`

`ThinkingLevel` controls reasoning depth across the full effort ladder: `tlNone`, `tlMinimal`, `tlLow`, `tlMedium`, `tlHigh`, `tlXHigh`, `tlMax` (`tlDefault` = let the provider decide). Each driver maps it to what its models accept — for example Gemini 3 has no `minimal` and cannot turn thinking off, so both are sent as `LOW`.

### 🎨 FMX Visual Components

**Current (since v3.4) — Skia-native, virtualized, zero FMX child controls:**

- **`TAIChatView`** — single-canvas virtualized conversation renderer; only visible messages are painted; supports multi-message text selection, dark/light theme, context menu, long-press (mobile), copy-button feedback with timer, and a scrollbar that doesn't interfere with content
- **`TAIChatInput`** — fully Skia-painted input bar with custom dropdown overlay (no `TPopupMenu` required), voice-mode indicator, file attachment chips, and `TAIVoiceMonitor` integration; layout adapts from 1 to N attachment chips automatically

Both work with every provider and with streaming; messages also carry a footer, action buttons and collapsible task steps (see *Chat UI* in What's New).

**Deprecated — classic components:** `TChatList`, `TChatBubble`, `TChatInput` and `uMakerAi.Utils.ScreenCapture` are marked `deprecated` and kept only so existing applications still compile; no demo uses them since 4.0. When migrating, note that `TAIChatView.AddBubble(..., AInbound)` treats `AInbound = True` as the **user** (prefer `AddUserMessage` / `BeginAssistantMessage` + `AppendToken`), and that `ActiveMediaFile` has no equivalent: use `OnAttachmentOpen`. Demos `012-ChatAllFunctions` and `041-GeminiVeo` show the migrated code.

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
Source/Memory
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
2. `Source/Packages/MakerAi.RAG.Drivers.dpk` — database back-ends via FireDAC: PostgreSQL/pgvector, SQL Server and SQLite for RAG, graph RAG on PostgreSQL, DB checkpoints for agents, and `TAiMemory`
3. `Source/Packages/MakerAi.UI.dpk` — FMX visual components (requires Skia)
4. `Source/Packages/MakerAiDsg.dpk` — Design-time editors (requires VCL + DesignIDE)

Open `Source/Packages/MakerAiGrp.groupproj` to compile all packages at once.

### API Keys

API keys are resolved from environment variables using the `@VAR_NAME` convention. On `TAiChatConnection` the key goes through `Params` (it has no `ApiKey` property); direct drivers (`TAiOpenChat`, `TAiClaudeChat`...) and `TAiEmbeddingConnection` do have an `ApiKey` property:

```pascal
AiConn.Params.Values['ApiKey'] := '@OPENAI_API_KEY';    // reads OPENAI_API_KEY from environment
AiConn.Params.Values['ApiKey'] := '@CLAUDE_API_KEY';    // reads CLAUDE_API_KEY
AiConn.Params.Values['ApiKey'] := '@GEMINI_API_KEY';    // reads GEMINI_API_KEY
AiConn.Params.Values['ApiKey'] := '@GROK_API_KEY';      // reads GROK_API_KEY (xAI chat and Grok Voice)
AiConn.Params.Values['ApiKey'] := 'sk-...';             // or set a literal key directly
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

Open `Demos/DemosVersion31.groupproj` to load all 63 demo projects in the IDE. Every demo folder has a `CLAUDE.md` (or a section in `Demos/CLAUDE.md`) with what it shows, how to run it and which API keys it needs. Demos from earlier versions that no longer apply live in [`DeprecatedDemos/`](DeprecatedDemos/README.md), for reference only.

**New to MakerAI?** Start with the *Café La Ladera* series (`094`–`105`): twelve small demos, one per step, from the first streaming chat to a voice order that travels through federated agents. The book projects in `Demos/Libro/` follow the same path chapter by chapter.

### Café La Ladera series (start here)

| Demo | What it shows |
|------|---------------|
| `094-ChatStreaming` | Console chat: system prompt, streaming with time to first token, history, save/load the conversation |
| `095-ChatViewLadera` | FMX chat with `TAIChatView`/`TAIChatInput`: streaming bubbles, reasoning step, token footer, images, PDF and voice notes |
| `096-ChatToolsLadera` | Native web search, a text editor confined to a folder and an allow-listed shell, including the attempt that gets blocked |
| `097-ChatMemoryLadera` | `TAiMemory`: the customer's preferences are remembered in a new session with a fresh chat |
| `098-AgentGraphLadera` | An order through an agent graph: fan-out, `jmAll`, expression and conditional edges, and a `TLLMNode` (`--sin-llm` without a key) |
| `099-VozPedidoLadera` | The summit: a voice order through GPT-Live, a chat that calls the Orders agent over A2A, whose inventory node is federated to the Warehouse agent |
| `100-MCPLadera` | The same `.exe` is an MCP stdio server (also for Claude Desktop/Code and as a PPM package) and a chat that launches it and uses its tools |
| `101-SkillsLadera` | Three SKILL.md skills loaded on demand with `TAiSkills` |
| `102-AgenteLadera` | A `TLLMNode` with a skill and two tools serves a customer; a second node reviews the answer |
| `103-AprobacionLadera` | A large order waits for approval, the program exits, and `--responder` resumes it from the checkpoint on disk |
| `104-GuardrailsLadera` | Guardrails on the agent's tools (maximum discount, blocked cancellation) and evals with a judge model |
| `105-EnrutadorLadera` | Jev routes each customer message to the right agent, without an LLM, with time and cost. Needs `TYPESAFE_API_KEY` |

### Chat, multimodal and providers

| Demo | What it shows |
|------|---------------|
| `012-ChatAllFunctions` | Full-featured multimodal chat: images, audio, streaming and tools across providers |
| `041-GeminiVeo` | Video generation with Google Veo |
| `089-QwenShowcase` | Everything Qwen (Alibaba Model Studio) with one key: chat, vision, image editing, voice, translation, embeddings + rerank, realtime |
| `090-Varios/ListAllModels` | Lists the models available from each provider |
| `090-Varios/CompareEmbeddings` | Compares embedding models side by side |
| `090-Varios/Dalle_Generate` | Image generation |
| `090-Varios/VoiceMonitorAndWhisperDemo` | `TAiVoiceMonitor` + speech-to-text |

### RAG and memory

| Demo | What it shows |
|------|---------------|
| `021-RAG+Postgres-UpdateDB` | Build a vector RAG database on PostgreSQL + pgvector |
| `022-1-RAG_SQLite` | Lightweight vector RAG on SQLite |
| `023-RAGVQL` | VQL, the vector query language |
| `025-RAGGraph` | Knowledge-graph RAG with GQL queries |
| `026-RAGGraph-Basic` | The essentials of graph RAG |
| `027-DocumentManager` | Document manager over graph RAG, 100% local with Ollama |
| `070-ChatRAGTools` | Multi-provider chat + SQLite RAG + a tool that draws charts, in one form |
| `076-Memory` | `TAiMemory` on SQLite/FTS5; hybrid search when `OPENAI_API_KEY` is set |
| `077-RagPostgresConsole` | Headless vector RAG on pgvector with local Ollama embeddings, no API key. Windows and Linux64 |
| `078-RagChatOllama` | Complete local RAG: pgvector retrieval answered by Ollama, each question with and without context |

### MCP

| Demo | What it shows |
|------|---------------|
| `031-MCPServer` | Multi-protocol MCP server (HTTP, SSE, StdIO). Also compiles for Linux64 |
| `032-MCP_StdIO_FileManager` | File manager MCP server in three projects: StdIO, HTTP and SSE |
| `036-MCPServerStdIO_AiFunction` | StdIO MCP server built from `TAiFunctions` |
| `037-MCPServerRAG` | Semantic RAG served over MCP, on SQL Server 2025 or PostgreSQL + pgvector |
| `075-MCPElicitation` | Client side of MCP elicitation (MRTR) with `OnInputRequired` |

### Agents and A2A

| Demo | What it shows |
|------|---------------|
| `051-AgentDemo` | Visual fan-out + join: one task calls the model while another runs locally, in parallel |
| `052-AgentConsole` | Conditional and fork/join flows with the fluent API |
| `054-AgentCheckpointDB` | Durable execution: suspend and resume a graph with `TAiDatabaseCheckpointer` |
| `072-A2AFederation` | A2A 1.0 federation: expose a graph as an agent, consume it, delegate a local node to it (no LLM; `--otel` for tracing) |
| `074-A2AOrchestration` | A2A orchestration: manager pool, human-in-the-loop, federated HITL, non-blocking tasks |
| `079-A2ASutAgent` | The agent used to certify MakerAI against the official A2A TCK (MUST 89/89, SHOULD 8/8) |
| `080-A2APushNotifications` | A2A push notifications: the agent posts to your webhook when it finishes |
| `081-AgentChatHITL` | Human-in-the-loop inside the chat, without modal windows, resumable after restarting the app |
| `093-A2AChatAgentTool` | A chat that calls a remote A2A agent as a tool; `--ppm` discovers it in a PPM registry |

### Guardrails, evals and Jev (TypeSafe AI)

| Demo | What it shows |
|------|---------------|
| `073-GuardrailsEvals` | `TAiGuardrails` (the blocked tool never runs) and `TAiEvalRunner` with a report and exit code, no LLM |
| `084-JevRouter` | Route queries to specialized agents before spending an LLM call |
| `085-JevDispatchGuard` | Jev in SmartDispatch and in tool and input guardrails |
| `086-JevEvalsRag` | Jev as an eval scorer and as a RAG reranker that discards injections |
| `087-JevBatchLabeling` | Bulk labeling: 126 accounting entries against 225 ledger accounts in seconds |
| `088-JevModelRouter` | Send each request to the cheapest model that can handle it |

Demos `084`–`088` need `TYPESAFE_API_KEY`.

### Skills

| Demo | What it shows |
|------|---------------|
| `091-Skills` | SKILL.md skills three ways: on demand in a chat (`TAiSkills`), as the base of an agent node (`TAiSkill`), copied into the prompt (`TAiPrompts.ApplySkill`) |

### Voice and audio

| Demo | What it shows |
|------|---------------|
| `060-ElevenLabsTTS` | Text-to-speech with ElevenLabs |
| `061-LoopbackAudioCapture` | Capture what the PC is playing (WASAPI loopback) to a WAV file |
| `064-VoiceBridgeDiarized` | Meeting voice bridge with speaker diarization: a stable label and a distinct voice per participant |
| `071-VoiceBridgeTranslate` | Live call translation with `gpt-realtime-translate`: speak Spanish, the meeting hears English, and the other way round |
| `092-GPTLiveVoice` | OpenAI GPT-Live full-duplex voice: interruptible, with delegation to Responses or any `TAiChatConnection`, local functions and RAG over MCP |

### Computer Use

| Demo | What it shows |
|------|---------------|
| `066-ComputerUseTest` | Computer Use on the Windows desktop (three projects, including a manual loop against a server) |
| `082-ComputerUsePassthru` | Who executes a `computer_call`: a remote client, nobody, or a local executor. Self-verifying; never touches the screen |
| `083-ComputerUseLinux` | Computer Use on Linux/X11 over Xvfb, the headless counterpart of `066` |

### Integration checks against real APIs

More of these, which used to be demos 014, 068 and 069, now live in [`Tests/Integration/`](Tests/Integration/CLAUDE.md) with their own group project.

Small consoles with asserts that reproduce a specific issue against the live API (they need the provider's key). For the in-process suite without keys, see `Tests/RegressionSuite`.

| Demo | What it checks |
|------|----------------|
| `067-PromptCacheTest` | Claude prompt caching: the second request reads from the cache |

### The book: *Inteligencia Artificial con Delphi* (4.0 edition)

`Demos/Libro/cap03-primer-chat` … `Demos/Libro/cap37-los-demas-puentes`: one folder per chapter, 64 projects in all.

---

## 🔄 Changelog

### v4.0.0 (unreleased)
- Fix (agents): **`OnSuspend` could call a freed manager** — it is queued to the main thread with `TThread.Queue`, and the closure read `Self`/`OnSuspend` when the queue was served; if the owner freed the manager first (an immediate resume in a server), that was an AV. A queued `OnSuspend` is now dropped once its manager is destroyed. Case `agents.suspend.after-free`
- ⚠️ Fix (MCP): **`serverInfo.version` was always `1.0.0`** whatever program hosted the server. `ServerVersion` now defaults to empty, which reports the host executable's product version (Windows; `1.0.0` when it has none or on other platforms); an assigned `ServerVersion` is sent as before. `TAiMCPLogicServer.EffectiveServerVersion` returns the value actually sent
- New: **`TAiA2AServer` can host an agent it does not run** — `Executor: IAiA2AExecutor` hands each task to the host's own engine (an orchestrator with its own locks, policy, run log, spend caps or suspensions persisted on disk) and the host pushes the state back with `AReport` (`TAiA2AExecUpdate.Completed` / `Failed` / `InputRequired`); `input-required` frees the slot and the next `SendMessage` with that `taskId` arrives as `Execute` with `Resume=True`. Cancel marks the task first and then tells the host, so a late report never resurrects it. **`AgentPaths`** serves several agents on one port under `/<agent>/` (own card and URL each, tasks isolated: another agent's `taskId` is `TaskNotFound`), resolved and customized by `OnResolveAgent`. **`BindAddress`** (or `A2A_BIND_ADDRESS`) binds to one local IP — it always listened on 0.0.0.0. Fix: the bridge between `SendMessage` and the `message` arm of the response was a server field, so two concurrent calls could swap their structured answers; it is per thread now. Regression cases `a2a.executor.*`, `a2a.paths.multi-agent`, `a2a.bind-address`
- New: **localization groundwork** — `uMakerAi.Localization` (`TAiLocale`, `AiStr` / `AiFmt`) looks texts up by key in per-language tables (`uMakerAi.Lang.En`, the base, `uMakerAi.Lang.Es` and `uMakerAi.Lang.Pt`, Brazilian Portuguese registered as `pt`, so `pt-BR` and `pt-PT` both use it). The language comes from `MAKERAI_LANG`, then the OS (Windows UI language; `LC_ALL`/`LC_MESSAGES`/`LANG` on Linux/macOS; `Locale.getDefault` on Android), and can be switched at runtime with `TAiLocale.Language := 'es'`; lookup falls back region → language → English → the key. An application adds its own language with `TAiLocale.RegisterTable('pt', [...])`. Framework texts move to the tables in stages: **done** for `TAIChatView` (context menu, code-block and media buttons, steps, "Copy conversation" role labels), `TAIChatInput` (attachment menu, file dialog filter) and the design-time editors (MCP client editor, connection editors, About), which now follow the OS language. Internal prompts are **not** localized: they are written in English and tell the model to answer in the user's language (see *Behaviour changes*). Exception messages are in the tables too (437 keys across Core, chat drivers, Tools, RAG including the VQL/GQL parser errors, Agents, MCP client, Embeddings, Utils and WebSocket). Errors that an MCP server sends back over JSON-RPC stay in English, since the other side is usually a model. The status messages that reach `OnStateChange` and the MCP client's `OnStatusUpdate` ("Sending request...", "Generating video...", "Calling tool: ...") are translated too: 568 keys in English, Spanish and Portuguese. The deprecated `TChatList`/`TChatBubble`/`TChatInput` are not translated. Regression cases `locale.*` check every table has the base keys and the same `Format` specifiers; `Tests/Localization/hardcoded_strings.py` reports the texts still hard-coded
- New: **A2A agents as PPM packages** — package type `agent` (Agent Card + URL, no code). `TAiFunctions.SearchPPMAgents` / `GetAgentCardFromPPM` / `InstallAgentFromPPM`; AutoMCP lists agents in `ppm_search`, installs them with `ppm_install` and adds `call_agent` when `uMakerAi.A2A.Client` is in the project (`TAiPPMAgentHandler` keeps `uMakerAi.Tools.Functions` free of A2A). Allowed/Blocked, `OnAutoMCPRequest` and guardrails apply; an agent key comes only from `PPM_AGENT_KEY_<NAME>`. `TAiA2AAgentTool.Ask` is now public. Private registries only: PPMServer with `PPM_ALLOW_AGENTS=1` and `migration_003_agent_type.sql`; the public `registry.cimamaker.com` does not accept agents
- New: **company registry example** — `Demos/Libro/cap33-registry-corporativo/registro-local`: PPMServer on `127.0.0.1:8766` with the *Café La Ladera* packages (`mcp-bodega-ladera` = demo 100's MCP server, a skill, a prompt and the A2A catalog agent); `RegistryLadera --automcp` points AutoMCP at it
- New: demo `093` `--ppm [url]` (the chat discovers the agent in the registry) and `--solo-agente [--segundos N]`; demo `100` `--trafico` (every JSON-RPC message) and `--http` (`TAiMCPHttpServer` + `tpHttp`), `--protocol stdio` for `InstallMCPFromPPM`; demo `104` `--detalle` (question and answer of each eval)
- Fix: **Claude Haiku 5.5 with `ThinkingLevel = tlNone` now turns thinking off** (`thinking: disabled`); it was sent as effort `low`, which only applies to Opus/Sonnet 5.5 and Fable — a 250-token JSON answer went from ~1,200 to ~300 output tokens
- Fix: **Claude `code_execution`** — a data file returned by a tool (Excel, Word…) now reaches the sandbox (`container_upload` next to the `tool_result`); before, only images and PDFs did
- New: **MiniMax driver** — `TAiMiniMaxChat` (`DriverName='MiniMax'`, `@MINIMAX_API_KEY`), OpenAI-compatible endpoint `https://api.minimax.io/v1/`. `reasoning_split` always on (reasoning in `reasoning_content`, re-sent in the history), thinking `adaptive`/`disabled` from `cap_Reasoning` (`MiniMax-M3.1-Flash` cannot disable it and is left at the API default), `reasoning_effort` only on M3.1-Flash (`tlNone` → `low`, since `none` is rejected; registry default `tlMedium` instead of the API's `max`), `max_completion_tokens`, `stream_options.include_usage` when streaming, no sampling parameters (ignored by the API). Registered: `MiniMax-M3` (default), `MiniMax-M3.1-Flash`, `MiniMax-M2.7/2.5/2.1(-highspeed)`, `MiniMax-M2`. Package, registry and palette icon. Regression case `chat.minimax.request`. *Not runtime-tested yet (no API key)*
- ⚠️ Fix: **`claude-haiku-5-5` returned 400** — the driver treated it as a legacy Haiku and sent `thinking.budget_tokens` and `temperature`. It is now adaptive-only (`effort` from `ThinkingLevel`, default `medium`), accepts mid-conversation `system` messages, and `RefusalFallbackModel` is skipped with a log line (Haiku 5.5 has classifiers but no server-side fallback; a fallback list is a 400). Forced `tool_choice` still works on it. Registered and tested live (7/7). The driver default stays `claude-haiku-4-5`
- Retired (Anthropic): **`claude-sonnet-4-20250514` and `claude-opus-4-20250514`** (and their `-4-0` names) answer `not_found_error`; they are aliases of `claude-sonnet-5-5` / `claude-opus-5-5`
- New: **`gpt-6.1-sol`** ($2/$10, cache read $0.10) and **`gpt-6.1-terra`** ($1/$5) registered; both reject `temperature`/`top_p`, which the driver already omits for GPT-6. Tested live (9/9)
- ⚠️ Fix: **OpenAI effort per model** — `gpt-6-astra` and `gpt-6.1-sol` reject `reasoning.effort: none` (400), so `tlNone` is sent as `low`; `gpt-5.2/5.4/5.5-pro` accept only `medium`/`high`/`xhigh` (`gpt-5.5-pro` with `tlLow` was a 400), so lower levels become `medium` and `tlMax` becomes `xhigh`
- Retired (OpenAI): **`o3-pro`** → alias of `gpt-5.5-pro`; **`o3-deep-research` / `o4-mini-deep-research`** → `gpt-6.1-sol` with web search and code interpreter; **`dall-e-2` / `dall-e-3`** → `gpt-image-1` in the chat driver; `gpt-4o-audio-preview` / `gpt-4o-mini-audio-preview` removed (their `gpt-audio*` successors speak audio only through Chat Completions, while the driver uses the Responses API). `TAiDalleImageTool` with `imDallE2`/`imDallE3` still sends the retired names
- Fix: **`gemini-4-argon` was sent `thinkingBudget`** — the driver chose the thinking format with `Contains('gemini-3')`, so a 4.x model fell to the Gemini 2.5 path. It now goes by model version (≥ 3 → `thinkingLevel`). `gemini-4-argon` registered (unified 1M window, 32K output per non-streaming call, explicit `cachedContents` unsupported, 20-minute timeout), plus **`gemini-nano-banana-2.1`** and `gemini-omni-1.1-flash`. Deprecated and already rerouted by Google: `gemini-3.5-flash` → `gemini-3.6-flash`, `gemini-3.7-flash` → `gemini-3.8-flash` (now registry aliases); `gemini-2.5-computer-use-preview-10-2025` (shut down) → `gemini-3.8-flash`. ⚠️ The `imagen-4.0-*` aliases now point to Nano Banana 2.1 (`gemini-3.1-flash-image` is deprecated). Note: since 2026-10-26 new **paid** Gemini projects store `generateContent` requests by default (`store=true`); the driver does not send `store`. *Checked against the official docs; not runtime-tested (no Gemini account)*
- ⚠️ Fix: **Kimi rejected any `Temperature` other than 1** — every current model (`kimi-k2.6`, `k2.7`, `k3`, `k3.1`) accepts only `temperature = 1` and `top_p = 0.95`, but the base request sent the component's value. `TAiKimiChat` now omits sampling for the `kimi-k*` family. New: **`kimi-k3.1-pro`** ($2/$10, 1M, vision + reasoning) and `kimi-k3.1-pro-highspeed` ($4/$20). Retired: `kimi-k2.5`, `kimi-latest` and the six `moonshot-v1-*` → aliases (`kimi-latest` → `kimi-k3.1-pro`, the rest → `kimi-k3`). Tested live
- Fix: **GLM sent no `reasoning_effort` for `tlNone`/`tlMinimal`/`tlXHigh`/`tlMax`**, so the API used `max` (the most expensive); they now map to `low` / `max`. New: **`glm-5.3-flash`** and `glm-5.3-flashx` (MoE 320B/18B, MIT weights, 1M, native vision, forced thinking like `glm-5.3`). First live test of the GLM driver (text, vision, thinking on and off)
- New: **fluent agent API completed** — `AddEdge` adds targets to the existing link (it replaced `StartNode.Next`, orphaned the old link and a fluent fan-out ran only its last branch); new `AddNode(node)`, `AddToolNode`, `AddExpressionEdge`, `AddFallbackEdge` (`NextNo`) and `SetJoinMode`; construction errors raise `EAiGraphError`. `TAIAgentsNode.ExecuteNode` is virtual and protected and `TLLMNode` overrides it instead of reimplementing `DoExecute`; `OnEnterNode` runs after the join. `TLLMNode` gains `Guardrails`, gets the manager as `Owner` and sets capabilities through the typed channel
- ⚠️ Fix: **typed `ModelConfig` setters** (`ModelCaps`, `SessionCaps`, `ThinkingLevel`, `Tool_Active`) pin the field even when the value equals the current one — `Tool_Active := True` was overwritten by the driver registry
- Fix: **forms saved with older versions raised `EReadError`** (demos 012, 021, 051): `TAiChat.DefineProperties` reads the properties that are no longer published (`NativeInputFiles`, `Logprobs`, `Top_p`, `Voice`…), applies those with a destination and drops the rest. `TAiMCPStdioServer.WaitForInputClosed`; `Stop` releases the reader thread. Sample VQL in the README and guides fixed (it did not parse)
- Fix: **a `Model` saved in the form was lost on load** — `TAiChatConnection.Loaded` created the chat without it, so `DriverName=Claude` + `Model=claude-sonnet-5-5` started with `claude-haiku-4-5`. Case `conn.form-model-nondefault`
- Fix: **Claude Computer Use ran the actions of a turn in parallel** and in dictionary order — with `computer_toolset_20260801` two `type` actions mixed their letters and a five-field form took 114 actions and 322 s. A batch with a screen action now runs serially, in arrival order, on the parsing thread (14 actions, 26 s); normal tool calls stay parallel. `TAiComputerUseTool` honours the `repeat` of `key`. Case `chat.claude.computer-use-sequential`
- Fix: **the trace of a turn was split across threads** — tool calls (one `TTask` each), the tool-calling continuation and OpenAI's streaming function calls did not carry the turn's `chat` span. New `TAiTelemetry.ContextEnter` / `ContextLeave` / `CurrentTraceParent` (helpers `AiTraceContextEnter` / `Leave`); a turn delegating to a graph went from 5 traces to one of 14 spans. Case `telemetry.task-inherits-trace`
- Fix: **input bridges** — during the Phase 1 bridges (audio, image or PDF the model cannot read) the tool runs synchronously and its `DoData`/`DoDataEnd` are ignored; before, the SpeechTool fired `OnReceiveDataEnd` with the raw transcription and the UI closed the turn before the model answered. **Web search** results are injected as context for the model (they were overwritten by Phase 3) and `cmWebSearch` no longer searches twice. `TAiOpenAiWebSearchTool` moved to `/v1/responses` with `web_search` (default `gpt-6-luna`), since `gpt-4o-mini-search-preview` was retired. ElevenLabs and Gemini TTS generate inline (a synchronous chat returned without audio). Tools release their weak chat reference when replaced or when the chat is freed (AV on the next direct call). `TAiOllamaOcrTool` strips deepseek-ocr control tokens
- ⚠️ Fix (security): **`TAiShell` checked only the first token** — with an allow-list of `[cd, dir]`, `cd x && del /f /q *` passed. Every segment (`&`, `|`, `;`, new line) is now checked without quotes, `^`, path or extension; in `ssmAllowList` mode `>`, `<`, `` ` `` and `$( )` are rejected
- Fix: **a blocked message stayed in the history** — a prompt rejected by the sanitizer or the `PromptGuard` reached the model on the next turn (`TAiChat.DiscardBlockedMessage`). The sanitizer recognises injection phrases in Spanish as well
- Fix (OpenAI): `apply_patch_call` was not executed in **streaming** (*No tool output found for apply patch call*), and `create_file` went to `apply_diff` on a missing file, so the model could not create files; `EnforceStrictSchema` (now public) walks `items`, `anyOf`/`oneOf`/`allOf` and `$defs` (an array of objects was a 400), and tools with a free-form object are sent with `strict: false`. New `PromptCacheMode` → `prompt_cache_options.mode` (GPT-5.6+)
- Fix: **`Mistral Small 4` only accepts `reasoning_effort` `none`/`high`** — `tlLow` → `none`, `tlMedium`/`tlHigh` → `high` (`medium` was a 400). `TAiRagGraph.SearchText` returned only the header; it now returns the nodes and their neighbours
- ⚠️ Fix: **`TAiChat.Run` waits (up to 15 s) for the previous HTTP request to close** — its late close freed the new request's stream (intermittent *Stream read error*); it does not wait when called from the thread delivering the previous one. **`TAudioPushStream`** no longer deadlocks: `waveOutReset` is called without the lock and buffers are fed from a thread (`CALLBACK_EVENT`) instead of the winmm callback
- Fix (evals): **`ExpectJudge` sent the judge only the answer and the criterion**, without the case's question, and reused the judge's history between cases; each verdict now includes the question and starts a new chat
- Fix (MCP): `TMCPClientSSE` retries a POST dropped by the server (up to 3 times, not `tools/call`), exposes `IsConnected` (false when the SSE stream dropped) and streams over `https` with the credential on the GET too; freeing a running `TAiMCPSSEHttpServer` raised an AV (double `Stop`)
- Fix (WebSocket): **`ws://` opened TLS anyway** and hung forever against a plain HTTP server — new `uMakerAi.WebSocket.Plain` (`TPlainTransport`, ws2_32 / POSIX); OpenSSL connect and handshake time out after 10 s, and transport exceptions reach `OnError`. **Android:** the WebSocket TLS transport and `TAudioPushStream` compile for Android64, and the JNI signature of `SSLSocketFactory.getDefault` was wrong (every `wss://` failed)
- New: **`TAIChatView`** — `Footer`, `Actions` (buttons inside the bubble, `OnMessageAction`, `DisableMessageActions`), `Steps` (`AddStep` / `FinishStep`, collapsible with durations), `MergeConsecutive` (consecutive messages of the same role in one bubble), attachments in `AddAssistantMessage`; **`TAIChatInput`** voice-call button (`CallVisible`, `OnCallToggle`, `CallState`, mute while in a call)
- New: **`ResetTokenCounters`** on `TAiChat` and `TAiChatConnection` (#135); **`Cost_usd`** read from `usage.cost_usd` (MakerAI broker) and accumulated like the tokens; read-only `TAiChatConnection.Chat`
- New (realtime, MakerAI driver): `ExtraSessionJson`, `OnAssistantTextDelta` / `OnTranscriptDelta` from the live engine, and `Say` with optional audio
- Fix (A2A): `TAiA2AAgentTool` registers once when set up in the Object Inspector (it registered a second function on load); `TAiA2ARemoteAgentTool` is on the palette
- Fix (agents): **`TAIAgentManager.LoadFromStream` crashed on any graph with tools** — it called `TAiToolBase(LToolClass).Create(LNode)`, a cast of the class reference to an instance that runs the constructor without allocating the object (access violation). It now creates the tool through `TComponentClass`, as `TGraphBuilder` already did
- ⚠️ Fix (RAG, Postgres on Linux): **text with non-ASCII characters failed** (*invalid byte sequence for encoding "UTF8": 0x00*) in `TAiRagGraphPostgresDriver` (every insert, update and search) and in three queries of `TAiRAGVectorPostgresDriver` — FireDAC's `AsString` types the parameter as `ftString` (ANSI) except on iOS/Android, and the conversion breaks it on Linux; the parameters now go through `AsWideString`. Windows was not affected
- Fix (RAG): **`TAiRagGraphPostgresDriver.SearchNodes` with `Depth > 0` always failed** — the neighbour expansion (`traversal`) references itself but the query started with `WITH`, not `WITH RECURSIVE`. Runtime-tested with PostgreSQL 17 + pgvector 0.8.1 on Windows and Linux
- ⚠️ Fix (RAG): **in-memory vector search with `TAIBasicIndex` returned stale or empty results** — the fast path rebuilt each hit's score from `Node.Idx`, which this index never writes (its search is thread-safe and leaves the nodes alone). A query came back with what the previous hybrid search or rerank had left in `Idx` (another query's hit and score) or with zeros and nothing. New virtual `TAIEmbeddingIndex.SearchScored` returns each hit with its score; the basic index returns its own, the others keep reading `Idx`. Regression case `rag.index.basic-score`
- ⚠️ Fix (OpenAI): **speech, image and transcription endpoints were sent the chat model** — a chat on `gpt-6.1-sol` asking for audio or an image (`cap_GenAudio` / `cap_GenImage` gap, or `cmSpeechGeneration` / `cmImageGeneration`) reached `/audio/speech` or `/images/generations` with `gpt-6.1-sol` and got 404 *Invalid URL*. As video already did with `sora`, a session model that is not one of the endpoint's falls back to `gpt-4o-mini-tts`, `gpt-image-1` or `gpt-transcribe`; an endpoint model (`gpt-image-2`, `gpt-4o-mini-tts`…) is kept. Regression case `chat.openai.media-model` (fake server); runtime-tested with `gpt-image-1-mini`, `gpt-4o-mini-tts` and `gpt-4o-mini-transcribe` from PascalAI
- ⚠️ Fix: **TTS through `TAiChatConnection` sent an empty voice** — `SyncMediaParams` copied the connection's empty `TtsParams` over the voice the model's registry entry had set on the chat (`gpt-4o-mini-tts`: `Voice=alloy`) on every `Run`, and OpenAI answered 400 *Invalid value: ''* unless the voice was set by hand. Empty TTS and transcription fields on the connection no longer overwrite the chat's. Regression case `chat.tts.registry-voice`
- Build: **packages and the regression suite compile without the IDE Library Path** (PR #132; `uMakerAi.Version.inc` included by relative path, missing search paths added, `MakerAI.dproj` synced with the `.dpk`); **Delphi sources check out with CRLF**, also in the GitHub ZIP (#133); PDF and audio marked as binary
- New demos: **`Demos/Libro`** (chapters 3–37 of the 4.0 edition of the book), **`093-A2AChatAgentTool`** and **`094`–`105`** (*Café La Ladera* series); 012, 023, 036, 051 and 052 updated
- Demos cleanup: the demo sets of 3.1, 3.4 and the previous book edition, plus six demos superseded by newer ones (010, 032-MCPServerDataSnap, 035, 062, 063, 065), moved to [`DeprecatedDemos/`](DeprecatedDemos/README.md); the demo list above is regrouped by topic and covers every project; the test consoles 014, 068 and 069 moved to `Tests/Integration/` (`IntegrationTests.groupproj`)
- Demos `012-ChatAllFunctions` and `041-GeminiVeo` moved to `TAIChatView` / `TAIChatInput` (the last ones on the deprecated `TChatList`); `041` keeps the metadata to extend a video under the MD5 of its content and no longer leaks the result; `012` no longer pops up its connection parameters (with the resolved API key) when the driver changes
- Demos on current models: `021` (`gpt-transcribe`, `gpt-4o-mini-tts`), Computer Use `066`/`083` (`claude-opus-5-5`, `gemini-3.8-flash`), `067`/`068` (`claude-haiku-5-5`, re-run live)
- Regression suite: 147 cases (`chat.tts.registry-voice`, `rag.index.basic-score`, `chat.openai.media-model`, `chat.minimax.request`; `chat.gemini.request` now covers Argon and the new image alias)

### v3.9.0 (2026-10-05)
- Changed: **the PPM registry moved to `https://registry.cimamaker.com`** — new default in `TAiFunctions` (AutoMCP, PPM search), `TAiPrompts`, `TAiSkill`, the SKILL.md client and the demos. Code that sets `registry.pascalai.org` explicitly keeps working while that host stays up
- ⚠️ Fix: **the OpenAI Responses driver never sent `max_output_tokens`** — `Max_Tokens` was silently ignored in the whole driver. `response.incomplete` now closes the stream with its finish reason, and `response.failed` / stream `error` events end the turn through the error path instead of leaving the caller waiting for its timeout
- Fix: **parallel `tools/call` on a shared `TMCPClientSSE` lost responses** (a call waited 180 s and came back empty) — two races on the request id
- New: **`TAiMCPServer.BindAddress`** (or the `MCP_BIND_ADDRESS` environment variable) binds SSE/HTTP MCP servers to one local IP; they always listened on 0.0.0.0
- Fix: **Grok** — `reasoning_effort` reaches the grok-4 models and streaming returns usage (it came back as zeros)
- Fix: `EnforceStrictSchema` crashed on JSON Schema union types (`"type": ["integer", "string"]`); Claude `code_execution` files keep their real names
- New: **palette icons for every component** — 76 were missing (A2A, bridges, realtime, Jev, Qwen, GLM, Cohere, llama.cpp, tools, embeddings…), and the `MakerAi.UI` and `MakerAi.RAG.Drivers` packages had none because only `MakerAI` linked a resource. Each package now links its own `.res`. Generator and category sheet in `Source/Resources/`
- Fix: **debug logs wrote to `C:\Temp` unconditionally** — the SChannel TLS transport (`schannel_diag.txt`), the Gemini Veo upload (`responses.txt`, which also leaked a stream) and the MakerAI realtime driver; the MCP client shutdown log did so in every Debug build. All off by default (opt-in: `MAKERAI_SCHANNEL_DIAG`, `MAKERAI_MCP_SHUTDOWN_LOG`). `MakerAiGrp.groupproj` now builds in the documented order and demo 059 lost an absolute `E:` path. Reported in #130
- Fix: **`TAiMemory.Delete`, `Link` and `Unlink` return `Boolean`** — after the namespace isolation of 3.8 an operation on a foreign id does nothing, but the `memory_delete` / `memory_link` MCP tools still answered "deleted"/"linked". They now return `False` (existing calls still compile) and the tools answer `not_found`. `IAiMemoryStorage` is unchanged
- New: **`TAiOpenAiLiveChat` — OpenAI GPT-Live** (`gpt-live-1`, DriverName `OpenAiLive`), full-duplex voice: it listens while it speaks and delegates reasoning. By default to a Responses model run by OpenAI, with web search and your `TAiFunctions` executed locally; or, assigning `DelegateChat`, to **any `TAiChatConnection`** (Claude, Ollama, an agent graph, RAG) — the driver builds the task from the transcript and speaks the answer back. Turns are rebuilt from the session timeline, so overlapping speech no longer splits answers. Tested live: conversation, interruptions, a 2-minute story, local function (≈ 2 s delegation round trip) and `DelegateChat` (≈ 4.9 s). Demo `092-GPTLiveVoice` (FMX: microphone, speaker, delegation mode, local functions, anti-echo speaker mode)
- New: **Jev decisions on local models through Ollama 0.35+** (`/v1/systemone`: `nimble`, `tev1`, Cloudflare's `clef`/`clef-flash`). `TAiJev` works as is with `Url := 'http://localhost:11434/v1/'`; the eight Jev adapters gained a `Url` property (keeping parallelism in the RAG reranker and the batch labeler), `TAiJev.Ask` accepts images for Clef (PNG/JPEG/WebP, validated by their bytes), and the TypeSafe default price is no longer charged on other servers. Tested live with `nimble` (routing, prompt guard and parallel batch labeling, at zero cost) and with `clef` 27B on images (PNG/JPEG/WebP and a control chart told apart at 0.99). `clef-flash` fails in Ollama 0.35.1 itself (ollama#18769); `clef` needs a smaller context (`num_ctx 4096`) to fit in 24 GB of VRAM
- ⚠️ Behaviour change: **`TAiOpenAiAudio` and `TAiOpenAiSpeechTool` default to `gpt-transcribe` and `gpt-4o-mini-tts`** (before `whisper-1` and `tts-1`, which OpenAI shuts down on 2027-02-26 and 2027-01-06). Forms that kept the default switch automatically. `gpt-transcribe` returns JSON only: srt/vtt/verbose_json, timestamps and logprobs downgrade and are reported in `TTranscriptionResult.Warning` — set `TranscriptionModel := tmWhisper1` to keep them until the shutdown. `gpt-4o-mini-tts` is also deprecated (no replacement on `/audio/speech` yet) but sounds better and supports `TTSInstructions`. Tested live (speech → transcription round trip). `TAIWhisper` is unchanged
- ⚠️ Fix: **an MCP server grew in memory with every `tools/list`** — the schema of each tool was cloned and the original leaked (~32 blocks per request), since a June change. `IAiMCPTool.GetInputSchema` now documents what both implementations already did: it returns a new object the caller owns. Custom `IAiMCPTool` implementations must not return a cached field
- Fix: **the MCP client leaked the `tools/list` response on every `Initialize`** (HTTP and base transports), and `TAiRealtimeFactory` never freed its driver dictionary — both reported as leaks when closing an app with MCP or realtime. Regression case `mcp.initialize.no-leak`
- New: **demo 037 (MCP RAG server) runs on PostgreSQL + pgvector** (`Driver=postgres` in its ini; values starting with `@` are read from the environment, e.g. `Password=@PGPASSWORD`). Tested at runtime; SQL Server 2025 stays the default. **Demo 092** consumes it by voice through MCP, with a read-only `TAiGuardrails` policy, and gained a quick guide of example questions
- Fix: **`TAiOpenAiAudio.Transcribe` left the raw JSON in `Text`** when the model could not honour the requested format — e.g. `gpt-transcribe` with `trfText`, or a `gpt-4o` model with `trfSrt`/`trfVtt`: the request was downgraded to `json` but the response was parsed as plain text. It is now parsed with the format actually requested. New **`TTranscriptionResult.Warning`**: one line per thing that was requested but not honoured (format downgraded, timestamps, logprobs or known speakers ignored by the model) and a deprecation notice for the old models
- Deprecated (OpenAI): **`whisper-1`, `gpt-4o-transcribe`, `gpt-4o-mini-transcribe` and `gpt-4o-transcribe-diarize`** shut down on 2027-02-26, and **`tts-1`, `tts-1-hd` and `gpt-4o-mini-tts`** on 2027-01-06. Their replacements have no srt/vtt/verbose_json, timestamps, logprobs, diarization or translation to English, so those features (and `TAiOpenAiAudio.TranslateToEnglish`) end on that date. Marked in the enums, the catalog and the docs; defaults are unchanged for now. Realtime docs and demos no longer use the retired `gpt-4o-realtime-preview`

### v3.8.0 (2026-09-29)
- ⚠️ Fix: **a forced `tool_choice` looped forever** — `required` (or a named function) was re-sent on every round, including the one that returns tool results, so the model had to call a tool again each time: measured on `claude-sonnet-5`, 231 requests until killed. `TAiChat.Tool_choice` now reads `auto` in follow-up rounds, which fixes every driver at once. **Behaviour change**: forcing applies to the first call of a turn only
- Fix: **`TAiOpenChat` dropped on a form used `gpt-5`**, not the documented default — the base constructor set `gpt-5` and the driver only assigned its own default when the model was empty. The OpenAI default is now **`gpt-6-sol`** (the successor of `gpt-5.1` at the same price point) for both the component and `TAiChatConnection`; `gpt-6-sol` and `gpt-6-luna` registered. `TAiMakerAiChat`, which inherits from it, now defaults to `mk-gpt-oss-20b` like its connection. GPT-6 rejects effort `minimal`; it is sent as `low`. Tested live
- New: **Claude Opus 5.5, Sonnet 5.5 and Fable 5.1** registered. They reject forced tool use with a 400: the driver sends `auto` instead. `tlXHigh`/`tlMax` now reach Claude as `xhigh`/`max`. `claude-opus-4-1`, retired on 2026-08-05, is now an alias of `claude-opus-5-5`. Tested live. The default stays `claude-haiku-4-5` (still the current Haiku)
- Fix: **Gemini defaults pointed to models new accounts cannot use** — `gemini-2.5-flash` (driver default) is restricted to accounts that already used the 2.5 family, and the transcription and web-search tools defaulted to `gemini-2.0-flash`, shut down on 2026-06-01. The driver, `TAiGeminiWebSearchTool` and transcription now default to **`gemini-3.8-flash`**, and `TAiGeminiSpeechTool` TTS to **`gemini-3.8-flash-tts`**. `gemini-3.8-flash`, `gemini-3.7-flash` and both 3.8 TTS models registered; `gemini-3-pro-preview` and the three `imagen-4.0-*` (shut down) became aliases of their successors. Sampling is now omitted by **version** (3.5 and later) instead of a name list, which missed 3.7/3.8, and the new effort levels map to what Gemini 3 accepts (`minimal` is rejected by 3.7/3.8). *Checked against the official docs; not runtime-tested (no API key).* Forms saved with the old model keep it
- ⚠️ Fix (security): **`TAiMemory` enforced namespaces only in searches** — `Get`, `Update`, `Delete`, `Link` and `Unlink` addressed memories by id alone, and ids are sequential, so an agent could read, change or delete another namespace's memories by guessing one, including through the `memory_delete` / `memory_link` MCP tools. `ImportFromJSON` also honoured the `namespace` of the JSON. Every id-based operation now requires the active namespace (a foreign id behaves like a missing one) and imports land in the active namespace. **Breaking for custom storages**: the id-based methods of `IAiMemoryStorage` take the namespace. Reported responsibly in #127
- New: **Skills in SKILL.md format** — one parser for the whole framework (`uMakerAi.Skills.Format`, with a read-only PPM registry client: semver version resolution, `skill-` prefix fallback, clear errors on a missing package, a non-skill package or an HTML page). **`TAiSkills`** gives any chat with function calling on-demand skills: the model sees a catalog in `use_skill` and loads the instructions it needs; folder skills ship supporting files read with `read_skill_file`, confined to the skill folder. Live 9/9 on OpenAI, Claude and Groq. Guide: `Docs/Version 3/uMakerAi-Skills.EN.md`, demo `091-Skills`
- Fix: **`TAiSkill.FromPPM` never worked** — it requested a URL that returns the website's HTML and a JSON format no package publishes. It now downloads the real SKILL.md; new `FromFolder` / `FromSkillFile`, the JSON format still works for local files, and a SKILL.md never supplies an API key
- ⚠️ Fix: **`TLLMNode` ignored its skill's driver** — the constructor's `DriverName := 'Claude'` always won, and applying the skill before the node lost the skill's model and key whenever the driver changed. New `ResolveConfig` / `ConfigureChat`: driver node → skill → Claude, the skill's model only if it belongs to that driver, skill prompt followed by the node's. `DriverName` now defaults to `''` (no change without a skill)
- New: **`TAiPrompts` local skills** — `LoadSkillFromFile`, `LoadSkillsFromFolder`, `ApplySkill` (into any `SystemPrompt`) and `SkillDescription`
- New: **Jev (TypeSafe AI)** — `TAiJev` for calibrated typed decisions (Choice / Score / Noul) and adapters for agent routing (`TAiJevRouterTool`), SmartDispatch (`TAiJevDispatchClassifier`), tool-call and input guardrails (`TAiJevGuardrailClassifier`, `TAiJevPromptGuard`), eval scoring (`TAiJevEvalScorer`), RAG reranking with injection filtering (`TAiJevRAGReranker`), bulk labeling (`TAiJevBatchLabeler`) and model routing (`TAiJevModelRouter`), with usage metering for billing. New provider-neutral hooks: `ChatTools.DispatchClassifier`, `ChatTools.PromptGuard`, `TAiGuardrails.Classifier`, `TAiEvalRunner.Scorer`, `TAiRAGVector.Reranker`. Demos 084–088
- New: **`TAiThinkingLevel` covers the full effort ladder** (`none`/`minimal`/`low`/`medium`/`high`/`xhigh`/`max`, the historical values unchanged) — `xhigh`/`max` on `gpt-6-astra` silently fell back to the default. Also **async tool calling** (`TFunctionActionItem.IsAsync`)
- New: **`gpt-image-2.5` flare/sunburst** (`iqXHigh` / `iqMax` quality, real alpha transparency), `TAiDalle.ModelName` for OpenAI-compatible services that serve a named checkpoint, and `EAiDalleHTTPError` with `StatusCode` and `RetryAfter`
- Fix: **`deepseek-flash` billed reasoning with `ModelCaps=[]`** — the thinking gate only matched `deepseek-v4*`, so the canonical name let the API default (thinking on) through
- Fix: **`TOpenSSLTransport` never sent SNI** — `SSL_set_tlsext_host_name` is a macro, not an export, so no host behind a CDN could be reached (alert 40). Now through `SSL_ctrl`
- Fix: **`TMCPClientSSE` destructor spun forever** on an empty queue after shutdown, burning a core
- Fix: `TAiVoiceMonitor` compiles outside Windows (Linux64, Android)
- Docs: `Source/Memory` added to the Library Paths list — packages compile without it, but an app using `TAiMemory` did not

- ⚠️ Behaviour change: **a connection with `DriverName` but no `Model` now gets its default model's catalog parameters** — the registry only applied per-model settings when `Model` was set, so the default model ran without them (Qwen without vision, Groq without reasoning or code interpreter and with an 8192 token limit, Gemini without audio/PDF/search; 9 of 15 drivers differed). An empty `Model` now behaves exactly like setting the driver's default model explicitly. `Model` itself stays empty, so nothing changes in forms
- New: **`TAiRealtimeConnection.DriverParams`** — driver-specific properties (`Voice`, `Instructions`, `TargetLanguage`, `ReasoningEffort`, `Keyterms`...) set through the universal connector as `Property=Value` lines, applied by RTTI on driver creation and on `Connect`. Previously only the base properties reached the driver. Unknown keys are reported through `OnError` (`driver_param`) on connect
- Fix: **asynchronous answers arrived duplicated in `OnReceiveDataEnd`** (`'Done'#13#10'Done'`) in every driver on the shared streaming parser — the end-of-stream message re-added the accumulated text. `TAiDeepSeekChat` has its own copy of the parser and got the same fix
- Fix: **Groq `executed_tools` were ignored when streaming** — `gpt-oss` sends them inside `choices[0].delta`, two chunks per tool with the same `index`; the parser only read the top-level field of the retired `groq/compound`. Files generated in the code-interpreter sandbox now arrive in asynchronous mode too
- Fix: **Groq catalog** — the retired `llama-3.1-8b-instant` (driver default) and `llama-3.3-70b-versatile` are now aliases of `openai/gpt-oss-20b` / `gpt-oss-120b`, the new default is `gpt-oss-20b`, `groq/compound` and `compound-mini` removed, `qwen/qwen3.6-27b` token limit fixed (16384) and `qwen/qwen3.8-27b` added. Demo 014 runs Groq's code interpreter on `gpt-oss-20b` (`--groq`)
- Fix: **`Invalid pointer operation` when freeing a chat right after `OnReceiveDataEnd` in asynchronous mode** — the final event fires before the HTTP client closes the request, and closing it (on the HTTP thread) frees the request stream the destructor was freeing at the same time. Intermittent, and it reached `OnError`. The destructor now waits for the request to close (bounded), through a fixed layer between the HTTP client and the drivers' virtual handlers, so every driver is covered. No wait when nothing is in flight
- Fix: **audio-to-text bridge fired `OnReceiveDataEnd` with the raw transcript** before the model's answer (Cohere, Groq, Mistral, OpenAI) when a text model received audio in conversation mode. Outside `cmTranscription` the transcript is now only input for the model
- Fix: **`Voice_Format` was silently ignored** — the OpenAI TTS catalog entry and demo 012 use that key, but the property is `TtsParams.VoiceFormat`. Accepted as an alias now
- Fix: **OpenAI and Mistral embeddings leaked the request JSON on every successful call** (the variable was reused for the response). Measured: +62 KB over 40 calls before, +176 bytes after. `TAiQwenEmbeddings` inherited it
- New: **Qwen driver** (`TAiQwenChat`, Alibaba Model Studio / DashScope, OpenAI-compatible). Always sends `enable_thinking` (the API thinks by default on hybrid models), `ThinkingLevel` → `thinking_budget`, `qwq-plus` registered async (it answers empty without streaming), open-weight models only think when streaming. Runtime-tested: sync, async, reasoning on/off, tools (sync and streaming), vision, qwq-plus. Also image generation and editing (1–3 attached images, aspect ratio preserved), video with Wan (text, image or first/last frame; 720p by default), three realtime drivers (voice conversation, live STT, simultaneous translation; new unit, no change to the realtime module) plus `TAiQwenRealtimeTTS` for streaming text-to-speech, text translation with `qwen-mt` (glossary, domain, streaming fixed for the models that resend the accumulated text), custom voices (`TAiQwenVoices`: clone, design, list, delete), TTS and transcription through the capability gap, native audio input on omni models (the API needs a data URI, the driver rewrites it), `TAiQwenEmbeddings` and `TAiQwenRAGReranker` (`qwen3-rerank`, batched above 500 passages). Twelve new offline regression cases
- Fix: **`TAiChatConnection` did not pass media sub-parameters edited in place** — `C.VideoParams.Params.Values['duration'] := '3'`, `C.TtsParams.Voice := ...` or `C.ImageParams.Params.Values['size'] := ...` after the chat existed changed only the connection's copy; they reached the driver only when the whole object was assigned or `Params` changed. Affected every driver. They are now copied on each `Run` / `AddMessageAndRun`. Found when a 3 s Wan video came back at 5 s
- New: **Computer Use on Linux** — `TAiLinuxExecutor` (X11 via xdotool + scrot) covers the 19 canonical actions with the same public interface as the Windows and macOS executors. The framework needed no change: `TAiComputerUseTool` only uses the RTL and delegates through its two events, so it cross-compiled to Linux64 untouched. Runtime-tested on Xvfb with `gpt-6-astra` and `claude-opus-4-8` against a text editor and Chrome. Demo `083-ComputerUseLinux`, plus a repeatable setup script for a headless VPS
- New: **Computer Use delegation** — OpenAI and Gemini now honour the framework contract (fill `ToolCall.Response` from `OnCallToolFunction` and the driver does not execute locally), which is what lets a headless broker forward the call to a remote client. OpenAI ignored `Response` and executed anyway; Gemini never fired the event at all. For OpenAI the delegation is atomic over the batch, since `gpt-6-astra` sends an array of actions that admits exactly one `computer_call_output`. Without a `TAiComputerUseTool` assigned the synchronous path used to emit an output with no `image_url`, which the API rejects with 400; it now ends the turn and reports through `LastError`. Demo `082-ComputerUsePassthru`
- Fix: **Claude sends coordinates as an array *and* as a string containing one, within the same turn** — `"coordinate": [299, 282]` in the first calls, `"coordinate": "[299, 400]"` later. `TryGetValue<TJSONArray>` does not match the second form, so the coordinate was lost and the action fell back to (0,0): a click in the screen corner, after which the model retried until the turn ran out. Affected `coordinate`, `start_coordinate`, `region` (zoom) and numeric fields (`"duration": "1"`), i.e. click, double/triple click, drag and zoom. Not a Linux issue — it hit Windows and macOS just the same
- Fix: **`TAiGeminiChat` executed no user functions** — `DoCallFunction` had its `inherited` commented out and answered `'Command <name> not found'` to every non-Computer-Use tool call, and it is the driver's only dispatch point. Neither `AiFunctions` nor `OnCallToolFunction` ever ran. Present since before v3.3. **Behaviour change**: those tools now execute
- Fix (security): **`TOpenSSLTransport` did not validate the server certificate** — it ran with `SSL_VERIFY_NONE`, and that is the transport the Realtime module uses on Linux and macOS. Now verifies the chain (system CA store) **and the hostname** via `SSL_set1_host`; `SSL_VERIFY_PEER` on its own checks the chain but not that the certificate was issued for the host you dialed, which is the classic half-done validation. Failures carry the `X509_V` code. Verified against badssl.com 7/7 (self-signed 18, expired 10, untrusted root 19, **hostname mismatch 62**). **Breaking**: self-signed endpoints need `InsecureSkipVerify := True`
- Fix: **`TAiShell` died on the first command on any non-English Windows** — output was decoded with `TEncoding.UTF8.GetString`, which validates and raises `EEncodingError`, while `cmd.exe` writes in the console OEM codepage (cp850 on a Spanish Windows) and even its banner carries accents. Invisible in English and on Linux. Also: stderr was dropped when the sentinel arrived in the same read (a failing command reported nothing), and a timeout left the session unusable for the life of the process (the `Restart` was written but commented out). Documented that `TimeOut` is an *inactivity* timeout, not a total one
- Fix: **`otel.scope.version` was hardcoded to `3.5`** with the framework on 3.7 — every span lied about which version produced it. Found while looking at the traces in a real Jaeger for the first time, which also confirmed that the trace **crosses the A2A boundary** (client and server spans share a trace, so `traceparent` propagation through `_meta` works)
- Fix: **demos `031-MCPServer` and `077-RagPostgresConsole` now build and run on Linux64**. `031` called the Windows API from the body of its `system_info` tool, which took the whole MCP server down outside Windows; `077` needed `FireDAC.ConsoleUI.Wait` and assumed its catalogue tables already existed. Verified end to end on Ubuntu 26.04 — which also makes **FireDAC on Linux a tested path** (PostgreSQL 18.6 + pgvector 0.8.1 through `libpq.so.5`)
- Docs: the Realtime module needs `CheckSynchronize` in console apps and services — every event is dispatched with `TThread.Queue(nil, ...)`, so without a message loop **no event ever fires**, not even `OnError`, while the WebSocket connects and the audio is sent

### v3.7.0 (2026-09-10)
- Fix: **Claude Computer Use was broken, not merely outdated** — the driver still declared `computer_20251124`, a tool type the Anthropic API now rejects for *every* model (`does not match any of the expected tags`). Updated to **`computer_toolset_20260801`**, which changed shape as well as date: it is a *toolset* entry taking **no parameters** at all (no `name`, no `display_width_px`/`display_height_px`, no `enable_zoom` — the API answers *"Extra inputs are not permitted"*) and it needs no beta header. Structurally the single `computer` tool with an `action` discriminator was exploded into **17 individually named tools** (`left_click`, `right_click`, `middle_click`, `double_click`, `triple_click`, `left_click_drag`, `left_mouse_down`, `left_mouse_up`, `mouse_move`, `cursor_position`, `key`, `hold_key`, `type`, `scroll`, `wait`, `screenshot`, `zoom`), so dispatch now goes by `tool_use.name`; Claude emits several of them per turn. Coordinates arrive as **pixels of the submitted screenshot**. Supported only on `claude-opus-4-8`, `claude-opus-5`, `claude-sonnet-5` and `claude-fable-5` — every older model lost computer use entirely. Also removed `TAiClaudeChat.TranslateClaudeComputerArgs`, dead private code that had silently diverged from the live translator
- New: **OpenAI Computer Use — `gpt-6-astra`** — the Responses API tool `{"type":"computer"}` (no parameters; it replaces `computer_use_preview`, whose dedicated model was shut down on 2026-07-23 and which astra rejects). Unlike Claude and Gemini, astra sends a **batch**: one `computer_call` carrying an `actions` array (e.g. `keypress[WIN,r]` → `type "notepad"` → `keypress[ENTER]`). The driver runs them in order and answers with a **single** `computer_call_output` holding the final screenshot, which is what the API expects per `call_id` — and as a side effect avoids the screenshot amplification the per-action providers suffer. Implemented on **both** the synchronous and the streaming paths, including history serialisation of `computer_call` / `computer_call_output`. New `TAiComputerUseTool.TranslateOpenAIToolCall` keeps the canonical action model in the tool, next to the Claude translator. Runtime-tested end-to-end (screenshot → click → type → screenshot, against a real desktop)
- Fix: **agentic loop stalled after the first Computer Use step (OpenAI)** — the synchronous continuation condition was `(last message is 'tool') and (FLastContent = '')`, written for shell/patch calls, which never emit commentary. astra *does* emit text (`phase:'commentary'`) in the same turn as the `computer_call`, so `FLastContent` was never empty and the loop stopped after one action. Recursion is now forced when a computer call was processed
- New: **`gpt-6-astra` registered** — 1.05M context (272K before surcharge), 128K output, vision + reasoning + tools. Note: astra accepts `xhigh` and `max` reasoning efforts, which `TAiThinkingLevel` (`tlDefault`/`tlLow`/`tlMedium`/`tlHigh`) cannot yet express — capped at `tlHigh`
- Update: **demo `066-ComputerUseTest`** — third provider option (`OpenAI (gpt-6-astra)`), command-line startup (`-provider=`, `-prompt=`, `-autorun`) and a `run.log` next to the executable, so the loop can be exercised without touching the GUI
- New: **GLM driver (Zhipu AI / Z.ai)** — `TAiGLMChat` (`DriverName='GLM'`, `@GLM_API_KEY`), OpenAI-compatible endpoint `https://api.z.ai/api/paas/v4/` (mainland China via the `URL` property). The API ships with thinking ON by default — the driver controls it explicitly (`cap_Reasoning` → `thinking:{enabled}`, disabled otherwise; `glm-5.3` uses forced thinking and is always sent enabled); `reasoning_effort` (`low`/`high`/`max`) sent on glm-5.2/5.3 per `ThinkingLevel`; `reasoning_content` captured in parse and streaming and re-sent in multi-turn history (required by Z.ai). Registered models: `glm-4.7` (driver default), `glm-4.7-flash` (**free**), `glm-4.7-flashx`, `glm-5.3`/`glm-5.2`/`glm-5.1`/`glm-5` (reasoning), `glm-5-turbo`, and vision `glm-5v-turbo`/`glm-4.6v` (native tool calling)/`glm-4.6v-flash` (**free**)/`glm-4.6v-flashx`/`glm-4.5v` (no tools, 16K output). Sampling clamped to the Z.ai ranges (temperature [0,1], top_p [0.01,1], max_tokens ≤131072); `tool_choice` supports only `auto`. Capabilities verified against the official docs; *not runtime-tested yet*

### v3.6.0 (2026-08-02)
- New: **Regression suite — `Tests/RegressionSuite/`** — the framework finally has an automated safety net: 17 cases covering MCP dual-era + MRTR, agent graphs, A2A 1.0 + federation, guardrails and the evals runner itself. Fully in-process (spins up its own MCP and A2A servers, plus a legacy-only MCP server to exercise the dual-era fallback), no API keys, runs in under a second. Built **on `TAiEvalRunner`**, so it doubles as the canonical usage example. `--json` writes a CI-friendly report; `--otel` traces every case as an `eval.case` span
- New: **Guardrails — `TAiGuardrails`** — policy layer that intercepts every tool call *before* execution (the single choke point in `TAiFunctions.DoCallFunction`, so it covers local functions, MCP tools and AutoMCP alike). Strict allowlist and blocklist with wildcard masks, forbidden substring patterns in tool arguments, and a programmatic `OnCheckToolCall` veto; blocked calls never execute and the LLM receives the reason as a JSON error so it can replan. `OnBlocked` for auditing, `BlockedCount` for metrics, and a `guardrail.blocked` span attribute. Assign via `TAiFunctions.Guardrails` (opt-in, zero impact when unassigned)
- New: **Evals — `TAiEvalRunner`** — lightweight evaluation framework for AI pipelines: fluent test cases (`AddCase('x').Input(...).ExpectContains(...).ExpectRegex(...).ExpectMaxLength(...)`) run against a generic target function, so the same suite can evaluate a `TAiChat`, an agent graph, an MCP tool or an A2A agent. Deterministic checks plus optional **LLM-as-judge** (`ExpectJudge('criteria')` with a `Judge` chat). Reports offer `ToText` for consoles and `ToJSON` for CI, and each case emits an `eval.case` OTel span
- New: **A2A protocol (Agent-to-Agent, Linux Foundation) — MVP** — first Delphi implementation of the A2A 1.0 spec: `TAiA2AServer` exposes any `TAIAgentManager` graph as an A2A agent (Agent Card at `/.well-known/agent-card.json`, JSON-RPC `SendMessage`/`GetTask`/`CancelTask` with 0.x method aliases; graph suspension maps to `TASK_STATE_INPUT_REQUIRED` for human-in-the-loop) and `TAiA2AClient` consumes remote A2A agents (`FetchAgentCard`, `SendText`, task lifecycle). No streaming/push yet (declared `false` per spec, `UnsupportedOperationError` on streaming calls). OTel spans `a2a.client`/`a2a.server` included. **Agent federation**: `TAiA2ARemoteAgentTool` lets any graph node delegate its input to a remote A2A agent (assign it as the node's `Tool`). Runtime-tested e2e (card + SendMessage → COMPLETED + GetTask, plus a local graph federating to a remote A2A graph)
- New: **OpenTelemetry tracing — `TAiTelemetry`** (observability phase 1) — opt-in OTLP/HTTP JSON exporter (standard collector endpoint `localhost:4318`; works with Jaeger, Grafana Tempo, Langfuse, Arize Phoenix). Spans follow the OpenTelemetry **GenAI semantic conventions**: chat turns (`chat <model>` with `gen_ai.request.model`, `gen_ai.system`, `gen_ai.usage.input/output_tokens`, sync and async), tool executions (`execute_tool <name>`), agent graphs (`agent.graph` + `agent.node <name>` nested across pool threads via explicit trace context), RAG retrieval (`rag.search` with top-K/results/hybrid flags), and MCP client/server requests — with W3C `traceparent` propagated through MCP `_meta` (spec 2026-07-28 convention) so client and server processes share one distributed trace. Zero overhead when no `TAiTelemetry` instance is enabled. Demo 031 gains an `--otel` flag. Runtime-tested end-to-end (27 spans, cross-process trace propagation, live OpenAI chat span with token usage)
- New: **MCP spec 2026-07-28 (stateless) — dual-era support** — the server implements `server/discover`, per-request `_meta` (protocol version, client identity, capabilities), `resultType` + `serverInfo` on every result, `ttlMs`/`cacheScope` cache hints on list results, and the reserved error codes `-32020` (HeaderMismatch) / `-32022` (UnsupportedProtocolVersion with `data.supported`). Modern stateless requests bypass the session gate with per-request `OnClientConnect` vetting; the legacy `initialize` handshake + `Mcp-Session-Id` gating remain fully functional
- New: **MCP client dual-era probe** — `TMCPClientStdIo` / `TMCPClientHttp` try `server/discover` and fall back to the legacy handshake automatically (`NegotiatedProtocol` exposes the result); modern requests carry `_meta` plus the `MCP-Protocol-Version` / `Mcp-Method` / `Mcp-Name` headers; the StdIO reader now rescues JSON-RPC embedded in noisy stdout lines
- New: **MRTR (Multi Round-Trip Requests)** — tools can request user input via elicitation: the server plumbs `params.inputResponses` / `params.requestState` into `TAiAuthContext`; the client's new `OnInputRequired` event drives the retry loop (max 3 rounds, opaque `requestState` echo; assigning the handler declares the `elicitation` capability). New demo tool `confirm_demo` in `031-MCPServer`
- Update: MCP spec alignment — deterministic `tools/list` / `resources/list` ordering (client caching + LLM prompt-cache friendly); unknown tool/resource now returns `-32602` Invalid Params; the 031 demo sends banners to stderr in stdio mode (stdout is protocol-only)
- Note: the legacy **HTTP+SSE transport is formally Deprecated** by MCP spec 2026-07-28 (earliest removal from the spec: **July 2027**, 12-month minimum window); MakerAI keeps it as frozen legacy — prefer HTTP or StdIO for new work

### v3.5.0 (2026-08-01)
- New: **Typed ModelConfig channel** — `ModelCaps`/`SessionCaps`/`Tool_Active`/`ThinkingLevel` moved out of Params/RTTI into a typed surface with per-field user pins (`UserFields`) and transparent compatibility migration
- New: **MSSQL driver for RAG Vector** (FireDAC SQL Server)
- New: **Agents hardening** — strict JSON graph validation, public RTTI mapper `TAiToolParams`, `[TSecret]` attribute, `out_failure` in conditional mode, `Compile` no longer clears the Blackboard
- New: **ChatTools single surface** with `OnChange` propagation; `ToolCall.ResMsg` available in streaming; media delivered at `OnReceiveDataEnd`
- Fix: **`LastError` now populated on every error path** — `DoError` assigns `FLastError`, so synchronous callers can diagnose HTTP 4xx/5xx (previously empty string with no exception)
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
