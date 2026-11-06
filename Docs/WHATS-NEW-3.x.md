# What's new in MakerAI 3.4 to 3.9

Feature notes of each 3.x release, moved out of the main [README](../README.md) for v4.0.
The condensed per-version list is in the README's *Changelog* section.

## What came in v3.9

### OpenAI GPT-Live — full-duplex voice with delegation
`TAiOpenAiLiveChat` (`gpt-live-1`, DriverName `OpenAiLive`) listens while it speaks: you can interrupt it, and it hands the heavy thinking to another model — a Responses model run by OpenAI (web search, your `TAiFunctions` and MCP tools executed locally) or **any `TAiChatConnection`** through `DelegateChat` (Claude, Ollama, an agent graph). Conversation turns are rebuilt from the session timeline, so overlapping speech no longer splits answers. Demo `092-GPTLiveVoice`: microphone, speaker, local functions, RAG over MCP with a read-only guardrail, anti-echo speaker mode and a quick guide. Tested live.

### Jev decisions on local models (Ollama)
Ollama 0.35+ serves the Jev contract at `/v1/systemone` with local decision models — `nimble`, `tev1` and Cloudflare's `clef`/`clef-flash`. `TAiJev` only needs `Url := 'http://localhost:11434/v1/'`; the eight Jev adapters gained `Url`, `TAiJev.Ask` takes images for Clef (PNG/JPEG/WebP), and the TypeSafe price is no longer charged on other servers. Tested with `nimble` (routing, prompt guard, batch labeling) and `clef` 27B on images.

### Memory leaks fixed
An MCP server leaked every tool schema on each `tools/list` (it grew forever in production), the MCP client leaked the `tools/list` response on each `Initialize`, and `TAiRealtimeFactory` never freed its dictionary.

### ⚠️ Behaviour changes
- **OpenAI audio defaults:** `TAiOpenAiAudio` / `TAiOpenAiSpeechTool` use `gpt-transcribe` and `gpt-4o-mini-tts` (OpenAI shuts down `whisper-1` on 2027-02-26 and `tts-1` on 2027-01-06). `gpt-transcribe` returns JSON only — srt/vtt/timestamps are reported in `TTranscriptionResult.Warning`; set `tmWhisper1` to keep them until the shutdown.
- **Custom MCP tools:** `IAiMCPTool.GetInputSchema` returns a new object owned by the caller (what the built-in tools already did). A custom tool that returned a cached field must return a copy.
- **OpenAI Responses driver:** `Max_Tokens` is now sent as `max_output_tokens` (it was silently ignored), so a low limit can cut answers that used to be complete.

## What came in v3.8

Released 2026-09-29. Six items change existing behaviour; they are marked ⚠️ below — two of them
(TLS certificate checks on POSIX and the `IAiMemoryStorage` signature) can require code changes.

### Jev — calibrated decisions before spending an LLM

`TAiJev` (`Source/Tools/uMakerAi.Jev.pas`) wraps **Jev**, TypeSafe AI's "System One" model.
Jev does not generate text: it answers typed questions — *Choice*, *Score*, *Noul* (yes/no) —
with calibrated probabilities and a confidence value your code can threshold. It is meant for
the fast, cheap decisions that today cost a full LLM round-trip: routing a query to the right
specialised agent, deciding whether that agent needs its RAG variant, classifying, gating.
Several questions travel in one request (~700 input tokens at US$0.042 per million). Questions
are validated locally, 429/529 are retried with backoff, and the model is pinned to
`jev-1.13.0` so thresholds stay valid. Demo: `084-JevRouter`.

For agent graphs, `TAiJevRouterTool` (`Source/Agents/uMakerAi.Agents.Tools.JevRouter.pas`) is a
node tool that writes the chosen route to `Blackboard['next_route']`, so an existing
`lmConditional` link follows it — no engine change. Low confidence or a failing API falls back
to `NextNo` instead of breaking the graph, and yes/no flags asked in the same call land in the
blackboard for `lmExpression` or an `OnRoute` handler. Twenty-one new regression cases run all
of it offline against a fake transport.

Two more hand-offs make Jev a drop-in for decisions the framework already takes:

- **SmartDispatch without the classification LLM call.** `ChatTools.DispatchClassifier` (new,
  provider-neutral) receives the tags whose tools are assigned; `TAiJevDispatchClassifier` answers
  with Jev. Tool requests skip one LLM round-trip, and CHAT replies are now generated **with the
  conversation history** (the LLM pass answered in an isolated two-message context). Unsure or
  failing → the usual LLM pass. 15/15 on Spanish and English requests.
- **Semantic guardrails.** `TAiGuardrails.Classifier` (new) judges the tool calls the allow/block
  lists let through; `TAiJevGuardrailClassifier` blocks when P(risk) ≥ 0.5. Lists still catch the
  enumerable (`rm -rf`) for free; Jev catches what no list anticipates — an e-mail carrying a
  password to an outside address, a transfer to an unknown account. Safe calls scored ≤ 0.17 and
  harmful ones ≥ 0.88 across 13 calibration cases. Fails closed by default. Optional **permission
  categories** (`read`, `write`, `financial`, `system`, …) are judged in the same call: block whole
  categories, audit each call via `OnCategorized`, and describe domain-named tools with
  `ToolDescriptions` (20/20 on 20 calibration calls).

- **Input guardrail.** `ChatTools.PromptGuard` (new) checks the user's message *before* it reaches
  the LLM, right after the existing regex sanitizer. `TAiJevPromptGuard` asks in one call about
  prompt injection, credentials in the message, harmful requests and — given a `Scope` — off-topic
  questions. On nine test messages the regex caught 1 of 6 problematic ones; Jev caught all 6 and
  let the greeting and the legitimate questions through. A blocked message never touches the network.

Demo: `085-JevDispatchGuard`.

And two more for quality and retrieval:

- **Calibrated eval judge.** `TAiEvalRunner.Scorer` (new) answers `ExpectScore('criterion', 0.7)`
  with the probability that the output meets the criterion — faster and cheaper than an LLM
  judge, and the bar is set in code. `TAiJevEvalScorer`: passing answers ≥ 0.97, failing ≤ 0.02
  on 10 calibration pairs.
- **Semantic reranking for RAG.** `TAiRAGVector.Reranker` (new) replaces the cosine second stage
  of VQL `RERANK`: `TAiJevRAGReranker` scores each passage for usable evidence and **drops
  passages that try to instruct the model** (prompt injection). No embeddings are recomputed;
  if the reranker fails, the search falls back to cosine. Demo: `086-JevEvalsRag`.
- **Bulk labeling.** `TAiJevBatchLabeler` runs the same questions over many rows in parallel and
  returns labels, confidence, top-3 suggestions, total cost and the rows worth a human look; a
  failing row never stops the batch. Validated by reproducing the prototype: 126 accounting
  entries against 225 chart-of-accounts codes in 4.2 s for US$0.05 — 92.9% overall and **100% on
  the 61% it would book automatically** (confidence ≥ 0.8). Demo: `087-JevBatchLabeling`.
- **Model routing.** `TAiJevModelRouter` sends each request to the cheapest model that can handle
  it. Jev describes the request (task, difficulty, sensitivity); readable rules in code pick the
  tier. Switching provider on a `TAiChatConnection` used to drop the conversation — the router
  migrates the text history, so a chat can start on Groq and escalate to Claude and still
  remember the first turn. Live, with Groq → DeepSeek → Claude Sonnet → Opus: 7 of 8 answers
  judged useful, the trivial ones in under a second. Demo: `088-JevModelRouter`.
- **Metering for usage-based billing.** `TAiJev` and all eight adapters expose `Usage` (requests,
  input/output tokens, cost in USD) and an `OnUsage` event fired once per operation — one guard
  check, one rerank, one batch — on the thread that ran it, so a server can charge the right
  customer. The parallel reranker reports a single total after joining its calls. Previously only
  the batch labeler exposed tokens; the other adapters received them and dropped them.

**Also fixed:** `lmExpression` parsed numbers with the regional settings only, so on a Windows
using a decimal comma `'10.25' > 9.5` was compared as text and returned `False`. It now falls
back to a decimal point.

### Computer Use on Linux

`TAiLinuxExecutor` (`Source/Tools/uMakerAi.Tools.ComputerUse.Linux.pas`) drives X11 through
**xdotool** and captures with **scrot**, covering the 19 canonical actions with the same
public interface as the Windows and macOS executors. The framework itself needed no change:
`TAiComputerUseTool` only ever used the RTL and delegates everything to
`OnExecuteAction` / `OnRequestScreenshot`, so it cross-compiled to Linux64 untouched — what
was missing was only the executor. Runtime-tested on Xvfb with `gpt-6-astra` and
`claude-opus-4-8`, driving both a text editor and Chrome. Demo: `083-ComputerUseLinux`.

### Computer Use delegation — handing the call to a remote client

A headless process (a broker on a VPS) can now claim a `computer_call` and forward it to
whoever actually owns a screen. The contract already existed in the base class and in Claude
— fill `ToolCall.Response` from `OnCallToolFunction` and the driver does not execute
locally — but **OpenAI ignored it and Gemini never even fired the event**. Both now honour
it. For OpenAI the delegation is *atomic over the batch*: `gpt-6-astra` sends an array of
actions that admits a single `computer_call_output`, so splitting it between a local and a
remote executor would leave the final screenshot ownerless. Demo: `082-ComputerUsePassthru`.

### ⚠️ TLS certificates are now verified on POSIX

`TOpenSSLTransport` used to run with `SSL_VERIFY_NONE` — it accepted **any** certificate,
from anyone, and that is the transport the Realtime module uses on Linux and macOS. It now
verifies the chain against the system CA store **and the hostname** (`SSL_set1_host`;
`SSL_VERIFY_PEER` alone validates the chain but not that the certificate was issued for the
host you dialed). Verified against badssl.com, 7/7, including the `wrong.host` case.

> **Breaking**: pointing Realtime at an endpoint with a self-signed certificate (a local LM
> Studio, an internal proxy) now fails until you set `InsecureSkipVerify := True`.

### ⚠️ Gemini executed no user functions at all

`TAiGeminiChat.DoCallFunction` had its `inherited` commented out and answered
`'Command <name> not found'` to every non-Computer-Use tool call — and it is the driver's
only dispatch point. Neither `AiFunctions` nor `OnCallToolFunction` ever ran. Restored.

> **Behaviour change**: tools that silently returned "not found" with Gemini now execute.

### ⚠️ TAiShell crashed on non-English Windows

Shell output was decoded with `TEncoding.UTF8.GetString`, which *validates* its input and
raises `EEncodingError`. `cmd.exe` writes in the console **OEM** codepage (cp850 on a Spanish
Windows) and even its own banner carries accents, so the component died on the **first
command**. Invisible in English (pure ASCII) and on Linux (bash does emit UTF-8). Decoding
now falls back to the OEM codepage. Two more, found while exercising it on Linux: stderr was
dropped whenever the sentinel arrived in the same read, and a timeout left the session
permanently unusable (the `Restart` was written but commented out).

### ⚠️ TAiMemory: namespaces are now enforced on id-based operations

`TAiMemory` isolated namespaces in its searches, but `Get`, `Update`, `Delete`, `Link` and
`Unlink` addressed memories by id alone — and ids are sequential, so an agent could read,
change or delete another namespace's memories by guessing one, including through the
`memory_delete` / `memory_link` MCP tools. `ImportFromJSON` also honoured the `namespace` field
of the JSON and could write into another namespace. Every id-based operation now requires the
active namespace (a foreign id behaves exactly like a missing one), and imports always land in
the active namespace. Reported responsibly in #127.

> **Breaking for custom storages**: the id-based methods of `IAiMemoryStorage` now take the
> namespace. The bundled SQLite storage is updated; a custom implementation must add the
> parameter.

### ⚠️ A forced tool_choice applies to the first call only

With `Tool_choice := 'required'` (or a named function) the driver re-sent the forcing on
every round, including the one that returns tool results, so the model had to call a tool
again each time and the agentic loop never ended — 231 requests on `claude-sonnet-5` before
it was killed. `TAiChat.Tool_choice` now reads `auto` in those rounds, which fixes every driver
at once. The first call of a turn is still forced.

### ⚠️ TLLMNode.DriverName now defaults to empty

The constructor used to set `DriverName := 'Claude'`, which always overrode the driver of an
assigned `TAiSkill`. It is now empty and resolves node → skill → `'Claude'`, so a node without
a skill behaves exactly as before and forms saved with the old default keep `'Claude'`
explicitly. See the skills section below for the rest of the precedence fix.

### Qwen — native Alibaba Model Studio driver

`TAiQwenChat` (`Source/Chat/uMakerAi.Chat.Qwen.pas`, driver name `Qwen`) talks to DashScope's
OpenAI-compatible endpoint (international region by default; key in `DASHSCOPE_API_KEY`).
Hybrid Qwen models think by default on the API side, so the driver always sends
`enable_thinking`: off unless `cap_Reasoning` is in `ModelCaps`, with `ThinkingLevel` mapped to
`thinking_budget`. Thinking-only models (`qwq-plus`, `*-thinking-*`) and open-weight models
(which only think when streaming) are handled for you. Registered models are the ones tested
live: `qwen3.8-flash` (default), `qwen3.8-max`, `qwen3.7-plus`, `qwen3-vl-flash`,
`qwen3.8-omni-flash` with vision; `qwen3-max`, `qwen-plus/flash/turbo`, `qwen3-coder-plus/flash`;
and `qwq-plus`, which only answers when streaming and is registered as asynchronous.

Beyond chat, the same key covers the rest of Model Studio, all tested live from Delphi:

- **Image generation and editing** (`cap_GenImage`): `qwen-image-3.0`, `qwen-image-2.0`,
  `qwen-image-max`, `z-image-turbo`, `wan2.7-image`. Attach one to three images to the prompt and
  the same call edits them (`qwen-image-edit-plus` by default), keeping the original aspect ratio
  unless you set a size. The driver downloads the result into `MediaFiles`.
- **Video** (`cap_GenVideo`, Wan): text to video, image to video (attach one image) or first
  and last frame (attach two). 720p by default instead of the API's 1080p; `VideoParams.Params`
  passes through (duration, audio, seed...). Wan 2.5+ videos come with an audio track.
- **Text to speech** (`cap_GenAudio`): `qwen3-tts-flash`, voice and language from `TtsParams`.
- **Transcription**: `qwen3-asr-flash` in `cmTranscription`, where the prompt is passed as
  context so proper names come out right. A text model with `cap_Audio` in `SessionCaps` uses it
  to transcribe before answering, and `qwen3.8-omni-flash` understands audio natively.
- **Realtime** (`TAiRealtimeConnection`, new unit `uMakerAi.Realtime.Qwen`): `DriverName := 'Qwen'`
  for voice conversation, `'QwenSTT'` for live transcription and `'QwenTranslate'` for simultaneous
  translation with a translated voice. Same events as the Grok and OpenAI drivers; nothing in the
  realtime module changed. Driver-specific settings (voice, instructions, target language) go in
  the connector's new `DriverParams`, which works for every realtime driver. `TAiQwenRealtimeTTS` goes the other way — text in, audio out while it
  is generated — so an LLM answer can be spoken as it streams (first audio ~0.5 s after the first
  text; 2.5 s from question to voice with qwen3.8-flash).
- **Text translation** with `qwen-mt-plus/flash/turbo/lite`: set `TranslateTo` (and optionally
  `TranslateFrom`, `TranslateDomain`, a `TranslateTerms` glossary) and each message is translated.
  The driver sends only the latest message, as the API requires, and fixes plus/turbo streaming,
  which repeats the accumulated text in every chunk.
Demo `089-QwenShowcase` walks through all of it with one key.

- **Custom voices**: `TAiQwenVoices` clones a voice from a sample or designs one from a text
  description, and lists or deletes them. Put the returned id in `TtsParams.Voice`; the driver
  switches to the TTS model that voice requires. Clone only voices you have consent to use.
- **Embeddings**: `TAiQwenEmbeddings` (driver `Qwen`), `text-embedding-v4` by default.
- **Rerank**: `TAiQwenRAGReranker` plugs `qwen3-rerank` into `TAiRAGVector.Reranker`, up to 500
  passages per call, with an optional task instruction.

### Skills — SKILL.md, loaded on demand

MakerAI now speaks **SKILL.md**, the format of Claude's Agent Skills and of the PPM registry's
`skill` packages: YAML frontmatter (name, when to use it) plus Markdown instructions. One parser
(`uMakerAi.Skills.Format`) serves the whole framework, from a string, a file, a folder or the
registry.

- **`TAiSkills`** (new, `Source/Tools/uMakerAi.Tools.Skills.pas`) gives any chat with function
  calling on-demand skills. The model sees only a one-line-per-skill catalog in the description
  of `use_skill` and loads the full instructions when it needs them, so dozens of skills cost no
  tokens until used. Folder-based skills can ship supporting files, read with `read_skill_file`,
  which is confined to the skill folder (no `..`, absolute paths, links or binaries, never
  executes anything). Veto and audit events; guardrails apply as to any tool. Live, 9/9 on
  OpenAI, Claude and Groq: the right skill for each request, none for an unrelated one, and
  `use_skill` → `read_skill_file` chained when the instructions asked for it.
- **`TAiSkill.FromPPM` works.** It requested a URL that returns the website's HTML and a JSON
  format no package publishes; it now downloads the real SKILL.md (`'code-review'` also finds
  `skill-code-review`). New `FromFolder` / `FromSkillFile`; the JSON format still works for local
  files. A SKILL.md never supplies an API key.
- **`TLLMNode` respects its skill.** New `ResolveConfig`: driver from the node, else the skill,
  else Claude; the skill's model only if it belongs to that driver; the skill's system prompt
  followed by the node's. Previously the skill's driver was always overridden and switching
  driver lost the skill's model and key.
- **`TAiPrompts`** loads local skills (`LoadSkillFromFile`, `LoadSkillsFromFolder`) and
  `ApplySkill` copies one into any `SystemPrompt`. Registry versions are now picked by semver.

Guides: `Docs/Version 3/uMakerAi-Skills.EN.md` (Spanish: `uMakerAi-Skills.md`). Demo `091-Skills`.

### Also

- **Every driver on the shared streaming parser returned the answer twice.** In asynchronous
  mode `OnReceiveDataEnd` received `'Done'#13#10'Done'`: the end of the stream re-added the
  accumulated text. Affected all drivers built on the common parser (verified on Groq and
  DeepSeek; Kimi, Grok and Mistral now return it once too).
- **Files generated by Groq's code interpreter were lost when streaming.** `gpt-oss` sends
  `executed_tools` inside each delta, in two chunks per tool; the parser only looked at the
  top level, the format of the retired `groq/compound`. They are now merged and delivered as
  in the synchronous path.
- **Groq catalog brought up to date with the API.** `llama-3.1-8b-instant` and
  `llama-3.3-70b-versatile` no longer exist and were the driver's default, so a Groq connection
  without an explicit model came back empty. The default is now `openai/gpt-oss-20b`, and both
  old names are aliases (to `gpt-oss-20b` and `gpt-oss-120b`), so existing code keeps working.
  `groq/compound` was removed without a replacement. `qwen/qwen3.6-27b` was registered with a
  token limit the API rejects; fixed, and `qwen/qwen3.8-27b` added. Demo 014 now runs Groq's
  code interpreter on `gpt-oss-20b` (`--groq`).
- **Claude sometimes sends coordinates as a JSON array and sometimes as a string** containing
  one — *within the same turn*. `TryGetValue<TJSONArray>` missed the second form, the
  coordinate was lost and the action landed on (0,0): a click in the screen corner, after
  which the model retried until it ran out of turns. Affected `coordinate`,
  `start_coordinate` and `region`, i.e. click, double/triple click, drag and zoom.
- **OpenTelemetry validated against a real collector** for the first time (Jaeger): the trace
  crosses the A2A boundary, so `traceparent` propagation through `_meta` works. Fixed
  `otel.scope.version`, hardcoded to `3.5` while the framework was on 3.7.
- **Demos 031 and 077 now build and run on Linux64** — which also makes FireDAC on Linux a
  tested path (PostgreSQL 18 + pgvector 0.8.1 through `libpq.so.5`).
- Documented that the Realtime module needs `CheckSynchronize` in console apps and services:
  every event is dispatched with `TThread.Queue`, so without a message loop **no event ever
  fires** — not even `OnError`.

---

## What's New in v3.7

### Computer Use, Refreshed on Both Live Providers

Both vendor APIs changed under our feet, and one of them had gone silently dead. **Claude was broken**: the driver still declared `computer_20251124`, a tool type the Anthropic API now rejects for *every* model. It is now `computer_toolset_20260801`, which takes **no parameters** and explodes the old single `computer` tool into **17 individually named tools**. **OpenAI joins natively** with `gpt-6-astra` and the parameterless `computer` tool, which sends a whole **batch of actions per turn** — the driver runs them in order and answers with one final screenshot.

Both APIs converged on the same design: the tool declares no screen dimensions (the model infers them from the screenshot) and coordinates come back as pixels of the image you sent.

### GLM (Zhipu AI / Z.ai) — New Provider

`TAiGLMChat` brings a 14th provider: OpenAI-compatible endpoint, explicit thinking control (the API ships it ON by default, the driver decides), `reasoning_content` captured and re-sent across turns, and free tiers (`glm-4.7-flash`, `glm-4.6v-flash`).

### Correctness Pass

RAG on pgvector (`OFFSET` was trimming instead of paginating; the vector did not survive the round trip), token and cache accounting across OpenAI, Claude and MakerAi streams, a race where the model travelled through the global registry, tool-calling continuation off the main thread, and reasoning no longer leaking into the user-visible answer.

---

## What's New in v3.6

### MCP Specification 2026-07-28 — Stateless, Dual-Era

The Model Context Protocol dropped sessions and the `initialize` handshake. MakerAI implements the new **stateless** revision on both sides *and* keeps talking to legacy peers: clients probe with `server/discover` and fall back automatically; the server serves modern per-request `_meta` requests statelessly while the legacy handshake and session gating keep working. Includes the **MRTR** pattern, so a tool can pause and ask the user for confirmation (`OnInputRequired` on the client, `TAiAuthContext.InputResponses` on the server).

### Observability — OpenTelemetry Tracing

**`TAiTelemetry`** exports OTLP traces to any standard collector (Jaeger, Grafana Tempo, Langfuse, Arize Phoenix) following the **GenAI semantic conventions**. Spans cover chat turns with token usage, tool executions, agent graphs and nodes, RAG retrieval and MCP requests — with W3C `traceparent` propagated through MCP `_meta`, so a client and a server in different processes share one distributed trace. Opt-in, zero overhead when disabled.

### A2A — Agent-to-Agent Protocol (first Delphi implementation)

If MCP is the agent-to-*tool* layer, **A2A** (Linux Foundation) is the agent-to-*agent* layer. `TAiA2AServer` publishes any agent graph as a standard A2A agent (Agent Card + JSON-RPC), `TAiA2AClient` consumes remote agents, and `TAiA2ARemoteAgentTool` **federates**: a node in your graph can delegate its work to a remote agent — including one written in another language or framework. Demo: `072-A2AFederation`.

### Guardrails & Evals

**`TAiGuardrails`** intercepts every tool call before it executes (allowlists, blocklists, forbidden argument patterns, programmatic veto) — blocked calls never run and the LLM gets the reason so it can replan. **`TAiEvalRunner`** brings systematic evaluation: fluent test cases against any target, deterministic checks plus optional LLM-as-judge, with `ToJSON` reports for CI.

### First Automated Regression Suite

`Tests/RegressionSuite/` — 17 in-process cases covering MCP, agents, A2A, guardrails and evals. No API keys, under a second, exit code for CI. Built on `TAiEvalRunner` itself.

---

## What's New in v3.5

### Typed ModelConfig Channel

Capability configuration now lives in a single typed surface: `ModelConfig.ModelCaps` / `SessionCaps` / `Tool_Active` / `ThinkingLevel` moved out of the string-based Params/RTTI channel, with per-field user pins and transparent compatibility migration — existing code keeps working unchanged.

### Full-Duplex Voice Suite

- **`TAiGrokRealtimeChat`** — xAI Grok Voice speech-to-speech (function calling, session resumption with replay, binary audio transport, ephemeral tokens)
- **`TAiOpenAiRealtimeTranslate`** — continuous streaming speech translation (one WebSocket per direction; demo 071-VoiceBridgeTranslate)
- **`TAiRealtimeVoiceBase`** — shared full-duplex base; voice events flow through the universal `TAiRealtimeConnection`
- **gpt-transcribe / gpt-live-transcribe** — OpenAI's Whisper successors, fully integrated

### August 2026 Provider Refresh — All 9 Cloud Providers, Runtime-Tested

Claude 5 family (adaptive thinking, FastMode, compaction, server-side fallbacks) · Gemini 3.5/3.6 + Nano Banana GA · Mistral Voxtral TTS + OCR 4 · Kimi K3 · DeepSeek V4 (explicit thinking control) · Cohere Command A+ · Groq qwen3.6 · xAI grok-4.3/4.5/build — with retired-model cleanup and compatibility aliases throughout.

### Grok Native Video & Image Generation

`TAiGrokChat` now generates video with grok-imagine (async job + polling + mp4 as `TAiMediaFile`, new `VideoDurationSeconds` property) and images with `grok-imagine-image` — activated by `cmVideoGeneration`/`cmImageGeneration` or the `[cap_GenVideo]`/`[cap_GenImage]` gaps.

---

## What's New in v3.4

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
