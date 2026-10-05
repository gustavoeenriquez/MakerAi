# MakerAI Suite v3.8 — The AI Ecosystem for Delphi

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

## 🚀 What's New in v3.9

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
│  Switch provider at runtime via DriverName property             │
└──────────────────────────────┬───────────────────────────────────┘
                               │
┌──────────────────────────────▼───────────────────────────────────┐
│  Native Provider Drivers  (direct API access, full fidelity)     │
│  OpenAI · Claude · Gemini · Grok · Mistral · DeepSeek · Kimi    │
│  GLM · Qwen · Groq · Cohere · Ollama · LM Studio · GenericLLM   │
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
| `TAiOpenChat` | OpenAI | **gpt-6-sol** (default), gpt-6-luna, gpt-6-astra, gpt-5.6-sol/-terra/-luna, gpt-image-2.5 |
| `TAiClaudeChat` | Anthropic | claude-opus-5-5, claude-sonnet-5-5, claude-fable-5-1, **claude-haiku-4-5** (default) |
| `TAiGeminiChat` | Google | **gemini-3.8-flash** (default), gemini-3.7-flash, gemini-3.1-pro, gemini-3.8-flash-tts, Nano Banana |
| `TAiGrokChat` | xAI | grok-4.3, grok-4.5, grok-build, grok-imagine (image/video) |
| `TAiMistralChat` | Mistral AI | mistral-large/medium/small, magistral, devstral, voxtral (STT/TTS) |
| `TAiDeepSeekChat` | DeepSeek | deepseek-flash, deepseek-v4-pro |
| `TAiKimiChat` | Moonshot | kimi-k3, kimi-k2.7-code, kimi-k2.6 |
| `TAiGLMChat` | GLM (Zhipu / Z.ai) | glm-4.7, glm-5.3, glm-5v-turbo, free tiers: glm-4.7-flash / glm-4.6v-flash |
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
> `gpt-6-astra`; Claude `claude-opus-4-8` / `-opus-5` / `-sonnet-5` / `-fable-5`.
> Gemini is marked ⚠️ because the registry still points at the
> `gemini-2.5-computer-use-preview` model (3.7/3.8 Flash list computer use as a preview
> tool, not yet wired or tested). The Claude 5.5 / Fable 5.1 models use the same
> `computer_toolset` but have not been runtime-tested with it yet.

---

## 🎨 Component Palette

Every one of the 108 components has its own palette icon. The tile color identifies the
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
| `IAiPdfTool` | Extract text from PDFs | Mistral OCR, Ollama OCR |
| `IAiVisionTool` | Describe / analyze images | Any vision model |
| `IAiSpeechTool` | Text-to-speech / speech-to-text | Whisper, Gemini Speech, OpenAI TTS |
| `IAiWebSearchTool` | Live web search | Gemini Web Search |
| `IAiImageTool` | Generate images | gpt-image-2.5, Gemini (Nano Banana), Grok Imagine, Qwen Image |
| `IAiVideoTool` | Generate video | Sora, Gemini Veo, Grok Imagine, Qwen Wan |
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
- Demos: `072-A2AFederation`, `074-A2AOrchestration`, `079-A2ASutAgent`, `080-A2APushNotifications`

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
- **`TAiEvalRunner`** — fluent test cases (`ExpectContains`, `ExpectRegex`, `ExpectEquals`, `ExpectJudge` with an LLM judge, `ExpectScore` with a calibrated scorer) against any target, with text and JSON reports. The framework's own regression suite (117 cases, no API keys) is built on it
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
- **Gap = SessionCaps − ModelCaps** — any missing capability activates an automatic ChatTool bridge; for example, a text-only model with `cap_GenImage` in `SessionCaps` automatically routes image generation requests through a DALL-E or Gemini bridge

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
| `072-A2AFederation` | Agent federation over the A2A 1.0 protocol: expose a graph as an A2A agent, consume it, and delegate a local node to a remote agent (no LLM required; `--otel` for tracing) |
| `077-RagPostgresConsole` | Headless vector RAG on PostgreSQL + pgvector with local Ollama embeddings — no API key. Runs on Windows and Linux64 |
| `082-ComputerUsePassthru` | Who executes a `computer_call`: delegated to a remote client, missing tool, or local. Self-verifying with an exit code; never touches the screen |
| `083-ComputerUseLinux` | Computer Use on Linux/X11 over Xvfb — the headless counterpart of `066` |
| `084-JevRouter` … `088-JevModelRouter` | Jev (TypeSafe AI): agent routing, SmartDispatch and guardrails, evals and RAG reranking, bulk labeling, model routing. Need `TYPESAFE_API_KEY` |
| `089-QwenShowcase` | Everything Qwen (Alibaba Model Studio) with one key: chat, vision, image editing, voice, translation, embeddings + rerank, realtime |
| `091-Skills` | SKILL.md skills three ways: on demand in a chat (`TAiSkills`), as the base of an agent node (`TAiSkill`), copied into the prompt (`TAiPrompts.ApplySkill`) |

---

## 🔄 Changelog

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
