# CLAUDE.md — Source/Realtime

This file provides guidance to Claude Code when working with the Realtime module of MakerAI.

## Overview

The `Source/Realtime/` module provides real-time audio streaming via WebSocket. It is a **parallel hierarchy** to `TAiChat` — it does not inherit from it. The design mirrors the Chat module: a base class, provider-specific drivers, and a universal connector.

Two types of drivers exist:
- **STT-only** (OpenAI, Gemini): transcribe user audio → fire `OnTranscriptDelta` / `OnTranscriptCompleted`
- **Full voice conversation** (MakerAI, Grok, Qwen, OpenAI GPT-Live): STT + LLM + TTS in one WebSocket — inherit from `TAiRealtimeVoiceBase`, which adds `OnAssistantText`, `OnAssistantTextDelta`, `OnAudioChunk`, `OnAudioDone`

Demos:
- Console: `Demos/Console/Demos09-Realtime/01-RealtimeSTT/`
- FMX: `Demos/FMX/Demos09-Realtime/01-RealtimeSTT/`

## Module Structure

### Units

| File | Class | Role |
|------|-------|------|
| `uMakerAi.Realtime.pas` | `TAiRealtimeBase`, `TAiRealtimeVoiceBase`, `TAiRealtimeFactory` | Abstract bases + factory |
| `uMakerAi.Realtime.AiConnection.pas` | `TAiRealtimeConnection` | Universal connector (same pattern as `TAiChatConnection`) |
| `uMakerAi.Realtime.OpenAI.pas` | `TAiOpenAiRealtimeSTT`, `TAiOpenAiRealtimeTranslate` | OpenAI drivers — **complete** (STT + streaming translation) |
| `uMakerAi.Realtime.OpenAI.Live.pas` | `TAiOpenAiLiveChat` | OpenAI GPT-Live — full-duplex voice with task delegation (Responses or your own chat) — **runtime-tested** (2026-10-05) |
| `uMakerAi.Realtime.Gemini.pas` | `TAiGeminiRealtimeSTT` | Gemini driver — **stub, pending** |
| `uMakerAi.Realtime.MakerAi.pas` | `TAiMakerAiRealtimeChat` | MakerAI driver — **complete** (STT+LLM+TTS) |
| `uMakerAi.Realtime.Grok.pas` | `TAiGrokRealtimeChat` | xAI Grok Voice driver — speech-to-speech, OpenAI Realtime-compatible protocol — **implemented, pending runtime test** |
| `uMakerAi.Realtime.Qwen.pas` | `TAiQwenRealtimeChat`, `TAiQwenRealtimeSTT`, `TAiQwenRealtimeTranslate` | Alibaba Qwen (DashScope) — voice conversation, live STT and simultaneous translation — **runtime-tested** (2026-09-28) |
| `uMakerAi.Realtime.QwenTTS.pas` | `TAiQwenRealtimeTTS` | Qwen streaming **text-to-speech** (text in → audio out) — standalone component, not a `TAiRealtimeBase` driver — **runtime-tested** (2026-09-28) |
| `uMakerAi.Realtime.WebSocket.pas` | `TAiRealtimeWSClient` (shim → `TAiWSClient`) | Compatibility alias; implementation in `Source/WebSocket/` |

### Class Hierarchy

```
TAiRealtimeBase (abstract)
  ├── TAiOpenAiRealtimeSTT    — wss://api.openai.com/v1/realtime, 24 kHz  (STT only)
  ├── TAiGeminiRealtimeSTT    — 16 kHz [STUB]
  └── TAiRealtimeVoiceBase (abstract — adds OnAssistantText/Delta, OnAudioChunk, OnAudioDone)
        ├── TAiMakerAiRealtimeChat  — wss://api.cimamaker.com/v1/audio/realtime, 24 kHz  (STT+LLM+TTS)
        ├── TAiGrokRealtimeChat     — wss://api.x.ai/v1/realtime, 24 kHz  (speech-to-speech)
        ├── TAiOpenAiLiveChat       — wss://api.openai.com/v1/live/sessions, 24/16 kHz  (full-duplex + delegation)
        ├── TAiQwenRealtimeBase     — wss://dashscope-intl.aliyuncs.com/api-ws/v1/realtime, 16 kHz in / 24 kHz out
        │     ├── TAiQwenRealtimeChat      'Qwen'          (omni speech-to-speech)
        │     ├── TAiQwenRealtimeSTT       'QwenSTT'       (live STT)
        │     └── TAiQwenRealtimeTranslate 'QwenTranslate' (simultaneous translation + voice)
        └── TAiRealtimeConnection   — universal connector (wraps concrete driver)
```

---

## TAiRealtimeBase — Base class

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `ApiKey` | string | API key (`@VAR_NAME` env resolution) |
| `Model` | string | Model name |
| `Language` | string | BCP-47 language hint (e.g. `'es'`, `'en'`) |
| `InputSampleRate` | Integer | Microphone sample rate (default 44100 Hz); auto-resampled to provider rate |
| `VADMode` | `TAiRealtimeVadMode` | `rvmServerVad`, `rvmSemanticVad`, `rvmManual` |
| `VADThreshold` | Double | Energy threshold for VAD (0.0–1.0) |
| `SilenceDurationMs` | Integer | Silence duration before speech-end event |
| `PrefixPaddingMs` | Integer | Pre-speech padding to include |
| `NoiseReduction` | `TAiNoiseReduction` | `nrNone`, `nrNearField`, `nrFarField` |
| `IsConnected` | Boolean | Read-only connection state |

### Events

| Event | Signature | When fires |
|-------|-----------|-----------|
| `OnConnected` | `procedure` | WebSocket handshake complete |
| `OnDisconnected` | `procedure` | Connection closed |
| `OnSessionReady` | `procedure` | Session configured on server |
| `OnSpeechStarted` | `(AudioMs: Integer; ItemId: string)` | VAD detected voice start |
| `OnSpeechStopped` | `(AudioMs: Integer; ItemId: string)` | VAD detected voice end |
| `OnTranscriptDelta` | `(Delta: string)` | Partial transcription chunk |
| `OnTranscriptCompleted` | `(Transcript, ItemId: string)` | Final transcription for one utterance |
| `OnError` | `(ErrorMsg, ErrorCode: string)` | Protocol or network error |

All events are dispatched via `TThread.Queue(nil, ...)` — safe to update UI directly.

### Methods

```pascal
procedure Connect;
procedure Disconnect;
procedure SendAudio(const PCMData: TBytes);   // PCM16 mono at InputSampleRate
procedure CommitAudio;                         // Flush pending audio (manual VAD)
procedure ClearAudio;                          // Cancel pending audio
```

---

## TAiRealtimeConnection — Universal connector

Same pattern as `TAiChatConnection`. Wraps a concrete driver instance, re-exposing all properties and events.

```pascal
AiRealtime := TAiRealtimeConnection.Create(nil);
AiRealtime.DriverName := 'OpenAI';
AiRealtime.ApiKey     := '@OPENAI_API_KEY';
AiRealtime.Model      := 'gpt-realtime-2.1';
AiRealtime.VADMode    := rvmServerVad;
AiRealtime.OnTranscriptCompleted := HandleTranscript;
AiRealtime.Connect;
```

`DriverName` values: `'OpenAI'`, `'OpenAiTranslate'`, `'OpenAiLive'`, `'MakerAi'`, `'Grok'`, `'Qwen'`, `'QwenSTT'`, `'QwenTranslate'`, `'Gemini'` (stub).  
Changing `DriverName` recreates the internal driver instance.

### Driver-specific properties: `DriverParams` (Sep 2026)

The connector only forwards the base properties (`ApiKey`, `Model`, `Language`, VAD...). Everything a driver adds on top — `Voice`, `Instructions`, `TargetLanguage`, `ReasoningEffort`, `Keyterms`, the Qwen `Url`… — goes in **`DriverParams`** (published `TStrings`, one `Property=Value` per line), applied to the driver by RTTI when it is created and again on `Connect`:

```pascal
AiRealtime.DriverName := 'Qwen';
AiRealtime.DriverParams.Values['Voice'] := 'Tina';
AiRealtime.DriverParams.Values['Instructions'] := 'Answer in one sentence.';
AiRealtime.DriverParams.Values['ReasoningEffort'] := 'greNone';   // Grok: enum by name
AiRealtime.DriverParams.Values['Keyterms'] := 'PUC|DIAN';         // TStrings: '|' separates items
```

Strings, integers, floats (invariant), enums by name, booleans and `TStrings` are supported. **A key the driver does not have (or an invalid value) is reported through `OnError` with code `driver_param` on `Connect`** — not when the driver is created, because `DriverParams` may legitimately carry keys for another driver while switching. What cannot be written as text (`AiFunctions`, driver-specific events, `CreateResponse`) is reached through the public `Instance` property: `(AiRealtime.Instance as TAiGrokRealtimeChat).AiFunctions := ...`.

The connector inherits from `TAiRealtimeVoiceBase`, so it also re-exposes the
voice events (`OnAssistantText`, `OnAssistantTextDelta`, `OnAudioChunk`,
`OnAudioDone`); with STT-only drivers those events simply never fire.

---

## TAiOpenAiRealtimeSTT — OpenAI driver

**Status:** Complete and tested.

- **Endpoint:** `wss://api.openai.com/v1/realtime?model=<model>`
- **Audio format:** PCM16, 24 kHz, mono, little-endian, base64-encoded chunks
- **Resampler:** Linear interpolation from `InputSampleRate` → 24000 Hz (in base class)

### Supported models

| Model | Notes |
|-------|-------|
| `gpt-realtime-2.1` | **Default** session model (Jul 2026) — better alphanumeric recognition and noise/silence handling |
| `gpt-realtime-2.1-mini` | Faster, lower cost |
| `gpt-realtime` | Previous generation |

`gpt-4o-realtime-preview` / `gpt-4o-mini-realtime-preview` were retired by OpenAI — do not use them.

### Transcription models (`TranscriptionModel` property)

| Enum | Model | Notes |
|------|-------|-------|
| `otmGptLiveTranscribe` | `gpt-live-transcribe` | **Default** (2026) — low-latency live STT, WER 9.60% |
| `otmGptTranscribe` | `gpt-transcribe` | Committed turns; uses prior turns as context |
| `otmGpt4oTranscribe` / `otmGpt4oMiniTranscribe` | `gpt-4o-transcribe[-mini]` | **Deprecated** (2026-08-26), shutdown 2027-02-26 |
| `otmWhisper1` | `whisper-1` | **Deprecated** (2026-08-26), shutdown 2027-02-26 |

The deprecated values stay in the enum so existing DFM/FMX files keep loading; migrate them to `otmGptLiveTranscribe` or `otmGptTranscribe`.

The new models accept context config (verified live 2026-08-01): `TranscriptionPrompt` (free-form topic), `TranscriptionKeywords` (domain terms, one per line), `Languages` (multi-language list; falls back to base `Language`), `LowDelay` (faster partials, live model only). Legacy models keep the singular `language` field — the driver switches the session.update schema automatically. OpenAI deltas are **incremental** (unlike Grok's cumulative transcript).

### Internal protocol flow

1. WebSocket connect with `Authorization: Bearer <ApiKey>` header
2. Send `session.update` with VAD config, input/output format, transcription model
3. Stream PCM16 chunks via `input_audio_buffer.append` (base64)
4. Receive `input_audio_buffer.speech_started` → `OnSpeechStarted`
5. Receive `conversation.item.input_audio_transcription.delta` → `OnTranscriptDelta`
6. Receive `conversation.item.input_audio_transcription.completed` → `OnTranscriptCompleted`

### Key implementation notes

- `FConnectThread: TThread` — connection runs on a background thread
- `FSessionConfigured: Boolean` — guards against sending audio before session ready
- Session config sent in `OnSessionReady` handler, not in `Connect`
- `TAiRealtimeWSClient` handles fragmented WebSocket frames automatically

---

## TAiOpenAiRealtimeTranslate — streaming speech translation

**Status:** Complete — runtime-tested (2026-08-01): es→en text + TTS audio verified.

- **Endpoint:** `wss://api.openai.com/v1/realtime/translations?model=gpt-realtime-translate` (unit `uMakerAi.Realtime.OpenAI.pas`, DriverName `'OpenAiTranslate'`)
- **Continuous stream — no VAD, no turns**: keep appending audio (including silence between phrases); results arrive as they're ready. `CommitAudio`/`ClearAudio` are no-ops.
- Inherits `TAiRealtimeVoiceBase`; event mapping:
  - `session.input_transcript.delta` → `OnTranscriptDelta` (source language) — **opt-in** via `SourceTranscription := True` (adds `audio.input.transcription {model: gpt-live-transcribe}`; the endpoint default is transcription:null)
  - `session.output_transcript.delta` → `OnAssistantTextDelta` (translated text)
  - `session.output_audio.delta` → `OnAudioChunk` (translated TTS, PCM16 24 kHz)
  - `session.closed` → `OnAssistantText` (full) + `OnAudioDone`
- `TargetLanguage` ('en', 'es', ...) sent as `audio.output.language` in session.update (sent right after connect; `session.created/updated` fire `OnSessionReady` once)
- `Disconnect` sends `session.close` before closing the socket
- Demo: `Demos/071-VoiceBridgeTranslate` — the 063 voice bridge refactored to a single socket per direction (STT→LLM→TTS pipeline replaced entirely)

---

## TAiOpenAiLiveChat — OpenAI GPT-Live (full-duplex voice)

**Status:** runtime-tested against the live API (2026-10-05): conversation, local function through Responses delegation, and client delegation through `DelegateChat`. Unit `uMakerAi.Realtime.OpenAI.Live.pas`, DriverName `'OpenAiLive'`, key `@OPENAI_API_KEY`.

GPT-Live listens while it speaks: audio streams continuously and the model decides when to talk (no VAD, commits or turns; `CommitAudio`/`ClearAudio` are no-ops). Heavy reasoning is **delegated**:

| Mode | Who resolves the task | Use when |
|---|---|---|
| `Delegation = ldResponses` (default) | A Responses model run by OpenAI (`DelegationModel`, default `gpt-6-luna`) with `EnableWebSearch`, `AiFunctions`, `CustomToolsJson`, `ToolChoice`, `ReasoningEffort`, `MaxOutputTokens`, `ParallelToolCalls`, `DelegationInstructions` | Best latency (measured ≈ 2 s from delegation to answer, with a local function); OpenAI keeps the full context |
| `DelegateChat` assigned (forces `ldClient`) | Any `TAiChatConnection` — Claude, Ollama, an agent graph, RAG… | The brain must not be OpenAI (privacy, own data, own agents). Measured ≈ 4.9 s with `gpt-6-luna` |
| `ldClient` without `DelegateChat` | Your code, in `OnDelegation(DelegationId, Context)`; answer with `AppendCommentary(Text, DelegationId)` | Custom flows |

- **Protocol** (types from the official `openai-python` SDK, `src/openai/types/live`): `wss://api.openai.com/v1/live/sessions`, `Authorization: Bearer`. The model goes in `session.start` (not in the URL); wait for `session.started` before any other command (the driver drops audio until then). Audio: `session.input_audio.append` (base64 without line breaks) / `session.output_audio.delta`.
- **Voice, audio format and instructions are immutable** after `session.start`. `AudioRate`: `lar24k` (default) or `lar16k`; G.711 8 kHz is not supported by the driver.
- **Function calling (ldResponses):** `response.event` wraps the Responses stream; on `response.output_item.done` with a `function_call` the driver runs it on its own thread (`AiFunctions.DoCallFunction`, then `OnCallToolFunction` synchronized), sends `response.item.create` (`function_call_output`) and ONE `response.create` once the response that asked for it reached `response.completed` — also when it completes before the function returns. Tracked per `delegation_id`.
- **Client delegation:** `session.delegation.created` carries only an id, **not the task text**. The driver builds it from the transcript: what `DelegateChat` has not seen yet (it keeps its own history), closed and open turns in timeline order. `DelegateChat` runs on a worker thread, one task at a time (`Asynchronous` is forced to `False` on `Connect`); the answer goes back with `session.commentary.append`, split in ≤ 400-byte UTF-8 chunks (the API caps each append at 500 tokens). Nobody handling a client delegation → `OnError` (`delegation_unhandled`) and a commentary so the model does not wait in silence. With `ldResponses` the server only accepts `delegation_id: null` in appends; the driver enforces it.
- **Turns.** Transcripts have no turn boundaries and no "done" event, and full-duplex speech overlaps (verified: the model started "La capital" at 6400 ms while the user was still saying "favor." at 6600–6800). Deltas fire on arrival; `OnTranscriptCompleted` / `OnAssistantText` use the timeline (`start_ms`/`end_ms`) with the SDK grouper policy: the turn changes only when the other speaker starts ≥ 500 ms after the current turn's end (overlapping speech waits); a fragment that started before the current turn is a late transcript of the previous one (1 s window); the assistant turn closes after 2 s without text **and without voice in the output audio** (mean |PCM16| > 500). Transcripts arrive in bursts: with the local clock alone a 1 s pause between sentences arrived as > 2 s and a 2-minute story came out split (verified live). A turn closed for inactivity waits 1 s and is **resumed** if the same speaker continues less than 2 s later on the timeline. Unlike the SDK, short acknowledgments ("mhm") are **not** dropped. The local clock (`NowMs`, virtual) is checked on every event — output audio arrives every ~100 ms.
- **Output audio is a continuous stream including silence**, with no timestamps or end event: `OnAudioDone` fires only on `session.closed`. Measure latency with the first assistant text delta (≈ 0.5 s after the question ended), not with the first audio chunk.
- **Events run on the main thread.** `TAiWSClient` delivers frames with `TThread.Queue`, so `ProcessServerEvent` runs on the main thread. `Disconnect` sends `session.close` and waits for `session.closed` (`CloseTimeoutMs`, default 5000) **draining the queue**; waiting without draining blocked it until the timeout (measured: 5.0 s → 0.7 s after the fix).
- **Barge-in is real.** Anything the model hears while speaking — a cough, an "mm", or **its own voice** leaking into the microphone (speakers, loud headphones) — makes it stop. A long story "cut off" in the demo for that reason; with clean input (silence or steady noise) it finishes. With speakers, mute the microphone while the output has voice (`TAiAudioCapture.Muted`, demo 092 "speaker mode") or use `Mute`/`Unmute` (`session.input_audio.mute`).
- Extra events: `OnUsage(Seconds, ContextRatio)` (cumulative, do not add up), `OnSessionClosed(Reason, Seconds)` (`close_requested`, `expired`, `content`, `remote_hangup`, `connection_lost`), `OnResponseEvent` (raw nested Responses events). Public: `Mute`/`Unmute`, `AppendCommentary`/`AppendThinking`/`AppendInstructions`, `SessionId`, `UsageSeconds`, `CloseReason`.
- Cost: $0.05 per minute of session, billed per second (plus the delegated model's tokens).
- Regression suite: `realtime.openai-live.*` (session.start, events, turns, overlap with the real trace, long story with bursty transcripts, tools, client delegation, `DelegateChat` against a closed port).
- Demo: `Demos/092-GPTLiveVoice` (FMX: microphone, speaker, voice, delegation mode, local functions, speaker mode, interruption diagnostics).

---

## TAiGrokRealtimeChat — xAI Grok Voice driver

**Status:** Complete — runtime-tested against the live API (2026-07-31): STT transcription, assistant text (delta + complete) and TTS audio response all verified. API key env var: `GROK_API_KEY`.

- **Endpoint:** `wss://api.x.ai/v1/realtime?model=<model>` — protocol compatible with OpenAI Realtime
- **Audio:** PCM16, 24 kHz, mono, base64 over JSON (`input_audio_buffer.append` / `response.output_audio.delta`)
- **Default model:** `grok-voice-think-fast-2.0` (pinned; `grok-voice-latest` is a moving alias)
- **Auth:** `Authorization: Bearer` — do **not** send `Sec-WebSocket-Protocol` (Grok reserves the subprotocol slot for ephemeral tokens `xai-client-secret.*`)

### Driver-specific published properties

| Property | Purpose |
|----------|---------|
| `Voice` | TTS voice: `eva`, `ara`, `rex`, `sal`, `leo`... or a Custom Voices `voice_id` |
| `Instructions` | Session system prompt |
| `ReasoningEffort` | `greHigh` (default) / `greNone` (lower latency) |
| `IdleTimeoutMs` | Re-engagement timeout when user is silent (0 = omit) |
| `OutputSpeed` | TTS playback speed 0.7–1.5 (0 = server default) |
| `AiFunctions` | `TAiFunctions` component — local functions + MCP declared as session tools |
| `OnCallToolFunction` | Fallback tool handler when `AiFunctions` doesn't handle the call (synchronized to main thread) |
| `EnableWebSearch` / `EnableXSearch` | xAI native server-side tools |
| `Keyterms` | Up to 100 domain terms for transcription biasing |
| `PronunciationReplace` | `Phrase=SpokenAs` TTS corrections (case-insensitive, whole word) |

### Function calling (verified live 2026-07-31)

Assign `AiFunctions` (or `OnCallToolFunction`). The driver declares the tools in `session.update` (flat OpenAI Responses format via `GetTools(tfOpenAIResponses)` — exactly what Grok expects) and handles the round-trip automatically:

1. `response.function_call_arguments.done` → executes the call on its own thread (never blocks the WebSocket reader); `TAiFunctions.DoCallFunction` first, then the `OnCallToolFunction` fallback (via `TThread.Synchronize`).
2. Sends `conversation.item.create` (`function_call_output` with `call_id`) per call.
3. Sends ONE `response.create` only after ALL pending calls finished AND the tool turn's `response.done` arrived (xAI requirement) — guarded by `TInterlocked.CompareExchange`.
4. Grok may speak *before* calling the tool ("let me check..."); the driver carries that text so the final `OnAssistantText` contains the full turn (pre-tools + post-tools). `OnAudioDone` fires only on the final response.

`ForceMessage(Text, Interruptible)` (public) sends a scripted TTS utterance bypassing the model (`conversation.item.create` with `item.type: force_message`) — IVR prompts, legal disclosures.

### Phase 3 features (verified live 2026-08-01)

| Feature | API | Notes |
|---------|-----|-------|
| Session resumption | `EnableResumption` + `ConversationId` (public r/w) | `resumption:{enabled:true}` in session.update; id captured from `conversation.created`; reconnect appends `?conversation_id=`. Cached turns (user, assistant, tool calls/outputs) replay as `conversation.item.added` events (doc says `.created` — verified it's `.added`). History expires after 30 min |
| Binary audio transport | `BinaryAudio: Boolean` | `audio.input/output.transport: "binary"`; input sent via `SendBinary` (raw PCM frames), output arrives as WS binary frames (handled in `OnWSFrame`, replaces `response.output_audio.delta`). ~33% less bandwidth. JSON events stay text |
| Ephemeral tokens | `EphemeralToken` property + `MintEphemeralToken(ApiKey, Seconds)` class function | Minted via `POST /v1/realtime/client_secrets` (token value starts with `xai-realtime-client-secret-`); passed as WS subprotocol `xai-client-secret.<token>` (driver auto-prefixes); ApiKey NOT sent. For mobile/browser clients — mint on your backend, hand the token to the client |
| file_search tool | `FileSearchCollections` (vector_store_ids) + `FileSearchMaxResults` | xAI Collections; server-side |
| Custom/MCP tools | `CustomToolsJson: TStrings` | Raw JSON object or array appended to session tools — covers `{"type":"mcp","server_url":...}` and future shapes |

**Still pending:** Opus codec (no native Delphi encoder — binary PCM already removes the base64 overhead), continuous-microphone VAD validation, `Demos09-Realtime/02-GrokVoice` demo.

### Protocol differences vs OpenAI handled by the driver (verified live 2026-07-31)

1. User transcription arrives **cumulative** in `conversation.item.input_audio_transcription.updated` (not `.delta`); the driver diffs against the previous text to keep `OnTranscriptDelta` semantics. The server may rewrite the prefix — the driver then emits the full text as delta.
2. Grok also emits `...transcription.completed` for **partial** segments with `status: "in_progress"`; only `status: "completed"` fires `OnTranscriptCompleted` (exactly once per utterance).
3. Assistant spoken text arrives in `response.output_audio_transcript.delta` / `.done` (NOT `response.text.delta`, which the driver also accepts for compat) → `OnAssistantTextDelta` per delta; `.done` carries the authoritative full transcript → `OnAssistantText` fires on `response.done`.
4. Only `server_vad` (no `semantic_vad`, no `noise_reduction`); `rvmSemanticVad` maps to `server_vad`. Grok's VAD threshold default is 0.85 (constructor sets it).
5. `language_hint` requires regional BCP-47 for Spanish/Portuguese — driver maps `es`→`es-MX`, `pt`→`pt-BR`.
6. `input_audio_buffer.speech_started/stopped` ARE emitted → `OnSpeechStarted/Stopped`. Extra events `conversation.created`, `conversation.item.added`, `ping` are ignored harmlessly.

### Burst-send caveat (file audio vs live microphone)

With audio sent faster than real time and then stopped (file upload pattern), the server VAD detects speech start but **never closes the turn** — not even after 3 s of trailing silence/noise. For file-based sends, call `CommitAudio` + `CreateResponse` after streaming the audio; the server then emits `speech_stopped`, `input_audio_buffer.committed`, the final transcription and the response. Continuous microphone streaming (VoiceMonitor) keeps the VAD timeline alive and closes turns automatically.

`CreateResponse` (public method) sends `response.create` — required after `CommitAudio` in `rvmManual` mode, optional forcing mechanism otherwise.

Phase 3 (resumption, binary transport, ephemeral tokens, file_search/mcp declarations) — see the phase 3 table below.

---

## Qwen drivers — Alibaba Model Studio (DashScope)

**Status:** Complete — runtime-tested through `TAiRealtimeConnection` (2026-09-28): STT, voice conversation (user transcript, assistant text and 9 s of TTS; the audio re-transcribed matches the text) and es→en translation with voice. Added as a new unit only: no change to the base classes, the connector or the WebSocket client. API key: `@DASHSCOPE_API_KEY` (applied when the connector passes an empty `ApiKey`).

- **Endpoint:** `wss://dashscope-intl.aliyuncs.com/api-ws/v1/realtime?model=<model>` (`Url` property for other regions), `Authorization: Bearer`. OpenAI Realtime **beta** event names (`response.audio.delta`, `response.audio_transcript.delta/.done`); the GA names are accepted too.
- **Audio:** input PCM16 mono **16 kHz** for every model (`TargetSampleRate = 16000`; the base resamples from `InputSampleRate`); output PCM16 **24 kHz**.
- **Base64 without line breaks.** `TNetEncoding.Base64` wraps every 76 chars; DashScope rejects that with *"Illegal base64 character d"* (xAI and OpenAI tolerate it). The driver uses `TBase64Encoding.Create(0)`.

| DriverName | Class | Default model | Events |
|---|---|---|---|
| `Qwen` | `TAiQwenRealtimeChat` | `qwen3.8-omni-flash-realtime` | STT events + `OnAssistantTextDelta/Text`, `OnAudioChunk`, `OnAudioDone`. `Instructions` = system prompt |
| `QwenSTT` | `TAiQwenRealtimeSTT` | `qwen3-asr-flash-realtime` | `OnTranscriptDelta/Completed`. `Language` → `input_audio_transcription.language` |
| `QwenTranslate` | `TAiQwenRealtimeTranslate` | `qwen3.8-livetranslate-flash-realtime` | source language in `OnTranscriptDelta/Completed`, translation in `OnAssistantTextDelta/Text`, translated voice in `OnAudioChunk`. `TargetLanguage` (default `en`) |

Other models work through `Model`: `qwen3.5-omni-flash/plus-realtime`, `qwen3-omni-flash-realtime`, `qwen3-livetranslate-flash-realtime`, `qwen3.5-livetranslate-flash-realtime`.

### Protocol differences handled by the drivers (verified live)

1. **User transcription arrives in three shapes**, normalized to incremental `OnTranscriptDelta` (if the server rewrites the beginning the full text is emitted, same rule as Grok):
   - omni: `...input_audio_transcription.delta` with `text: ""` and `stash` = accumulated text;
   - asr: `...input_audio_transcription.text` with `text` = confirmed + `stash` = pending;
   - translate: `...input_audio_transcription.delta` with an incremental `delta`.
2. **Voices are per model** and a foreign voice is an error (`Voice 'Cherry' is not supported`): qwen3.8-omni defaults to Tina, qwen3-omni to Cherry. Empty `Voice` = server default. **Exception, translate:** a `session.update` without a voice makes the server fall back to Chelsie, which that model rejects — `QwenTranslate` sends `Tina` by default.
3. VAD: `server_vad` (`rvmSemanticVad` maps to it); `rvmManual` sends `turn_detection: null` → `CommitAudio` + `CreateResponse`. **The translate model uses `speaker_detection` with 2.5 s of silence**: a turn closes only after ~2.5 s without speech (file tests must trail at least 3 s of silence).
4. `session.created` and `session.updated` both arrive; `OnSessionReady` fires once.
5. `qwen3-tts-*-realtime` (text in → audio out) is covered by the separate component `TAiQwenRealtimeTTS` (next section). `qwen3-s2s-flash-realtime` is **not available to the tested account**: the server closes the socket right after `session.created`, with or without `session.update`.

## TAiQwenRealtimeTTS — streaming text-to-speech (standalone component)

The realtime hierarchy is audio-in (microphone → server); this goes the other way, so it is a separate `TComponent` (same WebSocket client, same `TThread.Queue` dispatch, same base64 rule). Typical use: speak an LLM answer while it is being generated.

```pascal
TTS := TAiQwenRealtimeTTS.Create(nil);          // Voice 'Cherry', Mode tmServerCommit
TTS.OnAudioChunk := HandlePcm24k;               // PCM16 mono 24 kHz, as it is produced
TTS.Connect;                                    // wait OnSessionReady
Chat.OnReceiveData    := procedure(...) begin TTS.AppendText(aText) end;  // each LLM delta
Chat.OnReceiveDataEnd := procedure(...) begin TTS.Finish end;             // flush + OnFinished
```

- Protocol (verified live): client `session.update {voice, mode, response_format: pcm, sample_rate, language_type, instructions}`, `input_text_buffer.append {text}`, `input_text_buffer.commit`, `session.finish`; server `response.audio.delta`, `response.done` (per utterance), `session.finished`.
- `Mode`: `tmServerCommit` (server decides when to speak; `Finish` flushes the rest) or `tmCommit` (`Commit` / `Speak` closes each utterance; several responses in one session — verified with 2).
- Measured latency: first audio **~0.5 s** after the first text fragment; LLM → voice (qwen3.8-flash streaming into the TTS) **2.5 s** from the question to the first audio.
- Models: `qwen3-tts-flash-realtime` [default], `qwen3-tts-instruct-flash-realtime` (`Instructions`: reading style, e.g. whisper — verified), `qwen3-tts-vc-realtime-2026-01-15` / `qwen3-tts-vd-realtime-2026-01-15` for custom voices. **A custom voice must be created for the realtime model** (`TAiQwenVoices.CloneModel := 'qwen3-tts-vc-realtime-2026-01-15'`); `EffectiveModel` picks the realtime vc/vd model from the voice id prefix.
- `Language`: ISO code or English name (`es` → `Spanish`); empty = auto.
- All outputs verified by re-transcribing the audio (fragments, two utterances, LLM answer, instruct, cloned voice).

---

## TAiGeminiRealtimeSTT — Gemini driver (STUB)

**Status:** Skeleton only. All methods raise `ENotImplemented` or are no-ops.

- **Planned endpoint:** Gemini Live API WebSocket
- **Planned audio format:** PCM16, 16 kHz, mono
- **Implementation target:** v3.5

When implementing:
1. Override `InternalConnect` — connect to Gemini Live WebSocket
2. Override `InternalSendAudio` — send PCM16 16 kHz chunks
3. Override `InternalDisconnect`, `InternalCommitAudio`, `InternalClearAudio`
4. Parse Gemini Live JSON events → call inherited `FireXxx` event dispatchers

---

## TAiRealtimeWSClient — WebSocket client (shim)

`uMakerAi.Realtime.WebSocket.pas` is now a **compatibility shim** that re-exports `TAiRealtimeWSClient` as an alias for `TAiWSClient` from `Source/WebSocket/`. The actual implementation lives in:

| Unit | Class | Role |
|------|-------|------|
| `uMakerAi.WebSocket.Client.pas` | `TAiWSClient` | RFC 6455 + HTTP Upgrade + reader thread |
| `uMakerAi.WebSocket.SChannel.pas` | `TSChannelTransport` | TLS — Windows (`secur32.dll`, zero extra DLLs) |
| `uMakerAi.WebSocket.Android.pas` | `TAndroidSSLTransport` | TLS — Android (`javax.net.ssl` via JNI) |
| `uMakerAi.WebSocket.OpenSSL.pas` | `TOpenSSLTransport` | TLS — Linux/macOS (`dlopen(libssl.so)`) |

### Platform support

| Platform | TLS backend | Status |
|----------|-------------|--------|
| Windows Win64 | `TSChannelTransport` (`secur32.dll`) | ✅ Tested |
| Android ARM/ARM64 | `TAndroidSSLTransport` (`javax.net.ssl` via JNI) | ⚠️ Compiles, not yet tested on real hardware |
| Linux64 | `TOpenSSLTransport` (`libssl.so.3` or `libssl.so.1.1`) | ⚠️ Compiles, not yet tested on real hardware |
| macOS | `TOpenSSLTransport` (`libssl.dylib`) | ⚠️ Compiles, not yet tested |
| iOS | — | ❌ Not implemented |

### Why SChannel instead of WinHTTP?

The previous implementation used WinHTTP. The rewrite uses a pure-Pascal RFC 6455 client (`TAiWSClient`) with a pluggable `ITlsTransport` interface. On Windows, `TSChannelTransport` delegates TLS to `secur32.dll` (Schannel — the Windows system TLS stack), which requires no external DLLs and handles Cloudflare's CDN reliably.

### Why OpenSSL via dlopen on POSIX?

`secur32.dll` is Windows-only. On Linux/macOS, `TOpenSSLTransport` loads `libssl` at runtime via `dlopen` — it searches for `libssl.so.3` → `libssl.so.1.1` → `libssl.dylib` in order. This avoids a hard link-time dependency.

**Linux prerequisite:** `libssl3` (Ubuntu 22.04+) or `libssl1.1` (Ubuntu 20.04/Debian 11):
```bash
apt install libssl3     # Ubuntu 22.04+ / Debian 12
apt install libssl1.1   # Ubuntu 20.04 / Debian 11
```

### Key implementation details — TAiWSClient

- Pure-Pascal RFC 6455: HTTP Upgrade handshake, frame encode/decode, masking, fragmentation, control frames (Ping/Pong/Close)
- Reader thread polls receive loop; multi-frame messages are reassembled automatically
- Client masking: MASK=1 with 4-byte random key (required by RFC 6455 §5.3)
- Thread safety: `TCriticalSection` on send path; events dispatched via `TThread.Queue`
- Ping/Pong: reader thread responds to server Ping automatically

---

## Audio pipeline

```
Microphone (TAIVoiceMonitor)
  │  PCM16, InputSampleRate Hz (e.g. 44100)
  ▼
TAiRealtimeBase.SendAudio()
  │  Linear resample → provider rate (24000 or 16000 Hz)
  ▼
TAiOpenAiRealtimeSTT / TAiGeminiRealtimeSTT
  │  Base64 encode → WebSocket chunk
  ▼
Provider API → VAD → Transcription events
```

`TAIVoiceMonitor` (`Source/Utils/`) captures from the default microphone and exposes an `OnAudioData: TBytes` event. Wire it to `AiRealtime.SendAudio(Data)`.

---

## Factory registration

Drivers self-register in their `initialization` section:

```pascal
// uMakerAi.Realtime.OpenAI.pas
initialization
  TAiRealtimeFactory.Instance.RegisterDriver('OpenAI', TAiOpenAiRealtimeSTT);

// uMakerAi.Realtime.Gemini.pas
initialization
  TAiRealtimeFactory.Instance.RegisterDriver('Gemini', TAiGeminiRealtimeSTT);

// uMakerAi.Realtime.Grok.pas
initialization
  TAiRealtimeFactory.Instance.RegisterDriver('Grok', TAiGrokRealtimeChat);

// uMakerAi.Realtime.Qwen.pas — tres drivers
initialization
  TAiRealtimeFactory.Instance.RegisterDriver('Qwen', TAiQwenRealtimeChat);
  TAiRealtimeFactory.Instance.RegisterDriver('QwenSTT', TAiQwenRealtimeSTT);
  TAiRealtimeFactory.Instance.RegisterDriver('QwenTranslate', TAiQwenRealtimeTranslate);
```

Import the driver unit to activate registration (same pattern as Chat drivers).

---

## Minimal usage example

```pascal
uses
  uMakerAi.Realtime.AiConnection,
  uMakerAi.Realtime.OpenAI;   // registra el driver

var
  STT: TAiRealtimeConnection;

// Setup
STT := TAiRealtimeConnection.Create(nil);
STT.DriverName  := 'OpenAI';
STT.Model       := 'gpt-realtime-2.1';
STT.VADMode     := rvmServerVad;
STT.OnTranscriptDelta     := procedure(Delta: string) begin Write(Delta); end;
STT.OnTranscriptCompleted := procedure(Text, Id: string) begin WriteLn; WriteLn('→ ', Text); end;

// Connect and stream
STT.Connect;
// ... wire TAIVoiceMonitor.OnAudioData → STT.SendAudio(Data) ...
// STT.Disconnect when done
```

---

## Thread safety

| What | Mechanism |
|------|-----------|
| WebSocket send | `TCriticalSection` in `TAiRealtimeWSClient` |
| Event dispatch | `TThread.Queue(nil, proc)` — all events fire on main thread |
| Connect sequence | Separate `TThread` descendant (`FConnectThread`) |
| Reader loop | `TAiRealtimeWSReaderThread` on background thread |

### Console apps and services must drain the queue

Because **every** event is dispatched with `TThread.Queue(nil, ...)`, it only
runs when somebody drains the main-thread queue. A VCL/FMX app does that in its
message loop; a **console app, a daemon or a Linux service does not**, so unless
you call `CheckSynchronize` periodically **no event ever fires** — not
`OnSessionReady`, not `OnTranscriptCompleted`, not even `OnError`.

The failure is confusing: the WebSocket connects, the server accepts the
upgrade, audio is sent, and the program still concludes it never connected —
with no error to show for it.

```pascal
// En vez de Sleep(100) en cualquier espera:
for I := 1 to 100 do
begin
  CheckSynchronize(100);   // System.Classes
  if Listo then Break;
end;
```

Demos 062 and 063 do this. Verified on Linux 2026-09-20: the same program
times out waiting for the session with `Sleep`, and transcribes correctly with
`CheckSynchronize`.

---

## Known issues / limitations

- **Gemini driver is a stub** — raises `ENotImplemented` on `Connect`
- **POSIX not yet tested** — `TOpenSSLTransport` + `TAiWSClient` compile on Linux/macOS but have not been validated on real hardware
- **POSIX prerequisite** — Linux requires `libssl3` or `libssl1.1`; macOS requires LibreSSL (system) or OpenSSL (Homebrew)
- **Android** — `TAndroidSSLTransport` compiles but has not been validated on real hardware
- **iOS** — `ITlsTransport` not yet implemented

---

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
