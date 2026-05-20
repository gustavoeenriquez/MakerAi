# CLAUDE.md — Source/Realtime

This file provides guidance to Claude Code when working with the Realtime module of MakerAI.

## Overview

The `Source/Realtime/` module provides real-time Speech-to-Text (STT) streaming via WebSocket. It is a **parallel hierarchy** to `TAiChat` — it does not inherit from it. The design mirrors the Chat module: a base class, provider-specific drivers, and a universal connector.

Demos:
- Console: `Demos/Console/Demos09-Realtime/01-RealtimeSTT/`
- FMX: `Demos/FMX/Demos09-Realtime/01-RealtimeSTT/`

## Module Structure

### Units

| File | Class | Role |
|------|-------|------|
| `uMakerAi.Realtime.pas` | `TAiRealtimeBase`, `TAiRealtimeFactory` | Abstract base + factory |
| `uMakerAi.Realtime.AiConnection.pas` | `TAiRealtimeConnection` | Universal connector (same pattern as `TAiChatConnection`) |
| `uMakerAi.Realtime.OpenAI.pas` | `TAiOpenAiRealtimeSTT` | OpenAI driver — **complete** |
| `uMakerAi.Realtime.Gemini.pas` | `TAiGeminiRealtimeSTT` | Gemini driver — **stub, pending** |
| `uMakerAi.Realtime.WebSocket.pas` | `TAiRealtimeWSClient` (shim → `TAiWSClient`) | Compatibility alias; implementation in `Source/WebSocket/` |

### Class Hierarchy

```
TAiRealtimeBase (abstract)
  ├── TAiOpenAiRealtimeSTT   — wss://api.openai.com/v1/realtime, 24 kHz
  ├── TAiGeminiRealtimeSTT   — 16 kHz [STUB]
  └── TAiRealtimeConnection  — universal connector (wraps concrete driver)
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
AiRealtime.Model      := 'gpt-4o-realtime-preview';
AiRealtime.VADMode    := rvmServerVad;
AiRealtime.OnTranscriptCompleted := HandleTranscript;
AiRealtime.Connect;
```

`DriverName` values: `'OpenAI'`, `'Gemini'` (stub).  
Changing `DriverName` recreates the internal driver instance.

---

## TAiOpenAiRealtimeSTT — OpenAI driver

**Status:** Complete and tested.

- **Endpoint:** `wss://api.openai.com/v1/realtime?model=<model>`
- **Audio format:** PCM16, 24 kHz, mono, little-endian, base64-encoded chunks
- **Resampler:** Linear interpolation from `InputSampleRate` → 24000 Hz (in base class)

### Supported models

| Model | Notes |
|-------|-------|
| `gpt-4o-realtime-preview` | Default; full quality |
| `gpt-4o-mini-realtime-preview` | Faster, lower cost |

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
STT.Model       := 'gpt-4o-realtime-preview';
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
