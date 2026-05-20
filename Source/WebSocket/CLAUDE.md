# CLAUDE.md — Source/WebSocket

This file provides guidance to Claude Code when working with the WebSocket module of MakerAI.

## Overview

`Source/WebSocket/` is a **standalone RFC 6455 WebSocket client** used by the Realtime module. It has no dependency on Indy, WinHTTP, or any third-party library. TLS is provided by a pluggable `ITlsTransport` interface with platform-specific implementations.

## Module Structure

### Units

| File | Class | Role |
|------|-------|------|
| `uMakerAi.WebSocket.Client.pas` | `TAiWSClient` | RFC 6455 + HTTP Upgrade handshake + reader thread |
| `uMakerAi.WebSocket.SChannel.pas` | `TSChannelTransport` | TLS via `secur32.dll` (Windows native) |
| `uMakerAi.WebSocket.OpenSSL.pas` | `TOpenSSLTransport` | TLS via `dlopen(libssl.so)` (Linux/macOS) |
| `uMakerAi.WebSocket.Android.pas` | `TAndroidSSLTransport` | TLS via `javax.net.ssl.SSLSocketFactory` (JNI) |

### Class / Interface Hierarchy

```
ITlsTransport  (interface)
  ├── TSChannelTransport    — Windows (secur32.dll, zero extra DLLs)
  ├── TAndroidSSLTransport  — Android (BoringSSL via JNI, zero extra DLLs)
  └── TOpenSSLTransport     — Linux/macOS (libssl via dlopen)

TAiWSClient
  └── uses ITlsTransport (injected at construction)
```

---

## TAiWSClient

Full RFC 6455 WebSocket client. Handles:
- HTTP Upgrade handshake (Sec-WebSocket-Key / Accept)
- Frame encoding/decoding: text, binary, continuation, Ping, Pong, Close
- Client-side masking (MASK=1 with 4-byte random key — required by RFC 6455 §5.3)
- Fragmented messages: reassembled before delivering to caller
- Background reader thread; events dispatched via `TThread.Queue` (main-thread safe)

### Key methods

```pascal
constructor Create(Transport: ITlsTransport);
procedure Connect(const Host, Path: string; Port: Integer;
  const ExtraHeaders: TStrings = nil);
procedure Disconnect;
procedure SendText(const Text: string);
procedure SendBinary(const Data: TBytes);
// Events: OnOpen, OnMessage, OnClose, OnError
```

---

## TSChannelTransport — Windows TLS

Uses `secur32.dll` (Schannel), which is part of Windows since Vista. No additional DLLs required. Uses the system certificate store — certificates are updated automatically by Windows Update.

**Why Schannel instead of OpenSSL on Windows:** The OpenAI Realtime API routes through Cloudflare's CDN. In certain environments Cloudflare rejects TLS handshakes from OpenSSL/Indy clients on Windows. Schannel is accepted reliably.

---

## TOpenSSLTransport — POSIX TLS

Loads `libssl` at runtime via `dlopen`. Search order:
1. `libssl.so.3`
2. `libssl.so.1.1`
3. `libssl.dylib`

**Linux prerequisites:**
```bash
apt install libssl3     # Ubuntu 22.04+ / Debian 12
apt install libssl1.1   # Ubuntu 20.04 / Debian 11
```

**macOS:** Uses LibreSSL (system) or OpenSSL from Homebrew (`brew install openssl`).

---

## Platform Support

| Platform | Transport | Status |
|----------|-----------|--------|
| Windows Win64 | `TSChannelTransport` | ✅ Tested |
| Android ARM/ARM64 | `TAndroidSSLTransport` | ⚠️ Compiles, not yet tested on real hardware |
| Linux64 | `TOpenSSLTransport` | ⚠️ Compiles, not yet tested on real hardware |
| macOS | `TOpenSSLTransport` | ⚠️ Compiles, not yet tested |
| iOS | — | ❌ Not implemented |

---

## Usage by the Realtime Module

`uMakerAi.Realtime.WebSocket.pas` is a compatibility shim that re-exports `TAiRealtimeWSClient` as an alias for `TAiWSClient`. Code in the Realtime module that previously used `TAiRealtimeWSClient` continues to work unchanged.

To use `TAiWSClient` directly (e.g. for a future non-Realtime WebSocket feature):

```pascal
uses
  uMakerAi.WebSocket.Client,
  uMakerAi.WebSocket.SChannel;   // or OpenSSL for POSIX

var
  WS: TAiWSClient;
begin
  WS := TAiWSClient.Create(TSChannelTransport.Create);
  WS.OnMessage := HandleMessage;
  WS.Connect('api.openai.com', '/v1/realtime?model=gpt-4o-realtime-preview', 443);
  // ...
  WS.Disconnect;
  WS.Free;
end;
```

---

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview, [../../CLAUDE.md](../../CLAUDE.md) for project overview, and [../Realtime/CLAUDE.md](../Realtime/CLAUDE.md) for how this module is used.
