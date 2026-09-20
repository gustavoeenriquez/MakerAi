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

> **Gotcha for daemons/servers:** `TThread.Queue` is only drained by
> `CheckSynchronize`. A console daemon whose main loop is `while Running do
> Sleep(...)` — such as MKAIServer's `ResApiServer` — never calls it, so
> **none of these events would ever fire there**. Either pump
> `CheckSynchronize` in the main loop or give the client a synchronous dispatch
> path before embedding it in a server.

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
| Linux64 | `TOpenSSLTransport` | ✅ Tested 2026-09-19 (see below) |
| macOS | `TOpenSSLTransport` | ⚠️ Compiles, not yet tested |
| iOS | — | ❌ Not implemented |

### Linux64 — tested 2026-09-19

Console client built with the Linux64 compiler and run on a real Ubuntu host
(libssl.so.3) against `wss://api.openai.com/v1/responses`: TLS handshake, RFC
6455 upgrade, **2190 text frames** parsed, two full responses, clean exit.

**It did not work before that run.** `SSL_set_tlsext_host_name` is *not* an
exported symbol — it is a macro in `ssl.h` over `SSL_ctrl` — so `dlsym` always
returned nil, the `if Assigned(...)` guard skipped SNI **silently**, and the
handshake died with `sslv3 alert handshake failure` (alert 40) against any host
behind a CDN, which today is almost any host. Reproduced exactly with
`openssl s_client -noservername`. Fixed by binding `SSL_ctrl` and calling it
with `SSL_CTRL_SET_TLSEXT_HOSTNAME` (55) / `TLSEXT_NAMETYPE_host_name` (0).

### Certificate verification — closed 2026-09-20

The transport used to run with `SSL_VERIFY_NONE`: it did not validate the
server certificate at all. It now verifies by default.

- **Chain:** `SSL_CTX_set_default_verify_paths` (system CA store) +
  `SSL_VERIFY_PEER`. Needs the `ca-certificates` package on the host.
- **Hostname:** `SSL_set1_host`. This part is easy to forget and the reason
  half-done TLS validation is worse than none: `SSL_VERIFY_PEER` alone checks
  that the chain is valid, **not that the certificate was issued for the host
  you dialed**, so a valid certificate for any other domain would sail through.
- **Escape hatch:** `InsecureSkipVerify := True` restores the old behaviour for
  endpoints with a self-signed certificate. It is opt-in and off by default.
- On failure the exception carries the `X509_V` code from
  `SSL_get_verify_result`, so the cause is visible instead of a generic
  handshake error.

Verified on a real Ubuntu 26.04 host against badssl.com, 7/7:

| Host | Expected | X509_V |
|------|----------|--------|
| `api.openai.com`, `www.google.com` | connects | — |
| `self-signed.badssl.com` | rejected | 18 (self signed) |
| `expired.badssl.com` | rejected | 10 (expired) |
| `untrusted-root.badssl.com` | rejected | 19 (self signed in chain) |
| `wrong.host.badssl.com` | rejected | **62 (hostname mismatch)** |
| `self-signed` + `InsecureSkipVerify` | connects | — |

The `wrong.host` row is the one that matters: valid chain, wrong name. It is
what proves the hostname check is wired, and it is exactly the case that passes
when only `SSL_VERIFY_PEER` is set.

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
