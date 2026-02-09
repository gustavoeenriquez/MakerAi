# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this directory.

## Directory Purpose

`Source/Compatibility/` is the **cross-platform compatibility layer** for MakerAI. It provides unified APIs that abstract differences between **Delphi** (10.2–13) and **Free Pascal (FPC 3.3)**, enabling the entire framework to compile on both compilers without `{$IFDEF}` guards scattered throughout the codebase.

## Architecture Pattern

Each unit follows the same pattern:
- **FPC block** (`{$IFDEF FPC}`): Implements classes/helpers that replicate Delphi's native APIs (e.g., `TTask`, `TFile`, `TNetHTTPClient`)
- **Delphi block** (`{$ELSE}`): Provides polyfills for features missing in older Delphi versions or exports type aliases for uniform naming

All units include `{$include ../CompilerDirectives.inc}` for consistent compiler settings.

## Units Reference

| Unit | Purpose | Key Types |
|------|---------|-----------|
| `uSysUtilsHelper.pas` | String, file, path, and array helpers | `TStringHelper`, `TFile`, `TPath`, `TStreamReader`, `TStreamWriter`, `TArrayUtils` (FPC); `TStringSplitOptions`, polyfills (Delphi <11) |
| `uJsonHelper.pas` | JSON compatibility layer | `TJSONObjectHelper`, `TJSONArrayHelper`, `TJSONValueHelper`, `TJsonTextWriter` (FPC); `AddPair`/`TryGetValue` overloads (Delphi <11) |
| `uHttpHelper.pas` | HTTP client abstraction | `TNetHTTPClient`, `IHTTPResponse`, `TMultipartFormData` (FPC); `ConfigureForAsync`, `SetSendTimeout` helpers (Delphi) |
| `uThreadingHelper.pas` | Threading and async tasks | `TTask`, `ITask`, `TThreadPoolManager`, `TThreadedQueue<T>`, `TInterlocked` (FPC); `QueueInMainThread`, `SynchronizeInMainThread` |
| `uEnvHelper.pas` | Environment variable loading | `TEnvHelper.LoadEnv()` - Loads `Config/.env` file as process environment variables |
| `uBase64Helper.pas` | Base64 encoding/decoding | Cross-compiler Base64 operations |
| `uGenericsHelper.pas` | Generics compatibility | Bridges differences in generics between Delphi and FPC |
| `uRttiHelper.pas` | RTTI abstraction | Cross-compiler RTTI access and manipulation |
| `uRegularExpressionsHelper.pas` | Regex compatibility | Unified regex API for both compilers |
| `uSSEHelper.pas` | Server-Sent Events parsing | SSE stream parsing for MCP transport |
| `uExpressionHelper.pas` | Expression evaluation | Used by agent graph `lmExpression` link mode |
| `uPosixHelper.pas` | POSIX system calls | Unix/Linux specific helpers (FPC) |
| `uXMLHelper.pas` | XML parsing compatibility | Cross-compiler XML operations |

## Key Design Decisions

- **Helpers over inheritance**: Uses `class helper` extensively to augment existing types (e.g., `TJSONObjectHelper`, `TStringHelper`) rather than creating wrapper classes.
- **Polyfill strategy**: Features added in newer Delphi versions (e.g., `TMultipartFormData.AddStream` with `AOwnsStream` in Delphi 12+) are backfilled via helpers for older versions.
- **Deprecated alias**: `TAiEnvLoader` is deprecated in favor of `TEnvHelper`.

## Dependencies

This layer has **no dependencies on other MakerAI modules**. It is the foundational layer that all other Source directories depend on.

## Modification Guidelines

- When adding a new helper, provide implementations for **both** FPC and Delphi paths
- Test changes on FPC 3.3 and at least Delphi 11+ to verify compatibility
- Keep conditional compilation blocks clean: `{$IFDEF FPC}` ... `{$ELSE}` ... `{$ENDIF}`
- Use `CompilerVersion` checks for Delphi version-specific polyfills (e.g., `{$IF CompilerVersion < 35.0}`)
