# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

MCPClient implements the Model Context Protocol (MCP) client for MakerAI, enabling Delphi applications to consume external MCP servers through multiple transport protocols. This module is **dual-era**: supports the stateless MCP Specification (2026-07-28) with automatic fallback to the legacy handshake spec (2025-06-18).

## Architecture

### Class Hierarchy

```text
TMCPClientCustom (Abstract Base)
├── TMCPClientStdIo   - Local subprocess via stdin/stdout pipes
├── TMCPClientHttp    - Remote server via HTTP POST JSON-RPC
├── TMCPClientSSE     - Bidirectional Server-Sent Events streaming
└── TMCPClientMakerAi - DataSnap-wrapped REST API variant
```

### Transport Types

| Class | Use Case | Key Component |
|-------|----------|---------------|
| `TMCPClientStdIo` | Local MCP server (npx, node) | `TInteractiveProcessInfo` subprocess + read thread |
| `TMCPClientHttp` | Remote HTTP endpoints | `TNetHTTPClient` with retry logic |
| `TMCPClientSSE` | Real-time streaming | Async HTTP + `TThreadedQueue` message buffer |
| `TMCPClientMakerAi` | DataSnap servers | Unwraps nested JSON-RPC from DataSnap array |

### MCP Protocol Flow (dual-era)

El cliente sondea la era del servidor con `server/discover` (StdIO y HTTP):

**Servidor moderno (spec 2026-07-28, stateless):**
1. `server/discover` responde → `NegotiatedProtocol = '2026-07-28'`
2. Sin handshake ni `notifications/initialized`; cada request lleva `_meta`
   (`io.modelcontextprotocol/protocolVersion`, `clientInfo`, `clientCapabilities`)
   via `AttachModernMeta`. En HTTP se agregan los headers `MCP-Protocol-Version`,
   `Mcp-Method` y `Mcp-Name`.
3. `tools/list` / `tools/call` directos.

**Servidor legacy (fallback automatico):**
1. `server/discover` devuelve error (-32601) o timeout → `NegotiatedProtocol = 'legacy'`
2. Send `initialize` request (StdIO: `2024-11-05`, HTTP: `2025-06-18`)
3. Send `notifications/initialized` notification
4. `tools/list` / `tools/call` como siempre

Notas:
- `TMCPClientSSE` queda **legacy a proposito** (el transporte HTTP+SSE esta
  deprecado en la spec 2026-07-28).
- Un error del rango MCP reservado (-32020..-32099) identifica servidor
  moderno (`IsModernErrorCode`) — la sonda no debe caer a legacy ante el.
- `ReadProcessOutput` (StdIO) rescata JSON-RPC embebido en lineas con ruido
  (`banner{"jsonrpc"...}`) de servidores que ensucian stdout.
- Claves `_meta` llevan puntos: leerlas con `GetValue(nombre)` exacto, nunca
  variantes por path (`GetValue<T>`).

## Key Patterns

**Thread-Safe Messaging**: StdIo and SSE use `TThreadedQueue<TJSONObject>` for async response handling.

**Media Extraction**: `ProcessAndExtractMedia()` extracts base64-encoded binary content from tool responses into `TAiMediaFile` objects.

**Default Parameters**:
```text
Command=npx
Arguments=@
RootDir=<home-path>
Timeout=15000
URL=http://localhost:3001/sse
```

## Integration Points

- **TAiFunctions**: MCP clients registered via `MCPClients` collection property
- **Design-Time Editor**: `TFMCPClientEditor` in `Source/Design/uMCPClientEditor.pas`
- **Package**: Included in `Source/Packages/MakerAI.dpk`

## Dependencies

- `uMakerAi.Utils.System` - Subprocess management (`TUtilsSystem.StartInteractiveProcess`)
- `uMakerAi.Core` - Core types (`TAiMediaFile`, `TToolTransportType`)
- `uJSONHelper` - JSON parsing utilities

## Known Limitations

- SSE transport is experimental (intermittent connectivity per parent CLAUDE.md)
- `FBusy` flag prevents concurrent operations on same client
- Windows-focused subprocess management (`{$IFDEF MSWINDOWS}`)

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
