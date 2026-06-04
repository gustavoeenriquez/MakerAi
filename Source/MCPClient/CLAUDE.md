# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

MCPClient implements the Model Context Protocol (MCP) client for MakerAI, enabling Delphi applications to consume external MCP servers through multiple transport protocols. This module follows the MCP Specification (2025-06-18).

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

### MCP Protocol Flow

1. Send `initialize` request with `protocolVersion: "2025-06-18"`
2. Receive server capabilities
3. Send `notifications/initialized` notification
4. Call `tools/list` to retrieve available tools
5. Invoke tools via `tools/call`

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
