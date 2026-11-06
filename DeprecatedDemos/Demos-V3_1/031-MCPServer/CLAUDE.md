# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MakerAI MCP Server Demo - A Delphi console application demonstrating a multi-protocol MCP (Model Context Protocol) server. Part of MakerAI 3.x framework for AI integration in Delphi applications.

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria to 13 Florence)

Open `AiMCPServerDemo.dproj` in RAD Studio and build for Win64/Release.

**Command-line execution:**
```bash
# SSE protocol (default, for Claude Desktop/MakerAI clients)
AiMCPServerDemo.exe --protocol sse --port 8080 --cors-origins "*"

# Standard HTTP JSON-RPC
AiMCPServerDemo.exe --protocol http --port 8080

# Stdio (for subprocess/local integration)
AiMCPServerDemo.exe --protocol stdio
```

## Architecture

### Three-Protocol Design

The server supports three transport protocols via factory pattern instantiation:

1. **SSE (Server-Sent Events)** - Long-polling event stream at `/sse`, commands via POST `/messages`
2. **HTTP** - Stateless JSON-RPC at POST `/mcp`
3. **Stdio** - Direct stdin/stdout JSON-RPC communication

### Core Classes (in /Source/MCPServer/)

- `TAiMCPServer` - Abstract base server class
- `TAiMCPLogicServer` - Tool/resource management
- `TAiMCPToolBase<T>` - Generic base for implementing tools
- `TAiMCPResponseBuilder` - Fluent response construction

### Tool Implementation Pattern

```delphi
type
  TMyParams = class
    [AiMCPSchemaDescription('Parameter description')]
    property Param: string read FParam write FParam;
  end;

  TMyTool = class(TAiMCPToolBase<TMyParams>)
  protected
    function ExecuteWithParams(const AParams: TMyParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// Registration
AServer.RegisterTool('my_tool', function: IAiMCPTool begin Result := TMyTool.Create; end);
```

### Demo Tools

| File | Tool | Purpose |
|------|------|---------|
| `uTool.FileAccess.pas` | list_files, read_file, write_file | File operations with path/extension whitelisting |
| `uTool.SysInfo.pas` | system_info | System metrics (memory, disk, network) |
| `uTool.WorldTime.pas` | get_city_time | Timezone lookups (placeholder for external API) |

## Key Source Locations

- Main entry: `AiMCPServerDemo.dpr`
- Framework: `/Source/MCPServer/uMakerAi.MCPServer.Core.pas`
- SSE transport: `/Source/MCPServer/UMakerAi.MCPServer.SSE.pas`
- HTTP transport: `/Source/MCPServer/UMakerAi.MCPServer.Http.pas`
- Stdio transport: `/Source/MCPServer/UMakerAi.MCPServer.Stdio.pas`

## Testing Tools via HTTP

```bash
# List available tools
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'

# Call a tool
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"system_info","arguments":{"type":"basic"}},"id":2}'
```

## Security Notes

File access tools implement:
- Path whitelist (temp dir, documents, app directory by default)
- Extension whitelist (.txt, .json, .xml, .csv, .log, .md, .png, .jpg, .pdf, .docx, .xlsx, .mp3, .wav)
- 10 MB file size limit for reads

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
