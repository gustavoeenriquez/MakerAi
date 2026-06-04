# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This directory contains documentation for the **MakerAI MCP Server** module - a Model Context Protocol implementation for Delphi that allows applications to expose AI-callable tools and resources through standardized protocols (HTTP, SSE, StdIO, Direct).

## Source Code Location

The MCPServer source code is located in `/Source/MCPServer/` (6 units, ~2,900 lines):

| Unit | Purpose |
|------|---------|
| `uMakerAi.MCPServer.Core.pas` | Core classes, interfaces, JSON-RPC engine, tool/resource registry |
| `UMakerAi.MCPServer.Http.pas` | Stateless HTTP JSON-RPC transport |
| `UMakerAi.MCPServer.SSE.pas` | Server-Sent Events transport (bidirectional, session-aware) |
| `UMakerAi.MCPServer.Stdio.pas` | Standard I/O transport for subprocess integration |
| `UMakerAi.MCPServer.Direct.pas` | In-process direct invocation (zero network overhead) |
| `uMakerAi.MCPServer.Bridge.pas` | Adapter bridging TAiFunctions to IAiMCPTool interface |

## Building

MCPServer is included in the main MakerAI runtime package. Compile from Delphi IDE:

```
Open: Source/Packages/MakerAI.dpk
Build → Compile
```

Required library paths: `Source/MCPServer`, `Source/Core`, `Source/Tools`, `Source/Utils`

## Demo Projects

Located in `/Demos/`:

| Demo | Description | Command |
|------|-------------|---------|
| `031-MCPServer` | Multi-protocol showcase | `AiMCPServerDemo.exe --protocol {sse\|http\|stdio} --port 8080` |
| `032-MCPServerDataSnap` | DataSnap integration | |
| `035-MCPServerWithTAiFunctions` | TAiFunctions integration | HTTP on port 3000 |
| `036-MCPServerStdIO_AiFunction` | StdIO + functions | |

## Testing the Server

```bash
# List tools
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'

# Call a tool
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"system_info","arguments":{"type":"basic"}},"id":2}'
```

## Architecture

### Class Hierarchy

```
TAiMCPLogicServer (JSON-RPC engine, tool/resource registry)
    └── TAiMCPServer (TComponent wrapper)
            ├── TAiMCPHttpServer
            ├── TAiMCPSSEHttpServer
            ├── TAiMCPStdioServer
            └── TAiMCPDirectConnection
```

### Key Interfaces

- `IAiMCPTool` - Tool contract: `GetName`, `GetDescription`, `GetInputSchema`, `Execute`
- `IAiMCPResource` - Resource contract: `GetURI`, `GetName`, `Read`

### Tool Implementation Pattern

```delphi
type
  TMyParams = class
    [AiMCPSchemaDescription('File path to process')]
    property Path: string read FPath write FPath;
  end;

  TMyTool = class(TAiMCPToolBase<TMyParams>)
  protected
    function ExecuteWithParams(const AParams: TMyParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// Registration
Server.RegisterTool('my_tool',
  function: IAiMCPTool begin Result := TMyTool.Create; end);
```

### Schema Attributes

- `[AiMCPSchemaDescription('...')]` - Parameter description
- `[AiMCPOptional]` - Mark parameter as optional
- `[AiMCPSchemaEnum(['a', 'b'])]` - Enum constraint

### MCP Protocol Methods

| Method | Purpose |
|--------|---------|
| `initialize` | Server capability handshake |
| `tools/list` | List available tools |
| `tools/call` | Execute tool with arguments |
| `resources/list` | List available resources |
| `resources/read` | Read resource content |

## Protocol Selection Guide

- **HTTP**: Simple clients, stateless interactions, load balancing
- **SSE**: Claude Desktop, MakerAI clients, real-time streaming
- **StdIO**: Local subprocess, CLI integration
- **Direct**: Embedded integration, testing, same-process calls

## Documentation Files

- `uMakerAi-MCP.Server.EN.pdf` - English technical documentation
- `uMakerAi-MCP.Server.ES.pdf` - Spanish technical documentation

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for Version 3 documentation index and [../../../CLAUDE.md](../../../CLAUDE.md) for project overview.
