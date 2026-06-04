# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

MakerAI MCP Server - A Model Context Protocol implementation for Delphi that exposes AI-callable tools and resources through multiple transport protocols (HTTP, SSE, StdIO, Direct). Part of MakerAI 3.x framework.

## Building

MCPServer is included in the main MakerAI runtime package:

```text
Open: Source/Packages/MakerAI.dpk
Build → Compile
```

**IDE:** RAD Studio (Delphi 11 Alexandria to 13 Florence)

**Required library paths:** `Source/MCPServer`, `Source/Core`, `Source/Tools`, `Source/Utils`

## Testing

No dedicated test framework. Testing via demo projects with HTTP/curl.

**Run demo server:**
```bash
# SSE protocol (default)
AiMCPServerDemo.exe --protocol sse --port 8080 --cors-origins "*"

# HTTP protocol
AiMCPServerDemo.exe --protocol http --port 8080

# Stdio protocol
AiMCPServerDemo.exe --protocol stdio
```

**Test via curl:**
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

```text
TAiMCPLogicServer (JSON-RPC engine, tool/resource registry)
    └── TAiMCPServer (TComponent wrapper)
            ├── TAiMCPHttpServer (stateless HTTP JSON-RPC)
            ├── TAiMCPSSEHttpServer (bidirectional event streaming)
            ├── TAiMCPStdioServer (stdin/stdout for subprocess)
            └── TAiMCPDirectConnection (in-process, zero network)
```

### Key Interfaces

**IAiMCPTool** - Tool contract:
- `GetName`, `GetDescription`, `GetInputSchema`
- `Execute(Arguments: TJSONObject; AuthContext: TAiAuthContext): TJSONObject`

**IAiMCPResource** - Resource contract:
- `GetURI`, `GetName`, `GetDescription`, `GetMimeType`, `Read`

### Units

| Unit | Purpose |
|------|---------|
| `uMakerAi.MCPServer.Core.pas` | Core classes, JSON-RPC engine, tool/resource registry (~1,500 lines) |
| `UMakerAi.MCPServer.Http.pas` | HTTP transport with CORS support |
| `UMakerAi.MCPServer.SSE.pas` | Server-Sent Events transport (session-aware) |
| `UMakerAi.MCPServer.Stdio.pas` | Standard I/O transport |
| `UMakerAi.MCPServer.Direct.pas` | In-process direct invocation |
| `uMakerAi.MCPServer.Bridge.pas` | Adapter bridging TAiFunctions to IAiMCPTool |

### Tool Implementation Pattern

```delphi
type
  TMyParams = class
  private
    FPath: string;
    FRecursive: Boolean;
  public
    [AiMCPSchemaDescription('Directory to process')]
    property Path: string read FPath write FPath;
    [AiMCPOptional]
    [AiMCPSchemaDescription('Include subdirectories')]
    property Recursive: Boolean read FRecursive write FRecursive;
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

- `[AiMCPSchemaDescription('...')]` - Parameter description for JSON Schema
- `[AiMCPOptional]` - Mark parameter as optional
- `[AiMCPSchemaEnum(['a', 'b', 'c'])]` - Enum constraint

### Response Builder

```delphi
Result := TAiMCPResponseBuilder.New
  .AddText('Processing complete')
  .AddFile('/path/to/output.pdf')
  .Build;
```

### Protocol Selection

| Protocol | Use Case |
|----------|----------|
| HTTP | Simple clients, stateless interactions, load balancing |
| SSE | Claude Desktop, MakerAI clients, real-time streaming |
| StdIO | Local subprocess, CLI integration |
| Direct | Embedded integration, testing, same-process calls |

### MCP Protocol Methods

| Method | Purpose |
|--------|---------|
| `initialize` | Server capability handshake |
| `tools/list` | List available tools |
| `tools/call` | Execute tool with arguments |
| `resources/list` | List available resources |
| `resources/read` | Read resource content |

## Demo Projects

| Demo | Location | Description |
|------|----------|-------------|
| 031-MCPServer | `/Demos/031-MCPServer/` | Multi-protocol showcase with FileAccess, SysInfo, WorldTime tools |
| 035-MCPServerWithTAiFunctions | `/Demos/035-MCPServerWithTAiFunctions/` | TAiFunctions bridge integration |
| 036-MCPServerStdIO_AiFunction | `/Demos/036-MCPServerStdIO_AiFunction/` | StdIO + TAiFunctions |

## Known Issues

- MCP SSE Server is experimental (intermittent connectivity)
- Linux compilation requires manual path adjustments
- No built-in authentication (`OnValidateRequest` event is stubbed)

## Documentation

- `/Docs/Version 3/MCPServer/uMakerAi-MCP.Server.EN.pdf` - English documentation
- `/Docs/Version 3/MCPServer/uMakerAi-MCP.Server.ES.pdf` - Spanish documentation

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
