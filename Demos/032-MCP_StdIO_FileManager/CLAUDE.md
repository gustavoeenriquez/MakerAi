# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Delphi demo project implementing MCP (Model Context Protocol) servers for file management operations. It's part of the MakerAI Suite and demonstrates three transport modes for MCP servers.

## Architecture

### Server Variants

Three separate executables showcasing different MCP transport protocols:

- **McpFileServerStIO.dpr** - Standard I/O transport (stdin/stdout). Primary mode for local AI tool integration (e.g., Claude Desktop).
- **McpFileServerSSE.dpr** - Server-Sent Events over HTTP. Uses port 8080 with `/sse` and `/messages` endpoints.
- **McpFileServerHttp.dpr** - Pure HTTP JSON-RPC. Uses port 8080 with `/mcp` endpoint.

### Core Components

All servers use the MakerAI MCP framework from `../../Source/MCPServer/`:

- `uMakerAi.MCPServer.Core` - Base classes: `TAiMCPServer`, `TAiMCPToolBase<T>`, `IAiMCPTool`, `TAiMCPResponseBuilder`
- `uMakerAi.MCPServer.Stdio` - `TAiMCPStdioServer` for stdin/stdout transport
- `uMakerAi.MCPServer.SSE` - `TAiMCPSSEHttpServer` for SSE transport
- `uMakerAi.MCPServer.Http` - `TAiMCPHttpServer` for HTTP transport

### Tool Implementation Pattern

Tools are implemented by:
1. Defining a parameters class with `[AiMCPSchemaDescription]` attributes
2. Creating a tool class inheriting from `TAiMCPToolBase<TParamsClass>`
3. Implementing `ExecuteWithParams` returning `TJSONObject` via `TAiMCPResponseBuilder`
4. Registering via `ALogicServer.RegisterTool('tool_name', function: IAiMCPTool begin Result := TMyTool.Create; end)`

### Registered Tools

**uTool.Sandbox.pas** - Sandboxed file operations (all paths relative to `SANDBOX_ROOT`):
- `fs_list` - List directory contents
- `fs_read` - Read file content
- `fs_write` - Create/overwrite files
- `fs_delete` - Delete files/directories
- `fs_copy` - Copy files

**uTool.WorldTime.pas**:
- `get_city_time` - Get current time for a city (stub implementation)

### Security

All file operations are sandboxed to `SANDBOX_ROOT` (default: `d:\taller\mcpdir`). The `ResolvePath` function validates paths to prevent directory traversal attacks.

## Build

Open and compile in Delphi IDE using the `.dproj` project files. No external dependencies beyond standard Delphi RTL and the MakerAI framework.

## Claude Desktop Configuration

For StdIO mode (recommended):
```json
{
  "mcpServers": {
    "delphi-filemanager": {
      "command": "path/to/McpFileServerStIO.exe",
      "args": []
    }
  }
}

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
```
