# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Delphi MCP (Model Context Protocol) Server that communicates via StdIO. It implements a file system tool that can be exposed to AI clients through the JSON-RPC protocol.

## Build Commands

Build using RAD Studio/Delphi IDE or MSBuild:
```bash
msbuild MCPServerFileSystem.dproj /p:Config=Release /p:Platform=Win64
```

Output: `Win64\Release\MCPServerFileSystem.exe` (antes `C:\mcp\servers\` fijo, y el `.dproj` apuntaba a una copia local de `uMakerAi.MCPServer.Bridge.pas` que no existía).

## Usarlo desde Claude Desktop

`%APPDATA%\Claude\claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "makerai-filesystem": {
      "command": "E:\\ruta\\Win64\\Release\\MCPServerFileSystem.exe",
      "args": ["D:\\MiCarpetaSandbox"]
    }
  }
}
```

El argumento es opcional: sin él usa `sandbox\` junto al ejecutable (la crea con un `leeme.txt`). Claude Desktop habla stdio; SSE no sirve para esto.

Prueba sin cliente (una línea JSON por petición; el proceso termina al cerrarse stdin):

```bash
type peticiones.jsonl | Win64\Release\MCPServerFileSystem.exe
```

## Architecture

**Main Components:**

- `MCPServerFileSystem.dpr` - Console application entry point. Creates the DataModule, starts the MCP StdIO server and waits with `WaitForInputClosed` until the client closes stdin (before: `while True`, the process outlived the client).

- `uMCPServerFileSystem_Tool.pas/.dfm` - TDataModule containing:
  - `TAiMCPStdioServer` - MCP server component that handles JSON-RPC communication over StdIO
  - `TAiFunctions` - Collection of AI-callable functions/tools

**MakerAI Framework Dependencies:**
- `uMakerAi.MCPServer.Core` - Core MCP server functionality
- `UMakerAi.MCPServer.Stdio` - StdIO transport implementation
- `uMakerAi.Tools.Functions` - AI function/tool definitions
- `uMakerAi.Core` / `uMakerAi.Chat.Messages` - Core AI types

## Implementing New Tools

1. Add a new function item to `AiFunctions1.Functions` collection in the DFM
2. Set `FunctionName`, `Description`, and `Parameters`
3. Create an `OnAction` event handler in the PAS file
4. Access parameters via `ToolCall.Params.Values['param_name']`
5. Set response via `ToolCall.Response := 'result'`
6. Set `Handled := True` to indicate completion

## Current Tools

- `fs_listar` - Lists the files of the sandbox folder matching a name mask (`*.txt`). The mask cannot contain paths (`..`, `\`, `/`, `:`): that is what keeps the tool inside the sandbox

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
