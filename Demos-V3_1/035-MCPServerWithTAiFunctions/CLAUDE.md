# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **Delphi demo project** that demonstrates how to create an MCP (Model Context Protocol) Server using the **MakerAI Suite** framework. The server exposes TAiFunctions as MCP tools that can be called by AI clients.

## Architecture

The project uses the MakerAI MCP Server framework with three key components:

- **TAiFunctions** (`AiFunctions1`): Component that defines callable functions/tools with parameters, descriptions, and action handlers
- **TAiMCPHttpServer** (`AiMCPHttpServer1`): HTTP-based MCP server that listens on port 3000 with CORS enabled
- **TAiMCPStdioServer** (`AiMCPStdioServer1`): Alternative StdIO-based MCP server (currently commented out)

### Key Files

| File | Purpose |
|------|---------|
| `MCPServerAiFunctions.dpr` | Main console application entry point |
| `uMCPServerAiFunctionsDm.pas` | DataModule containing server components and function handlers |
| `uMCPServerAiFunctionsDm.dfm` | Form file with component configurations |

### MakerAI Framework Dependencies

Located in the parent repository `Source/` folder:
- `uMakerAi.MCPServer.Core` - Core MCP server logic and JSON-RPC handling
- `uMakerAi.MCPServer.Bridge` - Proxy that converts TAiFunctions to IAiMCPTool interface
- `UMakerAi.MCPServer.Http` - HTTP transport using Indy
- `UMakerAi.MCPServer.Stdio` - StdIO transport for command-line integration

## Build Commands

```bash
# Build using Delphi command-line compiler (requires RAD Studio installation)
msbuild MCPServerAiFunctions.dproj /p:Config=Release /p:Platform=Win32

# Or using dcc32/dcc64
dcc64 MCPServerAiFunctions.dpr
```

## Server Configuration

The HTTP server runs on:
- **Port**: 3000 (configurable via `Port` property)
- **Endpoint**: `/mcp`
- **CORS**: Enabled with `*` origin allowed

Settings can be loaded from an INI file (same name as executable with `.ini` extension).

## Adding New Tools

1. Add a new function item to `AiFunctions1.Functions` in the DFM or via code
2. Define `FunctionName`, `Description`, and `Parameters` collection
3. Implement the `OnAction` event handler:
```pascal
procedure TMyDataModule.OnMyFunctionAction(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  // Access parameters via ToolCall.Arguments (JSON string)
  // Set response via ToolCall.Response
  ToolCall.Response := 'Result here';
  Handled := True;
end;
```

## MCP Protocol

The server implements JSON-RPC 2.0 with MCP methods:
- `initialize` - Server capability handshake
- `tools/list` - List available tools
- `tools/call` - Execute a tool with arguments
- `resources/list` / `resources/read` - Resource access (if configured)

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
