# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Delphi MCP (Model Context Protocol) Server that communicates via StdIO. It implements a file system tool that can be exposed to AI clients through the JSON-RPC protocol.

## Build Commands

Build using RAD Studio/Delphi IDE or MSBuild:
```bash
msbuild MCPServerFileSystem.dproj /p:Config=Debug /p:Platform=Win32
```

Output directory: `C:\mcp\servers\`

## Architecture

**Main Components:**

- `MCPServerFileSystem.dpr` - Console application entry point. Creates the DataModule and starts the MCP StdIO server in an infinite loop.

- `uMCPServerFileSystem_Tool.pas/.dfm` - TDataModule containing:
  - `TAiMCPStdioServer` - MCP server component that handles JSON-RPC communication over StdIO
  - `TAiFunctions` - Collection of AI-callable functions/tools

**MakerAI Framework Dependencies:**
- `uMakerAi.MCPServer.Core` - Core MCP server functionality
- `uMakerAi.MCPServer.Bridge` - Bridge for MCP communication
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

- `fs_listar` - Lists files in a sandbox directory with optional filter parameter

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
