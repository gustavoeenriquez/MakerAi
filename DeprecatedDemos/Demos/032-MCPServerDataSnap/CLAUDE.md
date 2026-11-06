# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Delphi DataSnap REST server that implements the MCP (Model Context Protocol) for the MakerAi Delphi Suite. It exposes MCP tools via DataSnap REST methods using `TAiMCPDirectConnection` to map REST calls to internal MCP logic.

## Build & Run

- **IDE**: Embarcadero RAD Studio (Delphi)
- **Project file**: `MCPDataSnapServer.dpr`
- **Build**: Open project in RAD Studio and compile (Ctrl+F9) or build (F9)
- **Output**: Console application (APPTYPE CONSOLE)
- **Default port**: 8000

### Console Commands
```
start       - Start the HTTP server
stop        - Stop the server
set port N  - Change port (e.g., "set port 9000")
status      - Show server status
help        - Show available commands
exit        - Stop server and close application
```

## Architecture

### Core Components

```
MCPDataSnapServer.dpr    → Main program, console UI, HTTP bridge (TIdHTTPWebBrokerBridge)
    ↓
uWebModule.pas           → Web module with TDSHTTPWebDispatcher for REST routing
    ↓
uServerContainer.pas     → DataSnap server container (TDSServer, TDSAuthenticationManager)
    ↓
uServerMethods.pas       → TSSE class: DataSnap server methods exposing MCP endpoints
    ↓
uTool.DataSnap.FileAccess.pas → MCP tool implementations with security config
```

### REST Endpoints

Base URL: `http://localhost:8000/datasnap/rest/TSSE`

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/ListTools` | GET | Returns available MCP tools |
| `/CallTool` | POST/GET | Executes a tool (params: ToolName, Args JSON) |
| `/EchoString` | GET | Echo test |
| `/ReverseString` | GET | String reversal test |

### MCP Integration

The `TSSE` data module uses `TAiMCPDirectConnection` (from MakerAi Suite) to:
1. Register MCP tools with factory functions
2. Handle tool listing and execution
3. Manage per-session user context from DataSnap sessions

### Security Model (TFileAccessConfig)

File access tools enforce path and extension restrictions:
- **Allowed paths**: Configured per-user in `TSSE.InitMCP`
- **Allowed extensions**: .txt, .pdf, .json, .xml, .csv, .log, .md, .png, .jpg, .jpeg (configurable)
- Tools: `list_files`, `read_file`, `write_file`

Each tool inherits from `TAiFileToolBase<T>` which wraps `TFileAccessConfig` for security validation.

## Dependencies

External units from MakerAi Suite (not included in this project):
- `uMakerAi.MCPServer.Core` - MCP protocol types, `TAiMCPServer`, `IAiMCPTool`
- `uMakerAi.MCPServer.Direct` - `TAiMCPDirectConnection` for in-process MCP handling

## Key Patterns

- DataSnap uses `{$METHODINFO ON}` to expose public methods as REST endpoints
- Tool parameters use RTTI attributes (`AiMCPSchemaDescription`, `AiMCPOptional`) for JSON schema generation
- Authentication is configured but defaults to `valid := True` (TODO markers in code)

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
