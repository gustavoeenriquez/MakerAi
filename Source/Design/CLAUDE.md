# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Folder Purpose

This folder contains Delphi IDE design-time support: property editors, component editors, and dialogs that enhance the developer experience when using MakerAI components in the Object Inspector.

## Package

All units in this folder belong to `MakerAiDsg.dpk` (design-time package). This package:
- Requires VCL and DesignIDE
- Must be compiled last in the build order (after runtime packages)
- Cannot be included in runtime applications

## Architecture

### Property/Component Editor Registration

Each editor unit contains a `Register` procedure that hooks into the Delphi IDE:

| Unit | Target | Property | Editor Type |
|------|--------|----------|-------------|
| `uAiEditors.ChatConnectionEditor.pas` | `TAiChatConnection` | `DriverName` | Dropdown list of registered drivers |
| `uMakerAi.VersionPropertyEditor.pas` | `TAiChatConnection` | `Version` | Read-only dialog (About) |
| `uMCP.ClientEditorProperties.pas` | `TMCPClientItem` | `Configuration` | Modal dialog editor |

### Key Classes

**`TAiChatFactory`** (`UMakerAi.ParamsRegistry.pas`): Singleton factory for driver management
- `RegisterDriver(AClass)` - Register a chat driver class
- `GetDriverParams(DriverName, ModelName, Params)` - Get hierarchical params (driver defaults → driver overrides → model overrides)
- `RegisterUserParam(DriverName, [ModelName,] ParamName, Value)` - Override default params
- `RegisterCustomModel(DriverName, CustomModelName, BaseModelName)` - Map custom model aliases to base models
- Environment variable expansion: Values starting with `@` are replaced with the corresponding env var

**`TFMCPClientEditor`** (`uMCPClientEditor.pas`): VCL form for MCP client configuration
- Supports StdIO, HTTP, SSE, and MakerAi transport types
- `SetProperties/GetProperties` - Sync UI grid with `Params` and `EnvVars` TStringLists
- Environment variables are prefixed with `Env:` in the properties grid

## Adding New Property Editors

1. Create a new unit following the `u*.pas` naming convention
2. Inherit from `TStringProperty`, `TClassProperty`, or other DesignEditors base class
3. Override `GetAttributes` to define editor behavior (`paValueList`, `paDialog`, `paReadOnly`, etc.)
4. Override `GetValues` (for dropdowns) or `Edit` (for dialogs)
5. Call `RegisterPropertyEditor` in the `Register` procedure
6. Add the unit to `MakerAiDsg.dpk`

## Dependencies

Design units depend on runtime units but not vice versa:
- `uMakerAi.Chat.AiConnection` (for `TAiChatConnection`)
- `uMakerAi.Tools.Functions` (for `TMCPClientItem`)
- `uMakerAi.MCPClient.Core` (for MCP client types)
- `uMakerAi.Core` (for base types)

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
