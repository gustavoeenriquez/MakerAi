# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **MakerAI 3.x**, a comprehensive AI orchestration framework for Delphi developers. It provides unified access to multiple AI providers (OpenAI, Claude, Gemini, local models via Ollama/LM Studio) with support for RAG, Knowledge Graphs, MCP Servers, Agents, and Native Capability Tools.

**Current directory:** Demo 012-ChatAllFunctions - A full-featured chat demo showcasing all framework capabilities.

## Estado (oct 22/2026)

- **El formulario no cargaba** ("Property NativeInputFiles does not exist", luego `Logprobs`): traía propiedades que dejaron de publicarse. Ahora `TAiChat.DefineProperties` las lee sin error (aplica `Top_p`, `Seed`, `N`, `Logprobs`, `Tool_Active`, `ThinkingLevel` ≠ `tlDefault`, `Voice`/`voice_format` → `TtsParams`; descarta el resto). Suite: `chat.legacy-form-props`.
- **Capacidades por el canal tipado:** `ApplyParams` asigna `AiConn.Tool_Active`, `AiConn.ModelConfig.ThinkingLevel`, `AiConn.ModelCaps` y `AiConn.SessionCaps` (antes escribía esas claves en `Params`). El memo de parámetros las muestra arriba.
- **Edits renombrados** según lo que configuran: `EditModelCapsMedia`/`EditModelCapsGen` (ModelCaps: medios de entrada y generación nativa) y `EditSessionCapsMedia`/`EditSessionCapsGen` (SessionCaps; Gap = SessionCaps − ModelCaps activa los bridges). Antes se llamaban como la API eliminada (`EditChatMediaSupports`, `EditNativeInputFiles`, `EditEnabledFeatures`, `EditNativeOutputFiles`).
- **Modelos:** conexión `gpt-6-sol`; `AiOpenChat1` `gpt-6-sol`; `AiGeminiChat1` `gemini-3.8-flash` (2.0 apagado). Voz a texto: `AIWhisper1` con `gpt-transcribe` y `ResponseFormat = json` (whisper-1 se apaga en api.openai.com; el demo extrae `text` del JSON).
- **Endpoints locales:** Ollama `http://localhost:11434/`, GenericLLM `http://localhost:1234/v1/` (antes una IP de la LAN de desarrollo).
- El archivo `uMainChatFull.pas` está en **Windows-1252**: editarlo preservando la codificación.

## Build System

This is a Delphi project using MSBuild. Source files are `.pas` (Pascal units), `.dproj` (project files), `.dpk` (package definitions), and `.fmx` (FireMonkey forms).

### Package Compilation Order
1. `Source/Packages/MakerAI.dpk` - Main runtime package (compile first)
2. `Source/Packages/MakerAi.RAG.Drivers.dpk` - RAG database drivers
3. `Source/Packages/MakerAi.UI.dpk` - FireMonkey UI components
4. `Source/Packages/MakerAiDsg.dpk` - Design-time IDE integration

### Building
- Add all `Source/*` folders to Delphi Library Path
- Open project group `DemosVersion31.groupproj` for full demo access
- Output goes to `.\$(Platform)\$(Config)` (e.g., `.\Win64\Debug`)
- Supports: Win32, Win64, Android, Android64, Linux64, macOS

### Requirements
- Delphi 11 Alexandria through 13 Florence
- Dependencies: RTL, Indy, REST Components, XML/JSON support
- Optional: PostgreSQL client libraries for RAG PostgreSQL drivers

## Architecture

### Central Component: TAiChatConnection
Provider-agnostic facade that manages driver switching, model selection, and message lifecycle. Acts as the main entry point for all AI interactions.

### Core Flow
```
TAiChatConnection → SetDriver/Model → TAiChat (provider-specific)
→ ProcessMessage → SendRequest → HandleResponse → UpdateUI
```

### Key Directories (from repository root)
| Directory | Purpose |
|-----------|---------|
| `Source/Core/` | Base classes: TAiChat, messages, tools framework |
| `Source/Chat/` | Provider implementations (OpenAI, Claude, Gemini, Ollama, etc.) |
| `Source/ChatUI/` | FireMonkey visual components for chat UIs |
| `Source/RAG/` | Vector and graph-based retrieval with VQL/GQL query languages |
| `Source/Agents/` | Autonomous workflow orchestration |
| `Source/Tools/` | ChatTools, native capabilities, media processing |
| `Source/MCPServer/` | Model Context Protocol server (SSE, StdIO, DataSnap) |
| `Source/MCPClient/` | MCP client for consuming external servers |
| `Demos/` | 18 working examples for all major features |

### Major Source Files
- `uMakerAi.Chat.pas` (3099 lines) - Main chat orchestration
- `uMakerAi.Chat.AiConnection.pas` - Provider-agnostic connection
- `uMakerAi.Chat.Gemini.pas` (3937 lines) - Largest provider implementation
- `uMakerAi.RAG.Graph.Core.pas` (4869 lines) - Knowledge graph engine
- `uMakerAi.Agents.pas` (2890 lines) - Agent orchestration

### Design Patterns
- **Facade:** TAiChatConnection abstracts provider complexity
- **Factory:** Provider initialization via TAiChatInitialization
- **Strategy:** Different implementations per AI provider
- **Observer:** Event-driven (OnReceiveData, OnCallToolFunction, etc.)
- **Component:** TComponent-based for Delphi IDE integration

## Supported AI Providers

OpenAI (GPT-6, imagen, Sora, gpt-transcribe, TTS), Google Gemini (3.x, Veo), Anthropic Claude (computer use), DeepSeek, Groq, Ollama, LM Studio, Mistral, Kimi, Grok, Cohere

## Configuration Files

- `MCPConfig.mcpconf` - MCP server connections (JSON format)
- `.dproj.local` - Local IDE settings (not committed)
- Package `.dpk` files define module dependencies

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
