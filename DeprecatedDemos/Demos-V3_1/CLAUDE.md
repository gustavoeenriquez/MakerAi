# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is the Demos directory for the MakerAI 3.x framework. Contains 18+ working example applications demonstrating AI integration patterns for Delphi developers. Each demo has its own CLAUDE.md with specific implementation details.

## Building Demos

**IDE:** Delphi 11 Alexandria through 13 Florence (demos require Delphi 11+; the core framework supports 10.4 Sydney minimum)

**Group project:** Open `DemosVersion31.groupproj` in Delphi IDE to access all demos.

**Individual build (MSBuild):**
```bash
msbuild 010-Minimalchat/MinimalChat.dproj /p:Config=Release /p:Platform=Win64
```

**Build all demos:**
```bash
msbuild DemosVersion31.groupproj /t:Build /p:Config=Release /p:Platform=Win64
```

**Output:** Each project compiles to `./$(Platform)/$(Config)/` (e.g., `./Win64/Release/`)

## Demo Categories

### Chat Integration (01x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 010-Minimalchat | Basic FMX chat | `TAiOllamaChat` direct + `TAiChatConnection` factory |
| 012-ChatAllFunctions | Full-featured chat | Multimodal (images, audio), streaming, tool calls |
| 014-ChatTest | Claude-specific testing | Claude API integration |

### RAG Systems (02x)
| Demo | Purpose | Key Classes |
|------|---------|-------------|
| 021-RAG+Postgres-UpdateDB | Vector RAG with PostgreSQL | `TAiVectorRAG`, pgvector |
| 022-1-RAG_SQLite | Vector RAG with SQLite | Lightweight local storage |
| 023-RAGVQL | Vector Query Language | VQL semantic search syntax |
| 025-RAGGraph | Knowledge graph RAG | `TAiGraphRAG`, entity relationships |
| 026-RAGGraph-Basic | Basic graph RAG | Simplified graph patterns |

### MCP Servers (03x)
| Demo | Purpose | Transport |
|------|---------|-----------|
| 031-MCPServer | Multi-protocol server | SSE, HTTP, StdIO |
| 032-MCP_StdIO_FileManager | File manager MCP | StdIO, HTTP, SSE variants |
| 032-MCPServerDataSnap | DataSnap integration | HTTP with DataSnap |
| 035-MCPServerWithTAiFunctions | MCP + function calling | TAiFunctions integration |
| 036-MCPServerStdIO_AiFunction | Function-based MCP | StdIO with AI functions |

### Video Generation (04x)
| Demo | Purpose | Provider |
|------|---------|----------|
| 041-GeminiVeo | Video generation | Google Veo API |

### Agent Orchestration (05x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 051-AgentDemo | Agent graph workflows | `TAIAgentManager`, visual orchestration |
| 052-AgentConsole | Console agent interface | Command-line agent execution |
| 053-DemoAgentesTools | Agents with tools | Tool integration in agent flows |

### Utilities (09x)
| Demo | Purpose |
|------|---------|
| 090-Varios/ListAllModels | List available models across providers |
| 090-Varios/CompareEmbeddings | Compare embedding quality |

## Common Runtime Dependencies

- **Ollama**: Required for local models (default: `localhost:11434`)
- **API Keys**: OpenAI, Claude, Gemini keys in environment or config
- **PostgreSQL + pgvector**: For 021-RAG demos
- **SQLite**: For 022-RAG demos (included with Delphi)

## Navigation Pattern

Each demo subdirectory contains:
- `*.dproj` - Delphi project file
- `*.dpr` - Program source
- `uMain*.pas` / `uMain*.fmx` - Main form
- `CLAUDE.md` - Demo-specific instructions

## Common Code Patterns Across Demos

**Synchronous AI call:**
```pascal
AiModel.Asynchronous := False;
Response := AiModel.AddMessageAndRun(Prompt, 'user', []);
```

**Streaming with callback:**
```pascal
AiModel.Asynchronous := True;
AiModel.OnReceiveData := procedure(Sender: TObject; Data: string; IsDone: Boolean)
begin
  TThread.Synchronize(nil, procedure begin Memo1.Text := Memo1.Text + Data; end);
end;
AiModel.AddMessageAndRun(Prompt, 'user', []);
```

**Error handling:**
```pascal
AiModel.OnError := procedure(Sender: TObject; ErrorMsg: string; E: Exception; Resp: IHTTPResponse)
begin
  ShowMessage(ErrorMsg);
end;
```

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for project overview, build instructions, and architecture details.
