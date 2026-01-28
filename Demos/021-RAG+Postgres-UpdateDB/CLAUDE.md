# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is Demo 021 of the **MakerAI 3.x Suite** - a Delphi FMX application demonstrating RAG (Retrieval-Augmented Generation) with PostgreSQL vector database integration. The demo showcases document embedding storage and semantic similarity search.

## Build Commands

The project uses Delphi MSBuild system:

```bash
# Build Debug (Win32)
msbuild RagUpdateDb.dproj /p:Config=Debug /p:Platform=Win32

# Build Release (Win64)
msbuild RagUpdateDb.dproj /p:Config=Release /p:Platform=Win64

# Build from Delphi IDE: Open RagUpdateDb.dproj, select configuration/platform, press F9
```

**Supported Platforms:** Win32, Win64, Android, Android64, Linux64

## Architecture

### Data Flow
```
User Query → Embedding Generation (Ollama) → Vector Search (PostgreSQL pgvector)
→ Semantic Ranking → Context Assembly → LLM Response → UI
```

### Key Components in uRagUpdateDBMain.pas
- `TAiOllamaEmbeddings` - Generates 768-dimension embeddings via Ollama
- `TAiRAGVector` - RAG system for vector operations (AddData, Search)
- `TFDConnection/TFDQuery` - FireDAC PostgreSQL connection
- `TAiOllamaChat` - LLM chat interface for responses

### Database Schema (PostgreSQL + pgvector)
```sql
CREATE TABLE anexos (
  id BIGSERIAL PRIMARY KEY,
  FechaDoc TIMESTAMP,
  Categoria VARCHAR,
  PathDoc VARCHAR,
  FileName VARCHAR,
  Resumen TEXT,
  FileFormat VARCHAR,
  embedding vector(768)
);
```

## MakerAI Framework Structure

The parent framework (`Source/`) provides:

| Directory | Purpose |
|-----------|---------|
| `Chat/` | LLM provider connectors (OpenAI, Claude, Gemini, Ollama, etc.) |
| `Core/` | Base framework classes and utilities |
| `RAG/` | Vector and Graph RAG implementations |
| `Tools/` | Function calling, shell, computer use, audio, image generation |
| `Agents/` | Agent orchestration engine |
| `MCPClient/`, `MCPServer/` | Model Context Protocol support |

### Provider Pattern
All LLM providers inherit from common interfaces:
- `uMakerAi.Chat.AiConnection.pas` - Unified connection interface
- Each provider (OpenAI, Claude, Gemini, Ollama) implements the same chat/embedding contracts

### Event-Driven Processing
Key callbacks used throughout:
- `OnDataVecAddItem` - Embedding insertion complete
- `OnDataVecSearch` - Search results returned
- `ReceiveData` - LLM streaming response

## Dependencies

- **Delphi 12.2+** (Alexandria/Florence)
- **FireDAC** - Database connectivity layer
- **FMX** - FireMonkey cross-platform UI
- **PostgreSQL** with **pgvector** extension enabled
- **Ollama** (local) or other LLM provider for embeddings/chat

## Working with This Codebase

- UI forms use `.pas` (code) + `.fmx` (layout) pairs
- Async operations use `TTask.Run` from System.Threading
- Vector similarity uses pgvector's `<->` cosine distance operator
- Comments and commit messages are in Spanish

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
