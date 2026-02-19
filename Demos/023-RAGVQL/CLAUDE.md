# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

RagVQL is a Delphi FireMonkey (FMX) desktop application demonstrating advanced RAG (Retrieval-Augmented Generation) capabilities using VQL (Vector Query Language) for semantic and lexical search with PostgreSQL vector database. Part of the MakerAI 3.x AI Orchestration Framework.

## Build Commands

```bash
# Using MSBuild (from command line)
msbuild RagVectorQL.dproj /p:Config=Debug /p:Platform=Win32
msbuild RagVectorQL.dproj /p:Config=Release /p:Platform=Win64

# Using Delphi IDE
# Open RagVectorQL.dproj, select platform, press F9
```

**Output:** `.\$(Platform)\$(Config)\` (e.g., `.\Win64\Debug\`)

**Target Platforms:** Win32, Win64, Android, Android64, Linux64

## Runtime Requirements

- PostgreSQL with pgvector extension
- Ollama running on `localhost:11434` for embeddings and chat

## Architecture

```
RagVectorQL.dpr (Entry Point)
└── TForm21 (uMainRAGVQL.pas)
    ├── TAiOllamaEmbeddings       - Embedding generation (768-dim vectors)
    ├── TAiRAGVector              - Core RAG system (vector operations)
    ├── TAiRAGVectorPostgresDriver - PostgreSQL/pgvector integration
    ├── TFDConnection             - FireDAC database connection
    └── UI Components (Memos, Buttons, CheckBoxes)
```

**Data Flow:** User Query → Embedding Generation (Ollama) → Vector Search (PostgreSQL) → Ranking → Context Assembly → Display

## Key MakerAI Framework Classes

| Class | Unit | Purpose |
|-------|------|---------|
| `TAiOllamaEmbeddings` | `uMakerAi.Embeddings` | Generates embeddings via Ollama |
| `TAiRAGVector` | `uMakerAi.rag.Vectors` | Core RAG vector operations |
| `TAiRAGVectorPostgresDriver` | `uMakerAi.rag.Vector.Driver.Postgres` | PostgreSQL integration |
| `TVGQLParser`, `TVGQLCompiler` | `uMakerAi.RAG.Vectors.VQL` | VQL parsing & compilation |
| `TAiFilterCriteria` | `uMakerAi.rag.Vectors` | Advanced filtering logic |

## Search Features

- **UseEmbeddings:** Semantic similarity search
- **UseBM25:** Lexical keyword search (BM25 algorithm)
- **UseRRF:** Reciprocal Rank Fusion for hybrid results
- **UseReorderABC:** "Lost-in-Middle" context reordering
- **EmbeddingWeight/BM25Weight:** Score weighting in hybrid mode

## Delphi Naming Conventions

- **Classes:** `T` prefix (`TForm21`, `TAiRAGVector`)
- **Units:** `u` prefix (`uMainRAGVQL`)
- **Components:** Prefixes: `Btn*`, `Ch*`, `Lbl*`, `Memo*`, `Edit*`
- **Event handlers:** `[Component][Event]` (`BtnConsultarClick`)
- **Local variables:** `L` prefix (`LPrompt`, `LResponse`)
- **Private fields:** `F` prefix (`FUserCancelled`)

## Threading Pattern

```pascal
TTask.Run(procedure
begin
  // Background work
  LResponse := rag.SearchText(...);

  // UI update on main thread
  TThread.Queue(nil, procedure
  begin
    MemoResponse.Lines.Text := LResponse;
  end);
end);
```

## File Formats

- **.dpr** - Delphi Project Root
- **.dproj** - MSBuild project configuration
- **.pas** - Pascal Unit source
- **.fmx** - FireMonkey form definition (XML)
- **.mkVRag** - MakerAI Vector RAG file format (custom binary)

## Code Language

Comments and UI strings are in Spanish.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
