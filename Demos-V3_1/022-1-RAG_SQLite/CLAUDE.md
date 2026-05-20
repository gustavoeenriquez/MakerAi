# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Delphi VCL demo application demonstrating RAG (Retrieval Augmented Generation) using SQLite with the vec0 vector extension for semantic similarity search.

## Build Commands

Open `Sqlite_RAG.dproj` in RAD Studio/Delphi IDE and build with F9 or via MSBuild:
```bash
msbuild Sqlite_RAG.dproj /p:Config=Release /p:Platform=Win64
```

Output: `Win64\Release\Sqlite_RAG.exe` or `Win32\Debug\Sqlite_RAG.exe`

## Architecture

**Single-form application** (`UMain.pas` / `UMain.dfm`):
- Uses FireDAC for SQLite connectivity (`TFDConnection`, `TFDQuery`)
- Integrates MakerAI library components for embeddings (`TAiOpenAiEmbeddings`)
- Model: `text-embedding-3-small` with 1536 dimensions

**Database: `vectores.db`**
- Virtual table `vec_docs` using SQLite vec0 extension
- Schema: `doc_id INTEGER PRIMARY KEY, tipo INTEGER, content TEXT, emb_cos FLOAT[1536], emb_l2 FLOAT[1536]`
- Dual distance metrics: cosine and L2 (Euclidean)

**Runtime Dependencies**
- `vec0.dll` - SQLite vector search extension (must match 32/64-bit architecture)
- `sqlite3.dll` - SQLite library
- Both DLLs must be in same directory as executable

## Key Functions

- `InsertDoc()` - Inserts document with embedding into vec_docs table
- `butSearchClick` - Performs vector similarity search using `MATCH` operator with configurable k-nearest neighbors
- `FDConnectionAfterConnect` - Loads vec0 extension via `SELECT load_extension('vec0.dll')`

## External Dependencies

- **MakerAI library**: Units `uMakerAi.Embeddings.core`, `uMakerAi.Embeddings`, `uMakerAi.Chat.OpenAi`
- Path configured via `$(MAKERAI)` environment variable in project search path

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
