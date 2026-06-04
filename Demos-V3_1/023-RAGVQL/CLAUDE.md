# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

RagVQL is a Delphi FireMonkey (FMX) desktop application demonstrating advanced RAG (Retrieval-Augmented Generation) using **VQL (Vector Query Language)** — a SQL-like DSL for semantic and lexical search against a PostgreSQL vector database (pgvector) or in-memory store. Part of the MakerAI 3.x framework.

**Active project file:** `RagVectorQL.dproj` / `RagVectorQL.dpr` (entry form: `TForm21` in `uMainRAGVQL.pas`).
Note: `RagVectorGQL.dproj` also exists (historical graph variant).

## Build Commands

```bash
msbuild RagVectorQL.dproj /p:Config=Debug /p:Platform=Win32
msbuild RagVectorQL.dproj /p:Config=Release /p:Platform=Win64
```

**Output:** `.\$(Platform)\$(Config)\`
**Target Platforms:** Win32, Win64, Android, Android64, Linux64

## Runtime Requirements

- PostgreSQL with `pgvector` extension — call `PgDriver.CreateSchema(TableName, 1024)` (via `BtnCreateTable`) to initialize the table before first use
- Ollama at `localhost:11434` — provides both the embedding model (768-dim) and optionally the chat model

## Architecture

```
RagVectorQL.dpr
└── TForm21 (uMainRAGVQL.pas)
    ├── AiOllamaEmbeddings1: TAiOllamaEmbeddings  — embedding generation
    ├── rag: TAiRAGVector                          — in-memory vector store + search engine
    ├── PgDriver: TAiRAGVectorPostgresDriver       — optional PostgreSQL/pgvector backend
    ├── DbConn: TFDConnection                     — FireDAC PostgreSQL connection
    └── UI: MemoPrompt / MemoResponse / checkboxes / progress
```

**Dual-mode storage:**
- **In-memory**: `rag` holds all nodes in RAM. Persist with `rag.SaveToFile()` / `rag.LoadFromFile()` (`.mkVRag` format).
- **PostgreSQL**: `PgDriver` attached to `rag` delegates storage and vector search to the database.

**Data flow:**
Upload text → chunk via `rag.AddItemsFromPlainText(text, metadata, chunkSize=200, overlap=15)` → embed each chunk → store node → `rag.BuildIndex()` required afterward for HNSW search; `rag.BuildLexicalIndex()` required for BM25.

## Search Paths

### 1. Simple search (`BtnConsultar`)
```pascal
rag.SearchText(prompt, limit, precision, filter, showProperties)
```
Runs a single-pass vector search with current `rag.SearchOptions` settings. Does **not** execute RERANK or OPTIMIZE clauses.

### 2. VQL search (`BtnBuscarVGQL`)
```pascal
rag.ExecuteVGQL(vqlQueryString)
```
Full pipeline: Lexer → Parser → AST → Compiler → `TVGQLRequest` → Search → optional RERANK → optional OPTIMIZE.

### 3. VQL debug (`BtnVGqlToText`)
Parses the VQL query and renders the compiled `TVGQLRequest` as a human-readable debug report — useful for verifying query semantics without executing a search.

## VQL Syntax Reference

```sql
MATCH <entity>            -- target entity / collection name (required)
SEARCH '<query text>'     -- semantic query (required)
USING HYBRID              -- search mode: HYBRID | EMBEDDINGS | BM25
  WEIGHTS(semantic: 0.7, lexical: 0.3)
  FUSION RRF              -- fusion: RRF | WEIGHTED
WHERE <metadata_field> = 'value'
  AND date > '2024-01-01'
  AND category IN ('tech', 'science')
  AND score BETWEEN 0.5 AND 1.0
THRESHOLD GLOBAL 0.5      -- min score: GLOBAL | SEMANTIC | LEXICAL
RERANK '<rerank query>' WITH REGENERATE
OPTIMIZE REORDER ABC      -- Lost-in-Middle reordering
RETURN TEXT, METADATA, SCORE
LIMIT 10
```

**WHERE operators:** `=`, `<>`, `>`, `>=`, `<`, `<=`, `CONTAINS`, `STARTS_WITH`, `ENDS_WITH`, `LIKE`, `ILIKE`, `IN`, `NOT IN`, `BETWEEN`, `IS NULL`, `IS NOT NULL`, `EXISTS`
**Logical:** `AND`, `OR` (with implicit grouping via parentheses)

Errors raise `EVGQLParserError` (lexer/parse) or `EVGQLTranslationError` (compile).

## Key MakerAI Framework Classes

| Class | Unit | Purpose |
|-------|------|---------|
| `TAiOllamaEmbeddings` | `uMakerAi.Embeddings` | Generates embeddings via Ollama |
| `TAiRAGVector` | `uMakerAi.rag.Vectors` | In-memory vector store + search engine |
| `TAiRAGVectorPostgresDriver` | `uMakerAi.rag.Vector.Driver.Postgres` | pgvector backend driver |
| `TAiSearchOptions` | `uMakerAi.rag.Vectors` | Search mode flags (UseEmbeddings, UseBM25, UseRRF, UseReorderABC, weights) |
| `TAiEmbeddingMetaData` | `uMakerAi.RAG.MetaData` | Key-value metadata attached to each node |
| `TAiEmbeddingNode` | `uMakerAi.RAG.Vectors.Index` | Individual vector chunk (Tag, Text, MetaData, embedding) |
| `TAiFilterCriteria` | `uMakerAi.rag.Vectors` | Recursive filter tree for `SearchText()` |
| `TVGQLParser` | `uMakerAi.RAG.Vectors.VQL` | Parses VQL text → `TVGQLQuery` AST |
| `TVGQLCompiler` | `uMakerAi.RAG.Vectors.VQL` | Compiles AST → `TVGQLRequest` |

## Threading Pattern

All heavy operations run in `TTask.Run()`. UI-safe state is captured **before** the task (thread-safety critical):

```pascal
// Capture UI state on main thread
LPrompt := MemoPrompt.Lines.Text.Trim;
LUseBM25 := ChLexicalSearch.IsChecked;

TTask.Run(procedure
begin
  LResponse := rag.SearchText(LPrompt, ...);
  TThread.Queue(nil, procedure
  begin
    MemoResponse.Lines.Text := LResponse;
  end);
end);
```

Progress callbacks from `rag.AddItemsFromPlainText()` use `TThread.Queue(nil, ...)` to update `ProgressBar1` and check `FUserCancelled`.

## File Formats

- `.mkVRag` — MakerAI Vector RAG binary (via `rag.SaveToFile` / `rag.LoadFromFile`)

## Code Language

Comments and UI strings are in Spanish.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
> See [../../Source/RAG/CLAUDE.md](../../Source/RAG/CLAUDE.md) for RAG module internals.
