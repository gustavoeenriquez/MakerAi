# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **RagGraphDemo**, a Delphi FMX demonstration application for the MakerAI Suite's RAG Graph functionality. It showcases a knowledge graph engine that combines vector similarity search with graph traversal for enhanced AI-powered information retrieval.

The demo is part of the larger MakerAI 3.x framework - an AI orchestration suite for Delphi supporting multiple LLM providers (OpenAI, Gemini, Claude, Ollama, etc.).

## Build Commands

```bash
# Build from command line using MSBuild (requires RAD Studio installed)
msbuild RagGraphDemo.dproj /p:Config=Debug /p:Platform=Win32

# Or use RAD Studio IDE to open and build:
# Open RagGraphDemo.dproj in Delphi IDE (11 Alexandria to 13 Florence)
```

**Supported Platforms:** Win32, Win64, Android, Android64, Linux64

## Architecture

### Core Components

- **TAiRagGraph** (`RAG`): Main knowledge graph component supporting both in-memory and database-backed storage
- **TAiRagGraphBuilder** (`RAGBuilder1`): Converts LLM-generated JSON triplets into graph nodes and edges
- **TAiRagGraphPostgresDriver** (`RAGPgDriver`): PostgreSQL persistence driver using pgvector for embedding storage
- **TAiChatConnection** (`AiConn`): Connection to LLM providers for text processing and query planning

### Key Source Files

| File | Purpose |
|------|---------|
| `uMainRagGraphDemo.pas` | Main form with all UI event handlers and database event implementations |
| `../../Source/RAG/uMakerAi.RAG.Graph.Core.pas` | Core graph types: TAiRagGraphNode, TAiRagGraphEdge, TQueryPlan |
| `../../Source/RAG/uMakerAi.RAG.Graph.Builder.pas` | JSON triplet parser that populates the graph |
| `../../Source/RAG/uMakerAi.RAG.Graph.Driver.Postgres.pas` | PostgreSQL driver with pgvector support |
| `../../Source/RAG/uMakerAi.RAG.Graph.GQL.pas` | MakerGQL query language parser |

### Data Flow

1. **Text Input** → LLM extracts entities/relationships as JSON triplets
2. **Graph Builder** → Parses JSON into nodes (entities) and edges (relationships)
3. **Embeddings** → Generated for semantic search (uses TAiOllamaEmbeddings or other providers)
4. **Storage** → In-memory (serialized to .mkai) or PostgreSQL with pgvector
5. **Query** → Hybrid search combining vector similarity, lexical matching, and graph traversal

### Query Modes

- **RAG Search** (`SearchText`): Semantic search using embeddings with optional depth expansion
- **Match Query** (`Match`): Cypher-like pattern matching via `TGraphMatchQuery`
- **MakerGQL** (`ExecuteMakerGQL`): Custom graph query language
- **LLM Query Planning**: LLM generates a `TQueryPlan` JSON which is then executed

### Database Events Pattern

When using PostgreSQL driver, the demo implements callback events for database operations:
- `OnAddNode`, `OnAddEdge`: INSERT into graph_nodes/graph_edges tables
- `OnFindNodeByID`, `OnFindEdgeByID`: Lazy loading with hydration
- `OnSearchNodes`: Vector similarity search with recursive CTE for depth expansion
- `OnGetNodeEdges`: Load connected edges for a node

## Sample Data

The `Docs/` folder contains a sample Spanish-language story ("El Misterio de la Biblioteca Olvidada") used to demonstrate entity extraction and relationship mapping. The story includes characters with family relationships, work relationships, and location references - ideal for knowledge graph demonstration.

## Key Types

```pascal
// Query plan for LLM-generated graph traversal
TQueryPlan = record
  AnchorPrompt: string;      // Initial semantic search prompt
  AnchorVariable: string;    // Starting node variable
  ResultVariable: string;    // Return variable
  Steps: TArray<TQueryStep>; // Traversal steps
end;

// Pattern matching query (Cypher-like)
TGraphMatchQuery = class
  MatchClauses: TList<TMatchClause>;
  NodePatternByVariable: TDictionary<string, TMatchNodePattern>;
end;
```

## File Formats

- `.mkai`: Native binary format for graph serialization (includes optional embeddings)
- `.graphml`: Export for tools like Gephi, yEd
- `.dot`: Graphviz format

## Navigation

> See [../../CLAUDE.md](../../CLAUDE.md) for demos overview and [../../../CLAUDE.md](../../../CLAUDE.md) for project overview.
