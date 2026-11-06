# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**RagGraphDemo** is a Delphi FMX application demonstrating the MakerAI Suite's Knowledge Graph RAG engine. It combines vector similarity search with graph traversal for enhanced AI-powered information retrieval.

Part of MakerAI 3.x - an AI orchestration framework for Delphi supporting multiple LLM providers (OpenAI, Gemini, Claude, Ollama, etc.).

## Build Commands

```bash
# Command line (requires RAD Studio)
msbuild RagGraphDemo.dproj /p:Config=Debug /p:Platform=Win32

# Or open RagGraphDemo.dproj in Delphi IDE (11 Alexandria to 13 Florence)
```

**Platforms:** Win32, Win64, Android, Android64, Linux64

## Architecture

### Core Components

| Component | Purpose |
|-----------|---------|
| `TAiRagGraph` | Main knowledge graph with in-memory or database storage |
| `TAiRagGraphBuilder` | Converts LLM-generated JSON triplets into graph nodes/edges |
| `TAiRagGraphPostgresDriver` | PostgreSQL persistence using pgvector for embeddings |
| `TAiChatConnection` | LLM connection for text processing and query planning |

### Data Flow

1. **Text Input** → LLM extracts entities/relationships as JSON triplets
2. **Graph Builder** → Parses JSON into nodes (entities) and edges (relationships)
3. **Embeddings** → Generated for semantic search via TAiOllamaEmbeddings
4. **Storage** → In-memory (.mkai) or PostgreSQL with pgvector
5. **Query** → Hybrid search: vector similarity + lexical matching + graph traversal

### Query Modes

- **RAG Search** (`SearchText`): Semantic vector search with depth expansion
- **Match Query** (`Match`): Cypher-like pattern matching via `TGraphMatchQuery`
- **MakerGQL** (`ExecuteMakerGQL`): Custom graph query language
- **LLM Query Planning**: LLM generates `TQueryPlan` JSON for execution

### Database Events Pattern

PostgreSQL mode requires implementing callback events:
- `OnAddNode/OnAddEdge`: INSERT operations
- `OnFindNodeByID/OnFindEdgeByID`: Lazy loading with hydration
- `OnSearchNodes`: Vector similarity with recursive CTE for depth expansion
- `OnGetNodeEdges`: Load connected edges

### Key Source Files

| File | Description |
|------|-------------|
| `uMainRagGraphDemo.pas` | Main form with UI handlers and database callbacks |
| `../../Source/RAG/uMakerAi.RAG.Graph.Core.pas` | TAiRagGraphNode, TAiRagGraphEdge, TQueryPlan |
| `../../Source/RAG/uMakerAi.RAG.Graph.Builder.pas` | JSON triplet parser |
| `../../Source/RAG/uMakerAi.RAG.Graph.GQL.pas` | MakerGQL parser |

## Key Types

```pascal
// LLM-generated graph traversal plan
TQueryPlan = record
  AnchorPrompt: string;      // Initial semantic search prompt
  AnchorVariable: string;    // Starting node variable
  ResultVariable: string;    // Return variable
  Steps: TArray<TQueryStep>; // Traversal steps
end;

// Cypher-like pattern matching
TGraphMatchQuery = class
  MatchClauses: TList<TMatchClause>;
  NodePatternByVariable: TDictionary<string, TMatchNodePattern>;
end;
```

## File Formats

- `.mkai` - Native binary graph format (optional embeddings)
- `.graphml` - Export for Gephi, yEd
- `.dot` - Graphviz format

## Sample Data

`DatosGrafo/` contains a Spanish-language story ("El Misterio de la Biblioteca Olvidada") with extracted entities (Persona, Lugar, Organizacion, Concepto) and relationships (ES_HIJO_DE, TRABAJA_EN, etc.).

## Database Setup

Tables: `graph_nodes`, `graph_edges`
- Requires PostgreSQL with pgvector extension
- Multi-tenant via `entidad` column

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
