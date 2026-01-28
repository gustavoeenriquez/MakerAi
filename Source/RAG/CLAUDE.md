# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

The RAG (Retrieval-Augmented Generation) module provides two complementary retrieval systems:
- **Vector RAG** (`TAiRAGVector`): Embedding-based semantic search with hybrid BM25 support
- **Graph RAG** (`TAiRagGraph`): Knowledge graph with semantic search over nodes and edges

Both systems share common infrastructure for embeddings, metadata filtering, and database drivers.

## Architecture

### Core Components

```text
uMakerAi.RAG.Vectors.Index.pas   # Base classes: TAiEmbeddingNode, TAIEmbeddingIndex, THNSWIndex, TAIBm25Index
uMakerAi.RAG.MetaData.pas        # TAiEmbeddingMetaData, TAiFilterCriteria (filter tree)
uMakerAi.RAG.Vectors.pas         # TAiRAGVector - main vector store component
uMakerAi.RAG.Vectors.VQL.pas     # VGQL query language (Lexer, Parser, Compiler)
uMakerAi.RAG.Graph.Core.pas      # TAiRagGraph, TAiRagGraphNode, TAiRagGraphEdge
uMakerAi.RAG.Graph.GQL.pas       # MakerGQL query language for graphs
uMakerAi.RAG.Graph.Builder.pas   # Graph construction helpers
```

### Inheritance Hierarchy

```text
TAiEmbeddingNode (base embedding container)
├── TAiRagGraphNode (graph node with edges)
└── TAiRagGraphEdge (graph edge with weight)

TAIEmbeddingIndex (abstract index)
├── TAIBasicEmbeddingIndex (brute force)
├── THNSWIndex (HNSW approximate search)
└── TAIEuclideanDistanceIndex (L2 distance)

TAiVectorStoreDriverBase (abstract DB driver)
└── [PostgreSQL driver in uMakerAi.RAG.Vector.Driver.Postgres]

TAiRagGraphDriverBase (abstract graph DB driver)
└── [PostgreSQL driver in uMakerAi.RAG.Graph.Driver.Postgres]
```

## Key Patterns

### Vector Search Options

`TAiSearchOptions` controls hybrid search behavior:
- `UseEmbeddings`: Enable semantic vector search (cosine similarity)
- `UseBM25`: Enable lexical search (BM25 algorithm)
- `UseRRF`: Reciprocal Rank Fusion vs Weighted Score Fusion
- `UseReorderABC`: Lost-in-the-middle reordering for LLM context

### Filter Criteria System

`TAiFilterCriteria` supports recursive filter trees with operators:
- Basic: `foEqual`, `foNotEqual`, `foGreater`, `foLess`, etc.
- Text: `foContains`, `foLike`, `foILike`, `foStartsWith`, `foEndsWith`
- Lists: `foIn`, `foNotIn`, `foBetween`
- Existence: `foIsNull`, `foIsNotNull`, `foExists`

Logical grouping via `loAnd`/`loOr` with nested `AddGroup()`.

### VGQL Query Language (Vector)

SQL-like DSL for vector searches:
```sql
MATCH documents
SEARCH 'machine learning algorithms'
USING HYBRID WEIGHTS(semantic: 0.7, lexical: 0.3) FUSION RRF
WHERE category = 'tech' AND date > '2024-01-01'
THRESHOLD GLOBAL 0.5
RERANK 'neural networks' WITH REGENERATE
OPTIMIZE REORDER ABC
RETURN TEXT, METADATA, SCORE
LIMIT 10
```

Execution flow: `TVGQLLexer` → `TVGQLParser` → `TVGQLQuery` (AST) → `TVGQLCompiler` → `TVGQLRequest` → `TAiRAGVector.ExecuteVGQL()`

### MakerGQL Query Language (Graph)

Cypher-like DSL for graph traversal:
```cypher
MATCH (p:Persona)-[r:TRABAJA_EN]->(e:Empresa)
WHERE p.ciudad = 'Madrid' AND e.empleados > 100
RETURN p, r, e
DEPTH 2
```

Special commands:
- `SHOW LABELS` / `SHOW EDGES`: Introspection
- `GET SHORTEST PATH`: Dijkstra's algorithm
- `GET CENTRALITY(node)`: Closeness centrality
- `GET DEGREES TOP n`: Hub detection

## Thread Safety

- `TAiRAGVector` uses `TMultiReadExclusiveWriteSynchronizer` for concurrent access
- Search results use `TAiSearchResult` record to avoid modifying original nodes
- Index operations (HNSW) are write-locked during updates

## Component Registration

Both `TAiRAGVector` and `TAiRagGraph` register under the `MakerAI` palette category:
```pascal
RegisterComponents('MakerAI', [TAiRAGVector]);
RegisterComponents('MakerAI', [TAiRagGraph]);
```

## Graph Export Formats

`TAiRagGraph.SaveToFile()` supports:
- `gefDOT`: GraphViz format
- `gefGraphML`: Standard XML format (compatible with Gephi)
- `gefGraphMkai`: Native JSON format with embeddings

## Database Drivers

External persistence requires implementing:
- `TAiVectorStoreDriverBase.Add()`, `Search()`, `Delete()`, `Clear()`
- `TAiRagGraphDriverBase.FindNodeByID()`, `AddNode()`, `AddEdge()`, `SearchNodes()`, etc.

PostgreSQL implementations use pgvector extension for vector similarity.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
