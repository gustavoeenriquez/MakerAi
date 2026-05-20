# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

BasicRAGGraph is a Delphi VCL demo showcasing MakerAI's graph-based RAG (Retrieval-Augmented Generation) capabilities. It demonstrates knowledge graph operations with semantic search, structural pattern matching, and hybrid queries.

## Build & Run

**IDE**: Delphi 11 Alexandria or newer

**Build Steps**:
1. Open `BasicRAGGraph.dproj`
2. Ensure MakerAI `/Source/*` directories are in Library Path
3. Compile packages in order: `MakerAI.dpk` → `MakerAi.RAG.Drivers.dpk` → `MakerAiDsg.dpk` → `MakerAi.UI.dpk`
4. Build and run

**Runtime Requirements**:
- Ollama running locally at `http://localhost:11434/`
- Model: `mxbai-embed-large:latest` (1024 dimensions)

## Architecture

### Key Components (VCL)
- `RAG: TAiRagGraph` - Main graph database with nodes, edges, and query capabilities
- `AiOllamaEmbeddings1: TAiOllamaEmbeddings` - Local embedding provider
- `GraphBuilder: TAiRagGraphBuilder` - JSON-to-graph converter with merge strategies

### Graph Data Model
- **Nodes** (`TAiRagGraphNode`): Entities with labels, properties, and vector embeddings
- **Edges** (`TAiRagGraphEdge`): Labeled relationships with optional properties
- **Triplets**: Subject → Predicate → Object patterns

### Search Modes
1. **Semantic**: Vector-based similarity search using `RAG.Search(prompt, TopK)`
2. **Structural**: Pattern matching with `TGraphMatchQuery` (MATCH-style clauses)
3. **Hybrid**: Combined vector + graph traversal via `TQueryPlan`

## Code Locations

| File | Purpose |
|------|---------|
| `uMainBasicRagGraph.pas` | Main form with all demo operations |
| `../../Source/RAG/uMakerAi.RAG.Graph.Core.pas` | Graph engine, nodes, edges, query execution |
| `../../Source/RAG/uMakerAi.RAG.Graph.Builder.pas` | JSON import with embedding generation |
| `../../Source/RAG/uMakerAi.RAG.Graph.GQL.pas` | Graph query language parser |

## Common Operations

**Add nodes manually**:
```pascal
Node := RAG.AddNode('Label', 'description text');
Node.Properties.AddPair('key', 'value');
Node.SetEmbeddings(EmbeddingsProvider.CreateEmbedding(Node.Description));
```

**Import from JSON**:
```pascal
GraphBuilder.ImportFromJson(JsonString, RAG, TMergeStrategy.msAddNewOnly);
```

**Semantic search**:
```pascal
Results := RAG.Search('query text', 10); // Top 10 similar nodes
```

**Pattern matching**:
```pascal
Query := TGraphMatchQuery.Create;
Query.AddNodePattern('person', 'Person', nil);
Query.AddEdgePattern('person', 'WORKS_AT', 'company', nil);
Results := RAG.MatchPattern(Query);
```

## Conventions

- Classes use `T` prefix (e.g., `TAiRagGraph`)
- Framework components use `TAi` prefix
- Merge strategies: `msAddNewOnly`, `msOverwrite`, `msKeepExisting`
- Export formats: `gefDOT` (Graphviz), `gefGraphML` (XML), `gefGraphMkai` (native)

## Related Resources

- Main framework: `/Source/` directory
- JSON structure examples: `/Source/RAG/RagGraph_Prompt_Example.md`
- Other demos: `/Demos/025-RAGGraph/` (advanced), `/Demos/022-1-RAG_SQLite/` (vector-only)

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
