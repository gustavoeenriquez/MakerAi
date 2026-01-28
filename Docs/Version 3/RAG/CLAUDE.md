# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Folder Purpose

This folder contains Spanish-language documentation for the MakerAI RAG (Retrieval-Augmented Generation) module. The documentation covers both theoretical concepts and practical implementation with the MakerAi Delphi Suite.

## Documentation Files

- `uMakerAi-RAG.ES.pdf` / `.docx` - Comprehensive RAG manual covering:
  - RAG fundamentals and architecture (indexing, chunking, embeddings, vector stores)
  - Hands-on tutorial using `TAiRAGVector` with PostgreSQL/pgvector
  - Optimization techniques (chunking strategies, query transformation, re-ranking, self-correction)
  - Structured RAG with knowledge graphs vs vector RAG
  - Multimodal RAG considerations
  - Evaluation metrics (Faithfulness, Answer Relevancy, Context Precision/Recall)
  - Production deployment considerations

- `uMakerAi-Agents.ES.docx` - Agent orchestration documentation

## Key MakerAI RAG Components (from documentation)

| Component | Purpose |
|-----------|---------|
| `TAiRAGVector` | Core RAG orchestrator for indexing and search |
| `TAiOllamaEmbeddings` / `TAiOpenAiEmbeddings` | Embedding generation |
| `TAIAgents` | Agent workflow orchestration for advanced RAG patterns |
| `TAiEmbeddingNode` | Vector representation with `CosineSimilarity` method |

## RAG Implementation Pattern (PostgreSQL/pgvector)

```pascal
// Indexing: AddItemsFromPlainText triggers OnDataVecAddItem event
RAGVector.AddItemsFromPlainText(TextoCompleto, 512, 80); // chunk_size, overlap

// Search: SearchText triggers OnDataVecSearch event
ContextoRecuperado := RAGVector.SearchText(Pregunta, 5, 0.0); // limit, precision
```

Events `OnDataVecAddItem` and `OnDataVecSearch` handle database persistence with pgvector's `<->` operator for cosine distance.

## pgvector Quick Reference

```sql
-- Enable extension
CREATE EXTENSION vector;

-- Create table with vector column
CREATE TABLE items (embedding vector(1024));

-- Cosine distance search
SELECT * FROM items ORDER BY embedding <-> :query_embedding LIMIT 5;

-- Create HNSW index (recommended)
CREATE INDEX ON items USING hnsw (embedding vector_cosine_ops);
```

## Related Source Code

The implementation code for these components is in the parent project:
- `Source/RAG/uMakerAi.RAG.Vectors.pas` - Vector RAG implementation
- `Source/RAG/uMakerAi.RAG.Graph.Core.pas` - Knowledge graph RAG
- `Source/Core/uMakerAi.Embeddings.pas` - Embedding generation
- `Source/Agents/uMakerAi.Agents.pas` - Agent orchestration

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for Version 3 documentation index and [../../../CLAUDE.md](../../../CLAUDE.md) for project overview.
