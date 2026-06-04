# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is the **data folder** for the RAGGraph Demo (`025-RAGGraph`), which is part of the MakerAI Suite - an AI orchestration framework for Delphi. This folder contains sample data files used to demonstrate the Knowledge Graph and RAG (Retrieval-Augmented Generation) capabilities.

## Folder Contents

- **CuentoBibliotecaria.mkai** - Serialized MakerAI graph file with embeddings data
- **CuentoBibliotecaria.graphml** - GraphML export for use with graph visualization tools (Gephi, yEd, Graphviz)
- **Texto Original El Misterio de la Biblioteca Olvidada.txt** - Source text (Spanish story) used to generate the knowledge graph

## Parent Demo Application

The main application is in the parent folder (`025-RAGGraph/`):

- **RagGraphDemo.dpr** - Main project file (Delphi FMX application)
- **uMainRagGraphDemo.pas** - Main form unit with all demo functionality

### Key Components Used

```pascal
TAiRagGraph              // Main knowledge graph component
TAiRagGraphBuilder       // Builds graphs from JSON/LLM output
TAiRagGraphPostgresDriver // PostgreSQL persistence driver
TAiChatConnection        // LLM connection for entity extraction
TAiPrompts               // Template prompts for LLM interactions
```

## File Formats

### .mkai (MakerAI Graph)
Native binary format that can include or exclude embeddings. Load/save via:
```pascal
RAG.LoadFromFile('graph.mkai');
RAG.SaveToFile('graph.mkai', IncludeEmbeddings);
```

### .graphml
Standard XML graph format for interoperability. Contains nodes with labels (Persona, Lugar, Organización, Concepto, Producto) and labeled edges representing relationships (ES_HIJO_DE, TRABAJA_EN, etc.).

## Demo Workflow

1. **Load text** - Import source text from `.txt` file
2. **Process with LLM** - Use `CreaJSonFromTexto` prompt template to extract entities and relationships
3. **Build graph** - `RAGBuilder1.Process()` parses JSON and creates nodes/edges
4. **Search** - Semantic search via `RAG.Search()` or pattern matching via `RAG.Match()`
5. **Query with LLM** - Use `CreaJSonQueryPlan` to generate graph queries from natural language
6. **Export** - Save to `.mkai`, `.graphml`, `.dot`, or `.json`

## Database Mode

The demo supports both in-memory and PostgreSQL persistence:
- Tables: `graph_nodes`, `graph_edges`
- Requires pgvector extension for embedding similarity search
- Entity isolation via `entidad` column (multi-tenant)

## Navigation

> See [../../CLAUDE.md](../../CLAUDE.md) for demos overview and [../../../CLAUDE.md](../../../CLAUDE.md) for project overview.
