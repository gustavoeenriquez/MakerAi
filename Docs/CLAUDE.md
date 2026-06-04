# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Directory Purpose

This is the documentation root for MakerAI v3.2 - an AI orchestration framework for Delphi. **No source code exists here** - all implementation is in `/Source/`.

## Documentation Structure

```text
Docs/
└── Version 3/           ← Current version documentation
    ├── Agents/          ← Agent orchestration (graph-based workflows)
    ├── MCPServer/       ← Model Context Protocol server implementation
    ├── RAG/             ← Retrieval-Augmented Generation (vector + graph)
    └── PDF/             ← Distribution-ready consolidated PDFs
```

## Language

Documentation is primarily in **Spanish**. English versions available for:
- MCP Server (`MCPServer/uMakerAi-MCP.Server.EN.pdf`)

## Subdirectory CLAUDE.md Files

Specialized guidance exists in subdirectories:
- `Version 3/CLAUDE.md` - Full documentation map and source code cross-references
- `Version 3/MCPServer/CLAUDE.md` - MCP protocol patterns, testing commands
- `Version 3/RAG/CLAUDE.md` - pgvector reference, RAG implementation patterns

## Key Documentation → Source Mapping

| Documentation | Source Code |
|---------------|-------------|
| `uMakerAi-ChatConnection.docx` | `Source/Chat/uMakerAi.Chat.AiConnection.pas` |
| `uMakerAi.Chat.docx` | `Source/Core/uMakerAi.Chat.pas` |
| `uMakerAi-Agents.ES.docx` | `Source/Agents/uMakerAi.Agents.pas` |
| `uMakerAi-RAG.ES.docx` | `Source/RAG/uMakerAi.RAG.Vectors.pas` |
| `uMakerAI-RAGGraph.docx` | `Source/RAG/uMakerAi.RAG.Graph.Core.pas` |
| `uMakerAi-MCP.Server.*.docx` | `Source/MCPServer/uMakerAi.MCPServer.Core.pas` |
| `uMakerAi.ToolFuncions.docx` | `Source/Tools/uMakerAi.Tools.Functions.pas` |

## File Formats

- `.docx` - Editable source documents (Word)
- `.pdf` - Distribution versions
- `.xlsx` - Test specification matrix (`Test List.xlsx`)
- `.m4a` - Webinar audio recordings

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for project overview.
