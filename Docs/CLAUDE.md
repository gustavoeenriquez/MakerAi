# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Directory Purpose

This is the documentation root for MakerAI v3.2 - an AI orchestration framework for Delphi. **No source code exists here** - all implementation is in `/Source/`.

## Documentation Structure

```text
Docs/
├── Version 3/           ← Current version documentation
│   ├── Agents/          ← Agent orchestration (graph-based workflows)
│   ├── MCPServer/       ← Model Context Protocol server implementation
│   ├── RAG/             ← Retrieval-Augmented Generation (vector + graph)
│   └── PDF/             ← Distribution-ready consolidated PDFs
└── Reportes/            ← Informes de campo desde consumidores del framework
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
| `uMakerAi-AudioBridge.md` | `Source/Utils/uMakerAi.Utils.AudioCapture.pas`, `Source/Utils/uMakerAi.Utils.AudioPlayback.pas` |

## Reportes de campo

`Reportes/` recoge informes escritos desde aplicaciones que consumen el
framework en produccion. No son documentacion de uso: son hallazgos medidos,
con su evidencia y como reproducirlos. Utiles porque cazan fallos que el
compilador no ve y que los demos no ejercitan.

| Informe | Origen | Estado |
|---------|--------|--------|
| `REPORTE_MKAISERVER_2026-08-29.md` | MKAIServer (broker del SaaS) | Atendido en 36b4379 y aa77522, salvo la verificacion runtime del usage de DeepSeek |

## File Formats

- `.docx` - Editable source documents (Word)
- `.pdf` - Distribution versions
- `.xlsx` - Test specification matrix (`Test List.xlsx`)
- `.m4a` - Webinar audio recordings

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for project overview.
