# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Directory Purpose

This directory contains technical documentation for MakerAI v3.2 - an AI orchestration framework for Delphi. The documentation is primarily in Spanish with English versions available for key modules.

**This is documentation only** - no source code lives here. Source code is in `/Source/`.

## Documentation Structure

| Directory | Content | Language |
|-----------|---------|----------|
| Root | Installation guide, component user guides, chat/prompts/tools docs | Spanish |
| `Agents/` | Agent orchestration guide, conditional language appendix, webinar audio | Spanish |
| `MCPServer/` | MCP Server technical documentation | English + Spanish |
| `RAG/` | Vector and knowledge graph RAG documentation | Spanish |
| `PDF/` | Consolidated PDF versions for distribution | Both |

## Subdirectory CLAUDE.md Files

Two subdirectories have specialized guides:
- `MCPServer/CLAUDE.md` - MCP protocol implementation patterns, testing commands
- `RAG/CLAUDE.md` - pgvector reference, RAG implementation patterns

## Key Documentation Files

| File | Covers |
|------|--------|
| `Manual Instalacion.docx` | Step-by-step installation |
| `uMakerAi-ChatConnection.docx` | Universal LLM connector (`TAiChatConnection`) |
| `uMakerAi.Chat.docx` | Chat message handling and state machine |
| `uMakerAi-Agents.ES.docx` | Graph-based agent orchestration |
| `uMakerAi-RAG.ES.docx` | Vector RAG with PostgreSQL/pgvector |
| `uMakerAI-RAGGraph.docx` | Knowledge graph RAG |
| `uMakerAi.ToolFunciones.docx` | Function calling system |
| `TAiChatClaude-Guia de Uso.docx` | Claude provider specifics |
| `TAiChatGemini-Guia de Uso.docx` | Gemini provider specifics |

## Cross-Reference to Source Code

Documentation files map to source directories:

| Documentation | Source Location |
|---------------|-----------------|
| Chat/ChatConnection docs | `Source/Chat/`, `Source/Core/` |
| Agent docs | `Source/Agents/uMakerAi.Agents.pas` |
| RAG docs | `Source/RAG/` |
| MCP Server docs | `Source/MCPServer/` |
| Tools/Functions docs | `Source/Tools/uMakerAi.Tools.Functions.pas` |

## File Formats

- `.docx` - Editable source documents
- `.pdf` - Distribution-ready versions
- `.xlsx` - Test specification matrix (`Test List.xlsx`)
- `.m4a` - Webinar audio (Agents orchestration)

## Translation Notes

Most documentation is Spanish-first. English versions exist for:
- MCP Server (`uMakerAi-MCP.Server.EN.docx/.pdf`)

When referencing documentation for English speakers, prefer the MCPServer English docs or the parent project's `CLAUDE.md` which is in English.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for documentation index and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
