# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Delphi FMX demo application showcasing AI agent tools integration using the MakerAI library. It demonstrates multiple AI provider integrations and RAG (Retrieval Augmented Generation) capabilities.

## Build Commands

- Open `DemoAgentesTools.dproj` in Delphi IDE (RAD Studio)
- Build with `Shift+F9` or Run with `F9`
- Command line: `msbuild DemoAgentesTools.dproj` (requires RAD Studio command prompt)

## Architecture

### Main Components

- **DemoAgentesTools.dpr** - Application entry point
- **uMainDemoAgenteTools.pas** - Main form with UI (FMX)
- **uDmAgentesTool.pas** - Data module containing all AI service components

### AI Provider Integration

The data module (`TFDataModule`) integrates multiple AI services through MakerAI components:

| Component | Provider | Purpose |
|-----------|----------|---------|
| `AiGeminiChat1` | Google Gemini | Web search with Gemini 3 Pro |
| `AiOpenChat1` | OpenAI | Chat with GPT-5, web search enabled |
| `NanoBanana` | Google Gemini | Image generation (gemini-3-pro-image-preview) |
| `OllamaHtml` | Ollama (local) | HTML/Dashboard generation (gpt-oss:20b) |
| `AiOllamaEmbeddings1` | Ollama (local) | Embeddings (snowflake-arctic-embed, 1024 dims) |
| `AiOllamaPdfTool1` | Ollama (local) | PDF to text extraction (deepseek-ocr) |

### RAG System

- **TAiRAGVector** - Vector store manager with BM25/embedding hybrid search
- **TAiRAGVectorPostgresDriver** - PostgreSQL pgvector backend
- **VQL Support** - Vector Query Language via `ExecuteVGQL()`

### Key Methods in TFDataModule

- `WebSearch(Pregunta)` - AI-powered web search via OpenAI
- `BuscarRAGV(Pregunta)` - Semantic vector search (top 10, threshold 0.7)
- `BuscarVQL(Pregunta)` - Execute VQL queries
- `CreateImage(Pregunta)` - Generate images via Gemini
- `CreateDashBoard(Pregunta)` - Generate HTML dashboards via local Ollama
- `PdfToText(FileName)` - Extract text from PDFs using OCR

## External Dependencies

- PostgreSQL database (`lunaidb`) with pgvector extension
- Ollama running locally on port 11434
- Ghostscript (`gswin64c.exe`) for PDF processing

## API Keys Configuration

Keys are configured in the DFM using environment variable syntax:
- `@GEMINI_API_KEY`
- `@OPENAI_API_KEY`
- `@OLLAMA_API_KEY`

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
