# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This folder contains miscellaneous MakerAI demo applications demonstrating various library features.

## Build Commands

```bash
# Build console app
msbuild ListAllModels.dproj /p:Config=Debug /p:Platform=Win32

# Build VCL app
msbuild CompareEmbeddings/CompareEmbeddings.dproj /p:Config=Debug /p:Platform=Win32
```

Or open `../../DemosVersion31.groupproj` in RAD Studio IDE.

## Demo Projects

### ListAllModels (Console)
Lists all registered LLM drivers and their available models using `TAiChatConnection`. Demonstrates:
- Driver registration system via `uMakerAi.chat.Initializations`
- `TAiChatConnection.GetDriversNames` / `GetModels` API

### CompareEmbeddings (VCL)
Compares text similarity using Ollama embeddings. Requires Ollama running locally.
- Uses `TAiOllamaEmbeddings.CreateEmbedding()` to generate vectors
- Compares via `CosineSimilarity()` and `EuclideanDistance()`

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
