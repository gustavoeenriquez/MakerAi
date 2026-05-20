# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Delphi VCL demo application that compares text embeddings using the MakerAI library. It demonstrates similarity calculations between two text inputs using Ollama embeddings.

## Build Commands

```bash
# Build from command line using MSBuild (requires RAD Studio installed)
msbuild CompareEmbeddings.dproj /p:Config=Debug /p:Platform=Win32
msbuild CompareEmbeddings.dproj /p:Config=Release /p:Platform=Win64
```

Alternatively, open `CompareEmbeddings.dproj` in RAD Studio IDE and build with F9.

## Architecture

**Main Form**: `uMainCompareEmbeddings.pas` (TForm14)
- Uses `TAiOllamaEmbeddings` component from MakerAI library to generate embeddings via Ollama
- Compares two text inputs using:
  - **Cosine Similarity**: Measures angle between embedding vectors (higher = more similar)
  - **Euclidean Distance**: Measures absolute distance (lower = more similar)

## Dependencies

- **MakerAI library**: Provides `uMakerAi.Embeddings.core`, `uMakerAi.Embeddings`, and `uMakerAi.Chat.Ollama` units
- **Ollama**: Must be running locally for embeddings generation

## Key Components

- `TAiOllamaEmbeddings`: Generates embeddings via local Ollama instance
- `TAiEmbeddingData`: Data structure holding embedding vectors
- `CosineSimilarity()` / `EuclideanDistance()`: Comparison methods

## Navigation

> See [../../CLAUDE.md](../../CLAUDE.md) for demos overview and [../../../CLAUDE.md](../../../CLAUDE.md) for project overview.
