# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Purpose

This folder contains Delphi IDE component palette icons for MakerAI components. The icons appear in the Delphi component palette when components are installed.

## Structure

- `icons/` - 24x24 BMP bitmap files for each component
- `uMakerAiResources.rc` - Resource script linking bitmaps to component class names
- `uMakerAiResources.RES` - Compiled resource file (auto-generated)

## Adding a New Component Icon

1. Create a 24x24 pixel BMP file named `T[ComponentName]_24.bmp` in `icons/`
2. Add an entry to `uMakerAiResources.rc` in the appropriate category section:
   ```
   T[COMPONENTNAME]  BITMAP "icons\T[ComponentName]_24.bmp"
   ```
   Note: The resource name (left side) must be UPPERCASE and match the class name exactly
3. Recompile the resource file:
   ```
   brcc32 uMakerAiResources.rc
   ```
   (BRCC32 is in Delphi's `bin` directory, e.g., `C:\Program Files (x86)\Embarcadero\Studio\23.0\bin`)

## Naming Convention

- Icon filename: `T[ComponentName]_24.bmp` (matches class name, 24px size suffix)
- Resource identifier: `T[COMPONENTNAME]` (uppercase, no suffix)
- Some identifiers use shortened names (e.g., `TAIOPENCHAT` for `TAiOpenAIChat`)

## RC File Categories

The resource script organizes icons by component category:
- CORE COMPONENTS - `TAiChatConnection`
- CHAT COMPONENTS - Provider-specific chat drivers
- AI SERVICES - Whisper, DALL-E, VoiceMonitor
- MCP SERVICES - MCP server components
- EMBEDDINGS - Embedding providers
- RAG COMPONENTS - Vector and graph RAG
- AGENTS - Agent system components

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
