# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Purpose

This folder contains Delphi IDE component palette icons for MakerAI components. The icons appear in the Delphi component palette when components are installed.

## Structure

- `icons/` - 24x24 BMP bitmap files for each component
- `gen_icons.py` - generator for the MakerAI-style icons (Python + Pillow); see below
- `uMakerAiResources.rc` / `.RES` - icons of `MakerAI.dpk`, linked from `uMakerAi.Chat.AiConnection.pas`
- `uMakerAiUIIcons.rc` / `.RES` - icons of `MakerAi.UI.dpk`, linked from `uMakerAi.UI.ChatList.pas`
- `uMakerAiRAGDriversIcons.rc` / `.RES` - icons of `MakerAi.RAG.Drivers.dpk`, linked from `uMakerAi.RAG.Vector.Driver.Postgres.pas`

The IDE looks for the bitmap in the package that registers the component, so each
package links its own `.res`. An icon placed in the wrong `.rc` does not show up.

## Adding a New Component Icon

1. Add an entry to `SPECS` in `gen_icons.py`: package (`core`/`ui`/`rag`), class name,
   layout and arguments. Layouts: `glyph` (white glyph on tile), `glyphtag` (glyph +
   short tag), `text2`/`text1` (text lines, like the RAG icons), `bubble` (chat driver
   monogram), `bridge`, `emb` (embeddings document).
   Tile color = provider (MAROON = MakerAI/framework). **Keep texts to 2-4 letters**:
   longer words are unreadable at 24 px.
2. `python gen_icons.py --preview %TEMP%\icons.png` generates the missing BMPs
   (`--force` regenerates all generated ones) and an enlarged contact sheet.
3. Add the line to the package's `.rc` (resource name = class name in UPPERCASE):
   ```
   T[COMPONENTNAME]  BITMAP "icons\T[ComponentName]_24.bmp"
   ```
4. Recompile the resource and then the package:
   ```
   brcc32 uMakerAiResources.rc
   ```
   (`brcc32` is in `C:\Program Files (x86)\Embarcadero\Studio\37.0\bin`)

The provider icons drawn by hand (OpenAI, Claude, Gemini, ...) are not in `SPECS`
and are never overwritten by the generator.

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
