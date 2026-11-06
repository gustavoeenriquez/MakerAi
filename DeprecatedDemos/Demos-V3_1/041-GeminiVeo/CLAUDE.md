# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **VeoDemo**, a Delphi FireMonkey (FMX) demo application for Google's Gemini Veo video generation API. It's part of the MakerAI Suite - an AI orchestration framework for Delphi.

## Build Commands

Open `VeoDemo.dproj` in Delphi IDE (11 Alexandria to 13 Florence) and build. No command-line build configured.

Requires MakerAI library units in the Delphi Library Path:
- `uMakerAi.Gemini.Veo` - Veo API wrapper
- `uMakerAi.UI.ChatInput` / `uMakerAi.UI.ChatList` - FMX chat components
- `uMakerAi.Core` - Core types (`TAiMediaFile`, `TAiMediaFiles`, `TAiFileCategory`)
- `uMakerAi.utils.System` - Utility functions

## Architecture

### Main Components

- **`TAiVeoGenerator`** (`AiVeo`): Non-visual component wrapping Gemini Veo API. Configured via properties: `Model`, `AspectRatio`, `Resolution`, `PersonGeneration`, `DurationSeconds`, `NegativePrompt`. API key set via `@GEMINI_API_KEY` placeholder.

- **`TChatInput`** / **`TChatList`**: MakerAI FMX components for user input with file attachments and message history display.

### Video Generation Modes

The app auto-selects generation mode based on attached files:

| Attachments | Mode |
|-------------|------|
| None | Text-to-Video (`GenerateFromText`) |
| 1 image | Image-to-Video (`GenerateFromImage`) |
| 1 video | Extend Video (`ExtendVideo`) - requires `.veometa` file |
| 2 images | Interpolation (`GenerateFromFrames`) or Reference mode |
| 3 images | Reference Images (`GenerateWithReferences`) |

User can force mode via radio buttons when 2 images attached.

### Key Types

```pascal
TVeoGenerationMode = (gmAuto, gmFrames, gmReferences);
TVeoModel = (vmCustom, vmVeo3_1, vmVeo3_1_Fast, vmVeo3_0, vmVeo3_0_Fast, vmVeo2_0);
TVeoAspectRatio = (arDefault, ar16x9, ar9x16);
TVeoResolution = (vrDefault, vr720p, vr1080p);
TVeoPersonGeneration = (pgDefault, pgAllowAll, pgAllowAdult, pgDontAllow);
```

### Event Flow

1. `ChatInput1SendEvent` → collects prompt, media files, and configuration
2. `ExecuteVeoTask` → routes to appropriate `AiVeo` method based on attachments
3. `AiVeoProgress` → updates status in log
4. `AiVeoSuccess` → displays result, prompts to save `.mp4` and `.veometa`
5. `AiVeoError` → displays error message

### Video Extension Workflow

Generated videos can be extended if saved with metadata:
- Video saved as `.mp4`
- Metadata saved as `.veometa` (JSON without base64 content, contains `UrlMedia` identifier)
- To extend: attach the `.mp4` file (must have adjacent `.veometa`)

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
