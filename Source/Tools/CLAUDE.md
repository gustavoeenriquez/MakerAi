# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Tools Module Overview

The `Source/Tools/` directory contains capability components that extend LLM functionality beyond basic chat. These are standalone components that can be attached to chat instances to enable function calling, media generation, shell execution, and computer automation.

## Unit Categories

### Function Calling System
- `uMakerAi.Tools.Functions.pas` - Core function calling infrastructure
  - `TAiFunctions` - Component for defining callable functions with parameters
  - `TFunctionActionItem` - Individual function definition with `FunctionName`, `Description`, `Parameters`
  - `TFunctionParamsItem` - Parameter definition with `ParamType` (ptString, ptInteger, ptBoolean, etc.)
  - `TMCPClientItem` - MCP server connection management within functions

### Agent Automation Tools
- `uMakerAi.Tools.Shell.pas` - Interactive shell execution (`TAiShell`)
  - Persistent session via `FSession: TInteractiveProcessInfo`
  - Auto-detects JSON format from Claude/OpenAI/Generic providers
  - Events: `OnCommand`, `OnConsoleLog`

- `uMakerAi.Tools.TextEditor.pas` - File editing tool (`TAiTextEditorTool`)
  - Commands: `Cmd_View`, `Cmd_Create`, `Cmd_StrReplace`, `Cmd_Insert`, `Cmd_ApplyDiff`
  - Event-based I/O virtualization (can target memory/DB instead of disk)
  - Set `Handled := True` in events to override default file system behavior

- `uMakerAi.Tools.ComputerUse.pas` - Computer automation (`TAiComputerUseTool`)
  - Handles Gemini's normalized coordinates (0-1000) conversion to screen pixels
  - Action types: click, drag, type, scroll, navigate, screenshot
  - Platform implementations in `*.Windows.pas` and `*.WindowsFMX.pas`

### Media Generation Tools
- `uMakerAi.OpenAi.Dalle.pas` - Image generation (`TAiDalle`)
  - Supports dall-e-2, dall-e-3, gpt-image-1
  - Streaming with `OnPartialImageReceived`, `OnStreamCompleted`

- `uMakerAi.OpenAI.Sora.pas` - Video generation (`TAiSoraGenerator`)
  - Async methods: `GenerateFromText`, `GenerateFromImage`, `RemixVideo`
  - Polling-based job completion

- `uMakerAi.Gemini.Video.pas` - Veo video generation (`TAiGeminiVideoTool`)
  - Inherits from `TAiVideoToolBase`
  - Properties: `AspectRatio`, `Resolution`, `DurationSeconds`, `PersonGeneration`

### Speech/Audio Tools
- `uMakerAi.Whisper.pas` - OpenAI Whisper compatibility (legacy)
- `uMakerAi.OpenAI.Audio.pas` - Modern OpenAI audio (`TAiOpenAiAudio`)
  - TTS with streaming (`OnAudioChunkReceived`)
  - Transcription with GPT-4o support and diarization

- `uMakerAi.Gemini.Speech.pas` - Gemini TTS (`TAiGeminiSpeechTool`)
  - Multi-voice support: `"Anya=Kore, Liam=Puck"`
  - Director's notes and audio profile configuration

### Utility Tools
- `uMakerAi.Gemini.WebSearch.pas` - Web search grounding
- `uMakerAi.Ollama.Ocr.pas` - OCR via Ollama vision models

## Key Patterns

### Tool Base Classes
Media tools inherit from base classes in `uMakerAi.Chat.Tools.pas`:
- `TAiSpeechToolBase` - TTS and transcription
- `TAiVideoToolBase` - Video generation

### Event-Driven Design
All tools use events for extensibility:
```pascal
// Shell intercepts commands before execution
FOnCommand: TAiShellCommandEvent;
// TextEditor virtualizes file I/O
FOnLoadFile: TAiFileReadEvent;
FOnSaveFile: TAiFileWriteEvent;
```

### Provider-Agnostic Execution
Shell and TextEditor auto-detect provider format:
```pascal
function ExecuteClaudeAction(const CallId: string; JArgs: TJSONObject): string;
function ExecuteOpenAIAction(const CallId: string; JArgs: TJSONObject): string;
function ExecuteGenericAction(const CallId: string; JArgs: TJSONObject): string;
```

## Dependencies

All tools depend on:
- `uMakerAi.Core` - `TAiMediaFile`, base types
- `uMakerAi.Chat.Messages` - `TAiChatMessage`, `TAiToolsFunction`

Some tools require additional utils:
- `uMakerAi.Utils.System` - Process management for shell
- `uMakerAi.Utils.DiffUpdater` - Diff application for text editor
- `uMakerAi.Utils.PcmToWav` - Audio format conversion

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
