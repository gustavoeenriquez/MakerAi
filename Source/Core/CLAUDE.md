# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Source/Core Overview

This folder contains the foundation layer of the MakerAI framework. All provider-specific chat drivers, RAG systems, and UI components depend on these core abstractions.

## Unit Responsibilities

| Unit | Purpose |
|------|---------|
| `uMakerAi.Core.pas` | Base types: `TAiMediaFile`, `TAiFileCategory`, `TAiChatMediaSupport`, `TAiChatState`, MIME utilities |
| `uMakerAi.Chat.pas` | Abstract `TAiChat` base class - all LLM drivers inherit from this |
| `uMakerAi.Chat.Messages.pas` | `TAiChatMessage`, `TAiChatMessages`, `TAiToolsFunction`, citations system |
| `uMakerAi.Chat.Tools.pas` | `IAiToolContext` interface and base tool classes (`TAiSpeechToolBase`, `TAiVisionToolBase`, etc.) |
| `uMakerAi.Chat.Bridge.pas` | Bridge utilities for chat interoperability |
| `uMakerAi.Prompts.pas` | Prompt template utilities |
| `uMakerAi.Telemetry.pas` | `TAiTelemetry` — OpenTelemetry tracing opt-in (OTLP/HTTP JSON, GenAI semconv); helpers no-op-safe `AiSpanStart`/`AiSpanEnd`/`AiSpanAttr` usados por Chat, Tools y MCP |
| `uMakerAi.Evals.pas` | `TAiEvalRunner` — evals con casos fluidos (`ExpectContains`/`Regex`/`Equals`/`MinLength`/`MaxLength`/`Judge`) sobre un target genérico `function(Input): string`; reporte `ToText`/`ToJSON`; LLM-as-judge opcional vía propiedad `Judge`; span `eval.case` por caso |
| `uMakerAi.Version.inc` | Version constants and feature flags - included via `{$I}` directive |
| `uMakerAi.Utils.*.pas` | Utility helpers (CodeExtractor, PcmToWav, System) |

## Key Types and Patterns

### File Category System
```pascal
TAiFileCategory = (Tfc_Text, Tfc_Image, Tfc_Audio, Tfc_Video, Tfc_Pdf, ...)
TAiChatMediaSupport = (Tcm_Text, Tcm_Image, Tcm_Audio, Tcm_Video, Tcm_Pdf, ...)
```
`TAiFileCategory` represents physical file types. `TAiChatMediaSupport` represents model capabilities.

### Chat State Machine
```pascal
TAiChatState = (acsIdle, acsConnecting, acsCreated, acsReasoning, acsWriting,
                acsToolCalling, acsToolExecuting, acsFinished, acsAborted, acsError)
```
All chat implementations follow this state flow. Use `OnStateChange` event to track transitions.

### TAiChat Abstract Methods
When creating a new LLM driver, these class methods must be implemented:
```pascal
class function GetDriverName: string; virtual; abstract;
class procedure RegisterDefaultParams(Params: TStrings); virtual; abstract;
class function CreateInstance(Sender: TComponent): TAiChat; virtual; abstract;
```

### Tool Context Interface
`IAiToolContext` allows tools to report events back to the chat without circular dependencies:
```pascal
IAiToolContext = interface
  procedure DoData(Msg: TAiChatMessage; const Role, Text: string; AResponse: TJSONObject = nil);
  procedure DoDataEnd(Msg: TAiChatMessage; const Role, Text: string; AResponse: TJSONObject = nil);
  procedure DoError(const ErrorMsg: string; E: Exception);
  procedure DoStateChange(State: TAiChatState; const Description: string = '');
  function GetAsynchronous: Boolean;
end;
```

## Delphi Version Compatibility

The code uses conditional compilation for Delphi version differences:
```pascal
{$IF CompilerVersion >= 34}  // Delphi 10.3 Rio+
  Client.SynchronizeEvents := False;
{$IFEND}

{$IF CompilerVersion < 35}
uses uJSONHelper;  // JSON helper for older Delphi versions
{$ENDIF}
```

## Thread Safety

- `TAiChatMessage` uses `TCriticalSection` (`FLock`) for thread-safe media file operations
- Tool base classes use `TThread.Queue` for reporting events to the main thread
- Streaming responses are handled asynchronously via `OnReceiveData` event

## Helper Functions

From `uMakerAi.Core.pas`:
- `GetContentCategory(FileExtension)` - Returns `TAiFileCategory` from file extension
- `GetMimeTypeFromFileName(FileExtension)` - Returns MIME type string
- `GetFileExtensionFromMimeType(MimeType)` - Reverse lookup
- `StreamToBase64(Stream)` - Converts TMemoryStream to Base64 string

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
