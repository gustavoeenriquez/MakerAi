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

## SmartDispatch: clasificador dedicado (sep 27/2026)

`TAiChatTools.DispatchClassifier: TAiDispatchClassifierBase` (interfaz `IAiDispatchClassifier` en `uMakerAi.Chat.Tools`). Si está asignado, `InternalRunSmartDispatch` le pide el tag antes del pase 1 por LLM (`ClassifySmartDispatch`); solo se aceptan tags cuya tool esté asignada (`SmartDispatchTags`). Tag de tool → `RunSmartDispatchTool` con el **prompt original** (el clasificador no reescribe). `CHAT` → `InternalRunCompletions` normal, **con historial** (el pase por LLM responde en un contexto aislado de dos mensajes). `''` o excepción → pase 1 por LLM como siempre; la excepción no dispara `OnError`, solo `DoStateChange`. Implementación con Jev: `TAiJevDispatchClassifier` (`Source/Tools/uMakerAi.Jev.SmartDispatch.pas`).

## Evals: juez calibrado (sep 27/2026)

`TAiEvalRunner.Scorer: TAiEvalScorerBase` + check `ExpectScore('criterio', min = 0.5)` (`ekScore`, agregado al final del enum para no mover ordinales). `Score(criterio, input, salida)` devuelve la probabilidad 0..1 de que la salida cumpla; el check pasa si es ≥ `min`. Un error del scorer falla el check con `scorer error: …` (no lanza). Sin `Scorer` asignado, el check falla con motivo explícito. Implementación con Jev: `TAiJevEvalScorer` (`Source/Tools/uMakerAi.Jev.Evals.pas`).

## Guardrail de entrada (sep 27/2026)

`TAiChatTools.PromptGuard: TAiPromptGuardBase` (en `uMakerAi.Chat.Tools`, con el record `TAiPromptVerdict`: `Allowed`, `Category`, `Score`, `Reason`). `TAiChat.Run` lo consulta para cada mensaje `user` **después** del sanitizador por regex (`SanitizerActive`) y antes de la memoria y del LLM. Si bloquea, dispara `OnPromptGuard(Sender, Verdict, var Action)` con `Action = saBlock` por defecto (mismo `TAiSanitizeAction` que el sanitizador: `saAllow` sigue, `saAllowWrapped` envuelve el prompt con `TSanitizerPipeline`); con `saBlock` llama `DoError` y `Run` devuelve `''` sin tocar la red. Si `CheckPrompt` lanza, `BlockOnError` (default `True`) decide. `TAiChatConnection` propaga `OnPromptGuard` como `OnSanitize`. Implementación con Jev: `TAiJevPromptGuard` (`Source/Tools/uMakerAi.Jev.PromptGuard.pas`).

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
