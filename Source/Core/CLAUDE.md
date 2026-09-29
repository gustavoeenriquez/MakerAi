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
| `uMakerAi.Prompts.pas` | `TAiPrompts`: plantillas con `<#var>`, prompts y skills de PPM (`LoadFromPPM`, `LoadSkillFromPPM`) y skills locales (`LoadSkillFromFile`, `LoadSkillsFromFolder`); `ApplySkill(nombre, SystemPrompt, Append)` copia las instrucciones a cualquier `TStrings` (sirve igual para `TAiChat` y `TAiChatConnection`, que no heredan uno del otro) |
| `uMakerAi.Skills.Format.pas` | Parser único de SKILL.md (`TAiSkillDoc`: frontmatter YAML + cuerpo; texto, archivo, carpeta o PPM; `FindSkillFiles`) y cliente de solo lectura del registry PPM (`TAiPPMClient`: versión por semver sin retiradas, prefijo `skill-` ante un 404, error claro ante un 200 con HTML). Lo usan `TAiPrompts`, `TAiSkill` y `TAiSkills` |
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

## Parser de streaming: dos fixes (sep 28/2026)

- **Texto duplicado en `OnReceiveDataEnd` (asíncrono).** El `[DONE]` arma un mensaje sintético con `content = FLastContent` (ya acumulado por los deltas) y `ParseChat` volvía a sumarle ese content a `FLastContent`: el evento recibía `'Listo'#13#10'Listo'`. Afectaba a todos los drivers del parser común (verificado en vivo: Groq, DeepSeek; tras el fix también Kimi, Grok y Mistral entregan una sola vez). Fix: se restaura `FLastContent` tras `ParseChat` **solo si había content** (sin content, `ParseChat` usa el reasoning como respuesta y eso se conserva). `TAiDeepSeekChat` tiene una copia de `ProcessLine` y lleva el mismo fix. `TAiOpenChat` no pasa por aquí (sobrescribe `OnInternalReceiveData` para la API Responses).
- **`executed_tools` en el delta.** Groq `code_interpreter` con `gpt-oss` manda `executed_tools` dentro de `choices[0].delta`, en dos chunks por tool con el mismo `index` (primero `arguments`, luego `arguments` + `output`); el parser solo leía el campo en la raíz (formato del retirado `groq/compound`). `MergeStreamExecutedTools` los combina por `index` en `FLastExecutedToolsJSON` y el `[DONE]` los entrega a `ProcessExecutedTools`, como el camino sync.

## Liberar un chat con una petición asíncrona en vuelo (sep 28/2026)

`OnReceiveDataEnd` sale del último chunk, **antes** de que el cliente HTTP cierre la petición. El cierre (`OnRequestCompletedEvent`, en el hilo HTTP porque `SynchronizeEvents := False`) libera `FCurrentPostStream`; si el integrador liberaba el chat justo después del evento final — lo natural — el destructor liberaba el mismo stream en paralelo: **`Invalid pointer operation` intermitente**, que además llegaba a `OnError`. Fix: el constructor ya no enchufa los métodos virtuales directo al `TNetHTTPClient` sino una capa fija (`ClientReceiveData`, `ClientRequestCompleted/Error/Exception`) que marca la petición en vuelo (`FRequestDone: TEvent`, se apaga al llegar datos en modo asíncrono y se enciende al cerrar) y llama a los virtuales — sirve para todos los drivers sin tocarlos. El destructor (`WaitPendingRequest`) espera ese cierre (máx. 15 s; bombea `CheckSynchronize` si corre en el hilo principal, para Delphi < 10.4), y antes desconecta los eventos del usuario y pone `FAbort`. Sin petición en vuelo no espera nada. **Un driver no debe reasignar `FClient.OnReceiveData`/`OnRequest*`** (Claude lo hacía de forma redundante y se quitó). Suite: `chat.async.free-waits-request` (determinista, cierre desde otro hilo a los 300 ms; falla sin el fix).

## ParseJsonTranscript fuera de cmTranscription (sep 28/2026)

En el puente de Fase 1 de `RunNew` (modelo de texto con `SessionCaps` `[cap_Audio]` en `cmConversation`) la transcripción es **entrada** para el modelo. `ParseJsonTranscript` la escribía además en `ResMsg` y disparaba `OnReceiveDataEnd` con el texto crudo antes de la respuesta real (Cohere, Groq, Mistral, OpenAI). Ahora, fuera de `cmTranscription`, solo llena `MediaFile.Transcription`, `Procesado` y los contadores de tokens. Nota: esos drivers transcriben con el `Model` de la sesión, así que el puente solo aplicaba con configuraciones manuales (modelo de transcripción con `ModelCaps []`). Suite: `chat.transcript.bridge-mode`.

## tool_choice forzado solo en la primera llamada (sep 29/2026)

`TAiChat.Tool_choice` se lee con `GetTool_choice`: si el valor fuerza una tool (`required`/`any` o una función concreta) y el último mensaje del historial es un resultado de tool (`Role = 'tool'` o `ToolCallId`), devuelve `'auto'`. Antes se reenviaba en cada ronda y el modelo quedaba obligado a llamar otra tool: con `claude-sonnet-5` y `required`, **231 requests en un loop sin fin**. Todos los drivers leen la propiedad (la base usaba el campo `FTool_choice` y se cambió), así que el arreglo es uno solo. `auto` y `none` no cambian. Suite: `chat.toolchoice.followup`; probado en vivo con Claude (sonnet-5, sonnet-5-5, opus-5-5) y OpenAI (`gpt-6-sol`).

## Model vacío = modelo por defecto con sus parámetros (sep 28/2026)

`TAiChatFactory.GetDriverParams` (y la de embeddings) arma los parámetros en tres niveles: defaults del driver (`RegisterDefaultParams`, incluye `Model=<default>`), los del driver en el catálogo y los del **modelo**. El tercero solo se consultaba si `ModelName` tenía valor, así que una conexión con `DriverName` y sin `Model` usaba el modelo por defecto **sin** sus caps, `Max_Tokens` ni `ThinkingLevel`. Medido: 9 de 15 drivers daban otro chat (Qwen sin visión, Groq sin razonamiento/intérprete y con `Max_Tokens` 8192 en lugar de 65536, Gemini sin audio/PDF/búsqueda, Grok, Kimi, Mistral, MakerAi, DeepSeek, GLM). Ahora, sin `ModelName`, el nivel 3 usa el `Model` que resolvieron los niveles 1 y 2. `TAiChatConnection.Model` sigue vacío (no cambia lo que se guarda en el DFM). **Cambio de comportamiento:** quien usaba solo `DriverName` recibe ahora la configuración del catálogo de su modelo por defecto — la misma que obtenía asignándolo explícitamente. Suite: `conn.empty-model-default-params` (invariante para todos los drivers registrados).

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
