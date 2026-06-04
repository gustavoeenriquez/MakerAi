# Capability System: ModelCaps and SessionCaps

**MakerAI v3.3 — Technical Documentation**
Last updated: March 2026

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [The TAiCapability Enum](#2-the-taicapability-enum)
3. [ModelCaps vs SessionCaps](#3-modelcaps-vs-sessioncaps)
4. [Gap Analysis: The Core Engine](#4-gap-analysis-the-core-engine)
5. [Configuration via TAiChatFactory](#5-configuration-via-taichatfactory)
6. [Runtime Configuration](#6-runtime-configuration)
7. [Backward Compatibility with the Legacy System](#7-backward-compatibility-with-the-legacy-system)
8. [Common Configuration Patterns](#8-common-configuration-patterns)
9. [Capability Reference by Provider](#9-capability-reference-by-provider)
10. [Adding a New Provider](#10-adding-a-new-provider)
11. [Source File Reference](#11-source-file-reference)

---

## 1. Introduction

MakerAI v3.3 introduces the **unified capability system** (`TAiCapabilities`), which replaces four scattered legacy parameters (`NativeInputFiles`, `NativeOutputFiles`, `ChatMediaSupports`, `EnabledFeatures`) with two simple, orthogonal properties:

| Property | Meaning |
|----------|---------|
| `ModelCaps` | What the model can do **natively** via its completions endpoint |
| `SessionCaps` | What the session **needs** (may exceed native capabilities) |

The difference between them (`Gap = SessionCaps - ModelCaps`) determines which bridges and tools the orchestrator automatically activates before calling the API.

**Advantages over the previous system:**
- A single pair of lines fully describes a model's capabilities
- The correct bridge is activated automatically, with no extra code
- Fully backward-compatible: models configured with the legacy system continue working unchanged

---

## 2. The TAiCapability Enum

Defined in `Source/Core/uMakerAi.Core.pas`:

```pascal
TAiCapability = (
  // ---- Input / Understanding (native via completions) ----
  cap_Image,            // model understands incoming images
  cap_Audio,            // model understands/transcribes incoming audio
  cap_Video,            // model understands incoming video
  cap_Pdf,              // model understands incoming PDFs
  cap_WebSearch,        // model can search the web natively
  cap_Reasoning,        // model has extended reasoning (CoT/thinking)
  cap_CodeInterpreter,  // model can execute code natively
  cap_Memory,           // model has persistent memory
  cap_TextEditor,       // model can edit text files
  cap_ComputerUse,      // model can control the computer
  cap_Shell,            // model can execute shell commands

  // ---- Output / Generation (gap -> activates automatic bridge) ----
  cap_GenImage,         // produce an image as output
  cap_GenAudio,         // produce audio as output (TTS)
  cap_GenVideo,         // produce video as output
  cap_GenReport,        // produce a report (PDF, HTML, XLSX)
  cap_ExtractCode       // post-process: extract code blocks from the response
);

TAiCapabilities = set of TAiCapability;
```

### Conceptual classification

**Input capabilities** (`cap_Image` .. `cap_Shell`): describe what types of content the model understands natively in the completions endpoint. If the model lacks an input capability that the session requires, the orchestrator runs an **input bridge** (transcription, image description, PDF text extraction) before sending the prompt.

**Generation capabilities** (`cap_GenImage` .. `cap_ExtractCode`): describe what type of content the session produces as output. When there is a gap in these capabilities, the orchestrator redirects the call to the appropriate specialized endpoint (TTS, image generation, etc.) instead of completions.

---

## 3. ModelCaps vs SessionCaps

### ModelCaps — native model capabilities

Represents exactly what the model can do through the completions endpoint **without external intervention**. This is a fixed fact about the model; it does not change based on user needs.

Examples:
- `GPT-4.1`: can process images → `ModelCaps = [cap_Image]`
- `dall-e-3`: only generates images, does not handle completions → `ModelCaps = []`
- `gemini-2.5-flash`: fully multimodal → `ModelCaps = [cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]`

### SessionCaps — desired session capabilities

Represents what the **user wants** the session to be able to do. It can match `ModelCaps` (no gap, direct call) or exceed it (gap present, bridges are activated).

Examples:
- Using GPT-4o to generate TTS audio: `SessionCaps = [cap_Image, cap_GenAudio]` → Gap = `[cap_GenAudio]` → uses OpenAI TTS endpoint
- Using Ollama (text-only) with images: `SessionCaps = [cap_Image]` + `ModelCaps = []` → Gap = `[cap_Image]` → image description bridge runs before completions

### Relationship with legacy parameters

When `ModelCaps` and `SessionCaps` are assigned, the system automatically synchronizes the legacy parameters:

| New property | Synchronizes (legacy) |
|---|---|
| `ModelCaps` | `NativeInputFiles` + `ChatMediaSupports` |
| `SessionCaps` | `NativeOutputFiles` + `EnabledFeatures` |

This synchronization occurs in the `SetModelCaps` and `SetSessionCaps` setters, which are called both from code and via RTTI when applying factory parameters.

---

## 4. Gap Analysis: The Core Engine

The `TAiChat.RunNew` method computes the gap on its very first line:

```pascal
Gap := FSessionCaps - FModelCaps;  // set subtraction
```

It then executes three phases:

### Phase 1: Input bridge (only in `cmConversation` mode)

For each file attached to the message that the model does not support natively:

| Gap contains | File type | Bridge activated |
|---|---|---|
| `cap_Audio` | `.mp3`, `.wav`, etc. | `InternalRunTranscription` → converts to text |
| `cap_Image` | `.png`, `.jpg`, etc. | `InternalRunImageDescription` → describes the image |
| `cap_Pdf` | `.pdf` | `InternalRunPDFDescription` → extracts/describes the PDF |

Already-processed files (`MF.Procesado = True`) are skipped. There is also a priority 1 override: if the `OnProcessMediaFile` event is assigned, it is called before any automatic bridge.

### Phase 2: Grounding (always, regardless of chat mode)

| Gap contains | Action |
|---|---|
| `cap_WebSearch` | `InternalRunWebSearch` — searches the web and injects results into the context |

### Phase 3: Output orchestration (in `cmConversation` mode)

The gap determines which endpoint is used. Evaluated in priority order:

| Gap contains | Method invoked |
|---|---|
| `cap_GenVideo` | `InternalRunImageVideoGeneration` |
| `cap_GenImage` | `InternalRunImageGeneration` |
| `cap_GenAudio` | `InternalRunSpeechGeneration` |
| `cap_GenReport` | `InternalRunReport` |
| (empty) | `InternalRunCompletions` (normal conversation) |

`cap_ExtractCode` does not redirect the endpoint — it is handled internally by `InternalRunCompletions` through the legacy parameter `Tfc_ExtractTextFile in NativeOutputFiles` (synchronized by `SyncLegacyFromSessionCaps`).

### Forced modes

If `ChatMode` is anything other than `cmConversation`, Phases 1 and 3 ignore the gap and call the corresponding method directly:

```pascal
cmImageGeneration  → InternalRunImageGeneration  (always)
cmVideoGeneration  → InternalRunImageVideoGeneration
cmSpeechGeneration → InternalRunSpeechGeneration
cmWebSearch        → InternalRunWebSearch
cmReportGeneration → InternalRunReport
cmTranscription    → InternalRunTranscription (first audio file in the message)
```

---

## 5. Configuration via TAiChatFactory

The standard way to configure capabilities is in `Source/Chat/uMakerAi.Chat.Initializations.pas`, using `TAiChatFactory.Instance.RegisterUserParam`.

### Configuration levels

Parameters have three levels (lowest to highest priority):

1. **Driver defaults** — `RegisterDefaultParams` in the driver class
2. **Global provider defaults** — `RegisterUserParam(Driver, Param, Value)`
3. **Per-model overrides** — `RegisterUserParam(Driver, Model, Param, Value)`

A per-model override always wins over the driver global.

### Syntax

```pascal
// Global default for all models of a provider
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'ModelCaps',   '[cap_Image]');
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'SessionCaps', '[cap_Image]');

// Override for a specific model
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'model-name', 'ModelCaps',    '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'model-name', 'SessionCaps',  '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'model-name', 'ThinkingLevel', 'tlMedium');
```

### Capability string format

The string follows Pascal set notation, using the exact names from the `TAiCapability` enum:

```
'[]'                                          // empty set
'[cap_Image]'                                 // single capability
'[cap_Image, cap_Reasoning]'                  // multiple capabilities
'[cap_Image, cap_Audio, cap_Video, cap_Pdf]'  // full multimedia
```

The parser (in `ApplyParamsToChat`, via RTTI `tkSet`) is case-sensitive: names must match the enum values exactly.

### ThinkingLevel parameter

For models with extended reasoning, the level is configured with `ThinkingLevel`:

| Value | Description |
|-------|-------------|
| `tlDefault` | Let the provider decide (usually Medium) |
| `tlLow` | Minimal reasoning — fast response, lower cost |
| `tlMedium` | Balanced quality/speed — recommended default |
| `tlHigh` | Maximum reasoning — highest quality, slower and more expensive |

`ThinkingLevel` only takes effect when `cap_Reasoning` is in `ModelCaps`. If the model does not have `cap_Reasoning`, the parameter is ignored.

### Custom profiles (aa_* prefix)

To create model variants with different settings (e.g. different thinking level, reduced caps), use `RegisterCustomModel` plus an override:

```pascal
// Creates 'aa_o3-high' that internally uses model 'o3'
TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_o3-high', 'o3');
TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'aa_o3-high', 'ThinkingLevel', 'tlHigh');

// Creates 'aa_gemini-3-pro-fast' with reduced thinking for faster responses
TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-3-pro-fast', 'gemini-3-pro-preview');
TAiChatFactory.Instance.RegisterUserParam('Gemini', 'aa_gemini-3-pro-fast', 'ThinkingLevel', 'tlLow');
TAiChatFactory.Instance.RegisterUserParam('Gemini', 'aa_gemini-3-pro-fast', 'ModelCaps',
  '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch]');  // no cap_Reasoning
TAiChatFactory.Instance.RegisterUserParam('Gemini', 'aa_gemini-3-pro-fast', 'SessionCaps',
  '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch]');
```

The `aa_` prefix is a project convention for custom profiles; it is not mandatory but helps distinguish them from official model names in dropdown lists.

---

## 6. Runtime Configuration

Besides the factory, `ModelCaps` and `SessionCaps` can be assigned directly on an instance in code:

### On TAiChatConnection

```pascal
// At design time (Object Inspector) or in code
AiConnection.DriverName := 'OpenAi';
AiConnection.Model := 'gpt-4.1';

// Override caps for this specific session
AiConnection.ModelCaps   := [cap_Image, cap_Pdf];
AiConnection.SessionCaps := [cap_Image, cap_Pdf, cap_GenAudio];
// Gap = [cap_GenAudio] -> next Run() will use the TTS endpoint
```

### On TAiChat directly

```pascal
var Chat: TAiOpenChat;
Chat := TAiOpenChat.Create(nil);
Chat.ApiKey := '@OPENAI_API_KEY';
Chat.Model  := 'gpt-image-1';
Chat.ModelCaps   := [];
Chat.SessionCaps := [cap_GenImage];
// Gap = [cap_GenImage] -> Run() calls InternalRunImageGeneration
```

**Important:** Assigning `ModelCaps` or `SessionCaps` directly sets `FNewSystemConfigured = True`, which prevents `EnsureNewSystemConfig` from overwriting these values with the automatic translation from legacy parameters.

### Reading the current caps

```pascal
// Read effective caps (after factory params have been applied)
var Gap: TAiCapabilities;
Gap := AiConnection.SessionCaps - AiConnection.ModelCaps;

if cap_GenAudio in Gap then
  ShowMessage('This session will generate audio via TTS');

if cap_Reasoning in AiConnection.ModelCaps then
  ShowMessage('This model has native extended reasoning');
```

---

## 7. Backward Compatibility with the Legacy System

### Legacy parameters

The previous system configured capabilities through four parameters:

| Legacy parameter | Type | Description |
|---|---|---|
| `NativeInputFiles` | `TAiFileCategories` | File types the model accepts natively |
| `NativeOutputFiles` | `TAiFileCategories` | File types the model generates |
| `ChatMediaSupports` | `TAiChatMediaSupports` | Native logical capabilities of the model |
| `EnabledFeatures` | `TAiChatMediaSupports` | Desired capabilities for the session |

### Automatic translation

If a model was configured with the legacy system and `ModelCaps`/`SessionCaps` were **not** assigned explicitly (`FNewSystemConfigured = False`), the method `EnsureNewSystemConfig` (called at the start of every `Run`) automatically translates:

```
ChatMediaSupports + NativeInputFiles/OutputFiles  →  ModelCaps
EnabledFeatures   + NativeOutputFiles             →  SessionCaps
```

This translation happens once per session and is invisible to the user. The legacy system continues to work without any code changes.

### Configuration priority

```
1. ModelCaps/SessionCaps assigned explicitly  (FNewSystemConfigured=True)  <- Highest priority
2. Automatic translation from legacy params   (FNewSystemConfigured=False)
3. Driver defaults (RegisterDefaultParams)                                  <- Lowest priority
```

### Migration guide

To migrate an existing driver to the new system:

```pascal
// Before (legacy)
TAiChatFactory.Instance.RegisterUserParam('MyDriver', 'ChatMediaSupports', 'Tcm_Image,Tcm_Pdf');
TAiChatFactory.Instance.RegisterUserParam('MyDriver', 'EnabledFeatures',   'Tcm_Image,Tcm_Pdf');
TAiChatFactory.Instance.RegisterUserParam('MyDriver', 'NativeInputFiles',  'Tfc_Image,Tfc_Pdf');

// After (new system v3.3)
TAiChatFactory.Instance.RegisterUserParam('MyDriver', 'ModelCaps',   '[cap_Image, cap_Pdf]');
TAiChatFactory.Instance.RegisterUserParam('MyDriver', 'SessionCaps', '[cap_Image, cap_Pdf]');
```

There is no need to remove the legacy parameters if the new ones are already configured. When `ModelCaps`/`SessionCaps` are present in the factory params, RTTI applies them through the setters that set `FNewSystemConfigured = True`, and the automatic translation is skipped.

---

## 8. Common Configuration Patterns

### Pattern 1: Plain text model (no special capabilities)

```pascal
// Text only, no tools
RegisterUserParam('Driver', 'ModelCaps',   '[]');
RegisterUserParam('Driver', 'SessionCaps', '[]');
RegisterUserParam('Driver', 'Tool_Active', 'False');
```

Result: `Gap = []` → direct `InternalRunCompletions` call.

### Pattern 2: Model with native vision

```pascal
// Model can process images directly via completions
RegisterUserParam('Driver', 'ModelCaps',   '[cap_Image]');
RegisterUserParam('Driver', 'SessionCaps', '[cap_Image]');
RegisterUserParam('Driver', 'Tool_Active', 'True');
```

Result: `Gap = []` → images go directly to the completions API.

### Pattern 3: Fully multimodal model (Gemini 2.5 Flash)

```pascal
RegisterUserParam('Gemini', 'gemini-2.5-flash', 'ModelCaps',
  '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
RegisterUserParam('Gemini', 'gemini-2.5-flash', 'SessionCaps',
  '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
```

Result: `Gap = []` → everything goes directly to Gemini's native completions.

### Pattern 4: Reasoning model (CoT/thinking)

```pascal
RegisterUserParam('Driver', 'reasoning-model', 'ModelCaps',    '[cap_Image, cap_Reasoning]');
RegisterUserParam('Driver', 'reasoning-model', 'SessionCaps',  '[cap_Image, cap_Reasoning]');
RegisterUserParam('Driver', 'reasoning-model', 'ThinkingLevel', 'tlMedium');
```

Result: the driver activates extended reasoning mode at the configured level.

### Pattern 5: TTS via dedicated endpoint

```pascal
// Empty ModelCaps = no completions / cannot understand inputs
// Gap = [cap_GenAudio] → InternalRunSpeechGeneration
RegisterUserParam('Driver', 'tts-model', 'ModelCaps',   '[]');
RegisterUserParam('Driver', 'tts-model', 'SessionCaps', '[cap_GenAudio]');
RegisterUserParam('Driver', 'tts-model', 'Tool_Active', 'False');
RegisterUserParam('Driver', 'tts-model', 'Voice',       'alloy');
```

### Pattern 6: Image generation via dedicated endpoint

```pascal
// Empty ModelCaps = uses image endpoint, not completions
// Gap = [cap_GenImage] → InternalRunImageGeneration
RegisterUserParam('Driver', 'image-model', 'ModelCaps',   '[]');
RegisterUserParam('Driver', 'image-model', 'SessionCaps', '[cap_GenImage]');
RegisterUserParam('Driver', 'image-model', 'Tool_Active', 'False');
```

### Pattern 7: NATIVE image generation via completions (Gemini)

```pascal
// cap_GenImage in BOTH ModelCaps AND SessionCaps = model returns image inline in the completions response
// Gap = [] → InternalRunCompletions (model generates image inline in the response)
RegisterUserParam('Gemini', 'gemini-2.5-flash-image', 'ModelCaps',   '[cap_Image, cap_GenImage]');
RegisterUserParam('Gemini', 'gemini-2.5-flash-image', 'SessionCaps', '[cap_Image, cap_GenImage]');
RegisterUserParam('Gemini', 'gemini-2.5-flash-image', 'Tool_Active', 'False');
```

Key difference from Pattern 6: here `cap_GenImage` is in **both** `ModelCaps` and `SessionCaps`, so there is no gap. Completions itself returns the image inline. In Pattern 6, `cap_GenImage` is only in `SessionCaps`, creating the gap that redirects the call to the dedicated image endpoint.

### Pattern 8: STT (transcription) via dedicated endpoint

```pascal
// cap_Audio in ModelCaps (supports audio) + Tool_Active=False (no tools)
// Use with ChatMode = cmTranscription
RegisterUserParam('Driver', 'whisper', 'ModelCaps',   '[cap_Audio]');
RegisterUserParam('Driver', 'whisper', 'SessionCaps', '[cap_Audio]');
RegisterUserParam('Driver', 'whisper', 'Tool_Active', 'False');
```

### Pattern 9: Vision bridge for a text-only model

```pascal
// The model (e.g. plain Ollama) has no native vision
// The user wants to send images → automatic bridge describes them before the prompt
RegisterUserParam('Ollama', 'ModelCaps',   '[]');          // cannot see images
RegisterUserParam('Ollama', 'SessionCaps', '[cap_Image]'); // session requires images
// Gap = [cap_Image] → in Phase 1, InternalRunImageDescription runs automatically
// The image is described in text and appended to the prompt
```

### Pattern 10: Video generation

```pascal
// ModelCaps=[cap_Image]: accepts image input (text-to-video or image-to-video)
// SessionCaps adds cap_GenVideo: Gap=[cap_GenVideo] → InternalRunImageVideoGeneration
RegisterUserParam('Gemini', 'aa_veo-3.0-generate-preview', 'ModelCaps',   '[cap_Image]');
RegisterUserParam('Gemini', 'aa_veo-3.0-generate-preview', 'SessionCaps', '[cap_Image, cap_GenVideo]');
RegisterUserParam('Gemini', 'aa_veo-3.0-generate-preview', 'Tool_Active', 'False');
```

---

## 9. Capability Reference by Provider

### OpenAI

| Model | ModelCaps | SessionCaps | Tool_Active | Notes |
|-------|-----------|-------------|-------------|-------|
| gpt-4.1 / 4.1-mini / 4.1-nano | `[cap_Image]` | `[cap_Image]` | True | 1M ctx, 32K output |
| gpt-4o / gpt-4o-mini | `[cap_Image]` | `[cap_Image]` | True | |
| o3 | `[cap_Image, cap_Reasoning]` | `[cap_Image, cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| o3-pro | `[cap_Image, cap_Reasoning]` | `[cap_Image, cap_Reasoning]` | True | ThinkingLevel=tlHigh |
| o4-mini | `[cap_Image, cap_Reasoning]` | `[cap_Image, cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| o3/o4-mini-deep-research | `[cap_Reasoning, cap_WebSearch, cap_CodeInterpreter]` | same | True | |
| gpt-4o-search-preview | `[cap_WebSearch]` | `[cap_WebSearch]` | False | |
| gpt-image-1 / dall-e-3 / dall-e-2 | `[]` | `[cap_GenImage]` | False | Gap → image endpoint |
| gpt-4o-mini-tts | `[]` | `[cap_GenAudio]` | False | Gap → TTS endpoint |
| gpt-4o-audio-preview | `[cap_Audio, cap_GenAudio]` | `[cap_Audio, cap_GenAudio]` | False | Native audio I/O in completions |
| gpt-4o-transcribe / mini-transcribe | `[cap_Audio]` | `[cap_Audio]` | False | Native STT |
| aa_gpt-4.1-pdf | `[cap_Image, cap_Pdf]` | `[cap_Image, cap_Pdf]` | True | Custom profile with native PDF |

### Gemini (Google)

| Model | ModelCaps | SessionCaps | Tool_Active | Notes |
|-------|-----------|-------------|-------------|-------|
| gemini-2.5-flash | `[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]` | same | True | 1M ctx, 65K output |
| gemini-2.5-flash-lite | `[cap_Image, cap_Audio, cap_Video, cap_Pdf]` | same | True | Budget/fast |
| gemini-2.5-pro | `[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]` | same | True | |
| gemini-3-pro-preview | same as 2.5-pro | same | True | ThinkingLevel=tlHigh |
| gemini-3.1-pro-preview | same | same | True | ThinkingLevel=tlHigh |
| gemini-2.5-flash-image | `[cap_Image, cap_GenImage]` | same | False | Native image gen in completions |
| gemini-3-pro-image-preview | `[cap_Image, cap_GenImage]` | same | False | No ThinkingLevel support |
| gemini-2.5-flash-preview-tts | `[]` | `[cap_GenAudio]` | False | Gap → TTS |
| gemini-2.5-pro-preview-tts | `[]` | `[cap_GenAudio]` | False | Gap → TTS |
| aa_veo-2.0/3.0/3.1 | `[cap_Image]` | `[cap_Image, cap_GenVideo]` | False | Gap=[cap_GenVideo] → video |

### Claude (Anthropic)

| Configuration | ModelCaps | SessionCaps | Tool_Active |
|---|---|---|---|
| Global (all models) | `[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]` | same | False |

All current Claude models (Opus 4.6, Sonnet 4.6/4.5, Haiku 4.5) share the same native capabilities: vision, PDF, reasoning, and web search.

### Groq

| Model | ModelCaps | SessionCaps | Tool_Active |
|-------|-----------|-------------|-------------|
| Global (default) | `[]` | `[]` | True |
| llama-3.1/3.3 | `[]` (inherits global) | `[]` | True |
| qwen/qwen-3-32b | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| deepseek-r1-distill-llama-70b | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| llama-4-scout / llama-4-maverick | `[cap_Image]` | `[cap_Image]` | True | |
| compound-beta / mini | `[cap_WebSearch, cap_CodeInterpreter]` | same | False | Native, no tool calls |
| whisper-large-v3 / turbo | `[cap_Audio]` | `[cap_Audio]` | False | STT |
| canopylabs/orpheus-v1-english | `[]` | `[cap_GenAudio]` | False | Gap → TTS |

### DeepSeek

| Model | ModelCaps | SessionCaps | Tool_Active | Notes |
|-------|-----------|-------------|-------------|-------|
| deepseek-chat | `[]` | `[]` | True | Text + tools, 128K ctx |
| deepseek-reasoner | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |

### Kimi (Moonshot AI)

| Model | ModelCaps | SessionCaps | Tool_Active |
|-------|-----------|-------------|-------------|
| kimi-k2 | `[]` | `[]` | True |
| kimi-k2.5 | `[cap_Image, cap_Pdf, cap_Reasoning]` | same | True |
| kimi-k2-thinking | `[cap_Reasoning]` | `[cap_Reasoning]` | True |
| moonshot-v1-* | `[]` | `[]` | False |
| moonshot-v1-*-vision | `[cap_Image]` | `[cap_Image]` | False |

### xAI Grok

| Model | ModelCaps | SessionCaps | Tool_Active |
|-------|-----------|-------------|-------------|
| grok-3 | `[]` | `[]` | True |
| grok-3-mini | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlLow |
| grok-4-fast-reasoning | `[cap_Image, cap_Reasoning]` | same | True | 2M ctx |
| grok-2-image-1212 / grok-imagine-* | `[]` | `[cap_GenImage]` | False | Gap → image |
| grok-imagine-video | `[]` | `[cap_GenVideo]` | False | Gap → video |

### Mistral

| Configuration | ModelCaps | SessionCaps | Tool_Active |
|---|---|---|---|
| Global (default) | `[cap_Image]` | `[cap_Image]` | True |
| magistral-medium/small | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| devstral-latest | `[]` | `[cap_Pdf, cap_Image]` | True | No native vision |
| voxtral-mini/small | `[cap_Audio]` | `[cap_Audio]` | False | STT via completions |
| mistral-ocr-latest | `[cap_Pdf]` | `[cap_Pdf]` | False | OCR via /v1/ocr |

### Cohere

| Model | ModelCaps | SessionCaps | Tool_Active |
|-------|-----------|-------------|-------------|
| command-a-03-2025 | `[]` | `[]` | True |
| command-a-reasoning-08-2025 | `[cap_Reasoning]` | `[cap_Reasoning]` | True |
| command-a-vision-07-2025 | `[cap_Image]` | `[cap_Image]` | False |
| c4ai-aya-vision-8b/32b | `[cap_Image]` | `[cap_Image]` | False |

### Ollama (local models)

| Configuration | ModelCaps | SessionCaps | Tool_Active |
|---|---|---|---|
| Global (default) | `[]` | `[]` | False |
| llama3.3 / qwen2.5 | `[]` | `[]` | True |
| qwen3:latest | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| deepseek-r1:latest | `[cap_Reasoning]` | `[cap_Reasoning]` | False | ThinkingLevel=tlMedium |
| llama3.2-vision | `[cap_Image]` | `[cap_Image]` | False | |
| qwen2.5vl | `[cap_Image]` | `[cap_Image]` | True | |
| gemma3:1b/4b/12b/27b | `[cap_Image]` | `[cap_Image]` | True | |

---

## 10. Adding a New Provider

When creating a new driver (`TAi[Provider]Chat` inheriting from `TAiChat`), configure capabilities in `uMakerAi.Chat.Initializations.pas`:

```pascal
// 1. Configure global defaults for the provider
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'Max_Tokens',  '16000');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'Tool_Active', 'True');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'ModelCaps',   '[cap_Image]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'SessionCaps', '[cap_Image]');

// 2. Per-model overrides
// Basic text-only model
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model-basic', 'ModelCaps',   '[]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model-basic', 'SessionCaps', '[]');

// Reasoning model
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model-think', 'ModelCaps',    '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model-think', 'SessionCaps',  '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model-think', 'ThinkingLevel', 'tlMedium');

// TTS model
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model-tts', 'ModelCaps',   '[]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model-tts', 'SessionCaps', '[cap_GenAudio]');
TAiChatFactory.Instance.RegisterUserParam('MyProvider', 'my-model-tts', 'Tool_Active', 'False');
```

### Design questions when configuring a new model

1. **What input types does the completions endpoint accept?**
   → Those are the input `ModelCaps` (`cap_Image`, `cap_Audio`, etc.)

2. **Can the completions endpoint generate images/audio/video inline?**
   → Add the corresponding `cap_Gen*` to both `ModelCaps` and `SessionCaps`. Gap = 0, call goes directly.

3. **Does the model have a separate TTS/image/video endpoint?**
   → `ModelCaps = []`, `SessionCaps = [cap_Gen*]`. The gap activates the bridge.

4. **Does the model support extended reasoning?**
   → `cap_Reasoning` in both `ModelCaps` and `SessionCaps`, plus `ThinkingLevel`.

5. **Does the model support tool calling?**
   → `Tool_Active = True`.

---

## 11. Source File Reference

| Purpose | File |
|---------|------|
| Definition of `TAiCapability` and `TAiCapabilities` | `Source/Core/uMakerAi.Core.pas` (line 65) |
| Declaration of `ModelCaps`/`SessionCaps` on `TAiChat` | `Source/Core/uMakerAi.Chat.pas` (line 430) |
| Setter implementation and legacy sync | `Source/Core/uMakerAi.Chat.pas` (line 2886) |
| Gap analysis and orchestration phases (`RunNew`) | `Source/Core/uMakerAi.Chat.pas` (line 3004) |
| Params applied via RTTI (`ApplyParamsToChat`) | `Source/Chat/uMakerAi.Chat.AiConnection.pas` (line 509) |
| Per-provider model configuration | `Source/Chat/uMakerAi.Chat.Initializations.pas` |

---

*Documentation for MakerAI v3.3 — March 2026*
*Official project website: https://makerai.cimamaker.com*
