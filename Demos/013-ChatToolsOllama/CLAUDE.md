# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Demo 013 - Ollama ChatTools (gemma3:4b)**

FMX demo showcasing the full ChatTools bridge system with Ollama as the primary LLM. Uses `gemma3:4b` (local model via Ollama) with 6 operational modes that demonstrate automatic capability bridging when the model lacks native support for a feature.

## Build

Open `ChatToolsOllamaDemo.dproj` in RAD Studio and press F9.

**Requirements:**
- Ollama running on `localhost:11434` with `gemma3:4b` pulled (`ollama pull gemma3:4b`)
- `GEMINI_API_KEY` env var → Modes 2 (WebSearch) and 5 (TTS)
- `OPENAI_API_KEY` env var → Mode 4 (Image generation via DALL-E)
- For Mode 3 (PDF): Ollama model for OCR + Ghostscript (`gswin64c.exe`)

## Architecture

**Single form:** `TForm26` in `uMainChatToolsOllamaDemo.pas` / `.fmx`

**Key components (drop on form):**
- `TAiOllamaChat` — main LLM driver (gemma3:4b)
- `TAiGeminiWebSearchTool` — WebSearch bridge (Gemini grounding)
- `TAiGeminiSpeechTool` — TTS bridge (Gemini Speech)
- `TAiOllamaPdfTool` — PDF bridge (Ollama OCR + Ghostscript)
- `TAiDalleImageTool` — Image generation bridge (DALL-E)
- `TAiSoraVideoTool` — Video bridge (not used in this demo)
- `TAiFunctions` — created programmatically in `InicializarFunciones`

## 6 Operational Modes

| Mode | SessionCaps | Bridge Activated | Description |
|------|-------------|-----------------|-------------|
| 0: Chat + Funciones | `[]` + `Tool_Active=True` | TAiFunctions | Function calling with 3 tools |
| 1: Vision | `[cap_Image]` | None (native) | gemma3:4b analyzes images |
| 2: Web Search | `[cap_WebSearch]` | AiGeminiWebSearchTool | Gemini grounding, gemma3:4b answers |
| 3: PDF | `[cap_Pdf]` | AiOllamaPdfTool | Ollama OCR, gemma3:4b analyzes |
| 4: Generar Imagen | `[cap_GenImage]` | AiDalleImageTool | DALL-E generates, shown in Image tab |
| 5: TTS | `[cap_GenAudio]` | AiGeminiSpeechTool | Gemini TTS, played via TAudioPushStream |

## TAiFunctions Tools (Mode 0)

Three functions registered programmatically:
- `get_datetime` — returns current date/time (optional `format` param)
- `calculate` — evaluates `A op B` expressions (+, -, *, /)
- `get_weather` — simulated weather for a city (demo only)

## Key Implementation Patterns

### Capability Gap System
```pascal
// Mode 2: gap cap_WebSearch triggers AiGeminiWebSearchTool bridge
AiOllamaChat1.ModelCaps := [];        // gemma3:4b can't search the web
AiOllamaChat1.SessionCaps := [cap_WebSearch];  // gap → bridge activates
```

### TAiFunctions Programmatic Setup
```pascal
LFn := FAiFunctions.Functions.AddFunction('get_datetime', True, OnFnGetDateTime);
LFn.Description.Text := 'Description...';
LParam := LFn.Parameters.Add;
LParam.Name := 'format';
LParam.ParamType := ptString;
LParam.Required := False;
AiOllamaChat1.AiFunctions := FAiFunctions;
```

### Audio Playback (TAudioPushStream)
```pascal
FAudio := TAudioPushStream.Create;
// In ReceiveDataEnd, for Tfc_Audio media files:
MF.Content.Position := 0;
FAudio.PushWavData(MF.Content);  // Auto-detects WAV format, auto-starts
```

### Streaming
Responses stream via `OnReceiveData` → `MemoTexto`. Final text in `OnReceiveDataEnd` → `MemoChatList`.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
