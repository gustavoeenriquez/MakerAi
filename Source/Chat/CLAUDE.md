# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The Chat module contains LLM provider drivers for the MakerAI framework. Each driver implements provider-specific API communication, message formatting, streaming, and tool calling for a different LLM service.

## Module Structure

### Universal Connector
- `uMakerAi.Chat.AiConnection.pas` - `TAiChatConnection` component that abstracts all provider differences. Set `DriverName` property to switch providers without code changes. Manages tool integration (Shell, TextEditor, ComputerUse, Speech, Video, Vision, WebSearch, Image).

### Provider Drivers
Each inherits from `TAiChat` (defined in Core):

| File | Class | Provider |
|------|-------|----------|
| `uMakerAi.Chat.OpenAi.pas` | `TAiOpenChat` | OpenAI (GPT-5.5, GPT-5.4, GPT-5.4-mini — familia GPT-5.x) |
| `uMakerAi.Chat.Claude.pas` | `TAiClaudeChat` | Anthropic Claude (claude-opus-4-7, claude-sonnet-4-6, claude-haiku-4-5) |
| `uMakerAi.Chat.Gemini.pas` | `TAiGeminiChat` | Google Gemini (gemini-3.1-pro, gemini-3-flash, gemini-3.1-flash-lite — familia 3.x) |
| `uMakerAi.Chat.Ollama.pas` | `TAiOllamaChat` | Ollama (local models) |
| `uMakerAi.Chat.LMStudio.pas` | `TAiLMStudioChat` | LM Studio (local OpenAI-compatible) |
| `uMakerAi.Chat.Groq.pas` | `TAiGroqChat` | Groq inference (llama, qwen, deepseek, voxtral) |
| `uMakerAi.Chat.DeepSeek.pas` | `TAiDeepSeekChat` | DeepSeek (deepseek-v4-flash, deepseek-v4-pro) |
| `uMakerAi.Chat.Mistral.pas` | `TAiMistralChat` | Mistral (large, magistral, devstral, voxtral) |
| `uMakerAi.Chat.Kimi.pas` | `TAiKimiChat` | Kimi/Moonshot (kimi-k3, kimi-k2.6/k2.7) |
| `uMakerAi.Chat.Grok.pas` | `TAiGrokChat` | xAI Grok (grok-4.3, grok-4.5, grok-build) |
| `uMakerAi.Chat.Cohere.pas` | `TCohereChat` | Cohere (command-a, aya-vision) |
| `uMakerAi.Chat.GenericLLM.pas` | `TAiGenericChat` | Any OpenAI-compatible API |

### Configuration
- `uMakerAi.Chat.Initializations.pas` - Driver registration and model capabilities. Uses `TAiChatFactory` to configure `ModelCaps`, `SessionCaps`, `Tool_Active`, `ThinkingLevel` per model. **Última actualización: May 2026.**

### Legacy (Deprecated)
- `uMakerAi.Chat.OpenAi_Deprecated.pas`
- `uMakerAi.Chat.Ollama_old.pas`, `uMakerAi.Chat.Ollama_old1.pas`
- `uMakerAi.Chat.Claude_beta.pas`

---

## Capability System (v3.3 — nuevo)

### Concepto central
Dos propiedades de tipo `TAiCapabilities` (set de `TAiCapability`) reemplazan los cuatro params legacy:

| Propiedad | Descripción | Sincroniza a (legacy) |
|-----------|-------------|----------------------|
| `ModelCaps` | Capacidades nativas del modelo vía completions | `NativeInputFiles` + `ChatMediaSupports` |
| `SessionCaps` | Capacidades deseadas en la sesión | `NativeOutputFiles` + `EnabledFeatures` |

**Gap = SessionCaps − ModelCaps** → determina qué bridge/tool activa el orquestador interno automáticamente.

### TAiCapability enum
```delphi
TAiCapability = (
  // Entrada / Comprensión
  cap_Image, cap_Audio, cap_Video, cap_Pdf,
  cap_WebSearch, cap_Reasoning, cap_CodeInterpreter,
  cap_Memory, cap_TextEditor, cap_ComputerUse, cap_Shell,
  // Salida / Generación (gap → activa bridge)
  cap_GenImage, cap_GenAudio, cap_GenVideo, cap_GenReport, cap_ExtractCode
);
```

### Patrones de configuración frecuentes

```delphi
// Modelo de texto + tools (default para la mayoría)
RegisterUserParam('Driver', 'ModelCaps',   '[]');
RegisterUserParam('Driver', 'SessionCaps', '[]');

// Modelo con visión nativa
RegisterUserParam('Driver', 'ModelCaps',   '[cap_Image]');
RegisterUserParam('Driver', 'SessionCaps', '[cap_Image]');

// Modelo con reasoning
RegisterUserParam('Driver', Model, 'ModelCaps',    '[cap_Reasoning]');
RegisterUserParam('Driver', Model, 'SessionCaps',  '[cap_Reasoning]');
RegisterUserParam('Driver', Model, 'ThinkingLevel', 'tlMedium');

// TTS via endpoint dedicado (Gap=[cap_GenAudio] → InternalRunSpeechGeneration)
RegisterUserParam('Driver', Model, 'ModelCaps',   '[]');
RegisterUserParam('Driver', Model, 'SessionCaps', '[cap_GenAudio]');
RegisterUserParam('Driver', Model, 'Tool_Active', 'False');

// Generación de imagen via endpoint dedicado (Gap=[cap_GenImage])
RegisterUserParam('Driver', Model, 'ModelCaps',   '[]');
RegisterUserParam('Driver', Model, 'SessionCaps', '[cap_GenImage]');
RegisterUserParam('Driver', Model, 'Tool_Active', 'False');
```

### Compatibilidad con sistema legacy
`EnsureNewSystemConfig` traduce automáticamente los params legacy (`NativeInputFiles`, `ChatMediaSupports`, `EnabledFeatures`, `NativeOutputFiles`) al nuevo sistema en el primer `Run`. Los modelos con configuración antigua siguen funcionando sin cambios. Si se asignan `ModelCaps`/`SessionCaps` explícitamente, `FNewSystemConfigured=True` y la traducción automática se omite.

---

## Key Patterns

### Driver Implementation
All drivers implement these core methods:
- `Run()` - Execute chat completion (sync or async). Calls `EnsureNewSystemConfig`, aplica el sanitizador de prompts, luego delega al orquestador privado `RunNew()`. Cadena completa: `AddMessageAndRun → Run → RunNew`.
- `GetMessages()` - Serialize message history to provider-specific JSON format
- `ParseChat()` - Parse provider response into `TAiChatMessage`
- `GetModels()` - Retrieve available models from API

Streaming drivers additionally implement:
- `ProcessStreamChunk()` - Handle SSE/streaming response chunks
- `OnInternalReceiveData` / `OnInternalReceiveDataEnd` - Event callbacks

### Parameter Registry
Model capabilities are configured via `TAiChatFactory.Instance.RegisterUserParam()`:
```delphi
// Global driver defaults
TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Max_Tokens', '8000');
TAiChatFactory.Instance.RegisterUserParam('Ollama', 'ModelCaps',  '[]');
TAiChatFactory.Instance.RegisterUserParam('Ollama', 'SessionCaps','[]');

// Model-specific overrides
TAiChatFactory.Instance.RegisterUserParam('Ollama', 'qwen3:latest', 'ModelCaps',    '[cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('Ollama', 'qwen3:latest', 'ThinkingLevel', 'tlMedium');
```

Key parameters:
- `ModelCaps` / `SessionCaps` — **sistema nuevo v3.3** (preferido)
- `NativeInputFiles` / `NativeOutputFiles` — físico, legacy (aún soportado)
- `ChatMediaSupports` / `EnabledFeatures` — lógico, legacy (aún soportado)
- `Tool_Active` — habilita function calling
- `ThinkingLevel` — nivel de razonamiento (`tlLow`, `tlMedium`, `tlHigh`)
- `Max_Tokens` — tokens máximos de respuesta
- `Temperature` — temperatura de sampling

### Chat State Machine
Drivers follow states in `TAiChatState`:
```text
acsIdle → acsConnecting → acsReasoning → acsWriting → acsToolCalling → acsFinished/acsError
```

### Tool Calling Flow
1. Message sent with tool definitions
2. Model responds with `tool_use` block
3. `OnCallToolFunction` event fires
4. Tool result appended to messages
5. `Run()` called again for final response

---

## Dependencies

- `uMakerAi.Core.pas` - Base types (`TAiMediaFile`, `TAiFileCategory`, `TAiChatState`, `TAiCapability`)
- `uMakerAi.Chat.pas` - Abstract `TAiChat` base class (`ModelCaps`, `SessionCaps`). API pública: `AddMessageAndRun`, `Run`. `RunNew` es privado (orquestador interno de las 3 fases).
- `uMakerAi.Chat.Messages.pas` - `TAiChatMessage`, `TAiChatMessages`
- `uMakerAi.Tools.Functions.pas` - `TAiFunctions`, `TAiToolsFunction`
- `uMakerAi.ParamsRegistry.pas` - `TAiChatFactory` for model configuration

---

## Adding a New Driver

1. Create `uMakerAi.Chat.NewProvider.pas`
2. Define class inheriting from `TAiChat`
3. Implement `Run()`, `GetMessages()`, `ParseChat()`, `GetModels()`
4. Register in `uMakerAi.Chat.Initializations.pas` initialization section
5. Configure capabilities via `TAiChatFactory.Instance.RegisterUserParam()`:
   ```delphi
   TAiChatFactory.Instance.RegisterUserParam('NewProvider', 'Max_Tokens',  '16000');
   TAiChatFactory.Instance.RegisterUserParam('NewProvider', 'Tool_Active', 'True');
   TAiChatFactory.Instance.RegisterUserParam('NewProvider', 'ModelCaps',   '[cap_Image]');
   TAiChatFactory.Instance.RegisterUserParam('NewProvider', 'SessionCaps', '[cap_Image]');
   ```

---

## Provider-Specific Notes

### Claude (Anthropic)
- `x-anthropic-version` header + beta features via dynamic headers
- **Thinking por familia (ago 2026, probado runtime):** el driver clasifica el modelo con `IsClaudeAdaptiveOnly` (4.7/4.8/5: budget_tokens y temperature/top_p/top_k devuelven 400) e `IsClaude46` (4.6). En 4.6+ envía `thinking:{type:"adaptive"}` y mapea `ThinkingLevel` → `output_config.effort` (low/medium/high); en ≤4.5 mantiene `{enabled, budget_tokens}`. El header `interleaved-thinking` solo se envía en el camino legacy.
- `output_format` migrado a `output_config.format` (deprecado API-wide); format y effort comparten el mismo objeto `output_config`
- Web search: `web_search_20260209` (filtrado dinámico) en 4.6+; `web_search_20250305` en legacy
- `stop_reason:"refusal"` (clasificadores de opus-5/fable-5): marca `IsRefusal`, parsea `stop_details` (category/explanation) y dispara `OnError`
- **Fase ago 2026 (probada runtime salvo Fast mode):**
  - `FastMode` — `speed:"fast"` + beta `fast-mode-2026-02-01`, solo opus-5/4.8 (en otros se ignora con log). OJO: research preview con rate limit propio; requiere cupo del org (la org de prueba tiene 0 TPM asignados → 429)
  - Mensajes `{role:"system"}` mid-conversation en el historial (preservan el prompt cache): pasan directo en opus-5/4.8/fable/mythos; en modelos sin soporte (sonnet-5, 4.6…) se degradan automáticamente a turno `user` envuelto en `<system-reminder>`. Uso: `AddMessage(texto, 'system')` tras un turno user + `Run(nil)`
  - `EnableCompaction` — beta `compact-2026-01-12` + `context_management.edits[compact_20260112]` (se fusiona con `FContextConfig` si existe); los bloques `compaction` recibidos se preservan (`FCompactionBlocks`) y se reenvían íntegros al inicio del mensaje assistant correspondiente
  - `RefusalFallbackModel` — beta `server-side-fallback-2026-06-01` + `fallbacks:[{model}]`: ante un refusal el API reintenta en ese modelo en la misma llamada (único target soportado hoy: `claude-opus-4-8`)
- Citations (RAG nativo): soporte parcial implementado

**Modelos activos (ago 2026, todos registrados):**
- `claude-opus-5` — **RECOMENDADO**, sucesor de 4.8 al mismo precio ($5/$25), thinking activo por defecto, 1M ctx / 128K out
- `claude-sonnet-5` — mejor precio/calidad, casi-Opus en código/agentes ($3/$15; intro $2/$10 hasta ago 31/2026); tokenizer nuevo ~30% más tokens que 4.6. PROBADO runtime
- `claude-fable-5` — tope de capacidad ($10/$50); **requiere retención de datos 30 días** (ZDR → 400 en toda petición); thinking siempre activo
- `claude-opus-4-8`, `claude-opus-4-7` — generaciones Opus 4.x (misma superficie adaptive-only)
- `claude-sonnet-4-6`, `claude-opus-4-6` — generación anterior (adaptive recomendado, budget deprecado). Opus 4.6 PROBADO runtime con adaptive
- `claude-haiku-4-5-20251001` — velocidad/costo, 200K ctx; camino legacy budget PROBADO runtime

**Deprecados:** `claude-opus-4-1` (retira 5 ago 2026 → opus-5); `claude-sonnet-4-20250514` / `claude-opus-4-20250514` (TBD)

### OpenAI
**Familia GPT-5.6 (julio 2026 — producción actual):** 1.05M contexto, 128K output, visión + reasoning + tools + prompt caching en toda la familia. El alias `gpt-5.6` enruta a Sol.
- `gpt-5.6-sol` — Flagship. `ModelCaps=[cap_Image, cap_Reasoning]`, `ThinkingLevel=tlHigh`
- `gpt-5.6-terra` — Balance costo/capacidad. `ThinkingLevel=tlMedium`
- `gpt-5.6-luna` — Tier económico (probado runtime ago 2026). `ThinkingLevel=tlLow`
- Precios jul 30/2026: Luna −80%, Terra −20%; "Fast mode" reemplaza Priority Processing (2.5× velocidad, 2× precio, solo Sol)

**Familia GPT-5.x (mayo 2026):**
- `gpt-5.4` — Producción estándar, visión + tools, 1M contexto. `ModelCaps=[cap_Image]`
- `gpt-5.4-mini` — Rápido y económico, visión + tools. `ModelCaps=[cap_Image]`
- `gpt-5.5` — visión + reasoning. `ModelCaps=[cap_Image, cap_Reasoning]`, `ThinkingLevel=tlMedium`
- `gpt-5.5-pro` — Reasoning intensivo. `ModelCaps=[cap_Image, cap_Reasoning]`, `ThinkingLevel=tlHigh`

**Capacidades multimedia (sin cambios en nombres de modelos):**
- Generación de imagen: `gpt-image-1` (y `gpt-image-1.5` / `gpt-image-2`) → `SessionCaps=[cap_GenImage]`
- TTS: `gpt-4o-mini-tts` → `SessionCaps=[cap_GenAudio]`
- Transcripción: `gpt-transcribe` (recomendado, WER 8.98%), `gpt-live-transcribe` (vivo/Realtime), `gpt-4o-transcribe`, `gpt-4o-mini-transcribe`, `whisper-1` → `ModelCaps=[cap_Audio]`
- Video: Sora → `SessionCaps=[cap_GenVideo]`
- Web search: `gpt-4o-search-preview` → `ModelCaps=[cap_WebSearch]`, `Tool_Active=False`

**Deprecated (feb 2026):** GPT-4.1, GPT-4o (chat), o3, o4-mini — mantenidos en el código por backward compatibility pero no recomendados para proyectos nuevos.

### Gemini (Google)

**Familia 3.5/3.6 — actuales (jul 2026, registrados SIN prueba runtime — falta API key):**
- `gemini-3.5-flash` (GA may 19/2026, alias `gemini-flash-latest`) — flash flagship, frontier agentic/coding; Computer Use tool en public preview para este modelo. `ThinkingLevel=tlMedium`
- `gemini-3.6-flash` (GA jul 21/2026) — mejor eficiencia de tokens y planificación agéntica, más barato que 3.5 Flash. `ThinkingLevel=tlMedium`
- `gemini-3.5-flash-lite` (GA jul 21/2026) — baja latencia, subagentes. `ThinkingLevel=tlLow`
- **Sampling params deprecados en 3.5+/3.6/omni** (jul 21/2026): el driver omite `temperature`/`topP` automáticamente para esos modelos
- `gemini-3.5-pro` NO existe aún (anunciado en I/O, retrasado — Reuters jul 16)

**Familia 3.x anterior (siguen activos):**
- `gemini-3.1-pro-preview` — flagship pro, 2M contexto. `ThinkingLevel=tlHigh`
- `gemini-3-flash-preview`, `gemini-3.1-flash-lite` — generación previa

**Modelos especializados:**
- Imagen (familia **Nano Banana**, GA may-jun 2026): `gemini-3.1-flash-image` (NB 2, con video-to-image), `gemini-3-pro-image` (NB Pro), `gemini-3.1-flash-lite-image` (NB 2 Lite, ultra-rápido). Los `-preview` previos siguen registrados
- `gemini-3.1-flash-tts-preview` — TTS; desde jun 17/2026 soporta streaming (`streamGenerateContent`)
- `veo-3.1-generate-preview` — video. **veo-2.0 y veo-3.0 APAGADOS el 30 jun 2026** (enum del tool conservado por compat de DFMs)
- `gemini-omni-flash-preview` — video 3-10s 720p (preview jun 30/2026)
- `gemini-embedding-2` (GA abr 2026) — embedding multimodal, 3072 dims

**Apagados/deprecados:**
- **Imagen 4.0 (`imagen-4.0-*-generate-001`): SHUTDOWN 17 ago 2026**
- `gemini-2.5-flash`, `gemini-2.5-pro` — cerrados 17 jun 2026

**Otros:**
- Grounding nativo: el driver gestiona `groundingSupports` automáticamente

### Groq (inferencia rápida)
**Actualizado ago 2026, probado runtime 4/4.** Dos sistemas de reasoning MUTUAMENTE excluyentes (gating por prefijo en el driver): `openai/gpt-oss-*` usa `include_reasoning` + `reasoning_effort` low/medium/high; `qwen/*` usa `reasoning_format` parsed/raw/hidden + `reasoning_effort` default/none (sin `parsed` el `<think>` llega crudo en content).
- Texto: `llama-3.1-8b-instant` (default del driver), `llama-3.3-70b-versatile`
- Reasoning: `openai/gpt-oss-120b/20b` (probado), `qwen/qwen3.6-27b` (nuevo ago 2026, reemplaza a qwen3-32b — alias registrado; probado)
- **Groq NO tiene visión actualmente**: llama-4-scout/maverick retirados y gpt-oss-120b es solo texto ("content must be a string" con imágenes — verificado; cap_Image eliminado del registry)
- RETIRADOS ago 2026: `qwen/qwen3-32b`, `llama-4-scout`, `moonshotai/kimi-k2-instruct(-0905)`
- Agénticos: `groq/compound`/`-mini` (web search + code execution, `Tool_Active=False`; aliases compound-beta)
- TTS: `canopylabs/orpheus-v1-english`/`-arabic-saudi` → `SessionCaps=[cap_GenAudio]` (playai-tts eliminado 12/31/25)
- STT: `whisper-large-v3/turbo` → `ModelCaps=[cap_Audio]`, `Tool_Active=False`
- Árabe: `allam-2-7b` (4K ctx, sin tools); prompt-guard-2 son clasificadores, no chat

### Mistral
**Modelos activos (jun 2026):**
- `mistral-large-latest` → Large 3 (v25.12), 256K ctx, vision + tools. Hereda defaults `[cap_Image]`
- `mistral-medium-latest` → **Medium 3.5** (v26.04), 256K ctx, vision + reasoning_effort + tools. `ModelCaps=[cap_Image,cap_Reasoning]`, `ThinkingLevel=tlMedium`
- `mistral-small-latest` → **Small 4** (v26.03), 256K ctx, vision + reasoning_effort + tools. `ModelCaps=[cap_Image,cap_Reasoning]`, `ThinkingLevel=tlMedium`
- Ministral: `ministral-14b/8b/3b-latest` → 262K ctx, vision + tools. Hereda defaults
- Reasoning dedicado: `magistral-medium/small-latest` → `ModelCaps=[cap_Reasoning]`, `ThinkingLevel=tlMedium` (usa `prompt_mode: 'reasoning'`)
- Código: `devstral-latest` / `devstral-small-latest` → `ModelCaps=[]` (sin visión)
- STT: `voxtral-mini/small-latest` → `ModelCaps=[cap_Audio]`, `Tool_Active=False`
- **TTS: `voxtral-mini-tts-2603`** (mar 2026, PROBADO runtime ago 2026) → `SessionCaps=[cap_GenAudio]` activa `InternalRunNativeSpeechGeneration` (POST `/v1/audio/speech`). El API **exige** `voice` (slug del catálogo `GET /v1/audio/voices`: `en_paul_neutral`/happy/sad…, `gb_oliver_neutral`, `gb_jane_sarcasm`) o `ref_audio`; propiedades `TtsVoice` (default `en_paul_neutral`) y `TtsFormat` (mp3/wav/pcm/flac/opus). Modelo multilingüe con cualquier voz
- OCR: `mistral-ocr-latest` → endpoint `/v1/ocr`, `SessionCaps=[cap_Pdf]`, `Tool_Active=False`. **OCR 4** (`mistral-ocr-4-0`, jun 2026): el alias ya apunta a él; nueva propiedad `OcrIncludeBlocks` (bloques estructurales con bounding boxes por página) y `pages` acepta rangos (`"0-5"`)
- Devstral 2 (`devstral-2512`) y Magistral 1.2 (`magistral-*-2509`) cubiertos por los alias `-latest`

**Reasoning en el driver:**
- Magistral → `prompt_mode: 'reasoning'` (chain-of-thought visible en respuesta)
- Small 4 / Medium 3.5 → `reasoning_effort: 'low'|'medium'|'high'` según `ThinkingLevel`

### xAI Grok
**Actualizado ago 2026, probado runtime 6/6.** Recambio total del catálogo: la familia actual (grok-4.x, grok-build) **razona siempre** (`reasoning_content` capturado por la base) y NO acepta `frequency/presence/stop` ni `reasoning_effort` (gate por prefijo en el driver); logprobs no soportado en 4.20+.
- `grok-4.3` [default del driver]: 1M ctx, visión + reasoning (probados). $1.25/$2.50 por M (<200K; 2x sobre 200K)
- `grok-4.5` [premium]: 500K ctx, visión + reasoning (probados). $2/$6 por M
- `grok-4.20-0309-reasoning`/`-non-reasoning`/`-multi-agent-0309`: 1M ctx
- `grok-build-0.1`: coding con reasoning (probado), 256K ctx. $1/$2 por M
- Imagen: `grok-imagine-image` ($0.02) / `-image-quality` ($0.05) → `SessionCaps=[cap_GenImage]`
- Video: `grok-imagine-video` ($0.05/s) / `-video-1.5` ($0.08/s) → `SessionCaps=[cap_GenVideo]`
- **RETIRADOS ago 2026**: familia grok-3 completa, grok-4-0709, grok-4-fast-*, grok-4-1*, grok-code-fast-1, grok-2-vision, grok-2-image, grok-imagine-image-pro. Aliases registrados: grok-3/grok-4/grok-4-0709 → grok-4.3; grok-code-fast-1 → grok-build-0.1; grok-2-image(-1212) → grok-imagine-image; -image-pro → -image-quality (probado vía alias)

### DeepSeek
**Actualizado ago 2026, probado runtime 4/4 (incl. tools en modo thinking).** V4 (abr 2026) son los únicos modelos en `/v1/models`: 1M ctx / 384K output. El API activa thinking **por defecto** (effort=high); el driver lo controla explícitamente en `InitChatCompletions`: `cap_Reasoning` en `ModelCaps` → `thinking:{type:enabled}` + `reasoning_effort` (tlLow=low, tlMedium=high, tlHigh=max); sin el cap → `thinking:{type:disabled}` (modo rápido/económico). En modo thinking el API ignora temperature/top_p/penalties sin error. Con tools, `reasoning_content` DEBE reenviarse en el historial (400 si falta) — el override `GetMessages` del driver ya lo hace.
- `deepseek-v4-flash` [default del driver]: 284B (13B act). $0.14/M in miss / $0.0028 hit / $0.28/M out
- `deepseek-v4-pro`: 1.6T (49B act), razonamiento por defecto vía registry. $0.435/M in / $0.87/M out. Nota: effort low→high (mínimo soportado, ago 2026)
- `deepseek-chat` y `deepseek-reasoner`: **RETIRADOS oficialmente 24 jul 2026** — aún enrutan a v4-flash (no-thinking/thinking) en gracia; no depender de ellos
- Sin visión en la API pública
- OJO: pricing pico 2x anunciado (9:00-12:00 y 14:00-18:00 UTC+8)

### Kimi (Moonshot AI)
**Actualizado ago 2026, probado runtime 4/4.** REGLA CRÍTICA descubierta empíricamente: la familia nueva (k3/k2.6/k2.7) devuelve **400 si el request incluye `top_p`** (solo acepta `temperature`) — el default global `top_p` se eliminó del registry y el constructor fija `Top_p := 0`. Todos los modelos nuevos devuelven `reasoning_content` y necesitan `max_tokens` amplio (con presupuesto corto el razonamiento lo consume y `content` llega vacío con `finish=length`).
- `kimi-k3` (jul 16/2026): **flagship y default del driver**, 1M ctx, visión + reasoning (probado). Precio plano: $3/M input, $0.30/M cache-hit, $15/M output
- `kimi-k2.7-code` / `-highspeed`: coding multimodal (probado — genera Delphi correcto)
- `kimi-k2.6`: visión + texto + tools (probado)
- `kimi-latest`: alias móvil
- `kimi-k2.5`: **RETIRA 31 ago 2026** → migrar a kimi-k3
- `kimi-k2` y `kimi-k2-thinking`: **YA RETIRADOS** del API (entradas eliminadas del registry)
- `moonshot-v1-*` (+vision-preview): **SUNSET TOTAL 31 ago 2026**

### Cohere
**Actualizado ago 2026, probado runtime 5/5 (incl. tools).** Los modelos nuevos (a-plus, north, a-reasoning) razonan por defecto: el content trae bloque `type:'thinking'` antes del `text`. El driver lo controla en `InitChatCompletions`: `cap_Reasoning` → `thinking:{enabled}`; sin el cap → `disabled` (modo rápido), EXCEPTO command-a-plus que **no permite disabled** (falla con `INVALID_TOOL_GENERATION`). `ParseChat` y streaming capturan el thinking a `ReasoningContent` / `OnReceiveThinking`. FIX ago 2026: el retorno síncrono con tool calling llegaba vacío — `ExecuteAndRespondToToolCalls` ahora reutiliza el mismo `ResMsg` en el round 2 (patrón de la base `Run(Nil, ResMsg)`).
- `command-a-plus-05-2026` [FLAGSHIP]: MoE 218B/25B, Apache 2.0, 436K ctx, visión + reasoning siempre activo (probados). $2.5/$10 por M
- `north-mini-code-1-0`: coding, 436K ctx, razona por defecto (driver manda disabled sin cap_Reasoning; probado)
- `command-a-03-2025`: texto + tools (288K ctx, default del driver)
- `command-a-reasoning-08-2025`: reasoning + tools
- `command-a-vision-07-2025`: visión, **sin tools** (`Tool_Active=False`)
- `command-a-translate-08-2025`: traducción especializada, sin tools
- `c4ai-aya-expanse-32b` / `c4ai-aya-vision-32b`: multilingual, sin tools (los 8b YA NO están en el API)
- `tiny-aya-global/earth/fire/water`: ligeros 8K ctx, sin tools
- `cohere-transcribe-03-2026`: STT (endpoint transcriptions)
- Rerank v4.0: `rerank-v4.0-fast`/`-pro` (32K ctx) vía `RerankModel` + método `Rerank()`

### Ollama
- Default global: texto puro, sin tools (`Tool_Active=False`, `ModelCaps=[]`)
- Modelos con tools: llama3.3, qwen2.5, qwen3, qwen2.5vl
- Modelos con reasoning: qwen3 (`ThinkingLevel=tlMedium`), deepseek-r1 (via `<think>` tags)
- Modelos con visión: llama3.2-vision, qwen2.5vl, gemma3 (todos los tamaños: 1b/4b/12b/27b)
- Gemma 3: visión en todos los tamaños; **sin function calling** vía Ollama (`Tool_Active=False`)
- Gemma 4 (e2b/e4b): visión + audio nativo + reasoning + **function calling** (`Tool_Active=True`)

### LM Studio
- Default global conservador: texto puro, sin tools, `Max_Tokens=4096`
- IDs de modelo según nombre/alias configurado en la app (no son fijos)
- Modelos configurados: llama-3.3-70b, qwen2.5-7b, deepseek-r1-7b, llama-3.2-11b-vision, mistral-7b, gemma-3-4b/12b/27b-it

### GenericLLM
- Driver catch-all para cualquier API OpenAI-compatible
- Defaults conservadores: texto, sin tools, `Max_Tokens=4096`
- Configurar: `DriverName='GenericLLM'`, `URL='http://host/v1/'`, `Model='nombre'`
- El usuario activa caps según las capacidades reales de su endpoint

---

## Cadena de llamada pública

```
AddMessageAndRun(prompt, role, files)  ← punto de entrada recomendado (public)
  └── Run(AskMsg, ResMsg)              ← sanitizador de prompts (public, virtual)
        └── RunNew(AskMsg, ResMsg)     ← orquestador interno: 3 fases + ChatMode (PRIVATE)
```

- **`AddMessageAndRun`** — crea el mensaje, lo agrega al historial y llama a `Run`.
- **`Run`** — aplica el sanitizador de prompts; si pasa, delega a `RunNew`.
- **`RunNew`** — hace el trabajo real: gap analysis, Fase 1 (bridges entrada), Fase 2 (web), Fase 3 (despacho por `ChatMode`). Es `private` — nunca llamar directamente.

## Eventos — tipo `of object`

Todos los eventos de `TAiChat` / `TAiChatConnection` son del tipo `of object`:

```delphi
TAiChatOnDataEvent = procedure(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: String) of object;
```

**No aceptan métodos anónimos.** Asignar siempre un método de clase:

```delphi
// Correcto — método del formulario o clase
Conn.OnReceiveDataEnd := Self.HandleDataEnd;

// NO compila — of object no acepta lambdas
Conn.OnReceiveDataEnd := procedure(...) begin ... end;
```

Para código sin clase (consola), usar el valor de retorno síncrono de `AddMessageAndRun`:
```delphi
Resp := Conn.AddMessageAndRun(prompt, 'user', []);
```

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
