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

### Guardrails (política de seguridad)
- `uMakerAi.Guardrails.pas` — `TAiGuardrails`: se asigna a `TAiFunctions.Guardrails` y se consulta en `DoCallFunction` **antes** de ejecutar cualquier tool (local, MCP o AutoMCP)
  - Orden de evaluación: `Enabled` → `AllowedTools` (whitelist estricta, comodines `TMask`) → `BlockedTools` → `BlockedArgPatterns` (substrings prohibidos en el JSON de argumentos, case-insensitive) → `Classifier` (juicio semántico, solo para lo que las listas dejaron pasar; si lanza excepción **bloquea**) → `OnCheckToolCall` (veto/permiso programático final)
  - `Classifier: TAiGuardrailClassifierBase` — clase base abstracta con `CheckToolCall(tool, args, out reason): Boolean`. Implementación con Jev: `TAiJevGuardrailClassifier`
  - Un bloqueo NO ejecuta el tool: pone `ToolCall.Response` con `{"error":"Blocked by guardrails: ..."}` para que el LLM replantee, dispara `OnBlocked` (auditoría), incrementa `BlockedCount` y marca el span con `guardrail.blocked`

### Jev (TypeSafe AI) — decisiones calibradas
- `uMakerAi.Jev.pas` — `TAiJev`: cliente de `POST https://api.typesafe.ai/v1/systemone`. **No es un driver de chat** (Jev no genera texto): recibe un `state` (texto o JSON) y preguntas tipadas, y devuelve probabilidades calibradas + confianza
  - Preguntas en `TAiJevQuestions` (`TCollection`, editable en diseño): `AddChoice` (opciones `'clave=descripcion'`, 2 a 255), `AddScore` (niveles ordenados, 2 a 10), `AddNoul` (sí/no, criteria opcional `true=`/`false=`). Se validan **antes** de ir a la red
  - `Ask(state, preguntas)` → `TAiJevResult` (lo libera el llamador); `R['id'].Choice/.Score/.Noul/.Confidence/.Probability(op)/.Top(n)`. Atajos `Choose` y `Noul` para una sola pregunta
  - Varias preguntas viajan en **una** llamada: preferir un `TAiJevQuestions` completo a llamadas sueltas
  - `Model` por defecto `jev-1.13.0` (no `jev-latest`: los umbrales se calibran contra una versión). `ApiKey` por defecto `@TYPESAFE_API_KEY`
  - Reintenta 429/503/529 con espera exponencial (`MaxRetries`, `RetryDelay`); otros errores lanzan `EAiJevError` (con `StatusCode`), llenan `LastError` y disparan `OnError`. Span de telemetría `jev.ask`
  - Envía el cuerpo en UTF-8 explícito: con la codificación por defecto la API devuelve 400 ante tildes o `¿`
  - `DoPost` es virtual: la suite de regresión lo sustituye (`TFakeJev`) para probar sin red
  - Jev no hace aritmética, no cuenta y no compara fechas de forma fiable: eso va en código. La confianza de una misma entrada varía unos puntos entre llamadas; dejar margen en los umbrales
  - Demo: `Demos/084-JevRouter`
- `uMakerAi.Jev.SmartDispatch.pas` — `TAiJevDispatchClassifier` para `ChatTools.DispatchClassifier`: reemplaza el pase 1 de `cmSmartDispatch` (un LLM que clasifica) por una Choice a Jev sobre los tags con tool asignada. Si solo queda CHAT no llama a Jev. Con confianza < `MinConfidence` (0.6) devuelve `''` y el chat usa el LLM como antes. Descripciones de tags calibradas (15/15 en español e inglés); `TagDescriptions` (`TAG=descripcion`) las reemplaza
- `uMakerAi.Jev.Guardrails.pas` — `TAiJevGuardrailClassifier` para `TAiGuardrails.Classifier`: Noul sobre `{tool, arguments}` con `Policy` (vacío = `DEFAULT_POLICY`: borrar/sobrescribir datos, exponer credenciales o datos privados, mover dinero, comandos destructivos); bloquea si P ≥ `BlockThreshold` (0.5). `BlockOnError` (default `True`) bloquea si Jev no responde. `LastRisk` guarda la última probabilidad. Calibrado: seguros ≤ 0.17, peligrosos ≥ 0.88 sobre 13 tool calls
  - **Categorías de permiso (opcional):** `Categories` (`clave=descripcion`) agrega una Choice en la **misma llamada**; bloquea si la probabilidad de alguna de `BlockedCategories` alcanza `CategoryThreshold` (0.5), aunque no sea la elegida. `OnCategorized(tool, args, categoria, confianza, var Allow, var Reason)` audita o cambia la decisión; `LastCategory`/`LastCategoryConfidence`. `ToolDescriptions` (`nombre=descripcion`) agrega una descripción al state: sin ella `anular_comprobante` salía `read` (0.83), con ella `write` (0.95). Calibrado: 19/20 en 6 categorías sin descripciones, 20/20 con ella
  - **Con `Categories` y sin `Policy` se usa `ABUSE_POLICY`** (solo lo abusivo) en lugar de `DEFAULT_POLICY`: la política estricta trata cualquier pago o borrado como riesgo (un `pay_invoice` normal daba 0.88) y pisaba a las categorías. `ABUSE_POLICY`: 0 errores en 19 casos, peligrosos ≥ 0.64, legítimos ≤ 0.35. Sin `Categories` el comportamiento no cambia
- `uMakerAi.Jev.Evals.pas` — `TAiJevEvalScorer` para `TAiEvalRunner.Scorer`: responde `ExpectScore('criterio', min)` con un Noul sobre `{input, response}` ("Does `response` satisfy this criterion: …?"). Calibrado 10/10: cumplen ≥ 0.97, no cumplen ≤ 0.02. `LastScore` guarda la última probabilidad
- `uMakerAi.Jev.RAG.pas` — `TAiJevRAGReranker` para `TAiRAGVector.Reranker`: una llamada por pasaje con dos Nouls, `evidence` (puntaje) e `injection` (≥ `InjectionThreshold` 0.9 → puntaje −1, descartado siempre). En paralelo con `MaxParallel` (4) y un `TAiJev` por llamada; con un `Jev` externo va en serie. Recorta pasajes a `MaxPassageChars` (4000). `LastInjected` cuenta los descartados. Calibrado: el pasaje correcto primero en 4/4 consultas; inyección 0.98 vs máximo legítimo 0.73
- `uMakerAi.Qwen.Rerank.pas` — `TAiQwenRAGReranker` para `TAiRAGVector.Reranker` con `qwen3-rerank` (Alibaba, key `@DASHSCOPE_API_KEY`): una llamada puntúa hasta 500 pasajes (lotes de `BatchSize` por encima), `Instruct` opcional, recorte a `MaxPassageChars` (4000), `LastTokens`. `Post` es virtual: la suite lo prueba con un transporte falso. Detalle en `Source/Chat/CLAUDE.md` (sección Qwen)
- `uMakerAi.Qwen.Voices.pas` — `TAiQwenVoices`: clonar voces desde una muestra, diseñarlas desde una descripción, listarlas y borrarlas (Alibaba, `@DASHSCOPE_API_KEY`). El id se usa como `TtsParams.Voice` en una conexión `Qwen`; el driver elige el modelo TTS que la voz exige. `Post` virtual para la suite. Detalle en `Source/Chat/CLAUDE.md` (sección Qwen)
- `uMakerAi.Jev.PromptGuard.pas` — `TAiJevPromptGuard` para `ChatTools.PromptGuard` (guardrail de **entrada**: el mensaje del usuario antes del LLM). Una llamada con hasta 4 Nouls: `injection`, `sensitive_data`, `harmful` y `out_of_scope` (solo si `Scope` no está vacío). Umbral por categoría (0.5). Las categorías de seguridad tienen **prioridad** sobre `out_of_scope` aunque este tenga más probabilidad (un jailbreak también sale "fuera de alcance"). `LastScores` guarda las probabilidades. Calibrado sobre 20 mensajes: injection ≥ 0.81 / ≤ 0.09, sensitive_data ≥ 0.98 / ≤ 0.10, harmful ≥ 0.98 / ≤ 0.53, out_of_scope ≥ 0.98 / ≤ 0.16. La aclaración "not counting greetings or thanks" es necesaria: sin ella "Hola, buenos días" salía fuera de alcance (0.82)
- `uMakerAi.Jev.Batch.pas` — `TAiJevBatchLabeler`: aplica `Questions` a muchas filas, una llamada por fila, en paralelo (`MaxParallel` 8, un `TAiJev` por fila; con un `Jev` externo va en serie). Entrada: textos (`{ItemField: texto}`) o `TJSONObject` ya armados (no toma posesión). `Run` devuelve un `TAiJevBatchReport` (lo libera el llamador): por fila `Choice`/`Confidence`/`Top(3)`/`NeedsReview`/`Error`/`Result` completo; totales `InputTokens`, `CostUSD` (`PricePerMillion` 0.042), `ElapsedMs`, `NeedsReview`, `SaveToCSV`. La etiqueta sale de `LabelQuestion` (vacío = la primera Choice). **Una fila con error no detiene el lote.** `Questions.Validate` y `LabelQuestion` se revisan antes de la primera llamada. `Cancel` es seguro desde otro hilo (filas pendientes → `Error = 'cancelled'`). `OnProgress` se dispara desde los hilos del pool: usar `TThread.Queue` para la UI. Validado reproduciendo el PUC (demo 087): mismos tokens que el prototipo en Python, 92.9% total y 100% en lo automatizado
- `uMakerAi.Jev.ModelRouter.pas` — `TAiJevModelRouter`: elige para cada petición el tier más barato que alcanza. Jev **no** elige el modelo: describe la petición (tarea Choice, dificultad Score 0–3, sensible Noul) y las reglas en código fijan el nivel (`round(dificultad)`, +1 si duda con `BumpOnDoubt`, `CodeMinLevel` para código/análisis/matemáticas, `SensitiveMinLevel` si sensible > `SensitiveThreshold`, 3 si la tarea es incierta). `Tiers` (colección: `Name`, `DriverName`, `Model`, `MaxLevel`, `Cost`, `Params`); `Pick` = el más barato con `MaxLevel` ≥ nivel, o el más capaz si ninguno alcanza. `Route` solo decide; `Apply(Connection, Route)` pone driver y modelo; `Ask` = Route + Apply + `AddMessageAndRun`. **Cambiar `DriverName` en `TAiChatConnection` recrea el chat y pierde el historial**: `MigrateHistory` (default) copia los mensajes de texto user/assistant (no los de tool calls). **Asignar `Model` recarga los defaults del modelo**: `ConnectionParams` (global) y `Tier.Params` (del modelo, ganan) se aplican después. Calibrado: 13/16 niveles exactos, 2 arriba, 1 abajo; en vivo (demo 088) 7/8 respuestas útiles
- Los seis adaptadores (`TAiJevRouterTool`, `TAiJevDispatchClassifier`, `TAiJevGuardrailClassifier`, `TAiJevEvalScorer`, `TAiJevRAGReranker`, `TAiJevPromptGuard`) aceptan un `TAiJev` externo en `Jev` (compartido, o el `TFakeJev` de la suite); si no, crean el suyo con `ApiKey`/`Model`. Demo: `Demos/085-JevDispatchGuard`
- **Ollama y otros servidores System One (oct 2026).** Ollama 0.35+ expone el mismo contrato en `http://localhost:11434/v1/systemone` con modelos de decisión locales: `nimble` (9B, Bespoke Labs), `tev1` (4B y 0.8B, Together AI), y `clef` (27B) / `clef-flash` (9B) de Cloudflare (Apache-2.0, Ollama 0.35.1+), que **leen imágenes**. `TAiJev` funciona sin cambios con `Url := 'http://localhost:11434/v1/'` y `Model := 'nimble'` (sin API key). Lo nuevo:
  - **`Url` en los ocho adaptadores** (`''` = TypeSafe; ignorada si `Jev` está asignado). Con `Url` propia, el reranker y el etiquetado masivo **mantienen el paralelismo** (con un `Jev` externo trabajan en serie).
  - **Imágenes:** `Ask(State, [Img1, Img2], Preguntas)` (`array of TAiMediaFile`; también `BuildRequest` con imágenes). Se validan **por los bytes** (PNG, JPEG o WebP; el nombre del archivo no importa) y van en base64 crudo sin saltos de línea en `images`. Solo modelos que leen imágenes (Clef).
  - **Precio:** `JEV_PRICE_PER_MILLION_INPUT` solo se cobra cuando la `Url` apunta a TypeSafe; un precio fijado a mano se respeta siempre (`JevEffectiveInputPrice`; en adaptadores `JevAdapterInputPrice` usa la `Url` del `Jev` que realmente se usa).
  - **Límites de Ollama:** `choice` y `score` de 2 a 26 opciones (TypeSafe: hasta 255; el servidor responde 400 con un mensaje claro), requests de hasta 64 KiB, hasta 64 preguntas. La primera llamada carga el modelo en memoria: subir `Timeout` (Clef tardó más de 30 s).
  - Telemetría: `gen_ai.system` = `typesafe` o `systemone`, y `jev.images`.
  - **Probado en vivo (oct 5 2026, Ollama 0.35.1, RTX 4060 Ti):** `TAiJev` con `nimble` (~430 ms, los tres tipos de pregunta, tildes), `TAiJevPromptGuard` (bloquea una inyección con 0.99) y `TAiJevBatchLabeler` en paralelo con su propia `Url`; costo 0. **Imágenes verificadas con `clef` (27B):** la captura del demo 092 en PNG, JPEG y WebP → `chat_voz` 0.99, micrófono 0.98, barras 0.02; un gráfico de barras de control → `grafico` 0.99, micrófono 0.02, barras 0.99. ~2.5 s por imagen con el modelo cargado (la carga inicial, 43 s). Un JPEG de 91 KB pasó: el límite de 64 KiB no aplica a las imágenes.
  - **`clef-flash` no funciona en Ollama 0.35.1**: HTTP 500 `Clef: non-finite logit` con cualquier request, incluso solo texto ([ollama#18769](https://github.com/ollama/ollama/issues/18769), abierto). Usar `clef` (27B) hasta que se arregle.
  - **Memoria de video de `clef` (27B):** con su contexto por defecto (16384) no cabe en 24 GB (RTX 4060 Ti 16 GB + RTX 3050 8 GB): pesos 17 GB más 12 GB de buffers de cálculo → HTTP 500 `cudaMalloc failed: out of memory`. Una decisión con imagen usa ~1200 tokens, así que basta una variante con menos contexto, sin volver a descargar: un Modelfile con `FROM clef` y `PARAMETER num_ctx 4096`, `ollama create clef-4k -f Modelfile`, y `Model := 'clef-4k'` (ocupa 19 GB).
  - Calidad publicada (no medida aquí): Nimble iguala o supera a Jev en clasificación corta (TREC 95–96 %) y queda unos 10 puntos atrás en razonamiento; su confianza está menos calibrada en tareas difíciles. Usar los locales para ruteo y triage; Jev para juicios finos.
- **Consumo para cobrar por uso (sep 28 2026).** `TAiJev` y los ocho adaptadores (los seis anteriores + `TAiJevBatchLabeler` + `TAiJevModelRouter`) exponen `Usage: TAiJevUsage` (`Requests`, `InputTokens`, `OutputTokens`, `CostUSD`; acumulado desde `Create` o `ResetUsage`, seguro entre hilos), `PricePerMillionInput` (default US$0.042) / `PricePerMillionOutput` (default 0: la salida no se cobra) y `OnUsage`. **`OnUsage` se dispara una vez por operación** (un `CheckPrompt`, un `Score`, un `Run` de lote…) con el consumo de esa operación, **sincrónico en el hilo que la ejecutó** — en un servidor, el de la petición, así que se sabe a qué cliente cargarlo. El reranker en paralelo (un `TAiJev` por pasaje) da **un solo evento con el total**, después de juntar las llamadas; también se reporta lo gastado si una llamada falla a mitad. `TAiJev.OnUsage` es por llamada HTTP: cobrar desde el adaptador **o** desde `TAiJev`, no de ambos (se contaría doble si el `Jev` es externo). Piezas comunes en `uMakerAi.Jev.pas`: `TAiJevUsageMeter` (acumulador con `TInterlocked`), `JevReportOperation` / `JevReportResult`. Batch conserva `PricePerMillion` como precio de entrada. Antes solo Batch exponía tokens; el resto los recibía en `TAiJevResult` y los descartaba. Verificado en vivo: los tokens coinciden con los que reporta la API.

### Skills bajo demanda
- `uMakerAi.Tools.Skills.pas` — `TAiSkills` (sep 2026): skills estilo Agent Skills para **cualquier** chat con function calling. Se asigna a un `TAiFunctions` (`Functions`) y registra dos funciones:
  - `use_skill(name)`: su descripción lleva el **catálogo** (una línea `- nombre: descripción` por skill habilitado) y `name` es un `enum` con esos nombres. El modelo ve solo el catálogo y pide las instrucciones cuando las necesita, así que tener muchos skills no cuesta tokens en cada turno. Responde `<skill name=".." source="inline|file|ppm">instrucciones</skill>`, marca los de PPM como de terceros y lista los archivos de apoyo. Sin skills la función queda `Enabled=False` (un enum vacío es un schema inválido)
  - `read_skill_file(name, path)`: solo si algún skill viene de una carpeta y `AllowFileAccess`. Confinada a la carpeta del skill: rechaza rutas absolutas, `..` (se resuelve con `GetFullPath` y se compara el prefijo), enlaces simbólicos/reparse points en el archivo o en cualquier carpeta intermedia, binarios (byte 0) y archivos mayores que `MaxFileSize` (256 KB). Nunca ejecuta nada
  - Carga: `AddSkill(nombre, descripción, instrucciones)`, `LoadFromFile`, `LoadFromFolder` (`<dir>/<nombre>/SKILL.md`), `LoadFromPPM`, o la propiedad `Folder` (se carga en `Loaded`, solo en runtime). Un nombre repetido reemplaza al skill. `Skills` es una colección editable en diseño (instrucciones inline)
  - Eventos: `OnBeforeUseSkill` (veto), `OnBeforeReadFile` (veto con la ruta ya validada), `OnSkillLoaded` (auditoría). Span de telemetría `skill.load`. Las llamadas pasan por `TAiFunctions.DoCallFunction`, así que `TAiGuardrails` también las filtra
  - **En el IDE no registra las funciones** (quedarían en el DFM del `TAiFunctions` con un `OnAction` que apunta a otro componente). Al destruirse las saca de `Functions`; si se libera primero el `TAiFunctions`, `Notification` suelta la referencia
  - `ExecuteUseSkill` / `ExecuteReadFile` / `Catalog` son públicos: lo mismo que ve el modelo, sin pasar por él
  - Probado en vivo (sep 29 2026) con gpt-5.6-luna, claude-haiku-4-5 y gpt-oss-120b (Groq): 9/9 — carga el skill que corresponde, no carga ninguno ante una petición ajena, y encadena `use_skill` → `read_skill_file` cuando las instrucciones lo piden

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
  - **El componente es agnóstico de plataforma**: solo usa RTL y delega TODO en `OnExecuteAction` / `OnRequestScreenshot`. Compila para Linux64 sin cambios (verificado sep 20/2026)
  - Executors por plataforma, **ninguno en el `.dpk`** (las apps los incluyen directo): `*.Windows.pas` (VCL/GDI), `*.WindowsFMX.pas` (FMX, dibuja el cursor), `*.Mac.pas` (CGEvent, sin probar en hardware) y `*.Linux.pas` (`TAiLinuxExecutor`, X11 vía xdotool + scrot; **probado en runtime** sobre Xvfb con gpt-6-astra y claude-opus-4-8 — ver demo 083)

### Media Generation Tools
- `uMakerAi.OpenAi.Dalle.pas` - Image generation (`TAiDalle`)
  - Supports dall-e-2, dall-e-3 (both deprecated) and the whole gpt-image family: `gpt-image-1`, `-1-mini`, `-1.5`, `-2`, `gpt-image-2.5-flare`, `gpt-image-2.5-sunburst`, `chatgpt-image-latest`
  - **gpt-image-2.5** (sep 2026): only family that takes `quality` = `xhigh`/`max` (`iqXHigh`/`iqMax`, degraded to `high` on older models) and that supports real alpha `background=transparent`; `jpeg` is auto-switched to `png` when transparent; `input_fidelity` is not sent (undocumented for 2.5, same as gpt-image-2); `n` capped at 8 like gpt-image-2
  - Streaming with `OnPartialImageReceived`, `OnStreamCompleted` (not on `gpt-image-2`; available again on 2.5)

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
  - **gpt-transcribe / gpt-live-transcribe** (2026, verified live): `tmGptTranscribe` (recommended for files/batch, WER 8.98%) and `tmGptLiveTranscribe`; context via `TranscriptionKeywords` (`keywords[]`) and `TranscriptionLanguages` (`languages[]`, replaces singular `language`); these models only return JSON — srt/vtt/verbose_json/timestamps degrade to json; diarization stays on `tmGpt4oDiarize`
  - **`TTranscriptionResult.Warning`** (Oct 2026): one line per thing that was requested but not honored (format degraded to json, timestamps/logprobs/known speakers ignored by the model) plus a deprecation notice for the old models. The result is parsed with the format actually requested (before, `trfText`/`trfSrt` degraded to json left the raw JSON in `Text`)
  - **Deprecations:** `whisper-1`, `gpt-4o-transcribe`, `gpt-4o-mini-transcribe`, `gpt-4o-transcribe-diarize` (OpenAI 2026-08-26, shutdown 2027-02-26) and `tts-1`, `tts-1-hd`, `gpt-4o-mini-tts` (2026-10-01, shutdown 2027-01-06; OpenAI points to `gpt-realtime-2.1-mini`). The replacements have **no** srt/vtt/verbose_json, timestamps, logprobs, diarization or translation to English: those features (and `TranslateToEnglish`, whisper-1 only) disappear on the shutdown date. **Defaults changed in v3.9** (`TAiOpenAiAudio` and `TAiOpenAiSpeechTool`): `TranscriptionModel` `tmWhisper1` → `tmGptTranscribe`, `TTSModel` `tts_1` → `gpt_4o_mini_tts` (constructor and published `default` together, checked by the `audio.defaults` regression case). Forms that kept the old default switch silently: set `tmWhisper1` explicitly to keep srt/vtt/timestamps until the shutdown. `gpt-4o-mini-tts` is also deprecated (Jan 6 2027) but is the best model `/audio/speech` still accepts; it supports `TTSSpeed` and `TTSInstructions`. `TAIWhisper` keeps `whisper-1` (it also targets self-hosted whisper servers)
  - **Diarization** (verified Jun 2026): `TranscriptionModel := tmGpt4oDiarize` + `TranscriptionResponseFormat := trfDiarizedJson` -> `TTranscriptionResult.Segments` (array of `TDiarizedSegment`: Speaker/Text/StartTime/EndTime) and `DiarizedText` ("Speaker: text" per line). Optional named speakers: `AddKnownSpeaker(aName, aWavFileOrStream)` (max 4, 2-10 s voice sample) makes segments use real names instead of 'A'/'B'. Notes: the diarize model does not support logprobs (auto-excluded); `chunking_strategy=auto` is sent automatically; segments work with bilingual audio.

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
