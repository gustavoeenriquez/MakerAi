# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**Suite de regresión de MakerAI** — la red de seguridad del framework. Valida los subsistemas críticos **in-process**, sin servicios externos, sin API keys y sin depender de demos compilados: levanta sus propios servidores MCP, A2A y un registry PPM falso en puertos altos (18790-18794) y los apaga al terminar.

Está construida sobre `TAiEvalRunner` (`Source/Core/uMakerAi.Evals.pas`), así que es a la vez la suite de tests y el ejemplo canónico de uso del componente de evals.

## Build & Run

```bash
msbuild MakerAiRegressionSuite.dproj /p:Config=Release /p:Platform=Win64
```

```bash
# Ejecutar (exit code 0 = todo verde, 1 = fallos, 2 = error no controlado)
Win64\Release\MakerAiRegressionSuite.exe

# Reporte JSON para CI
Win64\Release\MakerAiRegressionSuite.exe --json report.json

# Con trazas OpenTelemetry (collector OTLP en localhost:4318)
Win64\Release\MakerAiRegressionSuite.exe --otel
```

Duración típica: ~5 segundos (los casos con URL a puerto cerrado esperan el rechazo de Windows, ~2 s cada uno).

## Cobertura actual (117 casos)

| Área | Casos |
|------|-------|
| MCP dual-era | negociación moderna (2026-07-28), fallback a handshake legacy, `tools/list`, `tools/call` |
| MCP fugas | `mcp.initialize.no-leak`: 5 `Initialize` contra el servidor in-process no dejan bloques vivos (mide `GetMemoryManagerState`). Cubre la fuga del cliente (respuesta de `tools/list`) y la del servidor (esquema clonado por tool en cada `tools/list`); falla sin cualquiera de las dos |
| MCP MRTR | reintento con `accept`, mensaje de elicitation recibido, sin handler → error explícito |
| Agentes | grafo secuencial con status final y salida encadenada; `lmExpression` con punto decimal bajo configuración regional con coma |
| SmartDispatch + Jev | `ChatTools.DispatchClassifier` sobre un `TAiOpenChat` real (URL a puerto cerrado): el tag va directo a la tool sin pase por LLM; `TAiJevDispatchClassifier` solo ofrece los tags recibidos, respeta `MinConfidence` y no consulta a Jev si solo queda CHAT |
| Guardrail de entrada | `ChatTools.PromptGuard` sobre un `TAiOpenChat` real (URL a puerto cerrado, SmartDispatch a una tool falsa): bloquea sin tocar la red, permite, `OnPromptGuard` anula el bloqueo, guard caído con `BlockOnError` cerrado/abierto; `TAiJevPromptGuard` prioriza seguridad sobre `out_of_scope` y solo pregunta el alcance con `Scope` |
| Guardrails + Jev | `TAiGuardrails.Classifier` con `TAiJevGuardrailClassifier`: riesgo alto bloquea con motivo, bajo permite, lo que ya bloquean las listas no llega a Jev, `BlockOnError` cerrado/abierto; categorías de permiso: bloquea una categoría prohibida aunque no sea la elegida, el riesgo manda primero, `ToolDescriptions` viaja en el state, `OnCategorized` anula |
| Etiquetado masivo | `TAiJevBatchLabeler`: etiqueta/confianza por fila, una fila con error no detiene el lote, filas dudosas, tokens sumados, validación de `Questions` y `LabelQuestion` antes de llamar, estados JSON tal cual, `Cancel` desde `OnProgress` |
| Enrutador de modelos | `TAiJevModelRouter`: mínimo por código, mínimo por sensible, +1 por duda, el tier más barato que alcanza y el más capaz si ninguno llega; migración entre proveedores (solo user/assistant de texto), `ConnectionParams` y `Tier.Params` aplicados después del modelo, mismo proveedor conserva el chat — sobre una `TAiChatConnection` real sin red |
| Evals + Jev | `TAiEvalRunner.Scorer` con `TAiJevEvalScorer`: `ExpectScore` pasa, falla con el puntaje en el motivo, un error de Jev falla el check sin excepción, el input viaja en el state |
| RAG + Jev | `TAiRAGVector.Reranker` con `TAiJevRAGReranker` en el pipeline VQL real (embeddings falsos vía `TAiEmbeddingsCore.OnGetEmbedding`): ordena por evidencia y descarta el pasaje inyectado; reranker caído → cae al coseno sin excepción |
| Agentes + Jev | `TAiJevRouterTool` en un grafo real: ruta elegida, confianza baja → `NextNo`, Jev caído (401) → `NextNo` sin romper el grafo, y las claves `<Nodo>.jev.*` del blackboard |
| A2A 1.0 | Agent Card, `SendMessage` → `TASK_STATE_COMPLETED`, federación (grafo local → agente remoto) |
| A2A orquestación | pool con 3 tasks simultáneos, human-in-the-loop con resume por `taskId`, human-in-the-loop federado (suspensión del nodo local), `blocking=false` + `GetTask`, tolerancia de literales de estado, cancelar task terminal → `-32002` |
| Guardrails | blocklist con comodín, allowlist estricta, patrón prohibido en argumentos, veto programático, integración real (el tool bloqueado NO se ejecuta) |
| A2A autorizacion | card base publica, RPC sin clave -> 401, clave con otras mayusculas -> 401 |
| A2A streaming | reanudar un task en `input-required` con `SendStreamingMessage` (SSE crudo) |
| A2A Agent Card | skills declaradas (con tags) y skill `run-graph` por defecto cuando no hay ninguna |
| RAG | búsqueda con `Options` en nil sobre el driver `.mkai` (regresión del AV por `IfThen`) |
| Memoria | `TAiMemory` aísla namespaces también en las operaciones por Id (issue #127): desde otro namespace, `Get` no ve la memoria y `Update`/`Link`/`Delete` no la tocan; `ImportFromJSON` ignora el `namespace` del JSON y escribe en el activo (falla sin el fix) |
| Skills (formato) | parser común de SKILL.md (`uMakerAi.Skills.Format`): comillas, comentario `#`, escalar partido en dos líneas, listas `- x` e inline `[a, "b"]`, bloque `>`, Markdown sin frontmatter; carpeta `<dir>/<nombre>/SKILL.md` con el nombre de la carpeta como fallback. Registry PPM falso (puerto 18794): prefijo `skill-` automático, mayor versión por semver sin las retiradas (1.10.0 gana a 1.2.0), `TAiPrompts.LoadSkillFromPPM` por el mismo camino; errores claros ante paquete inexistente, de otro tipo o un 200 con HTML, y `TAiPrompts` devuelve nil sin excepción |
| Skills (TAiPrompts) | `LoadSkillsFromFolder` carga dos skills e ignora la carpeta sin SKILL.md; nombre del frontmatter o de la carpeta; `SkillDescription`; `ApplySkill` reemplaza y agrega sobre un `TStrings`; nombre inexistente → `False` |
| Skills (TAiSkills) | registro en `TAiFunctions` (apagada sin skills, `enum` solo con los habilitados, catálogo en la descripción, sin `read_skill_file` si no hay carpetas); `use_skill` por `DoCallFunction` (entrega con origen, nombre desconocido con la lista, veto, `OnSkillLoaded`); `read_skill_file` confinado (lee y lista archivos de apoyo, rechaza `..` en dos formas, rutas absolutas, binarios, archivos grandes, inexistentes y skills sin carpeta); ciclo de vida (al liberarse saca sus funciones; liberar antes el `TAiFunctions` no deja referencias colgantes) |
| Skills (agentes) | `TLLMNode.ResolveConfig`: siete combinaciones nodo/skill (sin skill → Claude; driver/modelo/clave del skill con el nodo vacío; el nodo cambia de driver y no hereda un modelo ajeno; modelo solo del nodo; SKILL.md sin driver; `driver:` del frontmatter sí y `apikey:` no); `SystemPrompt` skill + nodo; `ConfigureChat` sobre una `TAiChatConnection` real deja driver, modelo y clave resueltos también al cambiar de driver (falla con el orden viejo); `TAiSkill` desde JSON, carpeta con SKILL.md y registry falso |
| Streaming | `OnReceiveDataEnd` sin texto duplicado al cerrar el stream (SSE simulado sobre `TAiGroqChat`; falla sin el fix) y `executed_tools` de Groq combinados por `index` desde el delta (chunks de un SSE real de `gpt-oss-20b`) |
| tool_choice | `Tool_choice` forzado (`required` o una función) se lee `auto` en la ronda que devuelve resultados de tools; `auto`/`none` no cambian (antes: loop sin fin, 231 requests con `claude-sonnet-5`) |
| OpenAI (request) | default `gpt-6-sol` en el driver y en `TAiChatConnection`; GPT-6 sin effort `minimal` (→ `low`), 5.x sí; `tlMax` → `max`; `TAiMakerAiChat` con `mk-gpt-oss-20b` |
| Claude (request) | generación sep 2026 (sonnet-5-5/opus-5-5): forzar tool → `auto`; `claude-sonnet-5` manda `any` solo en la primera llamada; effort `max`/`xhigh` (no en 4.6 → `high`)/`minimal` → `low`; alias del retirado `claude-opus-4-1` → `claude-opus-5-5` |
| Gemini (request) | defaults del driver, de `TAiChatConnection` y de las tools (TTS, transcripción, búsqueda) en `gemini-3.8-flash(-tts)`; sampling omitido por versión (3.6/3.7/3.8 no, 3.0 sí); `tlMinimal` → `LOW` y `tlMax` → `HIGH`; alias de los apagados (`gemini-3-pro-preview`, `imagen-4.0-*`, `aa_gemini-3-pro-fast`) — sin red ni key |
| Qwen (request) | `enable_thinking` siempre presente (false sin `cap_Reasoning`), `thinking_budget` por `ThinkingLevel`, ausente en `qwq-*`, false en pesos abiertos síncronos; `stream_options.include_usage` solo en streaming; audio de entrada reescrito como data URI (falla sin el fix); `TAiQwenRAGReranker` con transporte falso: lotes, mapeo por `index`, `instruct`, recorte; request de imagen: genera sin adjuntos, edita con 1-3 (modelo de edición, data URI, sin forzar tamaño), falla con 4 o con `z-image-turbo`; request de video: t2v/i2v/kf2v según las imágenes adjuntas, `-t2v` → `-i2v`, 720p por `size` o `resolution` según familia, tipos de `VideoParams.Params`; traducción `qwen-mt`: un solo mensaje user con `translation_options` (idiomas, dominio, glosario) y streaming acumulado de plus/turbo convertido a incrementos con una línea SSE partida (falla sin el fix); `TAiQwenVoices` con transporte falso (clonar, diseñar con fallback y preview, listar, borrar) y modelo TTS elegido por el prefijo de la voz |
| Conexión | sub-parámetros de medios (`VideoParams`, `TtsParams`, `ImageParams`) editados después de crear el chat llegan al driver en `AddMessageAndRun` (URL a puerto cerrado; falla sin el fix) |
| Registro de parámetros | con `Model` vacío, cada driver registrado recibe los mismos parámetros que con su modelo por defecto; conexión Qwen sin `Model` con `cap_Image`, Groq con `Max_Tokens` 65536 (falla sin el fix) |
| Realtime: conector | `DriverParams` aplicado por RTTI al crear el driver (texto), al cambiar de driver sin falsos errores, Grok con enumerado y lista (`Keyterms` con `|`), clave desconocida informada por `OnError` al conectar (`Url` a puerto cerrado, sin red) |
| Realtime Qwen | eventos del servidor sin red: los tres formatos de transcripción (stash acumulado, text+stash con reescritura, delta incremental) salen como deltas; texto y audio del asistente, cierre y error; `session.update` de los tres drivers (formatos, VAD manual → null, idioma, voz de traducción) y registro en la fábrica y en `TAiRealtimeConnection`; `TAiQwenRealtimeTTS`: `session.update` (modo, idioma, voz, instrucciones), modelo realtime elegido por el prefijo de la voz propia y eventos (listo una sola vez, audio, respuestas, fin, error) |
| Ciclo de vida del chat | liberar el chat con una petición asíncrona en vuelo espera a que el cliente HTTP la cierre (cierre simulado desde otro hilo; falla sin el fix) y sin petición libera al instante; `ParseJsonTranscript` en el puente de Fase 1 no escribe la respuesta ni dispara eventos (en `cmTranscription` sí); `Voice`/`Voice_Format` del catálogo y del usuario llegan a `TtsParams` |
| Chat / tool results | serialización OpenAI-compatible: tool calls paralelas con imagen → un solo `user` sintético tras el grupo, modelo sin `cap_Image` → transcripción sin media, transcripción no duplicada, adjunto de texto inline |
| GPT-Live | `TAiOpenAiLiveChat` con `TLiveProbe` (captura lo enviado, reloj `NowMs` simulado, sin red): `session.start` (modelo en la sesión, delegación a Responses con tools/tool_choice/razonamiento, `DelegateChat` → `client`, fábrica y conector); eventos (deltas, audio, consumo, cierre, error); turnos con la línea de tiempo (fragmento atrasado, cierre por inactividad) la **traza real** de habla superpuesta (falla con la agrupación por orden de llegada) y un cuento largo con transcripts a ráfagas (la voz del audio mantiene el turno; un turno cerrado por inactividad se retoma si el modelo sigue); funciones vía `response.event` con un solo `response.create` también si la respuesta termina antes que la función; delegación `client` (contexto sin repetir lo ya enviado, fragmentos ≤ 400 bytes, `delegation_id` null con Responses, aviso si nadie atiende); `DelegateChat` real contra un puerto cerrado |
| OpenAI Audio | `TAiOpenAiAudio` con `TFakeOpenAiAudio` (sustituye `PostMultipart`, sin red): formato degradado a json se interpreta como json y queda en `Warning` (falla sin el fix: `Text` con el JSON crudo); timestamps y logprobs ignorados avisados y no enviados; `languages[]`/`keywords[]` sin avisos; `whisper-1` con srt + timestamps sigue igual y avisa la deprecación; `TranslateToEnglish` (solo whisper-1) avisa la deprecación; `audio.defaults`: defaults v3.9 (`gpt-transcribe`, `gpt-4o-mini-tts`) en `TAiOpenAiAudio` y `TAiOpenAiSpeechTool`, con el valor del constructor igual al `default` publicado (RTTI) |
| Evals | autoprueba del runner (conteo PASS/FAIL) |
| Jev: consumo | `Usage` y `OnUsage` en `TAiJev` (un evento por llamada) y en PromptGuard, Dispatch, Guardrail, Eval, ModelRouter, RAG y Batch (uno por operación); reranker en paralelo con un `TAiJev` falso por pasaje → un solo evento con el total exacto y en el hilo del llamador; `ResetUsage`; precio configurable |
| Jev en servidores System One (Ollama) | `jev.systemone.local`: imágenes en el request (`images`, base64 sin saltos que decodifica a los bytes originales, PNG y JPEG reconocidos por los bytes aunque el archivo no tenga extensión), una "imagen" que no es PNG/JPEG/WebP rechazada antes de la red, el precio de TypeSafe no se cobra con `Url` local salvo precio propio, y `JevAdapterInputPrice` toma la `Url` del `Jev` que se usa |
| Jev (TypeSafe) | forma del request (Choice con opción sin descripción → `null`, Score, Noul con criteria parcial), parseo de las tres respuestas, reintento ante 429/529, reintentos agotados, validación local sin red, 401 sin reintento — con `TFakeJev` (sin red ni API key) |

## Estructura

| Archivo | Contenido |
|---------|-----------|
| `MakerAiRegressionSuite.dpr` | Programa principal: CLI (`--json`, `--otel`), ejecución y exit code |
| `uRegression.Suites.pas` | Definición de los casos (`DefineCases`) y el *dispatcher* que ejecuta cada escenario contra los componentes reales |
| `uRegression.Fixtures.pas` | `TFakeJev` (TAiJev con respuestas HTTP encoladas), `TFakeDispatchClassifier` y `TFakeImageTool` (SmartDispatch sin red), `TPassageFakeJev` (responde según el pasaje), `FakeEmbedding`, `TFakePromptGuard`, `TFakeOpenAiAudio` (multipart capturado, `FieldValues`) y los handlers `ChatError` / `PromptGuardAllow` / `JevCategorizedAllow` / `BatchCancelAfterFirst`, tools MCP de prueba (`echo_upper`, `confirm_op` con MRTR), servidor MCP "solo legacy" (responde `-32601` a `server/discover`) y handlers `of object` (incluye `NodeSuspendOnce` para human-in-the-loop y `AcquireManager` como fábrica del pool A2A) |

Los escenarios A2A de orquestación viven en `RunA2AFlowScenario`, aparte del bloque `a2a:` básico, porque cada uno arma su propia topología (pool, suspensión, no bloqueante).

## Cómo agregar un caso

1. En `uRegression.Suites.pas` → `DefineCases`, declarar el caso con su escenario y expectativas:
   ```pascal
   FRunner.AddCase('area.subarea.detalle')
     .Input('area:escenario')
     .ExpectContains('...');
   ```
2. En el `Run<Area>Scenario` correspondiente, implementar el escenario devolviendo un string con el resultado observable.
3. Si el escenario necesita un tool o servidor nuevo, agregarlo a `uRegression.Fixtures.pas`.

Convenciones: nombres de caso en `area.subarea.detalle`; los escenarios devuelven strings compactos (`'estado|salida'`) para poder afirmar con `ExpectEquals`.

## Notas

- Los eventos del framework son `of object` (sin lambdas): los handlers viven en `TFixtureHandlers`.
- Cada caso emite un span `eval.case <nombre>` cuando se corre con `--otel`, útil para ver la suite completa en Jaeger/Langfuse.
- La suite no requiere claves de API. Si en el futuro se agregan casos que llamen a proveedores reales, deben omitirse (skip) cuando falte la variable de entorno correspondiente.

## Navigation

> See [../../CLAUDE.md](../../CLAUDE.md) for project overview.
