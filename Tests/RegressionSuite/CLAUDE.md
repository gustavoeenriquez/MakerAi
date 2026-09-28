# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**Suite de regresión de MakerAI** — la red de seguridad del framework. Valida los subsistemas críticos **in-process**, sin servicios externos, sin API keys y sin depender de demos compilados: levanta sus propios servidores MCP y A2A en puertos altos (18790-18793) y los apaga al terminar.

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

Duración típica: ~3 segundos (los casos con URL a puerto cerrado esperan el rechazo de Windows, ~2 s).

## Cobertura actual (74 casos)

| Área | Casos |
|------|-------|
| MCP dual-era | negociación moderna (2026-07-28), fallback a handshake legacy, `tools/list`, `tools/call` |
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
| Streaming | `OnReceiveDataEnd` sin texto duplicado al cerrar el stream (SSE simulado sobre `TAiGroqChat`; falla sin el fix) y `executed_tools` de Groq combinados por `index` desde el delta (chunks de un SSE real de `gpt-oss-20b`) |
| Qwen (request) | `enable_thinking` siempre presente (false sin `cap_Reasoning`), `thinking_budget` por `ThinkingLevel`, ausente en `qwq-*`, false en pesos abiertos síncronos; `stream_options.include_usage` solo en streaming; audio de entrada reescrito como data URI (falla sin el fix); `TAiQwenRAGReranker` con transporte falso: lotes, mapeo por `index`, `instruct`, recorte; request de imagen: genera sin adjuntos, edita con 1-3 (modelo de edición, data URI, sin forzar tamaño), falla con 4 o con `z-image-turbo`; request de video: t2v/i2v/kf2v según las imágenes adjuntas, `-t2v` → `-i2v`, 720p por `size` o `resolution` según familia, tipos de `VideoParams.Params`; traducción `qwen-mt`: un solo mensaje user con `translation_options` (idiomas, dominio, glosario) y streaming acumulado de plus/turbo convertido a incrementos con una línea SSE partida (falla sin el fix); `TAiQwenVoices` con transporte falso (clonar, diseñar con fallback y preview, listar, borrar) y modelo TTS elegido por el prefijo de la voz |
| Conexión | sub-parámetros de medios (`VideoParams`, `TtsParams`, `ImageParams`) editados después de crear el chat llegan al driver en `AddMessageAndRun` (URL a puerto cerrado; falla sin el fix) |
| Realtime Qwen | eventos del servidor sin red: los tres formatos de transcripción (stash acumulado, text+stash con reescritura, delta incremental) salen como deltas; texto y audio del asistente, cierre y error; `session.update` de los tres drivers (formatos, VAD manual → null, idioma, voz de traducción) y registro en la fábrica y en `TAiRealtimeConnection`; `TAiQwenRealtimeTTS`: `session.update` (modo, idioma, voz, instrucciones), modelo realtime elegido por el prefijo de la voz propia y eventos (listo una sola vez, audio, respuestas, fin, error) |
| Chat / tool results | serialización OpenAI-compatible: tool calls paralelas con imagen → un solo `user` sintético tras el grupo, modelo sin `cap_Image` → transcripción sin media, transcripción no duplicada, adjunto de texto inline |
| Evals | autoprueba del runner (conteo PASS/FAIL) |
| Jev (TypeSafe) | forma del request (Choice con opción sin descripción → `null`, Score, Noul con criteria parcial), parseo de las tres respuestas, reintento ante 429/529, reintentos agotados, validación local sin red, 401 sin reintento — con `TFakeJev` (sin red ni API key) |

## Estructura

| Archivo | Contenido |
|---------|-----------|
| `MakerAiRegressionSuite.dpr` | Programa principal: CLI (`--json`, `--otel`), ejecución y exit code |
| `uRegression.Suites.pas` | Definición de los casos (`DefineCases`) y el *dispatcher* que ejecuta cada escenario contra los componentes reales |
| `uRegression.Fixtures.pas` | `TFakeJev` (TAiJev con respuestas HTTP encoladas), `TFakeDispatchClassifier` y `TFakeImageTool` (SmartDispatch sin red), `TPassageFakeJev` (responde según el pasaje), `FakeEmbedding`, `TFakePromptGuard` y los handlers `ChatError` / `PromptGuardAllow` / `JevCategorizedAllow` / `BatchCancelAfterFirst`, tools MCP de prueba (`echo_upper`, `confirm_op` con MRTR), servidor MCP "solo legacy" (responde `-32601` a `server/discover`) y handlers `of object` (incluye `NodeSuspendOnce` para human-in-the-loop y `AcquireManager` como fábrica del pool A2A) |

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
