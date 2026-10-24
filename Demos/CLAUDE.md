# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is the Demos directory for the MakerAI 3.x framework. Contains 66 working example projects demonstrating AI integration patterns for Delphi developers. La mayoria tiene su propio CLAUDE.md con los detalles de implementacion; los que no, estan marcados abajo.

**Estado de compilación (ago 7/2026): los 48 proyectos compilan** en Win64/Release; el 081 (ago 18/2026), el 082 y el 083 (sep 20/2026) también compilan; el 082 se corrió en runtime (6/6 PASS) y el 083 en **Linux64** sobre Xvfb con OpenAI y Claude.

## Building Demos

**IDE:** Delphi 11 Alexandria through 13 Florence (demos require Delphi 11+; the core framework supports 10.4 Sydney minimum)

**Group project:** Open `DemosVersion31.groupproj` in Delphi IDE to access all demos. El grupo se regeneró en ago 2026; desde sep 29/2026 incluye los 58 proyectos (se sumaron 089-QwenShowcase, que había quedado fuera, y 091-Skills); oct 5/2026 se sumó 092-GPTLiveVoice (59); oct 22/2026, 093-A2AChatAgentTool (60) y de 094 a 099, los demos de la serie de Café La Ladera (66); de 100 a 105, más demos de la serie (72).

> **OJO al editar los `.pas` de los demos:** varios están en **ANSI (Windows-1252) con saltos LF**, no en UTF-8. Guardarlos como UTF-8 destruye todas las tildes de forma silenciosa (compila igual, y `git diff` lo disimula si `core.autocrlf` está activo). Verificar siempre con `git diff --numstat` que solo cambien las líneas que se tocaron.

**Individual build (MSBuild):**
```bash
msbuild 010-Minimalchat/MinimalChat.dproj /p:Config=Release /p:Platform=Win64
```

**Build all demos:**
```bash
msbuild DemosVersion31.groupproj /t:Build /p:Config=Release /p:Platform=Win64
```

**Output:** Each project compiles to `./$(Platform)/$(Config)/` (e.g., `./Win64/Release/`)

## Demo Categories

### Chat Integration (01x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 010-Minimalchat | Basic FMX chat | `TAiOllamaChat` direct + `TAiChatConnection` factory |
| 012-ChatAllFunctions | Full-featured chat | Multimodal (images, audio), streaming, tool calls |
| 014-ChatTest | Claude-specific testing | Claude API integration |

### RAG Systems (02x)
| Demo | Purpose | Key Classes |
|------|---------|-------------|
| 021-RAG+Postgres-UpdateDB | Vector RAG with PostgreSQL | `TAiVectorRAG`, pgvector |
| 022-1-RAG_SQLite | Vector RAG with SQLite | Lightweight local storage |
| 023-RAGVQL | Vector Query Language | VQL semantic search syntax |
| 025-RAGGraph | Knowledge graph RAG | `TAiGraphRAG`, entity relationships |
| 026-RAGGraph-Basic | Basic graph RAG | Simplified graph patterns |
| 027-DocumentManager | Gestor de documentos sobre RAG Graph, 100% local | `TAiRagDocumentManager` + `TAiRagGraph` con `TAiOllamaChat`/`TAiOllamaEmbeddings`; FMX |
| 070-ChatRAGTools | Chat multi-proveedor + RAG SQLite + tools, todo en un formulario | `TAiChatConnection` con selector de proveedor/modelo, `TAiRAGVector` sobre `TAiRAGVectorSQLiteDriver` (vectorial en Delphi puro, sin `vec0`) y `TAiFunctions` con una función que dibuja series en TeeChart |

### MCP Servers (03x)
| Demo | Purpose | Transport |
|------|---------|-----------|
| 031-MCPServer | Multi-protocol server. **Compila tambien en Linux64** desde sep 2026: la herramienta `system_info` llamaba a la API de Windows desde el cuerpo de sus funciones y tumbaba el demo entero fuera de Windows | SSE, HTTP, StdIO |
| 032-MCP_StdIO_FileManager | File manager MCP | StdIO, HTTP, SSE variants |
| 032-MCPServerDataSnap | DataSnap integration | HTTP with DataSnap |
| 035-MCPServerWithTAiFunctions | MCP + function calling | TAiFunctions integration |
| 036-MCPServerStdIO_AiFunction | Function-based MCP | StdIO with AI functions |
| 037-MCPServerRAG | RAG semántico vía MCP (SSE default) | `TAiRAGVector` + `TAiOpenAiEmbeddings`, híbrido embeddings+BM25 sobre SQL Server 2025 o **PostgreSQL + pgvector** (`Driver` en el ini; postgres probado en runtime oct 2026) |

### Video Generation (04x)
| Demo | Purpose | Provider |
|------|---------|----------|
| 041-GeminiVeo | Video generation | Google Veo API |

### Agent Orchestration (05x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 051-AgentDemo | Fanout + join visual: TareaA llama al modelo y TareaB analiza en local, en paralelo; NodoFinal (`jmAll`) une. Limpiado oct 22/2026 (llamada al modelo real, sin el código muerto, `gpt-6-sol`) | `TAIAgentManager`, visual orchestration |
| 052-AgentConsole | Console agent interface: condicional y fork/join | El fork/join usa la API fluida (`AddEdge` repetido + `SetJoinMode`) desde oct 22/2026 |
| 054-AgentCheckpointDB | Durable execution con checkpoints en base de datos | `IAiCheckpointer` + suspend/resume de grafos |
| 072-A2AFederation | Federación de agentes vía protocolo A2A 1.0 (sin LLM) | `TAiA2AServer` expone un grafo (Agent Card + JSON-RPC), `TAiA2AClient` lo consume, `TAiA2ARemoteAgentTool` federa un nodo local al agente remoto; `--otel` para trazas |
| 074-A2AOrchestration | Flujos de orquestación A2A (sin LLM) | Pool de managers con `OnAcquireManager` (3 tasks simultáneos), human-in-the-loop con resume por `taskId`, HITL federado que suspende el nodo local, y `blocking=false` + `GetTask` |
| 079-A2ASutAgent | **Agente SUT del TCK oficial de A2A**: con él se certifica la implementación (MUST 89/89, SHOULD 8/8) | Despacha por PREFIJO del `messageId` (`tck-artifact-text`, `-file-url`, `tck-input-required`…) y es el ejemplo de cómo emitir **artifacts estructurados** desde un grafo vía blackboard. `--port 9999` |
| 080-A2APushNotifications | Push notifications A2A: el agente hace POST a tu webhook al terminar | Agente (8282) + receptor de webhook propio (8283) en el mismo proceso, sin claves. Cubre lo que el TCK **no** cubre: la entrega con `blocking:false`, cuando nadie consulta el task |
| 081-AgentChatHITL | Human-in-the-loop integrado en el chat, sin ventanas modales (sin LLM) | Grafo con `TAiWaitApprovalTool` que suspende sin bloquear threads; `OnSuspend` publica la pregunta como mensaje del chat y la siguiente respuesta del usuario reanuda con `ResumeThread`; `TAiFileCheckpointer` reanuda incluso tras reiniciar la app. **Primer demo con la UI nueva `TAIChatView`/`TAIChatInput` (Skia)** |
| 093-A2AChatAgentTool | **Serie, ep. 17.** **Un chat que consulta a un agente A2A como herramienta**. El agente (catálogo de Café La Ladera) es un grafo sin LLM; el chat usa la key del driver (`--driver`, OpenAI por defecto) o `--sin-llm`. Tiene CLAUDE.md propio | `TAiA2AAgentTool` en un `TAiFunctions` (`DiscoverFromCard` toma la descripción de la Agent Card); hermano de `TAiA2ARemoteAgentTool`, que federa un nodo |
| 094-ChatStreaming | **Serie Café La Ladera, ep. 3.** Consola: prompt de sistema (Lucía, la barista), streaming con tiempo al primer pedazo, historial que recuerda, `SaveToFile`/`NewChat`/`LoadFromFile`. `--sin-streaming`, `--interactivo`. Tiene CLAUDE.md | `OnReceiveData`/`OnReceiveDataEnd` en una clase (`of object`), `CheckSynchronize` en consola |
| 095-ChatViewLadera | **Serie, ep. 4 y 5.** FMX con `TAIChatView`/`TAIChatInput`: streaming en burbujas, paso "Pensando", pie con tokens, imágenes y PDF nativos, nota de voz transcrita por el puente de `cap_Audio`, guardar/abrir, cierre seguro a mitad de turno. `--prueba` para probar sin teclado. Tiene CLAUDE.md | `ChatTools.SpeechTool` (`gpt-transcribe`), `SessionCaps := ModelCaps + [cap_Audio]`, orden del `uses` por el choque de `TAIChatMessage` |
| 096-ChatToolsLadera | **Serie, ep. 7.** Consola: búsqueda web nativa, editor confinado a una carpeta y consola con lista permitida; la última tarea intenta borrar todo y se bloquea. Tiene CLAUDE.md | `cap_WebSearch`/`cap_TextEditor`/`cap_Shell` en `ModelCaps` + `ChatTools.ShellTool`/`TextEditorTool` |
| 097-ChatMemoryLadera | **Serie, ep. 9.** Consola: Marta cuenta sus gustos, el analizador los guarda y en otra sesión, con un chat nuevo, Lucía los recuerda. Tiene CLAUDE.md | `TAiMemory` + `PersistentMemory` + `Analyzer` (`gpt-6-luna`) + embeddings |
| 098-AgentGraphLadera | **Serie, ep. 13.** Consola: un pedido recorre un grafo con fanout, `jmAll`, `AddExpressionEdge` + `AddFallbackEdge`, `AddConditionalEdge` y un `TLLMNode` que redacta el mensaje (`--sin-llm` sin clave). Tiene CLAUDE.md | API fluida de `TAIAgentManager`, blackboard |
| 099-VozPedidoLadera | **Serie, ep. 18 (la cima).** Consola: la clienta pide por voz; GPT-Live delega en un chat que llama por A2A al agente de Pedidos, cuyo nodo Inventario está federado en el agente de Bodega. `--audio archivo.pcm` (prueba sin micrófono) y `--texto`. Tiene CLAUDE.md | `TAiOpenAiLiveChat.DelegateChat` + `TAiA2AAgentTool` + `TAiA2AServer` (x2) + `TAiA2ARemoteAgentTool` |
| 100-MCPLadera | **Serie, ep. 10.** Consola: el mismo `.exe` es servidor MCP por stdio (`--servidor`, para Claude Desktop/Code) y chat que lo lanza como proceso hijo y usa sus herramientas (inventario y precio mayorista); muestra cada `tools/call`. Tiene CLAUDE.md | `TAiMCPStdioServer` + `TAiFunctions.MCPClients` (`tpStdIo`) |
| 101-SkillsLadera | **Serie, ep. 11.** Consola: tres skills de la tienda (cotización con plantilla, reclamos, ficha de cata) bajo demanda; una petición no carga ninguno. Tiene CLAUDE.md | `TAiSkills` (`use_skill`, `read_skill_file`) |
| 102-AgenteLadera | **Serie, ep. 12.** Consola: un `TLLMNode` con el skill de reclamos del 101 y dos herramientas atiende a una clienta; un segundo `TLLMNode` revisa la respuesta. Tiene CLAUDE.md | `TLLMNode.Skill` + `Registry` (`RegisterFromTAiFunctions`), API fluida |
| 103-AprobacionLadera | **Serie, ep. 14.** Consola: un pedido grande se suspende esperando aprobación, guarda el checkpoint y el programa termina; `--responder "texto"` lo reanuda desde disco y un `TLLMNode` redacta. Tiene CLAUDE.md | `TAiWaitApprovalTool` + `TAiFileCheckpointer` + `ResumeThread` |
| 104-GuardrailsLadera | **Serie, ep. 15.** Consola: guardrails sobre las herramientas de Lucía (descuento máximo y anulación bloqueada) y evals de sus respuestas con un modelo juez. `--solo-evals`. Tiene CLAUDE.md | `TAiGuardrails` (`BlockedTools`, `OnCheckToolCall`) + `TAiEvalRunner` (`ExpectJudge`) |
| 105-EnrutadorLadera | **Serie, ep. 16.** Consola: Jev elige a qué agente va cada mensaje de un cliente (pedidos, reclamos, mayorista, carta o una persona) y si es urgente, sin LLM; muestra tiempo y costo. Requiere `TYPESAFE_API_KEY`. Tiene CLAUDE.md | `TAiJevRouterTool` + `lmConditional` + `NextNo` |

### Guardrails, Evals y Memoria (07x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 073-GuardrailsEvals | Política de tool calls y evals (sin LLM) | `TAiGuardrails` (allow/blocklist, patrón en argumentos, veto programático) verificando que el tool bloqueado **no se ejecuta**, + `TAiEvalRunner` con reporte y exit code |
| 075-MCPElicitation | Elicitation MCP (MRTR) desde el **cliente** | `TMCPClientHttp.OnInputRequired` respondiendo accept/decline, y qué se ve si nadie atiende el evento; servidor MCP in-process |
| 076-Memory | Memoria semántica persistente | `TAiMemory` sobre SQLite/FTS5: `Store`/`Search`/`Recall`/`Context`/`Stats` + persistencia. FTS sin API key; híbrido (FTS+semántica con RRF) si hay `OPENAI_API_KEY` |
| 077-RagPostgresConsole | RAG vectorial sobre PostgreSQL+pgvector, **headless y sin API key** | `TAiRAGVectorPostgresDriver` + `TAiOllamaEmbeddings`: `CreateSchema` con HNSW, indexado incremental, filtro de metadatos en SQL (JSONB+GIN) e híbrido BM25+RRF. Es el único demo que ejercita el stack RAG sin abrir el IDE |
| 078-RagChatOllama | **RAG completo 100% local**: recupera de pgvector y responde con `TAiOllamaChat` | Cada pregunta se hace con y sin contexto para que se vea la alucinación frente a la respuesta fundamentada. Documenta la trampa de `num_predict` con modelos de razonamiento (qwen3.5 devolvía vacío) |

### Decisiones con Jev (08x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 084-JevRouter | **Enrutar consultas a agentes especializados** antes de gastar un LLM. Requiere `TYPESAFE_API_KEY`. Tiene CLAUDE.md propio | `TAiJev`: una llamada con Choice (dominio) + Noul por dominio (consultas cruzadas) + Noul `necesita_fuentes` (agente con o sin RAG); reglas y catálogo de agentes en código. Bloque C: lo mismo dentro de un grafo con `TAiJevRouterTool` + `lmConditional` |
| 085-JevDispatchGuard | **Jev en SmartDispatch y en Guardrails** (de herramientas y de entrada). Requiere `TYPESAFE_API_KEY`. Tiene CLAUDE.md propio | `TAiJevDispatchClassifier` (pase 1 de `cmSmartDispatch` sin LLM) y `TAiJevGuardrailClassifier` en `TAiGuardrails.Classifier`: listas para lo enumerable, Jev para el riesgo que ninguna lista anticipa |
| 088-JevModelRouter | **Enrutador de modelos**: cada petición al modelo más barato que alcanza (Groq → DeepSeek → Claude Sonnet → Opus). Requiere claves de Jev, Groq, DeepSeek y Claude. Tiene CLAUDE.md propio | `TAiJevModelRouter`: reglas de nivel en código, calidad medida con `TAiJevEvalScorer` (7/8 útiles), migración del historial entre proveedores en una conversación que escala |
| 089-QwenShowcase | **Qwen (Alibaba Model Studio)**: chat, visión (generar → editar → describir), voz (TTS, transcripción, omni), traducción `qwen-mt`, embeddings + rerank y tiempo real (LLM → voz, traducción simultánea con `DriverParams`); opcionales `--video` y `--voces`. Requiere `DASHSCOPE_API_KEY`. Tiene CLAUDE.md propio | `TAiQwenChat`, `TAiQwenEmbeddings`, `TAiQwenRAGReranker`, `TAiQwenVoices`, `TAiQwenRealtimeTTS`, `TAiRealtimeConnection` |
| 087-JevBatchLabeling | **Etiquetado masivo**: 126 movimientos contables contra 225 cuentas del PUC. Requiere `TYPESAFE_API_KEY`. Tiene CLAUDE.md propio | `TAiJevBatchLabeler` en paralelo: 92.9% total, 100% en lo automatizado (conf ≥ 0.8, 61% de las filas), 4.2 s, US$0.05 |
| 086-JevEvalsRag | **Jev en Evals y en RAG**. Requiere `TYPESAFE_API_KEY`. Tiene CLAUDE.md propio | `TAiJevEvalScorer` en `TAiEvalRunner.Scorer` (`ExpectScore`) y `TAiJevRAGReranker` (evidencia por pasaje + descarte de inyecciones) |

### Skills (09x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 091-Skills | **Skills en formato SKILL.md** (Agent Skills / PPM) de tres formas: bajo demanda en un chat, como base de un nodo de agente y copiados al prompt. Requiere la key del driver (`--driver`, OpenAI por defecto). Tiene CLAUDE.md propio | `TAiSkills` (`use_skill` + `read_skill_file`: el modelo ve el catálogo y carga el skill que necesita), `TLLMNode` + `TAiSkill` (`ResolveConfig`), `TAiPrompts.ApplySkill` |

### Audio / Speech (06x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 060-ElevenLabsTTS | Text-to-speech | ElevenLabs API |
| 061-LoopbackAudioCapture | Capture system playback audio (console) | `TAiAudioCapture` (WASAPI loopback) -> 16 kHz mono WAV |
| 062-BidirectionalTranslator | Real-time bidirectional call translator (console, text output) | 2x `TAiAudioCapture` (loopback + mic) -> 2x `TAiOpenAiRealtimeSTT` -> `TAiChatConnection` translation. Requires `OPENAI_API_KEY`. Session model `gpt-realtime-2.1` (also the driver default since Aug 2026). Transcription uses `gpt-live-transcribe` (Aug 2026) with `TranscriptionPrompt` + guided autodetect via `Languages` (['en','es']). |
| 063-VoiceBridgeTranslator | Full voice bridge: speak Spanish, the meeting hears English TTS (and vice versa) | 062 pipeline + `TAiOpenAiAudio.Speech` (trfPcm) + `TAiAudioPlayer` per side: remote TTS -> default device, own TTS -> VB-CABLE ("CABLE Input"; select "CABLE Output" as mic in the meeting). Uses `TAiAudioCapture.Muted` as anti-feedback while own TTS plays. Falls back to default device with a warning if no cable found. STT on `gpt-live-transcribe` (Aug 2026). |
| 064-VoiceBridgeDiarized | 063 + speaker diarization on the remote channel: each meeting participant gets a stable label and a distinct TTS voice | Remote channel replaces Realtime STT with: local VAD segmenter (level-based, in-demo `TSpeechSegmenter`) -> `Transcribe` with `tmGpt4oDiarize`/`trfDiarizedJson` -> **on-the-fly speaker enrollment** (first time a voice appears, its audio is sliced (2-9 s) and registered via `AddKnownSpeaker` as 'Hablante N' so labels stay consistent across requests; max 4) -> per-speaker translation -> per-speaker TTS voice. Auto-recovers if the API rejects stored speaker samples (clears and re-enrolls). [YO] channel stays on Realtime STT (single speaker, lower latency; `gpt-live-transcribe` since Aug 2026 — the diarized remote channel stays on `gpt-4o-transcribe-diarize` because the new models don't support diarization). |
| 065-VoiceBridgeUI | El puente de voz del 063 con interfaz FMX: selección de dispositivos y control en pantalla | `TAiAudioCapture`/`TAiAudioPlayer` + `TAiOpenAiRealtimeSTT` + `TAiOpenAiAudio` sobre `TAiChatConnection`. **Sin CLAUDE.md propio** |
| 092-GPTLiveVoice | **OpenAI GPT-Live**: conversación de voz full-duplex (se le puede interrumpir) con delegación a Responses o a tu propio chat. FMX. Requiere `OPENAI_API_KEY`. Tiene CLAUDE.md propio | `TAiAudioCapture` (micrófono) → `TAiOpenAiLiveChat` → `TAiAudioPlayer`; `TAiFunctions` (hora, luz que cambia en pantalla); `DelegateChat` opcional; RAG del 037 por MCP con `TAiGuardrails` de solo lectura; "Modo altavoz" anti-eco por energía del audio de salida, diagnóstico de lo que el modelo oyó mientras hablaba y guía rápida de preguntas. Probado en vivo |
| 071-VoiceBridgeTranslate | **Refactor of 063 using `gpt-realtime-translate`**: one WebSocket per direction replaces the whole STT -> LLM-translate -> TTS pipeline | 2x `TAiAudioCapture` -> 2x `TAiOpenAiRealtimeTranslate` (continuous stream, no VAD/turns) -> `TAiAudioPlayer`. The server returns translated text (`OnAssistantTextDelta`) AND translated TTS audio (`OnAudioChunk`, PCM16 24 kHz) in streaming; `SourceTranscription := True` also shows what was heard. Lower latency and ~1/3 of the code vs 063; trade-off: the TTS voice is chosen by the server. Same VB-CABLE setup and `Muted` anti-feedback as 063. |

### Tests runtime de issues y subsistemas (06x, 08x)

Consolas con asserts automáticos que reproducen un issue concreto contra APIs reales. No son la suite de regresión (esa vive en `Tests/RegressionSuite/` y no necesita claves); estos **sí requieren API key** y sirven para verificar un fix de punta a punta. **Solo el 082 y el 083 tienen CLAUDE.md propio.**

| Demo | Verifica | Requiere |
|------|----------|----------|
| 066-ComputerUseTest | Computer Use: 3 proyectos (`ComputerUseTest`, `ComputerUseTest01` y `ComputerUseServer`, este último con loop manual en formato OpenAI contra el servidor cimamaker). Ver `CAPTURA_PANTALLA.md` | según proyecto |
| 067-PromptCacheTest | Prompt caching de Claude: dos requests con el mismo system prompt grande (>1024 tokens) y `CacheContext`; espera `cache_write>0` y luego `cache_read>0` | `CLAUDE_API_KEY` |
| 068-AsyncHistoryTest | Issue #105: en asíncrono Claude debe archivar sus respuestas en el historial. Conversación multi-turno donde el último turno debe recordar los previos | `CLAUDE_API_KEY` |
| 069-McpSessionGateTest | Issue #110: vetting de cliente MCP (`OnClientConnect`) y gate de sesión HTTP por `Mcp-Session-Id`. Levanta un `TAiMCPHttpServer` real y lo prueba como cliente en 4 fases | — (in-process) |
| 083-ComputerUseLinux | **Computer Use en Linux (X11)**: ciclo agéntico real sobre Xvfb con `TAiLinuxExecutor` (xdotool + scrot). Es el 066 pero headless y sin tocar el escritorio del usuario. Compila Linux64; en Win64 solo imprime un aviso. Tiene CLAUDE.md propio | `OPENAI_API_KEY` o `CLAUDE_API_KEY` |
| 082-ComputerUsePassthru | **Quién ejecuta un `computer_call`**: delegado vía `OnCallToolFunction` (broker headless), sin `ComputerUseTool` asignado, y local con ejecutor simulado — los tres en síncrono y en streaming, porque el driver de OpenAI tiene dos implementaciones. Verifica con asserts y exit code; **no toca la pantalla** (a diferencia del 066). Tiene CLAUDE.md propio | `OPENAI_API_KEY` |

### Utilities (09x)
| Demo | Purpose |
|------|---------|
| 090-Varios/ListAllModels | List available models across providers |
| 090-Varios/CompareEmbeddings | Compare embedding quality |
| 090-Varios/Dalle_Generate | Generación de imágenes (hoy `gpt-image-1`; DALL-E fue retirado) |
| 090-Varios/VoiceMonitorAndWhisperDemo | `TAiVoiceMonitor` + transcripción con Whisper |

### Libro "IA con Delphi", edición MakerAI 4.0 (`Demos/Libro/`)

Un proyecto por capítulo, en `Demos/Libro/capNN-tema/`. La app FMX `LaderaApp` (Café La Ladera, Lucía) **crece de capítulo en capítulo** sobre `TAIChatView`/`TAIChatInput`: cada carpeta es la etapa completa de ese capítulo, no un diff. Casi todas traen además una consola con lo mismo "desde código". Todas aceptan `--prueba "pregunta" "@archivo|pregunta" /nueva --captura x.png --registro t.txt` (corridas sin teclado; las capturas y cifras del libro salen de ahí). Las apps hacen `SetCurrentDir` a `Documentos\CafeLaLadera\trabajo`: rutas de `--captura`/`--registro` absolutas.

| Carpeta | Capítulo | Qué agrega |
|---|---|---|
| cap03-primer-chat | 3 | Primer chat: consola `PrimerChat` y FMX `PrimerChatForm` |
| cap04-conversacion | 4 | Historial, streaming asíncrono |
| cap05-prompts | 5 | `TAiPrompts`, prompt de sistema de Lucía (consola) |
| cap06-proveedores | 6 | Mismo chat con varios drivers, `ModelCaps` por driver (consola) |
| cap07-chat-en-pantalla | 7 | Primera etapa de `LaderaApp`: vista, entrada, Nueva/Guardar/Abrir |
| cap08-filtro | 8 | Sanitizador + `TAiJevPromptGuard` (`Scope`); consola `FiltroLadera` |
| cap09-sentidos | 9 | Imagen, nota de voz (`SpeechTool`, `VoiceMonitor`), `SessionCaps` |
| cap10-documentos | 10 | PDF nativo; consola `DocumentosLadera` (OpenAI/Claude/Groq) |
| cap11-herramientas | 11 | `TAiFunctions` con inventario y total; consola `FuncionesLadera` |
| cap12-herramientas-sistema | 12 | Web, `TAiTextEditorTool` confinado, `TAiShell` con lista permitida |
| cap13-conocimiento | 13 | Manual en `TAiRAGVector` + SQLite, `TAiJevRAGReranker`, VQL; `buscar_en_el_manual`; consola `ConocimientoLadera` |
| cap14-grafos | 14 | `TAiRagGraph` + `TAiRagGraphBuilder` desde `grafo\tripletas.json`, MakerGQL; consola `GrafosLadera --extraer` |

Los proyectos se generan con scripts `_crear_*.py` que se borran después; los `.pas/.fmx/.dpr` van en UTF-8 con BOM y CRLF. Requieren `OPENAI_API_KEY` (cap 8+ también `TYPESAFE_API_KEY`). El manuscrito vive fuera del repo (`D:\Documentos\LibroIA\manuscript-v4`).

## Common Runtime Dependencies

- **Ollama**: Required for local models (default: `localhost:11434`)
- **API Keys**: OpenAI, Claude, Gemini keys in environment or config
- **PostgreSQL + pgvector**: For 021-RAG demos
- **SQLite**: For 022-RAG demos (included with Delphi)

## Navigation Pattern

Each demo subdirectory contains:
- `*.dproj` - Delphi project file
- `*.dpr` - Program source
- `uMain*.pas` / `uMain*.fmx` - Main form
- `CLAUDE.md` - Demo-specific instructions

## Common Code Patterns Across Demos

Los eventos de MakerAI son `of object`: se asignan **métodos** del formulario o de una clase, no métodos anónimos (no compila). Las firmas exactas: `TAiChatOnDataEvent` en `Source/Core/uMakerAi.Chat.pas` y `TAiErrorEvent` en `Source/Core/uMakerAi.Core.pas`.

**Synchronous AI call:**
```pascal
AiConn.Params.Values['Asynchronous'] := 'False';
Response := AiConn.AddMessageAndRun(Prompt, 'user', []);
```

**Streaming with callback:**
```pascal
AiConn.Params.Values['Asynchronous'] := 'True';
AiConn.OnReceiveData := AiConnReceiveData;   // metodo del formulario (TAiChatOnDataEvent)
AiConn.AddMessageAndRun(Prompt, 'user', []);

procedure TForm1.AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
begin
  TThread.Queue(nil, procedure begin Memo1.Text := Memo1.Text + aText; end);
end;
```

**Tools y capacidades (canal tipado):**
```pascal
AiConn.AiFunctions := AiFunctions1;
AiConn.Tool_Active := True;               // no Params.Values['Tool_Active']
AiConn.ModelCaps := [cap_Image];
```

**Error handling:**
```pascal
AiConn.OnError := AiConnError;            // metodo del formulario (TAiErrorEvent)
```

**Demos de consola con streaming:** cuando la salida va a un pipe (grabaciones, logs, `capturar_consola.py` del skill de videos), el RTL la junta en bloques de 128 bytes y el streaming se ve de golpe. Los demos 093-099 ponen al arrancar `SetTextBuf(Output, BufSalida, 1)` (global `BufSalida: array [0 .. 0] of Byte`). `TTextRec(Output).FlushFunc := InOutFunc` no funciona en Delphi 13.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for project overview, build instructions, and architecture details.
