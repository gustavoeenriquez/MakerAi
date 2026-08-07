# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is the Demos directory for the MakerAI 3.x framework. Contains 48 working example projects demonstrating AI integration patterns for Delphi developers. La mayoria tiene su propio CLAUDE.md con los detalles de implementacion; los que no, estan marcados abajo.

**Estado de compilación (ago 7/2026): los 48 proyectos compilan** en Win64/Release.

## Building Demos

**IDE:** Delphi 11 Alexandria through 13 Florence (demos require Delphi 11+; the core framework supports 10.4 Sydney minimum)

**Group project:** Open `DemosVersion31.groupproj` in Delphi IDE to access all demos. El grupo se regeneró en ago 2026 e incluye los 48 proyectos.

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
| 031-MCPServer | Multi-protocol server | SSE, HTTP, StdIO |
| 032-MCP_StdIO_FileManager | File manager MCP | StdIO, HTTP, SSE variants |
| 032-MCPServerDataSnap | DataSnap integration | HTTP with DataSnap |
| 035-MCPServerWithTAiFunctions | MCP + function calling | TAiFunctions integration |
| 036-MCPServerStdIO_AiFunction | Function-based MCP | StdIO with AI functions |
| 037-MCPServerRAG | RAG semántico vía MCP (SSE default) | `TAiRAGVector` + `TAiOpenAiEmbeddings`, híbrido embeddings+BM25, persistencia `rag_index.mkai` |

### Video Generation (04x)
| Demo | Purpose | Provider |
|------|---------|----------|
| 041-GeminiVeo | Video generation | Google Veo API |

### Agent Orchestration (05x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 051-AgentDemo | Agent graph workflows | `TAIAgentManager`, visual orchestration |
| 052-AgentConsole | Console agent interface | Command-line agent execution |
| 054-AgentCheckpointDB | Durable execution con checkpoints en base de datos | `IAiCheckpointer` + suspend/resume de grafos |
| 072-A2AFederation | Federación de agentes vía protocolo A2A 1.0 (sin LLM) | `TAiA2AServer` expone un grafo (Agent Card + JSON-RPC), `TAiA2AClient` lo consume, `TAiA2ARemoteAgentTool` federa un nodo local al agente remoto; `--otel` para trazas |
| 074-A2AOrchestration | Flujos de orquestación A2A (sin LLM) | Pool de managers con `OnAcquireManager` (3 tasks simultáneos), human-in-the-loop con resume por `taskId`, HITL federado que suspende el nodo local, y `blocking=false` + `GetTask` |
| 079-A2ASutAgent | **Agente SUT del TCK oficial de A2A**: con él se certifica la implementación (MUST 89/89, SHOULD 8/8) | Despacha por PREFIJO del `messageId` (`tck-artifact-text`, `-file-url`, `tck-input-required`…) y es el ejemplo de cómo emitir **artifacts estructurados** desde un grafo vía blackboard. `--port 9999` |
| 080-A2APushNotifications | Push notifications A2A: el agente hace POST a tu webhook al terminar | Agente (8282) + receptor de webhook propio (8283) en el mismo proceso, sin claves. Cubre lo que el TCK **no** cubre: la entrega con `blocking:false`, cuando nadie consulta el task |

### Guardrails, Evals y Memoria (07x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 073-GuardrailsEvals | Política de tool calls y evals (sin LLM) | `TAiGuardrails` (allow/blocklist, patrón en argumentos, veto programático) verificando que el tool bloqueado **no se ejecuta**, + `TAiEvalRunner` con reporte y exit code |
| 075-MCPElicitation | Elicitation MCP (MRTR) desde el **cliente** | `TMCPClientHttp.OnInputRequired` respondiendo accept/decline, y qué se ve si nadie atiende el evento; servidor MCP in-process |
| 076-Memory | Memoria semántica persistente | `TAiMemory` sobre SQLite/FTS5: `Store`/`Search`/`Recall`/`Context`/`Stats` + persistencia. FTS sin API key; híbrido (FTS+semántica con RRF) si hay `OPENAI_API_KEY` |
| 077-RagPostgresConsole | RAG vectorial sobre PostgreSQL+pgvector, **headless y sin API key** | `TAiRAGVectorPostgresDriver` + `TAiOllamaEmbeddings`: `CreateSchema` con HNSW, indexado incremental, filtro de metadatos en SQL (JSONB+GIN) e híbrido BM25+RRF. Es el único demo que ejercita el stack RAG sin abrir el IDE |
| 078-RagChatOllama | **RAG completo 100% local**: recupera de pgvector y responde con `TAiOllamaChat` | Cada pregunta se hace con y sin contexto para que se vea la alucinación frente a la respuesta fundamentada. Documenta la trampa de `num_predict` con modelos de razonamiento (qwen3.5 devolvía vacío) |

### Audio / Speech (06x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 060-ElevenLabsTTS | Text-to-speech | ElevenLabs API |
| 061-LoopbackAudioCapture | Capture system playback audio (console) | `TAiAudioCapture` (WASAPI loopback) -> 16 kHz mono WAV |
| 062-BidirectionalTranslator | Real-time bidirectional call translator (console, text output) | 2x `TAiAudioCapture` (loopback + mic) -> 2x `TAiOpenAiRealtimeSTT` -> `TAiChatConnection` translation. Requires `OPENAI_API_KEY`. Note: sets `Model := 'gpt-realtime'` (the driver default `gpt-4o-realtime-preview` was retired by OpenAI). Transcription uses `gpt-live-transcribe` (Aug 2026) with `TranscriptionPrompt` + guided autodetect via `Languages` (['en','es']). |
| 063-VoiceBridgeTranslator | Full voice bridge: speak Spanish, the meeting hears English TTS (and vice versa) | 062 pipeline + `TAiOpenAiAudio.Speech` (trfPcm) + `TAiAudioPlayer` per side: remote TTS -> default device, own TTS -> VB-CABLE ("CABLE Input"; select "CABLE Output" as mic in the meeting). Uses `TAiAudioCapture.Muted` as anti-feedback while own TTS plays. Falls back to default device with a warning if no cable found. STT on `gpt-live-transcribe` (Aug 2026). |
| 064-VoiceBridgeDiarized | 063 + speaker diarization on the remote channel: each meeting participant gets a stable label and a distinct TTS voice | Remote channel replaces Realtime STT with: local VAD segmenter (level-based, in-demo `TSpeechSegmenter`) -> `Transcribe` with `tmGpt4oDiarize`/`trfDiarizedJson` -> **on-the-fly speaker enrollment** (first time a voice appears, its audio is sliced (2-9 s) and registered via `AddKnownSpeaker` as 'Hablante N' so labels stay consistent across requests; max 4) -> per-speaker translation -> per-speaker TTS voice. Auto-recovers if the API rejects stored speaker samples (clears and re-enrolls). [YO] channel stays on Realtime STT (single speaker, lower latency; `gpt-live-transcribe` since Aug 2026 — the diarized remote channel stays on `gpt-4o-transcribe-diarize` because the new models don't support diarization). |
| 065-VoiceBridgeUI | El puente de voz del 063 con interfaz FMX: selección de dispositivos y control en pantalla | `TAiAudioCapture`/`TAiAudioPlayer` + `TAiOpenAiRealtimeSTT` + `TAiOpenAiAudio` sobre `TAiChatConnection`. **Sin CLAUDE.md propio** |
| 071-VoiceBridgeTranslate | **Refactor of 063 using `gpt-realtime-translate`**: one WebSocket per direction replaces the whole STT -> LLM-translate -> TTS pipeline | 2x `TAiAudioCapture` -> 2x `TAiOpenAiRealtimeTranslate` (continuous stream, no VAD/turns) -> `TAiAudioPlayer`. The server returns translated text (`OnAssistantTextDelta`) AND translated TTS audio (`OnAudioChunk`, PCM16 24 kHz) in streaming; `SourceTranscription := True` also shows what was heard. Lower latency and ~1/3 of the code vs 063; trade-off: the TTS voice is chosen by the server. Same VB-CABLE setup and `Muted` anti-feedback as 063. |

### Tests runtime de issues y subsistemas (06x)

Consolas con asserts automáticos que reproducen un issue concreto contra APIs reales. No son la suite de regresión (esa vive en `Tests/RegressionSuite/` y no necesita claves); estos **sí requieren API key** y sirven para verificar un fix de punta a punta. **Ninguno tiene CLAUDE.md propio.**

| Demo | Verifica | Requiere |
|------|----------|----------|
| 066-ComputerUseTest | Computer Use: 3 proyectos (`ComputerUseTest`, `ComputerUseTest01` y `ComputerUseServer`, este último con loop manual en formato OpenAI contra el servidor cimamaker). Ver `CAPTURA_PANTALLA.md` | según proyecto |
| 067-PromptCacheTest | Prompt caching de Claude: dos requests con el mismo system prompt grande (>1024 tokens) y `CacheContext`; espera `cache_write>0` y luego `cache_read>0` | `CLAUDE_API_KEY` |
| 068-AsyncHistoryTest | Issue #105: en asíncrono Claude debe archivar sus respuestas en el historial. Conversación multi-turno donde el último turno debe recordar los previos | `CLAUDE_API_KEY` |
| 069-McpSessionGateTest | Issue #110: vetting de cliente MCP (`OnClientConnect`) y gate de sesión HTTP por `Mcp-Session-Id`. Levanta un `TAiMCPHttpServer` real y lo prueba como cliente en 4 fases | — (in-process) |

### Utilities (09x)
| Demo | Purpose |
|------|---------|
| 090-Varios/ListAllModels | List available models across providers |
| 090-Varios/CompareEmbeddings | Compare embedding quality |
| 090-Varios/Dalle_Generate | Generación de imágenes (hoy `gpt-image-1`; DALL-E fue retirado) |
| 090-Varios/VoiceMonitorAndWhisperDemo | `TAiVoiceMonitor` + transcripción con Whisper |

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

**Synchronous AI call:**
```pascal
AiModel.Asynchronous := False;
Response := AiModel.AddMessageAndRun(Prompt, 'user', []);
```

**Streaming with callback:**
```pascal
AiModel.Asynchronous := True;
AiModel.OnReceiveData := procedure(Sender: TObject; Data: string; IsDone: Boolean)
begin
  TThread.Synchronize(nil, procedure begin Memo1.Text := Memo1.Text + Data; end);
end;
AiModel.AddMessageAndRun(Prompt, 'user', []);
```

**Error handling:**
```pascal
AiModel.OnError := procedure(Sender: TObject; ErrorMsg: string; E: Exception; Resp: IHTTPResponse)
begin
  ShowMessage(ErrorMsg);
end;
```

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for project overview, build instructions, and architecture details.
