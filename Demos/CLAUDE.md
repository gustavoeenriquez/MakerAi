# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is the Demos directory for the MakerAI 3.x framework. Contains 18+ working example applications demonstrating AI integration patterns for Delphi developers. Each demo has its own CLAUDE.md with specific implementation details.

## Building Demos

**IDE:** Delphi 11 Alexandria through 13 Florence (demos require Delphi 11+; the core framework supports 10.4 Sydney minimum)

**Group project:** Open `DemosVersion31.groupproj` in Delphi IDE to access all demos.

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
| 053-DemoAgentesTools | Agents with tools | Tool integration in agent flows |

### Audio / Speech (06x)
| Demo | Purpose | Key Pattern |
|------|---------|-------------|
| 060-ElevenLabsTTS | Text-to-speech | ElevenLabs API |
| 061-LoopbackAudioCapture | Capture system playback audio (console) | `TAiAudioCapture` (WASAPI loopback) -> 16 kHz mono WAV |
| 062-BidirectionalTranslator | Real-time bidirectional call translator (console, text output) | 2x `TAiAudioCapture` (loopback + mic) -> 2x `TAiOpenAiRealtimeSTT` -> `TAiChatConnection` translation. Requires `OPENAI_API_KEY`. Note: sets `Model := 'gpt-realtime'` (the driver default `gpt-4o-realtime-preview` was retired by OpenAI). Transcription uses `gpt-live-transcribe` (Aug 2026) with `TranscriptionPrompt` + guided autodetect via `Languages` (['en','es']). |
| 063-VoiceBridgeTranslator | Full voice bridge: speak Spanish, the meeting hears English TTS (and vice versa) | 062 pipeline + `TAiOpenAiAudio.Speech` (trfPcm) + `TAiAudioPlayer` per side: remote TTS -> default device, own TTS -> VB-CABLE ("CABLE Input"; select "CABLE Output" as mic in the meeting). Uses `TAiAudioCapture.Muted` as anti-feedback while own TTS plays. Falls back to default device with a warning if no cable found. STT on `gpt-live-transcribe` (Aug 2026). |
| 064-VoiceBridgeDiarized | 063 + speaker diarization on the remote channel: each meeting participant gets a stable label and a distinct TTS voice | Remote channel replaces Realtime STT with: local VAD segmenter (level-based, in-demo `TSpeechSegmenter`) -> `Transcribe` with `tmGpt4oDiarize`/`trfDiarizedJson` -> **on-the-fly speaker enrollment** (first time a voice appears, its audio is sliced (2-9 s) and registered via `AddKnownSpeaker` as 'Hablante N' so labels stay consistent across requests; max 4) -> per-speaker translation -> per-speaker TTS voice. Auto-recovers if the API rejects stored speaker samples (clears and re-enrolls). [YO] channel stays on Realtime STT (single speaker, lower latency; `gpt-live-transcribe` since Aug 2026 — the diarized remote channel stays on `gpt-4o-transcribe-diarize` because the new models don't support diarization). |

| 071-VoiceBridgeTranslate | **Refactor of 063 using `gpt-realtime-translate`**: one WebSocket per direction replaces the whole STT -> LLM-translate -> TTS pipeline | 2x `TAiAudioCapture` -> 2x `TAiOpenAiRealtimeTranslate` (continuous stream, no VAD/turns) -> `TAiAudioPlayer`. The server returns translated text (`OnAssistantTextDelta`) AND translated TTS audio (`OnAudioChunk`, PCM16 24 kHz) in streaming; `SourceTranscription := True` also shows what was heard. Lower latency and ~1/3 of the code vs 063; trade-off: the TTS voice is chosen by the server. Same VB-CABLE setup and `Muted` anti-feedback as 063. |

### Utilities (09x)
| Demo | Purpose |
|------|---------|
| 090-Varios/ListAllModels | List available models across providers |
| 090-Varios/CompareEmbeddings | Compare embedding quality |

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
