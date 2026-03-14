# MakerAI Suite — Free Pascal Port

Complete port of [MakerAI Suite v3.3](https://github.com/gustavoeenriquez/MakerAi) for **Free Pascal (FPC) 3.2+**.

MakerAI Suite is an AI orchestration library that lets you integrate multiple LLM providers (OpenAI, Claude, Gemini, Ollama, and more) into Pascal applications with a unified API. This repository is the FPC/Lazarus port of the original Delphi library.

- **Original Delphi library:** https://github.com/gustavoeenriquez/MakerAi  (`master` branch)
- **Official site:** https://makerai.cimamaker.com
- **Author:** Gustavo Enriquez — gustavoeenriquez@gmail.com
- **License:** MIT
- **[Leer en español](README.es.md)**

---

## Port status

```
████████████████████████████████████████████████████████████  100%
```

| Module | Status |
|--------|--------|
| Core (Chat base, Messages, Tools, Factory) | ✅ Complete |
| 12 LLM Drivers (all sync + async/streaming) | ✅ Complete |
| TAiChatConnection (universal connector) | ✅ Complete |
| Tools (Functions, Shell, TextEditor, ComputerUse) | ✅ Complete |
| Multimedia (Whisper, OpenAI Audio, Gemini Speech, DALL-E) | ✅ Complete |
| Embeddings (TAiEmbeddingsCore + TAiEmbeddings OpenAI) | ✅ Complete |
| RAG Vector (HNSW + BM25 + RRF + VQL) | ✅ Complete |
| RAG Graph (knowledge graph + GQL) | ✅ Complete |
| Agents (multi-agent + Blackboard + Checkpoint) | ✅ Complete |
| MCP Server (HTTP, SSE, StdIO, Direct, Bridge) | ✅ Complete |
| MCP Client (HTTP, SSE, StdIO) | ✅ Complete |
| Demos (40+ demo programs) | ✅ Complete |

---

## Features

### LLM Drivers

| Driver | Class | API |
|--------|-------|-----|
| **OpenAI** | `TAiOpenChat` | Responses API `/v1/responses`, typed SSE, shell/patch/image gen |
| **Claude** | `TAiClaudeChat` | Anthropic API, thinking, citations, context management |
| **Gemini** | `TAiGeminiChat` | Google `generateContent`, thinking budget, brace-counting SSE |
| **Groq** | `TAiGroqChat` | reasoning_format, reasoning_effort |
| **Mistral** | `TAiMistralChat` | OCR, Document QnA, Magistral reasoning, content-array |
| **Grok** | `TAiGrokChat` | xAI, web search (`/v1/responses`), image gen |
| **Cohere** | `TCohereChat` | v2 API, SSE event/data blocks, citations, rerank |
| **DeepSeek** | `TAiDeepSeekChat` | reasoning_content (deepseek-reasoner) |
| **Kimi** | `TAiKimiChat` | Moonshot AI (`moonshot-v1-8k`, `kimi-k2`) |
| **Ollama** | `TAiOllamaChat` | Native API `/api/chat`, model management, thinking |
| **LM Studio** | `TAiLMStudioChat` | OpenAI-compat local `127.0.0.1:1234` |
| **GenericLLM** | `TAiGenericChat` | Any OpenAI-compatible endpoint (vLLM, etc.) |

### Universal Connector

`TAiChatConnection` — switch provider at runtime by setting `DriverName`. Your application code stays the same. Parameter injection via `TypInfo` (FPC).

### Tools

| Unit | Class | Description |
|------|-------|-------------|
| `uMakerAi.Tools.Functions` | `TAiFunctions` | Full function calling — JSON schema, typed params, automatic LLM invocation |
| `uMakerAi.Tools.Shell` | `TAiShell` | Interactive shell with persistent session, timeout, audit log |
| `uMakerAi.Tools.TextEditor` | `TAiTextEditorTool` | view/create/str_replace/insert/undo with virtualized I/O |
| `uMakerAi.Tools.ComputerUse` | `TAiComputerUseTool` | Universal Computer Use adapter (Claude/Gemini/OpenAI) |

### Multimedia

| Unit | Class | Description |
|------|-------|-------------|
| `uMakerAi.Whisper` | `TAIWhisper` | OpenAI TTS + STT/Whisper, audio format conversion via ffmpeg |
| `uMakerAi.OpenAI.Audio` | `TAiOpenAiAudio` | TTS (tts-1/tts-1-hd/gpt-4o-mini-tts), streaming, verbose_json transcription, translation |
| `uMakerAi.Gemini.Speech` | `TAiGeminiSpeechTool` | Gemini multi-speaker TTS (PCM→WAV), multimodal transcription |
| `uMakerAi.OpenAi.Dalle` | `TAiDalle` | DALL-E 2/3 — URL + b64_json, generation and auto-download |

### Embeddings

| Unit | Class | Description |
|------|-------|-------------|
| `uMakerAi.Embeddings.Core` | `TAiEmbeddingsCore` | Vector base class — CosineSimilarity, DotProduct, EuclideanDistance, FindNearest, FindTopK, AverageEmbedding, overloaded operators |
| `uMakerAi.Embeddings` | `TAiEmbeddings` | OpenAI driver (`text-embedding-3-small/large`) |

### RAG Vector

`TAiRAGVector` — in-memory vector knowledge base with hybrid search:

- **HNSW** index (cosine similarity search)
- **BM25** lexical index (keyword search)
- **RRF** or weighted hybrid fusion
- Metadata filters (`TAiFilterCriteria`)
- **VQL** query language (SQL-like DSL)
- `AddItem(text, metadata)`, `SearchText(query, limit, precision, filter)`, `ExecuteVGQL(query)`

### RAG Graph

`TAiRagGraph` — knowledge graph with semantic + structural search:

- Nodes (`TAiRagGraphNode`) and edges (`TAiRagGraphEdge`) with their own embeddings
- Vector-based semantic search with depth expansion
- **GQL** (Cypher-like): `MATCH`, `WHERE`, `RETURN`, `GET SHORTEST PATH`, `GET DEGREES TOP N`, `SHOW LABELS/EDGES`
- Programmatic match patterns (`TGraphMatchQuery`)
- Graph algorithms: `GetShortestPath`, `GetAllShortestPaths`, `GetNodesByDegree`, `DetectCommunities`, `GetClosenessCentrality`
- Export: `.mkai` (MakerAI), DOT, GraphML

### Agents

Graph-based multi-agent orchestration framework with human-in-the-loop:

| Class | Description |
|-------|-------------|
| `TAIAgentManager` | Main agent graph orchestrator |
| `TAIAgentsNode` | Execution unit (connects to an LLM driver) |
| `TAIAgentsLink` | Edge with modes: `lmFanout`, `lmConditional`, `lmManual` |
| `TAIBlackboard` | Thread-safe shared state between nodes |
| `TAiFileCheckpointer` | Durable execution — pause and resume agents |
| `TAiWaitApprovalTool` | Human-in-the-loop — suspends and waits for human approval |

### MCP (Model Context Protocol)

**Server:** exposes Pascal functions as MCP tools consumable by any client (Claude Desktop, etc.):

| Transport | Class | Description |
|-----------|-------|-------------|
| HTTP | `TAiMCPHttpServer` | `TFPHTTPServer` in thread, CORS, `@ApiKey` auth |
| SSE | `TAiMCPSSEHttpServer` | SSE over ssockets (experimental) |
| StdIO | `TAiMCPStdioServer` | stdin/stdout — recommended for CLI |
| Direct | `TAiMCPDirectConnection` | In-process, no network |
| Bridge | `TTAiFunctionToolProxy` | Auto-adapts `TAiFunctions` → `IAiMCPTool` |

**Client:** `TMCPClientCustom` — consume external MCP servers (StdIO, HTTP, SSE).

---

## Quick start

### Synchronous chat (any driver)

```pascal
uses
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

var
  Conn: TAiChatConnection;
  Resp: string;
begin
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := 'Ollama';
    Conn.Params.Values['Model']        := 'gemma3:1b';
    Conn.Params.Values['Asynchronous'] := 'False';

    Resp := Conn.AddMessageAndRun('Hello, who are you?', 'user');
    WriteLn(Resp);
    WriteLn('Tokens: ', Conn.Total_tokens);
  finally
    Conn.Free;
  end;
end.
```

### Streaming / async chat

```pascal
type
  TEvents = class
    Done: Boolean;
    procedure OnData(const Sender: TObject; aMsg: TAiChatMessage;
        aResponse: TJSONObject; aRole, aText: string);
    procedure OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
        aResponse: TJSONObject; aRole, aText: string);
  end;

var
  Conn: TAiChatConnection;
  Ev: TEvents;
begin
  Ev   := TEvents.Create;
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := 'Claude';
    Conn.Params.Values['Model']        := 'claude-haiku-4-5-20251001';
    Conn.Params.Values['Asynchronous'] := 'True';
    Conn.OnReceiveData    := @Ev.OnData;
    Conn.OnReceiveDataEnd := @Ev.OnDataEnd;

    Conn.AddMessageAndRun('Tell me a joke', 'user');
    while not Ev.Done do Sleep(50);
  finally
    Conn.Free; Ev.Free;
  end;
end.
```

### Embeddings + cosine similarity

```pascal
uses uMakerAi.Embeddings, uMakerAi.Embeddings.Core;

var
  Emb: TAiEmbeddings;
  V1, V2: TAiEmbeddingData;
begin
  Emb := TAiEmbeddings.Create(nil);
  try
    Emb.ApiKey := '@OPENAI_API_KEY';
    V1 := Emb.CreateEmbedding('machine learning', '');
    V2 := Emb.CreateEmbedding('deep neural networks', '');
    WriteLn(TAiEmbeddingsCore.CosineSimilarity(V1, V2):0:4);
  finally
    Emb.Free;
  end;
end.
```

### RAG Vector — knowledge base

```pascal
uses uMakerAi.RAG.Vectors, uMakerAi.RAG.MetaData, uMakerAi.Embeddings;

var
  Emb : TAiEmbeddings;
  RAG : TAiRAGVector;
  Meta: TAiEmbeddingMetaData;
begin
  Emb := TAiEmbeddings.Create(nil);
  RAG := TAiRAGVector.Create(nil);
  try
    Emb.ApiKey     := '@OPENAI_API_KEY';
    RAG.Embeddings := Emb;

    Meta := TAiEmbeddingMetaData.Create;
    try
      Meta['category'] := 'science';
      RAG.AddItem('Machine learning requires large datasets.', Meta);
    finally Meta.Free; end;

    RAG.BuildIndex;
    RAG.BuildLexicalIndex;
    WriteLn(RAG.SearchText('AI and data', 3, 0.3, nil));
  finally
    RAG.Free; Emb.Free;
  end;
end.
```

### RAG Graph — knowledge graph

```pascal
uses uMakerAi.RAG.Graph.Core, uMakerAi.Embeddings;

var
  Emb  : TAiEmbeddings;
  Graph: TAiRagGraph;
  N1, N2: TAiRagGraphNode;
begin
  Emb   := TAiEmbeddings.Create(nil);
  Graph := TAiRagGraph.Create(nil);
  try
    Emb.ApiKey       := '@OPENAI_API_KEY';
    Graph.Embeddings := Emb;

    N1 := Graph.AddNode('openai', 'COMPANY', 'OpenAI');
    N1.Text := 'OpenAI is an AI company that created GPT-4.';
    N1.Data := Emb.CreateEmbedding(N1.Text, 'user');

    N2 := Graph.AddNode('gpt4', 'MODEL', 'GPT-4');
    N2.Text := 'GPT-4 is a large multimodal language model.';
    N2.Data := Emb.CreateEmbedding(N2.Text, 'user');

    Graph.AddEdge(N1, N2, 'e1', 'CREATED', 'created');
    Graph.RebuildIndexes;
    Graph.Nodes.BuildIndex;

    WriteLn(Graph.SearchText('language model for chat', 0, True, 3, 0.3));
    WriteLn(Graph.ExecuteMakerGQL(
      'MATCH (c:COMPANY) -[r:CREATED]-> (m:MODEL) RETURN c, r, m'));
  finally
    Graph.Free; Emb.Free;
  end;
end.
```

### MCP Server (HTTP)

```pascal
uses uMakerAi.MCPServer.Core, uMakerAi.MCPServer.Http;

var
  Logic : TAiMCPLogicServer;
  Server: TAiMCPHttpServer;
begin
  Logic := TAiMCPLogicServer.Create(nil);
  Logic.ServerName := 'MyServer';
  Logic.RegisterTool(TMyTool.Create);   // IAiMCPTool

  Server := TAiMCPHttpServer.Create(nil);
  Server.LogicServer := Logic;
  Server.Port        := 8088;
  Server.Active      := True;   // opens server in background thread
  ReadLn;
end.
```

### Function Calling

```pascal
uses uMakerAi.Tools.Functions;

var
  Funcs: TAiFunctions;
  Fn   : TFunctionActionItem;
begin
  Funcs := TAiFunctions.Create(nil);
  Fn    := Funcs.Functions.Add;
  Fn.FunctionName := 'get_weather';
  Fn.Description.Add('Returns current weather for a city.');
  with Fn.Params.Add do begin
    Name := 'city'; ParamType := ptString; Required := True;
    Description.Add('Name of the city');
  end;
  Fn.OnExecute := @MyWeatherHandler;

  AiConn.AiFunctions := Funcs;
end.
```

---

## Build

This is a **source library** — include it directly in your projects via unit search paths:

```bash
fpc my_program.pas \
  -Fu/path/to/MakerAI/Source/Core    \
  -Fu/path/to/MakerAI/Source/Chat    \
  -Fu/path/to/MakerAI/Source/Design  \
  -Fu/path/to/MakerAI/Source/Tools   \
  -Fu/path/to/MakerAI/Source/Utils   \
  -Fu/path/to/MakerAI/Source/Agents  \
  -Fu/path/to/MakerAI/Source/RAG     \
  -Fu/path/to/MakerAI/Source/MCPClient \
  -Fu/path/to/MakerAI/Source/MCPServer \
  -Fi/path/to/MakerAI/Source/Core
```

> `-Fi` is required so FPC can find `uMakerAi.Version.inc`.

**Requirements:**
- Free Pascal 3.2.2+
- Standard FPC packages: `fcl-base`, `fcl-json`, `fphttpclient`, `fgl`, `generics.collections`
- OpenSSL on the system for HTTPS connections (`opensslsockets`)

### Running demos

```bash
cd Demos

# Chat with Ollama (requires Ollama running locally)
fpc -Fu../Source/Core -Fu../Source/Chat -Fu../Source/Design \
    demo_ollama.pas && ./demo_ollama

# Universal connector (switches driver at runtime)
fpc -Fu... demo_aiconnection.pas && ./demo_aiconnection

# Embeddings + similarity (requires OPENAI_API_KEY)
fpc -Fu../Source/Core -Fu../Source/Utils \
    demo_embeddings.pas && ./demo_embeddings

# RAG Vector (requires OPENAI_API_KEY)
fpc -Fu../Source/Core -Fu../Source/RAG -Fu../Source/Utils \
    demo_rag_vector.pas && ./demo_rag_vector

# RAG Graph (requires OPENAI_API_KEY)
fpc -Fu../Source/Core -Fu../Source/RAG -Fu../Source/Utils \
    demo_rag_graph.pas && ./demo_rag_graph

# MCP HTTP Server (port 8088)
fpc -Fu../Source/Core -Fu../Source/Chat -Fu../Source/MCPServer \
    demo_mcp_server.pas && ./demo_mcp_server
```

---

## Demo programs

### Chat — LLM drivers
| Demo | Driver | Mode |
|------|--------|------|
| `demo_ollama` / `demo_ollama_async` | Ollama (OpenAI-compat) | Sync / Async |
| `demo_ollama_native` / `demo_ollama_native_async` | Ollama (native API) | Sync / Async |
| `demo_generic` / `demo_generic_async` | GenericLLM (OpenAI-compat) | Sync / Async |
| `demo_lmstudio` / `demo_lmstudio_async` | LM Studio local | Sync / Async |
| `demo_groq` / `demo_groq_async` | Groq | Sync / Async |
| `demo_kimi` / `demo_kimi_async` | Kimi Moonshot | Sync / Async |
| `demo_deepseek` / `demo_deepseek_async` | DeepSeek | Sync / Async |
| `demo_openai` / `demo_openai_async` | OpenAI Responses API | Sync / Async |
| `demo_claude` / `demo_claude_async` | Anthropic Claude | Sync / Async |
| `demo_gemini` / `demo_gemini_async` | Google Gemini | Sync / Async |
| `demo_mistral` / `demo_mistral_async` | Mistral AI | Sync / Async |
| `demo_grok` / `demo_grok_async` | xAI Grok | Sync / Async |
| `demo_cohere` / `demo_cohere_async` | Cohere v2 | Sync / Async |
| `demo_aiconnection` / `demo_aiconnection_async` | Universal (multi-driver) | Sync / Async |

### Tools
| Demo | Module |
|------|--------|
| `demo_functions` | TAiFunctions — function calling, parameters, execution |
| `demo_shell` | TAiShell — interactive session, timeout, audit |
| `demo_text_editor` | TAiTextEditorTool — view/create/edit/undo |

### Multimedia
| Demo | Module |
|------|--------|
| `demo_whisper` | TAIWhisper — full TTS+STT round-trip (MP3) |
| `demo_openai_audio` | TAiOpenAiAudio — typed TTS enums, SpeechStreamed, verbose_json STT, translation |
| `demo_gemini_speech` | TAiGeminiSpeechTool — multi-speaker TTS, AudioProfile, TTS→STT |
| `demo_dalle` | TAiDalle — DALL-E 3 (URL + HD), DALL-E 2 (b64_json) |

### Embeddings & RAG
| Demo | Module |
|------|--------|
| `demo_embeddings` | TAiEmbeddings — generate vectors, CosineSimilarity, FindNearest, FindTopK |
| `demo_rag_vector` | TAiRAGVector — AddItem, BuildIndex+BM25, SearchText, filters, RRF |
| `demo_rag_graph` | TAiRagGraph — AI knowledge graph (13 nodes, 14 edges), GQL, Match, GetShortestPath |

### Agents
| Demo | Module |
|------|--------|
| `demo_agents` | TAIAgentManager — agent graph, Blackboard, conditional links |

### MCP
| Demo | Description |
|------|-------------|
| `demo_mcp_direct` | Direct in-process — 2 tools (echo + time) |
| `demo_mcp_server` | HTTP server on port 8088 |
| `demo_mcp_client` | HTTP client — tool invocation, error handling |
| `demo_mcp_server_stdio` | StdIO server |
| `demo_mcp_client_stdio` | StdIO client — launches external process |

---

## Architecture

```
Application
    │
    ├─ TAiChatConnection ──────────── Universal connector (switch via DriverName)
    │       │
    │       └─ LLM Drivers ────────── TAiOpenChat, TAiClaudeChat, TAiGeminiChat, ...
    │               │                 (inherit from TAiChat)
    │               └─ TAiChat ─────── HTTP async/sync, SSE streaming, tool calls,
    │                                  capability orchestration (ModelCaps/SessionCaps)
    │
    ├─ Tool System ─────────────────── TAiFunctions, TAiShell, TAiTextEditorTool,
    │                                  TAiComputerUseTool, TAIWhisper, TAiDalle, ...
    │
    ├─ Embeddings ──────────────────── TAiEmbeddingsCore → TAiEmbeddings (OpenAI)
    │
    ├─ RAG ─────────────────────────── TAiRAGVector (HNSW + BM25 + RRF + VQL)
    │                                  TAiRagGraph (Knowledge Graph + GQL)
    │
    ├─ Agents ──────────────────────── TAIAgentManager → TAIAgentsNode → TAIAgentsLink
    │                                  TAIBlackboard (thread-safe shared state)
    │                                  TAiFileCheckpointer (durable execution)
    │
    └─ MCP ─────────────────────────── TAiMCPLogicServer (JSON-RPC engine)
                                       ├─ TAiMCPHttpServer (HTTP)
                                       ├─ TAiMCPSSEHttpServer (SSE, experimental)
                                       ├─ TAiMCPStdioServer (stdin/stdout)
                                       └─ TAiMCPDirectConnection (in-process)
```

---

## API Keys

Resolved from environment variables at runtime using the `@` prefix:

```pascal
Conn.ApiKey := '@OPENAI_API_KEY';    // reads the OPENAI_API_KEY env var
Conn.ApiKey := '@CLAUDE_API_KEY';
Conn.ApiKey := '@GEMINI_API_KEY';
```

Standard variable names: `OPENAI_API_KEY`, `CLAUDE_API_KEY`, `GEMINI_API_KEY`, `GROQ_API_KEY`,
`DEEPSEEK_API_KEY`, `KIMI_API_KEY`, `GROK_API_KEY`, `MISTRAL_API_KEY`, `COHERE_API_KEY`.

---

## FPC vs Delphi differences

| Delphi | FPC |
|--------|-----|
| `System.JSON` | `fpjson` + `jsonparser` |
| `System.Net.HttpClient` | `fphttpclient` + `opensslsockets` |
| `TThread.Queue(nil, proc begin...end)` | `TAiQueueHelper` (method pointer) |
| `TRttiContext` / `TValue` | `TypInfo.GetPropInfo` + `SetOrdProp/SetFloatProp/SetStrProp` |
| `Boolean` = `tkEnumeration` | `Boolean` = `tkBool` (separate FPC type) |
| `TJSONObject.GetValue<T>('a.b')` | Manual `.Find()` navigation + cast |
| `JObj.AddPair('k', v)` | `JObj.Add('k', v)` |
| `System.NetEncoding` Base64 | Shim `Source/Core/EncdDecd.pas` |
| `ITask` / `TTask.Run` | `TAiNodeTask + TAiOrchestratorThread` (TThread) |
| `TDictionary<K,V>` | `specialize TDictionary<K,V>` (generics.collections) |
| `specialize TObjectList<T>` | Requires explicit `specialize` keyword |
| `for var I := ...` (inline var) | Declare in `var` section |
| `String.IsEmpty`, `.Trim()` | `Length(s) = 0`, `Trim(s)` |
| `TStringBuilder` | String concatenation with `+` |

---

*FPC Port completed: March 2026 — MakerAI Suite v3.3.0.0*
