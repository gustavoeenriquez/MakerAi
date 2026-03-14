# MakerAI Suite — Port para Free Pascal

Port completo de la librería [MakerAI Suite v3.3](https://github.com/gustavoeenriquez/MakerAi) para **Free Pascal (FPC) 3.2+**.

MakerAI Suite es una librería de orquestación de IA que permite integrar múltiples proveedores LLM (OpenAI, Claude, Gemini, Ollama, etc.) en aplicaciones Pascal con una API unificada. Este repositorio es el port a FPC/Lazarus de la versión original escrita en Delphi.

- **Librería original Delphi:** https://github.com/gustavoeenriquez/MakerAi  (rama `master`)
- **Sitio oficial:** https://makerai.cimamaker.com
- **Autor:** Gustavo Enriquez — gustavoeenriquez@gmail.com
- **Licencia:** MIT
- **[Read in English](README.md)**

---

## Estado del port

```
████████████████████████████████████████████████████████████  100%
```

| Módulo | Estado |
|--------|--------|
| Core (Chat base, Messages, Tools, Factory) | ✅ Completo |
| 12 Drivers LLM (todos sync + async) | ✅ Completo |
| TAiChatConnection (conector universal) | ✅ Completo |
| Tools (Functions, Shell, TextEditor, ComputerUse) | ✅ Completo |
| Multimedia (Whisper, OpenAI Audio, Gemini Speech, DALL-E) | ✅ Completo |
| Embeddings (TAiEmbeddingsCore + TAiEmbeddings OpenAI) | ✅ Completo |
| RAG Vector (HNSW + BM25 + RRF + VQL) | ✅ Completo |
| RAG Graph (grafo de conocimiento + GQL) | ✅ Completo |
| Agentes (multi-agente + Blackboard + Checkpoint) | ✅ Completo |
| MCP Server (HTTP, SSE, StdIO, Direct, Bridge) | ✅ Completo |
| MCP Client (HTTP, SSE, StdIO) | ✅ Completo |
| Demos (40+ programas de demostración) | ✅ Completo |

---

## Características

### Drivers LLM

| Driver | Clase | API |
|--------|-------|-----|
| **OpenAI** | `TAiOpenChat` | Responses API `/v1/responses`, SSE tipado, shell/patch/image gen |
| **Claude** | `TAiClaudeChat` | Anthropic API, thinking, citations, context management |
| **Gemini** | `TAiGeminiChat` | Google `generateContent`, thinking budget, brace-counting SSE |
| **Groq** | `TAiGroqChat` | reasoning_format, reasoning_effort |
| **Mistral** | `TAiMistralChat` | OCR, Document QnA, Magistral reasoning, content-array |
| **Grok** | `TAiGrokChat` | xAI, web search (`/v1/responses`), image gen |
| **Cohere** | `TCohereChat` | v2 API, SSE bloques event/data, citations, rerank |
| **DeepSeek** | `TAiDeepSeekChat` | reasoning_content (deepseek-reasoner) |
| **Kimi** | `TAiKimiChat` | Moonshot AI (`moonshot-v1-8k`, `kimi-k2`) |
| **Ollama** | `TAiOllamaChat` | API nativa `/api/chat`, gestión de modelos, thinking |
| **LM Studio** | `TAiLMStudioChat` | OpenAI-compat local `127.0.0.1:1234` |
| **GenericLLM** | `TAiGenericChat` | Cualquier endpoint OpenAI-compatible (vLLM, etc.) |

### Conector Universal

`TAiChatConnection` — cambia de proveedor asignando `DriverName` sin modificar el código consumidor. Inyección de parámetros via `TypInfo` (FPC).

### Tools

| Unit | Clase | Descripción |
|------|-------|-------------|
| `uMakerAi.Tools.Functions` | `TAiFunctions` | Function calling completo — esquema JSON, parámetros tipados, ejecución automática desde LLM |
| `uMakerAi.Tools.Shell` | `TAiShell` | Shell interactivo con sesión persistente, timeout, audit log |
| `uMakerAi.Tools.TextEditor` | `TAiTextEditorTool` | view/create/str_replace/insert/undo con I/O virtualizado |
| `uMakerAi.Tools.ComputerUse` | `TAiComputerUseTool` | Adaptador universal Computer Use (Claude/Gemini/OpenAI) |

### Multimedia

| Unit | Clase | Descripción |
|------|-------|-------------|
| `uMakerAi.Whisper` | `TAIWhisper` | OpenAI TTS + STT/Whisper, conversión de formatos via ffmpeg |
| `uMakerAi.OpenAI.Audio` | `TAiOpenAiAudio` | TTS (tts-1/tts-1-hd/gpt-4o-mini-tts), streaming, transcripción verbose_json, traducción |
| `uMakerAi.Gemini.Speech` | `TAiGeminiSpeechTool` | Gemini TTS multi-hablante (PCM→WAV), transcripción multimodal |
| `uMakerAi.OpenAi.Dalle` | `TAiDalle` | DALL-E 2/3 — URL + b64_json, generación y descarga automática |

### Embeddings

| Unit | Clase | Descripción |
|------|-------|-------------|
| `uMakerAi.Embeddings.Core` | `TAiEmbeddingsCore` | Clase base vectorial — CosineSimilarity, DotProduct, EuclideanDistance, FindNearest, FindTopK, AverageEmbedding, operadores sobrecargados |
| `uMakerAi.Embeddings` | `TAiEmbeddings` | Driver OpenAI (`text-embedding-3-small/large`) |

### RAG Vector

`TAiRAGVector` — base de conocimiento vectorial en memoria con búsqueda híbrida:

- Índice **HNSW** (búsqueda por similitud coseno)
- Índice léxico **BM25** (búsqueda por palabras clave)
- Fusión híbrida **RRF** o ponderada
- Filtros de metadatos (`TAiFilterCriteria`)
- Lenguaje de consulta **VQL** (DSL tipo SQL)
- `AddItem(text, metadata)`, `SearchText(query, limit, precision, filter)`, `ExecuteVGQL(query)`

### RAG Graph

`TAiRagGraph` — grafo de conocimiento (Knowledge Graph) con búsqueda semántica + estructural:

- Nodos (`TAiRagGraphNode`) y aristas (`TAiRagGraphEdge`) con embeddings propios
- Búsqueda semántica por vector en el grafo con expansión por profundidad
- **GQL** (Cypher-like): `MATCH`, `WHERE`, `RETURN`, `GET SHORTEST PATH`, `GET DEGREES TOP N`, `SHOW LABELS/EDGES`
- Patrón match programático (`TGraphMatchQuery`)
- Algoritmos de grafo: `GetShortestPath`, `GetAllShortestPaths`, `GetNodesByDegree`, `DetectCommunities`, `GetClosenessCentrality`
- Exportación: `.mkai` (MakerAI), DOT, GraphML

### Agentes

Framework de orquestación multi-agente basado en grafos con human-in-the-loop:

| Clase | Descripción |
|-------|-------------|
| `TAIAgentManager` | Orquestador principal del grafo de agentes |
| `TAIAgentsNode` | Unidad de ejecución (conecta con un driver LLM) |
| `TAIAgentsLink` | Arista con modos: `lmFanout`, `lmConditional`, `lmManual` |
| `TAIBlackboard` | Estado compartido thread-safe entre nodos |
| `TAiFileCheckpointer` | Ejecución durable — pausa y reanuda agentes |
| `TAiWaitApprovalTool` | Human-in-the-loop — suspende y espera aprobación humana |

### MCP (Model Context Protocol)

**Servidor:** expone funciones Pascal como herramientas MCP consumibles por cualquier cliente (Claude Desktop, etc.):

| Transporte | Clase | Descripción |
|-----------|-------|-------------|
| HTTP | `TAiMCPHttpServer` | `TFPHTTPServer` en thread, CORS, autenticación `@ApiKey` |
| SSE | `TAiMCPSSEHttpServer` | SSE sobre ssockets (experimental) |
| StdIO | `TAiMCPStdioServer` | stdin/stdout — recomendado para CLI |
| Direct | `TAiMCPDirectConnection` | In-process sin red |
| Bridge | `TTAiFunctionToolProxy` | Adapta `TAiFunctions` → `IAiMCPTool` automáticamente |

**Cliente:** `TMCPClientCustom` — consume servidores MCP externos (StdIO, HTTP, SSE).

---

## Uso rápido

### Chat sincrónico (cualquier driver)

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

    Resp := Conn.AddMessageAndRun('Hola, ¿quién eres?', 'user');
    WriteLn(Resp);
    WriteLn('Tokens: ', Conn.Total_tokens);
  finally
    Conn.Free;
  end;
end.
```

### Chat streaming / asincrónico

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

    Conn.AddMessageAndRun('Cuéntame un chiste', 'user');
    while not Ev.Done do Sleep(50);
  finally
    Conn.Free; Ev.Free;
  end;
end.
```

### Embeddings + similitud coseno

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

### RAG Vector — base de conocimiento

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

### RAG Graph — grafo de conocimiento

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

    // Búsqueda semántica
    WriteLn(Graph.SearchText('language model for chat', 0, True, 3, 0.3));

    // GQL
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
  Logic.ServerName := 'MiServidor';
  Logic.RegisterTool(TMiHerramienta.Create);   // IAiMCPTool

  Server := TAiMCPHttpServer.Create(nil);
  Server.LogicServer := Logic;
  Server.Port        := 8088;
  Server.Active      := True;        // abre el server en thread
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
  Fn.Description.Add('Devuelve el clima actual para una ciudad.');
  with Fn.Params.Add do begin
    Name := 'city'; ParamType := ptString; Required := True;
    Description.Add('Nombre de la ciudad');
  end;
  Fn.OnExecute := @MiHandlerDelTiempo;

  AiConn.AiFunctions := Funcs;
end.
```

---

## Compilar

Esta es una **librería de fuentes** — se incluye directamente en los proyectos vía rutas de búsqueda:

```bash
fpc mi_programa.pas \
  -Fu/ruta/MakerAI/Source/Core    \
  -Fu/ruta/MakerAI/Source/Chat    \
  -Fu/ruta/MakerAI/Source/Design  \
  -Fu/ruta/MakerAI/Source/Tools   \
  -Fu/ruta/MakerAI/Source/Utils   \
  -Fu/ruta/MakerAI/Source/Agents  \
  -Fu/ruta/MakerAI/Source/RAG     \
  -Fu/ruta/MakerAI/Source/MCPClient \
  -Fu/ruta/MakerAI/Source/MCPServer \
  -Fi/ruta/MakerAI/Source/Core
```

> `-Fi` es necesario para que FPC encuentre `uMakerAi.Version.inc`.

**Requisitos:**
- Free Pascal 3.2.2+
- Paquetes incluidos en FPC estándar: `fcl-base`, `fcl-json`, `fphttpclient`, `fgl`, `generics.collections`
- OpenSSL en el sistema para conexiones HTTPS (`opensslsockets`)

### Ejecutar demos

```bash
cd Demos

# Chat con Ollama (requiere Ollama corriendo)
fpc -Fu../Source/Core -Fu../Source/Chat -Fu../Source/Design \
    demo_ollama.pas && ./demo_ollama

# Conector universal (cambia de driver en runtime)
fpc -Fu... demo_aiconnection.pas && ./demo_aiconnection

# Embeddings + similitud (requiere OPENAI_API_KEY)
fpc -Fu../Source/Core -Fu../Source/Utils \
    demo_embeddings.pas && ./demo_embeddings

# RAG Vector (requiere OPENAI_API_KEY)
fpc -Fu../Source/Core -Fu../Source/RAG -Fu../Source/Utils \
    demo_rag_vector.pas && ./demo_rag_vector

# RAG Graph (requiere OPENAI_API_KEY)
fpc -Fu../Source/Core -Fu../Source/RAG -Fu../Source/Utils \
    demo_rag_graph.pas && ./demo_rag_graph

# MCP Server HTTP (puerto 8088)
fpc -Fu../Source/Core -Fu../Source/Chat -Fu../Source/MCPServer \
    demo_mcp_server.pas && ./demo_mcp_server
```

---

## Demos incluidos

### Chat — drivers LLM
| Demo | Driver | Modo |
|------|--------|------|
| `demo_ollama` / `demo_ollama_async` | Ollama (OpenAI-compat) | Sync / Async |
| `demo_ollama_native` / `demo_ollama_native_async` | Ollama (API nativa) | Sync / Async |
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
| Demo | Módulo |
|------|--------|
| `demo_functions` | TAiFunctions — function calling, parámetros, ejecución |
| `demo_shell` | TAiShell — sesión interactiva, timeout, audit |
| `demo_text_editor` | TAiTextEditorTool — view/create/edit/undo |

### Multimedia
| Demo | Módulo |
|------|--------|
| `demo_whisper` | TAIWhisper — TTS+STT ciclo completo (MP3) |
| `demo_openai_audio` | TAiOpenAiAudio — TTS enums, SpeechStreamed, verbose_json STT, translate |
| `demo_gemini_speech` | TAiGeminiSpeechTool — multi-hablante TTS, AudioProfile, TTS→STT |
| `demo_dalle` | TAiDalle — DALL-E 3 (URL + HD), DALL-E 2 (b64_json) |

### Embeddings y RAG
| Demo | Módulo |
|------|--------|
| `demo_embeddings` | TAiEmbeddings — generar vectores, CosineSimilarity, FindNearest, FindTopK |
| `demo_rag_vector` | TAiRAGVector — AddItem, BuildIndex+BM25, SearchText, filtros, RRF |
| `demo_rag_graph` | TAiRagGraph — grafo AI (13 nodos, 14 aristas), GQL, Match, GetShortestPath |

### Agentes
| Demo | Módulo |
|------|--------|
| `demo_agents` | TAIAgentManager — grafo de agentes, Blackboard, links condicionales |

### MCP
| Demo | Descripción |
|------|-------------|
| `demo_mcp_direct` | Direct in-process — 2 herramientas (echo + time) |
| `demo_mcp_server` | HTTP server en puerto 8088 |
| `demo_mcp_client` | Cliente HTTP — invocación de herramientas, error handling |
| `demo_mcp_server_stdio` | StdIO server |
| `demo_mcp_client_stdio` | Cliente StdIO — lanza proceso externo |

---

## Arquitectura

```
Aplicación
    │
    ├─ TAiChatConnection ──────────── Conector universal (switch por DriverName)
    │       │
    │       └─ Drivers LLM ────────── TAiOpenChat, TAiClaudeChat, TAiGeminiChat, ...
    │               │                 (heredan de TAiChat)
    │               └─ TAiChat ─────── HTTP async/sync, SSE streaming, tool calls,
    │                                  orquestación de capacidades (ModelCaps/SessionCaps)
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
    │                                  TAIBlackboard (estado compartido thread-safe)
    │                                  TAiFileCheckpointer (ejecución durable)
    │
    └─ MCP ─────────────────────────── TAiMCPLogicServer (motor JSON-RPC)
                                       ├─ TAiMCPHttpServer (HTTP)
                                       ├─ TAiMCPSSEHttpServer (SSE, experimental)
                                       ├─ TAiMCPStdioServer (stdin/stdout)
                                       └─ TAiMCPDirectConnection (in-process)
```

---

## API Keys

Resueltas desde variables de entorno en runtime con el prefijo `@`:

```pascal
Conn.ApiKey := '@OPENAI_API_KEY';    // lee la variable OPENAI_API_KEY
Conn.ApiKey := '@CLAUDE_API_KEY';
Conn.ApiKey := '@GEMINI_API_KEY';
```

Variables estándar: `OPENAI_API_KEY`, `CLAUDE_API_KEY`, `GEMINI_API_KEY`, `GROQ_API_KEY`,
`DEEPSEEK_API_KEY`, `KIMI_API_KEY`, `GROK_API_KEY`, `MISTRAL_API_KEY`, `COHERE_API_KEY`.

---

## Diferencias FPC vs Delphi

| Delphi | FPC |
|--------|-----|
| `System.JSON` | `fpjson` + `jsonparser` |
| `System.Net.HttpClient` | `fphttpclient` + `opensslsockets` |
| `TThread.Queue(nil, proc begin...end)` | `TAiQueueHelper` (method pointer) |
| `TRttiContext` / `TValue` | `TypInfo.GetPropInfo` + `SetOrdProp/SetFloatProp/SetStrProp` |
| `Boolean` = `tkEnumeration` | `Boolean` = `tkBool` (FPC tipo separado) |
| `TJSONObject.GetValue<T>('a.b')` | Navegación manual con `.Find()` + cast |
| `JObj.AddPair('k', v)` | `JObj.Add('k', v)` |
| `System.NetEncoding` Base64 | Shim `Source/Core/EncdDecd.pas` |
| `ITask` / `TTask.Run` | `TAiNodeTask + TAiOrchestratorThread` (TThread) |
| `TDictionary<K,V>` | `specialize TDictionary<K,V>` (generics.collections) |
| `specialize TObjectList<T>` | Requiere `specialize` explícito |
| `for var I := ...` (inline var) | Declarar en sección `var` |
| `String.IsEmpty`, `.Trim()` | `Length(s) = 0`, `Trim(s)` |
| `TStringBuilder` | Concatenación de strings con `+` |

---

*Port FPC completado: marzo 2026 — MakerAI Suite v3.3.0.0*
