# MakerAI Suite — Free Pascal Port

Port completo de la librería [MakerAI Suite v3.3](https://github.com/gustavoeenriquez/MakerAi) para **Free Pascal (FPC) 3.2+**.

MakerAI Suite es una librería de orquestación de IA que permite integrar múltiples proveedores LLM (OpenAI, Claude, Gemini, Ollama, etc.) en aplicaciones Pascal con una API unificada. Este repositorio es el port a FPC/Lazarus de la versión original escrita en Delphi.

- **Librería original Delphi:** https://github.com/gustavoeenriquez/MakerAi
- **Sitio oficial:** https://makerai.cimamaker.com
- **Autor:** Gustavo Enriquez — gustavoeenriquez@gmail.com
- **Licencia:** MIT

---

## Características implementadas

### Capa Core
| Unit | Descripción |
|------|-------------|
| `uMakerAi.Core` | `TAiMediaFile`, `TAiCapability`, `TAiChatState`, `IAiHttpResponse`, tipos MIME, Base64 |
| `uMakerAi.Chat.Messages` | `TAiChatMessage`, `TAiChatMessages`, `TAiToolsFunction`, citations, tokens |
| `uMakerAi.Chat.Tools` | Interfaces de herramientas (`IAiToolContext`, `TAiSpeechToolBase`, etc.) |
| `uMakerAi.Chat` | `TAiChat` — clase base de todos los drivers LLM, HTTP async/sync, SSE streaming, orquestación de capacidades |
| `UMakerAi.ParamsRegistry` | `TAiChatFactory` (singleton), `TAiEmbeddingFactory`, registro de drivers y parámetros |
| `uMakerAi.Utils.CodeExtractor` | Extrae bloques de código de respuestas Markdown |
| `Source/Core/EncdDecd` | Shim de compatibilidad Base64 para FPC |

### Drivers LLM — Chat
| Driver | Clase | Estado | Características |
|--------|-------|--------|-----------------|
| **GenericLLM** | `TAiGenericChat` | ✅ Probado | OpenAI-compatible genérico para cualquier endpoint |
| **Ollama** | `TAiOllamaChat` | ✅ Probado sync + async | API nativa `/api/chat`, streaming, gestión de modelos locales |
| **Groq** | `TAiGroqChat` | ✅ Probado sync + async | reasoning_format, reasoning_effort |
| **LM Studio** | `TAiLMStudioChat` | ✅ Compilado | OpenAI-compat local, `127.0.0.1:1234` |
| **Kimi** | `TAiKimiChat` | ✅ Compilado | Moonshot AI, OpenAI-compat |
| **DeepSeek** | `TAiDeepSeekChat` | ✅ Compilado | reasoning_content para deepseek-reasoner |
| **OpenAI** | `TAiOpenChat` | ✅ Compilado | Responses API `/v1/responses`, SSE tipado, shell/patch/image |
| **Claude** | `TAiClaudeChat` | ✅ Compilado | Anthropic API, `x-api-key`, thinking, citations |
| **Gemini** | `TAiGeminiChat` | ✅ Compilado | Google `generateContent`, thinking budget, imagen/video |
| **Mistral** | `TAiMistralChat` | ✅ Compilado | OCR, Document QnA, Magistral reasoning, `content-array` |
| **Grok** | `TAiGrokChat` | ✅ Compilado | xAI, web search, image gen, reasoning |
| **Cohere** | `TCohereChat` | ✅ Compilado | v2 API, SSE bloques `event:+data:`, citations, rerank, tool calls |

### Conector Universal
| Unit | Descripción |
|------|-------------|
| `uMakerAi.Chat.AiConnection` | `TAiChatConnection` — router runtime entre todos los drivers. Cambia de proveedor asignando `DriverName` sin modificar el código consumidor. Inyección de parámetros via RTTI (`TypInfo`). |

### API Keys
Las API keys se resuelven desde variables de entorno en runtime usando el prefijo `@`:

```pascal
AiConnection.ApiKey := '@OPENAI_API_KEY';   // lee OPENAI_API_KEY del entorno
AiConnection.ApiKey := '@CLAUDE_API_KEY';
AiConnection.ApiKey := '@GEMINI_API_KEY';
// etc.
```

Variables estándar: `OPENAI_API_KEY`, `CLAUDE_API_KEY`, `GEMINI_API_KEY`, `GROQ_API_KEY`,
`DEEPSEEK_API_KEY`, `KIMI_API_KEY`, `GROK_API_KEY`, `MISTRAL_API_KEY`, `COHERE_API_KEY`.

---

## Uso rápido

### Sincrónico (cualquier driver)
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
    Conn.DriverName := 'Ollama';           // o 'OpenAi', 'Claude', 'Groq', etc.
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

### Streaming / Asincrónico
```pascal
type
  TEvents = class
    Done: Boolean;
    procedure OnData(const Sender: TObject; aMsg: TAiChatMessage;
        aResponse: TJSONObject; aRole, aText: string);
    procedure OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
        aResponse: TJSONObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string;
        E: Exception; const Response: IAiHttpResponse);
  end;

// En OnData → Write(aText)   (chunk a chunk)
// En OnDataEnd → tokens acumulados en TAiChatConnection(Sender)

var
  Conn: TAiChatConnection;
  Ev: TEvents;
begin
  Ev   := TEvents.Create;
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := 'Ollama';
    Conn.Params.Values['Model']        := 'gemma3:1b';
    Conn.Params.Values['Asynchronous'] := 'True';

    Conn.OnReceiveData    := @Ev.OnData;
    Conn.OnReceiveDataEnd := @Ev.OnDataEnd;
    Conn.OnError          := @Ev.OnError;

    Conn.AddMessageAndRun('Cuéntame un chiste', 'user');

    while not Ev.Done do Sleep(50);   // esperar respuesta
  finally
    Conn.Free;
    Ev.Free;
  end;
end.
```

### Cambiar de driver en runtime
```pascal
// El mismo Conn, otro driver — el codigo consumidor no cambia
Conn.NewChat;
Conn.DriverName := 'Claude';
Conn.Params.Values['Model'] := 'claude-opus-4-5';
Resp := Conn.AddMessageAndRun('¿Cómo te llamas?', 'user');
```

---

## Compilar

Esta es una **librería de fuentes** — no tiene sistema de build propio. Se incluye directamente en los proyectos consumidores via rutas de búsqueda:

```bash
fpc \
  -FuPath/MakerAI/Source/Core  \
  -FuPath/MakerAI/Source/Chat  \
  -FuPath/MakerAI/Source/Design \
  -FuPath/MakerAI/Source/Tools \
  -FuPath/MakerAI/Source/Utils \
  -FiPath/MakerAI/Source/Core  \
  mi_programa.pas
```

> La flag `-Fi` es necesaria para que FPC encuentre `uMakerAi.Version.inc`.

**Requisitos:**
- Free Pascal 3.2.2+
- Paquetes FPC: `fcl-base`, `fcl-json`, `rtl`, `fphttpclient` (incluidos en FPC estándar)
- OpenSSL en el sistema para conexiones HTTPS

### Ejecutar los demos

```bash
# Compilar y ejecutar un demo (requiere Ollama corriendo localmente)
fpc -Fu... demo_ollama.pas && ./demo_ollama

# Demo del conector universal (cambia de driver en runtime)
fpc -Fu... demo_aiconnection.pas && ./demo_aiconnection

# Demo streaming/async
fpc -Fu... demo_aiconnection_async.pas && ./demo_aiconnection_async
```

---

## Arquitectura

```
Aplicación
    ↓
TAiChatConnection          ← Conector universal (cambia driver en runtime)
    ↓
Drivers LLM                ← TAiOllamaChat, TAiOpenChat, TAiClaudeChat, ...
    ↓ (heredan de TAiChat)
TAiChatMessages            ← Historial, tool calls, citations, tokens
    ↓
Tool System                ← IAiToolContext, TAiSpeechToolBase, TAiImageToolBase, ...
    ↓
Core / HTTP                ← TAiChat (SSE streaming, threading, orquestación)
    ↓
RAG / Agents / MCP         ← Capas de alto nivel (pendiente de portar)
```

### Patrón de capacidades (v3.3)

`ModelCaps` = lo que el modelo soporta nativamente.
`SessionCaps` = lo que la sesión necesita.
El gap (`SessionCaps − ModelCaps`) activa automáticamente herramientas bridge
(p.ej. si el modelo no genera imágenes, `TAiChatConnection` puede delegar a DALL-E).

### Patrón Factory + Registry

```pascal
// Registro de drivers y parámetros por defecto
TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Max_Tokens', '4096');

// Creación dinámica por nombre
Chat := TAiChatFactory.Instance.CreateDriver('Ollama');
```

---

## Demos incluidos

| Demo | Driver | Modo |
|------|--------|------|
| `demo_ollama.pas` | Ollama (OpenAI-compat) | Sync |
| `demo_ollama_async.pas` | Ollama (OpenAI-compat) | Async/Streaming |
| `demo_ollama_native.pas` | Ollama (API nativa) | Sync |
| `demo_ollama_native_async.pas` | Ollama (API nativa) | Async/Streaming |
| `demo_groq.pas` | Groq | Sync |
| `demo_groq_async.pas` | Groq | Async/Streaming |
| `demo_openai.pas` | OpenAI Responses API | Sync |
| `demo_openai_async.pas` | OpenAI Responses API | Async/Streaming |
| `demo_claude.pas` | Claude Anthropic | Sync |
| `demo_claude_async.pas` | Claude Anthropic | Async/Streaming |
| `demo_gemini.pas` | Google Gemini | Sync |
| `demo_gemini_async.pas` | Google Gemini | Async/Streaming |
| `demo_mistral.pas` | Mistral AI | Sync |
| `demo_mistral_async.pas` | Mistral AI | Async/Streaming |
| `demo_grok.pas` | xAI Grok | Sync |
| `demo_grok_async.pas` | xAI Grok | Async/Streaming |
| `demo_cohere.pas` | Cohere v2 | Sync |
| `demo_cohere_async.pas` | Cohere v2 | Async/Streaming |
| `demo_deepseek.pas` | DeepSeek | Sync |
| `demo_deepseek_async.pas` | DeepSeek | Async/Streaming |
| `demo_kimi.pas` | Kimi Moonshot | Sync |
| `demo_kimi_async.pas` | Kimi Moonshot | Async/Streaming |
| `demo_lmstudio.pas` | LM Studio local | Sync |
| `demo_lmstudio_async.pas` | LM Studio local | Async/Streaming |
| `demo_aiconnection.pas` | Universal (Ollama→GenericLLM) | Sync |
| `demo_aiconnection_async.pas` | Universal (Ollama→GenericLLM) | Async/Streaming |

---

## Roadmap — Hacia la paridad con Delphi v3.3

### Fase 2 — Herramientas (Tools)

Las herramientas permiten que los LLM ejecuten acciones reales: llamar funciones Pascal,
ejecutar comandos shell, editar archivos, capturar la pantalla, etc.

| Unit a portar | Descripción | Complejidad |
|---------------|-------------|-------------|
| `uMakerAi.Tools.Functions` | **Function calling completo** — esquema JSON de funciones, parámetros tipados, invocación automática desde el LLM | Alta |
| `uMakerAi.Tools.Shell` | Herramienta Shell — ejecuta comandos del sistema, lee stdout/stderr | Media |
| `uMakerAi.Tools.TextEditor` | Text Editor Tool (Claude) — view/create/str_replace/insert/undo | Media |
| `uMakerAi.Tools.ComputerUse` | Computer Use (Claude) — captura de pantalla, click, teclado | Alta |
| `uMakerAi.OpenAI.Audio` | OpenAI Audio — TTS y transcripción (Whisper) | Media |
| `uMakerAi.Whisper` | Whisper local/API — transcripción de audio | Media |
| `uMakerAi.OpenAi.Dalle` | DALL-E — generación de imágenes | Baja |
| `uMakerAi.OpenAI.Sora` | Sora — generación de video | Media |
| `uMakerAi.Gemini.Speech` | Gemini TTS | Baja |
| `uMakerAi.Gemini.Video` | Gemini Veo — generación de video | Media |
| `uMakerAi.Gemini.WebSearch` | Gemini Search Tool | Baja |
| `uMakerAi.Ollama.Ocr` | Ollama OCR | Baja |

### Fase 3 — Embeddings

| Unit a portar | Descripción | Complejidad |
|---------------|-------------|-------------|
| `uMakerAi.Embeddings.core` | Clase base de embeddings, vectorización de texto | Media |
| `uMakerAi.Embeddings.Connection` | Conector universal de embeddings (igual patrón que AiConnection) | Media |

### Fase 4 — RAG (Retrieval-Augmented Generation)

Motor de búsqueda semántica y base de conocimiento para los agentes.

| Unit a portar | Descripción | Complejidad |
|---------------|-------------|-------------|
| `uMakerAi.RAG.MetaData` | Metadatos de documentos | Baja |
| `uMakerAi.RAG.Vectors` | **Vector RAG** — índice HNSW + BM25, fusión RRF, reranking, VQL (DSL tipo SQL) | Muy Alta |
| `uMakerAi.RAG.Vectors.Index` | Gestión del índice vectorial | Alta |
| `uMakerAi.RAG.Vectors.VQL` | VQL — lenguaje de consulta vectorial | Alta |
| `uMakerAi.RAG.Vector.Driver.Postgres` | Backend PostgreSQL + pgvector | Alta |
| `uMakerAi.RAG.Graph.Core` | **Graph RAG** — grafo de entidades y relaciones, GQL (tipo Cypher) | Muy Alta |
| `uMakerAi.RAG.Graph.Documents` | Documentos del grafo | Media |
| `uMakerAi.RAG.Graph.Builder` | Constructor de grafos desde texto | Alta |
| `uMakerAi.RAG.Graph.GQL` | GQL — lenguaje de consulta de grafos | Alta |
| `uMakerAi.RAG.Graph.Driver.Postgres` | Backend PostgreSQL del grafo | Alta |

### Fase 5 — Agentes

Orquestación multi-agente basada en grafos con soporte de human-in-the-loop.

| Unit a portar | Descripción | Complejidad |
|---------------|-------------|-------------|
| `uMakerAi.Agents` | **Core del framework de agentes** — `TAIAgentManager`, `TAIAgentsNode`, `TAIAgentsLink`, `TAIBlackboard` | Muy Alta |
| `uMakerAi.Agents.Attributes` | Atributos de configuración declarativa de agentes | Media |
| `uMakerAi.Agents.Checkpoint` | Checkpoints durables — `TAiFileCheckpointer` para pausar/reanudar agentes | Alta |
| `uMakerAi.Agents.EngineRegistry` | Registro de motores de agentes | Media |
| `uMakerAi.Agents.GraphBuilder` | Constructor visual de grafos de agentes | Alta |
| `uMakerAi.Agents.Tools.Approval` | `TAiWaitApprovalTool` — human-in-the-loop | Media |
| `uMakerAi.Agents.DmGenerator` | Generador de data modules de agentes | Alta |

### Fase 6 — MCP (Model Context Protocol)

| Unit a portar | Descripción | Complejidad |
|---------------|-------------|-------------|
| `uMakerAi.MCPServer.Core` | **Servidor MCP core** — exposición de herramientas como recursos MCP | Muy Alta |
| `uMakerAi.MCPServer.Bridge` | Bridge TAiFunctions → herramientas MCP | Alta |
| `UMakerAi.MCPServer.Stdio` | Transporte StdIO (recomendado para CLI) | Media |
| `UMakerAi.MCPServer.Http` | Transporte HTTP | Media |
| `UMakerAi.MCPServer.SSE` | Transporte SSE (experimental) | Alta |
| `UMakerAi.MCPServer.Direct` | Transporte directo en memoria | Baja |
| MCP Client | Consumo de servidores MCP externos | Alta |

### Fase 7 — Utilidades adicionales

| Unit a portar | Descripción |
|---------------|-------------|
| `uMakerAi.Prompts` | Templates de prompts reutilizables |
| `uMakerAi.Utils.System` | Utilidades del sistema |
| `uMakerAi.Utils.PcmToWav` | Conversión PCM → WAV para audio |
| `uJSONHelper` | Helpers JSON adicionales |
| `uMakerAi.Chat.Bridge` | Bridge para integración con UI |

---

## Estado general del port

```
████████████████████████████████████░░░░░░░░░░░░░░░░░░░░░░░░  ~38%
```

| Módulo | Estado | Notas |
|--------|--------|-------|
| Core (Chat base + Messages + Tools) | ✅ Completo | |
| Factory + Registry | ✅ Completo | |
| 12 Drivers LLM | ✅ Completo | Todos compilan; Ollama + Groq probados en producción |
| TAiChatConnection | ✅ Completo | Router runtime con RTTI FPC (TypInfo) |
| Tools (Functions, Shell, etc.) | 🔲 Stubs | Interfaces declaradas, lógica pendiente |
| Embeddings | 🔲 Parcial | Clase base stub, Connection pendiente |
| RAG Vector + Graph | 🔲 Pendiente | Fase 4 |
| Agentes | 🔲 Pendiente | Fase 5 |
| MCP Server/Client | 🔲 Pendiente | Fase 6 |
| Utilidades adicionales | 🔲 Pendiente | Fase 7 |

---

## Diferencias FPC vs Delphi

Las principales adaptaciones realizadas al portar de Delphi a FPC:

| Delphi | FPC |
|--------|-----|
| `System.JSON` | `fpjson` + `jsonparser` |
| `System.Net.HttpClient` | `fphttpclient` + `opensslsockets` |
| `FClient.Asynchronous := True` | `TAiHttpThread` — thread manual |
| `OnInternalReceiveData(len, count, abort)` | `OnSSELine(ALine: string)` via `TAiSSEStream` |
| `TThread.Queue(nil, proc begin...end)` | Llamada directa desde thread (mismo comportamiento) |
| `TRttiContext` / `TRttiType` / `TValue` | `TypInfo.GetPropInfo` + `SetOrdProp/SetFloatProp/SetStrProp` |
| `Boolean` = `tkEnumeration` | `Boolean` = `tkBool` (FPC tiene tipo separado) |
| `TJSonObject.GetValue<T>('key')` | `JGetStr/JGetInt/...` (helpers locales en `uMakerAi.Chat`) |
| `JObj.AddPair('k', v)` | `JObj.Add('k', v)` |
| `System.NetEncoding` (Base64) | Shim `Source/Core/EncdDecd.pas` |
| `TArray<string>` | `TAiStringArray = array of string` |
| `specialize TObjectList<T>` | Requiere `specialize` explícito |
| `for var I := ...` (inline var) | Declarar en sección `var` |
| `String.IsEmpty`, `.Trim()` | `Length(s) = 0`, `Trim(s)` |

---

## Contribuir

Este es un port en progreso. Si quieres contribuir:

1. Revisa el [proyecto original Delphi](https://github.com/gustavoeenriquez/MakerAi) para entender la API de referencia.
2. Cada unit en `E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker\Source\` es la implementación autoritativa.
3. La tabla de traducciones FPC↔Delphi de arriba aplica a todos los módulos.
4. Consulta los `CLAUDE.md` de cada subdirectorio del original para patrones específicos.

---

*FPC Port iniciado: Feb 2026 — MakerAI Suite v3.3.0.0*
