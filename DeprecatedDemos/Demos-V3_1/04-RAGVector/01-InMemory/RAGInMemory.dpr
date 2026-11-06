program RAGInMemory;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 04-RAGVector / 01-InMemory
// =============================================================================
// RAG (Retrieval-Augmented Generation) con vector store en memoria.
// Crea un índice de documentos, busca los más relevantes y usa el LLM
// para responder preguntas con ese contexto.
//
// Conceptos que cubre:
//   - TAiRAGVector como almacén de vectores en memoria
//   - AddItem() para indexar documentos de texto
//   - BuildIndex() / BuildLexicalIndex() para optimizar búsqueda
//   - SearchText() para recuperar fragmentos relevantes
//   - Patrón completo: pregunta → recuperar contexto → generar respuesta
//
// Flujo RAG:
//   1. Indexar documentos (una sola vez)
//   2. Para cada pregunta: buscar top-K fragmentos relevantes
//   3. Construir prompt con [contexto + pregunta]
//   4. LLM genera respuesta fundamentada en el contexto
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi;

const
  // Proveedor para embeddings
  EMB_DRIVER  = 'OpenAI';
  EMB_MODEL   = 'text-embedding-3-small';
  EMB_API_KEY = '@OPENAI_API_KEY';

  // Proveedor para generación de respuesta
  LLM_DRIVER  = 'Claude';
  LLM_MODEL   = 'claude-haiku-4-5-20251001';
  LLM_API_KEY = '@CLAUDE_API_KEY';

// =============================================================================
//  Base de conocimiento de ejemplo
// =============================================================================
const
  DOCUMENTOS: array[0..8] of String = (
    'MakerAI es una suite Delphi para integrar LLMs en aplicaciones FMX. ' +
    'Soporta Claude, OpenAI, Gemini, Groq, Ollama y otros proveedores.',

    'TAiChatConnection es el componente central de MakerAI. ' +
    'Permite chatear con cualquier LLM cambiando solo DriverName y Model.',

    'Los embeddings son representaciones vectoriales de texto. ' +
    'Textos semanticamente similares tienen vectores parecidos en el espacio vectorial.',

    'RAG (Retrieval-Augmented Generation) combina busqueda vectorial con LLMs. ' +
    'Permite responder preguntas usando informacion que no esta en el modelo.',

    'TAiRAGVector almacena embeddings en memoria RAM. ' +
    'Soporta busqueda hibrida: vectorial (semantica) + lexica (BM25).',

    'Para instalar MakerAI en Delphi debes abrir el grupo de proyectos ' +
    'MakerAI.groupproj y compilar los paquetes en orden: Core, RAG, UI, DSG.',

    'El proveedor Ollama permite usar LLMs locales sin internet. ' +
    'Modelos populares: llama3.2, gemma3, mistral, codellama.',

    'La similitud coseno mide el angulo entre dos vectores. ' +
    'Un valor cercano a 1.0 indica alta similitud semantica.',

    'MCP (Model Context Protocol) es un estandar de Anthropic ' +
    'para conectar LLMs con herramientas externas de forma estandarizada.'
  );

// =============================================================================
//  Pregunta al RAG y genera respuesta con LLM
// =============================================================================
procedure AskRAG(RAG: TAiRAGVector; Chat: TAiChatConnection; const Question: String);
var
  Contexto: String;
  Prompt  : String;
  Response: String;
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', Question);
  Writeln;

  // Recuperar fragmentos relevantes
  Contexto := RAG.SearchText(Question, 3, 0.4, nil, False, True);

  if Contexto = '' then
  begin
    Writeln('(Sin contexto recuperado)');
    Exit;
  end;

  Writeln('Contexto recuperado:');
  Writeln(Contexto);

  // Construir prompt RAG
  Prompt :=
    'Contexto de referencia:' + sLineBreak +
    Contexto + sLineBreak +
    'Pregunta del usuario: ' + Question + sLineBreak +
    'Responde basandote SOLO en el contexto proporcionado. ' +
    'Si la informacion no esta en el contexto, indicalo.';

  // Generar respuesta
  Chat.Messages.Clear;
  Response := Chat.AddMessageAndRun(Prompt, 'user', []);
  Writeln('Respuesta LLM:');
  Writeln(Response);
  Writeln;
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn: TAiEmbeddingConnection;
  RAG    : TAiRAGVector;
  Chat   : TAiChatConnection;
  I      : Integer;
begin
  Writeln('=== RAGInMemory ===');
  Writeln('Embeddings: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln('LLM       : ', LLM_DRIVER, ' / ', LLM_MODEL);
  Writeln;

  EmbConn := TAiEmbeddingConnection.Create(nil);
  RAG     := TAiRAGVector.Create(nil);
  Chat    := TAiChatConnection.Create(nil);
  try
    // ── Configurar embeddings ─────────────────────────────────────────────
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    // ── Configurar RAG ────────────────────────────────────────────────────
    RAG.Embeddings          := EmbConn.AiEmbeddings;
    RAG.NameVec             := 'KnowledgeBase';
    RAG.InMemoryIndexType   := TAIHNSWIndex;

    // ── Indexar documentos ────────────────────────────────────────────────
    Writeln('Indexando ', Length(DOCUMENTOS), ' documentos...');
    for I := 0 to High(DOCUMENTOS) do
    begin
      RAG.AddItem(DOCUMENTOS[I]);
      Write('.');
    end;
    Writeln;

    RAG.BuildIndex;
    RAG.BuildLexicalIndex;
    Writeln(Format('Indice construido: %d documentos', [RAG.Count]));
    Writeln;

    // ── Configurar LLM ────────────────────────────────────────────────────
    Chat.DriverName := LLM_DRIVER;
    Chat.Model      := LLM_MODEL;
    Chat.Params.Values['ApiKey']       := LLM_API_KEY;
    Chat.Params.Values['Asynchronous'] := 'False';
    Chat.Params.Values['Max_Tokens']   := '512';
    Chat.SystemPrompt.Text :=
      'Eres un asistente tecnico de MakerAI. ' +
      'Responde en espanol de forma concisa y precisa.';

    // ── Preguntas al RAG ──────────────────────────────────────────────────
    AskRAG(RAG, Chat, 'Que es MakerAI y para que sirve?');
    AskRAG(RAG, Chat, 'Como puedo usar un LLM local sin internet?');
    AskRAG(RAG, Chat, 'Que es RAG y como funciona?');

  finally
    Chat.Free;
    RAG.Free;
    EmbConn.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
