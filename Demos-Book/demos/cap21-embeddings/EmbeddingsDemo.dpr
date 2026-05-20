program EmbeddingsDemo;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI -- Capitulo 21: Embeddings: El lenguaje de los vectores
// =============================================================================
// Tres demos usando TAiEmbeddingConnection + TAiEmbeddingsCore con Ollama local.
//
// Modelos requeridos (Ollama, sin internet, sin API key):
//   snowflake-arctic-embed  -- 1024 dims, BERT  (modelo principal)
//   mxbai-embed-large       -- 1024 dims, BERT  (comparacion Demo 3)
//   embeddinggemma          --  768 dims, Gemma3 (comparacion Demo 3)
//
// Para instalar modelos: ollama pull snowflake-arctic-embed
//                        ollama pull mxbai-embed-large
//                        ollama pull embeddinggemma
// =============================================================================

uses
  System.SysUtils, System.Classes,
  uMakerAi.Embeddings,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.Ollama;  // auto-registra el driver en initialization

// ---------------------------------------------------------------------------
//  Constantes de driver y modelos
// ---------------------------------------------------------------------------
const
  OLLAMA_DRV    = 'Ollama';
  MDL_SNOWFLAKE = 'snowflake-arctic-embed';
  MDL_MXBAI     = 'mxbai-embed-large';
  MDL_GEMMA     = 'embeddinggemma';

// ---------------------------------------------------------------------------
//  Corpus Demo 1: 6 frases, rango tematico amplio
// ---------------------------------------------------------------------------
const
  D1_QUERY = 'Como se programa una aplicacion en Delphi';

  D1_CORPUS: array[0..5] of string = (
    'Delphi es un lenguaje de programacion orientado a objetos',
    'RAD Studio permite compilar aplicaciones para Windows y Android',
    'Los componentes FMX facilitan el diseno de interfaces multiplataforma',
    'La inteligencia artificial transforma la forma de desarrollar software',
    'El cafe es la bebida favorita de los programadores',
    'La temperatura media en la superficie de Marte es de menos 60 grados'
  );

// ---------------------------------------------------------------------------
//  Corpus Demo 2: 15 frases, busqueda semantica con FindTopK
// ---------------------------------------------------------------------------
const
  D2_QUERIES: array[0..2] of string = (
    'componentes visuales para interfaces de usuario',
    'modelos locales sin necesidad de internet ni API key',
    'busqueda y recuperacion de informacion semantica'
  );

  D2_CORPUS: array[0..14] of string = (
    'FireMonkey permite disennar interfaces multiplataforma en Delphi',  // 0
    'TChatList y TChatInput son componentes FMX de MakerAI',             // 1
    'Delphi compila codigo nativo para Windows, Android e iOS',          // 2
    'Ollama ejecuta modelos de lenguaje de forma local sin conexion',    // 3
    'gemma, llama y mistral son modelos open-source que corren en CPU',  // 4
    'Los embeddings convierten texto en vectores numericos',             // 5
    'La similitud coseno mide el angulo entre dos vectores',             // 6
    'RAG combina busqueda semantica con generacion de texto',            // 7
    'TAiChat es la clase base abstracta de todos los drivers en MakerAI', // 8
    'Los agentes de IA ejecutan herramientas y toman decisiones autonomas', // 9
    'La base de datos vectorial almacena embeddings para busqueda eficiente', // 10
    'MakerAI soporta OpenAI, Claude, Gemini, Ollama y mas providers',   // 11
    'El protocolo MCP conecta servidores de herramientas a modelos LLM', // 12
    'Python es ampliamente usado en ciencia de datos e inteligencia artificial', // 13
    'La cocina italiana es famosa por su pasta y sus pizzas artesanales' // 14
  );

// ---------------------------------------------------------------------------
//  Corpus Demo 3: 8 frases, comparacion de tres modelos
// ---------------------------------------------------------------------------
const
  D3_QUERY = 'procesamiento de lenguaje natural con redes neuronales';

  D3_CORPUS: array[0..7] of string = (
    'Los transformers son la arquitectura base de los LLMs modernos',
    'GPT, Claude y Gemini son modelos de lenguaje de gran escala',
    'El procesamiento de lenguaje natural analiza y comprende texto humano',
    'Las redes neuronales aprenden patrones a partir de grandes datasets',
    'Delphi compila codigo nativo altamente eficiente para multiples plataformas',
    'FireDAC es el framework de acceso a datos de RAD Studio',
    'El sistema solar tiene ocho planetas orbitando alrededor del Sol',
    'La receta del gazpacho lleva tomate, pepino, ajo y aceite de oliva'
  );

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

// Barra visual de score (0.0-1.0 -> 20 chars)
function ScoreBar(Score: Double): string;
var
  Filled: Integer;
begin
  Filled := Round(Score * 20);
  if Filled < 0  then Filled := 0;
  if Filled > 20 then Filled := 20;
  Result := '[' + StringOfChar('#', Filled) + StringOfChar('.', 20 - Filled) + ']';
end;

// Genera embeddings para un array de frases y devuelve TAiEmbeddingList
function BuildCorpus(Conn: TAiEmbeddingConnection;
  const Textos: array of string): TAiEmbeddingList;
var
  I: Integer;
begin
  SetLength(Result, Length(Textos));
  for I := 0 to High(Textos) do
    Result[I] := Conn.CreateEmbedding(Textos[I], '');
end;

// ---------------------------------------------------------------------------
//  Demo 1 -- Similitud coseno: frases cercanas y lejanas
// ---------------------------------------------------------------------------

procedure Demo1_Similitud;
var
  Conn:    TAiEmbeddingConnection;
  QVec:    TAiEmbeddingData;
  Corpus:  TAiEmbeddingList;
  Results: TAiSimilarityList;
  I:       Integer;
begin
  Separador('Demo 1 -- Similitud coseno: frases cercanas y lejanas');
  WriteLn('  Modelo : ', MDL_SNOWFLAKE, '  [1024 dims]');
  WriteLn('  Query  : "', D1_QUERY, '"');
  WriteLn('');

  Conn := TAiEmbeddingConnection.Create(nil);
  try
    Conn.DriverName := OLLAMA_DRV;
    Conn.Model      := MDL_SNOWFLAKE;

    Write('  Calculando embeddings...');
    Corpus := BuildCorpus(Conn, D1_CORPUS);
    QVec   := Conn.CreateEmbedding(D1_QUERY, '');
    WriteLn(Format(' OK  (%d dims x %d frases)', [Length(QVec), Length(D1_CORPUS)]));

    // FindTopK con K = total: devuelve todo el corpus ordenado por similitud
    Results := TAiEmbeddingsCore.FindTopK(QVec, Corpus, Length(D1_CORPUS));

    WriteLn('');
    WriteLn('  Ranking de similitud  (1.0 = identico, 0.0 = sin relacion):');
    WriteLn('  ' + StringOfChar('=', 60));
    for I := 0 to High(Results) do
      WriteLn(Format('  #%d  %.3f  %s  %s',
        [I + 1,
         Results[I].Score,
         ScoreBar(Results[I].Score),
         Copy(D1_CORPUS[Results[I].Index], 1, 42)]));
    WriteLn('  ' + StringOfChar('=', 60));
    WriteLn('');
    WriteLn('-> "Cafe" sube al #2 porque "programadores" es proximo a "programar".');
    WriteLn('   Los embeddings capturan afinidad lexical, no solo temas completos.');
    WriteLn('   Marte obtiene el score mas bajo: tema completamente ajeno al query.');

  finally
    Conn.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  Demo 2 -- Busqueda semantica con FindTopK
// ---------------------------------------------------------------------------

procedure Demo2_FindTopK;
var
  Conn:    TAiEmbeddingConnection;
  QVec:    TAiEmbeddingData;
  Corpus:  TAiEmbeddingList;
  Results: TAiSimilarityList;
  Q, K:    Integer;
begin
  Separador('Demo 2 -- Busqueda semantica con FindTopK');
  WriteLn('  Modelo : ', MDL_SNOWFLAKE, '  [1024 dims]');
  WriteLn('  Corpus : ', Length(D2_CORPUS), ' frases  |  Top-K = 3');
  WriteLn('');

  Conn := TAiEmbeddingConnection.Create(nil);
  try
    Conn.DriverName := OLLAMA_DRV;
    Conn.Model      := MDL_SNOWFLAKE;

    // Pre-calcular embeddings del corpus (una sola vez, reutilizar en las 3 queries)
    Write('  Indexando corpus...');
    Corpus := BuildCorpus(Conn, D2_CORPUS);
    WriteLn(Format(' OK  (%d embeddings listos)', [Length(Corpus)]));
    WriteLn('');

    for Q := 0 to High(D2_QUERIES) do
    begin
      WriteLn(Format('  [Q%d] "%s"', [Q + 1, D2_QUERIES[Q]]));
      QVec    := Conn.CreateEmbedding(D2_QUERIES[Q], '');
      Results := TAiEmbeddingsCore.FindTopK(QVec, Corpus, 3);

      for K := 0 to High(Results) do
        WriteLn(Format('       #%d  %.3f  %s',
          [K + 1,
           Results[K].Score,
           Copy(D2_CORPUS[Results[K].Index], 1, 55)]));
      WriteLn('');
    end;

    WriteLn('-> Patron RAG: el corpus se indexa UNA vez, se consulta N veces.');
    WriteLn('   En produccion los embeddings viven en una base de datos vectorial');
    WriteLn('   (cap. 22-23). FindTopK es el motor de busqueda semantica.');

  finally
    Conn.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  Demo 3 -- Los tres modelos cara a cara
// ---------------------------------------------------------------------------

procedure Demo3_Comparacion;
const
  MODELS: array[0..2] of string = (MDL_SNOWFLAKE, MDL_MXBAI, MDL_GEMMA);
  DIMS:   array[0..2] of string = ('1024d', '1024d', ' 768d');
var
  Conn:    TAiEmbeddingConnection;
  QVec:    TAiEmbeddingData;
  Corpus:  TAiEmbeddingList;
  Results: TAiSimilarityList;
  M, K:    Integer;
begin
  Separador('Demo 3 -- Los tres modelos cara a cara');
  WriteLn('  Query : "', D3_QUERY, '"');
  WriteLn('  Corpus: ', Length(D3_CORPUS), ' frases  |  Top-3 por modelo');
  WriteLn('');

  for M := 0 to High(MODELS) do
  begin
    WriteLn(Format('  %-30s  (%s)', [MODELS[M], DIMS[M]]));
    WriteLn('  ' + StringOfChar('-', 55));

    Conn := TAiEmbeddingConnection.Create(nil);
    try
      Conn.DriverName := OLLAMA_DRV;
      Conn.Model      := MODELS[M];

      Corpus  := BuildCorpus(Conn, D3_CORPUS);
      QVec    := Conn.CreateEmbedding(D3_QUERY, '');
      Results := TAiEmbeddingsCore.FindTopK(QVec, Corpus, 3);

      for K := 0 to High(Results) do
        WriteLn(Format('  #%d  %.3f  %s',
          [K + 1,
           Results[K].Score,
           Copy(D3_CORPUS[Results[K].Index], 1, 52)]));
      WriteLn('');
    finally
      Conn.Free;
    end;
  end;

  WriteLn('-> Los tres modelos suelen coincidir en el ORDEN semantico,');
  WriteLn('   pero sus scores absolutos difieren (BERT-1024 vs Gemma-768).');
  WriteLn('   Para comparar entre modelos siempre usar ranking relativo,');
  WriteLn('   nunca el valor de score como umbral absoluto.');
end;

// ---------------------------------------------------------------------------
//  Programa principal
// ---------------------------------------------------------------------------

begin
  WriteLn('=== MakerAI -- Capitulo 21: Embeddings: El lenguaje de los vectores ===');
  WriteLn('');
  WriteLn('  Modelos locales (Ollama, sin internet, sin API key):');
  WriteLn('    snowflake-arctic-embed   [1024 dims, BERT,   334M params]');
  WriteLn('    mxbai-embed-large        [1024 dims, BERT,   334M params]');
  WriteLn('    embeddinggemma           [ 768 dims, Gemma3, 308M params]');
  WriteLn('');
  WriteLn('  Nota: cada CreateEmbedding hace una llamada HTTP a localhost:11434.');
  WriteLn('  En produccion los embeddings del corpus se precalculan y persisten.');

  try
    Demo1_Similitud;
    Demo2_FindTopK;
    Demo3_Comparacion;

    WriteLn('');
    WriteLn(StringOfChar('=', 62));
    WriteLn('  Demos completados.');
    WriteLn('  Ver cap21-embeddings.md para la documentacion completa.');
    WriteLn(StringOfChar('=', 62));

  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('[ERROR] ', E.ClassName, ': ', E.Message);
    end;
  end;

  WriteLn('');
  Write('Presiona Enter para salir...');
  ReadLn;
end.
