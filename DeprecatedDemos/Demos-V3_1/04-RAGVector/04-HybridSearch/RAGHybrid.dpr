program RAGHybrid;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 04-RAGVector / 04-HybridSearch
// =============================================================================
// Combina búsqueda vectorial (semántica) con búsqueda lexica (BM25).
// La búsqueda híbrida supera a cada método por separado en muchos casos.
//
// Conceptos que cubre:
//   - TAiSearchOptions: UseEmbeddings, UseBM25, UseRRF
//   - Reciprocal Rank Fusion (RRF): combina rankings vectorial + BM25
//   - Cuándo falla la búsqueda vectorial (términos técnicos exactos)
//   - Cuándo falla BM25 (sinónimos, paráfrasis)
//   - La combinación híbrida captura ambos casos
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index;

const
  EMB_DRIVER  = 'OpenAI';
  EMB_MODEL   = 'text-embedding-3-small';
  EMB_API_KEY = '@OPENAI_API_KEY';

// Corpus con términos técnicos y texto semántico mezclado
const
  CORPUS: array[0..9] of String = (
    'La funcion SHA256 genera un hash criptografico de 256 bits.',
    'El algoritmo de hashing es fundamental para la seguridad de contraseñas.',
    'HTTPS usa TLS para cifrar las comunicaciones web de forma segura.',
    'SQL injection es un ataque donde codigo malicioso se inyecta en consultas.',
    'Los ataques de ciberseguridad pueden comprometer datos sensibles.',
    'REST API permite comunicacion entre servicios usando HTTP y JSON.',
    'GraphQL es una alternativa a REST para consultar datos de forma flexible.',
    'Docker containeriza aplicaciones para facilitar el despliegue.',
    'Kubernetes orquesta contenedores Docker a escala empresarial.',
    'DevOps combina desarrollo y operaciones para entregas continuas.'
  );

// =============================================================================
//  Compara los 3 modos de búsqueda para la misma consulta
// =============================================================================
procedure CompararModos(RAG: TAiRAGVector; const Query: String);
var
  Opts   : TAiSearchOptions;
  Results: String;
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Consulta: "', Query, '"');
  Writeln;

  Opts := RAG.SearchOptions;

  // Modo 1: solo vectorial
  Opts.UseEmbeddings := True;
  Opts.UseBM25       := False;
  Opts.UseRRF        := False;
  Results := RAG.SearchText(Query, 3, 0.0, nil, False, True);
  Writeln('VECTORIAL (semantico):');
  Writeln(Results);

  // Modo 2: solo BM25 (lexical)
  Opts.UseEmbeddings := False;
  Opts.UseBM25       := True;
  Opts.UseRRF        := False;
  Results := RAG.SearchText(Query, 3, 0.0, nil, False, True);
  Writeln('BM25 (lexical):');
  Writeln(Results);

  // Modo 3: híbrido RRF
  Opts.UseEmbeddings := True;
  Opts.UseBM25       := True;
  Opts.UseRRF        := True;
  Results := RAG.SearchText(Query, 3, 0.0, nil, False, True);
  Writeln('HIBRIDO (RRF):');
  Writeln(Results);
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn: TAiEmbeddingConnection;
  RAG    : TAiRAGVector;
  I      : Integer;
begin
  Writeln('=== RAGHybrid ===');
  Writeln('Driver: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln;

  EmbConn := TAiEmbeddingConnection.Create(nil);
  RAG     := TAiRAGVector.Create(nil);
  try
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    RAG.Embeddings        := EmbConn.AiEmbeddings;
    RAG.NameVec           := 'TechDocs';
    RAG.InMemoryIndexType := TAIHNSWIndex;

    Writeln('Indexando corpus...');
    for I := 0 to High(CORPUS) do
      RAG.AddItem(CORPUS[I]);

    RAG.BuildIndex;
    RAG.BuildLexicalIndex;
    Writeln(Format('%d documentos indexados.', [RAG.Count]));
    Writeln;

    // Consulta 1: término técnico exacto → BM25 gana
    CompararModos(RAG, 'SHA256 hash');

    // Consulta 2: semántica → vectorial gana
    CompararModos(RAG, 'proteccion de comunicaciones en internet');

    // Consulta 3: mix → híbrido gana
    CompararModos(RAG, 'seguridad en bases de datos SQL');

  finally
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
