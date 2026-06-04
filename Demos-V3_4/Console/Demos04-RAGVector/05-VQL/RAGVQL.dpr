program RAGVQL;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 04-RAGVector / 05-VQL
// =============================================================================
// VQL (Vector Query Language) — consultas avanzadas sobre el vector store
// usando una sintaxis tipo SQL adaptada a búsqueda vectorial.
//
// Conceptos que cubre:
//   - TAiRAGVector.ExecuteVQL() para consultas VQL
//   - SELECT, SEARCH NEAR, WHERE, ORDER BY, LIMIT
//   - Filtrado por metadatos con operadores
//   - Combinación de búsqueda semántica + filtros estructurados
//
// Sintaxis VQL básica:
//   SEARCH "<query>" NEAR <campo>
//   WHERE metadata.<campo> = <valor>
//   ORDER BY score DESC
//   LIMIT N
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.MetaData;

const
  EMB_DRIVER  = 'OpenAI';
  EMB_MODEL   = 'text-embedding-3-small';
  EMB_API_KEY = '@OPENAI_API_KEY';

// Corpus con metadatos
type
  TDocMeta = record
    Text    : String;
    Categoria: String;
    Año     : Integer;
    Autor   : String;
  end;

const
  CORPUS: array[0..7] of TDocMeta = (
    (Text: 'Machine learning es un subcampo de la inteligencia artificial.'; Categoria: 'IA'; Año: 2023; Autor: 'Lopez'),
    (Text: 'Redes neuronales profundas aprenden representaciones jerarquicas.'; Categoria: 'IA'; Año: 2023; Autor: 'Garcia'),
    (Text: 'Python es el lenguaje mas popular para ciencia de datos.'; Categoria: 'Programacion'; Año: 2022; Autor: 'Martinez'),
    (Text: 'Delphi permite crear aplicaciones nativas multiplataforma.'; Categoria: 'Programacion'; Año: 2024; Autor: 'Lopez'),
    (Text: 'Los transformers revolucionaron el procesamiento de lenguaje natural.'; Categoria: 'IA'; Año: 2024; Autor: 'Hernandez'),
    (Text: 'Rust garantiza seguridad de memoria sin recolector de basura.'; Categoria: 'Programacion'; Año: 2023; Autor: 'Garcia'),
    (Text: 'ChatGPT popularizo el uso de LLMs en aplicaciones de consumo.'; Categoria: 'IA'; Año: 2022; Autor: 'Martinez'),
    (Text: 'TypeScript añade tipos estaticos al ecosistema JavaScript.'; Categoria: 'Programacion'; Año: 2022; Autor: 'Hernandez')
  );

// =============================================================================
//  Indexa el corpus con metadatos
// =============================================================================
procedure IndexarCorpus(RAG: TAiRAGVector);
var
  I   : Integer;
  Meta: TAiEmbeddingMetaData;
begin
  Writeln('Indexando corpus con metadatos...');
  for I := 0 to High(CORPUS) do
  begin
    Meta := TAiEmbeddingMetaData.Create;
    try
      Meta['categoria'] := CORPUS[I].Categoria;
      Meta['año']       := CORPUS[I].Año;
      Meta['autor']     := CORPUS[I].Autor;
      RAG.AddItem(CORPUS[I].Text, Meta);
    finally
      Meta.Free;
    end;
    Write('.');
  end;
  Writeln;
  RAG.BuildIndex;
  RAG.BuildLexicalIndex;
  Writeln(Format('%d documentos indexados.', [RAG.Count]));
  Writeln;
end;

// =============================================================================
//  Ejecuta una consulta VQL y muestra el resultado
// =============================================================================
procedure EjecutarVQL(RAG: TAiRAGVector; const Descripcion, Query: String);
var
  Resultado: String;
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Query: ', Descripcion);
  Writeln('VQL:');
  Writeln('  ', Query);
  Writeln;
  try
    Resultado := RAG.ExecuteVQL(Query);
    Writeln('Resultado:');
    Writeln(Resultado);
  except
    on E: Exception do
      Writeln('Error en VQL: ', E.Message);
  end;
  Writeln;
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn: TAiEmbeddingConnection;
  RAG    : TAiRAGVector;
begin
  Writeln('=== RAGVQL ===');
  Writeln('Driver: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln;

  EmbConn := TAiEmbeddingConnection.Create(nil);
  RAG     := TAiRAGVector.Create(nil);
  try
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    RAG.Embeddings        := EmbConn.AiEmbeddings;
    RAG.NameVec           := 'TechPapers';
    RAG.InMemoryIndexType := TAIHNSWIndex;

    IndexarCorpus(RAG);

    // Consultas VQL progresivas
    EjecutarVQL(RAG,
      'Busqueda semantica simple',
      'SEARCH "inteligencia artificial y redes neuronales" LIMIT 3');

    EjecutarVQL(RAG,
      'Busqueda filtrada por categoria',
      'SEARCH "aprendizaje automatico" WHERE metadata.categoria = "IA" LIMIT 3');

    EjecutarVQL(RAG,
      'Busqueda filtrada por año',
      'SEARCH "lenguajes de programacion" WHERE metadata.año >= 2023 LIMIT 3');

    EjecutarVQL(RAG,
      'Busqueda con multiples filtros',
      'SEARCH "machine learning" WHERE metadata.categoria = "IA" AND metadata.año = 2024 LIMIT 3');

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
