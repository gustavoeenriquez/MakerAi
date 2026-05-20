program RAGDriversDemo;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - Demo 062: TAiMkVecDriver y TAiRAGVectorSQLiteDriver
// =============================================================================
//
// Demuestra los dos nuevos drivers de persistencia para TAiRAGVector:
//
//   TAiMkVecDriver  (uMakerAi.RAG.Vector.Driver.BinFile)
//     - Archivo binario propio .mkvec, sin dependencias externas
//     - Busqueda vectorial por coseno en Delphi (scan lineal)
//     - Busqueda lexical BM25 en memoria (TAIBm25Index)
//     - Busqueda hibrida con fusion RRF o ponderada
//     - Compact() para defragmentar el archivo
//
//   TAiRAGVectorSQLiteDriver  (uMakerAi.RAG.Vector.Driver.SQLite)
//     - SQLite via FireDAC, archivo .db
//     - Busqueda vectorial en Delphi (brute-force cosine)
//     - Busqueda lexical BM25 via FTS5 nativo de SQLite
//     - Busqueda hibrida con fusion RRF o ponderada
//     - Sin dependencias externas mas alla de FireDAC (incluido en Delphi)
//
// Este demo usa embeddings sinteticos de 8 dimensiones para no requerir
// una API key de LLM. En produccion se usaria TAiOpenAiEmbeddings,
// TAiOllamaEmbeddings o cualquier otro proveedor de embeddings.
//
// Configura las constantes DIM, FILE_PATH y DB_PATH antes de compilar.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Phys.SQLite,        // Registra el driver SQLite en FireDAC
  FireDAC.Stan.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Phys,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Comp.Client,
  uMakerAi.Embeddings.Core,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vector.Driver.BinFile,
  uMakerAi.RAG.Vector.Driver.SQLite;

const
  // Dimension de los embeddings sinteticos (pequena para rapidez de demo)
  DIM       = 8;

  // Rutas de los archivos generados por el demo
  FILE_PATH = 'demo_rag.mkvec';
  DB_PATH   = 'demo_rag.db';

  // Nombre de tabla para el driver SQLite
  TABLE_NAME = 'documentos';

  // Entidades (colecciones) de ejemplo
  ENT_TECH   = 'tecnologia';
  ENT_ARTE   = 'arte';

// =============================================================================
//  Documentos de ejemplo con embeddings sinteticos
// =============================================================================
//
//  Estructura de los vectores (8 dims):
//    [0..1] = componente "tecnologia/IA"
//    [2..3] = componente "musica"
//    [4..5] = componente "gastronomia"
//    [6..7] = componente "arte/fotografia"
//
//  Los vectores estan normalizados para que la similitud coseno sea
//  significativa (rango -1..1 donde 1 = identico).

type
  TDocSample = record
    ID:      string;
    Entidad: string;
    Text:    string;
    Categ:   string;
    Emb:     array[0..DIM-1] of Double;
  end;

const
  DOCS: array[0..6] of TDocSample = (
    // --- Tecnologia / IA ---
    (ID: 'doc-01'; Entidad: ENT_TECH;
     Text: 'inteligencia artificial redes neuronales machine learning deep learning';
     Categ: 'IA';
     Emb: (0.90, 0.85, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)),

    (ID: 'doc-02'; Entidad: ENT_TECH;
     Text: 'python programacion lenguajes software desarrollo algoritmos';
     Categ: 'Programacion';
     Emb: (0.80, 0.75, 0.10, 0.05, 0.05, 0.10, 0.05, 0.05)),

    (ID: 'doc-03'; Entidad: ENT_TECH;
     Text: 'base de datos SQL NoSQL almacenamiento vectores embeddings';
     Categ: 'Datos';
     Emb: (0.70, 0.65, 0.10, 0.10, 0.05, 0.10, 0.10, 0.05)),

    // --- Arte ---
    (ID: 'doc-04'; Entidad: ENT_ARTE;
     Text: 'musica guitarra piano jazz rock melodia armonia ritmo';
     Categ: 'Musica';
     Emb: (0.05, 0.05, 0.90, 0.85, 0.05, 0.05, 0.05, 0.05)),

    (ID: 'doc-05'; Entidad: ENT_ARTE;
     Text: 'fotografia camara exposicion iluminacion composicion encuadre';
     Categ: 'Foto';
     Emb: (0.05, 0.05, 0.10, 0.05, 0.10, 0.05, 0.90, 0.85)),

    (ID: 'doc-06'; Entidad: ENT_ARTE;
     Text: 'pintura oleo acuarela lienzo color textura artista';
     Categ: 'Pintura';
     Emb: (0.05, 0.05, 0.05, 0.05, 0.10, 0.10, 0.85, 0.80)),

    // --- Gastronomia (en ENT_TECH para probar filtrado por entidad) ---
    (ID: 'doc-07'; Entidad: ENT_TECH;
     Text: 'cocina recetas gastronomia ingredientes sabores culinaria';
     Categ: 'Gastronomia';
     Emb: (0.05, 0.05, 0.05, 0.10, 0.90, 0.85, 0.05, 0.10))
  );

// =============================================================================
//  Helpers
// =============================================================================

function MakeEmbeddingData(const AEmb: array of Double): TAiEmbeddingData;
var I: Integer;
begin
  SetLength(Result, DIM);
  for I := 0 to DIM - 1 do
    Result[I] := AEmb[I];
end;

function MakeNode(const ADoc: TDocSample): TAiEmbeddingNode;
begin
  Result := TAiEmbeddingNode.Create(DIM);
  Result.Tag   := ADoc.ID;
  Result.Text  := ADoc.Text;
  Result.Model := 'sintetico-v1';
  Result.Data  := MakeEmbeddingData(ADoc.Emb);
  Result.MetaData['categoria'] := ADoc.Categ;
  Result.MetaData['entidad']   := ADoc.Entidad;
end;

procedure PrintResults(const ATitle: string; AResults: TAiRAGVector);
var I: Integer;
begin
  Writeln;
  Writeln('  >> ', ATitle, ' (', AResults.Items.Count, ' resultado(s)):');
  if AResults.Items.Count = 0 then
  begin
    Writeln('     (sin resultados)');
    Exit;
  end;
  for I := 0 to AResults.Items.Count - 1 do
  begin
    var N := AResults.Items[I];
    Writeln(Format('     [%d] ID=%-8s  score=%.4f  "%s"',
      [I+1, N.Tag, N.Idx, Copy(N.Text, 1, 55)]));
  end;
end;

procedure Sep;
begin
  Writeln(StringOfChar('-', 70));
end;

// =============================================================================
//  Cargar documentos en el driver
// =============================================================================

procedure CargarDocumentos(ADriver: TAiVectorStoreDriverBase);
var I: Integer;
    Node: TAiEmbeddingNode;
begin
  Writeln('  Cargando ', Length(DOCS), ' documentos...');
  for I := 0 to High(DOCS) do
  begin
    Node := MakeNode(DOCS[I]);
    try
      ADriver.Add(Node, DOCS[I].Entidad);
    finally
      Node.Free;
    end;
    Write('    + ', DOCS[I].ID, ' [', DOCS[I].Entidad, ']');
    Writeln(' "', Copy(DOCS[I].Text, 1, 40), '"');
  end;
end;

// =============================================================================
//  Consultas de prueba
// =============================================================================

procedure TestConsultas(ADriver: TAiVectorStoreDriverBase; const ATag: string);
var
  QueryNode: TAiEmbeddingNode;
  Options: TAiSearchOptions;
  Results: TAiRAGVector;
  Filter: TAiFilterCriteria;
begin
  Writeln;
  Writeln('  [', ATag, '] --- Busqueda vectorial (tema IA) ---');
  QueryNode := TAiEmbeddingNode.Create(DIM);
  try
    QueryNode.Tag  := 'query-ia';
    QueryNode.Text := 'aprendizaje automatico redes neuronales';
    // Vector similar a doc-01 y doc-02 (zona de IA)
    QueryNode.Data := MakeEmbeddingData([0.85, 0.80, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05]);

    Options := TAiSearchOptions.Create;
    try
      Options.UseEmbeddings := True;
      Options.UseBM25       := False;
      Results := ADriver.Search(QueryNode, ENT_TECH, 3, 0.0, nil, Options);
      try
        PrintResults('Vector search / entidad=' + ENT_TECH, Results);
      finally
        Results.Free;
      end;
    finally
      Options.Free;
    end;

  finally
    QueryNode.Free;
  end;

  // ---- BM25 lexical search ----
  Writeln;
  Writeln('  [', ATag, '] --- Busqueda lexical BM25 (python) ---');
  QueryNode := TAiEmbeddingNode.Create(DIM);
  try
    QueryNode.Tag  := 'query-bm25';
    QueryNode.Text := 'python lenguajes programacion';
    // Sin embedding relevante para BM25
    QueryNode.Data := MakeEmbeddingData([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]);

    Options := TAiSearchOptions.Create;
    try
      Options.UseEmbeddings := False;
      Options.UseBM25       := True;
      Results := ADriver.Search(QueryNode, ENT_TECH, 3, 0.0, nil, Options);
      try
        PrintResults('BM25 search / entidad=' + ENT_TECH, Results);
      finally
        Results.Free;
      end;
    finally
      Options.Free;
    end;

  finally
    QueryNode.Free;
  end;

  // ---- Busqueda hibrida ----
  Writeln;
  Writeln('  [', ATag, '] --- Busqueda hibrida RRF (musica) ---');
  QueryNode := TAiEmbeddingNode.Create(DIM);
  try
    QueryNode.Tag  := 'query-hibrida';
    QueryNode.Text := 'musica guitarra melodia';
    QueryNode.Data := MakeEmbeddingData([0.05, 0.05, 0.88, 0.82, 0.05, 0.05, 0.05, 0.05]);

    Options := TAiSearchOptions.Create;
    try
      Options.UseEmbeddings := True;
      Options.UseBM25       := True;
      Options.UseRRF        := True;
      Results := ADriver.Search(QueryNode, ENT_ARTE, 3, 0.0, nil, Options);
      try
        PrintResults('Hibrida RRF / entidad=' + ENT_ARTE, Results);
      finally
        Results.Free;
      end;
    finally
      Options.Free;
    end;

  finally
    QueryNode.Free;
  end;

  // ---- Filtrado por metadato ----
  Writeln;
  Writeln('  [', ATag, '] --- Busqueda con filtro metadata (categoria=IA) ---');
  QueryNode := TAiEmbeddingNode.Create(DIM);
  Filter    := TAiFilterCriteria.Create;
  try
    QueryNode.Tag  := 'query-filtro';
    QueryNode.Text := 'aprendizaje';
    QueryNode.Data := MakeEmbeddingData([0.85, 0.80, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05]);
    Filter.Add('categoria', foEqual, 'IA');

    Options := TAiSearchOptions.Create;
    try
      Options.UseEmbeddings := True;
      Options.UseBM25       := False;
      Results := ADriver.Search(QueryNode, ENT_TECH, 5, 0.0, Filter, Options);
      try
        PrintResults('Vector + filtro categoria=IA', Results);
      finally
        Results.Free;
      end;
    finally
      Options.Free;
    end;

  finally
    QueryNode.Free;
    Filter.Free;
  end;
end;

// =============================================================================
//  Test Delete
// =============================================================================

procedure TestDelete(ADriver: TAiVectorStoreDriverBase; const ATag: string);
var
  QueryNode: TAiEmbeddingNode;
  Options: TAiSearchOptions;
  Results: TAiRAGVector;
begin
  Writeln;
  Writeln('  [', ATag, '] --- Borrado de doc-02 ---');
  ADriver.Delete('doc-02', ENT_TECH);
  Writeln('    doc-02 borrado.');

  // Verificar que ya no aparece
  QueryNode := TAiEmbeddingNode.Create(DIM);
  Options   := TAiSearchOptions.Create;
  try
    QueryNode.Tag  := 'query-post-delete';
    QueryNode.Text := 'python lenguajes programacion';
    QueryNode.Data := MakeEmbeddingData([0.80, 0.75, 0.10, 0.05, 0.05, 0.10, 0.05, 0.05]);
    Options.UseEmbeddings := True;
    Options.UseBM25       := True;
    Options.UseRRF        := True;

    Results := ADriver.Search(QueryNode, ENT_TECH, 3, 0.0, nil, Options);
    try
      PrintResults('Busqueda tras borrar doc-02', Results);
    finally
      Results.Free;
    end;
  finally
    QueryNode.Free;
    Options.Free;
  end;
end;

// =============================================================================
//  DEMO 1: TAiMkVecDriver (BinFile)
// =============================================================================

procedure RunBinFileDemo;
var
  Driver: TAiMkVecDriver;
begin
  Writeln;
  Sep;
  Writeln('DEMO 1 — TAiMkVecDriver (.mkvec)');
  Sep;

  // Borrar archivo previo para empezar limpio
  if FileExists(FILE_PATH) then DeleteFile(FILE_PATH);

  Driver := TAiMkVecDriver.Create(nil);
  try
    Driver.FilePath := FILE_PATH;
    Driver.Dim      := DIM;
    Driver.Language := alSpanish;
    Driver.AutoOpen := True;   // se abre solo al primer Add/Search

    // --- Carga ---
    CargarDocumentos(Driver);
    Writeln(Format('  NodeCount total: %d', [Driver.NodeCount]));

    // --- Consultas ---
    TestConsultas(Driver, 'BinFile');

    // --- Delete ---
    TestDelete(Driver, 'BinFile');
    Writeln(Format('  NodeCount tras borrado: %d', [Driver.NodeCount]));

    // --- Compact ---
    Writeln;
    Writeln('  [BinFile] --- Compact() ---');
    Driver.Compact;
    Writeln(Format('  NodeCount tras Compact: %d', [Driver.NodeCount]));
    Writeln('  Archivo compactado: ', FILE_PATH);

    Writeln;
    Writeln('  [BinFile] --- EntidadList ---');
    for var Ent in Driver.EntidadList do
      Writeln(Format('    Entidad "%s": %d nodos', [Ent, Driver.NodeCount(Ent)]));

  finally
    Driver.Free;
  end;

  Writeln;
  Writeln('  DEMO 1 completado. Archivo: ', FILE_PATH);
end;

// =============================================================================
//  DEMO 2: TAiRAGVectorSQLiteDriver
// =============================================================================

procedure RunSQLiteDemo;
var
  Conn:   TFDConnection;
  Driver: TAiRAGVectorSQLiteDriver;
begin
  Writeln;
  Sep;
  Writeln('DEMO 2 — TAiRAGVectorSQLiteDriver (.db)');
  Sep;

  // Borrar base de datos previa
  if FileExists(DB_PATH) then DeleteFile(DB_PATH);

  Conn := TFDConnection.Create(nil);
  try
    Conn.DriverName := 'SQLite';
    Conn.Params.Values['Database'] := DB_PATH;
    Conn.LoginPrompt := False;
    Conn.Open;
    Writeln('  Conexion SQLite abierta: ', DB_PATH);

    Driver := TAiRAGVectorSQLiteDriver.Create(nil);
    try
      Driver.Connection := Conn;
      Driver.TableName  := TABLE_NAME;
      Driver.Language   := alSpanish;

      // Crear tablas: tabla principal + FTS5 + triggers
      Driver.CreateSchema(TABLE_NAME);
      Writeln('  Schema creado: ', TABLE_NAME, ' + ', TABLE_NAME, '_fts');

      // --- Carga ---
      CargarDocumentos(Driver);

      // --- Consultas ---
      TestConsultas(Driver, 'SQLite');

      // --- Delete ---
      TestDelete(Driver, 'SQLite');

    finally
      Driver.Free;
    end;

  finally
    Conn.Free;
  end;

  Writeln;
  Writeln('  DEMO 2 completado. Base de datos: ', DB_PATH);
end;

// =============================================================================
//  MAIN
// =============================================================================

begin
  Writeln('========================================');
  Writeln(' MakerAI - Demo 062: RAG Drivers');
  Writeln(' TAiMkVecDriver + SQLiteDriver');
  Writeln(' Embeddings sinteticos ', DIM, 'D (sin API)');
  Writeln('========================================');

  try
    RunBinFileDemo;
    RunSQLiteDemo;
  except
    on E: Exception do
    begin
      Writeln;
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
    end;
  end;

  Writeln;
  Sep;
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
