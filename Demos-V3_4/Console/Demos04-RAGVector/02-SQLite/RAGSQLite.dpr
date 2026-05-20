program RAGSQLite;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 04-RAGVector / 02-SQLite
// =============================================================================
// Vector store persistente usando SQLite como backend vía FireDAC.
// Los embeddings se guardan en disco y se pueden recargar entre sesiones.
//
// Conceptos que cubre:
//   - TAiRAGVectorSQLiteDriver como driver de persistencia (FireDAC + SQLite)
//   - TFDConnection: conexión FireDAC a SQLite
//   - Driver.CreateSchema(tableName): crea tablas de vectores + FTS5
//   - Driver.Connection / TableName / Language
//   - Diferencia entre primera ejecución (crear schema + indexar) y posteriores
//   - Búsqueda con TAiSearchOptions (vectorial, BM25, híbrida RRF)
//
// Requisitos:
//   - Paquete MakerAI.RAG.Drivers instalado
//   - Unidad: uMakerAi.RAG.Vector.Driver.SQLite
//   - FireDAC.Phys.SQLite (driver SQLite de FireDAC)
// =============================================================================

uses
  System.SysUtils,
  System.IOUtils,
  Data.DB,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Phys,
  FireDAC.Comp.Client,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vector.Driver.SQLite;

const
  EMB_DRIVER  = 'OpenAI';
  EMB_MODEL   = 'text-embedding-3-small';
  EMB_API_KEY = '@OPENAI_API_KEY';

  DB_PATH    = 'rag_demo.db';
  TABLE_NAME = 'DelphiDocs';

const
  DOCS: array[0..5] of String = (
    'Delphi es un lenguaje de programacion orientado a objetos basado en Pascal.',
    'FireDAC es el framework de acceso a datos de Embarcadero para Delphi.',
    'FMX (FireMonkey) permite crear interfaces graficas multiplataforma con Delphi.',
    'RTL (Runtime Library) contiene las clases y funciones basicas de Delphi.',
    'VCL (Visual Component Library) es el framework GUI original de Delphi para Windows.',
    'El IDE de Delphi incluye depurador integrado, editor de codigo y disenador visual.'
  );

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn : TAiEmbeddingConnection;
  RAG     : TAiRAGVector;
  Driver  : TAiRAGVectorSQLiteDriver;
  Conn    : TFDConnection;
  I       : Integer;
  IsNew   : Boolean;
  Results : String;
begin
  Writeln('=== RAGSQLite ===');
  Writeln('Driver: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln('Base de datos: ', DB_PATH);
  Writeln;

  IsNew := not TFile.Exists(DB_PATH);

  EmbConn := TAiEmbeddingConnection.Create(nil);
  Conn    := TFDConnection.Create(nil);
  Driver  := TAiRAGVectorSQLiteDriver.Create(nil);
  RAG     := TAiRAGVector.Create(nil);
  try
    // ── Configurar embeddings ─────────────────────────────────────────────
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    // ── Configurar FireDAC + SQLite ───────────────────────────────────────
    Conn.DriverName                := 'SQLite';
    Conn.Params.Values['Database'] := DB_PATH;
    Conn.LoginPrompt               := False;
    Conn.Open;
    Writeln('Conexion SQLite abierta.');

    // ── Configurar SQLite driver ──────────────────────────────────────────
    Driver.Connection := Conn;
    Driver.TableName  := TABLE_NAME;
    Driver.Language   := alSpanish;

    // ── Configurar RAG con driver SQLite ──────────────────────────────────
    RAG.Embeddings := EmbConn.AiEmbeddings;
    RAG.NameVec    := TABLE_NAME;
    RAG.Driver     := Driver;
    RAG.Entidad    := TABLE_NAME;

    if IsNew then
    begin
      // Primera ejecución: crear schema e indexar documentos
      Writeln('Base de datos nueva. Creando schema e indexando...');
      Driver.CreateSchema(TABLE_NAME);
      for I := 0 to High(DOCS) do
      begin
        RAG.AddItem(DOCS[I]);
        Writeln(Format('  [%d] indexado', [I + 1]));
      end;
      Writeln(Format('%d documentos guardados en SQLite.', [RAG.Count]));
    end
    else
    begin
      // Ejecuciones posteriores: datos ya están en SQLite
      Writeln(Format('Base de datos existente con %d documentos.', [RAG.Count]));
      Writeln('(No se necesita re-indexar.)');
    end;
    Writeln;

    // ── Búsqueda ──────────────────────────────────────────────────────────
    Writeln('--- Busqueda semantica ---');
    Results := RAG.SearchText('frameworks GUI para interfaces graficas', 3, 0.4);
    Writeln('Consulta: "frameworks GUI para interfaces graficas"');
    Writeln(Results);

    Results := RAG.SearchText('acceso a base de datos', 3, 0.4);
    Writeln('Consulta: "acceso a base de datos"');
    Writeln(Results);

    Writeln('(Para reiniciar el demo, elimina: ', DB_PATH, ')');

  finally
    RAG.Free;
    Driver.Free;
    Conn.Free;
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
