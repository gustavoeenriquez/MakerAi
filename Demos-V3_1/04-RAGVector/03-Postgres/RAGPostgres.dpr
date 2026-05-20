program RAGPostgres;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 04-RAGVector / 03-Postgres
// =============================================================================
// Vector store persistente con extensión pgvector de PostgreSQL.
// Ideal para producción con grandes volúmenes de documentos.
//
// Conceptos que cubre:
//   - TAiRAGVectorPostgresDriver como driver de vectores en PostgreSQL
//   - TFDConnection: conexión FireDAC a PostgreSQL
//   - Driver.CreateSchema(tableName, dim): crea tabla con columna vector
//   - Guardar y recuperar embeddings desde pgvector
//   - Búsqueda vectorial y búsqueda híbrida con pgvector
//
// Requisitos previos:
//   1. PostgreSQL instalado y ejecutándose
//   2. Extensión pgvector instalada: CREATE EXTENSION IF NOT EXISTS vector;
//   3. Ajustar las constantes de conexión abajo
//   4. Paquete MakerAI.RAG.Drivers instalado
// =============================================================================

uses
  System.SysUtils,
  Data.DB,
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
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
  uMakerAi.RAG.Vector.Driver.Postgres;

const
  EMB_DRIVER  = 'OpenAI';
  EMB_MODEL   = 'text-embedding-3-small';
  EMB_API_KEY = '@OPENAI_API_KEY';

  // ── Ajustar según tu instalación de PostgreSQL ───────────────────────────
  PG_SERVER   = 'localhost';
  PG_PORT     = '5432';
  PG_DATABASE = 'makerai_demo';
  PG_USER     = 'postgres';
  PG_PASSWORD = 'postgres';

  TABLE_NAME  = 'PGVectorDemo';
  EMB_DIM     = 1536; // dimensión de text-embedding-3-small

const
  DOCS: array[0..4] of String = (
    'PostgreSQL es un sistema gestor de base de datos relacional avanzado.',
    'pgvector anade soporte de vectores y busqueda por similitud a PostgreSQL.',
    'Los indices IVFFlat y HNSW aceleran la busqueda vectorial en grandes datasets.',
    'Vector embeddings permiten busqueda semantica sobre texto en PostgreSQL.',
    'La extension vector de PostgreSQL es compatible con distancia coseno y euclidiana.'
  );

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn : TAiEmbeddingConnection;
  RAG     : TAiRAGVector;
  Driver  : TAiRAGVectorPostgresDriver;
  Conn    : TFDConnection;
  I       : Integer;
  Results : String;
begin
  Writeln('=== RAGPostgres ===');
  Writeln('Driver: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln('PostgreSQL: ', PG_SERVER, ':', PG_PORT, '/', PG_DATABASE);
  Writeln;

  EmbConn := TAiEmbeddingConnection.Create(nil);
  Conn    := TFDConnection.Create(nil);
  Driver  := TAiRAGVectorPostgresDriver.Create(nil);
  RAG     := TAiRAGVector.Create(nil);
  try
    // ── Configurar embeddings ─────────────────────────────────────────────
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    // ── Configurar FireDAC + PostgreSQL ───────────────────────────────────
    Conn.DriverName                  := 'PG';
    Conn.Params.Values['Server']     := PG_SERVER;
    Conn.Params.Values['Port']       := PG_PORT;
    Conn.Params.Values['Database']   := PG_DATABASE;
    Conn.Params.Values['User_Name']  := PG_USER;
    Conn.Params.Values['Password']   := PG_PASSWORD;
    Conn.LoginPrompt                 := False;

    try
      Conn.Open;
    except
      on E: Exception do
      begin
        Writeln('ERROR: No se pudo conectar a PostgreSQL.');
        Writeln('  ', E.Message);
        Writeln('Verifica que PostgreSQL este ejecutandose y pgvector instalado:');
        Writeln('  CREATE EXTENSION IF NOT EXISTS vector;');
        Exit;
      end;
    end;
    Writeln('Conexion exitosa a PostgreSQL.');
    Writeln;

    // ── Configurar Postgres driver ────────────────────────────────────────
    Driver.Connection := Conn;
    Driver.TableName  := TABLE_NAME;
    Driver.Language   := alSpanish;

    // ── Configurar RAG con driver Postgres ────────────────────────────────
    RAG.Embeddings := EmbConn.AiEmbeddings;
    RAG.NameVec    := TABLE_NAME;
    RAG.Driver     := Driver;
    RAG.Entidad    := TABLE_NAME;

    // Crear schema con dimensión correcta para el modelo
    Writeln('Creando schema en PostgreSQL...');
    Driver.CreateSchema(TABLE_NAME, EMB_DIM);
    Writeln('Schema listo.');
    Writeln;

    // Indexar documentos
    Writeln('Indexando ', Length(DOCS), ' documentos...');
    for I := 0 to High(DOCS) do
    begin
      RAG.AddItem(DOCS[I]);
      Writeln(Format('  [%d] guardado en pgvector', [I + 1]));
    end;
    Writeln;

    // Búsqueda
    Writeln('--- Busqueda semantica ---');
    Results := RAG.SearchText('extensiones de bases de datos vectoriales', 3, 0.4);
    Writeln('Consulta: "extensiones de bases de datos vectoriales"');
    Writeln(Results);

    Results := RAG.SearchText('rendimiento en busquedas de vectores', 3, 0.4);
    Writeln('Consulta: "rendimiento en busquedas de vectores"');
    Writeln(Results);

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
