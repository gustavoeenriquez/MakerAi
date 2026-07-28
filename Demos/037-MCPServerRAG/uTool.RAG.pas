unit uTool.RAG;

(*
  uTool.RAG  -  Demo 037-MCPServerRAG

  Herramienta MCP que expone un sistema RAG real respaldado por SQL Server:

    - TAiRAGVector             : orquestador; con Driver asignado delega la
                                 busqueda 100% al motor T-SQL
    - TAiRAGVectorMSSQLDriver  : SQL Server 2025+ (tipo VECTOR nativo,
                                 VECTOR_DISTANCE coseno + FREETEXTTABLE BM25)
    - TAiOpenAiEmbeddings      : generacion de embeddings client-side
    - Configuracion            : archivo .ini (ver TRagServerConfig / LoadServerConfig)
    - Autenticacion            : login/password QUEMADOS (RAG_LOGIN / RAG_PASSWORD);
                                 se valida via OnValidateRequest del servidor MCP.
                                 Formatos aceptados en el header Authorization:
                                   Basic  base64(usuario:password)   (estandar HTTP)
                                   Bearer usuario:password           (compat MCPClient MakerAI)
                                   X-API-Key: usuario:password

  Operaciones del tool 'rag_vector':
    index_text, index_file, search, list_docs, delete_doc, clear, stats

  La conexion a la base de datos es perezosa (EnsureDb): el servidor arranca
  aunque SQL Server no este disponible y reporta el error por operacion.
  Todas las operaciones se serializan con un TCriticalSection global
  (FireDAC TFDConnection no es thread-safe entre hilos del servidor).
*)

interface

uses
  System.JSON,
  uMakerAi.MCPServer.Core;

const
  // Credenciales de acceso al servidor MCP (quemadas segun especificacion)
  RAG_LOGIN = 'admin';
  RAG_PASSWORD = 'MakerAi2026*';

type

  // Configuracion leida del archivo .ini
  TRagServerConfig = record
    Protocol: string;
    Port: Integer;
    // [Database]
    DbServer: string;
    DbDatabase: string;
    DbUserName: string;
    DbPassword: string;
    OSAuthent: Boolean;
    TableName: string;
    Entidad: string;
    // [Embeddings]
    ApiKey: string;
    Model: string;
    Dimensions: Integer;
    // [Search]
    UseBM25: Boolean;
    EmbeddingWeight: Double;
    BM25Weight: Double;
    Language: string;
  end;

  // Handler del evento OnValidateRequest (requiere metodo de objeto)
  TRagAuth = class
  public
    procedure HandleValidateRequest(Sender: TObject; const AAuthHeader, ARemoteIP: string;
      out AAuthContext: TAiAuthContext; out AIsValid: Boolean);
  end;

  TRagParams = class
  private
    FOperation: string;
    FTextContent: string;
    FDocName: string;
    FFilePath: string;
    FQuery: string;
    FTopK: Integer;
    FMinScore: string;
    FChunkSize: Integer;
    FOverlapPct: Integer;
  public
    constructor Create;

    [AiMCPSchemaDescription('Operacion: index_text, index_file, search, list_docs, delete_doc, clear, stats')]
    property Operation: string read FOperation write FOperation;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Texto a indexar directamente (para index_text)')]
    property TextContent: string read FTextContent write FTextContent;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Nombre del documento (para index_text, delete_doc; opcional en index_file)')]
    property DocName: string read FDocName write FDocName;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Ruta de un archivo de texto a indexar (para index_file)')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Consulta de busqueda semantica (para search)')]
    property Query: string read FQuery write FQuery;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Numero maximo de resultados (para search, default: 5)')]
    property TopK: Integer read FTopK write FTopK;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Score minimo 0..1 como texto, ej. "0.3" (para search, default: "0")')]
    property MinScore: string read FMinScore write FMinScore;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Tamano del chunk en caracteres (para index_*, default: 800)')]
    property ChunkSize: Integer read FChunkSize write FChunkSize;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Porcentaje de solapamiento entre chunks 0-99 (para index_*, default: 15)')]
    property OverlapPct: Integer read FOverlapPct write FOverlapPct;
  end;

  TRagTool = class(TAiMCPToolBase<TRagParams>)
  private
    function DoIndexText(const P: TRagParams): TJSONObject;
    function DoIndexFile(const P: TRagParams): TJSONObject;
    function DoSearch(const P: TRagParams): TJSONObject;
    function DoListDocs: TJSONObject;
    function DoDeleteDoc(const P: TRagParams): TJSONObject;
    function DoClear: TJSONObject;
    function DoStats: TJSONObject;
    function IndexText(const AText, ADocName: string; AChunkSize, AOverlapPct: Integer): Integer;
  protected
    function ExecuteWithParams(const AParams: TRagParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// Lee el .ini de configuracion; si no existe lo crea con valores por defecto.
function LoadServerConfig(const AFileName: string): TRagServerConfig;

// Inicializa el motor RAG compartido (no conecta a la DB; la conexion es perezosa).
procedure InitRagEngine(const ACfg: TRagServerConfig);
procedure DoneRagEngine;

// Registra el tool y engancha la autenticacion (OnValidateRequest) en el servidor.
procedure RegisterTools(AServer: TAiMCPServer);

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Variants,
  System.SyncObjs, System.NetEncoding, System.IniFiles,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Stan.Param, FireDAC.Phys,
  FireDAC.Phys.Intf, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.ConsoleUI.Wait, FireDAC.DApt, Data.DB, FireDAC.Comp.Client,
  uMakerAi.RAG.Vectors, uMakerAi.RAG.Vectors.Index, uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vector.Driver.MSSQL,
  uMakerAi.Embeddings.OpenAi;

var
  GLock: TCriticalSection;
  GCfg: TRagServerConfig;
  GAuth: TRagAuth;
  GDriverLink: TFDPhysMSSQLDriverLink;
  GConn: TFDConnection;
  GDriver: TAiRAGVectorMSSQLDriver;
  GRag: TAiRAGVector;
  GEmb: TAiOpenAiEmbeddings;
  GSchemaReady: Boolean;

// ---------------------------------------------------------------------------
// Configuracion (.ini)
// ---------------------------------------------------------------------------

function ReadFloatInv(Ini: TCustomIniFile; const ASection, AKey: string; ADefault: Double): Double;
begin
  // ReadFloat de TIniFile depende del locale (coma decimal en Windows es-*);
  // leemos como texto y parseamos con punto decimal invariante.
  Result := StrToFloatDef(Ini.ReadString(ASection, AKey, ''), ADefault, TFormatSettings.Invariant);
end;

function LoadServerConfig(const AFileName: string): TRagServerConfig;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(AFileName, TEncoding.UTF8);
  try
    if not TFile.Exists(AFileName) then
    begin
      // Primera ejecucion: generar plantilla editable
      Ini.WriteString('Server', 'Protocol', 'sse');
      Ini.WriteInteger('Server', 'Port', 8080);
      Ini.WriteString('Database', 'Server', 'localhost');
      Ini.WriteString('Database', 'Database', 'MakerAiRag');
      Ini.WriteString('Database', 'UserName', 'sa');
      Ini.WriteString('Database', 'Password', '');
      Ini.WriteString('Database', 'OSAuthent', 'No');
      Ini.WriteString('Database', 'TableName', 'rag_docs');
      Ini.WriteString('Database', 'Entidad', 'default');
      Ini.WriteString('Embeddings', 'ApiKey', '@OPENAI_API_KEY');
      Ini.WriteString('Embeddings', 'Model', 'text-embedding-3-small');
      Ini.WriteInteger('Embeddings', 'Dimensions', 1536);
      Ini.WriteString('Search', 'UseBM25', 'True');
      Ini.WriteString('Search', 'EmbeddingWeight', '0.7');
      Ini.WriteString('Search', 'BM25Weight', '0.3');
      Ini.WriteString('Search', 'Language', 'es');
      Ini.UpdateFile;
      WriteLn(ErrOutput, '[rag] Archivo de configuracion creado con defaults: ' + AFileName);
    end;

    Result.Protocol := Ini.ReadString('Server', 'Protocol', 'sse');
    Result.Port := Ini.ReadInteger('Server', 'Port', 8080);

    Result.DbServer := Ini.ReadString('Database', 'Server', 'localhost');
    Result.DbDatabase := Ini.ReadString('Database', 'Database', 'MakerAiRag');
    Result.DbUserName := Ini.ReadString('Database', 'UserName', '');
    Result.DbPassword := Ini.ReadString('Database', 'Password', '');
    Result.OSAuthent := Ini.ReadBool('Database', 'OSAuthent', False);
    Result.TableName := Ini.ReadString('Database', 'TableName', 'rag_docs');
    Result.Entidad := Ini.ReadString('Database', 'Entidad', 'default');

    Result.ApiKey := Ini.ReadString('Embeddings', 'ApiKey', '@OPENAI_API_KEY');
    Result.Model := Ini.ReadString('Embeddings', 'Model', 'text-embedding-3-small');
    Result.Dimensions := Ini.ReadInteger('Embeddings', 'Dimensions', 1536);

    Result.UseBM25 := Ini.ReadBool('Search', 'UseBM25', True);
    Result.EmbeddingWeight := ReadFloatInv(Ini, 'Search', 'EmbeddingWeight', 0.7);
    Result.BM25Weight := ReadFloatInv(Ini, 'Search', 'BM25Weight', 0.3);
    Result.Language := Ini.ReadString('Search', 'Language', 'es');
  finally
    Ini.Free;
  end;
end;

function ParseLanguage(const ALang: string): TAiLanguage;
begin
  if SameText(ALang, 'en') then
    Result := alEnglish
  else if SameText(ALang, 'pt') then
    Result := alPortuguese
  else
    Result := alSpanish;
end;

// ---------------------------------------------------------------------------
// Autenticacion (login/password quemados)
// ---------------------------------------------------------------------------

procedure TRagAuth.HandleValidateRequest(Sender: TObject; const AAuthHeader, ARemoteIP: string;
  out AAuthContext: TAiAuthContext; out AIsValid: Boolean);
var
  Cred: string;
begin
  AAuthContext := Default(TAiAuthContext);
  AIsValid := False;

  if AAuthHeader.StartsWith('Basic ', True) then
  begin
    try
      Cred := TNetEncoding.Base64.Decode(AAuthHeader.Substring(6).Trim);
    except
      Cred := '';
    end;
  end
  else if AAuthHeader.StartsWith('Bearer ', True) then
    Cred := AAuthHeader.Substring(7).Trim
  else
    Cred := AAuthHeader.Trim; // valor crudo de X-API-Key

  if Cred = RAG_LOGIN + ':' + RAG_PASSWORD then
  begin
    AAuthContext.IsAuthenticated := True;
    AAuthContext.UserID := RAG_LOGIN;
    AAuthContext.UserName := RAG_LOGIN;
    AIsValid := True;
  end
  else
    WriteLn(ErrOutput, '[rag] Acceso denegado desde ' + ARemoteIP);
end;

// ---------------------------------------------------------------------------
// Motor compartido
// ---------------------------------------------------------------------------

procedure InitRagEngine(const ACfg: TRagServerConfig);
begin
  GLock := TCriticalSection.Create;
  GAuth := TRagAuth.Create;
  GCfg := ACfg;
  GSchemaReady := False;

  GDriverLink := TFDPhysMSSQLDriverLink.Create(nil);

  GConn := TFDConnection.Create(nil);
  GConn.LoginPrompt := False;
  GConn.Params.DriverID := 'MSSQL';
  GConn.Params.Values['Server'] := ACfg.DbServer;
  GConn.Params.Database := ACfg.DbDatabase;
  if ACfg.OSAuthent then
    GConn.Params.Values['OSAuthent'] := 'Yes'
  else
  begin
    GConn.Params.UserName := ACfg.DbUserName;
    GConn.Params.Password := ACfg.DbPassword;
  end;

  GEmb := TAiOpenAiEmbeddings.Create(nil);
  GEmb.ApiKey := ACfg.ApiKey;
  GEmb.Model := ACfg.Model;

  GDriver := TAiRAGVectorMSSQLDriver.Create(nil);
  GDriver.Connection := GConn;
  GDriver.TableName := ACfg.TableName;
  GDriver.CurrentEntidad := ACfg.Entidad;
  GDriver.Language := ParseLanguage(ACfg.Language);

  GRag := TAiRAGVector.Create(nil, True);
  GRag.Embeddings := GEmb;
  GRag.Driver := GDriver;
  GRag.Entidad := ACfg.Entidad;
  GRag.SearchOptions.UseEmbeddings := True;
  GRag.SearchOptions.UseBM25 := ACfg.UseBM25;
  GRag.SearchOptions.EmbeddingWeight := ACfg.EmbeddingWeight;
  GRag.SearchOptions.BM25Weight := ACfg.BM25Weight;

  WriteLn(ErrOutput, Format('[rag] SQL Server=%s DB=%s Tabla=%s Entidad=%s Modelo=%s Dim=%d',
    [ACfg.DbServer, ACfg.DbDatabase, ACfg.TableName, ACfg.Entidad, ACfg.Model, ACfg.Dimensions]));
end;

procedure DoneRagEngine;
begin
  FreeAndNil(GRag);
  FreeAndNil(GDriver);
  FreeAndNil(GEmb);
  FreeAndNil(GConn);
  FreeAndNil(GDriverLink);
  FreeAndNil(GAuth);
  FreeAndNil(GLock);
end;

// Conexion y esquema perezosos: el servidor arranca aunque la DB no este
// disponible; cada operacion reporta el error real si sigue caida.
procedure EnsureDb;
begin
  if not GConn.Connected then
    GConn.Connected := True;

  if not GSchemaReady then
  begin
    GDriver.CreateSchema(GCfg.TableName, GCfg.Dimensions);
    GSchemaReady := True;
    if not GDriver.FullTextAvailable then
      WriteLn(ErrOutput, '[rag] Aviso: Full-Text Search no disponible; busqueda solo vectorial');
  end;
end;

// Borra en la DB todos los chunks de un documento (via metadato JSON 'doc')
// y limpia la copia cache en memoria de la sesion actual.
function RemoveDocChunks(const ADoc: string): Integer;
var
  Q: TFDQuery;
  i: Integer;
  Node: TAiEmbeddingNode;
  MemRemoved: Boolean;
begin
  Q := TFDQuery.Create(nil);
  try
    Q.Connection := GConn;
    Q.SQL.Text := 'DELETE FROM ' + GDriver.TableName +
      ' WHERE entidad = :ent AND JSON_VALUE(properties, ''$.doc'') = :doc';
    Q.ParamByName('ent').AsString := GCfg.Entidad;
    Q.ParamByName('doc').AsString := ADoc;
    Q.ExecSQL;
    Result := Q.RowsAffected;
  finally
    Q.Free;
  end;

  MemRemoved := False;
  for i := GRag.Items.Count - 1 downto 0 do
  begin
    Node := GRag.Items[i];
    if SameText(VarToStr(Node.MetaData['doc']), ADoc) then
    begin
      GRag.Items.Delete(i);
      Node.Free;
      MemRemoved := True;
    end;
  end;
  if MemRemoved then
    GRag.BuildIndex; // los indices en memoria (HNSW/BM25) guardan referencias a los nodos
end;

// ---------------------------------------------------------------------------
// TRagParams
// ---------------------------------------------------------------------------

constructor TRagParams.Create;
begin
  inherited;
  FTopK := 5;
  FMinScore := '0';
  FChunkSize := 800;
  FOverlapPct := 15;
end;

// ---------------------------------------------------------------------------
// TRagTool
// ---------------------------------------------------------------------------

constructor TRagTool.Create;
begin
  inherited;
  FName := 'rag_vector';
  FDescription :=
    'Sistema RAG (Retrieval-Augmented Generation) sobre SQL Server 2025: ' +
    'embeddings de OpenAI + BM25 hibrido ejecutados en T-SQL (VECTOR_DISTANCE + FREETEXTTABLE). ' +
    'Operaciones: ' +
    'index_text (indexa un texto; params: text_content, doc_name, chunk_size, overlap_pct), ' +
    'index_file (indexa un archivo de texto; params: filepath, doc_name opcional), ' +
    'search (busqueda semantica; params: query, top_k, min_score), ' +
    'list_docs (documentos indexados), ' +
    'delete_doc (elimina un documento; param: doc_name), ' +
    'clear (vacia el indice de la entidad), ' +
    'stats (estadisticas). ' +
    'Los datos persisten en la base de datos configurada en el archivo .ini.';
end;

function TRagTool.IndexText(const AText, ADocName: string; AChunkSize, AOverlapPct: Integer): Integer;
var
  Meta: TAiEmbeddingMetaData;
begin
  // Reindexar = borrar la version anterior del documento primero
  RemoveDocChunks(ADocName);

  Meta := TAiEmbeddingMetaData.Create;
  try
    Meta['doc'] := ADocName;
    Result := GRag.AddItemsFromPlainText(AText, Meta, AChunkSize, AOverlapPct);
  finally
    Meta.Free; // AddItem copia los metadatos con Assign; el caller conserva la propiedad
  end;
end;

function TRagTool.DoIndexText(const P: TRagParams): TJSONObject;
var
  Chunks: Integer;
begin
  if P.TextContent.Trim = '' then
    raise Exception.Create('"text_content" es requerido para index_text');
  if P.DocName.Trim = '' then
    raise Exception.Create('"doc_name" es requerido para index_text');

  Chunks := IndexText(P.TextContent, P.DocName.Trim, P.ChunkSize, P.OverlapPct);

  Result := TJSONObject.Create;
  Result.AddPair('ok', TJSONTrue.Create);
  Result.AddPair('doc', P.DocName.Trim);
  Result.AddPair('chunks', TJSONNumber.Create(Chunks));
end;

function TRagTool.DoIndexFile(const P: TRagParams): TJSONObject;
var
  DocName, RawText: string;
  Chunks: Integer;
begin
  if P.FilePath.Trim = '' then
    raise Exception.Create('"filepath" es requerido para index_file');
  if not TFile.Exists(P.FilePath) then
    raise Exception.CreateFmt('Archivo no encontrado: %s', [P.FilePath]);

  if P.DocName.Trim <> '' then
    DocName := P.DocName.Trim
  else
    DocName := TPath.GetFileName(P.FilePath);

  // UTF-8 estricto con fallback a ANSI/deteccion por BOM
  try
    RawText := TFile.ReadAllText(P.FilePath, TEncoding.UTF8);
  except
    RawText := TFile.ReadAllText(P.FilePath);
  end;

  Chunks := IndexText(RawText, DocName, P.ChunkSize, P.OverlapPct);

  Result := TJSONObject.Create;
  Result.AddPair('ok', TJSONTrue.Create);
  Result.AddPair('doc', DocName);
  Result.AddPair('chunks', TJSONNumber.Create(Chunks));
end;

function TRagTool.DoSearch(const P: TRagParams): TJSONObject;
var
  ResVec: TAiRAGVector;
  ResultsArr: TJSONArray;
  ResObj: TJSONObject;
  Node: TAiEmbeddingNode;
  MinScore: Double;
  TopK, i: Integer;
begin
  if P.Query.Trim = '' then
    raise Exception.Create('"query" es requerido para search');

  TopK := P.TopK;
  if TopK < 1 then
    TopK := 5;
  MinScore := StrToFloatDef(P.MinScore, 0, TFormatSettings.Invariant);

  ResultsArr := TJSONArray.Create;

  // Con Driver asignado, Search delega al motor T-SQL y devuelve un vector
  // PROPIETARIO de sus nodos: ResVec.Free libera contenedor y nodos.
  ResVec := GRag.Search(P.Query, TopK, MinScore, nil);
  try
    for i := 0 to ResVec.Count - 1 do
    begin
      Node := ResVec.Items[i];
      ResObj := TJSONObject.Create;
      ResObj.AddPair('doc', VarToStr(Node.MetaData['doc']));
      ResObj.AddPair('chunk', VarToStr(Node.MetaData['Posicion']));
      ResObj.AddPair('score', TJSONNumber.Create(Node.Idx));
      ResObj.AddPair('text', Node.Text);
      ResultsArr.AddElement(ResObj);
    end;
  finally
    ResVec.Free;
  end;

  Result := TJSONObject.Create;
  Result.AddPair('ok', TJSONTrue.Create);
  Result.AddPair('count', TJSONNumber.Create(ResultsArr.Count));
  Result.AddPair('results', ResultsArr);
end;

function TRagTool.DoListDocs: TJSONObject;
var
  Q: TFDQuery;
  DocsArr: TJSONArray;
  DocObj: TJSONObject;
  Total: Integer;
begin
  Total := 0;
  DocsArr := TJSONArray.Create;

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := GConn;
    Q.SQL.Text :=
      'SELECT ISNULL(JSON_VALUE(properties, ''$.doc''), '''') AS doc, COUNT(*) AS chunks ' +
      'FROM ' + GDriver.TableName + ' WHERE entidad = :ent ' +
      'GROUP BY JSON_VALUE(properties, ''$.doc'') ORDER BY 1';
    Q.ParamByName('ent').AsString := GCfg.Entidad;
    Q.Open;
    while not Q.Eof do
    begin
      DocObj := TJSONObject.Create;
      DocObj.AddPair('doc', Q.FieldByName('doc').AsString);
      DocObj.AddPair('chunks', TJSONNumber.Create(Q.FieldByName('chunks').AsInteger));
      DocsArr.AddElement(DocObj);
      Inc(Total, Q.FieldByName('chunks').AsInteger);
      Q.Next;
    end;
  finally
    Q.Free;
  end;

  Result := TJSONObject.Create;
  Result.AddPair('ok', TJSONTrue.Create);
  Result.AddPair('docs', DocsArr);
  Result.AddPair('total_chunks', TJSONNumber.Create(Total));
end;

function TRagTool.DoDeleteDoc(const P: TRagParams): TJSONObject;
var
  Removed: Integer;
begin
  if P.DocName.Trim = '' then
    raise Exception.Create('"doc_name" es requerido para delete_doc');

  Removed := RemoveDocChunks(P.DocName.Trim);

  Result := TJSONObject.Create;
  Result.AddPair('ok', TJSONTrue.Create);
  Result.AddPair('removed_chunks', TJSONNumber.Create(Removed));
end;

function TRagTool.DoClear: TJSONObject;
var
  Q: TFDQuery;
  Cleared: Integer;
begin
  Q := TFDQuery.Create(nil);
  try
    Q.Connection := GConn;
    Q.SQL.Text := 'DELETE FROM ' + GDriver.TableName + ' WHERE entidad = :ent';
    Q.ParamByName('ent').AsString := GCfg.Entidad;
    Q.ExecSQL;
    Cleared := Q.RowsAffected;
  finally
    Q.Free;
  end;

  GRag.Clear; // cache en memoria de la sesion

  Result := TJSONObject.Create;
  Result.AddPair('ok', TJSONTrue.Create);
  Result.AddPair('cleared_chunks', TJSONNumber.Create(Cleared));
end;

function TRagTool.DoStats: TJSONObject;
var
  Q: TFDQuery;
  TotalDocs, TotalChunks: Integer;
begin
  TotalDocs := 0;
  TotalChunks := 0;

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := GConn;
    Q.SQL.Text :=
      'SELECT COUNT(*) AS chunks, COUNT(DISTINCT JSON_VALUE(properties, ''$.doc'')) AS docs ' +
      'FROM ' + GDriver.TableName + ' WHERE entidad = :ent';
    Q.ParamByName('ent').AsString := GCfg.Entidad;
    Q.Open;
    if not Q.Eof then
    begin
      TotalChunks := Q.FieldByName('chunks').AsInteger;
      TotalDocs := Q.FieldByName('docs').AsInteger;
    end;
  finally
    Q.Free;
  end;

  Result := TJSONObject.Create;
  Result.AddPair('ok', TJSONTrue.Create);
  Result.AddPair('total_docs', TJSONNumber.Create(TotalDocs));
  Result.AddPair('total_chunks', TJSONNumber.Create(TotalChunks));
  Result.AddPair('embedding_model', GEmb.Model);
  Result.AddPair('database', GCfg.DbDatabase);
  Result.AddPair('table', GDriver.TableName);
  Result.AddPair('entidad', GCfg.Entidad);
  Result.AddPair('fulltext_available', TJSONBool.Create(GDriver.FullTextAvailable));
end;

function TRagTool.ExecuteWithParams(const AParams: TRagParams; const AuthContext: TAiAuthContext): TJSONObject;
var
  Op: string;
  R: TJSONObject;
  ErrObj: TJSONObject;
begin
  GLock.Enter;
  try
    try
      Op := LowerCase(Trim(AParams.Operation));
      if Op = '' then
        raise Exception.Create('"operation" es requerido');

      EnsureDb;

      if Op = 'index_text' then
        R := DoIndexText(AParams)
      else if Op = 'index_file' then
        R := DoIndexFile(AParams)
      else if Op = 'search' then
        R := DoSearch(AParams)
      else if Op = 'list_docs' then
        R := DoListDocs
      else if Op = 'delete_doc' then
        R := DoDeleteDoc(AParams)
      else if Op = 'clear' then
        R := DoClear
      else if Op = 'stats' then
        R := DoStats
      else
        raise Exception.CreateFmt('Operacion desconocida "%s"', [Op]);

      Result := TAiMCPResponseBuilder.New.AddText(R.ToJSON).Build;
      R.Free;
    except
      on E: Exception do
      begin
        // Construimos el error como JSON real para escapar el mensaje con seguridad
        ErrObj := TJSONObject.Create;
        try
          ErrObj.AddPair('ok', TJSONFalse.Create);
          ErrObj.AddPair('error', E.Message);
          Result := TAiMCPResponseBuilder.New.AddText(ErrObj.ToJSON).Build;
        finally
          ErrObj.Free;
        end;
      end;
    end;
  finally
    GLock.Leave;
  end;
end;

procedure RegisterTools(AServer: TAiMCPServer);
begin
  // Autenticacion: login/password quemados via OnValidateRequest (Layer 2)
  AServer.OnValidateRequest := GAuth.HandleValidateRequest;

  AServer.RegisterTool('rag_vector',
    function: IAiMCPTool
    begin
      Result := TRagTool.Create;
    end);
  WriteLn(ErrOutput, '[rag]   + rag_vector (autenticacion activa: usuario "' + RAG_LOGIN + '")');
end;

end.
