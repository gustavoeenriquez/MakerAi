unit uBotMemory;

// =============================================================================
// TBotMemory — Módulo de memoria semántica para Demo 058 FMX Telegram Bot
//
// Combina dos capas de almacenamiento:
//   1. KV store JSON  → acceso exacto rápido (save/get/delete/list)
//   2. TAiRAGVector   → búsqueda semántica híbrida (BM25 + embeddings)
//
// Archivos de datos en ADataDir:
//   bot_memory.json      — KV store persistido
//   bot_memory.mkvec     — índice vectorial binario (TAiMkVecDriver)
//
// Uso:
//   FMemory := TBotMemory.Create(FDataDir, '@OPENAI_API_KEY');
//   FMemory.Save('capital_france', 'Paris', 'geography');
//   txt := FMemory.Search('cities in Europe', 5);
//   txt := FMemory.Get('capital_france');
// =============================================================================

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vector.Driver.BinFile,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.MetaData,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Chat.Initializations;

type
  // ---------------------------------------------------------------------------
  // TBotMemory
  // ---------------------------------------------------------------------------
  TBotMemory = class
  private
    // ── KV store (acceso exacto) ────────────────────────────────────────────
    FKVData : TJSONObject;   // key → {value, category, timestamp}
    FKVFile : string;
    FLock   : TCriticalSection;

    // ── RAG (búsqueda semántica) ────────────────────────────────────────────
    FRagVector  : TAiRAGVector;
    FDriver     : TAiMkVecDriver;
    FEmbConn    : TAiEmbeddingConnection;
    FRagEnabled : Boolean;
    FEntidad    : string;

    // ── Helpers ────────────────────────────────────────────────────────────
    function  NowISO: string;
    procedure KVSave;
    procedure KVLoad;
    procedure RagAdd(const AKey, AValue, ACategory: string);
    procedure RagDelete(const AKey: string);
  public
    constructor Create(const ADataDir, AApiKey: string;
      const AEmbDriverName: string = 'OpenAI';
      const AEmbModel: string = 'text-embedding-3-small');
    destructor  Destroy; override;

    // Exact operations
    function Save(const AKey, AValue: string; const ACategory: string = 'general'): string;
    function Get(const AKey: string): string;
    function Delete(const AKey: string): Boolean;
    function List: TJSONArray;

    // Semantic search
    function Search(const AQuery: string; ALimit: Integer = 5): string;

    property RagEnabled: Boolean read FRagEnabled;
  end;

implementation

{ TBotMemory }

constructor TBotMemory.Create(const ADataDir, AApiKey: string;
  const AEmbDriverName: string; const AEmbModel: string);
begin
  inherited Create;
  FEntidad := 'memory';
  FLock    := TCriticalSection.Create;
  FKVFile  := TPath.Combine(ADataDir, 'bot_memory.json');

  FLock.Enter;
  try KVLoad;
  finally FLock.Leave; end;

  // ── Inicializar RAG (no fatal si falla) ──────────────────────────────────
  FRagEnabled := False;
  FRagVector  := nil;
  FDriver     := nil;
  FEmbConn    := nil;
  try
    // Embedding connection
    FEmbConn            := TAiEmbeddingConnection.Create(nil);
    FEmbConn.ApiKey     := AApiKey;         // establecer ApiKey ANTES del DriverName
    FEmbConn.DriverName := AEmbDriverName;
    if AEmbModel <> '' then
      FEmbConn.Model := AEmbModel;

    // Binary file driver (sin dependencias externas)
    FDriver          := TAiMkVecDriver.Create(nil);
    FDriver.FilePath := TPath.Combine(ADataDir, 'bot_memory.mkvec');

    // RAG vector store
    FRagVector := TAiRAGVector.Create(nil);
    FRagVector.Entidad       := FEntidad;
    FRagVector.Driver        := FDriver;
    FRagVector.Embeddings    := FEmbConn.AiEmbeddings;
    FRagVector.LexicalLanguage := alEnglish;

    // Opciones de búsqueda: híbrido BM25 + semántica, fusión RRF
    FRagVector.SearchOptions.UseEmbeddings := True;
    FRagVector.SearchOptions.UseBM25       := True;
    FRagVector.SearchOptions.UseRRF        := True;

    FRagEnabled := True;
  except on E: Exception do
  begin
    // RAG no disponible: el KV store sigue funcionando
    FRagEnabled := False;
    FreeAndNil(FRagVector);
    FreeAndNil(FDriver);
    FreeAndNil(FEmbConn);
  end;
  end;
end;

destructor TBotMemory.Destroy;
begin
  FreeAndNil(FRagVector);
  FreeAndNil(FDriver);
  FreeAndNil(FEmbConn);

  FLock.Enter;
  try FreeAndNil(FKVData);
  finally FLock.Leave; end;
  FreeAndNil(FLock);

  inherited;
end;

// =============================================================================
// Helpers privados
// =============================================================================

function TBotMemory.NowISO: string;
begin
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
end;

procedure TBotMemory.KVLoad;
var JVal: TJSONValue;
begin
  FreeAndNil(FKVData);
  if TFile.Exists(FKVFile) then
    try
      JVal := TJSONObject.ParseJSONValue(TFile.ReadAllText(FKVFile, TEncoding.UTF8));
      if JVal is TJSONObject then
        FKVData := TJSONObject(JVal)
      else
        JVal.Free;
    except end;
  if FKVData = nil then
    FKVData := TJSONObject.Create;
end;

procedure TBotMemory.KVSave;
var Dir: string;
begin
  Dir := TPath.GetDirectoryName(FKVFile);
  if not TDirectory.Exists(Dir) then
    TDirectory.CreateDirectory(Dir);
  TFile.WriteAllText(FKVFile, FKVData.Format(2), TEncoding.UTF8);
end;

procedure TBotMemory.RagAdd(const AKey, AValue, ACategory: string);
var
  Node : TAiEmbeddingNode;
  Meta : TAiEmbeddingMetaData;
  Text : string;
begin
  if not FRagEnabled then Exit;
  try
    // Borrar registro previo con la misma clave (actualización)
    RagDelete(AKey);

    Text := '[' + AKey + ']: ' + AValue;
    Node := FRagVector.CreateEmbeddingNode(Text);
    Node.Tag := AKey;   // clave = ID en el .mkvec (permite Delete exacto)

    Meta := TAiEmbeddingMetaData.Create;
    try
      Meta['key']       := AKey;
      Meta['category']  := ACategory;
      Meta['timestamp'] := NowISO;
      FRagVector.AddItem(Node, Meta);
    finally
      Meta.Free;
    end;
  except
    // Errores de red/embedding son no fatales
  end;
end;

procedure TBotMemory.RagDelete(const AKey: string);
begin
  if FRagEnabled and Assigned(FDriver) then
    try
      FDriver.Delete(AKey, FEntidad);
    except end;
end;

// =============================================================================
// Operaciones exactas (KV store)
// =============================================================================

function TBotMemory.Save(const AKey, AValue: string; const ACategory: string): string;
var
  JEntry : TJSONObject;
  Pair   : TJSONPair;
  JR     : TJSONObject;
begin
  FLock.Enter;
  try
    // Actualizar KV store
    Pair := FKVData.RemovePair(AKey);
    Pair.Free;

    JEntry := TJSONObject.Create;
    JEntry.AddPair('value',     AValue);
    JEntry.AddPair('category',  ACategory);
    JEntry.AddPair('timestamp', NowISO);
    FKVData.AddPair(AKey, JEntry);
    KVSave;
  finally
    FLock.Leave;
  end;

  // Actualizar índice RAG (fuera del lock; puede ser lento por la red)
  RagAdd(AKey, AValue, ACategory);

  JR := TJSONObject.Create;
  try
    JR.AddPair('ok',       TJSONBool.Create(True));
    JR.AddPair('key',      AKey);
    JR.AddPair('category', ACategory);
    Result := JR.ToJSON;
  finally JR.Free; end;
end;

function TBotMemory.Get(const AKey: string): string;
var
  JEntry : TJSONObject;
  Value  : string;
  JR     : TJSONObject;
begin
  Value := '';
  FLock.Enter;
  try
    if FKVData.TryGetValue<TJSONObject>(AKey, JEntry) then
      JEntry.TryGetValue<string>('value', Value);
  finally FLock.Leave; end;

  JR := TJSONObject.Create;
  try
    JR.AddPair('key',   AKey);
    JR.AddPair('found', TJSONBool.Create(Value <> ''));
    if Value <> '' then JR.AddPair('value', Value);
    Result := JR.ToJSON;
  finally JR.Free; end;
end;

function TBotMemory.Delete(const AKey: string): Boolean;
var Pair: TJSONPair;
begin
  FLock.Enter;
  try
    Pair   := FKVData.RemovePair(AKey);
    Result := Assigned(Pair);
    Pair.Free;
    if Result then KVSave;
  finally FLock.Leave; end;

  if Result then RagDelete(AKey);
end;

function TBotMemory.List: TJSONArray;
var
  P      : TJSONPair;
  JEntry : TJSONObject;
  JElem  : TJSONObject;
  Value  : string;
  Cat    : string;
  Ts     : string;
begin
  Result := TJSONArray.Create;
  FLock.Enter;
  try
    for P in FKVData do
    begin
      Value := '';
      Cat   := '';
      Ts    := '';
      if P.JsonValue is TJSONObject then
      begin
        JEntry := TJSONObject(P.JsonValue);
        JEntry.TryGetValue<string>('value',     Value);
        JEntry.TryGetValue<string>('category',  Cat);
        JEntry.TryGetValue<string>('timestamp', Ts);
      end
      else
        Value := P.JsonValue.Value;   // compatibilidad con formato antiguo

      JElem := TJSONObject.Create;
      JElem.AddPair('key',      P.JsonString.Value);
      JElem.AddPair('value',    Value);
      JElem.AddPair('category', Cat);
      JElem.AddPair('timestamp', Ts);
      Result.Add(JElem);
    end;
  finally FLock.Leave; end;
end;

// =============================================================================
// Búsqueda semántica (RAG)
// =============================================================================

function TBotMemory.Search(const AQuery: string; ALimit: Integer): string;
begin
  if not FRagEnabled then
  begin
    Result := '(semantic search not available — embedding API not configured)';
    Exit;
  end;
  try
    Result := FRagVector.SearchText(AQuery, ALimit, 0.3);
    if Result = '' then
      Result := '(no relevant memories found)';
  except on E: Exception do
    Result := '(search error: ' + E.Message + ')';
  end;
end;

end.
