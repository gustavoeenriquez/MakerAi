unit uMakerAi.RAG.Vector.Driver.BinFile;

{
  TAiMkVecDriver — Driver de archivo binario propio para TAiRAGVector

  Base de datos local para el usuario, sin dependencias externas.
  Solo System.*: no SQLite, no FireDAC, no PostgreSQL.

  Estrategia de memoria / disco:
  ┌──────────────────────────────────────────────────────────────────┐
  │  EN MEMORIA (liviano)                                            │
  │  ─────────────────────────────────────────────────────────────── │
  │  FEntryIndex:  compKey → TNodeFileEntry  (offsets en archivo)    │
  │  FStubNodes:   compKey → TAiEmbeddingNode (sin embedding!)       │
  │  FBm25Indices: entidad → TAIBm25Index (usa los stubs)            │
  │                                                                  │
  │  Para N=100K nodos / texto promedio 500 bytes:                   │
  │    Stubs       ≈ 50 MB    (texto + metadata, sin floats)         │
  │    EntryIndex  ≈  6 MB    (solo offsets)                         │
  │    BM25 index  ≈ 20 MB    (índice invertido)                     │
  │                                                                  │
  │  EN DISCO (.mkvec)                                               │
  │  ─────────────────────────────────────────────────────────────── │
  │  Todo: embedding (Dim×8 bytes float64) + texto + metadata        │
  │  El disco es el límite, no la RAM de embeddings                  │
  └──────────────────────────────────────────────────────────────────┘

  Búsqueda vectorial (Fase 1 — scan de embeddings):
    Para cada nodo activo: seek(EmbOffset) → leer Dim×8 bytes → coseno
    Solo se leen embeddings, NO el texto. O(n) pero secuencial.

  Búsqueda vectorial (Fase 2 — Top-K):
    seek(TextOffset) → leer texto + metadata
    Solo ALimit lecturas completas de disco.

  Búsqueda léxical BM25:
    Completamente en memoria via TAIBm25Index.Search() sobre los stubs.
    Los stubs tienen Tag = ID del nodo en el archivo.
    Luego se cargan los textos completos para el Top-K.

  Búsqueda híbrida:
    Ambas búsquedas por separado, fusión RRF o ponderada.

  Formato del archivo (.mkvec):
    Header 64 bytes:
      [4] 'MKVC'  [2] Version=1  [4] Dim  [8] NodeCount  [46] Reserved
    Registros (variable):
      [1] Status  [2+N] Entidad  [2+N] ID  [2+N] Model
      [Dim×8] Embedding (float64 = Double)
      [4+N] Text  [4+N] Properties JSON

  Notas:
    - Delete() y Clear() marcan registros como status=0 (deleted).
    - Compact() reescribe el archivo sin los registros borrados.
    - RebuildBm25(entidad) se llama tras Delete/Clear: limpia el
      índice BM25 y re-agrega los stubs activos. Es O(n_activos),
      no requiere releer el disco.
}

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.JSON,
  System.Math,
  System.SyncObjs,
  System.Generics.Collections,
  System.Generics.Defaults,
  uMakerAi.Embeddings.Core,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.MetaData;

const
  MKVEC_MAGIC_0 = Ord('M');
  MKVEC_MAGIC_1 = Ord('K');
  MKVEC_MAGIC_2 = Ord('V');
  MKVEC_MAGIC_3 = Ord('C');
  MKVEC_VERSION     = 1;
  MKVEC_HEADER_SIZE = 64;
  MKVEC_ACTIVE      = 1;
  MKVEC_DELETED     = 0;

type
  // ---------------------------------------------------------------------------
  // Entrada del índice de archivo: offsets para lectura eficiente
  // ---------------------------------------------------------------------------
  TNodeFileEntry = record
    RecordOffset: Int64;  // inicio del byte de status
    EmbOffset:    Int64;  // inicio del array float64 (Double)
    TextOffset:   Int64;  // inicio del campo TextLen
  end;

  // ---------------------------------------------------------------------------
  // TAiMkVecDriver
  // ---------------------------------------------------------------------------
  TAiMkVecDriver = class(TAiVectorStoreDriverBase)
  private
    FFilePath:    string;
    FDim:         Integer;
    FLanguage:    TAiLanguage;
    FAutoOpen:    Boolean;
    FStream:      TFileStream;
    FLock:        TCriticalSection;

    // Índice de offsets: compKey → TNodeFileEntry
    FEntryIndex:  TDictionary<string, TNodeFileEntry>;

    // IDs activos por entidad: entidad → TList<string> de IDs
    FEntidadIDs:  TObjectDictionary<string, TList<string>>;

    // Stubs: compKey → TAiEmbeddingNode (sin embedding, para BM25)
    FStubNodes:   TObjectDictionary<string, TAiEmbeddingNode>;

    // Índices BM25: entidad → TAIBm25Index
    FBm25Indices: TObjectDictionary<string, TAIBm25Index>;

    function  CompKey(const AEntidad, AID: string): string; inline;
    procedure EnsureOpen;
    procedure WriteFileHeader;
    procedure FlushNodeCount;
    procedure RebuildAllIndices;
    procedure RebuildBm25(const AEntidad: string);
    function  GetOrCreateBm25(const AEntidad: string): TAIBm25Index;

    function  ReadUTF8(ALen: Integer): string;
    procedure WriteUTF8(const S: string; out ABytes: TBytes; out ALen: Word);
    function  ReadEmbedding(AOffset: Int64): TAiEmbeddingData;
    function  LoadFullNode(const AEntry: TNodeFileEntry): TAiEmbeddingNode;

    function  CosineSim(const A, B: TAiEmbeddingData): Double;

    // Búsquedas individuales
    function  VectorSearch(const ATarget: TAiEmbeddingNode;
                const AEntidad: string; ALimit: Integer;
                AMinScore: Double; AFilter: TAiFilterCriteria): TAiRAGVector;
    function  LexicalSearch(const AQuery: string;
                const AEntidad: string; ALimit: Integer;
                AMinScore: Double; AFilter: TAiFilterCriteria): TAiRAGVector;

    // Fusión
    function  FuseRRF(AVec, ALex: TAiRAGVector; ALimit: Integer): TAiRAGVector;
    function  FuseWeighted(AVec, ALex: TAiRAGVector;
                AVW, ALW: Double; ALimit: Integer): TAiRAGVector;

    function  CopyNode(Src: TAiEmbeddingNode): TAiEmbeddingNode;
    procedure TransferFiltered(Src: TAiRAGVector; Dst: TAiRAGVector;
                ALimit: Integer; AMinScore: Double);

    procedure SetFilePath(const Value: string);
    procedure SetLanguage(const Value: TAiLanguage);
    function  GetIsOpen: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    // ---- Contrato TAiVectorStoreDriverBase ----
    procedure Add(const ANode: TAiEmbeddingNode; const AEntidad: string = ''); override;
    function  Search(const ATarget: TAiEmbeddingNode; const AEntidad: string;
                ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria;
                Options: TAiSearchOptions): TAiRAGVector; override;
    procedure Delete(const AID: string; const AEntidad: string); override;
    procedure Clear(const AEntidad: string); override;

    // ---- Gestión del archivo ----
    procedure Open;
    procedure Close;
    procedure Compact;

    function  NodeCount(const AEntidad: string = ''): Int64;
    function  EntidadList: TArray<string>;

    property IsOpen: Boolean read GetIsOpen;
  published
    property FilePath: string   read FFilePath  write SetFilePath;
    property Dim:      Integer  read FDim       write FDim       default 1536;
    property Language: TAiLanguage read FLanguage write SetLanguage default alSpanish;
    property AutoOpen: Boolean  read FAutoOpen  write FAutoOpen  default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI.RAG.Drivers', [TAiMkVecDriver]);
end;

// =============================================================================
// Constructor / Destructor
// =============================================================================

constructor TAiMkVecDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDim      := 1536;
  FLanguage := alSpanish;
  FAutoOpen := True;
  FLock     := TCriticalSection.Create;
  FEntryIndex  := TDictionary<string, TNodeFileEntry>.Create;
  FEntidadIDs  := TObjectDictionary<string, TList<string>>.Create([doOwnsValues]);
  FStubNodes   := TObjectDictionary<string, TAiEmbeddingNode>.Create([doOwnsValues]);
  FBm25Indices := TObjectDictionary<string, TAIBm25Index>.Create([doOwnsValues]);
end;

destructor TAiMkVecDriver.Destroy;
begin
  Close;
  FBm25Indices.Free;
  FStubNodes.Free;
  FEntidadIDs.Free;
  FEntryIndex.Free;
  FLock.Free;
  inherited;
end;

procedure TAiMkVecDriver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

function TAiMkVecDriver.GetIsOpen: Boolean;
begin
  Result := Assigned(FStream);
end;

procedure TAiMkVecDriver.SetFilePath(const Value: string);
begin
  if FFilePath = Value then Exit;
  if Assigned(FStream) then Close;
  FFilePath := Value;
end;

procedure TAiMkVecDriver.SetLanguage(const Value: TAiLanguage);
begin
  FLanguage := Value;
  // Actualizar idioma en todos los índices BM25 existentes
  for var BM25 in FBm25Indices.Values do
    BM25.Language := FLanguage;
end;

// =============================================================================
// Helpers de clave compuesta y acceso a índices
// =============================================================================

function TAiMkVecDriver.CompKey(const AEntidad, AID: string): string;
begin
  // #1 como separador — improbable en IDs o entidades reales
  Result := AEntidad + #1 + AID;
end;

function TAiMkVecDriver.GetOrCreateBm25(const AEntidad: string): TAIBm25Index;
begin
  if not FBm25Indices.TryGetValue(AEntidad, Result) then
  begin
    Result := TAIBm25Index.Create;
    Result.Language := FLanguage;
    FBm25Indices.Add(AEntidad, Result);
  end;
end;

// =============================================================================
// I/O helpers
// =============================================================================

function TAiMkVecDriver.ReadUTF8(ALen: Integer): string;
var
  Buf: TBytes;
begin
  if ALen <= 0 then Exit('');
  SetLength(Buf, ALen);
  FStream.ReadBuffer(Buf[0], ALen);
  Result := TEncoding.UTF8.GetString(Buf);
end;

procedure TAiMkVecDriver.WriteUTF8(const S: string; out ABytes: TBytes; out ALen: Word);
begin
  ABytes := TEncoding.UTF8.GetBytes(S);
  ALen   := Length(ABytes);
end;

function TAiMkVecDriver.ReadEmbedding(AOffset: Int64): TAiEmbeddingData;
begin
  SetLength(Result, FDim);
  FStream.Position := AOffset;
  FStream.ReadBuffer(Result[0], FDim * SizeOf(Double));
end;

function TAiMkVecDriver.LoadFullNode(const AEntry: TNodeFileEntry): TAiEmbeddingNode;
var
  TextLen, PropsLen: Cardinal;
  Text, PropsStr: string;
  JObj: TJSONObject;
begin
  Result := TAiEmbeddingNode.Create(FDim);
  try
    // Cargar embedding desde disco
    Result.Data := ReadEmbedding(AEntry.EmbOffset);
    if Length(Result.Data) > 0 then
      Result.SetDataLength(Length(Result.Data));

    // Cargar texto y propiedades
    FStream.Position := AEntry.TextOffset;
    FStream.ReadBuffer(TextLen, 4);
    Text := ReadUTF8(TextLen);
    FStream.ReadBuffer(PropsLen, 4);
    PropsStr := ReadUTF8(PropsLen);

    Result.Text := Text;

    if PropsStr <> '' then
    begin
      JObj := TJSONObject.ParseJSONValue(PropsStr) as TJSONObject;
      if Assigned(JObj) then
        try Result.MetaData.FromJSON(JObj); finally JObj.Free; end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TAiMkVecDriver.CopyNode(Src: TAiEmbeddingNode): TAiEmbeddingNode;
var
  J: TJSONObject;
begin
  Result := TAiEmbeddingNode.Create(FDim);
  try
    Result.Tag   := Src.Tag;
    Result.Text  := Src.Text;
    Result.Model := Src.Model;
    Result.Idx   := Src.Idx;
    if Length(Src.Data) > 0 then
    begin
      Result.Data := Copy(Src.Data);
      Result.SetDataLength(Length(Result.Data));
    end;
    J := Src.MetaData.ToJSON;
    try Result.MetaData.FromJSON(J); finally J.Free; end;
  except
    Result.Free;
    raise;
  end;
end;

// =============================================================================
// Apertura / cierre
// =============================================================================

procedure TAiMkVecDriver.EnsureOpen;
begin
  if not Assigned(FStream) and FAutoOpen and (FFilePath <> '') then
    Open;
  if not Assigned(FStream) then
    raise Exception.Create('TAiMkVecDriver: archivo no abierto. Asigna FilePath y llama a Open().');
end;

procedure TAiMkVecDriver.Open;
var
  Magic: array[0..3] of Byte;
  Version: Word;
  DimRead: Cardinal;
begin
  FLock.Enter;
  try
    if Assigned(FStream) then Exit;
    if FFilePath = '' then
      raise Exception.Create('TAiMkVecDriver: FilePath vacío.');

    FEntryIndex.Clear;
    FEntidadIDs.Clear;
    FStubNodes.Clear;
    FBm25Indices.Clear;

    if FileExists(FFilePath) then
    begin
      FStream := TFileStream.Create(FFilePath, fmOpenReadWrite or fmShareDenyWrite);
      FStream.ReadBuffer(Magic, 4);
      if (Magic[0] <> MKVEC_MAGIC_0) or (Magic[1] <> MKVEC_MAGIC_1) or
         (Magic[2] <> MKVEC_MAGIC_2) or (Magic[3] <> MKVEC_MAGIC_3) then
        raise Exception.CreateFmt('"%s" no es un archivo .mkvec válido.', [FFilePath]);
      FStream.ReadBuffer(Version, 2);
      if Version > MKVEC_VERSION then
        raise Exception.CreateFmt('Versión %d no soportada.', [Version]);
      FStream.ReadBuffer(DimRead, 4);
      FDim := DimRead;
      RebuildAllIndices;
    end
    else
    begin
      FStream := TFileStream.Create(FFilePath, fmCreate or fmShareDenyWrite);
      WriteFileHeader;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TAiMkVecDriver.Close;
begin
  FLock.Enter;
  try
    if not Assigned(FStream) then Exit;
    FlushNodeCount;
    FStream.Free;
    FStream := nil;
  finally
    FLock.Leave;
  end;
end;

procedure TAiMkVecDriver.WriteFileHeader;
var
  Header: array[0..MKVEC_HEADER_SIZE - 1] of Byte;
  DimCard: Cardinal;
  NC: Int64;
begin
  FillChar(Header, MKVEC_HEADER_SIZE, 0);
  Header[0] := MKVEC_MAGIC_0; Header[1] := MKVEC_MAGIC_1;
  Header[2] := MKVEC_MAGIC_2; Header[3] := MKVEC_MAGIC_3;
  Header[4] := MKVEC_VERSION; Header[5] := 0;
  DimCard := FDim;
  Move(DimCard, Header[6], 4);
  NC := 0;
  Move(NC, Header[10], 8);
  FStream.Position := 0;
  FStream.WriteBuffer(Header, MKVEC_HEADER_SIZE);
end;

procedure TAiMkVecDriver.FlushNodeCount;
var
  NC: Int64;
begin
  if not Assigned(FStream) then Exit;
  NC := NodeCount;
  FStream.Position := 10;
  FStream.WriteBuffer(NC, 8);
end;

// =============================================================================
// Reconstrucción de índices desde el archivo (se llama en Open)
// =============================================================================

procedure TAiMkVecDriver.RebuildAllIndices;
var
  Status:   Byte;
  EntLen, IDLen, ModelLen: Word;
  Entidad, ID, Model, PropsStr: string;
  TextLen, PropsLen: Cardinal;
  RecOff, EmbOff, TxtOff: Int64;
  Entry: TNodeFileEntry;
  CK: string;
  Stub: TAiEmbeddingNode;
  IDList: TList<string>;
  BM25: TAIBm25Index;
  JObj: TJSONObject;
begin
  FStream.Position := MKVEC_HEADER_SIZE;

  while FStream.Position < FStream.Size do
  begin
    RecOff := FStream.Position;

    FStream.ReadBuffer(Status, 1);

    // Leer campos de cadena del registro
    FStream.ReadBuffer(EntLen, 2);   Entidad := ReadUTF8(EntLen);
    FStream.ReadBuffer(IDLen, 2);    ID      := ReadUTF8(IDLen);
    FStream.ReadBuffer(ModelLen, 2); Model   := ReadUTF8(ModelLen);

    EmbOff := FStream.Position;
    FStream.Seek(FDim * SizeOf(Double), soFromCurrent); // saltar embedding

    TxtOff := FStream.Position;
    FStream.ReadBuffer(TextLen, 4);
    var TextStr := ReadUTF8(TextLen);
    FStream.ReadBuffer(PropsLen, 4);
    PropsStr := ReadUTF8(PropsLen);

    if Status <> MKVEC_ACTIVE then Continue;

    CK := CompKey(Entidad, ID);

    // --- Índice de offsets ---
    Entry.RecordOffset := RecOff;
    Entry.EmbOffset    := EmbOff;
    Entry.TextOffset   := TxtOff;
    FEntryIndex.AddOrSetValue(CK, Entry);

    // --- IDs por entidad ---
    if not FEntidadIDs.TryGetValue(Entidad, IDList) then
    begin
      IDList := TList<string>.Create;
      FEntidadIDs.Add(Entidad, IDList);
    end;
    IDList.Add(ID);

    // --- Stub node para BM25 (sin embedding) ---
    Stub := TAiEmbeddingNode.Create(0); // dim=0: sin array de floats
    Stub.Tag   := ID;
    Stub.Text  := TextStr;
    Stub.Model := Model;
    if PropsStr <> '' then
    begin
      JObj := TJSONObject.ParseJSONValue(PropsStr) as TJSONObject;
      if Assigned(JObj) then
        try Stub.MetaData.FromJSON(JObj); finally JObj.Free; end;
    end;
    FStubNodes.AddOrSetValue(CK, Stub);

    // --- Agregar al BM25 ---
    BM25 := GetOrCreateBm25(Entidad);
    BM25.AddNode(Stub);
  end;
end;

// =============================================================================
// Reconstrucción BM25 tras borrados (solo en memoria, no releer disco)
// =============================================================================

procedure TAiMkVecDriver.RebuildBm25(const AEntidad: string);
var
  BM25: TAIBm25Index;
  IDList: TList<string>;
  Stub: TAiEmbeddingNode;
  I: Integer;
begin
  BM25 := GetOrCreateBm25(AEntidad);
  BM25.Clear;

  if not FEntidadIDs.TryGetValue(AEntidad, IDList) then Exit;
  for I := 0 to IDList.Count - 1 do
  begin
    if FStubNodes.TryGetValue(CompKey(AEntidad, IDList[I]), Stub) then
      BM25.AddNode(Stub);
  end;
end;

// =============================================================================
// CRUD
// =============================================================================

procedure TAiMkVecDriver.Add(const ANode: TAiEmbeddingNode; const AEntidad: string);
var
  LEnt, LID, LModel, PropsStr: string;
  EntB, IDB, ModelB, TextB, PropsB: TBytes;
  EntLen, IDLen, ModelLen: Word;
  TextLen, PropsLen: Cardinal;
  Status: Byte;
  CK: string;
  Entry: TNodeFileEntry;
  IDList: TList<string>;
  BM25: TAIBm25Index;
  Stub: TAiEmbeddingNode;
  J: TJSONObject;
  EmbData: TAiEmbeddingData;
  I: Integer;
begin
  EnsureOpen;
  if FDim <= 0 then
    raise Exception.Create('Dim debe ser > 0 antes de Add.');

  LEnt   := IfThen(AEntidad = '', 'DEFAULT', AEntidad);
  LID    := ANode.Tag;
  LModel := ANode.Model;
  CK     := CompKey(LEnt, LID);

  // Si ya existe, borrarlo primero (sin reconstruir BM25 todavía)
  if FEntryIndex.ContainsKey(CK) then
  begin
    // Marcar como borrado en archivo
    var OldEntry: TNodeFileEntry;
    if FEntryIndex.TryGetValue(CK, OldEntry) then
    begin
      Status := MKVEC_DELETED;
      FStream.Position := OldEntry.RecordOffset;
      FStream.WriteBuffer(Status, 1);
    end;
    // Limpiar de índices en memoria
    FEntryIndex.Remove(CK);
    FStubNodes.Remove(CK); // libera el stub viejo (doOwnsValues)
    if FEntidadIDs.TryGetValue(LEnt, IDList) then
    begin
      for I := IDList.Count - 1 downto 0 do
        if IDList[I] = LID then { IDList.Delete(I); } Break;
      // No borrar de IDList aquí — lo reemplazaremos al agregar
    end;
  end;

  // Preparar bytes
  WriteUTF8(LEnt,   EntB,   EntLen);
  WriteUTF8(LID,    IDB,    IDLen);
  WriteUTF8(LModel, ModelB, ModelLen);
  TextB  := TEncoding.UTF8.GetBytes(ANode.Text);
  TextLen := Length(TextB);

  J := ANode.MetaData.ToJSON;
  try
    PropsStr := IfThen(Assigned(J), J.ToJSON, '{}');
  finally
    J.Free;
  end;
  PropsB  := TEncoding.UTF8.GetBytes(PropsStr);
  PropsLen := Length(PropsB);

  // Preparar embedding (con padding si la dimensión no coincide)
  SetLength(EmbData, FDim);
  FillChar(EmbData[0], FDim * SizeOf(Double), 0);
  if Length(ANode.Data) > 0 then
    Move(ANode.Data[0], EmbData[0], Min(Length(ANode.Data), FDim) * SizeOf(Double));

  FLock.Enter;
  try
    // Append al final del archivo
    FStream.Seek(0, soFromEnd);
    Entry.RecordOffset := FStream.Position;

    Status := MKVEC_ACTIVE;
    FStream.WriteBuffer(Status,  1);
    FStream.WriteBuffer(EntLen,  2); if EntLen   > 0 then FStream.WriteBuffer(EntB[0],   EntLen);
    FStream.WriteBuffer(IDLen,   2); if IDLen    > 0 then FStream.WriteBuffer(IDB[0],    IDLen);
    FStream.WriteBuffer(ModelLen,2); if ModelLen > 0 then FStream.WriteBuffer(ModelB[0], ModelLen);

    Entry.EmbOffset := FStream.Position;
    FStream.WriteBuffer(EmbData[0], FDim * SizeOf(Double));

    Entry.TextOffset := FStream.Position;
    FStream.WriteBuffer(TextLen,  4); if TextLen  > 0 then FStream.WriteBuffer(TextB[0],  TextLen);
    FStream.WriteBuffer(PropsLen, 4); if PropsLen > 0 then FStream.WriteBuffer(PropsB[0], PropsLen);

    // Actualizar índice de offsets
    FEntryIndex.AddOrSetValue(CK, Entry);

    // Actualizar lista de IDs por entidad
    if not FEntidadIDs.TryGetValue(LEnt, IDList) then
    begin
      IDList := TList<string>.Create;
      FEntidadIDs.Add(LEnt, IDList);
    end;
    // Agregar solo si no estaba (puede quedar tras el borrado anterior)
    if IDList.IndexOf(LID) < 0 then
      IDList.Add(LID);

    // Crear stub para BM25 (sin embedding)
    Stub := TAiEmbeddingNode.Create(0);
    Stub.Tag   := LID;
    Stub.Text  := ANode.Text;
    Stub.Model := LModel;
    var JJ := ANode.MetaData.ToJSON;
    try Stub.MetaData.FromJSON(JJ); finally JJ.Free; end;
    FStubNodes.AddOrSetValue(CK, Stub);

    // Agregar stub al índice BM25
    BM25 := GetOrCreateBm25(LEnt);
    BM25.AddNode(Stub);

  finally
    FLock.Leave;
  end;
end;

procedure TAiMkVecDriver.Delete(const AID: string; const AEntidad: string);
var
  LEnt, CK: string;
  Entry: TNodeFileEntry;
  Status: Byte;
  IDList: TList<string>;
  I: Integer;
begin
  EnsureOpen;
  LEnt := IfThen(AEntidad = '', 'DEFAULT', AEntidad);
  CK   := CompKey(LEnt, AID);

  FLock.Enter;
  try
    if not FEntryIndex.TryGetValue(CK, Entry) then Exit;

    // Marcar en archivo
    Status := MKVEC_DELETED;
    FStream.Position := Entry.RecordOffset;
    FStream.WriteBuffer(Status, 1);

    // Limpiar índices
    FEntryIndex.Remove(CK);
    FStubNodes.Remove(CK);

    if FEntidadIDs.TryGetValue(LEnt, IDList) then
      for I := IDList.Count - 1 downto 0 do
        if IDList[I] = AID then begin IDList.Delete(I); Break; end;

    // Reconstruir BM25 solo para esta entidad (en memoria, rápido)
    RebuildBm25(LEnt);
  finally
    FLock.Leave;
  end;
end;

procedure TAiMkVecDriver.Clear(const AEntidad: string);
var
  LEnt: string;
  IDList: TList<string>;
  Entry: TNodeFileEntry;
  Status: Byte;
  I: Integer;
  CK: string;
begin
  EnsureOpen;
  LEnt := IfThen(AEntidad = '', 'DEFAULT', AEntidad);

  FLock.Enter;
  try
    if not FEntidadIDs.TryGetValue(LEnt, IDList) then Exit;

    Status := MKVEC_DELETED;
    for I := 0 to IDList.Count - 1 do
    begin
      CK := CompKey(LEnt, IDList[I]);
      if FEntryIndex.TryGetValue(CK, Entry) then
      begin
        FStream.Position := Entry.RecordOffset;
        FStream.WriteBuffer(Status, 1);
      end;
      FEntryIndex.Remove(CK);
      FStubNodes.Remove(CK);
    end;
    IDList.Clear;

    // Limpiar BM25 de la entidad
    if FBm25Indices.ContainsKey(LEnt) then
      FBm25Indices[LEnt].Clear;
  finally
    FLock.Leave;
  end;
end;

// =============================================================================
// Búsqueda vectorial (embeddings desde disco)
// =============================================================================

function TAiMkVecDriver.CosineSim(const A, B: TAiEmbeddingData): Double;
var
  Dot, NA, NB: Double;
  I: Integer;
begin
  Dot := 0; NA := 0; NB := 0;
  if Length(A) <> Length(B) then Exit(0.0);
  for I := 0 to High(A) do
  begin
    Dot := Dot + A[I] * B[I];
    NA  := NA  + A[I] * A[I];
    NB  := NB  + B[I] * B[I];
  end;
  NA := Sqrt(NA); NB := Sqrt(NB);
  if (NA < 1e-12) or (NB < 1e-12) then Exit(0.0);
  Result := Dot / (NA * NB);
end;

function TAiMkVecDriver.VectorSearch(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string; ALimit: Integer;
  AMinScore: Double; AFilter: TAiFilterCriteria): TAiRAGVector;
type
  TCandidate = record Score: Double; ID: string; end;
var
  IDList: TList<string>;
  Candidates: TList<TCandidate>;
  Emb: TAiEmbeddingData;
  Score: Double;
  Cand: TCandidate;
  Entry: TNodeFileEntry;
  Node: TAiEmbeddingNode;
  CK: string;
  I: Integer;
  Stub: TAiEmbeddingNode;
begin
  Result := TAiRAGVector.Create(nil, True);
  if not FEntidadIDs.TryGetValue(AEntidad, IDList) or (IDList.Count = 0) then Exit;

  Candidates := TList<TCandidate>.Create;
  try
    // FASE 1: Scan de embeddings (solo Dim×8 bytes por nodo)
    for I := 0 to IDList.Count - 1 do
    begin
      CK := CompKey(AEntidad, IDList[I]);
      if not FEntryIndex.TryGetValue(CK, Entry) then Continue;

      // Aplicar filtro usando el stub (que tiene la metadata en memoria)
      if Assigned(AFilter) and (AFilter.Count > 0) then
      begin
        if FStubNodes.TryGetValue(CK, Stub) then
          if not Stub.MetaData.Matches(AFilter) then Continue;
      end;

      Emb := ReadEmbedding(Entry.EmbOffset);
      Score := CosineSim(ATarget.Data, Emb);

      if (AMinScore > 0) and (Score < AMinScore) then Continue;

      Cand.Score := Score;
      Cand.ID    := IDList[I];

      if Candidates.Count < ALimit then
      begin
        Candidates.Add(Cand);
        Candidates.Sort(TComparer<TCandidate>.Construct(
          function(const A, B: TCandidate): Integer
          begin Result := IfThen(A.Score > B.Score, -1, IfThen(A.Score < B.Score, 1, 0)); end));
      end
      else if Score > Candidates.Last.Score then
      begin
        Candidates[Candidates.Count - 1] := Cand;
        Candidates.Sort(TComparer<TCandidate>.Construct(
          function(const A, B: TCandidate): Integer
          begin Result := IfThen(A.Score > B.Score, -1, IfThen(A.Score < B.Score, 1, 0)); end));
      end;
    end;

    // FASE 2: Cargar texto completo solo para el Top-K
    for I := 0 to Candidates.Count - 1 do
    begin
      CK := CompKey(AEntidad, Candidates[I].ID);
      if not FEntryIndex.TryGetValue(CK, Entry) then Continue;
      Node := LoadFullNode(Entry);
      Node.Tag := Candidates[I].ID;
      Node.Idx := Candidates[I].Score;
      // Recuperar Model del stub
      if FStubNodes.TryGetValue(CK, Stub) then
        Node.Model := Stub.Model;
      Result.Items.Add(Node);
    end;

  finally
    Candidates.Free;
  end;
end;

// =============================================================================
// Búsqueda léxical BM25 (completamente en memoria, stubs)
// =============================================================================

function TAiMkVecDriver.LexicalSearch(
  const AQuery: string;
  const AEntidad: string; ALimit: Integer;
  AMinScore: Double; AFilter: TAiFilterCriteria): TAiRAGVector;
var
  BM25: TAIBm25Index;
  BM25Results: TList<TPair<Double, TAiEmbeddingNode>>;
  Pair: TPair<Double, TAiEmbeddingNode>;
  MaxScore: Double;
  NormScore: Double;
  Entry: TNodeFileEntry;
  Node: TAiEmbeddingNode;
  CK: string;
begin
  Result := TAiRAGVector.Create(nil, True);
  if not FBm25Indices.TryGetValue(AEntidad, BM25) then Exit;
  if Trim(AQuery) = '' then Exit;

  BM25Results := BM25.Search(AQuery, ALimit * 2, AFilter);
  try
    if BM25Results.Count = 0 then Exit;

    // Normalizar scores BM25 a [0, 1] usando el máximo del conjunto
    MaxScore := BM25Results[0].Key; // ya está ordenado desc
    if MaxScore < 1e-12 then Exit;

    for Pair in BM25Results do
    begin
      NormScore := Pair.Key / MaxScore;
      if (AMinScore > 0) and (NormScore < AMinScore) then Continue;
      if Result.Items.Count >= ALimit then Break;

      // Cargar nodo completo desde disco (usando Tag del stub)
      CK := CompKey(AEntidad, Pair.Value.Tag);
      if not FEntryIndex.TryGetValue(CK, Entry) then Continue;

      Node := LoadFullNode(Entry);
      Node.Tag   := Pair.Value.Tag;
      Node.Model := Pair.Value.Model;
      Node.Idx   := NormScore;
      Result.Items.Add(Node);
    end;
  finally
    BM25Results.Free;
  end;
end;

// =============================================================================
// Fusión RRF y ponderada
// =============================================================================

function TAiMkVecDriver.FuseRRF(AVec, ALex: TAiRAGVector; ALimit: Integer): TAiRAGVector;
const
  K = 60;
var
  Scores: TDictionary<string, Double>;
  NodeMap: TDictionary<string, TAiEmbeddingNode>;
  Pairs: TList<TPair<string, Double>>;
  I: Integer;
  Tag: string;
begin
  Result  := TAiRAGVector.Create(nil, True);
  Scores  := TDictionary<string, Double>.Create;
  NodeMap := TDictionary<string, TAiEmbeddingNode>.Create;
  Pairs   := TList<TPair<string, Double>>.Create;
  try
    for I := 0 to AVec.Items.Count - 1 do
    begin
      Tag := AVec.Items[I].Tag;
      var RRF := 1.0 / (K + I + 1);
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + RRF
      else begin Scores.Add(Tag, RRF); NodeMap.Add(Tag, AVec.Items[I]); end;
    end;
    for I := 0 to ALex.Items.Count - 1 do
    begin
      Tag := ALex.Items[I].Tag;
      var RRF := 1.0 / (K + I + 1);
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + RRF
      else begin Scores.Add(Tag, RRF); NodeMap.Add(Tag, ALex.Items[I]); end;
    end;
    for var KV in Scores do
      Pairs.Add(TPair<string, Double>.Create(KV.Key, KV.Value));
    Pairs.Sort(TComparer<TPair<string, Double>>.Construct(
      function(const A, B: TPair<string, Double>): Integer
      begin Result := IfThen(A.Value > B.Value, -1, IfThen(A.Value < B.Value, 1, 0)); end));
    for I := 0 to Min(ALimit - 1, Pairs.Count - 1) do
    begin
      if not NodeMap.ContainsKey(Pairs[I].Key) then Continue;
      var N := CopyNode(NodeMap[Pairs[I].Key]);
      N.Idx := Pairs[I].Value;
      Result.Items.Add(N);
    end;
  finally
    Scores.Free; NodeMap.Free; Pairs.Free;
  end;
end;

function TAiMkVecDriver.FuseWeighted(AVec, ALex: TAiRAGVector;
  AVW, ALW: Double; ALimit: Integer): TAiRAGVector;
var
  Scores: TDictionary<string, Double>;
  NodeMap: TDictionary<string, TAiEmbeddingNode>;
  Pairs: TList<TPair<string, Double>>;
  I: Integer;
  Tag: string;
begin
  Result  := TAiRAGVector.Create(nil, True);
  Scores  := TDictionary<string, Double>.Create;
  NodeMap := TDictionary<string, TAiEmbeddingNode>.Create;
  Pairs   := TList<TPair<string, Double>>.Create;
  try
    for I := 0 to AVec.Items.Count - 1 do
    begin
      Tag := AVec.Items[I].Tag;
      var S := AVec.Items[I].Idx * AVW;
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + S
      else begin Scores.Add(Tag, S); NodeMap.Add(Tag, AVec.Items[I]); end;
    end;
    for I := 0 to ALex.Items.Count - 1 do
    begin
      Tag := ALex.Items[I].Tag;
      var S := ALex.Items[I].Idx * ALW;
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + S
      else begin Scores.Add(Tag, S); NodeMap.Add(Tag, ALex.Items[I]); end;
    end;
    for var KV in Scores do
      Pairs.Add(TPair<string, Double>.Create(KV.Key, KV.Value));
    Pairs.Sort(TComparer<TPair<string, Double>>.Construct(
      function(const A, B: TPair<string, Double>): Integer
      begin Result := IfThen(A.Value > B.Value, -1, IfThen(A.Value < B.Value, 1, 0)); end));
    for I := 0 to Min(ALimit - 1, Pairs.Count - 1) do
    begin
      if not NodeMap.ContainsKey(Pairs[I].Key) then Continue;
      var N := CopyNode(NodeMap[Pairs[I].Key]);
      N.Idx := Pairs[I].Value;
      Result.Items.Add(N);
    end;
  finally
    Scores.Free; NodeMap.Free; Pairs.Free;
  end;
end;

procedure TAiMkVecDriver.TransferFiltered(Src, Dst: TAiRAGVector;
  ALimit: Integer; AMinScore: Double);
var
  I: Integer;
  N: TAiEmbeddingNode;
begin
  for I := 0 to Src.Items.Count - 1 do
  begin
    if Dst.Items.Count >= ALimit then Break;
    if (AMinScore > 0) and (Src.Items[I].Idx < AMinScore) then Continue;
    N := CopyNode(Src.Items[I]);
    Dst.Items.Add(N);
  end;
end;

// =============================================================================
// Search — orquestador principal (mismo contrato que Postgres/SQLite)
// =============================================================================

function TAiMkVecDriver.Search(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  APrecision: Double;
  AFilter: TAiFilterCriteria;
  Options: TAiSearchOptions): TAiRAGVector;
var
  LEnt: string;
  LOptions: TAiSearchOptions;
  DoVector, DoLexical: Boolean;
  MinVec, MinLex, VW, LW: Double;
  VecRes, LexRes, Fused: TAiRAGVector;
begin
  Result := TAiRAGVector.Create(nil, True);
  EnsureOpen;

  LEnt := IfThen(AEntidad = '', 'DEFAULT', AEntidad);

  LOptions := Options;
  if not Assigned(LOptions) and Assigned(Owner) and (Owner is TAiRAGVector) then
    LOptions := TAiRAGVector(Owner).SearchOptions;

  DoVector  := (Length(ATarget.Data) > 0) and ((LOptions = nil) or LOptions.UseEmbeddings);
  DoLexical := Assigned(LOptions) and LOptions.UseBM25 and (Trim(ATarget.Text) <> '');

  if not (DoVector or DoLexical) then Exit;

  MinVec := IfThen(Assigned(LOptions), LOptions.MinAbsoluteScoreEmbedding, 0.0);
  MinLex := IfThen(Assigned(LOptions), LOptions.MinAbsoluteScoreBM25,      0.0);

  VecRes := nil;
  LexRes := nil;
  FLock.Enter;
  try
    if DoVector then
      VecRes := VectorSearch(ATarget, LEnt, ALimit * 3, MinVec, AFilter);

    if DoLexical then
      LexRes := LexicalSearch(ATarget.Text, LEnt, ALimit * 3, MinLex, AFilter);

    if DoVector and DoLexical then
    begin
      if Assigned(LOptions) and LOptions.UseRRF then
        Fused := FuseRRF(VecRes, LexRes, ALimit)
      else
      begin
        VW := IfThen(Assigned(LOptions), LOptions.EmbeddingWeight, 0.7);
        LW := IfThen(Assigned(LOptions), LOptions.BM25Weight,      0.3);
        Fused := FuseWeighted(VecRes, LexRes, VW, LW, ALimit);
      end;
      try
        TransferFiltered(Fused, Result, ALimit, APrecision);
      finally
        Fused.Free;
      end;
    end
    else if DoVector and Assigned(VecRes) then
      TransferFiltered(VecRes, Result, ALimit, APrecision)
    else if DoLexical and Assigned(LexRes) then
      TransferFiltered(LexRes, Result, ALimit, APrecision);

  finally
    FLock.Leave;
    VecRes.Free;
    LexRes.Free;
  end;
end;

// =============================================================================
// Compact — reescribe el archivo sin registros borrados
// =============================================================================

procedure TAiMkVecDriver.Compact;
var
  TempPath: string;
  TempStream: TFileStream;
  Status: Byte;
  EntLen, IDLen, ModelLen: Word;
  TextLen, PropsLen: Cardinal;
  ActiveCount: Int64;
  EntB, IDB, ModB, TxtB, PropB, EmbB: TBytes;

  procedure ReadBytes(var Dest: TBytes; ALen: Integer);
  begin
    SetLength(Dest, ALen);
    if ALen > 0 then FStream.ReadBuffer(Dest[0], ALen);
  end;

begin
  EnsureOpen;
  TempPath := FFilePath + '.tmp';

  FLock.Enter;
  try
    FlushNodeCount;
    TempStream := TFileStream.Create(TempPath, fmCreate);
    try
      // Header provisional
      var Hdr: array[0..MKVEC_HEADER_SIZE-1] of Byte;
      FillChar(Hdr, MKVEC_HEADER_SIZE, 0);
      Hdr[0] := MKVEC_MAGIC_0; Hdr[1] := MKVEC_MAGIC_1;
      Hdr[2] := MKVEC_MAGIC_2; Hdr[3] := MKVEC_MAGIC_3;
      Hdr[4] := MKVEC_VERSION;
      var DC: Cardinal := FDim;
      Move(DC, Hdr[6], 4);
      TempStream.WriteBuffer(Hdr, MKVEC_HEADER_SIZE);

      FStream.Position := MKVEC_HEADER_SIZE;
      ActiveCount := 0;

      while FStream.Position < FStream.Size do
      begin
        FStream.ReadBuffer(Status, 1);

        FStream.ReadBuffer(EntLen,   2); ReadBytes(EntB,   EntLen);
        FStream.ReadBuffer(IDLen,    2); ReadBytes(IDB,    IDLen);
        FStream.ReadBuffer(ModelLen, 2); ReadBytes(ModB,   ModelLen);
        ReadBytes(EmbB, FDim * SizeOf(Double));
        FStream.ReadBuffer(TextLen,  4); ReadBytes(TxtB,  TextLen);
        FStream.ReadBuffer(PropsLen, 4); ReadBytes(PropB, PropsLen);

        if Status <> MKVEC_ACTIVE then Continue;

        TempStream.WriteBuffer(Status,   1);
        TempStream.WriteBuffer(EntLen,   2); if EntLen   > 0 then TempStream.WriteBuffer(EntB[0],  EntLen);
        TempStream.WriteBuffer(IDLen,    2); if IDLen    > 0 then TempStream.WriteBuffer(IDB[0],   IDLen);
        TempStream.WriteBuffer(ModelLen, 2); if ModelLen > 0 then TempStream.WriteBuffer(ModB[0],  ModelLen);
        TempStream.WriteBuffer(EmbB[0], FDim * SizeOf(Double));
        TempStream.WriteBuffer(TextLen,  4); if TextLen  > 0 then TempStream.WriteBuffer(TxtB[0], TextLen);
        TempStream.WriteBuffer(PropsLen, 4); if PropsLen > 0 then TempStream.WriteBuffer(PropB[0],PropsLen);
        Inc(ActiveCount);
      end;

      // Actualizar NodeCount en header del temp
      TempStream.Position := 10;
      TempStream.WriteBuffer(ActiveCount, 8);
    finally
      TempStream.Free;
    end;

    // Reemplazar archivo original
    FStream.Free; FStream := nil;
    DeleteFile(FFilePath);
    RenameFile(TempPath, FFilePath);

    // Reabrir y reconstruir índices
    FEntryIndex.Clear;
    FEntidadIDs.Clear;
    FStubNodes.Clear;
    FBm25Indices.Clear;

    FStream := TFileStream.Create(FFilePath, fmOpenReadWrite or fmShareDenyWrite);
    FStream.Seek(18, soFromBeginning); // saltar Magic+Version+Dim+NodeCount
    FStream.Position := MKVEC_HEADER_SIZE;
    RebuildAllIndices;
  finally
    FLock.Leave;
  end;
end;

// =============================================================================
// Estadísticas
// =============================================================================

function TAiMkVecDriver.NodeCount(const AEntidad: string): Int64;
var
  IDList: TList<string>;
  Entidad: string;
begin
  Result := 0;
  if AEntidad <> '' then
  begin
    if FEntidadIDs.TryGetValue(AEntidad, IDList) then
      Result := IDList.Count;
  end
  else
    for Entidad in FEntidadIDs.Keys do
    begin
      if FEntidadIDs.TryGetValue(Entidad, IDList) then
        Inc(Result, IDList.Count);
    end;
end;

function TAiMkVecDriver.EntidadList: TArray<string>;
begin
  Result := FEntidadIDs.Keys.ToArray;
end;

end.
