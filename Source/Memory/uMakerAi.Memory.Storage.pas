// MIT License
//
// Copyright (c) 2024 Gustavo Enríquez - CimaMaker
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Memory.Storage;

// Capa de almacenamiento para TAiMemory.
//
// IAiMemoryStorage  — contrato abstracto (permite futuros drivers: Postgres, etc.)
// TAiMemorySQLiteStorage — implementación SQLite vía FireDAC, mismo patrón que
//                          TAiRAGVectorSQLiteDriver.
//
// Schema SQLite (tabla ai_memories + ai_memories_fts):
//   - FTS5 para búsqueda léxica rápida
//   - Triggers para mantener FTS5 sincronizado
//   - UNIQUE(content_hash, namespace) para deduplicación automática
//   - json_extract() para filtrado por metadata

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, System.DateUtils, System.Hash, System.Math, System.StrUtils,
  Data.DB,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DApt,
  uMakerAi.Memory.Types;

type
  // ---------------------------------------------------------------------------
  // Interfaz abstracta de storage — cualquier driver debe implementarla
  // ---------------------------------------------------------------------------
  IAiMemoryStorage = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    // Guarda un entry. Actualiza si content_hash+namespace ya existe.
    // Retorna el Id asignado.
    function  StoreEntry(AEntry: TMemoryEntry): Integer;

    // Búsqueda léxica FTS5
    function  SearchFTS(const AQuery, ANamespace: string; ALimit: Integer): TMemoryEntryList;

    // Búsqueda por similitud coseno sobre embeddings almacenados
    function  SearchSemantic(const AEmbedding: TArray<Double>;
                              const ANamespace: string;
                              ALimit: Integer;
                              AMinScore: Double = 0.5): TMemoryEntryList;

    // Recupera entradas de alta importancia
    function  Recall(AMinImportance: Integer; const ANamespace: string;
                     ALimit: Integer): TMemoryEntryList;

    function  GetById(AId: Integer): TMemoryEntry;
    procedure DeleteById(AId: Integer);
    procedure UpdateAccessStats(AId: Integer);
    procedure UpdateDecayScore(AId: Integer; AScore: Double);
    procedure UpdateContent(AId: Integer; const AContent: string;
                            AImportance: Integer);

    function  Stats(const ANamespace: string): TMemoryStats;
    procedure CleanupExpired(const ANamespace: string);

    // Knowledge graph básico sobre la misma DB
    procedure LinkEntries(AFromId, AToId: Integer; const ARelation: string);
    procedure UnlinkEntries(AFromId, AToId: Integer);
    function  GetLinks(AId: Integer; const ANamespace: string): TMemoryEntryList;

    function  ExportAll(const ANamespace: string): TMemoryEntryList;
  end;

  // ---------------------------------------------------------------------------
  // Implementación SQLite con FireDAC
  // ---------------------------------------------------------------------------
  TAiMemorySQLiteStorage = class(TInterfacedObject, IAiMemoryStorage)
  private
    FConn:      TFDConnection;
    FOwnsConn:  Boolean;
    FTableName: string;

    function  NewQuery: TFDQuery;
    procedure EnsureConnected;
    procedure CreateSchema;
    function  ComputeHash(const AContent, ANamespace: string): string;
    function  RowToEntry(ADS: TDataSet): TMemoryEntry;
    function  EmbeddingToStr(const AEmb: TArray<Double>): string;
    function  StrToEmbedding(const AStr: string): TArray<Double>;
    function  CosineSimilarity(const A, B: TArray<Double>): Double;
  public
    // Crea su propia conexión SQLite en ADbPath
    constructor CreateWithPath(const ADbPath: string);
    // Reutiliza una conexión FireDAC existente (no la libera)
    constructor CreateWithConnection(AConn: TFDConnection);
    destructor Destroy; override;

    // IAiMemoryStorage
    function  StoreEntry(AEntry: TMemoryEntry): Integer;
    function  SearchFTS(const AQuery, ANamespace: string; ALimit: Integer): TMemoryEntryList;
    function  SearchSemantic(const AEmbedding: TArray<Double>;
                              const ANamespace: string;
                              ALimit: Integer;
                              AMinScore: Double = 0.5): TMemoryEntryList;
    function  Recall(AMinImportance: Integer; const ANamespace: string;
                     ALimit: Integer): TMemoryEntryList;
    function  GetById(AId: Integer): TMemoryEntry;
    procedure DeleteById(AId: Integer);
    procedure UpdateAccessStats(AId: Integer);
    procedure UpdateDecayScore(AId: Integer; AScore: Double);
    procedure UpdateContent(AId: Integer; const AContent: string;
                            AImportance: Integer);
    function  Stats(const ANamespace: string): TMemoryStats;
    procedure CleanupExpired(const ANamespace: string);
    procedure LinkEntries(AFromId, AToId: Integer; const ARelation: string);
    procedure UnlinkEntries(AFromId, AToId: Integer);
    function  GetLinks(AId: Integer; const ANamespace: string): TMemoryEntryList;
    function  ExportAll(const ANamespace: string): TMemoryEntryList;

    property TableName: string read FTableName write FTableName;
  end;

implementation

// ---------------------------------------------------------------------------
// Tipos auxiliares de implementación
// ---------------------------------------------------------------------------

type
  TScoredEntry = record
    Entry: TMemoryEntry;
    Score: Double;
  end;

// ---------------------------------------------------------------------------
// TAiMemorySQLiteStorage
// ---------------------------------------------------------------------------

constructor TAiMemorySQLiteStorage.CreateWithPath(const ADbPath: string);
begin
  inherited Create;
  FTableName := 'ai_memories';
  FOwnsConn  := True;
  FConn      := TFDConnection.Create(nil);
  FConn.DriverName            := 'SQLite';
  FConn.Params.Values['Database'] := ADbPath;
  FConn.Params.Values['LockingMode'] := 'Normal';
  FConn.Params.Values['Synchronous'] := 'Normal';
  FConn.Params.Values['JournalMode']  := 'WAL';
  FConn.LoginPrompt := False;
  FConn.Connected   := True;
  CreateSchema;
end;

constructor TAiMemorySQLiteStorage.CreateWithConnection(AConn: TFDConnection);
begin
  inherited Create;
  FTableName := 'ai_memories';
  FOwnsConn  := False;
  FConn      := AConn;
  EnsureConnected;
  CreateSchema;
end;

destructor TAiMemorySQLiteStorage.Destroy;
begin
  if FOwnsConn then
    FConn.Free;
  inherited;
end;

procedure TAiMemorySQLiteStorage.EnsureConnected;
begin
  if not FConn.Connected then
    FConn.Connected := True;
end;

function TAiMemorySQLiteStorage.NewQuery: TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := FConn;
end;

procedure TAiMemorySQLiteStorage.CreateSchema;
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    // WAL mode para concurrencia
    Q.SQL.Text := 'PRAGMA journal_mode=WAL';
    Q.ExecSQL;

    // Tabla principal
    Q.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS ' + FTableName + ' (' +
      '  id           INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  content      TEXT    NOT NULL,' +
      '  memory_type  TEXT    NOT NULL DEFAULT ''fact'',' +
      '  importance   INTEGER NOT NULL DEFAULT 5,' +
      '  namespace    TEXT    NOT NULL DEFAULT ''default'',' +
      '  tags         TEXT,' +
      '  metadata     TEXT    DEFAULT ''{}'',' +
      '  content_hash TEXT    NOT NULL,' +
      '  embedding    TEXT,' +
      '  decay_score  REAL    NOT NULL DEFAULT 1.0,' +
      '  created_at   TEXT    NOT NULL,' +
      '  accessed_at  TEXT    NOT NULL,' +
      '  access_count INTEGER NOT NULL DEFAULT 0,' +
      '  expires_at   TEXT,' +
      '  UNIQUE(content_hash, namespace)' +
      ')';
    Q.ExecSQL;

    // Índices
    Q.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_mem_ns_imp ON ' +
      FTableName + ' (namespace, importance DESC)';
    Q.ExecSQL;
    Q.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_mem_ns_decay ON ' +
      FTableName + ' (namespace, decay_score DESC)';
    Q.ExecSQL;
    Q.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_mem_expires ON ' +
      FTableName + ' (expires_at) WHERE expires_at IS NOT NULL';
    Q.ExecSQL;

    // FTS5 virtual table
    Q.SQL.Text :=
      'CREATE VIRTUAL TABLE IF NOT EXISTS ' + FTableName + '_fts USING fts5(' +
      '  content, tags,' +
      '  content=''' + FTableName + ''',' +
      '  content_rowid=''id'',' +
      '  tokenize=''unicode61 remove_diacritics 1''' +
      ')';
    Q.ExecSQL;

    // Triggers para sincronizar FTS5
    Q.SQL.Text :=
      'CREATE TRIGGER IF NOT EXISTS ' + FTableName + '_ai AFTER INSERT ON ' + FTableName + ' BEGIN' +
      '  INSERT INTO ' + FTableName + '_fts(rowid, content, tags) VALUES (new.id, new.content, new.tags);' +
      'END';
    Q.ExecSQL;

    Q.SQL.Text :=
      'CREATE TRIGGER IF NOT EXISTS ' + FTableName + '_ad AFTER DELETE ON ' + FTableName + ' BEGIN' +
      '  INSERT INTO ' + FTableName + '_fts(' + FTableName + '_fts, rowid, content, tags) VALUES(''delete'', old.id, old.content, old.tags);' +
      'END';
    Q.ExecSQL;

    Q.SQL.Text :=
      'CREATE TRIGGER IF NOT EXISTS ' + FTableName + '_au AFTER UPDATE ON ' + FTableName + ' BEGIN' +
      '  INSERT INTO ' + FTableName + '_fts(' + FTableName + '_fts, rowid, content, tags) VALUES(''delete'', old.id, old.content, old.tags);' +
      '  INSERT INTO ' + FTableName + '_fts(rowid, content, tags) VALUES (new.id, new.content, new.tags);' +
      'END';
    Q.ExecSQL;

    // Tabla de links para memory graph
    Q.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS ' + FTableName + '_links (' +
      '  from_id  INTEGER NOT NULL,' +
      '  to_id    INTEGER NOT NULL,' +
      '  relation TEXT    NOT NULL DEFAULT ''related'',' +
      '  PRIMARY KEY(from_id, to_id)' +
      ')';
    Q.ExecSQL;

  finally
    Q.Free;
  end;
end;

function TAiMemorySQLiteStorage.ComputeHash(const AContent, ANamespace: string): string;
begin
  Result := Copy(THashSHA2.GetHashString(AContent + '|' + ANamespace), 1, 16);
end;

function TAiMemorySQLiteStorage.EmbeddingToStr(const AEmb: TArray<Double>): string;
var
  Parts: TArray<string>;
  I: Integer;
begin
  if Length(AEmb) = 0 then Exit('');
  SetLength(Parts, Length(AEmb));
  for I := 0 to High(AEmb) do
    Parts[I] := FloatToStr(AEmb[I]);
  Result := '[' + String.Join(',', Parts) + ']';
end;

function TAiMemorySQLiteStorage.StrToEmbedding(const AStr: string): TArray<Double>;
var
  S:     string;
  Parts: TArray<string>;
  I:     Integer;
begin
  Result := nil;
  S := Trim(AStr);
  if (Length(S) < 2) or (S[1] <> '[') then Exit;
  S      := Copy(S, 2, Length(S) - 2);
  Parts  := S.Split([',']);
  SetLength(Result, Length(Parts));
  for I := 0 to High(Parts) do
    Result[I] := StrToFloatDef(Trim(Parts[I]), 0.0);
end;

function TAiMemorySQLiteStorage.CosineSimilarity(const A, B: TArray<Double>): Double;
var
  Dot, MagA, MagB: Double;
  I, Len: Integer;
begin
  Result := 0;
  Len := Min(Length(A), Length(B));
  if Len = 0 then Exit;
  Dot := 0; MagA := 0; MagB := 0;
  for I := 0 to Len - 1 do
  begin
    Dot  := Dot  + A[I] * B[I];
    MagA := MagA + A[I] * A[I];
    MagB := MagB + B[I] * B[I];
  end;
  if (MagA = 0) or (MagB = 0) then Exit;
  Result := Dot / (Sqrt(MagA) * Sqrt(MagB));
end;

function TAiMemorySQLiteStorage.RowToEntry(ADS: TDataSet): TMemoryEntry;
var
  TagStr: string;
begin
  Result              := TMemoryEntry.Create;
  Result.Id           := ADS.FieldByName('id').AsInteger;
  Result.Content      := ADS.FieldByName('content').AsString;
  Result.MemoryType   := StrToMemoryType(ADS.FieldByName('memory_type').AsString);
  Result.Importance   := ADS.FieldByName('importance').AsInteger;
  Result.Namespace    := ADS.FieldByName('namespace').AsString;
  Result.ContentHash  := ADS.FieldByName('content_hash').AsString;
  Result.DecayScore   := ADS.FieldByName('decay_score').AsFloat;
  Result.AccessCount  := ADS.FieldByName('access_count').AsInteger;
  Result.Metadata     := ADS.FieldByName('metadata').AsString;
  if Result.Metadata = '' then Result.Metadata := '{}';

  var CreatedStr  := ADS.FieldByName('created_at').AsString;
  var AccessedStr := ADS.FieldByName('accessed_at').AsString;
  var ExpiresStr  := ADS.FieldByName('expires_at').AsString;

  if CreatedStr  <> '' then Result.CreatedAt  := ISO8601ToDate(CreatedStr, False);
  if AccessedStr <> '' then Result.AccessedAt := ISO8601ToDate(AccessedStr, False);
  if ExpiresStr  <> '' then Result.ExpiresAt  := ISO8601ToDate(ExpiresStr, False);

  TagStr := ADS.FieldByName('tags').AsString;
  if TagStr <> '' then
    Result.Tags := TagStr.Split([',']);

  var EmbStr := ADS.FieldByName('embedding').AsString;
  if EmbStr <> '' then
    Result.Embedding := StrToEmbedding(EmbStr);
end;

// ---------------------------------------------------------------------------
// StoreEntry — INSERT OR REPLACE con dedup por content_hash+namespace
// ---------------------------------------------------------------------------

function TAiMemorySQLiteStorage.StoreEntry(AEntry: TMemoryEntry): Integer;
var
  Q: TFDQuery;
begin
  AEntry.ContentHash := ComputeHash(AEntry.Content, AEntry.Namespace);
  AEntry.CreatedAt   := Now;
  AEntry.AccessedAt  := Now;

  Q := NewQuery;
  try
    Q.SQL.Text :=
      'INSERT INTO ' + FTableName +
      ' (content, memory_type, importance, namespace, tags, metadata,' +
      '  content_hash, embedding, decay_score, created_at, accessed_at,' +
      '  access_count, expires_at)' +
      ' VALUES (:content, :memory_type, :importance, :namespace, :tags, :metadata,' +
      '  :content_hash, :embedding, :decay_score, :created_at, :accessed_at,' +
      '  :access_count, :expires_at)' +
      ' ON CONFLICT(content_hash, namespace) DO UPDATE SET' +
      '  content=excluded.content,' +
      '  importance=excluded.importance,' +
      '  memory_type=excluded.memory_type,' +
      '  tags=excluded.tags,' +
      '  metadata=excluded.metadata,' +
      '  embedding=excluded.embedding,' +
      '  accessed_at=excluded.accessed_at';

    Q.ParamByName('content').AsString      := AEntry.Content;
    Q.ParamByName('memory_type').AsString  := MemoryTypeToStr(AEntry.MemoryType);
    Q.ParamByName('importance').AsInteger  := AEntry.Importance;
    Q.ParamByName('namespace').AsString    := AEntry.Namespace;
    Q.ParamByName('tags').AsString         := AEntry.TagsAsString;
    Q.ParamByName('metadata').AsString     := AEntry.Metadata;
    Q.ParamByName('content_hash').AsString := AEntry.ContentHash;
    Q.ParamByName('decay_score').AsFloat   := AEntry.DecayScore;
    Q.ParamByName('created_at').AsString   := DateToISO8601(AEntry.CreatedAt, False);
    Q.ParamByName('accessed_at').AsString  := DateToISO8601(AEntry.AccessedAt, False);
    Q.ParamByName('access_count').AsInteger:= AEntry.AccessCount;

    if Length(AEntry.Embedding) > 0 then
      Q.ParamByName('embedding').AsString  := EmbeddingToStr(AEntry.Embedding)
    else
      Q.ParamByName('embedding').AsString  := '';

    if AEntry.ExpiresAt > 0 then
      Q.ParamByName('expires_at').AsString := DateToISO8601(AEntry.ExpiresAt, False)
    else
      Q.ParamByName('expires_at').AsString := '';

    Q.ExecSQL;

    // Recupera el id asignado (INSERT o el existente por CONFLICT)
    Q.SQL.Text := 'SELECT id FROM ' + FTableName +
      ' WHERE content_hash=:hash AND namespace=:ns';
    Q.ParamByName('hash').AsString := AEntry.ContentHash;
    Q.ParamByName('ns').AsString   := AEntry.Namespace;
    Q.Open;
    Result := Q.Fields[0].AsInteger;
    AEntry.Id := Result;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// SearchFTS
// ---------------------------------------------------------------------------

function TAiMemorySQLiteStorage.SearchFTS(const AQuery, ANamespace: string;
  ALimit: Integer): TMemoryEntryList;
var
  Q:        TFDQuery;
  SafeQ:    string;
  Words:    TArray<string>;
  I:        Integer;
  Parts:    TStringList;
begin
  Result := TMemoryEntryList.Create(True);

  // Sanitizar query para FTS5: quitar caracteres especiales, convertir a OR
  SafeQ := StringReplace(AQuery, '(', ' ', [rfReplaceAll]);
  SafeQ := StringReplace(SafeQ,  ')', ' ', [rfReplaceAll]);
  SafeQ := StringReplace(SafeQ,  '"', ' ', [rfReplaceAll]);
  SafeQ := StringReplace(SafeQ,  '*', ' ', [rfReplaceAll]);
  SafeQ := StringReplace(SafeQ,  '+', ' ', [rfReplaceAll]);
  SafeQ := StringReplace(SafeQ,  '-', ' ', [rfReplaceAll]);
  SafeQ := StringReplace(SafeQ,  '^', ' ', [rfReplaceAll]);
  SafeQ := StringReplace(SafeQ,  '~', ' ', [rfReplaceAll]);
  Words := SafeQ.Trim.Split([' ']);
  Parts := TStringList.Create;
  try
    for I := 0 to High(Words) do
      if Words[I].Trim <> '' then
        Parts.Add('"' + Words[I].Trim + '"');
    if Parts.Count = 0 then Exit;
    SafeQ := String.Join(' OR ', Parts.ToStringArray);
  finally
    Parts.Free;
  end;

  Q := NewQuery;
  try
    Q.SQL.Text :=
      'SELECT m.* FROM ' + FTableName + ' m' +
      ' JOIN ' + FTableName + '_fts f ON m.id = f.rowid' +
      ' WHERE f.' + FTableName + '_fts MATCH :q' +
      '   AND m.namespace = :ns' +
      ' ORDER BY bm25(f.' + FTableName + '_fts) ASC' +
      ' LIMIT :lim';
    Q.ParamByName('q').AsString    := SafeQ;
    Q.ParamByName('ns').AsString   := ANamespace;
    Q.ParamByName('lim').AsInteger := ALimit;
    Q.Open;
    while not Q.Eof do
    begin
      Result.Add(RowToEntry(Q));
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// SearchSemantic — coseno calculado en Delphi (brute-force para n < 100K)
// ---------------------------------------------------------------------------

function TAiMemorySQLiteStorage.SearchSemantic(const AEmbedding: TArray<Double>;
  const ANamespace: string; ALimit: Integer; AMinScore: Double): TMemoryEntryList;
var
  Q:        TFDQuery;
  Scored:   TArray<TScoredEntry>;
  Count, I: Integer;
  Emb:      TArray<Double>;
  Sim:      Double;
begin
  Result := TMemoryEntryList.Create(False); // no owned — entries en Scored

  Q := NewQuery;
  try
    Q.SQL.Text :=
      'SELECT * FROM ' + FTableName +
      ' WHERE namespace = :ns AND embedding IS NOT NULL AND embedding <> ''''';
    Q.ParamByName('ns').AsString := ANamespace;
    Q.Open;

    Count := 0;
    SetLength(Scored, 0);
    while not Q.Eof do
    begin
      Emb := StrToEmbedding(Q.FieldByName('embedding').AsString);
      Sim := CosineSimilarity(AEmbedding, Emb);
      if Sim >= AMinScore then
      begin
        SetLength(Scored, Count + 1);
        Scored[Count].Entry := RowToEntry(Q);
        Scored[Count].Score := Sim;
        Inc(Count);
      end;
      Q.Next;
    end;
  finally
    Q.Free;
  end;

  // Ordenar por score desc — selection sort (n < 10K, no se justifica complejidad)
  var J: Integer;
  var Tmp: TScoredEntry;
  for I := 0 to Count - 2 do
    for J := I + 1 to Count - 1 do
      if Scored[J].Score > Scored[I].Score then
      begin
        Tmp := Scored[I]; Scored[I] := Scored[J]; Scored[J] := Tmp;
      end;

  for I := 0 to Min(ALimit - 1, Count - 1) do
    Result.Add(Scored[I].Entry);

  // Liberar los entries que no quedaron en Result
  for I := Result.Count to Count - 1 do
    Scored[I].Entry.Free;
end;

// ---------------------------------------------------------------------------
// Recall — recupera memorias de alta importancia
// ---------------------------------------------------------------------------

function TAiMemorySQLiteStorage.Recall(AMinImportance: Integer;
  const ANamespace: string; ALimit: Integer): TMemoryEntryList;
var
  Q: TFDQuery;
begin
  Result := TMemoryEntryList.Create(True);
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'SELECT * FROM ' + FTableName +
      ' WHERE namespace = :ns AND importance >= :imp' +
      ' ORDER BY importance DESC, decay_score DESC' +
      ' LIMIT :lim';
    Q.ParamByName('ns').AsString    := ANamespace;
    Q.ParamByName('imp').AsInteger  := AMinImportance;
    Q.ParamByName('lim').AsInteger  := ALimit;
    Q.Open;
    while not Q.Eof do
    begin
      Result.Add(RowToEntry(Q));
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// GetById
// ---------------------------------------------------------------------------

function TAiMemorySQLiteStorage.GetById(AId: Integer): TMemoryEntry;
var
  Q: TFDQuery;
begin
  Result := nil;
  Q := NewQuery;
  try
    Q.SQL.Text := 'SELECT * FROM ' + FTableName + ' WHERE id = :id';
    Q.ParamByName('id').AsInteger := AId;
    Q.Open;
    if not Q.IsEmpty then
      Result := RowToEntry(Q);
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// DeleteById
// ---------------------------------------------------------------------------

procedure TAiMemorySQLiteStorage.DeleteById(AId: Integer);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE id = :id';
    Q.ParamByName('id').AsInteger := AId;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// UpdateAccessStats — incrementa contador y actualiza accessed_at
// ---------------------------------------------------------------------------

procedure TAiMemorySQLiteStorage.UpdateAccessStats(AId: Integer);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'UPDATE ' + FTableName +
      ' SET access_count = access_count + 1,' +
      '     accessed_at  = :acc' +
      ' WHERE id = :id';
    Q.ParamByName('acc').AsString  := DateToISO8601(Now, False);
    Q.ParamByName('id').AsInteger  := AId;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// UpdateDecayScore
// ---------------------------------------------------------------------------

procedure TAiMemorySQLiteStorage.UpdateDecayScore(AId: Integer; AScore: Double);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'UPDATE ' + FTableName + ' SET decay_score = :score WHERE id = :id';
    Q.ParamByName('score').AsFloat  := AScore;
    Q.ParamByName('id').AsInteger   := AId;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// UpdateContent
// ---------------------------------------------------------------------------

procedure TAiMemorySQLiteStorage.UpdateContent(AId: Integer;
  const AContent: string; AImportance: Integer);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'UPDATE ' + FTableName +
      ' SET content = :content, importance = :imp, accessed_at = :acc' +
      ' WHERE id = :id';
    Q.ParamByName('content').AsString  := AContent;
    Q.ParamByName('imp').AsInteger     := AImportance;
    Q.ParamByName('acc').AsString      := DateToISO8601(Now, False);
    Q.ParamByName('id').AsInteger      := AId;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Stats
// ---------------------------------------------------------------------------

function TAiMemorySQLiteStorage.Stats(const ANamespace: string): TMemoryStats;
var
  Q: TFDQuery;
begin
  Result := Default(TMemoryStats);
  Result.Namespace := ANamespace;

  Q := NewQuery;
  try
    Q.SQL.Text :=
      'SELECT COUNT(*) as total,' +
      '       AVG(importance) as avg_imp,' +
      '       AVG(decay_score) as avg_decay,' +
      '       MIN(created_at) as oldest,' +
      '       MAX(created_at) as newest,' +
      '       SUM(CASE WHEN expires_at IS NOT NULL AND expires_at <> '''' AND expires_at < :now THEN 1 ELSE 0 END) as expired' +
      ' FROM ' + FTableName +
      ' WHERE namespace = :ns';
    Q.ParamByName('now').AsString := DateToISO8601(Now, False);
    Q.ParamByName('ns').AsString  := ANamespace;
    Q.Open;
    if not Q.IsEmpty then
    begin
      Result.TotalCount    := Q.FieldByName('total').AsInteger;
      Result.AvgImportance := Q.FieldByName('avg_imp').AsFloat;
      Result.AvgDecay      := Q.FieldByName('avg_decay').AsFloat;
      Result.ExpiredCount  := Q.FieldByName('expired').AsInteger;
      var OldestStr := Q.FieldByName('oldest').AsString;
      var NewestStr := Q.FieldByName('newest').AsString;
      if OldestStr <> '' then Result.OldestEntry := ISO8601ToDate(OldestStr, False);
      if NewestStr <> '' then Result.NewestEntry := ISO8601ToDate(NewestStr, False);
    end;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// CleanupExpired
// ---------------------------------------------------------------------------

procedure TAiMemorySQLiteStorage.CleanupExpired(const ANamespace: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text :=
      // expires_at = '' significa "sin expiración" (Insert lo escribe así cuando
      // ExpiresAt=0); sin este filtro, '' < :now borraba TODAS las memorias sin TTL.
      'DELETE FROM ' + FTableName +
      ' WHERE namespace = :ns AND expires_at IS NOT NULL AND expires_at <> '''' AND expires_at < :now';
    Q.ParamByName('ns').AsString  := ANamespace;
    Q.ParamByName('now').AsString := DateToISO8601(Now, False);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Graph links
// ---------------------------------------------------------------------------

procedure TAiMemorySQLiteStorage.LinkEntries(AFromId, AToId: Integer;
  const ARelation: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'INSERT OR REPLACE INTO ' + FTableName + '_links (from_id, to_id, relation)' +
      ' VALUES (:from, :to, :rel)';
    Q.ParamByName('from').AsInteger := AFromId;
    Q.ParamByName('to').AsInteger   := AToId;
    Q.ParamByName('rel').AsString   := ARelation;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TAiMemorySQLiteStorage.UnlinkEntries(AFromId, AToId: Integer);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'DELETE FROM ' + FTableName + '_links WHERE from_id=:from AND to_id=:to';
    Q.ParamByName('from').AsInteger := AFromId;
    Q.ParamByName('to').AsInteger   := AToId;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

function TAiMemorySQLiteStorage.GetLinks(AId: Integer;
  const ANamespace: string): TMemoryEntryList;
var
  Q: TFDQuery;
begin
  Result := TMemoryEntryList.Create(True);
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'SELECT m.* FROM ' + FTableName + ' m' +
      ' JOIN ' + FTableName + '_links l ON m.id = l.to_id' +
      ' WHERE l.from_id = :id AND m.namespace = :ns' +
      ' UNION' +
      ' SELECT m.* FROM ' + FTableName + ' m' +
      ' JOIN ' + FTableName + '_links l ON m.id = l.from_id' +
      ' WHERE l.to_id = :id2 AND m.namespace = :ns2';
    Q.ParamByName('id').AsInteger   := AId;
    Q.ParamByName('ns').AsString    := ANamespace;
    Q.ParamByName('id2').AsInteger  := AId;
    Q.ParamByName('ns2').AsString   := ANamespace;
    Q.Open;
    while not Q.Eof do
    begin
      Result.Add(RowToEntry(Q));
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;

// ---------------------------------------------------------------------------
// ExportAll
// ---------------------------------------------------------------------------

function TAiMemorySQLiteStorage.ExportAll(const ANamespace: string): TMemoryEntryList;
var
  Q: TFDQuery;
begin
  Result := TMemoryEntryList.Create(True);
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'SELECT * FROM ' + FTableName + ' WHERE namespace = :ns ORDER BY id';
    Q.ParamByName('ns').AsString := ANamespace;
    Q.Open;
    while not Q.Eof do
    begin
      Result.Add(RowToEntry(Q));
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;

end.
