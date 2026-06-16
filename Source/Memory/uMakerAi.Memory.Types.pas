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

unit uMakerAi.Memory.Types;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
{$IF CompilerVersion < 35}
  uJSONHelper, // Delphi 10.4: helper genérico GetValue<T> de TJSONObject
{$ENDIF}
  System.JSON, System.DateUtils;

type
  // Categoría semántica de la memoria
  TMemoryType = (
    mt_Fact,        // hecho objetivo: "el usuario usa Delphi 13"
    mt_Preference,  // preferencia: "prefiere respuestas cortas"
    mt_Decision,    // decisión tomada: "usamos SQLite como backend"
    mt_ErrorFix,    // corrección aprendida: "ese bug se resuelve con X"
    mt_Pattern,     // patrón detectado: "siempre pregunta sobre RAG primero"
    mt_Workflow,    // procedimiento paso a paso
    mt_Summary,     // resumen de sesión o conversación
    mt_Custom       // uso libre
  );

  TMemorySearchMode = (
    ms_FTS,         // solo full-text search (FTS5)
    ms_Semantic,    // solo búsqueda vectorial por embeddings
    ms_Hybrid       // FTS + semántica con fusión RRF
  );

  // Registro atómico de memoria — equivalente a MemoryEntry de engram-core
  TMemoryEntry = class
  public
    Id:           Integer;
    Content:      string;
    MemoryType:   TMemoryType;
    Importance:   Integer;        // 1–10
    Namespace:    string;
    Tags:         TArray<string>;
    Metadata:     string;         // JSON serializado
    ContentHash:  string;         // SHA-256[:16] para dedup
    Embedding:    TArray<Double>; // nil si no hay embedder
    DecayScore:   Double;         // 0.0–1.0
    CreatedAt:    TDateTime;
    AccessedAt:   TDateTime;
    AccessCount:  Integer;
    ExpiresAt:    TDateTime;      // 0 = sin TTL

    constructor Create;
    function    TagsAsString: string;    // CSV de tags
    function    ToJSON: TJSONObject;
    procedure   Assign(ASource: TMemoryEntry);
    class function FromJSON(AObj: TJSONObject): TMemoryEntry;
  end;

  TMemoryEntryList = TObjectList<TMemoryEntry>;

  TMemorySearchResult = record
    Entry:     TMemoryEntry;     // no owned — el caller gestiona lifetime
    Score:     Double;
    MatchType: string;           // 'fts' | 'semantic' | 'hybrid'
  end;

  TMemorySearchResults = TArray<TMemorySearchResult>;

  TMemoryStats = record
    Namespace:     string;
    TotalCount:    Integer;
    AvgImportance: Double;
    AvgDecay:      Double;
    OldestEntry:   TDateTime;
    NewestEntry:   TDateTime;
    ExpiredCount:  Integer;
    function ToJSON: TJSONObject;
  end;

  TMemoryContextResult = record
    FormattedText: string;
    MemoryCount:   Integer;
    TokenEstimate: Integer;
    Truncated:     Boolean;
    MemoryIds:     TArray<Integer>;
  end;

  // Interfaz mínima para que TAiChat use TAiMemory sin depender del paquete RAG.Drivers
  IAiPersistentMemory = interface
    ['{A7F3E21C-4D5B-4E8F-9A0C-3B6D7E2F1948}']
    function  BuildContext(const APrompt: string; ATokenBudget: Integer): string;
    procedure AutoStore(const AContent: string; AImportance: Integer);
  end;

// Helpers de conversión para TMemoryType
function  MemoryTypeToStr(AType: TMemoryType): string;
function  StrToMemoryType(const AStr: string): TMemoryType;

implementation

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function MemoryTypeToStr(AType: TMemoryType): string;
const
  Names: array[TMemoryType] of string = (
    'fact', 'preference', 'decision', 'error_fix',
    'pattern', 'workflow', 'summary', 'custom'
  );
begin
  Result := Names[AType];
end;

function StrToMemoryType(const AStr: string): TMemoryType;
var
  T: TMemoryType;
begin
  Result := mt_Custom;
  for T := Low(TMemoryType) to High(TMemoryType) do
    if SameText(MemoryTypeToStr(T), AStr) then
      Exit(T);
end;

// ---------------------------------------------------------------------------
// TMemoryEntry
// ---------------------------------------------------------------------------

constructor TMemoryEntry.Create;
begin
  inherited;
  MemoryType  := mt_Fact;
  Importance  := 5;
  Namespace   := 'default';
  DecayScore  := 1.0;
  CreatedAt   := Now;
  AccessedAt  := Now;
  AccessCount := 0;
  Metadata    := '{}';
end;

procedure TMemoryEntry.Assign(ASource: TMemoryEntry);
begin
  Id           := ASource.Id;
  Content      := ASource.Content;
  MemoryType   := ASource.MemoryType;
  Importance   := ASource.Importance;
  Namespace    := ASource.Namespace;
  Tags         := Copy(ASource.Tags);
  Metadata     := ASource.Metadata;
  ContentHash  := ASource.ContentHash;
  Embedding    := Copy(ASource.Embedding);
  DecayScore   := ASource.DecayScore;
  CreatedAt    := ASource.CreatedAt;
  AccessedAt   := ASource.AccessedAt;
  AccessCount  := ASource.AccessCount;
  ExpiresAt    := ASource.ExpiresAt;
end;

function TMemoryEntry.TagsAsString: string;
begin
  Result := String.Join(',', Tags);
end;

function TMemoryEntry.ToJSON: TJSONObject;
var
  TagArr: TJSONArray;
  T: string;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id',           TJSONNumber.Create(Id));
  Result.AddPair('content',      Content);
  Result.AddPair('memory_type',  MemoryTypeToStr(MemoryType));
  Result.AddPair('importance',   TJSONNumber.Create(Importance));
  Result.AddPair('namespace',    Namespace);
  Result.AddPair('content_hash', ContentHash);
  Result.AddPair('decay_score',  TJSONNumber.Create(DecayScore));
  Result.AddPair('access_count', TJSONNumber.Create(AccessCount));
  Result.AddPair('created_at',   DateToISO8601(CreatedAt, False));
  Result.AddPair('accessed_at',  DateToISO8601(AccessedAt, False));
  if ExpiresAt > 0 then
    Result.AddPair('expires_at', DateToISO8601(ExpiresAt, False));
  TagArr := TJSONArray.Create;
  for T in Tags do
    TagArr.Add(T);
  Result.AddPair('tags', TagArr);
  Result.AddPair('metadata', TJSONObject.ParseJSONValue(Metadata) as TJSONValue);
end;

class function TMemoryEntry.FromJSON(AObj: TJSONObject): TMemoryEntry;
var
  TagArr: TJSONArray;
  I: Integer;
begin
  Result := TMemoryEntry.Create;
  if AObj.TryGetValue('id', Result.Id) then ;
  AObj.TryGetValue('content',      Result.Content);
  AObj.TryGetValue('importance',   Result.Importance);
  AObj.TryGetValue('namespace',    Result.Namespace);
  AObj.TryGetValue('content_hash', Result.ContentHash);
  AObj.TryGetValue('decay_score',  Result.DecayScore);
  AObj.TryGetValue('access_count', Result.AccessCount);

  var TypeStr: string;
  if AObj.TryGetValue('memory_type', TypeStr) then
    Result.MemoryType := StrToMemoryType(TypeStr);

  var DateStr: string;
  if AObj.TryGetValue('created_at',  DateStr) then Result.CreatedAt  := ISO8601ToDate(DateStr, False);
  if AObj.TryGetValue('accessed_at', DateStr) then Result.AccessedAt := ISO8601ToDate(DateStr, False);
  if AObj.TryGetValue('expires_at',  DateStr) then Result.ExpiresAt  := ISO8601ToDate(DateStr, False);

  TagArr := AObj.GetValue<TJSONArray>('tags');
  if Assigned(TagArr) then
  begin
    SetLength(Result.Tags, TagArr.Count);
    for I := 0 to TagArr.Count - 1 do
      Result.Tags[I] := TagArr.Items[I].Value;
  end;

  var MetaVal: TJSONValue;
  if AObj.TryGetValue('metadata', MetaVal) then
    Result.Metadata := MetaVal.ToJSON;
end;

// ---------------------------------------------------------------------------
// TMemoryStats
// ---------------------------------------------------------------------------

function TMemoryStats.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('namespace',      Namespace);
  Result.AddPair('total_count',    TJSONNumber.Create(TotalCount));
  Result.AddPair('avg_importance', TJSONNumber.Create(AvgImportance));
  Result.AddPair('avg_decay',      TJSONNumber.Create(AvgDecay));
  Result.AddPair('expired_count',  TJSONNumber.Create(ExpiredCount));
  if OldestEntry > 0 then
    Result.AddPair('oldest_entry', DateToISO8601(OldestEntry, False));
  if NewestEntry > 0 then
    Result.AddPair('newest_entry', DateToISO8601(NewestEntry, False));
end;

end.
