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

unit uMakerAi.Memory;

// TAiMemory — capa semántica de memoria persistente para agentes AI.
//
// API pública de 5 operaciones core (equivalente a engram-core en Python):
//   Store   — persiste contenido con tipo, importancia, tags, TTL
//   Search  — búsqueda FTS / semántica / híbrida
//   Recall  — recupera memorias de alta importancia
//   Delete  — elimina por ID
//   Stats   — estadísticas del namespace
//
// Operaciones extendidas:
//   Context — Smart Context Builder con token budget
//   Update  — actualiza contenido / importancia de un entry existente
//   Prune   — elimina entradas antiguas de baja importancia
//   Link    — crea relación entre dos memorias (knowledge graph)
//   Export / Import — portabilidad JSON
//
// Usa la misma conexión FireDAC del proyecto si se asigna (property Connection).
// Sin conexión asignada, crea su propio archivo SQLite en DbPath.

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, System.DateUtils, System.Math, System.IOUtils,
  FireDAC.Comp.Client,
  uMakerAi.Memory.Types,
  uMakerAi.Memory.Storage,
  uMakerAi.Memory.Decay,
  uMakerAi.Memory.Context,
  uMakerAi.Embeddings.Core;

type
  TOnMemoryStore  = procedure(Sender: TObject; AEntry: TMemoryEntry) of object;
  TOnMemorySearch = procedure(Sender: TObject; const AQuery: string;
                              const AResults: TMemorySearchResults) of object;

  TAiMemory = class(TComponent, IAiPersistentMemory)
  private
    FStorage:     IAiMemoryStorage;
    FContext:     TAiMemoryContext;
    FEmbedder:    TAiEmbeddingsCore;
    FConnection:  TFDConnection;
    FNamespace:   string;
    FDbPath:      string;
    FAutoDecay:   Boolean;

    FOnStore:     TOnMemoryStore;
    FOnSearch:    TOnMemorySearch;

    procedure SetEmbedder(AValue: TAiEmbeddingsCore);
    procedure SetDbPath(const AValue: string);
    procedure SetConnection(AValue: TFDConnection);
    procedure EnsureStorage;
    procedure RebuildContext;
    function  FuseRRF(AFTS, ASemantic: TMemoryEntryList; ALimit: Integer): TMemorySearchResults;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    // ── API core (5 operaciones) ──────────────────────────────────────────────

    // Guarda una memoria. Retorna el Id asignado.
    // Si el contenido ya existe en el namespace (mismo hash), actualiza en lugar de duplicar.
    function Store(
      const AContent:  string;
      AType:           TMemoryType      = mt_Fact;
      AImportance:     Integer          = 5;
      const ATags:     string           = '';   // CSV
      AMetadata:       TJSONObject      = nil;
      ATtlDays:        Integer          = 0     // 0 = sin expiración
    ): Integer;

    // Búsqueda. AMode = ms_Hybrid usa FTS + semántica con fusión RRF.
    function Search(
      const AQuery: string;
      ALimit:       Integer          = 10;
      AMode:        TMemorySearchMode = ms_Hybrid
    ): TMemorySearchResults;

    // Recupera memorias con importancia >= AMinImportance, ordenadas por imp desc.
    function Recall(
      AMinImportance: Integer = 7;
      ALimit:         Integer = 20
    ): TMemoryEntryList;

    // Elimina la memoria con el Id dado.
    procedure Delete(AId: Integer);

    // Estadísticas del namespace actual.
    function Stats: TMemoryStats;

    // ── Operaciones extendidas ────────────────────────────────────────────────

    // Construye un bloque de texto con las memorias más relevantes para APrompt.
    // Listo para inyectar como parte del system prompt.
    function Context(
      const APrompt:  string;
      ATokenBudget:   Integer = 2000;
      AMinImportance: Integer = 1
    ): TMemoryContextResult;

    // Recupera una entrada por Id.
    function Get(AId: Integer): TMemoryEntry;

    // Actualiza contenido y/o importancia de un entry existente.
    procedure Update(
      AId:         Integer;
      const AContent: string  = '';
      AImportance: Integer    = -1   // -1 = no cambiar
    );

    // Elimina entradas con decay bajo y edad mayor a AMaxAgeDays.
    procedure Prune(AMinImportance: Integer = 3; AMaxAgeDays: Integer = 90);

    // Elimina las memorias que superaron su TTL.
    procedure CleanupExpired;

    // Regenera embeddings para entradas que no tienen embedding almacenado.
    // Requiere Embedder asignado.
    procedure BackfillEmbeddings;

    // Exporta todas las memorias del namespace actual a JSON.
    function ExportToJSON: TJSONArray;

    // Importa memorias desde un TJSONArray (generado por ExportToJSON).
    procedure ImportFromJSON(AData: TJSONArray);

    // Crea relación dirigida entre dos memorias.
    procedure Link(AFromId, AToId: Integer; const ARelation: string = 'related');
    procedure Unlink(AFromId, AToId: Integer);
    function  Links(AId: Integer): TMemoryEntryList;

    // Recalcula decay_score de todas las memorias del namespace.
    // Llamar periódicamente (e.g., al inicio de sesión).
    procedure RefreshDecay;

    // ── IAiPersistentMemory ────────────────────────────────────────────────────
    function  BuildContext(const APrompt: string; ATokenBudget: Integer): string;
    procedure AutoStore(const AContent: string; AImportance: Integer);

  published
    // Namespace activo — aísla memorias entre agentes/proyectos
    property Namespace:  string           read FNamespace  write FNamespace;

    // Ruta al archivo SQLite. Ignorado si Connection está asignada.
    property DbPath:     string           read FDbPath     write SetDbPath;

    // Conexión FireDAC externa (opcional). Si se asigna, reutiliza la BD del proyecto.
    property Connection: TFDConnection    read FConnection write SetConnection;

    // Embedder para búsqueda semántica (opcional). Si nil = solo FTS.
    property Embedder:   TAiEmbeddingsCore read FEmbedder  write SetEmbedder;

    // Si True, llama RefreshDecay en cada Store/Recall.
    property AutoDecay:  Boolean          read FAutoDecay  write FAutoDecay default False;

    property OnStore:    TOnMemoryStore   read FOnStore    write FOnStore;
    property OnSearch:   TOnMemorySearch  read FOnSearch   write FOnSearch;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI.Memory', [TAiMemory]);
end;

// ---------------------------------------------------------------------------
// Constructor / Destructor
// ---------------------------------------------------------------------------

constructor TAiMemory.Create(AOwner: TComponent);
begin
  inherited;
  FNamespace  := 'default';
  FDbPath     := TPath.Combine(TPath.GetHomePath, 'ai_memory.db');
  FAutoDecay  := False;
end;

destructor TAiMemory.Destroy;
begin
  FContext.Free;
  // FStorage é interface — se libera solo al salir del scope
  inherited;
end;

// ---------------------------------------------------------------------------
// Setters que invalidan el storage/context si cambian configuración
// ---------------------------------------------------------------------------

procedure TAiMemory.SetDbPath(const AValue: string);
begin
  if FDbPath = AValue then Exit;
  FDbPath  := AValue;
  FStorage := nil;
  FreeAndNil(FContext);
end;

procedure TAiMemory.SetConnection(AValue: TFDConnection);
begin
  if FConnection = AValue then Exit;
  FConnection := AValue;
  FStorage    := nil;
  FreeAndNil(FContext);
end;

procedure TAiMemory.SetEmbedder(AValue: TAiEmbeddingsCore);
begin
  FEmbedder := AValue;
  RebuildContext;
end;

procedure TAiMemory.EnsureStorage;
begin
  if Assigned(FStorage) then Exit;

  if Assigned(FConnection) then
    FStorage := TAiMemorySQLiteStorage.CreateWithConnection(FConnection)
  else
    FStorage := TAiMemorySQLiteStorage.CreateWithPath(FDbPath);

  RebuildContext;
end;

procedure TAiMemory.RebuildContext;
begin
  FreeAndNil(FContext);
  if Assigned(FStorage) then
    FContext := TAiMemoryContext.Create(FStorage, FEmbedder);
end;

// ---------------------------------------------------------------------------
// FuseRRF — Reciprocal Rank Fusion para combinar FTS + semántica
// ---------------------------------------------------------------------------

function TAiMemory.FuseRRF(AFTS, ASemantic: TMemoryEntryList;
  ALimit: Integer): TMemorySearchResults;
const
  K = 60; // constante RRF estándar
var
  Scores: TDictionary<Integer, Double>;
  All:    TObjectList<TMemoryEntry>;
  E:      TMemoryEntry;
  I:      Integer;
  Si, Sj: Integer;
  SA, SB: Double;

  procedure AddRank(AList: TMemoryEntryList; AMatchType: string);
  var
    Rank: Integer;
    Item: TMemoryEntry;
  begin
    Rank := 1;
    for Item in AList do
    begin
      if not Scores.ContainsKey(Item.Id) then
        Scores.Add(Item.Id, 0);
      Scores[Item.Id] := Scores[Item.Id] + 1.0 / (K + Rank);
      Inc(Rank);
    end;
  end;

begin
  Scores := TDictionary<Integer, Double>.Create;
  All    := TObjectList<TMemoryEntry>.Create(False);
  try
    AddRank(AFTS,     'fts');
    AddRank(ASemantic,'semantic');

    // Unir todos los entries únicos
    for E in AFTS do
      if not All.Contains(E) then All.Add(E);
    for E in ASemantic do
      if not All.Contains(E) then All.Add(E);

    // Ordenar por score RRF descendente — selection sort
    for Si := 0 to All.Count - 2 do
      for Sj := Si + 1 to All.Count - 1 do
      begin
        SA := 0; SB := 0;
        Scores.TryGetValue(All[Si].Id, SA);
        Scores.TryGetValue(All[Sj].Id, SB);
        if SB > SA then
        begin
          E := All[Si]; All[Si] := All[Sj]; All[Sj] := E;
        end;
      end;

    SetLength(Result, Min(ALimit, All.Count));
    for I := 0 to High(Result) do
    begin
      Result[I].Entry     := All[I];
      Result[I].Score     := 0;
      Result[I].MatchType := 'hybrid';
      Scores.TryGetValue(All[I].Id, Result[I].Score);
    end;
  finally
    Scores.Free;
    All.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Store
// ---------------------------------------------------------------------------

function TAiMemory.Store(const AContent: string; AType: TMemoryType;
  AImportance: Integer; const ATags: string; AMetadata: TJSONObject;
  ATtlDays: Integer): Integer;
var
  Entry: TMemoryEntry;
begin
  EnsureStorage;

  Entry := TMemoryEntry.Create;
  try
    Entry.Content    := AContent;
    Entry.MemoryType := AType;
    Entry.Importance := Max(1, Min(10, AImportance));
    Entry.Namespace  := FNamespace;
    Entry.DecayScore := 1.0;

    if ATags <> '' then
      Entry.Tags := ATags.Split([',']);

    if Assigned(AMetadata) then
      Entry.Metadata := AMetadata.ToJSON
    else
      Entry.Metadata := '{}';

    if ATtlDays > 0 then
      Entry.ExpiresAt := Now + ATtlDays;

    // Generar embedding si hay embedder
    if Assigned(FEmbedder) then
    begin
      try
        Entry.Embedding := FEmbedder.CreateEmbedding(AContent, '');
      except
        // Embedding falla → guarda sin embedding
      end;
    end;

    Result := FStorage.StoreEntry(Entry);

    if Assigned(FOnStore) then
      FOnStore(Self, Entry);
  finally
    Entry.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Search
// ---------------------------------------------------------------------------

function TAiMemory.Search(const AQuery: string; ALimit: Integer;
  AMode: TMemorySearchMode): TMemorySearchResults;
var
  FTSList, SemList: TMemoryEntryList;
  I:                Integer;
begin
  EnsureStorage;

  case AMode of
    ms_FTS:
    begin
      FTSList := FStorage.SearchFTS(AQuery, FNamespace, ALimit);
      try
        SetLength(Result, FTSList.Count);
        for I := 0 to FTSList.Count - 1 do
        begin
          Result[I].Entry     := FTSList[I];
          Result[I].Score     := 1.0 - (I * 0.05);
          Result[I].MatchType := 'fts';
          FStorage.UpdateAccessStats(FTSList[I].Id);
        end;
      finally
        FTSList.OwnsObjects := False; // ownership transferred to Result entries
        FTSList.Free;
      end;
    end;

    ms_Semantic:
    begin
      Result := nil;
      if not Assigned(FEmbedder) then Exit;
      try
        var EmbData := FEmbedder.CreateEmbedding(AQuery, '');
        SemList := FStorage.SearchSemantic(EmbData, FNamespace, ALimit);
        try
          SetLength(Result, SemList.Count);
          for I := 0 to SemList.Count - 1 do
          begin
            Result[I].Entry     := SemList[I];
            Result[I].Score     := 1.0 - (I * 0.06);
            Result[I].MatchType := 'semantic';
            FStorage.UpdateAccessStats(SemList[I].Id);
          end;
        finally
          SemList.Free;
        end;
      except
        Result := nil;
      end;
    end;

    ms_Hybrid:
    begin
      FTSList := FStorage.SearchFTS(AQuery, FNamespace, ALimit * 2);
      SemList := TMemoryEntryList.Create(True);
      try
        if Assigned(FEmbedder) then
        begin
          try
            var EmbData := FEmbedder.CreateEmbedding(AQuery, '');
            var Tmp := FStorage.SearchSemantic(EmbData, FNamespace, ALimit * 2);
            try
              for var E in Tmp do SemList.Add(E);
            finally
              Tmp.Free;
            end;
          except
            // falla semántica → solo FTS
          end;
        end;

        if SemList.Count = 0 then
        begin
          // Sin semántica, degradar a FTS puro
          SetLength(Result, Min(ALimit, FTSList.Count));
          for I := 0 to High(Result) do
          begin
            Result[I].Entry     := FTSList[I];
            Result[I].Score     := 1.0 - (I * 0.05);
            Result[I].MatchType := 'fts';
            FStorage.UpdateAccessStats(FTSList[I].Id);
          end;
        end
        else
          Result := FuseRRF(FTSList, SemList, ALimit);

      finally
        FTSList.OwnsObjects := False; // ownership transferred to Result entries
        SemList.OwnsObjects := False;
        FTSList.Free;
        SemList.Free;
      end;
    end;
  end;

  if Assigned(FOnSearch) then
    FOnSearch(Self, AQuery, Result);
end;

// ---------------------------------------------------------------------------
// Recall
// ---------------------------------------------------------------------------

function TAiMemory.Recall(AMinImportance: Integer; ALimit: Integer): TMemoryEntryList;
begin
  EnsureStorage;
  Result := FStorage.Recall(AMinImportance, FNamespace, ALimit);
  if FAutoDecay then RefreshDecay;
end;

// ---------------------------------------------------------------------------
// Delete
// ---------------------------------------------------------------------------

procedure TAiMemory.Delete(AId: Integer);
begin
  EnsureStorage;
  FStorage.DeleteById(AId);
end;

// ---------------------------------------------------------------------------
// Stats
// ---------------------------------------------------------------------------

function TAiMemory.Stats: TMemoryStats;
begin
  EnsureStorage;
  Result := FStorage.Stats(FNamespace);
end;

// ---------------------------------------------------------------------------
// Context
// ---------------------------------------------------------------------------

function TAiMemory.Context(const APrompt: string; ATokenBudget: Integer;
  AMinImportance: Integer): TMemoryContextResult;
begin
  EnsureStorage;
  Result := FContext.Build(APrompt, ATokenBudget, AMinImportance, FNamespace);
end;

// ---------------------------------------------------------------------------
// IAiPersistentMemory
// ---------------------------------------------------------------------------

function TAiMemory.BuildContext(const APrompt: string; ATokenBudget: Integer): string;
begin
  Result := Context(APrompt, ATokenBudget).FormattedText;
end;

procedure TAiMemory.AutoStore(const AContent: string; AImportance: Integer);
begin
  Store(AContent, mt_Fact, AImportance);
end;

// ---------------------------------------------------------------------------
// Get
// ---------------------------------------------------------------------------

function TAiMemory.Get(AId: Integer): TMemoryEntry;
begin
  EnsureStorage;
  Result := FStorage.GetById(AId);
  if Assigned(Result) then
    FStorage.UpdateAccessStats(AId);
end;

// ---------------------------------------------------------------------------
// Update
// ---------------------------------------------------------------------------

procedure TAiMemory.Update(AId: Integer; const AContent: string;
  AImportance: Integer);
var
  Current:      TMemoryEntry;
  NewContent:   string;
  NewImportance: Integer;
begin
  EnsureStorage;
  Current := FStorage.GetById(AId);
  if not Assigned(Current) then Exit;
  try
    if AContent <> '' then NewContent := AContent else NewContent := Current.Content;
    if AImportance >= 0 then NewImportance := AImportance else NewImportance := Current.Importance;
    FStorage.UpdateContent(AId, NewContent, NewImportance);
  finally
    Current.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Prune — elimina entradas antiguas de baja importancia con decay bajo
// ---------------------------------------------------------------------------

procedure TAiMemory.Prune(AMinImportance: Integer; AMaxAgeDays: Integer);
var
  AllEntries: TMemoryEntryList;
  E: TMemoryEntry;
  Cutoff: TDateTime;
begin
  EnsureStorage;
  Cutoff     := Now - AMaxAgeDays;
  AllEntries := FStorage.ExportAll(FNamespace);
  try
    for E in AllEntries do
    begin
      if (E.Importance < AMinImportance) and
         (E.CreatedAt < Cutoff) and
         (TAiMemoryDecay.Compute(E.Importance, E.AccessCount, E.AccessedAt) < 0.2) then
        FStorage.DeleteById(E.Id);
    end;
  finally
    AllEntries.Free;
  end;
end;

// ---------------------------------------------------------------------------
// CleanupExpired
// ---------------------------------------------------------------------------

procedure TAiMemory.CleanupExpired;
begin
  EnsureStorage;
  FStorage.CleanupExpired(FNamespace);
end;

// ---------------------------------------------------------------------------
// BackfillEmbeddings
// ---------------------------------------------------------------------------

procedure TAiMemory.BackfillEmbeddings;
var
  AllEntries: TMemoryEntryList;
  E: TMemoryEntry;
begin
  if not Assigned(FEmbedder) then Exit;
  EnsureStorage;
  AllEntries := FStorage.ExportAll(FNamespace);
  try
    for E in AllEntries do
    begin
      if Length(E.Embedding) = 0 then
      begin
        try
          var Emb := FEmbedder.CreateEmbedding(E.Content, '');
          if Length(Emb) > 0 then
          begin
            // Re-store con embedding — el storage hace upsert por content_hash
            var NewEntry := TMemoryEntry.Create;
            try
              NewEntry.Assign(E);
              NewEntry.Embedding := Emb;
              FStorage.StoreEntry(NewEntry);
            finally
              NewEntry.Free;
            end;
          end;
        except
          // Continúa con el siguiente
        end;
      end;
    end;
  finally
    AllEntries.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Export / Import
// ---------------------------------------------------------------------------

function TAiMemory.ExportToJSON: TJSONArray;
var
  AllEntries: TMemoryEntryList;
  E: TMemoryEntry;
begin
  EnsureStorage;
  Result     := TJSONArray.Create;
  AllEntries := FStorage.ExportAll(FNamespace);
  try
    for E in AllEntries do
      Result.AddElement(E.ToJSON);
  finally
    AllEntries.Free;
  end;
end;

procedure TAiMemory.ImportFromJSON(AData: TJSONArray);
var
  I:     Integer;
  Entry: TMemoryEntry;
begin
  EnsureStorage;
  for I := 0 to AData.Count - 1 do
  begin
    Entry := TMemoryEntry.FromJSON(AData.Items[I] as TJSONObject);
    try
      FStorage.StoreEntry(Entry);
    finally
      Entry.Free;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// Graph
// ---------------------------------------------------------------------------

procedure TAiMemory.Link(AFromId, AToId: Integer; const ARelation: string);
begin
  EnsureStorage;
  FStorage.LinkEntries(AFromId, AToId, ARelation);
end;

procedure TAiMemory.Unlink(AFromId, AToId: Integer);
begin
  EnsureStorage;
  FStorage.UnlinkEntries(AFromId, AToId);
end;

function TAiMemory.Links(AId: Integer): TMemoryEntryList;
begin
  EnsureStorage;
  Result := FStorage.GetLinks(AId, FNamespace);
end;

// ---------------------------------------------------------------------------
// RefreshDecay — recalcula decay_score de todas las memorias del namespace
// ---------------------------------------------------------------------------

procedure TAiMemory.RefreshDecay;
var
  AllEntries: TMemoryEntryList;
  E:     TMemoryEntry;
  Score: Double;
begin
  EnsureStorage;
  AllEntries := FStorage.ExportAll(FNamespace);
  try
    for E in AllEntries do
    begin
      Score := TAiMemoryDecay.Compute(E.Importance, E.AccessCount, E.AccessedAt);
      if Abs(Score - E.DecayScore) > 0.01 then
        FStorage.UpdateDecayScore(E.Id, Score);
    end;
  finally
    AllEntries.Free;
  end;
end;

end.
