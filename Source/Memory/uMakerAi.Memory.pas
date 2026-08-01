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
// La persistencia es autonoma: crea y gestiona su propio archivo SQLite en DbPath.

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, System.DateUtils, System.Math, System.IOUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Memory.Types,
  uMakerAi.Memory.Storage,
  uMakerAi.Memory.Decay,
  uMakerAi.Memory.Context,
  uMakerAi.Embeddings.Core;

type
  TOnMemoryStore  = procedure(Sender: TObject; AEntry: TMemoryEntry) of object;
  TOnMemorySearch = procedure(Sender: TObject; const AQuery: string;
                              const AResults: TMemorySearchResults) of object;

  TAiMemory = class(TAiPersistentMemoryBase)
  private
    FStorage:     IAiMemoryStorage;
    FContext:     TAiMemoryContext;
    FEmbedder:    TAiEmbeddingsCore;
    FNamespace:   string;
    FDbPath:      string;
    FAutoDecay:   Boolean;

    FOnStore:     TOnMemoryStore;
    FOnSearch:    TOnMemorySearch;

    // Analisis automatico de conversacion (extraccion de hechos via LLM barato)
    FAnalyzer:            TAiChatConnection;
    FAnalysisInterval:    Integer;
    FExchangeBuffer:      TStringBuilder;
    FTurnsSinceAnalysis:  Integer;

    procedure SetEmbedder(AValue: TAiEmbeddingsCore);
    procedure SetDbPath(const AValue: string);
    procedure SetAnalyzer(AValue: TAiChatConnection);
    procedure EnsureStorage;
    procedure RebuildContext;
    function  FuseRRF(AFTS, ASemantic: TMemoryEntryList; ALimit: Integer): TMemorySearchResults;
    function  DoAnalyzeAndStore(const AConversationText: string): Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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

    // ── IAiPersistentMemory (override de TAiPersistentMemoryBase) ──────────────
    function  BuildContext(const APrompt: string; ATokenBudget: Integer): string; override;
    procedure AutoStore(const AContent: string; AImportance: Integer); override;
    procedure NotifyExchange(const AUserPrompt, AAssistantResponse: string); override;

    // Fuerza el analisis del buffer acumulado ahora mismo, sin esperar a
    // AnalysisInterval (uso manual: boton "recordar esto", cron, etc.).
    // Requiere Analyzer asignado. Retorna cuantas memorias se guardaron.
    function AnalyzeNow: Integer;

  published
    // LLM barato usado para analizar la conversacion y extraer lo que valga la pena
    // recordar (ver AnalysisInterval / AnalyzeNow). Sin asignar = funcion apagada.
    property Analyzer: TAiChatConnection read FAnalyzer write SetAnalyzer;

    // Cada cuantos intercambios usuario/asistente se dispara el analisis
    // automatico. 0 = desactivado (solo queda el disparo manual via AnalyzeNow).
    property AnalysisInterval: Integer read FAnalysisInterval write FAnalysisInterval default 10;
    // Namespace activo — aísla memorias entre agentes/proyectos
    property Namespace:  string           read FNamespace  write FNamespace;

    // Ruta al archivo SQLite donde persisten las memorias.
    property DbPath:     string           read FDbPath     write SetDbPath;

    // Embedder para búsqueda semántica (opcional). Si nil = solo FTS.
    property Embedder:   TAiEmbeddingsCore read FEmbedder  write SetEmbedder;

    // Si True, llama RefreshDecay en cada Recall (Store no lo dispara).
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
  FAnalysisInterval   := 10;
  FExchangeBuffer     := TStringBuilder.Create;
  FTurnsSinceAnalysis := 0;
end;

destructor TAiMemory.Destroy;
begin
  FContext.Free;
  FExchangeBuffer.Free;
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

procedure TAiMemory.SetEmbedder(AValue: TAiEmbeddingsCore);
begin
  if FEmbedder = AValue then Exit;
  if Assigned(FEmbedder) then
    FEmbedder.RemoveFreeNotification(Self);
  FEmbedder := AValue;
  if Assigned(FEmbedder) then
    FEmbedder.FreeNotification(Self);
  RebuildContext;
end;

procedure TAiMemory.SetAnalyzer(AValue: TAiChatConnection);
begin
  if FAnalyzer = AValue then Exit;
  if Assigned(FAnalyzer) then
    FAnalyzer.RemoveFreeNotification(Self);
  FAnalyzer := AValue;
  if Assigned(FAnalyzer) then
    FAnalyzer.FreeNotification(Self);
end;

procedure TAiMemory.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FAnalyzer then
      FAnalyzer := nil;

    if AComponent = FEmbedder then
    begin
      // El contexto retiene el embedder: reconstruir sin el (queda solo FTS)
      FEmbedder := nil;
      RebuildContext;
    end;
  end;
end;

procedure TAiMemory.EnsureStorage;
begin
  if Assigned(FStorage) then Exit;

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
  ById:   TDictionary<Integer, TMemoryEntry>;
  All:    TObjectList<TMemoryEntry>;
  E:      TMemoryEntry;
  I:      Integer;
  Si, Sj: Integer;
  SA, SB: Double;

  procedure AddRank(AList: TMemoryEntryList);
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

  // Dedup por Id: FTS y semántica devuelven OBJETOS distintos para la misma
  // fila, así que comparar referencias (All.Contains) duplicaba resultados.
  // El duplicado descartado se libera aquí: el caller pone OwnsObjects=False
  // en las listas de entrada antes de liberarlas, nadie más lo posee.
  procedure Merge(AList: TMemoryEntryList);
  var
    Item: TMemoryEntry;
  begin
    for Item in AList do
      if not ById.ContainsKey(Item.Id) then
      begin
        ById.Add(Item.Id, Item);
        All.Add(Item);
      end
      else
        Item.Free;
  end;

begin
  Scores := TDictionary<Integer, Double>.Create;
  ById   := TDictionary<Integer, TMemoryEntry>.Create;
  All    := TObjectList<TMemoryEntry>.Create(False);
  try
    AddRank(AFTS);
    AddRank(ASemantic);

    // Unir todos los entries únicos (por Id)
    Merge(AFTS);
    Merge(ASemantic);

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
    // Los que quedaron fuera del límite no van en Result y nadie más los
    // posee: liberarlos aquí evita la fuga.
    for I := Length(Result) to All.Count - 1 do
      All[I].Free;
  finally
    Scores.Free;
    ById.Free;
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
          // FTSList trae hasta ALimit*2 entradas; las que no pasaron a Result
          // no las posee nadie más (el finally quita OwnsObjects antes de
          // liberar la lista): liberarlas aquí evita la fuga.
          for I := Length(Result) to FTSList.Count - 1 do
            FTSList[I].Free;
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
// Analisis automatico de conversacion (extraccion via LLM barato)
// ---------------------------------------------------------------------------

procedure TAiMemory.NotifyExchange(const AUserPrompt, AAssistantResponse: string);
begin
  if not Assigned(FAnalyzer) then
    Exit; // funcion apagada por completo sin Analyzer configurado

  FExchangeBuffer.Append('Usuario: ').Append(AUserPrompt).Append(sLineBreak);
  FExchangeBuffer.Append('Asistente: ').Append(AAssistantResponse).Append(sLineBreak).Append(sLineBreak);
  Inc(FTurnsSinceAnalysis);

  // El buffer se acumula siempre que hay Analyzer, independientemente de
  // AnalysisInterval: si el auto-disparo esta apagado (0), igual queda
  // disponible para un AnalyzeNow manual.
  if (FAnalysisInterval > 0) and (FTurnsSinceAnalysis >= FAnalysisInterval) then
    AnalyzeNow;
end;

function TAiMemory.AnalyzeNow: Integer;
var
  LConversationText: string;
begin
  Result := 0;
  if not Assigned(FAnalyzer) or (FExchangeBuffer.Length = 0) then
    Exit;

  // Snapshot + reset inmediato: si el analisis falla mas abajo, no reintentamos
  // la misma ventana para siempre (evita crecimiento sin limite del buffer).
  LConversationText   := FExchangeBuffer.ToString;
  FExchangeBuffer.Clear;
  FTurnsSinceAnalysis := 0;

  Result := DoAnalyzeAndStore(LConversationText);
end;

function TAiMemory.DoAnalyzeAndStore(const AConversationText: string): Integer;
var
  LPrompt, LResponse, LJsonText, LTypeStr, LContent, LTags: string;
  LJsonValue: TJSONValue;
  LJsonObj: TJSONObject;
  LMemories: TJSONArray;
  LItem: TJSONObject;
  LWorthSaving: Boolean;
  LImportance, LStart, LEnd, I: Integer;
begin
  Result := 0;

  // Extraccion via prompt (no via Response_format:=tiaChatRfJsonSchema): Analyzer
  // puede ser cualquiera de los 12 providers, incluyendo modelos locales/baratos
  // (Ollama, LM Studio) donde el modo JSON estricto no siempre esta soportado de
  // forma confiable, y ademas puede chocar con Tool_Active en algunos providers
  // (p.ej. Gemini). Se parsea el texto de forma defensiva mas abajo.
  LPrompt :=
    'Analiza el siguiente fragmento de conversacion y decide si contiene informacion ' +
    'que valga la pena recordar a largo plazo: hechos sobre el usuario, preferencias, ' +
    'decisiones tomadas, soluciones a errores, patrones de trabajo. Ignora saludos, ' +
    'charla trivial o preguntas puntuales sin informacion reutilizable.' + sLineBreak + sLineBreak +
    'Responde UNICAMENTE con un objeto JSON (sin texto adicional, sin bloques de codigo ' +
    'markdown) con esta forma exacta:' + sLineBreak +
    '{"worth_saving": boolean, "memories": [{"content": string, "type": ' +
    '"fact"|"preference"|"decision"|"error_fix"|"pattern"|"workflow"|"summary"|"custom", ' +
    '"importance": 1-10, "tags": "csv"}]}' + sLineBreak +
    'Si no hay nada memorable, responde {"worth_saving": false, "memories": []}.' +
    sLineBreak + sLineBreak +
    'Conversacion:' + sLineBreak + AConversationText;

  try
    LResponse := FAnalyzer.AddMessageAndRun(LPrompt, 'user', []);
  except
    on E: Exception do
    begin
      LogDebug('TAiMemory.Analyzer fallo al analizar la conversacion: ' + E.Message);
      Exit;
    end;
  end;

  // Tolerar que el modelo igual envuelva la respuesta en texto/markdown alrededor
  // del JSON: nos quedamos con lo que hay entre la primera '{' y la ultima '}'.
  LStart := Pos('{', LResponse);
  LEnd   := LastDelimiter('}', LResponse);
  if (LStart = 0) or (LEnd < LStart) then
  begin
    LogDebug('TAiMemory.Analyzer: respuesta sin JSON reconocible, se descarta.');
    Exit;
  end;
  LJsonText := Copy(LResponse, LStart, LEnd - LStart + 1);

  LJsonValue := TJSONObject.ParseJSONValue(LJsonText);
  if not Assigned(LJsonValue) then
  begin
    LogDebug('TAiMemory.Analyzer: JSON invalido, se descarta.');
    Exit;
  end;

  try
    if not (LJsonValue is TJSONObject) then
      Exit;
    LJsonObj := TJSONObject(LJsonValue);

    LWorthSaving := False;
    LJsonObj.TryGetValue('worth_saving', LWorthSaving);
    if not LWorthSaving then
      Exit;

    LMemories := LJsonObj.GetValue<TJSONArray>('memories');
    if not Assigned(LMemories) then
      Exit;

    for I := 0 to LMemories.Count - 1 do
    begin
      if not (LMemories.Items[I] is TJSONObject) then
        Continue;
      LItem := TJSONObject(LMemories.Items[I]);

      LContent := '';
      LItem.TryGetValue('content', LContent);
      if Trim(LContent) = '' then
        Continue;

      LTypeStr := 'custom';
      LItem.TryGetValue('type', LTypeStr);
      LImportance := 5;
      LItem.TryGetValue('importance', LImportance);
      LTags := '';
      LItem.TryGetValue('tags', LTags);

      Store(LContent, StrToMemoryType(LTypeStr), LImportance, LTags);
      Inc(Result);
    end;
  finally
    LJsonValue.Free;
  end;
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
