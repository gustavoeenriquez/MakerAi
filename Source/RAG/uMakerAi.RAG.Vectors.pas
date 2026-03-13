// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.RAG.Vectors
// Motor principal RAG vectorial: búsqueda semántica (HNSW/coseno), léxica (BM25)
// y fusión híbrida (RRF / ponderada). Serialización JSON.
unit uMakerAi.RAG.Vectors;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, Variants,
  fgl,
  fpjson, jsonparser,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vectors.VQL,
  uMakerAi.Embeddings.Core,
  uMakerAi.RAG.MetaData;

type
  TAiRAGVector = class;

  TOnDataVecAddItem = procedure(Sender: TObject; aItem: TAiEmbeddingNode;
      MetaData: TAiEmbeddingMetaData; var Handled: Boolean) of object;
  TOnDataVecSearch  = procedure(Sender: TObject; Target: TAiEmbeddingNode;
      const aPrompt: string; aLimit: Integer; aPrecision: Double;
      aFilter: TAiFilterCriteria; var aDataVec: TAiRAGVector;
      var Handled: Boolean) of object;
  TOnImportProgress = procedure(Sender: TObject; Position, Total: Integer;
      var Cancel: Boolean) of object;

  // ---------------------------------------------------------------------------
  // TAiSearchOptions — configuración del motor de búsqueda híbrida
  // ---------------------------------------------------------------------------
  TAiSearchOptions = class(TPersistent)
  private
    FUseEmbeddings            : Boolean;
    FUseBM25                  : Boolean;
    FUseRRF                   : Boolean;
    FUseReorderABC            : Boolean;
    FBM25Weight               : Double;
    FEmbeddingWeight          : Double;
    FOnChange                 : TNotifyEvent;
    FMinAbsoluteScoreEmbedding: Double;
    FMinAbsoluteScoreBM25     : Double;
    procedure SetUseEmbeddings(const Value: Boolean);
    procedure SetUseBM25(const Value: Boolean);
    procedure SetUseRRF(const Value: Boolean);
    procedure SetUseReorderABC(const Value: Boolean);
    procedure SetBM25Weight(const Value: Double);
    procedure SetEmbeddingWeight(const Value: Double);
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property UseEmbeddings : Boolean read FUseEmbeddings  write SetUseEmbeddings  default True;
    property UseBM25       : Boolean read FUseBM25         write SetUseBM25         default True;
    property UseRRF        : Boolean read FUseRRF          write SetUseRRF          default False;
    property UseReorderABC : Boolean read FUseReorderABC   write SetUseReorderABC   default False;
    property BM25Weight    : Double  read FBM25Weight      write SetBM25Weight;
    property EmbeddingWeight: Double read FEmbeddingWeight  write SetEmbeddingWeight;
    property MinAbsoluteScoreEmbedding: Double
        read FMinAbsoluteScoreEmbedding write FMinAbsoluteScoreEmbedding;
    property MinAbsoluteScoreBM25: Double
        read FMinAbsoluteScoreBM25 write FMinAbsoluteScoreBM25;
  end;

  // ---------------------------------------------------------------------------
  // TAiVectorStoreDriverBase — base para drivers de persistencia externos
  // ---------------------------------------------------------------------------
  TAiVectorStoreDriverBase = class(TComponent)
  protected
    procedure Add(const aNode: TAiEmbeddingNode;
        const AEntidad: string); virtual; abstract;
    function Search(const ATarget: TAiEmbeddingNode; const AEntidad: string;
        aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria;
        Options: TAiSearchOptions): TAiRAGVector; virtual; abstract;
    procedure Delete(const aID, AEntidad: string); virtual; abstract;
    procedure Clear(const AEntidad: string); virtual; abstract;
  public
    function EmbeddingToString(const AData: TAiEmbeddingData): string;
    function VariantToJSONValue(const V: Variant): TJSONData;
  end;

  // ---------------------------------------------------------------------------
  // TAiRAGVector — contenedor principal de embeddings + motor de búsqueda
  // ---------------------------------------------------------------------------
  TAiRAGVector = class(TComponent)
  private
    FActive             : Boolean;
    FRagIndex           : TAIEmbeddingIndex;
    FEmbeddings         : TAiEmbeddingsCore;
    FItems              : TRagItems;
    FOnDataVecAddItem   : TOnDataVecAddItem;
    FOnDataVecSearch    : TOnDataVecSearch;
    FDim                : Integer;
    FModel              : string;
    FNameVec            : string;
    FDescription        : string;
    FInMemoryIndexType  : TAiRagIndexType;
    FOnGetEmbedding     : TOnGetEmbedding;
    FOnFilterItem       : TOnFilterItem;
    FBm25Index          : TAIBm25Index;
    FLock               : TMultiReadExclusiveWriteSynchronizer;
    FDriver             : TAiVectorStoreDriverBase;
    FEntidad            : string;
    FOwnsObjects        : Boolean;
    FOnImportProgress   : TOnImportProgress;
    FSearchOptions      : TAiSearchOptions;
    procedure SetActive(const Value: Boolean);
    procedure SetRagIndex(const Value: TAIEmbeddingIndex);
    procedure SetEmbeddings(const Value: TAiEmbeddingsCore);
    function  GetItems: TRagItems;
    procedure SetOnDataVecAddItem(const Value: TOnDataVecAddItem);
    procedure SetOnDataVecSearch(const Value: TOnDataVecSearch);
    procedure SetDescription(const Value: string);
    procedure SetNameVec(const Value: string);
    procedure SetInMemoryIndexType(const Value: TAiRagIndexType);
    function  ReciprocalRankFusion(VectorResults: TSearchResultList;
        LexicalResults: TBM25ResultList; aLimit: Integer): TAiRAGVector;
    function  GetLexicalLanguage: TAiLanguage;
    procedure SetLexicalLanguage(const Value: TAiLanguage);
    procedure SetDriver(const Value: TAiVectorStoreDriverBase);
    procedure SetSearchOptions(const Value: TAiSearchOptions);
    procedure NormalizeResults(aList: TSearchResultList);
  protected
    function  DoOnGetEmbedding(aInput, aUser: string; aDimensions: Integer = -1;
        aModel: string = ''; aEncodingFormat: string = 'float'): TAiEmbeddingData;
    procedure InternalInit(AOwnsObjects: Boolean);
    procedure CheckIndexes;
    function  InternalSearchSafe(Target: TAiEmbeddingNode; aLimit: Integer;
        aPrecision: Double; aFilter: TAiFilterCriteria): TSearchResultList;
    function  FilterByCriteria(const aCriteria: TAiFilterCriteria): TAiRAGVector;
    procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;
    function  WeightedScoreFusion(VectorResults: TSearchResultList;
        LexicalResults: TBM25ResultList; aLimit: Integer): TAiRAGVector;
    procedure ApplyContextReordering(Vector: TAiRAGVector);
    procedure InternalNormalizeVector(aList: TSearchResultList);
    procedure InternalNormalizeLexical(aList: TBM25ResultList);
  public
    constructor Create(aOwner: TComponent;
        AOwnsObjects: Boolean); reintroduce; overload;
    constructor Create(aOwner: TComponent); overload; override;
    destructor  Destroy; override;

    procedure SaveToStream(Stream: TMemoryStream);
    function  LoadFromStream(Stream: TStream): Integer;
    procedure SaveToFile(FileName: string);
    function  LoadFromFile(FileName: string): Integer;
    function  Connect(aHost, aPort, aLogin, aPassword: string): Boolean;

    function Search(Target: TAiEmbeddingNode; aLimit: Integer;
        aPrecision: Double;
        aFilter: TAiFilterCriteria): TAiRAGVector; overload;
    function Search(Prompt: string; aLimit: Integer; aPrecision: Double;
        aFilter: TAiFilterCriteria): TAiRAGVector; overload;

    function SearchText(aPrompt: string; aLimit: Integer = 10;
        aPrecision: Double = 0.5; aFilter: TAiFilterCriteria = nil;
        IncludeMetadata: Boolean = True;
        IncludeScore: Boolean = True): string; overload; virtual;
    function SearchText(aPrompt: TAiEmbeddingNode; aLimit: Integer = 10;
        aPresicion: Double = 0.5; aFilter: TAiFilterCriteria = nil;
        IncludeMetadata: Boolean = True;
        IncludeScore: Boolean = True): string; overload; virtual;

    function ExecuteVGQL(const AVgqlQuery: string): string; overload;
    function ExecuteVGQL(const AVgqlQuery: string;
        out AResultVector: TAiRAGVector): string; overload;

    function VectorToContextText(DataVec: TAiRAGVector;
        IncludeMetadata: Boolean; IncludeScore: Boolean): string;

    procedure BuildIndex;
    procedure BuildLexicalIndex;

    function AddItem(aItem: TAiEmbeddingNode): string; overload;
    function AddItem(aItem: TAiEmbeddingNode;
        MetaData: TAiEmbeddingMetaData): NativeInt; overload; virtual;
    function AddItem(aText: string;
        MetaData: TAiEmbeddingMetaData = nil): TAiEmbeddingNode; overload; virtual;

    function AddItemsFromJSonArray(aJSonArray: TJSONArray;
        MetaData: TAiEmbeddingMetaData = nil): Integer; virtual;
    function AddItemsFromPlainText(aText: string;
        MetaData: TAiEmbeddingMetaData; aLenChunk: Integer;
        aOverlapPct: Integer): Integer; virtual;

    function CreateEmbeddingNode(aText: string;
        aEmbeddings: TAiEmbeddingsCore = nil): TAiEmbeddingNode;
    function Count: Integer;
    procedure Clear;

    procedure RegenerateAll(const aNewModel: string = ''); virtual;
    procedure Rerank(Target: TAiEmbeddingNode;
        aAutoRegenerate: Boolean = True); overload;
    procedure Rerank(NewPrompt: string;
        aAutoRegenerate: Boolean = True); overload;
    function  FilterByMetaData(
        const aCriteria: TAiEmbeddingMetaData): TAiRAGVector;

    property RagIndex: TAIEmbeddingIndex read FRagIndex write SetRagIndex;
    property Active  : Boolean          read FActive   write SetActive;
    property Items   : TRagItems        read GetItems;
  published
    property OnDataVecAddItem: TOnDataVecAddItem
        read FOnDataVecAddItem write SetOnDataVecAddItem;
    property OnDataVecSearch : TOnDataVecSearch
        read FOnDataVecSearch  write SetOnDataVecSearch;
    property OnGetEmbedding  : TOnGetEmbedding
        read FOnGetEmbedding write FOnGetEmbedding;
    property OnFilterItem    : TOnFilterItem
        read FOnFilterItem   write FOnFilterItem;
    property OnImportProgress: TOnImportProgress
        read FOnImportProgress write FOnImportProgress;

    property Embeddings      : TAiEmbeddingsCore read FEmbeddings write SetEmbeddings;
    property Model           : string            read FModel;
    property Dim             : Integer           read FDim;
    property NameVec         : string            read FNameVec    write SetNameVec;
    property Description     : string            read FDescription write SetDescription;
    property InMemoryIndexType: TAiRagIndexType
        read FInMemoryIndexType write SetInMemoryIndexType default TAIHNSWIndex;
    property LexicalLanguage : TAiLanguage
        read GetLexicalLanguage write SetLexicalLanguage;
    property Driver          : TAiVectorStoreDriverBase read FDriver write SetDriver;
    property Entidad         : string read FEntidad write FEntidad;
    property OwnsObjects     : Boolean read FOwnsObjects write FOwnsObjects;
    property SearchOptions   : TAiSearchOptions
        read FSearchOptions write SetSearchOptions;
  end;

implementation

// ---------------------------------------------------------------------------
// Helpers locales
// ---------------------------------------------------------------------------

// Reemplaza TFormatSettings.Invariant de Delphi
function InvFS: TFormatSettings;
begin
  Result                   := DefaultFormatSettings;
  Result.DecimalSeparator  := '.';
  Result.ThousandSeparator := #0;
end;

// Acumulador de puntajes por nodo (reemplaza TDictionary<TAiEmbeddingNode, Double>)
type
  TNodeAccum = record
    Node : TAiEmbeddingNode;
    Score: Double;
  end;

function AccumIndexOf(const Arr: array of TNodeAccum; Count: Integer;
    ANode: TAiEmbeddingNode): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Arr[I].Node = ANode then begin Result := I; Exit; end;
  Result := -1;
end;

// ---------------------------------------------------------------------------
// QuickSort para TNodeAccum descending por Score
// ---------------------------------------------------------------------------
procedure QSortNodeAccumDesc(var A: array of TNodeAccum; L, R: Integer);
var
  I, J: Integer;
  P   : Double;
  Tmp : TNodeAccum;
begin
  if L >= R then Exit;
  I := L; J := R;
  P := A[(L + R) div 2].Score;
  repeat
    while A[I].Score > P do Inc(I);
    while A[J].Score < P do Dec(J);
    if I <= J then begin
      Tmp := A[I]; A[I] := A[J]; A[J] := Tmp;
      Inc(I); Dec(J);
    end;
  until I > J;
  if L < J then QSortNodeAccumDesc(A, L, J);
  if I < R then QSortNodeAccumDesc(A, I, R);
end;

// ===========================================================================
// TAiSearchOptions
// ===========================================================================

constructor TAiSearchOptions.Create;
begin
  inherited;
  FUseEmbeddings := True;
  FUseBM25       := True;
  FUseRRF        := False;
  FUseReorderABC := False;
  FBM25Weight    := 0.7;
  FEmbeddingWeight := 0.3;
end;

procedure TAiSearchOptions.Assign(Source: TPersistent);
var
  S: TAiSearchOptions;
begin
  if Source is TAiSearchOptions then
  begin
    S := TAiSearchOptions(Source);
    FUseEmbeddings := S.UseEmbeddings;
    FUseBM25       := S.UseBM25;
    FUseRRF        := S.UseRRF;
    FUseReorderABC := S.UseReorderABC;
    FBM25Weight    := S.BM25Weight;
    FEmbeddingWeight := S.EmbeddingWeight;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TAiSearchOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TAiSearchOptions.SetUseEmbeddings(const Value: Boolean);
begin
  if FUseEmbeddings <> Value then begin FUseEmbeddings := Value; Changed; end;
end;

procedure TAiSearchOptions.SetUseBM25(const Value: Boolean);
begin
  if FUseBM25 <> Value then begin FUseBM25 := Value; Changed; end;
end;

procedure TAiSearchOptions.SetUseRRF(const Value: Boolean);
begin
  if FUseRRF <> Value then begin FUseRRF := Value; Changed; end;
end;

procedure TAiSearchOptions.SetUseReorderABC(const Value: Boolean);
begin
  if FUseReorderABC <> Value then begin FUseReorderABC := Value; Changed; end;
end;

procedure TAiSearchOptions.SetBM25Weight(const Value: Double);
begin
  if FBM25Weight <> Value then begin FBM25Weight := Value; Changed; end;
end;

procedure TAiSearchOptions.SetEmbeddingWeight(const Value: Double);
begin
  if FEmbeddingWeight <> Value then begin FEmbeddingWeight := Value; Changed; end;
end;

// ===========================================================================
// TAiVectorStoreDriverBase
// ===========================================================================

function TAiVectorStoreDriverBase.EmbeddingToString(
    const AData: TAiEmbeddingData): string;
var
  I : Integer;
  FS: TFormatSettings;
  SResult: string;
begin
  if Length(AData) = 0 then begin Result := '[]'; Exit; end;
  FS      := InvFS;
  SResult := '[';
  for I := 0 to High(AData) do
  begin
    SResult += FloatToStr(AData[I], FS);
    if I < High(AData) then SResult += ',';
  end;
  SResult += ']';
  Result  := SResult;
end;

function TAiVectorStoreDriverBase.VariantToJSONValue(const V: Variant): TJSONData;
begin
  case VarType(V) of
    varSmallint, varInteger, varByte, varShortInt,
    varWord, varLongWord, varInt64:
      Result := TJSONInt64Number.Create(Int64(V));
    varSingle, varDouble, varCurrency:
      Result := TJSONFloatNumber.Create(Double(V));
    varBoolean:
      Result := TJSONBoolean.Create(Boolean(V));
    varNull, varEmpty:
      Result := TJSONNull.Create;
  else
    Result := TJSONString.Create(VarToStr(V));
  end;
end;

// ===========================================================================
// TAiRAGVector — constructores y destructor
// ===========================================================================

constructor TAiRAGVector.Create(aOwner: TComponent; AOwnsObjects: Boolean);
begin
  inherited Create(aOwner);
  InternalInit(AOwnsObjects);
end;

constructor TAiRAGVector.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InternalInit(True);
end;

destructor TAiRAGVector.Destroy;
var
  I: Integer;
begin
  FActive := False;

  // Índices primero (contienen punteros a Items)
  FreeAndNil(FBm25Index);
  FreeAndNil(FRagIndex);

  // Liberar nodos si somos dueños
  if Assigned(FItems) then
  begin
    if FOwnsObjects then
      for I := 0 to FItems.Count - 1 do
        FItems[I].Free;
    FreeAndNil(FItems);
  end;

  FreeAndNil(FLock);
  FSearchOptions.Free;

  inherited Destroy;
end;

procedure TAiRAGVector.InternalInit(AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;
  FLock        := TMultiReadExclusiveWriteSynchronizer.Create;
  FItems       := TRagItems.Create;
  FBm25Index   := TAIBm25Index.Create;
  FSearchOptions := TAiSearchOptions.Create;
  FSearchOptions.UseEmbeddings := True;
  FSearchOptions.UseBM25       := False;
  FSearchOptions.UseRRF        := False;
  FSearchOptions.UseReorderABC := False;
  FInMemoryIndexType := TAIHNSWIndex;
  FRagIndex := THNSWIndex.Create;
  FRagIndex.BuildIndex(FItems);
end;

// ===========================================================================
// AddItem overloads
// ===========================================================================

function TAiRAGVector.AddItem(aItem: TAiEmbeddingNode): string;
begin
  AddItem(aItem, nil);
  Result := aItem.Tag;
end;

function TAiRAGVector.AddItem(aItem: TAiEmbeddingNode;
    MetaData: TAiEmbeddingMetaData): NativeInt;
var
  Handled: Boolean;
begin
  Result  := -1;
  Handled := False;

  FLock.BeginWrite;
  try
    // Sincronizar metadatos externos
    if Assigned(MetaData) and (MetaData <> aItem.MetaData) then
      aItem.MetaData.Assign(MetaData);

    // Notificar al evento
    if Assigned(FOnDataVecAddItem) then
      FOnDataVecAddItem(Self, aItem, aItem.MetaData, Handled);

    if Handled then Exit;

    // Driver externo (sin salir — continuamos para poblar memoria local)
    if Assigned(FDriver) then
      FDriver.Add(aItem, FEntidad);

    // Almacenamiento en lista principal
    Result := FItems.Add(aItem);

    // Indexación léxica (BM25)
    if FSearchOptions.FUseBM25 then
      FBm25Index.AddNode(aItem);

    // Indexación vectorial
    if Assigned(FRagIndex) then
      FRagIndex.Add(aItem);

    // Actualizar metadatos del contenedor
    if FItems.Count = 1 then
    begin
      FModel := aItem.Model;
      FDim   := aItem.Dim;
    end;

  finally
    FLock.EndWrite;
  end;
end;

function TAiRAGVector.AddItem(aText: string;
    MetaData: TAiEmbeddingMetaData): TAiEmbeddingNode;
var
  vData     : TAiEmbeddingData;
  AddedIndex: NativeInt;
begin
  if Trim(aText) = '' then
    raise Exception.Create('El texto no puede estar vacío');

  if Assigned(FEmbeddings) then
    vData := FEmbeddings.CreateEmbedding(aText, 'user')
  else
    vData := DoOnGetEmbedding(aText, 'user');

  Result := TAiEmbeddingNode.Create(Length(vData));
  try
    Result.Text := aText;
    Result.Data := vData;

    if Assigned(FEmbeddings) then
      Result.Model := FEmbeddings.Model
    else
      Result.Model := FModel;

    if Assigned(MetaData) then
      Result.MetaData.Assign(MetaData);

    AddedIndex := Self.AddItem(Result, Result.MetaData);
    // Si AddedIndex = -1 el evento lo manejó; el llamador recibe el nodo
    if AddedIndex < -1 then ; // silenciar warning
  except
    Result.Free;
    raise;
  end;
end;

// ===========================================================================
// AddItemsFromJSonArray
// ===========================================================================

function TAiRAGVector.AddItemsFromJSonArray(aJSonArray: TJSONArray;
    MetaData: TAiEmbeddingMetaData): Integer;
var
  JVal       : TJSONData;
  Emb        : TAiEmbeddingNode;
  I, TotalCount: Integer;
  TextToEmbed: string;
  Cancel     : Boolean;
begin
  Result := 0;
  Cancel := False;
  I      := 0;

  if not Assigned(aJSonArray) then Exit;
  TotalCount := aJSonArray.Count;

  while I < TotalCount do
  begin
    JVal := aJSonArray.Items[I];

    if Assigned(FOnImportProgress) then
    begin
      FOnImportProgress(Self, I, TotalCount, Cancel);
      if Cancel then Break;
    end;

    if JVal is TJSONString then
      TextToEmbed := JVal.AsString
    else
      TextToEmbed := JVal.AsJSON;

    if TextToEmbed <> '' then
    begin
      Emb := AddItem(TextToEmbed, MetaData);
      if Assigned(Emb) then
      begin
        Emb.Orden := I;
        Inc(Result);
      end;
    end;

    Inc(I);
  end;

  if Assigned(FOnImportProgress) and (not Cancel) then
    FOnImportProgress(Self, TotalCount, TotalCount, Cancel);
end;

// ===========================================================================
// AddItemsFromPlainText
// ===========================================================================

function TAiRAGVector.AddItemsFromPlainText(aText: string;
    MetaData: TAiEmbeddingMetaData; aLenChunk: Integer;
    aOverlapPct: Integer): Integer;
const
  TOLERANCE_PCT = 0.15;
var
  TotalLen, StartPos, EndPos, NextStartPos: Integer;
  IdealEnd, MaxEnd, MinEnd, CutPos        : Integer;
  OverlapChars                            : Integer;
  ChunkText                               : string;
  Emb                                     : TAiEmbeddingNode;
  Idx                                     : Integer;
  Cancel                                  : Boolean;

  function FindSmartCut(LimitStart, LimitEnd: Integer): Integer;
  var
    K                              : Integer;
    LastPeriod, LastSpace, LastNewLine: Integer;
    C                              : Char;
  begin
    LastPeriod  := -1;
    LastSpace   := -1;
    LastNewLine := -1;

    for K := LimitEnd downto LimitStart do
    begin
      if K > TotalLen then Continue;
      C := aText[K];

      if (C = #10) or (C = #13) then begin Result := K; Exit; end;

      if (C in ['.', '?', '!', ';']) and (LastPeriod = -1) then
        LastPeriod := K;
      if (C = ' ') and (LastSpace = -1) then
        LastSpace := K;
    end;

    if LastPeriod <> -1 then Result := LastPeriod
    else if LastSpace <> -1 then Result := LastSpace
    else Result := -1;
  end;

begin
  Result := 0;
  Cancel := False;

  if aLenChunk <= 10 then
    raise Exception.Create('El tamaño del Chunk es demasiado pequeño.');
  if (aOverlapPct < 0) or (aOverlapPct >= 100) then
    raise Exception.Create('El porcentaje de Overlap debe estar entre 0 y 99.');

  aText    := Trim(aText);
  TotalLen := Length(aText);
  if TotalLen = 0 then Exit;

  OverlapChars := Round(aLenChunk * (aOverlapPct / 100));
  if OverlapChars >= aLenChunk then
    OverlapChars := aLenChunk - 1;

  StartPos := 1;
  Idx      := 0;

  while StartPos <= TotalLen do
  begin
    if Assigned(FOnImportProgress) then
    begin
      FOnImportProgress(Self, StartPos, TotalLen, Cancel);
      if Cancel then Break;
    end;

    IdealEnd := StartPos + aLenChunk;

    if IdealEnd > TotalLen then
    begin
      ChunkText := Copy(aText, StartPos, TotalLen - StartPos + 1);
      if ChunkText <> '' then
      begin
        MetaData.Properties['Posicion'] := Idx;
        Emb := AddItem(ChunkText, MetaData);
        if Assigned(Emb) then
        begin
          Emb.Orden := Idx;
          Inc(Result);
        end;
      end;
      Break;
    end;

    MinEnd := IdealEnd - Round(aLenChunk * TOLERANCE_PCT);
    MaxEnd := IdealEnd + Round(aLenChunk * TOLERANCE_PCT);
    if MaxEnd > TotalLen then MaxEnd := TotalLen;
    if MinEnd < StartPos  then MinEnd := StartPos;

    CutPos := FindSmartCut(MinEnd, MaxEnd);
    if CutPos = -1 then CutPos := IdealEnd;

    ChunkText := Trim(Copy(aText, StartPos, CutPos - StartPos + 1));
    if ChunkText <> '' then
    begin
      MetaData.Properties['Posicion'] := Idx;
      Emb := AddItem(ChunkText, MetaData);
      if Assigned(Emb) then
      begin
        Emb.Orden := Idx;
        Inc(Result);
      end;
      Inc(Idx);
    end;

    NextStartPos := (CutPos + 1) - OverlapChars;
    if NextStartPos <= StartPos then
      NextStartPos := StartPos + 1;

    if (NextStartPos > 1) and (NextStartPos < TotalLen) then
    begin
      while (NextStartPos > StartPos + 1) and
          (not (aText[NextStartPos - 1] in
                [' ', #13, #10, '.', ',', ';', ':', '!', '?'])) do
        Dec(NextStartPos);
    end;

    StartPos := NextStartPos;
  end;

  if Assigned(FOnImportProgress) and (not Cancel) then
    FOnImportProgress(Self, TotalLen, TotalLen, Cancel);
end;

// ===========================================================================
// Reordenamiento de contexto ABC (lost-in-the-middle mitigation)
// ===========================================================================

procedure TAiRAGVector.ApplyContextReordering(Vector: TAiRAGVector);
var
  ReorderedArr        : array of TAiEmbeddingNode;
  I, LeftIdx, RightIdx: Integer;
begin
  if (Vector = nil) or (Vector.Count <= 2) then Exit;

  SetLength(ReorderedArr, Vector.Count);
  LeftIdx  := 0;
  RightIdx := Vector.Count - 1;

  for I := 0 to Vector.Count - 1 do
  begin
    if (I mod 2 = 0) then
    begin
      ReorderedArr[LeftIdx] := Vector.Items[I];
      Inc(LeftIdx);
    end
    else
    begin
      ReorderedArr[RightIdx] := Vector.Items[I];
      Dec(RightIdx);
    end;
  end;

  Vector.Items.Clear;
  for I := 0 to High(ReorderedArr) do
    Vector.Items.Add(ReorderedArr[I]);
end;

// ===========================================================================
// Índices
// ===========================================================================

procedure TAiRAGVector.BuildIndex;
begin
  if not Assigned(FRagIndex) then
    raise Exception.Create('No existe un indice asignado');
  FRagIndex.BuildIndex(FItems);
  BuildLexicalIndex;
  FActive := True;
end;

procedure TAiRAGVector.BuildLexicalIndex;
var
  I: Integer;
begin
  if not Assigned(FBm25Index) then
    FBm25Index := TAIBm25Index.Create;

  FLock.BeginWrite;
  try
    FBm25Index.Clear;
    for I := 0 to FItems.Count - 1 do
      FBm25Index.AddNode(FItems[I]);
  finally
    FLock.EndWrite;
  end;
end;

procedure TAiRAGVector.CheckIndexes;
begin
  if not Assigned(FRagIndex) then
  begin
    case FInMemoryIndexType of
      TAIBasicIndex    : FRagIndex := TAIBasicEmbeddingIndex.Create;
      TAIHNSWIndex     : FRagIndex := THNSWIndex.Create;
      TAIEuclideanIndex: FRagIndex := TAIEuclideanDistanceIndex.Create;
    else
      FRagIndex := THNSWIndex.Create;
    end;
    if Assigned(FRagIndex) then
      FRagIndex.BuildIndex(FItems);
  end;

  if not Assigned(FBm25Index) then
    FBm25Index := TAIBm25Index.Create;
end;

// ===========================================================================
// Clear
// ===========================================================================

procedure TAiRAGVector.Clear;
var
  I: Integer;
begin
  FLock.BeginWrite;
  try
    if FOwnsObjects then
      for I := 0 to FItems.Count - 1 do
        FItems[I].Free;
    FItems.Clear;

    if Assigned(FBm25Index) then FBm25Index.Clear;

    if Assigned(FRagIndex) then
    begin
      FreeAndNil(FRagIndex);
      CheckIndexes;
    end;
  finally
    FLock.EndWrite;
  end;
end;

// ===========================================================================
// Connect (deprecated)
// ===========================================================================

function TAiRAGVector.Connect(aHost, aPort, aLogin, aPassword: string): Boolean;
begin
  if not Assigned(FRagIndex) then
    raise Exception.Create('No existe un indice asignado');
  FActive := True;
  Result  := True;
end;

// ===========================================================================
// Count / GetItems
// ===========================================================================

function TAiRAGVector.Count: Integer;
begin
  if Assigned(FItems) then Result := FItems.Count
  else Result := 0;
end;

function TAiRAGVector.GetItems: TRagItems;
begin
  Result := FItems;
end;

// ===========================================================================
// CreateEmbeddingNode
// ===========================================================================

function TAiRAGVector.CreateEmbeddingNode(aText: string;
    aEmbeddings: TAiEmbeddingsCore): TAiEmbeddingNode;
var
  Ar: TAiEmbeddingData;
begin
  if aEmbeddings = nil then aEmbeddings := FEmbeddings;
  if aEmbeddings = nil then
    raise Exception.Create('Debe especificar un modelo de embeddings primero');

  Ar     := aEmbeddings.CreateEmbedding(aText, 'user');
  Result := TAiEmbeddingNode.Create(1);
  Result.Text  := aText;
  Result.Data  := Ar;
  Result.Model := aEmbeddings.Model;
end;

// ===========================================================================
// DoOnGetEmbedding
// ===========================================================================

function TAiRAGVector.DoOnGetEmbedding(aInput, aUser: string;
    aDimensions: Integer; aModel, aEncodingFormat: string): TAiEmbeddingData;
begin
  Result := nil;
  if Assigned(FOnGetEmbedding) then
    FOnGetEmbedding(Self, aInput, aUser, aModel, aEncodingFormat,
        aDimensions, Result)
  else
    raise Exception.Create(
        'El evento OnGetEmbedding no ha sido asignado. '
        + 'No se puede generar el embedding.');
end;

// ===========================================================================
// FilterByCriteria / FilterByMetaData
// ===========================================================================

function TAiRAGVector.FilterByCriteria(
    const aCriteria: TAiFilterCriteria): TAiRAGVector;
var
  I    : Integer;
  Node : TAiEmbeddingNode;
  Inc_ : Boolean;
begin
  Result := TAiRAGVector.Create(nil, False);
  FLock.BeginRead;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      Node := FItems[I];
      Inc_ := True;

      if Assigned(FOnFilterItem) then
        FOnFilterItem(Self, Node, Inc_);

      if Inc_ and Assigned(aCriteria) and (aCriteria.Count > 0) then
        if not Node.MetaData.Matches(aCriteria) then
          Inc_ := False;

      if Inc_ then Result.Items.Add(Node);
    end;
  finally
    FLock.EndRead;
  end;
end;

function TAiRAGVector.FilterByMetaData(
    const aCriteria: TAiEmbeddingMetaData): TAiRAGVector;
var
  I, J  : Integer;
  Node  : TAiEmbeddingNode;
  Inc_  : Boolean;
begin
  Result := TAiRAGVector.Create(nil, False);
  FLock.BeginRead;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      Node := FItems[I];
      Inc_ := True;

      if Assigned(FOnFilterItem) then
        FOnFilterItem(Self, Node, Inc_);

      if Inc_ and Assigned(aCriteria) and (aCriteria.GetPropCount > 0) then
      begin
        for J := 0 to aCriteria.GetPropCount - 1 do
        begin
          if not Node.MetaData.Evaluate(aCriteria.GetPropKey(J),
              foEqual, aCriteria.GetPropValue(J)) then
          begin
            Inc_ := False;
            Break;
          end;
        end;
      end;

      if Inc_ then Result.Items.Add(Node);
    end;
  finally
    FLock.EndRead;
  end;
end;

// ===========================================================================
// Load / Save
// ===========================================================================

function TAiRAGVector.LoadFromFile(FileName: string): Integer;
var
  FS: TFileStream;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('El archivo no existe: %s', [FileName]);

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

function TAiRAGVector.LoadFromStream(Stream: TStream): Integer;
var
  ST          : TStringStream;
  JItem, JObj : TJSONObject;
  JArr        : TJSONArray;
  JTmp        : TJSONData;
  Emb         : TAiEmbeddingNode;
  JSONContent : string;
  I           : Integer;
begin
  Result := 0;
  if not Assigned(Stream) then Exit;

  ST := TStringStream.Create('');
  try
    Stream.Position := 0;
    ST.CopyFrom(Stream, 0);
    JSONContent := ST.DataString;
  finally
    ST.Free;
  end;

  JTmp := GetJSON(JSONContent);
  if not (JTmp is TJSONObject) then
  begin
    JTmp.Free;
    raise Exception.Create('El formato del archivo no es un objeto JSON válido.');
  end;

  JObj := TJSONObject(JTmp);
  try
    FLock.BeginWrite;
    try
      Self.Clear;

      JTmp := JObj.Find('name');
      if Assigned(JTmp) then FNameVec := JTmp.AsString;

      JTmp := JObj.Find('description');
      if Assigned(JTmp) then FDescription := JTmp.AsString;

      JTmp := JObj.Find('model');
      if Assigned(JTmp) then FModel := JTmp.AsString;

      JTmp := JObj.Find('dim');
      if Assigned(JTmp) then FDim := JTmp.AsInteger;

      JTmp := JObj.Find('data');
      if Assigned(JTmp) and (JTmp is TJSONArray) then
      begin
        JArr := TJSONArray(JTmp);
        for I := 0 to JArr.Count - 1 do
        begin
          if JArr.Items[I] is TJSONObject then
          begin
            JItem := TJSONObject(JArr.Items[I]);
            Emb   := TAiEmbeddingNode.FromJSON(JItem);
            FItems.Add(Emb);
            if FSearchOptions.FUseBM25 then
              FBm25Index.AddNode(Emb);
          end;
        end;
      end;

      if Assigned(FRagIndex) then
        FRagIndex.BuildIndex(FItems);

      Result := FItems.Count;
    finally
      FLock.EndWrite;
    end;
  finally
    JObj.Free;
  end;
end;

procedure TAiRAGVector.SaveToFile(FileName: string);
var
  ST: TMemoryStream;
begin
  ST := TMemoryStream.Create;
  try
    SaveToStream(ST);
    ST.SaveToFile(FileName);
  finally
    ST.Free;
  end;
end;

procedure TAiRAGVector.SaveToStream(Stream: TMemoryStream);
var
  Emb : TAiEmbeddingNode;
  I   : Integer;
  ST  : TStringStream;
  JArr: TJSONArray;
  JObj: TJSONObject;
begin
  if not Assigned(Stream) then
    raise Exception.Create(
        'El Stream de destino no puede ser nil en SaveToStream.');

  JObj := TJSONObject.Create;
  try
    JObj.Add('name',        FNameVec);
    JObj.Add('description', FDescription);
    JObj.Add('model',       FModel);
    JObj.Add('dim',         TJSONIntegerNumber.Create(FDim));

    JArr := TJSONArray.Create;
    JObj.Add('data', JArr);

    FLock.BeginRead;
    try
      for I := 0 to FItems.Count - 1 do
      begin
        Emb := FItems[I];
        JArr.Add(Emb.ToJSON);
      end;
    finally
      FLock.EndRead;
    end;

    ST := TStringStream.Create(JObj.AsJSON);
    try
      Stream.CopyFrom(ST, 0);
    finally
      ST.Free;
    end;
  finally
    JObj.Free;
  end;
end;

// ===========================================================================
// NormalizeResults (normalización Min-Max inplace)
// ===========================================================================

procedure TAiRAGVector.NormalizeResults(aList: TSearchResultList);
var
  MaxS, MinS, Range, S: Double;
  I : Integer;
  Rg: TAiSearchResult;
begin
  if aList.Count = 0 then Exit;

  MaxS := -1;
  MinS := 999999;
  for I := 0 to aList.Count - 1 do
  begin
    if aList[I].Score > MaxS then MaxS := aList[I].Score;
    if aList[I].Score < MinS then MinS := aList[I].Score;
  end;

  Range := MaxS - MinS;
  for I := 0 to aList.Count - 1 do
  begin
    Rg := aList[I];
    if Range > 0 then
      Rg.Score := (aList[I].Score - MinS) / Range
    else
      Rg.Score := 1.0;
    aList[I] := Rg;
    S := Rg.Score; if S < S then; // silenciar warning unused
  end;
end;

// ===========================================================================
// Notification
// ===========================================================================

procedure TAiRAGVector.Notification(AComponent: TComponent;
    Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FEmbeddings then FEmbeddings := nil;
    if AComponent = FDriver     then FDriver     := nil;
  end;
end;

// ===========================================================================
// Rerank
// ===========================================================================

procedure TAiRAGVector.Rerank(Target: TAiEmbeddingNode;
    aAutoRegenerate: Boolean);
var
  I  : Integer;
  Emb: TAiEmbeddingNode;

  procedure QSortNodes(L, R: Integer);
  const
    TOLERANCE = 1.0E-12;
  var
    I2, J: Integer;
    P : Double;
    Tmp: TAiEmbeddingNode;
  begin
    if L >= R then Exit;
    I2 := L; J := R;
    P  := FItems[(L + R) div 2].Idx;
    repeat
      while FItems[I2].Idx > P + TOLERANCE do Inc(I2);
      while FItems[J].Idx  < P - TOLERANCE do Dec(J);
      if I2 <= J then
      begin
        Tmp := FItems[I2]; FItems[I2] := FItems[J]; FItems[J] := Tmp;
        Inc(I2); Dec(J);
      end;
    until I2 > J;
    if L  < J  then QSortNodes(L, J);
    if I2 < R  then QSortNodes(I2, R);
  end;

begin
  if FItems.Count = 0 then Exit;

  if (FModel <> '') and (FModel <> Target.Model) then
  begin
    if aAutoRegenerate then
      RegenerateAll(Target.Model)
    else
      raise Exception.Create(
          'Rerank Error: El modelo del query no coincide '
          + 'y AutoRegenerate está desactivado.');
  end;

  for I := 0 to FItems.Count - 1 do
  begin
    Emb     := FItems[I];
    Emb.Idx := TAiEmbeddingNode.CosineSimilarity(Emb, Target);
  end;

  if FItems.Count > 1 then
    QSortNodes(0, FItems.Count - 1);
end;

procedure TAiRAGVector.Rerank(NewPrompt: string; aAutoRegenerate: Boolean);
var
  Target: TAiEmbeddingNode;
begin
  if (not Assigned(FEmbeddings)) and (not Assigned(FOnGetEmbedding)) then
    raise Exception.Create(
        'Rerank: No hay motor de embeddings configurado.');

  Target := CreateEmbeddingNode(NewPrompt);
  try
    Rerank(Target, aAutoRegenerate);
  finally
    Target.Free;
  end;
end;

// ===========================================================================
// RegenerateAll
// ===========================================================================

procedure TAiRAGVector.RegenerateAll(const aNewModel: string);
var
  I           : Integer;
  Emb         : TAiEmbeddingNode;
  NewModelName: string;
begin
  if FItems.Count = 0 then Exit;

  if aNewModel <> '' then
    NewModelName := aNewModel
  else if Assigned(FEmbeddings) then
    NewModelName := FEmbeddings.Model
  else
    NewModelName := FModel;

  for I := 0 to FItems.Count - 1 do
  begin
    Emb := FItems[I];
    if Emb.Text = '' then Continue;

    if Assigned(FEmbeddings) then
    begin
      if aNewModel <> '' then FEmbeddings.Model := aNewModel;
      Emb.Data := FEmbeddings.CreateEmbedding(Emb.Text, 'user');
    end
    else
      Emb.Data := DoOnGetEmbedding(Emb.Text, 'user', -1, NewModelName);

    Emb.Model := NewModelName;
    Emb.SetDataLength(Length(Emb.Data));
  end;

  FModel := NewModelName;
  if FItems.Count > 0 then FDim := FItems[0].Dim;

  BuildIndex;
end;

// ===========================================================================
// Search
// ===========================================================================

function TAiRAGVector.InternalSearchSafe(Target: TAiEmbeddingNode;
    aLimit: Integer; aPrecision: Double;
    aFilter: TAiFilterCriteria): TSearchResultList;
var
  I         : Integer;
  Emb       : TAiEmbeddingNode;
  Score     : Double;
  PassFilter: Boolean;
begin
  Result := TSearchResultList.Create;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      Emb := FItems[I];

      if Assigned(aFilter) and (aFilter.Count > 0) then
        if not Emb.MetaData.Matches(aFilter, Emb.Text) then Continue;

      if Assigned(FOnFilterItem) then
      begin
        PassFilter := True;
        FOnFilterItem(Self, Emb, PassFilter);
        if not PassFilter then Continue;
      end;

      Score := TAiEmbeddingNode.CosineSimilarity(Emb, Target);

      if (aPrecision > 0) and (Score < aPrecision) then Continue;

      Result.Add(TAiSearchResult.Create(Emb, Score));
    end;

    Result.SortDescending;
    Result.TrimToSize(aLimit);
  except
    Result.Free;
    raise;
  end;
end;

procedure TAiRAGVector.InternalNormalizeVector(aList: TSearchResultList);
var
  I : Integer;
  S : Double;
  SR: TAiSearchResult;
begin
  for I := 0 to aList.Count - 1 do
  begin
    SR := aList[I];
    S  := SR.Score;
    if S < 0 then S := 0;
    if S > 1 then S := 1;
    SR.Score  := S;
    aList[I]  := SR;
  end;
end;

procedure TAiRAGVector.InternalNormalizeLexical(aList: TBM25ResultList);
var
  MaxS    : Double;
  I       : Integer;
  NewScore: Double;
begin
  if aList.Count = 0 then Exit;

  MaxS := 0;
  for I := 0 to aList.Count - 1 do
    if aList[I].Key > MaxS then MaxS := aList[I].Key;

  for I := 0 to aList.Count - 1 do
  begin
    NewScore := aList[I].Key;
    if MaxS > 0 then NewScore := NewScore / MaxS;
    aList.SetKey(I, NewScore);
  end;
end;

// ---------------------------------------------------------------------------
// RRF — Reciprocal Rank Fusion
// ---------------------------------------------------------------------------

function TAiRAGVector.ReciprocalRankFusion(VectorResults: TSearchResultList;
    LexicalResults: TBM25ResultList; aLimit: Integer): TAiRAGVector;
const
  K_RRF = 60;
var
  ScoreArr  : array of TNodeAccum;
  ScoreCount: Integer;
  I, Idx    : Integer;
  Node      : TAiEmbeddingNode;
  RankScore  : Double;
  MaxRRF, MinRRF, RangeRRF: Double;
  Combined  : TBM25ResultList;
  P         : TNodeScorePair;
begin
  Result     := TAiRAGVector.Create(nil, False);
  ScoreCount := 0;
  SetLength(ScoreArr, 256);

  Combined := TBM25ResultList.Create;
  try
    // 1. Ranking vectorial
    if Assigned(VectorResults) then
    begin
      for I := 0 to VectorResults.Count - 1 do
      begin
        Node      := VectorResults[I].Node;
        RankScore := 1.0 / (K_RRF + (I + 1));
        Idx := AccumIndexOf(ScoreArr, ScoreCount, Node);
        if Idx >= 0 then
          ScoreArr[Idx].Score += RankScore
        else
        begin
          if ScoreCount >= Length(ScoreArr) then
            SetLength(ScoreArr, Length(ScoreArr) * 2);
          ScoreArr[ScoreCount].Node  := Node;
          ScoreArr[ScoreCount].Score := RankScore;
          Inc(ScoreCount);
        end;
      end;
    end;

    // 2. Ranking léxico (BM25)
    if Assigned(LexicalResults) then
    begin
      for I := 0 to LexicalResults.Count - 1 do
      begin
        Node      := LexicalResults[I].Value;
        RankScore := 1.0 / (K_RRF + (I + 1));
        Idx := AccumIndexOf(ScoreArr, ScoreCount, Node);
        if Idx >= 0 then
          ScoreArr[Idx].Score += RankScore
        else
        begin
          if ScoreCount >= Length(ScoreArr) then
            SetLength(ScoreArr, Length(ScoreArr) * 2);
          ScoreArr[ScoreCount].Node  := Node;
          ScoreArr[ScoreCount].Score := RankScore;
          Inc(ScoreCount);
        end;
      end;
    end;

    if ScoreCount = 0 then Exit;

    // 3. Min/Max del score RRF
    MaxRRF := -1;
    MinRRF := 999999;
    for I := 0 to ScoreCount - 1 do
    begin
      if ScoreArr[I].Score > MaxRRF then MaxRRF := ScoreArr[I].Score;
      if ScoreArr[I].Score < MinRRF then MinRRF := ScoreArr[I].Score;
    end;
    RangeRRF := MaxRRF - MinRRF;

    // 4. Ordenar descending
    if ScoreCount > 1 then
      QSortNodeAccumDesc(ScoreArr, 0, ScoreCount - 1);

    // 5. Volcar al resultado con normalización Min-Max
    for I := 0 to Min(aLimit * 2, ScoreCount) - 1 do
    begin
      Node := ScoreArr[I].Node;
      if RangeRRF > 0 then
        Node.Idx := (ScoreArr[I].Score - MinRRF) / RangeRRF
      else
        Node.Idx := 1.0;
      Result.Items.Add(Node);
    end;

  finally
    Combined.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Fusión ponderada
// ---------------------------------------------------------------------------

function TAiRAGVector.WeightedScoreFusion(VectorResults: TSearchResultList;
    LexicalResults: TBM25ResultList; aLimit: Integer): TAiRAGVector;
var
  ScoreArr  : array of TNodeAccum;
  ScoreCount: Integer;
  I, Idx    : Integer;
  Node      : TAiEmbeddingNode;
  W_Sem, W_Lex, TotalWeight, LexScore: Double;
  Combined  : TBM25ResultList;
  P         : TNodeScorePair;
begin
  Result     := TAiRAGVector.Create(nil, False);
  ScoreCount := 0;
  SetLength(ScoreArr, 256);

  TotalWeight := FSearchOptions.EmbeddingWeight + FSearchOptions.BM25Weight;
  if TotalWeight > 0 then
  begin
    W_Sem := FSearchOptions.EmbeddingWeight / TotalWeight;
    W_Lex := FSearchOptions.BM25Weight      / TotalWeight;
  end
  else
  begin
    W_Sem := 0.5;
    W_Lex := 0.5;
  end;

  Combined := TBM25ResultList.Create;
  try
    // A. Canal vectorial (semántico)
    if Assigned(VectorResults) then
    begin
      for I := 0 to VectorResults.Count - 1 do
      begin
        Node := VectorResults[I].Node;
        Idx  := AccumIndexOf(ScoreArr, ScoreCount, Node);
        if Idx >= 0 then
          ScoreArr[Idx].Score += VectorResults[I].Score * W_Sem
        else
        begin
          if ScoreCount >= Length(ScoreArr) then
            SetLength(ScoreArr, Length(ScoreArr) * 2);
          ScoreArr[ScoreCount].Node  := Node;
          ScoreArr[ScoreCount].Score := VectorResults[I].Score * W_Sem;
          Inc(ScoreCount);
        end;
      end;
    end;

    // B. Canal léxico (BM25)
    if Assigned(LexicalResults) then
    begin
      for I := 0 to LexicalResults.Count - 1 do
      begin
        Node     := LexicalResults[I].Value;
        LexScore := LexicalResults[I].Key * W_Lex;
        Idx := AccumIndexOf(ScoreArr, ScoreCount, Node);
        if Idx >= 0 then
          ScoreArr[Idx].Score += LexScore
        else
        begin
          if ScoreCount >= Length(ScoreArr) then
            SetLength(ScoreArr, Length(ScoreArr) * 2);
          ScoreArr[ScoreCount].Node  := Node;
          ScoreArr[ScoreCount].Score := LexScore;
          Inc(ScoreCount);
        end;
      end;
    end;

    // Ordenar descending
    if ScoreCount > 1 then
      QSortNodeAccumDesc(ScoreArr, 0, ScoreCount - 1);

    // Volcar al resultado
    for I := 0 to Min(aLimit, ScoreCount) - 1 do
    begin
      Node     := ScoreArr[I].Node;
      Node.Idx := ScoreArr[I].Score;
      Result.Items.Add(Node);
    end;

  finally
    Combined.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Search(Target) — búsqueda principal
// ---------------------------------------------------------------------------

function TAiRAGVector.Search(Target: TAiEmbeddingNode; aLimit: Integer;
    aPrecision: Double; aFilter: TAiFilterCriteria): TAiRAGVector;
var
  Handled        : Boolean;
  VectorResults  : TSearchResultList;
  LexicalRes     : TBM25ResultList;
  NewLex         : TBM25ResultList;
  RawIndexResults: TRagItems;
  SearchRes      : TAiSearchResult;
  LexPair        : TNodeScorePair;
  I              : Integer;
  Node           : TAiEmbeddingNode;
begin
  Result  := nil;
  Handled := False;

  if FSearchOptions.UseEmbeddings and (Target.MagnitudeValue <= 0) then
  begin
    if not FSearchOptions.UseBM25 then
      raise Exception.Create(
          'Error: La consulta no generó un vector válido. '
          + 'Verifique su motor de embeddings.');
  end;

  // FASE 1: Evento externo
  if Assigned(FOnDataVecSearch) then
  begin
    FOnDataVecSearch(Self, Target, Target.Text, aLimit, aPrecision, aFilter,
        Result, Handled);
    if Handled then Exit;
  end;

  // FASE 2: Driver externo
  if Assigned(FDriver) then
  begin
    Result := FDriver.Search(Target, FEntidad, aLimit, aPrecision, aFilter,
        FSearchOptions);
    Exit;
  end;

  // FASE 3: Búsqueda en memoria
  FLock.BeginRead;
  try
    VectorResults := nil;
    LexicalRes    := nil;
    try
      // --- A: Búsqueda vectorial ---
      if FSearchOptions.UseEmbeddings then
      begin
        if not Assigned(FRagIndex) then
          raise Exception.Create('No existe un índice asignado');

        if (FModel <> '') and (Target.Model <> '') and
            (FModel <> Target.Model) then
          raise Exception.CreateFmt(
              'Error de Modelo: El vector usa "%s" pero el Query usa "%s"',
              [FModel, Target.Model]);

        if (Assigned(aFilter) and (aFilter.Count > 0)) or
            Assigned(FOnFilterItem) then
        begin
          VectorResults := InternalSearchSafe(Target,
              Max(50, aLimit * 5),
              FSearchOptions.MinAbsoluteScoreEmbedding, aFilter);
        end
        else
        begin
          VectorResults    := TSearchResultList.Create;
          RawIndexResults  := FRagIndex.Search(Target,
              Max(50, aLimit * 5),
              FSearchOptions.MinAbsoluteScoreEmbedding);
          try
            if Assigned(RawIndexResults) then
              for I := 0 to RawIndexResults.Count - 1 do
              begin
                Node := RawIndexResults[I];
                VectorResults.Add(TAiSearchResult.Create(Node, Node.Idx));
              end;
          finally
            RawIndexResults.Free;
          end;
        end;
      end;

      // --- B: Búsqueda léxica (BM25) ---
      if FSearchOptions.UseBM25 and (Trim(Target.Text) <> '') then
      begin
        LexicalRes := FBm25Index.Search(Target.Text,
            Max(50, aLimit * 5), aFilter);

        // Filtrar resultados BM25 por umbral mínimo (rebuild sin Delete)
        NewLex := TBM25ResultList.Create;
        for I := 0 to LexicalRes.Count - 1 do
          if LexicalRes[I].Key >= FSearchOptions.MinAbsoluteScoreBM25 then
            NewLex.Add(LexicalRes[I]);
        LexicalRes.Free;
        LexicalRes := NewLex;

        InternalNormalizeLexical(LexicalRes);
      end;

      // --- C: Fusión ---
      if Assigned(VectorResults) and not Assigned(LexicalRes) then
      begin
        Result := TAiRAGVector.Create(nil, False);
        for I := 0 to VectorResults.Count - 1 do
        begin
          SearchRes := VectorResults[I];
          SearchRes.Node.Idx := SearchRes.Score;
          Result.Items.Add(SearchRes.Node);
        end;
      end
      else if (not Assigned(VectorResults)) and Assigned(LexicalRes) then
      begin
        Result := TAiRAGVector.Create(nil, False);
        for I := 0 to LexicalRes.Count - 1 do
        begin
          LexPair := LexicalRes[I];
          LexPair.Value.Idx := LexPair.Key;
          Result.Items.Add(LexPair.Value);
        end;
      end
      else if Assigned(VectorResults) and Assigned(LexicalRes) then
      begin
        if FSearchOptions.UseRRF then
          Result := ReciprocalRankFusion(VectorResults, LexicalRes, aLimit)
        else
          Result := WeightedScoreFusion(VectorResults, LexicalRes, aLimit);
      end
      else
        Result := TAiRAGVector.Create(nil, False);

      // --- D: Filtro final y post-procesado ---
      if Assigned(Result) then
      begin
        if aPrecision > 0 then
          for I := Result.Count - 1 downto 0 do
            if Result.Items[I].Idx < aPrecision then
              Result.Items.Delete(I);

        while Result.Count > aLimit do
          Result.Items.Delete(Result.Count - 1);

        Result.FModel := Self.Model;
        Result.FDim   := Self.Dim;

        if FSearchOptions.UseReorderABC and (Result.Count > 2) then
          ApplyContextReordering(Result);
      end;

    finally
      if Assigned(VectorResults) then VectorResults.Free;
      if Assigned(LexicalRes)    then LexicalRes.Free;
    end;
  finally
    FLock.EndRead;
  end;
end;

// ---------------------------------------------------------------------------
// Search(Prompt: string)
// ---------------------------------------------------------------------------

function TAiRAGVector.Search(Prompt: string; aLimit: Integer;
    aPrecision: Double; aFilter: TAiFilterCriteria): TAiRAGVector;
var
  Target: TAiEmbeddingNode;
begin
  if (not Assigned(FEmbeddings)) and (not Assigned(FOnGetEmbedding)) then
    raise Exception.Create(
        'Error: No hay motor de embeddings configurado. '
        + 'Asigne la propiedad Embeddings o el evento OnGetEmbedding.');

  Target := CreateEmbeddingNode(Prompt);
  try
    Result := Search(Target, aLimit, aPrecision, aFilter);
  finally
    Target.Free;
  end;
end;

// ---------------------------------------------------------------------------
// SearchText
// ---------------------------------------------------------------------------

function TAiRAGVector.SearchText(aPrompt: string; aLimit: Integer;
    aPrecision: Double; aFilter: TAiFilterCriteria;
    IncludeMetadata, IncludeScore: Boolean): string;
var
  TmpVec: TAiRAGVector;
begin
  TmpVec := Search(aPrompt, aLimit, aPrecision, aFilter);
  try
    Result := VectorToContextText(TmpVec, IncludeMetadata, IncludeScore);
  finally
    TmpVec.Free;
  end;
end;

function TAiRAGVector.SearchText(aPrompt: TAiEmbeddingNode; aLimit: Integer;
    aPresicion: Double; aFilter: TAiFilterCriteria;
    IncludeMetadata, IncludeScore: Boolean): string;
var
  TmpVec: TAiRAGVector;
begin
  TmpVec := Search(aPrompt, aLimit, aPresicion, aFilter);
  try
    Result := VectorToContextText(TmpVec, IncludeMetadata, IncludeScore);
  finally
    TmpVec.Free;
  end;
end;

// ===========================================================================
// VectorToContextText
// ===========================================================================

function TAiRAGVector.VectorToContextText(DataVec: TAiRAGVector;
    IncludeMetadata: Boolean; IncludeScore: Boolean): string;
var
  I, J            : Integer;
  Emb             : TAiEmbeddingNode;
  SResult         : string;
  HasContentBefore: Boolean;
begin
  if (DataVec = nil) or (DataVec.Count = 0) then begin Result := ''; Exit; end;

  SResult := '';

  for I := 0 to DataVec.Count - 1 do
  begin
    Emb              := DataVec.Items[I];
    HasContentBefore := False;

    if IncludeMetadata or IncludeScore then
    begin
      SResult += '[';

      if IncludeScore then
      begin
        SResult += Format('Score: %.4f', [Emb.Idx]);
        HasContentBefore := True;
      end;

      if IncludeMetadata then
      begin
        if HasContentBefore then SResult += ' | ';

        if Emb.Tag <> '' then
        begin
          SResult += 'ID: ' + Emb.Tag;
          HasContentBefore := True;
        end;

        if Assigned(Emb.MetaData) then
        begin
          for J := 0 to Emb.MetaData.GetPropCount - 1 do
          begin
            if HasContentBefore then SResult += ' | ';
            SResult += Emb.MetaData.GetPropKey(J) + ': '
                     + VarToStr(Emb.MetaData.GetPropValue(J));
            HasContentBefore := True;
          end;
        end;
      end;

      SResult += ']' + LineEnding;
    end;

    SResult += Trim(Emb.Text) + LineEnding + LineEnding;
  end;

  Result := SResult;
end;

// ===========================================================================
// ExecuteVGQL
// ===========================================================================

function TAiRAGVector.ExecuteVGQL(const AVgqlQuery: string;
    out AResultVector: TAiRAGVector): string;
var
  Parser     : TVGQLParser;
  AST        : TVGQLQuery;
  Compiler   : TVGQLCompiler;
  Req        : TVGQLRequest;
  TempOptions: TAiSearchOptions;
begin
  Result       := '';
  AResultVector := nil;
  AST          := nil;
  Req          := nil;

  // 1. PARSEO
  Parser := TVGQLParser.Create(AVgqlQuery);
  try
    AST := Parser.Parse;
  finally
    Parser.Free;
  end;

  try
    // 2. COMPILACIÓN
    Compiler := TVGQLCompiler.Create;
    try
      Req := Compiler.Translate(AST);
    finally
      Compiler.Free;
      if Assigned(AST) then FreeAndNil(AST);
    end;

    if not Assigned(Req) then Exit;

    // 3. CONFIGURACIÓN DINÁMICA
    TempOptions := TAiSearchOptions.Create;
    try
      TempOptions.UseEmbeddings :=
          (Req.Mode = smEmbeddings) or (Req.Mode = smHybrid);
      TempOptions.UseBM25 :=
          (Req.Mode = smBM25) or (Req.Mode = smHybrid);
      TempOptions.EmbeddingWeight := Req.WeightSemantic;
      TempOptions.BM25Weight      := Req.WeightLexical;
      TempOptions.UseRRF          := (Req.Fusion = fmRRF);
      TempOptions.MinAbsoluteScoreEmbedding := Req.MinSemantic;
      TempOptions.MinAbsoluteScoreBM25      := Req.MinLexical;
      TempOptions.UseReorderABC  := Req.UseReorderABC;

      Self.SearchOptions.Assign(TempOptions);

      // Sincronizar idioma BM25
      if Req.Language <> '' then
      begin
        if SameText(Req.Language, 'spanish') then
          Self.LexicalLanguage := alSpanish
        else if SameText(Req.Language, 'english') then
          Self.LexicalLanguage := alEnglish
        else if SameText(Req.Language, 'portuguese') then
          Self.LexicalLanguage := alPortuguese;
      end;

      // 4. BÚSQUEDA PRINCIPAL
      AResultVector := Self.Search(Req.Query, Req.Limit,
          Req.MinGlobal, Req.Filter);

      // 5. RERANK (opcional)
      if Assigned(AResultVector) and (Req.RerankQuery <> '')
          and (AResultVector.Count > 0) then
      begin
        AResultVector.Embeddings := Self.Embeddings;
        AResultVector.RegenerateAll;
        AResultVector.BuildIndex;
        AResultVector.Rerank(Req.RerankQuery, Req.RerankRegenerate);
      end;

      // 6. REORDENAMIENTO DE CONTEXTO
      if Assigned(AResultVector) and Req.UseReorderABC then
        ApplyContextReordering(AResultVector);

      // 7. OUTPUT PARA LLM
      if Assigned(AResultVector) then
        Result := VectorToContextText(AResultVector,
            Req.IncludeMetadata, Req.IncludeScore);

    finally
      TempOptions.Free;
    end;
  finally
    if Assigned(Req) then Req.Free;
  end;
end;

function TAiRAGVector.ExecuteVGQL(const AVgqlQuery: string): string;
var
  TempVec: TAiRAGVector;
begin
  Result := ExecuteVGQL(AVgqlQuery, TempVec);
  if Assigned(TempVec) then TempVec.Free;
end;

// ===========================================================================
// Setters
// ===========================================================================

procedure TAiRAGVector.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TAiRAGVector.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TAiRAGVector.SetDriver(const Value: TAiVectorStoreDriverBase);
begin
  if FDriver <> Value then
  begin
    FDriver := Value;
    if Assigned(FDriver) then FDriver.FreeNotification(Self);
  end;
end;

procedure TAiRAGVector.SetEmbeddings(const Value: TAiEmbeddingsCore);
begin
  if FEmbeddings <> Value then
  begin
    FEmbeddings := Value;
    if Assigned(FEmbeddings) then FEmbeddings.FreeNotification(Self);

    if (csDesigning in ComponentState) then Exit;

    if Assigned(Value) then
    begin
      FModel := Value.Model;
      FDim   := Value.Dimensions;
    end;
  end;
end;

procedure TAiRAGVector.SetInMemoryIndexType(const Value: TAiRagIndexType);
begin
  if FInMemoryIndexType = Value then Exit;
  FInMemoryIndexType := Value;

  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  FLock.BeginWrite;
  try
    if Assigned(FRagIndex) then FreeAndNil(FRagIndex);
    CheckIndexes;
  finally
    FLock.EndWrite;
  end;
end;

procedure TAiRAGVector.SetLexicalLanguage(const Value: TAiLanguage);
begin
  if Assigned(FBm25Index) then FBm25Index.Language := Value;
end;

function TAiRAGVector.GetLexicalLanguage: TAiLanguage;
begin
  if Assigned(FBm25Index) then Result := FBm25Index.Language
  else Result := alSpanish;
end;

procedure TAiRAGVector.SetNameVec(const Value: string);
begin
  FNameVec := Value;
end;

procedure TAiRAGVector.SetOnDataVecAddItem(const Value: TOnDataVecAddItem);
begin
  FOnDataVecAddItem := Value;
end;

procedure TAiRAGVector.SetOnDataVecSearch(const Value: TOnDataVecSearch);
begin
  FOnDataVecSearch := Value;
end;

procedure TAiRAGVector.SetRagIndex(const Value: TAIEmbeddingIndex);
begin
  FRagIndex := Value;
end;

procedure TAiRAGVector.SetSearchOptions(const Value: TAiSearchOptions);
begin
  FSearchOptions := Value;
end;

end.
