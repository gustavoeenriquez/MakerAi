// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.RAG.Vectors.Index
// Índices de embeddings: básico (coseno), euclidiano y HNSW. BM25 léxico.
unit uMakerAi.RAG.Vectors.Index;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, Math,
  fgl,
  fpjson, jsonparser,
  uMakerAi.Embeddings.Core, uMakerAi.RAG.MetaData;

type
  TAiRagIndexType = (TAIBasicIndex, TAIHNSWIndex, TAIEuclideanIndex);

  // Forward para procedural type
  TAiEmbeddingNode = class;
  TOnFilterItem    = procedure(Sender: TObject; const aNode: TAiEmbeddingNode;
                               var aInclude: Boolean) of object;

  // Listas genéricas (fgl — no owning)
  TRagItems = specialize TFPGList<TAiEmbeddingNode>;
  TIntList  = specialize TFPGList<Integer>;

  // ---------------------------------------------------------------------------
  // TAiEmbeddingNode
  // ---------------------------------------------------------------------------
  TAiEmbeddingNode = class
  private
    FData      : TAiEmbeddingData;
    FDim       : Integer;
    FText      : string;
    FIdx       : Double;
    FModel     : string;
    FMetaData  : TAiEmbeddingMetaData;
    FTag       : string;
    FTagObject : TObject;
    FOrden     : Integer;
    FMagnitude : Double;
    FDocLength : Integer; // usado por TAIBm25Index
    procedure SetData(const Value: TAiEmbeddingData);
  public
    constructor Create(aDim: Integer);
    destructor  Destroy; override;

    class function CosineSimilarity(const A, B: TAiEmbeddingNode): Double;
    class function DotProduct(const A, B: TAiEmbeddingNode): Double;
    class function Magnitude(const A: TAiEmbeddingNode): Double;

    function  ToJSON: TJSONObject;
    class function FromJSON(AJSONObject: TJSONObject): TAiEmbeddingNode;
    function  ToJsonArray: TJSONArray; overload;
    class function ToJsonArray(Val: TAiEmbeddingNode): TJSONArray; overload;

    procedure SetDataLength(aDim: Integer);

    property Data          : TAiEmbeddingData     read FData      write SetData;
    property Text          : string               read FText      write FText;
    property Idx           : Double               read FIdx       write FIdx;
    property Model         : string               read FModel     write FModel;
    property MetaData      : TAiEmbeddingMetaData read FMetaData;
    property Tag           : string               read FTag       write FTag;
    property TagObject     : TObject              read FTagObject write FTagObject;
    property Orden         : Integer              read FOrden     write FOrden;
    property Dim           : Integer              read FDim;
    property MagnitudeValue: Double               read FMagnitude;
    property DocLength     : Integer              read FDocLength write FDocLength;
  end;

  // ---------------------------------------------------------------------------
  // TAiSearchResult — resultado thread-safe (no modifica nodos originales)
  // ---------------------------------------------------------------------------
  TAiSearchResult = record
    Node  : TAiEmbeddingNode;
    FScore: Double;
    class function Create(aNode: TAiEmbeddingNode;
                          aScore: Double): TAiSearchResult; static;
    class function CompareDescending(const Left,
                                     Right: TAiSearchResult): Integer; static;
    property Score: Double read FScore write FScore;
  end;

  TSearchResultList = class
  private
    FItems: array of TAiSearchResult;
    FCount: Integer;
    function  GetItem(I: Integer): TAiSearchResult;
    procedure SetItem(I: Integer; const V: TAiSearchResult);
    procedure Grow;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Add(const R: TAiSearchResult): Integer;
    procedure Delete(I: Integer);
    function  Last: TAiSearchResult;
    procedure SortDescending;
    procedure TrimToSize(MaxCount: Integer);
    property Count: Integer read FCount;
    property Items[I: Integer]: TAiSearchResult read GetItem write SetItem; default;
  end;

  // ---------------------------------------------------------------------------
  // Índice base abstracto
  // ---------------------------------------------------------------------------
  TAIEmbeddingIndex = class
  private
    FDataVec: TRagItems;
    FActive : Boolean;
  protected
    procedure InternalClear; virtual; abstract;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure BuildIndex(Points: TRagItems); virtual;
    function  Add(Point: TAiEmbeddingNode): Integer; virtual;
    function  Search(Target: TAiEmbeddingNode; aLimit: Integer;
                     aPrecision: Double): TRagItems; virtual;
    procedure Clear; virtual;

    property DataVec: TRagItems read FDataVec write FDataVec;
    property Active : Boolean   read FActive;
  end;

  // ---------------------------------------------------------------------------
  // TAIBasicEmbeddingIndex — coseno O(n)
  // ---------------------------------------------------------------------------
  TAIBasicEmbeddingIndex = class(TAIEmbeddingIndex)
  protected
    procedure InternalClear; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure BuildIndex(Points: TRagItems); override;
    function  Search(Target: TAiEmbeddingNode; aLimit: Integer;
                     aPrecision: Double): TRagItems; override;

    class function InternalSearchSafe(Target: TAiEmbeddingNode; aLimit: Integer;
        aPrecision: Double; Source: TRagItems): TSearchResultList; static;
    class function InternalSearch(Target: TAiEmbeddingNode; aLimit: Integer;
        aPrecision: Double; Source: TRagItems): TRagItems; static;
  end;

  // ---------------------------------------------------------------------------
  // TAIEuclideanDistanceIndex
  // ---------------------------------------------------------------------------
  TL2Pair = record
    Key : Double;
    Node: TAiEmbeddingNode;
  end;

  TAIEuclideanDistanceIndex = class(TAIEmbeddingIndex)
  protected
    procedure InternalClear; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure BuildIndex(Points: TRagItems); override;
    function  Search(Target: TAiEmbeddingNode; aLimit: Integer;
                     aPrecision: Double): TRagItems; override;
  end;

  // ---------------------------------------------------------------------------
  // HNSW
  // ---------------------------------------------------------------------------
  THNSWNode = class
  private
    FID         : Integer;
    FVector     : TAiEmbeddingNode;
    FConnections: array of TIntList;
  public
    constructor Create(aID: Integer; aVector: TAiEmbeddingNode; aNumLevels: Integer);
    destructor  Destroy; override;
    function GetConnections(Level: Integer): TIntList;
    function LevelCount: Integer;
    property ID    : Integer          read FID;
    property Vector: TAiEmbeddingNode read FVector;
  end;

  TDoubleIntPair = record
    Key  : Double;
    Value: Integer;
  end;

  TDoubleIntPairList = class
  private
    FItems: array of TDoubleIntPair;
    FCount: Integer;
    function  GetItem(I: Integer): TDoubleIntPair;
    procedure Grow;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const P: TDoubleIntPair);
    procedure Delete(I: Integer);
    function  Last: TDoubleIntPair;
    procedure SortDescending;
    property Count: Integer read FCount;
    property Items[I: Integer]: TDoubleIntPair read GetItem; default;
  end;

  THNSWIndex = class(TAIEmbeddingIndex)
  private
    FNodes         : array of THNSWNode; // indexado por NodeID (0..FNodeCount-1)
    FNodeCount     : Integer;
    FEntryPoint    : Integer;
    FEntryLevel    : Integer;
    FMaxLevel      : Integer;
    FLevelMult     : Double;
    FEfConstruction: Integer;
    FMaxConnections: Integer;

    function GetRandomLevel: Integer;
    procedure InsertConnection(Node: THNSWNode; Level, TargetID: Integer);
    function SearchLayer(Query: TAiEmbeddingNode;
                         EntryPoint, Level, Ef: Integer): TIntList;
  protected
    procedure InternalClear; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure BuildIndex(Points: TRagItems); override;
    function  Add(Point: TAiEmbeddingNode): Integer; override;
    function  Search(Target: TAiEmbeddingNode; aLimit: Integer;
                     aPrecision: Double): TRagItems; override;
    procedure Clear; override;
  end;

  // ---------------------------------------------------------------------------
  // BM25
  // ---------------------------------------------------------------------------
  TWordOccurrence = record
    Node : TAiEmbeddingNode;
    Count: Integer;
  end;

  TWordOccurrenceList = class
  private
    FItems: array of TWordOccurrence;
    FCount: Integer;
    function GetItem(I: Integer): TWordOccurrence;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const Occ: TWordOccurrence);
    property Count: Integer read FCount;
    property Items[I: Integer]: TWordOccurrence read GetItem; default;
  end;

  TStringHashSet = class
  private
    FList: TStringList;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const S: string);
    function  Contains(const S: string): Boolean;
    procedure Clear;
    property  List: TStringList read FList;
  end;

  TNodeScorePair = record
    Key  : Double;
    Value: TAiEmbeddingNode;
  end;

  TBM25ResultList = class
  private
    FItems: array of TNodeScorePair;
    FCount: Integer;
    function GetItem(I: Integer): TNodeScorePair;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(const P: TNodeScorePair);
    procedure SortDescending;
    procedure TrimToSize(MaxCount: Integer);
    property Count: Integer read FCount;
    procedure SetKey(I: Integer; NewKey: Double);
    property Items[I: Integer]: TNodeScorePair read GetItem; default;
  end;

  TAIBm25Index = class
  private
    FInvertedIndex: TStringList;  // Sorted; Objects[i] = TWordOccurrenceList
    FAvgDocLength : Double;
    FDocCount     : Integer;
    FLanguage     : TAiLanguage;
    FStopWords    : TStringHashSet;
    procedure SetLanguage(const Value: TAiLanguage);
    procedure LoadDefaultStopWords(Lang: TAiLanguage);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddNode(aNode: TAiEmbeddingNode);
    function  Search(const aQuery: string; aLimit: Integer;
                     aFilter: TAiFilterCriteria = nil): TBM25ResultList;
    procedure Clear;
    procedure Tokenize(const aText: string; aList: TStrings);

    property Language : TAiLanguage    read FLanguage  write SetLanguage;
    property StopWords: TStringHashSet read FStopWords;
  end;

function CompareEmbeddings(const Left, Right: TAiEmbeddingNode;
                           Axis: Integer): Integer;

implementation

// ============================================================================
// Tipos privados de implementación
// ============================================================================
type
  TNodeAccum = record
    Node : TAiEmbeddingNode;
    Score: Double;
  end;

// ============================================================================
// Helpers de ordenamiento (QuickSort sobre arrays dinámicos)
// ============================================================================

procedure QSortSearchResultsDesc(var A: array of TAiSearchResult; L, R: Integer);
var
  I, J: Integer;
  P   : Double;
  Tmp : TAiSearchResult;
begin
  if L >= R then Exit;
  I := L; J := R;
  P := A[(L + R) div 2].FScore;
  repeat
    while A[I].FScore > P do Inc(I);
    while A[J].FScore < P do Dec(J);
    if I <= J then begin
      Tmp := A[I]; A[I] := A[J]; A[J] := Tmp;
      Inc(I); Dec(J);
    end;
  until I > J;
  if L < J then QSortSearchResultsDesc(A, L, J);
  if I < R then QSortSearchResultsDesc(A, I, R);
end;

procedure QSortDoubleIntDesc(var A: array of TDoubleIntPair; L, R: Integer);
var
  I, J: Integer;
  P   : Double;
  Tmp : TDoubleIntPair;
begin
  if L >= R then Exit;
  I := L; J := R;
  P := A[(L + R) div 2].Key;
  repeat
    while A[I].Key > P do Inc(I);
    while A[J].Key < P do Dec(J);
    if I <= J then begin
      Tmp := A[I]; A[I] := A[J]; A[J] := Tmp;
      Inc(I); Dec(J);
    end;
  until I > J;
  if L < J then QSortDoubleIntDesc(A, L, J);
  if I < R then QSortDoubleIntDesc(A, I, R);
end;

procedure QSortL2PairsAsc(var A: array of TL2Pair; L, R: Integer);
var
  I, J: Integer;
  P   : Double;
  Tmp : TL2Pair;
begin
  if L >= R then Exit;
  I := L; J := R;
  P := A[(L + R) div 2].Key;
  repeat
    while A[I].Key < P do Inc(I);
    while A[J].Key > P do Dec(J);
    if I <= J then begin
      Tmp := A[I]; A[I] := A[J]; A[J] := Tmp;
      Inc(I); Dec(J);
    end;
  until I > J;
  if L < J then QSortL2PairsAsc(A, L, J);
  if I < R then QSortL2PairsAsc(A, I, R);
end;

procedure QSortNodeScoresDesc(var A: array of TNodeScorePair; L, R: Integer);
var
  I, J: Integer;
  P   : Double;
  Tmp : TNodeScorePair;
begin
  if L >= R then Exit;
  I := L; J := R;
  P := A[(L + R) div 2].Key;
  repeat
    while A[I].Key > P do Inc(I);
    while A[J].Key < P do Dec(J);
    if I <= J then begin
      Tmp := A[I]; A[I] := A[J]; A[J] := Tmp;
      Inc(I); Dec(J);
    end;
  until I > J;
  if L < J then QSortNodeScoresDesc(A, L, J);
  if I < R then QSortNodeScoresDesc(A, I, R);
end;

procedure SplitCSV(const S: string; AList: TStrings);
var
  I, Start: Integer;
begin
  AList.Clear;
  if S = '' then Exit;
  Start := 1;
  for I := 1 to Length(S) do
    if S[I] = ',' then begin
      if I > Start then AList.Add(Copy(S, Start, I - Start));
      Start := I + 1;
    end;
  if Start <= Length(S) then AList.Add(Copy(S, Start, MaxInt));
end;

// ============================================================================
// TSearchResultList
// ============================================================================

constructor TSearchResultList.Create;
begin
  inherited;
  FCount := 0;
  SetLength(FItems, 16);
end;

destructor TSearchResultList.Destroy;
begin
  SetLength(FItems, 0);
  inherited;
end;

function TSearchResultList.GetItem(I: Integer): TAiSearchResult;
begin
  Result := FItems[I];
end;

procedure TSearchResultList.SetItem(I: Integer; const V: TAiSearchResult);
begin
  FItems[I] := V;
end;

procedure TSearchResultList.Grow;
begin
  SetLength(FItems, Length(FItems) * 2 + 16);
end;

function TSearchResultList.Add(const R: TAiSearchResult): Integer;
begin
  if FCount >= Length(FItems) then Grow;
  FItems[FCount] := R;
  Inc(FCount);
  Result := FCount - 1;
end;

procedure TSearchResultList.Delete(I: Integer);
var
  J: Integer;
begin
  for J := I to FCount - 2 do
    FItems[J] := FItems[J + 1];
  Dec(FCount);
end;

function TSearchResultList.Last: TAiSearchResult;
begin
  Result := FItems[FCount - 1];
end;

procedure TSearchResultList.SortDescending;
begin
  if FCount > 1 then
    QSortSearchResultsDesc(FItems, 0, FCount - 1);
end;

procedure TSearchResultList.TrimToSize(MaxCount: Integer);
begin
  if FCount > MaxCount then FCount := MaxCount;
end;

// ============================================================================
// TDoubleIntPairList
// ============================================================================

constructor TDoubleIntPairList.Create;
begin
  inherited;
  FCount := 0;
  SetLength(FItems, 8);
end;

destructor TDoubleIntPairList.Destroy;
begin
  SetLength(FItems, 0);
  inherited;
end;

function TDoubleIntPairList.GetItem(I: Integer): TDoubleIntPair;
begin
  Result := FItems[I];
end;

procedure TDoubleIntPairList.Grow;
begin
  SetLength(FItems, Length(FItems) * 2 + 8);
end;

procedure TDoubleIntPairList.Add(const P: TDoubleIntPair);
begin
  if FCount >= Length(FItems) then Grow;
  FItems[FCount] := P;
  Inc(FCount);
end;

procedure TDoubleIntPairList.Delete(I: Integer);
var
  J: Integer;
begin
  for J := I to FCount - 2 do
    FItems[J] := FItems[J + 1];
  Dec(FCount);
end;

function TDoubleIntPairList.Last: TDoubleIntPair;
begin
  Result := FItems[FCount - 1];
end;

procedure TDoubleIntPairList.SortDescending;
begin
  if FCount > 1 then
    QSortDoubleIntDesc(FItems, 0, FCount - 1);
end;

// ============================================================================
// TWordOccurrenceList
// ============================================================================

constructor TWordOccurrenceList.Create;
begin
  inherited;
  FCount := 0;
  SetLength(FItems, 8);
end;

destructor TWordOccurrenceList.Destroy;
begin
  SetLength(FItems, 0);
  inherited;
end;

function TWordOccurrenceList.GetItem(I: Integer): TWordOccurrence;
begin
  Result := FItems[I];
end;

procedure TWordOccurrenceList.Add(const Occ: TWordOccurrence);
begin
  if FCount >= Length(FItems) then
    SetLength(FItems, Length(FItems) * 2 + 8);
  FItems[FCount] := Occ;
  Inc(FCount);
end;

// ============================================================================
// TStringHashSet
// ============================================================================

constructor TStringHashSet.Create;
begin
  inherited;
  FList            := TStringList.Create;
  FList.Sorted     := True;
  FList.Duplicates := dupIgnore;
end;

destructor TStringHashSet.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TStringHashSet.Add(const S: string);
begin
  FList.Add(S);
end;

function TStringHashSet.Contains(const S: string): Boolean;
begin
  Result := FList.IndexOf(S) >= 0;
end;

procedure TStringHashSet.Clear;
begin
  FList.Clear;
end;

// ============================================================================
// TBM25ResultList
// ============================================================================

constructor TBM25ResultList.Create;
begin
  inherited;
  FCount := 0;
  SetLength(FItems, 16);
end;

destructor TBM25ResultList.Destroy;
begin
  SetLength(FItems, 0);
  inherited;
end;

function TBM25ResultList.GetItem(I: Integer): TNodeScorePair;
begin
  Result := FItems[I];
end;

procedure TBM25ResultList.Add(const P: TNodeScorePair);
begin
  if FCount >= Length(FItems) then
    SetLength(FItems, Length(FItems) * 2 + 16);
  FItems[FCount] := P;
  Inc(FCount);
end;

procedure TBM25ResultList.SortDescending;
begin
  if FCount > 1 then
    QSortNodeScoresDesc(FItems, 0, FCount - 1);
end;

procedure TBM25ResultList.TrimToSize(MaxCount: Integer);
begin
  if FCount > MaxCount then FCount := MaxCount;
end;

procedure TBM25ResultList.SetKey(I: Integer; NewKey: Double);
begin
  FItems[I].Key := NewKey;
end;

// ============================================================================
// TAiEmbeddingNode
// ============================================================================

constructor TAiEmbeddingNode.Create(aDim: Integer);
var
  G: TGuid;
begin
  inherited Create;
  FDim       := aDim;
  SetLength(FData, FDim);
  FMetaData  := TAiEmbeddingMetaData.Create;
  FTagObject := nil;
  FOrden     := 0;
  FMagnitude := 0;
  FDocLength := 0;
  CreateGUID(G);
  FTag := GUIDToString(G);
end;

destructor TAiEmbeddingNode.Destroy;
begin
  FMetaData.Free;
  // NOT freeing FTagObject — not owner
  inherited;
end;

procedure TAiEmbeddingNode.SetData(const Value: TAiEmbeddingData);
var
  I  : Integer;
  Sum: Double;
begin
  FData := Value;
  FDim  := Length(Value);
  Sum   := 0;
  for I := 0 to High(FData) do
    Sum := Sum + FData[I] * FData[I];
  FMagnitude := Sqrt(Sum);
end;

procedure TAiEmbeddingNode.SetDataLength(aDim: Integer);
begin
  FDim := aDim;
  SetLength(FData, FDim);
end;

function TAiEmbeddingNode.ToJSON: TJSONObject;
var
  JArrData: TJSONArray;
  I       : Integer;
begin
  Result := TJSONObject.Create;
  try
    JArrData := TJSONArray.Create;
    for I := 0 to High(FData) do
      JArrData.Add(FData[I]);
    Result.Add('data',     JArrData);
    Result.Add('text',     FText);
    Result.Add('model',    FModel);
    Result.Add('tag',      FTag);
    Result.Add('orden',    TJSONIntegerNumber.Create(FOrden));
    Result.Add('idx',      TJSONFloatNumber.Create(FIdx));
    if Assigned(FMetaData) then
      Result.Add('metadata', FMetaData.ToJSON);
  except
    Result.Free;
    raise;
  end;
end;

class function TAiEmbeddingNode.FromJSON(AJSONObject: TJSONObject): TAiEmbeddingNode;
var
  JArr : TJSONArray;
  JTmp : TJSONData;
  I    : Integer;
  Sum  : Double;
begin
  if not Assigned(AJSONObject) then
    raise Exception.Create('AJSONObject no puede ser nil');
  JTmp := AJSONObject.Find('data');
  if not Assigned(JTmp) or not (JTmp is TJSONArray) then
    raise Exception.Create('Campo "data" inválido en JSON de nodo');
  JArr   := TJSONArray(JTmp);
  Result := TAiEmbeddingNode.Create(JArr.Count);
  try
    Sum := 0.0;
    for I := 0 to JArr.Count - 1 do begin
      Result.FData[I] := JArr.Items[I].AsFloat;
      Sum             := Sum + Result.FData[I] * Result.FData[I];
    end;
    Result.FMagnitude := Sqrt(Sum);

    JTmp := AJSONObject.Find('text');  if Assigned(JTmp) then Result.FText  := JTmp.AsString;
    JTmp := AJSONObject.Find('model'); if Assigned(JTmp) then Result.FModel := JTmp.AsString;
    JTmp := AJSONObject.Find('tag');   if Assigned(JTmp) then Result.FTag   := JTmp.AsString;
    JTmp := AJSONObject.Find('orden'); if Assigned(JTmp) then Result.FOrden := JTmp.AsInteger;
    JTmp := AJSONObject.Find('idx');   if Assigned(JTmp) then Result.FIdx   := JTmp.AsFloat;

    JTmp := AJSONObject.Find('metadata');
    if Assigned(JTmp) and (JTmp is TJSONObject) then
      Result.FMetaData.FromJSON(TJSONObject(JTmp));
  except
    Result.Free;
    raise;
  end;
end;

function TAiEmbeddingNode.ToJsonArray: TJSONArray;
var
  I: Integer;
begin
  Result := TJSONArray.Create;
  for I := 0 to High(FData) do
    Result.Add(FData[I]);
end;

class function TAiEmbeddingNode.ToJsonArray(Val: TAiEmbeddingNode): TJSONArray;
var
  I: Integer;
begin
  if not Assigned(Val) then
    raise Exception.Create('Val no puede ser nil');
  Result := TJSONArray.Create;
  for I := 0 to High(Val.FData) do
    Result.Add(Val.FData[I]);
end;

class function TAiEmbeddingNode.DotProduct(const A, B: TAiEmbeddingNode): Double;
var
  I: Integer;
begin
  if Length(A.FData) <> Length(B.FData) then
    raise Exception.Create('Los vectores deben ser de la misma longitud');
  Result := 0;
  for I := Low(A.FData) to High(A.FData) do
    Result := Result + A.FData[I] * B.FData[I];
end;

class function TAiEmbeddingNode.Magnitude(const A: TAiEmbeddingNode): Double;
var
  Sum: Double;
  I  : Integer;
begin
  Sum := 0.0;
  for I := Low(A.FData) to High(A.FData) do
    Sum := Sum + A.FData[I] * A.FData[I];
  Result := Sqrt(Sum);
end;

class function TAiEmbeddingNode.CosineSimilarity(const A, B: TAiEmbeddingNode): Double;
begin
  if (A.FMagnitude <= 0) or (B.FMagnitude <= 0) then
    Exit(0.0);
  Result := TAiEmbeddingNode.DotProduct(A, B) / (A.FMagnitude * B.FMagnitude);
  if      Result >  1.0 then Result :=  1.0
  else if Result < -1.0 then Result := -1.0;
end;

function CompareEmbeddings(const Left, Right: TAiEmbeddingNode; Axis: Integer): Integer;
const
  TOLERANCE = 1.0E-12;
begin
  if Abs(Left.Data[Axis] - Right.Data[Axis]) < TOLERANCE then Result :=  0
  else if Left.Data[Axis] < Right.Data[Axis]              then Result := -1
  else                                                          Result :=  1;
end;

// ============================================================================
// TAiSearchResult
// ============================================================================

class function TAiSearchResult.Create(aNode: TAiEmbeddingNode;
    aScore: Double): TAiSearchResult;
begin
  Result.Node   := aNode;
  Result.FScore := aScore;
end;

class function TAiSearchResult.CompareDescending(const Left,
    Right: TAiSearchResult): Integer;
begin
  Result := CompareValue(Right.FScore, Left.FScore);
end;

// ============================================================================
// TAIEmbeddingIndex
// ============================================================================

constructor TAIEmbeddingIndex.Create;
begin
  inherited;
  FDataVec := nil;
  FActive  := False;
end;

destructor TAIEmbeddingIndex.Destroy;
begin
  // NOT freeing FDataVec — not owner
  inherited;
end;

function TAIEmbeddingIndex.Add(Point: TAiEmbeddingNode): Integer;
begin
  Result := -1;
end;

procedure TAIEmbeddingIndex.BuildIndex(Points: TRagItems);
begin
  FDataVec := Points;
  FActive  := True;
end;

function TAIEmbeddingIndex.Search(Target: TAiEmbeddingNode; aLimit: Integer;
    aPrecision: Double): TRagItems;
begin
  Result := nil;
end;

procedure TAIEmbeddingIndex.Clear;
begin
  InternalClear;
  FActive := False;
end;

// ============================================================================
// TAIBasicEmbeddingIndex
// ============================================================================

procedure TAIBasicEmbeddingIndex.InternalClear;
begin
  // No internal structure
end;

constructor TAIBasicEmbeddingIndex.Create;
begin
  inherited Create;
end;

destructor TAIBasicEmbeddingIndex.Destroy;
begin
  inherited;
end;

procedure TAIBasicEmbeddingIndex.BuildIndex(Points: TRagItems);
begin
  if not Assigned(Points) then Exit;
  inherited;
end;

class function TAIBasicEmbeddingIndex.InternalSearchSafe(
    Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double;
    Source: TRagItems): TSearchResultList;
var
  I    : Integer;
  Emb  : TAiEmbeddingNode;
  Score: Double;
  Tmp  : TSearchResultList;
begin
  Result := TSearchResultList.Create;
  Tmp    := TSearchResultList.Create;
  try
    for I := 0 to Source.Count - 1 do begin
      Emb   := Source[I];
      Score := TAiEmbeddingNode.CosineSimilarity(Emb, Target);
      if (aPrecision > 0) and (Score < aPrecision) then Continue;
      Tmp.Add(TAiSearchResult.Create(Emb, Score));
    end;
    Tmp.SortDescending;
    for I := 0 to Min(aLimit - 1, Tmp.Count - 1) do
      Result.Add(Tmp[I]);
  finally
    Tmp.Free;
  end;
end;

class function TAIBasicEmbeddingIndex.InternalSearch(Target: TAiEmbeddingNode;
    aLimit: Integer; aPrecision: Double; Source: TRagItems): TRagItems;
var
  SafeResults: TSearchResultList;
  I          : Integer;
begin
  Result      := TRagItems.Create;
  SafeResults := InternalSearchSafe(Target, aLimit, aPrecision, Source);
  try
    for I := 0 to SafeResults.Count - 1 do begin
      SafeResults[I].Node.Idx := SafeResults[I].FScore;
      Result.Add(SafeResults[I].Node);
    end;
  finally
    SafeResults.Free;
  end;
end;

function TAIBasicEmbeddingIndex.Search(Target: TAiEmbeddingNode; aLimit: Integer;
    aPrecision: Double): TRagItems;
var
  SafeList: TSearchResultList;
  I       : Integer;
begin
  Result   := TRagItems.Create;
  SafeList := InternalSearchSafe(Target, aLimit, aPrecision, DataVec);
  try
    for I := 0 to SafeList.Count - 1 do
      Result.Add(SafeList[I].Node);
  finally
    SafeList.Free;
  end;
end;

// ============================================================================
// TAIEuclideanDistanceIndex
// ============================================================================

procedure TAIEuclideanDistanceIndex.InternalClear;
begin
  // No internal structure
end;

constructor TAIEuclideanDistanceIndex.Create;
begin
  inherited;
end;

destructor TAIEuclideanDistanceIndex.Destroy;
begin
  inherited;
end;

procedure TAIEuclideanDistanceIndex.BuildIndex(Points: TRagItems);
begin
  inherited;
end;

function TAIEuclideanDistanceIndex.Search(Target: TAiEmbeddingNode; aLimit: Integer;
    aPrecision: Double): TRagItems;
var
  I       : Integer;
  Emb     : TAiEmbeddingNode;
  Distance: Double;
  Pairs   : array of TL2Pair;
  PairCnt : Integer;
  P       : TL2Pair;
begin
  Result  := TRagItems.Create;
  PairCnt := 0;
  SetLength(Pairs, DataVec.Count + 1);
  for I := 0 to DataVec.Count - 1 do begin
    Emb      := DataVec[I];
    Distance := TAiEmbeddingsCore.EuclideanDistance(Emb.Data, Target.Data);
    Emb.Idx  := Distance;
    if (aPrecision <= 0) or (Distance <= aPrecision) then begin
      P.Key  := Distance;
      P.Node := Emb;
      Pairs[PairCnt] := P;
      Inc(PairCnt);
    end;
  end;
  if PairCnt > 1 then
    QSortL2PairsAsc(Pairs, 0, PairCnt - 1);
  for I := 0 to Min(aLimit - 1, PairCnt - 1) do
    Result.Add(Pairs[I].Node);
end;

// ============================================================================
// THNSWNode
// ============================================================================

constructor THNSWNode.Create(aID: Integer; aVector: TAiEmbeddingNode;
    aNumLevels: Integer);
var
  I: Integer;
begin
  inherited Create;
  FID     := aID;
  FVector := aVector;
  SetLength(FConnections, aNumLevels);
  try
    for I := 0 to aNumLevels - 1 do
      FConnections[I] := TIntList.Create;
  except
    for I := 0 to High(FConnections) do
      if Assigned(FConnections[I]) then FConnections[I].Free;
    raise;
  end;
end;

destructor THNSWNode.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FConnections) do
    if Assigned(FConnections[I]) then FreeAndNil(FConnections[I]);
  // NOT freeing FVector — not owner
  inherited;
end;

function THNSWNode.GetConnections(Level: Integer): TIntList;
begin
  if (Level < 0) or (Level >= Length(FConnections)) then
    raise Exception.CreateFmt('Nivel %d fuera de rango [0..%d]',
          [Level, Length(FConnections) - 1]);
  Result := FConnections[Level];
end;

function THNSWNode.LevelCount: Integer;
begin
  Result := Length(FConnections);
end;

// ============================================================================
// THNSWIndex
// ============================================================================

constructor THNSWIndex.Create;
begin
  inherited;
  FNodeCount      := 0;
  FMaxLevel       := 16;
  FLevelMult      := 1 / Ln(2);
  FEfConstruction := 40;
  FMaxConnections := 16;
  FEntryPoint     := -1;
  FEntryLevel     := -1;
end;

destructor THNSWIndex.Destroy;
begin
  Clear;
  inherited;
end;

procedure THNSWIndex.InternalClear;
var
  I: Integer;
begin
  for I := 0 to FNodeCount - 1 do
    if Assigned(FNodes[I]) then FreeAndNil(FNodes[I]);
  FNodeCount := 0;
  SetLength(FNodes, 0);
end;

procedure THNSWIndex.Clear;
begin
  InternalClear;
  FEntryPoint := -1;
  FEntryLevel := -1;
  FActive     := False;
end;

function THNSWIndex.GetRandomLevel: Integer;
var
  R: Double;
begin
  R := Random;
  if R <= 0 then R := 1E-12;
  Result := Floor(-Ln(R) * FLevelMult);
  if Result >= FMaxLevel then Result := FMaxLevel - 1;
end;

procedure THNSWIndex.InsertConnection(Node: THNSWNode; Level, TargetID: Integer);
var
  ConnList    : TIntList;
  TargetNode  : THNSWNode;
  NeighborNode: THNSWNode;
  TargetSim   : Double;
  NeighborSim : Double;
  MinSim      : Double;
  WorstIndex  : Integer;
  I           : Integer;
begin
  ConnList := Node.GetConnections(Level);
  if ConnList.IndexOf(TargetID) >= 0 then Exit; // sin duplicados

  if ConnList.Count < FMaxConnections then begin
    ConnList.Add(TargetID);
    Exit;
  end;

  if (TargetID < 0) or (TargetID >= FNodeCount) or not Assigned(FNodes[TargetID]) then Exit;

  TargetNode := FNodes[TargetID];
  TargetSim  := TAiEmbeddingNode.CosineSimilarity(Node.Vector, TargetNode.Vector);
  MinSim     := 2.0;
  WorstIndex := -1;

  for I := 0 to ConnList.Count - 1 do begin
    if (ConnList[I] >= 0) and (ConnList[I] < FNodeCount) and
       Assigned(FNodes[ConnList[I]]) then begin
      NeighborNode := FNodes[ConnList[I]];
      NeighborSim  := TAiEmbeddingNode.CosineSimilarity(Node.Vector, NeighborNode.Vector);
      if NeighborSim < MinSim then begin
        MinSim     := NeighborSim;
        WorstIndex := I;
      end;
    end;
  end;

  if (WorstIndex > -1) and (TargetSim > MinSim) then
    ConnList[WorstIndex] := TargetID;
end;

function THNSWIndex.SearchLayer(Query: TAiEmbeddingNode;
    EntryPoint, Level, Ef: Integer): TIntList;
var
  Visited       : array of Boolean;
  Candidates    : TDoubleIntPairList;
  BestCandidates: TDoubleIntPairList;
  CurNode       : THNSWNode;
  ConnList      : TIntList;
  P             : TDoubleIntPair;
  Distance      : Double;
  I, NbrID      : Integer;
begin
  Result := TIntList.Create;
  SetLength(Visited, FNodeCount);
  FillChar(Visited[0], FNodeCount * SizeOf(Boolean), 0);

  Candidates     := TDoubleIntPairList.Create;
  BestCandidates := TDoubleIntPairList.Create;
  try
    CurNode  := FNodes[EntryPoint];
    Distance := TAiEmbeddingNode.CosineSimilarity(Query, CurNode.Vector);
    P.Key := Distance; P.Value := EntryPoint;
    Candidates.Add(P);
    BestCandidates.Add(P);
    Visited[EntryPoint] := True;

    while Candidates.Count > 0 do begin
      Candidates.SortDescending;
      P       := Candidates[0];
      Candidates.Delete(0);
      CurNode := FNodes[P.Value];

      ConnList := CurNode.GetConnections(Level);
      for I := 0 to ConnList.Count - 1 do begin
        NbrID := ConnList[I];
        if (NbrID < 0) or (NbrID >= FNodeCount) then Continue;
        if Visited[NbrID] then Continue;
        Visited[NbrID] := True;
        Distance := TAiEmbeddingNode.CosineSimilarity(Query, FNodes[NbrID].Vector);
        if (BestCandidates.Count < Ef) or (Distance > BestCandidates.Last.Key) then begin
          P.Key := Distance; P.Value := NbrID;
          Candidates.Add(P);
          BestCandidates.Add(P);
          if BestCandidates.Count > Ef then begin
            BestCandidates.SortDescending;
            BestCandidates.Delete(BestCandidates.Count - 1);
          end;
        end;
      end;
    end;

    for I := 0 to BestCandidates.Count - 1 do
      Result.Add(BestCandidates[I].Value);
  finally
    BestCandidates.Free;
    Candidates.Free;
  end;
end;

function THNSWIndex.Add(Point: TAiEmbeddingNode): Integer;
var
  NodeID        : Integer;
  Level         : Integer;
  CurrentLevel  : Integer;
  EntryPointCopy: Integer;
  W             : TIntList;
  Node          : THNSWNode;
  I, NIdx       : Integer;
begin
  NodeID := FNodeCount;
  Level  := GetRandomLevel;

  if FNodeCount >= Length(FNodes) then
    SetLength(FNodes, Length(FNodes) * 2 + 16);

  Node := THNSWNode.Create(NodeID, Point, FMaxLevel);
  try
    FNodes[FNodeCount] := Node;
    Inc(FNodeCount);
  except
    Node.Free;
    raise;
  end;

  if FEntryPoint = -1 then begin
    FEntryPoint := NodeID;
    FEntryLevel := Level;
    Result := NodeID;
    Exit;
  end;

  EntryPointCopy := FEntryPoint;
  CurrentLevel   := FMaxLevel - 1;

  while CurrentLevel > Level do begin
    W := SearchLayer(Point, EntryPointCopy, CurrentLevel, 1);
    try
      if W.Count > 0 then EntryPointCopy := W[0];
    finally
      W.Free;
    end;
    Dec(CurrentLevel);
  end;

  while CurrentLevel >= 0 do begin
    W := SearchLayer(Point, EntryPointCopy, CurrentLevel, FEfConstruction);
    try
      for I := 0 to W.Count - 1 do begin
        NIdx := W[I];
        InsertConnection(Node, CurrentLevel, NIdx);
        InsertConnection(FNodes[NIdx], CurrentLevel, NodeID);
      end;
      if W.Count > 0 then EntryPointCopy := W[0];
    finally
      W.Free;
    end;
    Dec(CurrentLevel);
  end;

  if Level > FEntryLevel then begin
    FEntryPoint := NodeID;
    FEntryLevel := Level;
  end;

  Result := NodeID;
end;

procedure THNSWIndex.BuildIndex(Points: TRagItems);
var
  I: Integer;
begin
  if not Assigned(Points) then
    raise Exception.Create('THNSWIndex.BuildIndex: Points no puede ser nil');
  Clear;
  inherited BuildIndex(Points);
  for I := 0 to Points.Count - 1 do
    Add(Points[I]);
end;

function THNSWIndex.Search(Target: TAiEmbeddingNode; aLimit: Integer;
    aPrecision: Double): TRagItems;
var
  CurrentLevel  : Integer;
  EntryPointCopy: Integer;
  W             : TIntList;
  ResultPairs   : TDoubleIntPairList;
  I, NIdx       : Integer;
  Distance      : Double;
  Node          : THNSWNode;
  P             : TDoubleIntPair;
begin
  Result := TRagItems.Create;
  if FEntryPoint = -1 then Exit;

  ResultPairs    := TDoubleIntPairList.Create;
  EntryPointCopy := FEntryPoint;
  CurrentLevel   := FMaxLevel - 1;
  try
    while CurrentLevel >= 0 do begin
      W := SearchLayer(Target, EntryPointCopy, CurrentLevel, 1);
      try
        if W.Count > 0 then EntryPointCopy := W[0];
      finally
        W.Free;
      end;
      Dec(CurrentLevel);
    end;

    W := SearchLayer(Target, EntryPointCopy, 0, Max(100, aLimit * 2));
    try
      for I := 0 to W.Count - 1 do begin
        NIdx     := W[I];
        Node     := FNodes[NIdx];
        Distance := TAiEmbeddingNode.CosineSimilarity(Target, Node.Vector);
        if Distance >= aPrecision then begin
          P.Key := Distance; P.Value := NIdx;
          ResultPairs.Add(P);
        end;
      end;
    finally
      W.Free;
    end;

    ResultPairs.SortDescending;
    for I := 0 to Min(aLimit - 1, ResultPairs.Count - 1) do begin
      NIdx := ResultPairs[I].Value;
      Node := FNodes[NIdx];
      Node.Vector.Idx := ResultPairs[I].Key;
      Result.Add(Node.Vector);
    end;
  finally
    ResultPairs.Free;
  end;
end;

// ============================================================================
// TAIBm25Index
// ============================================================================

constructor TAIBm25Index.Create;
begin
  inherited Create;
  FInvertedIndex            := TStringList.Create;
  FInvertedIndex.Sorted     := True;
  FInvertedIndex.Duplicates := dupError;
  FAvgDocLength             := 0;
  FDocCount                 := 0;
  FStopWords                := TStringHashSet.Create;
  FLanguage                 := alCustom; // fuerza cambio de lenguaje
  Language                  := alSpanish;
end;

destructor TAIBm25Index.Destroy;
var
  I: Integer;
begin
  for I := 0 to FInvertedIndex.Count - 1 do
    TWordOccurrenceList(FInvertedIndex.Objects[I]).Free;
  FInvertedIndex.Free;
  FStopWords.Free;
  inherited;
end;

procedure TAIBm25Index.LoadDefaultStopWords(Lang: TAiLanguage);
const
  S_ES = 'el,la,lo,los,las,un,una,unos,unas,de,del,al,y,o,u,e,ni,que,en,a,' +
         'ante,bajo,con,contra,desde,donde,durante,este,esta,estos,estas,ese,' +
         'esa,esos,esas,aquel,aquella,aquellos,aquellas,mi,tu,su,nuestro,' +
         'vuestro,me,te,se,nos,os,le,les,ser,estar,haber,tener,hacer,hay,' +
         'he,ha,han,son,es,fue,sido,como,mas,pero,por,para,sin,sobre,' +
         'tambien,muy,ya,si,no,cuando,entre';
  S_EN = 'the,a,an,and,or,but,if,then,else,when,at,from,by,for,with,about,' +
         'against,between,into,through,during,before,after,above,below,to,' +
         'of,in,is,are,was,were,be,been,being,have,has,had,do,does,did,' +
         'will,would,should,could,may,might,can,this,that,these,those,' +
         'i,you,he,she,it,we,they,me,him,her,us,them';
var
  Tmp: TStringList;
  I  : Integer;
  W, S: string;
begin
  FStopWords.Clear;
  case Lang of
    alSpanish: S := S_ES;
    alEnglish: S := S_EN;
  else         S := '';
  end;
  if S = '' then Exit;
  Tmp := TStringList.Create;
  try
    SplitCSV(S, Tmp);
    for I := 0 to Tmp.Count - 1 do begin
      W := Trim(LowerCase(Tmp[I]));
      if W <> '' then FStopWords.Add(W);
    end;
  finally
    Tmp.Free;
  end;
end;

procedure TAIBm25Index.SetLanguage(const Value: TAiLanguage);
begin
  if FLanguage <> Value then begin
    FLanguage := Value;
    if FLanguage <> alCustom then
      LoadDefaultStopWords(FLanguage);
  end;
end;

procedure TAIBm25Index.Tokenize(const aText: string; aList: TStrings);
const
  Separators = [' ', '.', ',', ';', ':', '-', '_', '(', ')', '[', ']', '{',
                '}', '"', '?', '!', '/', '\', '|', #9, #10, #13];
var
  I, Start: Integer;
  W       : string;
begin
  aList.Clear;
  if aText = '' then Exit;
  Start := 1;
  for I := 1 to Length(aText) do
    if aText[I] in Separators then begin
      if I > Start then begin
        W := LowerCase(Copy(aText, Start, I - Start));
        if (Length(W) > 2) and not FStopWords.Contains(W) then
          aList.Add(W);
      end;
      Start := I + 1;
    end;
  if Start <= Length(aText) then begin
    W := LowerCase(Copy(aText, Start, MaxInt));
    if (Length(W) > 2) and not FStopWords.Contains(W) then
      aList.Add(W);
  end;
end;

procedure TAIBm25Index.AddNode(aNode: TAiEmbeddingNode);
var
  Tokens   : TStringList;
  SortedTok: TStringList;
  W        : string;
  OccList  : TWordOccurrenceList;
  Occ      : TWordOccurrence;
  WordFreq : Integer;
  I, J, Idx: Integer;
begin
  if aNode.Text = '' then Exit;
  Tokens    := TStringList.Create;
  SortedTok := TStringList.Create;
  try
    Tokenize(aNode.Text, Tokens);
    if Tokens.Count = 0 then Exit;

    aNode.DocLength := Tokens.Count;

    SortedTok.Assign(Tokens);
    SortedTok.Sort;

    I := 0;
    while I < SortedTok.Count do begin
      W        := SortedTok[I];
      WordFreq := 0;
      J        := I;
      while (J < SortedTok.Count) and (SortedTok[J] = W) do begin
        Inc(WordFreq);
        Inc(J);
      end;

      Idx := FInvertedIndex.IndexOf(W);
      if Idx >= 0 then
        OccList := TWordOccurrenceList(FInvertedIndex.Objects[Idx])
      else begin
        OccList := TWordOccurrenceList.Create;
        FInvertedIndex.AddObject(W, OccList);
      end;

      Occ.Node  := aNode;
      Occ.Count := WordFreq;
      OccList.Add(Occ);

      I := J;
    end;

    Inc(FDocCount);
    FAvgDocLength := ((FAvgDocLength * (FDocCount - 1)) + Tokens.Count) / FDocCount;
  finally
    SortedTok.Free;
    Tokens.Free;
  end;
end;

procedure TAIBm25Index.Clear;
var
  I: Integer;
begin
  for I := 0 to FInvertedIndex.Count - 1 do
    TWordOccurrenceList(FInvertedIndex.Objects[I]).Free;
  FInvertedIndex.Clear;
  FAvgDocLength := 0;
  FDocCount     := 0;
end;

function TAIBm25Index.Search(const aQuery: string; aLimit: Integer;
    aFilter: TAiFilterCriteria): TBM25ResultList;
const
  k1 = 1.2;
  B  = 0.75;
var
  QueryTokens: TStringList;
  QW         : string;
  OccList    : TWordOccurrenceList;
  Accum      : array of TNodeAccum;
  AccumCount : Integer;
  AccumIdx   : Integer;
  I, J, K    : Integer;
  Idx        : Integer;
  N, n_q, f_q_d: Integer;
  IDF, Score, D_len: Double;
  P          : TNodeScorePair;
begin
  Result := TBM25ResultList.Create;
  if (aQuery = '') or (FDocCount = 0) then Exit;

  QueryTokens := TStringList.Create;
  SetLength(Accum, 64);
  AccumCount := 0;
  try
    Tokenize(aQuery, QueryTokens);
    if QueryTokens.Count = 0 then Exit;

    N := FDocCount;
    for I := 0 to QueryTokens.Count - 1 do begin
      QW  := QueryTokens[I];
      Idx := FInvertedIndex.IndexOf(QW);
      if Idx < 0 then Continue;

      OccList := TWordOccurrenceList(FInvertedIndex.Objects[Idx]);
      n_q     := OccList.Count;
      IDF     := Ln((N - n_q + 0.5) / (n_q + 0.5) + 1.0);

      for J := 0 to OccList.Count - 1 do begin
        if Assigned(aFilter) and (aFilter.Count > 0) then
          if not OccList[J].Node.MetaData.Matches(aFilter) then Continue;

        f_q_d := OccList[J].Count;
        D_len := OccList[J].Node.DocLength;
        if (D_len <= 0) or (FAvgDocLength <= 0) then Continue;

        Score := IDF * (f_q_d * (k1 + 1)) /
                 (f_q_d + k1 * (1 - B + B * (D_len / FAvgDocLength)));

        AccumIdx := -1;
        for K := 0 to AccumCount - 1 do
          if Accum[K].Node = OccList[J].Node then begin
            AccumIdx := K;
            Break;
          end;

        if AccumIdx >= 0 then
          Accum[AccumIdx].Score := Accum[AccumIdx].Score + Score
        else begin
          if AccumCount >= Length(Accum) then
            SetLength(Accum, AccumCount * 2 + 64);
          Accum[AccumCount].Node  := OccList[J].Node;
          Accum[AccumCount].Score := Score;
          Inc(AccumCount);
        end;
      end;
    end;

    for I := 0 to AccumCount - 1 do begin
      P.Key   := Accum[I].Score;
      P.Value := Accum[I].Node;
      Result.Add(P);
    end;
    Result.SortDescending;
    if aLimit > 0 then Result.TrimToSize(aLimit);

  finally
    QueryTokens.Free;
  end;
end;

end.
