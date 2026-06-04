// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.Embeddings.Core
// Tipos base, operadores de vector y clase TAiEmbeddingsCore.
unit uMakerAi.Embeddings.Core;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, Math, StrUtils,
  fpjson;

type
  // Tipo base para un vector de embedding (array dinámico de doubles)
  TAiEmbeddingData = array of Double;
  // Lista de vectores
  TAiEmbeddingList = array of TAiEmbeddingData;

  // Resultado de búsqueda por similitud
  TAiSimilarityResult = record
    Index : Integer;
    Score : Double;
    Vector: TAiEmbeddingData;
  end;

  TAiSimilarityList = array of TAiSimilarityResult;

  // Evento para generación personalizada de embeddings
  TOnGetEmbedding = procedure(Sender: TObject;
      const aInput, aUser, aModel, aEncodingFormat: string;
      aDimensions: Integer; var aEmbedding: TAiEmbeddingData) of object;

  // Record con operadores sobrecargados para vectores de embedding
  TAiEmbeddingDataRec = record
  private
    FData: TAiEmbeddingData;
  public
    class function FromArray(const A: TAiEmbeddingData): TAiEmbeddingDataRec; static;
    function ToArray: TAiEmbeddingData;
    function Magnitude: Double;
    procedure Normalize;
    class function StringToEmbedding(const AVectorString: string): TAiEmbeddingData; static;
    class function EmbeddingToString(const AEmbedding: TAiEmbeddingData): string; static;
    property Data: TAiEmbeddingData read FData write FData;
  end;

  // Clase base para proveedores de embeddings
  TAiEmbeddingsCore = class(TComponent)
  private
    FOnGetEmbedding: TOnGetEmbedding;
    procedure SetData(const Value: TAiEmbeddingData);
  protected
    FData         : TAiEmbeddingData;
    FModel        : string;
    FDimensions   : Integer;
    Ftotal_tokens : Integer;
    Fprompt_tokens: Integer;
  public
    constructor Create(aOwner: TComponent); override;

    function CreateEmbedding(aInput, aUser: string; aDimensions: Integer = -1;
        aModel: string = ''; aEncodingFormat: string = 'float'): TAiEmbeddingData; virtual;

    function ToJsonArray: TJSONArray; overload;
    class function ToJsonArray(Val: TAiEmbeddingData): TJSONArray; overload;
    class function ToEmbeddingData(Value: TJSONArray): TAiEmbeddingData;

    // Métodos de aritmética vectorial
    class function Magnitude(const V: TAiEmbeddingData): Double;
    class function DotProduct(const A, B: TAiEmbeddingData): Double;
    class function CosineSimilarity(const A, B: TAiEmbeddingData): Double;
    class function EuclideanDistance(const A, B: TAiEmbeddingData): Double;
    class procedure Normalize(var V: TAiEmbeddingData);
    class function FindNearest(const Query: TAiEmbeddingData;
        const Candidates: TAiEmbeddingList): TAiSimilarityResult;
    class function FindTopK(const Query: TAiEmbeddingData;
        const Candidates: TAiEmbeddingList; K: Integer): TAiSimilarityList;
    class function VectorAdd(const A, B: TAiEmbeddingData): TAiEmbeddingData;
    class function VectorSubtract(const A, B: TAiEmbeddingData): TAiEmbeddingData;
    class function AverageEmbedding(const Vectors: TAiEmbeddingList): TAiEmbeddingData;

    property Data: TAiEmbeddingData read FData write SetData;
  published
    property Model        : string  read FModel       write FModel;
    property Dimensions   : Integer read FDimensions   write FDimensions;
    property prompt_tokens: Integer read Fprompt_tokens;
    property total_tokens : Integer read Ftotal_tokens;
    property OnGetEmbedding: TOnGetEmbedding read FOnGetEmbedding write FOnGetEmbedding;
  end;

// Operadores sobrecargados para TAiEmbeddingDataRec (fuera del bloque type — requerido en FPC)
operator + (const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
operator - (const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
operator * (const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec;
operator * (const A, B: TAiEmbeddingDataRec): Double;  // Dot product
operator / (const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec;
operator = (const A, B: TAiEmbeddingDataRec): Boolean;
operator <> (const A, B: TAiEmbeddingDataRec): Boolean;

implementation

// ---------------------------------------------------------------------------
// Helpers internos
// ---------------------------------------------------------------------------

// QuickSort descendente por Score para TAiSimilarityList
procedure QuickSortByScoreDesc(var A: TAiSimilarityList; L, R: Integer);
var
  I, J  : Integer;
  Pivot : Double;
  Tmp   : TAiSimilarityResult;
begin
  if L >= R then Exit;
  I := L; J := R;
  Pivot := A[(L + R) shr 1].Score;
  repeat
    while A[I].Score > Pivot do Inc(I);
    while A[J].Score < Pivot do Dec(J);
    if I <= J then
    begin
      Tmp := A[I]; A[I] := A[J]; A[J] := Tmp;
      Inc(I); Dec(J);
    end;
  until I > J;
  if L < J then QuickSortByScoreDesc(A, L, J);
  if I < R then QuickSortByScoreDesc(A, I, R);
end;

// FormatSettings con punto decimal invariante
function InvariantFS: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator  := '.';
  Result.ThousandSeparator := #0;
end;

// ---------------------------------------------------------------------------
// TAiEmbeddingsCore
// ---------------------------------------------------------------------------

constructor TAiEmbeddingsCore.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDimensions := 1536;
  FModel      := 'custom-model';
end;

function TAiEmbeddingsCore.CreateEmbedding(aInput, aUser: string;
    aDimensions: Integer; aModel, aEncodingFormat: string): TAiEmbeddingData;
var
  LModel     : string;
  LDimensions: Integer;
begin
  if not Assigned(FOnGetEmbedding) then
    raise Exception.Create(
        'The OnGetEmbedding event has not been assigned. Cannot generate embedding.');

  LModel      := IfThen(aModel <> '', aModel, FModel);
  LDimensions := IfThen(aDimensions > 0, aDimensions, FDimensions);

  SetLength(Result, 0);
  FOnGetEmbedding(Self, aInput, aUser, LModel, aEncodingFormat, LDimensions, Result);
  FData := Result;
end;

procedure TAiEmbeddingsCore.SetData(const Value: TAiEmbeddingData);
begin
  FData := Value;
end;

class function TAiEmbeddingsCore.Magnitude(const V: TAiEmbeddingData): Double;
var
  Sum: Double;
  I  : Integer;
begin
  Sum := 0.0;
  for I := 0 to High(V) do
    Sum := Sum + Sqr(V[I]);
  Result := Sqrt(Sum);
end;

class procedure TAiEmbeddingsCore.Normalize(var V: TAiEmbeddingData);
var
  Mag: Double;
  I  : Integer;
begin
  Mag := Magnitude(V);
  if Mag = 0 then Exit;
  for I := 0 to High(V) do
    V[I] := V[I] / Mag;
end;

class function TAiEmbeddingsCore.DotProduct(const A, B: TAiEmbeddingData): Double;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    raise Exception.Create('Vectors must have the same dimension for dot product.');
  Result := 0.0;
  for I := 0 to High(A) do
    Result := Result + A[I] * B[I];
end;

class function TAiEmbeddingsCore.CosineSimilarity(const A, B: TAiEmbeddingData): Double;
var
  Dot, MagA, MagB: Double;
begin
  Dot := DotProduct(A, B);
  if IsZero(Dot) then
  begin
    Result := 0.0;
    Exit;
  end;
  MagA := Magnitude(A);
  MagB := Magnitude(B);
  if (MagA = 0) or (MagB = 0) then
    Result := 0
  else
    Result := Dot / (MagA * MagB);
end;

class function TAiEmbeddingsCore.EuclideanDistance(const A, B: TAiEmbeddingData): Double;
var
  I           : Integer;
  SumOfSquares: Double;
begin
  if Length(A) <> Length(B) then
    raise Exception.Create(
        'Vectors must have the same dimension for Euclidean distance.');
  SumOfSquares := 0.0;
  for I := 0 to High(A) do
    SumOfSquares := SumOfSquares + Sqr(A[I] - B[I]);
  Result := Sqrt(SumOfSquares);
end;

class function TAiEmbeddingsCore.FindNearest(const Query: TAiEmbeddingData;
    const Candidates: TAiEmbeddingList): TAiSimilarityResult;
var
  I            : Integer;
  BestScore    : Double;
  CurrentScore : Double;
  QueryMag, CandidateMag, Dot: Double;
begin
  if Length(Candidates) = 0 then
    raise Exception.Create('The list of candidates cannot be empty.');

  BestScore    := -2.0;
  Result.Index := -1;
  QueryMag     := Magnitude(Query);
  if QueryMag = 0 then Exit;

  for I := 0 to High(Candidates) do
  begin
    Dot := DotProduct(Query, Candidates[I]);
    if IsZero(Dot) then
      CurrentScore := 0.0
    else
    begin
      CandidateMag := Magnitude(Candidates[I]);
      if CandidateMag = 0 then
        CurrentScore := 0.0
      else
        CurrentScore := Dot / (QueryMag * CandidateMag);
    end;

    if CurrentScore > BestScore then
    begin
      BestScore    := CurrentScore;
      Result.Index := I;
      Result.Score := CurrentScore;
    end;
  end;

  if Result.Index <> -1 then
    Result.Vector := Candidates[Result.Index];
end;

class function TAiEmbeddingsCore.FindTopK(const Query: TAiEmbeddingData;
    const Candidates: TAiEmbeddingList; K: Integer): TAiSimilarityList;
var
  I            : Integer;
  QueryMag, CandidateMag, Dot: Double;
  Res          : TAiSimilarityResult;
  FinalK       : Integer;
begin
  SetLength(Result, Length(Candidates));
  QueryMag := Magnitude(Query);

  for I := 0 to High(Candidates) do
  begin
    Res.Index  := I;
    Res.Vector := Candidates[I];
    if QueryMag = 0 then
      Res.Score := 0
    else
    begin
      Dot := DotProduct(Query, Candidates[I]);
      if IsZero(Dot) then
        Res.Score := 0
      else
      begin
        CandidateMag := Magnitude(Candidates[I]);
        if CandidateMag = 0 then
          Res.Score := 0
        else
          Res.Score := Dot / (QueryMag * CandidateMag);
      end;
    end;
    Result[I] := Res;
  end;

  // Ordenar descendente por Score
  if Length(Result) > 1 then
    QuickSortByScoreDesc(Result, 0, High(Result));

  // Recortar a K
  FinalK := K;
  if FinalK > Length(Result) then FinalK := Length(Result);
  SetLength(Result, FinalK);
end;

class function TAiEmbeddingsCore.VectorAdd(const A, B: TAiEmbeddingData): TAiEmbeddingData;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    raise Exception.Create('Vectors must have the same dimension to be added.');
  SetLength(Result, Length(A));
  for I := 0 to High(A) do
    Result[I] := A[I] + B[I];
end;

class function TAiEmbeddingsCore.VectorSubtract(const A, B: TAiEmbeddingData): TAiEmbeddingData;
var
  I: Integer;
begin
  if Length(A) <> Length(B) then
    raise Exception.Create('Vectors must have the same dimension to be subtracted.');
  SetLength(Result, Length(A));
  for I := 0 to High(A) do
    Result[I] := A[I] - B[I];
end;

class function TAiEmbeddingsCore.AverageEmbedding(const Vectors: TAiEmbeddingList): TAiEmbeddingData;
var
  I, J      : Integer;
  SumVector : TAiEmbeddingData;
begin
  if Length(Vectors) = 0 then
  begin
    Result := nil;
    Exit;
  end;
  SetLength(SumVector, Length(Vectors[0]));
  for I := 0 to High(Vectors) do
  begin
    if Length(Vectors[I]) <> Length(SumVector) then
      raise Exception.Create(
          'All vectors must have the same dimension to be averaged.');
    for J := 0 to High(SumVector) do
      SumVector[J] := SumVector[J] + Vectors[I][J];
  end;
  Result := SumVector;
  for I := 0 to High(Result) do
    Result[I] := Result[I] / Length(Vectors);
end;

class function TAiEmbeddingsCore.ToJsonArray(Val: TAiEmbeddingData): TJSONArray;
var
  I: Integer;
begin
  Result := TJSONArray.Create;
  for I := 0 to High(Val) do
    Result.Add(Val[I]);
end;

function TAiEmbeddingsCore.ToJsonArray: TJSONArray;
begin
  Result := ToJsonArray(FData);
end;

class function TAiEmbeddingsCore.ToEmbeddingData(Value: TJSONArray): TAiEmbeddingData;
var
  J: Integer;
begin
  SetLength(Result, Value.Count);
  for J := 0 to Value.Count - 1 do
    Result[J] := Value.Items[J].AsFloat;
end;

// ---------------------------------------------------------------------------
// TAiEmbeddingDataRec
// ---------------------------------------------------------------------------

class function TAiEmbeddingDataRec.FromArray(const A: TAiEmbeddingData): TAiEmbeddingDataRec;
begin
  Result.FData := Copy(A);
end;

function TAiEmbeddingDataRec.ToArray: TAiEmbeddingData;
begin
  Result := Copy(FData);
end;

function TAiEmbeddingDataRec.Magnitude: Double;
var
  Dot: Double;
  I  : Integer;
begin
  Dot := 0;
  for I := 0 to High(FData) do
    Dot := Dot + FData[I] * FData[I];
  Result := Sqrt(Dot);
end;

procedure TAiEmbeddingDataRec.Normalize;
var
  Mag: Double;
  I  : Integer;
begin
  Mag := Magnitude;
  if Mag > 0 then
    for I := 0 to High(FData) do
      FData[I] := FData[I] / Mag;
end;

class function TAiEmbeddingDataRec.StringToEmbedding(const AVectorString: string): TAiEmbeddingData;
var
  Cleaned: string;
  Parts  : array of string;
  FS     : TFormatSettings;
  I      : Integer;
begin
  Result := nil;
  if (AVectorString = '') or (AVectorString = '[]') then Exit;

  Cleaned := Trim(AVectorString);
  if (Length(Cleaned) >= 2) and (Cleaned[1] = '[') and (Cleaned[Length(Cleaned)] = ']') then
    Cleaned := Copy(Cleaned, 2, Length(Cleaned) - 2);
  Cleaned := Trim(Cleaned);
  if Cleaned = '' then Exit;

  Parts := SplitString(Cleaned, ',');
  FS    := InvariantFS;
  SetLength(Result, Length(Parts));
  for I := 0 to High(Parts) do
    if not TryStrToFloat(Trim(Parts[I]), Result[I], FS) then
      Result[I] := 0.0;
end;

class function TAiEmbeddingDataRec.EmbeddingToString(const AEmbedding: TAiEmbeddingData): string;
var
  SS: TStringStream;
  FS: TFormatSettings;
  I : Integer;
begin
  if Length(AEmbedding) = 0 then
  begin
    Result := '[]';
    Exit;
  end;
  FS := InvariantFS;
  SS := TStringStream.Create('');
  try
    SS.WriteString('[');
    SS.WriteString(FloatToStr(AEmbedding[0], FS));
    for I := 1 to High(AEmbedding) do
    begin
      SS.WriteString(',');
      SS.WriteString(FloatToStr(AEmbedding[I], FS));
    end;
    SS.WriteString(']');
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Operadores de TAiEmbeddingDataRec
// ---------------------------------------------------------------------------

operator + (const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensión');
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] + B.FData[I];
end;

operator - (const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensión');
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] - B.FData[I];
end;

operator * (const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] * Scalar;
end;

operator * (const A, B: TAiEmbeddingDataRec): Double;
var
  I: Integer;
begin
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensión');
  Result := 0;
  for I := 0 to High(A.FData) do
    Result := Result + (A.FData[I] * B.FData[I]);
end;

operator / (const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  Assert(Scalar <> 0, 'División por cero');
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] / Scalar;
end;

operator = (const A, B: TAiEmbeddingDataRec): Boolean;
var
  I: Integer;
begin
  if Length(A.FData) <> Length(B.FData) then
  begin
    Result := False;
    Exit;
  end;
  for I := 0 to High(A.FData) do
    if A.FData[I] <> B.FData[I] then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

operator <> (const A, B: TAiEmbeddingDataRec): Boolean;
begin
  Result := not (A = B);
end;

end.
