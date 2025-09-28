﻿unit uMakerAi.Embeddings.core;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Generics.Collections,
  System.Generics.Defaults, System.JSON, System.Math; // Added System.Math for SameValue

type
  // Defines the type for a single embedding vector.
  TAiEmbeddingData = TArray<Double>;
  // Defines a list of embedding vectors.
  TAiEmbeddingList = TArray<TAiEmbeddingData>;

  // Represents the result of a similarity search.
  TAiSimilarityResult = record
    Index: Integer;       // The index of the vector in the original list.
    Score: Double;        // The similarity score (e.g., cosine similarity).
    Vector: TAiEmbeddingData; // Optional: the found vector itself.
  end;

  // A list of similarity results, typically used for Top-K searches.
  TAiSimilarityList = TArray<TAiSimilarityResult>;


  // Event handler for custom embedding generation.
  TOnGetEmbedding = procedure(Sender: TObject;
    const aInput, aUser, aModel, aEncodingFormat: String;
    aDimensions: Integer; var aEmbedding: TAiEmbeddingData) of object;


TAiEmbeddingDataRec = record
  private
    FData: TAiEmbeddingData;
  public
    // --- Constructores y conversiones ---
    class function FromArray(const A: TAiEmbeddingData): TAiEmbeddingDataRec; static;
    function ToArray: TAiEmbeddingData;

    // --- Propiedades ---
    property Data: TAiEmbeddingData read FData write FData;

    // --- Operadores sobrecargados ---
    class operator Add(const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
    class operator Subtract(const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
    class operator Multiply(const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec; overload;
    class operator Multiply(const A, B: TAiEmbeddingDataRec): Double; overload; // Dot Product
    class operator Divide(const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec;
    class operator Equal(const A, B: TAiEmbeddingDataRec): Boolean;
    class operator NotEqual(const A, B: TAiEmbeddingDataRec): Boolean;

    // --- Métodos útiles ---
    function Magnitude: Double;
    procedure Normalize;
  end;




  // Base class for AI Embeddings.
  TAiEmbeddingsCore = class(TComponent)
  private
    FOnGetEmbedding: TOnGetEmbedding;
    procedure SetData(const Value: TAiEmbeddingData);
  protected
    FData: TAiEmbeddingData;
    FModel: String;
    FDimensions: Integer;
    Ftotal_tokens: Integer;
    Fprompt_tokens: Integer;
  public
    constructor Create(aOwner: TComponent); override;

    function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1;
      aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; virtual;

    // Utility methods not dependent on an external API.
    function ToJsonArray: TJsonArray; overload;
    class function ToJsonArray(Val: TAiEmbeddingData): TJsonArray; overload;
    class function ToEmbeddingData(Value: TJsonArray): TAiEmbeddingData;

    // Vector arithmetic helper methods.
    class function Magnitude(const V: TAiEmbeddingData): Double;
    class function DotProduct(const A, B: TAiEmbeddingData): Double;
    class function CosineSimilarity(const A, B: TAiEmbeddingData): Double;
    class function EuclideanDistance(const A, B: TAiEmbeddingData): Double;
    class procedure Normalize(var V: TAiEmbeddingData);
    class function FindNearest(const Query: TAiEmbeddingData; const Candidates: TAiEmbeddingList): TAiSimilarityResult;
    class function FindTopK(const Query: TAiEmbeddingData; const Candidates: TAiEmbeddingList; K: Integer): TAiSimilarityList;
    class function VectorAdd(const A, B: TAiEmbeddingData): TAiEmbeddingData;
    class function VectorSubtract(const A, B: TAiEmbeddingData): TAiEmbeddingData;
    class function AverageEmbedding(const Vectors: TAiEmbeddingList): TAiEmbeddingData;

    property Data: TAiEmbeddingData read FData write SetData;
  published
    property Model: String read FModel write FModel;
    property Dimensions: Integer read FDimensions write FDimensions;
    property prompt_tokens: Integer read Fprompt_tokens;
    property total_tokens: Integer read Ftotal_tokens;

    // Event that allows for custom embedding implementation.
    property OnGetEmbedding: TOnGetEmbedding read FOnGetEmbedding write FOnGetEmbedding;
  end;

implementation

{ TAiEmbeddingsCore }

constructor TAiEmbeddingsCore.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  // Default generic values
  FDimensions := 1536;
  FModel := 'custom-model';
end;

function TAiEmbeddingsCore.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
var
  LModel: String;
  LDimensions: Integer;
begin
  if not Assigned(FOnGetEmbedding) then
    raise Exception.Create('The OnGetEmbedding event has not been assigned. Cannot generate embedding.');

  // Use parameters if provided, otherwise use property values.
  if aModel <> '' then
    LModel := aModel
  else
    LModel := Self.FModel;
  if aDimensions > 0 then
    LDimensions := aDimensions
  else
    LDimensions := Self.FDimensions;

  // Clear previous result
  SetLength(Result, 0);

  // Invoke the event to let the user generate the embedding.
  FOnGetEmbedding(Self, aInput, aUser, LModel, aEncodingFormat, LDimensions, Result);

  // Save the result to the Data property and return it.
  Self.FData := Result;
end;

procedure TAiEmbeddingsCore.SetData(const Value: TAiEmbeddingData);
begin
  FData := Value;
end;

class function TAiEmbeddingsCore.Magnitude(const V: TAiEmbeddingData): Double;
var
  Sum: Double;
  i: Integer;
begin
  Sum := 0.0;
  for i := Low(V) to High(V) do
    Sum := Sum + Sqr(V[i]);
  Result := Sqrt(Sum);
end;

class procedure TAiEmbeddingsCore.Normalize(var V: TAiEmbeddingData);
var
  mag: Double;
  i: Integer;
begin
  mag := Magnitude(V);
  if mag = 0 then
    Exit; // Cannot normalize a zero vector.

  for i := 0 to High(V) do
    V[i] := V[i] / mag;
end;

class function TAiEmbeddingsCore.DotProduct(const A, B: TAiEmbeddingData): Double;
var
  i: Integer;
begin
  if Length(A) <> Length(B) then
    raise Exception.Create('Vectors must have the same dimension for dot product.');

  Result := 0.0;
  for i := Low(A) to High(A) do
    Result := Result + A[i] * B[i];
end;

class function TAiEmbeddingsCore.EuclideanDistance(const A, B: TAiEmbeddingData): Double;
var
  i: Integer;
  SumOfSquares: Double;
begin
  if Length(A) <> Length(B) then
    raise Exception.Create('Vectors must have the same dimension for Euclidean distance.');

  SumOfSquares := 0.0;
  for i := 0 to High(A) do
    SumOfSquares := SumOfSquares + Sqr(A[i] - B[i]);

  Result := Sqrt(SumOfSquares);
end;

class function TAiEmbeddingsCore.FindNearest(const Query: TAiEmbeddingData; const Candidates: TAiEmbeddingList): TAiSimilarityResult;
var
  i: Integer;
  bestScore: Double;
  currentScore: Double;
begin
  if Length(Candidates) = 0 then
    raise Exception.Create('The list of candidates cannot be empty.');

  bestScore := -2.0; // Start with a value lower than any possible cosine similarity (-1 to 1).
  Result.Index := -1;

  for i := 0 to High(Candidates) do
  begin
    currentScore := CosineSimilarity(Query, Candidates[i]);
    if currentScore > bestScore then
    begin
      bestScore := currentScore;
      Result.Index := i;
      Result.Score := currentScore;
    end;
  end;

  if Result.Index <> -1 then
    Result.Vector := Candidates[Result.Index];
end;

class function TAiEmbeddingsCore.FindTopK(const Query: TAiEmbeddingData; const Candidates: TAiEmbeddingList; K: Integer): TAiSimilarityList;
var
  AllResults: TList<TAiSimilarityResult>;
  i: Integer;
  aResult: TAiSimilarityResult;
begin
  AllResults := TList<TAiSimilarityResult>.Create;
  try
    for i := 0 to High(Candidates) do
    begin
      aResult.Index := i;
      aResult.Score := CosineSimilarity(Query, Candidates[i]);
      aResult.Vector := Candidates[i];
      AllResults.Add(aResult);
    end;

    // Sort the list of results by score in descending order.
    AllResults.Sort(TComparer<TAiSimilarityResult>.Construct(
      function(const L, R: TAiSimilarityResult): Integer
      begin
        Result := CompareValue(R.Score, L.Score); // R vs L for descending
      end));

    // Return the top K results.
    if K > AllResults.Count then
      K := AllResults.Count;
    Result := Copy(AllResults.ToArray, 0, K);

  finally
    AllResults.Free;
  end;
end;

class function TAiEmbeddingsCore.AverageEmbedding(const Vectors: TAiEmbeddingList): TAiEmbeddingData;
var
  i, j: Integer;
  SumVector: TAiEmbeddingData;
begin
  if Length(Vectors) = 0 then
  begin
    Result := nil;
    Exit;
  end;

  // Initialize the sum vector with the dimensions of the first vector.
  SetLength(SumVector, Length(Vectors[0]));

  for i := 0 to High(Vectors) do
  begin
    // Ensure all vectors have the same length.
    if Length(Vectors[i]) <> Length(SumVector) then
      raise Exception.Create('All vectors must have the same dimension to be averaged.');

    for j := 0 to High(SumVector) do
      SumVector[j] := SumVector[j] + Vectors[i][j];
  end;

  // Divide each component by the total number of vectors.
  Result := SumVector;
  for i := 0 to High(Result) do
    Result[i] := Result[i] / Length(Vectors);
end;

class function TAiEmbeddingsCore.CosineSimilarity(const A, B: TAiEmbeddingData): Double;
var
  MagA, MagB: Double;
begin
  MagA := Magnitude(A);
  MagB := Magnitude(B);
  if (MagA = 0) or (MagB = 0) then
    Result := 0 // To avoid division by zero.
  else
    Result := DotProduct(A, B) / (MagA * MagB);
end;

class function TAiEmbeddingsCore.ToEmbeddingData(Value: TJsonArray): TAiEmbeddingData;
Var
  j: Integer;
begin
  SetLength(Result, Value.Count);
  for j := 0 to Value.Count - 1 do
    Result[j] := Value.Items[j].GetValue<Double>;
end;

class function TAiEmbeddingsCore.ToJsonArray(Val: TAiEmbeddingData): TJsonArray;
var
  i: Integer;
begin
  Result := TJsonArray.Create;
  for i := 0 to Length(Val) - 1 do
    Result.Add(Val[i]);
end;

class function TAiEmbeddingsCore.VectorAdd(const A, B: TAiEmbeddingData): TAiEmbeddingData;
var
  i: Integer;
begin
  if Length(A) <> Length(B) then
    raise Exception.Create('Vectors must have the same dimension to be added.');

  SetLength(Result, Length(A));
  for i := 0 to High(A) do
    Result[i] := A[i] + B[i];
end;

class function TAiEmbeddingsCore.VectorSubtract(const A, B: TAiEmbeddingData): TAiEmbeddingData;
var
  i: Integer;
begin
  if Length(A) <> Length(B) then
    raise Exception.Create('Vectors must have the same dimension to be subtracted.');

  SetLength(Result, Length(A));
  for i := 0 to High(A) do
    Result[i] := A[i] - B[i];
end;

function TAiEmbeddingsCore.ToJsonArray: TJsonArray;
begin
  Result := ToJsonArray(Self.FData);
end;


{ TAiEmbeddingDataRec }

class function TAiEmbeddingDataRec.FromArray(const A: TAiEmbeddingData): TAiEmbeddingDataRec;
begin
  Result.FData := Copy(A);
end;

function TAiEmbeddingDataRec.ToArray: TAiEmbeddingData;
begin
  Result := Copy(FData);
end;

class operator TAiEmbeddingDataRec.Add(const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensión');
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] + B.FData[I];
end;

class operator TAiEmbeddingDataRec.Subtract(const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensión');
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] - B.FData[I];
end;

class operator TAiEmbeddingDataRec.Multiply(const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] * Scalar;
end;

class operator TAiEmbeddingDataRec.Multiply(const A, B: TAiEmbeddingDataRec): Double;
var
  I: Integer;
begin
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensión');
  Result := 0;
  for I := 0 to High(A.FData) do
    Result := Result + (A.FData[I] * B.FData[I]);
end;

class operator TAiEmbeddingDataRec.Divide(const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  Assert(Scalar <> 0, 'División por cero');
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] / Scalar;
end;

class operator TAiEmbeddingDataRec.Equal(const A, B: TAiEmbeddingDataRec): Boolean;
var
  I: Integer;
begin
  if Length(A.FData) <> Length(B.FData) then
    Exit(False);
  for I := 0 to High(A.FData) do
    if A.FData[I] <> B.FData[I] then
      Exit(False);
  Result := True;
end;

class operator TAiEmbeddingDataRec.NotEqual(const A, B: TAiEmbeddingDataRec): Boolean;
begin
  Result := not (A = B);
end;

function TAiEmbeddingDataRec.Magnitude: Double;
begin
  Result := Sqrt(Self * Self); // usa el dot product
end;

procedure TAiEmbeddingDataRec.Normalize;
var
  Mag: Double;
  I: Integer;
begin
  Mag := Magnitude;
  if Mag > 0 then
    for I := 0 to High(FData) do
      FData[I] := FData[I] / Mag;
end;


end.
