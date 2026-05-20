// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enr�quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Embeddings.core;

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
    Class function StringToEmbedding(Const AVectorString: String): TAiEmbeddingData; static;
    Class Function EmbeddingToString(Const AEmbedding : TAiEmbeddingData) : String; static;

    // --- M�todos �tiles ---
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
  QueryMag, CandidateMag, Dot: Double;
begin
  if Length(Candidates) = 0 then
    raise Exception.Create('The list of candidates cannot be empty.');

  bestScore := -2.0; // Iniciar m�s bajo que cualquier similitud coseno posible (-1 a 1).
  Result.Index := -1;

  // OPTIMIZACI�N: Calcular la magnitud del Query UNA SOLA VEZ fuera del bucle.
  QueryMag := Magnitude(Query);

  // Si el Query es un vector cero, no se puede buscar similitud.
  if QueryMag = 0 then Exit;

  for i := 0 to High(Candidates) do
  begin
    // Calculamos los componentes manualmente para aprovechar el QueryMag pre-calculado
    // en lugar de llamar a CosineSimilarity() que lo recalcular�a.
    Dot := DotProduct(Query, Candidates[i]);

    if IsZero(Dot) then
      currentScore := 0.0
    else
    begin
      CandidateMag := Magnitude(Candidates[i]);
      if CandidateMag = 0 then
        currentScore := 0.0
      else
        currentScore := Dot / (QueryMag * CandidateMag);
    end;

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
  QueryMag, CandidateMag, Dot: Double;
begin
  AllResults := TList<TAiSimilarityResult>.Create;
  try
    // OPTIMIZACI�N: Pre-calcular magnitud del Query
    QueryMag := Magnitude(Query);

    for i := 0 to High(Candidates) do
    begin
      aResult.Index := i;
      aResult.Vector := Candidates[i];

      // L�gica de similitud optimizada inline
      if QueryMag = 0 then
        aResult.Score := 0
      else
      begin
        Dot := DotProduct(Query, Candidates[i]);
        if IsZero(Dot) then
          aResult.Score := 0
        else
        begin
          CandidateMag := Magnitude(Candidates[i]);
          if CandidateMag = 0 then
            aResult.Score := 0
          else
            aResult.Score := Dot / (QueryMag * CandidateMag);
        end;
      end;

      AllResults.Add(aResult);
    end;

    // Ordenar resultados por Score descendente
    AllResults.Sort(TComparer<TAiSimilarityResult>.Construct(
      function(const L, R: TAiSimilarityResult): Integer
      begin
        Result := CompareValue(R.Score, L.Score);
      end));

    // OPTIMIZACI�N DE MEMORIA:
    // En lugar de hacer Copy() sobre el array (que duplica memoria),
    // recortamos la lista si es necesario y devolvemos su array directo.
    if K < AllResults.Count then
      AllResults.Count := K; // Esto elimina el exceso de elementos internamente

    Result := AllResults.ToArray;

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
  MagA, MagB, Dot: Double;
begin
  // 1. Calcular Producto Punto
  Dot := DotProduct(A, B);

  // Optimizaci�n: Si los vectores son ortogonales (dot=0), la similitud es 0.
  // Esto evita calcular ra�ces cuadradas costosas innecesariamente.
  if IsZero(Dot) then
    Exit(0.0);

  // 2. Calcular Magnitudes
  MagA := Magnitude(A);
  MagB := Magnitude(B);

  if (MagA = 0) or (MagB = 0) then
    Result := 0 // Evitar divisi�n por cero
  else
    Result := Dot / (MagA * MagB);
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
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensi�n');
  SetLength(Result.FData, Length(A.FData));
  for I := 0 to High(A.FData) do
    Result.FData[I] := A.FData[I] + B.FData[I];
end;

class function TAiEmbeddingDataRec.StringToEmbedding(Const AVectorString: String): TAiEmbeddingData;
var
  CleanedString: string;
  ValueStrings: TArray<string>;
  I: Integer;
  FormatSettings: TFormatSettings;
begin
  Result := []; // Devuelve un array vac�o por defecto
  if AVectorString.IsEmpty or (AVectorString = '[]') then
    Exit;

  // 1. Limpiar el string, quitando los corchetes
  CleanedString := AVectorString.Trim(['[', ']']);
  if CleanedString.IsEmpty then
    Exit;

  // 2. Separar los valores por la coma
  ValueStrings := CleanedString.Split([',']);

  // 3. Preparar para la conversi�n de float insensible a la localizaci�n
  // Esto asegura que el '.' siempre se interprete como el separador decimal.
  FormatSettings := TFormatSettings.Invariant;

  // 4. Convertir cada valor y a�adirlo al resultado
  SetLength(Result, Length(ValueStrings));
  for I := 0 to High(ValueStrings) do
  begin
    // Usamos TryStrToFloat para m�s seguridad contra datos mal formados
    if TryStrToFloat(ValueStrings[I], Result[I], FormatSettings) then
    begin
      // La conversi�n fue exitosa, continuar.
    end
    else
    begin
      Result[I] := 0.0; // O manejar el error como se prefiera
    end;
  end;
end;


class function TAiEmbeddingDataRec.EmbeddingToString(Const AEmbedding : TAiEmbeddingData) : String;
var
  I: Integer;
  FormatSettings: TFormatSettings;
  StringBuilder: TStringBuilder;
begin
  // Si el array est� vac�o, devolver '[]'
  if Length(AEmbedding) = 0 then
  begin
    Result := '[]';
    Exit;
  end;

  // Preparar FormatSettings para usar punto como separador decimal
  FormatSettings := TFormatSettings.Invariant;

  // Usar StringBuilder para mejor performance con strings largos
  StringBuilder := TStringBuilder.Create;
  try
    StringBuilder.Append('[');

    // Agregar el primer elemento
    StringBuilder.Append(FloatToStr(AEmbedding[0], FormatSettings));

    // Agregar los elementos restantes con coma
    for I := 1 to High(AEmbedding) do
    begin
      StringBuilder.Append(',');
      StringBuilder.Append(FloatToStr(AEmbedding[I], FormatSettings));
    end;

    StringBuilder.Append(']');
    Result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;


class operator TAiEmbeddingDataRec.Subtract(const A, B: TAiEmbeddingDataRec): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensi�n');
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
  Assert(Length(A.FData) = Length(B.FData), 'Vectores de distinta dimensi�n');
  Result := 0;
  for I := 0 to High(A.FData) do
    Result := Result + (A.FData[I] * B.FData[I]);
end;

class operator TAiEmbeddingDataRec.Divide(const A: TAiEmbeddingDataRec; const Scalar: Double): TAiEmbeddingDataRec;
var
  I: Integer;
begin
  Assert(Scalar <> 0, 'Divisi�n por cero');
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


