
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

unit uMakerAi.RAG.Vectors.Index;


interface

uses
  System.SysUtils, System.Math, System.Generics.Collections, System.Variants, System.StrUtils,
  System.Generics.Defaults, System.Classes, System.JSon, Rest.JSon, System.SyncObjs,
  System.NetEncoding, System.Hash,

  uMakerAi.Embeddings.Core, uMakerAi.RAG.MetaData;

Type

  TAiEmbeddingNode = Class;

  TAiRagIndexType = (TAIBasicIndex, TAIHNSWIndex, TAIEuclideanIndex);
  TOnFilterItem = procedure(Sender: TObject; const aNode: TAiEmbeddingNode; var aInclude: Boolean) of object;

  TRagItems = TList<TAiEmbeddingNode>;

  TAiEmbeddingNode = class
  private
    FData: TAiEmbeddingData;
    FDim: Integer;
    FText: String;
    FIdx: Double;
    FModel: String;
    FMetaData: TAiEmbeddingMetaData;

    FTag: string; // GUID o ID de DB (m�s vers�til que Integer)
    FTagObject: TObject; // Referencia a objetos externos
    FOrden: Integer;
    FMagnitude: Double;
    procedure SetData(const Value: TAiEmbeddingData); // Ordenamiento custom

  public
    constructor Create(aDim: Integer);
    destructor Destroy; override;

    // M�todos de c�lculo vectorial
    class function CosineSimilarity(const A, B: TAiEmbeddingNode): Double;
    class function DotProduct(const A, B: TAiEmbeddingNode): Double;
    class function Magnitude(const A: TAiEmbeddingNode): Double;

    // Serializaci�n
    function ToJSON: TJSONObject;
    class function FromJSON(AJSONObject: TJSONObject): TAiEmbeddingNode;
    function ToJsonArray: TJSonArray; overload;
    class function ToJsonArray(Val: TAiEmbeddingNode): TJSonArray; overload;

    // Utilidades
    procedure SetDataLength(aDim: Integer);

    // Propiedades (sin setters triviales)
    property Data: TAiEmbeddingData read FData write SetData;
    property Text: String read FText write FText;
    property Idx: Double read FIdx write FIdx;
    property Model: String read FModel write FModel;
    property MetaData: TAiEmbeddingMetaData read FMetaData;
    property Tag: string read FTag write FTag;
    property TagObject: TObject read FTagObject write FTagObject;
    property Orden: Integer read FOrden write FOrden;
    property Dim: Integer read FDim;
    property MagnitudeValue: Double read FMagnitude;
  end;


  // ============================================================================
  // RECORD AUXILIAR PARA RESULTADOS THREAD-SAFE
  // ============================================================================
  /// <summary>
  /// Estructura inmutable para almacenar resultados de b�squeda sin modificar
  /// los nodos originales. Esto elimina race conditions en escenarios multihilo.
  /// </summary>
  TAiSearchResult = record
    Node: TAiEmbeddingNode;
    FScore: Double;

    class function Create(aNode: TAiEmbeddingNode; aScore: Double): TAiSearchResult; static;

    /// <summary>
    /// Comparador para ordenamiento descendente por score
    /// </summary>
    class function CompareDescending(const Left, Right: TAiSearchResult): Integer; static;
    Property Score: Double Read FScore Write FScore;
  end;


  /// ---------------------------------------------------------------------------
  /// TAIEmbeddingIndex representa la clase base para la b�squeda con embeddings en memoria
  /// consiste en un vector de nodos y un indice de punteros a embeddings que permite la
  /// b�squeda y seleccion de los candidatos que cumplen la condici�n
  /// -------------------------------------------------------------------------
  TAIEmbeddingIndex = class
  private
    FDataVec: TRagItems;
    FActive: Boolean;
  protected
    /// <summary>
    /// Limpia el �ndice interno pero mantiene la referencia al DataVec
    /// </summary>
    procedure InternalClear; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure BuildIndex(Points: TRagItems); virtual;
    function Add(Point: TAiEmbeddingNode): Integer; virtual;
    function Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double): TRagItems; virtual;

    /// <summary>
    /// Limpia el �ndice y lo deja listo para reconstruir
    /// </summary>
    procedure Clear; virtual;

    property DataVec: TRagItems read FDataVec write FDataVec;
    property Active: Boolean read FActive;
  end;


  /// ---------------------------------------------------------------------------
  /// TAIBasicEmbeddingIndex implementaci�n sencilla de un Indice de embeddings
  /// el cual se asigna por defecto al vector para realizar b�squedas en memoria
  /// sin embargo hay maneras m�s eficientes de controlar esto en vectores de
  /// embeddings.
  /// -------------------------------------------------------------------------

  TAIBasicEmbeddingIndex = class(TAIEmbeddingIndex)
  private
    procedure BuildIndex(Points: TRagItems);
    function Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double): TRagItems;
  public
    /// <summary>
    /// Versi�n thread-safe que NO modifica los nodos originales
    /// </summary>
    class function InternalSearchSafe(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; Source: TRagItems): TList<TAiSearchResult>; static;

    /// <summary>
    /// Versi�n legacy para compatibilidad (DEPRECATED)
    /// </summary>
    class function InternalSearch(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; Source: TRagItems): TRagItems; static;
    constructor Create;
    destructor Destroy; override;
  end;

  TL2Pair = TPair<Double, TAiEmbeddingNode>;


  TAIEuclideanDistanceIndex = class(TAIEmbeddingIndex)
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure BuildIndex(Points: TRagItems); Override;
    Function Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double): TRagItems; Override;
  end;


  /// ---------------------------------------------------------------------------
  /// THNSWIndex implementa un Approximate Nearest Neighbors (ANN) usando el algoritmo
  /// HNSW (Hierarchical Navigable Small World) que es mucho m�s eficiente en la busqueda
  /// en vectores embeddings
  /// -------------------------------------------------------------------------

  TConnListArray = array of TList<Integer>;

  THNSWNode = class
  private
    FID: Integer;
    FVector: TAiEmbeddingNode;
    FConnections: array of TList<Integer>;
  public
    constructor Create(aID: Integer; aVector: TAiEmbeddingNode; aNumLevels: Integer);
    destructor Destroy; override;

    property ID: Integer read FID;
    property Vector: TAiEmbeddingNode read FVector;
    // property Connections: array of TList<Integer> read FConnections;
    function GetConnections(Level: Integer): TList<Integer>;
    function LevelCount: Integer;
  end;

  THNSWIndex = class(TAIEmbeddingIndex)
  private
    FNodes: TDictionary<Integer, THNSWNode>;
    FEntryPoint: Integer;
    FEntryLevel: Integer;
    FMaxLevel: Integer;
    FLevelMult: Double;
    FEfConstruction: Integer;
    FMaxConnections: Integer;

    function GetRandomLevel: Integer;
    procedure InsertConnection(Node: THNSWNode; Level: Integer; TargetID: Integer);
    function SearchLayer(Query: TAiEmbeddingNode; EntryPoint: Integer; Level: Integer; Ef: Integer): TList<Integer>;
  protected
    /// <summary>
    /// Limpia el grafo HNSW sin destruir el diccionario
    /// </summary>
    procedure InternalClear; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// Reconstruye el �ndice desde cero (thread-safe)
    /// </summary>
    procedure BuildIndex(Points: TRagItems); override;

    function Add(Point: TAiEmbeddingNode): Integer; override;
    function Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double): TRagItems; override;

    /// <summary>
    /// Limpia el grafo y reinicia al estado inicial
    /// </summary>
    procedure Clear; override;
  end;


  // Representa la frecuencia de una palabra en un nodo espec�fico
  TWordOccurrence = record
    Node: TAiEmbeddingNode;
    Count: Integer;
  end;

  TAIBm25Index = class
  private
    FInvertedIndex: TDictionary<string, TList<TWordOccurrence>>;
    FDocLengths: TDictionary<TAiEmbeddingNode, Integer>;
    FAvgDocLength: Double;
    FLanguage: TAiLanguage;
{$IF CompilerVersion >= 35}
    FStopWords: THashSet<string>;
{$ELSE}
    FStopWords: TDictionary<string, Boolean>;
{$ENDIF}
    procedure SetLanguage(const Value: TAiLanguage);
    procedure LoadDefaultStopWords(Lang: TAiLanguage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNode(aNode: TAiEmbeddingNode); // Para indexar al a�adir
    function Search(const aQuery: string; aLimit: Integer; aFilter: TAiFilterCriteria = nil): TList<TPair<Double, TAiEmbeddingNode>>;

    procedure Clear;

    property Language: TAiLanguage read FLanguage write SetLanguage;
{$IF CompilerVersion >= 35}
    property StopWords: THashSet<string> read FStopWords; // Permite a�adir palabras personalizadas
{$ELSE}
    property StopWords: TDictionary<string, Boolean> read FStopWords;
{$ENDIF}
    procedure Tokenize(const aText: string; aList: TStrings);

  end;



implementation

{$IF CompilerVersion < 35}
uses
  uJSONHelper;
{$ENDIF}

{ TAiEmbeddingNode }

constructor TAiEmbeddingNode.Create(aDim: Integer);
begin
  inherited Create;
  FDim := aDim;
  SetLength(FData, FDim);
  FMetaData := TAiEmbeddingMetaData.Create;
  FTag := TGuid.NewGuid.ToString; // ID �nico por defecto
  FOrden := 0;
  FTagObject := nil;
  FMagnitude := 0;
end;

destructor TAiEmbeddingNode.Destroy;
begin
  FMetaData.Free;
  // Nota: NO liberamos FTagObject (no somos owner)
  inherited;
end;

procedure TAiEmbeddingNode.SetData(const Value: TAiEmbeddingData);
var
  i: Integer;
  Sum: Double;
begin
  FData := Value;
  FDim := Length(Value);
  Sum := 0;
  // Calculamos la magnitud al vuelo para que MagnitudeValue no sea 0
  for i := 0 to High(FData) do
    Sum := Sum + (FData[i] * FData[i]);
  FMagnitude := Sqrt(Sum);
end;

procedure TAiEmbeddingNode.SetDataLength(aDim: Integer);
begin
  FDim := aDim;
  SetLength(FData, FDim);
end;

function TAiEmbeddingNode.ToJSON: TJSONObject;
var
  JSONArray: TJSonArray;
  Value: Double;
begin
  Result := TJSONObject.Create;
  try
    // Array de embeddings
    JSONArray := TJSonArray.Create;
    for Value in FData do
      JSONArray.Add(Value);
    Result.AddPair('data', JSONArray);

    // Campos b�sicos
    Result.AddPair('text', FText);
    Result.AddPair('model', FModel);
    Result.AddPair('tag', FTag);
    Result.AddPair('orden', FOrden);
    Result.AddPair('idx', FIdx);

    // Metadatos
    if Assigned(FMetaData) then
      Result.AddPair('metadata', FMetaData.ToJSON);
  except
    Result.Free;
    raise;
  end;
end;

class function TAiEmbeddingNode.FromJSON(AJSONObject: TJSONObject): TAiEmbeddingNode;
var
  JSONArray: TJSonArray;
  i: Integer;
  jMeta: TJSONObject;
  Sum: Double; // <--- 1. Agregamos variable para acumular
begin
  if not Assigned(AJSONObject) then
    raise Exception.Create('AJSONObject no puede ser nil');

  // Crear instancia
  JSONArray := AJSONObject.GetValue<TJSonArray>('data');
  Result := TAiEmbeddingNode.Create(JSONArray.Count);

  try
    Sum := 0.0; // <--- 2. Inicializamos suma

    // Cargar datos del embedding
    for i := 0 to JSONArray.Count - 1 do
    begin
      Result.FData[i] := JSONArray.Items[i].AsType<Double>;

      // <--- 3. Calculamos la suma de cuadrados al vuelo (sin recorrer el array dos veces)
      Sum := Sum + (Result.FData[i] * Result.FData[i]);
    end;

    // <--- 4. Asignamos la magnitud final
    Result.FMagnitude := Sqrt(Sum);

    // Campos b�sicos
    AJSONObject.TryGetValue<String>('text', Result.FText);
    AJSONObject.TryGetValue<String>('model', Result.FModel);
    AJSONObject.TryGetValue<String>('tag', Result.FTag);
    AJSONObject.TryGetValue<Integer>('orden', Result.FOrden);
    AJSONObject.TryGetValue<Double>('idx', Result.FIdx);

    // Metadatos
    if AJSONObject.TryGetValue<TJSONObject>('metadata', jMeta) then
      Result.FMetaData.FromJSON(jMeta);
  except
    Result.Free;
    raise;
  end;
end;

function TAiEmbeddingNode.ToJsonArray: TJSonArray;
var
  Value: Double;
begin
  Result := TJSonArray.Create;
  for Value in FData do
    Result.Add(Value);
end;

class function TAiEmbeddingNode.ToJsonArray(Val: TAiEmbeddingNode): TJSonArray;
var
  Value: Double;
begin
  if not Assigned(Val) then
    raise Exception.Create('Val no puede ser nil');

  Result := TJSonArray.Create;
  for Value in Val.FData do
    Result.Add(Value);
end;

class function TAiEmbeddingNode.DotProduct(const A, B: TAiEmbeddingNode): Double;
var
  i: Integer;
begin
  if Length(A.FData) <> Length(B.FData) then
    raise Exception.Create('Los vectores deben ser de la misma longitud');

  Result := 0;
  for i := Low(A.FData) to High(A.FData) do
    Result := Result + A.FData[i] * B.FData[i];
end;

class function TAiEmbeddingNode.Magnitude(const A: TAiEmbeddingNode): Double;
var
  Sum: Double;
  i: Integer;
begin
  Sum := 0.0;
  for i := Low(A.FData) to High(A.FData) do
    Sum := Sum + A.FData[i] * A.FData[i];
  Result := Sqrt(Sum);
end;

class function TAiEmbeddingNode.CosineSimilarity(const A, B: TAiEmbeddingNode): Double;
begin
  // VALIDACI�N CR�TICA: Si alguno no tiene magnitud, no hay similitud posible (0.0)
  if (A.FMagnitude <= 0) or (B.FMagnitude <= 0) then
    Exit(0.0);

  // Producto punto / (MagA * MagB)
  Result := TAiEmbeddingNode.DotProduct(A, B) / (A.FMagnitude * B.FMagnitude);

  // Limpieza por precisi�n de punto flotante
  if Result > 1.0 then
    Result := 1.0
  else if Result < -1.0 then
    Result := -1.0;
end;

function CompareEmbeddings(const Left, Right: TAiEmbeddingNode; Axis: Integer): Integer;
const
  TOLERANCE = 1.0E-12;
begin
  if Abs(Left.Data[Axis] - Right.Data[Axis]) < TOLERANCE then
    Result := 0
  else if Left.Data[Axis] < Right.Data[Axis] then
    Result := -1
  else
    Result := 1;
end;


{ TAiSearchResult }

class function TAiSearchResult.Create(aNode: TAiEmbeddingNode; aScore: Double): TAiSearchResult;
begin
  Result.Node := aNode;
  Result.Score := aScore;
end;

class function TAiSearchResult.CompareDescending(const Left, Right: TAiSearchResult): Integer;
begin
  // Orden descendente (Mayor score primero)
  Result := CompareValue(Right.Score, Left.Score);
end;


{ TAIEmbeddingIndex }

constructor TAIEmbeddingIndex.Create;
begin
  inherited;
  FDataVec := nil;
  FActive := False;
end;

destructor TAIEmbeddingIndex.Destroy;
begin
  // No liberamos FDataVec - no somos due�os
  inherited;
end;

function TAIEmbeddingIndex.Add(Point: TAiEmbeddingNode): Integer;
begin
  Result := -1;
  // Esta funci�n se debe implementar en cada modelo solo cuando sea necesario
end;

procedure TAIEmbeddingIndex.BuildIndex(Points: TRagItems);
begin
  FDataVec := Points;
  FActive := True;
end;

function TAIEmbeddingIndex.Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double): TRagItems;
begin
  Result := nil; // Debe ser implementado en clases derivadas
end;

procedure TAIEmbeddingIndex.Clear;
begin
  InternalClear;
  FActive := False;
end;


{ TAOIBasicIndex }

procedure TAIBasicEmbeddingIndex.BuildIndex(Points: TRagItems);
begin
  if not Assigned(Points) then
    Exit; // <- Protecci�n
  inherited;
end;

constructor TAIBasicEmbeddingIndex.Create;
begin
  Inherited;
end;

destructor TAIBasicEmbeddingIndex.Destroy;
begin

  inherited;
end;

class function TAIBasicEmbeddingIndex.InternalSearch(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; Source: TRagItems): TRagItems;
var
  SafeResults: TList<TAiSearchResult>;
  SearchResult: TAiSearchResult;
begin
  Result := TRagItems.Create;

  // Usar la versi�n thread-safe internamente
  SafeResults := InternalSearchSafe(Target, aLimit, aPrecision, Source);
  try
    for SearchResult in SafeResults do
    begin
      // AQU� es donde asignamos Idx (compatible con c�digo existente)
      SearchResult.Node.Idx := SearchResult.Score;
      Result.Add(SearchResult.Node);
    end;
  finally
    SafeResults.Free;
  end;
end;

class function TAIBasicEmbeddingIndex.InternalSearchSafe(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; Source: TRagItems): TList<TAiSearchResult>;
var
  i: Integer;
  Emb: TAiEmbeddingNode;
  Score: Double;
  TempList: TList<TAiSearchResult>;
begin
  Result := TList<TAiSearchResult>.Create;
  TempList := TList<TAiSearchResult>.Create;

  try
    // ========================================================================
    // FASE 1: Calcular similitudes (100% Thread-Safe)
    // ========================================================================
    // No modificamos los nodos originales en absoluto.
    // Cada hilo trabaja con su propia lista local.

    for i := 0 to Source.Count - 1 do
    begin
      Emb := Source.Items[i];
      Score := TAiEmbeddingNode.CosineSimilarity(Emb, Target);

      // Filtrado previo por precisi�n (optimizaci�n)
      if (aPrecision > 0) and (Score < aPrecision) then
        Continue;

      TempList.Add(TAiSearchResult.Create(Emb, Score));
    end;

    // ========================================================================
    // FASE 2: Ordenar (100% Thread-Safe)
    // ========================================================================
    // Cada hilo ordena su propia lista sin tocar datos compartidos

    TempList.Sort(TComparer<TAiSearchResult>.Construct(
      function(const Left, Right: TAiSearchResult): Integer
      begin
        Result := TAiSearchResult.CompareDescending(Left, Right);
      end));

    // ========================================================================
    // FASE 3: Construir resultado (100% Thread-Safe)
    // ========================================================================
    // Simplemente copiamos las referencias - NO escribimos en los nodos

    for i := 0 to Min(aLimit - 1, TempList.Count - 1) do
    begin
      Result.Add(TempList[i]); // Solo lectura de nodos compartidos
    end;

  finally
    TempList.Free;
  end;
end;

function TAIBasicEmbeddingIndex.Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double): TRagItems;
var
  SafeList: TList<TAiSearchResult>;
  Res: TAiSearchResult;
begin
  Result := TRagItems.Create;

  // Usamos la versi�n "Safe" que ya creaste y que usa registros temporales
  SafeList := InternalSearchSafe(Target, aLimit, aPrecision, DataVec);
  try
    for Res in SafeList do
      Result.Add(Res.Node);
    // Nota: El score se recuperar� en el motor principal.
  finally
    SafeList.Free;
  end;
end;

{ TAIEuclideanDistanceIndex }

procedure TAIEuclideanDistanceIndex.BuildIndex(Points: TRagItems);
begin
  Inherited; // La implementaci�n base es suficiente
end;

constructor TAIEuclideanDistanceIndex.Create;
begin
  Inherited;
end;

destructor TAIEuclideanDistanceIndex.Destroy;
begin
  Inherited;
end;

function TAIEuclideanDistanceIndex.Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double): TRagItems;
Var
  i: Integer;
  Emb: TAiEmbeddingNode;
  Distance: Double;
  Pair: TL2Pair;
  Results: TList<TL2Pair>; // Usamos una lista temporal para no modificar la original
begin
  Result := TRagItems.Create;
  Results := TList < TPair < Double, TAiEmbeddingNode >>.Create;

  try
    // 1. Calcula la distancia Euclidiana para todos los elementos y los guarda en una lista temporal
    for i := 0 to DataVec.Count - 1 do
    begin
      Emb := DataVec.Items[i];
      // Aqu� usamos la funci�n de la unidad Core, que ya tienes
      Distance := TAiEmbeddingsCore.EuclideanDistance(Emb.Data, Target.Data);
      Emb.Idx := Distance; // Guardamos la distancia para referencia/debugging

      // aPrecision en este contexto significa "distancia m�xima"
      // Si aPrecision <= 0, ignoramos el filtro de precisi�n
      if (aPrecision <= 0) or (Distance <= aPrecision) then
      begin
        Pair := TL2Pair.Create(Distance, Emb);
        Results.Add(Pair);
      end;
    end;

    // 2. Ordena la lista de resultados por distancia en orden ASCENDENTE (menor es mejor)
    Results.Sort(TComparer < TPair < Double, TAiEmbeddingNode >>.Construct(
      function(const Left, Right: TPair<Double, TAiEmbeddingNode>): Integer
      begin
        // Left.Key vs Right.Key para orden ascendente
        Result := CompareValue(Left.Key, Right.Key);
      end));

    // 3. Recorre la lista ordenada y a�ade los mejores 'aLimit' resultados
    for i := 0 to Min(aLimit - 1, Results.Count - 1) do
    begin
      Result.Add(Results[i].Value);
    end;

  finally
    Results.Free;
  end;
end;



{ THNSWNode }

constructor THNSWNode.Create(aID: Integer; aVector: TAiEmbeddingNode; aNumLevels: Integer);
var
  i: Integer;
begin
  inherited Create;
  FID := aID;
  FVector := aVector;

  // Inicializaci�n segura del array de conexiones
  SetLength(FConnections, aNumLevels);
  try
    for i := 0 to aNumLevels - 1 do
      FConnections[i] := TList<Integer>.Create;
  except
    // Si falla la creaci�n de alguna lista, limpiamos lo ya creado
    for i := 0 to High(FConnections) do
    begin
      if Assigned(FConnections[i]) then
        FConnections[i].Free;
    end;
    raise; // Re-lanzamos la excepci�n
  end;
end;

destructor THNSWNode.Destroy;
var
  i: Integer;
begin
  // Liberaci�n defensiva - Protecci�n triple
  if Length(FConnections) > 0 then
  begin
    for i := 0 to High(FConnections) do
    begin
      // Free es seguro con nil, pero la doble validaci�n no hace da�o
      if Assigned(FConnections[i]) then
        FreeAndNil(FConnections[i]);
    end;
  end;

  // IMPORTANTE: NO liberamos FVector - no somos due�os
  // FVector es una referencia a un nodo del TAiRAGVector principal

  inherited;
end;

function THNSWNode.GetConnections(Level: Integer): TList<Integer>;
begin
  if (Level < 0) or (Level >= Length(FConnections)) then
    raise EArgumentOutOfRangeException.CreateFmt('Nivel %d fuera de rango [0..%d]', [Level, Length(FConnections) - 1]);

  Result := FConnections[Level];
end;

function THNSWNode.LevelCount: Integer;
begin
  Result := Length(FConnections);
end;

function THNSWIndex.GetRandomLevel: Integer;
var
  R: Double;
begin
  R := Random;
  if R <= 0 then
    R := 1E-12; // evita ln(0)

  Result := Floor(-Ln(R) * FLevelMult);

  if Result >= FMaxLevel then
    Result := FMaxLevel - 1;
end;

{ THNSWIndex }

constructor THNSWIndex.Create;
begin
  inherited;

  // Configuraci�n HNSW
  FNodes := TDictionary<Integer, THNSWNode>.Create;
  FMaxLevel := 16;
  FLevelMult := 1 / Ln(2);
  FEfConstruction := 40;
  FMaxConnections := 16;
  FEntryPoint := -1;
  FEntryLevel := -1;
end;

destructor THNSWIndex.Destroy;
begin
  // Limpieza centralizada
  Clear;

  // Liberar el diccionario contenedor
  FNodes.Free;

  inherited;
end;

procedure THNSWIndex.InternalClear;
var
  Node: THNSWNode;
begin
  if (FNodes = nil) then
    Exit;

  try
    for Node in FNodes.Values do
      if Assigned(Node) then
        Node.Free;

    FNodes.Clear;
  except
    // Capturar cualquier error extra�o durante la limpieza para no colgar el IDE
  end;
end;

procedure THNSWIndex.Clear;
begin
  // Llamar a la limpieza interna
  InternalClear;

  // Reiniciar estado del grafo
  FEntryPoint := -1;
  FEntryLevel := -1;

  // NOTA: Los par�metros de configuraci�n (FMaxLevel, FEfConstruction, etc.)
  // NO se resetean - son configuraci�n persistente

  // Marcar como inactivo
  FActive := False;
end;

procedure THNSWIndex.InsertConnection(Node: THNSWNode; Level: Integer; TargetID: Integer);
var
  CurrentList: TList<Integer>;
  TargetNode, NeighborNode: THNSWNode;
  TargetSim, NeighborSim, MinSim: Double;
  WorstIndex, i: Integer;
begin
  CurrentList := Node.GetConnections(Level);

  // Evitar duplicados
  if CurrentList.Contains(TargetID) then
    Exit;

  // Si hay espacio, agregar directamente
  if CurrentList.Count < FMaxConnections then
  begin
    CurrentList.Add(TargetID);
    Exit;
  end;

  // Pol�tica de selecci�n (mantener los vecinos m�s cercanos)
  if not FNodes.TryGetValue(TargetID, TargetNode) then
    Exit;

  TargetSim := TAiEmbeddingNode.CosineSimilarity(Node.Vector, TargetNode.Vector);

  MinSim := 2.0;
  WorstIndex := -1;

  for i := 0 to CurrentList.Count - 1 do
  begin
    if FNodes.TryGetValue(CurrentList[i], NeighborNode) then
    begin
      NeighborSim := TAiEmbeddingNode.CosineSimilarity(Node.Vector, NeighborNode.Vector);
      if NeighborSim < MinSim then
      begin
        MinSim := NeighborSim;
        WorstIndex := i;
      end;
    end;
  end;

  if (WorstIndex > -1) and (TargetSim > MinSim) then
    CurrentList[WorstIndex] := TargetID;
end;

function THNSWIndex.SearchLayer(Query: TAiEmbeddingNode; EntryPoint: Integer; Level: Integer; Ef: Integer): TList<Integer>;
var
  Visited: TDictionary<Integer, Boolean>;
  Candidates: TList<TPair<Double, Integer>>;
  BestCandidates: TList<TPair<Double, Integer>>;
  CurrentNode: THNSWNode;
  Distance: Double;
  i: Integer;
begin
  Result := TList<Integer>.Create;
  Visited := TDictionary<Integer, Boolean>.Create;
  Candidates := TList < TPair < Double, Integer >>.Create;
  BestCandidates := TList < TPair < Double, Integer >>.Create;

  try
    CurrentNode := FNodes[EntryPoint];
    Distance := TAiEmbeddingNode.CosineSimilarity(Query, CurrentNode.Vector);

    Candidates.Add(TPair<Double, Integer>.Create(Distance, EntryPoint));
    BestCandidates.Add(TPair<Double, Integer>.Create(Distance, EntryPoint));
    Visited.Add(EntryPoint, True);

    while Candidates.Count > 0 do
    begin
      Candidates.Sort(TComparer < TPair < Double, Integer >>.Construct(
        function(const Left, Right: TPair<Double, Integer>): Integer
        begin
          if Left.Key > Right.Key then
            Result := -1
          else if Left.Key < Right.Key then
            Result := 1
          else
            Result := 0;
        end));

      CurrentNode := FNodes[Candidates[0].Value];
      Candidates.Delete(0);

      for i in CurrentNode.GetConnections(Level) do
      begin
        if not Visited.ContainsKey(i) then
        begin
          Visited.Add(i, True);
          Distance := TAiEmbeddingNode.CosineSimilarity(Query, FNodes[i].Vector);

          if (BestCandidates.Count < Ef) or (Distance > BestCandidates.Last.Key) then
          begin
            Candidates.Add(TPair<Double, Integer>.Create(Distance, i));
            BestCandidates.Add(TPair<Double, Integer>.Create(Distance, i));

            if BestCandidates.Count > Ef then
            begin
              BestCandidates.Sort(TComparer < TPair < Double, Integer >>.Construct(
                function(const Left, Right: TPair<Double, Integer>): Integer
                begin
                  if Left.Key > Right.Key then
                    Result := -1
                  else if Left.Key < Right.Key then
                    Result := 1
                  else
                    Result := 0;
                end));
              BestCandidates.Delete(BestCandidates.Count - 1);
            end;
          end;
        end;
      end;
    end;

    for i := 0 to BestCandidates.Count - 1 do
      Result.Add(BestCandidates[i].Value);

  finally
    Visited.Free;
    Candidates.Free;
    BestCandidates.Free;
  end;
end;

function THNSWIndex.Add(Point: TAiEmbeddingNode): Integer;
var
  NodeID: Integer;
  Level: Integer;
  CurrentLevel: Integer;
  EntryPointCopy: Integer;
  W: TList<Integer>;
  Node: THNSWNode;
  i: Integer;
begin
  // Asignar ID �nico basado en el conteo actual
  NodeID := FNodes.Count;
  Level := GetRandomLevel;

  // Crear nuevo nodo HNSW
  Node := THNSWNode.Create(NodeID, Point, FMaxLevel);
  try
    FNodes.Add(NodeID, Node);
  except
    Node.Free; // Si falla la inserci�n en el diccionario, limpiamos
    raise;
  end;

  // Si es el primer nodo, se convierte en punto de entrada
  if FEntryPoint = -1 then
  begin
    FEntryPoint := NodeID;
    FEntryLevel := Level;
    Result := NodeID;
    Exit;
  end;

  // Insertar en la estructura HNSW (c�digo original se mantiene)
  EntryPointCopy := FEntryPoint;
  CurrentLevel := FMaxLevel - 1;

  // Descender niveles superiores
  while CurrentLevel > Level do
  begin
    W := SearchLayer(Point, EntryPointCopy, CurrentLevel, 1);
    try
      if W.Count > 0 then
        EntryPointCopy := W[0];
    finally
      W.Free;
    end;
    Dec(CurrentLevel);
  end;

  // Insertar en niveles objetivo
  while CurrentLevel >= 0 do
  begin
    W := SearchLayer(Point, EntryPointCopy, CurrentLevel, FEfConstruction);
    try
      for i in W do
      begin
        InsertConnection(Node, CurrentLevel, i);
        InsertConnection(FNodes[i], CurrentLevel, NodeID);
      end;
      if W.Count > 0 then
        EntryPointCopy := W[0];
    finally
      W.Free;
    end;
    Dec(CurrentLevel);
  end;

  // Actualizar punto de entrada si este nodo tiene m�s niveles

  if Level > FEntryLevel then
  begin
    FEntryPoint := NodeID;
    FEntryLevel := Level;
  end;

  Result := NodeID;
end;

procedure THNSWIndex.BuildIndex(Points: TRagItems);
var
  i: Integer;
  Point: TAiEmbeddingNode;
begin
  if not Assigned(Points) then
    raise Exception.Create('THNSWIndex.BuildIndex: Points no puede ser nil');

  // ========================================================================
  // PASO 1: Limpiar el �ndice anterior
  // ========================================================================
  // CR�TICO: Esto previene fugas de memoria si BuildIndex se llama 2+ veces
  Clear;

  // ========================================================================
  // PASO 2: Asignar la referencia a los datos
  // ========================================================================

  inherited BuildIndex(Points); // Asigna FDataVec y FActive := True

  // ========================================================================
  // PASO 3: Construir el grafo HNSW
  // ========================================================================
  // Optimizaci�n: Pre-reservar capacidad si tu versi�n de Delphi lo soporta
  // FNodes.Capacity := Points.Count; // No disponible en todas las versiones

  for i := 0 to Points.Count - 1 do
  begin
    Point := Points.Items[i];
    Add(Point);
  end;

  // El �ndice ahora est� activo (heredado ya lo marc�)
end;

function THNSWIndex.Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double): TRagItems;
var
  CurrentLevel: Integer;
  EntryPointCopy: Integer;
  W: TList<Integer>;
  ResultList: TList<TPair<Double, Integer>>;
  i: Integer;
  Distance: Double;
  Node: THNSWNode;
begin
  Result := TRagItems.Create;

  if FEntryPoint = -1 then
    Exit;

  ResultList := TList < TPair < Double, Integer >>.Create;
  try
    EntryPointCopy := FEntryPoint;
    CurrentLevel := FMaxLevel - 1;

    while CurrentLevel >= 0 do
    begin
      W := SearchLayer(Target, EntryPointCopy, CurrentLevel, 1);
      try
        if W.Count > 0 then
          EntryPointCopy := W[0];
      finally
        W.Free;
      end;
      Dec(CurrentLevel);
    end;

    // W := SearchLayer(Target, EntryPointCopy, 0, aLimit * 2);

    W := SearchLayer(Target, EntryPointCopy, 0, Max(100, aLimit * 2));

    try
      for i in W do
      begin
        Node := FNodes[i];
        Distance := TAiEmbeddingNode.CosineSimilarity(Target, Node.Vector);
        if Distance >= aPrecision then
          ResultList.Add(TPair<Double, Integer>.Create(Distance, i));
      end;

      ResultList.Sort(TComparer < TPair < Double, Integer >>.Construct(
        function(const Left, Right: TPair<Double, Integer>): Integer
        begin
          if Left.Key > Right.Key then
            Result := -1
          else if Left.Key < Right.Key then
            Result := 1
          else
            Result := 0;
        end));

      for i := 0 to Min(aLimit - 1, ResultList.Count - 1) do
      begin
        Node := FNodes[ResultList[i].Value];
        Node.Vector.Idx := ResultList[i].Key;
        Result.Add(Node.Vector);
      end;
    finally
      W.Free;
    end;
  finally
    ResultList.Free;
  end;
end;


{ TAIBm25Index }

constructor TAIBm25Index.Create;
begin
  inherited Create;
  FInvertedIndex := TDictionary < string, TList < TWordOccurrence >>.Create;
  FDocLengths := TDictionary<TAiEmbeddingNode, Integer>.Create;
{$IF CompilerVersion >= 35}
  FStopWords := THashSet<string>.Create;
{$ELSE}
  FStopWords := TDictionary<string, Boolean>.Create;
{$ENDIF}
  FAvgDocLength := 0;
  Language := alSpanish; // Por defecto
end;

destructor TAIBm25Index.Destroy;
var
  List: TList<TWordOccurrence>;
begin
  for List in FInvertedIndex.Values do
    List.Free;
  FInvertedIndex.Free;
  FDocLengths.Free;
  inherited;
end;

procedure TAIBm25Index.LoadDefaultStopWords(Lang: TAiLanguage);
const
  S_ES = 'el,la,lo,los,las,un,una,unos,unas,de,del,al,y,o,u,e,ni,que,en,a,ante,bajo,con,contra,desde,donde,durante,' +
         'este,esta,estos,estas,ese,esa,esos,esas,aquel,aquella,aquellos,aquellas,mi,tu,su,nuestro,vuestro,' +
         'me,te,se,nos,os,le,les,ser,estar,haber,tener,hacer,hay,he,ha,han,son,es,fue,sido,' +
         'est�,est�n,estaba,como,m�s,pero,por,para,sin,sobre,tambi�n,muy,ya,s�,no,cuando,si,entre';
  S_EN = 'the,a,an,and,or,but,if,then,else,when,at,from,by,for,with,about,against,between,into,through,' +
         'during,before,after,above,below,to,of,in,is,are,was,were,be,been,being,have,has,had,' +
         'do,does,did,will,would,should,could,may,might,can,this,that,these,those,i,you,he,she,it,we,they,me,him,her,us,them';
var
  S: string;
  Words: TArray<string>;
  W, CleanWord: string;
begin
  FStopWords.Clear;

  case Lang of
    alSpanish:
      S := S_ES;
    alEnglish:
      S := S_EN;
  else
    S := '';
  end;

  if S <> '' then
  begin
    Words := S.Split([',']);
    for W in Words do
    begin
      CleanWord := W.Trim.ToLower;
      if CleanWord <> '' then
{$IF CompilerVersion >= 35}
        FStopWords.Add(CleanWord);
{$ELSE}
        FStopWords.AddOrSetValue(CleanWord, True);
{$ENDIF}
    end;
  end;
end;

procedure TAIBm25Index.Clear;
var
  List: TList<TWordOccurrence>;
begin
  for List in FInvertedIndex.Values do
    List.Free;
  FInvertedIndex.Clear;
  FDocLengths.Clear;
  FAvgDocLength := 0;
end;

procedure TAIBm25Index.Tokenize(const aText: string; aList: TStrings);
var
  Words: TArray<string>;
  W: string;
begin
  aList.Clear;
  if aText.IsEmpty then
    Exit;

  // Split con separadores extendidos (incluyendo caracteres especiales de espa�ol)
  Words := aText.ToLower.Split([' ', '.', ',', ';', ':', '-', '_', '(', ')', '[', ']', '{', '}', '"', '�', '?', '�', '!', '/', '\', '|', #13, #10, #9], // A�ad� TAB (#9)
  TStringSplitOptions.ExcludeEmpty);

  for W in Words do
  begin
    // Filtro de longitud m�nima y stop words (usando el HashSet para O(1))
{$IF CompilerVersion >= 35}
    if (W.Length > 2) and not FStopWords.Contains(W) then
{$ELSE}
    if (W.Length > 2) and not FStopWords.ContainsKey(W) then
{$ENDIF}
    begin
      // OPCIONAL: Lematizaci�n b�sica para mejorar b�squedas
      // if W.EndsWith('s') and (W.Length > 3) then
      // W := W.Substring(0, W.Length - 1);

      aList.Add(W);
    end;
  end;
end;

procedure TAIBm25Index.AddNode(aNode: TAiEmbeddingNode);
var
  Tokens: TStringList;
  WordCounts: TDictionary<string, Integer>;
  W: string;
  OccurList: TList<TWordOccurrence>;
  Occur: TWordOccurrence;
  TotalDocs, CurrentCount: Integer;
begin
  if aNode.Text.IsEmpty then
    Exit;

  Tokens := TStringList.Create;
  WordCounts := TDictionary<string, Integer>.Create;
  try
    Tokenize(aNode.Text, Tokens);
    if Tokens.Count = 0 then
      Exit;

    // 1. Contar frecuencias de palabras en este documento
    for W in Tokens do
    begin
      // TryGetValue es la forma m�s eficiente:
      // Intenta obtener el valor en 'CurrentCount'. Devuelve True si existe.
      if WordCounts.TryGetValue(W, CurrentCount) then
      begin
        // CASO A: Ya existe.
        // Incrementamos el valor que recuperamos y lo sobrescribimos.
        WordCounts[W] := CurrentCount + 1;
      end
      else
      begin
        // CASO B: No existe.
        // Lo ADICIONAMOS expl�citamente con valor 1.
        WordCounts.Add(W, 1);
      end;
    end;

    // 2. Registrar longitud del documento
    FDocLengths.Add(aNode, Tokens.Count);

    // 3. Actualizar el �ndice invertido
    for W in WordCounts.Keys do
    begin
      if not FInvertedIndex.TryGetValue(W, OccurList) then
      begin
        OccurList := TList<TWordOccurrence>.Create;
        FInvertedIndex.Add(W, OccurList);
      end;

      Occur.Node := aNode;
      Occur.Count := WordCounts[W];
      OccurList.Add(Occur);
    end;

    // 4. Recalcular el promedio de longitud de todos los documentos
    TotalDocs := FDocLengths.Count;
    FAvgDocLength := ((FAvgDocLength * (TotalDocs - 1)) + Tokens.Count) / TotalDocs;

  finally
    WordCounts.Free;
    Tokens.Free;
  end;
end;

function TAIBm25Index.Search(const aQuery: string; aLimit: Integer; aFilter: TAiFilterCriteria = nil): TList<TPair<Double, TAiEmbeddingNode>>;
const
  k1 = 1.2; // Saturaci�n de frecuencia
  B = 0.75; // Penalizaci�n por longitud
var
  QueryTokens: TStringList;
  QW: string;
  OccurList: TList<TWordOccurrence>;
  Occur: TWordOccurrence;
  Scores: TDictionary<TAiEmbeddingNode, Double>;
  N, n_q, f_q_d: Integer;
  IDF, Score, D_len: Double;
  Pair: TPair<TAiEmbeddingNode, Double>;
begin
  Result := TList < TPair < Double, TAiEmbeddingNode >>.Create;

  // 1. Validaciones iniciales
  if aQuery.IsEmpty or (FDocLengths.Count = 0) then
    Exit;

  QueryTokens := TStringList.Create;
  Scores := TDictionary<TAiEmbeddingNode, Double>.Create;
  try
    // 2. Tokenizar la consulta
    Tokenize(aQuery, QueryTokens);
    if QueryTokens.Count = 0 then
      Exit;

    // N = n�mero total de documentos
    N := FDocLengths.Count;

    // 3. Procesar cada token (OR l�xico)
    for QW in QueryTokens do
    begin
      if FInvertedIndex.TryGetValue(QW, OccurList) then
      begin
        // n_q = documentos que contienen el t�rmino
        n_q := OccurList.Count;

        // IDF probabil�stico (RSJ)
        IDF := Ln((N - n_q + 0.5) / (n_q + 0.5) + 1.0);

        // Recorrer ocurrencias
        for Occur in OccurList do
        begin
          // --- PRE-FILTERING CON CRITERIA ---
          if Assigned(aFilter) and (aFilter.Count > 0) then
          begin
            // Nuevo m�todo centralizado
            if not Occur.Node.MetaData.Matches(aFilter) then
              Continue;
          end;
          // ---------------------------------

          // Frecuencia del t�rmino en el documento
          f_q_d := Occur.Count;

          // Longitud del documento
          D_len := FDocLengths[Occur.Node];

          // BM25
          Score := IDF * (f_q_d * (k1 + 1)) / (f_q_d + k1 * (1 - B + B * (D_len / FAvgDocLength)));

          // Acumular score
          if Scores.ContainsKey(Occur.Node) then
            Scores[Occur.Node] := Scores[Occur.Node] + Score
          else
            Scores.Add(Occur.Node, Score);
        end;
      end;
    end;

    // 4. Convertir a lista
    for Pair in Scores do
      Result.Add(TPair<Double, TAiEmbeddingNode>.Create(Pair.Value, Pair.Key));

    // 5. Ordenar por score descendente
    Result.Sort(TComparer < TPair < Double, TAiEmbeddingNode >>.Construct(
      function(const L, R: TPair<Double, TAiEmbeddingNode>): Integer
      begin
        Result := CompareValue(R.Key, L.Key);
      end));

    // 6. Limitar resultados
    if (aLimit > 0) and (Result.Count > aLimit) then
      Result.Count := aLimit;

  finally
    Scores.Free;
    QueryTokens.Free;
  end;
end;

procedure TAIBm25Index.SetLanguage(const Value: TAiLanguage);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    if FLanguage <> alCustom then
      LoadDefaultStopWords(FLanguage);
  end;
end;


end.
