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

unit uMakerAi.RAG.Vectors;

interface

uses
  System.SysUtils, System.Math, System.Generics.Collections, System.Variants, System.StrUtils,
  System.Generics.Defaults, System.Classes, System.JSon, Rest.JSon, System.SyncObjs,
  System.NetEncoding, System.Hash,

   uMakerAi.RAG.Vectors.Index,  uMakerAi.RAG.Vectors.VQL,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.Embeddings.Core, uMakerAi.RAG.MetaData;

type

  /// ---------------------------------------------------------------------------
  /// TAiEmbeddingNode identifica un embedding, cincluyendo la longitud
  /// el modelo y los datos, permite adicionalmente comparar dos embeddings
  /// para conocer su similitud por coseno,  convierte de json a vector y de
  /// vector a json
  /// almacena tambi�n el dato de texto original del embedding
  /// -------------------------------------------------------------------------


  TAiRAGVector = Class;

  TOnDataVecAddItem = Procedure(Sender: TObject; aItem: TAiEmbeddingNode; MetaData: TAiEmbeddingMetaData; Var Handled: Boolean) of object;
  TOnDataVecSearch = procedure(Sender: TObject; Target: TAiEmbeddingNode; const aPrompt: string; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria; var aDataVec: TAiRAGVector; var Handled: Boolean) of object;
  TOnImportProgress = procedure(Sender: TObject; Position, Total: Integer; var Cancel: Boolean) of object;



  /// ---------------------------------------------------------------------------
  /// TAIBasicEmbeddingIndex implementaci�n sencilla de un Indice de embeddings
  /// el cual se asigna por defecto al vector para realizar b�squedas en memoria
  /// sin embargo hay maneras m�s eficientes de controlar esto en vectores de
  /// embeddings.
  /// -------------------------------------------------------------------------




  /// ---------------------------------------------------------------------------
  /// THNSWIndex implementa un Approximate Nearest Neighbors (ANN) usando el algoritmo
  /// HNSW (Hierarchical Navigable Small World) que es mucho m�s eficiente en la busqueda
  /// en vectores embeddings
  /// -------------------------------------------------------------------------




  TAiSearchOptions = class(TPersistent)
  private
    FUseEmbeddings: Boolean;
    FUseBM25: Boolean;
    FUseRRF: Boolean;
    FUseReorderABC: Boolean;
    FBM25Weight: Double;
    FEmbeddingWeight: Double;
    FOnChange: TNotifyEvent;
    FMinAbsoluteScoreEmbedding: Double;
    FMinAbsoluteScoreBM25: Double;
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
    property UseEmbeddings: Boolean read FUseEmbeddings write SetUseEmbeddings default True;
    property UseBM25: Boolean read FUseBM25 write SetUseBM25 default True;
    property UseRRF: Boolean read FUseRRF write SetUseRRF default False;
    property UseReorderABC: Boolean read FUseReorderABC write SetUseReorderABC default False;
    property BM25Weight: Double read FBM25Weight write SetBM25Weight; // Default 0.7
    property EmbeddingWeight: Double read FEmbeddingWeight write SetEmbeddingWeight; // Default 0.3
    property MinAbsoluteScoreEmbedding: Double read FMinAbsoluteScoreEmbedding write FMinAbsoluteScoreEmbedding;
    property MinAbsoluteScoreBM25: Double read FMinAbsoluteScoreBM25 write FMinAbsoluteScoreBM25;

  end;

  TAiVectorStoreDriverBase = class(TComponent)
  public
    // Contrato del driver — cada implementacion sobreescribe estos metodos
    procedure Add(const aNode: TAiEmbeddingNode; const AEntidad: string); virtual; abstract;
    function Search(const ATarget: TAiEmbeddingNode; const AEntidad: string; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria; Options: TAiSearchOptions): TAiRAGVector; virtual; abstract;
    procedure Delete(const aID, AEntidad: string); virtual; abstract;
    procedure Clear(const AEntidad: string); virtual; abstract;
    // Helpers comunes
    function EmbeddingToString(const AData: TAiEmbeddingData): string;
    function VariantToJSONValue(const V: Variant): TJSONValue;
  end;

  /// ---------------------------------------------------------------------------
  /// TAiDataVec es la clase base que permite almacenar conjuntos de embeddings
  /// se utiliza tanto para representar bases de datos de embeddings en memoria
  /// como para la conexi�n con bases de datos de embeddings.
  /// Por si solo no indexa ni b�sca, solo es el contenedor, para buscar
  /// es necesario adicionar un TAIEmbeddingIndex, aunque por defecto tiene
  /// un indice b�sico de b�squeda, pero hay modelos mejores.
  /// -------------------------------------------------------------------------
  TAiRAGVector = Class(TComponent)
  Private
    FActive: Boolean;
    FRagIndex: TAIEmbeddingIndex;
    FEmbeddings: TAiEmbeddingsCore;
    FItems: TList<TAiEmbeddingNode>;
    FOnDataVecAddItem: TOnDataVecAddItem;
    FOnDataVecSearch: TOnDataVecSearch;
    FDim: Integer;
    FModel: String;
    FNameVec: String;
    FDescription: String;
    FInMemoryIndexType: TAiRagIndexType;
    FOnGetEmbedding: TOnGetEmbedding;
    FOnFilterItem: TOnFilterItem;
    FBm25Index: TAIBm25Index; // El �ndice l�xico en memoria
    FLock: TMultiReadExclusiveWriteSynchronizer;
    FDriver: TAiVectorStoreDriverBase;
    FEntidad: string;
    FOwnsObjects: Boolean;
    FOnImportProgress: TOnImportProgress;
    FSearchOptions: TAiSearchOptions;
    procedure SetActive(const Value: Boolean);
    procedure SetRagIndex(const Value: TAIEmbeddingIndex);
    procedure SetEmbeddings(const Value: TAiEmbeddingsCore);
    function GetItems: TList<TAiEmbeddingNode>;
    procedure SetOnDataVecAddItem(const Value: TOnDataVecAddItem);
    procedure SetOnDataVecSearch(const Value: TOnDataVecSearch);
    procedure SetDescription(const Value: String);
    procedure SetNameVec(const Value: String);
    procedure SetInMemoryIndexType(const Value: TAiRagIndexType);
    function ReciprocalRankFusion(VectorResults: TList<TAiSearchResult>; LexicalResults: TList<TPair<Double, TAiEmbeddingNode>>; aLimit: Integer): TAiRAGVector;

    function GetLexicalLanguage: TAiLanguage;
    procedure SetLexicalLanguage(const Value: TAiLanguage);
    procedure SetDriver(const Value: TAiVectorStoreDriverBase);
    procedure SetSearchOptions(const Value: TAiSearchOptions);
    // VQL advanced post-processing
    function  ApplyMMR(ASource: TAiRAGVector; ALambda: Double; ALimit: Integer): TAiRAGVector;
    procedure ApplyDistinct(AVector: TAiRAGVector; const AField: string);
    procedure ApplyOrderBy(AVector: TAiRAGVector; AFields: TList<TOrderByField>);
    function  BuildExplainOutput(AReq: TVGQLRequest): string;
    function  BuildExtendedContextText(DataVec: TAiRAGVector; AReq: TVGQLRequest): string;
  Protected

    function DoOnGetEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData;
    procedure InternalInit(AOwnsObjects: Boolean);
    procedure CheckIndexes;

    function InternalSearchSafe(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria): TList<TAiSearchResult>;
    function FilterByCriteria(const aCriteria: TAiFilterCriteria): TAiRAGVector;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function WeightedScoreFusion(VectorResults: TList<TAiSearchResult>; LexicalResults: TList<TPair<Double, TAiEmbeddingNode>>; aLimit: Integer): TAiRAGVector;
    procedure ApplyContextReordering(Vector: TAiRAGVector);
    procedure InternalNormalizeVector(aList: TList<TAiSearchResult>);
    procedure InternalNormalizeLexical(aList: TList < TPair < Double, TAiEmbeddingNode >> );

  Public
    Constructor Create(aOwner: TComponent; AOwnsObjects: Boolean); Reintroduce; Overload;
    Constructor Create(aOwner: TComponent); Overload; Override;

    Destructor Destroy; Override;

    Procedure SaveToStream(Stream: TMemoryStream);
    Function LoadFromStream(Stream: TStream): Integer;

    Procedure SaveToFile(FileName: String);
    Function LoadFromFile(FileName: String): Integer;
    Function Connect(aHost, aPort, aLogin, aPassword: String): Boolean;

    Function Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria): TAiRAGVector; Overload; // Definitivo
    Function Search(Prompt: String; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria): TAiRAGVector; Overload; // Definitivo

    Function SearchText(aPrompt: String; aLimit: Integer = 10; aPrecision: Double = 0.5; aFilter: TAiFilterCriteria = nil; IncludeMetadata: Boolean = True; IncludeScore: Boolean = True): String; Overload; Virtual;
    Function SearchText(aPrompt: TAiEmbeddingNode; aLimit: Integer = 10; aPresicion: Double = 0.5; aFilter: TAiFilterCriteria = nil; IncludeMetadata: Boolean = True; IncludeScore: Boolean = True): String; Overload; Virtual;

    function ExecuteVQL(const AVqlQuery: string): string; overload;

    function ExecuteVQL(const AVqlQuery: string; out AResultVector: TAiRAGVector): string; overload;

    Function VectorToContextText(DataVec: TAiRAGVector; IncludeMetadata: Boolean; IncludeScore: Boolean): String;

    procedure BuildIndex;
    procedure BuildLexicalIndex;

    function AddItem(aItem: TAiEmbeddingNode): string; overload;
    Function AddItem(aItem: TAiEmbeddingNode; MetaData: TAiEmbeddingMetaData): NativeInt; Overload; Virtual;
    Function AddItem(aText: String; MetaData: TAiEmbeddingMetaData = Nil): TAiEmbeddingNode; Overload; Virtual;

    Function AddItemsFromJSonArray(aJSonArray: TJSonArray; MetaData: TAiEmbeddingMetaData = Nil): Integer; Virtual;
    Function AddItemsFromPlainText(aText: String; MetaData: TAiEmbeddingMetaData; aLenChunk: Integer; aOverlapPct: Integer): Integer; Virtual;

    Function CreateEmbeddingNode(aText: String; aEmbeddings: TAiEmbeddingsCore = Nil): TAiEmbeddingNode;
    Function Count: Integer;
    Procedure Clear;

    procedure RegenerateAll(const aNewModel: String = ''); virtual;
    procedure Rerank(Target: TAiEmbeddingNode; aAutoRegenerate: Boolean = True); overload;
    procedure Rerank(NewPrompt: String; aAutoRegenerate: Boolean = True); overload;
    function FilterByMetaData(const aCriteria: TAiEmbeddingMetaData): TAiRAGVector;

    Property RagIndex: TAIEmbeddingIndex read FRagIndex write SetRagIndex;
    Property Active: Boolean read FActive write SetActive;
    Property Items: TList<TAiEmbeddingNode> read GetItems;
  Published
    Property OnDataVecAddItem: TOnDataVecAddItem read FOnDataVecAddItem write SetOnDataVecAddItem;
    Property OnDataVecSearch: TOnDataVecSearch read FOnDataVecSearch write SetOnDataVecSearch;
    Property OnGetEmbedding: TOnGetEmbedding read FOnGetEmbedding write FOnGetEmbedding;
    property OnFilterItem: TOnFilterItem read FOnFilterItem write FOnFilterItem;
    Property OnImportProgress: TOnImportProgress read FOnImportProgress write FOnImportProgress;

    Property Embeddings: TAiEmbeddingsCore read FEmbeddings write SetEmbeddings;
    Property Model: String read FModel;
    Property Dim: Integer read FDim;
    Property NameVec: String read FNameVec write SetNameVec;
    Property Description: String read FDescription write SetDescription;
    Property InMemoryIndexType: TAiRagIndexType read FInMemoryIndexType write SetInMemoryIndexType default TAIHNSWIndex;
    property LexicalLanguage: TAiLanguage read GetLexicalLanguage write SetLexicalLanguage;
    property Driver: TAiVectorStoreDriverBase read FDriver write SetDriver;
    property Entidad: string read FEntidad write FEntidad;
    Property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    Property SearchOptions: TAiSearchOptions read FSearchOptions write SetSearchOptions;
  End;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiRAGVector]);

end;


{ TOAiDataVec }

function TAiRAGVector.AddItem(aItem: TAiEmbeddingNode): string;
begin
  // Delegamos a la sobrecarga principal para evitar duplicar l�gica
  // y asegurar que se ejecuten todos los pasos de indexaci�n.
  AddItem(aItem, nil);

  // Retornamos el Tag (GUID) como se espera
  Result := aItem.Tag;
end;

function TAiRAGVector.AddItem(aItem: TAiEmbeddingNode; MetaData: TAiEmbeddingMetaData): NativeInt;
var
  Handled: Boolean;
begin
  Handled := False;

  FLock.BeginWrite;
  try
    // 1. Sincronizar Metadatos si se pasaron externamente
    if Assigned(MetaData) and (MetaData <> aItem.MetaData) then
      aItem.MetaData.Assign(MetaData);

    // 2. Notificar al evento (posibilidad de manejo externo)
    if Assigned(FOnDataVecAddItem) then
      FOnDataVecAddItem(Self, aItem, aItem.MetaData, Handled);

    // 3. L�gica de Inserci�n
    if Handled then
    begin
      // CORRECCI�N CR�TICA:
      // Si el usuario marc� 'Handled', asumimos que �L tom� posesi�n del nodo.
      // NO lo liberamos aqu�. Retornamos -1 indicando que este vector no lo index�.
      Exit(-1);
    end;

    // A. Persistencia en Driver (Opcional)
    // Agregamos al driver PERO seguimos agregando a memoria para que
    // funcionen las b�squedas h�bridas locales.
    if Assigned(FDriver) then
    begin
      FDriver.Add(aItem, FEntidad);
      // No salimos aqu�, continuamos para poblar la memoria cach�/�ndice
    end;

    // B. Almacenamiento en Lista Principal (Memoria)
    Result := FItems.Add(aItem);

    // C. Indexaci�n L�xica (BM25)
    if FSearchOptions.FUseBM25 then
      FBm25Index.AddNode(aItem);

    // D. Indexaci�n Vectorial (HNSW / Basic)
    // CORRECCI�N: Antes esto faltaba en la primera sobrecarga
    if Assigned(FRagIndex) then
      FRagIndex.Add(aItem);

    // E. Actualizar metadatos del contenedor (Modelo/Dimensi�n)
    // Solo si es el primer item o para mantener consistencia
    if FItems.Count = 1 then
    begin
      FModel := aItem.Model;
      FDim := aItem.Dim;
    end;

  finally
    FLock.EndWrite;
  end;
end;

function TAiRAGVector.AddItem(aText: string; MetaData: TAiEmbeddingMetaData = Nil): TAiEmbeddingNode;
var
  vData: TAiEmbeddingData;
  AddedIndex: NativeInt;
begin
  if Trim(aText) = '' then
    raise Exception.Create('El texto no puede estar vac�o');

  // 1. Generar el Embedding
  if Assigned(FEmbeddings) then
    vData := FEmbeddings.CreateEmbedding(aText, 'user')
  else
    vData := DoOnGetEmbedding(aText, 'user');

  // 2. Crear el Nodo
  Result := TAiEmbeddingNode.Create(Length(vData));
  try
    Result.Text := aText;
    Result.Data := vData; // Esto calcula autom�ticamente la Magnitud si aplicaste la optimizaci�n anterior

    if Assigned(FEmbeddings) then
      Result.Model := FEmbeddings.Model
    else
      Result.Model := FModel;

    // 3. Asignar Metadatos
    if Assigned(MetaData) then
      Result.MetaData.Assign(MetaData);

    // 4. Registrar en el sistema llamando a la L�gica Maestra
    AddedIndex := Self.AddItem(Result, Result.MetaData);

    // 5. Gesti�n de Memoria post-intento
    if AddedIndex = -1 then
    begin
      // CASO ESPECIAL:
      // Creamos el nodo, pero AddItem devolvi� -1 (Handled).
      // Significa que TAiRAGVector NO tom� posesi�n del objeto en su lista FItems.
      // Pero como acabamos de crear el objeto aqu� (Result := ...Create),
      // si lo devolvemos, el usuario debe saber que �l es responsable de liberarlo.

      // Opci�n A (Segura): Si fue Handled, asumimos que el evento se lo llev�.
      // Devolvemos el Result tal cual.

      // Opci�n B (Estricta): Si no se agreg�, liberamos para evitar leaks si el usuario ignora el result.
      // Dado que es un "Factory+Add", lo normal es devolver el objeto creado aunque no se haya indexado localmente.
      // Mantenemos Result.
    end;

  except
    // Si algo falla durante la creaci�n o generaci�n de embeddings, liberar el nodo parcial
    Result.Free;
    raise;
  end;
end;

function TAiRAGVector.AddItemsFromJSonArray(aJSonArray: TJSonArray; MetaData: TAiEmbeddingMetaData): Integer;
Var
  JVal: TJSONValue;
  Emb: TAiEmbeddingNode;
  i: Integer;
  TextToEmbed: String;
  Cancel: Boolean;
  TotalCount: Integer;
begin
  Result := 0;
  Cancel := False;
  i := 0;

  if (aJSonArray = nil) then
    Exit;

  TotalCount := aJSonArray.Count;

  For JVal in aJSonArray do
  Begin
    // ---> INICIO LOGICA DE PROGRESO <---
    if Assigned(FOnImportProgress) then
    begin
      // Reportamos el �ndice actual 'i' sobre el total
      FOnImportProgress(Self, i, TotalCount, Cancel);
      if Cancel then
        Break;
    end;
    // ---> FIN LOGICA DE PROGRESO <---

    // Extraer texto limpio
    if JVal is TJSONString then
      TextToEmbed := JVal.Value
    else
      TextToEmbed := JVal.ToString;

    if not TextToEmbed.IsEmpty then
    begin
      Emb := AddItem(TextToEmbed, MetaData);
      if Assigned(Emb) then
      begin
        Emb.Orden := i;
        Inc(i);
        Inc(Result); // <--- CONTAMOS NODO CREADO
      end;
    end;
  End;

  // Reporte final 100%
  if Assigned(FOnImportProgress) and (not Cancel) then
    FOnImportProgress(Self, TotalCount, TotalCount, Cancel);
end;

function TAiRAGVector.AddItemsFromPlainText(aText: String; MetaData: TAiEmbeddingMetaData; aLenChunk: Integer; aOverlapPct: Integer): Integer;
const
  TOLERANCE_PCT = 0.15; // 15% de tolerancia para buscar cortes limpios
var
  TotalLen, StartPos, NextStartPos: Integer;
  IdealEnd, MaxEnd, MinEnd: Integer;
  CutPos: Integer;
  OverlapChars: Integer;
  ChunkText: String;
  Emb: TAiEmbeddingNode;
  i: Integer;
  Cancel: Boolean; // <--- NUEVA VARIABLE

  // Funci�n local para encontrar el mejor punto de corte con prioridades
  function FindSmartCut(LimitStart, LimitEnd: Integer): Integer;
  var
    k: Integer;
    LastPeriod, LastSpace: Integer;
    C: Char;
  begin
    LastPeriod := -1;
    LastSpace := -1;

    // Recorremos hacia atr�s desde el l�mite m�ximo permitido
    // Buscamos el corte "m�s lejano" posible que sea sem�nticamente correcto
    for k := LimitEnd downto LimitStart do
    begin
      if k > TotalLen then
        Continue;

      C := aText[k];

      // Prioridad 1: Salto de l�nea (P�rrafo)
      if (C = #10) or (C = #13) then
      begin
        Result := k;
        Exit; // Encontramos el mejor corte posible, salimos.
      end;

      // Prioridad 2: Puntuaci�n de fin de frase
      if CharInSet(C, ['.', '?', '!', ';']) and (LastPeriod = -1) then
        LastPeriod := k;

      // Prioridad 3: Espacio
      if (C = ' ') and (LastSpace = -1) then
        LastSpace := k;
    end;

    // Decisi�n basada en prioridades si no hubo salto de l�nea
    if LastPeriod <> -1 then
      Result := LastPeriod
    else if LastSpace <> -1 then
      Result := LastSpace
    else
      Result := -1; // No encontramos nada limpio
  end;

begin
  Result := 0; // <--- INICIALIZAMOS RESULTADO
  Cancel := False; // <--- INICIALIZAMOS CANCELACION

  // --- 1. Validaciones ---
  if aLenChunk <= 10 then
    raise Exception.Create('El tama�o del Chunk es demasiado peque�o.');

  if (aOverlapPct < 0) or (aOverlapPct >= 100) then
    raise Exception.Create('El porcentaje de Overlap debe estar entre 0 y 99.');

  // Normalizamos el texto (quitamos espacios extra al inicio/final)
  aText := aText.Trim;
  TotalLen := aText.Length; // Nota: Si usas versiones antiguas de Delphi usa Length(aText)

  if TotalLen = 0 then
    Exit;

  // Calculamos longitud de solapamiento en caracteres
  OverlapChars := Round(aLenChunk * (aOverlapPct / 100));

  // Si el solapamiento es absurdo, lo ajustamos
  if OverlapChars >= aLenChunk then
    OverlapChars := aLenChunk - 1;

  StartPos := 1; // Delphi Strings son base-1
  i := 0;

  // --- 2. Bucle de fragmentaci�n ---
  while StartPos <= TotalLen do
  begin
    // ---> INICIO LOGICA DE PROGRESO <---
    if Assigned(FOnImportProgress) then
    begin
      FOnImportProgress(Self, StartPos, TotalLen, Cancel);
      if Cancel then
        Break; // Salimos del bucle si el usuario cancela
    end;
    // ---> FIN LOGICA DE PROGRESO <---

    // Definimos la ventana de b�squeda ideal
    IdealEnd := StartPos + aLenChunk;

    // Si lo que queda es menor que el chunk, tomamos todo
    if IdealEnd > TotalLen then
    begin
      ChunkText := Copy(aText, StartPos, TotalLen - StartPos + 1);

      if ChunkText <> '' then
      begin
        if Assigned(Metadata) then
          Metadata.Properties['Posicion'] := I;
        Emb := AddItem(ChunkText, MetaData);
        if Assigned(Emb) then
        begin
          Emb.Orden := i;
          Inc(Result); // <--- CONTAMOS NODO CREADO
        end;
      end;
      Break; // Terminamos
    end;

    // Definimos rango de tolerancia:
    // Buscaremos desde un poco antes hasta un poco despu�s del IdealEnd
    MinEnd := IdealEnd - Round(aLenChunk * TOLERANCE_PCT);
    MaxEnd := IdealEnd + Round(aLenChunk * TOLERANCE_PCT);

    // No nos salimos del texto
    if MaxEnd > TotalLen then
      MaxEnd := TotalLen;
    // No retrocedemos antes del inicio actual
    if MinEnd < StartPos then
      MinEnd := StartPos;

    // Buscamos el corte inteligente
    CutPos := FindSmartCut(MinEnd, MaxEnd);

    // Si no encontramos un corte natural, cortamos a la fuerza en el IdealEnd
    if CutPos = -1 then
      CutPos := IdealEnd;

    // Extraemos el texto
    // +1 en Length si cortamos en un caracter inclusivo, pero Copy maneja count
    ChunkText := Copy(aText, StartPos, CutPos - StartPos + 1).Trim;

    if ChunkText <> '' then
    begin
      if Assigned(Metadata) then
      begin
        Metadata.Properties['Posicion'] := I;
        Metadata.Properties['FechaDoc'] := EncodeDate(Random(20)+2000,Random(11)+1,01);
      end;

      Emb := AddItem(ChunkText, MetaData);
      if Assigned(Emb) then
      begin
        Emb.Orden := i;
        Inc(Result); // <--- CONTAMOS NODO CREADO
      end;
      Inc(i);
    end;

    // --- 3. C�lculo del siguiente paso ---
    // El nuevo inicio es: Donde cortamos MENOS el solapamiento
    NextStartPos := (CutPos + 1) - OverlapChars;

    // SEGURIDAD ANTI-LOOP:
    // Aseguramos que siempre avanzamos al menos 1 caracter respecto al inicio actual
    if NextStartPos <= StartPos then
      NextStartPos := StartPos + 1;

    if (NextStartPos > 1) and (NextStartPos < TotalLen) then
    begin
      // Mientras el caracter ANTERIOR no sea un separador (espacio, punto, salto),
      // significa que estamos dentro de una palabra. Retrocedemos.
      while (NextStartPos > StartPos + 1) and // No retroceder m�s all� del inicio anterior
        (not CharInSet(aText[NextStartPos - 1], [' ', #13, #10, '.', ',', ';', ':', '!', '?'])) do
      begin
        Dec(NextStartPos);
      end;
    end;
    // =========================================================================

    StartPos := NextStartPos;
  end; // Fin del While

  // Opcional: Reportar 100% al finalizar si no se cancel�
  if Assigned(FOnImportProgress) and (not Cancel) then
    FOnImportProgress(Self, TotalLen, TotalLen, Cancel);
end;

procedure TAiRAGVector.ApplyContextReordering(Vector: TAiRAGVector);
var
  ReorderedArr: array of TAiEmbeddingNode;
  i, LeftIdx, RightIdx: Integer;
begin
  // 1. Validaciones b�sicas
  // Si es nil o tiene muy pocos elementos, no vale la pena reordenar
  if (Vector = nil) or (Vector.Count <= 2) then
    Exit;

  // 2. Inicializar estructura temporal
  // Usamos un array din�mico para acceso r�pido por �ndice
  SetLength(ReorderedArr, Vector.Count);

  LeftIdx := 0; // Puntero al inicio
  RightIdx := Vector.Count - 1; // Puntero al final

  // 3. Algoritmo de distribuci�n (Two-Pointers)
  // Input esperado: [1.Mejor, 2.Bueno, 3.Regular, 4.Malo, 5.Peor]
  for i := 0 to Vector.Count - 1 do
  begin
    if (i mod 2 = 0) then
    begin
      // Los �ndices PARES (0, 2, 4...) van al LADO IZQUIERDO.
      // i=0 (Mejor) -> Pos 0
      // i=2 (Regular) -> Pos 1
      ReorderedArr[LeftIdx] := Vector.Items[i];
      Inc(LeftIdx);
    end
    else
    begin
      // Los �ndices IMPARES (1, 3, 5...) van al LADO DERECHO.
      // i=1 (Bueno) -> Pos Final
      // i=3 (Malo) -> Pos Final - 1
      ReorderedArr[RightIdx] := Vector.Items[i];
      Dec(RightIdx);
    end;
  end;
  // Resultado visual: [Mejor, Regular, Peor, Malo, Bueno]
  // Atenci�n del LLM: [ALTA,  Media,   Baja, Media, ALTA]

  // 4. Volcar resultado al vector original
  // Limpiamos la lista de referencias interna.
  // IMPORTANTE: NO llamamos a Vector.Clear ni liberamos objetos,
  // solo vaciamos la lista de punteros para volver a llenarla en el nuevo orden.
  Vector.Items.Clear;

  for i := 0 to High(ReorderedArr) do
    Vector.Items.Add(ReorderedArr[i]);

  // Limpieza autom�tica del array din�mico al salir del scope
end;

procedure TAiRAGVector.BuildIndex;
begin
  // 1. Validaci�n del �ndice Vectorial
  if not Assigned(FRagIndex) then
    raise Exception.Create('No existe un indice asignado');

  // 2. Construir el �ndice Vectorial (HNSW o Basic)
  FRagIndex.BuildIndex(Self.FItems);

  BuildLexicalIndex;

  FActive := True;
end;

procedure TAiRAGVector.BuildLexicalIndex;
var
  i: Integer;
begin
  if not Assigned(FBm25Index) then
    FBm25Index := TAIBm25Index.Create;

  // Bloqueamos escritura para seguridad
  FLock.BeginWrite;
  try
    // 1. Limpiar basura anterior
    FBm25Index.Clear;

    // 2. Recorrer todos los items en memoria e indexarlos
    for i := 0 to FItems.Count - 1 do
    begin
      FBm25Index.AddNode(FItems[i]);
    end;
  finally
    FLock.EndWrite;
  end;
end;

procedure TAiRAGVector.CheckIndexes;
begin
  if not Assigned(FRagIndex) then
  begin
    case FInMemoryIndexType of
      TAIBasicIndex:
        FRagIndex := TAIBasicEmbeddingIndex.Create;
      TAIHNSWIndex:
        FRagIndex := THNSWIndex.Create;
      TAIEuclideanIndex:
        FRagIndex := TAIEuclideanDistanceIndex.Create;
    else
      FRagIndex := THNSWIndex.Create; // Fallback seguro
    end;

    // Vinculamos el �ndice a los datos actuales
    // IMPORTANTE: Pasar 'Self' expl�citamente
    if Assigned(FRagIndex) then
      FRagIndex.BuildIndex(Self.FItems);
  end;

  if not Assigned(FBm25Index) then
    FBm25Index := TAIBm25Index.Create;
end;

procedure TAiRAGVector.Clear;
var
  i: Integer;
begin
  FLock.BeginWrite; // Protecci�n multihilo
  try
    // 1. Liberar memoria de los nodos si somos due�os
    if FOwnsObjects then
    begin
      for i := 0 to FItems.Count - 1 do
        FItems[i].Free;
    end;

    // 2. Vaciar la lista principal
    FItems.Clear;

    // 3. Limpiar el �ndice L�xico (BM25)
    if Assigned(FBm25Index) then
      FBm25Index.Clear;

    // 4. Reiniciar el �ndice Vectorial (HNSW / Basic)
    // Como HNSW y otros �ndices suelen acumular datos y no tienen un m�todo Clear est�ndar
    // en tu implementaci�n actual, lo m�s seguro y limpio es destruirlo y recrearlo.
    if Assigned(FRagIndex) then
    begin
      FreeAndNil(FRagIndex);
      // CheckIndexes volver� a crear el �ndice vac�o del tipo correcto (HNSW/Basic)
      CheckIndexes;
    end;

  finally
    FLock.EndWrite;
  end;
end;

function TAiRAGVector.Connect(aHost, aPort, aLogin, aPassword: String): Boolean;
begin
  // esta funci�n no va, est� deprecade
  If not Assigned(FRagIndex) then
    Raise Exception.Create('No existe un indice asignado');

  // Result := FRagIndex.Connect(aHost, aPort, aLogin, aPassword);
  FActive := True;
  Result := True;
end;

function TAiRAGVector.Count: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
end;

constructor TAiRAGVector.Create(aOwner: TComponent; AOwnsObjects: Boolean);
begin
  inherited Create(aOwner); // 1. Llamar al padre
  InternalInit(AOwnsObjects); // 2. Inicializar con valor pasado
end;

constructor TAiRAGVector.Create(aOwner: TComponent);
begin
  inherited Create(aOwner); // 1. Llamar al padre
  InternalInit(True); // 2. Inicializar con valor por defecto
end;

function TAiRAGVector.CreateEmbeddingNode(aText: String; aEmbeddings: TAiEmbeddingsCore): TAiEmbeddingNode;
Var
  Ar: TAiEmbeddingData;
begin
  If aEmbeddings = Nil then
    aEmbeddings := FEmbeddings;

  If aEmbeddings = Nil then
    Raise Exception.Create('Debe especificar un modelo de embeddings primero');

  Try
    Ar := aEmbeddings.CreateEmbedding(aText, 'user');
    Result := TAiEmbeddingNode.Create(1);
    Result.Text := aText;
    Result.Data := Ar;
    Result.Model := aEmbeddings.Model;
  Finally
  End;
end;

destructor TAiRAGVector.Destroy;
var
  i: Integer;
begin
  // 1. Marcar como inactivo inmediatamente
  FActive := False;

  // 2. IMPORTANTE: No usar FLock.BeginWrite aqu�.
  // El objeto se est� destruyendo, no debe haber otros hilos accediendo a �l.
  // Si los hay, es un error de dise�o externo. Usar el lock aqu� suele causar excepciones.

  // 3. Liberar �ndices PRIMERO
  // Los �ndices contienen punteros a los Items. Debemos destruir los �ndices
  // mientras los Items a�n existen en memoria para evitar punteros hu�rfanos.
  FreeAndNil(FBm25Index);
  FreeAndNil(FRagIndex);

  // 4. Liberar la lista de Items y sus objetos
  if Assigned(FItems) then
  begin
    if FOwnsObjects then
    begin
      for i := 0 to FItems.Count - 1 do
        FItems[i].Free; // Liberar cada nodo
    end;
    FreeAndNil(FItems); // Liberar la lista
  end;

  // 5. Liberar el objeto de sincronizaci�n al final
  FreeAndNil(FLock);

  FSearchOptions.Free;

  // 6. Finalmente, lo heredado
  inherited Destroy;
end;

function TAiRAGVector.GetItems: TList<TAiEmbeddingNode>;
begin
  Result := FItems;
end;

function TAiRAGVector.GetLexicalLanguage: TAiLanguage;
begin
  If Assigned(FBm25Index) then
    Result := FBm25Index.Language
  Else
    Result := TAiLanguage.alSpanish;
end;

procedure TAiRAGVector.InternalInit(AOwnsObjects: Boolean);
begin
  FOwnsObjects := AOwnsObjects;

  // 1. Crear objetos contenedores PRIMERO
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FItems := TList<TAiEmbeddingNode>.Create;
  FBm25Index := TAIBm25Index.Create;

  // 2. Establecer valores por defecto
  FSearchOptions := TAiSearchOptions.Create;
  FSearchOptions.UseEmbeddings := True;
  FSearchOptions.UseBM25 := False;
  FSearchOptions.UseRRF := False;
  FSearchOptions.UseReorderABC := False;

  FInMemoryIndexType := TAIHNSWIndex; // Asegura que coincida con el 'default' de la propiedad

  // 3. Crear el �ndice inicial
  // No llamamos a CheckIndexes aqu� para evitar doble chequeo, lo hacemos expl�cito
  FRagIndex := THNSWIndex.Create;
  FRagIndex.BuildIndex(Self.FItems);
end;


procedure TAiRAGVector.InternalNormalizeVector(aList: TList<TAiSearchResult>);
begin
  // Los embeddings ya est�n en escala 0..1 (Coseno).
  // No aplicamos Min-Max porque el 1.0 debe ser "perfecci�n real", no "el mejor de la lista".
  // Simplemente aseguramos que no haya valores fuera de rango por error num�rico.
  for var i := 0 to aList.Count - 1 do
  begin
    var
    S := aList[i].Score;
    if S < 0 then
      S := 0;
    if S > 1 then
      S := 1;
    aList[i] := TAiSearchResult.Create(aList[i].Node, S);
  end;
end;

procedure TAiRAGVector.InternalNormalizeLexical(aList: TList < TPair < Double, TAiEmbeddingNode >> );
var
  MaxS: Double;
  i: Integer;
begin
  if aList.Count = 0 then
    Exit;

  // 1. Buscamos el m�ximo score BM25 de esta b�squeda
  MaxS := 0;
  for i := 0 to aList.Count - 1 do
    if aList[i].Key > MaxS then
      MaxS := aList[i].Key;

  // 2. Normalizamos respecto a un "M�ximo Te�rico" o un valor de confianza.
  // Si MaxS es muy peque�o (ej: 0.5), no lo subimos a 1.0.
  // Solo dividimos por MaxS si MaxS es un score robusto.

  // Una t�cnica est�ndar es dividir por el MaxS actual solo si es mayor a 1,
  // o usar un valor de referencia para que el score 1.0 sea dif�cil de alcanzar.
  for i := 0 to aList.Count - 1 do
  begin
    var
    NewScore := aList[i].Key;
    if MaxS > 0 then
      NewScore := NewScore / MaxS; // Ahora el mejor es 1.0, pero solo dentro de su canal

    aList[i] := TPair<Double, TAiEmbeddingNode>.Create(NewScore, aList[i].Value);
  end;
end;

{ TAiRAGVector }

function TAiRAGVector.InternalSearchSafe(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria): TList<TAiSearchResult>;
var
  i: Integer;
  Emb: TAiEmbeddingNode;
  Score: Double;
  PassFilter: Boolean;
begin
  // Lista resultado (el llamador debe liberarla)
  Result := TList<TAiSearchResult>.Create;
  try
    // B�squeda lineal / fuerza bruta
    for i := 0 to FItems.Count - 1 do
    begin
      Emb := FItems[i];

      // 1. FILTRADO POR CRITERIOS (pre-coseno)
      if Assigned(aFilter) and (aFilter.Count > 0) then
      begin
        // Centralizado en MetaData
        if not Emb.MetaData.Matches(aFilter, Emb.Text) then
          Continue;
      end;

      // 2. FILTRADO POR EVENTO (opcional, se mantiene igual)
      if Assigned(FOnFilterItem) then
      begin
        PassFilter := True;
        FOnFilterItem(Self, Emb, PassFilter);
        if not PassFilter then
          Continue;
      end;

      // 3. C�LCULO DE SIMILITUD (thread-safe)
      Score := TAiEmbeddingNode.CosineSimilarity(Emb, Target);

      // 4. FILTRADO POR PRECISI�N
      if (aPrecision > 0) and (Score < aPrecision) then
        Continue;

      // 5. AGREGAR RESULTADO
      Result.Add(TAiSearchResult.Create(Emb, Score));
    end;

    // 6. ORDENAR (mayor score primero)
    Result.Sort(TComparer<TAiSearchResult>.Construct(
      function(const Left, Right: TAiSearchResult): Integer
      begin
        Result := CompareValue(Right.Score, Left.Score);
      end));

    // 7. RECORTAR TOP-K
    while Result.Count > aLimit do
      Result.Delete(Result.Count - 1);

  except
    Result.Free;
    raise;
  end;
end;

function TAiRAGVector.DoOnGetEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
begin

  if Assigned(FOnGetEmbedding) then
    FOnGetEmbedding(Self, aInput, aUser, aModel, aEncodingFormat, aDimensions, Result)
  Else
    raise Exception.Create('El evento OnGetEmbedding no ha sido asignado. No se puede generar el embedding.');

end;

function TAiRAGVector.ExecuteVQL(const AVqlQuery: string; out AResultVector: TAiRAGVector): string;
var
  Parser: TVGQLParser;
  AST: TVGQLQuery;
  Compiler: TVGQLCompiler;
  Req: TVGQLRequest;
  TempOptions: TAiSearchOptions;
  i: Integer;
  MMRVector: TAiRAGVector;
begin
  Result := '';
  AResultVector := nil;
  Req := nil;

  // ---------------------------------------------------------------------------
  // 1. FRONT-END: PARSEO DE LA SINTAXIS (Texto -> AST)
  // ---------------------------------------------------------------------------
  Parser := TVGQLParser.Create(AVqlQuery);
  try
    AST := Parser.Parse;
  finally
    Parser.Free;
  end;

  try
    // -------------------------------------------------------------------------
    // 2. MIDDLE-END: COMPILACION (AST -> Request Object)
    // -------------------------------------------------------------------------
    Compiler := TVGQLCompiler.Create;
    try
      Req := Compiler.Translate(AST);
    finally
      Compiler.Free;
      if Assigned(AST) then
        AST.Free;
    end;

    if not Assigned(Req) then
      Exit;

    // -------------------------------------------------------------------------
    // EXPLAIN: retorna el plan de ejecucion sin ejecutar la busqueda
    // -------------------------------------------------------------------------
    if Req.Explain then
    begin
      Result := BuildExplainOutput(Req);
      Exit;
    end;

    // -------------------------------------------------------------------------
    // 3. BACK-END: CONFIGURACION DINAMICA DEL MOTOR
    // -------------------------------------------------------------------------
    TempOptions := TAiSearchOptions.Create;
    try
      TempOptions.UseEmbeddings := (Req.Mode = TSearchMode.smEmbeddings) or (Req.Mode = TSearchMode.smHybrid);
      TempOptions.UseBM25       := (Req.Mode = TSearchMode.smBM25) or (Req.Mode = TSearchMode.smHybrid);
      TempOptions.EmbeddingWeight := Req.WeightSemantic;
      TempOptions.BM25Weight      := Req.WeightLexical;
      TempOptions.UseRRF          := (Req.Fusion = TFusionMode.fmRRF);
      TempOptions.MinAbsoluteScoreEmbedding := Req.MinSemantic;
      TempOptions.MinAbsoluteScoreBM25      := Req.MinLexical;
      // ABC solo si no se usa MMR (son estrategias alternativas de post-procesado)
      TempOptions.UseReorderABC := Req.UseReorderABC and not Req.UseMMR;
      Self.SearchOptions.Assign(TempOptions);

      // Sincronizacion de idioma para BM25 -- ahora funcional via LANGUAGE clause
      if not Req.Language.IsEmpty then
      begin
        if SameText(Req.Language, 'spanish')    then Self.LexicalLanguage := alSpanish
        else if SameText(Req.Language, 'english')    then Self.LexicalLanguage := alEnglish
        else if SameText(Req.Language, 'portuguese') then Self.LexicalLanguage := alPortuguese;
      end;

      // -----------------------------------------------------------------------
      // 4. EJECUCION DE LA BUSQUEDA VECTORIAL PRINCIPAL
      // -----------------------------------------------------------------------
      AResultVector := Self.Search(Req.Query, Req.Limit, Req.MinGlobal, Req.Filter);

      // -----------------------------------------------------------------------
      // 5. RERANK (segunda etapa: refinamiento semantico profundo)
      // -----------------------------------------------------------------------
      if Assigned(AResultVector) and (Req.RerankQuery <> '') and (AResultVector.Count > 0) then
      begin
        AResultVector.Embeddings := Self.Embeddings;
        AResultVector.RegenerateAll;
        AResultVector.BuildIndex;
        AResultVector.Rerank(Req.RerankQuery, Req.RerankRegenerate);

        // Re-aplicar THRESHOLD GLOBAL tras el rerank: los nuevos scores
        // (cosine similarity vs. la query de rerank) pueden ser menores al umbral.
        if Req.MinGlobal > 0 then
          for i := AResultVector.Count - 1 downto 0 do
            if AResultVector.Items[i].Idx < Req.MinGlobal then
              AResultVector.Items.Delete(i);
      end;

      // -----------------------------------------------------------------------
      // 6a. OPTIMIZE MMR -- diversificacion de resultados
      // -----------------------------------------------------------------------
      if Req.UseMMR and Assigned(AResultVector) and (AResultVector.Count > 1) then
      begin
        MMRVector := ApplyMMR(AResultVector, Req.MmrLambda, AResultVector.Count);
        AResultVector.Free;  // safe: non-owning list
        AResultVector := MMRVector;
      end
      // -----------------------------------------------------------------------
      // 6b. OPTIMIZE REORDER ABC -- optimizacion ventana de contexto LLM
      // -----------------------------------------------------------------------
      else if Assigned(AResultVector) and Req.UseReorderABC then
        ApplyContextReordering(AResultVector);

      // -----------------------------------------------------------------------
      // 7. DISTINCT ON campo -- deduplicacion por campo de metadato
      // -----------------------------------------------------------------------
      if Assigned(AResultVector) and (Req.DistinctField <> '') then
        ApplyDistinct(AResultVector, Req.DistinctField);

      // -----------------------------------------------------------------------
      // 8. ORDER BY -- ordenamiento personalizado
      // -----------------------------------------------------------------------
      if Assigned(AResultVector) and (Req.OrderByFields.Count > 0) then
        ApplyOrderBy(AResultVector, Req.OrderByFields);

      // -----------------------------------------------------------------------
      // 9. OFFSET -- paginacion: saltar los primeros N resultados
      // -----------------------------------------------------------------------
      if Assigned(AResultVector) and (Req.Offset > 0) then
        for i := 1 to Min(Req.Offset, AResultVector.Count) do
          AResultVector.Items.Delete(0);

      // -----------------------------------------------------------------------
      // 10. GENERACION DEL OUTPUT FINAL PARA EL LLM
      // -----------------------------------------------------------------------
      if Assigned(AResultVector) then
      begin
        if Req.IncludeId or Req.IncludeRank or Req.IncludeModel then
          Result := BuildExtendedContextText(AResultVector, Req)
        else
          Result := VectorToContextText(AResultVector, Req.IncludeMetadata, Req.IncludeScore);
      end;

    finally
      TempOptions.Free;
    end;
  finally
    if Assigned(Req) then
      Req.Free;
  end;
end;

function TAiRAGVector.ApplyMMR(ASource: TAiRAGVector; ALambda: Double; ALimit: Integer): TAiRAGVector;
// Maximal Marginal Relevance: balances relevance vs. diversity in results.
// Formula: argmax_{d not in S} [ lambda * Rel(d) - (1-lambda) * max_{d' in S} Sim(d,d') ]
var
  Selected, Remaining: TList<TAiEmbeddingNode>;
  BestIdx: Integer;
  BestScore, Score, MaxSim, InterSim: Double;
  i, j: Integer;
  Item, SelItem: TAiEmbeddingNode;
begin
  Result := TAiRAGVector.Create(nil, False);
  if ASource.Count = 0 then Exit;

  // Clamp lambda to [0, 1]
  if ALambda < 0 then ALambda := 0;
  if ALambda > 1 then ALambda := 1;

  Selected  := TList<TAiEmbeddingNode>.Create;
  Remaining := TList<TAiEmbeddingNode>.Create;
  try
    for i := 0 to ASource.Count - 1 do
      Remaining.Add(ASource.Items[i]);

    while (Selected.Count < ALimit) and (Remaining.Count > 0) do
    begin
      BestIdx   := 0;
      BestScore := -MaxDouble;

      for i := 0 to Remaining.Count - 1 do
      begin
        Item := Remaining[i];

        // Diversity term: max cosine similarity to already-selected items
        if Selected.Count = 0 then
          MaxSim := 0
        else
        begin
          MaxSim := 0;
          for j := 0 to Selected.Count - 1 do
          begin
            SelItem  := Selected[j];
            InterSim := TAiEmbeddingNode.CosineSimilarity(Item, SelItem);
            if InterSim > MaxSim then
              MaxSim := InterSim;
          end;
        end;

        // MMR score: lambda * relevance - (1-lambda) * max_inter_similarity
        Score := ALambda * Item.Idx - (1.0 - ALambda) * MaxSim;
        if Score > BestScore then
        begin
          BestScore := Score;
          BestIdx   := i;
        end;
      end;

      Item := Remaining[BestIdx];
      Item.Idx := BestScore;  // update score to reflect MMR value
      Selected.Add(Item);
      Remaining.Delete(BestIdx);
    end;

    for i := 0 to Selected.Count - 1 do
      Result.Items.Add(Selected[i]);

    Result.FModel := ASource.FModel;
    Result.FDim   := ASource.FDim;
  finally
    Selected.Free;
    Remaining.Free;
  end;
end;

procedure TAiRAGVector.ApplyDistinct(AVector: TAiRAGVector; const AField: string);
// Keeps only the first occurrence of each unique value of AField.
// Items missing the field are all kept (treated as distinct).
var
  i: Integer;
  Seen: TDictionary<string, Boolean>;
  Item: TAiEmbeddingNode;
  FieldValue: Variant;
  StrVal: string;
  MissingCount: Integer;
begin
  Seen := TDictionary<string, Boolean>.Create;
  MissingCount := 0;
  try
    i := 0;
    while i < AVector.Count do
    begin
      Item := AVector.Items[i];

      if SameText(AField, 'id') or SameText(AField, 'tag') then
        StrVal := Item.Tag
      else if Assigned(Item.MetaData) and Item.MetaData.Has(AField) then
      begin
        FieldValue := Item.MetaData.Get(AField, Null);
        StrVal := VarToStr(FieldValue);
      end
      else
      begin
        // Items without the field each get a unique key so they are all kept
        StrVal := '__missing_' + IntToStr(MissingCount);
        Inc(MissingCount);
      end;

      if Seen.ContainsKey(StrVal) then
        AVector.Items.Delete(i)  // duplicate: remove, do not advance i
      else
      begin
        Seen.Add(StrVal, True);
        Inc(i);
      end;
    end;
  finally
    Seen.Free;
  end;
end;

procedure TAiRAGVector.ApplyOrderBy(AVector: TAiRAGVector; AFields: TList<TOrderByField>);
// Sorts AVector.Items according to the list of ORDER BY fields.
// Supports: 'score' (Idx), 'rank'/'orden' (Orden), 'model' (Model), 'id'/'tag' (Tag),
// and any metadata property name.
var
  LocalFields: TList<TOrderByField>;
  F: TOrderByField;
begin
  if (AVector = nil) or (AVector.Count <= 1) or (AFields.Count = 0) then
    Exit;

  // Copy fields into a local list so the anonymous method can safely capture it
  LocalFields := TList<TOrderByField>.Create;
  try
    for F in AFields do
      LocalFields.Add(F);

    AVector.Items.Sort(TComparer<TAiEmbeddingNode>.Construct(
      function(const Left, Right: TAiEmbeddingNode): Integer
      var
        k: Integer;
        LF: TOrderByField;
        LV, RV: Variant;
        Cmp: Integer;
      begin
        Result := 0;
        for k := 0 to LocalFields.Count - 1 do
        begin
          LF := LocalFields[k];

          // Resolve field value for Left and Right
          if SameText(LF.FieldName, 'score') then
          begin
            LV := Left.Idx;
            RV := Right.Idx;
          end
          else if SameText(LF.FieldName, 'rank') or SameText(LF.FieldName, 'orden') then
          begin
            LV := Left.Orden;
            RV := Right.Orden;
          end
          else if SameText(LF.FieldName, 'model') then
          begin
            LV := Left.Model;
            RV := Right.Model;
          end
          else if SameText(LF.FieldName, 'id') or SameText(LF.FieldName, 'tag') then
          begin
            LV := Left.Tag;
            RV := Right.Tag;
          end
          else
          begin
            LV := '';
            RV := '';
            if Assigned(Left.MetaData)  and Left.MetaData.Has(LF.FieldName)  then LV := Left.MetaData.Get(LF.FieldName, Null);
            if Assigned(Right.MetaData) and Right.MetaData.Has(LF.FieldName) then RV := Right.MetaData.Get(LF.FieldName, Null);
          end;

          // Compare values
          try
            if VarIsNumeric(LV) and VarIsNumeric(RV) then
            begin
              if Double(LV) < Double(RV) then Cmp := -1
              else if Double(LV) > Double(RV) then Cmp := 1
              else Cmp := 0;
            end
            else
              Cmp := AnsiCompareStr(VarToStr(LV), VarToStr(RV));
          except
            Cmp := 0;
          end;

          // Apply direction
          if LF.Direction = odDesc then
            Cmp := -Cmp;

          if Cmp <> 0 then
          begin
            Result := Cmp;
            Exit;
          end;
        end;
      end));
  finally
    LocalFields.Free;
  end;
end;

function TAiRAGVector.BuildExplainOutput(AReq: TVGQLRequest): string;
// Returns a human-readable description of the compiled VQL request
// without executing any search. Useful for debugging queries.
var
  SB: TStringBuilder;
  ModeStr, FusionStr, OptimStr, OrderStr: string;
  F: TOrderByField;
  First: Boolean;
begin
  SB := TStringBuilder.Create;
  try
    case AReq.Mode of
      smHybrid:
        ModeStr := Format('HYBRID (Semantic: %.0f%% | Lexical: %.0f%%)',
          [AReq.WeightSemantic * 100, AReq.WeightLexical * 100]);
      smEmbeddings: ModeStr := 'EMBEDDINGS only';
      smBM25:       ModeStr := 'BM25 (lexical) only';
    else             ModeStr := 'HYBRID';
    end;

    if AReq.Fusion = fmRRF then FusionStr := 'RRF' else FusionStr := 'Weighted Score Fusion';

    if AReq.UseMMR then
      OptimStr := Format('MMR (lambda=%.2f)', [AReq.MmrLambda])
    else if AReq.UseReorderABC then
      OptimStr := 'REORDER ABC'
    else
      OptimStr := '(none)';

    if AReq.OrderByFields.Count = 0 then
      OrderStr := '(default: score DESC)'
    else
    begin
      OrderStr := '';
      First := True;
      for F in AReq.OrderByFields do
      begin
        if not First then OrderStr := OrderStr + ', ';
        if F.Direction = odDesc then
          OrderStr := OrderStr + F.FieldName + ' DESC'
        else
          OrderStr := OrderStr + F.FieldName + ' ASC';
        First := False;
      end;
    end;

    SB.AppendLine('## VGQL Query Plan');
    SB.AppendLine('');
    SB.AppendLine('Entity    : ' + IfThen(AReq.Entity <> '', AReq.Entity, '(default)'));
    SB.AppendLine('Query     : ' + AReq.Query);
    SB.AppendLine('Mode      : ' + ModeStr);
    SB.AppendLine('Fusion    : ' + FusionStr);
    SB.AppendLine('Language  : ' + AReq.Language);
    SB.AppendLine('Rerank    : ' + IfThen(AReq.RerankQuery <> '', AReq.RerankQuery, '(none)'));
    if AReq.MinGlobal > 0 then
      SB.AppendLine(Format('Threshold : GLOBAL %.4f', [AReq.MinGlobal]));
    if AReq.MinSemantic > 0 then
      SB.AppendLine(Format('Threshold : SEMANTIC %.4f', [AReq.MinSemantic]));
    if AReq.MinLexical > 0 then
      SB.AppendLine(Format('Threshold : LEXICAL %.4f', [AReq.MinLexical]));
    SB.AppendLine(Format('Limit     : %d / Offset: %d', [AReq.Limit, AReq.Offset]));
    SB.AppendLine('Distinct  : ' + IfThen(AReq.DistinctField <> '', AReq.DistinctField, '(none)'));
    SB.AppendLine('Order By  : ' + OrderStr);
    SB.AppendLine('Optimize  : ' + OptimStr);
    Result := SB.ToString.TrimRight;
  finally
    SB.Free;
  end;
end;

function TAiRAGVector.BuildExtendedContextText(DataVec: TAiRAGVector; AReq: TVGQLRequest): string;
// Like VectorToContextText but also emits ID, RANK, MODEL fields when requested.
var
  i: Integer;
  Emb: TAiEmbeddingNode;
  SB: TStringBuilder;
  Pair: TPair<string, Variant>;
  HasContent: Boolean;
begin
  if (DataVec = nil) or (DataVec.Count = 0) then
    Exit('');

  SB := TStringBuilder.Create;
  try
    for i := 0 to DataVec.Count - 1 do
    begin
      Emb := DataVec.Items[i];
      HasContent := False;

      if AReq.IncludeMetadata or AReq.IncludeScore or
         AReq.IncludeId or AReq.IncludeRank or AReq.IncludeModel then
      begin
        SB.Append('[');

        if AReq.IncludeRank then
        begin
          SB.AppendFormat('Rank: %d', [i + 1]);
          HasContent := True;
        end;

        if AReq.IncludeScore then
        begin
          if HasContent then SB.Append(' | ');
          SB.AppendFormat('Score: %.4f', [Emb.Idx]);
          HasContent := True;
        end;

        if AReq.IncludeId then
        begin
          if HasContent then SB.Append(' | ');
          SB.Append('ID: ' + IfThen(Emb.Tag <> '', Emb.Tag, IntToStr(i)));
          HasContent := True;
        end;

        if AReq.IncludeModel then
        begin
          if HasContent then SB.Append(' | ');
          SB.Append('Model: ' + Emb.Model);
          HasContent := True;
        end;

        if AReq.IncludeMetadata then
        begin
          // Show Tag as ID only if not already shown by IncludeId
          if (Emb.Tag <> '') and not AReq.IncludeId then
          begin
            if HasContent then SB.Append(' | ');
            SB.Append('ID: ' + Emb.Tag);
            HasContent := True;
          end;
          if Assigned(Emb.MetaData) then
          begin
            for Pair in Emb.MetaData.InternalDictionary do
            begin
              if HasContent then SB.Append(' | ');
              SB.Append(Pair.Key + ': ' + VarToStr(Pair.Value));
              HasContent := True;
            end;
          end;
        end;

        SB.AppendLine(']');
      end;

      if Emb.Text <> '' then
        SB.AppendLine(Emb.Text);

      SB.AppendLine('');
    end;
    Result := SB.ToString.TrimRight;
  finally
    SB.Free;
  end;
end;

function TAiRAGVector.ExecuteVQL(const AVqlQuery: string): string;
var
  TempVec: TAiRAGVector;
begin
  Result := ExecuteVQL(AVqlQuery, TempVec);
  if Assigned(TempVec) then
    TempVec.Free;
end;

function TAiRAGVector.FilterByCriteria(const aCriteria: TAiFilterCriteria): TAiRAGVector;
var
  i: Integer;
  Node: TAiEmbeddingNode;
  Include: Boolean;
begin
  Result := TAiRAGVector.Create(nil, False); // No owner
  FLock.BeginRead;
  try
    for i := 0 to FItems.Count - 1 do
    begin
      Node := FItems[i];
      Include := True;

      // A. Filtro por Evento
      if Assigned(FOnFilterItem) then
        FOnFilterItem(Self, Node, Include);

      // B. Filtro por Criterios
      if Include and Assigned(aCriteria) and (aCriteria.Count > 0) then
      begin
        if not Node.MetaData.Matches(aCriteria) then
          Include := False;
      end;

      if Include then
        Result.Items.Add(Node);
    end;
  finally
    FLock.EndRead;
  end;
end;

// Este m�todo devuelve un TAiRAGVector que no es due�o de los nodos (solo tiene las referencias).
function TAiRAGVector.FilterByMetaData(const aCriteria: TAiEmbeddingMetaData): TAiRAGVector;
var
  i: Integer;
  Node: TAiEmbeddingNode;
  Include: Boolean;
  Pair: TPair<string, Variant>;
begin
  // El resultado es un nuevo contenedor que no es due�o de los nodos (solo referencias)
  Result := TAiRAGVector.Create(nil, False);

  FLock.BeginRead;
  try
    for i := 0 to FItems.Count - 1 do
    begin
      Node := FItems[i];
      Include := True;

      // A. Filtro por Evento (Prioridad m�xima)
      if Assigned(FOnFilterItem) then
        FOnFilterItem(Self, Node, Include);

      // B. Filtro por Diccionario de Metadatos
      if Include and Assigned(aCriteria) and (aCriteria.InternalDictionary.Count > 0) then
      begin
        for Pair in aCriteria.InternalDictionary do
        begin
          // Usamos la nueva l�gica de evaluaci�n de la clase MetaData
          // Comprobamos si el nodo actual cumple con el criterio (igualdad por defecto)
          if not Node.MetaData.Evaluate(Pair.Key, foEqual, Pair.Value) then
          begin
            Include := False;
            Break;
          end;
        end;
      end;

      if Include then
        Result.Items.Add(Node);
    end;
  finally
    FLock.EndRead;
  end;
end;

function TAiRAGVector.LoadFromFile(FileName: String): Integer;
Var
  FS: TFileStream;
begin
  if not FileExists(FileName) then
    Raise Exception.CreateFmt('El archivo no existe: %s', [FileName]);

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    // Llamamos a LoadFromStream y devolvemos su resultado
    Result := LoadFromStream(FS);
  Finally
    FS.Free;
  End;
end;

function TAiRAGVector.LoadFromStream(Stream: TStream): Integer;
Var
  ST: TStringStream;
  JItem, JObj: TJSONObject;
  JArr: TJSonArray;
  JVal: TJSONValue;
  Emb: TAiEmbeddingNode;
  JSONContent: string;
begin
  Result := 0; // Inicializamos

  if not Assigned(Stream) then
    Exit;

  // 1. Lectura segura con codificaci�n UTF-8
  ST := TStringStream.Create('', TEncoding.UTF8);
  Try
    Stream.Position := 0;
    ST.CopyFrom(Stream, 0);
    JSONContent := ST.DataString;
  Finally
    ST.Free;
  End;

  // 2. Parseo del JSON
  JVal := TJSONObject.ParseJSONValue(JSONContent);
  if not(JVal is TJSONObject) then
  begin
    JVal.Free;
    Raise Exception.Create('El formato del archivo no es un objeto JSON v�lido.');
  end;

  JObj := TJSONObject(JVal);
  Try
    // 3. Bloqueo de escritura
    FLock.BeginWrite;
    Try
      // 4. Limpieza previa
      Self.Clear;

      // 5. Cargar Propiedades del Vector
      JObj.TryGetValue<String>('name', FNameVec);
      JObj.TryGetValue<String>('description', FDescription);
      JObj.TryGetValue<String>('model', FModel);
      if JObj.TryGetValue<Integer>('dim', FDim) then;

      // 6. Cargar Array de Datos
      if JObj.TryGetValue<TJSonArray>('data', JArr) then
      Begin
        For JVal in JArr do
        Begin
          if JVal is TJSONObject then
          begin
            JItem := TJSONObject(JVal);

            // Reconstruir nodo desde JSON
            Emb := TAiEmbeddingNode.FromJSON(JItem);

            // Agregar a la lista
            FItems.Add(Emb);

            // Indexar en BM25 si aplica
            if FSearchOptions.FUseBM25 then
              FBm25Index.AddNode(Emb);
          end;
        End;
      End;

      // 7. Reconstruir el �ndice vectorial
      if Assigned(FRagIndex) then
        FRagIndex.BuildIndex(Self.FItems);

      // ASIGNAMOS EL RESULTADO FINAL
      Result := FItems.Count;

    Finally
      FLock.EndWrite;
    End;
  Finally
    JObj.Free;
  End;
end;

procedure TAiRAGVector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  // Si se est� eliminando un componente
  if Operation = opRemove then
  begin
    // Verificar si es el motor de Embeddings asignado
    if (AComponent = FEmbeddings) then
      FEmbeddings := nil;

    // Verificar si es el Driver de base de datos asignado
    if (AComponent = FDriver) then
      FDriver := nil;
  end;
end;

procedure TAiRAGVector.Rerank(Target: TAiEmbeddingNode; aAutoRegenerate: Boolean);
var
  i: Integer;
  Emb: TAiEmbeddingNode;
begin
  if FItems.Count = 0 then
    Exit;

  // 1. Verificaci�n de modelos
  if (FModel <> '') and (FModel <> Target.Model) then
  begin
    if aAutoRegenerate then
      RegenerateAll(Target.Model) // Migramos toda la lista al modelo del Target
    else
      raise Exception.Create('Rerank Error: El modelo del query no coincide y AutoRegenerate est� desactivado.');
  end;

  // 2. Recalcular puntuaciones (Idx) con el mismo modelo ya garantizado
  for i := 0 to FItems.Count - 1 do
  begin
    Emb := FItems[i];
    Emb.Idx := TAiEmbeddingNode.CosineSimilarity(Emb, Target);
  end;

  // 3. Reordenar la lista (El c�digo de sorting se mantiene igual)
  FItems.Sort(TComparer<TAiEmbeddingNode>.Construct(
    function(const Left, Right: TAiEmbeddingNode): Integer
    const
      TOLERANCE = 1.0E-12;
    begin
      if Abs(Left.Idx - Right.Idx) < TOLERANCE then
        Result := 0
      else if Left.Idx > Right.Idx then
        Result := -1
      else
        Result := 1;
    end));
end;

function TAiRAGVector.ReciprocalRankFusion(VectorResults: TList<TAiSearchResult>; LexicalResults: TList<TPair<Double, TAiEmbeddingNode>>; aLimit: Integer): TAiRAGVector;
const
  k = 60; // Constante de suavizado est�ndar
var
  Scores: TDictionary<TAiEmbeddingNode, Double>;
  i: Integer;
  Node: TAiEmbeddingNode;
  CombinedList: TList<TPair<Double, TAiEmbeddingNode>>;
  RankScore: Double;
  MaxRRF, MinRRF, RangeRRF: Double;
begin
  Result := TAiRAGVector.Create(nil, False);
  Scores := TDictionary<TAiEmbeddingNode, Double>.Create;
  CombinedList := TList < TPair < Double, TAiEmbeddingNode >>.Create;
  try
    // 1. Procesar Ranking Vectorial
    // Nota: Aunque VectorResults ya est� normalizado, RRF solo usa su posici�n (i)
    if Assigned(VectorResults) then
    begin
      for i := 0 to VectorResults.Count - 1 do
      begin
        Node := VectorResults[i].Node;
        RankScore := 1.0 / (k + (i + 1));
        Scores.Add(Node, RankScore);
      end;
    end;

    // 2. Procesar Ranking L�xico (BM25)
    if Assigned(LexicalResults) then
    begin
      for i := 0 to LexicalResults.Count - 1 do
      begin
        Node := LexicalResults[i].Value;
        RankScore := 1.0 / (k + (i + 1));

        if Scores.ContainsKey(Node) then
          Scores[Node] := Scores[Node] + RankScore
        else
          Scores.Add(Node, RankScore);
      end;
    end;

    // 3. Convertir a lista para poder normalizar y ordenar
    for Node in Scores.Keys do
      CombinedList.Add(TPair<Double, TAiEmbeddingNode>.Create(Scores[Node], Node));

    if CombinedList.Count = 0 then
      Exit;

    // --- PUNTO CR�TICO: NORMALIZACI�N DEL SCORE RRF ---
    // Encontramos el Min y Max de los scores RRF calculados
    MaxRRF := -1;
    MinRRF := 999999;
    for i := 0 to CombinedList.Count - 1 do
    begin
      if CombinedList[i].Key > MaxRRF then
        MaxRRF := CombinedList[i].Key;
      if CombinedList[i].Key < MinRRF then
        MinRRF := CombinedList[i].Key;
    end;

    RangeRRF := MaxRRF - MinRRF;

    // 4. Ordenar descendente por el score RRF
    CombinedList.Sort(TComparer < TPair < Double, TAiEmbeddingNode >>.Construct(
      function(const L, R: TPair<Double, TAiEmbeddingNode>): Integer
      begin
        Result := CompareValue(R.Key, L.Key);
      end));

    // 5. Volcar al resultado aplicando la normalizaci�n Min-Max final
    // Esto hace que el mejor ranking RRF tenga Idx = 1.0
    for i := 0 to Min(aLimit * 2, CombinedList.Count - 1) do
    begin
      Node := CombinedList[i].Value;

      if RangeRRF > 0 then
        Node.Idx := (CombinedList[i].Key - MinRRF) / RangeRRF
      else
        Node.Idx := 1.0;

      Result.Items.Add(Node);
    end;

  finally
    CombinedList.Free;
    Scores.Free;
  end;
end;

procedure TAiRAGVector.RegenerateAll(const aNewModel: String);
var
  i: Integer;
  Emb: TAiEmbeddingNode;
  NewModelName: String;
begin
  if FItems.Count = 0 then
    Exit;

  // Determinar qu� modelo usar
  if aNewModel <> '' then
    NewModelName := aNewModel
  else if Assigned(FEmbeddings) then
    NewModelName := FEmbeddings.Model
  else
    NewModelName := FModel;

  // Recorrer y regenerar
  for i := 0 to FItems.Count - 1 do
  begin
    Emb := FItems[i];
    if Emb.Text = '' then
      Continue; // Opcional: manejar error si no hay texto original

    // Generar el nuevo vector
    if Assigned(FEmbeddings) then
    begin
      // Si el programador pas� un modelo espec�fico, lo seteamos temporalmente
      if aNewModel <> '' then
        FEmbeddings.Model := aNewModel;
      Emb.Data := FEmbeddings.CreateEmbedding(Emb.Text, 'user');
    end
    else
    begin
      Emb.Data := DoOnGetEmbedding(Emb.Text, 'user', -1, NewModelName);
    end;

    Emb.Model := NewModelName;
    Emb.SetDataLength(Length(Emb.Data));
  end;

  // Actualizar metadatos del contenedor
  FModel := NewModelName;
  if FItems.Count > 0 then
    FDim := FItems[0].Dim;

  // �CR�TICO! El �ndice espacial anterior ya no sirve.
  BuildIndex;
end;

procedure TAiRAGVector.Rerank(NewPrompt: String; aAutoRegenerate: Boolean);
var
  Target: TAiEmbeddingNode;
begin
  if Not Assigned(FEmbeddings) and Not Assigned(FOnGetEmbedding) then
    Raise Exception.Create('Rerank: No hay motor de embeddings configurado.');

  // Creamos el nodo temporal para el nuevo prompt
  Target := CreateEmbeddingNode(NewPrompt);
  try
    // Pasamos el flag a la implementaci�n principal
    Rerank(Target, aAutoRegenerate);
  finally
    Target.Free;
  end;
end;

procedure TAiRAGVector.SaveToFile(FileName: String);
Var
  ST: TMemoryStream;
begin
  ST := TMemoryStream.Create;
  Try
    SaveToStream(ST);
    ST.SaveToFile(FileName);
  Finally
    ST.Free;
  End;
end;

procedure TAiRAGVector.SaveToStream(Stream: TMemoryStream);
Var
  Emb: TAiEmbeddingNode;
  i: Integer;
  ST: TStringStream;
  JArr: TJSonArray;
  JItem, JObj: TJSONObject;
begin
  // 1. Validaci�n de entrada
  If Not Assigned(Stream) then
    Raise Exception.Create('El Stream de destino no puede ser nil en SaveToStream.');

  // 2. Crear objeto ra�z
  JObj := TJSONObject.Create;
  try
    // 3. Guardar propiedades del Vector
    JObj.AddPair('name', FNameVec);
    JObj.AddPair('description', FDescription);
    JObj.AddPair('model', FModel);
    JObj.AddPair('dim', TJSONNumber.Create(FDim)); // Explicito para asegurar tipo num�rico

    // 4. Crear Array de Datos
    JArr := TJSonArray.Create;
    // IMPORTANTE: Al a�adir el par, JObj se convierte en el DUE�O de JArr.
    // No necesitamos (y no debemos) liberar JArr manualmente.
    JObj.AddPair('data', JArr);

    // 5. Llenar items con protecci�n de hilo
    FLock.BeginRead;
    try
      For i := 0 to FItems.Count - 1 do
      Begin
        Emb := FItems[i];
        // Delegamos la serializaci�n al nodo (que ya incluye Metadata, Texto, Vector)
        JItem := Emb.ToJSON;
        JArr.Add(JItem);
      End;
    finally
      FLock.EndRead;
    end;

    // 6. Escribir al Stream
    // Usamos ToJSON (compacto) en lugar de Format (indentado) para ahorrar espacio en disco.
    // Usamos UTF8 para soportar tildes y caracteres especiales.
    ST := TStringStream.Create(JObj.ToJSON, TEncoding.UTF8);
    try
      // Copiamos el contenido del string stream al stream de memoria destino
      Stream.CopyFrom(ST, 0);
    finally
      ST.Free;
    end;

  finally
    // Al liberar JObj, se liberan autom�ticamente JArr y todos los JItems hijos.
    JObj.Free;
  end;
end;

function TAiRAGVector.Search(Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria): TAiRAGVector;
var
  Handled: Boolean;
  VectorResults: TList<TAiSearchResult>;
  LexicalRes: TList<TPair<Double, TAiEmbeddingNode>>;
  SearchResult: TAiSearchResult;
  //TempResultVec: TAiRAGVector;
  i: Integer;
  Node: TAiEmbeddingNode;
  Pair: TPair<Double, TAiEmbeddingNode>;
  RawIndexResults: TRagItems;
begin
  Result := nil;
  Handled := False;

  if FSearchOptions.UseEmbeddings and (Target.MagnitudeValue <= 0) then
  begin
    // Si no hay vector en la pregunta, la b�squeda sem�ntica es imposible.
    // Podr�amos lanzar excepci�n o degradar autom�ticamente a BM25.
    if not FSearchOptions.UseBM25 then
      raise Exception.Create('Error: La consulta no gener� un vector v�lido. Verifique su motor de embeddings.');
  end;

  // ---------------------------------------------------------------------------
  // FASE 1: EVENTOS EXTERNOS
  // ---------------------------------------------------------------------------
  if Assigned(FOnDataVecSearch) then
  begin
    FOnDataVecSearch(Self, Target, Target.Text, aLimit, aPrecision, aFilter, Result, Handled);
    if Handled then
      Exit;
  end;

  // ---------------------------------------------------------------------------
  // FASE 2: DRIVER EXTERNO
  // ---------------------------------------------------------------------------
  if Assigned(FDriver) then
  begin
    Result := FDriver.Search(Target, FEntidad, aLimit, aPrecision, aFilter, FSearchOptions);
    Exit;
  end;

  // ---------------------------------------------------------------------------
  // FASE 3: B�SQUEDA EN MEMORIA
  // ---------------------------------------------------------------------------
  FLock.BeginRead;
  try
    VectorResults := nil;
    LexicalRes := nil;
    try
      // =======================================================================
      // SUBFASE A: B�SQUEDA VECTORIAL
      // =======================================================================
      if FSearchOptions.UseEmbeddings then
      begin
        if not Assigned(FRagIndex) then
          raise Exception.Create('No existe un �ndice asignado');

        if (FModel <> '') and (Target.Model <> '') and (FModel <> Target.Model) then
          raise Exception.CreateFmt('Error de Modelo: El vector usa "%s" pero el Query usa "%s"', [FModel, Target.Model]);

        // Si hay filtro o evento, usamos b�squeda lineal segura
        if ((Assigned(aFilter) and (aFilter.Count > 0)) or Assigned(FOnFilterItem)) then
        begin
          VectorResults := InternalSearchSafe(Target, Max(50, aLimit * 5), FSearchOptions.MinAbsoluteScoreEmbedding, aFilter);
        end
        else
        begin
          // V�a r�pida: �ndice vectorial (HNSW / Basic)
          VectorResults := TList<TAiSearchResult>.Create;
          //TempResultVec
          RawIndexResults  := FRagIndex.Search(Target, Max(50, aLimit * 5), FSearchOptions.MinAbsoluteScoreEmbedding);  //<<---- Error aqu�
          try
            if Assigned(RawIndexResults ) then
            begin
              for i := 0 to RawIndexResults.Count - 1 do
              begin
                Node := RawIndexResults [i];
                VectorResults.Add(TAiSearchResult.Create(Node, Node.Idx));
              end;
            end;
          finally
            RawIndexResults.Free;
          end;
        end;

        // Normalizaci�n vectorial [0..1]
        // InternalNormalizeVector(VectorResults);   //Eliminamos esto para dar los resultados reales y no los normalizados.
      end;

      // =======================================================================
      // SUBFASE B: B�SQUEDA L�XICA (BM25)
      // =======================================================================
      if FSearchOptions.UseBM25 and (Target.Text.Trim <> '') then
      begin
        LexicalRes := FBm25Index.Search(Target.Text, Max(50, aLimit * 5), aFilter);

        // Filtro A: dignidad BM25
        for i := LexicalRes.Count - 1 downto 0 do
          if LexicalRes[i].Key < FSearchOptions.MinAbsoluteScoreBM25 then
            LexicalRes.Delete(i);

        // Normalizaci�n l�xica [0..1]
        InternalNormalizeLexical(LexicalRes);
      end;

      // =======================================================================
      // SUBFASE C: FUSI�N
      // =======================================================================
      if Assigned(VectorResults) and not Assigned(LexicalRes) then
      begin
        // SOLO VECTORIAL
        Result := TAiRAGVector.Create(nil, False);
        for SearchResult in VectorResults do
        begin
          SearchResult.Node.Idx := SearchResult.Score;
          Result.Items.Add(SearchResult.Node);
        end;
      end
      else if not Assigned(VectorResults) and Assigned(LexicalRes) then
      begin
        // SOLO L�XICO
        Result := TAiRAGVector.Create(nil, False);
        for Pair in LexicalRes do
        begin
          Pair.Value.Idx := Pair.Key;
          Result.Items.Add(Pair.Value);
        end;
      end
      else if Assigned(VectorResults) and Assigned(LexicalRes) then
      begin
        // H�BRIDO
        if FSearchOptions.UseRRF then
          Result := ReciprocalRankFusion(VectorResults, LexicalRes, aLimit)
        else
          Result := WeightedScoreFusion(VectorResults, LexicalRes, aLimit);
      end
      else
        Result := TAiRAGVector.Create(nil, False);

      // =======================================================================
      // SUBFASE D: FILTRO FINAL Y POST-PROCESADO
      // =======================================================================
      if Assigned(Result) then
      begin
        // Precisi�n final sobre score unificado
        if aPrecision > 0 then
        begin
          for i := Result.Count - 1 downto 0 do
            if Result.Items[i].Idx < aPrecision then
              Result.Items.Delete(i);
        end;

        // Top-K definitivo
        while Result.Count > aLimit do
          Result.Items.Delete(Result.Count - 1);

        Result.FModel := Self.Model;
        Result.FDim := Self.Dim;

        if FSearchOptions.UseReorderABC and (Result.Count > 2) then
          ApplyContextReordering(Result);
      end;

    finally
      if Assigned(VectorResults) then
        VectorResults.Free;
      if Assigned(LexicalRes) then
        LexicalRes.Free;
    end;
  finally
    FLock.EndRead;
  end;
end;

function TAiRAGVector.Search(Prompt: String; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria): TAiRAGVector;
var
  Target: TAiEmbeddingNode;
begin
  // 1. Verificaci�n de Motor de Embeddings
  // Sin esto, no podemos convertir el texto 'Prompt' en n�meros.
  if Not Assigned(FEmbeddings) and Not Assigned(FOnGetEmbedding) then
    Raise Exception.Create('Error: No hay motor de embeddings configurado. Asigne la propiedad Embeddings o el evento OnGetEmbedding.');

  // 2. Vectorizaci�n
  // CreateEmbeddingNode se encarga de llamar al API de embeddings o al evento
  Target := CreateEmbeddingNode(Prompt);
  try
    // 3. Delegaci�n
    // Llamamos a la sobrecarga principal que busca por Nodo + Filtro Criteria
    Result := Search(Target, aLimit, aPrecision, aFilter);
  finally
    Target.Free;
  end;
end;

function TAiRAGVector.SearchText(aPrompt: TAiEmbeddingNode; aLimit: Integer; aPresicion: Double; aFilter: TAiFilterCriteria; IncludeMetadata, IncludeScore: Boolean): String;
Var
  TmpVec: TAiRAGVector;
Begin
  // Ahora la llamada es directa y expl�cita, pasando el criterio recibido (o nil)
  TmpVec := Search(aPrompt, aLimit, aPresicion, aFilter);

  Try
    Result := VectorToContextText(TmpVec, IncludeMetadata, IncludeScore);
  Finally
    TmpVec.Free;
  End;
end;

procedure TAiRAGVector.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TAiRAGVector.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TAiRAGVector.SetDriver(const Value: TAiVectorStoreDriverBase);
begin
  if FDriver <> Value then
  begin
    FDriver := Value;
    if Assigned(FDriver) then
      FDriver.FreeNotification(Self);
  end;
end;

procedure TAiRAGVector.SetEmbeddings(const Value: TAiEmbeddingsCore);
begin
  if FEmbeddings <> Value then
  begin
    FEmbeddings := Value;

    // Avisar al componente que queremos ser notificados cuando se destruya
    if Assigned(FEmbeddings) then
      FEmbeddings.FreeNotification(Self);

    // Tu l�gica existente para runtime/design-time
    if (csDesigning in ComponentState) then
      Exit;

    if Assigned(Value) then
    begin
      FModel := Value.Model;
      FDim := Value.Dimensions;
    end;
  end;
end;

procedure TAiRAGVector.SetInMemoryIndexType(const Value: TAiRagIndexType);
begin
  // Evitar cambios innecesarios
  if FInMemoryIndexType = Value then
    Exit;

  FInMemoryIndexType := Value;

  // Protecci�n: No reconstruir �ndices si estamos cargando el componente (lectura de DFM)
  // o si estamos en proceso de destrucci�n.
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  // Reconstrucci�n del �ndice
  FLock.BeginWrite;
  try
    if Assigned(FRagIndex) then
      FreeAndNil(FRagIndex);

    CheckIndexes; // Esto crear� el nuevo �ndice seg�n FInMemoryIndexType
  finally
    FLock.EndWrite;
  end;
end;

procedure TAiRAGVector.SetLexicalLanguage(const Value: TAiLanguage);
begin
  If Assigned(FBm25Index) then
    FBm25Index.Language := Value;
end;

procedure TAiRAGVector.SetNameVec(const Value: String);
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

function TAiRAGVector.VectorToContextText(DataVec: TAiRAGVector; IncludeMetadata: Boolean; IncludeScore: Boolean): String;
var
  i: Integer;
  Emb: TAiEmbeddingNode;
  SB: TStringBuilder;
  Pair: TPair<string, Variant>;
  HasContentBefore: Boolean;
begin
  if (DataVec = nil) or (DataVec.Count = 0) then
    Exit('');

  SB := TStringBuilder.Create;
  try
    for i := 0 to DataVec.Count - 1 do
    begin
      Emb := DataVec.Items[i];
      HasContentBefore := False;

      // --- 1. Bloque de Encabezado (Metadata y/o Score) ---
      if IncludeMetadata or IncludeScore then
      begin
        SB.Append('[');

        // A. Mostrar Score si se solicit�
        if IncludeScore then
        begin
          SB.AppendFormat('Score: %.4f', [Emb.Idx]);
          HasContentBefore := True;
        end;

        // B. Mostrar Metadatos si se solicit�
        if IncludeMetadata then
        begin
          // Separador si ya escribimos el score
          if HasContentBefore then
            SB.Append(' | ');

          // ID o Tag
          if Emb.Tag <> '' then
          begin
            SB.Append('ID: ' + Emb.Tag);
            HasContentBefore := True;
          end;

          // Diccionario de propiedades
          if Assigned(Emb.MetaData) then
          begin
            for Pair in Emb.MetaData.InternalDictionary do
            begin
              if HasContentBefore then
                SB.Append(' | ');
              SB.Append(Pair.Key + ': ' + VarToStr(Pair.Value));
              HasContentBefore := True;
            end;
          end;
        end;

        SB.AppendLine(']');
      end;

      // --- 2. Contenido del Texto ---
      SB.Append(Emb.Text.Trim);

      // --- 3. Separador entre fragmentos ---
      SB.AppendLine;
      SB.AppendLine;
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TAiRAGVector.WeightedScoreFusion(VectorResults: TList<TAiSearchResult>; LexicalResults: TList<TPair<Double, TAiEmbeddingNode>>; aLimit: Integer): TAiRAGVector;
var
  Scores: TDictionary<TAiEmbeddingNode, Double>;
  Node: TAiEmbeddingNode;
  i: Integer;
  CombinedList: TList<TPair<Double, TAiEmbeddingNode>>;
  W_Sem, W_Lex, TotalWeight: Double;
begin
  // El resultado es un nuevo contenedor que no es due�o de los objetos
  Result := TAiRAGVector.Create(nil, False);

  Scores := TDictionary<TAiEmbeddingNode, Double>.Create;
  CombinedList := TList < TPair < Double, TAiEmbeddingNode >>.Create;
  try
    // -------------------------------------------------------------------------
    // 1. NORMALIZACI�N INTERNA DE PESOS
    // Aseguramos que la suma de pesos sea exactamente 1.0 para no diluir el score
    // -------------------------------------------------------------------------
    TotalWeight := FSearchOptions.EmbeddingWeight + FSearchOptions.BM25Weight;

    if TotalWeight > 0 then
    begin
      W_Sem := FSearchOptions.EmbeddingWeight / TotalWeight;
      W_Lex := FSearchOptions.BM25Weight / TotalWeight;
    end
    else
    begin
      // Fallback de seguridad si ambos pesos son 0
      W_Sem := 0.5;
      W_Lex := 0.5;
    end;

    // -------------------------------------------------------------------------
    // 2. PROCESO DE FUSI�N PONDERADA
    // -------------------------------------------------------------------------

    // A. Sumar aportaci�n del canal Vectorial (Sem�ntico)
    if Assigned(VectorResults) then
    begin
      for i := 0 to VectorResults.Count - 1 do
      begin
        // Usamos el score real de similitud de coseno multiplicado por su peso
        Scores.Add(VectorResults[i].Node, VectorResults[i].Score * W_Sem);
      end;
    end;

    // B. Sumar aportaci�n del canal L�xico (BM25)
    if Assigned(LexicalResults) then
    begin
      for i := 0 to LexicalResults.Count - 1 do
      begin
        Node := LexicalResults[i].Value;
        // El score l�xico ya viene normalizado 0..1 desde InternalNormalizeLexical
        var
        LexScore := LexicalResults[i].Key * W_Lex;

        if Scores.ContainsKey(Node) then
          Scores[Node] := Scores[Node] + LexScore
        else
          Scores.Add(Node, LexScore);
      end;
    end;

    // -------------------------------------------------------------------------
    // 3. ORDENAMIENTO Y SELECCI�N DEL TOP-K
    // -------------------------------------------------------------------------

    // Pasamos del diccionario a una lista para poder ordenar
    for Node in Scores.Keys do
      CombinedList.Add(TPair<Double, TAiEmbeddingNode>.Create(Scores[Node], Node));

    // Ordenar de mayor a menor score combinado
    CombinedList.Sort(TComparer < TPair < Double, TAiEmbeddingNode >>.Construct(
      function(const L, R: TPair<Double, TAiEmbeddingNode>): Integer
      begin
        Result := CompareValue(R.Key, L.Key);
      end));

    // Llenar el vector de resultados respetando el l�mite
    // El score final combinado se guarda en Idx
    for i := 0 to Min(aLimit - 1, CombinedList.Count - 1) do
    begin
      Node := CombinedList[i].Value;
      Node.Idx := CombinedList[i].Key;
      Result.Items.Add(Node);
    end;

  finally
    CombinedList.Free;
    Scores.Free;
  end;
end;

function TAiRAGVector.SearchText(aPrompt: String; aLimit: Integer; aPrecision: Double; aFilter: TAiFilterCriteria; IncludeMetadata, IncludeScore: Boolean): String;
var
  TmpVec: TAiRAGVector;
begin
  // Llamamos a Search(String...) que ya soporta Criteria
  TmpVec := Search(aPrompt, aLimit, aPrecision, aFilter);
  try
    Result := VectorToContextText(TmpVec, IncludeMetadata, IncludeScore);
  finally
    TmpVec.Free;
  end;
end;


{ TAiVectorStoreDriverBase }

function TAiVectorStoreDriverBase.EmbeddingToString(const AData: TAiEmbeddingData): string;
var
  i: Integer;
  SB: TStringBuilder;
  FS: TFormatSettings;
begin
  if Length(AData) = 0 then
    Exit('[]');
  FS := TFormatSettings.Invariant;
  SB := TStringBuilder.Create;
  try
    SB.Append('[');
    for i := 0 to High(AData) do
    begin
      SB.Append(FloatToStr(AData[i], FS));
      if i < High(AData) then
        SB.Append(',');
    end;
    SB.Append(']');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TAiVectorStoreDriverBase.VariantToJSONValue(const V: Variant): TJSONValue;
begin
  case VarType(V) of
    varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord, varInt64:
      Result := TJSONNumber.Create(Int64(V));
    varSingle, varDouble, varCurrency:
      Result := TJSONNumber.Create(Double(V));
    varBoolean:
      Result := TJSONBool.Create(Boolean(V));
    varNull, varEmpty:
      Result := TJSONNull.Create;
  else
    Result := TJSONString.Create(VarToStr(V));
  end;
end;


{ TAiSearchOptions }

constructor TAiSearchOptions.Create;
begin
  inherited;
  // Valores por defecto solicitados
  FUseEmbeddings := True;
  FUseBM25 := True;
  FUseRRF := False;
  FUseReorderABC := False;
  FBM25Weight := 0.7;
  FEmbeddingWeight := 0.3;
end;

procedure TAiSearchOptions.Assign(Source: TPersistent);
begin
  if Source is TAiSearchOptions then
  begin
    FUseEmbeddings := TAiSearchOptions(Source).UseEmbeddings;
    FUseBM25 := TAiSearchOptions(Source).UseBM25;
    FUseRRF := TAiSearchOptions(Source).UseRRF;
    FUseReorderABC := TAiSearchOptions(Source).UseReorderABC;
    FBM25Weight := TAiSearchOptions(Source).BM25Weight;
    FEmbeddingWeight := TAiSearchOptions(Source).EmbeddingWeight;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TAiSearchOptions.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

// Setters (boilerplate necesario para detectar cambios)
procedure TAiSearchOptions.SetUseEmbeddings(const Value: Boolean);
begin
  if FUseEmbeddings <> Value then
  begin
    FUseEmbeddings := Value;
    Changed;
  end;
end;

procedure TAiSearchOptions.SetUseBM25(const Value: Boolean);
begin
  if FUseBM25 <> Value then
  begin
    FUseBM25 := Value;
    Changed;
  end;
end;

procedure TAiSearchOptions.SetUseRRF(const Value: Boolean);
begin
  if FUseRRF <> Value then
  begin
    FUseRRF := Value;
    Changed;
  end;
end;

procedure TAiSearchOptions.SetUseReorderABC(const Value: Boolean);
begin
  if FUseReorderABC <> Value then
  begin
    FUseReorderABC := Value;
    Changed;
  end;
end;

procedure TAiSearchOptions.SetBM25Weight(const Value: Double);
begin
  if FBM25Weight <> Value then
  begin
    FBM25Weight := Value;
    Changed;
  end;
end;

procedure TAiSearchOptions.SetEmbeddingWeight(const Value: Double);
begin
  if FEmbeddingWeight <> Value then
  begin
    FEmbeddingWeight := Value;
    Changed;
  end;
end;

end.
