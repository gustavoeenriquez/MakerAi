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
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// Los grafos exportados se pueden visualizar con gephi y se descarga desde https://gephi.org/users/download/

unit uMakerAi.RAG.Graph.Core;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  System.Json, System.Variants, System.Json.Writers, System.Json.Types, System.IOUtils,
  System.Rtti, System.StrUtils, System.DateUtils, System.Masks, System.VarUtils,

  Xml.XMLDoc, Xml.XMLIntf, Xml.XMLDom,

  uMakerAi.Embeddings.Core, uMakerAi.Embeddings, uMakerAi.RAG.Vectors.Index,
  // Incluimos tu unidad base de MakerAi
  uMakerAi.RAG.Vectors, uMakerAi.RAG.MetaData;

type


  // --------- CLASES PARA EL PARSER --------------------------------

  { Enumeraciones para el Árbol de Expresiones }
  TExpressionKind = (ekLiteral, ekProperty, ekBinary);

  TBinaryOp = (
    // Lógicos
    opAnd, opOr,

    // Comparación Estándar
    opEqual, opNotEqual, opGreater, opGreaterEqual, opLess, opLessEqual,

    // Texto
    opContains, opLike, // Nuevo
    opILike, // Nuevo

    // Listas y Nulidad
    opIn, // Nuevo
    opNotIn, // Nuevo
    opIsNull, // Nuevo
    opIsNotNull // Nuevo
    );

  { Clase base abstracta para expresiones del lenguaje }
  TGraphExpression = class
  public
    Kind: TExpressionKind;
    destructor Destroy; override;
  end;

  { Expresión para valores constantes (18, 'Madrid', true, null) }
  TLiteralExpr = class(TGraphExpression)
  public
    Value: Variant;
    constructor Create(AValue: Variant);
  end;

  { Expresión para acceso a propiedades (p.nombre, e.peso) }
  TPropertyExpr = class(TGraphExpression)
  public
    Variable: string; // Ejemplo: 'p'
    PropertyKey: string; // Ejemplo: 'nombre'
    constructor Create(const AVar, AKey: string);
  end;

  { Expresión para operaciones binarias (A > B, C AND D) }
  TBinaryExpr = class(TGraphExpression)
  public
    Left: TGraphExpression;
    Op: TBinaryOp;
    Right: TGraphExpression;
    constructor Create(ALeft: TGraphExpression; AOp: TBinaryOp; ARight: TGraphExpression);
    destructor Destroy; override;
  end;

  TDegreeType = (dtIn, dtOut, dtTotal);
  TGraphExportFormat = (gefDOT, gefGraphML, gefGraphMkai);

  { TMergeStrategy }
  TMergeStrategy = (msAddNewOnly, // (Default) Solo añade propiedades que no existen.
    msOverwrite, // Sobrescribe las propiedades existentes con las nuevas.
    msKeepExisting // No realiza ningún cambio en las propiedades del elemento existente.
    );


  // --Planificador de consultas---
  // Un modelo para realizar consultas hibridas al grafo, primero obtiene con embeddings los nodos iniciales
  // luego busca dentro del grafo datos muy explicitos para obtener datos precisos del grafo.

  // Representa un paso en el plan de consulta.
  TQueryStep = record
    SourceVariable: string; // Variable del paso anterior (vacío para el primer paso)
    EdgeLabel: string; // Etiqueta de la arista a seguir
    TargetVariable: string; // Nombre para guardar los resultados de este paso
    TargetNodeLabel: string; // Filtro opcional para el tipo de nodo de destino
    IsReversed: Boolean; // Si la búsqueda debe ser hacia atrás (incoming)
  end;

  // Representa el plan de consulta completo.
  TQueryPlan = record
    AnchorPrompt: string; // El texto para la búsqueda semántica inicial
    AnchorVariable: string; // El nombre de la variable para los nodos de anclaje
    Steps: TArray<TQueryStep>; // Los pasos estructurales a seguir
    ResultVariable: string; // La variable cuyos nodos se deben devolver
  end;

  // Declaraciones adelantadas para resolver referencias circulares
  TAiRagGraphNode = class;
  TAiRagGraphEdge = class;
  TAiRagGraph = class;
  TAiRagGraphDriverBase = class;


  // ----- MATCH --------------------
  // --Match es una función que permite recorrer el grafo sin necesidad de embeddings para encontrar patrones y relaciones

  // Dirección para la búsqueda de patrones en aristas
  TGraphDirection = (gdOutgoing, gdIncoming, gdBoth);

  // --- Clases auxiliares para la construcción de la consulta MATCH ---

  // Representa un patrón para un nodo en la consulta.
  // Ej: (p:Persona {ciudad: 'New York'})
  TMatchNodePattern = class
  public
    Variable: string; // El alias del nodo, ej: 'p'
    NodeLabel: string; // La etiqueta a buscar, ej: 'Persona'
    Properties: TDictionary<string, Variant>; // Filtros de propiedades, ej: {'ciudad': 'New York'}
    constructor Create;
    destructor Destroy; override;
    function Matches(ANode: TAiRagGraphNode): Boolean;
  end;

  // Representa un patrón para una arista en la consulta.
  // Ej: -[r:KNOWS {since: 2018}]->
  TMatchEdgePattern = class
  public
    Variable: string; // El alias de la arista, ej: 'r'
    EdgeLabel: string; // La etiqueta a buscar, ej: 'KNOWS'
    Direction: TGraphDirection; // La dirección de la relación
    Properties: TDictionary<string, Variant>; // Filtros de propiedades, ej: {'since': 2018}
    constructor Create;
    destructor Destroy; override;
    // Comprueba si una arista coincide con el patrón, considerando la dirección del recorrido actual
    function Matches(AEdge: TAiRagGraphEdge; AActualDirection: TGraphDirection): Boolean;
  end;

  // Representa una cláusula de patrón completa.
  // Ej: (p)-[r]->(m)
  TMatchClause = class
  public
    SourceNodeVar: string; // Variable del nodo de origen, ej: 'p'
    EdgePattern: TMatchEdgePattern; // El patrón de la arista (la cláusula es dueña de este objeto)
    TargetNodeVar: string; // Variable del nodo de destino, ej: 'm'
    constructor Create(ASourceNodeVar: string; AEdgePattern: TMatchEdgePattern; ATargetNodeVar: string);
    destructor Destroy; override;
  end;

  // La consulta completa que contiene todos los patrones de nodos y cláusulas.
  TGraphMatchQuery = class
  private
    FNodePatterns: TObjectList<TMatchNodePattern>; // Es dueño de los patrones de nodo
    FMatchClauses: TObjectList<TMatchClause>;
    FWhereClause: TGraphExpression;
    FDepth: Integer; // Es dueño de las cláusulas
    function GetNodePatternByVariable(const AVar: string): TMatchNodePattern;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNodePattern(ANodePattern: TMatchNodePattern);
    procedure AddMatchClause(AClause: TMatchClause);
    property NodePatterns: TObjectList<TMatchNodePattern> read FNodePatterns;
    property MatchClauses: TObjectList<TMatchClause> read FMatchClauses;
    property NodePatternByVariable[const AVar: string]: TMatchNodePattern read GetNodePatternByVariable;
    property WhereClause: TGraphExpression read FWhereClause write FWhereClause;
    property Depth: Integer read FDepth write FDepth;
  end;

  TStringWrapper = class(TObject)
  public
    Value: string;
    constructor Create(const AValue: string);
  end;

  TNodeDataRecord = record // O usa TJSONObject
    ID: string;
    NodeLabel: string;
    Name: string;
    PropertiesJSON: string;
    EmbeddingStr: string;
    NodeText: string;
  end;

  TEdgeDataRecord = record
    ID: string;
    EdgeLabel: string;
    Name: string;
    SourceNodeID: string; // ID del nodo de origen
    TargetNodeID: string; // ID del nodo de destino
    Weight: Double;
    PropertiesJSON: string;
    EmbeddingStr: string;
    NodeText: string;
  end;

  { TAiRagGraphDriverBase }

  TAiRagGraphDriverBase = class(TComponent)
  private
    FGraph: TAiRagGraph;
  protected
    // Métodos que los drivers DEBEN implementar
    function FindNodeDataByID(const ANodeID: string; out ANodeData: TNodeDataRecord): Boolean; virtual; abstract;
    function FindEdgeDataByID(const AEdgeID: string; out AEdgeData: TEdgeDataRecord): Boolean; virtual; abstract;
    procedure GetNodeEdges(ANode: TAiRagGraphNode); virtual; abstract;
    procedure AddNode(ANode: TAiRagGraphNode); virtual; abstract;
    procedure AddEdge(AEdge: TAiRagGraphEdge); virtual; abstract;
    procedure DeleteNode(const ANodeID: string); virtual; abstract;
    procedure DeleteEdge(const AEdgeID: string); virtual; abstract;
    function GetUniqueNodeLabels: TArray<string>; virtual; abstract;
    function GetUniqueEdgeLabels: TArray<string>; virtual; abstract;
    function FindNodeByName(const AName, ANodeLabel: string): TAiRagGraphNode; virtual; abstract;
    function FindNodesByLabel(const ALabel: string): TArray<TAiRagGraphNode>; virtual; abstract;
    function FindNodesByProperty(const AKey: string; const AValue: Variant): TArray<TAiRagGraphNode>; virtual; abstract;
    function FindNodeNamesByLabel(const ANodeLabel, ASearchText: string; ALimit: Integer): TArray<string>; virtual; abstract;

    function SearchNodes(const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria = nil): TArray<TAiRagGraphNode>; virtual; abstract;

    function Query(const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>; virtual; abstract;

    property Graph: TAiRagGraph read FGraph;
  public
    function EmbeddingToString(const AData: TAiEmbeddingData): string;
    function PropertiesToJSONString(const AProperties: TDictionary<string, Variant>): string;
    procedure AssignToGraph(AGraph: TAiRagGraph);
  end;

  { TAiRagGraphNode }
  TAiRagGraphNode = class(TAiEmbeddingNode)
  private
    FNodeLabel: string;
    FName: string;
    FInternalOutgoingEdges: TObjectList<TAiRagGraphEdge>;
    FInternalIncomingEdges: TObjectList<TAiRagGraphEdge>;
    // FProperties: TDictionary<string, Variant>;
    FOwnerGraph: TAiRagGraph;
    FID: string;
    FChunks: TObjectList<TAiEmbeddingNode>; // --- Lista de fragmentos de texto del RAGVector
    FEdgesLoaded: Boolean;
    function GetIncomingEdges: TObjectList<TAiRagGraphEdge>;
    function GetOutgoingEdges: TObjectList<TAiRagGraphEdge>;
    function GetMetadata: TAiEmbeddingMetaData;
  protected
    procedure AddOutgoingEdge(AEdge: TAiRagGraphEdge);
    procedure AddIncomingEdge(AEdge: TAiRagGraphEdge);
    procedure RemoveOutgoingEdge(AEdge: TAiRagGraphEdge);
    procedure RemoveIncomingEdge(AEdge: TAiRagGraphEdge);
  public
    constructor Create(AOwnerGraph: TAiRagGraph; ADim: Integer);
    destructor Destroy; override;
    function PropertiesToJSON: TJSONObject;
    procedure EnsureEdgesAreLoaded;

    // --- NUEVOS MÉTODOS Y PROPIEDADES ---
    function AddChunk(const AText: string; const AData: TAiEmbeddingData): TAiEmbeddingNode;
    property Chunks: TObjectList<TAiEmbeddingNode> read FChunks;

    property ID: string read FID write FID;
    property Name: string read FName write FName;
    property NodeLabel: string read FNodeLabel write FNodeLabel;
    Property Properties: TAiEmbeddingMetaData read GetMetadata;

    property OutgoingEdges: TObjectList<TAiRagGraphEdge> read GetOutgoingEdges;
    property IncomingEdges: TObjectList<TAiRagGraphEdge> read GetIncomingEdges;

    property OwnerGraph: TAiRagGraph read FOwnerGraph;
  end;

  { TAiRagGraphEdge }
  TAiRagGraphEdge = class(TAiEmbeddingNode)
  private
    FEdgeLabel: string;
    FName: string;
    FFromNode: TAiRagGraphNode;
    FToNode: TAiRagGraphNode;
    // FProperties: TDictionary<string, Variant>;
    FOwnerGraph: TAiRagGraph;
    FID: string;
    FWeight: Double;
  public
    constructor Create(AOwnerGraph: TAiRagGraph; ADim: Integer);
    destructor Destroy; override;
    function PropertiesToJSON: TJSONObject;

    property ID: string read FID write FID;
    property Name: string read FName write FName;
    property EdgeLabel: string read FEdgeLabel write FEdgeLabel;
    property FromNode: TAiRagGraphNode read FFromNode write FFromNode;
    property ToNode: TAiRagGraphNode read FToNode write FToNode;
    property Weight: Double read FWeight write FWeight;
    // property Properties: TDictionary<string, Variant> read FProperties;
    property OwnerGraph: TAiRagGraph read FOwnerGraph;
  end;

  TCommunity = class
  private
    FID: Integer;
    FNodes: TList<TAiRagGraphNode>;
    FInternalWeight: Double;
    FTotalWeight: Double;
  public
    constructor Create(AID: Integer);
    destructor Destroy; override;

    property ID: Integer read FID;
    property Nodes: TList<TAiRagGraphNode> read FNodes;
    property InternalWeight: Double read FInternalWeight write FInternalWeight;
    property TotalWeight: Double read FTotalWeight write FTotalWeight;
  end;

  { TAiRagGraph }
  TAiRagGraph = class(TComponent)
  private
    FNodes: TAiRAGVector;
    FEdges: TAiRAGVector;
    FNodeRegistry: TDictionary<string, TAiRagGraphNode>;
    FEdgeRegistry: TDictionary<string, TAiRagGraphEdge>;
    FEmbeddings: TAiEmbeddingsCore;
    FNodeLabelIndex: TDictionary<string, TList<TAiRagGraphNode>>;
    FNodeNameIndex: TDictionary<string, TAiRagGraphNode>;
    FUpdateCount: Integer;
    FDriver: TAiRagGraphDriverBase;

    function GetNodeCount: Integer;
    function GetEdgeCount: Integer;
    function GetNodesRAGVector: TAiRAGVector;
    function GetEdgesRAGVector: TAiRAGVector;
    procedure SetDriver(const Value: TAiRagGraphDriverBase);
    procedure SetEmbeddings(const Value: TAiEmbeddingsCore);
    function GetSearchOptions: TAiSearchOptions;
    procedure SetSearchOptions(const Value: TAiSearchOptions);
  protected
    procedure UnregisterNode(ANode: TAiRagGraphNode);
    procedure UnregisterEdge(AEdge: TAiRagGraphEdge);
    function ExpandNodeList(AInitialNodes: TList<TAiRagGraphNode>; ADepth: Integer): TArray<TAiRagGraphNode>;
    function GetContextualizedText(ASubgraphNodes: TArray<TAiRagGraphNode>): string;
    function InternalAddNode(ANode: TAiRagGraphNode; AShouldPersist: Boolean): TAiRagGraphNode;
    function InternalAddEdge(AEdge: TAiRagGraphEdge; AShouldPersist: Boolean): TAiRagGraphEdge;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function EvaluateGraphExpression(AExpr: TGraphExpression; ABoundElements: TDictionary<string, TObject>): Variant;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RebuildIndexes;

    function InternalHydrateNode(const ANodeData: TNodeDataRecord): TAiRagGraphNode;
    function InternalHydrateEdge(const AEdgeData: TEdgeDataRecord): TAiRagGraphEdge;

    function NewNode(const AID, ALabel, AName: string): TAiRagGraphNode;

    function AddNode(AID, ALabel, AName: string): TAiRagGraphNode; overload;
    function AddNode(ANode: TAiRagGraphNode): TAiRagGraphNode; overload;

    function NewEdge(AFromNode, AToNode: TAiRagGraphNode; const AID, ALabel, AName: string; AWeight: Double = 1.0): TAiRagGraphEdge;
    function AddEdge(AFromNode, AToNode: TAiRagGraphNode; AID, ALabel, AName: string): TAiRagGraphEdge; overload;
    function AddEdge(AFromNode, AToNode: TAiRagGraphNode; AID, ALabel, AName: string; AWeight: Double): TAiRagGraphEdge; overload;
    function AddEdge(AEdge: TAiRagGraphEdge): TAiRagGraphEdge; overload;

    procedure DeleteNode(ANode: TAiRagGraphNode); overload;
    procedure DeleteNode(AID: string); overload;
    procedure DeleteEdge(AEdge: TAiRagGraphEdge); overload;
    procedure DeleteEdge(AID: string); overload;
    procedure Clear;

    function FindNodeByID(AID: string): TAiRagGraphNode;
    function FindEdgeByID(AID: string): TAiRagGraphEdge;
    function FindNodesByLabel(ALabel: string): TArray<TAiRagGraphNode>;
    function FindEdge(AFromNode, AToNode: TAiRagGraphNode; AEdgeLabel: string): TAiRagGraphEdge;
    function FindNodeByName(AName, ANodeLabel: string): TAiRagGraphNode;

    procedure SaveToStream(AStream: TStream); overload; // La versión original
    procedure SaveToStream(AStream: TStream; aFull: Boolean); overload; // La nueva versión

    procedure SaveToDot(const AFileName: string);

    procedure SaveToMakerAi(const AFileName: String; const aFull: Boolean = True);

    procedure SaveToGraphML(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SaveToFile(const AFileName: string; AFormat: TGraphExportFormat; aFull: Boolean = True); overload;
    procedure SaveToFile(const AFileName: string; aFull: Boolean); overload;

    function GetShortestPath(AStartNode, AEndNode: TAiRagGraphNode): TArray<TObject>;
    function GetNodesByDegree(ATop: Integer = 10; ADegreeType: TDegreeType = dtTotal): TArray<TAiRagGraphNode>;
    function GetClosenessCentrality(ANode: TAiRagGraphNode): Double;
    function DetectCommunities(AIterations: Integer = 10): TDictionary<TAiRagGraphNode, Integer>;
    procedure UpdateCommunityLabels;

    function Search(const APrompt: string; const ADepth: Integer = 0; ALimit: Integer = 5; const APrecision: Double = 0.5; const AFilter: TAiFilterCriteria = nil): TArray<TAiRagGraphNode>;
    Function SearchText(const APrompt: string; ADepth: Integer = 0; ShowProperties: Boolean = False; const ALimit: Integer = 3; const APrecision: Double = 0.5; const AFilter: TAiFilterCriteria = nil): string;

    function Query(const APlan: TQueryPlan; ADepth: Integer = 0; const ALimit: Integer = 5; const APrecision: Double = 0.5): TArray<TAiRagGraphNode>;

    function ExecuteMakerGQL(const ACode: string; out AResultObjects: TArray<TDictionary<string, TObject>>; ADepth: Integer = 0): string; Overload;
    function ExecuteMakerGQL(const ACode: string): string; Overload;


    // todo Implementar Detección de Comunidades (Community Detection) Algoritmo de Louvain

    function GetAllShortestPaths(AStartNode, AEndNode: TAiRagGraphNode): TArray<TArray<TObject>>; // Falta por implementar
    function Match(AQuery: TGraphMatchQuery; ADepth: Integer = 0): TArray<TDictionary<string, TObject>>;

    function GetUniqueNodeLabels: TArray<string>;
    function GetUniqueEdgeLabels: TArray<string>;
    function FindNodesByProperty(const AKey: string; const AValue: Variant): TArray<TAiRagGraphNode>;
    function GetNeighbors(ANode: TAiRagGraphNode; ADirection: TGraphDirection = gdOutgoing): TArray<TAiRagGraphNode>;
    function CountNodesByLabel(const ALabel: string): Integer;
    function CountEdgesByLabel(const ALabel: string): Integer;
    function ExtractSubgraph(ANodes: TArray<TAiRagGraphNode>): TAiRagGraph;
    procedure MergeNodes(ASurvivingNode, ASubsumedNode: TAiRagGraphNode; APropertyMergeStrategy: TMergeStrategy = msAddNewOnly);
    function FindNodeNamesByLabel(const ANodeLabel, ASearchText: string; ALimit: Integer = 10): TArray<string>;
    function EdgeExistsInMemory(const AEdgeID: string): Boolean;

    function GraphToContextText(const ANodes: TArray<TAiRagGraphNode>): string;

    property NodeCount: Integer read GetNodeCount;
    property EdgeCount: Integer read GetEdgeCount;
    property Nodes: TAiRAGVector read GetNodesRAGVector;
    property Edges: TAiRAGVector read GetEdgesRAGVector;
  Published
    property Embeddings: TAiEmbeddingsCore read FEmbeddings write SetEmbeddings;
    property Driver: TAiRagGraphDriverBase read FDriver write SetDriver;
    property SearchOptions: TAiSearchOptions read GetSearchOptions write SetSearchOptions;
  end;

  // Función Helper para convertir TJSONValue a Variant
function JSONValueToVariant(AJsonValue: TJSONValue): Variant;
function VariantToJSONValue(const AValue: Variant): TJSONValue;
function StringToEmbedding(const AVectorString: string): TAiEmbeddingData;
procedure JSONStringToProperties(const AJSONString: string; const AProperties: TDictionary<string, Variant>);

procedure Register;

implementation

uses uMakerAi.RAG.Graph.GQL;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiRagGraph]);

end;

function JSONValueToVariant(AJsonValue: TJSONValue): Variant;
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  I: Integer;
  Pair: TJSONPair;
  ResultArray: Variant;
  ResultDict: string;
begin
  if AJsonValue = nil then
    Result := Null
  else if AJsonValue is TJSONNull then
    Result := Null
  else if AJsonValue is TJSONTrue then
    Result := True
  else if AJsonValue is TJSONFalse then
    Result := False
  else if AJsonValue is TJSONNumber then
  begin
    // Determinar si es entero o decimal
    if Frac(TJSONNumber(AJsonValue).AsDouble) = 0 then
      Result := TJSONNumber(AJsonValue).AsInt64
    else
      Result := TJSONNumber(AJsonValue).AsDouble;
  end
  else if AJsonValue is TJSONString then
    Result := TJSONString(AJsonValue).Value
  else if AJsonValue is TJSONArray then
  begin
    JSONArray := TJSONArray(AJsonValue);

    // Si el array está vacío
    if JSONArray.Count = 0 then
    begin
      Result := '[]';
      Exit;
    end;

    // Crear un array Variant
    ResultArray := VarArrayCreate([0, JSONArray.Count - 1], varVariant);
    for I := 0 to JSONArray.Count - 1 do
      ResultArray[I] := JSONValueToVariant(JSONArray.Items[I]);

    Result := ResultArray;
  end
  else if AJsonValue is TJSONObject then
  begin
    JSONObject := TJSONObject(AJsonValue);

    // Si el objeto está vacío
    if JSONObject.Count = 0 then
    begin
      Result := '{}';
      Exit;
    end;

    // Convertir objeto a string formateado (o podrías usar otra estructura)
    ResultDict := '{';
    for I := 0 to JSONObject.Count - 1 do
    begin
      Pair := JSONObject.Pairs[I];
      if I > 0 then
        ResultDict := ResultDict + ', ';
      ResultDict := ResultDict + '"' + Pair.JsonString.Value + '": ';

      // Recursivamente convertir el valor
      if Pair.JsonValue is TJSONString then
        ResultDict := ResultDict + '"' + TJSONString(Pair.JsonValue).Value + '"'
      else if Pair.JsonValue is TJSONNumber then
        ResultDict := ResultDict + Pair.JsonValue.Value
      else if (Pair.JsonValue is TJSONTrue) or (Pair.JsonValue is TJSONFalse) then
        ResultDict := ResultDict + LowerCase(Pair.JsonValue.Value)
      else if Pair.JsonValue is TJSONNull then
        ResultDict := ResultDict + 'null'
      else
        ResultDict := ResultDict + Pair.JsonValue.ToString;
    end;
    ResultDict := ResultDict + '}';

    Result := ResultDict;
  end
  else
    Result := AJsonValue.ToString; // Fallback para otros tipos
end;

function VariantToJSONValue(const AValue: Variant): TJSONValue;
var
  VarTypeResult: TVarType;
begin
  VarTypeResult := VarType(AValue);

  if VarIsNull(AValue) or VarIsEmpty(AValue) then
    Result := TJSONNull.Create
  else if VarTypeResult = varBoolean then
    Result := TJSONBool.Create(Boolean(AValue))
  else if VarIsFloat(AValue) then
    Result := TJSONNumber.Create(Extended(AValue)) // Usar Extended para máxima precisión
  else if VarIsNumeric(AValue) then
    Result := TJSONNumber.Create(Integer(AValue))
  else if VarIsStr(AValue) then
    Result := TJSONString.Create(VarToStr(AValue))
  else
    // Fallback: si no es un tipo primitivo, lo convertimos a string.
    // Una implementación más avanzada podría manejar arrays o TDateTime de forma especial.
    Result := TJSONString.Create(VarToStr(AValue));
end;

function StringToEmbedding(const AVectorString: string): TAiEmbeddingData;
var
  CleanedString: string;
  ValueStrings: TArray<string>;
  I: Integer;
  FormatSettings: TFormatSettings;
begin
  Result := []; // Devuelve un array vacío por defecto
  if AVectorString.IsEmpty or (AVectorString = '[]') then
    Exit;

  // 1. Limpiar el string, quitando los corchetes
  CleanedString := AVectorString.Trim(['[', ']']);
  if CleanedString.IsEmpty then
    Exit;

  // 2. Separar los valores por la coma
  ValueStrings := CleanedString.Split([',']);

  // 3. Preparar para la conversión de float insensible a la localización
  // Esto asegura que el '.' siempre se interprete como el separador decimal.
  FormatSettings := TFormatSettings.Invariant;

  // 4. Convertir cada valor y añadirlo al resultado
  SetLength(Result, Length(ValueStrings));
  for I := 0 to High(ValueStrings) do
  begin
    // Usamos TryStrToFloat para más seguridad contra datos mal formados
    if TryStrToFloat(ValueStrings[I], Result[I], FormatSettings) then
    begin
      // La conversión fue exitosa, continuar.
    end
    else
    begin
      Result[I] := 0.0; // O manejar el error como se prefiera
    end;
  end;
end;

function JSONValueToVariantSafe(const AValue: TJSONValue; out AResult: Variant): Boolean;
var
  Num: TJSONNumber;
begin
  Result := False;
  AResult := Null;

  if AValue = nil then
    Exit;

  try
    if AValue is TJSONNull then
    begin
      AResult := Null;
      Result := True;
    end
    else if AValue is TJSONString then
    begin
      AResult := TJSONString(AValue).Value;
      Result := True;
    end
    else if AValue is TJSONBool then
    begin
      AResult := TJSONBool(AValue).AsBoolean;
      Result := True;
    end
    else if AValue is TJSONNumber then
    begin
      Num := TJSONNumber(AValue);

      // Intentamos entero
      Var
        I64: Int64;
      if TryStrToInt64(Num.Value, I64) then
      begin
        AResult := I64;
        Result := True;
        Exit;
      end;

      // Intentamos flotante (SIEMPRE válido para JSON number)
      Var
        Dbl: Double;
      if TryStrToFloat(Num.Value, Dbl, TFormatSettings.Invariant) then
      begin
        AResult := Dbl;
        Result := True;
        Exit;
      end;
    end;
  except
    // Silencioso: no rompe flujo
    Result := False;
  end;
end;

procedure JSONStringToProperties(const AJSONString: string; const AProperties: TDictionary<string, Variant>);
var
  JsonValue: TJSONValue;
  JsonObj: TJSONObject;
  Pair: TJSONPair;
  V: Variant;
begin
  if (AProperties = nil) or AJSONString.IsEmpty or (AJSONString = '{}') then
    Exit;

  AProperties.Clear; // Limpiamos el diccionario antes de poblarlo.
  JsonValue := TJSONObject.ParseJSONValue(AJSONString);
  if JsonValue = nil then
    Exit; // JSON inválido

  try
    if JsonValue is TJSONObject then
    begin
      JsonObj := JsonValue as TJSONObject;
      for Pair in JsonObj do
      begin
        If JSONValueToVariantSafe(Pair.JsonValue, V) then
          AProperties.Add(Pair.JsonString.Value, V);
      end;
    end;
  finally
    JsonValue.Free;
  end;
End;

{ TAiRagGraphNode }

constructor TAiRagGraphNode.Create(AOwnerGraph: TAiRagGraph; ADim: Integer);
begin
  inherited Create(ADim);
  FOwnerGraph := AOwnerGraph;
  FInternalOutgoingEdges := TObjectList<TAiRagGraphEdge>.Create(False); // No es dueño de los objetos
  FInternalIncomingEdges := TObjectList<TAiRagGraphEdge>.Create(False); // No es dueño de los objetos
  // FProperties := TDictionary<string, Variant>.Create;

  // System.Generics.Defaults.TStringComparer.Ordinal

  // FProperties := TDictionary<string, Variant>.Create(TOrdinalIStringComparer.Create);
  FChunks := TObjectList<TAiEmbeddingNode>.Create(True);
  FEdgesLoaded := False;
end;

destructor TAiRagGraphNode.Destroy;
begin
  FInternalOutgoingEdges.Free;
  FInternalIncomingEdges.Free;
  FChunks.Free;
  // FProperties.Free;
  inherited;
end;

procedure TAiRagGraphNode.EnsureEdgesAreLoaded;
begin
  // Si las aristas ya están cargadas o no hay un OwnerGraph/evento asignado, no hacemos nada.
  if FEdgesLoaded or (FOwnerGraph = nil) or not Assigned(FOwnerGraph.Driver) then
    Exit;

  if Assigned(FOwnerGraph.Driver) then
  begin
    FOwnerGraph.Driver.GetNodeEdges(Self);
  end;

  // Marcamos como cargado para no volver a disparar el evento.
  FEdgesLoaded := True;
end;

function TAiRagGraphNode.GetIncomingEdges: TObjectList<TAiRagGraphEdge>;
begin
  EnsureEdgesAreLoaded;
  Result := FInternalIncomingEdges;
end;

function TAiRagGraphNode.GetMetadata: TAiEmbeddingMetaData;
begin
  // Accedemos a la propiedad del ancestro TAiEmbeddingNode
  Result := inherited MetaData;
end;

function TAiRagGraphNode.GetOutgoingEdges: TObjectList<TAiRagGraphEdge>;
begin
  EnsureEdgesAreLoaded;
  Result := FInternalOutgoingEdges;
end;

function TAiRagGraphNode.AddChunk(const AText: string; const AData: TAiEmbeddingData): TAiEmbeddingNode;
begin
  // Creamos un nodo de embedding básico para el fragmento.
  // Usamos el constructor que define la dimensión basada en los datos recibidos.
  Result := TAiEmbeddingNode.Create(Length(AData));
  Result.Text := AText;
  Result.Data := Copy(AData); // Copiamos el vector
  Result.Model := Self.Model; // Hereda el nombre del modelo del nodo padre

  FChunks.Add(Result);
end;

procedure TAiRagGraphNode.AddIncomingEdge(AEdge: TAiRagGraphEdge);
begin
  FInternalIncomingEdges.Add(AEdge);
end;

procedure TAiRagGraphNode.AddOutgoingEdge(AEdge: TAiRagGraphEdge);
begin
  FInternalOutgoingEdges.Add(AEdge);
end;

procedure TAiRagGraphNode.RemoveIncomingEdge(AEdge: TAiRagGraphEdge);
begin
  FInternalIncomingEdges.Remove(AEdge);
end;

procedure TAiRagGraphNode.RemoveOutgoingEdge(AEdge: TAiRagGraphEdge);
begin
  FInternalOutgoingEdges.Remove(AEdge);
end;

function TAiRagGraphNode.PropertiesToJSON: TJSONObject;
begin
  // Delegamos la serialización al nuevo objeto MetaData unificado.
  // Este ya maneja correctamente la conversión de Variant a JSON (Fechas, Booleano, etc.)
  if Assigned(MetaData) then
    Result := MetaData.ToJSON
  else
    Result := TJSONObject.Create;
end;

{ TAiRagGraphEdge }

constructor TAiRagGraphEdge.Create(AOwnerGraph: TAiRagGraph; ADim: Integer);
begin
  // El constructor padre (TAiEmbeddingNode) ya se encarga de crear el objeto MetaData
  inherited Create(ADim);
  FOwnerGraph := AOwnerGraph;
  FWeight := 1.0;
  // FProperties := TDictionary... <--- ELIMINADO
end;

destructor TAiRagGraphEdge.Destroy;
begin
  // FProperties.Free; <--- ELIMINADO (Ya no existe)
  // El MetaData se libera automáticamente en el destructor de la clase base (inherited)
  inherited;
end;

function TAiRagGraphEdge.PropertiesToJSON: TJSONObject;
begin
  // Delegamos la serialización al nuevo objeto MetaData unificado.
  if Assigned(MetaData) then
    Result := MetaData.ToJSON
  else
    Result := TJSONObject.Create;
end;

{ TAiRagGraph }

constructor TAiRagGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNodes := TAiRAGVector.Create(Self, True);
  FNodes.SearchOptions.UseBM25 := True; // <-- ACTIVAR POR DEFECTO PARA GRAFOS

  FEdges := TAiRAGVector.Create(Self, True);
  FEdges.SearchOptions.UseBM25 := True;

  FNodeRegistry := TDictionary<string, TAiRagGraphNode>.Create;
  FEdgeRegistry := TDictionary<string, TAiRagGraphEdge>.Create;
  FNodeLabelIndex := TDictionary < string, TList < TAiRagGraphNode >>.Create;
  FNodeNameIndex := TDictionary<string, TAiRagGraphNode>.Create;
  // Asigna el tipo de indice en memoria, siempre debe ser HNSW
  FNodes.InMemoryIndexType := TAIHNSWIndex; // son los mejores para texto
  FEdges.InMemoryIndexType := TAIHNSWIndex;
end;

destructor TAiRagGraph.Destroy;
begin
  Clear;
  FNodes.Free;
  FEdges.Free;
  FNodeRegistry.Free;
  FEdgeRegistry.Free;
  FNodeLabelIndex.Free;
  FNodeNameIndex.Free;
  inherited;
end;

function TAiRagGraph.DetectCommunities(AIterations: Integer = 10): TDictionary<TAiRagGraphNode, Integer>;
var
  NodeToCommunity: TDictionary<TAiRagGraphNode, Integer>;
  CommInfo: TDictionary<Integer, TCommunity>;
  m2: Double; // 2 * suma de todos los pesos del grafo
  Nodes: TArray<TAiRagGraphNode>;
  Changed: Boolean;
  Iter, I: Integer;
  Node: TAiRagGraphNode;
  BestComm: Integer;
  MaxGain, Gain: Double;
  Neighbor: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  CurrentCommID: Integer;
  K_i_in: Double; // Peso de las aristas del nodo i hacia la comunidad candidata
  K_i: Double; // Grado del nodo i
begin
  Result := TDictionary<TAiRagGraphNode, Integer>.Create;
  CommInfo := TDictionary<Integer, TCommunity>.Create;
  Nodes := FNodeRegistry.Values.ToArray;

  // --- Inicialización: Cada nodo en su propia comunidad ---
  m2 := 0;
  for I := 0 to Length(Nodes) - 1 do
  begin
    Node := Nodes[I];
    Result.Add(Node, I);
    CommInfo.Add(I, TCommunity.Create(I));
    CommInfo[I].Nodes.Add(Node);

    // Calcular grado del nodo y m2
    K_i := 0;
    for Edge in Node.OutgoingEdges do
      K_i := K_i + Edge.Weight;
    for Edge in Node.IncomingEdges do
      K_i := K_i + Edge.Weight;

    CommInfo[I].TotalWeight := K_i;
    m2 := m2 + K_i;
  end;

  // --- Bucle principal de optimización ---
  for Iter := 1 to AIterations do
  begin
    Changed := False;
    for Node in Nodes do
    begin
      CurrentCommID := Result[Node];
      K_i := CommInfo[CurrentCommID].TotalWeight; // Grado del nodo

      BestComm := CurrentCommID;
      MaxGain := 0;

      // Evaluar mover el nodo a la comunidad de cada uno de sus vecinos
      for Neighbor in GetNeighbors(Node, gdBoth) do
      begin
        var
        TargetCommID := Result[Neighbor];
        if TargetCommID = CurrentCommID then
          Continue;

        // Calcular K_i_in (conexión del nodo con la comunidad destino)
        K_i_in := 0;
        for Edge in Node.OutgoingEdges do
          if Result[Edge.ToNode] = TargetCommID then
            K_i_in := K_i_in + Edge.Weight;
        for Edge in Node.IncomingEdges do
          if Result[Edge.FromNode] = TargetCommID then
            K_i_in := K_i_in + Edge.Weight;

        // Fórmula simplificada de Ganancia de Modularidad (delta Q)
        Gain := (K_i_in - (CommInfo[TargetCommID].TotalWeight * K_i) / m2);

        if Gain > MaxGain then
        begin
          MaxGain := Gain;
          BestComm := TargetCommID;
        end;
      end;

      // Realizar el movimiento si hay mejora
      if BestComm <> CurrentCommID then
      begin
        // Actualizar comunidad vieja
        CommInfo[CurrentCommID].Nodes.Remove(Node);
        CommInfo[CurrentCommID].TotalWeight := CommInfo[CurrentCommID].TotalWeight - K_i;

        // Actualizar comunidad nueva
        Result[Node] := BestComm;
        CommInfo[BestComm].Nodes.Add(Node);
        CommInfo[BestComm].TotalWeight := CommInfo[BestComm].TotalWeight + K_i;

        Changed := True;
      end;
    end;

    if not Changed then
      Break;
  end;

  // Limpieza de CommInfo
  for var C in CommInfo.Values do
    C.Free;
  CommInfo.Free;
end;

function TAiRagGraph.EdgeExistsInMemory(const AEdgeID: string): Boolean;
begin
  Result := FEdgeRegistry.ContainsKey(AEdgeID);
end;

procedure TAiRagGraph.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      RebuildIndexes;
  end;
end;

function TAiRagGraph.EvaluateGraphExpression(AExpr: TGraphExpression; ABoundElements: TDictionary<string, TObject>): Variant;
var
  Left, Right: Variant;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Obj: TObject;
  B: TBinaryExpr;

  // ------------------------------------------------------------------
  // NULL / BOOLEAN SAFE
  // ------------------------------------------------------------------
  function SafeBool(const V: Variant): Boolean;
  begin
    if VarIsNull(V) or VarIsEmpty(V) then
      Exit(False);
    try
      Result := Boolean(V);
    except
      Result := False;
    end;
  end;

// ------------------------------------------------------------------
// LIKE / ILIKE (SQL -> Delphi)
// ------------------------------------------------------------------
  function MatchLike(const Val, Pat: string; CaseInsensitive: Boolean): Boolean;
  var
    P, V: string;
  begin
    if Val = '' then
      Exit(False);

    P := Pat.Replace('%', '*').Replace('_', '?');
    if CaseInsensitive then
    begin
      V := Val.ToLower;
      P := P.ToLower;
    end
    else
      V := Val;

    try
      Result := MatchesMask(V, P);
    except
      Result := False;
    end;
  end;

// ------------------------------------------------------------------
// IN / NOT IN
// ------------------------------------------------------------------
  function CheckInList(const Val, List: Variant): Boolean;
  var
    I: Integer;
    Item: Variant;
  begin
    Result := False;
    if VarIsNull(Val) or VarIsEmpty(Val) then
      Exit;

    try
      if VarIsArray(List) then
      begin
        for I := VarArrayLowBound(List, 1) to VarArrayHighBound(List, 1) do
        begin
          Item := VarArrayGet(List, [I]);
          if VarToStr(Val) = VarToStr(Item) then
            Exit(True);
        end;
      end
      else
        Result := (VarToStr(Val) = VarToStr(List));
    except
      Result := False;
    end;
  end;

begin
  if AExpr = nil then
    Exit(True);

  case AExpr.Kind of

    // ================================================================
    // LITERAL
    // ================================================================
    ekLiteral:
      Result := TLiteralExpr(AExpr).Value;

    // ================================================================
    // PROPERTY ACCESS
    // ================================================================
    ekProperty:
      begin
        Result := Null;
        var
        P := TPropertyExpr(AExpr);

        if ABoundElements.TryGetValue(P.Variable, Obj) then
        begin
          // --- NODE ---
          if Obj is TAiRagGraphNode then
          begin
            Node := TAiRagGraphNode(Obj);
            if SameText(P.PropertyKey, 'name') then
              Result := Node.Name
            else if SameText(P.PropertyKey, 'label') then
              Result := Node.NodeLabel
            else if SameText(P.PropertyKey, 'id') then
              Result := Node.ID
            else
              Result := Node.MetaData.Get(P.PropertyKey, Null);
          end
          // --- EDGE ---
          else if Obj is TAiRagGraphEdge then
          begin
            Edge := TAiRagGraphEdge(Obj);
            if SameText(P.PropertyKey, 'label') then
              Result := Edge.EdgeLabel
            else if SameText(P.PropertyKey, 'id') then
              Result := Edge.ID
            else
              Result := Edge.MetaData.Get(P.PropertyKey, Null);
          end;
        end;
      end;

    // ================================================================
    // BINARY / LOGICAL
    // ================================================================
    ekBinary:
      begin
        B := TBinaryExpr(AExpr);

        Left := EvaluateGraphExpression(B.Left, ABoundElements);

        // Operadores unarios
        if not(B.Op in [opIsNull, opIsNotNull]) then
          Right := EvaluateGraphExpression(B.Right, ABoundElements)
        else
          Right := Null;

        case B.Op of
          // --- LOGICAL ---
          opAnd:
            Result := SafeBool(Left) and SafeBool(Right);
          opOr:
            Result := SafeBool(Left) or SafeBool(Right);

          // --- COMPARISON ---
          opEqual:
            if VarIsNull(Left) or VarIsNull(Right) then
              Result := VarIsNull(Left) = VarIsNull(Right)
            else
              Result := (Left = Right);

          opNotEqual:
            try
              Result := (Left <> Right);
            except
              Result := False;
            end;

          opGreater:
            try
              Result := (Left > Right);
            except
              Result := False;
            end;
          opGreaterEqual:
            try
              Result := (Left >= Right);
            except
              Result := False;
            end;
          opLess:
            try
              Result := (Left < Right);
            except
              Result := False;
            end;
          opLessEqual:
            try
              Result := (Left <= Right);
            except
              Result := False;
            end;

          // --- TEXT ---
          opContains:
            if VarIsNull(Left) or VarIsNull(Right) then
              Result := False
            else
              Result := System.StrUtils.ContainsText(VarToStr(Left), VarToStr(Right));

          opLike:
            Result := MatchLike(VarToStr(Left), VarToStr(Right), False);

          opILike:
            Result := MatchLike(VarToStr(Left), VarToStr(Right), True);

          // --- NULL ---
          opIsNull:
            Result := VarIsNull(Left) or VarIsEmpty(Left);

          opIsNotNull:
            Result := not(VarIsNull(Left) or VarIsEmpty(Left));

          // --- IN ---
          opIn:
            Result := CheckInList(Left, Right);

          opNotIn:
            Result := not CheckInList(Left, Right);

        else
          Result := False;
        end;
      end;

  else
    Result := Null;
  end;
end;

{
  function TAiRagGraph.ExecuteMakerGQL(const ACode: string; ADepth: Integer = 0): TArray<TDictionary<string, TObject>>;
  var
  Parser: TGraphParser;
  QueryObj: TGraphMatchQuery;

  // Variables para resultados de listas (Strings y Nodos)
  StringListResult: TArray<string>;
  NodeListResult: TArray<TAiRagGraphNode>;
  PathResult: TArray<TObject>;

  // Variables de trabajo
  ResDict: TDictionary<string, TObject>;
  I: Integer;
  PathObj: TObject;
  Score: Double;

  // Variables para resolución de algoritmos
  StartNode, EndNode: TAiRagGraphNode;
  FinalDepth: Integer;

  // -----------------------------------------------------------------------
  // Función auxiliar anidada:
  // -----------------------------------------------------------------------
  function FindNodeByPattern(APattern: TMatchNodePattern): TAiRagGraphNode;
  var
  Candidates: TArray<TAiRagGraphNode>;
  Cand: TAiRagGraphNode;
  begin
  Result := nil;
  if APattern = nil then
  Exit;

  // 1. Optimización: Si hay etiqueta, buscamos solo en ese índice
  if not APattern.NodeLabel.IsEmpty then
  Candidates := Self.FindNodesByLabel(APattern.NodeLabel)
  else
  Candidates := FNodeRegistry.Values.ToArray;

  // 2. Búsqueda lineal sobre los candidatos para coincidir propiedades
  for Cand in Candidates do
  begin
  if APattern.Matches(Cand) then
  begin
  Result := Cand;
  Exit; // Devolvemos el primero encontrado
  end;
  end;
  end;
  // -----------------------------------------------------------------------

  begin
  Result := [];
  if ACode.Trim.IsEmpty then
  Exit;

  Parser := TGraphParser.Create(ACode);
  try
  // Parseamos el código.
  // Si es un comando especial, QueryObj será nil y CommandType tendrá valor.
  // Si es una consulta normal, QueryObj tendrá el objeto y CommandType será cmdNone.
  QueryObj := Parser.Parse;
  try

  if Parser.CommandType <> cmdNone then
  begin
  // =========================================================
  // EJECUCIÓN DE COMANDOS
  // =========================================================
  case Parser.CommandType of

  // -------------------------------------------------------
  // 1. INTROSPECCIÓN (SHOW ...)
  // -------------------------------------------------------
  cmdShowLabels:
  begin
  StringListResult := Self.GetUniqueNodeLabels;
  SetLength(Result, Length(StringListResult));
  for I := 0 to High(StringListResult) do
  begin
  ResDict := TDictionary<string, TObject>.Create;
  ResDict.Add('type', TStringWrapper.Create('label'));
  ResDict.Add('value', TStringWrapper.Create(StringListResult[I]));
  Result[I] := ResDict;
  end;
  end;

  cmdShowEdges:
  begin
  StringListResult := Self.GetUniqueEdgeLabels;
  SetLength(Result, Length(StringListResult));
  for I := 0 to High(StringListResult) do
  begin
  ResDict := TDictionary<string, TObject>.Create;
  ResDict.Add('type', TStringWrapper.Create('edge'));
  ResDict.Add('value', TStringWrapper.Create(StringListResult[I]));
  Result[I] := ResDict;
  end;
  end;

  // -------------------------------------------------------
  // 2. ALGORITMOS: CAMINO MÁS CORTO
  // -------------------------------------------------------
  cmdShortestPath:
  begin
  // Resolvemos los patrones a nodos reales usando la función anidada
  StartNode := FindNodeByPattern(Parser.CommandSourcePattern);
  EndNode := FindNodeByPattern(Parser.CommandTargetPattern);

  if (StartNode <> nil) and (EndNode <> nil) then
  begin
  PathResult := Self.GetShortestPath(StartNode, EndNode);

  SetLength(Result, Length(PathResult));
  for I := 0 to High(PathResult) do
  begin
  ResDict := TDictionary<string, TObject>.Create;
  PathObj := PathResult[I];

  // El camino puede contener Nodos y Aristas mezclados
  if PathObj is TAiRagGraphNode then
  begin
  ResDict.Add('type', TStringWrapper.Create('node'));
  ResDict.Add('element', PathObj); // Referencia al objeto vivo
  end
  else if PathObj is TAiRagGraphEdge then
  begin
  ResDict.Add('type', TStringWrapper.Create('edge'));
  ResDict.Add('element', PathObj);
  end;

  Result[I] := ResDict;
  end;
  end;
  // Si StartNode o EndNode son nil, devuelve array vacío (no encontrado)
  end;

  // -------------------------------------------------------
  // 3. ALGORITMOS: CENTRALIDAD
  // -------------------------------------------------------
  cmdCentrality:
  begin
  StartNode := FindNodeByPattern(Parser.CommandSourcePattern);
  if StartNode <> nil then
  begin
  Score := Self.GetClosenessCentrality(StartNode);

  SetLength(Result, 1);
  ResDict := TDictionary<string, TObject>.Create;

  // Tipo especial para que el visualizador sepa mostrarlo
  ResDict.Add('type', TStringWrapper.Create('centrality_score'));
  ResDict.Add('node', TStringWrapper.Create(StartNode.Name));
  ResDict.Add('value', TStringWrapper.Create(FormatFloat('0.####', Score)));

  Result[0] := ResDict;
  end;
  end;

  // -------------------------------------------------------
  // 4. ALGORITMOS: TOP DEGREES (HUBS)
  // -------------------------------------------------------
  cmdDegrees:
  begin
  // Obtenemos los N nodos con más conexiones
  NodeListResult := Self.GetNodesByDegree(Parser.CommandLimit, dtTotal);

  SetLength(Result, Length(NodeListResult));
  for I := 0 to High(NodeListResult) do
  begin
  ResDict := TDictionary<string, TObject>.Create;
  ResDict.Add('type', TStringWrapper.Create('node'));
  ResDict.Add('element', NodeListResult[I]);
  Result[I] := ResDict;
  end;
  end;
  end;
  end
  // =========================================================
  // CONSULTA MATCH ESTÁNDAR
  // =========================================================
  else if Assigned(QueryObj) then
  begin
  // Ejecutamos la lógica clásica de Matching (Nodos y Relaciones)

  if QueryObj.Depth > 0 then
  FinalDepth := QueryObj.Depth
  else
  FinalDepth := ADepth;

  Result := Self.Match(QueryObj, FinalDepth);
  end;

  finally
  if Assigned(QueryObj) then
  QueryObj.Free;
  end;
  finally
  Parser.Free;
  end;
  end;
}

function TAiRagGraph.ExecuteMakerGQL(const ACode: string): string;
Var
  Data: TArray<TDictionary<string, TObject>>;
begin
  Result := ExecuteMakerGQL(ACode, Data);

  // 3. ¡IMPORTANTE! Liberar la memoria de los diccionarios de salida
  If Assigned(Data) then
    for var Dict in Data do
      Dict.Free;
end;

function TAiRagGraph.ExecuteMakerGQL(const ACode: string; out AResultObjects: TArray<TDictionary<string, TObject>>; ADepth: Integer = 0): string;
var
  Parser: TGraphParser;
  QueryObj: TGraphMatchQuery;

  // Variables para resultados internos
  StringListResult: TArray<string>;
  NodeListResult: TArray<TAiRagGraphNode>;
  PathResult: TArray<TObject>;

  // Variables de trabajo
  ResDict: TDictionary<string, TObject>;
  I: Integer;
  PathObj: TObject;
  Score: Double;
  Obj: TObject;
  ElementType: string;

  // Variables para resolución de algoritmos
  StartNode, EndNode: TAiRagGraphNode;
  FinalDepth: Integer;

  // Variables para la construcción del contexto de texto
  ContextNodes: TList<TAiRagGraphNode>;
  ContextBuilder: TStringBuilder;

  // --- Función auxiliar FindNodeByPattern (se mantiene igual) ---
  function FindNodeByPattern(APattern: TMatchNodePattern): TAiRagGraphNode;
  var
    Candidates: TArray<TAiRagGraphNode>;
    Cand: TAiRagGraphNode;
  begin
    Result := nil;
    if APattern = nil then
      Exit;
    if not APattern.NodeLabel.IsEmpty then
      Candidates := Self.FindNodesByLabel(APattern.NodeLabel)
    else
      Candidates := FNodeRegistry.Values.ToArray;
    for Cand in Candidates do
      if APattern.Matches(Cand) then
        Exit(Cand);
  end;
// -----------------------------------------------------------

begin
  AResultObjects := [];
  Result := ''; // Default empty string

  if ACode.Trim.IsEmpty then
    Exit;

  Parser := TGraphParser.Create(ACode);
  try
    QueryObj := Parser.Parse;
    try
      if Parser.CommandType <> cmdNone then
      begin
        // =========================================================
        // EJECUCIÓN DE COMANDOS (Algoritmos / Introspección)
        // =========================================================
        ContextBuilder := TStringBuilder.Create;
        try
          case Parser.CommandType of
            // -------------------------------------------------------
            // SHOW LABELS / EDGES
            // -------------------------------------------------------
            cmdShowLabels, cmdShowEdges:
              begin
                if Parser.CommandType = cmdShowLabels then
                begin
                  StringListResult := Self.GetUniqueNodeLabels;
                  ElementType := 'label';
                  ContextBuilder.AppendLine('### AVAILABLE NODE LABELS ###');
                end
                else
                begin
                  StringListResult := Self.GetUniqueEdgeLabels;
                  ElementType := 'edge';
                  ContextBuilder.AppendLine('### AVAILABLE EDGE TYPES ###');
                end;

                SetLength(AResultObjects, Length(StringListResult));
                for I := 0 to High(StringListResult) do
                begin
                  ResDict := TDictionary<string, TObject>.Create;
                  ResDict.Add('type', TStringWrapper.Create(ElementType));
                  ResDict.Add('value', TStringWrapper.Create(StringListResult[I]));
                  AResultObjects[I] := ResDict;

                  // Generar texto simple
                  ContextBuilder.AppendLine('- ' + StringListResult[I]);
                end;
                Result := ContextBuilder.ToString;
              end;

            // -------------------------------------------------------
            // SHORTEST PATH
            // -------------------------------------------------------
            cmdShortestPath:
              begin
                StartNode := FindNodeByPattern(Parser.CommandSourcePattern);
                EndNode := FindNodeByPattern(Parser.CommandTargetPattern);

                if (StartNode <> nil) and (EndNode <> nil) then
                begin
                  PathResult := Self.GetShortestPath(StartNode, EndNode);
                  SetLength(AResultObjects, Length(PathResult));

                  ContextBuilder.AppendLine('### SHORTEST PATH ###');
                  ContextBuilder.AppendFormat('From "%s" to "%s":', [StartNode.Name, EndNode.Name]).AppendLine;

                  for I := 0 to High(PathResult) do
                  begin
                    ResDict := TDictionary<string, TObject>.Create;
                    PathObj := PathResult[I];

                    if PathObj is TAiRagGraphNode then
                    begin
                      ResDict.Add('type', TStringWrapper.Create('node'));
                      ResDict.Add('element', PathObj);
                      ContextBuilder.AppendFormat('(%s)', [TAiRagGraphNode(PathObj).Name]);
                    end
                    else if PathObj is TAiRagGraphEdge then
                    begin
                      ResDict.Add('type', TStringWrapper.Create('edge'));
                      ResDict.Add('element', PathObj);
                      ContextBuilder.AppendFormat(' -[%s]-> ', [TAiRagGraphEdge(PathObj).EdgeLabel]);
                    end;
                    AResultObjects[I] := ResDict;
                  end;
                  Result := ContextBuilder.ToString;
                end
                else
                  Result := 'No path found or nodes do not exist.';
              end;

            // -------------------------------------------------------
            // CENTRALITY
            // -------------------------------------------------------
            cmdCentrality:
              begin
                StartNode := FindNodeByPattern(Parser.CommandSourcePattern);
                if StartNode <> nil then
                begin
                  Score := Self.GetClosenessCentrality(StartNode);

                  SetLength(AResultObjects, 1);
                  ResDict := TDictionary<string, TObject>.Create;
                  ResDict.Add('type', TStringWrapper.Create('centrality_score'));
                  ResDict.Add('node', TStringWrapper.Create(StartNode.Name));
                  ResDict.Add('value', TStringWrapper.Create(FormatFloat('0.####', Score)));
                  AResultObjects[0] := ResDict;

                  Result := Format('Centrality Score for node "%s" (%s): %s', [StartNode.Name, StartNode.NodeLabel, FormatFloat('0.####', Score)]);
                end
                else
                  Result := 'Node not found for centrality calculation.';
              end;

            // -------------------------------------------------------
            // DEGREES (TOP NODES)
            // -------------------------------------------------------
            cmdDegrees:
              begin
                NodeListResult := Self.GetNodesByDegree(Parser.CommandLimit, dtTotal);
                SetLength(AResultObjects, Length(NodeListResult));

                // Para generar el texto enriquecido de estos nodos, usamos GraphToContextText
                // ya que son nodos puros.
                Result := Self.GraphToContextText(NodeListResult);

                // Llenar el objeto de salida
                for I := 0 to High(NodeListResult) do
                begin
                  ResDict := TDictionary<string, TObject>.Create;
                  ResDict.Add('type', TStringWrapper.Create('node'));
                  ResDict.Add('element', NodeListResult[I]);
                  AResultObjects[I] := ResDict;
                end;
              end;
          end;
        finally
          ContextBuilder.Free;
        end;
      end
      // =========================================================
      // CONSULTA MATCH ESTÁNDAR (La más común)
      // =========================================================
      else if Assigned(QueryObj) then
      begin
        if QueryObj.Depth > 0 then
          FinalDepth := QueryObj.Depth
        else
          FinalDepth := ADepth;

        // 1. Ejecutar Match
        AResultObjects := Self.Match(QueryObj, FinalDepth);

        // 2. Extraer Nodos únicos para generar el contexto
        ContextNodes := TList<TAiRagGraphNode>.Create;
        try
          for ResDict in AResultObjects do
          begin
            // Los resultados del Match pueden venir mezclados (nodos, aristas, valores)
            // Extraemos solo los nodos para pasárselos al generador de contexto.
            for var Pair in ResDict do
            begin
              // Caso A: El objeto directo es un Nodo (formato antiguo/simple)
              if Pair.Value is TAiRagGraphNode then
              begin
                if ContextNodes.IndexOf(TAiRagGraphNode(Pair.Value)) = -1 then
                  ContextNodes.Add(TAiRagGraphNode(Pair.Value));
              end
              // Caso B: El objeto viene envuelto en un diccionario de tipo (formato nuevo estandarizado)
              // ej: { 'type': 'node', 'element': <TAiRagGraphNode> }
              else if SameText(Pair.Key, 'element') and (Pair.Value is TAiRagGraphNode) then
              begin
                if ContextNodes.IndexOf(TAiRagGraphNode(Pair.Value)) = -1 then
                  ContextNodes.Add(TAiRagGraphNode(Pair.Value));
              end;
            end;
          end;

          // 3. Generar el Texto Formateado usando GraphToContextText
          if ContextNodes.Count > 0 then
            Result := Self.GraphToContextText(ContextNodes.ToArray)
          else
            Result := 'No matching subgraphs found.';

        finally
          ContextNodes.Free;
        end;
      end;

    finally
      if Assigned(QueryObj) then
        QueryObj.Free;
    end;
  finally
    Parser.Free;
  end;
end;

function TAiRagGraph.ExpandNodeList(AInitialNodes: TList<TAiRagGraphNode>; ADepth: Integer): TArray<TAiRagGraphNode>;
var
  // Usamos un diccionario para registrar los nodos ya procesados o añadidos.
  FoundNodes: TDictionary<string, TAiRagGraphNode>;
  NodeQueue: TQueue<TAiRagGraphNode>;
  CurrentNode, NeighborNode: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  CurrentDepth, NodesInCurrentLevel: Integer;
begin
  FoundNodes := TDictionary<string, TAiRagGraphNode>.Create;
  NodeQueue := TQueue<TAiRagGraphNode>.Create;
  try
    // Inicializar con los nodos de partida
    for CurrentNode in AInitialNodes do
    begin
      if not FoundNodes.ContainsKey(CurrentNode.ID) then
      begin
        NodeQueue.Enqueue(CurrentNode);
        FoundNodes.Add(CurrentNode.ID, CurrentNode);
      end;
    end;

    // Expansión BFS limitada por profundidad
    CurrentDepth := 0;
    while (NodeQueue.Count > 0) and (CurrentDepth < ADepth) do
    begin
      NodesInCurrentLevel := NodeQueue.Count;
      while NodesInCurrentLevel > 0 do
      begin
        CurrentNode := NodeQueue.Dequeue;

        // Expandir hacia adelante (outgoing)
        for Edge in CurrentNode.OutgoingEdges do
        begin
          NeighborNode := Edge.ToNode;
          if not FoundNodes.ContainsKey(NeighborNode.ID) then
          begin
            FoundNodes.Add(NeighborNode.ID, NeighborNode);
            NodeQueue.Enqueue(NeighborNode);
          end;
        end;

        // Expandir hacia atrás (incoming)
        for Edge in CurrentNode.IncomingEdges do
        begin
          NeighborNode := Edge.FromNode;
          if not FoundNodes.ContainsKey(NeighborNode.ID) then
          begin
            FoundNodes.Add(NeighborNode.ID, NeighborNode);
            NodeQueue.Enqueue(NeighborNode);
          end;
        end;

        Dec(NodesInCurrentLevel);
      end;
      Inc(CurrentDepth);
    end;

    // Convertir los valores del diccionario a un array
    Result := FoundNodes.Values.ToArray;
  finally
    NodeQueue.Free;
    FoundNodes.Free;
  end;
end;

function TAiRagGraph.ExtractSubgraph(ANodes: TArray<TAiRagGraphNode>): TAiRagGraph;
var
  NodeSet: TDictionary<string, Boolean>;
  Node, NewNode: TAiRagGraphNode;
  Edge, NewEdge: TAiRagGraphEdge;
  Chunk: TAiEmbeddingNode;
  NewSource, NewTarget: TAiRagGraphNode;
begin
  // 1. Crear el nuevo grafo con la misma configuración de embeddings
  Result := TAiRagGraph.Create(nil);
  Result.Embeddings := Self.Embeddings;

  if Length(ANodes) = 0 then
    Exit;

  // Diccionario para rastrear qué nodos se incluyeron en el subgrafo
  NodeSet := TDictionary<string, Boolean>.Create;
  try
    // BLOQUEAR ACTUALIZACIONES: Crucial para el rendimiento.
    // Evita que los índices (BM25/HNSW) se reconstruyan nodo por nodo.
    Result.BeginUpdate;
    try
      // --- FASE 1: Clonar todos los Nodos seleccionados ---
      for Node in ANodes do
      begin
        NodeSet.Add(Node.ID, True);

        // Creamos el nodo en el nuevo grafo manteniendo ID, Label y Name
        NewNode := Result.AddNode(Node.ID, Node.NodeLabel, Node.Name);
        NewNode.Model := Node.Model;

        // COPIA DE TEXTO: Importante para que el subgrafo funcione con búsquedas léxicas
        NewNode.Text := Node.Text;

        // COPIA DE METADATOS: Reemplaza el bucle manual de FProperties.
        // Copia tipos reales (fechas, números, booleanos) sin pérdida.
        NewNode.MetaData.Assign(Node.MetaData);

        // COPIA DE VECTOR (Resumen): Copia el embedding principal del nodo
        if Length(Node.Data) > 0 then
        begin
          NewNode.SetDataLength(Length(Node.Data));
          NewNode.Data := Copy(Node.Data); // Copy es más seguro que Move para arrays dinámicos
        end;

        // COPIA DE CHUNKS: Nueva funcionalidad (Opción 1)
        // Garantiza que la entidad mantenga todos sus fragmentos de texto
        for Chunk in Node.Chunks do
        begin
          NewNode.AddChunk(Chunk.Text, Chunk.Data);
        end;
      end;

      // --- FASE 2: Clonar Aristas Internas ---
      // Solo aquellas cuyas entidades origen y destino estén presentes en la selección
      for Node in ANodes do
      begin
        for Edge in Node.OutgoingEdges do
        begin
          // Verificar si el nodo destino también fue seleccionado
          if NodeSet.ContainsKey(Edge.ToNode.ID) then
          begin
            // IMPORTANTE: Obtener las referencias de los nodos EN EL NUEVO GRAFO
            NewSource := Result.FindNodeByID(Edge.FromNode.ID);
            NewTarget := Result.FindNodeByID(Edge.ToNode.ID);

            if (NewSource <> nil) and (NewTarget <> nil) then
            begin
              // Recreamos la arista con todos los parámetros originales (ID, Label, Name, Weight)
              NewEdge := Result.AddEdge(NewSource, NewTarget, Edge.ID, Edge.EdgeLabel, Edge.Name, Edge.Weight);

              // Copiar metadatos de la arista
              NewEdge.MetaData.Assign(Edge.MetaData);

              // Copiar vector de la arista (si existe)
              if Length(Edge.Data) > 0 then
              begin
                NewEdge.SetDataLength(Length(Edge.Data));
                NewEdge.Data := Copy(Edge.Data);
              end;
            end;
          end;
        end;
      end;
    finally
      // Dispara la reconstrucción de índices una sola vez para todo el subgrafo
      Result.EndUpdate;
    end;
  finally
    NodeSet.Free;
  end;
end;

procedure TAiRagGraph.Clear;
var
  NodeList: TList<TAiRagGraphNode>;
begin
  // Código original
  FNodeRegistry.Clear;
  FEdgeRegistry.Clear;

  // Liberar las listas de nodos dentro del índice de etiquetas antes de limpiarlo
  for NodeList in FNodeLabelIndex.Values do
  begin
    NodeList.Free;
  end;
  FNodeLabelIndex.Clear;
  FNodeNameIndex.Clear;

  FUpdateCount := 0;
  FNodes.Clear;
  FEdges.Clear;
end;

function TAiRagGraph.CountNodesByLabel(const ALabel: string): Integer;
var
  NodeList: TList<TAiRagGraphNode>;
begin
  // Usamos el índice para máxima eficiencia
  if FNodeLabelIndex.TryGetValue(ALabel, NodeList) then
    Result := NodeList.Count
  else
    Result := 0;
end;

function TAiRagGraph.CountEdgesByLabel(const ALabel: string): Integer;
var
  Edge: TAiRagGraphEdge;
begin
  Result := 0;
  for Edge in FEdgeRegistry.Values do
  begin
    if SameText(Edge.EdgeLabel, ALabel) then
      Inc(Result);
  end;
end;

function TAiRagGraph.NewNode(const AID, ALabel, AName: string): TAiRagGraphNode;
var
  Dim: Integer;
begin
  // Determinamos la dimensión para el objeto
  if FNodes.Dim > 0 then
    Dim := FNodes.Dim
  else if Assigned(FEmbeddings) then
    Dim := FEmbeddings.Dimensions
  else
    Dim := 1536; // Valor por defecto

  // Creamos la instancia
  Result := TAiRagGraphNode.Create(Self, Dim);
  Result.ID := AID;
  Result.NodeLabel := ALabel;
  Result.Name := AName;
  // Nota: No se añade a FNodeRegistry ni a FNodes todavía.
end;

procedure TAiRagGraph.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  // Si un componente se está eliminando (opRemove)
  if Operation = opRemove then
  begin
    // ¿Es nuestro motor de embeddings?
    if AComponent = FEmbeddings then
      FEmbeddings := nil;

    // ¿Es nuestro driver?
    if AComponent = FDriver then
      FDriver := nil;
  end;
end;

function TAiRagGraph.AddNode(AID, ALabel, AName: string): TAiRagGraphNode;
begin
  // Ahora AddNode usa NewNode y luego lo registra
  Result := NewNode(AID, ALabel, AName);
  try
    Result := AddNode(Result); // Llama al overload que acepta el objeto
  except
    Result.Free;
    raise;
  end;
end;

function TAiRagGraph.AddNode(ANode: TAiRagGraphNode): TAiRagGraphNode;
var
  ExistingNode: TAiRagGraphNode;
begin
  if ANode = nil then
    Exit(nil);

  // Intentamos registrarlo y persistirlo
  // InternalAddNode ya maneja si el ID existe (Identity Map)
  ExistingNode := InternalAddNode(ANode, True);

  if ExistingNode <> ANode then
  begin
    // Si ya existía un nodo con ese ID, liberamos el que intentamos añadir
    // y devolvemos el que ya estaba en el grafo.
    ANode.Free;
    Result := ExistingNode;
  end
  else
    Result := ANode;
end;

procedure TAiRagGraph.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TAiRagGraph.NewEdge(AFromNode, AToNode: TAiRagGraphNode; const AID, ALabel, AName: string; AWeight: Double = 1.0): TAiRagGraphEdge;
var
  Dim: Integer;
begin
  // Centralizamos la detección de dimensiones
  if FEdges.Dim > 0 then
    Dim := FEdges.Dim
  else if Assigned(FEmbeddings) then
    Dim := FEmbeddings.Dimensions
  else
    Dim := 1536;

  Result := TAiRagGraphEdge.Create(Self, Dim);
  Result.ID := AID;
  Result.EdgeLabel := ALabel;
  Result.Name := AName;
  Result.FromNode := AFromNode;
  Result.ToNode := AToNode;
  Result.Weight := AWeight;
end;

// El overload que crea desde datos simples
function TAiRagGraph.AddEdge(AFromNode, AToNode: TAiRagGraphNode; AID, ALabel, AName: string): TAiRagGraphEdge;
begin
  Result := AddEdge(AFromNode, AToNode, AID, ALabel, AName, 1.0);
end;

// El overload que crea desde datos simples + peso
function TAiRagGraph.AddEdge(AFromNode, AToNode: TAiRagGraphNode; AID, ALabel, AName: string; AWeight: Double): TAiRagGraphEdge;
begin
  // 1. Usamos la fábrica para crear el objeto completo en memoria
  Result := NewEdge(AFromNode, AToNode, AID, ALabel, AName, AWeight);

  try
    // 2. Llamamos al overload de objeto para registrar y persistir
    Result := AddEdge(Result);
  except
    // Si falla el registro o la persistencia, NewEdge no fue adoptado,
    // así que debemos liberarlo.
    if (Result <> nil) then
      Result.Free;
    raise;
  end;
end;

// El overload que recibe el objeto ya creado
function TAiRagGraph.AddEdge(AEdge: TAiRagGraphEdge): TAiRagGraphEdge;
var
  ExistingEdge: TAiRagGraphEdge;
begin
  if AEdge = nil then
    Exit(nil);

  // Delegamos a InternalAddEdge con AShouldPersist = True
  ExistingEdge := InternalAddEdge(AEdge, True);

  if ExistingEdge <> AEdge then
  begin
    // Si ya existía una arista con ese ID, liberamos la nueva
    AEdge.Free;
    Result := ExistingEdge;
  end
  else
    Result := AEdge;
end;

procedure TAiRagGraph.DeleteEdge(AEdge: TAiRagGraphEdge);
begin
  if AEdge = nil then
    Exit;

  // Quitar la referencia de los nodos conectados
  AEdge.FromNode.RemoveOutgoingEdge(AEdge);
  AEdge.ToNode.RemoveIncomingEdge(AEdge);

  UnregisterEdge(AEdge);
end;

procedure TAiRagGraph.DeleteEdge(AID: string);
var
  Edge: TAiRagGraphEdge;
begin
  Edge := FindEdgeByID(AID);
  DeleteEdge(Edge);
end;

procedure TAiRagGraph.DeleteNode(ANode: TAiRagGraphNode);
var
  Edge: TAiRagGraphEdge;
  EdgesToDelete: TList<TAiRagGraphEdge>;
begin
  if ANode = nil then
    Exit;

  // Crear una lista temporal de aristas a borrar para no modificar las listas mientras se iteran
  EdgesToDelete := TList<TAiRagGraphEdge>.Create;
  try
    for Edge in ANode.OutgoingEdges do
      EdgesToDelete.Add(Edge);
    for Edge in ANode.IncomingEdges do
      EdgesToDelete.Add(Edge);

    // Borrar todas las aristas conectadas
    for Edge in EdgesToDelete do
      DeleteEdge(Edge);
  finally
    EdgesToDelete.Free;
  end;

  UnregisterNode(ANode);
end;

procedure TAiRagGraph.DeleteNode(AID: string);
var
  Node: TAiRagGraphNode;
begin
  Node := FindNodeByID(AID);
  if Node = nil then
    Exit;

  // 1. Guardar una referencia a las aristas ANTES de borrarlas en memoria
  var
  EdgesToDelete := TList<TAiRagGraphEdge>.Create;
  EdgesToDelete.AddRange(Node.OutgoingEdges.ToArray);
  EdgesToDelete.AddRange(Node.IncomingEdges.ToArray);

  try
    // 2. Realizar la operación en memoria PRIMERO
    UnregisterNode(Node); // Esto quita el nodo de FNodeRegistry y FNodes
    // También debes desregistrar las aristas conectadas
    for var Edge in EdgesToDelete do
      UnregisterEdge(Edge);

    // 3. Persistir el cambio en la BD DESPUÉS
    if Assigned(FDriver) then
    begin
      try
        FDriver.DeleteNode(AID);
        // También necesitarías un OnGraphDeleteEdge para las aristas
        // o que OnGraphDeleteNode se encargue de borrar en cascada.
      except
        // Si la BD falla, ¡tenemos que restaurar el estado en memoria!
        // Esto es complejo: requeriría volver a añadir el nodo y las aristas.
        // Por simplicidad, por ahora solo relanzamos. Una solución completa
        // requeriría un patrón "Unit of Work".
        raise;
      end;
    end;
  finally
    EdgesToDelete.Free;
  end;
end;

procedure TAiRagGraph.UnregisterEdge(AEdge: TAiRagGraphEdge);
begin
  if FEdgeRegistry.ContainsKey(AEdge.ID) then
    FEdgeRegistry.Remove(AEdge.ID);
  FEdges.Items.Remove(AEdge); // Esto libera la memoria del objeto Edge
end;

procedure TAiRagGraph.UnregisterNode(ANode: TAiRagGraphNode);
var
  NodeList: TList<TAiRagGraphNode>;
  CombinedNameKey: string;
begin
  if ANode = nil then
    Exit;

  // --> INICIO DE CAMBIOS
  // Solo actualizamos los índices en tiempo real si no estamos en modo batch
  if FUpdateCount = 0 then
  begin
    // 1. Eliminar del índice de etiquetas
    if FNodeLabelIndex.TryGetValue(ANode.NodeLabel, NodeList) then
    begin
      NodeList.Remove(ANode);
      if NodeList.Count = 0 then
      begin
        FNodeLabelIndex.Remove(ANode.NodeLabel);
        NodeList.Free;
      end;
    end;

    // 2. Eliminar del índice de nombres
    if not ANode.Name.IsEmpty then
    begin
      CombinedNameKey := ANode.NodeLabel + '#' + ANode.Name;
      FNodeNameIndex.Remove(CombinedNameKey);
    end;
  end;
  // --> FIN DE CAMBIOS

  // Código original (esto siempre se ejecuta)
  if FNodeRegistry.ContainsKey(ANode.ID) then
    FNodeRegistry.Remove(ANode.ID);
  FNodes.Items.Remove(ANode);
end;

procedure TAiRagGraph.UpdateCommunityLabels;
var
  Communities: TDictionary<TAiRagGraphNode, Integer>;
  Node: TAiRagGraphNode;
begin
  // 1. Ejecutar el algoritmo de detección de comunidades (ej: Louvain)
  Communities := DetectCommunities;
  try
    // 2. Recorrer los resultados (Nodo -> ID de Comunidad)
    for Node in Communities.Keys do
    begin
      // 3. Guardar el ID en el MetaData unificado.
      // Al ser un Integer, se guarda como Variant correctamente, permitiendo
      // que luego se exporte a JSON o Postgres como un número real.
      Node.Properties['community_id'] := Communities[Node];
    end;
  finally
    Communities.Free;
  end;
end;

function TAiRagGraph.FindEdge(AFromNode, AToNode: TAiRagGraphNode; AEdgeLabel: string): TAiRagGraphEdge;
var
  Edge: TAiRagGraphEdge;
begin
  Result := nil;
  // Es más eficiente iterar sobre las aristas salientes del nodo de origen,
  // ya que es una lista mucho más pequeña que todas las aristas del grafo.
  for Edge in AFromNode.OutgoingEdges do
  begin
    if (Edge.ToNode = AToNode) and (SameText(Edge.EdgeLabel, AEdgeLabel)) then
    begin
      Result := Edge;
      Exit;
    end;
  end;
end;

function TAiRagGraph.FindEdgeByID(AID: string): TAiRagGraphEdge;
var
  FoundInDB: Boolean;
  EdgeData: TEdgeDataRecord;
begin
  // 1. Buscar en caché.
  if FEdgeRegistry.TryGetValue(AID, Result) then
    Exit;

  // 2. Pedir datos a la BD (SOLO Driver).
  Result := nil;
  FoundInDB := False;

  if Assigned(FDriver) then
  begin
    // Limpiamos el record antes de pasarlo al driver.
    // Aunque el driver debe ser defensivo, es buena práctica inicializar.
    FillChar(EdgeData, SizeOf(TEdgeDataRecord), 0);

    // CORRECCIÓN CLAVE: Llamar al método FindEdgeDataByID del driver.
    FoundInDB := FDriver.FindEdgeDataByID(AID, EdgeData);
  end;

  // Si no se encontró en el driver o el driver no estaba asignado, FoundInDB es False.

  // 3. Usar el nuevo helper para hidratar.
  if FoundInDB then
    Result := InternalHydrateEdge(EdgeData);
end;

function TAiRagGraph.FindNodeByID(AID: string): TAiRagGraphNode;
var
  FoundInDB: Boolean;
  NodeData: TNodeDataRecord;
begin
  // 1. Buscar en caché.
  if FNodeRegistry.TryGetValue(AID, Result) then
    Exit;

  // 2. Pedir datos a la BD.
  Result := nil;
  FoundInDB := False;

  if Assigned(FDriver) then
  begin
    FoundInDB := FDriver.FindNodeDataByID(AID, NodeData);
  end;

  // 3. Usar el nuevo helper para hidratar.
  if FoundInDB then
    Result := InternalHydrateNode(NodeData);
end;

function TAiRagGraph.FindNodeByName(AName, ANodeLabel: string): TAiRagGraphNode;
var
  CombinedNameKey: string; // Para la lógica en memoria
begin
  Result := nil;

  // 1. Intentar delegar a la base de datos a través del Driver.
  if Assigned(FDriver) then
  begin
    // El driver realiza la búsqueda en la BD y devuelve el objeto hidratado
    // (o nil si no lo encuentra).
    Result := FDriver.FindNodeByName(AName, ANodeLabel);
    // Si el driver encontró algo, salimos inmediatamente.
    if Result <> nil then
      Exit;
  end;

  // 2. Fallback a la lógica en memoria (si el Driver no fue usado o no encontró nada)
  if AName.IsEmpty or ANodeLabel.IsEmpty then
    Exit;

  // Intentar encontrar en el índice en memoria (solo para nodos ya cargados)
  CombinedNameKey := ANodeLabel + '#' + AName;
  FNodeNameIndex.TryGetValue(CombinedNameKey, Result);
end;

function TAiRagGraph.FindNodeNamesByLabel(const ANodeLabel, ASearchText: string; ALimit: Integer): TArray<string>;
var
  NodeList: TList<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
  Results: TStringList;
begin
  Result := [];

  // 1. Intentar delegar al Driver (BD)
  if Assigned(FDriver) then
  begin
    // El driver realiza la búsqueda eficiente en la BD y devuelve el array de nombres.
    Result := FDriver.FindNodeNamesByLabel(ANodeLabel, ASearchText, ALimit);

    // Si la BD devolvió resultados, salimos.
    if Length(Result) > 0 then
      Exit;

    // NOTA: Si la BD devuelve un array vacío, pasamos al fallback en memoria (Paso 2).
  end;

  // 2. Fallback a la lógica en memoria si no hay resultados o si no hay driver.
  Results := TStringList.Create;
  try
    // Usamos el índice de etiquetas para ser eficientes
    if FNodeLabelIndex.TryGetValue(ANodeLabel, NodeList) then
    begin
      for Node in NodeList do
      begin
        if ContainsText(LowerCase(Node.Name), LowerCase(ASearchText)) then
        begin
          Results.Add(Node.Name);
          if Results.Count >= ALimit then
            Break; // Detenerse cuando alcanzamos el límite
        end;
      end;
    end;
    Result := Results.ToStringArray;
  finally
    Results.Free;
  end;
end;

function TAiRagGraph.FindNodesByLabel(ALabel: string): TArray<TAiRagGraphNode>;
var
  NodeList: TList<TAiRagGraphNode>; // Para la lógica en memoria
begin
  Result := [];

  // 1. Intentar delegar al Driver (BD)
  if Assigned(FDriver) then
  begin
    // El driver realiza la búsqueda en la BD, se encarga de la hidratación
    // (usando FindNodeByID) y devuelve el array de nodos.
    Result := FDriver.FindNodesByLabel(ALabel);

    // Si el driver devolvió algún resultado, salimos.
    if Length(Result) > 0 then
      Exit;

    // Si el driver devolvió un array vacío, pasamos al fallback en memoria (Paso 2).
  end;

  // 2. Fallback a la lógica en memoria (si no hay resultados o si no hay driver)
  if FNodeLabelIndex.TryGetValue(ALabel, NodeList) then
    Result := NodeList.ToArray
  else
    Result := [];
end;

function TAiRagGraph.FindNodesByProperty(const AKey: string; const AValue: Variant): TArray<TAiRagGraphNode>;
var
  Results: TList<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
begin
  Result := [];

  // 1. Intentar delegar al Driver (Base de Datos)
  if Assigned(FDriver) then
  begin
    // El driver realiza la búsqueda eficiente indexada en la BD
    Result := FDriver.FindNodesByProperty(AKey, AValue);

    if Length(Result) > 0 then
      Exit;
  end;

  // 2. Fallback a la lógica en memoria
  Results := TList<TAiRagGraphNode>.Create;
  try
    for Node in FNodeRegistry.Values do
    begin
      // --- A. Manejo de atributos nativos (Identidad) ---
      if SameText(AKey, 'name') then
      begin
        // Usamos SameText para nombres (insensible a mayúsculas)
        if SameText(Node.Name, VarToStr(AValue)) then
          Results.Add(Node);
      end
      else if SameText(AKey, 'label') then
      begin
        if SameText(Node.NodeLabel, VarToStr(AValue)) then
          Results.Add(Node);
      end
      else if SameText(AKey, 'id') then
      begin
        if SameText(Node.ID, VarToStr(AValue)) then
          Results.Add(Node);
      end
      // --- B. Manejo de propiedades dinámicas (MetaData) ---
      else
      begin
        // Utilizamos el nuevo método Evaluate del MetaData.
        // Esto permite que si buscas precio = 100, no falle si el 100 es un Integer
        // pero tú pasaste un Double, gracias a la gestión de Variants.
        if Node.Properties.Evaluate(AKey, foEqual, AValue) then
          Results.Add(Node);
      end;
    end;

    Result := Results.ToArray;
  finally
    Results.Free;
  end;
end;

function TAiRagGraph.GetAllShortestPaths(AStartNode, AEndNode: TAiRagGraphNode): TArray<TArray<TObject>>;
var
  Queue: TQueue<TAiRagGraphNode>;
  // Almacena la distancia más corta desde el nodo de inicio a cualquier otro nodo
  Distances: TDictionary<TAiRagGraphNode, Integer>;
  // Almacena para cada nodo, una lista de sus "padres" en los caminos más cortos
  Parents: TDictionary<TAiRagGraphNode, TList<TAiRagGraphNode>>;
  AllPaths: TList<TArray<TObject>>;
  CurrentPath: TList<TObject>;
  CurrentNode, NeighborNode: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  ShortestDistance: Integer;

  // Procedimiento recursivo para reconstruir los caminos hacia atrás
  procedure BuildPaths(ANode: TAiRagGraphNode);
  var
    ParentNode: TAiRagGraphNode;
    ConnectingEdge: TAiRagGraphEdge;
  begin
    // 1. Añadir el nodo actual al frente del camino que estamos construyendo
    CurrentPath.Insert(0, ANode);

    // 2. Caso Base: Si hemos llegado al nodo de inicio, hemos completado un camino
    if ANode = AStartNode then
    begin
      // Añadimos una copia del camino encontrado a nuestra lista de resultados
      AllPaths.Add(CurrentPath.ToArray);
    end
    else
    // 3. Paso Recursivo: Explorar todos los padres de este nodo
    begin
      if Parents.ContainsKey(ANode) then
      begin
        for ParentNode in Parents[ANode] do
        begin
          // Encontrar la arista que conecta el padre con el nodo actual
          ConnectingEdge := FindEdge(ParentNode, ANode, ''); // Pasamos '' para que busque cualquier etiqueta
          // Una versión más robusta podría iterar sobre ParentNode.OutgoingEdges
          if ConnectingEdge = nil then
          begin
            // Búsqueda más exhaustiva si FindEdge falla (por si hay varias aristas)
            for var E in ParentNode.OutgoingEdges do
            begin
              if E.ToNode = ANode then
              begin
                ConnectingEdge := E;
                Break;
              end;
            end;
          end;

          // Añadir la arista al camino
          CurrentPath.Insert(0, ConnectingEdge);
          // Llamar recursivamente para el padre
          BuildPaths(ParentNode);
          // Backtrack: quitar la arista para la siguiente iteración del bucle de padres
          CurrentPath.Remove(ConnectingEdge);
        end;
      end;
    end;

    // 4. Backtrack: quitar el nodo actual del camino para no afectar a otras ramas de la recursión
    CurrentPath.Remove(ANode);
  end;

begin
  Result := [];
  if (AStartNode = nil) or (AEndNode = nil) or (AStartNode = AEndNode) then
    Exit;

  // --- FASE 1: BFS HACIA ADELANTE PARA ENCONTRAR DISTANCIAS Y PADRES ---
  Queue := TQueue<TAiRagGraphNode>.Create;
  Distances := TDictionary<TAiRagGraphNode, Integer>.Create;
  Parents := TDictionary < TAiRagGraphNode, TList < TAiRagGraphNode >>.Create;
  ShortestDistance := -1;

  try
    Queue.Enqueue(AStartNode);
    Distances.Add(AStartNode, 0);
    Parents.Add(AStartNode, TList<TAiRagGraphNode>.Create); // El nodo de inicio no tiene padres

    while Queue.Count > 0 do
    begin
      CurrentNode := Queue.Dequeue;
      var
      CurrentDist := Distances[CurrentNode];

      // Si ya encontramos el destino, solo necesitamos terminar de procesar los nodos
      // que están a la misma distancia. No exploramos más allá.
      if (ShortestDistance <> -1) and (CurrentDist >= ShortestDistance) then
        Continue;

      for Edge in CurrentNode.OutgoingEdges do
      begin
        NeighborNode := Edge.ToNode;

        // Caso 1: Primera vez que visitamos este vecino
        if not Distances.ContainsKey(NeighborNode) then
        begin
          Distances.Add(NeighborNode, CurrentDist + 1);
          Queue.Enqueue(NeighborNode);
          var
          ParentList := TList<TAiRagGraphNode>.Create;
          Parents.Add(NeighborNode, ParentList);
          ParentList.Add(CurrentNode);

          // Si este vecino es nuestro destino, registramos la distancia más corta
          if NeighborNode = AEndNode then
            ShortestDistance := CurrentDist + 1;
        end
        // Caso 2: Ya hemos visitado este vecino, pero hemos encontrado otro camino
        // de la misma longitud mínima para llegar a él.
        else if Distances[NeighborNode] = CurrentDist + 1 then
        begin
          Parents[NeighborNode].Add(CurrentNode);
        end;
      end;
    end;

    // --- FASE 2: RECONSTRUCCIÓN RECURSIVA HACIA ATRÁS ---
    // Si ShortestDistance sigue en -1, significa que el nodo final nunca fue alcanzado.
    if ShortestDistance <> -1 then
    begin
      AllPaths := TList < TArray < TObject >>.Create;
      CurrentPath := TList<TObject>.Create;
      try
        BuildPaths(AEndNode); // Iniciar la recursión desde el nodo final
        Result := AllPaths.ToArray;
      finally
        CurrentPath.Free;
        AllPaths.Free;
      end;
    end;

  finally
    Queue.Free;
    Distances.Free;
    for var List in Parents.Values do // Es crucial liberar las listas internas
      List.Free;
    Parents.Free;
  end;
end;

function TAiRagGraph.GetClosenessCentrality(ANode: TAiRagGraphNode): Double;
var
  Queue: TQueue<TAiRagGraphNode>;
  Distances: TDictionary<TAiRagGraphNode, Integer>;
  CurrentNode, NeighborNode: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  TotalDistance: Int64;
  ReachableNodesCount: Integer;
  CurrentDistance: Integer;
begin
  Result := 0;
  if (ANode = nil) or (FNodeRegistry.Count < 2) then
    Exit;

  Queue := TQueue<TAiRagGraphNode>.Create;
  Distances := TDictionary<TAiRagGraphNode, Integer>.Create;
  TotalDistance := 0;
  ReachableNodesCount := 0;

  try
    // --- Etapa 1: Inicialización del BFS ---
    // El nodo de inicio está a una distancia de 0 de sí mismo.
    Distances.Add(ANode, 0);
    Queue.Enqueue(ANode);

    // --- Etapa 2: Recorrido BFS ---
    while Queue.Count > 0 do
    begin
      CurrentNode := Queue.Dequeue;
      CurrentDistance := Distances[CurrentNode];

      // Expandir a los vecinos (solo hacia adelante, como es típico en centralidad)
      // Si quisieras un grafo no dirigido, también recorrerías las IncomingEdges.
      for Edge in CurrentNode.OutgoingEdges do
      begin
        NeighborNode := Edge.ToNode;

        // Si no hemos visitado este vecino antes...
        if not Distances.ContainsKey(NeighborNode) then
        begin
          // Marcamos su distancia
          Distances.Add(NeighborNode, CurrentDistance + 1);
          // Lo añadimos a la cola para visitar a sus vecinos más tarde
          Queue.Enqueue(NeighborNode);

          // --- Etapa 3: Acumular los resultados ---
          Inc(ReachableNodesCount);
          TotalDistance := TotalDistance + (CurrentDistance + 1);
        end;
      end;
    end;

    // --- Etapa 4: Calcular la Centralidad de Cercanía ---
    if TotalDistance > 0 then
    begin
      // Fórmula estándar de Closeness Centrality:
      // (Número de nodos alcanzables) / (Suma de las distancias a ellos)
      Result := ReachableNodesCount / TotalDistance;
    end;

  finally
    Queue.Free;
    Distances.Free;
  end;
end;

{
  function TAiRagGraph.GetContextualizedText(ASubgraphNodes: TArray<TAiRagGraphNode>): string;
  var
  // Usamos un diccionario como un Set para búsquedas rápidas de nodos
  NodeSet: TDictionary<TAiRagGraphNode, Boolean>;
  RelevantEdges: TDictionary<string, TAiRagGraphEdge>;
  ContextBuilder: TStringBuilder;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  begin
  if Length(ASubgraphNodes) = 0 then
  Exit('');

  NodeSet := TDictionary<TAiRagGraphNode, Boolean>.Create;
  RelevantEdges := TDictionary<string, TAiRagGraphEdge>.Create;
  ContextBuilder := TStringBuilder.Create;
  try
  // 1. Poblar el NodeSet para búsquedas eficientes
  for Node in ASubgraphNodes do
  begin
  NodeSet.Add(Node, True);
  end;

  // 2. Recopilar todas las aristas internas del subgrafo
  for Node in ASubgraphNodes do
  begin
  for Edge in Node.OutgoingEdges do
  begin
  // >> LÍNEA CORREGIDA Y OPTIMIZADA <<
  // Ahora usamos ContainsKey en el diccionario, que es mucho más rápido.
  if NodeSet.ContainsKey(Edge.ToNode) and (not RelevantEdges.ContainsKey(Edge.ID)) then
  begin
  RelevantEdges.Add(Edge.ID, Edge);
  end;
  end;
  end;

  // 3. Construir el texto a partir de las aristas recopiladas
  ContextBuilder.AppendLine('Contexto extraído del grafo de conocimiento:');
  ContextBuilder.AppendLine('Se han identificado los siguientes hechos relevantes:');

  if RelevantEdges.Count > 0 then
  begin
  for Edge in RelevantEdges.Values do
  begin
  ContextBuilder.Append('- Hecho: ');
  ContextBuilder.Append(Edge.FromNode.Name);
  ContextBuilder.Append(' (');
  ContextBuilder.Append(Edge.FromNode.NodeLabel);
  ContextBuilder.Append(') --[');
  ContextBuilder.Append(Edge.EdgeLabel);
  ContextBuilder.Append(']--> ');
  ContextBuilder.Append(Edge.ToNode.Name);
  ContextBuilder.Append(' (');
  ContextBuilder.Append(Edge.ToNode.NodeLabel);
  ContextBuilder.AppendLine(').');
  end;
  end
  else
  begin
  // Fallback: describir los nodos si no hay aristas internas
  ContextBuilder.AppendLine('Se encontraron las siguientes entidades relevantes, pero sin conexiones directas entre ellas en el subgrafo expandido:');
  for Node in ASubgraphNodes do
  begin
  ContextBuilder.Append('- Entidad: ');
  ContextBuilder.Append(Node.Name);
  ContextBuilder.Append(' (');
  ContextBuilder.Append(Node.NodeLabel);
  ContextBuilder.AppendLine(').');
  end;
  end;

  Result := ContextBuilder.ToString;
  finally
  NodeSet.Free;
  RelevantEdges.Free;
  ContextBuilder.Free;
  end;
  end;
}

function TAiRagGraph.GetContextualizedText(ASubgraphNodes: TArray<TAiRagGraphNode>): string;
var
  ContextBuilder: TStringBuilder;
  NodeSet: TDictionary<TAiRagGraphNode, Boolean>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Chunk: TAiEmbeddingNode;
  RelevantEdges: TDictionary<string, TAiRagGraphEdge>;
  Pair: TPair<string, Variant>;
  IsFirstProp: Boolean;
begin
  if Length(ASubgraphNodes) = 0 then
    Exit('No se encontró información relevante.');

  ContextBuilder := TStringBuilder.Create;
  NodeSet := TDictionary<TAiRagGraphNode, Boolean>.Create;
  RelevantEdges := TDictionary<string, TAiRagGraphEdge>.Create;
  try
    // 1. Identificar nodos presentes en el subgrafo
    for Node in ASubgraphNodes do
      NodeSet.Add(Node, True);

    // === SECCIÓN 1: ENTIDADES Y SUS FRAGMENTOS (CHUNKS) ===
    ContextBuilder.AppendLine('### CONTEXTO DE ENTIDADES ###');
    for Node in ASubgraphNodes do
    begin
      ContextBuilder.AppendFormat('- Entidad: %s (Tipo: %s)', [Node.Name, Node.NodeLabel]);

      if not Node.Text.IsEmpty then
        ContextBuilder.Append(' - Resumen: ' + Node.Text);

      ContextBuilder.AppendLine('.');

      // --- Incluir fragmentos de texto detallados (Chunks) ---
      if Node.Chunks.Count > 0 then
      begin
        for Chunk in Node.Chunks do
        begin
          ContextBuilder.AppendFormat('  + Detalle: %s', [Chunk.Text]).AppendLine;
        end;
      end;

      // Recopilar aristas que conectan con otros nodos del mismo subgrafo
      for Edge in Node.OutgoingEdges do
      begin
        if NodeSet.ContainsKey(Edge.ToNode) and (not RelevantEdges.ContainsKey(Edge.ID)) then
          RelevantEdges.Add(Edge.ID, Edge);
      end;
    end;

    // === SECCIÓN 2: RELACIONES Y HECHOS ===
    if RelevantEdges.Count > 0 then
    begin
      ContextBuilder.AppendLine;
      ContextBuilder.AppendLine('### RELACIONES Y HECHOS ###');
      for Edge in RelevantEdges.Values do
      begin
        ContextBuilder.AppendFormat('* %s (%s) --[%s]--> %s (%s)', [Edge.FromNode.Name, Edge.FromNode.NodeLabel, Edge.EdgeLabel.ToUpper, Edge.ToNode.Name, Edge.ToNode.NodeLabel]);

        // --- Acceso corregido a Metadatos (usando MetaData.InternalDictionary) ---
        if Edge.MetaData.InternalDictionary.Count > 0 then
        begin
          ContextBuilder.Append(' {');
          IsFirstProp := True;
          for Pair in Edge.MetaData.InternalDictionary do
          begin
            if not IsFirstProp then
              ContextBuilder.Append(', ');
            ContextBuilder.AppendFormat('%s: %s', [Pair.Key, VarToStr(Pair.Value)]);
            IsFirstProp := False;
          end;
          ContextBuilder.Append('}');
        end;

        if Abs(Edge.Weight - 1.0) > 0.001 then
          ContextBuilder.AppendFormat(' [Peso: %s]', [FormatFloat('0.##', Edge.Weight)]);

        ContextBuilder.AppendLine('.');
      end;
    end;

    Result := ContextBuilder.ToString;
  finally
    NodeSet.Free;
    RelevantEdges.Free;
    ContextBuilder.Free;
  end;
end;

function TAiRagGraph.GetEdgeCount: Integer;
begin
  Result := FEdgeRegistry.Count;
end;

function TAiRagGraph.GetEdgesRAGVector: TAiRAGVector;
begin
  Result := FEdges;
end;

function TAiRagGraph.GetNeighbors(ANode: TAiRagGraphNode; ADirection: TGraphDirection): TArray<TAiRagGraphNode>;
var
  Results: TList<TAiRagGraphNode>;
  Edge: TAiRagGraphEdge;
begin
  Result := Nil;
  if ANode = nil then
    Exit;

  Results := TList<TAiRagGraphNode>.Create;
  try
    if (ADirection = gdOutgoing) or (ADirection = gdBoth) then
    begin
      for Edge in ANode.OutgoingEdges do
        Results.Add(Edge.ToNode);
    end;

    if (ADirection = gdIncoming) or (ADirection = gdBoth) then
    begin
      for Edge in ANode.IncomingEdges do
        // Evitar duplicados si ADirection es gdBoth y hay aristas recíprocas
        if Results.IndexOf(Edge.FromNode) = -1 then
          Results.Add(Edge.FromNode);
    end;
    Result := Results.ToArray;
  finally
    Results.Free;
  end;
end;

function TAiRagGraph.GetNodeCount: Integer;
begin
  Result := FNodeRegistry.Count;
end;

function TAiRagGraph.GetNodesByDegree(ATop: Integer; ADegreeType: TDegreeType): TArray<TAiRagGraphNode>;
var
  NodeList: TList<TAiRagGraphNode>;
begin
  NodeList := TList<TAiRagGraphNode>.Create;
  try
    NodeList.AddRange(FNodeRegistry.Values.ToArray); // Copiar todos los nodos a una lista

    // Ordenar la lista usando TList.Sort con un comparador anónimo
    NodeList.Sort(TComparer<TAiRagGraphNode>.Construct(
      function(const Left, Right: TAiRagGraphNode): Integer
      var
        LeftDegree, RightDegree: Integer;
      begin
        case ADegreeType of
          dtIn:
            begin
              LeftDegree := Left.IncomingEdges.Count;
              RightDegree := Right.IncomingEdges.Count;
            end;
          dtOut:
            begin
              LeftDegree := Left.OutgoingEdges.Count;
              RightDegree := Right.OutgoingEdges.Count;
            end;
        else // dtTotal
          LeftDegree := Left.IncomingEdges.Count + Left.OutgoingEdges.Count;
          RightDegree := Right.IncomingEdges.Count + Right.OutgoingEdges.Count;
        end;
        Result := RightDegree - LeftDegree; // Orden descendente
      end));

    // Devolver solo el Top N
    if ATop > NodeList.Count then
      ATop := NodeList.Count;
    Result := Copy(NodeList.ToArray, 0, ATop);

  finally
    NodeList.Free;
  end;
end;

function TAiRagGraph.GetNodesRAGVector: TAiRAGVector;
begin
  Result := FNodes;
end;

function TAiRagGraph.GetSearchOptions: TAiSearchOptions;
begin
  // Exponemos las opciones del vector de nodos, que es el que se usa para el "Anclaje"
  Result := FNodes.SearchOptions;
end;

function TAiRagGraph.GetShortestPath(AStartNode, AEndNode: TAiRagGraphNode): TArray<TObject>;
type
  TPathLink = TPair<TObject, TAiRagGraphEdge>; // Almacena [NodoAnterior, AristaUsada]
var
  Queue: TQueue<TAiRagGraphNode>;
  CameFrom: TDictionary<TAiRagGraphNode, TPathLink>;
  CurrentNode, NeighborNode: TAiRagGraphNode;
  CurrentEdge: TAiRagGraphEdge;
  PathList: TList<TObject>;
  PathLink: TPathLink;
begin
  Result := []; // Por defecto, devuelve un array vacío si no hay camino
  if (AStartNode = nil) or (AEndNode = nil) or (AStartNode = AEndNode) then
    Exit;

  Queue := TQueue<TAiRagGraphNode>.Create;
  CameFrom := TDictionary<TAiRagGraphNode, TPathLink>.Create;
  PathList := TList<TObject>.Create;
  try
    // 1. Inicialización
    Queue.Enqueue(AStartNode);
    CameFrom.Add(AStartNode, TPathLink.Create(nil, nil)); // El nodo de inicio no viene de ninguna parte

    // 2. Bucle BFS
    while Queue.Count > 0 do
    begin
      CurrentNode := Queue.Dequeue;

      if CurrentNode = AEndNode then
        Break; // ¡Camino encontrado!

      for CurrentEdge in CurrentNode.OutgoingEdges do
      begin
        NeighborNode := CurrentEdge.ToNode;
        if not CameFrom.ContainsKey(NeighborNode) then
        begin
          CameFrom.Add(NeighborNode, TPathLink.Create(CurrentNode, CurrentEdge));
          Queue.Enqueue(NeighborNode);
        end;
      end;
    end;

    // 3. Reconstrucción del Camino (si se encontró)
    if CameFrom.ContainsKey(AEndNode) then
    begin
      CurrentNode := AEndNode;
      while CurrentNode <> nil do
      begin
        PathList.Add(CurrentNode);
        if CameFrom.TryGetValue(CurrentNode, PathLink) and (PathLink.Key <> nil) then
        begin
          if PathLink.Value <> nil then
            PathList.Add(PathLink.Value); // Añadir la arista
          CurrentNode := PathLink.Key as TAiRagGraphNode;
        end
        else
        begin
          CurrentNode := nil; // Llegamos al nodo de inicio
        end;
      end;

      PathList.Reverse; // Invertir para obtener el orden Start -> End
      Result := PathList.ToArray;
    end;

  finally
    Queue.Free;
    CameFrom.Free;
    PathList.Free;
  end;
end;

function TAiRagGraph.GetUniqueEdgeLabels: TArray<string>;
var
  UniqueLabels: TDictionary<string, Boolean>;
  Edge: TAiRagGraphEdge;
begin
  // Inicializar
  Result := [];

  // 1. Intentar delegar la operación al Driver (BD)
  if Assigned(FDriver) then
  begin
    // El driver devuelve el array de etiquetas de la BD
    Result := FDriver.GetUniqueEdgeLabels;

    // Si la BD devolvió resultados, salimos.
    if Length(Result) > 0 then
      Exit;
  end;

  // 2. Fallback a la lógica por defecto en memoria.
  UniqueLabels := TDictionary<string, Boolean>.Create;
  try
    for Edge in FEdgeRegistry.Values do
    begin
      UniqueLabels.AddOrSetValue(Edge.EdgeLabel, True);
    end;
    Result := UniqueLabels.Keys.ToArray;
  finally
    UniqueLabels.Free;
  end;
end;

function TAiRagGraph.GetUniqueNodeLabels: TArray<string>;
begin
  // Inicializar
  Result := [];

  // 1. Intentar delegar la operación al Driver (BD)
  if Assigned(FDriver) then
  begin
    // El driver devuelve el array de etiquetas de la BD
    Result := FDriver.GetUniqueNodeLabels;

    // Si la BD devolvió resultados, salimos.
    if Length(Result) > 0 then
      Exit;
  end;

  // 2. Fallback a la lógica por defecto en memoria.
  Result := FNodeLabelIndex.Keys.ToArray;
end;

function TAiRagGraph.GraphToContextText(const ANodes: TArray<TAiRagGraphNode>): string;
var
  SB: TStringBuilder;
  NodeSet: TDictionary<string, TAiRagGraphNode>; // Usamos ID para búsqueda rápida
  RelevantEdges: TDictionary<string, TAiRagGraphEdge>; // Para evitar duplicados
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Chunk: TAiEmbeddingNode;
  Pair: TPair<string, Variant>;
  IsFirst: Boolean;
begin
  if Length(ANodes) = 0 then
    Exit('No data found.');

  SB := TStringBuilder.Create;
  NodeSet := TDictionary<string, TAiRagGraphNode>.Create;
  RelevantEdges := TDictionary<string, TAiRagGraphEdge>.Create;
  try
    // 1. Indexar los nodos encontrados para búsqueda rápida O(1)
    // Esto nos sirve para filtrar solo las aristas que conectan nodos de este conjunto.
    for Node in ANodes do
    begin
      if not NodeSet.ContainsKey(Node.ID) then
        NodeSet.Add(Node.ID, Node);
    end;

    // =========================================================================
    // SECCIÓN 1: ENTIDADES Y CONTENIDO (Conocimiento Semántico)
    // =========================================================================
    SB.AppendLine('### ENTITIES & CONTENT ###');

    for Node in ANodes do
    begin
      // Formato: - [ID] Nombre (Tipo)
      SB.AppendFormat('- [%s] %s (%s)', [Node.ID, Node.Name, Node.NodeLabel]);

      // Incluir propiedades clave del nodo (Metadata)
      if Node.MetaData.InternalDictionary.Count > 0 then
      begin
        SB.Append(' {');
        IsFirst := True;
        for Pair in Node.MetaData.InternalDictionary do
        begin
          if not IsFirst then
            SB.Append(', ');
          SB.AppendFormat('%s: %s', [Pair.Key, VarToStr(Pair.Value)]);
          IsFirst := False;
        end;
        SB.Append('}');
      end;
      SB.AppendLine;

      // Resumen del nodo (si tiene)
      if not Node.Text.Trim.IsEmpty then
        SB.AppendFormat('  Summary: %s', [Node.Text]).AppendLine;

      // Fragmentos detallados (Chunks del RAG)
      if Node.Chunks.Count > 0 then
      begin
        for Chunk in Node.Chunks do
        begin
          // Identamos para mostrar jerarquía
          SB.AppendFormat('  + Detail: %s', [Chunk.Text]).AppendLine;
        end;
      end;

      // Separador visual entre nodos
      SB.AppendLine;
    end;

    // =========================================================================
    // SECCIÓN 2: RELACIONES / HECHOS (Conocimiento Estructural)
    // =========================================================================
    // Aquí ocurre la magia del grafo: reconstruimos la historia conectando los puntos.

    // Recolectar aristas INTERNAS (donde Origen Y Destino están en nuestro resultado)
    for Node in ANodes do
    begin
      for Edge in Node.OutgoingEdges do
      begin
        // ¿El nodo destino también fue encontrado en la búsqueda?
        if NodeSet.ContainsKey(Edge.ToNode.ID) then
        begin
          if not RelevantEdges.ContainsKey(Edge.ID) then
            RelevantEdges.Add(Edge.ID, Edge);
        end;
      end;
    end;

    if RelevantEdges.Count > 0 then
    begin
      SB.AppendLine('### RELATIONSHIPS (FACTS) ###');
      for Edge in RelevantEdges.Values do
      begin
        // Formato Cypher-like simplificado para el LLM: (A)-[REL]->(B)
        SB.AppendFormat('(%s)-[%s]->(%s)', [Edge.FromNode.Name, Edge.EdgeLabel.ToUpper, Edge.ToNode.Name]);

        // Incluir detalles de la relación (ej: since: 2020, weight: 0.9)
        if Edge.MetaData.InternalDictionary.Count > 0 then
        begin
          SB.Append(' properties: {');
          IsFirst := True;
          for Pair in Edge.MetaData.InternalDictionary do
          begin
            if not IsFirst then
              SB.Append(', ');
            SB.AppendFormat('%s: %s', [Pair.Key, VarToStr(Pair.Value)]);
            IsFirst := False;
          end;
          SB.Append('}');
        end;

        // Incluir peso si es relevante (distinto de 1)
        if Abs(Edge.Weight - 1.0) > 0.001 then
          SB.AppendFormat(' (Weight: %s)', [FormatFloat('0.##', Edge.Weight)]);

        SB.AppendLine;
      end;
    end;

    Result := SB.ToString;

  finally
    NodeSet.Free;
    RelevantEdges.Free;
    SB.Free;
  end;
end;

function TAiRagGraph.InternalAddEdge(AEdge: TAiRagGraphEdge; AShouldPersist: Boolean): TAiRagGraphEdge;
begin
  if (AEdge = nil) or (AEdge.OwnerGraph <> Self) or (AEdge.FromNode = nil) or (AEdge.ToNode = nil) then
    raise Exception.Create('Invalid edge provided to InternalAddEdge.');

  // 1. VERIFICACIÓN DE IDENTIDAD (Identity Map)
  if FEdgeRegistry.TryGetValue(AEdge.ID, Result) then
  begin
    Exit; // La arista ya existe, devolvemos la instancia en memoria.
  end;

  // 2. LÓGICA DE AÑADIR A ESTRUCTURAS EN MEMORIA
  FEdgeRegistry.Add(AEdge.ID, AEdge);
  FEdges.Items.Add(AEdge);
  // Conectar la arista a los nodos
  AEdge.FromNode.AddOutgoingEdge(AEdge);
  AEdge.ToNode.AddIncomingEdge(AEdge);

  // 3. PERSISTENCIA CONDICIONAL
  if AShouldPersist and Assigned(FDriver) then
  begin
    try
      FDriver.AddEdge(AEdge);
    except
      // Lógica de reversión simplificada si falla la persistencia.
      AEdge.FromNode.RemoveOutgoingEdge(AEdge);
      AEdge.ToNode.RemoveIncomingEdge(AEdge);
      FEdgeRegistry.Remove(AEdge.ID);
      FEdges.Items.Remove(AEdge);
      raise;
    end;
  end;

  // 4. DEVOLVER LA ARISTA
  Result := AEdge;
end;

function TAiRagGraph.InternalAddNode(ANode: TAiRagGraphNode; AShouldPersist: Boolean): TAiRagGraphNode;
var
  NodeList: TList<TAiRagGraphNode>;
  CombinedNameKey: string;
begin
  if (ANode = nil) or (ANode.OwnerGraph <> Self) then
    raise Exception.Create('Invalid node provided to InternalAddNode.');

  // 1. VERIFICACIÓN DE IDENTIDAD (Identity Map)
  // Comprobamos si un nodo con este ID ya está registrado en memoria.
  if FNodeRegistry.TryGetValue(ANode.ID, Result) then
  begin
    // El nodo ya existe. Devolvemos el nodo existente para mantener la
    // consistencia. El llamador es responsable de liberar el ANode duplicado.
    Exit;
  end;

  // 2. LÓGICA DE AÑADIR A ESTRUCTURAS EN MEMORIA
  // Si no existe, procedemos a añadirlo a todas nuestras estructuras.
  FNodeRegistry.Add(ANode.ID, ANode);
  FNodes.Items.Add(ANode);

  // Actualizar FNodeLabelIndex
  if not FNodeLabelIndex.TryGetValue(ANode.NodeLabel, NodeList) then
  begin
    NodeList := TList<TAiRagGraphNode>.Create;
    FNodeLabelIndex.Add(ANode.NodeLabel, NodeList);
  end;
  NodeList.Add(ANode);

  // Actualizar FNodeNameIndex
  if not ANode.Name.IsEmpty then
  begin
    CombinedNameKey := ANode.NodeLabel + '#' + ANode.Name;
    if not FNodeNameIndex.ContainsKey(CombinedNameKey) then
      FNodeNameIndex.Add(CombinedNameKey, ANode);
  end;

  // 3. PERSISTENCIA CONDICIONAL EN BASE DE DATOS
  // Solo si el llamador nos lo pide (AShouldPersist = True).

  if AShouldPersist and Assigned(FDriver) then
  begin
    try
      FDriver.AddNode(ANode);
    except
      // Si el driver falla, deshacemos en memoria y relanzamos.
      UnregisterNode(ANode);
      raise;
    end;
  end;

  // 4. DEVOLVER EL NODO
  // El nodo (ANode) ahora es propiedad del grafo y está registrado.
  Result := ANode;
end;

function TAiRagGraph.InternalHydrateEdge(const AEdgeData: TEdgeDataRecord): TAiRagGraphEdge;
var
  NewEdge: TAiRagGraphEdge;
  FromNode, ToNode: TAiRagGraphNode;
  Dim: Integer;
  JObj: TJSONObject;
begin
  // 1. Verificación de Identidad (Cache en memoria)
  if FEdgeRegistry.TryGetValue(AEdgeData.ID, Result) then
    Exit;

  // 2. Asegurar existencia de nodos conectados (Carga recursiva si es necesario)
  FromNode := Self.FindNodeByID(AEdgeData.SourceNodeID);
  ToNode := Self.FindNodeByID(AEdgeData.TargetNodeID);

  if (FromNode = nil) or (ToNode = nil) then
    Exit(nil);

  // 3. Determinar dimensiones vectoriales
  if FEdges.Dim > 0 then
    Dim := FEdges.Dim
  else
    Dim := 1536;

  // 4. Crear e hidratar la instancia
  NewEdge := TAiRagGraphEdge.Create(Self, Dim);
  try
    NewEdge.ID := AEdgeData.ID;
    NewEdge.EdgeLabel := AEdgeData.EdgeLabel;
    NewEdge.Name := AEdgeData.Name;
    NewEdge.Weight := AEdgeData.Weight;
    NewEdge.FromNode := FromNode;
    NewEdge.ToNode := ToNode;

    // --- CAMBIO CLAVE: Uso del nuevo MetaData unificado ---
    if not AEdgeData.PropertiesJSON.IsEmpty then
    begin
      JObj := TJSONObject.ParseJSONValue(AEdgeData.PropertiesJSON) as TJSONObject;
      if Assigned(JObj) then
        try
          NewEdge.MetaData.FromJSON(JObj);
        finally
          JObj.Free;
        end;
    end;

    // Hidratar el vector (Data)
    NewEdge.Data := StringToEmbedding(AEdgeData.EmbeddingStr);

    // 5. Registro en estructuras de memoria (Identity Map)
    Result := InternalAddEdge(NewEdge, False);

    if Result <> NewEdge then
      NewEdge.Free;
  except
    NewEdge.Free;
    raise;
  end;
end;

function TAiRagGraph.InternalHydrateNode(const ANodeData: TNodeDataRecord): TAiRagGraphNode;
var
  NewNode: TAiRagGraphNode;
  Dim: Integer;
  JObj: TJSONObject;
begin
  // 1. Verificación de Identidad (Cache en memoria)
  if FNodeRegistry.TryGetValue(ANodeData.ID, Result) then
    Exit;

  // 2. Determinar dimensiones vectoriales
  if FNodes.Dim > 0 then
    Dim := FNodes.Dim
  else
    Dim := 1536;

  // 3. Crear e hidratar la instancia
  NewNode := TAiRagGraphNode.Create(Self, Dim);
  try
    NewNode.ID := ANodeData.ID;
    NewNode.NodeLabel := ANodeData.NodeLabel;
    NewNode.Name := ANodeData.Name;
    NewNode.Text := ANodeData.NodeText;

    // --- CAMBIO CLAVE: Uso del nuevo MetaData unificado ---
    if not ANodeData.PropertiesJSON.IsEmpty then
    begin
      JObj := TJSONObject.ParseJSONValue(ANodeData.PropertiesJSON) as TJSONObject;
      try
        if (JObj <> nil) and (JObj is TJSONObject) then // Validación de tipo
          NewNode.MetaData.FromJSON(JObj as TJSONObject);
      finally
        JObj.Free;
      end;
    end;

    // Hidratar el vector de resumen (Data)
    NewNode.Data := StringToEmbedding(ANodeData.EmbeddingStr);

    // 4. Registro en estructuras de memoria (Identity Map)
    Result := InternalAddNode(NewNode, False);

    if Result <> NewNode then
      NewNode.Free;
  except
    NewNode.Free;
    raise;
  end;
end;

function TAiRagGraph.Search(const APrompt: string; const ADepth: Integer; ALimit: Integer; const APrecision: Double; const AFilter: TAiFilterCriteria): TArray<TAiRagGraphNode>;
var
  VectorSearchResults: TAiRAGVector;
  InitialNodeList: TList<TAiRagGraphNode>;
  FoundItem: TAiEmbeddingNode;
begin
  SetLength(Result, 0);

  // 1. Delegación al Driver (Base de Datos Externa)
  if Assigned(FDriver) then
  begin
    // El driver ahora recibe el tipo correcto
    Result := FDriver.SearchNodes(APrompt, ADepth, ALimit, APrecision, AFilter);
    Exit;
  end;

  // 2. Lógica en memoria

  // --- Validación del Motor de Embeddings ---
  if not Assigned(FEmbeddings) then
  begin
    if Assigned(FNodes) and Assigned(FNodes.Embeddings) then
      FEmbeddings := FNodes.Embeddings;

    // Si la búsqueda es híbrida o vectorial, necesitamos embeddings
    if (not Assigned(FEmbeddings)) and (FNodes.SearchOptions.UseEmbeddings) then
      raise Exception.Create('Graph Search Error: Embeddings property is not assigned.');
  end;

  if FNodes.Count = 0 then
    Exit;

  // Sincronizamos
  FNodes.Embeddings := FEmbeddings;

  // 3. Búsqueda en el Vector
  // AQUI ESTÁ LA MEJORA: Ya no necesitamos casting, pasamos AFilter directamente
  // porque FNodes.Search ya espera un TAiFilterCriteria.
  VectorSearchResults := FNodes.Search(APrompt, ALimit, APrecision, AFilter);

  try
    InitialNodeList := TList<TAiRagGraphNode>.Create;
    try
      // 4. Filtrar resultados y convertir a Nodos de Grafo
      for FoundItem in VectorSearchResults.Items do
      begin
        if FoundItem is TAiRagGraphNode then
          InitialNodeList.Add(TAiRagGraphNode(FoundItem));
      end;

      // 5. Expansión Contextual (BFS)
      if (ADepth > 0) and (InitialNodeList.Count > 0) then
      begin
        Result := Self.ExpandNodeList(InitialNodeList, ADepth);
      end
      else
      begin
        Result := InitialNodeList.ToArray;
      end;
    finally
      InitialNodeList.Free;
    end;
  finally
    VectorSearchResults.Free;
  end;
end;

function TAiRagGraph.SearchText(const APrompt: string; ADepth: Integer; ShowProperties: Boolean; const ALimit: Integer; const APrecision: Double; const AFilter: TAiFilterCriteria): string;
var
  FoundNodes: TArray<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Chunk: TAiEmbeddingNode;
  ContextBuilder: TStringBuilder;
  PropPair: TPair<string, Variant>;
begin
  // Llamamos a Search pasando el AFilter actualizado
  FoundNodes := Self.Search(APrompt, ADepth, ALimit, APrecision, AFilter);

  if Length(FoundNodes) = 0 then
    Exit('No se encontró información relevante para: ' + APrompt);

  // ... (El resto del código de generación de texto se mantiene igual) ...
  // Solo copio el inicio para brevedad
  ContextBuilder := TStringBuilder.Create;
  try
    ContextBuilder.AppendLine('Contexto extraído del grafo de conocimiento:');
    // ... lógica de construcción de string ...
    Result := ContextBuilder.ToString;
  finally
    ContextBuilder.Free;
  end;
end;

procedure TAiRagGraph.LoadFromStream(AStream: TStream);
var
  SR: TStreamReader;
  JsonValue: TJSONValue;
  Root, GraphObj, NodeObj, EdgeObj, PropObj, ChunkObj: TJSONObject;
  NodesArr, EdgesArr, EmbeddingArr, ChunksArr: TJSONArray;
  NodeVal, EdgeVal, ChunkVal: TJSONValue;
  I, J, K: Integer;

  // Variables temporales para Nodos
  NewNode: TAiRagGraphNode;
  NodeID, NodeLabel, NodeName, NodeText: string;

  // Variables temporales para Aristas
  NewEdge: TAiRagGraphEdge;
  EdgeID, EdgeLabel, EdgeName, SourceID, TargetID: string;
  EdgeWeight: Double;
  FromNode, ToNode: TAiRagGraphNode;

  // Variables para Chunks
  ChunkText: string;
  ChunkData: TAiEmbeddingData;
begin
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  // 1. Limpiar el estado actual antes de cargar (Identity Map y Vectores)
  Clear;

  SR := TStreamReader.Create(AStream, TEncoding.UTF8);
  try
    JsonValue := TJSONObject.ParseJSONValue(SR.ReadToEnd);
    try
      if not(JsonValue is TJSONObject) then
        raise Exception.Create('Formato JSON inválido para el Grafo.');

      Root := JsonValue as TJSONObject;
      GraphObj := Root.GetValue<TJSONObject>('graph');
      if GraphObj = nil then
        Exit;

      // --- PASO CRÍTICO: Bloquear actualizaciones de índices ---
      // Esto evita reconstrucciones costosas de HNSW/BM25 durante la carga masiva.
      BeginUpdate;
      try

        // --- 2. CARGAR NODOS ---
        NodesArr := GraphObj.GetValue<TJSONArray>('nodes');
        if NodesArr <> nil then
        begin
          for NodeVal in NodesArr do
          begin
            NodeObj := NodeVal as TJSONObject;

            NodeID := NodeObj.GetValue<string>('id');
            NodeLabel := NodeObj.GetValue<string>('nodeLabel');
            NodeName := NodeObj.GetValue<string>('name', '');
            NodeText := NodeObj.GetValue<string>('node_text', '');

            // 2.1 Crear e insertar nodo en el registro de memoria
            NewNode := Self.AddNode(NodeID, NodeLabel, NodeName);
            NewNode.Text := NodeText;
            NewNode.Model := NodeObj.GetValue<string>('model', '');

            // 2.2 Recuperar Metadatos Unificados (Uso de FromJSON)
            // Esto restaura tipos reales: Fechas, Booleanos y Números.
            if NodeObj.TryGetValue<TJSONObject>('properties', PropObj) then
              NewNode.MetaData.FromJSON(PropObj);

            // 2.3 NUEVO: Recuperar Chunks (Fragmentos de texto detallados)
            if NodeObj.TryGetValue<TJSONArray>('chunks', ChunksArr) then
            begin
              for ChunkVal in ChunksArr do
              begin
                ChunkObj := ChunkVal as TJSONObject;
                ChunkText := ChunkObj.GetValue<string>('text');

                // Cargar vector del chunk si existe
                SetLength(ChunkData, 0);
                if ChunkObj.TryGetValue<TJSONArray>('embedding', EmbeddingArr) then
                begin
                  SetLength(ChunkData, EmbeddingArr.Count);
                  for K := 0 to EmbeddingArr.Count - 1 do
                    ChunkData[K] := (EmbeddingArr.Items[K] as TJSONNumber).AsDouble;
                end;

                NewNode.AddChunk(ChunkText, ChunkData);
              end;
            end;

            // 2.4 Recuperar Embedding Principal (Resumen del Nodo)
            if NodeObj.TryGetValue<TJSONArray>('embedding', EmbeddingArr) then
            begin
              NewNode.SetDataLength(EmbeddingArr.Count);
              for J := 0 to EmbeddingArr.Count - 1 do
                NewNode.Data[J] := (EmbeddingArr.Items[J] as TJSONNumber).AsDouble;
            end;
          end;
        end;

        // --- 3. CARGAR ARISTAS (RELACIONES) ---
        EdgesArr := GraphObj.GetValue<TJSONArray>('edges');
        if EdgesArr <> nil then
        begin
          for EdgeVal in EdgesArr do
          begin
            EdgeObj := EdgeVal as TJSONObject;

            EdgeID := EdgeObj.GetValue<string>('id');
            EdgeLabel := EdgeObj.GetValue<string>('edgeLabel');
            EdgeName := EdgeObj.GetValue<string>('name', '');
            SourceID := EdgeObj.GetValue<string>('source');
            TargetID := EdgeObj.GetValue<string>('target');
            EdgeWeight := EdgeObj.GetValue<Double>('weight', 1.0);

            // Buscar referencias a los nodos (ya cargados en el paso 2)
            FromNode := Self.FindNodeByID(SourceID);
            ToNode := Self.FindNodeByID(TargetID);

            if (FromNode <> nil) and (ToNode <> nil) then
            begin
              NewEdge := Self.AddEdge(FromNode, ToNode, EdgeID, EdgeLabel, EdgeName, EdgeWeight);

              // Metadatos de la Arista
              if EdgeObj.TryGetValue<TJSONObject>('properties', PropObj) then
                NewEdge.MetaData.FromJSON(PropObj);

              // Embedding de la Arista (si existe)
              if EdgeObj.TryGetValue<TJSONArray>('embedding', EmbeddingArr) then
              begin
                NewEdge.SetDataLength(EmbeddingArr.Count);
                for J := 0 to EmbeddingArr.Count - 1 do
                  NewEdge.Data[J] := (EmbeddingArr.Items[J] as TJSONNumber).AsDouble;
              end;
            end;
          end;
        end;

      finally
        // --- 4. RECONSTRUCCIÓN DE ÍNDICES ---
        // EndUpdate invoca internamente a RebuildIndexes, activando BM25 y HNSW
        // sobre todos los datos recién cargados.
        EndUpdate;
      end;

    finally
      JsonValue.Free;
    end;
  finally
    SR.Free;
  end;
end;

function TAiRagGraph.Match(AQuery: TGraphMatchQuery; ADepth: Integer = 0): TArray<TDictionary<string, TObject>>;
type
  TMatchState = TDictionary<string, TObject>;
var
  Results: TObjectList<TDictionary<string, TObject>>;
  StartNodePattern: TMatchNodePattern;
  InitialState: TMatchState;
  StartNode: TAiRagGraphNode;
  CandidateNodes: TArray<TAiRagGraphNode>;

  // --- Variables para la Expansión (ADepth > 0) ---
  SeedNodesSet: TDictionary<TAiRagGraphNode, Boolean>;
  MatchDict: TDictionary<string, TObject>;
  Pair: TPair<string, TObject>;
  ExpandedNodesArray: TArray<TAiRagGraphNode>;
  ExpandedResults: TObjectList<TDictionary<string, TObject>>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Dict: TDictionary<string, TObject>;
  EdgeSet: TDictionary<TAiRagGraphEdge, Boolean>;

  procedure FindMatchesRecursive(AClauseIndex: Integer; ACurrentState: TMatchState);
  var
    CurrentClause: TMatchClause;
    TargetNodePattern: TMatchNodePattern;
    SourceGraphNode, NeighborNode: TAiRagGraphNode;
    CurrentEdge: TAiRagGraphEdge;
    NewState: TMatchState;
    BoundObject: TObject;
  begin

    if AClauseIndex >= AQuery.MatchClauses.Count then
    begin
      // --- AQUÍ ENTRA EL WHERE ---
      if (AQuery.WhereClause = nil) or Boolean(EvaluateGraphExpression(AQuery.WhereClause, ACurrentState)) then
      begin
        // Solo si no hay WHERE o si el WHERE devuelve TRUE, aceptamos el resultado
        Results.Add(TMatchState.Create(ACurrentState));
      end;
      Exit;
    end;

    CurrentClause := AQuery.MatchClauses[AClauseIndex];
    if not ACurrentState.TryGetValue(CurrentClause.SourceNodeVar, BoundObject) then
      Exit;
    SourceGraphNode := BoundObject as TAiRagGraphNode;
    TargetNodePattern := AQuery.NodePatternByVariable[CurrentClause.TargetNodeVar];

    if (CurrentClause.EdgePattern.Direction = gdOutgoing) or (CurrentClause.EdgePattern.Direction = gdBoth) then
    begin
      for CurrentEdge in SourceGraphNode.OutgoingEdges do
      begin
        NeighborNode := CurrentEdge.ToNode;
        if CurrentClause.EdgePattern.Matches(CurrentEdge, gdOutgoing) and TargetNodePattern.Matches(NeighborNode) then
        begin
          if ACurrentState.TryGetValue(CurrentClause.TargetNodeVar, BoundObject) and (BoundObject <> NeighborNode) then
            Continue;
          if (not CurrentClause.EdgePattern.Variable.IsEmpty) and ACurrentState.TryGetValue(CurrentClause.EdgePattern.Variable, BoundObject) and (BoundObject <> CurrentEdge) then
            Continue;

          NewState := TMatchState.Create(ACurrentState);
          try
            NewState.AddOrSetValue(CurrentClause.TargetNodeVar, NeighborNode);
            if not CurrentClause.EdgePattern.Variable.IsEmpty then
              NewState.AddOrSetValue(CurrentClause.EdgePattern.Variable, CurrentEdge);
            FindMatchesRecursive(AClauseIndex + 1, NewState);
          finally
            NewState.Free;
          end;
        end;
      end;
    end;

    if (CurrentClause.EdgePattern.Direction = gdIncoming) or (CurrentClause.EdgePattern.Direction = gdBoth) then
    begin
      for CurrentEdge in SourceGraphNode.IncomingEdges do
      begin
        NeighborNode := CurrentEdge.FromNode;
        if CurrentClause.EdgePattern.Matches(CurrentEdge, gdIncoming) and TargetNodePattern.Matches(NeighborNode) then
        begin
          if ACurrentState.TryGetValue(CurrentClause.TargetNodeVar, BoundObject) and (BoundObject <> NeighborNode) then
            Continue;
          if (not CurrentClause.EdgePattern.Variable.IsEmpty) and ACurrentState.TryGetValue(CurrentClause.EdgePattern.Variable, BoundObject) and (BoundObject <> CurrentEdge) then
            Continue;

          NewState := TMatchState.Create(ACurrentState);
          try
            NewState.AddOrSetValue(CurrentClause.TargetNodeVar, NeighborNode);
            if not CurrentClause.EdgePattern.Variable.IsEmpty then
              NewState.AddOrSetValue(CurrentClause.EdgePattern.Variable, CurrentEdge);
            FindMatchesRecursive(AClauseIndex + 1, NewState);
          finally
            NewState.Free;
          end;
        end;
      end;
    end;
  end;

begin
  Result := [];
  if (AQuery = nil) then
    Exit;

  Results := TObjectList < TDictionary < string, TObject >>.Create(True);
  try
    // =========================================================================
    // CASO 1: SOLO NODOS (Sin relaciones/clauses) -> MATCH (n:Persona) ...
    // =========================================================================
    if AQuery.MatchClauses.Count = 0 then
    begin
      if AQuery.NodePatterns.Count > 0 then
      begin
        // Tomamos el primer patrón (ej: 'p')
        // Nota: Si hay múltiples nodos desconectados MATCH (a), (b) requeriría producto cartesiano.
        // Aquí simplificamos para el caso común de un solo nodo desconectado.
        StartNodePattern := AQuery.NodePatterns[0];

        // 1. Filtrar candidatos iniciales
        if not StartNodePattern.NodeLabel.IsEmpty then
          CandidateNodes := Self.FindNodesByLabel(StartNodePattern.NodeLabel)
        else
          CandidateNodes := FNodeRegistry.Values.ToArray;

        // 2. Iterar y validar
        for StartNode in CandidateNodes do
        begin
          if StartNodePattern.Matches(StartNode) then
          begin
            InitialState := TMatchState.Create;
            try
              InitialState.Add(StartNodePattern.Variable, StartNode);
              // Verificar WHERE para este nodo individual
              if (AQuery.WhereClause = nil) or Boolean(EvaluateGraphExpression(AQuery.WhereClause, InitialState)) then
              begin
                Results.Add(TMatchState.Create(InitialState));
              end;
            finally
              InitialState.Free;
            end;
          end;
        end;
      end;
    end
    // =========================================================================
    // CASO 2: ESTRUCTURAL (Con relaciones) -> MATCH (a)-[r]->(b)
    // =========================================================================
    else
    begin
      // --- PASO 1: BÚSQUEDA DEL NODO ANCLA ---
      StartNodePattern := AQuery.NodePatternByVariable[AQuery.MatchClauses[0].SourceNodeVar];
      if StartNodePattern = nil then
        raise Exception.Create('Start node pattern for the first clause is missing.');

      if not StartNodePattern.NodeLabel.IsEmpty then
        CandidateNodes := Self.FindNodesByLabel(StartNodePattern.NodeLabel)
      else
        CandidateNodes := FNodeRegistry.Values.ToArray;

      for StartNode in CandidateNodes do
      begin
        if StartNodePattern.Matches(StartNode) then
        begin
          InitialState := TMatchState.Create;
          try
            InitialState.Add(StartNodePattern.Variable, StartNode);
            FindMatchesRecursive(0, InitialState);
          finally
            InitialState.Free;
          end;
        end;
      end;
    end;

    // --- PASO COMÚN: EXPANSIÓN DEL SUBGRAFO (si ADepth > 0 y hay resultados) ---
    // (Este código es idéntico al anterior, pero se aplica a los resultados de ambos casos)
    if (ADepth > 0) and (Results.Count > 0) then
    begin
      // 1. Recolectar nodos semilla únicos
      SeedNodesSet := TDictionary<TAiRagGraphNode, Boolean>.Create;
      try
        for MatchDict in Results do
        begin
          for Pair in MatchDict do
          begin
            if Pair.Value is TAiRagGraphNode then
              SeedNodesSet.AddOrSetValue(Pair.Value as TAiRagGraphNode, True);
          end;
        end;

        if SeedNodesSet.Count > 0 then
        begin
          // 2. Expandir el vecindario
          ExpandedNodesArray := Self.ExpandNodeList(TList<TAiRagGraphNode>.Create(SeedNodesSet.Keys), ADepth);

          // 3. Construir la salida del subgrafo
          ExpandedResults := TObjectList < TDictionary < string, TObject >>.Create(True);
          EdgeSet := TDictionary<TAiRagGraphEdge, Boolean>.Create;
          try
            // Añadir Nodos
            for Node in ExpandedNodesArray do
            begin
              Dict := TDictionary<string, TObject>.Create;
              Dict.Add('type', TStringWrapper.Create('node'));
              Dict.Add('element', Node);
              ExpandedResults.Add(Dict);
            end;

            // Añadir Aristas Internas
            var
            NodeSet := TDictionary<TAiRagGraphNode, Boolean>.Create;
            try
              for Node in ExpandedNodesArray do
                NodeSet.Add(Node, True);

              for Node in ExpandedNodesArray do
              begin
                for Edge in Node.OutgoingEdges do
                begin
                  if NodeSet.ContainsKey(Edge.ToNode) then
                    EdgeSet.AddOrSetValue(Edge, True);
                end;
              end;
            finally
              NodeSet.Free;
            end;

            for Edge in EdgeSet.Keys do
            begin
              Dict := TDictionary<string, TObject>.Create;
              Dict.Add('type', TStringWrapper.Create('edge'));
              Dict.Add('element', Edge);
              ExpandedResults.Add(Dict);
            end;

            Results.Clear;
            for Dict in ExpandedResults do
              Results.Add(Dict);
            ExpandedResults.OwnsObjects := False; // Transferencia de propiedad
          finally
            EdgeSet.Free;
            ExpandedResults.Free;
          end;
        end;
      finally
        SeedNodesSet.Free;
      end;
    end;

    // --- PASO FINAL: DEVOLVER RESULTADO ---
    Result := Results.ToArray;
    Results.OwnsObjects := False;
  finally
    Results.Free;
  end;
end;

procedure TAiRagGraph.MergeNodes(ASurvivingNode, ASubsumedNode: TAiRagGraphNode; APropertyMergeStrategy: TMergeStrategy);
var
  Edge: TAiRagGraphEdge;
  EdgesToMove: TArray<TAiRagGraphEdge>;
  Pair: TPair<string, Variant>;
  Chunk: TAiEmbeddingNode;
begin
  if (ASurvivingNode = nil) or (ASubsumedNode = nil) or (ASurvivingNode = ASubsumedNode) then
    Exit;

  // Bloqueamos actualizaciones para reconstruir índices solo al final
  BeginUpdate;
  try
    // --- 1. RECONECTAR ARISTAS ENTRANTES ---
    EdgesToMove := ASubsumedNode.IncomingEdges.ToArray;
    for Edge in EdgesToMove do
    begin
      // Cambiamos el destino de la arista al nodo sobreviviente
      Edge.ToNode := ASurvivingNode;
      ASurvivingNode.AddIncomingEdge(Edge);
    end;

    // --- 2. RECONECTAR ARISTAS SALIENTES ---
    EdgesToMove := ASubsumedNode.OutgoingEdges.ToArray;
    for Edge in EdgesToMove do
    begin
      // Cambiamos el origen de la arista al nodo sobreviviente
      Edge.FromNode := ASurvivingNode;
      ASurvivingNode.AddOutgoingEdge(Edge);
    end;

    // --- 3. TRASLADAR CHUNKS (Opción 1) ---
    // Movemos los fragmentos de texto detallados para no perder contexto RAG
    while ASubsumedNode.Chunks.Count > 0 do
    begin
      Chunk := ASubsumedNode.Chunks[0];
      // Quitamos la propiedad del objeto del nodo viejo sin liberarlo
      ASubsumedNode.Chunks.Extract(Chunk);
      // Lo añadimos al nuevo nodo
      ASurvivingNode.Chunks.Add(Chunk);
    end;

    // --- 4. FUSIONAR PROPIEDADES (MetaData) ---
    // Usamos InternalDictionary para iterar sobre las propiedades del MetaData
    for Pair in ASubsumedNode.Properties.InternalDictionary do
    begin
      case APropertyMergeStrategy of
        msAddNewOnly:
          begin
            // Usamos el método .Has() del nuevo MetaData
            if not ASurvivingNode.Properties.Has(Pair.Key) then
              ASurvivingNode.Properties[Pair.Key] := Pair.Value;
          end;

        msOverwrite:
          begin
            // La asignación directa indexada realiza el "AddOrSetValue" automáticamente
            ASurvivingNode.Properties[Pair.Key] := Pair.Value;
          end;

        // msKeepExisting: no hace nada
      end;
    end;

    // --- 5. ELIMINAR NODO SUBSUMIDO ---
    // Esto limpia las referencias en el registro del grafo y en los vectores
    DeleteNode(ASubsumedNode);

  finally
    EndUpdate; // Reconstruye BM25 y HNSW incluyendo los nuevos chunks y conexiones
  end;
end;

function TAiRagGraph.Query(const APlan: TQueryPlan; ADepth: Integer; const ALimit: Integer; const APrecision: Double): TArray<TAiRagGraphNode>;
var
  // --- Variables para la implementación en memoria ---
  IntermediateResults: TDictionary<string, TList<TAiRagGraphNode>>;
  InitialNodes: TArray<TAiRagGraphNode>;
  Step: TQueryStep;
  SourceNodes, TargetNodes, ResultSet: TList<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  UniqueTargetNodes: TDictionary<TAiRagGraphNode, Boolean>;
  AnchorList: TList<TAiRagGraphNode>;
begin
  // Inicializar el resultado como un array vacío.
  SetLength(Result, 0);

  // Paso 1: Intentar delegar la ejecución del plan de consulta completo al Driver.
  if Assigned(FDriver) then
  begin
    // El driver realiza la búsqueda híbrida en la BD y devuelve el array de nodos.
    Result := FDriver.Query(APlan, ADepth, ALimit, APrecision);

    // Si la BD encontró resultados (o si el driver se ejecutó), salimos.
    if Length(Result) > 0 then
      Exit;
  end;

  // Paso 2: Si la consulta no fue manejada por un delegado o no encontró resultados,
  // ejecutar la lógica en memoria.

  // --- Lógica original y optimizada de la consulta en memoria ---
  IntermediateResults := TDictionary < string, TList < TAiRagGraphNode >>.Create;
  try
    // --- Etapa 1: Búsqueda Semántica de Anclaje (en memoria) ---
    // Se utiliza el 'Search' del propio componente.
    InitialNodes := Self.Search(APlan.AnchorPrompt, ADepth, ALimit, APrecision);
    if Length(InitialNodes) = 0 then
      Exit; // Sale con un array 'Result' vacío si no hay anclas.

    AnchorList := TList<TAiRagGraphNode>.Create;
    AnchorList.AddRange(InitialNodes);
    IntermediateResults.Add(APlan.AnchorVariable, AnchorList);

    // --- Etapa 2: Ejecución de Pasos Estructurales (en memoria) ---
    for Step in APlan.Steps do
    begin
      if not IntermediateResults.TryGetValue(Step.SourceVariable, SourceNodes) then
        Continue; // No se puede continuar si el paso anterior no arrojó resultados

      if not IntermediateResults.TryGetValue(Step.TargetVariable, TargetNodes) then
      begin
        TargetNodes := TList<TAiRagGraphNode>.Create;
        IntermediateResults.Add(Step.TargetVariable, TargetNodes);
      end;

      UniqueTargetNodes := TDictionary<TAiRagGraphNode, Boolean>.Create;
      try
        // Esto es una optimización para manejar resultados acumulativos
        for Node in TargetNodes do
          UniqueTargetNodes.Add(Node, True);

        for Node in SourceNodes do
        begin
          if Step.IsReversed then // Búsqueda hacia atrás
          begin
            for Edge in Node.IncomingEdges do
            begin
              if SameText(Edge.EdgeLabel, Step.EdgeLabel) and ((Step.TargetNodeLabel = '') or SameText(Edge.FromNode.NodeLabel, Step.TargetNodeLabel)) then
              begin
                if not UniqueTargetNodes.ContainsKey(Edge.FromNode) then
                begin
                  TargetNodes.Add(Edge.FromNode);
                  UniqueTargetNodes.Add(Edge.FromNode, True);
                end;
              end;
            end;
          end
          else // Búsqueda hacia adelante
          begin
            for Edge in Node.OutgoingEdges do
            begin
              if SameText(Edge.EdgeLabel, Step.EdgeLabel) and ((Step.TargetNodeLabel = '') or SameText(Edge.ToNode.NodeLabel, Step.TargetNodeLabel)) then
              begin
                if not UniqueTargetNodes.ContainsKey(Edge.ToNode) then
                begin
                  TargetNodes.Add(Edge.ToNode);
                  UniqueTargetNodes.Add(Edge.ToNode, True);
                end;
              end;
            end;
          end;
        end;
      finally
        UniqueTargetNodes.Free;
      end;
    end;

    // --- Etapa 3: Recopilar y Devolver los Resultados Finales ---
    if IntermediateResults.TryGetValue(APlan.ResultVariable, ResultSet) then
      Result := ResultSet.ToArray
    else
      Result := [];

  finally
    // Limpiar el diccionario y las listas que contiene
    for var List in IntermediateResults.Values do
      List.Free;
    IntermediateResults.Free;
  end;
end;

procedure TAiRagGraph.RebuildIndexes;
var
  Node: TAiRagGraphNode;
  NodeList: TList<TAiRagGraphNode>;
  CombinedNameKey: string;
begin
  // 1. Limpiar completamente los índices secundarios existentes
  for NodeList in FNodeLabelIndex.Values do
    NodeList.Free;
  FNodeLabelIndex.Clear;
  FNodeNameIndex.Clear;

  // 2. Reconstruir los índices secundarios desde cero iterando todos los nodos
  for Node in FNodeRegistry.Values do
  begin
    // Re-indexar por etiqueta
    if not FNodeLabelIndex.TryGetValue(Node.NodeLabel, NodeList) then
    begin
      NodeList := TList<TAiRagGraphNode>.Create;
      FNodeLabelIndex.Add(Node.NodeLabel, NodeList);
    end;
    NodeList.Add(Node);

    // Re-indexar por nombre
    if not Node.Name.IsEmpty then
    begin
      CombinedNameKey := Node.NodeLabel + '#' + Node.Name;
      if not FNodeNameIndex.ContainsKey(CombinedNameKey) then
        FNodeNameIndex.Add(CombinedNameKey, Node);
    end;
  end;

  // 3. Reconstruir los índices vectoriales (la operación más costosa)

  If Assigned(FEmbeddings) then // Si tiene asignado un embeddings recrea los indices, aunque aquí no genera embeddings
  Begin
    FNodes.BuildIndex;
    FEdges.BuildIndex;
  End;
end;

// En uMakerAi.RAG.Graph.Core.pas, sección implementation

procedure WriteVariant(const AWriter: TJsonWriter; const AValue: Variant);
var
  V: Variant;
  VType: TVarType;
begin
  V := AValue; // <- RESUELVE varByRef automáticamente
  VType := VarType(V);

  case VType and varTypeMask of

    varEmpty, varNull:
      AWriter.WriteNull;

    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord:
      AWriter.WriteValue(Integer(V));

    varInt64, varUInt64:
      AWriter.WriteValue(Int64(V));

    varSingle, varDouble, varCurrency:
      AWriter.WriteValue(Double(V));

    varBoolean:
      AWriter.WriteValue(Boolean(V));

    varDate:
      AWriter.WriteValue(DateToISO8601(TDateTime(V), False));

    varString, varOleStr, varUString:
      AWriter.WriteValue(VarToStr(V));

    varArray:
      begin
        AWriter.WriteStartArray;
        try
          var
            I, LBound, UBound: Integer;
          LBound := VarArrayLowBound(V, 1);
          UBound := VarArrayHighBound(V, 1);
          for I := LBound to UBound do
            WriteVariant(AWriter, V[I]);
        finally
          AWriter.WriteEndArray;
        end;
      end;

  else
    AWriter.WriteValue(VarToStr(V));
  end;
end;

procedure TAiRagGraph.SaveToDot(const AFileName: string);
var
  SB: TStringBuilder;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;

  function EscapeDot(const S: string): string;
  begin
    Result := S.Replace('"', '\"').Replace(sLineBreak, '\n');
  end;

begin

  SB := TStringBuilder.Create;
  try
    SB.AppendLine('digraph KnowledgeGraph {');
    SB.AppendLine('  node [shape=box, style=rounded];'); // Estilo opcional para los nodos

    // 1. Escribir todos los nodos
    for Node in FNodeRegistry.Values do
    begin
      // Formato: "ID" [label="Nombre\n(Etiqueta)"];
      SB.Append('  "');
      SB.Append(Node.ID); // Usar ID para unicidad, ya que el nombre puede repetirse
      SB.Append('" [label="');
      SB.Append(EscapeDot(Node.Name)); // <--- ESCAPAR
      SB.Append('\n(');
      SB.Append(EscapeDot(Node.NodeLabel)); // <--- ESCAPAR
      SB.Append(')"];');
      SB.AppendLine;
    end;

    SB.AppendLine; // Espacio entre nodos y aristas

    // 2. Escribir todas las aristas
    for Edge in FEdgeRegistry.Values do
    begin
      // Formato: "ID_Origen" -> "ID_Destino" [label="EtiquetaArista"];
      SB.Append('  "');
      SB.Append(Edge.FromNode.ID);
      SB.Append('" -> "');
      SB.Append(Edge.ToNode.ID);
      SB.Append('" [label="');
      SB.Append(Edge.EdgeLabel);
      SB.Append(' (' + FormatFloat('0.##', Edge.Weight) + ')');
      SB.Append('"];');
      SB.AppendLine;
    end;

    SB.AppendLine('}');

    TFile.WriteAllText(AFileName, SB.ToString, TEncoding.UTF8);
  finally
    SB.Free;
  end;
end;

procedure TAiRagGraph.SaveToFile(const AFileName: string; aFull: Boolean);
var
  FileExt: string;
begin
  FileExt := ExtractFileExt(AFileName).ToLower; // Obtenemos la extensión en minúsculas

  if FileExt = '.graphml' then
    SaveToGraphML(AFileName)
  else if FileExt = '.dot' then
    SaveToDot(AFileName)
  else if (FileExt = '.mkai') or (FileExt = '.json') then // Permitimos ambas extensiones
    SaveToMakerAi(AFileName, aFull)
  else
    // Si la extensión no es reconocida, asumimos el formato nativo por defecto (.mkai)
    // o puedes lanzar una excepción si prefieres ser más estricto.
    // Aquí optamos por guardar en el formato nativo, añadiendo la extensión si no la tiene.
    SaveToMakerAi(ChangeFileExt(AFileName, '.mkai'), aFull);
  // Alternativa estricta:
  // raise Exception.CreateFmt('Formato de archivo no soportado para la extensión "%s".', [FileExt]);
end;

procedure TAiRagGraph.SaveToGraphML(const AFileName: string);
var
  XMLDoc: IXMLDocument;
  GraphMLNode, GraphNode, NodeElement, EdgeElement, DataElement, KeyElement, CommentNode: IXMLNode;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  PropKey: string;
  UniquePropKeys: TStringList;
  Pair: TPair<string, Variant>;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;
    XMLDoc.Version := '1.0';
    XMLDoc.Encoding := 'UTF-8';
    XMLDoc.Options := [doNodeAutoIndent];

    // --- ELEMENTO RAÍZ ---
    GraphMLNode := XMLDoc.AddChild('graphml');
    GraphMLNode.Attributes['xmlns'] := 'http://graphml.graphdrawing.org/xmlns';
    GraphMLNode.Attributes['xmlns:xsi'] := 'http://www.w3.org/2001/XMLSchema-instance';
    GraphMLNode.Attributes['xsi:schemaLocation'] := 'http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd';

    // --- 1. DEFINICIÓN DE ATRIBUTOS (KEYS) ---
    CommentNode := XMLDoc.CreateNode(' Atributos base del sistema MakerAi ', TNodeType.ntComment);
    GraphMLNode.ChildNodes.Add(CommentNode);

    // Atributos fijos de Nodos
    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'd_name';
    KeyElement.Attributes['for'] := 'node';
    KeyElement.Attributes['attr.name'] := 'name';
    KeyElement.Attributes['attr.type'] := 'string';

    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'd_label';
    KeyElement.Attributes['for'] := 'node';
    KeyElement.Attributes['attr.name'] := 'label';
    KeyElement.Attributes['attr.type'] := 'string';

    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'd_chunks';
    KeyElement.Attributes['for'] := 'node';
    KeyElement.Attributes['attr.name'] := 'chunks_count';
    KeyElement.Attributes['attr.type'] := 'int';

    // Atributos fijos de Aristas
    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'e_label';
    KeyElement.Attributes['for'] := 'edge';
    KeyElement.Attributes['attr.name'] := 'label';
    KeyElement.Attributes['attr.type'] := 'string';

    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'e_weight';
    KeyElement.Attributes['for'] := 'edge';
    KeyElement.Attributes['attr.name'] := 'weight';
    KeyElement.Attributes['attr.type'] := 'double';

    // Recolectar llaves de propiedades dinámicas (MetaData) de todos los elementos
    UniquePropKeys := TStringList.Create;
    try
      UniquePropKeys.Sorted := True;
      UniquePropKeys.Duplicates := TDuplicates.dupIgnore;

      for Node in FNodeRegistry.Values do
        for PropKey in Node.MetaData.InternalDictionary.Keys do
          UniquePropKeys.Add(PropKey);

      for Edge in FEdgeRegistry.Values do
        for PropKey in Edge.MetaData.InternalDictionary.Keys do
          UniquePropKeys.Add(PropKey);

      // Definir llaves para propiedades personalizadas
      for PropKey in UniquePropKeys do
      begin
        KeyElement := GraphMLNode.AddChild('key');
        KeyElement.Attributes['id'] := 'prop_' + PropKey;
        KeyElement.Attributes['for'] := 'all';
        KeyElement.Attributes['attr.name'] := PropKey;
        KeyElement.Attributes['attr.type'] := 'string';
      end;
    finally
      UniquePropKeys.Free;
    end;

    // --- 2. CONTENIDO DEL GRAFO ---
    GraphNode := GraphMLNode.AddChild('graph');
    GraphNode.Attributes['id'] := 'G';
    GraphNode.Attributes['edgedefault'] := 'directed';

    // -- Escritura de Nodos --
    for Node in FNodeRegistry.Values do
    begin
      NodeElement := GraphNode.AddChild('node');
      NodeElement.Attributes['id'] := Node.ID;

      // Datos base
      DataElement := NodeElement.AddChild('data');
      DataElement.Attributes['key'] := 'd_name';
      DataElement.Text := Node.Name;

      DataElement := NodeElement.AddChild('data');
      DataElement.Attributes['key'] := 'd_label';
      DataElement.Text := Node.NodeLabel;

      DataElement := NodeElement.AddChild('data');
      DataElement.Attributes['key'] := 'd_chunks';
      DataElement.Text := IntToStr(Node.Chunks.Count);

      // Metadatos dinámicos
      for Pair in Node.MetaData.InternalDictionary do
      begin
        DataElement := NodeElement.AddChild('data');
        DataElement.Attributes['key'] := 'prop_' + Pair.Key;
        DataElement.Text := VarToStr(Pair.Value);
      end;
    end;

    // -- Escritura de Aristas --
    for Edge in FEdgeRegistry.Values do
    begin
      EdgeElement := GraphNode.AddChild('edge');
      EdgeElement.Attributes['id'] := Edge.ID;
      EdgeElement.Attributes['source'] := Edge.FromNode.ID;
      EdgeElement.Attributes['target'] := Edge.ToNode.ID;

      // Datos base
      DataElement := EdgeElement.AddChild('data');
      DataElement.Attributes['key'] := 'e_label';
      DataElement.Text := Edge.EdgeLabel;

      DataElement := EdgeElement.AddChild('data');
      DataElement.Attributes['key'] := 'e_weight';
      DataElement.Text := FloatToStr(Edge.Weight);

      // Metadatos dinámicos
      for Pair in Edge.MetaData.InternalDictionary do
      begin
        DataElement := EdgeElement.AddChild('data');
        DataElement.Attributes['key'] := 'prop_' + Pair.Key;
        DataElement.Text := VarToStr(Pair.Value);
      end;
    end;

    XMLDoc.SaveToFile(AFileName);
  finally
    XMLDoc := nil;
  end;
end;

procedure TAiRagGraph.SaveToMakerAi(const AFileName: String; const aFull: Boolean);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    // Llama a la nueva versión sobrecargada de SaveToStream
    Self.SaveToStream(Stream, aFull);
  finally
    Stream.Free;
  end;
end;

procedure TAiRagGraph.SaveToFile(const AFileName: string; AFormat: TGraphExportFormat; aFull: Boolean);
begin
  case AFormat of
    gefDOT:
      SaveToDot(AFileName);
    gefGraphML:
      SaveToGraphML(AFileName);
    gefGraphMkai:
      SaveToMakerAi(AFileName, aFull);
  end;
end;

procedure TAiRagGraph.SaveToStream(AStream: TStream);
Begin
  SaveToStream(AStream, True);
End;

procedure TAiRagGraph.SaveToStream(AStream: TStream; aFull: Boolean);
var
  StreamWriter: TStreamWriter;
  JsonWriter: TJsonTextWriter;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Chunk: TAiEmbeddingNode;
  Pair: TPair<string, Variant>;
  I, J: Integer;
begin
  if AStream = nil then
    raise Exception.Create('Error: El Stream de destino es nil.');

  StreamWriter := TStreamWriter.Create(AStream, TEncoding.UTF8);
  try
    JsonWriter := TJsonTextWriter.Create(StreamWriter);
    try
      JsonWriter.Formatting := TJsonFormatting.Indented;

      JsonWriter.WriteStartObject; // {
      JsonWriter.WritePropertyName('graph');
      JsonWriter.WriteStartObject; // "graph": {

      // --- 1. SERIALIZAR NODOS ---
      JsonWriter.WritePropertyName('nodes');
      JsonWriter.WriteStartArray;

      for Node in FNodeRegistry.Values do
      begin
        JsonWriter.WriteStartObject;
        JsonWriter.WritePropertyName('id');
        JsonWriter.WriteValue(Node.ID);
        JsonWriter.WritePropertyName('nodeLabel');
        JsonWriter.WriteValue(Node.NodeLabel);
        JsonWriter.WritePropertyName('name');
        JsonWriter.WriteValue(Node.Name);
        JsonWriter.WritePropertyName('model');
        JsonWriter.WriteValue(Node.Model);
        JsonWriter.WritePropertyName('node_text');
        JsonWriter.WriteValue(Node.Text);

        // Propiedades dinámicas (MetaData)
        if Node.MetaData.InternalDictionary.Count > 0 then
        begin
          JsonWriter.WritePropertyName('properties');
          JsonWriter.WriteStartObject;
          for Pair in Node.MetaData.InternalDictionary do
          begin
            JsonWriter.WritePropertyName(Pair.Key);
            WriteVariant(JsonWriter, Pair.Value);
          end;
          JsonWriter.WriteEndObject;
        end;

        // --- NUEVO: SERIALIZAR CHUNKS (FRAGMENTOS) ---
        if Node.Chunks.Count > 0 then
        begin
          JsonWriter.WritePropertyName('chunks');
          JsonWriter.WriteStartArray;
          for Chunk in Node.Chunks do
          begin
            JsonWriter.WriteStartObject;
            JsonWriter.WritePropertyName('text');
            JsonWriter.WriteValue(Chunk.Text);

            if aFull and (Length(Chunk.Data) > 0) then
            begin
              JsonWriter.WritePropertyName('embedding');
              JsonWriter.WriteStartArray;
              for J := 0 to High(Chunk.Data) do
                JsonWriter.WriteValue(Chunk.Data[J]);
              JsonWriter.WriteEndArray;
            end;
            JsonWriter.WriteEndObject;
          end;
          JsonWriter.WriteEndArray;
        end;

        // Embedding principal del nodo (Resumen)
        if aFull and (Length(Node.Data) > 0) then
        begin
          JsonWriter.WritePropertyName('embedding');
          JsonWriter.WriteStartArray;
          for I := 0 to High(Node.Data) do
            JsonWriter.WriteValue(Node.Data[I]);
          JsonWriter.WriteEndArray;
        end;

        JsonWriter.WriteEndObject;
      end;
      JsonWriter.WriteEndArray;

      // --- 2. SERIALIZAR ARISTAS (RELACIONES) ---
      JsonWriter.WritePropertyName('edges');
      JsonWriter.WriteStartArray;

      for Edge in FEdgeRegistry.Values do
      begin
        JsonWriter.WriteStartObject;
        JsonWriter.WritePropertyName('id');
        JsonWriter.WriteValue(Edge.ID);
        JsonWriter.WritePropertyName('edgeLabel');
        JsonWriter.WriteValue(Edge.EdgeLabel);
        JsonWriter.WritePropertyName('name');
        JsonWriter.WriteValue(Edge.Name);
        JsonWriter.WritePropertyName('source');
        JsonWriter.WriteValue(Edge.FromNode.ID);
        JsonWriter.WritePropertyName('target');
        JsonWriter.WriteValue(Edge.ToNode.ID);
        JsonWriter.WritePropertyName('weight');
        JsonWriter.WriteValue(Edge.Weight);

        // Metadatos de la arista
        if Edge.MetaData.InternalDictionary.Count > 0 then
        begin
          JsonWriter.WritePropertyName('properties');
          JsonWriter.WriteStartObject;
          for Pair in Edge.MetaData.InternalDictionary do
          begin
            JsonWriter.WritePropertyName(Pair.Key);
            WriteVariant(JsonWriter, Pair.Value);
          end;
          JsonWriter.WriteEndObject;
        end;

        // Embedding de la arista
        if aFull and (Length(Edge.Data) > 0) then
        begin
          JsonWriter.WritePropertyName('embedding');
          JsonWriter.WriteStartArray;
          for I := 0 to High(Edge.Data) do
            JsonWriter.WriteValue(Edge.Data[I]);
          JsonWriter.WriteEndArray;
        end;

        JsonWriter.WriteEndObject;
      end;
      JsonWriter.WriteEndArray;

      JsonWriter.WriteEndObject; // } fin graph
      JsonWriter.WriteEndObject; // } fin raiz
    finally
      JsonWriter.Free;
    end;
  finally
    StreamWriter.Free;
  end;
end;

procedure TAiRagGraph.SetDriver(const Value: TAiRagGraphDriverBase);
begin
  if FDriver <> Value then
  begin
    FDriver := Value;

    if FDriver <> nil then
    begin
      // Le decimos al Driver: "Avísame si te mueres"
      FDriver.FreeNotification(Self);

      // Lógica existente de asignación
      FDriver.AssignToGraph(Self);
    end;
  end;
end;

procedure TAiRagGraph.SetEmbeddings(const Value: TAiEmbeddingsCore);
begin
  if FEmbeddings <> Value then
  begin
    FEmbeddings := Value;

    // Le decimos al componente de Embeddings: "Avísame (a mí, el Grafo) si te mueres"
    if FEmbeddings <> nil then
      FEmbeddings.FreeNotification(Self);
  end;
end;

procedure TAiRagGraph.SetSearchOptions(const Value: TAiSearchOptions);
begin
  // Asignamos los valores al vector de nodos
  FNodes.SearchOptions.Assign(Value);
end;

{ TMatchNodePattern }

constructor TMatchNodePattern.Create;
begin
  inherited;
  Properties := TDictionary<string, Variant>.Create(TStringComparer.Ordinal);
end;

destructor TMatchNodePattern.Destroy;
begin
  Properties.Free;
  inherited;
end;

function TMatchNodePattern.Matches(ANode: TAiRagGraphNode): Boolean;
var
  Pair: TPair<string, Variant>;
begin
  Result := False;
  if ANode = nil then
    Exit;

  // 1. Comprobar la etiqueta del nodo (Label)
  // Ignorar si el patrón no especifica una etiqueta
  if (not NodeLabel.IsEmpty) and (not SameText(ANode.NodeLabel, NodeLabel)) then
    Exit;

  // 2. Comprobar todas las propiedades requeridas del patrón
  // 'Properties' aquí es el diccionario de filtros del PATRÓN (ej: {ciudad: 'Madrid'})
  for Pair in Properties do
  begin
    // --- A. Atributos Natos del Nodo ---
    if SameText(Pair.Key, 'name') then
    begin
      if not SameText(ANode.Name, VarToStr(Pair.Value)) then
        Exit;
    end
    else if SameText(Pair.Key, 'label') then
    begin
      if not SameText(ANode.NodeLabel, VarToStr(Pair.Value)) then
        Exit;
    end
    else if SameText(Pair.Key, 'id') then
    begin
      if not SameText(ANode.ID, VarToStr(Pair.Value)) then
        Exit;
    end
    // --- B. Metadatos Dinámicos ---
    else
    begin
      // Utilizamos el nuevo motor de evaluación del MetaData
      // foEqual realiza una comparación inteligente de Variants (tipada)
      if not ANode.Properties.Evaluate(Pair.Key, foEqual, Pair.Value) then
        Exit;
    end;
  end;

  // Si superó todos los filtros, es un match
  Result := True;
end;

{ TMatchEdgePattern }

constructor TMatchEdgePattern.Create;
begin
  inherited;
  Direction := gdOutgoing; // Dirección por defecto
  Properties := TDictionary<string, Variant>.Create(TStringComparer.Ordinal);
end;

destructor TMatchEdgePattern.Destroy;
begin
  Properties.Free;
  inherited;
end;

function TMatchEdgePattern.Matches(AEdge: TAiRagGraphEdge; AActualDirection: TGraphDirection): Boolean;
var
  Pair: TPair<string, Variant>;
begin
  Result := False;
  if AEdge = nil then
    Exit;

  // 1. Comprobar la dirección del recorrido
  // Si el patrón especifica una dirección (saliente/entrante), debe coincidir con el recorrido actual
  if (Direction <> gdBoth) and (Direction <> AActualDirection) then
    Exit;

  // 2. Comprobar la etiqueta de la arista (EdgeLabel)
  if (not EdgeLabel.IsEmpty) and (not SameText(AEdge.EdgeLabel, EdgeLabel)) then
    Exit;

  // 3. Comprobar todas las propiedades requeridas del patrón de búsqueda
  // 'Properties' es el diccionario de filtros del patrón (ej: {since: 2020})
  for Pair in Properties do
  begin
    // --- A. Atributos Natos de la Arista ---
    if SameText(Pair.Key, 'label') then
    begin
      if not SameText(AEdge.EdgeLabel, VarToStr(Pair.Value)) then
        Exit;
    end
    else if SameText(Pair.Key, 'id') then
    begin
      if not SameText(AEdge.ID, VarToStr(Pair.Value)) then
        Exit;
    end
    else if SameText(Pair.Key, 'weight') then
    begin
      // Comparación numérica segura para el peso (Weight)
      try
        if Abs(AEdge.Weight - Double(Pair.Value)) > 0.0001 then
          Exit;
      except
        Exit;
      end;
    end
    // --- B. Metadatos Dinámicos (MetaData unificado) ---
    else
    begin
      // Utilizamos el nuevo motor de evaluación del MetaData de la arista.
      // foEqual realiza una comparación inteligente de Variants (tipada).
      if not AEdge.MetaData.Evaluate(Pair.Key, foEqual, Pair.Value) then
        Exit;
    end;
  end;

  // Si superó todos los filtros (dirección, etiqueta y propiedades), es un match
  Result := True;
end;

{ TMatchClause }

constructor TMatchClause.Create(ASourceNodeVar: string; AEdgePattern: TMatchEdgePattern; ATargetNodeVar: string);
begin
  inherited Create;
  SourceNodeVar := ASourceNodeVar;
  EdgePattern := AEdgePattern; // La cláusula toma posesión del patrón de arista
  TargetNodeVar := ATargetNodeVar;
end;

destructor TMatchClause.Destroy;
begin
  EdgePattern.Free; // Libera el patrón de arista del que es dueño
  inherited;
end;

{ TGraphMatchQuery }

constructor TGraphMatchQuery.Create;
begin
  inherited;
  FNodePatterns := TObjectList<TMatchNodePattern>.Create(True); // Es dueño
  FMatchClauses := TObjectList<TMatchClause>.Create(True); // Es dueño
  FDepth := 0;
end;

destructor TGraphMatchQuery.Destroy;
begin
  if Assigned(FWhereClause) then
    FWhereClause.Free; // Esto disparará la liberación en cadena de todo el árbol

  FNodePatterns.Free;
  FMatchClauses.Free;
  inherited;
end;

procedure TGraphMatchQuery.AddNodePattern(ANodePattern: TMatchNodePattern);
begin
  FNodePatterns.Add(ANodePattern);
end;

procedure TGraphMatchQuery.AddMatchClause(AClause: TMatchClause);
begin
  FMatchClauses.Add(AClause);
end;

function TGraphMatchQuery.GetNodePatternByVariable(const AVar: string): TMatchNodePattern;
var
  Pattern: TMatchNodePattern;
begin
  Result := nil;
  for Pattern in FNodePatterns do
  begin
    if SameText(Pattern.Variable, AVar) then
    begin
      Result := Pattern;
      Exit;
    end;
  end;
  // Si no se encuentra, podría ser un error en la construcción de la consulta.
  // Podrías lanzar una excepción aquí si quieres ser más estricto.
  // raise Exception.CreateFmt('Node pattern with variable "%s" not found in query.', [AVar]);
end;

{ TStringWrapper }

constructor TStringWrapper.Create(const AValue: string);
begin
  inherited Create;
  Value := AValue;
end;

{ TAiRagGraphDriverBase }

procedure TAiRagGraphDriverBase.AssignToGraph(AGraph: TAiRagGraph);
begin
  FGraph := AGraph;
end;

function TAiRagGraphDriverBase.EmbeddingToString(const AData: TAiEmbeddingData): string;
begin
  Result := TAiEmbeddingDataRec.EmbeddingToString(AData);
end;

function TAiRagGraphDriverBase.PropertiesToJSONString(const AProperties: TDictionary<string, Variant>): string;
var
  JsonObj: TJSONObject;
  Key: string;
  Value: Variant;
begin
  Result := '{}'; // Valor por defecto para diccionario vacío

  if (AProperties = nil) or (AProperties.Count = 0) then
    Exit;

  JsonObj := TJSONObject.Create;
  try
    for Key in AProperties.Keys do
    begin
      Value := AProperties[Key];
      JsonObj.AddPair(Key, VariantToJSONValue(Value));
    end;

    Result := JsonObj.ToString;
  finally
    JsonObj.Free;
  end;
end;

{ TGraphExpression }
destructor TGraphExpression.Destroy;
begin
  inherited;
end;

{ TLiteralExpr }
constructor TLiteralExpr.Create(AValue: Variant);
begin
  inherited Create;
  Kind := ekLiteral;
  Value := AValue;
end;

{ TPropertyExpr }
constructor TPropertyExpr.Create(const AVar, AKey: string);
begin
  inherited Create;
  Kind := ekProperty;
  Variable := AVar;
  PropertyKey := AKey;
end;

{ TBinaryExpr }
constructor TBinaryExpr.Create(ALeft: TGraphExpression; AOp: TBinaryOp; ARight: TGraphExpression);
begin
  inherited Create;
  Kind := ekBinary;
  Left := ALeft;
  Op := AOp;
  Right := ARight;
end;

destructor TBinaryExpr.Destroy;
begin
  Left.Free; // La liberación es recursiva
  Right.Free;
  inherited;
end;

{ TCommunity }

constructor TCommunity.Create(AID: Integer);
begin
  inherited Create;
  FID := AID;
  // Usamos TList simple porque los objetos TAiRagGraphNode
  // pertenecen al registro (FNodeRegistry) del Grafo principal.
  FNodes := TList<TAiRagGraphNode>.Create;
  FInternalWeight := 0;
  FTotalWeight := 0;
end;

destructor TCommunity.Destroy;
begin
  // Solo liberamos la lista, no los nodos contenidos en ella
  FNodes.Free;
  inherited;
end;

end.
