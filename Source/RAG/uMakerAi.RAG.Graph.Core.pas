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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// Los grafos exportados se pueden visualizar con gephi y se descarga desde https://gephi.org/users/download/

unit uMakerAi.RAG.Graph.Core;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  System.Json, System.Variants, System.Json.Writers, System.Json.Types, System.IOUtils,
  System.Rtti, System.StrUtils,

  Xml.XMLDoc, Xml.XMLIntf, Xml.XMLDom,

  uMakerAi.Embeddings.Core, uMakerAi.Embeddings,
  // Incluimos tu unidad base de MakerAi
  uMakerAi.RAG.Vectors;

type

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
    FMatchClauses: TObjectList<TMatchClause>; // Es dueño de las cláusulas
    function GetNodePatternByVariable(const AVar: string): TMatchNodePattern;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNodePattern(ANodePattern: TMatchNodePattern);
    procedure AddMatchClause(AClause: TMatchClause);
    property NodePatterns: TObjectList<TMatchNodePattern> read FNodePatterns;
    property MatchClauses: TObjectList<TMatchClause> read FMatchClauses;
    property NodePatternByVariable[const AVar: string]: TMatchNodePattern read GetNodePatternByVariable;
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
    function SearchNodes(const APrompt: string; ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>; virtual; abstract;
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
    FProperties: TDictionary<string, Variant>;
    FOwnerGraph: TAiRagGraph;
    FID: string;
    FEdgesLoaded: Boolean;
    function GetIncomingEdges: TObjectList<TAiRagGraphEdge>;
    function GetOutgoingEdges: TObjectList<TAiRagGraphEdge>;
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

    property ID: string read FID write FID;
    property Name: string read FName write FName;
    property NodeLabel: string read FNodeLabel write FNodeLabel;
    property Properties: TDictionary<string, Variant> read FProperties;

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
    FProperties: TDictionary<string, Variant>;
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
    property Properties: TDictionary<string, Variant> read FProperties;
    property OwnerGraph: TAiRagGraph read FOwnerGraph;
  end;

  { TAiRagGraph }
  TAiRagGraph = class(TComponent)
  private
    FNodes: TAiRAGVector;
    FEdges: TAiRAGVector;
    FNodeRegistry: TDictionary<string, TAiRagGraphNode>;
    FEdgeRegistry: TDictionary<string, TAiRagGraphEdge>;
    FEmbeddings: TAiEmbeddingsCore;
    FInMemoryIndexType: TAiRagIndexType;
    FNodeLabelIndex: TDictionary<string, TList<TAiRagGraphNode>>;
    FNodeNameIndex: TDictionary<string, TAiRagGraphNode>;
    FUpdateCount: Integer;
    FDriver: TAiRagGraphDriverBase;

    function GetNodeCount: Integer;
    function GetEdgeCount: Integer;
    function GetNodesRAGVector: TAiRAGVector;
    function GetEdgesRAGVector: TAiRAGVector;
    procedure SetInMemoryIndexType(const Value: TAiRagIndexType);
    procedure RebuildIndexes;
    procedure SetDriver(const Value: TAiRagGraphDriverBase);
  protected
    procedure UnregisterNode(ANode: TAiRagGraphNode);
    procedure UnregisterEdge(AEdge: TAiRagGraphEdge);
    function ExpandNodeList(AInitialNodes: TList<TAiRagGraphNode>; ADepth: Integer): TArray<TAiRagGraphNode>;
    function GetContextualizedText(ASubgraphNodes: TArray<TAiRagGraphNode>): string;
    function InternalAddNode(ANode: TAiRagGraphNode; AShouldPersist: Boolean): TAiRagGraphNode;
    function InternalAddEdge(AEdge: TAiRagGraphEdge; AShouldPersist: Boolean): TAiRagGraphEdge;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function InternalHydrateNode(const ANodeData: TNodeDataRecord): TAiRagGraphNode;
    function InternalHydrateEdge(const AEdgeData: TEdgeDataRecord): TAiRagGraphEdge;

    function AddNode(AID, ALabel, AName: string): TAiRagGraphNode; overload;
    function AddNode(ANode: TAiRagGraphNode): TAiRagGraphNode; overload;

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

    function Search(const APrompt: string; const ADepth: Integer = 0; ALimit: Integer = 5; const APrecision: Double = 0.5): TArray<TAiRagGraphNode>;
    function SearchText(const APrompt: string; ADepth: Integer = 0; ShowProperties: Boolean = False; const ALimit: Integer = 3; const APrecision: Double = 0.5): string;

    function Query(const APlan: TQueryPlan; ADepth: Integer = 0; const ALimit: Integer = 5; const APrecision: Double = 0.5): TArray<TAiRagGraphNode>;

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

    property NodeCount: Integer read GetNodeCount;
    property EdgeCount: Integer read GetEdgeCount;
    property Nodes: TAiRAGVector read GetNodesRAGVector;
    property Edges: TAiRAGVector read GetEdgesRAGVector;
  Published
    property Embeddings: TAiEmbeddingsCore read FEmbeddings write FEmbeddings;
    Property InMemoryIndexType: TAiRagIndexType read FInMemoryIndexType write SetInMemoryIndexType;
    property Driver: TAiRagGraphDriverBase read FDriver write SetDriver;

  end;

  // Función Helper para convertir TJSONValue a Variant
function JSONValueToVariant(AJsonValue: TJSONValue): Variant;
function VariantToJSONValue(const AValue: Variant): TJSONValue;
function StringToEmbedding(const AVectorString: string): TAiEmbeddingData;
procedure JSONStringToProperties(const AJSONString: string; const AProperties: TDictionary<string, Variant>);

procedure Register;

implementation

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

procedure JSONStringToProperties(const AJSONString: string; const AProperties: TDictionary<string, Variant>);
var
  JsonValue: TJSONValue;
  JsonObj: TJSONObject;
  Pair: TJSONPair;
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
        AProperties.Add(Pair.JsonString.Value, JSONValueToVariant(Pair.JsonValue));
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
  FProperties := TDictionary<string, Variant>.Create;
  FEdgesLoaded := False;
end;

destructor TAiRagGraphNode.Destroy;
begin
  FInternalOutgoingEdges.Free;
  FInternalIncomingEdges.Free;
  FProperties.Free;
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

function TAiRagGraphNode.GetOutgoingEdges: TObjectList<TAiRagGraphEdge>;
begin
  EnsureEdgesAreLoaded;
  Result := FInternalOutgoingEdges;
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
var
  Pair: TPair<string, Variant>;
begin
  Result := TJSONObject.Create;
  for Pair in FProperties do
  begin
    // Esta es una simplificación. Una versión robusta necesitaría
    // un conversor de Variant a TJSONValue más completo.
    Result.AddPair(Pair.Key, VariantToJSONValue(Pair.Value));
  end;
end;

{ TAiRagGraphEdge }

constructor TAiRagGraphEdge.Create(AOwnerGraph: TAiRagGraph; ADim: Integer);
begin
  inherited Create(ADim);
  FOwnerGraph := AOwnerGraph;
  FProperties := TDictionary<string, Variant>.Create;
  FWeight := 1.0;
end;

destructor TAiRagGraphEdge.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TAiRagGraphEdge.PropertiesToJSON: TJSONObject;
var
  Pair: TPair<string, Variant>;
begin
  Result := TJSONObject.Create;
  for Pair in FProperties do
  begin
    Result.AddPair(Pair.Key, VariantToJSONValue(Pair.Value));
  end;
end;

{ TAiRagGraph }

constructor TAiRagGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodes := TAiRAGVector.Create(Self);
  FEdges := TAiRAGVector.Create(Self);
  FNodeRegistry := TDictionary<string, TAiRagGraphNode>.Create;
  FEdgeRegistry := TDictionary<string, TAiRagGraphEdge>.Create;
  FNodeLabelIndex := TDictionary < string, TList < TAiRagGraphNode >>.Create;
  FNodeNameIndex := TDictionary<string, TAiRagGraphNode>.Create;
  FInMemoryIndexType := TAIHNSWIndex;
  FNodes.InMemoryIndexType := FInMemoryIndexType;
  FEdges.InMemoryIndexType := FInMemoryIndexType;
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
  Edge: TAiRagGraphEdge;
  Pair: TPair<string, Variant>;
begin
  Result := TAiRagGraph.Create(nil); // Crear un nuevo grafo independiente
  Result.Embeddings := Self.Embeddings; // Compartir el motor de embeddings
  if Length(ANodes) = 0 then
    Exit;

  NodeSet := TDictionary<string, Boolean>.Create;
  try
    // Paso 1: Añadir todos los nodos al nuevo grafo y registrar sus IDs
    for Node in ANodes do
    begin
      NodeSet.Add(Node.ID, True);
      NewNode := Result.AddNode(Node.ID, Node.NodeLabel, Node.Name);
      // Copiar propiedades
      for Pair in Node.Properties do
        NewNode.Properties.Add(Pair.Key, Pair.Value);
      // Copiar embedding si existe
      if Length(Node.Data) > 0 then
      begin
        NewNode.SetDataLength(Length(Node.Data));
        Move(Node.Data[0], NewNode.Data[0], Length(Node.Data) * SizeOf(Single));
      end;
    end;

    // Paso 2: Iterar los nodos originales y añadir solo las aristas internas
    Result.BeginUpdate;
    try
      for Node in ANodes do
      begin
        for Edge in Node.OutgoingEdges do
        begin
          // Si el nodo destino también está en nuestro conjunto, añadir la arista
          if NodeSet.ContainsKey(Edge.ToNode.ID) then
          begin
            var
            FromNode := Result.FindNodeByID(Edge.FromNode.ID);
            var
            ToNode := Result.FindNodeByID(Edge.ToNode.ID);
            var
            NewEdge := Result.AddEdge(FromNode, ToNode, Edge.ID, Edge.EdgeLabel, Edge.Name, Edge.Weight);
            // Copiar propiedades
            for Pair in Edge.Properties do
              NewEdge.Properties.Add(Pair.Key, Pair.Value);
          end;
        end;
      end;
    finally
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

function TAiRagGraph.AddNode(AID, ALabel, AName: string): TAiRagGraphNode;
var
  NewNode: TAiRagGraphNode;
  Dim: Integer;
begin
  Result := Nil;

  // Determinar la dimensión del vector de embeddings.
  if FNodes.Dim > 0 then
    Dim := FNodes.Dim
  else
    Dim := 1536; // O tu valor por defecto

  NewNode := TAiRagGraphNode.Create(Self, Dim);
  NewNode.ID := AID;
  NewNode.NodeLabel := ALabel;
  NewNode.Name := AName;
  try
    // Llama al overload de abajo para hacer el trabajo real.
    Result := AddNode(NewNode);
  except
    // Si AddNode(NewNode) falla, NewNode no fue adoptado por el grafo,
    // así que debemos liberarlo aquí.
    if (Result <> NewNode) then
      NewNode.Free;
    raise;
  end;
end;

procedure TAiRagGraph.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// El overload que crea desde datos simples
function TAiRagGraph.AddEdge(AFromNode, AToNode: TAiRagGraphNode; AID, ALabel, AName: string): TAiRagGraphEdge;
begin
  Result := AddEdge(AFromNode, AToNode, AID, ALabel, AName, 1.0);
end;

// El overload que crea desde datos simples + peso
function TAiRagGraph.AddEdge(AFromNode, AToNode: TAiRagGraphNode; AID, ALabel, AName: string; AWeight: Double): TAiRagGraphEdge;
var
  NewEdge: TAiRagGraphEdge;
  Dim: Integer;
begin
  Result := Nil;

  if FEdges.Dim > 0 then
    Dim := FEdges.Dim
  else
    Dim := 1536; // O tu valor por defecto

  NewEdge := TAiRagGraphEdge.Create(Self, Dim);
  NewEdge.ID := AID;
  NewEdge.EdgeLabel := ALabel;
  NewEdge.Name := AName;
  NewEdge.Weight := AWeight;
  NewEdge.FromNode := AFromNode;
  NewEdge.ToNode := AToNode;

  try
    // Llamamos al overload principal, que a su vez llamará a InternalAddEdge
    Result := AddEdge(NewEdge);
  except
    if (Result <> NewEdge) then
      NewEdge.Free;
    raise;
  end;
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
  // Variables para la lógica en memoria
  Results: TList<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
  PropValue: Variant;
begin
  Result := [];

  // 1. Intentar delegar al Driver (BD)
  if Assigned(FDriver) then
  begin
    // El driver realiza la búsqueda en la BD, se encarga de la hidratación
    // y devuelve el array de nodos.
    Result := FDriver.FindNodesByProperty(AKey, AValue);

    // Si el driver devolvió algún resultado, salimos.
    if Length(Result) > 0 then
      Exit;

    // Si el driver devolvió un array vacío, pasamos al fallback en memoria (Paso 2).
  end;

  // 2. Fallback a la lógica en memoria
  Results := TList<TAiRagGraphNode>.Create;
  try
    for Node in FNodeRegistry.Values do
    begin
      // --- IMPORTANTE: Usar la lógica mejorada que maneja 'name' como caso especial ---
      if SameText(AKey, 'name') then
      begin
        if SameText(Node.Name, VarToStr(AValue)) then
          Results.Add(Node);
      end
      else
      begin
        if Node.Properties.TryGetValue(AKey, PropValue) then
        begin
          // La comparación de valor debe ser robusta (aquí se usa VarToStr y SameText)
          if SameText(VarToStr(PropValue), VarToStr(AValue)) then
            Results.Add(Node);
        end;
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
begin
  // Primero, comprobamos si ya existe.
  if FEdgeRegistry.TryGetValue(AEdgeData.ID, Result) then
    Exit;

  // ANTES de crear la arista, nos aseguramos de que sus nodos de origen
  // y destino existan en memoria (cargándolos desde la BD si es necesario).
  FromNode := Self.FindNodeByID(AEdgeData.SourceNodeID);
  ToNode := Self.FindNodeByID(AEdgeData.TargetNodeID);

  if (FromNode = nil) or (ToNode = nil) then
    Exit(nil); // No se puede crear la arista si faltan sus nodos.

  // Si todo está bien, creamos e hidratamos la arista.
  if FEdges.Dim > 0 then
    Dim := FEdges.Dim
  else
    Dim := 1536;
  NewEdge := TAiRagGraphEdge.Create(Self, Dim);
  try
    NewEdge.ID := AEdgeData.ID;
    NewEdge.EdgeLabel := AEdgeData.EdgeLabel;
    NewEdge.Name := AEdgeData.Name;
    NewEdge.Weight := AEdgeData.Weight;
    NewEdge.FromNode := FromNode;
    NewEdge.ToNode := ToNode;
    JSONStringToProperties(AEdgeData.PropertiesJSON, NewEdge.Properties);
    NewEdge.Data := StringToEmbedding(AEdgeData.EmbeddingStr);

    // Lo añadimos a memoria usando el método interno SIN persistir.
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
begin
  // Primero, comprobamos si ya existe, para no hacer trabajo de más.
  if FNodeRegistry.TryGetValue(ANodeData.ID, Result) then
    Exit;

  // Si no existe, lo creamos e hidratamos.
  if FNodes.Dim > 0 then
    Dim := FNodes.Dim
  else
    Dim := 1536;
  NewNode := TAiRagGraphNode.Create(Self, Dim);
  try
    NewNode.ID := ANodeData.ID;
    NewNode.NodeLabel := ANodeData.NodeLabel;
    NewNode.Name := ANodeData.Name;
    JSONStringToProperties(ANodeData.PropertiesJSON, NewNode.Properties);
    NewNode.Data := StringToEmbedding(ANodeData.EmbeddingStr);

    // Lo añadimos a memoria usando el método interno SIN persistir.
    Result := InternalAddNode(NewNode, False);

    // Si InternalAddNode devolvió un nodo diferente (condición de carrera),
    // liberamos el que acabamos de crear.
    if Result <> NewNode then
      NewNode.Free;
  except
    NewNode.Free;
    raise;
  end;
end;

function TAiRagGraph.Search(const APrompt: string; const ADepth: Integer; ALimit: Integer; const APrecision: Double): TArray<TAiRagGraphNode>;
var
  Handled: Boolean;
  VectorSearchResults: TAiRAGVector;
  InitialNodeList: TList<TAiRagGraphNode>;
  FoundItem: TAiEmbeddingNode;
begin
  // Inicializamos el resultado como un array vacío.
  SetLength(Result, 0);
  Handled := False;

  // Paso 1: Intentar delegar la búsqueda completa (semántica + estructural).
 if Assigned(FDriver) then
  begin
    // El manejador es responsable de:
    // 1. Obtener el embedding del APrompt.
    // 2. Realizar la búsqueda por similaridad en la BD.
    // 3. Si ADepth > 0, realizar la expansión estructural en la BD.
    // 4. "Hidratar" todos los nodos y aristas del subgrafo resultante.
    // 5. Asignar el array de nodos final a 'Result'.
    Result := FDriver.SearchNodes(APrompt, ADepth, ALimit, APrecision);
    Exit;
  end;

  // Paso 2: Si la búsqueda no fue manejada, ejecutar la lógica en memoria.
  if not Handled then
  begin
    // --- Lógica original de búsqueda en memoria ---

    // Asegurarse de tener el motor de embeddings
    if not Assigned(FEmbeddings) then
    begin
      if Assigned(FNodes) and Assigned(FNodes.Embeddings) then
        FEmbeddings := FNodes.Embeddings
      else
        raise Exception.Create('Embeddings property is not assigned to the graph or its underlying node vector.');
    end;

    if FNodes.Count = 0 then
      Exit; // Salir con un array vacío

    FNodes.Embeddings := FEmbeddings;

    // 1. Búsqueda Vectorial en memoria
    VectorSearchResults := FNodes.Search(APrompt, ALimit, APrecision);
    try
      InitialNodeList := TList<TAiRagGraphNode>.Create;
      try
        // 2. Anclaje al Grafo y Conversión
        for FoundItem in VectorSearchResults.Items do
        begin
          if FoundItem is TAiRagGraphNode then
            InitialNodeList.Add(FoundItem as TAiRagGraphNode);
        end;

        // 3. Expansión Contextual en memoria (si se solicita)
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

  // Al final, 'Result' contiene el array de nodos, ya sea desde la BD o desde la memoria.
end;

function TAiRagGraph.SearchText(const APrompt: string; ADepth: Integer = 0; ShowProperties: Boolean = False; const ALimit: Integer = 3; const APrecision: Double = 0.5): string;
var
  FoundNodes: TArray<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  ContextBuilder: TStringBuilder;
  PropPair: TPair<string, Variant>;
begin
  // 1. Obtener los nodos semánticamente más relevantes
  FoundNodes := Self.Search(APrompt, ADepth, ALimit, APrecision);

  // 2. Construir un contexto textual a partir de los nodos y sus relaciones directas
  ContextBuilder := TStringBuilder.Create;
  try
    ContextBuilder.AppendLine('Contexto extraído del grafo de conocimiento:');
    for Node in FoundNodes do
    begin
      // Añadir información sobre el nodo encontrado
      ContextBuilder.Append('- Se encontró la entidad: ');
      ContextBuilder.Append(Node.Name);
      ContextBuilder.Append(' (Tipo: ');
      ContextBuilder.Append(Node.NodeLabel);
      ContextBuilder.AppendLine(').');

      // Mostrar propiedades del nodo si se solicita
      if ShowProperties and (Node.Properties <> nil) and (Node.Properties.Count > 0) then
      begin
        ContextBuilder.AppendLine('  Propiedades de la entidad:');
        for PropPair in Node.Properties do
        begin
          ContextBuilder.Append('    • ');
          ContextBuilder.Append(PropPair.Key);
          ContextBuilder.Append(': ');
          try
            ContextBuilder.AppendLine(VarToStr(PropPair.Value));
          except
            ContextBuilder.AppendLine('<valor no legible>');
          end;
        end;
      end;

      // Añadir sus relaciones directas (sus "hechos")
      if (Node.OutgoingEdges <> nil) and (Node.OutgoingEdges.Count > 0) then
      begin
        ContextBuilder.AppendLine('  Hechos conocidos sobre esta entidad:');
        for Edge in Node.OutgoingEdges do
        begin
          ContextBuilder.Append('    * ');
          ContextBuilder.Append(Node.Name);
          ContextBuilder.Append(' ');
          ContextBuilder.Append(Edge.EdgeLabel); // El verbo
          ContextBuilder.Append(' ');
          ContextBuilder.Append(Edge.ToNode.Name);
          ContextBuilder.AppendLine('.');

          // Mostrar propiedades de la relación si se solicita
          if ShowProperties and (Edge.Properties <> nil) and (Edge.Properties.Count > 0) then
          begin
            ContextBuilder.AppendLine('      Propiedades de la relación:');
            for PropPair in Edge.Properties do
            begin
              ContextBuilder.Append('        • ');
              ContextBuilder.Append(PropPair.Key);
              ContextBuilder.Append(': ');
              try
                ContextBuilder.AppendLine(VarToStr(PropPair.Value));
              except
                ContextBuilder.AppendLine('<valor no legible>');
              end;
            end;
          end;
        end;
      end;
    end;
    Result := ContextBuilder.ToString;
  finally
    ContextBuilder.Free;
  end;
end;

procedure TAiRagGraph.LoadFromStream(AStream: TStream);
var
  JsonString: string;
  Root, GraphObject: TJSONObject;
  NodesArray, EdgesArray: TJSONArray;
  NodeValue, EdgeValue: TJSONValue;
  NodeObject, EdgeObject, PropertiesObject: TJSONObject;
  NodeID, NodeLabel, NodeName: string;
  EdgeID, EdgeLabel, EdgeName, SourceID, TargetID: string;
  NewNode: TAiRagGraphNode;
  NewEdge: TAiRagGraphEdge;
  FromNode, ToNode: TAiRagGraphNode;
  Pair: TJSONPair;
  EmbeddingArray: TJSONArray;
  I: Integer;
begin
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  // 0. Preparación: Limpiar el grafo actual y leer el stream a un string
  Clear;
  AStream.Position := 0;
  JsonString := TStreamReader.Create(AStream, TEncoding.UTF8).ReadToEnd;

  Root := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
  if Root = nil then
    raise Exception.Create('Invalid JSON format.');

  BeginUpdate;
  try
    GraphObject := Root.GetValue<TJSONObject>('graph');
    if GraphObject = nil then
      raise Exception.Create('JSON is missing "graph" root object.');

    // --- PRIMERA PASADA: CREAR TODOS LOS NODOS ---
    NodesArray := GraphObject.GetValue<TJSONArray>('nodes');
    if NodesArray <> nil then
    begin
      for NodeValue in NodesArray do
      begin
        if not(NodeValue is TJSONObject) then
          Continue;
        NodeObject := NodeValue as TJSONObject;

        NodeID := NodeObject.GetValue<string>('id');
        NodeLabel := NodeObject.GetValue<string>('nodeLabel');
        NodeName := NodeObject.GetValue<string>('name', ''); // Name es opcional

        if NodeID.IsEmpty or NodeLabel.IsEmpty then
          Continue; // Nodo mal formado

        NewNode := Self.AddNode(NodeID, NodeLabel, NodeName);

        // Procesar propiedades
        if NodeObject.TryGetValue<TJSONObject>('properties', PropertiesObject) then
        begin
          for Pair in PropertiesObject do
          begin
            NewNode.Properties.Add(Pair.JsonString.Value, JSONValueToVariant(Pair.JsonValue));
          end;
        end;

        // Procesar embedding
        if NodeObject.TryGetValue<TJSONArray>('embedding', EmbeddingArray) then
        begin
          NewNode.SetDataLength(EmbeddingArray.Count);
          for I := 0 to EmbeddingArray.Count - 1 do
          begin
            NewNode.Data[I] := StrToFloat(EmbeddingArray.Items[I].Value);
          end;
        end;
      end;
    end;

    // --- SEGUNDA PASADA: CREAR TODAS LAS ARISTAS ---
    EdgesArray := GraphObject.GetValue<TJSONArray>('edges');
    if EdgesArray <> nil then
    begin
      for EdgeValue in EdgesArray do
      begin
        if not(EdgeValue is TJSONObject) then
          Continue;
        EdgeObject := EdgeValue as TJSONObject;

        EdgeID := EdgeObject.GetValue<string>('id');
        EdgeLabel := EdgeObject.GetValue<string>('edgeLabel');
        EdgeName := EdgeObject.GetValue<string>('name', '');
        SourceID := EdgeObject.GetValue<string>('source');
        TargetID := EdgeObject.GetValue<string>('target');

        if EdgeID.IsEmpty or EdgeLabel.IsEmpty or SourceID.IsEmpty or TargetID.IsEmpty then
          Continue; // Arista mal formada

        // Buscar los nodos de origen y destino (ya deben existir)
        FromNode := Self.FindNodeByID(SourceID);
        ToNode := Self.FindNodeByID(TargetID);

        if (FromNode <> nil) and (ToNode <> nil) then
        begin
          NewEdge := Self.AddEdge(FromNode, ToNode, EdgeID, EdgeLabel, EdgeName);
          NewEdge.Weight := EdgeObject.GetValue<Double>('weight', 1.0);

          // Procesar propiedades
          if EdgeObject.TryGetValue<TJSONObject>('properties', PropertiesObject) then
          begin
            for Pair in PropertiesObject do
            begin
              NewEdge.Properties.Add(Pair.JsonString.Value, JSONValueToVariant(Pair.JsonValue));
            end;
          end;

          // Procesar embedding
          if EdgeObject.TryGetValue<TJSONArray>('embedding', EmbeddingArray) then
          begin
            NewEdge.SetDataLength(EmbeddingArray.Count);
            for I := 0 to EmbeddingArray.Count - 1 do
            begin
              NewEdge.Data[I] := StrToFloat(EmbeddingArray.Items[I].Value);
            end;
          end;
        end;
      end;
    end;

  finally
    EndUpdate;
    Root.Free;
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
      Results.Add(TMatchState.Create(ACurrentState));
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
  if (AQuery = nil) or (AQuery.MatchClauses.Count = 0) then
    Exit;

  Results := TObjectList < TDictionary < string, TObject >>.Create(True);
  try
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

    // --- PASO 2: EXPANSIÓN DEL SUBGRAFO (si ADepth > 0) ---
    if (ADepth > 0) and (Results.Count > 0) then
    begin
      // 1. Recolectar nodos semilla únicos de los resultados del match.
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

        if SeedNodesSet.Count = 0 then
          Exit;

        // 2. Expandir el vecindario. `ExpandNodeList` ya devuelve nodos únicos.
        ExpandedNodesArray := Self.ExpandNodeList(TList<TAiRagGraphNode>.Create(SeedNodesSet.Keys), ADepth);
      finally
        SeedNodesSet.Free;
      end;

      // 3. Construir la salida del subgrafo garantizando unicidad.
      ExpandedResults := TObjectList < TDictionary < string, TObject >>.Create(True);
      EdgeSet := TDictionary<TAiRagGraphEdge, Boolean>.Create;
      try
        // Añadir cada nodo ÚNICO del subgrafo a la salida.
        for Node in ExpandedNodesArray do
        begin
          Dict := TDictionary<string, TObject>.Create;
          Dict.Add('type', TStringWrapper.Create('node'));
          Dict.Add('element', Node);
          ExpandedResults.Add(Dict);
        end;

        // Recolectar todas las aristas INTERNAS y ÚNICAS.
        var
        NodeSet := TDictionary<TAiRagGraphNode, Boolean>.Create;
        try
          for Node in ExpandedNodesArray do // Poblar el set para búsquedas rápidas
            NodeSet.Add(Node, True);

          for Node in ExpandedNodesArray do
          begin
            for Edge in Node.OutgoingEdges do
            begin
              // Si el nodo destino está en el subgrafo, es una arista interna.
              // El `EdgeSet` previene que la misma arista se añada dos veces.
              if NodeSet.ContainsKey(Edge.ToNode) then
                EdgeSet.AddOrSetValue(Edge, True);
            end;
          end;
        finally
          NodeSet.Free;
        end;

        // Añadir cada arista ÚNICA a la salida.
        for Edge in EdgeSet.Keys do
        begin
          Dict := TDictionary<string, TObject>.Create;
          Dict.Add('type', TStringWrapper.Create('edge'));
          Dict.Add('element', Edge);
          ExpandedResults.Add(Dict);
        end;

        // Reemplazar los resultados originales con la nueva lista del subgrafo.
        Results.Clear;
        for Dict in ExpandedResults do
          Results.Add(Dict);
        ExpandedResults.OwnsObjects := False;
      finally
        EdgeSet.Free;
        ExpandedResults.Free;
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
begin
  if (ASurvivingNode = nil) or (ASubsumedNode = nil) or (ASurvivingNode = ASubsumedNode) then
    Exit;

  BeginUpdate;
  try
    // 1. Re-conectar las aristas entrantes del nodo subsumido
    EdgesToMove := ASubsumedNode.IncomingEdges.ToArray;
    for Edge in EdgesToMove do
    begin
      // Crear una nueva arista o modificar la existente
      // (simplificado: modificamos el puntero 'ToNode')
      // ¡CUIDADO! Esto modifica la arista original. Dependiendo del caso,
      // podrías querer crear una nueva y borrar la vieja.
      Edge.ToNode := ASurvivingNode;
      ASurvivingNode.AddIncomingEdge(Edge);
    end;

    // 2. Re-conectar las aristas salientes
    EdgesToMove := ASubsumedNode.OutgoingEdges.ToArray;
    for Edge in EdgesToMove do
    begin
      Edge.FromNode := ASurvivingNode;
      ASurvivingNode.AddOutgoingEdge(Edge);
    end;

    // 3. Fusionar propiedades
    for Pair in ASubsumedNode.Properties do
    begin
      case APropertyMergeStrategy of
        msAddNewOnly:
          if not ASurvivingNode.Properties.ContainsKey(Pair.Key) then
            ASurvivingNode.Properties.Add(Pair.Key, Pair.Value);
        msOverwrite:
          ASurvivingNode.Properties.AddOrSetValue(Pair.Key, Pair.Value);
        // msKeepExisting: no hacer nada.
      end;
    end;

    // 4. Eliminar el nodo subsumido
    DeleteNode(ASubsumedNode); // Esto se encargará de limpiar las referencias viejas

  finally
    EndUpdate; // Reconstruye índices después de todos los cambios
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
begin
  case VarType(AValue) of
    varEmpty, varNull:
      AWriter.WriteNull;
    varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord:
      AWriter.WriteValue(Integer(AValue));
    varInt64, varUInt64:
      AWriter.WriteValue(Int64(AValue));
    varSingle, varDouble, varCurrency: // , varExtended:
      AWriter.WriteValue(Double(AValue));
    varDate:
      AWriter.WriteValue(TDateTime(AValue));
    varBoolean:
      AWriter.WriteValue(Boolean(AValue));
  else // varString, varOleStr, varUString, y otros que se puedan convertir a string
    AWriter.WriteValue(VarToStr(AValue));
  end;
end;

procedure TAiRagGraph.SaveToDot(const AFileName: string);
var
  SB: TStringBuilder;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
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
      SB.Append(Node.Name);
      SB.Append('\n(');
      SB.Append(Node.NodeLabel);
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

    // Crear el elemento raíz <graphml>
    GraphMLNode := XMLDoc.AddChild('graphml');
    GraphMLNode.Attributes['xmlns'] := 'http://graphml.graphdrawing.org/xmlns';
    GraphMLNode.Attributes['xmlns:xsi'] := 'http://www.w3.org/2001/XMLSchema-instance';
    GraphMLNode.Attributes['xsi:schemaLocation'] := 'http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd';

    // --- 1. Definir las "claves" de los atributos (propiedades) ---
    // Forma correcta de añadir un comentario
    CommentNode := XMLDoc.CreateNode(' Attribute definitions ', TNodeType.ntComment);
    GraphMLNode.ChildNodes.Add(CommentNode);

    // Clave para name del nodo
    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'd_name';
    KeyElement.Attributes['for'] := 'node';
    KeyElement.Attributes['attr.name'] := 'name';
    KeyElement.Attributes['attr.type'] := 'string';

    // Clave para label del nodo
    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'd_label';
    KeyElement.Attributes['for'] := 'node';
    KeyElement.Attributes['attr.name'] := 'label';
    KeyElement.Attributes['attr.type'] := 'string';

    // Clave para label de la arista
    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'e_label';
    KeyElement.Attributes['for'] := 'edge';
    KeyElement.Attributes['attr.name'] := 'label';
    KeyElement.Attributes['attr.type'] := 'string';

    // Clave para el peso de la arista
    KeyElement := GraphMLNode.AddChild('key');
    KeyElement.Attributes['id'] := 'e_weight';
    KeyElement.Attributes['for'] := 'edge';
    KeyElement.Attributes['attr.name'] := 'weight';
    KeyElement.Attributes['attr.type'] := 'double';

    // Recolectar todas las claves de propiedades personalizadas
    UniquePropKeys := TStringList.Create;
    try
      UniquePropKeys.Sorted := True;
      UniquePropKeys.Duplicates := TDuplicates.dupIgnore;

      for Node in FNodeRegistry.Values do
        if Node.Properties <> nil then
          for PropKey in Node.Properties.Keys do
            UniquePropKeys.Add(PropKey);

      for Edge in FEdgeRegistry.Values do
        if Edge.Properties <> nil then
          for PropKey in Edge.Properties.Keys do
            UniquePropKeys.Add(PropKey);

      // Definir cada propiedad personalizada
      for PropKey in UniquePropKeys do
      begin
        KeyElement := GraphMLNode.AddChild('key');
        KeyElement.Attributes['id'] := 'prop_' + PropKey;
        KeyElement.Attributes['for'] := 'all'; // Para nodos y aristas
        KeyElement.Attributes['attr.name'] := PropKey;
        KeyElement.Attributes['attr.type'] := 'string'; // Simplificación: todo a string
      end;
    finally
      UniquePropKeys.Free;
    end;

    // --- 2. Escribir el grafo ---
    GraphNode := GraphMLNode.AddChild('graph');
    GraphNode.Attributes['id'] := 'G';
    GraphNode.Attributes['edgedefault'] := 'directed';

    // -- Nodos --
    CommentNode := XMLDoc.CreateNode(' Nodes ', TNodeType.ntComment);
    GraphNode.ChildNodes.Add(CommentNode);

    for Node in FNodeRegistry.Values do
    begin
      NodeElement := GraphNode.AddChild('node');
      NodeElement.Attributes['id'] := Node.ID;

      // Atributo name
      DataElement := NodeElement.AddChild('data');
      DataElement.Attributes['key'] := 'd_name';
      DataElement.Text := Node.Name;

      // Atributo label
      DataElement := NodeElement.AddChild('data');
      DataElement.Attributes['key'] := 'd_label';
      DataElement.Text := Node.NodeLabel;

      // Propiedades personalizadas del nodo
      if Node.Properties <> nil then
      begin
        for Pair in Node.Properties do
        begin
          DataElement := NodeElement.AddChild('data');
          DataElement.Attributes['key'] := 'prop_' + Pair.Key;
          DataElement.Text := VarToStr(Pair.Value);
        end;
      end;
    end;

    // -- Aristas --
    CommentNode := XMLDoc.CreateNode(' Edges ', TNodeType.ntComment);
    GraphNode.ChildNodes.Add(CommentNode);

    for Edge in FEdgeRegistry.Values do
    begin
      EdgeElement := GraphNode.AddChild('edge');
      EdgeElement.Attributes['id'] := Edge.ID;
      EdgeElement.Attributes['source'] := Edge.FromNode.ID;
      EdgeElement.Attributes['target'] := Edge.ToNode.ID;

      // Atributo label
      DataElement := EdgeElement.AddChild('data');
      DataElement.Attributes['key'] := 'e_label';
      DataElement.Text := Edge.EdgeLabel;

      // Atributo weight
      DataElement := EdgeElement.AddChild('data');
      DataElement.Attributes['key'] := 'e_weight';
      DataElement.Text := FloatToStr(Edge.Weight);

      // Propiedades personalizadas de la arista
      if Edge.Properties <> nil then
      begin
        for Pair in Edge.Properties do
        begin
          DataElement := EdgeElement.AddChild('data');
          DataElement.Attributes['key'] := 'prop_' + Pair.Key;
          DataElement.Text := VarToStr(Pair.Value);
        end;
      end;
    end;

    // Guardar el documento XML
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
  JsonWriter: TJsonWriter;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Pair: TPair<string, Variant>;
begin
  if AStream = nil then
    raise Exception.Create('Stream cannot be nil.');

  StreamWriter := TStreamWriter.Create(AStream, TEncoding.UTF8);
  try
    JsonWriter := TJsonTextWriter.Create(StreamWriter);
    try
      if JsonWriter is TJsonTextWriter then
        TJsonTextWriter(JsonWriter).Formatting := TJsonFormatting.Indented;

      JsonWriter.WriteStartObject; // {
      JsonWriter.WritePropertyName('graph');
      JsonWriter.WriteStartObject; // "graph": {

      // 1. Serializar Nodos
      JsonWriter.WritePropertyName('nodes');
      JsonWriter.WriteStartArray; // "nodes": [
      for Node in FNodeRegistry.Values do
      begin
        JsonWriter.WriteStartObject;
        // ... (Escribir id, nodeLabel, name, properties - sin cambios)
        JsonWriter.WritePropertyName('id');
        JsonWriter.WriteValue(Node.ID);
        JsonWriter.WritePropertyName('nodeLabel');
        JsonWriter.WriteValue(Node.NodeLabel);
        if not Node.Name.IsEmpty then
        begin
          JsonWriter.WritePropertyName('name');
          JsonWriter.WriteValue(Node.Name);
        end;
        if Node.Properties.Count > 0 then
        begin
          JsonWriter.WritePropertyName('properties');
          JsonWriter.WriteStartObject;
          for Pair in Node.Properties do
          begin
            JsonWriter.WritePropertyName(Pair.Key);
            WriteVariant(JsonWriter, Pair.Value);
          end;
          JsonWriter.WriteEndObject;
        end;

        if aFull and (Length(Node.Data) > 0) then
        begin
          JsonWriter.WritePropertyName('embedding');
          JsonWriter.WriteStartArray;
          for var V in Node.Data do
            JsonWriter.WriteValue(V);
          JsonWriter.WriteEndArray;
        end;

        JsonWriter.WriteEndObject;
      end;
      JsonWriter.WriteEndArray; // ]

      // 2. Serializar Aristas
      JsonWriter.WritePropertyName('edges');
      JsonWriter.WriteStartArray; // "edges": [
      for Edge in FEdgeRegistry.Values do
      begin
        JsonWriter.WriteStartObject;
        JsonWriter.WritePropertyName('id');
        JsonWriter.WriteValue(Edge.ID);
        JsonWriter.WritePropertyName('edgeLabel');
        JsonWriter.WriteValue(Edge.EdgeLabel);
        if not Edge.Name.IsEmpty then
        begin
          JsonWriter.WritePropertyName('name');
          JsonWriter.WriteValue(Edge.Name);
        end;
        JsonWriter.WritePropertyName('source');
        JsonWriter.WriteValue(Edge.FromNode.ID);
        JsonWriter.WritePropertyName('target');
        JsonWriter.WriteValue(Edge.ToNode.ID);

        if Edge.Weight <> 1.0 then
        begin
          JsonWriter.WritePropertyName('weight');
          JsonWriter.WriteValue(Edge.Weight);
        end;

        if Edge.Properties.Count > 0 then
        begin
          JsonWriter.WritePropertyName('properties');
          JsonWriter.WriteStartObject;
          for Pair in Edge.Properties do
          begin
            JsonWriter.WritePropertyName(Pair.Key);
            WriteVariant(JsonWriter, Pair.Value);
          end;
          JsonWriter.WriteEndObject;
        end;

        // --> INICIO DE CAMBIO: Lógica condicional para embeddings
        if aFull and (Length(Edge.Data) > 0) then
        begin
          JsonWriter.WritePropertyName('embedding');
          JsonWriter.WriteStartArray;
          for var V in Edge.Data do
            JsonWriter.WriteValue(V);
          JsonWriter.WriteEndArray;
        end;
        // --> FIN DE CAMBIO

        JsonWriter.WriteEndObject;
      end;
      JsonWriter.WriteEndArray; // ]

      JsonWriter.WriteEndObject; // } (fin del objeto graph)
      JsonWriter.WriteEndObject; // } (fin del objeto raíz)

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
    if Assigned(FDriver) then
      FDriver.AssignToGraph(Self); // El driver necesita conocer a su grafo
  end;
end;

procedure TAiRagGraph.SetInMemoryIndexType(const Value: TAiRagIndexType);
begin
  if FInMemoryIndexType <> Value then
  begin
    FInMemoryIndexType := Value;
    // Propagar el cambio a los vectores internos
    if Assigned(FNodes) then
      FNodes.InMemoryIndexType := Value;
    if Assigned(FEdges) then
      FEdges.InMemoryIndexType := Value;
  end;
end;

function TAiRagGraph.AddEdge(AEdge: TAiRagGraphEdge): TAiRagGraphEdge;
begin
  Result := InternalAddEdge(AEdge, True);
end;


function TAiRagGraph.AddNode(ANode: TAiRagGraphNode): TAiRagGraphNode;
// NOTA: Este es el overload que acepta un objeto pre-creado.
var
  ExistingNode: TAiRagGraphNode;
begin
  // 1. Delegar a InternalAddNode para manejar la lógica de registro, índices y persistencia.
  ExistingNode := InternalAddNode(ANode, True);

  // 2. Comprobar si InternalAddNode encontró un duplicado.
  if ExistingNode <> ANode then
  begin
    // Si devolvió una instancia existente (ExistingNode),
    // liberamos la instancia que se intentó añadir (ANode).
    ANode.Free;
    Result := ExistingNode;
  end
  else
  begin
    // Si InternalAddNode devolvió el objeto ANode, es el nodo adoptado.
    Result := ANode;
  end;
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
  PropValue: Variant;
begin
  Result := False;
  if ANode = nil then
    Exit;

  // 1. Comprobar la etiqueta del nodo (sin cambios)
  if (not NodeLabel.IsEmpty) and (not SameText(ANode.NodeLabel, NodeLabel)) then
    Exit;

  // 2. Comprobar todas las propiedades requeridas del patrón
  for Pair in Properties do
  begin
    // --- INICIO DE LA CORRECCIÓN ---
    // Comprobar si la propiedad del patrón es la especial 'name'
    if SameText(Pair.Key, 'name') then
    begin
      // Si es 'name', comparamos con la propiedad ANode.Name
      if not SameText(ANode.Name, VarToStr(Pair.Value)) then
        Exit; // El nombre no coincide, fallamos la búsqueda
    end
    else
    begin
      // Para cualquier otra propiedad, buscamos en el diccionario ANode.Properties
      if not ANode.Properties.TryGetValue(Pair.Key, PropValue) then
        Exit; // La propiedad requerida no existe en el diccionario del nodo

      if not SameText(VarToStr(PropValue), VarToStr(Pair.Value)) then
        Exit; // El valor de la propiedad no coincide
    end;
    // --- FIN DE LA CORRECCIÓN ---
  end;

  // Si pasamos todas las comprobaciones del bucle, el nodo coincide
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
  PropValue: Variant;
begin
  Result := False;
  if AEdge = nil then
    Exit;

  // 1. Comprobar la dirección del recorrido
  if (Direction <> gdBoth) and (Direction <> AActualDirection) then
    Exit;

  // 2. Comprobar la etiqueta de la arista (si se especificó)
  if (not EdgeLabel.IsEmpty) and (not SameText(AEdge.EdgeLabel, EdgeLabel)) then
    Exit;

  // 3. Comprobar todas las propiedades requeridas
  for Pair in Properties do
  begin
    if not AEdge.Properties.TryGetValue(Pair.Key, PropValue) then
      Exit; // La propiedad no existe

    if not SameText(VarToStr(PropValue), VarToStr(Pair.Value)) then
      Exit; // El valor no coincide
  end;

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
end;

destructor TGraphMatchQuery.Destroy;
begin
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

end.
