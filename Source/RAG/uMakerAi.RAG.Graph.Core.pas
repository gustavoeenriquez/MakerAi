// Los grafos exportados se pueden visualizar con gephi y se descarga desde https://gephi.org/users/download/

unit uMakerAi.RAG.Graph.Core;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  System.Json, System.Variants, System.Json.Writers, System.Json.Types, System.IOUtils,

  Xml.XMLDoc, Xml.XMLIntf, Xml.XMLDom,

  uMakerAi.Embeddings.Core, uMakerAi.Embeddings,
  // Incluimos tu unidad base de MakerAi
  uMakerAi.RAG.Vectors;

type

  TDegreeType = (dtIn, dtOut, dtTotal);
  TGraphExportFormat = (gefDOT, gefGraphML, gefGraphMkai);

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

  TOnGraphAddNode = procedure(Sender: TObject; ANode: TAiRagGraphNode; var Handled: Boolean) of object;
  // Se dispara cuando se añade una arista. El manejador debe persistirla.
  TOnGraphAddEdge = procedure(Sender: TObject; AEdge: TAiRagGraphEdge; var Handled: Boolean) of object;
  // Se dispara para borrar un nodo. El manejador debe borrarlo de la BD (y sus aristas conectadas).
  TOnGraphDeleteNode = procedure(Sender: TObject; const ANodeID: string; var Handled: Boolean) of object;
  // Se dispara para borrar una arista.
  TOnGraphDeleteEdge = procedure(Sender: TObject; const AEdgeID: string; var Handled: Boolean) of object;

  // --- Eventos para Operaciones de Lectura y Búsqueda ---
  // Se dispara para cargar el grafo completo o inicializar la conexión.
  TOnGraphLoad = procedure(Sender: TObject; Graph: TAiRagGraph; var Handled: Boolean) of object;
  // Se dispara para buscar un nodo por su ID. Debe devolver el nodo encontrado.
  TOnGraphFindNodeByID = procedure(Sender: TObject; const ANodeID: string; var ResultNode: TAiRagGraphNode; var Handled: Boolean) of object;
  // Búsqueda semántica. Debe devolver un array con los nodos encontrados.
  TOnGraphSearchNodes = procedure(Sender: TObject; const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; Out ResultNodes: TArray<TAiRagGraphNode>; var Handled: Boolean) of object;
  // Búsqueda por consulta híbrida. Es el más complejo de implementar en la BD.
  TOnGraphQuery = procedure(Sender: TObject; const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double; ResultNodes: TArray<TAiRagGraphNode>; var Handled: Boolean) of object;

  { TAiRagGraphNode }
  TAiRagGraphNode = class(TAiEmbeddingNode)
  private
    FNodeLabel: string;
    FName: string;
    FOutgoingEdges: TObjectList<TAiRagGraphEdge>;
    FIncomingEdges: TObjectList<TAiRagGraphEdge>;
    FProperties: TDictionary<string, Variant>;
    FOwnerGraph: TAiRagGraph;
    FID: string;
  protected
    procedure AddOutgoingEdge(AEdge: TAiRagGraphEdge);
    procedure AddIncomingEdge(AEdge: TAiRagGraphEdge);
    procedure RemoveOutgoingEdge(AEdge: TAiRagGraphEdge);
    procedure RemoveIncomingEdge(AEdge: TAiRagGraphEdge);
  public
    constructor Create(AOwnerGraph: TAiRagGraph; ADim: Integer);
    destructor Destroy; override;
    function PropertiesToJSON: TJSONObject;

    property ID: string read FID write FID;
    property Name: string read FName write FName;
    property NodeLabel: string read FNodeLabel write FNodeLabel;
    property Properties: TDictionary<string, Variant> read FProperties;
    property OutgoingEdges: TObjectList<TAiRagGraphEdge> read FOutgoingEdges;
    property IncomingEdges: TObjectList<TAiRagGraphEdge> read FIncomingEdges;
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
    FOnGraphAddEdge: TOnGraphAddEdge;
    FOnGraphDeleteNode: TOnGraphDeleteNode;
    FOnGraphLoad: TOnGraphLoad;
    FOnGraphAddNode: TOnGraphAddNode;
    FOnGraphFindNodeByID: TOnGraphFindNodeByID;
    FOnGraphSearchNodes: TOnGraphSearchNodes;
    FOnGraphDeleteEdge: TOnGraphDeleteEdge;
    FOnGraphQuery: TOnGraphQuery;

    function GetNodeCount: Integer;
    function GetEdgeCount: Integer;
    function GetNodesRAGVector: TAiRAGVector;
    function GetEdgesRAGVector: TAiRAGVector;
    procedure SetInMemoryIndexType(const Value: TAiRagIndexType);
    procedure RebuildIndexes;
  protected
    procedure UnregisterNode(ANode: TAiRagGraphNode);
    procedure UnregisterEdge(AEdge: TAiRagGraphEdge);
    function ExpandNodeList(AInitialNodes: TList<TAiRagGraphNode>; ADepth: Integer): TArray<TAiRagGraphNode>;
    function GetContextualizedText(ASubgraphNodes: TArray<TAiRagGraphNode>): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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
    // function Match(AQuery: TGraphQuery): TArray<TDictionary<string, TObject>>;  //Falta por implementar

    property NodeCount: Integer read GetNodeCount;
    property EdgeCount: Integer read GetEdgeCount;
    property Nodes: TAiRAGVector read GetNodesRAGVector;
    property Edges: TAiRAGVector read GetEdgesRAGVector;
  Published
    property Embeddings: TAiEmbeddingsCore read FEmbeddings write FEmbeddings;
    Property InMemoryIndexType: TAiRagIndexType read FInMemoryIndexType write SetInMemoryIndexType;
    property OnGraphAddNode: TOnGraphAddNode read FOnGraphAddNode write FOnGraphAddNode;
    property OnGraphAddEdge: TOnGraphAddEdge read FOnGraphAddEdge write FOnGraphAddEdge;
    property OnGraphDeleteNode: TOnGraphDeleteNode read FOnGraphDeleteNode write FOnGraphDeleteNode;
    property OnGraphDeleteEdge: TOnGraphDeleteEdge read FOnGraphDeleteEdge write FOnGraphDeleteEdge;
    property OnGraphLoad: TOnGraphLoad read FOnGraphLoad write FOnGraphLoad;
    property OnGraphFindNodeByID: TOnGraphFindNodeByID read FOnGraphFindNodeByID write FOnGraphFindNodeByID;
    property OnGraphSearchNodes: TOnGraphSearchNodes read FOnGraphSearchNodes write FOnGraphSearchNodes;
    property OnGraphQuery: TOnGraphQuery read FOnGraphQuery write FOnGraphQuery;

  end;

  // Función Helper para convertir TJSONValue a Variant
function JSONValueToVariant(AJsonValue: TJSONValue): Variant;

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

{ TAiRagGraphNode }

constructor TAiRagGraphNode.Create(AOwnerGraph: TAiRagGraph; ADim: Integer);
begin
  inherited Create(ADim);
  FOwnerGraph := AOwnerGraph;
  FOutgoingEdges := TObjectList<TAiRagGraphEdge>.Create(False); // No es dueño de los objetos
  FIncomingEdges := TObjectList<TAiRagGraphEdge>.Create(False); // No es dueño de los objetos
  FProperties := TDictionary<string, Variant>.Create;
end;

destructor TAiRagGraphNode.Destroy;
begin
  FOutgoingEdges.Free;
  FIncomingEdges.Free;
  FProperties.Free;
  inherited;
end;

procedure TAiRagGraphNode.AddIncomingEdge(AEdge: TAiRagGraphEdge);
begin
  FIncomingEdges.Add(AEdge);
end;

procedure TAiRagGraphNode.AddOutgoingEdge(AEdge: TAiRagGraphEdge);
begin
  FOutgoingEdges.Add(AEdge);
end;

procedure TAiRagGraphNode.RemoveIncomingEdge(AEdge: TAiRagGraphEdge);
begin
  FIncomingEdges.Remove(AEdge);
end;

procedure TAiRagGraphNode.RemoveOutgoingEdge(AEdge: TAiRagGraphEdge);
begin
  FOutgoingEdges.Remove(AEdge);
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
    Result.AddPair(Pair.Key, TJSONString.Create(VarToStr(Pair.Value)));
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
    Result.AddPair(Pair.Key, TJSONString.Create(VarToStr(Pair.Value)));
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


function TAiRagGraph.AddNode(AID, ALabel, AName: string): TAiRagGraphNode;
var
  NewNode: TAiRagGraphNode;
begin
  NewNode := TAiRagGraphNode.Create(Self, FNodes.Dim);
  NewNode.ID := AID;
  NewNode.NodeLabel := ALabel;
  NewNode.Name := AName;
  // Esta llamada pasa por toda la nueva lógica de delegación.
  Result := AddNode(NewNode);
  if Result <> NewNode then // Si el manejador de DB se hizo cargo, el resultado será el mismo puntero
  begin
    // En modo memoria, el resultado es el mismo nodo.
    // Si estuviéramos en modo DB, aquí el nodo ya habría sido liberado por el Builder,
    // y esta función devolvería nil.
  end;
end;

procedure TAiRagGraph.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TAiRagGraph.AddEdge(AFromNode, AToNode: TAiRagGraphNode; AID, ALabel, AName: string): TAiRagGraphEdge;
Begin
  Result := AddEdge(AFromNode, AToNode, AID, ALabel, AName, 1.0);
End;

function TAiRagGraph.AddEdge(AFromNode, AToNode: TAiRagGraphNode; AID, ALabel, AName: string; AWeight: Double): TAiRagGraphEdge;
var
  NewEdge: TAiRagGraphEdge;
begin
  NewEdge := TAiRagGraphEdge.Create(Self, FEdges.Dim);
  NewEdge.ID := AID;
  NewEdge.EdgeLabel := ALabel;
  NewEdge.Name := AName;
  NewEdge.Weight := AWeight;
  NewEdge.FromNode := AFromNode;
  NewEdge.ToNode := AToNode;
  Result := AddEdge(NewEdge);
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
  I: Integer;
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
  DeleteNode(Node);
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
begin
  FEdgeRegistry.TryGetValue(AID, Result);
end;

{function TAiRagGraph.FindNodeByID(AID: string): TAiRagGraphNode;
begin
  FNodeRegistry.TryGetValue(AID, Result);
end;
}

function TAiRagGraph.FindNodeByID(AID: string): TAiRagGraphNode;
var
  Handled: Boolean;
begin
  Result := nil;
  Handled := False;

  // Paso 1: Intentar delegar la operación de búsqueda.
  if Assigned(FOnGraphFindNodeByID) then
  begin
    // El manejador es responsable de buscar en la BD, crear el objeto TAiRagGraphNode,
    // y asignarlo a la variable 'Result' que se pasa por referencia.
    FOnGraphFindNodeByID(Self, AID, Result, Handled);
  end;

  // Paso 2: Si la operación no fue manejada, buscar en memoria.
  if not Handled then
  begin
    // Lógica original de búsqueda en memoria.
    FNodeRegistry.TryGetValue(AID, Result);
  end;

  // Al final, 'Result' contendrá el objeto encontrado (ya sea desde la BD o memoria), o será nil.
end;



function TAiRagGraph.FindNodeByName(AName, ANodeLabel: string): TAiRagGraphNode;
var
  CombinedNameKey: string;
begin
  Result := nil;
  if AName.IsEmpty or ANodeLabel.IsEmpty then
    Exit;

  CombinedNameKey := ANodeLabel + '#' + AName;
  // TryGetValue será case-insensitive porque así creamos el diccionario.
  FNodeNameIndex.TryGetValue(CombinedNameKey, Result);
end;

function TAiRagGraph.FindNodesByLabel(ALabel: string): TArray<TAiRagGraphNode>;
var
  NodeList: TList<TAiRagGraphNode>;
begin
  if FNodeLabelIndex.TryGetValue(ALabel, NodeList) then
    Result := NodeList.ToArray
  else
    Result := []; // Devuelve un array vacío si no se encuentra la etiqueta
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

function TAiRagGraph.GetNodeCount: Integer;
begin
  Result := FNodeRegistry.Count;
end;

function TAiRagGraph.GetNodesByDegree(ATop: Integer; ADegreeType: TDegreeType): TArray<TAiRagGraphNode>;
var
  NodeList: TList<TAiRagGraphNode>;
  Degree: Integer;
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

{function TAiRagGraph.Search(const APrompt: string; const ADepth: Integer = 0; ALimit: Integer = 5; const APrecision: Double = 0.5): TArray<TAiRagGraphNode>;
var
  VectorSearchResults: TAiRAGVector;
  FoundItem: TAiEmbeddingNode;
  InitialNodeList: TList<TAiRagGraphNode>; // Lista de resultados semánticos
begin
  Result := [];
  if not Assigned(FEmbeddings) then
  begin
    if Assigned(FNodes) and Assigned(FNodes.Embeddings) then
      FEmbeddings := FNodes.Embeddings
    else
      raise Exception.Create('Embeddings property is not assigned to the graph or its underlying node vector.');
  end;

  if FNodes.Count = 0 then
    Exit;

  FNodes.Embeddings := FEmbeddings;

  // 1. Búsqueda Vectorial
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

      // 3. Expansión Contextual (si se solicita)
      if (ADepth > 0) and (InitialNodeList.Count > 0) then
      begin
        // La expansión se realiza sobre los nodos encontrados semánticamente
        Result := Self.ExpandNodeList(InitialNodeList, ADepth);
      end
      else
      begin
        // Si no hay profundidad, se devuelven solo los resultados iniciales
        Result := InitialNodeList.ToArray;
      end;
    finally
      InitialNodeList.Free;
    end;
  finally
    VectorSearchResults.Free;
  end;
end;
}

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
  if Assigned(FOnGraphSearchNodes) then
  begin
    // El manejador es responsable de:
    // 1. Obtener el embedding del APrompt.
    // 2. Realizar la búsqueda por similaridad en la BD.
    // 3. Si ADepth > 0, realizar la expansión estructural en la BD.
    // 4. "Hidratar" todos los nodos y aristas del subgrafo resultante.
    // 5. Asignar el array de nodos final a 'Result'.
    FOnGraphSearchNodes(Self, APrompt, ADepth, ALimit, APrecision, Result, Handled);
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
  NodeValue, EdgeValue, PropValue: TJSONValue;
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


// El query es una consulta hibrida, primero busca por embeddings para ubicar nodos y luego
// utiliza el grafo para encontrar los datos precisos de la búsqueda requeridad
// es necesario que el usuario/programador conozca la estructura de los datos
// se puede utilizar una IA para definir el plan

{function TAiRagGraph.Query(const APlan: TQueryPlan; ADepth: Integer = 0; const ALimit: Integer = 5; const APrecision: Double = 0.5): TArray<TAiRagGraphNode>;
var
  IntermediateResults: TDictionary<string, TList<TAiRagGraphNode>>;
  InitialNodes: TArray<TAiRagGraphNode>;
  Step: TQueryStep;
  SourceNodes, TargetNodes: TList<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  ResultSet: TList<TAiRagGraphNode>;
  // --> CAMBIO 1: Declaramos el diccionario para el control de duplicados
  UniqueTargetNodes: TDictionary<TAiRagGraphNode, Boolean>;
begin
  Result := [];
  IntermediateResults := TDictionary < string, TList < TAiRagGraphNode >>.Create;
  // ResultSet no necesita ser creado, ya que apuntará a una lista dentro de IntermediateResults
  try
    // --- Etapa 1: Búsqueda Semántica de Anclaje ---
    InitialNodes := Self.Search(APlan.AnchorPrompt, ADepth, ALimit, APrecision);
    if Length(InitialNodes) = 0 then
      Exit;

    var
    AnchorList := TList<TAiRagGraphNode>.Create;
    AnchorList.AddRange(InitialNodes);
    IntermediateResults.Add(APlan.AnchorVariable, AnchorList);

    // --- Etapa 2: Ejecución de Pasos Estructurales ---
    for Step in APlan.Steps do
    begin
      if not IntermediateResults.TryGetValue(Step.SourceVariable, SourceNodes) then
        Continue;

      if not IntermediateResults.TryGetValue(Step.TargetVariable, TargetNodes) then
      begin
        TargetNodes := TList<TAiRagGraphNode>.Create;
        IntermediateResults.Add(Step.TargetVariable, TargetNodes);
      end;

      // --> CAMBIO 2: Creamos un nuevo conjunto de control para CADA paso.
      // Esto es importante. No queremos que los duplicados de un paso anterior
      // impidan añadir un nodo en este paso si la lógica lo requiere.
      UniqueTargetNodes := TDictionary<TAiRagGraphNode, Boolean>.Create;
      try
        // Pre-cargamos el diccionario con los nodos que ya están en la lista de destino
        for Node in TargetNodes do
        begin
          UniqueTargetNodes.Add(Node, True);
        end;

        for Node in SourceNodes do
        begin
          if Step.IsReversed then // Búsqueda hacia atrás
          begin
            for Edge in Node.IncomingEdges do
            begin
              if SameText(Edge.EdgeLabel, Step.EdgeLabel) and ((Step.TargetNodeLabel = '') or SameText(Edge.FromNode.NodeLabel, Step.TargetNodeLabel)) then
              begin
                // --> CAMBIO 3: Usamos ContainsKey (O(1)) en lugar de IndexOf (O(N))
                if not UniqueTargetNodes.ContainsKey(Edge.FromNode) then
                begin
                  TargetNodes.Add(Edge.FromNode);
                  UniqueTargetNodes.Add(Edge.FromNode, True); // Lo añadimos al control
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
                // --> CAMBIO 4: Repetimos la lógica optimizada
                if not UniqueTargetNodes.ContainsKey(Edge.ToNode) then
                begin
                  TargetNodes.Add(Edge.ToNode);
                  UniqueTargetNodes.Add(Edge.ToNode, True); // Lo añadimos al control
                end;
              end;
            end;
          end;
        end;
      finally
        UniqueTargetNodes.Free; // Liberamos el control para el siguiente paso
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
}

function TAiRagGraph.Query(const APlan: TQueryPlan; ADepth: Integer; const ALimit: Integer; const APrecision: Double): TArray<TAiRagGraphNode>;
var
  Handled: Boolean;
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
  Handled := False;

  // Paso 1: Intentar delegar la ejecución del plan de consulta completo.
  if Assigned(FOnGraphQuery) then
  begin
    // El manejador de eventos es responsable de la tarea completa:
    // 1. Interpretar el APlan.
    // 2. Realizar la búsqueda semántica inicial en la BD para el 'AnchorPrompt'.
    // 3. Realizar las travesías estructurales para cada uno de los 'Steps' usando SQL (posiblemente con JOINs o CTEs).
    // 4. Recolectar los nodos finales que corresponden al 'ResultVariable'.
    // 5. "Hidratar" los objetos TAiRagGraphNode resultantes y asignarlos al parámetro 'Result'.
    FOnGraphQuery(Self, APlan, ADepth, ALimit, APrecision, Result, Handled);
  end;

  // Paso 2: Si la consulta no fue manejada por un delegado, ejecutar la lógica en memoria.
  if not Handled then
  begin
    // --- Lógica original y optimizada de la consulta en memoria ---
    IntermediateResults := TDictionary<string, TList<TAiRagGraphNode>>.Create;
    try
      // --- Etapa 1: Búsqueda Semántica de Anclaje (en memoria) ---
      // Se utiliza el 'Search' del propio componente, que a su vez también respeta
      // el modo de operación (memoria o BD), haciendo el sistema muy cohesivo.
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
          for Node in TargetNodes do
            UniqueTargetNodes.Add(Node, True);

          for Node in SourceNodes do
          begin
            if Step.IsReversed then // Búsqueda hacia atrás
            begin
              for Edge in Node.IncomingEdges do
              begin
                if SameText(Edge.EdgeLabel, Step.EdgeLabel) and
                   ((Step.TargetNodeLabel = '') or SameText(Edge.FromNode.NodeLabel, Step.TargetNodeLabel)) then
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
                if SameText(Edge.EdgeLabel, Step.EdgeLabel) and
                   ((Step.TargetNodeLabel = '') or SameText(Edge.ToNode.NodeLabel, Step.TargetNodeLabel)) then
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
        Result := []; // Asegura un array vacío si la variable de resultado no existe.

    finally
      // Limpiar el diccionario y las listas que contiene
      for var List in IntermediateResults.Values do
        List.Free;
      IntermediateResults.Free;
    end;
  end;

  // Al final, 'Result' contiene el array de nodos, ya sea desde la BD o desde la ejecución en memoria.
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
  FNodes.BuildIndex;
  FEdges.BuildIndex;
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
var
  Handled: Boolean;
begin
  if (AEdge = nil) or (AEdge.OwnerGraph <> Self) then
     raise Exception.Create('Invalid edge provided to AddEdge.');

  Handled := False;
  if Assigned(FOnGraphAddEdge) then
    FOnGraphAddEdge(Self, AEdge, Handled);

  if Handled then
  begin
    Result := AEdge;
  end
  else
  begin
    // Lógica en memoria
    if FEdgeRegistry.ContainsKey(AEdge.ID) then
      raise Exception.CreateFmt('Edge with ID "%s" already exists.', [AEdge.ID]);

    FEdgeRegistry.Add(AEdge.ID, AEdge);
    FEdges.Items.Add(AEdge);
    AEdge.FromNode.AddOutgoingEdge(AEdge); // Aquí usamos el acceso protegido desde la misma unidad
    AEdge.ToNode.AddIncomingEdge(AEdge);
    Result := AEdge;
  end;
end;

function TAiRagGraph.AddNode(ANode: TAiRagGraphNode): TAiRagGraphNode;
var
  Handled: Boolean;
  NodeList: TList<TAiRagGraphNode>;
  CombinedNameKey: string;
begin
  if (ANode = nil) or (ANode.OwnerGraph <> Self) then
    raise Exception.Create('Invalid node provided to AddNode.');

  Handled := False;
  if Assigned(FOnGraphAddNode) then
    FOnGraphAddNode(Self, ANode, Handled);

  if Handled then
  begin
    // El manejador se hizo cargo. En este caso, el Builder debe liberar el nodo
    // que pasó como parámetro. No devolvemos nada o devolvemos el mismo puntero
    // para que el llamador sepa que debe liberarlo.
    Result := ANode; // El llamador (Builder) es responsable de liberar ANode
  end
  else
  begin
    // Lógica en memoria
    if FNodeRegistry.ContainsKey(ANode.ID) then
      raise Exception.CreateFmt('Node with ID "%s" already exists in memory.', [ANode.ID]);

    FNodeRegistry.Add(ANode.ID, ANode);
    FNodes.Items.Add(ANode);

    if FUpdateCount = 0 then
    begin
      if not FNodeLabelIndex.TryGetValue(ANode.NodeLabel, NodeList) then
      begin
        NodeList := TList<TAiRagGraphNode>.Create;
        FNodeLabelIndex.Add(ANode.NodeLabel, NodeList);
      end;
      NodeList.Add(ANode);

      if not ANode.Name.IsEmpty then
      begin
        CombinedNameKey := ANode.NodeLabel + '#' + ANode.Name;
        if not FNodeNameIndex.ContainsKey(CombinedNameKey) then
          FNodeNameIndex.Add(CombinedNameKey, ANode);
      end;
    end;
    Result := ANode; // Devolvemos el nodo que ahora es propiedad del grafo.
  end;
end;

end.
