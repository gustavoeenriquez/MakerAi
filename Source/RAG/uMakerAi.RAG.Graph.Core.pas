// MIT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
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

  { Enumeraciones para el �rbol de Expresiones }
  TExpressionKind = (ekLiteral, ekProperty, ekBinary, ekUnary, ekBetween, ekVariable, ekFunction, ekAggregate);

  TBinaryOp = (
    // L�gicos
    opAnd, opOr,

    // Comparaci�n Est�ndar
    opEqual, opNotEqual, opGreater, opGreaterEqual, opLess, opLessEqual,

    // Texto
    opContains, opLike, // Nuevo
    opILike, // Nuevo

    // Listas y Nulidad
    opIn, // Nuevo
    opNotIn, // Nuevo
    opIsNull, // Nuevo
    opIsNotNull, // Nuevo

    // v4.1: XOR y prefijo/sufijo de texto (sin distinguir mayúsculas, como CONTAINS)
    opXor, opStartsWith, opEndsWith,

    // Aritmética. + suma si ambos lados son números (aunque vengan como texto); si no, concatena
    opAdd, opSub, opMul, opDiv, opMod
    );

  TUnaryOp = (uoNot, uoNegate);

  { Clase base abstracta para expresiones del lenguaje }
  TGraphExpression = class
  public
    Kind: TExpressionKind;
    destructor Destroy; override;
  end;

  { Expresi�n para valores constantes (18, 'Madrid', true, null) }
  TLiteralExpr = class(TGraphExpression)
  public
    Value: Variant;
    constructor Create(AValue: Variant);
  end;

  { Expresi�n para acceso a propiedades (p.nombre, e.peso) }
  TPropertyExpr = class(TGraphExpression)
  public
    Variable: string; // Ejemplo: 'p'
    PropertyKey: string; // Ejemplo: 'nombre'
    constructor Create(const AVar, AKey: string);
  end;

  { Expresi�n para operaciones binarias (A > B, C AND D) }
  TBinaryExpr = class(TGraphExpression)
  public
    Left: TGraphExpression;
    Op: TBinaryOp;
    Right: TGraphExpression;
    constructor Create(ALeft: TGraphExpression; AOp: TBinaryOp; ARight: TGraphExpression);
    destructor Destroy; override;
  end;

  { Expresión unaria: NOT expr, -expr }
  TUnaryExpr = class(TGraphExpression)
  public
    Op: TUnaryOp;
    Operand: TGraphExpression;
    constructor Create(AOp: TUnaryOp; AOperand: TGraphExpression);
    destructor Destroy; override;
  end;

  { Variable suelta (n, r): argumento de id(n), type(r), degree(n)... }
  TVariableExpr = class(TGraphExpression)
  public
    Variable: string;
    constructor Create(const AVar: string);
  end;

  { Llamada a función: lower(n.name), coalesce(a, b). Name va en minúsculas (nombre canónico). }
  TFunctionExpr = class(TGraphExpression)
  public
    Name: string;
    Args: TObjectList<TGraphExpression>; // Es dueña de los argumentos
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

  { Agregado de RETURN/WITH/ORDER BY: count(*), count(DISTINCT x), sum, avg, min, max, collect.
    Value lo llena el ejecutor por grupo antes de evaluar la columna. }
  TAggregateFunc = (agCount, agSum, agAvg, agMin, agMax, agCollect);

  TAggregateExpr = class(TGraphExpression)
  public
    Func: TAggregateFunc;
    Arg: TGraphExpression; // nil = count(*)
    Distinct: Boolean;
    Value: Variant;
    constructor Create(AFunc: TAggregateFunc; AArg: TGraphExpression; ADistinct: Boolean);
    destructor Destroy; override;
  end;

  { Columna de RETURN/WITH: expresión y nombre (el alias o el texto de la expresión) }
  TGQLReturnItem = class
  public
    Expr: TGraphExpression;
    Name: string;
    destructor Destroy; override;
  end;

  TGQLOrderItem = class
  public
    Expr: TGraphExpression;
    Descending: Boolean;
    destructor Destroy; override;
  end;

  { Una etapa WITH o el RETURN final }
  TGQLProjection = class
  public
    IsReturn: Boolean;
    Distinct: Boolean;
    Items: TObjectList<TGQLReturnItem>;
    OrderBy: TObjectList<TGQLOrderItem>;
    Skip: Integer; // -1 = sin SKIP
    Limit: Integer; // -1 = sin LIMIT
    Where: TGraphExpression; // Solo en WITH (se evalúa sobre las columnas)
    constructor Create;
    destructor Destroy; override;
  end;

  { Rango con extremos incluidos: Value BETWEEN Low AND High }
  TBetweenExpr = class(TGraphExpression)
  public
    Value: TGraphExpression;
    Low: TGraphExpression;
    High: TGraphExpression;
    constructor Create(AValue, ALow, AHigh: TGraphExpression);
    destructor Destroy; override;
  end;

  TDegreeType = (dtIn, dtOut, dtTotal);
  TGraphExportFormat = (gefDOT, gefGraphML, gefGraphMkai);

  { TMergeStrategy }
  TMergeStrategy = (msAddNewOnly, // (Default) Solo a�ade propiedades que no existen.
    msOverwrite, // Sobrescribe las propiedades existentes con las nuevas.
    msKeepExisting // No realiza ning�n cambio en las propiedades del elemento existente.
    );


  // --Planificador de consultas---
  // Un modelo para realizar consultas hibridas al grafo, primero obtiene con embeddings los nodos iniciales
  // luego busca dentro del grafo datos muy explicitos para obtener datos precisos del grafo.

  // Representa un paso en el plan de consulta.
  TQueryStep = record
    SourceVariable: string; // Variable del paso anterior (vac�o para el primer paso)
    EdgeLabel: string; // Etiqueta de la arista a seguir
    TargetVariable: string; // Nombre para guardar los resultados de este paso
    TargetNodeLabel: string; // Filtro opcional para el tipo de nodo de destino
    IsReversed: Boolean; // Si la b�squeda debe ser hacia atr�s (incoming)
  end;

  // Representa el plan de consulta completo.
  TQueryPlan = record
    AnchorPrompt: string; // El texto para la b�squeda sem�ntica inicial
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
  // --Match es una funci�n que permite recorrer el grafo sin necesidad de embeddings para encontrar patrones y relaciones

  // Direcci�n para la b�squeda de patrones en aristas
  TGraphDirection = (gdOutgoing, gdIncoming, gdBoth);

  // --- Clases auxiliares para la construcci�n de la consulta MATCH ---

  // Representa un patr�n para un nodo en la consulta.
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

  // Representa un patr�n para una arista en la consulta.
  // Ej: -[r:KNOWS {since: 2018}]->
  TMatchEdgePattern = class
  public
    Variable: string; // El alias de la arista, ej: 'r'
    EdgeLabel: string; // La etiqueta a buscar, ej: 'KNOWS'
    Direction: TGraphDirection; // La direcci�n de la relaci�n
    Properties: TDictionary<string, Variant>; // Filtros de propiedades, ej: {'since': 2018}
    constructor Create;
    destructor Destroy; override;
    // Comprueba si una arista coincide con el patr�n, considerando la direcci�n del recorrido actual
    function Matches(AEdge: TAiRagGraphEdge; AActualDirection: TGraphDirection): Boolean;
  end;

  // Representa una cl�usula de patr�n completa.
  // Ej: (p)-[r]->(m)
  TMatchClause = class
  public
    SourceNodeVar: string; // Variable del nodo de origen, ej: 'p'
    EdgePattern: TMatchEdgePattern; // El patr�n de la arista (la cl�usula es due�a de este objeto)
    TargetNodeVar: string; // Variable del nodo de destino, ej: 'm'
    constructor Create(ASourceNodeVar: string; AEdgePattern: TMatchEdgePattern; ATargetNodeVar: string);
    destructor Destroy; override;
  end;

  // La consulta completa que contiene todos los patrones de nodos y cl�usulas.
  TGraphMatchQuery = class
  private
    FNodePatterns: TObjectList<TMatchNodePattern>; // Es due�o de los patrones de nodo
    FMatchClauses: TObjectList<TMatchClause>;
    FWhereClause: TGraphExpression;
    FDepth: Integer; // Es due�o de las cl�usulas
    FProjections: TObjectList<TGQLProjection>;
    FTabular: Boolean;
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
    // Etapas WITH ... y RETURN, en orden
    property Projections: TObjectList<TGQLProjection> read FProjections;
    // True: el resultado es una tabla (RETURN con columnas, agregados, DISTINCT, ORDER BY, SKIP, LIMIT o WITH).
    // False: el subgrafo encontrado como texto de contexto, como siempre.
    property Tabular: Boolean read FTabular write FTabular;
  end;

  TStringWrapper = class(TObject)
  public
    Value: string;
    constructor Create(const AValue: string);
  end;

  { Valor de una columna en un resultado tabular de MakerGQL: Value tiene el texto (sin depender
    del idioma) y Data el valor original. FreeGQLResults lo libera como cualquier TStringWrapper. }
  TGQLValue = class(TStringWrapper)
  public
    Data: Variant;
    constructor CreateValue(const AData: Variant);
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
    // M�todos que los drivers DEBEN implementar
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

    // --- NUEVOS M�TODOS Y PROPIEDADES ---
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
    function EvaluateGQLFunction(AFunc: TFunctionExpr; ABoundElements: TDictionary<string, TObject>): Variant;
    function RunGQLProjection(AStage: TGQLProjection; const AInput: TArray<TDictionary<string, TObject>>): TArray<TDictionary<string, TObject>>;
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

    procedure SaveToStream(AStream: TStream); overload; // La versi�n original
    procedure SaveToStream(AStream: TStream; aFull: Boolean); overload; // La nueva versi�n

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
    // Con parámetros: AParams alterna nombre y valor (sin el $), p. ej.
    //   ExecuteMakerGQL('MATCH (a:Acidez) WHERE a.name = $nivel', ['nivel', Nivel])
    // El valor nunca pasa por el lexer: es la forma segura de meter texto que llega de un LLM o de un usuario.
    // Para IN $lista, pasar un VarArrayOf([...]).
    function ExecuteMakerGQL(const ACode: string; const AParams: array of Variant): string; Overload;
    function ExecuteMakerGQL(const ACode: string; const AParams: array of Variant; out AResultObjects: TArray<TDictionary<string, TObject>>;
      ADepth: Integer = 0): string; Overload;
    function ExecuteMakerGQL(const ACode: string; AParams: TDictionary<string, Variant>; out AResultObjects: TArray<TDictionary<string, TObject>>;
      ADepth: Integer = 0): string; Overload;
    // Libera lo que devuelven ExecuteMakerGQL y Match: los diccionarios y los TStringWrapper que
    // contienen ('type', 'value'...). Los nodos y aristas son del grafo y no se tocan.
    class procedure FreeGQLResults(var AResults: TArray<TDictionary<string, TObject>>);


    // todo Implementar Detecci�n de Comunidades (Community Detection) Algoritmo de Louvain

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

  // Funci�n Helper para convertir TJSONValue a Variant
function JSONValueToVariant(AJsonValue: TJSONValue): Variant;
function VariantToJSONValue(const AValue: Variant): TJSONValue;
function StringToEmbedding(const AVectorString: string): TAiEmbeddingData;
procedure JSONStringToProperties(const AJSONString: string; const AProperties: TDictionary<string, Variant>);

procedure Register;

implementation

uses uMakerAi.Localization, uMakerAi.RAG.Graph.GQL;

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

    // Si el array est� vac�o
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

    // Si el objeto est� vac�o
    if JSONObject.Count = 0 then
    begin
      Result := '{}';
      Exit;
    end;

    // Convertir objeto a string formateado (o podr�as usar otra estructura)
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
    Result := TJSONNumber.Create(Extended(AValue)) // Usar Extended para m�xima precisi�n
  else if VarIsNumeric(AValue) then
    Result := TJSONNumber.Create(Integer(AValue))
  else if VarIsStr(AValue) then
    Result := TJSONString.Create(VarToStr(AValue))
  else
    // Fallback: si no es un tipo primitivo, lo convertimos a string.
    // Una implementaci�n m�s avanzada podr�a manejar arrays o TDateTime de forma especial.
    Result := TJSONString.Create(VarToStr(AValue));
end;

function StringToEmbedding(const AVectorString: string): TAiEmbeddingData;
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

      // Intentamos flotante (SIEMPRE v�lido para JSON number)
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
    Exit; // JSON inv�lido

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
  FInternalOutgoingEdges := TObjectList<TAiRagGraphEdge>.Create(False); // No es due�o de los objetos
  FInternalIncomingEdges := TObjectList<TAiRagGraphEdge>.Create(False); // No es due�o de los objetos
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
  // Si las aristas ya est�n cargadas o no hay un OwnerGraph/evento asignado, no hacemos nada.
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
  // Creamos un nodo de embedding b�sico para el fragmento.
  // Usamos el constructor que define la dimensi�n basada en los datos recibidos.
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
  // Delegamos la serializaci�n al nuevo objeto MetaData unificado.
  // Este ya maneja correctamente la conversi�n de Variant a JSON (Fechas, Booleano, etc.)
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
  // El MetaData se libera autom�ticamente en el destructor de la clase base (inherited)
  inherited;
end;

function TAiRagGraphEdge.PropertiesToJSON: TJSONObject;
begin
  // Delegamos la serializaci�n al nuevo objeto MetaData unificado.
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

  // --- Inicializaci�n: Cada nodo en su propia comunidad ---
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

  // --- Bucle principal de optimizaci�n ---
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

        // Calcular K_i_in (conexi�n del nodo con la comunidad destino)
        K_i_in := 0;
        for Edge in Node.OutgoingEdges do
          if Result[Edge.ToNode] = TargetCommID then
            K_i_in := K_i_in + Edge.Weight;
        for Edge in Node.IncomingEdges do
          if Result[Edge.FromNode] = TargetCommID then
            K_i_in := K_i_in + Edge.Weight;

        // F�rmula simplificada de Ganancia de Modularidad (delta Q)
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

// ---------------------------------------------------------------------------
// Ayudantes de MakerGQL para comparar valores (independientes del idioma del sistema)
// ---------------------------------------------------------------------------

function GQLIsNull(const V: Variant): Boolean;
begin
  Result := VarIsNull(V) or VarIsEmpty(V);
end;

function GQLToBool(const V: Variant): Boolean;
begin
  if GQLIsNull(V) then
    Exit(False);
  try
    Result := Boolean(V);
  except
    Result := False;
  end;
end;

// Número de un Variant: numéricos tal cual; textos con punto decimal ('3.5'), sin importar el idioma
function GQLTryNumber(const V: Variant; out D: Double): Boolean;
begin
  Result := False;
  if GQLIsNull(V) or VarIsType(V, varBoolean) then
    Exit;
  if VarIsNumeric(V) or VarIsType(V, varDate) then
  begin
    D := V;
    Exit(True);
  end;
  if VarIsStr(V) then
    Result := TryStrToFloat(Trim(VarToStr(V)), D, TFormatSettings.Invariant);
end;

function GQLTryDate(const V: Variant; out D: TDateTime): Boolean;
begin
  if VarIsType(V, varDate) then
  begin
    D := VarToDateTime(V);
    Exit(True);
  end;
  Result := VarIsStr(V) and TryISO8601ToDate(VarToStr(V), D, False);
end;

function GQLBoolText(const V: Variant): string;
begin
  if VarIsType(V, varBoolean) then
  begin
    if Boolean(V) then
      Result := 'true'
    else
      Result := 'false';
  end
  else
    Result := LowerCase(Trim(VarToStr(V)));
end;

// Compara L y R. False si alguno es NULL (no comparables). Orden de intento:
// fecha (si uno es fecha), booleano (si uno es booleano), número, texto (distingue mayúsculas).
function GQLCompare(const L, R: Variant; out Cmp: Integer): Boolean;
var
  DL, DR: Double;
  TL, TR: TDateTime;
begin
  Cmp := 0;
  if GQLIsNull(L) or GQLIsNull(R) then
    Exit(False);
  Result := True;

  if (VarIsType(L, varDate) or VarIsType(R, varDate)) and GQLTryDate(L, TL) and GQLTryDate(R, TR) then
  begin
    if TL < TR then
      Cmp := -1
    else if TL > TR then
      Cmp := 1;
  end
  else if VarIsType(L, varBoolean) or VarIsType(R, varBoolean) then
    Cmp := CompareText(GQLBoolText(L), GQLBoolText(R))
  else if GQLTryNumber(L, DL) and GQLTryNumber(R, DR) then
  begin
    if DL < DR then
      Cmp := -1
    else if DL > DR then
      Cmp := 1;
  end
  else
    Cmp := CompareStr(VarToStr(L), VarToStr(R));
end;

// Igualdad de MakerGQL: NULL = NULL es verdadero; NULL contra un valor, falso
function GQLEquals(const L, R: Variant): Boolean;
var
  Cmp: Integer;
begin
  if GQLIsNull(L) or GQLIsNull(R) then
    Exit(GQLIsNull(L) and GQLIsNull(R));
  Result := GQLCompare(L, R, Cmp) and (Cmp = 0);
end;

// LIKE de SQL: % = cualquier secuencia, _ = un carácter. Sin otros comodines.
function GQLLikeMatch(const S, P: string): Boolean;
var
  SIdx, PIdx, StarP, StarS: Integer;
begin
  SIdx := 1;
  PIdx := 1;
  StarP := 0;
  StarS := 0;
  while SIdx <= Length(S) do
  begin
    if (PIdx <= Length(P)) and ((P[PIdx] = '_') or ((P[PIdx] <> '%') and (P[PIdx] = S[SIdx]))) then
    begin
      Inc(SIdx);
      Inc(PIdx);
    end
    else if (PIdx <= Length(P)) and (P[PIdx] = '%') then
    begin
      StarP := PIdx;
      StarS := SIdx;
      Inc(PIdx);
    end
    else if StarP > 0 then
    begin
      PIdx := StarP + 1;
      Inc(StarS);
      SIdx := StarS;
    end
    else
      Exit(False);
  end;
  while (PIdx <= Length(P)) and (P[PIdx] = '%') do
    Inc(PIdx);
  Result := PIdx > Length(P);
end;

function GQLInList(const Val, List: Variant): Boolean;
var
  I: Integer;
begin
  Result := False;
  if GQLIsNull(Val) then
    Exit;
  if VarIsArray(List) then
  begin
    for I := VarArrayLowBound(List, 1) to VarArrayHighBound(List, 1) do
      if GQLEquals(Val, VarArrayGet(List, [I])) then
        Exit(True);
  end
  else
    Result := GQLEquals(Val, List);
end;

// Texto de un valor sin depender del idioma: números con punto, fechas ISO, booleanos true/false
function GQLToText(const V: Variant): string;
var
  Dt: TDateTime;
  I: Integer;
begin
  if GQLIsNull(V) then
    Exit('');
  if VarIsType(V, varBoolean) then
    Exit(GQLBoolText(V));
  if VarIsType(V, varDate) then
  begin
    Dt := VarToDateTime(V);
    if Frac(Dt) = 0 then
      Exit(FormatDateTime('yyyy-mm-dd', Dt, TFormatSettings.Invariant))
    else
      Exit(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Dt, TFormatSettings.Invariant));
  end;
  if VarIsArray(V) then
  begin
    Result := '[';
    for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
    begin
      if I > VarArrayLowBound(V, 1) then
        Result := Result + ', ';
      Result := Result + GQLToText(V[I]);
    end;
    Exit(Result + ']');
  end;
  if VarIsNumeric(V) then
    Exit(FloatToStr(Double(V), TFormatSettings.Invariant));
  Result := VarToStr(V);
end;

function GQLStringsToVarArray(const AItems: TArray<string>): Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, Length(AItems) - 1], varVariant);
  for I := 0 to High(AItems) do
    Result[I] := AItems[I];
end;

// ---------------------------------------------------------------------------
// MakerGQL: proyección (WITH / RETURN) y resultado en tabla
// ---------------------------------------------------------------------------

// Clave de un valor para agrupar y para DISTINCT (3 y 3.0 son el mismo número)
function GQLValueKey(const V: Variant): string;
var
  D: Double;
begin
  if GQLIsNull(V) then
    Result := 'null'
  else if VarIsType(V, varBoolean) then
    Result := 'b:' + GQLBoolText(V)
  else if GQLTryNumber(V, D) and not VarIsStr(V) then
    Result := 'n:' + FloatToStr(D, TFormatSettings.Invariant)
  else
    Result := 's:' + GQLToText(V);
end;

// Clave de una celda: identidad para nodos y aristas, valor para el resto
function GQLCellKey(AObj: TObject): string;
begin
  if AObj = nil then
    Result := 'null'
  else if AObj is TGQLValue then
    Result := GQLValueKey(TGQLValue(AObj).Data)
  else
    Result := 'o:' + IntToHex(NativeUInt(AObj), SizeOf(Pointer) * 2);
end;

function GQLCellText(AObj: TObject): string;
begin
  if AObj = nil then
    Result := ''
  else if AObj is TGQLValue then
    Result := GQLToText(TGQLValue(AObj).Data)
  else if AObj is TAiRagGraphNode then
    Result := TAiRagGraphNode(AObj).Name
  else if AObj is TAiRagGraphEdge then
    Result := TAiRagGraphEdge(AObj).EdgeLabel
  else if AObj is TStringWrapper then
    Result := TStringWrapper(AObj).Value
  else
    Result := '';
end;

// Tabla Markdown (la lee bien un LLM) con las columnas de la última etapa
function GQLRowsToText(AStage: TGQLProjection; const ARows: TArray<TDictionary<string, TObject>>): string;
var
  SB: TStringBuilder;
  Item: TGQLReturnItem;
  Row: TDictionary<string, TObject>;
  Obj: TObject;

  function Esc(const S: string): string;
  begin
    Result := S.Replace('|', '\|').Replace(#13#10, ' ').Replace(#10, ' ').Replace(#13, ' ');
  end;

begin
  SB := TStringBuilder.Create;
  try
    SB.Append('|');
    for Item in AStage.Items do
      SB.Append(' ').Append(Esc(Item.Name)).Append(' |');
    SB.AppendLine;
    SB.Append('|');
    for Item in AStage.Items do
      SB.Append('---|');
    SB.AppendLine;
    for Row in ARows do
    begin
      SB.Append('|');
      for Item in AStage.Items do
      begin
        if not Row.TryGetValue(Item.Name, Obj) then
          Obj := nil;
        SB.Append(' ').Append(Esc(GQLCellText(Obj))).Append(' |');
      end;
      SB.AppendLine;
    end;
    SB.Append('(').Append(Length(ARows)).Append(' rows)');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

type
  TGQLOutRow = record
    Row: TDictionary<string, TObject>;
    Sort: TArray<Variant>;
    Index: Integer;
  end;

function TAiRagGraph.RunGQLProjection(AStage: TGQLProjection; const AInput: TArray<TDictionary<string, TObject>>)
  : TArray<TDictionary<string, TObject>>;
var
  Aggs: TList<TAggregateExpr>;
  Outs: TList<TGQLOutRow>;
  GroupIdx: TDictionary<string, Integer>;
  Groups: TObjectList<TList<TDictionary<string, TObject>>>;
  Group: TList<TDictionary<string, TObject>>;
  Empty, InRow, First: TDictionary<string, TObject>;
  CellObj: TObject;
  Item: TGQLReturnItem;
  Order: TGQLOrderItem;
  Agg: TAggregateExpr;
  OR1: TGQLOutRow;
  Seen: TDictionary<string, Boolean>;
  Key: string;
  HasKeyItems: Boolean;
  I, FromIdx, ToIdx: Integer;

  procedure FreeRow(ARow: TDictionary<string, TObject>);
  var
    A: TArray<TDictionary<string, TObject>>;
  begin
    A := [ARow];
    TAiRagGraph.FreeGQLResults(A);
  end;

  procedure CollectAggs(E: TGraphExpression);
  var
    X: TGraphExpression;
  begin
    if E = nil then
      Exit;
    case E.Kind of
      ekAggregate:
        Aggs.Add(TAggregateExpr(E)); // No se anidan: el parser lo impide
      ekBinary:
        begin
          CollectAggs(TBinaryExpr(E).Left);
          CollectAggs(TBinaryExpr(E).Right);
        end;
      ekUnary:
        CollectAggs(TUnaryExpr(E).Operand);
      ekBetween:
        begin
          CollectAggs(TBetweenExpr(E).Value);
          CollectAggs(TBetweenExpr(E).Low);
          CollectAggs(TBetweenExpr(E).High);
        end;
      ekFunction:
        for X in TFunctionExpr(E).Args do
          CollectAggs(X);
    end;
  end;

  function HasAgg(E: TGraphExpression): Boolean;
  var
    Before: Integer;
  begin
    Before := Aggs.Count;
    CollectAggs(E);
    Result := Aggs.Count > Before;
    Aggs.Count := Before;
  end;

  // Variable enlazada a un nodo o arista (no a un valor): se conserva el objeto
  function BoundElement(E: TGraphExpression; ABind: TDictionary<string, TObject>; out AObj: TObject): Boolean;
  begin
    Result := (E is TVariableExpr) and ABind.TryGetValue(TVariableExpr(E).Variable, AObj) and (AObj <> nil) and
      not(AObj is TStringWrapper);
  end;

  function MakeCell(E: TGraphExpression; ABind: TDictionary<string, TObject>): TObject;
  var
    Obj: TObject;
  begin
    if BoundElement(E, ABind, Obj) then
      Result := Obj // Nodo o arista del grafo: no es dueña
    else if (E is TVariableExpr) and ABind.TryGetValue(TVariableExpr(E).Variable, Obj) and (Obj is TGQLValue) then
      Result := TGQLValue.CreateValue(TGQLValue(Obj).Data)
    else
      Result := TGQLValue.CreateValue(EvaluateGraphExpression(E, ABind));
  end;

  // Valor para ordenar: un nodo por su nombre, una arista por su tipo
  function SortValue(E: TGraphExpression; ABind: TDictionary<string, TObject>): Variant;
  var
    Obj: TObject;
  begin
    if BoundElement(E, ABind, Obj) then
      Result := GQLCellText(Obj)
    else
      Result := EvaluateGraphExpression(E, ABind);
  end;

  function ExprKey(E: TGraphExpression; ABind: TDictionary<string, TObject>): string;
  var
    Obj: TObject;
  begin
    if BoundElement(E, ABind, Obj) then
      Result := GQLCellKey(Obj)
    else
      Result := GQLValueKey(EvaluateGraphExpression(E, ABind));
  end;

  // Enlaces para ORDER BY: las columnas nuevas tapan a las variables de entrada
  function Merge(AIn, AOut: TDictionary<string, TObject>): TDictionary<string, TObject>;
  var
    P: TPair<string, TObject>;
  begin
    Result := TDictionary<string, TObject>.Create(AIn);
    for P in AOut do
      Result.AddOrSetValue(P.Key, P.Value);
  end;

  procedure AddOut(ARow, AIn: TDictionary<string, TObject>);
  var
    R: TGQLOutRow;
    Bind: TDictionary<string, TObject>;
    J: Integer;
  begin
    R.Row := ARow;
    R.Index := Outs.Count;
    SetLength(R.Sort, AStage.OrderBy.Count);
    if AStage.OrderBy.Count > 0 then
    begin
      Bind := Merge(AIn, ARow);
      try
        for J := 0 to AStage.OrderBy.Count - 1 do
          R.Sort[J] := SortValue(AStage.OrderBy[J].Expr, Bind);
      finally
        Bind.Free;
      end;
    end;
    Outs.Add(R);
  end;

  function BuildRow(ABind: TDictionary<string, TObject>): TDictionary<string, TObject>;
  var
    It: TGQLReturnItem;
  begin
    Result := TDictionary<string, TObject>.Create;
    try
      for It in AStage.Items do
        Result.Add(It.Name, MakeCell(It.Expr, ABind));
    except
      FreeRow(Result);
      raise;
    end;
  end;

  procedure ComputeAgg(AAgg: TAggregateExpr; ARows: TList<TDictionary<string, TObject>>);
  var
    Row: TDictionary<string, TObject>;
    Obj: TObject;
    V, Best: Variant;
    K: string;
    Seen2: TDictionary<string, Boolean>;
    Values: TList<Variant>;
    Count, NumCount, Cmp, J: Integer;
    Sum, D: Double;
    Arr: Variant;
  begin
    Count := 0;
    NumCount := 0;
    Sum := 0;
    Best := Null;
    Seen2 := TDictionary<string, Boolean>.Create;
    Values := TList<Variant>.Create;
    try
      for Row in ARows do
      begin
        if AAgg.Arg = nil then // count(*)
        begin
          Inc(Count);
          Continue;
        end;
        if BoundElement(AAgg.Arg, Row, Obj) then
        begin
          V := GQLCellText(Obj);
          K := GQLCellKey(Obj);
        end
        else
        begin
          V := EvaluateGraphExpression(AAgg.Arg, Row);
          if GQLIsNull(V) then
            Continue;
          K := GQLValueKey(V);
        end;
        if AAgg.Distinct then
        begin
          if Seen2.ContainsKey(K) then
            Continue;
          Seen2.Add(K, True);
        end;
        Inc(Count);
        case AAgg.Func of
          agSum, agAvg:
            if GQLTryNumber(V, D) then
            begin
              Sum := Sum + D;
              Inc(NumCount);
            end;
          agMin:
            if GQLIsNull(Best) or (GQLCompare(V, Best, Cmp) and (Cmp < 0)) then
              Best := V;
          agMax:
            if GQLIsNull(Best) or (GQLCompare(V, Best, Cmp) and (Cmp > 0)) then
              Best := V;
          agCollect:
            Values.Add(V);
        end;
      end;

      case AAgg.Func of
        agCount:
          AAgg.Value := Count;
        agSum:
          AAgg.Value := Sum;
        agAvg:
          if NumCount > 0 then
            AAgg.Value := Sum / NumCount
          else
            AAgg.Value := Null;
        agMin, agMax:
          AAgg.Value := Best;
        agCollect:
          begin
            Arr := VarArrayCreate([0, Values.Count - 1], varVariant);
            for J := 0 to Values.Count - 1 do
              Arr[J] := Values[J];
            AAgg.Value := Arr;
          end;
      end;
    finally
      Values.Free;
      Seen2.Free;
    end;
  end;

begin
  Result := [];
  Aggs := TList<TAggregateExpr>.Create;
  Outs := TList<TGQLOutRow>.Create;
  Empty := TDictionary<string, TObject>.Create;
  try
    try
      for Item in AStage.Items do
        CollectAggs(Item.Expr);
      for Order in AStage.OrderBy do
        CollectAggs(Order.Expr);

      if Aggs.Count = 0 then
      begin
        // --- Sin agregados: una fila de salida por fila de entrada ---
        for InRow in AInput do
          AddOut(BuildRow(InRow), InRow);
      end
      else
      begin
        // --- Con agregados: agrupar por las columnas que no agregan ---
        HasKeyItems := False;
        for Item in AStage.Items do
          if not HasAgg(Item.Expr) then
            HasKeyItems := True;

        GroupIdx := TDictionary<string, Integer>.Create;
        Groups := TObjectList < TList < TDictionary<string, TObject> >>.Create(True);
        try
          for InRow in AInput do
          begin
            Key := '';
            for Item in AStage.Items do
              if not HasAgg(Item.Expr) then
                Key := Key + ExprKey(Item.Expr, InRow) + #1;
            if not GroupIdx.TryGetValue(Key, I) then
            begin
              I := Groups.Add(TList < TDictionary < string, TObject >>.Create);
              GroupIdx.Add(Key, I);
            end;
            Groups[I].Add(InRow);
          end;

          // count(*) sin columnas de agrupación y sin filas: una fila con 0
          if (Groups.Count = 0) and not HasKeyItems then
            Groups.Add(TList < TDictionary < string, TObject >>.Create);

          for Group in Groups do
          begin
            for Agg in Aggs do
              ComputeAgg(Agg, Group);
            if Group.Count > 0 then
              First := Group[0]
            else
              First := Empty;
            AddOut(BuildRow(First), First);
          end;
        finally
          Groups.Free;
          GroupIdx.Free;
        end;
      end;

      // --- WHERE de WITH (sobre las columnas, antes de ordenar: el HAVING de SQL) ---
      if Assigned(AStage.Where) then
        for I := Outs.Count - 1 downto 0 do
          if not GQLToBool(EvaluateGraphExpression(AStage.Where, Outs[I].Row)) then
          begin
            FreeRow(Outs[I].Row);
            Outs.Delete(I);
          end;

      // --- DISTINCT ---
      if AStage.Distinct then
      begin
        Seen := TDictionary<string, Boolean>.Create;
        try
          for I := 0 to Outs.Count - 1 do
          begin
            Key := '';
            for Item in AStage.Items do
            begin
              if not Outs[I].Row.TryGetValue(Item.Name, CellObj) then
                CellObj := nil;
              Key := Key + GQLCellKey(CellObj) + #1;
            end;
            if Seen.ContainsKey(Key) then
            begin
              FreeRow(Outs[I].Row);
              OR1 := Outs[I];
              OR1.Row := nil;
              Outs[I] := OR1;
            end
            else
              Seen.Add(Key, True);
          end;
          for I := Outs.Count - 1 downto 0 do
            if Outs[I].Row = nil then
              Outs.Delete(I);
        finally
          Seen.Free;
        end;
      end;

      // --- ORDER BY (NULL va al final en ASC y al principio en DESC; empate: orden de llegada) ---
      if AStage.OrderBy.Count > 0 then
        Outs.Sort(TComparer<TGQLOutRow>.Construct(
          function(const A, B: TGQLOutRow): Integer
          var
            J, C: Integer;
            NA, NB: Boolean;
          begin
            for J := 0 to AStage.OrderBy.Count - 1 do
            begin
              NA := GQLIsNull(A.Sort[J]);
              NB := GQLIsNull(B.Sort[J]);
              if NA and NB then
                C := 0
              else if NA then
                C := 1
              else if NB then
                C := -1
              else if not GQLCompare(A.Sort[J], B.Sort[J], C) then
                C := 0;
              if AStage.OrderBy[J].Descending then
                C := -C;
              if C <> 0 then
                Exit(C);
            end;
            Result := A.Index - B.Index;
          end));

      // --- SKIP / LIMIT ---
      FromIdx := 0;
      if AStage.Skip > 0 then
        FromIdx := AStage.Skip;
      ToIdx := Outs.Count - 1;
      if (AStage.Limit >= 0) and (FromIdx + AStage.Limit - 1 < ToIdx) then
        ToIdx := FromIdx + AStage.Limit - 1;

      for I := 0 to Outs.Count - 1 do
        if (I < FromIdx) or (I > ToIdx) then
          FreeRow(Outs[I].Row)
        else
          Result := Result + [Outs[I].Row];
      Outs.Clear;
    except
      for I := 0 to Outs.Count - 1 do
        FreeRow(Outs[I].Row);
      Outs.Clear;
      raise;
    end;
  finally
    Empty.Free;
    Outs.Free;
    Aggs.Free;
  end;
end;

function TAiRagGraph.EvaluateGQLFunction(AFunc: TFunctionExpr; ABoundElements: TDictionary<string, TObject>): Variant;
var
  N, S: string;
  A: TArray<Variant>;
  Obj: TObject;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  D, F: Double;
  I, Start, Len, Decimals: Integer;
  Dt: TDateTime;

  function ArgInt(AIndex: Integer; out AValue: Integer): Boolean;
  var
    X: Double;
  begin
    Result := GQLTryNumber(A[AIndex], X);
    if Result then
      AValue := Trunc(X);
  end;

begin
  Result := Null;
  N := AFunc.Name;

  // --- Funciones de grafo: trabajan sobre el elemento enlazado a la variable ---
  if (N = 'id') or (N = 'label') or (N = 'labels') or (N = 'type') or (N = 'degree') or (N = 'indegree') or
    (N = 'outdegree') or (N = 'keys') then
  begin
    Obj := nil;
    if (AFunc.Args.Count > 0) and (AFunc.Args[0] is TVariableExpr) then
      ABoundElements.TryGetValue(TVariableExpr(AFunc.Args[0]).Variable, Obj);

    if Obj is TAiRagGraphNode then
    begin
      Node := TAiRagGraphNode(Obj);
      if N = 'id' then
        Result := Node.ID
      else if N = 'label' then
        Result := Node.NodeLabel
      else if N = 'labels' then
        Result := GQLStringsToVarArray([Node.NodeLabel])
      else if N = 'degree' then
        Result := Node.IncomingEdges.Count + Node.OutgoingEdges.Count
      else if N = 'indegree' then
        Result := Node.IncomingEdges.Count
      else if N = 'outdegree' then
        Result := Node.OutgoingEdges.Count
      else if N = 'keys' then
        Result := GQLStringsToVarArray(Node.MetaData.InternalDictionary.Keys.ToArray);
    end
    else if Obj is TAiRagGraphEdge then
    begin
      Edge := TAiRagGraphEdge(Obj);
      if N = 'id' then
        Result := Edge.ID
      else if (N = 'label') or (N = 'type') then
        Result := Edge.EdgeLabel
      else if N = 'keys' then
        Result := GQLStringsToVarArray(Edge.MetaData.InternalDictionary.Keys.ToArray);
    end;
    Exit;
  end;

  // --- El resto recibe valores ---
  SetLength(A, AFunc.Args.Count);
  for I := 0 to AFunc.Args.Count - 1 do
    A[I] := EvaluateGraphExpression(AFunc.Args[I], ABoundElements);

  if N = 'coalesce' then
  begin
    for I := 0 to High(A) do
      if not GQLIsNull(A[I]) then
        Exit(A[I]);
    Exit;
  end;
  if N = 'exists' then
    Exit(not GQLIsNull(A[0]));

  // Fechas sin argumento
  if (Length(A) = 0) and (N = 'date') then
    Exit(VarFromDateTime(Date));
  if (Length(A) = 0) and (N = 'datetime') then
    Exit(VarFromDateTime(Now));

  // Un NULL en cualquier argumento da NULL
  for I := 0 to High(A) do
    if GQLIsNull(A[I]) then
      Exit;

  // --- Texto ---
  if N = 'lower' then
    Result := AnsiLowerCase(GQLToText(A[0]))
  else if N = 'upper' then
    Result := AnsiUpperCase(GQLToText(A[0]))
  else if N = 'trim' then
    Result := Trim(GQLToText(A[0]))
  else if N = 'ltrim' then
    Result := TrimLeft(GQLToText(A[0]))
  else if N = 'rtrim' then
    Result := TrimRight(GQLToText(A[0]))
  else if N = 'substring' then
  begin
    // Como en Cypher: inicio desde 0
    S := GQLToText(A[0]);
    if not ArgInt(1, Start) or (Start < 0) then
      Exit;
    if Start > Length(S) then
      Exit('');
    Len := Length(S) - Start;
    if (Length(A) > 2) and (not ArgInt(2, Len) or (Len < 0)) then
      Exit;
    Result := S.Substring(Start, Len);
  end
  else if (N = 'left') or (N = 'right') then
  begin
    S := GQLToText(A[0]);
    if not ArgInt(1, Len) or (Len < 0) then
      Exit;
    if N = 'left' then
      Result := Copy(S, 1, Len)
    else if Len >= Length(S) then
      Result := S
    else
      Result := Copy(S, Length(S) - Len + 1, Len);
  end
  else if N = 'replace' then
    Result := StringReplace(GQLToText(A[0]), GQLToText(A[1]), GQLToText(A[2]), [rfReplaceAll])
  else if N = 'split' then
    Result := GQLStringsToVarArray(GQLToText(A[0]).Split([GQLToText(A[1])]))
  else if N = 'reverse' then
    Result := ReverseString(GQLToText(A[0]))
  else if (N = 'size') or (N = 'length') then
  begin
    if VarIsArray(A[0]) then
      Result := VarArrayHighBound(A[0], 1) - VarArrayLowBound(A[0], 1) + 1
    else
      Result := Length(GQLToText(A[0]));
  end
  else if N = 'tostring' then
    Result := GQLToText(A[0])

  // --- Listas ---
  else if (N = 'head') or (N = 'last') then
  begin
    if VarIsArray(A[0]) and (VarArrayHighBound(A[0], 1) >= VarArrayLowBound(A[0], 1)) then
    begin
      if N = 'head' then
        Result := A[0][VarArrayLowBound(A[0], 1)]
      else
        Result := A[0][VarArrayHighBound(A[0], 1)];
    end;
  end

  // --- Fechas: aceptan una fecha o texto ISO 8601 ---
  else if (N = 'date') or (N = 'datetime') or (N = 'year') or (N = 'month') or (N = 'day') then
  begin
    if not GQLTryDate(A[0], Dt) then
      Exit;
    if N = 'date' then
      Result := VarFromDateTime(DateOf(Dt))
    else if N = 'datetime' then
      Result := VarFromDateTime(Dt)
    else if N = 'year' then
      Result := YearOf(Dt)
    else if N = 'month' then
      Result := MonthOf(Dt)
    else
      Result := DayOf(Dt);
  end

  // --- Números ---
  else
  begin
    if not GQLTryNumber(A[0], D) then
      Exit;
    if N = 'tointeger' then
      Result := Int64(Trunc(D))
    else if N = 'tofloat' then
      Result := D
    else if N = 'abs' then
      Result := Abs(D)
    else if N = 'round' then
    begin
      // Mitades hacia afuera (2.5 -> 3, -2.5 -> -3), no el redondeo bancario de Round()
      Decimals := 0;
      if (Length(A) > 1) and not ArgInt(1, Decimals) then
        Exit;
      if Decimals < 0 then
        Decimals := 0
      else if Decimals > 10 then
        Decimals := 10;
      F := 1;
      for I := 1 to Decimals do
        F := F * 10;
      if D < 0 then
        Result := -Int(-D * F + 0.5) / F
      else
        Result := Int(D * F + 0.5) / F;
    end
    else if N = 'floor' then
    begin
      if (D < 0) and (Frac(D) <> 0) then
        Result := Int(D) - 1
      else
        Result := Int(D);
    end
    else if N = 'ceil' then
    begin
      if (D > 0) and (Frac(D) <> 0) then
        Result := Int(D) + 1
      else
        Result := Int(D);
    end
    else if N = 'sqrt' then
    begin
      if D >= 0 then
        Result := Sqrt(D);
    end
    else if N = 'sign' then
    begin
      if D > 0 then
        Result := 1
      else if D < 0 then
        Result := -1
      else
        Result := 0;
    end;
  end;
end;

function TAiRagGraph.EvaluateGraphExpression(AExpr: TGraphExpression; ABoundElements: TDictionary<string, TObject>): Variant;
var
  Left, Right, Lo, Hi: Variant;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Obj: TObject;
  B: TBinaryExpr;
  U: TUnaryExpr;
  Btw: TBetweenExpr;
  P: TPropertyExpr;
  Cmp, CmpLo, CmpHi: Integer;
  D, DL, DR: Double;
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
            else if SameText(P.PropertyKey, 'weight') then
              Result := Edge.Weight
            else
              Result := Edge.MetaData.Get(P.PropertyKey, Null);
          end;
        end;
      end;

    // ================================================================
    // VARIABLE suelta: el ID del elemento (NULL si no está enlazada)
    // ================================================================
    ekVariable:
      begin
        Result := Null;
        if ABoundElements.TryGetValue(TVariableExpr(AExpr).Variable, Obj) then
        begin
          if Obj is TAiRagGraphNode then
            Result := TAiRagGraphNode(Obj).ID
          else if Obj is TAiRagGraphEdge then
            Result := TAiRagGraphEdge(Obj).ID
          else if Obj is TGQLValue then
            Result := TGQLValue(Obj).Data; // Columna de un WITH anterior
        end;
      end;

    // ================================================================
    // AGREGADO: el ejecutor ya dejó el valor del grupo
    // ================================================================
    ekAggregate:
      Result := TAggregateExpr(AExpr).Value;

    // ================================================================
    // FUNCIÓN
    // ================================================================
    ekFunction:
      Result := EvaluateGQLFunction(TFunctionExpr(AExpr), ABoundElements);

    // ================================================================
    // UNARY: NOT, signo negativo
    // ================================================================
    ekUnary:
      begin
        U := TUnaryExpr(AExpr);
        Left := EvaluateGraphExpression(U.Operand, ABoundElements);
        case U.Op of
          uoNot:
            Result := not GQLToBool(Left);
          uoNegate:
            if GQLTryNumber(Left, D) then
              Result := -D
            else
              Result := Null;
        end;
      end;

    // ================================================================
    // BETWEEN (extremos incluidos)
    // ================================================================
    ekBetween:
      begin
        Btw := TBetweenExpr(AExpr);
        Left := EvaluateGraphExpression(Btw.Value, ABoundElements);
        Lo := EvaluateGraphExpression(Btw.Low, ABoundElements);
        Hi := EvaluateGraphExpression(Btw.High, ABoundElements);
        Result := GQLCompare(Left, Lo, CmpLo) and GQLCompare(Left, Hi, CmpHi) and (CmpLo >= 0) and (CmpHi <= 0);
      end;

    // ================================================================
    // BINARY / LOGICAL
    // ================================================================
    ekBinary:
      begin
        B := TBinaryExpr(AExpr);

        Left := EvaluateGraphExpression(B.Left, ABoundElements);

        // AND / OR evalúan el lado derecho solo si hace falta
        if B.Op = opAnd then
          Exit(GQLToBool(Left) and GQLToBool(EvaluateGraphExpression(B.Right, ABoundElements)));
        if B.Op = opOr then
          Exit(GQLToBool(Left) or GQLToBool(EvaluateGraphExpression(B.Right, ABoundElements)));

        // Operadores unarios
        if not(B.Op in [opIsNull, opIsNotNull]) then
          Right := EvaluateGraphExpression(B.Right, ABoundElements)
        else
          Right := Null;

        case B.Op of
          // --- LOGICAL ---
          opXor:
            Result := GQLToBool(Left) xor GQLToBool(Right);

          // --- ARITMÉTICA (un NULL da NULL; división por cero, NULL) ---
          opAdd:
            if GQLIsNull(Left) or GQLIsNull(Right) then
              Result := Null
            else if GQLTryNumber(Left, DL) and GQLTryNumber(Right, DR) then
              Result := DL + DR
            else
              Result := GQLToText(Left) + GQLToText(Right); // Concatenación
          opSub, opMul, opDiv, opMod:
            if not(GQLTryNumber(Left, DL) and GQLTryNumber(Right, DR)) then
              Result := Null
            else
              case B.Op of
                opSub:
                  Result := DL - DR;
                opMul:
                  Result := DL * DR;
                opDiv:
                  if DR = 0 then
                    Result := Null
                  else
                    Result := DL / DR;
              else
                if DR = 0 then
                  Result := Null
                else
                  Result := DL - DR * Int(DL / DR); // Resto con el signo del dividendo
              end;

          // --- COMPARISON ---
          opEqual:
            Result := GQLEquals(Left, Right);
          opNotEqual:
            Result := not GQLEquals(Left, Right);
          opGreater:
            Result := GQLCompare(Left, Right, Cmp) and (Cmp > 0);
          opGreaterEqual:
            Result := GQLCompare(Left, Right, Cmp) and (Cmp >= 0);
          opLess:
            Result := GQLCompare(Left, Right, Cmp) and (Cmp < 0);
          opLessEqual:
            Result := GQLCompare(Left, Right, Cmp) and (Cmp <= 0);

          // --- TEXT (CONTAINS, STARTS/ENDS WITH e ILIKE no distinguen mayúsculas) ---
          opContains:
            Result := not(GQLIsNull(Left) or GQLIsNull(Right)) and ContainsText(VarToStr(Left), VarToStr(Right));
          opStartsWith:
            Result := not(GQLIsNull(Left) or GQLIsNull(Right)) and StartsText(VarToStr(Right), VarToStr(Left));
          opEndsWith:
            Result := not(GQLIsNull(Left) or GQLIsNull(Right)) and EndsText(VarToStr(Right), VarToStr(Left));
          opLike:
            Result := not(GQLIsNull(Left) or GQLIsNull(Right)) and GQLLikeMatch(VarToStr(Left), VarToStr(Right));
          opILike:
            Result := not(GQLIsNull(Left) or GQLIsNull(Right)) and GQLLikeMatch(AnsiLowerCase(VarToStr(Left)),
              AnsiLowerCase(VarToStr(Right)));

          // --- NULL ---
          opIsNull:
            Result := GQLIsNull(Left);
          opIsNotNull:
            Result := not GQLIsNull(Left);

          // --- IN ---
          opIn:
            Result := GQLInList(Left, Right);
          opNotIn:
            Result := not GQLInList(Left, Right); // Un valor ausente NO está en la lista

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

  // Variables para resoluci�n de algoritmos
  StartNode, EndNode: TAiRagGraphNode;
  FinalDepth: Integer;

  // -----------------------------------------------------------------------
  // Funci�n auxiliar anidada:
  // -----------------------------------------------------------------------
  function FindNodeByPattern(APattern: TMatchNodePattern): TAiRagGraphNode;
  var
  Candidates: TArray<TAiRagGraphNode>;
  Cand: TAiRagGraphNode;
  begin
  Result := nil;
  if APattern = nil then
  Exit;

  // 1. Optimizaci�n: Si hay etiqueta, buscamos solo en ese �ndice
  if not APattern.NodeLabel.IsEmpty then
  Candidates := Self.FindNodesByLabel(APattern.NodeLabel)
  else
  Candidates := FNodeRegistry.Values.ToArray;

  // 2. B�squeda lineal sobre los candidatos para coincidir propiedades
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
  // Parseamos el c�digo.
  // Si es un comando especial, QueryObj ser� nil y CommandType tendr� valor.
  // Si es una consulta normal, QueryObj tendr� el objeto y CommandType ser� cmdNone.
  QueryObj := Parser.Parse;
  try

  if Parser.CommandType <> cmdNone then
  begin
  // =========================================================
  // EJECUCI�N DE COMANDOS
  // =========================================================
  case Parser.CommandType of

  // -------------------------------------------------------
  // 1. INTROSPECCI�N (SHOW ...)
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
  // 2. ALGORITMOS: CAMINO M�S CORTO
  // -------------------------------------------------------
  cmdShortestPath:
  begin
  // Resolvemos los patrones a nodos reales usando la funci�n anidada
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
  // Si StartNode o EndNode son nil, devuelve array vac�o (no encontrado)
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
  // Obtenemos los N nodos con m�s conexiones
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
  // CONSULTA MATCH EST�NDAR
  // =========================================================
  else if Assigned(QueryObj) then
  begin
  // Ejecutamos la l�gica cl�sica de Matching (Nodos y Relaciones)

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

class procedure TAiRagGraph.FreeGQLResults(var AResults: TArray<TDictionary<string, TObject>>);
var
  Dict: TDictionary<string, TObject>;
  Obj: TObject;
begin
  for Dict in AResults do
    if Assigned(Dict) then
    begin
      for Obj in Dict.Values do
        if Obj is TStringWrapper then
          Obj.Free;
      Dict.Free;
    end;
  AResults := nil;
end;

function TAiRagGraph.ExecuteMakerGQL(const ACode: string): string;
var
  Data: TArray<TDictionary<string, TObject>>;
begin
  Data := nil;
  try
    Result := ExecuteMakerGQL(ACode, nil, Data, 0);
  finally
    FreeGQLResults(Data);
  end;
end;

function TAiRagGraph.ExecuteMakerGQL(const ACode: string; out AResultObjects: TArray<TDictionary<string, TObject>>; ADepth: Integer): string;
begin
  Result := ExecuteMakerGQL(ACode, nil, AResultObjects, ADepth);
end;

function TAiRagGraph.ExecuteMakerGQL(const ACode: string; const AParams: array of Variant): string;
var
  Data: TArray<TDictionary<string, TObject>>;
begin
  Data := nil;
  try
    Result := ExecuteMakerGQL(ACode, AParams, Data, 0);
  finally
    FreeGQLResults(Data);
  end;
end;

function TAiRagGraph.ExecuteMakerGQL(const ACode: string; const AParams: array of Variant; out AResultObjects: TArray<TDictionary<string, TObject>>;
  ADepth: Integer): string;
var
  Params: TDictionary<string, Variant>;
  Name: string;
  I: Integer;
begin
  AResultObjects := [];
  if Odd(Length(AParams)) then
    raise EGQLParserError.CreateAt(AiStr('rag.gql.param_pairs'), 0);

  // Nombres sin distinguir mayúsculas; se acepta 'nivel' o '$nivel'
  Params := TDictionary<string, Variant>.Create(TIStringComparer.Ordinal);
  try
    I := 0;
    while I < Length(AParams) do
    begin
      Name := VarToStr(AParams[I]);
      if Name.StartsWith('$') then
        Name := Name.Substring(1);
      Params.AddOrSetValue(Name, AParams[I + 1]);
      Inc(I, 2);
    end;
    Result := ExecuteMakerGQL(ACode, Params, AResultObjects, ADepth);
  finally
    Params.Free;
  end;
end;

function TAiRagGraph.ExecuteMakerGQL(const ACode: string; AParams: TDictionary<string, Variant>; out AResultObjects: TArray<TDictionary<string, TObject>>;
  ADepth: Integer): string;
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
  ElementType: string;

  // Variables para resoluci�n de algoritmos
  StartNode, EndNode: TAiRagGraphNode;
  FinalDepth: Integer;

  // Variables para la construcci�n del contexto de texto
  ContextNodes: TList<TAiRagGraphNode>;
  ContextBuilder: TStringBuilder;

  // Modo tabla
  Rows, NextRows: TArray<TDictionary<string, TObject>>;
  Stage: TGQLProjection;

  // --- Funci�n auxiliar FindNodeByPattern (se mantiene igual) ---
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

  Parser := TGraphParser.Create(ACode, AParams);
  try
    QueryObj := Parser.Parse;
    try
      if Parser.CommandType <> cmdNone then
      begin
        // =========================================================
        // EJECUCI�N DE COMANDOS (Algoritmos / Introspecci�n)
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
      // CONSULTA MATCH EST�NDAR (La m�s com�n)
      // =========================================================
      else if Assigned(QueryObj) and QueryObj.Tabular then
      begin
        // =========================================================
        // RETURN con columnas, agregados, ORDER BY, LIMIT o WITH: tabla
        // =========================================================
        Rows := Self.Match(QueryObj, 0); // DEPTH no aplica a una tabla
        try
          for Stage in QueryObj.Projections do
          begin
            NextRows := RunGQLProjection(Stage, Rows);
            FreeGQLResults(Rows);
            Rows := NextRows;
          end;
          Result := GQLRowsToText(QueryObj.Projections.Last, Rows);
          AResultObjects := Rows;
          Rows := nil;
        except
          FreeGQLResults(Rows);
          raise;
        end;
      end
      else if Assigned(QueryObj) then
      begin
        if QueryObj.Depth > 0 then
          FinalDepth := QueryObj.Depth
        else
          FinalDepth := ADepth;

        // 1. Ejecutar Match
        AResultObjects := Self.Match(QueryObj, FinalDepth);

        // 2. Extraer Nodos �nicos para generar el contexto
        ContextNodes := TList<TAiRagGraphNode>.Create;
        try
          for ResDict in AResultObjects do
          begin
            // Los resultados del Match pueden venir mezclados (nodos, aristas, valores)
            // Extraemos solo los nodos para pas�rselos al generador de contexto.
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
  // Usamos un diccionario para registrar los nodos ya procesados o a�adidos.
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

    // Expansi�n BFS limitada por profundidad
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

        // Expandir hacia atr�s (incoming)
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
  // 1. Crear el nuevo grafo con la misma configuraci�n de embeddings
  Result := TAiRagGraph.Create(nil);
  Result.Embeddings := Self.Embeddings;

  if Length(ANodes) = 0 then
    Exit;

  // Diccionario para rastrear qu� nodos se incluyeron en el subgrafo
  NodeSet := TDictionary<string, Boolean>.Create;
  try
    // BLOQUEAR ACTUALIZACIONES: Crucial para el rendimiento.
    // Evita que los �ndices (BM25/HNSW) se reconstruyan nodo por nodo.
    Result.BeginUpdate;
    try
      // --- FASE 1: Clonar todos los Nodos seleccionados ---
      for Node in ANodes do
      begin
        NodeSet.Add(Node.ID, True);

        // Creamos el nodo en el nuevo grafo manteniendo ID, Label y Name
        NewNode := Result.AddNode(Node.ID, Node.NodeLabel, Node.Name);
        NewNode.Model := Node.Model;

        // COPIA DE TEXTO: Importante para que el subgrafo funcione con b�squedas l�xicas
        NewNode.Text := Node.Text;

        // COPIA DE METADATOS: Reemplaza el bucle manual de FProperties.
        // Copia tipos reales (fechas, n�meros, booleanos) sin p�rdida.
        NewNode.MetaData.Assign(Node.MetaData);

        // COPIA DE VECTOR (Resumen): Copia el embedding principal del nodo
        if Length(Node.Data) > 0 then
        begin
          NewNode.SetDataLength(Length(Node.Data));
          NewNode.Data := Copy(Node.Data); // Copy es m�s seguro que Move para arrays din�micos
        end;

        // COPIA DE CHUNKS: Nueva funcionalidad (Opci�n 1)
        // Garantiza que la entidad mantenga todos sus fragmentos de texto
        for Chunk in Node.Chunks do
        begin
          NewNode.AddChunk(Chunk.Text, Chunk.Data);
        end;
      end;

      // --- FASE 2: Clonar Aristas Internas ---
      // Solo aquellas cuyas entidades origen y destino est�n presentes en la selecci�n
      for Node in ANodes do
      begin
        for Edge in Node.OutgoingEdges do
        begin
          // Verificar si el nodo destino tambi�n fue seleccionado
          if NodeSet.ContainsKey(Edge.ToNode.ID) then
          begin
            // IMPORTANTE: Obtener las referencias de los nodos EN EL NUEVO GRAFO
            NewSource := Result.FindNodeByID(Edge.FromNode.ID);
            NewTarget := Result.FindNodeByID(Edge.ToNode.ID);

            if (NewSource <> nil) and (NewTarget <> nil) then
            begin
              // Recreamos la arista con todos los par�metros originales (ID, Label, Name, Weight)
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
      // Dispara la reconstrucci�n de �ndices una sola vez para todo el subgrafo
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
  // C�digo original
  FNodeRegistry.Clear;
  FEdgeRegistry.Clear;

  // Liberar las listas de nodos dentro del �ndice de etiquetas antes de limpiarlo
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
  // Usamos el �ndice para m�xima eficiencia
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
  // Determinamos la dimensi�n para el objeto
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
  // Nota: No se a�ade a FNodeRegistry ni a FNodes todav�a.
end;

procedure TAiRagGraph.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  // Si un componente se est� eliminando (opRemove)
  if Operation = opRemove then
  begin
    // �Es nuestro motor de embeddings?
    if AComponent = FEmbeddings then
      FEmbeddings := nil;

    // �Es nuestro driver?
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
    // Si ya exist�a un nodo con ese ID, liberamos el que intentamos a�adir
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
  // Centralizamos la detecci�n de dimensiones
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
  // 1. Usamos la f�brica para crear el objeto completo en memoria
  Result := NewEdge(AFromNode, AToNode, AID, ALabel, AName, AWeight);

  try
    // 2. Llamamos al overload de objeto para registrar y persistir
    Result := AddEdge(Result);
  except
    // Si falla el registro o la persistencia, NewEdge no fue adoptado,
    // as� que debemos liberarlo.
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
    // Si ya exist�a una arista con ese ID, liberamos la nueva
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
    // 2. Realizar la operaci�n en memoria PRIMERO
    UnregisterNode(Node); // Esto quita el nodo de FNodeRegistry y FNodes
    // Tambi�n debes desregistrar las aristas conectadas
    for var Edge in EdgesToDelete do
      UnregisterEdge(Edge);

    // 3. Persistir el cambio en la BD DESPU�S
    if Assigned(FDriver) then
    begin
      try
        FDriver.DeleteNode(AID);
        // Tambi�n necesitar�as un OnGraphDeleteEdge para las aristas
        // o que OnGraphDeleteNode se encargue de borrar en cascada.
      except
        // Si la BD falla, �tenemos que restaurar el estado en memoria!
        // Esto es complejo: requerir�a volver a a�adir el nodo y las aristas.
        // Por simplicidad, por ahora solo relanzamos. Una soluci�n completa
        // requerir�a un patr�n "Unit of Work".
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
  // Solo actualizamos los �ndices en tiempo real si no estamos en modo batch
  if FUpdateCount = 0 then
  begin
    // 1. Eliminar del �ndice de etiquetas
    if FNodeLabelIndex.TryGetValue(ANode.NodeLabel, NodeList) then
    begin
      NodeList.Remove(ANode);
      if NodeList.Count = 0 then
      begin
        FNodeLabelIndex.Remove(ANode.NodeLabel);
        NodeList.Free;
      end;
    end;

    // 2. Eliminar del �ndice de nombres
    if not ANode.Name.IsEmpty then
    begin
      CombinedNameKey := ANode.NodeLabel + '#' + ANode.Name;
      FNodeNameIndex.Remove(CombinedNameKey);
    end;
  end;
  // --> FIN DE CAMBIOS

  // C�digo original (esto siempre se ejecuta)
  if FNodeRegistry.ContainsKey(ANode.ID) then
    FNodeRegistry.Remove(ANode.ID);
  FNodes.Items.Remove(ANode);
end;

procedure TAiRagGraph.UpdateCommunityLabels;
var
  Communities: TDictionary<TAiRagGraphNode, Integer>;
  Node: TAiRagGraphNode;
begin
  // 1. Ejecutar el algoritmo de detecci�n de comunidades (ej: Louvain)
  Communities := DetectCommunities;
  try
    // 2. Recorrer los resultados (Nodo -> ID de Comunidad)
    for Node in Communities.Keys do
    begin
      // 3. Guardar el ID en el MetaData unificado.
      // Al ser un Integer, se guarda como Variant correctamente, permitiendo
      // que luego se exporte a JSON o Postgres como un n�mero real.
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
  // Es m�s eficiente iterar sobre las aristas salientes del nodo de origen,
  // ya que es una lista mucho m�s peque�a que todas las aristas del grafo.
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
  // 1. Buscar en cach�.
  if FEdgeRegistry.TryGetValue(AID, Result) then
    Exit;

  // 2. Pedir datos a la BD (SOLO Driver).
  Result := nil;
  FoundInDB := False;

  if Assigned(FDriver) then
  begin
    // Limpiamos el record antes de pasarlo al driver.
    // Aunque el driver debe ser defensivo, es buena pr�ctica inicializar.
    FillChar(EdgeData, SizeOf(TEdgeDataRecord), 0);

    // CORRECCI�N CLAVE: Llamar al m�todo FindEdgeDataByID del driver.
    FoundInDB := FDriver.FindEdgeDataByID(AID, EdgeData);
  end;

  // Si no se encontr� en el driver o el driver no estaba asignado, FoundInDB es False.

  // 3. Usar el nuevo helper para hidratar.
  if FoundInDB then
    Result := InternalHydrateEdge(EdgeData);
end;

function TAiRagGraph.FindNodeByID(AID: string): TAiRagGraphNode;
var
  FoundInDB: Boolean;
  NodeData: TNodeDataRecord;
begin
  // 1. Buscar en cach�.
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
  CombinedNameKey: string; // Para la l�gica en memoria
begin
  Result := nil;

  // 1. Intentar delegar a la base de datos a trav�s del Driver.
  if Assigned(FDriver) then
  begin
    // El driver realiza la b�squeda en la BD y devuelve el objeto hidratado
    // (o nil si no lo encuentra).
    Result := FDriver.FindNodeByName(AName, ANodeLabel);
    // Si el driver encontr� algo, salimos inmediatamente.
    if Result <> nil then
      Exit;
  end;

  // 2. Fallback a la l�gica en memoria (si el Driver no fue usado o no encontr� nada)
  if AName.IsEmpty or ANodeLabel.IsEmpty then
    Exit;

  // Intentar encontrar en el �ndice en memoria (solo para nodos ya cargados)
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
    // El driver realiza la b�squeda eficiente en la BD y devuelve el array de nombres.
    Result := FDriver.FindNodeNamesByLabel(ANodeLabel, ASearchText, ALimit);

    // Si la BD devolvi� resultados, salimos.
    if Length(Result) > 0 then
      Exit;

    // NOTA: Si la BD devuelve un array vac�o, pasamos al fallback en memoria (Paso 2).
  end;

  // 2. Fallback a la l�gica en memoria si no hay resultados o si no hay driver.
  Results := TStringList.Create;
  try
    // Usamos el �ndice de etiquetas para ser eficientes
    if FNodeLabelIndex.TryGetValue(ANodeLabel, NodeList) then
    begin
      for Node in NodeList do
      begin
        if ContainsText(LowerCase(Node.Name), LowerCase(ASearchText)) then
        begin
          Results.Add(Node.Name);
          if Results.Count >= ALimit then
            Break; // Detenerse cuando alcanzamos el l�mite
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
  NodeList: TList<TAiRagGraphNode>; // Para la l�gica en memoria
begin
  Result := [];

  // 1. Intentar delegar al Driver (BD)
  if Assigned(FDriver) then
  begin
    // El driver realiza la b�squeda en la BD, se encarga de la hidrataci�n
    // (usando FindNodeByID) y devuelve el array de nodos.
    Result := FDriver.FindNodesByLabel(ALabel);

    // Si el driver devolvi� alg�n resultado, salimos.
    if Length(Result) > 0 then
      Exit;

    // Si el driver devolvi� un array vac�o, pasamos al fallback en memoria (Paso 2).
  end;

  // 2. Fallback a la l�gica en memoria (si no hay resultados o si no hay driver)
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
    // El driver realiza la b�squeda eficiente indexada en la BD
    Result := FDriver.FindNodesByProperty(AKey, AValue);

    if Length(Result) > 0 then
      Exit;
  end;

  // 2. Fallback a la l�gica en memoria
  Results := TList<TAiRagGraphNode>.Create;
  try
    for Node in FNodeRegistry.Values do
    begin
      // --- A. Manejo de atributos nativos (Identidad) ---
      if SameText(AKey, 'name') then
      begin
        // Usamos SameText para nombres (insensible a may�sculas)
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
      // --- B. Manejo de propiedades din�micas (MetaData) ---
      else
      begin
        // Utilizamos el nuevo m�todo Evaluate del MetaData.
        // Esto permite que si buscas precio = 100, no falle si el 100 es un Integer
        // pero t� pasaste un Double, gracias a la gesti�n de Variants.
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
  // Almacena la distancia m�s corta desde el nodo de inicio a cualquier otro nodo
  Distances: TDictionary<TAiRagGraphNode, Integer>;
  // Almacena para cada nodo, una lista de sus "padres" en los caminos m�s cortos
  Parents: TDictionary<TAiRagGraphNode, TList<TAiRagGraphNode>>;
  AllPaths: TList<TArray<TObject>>;
  CurrentPath: TList<TObject>;
  CurrentNode, NeighborNode: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  ShortestDistance: Integer;

  // Procedimiento recursivo para reconstruir los caminos hacia atr�s
  procedure BuildPaths(ANode: TAiRagGraphNode);
  var
    ParentNode: TAiRagGraphNode;
    ConnectingEdge: TAiRagGraphEdge;
  begin
    // 1. A�adir el nodo actual al frente del camino que estamos construyendo
    CurrentPath.Insert(0, ANode);

    // 2. Caso Base: Si hemos llegado al nodo de inicio, hemos completado un camino
    if ANode = AStartNode then
    begin
      // A�adimos una copia del camino encontrado a nuestra lista de resultados
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
          // Una versi�n m�s robusta podr�a iterar sobre ParentNode.OutgoingEdges
          if ConnectingEdge = nil then
          begin
            // B�squeda m�s exhaustiva si FindEdge falla (por si hay varias aristas)
            for var E in ParentNode.OutgoingEdges do
            begin
              if E.ToNode = ANode then
              begin
                ConnectingEdge := E;
                Break;
              end;
            end;
          end;

          // A�adir la arista al camino
          CurrentPath.Insert(0, ConnectingEdge);
          // Llamar recursivamente para el padre
          BuildPaths(ParentNode);
          // Backtrack: quitar la arista para la siguiente iteraci�n del bucle de padres
          CurrentPath.Remove(ConnectingEdge);
        end;
      end;
    end;

    // 4. Backtrack: quitar el nodo actual del camino para no afectar a otras ramas de la recursi�n
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
      // que est�n a la misma distancia. No exploramos m�s all�.
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

          // Si este vecino es nuestro destino, registramos la distancia m�s corta
          if NeighborNode = AEndNode then
            ShortestDistance := CurrentDist + 1;
        end
        // Caso 2: Ya hemos visitado este vecino, pero hemos encontrado otro camino
        // de la misma longitud m�nima para llegar a �l.
        else if Distances[NeighborNode] = CurrentDist + 1 then
        begin
          Parents[NeighborNode].Add(CurrentNode);
        end;
      end;
    end;

    // --- FASE 2: RECONSTRUCCI�N RECURSIVA HACIA ATR�S ---
    // Si ShortestDistance sigue en -1, significa que el nodo final nunca fue alcanzado.
    if ShortestDistance <> -1 then
    begin
      AllPaths := TList < TArray < TObject >>.Create;
      CurrentPath := TList<TObject>.Create;
      try
        BuildPaths(AEndNode); // Iniciar la recursi�n desde el nodo final
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
    // --- Etapa 1: Inicializaci�n del BFS ---
    // El nodo de inicio est� a una distancia de 0 de s� mismo.
    Distances.Add(ANode, 0);
    Queue.Enqueue(ANode);

    // --- Etapa 2: Recorrido BFS ---
    while Queue.Count > 0 do
    begin
      CurrentNode := Queue.Dequeue;
      CurrentDistance := Distances[CurrentNode];

      // Expandir a los vecinos (solo hacia adelante, como es t�pico en centralidad)
      // Si quisieras un grafo no dirigido, tambi�n recorrer�as las IncomingEdges.
      for Edge in CurrentNode.OutgoingEdges do
      begin
        NeighborNode := Edge.ToNode;

        // Si no hemos visitado este vecino antes...
        if not Distances.ContainsKey(NeighborNode) then
        begin
          // Marcamos su distancia
          Distances.Add(NeighborNode, CurrentDistance + 1);
          // Lo a�adimos a la cola para visitar a sus vecinos m�s tarde
          Queue.Enqueue(NeighborNode);

          // --- Etapa 3: Acumular los resultados ---
          Inc(ReachableNodesCount);
          TotalDistance := TotalDistance + (CurrentDistance + 1);
        end;
      end;
    end;

    // --- Etapa 4: Calcular la Centralidad de Cercan�a ---
    if TotalDistance > 0 then
    begin
      // F�rmula est�ndar de Closeness Centrality:
      // (N�mero de nodos alcanzables) / (Suma de las distancias a ellos)
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
  // Usamos un diccionario como un Set para b�squedas r�pidas de nodos
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
  // 1. Poblar el NodeSet para b�squedas eficientes
  for Node in ASubgraphNodes do
  begin
  NodeSet.Add(Node, True);
  end;

  // 2. Recopilar todas las aristas internas del subgrafo
  for Node in ASubgraphNodes do
  begin
  for Edge in Node.OutgoingEdges do
  begin
  // >> L�NEA CORREGIDA Y OPTIMIZADA <<
  // Ahora usamos ContainsKey en el diccionario, que es mucho m�s r�pido.
  if NodeSet.ContainsKey(Edge.ToNode) and (not RelevantEdges.ContainsKey(Edge.ID)) then
  begin
  RelevantEdges.Add(Edge.ID, Edge);
  end;
  end;
  end;

  // 3. Construir el texto a partir de las aristas recopiladas
  ContextBuilder.AppendLine('Context extracted from the knowledge graph:');
  ContextBuilder.AppendLine('The following relevant facts were found:');

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
    Exit('No se encontr� informaci�n relevante.');

  ContextBuilder := TStringBuilder.Create;
  NodeSet := TDictionary<TAiRagGraphNode, Boolean>.Create;
  RelevantEdges := TDictionary<string, TAiRagGraphEdge>.Create;
  try
    // 1. Identificar nodos presentes en el subgrafo
    for Node in ASubgraphNodes do
      NodeSet.Add(Node, True);

    // === SECCI�N 1: ENTIDADES Y SUS FRAGMENTOS (CHUNKS) ===
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

    // === SECCI�N 2: RELACIONES Y HECHOS ===
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
        // Evitar duplicados si ADirection es gdBoth y hay aristas rec�procas
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

    // Ordenar la lista usando TList.Sort con un comparador an�nimo
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
  Result := []; // Por defecto, devuelve un array vac�o si no hay camino
  if (AStartNode = nil) or (AEndNode = nil) or (AStartNode = AEndNode) then
    Exit;

  Queue := TQueue<TAiRagGraphNode>.Create;
  CameFrom := TDictionary<TAiRagGraphNode, TPathLink>.Create;
  PathList := TList<TObject>.Create;
  try
    // 1. Inicializaci�n
    Queue.Enqueue(AStartNode);
    CameFrom.Add(AStartNode, TPathLink.Create(nil, nil)); // El nodo de inicio no viene de ninguna parte

    // 2. Bucle BFS
    while Queue.Count > 0 do
    begin
      CurrentNode := Queue.Dequeue;

      if CurrentNode = AEndNode then
        Break; // �Camino encontrado!

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

    // 3. Reconstrucci�n del Camino (si se encontr�)
    if CameFrom.ContainsKey(AEndNode) then
    begin
      CurrentNode := AEndNode;
      while CurrentNode <> nil do
      begin
        PathList.Add(CurrentNode);
        if CameFrom.TryGetValue(CurrentNode, PathLink) and (PathLink.Key <> nil) then
        begin
          if PathLink.Value <> nil then
            PathList.Add(PathLink.Value); // A�adir la arista
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

  // 1. Intentar delegar la operaci�n al Driver (BD)
  if Assigned(FDriver) then
  begin
    // El driver devuelve el array de etiquetas de la BD
    Result := FDriver.GetUniqueEdgeLabels;

    // Si la BD devolvi� resultados, salimos.
    if Length(Result) > 0 then
      Exit;
  end;

  // 2. Fallback a la l�gica por defecto en memoria.
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

  // 1. Intentar delegar la operaci�n al Driver (BD)
  if Assigned(FDriver) then
  begin
    // El driver devuelve el array de etiquetas de la BD
    Result := FDriver.GetUniqueNodeLabels;

    // Si la BD devolvi� resultados, salimos.
    if Length(Result) > 0 then
      Exit;
  end;

  // 2. Fallback a la l�gica por defecto en memoria.
  Result := FNodeLabelIndex.Keys.ToArray;
end;

function TAiRagGraph.GraphToContextText(const ANodes: TArray<TAiRagGraphNode>): string;
var
  SB: TStringBuilder;
  NodeSet: TDictionary<string, TAiRagGraphNode>; // Usamos ID para b�squeda r�pida
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
    // 1. Indexar los nodos encontrados para b�squeda r�pida O(1)
    // Esto nos sirve para filtrar solo las aristas que conectan nodos de este conjunto.
    for Node in ANodes do
    begin
      if not NodeSet.ContainsKey(Node.ID) then
        NodeSet.Add(Node.ID, Node);
    end;

    // =========================================================================
    // SECCI�N 1: ENTIDADES Y CONTENIDO (Conocimiento Sem�ntico)
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
          // Identamos para mostrar jerarqu�a
          SB.AppendFormat('  + Detail: %s', [Chunk.Text]).AppendLine;
        end;
      end;

      // Separador visual entre nodos
      SB.AppendLine;
    end;

    // =========================================================================
    // SECCI�N 2: RELACIONES / HECHOS (Conocimiento Estructural)
    // =========================================================================
    // Aqu� ocurre la magia del grafo: reconstruimos la historia conectando los puntos.

    // Recolectar aristas INTERNAS (donde Origen Y Destino est�n en nuestro resultado)
    for Node in ANodes do
    begin
      for Edge in Node.OutgoingEdges do
      begin
        // �El nodo destino tambi�n fue encontrado en la b�squeda?
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
        // Formato GQL (ISO/IEC 39075) simplificado para el LLM: (A)-[REL]->(B)
        SB.AppendFormat('(%s)-[%s]->(%s)', [Edge.FromNode.Name, Edge.EdgeLabel.ToUpper, Edge.ToNode.Name]);

        // Incluir detalles de la relaci�n (ej: since: 2020, weight: 0.9)
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
    raise Exception.Create(AiStr('rag.graph.invalid_edge'));

  // 1. VERIFICACI�N DE IDENTIDAD (Identity Map)
  if FEdgeRegistry.TryGetValue(AEdge.ID, Result) then
  begin
    Exit; // La arista ya existe, devolvemos la instancia en memoria.
  end;

  // 2. L�GICA DE A�ADIR A ESTRUCTURAS EN MEMORIA
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
      // L�gica de reversi�n simplificada si falla la persistencia.
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
    raise Exception.Create(AiStr('rag.graph.invalid_node'));

  // 1. VERIFICACI�N DE IDENTIDAD (Identity Map)
  // Comprobamos si un nodo con este ID ya est� registrado en memoria.
  if FNodeRegistry.TryGetValue(ANode.ID, Result) then
  begin
    // El nodo ya existe. Devolvemos el nodo existente para mantener la
    // consistencia. El llamador es responsable de liberar el ANode duplicado.
    Exit;
  end;

  // 2. L�GICA DE A�ADIR A ESTRUCTURAS EN MEMORIA
  // Si no existe, procedemos a a�adirlo a todas nuestras estructuras.
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
  // El nodo (ANode) ahora es propiedad del grafo y est� registrado.
  Result := ANode;
end;

function TAiRagGraph.InternalHydrateEdge(const AEdgeData: TEdgeDataRecord): TAiRagGraphEdge;
var
  NewEdge: TAiRagGraphEdge;
  FromNode, ToNode: TAiRagGraphNode;
  Dim: Integer;
  JObj: TJSONObject;
begin
  // 1. Verificaci�n de Identidad (Cache en memoria)
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
  // 1. Verificaci�n de Identidad (Cache en memoria)
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
        if (JObj <> nil) and (JObj is TJSONObject) then // Validaci�n de tipo
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

  // 1. Delegaci�n al Driver (Base de Datos Externa)
  if Assigned(FDriver) then
  begin
    // El driver ahora recibe el tipo correcto
    Result := FDriver.SearchNodes(APrompt, ADepth, ALimit, APrecision, AFilter);
    Exit;
  end;

  // 2. L�gica en memoria

  // --- Validaci�n del Motor de Embeddings ---
  if not Assigned(FEmbeddings) then
  begin
    if Assigned(FNodes) and Assigned(FNodes.Embeddings) then
      FEmbeddings := FNodes.Embeddings;

    // Si la b�squeda es h�brida o vectorial, necesitamos embeddings
    if (not Assigned(FEmbeddings)) and (FNodes.SearchOptions.UseEmbeddings) then
      raise Exception.Create(AiStr('rag.graph.no_embeddings'));
  end;

  if FNodes.Count = 0 then
    Exit;

  // Sincronizamos
  FNodes.Embeddings := FEmbeddings;

  // 3. B�squeda en el Vector
  // AQUI EST� LA MEJORA: Ya no necesitamos casting, pasamos AFilter directamente
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

      // 5. Expansi�n Contextual (BFS)
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
begin
  // Llamamos a Search pasando el AFilter actualizado
  FoundNodes := Self.Search(APrompt, ADepth, ALimit, APrecision, AFilter);

  if Length(FoundNodes) = 0 then
    Exit('No se encontró información relevante para: ' + APrompt);

  // El contexto son los nodos encontrados (y sus vecinos hasta ADepth), sus fragmentos y las relaciones entre
  // ellos. Antes esta funcion solo devolvia el encabezado: quien la usaba le pasaba al LLM un contexto vacio.
  Result := 'Context extracted from the knowledge graph:' + sLineBreak + GetContextualizedText(FoundNodes);
end;

procedure TAiRagGraph.LoadFromStream(AStream: TStream);
var
  SR: TStreamReader;
  JsonValue: TJSONValue;
  Root, GraphObj, NodeObj, EdgeObj, PropObj, ChunkObj: TJSONObject;
  NodesArr, EdgesArr, EmbeddingArr, ChunksArr: TJSONArray;
  NodeVal, EdgeVal, ChunkVal: TJSONValue;
  J, K: Integer;

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
        raise Exception.Create(AiStr('rag.graph.invalid_json'));

      Root := JsonValue as TJSONObject;
      GraphObj := Root.GetValue<TJSONObject>('graph');
      if GraphObj = nil then
        Exit;

      // --- PASO CR�TICO: Bloquear actualizaciones de �ndices ---
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
            // Esto restaura tipos reales: Fechas, Booleanos y N�meros.
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
        // --- 4. RECONSTRUCCI�N DE �NDICES ---
        // EndUpdate invoca internamente a RebuildIndexes, activando BM25 y HNSW
        // sobre todos los datos reci�n cargados.
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
  InitialState: TMatchState;

  // --- Variables para la Expansión (ADepth > 0) ---
  SeedNodesSet: TDictionary<TAiRagGraphNode, Boolean>;
  SeedList: TList<TAiRagGraphNode>;
  MatchDict: TDictionary<string, TObject>;
  Pair: TPair<string, TObject>;
  ExpandedNodesArray: TArray<TAiRagGraphNode>;
  ExpandedResults: TObjectList<TDictionary<string, TObject>>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Dict: TDictionary<string, TObject>;
  EdgeSet: TDictionary<TAiRagGraphEdge, Boolean>;

  function CandidatesFor(APattern: TMatchNodePattern): TArray<TAiRagGraphNode>;
  begin
    if not APattern.NodeLabel.IsEmpty then
      Result := Self.FindNodesByLabel(APattern.NodeLabel)
    else
      Result := FNodeRegistry.Values.ToArray;
  end;

  function PatternOf(const AVar: string): TMatchNodePattern;
  begin
    Result := AQuery.NodePatternByVariable[AVar];
    if Result = nil then
      raise Exception.Create(AiStr('rag.graph.no_start_pattern'));
  end;

  procedure FindMatchesRecursive(AClauseIndex: Integer; ACurrentState: TMatchState);
  var
    Clause: TMatchClause;
    Pattern: TMatchNodePattern;
    SourceObj, TargetObj: TObject;
    FromNode: TAiRagGraphNode;
    CurrentEdge: TAiRagGraphEdge;
    Dir: TGraphDirection;

    // Enlaza AVar con cada candidato de su patrón y sigue en la misma cláusula
    procedure BindCandidates(const AVar: string; AClauseIndex: Integer; ACurrentState: TMatchState);
    var
      Pattern: TMatchNodePattern;
      Cand: TAiRagGraphNode;
      NewState: TMatchState;
    begin
      Pattern := PatternOf(AVar);
      for Cand in CandidatesFor(Pattern) do
        if Pattern.Matches(Cand) then
        begin
          NewState := TMatchState.Create(ACurrentState);
          try
            NewState.AddOrSetValue(AVar, Cand);
            FindMatchesRecursive(AClauseIndex, NewState);
          finally
            NewState.Free;
          end;
        end;
    end;

    // Prueba una arista de la cláusula. APerspective es la dirección vista desde el ORIGEN
    // de la cláusula (lo que el patrón -[]->, <-[]- compara); ANeighbor es el extremo por enlazar.
    procedure TryEdge(AClause: TMatchClause; AEdge: TAiRagGraphEdge; ANeighbor: TAiRagGraphNode; APerspective: TGraphDirection;
      const ANeighborVar: string; AClauseIndex: Integer; ACurrentState: TMatchState);
    var
      BoundObject: TObject;
      NewState: TMatchState;
    begin
      if not(AClause.EdgePattern.Matches(AEdge, APerspective) and PatternOf(ANeighborVar).Matches(ANeighbor)) then
        Exit;
      if ACurrentState.TryGetValue(ANeighborVar, BoundObject) and (BoundObject <> ANeighbor) then
        Exit;
      if (not AClause.EdgePattern.Variable.IsEmpty) and ACurrentState.TryGetValue(AClause.EdgePattern.Variable, BoundObject) and
        (BoundObject <> AEdge) then
        Exit;

      NewState := TMatchState.Create(ACurrentState);
      try
        NewState.AddOrSetValue(ANeighborVar, ANeighbor);
        if not AClause.EdgePattern.Variable.IsEmpty then
          NewState.AddOrSetValue(AClause.EdgePattern.Variable, AEdge);
        FindMatchesRecursive(AClauseIndex + 1, NewState);
      finally
        NewState.Free;
      end;
    end;

  begin
    if AClauseIndex >= AQuery.MatchClauses.Count then
    begin
      // Patrones de nodo sin enlazar (MATCH (a:X), (b:Y) o un nodo suelto): producto cartesiano
      for Pattern in AQuery.NodePatterns do
        if not ACurrentState.ContainsKey(Pattern.Variable) then
        begin
          BindCandidates(Pattern.Variable, AClauseIndex, ACurrentState);
          Exit;
        end;

      // --- AQUÍ ENTRA EL WHERE ---
      if (AQuery.WhereClause = nil) or GQLToBool(EvaluateGraphExpression(AQuery.WhereClause, ACurrentState)) then
        Results.Add(TMatchState.Create(ACurrentState));
      Exit;
    end;

    Clause := AQuery.MatchClauses[AClauseIndex];
    Dir := Clause.EdgePattern.Direction;

    if ACurrentState.TryGetValue(Clause.SourceNodeVar, SourceObj) then
    begin
      // Origen enlazado: se recorre hacia el destino
      FromNode := SourceObj as TAiRagGraphNode;
      if Dir in [gdOutgoing, gdBoth] then
        for CurrentEdge in FromNode.OutgoingEdges do
          TryEdge(Clause, CurrentEdge, CurrentEdge.ToNode, gdOutgoing, Clause.TargetNodeVar, AClauseIndex, ACurrentState);
      if Dir in [gdIncoming, gdBoth] then
        for CurrentEdge in FromNode.IncomingEdges do
          TryEdge(Clause, CurrentEdge, CurrentEdge.FromNode, gdIncoming, Clause.TargetNodeVar, AClauseIndex, ACurrentState);
    end
    else if ACurrentState.TryGetValue(Clause.TargetNodeVar, TargetObj) then
    begin
      // Solo el destino enlazado (patrones convergentes: (c)-[]->(a), (p)-[]->(a)):
      // se recorre al revés. Una arista origen->destino es entrante para el destino.
      FromNode := TargetObj as TAiRagGraphNode;
      if Dir in [gdOutgoing, gdBoth] then
        for CurrentEdge in FromNode.IncomingEdges do
          TryEdge(Clause, CurrentEdge, CurrentEdge.FromNode, gdOutgoing, Clause.SourceNodeVar, AClauseIndex, ACurrentState);
      if Dir in [gdIncoming, gdBoth] then
        for CurrentEdge in FromNode.OutgoingEdges do
          TryEdge(Clause, CurrentEdge, CurrentEdge.ToNode, gdIncoming, Clause.SourceNodeVar, AClauseIndex, ACurrentState);
    end
    else
      // Ninguno enlazado: el origen de esta cláusula es un ancla nueva
      BindCandidates(Clause.SourceNodeVar, AClauseIndex, ACurrentState);
  end;

begin
  Result := [];
  if (AQuery = nil) or (AQuery.NodePatterns.Count = 0) then
    Exit;

  Results := TObjectList < TDictionary < string, TObject >>.Create(True);
  try
    // El primer ancla es el origen de la primera cláusula (o el primer nodo si no hay aristas)
    InitialState := TMatchState.Create;
    try
      FindMatchesRecursive(0, InitialState);
    finally
      InitialState.Free;
    end;

    // --- PASO COMÚN: EXPANSIÓN DEL SUBGRAFO (si ADepth > 0 y hay resultados) ---
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
          SeedList := TList<TAiRagGraphNode>.Create(SeedNodesSet.Keys);
          try
            ExpandedNodesArray := Self.ExpandNodeList(SeedList, ADepth);
          finally
            SeedList.Free;
          end;

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

  // Bloqueamos actualizaciones para reconstruir �ndices solo al final
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

    // --- 3. TRASLADAR CHUNKS (Opci�n 1) ---
    // Movemos los fragmentos de texto detallados para no perder contexto RAG
    while ASubsumedNode.Chunks.Count > 0 do
    begin
      Chunk := ASubsumedNode.Chunks[0];
      // Quitamos la propiedad del objeto del nodo viejo sin liberarlo
      ASubsumedNode.Chunks.Extract(Chunk);
      // Lo a�adimos al nuevo nodo
      ASurvivingNode.Chunks.Add(Chunk);
    end;

    // --- 4. FUSIONAR PROPIEDADES (MetaData) ---
    // Usamos InternalDictionary para iterar sobre las propiedades del MetaData
    for Pair in ASubsumedNode.Properties.InternalDictionary do
    begin
      case APropertyMergeStrategy of
        msAddNewOnly:
          begin
            // Usamos el m�todo .Has() del nuevo MetaData
            if not ASurvivingNode.Properties.Has(Pair.Key) then
              ASurvivingNode.Properties[Pair.Key] := Pair.Value;
          end;

        msOverwrite:
          begin
            // La asignaci�n directa indexada realiza el "AddOrSetValue" autom�ticamente
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
  // --- Variables para la implementaci�n en memoria ---
  IntermediateResults: TDictionary<string, TList<TAiRagGraphNode>>;
  InitialNodes: TArray<TAiRagGraphNode>;
  Step: TQueryStep;
  SourceNodes, TargetNodes, ResultSet: TList<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  UniqueTargetNodes: TDictionary<TAiRagGraphNode, Boolean>;
  AnchorList: TList<TAiRagGraphNode>;
begin
  // Inicializar el resultado como un array vac�o.
  SetLength(Result, 0);

  // Paso 1: Intentar delegar la ejecuci�n del plan de consulta completo al Driver.
  if Assigned(FDriver) then
  begin
    // El driver realiza la b�squeda h�brida en la BD y devuelve el array de nodos.
    Result := FDriver.Query(APlan, ADepth, ALimit, APrecision);

    // Si la BD encontr� resultados (o si el driver se ejecut�), salimos.
    if Length(Result) > 0 then
      Exit;
  end;

  // Paso 2: Si la consulta no fue manejada por un delegado o no encontr� resultados,
  // ejecutar la l�gica en memoria.

  // --- L�gica original y optimizada de la consulta en memoria ---
  IntermediateResults := TDictionary < string, TList < TAiRagGraphNode >>.Create;
  try
    // --- Etapa 1: B�squeda Sem�ntica de Anclaje (en memoria) ---
    // Se utiliza el 'Search' del propio componente.
    InitialNodes := Self.Search(APlan.AnchorPrompt, ADepth, ALimit, APrecision);
    if Length(InitialNodes) = 0 then
      Exit; // Sale con un array 'Result' vac�o si no hay anclas.

    AnchorList := TList<TAiRagGraphNode>.Create;
    AnchorList.AddRange(InitialNodes);
    IntermediateResults.Add(APlan.AnchorVariable, AnchorList);

    // --- Etapa 2: Ejecuci�n de Pasos Estructurales (en memoria) ---
    for Step in APlan.Steps do
    begin
      if not IntermediateResults.TryGetValue(Step.SourceVariable, SourceNodes) then
        Continue; // No se puede continuar si el paso anterior no arroj� resultados

      if not IntermediateResults.TryGetValue(Step.TargetVariable, TargetNodes) then
      begin
        TargetNodes := TList<TAiRagGraphNode>.Create;
        IntermediateResults.Add(Step.TargetVariable, TargetNodes);
      end;

      UniqueTargetNodes := TDictionary<TAiRagGraphNode, Boolean>.Create;
      try
        // Esto es una optimizaci�n para manejar resultados acumulativos
        for Node in TargetNodes do
          UniqueTargetNodes.Add(Node, True);

        for Node in SourceNodes do
        begin
          if Step.IsReversed then // B�squeda hacia atr�s
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
          else // B�squeda hacia adelante
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
  // 1. Limpiar completamente los �ndices secundarios existentes
  for NodeList in FNodeLabelIndex.Values do
    NodeList.Free;
  FNodeLabelIndex.Clear;
  FNodeNameIndex.Clear;

  // 2. Reconstruir los �ndices secundarios desde cero iterando todos los nodos
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

  // 3. Reconstruir los �ndices vectoriales (la operaci�n m�s costosa)

  If Assigned(FEmbeddings) then // Si tiene asignado un embeddings recrea los indices, aunque aqu� no genera embeddings
  Begin
    FNodes.BuildIndex;
    FEdges.BuildIndex;
  End;
end;

// En uMakerAi.RAG.Graph.Core.pas, secci�n implementation

procedure WriteVariant(const AWriter: TJsonWriter; const AValue: Variant);
var
  V: Variant;
  VType: TVarType;
begin
  V := AValue; // <- RESUELVE varByRef autom�ticamente
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
  FileExt := ExtractFileExt(AFileName).ToLower; // Obtenemos la extensi�n en min�sculas

  if FileExt = '.graphml' then
    SaveToGraphML(AFileName)
  else if FileExt = '.dot' then
    SaveToDot(AFileName)
  else if (FileExt = '.mkai') or (FileExt = '.json') then // Permitimos ambas extensiones
    SaveToMakerAi(AFileName, aFull)
  else
    // Si la extensi�n no es reconocida, asumimos el formato nativo por defecto (.mkai)
    // o puedes lanzar una excepci�n si prefieres ser m�s estricto.
    // Aqu� optamos por guardar en el formato nativo, a�adiendo la extensi�n si no la tiene.
    SaveToMakerAi(ChangeFileExt(AFileName, '.mkai'), aFull);
  // Alternativa estricta:
  // raise Exception.CreateFmt('Formato de archivo no soportado para la extensi�n "%s".', [FileExt]);
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

    // --- ELEMENTO RA�Z ---
    GraphMLNode := XMLDoc.AddChild('graphml');
    GraphMLNode.Attributes['xmlns'] := 'http://graphml.graphdrawing.org/xmlns';
    GraphMLNode.Attributes['xmlns:xsi'] := 'http://www.w3.org/2001/XMLSchema-instance';
    GraphMLNode.Attributes['xsi:schemaLocation'] := 'http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd';

    // --- 1. DEFINICI�N DE ATRIBUTOS (KEYS) ---
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

    // Recolectar llaves de propiedades din�micas (MetaData) de todos los elementos
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

      // Metadatos din�micos
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

      // Metadatos din�micos
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
    // Llama a la nueva versi�n sobrecargada de SaveToStream
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
    raise Exception.Create(AiStr('rag.target_stream_nil'));

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

        // Propiedades din�micas (MetaData)
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
      // Le decimos al Driver: "Av�same si te mueres"
      FDriver.FreeNotification(Self);

      // L�gica existente de asignaci�n
      FDriver.AssignToGraph(Self);
    end;
  end;
end;

procedure TAiRagGraph.SetEmbeddings(const Value: TAiEmbeddingsCore);
begin
  if FEmbeddings <> Value then
  begin
    FEmbeddings := Value;

    // Le decimos al componente de Embeddings: "Av�same (a m�, el Grafo) si te mueres"
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
  // Ignorar si el patr�n no especifica una etiqueta
  if (not NodeLabel.IsEmpty) and (not SameText(ANode.NodeLabel, NodeLabel)) then
    Exit;

  // 2. Comprobar todas las propiedades requeridas del patr�n
  // 'Properties' aqu� es el diccionario de filtros del PATR�N (ej: {ciudad: 'Madrid'})
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
    // --- B. Metadatos Din�micos ---
    else
    begin
      // Utilizamos el nuevo motor de evaluaci�n del MetaData
      // foEqual realiza una comparaci�n inteligente de Variants (tipada)
      if not ANode.Properties.Evaluate(Pair.Key, foEqual, Pair.Value) then
        Exit;
    end;
  end;

  // Si super� todos los filtros, es un match
  Result := True;
end;

{ TMatchEdgePattern }

constructor TMatchEdgePattern.Create;
begin
  inherited;
  Direction := gdOutgoing; // Direcci�n por defecto
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

  // 1. Comprobar la direcci�n del recorrido
  // Si el patr�n especifica una direcci�n (saliente/entrante), debe coincidir con el recorrido actual
  if (Direction <> gdBoth) and (Direction <> AActualDirection) then
    Exit;

  // 2. Comprobar la etiqueta de la arista (EdgeLabel)
  if (not EdgeLabel.IsEmpty) and (not SameText(AEdge.EdgeLabel, EdgeLabel)) then
    Exit;

  // 3. Comprobar todas las propiedades requeridas del patr�n de b�squeda
  // 'Properties' es el diccionario de filtros del patr�n (ej: {since: 2020})
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
      // Comparaci�n num�rica segura para el peso (Weight)
      try
        if Abs(AEdge.Weight - Double(Pair.Value)) > 0.0001 then
          Exit;
      except
        Exit;
      end;
    end
    // --- B. Metadatos Din�micos (MetaData unificado) ---
    else
    begin
      // Utilizamos el nuevo motor de evaluaci�n del MetaData de la arista.
      // foEqual realiza una comparaci�n inteligente de Variants (tipada).
      if not AEdge.MetaData.Evaluate(Pair.Key, foEqual, Pair.Value) then
        Exit;
    end;
  end;

  // Si super� todos los filtros (direcci�n, etiqueta y propiedades), es un match
  Result := True;
end;

{ TMatchClause }

constructor TMatchClause.Create(ASourceNodeVar: string; AEdgePattern: TMatchEdgePattern; ATargetNodeVar: string);
begin
  inherited Create;
  SourceNodeVar := ASourceNodeVar;
  EdgePattern := AEdgePattern; // La cl�usula toma posesi�n del patr�n de arista
  TargetNodeVar := ATargetNodeVar;
end;

destructor TMatchClause.Destroy;
begin
  EdgePattern.Free; // Libera el patr�n de arista del que es due�o
  inherited;
end;

{ TGraphMatchQuery }

constructor TGraphMatchQuery.Create;
begin
  inherited;
  FNodePatterns := TObjectList<TMatchNodePattern>.Create(True); // Es due�o
  FMatchClauses := TObjectList<TMatchClause>.Create(True); // Es due�o
  FProjections := TObjectList<TGQLProjection>.Create(True);
  FDepth := 0;
end;

destructor TGraphMatchQuery.Destroy;
begin
  if Assigned(FWhereClause) then
    FWhereClause.Free; // Esto disparar� la liberaci�n en cadena de todo el �rbol

  FProjections.Free;
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
    if Pattern.Variable = AVar then
    begin
      Result := Pattern;
      Exit;
    end;
  end;
  // Si no se encuentra, podr�a ser un error en la construcci�n de la consulta.
  // Podr�as lanzar una excepci�n aqu� si quieres ser m�s estricto.
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
  Result := '{}'; // Valor por defecto para diccionario vac�o

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
  Left.Free; // La liberaci�n es recursiva
  Right.Free;
  inherited;
end;

{ TUnaryExpr }
constructor TUnaryExpr.Create(AOp: TUnaryOp; AOperand: TGraphExpression);
begin
  inherited Create;
  Kind := ekUnary;
  Op := AOp;
  Operand := AOperand;
end;

destructor TUnaryExpr.Destroy;
begin
  Operand.Free;
  inherited;
end;

{ TVariableExpr }
constructor TVariableExpr.Create(const AVar: string);
begin
  inherited Create;
  Kind := ekVariable;
  Variable := AVar;
end;

{ TFunctionExpr }
constructor TFunctionExpr.Create(const AName: string);
begin
  inherited Create;
  Kind := ekFunction;
  Name := AName;
  Args := TObjectList<TGraphExpression>.Create(True);
end;

destructor TFunctionExpr.Destroy;
begin
  Args.Free;
  inherited;
end;

{ TAggregateExpr }
constructor TAggregateExpr.Create(AFunc: TAggregateFunc; AArg: TGraphExpression; ADistinct: Boolean);
begin
  inherited Create;
  Kind := ekAggregate;
  Func := AFunc;
  Arg := AArg;
  Distinct := ADistinct;
  Value := Null;
end;

destructor TAggregateExpr.Destroy;
begin
  Arg.Free;
  inherited;
end;

{ TGQLReturnItem }
destructor TGQLReturnItem.Destroy;
begin
  Expr.Free;
  inherited;
end;

{ TGQLOrderItem }
destructor TGQLOrderItem.Destroy;
begin
  Expr.Free;
  inherited;
end;

{ TGQLProjection }
constructor TGQLProjection.Create;
begin
  inherited;
  Items := TObjectList<TGQLReturnItem>.Create(True);
  OrderBy := TObjectList<TGQLOrderItem>.Create(True);
  Skip := -1;
  Limit := -1;
end;

destructor TGQLProjection.Destroy;
begin
  Where.Free;
  OrderBy.Free;
  Items.Free;
  inherited;
end;

{ TGQLValue }
constructor TGQLValue.CreateValue(const AData: Variant);
begin
  inherited Create(GQLToText(AData));
  Data := AData;
end;

{ TBetweenExpr }
constructor TBetweenExpr.Create(AValue, ALow, AHigh: TGraphExpression);
begin
  inherited Create;
  Kind := ekBetween;
  Value := AValue;
  Low := ALow;
  High := AHigh;
end;

destructor TBetweenExpr.Destroy;
begin
  Value.Free;
  Low.Free;
  High.Free;
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
