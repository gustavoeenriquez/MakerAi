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
  TExpressionKind = (ekLiteral, ekProperty, ekBinary, ekUnary, ekBetween, ekVariable, ekFunction, ekAggregate, ekPattern, ekSimilar);

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
    EmbedText: string; // similarity(n, 'texto'): embedding del texto ya calculado
    EmbedCache: TAiEmbeddingData;
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

  TGraphMatchQuery = class;

  // n SIMILAR TO 'texto' [> umbral | >= umbral]: similitud coseno entre el embedding del nodo
  // (o arista) y el del texto. Sin umbral: >= 0.5. El embedding del texto se calcula una vez.
  TSimilarExpr = class(TGraphExpression)
  public
    Target: TGraphExpression; // TVariableExpr
    Text: TGraphExpression;
    Threshold: TGraphExpression; // nil = 0.5
    StrictGreater: Boolean; // > en vez de >=
    CacheText: string;
    Cache: TAiEmbeddingData;
    constructor Create(ATarget, AText, AThreshold: TGraphExpression; AStrict: Boolean);
    destructor Destroy; override;
  end;

  // Patrón como condición: WHERE (p)-[:COMPRO]->(:Cafe), exists(...), EXISTS { MATCH ... WHERE ... }.
  // Vale True si el patrón tiene al menos una coincidencia con las variables ya enlazadas.
  TPatternExpr = class(TGraphExpression)
  public
    Query: TGraphMatchQuery; // Es dueña
    constructor Create(AQuery: TGraphMatchQuery);
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
    Optional: Boolean; // Lo introdujo un OPTIONAL MATCH (no participa en el MATCH principal)
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
    MinHops: Integer; // Longitud variable -[*1..3]->: por defecto 1..1 (un salto)
    MaxHops: Integer; // -1 = sin máximo (se acota a 10 saltos)
    constructor Create;
    destructor Destroy; override;
    function IsVariableLength: Boolean;
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

  { Escritura (solo con TAiRagGraph.AllowWrite = True) }
  TGQLWriteKind = (wkCreate, wkMerge, wkSet, wkRemove, wkDelete);

  { SET n.prop = expr, SET n:Etiqueta, REMOVE n.prop }
  TGQLSetItem = class
  public
    Variable: string;
    Key: string; // Propiedad ('' si se cambia la etiqueta)
    NewLabel: string; // SET n:Etiqueta
    Expr: TGraphExpression; // Es dueño; nil en REMOVE
    destructor Destroy; override;
  end;

  TGQLWriteClause = class
  public
    Kind: TGQLWriteKind;
    Pattern: TGraphMatchQuery; // CREATE / MERGE (es dueña): patrones a crear o buscar
    Items: TObjectList<TGQLSetItem>; // SET / REMOVE
    OnCreate: TObjectList<TGQLSetItem>; // MERGE ... ON CREATE SET
    OnMatch: TObjectList<TGQLSetItem>; // MERGE ... ON MATCH SET
    DeleteVars: TList<string>; // [DETACH] DELETE a, b
    Detach: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { Variable de camino: p = (a)-[...]->(b). Cubre las cláusulas FirstClause..LastClause del MATCH principal. }
  TGQLPathDef = record
    Variable: string;
    StartVar: string;
    FirstClause: Integer;
    LastClause: Integer; // -1 si el patrón es un solo nodo
  end;

  { OPTIONAL MATCH: sus cláusulas, los patrones de nodo nuevos que introdujo y su WHERE }
  TGQLOptionalMatch = class
  public
    Clauses: TObjectList<TMatchClause>; // Es dueña
    Patterns: TList<TMatchNodePattern>; // No es dueña (son de la consulta)
    Where: TGraphExpression; // Es dueña
    constructor Create;
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
    FOptionalParts: TObjectList<TGQLOptionalMatch>;
    FPaths: TList<TGQLPathDef>;
    FWrites: TObjectList<TGQLWriteClause>;
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
    property OptionalParts: TObjectList<TGQLOptionalMatch> read FOptionalParts;
    property Paths: TList<TGQLPathDef> read FPaths;
    // CREATE / MERGE / SET / REMOVE / DELETE, en orden
    property Writes: TObjectList<TGQLWriteClause> read FWrites;
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
    function Clone: TGQLValue; virtual;
  end;

  { Camino: p = (a)-[*1..3]->(b), o el tramo de una arista de longitud variable.
    Value/Data tienen el texto (Huila)-[:TIENE]->(Alta); Nodes y Edges, los elementos en orden. }
  TGQLPath = class(TGQLValue)
  public
    Nodes: TArray<TAiRagGraphNode>;
    Edges: TArray<TAiRagGraphEdge>;
    constructor CreatePath(const ANodes: TArray<TAiRagGraphNode>; const AEdges: TArray<TAiRagGraphEdge>);
    function Clone: TGQLValue; override;
    function NodeNames: Variant;
    function EdgeLabels: Variant;
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

    // Esquema calculado en la base, para SHOW SCHEMA / SHOW PROPERTIES de MakerGQL sin cargar el
    // grafo en memoria. Opcionales: la base devuelve False y el grafo carga todo para calcularlo.
    // GetSchemaCounts: etiqueta -> cantidad de nodos y '(:A)-[:TIPO]->(:B)' -> cantidad.
    // GetPropertyCounts: etiqueta -> (clave -> cantidad), aparte nodos y relaciones; ALabel vacio =
    // todas, si no solo las de esa etiqueta (sin distinguir mayusculas). Toda etiqueta de nodo que
    // pase el filtro aparece aunque sus nodos no tengan propiedades.
    function GetSchemaCounts(ALabelCounts, ATripleCounts: TDictionary<string, Integer>): Boolean; virtual;
    function GetPropertyCounts(const ALabel: string; ANodeProps, AEdgeProps: TObjectDictionary<string, TDictionary<string, Integer>>): Boolean; virtual;

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
    FAllowWrite: Boolean;
    FGQLFullyLoaded: Boolean; // Esta consulta de MakerGQL ya cargo todo el grafo desde el driver

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
    function GQLCandidates(APattern: TMatchNodePattern): TArray<TAiRagGraphNode>;
    procedure GQLEnsureFullGraph;
    function GQLSimilarity(AObj: TObject; const AText: string; var ACacheText: string; var ACache: TAiEmbeddingData): Variant;
    function GQLExplain(AQuery: TGraphMatchQuery; const ACommand: string): string;
    function GQLApplyWrites(AQuery: TGraphMatchQuery; const ARows: TArray<TDictionary<string, TObject>>): string;
    procedure GQLSolve(AQuery: TGraphMatchQuery; AClauses: TList<TMatchClause>; APatterns: TList<TMatchNodePattern>;
      AWhere: TGraphExpression; AState: TDictionary<string, TObject>; AResults: TObjectList<TDictionary<string, TObject>>;
      AFirstOnly: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RebuildIndexes;
    // Con driver: trae a memoria todos los nodos (por etiqueta) y sus relaciones. Lo usan las
    // operaciones de MakerGQL sobre todo el grafo (MATCH sin etiqueta, comunidades, centralidad,
    // grados). Cuesta del orden de dos consultas por nodo: para grafos grandes, usar etiquetas.
    procedure LoadFromDriver;

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
    // MakerGQL puede escribir (CREATE, MERGE, SET, REMOVE, DELETE). Apagado por defecto: el
    // lenguaje se le expone a modelos; encenderlo solo donde quien consulta puede cambiar el grafo.
    property AllowWrite: Boolean read FAllowWrite write FAllowWrite default False;
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

// Etiqueta contra un patrón que puede traer alternativas: 'Persona|Empresa'
function GQLLabelMatches(const AActual, APattern: string): Boolean;
var
  Part: string;
begin
  if APattern.IndexOf('|') < 0 then
    Exit(SameText(AActual, APattern));
  for Part in APattern.Split(['|']) do
    if SameText(AActual, Part) then
      Exit(True);
  Result := False;
end;

// Copia una fila de resultado: los nodos y aristas se comparten (son del grafo); los
// TGQLValue/TGQLPath se clonan, así cada fila es dueña de los suyos.
function GQLCloneRow(ARow: TDictionary<string, TObject>): TDictionary<string, TObject>;
var
  P: TPair<string, TObject>;
begin
  Result := TDictionary<string, TObject>.Create;
  for P in ARow do
    if P.Value is TGQLValue then
      Result.Add(P.Key, TGQLValue(P.Value).Clone)
    else
      Result.Add(P.Key, P.Value);
end;

procedure GQLFreeRowValues(ARow: TDictionary<string, TObject>);
var
  Obj: TObject;
begin
  for Obj in ARow.Values do
    if Obj is TStringWrapper then
      Obj.Free;
end;

procedure GQLBump(ADict: TDictionary<string, Integer>; const AKey: string);
var
  C: Integer;
begin
  if not ADict.TryGetValue(AKey, C) then
    C := 0;
  ADict.AddOrSetValue(AKey, C + 1);
end;

// ---------------------------------------------------------------------------
// MakerGQL: similitud semántica y EXPLAIN
// ---------------------------------------------------------------------------

// Coseno entre el embedding del nodo/arista y el del texto. El embedding del texto se calcula
// una vez por expresión (ACache). NULL si el elemento no tiene vector o los tamaños no casan.
function TAiRagGraph.GQLSimilarity(AObj: TObject; const AText: string; var ACacheText: string; var ACache: TAiEmbeddingData): Variant;
var
  V: TAiEmbeddingData;
  I: Integer;
  Dot, NA, NB: Double;
begin
  Result := Null;
  if not(AObj is TAiEmbeddingNode) then
    Exit;
  if FEmbeddings = nil then
    raise Exception.Create(AiStr('rag.gql.no_embeddings'));
  if (Length(ACache) = 0) or (ACacheText <> AText) then
  begin
    ACache := FEmbeddings.CreateEmbedding(AText, 'user');
    ACacheText := AText;
  end;
  V := TAiEmbeddingNode(AObj).Data;
  if (Length(V) = 0) or (Length(V) <> Length(ACache)) then
    Exit;
  Dot := 0;
  NA := 0;
  NB := 0;
  for I := 0 to High(V) do
  begin
    Dot := Dot + V[I] * ACache[I];
    NA := NA + V[I] * V[I];
    NB := NB + ACache[I] * ACache[I];
  end;
  if (NA = 0) or (NB = 0) then
    Exit; // Nodo sin embedding (vector en ceros)
  Result := Dot / (Sqrt(NA) * Sqrt(NB));
end;

function GQLNodePatternText(P: TMatchNodePattern): string;
var
  Pair: TPair<string, Variant>;
  Props: string;
begin
  Result := '(';
  if not P.Variable.StartsWith('#') then
    Result := Result + P.Variable;
  if P.NodeLabel <> '' then
    Result := Result + ':' + P.NodeLabel;
  Props := '';
  for Pair in P.Properties do
  begin
    if Props <> '' then
      Props := Props + ', ';
    Props := Props + Pair.Key + ': ' + GQLToText(Pair.Value);
  end;
  if Props <> '' then
    Result := Result + ' {' + Props + '}';
  Result := Result + ')';
end;

function GQLEdgePatternText(E: TMatchEdgePattern): string;
var
  Inner: string;
begin
  Inner := E.Variable;
  if E.EdgeLabel <> '' then
    Inner := Inner + ':' + E.EdgeLabel;
  if E.IsVariableLength then
  begin
    Inner := Inner + '*' + IntToStr(E.MinHops) + '..';
    if E.MaxHops >= 0 then
      Inner := Inner + IntToStr(E.MaxHops);
  end;
  case E.Direction of
    gdOutgoing:
      Result := '-[' + Inner + ']->';
    gdIncoming:
      Result := '<-[' + Inner + ']-';
  else
    Result := '-[' + Inner + ']-';
  end;
end;

// Describe cómo se va a ejecutar la consulta, sin ejecutarla
function TAiRagGraph.GQLExplain(AQuery: TGraphMatchQuery; const ACommand: string): string;
var
  SB: TStringBuilder;
  Bound: TList<string>;
  Step, I: Integer;
  Stage: TGQLProjection;
  Part: TGQLOptionalMatch;
  Def: TGQLPathDef;
  Names: string;
  P: TMatchNodePattern;

  function Plural(N: Integer; const ASingular, APlural: string): string;
  begin
    if N = 1 then
      Result := '1 ' + ASingular
    else
      Result := IntToStr(N) + ' ' + APlural;
  end;

  function VarName(const AVar: string): string;
  begin
    if AVar.StartsWith('#') then
      Result := 'an anonymous node'
    else
      Result := AVar;
  end;

  function Candidates(const AVar: string): string;
  var
    Pat: TMatchNodePattern;
  begin
    Pat := AQuery.NodePatternByVariable[AVar];
    if Pat = nil then
      Exit('?');
    if Pat.NodeLabel = '' then
      Result := Plural(Length(GQLCandidates(Pat)), 'node', 'nodes') + ' (no label: every node is scanned)'
    else
      Result := Plural(Length(GQLCandidates(Pat)), 'candidate', 'candidates') + ' by label';
  end;

  procedure ExplainClauses(AClauses: TList<TMatchClause>; APatterns: TList<TMatchNodePattern>; const AIndent: string);
  var
    C: TMatchClause;
    Hops: string;
    Pat: TMatchNodePattern;
  begin
    for C in AClauses do
    begin
      if not Bound.Contains(C.SourceNodeVar) and not Bound.Contains(C.TargetNodeVar) then
      begin
        Inc(Step);
        SB.AppendFormat('%s%d. Anchor %s: %s', [AIndent, Step, GQLNodePatternText(AQuery.NodePatternByVariable[C.SourceNodeVar]),
          Candidates(C.SourceNodeVar)]).AppendLine;
        Bound.Add(C.SourceNodeVar);
      end;
      if C.EdgePattern.IsVariableLength then
      begin
        if C.EdgePattern.MaxHops >= 0 then
          Hops := Format('%d..%d hops', [C.EdgePattern.MinHops, C.EdgePattern.MaxHops])
        else
          Hops := Format('%d..10 hops (no maximum given: capped at 10)', [C.EdgePattern.MinHops]);
      end
      else
        Hops := '1 hop';
      Inc(Step);
      if Bound.Contains(C.SourceNodeVar) then
        SB.AppendFormat('%s%d. Expand from %s: %s%s%s, %s', [AIndent, Step, VarName(C.SourceNodeVar),
          GQLNodePatternText(AQuery.NodePatternByVariable[C.SourceNodeVar]), GQLEdgePatternText(C.EdgePattern),
          GQLNodePatternText(AQuery.NodePatternByVariable[C.TargetNodeVar]), Hops]).AppendLine
      else
        SB.AppendFormat('%s%d. Expand backwards from %s (already bound): %s%s%s, %s', [AIndent, Step, VarName(C.TargetNodeVar),
          GQLNodePatternText(AQuery.NodePatternByVariable[C.SourceNodeVar]), GQLEdgePatternText(C.EdgePattern),
          GQLNodePatternText(AQuery.NodePatternByVariable[C.TargetNodeVar]), Hops]).AppendLine;
      if not Bound.Contains(C.SourceNodeVar) then
        Bound.Add(C.SourceNodeVar);
      if not Bound.Contains(C.TargetNodeVar) then
        Bound.Add(C.TargetNodeVar);
    end;
    for Pat in APatterns do
      if not Bound.Contains(Pat.Variable) then
      begin
        Inc(Step);
        SB.AppendFormat('%s%d. Combine every %s with each row (cartesian product): %s', [AIndent, Step, GQLNodePatternText(Pat),
          Candidates(Pat.Variable)]).AppendLine;
        Bound.Add(Pat.Variable);
      end;
  end;

  procedure ExplainStage(AStage: TGQLProjection; const ATitle: string);
  var
    It: TGQLReturnItem;
  begin
    Names := '';
    for It in AStage.Items do
    begin
      if Names <> '' then
        Names := Names + ', ';
      Names := Names + It.Name;
    end;
    SB.Append(ATitle).Append(': ');
    if AStage.Distinct then
      SB.Append('DISTINCT ');
    SB.Append(Names);
    if AStage.Where <> nil then
      SB.Append('; WHERE on the columns');
    if AStage.OrderBy.Count > 0 then
      SB.Append('; ORDER BY ').Append(AStage.OrderBy.Count).Append(' key(s)');
    if AStage.Skip >= 0 then
      SB.Append('; SKIP ').Append(AStage.Skip);
    if AStage.Limit >= 0 then
      SB.Append('; LIMIT ').Append(AStage.Limit);
    SB.AppendLine;
  end;

begin
  SB := TStringBuilder.Create;
  Bound := TList<string>.Create;
  try
    SB.AppendLine('### QUERY PLAN ###');
    if ACommand <> '' then
    begin
      SB.Append('Command ').Append(ACommand).AppendLine(': runs a graph algorithm or lists the schema; no pattern matching.');
      Exit(SB.ToString);
    end;

    if AQuery.Tabular then
      SB.Append('Result: a table with ').Append(Plural(AQuery.Projections.Last.Items.Count, 'column', 'columns')).AppendLine
    else
      SB.AppendLine('Result: the matched subgraph as context text');

    Step := 0;
    SB.AppendLine('MATCH:');
    var Main := TList<TMatchNodePattern>.Create;
    try
      for P in AQuery.NodePatterns do
        if not P.Optional then
          Main.Add(P);
      ExplainClauses(AQuery.MatchClauses, Main, '  ');
    finally
      Main.Free;
    end;
    for Def in AQuery.Paths do
      SB.AppendFormat('  Path variable %s', [Def.Variable]).AppendLine;
    if AQuery.WhereClause <> nil then
      SB.AppendLine('WHERE: filters each complete match');

    I := 0;
    for Part in AQuery.OptionalParts do
    begin
      Inc(I);
      SB.AppendFormat('OPTIONAL MATCH %d (rows without a match are kept, new variables NULL):', [I]).AppendLine;
      ExplainClauses(Part.Clauses, Part.Patterns, '  ');
      if Part.Where <> nil then
        SB.AppendLine('  WHERE: filters the optional part only');
    end;

    for var W in AQuery.Writes do
    begin
      case W.Kind of
        wkCreate: SB.Append('CREATE');
        wkMerge: SB.Append('MERGE (find the pattern, create it if missing)');
        wkSet: SB.Append('SET');
        wkRemove: SB.Append('REMOVE');
      else
        if W.Detach then
          SB.Append('DETACH DELETE')
        else
          SB.Append('DELETE');
      end;
      SB.AppendLine(': runs once per row; needs AllowWrite = True');
    end;

    if AQuery.Depth > 0 then
    begin
      if AQuery.Tabular then
        SB.AppendFormat('DEPTH %d: ignored (the result is a table)', [AQuery.Depth]).AppendLine
      else
        SB.AppendFormat('DEPTH %d: adds the neighbourhood of the matched nodes', [AQuery.Depth]).AppendLine;
    end;

    I := 0;
    for Stage in AQuery.Projections do
    begin
      Inc(I);
      if Stage.IsReturn then
        ExplainStage(Stage, 'RETURN')
      else
        ExplainStage(Stage, 'WITH ' + IntToStr(I));
    end;
    Result := SB.ToString;
  finally
    Bound.Free;
    SB.Free;
  end;
end;

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
      Result := TGQLValue(Obj).Clone
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

  // --- Caminos: length(p), nodes(p), relationships(p) ---
  if ((N = 'length') or (N = 'size') or (N = 'nodes') or (N = 'relationships')) and (AFunc.Args.Count = 1) and
    (AFunc.Args[0] is TVariableExpr) and ABoundElements.TryGetValue(TVariableExpr(AFunc.Args[0]).Variable, Obj) and
    (Obj is TGQLPath) then
  begin
    if (N = 'length') or (N = 'size') then
      Exit(Length(TGQLPath(Obj).Edges));
    if N = 'nodes' then
      Exit(TGQLPath(Obj).NodeNames);
    Exit(TGQLPath(Obj).EdgeLabels);
  end;

  // --- Funciones de grafo: trabajan sobre el elemento enlazado a la variable ---
  if (N = 'id') or (N = 'label') or (N = 'labels') or (N = 'type') or (N = 'degree') or (N = 'indegree') or
    (N = 'outdegree') or (N = 'keys') or (N = 'nodes') or (N = 'relationships') or (N = 'similarity') then
  begin
    Obj := nil;
    if (AFunc.Args.Count > 0) and (AFunc.Args[0] is TVariableExpr) then
      ABoundElements.TryGetValue(TVariableExpr(AFunc.Args[0]).Variable, Obj);

    // similarity(n, 'texto'): coseno entre embeddings (nodo o arista)
    if N = 'similarity' then
      Exit(GQLSimilarity(Obj, GQLToText(EvaluateGraphExpression(AFunc.Args[1], ABoundElements)), AFunc.EmbedText, AFunc.EmbedCache));

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
  begin
    if AFunc.Args[0].Kind = ekPattern then // exists((a)-[]->(b))
      Exit(A[0]);
    Exit(not GQLIsNull(A[0]));
  end;

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
  PatRes: TObjectList<TDictionary<string, TObject>>;
  PatRow: TDictionary<string, TObject>;
  Sim: TSimilarExpr;
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
    // SIMILAR TO: similitud de embeddings contra un umbral
    // ================================================================
    ekSimilar:
      begin
        Sim := TSimilarExpr(AExpr);
        Result := False;
        if ABoundElements.TryGetValue(TVariableExpr(Sim.Target).Variable, Obj) then
        begin
          Left := GQLSimilarity(Obj, GQLToText(EvaluateGraphExpression(Sim.Text, ABoundElements)), Sim.CacheText, Sim.Cache);
          if not GQLIsNull(Left) then
          begin
            DR := 0.5;
            if Assigned(Sim.Threshold) and not GQLTryNumber(EvaluateGraphExpression(Sim.Threshold, ABoundElements), DR) then
              Exit(False);
            DL := Left;
            if Sim.StrictGreater then
              Result := DL > DR
            else
              Result := DL >= DR;
          end;
        end;
      end;

    // ================================================================
    // PATRÓN como condición: ¿hay al menos una coincidencia?
    // ================================================================
    ekPattern:
      begin
        PatRes := TObjectList < TDictionary < string, TObject >>.Create(True);
        try
          GQLSolve(TPatternExpr(AExpr).Query, TPatternExpr(AExpr).Query.MatchClauses, TPatternExpr(AExpr).Query.NodePatterns,
            TPatternExpr(AExpr).Query.WhereClause, ABoundElements, PatRes, True);
          Result := PatRes.Count > 0;
        finally
          for PatRow in PatRes do
            GQLFreeRowValues(PatRow);
          PatRes.Free;
        end;
      end;

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
  ResDict.Add('value', TStringWrapper.Create(FormatFloat('0.####', Score, TFormatSettings.Invariant)));

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

  // Comandos de esquema y algoritmos
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Counts, Triples, KeyCounts: TDictionary<string, Integer>;
  PropMap, EdgePropMap: TObjectDictionary<string, TDictionary<string, Integer>>;
  Keys, PropKeys: TArray<string>;
  Key, PropKey, Owner, Line: string;
  CommMap: TDictionary<TAiRagGraphNode, Integer>;
  Groups: TObjectDictionary<Integer, TList<TAiRagGraphNode>>;
  GroupList: TList<TAiRagGraphNode>;
  GroupArr: TArray<TList<TAiRagGraphNode>>;
  J: Integer;
  SeedNodes: TList<TAiRagGraphNode>;
  Scored: TList<TPair<TAiRagGraphNode, Double>>;
  AllPaths: TArray<TArray<TObject>>;

  function CommandName(AType: TGraphCommandType): string;
  begin
    case AType of
      cmdShowLabels: Result := 'SHOW LABELS';
      cmdShowEdges: Result := 'SHOW EDGES';
      cmdShowSchema: Result := 'SHOW SCHEMA';
      cmdShowProperties: Result := 'SHOW PROPERTIES';
      cmdShortestPath: Result := 'SHORTEST PATH';
      cmdAllShortestPaths: Result := 'ALL SHORTEST PATHS';
      cmdCentrality: Result := 'GET CENTRALITY';
      cmdCentralityTop: Result := 'GET CENTRALITY TOP';
      cmdDegrees: Result := 'GET DEGREES TOP';
      cmdCommunities: Result := 'GET COMMUNITIES';
      cmdNeighbors: Result := 'GET NEIGHBORS';
    else
      Result := '';
    end;
  end;

  // --- Funci�n auxiliar FindNodeByPattern (se mantiene igual) ---
  function FindNodeByPattern(APattern: TMatchNodePattern): TAiRagGraphNode;
  var
    Candidates: TArray<TAiRagGraphNode>;
    Cand: TAiRagGraphNode;
  begin
    Result := nil;
    if APattern = nil then
      Exit;
    Candidates := Self.GQLCandidates(APattern);
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

  FGQLFullyLoaded := False;
  Parser := TGraphParser.Create(ACode, AParams);
  try
    QueryObj := Parser.Parse;
    try
      if Parser.Explain then
      begin
        // EXPLAIN: el plan, sin ejecutar nada
        Result := GQLExplain(QueryObj, CommandName(Parser.CommandType));
        Exit;
      end;

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
                  GQLEnsureFullGraph; // La cercania se mide contra todo el grafo
                  Score := Self.GetClosenessCentrality(StartNode);

                  SetLength(AResultObjects, 1);
                  ResDict := TDictionary<string, TObject>.Create;
                  ResDict.Add('type', TStringWrapper.Create('centrality_score'));
                  ResDict.Add('node', TStringWrapper.Create(StartNode.Name));
                  ResDict.Add('value', TStringWrapper.Create(FormatFloat('0.####', Score, TFormatSettings.Invariant)));
                  AResultObjects[0] := ResDict;

                  Result := Format('Centrality Score for node "%s" (%s): %s', [StartNode.Name, StartNode.NodeLabel, FormatFloat('0.####', Score, TFormatSettings.Invariant)]);
                end
                else
                  Result := 'Node not found for centrality calculation.';
              end;

            // -------------------------------------------------------
            // DEGREES (TOP NODES)
            // -------------------------------------------------------
            cmdDegrees:
              begin
                GQLEnsureFullGraph;
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
            // -------------------------------------------------------
            // SHOW SCHEMA: etiquetas y combinaciones (:A)-[:TIPO]->(:B)
            // -------------------------------------------------------
            cmdShowSchema:
              begin
                Counts := TDictionary<string, Integer>.Create;
                Triples := TDictionary<string, Integer>.Create;
                try
                  // Con driver: lo cuenta la base; si el driver no sabe, se carga todo y se cuenta aqui
                  if not(Assigned(FDriver) and FDriver.GetSchemaCounts(Counts, Triples)) then
                  begin
                    Counts.Clear;
                    Triples.Clear;
                    GQLEnsureFullGraph;
                    for Node in FNodeRegistry.Values do
                    begin
                      GQLBump(Counts, Node.NodeLabel);
                      for Edge in Node.OutgoingEdges do
                      begin
                        Key := '(:' + Edge.FromNode.NodeLabel + ')-[:' + Edge.EdgeLabel + ']->(:' + Edge.ToNode.NodeLabel + ')';
                        GQLBump(Triples, Key);
                      end;
                    end;
                  end;
                  Rows := [];
                  ContextBuilder.AppendLine('### NODE LABELS ###');
                  Keys := Counts.Keys.ToArray;
                  TArray.Sort<string>(Keys, TIStringComparer.Ordinal);
                  for Key in Keys do
                  begin
                    ContextBuilder.AppendFormat('- %s (%d)', [Key, Counts[Key]]).AppendLine;
                    ResDict := TDictionary<string, TObject>.Create;
                    ResDict.Add('type', TStringWrapper.Create('label'));
                    ResDict.Add('value', TStringWrapper.Create(Key));
                    ResDict.Add('count', TGQLValue.CreateValue(Counts[Key]));
                    Rows := Rows + [ResDict];
                  end;
                  ContextBuilder.AppendLine('### RELATIONSHIPS ###');
                  Keys := Triples.Keys.ToArray;
                  TArray.Sort<string>(Keys, TIStringComparer.Ordinal);
                  for Key in Keys do
                  begin
                    ContextBuilder.AppendFormat('- %s (%d)', [Key, Triples[Key]]).AppendLine;
                    ResDict := TDictionary<string, TObject>.Create;
                    ResDict.Add('type', TStringWrapper.Create('schema'));
                    ResDict.Add('value', TStringWrapper.Create(Key));
                    ResDict.Add('count', TGQLValue.CreateValue(Triples[Key]));
                    Rows := Rows + [ResDict];
                  end;
                  AResultObjects := Rows;
                  Result := ContextBuilder.ToString;
                finally
                  Triples.Free;
                  Counts.Free;
                end;
              end;

            // -------------------------------------------------------
            // SHOW PROPERTIES [OF etiqueta]: claves de metadatos
            // -------------------------------------------------------
            cmdShowProperties:
              begin
                PropMap := TObjectDictionary < string, TDictionary < string, Integer >>.Create([doOwnsValues]);
                EdgePropMap := TObjectDictionary < string, TDictionary < string, Integer >>.Create([doOwnsValues]);
                try
                  // Con driver: lo cuenta la base; si el driver no sabe, se carga todo y se cuenta aqui
                  if not(Assigned(FDriver) and FDriver.GetPropertyCounts(Parser.CommandLabel, PropMap, EdgePropMap)) then
                  begin
                    PropMap.Clear;
                    EdgePropMap.Clear;
                    GQLEnsureFullGraph;
                    for Node in FNodeRegistry.Values do
                    begin
                      if (Parser.CommandLabel = '') or SameText(Node.NodeLabel, Parser.CommandLabel) then
                      begin
                        if not PropMap.TryGetValue(Node.NodeLabel, KeyCounts) then
                        begin
                          KeyCounts := TDictionary<string, Integer>.Create;
                          PropMap.Add(Node.NodeLabel, KeyCounts);
                        end;
                        for Key in Node.MetaData.InternalDictionary.Keys do
                          GQLBump(KeyCounts, Key);
                      end;
                      for Edge in Node.OutgoingEdges do
                        if (Parser.CommandLabel = '') or SameText(Edge.EdgeLabel, Parser.CommandLabel) then
                        begin
                          if not EdgePropMap.TryGetValue(Edge.EdgeLabel, KeyCounts) then
                          begin
                            KeyCounts := TDictionary<string, Integer>.Create;
                            EdgePropMap.Add(Edge.EdgeLabel, KeyCounts);
                          end;
                          for Key in Edge.MetaData.InternalDictionary.Keys do
                            GQLBump(KeyCounts, Key);
                        end;
                    end;
                  end;

                  Rows := [];
                  for var Pass := 0 to 1 do
                  begin
                    if Pass = 0 then
                    begin
                      ContextBuilder.AppendLine('### NODE PROPERTIES ###');
                      ContextBuilder.AppendLine('(every node also has name, label and id)');
                      Keys := PropMap.Keys.ToArray;
                    end
                    else
                    begin
                      ContextBuilder.AppendLine('### RELATIONSHIP PROPERTIES ###');
                      ContextBuilder.AppendLine('(every relationship also has label, id and weight)');
                      Keys := EdgePropMap.Keys.ToArray;
                    end;
                    TArray.Sort<string>(Keys, TIStringComparer.Ordinal);
                    for Key in Keys do
                    begin
                      if Pass = 0 then
                      begin
                        KeyCounts := PropMap[Key];
                        Owner := ':' + Key;
                      end
                      else
                      begin
                        KeyCounts := EdgePropMap[Key];
                        Owner := '[:' + Key + ']';
                      end;
                      PropKeys := KeyCounts.Keys.ToArray;
                      TArray.Sort<string>(PropKeys, TIStringComparer.Ordinal);
                      Line := '';
                      for PropKey in PropKeys do
                      begin
                        if Line <> '' then
                          Line := Line + ', ';
                        Line := Line + Format('%s (%d)', [PropKey, KeyCounts[PropKey]]);
                        ResDict := TDictionary<string, TObject>.Create;
                        ResDict.Add('type', TStringWrapper.Create('property'));
                        ResDict.Add('owner', TStringWrapper.Create(Owner));
                        ResDict.Add('value', TStringWrapper.Create(PropKey));
                        ResDict.Add('count', TGQLValue.CreateValue(KeyCounts[PropKey]));
                        Rows := Rows + [ResDict];
                      end;
                      if Line = '' then
                        Line := '(none)';
                      ContextBuilder.Append('- ').Append(Owner).Append(': ').AppendLine(Line);
                    end;
                  end;
                  AResultObjects := Rows;
                  Result := ContextBuilder.ToString;
                finally
                  EdgePropMap.Free;
                  PropMap.Free;
                end;
              end;

            // -------------------------------------------------------
            // GET COMMUNITIES [TOP n]
            // -------------------------------------------------------
            cmdCommunities:
              begin
                GQLEnsureFullGraph;
                CommMap := Self.DetectCommunities;
                Groups := TObjectDictionary < Integer, TList < TAiRagGraphNode >>.Create([doOwnsValues]);
                try
                  for var CP in CommMap do
                  begin
                    if not Groups.TryGetValue(CP.Value, GroupList) then
                    begin
                      GroupList := TList<TAiRagGraphNode>.Create;
                      Groups.Add(CP.Value, GroupList);
                    end;
                    GroupList.Add(CP.Key);
                  end;
                  // Cada grupo por nombre; los grupos de mayor a menor (empate: el primer nombre)
                  for GroupList in Groups.Values do
                    GroupList.Sort(TComparer<TAiRagGraphNode>.Construct(
                      function(const A, B: TAiRagGraphNode): Integer
                      begin
                        Result := CompareText(A.Name, B.Name);
                      end));
                  GroupArr := Groups.Values.ToArray;
                  TArray.Sort < TList < TAiRagGraphNode >> (GroupArr, TComparer < TList < TAiRagGraphNode >>.Construct(
                    function(const A, B: TList<TAiRagGraphNode>): Integer
                    begin
                      Result := B.Count - A.Count;
                      if Result = 0 then
                        Result := CompareText(A[0].Name, B[0].Name);
                    end));
                  Rows := [];
                  ContextBuilder.AppendFormat('### COMMUNITIES (%d) ###', [Length(GroupArr)]).AppendLine;
                  for I := 0 to High(GroupArr) do
                  begin
                    if (Parser.CommandLimit > 0) and (I >= Parser.CommandLimit) then
                      Break;
                    GroupList := GroupArr[I];
                    Line := '';
                    var MemberNames: TArray<string> := [];
                    for J := 0 to GroupList.Count - 1 do
                    begin
                      MemberNames := MemberNames + [GroupList[J].Name];
                      if J < 20 then
                      begin
                        if Line <> '' then
                          Line := Line + ', ';
                        Line := Line + GroupList[J].Name;
                      end;
                    end;
                    if GroupList.Count > 20 then
                      Line := Line + ', ...';
                    ContextBuilder.AppendFormat('- Community %d (%d nodes): %s', [I + 1, GroupList.Count, Line]).AppendLine;
                    ResDict := TDictionary<string, TObject>.Create;
                    ResDict.Add('type', TStringWrapper.Create('community'));
                    ResDict.Add('id', TGQLValue.CreateValue(I + 1));
                    ResDict.Add('size', TGQLValue.CreateValue(GroupList.Count));
                    ResDict.Add('members', TGQLValue.CreateValue(GQLStringsToVarArray(MemberNames)));
                    Rows := Rows + [ResDict];
                  end;
                  AResultObjects := Rows;
                  Result := ContextBuilder.ToString;
                finally
                  Groups.Free;
                  CommMap.Free;
                end;
              end;

            // -------------------------------------------------------
            // GET NEIGHBORS (nodo) [DEPTH n]
            // -------------------------------------------------------
            cmdNeighbors:
              begin
                StartNode := FindNodeByPattern(Parser.CommandSourcePattern);
                if StartNode <> nil then
                begin
                  SeedNodes := TList<TAiRagGraphNode>.Create;
                  try
                    SeedNodes.Add(StartNode);
                    if Parser.CommandLimit > 1 then
                      NodeListResult := Self.ExpandNodeList(SeedNodes, Parser.CommandLimit)
                    else
                      NodeListResult := Self.ExpandNodeList(SeedNodes, 1);
                  finally
                    SeedNodes.Free;
                  end;
                  Result := Self.GraphToContextText(NodeListResult);
                  SetLength(AResultObjects, Length(NodeListResult));
                  for I := 0 to High(NodeListResult) do
                  begin
                    ResDict := TDictionary<string, TObject>.Create;
                    ResDict.Add('type', TStringWrapper.Create('node'));
                    ResDict.Add('element', NodeListResult[I]);
                    AResultObjects[I] := ResDict;
                  end;
                end
                else
                  Result := 'Node not found.';
              end;

            // -------------------------------------------------------
            // GET CENTRALITY TOP n (cercanía)
            // -------------------------------------------------------
            cmdCentralityTop:
              begin
                GQLEnsureFullGraph;
                Scored := TList < TPair < TAiRagGraphNode, Double >>.Create;
                try
                  for Node in FNodeRegistry.Values do
                    Scored.Add(TPair<TAiRagGraphNode, Double>.Create(Node, Self.GetClosenessCentrality(Node)));
                  Scored.Sort(TComparer < TPair < TAiRagGraphNode, Double >>.Construct(
                    function(const A, B: TPair<TAiRagGraphNode, Double>): Integer
                    begin
                      if A.Value > B.Value then
                        Result := -1
                      else if A.Value < B.Value then
                        Result := 1
                      else
                        Result := CompareText(A.Key.Name, B.Key.Name);
                    end));
                  Rows := [];
                  ContextBuilder.AppendFormat('### CENTRALITY (closeness) TOP %d ###', [Parser.CommandLimit]).AppendLine;
                  for I := 0 to Scored.Count - 1 do
                  begin
                    if I >= Parser.CommandLimit then
                      Break;
                    ContextBuilder.AppendFormat('- %s (:%s): %s', [Scored[I].Key.Name, Scored[I].Key.NodeLabel,
                      FormatFloat('0.####', Scored[I].Value, TFormatSettings.Invariant)]).AppendLine;
                    ResDict := TDictionary<string, TObject>.Create;
                    ResDict.Add('type', TStringWrapper.Create('node'));
                    ResDict.Add('element', Scored[I].Key);
                    ResDict.Add('score', TGQLValue.CreateValue(Scored[I].Value));
                    Rows := Rows + [ResDict];
                  end;
                  AResultObjects := Rows;
                  Result := ContextBuilder.ToString;
                finally
                  Scored.Free;
                end;
              end;

            // -------------------------------------------------------
            // ALL SHORTEST PATHS (a) TO (b)
            // -------------------------------------------------------
            cmdAllShortestPaths:
              begin
                StartNode := FindNodeByPattern(Parser.CommandSourcePattern);
                EndNode := FindNodeByPattern(Parser.CommandTargetPattern);
                AllPaths := [];
                if (StartNode <> nil) and (EndNode <> nil) then
                  AllPaths := Self.GetAllShortestPaths(StartNode, EndNode);
                if Length(AllPaths) = 0 then
                  Result := 'No path found or nodes do not exist.'
                else
                begin
                  Rows := [];
                  ContextBuilder.AppendFormat('### ALL SHORTEST PATHS (%d) ###', [Length(AllPaths)]).AppendLine;
                  for I := 0 to High(AllPaths) do
                  begin
                    var PNodes: TArray<TAiRagGraphNode> := [];
                    var PEdges: TArray<TAiRagGraphEdge> := [];
                    for PathObj in AllPaths[I] do
                      if PathObj is TAiRagGraphNode then
                        PNodes := PNodes + [TAiRagGraphNode(PathObj)]
                      else if PathObj is TAiRagGraphEdge then
                        PEdges := PEdges + [TAiRagGraphEdge(PathObj)];
                    var PathVal := TGQLPath.CreatePath(PNodes, PEdges);
                    ContextBuilder.AppendFormat('%d. %s', [I + 1, PathVal.Value]).AppendLine;
                    ResDict := TDictionary<string, TObject>.Create;
                    ResDict.Add('type', TStringWrapper.Create('path'));
                    ResDict.Add('path', PathVal);
                    ResDict.Add('length', TGQLValue.CreateValue(Length(PEdges)));
                    Rows := Rows + [ResDict];
                  end;
                  AResultObjects := Rows;
                  Result := ContextBuilder.ToString;
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
      else if Assigned(QueryObj) and (QueryObj.Writes.Count > 0) then
      begin
        // =========================================================
        // ESCRITURA: CREATE / MERGE / SET / REMOVE / DELETE
        // =========================================================
        if not FAllowWrite then
          raise Exception.Create(AiStr('rag.gql.write_disabled'));
        if QueryObj.NodePatterns.Count = 0 then
          Rows := [TDictionary<string, TObject>.Create] // CREATE/MERGE sin MATCH: una fila vacía
        else
          Rows := Self.Match(QueryObj, 0);
        try
          Line := GQLApplyWrites(QueryObj, Rows);
          if QueryObj.Projections.Count = 0 then
          begin
            // Sin RETURN: solo el resumen (las filas pueden apuntar a lo que se borró)
            FreeGQLResults(Rows);
            ResDict := TDictionary<string, TObject>.Create;
            ResDict.Add('type', TStringWrapper.Create('summary'));
            ResDict.Add('value', TStringWrapper.Create(Line));
            AResultObjects := [ResDict];
            Result := Line;
          end
          else if QueryObj.Tabular then
          begin
            for Stage in QueryObj.Projections do
            begin
              NextRows := RunGQLProjection(Stage, Rows);
              FreeGQLResults(Rows);
              Rows := NextRows;
            end;
            Result := Line + sLineBreak + GQLRowsToText(QueryObj.Projections.Last, Rows);
            AResultObjects := Rows;
            Rows := nil;
          end
          else
          begin
            // RETURN de variables: el resumen y el subgrafo como texto de contexto
            ContextNodes := TList<TAiRagGraphNode>.Create;
            try
              for ResDict in Rows do
                for var WPair in ResDict do
                  if (WPair.Value is TAiRagGraphNode) and (ContextNodes.IndexOf(TAiRagGraphNode(WPair.Value)) = -1) then
                    ContextNodes.Add(TAiRagGraphNode(WPair.Value));
              Result := Line + sLineBreak + Self.GraphToContextText(ContextNodes.ToArray);
            finally
              ContextNodes.Free;
            end;
            AResultObjects := Rows;
            Rows := nil;
          end;
        except
          FreeGQLResults(Rows);
          raise;
        end;
      end
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
              // Camino (p = ... o arista de longitud variable): todos sus nodos
              else if Pair.Value is TGQLPath then
              begin
                for var PathNode in TGQLPath(Pair.Value).Nodes do
                  if ContextNodes.IndexOf(PathNode) = -1 then
                    ContextNodes.Add(PathNode);
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
var
  EdgeID: string;
begin
  if AEdge = nil then
    Exit;
  EdgeID := AEdge.ID;

  // Quitar la referencia de los nodos conectados
  AEdge.FromNode.RemoveOutgoingEdge(AEdge);
  AEdge.ToNode.RemoveIncomingEdge(AEdge);

  UnregisterEdge(AEdge); // Libera la arista

  // Persistir el borrado (antes solo se borraba en memoria y la fila quedaba en la base)
  if Assigned(FDriver) then
    FDriver.DeleteEdge(EdgeID);
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
  NodeID: string;
begin
  if ANode = nil then
    Exit;
  NodeID := ANode.ID;

  // Las aristas se borran una por una con DeleteEdge: salen de las listas del nodo vecino
  // (antes DeleteNode(AID) las liberaba sin quitarlas de ahí y el vecino quedaba apuntando
  // a memoria liberada) y se borran también en el driver. Una arista de un nodo a sí mismo
  // está en las dos listas: se anota una sola vez.
  EdgesToDelete := TList<TAiRagGraphEdge>.Create;
  try
    for Edge in ANode.OutgoingEdges do
      if not EdgesToDelete.Contains(Edge) then
        EdgesToDelete.Add(Edge);
    for Edge in ANode.IncomingEdges do
      if not EdgesToDelete.Contains(Edge) then
        EdgesToDelete.Add(Edge);
    for Edge in EdgesToDelete do
      DeleteEdge(Edge);
  finally
    EdgesToDelete.Free;
  end;

  UnregisterNode(ANode); // Libera el nodo

  // Persistir el borrado (antes DeleteNode(ANode) no avisaba al driver; DeleteNode(AID) sí)
  if Assigned(FDriver) then
    FDriver.DeleteNode(NodeID);
end;

procedure TAiRagGraph.DeleteNode(AID: string);
begin
  // Las dos sobrecargas hacen lo mismo
  DeleteNode(FindNodeByID(AID));
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

procedure TAiRagGraph.LoadFromDriver;
var
  Lbl: string;
  Node: TAiRagGraphNode;
begin
  if not Assigned(FDriver) then
    Exit;
  for Lbl in FDriver.GetUniqueNodeLabels do
    FDriver.FindNodesByLabel(Lbl); // Hidrata cada nodo en el registro
  for Node in FNodeRegistry.Values.ToArray do
    Node.EnsureEdgesAreLoaded;
end;

// Una vez por consulta (ExecuteMakerGQL lo reinicia): lo nuevo en la base se ve en la siguiente
procedure TAiRagGraph.GQLEnsureFullGraph;
begin
  if FGQLFullyLoaded or not Assigned(FDriver) then
    Exit;
  LoadFromDriver;
  FGQLFullyLoaded := True;
end;

function TAiRagGraph.GQLCandidates(APattern: TMatchNodePattern): TArray<TAiRagGraphNode>;
var
  Part: string;
begin
  if APattern.NodeLabel.IsEmpty then
  begin
    // Sin etiqueta: todos los nodos. Con driver, solo los cargados no alcanza (un grafo recien
    // abierto no tiene ninguno y MATCH (n) devolvia vacio)
    GQLEnsureFullGraph;
    Result := FNodeRegistry.Values.ToArray;
  end
  else if APattern.NodeLabel.IndexOf('|') < 0 then
    Result := Self.FindNodesByLabel(APattern.NodeLabel)
  else
  begin
    Result := [];
    for Part in APattern.NodeLabel.Split(['|']) do
      Result := Result + Self.FindNodesByLabel(Part);
  end;
end;

// Busca las coincidencias de AClauses partiendo de AState (que no se modifica) y agrega a
// AResults una copia de cada estado completo que cumple AWhere. APatterns son los patrones de
// nodo de esta parte: los que queden sin enlazar al final se combinan (producto cartesiano).
// Con AFirstOnly se detiene en la primera coincidencia (patrones dentro de un WHERE).
procedure TAiRagGraph.GQLSolve(AQuery: TGraphMatchQuery; AClauses: TList<TMatchClause>; APatterns: TList<TMatchNodePattern>;
  AWhere: TGraphExpression; AState: TDictionary<string, TObject>; AResults: TObjectList<TDictionary<string, TObject>>;
  AFirstOnly: Boolean);
const
  GQL_MAX_HOPS = 10; // Tope de saltos de un patrón *, * .. o *n.. sin máximo
type
  TMatchState = TDictionary<string, TObject>;
var
  Temps: TObjectList<TObject>; // Tramos de camino creados durante el recorrido
  NeedEdgeKeys: Boolean; // La consulta tiene variables de camino: se enlaza cada arista como '#e<n>'
  Done: Boolean;

  function PatternOf(const AVar: string): TMatchNodePattern;
  begin
    Result := AQuery.NodePatternByVariable[AVar];
    if Result = nil then
      raise Exception.Create(AiStr('rag.graph.no_start_pattern'));
  end;

  function EdgeKey(AClauseIndex: Integer): string;
  begin
    Result := '#e' + IntToStr(AClauseIndex);
  end;

  // Arma las variables de camino (p = ...) sobre el estado completo
  procedure BuildPaths(AState: TMatchState);
  var
    Def: TGQLPathDef;
    Nodes: TList<TAiRagGraphNode>;
    Edges: TList<TAiRagGraphEdge>;
    Obj, Tgt: TObject;
    Seg: TGQLPath;
    I, J: Integer;
    Path: TGQLPath;
  begin
    for Def in AQuery.Paths do
    begin
      Nodes := TList<TAiRagGraphNode>.Create;
      Edges := TList<TAiRagGraphEdge>.Create;
      try
        if AState.TryGetValue(Def.StartVar, Obj) and (Obj is TAiRagGraphNode) then
          Nodes.Add(TAiRagGraphNode(Obj));
        for I := Def.FirstClause to Def.LastClause do
        begin
          if not AState.TryGetValue(EdgeKey(I), Obj) then
            Continue;
          if Obj is TAiRagGraphEdge then
          begin
            Edges.Add(TAiRagGraphEdge(Obj));
            if AState.TryGetValue(AQuery.MatchClauses[I].TargetNodeVar, Tgt) and (Tgt is TAiRagGraphNode) then
              Nodes.Add(TAiRagGraphNode(Tgt));
          end
          else if Obj is TGQLPath then
          begin
            Seg := TGQLPath(Obj);
            for J := 0 to High(Seg.Edges) do
              Edges.Add(Seg.Edges[J]);
            for J := 1 to High(Seg.Nodes) do
              Nodes.Add(Seg.Nodes[J]);
          end;
        end;
        Path := TGQLPath.CreatePath(Nodes.ToArray, Edges.ToArray);
        Temps.Add(Path);
        AState.AddOrSetValue(Def.Variable, Path);
      finally
        Edges.Free;
        Nodes.Free;
      end;
    end;
  end;

  procedure FindMatchesRecursive(AClauseIndex: Integer; ACurrentState: TMatchState);
  var
    Clause: TMatchClause;
    Pattern: TMatchNodePattern;
    SourceObj, TargetObj: TObject;
    FromNode: TAiRagGraphNode;
    CurrentEdge: TAiRagGraphEdge;
    Dir: TGraphDirection;
    Work: TMatchState;

    // Enlaza AVar con cada candidato de su patrón y sigue en la misma cláusula
    procedure BindCandidates(const AVar: string; AClauseIndex: Integer; ACurrentState: TMatchState);
    var
      Pattern: TMatchNodePattern;
      Cand: TAiRagGraphNode;
      NewState: TMatchState;
    begin
      Pattern := PatternOf(AVar);
      for Cand in GQLCandidates(Pattern) do
      begin
        if Done then
          Exit;
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
    end;

    // Enlaza el extremo, la arista (o el tramo) y sigue con la cláusula siguiente
    procedure Advance(AClause: TMatchClause; AEdgeObj: TObject; ANeighbor: TAiRagGraphNode; const ANeighborVar: string;
      AClauseIndex: Integer; ACurrentState: TMatchState);
    var
      BoundObject: TObject;
      NewState: TMatchState;
    begin
      if not PatternOf(ANeighborVar).Matches(ANeighbor) then
        Exit;
      if ACurrentState.TryGetValue(ANeighborVar, BoundObject) and (BoundObject <> ANeighbor) then
        Exit;
      if (not AClause.EdgePattern.Variable.IsEmpty) and ACurrentState.TryGetValue(AClause.EdgePattern.Variable, BoundObject) and
        (BoundObject <> AEdgeObj) then
        Exit;

      NewState := TMatchState.Create(ACurrentState);
      try
        NewState.AddOrSetValue(ANeighborVar, ANeighbor);
        if not AClause.EdgePattern.Variable.IsEmpty then
          NewState.AddOrSetValue(AClause.EdgePattern.Variable, AEdgeObj);
        if NeedEdgeKeys then
          NewState.AddOrSetValue(EdgeKey(AClauseIndex), AEdgeObj);
        FindMatchesRecursive(AClauseIndex + 1, NewState);
      finally
        NewState.Free;
      end;
    end;

    // Un salto. APerspective es la dirección vista desde el ORIGEN de la cláusula.
    procedure TryEdge(AClause: TMatchClause; AEdge: TAiRagGraphEdge; ANeighbor: TAiRagGraphNode; APerspective: TGraphDirection;
      const ANeighborVar: string; AClauseIndex: Integer; ACurrentState: TMatchState);
    begin
      if AClause.EdgePattern.Matches(AEdge, APerspective) then
        Advance(AClause, AEdge, ANeighbor, ANeighborVar, AClauseIndex, ACurrentState);
    end;

    // Longitud variable: caminos de MinHops a MaxHops saltos sin repetir aristas.
    // AReversed: se recorre desde el destino de la cláusula (el tramo se guarda en el orden del patrón).
    procedure ExpandVariable(AClause: TMatchClause; AStart: TAiRagGraphNode; AReversed: Boolean; const ANeighborVar: string;
      AClauseIndex: Integer; ACurrentState: TMatchState);
    var
      MaxHops: Integer;
      PathNodes: TList<TAiRagGraphNode>;
      PathEdges: TList<TAiRagGraphEdge>;

      procedure Emit(AEnd: TAiRagGraphNode);
      var
        NA: TArray<TAiRagGraphNode>;
        EA: TArray<TAiRagGraphEdge>;
        Seg: TGQLPath;
        K: Integer;
        TmpN: TAiRagGraphNode;
        TmpE: TAiRagGraphEdge;
      begin
        NA := PathNodes.ToArray;
        EA := PathEdges.ToArray;
        if AReversed then
        begin
          for K := 0 to (Length(NA) div 2) - 1 do
          begin
            TmpN := NA[K];
            NA[K] := NA[High(NA) - K];
            NA[High(NA) - K] := TmpN;
          end;
          for K := 0 to (Length(EA) div 2) - 1 do
          begin
            TmpE := EA[K];
            EA[K] := EA[High(EA) - K];
            EA[High(EA) - K] := TmpE;
          end;
        end;
        Seg := TGQLPath.CreatePath(NA, EA);
        Temps.Add(Seg);
        Advance(AClause, Seg, AEnd, ANeighborVar, AClauseIndex, ACurrentState);
      end;

      procedure Walk(ANode: TAiRagGraphNode; ADepth: Integer);
      var
        E: TAiRagGraphEdge;
        Dir: TGraphDirection;

        procedure Hop(AEdge: TAiRagGraphEdge; ANext: TAiRagGraphNode; APerspective: TGraphDirection);
        begin
          if Done or PathEdges.Contains(AEdge) or not AClause.EdgePattern.Matches(AEdge, APerspective) then
            Exit;
          PathEdges.Add(AEdge);
          PathNodes.Add(ANext);
          try
            Walk(ANext, ADepth + 1);
          finally
            PathEdges.Delete(PathEdges.Count - 1);
            PathNodes.Delete(PathNodes.Count - 1);
          end;
        end;

      begin
        if Done then
          Exit;
        if ADepth >= AClause.EdgePattern.MinHops then
          Emit(ANode);
        if ADepth >= MaxHops then
          Exit;
        Dir := AClause.EdgePattern.Direction;
        // Desde el origen se sigue la flecha; desde el destino, al revés
        if Dir in [gdOutgoing, gdBoth] then
        begin
          if AReversed then
            for E in ANode.IncomingEdges do
              Hop(E, E.FromNode, gdOutgoing)
          else
            for E in ANode.OutgoingEdges do
              Hop(E, E.ToNode, gdOutgoing);
        end;
        if Dir in [gdIncoming, gdBoth] then
        begin
          if AReversed then
            for E in ANode.OutgoingEdges do
              Hop(E, E.ToNode, gdIncoming)
          else
            for E in ANode.IncomingEdges do
              Hop(E, E.FromNode, gdIncoming);
        end;
      end;

    begin
      MaxHops := AClause.EdgePattern.MaxHops;
      if MaxHops < 0 then
        MaxHops := GQL_MAX_HOPS;
      PathNodes := TList<TAiRagGraphNode>.Create;
      PathEdges := TList<TAiRagGraphEdge>.Create;
      try
        PathNodes.Add(AStart);
        Walk(AStart, 0);
      finally
        PathEdges.Free;
        PathNodes.Free;
      end;
    end;

  begin
    if Done then
      Exit;

    if AClauseIndex >= AClauses.Count then
    begin
      // Patrones de nodo sin enlazar (MATCH (a:X), (b:Y) o un nodo suelto): producto cartesiano
      for Pattern in APatterns do
        if not ACurrentState.ContainsKey(Pattern.Variable) then
        begin
          BindCandidates(Pattern.Variable, AClauseIndex, ACurrentState);
          Exit;
        end;

      Work := TMatchState.Create(ACurrentState);
      try
        if AQuery.Paths.Count > 0 then
          BuildPaths(Work);
        // --- AQUÍ ENTRA EL WHERE ---
        if (AWhere = nil) or GQLToBool(EvaluateGraphExpression(AWhere, Work)) then
        begin
          AResults.Add(GQLCloneRow(Work));
          if AFirstOnly then
            Done := True;
        end;
      finally
        Work.Free;
      end;
      Exit;
    end;

    Clause := AClauses[AClauseIndex];
    Dir := Clause.EdgePattern.Direction;

    if ACurrentState.TryGetValue(Clause.SourceNodeVar, SourceObj) and (SourceObj is TAiRagGraphNode) then
    begin
      // Origen enlazado: se recorre hacia el destino
      FromNode := TAiRagGraphNode(SourceObj);
      if Clause.EdgePattern.IsVariableLength then
        ExpandVariable(Clause, FromNode, False, Clause.TargetNodeVar, AClauseIndex, ACurrentState)
      else
      begin
        if Dir in [gdOutgoing, gdBoth] then
          for CurrentEdge in FromNode.OutgoingEdges do
            TryEdge(Clause, CurrentEdge, CurrentEdge.ToNode, gdOutgoing, Clause.TargetNodeVar, AClauseIndex, ACurrentState);
        if Dir in [gdIncoming, gdBoth] then
          for CurrentEdge in FromNode.IncomingEdges do
            TryEdge(Clause, CurrentEdge, CurrentEdge.FromNode, gdIncoming, Clause.TargetNodeVar, AClauseIndex, ACurrentState);
      end;
    end
    else if ACurrentState.TryGetValue(Clause.TargetNodeVar, TargetObj) and (TargetObj is TAiRagGraphNode) then
    begin
      // Solo el destino enlazado (patrones convergentes): se recorre al revés.
      // Una arista origen->destino es entrante para el destino.
      FromNode := TAiRagGraphNode(TargetObj);
      if Clause.EdgePattern.IsVariableLength then
        ExpandVariable(Clause, FromNode, True, Clause.SourceNodeVar, AClauseIndex, ACurrentState)
      else
      begin
        if Dir in [gdOutgoing, gdBoth] then
          for CurrentEdge in FromNode.IncomingEdges do
            TryEdge(Clause, CurrentEdge, CurrentEdge.FromNode, gdOutgoing, Clause.SourceNodeVar, AClauseIndex, ACurrentState);
        if Dir in [gdIncoming, gdBoth] then
          for CurrentEdge in FromNode.OutgoingEdges do
            TryEdge(Clause, CurrentEdge, CurrentEdge.ToNode, gdIncoming, Clause.SourceNodeVar, AClauseIndex, ACurrentState);
      end;
    end
    else
      // Ninguno enlazado: el origen de esta cláusula es un ancla nueva
      BindCandidates(Clause.SourceNodeVar, AClauseIndex, ACurrentState);
  end;

begin
  Done := False;
  NeedEdgeKeys := (AQuery.Paths.Count > 0) and (AClauses = TList<TMatchClause>(AQuery.MatchClauses));
  Temps := TObjectList<TObject>.Create(True);
  try
    FindMatchesRecursive(0, AState);
  finally
    Temps.Free;
  end;
end;

// ---------------------------------------------------------------------------
// MakerGQL: escritura (CREATE, MERGE, SET, REMOVE, DELETE). Solo con AllowWrite = True.
// ---------------------------------------------------------------------------

// Texto que se convierte en embedding: el mismo formato que usa TAiRagGraphBuilder
// (son datos para indexar, no instrucciones para el modelo)
function GQLNodeEmbeddingText(ANode: TAiRagGraphNode): string;
var
  SB: TStringBuilder;
  Pair: TPair<string, Variant>;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendFormat('Entidad: %s. Categoria: %s.', [ANode.Name, ANode.NodeLabel]);
    for Pair in ANode.MetaData.InternalDictionary do
      SB.AppendFormat(' La propiedad %s tiene el valor %s.', [Pair.Key, GQLToText(Pair.Value)]);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function GQLNewID: string;
begin
  Result := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '').ToLower;
end;

function TAiRagGraph.GQLApplyWrites(AQuery: TGraphMatchQuery; const ARows: TArray<TDictionary<string, TObject>>): string;
var
  W: TGQLWriteClause;
  Row: TDictionary<string, TObject>;
  NodesCreated, EdgesCreated, PropsSet, LabelsSet, NodesDeleted, EdgesDeleted: Integer;
  TouchedNodes: TList<TAiRagGraphNode>;
  TouchedEdges: TList<TAiRagGraphEdge>;
  DelNodes: TList<TAiRagGraphNode>;
  DelEdges: TList<TAiRagGraphEdge>;
  DelVar: string;
  Obj: TObject;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  Remaining: Integer;

  procedure TouchNode(ANode: TAiRagGraphNode);
  begin
    if not TouchedNodes.Contains(ANode) then
      TouchedNodes.Add(ANode);
  end;

  procedure TouchEdge(AEdge: TAiRagGraphEdge);
  begin
    if not TouchedEdges.Contains(AEdge) then
      TouchedEdges.Add(AEdge);
  end;

  procedure ApplySetItems(AItems: TObjectList<TGQLSetItem>; ARemove: Boolean; ARow: TDictionary<string, TObject>);
  var
    It: TGQLSetItem;
    Target: TObject;
    V: Variant;
    D: Double;
  begin
    for It in AItems do
    begin
      // Una variable sin enlazar (p. ej. de un OPTIONAL MATCH vacío) no hace nada, como en Cypher
      if not ARow.TryGetValue(It.Variable, Target) or not((Target is TAiRagGraphNode) or (Target is TAiRagGraphEdge)) then
        Continue;

      if It.NewLabel <> '' then
      begin
        if Target is TAiRagGraphNode then
          TAiRagGraphNode(Target).NodeLabel := It.NewLabel
        else
          TAiRagGraphEdge(Target).EdgeLabel := It.NewLabel;
        Inc(LabelsSet);
      end
      else
      begin
        if SameText(It.Key, 'id') then
          raise EGQLParserError.CreateAt(AiStr('rag.gql.set_id'), 0);
        if ARemove then
          V := Null
        else
          V := EvaluateGraphExpression(It.Expr, ARow);

        if Target is TAiRagGraphNode then
        begin
          if SameText(It.Key, 'name') then
            TAiRagGraphNode(Target).Name := GQLToText(V)
          else if SameText(It.Key, 'label') then
            TAiRagGraphNode(Target).NodeLabel := GQLToText(V)
          else if GQLIsNull(V) then
            TAiRagGraphNode(Target).MetaData.Remove(It.Key)
          else
            TAiRagGraphNode(Target).MetaData[It.Key] := V;
        end
        else
        begin
          if SameText(It.Key, 'label') then
            TAiRagGraphEdge(Target).EdgeLabel := GQLToText(V)
          else if SameText(It.Key, 'weight') then
          begin
            if GQLTryNumber(V, D) then
              TAiRagGraphEdge(Target).Weight := D;
          end
          else if GQLIsNull(V) then
            TAiRagGraphEdge(Target).MetaData.Remove(It.Key)
          else
            TAiRagGraphEdge(Target).MetaData[It.Key] := V;
        end;
        Inc(PropsSet);
      end;

      if Target is TAiRagGraphNode then
        TouchNode(TAiRagGraphNode(Target))
      else
        TouchEdge(TAiRagGraphEdge(Target));
    end;
  end;

  // Crea lo que falte del patrón: los nodos sin enlazar y todas las relaciones
  procedure CreatePattern(APattern: TGraphMatchQuery; ARow: TDictionary<string, TObject>);
  var
    P: TMatchNodePattern;
    C: TMatchClause;
    N: TAiRagGraphNode;
    E: TAiRagGraphEdge;
    FromObj, ToObj, Tmp: TObject;
    Pair: TPair<string, Variant>;
    ID, Name: string;
    Weight: Double;
    V: Variant;
  begin
    for P in APattern.NodePatterns do
    begin
      if ARow.TryGetValue(P.Variable, Tmp) and (Tmp is TAiRagGraphNode) then
        Continue; // Ya existe (viene del MATCH o de un MERGE anterior)
      ID := '';
      Name := '';
      if P.Properties.TryGetValue('id', V) then
        ID := GQLToText(V);
      if ID = '' then
        ID := GQLNewID;
      if P.Properties.TryGetValue('name', V) then
        Name := GQLToText(V);
      N := Self.NewNode(ID, P.NodeLabel, Name);
      try
        for Pair in P.Properties do
          if not(SameText(Pair.Key, 'id') or SameText(Pair.Key, 'name') or SameText(Pair.Key, 'label')) and
            not GQLIsNull(Pair.Value) then
            N.MetaData[Pair.Key] := Pair.Value;
        N.Text := GQLNodeEmbeddingText(N);
        if Assigned(FEmbeddings) then
        begin
          N.Model := FEmbeddings.Model;
          N.Data := FEmbeddings.CreateEmbedding(N.Text, 'user');
        end;
      except
        N.Free;
        raise;
      end;
      N := Self.AddNode(N); // Persiste en el driver; si el id ya existía devuelve ese nodo
      ARow.AddOrSetValue(P.Variable, N);
      Inc(NodesCreated);
    end;

    for C in APattern.MatchClauses do
    begin
      if not(ARow.TryGetValue(C.SourceNodeVar, FromObj) and (FromObj is TAiRagGraphNode) and
        ARow.TryGetValue(C.TargetNodeVar, ToObj) and (ToObj is TAiRagGraphNode)) then
        Continue;
      if C.EdgePattern.Direction = gdIncoming then // (a)<-[:R]-(b): la arista va de b a a
      begin
        Tmp := FromObj;
        FromObj := ToObj;
        ToObj := Tmp;
      end;
      Weight := 1.0;
      if C.EdgePattern.Properties.TryGetValue('weight', V) and not GQLTryNumber(V, Weight) then
        Weight := 1.0;
      E := Self.NewEdge(TAiRagGraphNode(FromObj), TAiRagGraphNode(ToObj), GQLNewID, C.EdgePattern.EdgeLabel,
        C.EdgePattern.EdgeLabel, Weight);
      for Pair in C.EdgePattern.Properties do
        if not(SameText(Pair.Key, 'weight') or SameText(Pair.Key, 'id') or SameText(Pair.Key, 'label')) and
          not GQLIsNull(Pair.Value) then
          E.MetaData[Pair.Key] := Pair.Value;
      E := Self.AddEdge(E);
      if C.EdgePattern.Variable <> '' then
        ARow.AddOrSetValue(C.EdgePattern.Variable, E);
      Inc(EdgesCreated);
    end;
  end;

  procedure MergePattern(AW: TGQLWriteClause; ARow: TDictionary<string, TObject>);
  var
    Found: TObjectList<TDictionary<string, TObject>>;
    Pair: TPair<string, TObject>;
    D: TDictionary<string, TObject>;
  begin
    // Busca el patrón con las variables ya enlazadas; si no está, lo crea
    Found := TObjectList < TDictionary < string, TObject >>.Create(True);
    try
      GQLSolve(AW.Pattern, AW.Pattern.MatchClauses, AW.Pattern.NodePatterns, nil, ARow, Found, True);
      if Found.Count > 0 then
      begin
        for Pair in Found[0] do
          if (Pair.Value is TAiRagGraphNode) or (Pair.Value is TAiRagGraphEdge) then
            ARow.AddOrSetValue(Pair.Key, Pair.Value);
        ApplySetItems(AW.OnMatch, False, ARow);
      end
      else
      begin
        CreatePattern(AW.Pattern, ARow);
        ApplySetItems(AW.OnCreate, False, ARow);
      end;
    finally
      for D in Found do
        GQLFreeRowValues(D);
      Found.Free;
    end;
  end;

begin
  NodesCreated := 0;
  EdgesCreated := 0;
  PropsSet := 0;
  LabelsSet := 0;
  NodesDeleted := 0;
  EdgesDeleted := 0;
  TouchedNodes := TList<TAiRagGraphNode>.Create;
  TouchedEdges := TList<TAiRagGraphEdge>.Create;
  DelNodes := TList<TAiRagGraphNode>.Create;
  DelEdges := TList<TAiRagGraphEdge>.Create;
  BeginUpdate; // Los índices (nombre, etiqueta) se reconstruyen una vez al final
  try
    for W in AQuery.Writes do
      for Row in ARows do
        case W.Kind of
          wkCreate:
            CreatePattern(W.Pattern, Row);
          wkMerge:
            MergePattern(W, Row);
          wkSet:
            ApplySetItems(W.Items, False, Row);
          wkRemove:
            ApplySetItems(W.Items, True, Row);
          wkDelete:
            for DelVar in W.DeleteVars do
              if Row.TryGetValue(DelVar, Obj) then
              begin
                if (Obj is TAiRagGraphNode) and not DelNodes.Contains(TAiRagGraphNode(Obj)) then
                begin
                  // Sin DETACH, un nodo con relaciones que no se borran también es un error
                  if not W.Detach then
                  begin
                    Remaining := 0;
                    for Edge in TAiRagGraphNode(Obj).OutgoingEdges do
                      if not DelEdges.Contains(Edge) then
                        Inc(Remaining);
                    for Edge in TAiRagGraphNode(Obj).IncomingEdges do
                      if not DelEdges.Contains(Edge) then
                        Inc(Remaining);
                    if Remaining > 0 then
                      raise EGQLParserError.CreateAt(AiFmt('rag.gql.delete_edges', [TAiRagGraphNode(Obj).Name, Remaining]), 0);
                  end;
                  DelNodes.Add(TAiRagGraphNode(Obj));
                end
                else if (Obj is TAiRagGraphEdge) and not DelEdges.Contains(TAiRagGraphEdge(Obj)) then
                  DelEdges.Add(TAiRagGraphEdge(Obj));
              end;
        end;

    // Borrados al final: primero las relaciones (un nodo borrado se lleva las suyas)
    for Edge in DelEdges do
    begin
      TouchedEdges.Remove(Edge);
      Self.DeleteEdge(Edge); // Avisa al driver
      Inc(EdgesDeleted);
    end;
    for Node in DelNodes do
    begin
      TouchedNodes.Remove(Node);
      for Edge in Node.OutgoingEdges do
        TouchedEdges.Remove(Edge);
      for Edge in Node.IncomingEdges do
        TouchedEdges.Remove(Edge);
      Inc(EdgesDeleted, Node.OutgoingEdges.Count + Node.IncomingEdges.Count);
      Self.DeleteNode(Node); // Borra sus relaciones (también en el vecino) y avisa al driver
      Inc(NodesDeleted);
    end;

    // Lo modificado: embedding nuevo (si hay motor) y guardado en el driver (upsert)
    for Node in TouchedNodes do
    begin
      Node.Text := GQLNodeEmbeddingText(Node);
      if Assigned(FEmbeddings) then
        Node.Data := FEmbeddings.CreateEmbedding(Node.Text, 'user');
      if Assigned(FDriver) then
        FDriver.AddNode(Node);
    end;
    if Assigned(FDriver) then
      for Edge in TouchedEdges do
        FDriver.AddEdge(Edge);
  finally
    EndUpdate;
    DelEdges.Free;
    DelNodes.Free;
    TouchedEdges.Free;
    TouchedNodes.Free;
  end;

  Result := Format('Nodes created: %d, relationships created: %d, properties set: %d, labels set: %d, ' +
    'nodes deleted: %d, relationships deleted: %d.', [NodesCreated, EdgesCreated, PropsSet, LabelsSet, NodesDeleted, EdgesDeleted]);
end;

function TAiRagGraph.Match(AQuery: TGraphMatchQuery; ADepth: Integer = 0): TArray<TDictionary<string, TObject>>;
var
  Results, NextResults, PartResults: TObjectList<TDictionary<string, TObject>>;
  MainPatterns: TList<TMatchNodePattern>;
  InitialState: TDictionary<string, TObject>;
  Part: TGQLOptionalMatch;
  Row: TDictionary<string, TObject>;
  P: TMatchNodePattern;

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

  procedure FreeResults(AList: TObjectList<TDictionary<string, TObject>>);
  var
    D: TDictionary<string, TObject>;
  begin
    for D in AList do
      GQLFreeRowValues(D);
    AList.Free;
  end;

begin
  Result := [];
  if (AQuery = nil) or (AQuery.NodePatterns.Count = 0) then
    Exit;

  Results := TObjectList < TDictionary < string, TObject >>.Create(True);
  try
    // --- MATCH principal (los patrones de OPTIONAL MATCH no participan aquí) ---
    MainPatterns := TList<TMatchNodePattern>.Create;
    InitialState := TDictionary<string, TObject>.Create;
    try
      for P in AQuery.NodePatterns do
        if not P.Optional then
          MainPatterns.Add(P);
      GQLSolve(AQuery, AQuery.MatchClauses, MainPatterns, AQuery.WhereClause, InitialState, Results, False);
    finally
      InitialState.Free;
      MainPatterns.Free;
    end;

    // --- OPTIONAL MATCH: cada fila se extiende; si no hay extensión, se conserva tal cual ---
    for Part in AQuery.OptionalParts do
    begin
      NextResults := TObjectList < TDictionary < string, TObject >>.Create(True);
      try
        for Row in Results do
        begin
          PartResults := TObjectList < TDictionary < string, TObject >>.Create(True);
          try
            GQLSolve(AQuery, Part.Clauses, Part.Patterns, Part.Where, Row, PartResults, False);
            if PartResults.Count = 0 then
              NextResults.Add(GQLCloneRow(Row))
            else
            begin
              PartResults.OwnsObjects := False;
              for Dict in PartResults do
                NextResults.Add(Dict);
            end;
          finally
            PartResults.Free;
          end;
        end;
      except
        FreeResults(NextResults);
        raise;
      end;
      FreeResults(Results);
      Results := NextResults;
    end;

    // --- PASO COMÚN: EXPANSIÓN DEL SUBGRAFO (si ADepth > 0 y hay resultados) ---
    if (ADepth > 0) and (Results.Count > 0) then
    begin
      // 1. Recolectar nodos semilla únicos (también los de los caminos)
      SeedNodesSet := TDictionary<TAiRagGraphNode, Boolean>.Create;
      try
        for MatchDict in Results do
          for Pair in MatchDict do
            if Pair.Value is TAiRagGraphNode then
              SeedNodesSet.AddOrSetValue(Pair.Value as TAiRagGraphNode, True)
            else if Pair.Value is TGQLPath then
              for Node in TGQLPath(Pair.Value).Nodes do
                SeedNodesSet.AddOrSetValue(Node, True);

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
            for Node in ExpandedNodesArray do
            begin
              Dict := TDictionary<string, TObject>.Create;
              Dict.Add('type', TStringWrapper.Create('node'));
              Dict.Add('element', Node);
              ExpandedResults.Add(Dict);
            end;

            var
            NodeSet := TDictionary<TAiRagGraphNode, Boolean>.Create;
            try
              for Node in ExpandedNodesArray do
                NodeSet.Add(Node, True);
              for Node in ExpandedNodesArray do
                for Edge in Node.OutgoingEdges do
                  if NodeSet.ContainsKey(Edge.ToNode) then
                    EdgeSet.AddOrSetValue(Edge, True);
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

            for MatchDict in Results do
              GQLFreeRowValues(MatchDict);
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
  if (not NodeLabel.IsEmpty) and (not GQLLabelMatches(ANode.NodeLabel, NodeLabel)) then
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
  MinHops := 1;
  MaxHops := 1;
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
  if (not EdgeLabel.IsEmpty) and (not GQLLabelMatches(AEdge.EdgeLabel, EdgeLabel)) then
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

function TMatchEdgePattern.IsVariableLength: Boolean;
begin
  Result := (MinHops <> 1) or (MaxHops <> 1);
end;

{ TGQLSetItem }

destructor TGQLSetItem.Destroy;
begin
  Expr.Free;
  inherited;
end;

{ TGQLWriteClause }

constructor TGQLWriteClause.Create;
begin
  inherited;
  Items := TObjectList<TGQLSetItem>.Create(True);
  OnCreate := TObjectList<TGQLSetItem>.Create(True);
  OnMatch := TObjectList<TGQLSetItem>.Create(True);
  DeleteVars := TList<string>.Create;
end;

destructor TGQLWriteClause.Destroy;
begin
  Pattern.Free;
  DeleteVars.Free;
  OnMatch.Free;
  OnCreate.Free;
  Items.Free;
  inherited;
end;

{ TGQLOptionalMatch }

constructor TGQLOptionalMatch.Create;
begin
  inherited;
  Clauses := TObjectList<TMatchClause>.Create(True);
  Patterns := TList<TMatchNodePattern>.Create;
end;

destructor TGQLOptionalMatch.Destroy;
begin
  Where.Free;
  Patterns.Free;
  Clauses.Free;
  inherited;
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
  FOptionalParts := TObjectList<TGQLOptionalMatch>.Create(True);
  FPaths := TList<TGQLPathDef>.Create;
  FWrites := TObjectList<TGQLWriteClause>.Create(True);
  FDepth := 0;
end;

destructor TGraphMatchQuery.Destroy;
begin
  if Assigned(FWhereClause) then
    FWhereClause.Free; // Esto disparar� la liberaci�n en cadena de todo el �rbol

  FWrites.Free;
  FPaths.Free;
  FOptionalParts.Free;
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

function TAiRagGraphDriverBase.GetSchemaCounts(ALabelCounts, ATripleCounts: TDictionary<string, Integer>): Boolean;
begin
  Result := False; // No implementado: el grafo carga todo y cuenta en memoria
end;

function TAiRagGraphDriverBase.GetPropertyCounts(const ALabel: string;
  ANodeProps, AEdgeProps: TObjectDictionary<string, TDictionary<string, Integer>>): Boolean;
begin
  Result := False; // No implementado: el grafo carga todo y cuenta en memoria
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

function TGQLValue.Clone: TGQLValue;
begin
  Result := TGQLValue.CreateValue(Data);
end;

{ TGQLPath }
constructor TGQLPath.CreatePath(const ANodes: TArray<TAiRagGraphNode>; const AEdges: TArray<TAiRagGraphEdge>);
var
  SB: TStringBuilder;
  I: Integer;
  E: TAiRagGraphEdge;
begin
  Nodes := ANodes;
  Edges := AEdges;
  SB := TStringBuilder.Create;
  try
    for I := 0 to High(Nodes) do
    begin
      SB.Append('(').Append(Nodes[I].Name).Append(')');
      if I <= High(Edges) then
      begin
        E := Edges[I];
        if E.FromNode = Nodes[I] then
          SB.Append('-[:').Append(E.EdgeLabel).Append(']->')
        else
          SB.Append('<-[:').Append(E.EdgeLabel).Append(']-');
      end;
    end;
    inherited CreateValue(SB.ToString);
  finally
    SB.Free;
  end;
end;

function TGQLPath.Clone: TGQLValue;
begin
  Result := TGQLPath.CreatePath(Nodes, Edges);
end;

function TGQLPath.NodeNames: Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, Length(Nodes) - 1], varVariant);
  for I := 0 to High(Nodes) do
    Result[I] := Nodes[I].Name;
end;

function TGQLPath.EdgeLabels: Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, Length(Edges) - 1], varVariant);
  for I := 0 to High(Edges) do
    Result[I] := Edges[I].EdgeLabel;
end;

{ TSimilarExpr }
constructor TSimilarExpr.Create(ATarget, AText, AThreshold: TGraphExpression; AStrict: Boolean);
begin
  inherited Create;
  Kind := ekSimilar;
  Target := ATarget;
  Text := AText;
  Threshold := AThreshold;
  StrictGreater := AStrict;
end;

destructor TSimilarExpr.Destroy;
begin
  Target.Free;
  Text.Free;
  Threshold.Free;
  inherited;
end;

{ TPatternExpr }
constructor TPatternExpr.Create(AQuery: TGraphMatchQuery);
begin
  inherited Create;
  Kind := ekPattern;
  Query := AQuery;
end;

destructor TPatternExpr.Destroy;
begin
  Query.Free;
  inherited;
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
