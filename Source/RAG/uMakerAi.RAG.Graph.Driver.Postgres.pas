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


unit uMakerAi.RAG.Graph.Driver.Postgres;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Data.DB, FireDAC.Comp.Client, FireDac.Stan.Param,  uMakerAi.RAG.Vectors, uMakerAi.Embeddings.core,
  uMakerAi.RAG.Graph.core;


type
  TAiRagGraphPostgresDriver = class(TAiRagGraphDriverBase)
  private
    FConnection: TFDConnection;
    FCurrentEntidad: string;
    FTableName: String;
    FEdgesTableName: String;
    FNodesTableName: String;
    function NewQuery: TFDQuery;
    function PropertiesToJSONString(const AProperties: TDictionary<string, Variant>): string;
    procedure SetTableName(const Value: String);
  protected
    function FindNodeDataByID(const ANodeID: string; out ANodeData: TNodeDataRecord): Boolean; override;
    function FindEdgeDataByID(const AEdgeID: string; out AEdgeData: TEdgeDataRecord): Boolean; override;
    procedure GetNodeEdges(ANode: TAiRagGraphNode); override;
    procedure AddNode(ANode: TAiRagGraphNode); override;
    procedure AddEdge(AEdge: TAiRagGraphEdge); override;
    procedure DeleteNode(const ANodeID: string); override;
    procedure DeleteEdge(const AEdgeID: string); override;
    function GetUniqueNodeLabels: TArray<string>; override;
    function GetUniqueEdgeLabels: TArray<string>; override;
    function FindNodeByName(const AName, ANodeLabel: string): TAiRagGraphNode; override;
    function FindNodesByLabel(const ALabel: string): TArray<TAiRagGraphNode>; override;
    function FindNodesByProperty(const AKey: string; const AValue: Variant): TArray<TAiRagGraphNode>; override;
    function FindNodeNamesByLabel(const ANodeLabel, ASearchText: string; ALimit: Integer): TArray<string>; override;
    function SearchNodes(const APrompt: string; ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>; override;
    function Query(const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateSchema(const ABaseTableName: string; AVectorDim: Integer);
  published
    property Connection: TFDConnection read FConnection write FConnection;
    property CurrentEntidad: string read FCurrentEntidad write FCurrentEntidad;
    Property TableName: String read FTableName write SetTableName;
  end;

procedure Register;

implementation

uses System.Variants, System.JSON;

procedure Register;
begin
  RegisterComponents('MakerAI.RAG.Drivers', [TAiRagGraphPostgresDriver]);
end;

{ TAiRagGraphPostgresDriver }

constructor TAiRagGraphPostgresDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentEntidad := 'DEFAULT';
  FTableName := 'graph_';
  FEdgesTableName := FTableName + 'edges';
  FNodesTableName := FTableName + 'nodes';
end;

procedure TAiRagGraphPostgresDriver.CreateSchema(const ABaseTableName: string; AVectorDim: Integer);
var
  Query: TFDQuery;
  NodesTableName, EdgesTableName: string;
  VectorType: string;
begin
  if not Assigned(FConnection) or not FConnection.Connected then
    raise Exception.Create('La conexión a la base de datos no está activa.');

  // Configuramos nombres basados en el parámetro de entrada
  NodesTableName := ABaseTableName + 'nodes';
  EdgesTableName := ABaseTableName + 'edges';
  VectorType := Format('vector(%d)', [AVectorDim]);

  Query := NewQuery;
  try
    // --- 1. Crear la extensión vector si no existe (pgvector) ---
    // NOTA: Esto solo funcionará si el usuario tiene permisos de superusuario.
    // Si la BD ya está configurada, fallará inofensivamente con 'CREATE EXTENSION IF NOT EXISTS'.
    try
      Query.SQL.Text := 'CREATE EXTENSION IF NOT EXISTS vector;';
      Query.ExecSQL;
    except
      on E: Exception do
        // Generalmente, no es fatal si la extensión ya existe o el usuario no puede crearla.
        // Si no se puede crear, la siguiente CREATE TABLE fallará.
        ;
    end;

    // --- 2. Crear la Tabla de Nodos ---
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE TABLE IF NOT EXISTS public.%s (', [NodesTableName]));
    Query.SQL.Add('    entidad character varying(20) COLLATE pg_catalog."default" NOT NULL,'); // Cambié char(20) a varying(20)
    Query.SQL.Add('    id text COLLATE pg_catalog."default" NOT NULL,');
    Query.SQL.Add('    node_label text COLLATE pg_catalog."default" NOT NULL,');
    Query.SQL.Add('    name text COLLATE pg_catalog."default",');
    Query.SQL.Add('    properties jsonb,');
    Query.SQL.Add(Format('    embedding %s,', [VectorType])); // Dimensión dinámica
    Query.SQL.Add(Format('    CONSTRAINT %s_pkey PRIMARY KEY (id, entidad)', [NodesTableName]));
    Query.SQL.Add(') TABLESPACE pg_default;');
    Query.ExecSQL;

    // --- 3. Crear Índices de Nodos ---
    // Índice HNSW (vectorial)
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE INDEX IF NOT EXISTS idx_%s_embedding_hnsw', [NodesTableName]));
    Query.SQL.Add(Format('    ON public.%s USING hnsw', [NodesTableName]));
    Query.SQL.Add('    (embedding vector_cosine_ops)');
    Query.SQL.Add('    TABLESPACE pg_default;');
    Query.ExecSQL;

    // Índice BTree (entidad y label)
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE INDEX IF NOT EXISTS idx_%s_entidad_label', [NodesTableName]));
    Query.SQL.Add(Format('    ON public.%s USING btree', [NodesTableName]));
    Query.SQL.Add('    (entidad COLLATE pg_catalog."default" ASC NULLS LAST, node_label COLLATE pg_catalog."default" ASC NULLS LAST)');
    Query.SQL.Add('    TABLESPACE pg_default;');
    Query.ExecSQL;

    // --- 4. Crear la Tabla de Aristas ---
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE TABLE IF NOT EXISTS public.%s (', [EdgesTableName]));
    Query.SQL.Add('    entidad character varying(20) COLLATE pg_catalog."default" NOT NULL,');
    Query.SQL.Add('    id text COLLATE pg_catalog."default" NOT NULL,');
    Query.SQL.Add('    edge_label text COLLATE pg_catalog."default" NOT NULL,');
    Query.SQL.Add('    name text COLLATE pg_catalog."default",');
    Query.SQL.Add('    source_node_id text COLLATE pg_catalog."default" NOT NULL,');
    Query.SQL.Add('    target_node_id text COLLATE pg_catalog."default" NOT NULL,');
    Query.SQL.Add('    weight double precision DEFAULT 1.0,');
    Query.SQL.Add('    properties jsonb,');
    Query.SQL.Add(Format('    embedding %s,', [VectorType])); // Dimensión dinámica
    Query.SQL.Add(Format('    CONSTRAINT %s_pkey PRIMARY KEY (id, entidad),', [EdgesTableName]));

    // Opcional: Añadir restricciones de clave externa para garantizar la integridad
    // Esto es muy recomendable si se usa una tabla externa.
    Query.SQL.Add(Format('    CONSTRAINT fk_source_node FOREIGN KEY (source_node_id, entidad) REFERENCES %s (id, entidad) ON DELETE CASCADE,', [NodesTableName]));
    Query.SQL.Add(Format('    CONSTRAINT fk_target_node FOREIGN KEY (target_node_id, entidad) REFERENCES %s (id, entidad) ON DELETE CASCADE', [NodesTableName]));

    Query.SQL.Add(') TABLESPACE pg_default;');
    Query.ExecSQL;

    // --- 5. Crear Índices de Aristas ---
    // Índice BTree (entidad y label)
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE INDEX IF NOT EXISTS idx_%s_entidad_label', [EdgesTableName]));
    Query.SQL.Add(Format('    ON public.%s USING btree', [EdgesTableName]));
    Query.SQL.Add('    (entidad COLLATE pg_catalog."default" ASC NULLS LAST, edge_label COLLATE pg_catalog."default" ASC NULLS LAST)');
    Query.SQL.Add('    TABLESPACE pg_default;');
    Query.ExecSQL;

    // Índice BTree (entidad y source)
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE INDEX IF NOT EXISTS idx_%s_entidad_source', [EdgesTableName]));
    Query.SQL.Add(Format('    ON public.%s USING btree', [EdgesTableName]));
    Query.SQL.Add('    (entidad COLLATE pg_catalog."default" ASC NULLS LAST, source_node_id COLLATE pg_catalog."default" ASC NULLS LAST)');
    Query.SQL.Add('    TABLESPACE pg_default;');
    Query.ExecSQL;

    // Índice BTree (entidad y target)
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE INDEX IF NOT EXISTS idx_%s_entidad_target', [EdgesTableName]));
    Query.SQL.Add(Format('    ON public.%s USING btree', [EdgesTableName]));
    Query.SQL.Add('    (entidad COLLATE pg_catalog."default" ASC NULLS LAST, target_node_id COLLATE pg_catalog."default" ASC NULLS LAST)');
    Query.SQL.Add('    TABLESPACE pg_default;');
    Query.ExecSQL;

    // 6. Configurar el driver para usar las nuevas tablas
    Self.TableName := ABaseTableName;
    Self.CurrentEntidad := 'DEFAULT'; // O la entidad inicial que decidas

  finally
    Query.Free;
  end;
end;

procedure TAiRagGraphPostgresDriver.DeleteEdge(const AEdgeID: string);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Text := 'DELETE FROM ' + FEdgesTableName + ' WHERE id = :id AND entidad = :entidad';
    Query.ParamByName('id').AsString := AEdgeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TAiRagGraphPostgresDriver.DeleteNode(const ANodeID: string);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Text := 'DELETE FROM ' + FNodesTableName + ' WHERE id = :id AND entidad = :entidad';

    Query.ParamByName('id').AsString := ANodeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TAiRagGraphPostgresDriver.NewQuery: TFDQuery;
begin
  if not Assigned(FConnection) then
    raise Exception.Create('La propiedad Connection del Driver no ha sido asignada.');
  Result := TFDQuery.Create(nil); // Se crea sin propietario para que se libere manualmente
  Result.Connection := FConnection;
end;

function TAiRagGraphPostgresDriver.FindEdgeDataByID(const AEdgeID: string; out AEdgeData: TEdgeDataRecord): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := NewQuery;
  try
    Query.SQL.Add('SELECT id, edge_label, name, source_node_id, target_node_id, ');
    Query.SQL.Add('  weight, properties, embedding ');
    Query.SQL.Add('FROM  ' + FEdgesTableName);
    Query.SQL.Add('WHERE id = :id AND entidad = :entidad');

    Query.ParamByName('id').AsString := AEdgeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;

    if not Query.IsEmpty then
    begin
      // Llenamos el record con los datos crudos de la BD.
      AEdgeData.ID := Query.FieldByName('id').AsString;
      AEdgeData.EdgeLabel := Query.FieldByName('edge_label').AsString;
      AEdgeData.Name := Query.FieldByName('name').AsString;
      AEdgeData.SourceNodeID := Query.FieldByName('source_node_id').AsString;
      AEdgeData.TargetNodeID := Query.FieldByName('target_node_id').AsString;
      AEdgeData.Weight := Query.FieldByName('weight').AsFloat;

      if not Query.FieldByName('properties').IsNull then
        AEdgeData.PropertiesJSON := Query.FieldByName('properties').AsString
      else
        AEdgeData.PropertiesJSON := '{}';

      if not Query.FieldByName('embedding').IsNull then
        AEdgeData.EmbeddingStr := Query.FieldByName('embedding').AsString
      else
        AEdgeData.EmbeddingStr := '[]';

      Result := True;
    end;
  finally
    Query.Free;
  end;
end;

function TAiRagGraphPostgresDriver.FindNodeByName(const AName, ANodeLabel: string): TAiRagGraphNode;
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Add('SELECT *');
    Query.SQL.Add('FROM ' + FNodesTableName);
    Query.SQL.Add('WHERE entidad = :entidad');
    Query.SQL.Add('  AND node_label = :node_label');
    Query.SQL.Add('  AND name = :name LIMIT 1');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_label').AsString := ANodeLabel;
    Query.ParamByName('name').AsString := AName;
    Query.Open;

    if not Query.Eof then
    begin
      Var
      NodeID := Query.FieldByName('id').AsString;
      Result := Graph.FindNodeByID(NodeID);
    end
    else
    begin
      Result := nil;
    end;
  finally
    Query.Free;
  end;
end;

function TAiRagGraphPostgresDriver.FindNodeDataByID(const ANodeID: string; out ANodeData: TNodeDataRecord): Boolean;
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Add('SELECT id, node_label, name, properties, embedding ');
    Query.SQL.Add('FROM  ' + FNodesTableName);
    Query.SQL.Add('WHERE id = :id');
    Query.SQL.Add('  AND entidad = :entidad');

    Query.ParamByName('id').AsString := ANodeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;

    if not Query.IsEmpty then
    begin
      // En lugar de crear un objeto, llenamos el record con los datos crudos.
      ANodeData.ID := Query.FieldByName('id').AsString;
      ANodeData.NodeLabel := Query.FieldByName('node_label').AsString;
      ANodeData.Name := Query.FieldByName('name').AsString;

      if not Query.FieldByName('properties').IsNull then
        ANodeData.PropertiesJSON := Query.FieldByName('properties').AsString
      else
        ANodeData.PropertiesJSON := '{}';

      if not Query.FieldByName('embedding').IsNull then
        ANodeData.EmbeddingStr := Query.FieldByName('embedding').AsString
      else
        ANodeData.EmbeddingStr := '[]';
      Result := True;
    end
    Else
      Result := False;

  finally
    Query.Free;
  end;

end;

function TAiRagGraphPostgresDriver.FindNodeNamesByLabel(const ANodeLabel, ASearchText: string; ALimit: Integer): TArray<string>;
var
  Query: TFDQuery;
  NameList: TStringList;
begin
  Query := NewQuery;
  NameList := TStringList.Create;
  try
    // La consulta es perfecta. Usa LIKE y es case-insensitive gracias a UPPER.
    Query.SQL.Add('SELECT name ');
    Query.SQL.Add('FROM ' + FNodesTableName);
    Query.SQL.Add('WHERE entidad = :entidad');
    Query.SQL.Add('  AND node_label = :node_label');
    Query.SQL.Add('  AND UPPER(name) LIKE :search_text');
    Query.SQL.Add('ORDER BY name LIMIT :limit');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_label').AsString := ANodeLabel;
    Query.ParamByName('search_text').AsString := '%' + ASearchText.ToUpper + '%';
    Query.ParamByName('limit').AsInteger := ALimit;

    Query.Open;

    while not Query.Eof do
    begin
      NameList.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    Result := NameList.ToStringArray;
  finally
    Query.Free;
    NameList.Free;
  end;
end;

function TAiRagGraphPostgresDriver.FindNodesByLabel(const ALabel: string): TArray<TAiRagGraphNode>;
var
  Query: TFDQuery;
  NodeIDs: TStringList;
  i: Integer;
begin

  Query := NewQuery;
  NodeIDs := TStringList.Create;
  try
    // 1. Obtener solo los IDs de los nodos que coinciden.
    Query.SQL.Add('SELECT id');
    Query.SQL.Add('FROM graph_nodes');
    Query.SQL.Add('WHERE entidad = :entidad');
    Query.SQL.Add('  AND node_label = :node_label');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_label').AsString := ALabel;
    Query.Open;

    while not Query.Eof do
    begin
      NodeIDs.Add(Query.FieldByName('id').AsString);
      Query.Next;
    end;

    // 2. Pedirle al grafo que "hidrate" o devuelva cada nodo desde su caché.
    SetLength(Result, NodeIDs.Count);
    for i := 0 to NodeIDs.Count - 1 do
    begin
      // FindNodeByID es ahora nuestro único punto de entrada para obtener nodos.
      // Se encargará de crear el objeto si no existe, o devolver el
      // existente si ya fue cargado.
      Result[i] := Graph.FindNodeByID(NodeIDs[i]);
    end;

  finally
    Query.Free;
    NodeIDs.Free;
  end;
end;

function TAiRagGraphPostgresDriver.FindNodesByProperty(const AKey: string; const AValue: Variant): TArray<TAiRagGraphNode>;
var
  Query: TFDQuery;
  NodeIDs: TStringList;
begin
  Query := NewQuery;
  NodeIDs := TStringList.Create;
  try
    if SameText(AKey, 'name') then
    begin
      Query.SQL.Text := 'SELECT * FROM ' + FNodesTableName + ' WHERE entidad = :entidad AND name = :value';
      Query.ParamByName('value').AsString := VarToStr(AValue);
    end
    else
    begin
      Query.SQL.Text := 'SELECT * FROM ' + FNodesTableName + ' WHERE entidad = :entidad AND properties ->> :key = :value';
      Query.ParamByName('key').AsString := AKey;
      Query.ParamByName('value').AsString := VarToStr(AValue);
    end;

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;

    while not Query.Eof do
    begin
      NodeIDs.Add(Query.FieldByName('id').AsString);
      Query.Next;
    end;

    SetLength(Result, NodeIDs.Count);
    for var i := 0 to NodeIDs.Count - 1 do
    begin
      Result[i] := Graph.FindNodeByID(NodeIDs[i]);
    end;

  finally
    Query.Free;
    NodeIDs.Free;
  end;
end;

procedure TAiRagGraphPostgresDriver.GetNodeEdges(ANode: TAiRagGraphNode);
var
  Query: TFDQuery;
  EdgeData: TEdgeDataRecord;
  EdgeID: string;
begin

  Query := NewQuery;
  try
    // 1. Traemos TODOS los datos de las aristas en UNA SOLA consulta.
    Query.SQL.Add('SELECT id, edge_label, name, source_node_id, target_node_id, ');
    Query.SQL.Add('   weight, properties, embedding ');
    Query.SQL.Add('FROM ' + FEdgesTableName);
    Query.SQL.Add('WHERE entidad = :entidad');
    Query.SQL.Add('  AND (source_node_id = :node_id');
    Query.SQL.Add('       OR target_node_id = :node_id)');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_id').AsString := ANode.ID;
    Query.Open;

    // 2. Iteramos sobre los resultados y le pedimos al grafo que hidrate cada arista.
    while not Query.Eof do
    begin
      EdgeID := Query.FieldByName('id').AsString;

      // --- ¡LÓGICA CORREGIDA Y LIMPIA! ---
      // Usamos la nueva función pública para comprobar si la arista ya existe en memoria.
      if not Graph.EdgeExistsInMemory(EdgeID) then
      begin
        // La arista no existe en memoria, así que la creamos e hidratamos.
        EdgeData.ID := EdgeID;
        EdgeData.EdgeLabel := Query.FieldByName('edge_label').AsString;
        EdgeData.Name := Query.FieldByName('name').AsString;
        EdgeData.SourceNodeID := Query.FieldByName('source_node_id').AsString;
        EdgeData.TargetNodeID := Query.FieldByName('target_node_id').AsString;
        EdgeData.Weight := Query.FieldByName('weight').AsFloat;

        if not Query.FieldByName('properties').IsNull then
          EdgeData.PropertiesJSON := Query.FieldByName('properties').AsString
        else
          EdgeData.PropertiesJSON := '{}';

        if not Query.FieldByName('embedding').IsNull then
          EdgeData.EmbeddingStr := Query.FieldByName('embedding').AsString
        else
          EdgeData.EmbeddingStr := '[]';

        // Usamos el método protegido del grafo para crear el objeto desde los datos.
        // Para llamarlo, SÍ necesitamos el class helper aquí.
        Graph.InternalHydrateEdge(EdgeData);
      end;

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TAiRagGraphPostgresDriver.GetUniqueEdgeLabels: TArray<string>;
var
  Query: TFDQuery;
  LabelList: TStringList;
begin
  Query := NewQuery;
  LabelList := TStringList.Create;
  try
    // 1. Definir la consulta SQL
    Query.SQL.Text := 'SELECT DISTINCT edge_label FROM ' + FEdgesTableName + ' WHERE entidad = :entidad ORDER BY edge_label';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;

    // 2. Ejecutar la consulta
    Query.Open;

    // 3. Recorrer los resultados
    while not Query.Eof do
    begin
      LabelList.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    // 4. Poblar el array de resultado
    Result := LabelList.ToStringArray;

  finally
    Query.Free;
    LabelList.Free;
  end;
end;

function TAiRagGraphPostgresDriver.GetUniqueNodeLabels: TArray<string>;
var
  Query: TFDQuery;
  LabelList: TStringList;
begin
  // Asumimos que la conexión a la base de datos ya está activa.
  // Si no es así, puedes añadir una comprobación aquí.

  Query := NewQuery; // Usando tu función helper
  LabelList := TStringList.Create;
  try
    // 1. Definir la consulta SQL
    Query.SQL.Text := 'SELECT DISTINCT node_label FROM ' + FNodesTableName + ' WHERE entidad = :entidad ORDER BY node_label';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;

    // 2. Ejecutar la consulta
    Query.Open;

    // 3. Recorrer los resultados y añadirlos a la lista
    while not Query.Eof do
    begin
      LabelList.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    // 4. Convertir la TStringList a un TArray<string> para el resultado
    Result := LabelList.ToStringArray;
  finally
    Query.Free;
    LabelList.Free;
  end;
end;


function TAiRagGraphPostgresDriver.PropertiesToJSONString(const AProperties: TDictionary<string, Variant>): string;
var
  JsonObj: TJSONObject;
  Pair: TPair<string, Variant>;
begin
  if (AProperties = nil) or (AProperties.Count = 0) then
    Exit('{}');

  // La lógica es muy similar a la que acabamos de poner en el Core.
  // Podríamos incluso reutilizarla si tuviéramos una unidad de helpers común.
  JsonObj := TJSONObject.Create;
  try
    for Pair in AProperties do
    begin
      // Para no depender de VariantToJSONValue (que está en el Core),
      // podemos replicar su lógica simple aquí.
      case VarType(Pair.Value) of
        varBoolean:
          JsonObj.AddPair(Pair.Key, TJSONBool.Create(Boolean(Pair.Value)));
        varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord, varInt64, varUInt64, varSingle, varDouble, varCurrency:
          JsonObj.AddPair(Pair.Key, TJSONNumber.Create(Extended(Pair.Value)));
      else // varString y otros
        JsonObj.AddPair(Pair.Key, TJSONString.Create(VarToStr(Pair.Value)));
      end;
    end;
    Result := JsonObj.ToString;
  finally
    JsonObj.Free;
  end;
end;

function TAiRagGraphPostgresDriver.Query(const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>;
var
  Query: TFDQuery;
  Step: TQueryStep;
  QueryEmbedding: TAiEmbeddingData;
  EmbeddingStr: String;
  NodeIDs: TStringList;
  DirectionChar: string; // Para manejar la dirección de la arista
begin


  // --------------------------------------------------------------------------
  // TODO: Soporte para Query Multipaso.
  // La lógica actual solo maneja UN paso estructural después del anclaje semántico.
  // La implementación futura requerirá:
  // 1. Un bucle sobre APlan.Steps.
  // 2. Generación de SQL con múltiples CTEs o una CTE recursiva compleja.
  // --------------------------------------------------------------------------

  SetLength(Result, 0); // Corregido: Usar Result en lugar de ResultNodes

  if Graph = nil then Exit;

  // El driver en esta versión simplificada solo maneja un paso estructural
  if Length(APlan.Steps) <> 1 then
  begin
    // Si no es un plan de un solo paso, devolvemos un array vacío o podríamos
    // delegar a la lógica en memoria si queremos fallar suavemente.
    // Pero en el driver, el contrato es manejar la BD o fallar.
    Exit;
  end;

  Query := NewQuery;
  NodeIDs := TStringList.Create;
  Step := APlan.Steps[0];

  try
    // Paso 1: Obtener embedding
    if not Assigned(Graph.Embeddings) then
      raise Exception.Create('El motor de Embeddings no está asignado al grafo.');

    QueryEmbedding := Graph.Embeddings.CreateEmbedding(APlan.AnchorPrompt, 'user');
    EmbeddingStr := EmbeddingToString(QueryEmbedding);

    // Determinar la dirección de la búsqueda para el JOIN
    if Step.IsReversed then
      DirectionChar := 'INCOMING'
    else
      DirectionChar := 'OUTGOING';

    // -------------------------------------------------------------------------
    // Paso 2: Construir y ejecutar la consulta SQL Híbrida
    // -------------------------------------------------------------------------
    Query.SQL.Clear;

    // CTE 1: AnchorNodes (Búsqueda Semántica)
    Query.SQL.Add('WITH AnchorNodes AS (');
    Query.SQL.Add('  SELECT id');
    Query.SQL.Add('  FROM ' + FNodesTableName); // Usar nombre de tabla dinámico
    Query.SQL.Add('  WHERE entidad = :entidad');
    // Usamos el literal del embedding para el operador vectorial
    Query.SQL.Add('    AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold');
    Query.SQL.Add('  ORDER BY embedding <-> ''' + EmbeddingStr + '''');
    Query.SQL.Add('  LIMIT :limit');
    Query.SQL.Add(')');

    // Consulta final: Obtener los IDs de los nodos de destino (JOIN estructural)
    Query.SQL.Add('SELECT final_node.id ');
    Query.SQL.Add('FROM AnchorNodes an ');

    // JOIN a la tabla de aristas, condicionando por la etiqueta de la arista
    Query.SQL.Add('JOIN ' + FEdgesTableName + ' e ON e.entidad = :entidad AND e.edge_label = :edge_label ');

    // Condición de Dirección (Source vs Target)
    if Step.IsReversed then
    begin
      // Búsqueda inversa: Ancla es el nodo DESTINO, Resultado es el nodo ORIGEN
      Query.SQL.Add('  AND an.id = e.target_node_id ');
      Query.SQL.Add('JOIN ' + FNodesTableName + ' final_node ON e.source_node_id = final_node.id AND final_node.entidad = :entidad ');
    end
    else
    begin
      // Búsqueda directa: Ancla es el nodo ORIGEN, Resultado es el nodo DESTINO
      Query.SQL.Add('  AND an.id = e.source_node_id ');
      Query.SQL.Add('JOIN ' + FNodesTableName + ' final_node ON e.target_node_id = final_node.id AND final_node.entidad = :entidad ');
    end;

    // Filtro de etiqueta del nodo de destino (si se especificó)
    if not Step.TargetNodeLabel.IsEmpty then
    begin
      Query.SQL.Add('WHERE final_node.node_label = :target_label');
      Query.ParamByName('target_label').AsString := Step.TargetNodeLabel;
    end
    else
    begin
      // Aseguramos que el parámetro :target_label sea ignorado si no se usa
      Query.ParamByName('target_label').AsString := '';
    end;


    // Asignar parámetros comunes
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    // La distancia es 1 - Similaridad, ya que el operador <-> devuelve distancia (0 = perfecto match)
    Query.ParamByName('distance_threshold').AsFloat := 1 - APrecision;
    Query.ParamByName('limit').AsInteger := ALimit;
    Query.ParamByName('edge_label').AsString := Step.EdgeLabel;

    Query.Open;

    while not Query.Eof do
    begin
      NodeIDs.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    // -------------------------------------------------------------------------
    // Paso 3: Hidratación delegada y construcción del resultado
    // -------------------------------------------------------------------------

    // El resultado final es la lista de nodos obtenidos en el paso 2.
    SetLength(Result, NodeIDs.Count);
    for var i := 0 to NodeIDs.Count - 1 do
    begin
      // FindNodeByID se asegura de que el nodo exista en memoria,
      // cargándolo si es necesario.
      Result[i] := Graph.FindNodeByID(NodeIDs[i]);
    end;

    // Si ADepth > 0 (aunque actualmente no se usa para Query, está en el contrato),
    // se podría forzar la carga de vecindario como en SearchNodes.
    // Aquí no lo incluimos para mantener la Query simple (sin expansión).

  finally
    Query.Free;
    NodeIDs.Free;
  end;
end;

Function TAiRagGraphPostgresDriver.SearchNodes(const APrompt: string; ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>;
var
  Query: TFDQuery;
  QueryEmbedding: TAiEmbeddingData;
  EmbeddingStr: string;
  NodeIDs: TStringList;
  InitialNodeIDs: TStringList; // Para guardar los nodos semilla
  Node: TAiRagGraphNode;
  InitialQuery: TFDQuery; // Query separada para obtener los nodos ancla
  InitialIDsList: TStringList; // Lista para formatear los IDs para la CTE
  AnchorIDs: string;
begin
  SetLength(Result, 0); // Inicializar Result

  if Graph = nil then Exit;

  // Paso 1: Obtener el embedding
  if not Assigned(Graph.Embeddings) then
    raise Exception.Create('El motor de Embeddings no está asignado al grafo.');

  QueryEmbedding := Graph.Embeddings.CreateEmbedding(APrompt, 'user');
  EmbeddingStr := EmbeddingToString(QueryEmbedding);

  // Inicialización de recursos
  Query := NewQuery;
  InitialQuery := NewQuery;
  NodeIDs := TStringList.Create;
  InitialNodeIDs := TStringList.Create;
  InitialIDsList := TStringList.Create;

  try

    // -----------------------------------------------------------------
    // FASE A: Obtener Nodos Ancla Semánticos (InitialNodeIDs)
    // -----------------------------------------------------------------
    InitialQuery.SQL.Clear;
    InitialQuery.SQL.Add('SELECT id');
    InitialQuery.SQL.Add('FROM ' + FNodesTableName);
    InitialQuery.SQL.Add('WHERE entidad = :entidad');
    InitialQuery.SQL.Add('  AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold');
    InitialQuery.SQL.Add('ORDER BY embedding <-> ''' + EmbeddingStr + '''');
    InitialQuery.SQL.Add('LIMIT :limit');

    InitialQuery.ParamByName('entidad').AsString := FCurrentEntidad;
    InitialQuery.ParamByName('distance_threshold').AsFloat := 1 - APrecision;
    InitialQuery.ParamByName('limit').AsInteger := ALimit;
    InitialQuery.Open;

    while not InitialQuery.Eof do
    begin
      InitialNodeIDs.Add(InitialQuery.Fields[0].AsString);
      InitialQuery.Next;
    end;

    if InitialNodeIDs.Count = 0 then
      Exit; // No se encontraron anclas, salimos.


    // -----------------------------------------------------------------
    // FASE B: Ejecutar la consulta de Grafo (Expansión o simple)
    // -----------------------------------------------------------------
    if ADepth = 0 then
    begin
      // CASO SIMPLE: Solo usamos los IDs ancla ya encontrados
      NodeIDs.Assign(InitialNodeIDs);
    end
    else
    begin
      // CASO COMPLEJO: Expansión (CTE)

      // 1. Formatear la lista de IDs para inyectar en la CTE: ('ID1', 'ID2', 'ID3')
      for var I := 0 to InitialNodeIDs.Count - 1 do
          InitialIDsList.Add(QuotedStr(InitialNodeIDs[I]));

      AnchorIDs := InitialIDsList.CommaText;

      Query.SQL.Clear;
      Query.SQL.Add('WITH RECURSIVE traversal AS (');

      // La consulta inicial de la CTE usa los IDs ancla ya encontrados
      Query.SQL.Add('  SELECT id, 1 AS depth, ARRAY[id] AS path');
      Query.SQL.Add('  FROM ' + FNodesTableName);
      Query.SQL.Add('  WHERE entidad = :entidad');
      Query.SQL.Add('    AND id IN (' + AnchorIDs + ')'); // Usamos los IDs ya encontrados

      Query.SQL.Add('  UNION ALL');

      // La parte recursiva: encuentra vecinos en aristas (source o target)
      Query.SQL.Add('  SELECT');
      Query.SQL.Add('    CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END AS id,');
      Query.SQL.Add('    t.depth + 1,');
      Query.SQL.Add('    t.path || CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END');
      Query.SQL.Add('  FROM traversal t');
      Query.SQL.Add('  JOIN ' + FEdgesTableName + ' e');
      Query.SQL.Add('    ON (e.source_node_id = t.id OR e.target_node_id = t.id)');
      Query.SQL.Add('    AND e.entidad = :entidad');
      Query.SQL.Add('  WHERE t.depth <= :depth'); // Límite de profundidad
      Query.SQL.Add('    AND NOT (CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END = ANY(t.path))');
      Query.SQL.Add(')');

      // Consulta final: IDs únicos de todos los nodos en el subgrafo expandido
      Query.SQL.Add('SELECT DISTINCT id FROM traversal');

      Query.ParamByName('entidad').AsString := FCurrentEntidad;
      Query.ParamByName('depth').AsInteger := ADepth;
      Query.Open;

      // Recolectar todos los IDs del subgrafo expandido
      while not Query.Eof do
      begin
        NodeIDs.Add(Query.Fields[0].AsString);
        Query.Next;
      end;
    end; // Fin del if ADepth > 0


    // -----------------------------------------------------------------
    // FASE C: Hidratación (Carga de objetos en memoria)
    // -----------------------------------------------------------------

    // Cargar todos los nodos relevantes (ancla + expandidos)
    for var NodeID in NodeIDs do
    begin
      Graph.FindNodeByID(NodeID);
    end;

    // Forzar la carga de aristas para el subgrafo expandido.
    if ADepth > 0 then
    begin
      for var NodeID in NodeIDs do
      begin
        Node := Graph.FindNodeByID(NodeID);
        if Node <> nil then
          Node.EnsureEdgesAreLoaded;
      end;
    end;

    // -----------------------------------------------------------------
    // FASE D: Construir el resultado final (solo los nodos ancla)
    // -----------------------------------------------------------------

    SetLength(Result, InitialNodeIDs.Count);

    for var i := 0 to InitialNodeIDs.Count - 1 do
    begin
      Result[i] := Graph.FindNodeByID(InitialNodeIDs[i]);
    end;

  finally
    Query.Free;
    InitialQuery.Free;
    NodeIDs.Free;
    InitialNodeIDs.Free;
    InitialIDsList.Free;
  end;
end;

procedure TAiRagGraphPostgresDriver.SetTableName(const Value: String);
begin
  FTableName := Value;
  FEdgesTableName := FTableName + 'edges';
  FNodesTableName := FTableName + 'nodes';
end;


procedure TAiRagGraphPostgresDriver.AddEdge(AEdge: TAiRagGraphEdge);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Clear;

    if Length(AEdge.Data) > 0 then
    Begin
      Query.SQL.Add('INSERT INTO ' + FEdgesTableName + ' (entidad, id, edge_label, name, source_node_id, target_node_id, weight, properties, embedding) ');
      Query.SQL.Add('VALUES (:entidad, :id, :edge_label, :name, :source_node_id, :target_node_id, :weight, :properties::JsonB, ''' + EmbeddingToString(AEdge.Data) + ''')');
    End
    Else
    Begin
      Query.SQL.Add('INSERT INTO ' + FEdgesTableName + ' (entidad, id, edge_label, name, source_node_id, target_node_id, weight, properties) ');
      Query.SQL.Add('VALUES (:entidad, :id, :edge_label, :name, :source_node_id, :target_node_id, :weight, :properties::JsonB)');
    End;

    // Asignar valores a los parámetros
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('id').AsString := AEdge.ID;
    Query.ParamByName('edge_label').AsString := AEdge.EdgeLabel;
    Query.ParamByName('name').AsString := AEdge.Name;
    Query.ParamByName('source_node_id').AsString := AEdge.FromNode.ID;
    Query.ParamByName('target_node_id').AsString := AEdge.ToNode.ID;
    Query.ParamByName('weight').AsFloat := AEdge.Weight;
    Query.ParamByName('properties').AsString := PropertiesToJSONString(AEdge.Properties);

    Query.ExecSQL;

  finally
    Query.Free;
  end;
end;

procedure TAiRagGraphPostgresDriver.AddNode(ANode: TAiRagGraphNode);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    if Length(ANode.Data) > 0 then
    Begin
      Query.SQL.Add('INSERT INTO ' + FNodesTableName + ' (entidad, id, node_label, name, properties, embedding) ');
      Query.SQL.Add('VALUES (:entidad, :id, :node_label, :name, :properties::jsonb, ''' + EmbeddingToString(ANode.Data) + ''')');
    End
    Else
    Begin
      Query.SQL.Add('INSERT INTO ' + FNodesTableName + ' (entidad, id, node_label, name, properties) ');
      Query.SQL.Add('VALUES (:entidad, :id, :node_label, :name, :properties::jsonb)');
    End;

    // Asignar valores a los parámetros
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('id').AsString := ANode.ID;
    Query.ParamByName('node_label').AsString := ANode.NodeLabel;
    Query.ParamByName('name').AsString := ANode.Name;
    Query.ParamByName('properties').AsString := PropertiesToJSONString(ANode.Properties);

    Query.ExecSQL;
  finally
    Query.Free;
  end;

end;

end.
