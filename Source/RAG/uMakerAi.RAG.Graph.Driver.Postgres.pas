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
  System.SysUtils, System.Classes, System.Generics.Collections, System.StrUtils,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, uMakerAi.RAG.Vectors, uMakerAi.Embeddings.core,
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
    function SearchNodes(const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; AFilter: TAiEmbeddingMetaData = nil): TArray<TAiRagGraphNode>; Override;

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

{ procedure TAiRagGraphPostgresDriver.CreateSchema(const ABaseTableName: string; AVectorDim: Integer);
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
}

procedure TAiRagGraphPostgresDriver.CreateSchema(const ABaseTableName: string; AVectorDim: Integer);
var
  Query: TFDQuery;
  NodesTableName, EdgesTableName: string;
  VectorType: string;
begin
  if not Assigned(FConnection) or not FConnection.Connected then
    raise Exception.Create('La conexión a la base de datos no está activa.');

  // Configuración de nombres de tablas
  NodesTableName := ABaseTableName + 'nodes';
  EdgesTableName := ABaseTableName + 'edges';
  VectorType := Format('vector(%d)', [AVectorDim]);

  Query := NewQuery;
  try
    // 1. EXTENSIÓN PGVECTOR
    // Necesaria para el soporte de vectores y el índice HNSW
    try
      Query.SQL.Text := 'CREATE EXTENSION IF NOT EXISTS vector;';
      Query.ExecSQL;
    except
      on E: Exception do; // Ignorar si ya existe o no hay permisos de superusuario
    end;

    // 2. TABLA DE NODOS (Entidades)
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE TABLE IF NOT EXISTS public.%s (', [NodesTableName]));
    Query.SQL.Add('    entidad character varying(20) NOT NULL,');
    Query.SQL.Add('    id text NOT NULL,');
    Query.SQL.Add('    node_label text NOT NULL,');
    Query.SQL.Add('    name text,');
    Query.SQL.Add('    node_text text,'); // <--- Campo vital para el contexto RAG
    Query.SQL.Add('    properties jsonb,');
    Query.SQL.Add(Format('    embedding %s,', [VectorType]));

    // --- MOTOR DE BÚSQUEDA HÍBRIDA (TSVECTOR) ---
    // Indexamos Nombre (A), Etiqueta (B) y el Texto descriptivo (C)
    Query.SQL.Add('    search_vector tsvector GENERATED ALWAYS AS (');
    Query.SQL.Add('       setweight(to_tsvector(''spanish'', coalesce(name, '''')), ''A'') || ');
    Query.SQL.Add('       setweight(to_tsvector(''spanish'', coalesce(node_label, '''')), ''B'') || ');
    Query.SQL.Add('       setweight(to_tsvector(''spanish'', coalesce(node_text, '''')), ''C'')) STORED,');

    Query.SQL.Add(Format('    CONSTRAINT %s_pkey PRIMARY KEY (id, entidad)', [NodesTableName]));
    Query.SQL.Add(');');
    Query.ExecSQL;

    // 3. ÍNDICES DE NODOS
    // Índice Vectorial HNSW para búsqueda semántica (Coseno)
    Query.SQL.Text := Format('CREATE INDEX IF NOT EXISTS idx_%s_embedding_hnsw ON public.%s USING hnsw (embedding vector_cosine_ops);', [NodesTableName, NodesTableName]);
    Query.ExecSQL;

    // Índice GIN para búsqueda léxica (Palabras clave)
    Query.SQL.Text := Format('CREATE INDEX IF NOT EXISTS idx_%s_fts ON public.%s USING gin(search_vector);', [NodesTableName, NodesTableName]);
    Query.ExecSQL;

    // Índice GIN para metadatos JSONB (Filtros rápidos)
    Query.SQL.Text := Format('CREATE INDEX IF NOT EXISTS idx_%s_props ON public.%s USING gin(properties);', [NodesTableName, NodesTableName]);
    Query.ExecSQL;

    // Índice BTree para búsquedas exactas por nombre
    Query.SQL.Text := Format('CREATE INDEX IF NOT EXISTS idx_%s_name ON public.%s (name);', [NodesTableName, NodesTableName]);
    Query.ExecSQL;

    // 4. TABLA DE ARISTAS (Relaciones)
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE TABLE IF NOT EXISTS public.%s (', [EdgesTableName]));
    Query.SQL.Add('    entidad character varying(20) NOT NULL,');
    Query.SQL.Add('    id text NOT NULL,');
    Query.SQL.Add('    edge_label text NOT NULL,');
    Query.SQL.Add('    name text,');
    Query.SQL.Add('    node_text text,'); // <--- Contexto descriptivo de la relación
    Query.SQL.Add('    source_node_id text NOT NULL,');
    Query.SQL.Add('    target_node_id text NOT NULL,');
    Query.SQL.Add('    weight double precision DEFAULT 1.0,');
    Query.SQL.Add('    properties jsonb,');
    Query.SQL.Add(Format('    embedding %s,', [VectorType]));

    // Primary Key y Foreign Keys
    Query.SQL.Add(Format('    CONSTRAINT %s_pkey PRIMARY KEY (id, entidad),', [EdgesTableName]));
    Query.SQL.Add(Format('    CONSTRAINT fk_source_%s FOREIGN KEY (source_node_id, entidad) REFERENCES %s (id, entidad) ON DELETE CASCADE,', [EdgesTableName, NodesTableName]));
    Query.SQL.Add(Format('    CONSTRAINT fk_target_%s FOREIGN KEY (target_node_id, entidad) REFERENCES %s (id, entidad) ON DELETE CASCADE', [EdgesTableName, NodesTableName]));
    Query.SQL.Add(');');
    Query.ExecSQL;

    // 5. ÍNDICES DE ARISTAS
    // Índice para filtrar por tipo de relación
    Query.SQL.Text := Format('CREATE INDEX IF NOT EXISTS idx_%s_edge_label ON public.%s (entidad, edge_label);', [EdgesTableName, EdgesTableName]);
    Query.ExecSQL;

    // Índices fundamentales para navegación de grafos (JOINs rápidos)
    Query.SQL.Text := Format('CREATE INDEX IF NOT EXISTS idx_%s_source_id ON public.%s (source_node_id, entidad);', [EdgesTableName, EdgesTableName]);
    Query.ExecSQL;
    Query.SQL.Text := Format('CREATE INDEX IF NOT EXISTS idx_%s_target_id ON public.%s (target_node_id, entidad);', [EdgesTableName, EdgesTableName]);
    Query.ExecSQL;

    // 6. Sincronización del Driver
    Self.TableName := ABaseTableName;

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
      AEdgeData.NodeText := Query.FieldByName('node_text').AsString;

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
  Result := False;
  Query := NewQuery;
  try
    Query.SQL.Add('SELECT id, node_label, name, node_text, properties, embedding ');
    Query.SQL.Add('FROM ' + FNodesTableName);
    Query.SQL.Add('WHERE id = :id AND entidad = :entidad');
    Query.ParamByName('id').AsString := ANodeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;
    if not Query.IsEmpty then
    begin
      ANodeData.ID := Query.FieldByName('id').AsString;
      ANodeData.NodeLabel := Query.FieldByName('node_label').AsString;
      ANodeData.Name := Query.FieldByName('name').AsString;
      ANodeData.NodeText := Query.FieldByName('node_text').AsString;

      if not Query.FieldByName('properties').IsNull then
        ANodeData.PropertiesJSON := Query.FieldByName('properties').AsString
      else
        ANodeData.PropertiesJSON := '{}';

      // ✓ Mantener el manejo de NULL del embedding
      if not Query.FieldByName('embedding').IsNull then
        ANodeData.EmbeddingStr := Query.FieldByName('embedding').AsString
      else
        ANodeData.EmbeddingStr := '[]';

      Result := True;
    end;
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
    // Si buscamos por nombre, usamos ILIKE para que sea insensible a mayúsculas
    // Esto ayuda mucho con el nuevo Parser
    if SameText(AKey, 'name') then
    begin
      Query.SQL.Text := 'SELECT id FROM ' + FNodesTableName +
                        ' WHERE entidad = :entidad AND name ILIKE :value';
      Query.ParamByName('value').AsString := VarToStr(AValue);
    end
    else
    begin
      // Búsqueda en JSONB. Nota: properties ->> :key es case-sensitive en Postgres
      Query.SQL.Text := 'SELECT id FROM ' + FNodesTableName +
                        ' WHERE entidad = :entidad AND properties ->> :key = :value';
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
      Result[i] := Graph.FindNodeByID(NodeIDs[i]);

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
  // Evitamos carga innecesaria si la conexión no está lista
  if (ANode = nil) or (FConnection = nil) then Exit;

  Query := NewQuery;
  try
    // 1. SELECT METICULOSO: Traemos todos los campos necesarios para reconstruir la arista
    Query.SQL.Add('SELECT id, edge_label, name, source_node_id, target_node_id, ');
    Query.SQL.Add('       weight, properties, embedding ');
    Query.SQL.Add('FROM ' + FEdgesTableName);
    Query.SQL.Add('WHERE entidad = :entidad');
    // Buscamos donde el nodo sea ORIGEN o DESTINO (Carga bidireccional)
    Query.SQL.Add('  AND (source_node_id = :node_id OR target_node_id = :node_id)');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_id').AsString := ANode.ID;
    Query.Open;

    while not Query.Eof do
    begin
      EdgeID := Query.FieldByName('id').AsString;

      // 2. IDENTITY MAP: Verificamos si la arista ya existe en la memoria de Delphi
      // Esto evita crear objetos duplicados si ya cargamos esta arista desde el otro nodo extremo
      if not Graph.EdgeExistsInMemory(EdgeID) then
      begin
        // Llenamos el Record intermedio (Mantenemos la estructura del Core)
        EdgeData.ID := EdgeID;
        EdgeData.EdgeLabel := Query.FieldByName('edge_label').AsString;
        EdgeData.Name := Query.FieldByName('name').AsString;
        EdgeData.SourceNodeID := Query.FieldByName('source_node_id').AsString;
        EdgeData.TargetNodeID := Query.FieldByName('target_node_id').AsString;
        EdgeData.Weight := Query.FieldByName('weight').AsFloat;

        // Manejo de nulos en JSONB y Embedding
        if not Query.FieldByName('properties').IsNull then
          EdgeData.PropertiesJSON := Query.FieldByName('properties').AsString
        else
          EdgeData.PropertiesJSON := '{}';

        if not Query.FieldByName('embedding').IsNull then
          EdgeData.EmbeddingStr := Query.FieldByName('embedding').AsString
        else
          EdgeData.EmbeddingStr := '[]';

        // 3. HIDRATACIÓN: El grafo crea el objeto y conecta los nodos
        // InternalHydrateEdge llamará a FindNodeByID para los extremos.
        // Si el nodo vecino no está en RAM, se cargará automáticamente de la BD.
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

function TAiRagGraphPostgresDriver.Query(const APlan: TQueryPlan;
  ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>;
var
  Query: TFDQuery;
  SQL: TStringBuilder;
  Step: TQueryStep;
  QueryEmbedding: TAiEmbeddingData;
  EmbeddingStr: String;
  NodeIDs: TStringList;
  i: Integer;
  CurrentStepVar, PrevStepVar: string;
begin
  SetLength(Result, 0);

  if (Graph = nil) or (APlan.AnchorPrompt.IsEmpty) then
    Exit;

  // Validar que hay pasos definidos
  if Length(APlan.Steps) = 0 then
    Exit;

  Query := NewQuery;
  SQL := TStringBuilder.Create;
  NodeIDs := TStringList.Create;
  try
    // -----------------------------------------------------------------------
    // PASO 1: Obtener embedding del ancla
    // -----------------------------------------------------------------------
    if not Assigned(Graph.Embeddings) then
      raise Exception.Create('El motor de Embeddings no está asignado al grafo.');

    QueryEmbedding := Graph.Embeddings.CreateEmbedding(APlan.AnchorPrompt, 'user');
    EmbeddingStr := EmbeddingToString(QueryEmbedding);

    // -----------------------------------------------------------------------
    // PASO 2: Construir SQL con CTEs recursivos
    // -----------------------------------------------------------------------
    SQL.AppendLine('WITH RECURSIVE ');

    // CTE de Anclaje: Búsqueda Semántica Inicial
    SQL.AppendLine(Format('  %s AS (', [APlan.AnchorVariable]));
    SQL.AppendLine('    SELECT id FROM ' + FNodesTableName);
    SQL.AppendLine('    WHERE entidad = :entidad');

    // USAR OPERADOR DE COSENO (mejor para embeddings de texto)
    SQL.AppendLine('      AND (1 - (embedding <=> ''' + EmbeddingStr + ''')) >= :precision');
    SQL.AppendLine('    ORDER BY embedding <=> ''' + EmbeddingStr + ''' ASC');
    SQL.AppendLine('    LIMIT :limit');
    SQL.AppendLine('  )');

    // CTEs de Navegación Estructural (Multi-Hop)
    for i := 0 to High(APlan.Steps) do
    begin
      Step := APlan.Steps[i];
      PrevStepVar := Step.SourceVariable;
      CurrentStepVar := Step.TargetVariable;

      SQL.AppendLine(Format('  , %s AS (', [CurrentStepVar]));
      SQL.AppendLine('    SELECT DISTINCT ');

      // Dirección de la arista
      if Step.IsReversed then
        SQL.AppendLine('      e.source_node_id as id')
      else
        SQL.AppendLine('      e.target_node_id as id');

      SQL.AppendLine('    FROM ' + FEdgesTableName + ' e');
      SQL.AppendLine(Format('    JOIN %s prev ON ', [PrevStepVar]));

      if Step.IsReversed then
        SQL.AppendLine('      e.target_node_id = prev.id')
      else
        SQL.AppendLine('      e.source_node_id = prev.id');

      SQL.AppendLine('    WHERE e.entidad = :entidad');
      SQL.AppendLine(Format('      AND e.edge_label = :step_label_%d', [i]));

      // Filtro opcional por tipo de nodo destino
      if not Step.TargetNodeLabel.IsEmpty then
      begin
        SQL.AppendLine('      AND EXISTS (');
        SQL.AppendLine('        SELECT 1 FROM ' + FNodesTableName + ' fn');
        SQL.AppendLine('        WHERE fn.id = ' +
          IfThen(Step.IsReversed, 'e.source_node_id', 'e.target_node_id'));
        SQL.AppendLine(Format('          AND fn.node_label = :target_label_%d', [i]));
        SQL.AppendLine('          AND fn.entidad = :entidad');
        SQL.AppendLine('      )');
      end;

      SQL.AppendLine('  )');
    end;

    // Selección final
    SQL.AppendLine(Format('SELECT DISTINCT id FROM %s', [APlan.ResultVariable]));

    // -----------------------------------------------------------------------
    // PASO 3: Asignar parámetros y ejecutar
    // -----------------------------------------------------------------------
    Query.SQL.Text := SQL.ToString;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('precision').AsFloat := APrecision;
    Query.ParamByName('limit').AsInteger := ALimit;

    // Parámetros dinámicos de los pasos
    for i := 0 to High(APlan.Steps) do
    begin
      Query.ParamByName(Format('step_label_%d', [i])).AsString :=
        APlan.Steps[i].EdgeLabel;

      // SOLO asignar si el parámetro existe en el SQL
      if not APlan.Steps[i].TargetNodeLabel.IsEmpty then
        Query.ParamByName(Format('target_label_%d', [i])).AsString :=
          APlan.Steps[i].TargetNodeLabel;
    end;

    Query.Open;

    // -----------------------------------------------------------------------
    // PASO 4: Recolectar IDs
    // -----------------------------------------------------------------------
    while not Query.Eof do
    begin
      NodeIDs.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    // -----------------------------------------------------------------------
    // PASO 5: Hidratación con soporte para ADepth
    // -----------------------------------------------------------------------
    SetLength(Result, NodeIDs.Count);
    for i := 0 to NodeIDs.Count - 1 do
    begin
      Result[i] := Graph.FindNodeByID(NodeIDs[i]);

      // Pre-cargar aristas si se requiere contexto
      if (Result[i] <> nil) and (ADepth > 0) then
        Result[i].EnsureEdgesAreLoaded;
    end;

  finally
    SQL.Free;
    Query.Free;
    NodeIDs.Free;
  end;
end;


function TAiRagGraphPostgresDriver.SearchNodes(
  const APrompt: string;
  ADepth, ALimit: Integer;
  APrecision: Double;
  AFilter: TAiEmbeddingMetaData = nil
): TArray<TAiRagGraphNode>;
var
  Query: TFDQuery;
  EmbeddingStr: string;
  EmbeddingLiteral: string;
  AnchorIDs: TStringList;
  AnchorIDsList: TStringList;
  SQL: TStringBuilder;
  i: Integer;
begin
  SetLength(Result, 0);

  if (Graph = nil) or APrompt.IsEmpty then
    Exit;

  // -------------------------------------------------------------
  // 1. Obtener embedding del prompt
  // -------------------------------------------------------------
  if not Assigned(Graph.Embeddings) then
    raise Exception.Create('El motor de Embeddings no está asignado al grafo.');

  EmbeddingStr := EmbeddingToString(
    Graph.Embeddings.CreateEmbedding(APrompt, 'user')
  );
  EmbeddingLiteral := '''' + EmbeddingStr + '''';

  Query := NewQuery;
  AnchorIDs := TStringList.Create;
  SQL := TStringBuilder.Create;
  try
    // -------------------------------------------------------------
    // FASE A: BÚSQUEDA HÍBRIDA DE NODOS ANCLA
    // -------------------------------------------------------------
    SQL.AppendLine('WITH HybridScores AS (');
    SQL.AppendLine('  SELECT id, ');
    SQL.AppendLine('    (1 - (embedding <=> ' + EmbeddingLiteral + ')) AS semantic_score,');
    SQL.AppendLine('    ts_rank_cd(search_vector, websearch_to_tsquery(''spanish'', :prompt)) AS lexical_score');
    SQL.AppendLine('  FROM ' + FNodesTableName);
    SQL.AppendLine('  WHERE entidad = :entidad');

    // -------------------------------------------------------------
    // Filtros por metadata (JSONB)
    // -------------------------------------------------------------
    if Assigned(AFilter) and (AFilter.Data.Count > 0) then
    begin
      for i := 0 to AFilter.Data.Count - 1 do
        SQL.AppendLine(
          Format(
            '    AND properties ->> %s = %s',
            [
              QuotedStr(AFilter.Data.Names[i]),
              QuotedStr(AFilter.Data.ValueFromIndex[i])
            ]
          )
        );
    end;

    // Umbral híbrido
    SQL.AppendLine('  AND ( (1 - (embedding <=> ' + EmbeddingLiteral + ')) >= :precision');
    SQL.AppendLine('        OR search_vector @@ websearch_to_tsquery(''spanish'', :prompt) )');
    SQL.AppendLine(')');

    // Ranking híbrido ponderado
    SQL.AppendLine('SELECT id FROM HybridScores');
    SQL.AppendLine(
      'ORDER BY (semantic_score * 0.7 + (1 - 1/(1 + lexical_score)) * 0.3) DESC'
    );
    SQL.AppendLine('LIMIT :limit');

    Query.SQL.Text := SQL.ToString;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('prompt').AsString := APrompt;
    Query.ParamByName('precision').AsFloat := APrecision;
    Query.ParamByName('limit').AsInteger := ALimit;
    Query.Open;

    while not Query.Eof do
    begin
      AnchorIDs.Add(Query.FieldByName('id').AsString);
      Query.Next;
    end;

    if AnchorIDs.Count = 0 then
      Exit;

    // -------------------------------------------------------------
    // FASE B: EXPANSIÓN DEL GRAFO (CTE RECURSIVA)
    // -------------------------------------------------------------
    if ADepth > 0 then
    begin
      AnchorIDsList := TStringList.Create;
      try
        for i := 0 to AnchorIDs.Count - 1 do
          AnchorIDsList.Add(QuotedStr(AnchorIDs[i]));

        Query.Close;
        Query.SQL.Clear;

        Query.SQL.Add('WITH RECURSIVE traversal AS (');
        Query.SQL.Add('  SELECT id, 1 AS depth, ARRAY[id] AS path');
        Query.SQL.Add('  FROM ' + FNodesTableName);
        Query.SQL.Add('  WHERE entidad = :entidad');
        Query.SQL.Add('    AND id IN (' + AnchorIDsList.CommaText + ')');

        Query.SQL.Add('  UNION ALL');

        Query.SQL.Add(
          '  SELECT CASE WHEN e.source_node_id = t.id ' +
          '              THEN e.target_node_id ' +
          '              ELSE e.source_node_id END,'
        );
        Query.SQL.Add('         t.depth + 1,');
        Query.SQL.Add(
          '         t.path || CASE WHEN e.source_node_id = t.id ' +
          '                         THEN e.target_node_id ' +
          '                         ELSE e.source_node_id END'
        );

        Query.SQL.Add('  FROM traversal t');
        Query.SQL.Add(
          '  JOIN ' + FEdgesTableName +
          ' e ON (e.source_node_id = t.id OR e.target_node_id = t.id)'
        );
        Query.SQL.Add('  WHERE e.entidad = :entidad');
        Query.SQL.Add('    AND t.depth < :depth');
        Query.SQL.Add(
          '    AND NOT (CASE WHEN e.source_node_id = t.id ' +
          '                  THEN e.target_node_id ' +
          '                  ELSE e.source_node_id END = ANY(t.path))'
        );
        Query.SQL.Add(')');
        Query.SQL.Add('SELECT DISTINCT id FROM traversal');

        Query.ParamByName('entidad').AsString := FCurrentEntidad;
        Query.ParamByName('depth').AsInteger := ADepth;
        Query.Open;

        while not Query.Eof do
        begin
          // Hidratamos el subgrafo completo en memoria
          Graph.FindNodeByID(Query.Fields[0].AsString);
          Query.Next;
        end;
      finally
        AnchorIDsList.Free;
      end;
    end;

    // -------------------------------------------------------------
    // FASE C: RESULTADO FINAL (NODOS ANCLA)
    // -------------------------------------------------------------
    SetLength(Result, AnchorIDs.Count);
    for i := 0 to AnchorIDs.Count - 1 do
    begin
      Result[i] := Graph.FindNodeByID(AnchorIDs[i]);
      if Result[i] <> nil then
        Result[i].EnsureEdgesAreLoaded;
    end;

  finally
    Query.Free;
    AnchorIDs.Free;
    SQL.Free;
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
      Query.SQL.Add('INSERT INTO ' + FEdgesTableName + ' (entidad, id, edge_label, name, node_text, source_node_id, target_node_id, weight, properties, embedding) ');
      Query.SQL.Add('VALUES (:entidad, :id, :edge_label, :name, :node_text, :source_node_id, :target_node_id, :weight, :properties::JsonB, ''' + EmbeddingToString(AEdge.Data) + ''')');
    End
    Else
    Begin
      Query.SQL.Add('INSERT INTO ' + FEdgesTableName + ' (entidad, id, edge_label, name, node_text, source_node_id, target_node_id, weight, properties) ');
      Query.SQL.Add('VALUES (:entidad, :id, :edge_label, :name, :node_text, :source_node_id, :target_node_id, :weight, :properties::JsonB)');
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
    Query.ParamByName('node_text').AsString := AEdge.Text;

    Query.ExecSQL;

  finally
    Query.Free;
  end;
end;



procedure TAiRagGraphPostgresDriver.AddNode(ANode: TAiRagGraphNode);
var
  Query: TFDQuery;
  EmbeddingSQL: string;
begin
  Query := NewQuery;
  try
    if Length(ANode.Data) > 0 then
      EmbeddingSQL := '''' + EmbeddingToString(ANode.Data) + ''''
    else
      EmbeddingSQL := 'NULL';

    Query.SQL.Add('INSERT INTO ' + FNodesTableName +
      ' (entidad, id, node_label, name, node_text, properties, embedding)'); // Añadido node_text
    Query.SQL.Add(' VALUES (:entidad, :id, :node_label, :name, :node_text, :properties::jsonb, ' + EmbeddingSQL + ')');

    Query.SQL.Add(' ON CONFLICT (id, entidad) DO UPDATE SET ');
    Query.SQL.Add('   name = EXCLUDED.name, ');
    Query.SQL.Add('   node_text = EXCLUDED.node_text, '); // <--- Vital para actualizar el contexto
    Query.SQL.Add('   properties = EXCLUDED.properties');

    if Length(ANode.Data) > 0 then
      Query.SQL.Add(', embedding = EXCLUDED.embedding');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('id').AsString := ANode.ID;
    Query.ParamByName('node_label').AsString := ANode.NodeLabel;
    Query.ParamByName('name').AsString := ANode.Name;
    Query.ParamByName('node_text').AsString := ANode.Text;
    Query.ParamByName('properties').Value := PropertiesToJSONString(ANode.Properties);

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;


end.
