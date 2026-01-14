unit uMakerAi.RAG.Graph.Driver.Postgres;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.StrUtils,
  System.Variants, System.JSON, System.Math,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param,
  uMakerAi.RAG.Vectors, uMakerAi.Embeddings.core,
  uMakerAi.RAG.Graph.core, uMakerAi.RAG.MetaData;

type
  TAiRagGraphPostgresDriver = class(TAiRagGraphDriverBase)
  private
    FConnection: TFDConnection;
    FCurrentEntidad: string;
    FTableName: String;
    FEdgesTableName: String;
    FNodesTableName: String;
    FChunksTableName: String;
    FLanguage: TAiLanguage;

    function NewQuery: TFDQuery;
    procedure SetTableName(const Value: String);
    function GetPostgresLangConfig: string;
  protected
    // --- Lectura de Datos Primitivos ---
    function FindNodeDataByID(const ANodeID: string; out ANodeData: TNodeDataRecord): Boolean; override;
    function FindEdgeDataByID(const AEdgeID: string; out AEdgeData: TEdgeDataRecord): Boolean; override;

    // --- Navegación y Carga ---
    procedure GetNodeEdges(ANode: TAiRagGraphNode); override;

    // --- Escritura (CRUD) ---
    procedure AddNode(ANode: TAiRagGraphNode); override;
    procedure AddEdge(AEdge: TAiRagGraphEdge); override;
    procedure DeleteNode(const ANodeID: string); override;
    procedure DeleteEdge(const AEdgeID: string); override;

    // --- Metadatos del Grafo ---
    function GetUniqueNodeLabels: TArray<string>; override;
    function GetUniqueEdgeLabels: TArray<string>; override;

    // --- Búsquedas Específicas ---
    function FindNodeByName(const AName, ANodeLabel: string): TAiRagGraphNode; override;
    function FindNodesByLabel(const ALabel: string): TArray<TAiRagGraphNode>; override;
    function FindNodesByProperty(const AKey: string; const AValue: Variant): TArray<TAiRagGraphNode>; override;
    function FindNodeNamesByLabel(const ANodeLabel, ASearchText: string; ALimit: Integer): TArray<string>; override;

    // --- Motores de Búsqueda Avanzada (RAG y Grafos) ---
    // NOTA: AFilter actualizado a TAiFilterCriteria
    function SearchNodes(const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria = nil): TArray<TAiRagGraphNode>; override;
    function Query(const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateSchema(const ABaseTableName: string; AVectorDim: Integer);
  published
    property Connection: TFDConnection read FConnection write FConnection;
    property CurrentEntidad: string read FCurrentEntidad write FCurrentEntidad;
    Property TableName: String read FTableName write SetTableName;
    property Language: TAiLanguage read FLanguage write FLanguage default alSpanish;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI.RAG.Drivers', [TAiRagGraphPostgresDriver]);
end;

{ TAiRagGraphPostgresDriver }

constructor TAiRagGraphPostgresDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentEntidad := 'DEFAULT';
  SetTableName('graph_');
  FLanguage := alSpanish;
end;

procedure TAiRagGraphPostgresDriver.SetTableName(const Value: String);
begin
  FTableName := Value;
  FNodesTableName := FTableName + 'nodes';
  FEdgesTableName := FTableName + 'edges';
  FChunksTableName := FNodesTableName + '_chunks'; // Convención: nombre_tabla_nodes_chunks
end;

function TAiRagGraphPostgresDriver.NewQuery: TFDQuery;
begin
  if not Assigned(FConnection) then
    raise Exception.Create('La propiedad Connection del Driver no ha sido asignada.');
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
end;

function TAiRagGraphPostgresDriver.GetPostgresLangConfig: string;
begin
  case FLanguage of
    alSpanish:
      Result := 'spanish';
    alEnglish:
      Result := 'english';
    alPortuguese:
      Result := 'portuguese';
  else
    Result := 'simple';
  end;
end;

procedure TAiRagGraphPostgresDriver.CreateSchema(const ABaseTableName: string; AVectorDim: Integer);
var
  Query: TFDQuery;
  NodesTableName, EdgesTableName, ChunksTableName: string;
  VectorType: string;
  LangCfg: string;
begin
  if not Assigned(FConnection) or not FConnection.Connected then
    raise Exception.Create('La conexión a la base de datos no está activa.');

  Self.TableName := ABaseTableName; // Actualiza las variables internas de nombres
  NodesTableName := FNodesTableName;
  EdgesTableName := FEdgesTableName;
  ChunksTableName := FChunksTableName;

  VectorType := Format('vector(%d)', [AVectorDim]);
  LangCfg := GetPostgresLangConfig;

  Query := NewQuery;
  try
    // 1. EXTENSIÓN PGVECTOR
    try
      Query.SQL.Text := 'CREATE EXTENSION IF NOT EXISTS vector;';
      Query.ExecSQL;
    except
      on E: Exception do; // Ignorar si ya existe o falta permisos (idealmente loggear)
    end;

    // 2. TABLA DE NODOS
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE TABLE IF NOT EXISTS public.%s (', [NodesTableName]));
    Query.SQL.Add('    entidad character varying(50) NOT NULL,');
    Query.SQL.Add('    id text NOT NULL,');
    Query.SQL.Add('    node_label text NOT NULL,');
    Query.SQL.Add('    name text,');
    Query.SQL.Add('    node_text text,');
    Query.SQL.Add('    properties jsonb,');
    Query.SQL.Add(Format('    embedding %s,', [VectorType]));

    // Columna generada para búsqueda híbrida (FTS)
    Query.SQL.Add('    search_vector tsvector GENERATED ALWAYS AS (');
    Query.SQL.Add(Format('       setweight(to_tsvector(''%s'', coalesce(name, '''')), ''A'') || ', [LangCfg]));
    Query.SQL.Add(Format('       setweight(to_tsvector(''%s'', coalesce(node_label, '''')), ''B'') || ', [LangCfg]));
    Query.SQL.Add(Format('       setweight(to_tsvector(''%s'', coalesce(node_text, '''')), ''C'')) STORED,', [LangCfg]));

    Query.SQL.Add(Format('    CONSTRAINT %s_pkey PRIMARY KEY (id, entidad)', [NodesTableName]));
    Query.SQL.Add(');');
    Query.ExecSQL;

    // 3. TABLA DE CHUNKS
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE TABLE IF NOT EXISTS public.%s (', [ChunksTableName]));
    Query.SQL.Add('    entidad character varying(50) NOT NULL,');
    Query.SQL.Add('    node_id text NOT NULL,');
    Query.SQL.Add('    chunk_index int NOT NULL,');
    Query.SQL.Add('    content text,');
    Query.SQL.Add(Format('    embedding %s,', [VectorType]));
    Query.SQL.Add(Format('    CONSTRAINT %s_pkey PRIMARY KEY (entidad, node_id, chunk_index),', [ChunksTableName]));
    Query.SQL.Add(Format('    CONSTRAINT fk_%s_parent FOREIGN KEY (node_id, entidad) REFERENCES %s (id, entidad) ON DELETE CASCADE', [ChunksTableName, NodesTableName]));
    Query.SQL.Add(');');
    Query.ExecSQL;

    // 4. ÍNDICES NODOS Y CHUNKS
    Query.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_emb_hnsw ON public.%s USING hnsw (embedding vector_cosine_ops);', [NodesTableName, NodesTableName]));
    Query.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_fts ON public.%s USING gin(search_vector);', [NodesTableName, NodesTableName]));
    Query.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_props ON public.%s USING gin(properties);', [NodesTableName, NodesTableName]));
    Query.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_emb_hnsw ON public.%s USING hnsw (embedding vector_cosine_ops);', [ChunksTableName, ChunksTableName]));

    // 5. TABLA DE ARISTAS
    Query.SQL.Clear;
    Query.SQL.Add(Format('CREATE TABLE IF NOT EXISTS public.%s (', [EdgesTableName]));
    Query.SQL.Add('    entidad character varying(50) NOT NULL,');
    Query.SQL.Add('    id text NOT NULL,');
    Query.SQL.Add('    edge_label text NOT NULL,');
    Query.SQL.Add('    name text,');
    Query.SQL.Add('    node_text text,');
    Query.SQL.Add('    source_node_id text NOT NULL,');
    Query.SQL.Add('    target_node_id text NOT NULL,');
    Query.SQL.Add('    weight double precision DEFAULT 1.0,');
    Query.SQL.Add('    properties jsonb,');
    Query.SQL.Add(Format('    embedding %s,', [VectorType]));

    Query.SQL.Add(Format('    CONSTRAINT %s_pkey PRIMARY KEY (id, entidad),', [EdgesTableName]));
    Query.SQL.Add(Format('    CONSTRAINT fk_source_%s FOREIGN KEY (source_node_id, entidad) REFERENCES %s (id, entidad) ON DELETE CASCADE,', [EdgesTableName, NodesTableName]));
    Query.SQL.Add(Format('    CONSTRAINT fk_target_%s FOREIGN KEY (target_node_id, entidad) REFERENCES %s (id, entidad) ON DELETE CASCADE', [EdgesTableName, NodesTableName]));
    Query.SQL.Add(');');
    Query.ExecSQL;

    // 6. ÍNDICES DE ARISTAS
    Query.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_edge_label ON public.%s (entidad, edge_label);', [EdgesTableName, EdgesTableName]));
    Query.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_source_id ON public.%s (source_node_id, entidad);', [EdgesTableName, EdgesTableName]));
    Query.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_target_id ON public.%s (target_node_id, entidad);', [EdgesTableName, EdgesTableName]));
    Query.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_props ON public.%s USING gin(properties);', [EdgesTableName, EdgesTableName]));

  finally
    Query.Free;
  end;
end;

procedure TAiRagGraphPostgresDriver.AddNode(ANode: TAiRagGraphNode);
var
  Query: TFDQuery;
  EmbeddingSQL: string;
  i: Integer;
  JProp: TJSONObject;
begin
  Query := NewQuery;
  try
    if Length(ANode.Data) > 0 then
      EmbeddingSQL := '''' + EmbeddingToString(ANode.Data) + ''''
    else
      EmbeddingSQL := 'NULL';

    Query.SQL.Add('INSERT INTO ' + FNodesTableName + ' (entidad, id, node_label, name, node_text, properties, embedding)');
    Query.SQL.Add(' VALUES (:entidad, :id, :node_label, :name, :node_text, :properties::jsonb, ' + EmbeddingSQL + '::vector)');
    Query.SQL.Add(' ON CONFLICT (id, entidad) DO UPDATE SET ');
    Query.SQL.Add('   name = EXCLUDED.name, ');
    Query.SQL.Add('   node_text = EXCLUDED.node_text, ');
    Query.SQL.Add('   properties = EXCLUDED.properties');
    if Length(ANode.Data) > 0 then
      Query.SQL.Add(', embedding = EXCLUDED.embedding');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('id').AsString := ANode.ID;
    Query.ParamByName('node_label').AsString := ANode.NodeLabel;
    Query.ParamByName('name').AsString := ANode.Name;
    Query.ParamByName('node_text').AsString := ANode.Text;

    // Usamos la serialización JSON del MetaData
    if Assigned(ANode.MetaData) then
    begin
      JProp := ANode.MetaData.ToJSON;
      try
        Query.ParamByName('properties').AsString := JProp.ToString;
      finally
        JProp.Free;
      end;
    end
    else
      Query.ParamByName('properties').AsString := '{}';

    Query.ExecSQL;

    // Gestión de Chunks
    if (ANode.Chunks <> nil) and (ANode.Chunks.Count > 0) then
    begin
      // Borrar chunks viejos
      Query.SQL.Text := 'DELETE FROM ' + FChunksTableName + ' WHERE entidad = :entidad AND node_id = :id';
      Query.ParamByName('entidad').AsString := FCurrentEntidad;
      Query.ParamByName('id').AsString := ANode.ID;
      Query.ExecSQL;

      // Insertar chunks nuevos
      Query.SQL.Text := 'INSERT INTO ' + FChunksTableName + ' (entidad, node_id, chunk_index, content, embedding) ' + ' VALUES (:entidad, :id, :idx, :content, :emb::vector)';

      for i := 0 to ANode.Chunks.Count - 1 do
      begin
        Query.ParamByName('entidad').AsString := FCurrentEntidad;
        Query.ParamByName('id').AsString := ANode.ID;
        Query.ParamByName('idx').AsInteger := i;
        Query.ParamByName('content').AsString := ANode.Chunks[i].Text;
        Query.ParamByName('emb').AsString := EmbeddingToString(ANode.Chunks[i].Data);
        Query.ExecSQL;
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TAiRagGraphPostgresDriver.AddEdge(AEdge: TAiRagGraphEdge);
var
  Query: TFDQuery;
  EmbeddingSQL: string;
  JProp: TJSONObject;
begin
  Query := NewQuery;
  try
    if Length(AEdge.Data) > 0 then
      EmbeddingSQL := '''' + EmbeddingToString(AEdge.Data) + ''''
    else
      EmbeddingSQL := 'NULL';

    Query.SQL.Add('INSERT INTO ' + FEdgesTableName + ' (entidad, id, edge_label, name, node_text, source_node_id, target_node_id, weight, properties, embedding) ');
    Query.SQL.Add('VALUES (:entidad, :id, :edge_label, :name, :node_text, :source_node_id, :target_node_id, :weight, :properties::JsonB, ' + EmbeddingSQL + '::vector)');
    Query.SQL.Add('ON CONFLICT (id, entidad) DO UPDATE SET ');
    Query.SQL.Add('  weight = EXCLUDED.weight, ');
    Query.SQL.Add('  properties = EXCLUDED.properties, ');
    Query.SQL.Add('  node_text = EXCLUDED.node_text');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('id').AsString := AEdge.ID;
    Query.ParamByName('edge_label').AsString := AEdge.EdgeLabel;
    Query.ParamByName('name').AsString := AEdge.Name;
    Query.ParamByName('node_text').AsString := AEdge.Text;
    Query.ParamByName('source_node_id').AsString := AEdge.FromNode.ID;
    Query.ParamByName('target_node_id').AsString := AEdge.ToNode.ID;
    Query.ParamByName('weight').AsFloat := AEdge.Weight;

    if Assigned(AEdge.MetaData) then
    begin
      JProp := AEdge.MetaData.ToJSON;
      try
        Query.ParamByName('properties').AsString := JProp.ToString;
      finally
        JProp.Free;
      end;
    end
    else
      Query.ParamByName('properties').AsString := '{}';

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

function TAiRagGraphPostgresDriver.FindEdgeDataByID(const AEdgeID: string; out AEdgeData: TEdgeDataRecord): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := NewQuery;
  try
    Query.SQL.Add('SELECT id, edge_label, name, source_node_id, target_node_id, ');
    Query.SQL.Add('  weight, properties, embedding, node_text ');
    Query.SQL.Add('FROM  ' + FEdgesTableName);
    Query.SQL.Add('WHERE id = :id AND entidad = :entidad');

    Query.ParamByName('id').AsString := AEdgeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;

    if not Query.IsEmpty then
    begin
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

procedure TAiRagGraphPostgresDriver.GetNodeEdges(ANode: TAiRagGraphNode);
var
  Query: TFDQuery;
  EdgeData: TEdgeDataRecord;
  EdgeID: string;
begin
  if (ANode = nil) or (FConnection = nil) then
    Exit;

  Query := NewQuery;
  try
    Query.SQL.Add('SELECT id, edge_label, name, source_node_id, target_node_id, ');
    Query.SQL.Add('       weight, properties, embedding, node_text ');
    Query.SQL.Add('FROM ' + FEdgesTableName);
    Query.SQL.Add('WHERE entidad = :entidad');
    Query.SQL.Add('  AND (source_node_id = :node_id OR target_node_id = :node_id)');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_id').AsString := ANode.ID;
    Query.Open;

    while not Query.Eof do
    begin
      EdgeID := Query.FieldByName('id').AsString;

      if not Graph.EdgeExistsInMemory(EdgeID) then
      begin
        EdgeData.ID := EdgeID;
        EdgeData.EdgeLabel := Query.FieldByName('edge_label').AsString;
        EdgeData.Name := Query.FieldByName('name').AsString;
        EdgeData.SourceNodeID := Query.FieldByName('source_node_id').AsString;
        EdgeData.TargetNodeID := Query.FieldByName('target_node_id').AsString;
        EdgeData.Weight := Query.FieldByName('weight').AsFloat;
        EdgeData.NodeText := Query.FieldByName('node_text').AsString;

        if not Query.FieldByName('properties').IsNull then
          EdgeData.PropertiesJSON := Query.FieldByName('properties').AsString
        else
          EdgeData.PropertiesJSON := '{}';

        if not Query.FieldByName('embedding').IsNull then
          EdgeData.EmbeddingStr := Query.FieldByName('embedding').AsString
        else
          EdgeData.EmbeddingStr := '[]';

        Graph.InternalHydrateEdge(EdgeData);
      end;
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

// =============================================================================
// CORE DE BÚSQUEDA HÍBRIDA GRAFO (VECTOR + LÉXICA + RRF + CHUNKS)
// =============================================================================
function TAiRagGraphPostgresDriver.SearchNodes(const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria = nil): TArray<TAiRagGraphNode>;
var
  Query: TFDQuery;
  SQL: TStringBuilder;

  // Config
  Options: TAiSearchOptions;
  DoVector, DoLexical: Boolean;
  MinVectorScore, MinLexicalScore: Double;
  EmbeddingStr, EmbeddingLiteral: string;
  LangConfig, FilterSQL: string;
  HasFilter: Boolean;
  FS: TFormatSettings;

  // Vars
  AnchorIDsList: TStringList;
  i: Integer;

  // --- Helper: Convertir TAiFilterCriteria a SQL JSONB ---
  function GetCriteriaSQL(Prefix: string): string;
  var
    Criteria: TStringBuilder;
    k: Integer;
    Item: TFilterCriterion;
    KeyJSON: string;

    function FormatVal(V: Variant): string;
    begin
      if VarIsNumeric(V) or VarIsType(V, varBoolean) then
        Result := VarToStr(V)
      else
        Result := QuotedStr(VarToStr(V));
    end;

  begin
    if (AFilter = nil) or (AFilter.Count = 0) then
      Exit('TRUE');

    Criteria := TStringBuilder.Create;
    try
      for k := 0 to AFilter.Count - 1 do
      begin
        if k > 0 then
          Criteria.Append(' AND ');
        Item := AFilter.Items[k];

        KeyJSON := '(' + Prefix + '.properties ->> ' + QuotedStr(Item.Key) + ')';

        case Item.Op of
          foEqual:
            Criteria.Append(KeyJSON + ' = ' + FormatVal(Item.Value));
          foNotEqual:
            Criteria.Append(KeyJSON + ' <> ' + FormatVal(Item.Value));
          foGreater:
            Criteria.Append(KeyJSON + ' > ' + FormatVal(Item.Value));
          foGreaterOrEqual:
            Criteria.Append(KeyJSON + ' >= ' + FormatVal(Item.Value));
          foLess:
            Criteria.Append(KeyJSON + ' < ' + FormatVal(Item.Value));
          foLessOrEqual:
            Criteria.Append(KeyJSON + ' <= ' + FormatVal(Item.Value));

          foContains:
            Criteria.Append(KeyJSON + ' ILIKE ' + QuotedStr('%' + VarToStr(Item.Value) + '%'));
          foStartsWith:
            Criteria.Append(KeyJSON + ' ILIKE ' + QuotedStr(VarToStr(Item.Value) + '%'));
          foEndsWith:
            Criteria.Append(KeyJSON + ' ILIKE ' + QuotedStr('%' + VarToStr(Item.Value)));
          foLike, foILike:
            Criteria.Append(KeyJSON + ' ILIKE ' + QuotedStr(VarToStr(Item.Value)));

          foIsNull:
            Criteria.Append('(' + KeyJSON + ' IS NULL)');
          foIsNotNull, foExists:
            Criteria.Append('(' + KeyJSON + ' IS NOT NULL)');

          foBetween:
            begin
              Criteria.Append('(' + KeyJSON + ' >= ' + FormatVal(Item.Value));
              Criteria.Append(' AND ' + KeyJSON + ' <= ' + FormatVal(Item.Value2) + ')');
            end;

          foIn, foNotIn:
            begin
              if Item.Op = foNotIn then
                Criteria.Append(KeyJSON + ' NOT IN (')
              else
                Criteria.Append(KeyJSON + ' IN (');
              // Simplificación: Asume valor escalar o string CSV. Para arrays reales se requiere iterar Variant array.
              Criteria.Append(FormatVal(Item.Value) + ')');
            end;
        else
          Criteria.Append(KeyJSON + ' = ' + FormatVal(Item.Value));
        end;
      end;
      Result := Criteria.ToString;
    finally
      Criteria.Free;
    end;
  end;

begin
  SetLength(Result, 0);
  if (Graph = nil) or APrompt.IsEmpty then
    Exit;

  // 1. Obtener Opciones del Grafo
  Options := Graph.SearchOptions;
  FS := TFormatSettings.Invariant;
  LangConfig := GetPostgresLangConfig;

  DoVector := Options.UseEmbeddings;
  DoLexical := Options.UseBM25;

  if not(DoVector or DoLexical) then
    Exit;

  MinVectorScore := Options.MinAbsoluteScoreEmbedding;
  MinLexicalScore := Options.MinAbsoluteScoreBM25;

  // 2. Preparar Embedding
  if DoVector then
  begin
    if not Assigned(Graph.Embeddings) then
      raise Exception.Create('Graph Search: Embeddings engine needed.');
    EmbeddingStr := EmbeddingToString(Graph.Embeddings.CreateEmbedding(APrompt, 'user'));
    EmbeddingLiteral := '''' + EmbeddingStr + '''::vector';
  end
  else
    EmbeddingLiteral := 'NULL';

  HasFilter := (AFilter <> nil) and (AFilter.Count > 0);

  Query := NewQuery;
  SQL := TStringBuilder.Create;
  try
    SQL.AppendLine('WITH ');

    // -----------------------------------------------------------
    // CTE 1: VECTOR SEARCH (NODES + CHUNKS)
    // -----------------------------------------------------------
    SQL.AppendLine('vector_raw AS (');
    if DoVector then
    begin
      // Nodos
      SQL.AppendLine('  SELECT id, (1 - (embedding <=> ' + EmbeddingLiteral + ')) as v_score');
      SQL.AppendLine('  FROM ' + FNodesTableName + ' n');
      SQL.AppendLine('  WHERE entidad = :ent');
      if HasFilter then
        SQL.AppendLine('    AND ' + GetCriteriaSQL('n'));
      if MinVectorScore > 0 then
        SQL.AppendLine('    AND (1 - (embedding <=> ' + EmbeddingLiteral + ')) >= :min_v');

      SQL.AppendLine('  UNION ALL');

      // Chunks
      SQL.AppendLine('  SELECT node_id as id, (1 - (embedding <=> ' + EmbeddingLiteral + ')) as v_score');
      SQL.AppendLine('  FROM ' + FChunksTableName + ' c');
      SQL.AppendLine('  WHERE entidad = :ent');
      if MinVectorScore > 0 then
        SQL.AppendLine('    AND (1 - (embedding <=> ' + EmbeddingLiteral + ')) >= :min_v');
    end
    else
    begin
      SQL.AppendLine('  SELECT NULL::text as id, 0::float as v_score WHERE FALSE');
    end;
    SQL.AppendLine('),');

    // Max Pooling: Mejor score entre nodo y sus chunks
    SQL.AppendLine('vector_best AS (');
    SQL.AppendLine('  SELECT id, MAX(v_score) as v_score,');
    SQL.AppendLine('         ROW_NUMBER() OVER (ORDER BY MAX(v_score) DESC) as v_rank');
    SQL.AppendLine('  FROM vector_raw');
    SQL.AppendLine('  GROUP BY id');
    SQL.AppendLine('  LIMIT :prelim_limit');
    SQL.AppendLine('),');

    // -----------------------------------------------------------
    // CTE 2: LEXICAL SEARCH
    // -----------------------------------------------------------
    SQL.AppendLine('lexical_res AS (');
    if DoLexical then
    begin
      SQL.AppendLine('  SELECT id,');
      SQL.AppendLine('    ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', :query), 32) as l_score,');
      SQL.AppendLine('    ROW_NUMBER() OVER (ORDER BY ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', :query), 32) DESC) as l_rank');
      SQL.AppendLine('  FROM ' + FNodesTableName + ' n');
      SQL.AppendLine('  WHERE entidad = :ent');
      SQL.AppendLine('    AND search_vector @@ websearch_to_tsquery(''' + LangConfig + ''', :query)');
      if HasFilter then
        SQL.AppendLine('    AND ' + GetCriteriaSQL('n'));
      if MinLexicalScore > 0 then
        SQL.AppendLine('    AND ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', :query), 32) >= :min_l');
      SQL.AppendLine('  LIMIT :prelim_limit');
    end
    else
    begin
      SQL.AppendLine('  SELECT NULL::text as id, 0::float as l_score, 0::bigint as l_rank WHERE FALSE');
    end;
    SQL.AppendLine('),');

    // -----------------------------------------------------------
    // CTE 3: FUSION
    // -----------------------------------------------------------
    SQL.AppendLine('scored AS (');
    SQL.AppendLine('  SELECT COALESCE(v.id, l.id) as id,');
    SQL.AppendLine('         COALESCE(v.v_score, 0) as v_score, COALESCE(l.l_score, 0) as l_score,');

    if DoVector and DoLexical then
    begin
      if Options.UseRRF then
        SQL.AppendLine('    (1.0 / (60 + COALESCE(v.v_rank, 99999))) + (1.0 / (60 + COALESCE(l.l_rank, 99999))) as raw_score')
      else
        SQL.AppendFormat('    (COALESCE(v.v_score, 0) * %s) + (COALESCE(l.l_score, 0) * %s) as raw_score', [FloatToStr(Options.EmbeddingWeight, FS), FloatToStr(Options.BM25Weight, FS)]).AppendLine;
    end
    else if DoVector then
      SQL.AppendLine('    v.v_score as raw_score')
    else
      SQL.AppendLine('    l.l_score as raw_score');

    SQL.AppendLine('  FROM vector_best v FULL OUTER JOIN lexical_res l ON v.id = l.id');
    SQL.AppendLine('),');

    // CTE 4: ANCHORS
    SQL.AppendLine('anchors AS (');
    SQL.AppendLine('  SELECT id FROM scored');
    if APrecision > 0 then
      SQL.AppendLine('  WHERE raw_score >= :prec');
    SQL.AppendLine('  ORDER BY raw_score DESC');
    SQL.AppendLine('  LIMIT :limit');
    SQL.AppendLine(')');

    // -----------------------------------------------------------
    // CTE 5: GRAPH EXPANSION (BFS)
    // -----------------------------------------------------------
    if ADepth > 0 then
    begin
      SQL.AppendLine(', traversal AS (');
      SQL.AppendLine('  SELECT id, 1 AS depth, ARRAY[id] AS path');
      SQL.AppendLine('  FROM anchors');
      SQL.AppendLine('  UNION ALL');
      SQL.AppendLine('  SELECT CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END,');
      SQL.AppendLine('         t.depth + 1,');
      SQL.AppendLine('         t.path || CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END');
      SQL.AppendLine('  FROM traversal t');
      SQL.AppendLine('  JOIN ' + FEdgesTableName + ' e ON (e.source_node_id = t.id OR e.target_node_id = t.id)');
      SQL.AppendLine('  WHERE e.entidad = :ent');
      SQL.AppendLine('    AND t.depth < :depth');
      SQL.AppendLine('    AND NOT (CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END = ANY(t.path))');
      SQL.AppendLine(')');
      SQL.AppendLine('SELECT DISTINCT id FROM traversal');
    end
    else
    begin
      SQL.AppendLine('SELECT id FROM anchors');
    end;

    // --- EXECUTION ---
    Query.SQL.Text := SQL.ToString;

    Query.ParamByName('ent').AsString := FCurrentEntidad;
    Query.ParamByName('limit').AsInteger := ALimit;
    Query.ParamByName('prelim_limit').AsInteger := ALimit * 4;

    if APrecision > 0 then
      Query.ParamByName('prec').AsFloat := APrecision;
    if ADepth > 0 then
      Query.ParamByName('depth').AsInteger := ADepth;

    if DoVector then
    begin
      if MinVectorScore > 0 then
        Query.ParamByName('min_v').AsFloat := MinVectorScore;
    end;

    if DoLexical then
    begin
      Query.ParamByName('query').AsString := APrompt;
      if MinLexicalScore > 0 then
        Query.ParamByName('min_l').AsFloat := MinLexicalScore;
    end;

    Query.Open;

    // --- HYDRATION ---
    Query.First;
    AnchorIDsList := TStringList.Create;
    try
      while not Query.Eof do
      begin
        AnchorIDsList.Add(Query.FieldByName('id').AsString);
        Query.Next;
      end;

      SetLength(Result, AnchorIDsList.Count);
      for i := 0 to AnchorIDsList.Count - 1 do
      begin
        Result[i] := Graph.FindNodeByID(AnchorIDsList[i]);
        if Result[i] <> nil then
          Result[i].EnsureEdgesAreLoaded;
      end;
    finally
      AnchorIDsList.Free;
    end;

  finally
    Query.Free;
    SQL.Free;
  end;
end;

function TAiRagGraphPostgresDriver.Query(const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double): TArray<TAiRagGraphNode>;
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
  if Length(APlan.Steps) = 0 then
    Exit;

  Query := NewQuery;
  SQL := TStringBuilder.Create;
  NodeIDs := TStringList.Create;
  try
    if not Assigned(Graph.Embeddings) then
      raise Exception.Create('El motor de Embeddings no está asignado al grafo.');

    QueryEmbedding := Graph.Embeddings.CreateEmbedding(APlan.AnchorPrompt, 'user');
    EmbeddingStr := EmbeddingToString(QueryEmbedding);

    SQL.AppendLine('WITH RECURSIVE ');

    // Paso 1: Anclaje
    SQL.AppendLine(Format('  %s AS (', [APlan.AnchorVariable]));
    SQL.AppendLine('    SELECT id FROM ' + FNodesTableName);
    SQL.AppendLine('    WHERE entidad = :entidad');
    SQL.AppendLine('      AND (1 - (embedding <=> ''' + EmbeddingStr + ''')) >= :precision');
    SQL.AppendLine('    ORDER BY embedding <=> ''' + EmbeddingStr + ''' ASC');
    SQL.AppendLine('    LIMIT :limit');
    SQL.AppendLine('  )');

    // Pasos siguientes
    for i := 0 to High(APlan.Steps) do
    begin
      Step := APlan.Steps[i];
      PrevStepVar := Step.SourceVariable;
      CurrentStepVar := Step.TargetVariable;

      SQL.AppendLine(Format('  , %s AS (', [CurrentStepVar]));
      SQL.AppendLine('    SELECT DISTINCT ');
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

      if not Step.TargetNodeLabel.IsEmpty then
      begin
        SQL.AppendLine('      AND EXISTS (');
        SQL.AppendLine('        SELECT 1 FROM ' + FNodesTableName + ' fn');
        SQL.AppendLine('        WHERE fn.id = ' + IfThen(Step.IsReversed, 'e.source_node_id', 'e.target_node_id'));
        SQL.AppendLine(Format('          AND fn.node_label = :target_label_%d', [i]));
        SQL.AppendLine('          AND fn.entidad = :entidad');
        SQL.AppendLine('      )');
      end;
      SQL.AppendLine('  )');
    end;

    SQL.AppendLine(Format('SELECT DISTINCT id FROM %s', [APlan.ResultVariable]));

    Query.SQL.Text := SQL.ToString;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('precision').AsFloat := APrecision;
    Query.ParamByName('limit').AsInteger := ALimit;

    for i := 0 to High(APlan.Steps) do
    begin
      Query.ParamByName(Format('step_label_%d', [i])).AsString := APlan.Steps[i].EdgeLabel;
      if not APlan.Steps[i].TargetNodeLabel.IsEmpty then
        Query.ParamByName(Format('target_label_%d', [i])).AsString := APlan.Steps[i].TargetNodeLabel;
    end;

    Query.Open;
    while not Query.Eof do
    begin
      NodeIDs.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    SetLength(Result, NodeIDs.Count);
    for i := 0 to NodeIDs.Count - 1 do
    begin
      Result[i] := Graph.FindNodeByID(NodeIDs[i]);
      if (Result[i] <> nil) and (ADepth > 0) then
        Result[i].EnsureEdgesAreLoaded;
    end;

  finally
    SQL.Free;
    Query.Free;
    NodeIDs.Free;
  end;
end;

function TAiRagGraphPostgresDriver.FindNodeByName(const AName, ANodeLabel: string): TAiRagGraphNode;
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Add('SELECT id FROM ' + FNodesTableName);
    Query.SQL.Add('WHERE entidad = :entidad');
    Query.SQL.Add('  AND node_label = :node_label');
    Query.SQL.Add('  AND name = :name LIMIT 1');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_label').AsString := ANodeLabel;
    Query.ParamByName('name').AsString := AName;
    Query.Open;

    if not Query.Eof then
      Result := Graph.FindNodeByID(Query.FieldByName('id').AsString)
    else
      Result := nil;
  finally
    Query.Free;
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
    Query.SQL.Add('SELECT id FROM ' + FNodesTableName);
    Query.SQL.Add('WHERE entidad = :entidad AND node_label = :node_label');
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_label').AsString := ALabel;
    Query.Open;

    while not Query.Eof do
    begin
      NodeIDs.Add(Query.FieldByName('id').AsString);
      Query.Next;
    end;

    SetLength(Result, NodeIDs.Count);
    for i := 0 to NodeIDs.Count - 1 do
      Result[i] := Graph.FindNodeByID(NodeIDs[i]);
  finally
    Query.Free;
    NodeIDs.Free;
  end;
end;

function TAiRagGraphPostgresDriver.FindNodesByProperty(const AKey: string; const AValue: Variant): TArray<TAiRagGraphNode>;
var
  Query: TFDQuery;
  NodeIDs: TStringList;
  i: Integer;
begin
  Query := NewQuery;
  NodeIDs := TStringList.Create;
  try
    if SameText(AKey, 'name') then
    begin
      Query.SQL.Text := 'SELECT id FROM ' + FNodesTableName + ' WHERE entidad = :entidad AND name ILIKE :value';
      Query.ParamByName('value').AsString := VarToStr(AValue);
    end
    else
    begin
      Query.SQL.Text := 'SELECT id FROM ' + FNodesTableName + ' WHERE entidad = :entidad AND properties ->> :key = :value';
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
    for i := 0 to NodeIDs.Count - 1 do
      Result[i] := Graph.FindNodeByID(NodeIDs[i]);

  finally
    Query.Free;
    NodeIDs.Free;
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
    Query.SQL.Add('SELECT name FROM ' + FNodesTableName);
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

function TAiRagGraphPostgresDriver.GetUniqueNodeLabels: TArray<string>;
var
  Query: TFDQuery;
  LabelList: TStringList;
begin
  Query := NewQuery;
  LabelList := TStringList.Create;
  try
    Query.SQL.Text := 'SELECT DISTINCT node_label FROM ' + FNodesTableName + ' WHERE entidad = :entidad ORDER BY node_label';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;
    while not Query.Eof do
    begin
      LabelList.Add(Query.Fields[0].AsString);
      Query.Next;
    end;
    Result := LabelList.ToStringArray;
  finally
    Query.Free;
    LabelList.Free;
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
    Query.SQL.Text := 'SELECT DISTINCT edge_label FROM ' + FEdgesTableName + ' WHERE entidad = :entidad ORDER BY edge_label';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;
    while not Query.Eof do
    begin
      LabelList.Add(Query.Fields[0].AsString);
      Query.Next;
    end;
    Result := LabelList.ToStringArray;
  finally
    Query.Free;
    LabelList.Free;
  end;
end;

end.
