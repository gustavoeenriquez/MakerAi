unit uMakerAi.RAG.Graph.Builder;

interface

uses
  System.SysUtils, System.Classes, System.Json, System.Generics.Collections,
  System.Variants, uMakerAi.RAG.Vectors, uMakerAi.Embeddings.core, uMakerAi.Embeddings,
  uMakerAi.RAG.Graph.core; // Incluimos la unidad del grafo

type
  { TMergeStrategy }
  TMergeStrategy = (msAddNewOnly, // (Default) Solo añade propiedades que no existen.
    msOverwrite, // Sobrescribe las propiedades existentes con las nuevas.
    msKeepExisting // No realiza ningún cambio en las propiedades del elemento existente.
    );

  { TAiRagGraphBuilder }
  TAiRagGraphBuilder = class(TComponent)
  private
    FGraph: TAiRagGraph;
    FEmbeddings: TAiEmbeddingsCore;
  protected
    procedure MergeNodeProperties(ANode: TAiRagGraphNode; ANewProperties: TJSONObject; AStrategy: TMergeStrategy);
    procedure MergeEdgeProperties(AEdge: TAiRagGraphEdge; ANewProperties: TJSONObject; AStrategy: TMergeStrategy);
    function GetOrCreateNode(ANodeObject: TJSONObject; AMergeStrategy: TMergeStrategy): TAiRagGraphNode;
    function GenerateTextForEmbedding(AName, ANodeLabel: string; AProperties: TJSONObject; AAdditionalText: String = ''): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Process(AJsonTripletArray: string; AMergeStrategy: TMergeStrategy = msAddNewOnly);
    function FindExistingNode(AName, ALabel: string): TAiRagGraphNode;

  published
    property Graph: TAiRagGraph read FGraph write FGraph;
    property Embeddings: TAiEmbeddingsCore read FEmbeddings write FEmbeddings;
  end;

procedure Register;

implementation

uses
  System.NetEncoding; // Para Base64 si es necesario

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiRagGraphBuilder]);
end;

{ TAiRagGraphBuilder }

constructor TAiRagGraphBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraph := nil;
end;

destructor TAiRagGraphBuilder.Destroy;
begin
  inherited;
end;

function TAiRagGraphBuilder.FindExistingNode(AName, ALabel: string): TAiRagGraphNode;
begin
  Result := FGraph.FindNodeByName(AName, ALabel);
end;

procedure TAiRagGraphBuilder.MergeEdgeProperties(AEdge: TAiRagGraphEdge; ANewProperties: TJSONObject; AStrategy: TMergeStrategy);
var
  Pair: TJSONPair;
  Key: string;
  Value: Variant;
begin
  if (ANewProperties = nil) or (AStrategy = msKeepExisting) then
    Exit;

  for Pair in ANewProperties do
  begin
    Key := Pair.JsonString.Value;
    Value := JSONValueToVariant(Pair.JsonValue); // Helper para convertir TJSONValue a Variant

    case AStrategy of
      msOverwrite:
        // Siempre sobrescribe o añade.
        AEdge.Properties.AddOrSetValue(Key, Value);

      msAddNewOnly:
        // Solo añade si la clave no existe.
        if not AEdge.Properties.ContainsKey(Key) then
          AEdge.Properties.Add(Key, Value);
    end;
  end;
end;

procedure TAiRagGraphBuilder.MergeNodeProperties(ANode: TAiRagGraphNode; ANewProperties: TJSONObject; AStrategy: TMergeStrategy);
var
  Pair: TJSONPair;
  Key: string;
  Value: Variant;
begin
  if (ANewProperties = nil) or (AStrategy = msKeepExisting) then
    Exit;

  for Pair in ANewProperties do
  begin
    Key := Pair.JsonString.Value;
    Value := JSONValueToVariant(Pair.JsonValue);

    case AStrategy of
      msOverwrite:
        ANode.Properties.AddOrSetValue(Key, Value);
      msAddNewOnly:
        if not ANode.Properties.ContainsKey(Key) then
          ANode.Properties.Add(Key, Value);
    end;
  end;
end;

function TAiRagGraphBuilder.GenerateTextForEmbedding(AName, ANodeLabel: string; AProperties: TJSONObject; AAdditionalText: String = ''): string;
var
  ContextBuilder: TStringBuilder;
  Pair: TJSONPair;
begin
  ContextBuilder := TStringBuilder.Create;
  try
    // 1. Añadir la información principal
    ContextBuilder.Append(AName);
    ContextBuilder.Append(' (Tipo: ');
    ContextBuilder.Append(ANodeLabel);
    ContextBuilder.Append(').');

    // 2. Añadir las propiedades como "hechos"
    if AProperties <> nil then
    begin
      ContextBuilder.Append(' Hechos conocidos: ');
      for Pair in AProperties do
      begin
        ContextBuilder.Append(Pair.JsonString.Value); // la clave
        ContextBuilder.Append(' es ');
        ContextBuilder.Append(Pair.JsonValue.Value); // el valor
        ContextBuilder.Append('; ');
      end;
    end;

    if (Trim(AAdditionalText) <> '') then
    begin
      ContextBuilder.Append(' Contenido asociado: ');
      ContextBuilder.Append(AAdditionalText);
    end;

    Result := ContextBuilder.ToString;
  finally
    ContextBuilder.Free;
  end;
end;

function TAiRagGraphBuilder.GetOrCreateNode(ANodeObject: TJSONObject; AMergeStrategy: TMergeStrategy): TAiRagGraphNode;
var
  NodeName, NodeLabel, NewNodeID, NodeText, AdditionalText: string;
  ExistingNode: TAiRagGraphNode;
  PropertiesValue: TJSONValue;
  NewProperties: TJSONObject;
  NodeEmbedding: TAiEmbeddingData;
  AddedNode: TAiRagGraphNode;
begin
  // ... (código para extraer NodeName, NodeLabel, NewProperties igual que antes)
  NodeName := ANodeObject.GetValue<string>('name', '');
  NodeLabel := ANodeObject.GetValue<string>('nodeLabel', 'Undefined');

  if NodeName.IsEmpty then
    raise Exception.Create('Node name cannot be empty in JSON input.');

  // Usar la función optimizada
  ExistingNode := FindExistingNode(NodeName, NodeLabel);

  NewProperties := nil;
  PropertiesValue := ANodeObject.FindValue('properties');
  if (PropertiesValue <> nil) and (PropertiesValue is TJSONObject) then
    NewProperties := PropertiesValue as TJSONObject;

  if ExistingNode <> nil then
  begin
    // Nodo encontrado: Fusionar propiedades y devolver.
    Result := ExistingNode;
    if NewProperties <> nil then
      MergeNodeProperties(Result, NewProperties, AMergeStrategy);
  end
  else
  begin
    // Nodo no encontrado: Crear, poblar completamente y LUEGO añadir.

    // 1. Crear la instancia del nodo.
    Result := TAiRagGraphNode.Create(FGraph, FGraph.Nodes.Dim);
    NewNodeID := TGuid.NewGuid.ToString;

    // 2. Poblar TODOS los datos.
    Result.ID := NewNodeID;
    Result.NodeLabel := NodeLabel;
    Result.Name := NodeName;

    // Poblar propiedades ANTES de generar el embedding.
    if NewProperties <> nil then
      MergeNodeProperties(Result, NewProperties, msOverwrite);

    // Generar texto y embedding.
    if Assigned(FEmbeddings) then
    begin
      AdditionalText := ANodeObject.GetValue<string>('text', '');
      NodeText := GenerateTextForEmbedding(Result.Name, Result.NodeLabel, NewProperties, AdditionalText);
      NodeEmbedding := FEmbeddings.CreateEmbedding(NodeText, 'user');
      Result.Text := NodeText;
      Result.Data := NodeEmbedding;
    end;

    // 3. Añadir el nodo COMPLETAMENTE POBLADO al grafo.
    AddedNode := FGraph.AddNode(Result); // Llama a la nueva sobrecarga.

    // 4. Manejar el resultado.
    if AddedNode <> Result then // Esto sucederá si el manejador de DB se hizo cargo
    begin
      // El manejador de DB ha persistido los datos. El objeto en memoria `Result`
      // ya no es necesario y debe ser liberado por el llamador (el Builder).
      Result.Free;
      // Necesitamos encontrar el nodo en la BD para continuar (por si necesitamos conectar una arista).
      Result := FindExistingNode(NodeName, NodeLabel);
    end;
    // Si no, el nodo fue añadido al grafo en memoria y `Result` es el puntero correcto.
  end;
end;

procedure TAiRagGraphBuilder.Process(AJsonTripletArray: string; AMergeStrategy: TMergeStrategy);
var
  JsonValue, TripletValue, PredicatePropsValue: TJSONValue;
  JsonArray: TJSONArray;
  TripletObject: TJSONObject;
  SubjectObj, PredicateObj, ObjectObj: TJSONObject;
  SubjectNode, ObjectNode: TAiRagGraphNode;
  EdgeLabel, EdgeName: string;
  PredicateProps: TJSONObject;
  ExistingEdge: TAiRagGraphEdge; // Variable para la reconciliación de aristas
begin
  if FGraph = nil then
    raise Exception.Create('Graph property is not assigned.');

  JsonValue := TJSONObject.ParseJSONValue(AJsonTripletArray);
  if not(JsonValue is TJSONArray) then
  begin
    JsonValue.Free;
    raise Exception.Create('Input JSON is not a valid JSON array of triplets.');
  end;

  JsonArray := JsonValue as TJSONArray;

  FGraph.BeginUpdate;
  Try
    try
      for TripletValue in JsonArray do
      begin
        if not(TripletValue is TJSONObject) then
          continue;

        TripletObject := TripletValue as TJSONObject;
        SubjectObj := TripletObject.GetValue<TJSONObject>('subject');
        PredicateObj := TripletObject.GetValue<TJSONObject>('predicate');
        ObjectObj := TripletObject.GetValue<TJSONObject>('object');

        // Si la tripleta no tiene predicado, podría ser solo una actualización de nodo
        if (SubjectObj <> nil) and (PredicateObj = nil) and (ObjectObj = nil) then
        begin
          GetOrCreateNode(SubjectObj, AMergeStrategy);
          continue; // Procesar la siguiente entrada en el JSON
        end;

        // Para una relación, los 3 componentes son necesarios
        if (SubjectObj = nil) or (PredicateObj = nil) or (ObjectObj = nil) then
          continue; // Tripleta mal formada, la ignoramos

        // 1. Obtener o crear los nodos
        SubjectNode := GetOrCreateNode(SubjectObj, AMergeStrategy);
        ObjectNode := GetOrCreateNode(ObjectObj, AMergeStrategy);

        // 2. Crear o actualizar la arista
        if (SubjectNode <> nil) and (ObjectNode <> nil) then
        begin
          EdgeLabel := PredicateObj.GetValue<string>('edgeLabel', 'related_to');
          EdgeName := PredicateObj.GetValue<string>('name', '');

          PredicateProps := nil;
          PredicatePropsValue := PredicateObj.FindValue('properties');
          if (PredicatePropsValue <> nil) and (PredicatePropsValue is TJSONObject) then
          begin
            PredicateProps := PredicatePropsValue as TJSONObject;
          end;

          // --- LÓGICA DE RECONCILIACIÓN DE ARISTAS ---
          ExistingEdge := FGraph.FindEdge(SubjectNode, ObjectNode, EdgeLabel);

          if ExistingEdge <> nil then
          begin
            // ARISTA ENCONTRADA: Fusionar propiedades
            if PredicateProps <> nil then
              MergeEdgeProperties(ExistingEdge, PredicateProps, AMergeStrategy);

            if (not EdgeName.IsEmpty) and (AMergeStrategy = msOverwrite) then
              ExistingEdge.Name := EdgeName;
          end
          else
          begin
            // ARISTA NO ENCONTRADA: Crear una nueva
            var
            NewEdge := FGraph.AddEdge(SubjectNode, ObjectNode, TGuid.NewGuid.ToString, EdgeLabel, EdgeName);

            if Assigned(FEmbeddings) then
            begin
              // El texto para el embedding de una relación es la tripleta misma.
              var
              TextToEmbed := SubjectNode.Name + ' ' + EdgeLabel + ' ' + ObjectNode.Name;
              NewEdge.Data := FEmbeddings.CreateEmbedding(TextToEmbed, '');
            end;

            if PredicateProps <> nil then
              MergeEdgeProperties(NewEdge, PredicateProps, msOverwrite);
          end;
        end;
      end;
    finally
      JsonValue.Free;
    end;
  Finally
    FGraph.EndUpdate;
  End;
end;

end.
