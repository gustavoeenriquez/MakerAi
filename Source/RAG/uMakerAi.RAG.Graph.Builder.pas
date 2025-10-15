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


unit uMakerAi.RAG.Graph.Builder;

interface

uses
  System.SysUtils, System.Classes, System.Json, System.Generics.Collections,
  System.Variants, uMakerAi.RAG.Vectors, uMakerAi.Embeddings.core, uMakerAi.Embeddings,
  uMakerAi.RAG.Graph.core; // Incluimos la unidad del grafo

type
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
  PropertiesValue: TJSONValue;
  NewProperties: TJSONObject;
begin
  NodeName := ANodeObject.GetValue<string>('name', '');
  NodeLabel := ANodeObject.GetValue<string>('nodeLabel', 'Undefined');

  if NodeName.IsEmpty then
    raise Exception.Create('Node name cannot be empty in JSON input.');

  // 1. Intenta encontrar el nodo existente. Esta es la parte crucial que ahora funcionará.
  Result := FGraph.FindNodeByName(NodeName, NodeLabel);

  NewProperties := nil;
  PropertiesValue := ANodeObject.FindValue('properties');
  if (PropertiesValue <> nil) and (PropertiesValue is TJSONObject) then
    NewProperties := PropertiesValue as TJSONObject;

  if Result <> nil then
  begin
    // Nodo encontrado: Simplemente fusiona las propiedades.
    if NewProperties <> nil then
      MergeNodeProperties(Result, NewProperties, AMergeStrategy);
  end
  else
  begin
    // Nodo no encontrado: Créalo y añádelo.
    NewNodeID := TGuid.NewGuid.ToString;

    // Dejamos que el grafo cree y registre el nodo.
    Result := FGraph.AddNode(NewNodeID, NodeLabel, NodeName);

    // Ahora poblamos las propiedades y el embedding en el nodo recién creado.
    if NewProperties <> nil then
      MergeNodeProperties(Result, NewProperties, msOverwrite); // Al crear, siempre sobrescribimos las propiedades iniciales.

    if Assigned(FEmbeddings) then
    begin
      AdditionalText := ANodeObject.GetValue<string>('text', '');
      NodeText := GenerateTextForEmbedding(Result.Name, Result.NodeLabel, NewProperties, AdditionalText);
      Result.Text := NodeText; // Guardamos el texto usado para el embedding
      Result.Data := FEmbeddings.CreateEmbedding(NodeText, 'user');
    end;
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
