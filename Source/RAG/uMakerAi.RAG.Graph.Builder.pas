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
    procedure SetGraph(const Value: TAiRagGraph);
  protected
    procedure MergeNodeProperties(ANode: TAiRagGraphNode; ANewProperties: TJSONObject; AStrategy: TMergeStrategy);
    procedure MergeEdgeProperties(AEdge: TAiRagGraphEdge; ANewProperties: TJSONObject; AStrategy: TMergeStrategy);
    function GetOrCreateNode(ANodeObject: TJSONObject; AMergeStrategy: TMergeStrategy): TAiRagGraphNode;
    function GenerateTextForEmbedding(AName, ANodeLabel: string; AProperties: TJSONObject; AAdditionalText: String = ''): string; overload;
    function GenerateTextForEmbedding(AName, ANodeLabel: string; AProperties: TDictionary<string, Variant>; AAdditionalText: String = ''): string; overload;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Process(AJsonTripletArray: string; AMergeStrategy: TMergeStrategy = msAddNewOnly);
    function FindExistingNode(AName, ALabel: string): TAiRagGraphNode;

  published
    property Graph: TAiRagGraph read FGraph write SetGraph;
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

procedure TAiRagGraphBuilder.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FGraph) then
    FGraph := nil;
end;

function TAiRagGraphBuilder.GenerateTextForEmbedding(AName, ANodeLabel: string; AProperties: TJSONObject; AAdditionalText: String = ''): string;
var
  ContextBuilder: TStringBuilder;
  Pair: TJSONPair;
  PropertyValue: string;
begin
  ContextBuilder := TStringBuilder.Create;
  try
    // 1. INICIO CLARO: Identificación de la entidad
    // Formato repetitivo ayuda al modelo a entender la estructura
    ContextBuilder.AppendFormat('Entidad: %s. Categoría: %s.', [AName, ANodeLabel]);

    // 2. PROPIEDADES como oraciones completas y naturales
    // Cada propiedad es una frase independiente (mejor para chunking semántico)
    if Assigned(AProperties) then
    begin
      for Pair in AProperties do
      begin
        // Convertimos el valor JSON a string limpio
        PropertyValue := Pair.JsonValue.Value;

        // Formato natural: "La edad es 30." en vez de "edad es 30."
        // Usamos artículo determinado para mayor naturalidad
        ContextBuilder.AppendFormat(' La propiedad %s tiene el valor %s.', [Pair.JsonString.Value, PropertyValue]);
      end;
    end;

    // 3. CONTENIDO ADICIONAL sin prefijos
    // El texto adicional ya debería ser autoexplicativo
    if not AAdditionalText.Trim.IsEmpty then
    begin
      ContextBuilder.Append(' ');
      ContextBuilder.Append(AAdditionalText.Trim);

      // Aseguramos terminación con punto para coherencia
      if not AAdditionalText.Trim.EndsWith('.') then
        ContextBuilder.Append('.');
    end;

    Result := ContextBuilder.ToString;
  finally
    ContextBuilder.Free;
  end;
end;

function TAiRagGraphBuilder.GenerateTextForEmbedding(AName, ANodeLabel: string; AProperties: TDictionary<string, Variant>; AAdditionalText: String): string;
var
  ContextBuilder: TStringBuilder;
  Pair: TPair<string, Variant>;
begin
  ContextBuilder := TStringBuilder.Create;
  try
    ContextBuilder.AppendFormat('Entidad: %s. Tipo: %s. ', [AName, ANodeLabel]);

    if Assigned(AProperties) then
    begin
      for Pair in AProperties do
      begin
        if not SameText(Pair.Key, 'name') then
          ContextBuilder.AppendFormat('%s: %s. ', [Pair.Key, VarToStr(Pair.Value)]);
      end;
    end;

    if not AAdditionalText.IsEmpty then
      ContextBuilder.Append(AAdditionalText);

    Result := ContextBuilder.ToString;
  finally
    ContextBuilder.Free;
  end;
end;

function TAiRagGraphBuilder.GetOrCreateNode(ANodeObject: TJSONObject; AMergeStrategy: TMergeStrategy): TAiRagGraphNode;
var
  NodeName, NodeLabel, NewNodeID, NodeText, AdditionalText: string;
  NewProperties: TJSONObject;
  NeedEmbeddingUpdate: Boolean;
  LEmbeddings: TAiEmbeddingsCore;
begin
  if FGraph = nil then
    raise Exception.Create('Graph property is not assigned to the builder.');

  LEmbeddings := FGraph.Embeddings;

  // 1. Datos básicos
  NodeName := ANodeObject.GetValue<string>('name', '');
  NodeLabel := ANodeObject.GetValue<string>('nodeLabel', 'Undefined');

  if NodeName.Trim.IsEmpty then
    raise Exception.Create('Node name cannot be empty.');

  // 2. Intentar buscar si ya existe
  Result := FGraph.FindNodeByName(NodeName, NodeLabel);

  NewProperties := ANodeObject.GetValue<TJSONObject>('properties', nil);
  AdditionalText := ANodeObject.GetValue<string>('text', '');

  if Result <> nil then
  begin
    // --- CASO NODO EXISTENTE ---
    // Aquí el nodo ya está persistido, actualizamos y el Core se encargará de sincronizar
    NeedEmbeddingUpdate := (LEmbeddings <> nil) and (Length(Result.Data) = 0);

    if Assigned(NewProperties) then
    begin
      MergeNodeProperties(Result, NewProperties, AMergeStrategy);
      NeedEmbeddingUpdate := True;
    end;

    if not AdditionalText.Trim.IsEmpty then
      NeedEmbeddingUpdate := True;

    if NeedEmbeddingUpdate then
    begin
      NodeText := GenerateTextForEmbedding(Result.Name, Result.NodeLabel, Result.Properties, AdditionalText);
      Result.Text := NodeText;
      if LEmbeddings <> nil then
      begin
        Result.Model := LEmbeddings.Model;
        Result.Data := LEmbeddings.CreateEmbedding(NodeText, 'user');
      end;
      // El Core o el Driver deberían manejar el UPDATE aquí si es necesario
    end;
  end
  else
  begin
    // --- CASO NODO NUEVO (Optimizado) ---
    // 1. Creamos el objeto "huérfano" (Sin persistir)
    Result := FGraph.NewNode(TGuid.NewGuid.ToString, NodeLabel, NodeName);

    // 2. Rellenamos datos en memoria
    if Assigned(NewProperties) then
      MergeNodeProperties(Result, NewProperties, msOverwrite);

    NodeText := GenerateTextForEmbedding(Result.Name, Result.NodeLabel, Result.Properties, AdditionalText);
    Result.Text := NodeText;

    if LEmbeddings <> nil then
    begin
      Result.Model := LEmbeddings.Model;
      Result.Data := LEmbeddings.CreateEmbedding(NodeText, 'user');
    end;

    // 3. AHORA lo registramos y persistimos (Un solo INSERT con todo lleno)
    FGraph.AddNode(Result);
  end;
end;

procedure TAiRagGraphBuilder.Process(AJsonTripletArray: string;
  AMergeStrategy: TMergeStrategy);
var
  JsonValue, TripletValue: TJSONValue;
  JsonArray: TJSONArray;
  TripletObject: TJSONObject;
  SubjectObj, PredicateObj, ObjectObj: TJSONObject;
  SubjectNode, ObjectNode: TAiRagGraphNode;
  EdgeLabel, EdgeName, TextToEmbed: string;
  PredicateProps: TJSONObject;
  ExistingEdge: TAiRagGraphEdge;
  NewEdge: TAiRagGraphEdge;
  LEmbeddings: TAiEmbeddingsCore;
  NeedEmbeddingUpdate: Boolean;
begin
  if FGraph = nil then
    raise Exception.Create('Graph property is not assigned to the builder.');

  // 1. Obtener motor de embeddings del grafo
  LEmbeddings := FGraph.Embeddings;

  // 2. Parseo del JSON
  JsonValue := TJSONObject.ParseJSONValue(AJsonTripletArray);
  try
    if not (JsonValue is TJSONArray) then
      raise Exception.Create('Input JSON is not a valid JSON array of triplets.');

    JsonArray := JsonValue as TJSONArray;

    FGraph.BeginUpdate;
    try
      for TripletValue in JsonArray do
      begin
        if not (TripletValue is TJSONObject) then
          continue;

        TripletObject := TripletValue as TJSONObject;

        // Extraer componentes de la tripleta
        SubjectObj := TripletObject.GetValue<TJSONObject>('subject', nil);
        PredicateObj := TripletObject.GetValue<TJSONObject>('predicate', nil);
        ObjectObj := TripletObject.GetValue<TJSONObject>('object', nil);

        // CASO A: Solo actualización de Nodo (Sujeto sin predicado)
        if (SubjectObj <> nil) and (PredicateObj = nil) and (ObjectObj = nil) then
        begin
          GetOrCreateNode(SubjectObj, AMergeStrategy);
          continue;
        end;

        // Validar integridad de la tripleta para relaciones
        if (SubjectObj = nil) or (PredicateObj = nil) or (ObjectObj = nil) then
          continue;

        // 3. Procesar Nodos (ya optimizados con NewNode)
        SubjectNode := GetOrCreateNode(SubjectObj, AMergeStrategy);
        ObjectNode := GetOrCreateNode(ObjectObj, AMergeStrategy);

        if (SubjectNode <> nil) and (ObjectNode <> nil) then
        begin
          // Extraer datos del predicado
          EdgeLabel := PredicateObj.GetValue<string>('edgeLabel', 'related_to');
          EdgeName := PredicateObj.GetValue<string>('name', '');
          PredicateProps := PredicateObj.GetValue<TJSONObject>('properties', nil);

          // 4. LÓGICA DE RECONCILIACIÓN DE ARISTAS
          ExistingEdge := FGraph.FindEdge(SubjectNode, ObjectNode, EdgeLabel);

          if ExistingEdge <> nil then
          begin
            // --- ARISTA EXISTENTE ---
            NeedEmbeddingUpdate := False;

            // Fusionar propiedades
            if PredicateProps <> nil then
            begin
              MergeEdgeProperties(ExistingEdge, PredicateProps, AMergeStrategy);
              NeedEmbeddingUpdate := True; // Las propiedades cambiaron
            end;

            // Actualizar nombre si la estrategia lo permite
            if (not EdgeName.IsEmpty) and (AMergeStrategy = msOverwrite) then
            begin
              ExistingEdge.Name := EdgeName;
              NeedEmbeddingUpdate := True;
            end;

            // REPARACIÓN: Si hay motor y la arista NO tiene vector, regenerarlo
            if (LEmbeddings <> nil) and (Length(ExistingEdge.Data) = 0) then
              NeedEmbeddingUpdate := True;

            // Regenerar embedding si es necesario
            if NeedEmbeddingUpdate and (LEmbeddings <> nil) then
            begin
              TextToEmbed := SubjectNode.Name + ' ' + EdgeLabel + ' ' + ObjectNode.Name;
              ExistingEdge.Data := LEmbeddings.CreateEmbedding(TextToEmbed, '');
            end;

            // CRÍTICO: Si tu Core no sincroniza automáticamente, forzar UPDATE
            // Descomenta si es necesario:
            // if NeedEmbeddingUpdate then
            //   FGraph.UpdateEdge(ExistingEdge);
          end
          else
          begin
            // --- ARISTA NUEVA (Optimizado) ---
            // 1. Crear objeto arista en memoria (sin persistir)
            NewEdge := FGraph.NewEdge(SubjectNode, ObjectNode,
                                      TGuid.NewGuid.ToString,
                                      EdgeLabel,
                                      EdgeName);

            // 2. Aplicar propiedades en memoria
            if PredicateProps <> nil then
              MergeEdgeProperties(NewEdge, PredicateProps, msOverwrite);

            // 3. Generar embedding si hay motor configurado
            if LEmbeddings <> nil then
            begin
              TextToEmbed := SubjectNode.Name + ' ' + EdgeLabel + ' ' + ObjectNode.Name;
              NewEdge.Data := LEmbeddings.CreateEmbedding(TextToEmbed, '');
            end;

            // 4. AHORA persistir con todos los datos completos (1 INSERT)
            FGraph.AddEdge(NewEdge);
          end;
        end;
      end;
    finally
      FGraph.EndUpdate;
    end;
  finally
    JsonValue.Free;
  end;
end;

procedure TAiRagGraphBuilder.SetGraph(const Value: TAiRagGraph);
begin
  if FGraph <> Value then
  begin
    FGraph := Value;
    if FGraph <> nil then
      FGraph.FreeNotification(Self);
  end;
end;


end.
