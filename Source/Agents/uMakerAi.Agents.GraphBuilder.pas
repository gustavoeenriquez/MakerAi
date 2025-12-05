unit uMakerAi.Agents.GraphBuilder;

interface

uses
  System.SysUtils, System.JSON, System.Generics.Collections, System.Classes, System.Math,
  uMakerAi.Agents;

type
  TGraphBuilder = class
  private
  // MEJORA: Constantes para evitar "Magic Strings" y errores de tipeo.
    const
    // Estructura principal
    cJsonNodes = 'nodes';
    cJsonEdges = 'edges';
    cJsonProperties = 'properties';

    // Propiedades de Nodos
    cJsonNodeId = 'id';
    cJsonNodeLabel = 'label';
    cJsonNodePorts = 'ports';
    cJsonToolClassName = 'toolClassName';
    cJsonParameters = 'parameters';
    cJsonMetadataDescription = 'metadata.description'; // Usamos punto para denotar anidamiento

    // Propiedades de Puertos
    cJsonPortInternalId = 'internalId';
    cJsonPortId = 'idStr';
    cJsonPortCategory = 'category';
    cJsonPortCategoryAccessory = 'accessory';

    // Propiedades de Aristas
    cJsonEdgeSourceNodeId = 'sourceNodeId';
    cJsonEdgeTargetNodeId = 'targetNodeId';
    cJsonEdgeSourceTerminal = 'sourceTerminal';
    cJsonEdgeDescription = 'description';

    // Propiedades del motor (Engine)
    cJsonEngine = 'engine';
    cJsonEngineJoinMode = 'joinMode';
    cJsonEngineLinkMode = 'linkMode';
    cJsonEngineLinkMaxCycles = 'linkMaxCycles';
    cJsonEngineLinkCondKey = 'linkConditionalKey';
    cJsonEngineLinkManualKey = 'linkManualTargetsKey';
    cJsonEngineLinkExprA = 'linkExpressionA';
    cJsonEngineLinkExprB = 'linkExpressionB';
    cJsonEngineLinkExprC = 'linkExpressionC';
    cJsonEngineLinkExprD = 'linkExpressionD';

    // Puertos especiales
    cPortOutFailure = 'out_failure';

    cJsonMetadata = 'metadata'; // <-- Nueva constante
    cJsonDescription = 'description'; // <-- Nueva constante

  private
    FAgents: TAIAgents;
    FJsonGraph: TJSONObject;
    FNodeMap: TDictionary<string, TAIAgentsNode>;
    FNodeJsonMap: TDictionary<string, TJSONObject>;

    procedure ParseNodes;
    procedure ParseEdges;
    procedure SetToolParameters(ATool: TAiToolBase; AParamsJson: TJSONObject);
    function FindPortJsonByTerminalId(ANodeJson: TJSONObject; const APortTerminalId: string): TJSONObject;
    function FindEngineObject(JsonObj: TJSONObject): TJSONObject;

  public
    constructor Create(AAgents: TAIAgents);
    destructor Destroy; override;

    procedure BuildFromJson(const AJsonString: string);
  end;

implementation

uses
  System.Rtti, System.TypInfo,
  uMakerAi.Agents.EngineRegistry;

{ TGraphBuilder }

constructor TGraphBuilder.Create(AAgents: TAIAgents);
begin
  inherited Create;
  if not Assigned(AAgents) then
    raise Exception.Create('TGraphBuilder requires a valid TAIAgents instance.');
  FAgents := AAgents;
  FNodeMap := TDictionary<string, TAIAgentsNode>.Create;
  FNodeJsonMap := TDictionary<string, TJSONObject>.Create;
end;

destructor TGraphBuilder.Destroy;
begin
  FNodeMap.Free;
  FNodeJsonMap.Free;
  inherited;
end;

procedure TGraphBuilder.BuildFromJson(const AJsonString: string);
begin
  FJsonGraph := TJSONObject.ParseJSONValue(AJsonString) as TJSONObject;
  if not Assigned(FJsonGraph) then
    raise Exception.Create('Invalid JSON format for graph definition.');

  try
    FAgents.ClearGraph;
    FNodeMap.Clear;
    FNodeJsonMap.Clear;

    ParseNodes;
    ParseEdges;
    FAgents.Compile;

  finally
    FJsonGraph.Free;
    FJsonGraph := nil; // Buena práctica para evitar punteros colgantes
  end;
end;

// MEJORA: Se mantiene la función original, pero se añade un comentario sobre su naturaleza.
// Esta función realiza una búsqueda recursiva profunda para encontrar un objeto con la clave 'engine'.
// Es robusta si la ubicación del objeto 'engine' no es fija.
// Si la estructura del JSON fuera constante (ej: 'properties.engine'),
// una búsqueda directa sería más eficiente.
function TGraphBuilder.FindEngineObject(JsonObj: TJSONObject): TJSONObject;
var
  Pair: TJSONPair;
  Value: TJSONValue;
  I: Integer;
begin
  Result := nil;
  if JsonObj = nil then
    Exit;

  for Pair in JsonObj do
  begin
    if SameText(Pair.JsonString.Value, cJsonEngine) and (Pair.JsonValue is TJSONObject) then
      Exit(TJSONObject(Pair.JsonValue));

    Value := Pair.JsonValue;

    if Value is TJSONObject then
    begin
      Result := FindEngineObject(TJSONObject(Value));
      if Assigned(Result) then
        Exit;
    end
    else if Value is TJSONArray then
    begin
      for I := 0 to TJSONArray(Value).Count - 1 do
      begin
        if TJSONArray(Value).Items[I] is TJSONObject then
        begin
          Result := FindEngineObject(TJSONObject(TJSONArray(Value).Items[I]));
          if Assigned(Result) then
            Exit;
        end;
      end;
    end;
  end;
end;

procedure TGraphBuilder.ParseNodes;
var
  LNodesArray: TJSONArray;
  LNodeJson, LPropertiesJson, LParametersJson, LEngineJson, LMetadataJson: TJSONObject;
  LNode: TAIAgentsNode;
  LToolClassName, LJoinModeStr, LDescription, LNodeGuid, LLabel: string;
  LToolClass: TClass;
  LTool: TAiToolBase;
  LJoinModeOrdinal: Integer;
begin
  LNodesArray := FJsonGraph.GetValue<TJSONArray>(cJsonNodes);
  if not Assigned(LNodesArray) then
    raise Exception.CreateFmt('JSON graph must contain a "%s" array.', [cJsonNodes]);

  for var LJsonValue in LNodesArray do
  begin
    LNodeJson := LJsonValue as TJSONObject;
    LNodeGuid := LNodeJson.GetValue<string>(cJsonNodeId);
    LLabel := LNodeJson.GetValue<string>(cJsonNodeLabel, 'Node_' + Copy(LNodeGuid, 2, 8));

    LLabel := StringReplace(LLabel, ' ', '', [rfReplaceAll]); // Los nombres no pueden tener espacios

    // 1. Crear la instancia del nodo
    LNode := TAIAgentsNode.Create(FAgents);

    // 2. Asignar propiedades fundamentales
    LNode.ID := LNodeGuid; // Asignar el GUID original a la propiedad ID existente
    LNode.Name := LLabel;
    LNode.Graph := FAgents;
    LNode.JoinMode := jmAny;
    LTool := nil; // Buena práctica inicializar la referencia

    // 3. Procesar las propiedades del JSON
    if LNodeJson.TryGetValue<TJSONObject>(cJsonProperties, LPropertiesJson) then
    begin
      // 3.1. Leer la descripción (de forma anidada y segura)
      if LPropertiesJson.TryGetValue<TJSONObject>(cJsonMetadata, LMetadataJson) then
        LDescription := LMetadataJson.GetValue<string>(cJsonDescription, '')
      else
        LDescription := '';
      LNode.Description := LDescription;

      // 3.2. Buscar el objeto 'engine' para configurar JoinMode
      LEngineJson := FindEngineObject(LPropertiesJson);
      if Assigned(LEngineJson) then
      begin
        LJoinModeStr := LEngineJson.GetValue<string>(cJsonEngineJoinMode, 'jmAny');
        LJoinModeOrdinal := GetEnumValue(TypeInfo(TJoinMode), LJoinModeStr);
        if LJoinModeOrdinal <> -1 then
          LNode.JoinMode := TJoinMode(LJoinModeOrdinal);
      end;

      // 3.3. Crear y configurar la herramienta (Tool) asociada
      LToolClassName := LPropertiesJson.GetValue<string>(cJsonToolClassName,'');
      if not LToolClassName.IsEmpty then
      begin
        LToolClass := TEngineRegistry.Instance.FindToolClass(LToolClassName);
        if Assigned(LToolClass) then
        begin
          LTool := TAiToolBase(TComponentClass(LToolClass).Create(LNode));
          LNode.Tool := LTool;
          LTool.Description := LDescription; // Asignar la descripción también a la herramienta

          // Asignar parámetros a la herramienta usando RTTI
          if LPropertiesJson.TryGetValue<TJSONObject>(cJsonParameters, LParametersJson) then
            SetToolParameters(LTool, LParametersJson);
        end
        else
          raise Exception.CreateFmt('Tool class "%s" not found or not registered.', [LToolClassName]);
      end;

      // 3.4. Identificar nodos especiales (Start/End)
      if SameText(LToolClassName, 'TStartTool') then
        FAgents.StartNode := LNode;
      if SameText(LToolClassName, 'TEndTool') then
        FAgents.EndNode := LNode;
    end;

    // 4. Registrar el nodo en los diccionarios para el paso de 'ParseEdges'
    FNodeMap.Add(LNodeGuid, LNode);
    FNodeJsonMap.Add(LNodeGuid, LNodeJson);
  end;
end;


procedure TGraphBuilder.ParseEdges;
var
  LEdgesArray: TJSONArray;
  LEdgeJson, LSourceNodeJson, LSourcePortJson, LPropertiesJson, LEngineJson: TJSONObject;
  LSourceNode, LTargetNode: TAIAgentsNode;
  LSourceNodeId, LTargetNodeId, LSourceTerminalId, LSourcePortId, LSourcePortCategory: string;
  LLink: TAIAgentsLink;
  LLinkModeStr: string;
  LLinkModeOrdinal: Integer;
begin
  LEdgesArray := FJsonGraph.GetValue<TJSONArray>(cJsonEdges);
  if not Assigned(LEdgesArray) then
    Exit;

  for var LJsonValue in LEdgesArray do
  begin
    LEdgeJson := LJsonValue as TJSONObject;
    LSourceNodeId := LEdgeJson.GetValue<string>(cJsonEdgeSourceNodeId);
    LTargetNodeId := LEdgeJson.GetValue<string>(cJsonEdgeTargetNodeId);
    LSourceTerminalId := LEdgeJson.GetValue<string>(cJsonEdgeSourceTerminal);

    if not(FNodeMap.TryGetValue(LSourceNodeId, LSourceNode) and FNodeMap.TryGetValue(LTargetNodeId, LTargetNode)) then
      Continue; // Or raise an error for an edge pointing to a non-existent node

    if not FNodeJsonMap.TryGetValue(LSourceNodeId, LSourceNodeJson) then
      Continue;

    LSourcePortJson := FindPortJsonByTerminalId(LSourceNodeJson, LSourceTerminalId);
    if not Assigned(LSourcePortJson) then
      Continue;

    LSourcePortCategory := LSourcePortJson.GetValue<string>(cJsonPortCategory, 'tool');
    if SameText(LSourcePortCategory, cJsonPortCategoryAccessory) then
      Continue;

    // MEJORA: La lógica de creación del Link es más explícita.
    // Se crea un único Link por nodo de origen la primera vez que se encuentra una arista saliente.
    LLink := LSourceNode.Next;
    if not Assigned(LLink) then
    begin
      LLink := TAIAgentsLink.Create(FAgents);
      LLink.Graph := FAgents;
      LLink.Name := 'Link_From_' + LSourceNode.Name;
      LSourceNode.Next := LLink;

      // Las propiedades del Link se leen del objeto 'engine' del NODO DE ORIGEN.
      LEngineJson := FindEngineObject(LSourceNodeJson);
      if Assigned(LEngineJson) then
      begin
        LLinkModeStr := LEngineJson.GetValue<string>(cJsonEngineLinkMode, 'lmFanout');
        LLinkModeOrdinal := GetEnumValue(TypeInfo(TLinkMode), LLinkModeStr);
        if LLinkModeOrdinal <> -1 then
          LLink.Mode := TLinkMode(LLinkModeOrdinal);

        LLink.MaxCycles := LEngineJson.GetValue<Integer>(cJsonEngineLinkMaxCycles, 1);
        LLink.ConditionalKey := LEngineJson.GetValue<string>(cJsonEngineLinkCondKey, 'next_route');
        LLink.ManualTargetsKey := LEngineJson.GetValue<string>(cJsonEngineLinkManualKey, 'next_targets');
        LLink.ExpressionA := LEngineJson.GetValue<string>(cJsonEngineLinkExprA, '');
        LLink.ExpressionB := LEngineJson.GetValue<string>(cJsonEngineLinkExprB, '');
        LLink.ExpressionC := LEngineJson.GetValue<string>(cJsonEngineLinkExprC, '');
        LLink.ExpressionD := LEngineJson.GetValue<string>(cJsonEngineLinkExprD, '');
      end;

      if LEdgeJson.TryGetValue<TJSONObject>(cJsonProperties, LPropertiesJson) then
      begin
        LLink.Description := LPropertiesJson.GetValue<string>(cJsonEdgeDescription, '');
      end;
    end;

    LSourcePortId := LSourcePortJson.GetValue<string>(cJsonPortId, LSourceTerminalId);

    if LLink.Mode = lmConditional then
    begin
      LLink.AddConditionalTarget(LSourcePortId, LTargetNode);
    end
    else
    begin
      if SameText(LSourcePortId, cPortOutFailure) then
      begin
        LLink.NextNo := LTargetNode;
      end
      else
      begin
        // MEJORA: Lógica de asignación a NextA/B/C/D simplificada y menos repetitiva.
        var
          LNextSlots: array [0 .. 3] of PPointer;
        LNextSlots[0] := @LLink.NextA;
        LNextSlots[1] := @LLink.NextB;
        LNextSlots[2] := @LLink.NextC;
        LNextSlots[3] := @LLink.NextD;

        var
        LAssigned := False;
        for var I := 0 to High(LNextSlots) do
        begin
          if LNextSlots[I]^ = nil then
          begin
            LNextSlots[I]^ := LTargetNode;
            LAssigned := True;
            break;
          end;
        end;

        if not LAssigned then
          LSourceNode.Print(Format('Warning: More than %d standard output ports connected from node %s. Connection to %s ignored.', [Length(LNextSlots), LSourceNode.Name, LTargetNode.Name]));
      end;
    end;
  end;
end;

procedure TGraphBuilder.SetToolParameters(ATool: TAiToolBase; AParamsJson: TJSONObject);
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiProp: TRttiProperty;
  LParamPair: TJSONPair;
  LParamValue: TJSONValue;
begin
  LRttiContext := TRttiContext.Create;
  try
    LRttiType := LRttiContext.GetType(ATool.ClassType);
    for LParamPair in AParamsJson do
    begin
      LRttiProp := LRttiType.GetProperty(LParamPair.JsonString.Value);
      if Assigned(LRttiProp) and LRttiProp.IsWritable then
      begin
        LParamValue := LParamPair.JsonValue;

        case LRttiProp.PropertyType.TypeKind of
          tkString, tkUString:
            LRttiProp.SetValue(ATool, LParamValue.Value);
          tkInteger, tkInt64:
            LRttiProp.SetValue(ATool, StrToIntDef(LParamValue.Value, 0));
          tkFloat:
            LRttiProp.SetValue(ATool, StrToFloatDef(LParamValue.Value, 0.0, TFormatSettings.Invariant));
          tkEnumeration:
            begin
              if LRttiProp.PropertyType.Handle = TypeInfo(Boolean) then
              begin
                var
                LBoolValue := False;
                if LParamValue is TJSONTrue then
                  LBoolValue := True
                else if LParamValue is TJSONFalse then
                  LBoolValue := False
                else
                  LBoolValue := SameText(LParamValue.Value, 'true');
                LRttiProp.SetValue(ATool, TValue.From<Boolean>(LBoolValue));
              end
              else
              begin
                var
                LOrdinalValue := GetEnumValue(LRttiProp.PropertyType.Handle, LParamValue.Value);
                if LOrdinalValue <> -1 then
                begin
                  var
                  LEnumValue := TValue.FromOrdinal(LRttiProp.PropertyType.Handle, LOrdinalValue);
                  if not LEnumValue.IsEmpty then
                    LRttiProp.SetValue(ATool, LEnumValue);
                end;
              end;
            end;
        end;
      end;
    end;
  finally
    LRttiContext.Free;
  end;
end;

function TGraphBuilder.FindPortJsonByTerminalId(ANodeJson: TJSONObject; const APortTerminalId: string): TJSONObject;
var
  LPortsArray: TJSONArray;
  LPortJson: TJSONObject;
begin
  Result := nil;
  if not Assigned(ANodeJson) then
    Exit;
  if not ANodeJson.TryGetValue<TJSONArray>(cJsonNodePorts, LPortsArray) then
    Exit;

  for var LJsonValue in LPortsArray do
  begin
    LPortJson := LJsonValue as TJSONObject;
    if SameText(LPortJson.GetValue<string>(cJsonPortInternalId), APortTerminalId) then
    begin
      Result := LPortJson;
      Exit;
    end;
  end;
end;

end.
