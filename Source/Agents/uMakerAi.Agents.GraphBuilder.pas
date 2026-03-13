// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Constructor de grafos de agentes desde definiciones JSON.
//
// Adaptaciones FPC:
//   - TJSONObject.ParseJSONValue → GetJSON() + cast
//   - .GetValue<T>(key, default)  → .Get(key, default) / .Find(key)
//   - .TryGetValue<T>(key, out v) → local helper TryGetObj/TryGetArr
//   - System.Rtti (SetToolParameters) → TypInfo (GetPropInfo, Set*Prop)
//   - Inline var declarations → pre-declaradas
//   - for var X in Collection   → índice explícito / enumerador

unit uMakerAi.Agents.GraphBuilder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  generics.collections,
  fpjson, jsonparser,
  uMakerAi.Agents;

type
  TGraphBuilder = class
  private
    const
      // Estructura principal
      cJsonNodes    = 'nodes';
      cJsonEdges    = 'edges';
      cJsonProperties = 'properties';

      // Propiedades de Nodos
      cJsonNodeId    = 'id';
      cJsonNodeLabel = 'label';
      cJsonNodePorts = 'ports';
      cJsonToolClassName = 'toolClassName';
      cJsonParameters    = 'parameters';

      // Propiedades de Puertos
      cJsonPortInternalId = 'internalId';
      cJsonPortId         = 'idStr';
      cJsonPortCategory   = 'category';
      cJsonPortCategoryAccessory = 'accessory';

      // Propiedades de Aristas
      cJsonEdgeSourceNodeId  = 'sourceNodeId';
      cJsonEdgeTargetNodeId  = 'targetNodeId';
      cJsonEdgeSourceTerminal = 'sourceTerminal';
      cJsonEdgeDescription   = 'description';

      // Objeto Engine
      cJsonEngine               = 'engine';
      cJsonEngineJoinMode       = 'joinMode';
      cJsonEngineLinkMode       = 'linkMode';
      cJsonEngineLinkMaxCycles  = 'linkMaxCycles';
      cJsonEngineLinkCondKey    = 'linkConditionalKey';
      cJsonEngineLinkManualKey  = 'linkManualTargetsKey';
      cJsonEngineLinkExprA      = 'linkExpressionA';
      cJsonEngineLinkExprB      = 'linkExpressionB';
      cJsonEngineLinkExprC      = 'linkExpressionC';
      cJsonEngineLinkExprD      = 'linkExpressionD';

      // Puertos especiales
      cPortOutFailure = 'out_failure';

      cJsonMetadata   = 'metadata';
      cJsonDescription = 'description';

  private
    FAgents:      TAIAgents;
    FJsonGraph:   TJSONObject;
    FNodeMap:     specialize TDictionary<string, TAIAgentsNode>;
    FNodeJsonMap: specialize TDictionary<string, TJSONObject>;

    procedure ParseNodes;
    procedure ParseEdges;
    procedure SetToolParameters(ATool: TAiToolBase; AParamsJson: TJSONObject);
    function  FindPortJsonByTerminalId(ANodeJson: TJSONObject;
                const APortTerminalId: string): TJSONObject;
    function  FindEngineObject(JsonObj: TJSONObject): TJSONObject;

  public
    constructor Create(AAgents: TAIAgents);
    destructor  Destroy; override;

    procedure BuildFromJson(const AJsonString: string);
  end;

implementation

uses
  TypInfo,
  uMakerAi.Agents.EngineRegistry;

{ TGraphBuilder }

constructor TGraphBuilder.Create(AAgents: TAIAgents);
begin
  inherited Create;
  if not Assigned(AAgents) then
    raise Exception.Create('TGraphBuilder requires a valid TAIAgents instance.');
  FAgents      := AAgents;
  FNodeMap     := specialize TDictionary<string, TAIAgentsNode>.Create;
  FNodeJsonMap := specialize TDictionary<string, TJSONObject>.Create;
end;

destructor TGraphBuilder.Destroy;
begin
  FNodeMap.Free;
  FNodeJsonMap.Free;
  inherited;
end;

procedure TGraphBuilder.BuildFromJson(const AJsonString: string);
var
  LData: TJSONData;
begin
  LData := GetJSON(AJsonString);
  if not (LData is TJSONObject) then
  begin
    LData.Free;
    raise Exception.Create('Invalid JSON format for graph definition.');
  end;

  FJsonGraph := TJSONObject(LData);
  try
    FAgents.ClearGraph;
    FNodeMap.Clear;
    FNodeJsonMap.Clear;

    ParseNodes;
    ParseEdges;
    FAgents.Compile;
  finally
    FreeAndNil(FJsonGraph);
  end;
end;

// Busqueda recursiva del objeto 'engine' dentro de cualquier profundidad de JSON
function TGraphBuilder.FindEngineObject(JsonObj: TJSONObject): TJSONObject;
var
  I:     Integer;
  Val:   TJSONData;
  Sub:   TJSONObject;
  ArrI:  Integer;
begin
  Result := nil;
  if JsonObj = nil then Exit;

  for I := 0 to JsonObj.Count - 1 do
  begin
    if SameText(JsonObj.Names[I], cJsonEngine) and
       (JsonObj.Items[I] is TJSONObject) then
    begin
      Result := TJSONObject(JsonObj.Items[I]);
      Exit;
    end;

    Val := JsonObj.Items[I];

    if Val is TJSONObject then
    begin
      Sub := FindEngineObject(TJSONObject(Val));
      if Assigned(Sub) then
      begin
        Result := Sub;
        Exit;
      end;
    end
    else if Val is TJSONArray then
    begin
      for ArrI := 0 to TJSONArray(Val).Count - 1 do
      begin
        if TJSONArray(Val).Items[ArrI] is TJSONObject then
        begin
          Sub := FindEngineObject(TJSONObject(TJSONArray(Val).Items[ArrI]));
          if Assigned(Sub) then
          begin
            Result := Sub;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

procedure TGraphBuilder.ParseNodes;
var
  LNodesArray:     TJSONArray;
  LNodeJson:       TJSONObject;
  LPropertiesJson: TJSONObject;
  LParametersJson: TJSONObject;
  LEngineJson:     TJSONObject;
  LMetadataJson:   TJSONObject;
  LNode:           TAIAgentsNode;
  LToolClassName:  string;
  LJoinModeStr:    string;
  LDescription:    string;
  LNodeGuid:       string;
  LLabel:          string;
  LToolClass:      TClass;
  LTool:           TAiToolBase;
  LJoinModeOrd:    Integer;
  LPropData:       TJSONData;
  I:               Integer;
begin
  LNodesArray := FJsonGraph.Find(cJsonNodes) as TJSONArray;
  if not Assigned(LNodesArray) then
    raise Exception.CreateFmt('JSON graph must contain a "%s" array.', [cJsonNodes]);

  for I := 0 to LNodesArray.Count - 1 do
  begin
    LNodeJson := LNodesArray.Items[I] as TJSONObject;
    LNodeGuid := LNodeJson.Get(cJsonNodeId, '');
    LLabel    := LNodeJson.Get(cJsonNodeLabel,
                   'Node_' + Copy(LNodeGuid, 2, 8));

    // Los nombres no pueden tener espacios
    LLabel := StringReplace(LLabel, ' ', '', [rfReplaceAll]);

    LNode := TAIAgentsNode.Create(FAgents);
    LNode.ID       := LNodeGuid;
    LNode.Name     := LLabel;
    LNode.Graph    := TAIAgentManager(FAgents);
    LNode.JoinMode := jmAny;
    LTool          := nil;

    LPropData := LNodeJson.Find(cJsonProperties);
    if LPropData is TJSONObject then
    begin
      LPropertiesJson := TJSONObject(LPropData);

      // Descripcion (metadata.description)
      LDescription := '';
      LPropData := LPropertiesJson.Find(cJsonMetadata);
      if LPropData is TJSONObject then
      begin
        LMetadataJson := TJSONObject(LPropData);
        LDescription  := LMetadataJson.Get(cJsonDescription, '');
      end;
      LNode.Description := LDescription;

      // JoinMode desde objeto 'engine'
      LEngineJson := FindEngineObject(LPropertiesJson);
      if Assigned(LEngineJson) then
      begin
        LJoinModeStr := LEngineJson.Get(cJsonEngineJoinMode, 'jmAny');
        LJoinModeOrd := GetEnumValue(TypeInfo(TJoinMode), LJoinModeStr);
        if LJoinModeOrd >= 0 then
          LNode.JoinMode := TJoinMode(LJoinModeOrd);
      end;

      // Herramienta asociada
      LToolClassName := LPropertiesJson.Get(cJsonToolClassName, '');
      if LToolClassName <> '' then
      begin
        LToolClass := TEngineRegistry.Instance.FindToolClass(LToolClassName);
        if Assigned(LToolClass) then
        begin
          LTool := TAiToolBase(TComponentClass(LToolClass).Create(LNode));
          LNode.Tool := LTool;
          LTool.Description := LDescription;

          LPropData := LPropertiesJson.Find(cJsonParameters);
          if LPropData is TJSONObject then
            SetToolParameters(LTool, TJSONObject(LPropData));
        end
        else
          raise Exception.CreateFmt(
            'Tool class "%s" not found or not registered.', [LToolClassName]);
      end;

      // Nodos especiales Start / End
      if SameText(LToolClassName, 'TStartTool') then
        FAgents.StartNode := LNode;
      if SameText(LToolClassName, 'TEndTool') then
        FAgents.EndNode := LNode;
    end;

    FNodeMap.Add(LNodeGuid, LNode);
    FNodeJsonMap.Add(LNodeGuid, LNodeJson);
  end;
end;

procedure TGraphBuilder.ParseEdges;
var
  LEdgesArray:       TJSONArray;
  LEdgeJson:         TJSONObject;
  LSourceNodeJson:   TJSONObject;
  LSourcePortJson:   TJSONObject;
  LPropertiesJson:   TJSONObject;
  LEngineJson:       TJSONObject;
  LSourceNode:       TAIAgentsNode;
  LTargetNode:       TAIAgentsNode;
  LSourceNodeId:     string;
  LTargetNodeId:     string;
  LSourceTerminalId: string;
  LSourcePortId:     string;
  LSourcePortCat:    string;
  LLink:             TAIAgentsLink;
  LLinkModeStr:      string;
  LLinkModeOrd:      Integer;
  LPropData:         TJSONData;
  I:                 Integer;
  SlotIdx:           Integer;
begin
  LEdgesArray := FJsonGraph.Find(cJsonEdges) as TJSONArray;
  if not Assigned(LEdgesArray) then Exit;

  for I := 0 to LEdgesArray.Count - 1 do
  begin
    LEdgeJson         := LEdgesArray.Items[I] as TJSONObject;
    LSourceNodeId     := LEdgeJson.Get(cJsonEdgeSourceNodeId,  '');
    LTargetNodeId     := LEdgeJson.Get(cJsonEdgeTargetNodeId,  '');
    LSourceTerminalId := LEdgeJson.Get(cJsonEdgeSourceTerminal, '');

    if not (FNodeMap.TryGetValue(LSourceNodeId, LSourceNode) and
            FNodeMap.TryGetValue(LTargetNodeId, LTargetNode)) then
      Continue;

    if not FNodeJsonMap.TryGetValue(LSourceNodeId, LSourceNodeJson) then
      Continue;

    LSourcePortJson := FindPortJsonByTerminalId(LSourceNodeJson, LSourceTerminalId);
    if not Assigned(LSourcePortJson) then
      Continue;

    LSourcePortCat := LSourcePortJson.Get(cJsonPortCategory, 'tool');
    if SameText(LSourcePortCat, cJsonPortCategoryAccessory) then
      Continue;

    // Crear el Link la primera vez que se encuentre una arista saliente del nodo
    LLink := LSourceNode.Next;
    if not Assigned(LLink) then
    begin
      LLink := TAIAgentsLink.Create(FAgents);
      LLink.Graph := TAIAgentManager(FAgents);
      LLink.Name  := 'Link_From_' + LSourceNode.Name;
      LSourceNode.Next := LLink;

      // Propiedades del Link desde el objeto 'engine' del nodo de origen
      LEngineJson := FindEngineObject(LSourceNodeJson);
      if Assigned(LEngineJson) then
      begin
        LLinkModeStr := LEngineJson.Get(cJsonEngineLinkMode, 'lmFanout');
        LLinkModeOrd := GetEnumValue(TypeInfo(TLinkMode), LLinkModeStr);
        if LLinkModeOrd >= 0 then
          LLink.Mode := TLinkMode(LLinkModeOrd);

        LLink.MaxCycles        := LEngineJson.Get(cJsonEngineLinkMaxCycles, 1);
        LLink.ConditionalKey   := LEngineJson.Get(cJsonEngineLinkCondKey,  'next_route');
        LLink.ManualTargetsKey := LEngineJson.Get(cJsonEngineLinkManualKey,'next_targets');
        LLink.ExpressionA      := LEngineJson.Get(cJsonEngineLinkExprA, '');
        LLink.ExpressionB      := LEngineJson.Get(cJsonEngineLinkExprB, '');
        LLink.ExpressionC      := LEngineJson.Get(cJsonEngineLinkExprC, '');
        LLink.ExpressionD      := LEngineJson.Get(cJsonEngineLinkExprD, '');
      end;

      LPropData := LEdgeJson.Find(cJsonProperties);
      if LPropData is TJSONObject then
      begin
        LPropertiesJson  := TJSONObject(LPropData);
        LLink.Description := LPropertiesJson.Get(cJsonEdgeDescription, '');
      end;
    end;

    LSourcePortId := LSourcePortJson.Get(cJsonPortId, LSourceTerminalId);

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
        // Asignar a NextA/B/C/D en orden
        SlotIdx := 0;
        if not Assigned(LLink.NextA) then
        begin
          LLink.NextA := LTargetNode;
          SlotIdx := -1;
        end;
        if SlotIdx = 0 then
        begin
          if not Assigned(LLink.NextB) then
          begin
            LLink.NextB := LTargetNode;
            SlotIdx := -1;
          end;
        end;
        if SlotIdx = 0 then
        begin
          if not Assigned(LLink.NextC) then
          begin
            LLink.NextC := LTargetNode;
            SlotIdx := -1;
          end;
        end;
        if SlotIdx = 0 then
        begin
          if not Assigned(LLink.NextD) then
          begin
            LLink.NextD := LTargetNode;
            SlotIdx := -1;
          end;
        end;
        if SlotIdx = 0 then
          LSourceNode.Print(Format(
            'Warning: >4 output ports from node %s. Connection to %s ignored.',
            [LSourceNode.Name, LTargetNode.Name]));
      end;
    end;
  end;
end;

procedure TGraphBuilder.SetToolParameters(ATool: TAiToolBase;
  AParamsJson: TJSONObject);
var
  I:        Integer;
  PropName: string;
  PropInfo: PPropInfo;
  JsonVal:  TJSONData;
  SVal:     string;
begin
  for I := 0 to AParamsJson.Count - 1 do
  begin
    PropName := AParamsJson.Names[I];
    JsonVal  := AParamsJson.Items[I];
    PropInfo := GetPropInfo(ATool.ClassInfo, PropName);
    if not Assigned(PropInfo) then Continue;

    SVal := JsonVal.AsString;

    case PropInfo^.PropType^.Kind of
      tkAString, tkSString, tkUString, tkLString, tkWString:
        SetStrProp(ATool, PropInfo, SVal);

      tkInteger:
        SetOrdProp(ATool, PropInfo, StrToIntDef(SVal, 0));

      tkInt64, tkQWord:
        SetInt64Prop(ATool, PropInfo, StrToInt64Def(SVal, 0));

      tkFloat:
        SetFloatProp(ATool, PropInfo,
          StrToFloatDef(SVal, 0.0, DefaultFormatSettings));

      tkBool:
        if (JsonVal is TJSONBoolean) then
          SetOrdProp(ATool, PropInfo, Ord(TJSONBoolean(JsonVal).AsBoolean))
        else
          SetOrdProp(ATool, PropInfo,
            Ord(SameText(SVal, 'true') or SameText(SVal, '1')));

      tkEnumeration:
        begin
          var LOrd := GetEnumValue(PropInfo^.PropType, SVal);
          if LOrd >= 0 then
            SetOrdProp(ATool, PropInfo, LOrd);
        end;
    end;
  end;
end;

function TGraphBuilder.FindPortJsonByTerminalId(ANodeJson: TJSONObject;
  const APortTerminalId: string): TJSONObject;
var
  LPortsData: TJSONData;
  LPortsArr:  TJSONArray;
  LPortJson:  TJSONObject;
  I:          Integer;
begin
  Result := nil;
  if not Assigned(ANodeJson) then Exit;

  LPortsData := ANodeJson.Find(cJsonNodePorts);
  if not (LPortsData is TJSONArray) then Exit;

  LPortsArr := TJSONArray(LPortsData);
  for I := 0 to LPortsArr.Count - 1 do
  begin
    if LPortsArr.Items[I] is TJSONObject then
    begin
      LPortJson := TJSONObject(LPortsArr.Items[I]);
      if SameText(LPortJson.Get(cJsonPortInternalId, ''), APortTerminalId) then
      begin
        Result := LPortJson;
        Exit;
      end;
    end;
  end;
end;

end.
