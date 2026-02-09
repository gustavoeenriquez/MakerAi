unit uMakerAi.Agents.DmGenerator;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, StrUtils, Generics.Collections, Types, Variants, SyncObjs, Math, TypInfo,
  {$ELSE}
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON, System.AnsiStrings, System.Math,
  {$ENDIF}
  uJsonHelper, uRttiHelper, uHttpHelper, uSysUtilsHelper, uBase64Helper, uThreadingHelper, uRegularExpressionsHelper;

type
  // Estructura interna para mantener la información de cada componente a generar.
  TComponentInfo = record
  private
    FProperties: TStringList;
    procedure SetProperties(const Value: TStringList);
  public
    ComponentName: string;
    ComponentType: string;
    property Properties: TStringList read FProperties write SetProperties;
    class function New(const AName, AType: string): TComponentInfo; static;
  end;

  TDataModuleGenerator = class
  private
    FJsonGraph: TJSONObject;
    FNodeIdToNameMap: TDictionary<string, string>;
    FNodeJsonMap: TDictionary<string, TJSONObject>;
    FLinkIdToNameMap: TDictionary<string, string>;
    FComponentList: TList<TComponentInfo>;
    FAgentsComponentName: string;
    FCreateTools: Boolean;
    FBaseClassName: string;
    FUsedUnits: TStringList;
    FCalculatedWidth: Integer;
    FCalculatedHeight: Integer;

    // Métodos de parseo y construcción del modelo interno
    procedure ParseNodes;
    procedure ParseEdges;
    procedure GenerateToolProperties(const AToolClassName: string; AParamsJson: TJSONObject; var AToolInfo: TComponentInfo);
    procedure AddComponent(const AName, AType: string; out AComponentInfo: TComponentInfo);
    procedure Clear;
    procedure CalculateOptimalSize;

    // Métodos de utilidad (adaptados de tu TGraphBuilder)
    function SanitizeName(const ALabel: string): string;
    function FindPortJsonByTerminalId(ANodeJson: TJSONObject; const APortTerminalId: string): TJSONObject;
    function FindEngineObject(JsonObj: TJSONObject): TJSONObject;
    function MapPortToProperty(const APortId: string; Const ALinkInfo: TComponentInfo): string;
    function FindUnitName(const ATypeName: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GenerateFromJSON(const AJsonString: string; const AClassName: string; ACreateTools: Boolean = True);
    procedure SaveToFile(const ABaseFileName: string);
    function GetPasFileContent: string;
    function GetDfmFileContent: string;
  end;

implementation

uses
  uMakerAi.Agents, uMakerAi.Agents.EngineRegistry;

{ TDataModuleGenerator }

constructor TDataModuleGenerator.Create;
begin
  inherited;
  FNodeIdToNameMap := TDictionary<string, string>.Create;
  FNodeJsonMap := TDictionary<string, TJSONObject>.Create;
  FLinkIdToNameMap := TDictionary<string, string>.Create;
  FComponentList := TList<TComponentInfo>.Create;
  FUsedUnits := TStringList.Create;
  FUsedUnits.Sorted := True;
  FUsedUnits.Duplicates := dupIgnore;
  FCalculatedWidth := 640;
  FCalculatedHeight := 480;
end;

destructor TDataModuleGenerator.Destroy;
begin
  Clear;
  FNodeIdToNameMap.Free;
  FNodeJsonMap.Free;
  FLinkIdToNameMap.Free;
  FComponentList.Free;
  FUsedUnits.Free;
  inherited;
end;

procedure TDataModuleGenerator.CalculateOptimalSize;
const
  // Asumimos un tamaño máximo para cualquier componente (nodo, link, tool)
  // para el cálculo. Es mejor que sobre un poco de espacio.
  ComponentMaxWidth = 100;
  ComponentMaxHeight = 150; // Las tools pueden ser más altas
  Margin = 50; // Espacio extra alrededor del último componente

var
  maxX, maxY: Integer;
  LLeft, LTop: Integer;
  CompInfo: TComponentInfo;
begin
  maxX := 0;
  maxY := 0;

  if FComponentList.Count = 0 then
    Exit; // No hay componentes, usar los valores por defecto

  for CompInfo in FComponentList do
  begin
    // Leemos las propiedades Left y Top, convirtiéndolas a Integer
    // Usamos StrToIntDef para evitar errores si la propiedad no existe (devuelve 0)
    LLeft := StrToIntDef(CompInfo.Properties.Values['Left'], 0);
    LTop := StrToIntDef(CompInfo.Properties.Values['Top'], 0);

    // Actualizamos las coordenadas máximas encontradas hasta ahora
    if LLeft > maxX then
      maxX := LLeft;

    if LTop > maxY then
      maxY := LTop;
  end;

  // Calculamos el tamaño final añadiendo el tamaño del componente y un margen
  FCalculatedWidth := maxX + ComponentMaxWidth + Margin;
  FCalculatedHeight := maxY + ComponentMaxHeight + Margin;

  // Asegurarnos de que el tamaño no sea menor que un mínimo razonable
  FCalculatedWidth := Max(FCalculatedWidth, 640);
  FCalculatedHeight := Max(FCalculatedHeight, 480);
end;

procedure TDataModuleGenerator.Clear;
var
  i: Integer;
begin
  for i := 0 to FComponentList.Count - 1 do
    FComponentList[i].Properties.Free;
  FComponentList.Clear;

  FNodeIdToNameMap.Clear;
  FNodeJsonMap.Clear;
  FLinkIdToNameMap.Clear;
  FUsedUnits.Clear;
  FJsonGraph.Free;
  FJsonGraph := nil;
  FCalculatedWidth := 640;
  FCalculatedHeight := 480;
end;

procedure TDataModuleGenerator.GenerateFromJSON(const AJsonString, AClassName: string; ACreateTools: Boolean);
begin
  if Assigned(FJsonGraph) then
    Clear;

  FJsonGraph := TJSONObject.ParseJSONValue(AJsonString) as TJSONObject;
  if not Assigned(FJsonGraph) then
    raise Exception.Create('Invalid JSON format for graph definition.');

  try
    FBaseClassName := AClassName;
    FCreateTools := ACreateTools;

    ParseNodes;
    ParseEdges;
    CalculateOptimalSize;
  except
    // En caso de error, limpiar para no dejar un estado inconsistente.
    Clear;
    raise;
  end;
end;

procedure TDataModuleGenerator.AddComponent(const AName, AType: string; out AComponentInfo: TComponentInfo);
var
  LUnitName: string;
begin
  AComponentInfo := TComponentInfo.New(AName, AType);
  FComponentList.Add(AComponentInfo);
  // Añadir la unidad del tipo de componente a la lista de 'uses'
  LUnitName := FindUnitName(AType);
  if not LUnitName.IsEmpty then
    FUsedUnits.Add(LUnitName);
end;

procedure TDataModuleGenerator.ParseNodes;
var
  LNodesArray, LPortsArray: TJSONArray; // Añadido LPortsArray
  LNodeJson, LPropertiesJson, LParametersJson, LEngineJson, LPositionJson, LPortJson, LPortEngineJson: TJSONObject; // Añadido LPortJson y LPortEngineJson
  LNodeInfo, LLinkInfo, LToolInfo, LAgentsInfo: TComponentInfo;
  LToolClassName, LJoinModeStr, LDescription, LNodeGuid, LLabel, LNodeName, LLinkName: string;
  LJoinModeOrdinal, i: Integer;
  nodeX, nodeY, linkTop, toolTop: Integer; // Moved from inner scope
  LToolClass: TClass;
  LToolName: string;
  LJsonValue, LPortValue: TJSONValue; // Declared for loops
  minX, minY: Double;
  offsetX, offsetY: Double;
const
  Margin = 40;
begin
  LNodesArray := FJsonGraph.GetValueAsArray('nodes');
  if not Assigned(LNodesArray) then
    raise Exception.Create('JSON graph must contain a "nodes" array.');

  // --- PASO 1: Encontrar las coordenadas mínimas para la normalización ---
  minX := MaxSingle;
  minY := MaxSingle;
  for LJsonValue in LNodesArray do
  begin
    LNodeJson := LJsonValue as TJSONObject;
    if LNodeJson.TryGetValue('position', LPositionJson) then
    begin
      minX := Min(minX, LPositionJson.GetValueAsDouble('x'));
      minY := Min(minY, LPositionJson.GetValueAsDouble('y'));
    end;
  end;

  offsetX := ifthen(minX < 0, -minX, 0);
  offsetY := ifthen(minY < 0, -minY, 0);

  // --- PASO 2: Generar componentes ---
  FAgentsComponentName := 'AIAgents1';
  AddComponent(FAgentsComponentName, 'TAIAgents', LAgentsInfo);
  LAgentsInfo.Properties.Values['Left'] := Margin.ToString;
  LAgentsInfo.Properties.Values['Top'] := Margin.ToString;

  for LJsonValue in LNodesArray do
  begin
    LNodeJson := LJsonValue as TJSONObject;
    LNodeGuid := LNodeJson.GetValueAsString('id');
    LLabel := LNodeJson.GetValueAsString('label', 'Node_' + Copy(LNodeGuid, 2, 8));
    LNodeName := SanitizeName(LLabel);
    LLinkName := 'Link_From_' + LNodeName;

    AddComponent(LNodeName, 'TAIAgentsNode', LNodeInfo);
    AddComponent(LLinkName, 'TAIAgentsLink', LLinkInfo);

    LNodeInfo.Properties.Values['Next'] := LLinkName;
    LNodeInfo.Properties.Values['ID'] := QuotedStr(LNodeGuid);
    LNodeInfo.Properties.Values['Graph'] := FAgentsComponentName;
    LLinkInfo.Properties.Values['Graph'] := FAgentsComponentName;


    if LNodeJson.TryGetValue('position', LPositionJson) then
    begin
      nodeX := Round((LPositionJson.GetValueAsDouble('x')) + offsetX) + Margin;
      nodeY := Round((LPositionJson.GetValueAsDouble('y')) + offsetY) + Margin;
    end
    else
    begin
      nodeX := Margin;
      nodeY := Margin + (FComponentList.Count * 120);
    end;
    linkTop := nodeY + 60;
    toolTop := nodeY + 120;

    LNodeInfo.Properties.Values['Left'] := nodeX.ToString;
    LNodeInfo.Properties.Values['Top'] := nodeY.ToString;
    LLinkInfo.Properties.Values['Left'] := nodeX.ToString;
    LLinkInfo.Properties.Values['Top'] := linkTop.ToString;

    FNodeIdToNameMap.Add(LNodeGuid, LNodeName);
    FNodeJsonMap.Add(LNodeGuid, LNodeJson);
    FLinkIdToNameMap.Add(LNodeGuid, LLinkName);

    LPropertiesJson := LNodeJson.GetValueAsObject('properties');
    if Assigned(LPropertiesJson) then
    begin
      LDescription := LPropertiesJson.GetValueAsString('metadata.description', '');
      if not LDescription.IsEmpty then
        LNodeInfo.Properties.Values['Description'] := QuotedStr(LDescription);

      // Búsqueda original de JoinMode (se mantiene por retrocompatibilidad)
      LEngineJson := FindEngineObject(LPropertiesJson);
      if Assigned(LEngineJson) then
      begin
        LJoinModeStr := LEngineJson.GetValueAsString('joinMode', 'jmAny');
        LJoinModeOrdinal := GetEnumValue(TypeInfo(TJoinMode), LJoinModeStr);
        if (LJoinModeOrdinal <> -1) and (LJoinModeStr <> 'jmAny') then
          LNodeInfo.Properties.Values['JoinMode'] := LJoinModeStr;
      end;

      // --- INICIO DE LA MODIFICACIÓN ---
      // Nueva búsqueda de JoinMode anidado dentro del array 'ports'.
      // Esto sobreescribirá el valor anterior si se encuentra una definición más específica.
      if LNodeJson.TryGetValue('ports', LPortsArray) then
      begin
        for LPortValue in LPortsArray do
        begin
          LPortJson := LPortValue as TJSONObject;
          // Nos interesa solo si el puerto es de entrada y tiene un objeto 'engine'
          if (LPortJson.GetValueAsString('direction') = 'input') and LPortJson.TryGetValue('engine', LPortEngineJson) then
          begin
            // Si encontramos un 'joinMode' aquí, lo usamos y salimos del bucle
            if LPortEngineJson.TryGetValue('joinMode', LJoinModeStr) then
            begin
              LJoinModeOrdinal := GetEnumValue(TypeInfo(TJoinMode), LJoinModeStr);
              if (LJoinModeOrdinal <> -1) and (LJoinModeStr <> 'jmAny') then
                LNodeInfo.Properties.Values['JoinMode'] := LJoinModeStr;
              Break; // Salimos del bucle de puertos, ya hemos encontrado el joinMode.
            end;
          end;
        end;
      end;

      LToolClassName := LPropertiesJson.GetValueAsString('toolClassName');
      if FCreateTools and (not LToolClassName.IsEmpty) then
      begin
        LToolClass := TEngineRegistry.Instance.FindToolClass(LToolClassName);
        if Assigned(LToolClass) then
        begin
          LToolName := SanitizeName(Copy(LToolClassName, 2, MaxInt) + '_' + LNodeName);
          AddComponent(LToolName, LToolClassName, LToolInfo);
          LToolInfo.Properties.Values['ID'] := QuotedStr(LNodeGuid);
          LToolInfo.Properties.Values['Left'] := nodeX.ToString;
          LToolInfo.Properties.Values['Top'] := toolTop.ToString;
          LNodeInfo.Properties.Values['Tool'] := LToolName;

          if LPropertiesJson.TryGetValue('parameters', LParametersJson) then
            GenerateToolProperties(LToolClassName, LParametersJson, LToolInfo);
        end;
      end;

      for i := 0 to FComponentList.Count - 1 do
        if FComponentList[i].ComponentName = FAgentsComponentName then
        begin
          if SameText(LToolClassName, 'TStartTool') then
            FComponentList[i].Properties.Values['StartNode'] := LNodeName;
          if SameText(LToolClassName, 'TEndTool') then
            FComponentList[i].Properties.Values['EndNode'] := LNodeName;
        end;
    end;
  end;
end;

procedure TDataModuleGenerator.ParseEdges;
var
  LEdgesArray: TJSONArray;
  LEdgeGroup: TDictionary<string, TList<TJSONObject>>;
  i: Integer;
  LEdgeList: TList<TJSONObject>;
  LLinkName, LTargetNodeName, LExpr: string;
  // Variables moved from inner scopes
  LJsonValue: TJSONValue;
  LEdgeJson: TJSONObject;
  LSourceNodeId: string;
  LPair: TPair<string, TList<TJSONObject>>;
  LLinkInfoIndex: Integer;
  SharedPropertiesSet: Boolean;
  LSourceNodeJson: TJSONObject;
  LSourceTerminalId: string;
  LSourcePortJson: TJSONObject;
  LEngineJson: TJSONObject;
  LLinkModeStr: string;
  LLinkMode: TLinkMode;
  LValue: string;
  LMaxCycles: Integer;
  LSourcePortId, LTargetPropName: string;
begin
  LEdgesArray := FJsonGraph.GetValueAsArray('edges');
  if not Assigned(LEdgesArray) then
    Exit;

  LEdgeGroup := TDictionary < string, TList < TJSONObject >>.Create;
  try
    // --- PASO 1: FASE DE AGRUPACIÓN ---
    // Recorremos todas las aristas del JSON y las agrupamos en un diccionario
    // usando el ID del nodo de origen como clave.
    for LJsonValue in LEdgesArray do
    begin
      LEdgeJson := LJsonValue as TJSONObject;
      LSourceNodeId := LEdgeJson.GetValueAsString('sourceNodeId');
      if not LEdgeGroup.TryGetValue(LSourceNodeId, LEdgeList) then
      begin
        LEdgeList := TList<TJSONObject>.Create;
        LEdgeGroup.Add(LSourceNodeId, LEdgeList);
      end;
      LEdgeList.Add(LEdgeJson);
    end;

    // --- PASO 2: FASE DE PROCESAMIENTO POR GRUPO ---
    // Iteramos sobre cada grupo de aristas. Cada grupo corresponde a un único TAIAgentsLink.
    for LPair in LEdgeGroup do
    begin
      LSourceNodeId := LPair.Key;
      LEdgeList := LPair.Value;
      if LEdgeList.Count = 0 then
        Continue;

      // Encontrar el Link que ya creamos en ParseNodes
      if not FLinkIdToNameMap.TryGetValue(LSourceNodeId, LLinkName) then
        Continue; // No se encontró el link para este nodo, algo raro pasó.

      LLinkInfoIndex := -1;
      for i := 0 to FComponentList.Count - 1 do
      begin
        if FComponentList[i].ComponentName = LLinkName then
        begin
          LLinkInfoIndex := i;
          Break;
        end;
      end;
      if LLinkInfoIndex = -1 then
        Continue; // No se encontró el registro del Link en la lista.

      SharedPropertiesSet := False;
      LSourceNodeJson := FNodeJsonMap[LSourceNodeId];

      // --- PASO 3: PROCESAR CADA ARISTA DENTRO DEL GRUPO ---
      // Iteramos sobre cada arista para configurar el Link compartido.
      for LEdgeJson in LEdgeList do
      begin
        LSourceTerminalId := LEdgeJson.GetValueAsString('sourceTerminal');
        LSourcePortJson := FindPortJsonByTerminalId(LSourceNodeJson, LSourceTerminalId);
        if not Assigned(LSourcePortJson) then
          Continue;

        // Intentar obtener el objeto 'engine' desde el puerto
        if LSourcePortJson.TryGetValue('engine', LEngineJson) then
        begin
          // A) Procesar Propiedades Compartidas (solo la primera vez que se encuentren)
          if not SharedPropertiesSet then
          begin
            LLinkModeStr := LEngineJson.GetValueAsString('linkMode');
            if not LLinkModeStr.IsEmpty then
            begin
              LLinkMode := TLinkMode(GetEnumValue(TypeInfo(TLinkMode), LLinkModeStr));
              if LLinkMode <> lmFanout then
                FComponentList[LLinkInfoIndex].Properties.Values['Mode'] := LLinkModeStr;

              case LLinkMode of
                lmConditional:
                  Begin
                    if LEngineJson.TryGetValue('linkConditionalKey', LValue) then
                      FComponentList[LLinkInfoIndex].Properties.Values['ConditionalKey'] := LValue;
                  End;
                lmManual:
                  Begin
                    if LEngineJson.TryGetValue('linkManualTargetsKey', LValue) then
                      FComponentList[LLinkInfoIndex].Properties.Values['ManualTargetsKey'] := LValue;
                  End;
              end;
            end;

            LMaxCycles := LEngineJson.GetValueAsInteger('linkMaxCycles', -1);
            if LMaxCycles <> -1 then // Usar -1 para detectar si la propiedad existe
              if LMaxCycles <> 1 then // Solo añadir si no es el valor por defecto
                FComponentList[LLinkInfoIndex].Properties.Values['MaxCycles'] := LMaxCycles.ToString;

            SharedPropertiesSet := True;
          end;

          // B) Procesar Propiedades Específicas del Puerto (siempre se procesan)
          if LEngineJson.TryGetValue('linkExpressionA', LExpr) then
            FComponentList[LLinkInfoIndex].Properties.Values['ExpressionA'] := LExpr;
          if LEngineJson.TryGetValue('linkExpressionB', LExpr) then
            FComponentList[LLinkInfoIndex].Properties.Values['ExpressionB'] := LExpr;
          if LEngineJson.TryGetValue('linkExpressionC', LExpr) then
            FComponentList[LLinkInfoIndex].Properties.Values['ExpressionC'] := LExpr;
          if LEngineJson.TryGetValue('linkExpressionD', LExpr) then
            FComponentList[LLinkInfoIndex].Properties.Values['ExpressionD'] := LExpr;
        end;

        // C) Asignar el Destino (Target) de la arista
        if FNodeIdToNameMap.TryGetValue(LEdgeJson.GetValueAsString('targetNodeId'), LTargetNodeName) then
        begin
          LSourcePortId := LSourcePortJson.GetValueAsString('idStr', LSourceTerminalId);
          LTargetPropName := MapPortToProperty(LSourcePortId, FComponentList[LLinkInfoIndex]);
          FComponentList[LLinkInfoIndex].Properties.Values[LTargetPropName] := LTargetNodeName;
        end;
      end;
    end;
  finally
    // Limpiar la memoria del diccionario y de todas las listas que contiene.
    for LPair in LEdgeGroup do
      LPair.Value.Free;
    LEdgeGroup.Free;
  end;
end;

procedure TDataModuleGenerator.GenerateToolProperties(const AToolClassName: string; AParamsJson: TJSONObject; var AToolInfo: TComponentInfo);
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiProp: TRttiProperty;
  LParamPair: TJSONPair;
  LPropValueStr: string;
  LToolClass: TClass;
  LParamValue: TJSONValue;
begin
  LToolClass := TEngineRegistry.Instance.FindToolClass(AToolClassName);
  if not Assigned(LToolClass) then
    Exit;

  LRttiContext := TRttiContext.Create;
  try
    LRttiType := LRttiContext.GetType(LToolClass);
    for LParamPair in AParamsJson do
    begin
      LRttiProp := LRttiType.GetProperty(LParamPair.JsonString.AsString);
      if Assigned(LRttiProp) and LRttiProp.IsWritable then
      begin
        LPropValueStr := '';
        LParamValue := LParamPair.JsonValue;

        case PTypeInfo(LRttiProp.PropertyType.Handle)^.Kind of
          tkString, tkLString, tkWString, tkUString:
            LPropValueStr := QuotedStr(LParamValue.AsString);
          tkInteger, tkInt64:
            LPropValueStr := LParamValue.AsString;
          tkFloat:
            LPropValueStr := LParamValue.AsString;
          tkEnumeration:
            if LRttiProp.PropertyType.Handle = TypeInfo(Boolean) then
              LPropValueStr := LParamValue.AsString
            else
              LPropValueStr := LParamValue.AsString; // Asume que el nombre del enum ya viene en el JSON
        end;

        if not LPropValueStr.IsEmpty then
          AToolInfo.Properties.Add(LRttiProp.Name + ' = ' + LPropValueStr);
      end;
    end;
  finally
    LRttiContext.Free;
  end;
end;

function TDataModuleGenerator.FindEngineObject(JsonObj: TJSONObject): TJSONObject;
begin
  // Esta función es idéntica a la de tu TGraphBuilder, se puede copiar y pegar aquí.
  // ... implementación completa ...
  Result := nil;
  if JsonObj = nil then
    Exit;
  if JsonObj.TryGetValue('properties.metadata.engine', Result) then
    Exit;
  if JsonObj.TryGetValue('metadata.engine', Result) then
    Exit;
  if JsonObj.TryGetValue('engine', Result) then
    Exit;
end;

function TDataModuleGenerator.FindPortJsonByTerminalId(ANodeJson: TJSONObject; const APortTerminalId: string): TJSONObject;
var
  LPortJson: TJSONObject;
  LJsonValue: TJSONValue;
  LPortsArray: TJSONArray;
begin
  // Esta función es idéntica a la de tu TGraphBuilder, se puede copiar y pegar aquí.
  // ... implementación completa ...
  Result := nil;
  if not Assigned(ANodeJson) then
    Exit;
  if not ANodeJson.TryGetValue('ports', LPortsArray) then
    Exit;

  for LJsonValue in LPortsArray do
  begin
    LPortJson := LJsonValue as TJSONObject;
    if SameText(LPortJson.GetValueAsString('internalId'), APortTerminalId) then
    begin
      Result := LPortJson;
      Exit;
    end;
  end;
end;

function TDataModuleGenerator.FindUnitName(const ATypeName: string): string;
{ var
  LContext: TRttiContext;
  LType: TRttiType;
  LPackage: TPackage;
  I: Integer;
}
begin
  { Result := '';
    LContext := TRttiContext.Create;
    try
    // 1. Busca el tipo RTTI por su nombre de clase
    LType := LContext.FindType(ATypeName);

    if Assigned(LType) then
    begin
    // 2. Itera a través de todos los paquetes cargados en tiempo de diseño
    for LPackage in TPackage.Packages do
    begin
    // 3. Verifica si el tipo está contenido en el paquete actual
    if LPackage.ContainsType(LType) then
    begin
    // 4. Si lo encuentra, itera sobre las unidades de ese paquete
    for I := 0 to LPackage.UnitCount - 1 do
    begin
    if LPackage.UnitNames[I].Contains(LType.UnitName) then
    begin
    Result := LType.UnitName;
    Exit; // Encontramos la unidad, salimos.
    end;
    end;
    end;
    end;
    end;
    finally
    LContext.Free;
    end;

    // Fallback: Si no se encontró en ningún paquete (ej. clases de System o SysUtils),
    // puede que el nombre de la unidad sea parte del nombre completo del tipo.
    if Result.IsEmpty and Assigned(LType) and (LType.QualifiedName.Contains('.')) then
    begin
    Result := LType.UnitName;
    end;
  }
end;

function TDataModuleGenerator.GetDfmFileContent: string;
var
  SB: TStringBuilder;
  CompInfo: TComponentInfo;
  Prop: string;
begin
  SB := TStringBuilder.Create;
  try
    // CORREGIDO:
    SB.Append(Format('object %s: T%s', [FBaseClassName, FBaseClassName]));
    SB.AppendLine;
    SB.AppendLine(Format('  Height = %d', [FCalculatedHeight]));
    SB.AppendLine(Format('  Width = %d', [FCalculatedWidth]));

    for CompInfo in FComponentList do
    begin
      // CORREGIDO:
      SB.Append(Format('  object %s: %s', [CompInfo.ComponentName, CompInfo.ComponentType]));
      SB.AppendLine;
      for Prop in CompInfo.Properties do
        SB.AppendLine('    ' + Prop);
      SB.AppendLine('  end');
    end;

    SB.AppendLine('end');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TDataModuleGenerator.GetPasFileContent: string;
var
  SB: TStringBuilder;
  CompInfo: TComponentInfo;
begin
  SB := TStringBuilder.Create;
  try
    // CORREGIDO:
    SB.Append(Format('unit %s;', [FBaseClassName]));
    SB.AppendLine;
    SB.AppendLine;

    SB.AppendLine('interface');
    SB.AppendLine;

    FUsedUnits.Add('System.SysUtils');
    FUsedUnits.Add('System.Classes');
    FUsedUnits.Add('uMakerAi.Agents');
    // FUsedUnits.Add('uMakerAi.Agents.Tools'); // Asegúrate que esta unidad exista o adáptala

    SB.AppendLine('uses');
    SB.Append('  ').Append(FUsedUnits.CommaText);
    SB.AppendLine(';').AppendLine;

    SB.AppendLine('type');
    SB.Append(Format('  T%s = class(TDataModule)', [FBaseClassName]));
    SB.AppendLine;
    for CompInfo in FComponentList do
    begin
      // CORREGIDO:
      SB.Append(Format('    %s: %s;', [CompInfo.ComponentName, CompInfo.ComponentType]));
      SB.AppendLine;
    end;
    SB.AppendLine('  private');
    SB.AppendLine('    { Private declarations }');
    SB.AppendLine('  public');
    SB.AppendLine('    { Public declarations }');
    SB.AppendLine('  end;');
    SB.AppendLine;

    SB.AppendLine('var');
    // CORREGIDO:
    SB.Append(Format('  Dm%s: T%s;', [FBaseClassName, FBaseClassName]));
    SB.AppendLine;
    SB.AppendLine;

    SB.AppendLine('implementation');
    SB.AppendLine;
    SB.AppendLine('{%CLASSGROUP FMX.Controls.TControl}'); // o Vcl.Controls.TControl
    SB.AppendLine;
    // CORREGIDO:
    SB.Append(Format('{$R *.dfm}', [])); // No necesita formato, pero lo dejo así por consistencia
    SB.AppendLine;
    SB.AppendLine;

    SB.AppendLine('end.');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TDataModuleGenerator.MapPortToProperty(const APortId: string; Const ALinkInfo: TComponentInfo): string;
var
  NextProps: TArray<string>;
  i: Integer;
begin
  NextProps := ['NextA', 'NextB', 'NextC', 'NextD'];

  if SameText(APortId, 'out_failure') then
    Result := 'NextNo'
  else
  begin
    // Busca el primer 'NextX' que no esté usado en este Link.
    for i := Low(NextProps) to High(NextProps) do
    begin

      if ALinkInfo.Properties.IndexOfName(NextProps[i]) = -1 then
      begin
        Result := NextProps[i];
        Exit;
      end;
    end;
    // Si todos están usados, devuelve el primero como fallback (puede causar sobreescritura)
    Result := 'NextA';
  end;
end;

procedure TDataModuleGenerator.SaveToFile(const ABaseFileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := GetPasFileContent;
    SL.SaveToFile(ABaseFileName + '.pas');

    SL.Text := GetDfmFileContent;
    SL.SaveToFile(ABaseFileName + '.dfm'); // o .fmx
  finally
    SL.Free;
  end;
end;

function TDataModuleGenerator.SanitizeName(const ALabel: string): string;
begin
  Result := ALabel;
  // Elimina espacios y caracteres no válidos.
  Result := TRegEx.Replace(Result, '[^a-zA-Z0-9_]', '');
  // Si está vacío después de limpiar, genera un nombre por defecto.
  if Result.IsEmpty then
    Result := 'Component' + TGuid.NewGuid.ToString.Substring(1, 8);
  // Asegurarse de que no empiece con un número.
  if (Result[1] >= '0') and (Result[1] <= '9') then
    Result := '_' + Result;
end;

{ TComponentInfo }

class function TComponentInfo.New(const AName, AType: string): TComponentInfo;
begin
  Result.ComponentName := AName;
  Result.ComponentType := AType;
  Result.FProperties := TStringList.Create;
  // TStringList usado para propiedades es case-insensitive por defecto, lo cual es bueno para .dfm
  Result.FProperties.CaseSensitive := False;
end;

procedure TComponentInfo.SetProperties(const Value: TStringList);
begin
  FProperties.Assign(Value);
end;

end.
