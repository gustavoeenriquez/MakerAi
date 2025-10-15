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


unit uMakerAi.Agents.EngineRegistry;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections, System.Variants, System.TypInfo,
  // Dependencias del "Motor"
  uMakerAi.Agents, uMakerAi.Agents.Attributes;

type
  // Este es el "contrato" de datos que el Diseñador recibirá.
  // Es un simple registro, no contiene clases ni lógica compleja.
  TToolBlueprint = record
    ToolClassName: string;
    DisplayName: string;
    Description: string;
    Category: string;
    Schema: TJSONObject;
  end;

  // --- CAMBIO 1: Nueva estructura para almacenar la clase Y el nombre de su unidad. ---
  // Esto enriquece el registro para que el generador de código sepa qué unidades incluir.
  TToolInfo = record
    ToolClass: TClass;
    UnitName: string;
  end;

  // El Singleton que actúa como nuestra "caja negra".
  TEngineRegistry = class
  private
    class var
      FInstance: TEngineRegistry;
    // --- CAMBIO 2: El diccionario ahora almacena el registro TToolInfo. ---
    FRegisteredTools: TDictionary<string, TToolInfo>;
    constructor Create;
  public
    destructor Destroy; override;
    class property Instance: TEngineRegistry read FInstance;

    // --- CAMBIO 3: El método de registro ahora requiere el nombre de la unidad. ---
    procedure RegisterTool(ToolClass: TClass; const AUnitName: string);

    function FindToolClass(const AToolClassName: string): TClass;

    // --- CAMBIO 4: Nuevo método para que el generador consulte el nombre de la unidad. ---
    function GetUnitForToolClass(const AToolClassName: string): string;

    function GetToolBlueprints: TArray<TToolBlueprint>;
  end;

  // TAgentHandlerRegistry no requiere cambios.
  TAgentHandlerRegistry = class
  private
  class var
    FInstance: TAgentHandlerRegistry;
    FNodeHandlers: TDictionary<string, TAIAgentsNodeOnExecute>;
    FLinkHandlers: TDictionary<string, TAIAgentsLinkOnExecute>;
    constructor Create;
  public
    destructor Destroy; override;
    class property Instance: TAgentHandlerRegistry read FInstance;

    procedure RegisterNodeHandler(const AName: string; AHandler: TAIAgentsNodeOnExecute);
    procedure RegisterLinkHandler(const AName: string; AHandler: TAIAgentsLinkOnExecute);
    function FindNodeHandler(const AName: string): TAIAgentsNodeOnExecute;
    function FindLinkHandler(const AName: string): TAIAgentsLinkOnExecute;
  end;

implementation

uses System.Rtti;

// Helper interno para generar los esquemas JSON de los parámetros.
// No requiere cambios.
type
  TSchemaGen_Internal = class
  private
    class function DelphiTypeToJSONType(APropType: TRttiType): string;
    class function DelphiTypeToJSONFormat(APropType: TRttiType): string;
  public
    class function GenerateSchemaFor(AClass: TClass): TJSONObject;
  end;

{ TEngineRegistry }

constructor TEngineRegistry.Create;
begin
  inherited;
  // --- CAMBIO 5: Se crea el diccionario con el nuevo tipo de valor (TToolInfo). ---
  FRegisteredTools := TDictionary<string, TToolInfo>.Create;
end;

destructor TEngineRegistry.Destroy;
begin
  FRegisteredTools.Free;
  inherited;
end;

function TEngineRegistry.FindToolClass(const AToolClassName: string): TClass;
var
  LToolInfo: TToolInfo;
begin
  // --- CAMBIO 6: La búsqueda ahora extrae el TClass desde el registro TToolInfo. ---
  if FRegisteredTools.TryGetValue(AToolClassName, LToolInfo) then
    Result := LToolInfo.ToolClass
  else
    Result := nil;
end;

// --- CAMBIO 7: Implementación del nuevo método para obtener la unidad. ---
function TEngineRegistry.GetUnitForToolClass(const AToolClassName: string): string;
var
  LToolInfo: TToolInfo;
begin
  if FRegisteredTools.TryGetValue(AToolClassName, LToolInfo) then
    Result := LToolInfo.UnitName
  else
    Result := '';
end;

// --- CAMBIO 8: Implementación del método de registro modificado. ---
procedure TEngineRegistry.RegisterTool(ToolClass: TClass; const AUnitName: string);
var
  LToolInfo: TToolInfo;
begin
  if not ToolClass.InheritsFrom(TAiToolBase) then
    raise Exception.CreateFmt('Cannot register class "%s" because it does not inherit from TAiToolBase.', [ToolClass.ClassName]);

  // Se llena el registro con toda la información necesaria.
  LToolInfo.ToolClass := ToolClass;
  LToolInfo.UnitName := AUnitName;

  FRegisteredTools.Add(ToolClass.ClassName, LToolInfo);
end;

function TEngineRegistry.GetToolBlueprints: TArray<TToolBlueprint>;
var
  LToolInfo: TToolInfo;   // La variable del bucle ahora es del tipo TToolInfo.
  ToolClass: TClass;      // Variable local para mantener la claridad del código existente.
  LContext: TRttiContext;
  LRttiType: TRttiType;
  ClassToolAttr: TToolAttribute;
  i: Integer;
begin
  SetLength(Result, FRegisteredTools.Count);
  i := 0;
  LContext := TRttiContext.Create;
  try
    // --- CAMBIO 9: El bucle itera sobre los valores TToolInfo del diccionario. ---
    for LToolInfo in FRegisteredTools.Values do
    begin
      // Se extrae la clase del registro para que el resto del código funcione sin cambios.
      ToolClass := LToolInfo.ToolClass;

      LRttiType := LContext.GetType(ToolClass);
      ClassToolAttr := nil;

      for var Attr in LRttiType.GetAttributes do
      begin
        if Attr is TToolAttribute then
        begin
          ClassToolAttr := Attr as TToolAttribute;
          Break;
        end;
      end;

      Result[i].ToolClassName := ToolClass.ClassName;

      if Assigned(ClassToolAttr) then
      begin
        Result[i].DisplayName := ClassToolAttr.Name;
        Result[i].Description := ClassToolAttr.Description;
        Result[i].Category := ClassToolAttr.Category;
      end
      else
      begin
        Result[i].DisplayName := ToolClass.ClassName.Substring(1);
        Result[i].Description := 'No description provided.';
        Result[i].Category := 'Uncategorized';
      end;

      Result[i].Schema := TSchemaGen_Internal.GenerateSchemaFor(ToolClass);

      Inc(i);
    end;
  finally
    LContext.Free;
  end;
end;

{ TSchemaGen_Internal }

// Esta clase no necesita ninguna modificación.
class function TSchemaGen_Internal.GenerateSchemaFor(AClass: TClass): TJSONObject;
var
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LProp: TRttiProperty;
  ParamAttr: TToolParameterAttribute;
  LProperties, LPropSchema: TJSONObject;
  JsonType: string;
begin
  Result := TJSONObject.Create;
  LProperties := TJSONObject.Create;
  Result.AddPair('type', 'object');
  Result.AddPair('title', AClass.ClassName.Substring(1));
  Result.AddPair('properties', LProperties);

  LContext := TRttiContext.Create;
  try
    LRttiType := LContext.GetType(AClass);
    for LProp in LRttiType.GetProperties do
    begin
      ParamAttr := nil;
      for var Attr in LProp.GetAttributes do
        if Attr is TToolParameterAttribute then
        begin
          ParamAttr := Attr as TToolParameterAttribute;
          Break;
        end;

      if Assigned(ParamAttr) then
      begin
        LPropSchema := TJSONObject.Create;
        LPropSchema.AddPair('title', ParamAttr.DisplayName);
        LPropSchema.AddPair('description', ParamAttr.Hint);
        JsonType := DelphiTypeToJSONType(LProp.PropertyType);
        LPropSchema.AddPair('type', JsonType);

        var
        LFormat := DelphiTypeToJSONFormat(LProp.PropertyType);
        if not LFormat.IsEmpty then
          LPropSchema.AddPair('format', LFormat);

        if not ParamAttr.DefaultValue.IsEmpty then
        begin
          var
          PropTypeKind := LProp.PropertyType.TypeKind;
          if (PropTypeKind = tkInteger) or (PropTypeKind = tkInt64) then
            LPropSchema.AddPair('default', TJSONNumber.Create(StrToIntDef(ParamAttr.DefaultValue, 0)))
          else if PropTypeKind = tkFloat then
            LPropSchema.AddPair('default', TJSONNumber.Create(StrToFloatDef(ParamAttr.DefaultValue, 0.0)))
          else if PropTypeKind = tkEnumeration then
            if LProp.PropertyType.Handle = TypeInfo(Boolean) then
              LPropSchema.AddPair('default', TJSONBool.Create(SameText(ParamAttr.DefaultValue, 'True')))
            else
              LPropSchema.AddPair('default', TJSONString.Create(ParamAttr.DefaultValue))
          else
            LPropSchema.AddPair('default', TJSONString.Create(ParamAttr.DefaultValue));
        end;

        if (LProp.PropertyType.TypeKind = tkEnumeration) and (LProp.PropertyType.Handle <> TypeInfo(Boolean)) then
        begin
          var
          LEnumArray := TJSONArray.Create;
          var
          RttiEnum := LProp.PropertyType as TRttiEnumerationType;
          for var EnumName in RttiEnum.GetNames do
            LEnumArray.Add(EnumName);
          LPropSchema.AddPair('enum', LEnumArray);
        end;

        LProperties.AddPair(LProp.Name, LPropSchema);
      end;
    end;
  finally
    LContext.Free;
  end;
end;

class function TSchemaGen_Internal.DelphiTypeToJSONType(APropType: TRttiType): string;
begin
  if not Assigned(APropType) then
    Exit('string');
  case APropType.TypeKind of
    tkInteger, tkInt64:
      Result := 'integer';
    tkFloat:
      Result := 'number';
    tkEnumeration:
      if APropType.Handle = TypeInfo(Boolean) then
        Result := 'boolean'
      else
        Result := 'string';
    tkString, tkUString, tkChar, tkWChar, tkLString, tkWString:
      Result := 'string';
    tkClass:
      if APropType.Handle = TypeInfo(TDateTime) then
        Result := 'string'
      else if APropType.Handle = TypeInfo(TDate) then
        Result := 'string'
      else if APropType.Handle = TypeInfo(TTime) then
        Result := 'string'
      else
        Result := 'string';
  else
    Result := 'string';
  end;
end;

class function TSchemaGen_Internal.DelphiTypeToJSONFormat(APropType: TRttiType): string;
begin
  Result := '';
  if not Assigned(APropType) then
    Exit;
  if APropType.Handle = TypeInfo(TDateTime) then
    Result := 'date-time'
  else if APropType.Handle = TypeInfo(TDate) then
    Result := 'date'
  else if APropType.Handle = TypeInfo(TTime) then
    Result := 'time';
end;

{ TAgentHandlerRegistry }

// Esta clase no necesita ninguna modificación.
constructor TAgentHandlerRegistry.Create;
begin
  inherited;
  FNodeHandlers := TDictionary<string, TAIAgentsNodeOnExecute>.Create;
  FLinkHandlers := TDictionary<string, TAIAgentsLinkOnExecute>.Create;
end;

destructor TAgentHandlerRegistry.Destroy;
begin
  FNodeHandlers.Free;
  FLinkHandlers.Free;
  inherited;
end;

function TAgentHandlerRegistry.FindNodeHandler(const AName: string): TAIAgentsNodeOnExecute;
begin
  FNodeHandlers.TryGetValue(AName, Result);
end;

function TAgentHandlerRegistry.FindLinkHandler(const AName: string): TAIAgentsLinkOnExecute;
begin
  FLinkHandlers.TryGetValue(AName, Result);
end;

procedure TAgentHandlerRegistry.RegisterNodeHandler(const AName: string; AHandler: TAIAgentsNodeOnExecute);
begin
  FNodeHandlers.AddOrSetValue(AName, AHandler);
end;

procedure TAgentHandlerRegistry.RegisterLinkHandler(const AName: string; AHandler: TAIAgentsLinkOnExecute);
begin
  FLinkHandlers.AddOrSetValue(AName, AHandler);
end;

initialization
  TEngineRegistry.FInstance := TEngineRegistry.Create;
  TAgentHandlerRegistry.FInstance := TAgentHandlerRegistry.Create;

finalization
  TEngineRegistry.FInstance.Free;
  TAgentHandlerRegistry.FInstance.Free;

end.
