// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Registro singleton de herramientas de agentes y handlers de nodos/enlaces.
//
// Adaptaciones FPC:
//   - System.Rtti (TRttiContext, TRttiType) → TypInfo (GetPropList, PPropInfo)
//   - TDictionary<K,V> → specialize TDictionary<K,V>
//   - TToolAttribute/TToolParameterAttribute: clases normales (no RTTI attrs)
//     El nombre/descripcion/categoria se registran explicitamente.
//   - TAgentHandlerRegistry: metodos-puntero en wrapper classes para
//     evitar problemas de generics con procedure-of-object en FPC

unit uMakerAi.Agents.EngineRegistry;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TypInfo,
  generics.collections,
  fpjson,
  uMakerAi.Agents, uMakerAi.Agents.Attributes;

type
  // -------------------------------------------------------------------------
  // TToolBlueprint — descriptor de una herramienta registrada
  // -------------------------------------------------------------------------
  TToolBlueprint = record
    ToolClassName: string;
    DisplayName:   string;
    Description:   string;
    Category:      string;
    Schema:        TJSONObject; // caller frees
  end;

  TToolBlueprintArray = array of TToolBlueprint;

  // -------------------------------------------------------------------------
  // TToolInfo — clase + nombre de unidad almacenados en el registro
  // -------------------------------------------------------------------------
  TToolInfo = record
    ToolClass:   TClass;
    UnitName:    string;
    DisplayName: string;
    Description: string;
    Category:    string;
  end;

  TToolInfoDict = specialize TDictionary<string, TToolInfo>;

  // -------------------------------------------------------------------------
  // Wrappers para almacenar method-pointers en el diccionario de handlers
  // -------------------------------------------------------------------------
  TNodeHandlerWrapper = class
    Handler: TAIAgentsNodeOnExecute;
  end;

  TLinkHandlerWrapper = class
    Handler: TAIAgentsLinkOnExecute;
  end;

  TNodeHandlerDict = specialize TObjectDictionary<string, TNodeHandlerWrapper>;
  TLinkHandlerDict = specialize TObjectDictionary<string, TLinkHandlerWrapper>;

  // -------------------------------------------------------------------------
  // TEngineRegistry — singleton central de herramientas
  // -------------------------------------------------------------------------
  TEngineRegistry = class
  private
    class var FInstance: TEngineRegistry;
    FRegisteredTools: TToolInfoDict;
    constructor Create;
  public
    destructor Destroy; override;
    class property Instance: TEngineRegistry read FInstance;

    // Registro con nombre/descripcion/categoria opcionales.
    // Si no se proveen, se derivan del ClassName y TToolAttribute (no disponible en FPC via RTTI).
    procedure RegisterTool(AToolClass: TClass; const AUnitName: string;
                           const ADisplayName: string = '';
                           const ADescription: string = '';
                           const ACategory:    string = '');

    function FindToolClass(const AToolClassName: string): TClass;
    function GetUnitForToolClass(const AToolClassName: string): string;
    function GetToolBlueprints: TToolBlueprintArray;
  end;

  // -------------------------------------------------------------------------
  // TAgentHandlerRegistry — registro de handlers de nodos y enlaces
  // -------------------------------------------------------------------------
  TAgentHandlerRegistry = class
  private
    class var FInstance: TAgentHandlerRegistry;
    FNodeHandlers: TNodeHandlerDict;
    FLinkHandlers: TLinkHandlerDict;
    constructor Create;
  public
    destructor Destroy; override;
    class property Instance: TAgentHandlerRegistry read FInstance;

    procedure RegisterNodeHandler(const AName: string; AHandler: TAIAgentsNodeOnExecute);
    procedure RegisterLinkHandler(const AName: string; AHandler: TAIAgentsLinkOnExecute);
    function  FindNodeHandler(const AName: string): TAIAgentsNodeOnExecute;
    function  FindLinkHandler(const AName: string): TAIAgentsLinkOnExecute;
  end;

implementation

// ---------------------------------------------------------------------------
// Generador de esquemas JSON usando TypInfo (sin TRttiContext de Delphi)
// ---------------------------------------------------------------------------

function PropTypeToJsonType(APropInfo: PPropInfo): string;
var
  LKind: TTypeKind;
begin
  LKind := APropInfo^.PropType^.Kind;
  case LKind of
    tkInteger, tkInt64, tkQWord:
      Result := 'integer';
    tkFloat:
      Result := 'number';
    tkBool:
      Result := 'boolean';
    tkEnumeration:
      Result := 'string';
    tkAString, tkSString, tkUString, tkLString, tkWString:
      Result := 'string';
  else
    Result := 'string';
  end;
end;

function GenerateSchemaFor(AClass: TClass): TJSONObject;
var
  LProperties: TJSONObject;
  LPropSchema: TJSONObject;
  PropList:    PPropList;
  PropCount:   Integer;
  I:           Integer;
  PropInfo:    PPropInfo;
  JsonType:    string;
begin
  Result := TJSONObject.Create;
  LProperties := TJSONObject.Create;
  Result.Add('type', 'object');
  Result.Add('title', Copy(AClass.ClassName, 2, MaxInt));
  Result.Add('properties', LProperties);

  if AClass.ClassInfo = nil then Exit;

  PropCount := GetPropList(AClass.ClassInfo, tkAny, nil);
  if PropCount <= 0 then Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(AClass.ClassInfo, tkAny, PropList);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      JsonType := PropTypeToJsonType(PropInfo);
      LPropSchema := TJSONObject.Create;
      LPropSchema.Add('type', JsonType);
      LProperties.Add(PropInfo^.Name, LPropSchema);
    end;
  finally
    FreeMem(PropList);
  end;
end;

// ---------------------------------------------------------------------------
// TEngineRegistry
// ---------------------------------------------------------------------------

constructor TEngineRegistry.Create;
begin
  inherited Create;
  FRegisteredTools := TToolInfoDict.Create;
end;

destructor TEngineRegistry.Destroy;
begin
  FRegisteredTools.Free;
  inherited;
end;

procedure TEngineRegistry.RegisterTool(AToolClass: TClass; const AUnitName: string;
  const ADisplayName, ADescription, ACategory: string);
var
  LInfo: TToolInfo;
begin
  if not AToolClass.InheritsFrom(TAiToolBase) then
    raise Exception.CreateFmt(
      'Cannot register class "%s": does not inherit from TAiToolBase.',
      [AToolClass.ClassName]);

  LInfo.ToolClass   := AToolClass;
  LInfo.UnitName    := AUnitName;
  LInfo.DisplayName := ADisplayName;
  LInfo.Description := ADescription;
  LInfo.Category    := ACategory;

  // Defaults si no se proporcionaron
  if LInfo.DisplayName = '' then
    LInfo.DisplayName := Copy(AToolClass.ClassName, 2, MaxInt);
  if LInfo.Category = '' then
    LInfo.Category := 'General';

  FRegisteredTools.AddOrSetValue(AToolClass.ClassName, LInfo);
end;

function TEngineRegistry.FindToolClass(const AToolClassName: string): TClass;
var
  LInfo: TToolInfo;
begin
  if FRegisteredTools.TryGetValue(AToolClassName, LInfo) then
    Result := LInfo.ToolClass
  else
    Result := nil;
end;

function TEngineRegistry.GetUnitForToolClass(const AToolClassName: string): string;
var
  LInfo: TToolInfo;
begin
  if FRegisteredTools.TryGetValue(AToolClassName, LInfo) then
    Result := LInfo.UnitName
  else
    Result := '';
end;

function TEngineRegistry.GetToolBlueprints: TToolBlueprintArray;
var
  LEnum:  TToolInfoDict.TPairEnumerator;
  LInfo:  TToolInfo;
  I:      Integer;
begin
  SetLength(Result, FRegisteredTools.Count);
  I := 0;
  LEnum := FRegisteredTools.GetEnumerator;
  try
    while LEnum.MoveNext do
    begin
      LInfo := LEnum.Current.Value;
      Result[I].ToolClassName := LInfo.ToolClass.ClassName;
      Result[I].DisplayName   := LInfo.DisplayName;
      Result[I].Description   := LInfo.Description;
      Result[I].Category      := LInfo.Category;
      Result[I].Schema        := GenerateSchemaFor(LInfo.ToolClass);
      Inc(I);
    end;
  finally
    LEnum.Free;
  end;
end;

// ---------------------------------------------------------------------------
// TAgentHandlerRegistry
// ---------------------------------------------------------------------------

constructor TAgentHandlerRegistry.Create;
begin
  inherited Create;
  FNodeHandlers := TNodeHandlerDict.Create([doOwnsValues]);
  FLinkHandlers := TLinkHandlerDict.Create([doOwnsValues]);
end;

destructor TAgentHandlerRegistry.Destroy;
begin
  FNodeHandlers.Free;
  FLinkHandlers.Free;
  inherited;
end;

procedure TAgentHandlerRegistry.RegisterNodeHandler(const AName: string;
  AHandler: TAIAgentsNodeOnExecute);
var
  W: TNodeHandlerWrapper;
begin
  W := TNodeHandlerWrapper.Create;
  W.Handler := AHandler;
  FNodeHandlers.AddOrSetValue(AName, W);
end;

procedure TAgentHandlerRegistry.RegisterLinkHandler(const AName: string;
  AHandler: TAIAgentsLinkOnExecute);
var
  W: TLinkHandlerWrapper;
begin
  W := TLinkHandlerWrapper.Create;
  W.Handler := AHandler;
  FLinkHandlers.AddOrSetValue(AName, W);
end;

function TAgentHandlerRegistry.FindNodeHandler(const AName: string): TAIAgentsNodeOnExecute;
var
  W: TNodeHandlerWrapper;
begin
  Result := nil;
  if FNodeHandlers.TryGetValue(AName, W) then
    Result := W.Handler;
end;

function TAgentHandlerRegistry.FindLinkHandler(const AName: string): TAIAgentsLinkOnExecute;
var
  W: TLinkHandlerWrapper;
begin
  Result := nil;
  if FLinkHandlers.TryGetValue(AName, W) then
    Result := W.Handler;
end;

initialization
  TEngineRegistry.FInstance        := TEngineRegistry.Create;
  TAgentHandlerRegistry.FInstance  := TAgentHandlerRegistry.Create;

finalization
  TEngineRegistry.FInstance.Free;
  TAgentHandlerRegistry.FInstance.Free;

end.
