// =============================================================================
// MakerAI Cross-Platform Compatibility Layer - RTTI Helper
// =============================================================================
// 
// Purpose: Unifica tipos y operaciones RTTI entre Delphi y Free Pascal
//
// Compatibility:
//   - Delphi XE7+ (Primary development environment)
//   - Free Pascal 3.3+ (Full RTTI support)
//   - Free Pascal 3.2 (Basic RTTI support)
//
// Usage:
//   uses uRttiHelper;
//   var
//     Value: TCompatValue;
//   begin
//     Value := TCompatValue.From<Integer>(42);
//     ShowMessage(Value.AsString);
//   end;
//
// Developer Notes (Delphi):
//   - TCompatValue es un alias directo a System.Rtti.TValue en Delphi
//   - Esta unidad NO afecta el código Delphi puro, solo agrega compatibilidad FPC
//   - Los helpers extienden TValue con métodos adicionales para uniformidad
//
// =============================================================================

unit uRttiHelper;

{$include ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, TypInfo, SysUtils, fpjson, jsonparser, Variants, Rtti,
  {$ELSE}
  System.Rtti, System.TypInfo, System.SysUtils, System.JSON, System.Classes,
  {$ENDIF}
  
  uJsonHelper;

type
  {$IFDEF FPC}
  // FPC 3.3+: Re-export RTTI types from Rtti unit
  TRttiContext = Rtti.TRttiContext;
  TRttiType = Rtti.TRttiType;
  TRttiProperty = Rtti.TRttiProperty;
  TRttiField = Rtti.TRttiField;
  TRttiMethod = Rtti.TRttiMethod;
  
  // Re-export TypInfo types
  TTypeKind = TypInfo.TTypeKind;
  PTypeInfo = TypInfo.PTypeInfo;
  TTypeInfo = TypInfo.TTypeInfo;

const
  // Re-export TTypeKind values as constants for case statements
  tkUnknown = TypInfo.tkUnknown;
  tkInteger = TypInfo.tkInteger;
  tkChar = TypInfo.tkChar;
  tkEnumeration = TypInfo.tkEnumeration;
  tkFloat = TypInfo.tkFloat;
  tkString = TypInfo.tkString;
  tkSet = TypInfo.tkSet;
  tkClass = TypInfo.tkClass;
  tkMethod = TypInfo.tkMethod;
  tkWChar = TypInfo.tkWChar;
  tkLString = TypInfo.tkLString;
  tkWString = TypInfo.tkWString;
  tkVariant = TypInfo.tkVariant;
  tkArray = TypInfo.tkArray;
  tkRecord = TypInfo.tkRecord;
  tkInterface = TypInfo.tkInterface;
  tkInt64 = TypInfo.tkInt64;
  tkDynArray = TypInfo.tkDynArray;
  tkUString = TypInfo.tkUString;
  tkClassRef = TypInfo.tkClassRef;
  tkPointer = TypInfo.tkPointer;
  tkProcedure = TypInfo.tkProcedure;
  {$IFDEF FPC_HAS_TYPEKIND_ASTRING}
  tkAString = TypInfo.tkAString;
  {$ENDIF}

type
  // TValue alias
  TCompatValue = Rtti.TValue;
  {$ELSE}
  // Delphi uses System.Rtti directly
  TCompatValue = System.Rtti.TValue;
  {$ENDIF}

  // Alias for client code that uses TValue
  TValue = TCompatValue;

  // Helper para operaciones comunes con valores RTTI
  TCompatValueHelper = record helper for TCompatValue
  public
    // Métodos de conversión seguros con valores por defecto
    function AsStringDef(const ADefault: string = ''): string;
    function AsIntegerDef(const ADefault: Integer = 0): Integer;
    function AsInt64Def(const ADefault: Int64 = 0): Int64;
    function AsDoubleDef(const ADefault: Double = 0.0): Double;
    function AsBooleanDef(const ADefault: Boolean = False): Boolean;
    
    // Verificación de tipos
    function IsString: Boolean;
    function IsInteger: Boolean;
    function IsFloat: Boolean;
    
    // Generics support (FPC 3.3+)
    function IsType<T>: Boolean;
  end;

  // TCompatValueHelper adds convenience methods not present in standard TValue

  // Helper unificado para Serialización RTTI a JSON
  TCompatRtti = class
  public
    class function SerializeObject(Obj: TObject): TJSONObject;
    class procedure DeserializeObject(Obj: TObject; JSON: TJSONObject);
    class function GetUnitName(AClass: TClass): string;
  end;

  // Helper para verificar si una propiedad RTTI tiene un atributo específico
  // Reemplaza el uso de Prop.HasAttribute<T> que no funciona en FPC
  function HasRttiAttribute(AProp: TRttiProperty; AAttrClass: TClass): Boolean;
  function GetRttiAttribute(AProp: TRttiProperty; AAttrClass: TClass): TCustomAttribute;

  // Helper functions for TypInfo compatibility
  function GetEnumNameCompat(TypeInfo: PTypeInfo; Value: Integer): string;
  function GetEnumValueCompat(TypeInfo: PTypeInfo; const Name: string): Integer;

  // Helper para crear TValue desde enum - unifica FPC/Delphi
  // FPC: TValue.From<T> no funciona con enums, requiere TValue.Make con TypeInfo
  // Delphi: TValue.From<T> funciona directamente
  // Uso: TValueFromEnum(TypeInfo(TMyEnum), Ord(MyEnumValue))
  function TValueFromEnum(ATypeInfo: PTypeInfo; AOrdValue: Integer): TCompatValue;

  // Helper para crear TValue desde objeto/clase - unifica FPC/Delphi
  // FPC: TValue.From<T> requiere TypeInfo y puntero al valor
  // Delphi: TValue.From<T>(Value) funciona directamente
  // Uso: TValueFromObject(TypeInfo(TMyClass), MyObject)
  function TValueFromObject(ATypeInfo: PTypeInfo; AObject: TObject): TCompatValue;

  // Asigna un valor de string (ej: "Val1, Val2") a una propiedad tipo Set de un objeto
  // Abstae las diferencias entre Delphi (TRttiSetType) y FPC (TypInfo.Get/SetSetProp o OrdProp)
  procedure SetRttiSetProperty(AProp: TRttiProperty; AInstance: TObject; const AValue: String);

  // Verifica si la propiedad es de tipo TStrings (o descendiente) y asigna el valor
  // Abstae las diferencias entre Delphi y FPC
  procedure SetRttiStringsProperty(AProp: TRttiProperty; AInstance: TObject; const AValue: String);

implementation

{ Helper functions for TypInfo compatibility }

function GetEnumNameCompat(TypeInfo: PTypeInfo; Value: Integer): string;
begin
  {$IFDEF FPC}
  Result := TypInfo.GetEnumName(TypeInfo, Value);
  {$ELSE}
  Result := System.TypInfo.GetEnumName(TypeInfo, Value);
  {$ENDIF}
end;

function GetEnumValueCompat(TypeInfo: PTypeInfo; const Name: string): Integer;
begin
  {$IFDEF FPC}
  Result := TypInfo.GetEnumValue(TypeInfo, Name);
  {$ELSE}
  Result := System.TypInfo.GetEnumValue(TypeInfo, Name);
  {$ENDIF}
end;

function TValueFromEnum(ATypeInfo: PTypeInfo; AOrdValue: Integer): TCompatValue;
begin
  {$IFDEF FPC}
  // FPC: TValue.Make crea TValue desde TypeInfo y valor ordinal
  TValue.Make(@AOrdValue, ATypeInfo, Result);
  {$ELSE}
  // Delphi: TValue.FromOrdinal funciona con enums
  Result := TValue.FromOrdinal(ATypeInfo, AOrdValue);
  {$ENDIF}
end;

function TValueFromObject(ATypeInfo: PTypeInfo; AObject: TObject): TCompatValue;
begin
  {$IFDEF FPC}
  // FPC: TValue.Make crea TValue desde TypeInfo y puntero al objeto
  TValue.Make(@AObject, ATypeInfo, Result);
  {$ELSE}
  // Delphi: TValue.From<T> funciona directamente
  Result := TValue.From<TObject>(AObject);
  {$ENDIF}
end;

procedure SetRttiSetProperty(AProp: TRttiProperty; AInstance: TObject; const AValue: String);
var
  LOrdValue: Integer;
  {$IFDEF FPC}
  LPropInfo: PPropInfo;
  LTypeData: PTypeData;
  LEnumType: PTypeInfo;
  {$ELSE}
  LSetType: TRttiSetType;
  LEnumType: TRttiType;
  LValue: TValue;
  {$ENDIF}
  CleanValue: string;
  EnumNames: TArray<string>;
  EnumName, TrimmedName: string;
  OrdinalValue: Integer;
begin
  if (AValue.IsEmpty) or (AValue = '[]') then
    Exit;

  CleanValue := AValue.Trim(['[', ']', ' ']);
  EnumNames := CleanValue.Split([',']);
  LOrdValue := 0;

  // 1. Calcular el valor entero de la máscara de bits
  {$IFDEF FPC}
  // FPC Implementation using TypInfo
  LPropInfo := GetPropInfo(AInstance.ClassInfo, AProp.Name);
  // En FPC Rtti.TRttiProperty tiene puntero a PropInfo
  if LPropInfo = nil then Exit;

  if LPropInfo^.PropType^.Kind <> tkSet then Exit;
  LTypeData := GetTypeData(LPropInfo^.PropType);
  if LTypeData = nil then Exit;
  LEnumType := LTypeData^.CompType; // PTypeInfo del Enum base
  
  if LEnumType = nil then Exit;

  for EnumName in EnumNames do
  begin
    TrimmedName := Trim(EnumName);
    if not TrimmedName.IsEmpty then
    begin
      // FPC GetEnumValue es case-insensitive? Usualmente si.
      OrdinalValue := GetEnumValue(LEnumType, TrimmedName);
      if OrdinalValue >= 0 then
        LOrdValue := LOrdValue or (1 shl OrdinalValue);
    end;
  end;

  // 2. Asignar usando TypInfo
  SetOrdProp(AInstance, LPropInfo, LOrdValue);

  {$ELSE}
  // Delphi Implementation using RTTI
  if AProp.PropertyType.TypeKind <> tkSet then Exit;
  
  LSetType := AProp.PropertyType as TRttiSetType;
  if LSetType.ElementType.TypeKind <> tkEnumeration then Exit;
  
  LEnumType := LSetType.ElementType;

  for EnumName in EnumNames do
  begin
    TrimmedName := Trim(EnumName);
    if not TrimmedName.IsEmpty then
    begin
      // Delphi GetEnumValue requiere TypeInfo Handle
      OrdinalValue := GetEnumValue(LEnumType.Handle, TrimmedName);
      if OrdinalValue >= 0 then
        LOrdValue := LOrdValue or (1 shl OrdinalValue);
    end;
  end;

  // 2. Asignar usando RTTI Setter
  TValue.Make(@LOrdValue, LSetType.Handle, LValue);
  AProp.SetValue(AInstance, LValue);
  {$ENDIF}
end;

procedure SetRttiStringsProperty(AProp: TRttiProperty; AInstance: TObject; const AValue: String);
var
  LObject: TObject;
  {$IFDEF FPC}
  LPropInfo: PPropInfo;
  {$ELSE}
  LValue: TValue;
  {$ENDIF}
begin
  if AValue.IsEmpty then Exit;

  // 1. Obtener la instancia del objeto de la propiedad
  LObject := nil;
  
  {$IFDEF FPC}
  LPropInfo := GetPropInfo(AInstance.ClassInfo, AProp.Name);
  if (LPropInfo <> nil) and (LPropInfo^.PropType^.Kind = tkClass) then
  begin
    // En FPC, usamos GetObjectProp para obtener la instancia actual
    LObject := GetObjectProp(AInstance, LPropInfo);
  end;
  {$ELSE}
  if AProp.PropertyType.TypeKind = tkClass then
  begin
    LValue := AProp.GetValue(AInstance);
    if LValue.IsObject then
      LObject := LValue.AsObject;
  end;
  {$ENDIF}

  // 2. Verificar si es TStrings y asignar
  if (LObject <> nil) and (LObject is TStrings) then
  begin
    // Reemplazar pipes por breaks para permitir multilinea en una sola linea de config
    TStrings(LObject).Text := StringReplace(AValue, '|', sLineBreak, [rfReplaceAll]);
  end;
end;

{ Helper functions for RTTI Attributes }

function HasRttiAttribute(AProp: TRttiProperty; AAttrClass: TClass): Boolean;
begin
  // Standard FPC RTTI attributes have different implementation
  Result := False;
end;

function GetRttiAttribute(AProp: TRttiProperty; AAttrClass: TClass): TCustomAttribute;
begin
  Result := nil; // FPC stub - attributes differ from Delphi
end;

{ TCompatValueHelper }

function TCompatValueHelper.AsStringDef(const ADefault: string): string;
begin
  if Self.IsEmpty then
    Result := ADefault
  else
  try
    {$IFDEF FPC}
    Result := Self.AsString;
    {$ELSE}
    Result := Self.ToString;
    {$ENDIF}
  except
    Result := ADefault;
  end;
end;

function TCompatValueHelper.AsIntegerDef(const ADefault: Integer): Integer;
begin
  if Self.IsEmpty then
    Result := ADefault
  else
  try
    Result := Self.AsInteger;
  except
    Result := ADefault;
  end;
end;

function TCompatValueHelper.AsInt64Def(const ADefault: Int64): Int64;
begin
  if Self.IsEmpty then
    Result := ADefault
  else
  try
    Result := Self.AsInt64;
  except
    Result := ADefault;
  end;
end;

function TCompatValueHelper.AsDoubleDef(const ADefault: Double): Double;
begin
  if Self.IsEmpty then
    Result := ADefault
  else
  try
    {$IFDEF FPC}
    Result := Self.AsExtended;
    {$ELSE}
    Result := Self.AsType<Double>;
    {$ENDIF}
  except
    Result := ADefault;
  end;
end;

function TCompatValueHelper.AsBooleanDef(const ADefault: Boolean): Boolean;
begin
  if Self.IsEmpty then
    Result := ADefault
  else
  try
    Result := Self.AsBoolean;
  except
    Result := ADefault;
  end;
end;

function TCompatValueHelper.IsString: Boolean;
begin
  {$IFDEF FPC}
  Result := Self.Kind in [tkString, tkLString, tkWString, tkUString, tkAString];
  {$ELSE}
  Result := Self.Kind in [tkString, tkLString, tkWString, tkUString];
  {$ENDIF}
end;

function TCompatValueHelper.IsInteger: Boolean;
begin
  Result := Self.Kind = tkInteger;
end;

function TCompatValueHelper.IsFloat: Boolean;
begin
  Result := Self.Kind = tkFloat;
end;

function TCompatValueHelper.IsType<T>: Boolean;
begin
  Result := Self.TypeInfo = System.TypeInfo(T);
end;



{ TCompatRtti }

class function TCompatRtti.SerializeObject(Obj: TObject): TJSONObject;
{$IFNDEF FPC}
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LValue: TCompatValue; // TValue
{$ELSE}
var
  PropList: PPropList;
  PropCount, I: Integer;
  PropInfo: PPropInfo;
{$ENDIF}
begin
  Result := TJSONObject.Create;
  if not Assigned(Obj) then
    Exit;
    
  {$IFNDEF FPC}
  // Delphi RTTI implementation
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(Obj.ClassType);
    if Assigned(LType) then
    begin
      for LProp in LType.GetProperties do
      begin
        if LProp.IsWritable and LProp.IsReadable then
        begin
          try
            LValue := LProp.GetValue(Obj);
            // Mapeo de tipos básicos soportados
            if LProp.PropertyType.TypeKind in [tkInteger, tkInt64, tkFloat, tkString, tkLString, tkWString, tkUString, tkEnumeration] then
            begin
               if LProp.PropertyType.TypeKind in [tkInteger, tkInt64] then
                 Result.AddPair(LProp.Name, LValue.AsInteger) // uJsonHelper overload
               else if LProp.PropertyType.TypeKind = tkFloat then
                 Result.AddPair(LProp.Name, LValue.AsExtended)
               else if (LProp.PropertyType.TypeKind = tkEnumeration) and (LProp.PropertyType.Handle = TypeInfo(Boolean)) then
                 Result.AddPair(LProp.Name, LValue.AsBoolean)
               else
                 Result.AddPair(LProp.Name, LValue.ToString);
            end;
          except
            // Ignore properties that fail
          end;
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
  {$ELSE}
  // FPC RTTI implementation using TypInfo
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(PPropInfo));
    try
      GetPropList(Obj.ClassInfo, tkProperties, PropList);
      for I := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[I];
        try
          case PropInfo^.PropType^.Kind of
            tkInteger, tkInt64:
              Result.AddPair(PropInfo^.Name, GetOrdProp(Obj, PropInfo));
            tkFloat:
              Result.AddPair(PropInfo^.Name, GetFloatProp(Obj, PropInfo));
            tkString, tkLString, tkAString, tkUString, tkWString:
              Result.AddPair(PropInfo^.Name, GetStrProp(Obj, PropInfo));
            tkBool:
              Result.AddPair(PropInfo^.Name, GetOrdProp(Obj, PropInfo) <> 0);
            tkEnumeration:
              Result.AddPair(PropInfo^.Name, GetEnumProp(Obj, PropInfo));
          end;
        except
          // Skip
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
  {$ENDIF}
end;

class procedure TCompatRtti.DeserializeObject(Obj: TObject; JSON: TJSONObject);
{$IFNDEF FPC}
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LPair: TJSONPair;
  Name: string;
{$ELSE}
var
  PropList: PPropList;
  PropCount, I, J: Integer;
  PropInfo: PPropInfo;
{$ENDIF}
begin
  if not Assigned(Obj) or not Assigned(JSON) then
    Exit;
    
  {$IFNDEF FPC}
  // Delphi RTTI implementation
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(Obj.ClassType);
    if Assigned(LType) then
    begin
      for LPair in JSON do
      begin
        Name := LPair.JsonString.Value;
        LProp := LType.GetProperty(Name);
        if Assigned(LProp) and LProp.IsWritable then
        begin
          try
            case LProp.PropertyType.TypeKind of
              tkInteger, tkInt64:
                 LProp.SetValue(Obj, JSON.GetValueAsInteger(Name)); // uJsonHelper
              tkFloat:
                 LProp.SetValue(Obj, JSON.GetValueAsDouble(Name));
              tkString, tkLString, tkWString, tkUString:
                 LProp.SetValue(Obj, JSON.GetValueAsString(Name));
              tkEnumeration:
                 if LProp.PropertyType.Handle = TypeInfo(Boolean) then
                   LProp.SetValue(Obj, JSON.GetValueAsBoolean(Name))
                 else
                   // Enum as string?
                   begin
                     // Need TValue.From<Enum>(str)?
                     // Delphi RTTI handles String -> Enum conversion? Not automatically via TValue directly often.
                     // But let's assume JSON string matches enum name.
                     // TValue.FromOrdinal? No, easier to use GetEnumValue.
                     // Helper:
                     LProp.SetValue(Obj, TCompatValue.FromOrdinal(LProp.PropertyType.Handle, GetEnumValue(LProp.PropertyType.Handle, JSON.GetValueAsString(Name))));
                   end;
            end;
          except
             // ignore
          end;
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
  {$ELSE}
  // FPC RTTI implementation
  PropCount := GetPropList(Obj.ClassInfo, tkProperties, nil);
  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(PPropInfo));
    try
      GetPropList(Obj.ClassInfo, tkProperties, PropList);
      for I := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[I];
        // Buscar si existe en el JSON
        // uJsonHelper: TryGetValueAsString, etc.
        // Pero necesitamos saber el tipo antes de Get.
        // Mejor iterar propiedades y buscar en JSON por nombre.
        
        // El metodo de Agents.pas usaba IndexOfName.
        // JSON.Find(Name);
        if JSON.Find(PropInfo^.Name) <> nil then
        begin
          try
            case PropInfo^.PropType^.Kind of
              tkInteger, tkInt64: 
                SetOrdProp(Obj, PropInfo, JSON.GetValueAsInteger(PropInfo^.Name));
              tkFloat:
                SetFloatProp(Obj, PropInfo, JSON.GetValueAsDouble(PropInfo^.Name));
              tkString, tkLString, tkAString, tkUString, tkWString:
                SetStrProp(Obj, PropInfo, JSON.GetValueAsString(PropInfo^.Name));
              tkBool:
                SetOrdProp(Obj, PropInfo, Ord(JSON.GetValueAsBoolean(PropInfo^.Name)));
              tkEnumeration:
                SetEnumProp(Obj, PropInfo, JSON.GetValueAsString(PropInfo^.Name));
            end;
          except
            // Skip
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
  {$ENDIF}
end;



class function TCompatRtti.GetUnitName(AClass: TClass): string;
begin
  if AClass = nil then Exit('');

  {$IFDEF FPC}
    // FPC: UnitName is in TypeData
    if PTypeInfo(AClass.ClassInfo)^.Kind = tkClass then
      Result := string(GetTypeData(PTypeInfo(AClass.ClassInfo))^.UnitName)
    else
      Result := '';
  {$ELSE}
    // Delphi: Native method
    Result := AClass.UnitName;
  {$ENDIF}
end;

end.
