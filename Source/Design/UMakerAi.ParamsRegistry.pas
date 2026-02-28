// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// --------- FPC PORT --------------------
// Adaptaciones respecto a la version Delphi:
//   - System.Classes/SysUtils/Generics.Collections → nombres sin prefijo System.
//   - TDictionary<K,V>   → specialize TDictionary<K,V>
//   - TList<string>       → specialize TList<string>
//   - for var SL in ...   → for SL in ... (sin inline var)
//   - var List := TList<string>.Create → var + separado
//   - List.ToArray / Keys.ToArray → loop manual BuildStringArray
//   - ModelName.IsEmpty → ModelName = ''
//   - StartsWith('@')    → Copy(s,1,1) = '@'
//   - Substring(1)       → Copy(s,2,MaxInt)
//   - StartsWith(x+'@')  → Copy(key,1,Length(x)+1) = x+'@'
//   - var Valor :=       → eliminado (era solo lectura no usada)
//   - FInstance class var con finalization para singleton
unit UMakerAi.ParamsRegistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  UMakerAi.Chat, uMakerAi.Embeddings;

type
  // FPC: TAiStringArray de Delphi → alias array dinamico
  TAiStringArray = array of string;

  TAiChatClass = class of TAiChat;

  // ---------------------------------------------------------------------------
  //  TAiChatFactory  —  Singleton factory para drivers de chat
  // ---------------------------------------------------------------------------
  TAiChatFactory = class
  private
    class var FInstance: TAiChatFactory;

    FRegisteredClasses: specialize TDictionary<string, TAiChatClass>;
    // Clave: "DriverName" o "DriverName@ModelName"
    FUserParams:        specialize TDictionary<string, TStringList>;
    FCustomModels:      specialize TDictionary<string, string>;

    class function GetCompositeKey(const DriverName, ModelName: string): string; static;

  public
    constructor Create;
    destructor  Destroy; override;
    class function Instance: TAiChatFactory;

    procedure RegisterDriver(AClass: TAiChatClass); overload;
    procedure RegisterDriver(AClass: TAiChatClass; const ADriverName: string); overload;

    procedure GetDriverParams(const DriverName, ModelName: string;
                              Params: TStrings; ExpandVariables: Boolean = True);
    function  CreateDriver(const DriverName: string): TAiChat;
    function  GetRegisteredDrivers: TAiStringArray;
    function  HasDriver(const DriverName: string): Boolean;

    procedure RegisterUserParam(const DriverName, ModelName, ParamName, ParamValue: string); overload;
    procedure RegisterUserParam(const DriverName, ParamName, ParamValue: string);           overload;

    procedure ClearRegisterParams(const DriverName: string; ModelName: string = '');

    procedure RegisterCustomModel(const DriverName, CustomModelName, ModelBaseName: string);
    function  GetBaseModel(const DriverName, CustomModel: string): string;
    function  GetCustomModels(const DriverName: string): TAiStringArray;
    function  HasCustomModel(const DriverName, CustomModelName: string): Boolean;
    procedure ClearCustomModels(const DriverName: string);
  end;

  // ---------------------------------------------------------------------------
  //  TAiEmbeddingFactory  —  Singleton factory para drivers de embeddings
  // ---------------------------------------------------------------------------
  TAiEmbeddingsClass = class of TAiEmbeddings;

  TAiEmbeddingFactory = class
  private
    class var FInstance: TAiEmbeddingFactory;

    FRegisteredClasses: specialize TDictionary<string, TAiEmbeddingsClass>;
    FUserParams:        specialize TDictionary<string, TStringList>;

    class function GetCompositeKey(const DriverName, ModelName: string): string; static;

  public
    constructor Create;
    destructor  Destroy; override;
    class function Instance: TAiEmbeddingFactory;

    procedure RegisterDriver(AClass: TAiEmbeddingsClass);
    function  CreateDriver(const DriverName: string): TAiEmbeddings;
    procedure GetDriverParams(const DriverName, ModelName: string;
                              Params: TStrings; ExpandVariables: Boolean = True);
    function  GetRegisteredDrivers: TAiStringArray;
    function  HasDriver(const DriverName: string): Boolean;

    procedure RegisterUserParam(const DriverName, ModelName, ParamName, ParamValue: string); overload;
    procedure RegisterUserParam(const DriverName, ParamName, ParamValue: string);           overload;
    procedure ClearRegisterParams(const DriverName: string; ModelName: string = '');
  end;

implementation

// ===========================================================================
//  Helpers locales
// ===========================================================================

// Construye un TAiStringArray a partir de las Keys de un diccionario
function BuildStringArray(Dict: specialize TDictionary<string, TAiChatClass>): TAiStringArray; overload;
var
  Key: string;
  I:   Integer;
begin
  SetLength(Result, Dict.Count);
  I := 0;
  for Key in Dict.Keys do
  begin
    Result[I] := Key;
    Inc(I);
  end;
end;

function BuildStringArrayEmb(Dict: specialize TDictionary<string, TAiEmbeddingsClass>): TAiStringArray;
var
  Key: string;
  I:   Integer;
begin
  SetLength(Result, Dict.Count);
  I := 0;
  for Key in Dict.Keys do
  begin
    Result[I] := Key;
    Inc(I);
  end;
end;

function BuildStringArrayStr(Dict: specialize TDictionary<string, string>): TAiStringArray;
var
  Key: string;
  I:   Integer;
begin
  SetLength(Result, Dict.Count);
  I := 0;
  for Key in Dict.Keys do
  begin
    Result[I] := Key;
    Inc(I);
  end;
end;

// ===========================================================================
//  TAiChatFactory
// ===========================================================================

class function TAiChatFactory.GetCompositeKey(const DriverName, ModelName: string): string;
begin
  if ModelName = '' then
    Result := DriverName
  else
    Result := DriverName + '@' + ModelName;
end;

constructor TAiChatFactory.Create;
begin
  inherited;
  FRegisteredClasses := specialize TDictionary<string, TAiChatClass>.Create;
  FUserParams        := specialize TDictionary<string, TStringList>.Create;
  FCustomModels      := specialize TDictionary<string, string>.Create;
end;

destructor TAiChatFactory.Destroy;
var
  SL: TStringList;
begin
  // FPC: no soporta "for var SL in ..." — usamos variable declarada
  for SL in FUserParams.Values do
    SL.Free;
  FUserParams.Free;
  FRegisteredClasses.Free;
  FCustomModels.Free;
  inherited;
end;

class function TAiChatFactory.Instance: TAiChatFactory;
begin
  if not Assigned(FInstance) then
    FInstance := TAiChatFactory.Create;
  Result := FInstance;
end;

procedure TAiChatFactory.RegisterDriver(AClass: TAiChatClass);
begin
  FRegisteredClasses.AddOrSetValue(AClass.GetDriverName, AClass);
end;

procedure TAiChatFactory.RegisterDriver(AClass: TAiChatClass; const ADriverName: string);
begin
  if ADriverName <> '' then
    FRegisteredClasses.AddOrSetValue(ADriverName, AClass);
end;

procedure TAiChatFactory.GetDriverParams(const DriverName, ModelName: string;
                                         Params: TStrings; ExpandVariables: Boolean);
var
  DriverClass:   TAiChatClass;
  UserParamList: TStringList;
  I:             Integer;
  EnvVarName,
  EnvVarValue,
  Key:           string;
begin
  Params.Clear;

  // Nivel 1: parámetros por defecto de la clase del driver
  if FRegisteredClasses.TryGetValue(DriverName, DriverClass) then
    DriverClass.RegisterDefaultParams(Params);

  Params.Text := Trim(Params.Text);

  // Nivel 2: parámetros personalizados del DRIVER
  Key := GetCompositeKey(DriverName, '');
  if FUserParams.TryGetValue(Key, UserParamList) then
    for I := 0 to UserParamList.Count - 1 do
      Params.Values[UserParamList.Names[I]] := UserParamList.ValueFromIndex[I];

  // Nivel 3: parámetros personalizados del MODELO
  if ModelName <> '' then
  begin
    Key := GetCompositeKey(DriverName, ModelName);
    if FUserParams.TryGetValue(Key, UserParamList) then
      for I := 0 to UserParamList.Count - 1 do
        Params.Values[UserParamList.Names[I]] := UserParamList.ValueFromIndex[I];
  end;

  // Expansión de variables de entorno (valores que empiezan con '@')
  if ExpandVariables then
    for I := Params.Count - 1 downto 0 do
      if (Trim(Params[I]) <> '') and (Params.ValueFromIndex[I] <> '') and
         (Copy(Params.ValueFromIndex[I], 1, 1) = '@') then
      begin
        EnvVarName  := Copy(Params.ValueFromIndex[I], 2, MaxInt);
        EnvVarValue := Trim(GetEnvironmentVariable(EnvVarName));
        if EnvVarValue <> '' then
          Params.ValueFromIndex[I] := EnvVarValue;
      end;
end;

function TAiChatFactory.CreateDriver(const DriverName: string): TAiChat;
var
  DriverClass: TAiChatClass;
begin
  Result := nil;
  if FRegisteredClasses.TryGetValue(DriverName, DriverClass) then
    Result := DriverClass.CreateInstance(nil);
end;

function TAiChatFactory.GetRegisteredDrivers: TAiStringArray;
begin
  Result := BuildStringArray(FRegisteredClasses);
end;

function TAiChatFactory.HasDriver(const DriverName: string): Boolean;
begin
  Result := FRegisteredClasses.ContainsKey(DriverName);
end;

procedure TAiChatFactory.ClearRegisterParams(const DriverName: string; ModelName: string);
var
  UserParamList: TStringList;
  Key:           string;
begin
  Key := GetCompositeKey(DriverName, ModelName);
  if FUserParams.TryGetValue(Key, UserParamList) then
    UserParamList.Clear;
end;

procedure TAiChatFactory.RegisterUserParam(const DriverName, ModelName, ParamName, ParamValue: string);
var
  UserParamList: TStringList;
  Key:           string;
begin
  Key := GetCompositeKey(DriverName, ModelName);
  if not FUserParams.TryGetValue(Key, UserParamList) then
  begin
    UserParamList := TStringList.Create;
    FUserParams.Add(Key, UserParamList);
  end;
  UserParamList.Values[ParamName] := ParamValue;
end;

procedure TAiChatFactory.RegisterUserParam(const DriverName, ParamName, ParamValue: string);
begin
  RegisterUserParam(DriverName, '', ParamName, ParamValue);
end;

procedure TAiChatFactory.RegisterCustomModel(const DriverName, CustomModelName, ModelBaseName: string);
var
  Key: string;
begin
  if CustomModelName = '' then
    raise Exception.Create('CustomModelName cannot be empty when registering a custom model.');
  if ModelBaseName = '' then
    raise Exception.Create('ModelBaseName cannot be empty when registering a custom model.');

  Key := GetCompositeKey(DriverName, CustomModelName);
  FCustomModels.AddOrSetValue(Key, ModelBaseName);
end;

function TAiChatFactory.GetBaseModel(const DriverName, CustomModel: string): string;
var
  CompositeKey: string;
begin
  CompositeKey := GetCompositeKey(DriverName, CustomModel);
  if not FCustomModels.TryGetValue(CompositeKey, Result) then
    Result := CustomModel;
end;

function TAiChatFactory.GetCustomModels(const DriverName: string): TAiStringArray;
var
  List:         specialize TList<string>;
  CompositeKey: string;
  CustomModel:  string;
  I:            Integer;
begin
  List := specialize TList<string>.Create;
  try
    for CompositeKey in FCustomModels.Keys do
      // FPC: Copy en vez de StartsWith
      if Copy(CompositeKey, 1, Length(DriverName) + 1) = DriverName + '@' then
      begin
        // Extrae la parte posterior al '@'
        CustomModel := Copy(CompositeKey, Length(DriverName) + 2, MaxInt);
        List.Add(CustomModel);
      end;

    // FPC: no usar Move() con strings gestionados — copiar elemento a elemento
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I];
  finally
    List.Free;
  end;
end;

function TAiChatFactory.HasCustomModel(const DriverName, CustomModelName: string): Boolean;
begin
  Result := FCustomModels.ContainsKey(GetCompositeKey(DriverName, CustomModelName));
end;

procedure TAiChatFactory.ClearCustomModels(const DriverName: string);
var
  KeysToRemove: specialize TList<string>;
  CompositeKey: string;
begin
  KeysToRemove := specialize TList<string>.Create;
  try
    for CompositeKey in FCustomModels.Keys do
      if Copy(CompositeKey, 1, Length(DriverName) + 1) = DriverName + '@' then
        KeysToRemove.Add(CompositeKey);

    for CompositeKey in KeysToRemove do
      FCustomModels.Remove(CompositeKey);
  finally
    KeysToRemove.Free;
  end;
end;

// ===========================================================================
//  TAiEmbeddingFactory
// ===========================================================================

class function TAiEmbeddingFactory.GetCompositeKey(const DriverName, ModelName: string): string;
begin
  if ModelName = '' then
    Result := DriverName
  else
    Result := DriverName + '@' + ModelName;
end;

constructor TAiEmbeddingFactory.Create;
begin
  inherited;
  FRegisteredClasses := specialize TDictionary<string, TAiEmbeddingsClass>.Create;
  FUserParams        := specialize TDictionary<string, TStringList>.Create;
end;

destructor TAiEmbeddingFactory.Destroy;
var
  SL: TStringList;
begin
  for SL in FUserParams.Values do
    SL.Free;
  FUserParams.Free;
  FRegisteredClasses.Free;
  inherited;
end;

class function TAiEmbeddingFactory.Instance: TAiEmbeddingFactory;
begin
  if not Assigned(FInstance) then
    FInstance := TAiEmbeddingFactory.Create;
  Result := FInstance;
end;

procedure TAiEmbeddingFactory.RegisterDriver(AClass: TAiEmbeddingsClass);
begin
  FRegisteredClasses.AddOrSetValue(AClass.GetDriverName, AClass);
end;

procedure TAiEmbeddingFactory.GetDriverParams(const DriverName, ModelName: string;
                                              Params: TStrings; ExpandVariables: Boolean);
var
  DriverClass:   TAiEmbeddingsClass;
  UserParamList: TStringList;
  I:             Integer;
  EnvVarName,
  EnvVarValue,
  Key:           string;
begin
  Params.Clear;

  if FRegisteredClasses.TryGetValue(DriverName, DriverClass) then
    DriverClass.RegisterDefaultParams(Params);

  Params.Text := Trim(Params.Text);

  Key := GetCompositeKey(DriverName, '');
  if FUserParams.TryGetValue(Key, UserParamList) then
    for I := 0 to UserParamList.Count - 1 do
      Params.Values[UserParamList.Names[I]] := UserParamList.ValueFromIndex[I];

  if ModelName <> '' then
  begin
    Key := GetCompositeKey(DriverName, ModelName);
    if FUserParams.TryGetValue(Key, UserParamList) then
      for I := 0 to UserParamList.Count - 1 do
        Params.Values[UserParamList.Names[I]] := UserParamList.ValueFromIndex[I];
  end;

  if ExpandVariables then
    for I := Params.Count - 1 downto 0 do
      if (Trim(Params[I]) <> '') and (Params.ValueFromIndex[I] <> '') and
         (Copy(Params.ValueFromIndex[I], 1, 1) = '@') then
      begin
        EnvVarName  := Copy(Params.ValueFromIndex[I], 2, MaxInt);
        EnvVarValue := Trim(GetEnvironmentVariable(EnvVarName));
        if EnvVarValue <> '' then
          Params.ValueFromIndex[I] := EnvVarValue;
      end;
end;

function TAiEmbeddingFactory.CreateDriver(const DriverName: string): TAiEmbeddings;
var
  DriverClass: TAiEmbeddingsClass;
begin
  Result := nil;
  if FRegisteredClasses.TryGetValue(DriverName, DriverClass) then
    Result := DriverClass.CreateInstance(nil);
end;

function TAiEmbeddingFactory.GetRegisteredDrivers: TAiStringArray;
begin
  Result := BuildStringArrayEmb(FRegisteredClasses);
end;

function TAiEmbeddingFactory.HasDriver(const DriverName: string): Boolean;
begin
  Result := FRegisteredClasses.ContainsKey(DriverName);
end;

procedure TAiEmbeddingFactory.RegisterUserParam(const DriverName, ModelName, ParamName, ParamValue: string);
var
  UserParamList: TStringList;
  Key:           string;
begin
  Key := GetCompositeKey(DriverName, ModelName);
  if not FUserParams.TryGetValue(Key, UserParamList) then
  begin
    UserParamList := TStringList.Create;
    FUserParams.Add(Key, UserParamList);
  end;
  UserParamList.Values[ParamName] := ParamValue;
end;

procedure TAiEmbeddingFactory.RegisterUserParam(const DriverName, ParamName, ParamValue: string);
begin
  RegisterUserParam(DriverName, '', ParamName, ParamValue);
end;

procedure TAiEmbeddingFactory.ClearRegisterParams(const DriverName: string; ModelName: string);
var
  UserParamList: TStringList;
  Key:           string;
begin
  Key := GetCompositeKey(DriverName, ModelName);
  if FUserParams.TryGetValue(Key, UserParamList) then
    UserParamList.Clear;
end;

// ===========================================================================
//  initialization / finalization
// ===========================================================================

initialization
  // Las instancias se crean bajo demanda (lazy singleton)

finalization
  if Assigned(TAiChatFactory.FInstance) then
  begin
    TAiChatFactory.FInstance.Free;
    TAiChatFactory.FInstance := nil;
  end;

  if Assigned(TAiEmbeddingFactory.FInstance) then
  begin
    TAiEmbeddingFactory.FInstance.Free;
    TAiEmbeddingFactory.FInstance := nil;
  end;

end.
