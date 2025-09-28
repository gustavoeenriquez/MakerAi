unit UMakerAi.ParamsRegistry;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  UMakerAi.Chat;

type
  TAiChatClass = class of TAiChat;


  TAiChatFactory = class
  private
  class var
    FInstance: TAiChatFactory;
    FRegisteredClasses: TDictionary<string, TAiChatClass>;
    // La clave ahora puede ser "DriverName" o "DriverName@ModelName"
    FUserParams: TDictionary<string, TStringList>;

    FCustomModels: TDictionary<string, String>; // DriverName -> TStringList

    // Funci�n interna para crear la clave compuesta.
    class function GetCompositeKey(const DriverName, ModelName: string): string; static;

  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TAiChatFactory;

    // M�todos existentes (algunos con nueva firma)
    procedure RegisterDriver(AClass: TAiChatClass);
    // Ahora acepta un ModelName opcional para obtener los par�metros jer�rquicos.
    procedure GetDriverParams(const DriverName, ModelName: string; Params: TStrings; ExpandVariables: Boolean = True);
    function CreateDriver(const DriverName: string): TAiChat;
    function GetRegisteredDrivers: TArray<string>;
    function HasDriver(const DriverName: string): Boolean;

    // Versi�n principal para registrar un par�metro de un modelo espec�fico.
    procedure RegisterUserParam(const DriverName, ModelName, ParamName, ParamValue: string); Overload;
    // Sobrecarga para registrar un par�metro a nivel de Driver (compatibilidad y conveniencia).
    procedure RegisterUserParam(const DriverName, ParamName, ParamValue: string); Overload;

    // Limpia los par�metros (con sobrecarga para modelo).
    procedure ClearRegisterParams(const DriverName: String; ModelName: string = '');

    // --- Nuevos m�todos para manejar modelos personalizados ---
    procedure RegisterCustomModel(const DriverName, CustomModelName, ModelBaseName: string);
    function GetBaseModel(const DriverName, CustomModel: string): string;
    function GetCustomModels(const DriverName: string): TArray<string>;
    function HasCustomModel(const DriverName, CustomModelName: string): Boolean;
    procedure ClearCustomModels(const DriverName: string);

  end;

implementation

{ TAiChatFactory }

// Funci�n interna para crear la clave
class function TAiChatFactory.GetCompositeKey(const DriverName, ModelName: string): string;
begin
  if ModelName.IsEmpty then
    Result := DriverName
  else
    Result := DriverName + '@' + ModelName;
end;

constructor TAiChatFactory.Create;
begin
  inherited;
  FRegisteredClasses := TDictionary<string, TAiChatClass>.Create;
  FUserParams := TDictionary<string, TStringList>.Create;
  FCustomModels := TDictionary<string, String>.Create;
end;

destructor TAiChatFactory.Destroy;
begin
  for var SL in FUserParams.Values do
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

procedure TAiChatFactory.GetDriverParams(const DriverName, ModelName: string; Params: TStrings; ExpandVariables: Boolean);
var
  DriverClass: TAiChatClass;
  UserParamList: TStringList;
  I: Integer;
  EnvVarName, EnvVarValue, Key: String;
begin
  Params.Clear;

  // Nivel 1: Cargar par�metros por defecto desde la clase del driver
  if FRegisteredClasses.TryGetValue(DriverName, DriverClass) then
    DriverClass.RegisterDefaultParams(Params);

  Params.Text := Trim(Params.Text); // Elimina el �ltimo LineBreak

  // Nivel 2: Fusionar con par�metros personalizados del DRIVER
  Key := GetCompositeKey(DriverName, '');
  if FUserParams.TryGetValue(Key, UserParamList) then
  begin
    For I := 0 to UserParamList.Count - 1 do
      Params.Values[UserParamList.Names[I]] := UserParamList.ValueFromIndex[I];
  end;

  // Nivel 3: Fusionar con par�metros personalizados del MODELO (si se especifica)
  if not ModelName.IsEmpty then
  begin
    Key := GetCompositeKey(DriverName, ModelName);
    if FUserParams.TryGetValue(Key, UserParamList) then
    begin
      For I := 0 to UserParamList.Count - 1 do
        Params.Values[UserParamList.Names[I]] := UserParamList.ValueFromIndex[I];
    end;
  end;

  If ExpandVariables = True then // Debe expandir las variable con las de entorno
  Begin
    // Expansi�n de Variables de Entorno
    for I := Params.Count - 1 downto 0 do
    begin

      Var
      Valor := Params[I];
      if (Trim(Params[I]) <> '') and (Params.ValueFromIndex[I] <> '') and (Params.ValueFromIndex[I].StartsWith('@')) then
      begin
        EnvVarName := Params.ValueFromIndex[I].Substring(1);
        EnvVarValue := Trim(GetEnvironmentVariable(EnvVarName));
        If EnvVarValue <> '' then
          Params.ValueFromIndex[I] := EnvVarValue;
      end;
    end;
  End;
end;

function TAiChatFactory.CreateDriver(const DriverName: string): TAiChat;
var
  DriverClass: TAiChatClass;
begin
  Result := nil;
  if FRegisteredClasses.TryGetValue(DriverName, DriverClass) then
    Result := DriverClass.CreateInstance(Nil);
end;

function TAiChatFactory.GetRegisteredDrivers: TArray<string>;
begin
  Result := FRegisteredClasses.Keys.ToArray;
end;

function TAiChatFactory.HasDriver(const DriverName: string): Boolean;
begin
  Result := FRegisteredClasses.ContainsKey(DriverName);
end;

procedure TAiChatFactory.ClearRegisterParams(const DriverName: String; ModelName: string = '');
var
  UserParamList: TStringList;
  Key: string;
begin
  Key := GetCompositeKey(DriverName, ModelName);
  if FUserParams.TryGetValue(Key, UserParamList) then
  begin
    UserParamList.Clear;
  end;
end;

// Versi�n principal para registrar un par�metro de un modelo espec�fico.
procedure TAiChatFactory.RegisterUserParam(const DriverName, ModelName, ParamName, ParamValue: string);
var
  UserParamList: TStringList;
  Key: string;
begin
  Key := GetCompositeKey(DriverName, ModelName);
  if not FUserParams.TryGetValue(Key, UserParamList) then
  begin
    UserParamList := TStringList.Create;
    FUserParams.Add(Key, UserParamList);
  end;
  UserParamList.Values[ParamName] := ParamValue;
end;

// Sobrecarga para registrar un par�metro a nivel de Driver.
procedure TAiChatFactory.RegisterUserParam(const DriverName, ParamName, ParamValue: string);
begin
  // Llama a la versi�n principal con un ModelName vac�o.
  RegisterUserParam(DriverName, '', ParamName, ParamValue);
end;


// Implementaci�n de los nuevos m�todos para modelos personalizados
// Implementaci�n de los nuevos m�todos para modelos personalizados
// Implementaci�n de los nuevos m�todos para modelos personalizados


procedure TAiChatFactory.RegisterCustomModel(const DriverName, CustomModelName, ModelBaseName: string);
var
  Key : String;
begin
  // Verifica que ModelName no est� vac�o
  if CustomModelName.IsEmpty then
    raise Exception.Create('CustomModelName cannot be empty when registering a custom model.');

  if ModelBaseName.IsEmpty then
    raise Exception.Create('ModelBaseName cannot be empty when registering a custom model.');

  Key := GetCompositeKey(DriverName, CustomModelName);

  FCustomModels.AddOrSetValue(Key, ModelBaseName);  //Adiciona o actualiza el modelo asociado

end;

function TAiChatFactory.GetBaseModel(const DriverName, CustomModel: string): string;
var
  CompositeKey: string;
begin
  // Crea la clave compuesta
  CompositeKey := GetCompositeKey(DriverName, CustomModel);

  // Intenta obtener el ModeloBase para el CustomModel
  if not FCustomModels.TryGetValue(CompositeKey, Result) then
    Result := CustomModel; // Valor por defecto si no se encuentra
end;



function TAiChatFactory.GetCustomModels(const DriverName: string): TArray<string>;
var
  CustomModel: string;
  CompositeKey: string;
begin
  // Recorre el diccionario y filtra los CustomModels para el DriverName dado
  var List: TList<string> := TList<string>.Create;
  try
    for CompositeKey in FCustomModels.Keys do
    begin
      // Verifica si el DriverName coincide con el inicio de la clave compuesta
      if CompositeKey.StartsWith(DriverName + '@') then
      begin
        // Extrae el CustomModel de la clave compuesta
        CustomModel := Copy(CompositeKey, Length(DriverName) + 2, Length(CompositeKey)); // +2 para el @
        List.Add(CustomModel);
      end;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TAiChatFactory.HasCustomModel(const DriverName, CustomModelName: string): Boolean;
var
  CompositeKey: string;
begin
  // Crea la clave compuesta
  CompositeKey := GetCompositeKey(DriverName, CustomModelName);

  // Verifica si la clave compuesta existe en el diccionario
  Result := FCustomModels.ContainsKey(CompositeKey);
end;

procedure TAiChatFactory.ClearCustomModels(const DriverName: string);
var
  CompositeKey: string;
  KeysToRemove: TList<string>;
begin

  // Crea una lista para almacenar las claves que se van a eliminar
  KeysToRemove := TList<string>.Create;
  try
    // Recorre el diccionario y busca las claves que pertenecen al DriverName dado
    for CompositeKey in FCustomModels.Keys do
    begin
      // Verifica si el DriverName coincide con el inicio de la clave compuesta
      if CompositeKey.StartsWith(DriverName + '@') then
      begin
        // A�ade la clave a la lista de claves a eliminar
        KeysToRemove.Add(CompositeKey);
      end;
    end;

    // Elimina las claves encontradas del diccionario
    for CompositeKey in KeysToRemove do
    begin
      FCustomModels.Remove(CompositeKey);
    end;
  finally
    KeysToRemove.Free;
  end;
end;


initialization

// La instancia se crea bajo demanda
finalization

if Assigned(TAiChatFactory.FInstance) then
begin
  TAiChatFactory.FInstance.Free;
  TAiChatFactory.FInstance := nil;
end;

end.
