unit uJSONHelper;

interface

uses
  System.JSON, System.SysUtils;

type
  // Helper unificado para TJSONObject que combina la creaci�n y la navegaci�n segura.
  TJSONObjectHelper = class helper for TJSONObject
  public
    // M�todos para a�adir pares de forma fluida (compatibilidad con versiones antiguas)
    function AddPair(const AKey: string; const AValue: Integer): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: Int64): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: Double): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: Single): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: Boolean): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: string): TJSONObject; overload;

    // M�todos de navegaci�n segura
    function GetValueSafe(const Name: string): TJSONValue;
    function GetValueAsString(const Name: string; const DefaultValue: string = ''): string;
    function GetValueAsInteger(const Name: string; const DefaultValue: Integer = 0): Integer;
    function GetValueAsInt64(const Name: string; const DefaultValue: Int64 = 0): Int64;
    function GetValueAsDouble(const Name: string; const DefaultValue: Double = 0.0): Double;
    function GetValueAsBoolean(const Name: string; const DefaultValue: Boolean = False): Boolean;
    function GetValueAsObject(const Name: string): TJSONObject;
    function GetValueAsArray(const Name: string): TJSONArray;
    function TryGetValueAsString(const Name: string; out Value: string): Boolean;
  end;

  // Helper para TJSONArray para abstraer diferencias entre versiones
  TJSONArrayHelper = class helper for TJSONArray
  public
    function GetItem(Index: Integer): TJSONValue;
    function GetItemAsObject(Index: Integer): TJSONObject;
    function GetItemAsArray(Index: Integer): TJSONArray;
  end;

  // Clase de utilidad para operaciones globales de JSON
  TJSONUtils = class
  public
    class function Parse(const Data: string): TJSONValue; static;
    class function ParseAsObject(const Data: string): TJSONObject; static;
    class function ParseAsArray(const Data: string): TJSONArray; static;
  end;

implementation

{ TJSONObjectHelper }

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Integer): TJSONObject;
begin
  Result := Self.AddPair(AKey, TJSONNumber.Create(AValue));
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Int64): TJSONObject;
begin
  Result := Self.AddPair(AKey, TJSONNumber.Create(AValue));
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Double): TJSONObject;
begin
  Result := Self.AddPair(AKey, TJSONNumber.Create(AValue));
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Single): TJSONObject;
begin
  Result := Self.AddPair(AKey, TJSONNumber.Create(AValue));
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Boolean): TJSONObject;
begin
  Result := Self.AddPair(AKey, TJSONBool.Create(AValue));
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: string): TJSONObject;
begin
  Result := Self.AddPair(AKey, TJSONString.Create(AValue));
end;

function TJSONObjectHelper.GetValueSafe(const Name: string): TJSONValue;
begin
  Result := Self.GetValue(Name); // Devuelve nil si no existe, no lanza excepci�n
end;

function TJSONObjectHelper.GetValueAsString(const Name: string; const DefaultValue: string): string;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and (LValue is TJSONString) then // Tambi�n se puede usar LValue.TryGetValue<string>() en versiones nuevas
    Result := LValue.Value
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.GetValueAsInteger(const Name: string; const DefaultValue: Integer): Integer;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and (LValue is TJSONNumber) then
    Result := Trunc(TJSONNumber(LValue).AsDouble)
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.GetValueAsInt64(const Name: string; const DefaultValue: Int64): Int64;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and (LValue is TJSONNumber) then
    Result := Trunc(TJSONNumber(LValue).AsDouble)
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.GetValueAsDouble(const Name: string; const DefaultValue: Double): Double;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and (LValue is TJSONNumber) then
    Result := TJSONNumber(LValue).AsDouble
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.GetValueAsBoolean(const Name: string; const DefaultValue: Boolean): Boolean;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and (LValue is TJSONBool) then
    Result := TJSONBool(LValue).Value.ToBoolean
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.GetValueAsObject(const Name: string): TJSONObject;
var
  LValue: TJSONValue;
begin
  Result := nil;
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and (LValue is TJSONObject) then
    Result := TJSONObject(LValue);
end;

function TJSONObjectHelper.GetValueAsArray(const Name: string): TJSONArray;
var
  LValue: TJSONValue;
begin
  Result := nil;
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and (LValue is TJSONArray) then
    Result := TJSONArray(LValue);
end;

function TJSONObjectHelper.TryGetValueAsString(const Name: string; out Value: string): Boolean;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := GetValueSafe(Name);
  Result := Assigned(LJSONValue) and (LJSONValue is TJSONString);
  if Result then
    Value := LJSONValue.Value
  else
    Value := '';
end;

{ TJSONArrayHelper }

function TJSONArrayHelper.GetItem(Index: Integer): TJSONValue;
begin
  // Abstrae la diferencia entre .Items[Index] (versiones antiguas) y la propiedad por defecto (versiones nuevas)
  {$IF CompilerVersion >= 35.0} // Delphi 11+
  Result := Self[Index];
  {$ELSE}
  Result := Self.Items[Index];
  {$IFEND}
end;

function TJSONArrayHelper.GetItemAsObject(Index: Integer): TJSONObject;
var
  LItem: TJSONValue;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
  begin
    LItem := GetItem(Index);
    if Assigned(LItem) and (LItem is TJSONObject) then
      Result := TJSONObject(LItem);
  end;
end;

function TJSONArrayHelper.GetItemAsArray(Index: Integer): TJSONArray;
var
  LItem: TJSONValue;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
  begin
    LItem := GetItem(Index);
    if Assigned(LItem) and (LItem is TJSONArray) then
      Result := TJSONArray(LItem);
  end;
end;

{ TJSONUtils }

class function TJSONUtils.Parse(const Data: string): TJSONValue;
begin
  // TJSONObject.ParseJSONValue es el m�todo est�ndar y funciona en todas las versiones.
  // No se necesita directiva de compilador aqu�.
  Result := TJSONObject.ParseJSONValue(Data);
end;

class function TJSONUtils.ParseAsObject(const Data: string): TJSONObject;
var
  LJSONValue: TJSONValue;
begin
  Result := nil;
  LJSONValue := Parse(Data);
  if Assigned(LJSONValue) and (LJSONValue is TJSONObject) then
    Result := TJSONObject(LJSONValue)
  else if Assigned(LJSONValue) then
    LJSONValue.Free; // �Correcto! Evita fugas de memoria.
end;

class function TJSONUtils.ParseAsArray(const Data: string): TJSONArray;
var
  LJSONValue: TJSONValue;
begin
  Result := nil;
  LJSONValue := Parse(Data);
  if Assigned(LJSONValue) and (LJSONValue is TJSONArray) then
    Result := TJSONArray(LJSONValue)
  else if Assigned(LJSONValue) then
    LJSONValue.Free; // �Correcto! Evita fugas de memoria.
end;

end.
