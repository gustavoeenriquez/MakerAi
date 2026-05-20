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
// Nombre: Gustavo Enr�quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uJSONHelper;

interface

uses
  System.JSON, System.SysUtils, System.Generics.Collections;

type
  TJSONObjectHelper = class helper for TJSONObject
  public
    // Solo definimos los AddPair para versiones anteriores a Delphi 11 (35.0)
    // En Delphi 11, 12 y 13 ya existen de forma nativa.
    {$IF CompilerVersion < 35.0}
    function AddPair(const AKey: string; const AValue: Integer): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: Int64): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: Double): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: Single): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: Boolean): TJSONObject; overload;
    function AddPair(const AKey: string; const AValue: string): TJSONObject; overload;
    {$IFEND}

    // M�todos de navegaci�n segura (estos no existen nativamente en ninguna versi�n con esta firma)
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

  TJSONArrayHelper = class helper for TJSONArray
  public
    function GetItem(Index: Integer): TJSONValue;
    function GetItemAsObject(Index: Integer): TJSONObject;
    function GetItemAsArray(Index: Integer): TJSONArray;
  end;

  TJSONUtils = class
  public
    class function Parse(const Data: string): TJSONValue; static;
    class function ParseAsObject(const Data: string): TJSONObject; static;
    class function ParseAsArray(const Data: string): TJSONArray; static;
  end;

implementation

{ TJSONObjectHelper }

{$IF CompilerVersion < 35.0}
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
{$IFEND}

function TJSONObjectHelper.GetValueSafe(const Name: string): TJSONValue;
begin
  // En 10.3 GetValue retorna nil si no existe.
  // En versiones nuevas es igual, pero este helper asegura consistencia.
  Result := Self.GetValue(Name);
end;

function TJSONObjectHelper.GetValueAsString(const Name: string; const DefaultValue: string): string;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and not (LValue is TJSONNull) then
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
    Result := StrToIntDef(LValue.Value, DefaultValue)
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.GetValueAsInt64(const Name: string; const DefaultValue: Int64): Int64;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and (LValue is TJSONNumber) then
    Result := StrToInt64Def(LValue.Value, DefaultValue)
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
  if Assigned(LValue) then
  begin
    // En todas las versiones de Delphi, .Value devuelve 'true' o 'false' para tipos booleanos.
    // Usamos SameText para una comparaci�n segura que no dependa de tipos nativos.
    Result := SameText(LValue.Value, 'true');
  end
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
  Result := Assigned(LJSONValue) and not (LJSONValue is TJSONNull);
  if Result then
    Value := LJSONValue.Value
  else
    Value := '';
end;

{ TJSONArrayHelper }

function TJSONArrayHelper.GetItem(Index: Integer): TJSONValue;
begin
  {$IF CompilerVersion >= 35.0}
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
  Result := TJSONObject.ParseJSONValue(Data);
end;

class function TJSONUtils.ParseAsObject(const Data: string): TJSONObject;
var
  LJSONValue: TJSONValue;
begin
  Result := nil;
  LJSONValue := Parse(Data);
  if Assigned(LJSONValue) then
  begin
    if LJSONValue is TJSONObject then
      Result := TJSONObject(LJSONValue)
    else
      LJSONValue.Free;
  end;
end;

class function TJSONUtils.ParseAsArray(const Data: string): TJSONArray;
var
  LJSONValue: TJSONValue;
begin
  Result := nil;
  LJSONValue := Parse(Data);
  if Assigned(LJSONValue) then
  begin
    if LJSONValue is TJSONArray then
      Result := TJSONArray(LJSONValue)
    else
      LJSONValue.Free;
  end;
end;

end.
