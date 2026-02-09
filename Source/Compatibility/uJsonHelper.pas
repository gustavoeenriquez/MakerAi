// MIT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
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

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uJsonHelper;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, StrUtils, Generics.Collections, Types, Variants, Rtti, SyncObjs, Math,
  fpjson, jsonparser, TypInfo, fpjsonrtti,
  {$ELSE}
  System.JSON, System.SysUtils, System.Generics.Collections, System.TypInfo,
  {$ENDIF}
  uSysUtilsHelper, uBase64Helper, uThreadingHelper;

type
  {$IFDEF FPC}
  TJSONValue = TJSONData;
  TJSONObject = fpjson.TJSONObject;
  TJSONArray = fpjson.TJSONArray;
  TJSONBool = fpjson.TJSONBoolean;
  TJSONString = fpjson.TJSONString;
  TJSONNull = fpjson.TJSONNull;
  
  TJSONNumber = fpjson.TJSONNumber;
  
  TJSONPair = record
    JsonString: TJSONString;
    JsonValue: TJSONValue;
  end;

  { Enums para compatibilidad con Delphi System.JSON.Writers }
  TJsonFormatting = (None, Indented);

  { Shim de TJsonWriter para FPC }
  TJsonWriter = class
  public
    procedure WriteStartObject; virtual; abstract;
    procedure WriteEndObject; virtual; abstract;
    procedure WriteStartArray; virtual; abstract;
    procedure WriteEndArray; virtual; abstract;
    procedure WritePropertyName(const AName: string); virtual; abstract;
    procedure WriteValue(const AValue: string); overload; virtual; abstract;
    procedure WriteValue(const AValue: Integer); overload; virtual; abstract;
    procedure WriteValue(const AValue: Int64); overload; virtual; abstract;
    procedure WriteValue(const AValue: Double); overload; virtual; abstract;
    procedure WriteValue(const AValue: Boolean); overload; virtual; abstract;
    procedure WriteNull; virtual; abstract;
  end;

  { Shim de TJsonTextWriter para FPC }
  TJsonTextWriter = class(TJsonWriter)
  private
    FDestStream: TStream;
    FOwnsStream: Boolean;
    FFormatting: TJsonFormatting;
    FIndentLevel: Integer;
    FNeedComma: Boolean;
    FPendingProp: Boolean; 
    
    procedure WriteString(const S: string);
    procedure WriteIndent;
    procedure WriteCommaIfNeeded;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    property Formatting: TJsonFormatting read FFormatting write FFormatting;
    
    procedure WriteStartObject; override;
    procedure WriteEndObject; override;
    procedure WriteStartArray; override;
    procedure WriteEndArray; override;
    procedure WritePropertyName(const AName: string); override;
    procedure WriteValue(const AValue: string); overload; override;
    procedure WriteValue(const AValue: Integer); overload; override;
    procedure WriteValue(const AValue: Int64); overload; override;
    procedure WriteValue(const AValue: Double); overload; override;
    procedure WriteValue(const AValue: Boolean); overload; override;
    procedure WriteNull; override;
  end;

  
  { Enumerator para iterar TJSONObject con for..in }
  TJSONObjectEnumerator = class
  private
    FObject: TJSONObject;
    FIndex: Integer;
    FCurrent: TJSONPair;
  public
    constructor Create(AObject: TJSONObject);
    function MoveNext: Boolean;
    property Current: TJSONPair read FCurrent;
  end;

  { Enumerator para iterar TJSONArray con for..in }
  TJSONArrayEnumerator = class
  private
    FArray: TJSONArray;
    FIndex: Integer;
    FCurrent: TJSONValue;
  public
    constructor Create(AArray: TJSONArray);
    function MoveNext: Boolean;
    property Current: TJSONValue read FCurrent;
  end;
  {$ENDIF}



  // Wrapper for REST.JSON.TJSon (FPC)
  TJSon = class
  public
    class function ObjectToJsonObject(AObject: TObject): TJSONObject; static;
    class function JsonToObject<T: class, constructor>(AJsonObject: TJSONObject): T; static;
  end;

  {$IFDEF FPC}
  type
    TJSONFormatOption = (foSingleLine, foIndented);
    TJSONFormatOptions = set of TJSONFormatOption;
    
    // Cracker to access protected TJSONData.FormatJSON
    TJSONDataCracker = class(TJSONData);
  {$ELSE}
    // Delph already defines TJSONFormatOptions
  {$ENDIF}

  TJSONObjectHelper = class helper for TJSONObject
  public
    // Solo definimos los AddPair para versiones anteriores a Delphi 11 (35.0)
    // En Delphi 11, 12 y 13 ya existen de forma nativa.
    // Delphi < 11: AddPair tipados no existen. Delphi 11+ los tiene nativos.
    {$IFNDEF FPC}
      {$IF CompilerVersion < 35.0}
      function AddPair(const AKey: string; const AValue: Integer): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: Int64): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: Double): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: Single): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: Boolean): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: string): TJSONObject; overload;
      class function ParseJSONValue(const Data: string): TJSONValue; static;

      // Polyfill TryGetValue for Delphi < 10.3 (Rio)
      function TryGetValue(const Name: string; out Value: string): Boolean; overload;
      function TryGetValue(const Name: string; out Value: Integer): Boolean; overload;
      function TryGetValue(const Name: string; out Value: Int64): Boolean; overload;
      function TryGetValue(const Name: string; out Value: Double): Boolean; overload;
      function TryGetValue(const Name: string; out Value: Boolean): Boolean; overload;
      function TryGetValue(const Name: string; out Value: TJSONArray): Boolean; overload;
      function TryGetValue(const Name: string; out Value: TJSONObject): Boolean; overload;
      // NOTA: TryGetValue<T> genérico ELIMINADO. Usar overloads tipados arriba.
      {$IFEND}
    {$ELSE}
      // FPC Helper Methods to match Delphi syntax
      function AddPair(const AKey: string; const AValue: Integer): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: Int64): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: Double): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: Boolean): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: string): TJSONObject; overload;
      function AddPair(const AKey: string; const AValue: TJSONData): TJSONObject; overload;
      class function ParseJSONValue(const Data: string): TJSONValue; static;
    {$ENDIF}

    // Métodos de navegación segura (estos no existen nativamente en ninguna versión con esta firma)
    class function ParseAsArray(const Data: string): TJSONArray; static;
    function GetValueSafe(const Name: string): TJSONValue;
    function GetValueAsString(const Name: string; const DefaultValue: string = ''): string;
    function GetValueAsInteger(const Name: string; const DefaultValue: Integer = 0): Integer;
    function GetValueAsInt64(const Name: string; const DefaultValue: Int64 = 0): Int64;
    function GetValueAsDouble(const Name: string; const DefaultValue: Double = 0.0): Double;
    function GetValueAsBoolean(const Name: string; const DefaultValue: Boolean = False): Boolean;
    function GetValueAsObject(const Name: string): TJSONObject;
    function GetValueAsArray(const Name: string): TJSONArray;
    function TryGetValueAsString(const Name: string; out Value: string): Boolean;

    // Overloads de TryGetValue para compatibilidad cross-compiler
    // En Delphi moderno existen nativamente, en FPC y Delphi antiguo los implementamos
    {$IFDEF FPC}
    // GetValue: En Delphi existe nativo, en FPC usamos Find como equivalente
    function GetValue(const Name: string): TJSONValue; overload;
    function TryGetValue(const Name: string; out Value: string): Boolean; overload;
    function TryGetValue(const Name: string; out Value: Integer): Boolean; overload;
    function TryGetValue(const Name: string; out Value: Boolean): Boolean; overload;
    function TryGetValue(const Name: string; out Value: TJSONObject): Boolean; overload;
    function TryGetValue(const Name: string; out Value: TJSONArray): Boolean; overload;
    function TryGetValue(const Name: string; out Value: TJSONValue): Boolean; overload;
    function TryGetValue(const Name: string; out Value: Int64): Boolean; overload;
    function TryGetValue(const Name: string; out Value: Double): Boolean; overload;
    {$ENDIF}

    // RemovePair: FPC no tiene este método, Delphi moderno (>=35) sí lo tiene nativo
    {$IFDEF FPC}
    procedure RemovePair(const AName: string);
    {$ELSE}
      {$IF CompilerVersion < 35.0}
    procedure RemovePair(const AName: string);
      {$IFEND}
    {$ENDIF}
    
    // Método Find para compatibilidad con código FPC fpjson
    // En FPC ya existe nativamente en TJSONObject, en Delphi lo mapeamos a FindValue
    {$IFNDEF FPC}
    function Find(const AName: string): TJSONValue;
    {$ENDIF}
    
    // Compatibility with Delphi's GetValue(string): TJSONValue
    {$IFNDEF FPC}
    function GetValue(const Name: string): TJSONValue; overload;
    {$ENDIF}
    function FindValue(const Name: string): TJSONValue;

    {$IFDEF FPC}
    // NOTA: GetValue<T> genérico ELIMINADO intencionalmente.
    // FPC 3.3 sufre parser failures por colisión con fpjson.TJSONObject nativos.
    // Usar SIEMPRE getters explícitos: GetValueAsString, GetValueAsObject, etc.
    
    // Formatting (FPC usa FormatJSON, Delphi usa Format nativo)
    function Format(Options: TJSONFormatOptions = [foIndented]): string; overload;
    function Format(Indent: Integer): string; overload;
    {$ENDIF}
    
    // Polyfill de Format() para Delphi 10.2 Tokyo (CompilerVersion 32)
    // En Delphi 10.3 Rio+ (CompilerVersion >= 33) Format() existe nativo
    {$IFNDEF FPC}
      {$IF CompilerVersion < 33.0}
    function Format: string; overload;
    function Format(Indent: Integer): string; overload;
      {$IFEND}
    {$ENDIF}

    // Compatibility alias for Delphi's ToJSON
    function ToJSONString: string;
    
    {$IFDEF FPC}
    function GetEnumerator: TJSONObjectEnumerator;
    function GetPair(Index: Integer): TJSONPair;
    property Pairs[Index: Integer]: TJSONPair read GetPair;

    function AddElement(const AKey: string; const AValue: string): TJSONObject; overload; inline;
    function AddElement(const AKey: string; const AValue: Integer): TJSONObject; overload; inline;
    function AddElement(const AKey: string; const AValue: Boolean): TJSONObject; overload; inline;
    {$ENDIF}
    
  end;

  TJSONArrayHelper = class helper for TJSONArray
  public
    function GetItem(Index: Integer): TJSONValue;
    function GetItemAsObject(Index: Integer): TJSONObject;
    function GetItemAsArray(Index: Integer): TJSONArray;
    function Count: Integer;
    {$IFDEF FPC}
    procedure Add(Value: TJSONValue); overload;
    procedure Add(Value: string); overload;
    procedure Add(Value: Integer); overload;
    procedure Add(Value: Double); overload;
    procedure Add(Value: Boolean); overload;
    {$ENDIF}
    
    // Typed access methods (abstrae fpjson/Delphi differences)
    function GetFloatValue(Index: Integer): Double;
    function GetIntValue(Index: Integer): Integer;
    function GetStringValue(Index: Integer): string;
    
    {$IFDEF FPC}
    // Formatting (FPC usa FormatJSON, Delphi usa Format nativo)
    function Format(Options: TJSONFormatOptions = [foIndented]): string;
    function GetEnumerator: TJSONArrayEnumerator;

    function AddElement(Value: TJSONValue): TJSONArray; overload; inline;
    function AddElement(Value: string): TJSONArray; overload; inline;
    function AddElement(Value: Integer): TJSONArray; overload; inline;
    function AddElement(Value: Double): TJSONArray; overload; inline;
    function AddElement(Value: Boolean): TJSONArray; overload; inline;
    {$ENDIF}

    // Polyfill de Format() para Delphi 10.2 Tokyo (CompilerVersion 32)
    {$IFNDEF FPC}
      {$IF CompilerVersion < 33.0}
    function Format: string; overload;
      {$IFEND}
    {$ENDIF}
  end;

  {$IFDEF FPC}
  TJSONValueHelper = class helper for TJSONValue
  public
    function ToJSON: string;
    // NOTA: GetValue<T> y TryGetVal<T> genéricos ELIMINADOS.
    // Usar AsDoubleSafe, ToJSON, o castear con tipo explícito.
    function AsDoubleSafe(const ADefault: Double = 0.0): Double;
    {$IFNDEF FPC}
    function AsString: string;
    {$ENDIF}
  end;

  TJSONNumberHelper = class helper for TJSONNumber
  public
    function AsDouble: Double;
    function AsInt: Integer;   // FPC: Paridad con Delphi TJSONNumber.AsInt
    function AsInt64: Int64;
  end;
  {$ENDIF}

  {$IFDEF FPC}
  { Función para conversión de fecha a ISO8601 }
  function DateToISO8601(ADate: TDateTime; AInputIsUTC: Boolean = True): string;
  {$ENDIF}

  { Helper universal GetJSON para compatibilidad FPC/Delphi }
  function GetJSON(const aJSON: string): TJSONValue;

  { Helpers para creación unificada de JSON Number (FPC vs Delphi) }
  function CreateJSONNumber(Value: Int64): TJSONNumber; overload;
  function CreateJSONNumber(Value: Double): TJSONNumber; overload;
  function CreateJSONBool(Value: Boolean): TJSONBool;

  { FormatJSON unificado - abstrae JObj.AsJSON (FPC) vs JObj.ToString (Delphi) }
  function FormatJSON(const JObj: TJSONObject): string; overload;
  function FormatJSON(const JArr: TJSONArray): string; overload;

  { COMPAT: Abstrae TJSONValue.Value (Delphi) vs TJSONValue.AsString (FPC) }
  function GetJSONStringValue(const V: TJSONValue): string;

implementation

{$IFNDEF FPC}
uses
  REST.Json;
{$ENDIF}

function CreateJSONNumber(Value: Int64): TJSONNumber;
begin
  {$IFDEF FPC}
  Result := TJSONInt64Number.Create(Value);
  {$ELSE}
  Result := TJSONNumber.Create(Value);
  {$ENDIF}
end;

function CreateJSONNumber(Value: Double): TJSONNumber;
begin
  {$IFDEF FPC}
  Result := TJSONFloatNumber.Create(Value);
  {$ELSE}
  Result := TJSONNumber.Create(Value);
  {$ENDIF}
end;

function CreateJSONBool(Value: Boolean): TJSONBool;
begin
  {$IFDEF FPC}
  Result := TJSONBoolean.Create(Value);
  {$ELSE}
  Result := TJSONBool.Create(Value);
  {$ENDIF}
end;

function GetJSON(const aJSON: string): TJSONValue;
begin
  {$IFDEF FPC}
  Result := fpjson.GetJSON(aJSON);
  {$ELSE}
  Result := TJSONObject.ParseJSONValue(aJSON);
  {$ENDIF}
end;

function FormatJSON(const JObj: TJSONObject): string;
begin
  if not Assigned(JObj) then
  begin
    Result := '';
    Exit;
  end;
  {$IFDEF FPC}
  Result := JObj.AsJSON;
  {$ELSE}
  Result := JObj.ToString;
  {$ENDIF}
end;

function FormatJSON(const JArr: TJSONArray): string;
begin
  if not Assigned(JArr) then
  begin
    Result := '';
    Exit;
  end;
  {$IFDEF FPC}
  Result := JArr.AsJSON;
  {$ELSE}
  Result := JArr.ToString;
  {$ENDIF}
end;

function GetJSONStringValue(const V: TJSONValue): string;
begin
  if V = nil then
  begin
    Result := '';
    Exit;
  end;
  {$IFDEF FPC}
  Result := V.AsString;
  {$ELSE}
  Result := V.Value;
  {$ENDIF}
end;



{ TJSon }

{$IFDEF FPC}
// FPC: Usa fpjsonrtti (TJSONStreamer/TJSONDeStreamer)
class function TJSon.ObjectToJsonObject(AObject: TObject): TJSONObject;
var
  S: TJSONStreamer;
begin
  S := TJSONStreamer.Create(nil);
  try
    Result := S.ObjectToJSON(AObject) as TJSONObject;
  finally
    S.Free;
  end;
end;

class function TJSon.JsonToObject<T>(AJsonObject: TJSONObject): T;
var
  D: TJSONDeStreamer;
  Ctx: TRttiContext;
  Typ: TRttiType;
  Meth: TRttiMethod;
begin
  Result := nil;
  D := TJSONDeStreamer.Create(nil);
  try
    // FPC no soporta T.Create para genéricos con constraint class, constructor
    // Usamos RTTI para encontrar e invocar el constructor
    Ctx := TRttiContext.Create;
    try
      Typ := Ctx.GetType(TypeInfo(T));
      if Typ = nil then
        raise Exception.CreateFmt('RTTI not found for type %s', [T.ClassName]);
      for Meth in Typ.GetMethods do
      begin
        if (Meth.IsConstructor) and (Length(Meth.GetParameters) = 0) then
        begin
          Result := Meth.Invoke(T, []).AsObject as T;
          Break;
        end;
      end;
      if Result = nil then
        raise Exception.CreateFmt('No parameterless constructor found for %s', [T.ClassName]);
    finally
      Ctx.Free;
    end;
    try
      D.JSONToObject(AJsonObject, Result);
    except
      on E: Exception do
      begin
        Result.Free;
        raise Exception.CreateFmt('Error deserializing JSON to object of type %s: %s', [T.ClassName, E.Message]);
      end;
    end;
  finally
    D.Free;
  end;
end;

{$ELSE}
// Delphi: Usa REST.Json para serialización/deserialización
class function TJSon.ObjectToJsonObject(AObject: TObject): TJSONObject;
begin
  Result := REST.Json.TJson.ObjectToJsonObject(AObject);
end;

class function TJSon.JsonToObject<T>(AJsonObject: TJSONObject): T;
begin
  Result := REST.Json.TJson.JsonToObject<T>(AJsonObject);
end;
{$ENDIF}

{ TJSONObjectHelper }

// Delphi < 11: AddPair tipados no existen, los agregamos como helper
{$IFNDEF FPC}
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

{$IF CompilerVersion < 35.0}
class function TJSONObjectHelper.ParseJSONValue(const Data: string): TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(Data);
end;
{$IFEND}

{$ENDIF}

{$IFDEF FPC}
function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Integer): TJSONObject;
begin
  Self.Add(AKey, AValue);
  Result := Self;
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Int64): TJSONObject;
begin
  Self.Add(AKey, AValue);
  Result := Self;
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Double): TJSONObject;
begin
  Self.Add(AKey, AValue);
  Result := Self;
end;

function TJSONObjectHelper.AddElement(const AKey: string; const AValue: string): TJSONObject;
begin
  Result := AddPair(AKey, AValue);
end;

function TJSONObjectHelper.AddElement(const AKey: string; const AValue: Integer): TJSONObject;
begin
  Result := AddPair(AKey, AValue);
end;

function TJSONObjectHelper.AddElement(const AKey: string; const AValue: Boolean): TJSONObject;
begin
  Result := AddPair(AKey, AValue);
end;


function TJSONObjectHelper.AddPair(const AKey: string; const AValue: Boolean): TJSONObject;
begin
  Self.Add(AKey, AValue);
  Result := Self;
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: string): TJSONObject;
begin
  Self.Add(AKey, AValue);
  Result := Self;
end;

function TJSONObjectHelper.AddPair(const AKey: string; const AValue: TJSONData): TJSONObject;
begin
  Self.Add(AKey, AValue);
  Result := Self;
end;

class function TJSONObjectHelper.ParseJSONValue(const Data: string): TJSONValue;
begin
  Result := GetJSON(Data);
end;
{$ENDIF}

// Delphi: Implementación de Find para compatibilidad con código FPC fpjson
{$IFNDEF FPC}
function TJSONObjectHelper.Find(const AName: string): TJSONValue;
begin
  // En Delphi, FindValue retorna nil si no existe, igual que Find en FPC
  Result := Self.FindValue(AName);
end;
{$ENDIF}

function TJSONObjectHelper.GetValueSafe(const Name: string): TJSONValue;
begin
  // En 10.3 GetValue retorna nil si no existe.
  // En versiones nuevas es igual, pero este helper asegura consistencia.
  {$IFDEF FPC}
  Result := Self.Find(Name);
  {$ELSE}
  Result := Self.GetValue(Name);
  {$ENDIF}
end;

{$IFDEF FPC}
// GetValue: Compatibilidad con sintaxis Delphi - FPC usa Find nativamente
function TJSONObjectHelper.GetValue(const Name: string): TJSONValue;
begin
  Result := Self.Find(Name);
end;
{$ENDIF}

{$IFDEF FPC}
function GetVal(V: TJSONValue): string; inline;
begin
  if V = nil then Exit('');
  Result := V.AsString;
end;
{$ELSE}
function GetVal(V: TJSONValue): string; inline;
begin
  if V = nil then Exit('');
  Result := V.Value;
end;
{$ENDIF}

function TJSONObjectHelper.FindValue(const Name: string): TJSONValue;
begin
  Result := GetValueSafe(Name);
end;

function TJSONObjectHelper.GetValueAsString(const Name: string; const DefaultValue: string): string;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  if Assigned(LValue) and not (LValue is TJSONNull) then
    Result := GetVal(LValue)
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.GetValueAsInteger(const Name: string; const DefaultValue: Integer): Integer;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  {$IFDEF FPC}
  if Assigned(LValue) and (LValue is fpjson.TJSONNumber) then
  {$ELSE}
  if Assigned(LValue) and (LValue is TJSONNumber) then
  {$ENDIF}
    Result := StrToIntDef(GetVal(LValue), DefaultValue)
  else
    Result := DefaultValue;
end;

class function TJSONObjectHelper.ParseAsArray(const Data: string): TJSONArray;
var
  LVal: TJSONValue;
begin
  if Trim(Data) = '' then
    Exit(nil);

  try
    LVal := TJSONObject.ParseJSONValue(Data);
  except
    LVal := nil;
  end;

  if (LVal <> nil) and (LVal is TJSONArray) then
    Result := TJSONArray(LVal)
  else
  begin
    if LVal <> nil then
      LVal.Free;
    Result := nil;
  end;
end;

function TJSONObjectHelper.GetValueAsInt64(const Name: string; const DefaultValue: Int64): Int64;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  {$IFDEF FPC}
  if Assigned(LValue) and (LValue is fpjson.TJSONNumber) then
  {$ELSE}
  if Assigned(LValue) and (LValue is TJSONNumber) then
  {$ENDIF}
    Result := StrToInt64Def(GetVal(LValue), DefaultValue)
  else
    Result := DefaultValue;
end;

function TJSONObjectHelper.GetValueAsDouble(const Name: string; const DefaultValue: Double): Double;
var
  LValue: TJSONValue;
begin
  LValue := GetValueSafe(Name);
  {$IFDEF FPC}
  if Assigned(LValue) and (LValue is fpjson.TJSONNumber) then
    Result := fpjson.TJSONNumber(LValue).AsFloat
  {$ELSE}
  if Assigned(LValue) and (LValue is TJSONNumber) then
    Result := TJSONNumber(LValue).AsDouble
  {$ENDIF}
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
    // Usamos SameText para una comparación segura que no dependa de tipos nativos.
    Result := SameText(GetVal(LValue), 'true');
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
    Value := GetVal(LJSONValue)
  else
    Value := '';
end;

// RemovePair solo para FPC o Delphi < 35 (versiones modernas lo tienen nativo)
{$IFDEF FPC}
procedure TJSONObjectHelper.RemovePair(const AName: string);
var
  Idx: Integer;
begin
  Idx := Self.IndexOfName(AName);
  if Idx <> -1 then
    Self.Delete(Idx);
end;
{$ELSE}
  {$IF CompilerVersion < 35.0}
procedure TJSONObjectHelper.RemovePair(const AName: string);
var
  I: Integer;
begin
  for I := Self.Count - 1 downto 0 do
  begin
    if SameText(Self.Pairs[I].JsonString.Value, AName) then
    begin
      Self.Pairs[I].Free;
      Break;
    end;
  end;
end;
  {$IFEND}
{$ENDIF}

{$IFDEF FPC}
function TJSONObjectHelper.GetEnumerator: TJSONObjectEnumerator;
begin
  Result := TJSONObjectEnumerator.Create(Self);
end;

function TJSONObjectHelper.GetPair(Index: Integer): TJSONPair;
begin
  if (Index >= 0) and (Index < Self.Count) then
  begin
    Result.JsonString := TJSONString.Create(Self.Names[Index]);
    Result.JsonValue := Self.Items[Index];
  end
  else
    raise EArgumentOutOfRangeException.Create('JSON Pair index out of bounds');
end;
{$ENDIF}

function TJSONObjectHelper.ToJSONString: string;
begin
  {$IFDEF FPC}
  Result := Self.AsJSON;
  {$ELSE}
  Result := Self.ToJSON;
  {$ENDIF}
end;

{$IFDEF FPC}
function TJSONObjectHelper.Format(Indent: Integer): string;
begin
  Result := Self.FormatJSON([], Indent);
end;
{$ENDIF}

// Polyfill Format() para Delphi 10.2 (CompilerVersion < 33)
{$IFNDEF FPC}
  {$IF CompilerVersion < 33.0}
function TJSONObjectHelper.Format: string;
begin
  Result := Self.ToJSON;
end;

function TJSONObjectHelper.Format(Indent: Integer): string;
begin
  // D10.2 no tiene Format nativo, devolvemos ToJSON sin indentación
  Result := Self.ToJSON;
end;

function TJSONArrayHelper.Format: string;
begin
  Result := Self.ToJSON;
end;
  {$IFEND}
{$ENDIF}

{ TJSONArrayHelper }

function TJSONArrayHelper.GetItem(Index: Integer): TJSONValue;
begin
  {$IFDEF FPC}
    Result := Self.Items[Index];
  {$ELSE}
    {$IF CompilerVersion >= 35.0}
      Result := Self[Index];
    {$ELSE}
      Result := Self.Items[Index];
    {$IFEND}
  {$ENDIF}
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

function TJSONArrayHelper.Count: Integer;
begin
  {$IFDEF FPC}
  // FPC: TJSONArray hereda Count de TJSONData
  Result := TJSONData(Self).Count;
  {$ELSE}
  // Delphi: TJSONArray.Count es nativo
  Result := TJSONArray(Self).Count;
  {$ENDIF}
end;

{$IFDEF FPC}
procedure TJSONArrayHelper.Add(Value: TJSONValue);
begin
  // FPC TJSONArray.Add hereda de TJSONArray nativo, que devuelve Integer
  // Usamos inherited para evitar recursión al helper
  inherited Add(Value);
end;

procedure TJSONArrayHelper.Add(Value: string);
begin
  inherited Add(Value);
end;

procedure TJSONArrayHelper.Add(Value: Integer);
begin
  inherited Add(Value);
end;

procedure TJSONArrayHelper.Add(Value: Double);
begin
  inherited Add(Value);
end;

procedure TJSONArrayHelper.Add(Value: Boolean);
begin
  inherited Add(Value);
end;
{$ENDIF}

{$IFDEF FPC}
function TJSONArrayHelper.AddElement(Value: TJSONValue): TJSONArray;
begin
  Self.Add(Value);
  Result := Self;
end;

function TJSONArrayHelper.AddElement(Value: string): TJSONArray;
begin
  Self.Add(Value);
  Result := Self;
end;

function TJSONArrayHelper.AddElement(Value: Integer): TJSONArray;
begin
  Self.Add(Value);
  Result := Self;
end;

function TJSONArrayHelper.AddElement(Value: Double): TJSONArray;
begin
  Self.Add(Value);
  Result := Self;
end;

function TJSONArrayHelper.AddElement(Value: Boolean): TJSONArray;
begin
  Self.Add(Value);
  Result := Self;
end;
{$ENDIF}

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

function TJSONArrayHelper.GetFloatValue(Index: Integer): Double;
var
  LItem: TJSONValue;
begin
  Result := 0.0;
  if (Index >= 0) and (Index < Count) then
  begin
    LItem := GetItem(Index);
    if Assigned(LItem) then
    begin
      {$IFDEF FPC}
      if LItem is fpjson.TJSONNumber then
        Result := fpjson.TJSONNumber(LItem).AsFloat
      else
        Result := StrToFloatDef(GetVal(LItem), 0.0);
      {$ELSE}
      if LItem is TJSONNumber then
        Result := TJSONNumber(LItem).AsDouble
      else
        Result := StrToFloatDef(GetVal(LItem), 0.0);
      {$ENDIF}
    end;
  end;
end;

function TJSONArrayHelper.GetIntValue(Index: Integer): Integer;
var
  LItem: TJSONValue;
begin
  Result := 0;
  if (Index >= 0) and (Index < Count) then
  begin
    LItem := GetItem(Index);
    if Assigned(LItem) then
    begin
      {$IFDEF FPC}
      if LItem is fpjson.TJSONNumber then
        Result := fpjson.TJSONNumber(LItem).AsInteger
      else
        Result := StrToIntDef(GetVal(LItem), 0);
      {$ELSE}
      if LItem is TJSONNumber then
        Result := TJSONNumber(LItem).AsInt
      else
        Result := StrToIntDef(GetVal(LItem), 0);
      {$ENDIF}
    end;
  end;
end;

function TJSONArrayHelper.GetStringValue(Index: Integer): string;
var
  LItem: TJSONValue;
begin
  Result := '';
  if (Index >= 0) and (Index < Count) then
  begin
    LItem := GetItem(Index);
    if Assigned(LItem) and not (LItem is TJSONNull) then
      Result := GetVal(LItem);
  end;
end;


{$IFDEF FPC}
{ TJSONValueHelper }
function TJSONValueHelper.ToJSON: string;
begin
  Result := Self.AsJSON;
end;

// GetValue<T> y TryGetVal<T> genéricos ELIMINADOS de TJSONValueHelper.
// Usar AsDoubleSafe, AsFloat, AsInteger, AsString, AsBoolean directamente.

function TJSONValueHelper.AsDoubleSafe(const ADefault: Double = 0.0): Double;
begin
  if Self = nil then Exit(ADefault);
  try
    {$IFDEF FPC}
    Result := Self.AsFloat;
    {$ELSE}
    // In Delphi TJSONValue doesn't have AsFloat usually, but if this code runs in Delphi 
    // we should implementation it or use generic GetValue if available or just return 0 for now as focus is FPC
    // Assuming uJsonHelper for Delphi handles this via other helpers or not needed yet.
    // However, to keep it compilable if directives match:
    Result := ADefault; // Placeholder for Delphi if needed, but this block is mainly FPC active due to previous edits context
    // Actually, check IFDEF FPC context of this implementation block.
    // The previous view showed TJSONValueHelper is around line 306 inside {$IFDEF FPC}.
    // So this implementation is FPC only.
    {$ENDIF}
  except
    Result := ADefault;
  end;
end;

{$IFNDEF FPC}
function TJSONValueHelper.AsString: string;
begin
  Result := Self.Value;
end;
{$ENDIF}

{ TJSONNumberHelper }
function TJSONNumberHelper.AsDouble: Double;
begin
  Result := Self.AsFloat;
end;

// FPC: Emula TJSONNumber.AsInt de Delphi para mantener paridad de código
function TJSONNumberHelper.AsInt: Integer;
begin
  Result := Self.AsInteger;
end;

function TJSONNumberHelper.AsInt64: Int64;
begin
  Result := StrToInt64Def(GetVal(Self), 0);
end;


{ DateToISO8601 - Conversión fecha a formato ISO8601 }
function DateToISO8601(ADate: TDateTime; AInputIsUTC: Boolean = True): string;
begin
  { Formato: YYYY-MM-DDTHH:MM:SS }
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', ADate);
  if AInputIsUTC then
    Result := Result + 'Z';
end;

{ TJSONObjectEnumerator - Implementación }
constructor TJSONObjectEnumerator.Create(AObject: TJSONObject);
begin
  inherited Create;
  FObject := AObject;
  FIndex := -1;
end;

function TJSONObjectEnumerator.MoveNext: Boolean;
var
  Key: string;
begin
  Inc(FIndex);
  Result := FIndex < FObject.Count;
  if Result then
  begin
    Key := FObject.Names[FIndex];
    FCurrent.JsonString := TJSONString.Create(Key);
    FCurrent.JsonValue := FObject.Items[FIndex];
  end;
end;

{ TJSONArrayEnumerator - Implementación }
constructor TJSONArrayEnumerator.Create(AArray: TJSONArray);
begin
  inherited Create;
  FArray := AArray;
  FIndex := -1;
end;

function TJSONArrayEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FArray.Count;
  if Result then
    FCurrent := FArray.Items[FIndex];
end;

{ TJSONArrayHelper - Extension para permitir for Item in JSONArray }
function TJSONArrayHelper.GetEnumerator: TJSONArrayEnumerator;
begin
  Result := TJSONArrayEnumerator.Create(Self);
end;

{ TJsonTextWriter Implementation }

constructor TJsonTextWriter.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FDestStream := AStream;
  FOwnsStream := AOwnsStream;
  FFormatting := TJsonFormatting.None;
  FIndentLevel := 0;
  FNeedComma := False;
  FPendingProp := False;
end;

destructor TJsonTextWriter.Destroy;
begin
  if FOwnsStream then
    FDestStream.Free;
  inherited;
end;

procedure TJsonTextWriter.WriteString(const S: string);
var
  Bytes: TBytes;
begin
  if S = '' then Exit;
  Bytes := TEncoding.UTF8.GetBytes(S);
  FDestStream.WriteBuffer(Bytes[0], Length(Bytes));
end;

procedure TJsonTextWriter.WriteIndent;
var
  I: Integer;
begin
  if FFormatting = TJsonFormatting.Indented then
  begin
    WriteString(sLineBreak);
    for I := 1 to FIndentLevel do
      WriteString('  ');
  end;
end;

procedure TJsonTextWriter.WriteCommaIfNeeded;
begin
  if FNeedComma then
    WriteString(',');
    
  FNeedComma := False;
end;

procedure TJsonTextWriter.WriteStartObject;
begin
  WriteCommaIfNeeded;
  if not FPendingProp then WriteIndent;
  WriteString('{');
  Inc(FIndentLevel);
  FNeedComma := False;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WriteEndObject;
begin
  Dec(FIndentLevel);
  WriteIndent;
  WriteString('}');
  FNeedComma := True;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WriteStartArray;
begin
  WriteCommaIfNeeded;
  if not FPendingProp then WriteIndent;
  WriteString('[');
  Inc(FIndentLevel);
  FNeedComma := False;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WriteEndArray;
begin
  Dec(FIndentLevel);
  WriteIndent;
  WriteString(']');
  FNeedComma := True;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WritePropertyName(const AName: string);
begin
  WriteCommaIfNeeded;
  WriteIndent;
  WriteString('"' + AName + '":');
  if FFormatting = TJsonFormatting.Indented then
    WriteString(' ');
  FNeedComma := False;
  FPendingProp := True;
end;

procedure TJsonTextWriter.WriteValue(const AValue: string);
begin
  if not FPendingProp then WriteCommaIfNeeded; 
  WriteString('"' + StringReplace(AValue, '"', '\"', [rfReplaceAll]) + '"');
  FNeedComma := True;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WriteValue(const AValue: Integer);
begin
  if not FPendingProp then WriteCommaIfNeeded;
  WriteString(IntToStr(AValue));
  FNeedComma := True;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WriteValue(const AValue: Int64);
begin
  if not FPendingProp then WriteCommaIfNeeded;
  WriteString(IntToStr(AValue));
  FNeedComma := True;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WriteValue(const AValue: Double);
var
  FS: TFormatSettings;
begin
  if not FPendingProp then WriteCommaIfNeeded;
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  WriteString(FloatToStr(AValue, FS));
  FNeedComma := True;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WriteValue(const AValue: Boolean);
begin
  if not FPendingProp then WriteCommaIfNeeded;
  if AValue then WriteString('true') else WriteString('false');
  FNeedComma := True;
  FPendingProp := False;
end;

procedure TJsonTextWriter.WriteNull;
begin
  if not FPendingProp then WriteCommaIfNeeded;
  WriteString('null');
  FNeedComma := True;
  FPendingProp := False;
end;
{$ENDIF}

{$IFDEF FPC}
// FPC: Implementación de TryGetValue overloads (en Delphi moderno existen nativamente)
function TJSONObjectHelper.TryGetValue(const Name: string; out Value: string): Boolean;
begin
  Value := GetValueAsString(Name, '');
  Result := (Find(Name) <> nil);
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: Integer): Boolean;
begin
  Value := GetValueAsInteger(Name, 0);
  Result := (Find(Name) <> nil);
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: Boolean): Boolean;
begin
  Value := GetValueAsBoolean(Name, False);
  Result := (Find(Name) <> nil);
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: TJSONObject): Boolean;
begin
  Value := GetValueAsObject(Name);
  Result := (Value <> nil);
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: TJSONArray): Boolean;
begin
  Value := GetValueAsArray(Name);
  Result := (Value <> nil);
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: TJSONValue): Boolean;
begin
  Value := Find(Name);
  Result := (Value <> nil);
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: Int64): Boolean;
begin
  Value := GetValueAsInt64(Name, 0);
  Result := (Find(Name) <> nil);
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: Double): Boolean;
begin
  Value := GetValueAsDouble(Name, 0.0);
  Result := (Find(Name) <> nil);
end;
{$ENDIF}

// GetValue simple: Disponible para todos los compiladores
{$IFNDEF FPC}
function TJSONObjectHelper.GetValue(const Name: string): TJSONValue;
begin
  Result := Find(Name);
end;
{$ENDIF}

{$IFDEF FPC}
// GetValue<T>, GetValue<T>(default), TryGetValue<T> genéricos ELIMINADOS.
// FPC 3.3 sufre parser failures por colisión con fpjson nativos.
// Usar SIEMPRE: GetValueAsString, GetValueAsInteger, GetValueAsDouble,
//   GetValueAsBoolean, GetValueAsObject, GetValueAsArray, TryGetValue(typed).

function TJSONObjectHelper.Format(Options: TJSONFormatOptions): string;
begin
  if foIndented in Options then
    Result := TJSONDataCracker(Pointer(Self)).FormatJSON([], 2)
  else
    Result := Self.AsJSON;
end;

// --- TJSONArrayHelper ---

function TJSONArrayHelper.Format(Options: TJSONFormatOptions): string;
begin
  if foIndented in Options then
    Result := TJSONDataCracker(Pointer(Self)).FormatJSON([], 2)
  else
    Result := Self.AsJSON;
end;
{$ENDIF}

{$IFNDEF FPC}
  {$IF CompilerVersion < 33.0}
function TJSONObjectHelper.TryGetValue(const Name: string; out Value: string): Boolean;
var
  LVal: TJSONValue;
begin
  LVal := FindValue(Name);
  Result := (LVal <> nil) and (not (LVal is TJSONNull));
  if Result then
    Value := LVal.Value
  else
    Value := '';
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: Integer): Boolean;
var
  LVal: TJSONValue;
begin
  LVal := FindValue(Name);
  Result := (LVal <> nil) and (not (LVal is TJSONNull));
  if Result then
  begin
    if LVal is TJSONNumber then
      Value := StrToIntDef(LVal.Value, 0)
    else
      Result := TryStrToInt(LVal.Value, Value);
  end
  else
    Value := 0;
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: Int64): Boolean;
var
  LVal: TJSONValue;
begin
  LVal := FindValue(Name);
  Result := (LVal <> nil) and (not (LVal is TJSONNull));
  if Result then
  begin
    if LVal is TJSONNumber then
      Value := StrToInt64Def(LVal.Value, 0)
    else
      Result := TryStrToInt64(LVal.Value, Value);
  end
  else
    Value := 0;
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: Double): Boolean;
var
  LVal: TJSONValue;
begin
  LVal := FindValue(Name);
  Result := (LVal <> nil) and (not (LVal is TJSONNull));
  if Result then
  begin
    if LVal is TJSONNumber then
      Value := StrToFloatDef(LVal.Value, 0.0)
    else
      Result := TryStrToFloat(LVal.Value, Value);
  end
  else
    Value := 0.0;
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: Boolean): Boolean;
var
  LVal: TJSONValue;
begin
  LVal := FindValue(Name);
  Result := (LVal <> nil) and (not (LVal is TJSONNull));
  if Result then
  begin
    if LVal is TJSONTrue then
      Value := True
    else if LVal is TJSONFalse then
      Value := False
    else
      Result := TryStrToBool(LVal.Value, Value);
  end
  else
    Value := False;
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: TJSONArray): Boolean;
var
  LVal: TJSONValue;
begin
  LVal := FindValue(Name);
  Result := (LVal <> nil) and (LVal is TJSONArray);
  if Result then
    Value := TJSONArray(LVal)
  else
    Value := nil;
end;

function TJSONObjectHelper.TryGetValue(const Name: string; out Value: TJSONObject): Boolean;
var
  LVal: TJSONValue;
begin
  LVal := FindValue(Name);
  Result := (LVal <> nil) and (LVal is TJSONObject);
  if Result then
    Value := TJSONObject(LVal)
  else
    Value := nil;
end;

  {$IFEND}
{$ENDIF}

end.
