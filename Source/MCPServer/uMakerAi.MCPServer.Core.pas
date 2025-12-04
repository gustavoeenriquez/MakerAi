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

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


unit uMakerAi.MCPServer.Core;

interface

uses
  System.SysUtils, System.Classes, System.JSON, Rest.JSON, System.Rtti, System.StrUtils,
  System.ConvUtils, System.IOUtils, System.NetEncoding, System.Net.Mime, IdGlobalProtocols,
  System.Generics.Collections, System.TypInfo;

type

  TAiAuthContext = record
    IsAuthenticated: Boolean;
    UserID: string;
    UserName: string;
    Roles: TArray<string>;
  end;

  TAiMCPResponseBuilder = class
  private
    FContentArray: TJSONArray;
    constructor Create;
  public
    class function New: TAiMCPResponseBuilder;
    destructor Destroy; override;

    // Añade un bloque de texto simple a la respuesta
    function AddText(const AText: string): TAiMCPResponseBuilder;

    // Añade un archivo desde una ruta en disco
    function AddFile(const AFilePath: string; AFileName: string = ''): TAiMCPResponseBuilder;

    // Añade un archivo desde un TStream
    function AddFileFromStream(AStream: TStream; const AFileName: string; const AMimeType: string): TAiMCPResponseBuilder;

    // Construye el objeto JSON final que se devolverá como 'result' en la llamada al tool
    function Build: TJSONObject;
  end;

  TAiMCPSchemaUtils = class
  public
    class function GenerateSchema(AClass: TClass): TJSONObject;
  end;

  TAiMCPSerializerUtils = class
  public
    class function Deserialize<T: class, constructor>(JSON: TJSONObject): T;
    class procedure DeserializeObject(Instance: TObject; JSON: TJSONObject);
    // ----------------------
  end;

  AiMCPOptionalAttribute = class(TCustomAttribute)
  end;

  AiMCPSchemaDescriptionAttribute = class(TCustomAttribute)
  private
    FDescription: string;
  public
    constructor Create(const ADescription: string);
    property Description: string read FDescription;
  end;

  AiMCPSchemaEnumAttribute = class(TCustomAttribute)
  private
    FValues: TArray<string>;
  public
    constructor Create(const AValues: array of string);
    property Values: TArray<string> read FValues;
  end;

  //
  // --- SECTION: MakerAi Suite - Tool Base Types ---
  //
type
  IAiMCPTool = interface
    ['{B1A4D0F8-9A7B-4C6C-8D1F-4B9E3A5F7C1E}']
    function GetName: string;
    function GetDescription: string;
    function GetInputSchema: TJSONObject;
    // function Execute(const Arguments: TJSONObject): string;
    function Execute(const Arguments: TJSONObject; const AuthContext: TAiAuthContext): TJSONObject;

    property Name: string read GetName;
    property Description: string read GetDescription;
    property InputSchema: TJSONObject read GetInputSchema;
  end;

  TAiMCPToolBase<T: class, constructor> = class(TInterfacedObject, IAiMCPTool)
  protected
    FName: string;
    FDescription: string;
    // function ExecuteWithParams(const Params: T): string; virtual; abstract;
    function ExecuteWithParams(const Params: T; const AuthContext: TAiAuthContext): TJSONObject; virtual; abstract;
  public
    constructor Create; virtual;
    function GetName: string;
    function GetDescription: string;
    function GetInputSchema: TJSONObject;
    // function Execute(const Arguments: TJSONObject): string;
    function Execute(const Arguments: TJSONObject; const AuthContext: TAiAuthContext): TJSONObject;
  end;

  //
  // --- SECTION: MakerAi Suite - Resource Base Types ---
  //
type
  IAiMCPResource = interface
    ['{E6A3B2C9-5D8F-4A1E-B9C2-7D8F9A0B1C3D}']
    function GetURI: string;
    function GetName: string;
    function GetDescription: string;
    function GetMimeType: string;
    function Read: string;

    property URI: string read GetURI;
    property Name: string read GetName;
    property Description: string read GetDescription;
    property MimeType: string read GetMimeType;
  end;

  TAiMCPResourceBase<T: class, constructor> = class(TInterfacedObject, IAiMCPResource)
  protected
    FURI: string;
    FName: string;
    FDescription: string;
    FMimeType: string;
    function GetResourceData: T; virtual; abstract;
  public
    constructor Create; virtual;
    function GetURI: string;
    function GetName: string;
    function GetDescription: string;
    function GetMimeType: string;
    function Read: string;
  end;

  //
  // --- SECTION: MakerAi Suite - Main Server Class ---
  //
type
  TAiMCPToolFactory = reference to function: IAiMCPTool;
  TAiMCPResourceFactory = reference to function: IAiMCPResource;

  TAiMCPLogicServer = class(TObject)
  private
    // --- Settings Fields ---
    FPort: Integer;
    FHost: string;
    FServerName: string;
    FServerVersion: string;
    FProtocolVersion: string;
    FCorsEnabled: Boolean;
    FCorsAllowedOrigins: string;
    FSettingsFile: string;

    // --- Internal State ---
    FIsActive: Boolean;
    FToolFactories: TDictionary<string, TAiMCPToolFactory>;
    FResourceFactories: TDictionary<string, TAiMCPResourceFactory>;
    FActiveTools: TDictionary<string, IAiMCPTool>;
    FActiveResources: TDictionary<string, IAiMCPResource>;
    FUser: String;

    // --- Private Methods ---
    function ParseJSONRequest(const RequestBody: string): TJSONObject;
    function ExtractRequestID(JSONRequest: TJSONObject): TValue;
    function CreateJSONResponse(const RequestID: TValue): TJSONObject;
    procedure AddRequestIDToResponse(Response: TJSONObject; const RequestID: TValue);
    function CreateErrorResponse(const RequestID: TValue; ErrorCode: Integer; const ErrorMessage: string): string;
    function ExecuteMethodCall(const MethodName: string; Params: TJSONObject; const SessionID: string): TValue;
    function HandleCoreMethod(const Method: string; const Params: TJSONObject): TValue;
    function HandleToolsMethod(const Method: string; const Params: TJSONObject): TValue;
    function HandleResourcesMethod(const Method: string; const Params: TJSONObject): TValue;
    function Core_Initialize(const Params: TJSONObject): TValue;
    function Core_Ping: TValue;
    function Tools_ListTools: TValue;
    function Tools_CallTool(const Params: TJSONObject): TValue;
    function Resources_ListResources: TValue;
    function Resources_ReadResource(const Params: TJSONObject): TValue;
    function Resources_ListTemplates: TValue;
    procedure SetUser(const Value: String);
    procedure SetSettingsFile(const Value: String);

  Protected
    procedure LoadSettingsDefaults;
    procedure CreateDefaultSettingsFile;
    procedure LoadSettingsFromFile;

  public
    constructor Create(const ASettingsFile: string = '');
    destructor Destroy; override;

    procedure RegisterTool(const Name: string; Factory: TAiMCPToolFactory);
    procedure RegisterResource(const URI: string; Factory: TAiMCPResourceFactory);

    procedure Start;
    procedure Stop;

    function ExecuteRequest(const ARequestJson: string; const ASessionID: string): string;
    property SettingsFile: String read FSettingsFile write SetSettingsFile;
    property IsActive: Boolean read FIsActive;
    // Public properties for settings for convenience
    property Port: Integer read FPort write FPort;
    property Host: string read FHost write FHost;
    property ServerName: string read FServerName write FServerName;
    property ProtocolVersion: string read FProtocolVersion write FProtocolVersion;
    property CorsEnabled: Boolean read FCorsEnabled write FCorsEnabled;
    property CorsAllowedOrigins: string read FCorsAllowedOrigins write FCorsAllowedOrigins;
    property User: String read FUser write SetUser;

  end;

  TAiMCPServer = class(TComponent)
  private
    FActive: Boolean;
    FCorsEnabled: Boolean;
    FEndPoint: String;
    FCorsAllowedOrigins: string;
    function GetEndpoint: string;
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetUser: String;
    procedure SetUser(const Value: String);
    function GetSettingsFile: String;
    procedure SetSettingsFile(const Value: String);
  protected
    // Lo hacemos protected para que los descendientes puedan acceder a él directamente.
    FLogicServer: TAiMCPLogicServer;
    // Hacemos el setter protected para que solo los descendientes controlen el estado.
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Hacemos Start y Stop virtuales para que los descendientes puedan extenderlos.
    procedure Start; virtual;
    procedure Stop; virtual;

    procedure RegisterTool(const Name: string; Factory: TAiMCPToolFactory);
    procedure RegisterResource(const URI: string; Factory: TAiMCPResourceFactory);

    procedure LoadSettingsDefaults;
    procedure CreateDefaultSettingsFile;
    procedure LoadSettingsFromFile;

    property Port: Integer read GetPort write SetPort;
    property Endpoint: string read GetEndpoint;
    property CorsEnabled: Boolean read FCorsEnabled write FCorsEnabled;
    property CorsAllowedOrigins: string read FCorsAllowedOrigins write FCorsAllowedOrigins;

    // Propiedades comunes a todos los servidores
    property LogicServer: TAiMCPLogicServer read FLogicServer;
    property IsActive: Boolean read FActive;
    property User: String read GetUser Write SetUser;
    property SettingsFile: String read GetSettingsFile write SetSettingsFile;
  end;

implementation

uses
  System.IniFiles, System.JSON.Writers;

const
  // JSON-RPC 2.0 Error Codes
  JSONRPC_PARSE_ERROR = -32700;
  JSONRPC_INVALID_REQUEST = -32600;
  JSONRPC_METHOD_NOT_FOUND = -32601;
  JSONRPC_INVALID_PARAMS = -32602;
  JSONRPC_INTERNAL_ERROR = -32603;

  //
  // --- SECTION: Internal Helper Classes (Definition of methods) ---
  //
type
  TInternalSchemaGenerator = class
  private
    class function GetJsonTypeFromRttiType(RttiType: TRttiType): string;
    class function GetPropertyJsonName(Prop: TRttiProperty): string;
    class function IsRequiredProperty(Prop: TRttiProperty): Boolean;
  public
    class function GenerateSchema(Cls: TClass): TJSONObject;
  end;

  TInternalSerializer = class
  private
    class var FContext: TRttiContext;
    class function ConvertJsonToValue(const JsonValue: TJSONValue; const RttiType: TRttiType): TValue;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure DeserializeObject(Instance: TObject; const JSON: TJSONObject);
  end;

  // --- Public Utility Class Implementations ---

class function TAiMCPSchemaUtils.GenerateSchema(AClass: TClass): TJSONObject;
begin
  // This is fine because GenerateSchema is NOT a generic method.
  Result := TInternalSchemaGenerator.GenerateSchema(AClass);
end;

class procedure TAiMCPSerializerUtils.DeserializeObject(Instance: TObject; JSON: TJSONObject);
begin
  // Como este método NO es genérico, sí puede llamar a un tipo local de la implementation.
  TInternalSerializer.DeserializeObject(Instance, JSON);
end;
// -----------------------------------------------------------

class function TAiMCPSerializerUtils.Deserialize<T>(JSON: TJSONObject): T;
begin
  Result := T.Create;
  try
    // Ahora llamamos al intermediario público, lo que es válido para el compilador.
    DeserializeObject(Result, JSON);
  except
    Result.Free;
    raise;
  end;
end;

// --- Attribute Implementations ---

constructor AiMCPSchemaDescriptionAttribute.Create(const ADescription: string);
begin
  inherited Create;
  FDescription := ADescription;
end;

constructor AiMCPSchemaEnumAttribute.Create(const AValues: array of string);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FValues, Length(AValues));
  for I := 0 to High(AValues) do
    FValues[I] := AValues[I];
end;

// -----------------------------------------------------------------------------
// --- Implementation: TAiMCPToolBase<T> ---
// -----------------------------------------------------------------------------
constructor TAiMCPToolBase<T>.Create;
begin
  inherited Create;
end;

function TAiMCPToolBase<T>.GetName: string;
begin
  Result := FName;
end;

function TAiMCPToolBase<T>.GetDescription: string;
begin
  Result := FDescription;
end;

function TAiMCPToolBase<T>.GetInputSchema: TJSONObject;
begin
  Result := TAiMCPSchemaUtils.GenerateSchema(T);
end;

function TAiMCPToolBase<T>.Execute(const Arguments: TJSONObject; const AuthContext: TAiAuthContext): TJSONObject;
var
  ParamsInstance: T;
begin
  // El código de deserialización es el mismo
  if not Assigned(Arguments) then
    raise Exception.Create('Arguments cannot be nil for tool execution.');

  ParamsInstance := TAiMCPSerializerUtils.Deserialize<T>(Arguments);
  try
    // La diferencia es que ahora el resultado es un TJSONObject
    Result := ExecuteWithParams(ParamsInstance, AuthContext);
  finally
    ParamsInstance.Free;
  end;
end;

// -----------------------------------------------------------------------------
// --- Implementation: TAiMCPResourceBase<T> ---
// -----------------------------------------------------------------------------
constructor TAiMCPResourceBase<T>.Create;
begin
  inherited Create;
end;

function TAiMCPResourceBase<T>.GetURI: string;
begin
  Result := FURI;
end;

function TAiMCPResourceBase<T>.GetName: string;
begin
  Result := FName;
end;

function TAiMCPResourceBase<T>.GetDescription: string;
begin
  Result := FDescription;
end;

function TAiMCPResourceBase<T>.GetMimeType: string;
begin
  Result := FMimeType;
end;

function TAiMCPResourceBase<T>.Read: string;
var
  ResourceData: T;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
begin
  ResourceData := GetResourceData;
  try
    if SameText(FMimeType, 'application/json') then
    begin
      Result := TJson.ObjectToJsonString(ResourceData);
    end
    else // Assumes other text-based mime-types
    begin
      RttiContext := TRttiContext.Create;
      try
        RttiType := RttiContext.GetType(ResourceData.ClassType);
        Prop := RttiType.GetProperty('Content');
        if Assigned(Prop) and (Prop.PropertyType.TypeKind in [tkString, tkUString, tkLString, tkWString]) then
        begin
          Result := Prop.GetValue(TObject(ResourceData)).AsString;
        end
        else
          Result := 'Error: Resource object does not have a readable string property named "Content".';
      finally
        RttiContext.Free;
      end;
    end;
  finally
    ResourceData.Free;
  end;
end;

// -----------------------------------------------------------------------------
// --- TAiMCPServer Implementation ---
// -----------------------------------------------------------------------------

constructor TAiMCPLogicServer.Create(const ASettingsFile: string);
begin
  inherited Create;
  FIsActive := False;
  FToolFactories := TDictionary<string, TAiMCPToolFactory>.Create;
  FResourceFactories := TDictionary<string, TAiMCPResourceFactory>.Create;
  FActiveTools := TDictionary<string, IAiMCPTool>.Create;
  FActiveResources := TDictionary<string, IAiMCPResource>.Create;

  if ASettingsFile = '' then
    FSettingsFile := ChangeFileExt(ParamStr(0), '.ini')
  else
    FSettingsFile := ASettingsFile;

  LoadSettingsDefaults;

  if TFile.Exists(FSettingsFile) then
    LoadSettingsFromFile;
end;

destructor TAiMCPLogicServer.Destroy;
begin
  Stop;
  FToolFactories.Free;
  FResourceFactories.Free;
  FActiveTools.Free;
  FActiveResources.Free;
  inherited;
end;

procedure TAiMCPLogicServer.RegisterTool(const Name: string; Factory: TAiMCPToolFactory);
begin
  if FIsActive then
    raise Exception.Create('Cannot register tools while the server is active.');
  FToolFactories.AddOrSetValue(Name, Factory);
end;

procedure TAiMCPLogicServer.RegisterResource(const URI: string; Factory: TAiMCPResourceFactory);
begin
  if FIsActive then
    raise Exception.Create('Cannot register resources while the server is active.');
  FResourceFactories.AddOrSetValue(URI, Factory);
end;

procedure TAiMCPLogicServer.SetSettingsFile(const Value: String);
begin
  FSettingsFile := Value;
end;

procedure TAiMCPLogicServer.SetUser(const Value: String);
begin
  FUser := Value;
end;

procedure TAiMCPLogicServer.Start;
var
  Pair: TPair<string, TAiMCPToolFactory>;
  ResourcePair: TPair<string, TAiMCPResourceFactory>;
begin
  if FIsActive then
    Exit;

  for Pair in FToolFactories do
  begin
    var
    Tool := Pair.Value();
    FActiveTools.Add(Tool.Name, Tool);
  end;

  for ResourcePair in FResourceFactories do
  begin
    var
    Resource := ResourcePair.Value();
    FActiveResources.Add(Resource.URI, Resource);
  end;

  FIsActive := True;
end;

procedure TAiMCPLogicServer.Stop;
begin
  if not FIsActive then
    Exit;
  FActiveTools.Clear;
  FActiveResources.Clear;
  FIsActive := False;
end;

procedure TAiMCPLogicServer.LoadSettingsDefaults;
begin
  FPort := 3000;
  FHost := 'localhost';
  FServerName := 'MakerAi-MCPServer';
  FServerVersion := '1.0.0';
  FProtocolVersion := '2025-03-26';
  FCorsEnabled := True;
  FCorsAllowedOrigins := '*';
end;

procedure TAiMCPLogicServer.CreateDefaultSettingsFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FSettingsFile);
  try
    IniFile.WriteInteger('Server', 'Port', FPort);
    IniFile.WriteString('Server', 'Host', FHost);
    IniFile.WriteString('Server', 'Name', FServerName);
    IniFile.WriteString('Protocol', 'Version', FProtocolVersion);
    IniFile.WriteBool('CORS', 'Enabled', FCorsEnabled);
    IniFile.WriteString('CORS', 'AllowedOrigins', FCorsAllowedOrigins);
  finally
    IniFile.Free;
  end;
end;

procedure TAiMCPLogicServer.LoadSettingsFromFile;
var
  IniFile: TIniFile;
begin

  if FSettingsFile = '' then
    FSettingsFile := ChangeFileExt(ParamStr(0), '.ini');

  if not TFile.Exists(FSettingsFile) then
    Exit;

  IniFile := TIniFile.Create(FSettingsFile);
  try
    FPort := IniFile.ReadInteger('Server', 'Port', FPort);
    FHost := IniFile.ReadString('Server', 'Host', FHost);
    FServerName := IniFile.ReadString('Server', 'Name', FServerName);
    FProtocolVersion := IniFile.ReadString('Protocol', 'Version', FProtocolVersion);
    FCorsEnabled := IniFile.ReadBool('CORS', 'Enabled', FCorsEnabled);
    FCorsAllowedOrigins := IniFile.ReadString('CORS', 'AllowedOrigins', FCorsAllowedOrigins);
  finally
    IniFile.Free;
  end;
end;

function TAiMCPLogicServer.ExecuteRequest(const ARequestJson: string; const ASessionID: string): string;
var
  JSONRequest, JSONResponse, Params: TJSONObject;
  RequestID: TValue;
  MethodName: string;
  IsNotification: Boolean;
  ExecuteResult: TValue;
begin

  if not FIsActive then
  begin
    Result := '{"jsonrpc": "2.0", "error": {"code": -32000, "message": "Server not active"}, "id": null}';
    Exit;
  end;

  JSONRequest := nil;
  JSONResponse := nil;
  RequestID := TValue.Empty;

  try
    try
      JSONRequest := ParseJSONRequest(ARequestJson);
      RequestID := ExtractRequestID(JSONRequest);
      MethodName := JSONRequest.GetValue<string>('method', '');
      IsNotification := not Assigned(JSONRequest.GetValue('id'));

      if IsNotification then
      begin
        Exit('');
      end;

      JSONResponse := CreateJSONResponse(RequestID);

      var
      ParamsValue := JSONRequest.GetValue('params');
      if Assigned(ParamsValue) and (ParamsValue is TJSONObject) then
        Params := ParamsValue as TJSONObject
      else
        Params := nil;

      ExecuteResult := ExecuteMethodCall(MethodName, Params, ASessionID);

      if ExecuteResult.IsEmpty then
        JSONResponse.AddPair('result', TJSONNull.Create)
      else if ExecuteResult.IsType<TJSONObject> then
        JSONResponse.AddPair('result', ExecuteResult.AsType<TJSONObject>.Clone as TJSONObject)
      else
        JSONResponse.AddPair('result', TJSONString.Create(ExecuteResult.ToString));

      Result := JSONResponse.ToJSON;

    except
      on E: Exception do
      begin
        Result := CreateErrorResponse(RequestID, JSONRPC_PARSE_ERROR, 'Parse error: ' + E.Message);
        var
        ErrorCode := JSONRPC_INTERNAL_ERROR;
        if Pos('not found', E.Message) > 0 then
          ErrorCode := JSONRPC_METHOD_NOT_FOUND;
        if Pos('Invalid params', E.Message) > 0 then
          ErrorCode := JSONRPC_INVALID_PARAMS;

        if Assigned(JSONRequest) and RequestID.IsEmpty then
          RequestID := ExtractRequestID(JSONRequest);

        Result := CreateErrorResponse(RequestID, ErrorCode, E.Message);
      end;
    end;
  finally
    JSONRequest.Free;
    JSONResponse.Free;
  end;
end;

function TAiMCPLogicServer.ParseJSONRequest(const RequestBody: string): TJSONObject;
var
  ParsedValue: TJSONValue;
begin
  try
    ParsedValue := TJSONObject.ParseJSONValue(RequestBody, False);
    if not(ParsedValue is TJSONObject) then
    begin
      ParsedValue.Free;
      raise Exception.Create('Invalid JSON request: Not a JSON object.');
    end;
    Result := ParsedValue as TJSONObject;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

function TAiMCPLogicServer.ExtractRequestID(JSONRequest: TJSONObject): TValue;
var
  IdValue: TJSONValue;
begin
  IdValue := JSONRequest.GetValue('id');
  if not Assigned(IdValue) or (IdValue is TJSONNull) then
    Exit(TValue.Empty);

  if IdValue is TJSONNumber then
    Result := TValue.From<Int64>((IdValue as TJSONNumber).AsInt64)
  else if IdValue is TJSONString then
    Result := TValue.From<string>((IdValue as TJSONString).Value)
  else
    Result := TValue.Empty;
end;

function TAiMCPLogicServer.CreateJSONResponse(const RequestID: TValue): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('jsonrpc', '2.0');
  AddRequestIDToResponse(Result, RequestID);
end;

procedure TAiMCPLogicServer.AddRequestIDToResponse(Response: TJSONObject; const RequestID: TValue);
begin
  if RequestID.IsEmpty then
    Response.AddPair('id', TJSONNull.Create)
  else if RequestID.IsType<String> then
    Response.AddPair('id', RequestID.AsString)
  else if RequestID.IsOrdinal then
    Response.AddPair('id', TJSONNumber.Create(RequestID.AsInt64))
  else
    Response.AddPair('id', TJSONNull.Create);
end;

function TAiMCPLogicServer.CreateErrorResponse(const RequestID: TValue; ErrorCode: Integer; const ErrorMessage: string): string;
var
  JSONResponse, ErrorObj: TJSONObject;
begin
  JSONResponse := TJSONObject.Create;
  try
    JSONResponse.AddPair('jsonrpc', '2.0');
    ErrorObj := TJSONObject.Create;
    ErrorObj.AddPair('code', TJSONNumber.Create(ErrorCode));
    ErrorObj.AddPair('message', ErrorMessage);
    JSONResponse.AddPair('error', ErrorObj);
    AddRequestIDToResponse(JSONResponse, RequestID);
    Result := JSONResponse.ToJSON;
  finally
    JSONResponse.Free;
  end;
end;

function TAiMCPLogicServer.ExecuteMethodCall(const MethodName: string; Params: TJSONObject; const SessionID: string): TValue;
begin
  if Trim(MethodName) = '' then
    raise Exception.CreateHelp('Method not specified.', JSONRPC_INVALID_REQUEST);

  if StartsText('tools/', MethodName) then
    Result := HandleToolsMethod(MethodName, Params)
  else if StartsText('resources/', MethodName) then
    Result := HandleResourcesMethod(MethodName, Params)
  else if (MethodName = 'initialize') or (MethodName = 'ping') then
    Result := HandleCoreMethod(MethodName, Params)
  else
    raise Exception.CreateFmt('Method [%s] not found.', [MethodName]);
end;

function TAiMCPLogicServer.HandleCoreMethod(const Method: string; const Params: TJSONObject): TValue;
begin
  if Method = 'initialize' then
    Result := Core_Initialize(Params)
  else if Method = 'ping' then
    Result := Core_Ping
  else
    Result := TValue.Empty;
end;

function TAiMCPLogicServer.HandleToolsMethod(const Method: string; const Params: TJSONObject): TValue;
begin
  if Method = 'tools/list' then
    Result := Tools_ListTools
  else if Method = 'tools/call' then
    Result := Tools_CallTool(Params)
  else
    raise Exception.CreateFmt('Method %s not handled by Tools capability', [Method]);
end;

function TAiMCPLogicServer.HandleResourcesMethod(const Method: string; const Params: TJSONObject): TValue;
begin
  if Method = 'resources/list' then
    Result := Resources_ListResources
  else if Method = 'resources/read' then
    Result := Resources_ReadResource(Params)
  else if Method = 'resources/templates/list' then
    Result := Resources_ListTemplates
  else
    raise Exception.CreateFmt('Method %s not handled by Resources capability', [Method]);
end;

function TAiMCPLogicServer.Core_Initialize(const Params: TJSONObject): TValue;
var
  ResultJSON, Capabilities, ToolsCap, ResourcesCap, ServerInfo: TJSONObject;
begin
  ResultJSON := TJSONObject.Create;
  ResultJSON.AddPair('protocolVersion', FProtocolVersion);
  Capabilities := TJSONObject.Create;
  ResultJSON.AddPair('capabilities', Capabilities);
  ToolsCap := TJSONObject.Create;
  Capabilities.AddPair('tools', ToolsCap);
  ResourcesCap := TJSONObject.Create;
  Capabilities.AddPair('resources', ResourcesCap);
  ServerInfo := TJSONObject.Create;
  ResultJSON.AddPair('serverInfo', ServerInfo);
  ServerInfo.AddPair('name', FServerName);
  ServerInfo.AddPair('version', FServerVersion);
  Result := TValue.From<TJSONObject>(ResultJSON);
end;

function TAiMCPLogicServer.Core_Ping: TValue;
begin
  Result := TValue.From<TJSONObject>(TJSONObject.Create);
end;

function TAiMCPLogicServer.Tools_ListTools: TValue;
var
  ResultJSON, ToolJSON: TJSONObject;
  ToolsArray: TJSONArray;
  Tool: IAiMCPTool;
begin
  ResultJSON := TJSONObject.Create;
  ToolsArray := TJSONArray.Create;
  ResultJSON.AddPair('tools', ToolsArray);
  for Tool in FActiveTools.Values do
  begin
    ToolJSON := TJSONObject.Create;
    ToolJSON.AddPair('name', Tool.Name);
    ToolJSON.AddPair('description', Tool.Description);
    ToolJSON.AddPair('inputSchema', Tool.GetInputSchema.Clone as TJSONObject);
    ToolsArray.AddElement(ToolJSON);
  end;
  Result := TValue.From<TJSONObject>(ResultJSON);
end;

function TAiMCPLogicServer.Tools_CallTool(const Params: TJSONObject): TValue;
var
  ToolName: string;
  Arguments: TJSONObject;
  Tool: IAiMCPTool;
  // ResultText: string; // Ya no es un string
  ResultJSON: TJSONObject; // La herramienta devolverá el JSON directamente
  AuthContext: TAiAuthContext;
begin
  if not Assigned(Params) then
    raise Exception.CreateHelp('Invalid params for tools/call. Expected a JSON object.', JSONRPC_INVALID_PARAMS);

  ToolName := Params.GetValue<string>('name', '');
  Arguments := Params.GetValue<TJSONObject>('arguments', nil);

  if ToolName = '' then
    raise Exception.CreateHelp('Invalid params: Tool "name" not provided in tools/call', JSONRPC_INVALID_PARAMS);

  AuthContext.IsAuthenticated := (FUser <> '');
  AuthContext.UserID := FUser;

  if FActiveTools.TryGetValue(ToolName, Tool) then
  begin
    // La herramienta ahora devuelve el TJSONObject directamente
    ResultJSON := Tool.Execute(Arguments, AuthContext);
  end
  else
    raise Exception.CreateFmt('Tool not found: %s', [ToolName]);

  // Pasamos el TJSONObject resultante al TValue
  Result := TValue.From<TJSONObject>(ResultJSON);
end;

function TAiMCPLogicServer.Resources_ListResources: TValue;
var
  ResultJSON, ResourceObj: TJSONObject;
  ResourcesArray: TJSONArray;
  Resource: IAiMCPResource;
begin
  ResultJSON := TJSONObject.Create;
  ResourcesArray := TJSONArray.Create;
  ResultJSON.AddPair('resources', ResourcesArray);
  for Resource in FActiveResources.Values do
  begin
    ResourceObj := TJSONObject.Create;
    ResourceObj.AddPair('uri', Resource.URI);
    ResourceObj.AddPair('name', Resource.Name);
    ResourceObj.AddPair('description', Resource.Description);
    ResourceObj.AddPair('mimeType', Resource.MimeType);
    ResourcesArray.AddElement(ResourceObj);
  end;
  Result := TValue.From<TJSONObject>(ResultJSON);
end;

function TAiMCPLogicServer.Resources_ReadResource(const Params: TJSONObject): TValue;
var
  URI, ResourceText: string;
  ResultJSON, ContentItem: TJSONObject;
  ContentsArray: TJSONArray;
  Resource: IAiMCPResource;
begin
  if not Assigned(Params) then
    raise Exception.CreateHelp('Invalid params for resources/read. Expected a JSON object.', JSONRPC_INVALID_PARAMS);

  URI := Params.GetValue<string>('uri', '');
  if URI = '' then
    raise Exception.CreateHelp('Invalid params: Resource "uri" not provided in resources/read', JSONRPC_INVALID_PARAMS);

  ResultJSON := TJSONObject.Create;
  ContentsArray := TJSONArray.Create;
  ResultJSON.AddPair('contents', ContentsArray);
  ContentItem := TJSONObject.Create;
  ContentsArray.AddElement(ContentItem);

  if FActiveResources.TryGetValue(URI, Resource) then
  begin
    ContentItem.AddPair('uri', Resource.URI);
    ContentItem.AddPair('mimeType', Resource.MimeType);
    try
      ResourceText := Resource.Read;
      ContentItem.AddPair('text', ResourceText);
    except
      on E: Exception do
        raise Exception.Create('Error reading resource: ' + E.Message);
    end;
  end
  else
  begin
    raise Exception.CreateFmt('Resource not found: %s', [URI]);
  end;
  Result := TValue.From<TJSONObject>(ResultJSON);
end;

function TAiMCPLogicServer.Resources_ListTemplates: TValue;
var
  ResultJSON: TJSONObject;
  TemplatesArray: TJSONArray;
begin
  ResultJSON := TJSONObject.Create;
  TemplatesArray := TJSONArray.Create;
  ResultJSON.AddPair('resourceTemplates', TemplatesArray);
  Result := TValue.From<TJSONObject>(ResultJSON);
end;

// -----------------------------------------------------------------------------
// --- TInternalSchemaGenerator Implementation ---
// -----------------------------------------------------------------------------

class function TInternalSchemaGenerator.GenerateSchema(Cls: TClass): TJSONObject;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Properties, PropSchema: TJSONObject;
  RequiredArray, EnumArray: TJSONArray;
  JsonName: string;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
begin
  Result := TJSONObject.Create;
  Result.AddPair('type', 'object');
  Properties := TJSONObject.Create;
  Result.AddPair('properties', Properties);
  RequiredArray := TJSONArray.Create;
  Result.AddPair('required', RequiredArray);

  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(Cls);
    for RttiProp in RttiType.GetProperties do
    begin
      if RttiProp.Visibility in [mvPublic, mvPublished] then
      begin
        JsonName := GetPropertyJsonName(RttiProp);
        PropSchema := TJSONObject.Create;
        Properties.AddPair(JsonName, PropSchema);
        PropSchema.AddPair('type', GetJsonTypeFromRttiType(RttiProp.PropertyType));

        for Attr in RttiProp.GetAttributes do
        begin
          if Attr is AiMCPSchemaDescriptionAttribute then
            PropSchema.AddPair('description', (Attr as AiMCPSchemaDescriptionAttribute).Description)
          else if Attr is AiMCPSchemaEnumAttribute then
          begin
            EnumArray := TJSONArray.Create;
            for var Value in (Attr as AiMCPSchemaEnumAttribute).Values do
              EnumArray.Add(Value);
            PropSchema.AddPair('enum', EnumArray);
          end;
        end;

        if IsRequiredProperty(RttiProp) then
          RequiredArray.Add(JsonName);
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

class function TInternalSchemaGenerator.GetJsonTypeFromRttiType(RttiType: TRttiType): string;
begin
  case RttiType.TypeKind of
    tkInteger, tkInt64:
      Result := 'integer';
    tkFloat:
      Result := 'number';
    tkString, tkLString, tkWString, tkUString:
      Result := 'string';
    tkEnumeration:
      if RttiType.Name = 'Boolean' then
        Result := 'boolean'
      else
        Result := 'string';
    tkClass, tkDynArray, tkArray:
      Result := 'object';
  else
    Result := 'string';
  end;
end;

class function TInternalSchemaGenerator.GetPropertyJsonName(Prop: TRttiProperty): string;
begin
  Result := LowerCase(Prop.Name[1]) + Copy(Prop.Name, 2, Length(Prop.Name));
end;

class function TInternalSchemaGenerator.IsRequiredProperty(Prop: TRttiProperty): Boolean;
begin
  Result := not Prop.HasAttribute<AiMCPOptionalAttribute>;
end;

// -----------------------------------------------------------------------------
// --- TInternalSerializer Implementation ---
// -----------------------------------------------------------------------------

class constructor TInternalSerializer.Create;
begin
  FContext := TRttiContext.Create;
end;

class destructor TInternalSerializer.Destroy;
begin
  FContext.Free;
end;

class procedure TInternalSerializer.DeserializeObject(Instance: TObject; const JSON: TJSONObject);
var
  RttiType: TRttiType;
  PropName: string;
  JsonValue: TJSONValue;
  PropValue: TValue;
  RttiProp: TRttiProperty;
begin
  RttiType := FContext.GetType(Instance.ClassType);
  for RttiProp in RttiType.GetProperties do
  begin
    if not RttiProp.IsWritable then
      Continue;

    PropName := TInternalSchemaGenerator.GetPropertyJsonName(RttiProp);
    JsonValue := JSON.GetValue(PropName);

    if Assigned(JsonValue) then
    begin
      PropValue := ConvertJsonToValue(JsonValue, RttiProp.PropertyType);
      if not PropValue.IsEmpty then
        RttiProp.SetValue(Instance, PropValue);
    end;
  end;
end;

class function TInternalSerializer.ConvertJsonToValue(const JsonValue: TJSONValue; const RttiType: TRttiType): TValue;
begin
  Result := TValue.Empty;
  if (JsonValue = nil) or (JsonValue is TJSONNull) then
    Exit;

  case RttiType.TypeKind of
    tkInteger, tkInt64:
      if JsonValue is TJSONNumber then
        Result := (JsonValue as TJSONNumber).AsInt64
      else
        raise EConversionError.CreateFmt('JSON value for an integer property is not a number: %s', [JsonValue.ToString]);

    tkFloat:
      if JsonValue is TJSONNumber then
        Result := (JsonValue as TJSONNumber).AsDouble
      else
        raise EConversionError.CreateFmt('JSON value for a float property is not a number: %s', [JsonValue.ToString]);

    tkString, tkUString, tkWString, tkLString:
      if JsonValue is TJSONString then
        Result := (JsonValue as TJSONString).Value
      else
        Result := JsonValue.Value;

    tkEnumeration:
      if RttiType.Handle = TypeInfo(Boolean) then
      begin
        if JsonValue is TJSONBool then
          Result := (JsonValue as TJSONBool).AsBoolean
        else
          raise EConversionError.CreateFmt('JSON value for a boolean property is not a boolean: %s', [JsonValue.ToString]);
      end
      else
      begin
        if JsonValue is TJSONString then
          Result := TValue.FromOrdinal(RttiType.Handle, GetEnumValue(RttiType.Handle, (JsonValue as TJSONString).Value))
        else
          raise EConversionError.CreateFmt('JSON value for an enum property is not a string: %s', [JsonValue.ToString]);
      end;
  end;
end;


// -----------------------------------------------------------------------------
// --- TAiMCPResponseBuilder Implementation ---
// -----------------------------------------------------------------------------

constructor TAiMCPResponseBuilder.Create;
begin
  inherited;
  FContentArray := TJSONArray.Create;
end;

destructor TAiMCPResponseBuilder.Destroy;
begin
  FContentArray.Free;
  inherited;
end;

class function TAiMCPResponseBuilder.New: TAiMCPResponseBuilder;
begin
  Result := TAiMCPResponseBuilder.Create;
end;

function TAiMCPResponseBuilder.AddText(const AText: string): TAiMCPResponseBuilder;
var
  LTextItem: TJSONObject;
begin
  Result := Self;
  LTextItem := TJSONObject.Create;
  LTextItem.AddPair('type', 'text');
  LTextItem.AddPair('text', AText);
  FContentArray.AddElement(LTextItem);
end;

function TAiMCPResponseBuilder.AddFile(const AFilePath: string; AFileName: string = ''): TAiMCPResponseBuilder;
var
  LFileStream: TFileStream;
  LMimeType: string;
begin
  Result := Self;
  if not TFile.Exists(AFilePath) then
    raise Exception.CreateFmt('File not found: %s', [AFilePath]);

  LMimeType := GetMimeTypeFromFile(AFilePath);
  if AFileName = '' then
    AFileName := TPath.GetFileName(AFilePath);

  LFileStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    AddFileFromStream(LFileStream, AFileName, LMimeType);
  finally
    LFileStream.Free;
  end;
end;

function TAiMCPResponseBuilder.AddFileFromStream(AStream: TStream; const AFileName: string; const AMimeType: string): TAiMCPResponseBuilder;
var
  LFileItem: TJSONObject;
  LBytes: TBytes;
  LBase64: string;
  LType: string;
begin
  Result := Self;
  AStream.Position := 0;
  SetLength(LBytes, AStream.Size);
  AStream.ReadBuffer(LBytes, Length(LBytes));

  LBase64 := TNetEncoding.Base64.EncodeBytesToString(LBytes);

  LFileItem := TJSONObject.Create;

  // Mapeamos el MimeType al 'type' que espera el cliente
  if StartsText('image/', AMimeType) then
    LType := 'image'
  else if StartsText('audio/', AMimeType) then
    LType := 'audio'
  else if StartsText('video/', AMimeType) then
    LType := 'video'
  else if SameText(AMimeType, 'application/pdf') then
    LType := 'document'
  else
    LType := 'binary'; // Un tipo genérico

  LFileItem.AddPair('type', LType);
  LFileItem.AddPair('mimeType', AMimeType);
  LFileItem.AddPair('data', LBase64);
  LFileItem.AddPair('fileName', AFileName); // Opcional, pero buena práctica

  FContentArray.AddElement(LFileItem);
end;

function TAiMCPResponseBuilder.Build: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('content', FContentArray.Clone as TJSONArray);
end;

{ TAiMCPServer }

constructor TAiMCPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEndPoint := '/mcp';
  FActive := False;
  FLogicServer := TAiMCPLogicServer.Create;
end;

procedure TAiMCPServer.CreateDefaultSettingsFile;
begin
  FLogicServer.CreateDefaultSettingsFile
end;

destructor TAiMCPServer.Destroy;
begin
  Stop;
  FLogicServer.Free;
  inherited Destroy;
end;


function TAiMCPServer.GetEndpoint: string;
begin
  Result := FEndPoint;
end;

function TAiMCPServer.GetPort: Integer;
begin
  Result := FLogicServer.Port;
end;

function TAiMCPServer.GetSettingsFile: String;
begin
  Result := FLogicServer.SettingsFile;
end;

function TAiMCPServer.GetUser: String;
begin
  Result := FLogicServer.User;
end;

procedure TAiMCPServer.LoadSettingsDefaults;
begin
  FLogicServer.LoadSettingsDefaults;
end;

procedure TAiMCPServer.LoadSettingsFromFile;
begin
  FLogicServer.LoadSettingsFromFile;
end;

procedure TAiMCPServer.RegisterResource(const URI: string; Factory: TAiMCPResourceFactory);
begin
  FLogicServer.RegisterResource(URI, Factory);
end;

procedure TAiMCPServer.RegisterTool(const Name: string; Factory: TAiMCPToolFactory);
begin
  FLogicServer.RegisterTool(Name, Factory)
end;

procedure TAiMCPServer.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TAiMCPServer.SetPort(const Value: Integer);
begin
  FLogicServer.Port := Value;
end;

procedure TAiMCPServer.SetSettingsFile(const Value: String);
begin
  FLogicServer.SettingsFile := Value;
end;

procedure TAiMCPServer.SetUser(const Value: String);
begin
  FLogicServer.User := Value;
end;

procedure TAiMCPServer.Start;
begin
  if FActive then
    Exit;
  FLogicServer.Start;
  FActive := True;
end;

procedure TAiMCPServer.Stop;
begin
  if not FActive then
    Exit;
  FLogicServer.Stop;
  FActive := False;
end;

end.
