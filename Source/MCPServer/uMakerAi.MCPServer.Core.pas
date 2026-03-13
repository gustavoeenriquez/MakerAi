// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Motor JSON-RPC 2.0 para el servidor MCP (Model Context Protocol).
// Adaptaciones respecto a la version Delphi:
//   - System.JSON        → fpjson + jsonparser
//   - TDictionary<K,V>   → TStringList + OwnsObjects / wrapper TAiMCPToolHolder
//   - TArray<string>     → string separado por comas (Roles)
//   - Rest.JSON / RTTI   → no usado (sin generacion de esquema automatica)
//   - TAiMCPToolBase<T>  → TAiMCPToolBase (sin generico — FPC simplificado)
//   - TAiMCPToolFactory  → referencia a funcion de tipo TAiMCPToolFactory

unit uMakerAi.MCPServer.Core;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs,
  fpjson, jsonparser,
  EncdDecd,
  uMakerAi.Tools.Functions;

// ---------------------------------------------------------------------------
// Constantes de error JSON-RPC 2.0
// ---------------------------------------------------------------------------
const
  JSONRPC_PARSE_ERROR      = -32700;
  JSONRPC_INVALID_REQUEST  = -32600;
  JSONRPC_METHOD_NOT_FOUND = -32601;
  JSONRPC_INVALID_PARAMS   = -32602;
  JSONRPC_INTERNAL_ERROR   = -32603;
  MCP_AUTH_FAILED          = -32001;

  MCP_PROTOCOL_VERSION     = '2025-06-18';

type
  // -------------------------------------------------------------------------
  // TAiAuthContext - contexto de autenticacion del cliente
  // -------------------------------------------------------------------------
  TAiAuthContext = record
    IsAuthenticated : Boolean;
    UserID          : string;
    UserName        : string;
    Roles           : string; // roles separados por comas (FPC: sin TArray<string>)
  end;

  // -------------------------------------------------------------------------
  // Evento de validacion custom (Layer 2).
  // AAuthHeader: valor del header Authorization o X-API-Key.
  // ARemoteIP:   IP del cliente.
  // AAuthContext: contexto a rellenar.
  // AIsValid:    True si la peticion es valida.
  // -------------------------------------------------------------------------
  TAiMCPValidateEvent = procedure(Sender: TObject;
      const AAuthHeader, ARemoteIP: string;
      out AAuthContext: TAiAuthContext;
      out AIsValid: Boolean) of object;

  // -------------------------------------------------------------------------
  // IAiMCPTool - interfaz que deben implementar todas las herramientas MCP
  // -------------------------------------------------------------------------
  IAiMCPTool = interface
    ['{B1A4D0F8-9A7B-4C6C-8D1F-4B9E3A5F7C1E}']
    function GetName: string;
    function GetDescription: string;
    function GetInputSchema: TJSONObject;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject;
    property Name: string read GetName;
    property Description: string read GetDescription;
    property InputSchema: TJSONObject read GetInputSchema;
  end;

  // -------------------------------------------------------------------------
  // IAiMCPResource - interfaz para recursos MCP (archivos, URIs, etc.)
  // -------------------------------------------------------------------------
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

  // -------------------------------------------------------------------------
  // TAiMCPToolBase - clase base para herramientas MCP (FPC: sin generic)
  // Para implementar una herramienta, heredar de TAiMCPToolBase y
  // sobreescribir GetInputSchema y Execute.
  // -------------------------------------------------------------------------
  TAiMCPToolBase = class(TInterfacedObject, IAiMCPTool)
  protected
    FName        : string;
    FDescription : string;
  public
    constructor Create(const AName, ADescription: string); virtual;
    function GetName: string;
    function GetDescription: string;
    function GetInputSchema: TJSONObject; virtual;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; virtual;
  end;

  // -------------------------------------------------------------------------
  // TAiMCPResourceBase - clase base para recursos MCP
  // -------------------------------------------------------------------------
  TAiMCPResourceBase = class(TInterfacedObject, IAiMCPResource)
  protected
    FURI         : string;
    FName        : string;
    FDescription : string;
    FMimeType    : string;
  public
    constructor Create(const AURI, AName, ADescription, AMimeType: string); virtual;
    function GetURI: string;
    function GetName: string;
    function GetDescription: string;
    function GetMimeType: string;
    function Read: string; virtual;
  end;

  // -------------------------------------------------------------------------
  // TAiMCPResponseBuilder - constructor fluido de respuestas MCP
  //
  // Uso tipico:
  //   Builder := TAiMCPResponseBuilder.New;
  //   try
  //     Result := Builder.AddText('Respuesta').Build;
  //   finally
  //     Builder.Free;
  //   end;
  //
  // IMPORTANTE: el llamador es responsable de liberar el Builder y el
  // TJSONObject retornado por Build.
  // -------------------------------------------------------------------------
  TAiMCPResponseBuilder = class
  private
    FContentArray : TJSONArray;
    FIsError      : Boolean;
    FErrorMessage : string;
  public
    constructor Create;
  public
    class function New: TAiMCPResponseBuilder;
    destructor Destroy; override;

    // Anade un bloque de texto simple
    function AddText(const AText: string): TAiMCPResponseBuilder;

    // Anade una imagen en base64
    function AddImage(const ABase64Data, AMimeType: string): TAiMCPResponseBuilder;

    // Anade un archivo desde un TStream (convierte a base64)
    function AddFileFromStream(AStream: TStream;
        const AFileName, AMimeType: string): TAiMCPResponseBuilder;

    // Marca la respuesta como error (isError: true)
    function SetError(const AMessage: string): TAiMCPResponseBuilder;

    // Construye el TJSONObject final {content:[...], isError:bool}
    // El llamador es responsable de liberar el resultado.
    function Build: TJSONObject;
  end;

  // -------------------------------------------------------------------------
  // Tipos factory para registro de tools y recursos
  // -------------------------------------------------------------------------
  TAiMCPToolFactory     = function: IAiMCPTool;
  TAiMCPResourceFactory = function: IAiMCPResource;

  // Holder interno: envuelve la interfaz IAiMCPTool en un TObject
  // para poder meterlo en TStringList.Objects[]
  TAiMCPToolHolder = class(TObject)
    Tool: IAiMCPTool;
    constructor Create(ATool: IAiMCPTool);
  end;

  TAiMCPResourceHolder = class(TObject)
    Resource: IAiMCPResource;
    constructor Create(AResource: IAiMCPResource);
  end;

  // -------------------------------------------------------------------------
  // TAiMCPLogicServer - motor JSON-RPC 2.0
  //
  // Recibe peticiones como string JSON, las parsea, despacha al metodo
  // correcto y retorna la respuesta serializada como string JSON.
  // -------------------------------------------------------------------------
  TAiMCPLogicServer = class(TObject)
  private
    FPort               : Integer;
    FHost               : string;
    FServerName         : string;
    FServerVersion      : string;
    FProtocolVersion    : string;
    FCorsEnabled        : Boolean;
    FCorsAllowedOrigins : string;
    FIsActive           : Boolean;
    FUser               : string;

    // Registros de tools y recursos: TStringList con OwnsObjects = True
    // Key = nombre, Value = TAiMCPToolHolder o TAiMCPResourceHolder
    FTools     : TStringList;
    FResources : TStringList;

    // Metodos internos de despacho
    function ParseJSONRequest(const ARequestBody: string): TJSONObject;
    function ExtractIDStr(ARequest: TJSONObject): string;
    function BuildResponseFrame(const AIDStr: string): TJSONObject;
    function CreateErrorResponse(const AIDStr: string;
        ACode: Integer; const AMessage: string): string;

    function HandleInitialize(AParams: TJSONObject;
        const AIDStr: string): string;
    function HandlePing(const AIDStr: string): string;
    function HandleToolsList(const AIDStr: string): string;
    function HandleToolsCall(AParams: TJSONObject; const AIDStr: string;
        const AAuthCtx: TAiAuthContext): string;
    function HandleResourcesList(const AIDStr: string): string;
    function HandleResourcesRead(AParams: TJSONObject;
        const AIDStr: string): string;

  public
    constructor Create;
    destructor Destroy; override;

    // Registro de herramientas y recursos
    procedure RegisterTool(const AName: string; ATool: IAiMCPTool);
    procedure RegisterResource(const AURI: string; AResource: IAiMCPResource);

    procedure Start;
    procedure Stop;

    // Punto de entrada principal: recibe JSON-RPC → devuelve JSON-RPC
    function ExecuteRequest(const ARequestJson, ASessionID: string): string; overload;
    function ExecuteRequest(const ARequestJson, ASessionID: string;
        const AAuthCtx: TAiAuthContext): string; overload;

    property IsActive           : Boolean read FIsActive;
    property Port               : Integer read FPort write FPort;
    property Host               : string  read FHost  write FHost;
    property ServerName         : string  read FServerName  write FServerName;
    property ServerVersion      : string  read FServerVersion write FServerVersion;
    property ProtocolVersion    : string  read FProtocolVersion write FProtocolVersion;
    property CorsEnabled        : Boolean read FCorsEnabled write FCorsEnabled;
    property CorsAllowedOrigins : string  read FCorsAllowedOrigins write FCorsAllowedOrigins;
    property User               : string  read FUser write FUser;
  end;

  // -------------------------------------------------------------------------
  // TAiMCPServer - componente abstracto (base para Http, Stdio, Direct, SSE)
  // -------------------------------------------------------------------------
  TAiMCPServer = class(TComponent)
  private
    FActive              : Boolean;
    FCorsEnabled         : Boolean;
    FEndPoint            : string;
    FCorsAllowedOrigins  : string;
    FServerName          : string;
    FAiFunctions         : TAiFunctions;
    FApiKey              : string;
    FOnValidateRequest   : TAiMCPValidateEvent;

    function  GetEndpoint: string;
    function  GetPort: Integer;
    procedure SetPort(const AValue: Integer);
    function  GetServerName: string;
    procedure SetServerName(const AValue: string);
    procedure SetAiFunctions(const AValue: TAiFunctions);

  protected
    FLogicServer : TAiMCPLogicServer;

    procedure SetActive(const AValue: Boolean);
    procedure InternalRegisterFromAiFunctions;
    procedure Notification(AComponent: TComponent;
        AOperation: TOperation); override;

    // Validacion: Layer 1 (API Key) + Layer 2 (evento custom)
    function ValidateRequest(const AAuthHeader, ARemoteIP: string;
        out AAuthCtx: TAiAuthContext): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; virtual;
    procedure Stop;  virtual;

    // Registro directo de tools y recursos en el LogicServer
    procedure RegisterTool(const AName: string; ATool: IAiMCPTool);
    procedure RegisterResource(const AURI: string; AResource: IAiMCPResource);

    property Port               : Integer read GetPort write SetPort;
    property Endpoint           : string  read GetEndpoint;
    property CorsEnabled        : Boolean read FCorsEnabled write FCorsEnabled;
    property CorsAllowedOrigins : string  read FCorsAllowedOrigins write FCorsAllowedOrigins;
    property LogicServer        : TAiMCPLogicServer read FLogicServer;
    property IsActive           : Boolean read FActive;

  published
    property ServerName       : string  read GetServerName write SetServerName;
    property AiFunctions      : TAiFunctions read FAiFunctions write SetAiFunctions;
    property ApiKey           : string  read FApiKey write FApiKey;
    property OnValidateRequest: TAiMCPValidateEvent
        read FOnValidateRequest write FOnValidateRequest;
  end;


implementation

uses
  StrUtils;

// ---------------------------------------------------------------------------
// TAiMCPToolHolder
// ---------------------------------------------------------------------------

constructor TAiMCPToolHolder.Create(ATool: IAiMCPTool);
begin
  inherited Create;
  Tool := ATool;
end;

// ---------------------------------------------------------------------------
// TAiMCPResourceHolder
// ---------------------------------------------------------------------------

constructor TAiMCPResourceHolder.Create(AResource: IAiMCPResource);
begin
  inherited Create;
  Resource := AResource;
end;

// ---------------------------------------------------------------------------
// TAiMCPToolBase
// ---------------------------------------------------------------------------

constructor TAiMCPToolBase.Create(const AName, ADescription: string);
begin
  inherited Create;
  FName        := AName;
  FDescription := ADescription;
end;

function TAiMCPToolBase.GetName: string;
begin
  Result := FName;
end;

function TAiMCPToolBase.GetDescription: string;
begin
  Result := FDescription;
end;

function TAiMCPToolBase.GetInputSchema: TJSONObject;
var
  Props: TJSONObject;
begin
  // Esquema vacio por defecto — las subclases deben sobreescribir
  Result := TJSONObject.Create;
  Result.Add('type', TJSONString.Create('object'));
  Props := TJSONObject.Create;
  Result.Add('properties', Props);
end;

function TAiMCPToolBase.Execute(const Arguments: TJSONObject;
    const AuthContext: TAiAuthContext): TJSONObject;
begin
  // Implementacion base — subclases deben sobreescribir
  Result := TAiMCPResponseBuilder.New
    .AddText('Tool not implemented: ' + FName)
    .Build;
end;

// ---------------------------------------------------------------------------
// TAiMCPResourceBase
// ---------------------------------------------------------------------------

constructor TAiMCPResourceBase.Create(const AURI, AName, ADescription, AMimeType: string);
begin
  inherited Create;
  FURI         := AURI;
  FName        := AName;
  FDescription := ADescription;
  FMimeType    := AMimeType;
end;

function TAiMCPResourceBase.GetURI: string;        begin Result := FURI;         end;
function TAiMCPResourceBase.GetName: string;       begin Result := FName;        end;
function TAiMCPResourceBase.GetDescription: string; begin Result := FDescription; end;
function TAiMCPResourceBase.GetMimeType: string;   begin Result := FMimeType;    end;

function TAiMCPResourceBase.Read: string;
begin
  Result := '';  // Subclases deben sobreescribir
end;

// ---------------------------------------------------------------------------
// TAiMCPResponseBuilder
// ---------------------------------------------------------------------------

constructor TAiMCPResponseBuilder.Create;
begin
  inherited Create;
  FContentArray := TJSONArray.Create;
  FIsError      := False;
  FErrorMessage := '';
end;

class function TAiMCPResponseBuilder.New: TAiMCPResponseBuilder;
begin
  Result := TAiMCPResponseBuilder.Create;
end;

destructor TAiMCPResponseBuilder.Destroy;
begin
  // FContentArray se transfere al Build; si Build no fue llamado, liberamos aqui
  if Assigned(FContentArray) then
    FContentArray.Free;
  inherited;
end;

function TAiMCPResponseBuilder.AddText(const AText: string): TAiMCPResponseBuilder;
var
  Item: TJSONObject;
begin
  Item := TJSONObject.Create;
  Item.Add('type', TJSONString.Create('text'));
  Item.Add('text', TJSONString.Create(AText));
  FContentArray.Add(Item);
  Result := Self;
end;

function TAiMCPResponseBuilder.AddImage(const ABase64Data,
    AMimeType: string): TAiMCPResponseBuilder;
var
  Item: TJSONObject;
begin
  Item := TJSONObject.Create;
  Item.Add('type', TJSONString.Create('image'));
  Item.Add('mimeType', TJSONString.Create(AMimeType));
  Item.Add('data', TJSONString.Create(ABase64Data));
  FContentArray.Add(Item);
  Result := Self;
end;

function TAiMCPResponseBuilder.AddFileFromStream(AStream: TStream;
    const AFileName, AMimeType: string): TAiMCPResponseBuilder;
var
  Item     : TJSONObject;
  SS       : TStringStream;
  B64Data  : string;
begin
  if not Assigned(AStream) then
    Exit(Self);

  SS := TStringStream.Create('');
  try
    AStream.Position := 0;
    SS.CopyFrom(AStream, AStream.Size);
    if Length(SS.DataString) > 0 then
      B64Data := EncodeBase64(@SS.DataString[1], Length(SS.DataString))
    else
      B64Data := '';
  finally
    SS.Free;
  end;

  Item := TJSONObject.Create;
  Item.Add('type', TJSONString.Create('resource'));
  Item.Add('mimeType', TJSONString.Create(AMimeType));
  Item.Add('name', TJSONString.Create(AFileName));
  Item.Add('data', TJSONString.Create(B64Data));
  FContentArray.Add(Item);
  Result := Self;
end;

function TAiMCPResponseBuilder.SetError(const AMessage: string): TAiMCPResponseBuilder;
begin
  FIsError      := True;
  FErrorMessage := AMessage;
  Result := Self;
end;

function TAiMCPResponseBuilder.Build: TJSONObject;
begin
  Result := TJSONObject.Create;

  if FIsError and (FContentArray.Count = 0) then
    AddText(FErrorMessage);

  // Transfiere posesion de FContentArray al Result
  Result.Add('content', FContentArray);
  FContentArray := nil;  // ya no le pertenece a este builder

  if FIsError then
    Result.Add('isError', TJSONBoolean.Create(True));
end;

// ---------------------------------------------------------------------------
// TAiMCPLogicServer
// ---------------------------------------------------------------------------

constructor TAiMCPLogicServer.Create;
begin
  inherited Create;
  FPort               := 8080;
  FHost               := '0.0.0.0';
  FServerName         := 'MakerAI MCP Server';
  FServerVersion      := '1.0';
  FProtocolVersion    := MCP_PROTOCOL_VERSION;
  FCorsEnabled        := False;
  FCorsAllowedOrigins := '*';
  FIsActive           := False;
  FUser               := '';

  // TStringList con OwnsObjects: libera automaticamente los Holders
  FTools := TStringList.Create;
  FTools.OwnsObjects := True;
  FTools.CaseSensitive := False;

  FResources := TStringList.Create;
  FResources.OwnsObjects := True;
  FResources.CaseSensitive := False;
end;

destructor TAiMCPLogicServer.Destroy;
begin
  FTools.Free;
  FResources.Free;
  inherited;
end;

procedure TAiMCPLogicServer.RegisterTool(const AName: string; ATool: IAiMCPTool);
var
  Idx: Integer;
begin
  // Si ya existe, lo reemplaza
  Idx := FTools.IndexOf(AName);
  if Idx >= 0 then
    FTools.Delete(Idx);
  FTools.AddObject(AName, TAiMCPToolHolder.Create(ATool));
end;

procedure TAiMCPLogicServer.RegisterResource(const AURI: string;
    AResource: IAiMCPResource);
var
  Idx: Integer;
begin
  Idx := FResources.IndexOf(AURI);
  if Idx >= 0 then
    FResources.Delete(Idx);
  FResources.AddObject(AURI, TAiMCPResourceHolder.Create(AResource));
end;

procedure TAiMCPLogicServer.Start;
begin
  FIsActive := True;
end;

procedure TAiMCPLogicServer.Stop;
begin
  FIsActive := False;
end;

function TAiMCPLogicServer.ParseJSONRequest(const ARequestBody: string): TJSONObject;
var
  J: TJSONData;
begin
  Result := nil;
  if Trim(ARequestBody) = '' then
    Exit;
  try
    J := GetJSON(ARequestBody);
    if J is TJSONObject then
      Result := TJSONObject(J)
    else
      J.Free;
  except
    // Parseado fallido — el llamador maneja nil
  end;
end;

function TAiMCPLogicServer.ExtractIDStr(ARequest: TJSONObject): string;
var
  Node: TJSONData;
begin
  Node := ARequest.Find('id');
  if Assigned(Node) then
    Result := Node.AsJSON
  else
    Result := 'null';
end;

function TAiMCPLogicServer.BuildResponseFrame(const AIDStr: string): TJSONObject;
var
  IDVal: TJSONData;
begin
  Result := TJSONObject.Create;
  Result.Add('jsonrpc', TJSONString.Create('2.0'));
  try
    IDVal := GetJSON(AIDStr);
    Result.Add('id', IDVal);
  except
    Result.Add('id', TJSONNull.Create);
  end;
end;

function TAiMCPLogicServer.CreateErrorResponse(const AIDStr: string;
    ACode: Integer; const AMessage: string): string;
var
  Resp, ErrObj: TJSONObject;
begin
  Resp   := BuildResponseFrame(AIDStr);
  ErrObj := TJSONObject.Create;
  ErrObj.Add('code', TJSONIntegerNumber.Create(ACode));
  ErrObj.Add('message', TJSONString.Create(AMessage));
  Resp.Add('error', ErrObj);
  try
    Result := Resp.AsJSON;
  finally
    Resp.Free;
  end;
end;

// ---- Handlers de metodos MCP ------------------------------------------------

function TAiMCPLogicServer.HandleInitialize(AParams: TJSONObject;
    const AIDStr: string): string;
var
  Resp, ResultObj, CapsObj, ToolsCaps, ResCaps, SrvInfo: TJSONObject;
begin
  ResultObj := TJSONObject.Create;

  ResultObj.Add('protocolVersion', TJSONString.Create(FProtocolVersion));

  CapsObj   := TJSONObject.Create;
  ToolsCaps := TJSONObject.Create;
  ToolsCaps.Add('listChanged', TJSONBoolean.Create(False));
  CapsObj.Add('tools', ToolsCaps);

  ResCaps := TJSONObject.Create;
  ResCaps.Add('listChanged', TJSONBoolean.Create(False));
  CapsObj.Add('resources', ResCaps);

  ResultObj.Add('capabilities', CapsObj);

  SrvInfo := TJSONObject.Create;
  SrvInfo.Add('name', TJSONString.Create(FServerName));
  SrvInfo.Add('version', TJSONString.Create(FServerVersion));
  ResultObj.Add('serverInfo', SrvInfo);

  Resp := BuildResponseFrame(AIDStr);
  Resp.Add('result', ResultObj);
  try
    Result := Resp.AsJSON;
  finally
    Resp.Free;
  end;
end;

function TAiMCPLogicServer.HandlePing(const AIDStr: string): string;
var
  Resp, ResultObj: TJSONObject;
begin
  Resp      := BuildResponseFrame(AIDStr);
  ResultObj := TJSONObject.Create;
  Resp.Add('result', ResultObj);
  try
    Result := Resp.AsJSON;
  finally
    Resp.Free;
  end;
end;

function TAiMCPLogicServer.HandleToolsList(const AIDStr: string): string;
var
  Resp, ResultObj, ToolObj: TJSONObject;
  ToolsArr: TJSONArray;
  Schema: TJSONObject;
  Holder: TAiMCPToolHolder;
  I: Integer;
begin
  ToolsArr  := TJSONArray.Create;
  ResultObj := TJSONObject.Create;
  ResultObj.Add('tools', ToolsArr);

  for I := 0 to FTools.Count - 1 do
  begin
    Holder := TAiMCPToolHolder(FTools.Objects[I]);
    if not Assigned(Holder) or not Assigned(Holder.Tool) then
      Continue;

    ToolObj := TJSONObject.Create;
    ToolObj.Add('name', TJSONString.Create(Holder.Tool.GetName));
    ToolObj.Add('description', TJSONString.Create(Holder.Tool.GetDescription));

    Schema := Holder.Tool.GetInputSchema;
    if Assigned(Schema) then
      ToolObj.Add('inputSchema', Schema)
    else
    begin
      Schema := TJSONObject.Create;
      Schema.Add('type', TJSONString.Create('object'));
      ToolObj.Add('inputSchema', Schema);
    end;

    ToolsArr.Add(ToolObj);
  end;

  Resp := BuildResponseFrame(AIDStr);
  Resp.Add('result', ResultObj);
  try
    Result := Resp.AsJSON;
  finally
    Resp.Free;
  end;
end;

function TAiMCPLogicServer.HandleToolsCall(AParams: TJSONObject;
    const AIDStr: string; const AAuthCtx: TAiAuthContext): string;
var
  Resp, ToolResult, ArgsObj: TJSONObject;
  ArgsNode: TJSONData;
  ToolName: string;
  Idx: Integer;
  Holder: TAiMCPToolHolder;
begin
  if not Assigned(AParams) then
    Exit(CreateErrorResponse(AIDStr, JSONRPC_INVALID_PARAMS,
        'Parametros requeridos'));

  ToolName := AParams.Get('name', '');
  if ToolName = '' then
    Exit(CreateErrorResponse(AIDStr, JSONRPC_INVALID_PARAMS,
        'Falta parametro "name"'));

  Idx := FTools.IndexOf(ToolName);
  if Idx < 0 then
    Exit(CreateErrorResponse(AIDStr, JSONRPC_METHOD_NOT_FOUND,
        'Herramienta no encontrada: ' + ToolName));

  Holder := TAiMCPToolHolder(FTools.Objects[Idx]);
  if not Assigned(Holder) or not Assigned(Holder.Tool) then
    Exit(CreateErrorResponse(AIDStr, JSONRPC_INTERNAL_ERROR,
        'Herramienta invalida: ' + ToolName));

  // Extraer argumentos
  ArgsNode := AParams.Find('arguments');
  if Assigned(ArgsNode) and (ArgsNode is TJSONObject) then
    ArgsObj := TJSONObject(ArgsNode)
  else
    ArgsObj := nil;

  try
    ToolResult := Holder.Tool.Execute(ArgsObj, AAuthCtx);
  except
    on E: Exception do
      Exit(CreateErrorResponse(AIDStr, JSONRPC_INTERNAL_ERROR,
          'Excepcion en herramienta "' + ToolName + '": ' + E.Message));
  end;

  if not Assigned(ToolResult) then
    ToolResult := TAiMCPResponseBuilder.New.AddText('').Build;

  Resp := BuildResponseFrame(AIDStr);
  Resp.Add('result', ToolResult);
  try
    Result := Resp.AsJSON;
  finally
    Resp.Free;
  end;
end;

function TAiMCPLogicServer.HandleResourcesList(const AIDStr: string): string;
var
  Resp, ResultObj, ResObj: TJSONObject;
  ResArr: TJSONArray;
  Holder: TAiMCPResourceHolder;
  I: Integer;
begin
  ResArr    := TJSONArray.Create;
  ResultObj := TJSONObject.Create;
  ResultObj.Add('resources', ResArr);

  for I := 0 to FResources.Count - 1 do
  begin
    Holder := TAiMCPResourceHolder(FResources.Objects[I]);
    if not Assigned(Holder) or not Assigned(Holder.Resource) then
      Continue;

    ResObj := TJSONObject.Create;
    ResObj.Add('uri', TJSONString.Create(Holder.Resource.GetURI));
    ResObj.Add('name', TJSONString.Create(Holder.Resource.GetName));
    ResObj.Add('description', TJSONString.Create(Holder.Resource.GetDescription));
    ResObj.Add('mimeType', TJSONString.Create(Holder.Resource.GetMimeType));
    ResArr.Add(ResObj);
  end;

  Resp := BuildResponseFrame(AIDStr);
  Resp.Add('result', ResultObj);
  try
    Result := Resp.AsJSON;
  finally
    Resp.Free;
  end;
end;

function TAiMCPLogicServer.HandleResourcesRead(AParams: TJSONObject;
    const AIDStr: string): string;
var
  Resp, ResultObj, ContentObj: TJSONObject;
  ContentArr: TJSONArray;
  URI: string;
  Idx: Integer;
  Holder: TAiMCPResourceHolder;
  Data: string;
begin
  if not Assigned(AParams) then
    Exit(CreateErrorResponse(AIDStr, JSONRPC_INVALID_PARAMS,
        'Parametros requeridos'));

  URI := AParams.Get('uri', '');
  if URI = '' then
    Exit(CreateErrorResponse(AIDStr, JSONRPC_INVALID_PARAMS,
        'Falta parametro "uri"'));

  Idx := FResources.IndexOf(URI);
  if Idx < 0 then
    Exit(CreateErrorResponse(AIDStr, JSONRPC_METHOD_NOT_FOUND,
        'Recurso no encontrado: ' + URI));

  Holder := TAiMCPResourceHolder(FResources.Objects[Idx]);
  if not Assigned(Holder) then
    Exit(CreateErrorResponse(AIDStr, JSONRPC_INTERNAL_ERROR,
        'Recurso invalido: ' + URI));

  try
    Data := Holder.Resource.Read;
  except
    on E: Exception do
      Exit(CreateErrorResponse(AIDStr, JSONRPC_INTERNAL_ERROR,
          'Error leyendo recurso: ' + E.Message));
  end;

  ContentObj := TJSONObject.Create;
  ContentObj.Add('uri', TJSONString.Create(URI));
  ContentObj.Add('mimeType', TJSONString.Create(Holder.Resource.GetMimeType));
  ContentObj.Add('text', TJSONString.Create(Data));

  ContentArr := TJSONArray.Create;
  ContentArr.Add(ContentObj);

  ResultObj := TJSONObject.Create;
  ResultObj.Add('contents', ContentArr);

  Resp := BuildResponseFrame(AIDStr);
  Resp.Add('result', ResultObj);
  try
    Result := Resp.AsJSON;
  finally
    Resp.Free;
  end;
end;

// ---- ExecuteRequest ---------------------------------------------------------

function TAiMCPLogicServer.ExecuteRequest(const ARequestJson,
    ASessionID: string): string;
var
  EmptyCtx: TAiAuthContext;
begin
  EmptyCtx.IsAuthenticated := True;
  EmptyCtx.UserID   := '';
  EmptyCtx.UserName := '';
  EmptyCtx.Roles    := '';
  Result := ExecuteRequest(ARequestJson, ASessionID, EmptyCtx);
end;

function TAiMCPLogicServer.ExecuteRequest(const ARequestJson,
    ASessionID: string; const AAuthCtx: TAiAuthContext): string;
var
  ReqObj  : TJSONObject;
  Method  : string;
  IDStr   : string;
  ParamsNode: TJSONData;
  ParamsObj : TJSONObject;
begin
  Result := '';

  ReqObj := ParseJSONRequest(ARequestJson);
  if not Assigned(ReqObj) then
    Exit(CreateErrorResponse('null', JSONRPC_PARSE_ERROR, 'Parse error'));

  try
    IDStr  := ExtractIDStr(ReqObj);
    Method := ReqObj.Get('method', '');

    if Method = '' then
      Exit(CreateErrorResponse(IDStr, JSONRPC_INVALID_REQUEST,
          'Falta campo "method"'));

    // Extraer params (puede ser null/ausente)
    ParamsNode := ReqObj.Find('params');
    if Assigned(ParamsNode) and (ParamsNode is TJSONObject) then
      ParamsObj := TJSONObject(ParamsNode)
    else
      ParamsObj := nil;

    // Notificaciones (sin id) → sin respuesta
    if ReqObj.Find('id') = nil then
    begin
      // Procesar notificacion pero no responder
      Result := '';
      Exit;
    end;

    // Despacho de metodos
    if (Method = 'initialize') then
      Result := HandleInitialize(ParamsObj, IDStr)

    else if (Method = 'ping') then
      Result := HandlePing(IDStr)

    else if (Method = 'tools/list') then
      Result := HandleToolsList(IDStr)

    else if (Method = 'tools/call') then
      Result := HandleToolsCall(ParamsObj, IDStr, AAuthCtx)

    else if (Method = 'resources/list') then
      Result := HandleResourcesList(IDStr)

    else if (Method = 'resources/read') then
      Result := HandleResourcesRead(ParamsObj, IDStr)

    else
      Result := CreateErrorResponse(IDStr, JSONRPC_METHOD_NOT_FOUND,
          'Metodo no encontrado: ' + Method);

  finally
    ReqObj.Free;
  end;
end;

// ---------------------------------------------------------------------------
// TAiMCPServer
// ---------------------------------------------------------------------------

constructor TAiMCPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive             := False;
  FCorsEnabled        := False;
  FEndPoint           := '/mcp';
  FCorsAllowedOrigins := '*';
  FServerName         := 'MakerAI MCP Server';
  FAiFunctions        := nil;
  FApiKey             := '';

  FLogicServer := TAiMCPLogicServer.Create;
end;

destructor TAiMCPServer.Destroy;
begin
  if FActive then
    Stop;
  FLogicServer.Free;
  inherited;
end;

procedure TAiMCPServer.Notification(AComponent: TComponent;
    AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FAiFunctions) then
    FAiFunctions := nil;
end;

function TAiMCPServer.GetEndpoint: string;
begin
  Result := FEndPoint;
end;

function TAiMCPServer.GetPort: Integer;
begin
  Result := FLogicServer.Port;
end;

procedure TAiMCPServer.SetPort(const AValue: Integer);
begin
  FLogicServer.Port := AValue;
end;

function TAiMCPServer.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TAiMCPServer.SetServerName(const AValue: string);
begin
  FServerName := AValue;
  FLogicServer.ServerName := AValue;
end;

procedure TAiMCPServer.SetAiFunctions(const AValue: TAiFunctions);
begin
  FAiFunctions := AValue;
end;

procedure TAiMCPServer.SetActive(const AValue: Boolean);
begin
  FActive := AValue;
end;

procedure TAiMCPServer.InternalRegisterFromAiFunctions;
begin
  // TODO: cuando se implemente el Bridge completo en FPC,
  // iterar FAiFunctions.Functions y registrar cada item como TAiFunctionToolProxy
end;

function TAiMCPServer.ValidateRequest(const AAuthHeader, ARemoteIP: string;
    out AAuthCtx: TAiAuthContext): Boolean;
var
  Token, ResolvedKey: string;
  IsValid: Boolean;
begin
  // Valores por defecto
  AAuthCtx.IsAuthenticated := False;
  AAuthCtx.UserID          := '';
  AAuthCtx.UserName        := '';
  AAuthCtx.Roles           := '';

  // Sin ApiKey configurada → autenticacion libre
  if FApiKey = '' then
  begin
    AAuthCtx.IsAuthenticated := True;
    Result := True;

    // Disparar evento custom de todos modos (para logging, etc.)
    if Assigned(FOnValidateRequest) then
    begin
      FOnValidateRequest(Self, AAuthHeader, ARemoteIP, AAuthCtx, IsValid);
      Result := IsValid;
    end;
    Exit;
  end;

  // Extraer token del header Authorization: Bearer <token> o X-API-Key: <token>
  Token := AAuthHeader;
  if AnsiStartsText('Bearer ', Token) then
    Token := Copy(Token, 8, MaxInt)
  else if AnsiStartsText('bearer ', Token) then
    Token := Copy(Token, 8, MaxInt);
  Token := Trim(Token);

  // Resolver @ENV_VAR si corresponde
  ResolvedKey := FApiKey;
  if (Length(ResolvedKey) > 1) and (ResolvedKey[1] = '@') then
    ResolvedKey := GetEnvironmentVariable(Copy(ResolvedKey, 2, MaxInt));

  if Token = ResolvedKey then
  begin
    AAuthCtx.IsAuthenticated := True;
    AAuthCtx.UserID          := 'api_user';
    Result := True;
  end
  else
  begin
    Result := False;
  end;

  // Evento custom (Layer 2) — puede sobreescribir el resultado
  if Assigned(FOnValidateRequest) then
  begin
    FOnValidateRequest(Self, AAuthHeader, ARemoteIP, AAuthCtx, IsValid);
    Result := IsValid;
    AAuthCtx.IsAuthenticated := IsValid;
  end;
end;

procedure TAiMCPServer.RegisterTool(const AName: string; ATool: IAiMCPTool);
begin
  FLogicServer.RegisterTool(AName, ATool);
end;

procedure TAiMCPServer.RegisterResource(const AURI: string;
    AResource: IAiMCPResource);
begin
  FLogicServer.RegisterResource(AURI, AResource);
end;

procedure TAiMCPServer.Start;
begin
  FLogicServer.ServerName      := FServerName;
  FLogicServer.CorsEnabled     := FCorsEnabled;
  FLogicServer.CorsAllowedOrigins := FCorsAllowedOrigins;
  FLogicServer.Start;
  FActive := True;

  // Registrar funciones de TAiFunctions si estan asignadas
  if Assigned(FAiFunctions) then
    InternalRegisterFromAiFunctions;
end;

procedure TAiMCPServer.Stop;
begin
  FLogicServer.Stop;
  FActive := False;
end;

end.
