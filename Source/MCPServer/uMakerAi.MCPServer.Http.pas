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
// Transporte HTTP para el servidor MCP usando TFPHTTPServer de fphttpserver.
// Adaptaciones respecto a la version Delphi (que usa TIdHTTPServer de Indy):
//   - TIdHTTPServer → TFPHTTPServer (fphttpserver)
//   - TIdContext     → sin equivalente directo; IP en ARequest.RemoteAddr
//   - ReadStringFromStream → ARequest.Content (string)
//   - CustomHeaders  → AResponse.SetCustomHeader
//   - El servidor corre en un thread dedicado (TAiMCPHttpServerThread)

unit uMakerAi.MCPServer.Http;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs,
  fphttpserver,
  uMakerAi.MCPServer.Core;

type
  // Forward declarations
  TAiMCPHttpServer = class;

  // -------------------------------------------------------------------------
  // TAiMCPCustomHTTPServer - TFPHTTPServer con referencia al servidor MCP
  // -------------------------------------------------------------------------
  TAiMCPCustomHTTPServer = class(TFPHTTPServer)
  private
    FOwner: TAiMCPHttpServer;
  protected
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse); override;
  end;

  // -------------------------------------------------------------------------
  // TAiMCPHttpServerThread - hilo que corre TFPHTTPServer.Active := True
  // (bloquea hasta que Active := False)
  // -------------------------------------------------------------------------
  TAiMCPHttpServerThread = class(TThread)
  private
    FServer: TAiMCPCustomHTTPServer;
  public
    constructor Create(AServer: TAiMCPCustomHTTPServer);
    procedure Execute; override;
  end;

  // -------------------------------------------------------------------------
  // TAiMCPHttpServer - servidor MCP sobre HTTP
  // -------------------------------------------------------------------------
  TAiMCPHttpServer = class(TAiMCPServer)
  private
    FHttpServer  : TAiMCPCustomHTTPServer;
    FServerThread: TAiMCPHttpServerThread;

    procedure HandleOptionsRequest(var AResponse: TFPHTTPConnectionResponse);
    procedure HandleGetRequest(var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse);
    procedure HandlePostRequest(var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse;
        const AAuthCtx: TAiAuthContext);
    procedure SetCORSHeaders(var AResponse: TFPHTTPConnectionResponse;
        const AOrigin: string);
    function  CheckCORSAndGetOrigin(var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse;
        out AAllowedOrigin: string): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override;
    procedure Stop; override;

    // Llamado por TAiMCPCustomHTTPServer.HandleRequest
    procedure ProcessHTTPRequest(var ARequest: TFPHTTPConnectionRequest;
        var AResponse: TFPHTTPConnectionResponse);

  published
    property Port;
    property Endpoint;
    property CorsEnabled;
    property CorsAllowedOrigins;
    property ApiKey;
    property ServerName;
    property OnValidateRequest;
  end;

implementation

uses
  StrUtils, fpjson;

const
  HTTP_OK                    = 200;
  HTTP_NO_CONTENT            = 204;
  HTTP_NOT_FOUND             = 404;
  HTTP_METHOD_NOT_ALLOWED    = 405;
  HTTP_FORBIDDEN             = 403;
  HTTP_UNAUTHORIZED          = 401;
  HTTP_INTERNAL_SERVER_ERROR = 500;
  CORS_MAX_AGE_SECONDS       = 86400;

// ---------------------------------------------------------------------------
// TAiMCPCustomHTTPServer
// ---------------------------------------------------------------------------

procedure TAiMCPCustomHTTPServer.HandleRequest(
    var ARequest: TFPHTTPConnectionRequest;
    var AResponse: TFPHTTPConnectionResponse);
begin
  if Assigned(FOwner) then
    FOwner.ProcessHTTPRequest(ARequest, AResponse);
end;

// ---------------------------------------------------------------------------
// TAiMCPHttpServerThread
// ---------------------------------------------------------------------------

constructor TAiMCPHttpServerThread.Create(AServer: TAiMCPCustomHTTPServer);
begin
  inherited Create(True); // suspendido
  FServer    := AServer;
  FreeOnTerminate := False;
end;

procedure TAiMCPHttpServerThread.Execute;
begin
  try
    FServer.Active := True; // bloquea hasta Active := False
  except
    // Ignorar excepcion al detener
  end;
end;

// ---------------------------------------------------------------------------
// TAiMCPHttpServer
// ---------------------------------------------------------------------------

constructor TAiMCPHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpServer        := TAiMCPCustomHTTPServer.Create(nil);
  FHttpServer.FOwner := Self;
  FServerThread      := nil;
end;

destructor TAiMCPHttpServer.Destroy;
begin
  Stop;
  FHttpServer.Free;
  inherited;
end;

procedure TAiMCPHttpServer.Start;
begin
  inherited Start; // TAiMCPServer.Start → LogicServer.Start, FActive := True

  FHttpServer.Port := Port;

  FServerThread := TAiMCPHttpServerThread.Create(FHttpServer);
  FServerThread.Start;
end;

procedure TAiMCPHttpServer.Stop;
begin
  if Assigned(FHttpServer) and FHttpServer.Active then
  begin
    FHttpServer.Active := False;
  end;

  if Assigned(FServerThread) then
  begin
    FServerThread.WaitFor;
    FreeAndNil(FServerThread);
  end;

  inherited Stop;
end;

// --- CORS -------------------------------------------------------------------

function TAiMCPHttpServer.CheckCORSAndGetOrigin(
    var ARequest: TFPHTTPConnectionRequest;
    var AResponse: TFPHTTPConnectionResponse;
    out AAllowedOrigin: string): Boolean;
var
  Origin: string;
  SL: TStringList;
  I: Integer;
begin
  Result := True;
  AAllowedOrigin := '*';

  if not CorsEnabled then
    Exit;

  Origin := ARequest.GetCustomHeader('Origin');

  if (Origin = '') or (CorsAllowedOrigins = '*') then
  begin
    if Origin = '' then
      AAllowedOrigin := '*'
    else
      AAllowedOrigin := Origin;
    Exit;
  end;

  SL := TStringList.Create;
  try
    SL.DelimitedText := StringReplace(CorsAllowedOrigins, ';', ',', [rfReplaceAll]);
    for I := 0 to SL.Count - 1 do
    begin
      if SameText(Trim(SL[I]), Origin) then
      begin
        AAllowedOrigin := Origin;
        Exit;
      end;
    end;
  finally
    SL.Free;
  end;

  // Origen no permitido
  AResponse.Code    := HTTP_FORBIDDEN;
  AResponse.SetCustomHeader('Content-Type', 'application/json');
  AResponse.Content := '{"error":"CORS origin not allowed"}';
  Result := False;
end;

procedure TAiMCPHttpServer.SetCORSHeaders(
    var AResponse: TFPHTTPConnectionResponse;
    const AOrigin: string);
begin
  AResponse.SetCustomHeader('Access-Control-Allow-Origin',
      AOrigin);
  AResponse.SetCustomHeader('Access-Control-Allow-Methods',
      'POST, GET, OPTIONS');
  AResponse.SetCustomHeader('Access-Control-Allow-Headers',
      'Content-Type, Authorization, X-API-Key, X-Session-ID');
  AResponse.SetCustomHeader('Access-Control-Max-Age',
      IntToStr(CORS_MAX_AGE_SECONDS));
end;

// --- Handlers HTTP ----------------------------------------------------------

procedure TAiMCPHttpServer.HandleOptionsRequest(
    var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.Code    := HTTP_NO_CONTENT;
  AResponse.Content := '';
end;

procedure TAiMCPHttpServer.HandleGetRequest(
    var ARequest: TFPHTTPConnectionRequest;
    var AResponse: TFPHTTPConnectionResponse);
var
  InfoObj: TJSONObject;
begin
  InfoObj := TJSONObject.Create;
  try
    InfoObj.Add('serverName', TJSONString.Create(FLogicServer.ServerName));
    InfoObj.Add('protocolVersion', TJSONString.Create(FLogicServer.ProtocolVersion));
    InfoObj.Add('status', TJSONString.Create('active'));
    InfoObj.Add('endpoint', TJSONString.Create(Endpoint));

    AResponse.Code := HTTP_OK;
    AResponse.ContentType := 'application/json; charset=utf-8';
    AResponse.Content := InfoObj.AsJSON;
  finally
    InfoObj.Free;
  end;
end;

procedure TAiMCPHttpServer.HandlePostRequest(
    var ARequest: TFPHTTPConnectionRequest;
    var AResponse: TFPHTTPConnectionResponse;
    const AAuthCtx: TAiAuthContext);
var
  RequestBody, ResponseBody, SessionID: string;
begin
  try
    // Leer body del POST — en fphttpserver, ARequest.Content es el body como string
    RequestBody := ARequest.Content;

    SessionID := ARequest.GetCustomHeader('X-Session-ID');

    ResponseBody := FLogicServer.ExecuteRequest(RequestBody, SessionID, AAuthCtx);

    AResponse.ContentType := 'application/json; charset=utf-8';

    if ResponseBody = '' then
    begin
      AResponse.Code    := HTTP_NO_CONTENT;
      AResponse.Content := '';
    end
    else
    begin
      AResponse.Code    := HTTP_OK;
      AResponse.Content := ResponseBody;
    end;

  except
    on E: Exception do
    begin
      AResponse.Code := HTTP_INTERNAL_SERVER_ERROR;
      AResponse.ContentType := 'application/json';
      AResponse.Content :=
          '{"jsonrpc":"2.0","error":{"code":-32000,"message":"Server error: '
          + StringReplace(E.Message, '"', '\"', [rfReplaceAll])
          + '"},"id":null}';
    end;
  end;
end;

// --- Punto de entrada del servidor HTTP -------------------------------------

procedure TAiMCPHttpServer.ProcessHTTPRequest(
    var ARequest: TFPHTTPConnectionRequest;
    var AResponse: TFPHTTPConnectionResponse);
var
  AuthCtx: TAiAuthContext;
  AuthHeader, AllowedOrigin: string;
begin
  // 1. CORS
  if CorsEnabled then
  begin
    if not CheckCORSAndGetOrigin(ARequest, AResponse, AllowedOrigin) then
      Exit; // 403 ya escrito
    SetCORSHeaders(AResponse, AllowedOrigin);
  end;

  // 2. Autenticacion
  AuthHeader := ARequest.GetCustomHeader('Authorization');
  if AuthHeader = '' then
    AuthHeader := ARequest.GetCustomHeader('X-API-Key');

  if not ValidateRequest(AuthHeader, ARequest.RemoteAddr, AuthCtx) then
  begin
    AResponse.Code := HTTP_UNAUTHORIZED;
    AResponse.ContentType := 'application/json';
    AResponse.Content :=
        '{"jsonrpc":"2.0","error":{"code":-32001,"message":"Authentication failed"},"id":null}';
    Exit;
  end;

  // 3. Verificar endpoint
  if not SameText(ARequest.PathInfo, Endpoint) then
  begin
    AResponse.Code    := HTTP_NOT_FOUND;
    AResponse.Content := 'Endpoint not found. Use ' + Endpoint;
    Exit;
  end;

  // 4. Despachar por metodo HTTP
  if SameText(ARequest.Method, 'OPTIONS') then
    HandleOptionsRequest(AResponse)
  else if SameText(ARequest.Method, 'GET') then
    HandleGetRequest(ARequest, AResponse)
  else if SameText(ARequest.Method, 'POST') then
    HandlePostRequest(ARequest, AResponse, AuthCtx)
  else
  begin
    AResponse.Code    := HTTP_METHOD_NOT_ALLOWED;
    AResponse.Content := 'Only GET, POST and OPTIONS are supported.';
  end;
end;

end.
