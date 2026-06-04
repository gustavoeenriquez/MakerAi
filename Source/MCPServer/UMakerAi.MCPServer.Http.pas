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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit UMakerAi.MCPServer.Http;

interface

uses
  System.SysUtils, System.Classes,
  IdContext, IdCustomHTTPServer, IdHTTPServer,
  UMakerAi.MCPServer.Core;

type

  TAiMCPHttpServer = class(TAiMCPServer)
  private
    FHttpServer: TIdHTTPServer;

    procedure HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleOptionsRequest(AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetRequest(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandlePostRequest(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo; const AAuthContext: TAiAuthContext);
    function VerifyAndSetCORSHeaders(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; Override;
    procedure Stop; Override;

  Published
    property Port;
    property Endpoint;
    property CorsEnabled;
    property CorsAllowedOrigins;
    property ApiKey;
    property OnValidateRequest;
  end;

procedure Register;

implementation

uses System.StrUtils, IdGlobal, System.JSON;

const
  HTTP_OK = 200;
  HTTP_NO_CONTENT = 204;
  HTTP_NOT_FOUND = 404; // <-- CAMBIO: A?adida constante para claridad
  HTTP_FORBIDDEN = 403;
  HTTP_METHOD_NOT_ALLOWED = 405;
  HTTP_INTERNAL_SERVER_ERROR = 500;
  CORS_MAX_AGE_SECONDS = 86400;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMCPHttpServer]);
end;

{ TAiMCPHttpServer }

constructor TAiMCPHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHttpServer := TIdHTTPServer.Create(Self);
  FHttpServer.OnCommandGet := HttpCommand;
  FHttpServer.OnCommandOther := HttpCommand;
end;

destructor TAiMCPHttpServer.Destroy;
begin
  FHttpServer.Free;
  inherited Destroy;
end;

procedure TAiMCPHttpServer.Start;
begin
  inherited Start;
  FHttpServer.DefaultPort := FLogicServer.Port;
  FHttpServer.Active := True;
end;

procedure TAiMCPHttpServer.Stop;
begin
  if FHttpServer.Active then
    FHttpServer.Active := False;
  inherited Stop;
end;

function TAiMCPHttpServer.VerifyAndSetCORSHeaders(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;
var
  Origin: string;
  AllowedOriginsList: TStringList;
  AllowedOrigin: string;
  IsAllowed: Boolean;
begin
  Result := True;
  if not FLogicServer.CorsEnabled then
    Exit;
  Origin := ARequestInfo.RawHeaders.Values['Origin'];
  AllowedOrigin := '*';
  IsAllowed := False;
  if (Origin = '') or (FLogicServer.CorsAllowedOrigins = '*') then
  begin
    IsAllowed := True;
  end
  else
  begin
    AllowedOriginsList := TStringList.Create;
    try
      AllowedOriginsList.DelimitedText := StringReplace(FLogicServer.CorsAllowedOrigins, ';', ',', [rfReplaceAll]);
      if AllowedOriginsList.IndexOf(Origin) > -1 then
      begin
        AllowedOrigin := Origin;
        IsAllowed := True;
      end;
    finally
      AllowedOriginsList.Free;
    end;
  end;

  if not IsAllowed then
  begin
    AResponseInfo.ResponseNo := HTTP_FORBIDDEN;
    AResponseInfo.ResponseText := 'CORS: Origin Not Allowed';
    Result := False;
    Exit;
  end;

  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Origin'] := AllowedOrigin;
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'POST, GET, OPTIONS';
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'Content-Type, X-Session-ID';
  AResponseInfo.CustomHeaders.Values['Access-Control-Max-Age'] := IntToStr(CORS_MAX_AGE_SECONDS);
end;

procedure TAiMCPHttpServer.HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  AuthContext: TAiAuthContext;
  AuthHeader: string;
begin
  // --- PASO 1: Verificar y establecer cabeceras CORS ---
  if not VerifyAndSetCORSHeaders(ARequestInfo, AResponseInfo) then
    Exit;

  // --- PASO 2: Autenticaci�n (Layer 1: API Key + Layer 2: Evento custom) ---
  // Extraer header de autenticaci�n: primero Authorization, luego X-API-Key
  AuthHeader := ARequestInfo.RawHeaders.Values['Authorization'];
  if AuthHeader = '' then
    AuthHeader := ARequestInfo.RawHeaders.Values['X-API-Key'];

  if not ValidateRequest(AuthHeader, AContext.Binding.PeerIP, AuthContext) then
  begin
    AResponseInfo.ResponseNo := 401;
    AResponseInfo.ResponseText := 'Unauthorized';
    AResponseInfo.ContentText := '{"jsonrpc": "2.0", "error": {"code": -32001, "message": "Authentication failed"}, "id": null}';
    AResponseInfo.ContentType := 'application/json';
    Exit;
  end;

  // --- PASO 3: Despachar la petici�n al manejador correcto seg�n el m�todo HTTP ---
  if not SameText(ARequestInfo.URI, Endpoint) then
  begin
    // Si la URL no es nuestro endpoint /mcp, devolvemos un 404 Not Found.
    AResponseInfo.ResponseNo := HTTP_NOT_FOUND;
    AResponseInfo.ResponseText := 'Not Found';
    AResponseInfo.ContentText := 'Endpoint not found. Please use ' + Endpoint;
  end
  else if SameText(ARequestInfo.Command, 'OPTIONS') then
  begin
    // El navegador env?a una petici?n OPTIONS (preflight) para verificar CORS.
    HandleOptionsRequest(AResponseInfo);
  end
  else if SameText(ARequestInfo.Command, 'GET') then
  begin
    // Una petici?n GET a /mcp devuelve informaci?n del servidor.
    HandleGetRequest(ARequestInfo, AResponseInfo);
  end
  else if SameText(ARequestInfo.Command, 'POST') then
  begin
    // Una petici�n POST a /mcp contiene una llamada de procedimiento JSON-RPC.
    HandlePostRequest(ARequestInfo, AResponseInfo, AuthContext);
  end
  else
  begin
    // Cualquier otro m?todo (PUT, DELETE, etc.) no est? permitido.
    AResponseInfo.ResponseNo := HTTP_METHOD_NOT_ALLOWED;
    AResponseInfo.ResponseText := 'Method Not Allowed';
    AResponseInfo.ContentText := 'Only GET, POST, and OPTIONS are supported.';
  end;
end;

procedure TAiMCPHttpServer.HandleOptionsRequest(AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.ResponseNo := HTTP_NO_CONTENT;
  AResponseInfo.ResponseText := 'No Content';
  // Indy > 10.6 puede requerir esto para evitar el HTML por defecto
  AResponseInfo.ContentLength := 0;
end;

procedure TAiMCPHttpServer.HandleGetRequest(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  InfoObj: TJSONObject;
begin
  // Respondemos con informaci?n b?sica del servidor en formato JSON
  InfoObj := TJSONObject.Create;
  try
    InfoObj.AddPair('serverName', TJSONString.Create(FLogicServer.ServerName));
    InfoObj.AddPair('protocolVersion', TJSONString.Create(FLogicServer.ProtocolVersion));
    InfoObj.AddPair('status', TJSONString.Create('active'));

    AResponseInfo.ResponseNo := HTTP_OK;
    AResponseInfo.ResponseText := 'OK';
    AResponseInfo.ContentType := 'application/json; charset=utf-8';
    AResponseInfo.ContentText := InfoObj.ToJSON; // <- Aqu? asignamos el JSON
  finally
    InfoObj.Free;
  end;
end;

procedure TAiMCPHttpServer.HandlePostRequest(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo; const AAuthContext: TAiAuthContext);
var
  RequestBody, ResponseBody, SessionID: string;
begin
  try
    RequestBody := ReadStringFromStream(ARequestInfo.PostStream, -1, IndyTextEncoding_UTF8);
    SessionID := ARequestInfo.RawHeaders.Values['X-Session-ID'];

    ResponseBody := FLogicServer.ExecuteRequest(RequestBody, SessionID, AAuthContext);

    AResponseInfo.ContentType := 'application/json; charset=utf-8';
    AResponseInfo.CharSet := 'utf-8';

    if ResponseBody = '' then
    begin
      AResponseInfo.ResponseNo := HTTP_NO_CONTENT;
      AResponseInfo.ResponseText := 'No Content';
      AResponseInfo.ContentLength := 0;
    end
    else
    begin
      AResponseInfo.ResponseNo := HTTP_OK;
      AResponseInfo.ResponseText := 'OK';
      AResponseInfo.ContentText := ResponseBody; // <- Asignamos el JSON
    end;

  except
    on E: Exception do
    begin
      AResponseInfo.ResponseNo := HTTP_INTERNAL_SERVER_ERROR;
      AResponseInfo.ResponseText := 'Internal Server Error';
      AResponseInfo.ContentText := '{"jsonrpc": "2.0", "error": {"code": -32000, "message": "Server error during POST request processing"}, "id": null}';
      AResponseInfo.ContentType := 'application/json';
    end;
  end;
end;

end.
