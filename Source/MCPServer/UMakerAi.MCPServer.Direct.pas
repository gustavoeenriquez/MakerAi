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
// Conexion directa in-process al servidor MCP (sin red, sin pipes).
// Ideal para pruebas unitarias y para exponer tools al mismo proceso host.
//
// Adaptaciones respecto a la version Delphi:
//   - TJSONObject.ParseJSONValue → GetJSON (de jsonparser)
//   - TJSONObject.TryGetValue    → Find + cast
//   - AddPair                    → Add
//   - IntToStr para ID (FPC no tiene Integer.ToString)

unit uMakerAi.MCPServer.Direct;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  uMakerAi.MCPServer.Core;

type
  // -------------------------------------------------------------------------
  // TAiMCPDirectConnection - conexion en proceso al LogicServer
  //
  // No requiere red ni procesos externos.
  // Perfecto para demos, tests y aplicaciones monoliticas.
  // -------------------------------------------------------------------------
  TAiMCPDirectConnection = class(TAiMCPServer)
  private
    FRequestIDCounter: Integer;

    // Construye y ejecuta un JSON-RPC request en el LogicServer
    // Retorna el 'result' parseado (el llamador es responsable de liberar).
    // Lanza excepcion si la respuesta contiene 'error'.
    function ExecuteDirectRequest(const AMethod: string;
        AParams: TJSONObject): TJSONObject;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override;
    procedure Stop;  override;

    // ----- API de alto nivel -----

    // Lista herramientas: retorna {"tools":[...]}
    // El llamador es responsable de liberar el TJSONObject retornado.
    function ListTools: TJSONObject;

    // Lista recursos: retorna {"resources":[...]}
    function ListResources: TJSONObject;

    // Lee un recurso por URI: retorna {"contents":[...]}
    function ReadResource(const AURI: string): TJSONObject;

    // Llama una herramienta con argumentos como TJSONObject.
    // AArguments: puede ser nil (sin argumentos).
    // NOTA: AArguments es consumido (no liberar despues de llamar).
    function CallTool(const AToolName: string;
        AArguments: TJSONObject): TJSONObject; overload;

    // Sobrecarga conveniente: argumentos como TStrings ("key=value")
    function CallTool(const AToolName: string;
        AArguments: TStrings): TJSONObject; overload;

  published
    property Port;
    property ServerName;
    property ApiKey;
    property OnValidateRequest;
  end;

implementation

// ---------------------------------------------------------------------------
// TAiMCPDirectConnection
// ---------------------------------------------------------------------------

constructor TAiMCPDirectConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRequestIDCounter := 0;
end;

destructor TAiMCPDirectConnection.Destroy;
begin
  inherited;
end;

procedure TAiMCPDirectConnection.Start;
begin
  inherited Start;
end;

procedure TAiMCPDirectConnection.Stop;
begin
  inherited Stop;
end;

function TAiMCPDirectConnection.ExecuteDirectRequest(const AMethod: string;
    AParams: TJSONObject): TJSONObject;
var
  RequestObj    : TJSONObject;
  RequestStr    : string;
  ResponseStr   : string;
  ResponseJSON  : TJSONData;
  RespObj       : TJSONObject;
  ErrorNode     : TJSONData;
  ResultNode    : TJSONData;
  ErrMsg        : string;
  ErrCode       : Integer;
  AuthCtx       : TAiAuthContext;
begin
  Result := nil;

  if not IsActive then
    raise Exception.Create(
        'DirectConnection nao esta activa. Llamar Start primero.');

  // Construir JSON-RPC request
  Inc(FRequestIDCounter);

  RequestObj := TJSONObject.Create;
  try
    RequestObj.Add('jsonrpc', TJSONString.Create('2.0'));
    RequestObj.Add('id', TJSONIntegerNumber.Create(FRequestIDCounter));
    RequestObj.Add('method', TJSONString.Create(AMethod));

    if Assigned(AParams) then
      RequestObj.Add('params', AParams)
    else
      RequestObj.Add('params', TJSONObject.Create);

    RequestStr := RequestObj.AsJSON;
  finally
    RequestObj.Free;
    // NOTA: AParams fue transferido a RequestObj y liberado con el.
    // No acceder a AParams despues de Free del RequestObj.
  end;

  // Contexto de autenticacion (direct = confianza total)
  AuthCtx.IsAuthenticated := True;
  AuthCtx.UserID          := 'direct';
  AuthCtx.UserName        := 'direct_connection';
  AuthCtx.Roles           := 'admin';

  // Ejecutar en el LogicServer
  ResponseStr := FLogicServer.ExecuteRequest(RequestStr, 'direct', AuthCtx);

  if ResponseStr = '' then
    Exit; // Notificacion sin respuesta

  // Parsear respuesta
  ResponseJSON := GetJSON(ResponseStr);
  if not (ResponseJSON is TJSONObject) then
  begin
    ResponseJSON.Free;
    raise Exception.Create('Respuesta JSON-RPC invalida del LogicServer.');
  end;

  RespObj := TJSONObject(ResponseJSON);
  try
    // Verificar campo 'error'
    ErrorNode := RespObj.Find('error');
    if Assigned(ErrorNode) and (ErrorNode is TJSONObject) then
    begin
      ErrMsg  := TJSONObject(ErrorNode).Get('message', 'Error desconocido');
      ErrCode := TJSONObject(ErrorNode).Get('code', 0);
      raise Exception.CreateFmt('JSON-RPC Error %d: %s', [ErrCode, ErrMsg]);
    end;

    // Extraer 'result'
    ResultNode := RespObj.Find('result');
    if Assigned(ResultNode) then
    begin
      if ResultNode is TJSONObject then
        Result := TJSONObject(ResultNode.Clone)
      else
        raise Exception.Create(
            'Tipo de resultado inesperado en respuesta JSON-RPC.');
    end;

  finally
    RespObj.Free;
  end;
end;

function TAiMCPDirectConnection.ListTools: TJSONObject;
begin
  Result := ExecuteDirectRequest('tools/list', nil);
end;

function TAiMCPDirectConnection.ListResources: TJSONObject;
begin
  Result := ExecuteDirectRequest('resources/list', nil);
end;

function TAiMCPDirectConnection.ReadResource(const AURI: string): TJSONObject;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('uri', TJSONString.Create(AURI));
  Result := ExecuteDirectRequest('resources/read', Params);
end;

function TAiMCPDirectConnection.CallTool(const AToolName: string;
    AArguments: TJSONObject): TJSONObject;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('name', TJSONString.Create(AToolName));

  if Assigned(AArguments) then
    Params.Add('arguments', AArguments)
  else
    Params.Add('arguments', TJSONObject.Create);

  Result := ExecuteDirectRequest('tools/call', Params);
end;

function TAiMCPDirectConnection.CallTool(const AToolName: string;
    AArguments: TStrings): TJSONObject;
var
  ArgsObj: TJSONObject;
  I: Integer;
begin
  ArgsObj := TJSONObject.Create;
  if Assigned(AArguments) then
  begin
    for I := 0 to AArguments.Count - 1 do
      ArgsObj.Add(AArguments.Names[I],
          TJSONString.Create(AArguments.ValueFromIndex[I]));
  end;
  Result := CallTool(AToolName, ArgsObj);
end;

end.
