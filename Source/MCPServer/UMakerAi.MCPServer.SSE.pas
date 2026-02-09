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

unit uMakerAi.MCPServer.SSE;

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, StrUtils, Generics.Collections, Types, Variants, SyncObjs, Math,
  {$ELSE}
  System.SysUtils, System.StrUtils, System.Classes, System.JSON, System.Generics.Collections, System.SyncObjs,
  {$ENDIF}
  IdContext, IdCustomHTTPServer, IdHTTPServer, IdGlobal, IdSocketHandle, IdException,
  uMakerAi.MCPServer.Core,
  uJsonHelper, uHttpHelper, uSysUtilsHelper, uBase64Helper, uThreadingHelper;

// ... (skipping type definitions) ...



type
  // Representa una sesión SSE activa
  TMCPSSESession = class
  public
    SessionID: string;
    // Cola de mensajes. Usamos String porque el JSON ya es texto.
    Outbox: TThreadedQueue<string>;
    LastActivity: TDateTime;
    constructor Create(const AID: string);
    destructor Destroy; override;
  end;

  TAiMCPSSEHttpServer = class(TAiMCPServer)
  private
    FHttpServer: TIdHTTPServer;
    FSessions: TObjectDictionary<string, TMCPSSESession>;
    FSessionsLock: TCriticalSection;

    // Configuración de endpoints
    FSseEndpoint: string; // por defecto '/sse'
    FMessagesEndpoint: string; // por defecto '/messages'

    // Eventos de Indy
    procedure OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure OnCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    // Manejadores específicos
    procedure HandleSSEConnection(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandlePostMessage(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    // Helpers
    function GetOrCreateSession(const AID: string): TMCPSSESession;
    function GenerateSessionID: string;
    procedure VerifyAndSetCORSHeaders(AResponseInfo: TIdHTTPResponseInfo);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override;
    procedure Stop; override;

    // Propiedades de configuración
  published
    property SseEndpoint: string read FSseEndpoint write FSseEndpoint;
    property MessagesEndpoint: string read FMessagesEndpoint write FMessagesEndpoint;
  end;

procedure Register;

implementation

uses
  {$IFDEF FPC}
  DateUtils;
  {$ELSE}
  System.DateUtils;
  {$ENDIF}

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMCPSSEHttpServer]);
end;

{ TMCPSSESession }

constructor TMCPSSESession.Create(const AID: string);
begin
  inherited Create;
  SessionID := AID;
  // Capacidad: 1000 mensajes. 
  // PushTimeout=INFINITE (o 1000), PopTimeout=INFINITE.
  // Ajusta según necesidad.
  Outbox := TThreadedQueue<string>.Create(1000, INFINITE, 1000); 
  LastActivity := Now;
end;

destructor TMCPSSESession.Destroy;
begin
  Outbox.Free;
  inherited;
end;

{ TAiMCPSSEHttpServer }

constructor TAiMCPSSEHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSseEndpoint := '/sse';
  FMessagesEndpoint := '/messages';

  FSessions := TObjectDictionary<string, TMCPSSESession>.Create([doOwnsValues]);
  FSessionsLock := TCriticalSection.Create;

  // No creamos FHttpServer aquí para no activarlo inmediatamente, 
  // pero es común crearlo en el constructor y dejarlo Active=False.
  FHttpServer := TIdHTTPServer.Create(Self);
  FHttpServer.OnCommandGet := OnCommandGet;
  FHttpServer.OnCommandOther := OnCommandOther;
  FHttpServer.DefaultPort := 3000; // Puerto por defecto, se sobreescribe con FPort (propiedad de ancestro)
end;

destructor TAiMCPSSEHttpServer.Destroy;
begin
  Stop; // Asegura detener server y limpiar sesiones
  FSessions.Free;
  FSessionsLock.Free;
  // FHttpServer se libera solo si tiene Owner=Self, 
  // pero ya lo asignamos a Self.
  inherited;
end;

procedure TAiMCPSSEHttpServer.Start;
begin
  inherited Start; // Llama a FLogicServer.Start e inicializa FActive

  // Iniciar Indy Server
  if FHttpServer <> nil then
  begin
    FHttpServer.DefaultPort := Self.Port; // Usar el puerto configurado en el componente
    if not FHttpServer.Active then
      FHttpServer.Active := True;
  end;
end;

procedure TAiMCPSSEHttpServer.Stop;
begin
  if FHttpServer <> nil then
    FHttpServer.Active := False;
    
  inherited Stop;
end;

function TAiMCPSSEHttpServer.GetOrCreateSession(const AID: string): TMCPSSESession;
begin
  FSessionsLock.Enter;
  try
    if not FSessions.TryGetValue(AID, Result) then
    begin
      Result := TMCPSSESession.Create(AID);
      FSessions.Add(AID, Result);
    end;
    Result.LastActivity := Now;
  finally
    FSessionsLock.Leave;
  end;
end;

function TAiMCPSSEHttpServer.GenerateSessionID: string;
begin
  // Generar un ID aleatorio simple
  Result := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '');
end;

procedure TAiMCPSSEHttpServer.VerifyAndSetCORSHeaders(AResponseInfo: TIdHTTPResponseInfo);
begin
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*';
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'GET, POST, OPTIONS';
  AResponseInfo.CustomHeaders.Values['Access-Control-Allowed-Headers'] := 'Content-Type, Authorization';
end;

// Manejo centralizado de GET
procedure TAiMCPSSEHttpServer.OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  VerifyAndSetCORSHeaders(AResponseInfo);

  // Endpoint SSE
  if StartsText(FSseEndpoint, ARequestInfo.Document) then
  begin
    HandleSSEConnection(AContext, AResponseInfo);
    Exit;
  end;

  // Endpoint Messages (GET no suele usarse para enviar mensajes en este protocolo, pero...)
  // El cliente MCP usa POST para /messages
  AResponseInfo.ResponseNo := 404;
  AResponseInfo.ContentText := 'Not Found';
end;

procedure TAiMCPSSEHttpServer.OnCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  VerifyAndSetCORSHeaders(AResponseInfo);

  if ARequestInfo.Command = 'OPTIONS' then
  begin
    AResponseInfo.ResponseNo := 200;
    Exit;
  end;

  if (ARequestInfo.Command = 'POST') and StartsText(FMessagesEndpoint, ARequestInfo.Document) then
  begin
    HandlePostMessage(AContext, ARequestInfo, AResponseInfo);
    Exit;
  end;
  
  AResponseInfo.ResponseNo := 404;
end;

procedure TAiMCPSSEHttpServer.HandleSSEConnection(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
var
  SessionID: string;
  Session: TMCPSSESession;
  Msg: string;
  LQueueStatus: TWaitResult;
begin
  // 1. Obtener Session ID (query param o generar uno nuevo)
  SessionID := GenerateSessionID;
  Session := GetOrCreateSession(SessionID);

  // Escribir headers manualmente
  AContext.Connection.IOHandler.Write('HTTP/1.1 200 OK' + #13#10);
  AContext.Connection.IOHandler.Write('Content-Type: text/event-stream' + #13#10);
  AContext.Connection.IOHandler.Write('Cache-Control: no-cache' + #13#10);
  AContext.Connection.IOHandler.Write('Connection: keep-alive' + #13#10);
  AContext.Connection.IOHandler.Write('Access-Control-Allow-Origin: *' + #13#10);
  AContext.Connection.IOHandler.Write(#13#10); // Fin headers

  // Enviar evento inicial con el endpoint para POST
  // Format: event: endpoint\ndata: /messages?session_id=...
  Msg := Format('event: endpoint'#10'data: %s?session_id=%s'#10#10, [FMessagesEndpoint, SessionID]);
  AContext.Connection.IOHandler.Write(Msg);

  try
    while AContext.Connection.Connected do
    begin
      // Esperar mensajes en la cola (bloqueante con timeout para keep-alive ping)
      LQueueStatus := Session.Outbox.PopItem(Msg);
      
      if LQueueStatus = wrSignaled then
      begin
        // Enviar mensaje MCP (JSON-RPC)
        // Format: event: message\ndata: ...json...
        AContext.Connection.IOHandler.Write('event: message'#10);
        AContext.Connection.IOHandler.Write('data: ' + Msg + #10#10);
      end;
      
      // Opcional: Enviar ping/keepalive si timeout
    end;
  except
    // Cliente desconectado
  end;
  
  // Limpieza al desconectar
  FSessionsLock.Enter;
  try
    if FSessions.ContainsKey(SessionID) then
      FSessions.Remove(SessionID);
  finally
    FSessionsLock.Leave;
  end;
  
  // Abortar procesamiento normal de Indy para evitar doble escritura de headers
  raise EIdSilentException.Create('SSE End');
end;

procedure TAiMCPSSEHttpServer.HandlePostMessage(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  SessionID: string;
  Session: TMCPSSESession;
  JsonBody: string;
  ResponseJson: string;
  Stream: TStringStream;
begin
  // 1. Validar Session ID
  SessionID := ARequestInfo.Params.Values['session_id'];
  if SessionID = '' then
  begin
    AResponseInfo.ResponseNo := 400;
    AResponseInfo.ContentText := 'Missing session_id parameter';
    Exit;
  end;

  // 2. Buscar Sesión Activa
  FSessionsLock.Enter;
  try
    if not FSessions.TryGetValue(SessionID, Session) then
      Session := nil;
  finally
    FSessionsLock.Leave;
  end;

  if Session = nil then
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'Session not found or expired';
    Exit;
  end;

  // 3. Leer Body
  if ARequestInfo.PostStream = nil then
  begin
    AResponseInfo.ResponseNo := 400;
    Exit;
  end;

  // Leer string UTF8 seguro
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.CopyFrom(ARequestInfo.PostStream, 0);
    JsonBody := Stream.DataString;
  finally
    Stream.Free;
  end;

  // 4. Ejecutar Lógica (Core)
  try
    // ExecuteRequest devuelve el JSON de respuesta
    ResponseJson := FLogicServer.ExecuteRequest(JsonBody, SessionID);

    // 5. Enviar Respuesta por el CANAL SSE (No por HTTP response)
    if ResponseJson <> '' then
      Session.Outbox.PushItem(ResponseJson);

    // 6. Responder al POST con "202 Accepted"
    // Esto le dice al cliente: "Recibí tu orden, espera la respuesta por el stream".
    AResponseInfo.ResponseNo := 202;
    AResponseInfo.ContentText := 'Accepted';
    AResponseInfo.ContentType := 'text/plain';

  except
    on E: Exception do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentText := 'Internal Error: ' + E.Message;
    end;
  end;
end;

end.

