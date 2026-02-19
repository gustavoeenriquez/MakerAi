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

unit UMakerAi.MCPServer.SSE;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections, System.SyncObjs,
  IdContext, IdCustomHTTPServer, IdHTTPServer, IdGlobal, IdSocketHandle,
  UMakerAi.MCPServer.Core;

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
  System.DateUtils;

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
  // PushTimeout: INFINITE (El productor espera si está lleno)
  // PopTimeout: 1000ms (IMPORTANTE: No usar INFINITE aquí para permitir el heartbeat)
  Outbox := TThreadedQueue<string>.Create(1000, INFINITE, 1000);
  LastActivity := Now;
end;

destructor TMCPSSESession.Destroy;
begin
  Outbox.DoShutDown; // Desbloquear cualquier hilo esperando
  Outbox.Free;
  inherited;
end;

{ TAiMCPSSEHttpServer }

constructor TAiMCPSSEHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSessions := TObjectDictionary<string, TMCPSSESession>.Create([doOwnsValues]);
  FSessionsLock := TCriticalSection.Create;

  FHttpServer := TIdHTTPServer.Create(Self);
  // Asignamos manejadores
  FHttpServer.OnCommandGet := OnCommandGet;
  FHttpServer.OnCommandOther := OnCommandOther;

  // Configuración vital para SSE
  FHttpServer.KeepAlive := True;
  FHttpServer.AutoStartSession := False;

  // Endpoints por defecto
  FSseEndpoint := '/sse';
  FMessagesEndpoint := '/messages';
end;

destructor TAiMCPSSEHttpServer.Destroy;
begin
  Stop;
  FHttpServer.Free; // El server debe morir antes que las sesiones
  FSessions.Free;
  FSessionsLock.Free;
  inherited;
end;

procedure TAiMCPSSEHttpServer.Start;
begin
  inherited Start;
  if not FHttpServer.Active then
  begin
    FHttpServer.DefaultPort := FLogicServer.Port;
    FHttpServer.Active := True;
  end;
end;

procedure TAiMCPSSEHttpServer.Stop;
begin
  if FHttpServer.Active then
    FHttpServer.Active := False;

  // Limpiar todas las sesiones activas
  FSessionsLock.Enter;
  try
    FSessions.Clear;
  finally
    FSessionsLock.Leave;
  end;

  inherited Stop;
end;

function TAiMCPSSEHttpServer.GenerateSessionID: string;
begin
  Result := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '').ToLower;
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

procedure TAiMCPSSEHttpServer.VerifyAndSetCORSHeaders(AResponseInfo: TIdHTTPResponseInfo);
begin
  if not FLogicServer.CorsEnabled then
    Exit;

  // Para desarrollo y Claude Desktop, '*' suele funcionar,
  // pero algunas herramientas estrictas requieren el origen exacto.
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*';
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'GET, POST, OPTIONS';
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'Content-Type, Cache-Control, X-Session-ID';
  AResponseInfo.CustomHeaders.Values['Access-Control-Expose-Headers'] := 'Content-Type';
end;

// Manejador principal para GET
procedure TAiMCPSSEHttpServer.OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  // Siempre poner CORS
  VerifyAndSetCORSHeaders(AResponseInfo);

  if ARequestInfo.URI = FSseEndpoint then
  begin
    HandleSSEConnection(AContext, AResponseInfo);
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'Not Found. Use ' + FSseEndpoint + ' for SSE.';
  end;
end;

// Manejador para POST y OPTIONS
procedure TAiMCPSSEHttpServer.OnCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  VerifyAndSetCORSHeaders(AResponseInfo);

  if ARequestInfo.Command = 'OPTIONS' then
  begin
    AResponseInfo.ResponseNo := 204;
    AResponseInfo.ContentLength := 0;
    Exit;
  end;

  if (ARequestInfo.Command = 'POST') and (ARequestInfo.URI = FMessagesEndpoint) then
  begin
    HandlePostMessage(AContext, ARequestInfo, AResponseInfo);
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'Endpoint not found or method not allowed.';
  end;
end;

// =============================================================================
// LÓGICA CRÍTICA SSE
// =============================================================================

procedure TAiMCPSSEHttpServer.HandleSSEConnection(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
var
  SessionID: string;
  Session: TMCPSSESession;
  Msg: string;
  PopResult: TWaitResult;
  HandshakeStr: string;
  BytesToSend: TIdBytes;
begin
  // 1. Configurar Sesión
  SessionID := GenerateSessionID;
  Session := GetOrCreateSession(SessionID);

  WriteLn(Format('SSE: Client Connected (Session: %s)', [SessionID]));

  try
    // 2. Configurar Headers HTTP para Streaming
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ContentType := 'text/event-stream; charset=utf-8';
    AResponseInfo.CacheControl := 'no-cache';
    AResponseInfo.Connection := 'keep-alive';

    // IMPORTANTE: Forzar que Indy envíe las cabeceras AHORA MISMO
    // Esto impide que Indy bufferice la respuesta esperando que termine el método.
    AResponseInfo.WriteHeader;

    // 3. Enviar Handshake MCP (Protocolo)
    // Indica al cliente dónde mandar los comandos POST
    var
    FullMsgUrl := Format('%s?session_id=%s', [FMessagesEndpoint, SessionID]);

    HandshakeStr := 'event: endpoint' + #10 + 'data: ' + FullMsgUrl + #10#10;

    AContext.Connection.IOHandler.Write(ToBytes(HandshakeStr, IndyTextEncoding_UTF8));

    // 4. Bucle Principal de Streaming (Keep-Alive Loop)
    while AContext.Connection.Connected do
    begin
      // Intentar sacar mensaje de la cola.
      // Timeout de 1000ms (1 seg) definido en el constructor de TMCPSSESession.
      PopResult := Session.Outbox.PopItem(Msg);

      if PopResult = wrSignaled then
      begin
        // --- CASO A: Hay mensaje para enviar ---
        // Formato SSE: "data: <contenido>\n\n"
        var
        Payload := 'data: ' + Msg + #10#10;

        // Escribimos Bytes directamente para evitar corrupción de caracteres
        BytesToSend := ToBytes(Payload, IndyTextEncoding_UTF8);
        AContext.Connection.IOHandler.Write(BytesToSend);

        Session.LastActivity := Now;
      end
      else if PopResult = wrTimeout then
      begin
        // --- CASO B: No hay mensajes (Idle) ---
        // Enviamos un comentario "ping" para mantener la conexión viva
        // y evitar timeouts de antivirus/proxies.
        var
        Ping := ': keep-alive' + #10#10;
        AContext.Connection.IOHandler.Write(ToBytes(Ping, IndyTextEncoding_ASCII));
      end
      else if PopResult = wrAbandoned then
      begin
        // La cola se destruyó (Servidor deteniéndose)
        Break;
      end;

      // Verificación proactiva de desconexión
      // Si el cliente cerró el socket, esto lanzará una excepción segura.
      // AContext.Connection.CheckForDisconnect(True, True);

      if AContext.Connection.IOHandler <> nil then
        AContext.Connection.IOHandler.CheckForDisconnect(True, True);

    end;

  except
    on E: Exception do
    begin
      // Es normal ver "Connection Closed Gracefully" o "Socket Error" cuando el cliente cierra.
      // No lo tratamos como error crítico.
    end;
  end;

  // 5. Limpieza al salir del bucle
  FSessionsLock.Enter;
  try
    if FSessions.ContainsKey(SessionID) then
      FSessions.Remove(SessionID);
  finally
    FSessionsLock.Leave;
  end;

  WriteLn(Format('SSE: Client Disconnected (Session: %s)', [SessionID]));
end;

// =============================================================================
// LÓGICA POST (MESSAGES)
// =============================================================================

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
