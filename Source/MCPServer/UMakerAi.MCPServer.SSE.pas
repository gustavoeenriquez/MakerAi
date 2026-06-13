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

unit UMakerAi.MCPServer.SSE;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections, System.SyncObjs,
  System.Threading,
  IdContext, IdCustomHTTPServer, IdHTTPServer, IdGlobal, IdSocketHandle,
  UMakerAi.MCPServer.Core;

type
  // Evento para atender rutas HTTP que no son /sse ni /messages (p.ej. servir
  // archivos est?ticos como im?genes generadas). AHandled=True evita el 404.
  TAiMCPUnhandledRequestEvent = procedure(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
    AResponseInfo: TIdHTTPResponseInfo; var AHandled: Boolean) of object;
  // Representa una sesi?n SSE activa
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

    // Configuraci?n de endpoints
    FSseEndpoint: string; // por defecto '/sse'
    FMessagesEndpoint: string; // por defecto '/messages'
    FOnUnhandledRequest: TAiMCPUnhandledRequestEvent;

    // Eventos de Indy
    procedure OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure OnCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    // Manejadores espec�ficos
    procedure HandleSSEConnection(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandlePostMessage(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo; const AAuthContext: TAiAuthContext);

    // Helpers
    function GetOrCreateSession(const AID: string): TMCPSSESession;
    function GenerateSessionID: string;
    procedure VerifyAndSetCORSHeaders(AResponseInfo: TIdHTTPResponseInfo);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override;
    procedure Stop; override;

    // Env?a una notificaci?n JSON-RPC arbitraria por el canal SSE de una sesi?n.
    // Best-effort: si la sesi?n no existe o su cola est? llena, se descarta.
    procedure PushNotification(const ASessionID, ANotificationJson: string);

    // Propiedades de configuraci?n
  published
    property SseEndpoint: string read FSseEndpoint write FSseEndpoint;
    property MessagesEndpoint: string read FMessagesEndpoint write FMessagesEndpoint;
    property ApiKey;
    property OnValidateRequest;
    // Rutas HTTP fuera de /sse y /messages (archivos est?ticos, health checks...).
    // Se invoca ANTES de la autenticaci?n MCP — el handler decide su propia auth.
    property OnUnhandledRequest: TAiMCPUnhandledRequestEvent read FOnUnhandledRequest write FOnUnhandledRequest;
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
  // PushTimeout: INFINITE (El productor espera si est? lleno)
  // PopTimeout: 1000ms (IMPORTANTE: No usar INFINITE aqu? para permitir el heartbeat)
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

  // Configuraci?n vital para SSE
  FHttpServer.KeepAlive := True;
  FHttpServer.AutoStartSession := False;
  FHttpServer.ReuseSocket := rsTrue;  // Permite re-bind mientras CLOSE_WAIT sigue activo

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

  // Habilita notifications/progress: el core delega aqu? la entrega por sesi?n.
  FLogicServer.OnSendNotification :=
    procedure(const ASessionID, ANotificationJson: string)
    begin
      PushNotification(ASessionID, ANotificationJson);
    end;

  if not FHttpServer.Active then
  begin
    FHttpServer.DefaultPort := FLogicServer.Port;
    FHttpServer.Active := True;
  end;
end;

procedure TAiMCPSSEHttpServer.PushNotification(const ASessionID, ANotificationJson: string);
var
  Session: TMCPSSESession;
begin
  // Push DENTRO del lock: la sesi?n se destruye bajo este mismo lock al
  // desconectar, as? que aqu? sigue viva. El check de QueueSize garantiza que
  // nunca bloqueamos con el lock tomado (PushTimeout de la cola es INFINITE).
  FSessionsLock.Enter;
  try
    if FSessions.TryGetValue(ASessionID, Session) then
      if Session.Outbox.QueueSize < 900 then
        Session.Outbox.PushItem(ANotificationJson);
  finally
    FSessionsLock.Leave;
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
var
  AuthContext: TAiAuthContext;
  AuthHeader: string;
var
  Handled: Boolean;
begin
  VerifyAndSetCORSHeaders(AResponseInfo);

  // Rutas ajenas al protocolo MCP (est?ticos, health...) — antes de la auth MCP.
  if (ARequestInfo.URI <> FSseEndpoint) and (ARequestInfo.Document <> FMessagesEndpoint) and
     (ARequestInfo.URI <> FMessagesEndpoint) and Assigned(FOnUnhandledRequest) then
  begin
    Handled := False;
    FOnUnhandledRequest(AContext, ARequestInfo, AResponseInfo, Handled);
    if Handled then
      Exit;
  end;

  // Autenticaci�n en la conexi�n SSE
  AuthHeader := ARequestInfo.RawHeaders.Values['Authorization'];
  if AuthHeader = '' then
    AuthHeader := ARequestInfo.RawHeaders.Values['X-API-Key'];

  if not ValidateRequest(AuthHeader, AContext.Binding.PeerIP, AuthContext) then
  begin
    AResponseInfo.ResponseNo := 401;
    AResponseInfo.ContentText := '{"error": "Authentication failed"}';
    AResponseInfo.ContentType := 'application/json';
    Exit;
  end;

  if ARequestInfo.Command = 'OPTIONS' then
  begin
    AResponseInfo.ResponseNo := 204;
    AResponseInfo.ContentLength := 0;
  end
  else if (ARequestInfo.Command = 'POST') and
          ((ARequestInfo.URI = FMessagesEndpoint) or (ARequestInfo.Document = FMessagesEndpoint)) then
  begin
    HandlePostMessage(AContext, ARequestInfo, AResponseInfo, AuthContext);
  end
  else if ARequestInfo.URI = FSseEndpoint then
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
var
  AuthContext: TAiAuthContext;
  AuthHeader: string;
begin
  VerifyAndSetCORSHeaders(AResponseInfo);

  if ARequestInfo.Command = 'OPTIONS' then
  begin
    AResponseInfo.ResponseNo := 204;
    AResponseInfo.ContentLength := 0;
    Exit;
  end;

  // Autenticaci�n en cada POST
  AuthHeader := ARequestInfo.RawHeaders.Values['Authorization'];
  if AuthHeader = '' then
    AuthHeader := ARequestInfo.RawHeaders.Values['X-API-Key'];

  if not ValidateRequest(AuthHeader, AContext.Binding.PeerIP, AuthContext) then
  begin
    AResponseInfo.ResponseNo := 401;
    AResponseInfo.ContentText := '{"error": "Authentication failed"}';
    AResponseInfo.ContentType := 'application/json';
    Exit;
  end;

  if (ARequestInfo.Command = 'POST') and ((ARequestInfo.URI = FMessagesEndpoint) or (ARequestInfo.Document = FMessagesEndpoint)) then
  begin
    HandlePostMessage(AContext, ARequestInfo, AResponseInfo, AuthContext);
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'Endpoint not found or method not allowed.';
  end;
end;

// =============================================================================
// L?GICA CR?TICA SSE
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
  // 1. Configurar Sesi?n
  SessionID := GenerateSessionID;
  Session := GetOrCreateSession(SessionID);

  WriteLn(Format('SSE: Client Connected (Session: %s)', [SessionID]));

  try
    // 2. Headers HTTP para Streaming — escritos MANUALMENTE.
    // No usar AResponseInfo.WriteHeader: en Indy de Delphi 13, con
    // ContentLength=-1 y sin ContentText/Stream, WriteHeader auto-genera un
    // body "<HTML><BODY><B>200 OK</B></BODY></HTML>" y env?a Content-Length:39,
    // lo que hace que clientes HTTP correctos (curl, python, Claude Desktop)
    // corten el stream SSE tras 39 bytes. SSE requiere ausencia total de
    // Content-Length (lectura hasta cierre de conexi?n).
    var Hdr :=
      'HTTP/1.1 200 OK' + #13#10 +
      'Content-Type: text/event-stream; charset=utf-8' + #13#10 +
      'Cache-Control: no-cache' + #13#10 +
      'Connection: keep-alive' + #13#10 +
      'X-Accel-Buffering: no' + #13#10;
    if FLogicServer.CorsEnabled then
      Hdr := Hdr + 'Access-Control-Allow-Origin: *' + #13#10;
    Hdr := Hdr + #13#10;
    AContext.Connection.IOHandler.Write(ToBytes(Hdr, IndyTextEncoding_UTF8));

    // Marcar como escritos para que Indy NO genere sus propios headers
    // cuando este handler retorne.
    AResponseInfo.HeaderHasBeenWritten := True;

    // 3. Enviar Handshake MCP (Protocolo)
    // Indica al cliente d?nde mandar los comandos POST
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

        // Escribimos Bytes directamente para evitar corrupci?n de caracteres
        BytesToSend := ToBytes(Payload, IndyTextEncoding_UTF8);
        AContext.Connection.IOHandler.Write(BytesToSend);

        Session.LastActivity := Now;
      end
      else if PopResult = wrTimeout then
      begin
        // --- CASO B: No hay mensajes (Idle) ---
        // Enviamos un comentario "ping" para mantener la conexi?n viva
        // y evitar timeouts de antivirus/proxies.
        var
        Ping := ': keep-alive' + #10#10;
        AContext.Connection.IOHandler.Write(ToBytes(Ping, IndyTextEncoding_ASCII));
      end
      else if PopResult = wrAbandoned then
      begin
        // La cola se destruy? (Servidor deteni?ndose)
        Break;
      end;

      // Verificaci?n proactiva de desconexi?n
      // Si el cliente cerr? el socket, esto lanzar? una excepci?n segura.
      // AContext.Connection.CheckForDisconnect(True, True);

      if AContext.Connection.IOHandler <> nil then
        AContext.Connection.IOHandler.CheckForDisconnect(True, True);

    end;

  except
    on E: Exception do
    begin
      // Es normal ver "Connection Closed Gracefully" o "Socket Error" cuando el cliente cierra.
      // No lo tratamos como error cr?tico.
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
// L?GICA POST (MESSAGES)
// =============================================================================

procedure TAiMCPSSEHttpServer.HandlePostMessage(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo; const AAuthContext: TAiAuthContext);
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

  // 2. Buscar Sesi?n Activa
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

  // 4. Ejecutar L?gica (Core)
  try
    // tools/call puede tardar minutos (generaci?n de im?genes, procesos externos).
    // Se ejecuta async: el POST responde 202 de inmediato y tanto las
    // notifications/progress como el resultado viajan por el canal SSE.
    // Los dem?s m?todos (initialize, tools/list...) son r?pidos y van inline.
    var LMethod := '';
    var LParsed := TJSONObject.ParseJSONValue(JsonBody);
    try
      if LParsed is TJSONObject then
        LMethod := TJSONObject(LParsed).GetValue<string>('method', '');
    finally
      LParsed.Free;
    end;

    if SameText(LMethod, 'tools/call') then
    begin
      // Capturamos SessionID (string) y NO el objeto Session: si el cliente se
      // desconecta durante la ejecuci?n, PushNotification simplemente descarta.
      var LBody := JsonBody;
      var LSessionID := SessionID;
      var LAuth := AAuthContext;
      TTask.Run(
        procedure
        var
          Resp: string;
        begin
          try
            Resp := FLogicServer.ExecuteRequest(LBody, LSessionID, LAuth);
          except
            on E: Exception do
              Resp := '';
          end;
          if Resp <> '' then
            PushNotification(LSessionID, Resp);
        end);
    end
    else
    begin
      // ExecuteRequest devuelve el JSON de respuesta (con contexto de autenticaci�n)
      ResponseJson := FLogicServer.ExecuteRequest(JsonBody, SessionID, AAuthContext);

      // 5. Enviar Respuesta por el CANAL SSE (No por HTTP response)
      if ResponseJson <> '' then
        Session.Outbox.PushItem(ResponseJson);
    end;

    // 6. Responder al POST con "202 Accepted"
    // Esto le dice al cliente: "Recib? tu orden, espera la respuesta por el stream".
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
