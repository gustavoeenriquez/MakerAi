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
  IdContext, IdCustomHTTPServer, IdHTTPServer, IdGlobal,
  UMakerAi.MCPServer.Core;

type
  // Representa una sesión SSE activa
  TMCPSSESession = class
  public
    SessionID: string;
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
    FSseEndpoint: string;      // por defecto '/sse'
    FMessagesEndpoint: string; // por defecto '/messages'

    procedure OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    // Manejadores específicos
    procedure HandleSSEConnection(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandlePostMessage(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    // Helpers
    function GetOrCreateSession(const AID: string): TMCPSSESession;
    function GenerateSessionID: string;
    procedure CleanUpExpiredSessions;
    function VerifyAndSetCORSHeaders(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override;
    procedure Stop; override;

    // Método para enviar notificaciones asíncronas desde la lógica de negocio hacia un cliente específico
    procedure PushNotification(const ASessionID: string; const ANotificationJson: string);
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
  // Cola con capacidad para 1000 mensajes, timeout de push infinito, pop 100ms
  Outbox := TThreadedQueue<string>.Create(1000, INFINITE, 100);
  LastActivity := Now;
end;

destructor TMCPSSESession.Destroy;
var
  Item: string;
begin
  Outbox.DoShutDown;
  while Outbox.PopItem(Item) = wrSignaled do ;
  Outbox.Free;
  inherited;
end;

{ TAiMCPSSEHttpServer }

constructor TAiMCPSSEHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpServer := TIdHTTPServer.Create(Self);
  FHttpServer.OnCommandGet := OnCommandGet;
  FHttpServer.OnCommandOther := OnCommandGet; // Para manejar POST y OPTIONS

  FSessions := TObjectDictionary<string, TMCPSSESession>.Create([doOwnsValues]);
  FSessionsLock := TCriticalSection.Create;

  FSseEndpoint := '/sse';
  FMessagesEndpoint := '/messages';
end;

destructor TAiMCPSSEHttpServer.Destroy;
begin
  Stop;
  FSessions.Free;
  FSessionsLock.Free;
  inherited;
end;

procedure TAiMCPSSEHttpServer.Start;
begin
  inherited Start;
  FHttpServer.DefaultPort := FLogicServer.Port;
  FHttpServer.Active := True;
end;

procedure TAiMCPSSEHttpServer.Stop;
begin
  if FHttpServer.Active then
    FHttpServer.Active := False;

  // Limpiar sesiones
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

procedure TAiMCPSSEHttpServer.CleanUpExpiredSessions;
var
  KeysToRemove: TList<string>;
  Pair: TPair<string, TMCPSSESession>;
begin
  // Lógica simple de limpieza (ej. sesiones inactivas por > 1 hora)
  // Se podría llamar desde un Timer o un hilo de mantenimiento.
  // Aquí se deja implementada pero no se llama automáticamente para simplificar.
  KeysToRemove := TList<string>.Create;
  try
    FSessionsLock.Enter;
    try
      for Pair in FSessions do
      begin
        if MinutesBetween(Now, Pair.Value.LastActivity) > 60 then
          KeysToRemove.Add(Pair.Key);
      end;

      for var Key in KeysToRemove do
        FSessions.Remove(Key);
    finally
      FSessionsLock.Leave;
    end;
  finally
    KeysToRemove.Free;
  end;
end;

function TAiMCPSSEHttpServer.VerifyAndSetCORSHeaders(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;
begin
  // Copia de la lógica de tu servidor HTTP base, es necesaria aquí también.
  Result := True;
  if not FLogicServer.CorsEnabled then Exit;

  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*'; // Simplificado para SSE
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'POST, GET, OPTIONS';
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'Content-Type';

  // Para SSE es crucial exponer el header si usamos autenticación
  // AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Credentials'] := 'true';
end;

procedure TAiMCPSSEHttpServer.OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  VerifyAndSetCORSHeaders(ARequestInfo, AResponseInfo);

  if ARequestInfo.Command = 'OPTIONS' then
  begin
    AResponseInfo.ResponseNo := 204;
    Exit;
  end;

  if ARequestInfo.URI = FSseEndpoint then
  begin
    if ARequestInfo.Command = 'GET' then
      HandleSSEConnection(AContext, AResponseInfo)
    else
      AResponseInfo.ResponseNo := 405;
  end
  else if ARequestInfo.URI = FMessagesEndpoint then
  begin
    if ARequestInfo.Command = 'POST' then
      HandlePostMessage(AContext, ARequestInfo, AResponseInfo)
    else
      AResponseInfo.ResponseNo := 405;
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'MCP SSE Server Active. Use ' + FSseEndpoint;
  end;
end;

// =============================================================================
// Lógica SSE (Streaming)
// =============================================================================
// Asegúrate de tener IdGlobal en el uses
// uses ..., IdGlobal;



procedure TAiMCPSSEHttpServer.HandleSSEConnection(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
var
  SessionID: string;
  Session: TMCPSSESession;
  Msg: string;
  PopRes: TWaitResult;
  Handshake: string;
  Buffer: TIdBytes; // Usaremos bytes directos
begin
  WriteLn('SERVER: === New SSE Request Started ===');

  SessionID := GenerateSessionID;
  Session := GetOrCreateSession(SessionID);

  // 1. Cabeceras
  AResponseInfo.ResponseNo := 200;
  AResponseInfo.ContentType := 'text/event-stream; charset=utf-8';
  AResponseInfo.CacheControl := 'no-cache';
  AResponseInfo.Connection := 'keep-alive';
  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Origin'] := '*';
  AResponseInfo.CustomHeaders.Values['X-Accel-Buffering'] := 'no';

  AResponseInfo.WriteHeader;

  // 2. Handshake
  var EndpointUrl := Format('%s?session_id=%s', [FMessagesEndpoint, SessionID]);

  Handshake := 'event: endpoint' + #13#10 +
               'data: ' + EndpointUrl + #13#10 +
               #13#10;

  try
    WriteLn('SERVER: Converting Handshake to Bytes...');
    // CONVERSIÓN EXPLÍCITA A BYTES (Más seguro que Write(String))
    Buffer := ToBytes(Handshake, IndyTextEncoding_UTF8);

    WriteLn('SERVER: Writing Handshake Bytes...');
    AContext.Connection.IOHandler.Write(Buffer);

    // Pequeña pausa para asegurar que el socket flushea antes de entrar al loop
    Sleep(10);

    WriteLn('SERVER: Entered Keep-Alive Loop. Waiting for messages...');

    // 3. Bucle
    repeat
      PopRes := Session.Outbox.PopItem(Msg);

      if PopRes = wrSignaled then
      begin
        WriteLn('SERVER: Sending Message...');
        var Payload := 'data: ' + Msg + #13#10#13#10;

        // Conversión y envío de bytes
        Buffer := ToBytes(Payload, IndyTextEncoding_UTF8);
        AContext.Connection.IOHandler.Write(Buffer);

        Session.LastActivity := Now;
      end
      else if PopRes = wrAbandoned then
      begin
        WriteLn('SERVER: Queue Abandoned. Exiting.');
        Break;
      end;

      // Chequeo de seguridad: Si el socket se desconecta, Write lanzará excepción
      // en la siguiente vuelta, o podemos chequear aquí si queremos ser proactivos,
      // pero dejaremos que el try-except maneje la desconexión.

    until False;

  except
    on E: Exception do
    begin
      // Este Log es vital para saber por qué se cierra
      WriteLn('SERVER ERROR/DISCONNECT: ' + E.ClassName + ' - ' + E.Message);
    end;
  end;

  // Limpieza
  FSessionsLock.Enter;
  try
    if FSessions.ContainsKey(SessionID) then
      FSessions.Remove(SessionID);
  finally
    FSessionsLock.Leave;
  end;

  WriteLn('SERVER: === Request Ended ===');
end;


// =============================================================================
// Lógica POST (Comandos)
// =============================================================================
procedure TAiMCPSSEHttpServer.HandlePostMessage(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  SessionID: string;
  Session: TMCPSSESession;
  JsonBody: string;
  ResponseJson: string;
begin
  // 1. Validar sesión
  SessionID := ARequestInfo.Params.Values['session_id'];
  if SessionID = '' then
  begin
    AResponseInfo.ResponseNo := 400;
    AResponseInfo.ContentText := 'Missing session_id';
    Exit;
  end;

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

  // 2. Leer JSON del cuerpo
  if ARequestInfo.PostStream <> nil then
  begin
    var Stream := TStringStream.Create;
    try
      Stream.CopyFrom(ARequestInfo.PostStream, 0);
      JsonBody := Stream.DataString;
    finally
      Stream.Free;
    end;
  end;

  // 3. Ejecutar Lógica MCP (usando tu Core Logic Server)
  // Nota: ExecuteRequest es síncrono.
  try
    ResponseJson := FLogicServer.ExecuteRequest(JsonBody, SessionID);

    // 4. Enviar la respuesta por el canal SSE (Outbox)
    // No respondemos por HTTP Response, sino por el stream abierto.
    if ResponseJson <> '' then
      Session.Outbox.PushItem(ResponseJson);

    // 5. Responder al POST con 202 Accepted
    AResponseInfo.ResponseNo := 202;
    AResponseInfo.ContentText := 'Accepted';

  except
    on E: Exception do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentText := E.Message;
    end;
  end;
end;

procedure TAiMCPSSEHttpServer.PushNotification(const ASessionID: string; const ANotificationJson: string);
var
  Session: TMCPSSESession;
begin
  FSessionsLock.Enter;
  try
    if FSessions.TryGetValue(ASessionID, Session) then
      Session.Outbox.PushItem(ANotificationJson);
  finally
    FSessionsLock.Leave;
  end;
end;

end.
