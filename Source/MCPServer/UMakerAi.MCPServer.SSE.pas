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
// EXPERIMENTAL: Transporte SSE (Server-Sent Events) para el servidor MCP.
//
// Arquitectura:
//   - TAiMCPSSEServer: componente principal
//   - TSseAcceptThread: acepta conexiones TCP entrantes (TInetServer de ssockets)
//   - TSseClientThread: maneja una conexion TCP (GET /sse o POST /messages)
//   - TAiSSESession: estado de una sesion SSE activa
//
// Protocolo:
//   1. Cliente hace GET /sse → recibe endpoint event con session_id
//   2. Cliente hace POST /messages?session_id=xxx con body JSON-RPC
//   3. El servidor procesa y envia el resultado como data event en la sesion SSE
//
// Limitaciones conocidas:
//   - Experimental: puede tener problemas con clientes que no cierran conexiones
//   - No soporta multiples mensajes en la misma sesion SSE (una respuesta por llamada)
//   - Requiere que el cliente mantenga la conexion SSE abierta
//
// NOTA: MCP 2025-06-18 marca SSE como legacy. Usar HTTP Streaming si es posible.

unit uMakerAi.MCPServer.SSE;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs,
  ssockets,
  uMakerAi.MCPServer.Core;

type
  // Forward declarations
  TAiMCPSSEServer  = class;
  TSseClientThread = class;

  // -------------------------------------------------------------------------
  // TAiSSESession - estado de una sesion SSE activa
  // -------------------------------------------------------------------------
  TAiSSESession = class(TObject)
  public
    SessionID       : string;
    Socket          : TSocketStream;
    OutboxLock      : TCriticalSection;
    PendingMessages : TStringList; // mensajes JSON pendientes de enviar
    Active          : Boolean;

    constructor Create(const ASessionID: string; ASocket: TSocketStream);
    destructor Destroy; override;

    // Encola un mensaje para enviar al cliente SSE
    procedure EnqueueMessage(const AData: string);

    // Obtiene y limpia la lista de mensajes pendientes
    procedure DrainMessages(out AMessages: TStringList);
  end;

  // -------------------------------------------------------------------------
  // TSseAcceptThread - acepta conexiones TCP entrantes
  // -------------------------------------------------------------------------
  TSseAcceptThread = class(TThread)
  private
    FServer     : TAiMCPSSEServer;
    FServerSock : TInetServer;
  public
    constructor Create(AServer: TAiMCPSSEServer; APort: Word);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  // -------------------------------------------------------------------------
  // TSseClientThread - maneja una conexion TCP individual
  // -------------------------------------------------------------------------
  TSseClientThread = class(TThread)
  private
    FServer  : TAiMCPSSEServer;
    FSocket  : TSocketStream;
    FSession : TAiSSESession; // nil si es POST /messages

    function  ReadLine: string;
    procedure HandleSSEConnection(const APath: string);
    procedure HandlePostMessage(const APath, ABody: string;
        const AHeaders: TStrings);
    procedure SendHTTPResponse(AStatusCode: Integer;
        const AStatusText, AContentType, ABody: string);

  public
    constructor Create(AServer: TAiMCPSSEServer; ASocket: TSocketStream);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  // -------------------------------------------------------------------------
  // TAiMCPSSEServer - servidor MCP con transporte SSE
  // -------------------------------------------------------------------------
  TAiMCPSSEServer = class(TAiMCPServer)
  private
    FAcceptThread  : TSseAcceptThread;
    FSessionsLock  : TCriticalSection;
    FSessions      : TStringList; // key=SessionID, Object=TAiSSESession

    function GenerateSessionID: string;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override;
    procedure Stop;  override;

    // Llamado por TSseClientThread para registrar/obtener sesiones
    function  RegisterSession(ASocket: TSocketStream): TAiSSESession;
    procedure UnregisterSession(const ASessionID: string);
    function  FindSession(const ASessionID: string): TAiSSESession;

    // Procesa un request JSON-RPC y pone la respuesta en el outbox de la sesion
    procedure ProcessAndEnqueue(const ASessionID, ARequestJSON: string);

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
  StrUtils, fpjson, DateUtils;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function GenerateGUID: string;
var
  G: TGUID;
begin
  CreateGUID(G);
  Result := GUIDToString(G);
  // Quitar llaves y guiones para usar como session ID
  Result := StringReplace(Result, '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

function ExtractQueryParam(const APath, AParam: string): string;
var
  Pos1, Pos2: Integer;
  Search: string;
begin
  Result := '';
  Search := AParam + '=';
  Pos1 := System.Pos(Search, APath);
  if Pos1 = 0 then Exit;
  Inc(Pos1, Length(Search));
  Pos2 := Pos1;
  while (Pos2 <= Length(APath)) and (APath[Pos2] <> '&') do
    Inc(Pos2);
  Result := Copy(APath, Pos1, Pos2 - Pos1);
end;

function ExtractPathBase(const APath: string): string;
var
  P: Integer;
begin
  P := System.Pos('?', APath);
  if P > 0 then
    Result := Copy(APath, 1, P - 1)
  else
    Result := APath;
end;

// ---------------------------------------------------------------------------
// TAiSSESession
// ---------------------------------------------------------------------------

constructor TAiSSESession.Create(const ASessionID: string;
    ASocket: TSocketStream);
begin
  inherited Create;
  SessionID       := ASessionID;
  Socket          := ASocket;
  OutboxLock      := TCriticalSection.Create;
  PendingMessages := TStringList.Create;
  Active          := True;
end;

destructor TAiSSESession.Destroy;
begin
  Active := False;
  OutboxLock.Free;
  PendingMessages.Free;
  // Socket es liberado por TSseClientThread
  inherited;
end;

procedure TAiSSESession.EnqueueMessage(const AData: string);
begin
  OutboxLock.Enter;
  try
    PendingMessages.Add(AData);
  finally
    OutboxLock.Leave;
  end;
end;

procedure TAiSSESession.DrainMessages(out AMessages: TStringList);
begin
  AMessages := TStringList.Create;
  OutboxLock.Enter;
  try
    AMessages.Assign(PendingMessages);
    PendingMessages.Clear;
  finally
    OutboxLock.Leave;
  end;
end;

// ---------------------------------------------------------------------------
// TSseAcceptThread
// ---------------------------------------------------------------------------

constructor TSseAcceptThread.Create(AServer: TAiMCPSSEServer; APort: Word);
begin
  inherited Create(True);
  FServer := AServer;
  FServerSock := TInetServer.Create(APort);
  FreeOnTerminate := False;
end;

destructor TSseAcceptThread.Destroy;
begin
  try
    FServerSock.StopAccepting(True);
  except
    // Ignorar errores al cerrar
  end;
  FServerSock.Free;
  inherited;
end;

procedure TSseAcceptThread.Execute;
var
  ClientSocket : TSocketStream;
  ClientThread : TSseClientThread;
begin
  try
    FServerSock.Listen;
  except
    on E: Exception do
    begin
      // Error al escuchar — terminar
      Exit;
    end;
  end;

  while not Terminated do
  begin
    try
      // GetConnection bloquea hasta que haya una conexion entrante
      ClientSocket := FServerSock.GetConnection;
      if Assigned(ClientSocket) then
      begin
        ClientThread := TSseClientThread.Create(FServer, ClientSocket);
        ClientThread.FreeOnTerminate := True;
        ClientThread.Start;
      end;
    except
      // GetConnection puede lanzar cuando el servidor se cierra — ignorar
      if not Terminated then
        Sleep(100);
    end;
  end;
end;

// ---------------------------------------------------------------------------
// TSseClientThread
// ---------------------------------------------------------------------------

constructor TSseClientThread.Create(AServer: TAiMCPSSEServer;
    ASocket: TSocketStream);
begin
  inherited Create(True);
  FServer  := AServer;
  FSocket  := ASocket;
  FSession := nil;
  FreeOnTerminate := True;
end;

destructor TSseClientThread.Destroy;
begin
  FSocket.Free;
  inherited;
end;

function TSseClientThread.ReadLine: string;
var
  C   : Char;
  Line: string;
  Res : Integer;
begin
  Line := '';
  repeat
    Res := FSocket.Read(C, 1);
    if Res <= 0 then Break;
    if C = #10 then Break;
    if C <> #13 then Line := Line + C;
  until False;
  Result := Line;
end;

procedure TSseClientThread.SendHTTPResponse(AStatusCode: Integer;
    const AStatusText, AContentType, ABody: string);
var
  Headers: string;
begin
  Headers :=
    'HTTP/1.1 ' + IntToStr(AStatusCode) + ' ' + AStatusText + #13#10 +
    'Content-Type: ' + AContentType + #13#10 +
    'Content-Length: ' + IntToStr(Length(ABody)) + #13#10 +
    'Access-Control-Allow-Origin: *' + #13#10 +
    'Connection: close' + #13#10 +
    #13#10 +
    ABody;
  try
    FSocket.WriteBuffer(Headers[1], Length(Headers));
  except
    // Ignorar error de escritura
  end;
end;

procedure TSseClientThread.HandleSSEConnection(const APath: string);
var
  Session     : TAiSSESession;
  HeadersSSE  : string;
  Msgs        : TStringList;
  I           : Integer;
  SSEData     : string;
  HeartbeatMS : Cardinal;
begin
  Session := FServer.RegisterSession(FSocket);
  FSession := Session;

  // Enviar headers HTTP SSE
  HeadersSSE :=
    'HTTP/1.1 200 OK'                                         + #13#10 +
    'Content-Type: text/event-stream'                         + #13#10 +
    'Cache-Control: no-cache'                                 + #13#10 +
    'Connection: keep-alive'                                  + #13#10 +
    'Access-Control-Allow-Origin: *'                          + #13#10 +
    'X-Accel-Buffering: no'                                   + #13#10 +
    #13#10;

  try
    FSocket.WriteBuffer(HeadersSSE[1], Length(HeadersSSE));
  except
    FServer.UnregisterSession(Session.SessionID);
    Exit;
  end;

  // Enviar endpoint event con la URL de POST
  SSEData :=
    'event: endpoint' + #10 +
    'data: /messages?session_id=' + Session.SessionID + #10 +
    #10;
  try
    FSocket.WriteBuffer(SSEData[1], Length(SSEData));
  except
    FServer.UnregisterSession(Session.SessionID);
    Exit;
  end;

  // Loop: enviar mensajes pendientes y heartbeat cada segundo
  HeartbeatMS := 0;
  while Session.Active and not Terminated do
  begin
    try
      // Vaciar outbox
      Session.DrainMessages(Msgs);
      try
        for I := 0 to Msgs.Count - 1 do
        begin
          SSEData := 'data: ' + Msgs[I] + #10 + #10;
          FSocket.WriteBuffer(SSEData[1], Length(SSEData));
        end;
      finally
        Msgs.Free;
      end;

      // Heartbeat cada 30 segundos (comment event)
      Inc(HeartbeatMS, 100);
      if HeartbeatMS >= 30000 then
      begin
        HeartbeatMS := 0;
        SSEData := ': heartbeat' + #10 + #10;
        FSocket.WriteBuffer(SSEData[1], Length(SSEData));
      end;

      Sleep(100);
    except
      // Cliente desconectado
      Break;
    end;
  end;

  FServer.UnregisterSession(Session.SessionID);
end;

procedure TSseClientThread.HandlePostMessage(const APath, ABody: string;
    const AHeaders: TStrings);
var
  SessionID: string;
begin
  SessionID := ExtractQueryParam(APath, 'session_id');

  if SessionID = '' then
  begin
    SendHTTPResponse(400, 'Bad Request', 'application/json',
        '{"error":"Falta session_id"}');
    Exit;
  end;

  SendHTTPResponse(202, 'Accepted', 'application/json', '{}');

  // Procesar async: pone respuesta en el outbox de la sesion
  FServer.ProcessAndEnqueue(SessionID, ABody);
end;

procedure TSseClientThread.Execute;
var
  RequestLine : string;
  Method      : string;
  Path        : string;
  PathBase    : string;
  Headers     : TStringList;
  Line        : string;
  ContentLen  : Integer;
  Body        : string;
  BufLen      : Integer;
  Buf         : array of Byte;
  P1, P2      : Integer;
  Rest        : string;
begin
  Headers := TStringList.Create;
  try
    // Leer request line
    RequestLine := ReadLine;
    if RequestLine = '' then Exit;

    // Parsear: "METHOD /path HTTP/1.1"
    Method := '';
    Path   := '';
    P1 := Pos(' ', RequestLine);
    if P1 > 0 then
    begin
      Method := Copy(RequestLine, 1, P1 - 1);
      Rest   := Copy(RequestLine, P1 + 1, MaxInt);
      P2     := Pos(' ', Rest);
      if P2 > 0 then
        Path := Copy(Rest, 1, P2 - 1)
      else
        Path := Rest;
    end;

    PathBase := ExtractPathBase(Path);

    // Leer headers
    ContentLen := 0;
    repeat
      Line := ReadLine;
      if Line = '' then Break;
      Headers.Add(Line);
      if AnsiStartsText('Content-Length:', Line) then
        ContentLen := StrToIntDef(Trim(Copy(Line, 16, MaxInt)), 0);
    until False;

    // Leer body si hay
    Body := '';
    if ContentLen > 0 then
    begin
      BufLen := ContentLen;
      SetLength(Buf, BufLen);
      FSocket.ReadBuffer(Buf[0], BufLen);
      SetLength(Body, BufLen);
      Move(Buf[0], Body[1], BufLen);
    end;

    // Despachar
    if SameText(Method, 'OPTIONS') then
    begin
      SendHTTPResponse(204, 'No Content', 'text/plain', '');
    end
    else if SameText(Method, 'GET') and
            (SameText(PathBase, '/sse') or
             SameText(PathBase, FServer.Endpoint + '/sse')) then
    begin
      HandleSSEConnection(Path);
    end
    else if SameText(Method, 'POST') and
            (AnsiContainsText(PathBase, '/messages') or
             AnsiContainsText(PathBase, '/message')) then
    begin
      HandlePostMessage(Path, Body, Headers);
    end
    else
    begin
      SendHTTPResponse(404, 'Not Found', 'text/plain',
          'Endpoint not found');
    end;

  finally
    Headers.Free;
  end;
end;

// ---------------------------------------------------------------------------
// TAiMCPSSEServer
// ---------------------------------------------------------------------------

constructor TAiMCPSSEServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptThread := nil;
  FSessionsLock := TCriticalSection.Create;
  FSessions     := TStringList.Create;
  FSessions.OwnsObjects := True;
  FSessions.CaseSensitive := False;

  // Puerto por defecto para SSE
  Port := 8090;
end;

destructor TAiMCPSSEServer.Destroy;
begin
  Stop;
  FSessionsLock.Free;
  FSessions.Free;
  inherited;
end;

function TAiMCPSSEServer.GenerateSessionID: string;
begin
  Result := GenerateGUID;
end;

procedure TAiMCPSSEServer.Start;
begin
  inherited Start;

  FAcceptThread := TSseAcceptThread.Create(Self, Port);
  FAcceptThread.FreeOnTerminate := False;
  FAcceptThread.Start;
end;

procedure TAiMCPSSEServer.Stop;
var
  I: Integer;
begin
  if Assigned(FAcceptThread) then
  begin
    FAcceptThread.Terminate;
    FAcceptThread.WaitFor;
    FreeAndNil(FAcceptThread);
  end;

  // Marcar todas las sesiones como inactivas
  FSessionsLock.Enter;
  try
    for I := 0 to FSessions.Count - 1 do
    begin
      if Assigned(FSessions.Objects[I]) then
        TAiSSESession(FSessions.Objects[I]).Active := False;
    end;
  finally
    FSessionsLock.Leave;
  end;

  inherited Stop;
end;

function TAiMCPSSEServer.RegisterSession(ASocket: TSocketStream): TAiSSESession;
var
  SessionID: string;
begin
  SessionID := GenerateSessionID;
  Result    := TAiSSESession.Create(SessionID, ASocket);

  FSessionsLock.Enter;
  try
    FSessions.AddObject(SessionID, Result);
  finally
    FSessionsLock.Leave;
  end;
end;

procedure TAiMCPSSEServer.UnregisterSession(const ASessionID: string);
var
  Idx: Integer;
begin
  FSessionsLock.Enter;
  try
    Idx := FSessions.IndexOf(ASessionID);
    if Idx >= 0 then
    begin
      // Marcar inactiva antes de liberar
      if Assigned(FSessions.Objects[Idx]) then
        TAiSSESession(FSessions.Objects[Idx]).Active := False;
      FSessions.Delete(Idx); // OwnsObjects = True → libera el objeto
    end;
  finally
    FSessionsLock.Leave;
  end;
end;

function TAiMCPSSEServer.FindSession(const ASessionID: string): TAiSSESession;
var
  Idx: Integer;
begin
  Result := nil;
  FSessionsLock.Enter;
  try
    Idx := FSessions.IndexOf(ASessionID);
    if Idx >= 0 then
      Result := TAiSSESession(FSessions.Objects[Idx]);
  finally
    FSessionsLock.Leave;
  end;
end;

procedure TAiMCPSSEServer.ProcessAndEnqueue(const ASessionID,
    ARequestJSON: string);
var
  Session  : TAiSSESession;
  AuthCtx  : TAiAuthContext;
  Response : string;
begin
  Session := FindSession(ASessionID);
  if not Assigned(Session) then
    Exit;

  // Autenticacion simple (el handshake ya fue validado en la conexion SSE)
  AuthCtx.IsAuthenticated := True;
  AuthCtx.UserID          := 'sse_client';
  AuthCtx.UserName        := 'sse';
  AuthCtx.Roles           := '';

  Response := FLogicServer.ExecuteRequest(ARequestJSON, ASessionID, AuthCtx);

  if Response <> '' then
    Session.EnqueueMessage(Response);
end;

end.
