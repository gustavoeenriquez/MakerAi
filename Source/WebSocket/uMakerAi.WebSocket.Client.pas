// MakerAI Suite — Cliente WebSocket RFC 6455 cross-platform
// Capa de protocolo: HTTP Upgrade + framing RFC 6455 (pure Pascal)
// Capa TLS: delegada a ITlsTransport (SChannel en Windows, OpenSSL en POSIX)
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.WebSocket.Client;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.NetEncoding, System.Math, System.IOUtils;

const
  // Poner ruta de archivo para habilitar log de diagnóstico, '' para deshabilitar
  WS_DIAG_LOG = '';

type
  // -----------------------------------------------------------------------
  // Interfaz de transporte TLS — implementada por SChannel (Windows) u OpenSSL (POSIX)
  // -----------------------------------------------------------------------
  ITlsTransport = interface
    ['{5C9A1F8E-4B2D-4A71-8E3F-1D5A9C7B2E4F}']
    // Conecta TCP + TLS al host:port. True = éxito.
    function  Connect(const AHost: string; APort: Integer): Boolean;
    // Cierra la conexión (libera socket y contexto TLS)
    procedure Disconnect;
    // Envía todos los bytes de AData cifrados. True = éxito.
    function  SendAll(const AData: TBytes): Boolean;
    // Recibe bytes descifrados. > 0 = bytes recibidos, 0 = timeout, -1 = error/cerrado
    function  RecvSome(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
    function  IsConnected: Boolean;
  end;

  // -----------------------------------------------------------------------
  // Tipos públicos del cliente WebSocket
  // -----------------------------------------------------------------------
  TAiWSOpcode = (
    rwsoContinuation = 0,
    rwsoText         = 1,
    rwsoBinary       = 2,
    rwsoClose        = 8,
    rwsoPing         = 9,
    rwsoPong         = 10
  );

  TAiWSFrameEvent = procedure(Sender: TObject; Opcode: TAiWSOpcode;
    const Data: TBytes; IsFinal: Boolean) of object;
  TAiWSErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;

  TAiWSClient = class;

  // -----------------------------------------------------------------------
  // TAiWSReaderThread — lee bytes del transporte y parsea frames RFC 6455
  // -----------------------------------------------------------------------
  TAiWSReaderThread = class(TThread)
  private
    FClient:     TAiWSClient;
    FStopping:   Boolean;
    FBuf:        TBytes;     // buffer acumulador de bytes recibidos
    FBufLen:     Integer;    // bytes válidos en FBuf
    FFragBuf:    TBytes;     // acumulador de fragmentos RFC 6455
    FFragOpcode: Byte;       // opcode del primer fragmento
    procedure AppendToBuf(const AData: TBytes; ALen: Integer);
    function  TryParseFrame: Boolean; // True = frame parseado y despachado
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TAiWSClient);
    procedure Stop;
  end;

  // -----------------------------------------------------------------------
  // TAiWSClient — cliente WebSocket sobre ITlsTransport
  // -----------------------------------------------------------------------
  TAiWSClient = class
  private
    FTransport:       ITlsTransport;
    FReaderThread:    TAiWSReaderThread;
    FLock:            TCriticalSection;
    FConnected:       Boolean;
    FUrl:             string;
    FHost:            string;
    FPort:            Integer;
    FPath:            string;
    FUseSSL:          Boolean;
    FExtraHeaders:    TStringList;
    FHandshakeBuf:    TBytes;    // buffer de lectura durante HTTP upgrade
    FHandshakeBufLen: Integer;   // bytes válidos (pasados al reader thread al inicio)
    FOnFrame:         TAiWSFrameEvent;
    FOnConnected:     TNotifyEvent;
    FOnDisconnected:  TNotifyEvent;
    FOnError:         TAiWSErrorEvent;
    procedure ParseUrl(const AUrl: string);
    function  DoHttpUpgrade: Boolean;
    function  ReadHttpLine: string;
    procedure SendFrame(Opcode: Byte; const Data: TBytes; IsFinal: Boolean);
    procedure SendPongNoLock(const Data: TBytes);
    procedure HandleIncomingFrame(Opcode: TAiWSOpcode; const Data: TBytes; IsFinal: Boolean);
    procedure HandleDisconnect;
    procedure HandleError(const Msg: string);
  public
    constructor Create;
    destructor  Destroy; override;
    function  Connect(const AUrl: string): Boolean;
    procedure Disconnect;
    procedure SendText(const AText: string);
    procedure SendBinary(const AData: TBytes);
    procedure SendPing;
    procedure SendClose(Code: Word; const Reason: string);
    property Connected:      Boolean           read FConnected;
    property Url:            string            read FUrl;
    property ExtraHeaders:   TStringList       read FExtraHeaders;
    property OnFrame:        TAiWSFrameEvent   read FOnFrame        write FOnFrame;
    property OnConnected:    TNotifyEvent      read FOnConnected    write FOnConnected;
    property OnDisconnected: TNotifyEvent      read FOnDisconnected write FOnDisconnected;
    property OnError:        TAiWSErrorEvent   read FOnError        write FOnError;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  uMakerAi.WebSocket.SChannel;
{$ELSEIF DEFINED(ANDROID)}
  uMakerAi.WebSocket.Android;
{$ELSE}
  uMakerAi.WebSocket.OpenSSL;
{$ENDIF}

// -----------------------------------------------------------------------
// Diagnóstico
// -----------------------------------------------------------------------

procedure WsDiag(const S: string);
begin
  if WS_DIAG_LOG <> '' then
    try TFile.AppendAllText(WS_DIAG_LOG, S + sLineBreak); except end;
end;

// -----------------------------------------------------------------------
// TAiWSReaderThread
// -----------------------------------------------------------------------

constructor TAiWSReaderThread.Create(AClient: TAiWSClient);
begin
  FClient         := AClient;
  FStopping       := False;
  FBufLen         := 0;
  FFragOpcode     := 0;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TAiWSReaderThread.Stop;
begin
  FStopping := True;
  Terminate;
end;

procedure TAiWSReaderThread.AppendToBuf(const AData: TBytes; ALen: Integer);
begin
  if ALen <= 0 then Exit;
  if FBufLen + ALen > Length(FBuf) then
    SetLength(FBuf, FBufLen + ALen + 65536);
  Move(AData[0], FBuf[FBufLen], ALen);
  Inc(FBufLen, ALen);
end;

// Parsea un frame RFC 6455 del buffer FBuf.
// Retorna True si se parseó y despachó un frame completo.
// Retorna False si faltan bytes.
function TAiWSReaderThread.TryParseFrame: Boolean;
var
  B0, B1:    Byte;
  IsFinal:   Boolean;
  Opcode:    Byte;
  IsMasked:  Boolean;
  PayLen:    Int64;
  HeaderLen: Integer;
  MaskKey:   array[0..3] of Byte;
  OutBuf:    TBytes;
  OldLen, i: Integer;
begin
  Result := False;
  if FBufLen < 2 then Exit;

  B0 := FBuf[0];
  B1 := FBuf[1];
  IsFinal   := (B0 and $80) <> 0;
  Opcode    := B0 and $0F;
  IsMasked  := (B1 and $80) <> 0;
  PayLen    := B1 and $7F;
  HeaderLen := 2;

  // Longitud extendida RFC 6455 sec.5.2
  if PayLen = 126 then
  begin
    if FBufLen < 4 then Exit;
    PayLen    := (Int64(FBuf[2]) shl 8) or FBuf[3];
    HeaderLen := 4;
  end
  else if PayLen = 127 then
  begin
    if FBufLen < 10 then Exit;
    PayLen := (Int64(FBuf[2]) shl 56) or (Int64(FBuf[3]) shl 48) or
              (Int64(FBuf[4]) shl 40) or (Int64(FBuf[5]) shl 32) or
              (Int64(FBuf[6]) shl 24) or (Int64(FBuf[7]) shl 16) or
              (Int64(FBuf[8]) shl  8) or  Int64(FBuf[9]);
    HeaderLen := 10;
    if PayLen > MaxInt then
    begin
      FStopping := True;
      Terminate;
      FClient.HandleError('WebSocket: frame demasiado grande');
      Exit;
    end;
  end;

  // Máscara de servidor (raro, pero RFC lo permite)
  if IsMasked then
  begin
    if FBufLen < HeaderLen + 4 then Exit;
    MaskKey[0] := FBuf[HeaderLen];
    MaskKey[1] := FBuf[HeaderLen + 1];
    MaskKey[2] := FBuf[HeaderLen + 2];
    MaskKey[3] := FBuf[HeaderLen + 3];
    Inc(HeaderLen, 4);
  end;

  if FBufLen < HeaderLen + Integer(PayLen) then Exit;

  // Frame completo — extraer payload
  SetLength(OutBuf, Integer(PayLen));
  if PayLen > 0 then
    Move(FBuf[HeaderLen], OutBuf[0], Integer(PayLen));

  if IsMasked and (PayLen > 0) then
    for i := 0 to Integer(PayLen) - 1 do
      OutBuf[i] := OutBuf[i] xor MaskKey[i mod 4];

  // Consumir frame del buffer
  Move(FBuf[HeaderLen + Integer(PayLen)], FBuf[0],
    FBufLen - HeaderLen - Integer(PayLen));
  Dec(FBufLen, HeaderLen + Integer(PayLen));

  Result := True;

  WsDiag('FRAME opcode=' + IntToStr(Opcode) + ' len=' + IntToStr(PayLen) + ' fin=' + BoolToStr(IsFinal, True));

  // Despachar frame
  case Opcode of
    $8: // Close
    begin
      FStopping := True;
      Terminate;
      FClient.HandleDisconnect;
    end;

    $9: // Ping → responder con Pong (RFC 6455 sec.5.5.2)
    begin
      FClient.FLock.Enter;
      try
        FClient.SendPongNoLock(OutBuf);
      finally
        FClient.FLock.Leave;
      end;
    end;

    $A: ; // Pong — ignorar

    $0: // Continuation frame
    begin
      OldLen := Length(FFragBuf);
      SetLength(FFragBuf, OldLen + Integer(PayLen));
      if PayLen > 0 then
        Move(OutBuf[0], FFragBuf[OldLen], Integer(PayLen));
      if IsFinal then
      begin
        FClient.HandleIncomingFrame(TAiWSOpcode(FFragOpcode), FFragBuf, True);
        SetLength(FFragBuf, 0);
        FFragOpcode := 0;
      end;
    end;

  else
    // Text ($1), Binary ($2) u opcode desconocido
    if IsFinal then
      FClient.HandleIncomingFrame(TAiWSOpcode(Opcode), OutBuf, True)
    else
    begin
      // Primer fragmento — guardar opcode e iniciar acumulación
      FFragOpcode := Opcode;
      FFragBuf    := Copy(OutBuf);
    end;
  end;
end;

procedure TAiWSReaderThread.Execute;
var
  NewData: TBytes;
  n:       Integer;
begin
  // Inicializar con bytes sobrantes del HTTP upgrade handshake
  FBufLen := FClient.FHandshakeBufLen;
  SetLength(FBuf, Max(FBufLen + 65536, 65536));
  if FBufLen > 0 then
    Move(FClient.FHandshakeBuf[0], FBuf[0], FBufLen);
  SetLength(FFragBuf, 0);
  FFragOpcode := 0;

  WsDiag('=== READER STARTED ' + DateTimeToStr(Now) + ' ===');

  while not Terminated do
  begin
    // Intentar parsear un frame con los bytes disponibles
    if TryParseFrame then
      Continue;

    // Faltan bytes — leer del transporte (timeout 100ms para poder chequear Terminated)
    n := FClient.FTransport.RecvSome(NewData, 100);
    if n = 0 then Continue; // timeout, reintento
    if n < 0 then Break;    // error o conexión cerrada
    AppendToBuf(NewData, n);
  end;

  // Si la salida no fue iniciada por Stop() ni por Close frame, notificar desconexión
  if not FStopping and not Terminated then
    FClient.HandleDisconnect;

  WsDiag('=== READER STOPPED ' + DateTimeToStr(Now) + ' ===');
end;

// -----------------------------------------------------------------------
// TAiWSClient
// -----------------------------------------------------------------------

constructor TAiWSClient.Create;
begin
  inherited;
  FLock         := TCriticalSection.Create;
  FExtraHeaders := TStringList.Create;
  FExtraHeaders.NameValueSeparator := ':';
  FConnected       := False;
  FHandshakeBufLen := 0;
end;

destructor TAiWSClient.Destroy;
begin
  Disconnect;
  FExtraHeaders.Free;
  FLock.Free;
  inherited;
end;

procedure TAiWSClient.ParseUrl(const AUrl: string);
var
  Rest, HostPort: string;
  SlashPos, ColonPos: Integer;
begin
  FUrl := AUrl;
  if AUrl.StartsWith('wss://') then
  begin
    FUseSSL := True;
    FPort   := 443;
    Rest    := Copy(AUrl, 7, MaxInt);
  end
  else if AUrl.StartsWith('ws://') then
  begin
    FUseSSL := False;
    FPort   := 80;
    Rest    := Copy(AUrl, 6, MaxInt);
  end
  else
    raise EArgumentException.Create('URL WebSocket inválida: ' + AUrl);

  SlashPos := Pos('/', Rest);
  if SlashPos > 0 then
  begin
    HostPort := Copy(Rest, 1, SlashPos - 1);
    FPath    := Copy(Rest, SlashPos, MaxInt);
  end
  else
  begin
    HostPort := Rest;
    FPath    := '/';
  end;

  ColonPos := Pos(':', HostPort);
  if ColonPos > 0 then
  begin
    FHost := Copy(HostPort, 1, ColonPos - 1);
    FPort := StrToIntDef(Copy(HostPort, ColonPos + 1, MaxInt), FPort);
  end
  else
    FHost := HostPort;
end;

// Lee una línea HTTP del transporte, acumulando en FHandshakeBuf
function TAiWSClient.ReadHttpLine: string;
var
  NewBuf:    TBytes;
  n, i, Pos: Integer;
begin
  while True do
  begin
    // Buscar \r\n en el buffer actual
    Pos := -1;
    for i := 0 to FHandshakeBufLen - 2 do
      if (FHandshakeBuf[i] = $0D) and (FHandshakeBuf[i + 1] = $0A) then
      begin
        Pos := i;
        Break;
      end;

    if Pos >= 0 then
    begin
      Result := TEncoding.ASCII.GetString(FHandshakeBuf, 0, Pos);
      // Desplazar bytes restantes al frente
      Move(FHandshakeBuf[Pos + 2], FHandshakeBuf[0], FHandshakeBufLen - Pos - 2);
      Dec(FHandshakeBufLen, Pos + 2);
      Exit;
    end;

    // Necesitamos más datos del transporte
    n := FTransport.RecvSome(NewBuf, 5000);
    if n <= 0 then
      raise Exception.Create('HTTP handshake: conexión cerrada o timeout');
    if FHandshakeBufLen + n > Length(FHandshakeBuf) then
      SetLength(FHandshakeBuf, FHandshakeBufLen + n + 4096);
    Move(NewBuf[0], FHandshakeBuf[FHandshakeBufLen], n);
    Inc(FHandshakeBufLen, n);
  end;
end;

function TAiWSClient.DoHttpUpgrade: Boolean;
var
  KeyBytes:  TBytes;
  KeyBase64: string;
  Handshake: string;
  SendBuf:   TBytes;
  StatusLine, Line: string;
  i: Integer;
begin
  Result := False;

  // Generar Sec-WebSocket-Key: 16 bytes aleatorios en base64 (RFC 6455 sec.4.1)
  SetLength(KeyBytes, 16);
  for i := 0 to 15 do KeyBytes[i] := Byte(Random($100));
  KeyBase64 := TNetEncoding.Base64.EncodeBytesToString(KeyBytes).Trim;

  // Construir petición HTTP de upgrade
  Handshake := 'GET ' + FPath + ' HTTP/1.1' + #13#10 +
               'Host: ' + FHost + #13#10 +
               'Upgrade: websocket' + #13#10 +
               'Connection: Upgrade' + #13#10 +
               'Sec-WebSocket-Key: ' + KeyBase64 + #13#10 +
               'Sec-WebSocket-Version: 13' + #13#10;
  for i := 0 to FExtraHeaders.Count - 1 do
    if FExtraHeaders.Names[i] <> '' then
      Handshake := Handshake + FExtraHeaders.Names[i] + ': ' +
                   FExtraHeaders.ValueFromIndex[i] + #13#10
    else if FExtraHeaders[i] <> '' then
      Handshake := Handshake + FExtraHeaders[i] + #13#10;
  Handshake := Handshake + #13#10;

  SendBuf := TEncoding.ASCII.GetBytes(Handshake);
  if not FTransport.SendAll(SendBuf) then
  begin
    HandleError('HTTP upgrade: envío fallido');
    Exit;
  end;

  // Inicializar buffer de handshake para ReadHttpLine
  FHandshakeBufLen := 0;
  SetLength(FHandshakeBuf, 8192);

  // Leer línea de status — debe contener "101"
  StatusLine := ReadHttpLine;
  WsDiag('HTTP Status: ' + StatusLine);
  if Pos('101', StatusLine) = 0 then
  begin
    HandleError('WS handshake fallido: ' + StatusLine);
    Exit;
  end;

  // Leer y descartar cabeceras hasta línea en blanco
  repeat
    Line := ReadHttpLine;
  until (Line = '') or (Line = #13);

  // FHandshakeBuf[0..FHandshakeBufLen-1] contiene bytes recibidos tras las cabeceras
  // (inicio de frames WebSocket) — el reader thread los consumirá primero

  WsDiag('HTTP Upgrade OK — ' + IntToStr(FHandshakeBufLen) + ' bytes sobrantes');
  Result := True;
end;

// Codifica y envía un frame RFC 6455 (cliente→servidor: MASK=1 obligatorio)
procedure TAiWSClient.SendFrame(Opcode: Byte; const Data: TBytes; IsFinal: Boolean);
var
  Header:   array[0..13] of Byte;
  HdrLen:   Integer;
  PayLen:   Integer;
  Mask:     array[0..3] of Byte;
  Masked:   TBytes;
  Frame:    TBytes;
  i:        Integer;
begin
  PayLen := Length(Data);
  HdrLen := 0;

  // Byte 0: FIN + RSV(000) + Opcode
  if IsFinal then Header[HdrLen] := $80 or (Opcode and $0F)
  else             Header[HdrLen] := Opcode and $0F;
  Inc(HdrLen);

  // Byte 1+: MASK=1 + PayloadLen
  if PayLen <= 125 then
  begin
    Header[HdrLen] := $80 or Byte(PayLen);
    Inc(HdrLen);
  end
  else if PayLen <= 65535 then
  begin
    Header[HdrLen] := $80 or 126;              Inc(HdrLen);
    Header[HdrLen] := (PayLen shr 8) and $FF;  Inc(HdrLen);
    Header[HdrLen] :=  PayLen and $FF;          Inc(HdrLen);
  end
  else
  begin
    Header[HdrLen] := $80 or 127;               Inc(HdrLen);
    Header[HdrLen] := 0;                         Inc(HdrLen);
    Header[HdrLen] := 0;                         Inc(HdrLen);
    Header[HdrLen] := 0;                         Inc(HdrLen);
    Header[HdrLen] := 0;                         Inc(HdrLen);
    Header[HdrLen] := (PayLen shr 24) and $FF;   Inc(HdrLen);
    Header[HdrLen] := (PayLen shr 16) and $FF;   Inc(HdrLen);
    Header[HdrLen] := (PayLen shr  8) and $FF;   Inc(HdrLen);
    Header[HdrLen] :=  PayLen and $FF;            Inc(HdrLen);
  end;

  // Clave de máscara: 4 bytes aleatorios (RFC 6455 sec.5.3)
  Mask[0] := Byte(Random($100)); Header[HdrLen] := Mask[0]; Inc(HdrLen);
  Mask[1] := Byte(Random($100)); Header[HdrLen] := Mask[1]; Inc(HdrLen);
  Mask[2] := Byte(Random($100)); Header[HdrLen] := Mask[2]; Inc(HdrLen);
  Mask[3] := Byte(Random($100)); Header[HdrLen] := Mask[3]; Inc(HdrLen);

  // Payload enmascarado
  SetLength(Masked, PayLen);
  for i := 0 to PayLen - 1 do
    Masked[i] := Data[i] xor Mask[i mod 4];

  // Ensamblar frame y enviar bajo lock
  SetLength(Frame, HdrLen + PayLen);
  Move(Header[0], Frame[0], HdrLen);
  if PayLen > 0 then Move(Masked[0], Frame[HdrLen], PayLen);

  FLock.Enter;
  try
    if not FConnected then Exit;
    FTransport.SendAll(Frame);
  finally
    FLock.Leave;
  end;
end;

// Pong de respuesta — llamar CON FLock ya tomado
procedure TAiWSClient.SendPongNoLock(const Data: TBytes);
var
  PayLen: Integer;
  Mask:   array[0..3] of Byte;
  Frame:  TBytes;
  i:      Integer;
begin
  if not FConnected then Exit;
  // Control frames: payload máximo 125 bytes (RFC 6455 sec.5.5)
  PayLen  := Min(Length(Data), 125);
  Mask[0] := Byte(Random($100));
  Mask[1] := Byte(Random($100));
  Mask[2] := Byte(Random($100));
  Mask[3] := Byte(Random($100));
  SetLength(Frame, 6 + PayLen);
  Frame[0] := $8A;                   // FIN=1, Opcode=Pong
  Frame[1] := $80 or Byte(PayLen);   // MASK=1 + longitud
  Frame[2] := Mask[0]; Frame[3] := Mask[1];
  Frame[4] := Mask[2]; Frame[5] := Mask[3];
  for i := 0 to PayLen - 1 do
    Frame[6 + i] := Data[i] xor Mask[i mod 4];
  FTransport.SendAll(Frame);
end;

procedure TAiWSClient.HandleIncomingFrame(Opcode: TAiWSOpcode; const Data: TBytes; IsFinal: Boolean);
begin
  if Assigned(FOnFrame) then
    TThread.Queue(nil, procedure begin
      if Assigned(FOnFrame) then FOnFrame(Self, Opcode, Data, IsFinal);
    end);
end;

procedure TAiWSClient.HandleError(const Msg: string);
begin
  WsDiag('ERROR: ' + Msg);
  if Assigned(FOnError) then
    TThread.Queue(nil, procedure begin
      if Assigned(FOnError) then FOnError(Self, Msg);
    end);
end;

procedure TAiWSClient.HandleDisconnect;
begin
  if not FConnected then Exit;
  FConnected := False;
  WsDiag('DISCONNECTED');
  if Assigned(FOnDisconnected) then
    TThread.Queue(nil, procedure begin
      if Assigned(FOnDisconnected) then FOnDisconnected(Self);
    end);
end;

function TAiWSClient.Connect(const AUrl: string): Boolean;
begin
  Result := False;
  if FConnected then Exit;

  WsDiag('=== CONNECT ' + DateTimeToStr(Now) + ' ' + AUrl + ' ===');

  try
    ParseUrl(AUrl);
    WsDiag('ParseUrl OK host=' + FHost + ' port=' + IntToStr(FPort));
  except
    on E: Exception do
    begin
      HandleError('ParseUrl: ' + E.Message);
      Exit;
    end;
  end;

  // Crear transporte TLS para la plataforma actual
  try
{$IFDEF MSWINDOWS}
    FTransport := TSChannelTransport.Create;
{$ELSEIF DEFINED(ANDROID)}
    FTransport := TAndroidSSLTransport.Create;
{$ELSE}
    FTransport := TOpenSSLTransport.Create;
{$ENDIF}
    WsDiag('Transport created OK');
  except
    on E: Exception do
    begin
      HandleError('Transport.Create: ' + E.Message);
      Exit;
    end;
  end;

  // TCP + TLS
  if not FTransport.Connect(FHost, FPort) then
  begin
    HandleError('TLS connect fallido: ' + FHost + ':' + IntToStr(FPort));
    FTransport := nil;
    Exit;
  end;

  // HTTP WebSocket upgrade
  try
    if not DoHttpUpgrade then
    begin
      FTransport.Disconnect;
      FTransport := nil;
      Exit;
    end;
  except
    on E: Exception do
    begin
      HandleError('HTTP handshake: ' + E.Message);
      FTransport.Disconnect;
      FTransport := nil;
      Exit;
    end;
  end;

  FConnected    := True;
  FReaderThread := TAiWSReaderThread.Create(Self);
  Result        := True;

  WsDiag('CONNECT OK');

  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TAiWSClient.Disconnect;
begin
  FLock.Enter;
  try
    FConnected := False;
  finally
    FLock.Leave;
  end;

  if Assigned(FReaderThread) then
  begin
    FReaderThread.Stop;
    // Cerrar transporte para desbloquear RecvSome en el reader thread
    if Assigned(FTransport) then
      FTransport.Disconnect;
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
  end
  else if Assigned(FTransport) then
    FTransport.Disconnect;

  FTransport := nil; // libera via refcount
end;

procedure TAiWSClient.SendText(const AText: string);
begin
  SendFrame($1, TEncoding.UTF8.GetBytes(AText), True);
end;

procedure TAiWSClient.SendBinary(const AData: TBytes);
begin
  SendFrame($2, AData, True);
end;

procedure TAiWSClient.SendPing;
var
  Empty: TBytes;
begin
  SetLength(Empty, 0);
  SendFrame($9, Empty, True);
end;

procedure TAiWSClient.SendClose(Code: Word; const Reason: string);
var
  Payload, RBytes: TBytes;
begin
  RBytes := TEncoding.UTF8.GetBytes(Reason);
  SetLength(Payload, 2 + Length(RBytes));
  Payload[0] := (Code shr 8) and $FF;
  Payload[1] :=  Code and $FF;
  if Length(RBytes) > 0 then
    Move(RBytes[0], Payload[2], Length(RBytes));
  SendFrame($8, Payload, True);
end;

end.
