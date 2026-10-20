unit uMakerAi.WebSocket.Plain;

{
  TPlainTransport — ITlsTransport SIN cifrado, para URLs ws:// (un servidor
  MakerAI local o de staging en HTTP plano). Hasta ahora el cliente abria TLS
  tambien con ws://: contra un servidor HTTP el handshake TLS no recibe nunca
  respuesta y la conexion quedaba colgada para siempre, sin error.

  Windows: ws2_32 (mismo patron que uMakerAi.WebSocket.SChannel).
  POSIX (Linux, macOS, Android): sockets POSIX.
}

interface

uses
  System.SysUtils,
  uMakerAi.WebSocket.Client;

type
  TPlainTransport = class(TInterfacedObject, ITlsTransport)
  private
    FSocket:    NativeInt;   // -1 = sin conectar
    FConnected: Boolean;
    procedure CloseSocket;
  public
    constructor Create;
    destructor  Destroy; override;
    // ITlsTransport
    function  Connect(const AHost: string; APort: Integer): Boolean;
    procedure Disconnect;
    function  SendAll(const AData: TBytes): Boolean;
    function  RecvSome(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
    function  IsConnected: Boolean;
  end;

implementation

{$IFDEF MSWINDOWS}

uses
  Winapi.Windows;

const
  WSVERS        = $0202;
  AF_UNSPEC     = 0;
  SOCK_STREAM   = 1;
  IPPROTO_TCP   = 6;
  WS_INVAL_SOCK = NativeUInt(not NativeUInt(0));

type
  TWsAddrInfo = record
    ai_flags:     Integer;
    ai_family:    Integer;
    ai_socktype:  Integer;
    ai_protocol:  Integer;
    ai_addrlen:   NativeUInt;
    ai_canonname: PAnsiChar;
    ai_addr:      Pointer;
    ai_next:      Pointer;
  end;
  PWsAddrInfo  = ^TWsAddrInfo;
  PPWsAddrInfo = ^PWsAddrInfo;

  TWsData = record
    wVersion, wHighVersion: Word;
    szDescription:  array[0..256] of AnsiChar;
    szSystemStatus: array[0..128] of AnsiChar;
    iMaxSockets, iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  end;

  TWsFdSet = record
    fd_count: Cardinal;
    fd_array: array[0..63] of NativeUInt;
  end;
  TWsTimeval = record tv_sec, tv_usec: Longint; end;

function WsWSAStartup(wVer: Word; var d: TWsData): Integer; stdcall;
  external 'ws2_32.dll' name 'WSAStartup';
function WsWSACleanup: Integer; stdcall;
  external 'ws2_32.dll' name 'WSACleanup';
function WsSocket(af, stype, proto: Integer): NativeUInt; stdcall;
  external 'ws2_32.dll' name 'socket';
function WsConnect(s: NativeUInt; addr: Pointer; addrlen: Integer): Integer; stdcall;
  external 'ws2_32.dll' name 'connect';
function WsSend(s: NativeUInt; buf: Pointer; len, flags: Integer): Integer; stdcall;
  external 'ws2_32.dll' name 'send';
function WsRecv(s: NativeUInt; buf: Pointer; len, flags: Integer): Integer; stdcall;
  external 'ws2_32.dll' name 'recv';
function WsCloseSocket(s: NativeUInt): Integer; stdcall;
  external 'ws2_32.dll' name 'closesocket';
function WsGetAddrInfo(node, svc: PAnsiChar; hints: PWsAddrInfo; res: PPWsAddrInfo): Integer; stdcall;
  external 'ws2_32.dll' name 'getaddrinfo';
procedure WsFreeAddrInfo(ai: PWsAddrInfo); stdcall;
  external 'ws2_32.dll' name 'freeaddrinfo';
function WsSelect(nfds: Integer; rfds, wfds, efds: Pointer; tv: Pointer): Integer; stdcall;
  external 'ws2_32.dll' name 'select';

{$ELSE}

uses
  Posix.SysSocket,
  Posix.NetDB,
  Posix.SysSelect,
  Posix.SysTime,
  Posix.Unistd;

const
  // Plazo de connect() y de cada send(): un servidor que no contesta da
  // error en vez de colgar el hilo de conexion
  IO_TIMEOUT_SEC = 10;
{$IFDEF LINUX}
  // send() a un socket cerrado por el otro lado: error, no SIGPIPE
  SEND_FLAGS = $4000;   // MSG_NOSIGNAL
{$ELSE}
  SEND_FLAGS = 0;
{$ENDIF}

{$ENDIF}

{ TPlainTransport }

constructor TPlainTransport.Create;
begin
  inherited;
  FSocket := -1;
end;

destructor TPlainTransport.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TPlainTransport.CloseSocket;
begin
  if FSocket < 0 then Exit;
{$IFDEF MSWINDOWS}
  WsCloseSocket(NativeUInt(FSocket));
{$ELSE}
  __close(Integer(FSocket));
{$ENDIF}
  FSocket := -1;
end;

function TPlainTransport.Connect(const AHost: string; APort: Integer): Boolean;
{$IFDEF MSWINDOWS}
var
  Hints:    TWsAddrInfo;
  AddrList: PWsAddrInfo;
  Addr:     PWsAddrInfo;
  S:        NativeUInt;
begin
  FConnected := False;
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family   := AF_UNSPEC;
  Hints.ai_socktype := SOCK_STREAM;
  Hints.ai_protocol := IPPROTO_TCP;
  if WsGetAddrInfo(PAnsiChar(AnsiString(AHost)), PAnsiChar(AnsiString(IntToStr(APort))),
       @Hints, @AddrList) <> 0 then
    raise Exception.CreateFmt('ws: no se pudo resolver %s', [AHost]);
  try
    Addr := AddrList;
    while Addr <> nil do
    begin
      S := WsSocket(Addr.ai_family, Addr.ai_socktype, Addr.ai_protocol);
      if S <> WS_INVAL_SOCK then
      begin
        if WsConnect(S, Addr.ai_addr, Integer(Addr.ai_addrlen)) = 0 then
        begin
          FSocket := NativeInt(S);
          Break;
        end;
        WsCloseSocket(S);
      end;
      Addr := PWsAddrInfo(Addr.ai_next);
    end;
  finally
    WsFreeAddrInfo(AddrList);
  end;
  if FSocket < 0 then
    raise Exception.CreateFmt('ws: no se pudo conectar TCP a %s:%d', [AHost, APort]);
  FConnected := True;
  Result := True;
end;
{$ELSE}
var
  Hints:  addrinfo;
  Res, Cur: Paddrinfo;
  Fd:     Integer;
  TV:     timeval;
begin
  FConnected := False;
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family   := AF_UNSPEC;
  Hints.ai_socktype := SOCK_STREAM;
  if getaddrinfo(PAnsiChar(AnsiString(AHost)), PAnsiChar(AnsiString(IntToStr(APort))),
       Hints, Res) <> 0 then
    raise Exception.CreateFmt('ws: no se pudo resolver %s', [AHost]);
  try
    Cur := Res;
    while Cur <> nil do
    begin
      Fd := Posix.SysSocket.socket(Cur^.ai_family, Cur^.ai_socktype, Cur^.ai_protocol);
      if Fd >= 0 then
      begin
        // En Linux SO_SNDTIMEO tambien acota connect()
        TV.tv_sec  := IO_TIMEOUT_SEC;
        TV.tv_usec := 0;
        setsockopt(Fd, SOL_SOCKET, SO_SNDTIMEO, TV, SizeOf(TV));
        if Posix.SysSocket.connect(Fd, Cur^.ai_addr^, Cur^.ai_addrlen) = 0 then
        begin
          FSocket := Fd;
          Break;
        end;
        __close(Fd);
      end;
      Cur := Cur^.ai_next;
    end;
  finally
    freeaddrinfo(Res^);
  end;
  if FSocket < 0 then
    raise Exception.CreateFmt('ws: no se pudo conectar TCP a %s:%d', [AHost, APort]);
  FConnected := True;
  Result := True;
end;
{$ENDIF}

procedure TPlainTransport.Disconnect;
begin
  FConnected := False;
  CloseSocket;
end;

function TPlainTransport.SendAll(const AData: TBytes): Boolean;
var
  Offset, N: Integer;
begin
  Result := False;
  if (FSocket < 0) or (Length(AData) = 0) then Exit;
  Offset := 0;
  while Offset < Length(AData) do
  begin
{$IFDEF MSWINDOWS}
    N := WsSend(NativeUInt(FSocket), @AData[Offset], Length(AData) - Offset, 0);
{$ELSE}
    N := send(Integer(FSocket), AData[Offset], Length(AData) - Offset, SEND_FLAGS);
{$ENDIF}
    if N <= 0 then Exit;
    Inc(Offset, N);
  end;
  Result := True;
end;

// >0 = bytes leidos, 0 = timeout, -1 = error o cierre
function TPlainTransport.RecvSome(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
var
{$IFDEF MSWINDOWS}
  FdSet: TWsFdSet;
  TV:    TWsTimeval;
{$ELSE}
  FdSet: fd_set;
  TV:    timeval;
{$ENDIF}
  SR: Integer;
begin
  Result := -1;
  if FSocket < 0 then Exit;
  TV.tv_sec  := ATimeoutMs div 1000;
  TV.tv_usec := (ATimeoutMs mod 1000) * 1000;
{$IFDEF MSWINDOWS}
  FdSet.fd_count    := 1;
  FdSet.fd_array[0] := NativeUInt(FSocket);
  SR := WsSelect(0, @FdSet, nil, nil, @TV);
{$ELSE}
  __FD_ZERO(FdSet);
  __FD_SET(Integer(FSocket), FdSet);
  SR := select(Integer(FSocket) + 1, @FdSet, nil, nil, @TV);
{$ENDIF}
  if SR = 0 then Exit(0);
  if SR < 0 then Exit(-1);
  SetLength(ABuf, 32768);
{$IFDEF MSWINDOWS}
  Result := WsRecv(NativeUInt(FSocket), @ABuf[0], Length(ABuf), 0);
{$ELSE}
  Result := recv(Integer(FSocket), ABuf[0], Length(ABuf), 0);
{$ENDIF}
  if Result > 0 then
    SetLength(ABuf, Result)
  else
  begin
    SetLength(ABuf, 0);
    Result := -1;   // 0 = cierre ordenado del otro lado
  end;
end;

function TPlainTransport.IsConnected: Boolean;
begin
  Result := FConnected and (FSocket >= 0);
end;

{$IFDEF MSWINDOWS}
var
  GWsaData: TWsData;

initialization
  WsWSAStartup(WSVERS, GWsaData);

finalization
  WsWSACleanup;
{$ENDIF}

end.
