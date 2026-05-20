// MakerAI Suite — Transporte TLS Windows via SChannel (secur32.dll)
// TCP  : Winsock2 (ws2_32.dll) — socket/connect/send/recv/closesocket
// TLS  : SChannel SSPI — AcquireCredentialsHandle / InitializeSecurityContext
//                        EncryptMessage / DecryptMessage / QueryContextAttributes
// NO usa WinHTTP. NO usa Indy. NO usa OpenSSL.
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.WebSocket.SChannel;

{$IFDEF MSWINDOWS}

interface

uses
  System.SysUtils, System.Math, System.IOUtils, Winapi.Windows,
  uMakerAi.WebSocket.Client;

type
  TSChannelTransport = class(TInterfacedObject, ITlsTransport)
  private
    // ---- Winsock ----
    FSocket:    NativeUInt;   // SOCKET handle (NativeUInt = Winapi.Winsock2.TSocket)
    // ---- SChannel ----
    FhCred:     record dwLower, dwUpper: NativeUInt; end; // TCredHandle
    FhCtx:      record dwLower, dwUpper: NativeUInt; end; // TCtxtHandle
    FStreamSizes: record                                    // SecPkgContext_StreamSizes
      cbHeader, cbTrailer, cbMaximumMessage, cBuffers, cbBlockSize: Cardinal;
    end;
    FCredOK:    Boolean;
    FCtxOK:     Boolean;
    // ---- Buffers de recepción ----
    FRawBuf:    TBytes;   // bytes TCP cifrados recibidos pero no descifrados
    FRawBufLen: Integer;
    FDecBuf:    TBytes;   // bytes descifrados pendientes de entregar
    FDecBufLen: Integer;
    // ---- Estado ----
    FConnected: Boolean;
    FHost:      string;
    // ---- Helpers internos ----
    function  InitCredentials: Boolean;
    function  TcpConnect(const AHost: string; APort: Integer): Boolean;
    function  DoTlsHandshake: Boolean;
    function  RecvRaw(Buf: Pointer; BufSize: Integer; TimeoutMs: Integer): Integer;
    function  SendRawAll(Buf: Pointer; Len: Integer): Boolean;
    function  EncryptAndSendChunk(const Data: TBytes; Offset, Len: Integer): Boolean;
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

// ═══════════════════════════════════════════════════════════════════════
// Declaraciones Winsock2 (ws2_32.dll) — patrón sin Winapi.Winsock2
// ═══════════════════════════════════════════════════════════════════════

const
  WSVERS        = $0202;
  AF_UNSPEC     = 0;
  AF_INET       = 2;
  AF_INET6      = 23;
  SOCK_STREAM   = 1;
  IPPROTO_TCP   = 6;
  WS_INVAL_SOCK = NativeUInt(not NativeUInt(0)); // INVALID_SOCKET
  WS_SOC_ERR    = -1;

type
  TWsSockAddrIn = packed record
    sin_family: Word;
    sin_port:   Word;
    sin_addr:   DWORD;
    sin_zero:   array[0..7] of Byte;
  end;

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
function WsWSAGetLastError: Integer; stdcall;
  external 'ws2_32.dll' name 'WSAGetLastError';
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

// ═══════════════════════════════════════════════════════════════════════
// Declaraciones SChannel / SSPI (secur32.dll)
// ═══════════════════════════════════════════════════════════════════════

const
  UNISP_NAME_W             = 'Microsoft Unified Security Protocol Provider';
  SCHANNEL_CRED_VERSION    = 4;
  SP_PROT_TLS1_2_CLIENT    = $00000800;
  SP_PROT_TLS1_3_CLIENT    = $00002000;
  SCH_CRED_NO_DEFAULT_CREDS       = $00000010;
  SCH_CRED_MANUAL_CRED_VALIDATION = $00000008;
  SCH_USE_STRONG_CRYPTO           = $00000400;
  SECPKG_CRED_OUTBOUND     = 2;
  SECPKG_ATTR_STREAM_SIZES = 4;
  SECBUFFER_VERSION        = 0;
  SECBUFFER_EMPTY          = 0;
  SECBUFFER_DATA           = 1;
  SECBUFFER_TOKEN          = 2;
  SECBUFFER_EXTRA          = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER  = 7;
  SEC_E_OK                  = Integer($00000000);
  SEC_I_CONTINUE_NEEDED     = Integer($00090312);
  SEC_E_INCOMPLETE_MESSAGE  = Integer($80090318);
  SEC_I_CONTEXT_EXPIRED     = Integer($00090317);
  SEC_I_RENEGOTIATE         = Integer($00090321);
  ISC_REQ_SEQUENCE_DETECT   = $00000008;
  ISC_REQ_REPLAY_DETECT     = $00000004;
  ISC_REQ_CONFIDENTIALITY   = $00000010;
  ISC_REQ_EXTENDED_ERROR    = $00004000;
  ISC_REQ_ALLOCATE_MEMORY   = $00000100;
  ISC_REQ_STREAM            = $00008000;
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  ISC_FLAGS = ISC_REQ_SEQUENCE_DETECT  or ISC_REQ_REPLAY_DETECT or
              ISC_REQ_CONFIDENTIALITY  or ISC_REQ_EXTENDED_ERROR or
              ISC_REQ_ALLOCATE_MEMORY  or ISC_REQ_STREAM         or
              ISC_REQ_MANUAL_CRED_VALIDATION;

type
  TScHandle  = record dwLower, dwUpper: NativeUInt; end;
  PScHandle  = ^TScHandle;

  TSecBuffer = record
    cbBuffer:   Cardinal;
    BufferType: Cardinal;
    pvBuffer:   Pointer;
  end;
  PSecBuffer = ^TSecBuffer;

  TSecBufferDesc = record
    ulVersion: Cardinal;
    cBuffers:  Cardinal;
    pBuffers:  PSecBuffer;
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  TSchannelCred = record
    dwVersion, cCreds:                  DWORD;
    paCred, hRootStore:                 Pointer;
    cMappers:                           DWORD;
    aphMappers:                         Pointer;
    cSupportedAlgs:                     DWORD;
    palgSupportedAlgs:                  Pointer;
    grbitEnabledProtocols,
    dwMinimumCipherStrength,
    dwMaximumCipherStrength,
    dwSessionLifespan,
    dwFlags, dwCredFormat:              DWORD;
  end;

function ScAcquireCredentialsHandleW(pszPrincipal, pszPackage: PWideChar;
  fCredentialUse: Cardinal; pvLogonId, pAuthData, pGetKeyFn,
  pvGetKeyArgument: Pointer; phCredential: PScHandle; ptsExpiry: Pointer): Integer; stdcall;
  external 'secur32.dll' name 'AcquireCredentialsHandleW';
function ScFreeCredentialsHandle(phCredential: PScHandle): Integer; stdcall;
  external 'secur32.dll' name 'FreeCredentialsHandle';
function ScInitializeSecurityContextW(phCredential, phContext: PScHandle;
  pszTargetName: PWideChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
  pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PScHandle;
  pOutput: PSecBufferDesc; pfContextAttr: PCardinal; ptsExpiry: Pointer): Integer; stdcall;
  external 'secur32.dll' name 'InitializeSecurityContextW';
function ScDeleteSecurityContext(phContext: PScHandle): Integer; stdcall;
  external 'secur32.dll' name 'DeleteSecurityContext';
function ScEncryptMessage(phContext: PScHandle; fQOP: Cardinal;
  pMessage: PSecBufferDesc; MessageSeqNo: Cardinal): Integer; stdcall;
  external 'secur32.dll' name 'EncryptMessage';
function ScDecryptMessage(phContext: PScHandle; pMessage: PSecBufferDesc;
  MessageSeqNo: Cardinal; pfQOP: PCardinal): Integer; stdcall;
  external 'secur32.dll' name 'DecryptMessage';
function ScQueryContextAttributesW(phContext: PScHandle; ulAttribute: Cardinal;
  pBuffer: Pointer): Integer; stdcall;
  external 'secur32.dll' name 'QueryContextAttributesW';
function ScFreeContextBuffer(pvContextBuffer: Pointer): Integer; stdcall;
  external 'secur32.dll' name 'FreeContextBuffer';

// ═══════════════════════════════════════════════════════════════════════
// TSChannelTransport — implementación
// ═══════════════════════════════════════════════════════════════════════

constructor TSChannelTransport.Create;
begin
  inherited;
  FSocket     := WS_INVAL_SOCK;
  FCredOK     := False;
  FCtxOK      := False;
  FConnected  := False;
  FRawBufLen  := 0;
  FDecBufLen  := 0;
  SetLength(FRawBuf, 65536);
  SetLength(FDecBuf, 65536);
end;

destructor TSChannelTransport.Destroy;
begin
  Disconnect;
  inherited;
end;

function TSChannelTransport.IsConnected: Boolean;
begin
  Result := FConnected;
end;

// -----------------------------------------------------------------------
// TCP helpers
// -----------------------------------------------------------------------

function TSChannelTransport.TcpConnect(const AHost: string; APort: Integer): Boolean;
var
  Hints:    TWsAddrInfo;
  AddrList: PWsAddrInfo;
  Addr:     PWsAddrInfo;
  HostA:    AnsiString;
  PortA:    AnsiString;
begin
  Result := False;
  HostA  := AnsiString(AHost);
  PortA  := AnsiString(IntToStr(APort));

  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family   := AF_UNSPEC;
  Hints.ai_socktype := SOCK_STREAM;
  Hints.ai_protocol := IPPROTO_TCP;

  if WsGetAddrInfo(PAnsiChar(HostA), PAnsiChar(PortA), @Hints, @AddrList) <> 0 then
    Exit;
  try
    Addr := AddrList;
    while Addr <> nil do
    begin
      FSocket := WsSocket(Addr.ai_family, Addr.ai_socktype, Addr.ai_protocol);
      if FSocket = WS_INVAL_SOCK then
      begin
        Addr := PWsAddrInfo(Addr.ai_next);
        Continue;
      end;
      if WsConnect(FSocket, Addr.ai_addr, Integer(Addr.ai_addrlen)) = 0 then
      begin
        Result := True;
        Break;
      end;
      WsCloseSocket(FSocket);
      FSocket := WS_INVAL_SOCK;
      Addr := PWsAddrInfo(Addr.ai_next);
    end;
  finally
    WsFreeAddrInfo(AddrList);
  end;
end;

// Envío TCP bloqueante de exactamente Len bytes
function TSChannelTransport.SendRawAll(Buf: Pointer; Len: Integer): Boolean;
var
  Sent, n: Integer;
begin
  Sent := 0;
  while Sent < Len do
  begin
    n := WsSend(FSocket, PByte(NativeUInt(Buf) + NativeUInt(Sent)), Len - Sent, 0);
    if n <= 0 then
    begin
      Result := False;
      Exit;
    end;
    Inc(Sent, n);
  end;
  Result := True;
end;

// Recepción TCP con timeout opcional (ms). Retorna: >0=bytes, 0=timeout, -1=error/cerrado
function TSChannelTransport.RecvRaw(Buf: Pointer; BufSize: Integer; TimeoutMs: Integer): Integer;
var
  FdSet: TWsFdSet;
  tv:    TWsTimeval;
  n:     Integer;
begin
  if TimeoutMs > 0 then
  begin
    tv.tv_sec  := TimeoutMs div 1000;
    tv.tv_usec := (TimeoutMs mod 1000) * 1000;
    FdSet.fd_count    := 1;
    FdSet.fd_array[0] := FSocket;
    n := WsSelect(0, @FdSet, nil, nil, @tv);
    if n = 0 then begin Result := 0; Exit; end;
    if n < 0 then begin Result := -1; Exit; end;
  end;
  Result := WsRecv(FSocket, Buf, BufSize, 0);
  if Result = 0  then Result := -1; // conexión cerrada limpiamente
  if Result < 0  then Result := -1; // error de socket
end;

// -----------------------------------------------------------------------
// SChannel — inicializar credenciales
// -----------------------------------------------------------------------

function TSChannelTransport.InitCredentials: Boolean;
var
  Cred: TSchannelCred;
  Status: Integer;
begin
  FillChar(Cred, SizeOf(Cred), 0);
  Cred.dwVersion             := SCHANNEL_CRED_VERSION;
  Cred.grbitEnabledProtocols := SP_PROT_TLS1_2_CLIENT or SP_PROT_TLS1_3_CLIENT;
  Cred.dwFlags               := SCH_CRED_NO_DEFAULT_CREDS or
                                 SCH_CRED_MANUAL_CRED_VALIDATION or
                                 SCH_USE_STRONG_CRYPTO;

  Status := ScAcquireCredentialsHandleW(nil, UNISP_NAME_W,
    SECPKG_CRED_OUTBOUND, nil, @Cred, nil, nil,
    PScHandle(@FhCred), nil);
  Result := Status = SEC_E_OK;
  if Result then FCredOK := True;
end;

// -----------------------------------------------------------------------
// Diagnóstico SChannel (escribe a C:\Temp\schannel_diag.txt)
// -----------------------------------------------------------------------

procedure ScDiag(const S: string);
const
  LOG = 'C:\Temp\schannel_diag.txt';
begin
  try System.IOUtils.TFile.AppendAllText(LOG, S + sLineBreak); except end;
end;

// -----------------------------------------------------------------------
// SChannel — handshake TLS
// -----------------------------------------------------------------------

function TSChannelTransport.DoTlsHandshake: Boolean;
const
  READ_CHUNK = 16384;
var
  Status:      Integer;
  InBuf:       array[0..1] of TSecBuffer;
  OutBuf:      TSecBuffer;
  InDesc:      TSecBufferDesc;
  OutDesc:     TSecBufferDesc;
  CtxAttr:     Cardinal;
  InDataLen:   Integer;
  ReadBuf:     TBytes;
  n, ExtraLen: Integer;
  FirstCall:   Boolean;
  j:           Integer;
begin
  Result    := False;
  FirstCall := True;
  InDataLen := 0;
  SetLength(ReadBuf, READ_CHUNK * 4);

  // Descriptor de salida (SChannel aloca con ISC_REQ_ALLOCATE_MEMORY)
  OutDesc.ulVersion := SECBUFFER_VERSION;
  OutDesc.cBuffers  := 1;
  OutDesc.pBuffers  := @OutBuf;

  // Descriptor de entrada
  InDesc.ulVersion := SECBUFFER_VERSION;
  InDesc.cBuffers  := 2;
  InDesc.pBuffers  := @InBuf[0];

  repeat
    OutBuf.BufferType := SECBUFFER_TOKEN;
    OutBuf.cbBuffer   := 0;
    OutBuf.pvBuffer   := nil;

    if FirstCall then
    begin
      // Primera llamada: sin input → genera ClientHello
      Status := ScInitializeSecurityContextW(
        PScHandle(@FhCred), nil, PWideChar(FHost),
        ISC_FLAGS, 0, 0, nil, 0,
        PScHandle(@FhCtx), @OutDesc, @CtxAttr, nil);
      FCtxOK    := True;
      FirstCall := False;
    end
    else
    begin
      InBuf[0].BufferType := SECBUFFER_TOKEN;
      InBuf[0].cbBuffer   := InDataLen;
      InBuf[0].pvBuffer   := @ReadBuf[0];
      InBuf[1].BufferType := SECBUFFER_EMPTY;
      InBuf[1].cbBuffer   := 0;
      InBuf[1].pvBuffer   := nil;

      Status := ScInitializeSecurityContextW(
        PScHandle(@FhCred), PScHandle(@FhCtx), nil,
        ISC_FLAGS, 0, 0, @InDesc, 0,
        nil, @OutDesc, @CtxAttr, nil);
    end;

    // Enviar output al servidor si lo hay
    if (OutBuf.pvBuffer <> nil) and (OutBuf.cbBuffer > 0) then
    begin
      if not SendRawAll(OutBuf.pvBuffer, OutBuf.cbBuffer) then
        ScDiag('WARN: SendRawAll failed during handshake');
      ScFreeContextBuffer(OutBuf.pvBuffer);
      OutBuf.pvBuffer := nil;
    end;

    ScDiag('ISC status=0x' + IntToHex(Cardinal(Status), 8) + ' first=' + BoolToStr(FirstCall, True) + ' inLen=' + IntToStr(InDataLen));

    case Status of

      SEC_E_OK:
      begin
        // Handshake completo — guardar bytes EXTRA (inicio de datos de aplicación)
        for j := 0 to 1 do
          if (not FirstCall) and (InBuf[j].BufferType = SECBUFFER_EXTRA) then
          begin
            ExtraLen := InBuf[j].cbBuffer;
            if ExtraLen > 0 then
            begin
              if ExtraLen > Length(FRawBuf) then
                SetLength(FRawBuf, ExtraLen + 65536);
              Move(ReadBuf[InDataLen - ExtraLen], FRawBuf[0], ExtraLen);
              FRawBufLen := ExtraLen;
            end;
            Break;
          end;
        // Obtener tamaños de header/trailer para EncryptMessage
        ScQueryContextAttributesW(PScHandle(@FhCtx),
          SECPKG_ATTR_STREAM_SIZES, @FStreamSizes);
        Result := True;
        Break;
      end;

      SEC_I_CONTINUE_NEEDED:
      begin
        // Gestionar EXTRA del input — bytes no consumidos por SChannel
        ExtraLen := 0;
        for j := 0 to 1 do
          if InBuf[j].BufferType = SECBUFFER_EXTRA then
          begin
            ExtraLen := InBuf[j].cbBuffer;
            Break;
          end;
        if ExtraLen > 0 then
        begin
          Move(ReadBuf[InDataLen - ExtraLen], ReadBuf[0], ExtraLen);
          InDataLen := ExtraLen;
        end
        else
          InDataLen := 0;

        // Leer más datos del servidor
        if Length(ReadBuf) - InDataLen < READ_CHUNK then
          SetLength(ReadBuf, InDataLen + READ_CHUNK * 2);
        n := RecvRaw(@ReadBuf[InDataLen], Length(ReadBuf) - InDataLen, 15000);
        if n <= 0 then
        begin
          ScDiag('FAIL: RecvRaw=' + IntToStr(n) + ' in CONTINUE, inDataLen=' + IntToStr(InDataLen));
          Exit;
        end;
        Inc(InDataLen, n);
      end;

      SEC_E_INCOMPLETE_MESSAGE:
      begin
        // Input incompleto — leer más TCP sin mover los bytes existentes
        if Length(ReadBuf) - InDataLen < READ_CHUNK then
          SetLength(ReadBuf, InDataLen + READ_CHUNK * 2);
        n := RecvRaw(@ReadBuf[InDataLen], Length(ReadBuf) - InDataLen, 15000);
        if n <= 0 then
        begin
          ScDiag('FAIL: RecvRaw=' + IntToStr(n) + ' in INCOMPLETE, inDataLen=' + IntToStr(InDataLen));
          Exit;
        end;
        Inc(InDataLen, n);
      end;

    else
      ScDiag('FAIL: ISC error 0x' + IntToHex(Cardinal(Status), 8));
      Exit; // Error de handshake
    end;

  until False;
end;

// -----------------------------------------------------------------------
// EncryptMessage + send para un chunk (<= cbMaximumMessage)
// -----------------------------------------------------------------------

function TSChannelTransport.EncryptAndSendChunk(const Data: TBytes; Offset, Len: Integer): Boolean;
var
  Buffers: array[0..3] of TSecBuffer;
  BufDesc: TSecBufferDesc;
  MsgBuf:  TBytes;
  Status:  Integer;
  TotalSend: Integer;
begin
  Result := False;

  SetLength(MsgBuf, FStreamSizes.cbHeader + Cardinal(Len) + FStreamSizes.cbTrailer);
  if Len > 0 then
    Move(Data[Offset], MsgBuf[FStreamSizes.cbHeader], Len);

  Buffers[0].BufferType := SECBUFFER_STREAM_HEADER;
  Buffers[0].cbBuffer   := FStreamSizes.cbHeader;
  Buffers[0].pvBuffer   := @MsgBuf[0];

  Buffers[1].BufferType := SECBUFFER_DATA;
  Buffers[1].cbBuffer   := Len;
  Buffers[1].pvBuffer   := @MsgBuf[FStreamSizes.cbHeader];

  Buffers[2].BufferType := SECBUFFER_STREAM_TRAILER;
  Buffers[2].cbBuffer   := FStreamSizes.cbTrailer;
  Buffers[2].pvBuffer   := @MsgBuf[FStreamSizes.cbHeader + Cardinal(Len)];

  Buffers[3].BufferType := SECBUFFER_EMPTY;
  Buffers[3].cbBuffer   := 0;
  Buffers[3].pvBuffer   := nil;

  BufDesc.ulVersion := SECBUFFER_VERSION;
  BufDesc.cBuffers  := 4;
  BufDesc.pBuffers  := @Buffers[0];

  Status := ScEncryptMessage(PScHandle(@FhCtx), 0, @BufDesc, 0);
  if Status <> SEC_E_OK then Exit;

  // Los cbBuffer pueden haber cambiado — enviar exactamente lo que SChannel cifró
  TotalSend := Integer(Buffers[0].cbBuffer) +
               Integer(Buffers[1].cbBuffer) +
               Integer(Buffers[2].cbBuffer);
  Result := SendRawAll(@MsgBuf[0], TotalSend);
end;

// -----------------------------------------------------------------------
// ITlsTransport — implementación pública
// -----------------------------------------------------------------------

function TSChannelTransport.Connect(const AHost: string; APort: Integer): Boolean;
begin
  Result := False;
  FHost  := AHost;
  ScDiag('--- SChannel.Connect ' + AHost + ':' + IntToStr(APort) + ' ---');

  if not InitCredentials then begin ScDiag('FAIL: InitCredentials'); Exit; end;
  ScDiag('OK: InitCredentials');

  if not TcpConnect(AHost, APort) then begin ScDiag('FAIL: TcpConnect WSAError=' + IntToStr(WsWSAGetLastError)); Exit; end;
  ScDiag('OK: TcpConnect socket=' + IntToStr(FSocket));

  if not DoTlsHandshake then
  begin
    ScDiag('FAIL: DoTlsHandshake');
    WsCloseSocket(FSocket);
    FSocket := WS_INVAL_SOCK;
    Exit;
  end;
  ScDiag('OK: DoTlsHandshake — TLS established');

  FConnected := True;
  Result     := True;
end;

procedure TSChannelTransport.Disconnect;
begin
  FConnected := False;
  if FSocket <> WS_INVAL_SOCK then
  begin
    WsCloseSocket(FSocket);
    FSocket := WS_INVAL_SOCK;
  end;
  if FCtxOK  then begin ScDeleteSecurityContext(PScHandle(@FhCtx));   FCtxOK  := False; end;
  if FCredOK then begin ScFreeCredentialsHandle(PScHandle(@FhCred)); FCredOK := False; end;
  FRawBufLen := 0;
  FDecBufLen := 0;
end;

function TSChannelTransport.SendAll(const AData: TBytes): Boolean;
var
  Offset, ChunkLen: Integer;
begin
  Result := True;
  if not FConnected then begin Result := False; Exit; end;
  if Length(AData) = 0 then Exit;

  Offset := 0;
  while Offset < Length(AData) do
  begin
    ChunkLen := Min(Length(AData) - Offset, Integer(FStreamSizes.cbMaximumMessage));
    if not EncryptAndSendChunk(AData, Offset, ChunkLen) then
    begin
      Result := False;
      Exit;
    end;
    Inc(Offset, ChunkLen);
  end;
end;

// DecryptMessage sobre FRawBuf y retornar bytes descifrados
// Returns: >0=bytes en ABuf, 0=timeout, -1=error/cerrado
function TSChannelTransport.RecvSome(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
var
  InBuf:    array[0..3] of TSecBuffer;
  BufDesc:  TSecBufferDesc;
  Status:   Integer;
  n, j:     Integer;
  DataPtr:  Pointer;
  DataLen:  Integer;
  ExtraPtr: Pointer;
  ExtraLen: Integer;
begin
  // Entregar datos ya descifrados si los hay
  if FDecBufLen > 0 then
  begin
    SetLength(ABuf, FDecBufLen);
    Move(FDecBuf[0], ABuf[0], FDecBufLen);
    Result     := FDecBufLen;
    FDecBufLen := 0;
    Exit;
  end;

  while True do
  begin
    // Intentar descifrar si tenemos datos suficientes (mínimo header TLS = 5 bytes)
    if FRawBufLen >= 5 then
    begin
      InBuf[0].BufferType := SECBUFFER_DATA;
      InBuf[0].cbBuffer   := FRawBufLen;
      InBuf[0].pvBuffer   := @FRawBuf[0];
      for j := 1 to 3 do
      begin
        InBuf[j].BufferType := SECBUFFER_EMPTY;
        InBuf[j].cbBuffer   := 0;
        InBuf[j].pvBuffer   := nil;
      end;
      BufDesc.ulVersion := SECBUFFER_VERSION;
      BufDesc.cBuffers  := 4;
      BufDesc.pBuffers  := @InBuf[0];

      Status := ScDecryptMessage(PScHandle(@FhCtx), @BufDesc, 0, nil);

      case Status of
        SEC_E_OK:
        begin
          // Buscar SECBUFFER_DATA (plaintext) y SECBUFFER_EXTRA (siguiente record)
          DataPtr  := nil; DataLen  := 0;
          ExtraPtr := nil; ExtraLen := 0;
          for j := 0 to 3 do
          begin
            if InBuf[j].BufferType = SECBUFFER_DATA  then
            begin DataPtr := InBuf[j].pvBuffer; DataLen := InBuf[j].cbBuffer; end
            else if InBuf[j].BufferType = SECBUFFER_EXTRA then
            begin ExtraPtr := InBuf[j].pvBuffer; ExtraLen := InBuf[j].cbBuffer; end;
          end;

          // Copiar plaintext al caller
          if DataLen > 0 then
          begin
            SetLength(ABuf, DataLen);
            Move(DataPtr^, ABuf[0], DataLen);
          end;

          // Guardar bytes EXTRA (próximo record cifrado) en FRawBuf
          if ExtraLen > 0 then
          begin
            if ExtraLen > Length(FRawBuf) then
              SetLength(FRawBuf, ExtraLen + 65536);
            Move(ExtraPtr^, FRawBuf[0], ExtraLen);
            FRawBufLen := ExtraLen;
          end
          else
            FRawBufLen := 0;

          Result := DataLen;
          Exit;
        end;

        SEC_E_INCOMPLETE_MESSAGE:
          ; // Necesitamos más bytes TCP — caer al receive de abajo

        SEC_I_CONTEXT_EXPIRED:
        begin Result := -1; Exit; end;

        SEC_I_RENEGOTIATE:
        begin Result := -1; Exit; end; // Fase 2: implementar re-handshake si necesario
      else
        Result := -1;
        Exit;
      end;
    end;

    // Leer más bytes TCP (con timeout)
    if Length(FRawBuf) - FRawBufLen < 4096 then
      SetLength(FRawBuf, FRawBufLen + 65536);

    n := RecvRaw(@FRawBuf[FRawBufLen], Length(FRawBuf) - FRawBufLen, ATimeoutMs);
    if n = 0 then begin Result := 0; Exit; end;  // timeout
    if n < 0 then begin Result := -1; Exit; end; // error
    Inc(FRawBufLen, n);
  end;
end;

// ═══════════════════════════════════════════════════════════════════════
// Inicialización / Finalización de Winsock
// ═══════════════════════════════════════════════════════════════════════

var
  GWsaData: TWsData;

initialization
  WsWSAStartup(WSVERS, GWsaData);

finalization
  WsWSACleanup;

{$ELSE}

interface
implementation

{$ENDIF}

end.
