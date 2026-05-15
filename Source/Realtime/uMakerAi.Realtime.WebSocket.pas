// MakerAI Suite — WebSocket client para el modulo Realtime
// Implementacion via WinHTTP WebSocket API (Windows-native Schannel TLS)
// Reemplaza Indy+OpenSSL; compatible con Cloudflare CDN (OpenAI Realtime API)
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.WebSocket;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.StrUtils,
  System.IOUtils,
  Winapi.Windows;

const
  WS_DIAG_LOG = 'C:\Temp\ws_diag.txt';

type
  TAiRealtimeWSOpcode = (
    rwsoContinuation = 0,
    rwsoText         = 1,
    rwsoBinary       = 2,
    rwsoClose        = 8,
    rwsoPing         = 9,
    rwsoPong         = 10
  );

  TAiRealtimeWSFrameEvent = procedure(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
    const Data: TBytes; IsFinal: Boolean) of object;
  TAiRealtimeWSErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;

  TAiRealtimeWSClient = class;

  TAiRealtimeWSReaderThread = class(TThread)
  private
    FClient:   TAiRealtimeWSClient;
    FWSHandle: THandle;
    FStopping: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TAiRealtimeWSClient; AWSHandle: THandle);
    procedure Stop;
  end;

  TAiRealtimeWSClient = class
  private
    FSession:      THandle;
    FConnection:   THandle;
    FRequest:      THandle;
    FWSHandle:     THandle;
    FReaderThread: TAiRealtimeWSReaderThread;
    FLock:         TCriticalSection;
    FConnected:    Boolean;
    FUrl:          string;
    FHost:         string;
    FPath:         string;
    FPort:         Integer;
    FUseSSL:       Boolean;
    FExtraHeaders: TStringList;
    FOnFrame:        TAiRealtimeWSFrameEvent;
    FOnConnected:    TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnError:        TAiRealtimeWSErrorEvent;
    procedure ParseUrl(const AUrl: string);
  public
    constructor Create;
    destructor  Destroy; override;
    function  Connect(const AUrl: string): Boolean;
    procedure Disconnect;
    procedure SendText(const AText: string);
    procedure SendBinary(const AData: TBytes);
    procedure SendPing;
    procedure SendClose(Code: Word = 1000; const Reason: string = '');
    procedure HandleIncomingFrame(Opcode: TAiRealtimeWSOpcode;
      const Data: TBytes; IsFinal: Boolean);
    procedure HandleDisconnect;
    procedure HandleError(const Msg: string);
    property Connected:      Boolean                   read FConnected;
    property Url:            string                    read FUrl;
    property ExtraHeaders:   TStringList               read FExtraHeaders;
    property OnFrame:        TAiRealtimeWSFrameEvent   read FOnFrame        write FOnFrame;
    property OnConnected:    TNotifyEvent              read FOnConnected    write FOnConnected;
    property OnDisconnected: TNotifyEvent              read FOnDisconnected write FOnDisconnected;
    property OnError:        TAiRealtimeWSErrorEvent   read FOnError        write FOnError;
  end;

implementation

const
  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY    = 0;
  WINHTTP_FLAG_SECURE                  = $00800000;
  WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET = 114;
  WINHTTP_ADDREQ_FLAG_ADD              = $20000000;

  // WINHTTP_WEB_SOCKET_BUFFER_TYPE enum — valores del SDK de Windows
  WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE  = 0;
  WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE = 1;
  WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE    = 2;
  WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE   = 3;
  WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE           = 4;

// Formato legible para errores WinHTTP (12000+) usando FormatMessage con winhttp.dll
function WinHttpErrMsg(Code: DWORD): string;
var
  hMod: THandle;
  Buf:  array[0..511] of Char;
begin
  Result := SysErrorMessage(Code);
  if Result = '' then
  begin
    hMod := LoadLibraryEx('winhttp.dll', 0, $00000002 {LOAD_LIBRARY_AS_DATAFILE});
    if hMod <> 0 then
    try
      if FormatMessage(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_IGNORE_INSERTS,
        Pointer(hMod), Code, 0, Buf, Length(Buf), nil) > 0 then
        Result := Trim(Buf);
    finally
      FreeLibrary(hMod);
    end;
  end;
  Result := 'err=' + IntToStr(Code) + IfThen(Result <> '', ' (' + Result + ')');
end;

// WinHTTP API — disponible en Windows Vista+; WebSocket en Windows 8+
function WinHttpOpen(pszAgent: PWideChar; dwAccess: DWORD;
  pszProxy, pszBypass: PWideChar; dwFlags: DWORD): THandle; stdcall;
  external 'winhttp.dll';
function WinHttpConnect(hSession: THandle; pszServer: PWideChar;
  nPort: Word; dwReserved: DWORD): THandle; stdcall;
  external 'winhttp.dll';
function WinHttpOpenRequest(hConnect: THandle; pwszVerb, pwszObject,
  pwszVersion, pwszReferrer: PWideChar; ppAcceptTypes: Pointer;
  dwFlags: DWORD): THandle; stdcall;
  external 'winhttp.dll';
function WinHttpSetOption(hInternet: THandle; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
  external 'winhttp.dll';
function WinHttpAddRequestHeaders(hRequest: THandle; pwszHeaders: PWideChar;
  dwHeadersLength: DWORD; dwModifiers: DWORD): BOOL; stdcall;
  external 'winhttp.dll';
function WinHttpSendRequest(hRequest: THandle; pwszAddlHeaders: PWideChar;
  dwAddlHeadersLen: DWORD; lpOptional: Pointer; dwOptionalLen: DWORD;
  dwTotalLen: DWORD; dwContext: NativeUInt): BOOL; stdcall;
  external 'winhttp.dll';
function WinHttpReceiveResponse(hRequest: THandle; lpReserved: Pointer): BOOL; stdcall;
  external 'winhttp.dll';
function WinHttpWebSocketCompleteUpgrade(hRequest: THandle;
  pContext: NativeUInt): THandle; stdcall;
  external 'winhttp.dll';
function WinHttpWebSocketSend(hWebSocket: THandle; eBufferType: DWORD;
  pvBuffer: Pointer; dwBufferLength: DWORD): DWORD; stdcall;
  external 'winhttp.dll';
function WinHttpWebSocketReceive(hWebSocket: THandle; pvBuffer: Pointer;
  dwBufferLength: DWORD; pdwBytesRead: PDWORD; peBufferType: PDWORD): DWORD; stdcall;
  external 'winhttp.dll';
function WinHttpWebSocketClose(hWebSocket: THandle; usStatus: Word;
  pvReason: Pointer; dwReasonLength: DWORD): DWORD; stdcall;
  external 'winhttp.dll';
function WinHttpCloseHandle(hInternet: THandle): BOOL; stdcall;
  external 'winhttp.dll';

{ TAiRealtimeWSReaderThread }

constructor TAiRealtimeWSReaderThread.Create(AClient: TAiRealtimeWSClient;
  AWSHandle: THandle);
begin
  FClient         := AClient;
  FWSHandle       := AWSHandle;
  FStopping       := False;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TAiRealtimeWSReaderThread.Stop;
begin
  FStopping := True;
  Terminate;
end;

procedure WsDiag(const S: string);
begin
  try TFile.AppendAllText(WS_DIAG_LOG, S + sLineBreak); except end;
end;

procedure TAiRealtimeWSReaderThread.Execute;
const
  READ_BUF_SIZE = 128 * 1024; // 128 KB por lectura
var
  Buf:       TBytes;
  BytesRead: DWORD;
  BufType:   DWORD;
  Ret:       DWORD;
  FragBuf:   TBytes;
  FragLen:   Integer;
  Data:      TBytes;
begin
  SetLength(Buf,     READ_BUF_SIZE);
  SetLength(FragBuf, 0);
  FragLen := 0;
  WsDiag('=== READER STARTED ' + DateTimeToStr(Now) + ' handle=' + IntToStr(FWSHandle) + ' ===');

  while not Terminated do
  begin
    BytesRead := 0;
    BufType   := 0;
    Ret := WinHttpWebSocketReceive(FWSHandle, @Buf[0], READ_BUF_SIZE,
      @BytesRead, @BufType);
    WsDiag('Receive: Ret=' + IntToStr(Ret) + ' BytesRead=' + IntToStr(BytesRead) + ' BufType=' + IntToStr(BufType));

    if Ret <> 0 then
    begin
      if not FStopping and not Terminated then
        FClient.HandleError('WebSocket recepcion: ' + WinHttpErrMsg(Ret));
      Break;
    end;

    case BufType of

      WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE,
      WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE:
      begin
        // Mensaje completo — combinar con fragmentos previos si los hay
        SetLength(Data, FragLen + Integer(BytesRead));
        if FragLen > 0 then
          Move(FragBuf[0], Data[0], FragLen);
        if BytesRead > 0 then
          Move(Buf[0], Data[FragLen], BytesRead);
        FragLen := 0;
        SetLength(FragBuf, 0);
        if BufType = WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE then
          FClient.HandleIncomingFrame(rwsoText, Data, True)
        else
          FClient.HandleIncomingFrame(rwsoBinary, Data, True);
      end;

      WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE,
      WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE:
      begin
        // Fragmento — acumular hasta recibir el frame MESSAGE final
        SetLength(FragBuf, FragLen + Integer(BytesRead));
        if BytesRead > 0 then
          Move(Buf[0], FragBuf[FragLen], BytesRead);
        Inc(FragLen, Integer(BytesRead));
      end;

      WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE:
        Break;

      // PING_PONG_BUFFER_TYPE (5/6): gestionado por WinHTTP automaticamente
    end;
  end;

  FClient.HandleDisconnect;
end;

{ TAiRealtimeWSClient }

constructor TAiRealtimeWSClient.Create;
begin
  inherited;
  FLock         := TCriticalSection.Create;
  FExtraHeaders := TStringList.Create;
  FExtraHeaders.NameValueSeparator := ':';
  FConnected  := False;
  FSession    := 0;
  FConnection := 0;
  FRequest    := 0;
  FWSHandle   := 0;
end;

destructor TAiRealtimeWSClient.Destroy;
begin
  Disconnect;
  FExtraHeaders.Free;
  FLock.Free;
  inherited;
end;

procedure TAiRealtimeWSClient.ParseUrl(const AUrl: string);
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
    raise EArgumentException.Create('URL invalida: ' + AUrl);

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

function TAiRealtimeWSClient.Connect(const AUrl: string): Boolean;
var
  Flags:     DWORD;
  i:         Integer;
  HeaderStr: string;
  LastErr:   DWORD;
begin
  Result := False;
  if FConnected then Exit;

  ParseUrl(AUrl);

  // Sesion WinHTTP — usa Schannel (TLS nativo de Windows, no OpenSSL)
  FSession := WinHttpOpen('MakerAI-Realtime/1.0',
    WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0);
  if FSession = 0 then
  begin
    HandleError('WinHttpOpen: ' + WinHttpErrMsg(GetLastError));
    Exit;
  end;

  FConnection := WinHttpConnect(FSession, PWideChar(FHost), Word(FPort), 0);
  if FConnection = 0 then
  begin
    LastErr := GetLastError;
    HandleError('WinHttpConnect: ' + WinHttpErrMsg(LastErr));
    WinHttpCloseHandle(FSession); FSession := 0;
    Exit;
  end;

  Flags := 0;
  if FUseSSL then Flags := WINHTTP_FLAG_SECURE;
  FRequest := WinHttpOpenRequest(FConnection, 'GET', PWideChar(FPath),
    nil, nil, nil, Flags);
  if FRequest = 0 then
  begin
    LastErr := GetLastError;
    HandleError('WinHttpOpenRequest: ' + WinHttpErrMsg(LastErr));
    WinHttpCloseHandle(FConnection); FConnection := 0;
    WinHttpCloseHandle(FSession); FSession := 0;
    Exit;
  end;

  // Marcar como solicitud de upgrade WebSocket
  if not WinHttpSetOption(FRequest, WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET, nil, 0) then
  begin
    LastErr := GetLastError;
    HandleError('WS upgrade option: ' + WinHttpErrMsg(LastErr));
    WinHttpCloseHandle(FRequest); FRequest := 0;
    WinHttpCloseHandle(FConnection); FConnection := 0;
    WinHttpCloseHandle(FSession); FSession := 0;
    Exit;
  end;

  // Agregar headers de autorizacion y protocolo
  for i := 0 to FExtraHeaders.Count - 1 do
  begin
    if FExtraHeaders.Names[i] <> '' then
      HeaderStr := FExtraHeaders.Names[i] + ': ' + FExtraHeaders.ValueFromIndex[i]
    else
      HeaderStr := FExtraHeaders[i];
    if HeaderStr <> '' then
      WinHttpAddRequestHeaders(FRequest, PWideChar(HeaderStr),
        DWORD(-1), WINHTTP_ADDREQ_FLAG_ADD);
  end;

  if not WinHttpSendRequest(FRequest, nil, 0, nil, 0, 0, 0) then
  begin
    LastErr := GetLastError;
    HandleError('WinHttpSendRequest: ' + WinHttpErrMsg(LastErr));
    WinHttpCloseHandle(FRequest); FRequest := 0;
    WinHttpCloseHandle(FConnection); FConnection := 0;
    WinHttpCloseHandle(FSession); FSession := 0;
    Exit;
  end;

  // Esperar respuesta HTTP 101 Switching Protocols
  if not WinHttpReceiveResponse(FRequest, nil) then
  begin
    LastErr := GetLastError;
    HandleError('WinHttpReceiveResponse: ' + WinHttpErrMsg(LastErr));
    WinHttpCloseHandle(FRequest); FRequest := 0;
    WinHttpCloseHandle(FConnection); FConnection := 0;
    WinHttpCloseHandle(FSession); FSession := 0;
    Exit;
  end;

  // Completar handshake WebSocket — falla si el servidor no respondio 101
  FWSHandle := WinHttpWebSocketCompleteUpgrade(FRequest, 0);
  if FWSHandle = 0 then
  begin
    LastErr := GetLastError;
    HandleError('WS handshake fallido: ' + WinHttpErrMsg(LastErr));
    WinHttpCloseHandle(FRequest); FRequest := 0;
    WinHttpCloseHandle(FConnection); FConnection := 0;
    WinHttpCloseHandle(FSession); FSession := 0;
    Exit;
  end;

  // El handle de request ya no se necesita; usar FWSHandle para I/O
  WinHttpCloseHandle(FRequest);
  FRequest := 0;

  FConnected    := True;
  FReaderThread := TAiRealtimeWSReaderThread.Create(Self, FWSHandle);
  Result        := True;

  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TAiRealtimeWSClient.Disconnect;
begin
  FConnected := False;

  if Assigned(FReaderThread) then
  begin
    FReaderThread.Stop;
    // Cerrar FWSHandle para desbloquear WinHttpWebSocketReceive en el reader thread
    if FWSHandle <> 0 then
    begin
      WinHttpCloseHandle(FWSHandle);
      FWSHandle := 0;
    end;
    FReaderThread.WaitFor;
    FreeAndNil(FReaderThread);
  end
  else if FWSHandle <> 0 then
  begin
    WinHttpCloseHandle(FWSHandle);
    FWSHandle := 0;
  end;

  if FRequest    <> 0 then begin WinHttpCloseHandle(FRequest);    FRequest    := 0; end;
  if FConnection <> 0 then begin WinHttpCloseHandle(FConnection); FConnection := 0; end;
  if FSession    <> 0 then begin WinHttpCloseHandle(FSession);    FSession    := 0; end;
end;

procedure TAiRealtimeWSClient.SendText(const AText: string);
var
  Data: TBytes;
  Ptr:  Pointer;
  Ret:  DWORD;
begin
  Data := TEncoding.UTF8.GetBytes(AText);
  FLock.Enter;
  try
    if not FConnected or (FWSHandle = 0) then Exit;
    if Length(Data) > 0 then Ptr := @Data[0] else Ptr := nil;
    Ret := WinHttpWebSocketSend(FWSHandle,
      WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE, Ptr, Length(Data));
    if Ret <> 0 then
      HandleError('SendText: ' + SysErrorMessage(Ret));
  finally
    FLock.Leave;
  end;
end;

procedure TAiRealtimeWSClient.SendBinary(const AData: TBytes);
var
  Ptr: Pointer;
  Ret: DWORD;
begin
  FLock.Enter;
  try
    if not FConnected or (FWSHandle = 0) then Exit;
    if Length(AData) > 0 then Ptr := @AData[0] else Ptr := nil;
    Ret := WinHttpWebSocketSend(FWSHandle,
      WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE, Ptr, Length(AData));
    if Ret <> 0 then
      HandleError('SendBinary: ' + SysErrorMessage(Ret));
  finally
    FLock.Leave;
  end;
end;

procedure TAiRealtimeWSClient.SendPing;
begin
  // WinHTTP gestiona keepalive/ping-pong automaticamente
end;

procedure TAiRealtimeWSClient.SendClose(Code: Word; const Reason: string);
var
  ReasonBytes: TBytes;
  Ptr:         Pointer;
begin
  FLock.Enter;
  try
    if not FConnected or (FWSHandle = 0) then Exit;
    ReasonBytes := TEncoding.UTF8.GetBytes(Reason);
    if Length(ReasonBytes) > 0 then Ptr := @ReasonBytes[0] else Ptr := nil;
    WinHttpWebSocketClose(FWSHandle, Code, Ptr, Length(ReasonBytes));
  finally
    FLock.Leave;
  end;
end;

procedure TAiRealtimeWSClient.HandleIncomingFrame(Opcode: TAiRealtimeWSOpcode;
  const Data: TBytes; IsFinal: Boolean);
begin
  if Assigned(FOnFrame) then
    FOnFrame(Self, Opcode, Data, IsFinal);
end;

procedure TAiRealtimeWSClient.HandleDisconnect;
begin
  if not FConnected then Exit;
  FConnected := False;
  if FWSHandle   <> 0 then begin WinHttpCloseHandle(FWSHandle);   FWSHandle   := 0; end;
  if FRequest    <> 0 then begin WinHttpCloseHandle(FRequest);    FRequest    := 0; end;
  if FConnection <> 0 then begin WinHttpCloseHandle(FConnection); FConnection := 0; end;
  if FSession    <> 0 then begin WinHttpCloseHandle(FSession);    FSession    := 0; end;
  if Assigned(FOnDisconnected) then
    TThread.Queue(nil, procedure begin
      if Assigned(FOnDisconnected) then FOnDisconnected(Self);
    end);
end;

procedure TAiRealtimeWSClient.HandleError(const Msg: string);
begin
  if Assigned(FOnError) then
    TThread.Queue(nil, procedure begin
      if Assigned(FOnError) then FOnError(Self, Msg);
    end);
end;

end.
