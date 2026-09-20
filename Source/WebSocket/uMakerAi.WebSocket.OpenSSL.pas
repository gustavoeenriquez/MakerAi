unit uMakerAi.WebSocket.OpenSSL;

{
  TOpenSSLTransport — ITlsTransport via libssl.so (dlopen) + POSIX sockets
  Plataformas: Linux64, macOS
  Android usa TAndroidSSLTransport (uMakerAi.WebSocket.Android.pas)
  Requiere: libssl3 o libssl1.1 (Linux) / LibreSSL o Homebrew OpenSSL (macOS)
}

interface

{$IF NOT DEFINED(MSWINDOWS) AND NOT DEFINED(ANDROID)}

uses
  System.SysUtils,
  uMakerAi.WebSocket.Client;

type
  TOpenSSLTransport = class(TInterfacedObject, ITlsTransport)
  private
    FSocket:    Integer;    // POSIX socket fd; -1 = not connected
    FHost:      string;
    FLibSSL:    NativeUInt; // handle de dlopen (0 = no cargada)
    FCtx:       Pointer;   // SSL_CTX*
    FSSL:       Pointer;   // SSL*
    FConnected: Boolean;
    FInsecureSkipVerify: Boolean;

    // Punteros a funciones OpenSSL
    FSSL_CTX_new:              function(method: Pointer): Pointer; cdecl;
    FSSL_CTX_free:             procedure(ctx: Pointer); cdecl;
    FSSL_CTX_set_verify:       procedure(ctx: Pointer; mode: Integer; cb: Pointer); cdecl;
    FTLS_client_method:        function: Pointer; cdecl;
    FSSL_new:                  function(ctx: Pointer): Pointer; cdecl;
    FSSL_free:                 procedure(ssl: Pointer); cdecl;
    FSSL_set_fd:               function(ssl: Pointer; fd: Integer): Integer; cdecl;
    // OJO: SSL_set_tlsext_host_name NO es una funcion exportada por libssl, es
    // una MACRO de ssl.h sobre SSL_ctrl. Buscarla con dlsym devuelve siempre
    // nil (comprobado con nm -D sobre libssl.so.3), asi que el SNI se saltaba
    // en silencio y el handshake moria con "sslv3 alert handshake failure"
    // contra cualquier host detras de un CDN — que hoy es casi cualquiera.
    // Se manda por SSL_ctrl, que si esta exportada.
    FSSL_ctrl:                 function(ssl: Pointer; cmd: Integer; larg: NativeInt; parg: Pointer): NativeInt; cdecl;
    FSSL_connect:              function(ssl: Pointer): Integer; cdecl;
    FSSL_read:                 function(ssl: Pointer; buf: Pointer; num: Integer): Integer; cdecl;
    FSSL_write:                function(ssl: Pointer; buf: Pointer; num: Integer): Integer; cdecl;
    // Verificacion de certificado. SSL_VERIFY_PEER valida la CADENA pero NO el
    // hostname: un certificado valido de otro dominio pasaria el filtro. Para
    // atar el certificado al host hay que llamar ademas a SSL_set1_host
    // (exportada desde OpenSSL 1.1.0), que fija el X509_VERIFY_PARAM.
    FSSL_CTX_set_default_verify_paths: function(ctx: Pointer): Integer; cdecl;
    FSSL_set1_host:            function(ssl: Pointer; host: PAnsiChar): Integer; cdecl;
    FSSL_get_verify_result:    function(ssl: Pointer): NativeInt; cdecl;

    procedure LoadLibSSL;
    procedure UnloadLibSSL;
    function  TcpConnect(const AHost: string; APort: Integer): Boolean;
    procedure TcpClose;
    function  RecvRaw(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    // ITlsTransport
    function  Connect(const AHost: string; APort: Integer): Boolean;
    procedure Disconnect;
    function  SendAll(const AData: TBytes): Boolean;
    function  RecvSome(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
    function  IsConnected: Boolean;

    // Por defecto False: el certificado del servidor SE VALIDA (cadena contra el
    // almacen de CAs del sistema + hostname). Ponerlo a True solo para pruebas
    // contra endpoints con certificado propio o caducado; deja la conexion
    // expuesta a un intermediario.
    property InsecureSkipVerify: Boolean read FInsecureSkipVerify write FInsecureSkipVerify;
  end;

{$ELSE}
// Stub vacío en Windows y Android — TOpenSSLTransport no se usa en estas plataformas
type
  TOpenSSLTransport = class(TInterfacedObject)
  end;
{$ENDIF}

implementation

{$IF NOT DEFINED(MSWINDOWS) AND NOT DEFINED(ANDROID)}

uses
  System.StrUtils,
  Posix.Dlfcn,
  Posix.SysSocket,
  Posix.NetDB,
  Posix.SysSelect,
  Posix.SysTime,
  Posix.Unistd,
  Posix.Errno;

const
  SSL_VERIFY_NONE = 0;
  SSL_VERIFY_PEER = 1;
  X509_V_OK       = 0;

{ TOpenSSLTransport }

constructor TOpenSSLTransport.Create;
begin
  inherited;
  FSocket := -1;
  FLibSSL := 0;
  FCtx    := nil;
  FSSL    := nil;
end;

destructor TOpenSSLTransport.Destroy;
begin
  Disconnect;
  inherited;
end;

// ---------------------------------------------------------------------------
//  LoadLibSSL — carga dinámica de libssl en orden de preferencia
// ---------------------------------------------------------------------------
procedure TOpenSSLTransport.LoadLibSSL;

  function TryLoad(const AName: string): NativeUInt;
  begin
    Result := dlopen(PAnsiChar(AnsiString(AName)), RTLD_LAZY);
  end;

  procedure Bind(var APtr; const AName: string);
  begin
    Pointer(APtr) := dlsym(FLibSSL, PAnsiChar(AnsiString(AName)));
  end;

begin
  if FLibSSL <> 0 then Exit;  // ya cargada

  FLibSSL := TryLoad('libssl.so.3');
  if FLibSSL = 0 then
    FLibSSL := TryLoad('libssl.so.1.1');
  if FLibSSL = 0 then
    FLibSSL := TryLoad('libssl.dylib');   // macOS (Homebrew / LibreSSL)
  if FLibSSL = 0 then
    raise Exception.Create('OpenSSL no encontrado: instalar libssl3 o libssl1.1');

  Bind(FTLS_client_method,        'TLS_client_method');
  Bind(FSSL_CTX_new,              'SSL_CTX_new');
  Bind(FSSL_CTX_free,             'SSL_CTX_free');
  Bind(FSSL_CTX_set_verify,       'SSL_CTX_set_verify');
  Bind(FSSL_new,                  'SSL_new');
  Bind(FSSL_free,                 'SSL_free');
  Bind(FSSL_set_fd,               'SSL_set_fd');
  Bind(FSSL_ctrl,                 'SSL_ctrl');
  Bind(FSSL_connect,              'SSL_connect');
  Bind(FSSL_read,                 'SSL_read');
  Bind(FSSL_write,                'SSL_write');
  Bind(FSSL_CTX_set_default_verify_paths, 'SSL_CTX_set_default_verify_paths');
  Bind(FSSL_set1_host,            'SSL_set1_host');
  Bind(FSSL_get_verify_result,    'SSL_get_verify_result');

  if not (Assigned(FSSL_CTX_new) and Assigned(FSSL_connect) and
          Assigned(FSSL_read)    and Assigned(FSSL_write)) then
    raise Exception.Create('OpenSSL: símbolos críticos no encontrados en la librería');
end;

procedure TOpenSSLTransport.UnloadLibSSL;
begin
  if FLibSSL <> 0 then
  begin
    dlclose(FLibSSL);
    FLibSSL := 0;
  end;
end;

// ---------------------------------------------------------------------------
//  TcpConnect — POSIX getaddrinfo + socket + connect
// ---------------------------------------------------------------------------
function TOpenSSLTransport.TcpConnect(const AHost: string; APort: Integer): Boolean;
var
  Hints: addrinfo;
  Res, Cur: Paddrinfo;
  PortStr: AnsiString;
  SockFd: Integer;
begin
  Result := False;
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family   := AF_UNSPEC;
  Hints.ai_socktype := SOCK_STREAM;

  PortStr := AnsiString(IntToStr(APort));
  if getaddrinfo(PAnsiChar(AnsiString(AHost)), PAnsiChar(PortStr), Hints, Res) <> 0 then
    Exit;

  try
    Cur := Res;
    while Cur <> nil do
    begin
      SockFd := Posix.SysSocket.socket(Cur^.ai_family, Cur^.ai_socktype, Cur^.ai_protocol);
      if SockFd >= 0 then
      begin
        if Posix.SysSocket.connect(SockFd, Cur^.ai_addr^, Cur^.ai_addrlen) = 0 then
        begin
          FSocket := SockFd;
          Exit(True);
        end;
        __close(SockFd);
      end;
      Cur := Cur^.ai_next;
    end;
  finally
    freeaddrinfo(Res^);
  end;
end;

procedure TOpenSSLTransport.TcpClose;
begin
  if FSocket >= 0 then
  begin
    __close(FSocket);
    FSocket := -1;
  end;
end;

// ---------------------------------------------------------------------------
//  ITlsTransport implementation
// ---------------------------------------------------------------------------
function TOpenSSLTransport.Connect(const AHost: string; APort: Integer): Boolean;
var
  LHostAnsi: AnsiString;
  LVerify: NativeInt;
begin
  Result   := False;
  FConnected := False;
  FHost    := AHost;

  LoadLibSSL;

  if not TcpConnect(AHost, APort) then
    raise Exception.CreateFmt('OpenSSL: no se pudo conectar TCP a %s:%d', [AHost, APort]);

  // Crear contexto SSL
  FCtx := FSSL_CTX_new(FTLS_client_method);
  if FCtx = nil then
    raise Exception.Create('OpenSSL: SSL_CTX_new falló');

  // Verificacion del certificado del servidor.
  if FInsecureSkipVerify then
    FSSL_CTX_set_verify(FCtx, SSL_VERIFY_NONE, nil)
  else
  begin
    // Cadena de confianza: CAs del sistema (/etc/ssl/certs en Debian/Ubuntu).
    if not Assigned(FSSL_CTX_set_default_verify_paths) then
      raise Exception.Create('OpenSSL: SSL_CTX_set_default_verify_paths no disponible; ' +
        'no se puede validar el certificado (usar InsecureSkipVerify para aceptar el riesgo)');
    if FSSL_CTX_set_default_verify_paths(FCtx) <> 1 then
      raise Exception.Create('OpenSSL: no se pudo cargar el almacen de CAs del sistema ' +
        '(instalar ca-certificates)');
    FSSL_CTX_set_verify(FCtx, SSL_VERIFY_PEER, nil);
  end;

  // Crear sesión SSL
  FSSL := FSSL_new(FCtx);
  if FSSL = nil then
    raise Exception.Create('OpenSSL: SSL_new falló');

  // Asociar socket
  if FSSL_set_fd(FSSL, FSocket) <> 1 then
    raise Exception.Create('OpenSSL: SSL_set_fd falló');

  // SNI (Server Name Indication) — via SSL_ctrl, porque la version "bonita"
  // es una macro y no existe como simbolo. Sin SNI, api.openai.com (y todo lo
  // que viva tras Cloudflare) corta el handshake con alert 40.
  // 55 = SSL_CTRL_SET_TLSEXT_HOSTNAME, 0 = TLSEXT_NAMETYPE_host_name.
  LHostAnsi := AnsiString(AHost);
  if Assigned(FSSL_ctrl) and (LHostAnsi <> '') then
    FSSL_ctrl(FSSL, 55, 0, PAnsiChar(LHostAnsi));

  // Ata el certificado a ESTE host. Sin esto, SSL_VERIFY_PEER aceptaria un
  // certificado valido emitido para cualquier otro dominio: la cadena estaria
  // bien y el nombre no se miraria. Es el fallo clasico de las validaciones a
  // medias. SSL_set1_host existe desde OpenSSL 1.1.0.
  if (not FInsecureSkipVerify) and (LHostAnsi <> '') then
  begin
    if not Assigned(FSSL_set1_host) then
      raise Exception.Create('OpenSSL: SSL_set1_host no disponible (libssl < 1.1.0); ' +
        'sin verificacion de hostname no se puede considerar seguro');
    if FSSL_set1_host(FSSL, PAnsiChar(LHostAnsi)) <> 1 then
      raise Exception.CreateFmt('OpenSSL: SSL_set1_host fallo para "%s"', [AHost]);
  end;

  // Handshake TLS
  if FSSL_connect(FSSL) <> 1 then
  begin
    // Si el handshake cayo por el certificado, SSL_get_verify_result dice por
    // que (18 = self signed, 10 = expirado, 62 = hostname no coincide...).
    LVerify := X509_V_OK;
    if Assigned(FSSL_get_verify_result) then
      LVerify := FSSL_get_verify_result(FSSL);
    if LVerify <> X509_V_OK then
      raise Exception.CreateFmt(
        'OpenSSL: certificado de %s rechazado (X509_V code %d). ' +
        'Si el endpoint usa un certificado propio, poner InsecureSkipVerify := True.',
        [AHost, LVerify]);
    raise Exception.Create('OpenSSL: SSL_connect falló — handshake TLS rechazado' +
      IfThen(Assigned(FSSL_ctrl), '', ' (ademas SSL_ctrl no se pudo enlazar: no habra SNI)'));
  end;

  FConnected := True;
  Result     := True;
end;

procedure TOpenSSLTransport.Disconnect;
begin
  FConnected := False;

  if FSSL <> nil then
  begin
    FSSL_free(FSSL);
    FSSL := nil;
  end;

  if FCtx <> nil then
  begin
    FSSL_CTX_free(FCtx);
    FCtx := nil;
  end;

  TcpClose;
  // No llamamos UnloadLibSSL aquí — la lib se mantiene cargada para reconexiones
end;

function TOpenSSLTransport.SendAll(const AData: TBytes): Boolean;
var
  Total, Written: Integer;
  Offset: Integer;
begin
  Result := False;
  if (FSSL = nil) or (Length(AData) = 0) then Exit;

  Total  := Length(AData);
  Offset := 0;
  while Offset < Total do
  begin
    Written := FSSL_write(FSSL, @AData[Offset], Total - Offset);
    if Written <= 0 then Exit;
    Inc(Offset, Written);
  end;
  Result := True;
end;

// ---------------------------------------------------------------------------
//  RecvRaw — espera datos TCP con timeout usando select()
//  Retorna: >0 = bytes leídos en ABuf, 0 = timeout, -1 = error/cierre
// ---------------------------------------------------------------------------
function TOpenSSLTransport.RecvRaw(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
var
  FdSet: fd_set;
  TV: timeval;
  SR: Integer;
begin
  Result := -1;
  if FSocket < 0 then Exit;

  __FD_ZERO(FdSet);
  __FD_SET(FSocket, FdSet);
  TV.tv_sec  := ATimeoutMs div 1000;
  TV.tv_usec := (ATimeoutMs mod 1000) * 1000;

  SR := select(FSocket + 1, @FdSet, nil, nil, @TV);
  if SR = 0 then
    Exit(0)   // timeout
  else if SR < 0 then
    Exit(-1); // error

  SetLength(ABuf, 32768);
  Result := FSSL_read(FSSL, @ABuf[0], Length(ABuf));
  if Result > 0 then
    SetLength(ABuf, Result)
  else
  begin
    SetLength(ABuf, 0);
    Result := -1;  // SSL_read devolvió 0 (cierre) o <0 (error)
  end;
end;

function TOpenSSLTransport.RecvSome(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
begin
  Result := RecvRaw(ABuf, ATimeoutMs);
end;

function TOpenSSLTransport.IsConnected: Boolean;
begin
  Result := FConnected and (FSocket >= 0) and (FSSL <> nil);
end;

{$IFEND} // NOT MSWINDOWS AND NOT ANDROID

end.
