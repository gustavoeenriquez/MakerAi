// MakerAI Suite — TLS Transport Android vía javax.net.ssl.SSLSocketFactory (JNI)
//
// TAndroidSSLTransport implementa ITlsTransport usando el stack TLS nativo de Android
// (BoringSSL — sin DLLs externos ni permiso de red extra más allá de INTERNET).
//
// Gestión de memoria JNI:
//   - jobject retornados por JNI son locales; se convierten a refs globales con
//     NewGlobalRef para sobrevivir entre llamadas.
//   - ReleaseRefs() elimina todas las refs globales en Disconnect/Destroy.
//   - Hilos TThread en Delphi/Android están auto-attached al JVM; GetCurrentEnv
//     llama AttachCurrentThread como seguro adicional para hilos nativos puros.
//
// Prerequisito en AndroidManifest.xml:
//   <uses-permission android:name="android.permission.INTERNET"/>
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.WebSocket.Android;

interface

{$IFDEF ANDROID}

uses
  System.SysUtils,
  uMakerAi.WebSocket.Client;

type
  TAndroidSSLTransport = class(TInterfacedObject, ITlsTransport)
  private
    FConnected:    Boolean;
    // Referencias globales JNI (válidas entre llamadas a métodos nativos)
    FSocket:       jobject;     // javax.net.ssl.SSLSocket
    FInputStream:  jobject;     // java.io.InputStream
    FOutputStream: jobject;     // java.io.OutputStream
    FReadBuf:      jbyteArray;  // byte[READ_BUF_SIZE] reutilizable para lecturas
    // Method IDs cacheados tras Connect (válidos mientras el proceso vive)
    FMidISRead:     jmethodID;  // InputStream.read([BII)I
    FMidOSWrite:    jmethodID;  // OutputStream.write([BII)V
    FMidSetTimeout: jmethodID;  // Socket.setSoTimeout(I)V
    FMidClose:      jmethodID;  // Socket.close()V
    procedure ReleaseRefs;
    function  GetCurrentEnv: PJNIEnv;
    // True si había excepción pendiente (la limpia siempre)
    function  CheckAndClearException(const E: PJNIEnv): Boolean;
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

{$ELSE}
// Stub vacío en plataformas no-Android
type
  TAndroidSSLTransport = class(TInterfacedObject)
  end;
{$ENDIF}

implementation

{$IFDEF ANDROID}

uses
  Androidapi.Jni,       // jboolean, jbyte, jint, jobject, jclass, jmethodID, jvalue, PJNIEnv …
  Androidapi.JNIBridge; // TJNIResolver

const
  READ_BUF_SIZE   = 32768;
  JNI_VERSION_1_6 = $00010006;
  JNI_OK          = 0;
  JNI_EDETACHED   = -2;
  JNI_ABORT       = 2; // ReleaseByteArrayElements: no copiar de vuelta al heap Java

// ---------------------------------------------------------------------------
//  GetCurrentEnv — PJNIEnv para el hilo actual
//  Adjunta el hilo al JVM si aún no está adjunto (p.ej. hilos nativos puros).
// ---------------------------------------------------------------------------
function TAndroidSSLTransport.GetCurrentEnv: PJNIEnv;
var
  VM:     PJavaVM;
  EnvPtr: Pointer;
  Res:    jint;
begin
  Result := nil;
  VM     := TJNIResolver.GetJavaVM;
  if VM = nil then Exit;
  // GetEnv devuelve la env del hilo actual si está adjunto
  Res := VM^.GetEnv(VM, @EnvPtr, JNI_VERSION_1_6);
  if Res = JNI_EDETACHED then
    VM^.AttachCurrentThread(VM, @EnvPtr, nil);
  Result := PJNIEnv(EnvPtr);
end;

// ---------------------------------------------------------------------------
//  CheckAndClearException — True si había excepción; siempre la limpia.
// ---------------------------------------------------------------------------
function TAndroidSSLTransport.CheckAndClearException(const E: PJNIEnv): Boolean;
begin
  Result := E^.ExceptionCheck(E) <> 0;
  if Result then
    E^.ExceptionClear(E);
end;

// ---------------------------------------------------------------------------
//  ReleaseRefs — libera todas las refs globales JNI
// ---------------------------------------------------------------------------
procedure TAndroidSSLTransport.ReleaseRefs;
var
  E: PJNIEnv;
begin
  E := GetCurrentEnv;
  if E = nil then Exit;
  if FSocket       <> nil then begin E^.DeleteGlobalRef(E, FSocket);       FSocket       := nil; end;
  if FInputStream  <> nil then begin E^.DeleteGlobalRef(E, FInputStream);  FInputStream  := nil; end;
  if FOutputStream <> nil then begin E^.DeleteGlobalRef(E, FOutputStream); FOutputStream := nil; end;
  if FReadBuf      <> nil then begin E^.DeleteGlobalRef(E, FReadBuf);      FReadBuf      := nil; end;
end;

constructor TAndroidSSLTransport.Create;
begin
  inherited;
  FConnected     := False;
  FSocket        := nil;
  FInputStream   := nil;
  FOutputStream  := nil;
  FReadBuf       := nil;
  FMidISRead     := nil;
  FMidOSWrite    := nil;
  FMidSetTimeout := nil;
  FMidClose      := nil;
end;

destructor TAndroidSSLTransport.Destroy;
begin
  Disconnect;
  inherited;
end;

// ---------------------------------------------------------------------------
//  Connect — establece conexión TCP + TLS usando javax.net.ssl.SSLSocketFactory
//
//  Flujo JNI:
//   1. SSLSocketFactory.getDefault()  → factory
//   2. factory.createSocket(host, port) → SSLSocket (conecta TCP + inicia TLS)
//   3. sslSocket.startHandshake()       → completa handshake TLS
//   4. Cachear getInputStream/getOutputStream y sus method IDs
//   5. Crear byte[] reutilizable para lecturas
// ---------------------------------------------------------------------------
function TAndroidSSLTransport.Connect(const AHost: string; APort: Integer): Boolean;
var
  E: PJNIEnv;
  // Clases JNI locales
  CSSLFactory, CActualFactory, CSocket, CIS, COS: jclass;
  // Method IDs locales
  MGetDefault, MCreateSocket, MStartHandshake, MGetIS, MGetOS: jmethodID;
  // Objetos JNI locales (refs locales — se eliminan o se hacen globales)
  LFactory, LSocket, LIS, LOS: jobject;
  LReadBuf: jbyteArray;
  JHost: jstring;
  Args: array[0..1] of jvalue;
begin
  Result     := False;
  FConnected := False;

  E := GetCurrentEnv;
  if E = nil then Exit;

  // 1. javax.net.ssl.SSLSocketFactory.getDefault()
  CSSLFactory := E^.FindClass(E, 'javax/net/ssl/SSLSocketFactory');
  if CheckAndClearException(E) or (CSSLFactory = nil) then Exit;

  MGetDefault := E^.GetStaticMethodID(E, CSSLFactory,
    'getDefault', '()Ljavax/net/ssl/SocketFactory;');
  if CheckAndClearException(E) or (MGetDefault = nil) then
  begin
    E^.DeleteLocalRef(E, CSSLFactory);
    Exit;
  end;

  LFactory := E^.CallStaticObjectMethod(E, CSSLFactory, MGetDefault);
  E^.DeleteLocalRef(E, CSSLFactory);
  if CheckAndClearException(E) or (LFactory = nil) then Exit;

  // 2. factory.createSocket(host, port)
  //    Usamos la clase real de la factory para buscar el método (puede ser subclase)
  CActualFactory := E^.GetObjectClass(E, LFactory);
  MCreateSocket  := E^.GetMethodID(E, CActualFactory,
    'createSocket', '(Ljava/lang/String;I)Ljava/net/Socket;');
  E^.DeleteLocalRef(E, CActualFactory);
  if CheckAndClearException(E) or (MCreateSocket = nil) then
  begin
    E^.DeleteLocalRef(E, LFactory);
    Exit;
  end;

  JHost    := E^.NewStringUTF(E, MarshaledAString(UTF8Encode(AHost)));
  Args[0].l := JHost;
  Args[1].i := APort;
  LSocket  := E^.CallObjectMethodA(E, LFactory, MCreateSocket, @Args[0]);
  E^.DeleteLocalRef(E, JHost);
  E^.DeleteLocalRef(E, LFactory);
  if CheckAndClearException(E) or (LSocket = nil) then Exit;

  // Convertir socket a ref global
  FSocket := E^.NewGlobalRef(E, LSocket);
  E^.DeleteLocalRef(E, LSocket);

  // 3. sslSocket.startHandshake() — forza el handshake TLS ahora (falla rápido)
  CSocket         := E^.GetObjectClass(E, FSocket);
  MStartHandshake := E^.GetMethodID(E, CSocket, 'startHandshake', '()V');
  if (not CheckAndClearException(E)) and (MStartHandshake <> nil) then
  begin
    E^.CallVoidMethod(E, FSocket, MStartHandshake);
    if CheckAndClearException(E) then
    begin
      // Handshake TLS fallido
      E^.DeleteLocalRef(E, CSocket);
      ReleaseRefs;
      Exit;
    end;
  end;

  // 4a. Cachear method IDs de Socket (heredados por SSLSocket)
  FMidSetTimeout := E^.GetMethodID(E, CSocket, 'setSoTimeout', '(I)V');
  FMidClose      := E^.GetMethodID(E, CSocket, 'close', '()V');
  CheckAndClearException(E); // ignorar si alguno falta (muy improbable)

  // 4b. getInputStream / getOutputStream
  MGetIS := E^.GetMethodID(E, CSocket, 'getInputStream',  '()Ljava/io/InputStream;');
  MGetOS := E^.GetMethodID(E, CSocket, 'getOutputStream', '()Ljava/io/OutputStream;');
  E^.DeleteLocalRef(E, CSocket);
  if CheckAndClearException(E) then begin ReleaseRefs; Exit; end;

  LIS := E^.CallObjectMethod(E, FSocket, MGetIS);
  if CheckAndClearException(E) or (LIS = nil) then begin ReleaseRefs; Exit; end;
  FInputStream := E^.NewGlobalRef(E, LIS);
  E^.DeleteLocalRef(E, LIS);

  LOS := E^.CallObjectMethod(E, FSocket, MGetOS);
  if CheckAndClearException(E) or (LOS = nil) then begin ReleaseRefs; Exit; end;
  FOutputStream := E^.NewGlobalRef(E, LOS);
  E^.DeleteLocalRef(E, LOS);

  // 4c. Cachear InputStream.read y OutputStream.write
  CIS        := E^.FindClass(E, 'java/io/InputStream');
  FMidISRead := E^.GetMethodID(E, CIS, 'read', '([BII)I');
  E^.DeleteLocalRef(E, CIS);
  COS         := E^.FindClass(E, 'java/io/OutputStream');
  FMidOSWrite := E^.GetMethodID(E, COS, 'write', '([BII)V');
  E^.DeleteLocalRef(E, COS);
  if CheckAndClearException(E) or (FMidISRead = nil) or (FMidOSWrite = nil) then
  begin
    ReleaseRefs;
    Exit;
  end;

  // 5. Buffer de lectura Java byte[] reutilizable
  LReadBuf := E^.NewByteArray(E, READ_BUF_SIZE);
  if CheckAndClearException(E) or (LReadBuf = nil) then begin ReleaseRefs; Exit; end;
  FReadBuf := jbyteArray(E^.NewGlobalRef(E, LReadBuf));
  E^.DeleteLocalRef(E, LReadBuf);

  FConnected := True;
  Result     := True;
end;

// ---------------------------------------------------------------------------
//  Disconnect — cierra el socket y libera todas las refs JNI
// ---------------------------------------------------------------------------
procedure TAndroidSSLTransport.Disconnect;
var
  E: PJNIEnv;
begin
  FConnected := False;
  if (FSocket <> nil) and (FMidClose <> nil) then
  begin
    E := GetCurrentEnv;
    if E <> nil then
    begin
      E^.CallVoidMethod(E, FSocket, FMidClose);
      E^.ExceptionClear(E); // ignorar errores al cerrar
    end;
  end;
  ReleaseRefs;
end;

// ---------------------------------------------------------------------------
//  SendAll — envía AData completo vía OutputStream.write(byte[],0,len)
// ---------------------------------------------------------------------------
function TAndroidSSLTransport.SendAll(const AData: TBytes): Boolean;
var
  E:    PJNIEnv;
  JBuf: jbyteArray;
  Total: Integer;
  Args: array[0..2] of jvalue;
begin
  Result := False;
  if not FConnected or (FOutputStream = nil) or (Length(AData) = 0) then Exit;

  Total := Length(AData);
  E     := GetCurrentEnv;
  if E = nil then Exit;

  // Crear Java byte[] temporal con los datos a enviar
  JBuf := E^.NewByteArray(E, Total);
  if JBuf = nil then Exit;
  E^.SetByteArrayRegion(E, JBuf, 0, Total, pjbyte(@AData[0]));

  Args[0].l := JBuf;
  Args[1].i := 0;
  Args[2].i := Total;
  E^.CallVoidMethodA(E, FOutputStream, FMidOSWrite, @Args[0]);
  E^.DeleteLocalRef(E, JBuf);

  Result := not CheckAndClearException(E);
  if not Result then
    FConnected := False;
end;

// ---------------------------------------------------------------------------
//  RecvSome — lee hasta READ_BUF_SIZE bytes con timeout ATimeoutMs (ms)
//  Retorna: >0 = bytes en ABuf, 0 = timeout, -1 = error / conexión cerrada
//
//  Distingue SocketTimeoutException (timeout normal) de IOException (error).
// ---------------------------------------------------------------------------
function TAndroidSSLTransport.RecvSome(var ABuf: TBytes; ATimeoutMs: Integer): Integer;
var
  E:         PJNIEnv;
  Args:      array[0..2] of jvalue;
  N:         jint;
  BufPtr:    pjbyte;
  ExObj:     jobject;
  TCls:      jclass;
  IsTimeout: Boolean;
begin
  Result := -1;
  if not FConnected or (FInputStream = nil) then Exit;

  E := GetCurrentEnv;
  if E = nil then Exit;

  // Ajustar timeout de lectura en el socket antes de cada recv
  if FMidSetTimeout <> nil then
  begin
    Args[0].i := ATimeoutMs;
    E^.CallVoidMethodA(E, FSocket, FMidSetTimeout, @Args[0]);
    CheckAndClearException(E); // no crítico si falla
  end;

  // InputStream.read(readBuf, 0, READ_BUF_SIZE) — bloqueante hasta ATimeoutMs
  Args[0].l := FReadBuf;
  Args[1].i := 0;
  Args[2].i := READ_BUF_SIZE;
  N := E^.CallIntMethodA(E, FInputStream, FMidISRead, @Args[0]);

  if E^.ExceptionCheck(E) <> 0 then
  begin
    // Determinar si es timeout (recuperable) o error real (fatal)
    ExObj     := E^.ExceptionOccurred(E);  // OK llamar con excepción pendiente
    E^.ExceptionClear(E);                   // limpiar antes de llamar FindClass

    TCls      := E^.FindClass(E, 'java/net/SocketTimeoutException');
    IsTimeout := (TCls <> nil) and (ExObj <> nil) and
                 (E^.IsInstanceOf(E, ExObj, TCls) <> 0);

    if TCls  <> nil then E^.DeleteLocalRef(E, TCls);
    if ExObj <> nil then E^.DeleteLocalRef(E, ExObj);
    CheckAndClearException(E); // limpiar posible excepción de FindClass

    if IsTimeout then
      Result := 0   // timeout — el reader thread reintentará
    else
    begin
      FConnected := False;
      Result     := -1;
    end;
    Exit;
  end;

  if N <= 0 then
  begin
    // read() devolvió -1: fin de stream (socket cerrado limpiamente)
    FConnected := False;
    Exit;
  end;

  // Copiar N bytes de FReadBuf (Java heap) a ABuf (Delphi heap)
  SetLength(ABuf, N);
  BufPtr := E^.GetByteArrayElements(E, FReadBuf, nil);
  if BufPtr <> nil then
  begin
    Move(BufPtr^, ABuf[0], N);
    E^.ReleaseByteArrayElements(E, FReadBuf, BufPtr, JNI_ABORT);
  end;

  Result := N;
end;

function TAndroidSSLTransport.IsConnected: Boolean;
begin
  Result := FConnected and (FSocket <> nil);
end;

{$ENDIF} // ANDROID

end.
