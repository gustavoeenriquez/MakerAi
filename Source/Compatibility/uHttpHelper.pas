// =============================================================================
// MakerAI Cross-Platform Compatibility Layer - HTTP Helper
// =============================================================================
//
// Purpose: Unifica clientes HTTP entre Delphi (TNetHTTPClient) y FPC (TFPHTTPClient)
//
// Compatibility:
//   - Delphi XE7+ (Primary development environment con System.Net.HttpClient)
//   - Free Pascal 3.2+ (usa fphttpclient y opensslsockets)
//
// Usage:
//   uses uHttpHelper;
//   var
//     Client: TNetHTTPClient;
//     Response: string;
//   begin
//     Client := CreateHttpClient;
//     try
//       {$IFDEF FPC}
//       Response := Client.GetString('https://api.example.com');
//       {$ELSE}
//       Response := ExtractResponseString(Client.Get('https://api.example.com'));
//       {$ENDIF}
//     finally
//       Client.Free;
//     end;
//   end;
//
// Developer Notes (Delphi):
//   - TNetHTTPClient se usa directamente (wrapper en FPC, nativo en Delphi)
//   - ExtractResponseString() simplifica acceso a IHTTPResponse
//   - En FPC, GetString() está disponible via helper para API uniforme
//
// =============================================================================

unit uHttpHelper;

{$include ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, fphttpclient, opensslsockets, SysUtils, URIParser,
  {$ELSE}
  System.Classes, System.Net.HttpClient, System.Net.URLClient, System.Net.Mime, 
  System.Net.HttpClientComponent, System.SysUtils, System.IOUtils,
  {$ENDIF}
  uJsonHelper;

type
  TAiURI = record
    Scheme: string;
    Host: string;
    Port: Integer;
    Path: string;
    Query: string;
  end;
  {$IFDEF FPC}
  TNetHeader = record
    Name: string;
    Value: string;
    class function Create(const AName, AValue: string): TNetHeader; static;
  end;
  TNetHeaders = array of TNetHeader;




  IHTTPResponse = interface
    ['{DA3E6334-1F67-4229-8730-8D752B52B7AF}']
    function GetStatusCode: Integer;
    function GetStatusText: string;
    function GetContentAsString: string;
    function GetHeaderValue(const AName: string): string;
    property StatusCode: Integer read GetStatusCode;
    property StatusText: string read GetStatusText;
    property ContentAsString: string read GetContentAsString;
    property HeaderValue[const AName: string]: string read GetHeaderValue; default;
  end;

  // FPC Implementation of TMultipartFormData for multipart/form-data HTTP requests
  TMultipartFormData = class
  private
    FBoundary: string;
    FStream: TMemoryStream;
    FFinalized: Boolean;
    procedure WriteBoundary(IsLast: Boolean = False);
    procedure WriteString(const S: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddField(const AName, AValue: string);
    procedure AddStream(const AName: string; AStream: TStream; FreeStream: Boolean = False; const AFileName: string = ''; const AContentType: string = '');
    function GetStream: TStream;
    function GetContentType: string;
  end;

  // Definición manual de evento compatible con FPC TFPHTTPClient (TDataReceivedEvent)
  TAiDataReceivedEvent = procedure(const Sender: TObject; const ContentLength, CurrentPos: Int64; var Abort: Boolean) of object;
  TRequestErrorEvent = procedure(const Sender: TObject; const AError: string) of object;
  TRequestCompletedEvent = procedure(const Sender: TObject; const AResponse: IHTTPResponse) of object;

  TNetHTTPClient = class(TFPHTTPClient)
  private
    FSynchronizeEvents: Boolean;
    FAsynchronous: Boolean;
    FUserOnReceiveData: TAiDataReceivedEvent;
    FOnRequestError: TRequestErrorEvent;
    FOnRequestCompleted: TRequestCompletedEvent;
    // ...

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetResponseTimeout: Integer;
    procedure SetResponseTimeout(const Value: Integer);
    function GetContentType: string;
    procedure SetContentType(const Value: string);
    // Adaptador interno para el evento de FPC
    procedure InternalOnDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
  private
    function GetAccept: string;
    procedure SetAccept(const Value: string);
    function GetCustomHeaders: TStrings;
    function GetOnReceiveData: TAiDataReceivedEvent;
    procedure SetOnReceiveData(const Value: TAiDataReceivedEvent);
  public
    // Propiedades de compatibilidad
    property SynchronizeEvents: Boolean read FSynchronizeEvents write FSynchronizeEvents;
    property Asynchronous: Boolean read FAsynchronous write FAsynchronous; // Stub: FPC is always sync
    property ContentType: string read GetContentType write SetContentType;

    // Métodos de compatibilidad
    procedure ConfigureForAsync;
    
    // Métodos estilo NetHTTPClient
    function Post(const AURL: string; ASource: TStream; AResponse: TStream = nil; const AHeaders: TNetHeaders = nil): IHTTPResponse; overload;
    function Post(const AURL: string; ABody: TMultipartFormData; AResponse: TStream = nil): IHTTPResponse; overload;
    function Post(const AURL: string; ABody: TMultipartFormData; AResponse: TStream; const AHeaders: TNetHeaders): IHTTPResponse; overload;
    function Get(const AURL: string; AResponse: TStream = nil; const AHeaders: TNetHeaders = nil): IHTTPResponse; overload;
    function Delete(const AURL: string; AResponse: TStream = nil; const AHeaders: TNetHeaders = nil): IHTTPResponse; overload;
    
    // Helpers existentes (mantenidos por compatibilidad)
    function GetString(const AURL: string): string;
    function PostString(const AURL, AContent: string): string;
    procedure SetHeader(const AName, AValue: string);
    /// <summary>COMPAT: No-op en FPC (TFPHTTPClient no tiene SendTimeout).
    /// Permite código sin guards {$IFNDEF FPC}.</summary>
    procedure SetSendTimeout(const AValue: Integer);

    // Propiedades estilo Delphi
    property Accept: string read GetAccept write SetAccept;
    property CustomHeaders: TStrings read GetCustomHeaders;
    property OnReceiveData: TAiDataReceivedEvent read GetOnReceiveData write SetOnReceiveData;
    property OnRequestError: TRequestErrorEvent read FOnRequestError write FOnRequestError;
    property OnRequestCompleted: TRequestCompletedEvent read FOnRequestCompleted write FOnRequestCompleted;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property ResponseTimeout: Integer read GetResponseTimeout write SetResponseTimeout;
  end;
  
  {$ELSE}
  // En Delphi TNetHTTPClient, TMultipartFormData e IHTTPResponse ya existen en System.Net.*
  // Exportamos aliases para compatibilidad con código que usa nombres del helper FPC
  TNetHeaders = System.Net.URLClient.TNetHeaders;
  
  // Alias para tipos de evento - permite código uniforme en ambos compiladores
  // TAiDataReceivedEvent usa el tipo nativo de Delphi TReceiveDataEvent
  TAiDataReceivedEvent = System.Net.HttpClient.TReceiveDataEvent;
  // TRequestErrorEvent y TRequestCompletedEvent: definidos como procedure of object
  // compatible con la firma que usa el código principal
  TRequestErrorEvent = procedure(const Sender: TObject; const AError: string) of object;
  TRequestCompletedEvent = procedure(const Sender: TObject; const AResponse: IHTTPResponse) of object;

  // Polyfill for TRequestExceptionEvent (Delphi 12+)
  {$IF CompilerVersion < 36}
  TRequestExceptionEvent = procedure(const Sender: TObject; const AError: Exception) of object;
  {$IFEND}

type
  // TMultipartFormDataHelper - Polyfill para AddStream con AOwnsStream (Delphi 12+)
  TMultipartFormDataHelper = class helper for TMultipartFormData
  public
    procedure AddStream(const AFieldName: string; AStream: TStream;
      AOwnsStream: Boolean; const AFileName: string = '';
      const AContentType: string = ''); overload;
  end;

  // ==========================================================================
  // TNetHTTPClientHelper - Configuración cross-version para operaciones HTTP
  // ==========================================================================
  //
  // IMPORTANTE: SynchronizeEvents y el riesgo de DEADLOCK
  // ------------------------------------------------------
  //
  // TNetHTTPClient tiene una propiedad SynchronizeEvents (Delphi 10.3+) que controla
  // cómo se ejecutan los callbacks (OnReceiveData, OnRequestCompleted, etc.):
  //
  // SynchronizeEvents = TRUE (default):
  //   - Los callbacks se ejecutan en el HILO PRINCIPAL automáticamente.
  //   - Conveniente: puedes actualizar UI directamente en los callbacks.
  //   - PELIGRO: Si el hilo principal está bloqueado (ej: TTask.WaitForAll),
  //     y el hilo HTTP intenta sincronizar un callback, ambos se bloquean → DEADLOCK.
  //
  // SynchronizeEvents = FALSE (nuestro estándar con ConfigureForAsync):
  //   - Los callbacks se ejecutan en el HILO HTTP directamente.
  //   - Seguro: No hay riesgo de deadlock con WaitForAll u otras esperas.
  //   - Responsabilidad: Si actualizas UI en callbacks, DEBES usar TThread.Queue:
  //
  //       Client.OnReceiveData := procedure(...)
  //       begin
  //         TThread.Queue(nil, procedure
  //         begin
  //           ProgressBar.Position := X;  // Thread-safe
  //         end);
  //       end;
  //
  // MakerAI usa SynchronizeEvents = FALSE porque:
  //   1. Usamos TTask.Run + WaitForAll extensivamente.
  //   2. Preferimos control explícito del threading.
  //   3. Es compatible con nuestro shim FPC (que no tiene SynchronizeEvents).
  //
  // ==========================================================================
  TNetHTTPClientHelper = class helper for TNetHTTPClient
  public
    /// <summary>
    /// Configura el cliente HTTP para operaciones asíncronas seguras.
    /// En Delphi 10.4+: Establece SynchronizeEvents := False para evitar deadlocks.
    /// En versiones anteriores y FPC: No-op (la propiedad no existe).
    /// 
    /// SEGURIDAD EN AMBOS MODOS:
    /// - Modo SÍNCRONO (Asynchronous=False): SynchronizeEvents no tiene efecto
    ///   porque no hay callbacks que sincronizar. Es seguro llamar ConfigureForAsync.
    /// - Modo ASÍNCRONO (Asynchronous=True): SynchronizeEvents=False es CRÍTICO
    ///   para evitar deadlocks con TTask.WaitForAll u otras esperas.
    /// 
    /// IMPORTANTE: Si usas callbacks que actualizan UI (OnReceiveData, etc.),
    /// debes envolver las actualizaciones con TThread.Queue.
    /// </summary>
    procedure ConfigureForAsync;
    /// <summary>Asigna OnRequestException de forma segura (solo funcional en Delphi 12+)</summary>
    procedure SetOnRequestException(const AHandler: TRequestExceptionEvent);
    /// <summary>COMPAT: Wrapper unificado para asignar headers HTTP
    /// En Delphi 12+ usa CustomHeaders[], en versiones antiguas usa AddHeader
    /// </summary>
    procedure SetHeader(const AName, AValue: string);
    /// <summary>COMPAT: Asigna SendTimeout de forma segura.
    /// En Delphi 10.3+ (Rio): Asigna la propiedad SendTimeout.
    /// En Delphi 10.2 y FPC: No-op (la propiedad no existe).
    /// </summary>
    procedure SetSendTimeout(const AValue: Integer);
  end;
  {$ENDIF}

function CreateHttpClient(AOwner: TComponent = nil): TNetHTTPClient; deprecated 'Use TNetHTTPClient.Create + ConfigureForAsync';
function ExtractResponseString(const AResponse: IHTTPResponse): string;
// Delphi: IHTTPResponse tiene StatusCode nativo. FPC usa implementación diferente en TNetHTTPClient shim.
{$IFNDEF FPC}
function GetResponseStatusCode(const AResponse: IHTTPResponse): Integer;
{$ENDIF}

// Helper para TMultipartFormData.AddStream - abstrae diferencias de firma entre versiones Delphi
// Delphi < 11: AddStream(Name, Stream, FileName, ContentType)
// Delphi 11+:  AddStream(Name, Stream, FreeStream, FileName, ContentType)
// FPC:         AddStream(Name, Stream, FreeStream, FileName, ContentType)
procedure AddStreamToMultipart(Body: TMultipartFormData; const AName: string; 
  AStream: TStream; AFreeStream: Boolean; const AFileName, AContentType: string);

function ParseUriCompat(const AUrl: string): TAiURI;
function ExtractFileNameFromUrl(const AUrl: string): string;

/// <summary>Copia el contenido de una respuesta HTTP a un stream destino (cross-platform).</summary>
procedure CopyResponseToStream(const AResponse: IHTTPResponse; ADestStream: TStream);

implementation

function CreateHttpClient(AOwner: TComponent = nil): TNetHTTPClient;
begin
  Result := TNetHTTPClient.Create(AOwner);
  Result.ConfigureForAsync;
end;

procedure CopyResponseToStream(const AResponse: IHTTPResponse; ADestStream: TStream);
var
  LTempStream: TStringStream;
begin
  {$IFDEF FPC}
  // FPC: nuestro shim IHTTPResponse no tiene ContentStream, usamos ContentAsString
  LTempStream := TStringStream.Create(AResponse.ContentAsString, TEncoding.UTF8);
  try
    ADestStream.CopyFrom(LTempStream, 0);
  finally
    LTempStream.Free;
  end;
  {$ELSE}
  // Delphi: ContentStream nativo
  ADestStream.CopyFrom(AResponse.ContentStream, 0);
  {$ENDIF}
end;

// Delphi: usa IHTTPResponse de System.Net.HttpClient. FPC tiene implementación propia abajo.
{$IFNDEF FPC}
function ExtractResponseString(const AResponse: IHTTPResponse): string;
begin
  Result := AResponse.ContentAsString(TEncoding.UTF8); // Force UTF8 for consistency
end;

function GetResponseStatusCode(const AResponse: IHTTPResponse): Integer;
begin
  Result := AResponse.StatusCode;
end;
{$ELSE}
function ExtractResponseString(const AResponse: IHTTPResponse): string;
begin
  Result := AResponse.ContentAsString;
end;
{$ENDIF}

{$IFDEF FPC}
type
  THTTPResponseImpl = class(TInterfacedObject, IHTTPResponse)
  private
    FStatusCode: Integer;
    FStatusText: string;
    FContent: string;
    FHeaders: TStringList;
  public
    constructor Create(Code: Integer; const Text, Content: string; AHeaders: TStrings = nil);
    destructor Destroy; override;
    function GetStatusCode: Integer;
    function GetStatusText: string;
    function GetContentAsString: string;
    function GetHeaderValue(const AName: string): string;
  end;

{ TNetHeader }

class function TNetHeader.Create(const AName, AValue: string): TNetHeader;
begin
  Result.Name := AName;
  Result.Value := AValue;
end;

{ THTTPResponseImpl }

constructor THTTPResponseImpl.Create(Code: Integer; const Text, Content: string; AHeaders: TStrings);
begin
  inherited Create;
  FStatusCode := Code;
  FStatusText := Text;
  FContent := Content;
  FHeaders := TStringList.Create;
  if Assigned(AHeaders) then
    FHeaders.Assign(AHeaders);
end;

destructor THTTPResponseImpl.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function THTTPResponseImpl.GetHeaderValue(const AName: string): string;
begin
  Result := FHeaders.Values[AName]; // TStringList name-value pair lookup
end;

function THTTPResponseImpl.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function THTTPResponseImpl.GetStatusText: string;
begin
  Result := FStatusText;
end;

function THTTPResponseImpl.GetContentAsString: string;
begin
  Result := FContent;
end;

{ TNetHTTPClient }

function TNetHTTPClient.GetContentType: string;
begin
  Result := RequestHeaders.Values['Content-Type'];
end;

procedure TNetHTTPClient.SetContentType(const Value: string);
begin
  AddHeader('Content-Type', Value);
end;

function TNetHTTPClient.GetAccept: string;
begin
  Result := RequestHeaders.Values['Accept'];
end;

procedure TNetHTTPClient.SetAccept(const Value: string);
begin
  AddHeader('Accept', Value);
end;

function TNetHTTPClient.GetCustomHeaders: TStrings;
begin
  Result := RequestHeaders;
end;

// Adaptador para el evento
procedure TNetHTTPClient.InternalOnDataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
var
  Abort: Boolean;
begin
  if Assigned(FUserOnReceiveData) then
  begin
    Abort := False;
    FUserOnReceiveData(Self, ContentLength, CurrentPos, Abort);
    if Abort then
      Terminate; // TFPHTTPClient.Terminate para abortar
  end;
end;

function TNetHTTPClient.GetOnReceiveData: TAiDataReceivedEvent;
begin
  Result := FUserOnReceiveData;
end;


procedure TNetHTTPClient.SetOnReceiveData(const Value: TAiDataReceivedEvent);
begin
  FUserOnReceiveData := Value;
  if Assigned(Value) then
    inherited OnDataReceived := InternalOnDataReceived
  else
    inherited OnDataReceived := nil;
end;

procedure TNetHTTPClient.ConfigureForAsync;
begin
  // Dummy implementation for API compatibility with Delphi
  // FPC TFPHTTPClient operates differently regarding async/sync events
  self.FSynchronizeEvents := False;
end;

function TNetHTTPClient.GetConnectionTimeout: Integer;
begin
  Result := ConnectTimeout;
end;

procedure TNetHTTPClient.SetConnectionTimeout(const Value: Integer);
begin
  ConnectTimeout := Value;
end;

function TNetHTTPClient.GetResponseTimeout: Integer;
begin
  Result := IOTimeout;
end;

procedure TNetHTTPClient.SetResponseTimeout(const Value: Integer);
begin
  IOTimeout := Value;
end;

function TNetHTTPClient.GetString(const AURL: string): string;
begin
  Result := inherited Get(AURL);
end;

function TNetHTTPClient.PostString(const AURL, AContent: string): string;
var
  InStream, OutStream: TStringStream;
begin
  InStream := TStringStream.Create(AContent, TEncoding.UTF8);
  OutStream := TStringStream.Create('', TEncoding.UTF8);
  try
    Self.Post(AURL, InStream, OutStream);
    Result := OutStream.DataString;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

procedure TNetHTTPClient.SetHeader(const AName, AValue: string);
begin
  AddHeader(AName, AValue);
end;

procedure TNetHTTPClient.SetSendTimeout(const AValue: Integer);
begin
  // No-op: FPC TFPHTTPClient no soporta SendTimeout.
  // La propiedad IOTimeout (mapeada como ResponseTimeout) cubre lectura.
end;

function TNetHTTPClient.Post(const AURL: string; ASource: TStream; AResponse: TStream; const AHeaders: TNetHeaders): IHTTPResponse;
var
  RespStream: TMemoryStream;
  UseInternalStream: Boolean;
  H: TNetHeader;
  ResString: string;
begin
  // Set Headers
  for H in AHeaders do
    AddHeader(H.Name, H.Value);
    
  // Set Body
  if Assigned(ASource) then
  begin
    RequestBody := ASource;
    ASource.Position := 0;
  end;
  
  // Prepare Response Target
  UseInternalStream := AResponse = nil;
  if UseInternalStream then
    AResponse := TMemoryStream.Create;
    
  try
    try
      // Execute FPC Post
      inherited Post(AURL, AResponse);
      
      // Capture result for Interface
      if AResponse is TStringStream then
        ResString := TStringStream(AResponse).DataString
      else if AResponse is TMemoryStream then
      begin
        SetLength(ResString, AResponse.Size);
        if AResponse.Size > 0 then
          Move(TMemoryStream(AResponse).Memory^, ResString[1], AResponse.Size);
      end;
      
      Result := THTTPResponseImpl.Create(ResponseStatusCode, ResponseStatusText, ResString, Self.ResponseHeaders);
    except
      on E: Exception do
        // Propagate exception or handle? Delphi might raise.
        raise; 
    end;
  finally
    if UseInternalStream then
      AResponse.Free;
  end;
end;

function TNetHTTPClient.Post(const AURL: string; ABody: TMultipartFormData; AResponse: TStream): IHTTPResponse;
var
  UseInternalStream: Boolean;
  ResString: string;
  BodyStream: TStream;
begin
  // Set Content-Type with boundary
  ContentType := ABody.GetContentType;
  
  // Get the body stream
  BodyStream := ABody.GetStream;
  RequestBody := BodyStream;
  
  // Prepare Response Target
  UseInternalStream := AResponse = nil;
  if UseInternalStream then
    AResponse := TMemoryStream.Create;
    
  try
    try
      // Execute FPC Post
      inherited Post(AURL, AResponse);
      
      // Capture result for Interface
      if AResponse is TStringStream then
        ResString := TStringStream(AResponse).DataString
      else if AResponse is TMemoryStream then
      begin
        AResponse.Position := 0;
        SetLength(ResString, AResponse.Size);
        if AResponse.Size > 0 then
          AResponse.Read(ResString[1], AResponse.Size);
      end;
      
      Result := THTTPResponseImpl.Create(ResponseStatusCode, ResponseStatusText, ResString, Self.ResponseHeaders);
    except
      on E: Exception do
        raise; 
    end;
  finally
    if UseInternalStream then
      AResponse.Free;
  end;
end;

function TNetHTTPClient.Post(const AURL: string; ABody: TMultipartFormData; AResponse: TStream; const AHeaders: TNetHeaders): IHTTPResponse;
var
  H: TNetHeader;
begin
  // Apply headers first
  for H in AHeaders do
    AddHeader(H.Name, H.Value);
  // Delegate to existing implementation
  Result := Post(AURL, ABody, AResponse);
end;

function TNetHTTPClient.Get(const AURL: string; AResponse: TStream; const AHeaders: TNetHeaders): IHTTPResponse;
var
  H: TNetHeader;
  ResString: string;
  UseInternalStream: Boolean;
begin
  for H in AHeaders do
    AddHeader(H.Name, H.Value);
    
  UseInternalStream := AResponse = nil;
  if UseInternalStream then
    AResponse := TMemoryStream.Create;
    
  try
    inherited Get(AURL, AResponse);
    
    if AResponse is TStringStream then
      ResString := TStringStream(AResponse).DataString
    else 
    begin
       AResponse.Position := 0;
       SetLength(ResString, AResponse.Size);
       if AResponse.Size > 0 then
         AResponse.ReadBuffer(Pointer(ResString)^, AResponse.Size);
    end;

    Result := THTTPResponseImpl.Create(ResponseStatusCode, ResponseStatusText, ResString);
  finally
    if UseInternalStream then
      AResponse.Free;
  end;
end;

function TNetHTTPClient.Delete(const AURL: string; AResponse: TStream; const AHeaders: TNetHeaders): IHTTPResponse;
var
  H: TNetHeader;
  ResString: string;
  UseInternalStream: Boolean;
begin
  for H in AHeaders do
    AddHeader(H.Name, H.Value);

  UseInternalStream := AResponse = nil;
  if UseInternalStream then
    AResponse := TMemoryStream.Create;

  try
    HTTPMethod('DELETE', AURL, AResponse, []);

    if AResponse is TStringStream then
      ResString := TStringStream(AResponse).DataString
    else
    begin
       AResponse.Position := 0;
       SetLength(ResString, AResponse.Size);
       if AResponse.Size > 0 then
         AResponse.ReadBuffer(Pointer(ResString)^, AResponse.Size);
    end;

    Result := THTTPResponseImpl.Create(ResponseStatusCode, ResponseStatusText, ResString);
  finally
    if UseInternalStream then
      AResponse.Free;
  end;
end;

{$ENDIF}

{$IFDEF FPC}
{ TMultipartFormData - Real FPC Implementation }

constructor TMultipartFormData.Create;
begin
  inherited Create;
  // Generate unique boundary
  FBoundary := '----MakerAIBoundary' + IntToStr(Random(MaxInt)) + IntToStr(GetTickCount64);
  FStream := TMemoryStream.Create;
  FFinalized := False;
end;

destructor TMultipartFormData.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TMultipartFormData.WriteString(const S: string);
var
  Bytes: TBytes;
begin
  if S = '' then Exit;
  Bytes := TEncoding.UTF8.GetBytes(S);
  FStream.WriteBuffer(Bytes[0], Length(Bytes));
end;

procedure TMultipartFormData.WriteBoundary(IsLast: Boolean);
begin
  WriteString('--' + FBoundary);
  if IsLast then
    WriteString('--');
  WriteString(#13#10);
end;

procedure TMultipartFormData.AddField(const AName, AValue: string);
begin
  if FFinalized then
    raise Exception.Create('Cannot add to finalized multipart form');
  WriteBoundary;
  WriteString('Content-Disposition: form-data; name="' + AName + '"' + #13#10);
  WriteString(#13#10);
  WriteString(AValue);
  WriteString(#13#10);
end;

procedure TMultipartFormData.AddStream(const AName: string; AStream: TStream; FreeStream: Boolean; const AFileName, AContentType: string);
var
  Buffer: TBytes;
  ActualContentType: string;
begin
  if FFinalized then
    raise Exception.Create('Cannot add to finalized multipart form');
    
  WriteBoundary;
  WriteString('Content-Disposition: form-data; name="' + AName + '"');
  if AFileName <> '' then
    WriteString('; filename="' + AFileName + '"');
  WriteString(#13#10);
  
  ActualContentType := AContentType;
  if ActualContentType = '' then
    ActualContentType := 'application/octet-stream';
  WriteString('Content-Type: ' + ActualContentType + #13#10);
  WriteString(#13#10);
  
  // Copy stream content
  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    AStream.Position := 0;
    SetLength(Buffer, AStream.Size);
    AStream.ReadBuffer(Buffer[0], AStream.Size);
    FStream.WriteBuffer(Buffer[0], Length(Buffer));
  end;
  WriteString(#13#10);
  
  if FreeStream then
    AStream.Free;
end;

function TMultipartFormData.GetStream: TStream;
begin
  if not FFinalized then
  begin
    WriteBoundary(True); // Close boundary
    FFinalized := True;
  end;
  FStream.Position := 0;
  Result := FStream;
end;

function TMultipartFormData.GetContentType: string;
begin
  Result := 'multipart/form-data; boundary=' + FBoundary;
end;

{$ENDIF}

// Implementación del helper AddStreamToMultipart
// Abstrae las diferencias de firma entre versiones de Delphi y FPC
procedure AddStreamToMultipart(Body: TMultipartFormData; const AName: string;
  AStream: TStream; AFreeStream: Boolean; const AFileName, AContentType: string);
{$IFNDEF FPC}
  {$IF CompilerVersion < 33} // Delphi 10.2: AddStream no existe
var
  TmpFile: string;
  FS: TFileStream;
  {$IFEND}
{$ENDIF}
begin
  {$IFDEF FPC}
  // FPC: Implementación propia con FreeStream como parámetro
  Body.AddStream(AName, AStream, AFreeStream, AFileName, AContentType);
  {$ELSE}
    {$IF CompilerVersion >= 33} // Delphi 10.3+
    Body.AddStream(AName, AStream, AFileName, AContentType);
    {$ELSE}
    // Delphi 10.2: Fallback via archivo temporal
    TmpFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + AFileName;
    FS := TFileStream.Create(TmpFile, fmCreate);
    try
      AStream.Position := 0;
      FS.CopyFrom(AStream, AStream.Size);
    finally
      FS.Free;
    end;
    Body.AddFile(AName, TmpFile);
    {$IFEND}
  {$ENDIF}
end;


function ParseUriCompat(const AUrl: string): TAiURI;
var
  {$IFDEF FPC}
  LUri: TURI;
  {$ELSE}
  LUri: TURI;
  {$ENDIF}
begin
  {$IFDEF FPC}
  LUri := ParseURI(AUrl);
  Result.Scheme := LUri.Protocol;
  Result.Host := LUri.Host;
  Result.Port := LUri.Port;
  Result.Path := LUri.Path;
  Result.Query := LUri.Params;
  {$ELSE}
  LUri := TURI.Create(AUrl);
  Result.Scheme := LUri.Scheme;
  Result.Host := LUri.Host;
  Result.Port := LUri.Port;
  Result.Path := LUri.Path;
  Result.Query := LUri.Query;
  {$ENDIF}
end;

// =============================================================================
// Implementación de Helpers Delphi (fusionado desde uNetHttpHelper.pas)
// =============================================================================
{$IFNDEF FPC}

procedure TMultipartFormDataHelper.AddStream(const AFieldName: string;
  AStream: TStream; AOwnsStream: Boolean; const AFileName: string;
  const AContentType: string);
{$IF CompilerVersion < 33}
var
  LTempFile: string;
  LFileStream: TFileStream;
{$ENDIF}
begin
{$IF CompilerVersion >= 36} // Delphi 12+
  Self.AddStream(AFieldName, AStream, AOwnsStream, AFileName, AContentType);
{$ELSEIF CompilerVersion >= 33} // Delphi 10.3 - 11
  Self.AddStream(AFieldName, AStream, AFileName, AContentType);
  if AOwnsStream then
    ; // Ignorar - el caller debe manejar
{$ELSE}
  // Delphi 10.2 and below
  LTempFile := System.IOUtils.TPath.GetTempPath + ExtractFileName(AFileName);
  LFileStream := TFileStream.Create(LTempFile, fmCreate);
  try
    AStream.Position := 0;
    LFileStream.CopyFrom(AStream, AStream.Size);
  finally
    LFileStream.Free;
    if AOwnsStream then
      AStream.Free;
  end;
  Self.AddFile(AFieldName, LTempFile);
{$ENDIF}
end;

procedure TNetHTTPClientHelper.SetOnRequestException(const AHandler: TRequestExceptionEvent);
begin
{$IF CompilerVersion >= 36}
  Self.OnRequestException := AHandler;
{$ENDIF}
end;

procedure TNetHTTPClientHelper.ConfigureForAsync;
begin
{$IF CompilerVersion >= 34} // Delphi 10.4+ (Sydney)
  Self.SynchronizeEvents := False;
{$ENDIF}
end;

procedure TNetHTTPClientHelper.SetHeader(const AName, AValue: string);
begin
{$IF CompilerVersion >= 35} // Delphi 11+
  Self.CustomHeaders[AName] := AValue;
{$ELSE}
  {$IF CompilerVersion >= 33} // Delphi 10.3-10.4
  Self.CustHeaders := Self.CustHeaders + [TNameValuePair.Create(AName, AValue)];
  {$ELSE}
  // Delphi 10.2: No hay API para set custom headers en TNetHTTPClient.
  // Los headers custom se deben pasar como parámetro en Get/Post.
  {$IFEND}
{$ENDIF}
end;

procedure TNetHTTPClientHelper.SetSendTimeout(const AValue: Integer);
begin
{$IF CompilerVersion >= 33.0} // Delphi 10.3 Rio+
  Self.SendTimeout := AValue;
{$ENDIF}
end;

{$ENDIF}

function ExtractFileNameFromUrl(const AUrl: string): string;
var
  LUri: TAiURI;
begin
  LUri := ParseUriCompat(AUrl);
  Result := ExtractFileName(StringReplace(LUri.Path, '/', PathDelim, [rfReplaceAll]));
end;

end.



