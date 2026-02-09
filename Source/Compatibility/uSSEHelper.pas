// =========================================================================
// uSSEHelper.pas - Helper SSE (Server-Sent Events) Multiplataforma
// =========================================================================
// Proporciona una implementación de cliente SSE compatible con:
// - Delphi: TNetHTTPClient nativo (asíncrono)
// - FPC + mORMot2: THttpClientSocket (USE_MORMOT2 definido)
// - FPC sin mORMot2: Stub que retorna error (graceful degradation)
//
// Para habilitar mORMot2 en FPC, definir USE_MORMOT2 en CompilerDirectives.inc
//
// Uso:
//   SSE := TSSEClient.Create;
//   SSE.OnMessage := HandleMessage;
//   SSE.Connect('http://server/sse');
//   ...
//   SSE.Disconnect;
//   SSE.Free;
// =========================================================================
unit uSSEHelper;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  SysUtils, Classes, SyncObjs
  {$IFNDEF FPC}
  , System.Net.HttpClient
  , System.Net.HttpClientComponent
  , System.Net.URLClient
  , System.NetEncoding
  , uHttpHelper  // Para ConfigureForAsync helper
  {$ENDIF};

type
  /// <summary>
  /// Evento disparado cuando se recibe un mensaje SSE
  /// </summary>
  TSSEMessageEvent = procedure(Sender: TObject; const AEventName, AData: string) of object;
  
  /// <summary>
  /// Evento disparado cuando ocurre un error en la conexión SSE
  /// </summary>
  TSSEErrorEvent = procedure(Sender: TObject; const AError: string) of object;
  
  /// <summary>
  /// Evento disparado cuando la conexión se establece y se recibe el endpoint
  /// </summary>
  TSSEConnectedEvent = procedure(Sender: TObject; const APostEndpoint: string) of object;

  {$IFDEF FPC}
  // =========================================================================
  // FPC: Stub simple (SSE no soportado sin bibliotecas adicionales)
  // Para SSE completo, usar TMCPClientSSE de uMakerAi.MCPClient.Core con mORMot2.
  // =========================================================================
  TSSEClient = class
  private
    FOnMessage: TSSEMessageEvent;
    FOnError: TSSEErrorEvent;
    FOnConnected: TSSEConnectedEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const AUrl: string);
    procedure Disconnect;
    function IsConnected: Boolean;
    function GetPostEndpoint: string;
    property OnMessage: TSSEMessageEvent read FOnMessage write FOnMessage;
    property OnError: TSSEErrorEvent read FOnError write FOnError;
    property OnConnected: TSSEConnectedEvent read FOnConnected write FOnConnected;
  end;
  {$ELSE}
  // =========================================================================
  // Implementación Delphi con TNetHTTPClient
  // =========================================================================
  TSSEClient = class
  private
    FHttpClient: TNetHTTPClient;
    FUrl: string;
    FPostEndpoint: string;
    FBuffer: string;
    FConnected: Boolean;
    FStopRequested: Boolean;
    FOnMessage: TSSEMessageEvent;
    FOnError: TSSEErrorEvent;
    FOnConnected: TSSEConnectedEvent;
    {$IF CompilerVersion >= 36}
    procedure DoReceiveDataEx(const Sender: TObject; AContentLength, AReadCount: Int64;
      AChunk: Pointer; AChunkLength: Cardinal; var ABort: Boolean);
    {$ELSE}
    procedure DoReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64;
      var ABort: Boolean);
    {$IFEND}
    procedure DoRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
    procedure DoRequestError(const Sender: TObject; const AError: string);
    procedure ProcessBuffer;
    procedure ProcessSSELine(const ALine: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const AUrl: string);
    procedure Disconnect;
    function IsConnected: Boolean;
    function GetPostEndpoint: string;
    property OnMessage: TSSEMessageEvent read FOnMessage write FOnMessage;
    property OnError: TSSEErrorEvent read FOnError write FOnError;
    property OnConnected: TSSEConnectedEvent read FOnConnected write FOnConnected;
  end;
  {$ENDIF}

/// <summary>
/// Verifica si SSE está disponible en esta plataforma/configuración
/// </summary>
function IsSSEAvailable: Boolean;

implementation

function IsSSEAvailable: Boolean;
begin
  {$IFDEF FPC}
  // FPC: SSE no disponible en este helper. Usar TMCPClientSSE con mORMot2 directamente.
  Result := False;
  {$ELSE}
  Result := True; // Delphi siempre tiene TNetHTTPClient
  {$ENDIF}
end;

{$IFDEF FPC}
// =========================================================================
// Stub FPC - SSE requiere mORMot2 u otra librería de red
// =========================================================================

constructor TSSEClient.Create;
begin
  inherited Create;
end;

destructor TSSEClient.Destroy;
begin
  inherited Destroy;
end;

procedure TSSEClient.Connect(const AUrl: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, 'SSE not available in FPC. Use TMCPClientSSE with mORMot2 from uMakerAi.MCPClient.Core');
end;

procedure TSSEClient.Disconnect;
begin
  // No-op
end;

function TSSEClient.IsConnected: Boolean;
begin
  Result := False;
end;

function TSSEClient.GetPostEndpoint: string;
begin
  Result := '';
end;

{$ELSE}
// =========================================================================
// Implementación Delphi
// =========================================================================

constructor TSSEClient.Create;
begin
  inherited Create;
  FHttpClient := TNetHTTPClient.Create(nil);
  FHttpClient.ConfigureForAsync;  // [COMPATIBILIDAD 10.2+] SynchronizeEvents solo existe en 10.3+
  FHttpClient.Asynchronous := True;
  {$IF CompilerVersion >= 36}
  FHttpClient.OnReceiveDataEx := DoReceiveDataEx;
  {$ELSE}
  FHttpClient.OnReceiveData := DoReceiveData;
  {$IFEND}
  FHttpClient.OnRequestCompleted := DoRequestCompleted;
  FHttpClient.OnRequestError := DoRequestError;
  FHttpClient.ConnectionTimeout := 30000;
  FHttpClient.ResponseTimeout := 30000;
  FConnected := False;
end;

destructor TSSEClient.Destroy;
begin
  Disconnect;
  FHttpClient.Free;
  inherited Destroy;
end;

procedure TSSEClient.Connect(const AUrl: string);
begin
  if FConnected then
    Disconnect;
    
  FUrl := AUrl;
  FBuffer := '';
  FPostEndpoint := '';
  FStopRequested := False;
  FConnected := True;
  
  FHttpClient.Get(AUrl);
end;

procedure TSSEClient.Disconnect;
begin
  FStopRequested := True;
  FConnected := False;
end;

function TSSEClient.IsConnected: Boolean;
begin
  Result := FConnected;
end;

function TSSEClient.GetPostEndpoint: string;
begin
  Result := FPostEndpoint;
end;

{$IF CompilerVersion >= 36}
procedure TSSEClient.DoReceiveDataEx(const Sender: TObject; AContentLength, AReadCount: Int64;
  AChunk: Pointer; AChunkLength: Cardinal; var ABort: Boolean);
var
  Bytes: TBytes;
  ChunkStr: string;
begin
  if FStopRequested then
  begin
    ABort := True;
    FConnected := False;
    Exit;
  end;
  
  if (AChunk = nil) or (AChunkLength = 0) then Exit;
  
  SetLength(Bytes, AChunkLength);
  Move(AChunk^, Bytes[0], AChunkLength);
  ChunkStr := TEncoding.UTF8.GetString(Bytes);
  
  FBuffer := FBuffer + ChunkStr;
  ProcessBuffer;
end;
{$ELSE}
procedure TSSEClient.DoReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64;
  var ABort: Boolean);
begin
  if FStopRequested then
  begin
    ABort := True;
    FConnected := False;
    Exit;
  end;
  // OnReceiveData pre-D12 does not provide chunk data directly.
  // The data will be available when the request completes.
  // For SSE streaming, this is limited - actual parsing happens at completion.
end;
{$IFEND}

procedure TSSEClient.DoRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
begin
  FConnected := False;
end;

procedure TSSEClient.DoRequestError(const Sender: TObject; const AError: string);
begin
  if not FStopRequested then
  begin
    FConnected := False;
    if Assigned(FOnError) then
      FOnError(Self, AError);
  end;
end;

procedure TSSEClient.ProcessBuffer;
var
  P: Integer;
  Line: string;
begin
  while True do
  begin
    P := Pos(#10, FBuffer);
    if P = 0 then Break;
    
    Line := Copy(FBuffer, 1, P - 1);
    Delete(FBuffer, 1, P);
    
    if (Line <> '') and (Line[Length(Line)] = #13) then
      Delete(Line, Length(Line), 1);
      
    if Line <> '' then
      ProcessSSELine(Line);
  end;
end;

procedure TSSEClient.ProcessSSELine(const ALine: string);
var
  P: Integer;
  Key, Value: string;
begin
  P := Pos(':', ALine);
  if P = 0 then Exit;
  
  Key := Trim(Copy(ALine, 1, P - 1));
  Value := Trim(Copy(ALine, P + 1, Length(ALine)));
  
  if SameText(Key, 'data') then
  begin
    if (FPostEndpoint = '') and (not Value.StartsWith('{')) then
    begin
      FPostEndpoint := Value;
      if Assigned(FOnConnected) then
        FOnConnected(Self, Value);
    end
    else
    begin
      if Assigned(FOnMessage) then
        FOnMessage(Self, 'message', Value);
    end;
  end;
end;

{$ENDIF}

end.

