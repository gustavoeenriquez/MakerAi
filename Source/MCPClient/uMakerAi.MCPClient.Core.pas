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
// Clientes MCP (Model Context Protocol) para FPC.
// Implementa los tres transportes principales:
//   - TMCPClientStdIo  : lanza proceso externo y se comunica por pipes
//   - TMCPClientHttp   : POST JSON-RPC a endpoint HTTP
//   - TMCPClientSSE    : handshake SSE + POST /messages (experimental)
//
// Adaptaciones respecto a la version Delphi:
//   - TNetHTTPClient      → TFPHTTPClient (fphttpclient)
//   - TThreadedQueue<T>   → TJsonMessageQueue (implementacion manual con lock)
//   - TObjectList<T>      → specialize TObjectList<TAiMediaFile> / TObject list
//   - TPath.GetHomePath   → GetEnvironmentVariable('HOME') o USERPROFILE
//   - System.JSON         → fpjson + jsonparser
//   - TUtilsSystem.Start/StopInteractiveProcess → implementacion simplificada FPC
//   - TObject.ToString    → IntToStr
//
// NOTA: TMCPClientStdIo requiere que el proceso MCP externo siga el protocolo
// JSON-RPC 2.0 sobre stdin/stdout (un mensaje JSON por linea).

unit uMakerAi.MCPClient.Core;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs, Contnrs,
  fpjson, jsonparser,
  fphttpclient,
  uMakerAi.Core;

type
  // -------------------------------------------------------------------------
  // Tipos de transporte MCP
  // -------------------------------------------------------------------------
  TToolTransportType = (tpStdIo, tpHttp, tpSSE, tpMakerAi);

  // -------------------------------------------------------------------------
  // Eventos MCP
  // -------------------------------------------------------------------------
  TMCPStatusEvent       = procedure(Sender: TObject;
      const StatusMsg: string) of object;
  TMCPLogEvent          = procedure(Sender: TObject;
      const Msg: string) of object;
  TMCPStreamMessageEvent = procedure(Sender: TObject;
      const EventType, Data: string) of object;

  // -------------------------------------------------------------------------
  // EMCPClientException - excepcion especifica de clientes MCP
  // -------------------------------------------------------------------------
  EMCPClientException = class(Exception);

  // -------------------------------------------------------------------------
  // TJsonMessageQueue - cola thread-safe de TJSONObject
  // (reemplaza TThreadedQueue<TJSONObject> de Delphi)
  // -------------------------------------------------------------------------
  TJsonMessageQueue = class(TObject)
  private
    FLock     : TCriticalSection;
    FMessages : TObjectList; // OwnsObjects = True
  public
    constructor Create;
    destructor Destroy; override;

    // Encola un objeto JSON (toma posesion)
    procedure Enqueue(AMsg: TJSONObject);

    // Dequeue con timeout: retorna nil si no hay mensaje o timeout
    // El llamador es responsable de liberar el TJSONObject retornado.
    function Dequeue(ATimeoutMs: Cardinal): TJSONObject;

    function Count: Integer;
  end;

  // -------------------------------------------------------------------------
  // TInteractiveProcessInfo - informacion de proceso interactivo
  // (equivalente simplificado del Delphi, usando TProcess de FPC)
  // -------------------------------------------------------------------------
  TInteractiveProcessInfo = class(TObject)
  private
    FIsRunning : Boolean;
  public
    {$IFDEF MSWINDOWS}
    // Handles para comunicacion en Windows
    StdinWrite  : THandle;
    StdoutRead  : THandle;
    ProcessHandle: THandle;
    {$ELSE}
    // En Unix, usamos TProcess de FPC
    FProcess: TObject; // TProcess — type forward para evitar dep. circular
    {$ENDIF}
    InputPipe  : TStream; // stream de escritura al stdin del proceso
    OutputPipe : TStream; // stream de lectura del stdout del proceso

    constructor Create;
    destructor Destroy; override;

    property IsRunning: Boolean read FIsRunning write FIsRunning;
  end;

  // -------------------------------------------------------------------------
  // TMCPClientCustom - clase base abstracta
  // -------------------------------------------------------------------------
  TMCPClientCustom = class(TComponent)
  private
    FOnLog          : TMCPLogEvent;
    FOnStatusUpdate : TMCPStatusEvent;
    FOnStreamMessage: TMCPStreamMessageEvent;
    FTransportType  : TToolTransportType;
    FInitialized    : Boolean;
    FEnabled        : Boolean;
    FTools          : TStringList;
    FAvailable      : Boolean;
    FParams         : TStringList;
    FEnvVars        : TStringList;
    FURL            : string;

    procedure SetTransportType(const AValue: TToolTransportType);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetInitialized(const AValue: Boolean);
    procedure SetAvailable(const AValue: Boolean);
    procedure SetURL(const AValue: string);

  protected
    FLastError : string;
    FBusy      : Boolean;

    procedure DoLog(const AMsg: string); virtual;
    procedure DoStatusUpdate(const AStatusMsg: string); virtual;
    procedure DoStreamMessage(const AEventType, AData: string); virtual;

    function IsBinaryContentType(const AContentType: string): Boolean;

    // Extrae TAiMediaFile de un result JSON con content[]
    // Retorna el JSON limpiado (el llamador libera ambos).
    function ProcessAndExtractMedia(const AJsonResult: TJSONObject;
        AExtractedMedia: TObjectList): TJSONObject;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetParamByName(const AParamName: string): string;
    function GetDefaultParams: TStringList; virtual;

    // Inicializa el cliente: llama ListTools y guarda resultado en FTools.
    // Retorna True si exitoso, False si fallo (Enabled := False).
    function Initialize: Boolean; virtual;

    function ListTools: TJSONObject; virtual;

    // AExtractedMedia: TObjectList — recibe TAiMediaFile extraidos.
    // Puede ser nil si no se necesitan los media.
    function CallTool(const AToolName: string; AArguments: TJSONObject;
        AExtractedMedia: TObjectList): TJSONObject; overload; virtual;
    function CallTool(const AToolName: string; AArguments: TStrings;
        AExtractedMedia: TObjectList): TJSONObject; overload; virtual;

    property TransportType : TToolTransportType read FTransportType
        write SetTransportType;
    property Tools         : TStringList read FTools;
    property Initialized   : Boolean read FInitialized write SetInitialized;
    property Enabled       : Boolean read FEnabled write SetEnabled default True;
    property Available     : Boolean read FAvailable write SetAvailable;
    property Params        : TStringList read FParams;
    property EnvVars       : TStringList read FEnvVars;
    property URL           : string read FURL write SetURL;
    property LastError     : string read FLastError;

    property OnLog          : TMCPLogEvent read FOnLog write FOnLog;
    property OnStatusUpdate : TMCPStatusEvent
        read FOnStatusUpdate write FOnStatusUpdate;
    property OnStreamMessage: TMCPStreamMessageEvent
        read FOnStreamMessage write FOnStreamMessage;
  end;

  // =========================================================================
  // TMCPClientHttp - cliente MCP sobre HTTP (POST JSON-RPC)
  // =========================================================================
  TMCPClientHttp = class(TMCPClientCustom)
  private
    FHttpClient       : TFPHTTPClient;
    FRequestIDCounter : Integer;
    FServerCapabilities: TJSONObject;

    // Ejecuta un JSON-RPC request contra FURL via HTTP POST.
    // Retorna el 'result' (el llamador libera).
    // Lanza EMCPClientException si hay error.
    function InternalSendRequest(const AMethod: string;
        AParams: TJSONObject): TJSONObject;

    procedure InternalPerformMCPInitialize;
    procedure InternalSendInitializedNotification;

    procedure ConfigureHttpAuth;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Initialize: Boolean; override;
    function ListTools: TJSONObject; override;

    function CallTool(const AToolName: string; AArguments: TJSONObject;
        AExtractedMedia: TObjectList): TJSONObject; overload; override;
    function CallTool(const AToolName: string; AArguments: TStrings;
        AExtractedMedia: TObjectList): TJSONObject; overload; override;
  end;

  // =========================================================================
  // TStdioReadThread - lee stdout del proceso MCP externo
  // =========================================================================
  TStdioReadThread = class;

  // =========================================================================
  // TMCPClientStdIo - cliente MCP sobre stdin/stdout (proceso externo)
  // =========================================================================
  TMCPClientStdIo = class(TMCPClientCustom)
  private
    FProcess          : TInteractiveProcessInfo;
    FReadThread       : TStdioReadThread;
    FIncomingMessages : TJsonMessageQueue;
    FRequestIDCounter : Integer;
    FIsRunning        : Boolean;

    procedure InternalStartServerProcess;
    procedure InternalStopServerProcess;

    procedure InternalSendRawMessage(const AJsonString: string);
    function  InternalReceiveResponse(AExpectedID: Integer;
        ATimeoutMs: Cardinal = 15000): TJSONObject;

    function  InternalInitialize: TJSONObject;
    procedure InternalSendInitializedNotification;
    function  InternalListTools: TJSONObject;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Initialize: Boolean; override;
    function ListTools: TJSONObject; override;

    function CallTool(const AToolName: string; AArguments: TJSONObject;
        AExtractedMedia: TObjectList): TJSONObject; overload; override;
    function CallTool(const AToolName: string; AArguments: TStrings;
        AExtractedMedia: TObjectList): TJSONObject; overload; override;
  end;

  // -------------------------------------------------------------------------
  // TStdioReadThread - hilo que lee stdout del proceso MCP
  // -------------------------------------------------------------------------
  TStdioReadThread = class(TThread)
  private
    FClient: TMCPClientStdIo;
  public
    constructor Create(AClient: TMCPClientStdIo);
    procedure Execute; override;
  end;

  // =========================================================================
  // TMCPClientSSE - cliente MCP sobre SSE + POST (experimental)
  // =========================================================================
  TMCPClientSSE = class(TMCPClientCustom)
  private
    FHttpClient       : TFPHTTPClient;
    FRequestIDCounter : Integer;
    FIncomingMessages : TJsonMessageQueue;
    FPostEndpoint     : string;
    FIsConnected      : Boolean;
    FSSEThread        : TThread;
    FStopRequested    : Boolean;

    function  InternalSendRequest(const AMethod: string;
        AParams: TJSONObject): TJSONObject;
    function  InternalReceiveResponse(AExpectedID: Integer;
        ATimeoutMs: Cardinal = 15000): TJSONObject;
    procedure StartEventStream;
    procedure StopEventStream;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Initialize: Boolean; override;
    function ListTools: TJSONObject; override;

    function CallTool(const AToolName: string; AArguments: TJSONObject;
        AExtractedMedia: TObjectList): TJSONObject; overload; override;
    function CallTool(const AToolName: string; AArguments: TStrings;
        AExtractedMedia: TObjectList): TJSONObject; overload; override;
  end;

implementation

uses
  StrUtils, Process;

// ---------------------------------------------------------------------------
// TJsonMessageQueue
// ---------------------------------------------------------------------------

constructor TJsonMessageQueue.Create;
begin
  inherited Create;
  FLock     := TCriticalSection.Create;
  FMessages := TObjectList.Create(True); // OwnsObjects
end;

destructor TJsonMessageQueue.Destroy;
begin
  FLock.Free;
  FMessages.Free;
  inherited;
end;

procedure TJsonMessageQueue.Enqueue(AMsg: TJSONObject);
begin
  FLock.Enter;
  try
    FMessages.Add(AMsg);
  finally
    FLock.Leave;
  end;
end;

function TJsonMessageQueue.Dequeue(ATimeoutMs: Cardinal): TJSONObject;
var
  Elapsed: Cardinal;
begin
  Result  := nil;
  Elapsed := 0;

  while Elapsed < ATimeoutMs do
  begin
    FLock.Enter;
    try
      if FMessages.Count > 0 then
      begin
        Result := TJSONObject(FMessages[0]);
        FMessages.OwnsObjects := False;
        FMessages.Delete(0);
        FMessages.OwnsObjects := True;
        Exit;
      end;
    finally
      FLock.Leave;
    end;
    Sleep(10);
    Inc(Elapsed, 10);
  end;
end;

function TJsonMessageQueue.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FMessages.Count;
  finally
    FLock.Leave;
  end;
end;

// ---------------------------------------------------------------------------
// TInteractiveProcessInfo
// ---------------------------------------------------------------------------

constructor TInteractiveProcessInfo.Create;
begin
  inherited Create;
  FIsRunning  := False;
  InputPipe   := nil;
  OutputPipe  := nil;
{$IFDEF MSWINDOWS}
  StdinWrite    := 0;
  StdoutRead    := 0;
  ProcessHandle := 0;
{$ELSE}
  FProcess := nil;
{$ENDIF}
end;

destructor TInteractiveProcessInfo.Destroy;
begin
  FIsRunning := False;
  // InputPipe y OutputPipe son gestionados por TProcess — no liberar aqui
{$IFNDEF MSWINDOWS}
  if Assigned(FProcess) then
  begin
    TProcess(FProcess).Free;
    FProcess := nil;
  end;
{$ENDIF}
  inherited;
end;

// ---------------------------------------------------------------------------
// TMCPClientCustom
// ---------------------------------------------------------------------------

constructor TMCPClientCustom.Create(AOwner: TComponent);
var
  Defaults: TStringList;
begin
  inherited Create(AOwner);
  FTransportType := tpStdIo;
  FInitialized   := False;
  FEnabled       := True;
  FAvailable     := False;
  FBusy          := False;
  FLastError     := '';
  FURL           := '';

  FTools   := TStringList.Create;
  FParams  := TStringList.Create;
  FEnvVars := TStringList.Create;

  Defaults := GetDefaultParams;
  try
    FParams.Assign(Defaults);
  finally
    Defaults.Free;
  end;
end;

destructor TMCPClientCustom.Destroy;
begin
  FTools.Free;
  FParams.Free;
  FEnvVars.Free;
  inherited;
end;

procedure TMCPClientCustom.DoLog(const AMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, AMsg);
end;

procedure TMCPClientCustom.DoStatusUpdate(const AStatusMsg: string);
begin
  if Assigned(FOnStatusUpdate) then
    FOnStatusUpdate(Self, AStatusMsg);
end;

procedure TMCPClientCustom.DoStreamMessage(const AEventType, AData: string);
begin
  if Assigned(FOnStreamMessage) then
    FOnStreamMessage(Self, AEventType, AData);
end;

procedure TMCPClientCustom.SetTransportType(const AValue: TToolTransportType);
begin
  FTransportType := AValue;
end;

procedure TMCPClientCustom.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TMCPClientCustom.SetInitialized(const AValue: Boolean);
begin
  FInitialized := AValue;
end;

procedure TMCPClientCustom.SetAvailable(const AValue: Boolean);
begin
  FAvailable := AValue;
end;

procedure TMCPClientCustom.SetURL(const AValue: string);
begin
  FURL := AValue;
  FParams.Values['URL'] := AValue;
end;

function TMCPClientCustom.GetParamByName(const AParamName: string): string;
begin
  Result := FParams.Values[AParamName];
end;

function TMCPClientCustom.GetDefaultParams: TStringList;
var
  HomeDir: string;
begin
  Result := TStringList.Create;

  HomeDir := GetEnvironmentVariable('HOME');
  if HomeDir = '' then
    HomeDir := GetEnvironmentVariable('USERPROFILE');
  if HomeDir = '' then
    HomeDir := GetEnvironmentVariable('HOMEPATH');
  if HomeDir = '' then
    HomeDir := '/tmp';

  Result.Add('Command=npx');
  Result.Add('Arguments=@');
  Result.Add('RootDir=' + HomeDir);
  Result.Add('ApiHeaderName=Authorization');
  Result.Add('ApiBearerToken=@MCPBearerToken');
  Result.Add('URL=http://localhost:3001/sse');
  Result.Add('Login=login');
  Result.Add('Password=password');
  Result.Add('Timeout=15000');
  Result.Add('InitializeEndpointSuffix=');
  Result.Add('NotificationEndpointSuffix=');
  Result.Add('RpcEndpointSuffix=');
end;

function TMCPClientCustom.IsBinaryContentType(const AContentType: string): Boolean;
begin
  Result :=
    SameText(AContentType, 'image')    or
    SameText(AContentType, 'audio')    or
    SameText(AContentType, 'video')    or
    SameText(AContentType, 'document') or
    SameText(AContentType, 'archive');
end;

function TMCPClientCustom.ProcessAndExtractMedia(
    const AJsonResult: TJSONObject;
    AExtractedMedia: TObjectList): TJSONObject;
var
  ContentArr : TJSONArray;
  ContentNode: TJSONData;
  Item       : TJSONObject;
  ItemType   : string;
  DataStr    : string;
  MimeStr    : string;
  Ext        : string;
  MediaFile  : TAiMediaFile;
  I          : Integer;
begin
  // Clonar para no modificar el original
  Result := TJSONObject(AJsonResult.Clone);

  if not Assigned(AExtractedMedia) then
    Exit;

  ContentNode := Result.Find('content');
  if not (Assigned(ContentNode) and (ContentNode is TJSONArray)) then
    Exit;

  ContentArr := TJSONArray(ContentNode);

  for I := 0 to ContentArr.Count - 1 do
  begin
    if not (ContentArr[I] is TJSONObject) then Continue;
    Item     := TJSONObject(ContentArr[I]);
    ItemType := Item.Get('type', '');
    DataStr  := Item.Get('data', '');

    if (DataStr <> '') and IsBinaryContentType(ItemType) then
    begin
      MimeStr   := Item.Get('mimeType', 'application/octet-stream');
      MediaFile := TAiMediaFile.Create;
      try
        Ext := GetFileExtensionFromMimeType(MimeStr);
        if Ext = '' then Ext := 'bin';
        MediaFile.LoadFromBase64('media.' + Ext, DataStr);
        AExtractedMedia.Add(MediaFile);
      except
        MediaFile.Free;
      end;
    end;
  end;
end;

function TMCPClientCustom.Initialize: Boolean;
var
  JTools : TJSONObject;
  JNode  : TJSONData;
begin
  Result := False;
  try
    JTools := ListTools;
    if Assigned(JTools) then
    try
      JNode := JTools.Find('tools');
      if Assigned(JNode) and (JNode is TJSONArray) then
      begin
        FTools.Text  := JTools.FormatJSON;
        Initialized  := True;
        Enabled      := True;
        Available    := True;
        Result       := True;
      end;
    finally
      JTools.Free;
    end;
  except
    on E: Exception do
    begin
      FLastError  := E.Message;
      FTools.Clear;
      Initialized := True;
      Enabled     := False;
      Available   := False;
      DoLog('Initialize failed: ' + E.Message);
    end;
  end;
end;

function TMCPClientCustom.ListTools: TJSONObject;
begin
  Result := nil;
end;

function TMCPClientCustom.CallTool(const AToolName: string;
    AArguments: TJSONObject; AExtractedMedia: TObjectList): TJSONObject;
begin
  Result := nil;
end;

function TMCPClientCustom.CallTool(const AToolName: string;
    AArguments: TStrings; AExtractedMedia: TObjectList): TJSONObject;
var
  ArgsObj: TJSONObject;
  I: Integer;
begin
  ArgsObj := TJSONObject.Create;
  if Assigned(AArguments) then
  begin
    for I := 0 to AArguments.Count - 1 do
      ArgsObj.Add(AArguments.Names[I],
          TJSONString.Create(AArguments.ValueFromIndex[I]));
  end;
  Result := CallTool(AToolName, ArgsObj, AExtractedMedia);
end;

// ===========================================================================
// TMCPClientHttp
// ===========================================================================

constructor TMCPClientHttp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransportType      := tpHttp;
  FRequestIDCounter   := 0;
  FServerCapabilities := nil;
  FHttpClient         := TFPHTTPClient.Create(nil);
  FHttpClient.AddHeader('Content-Type', 'application/json');
  FHttpClient.AddHeader('Accept', 'application/json');
  FHttpClient.ConnectTimeout := 10000;
  FHttpClient.IOTimeout      := 30000;

  // Actualizar URL desde FParams si esta configurada
  if FParams.Values['URL'] <> '' then
    FURL := FParams.Values['URL'];
end;

destructor TMCPClientHttp.Destroy;
begin
  FHttpClient.Free;
  if Assigned(FServerCapabilities) then
    FServerCapabilities.Free;
  inherited;
end;

procedure TMCPClientHttp.ConfigureHttpAuth;
var
  HeaderName: string;
  BearerToken: string;
  ResolvedToken: string;
begin
  HeaderName := FParams.Values['ApiHeaderName'];
  if HeaderName = '' then HeaderName := 'Authorization';

  BearerToken := FParams.Values['ApiBearerToken'];
  if BearerToken = '' then Exit;

  // Resolver @ENV_VAR
  ResolvedToken := BearerToken;
  if (Length(ResolvedToken) > 1) and (ResolvedToken[1] = '@') then
    ResolvedToken := GetEnvironmentVariable(Copy(ResolvedToken, 2, MaxInt));

  if ResolvedToken <> '' then
  begin
    if SameText(HeaderName, 'Authorization') then
      FHttpClient.AddHeader('Authorization', 'Bearer ' + ResolvedToken)
    else
      FHttpClient.AddHeader(HeaderName, ResolvedToken);
  end;
end;

function TMCPClientHttp.InternalSendRequest(const AMethod: string;
    AParams: TJSONObject): TJSONObject;
var
  ReqObj      : TJSONObject;
  RequestJSON : string;
  ReqStream   : TStringStream;
  RespBody    : RawByteString;
  ResponseJSON: TJSONData;
  RespObj     : TJSONObject;
  ErrorNode   : TJSONData;
  ResultNode  : TJSONData;
  ErrMsg      : string;
  ErrCode     : Integer;
  EffURL      : string;
begin
  Result := nil;

  Inc(FRequestIDCounter);

  ReqObj := TJSONObject.Create;
  try
    ReqObj.Add('jsonrpc', TJSONString.Create('2.0'));
    ReqObj.Add('id', TJSONIntegerNumber.Create(FRequestIDCounter));
    ReqObj.Add('method', TJSONString.Create(AMethod));
    if Assigned(AParams) then
      ReqObj.Add('params', AParams)
    else
      ReqObj.Add('params', TJSONObject.Create);
    RequestJSON := ReqObj.AsJSON;
  finally
    ReqObj.Free;
    // AParams fue transferido a ReqObj y liberado.
  end;

  // Determinar URL efectiva
  EffURL := FURL;
  if EffURL = '' then
    EffURL := FParams.Values['URL'];
  if EffURL = '' then
    raise EMCPClientException.Create('URL no configurada para TMCPClientHttp');

  ReqStream := TStringStream.Create(RequestJSON);
  try
    FHttpClient.RequestBody := ReqStream;
    try
      RespBody := FHttpClient.Post(EffURL);
    except
      on E: Exception do
        raise EMCPClientException.Create('HTTP POST failed: ' + E.Message);
    end;
    FHttpClient.RequestBody := nil;

    ResponseJSON := GetJSON(string(RespBody));
    if not (ResponseJSON is TJSONObject) then
    begin
      ResponseJSON.Free;
      raise EMCPClientException.Create('Respuesta HTTP no es JSON valido.');
    end;

    RespObj := TJSONObject(ResponseJSON);
    try
      // Verificar error JSON-RPC
      ErrorNode := RespObj.Find('error');
      if Assigned(ErrorNode) and (ErrorNode is TJSONObject) then
      begin
        ErrMsg  := TJSONObject(ErrorNode).Get('message', 'Error desconocido');
        ErrCode := TJSONObject(ErrorNode).Get('code', 0);
        raise EMCPClientException.CreateFmt(
            'MCP Error %d: %s', [ErrCode, ErrMsg]);
      end;

      // Extraer result
      ResultNode := RespObj.Find('result');
      if Assigned(ResultNode) then
      begin
        if ResultNode is TJSONObject then
          Result := TJSONObject(ResultNode.Clone)
        else
          raise EMCPClientException.Create(
              'Tipo de resultado MCP inesperado.');
      end;

    finally
      RespObj.Free;
    end;

  finally
    ReqStream.Free;
  end;
end;

procedure TMCPClientHttp.InternalPerformMCPInitialize;
var
  InitParams  : TJSONObject;
  ClientInfo  : TJSONObject;
  Caps        : TJSONObject;
  InitResult  : TJSONObject;
begin
  InitParams := TJSONObject.Create;
  InitParams.Add('protocolVersion', TJSONString.Create('2025-06-18'));

  Caps := TJSONObject.Create;
  InitParams.Add('capabilities', Caps);

  ClientInfo := TJSONObject.Create;
  ClientInfo.Add('name', TJSONString.Create('MakerAI FPC Client'));
  ClientInfo.Add('version', TJSONString.Create('1.0'));
  InitParams.Add('clientInfo', ClientInfo);

  InitResult := InternalSendRequest('initialize', InitParams);
  if Assigned(InitResult) then
  begin
    if Assigned(FServerCapabilities) then
      FServerCapabilities.Free;
    FServerCapabilities := InitResult;
  end;
end;

procedure TMCPClientHttp.InternalSendInitializedNotification;
var
  ReqObj   : TJSONObject;
  ReqJSON  : string;
  ReqStream: TStringStream;
  EffURL   : string;
begin
  // Notificacion (sin id — no espera respuesta)
  ReqObj := TJSONObject.Create;
  try
    ReqObj.Add('jsonrpc', TJSONString.Create('2.0'));
    ReqObj.Add('method', TJSONString.Create('notifications/initialized'));
    ReqObj.Add('params', TJSONObject.Create);
    ReqJSON := ReqObj.AsJSON;
  finally
    ReqObj.Free;
  end;

  EffURL := FURL;
  if EffURL = '' then EffURL := FParams.Values['URL'];

  ReqStream := TStringStream.Create(ReqJSON);
  try
    FHttpClient.RequestBody := ReqStream;
    try
      FHttpClient.Post(EffURL); // respuesta ignorada (notificacion)
    except
      // Ignorar error en notificacion
    end;
    FHttpClient.RequestBody := nil;
  finally
    ReqStream.Free;
  end;
end;

function TMCPClientHttp.Initialize: Boolean;
begin
  Result := False;
  try
    ConfigureHttpAuth;

    // 1. Handshake MCP initialize
    InternalPerformMCPInitialize;

    // 2. Enviar notifications/initialized
    InternalSendInitializedNotification;

    // 3. Cargar lista de herramientas
    FInitialized := True;
    Result := inherited Initialize;

  except
    on E: Exception do
    begin
      FLastError   := E.Message;
      FInitialized := True;
      FEnabled     := False;
      FAvailable   := False;
      DoLog('TMCPClientHttp.Initialize failed: ' + E.Message);
    end;
  end;
end;

function TMCPClientHttp.ListTools: TJSONObject;
begin
  Result := InternalSendRequest('tools/list', nil);
end;

function TMCPClientHttp.CallTool(const AToolName: string;
    AArguments: TJSONObject; AExtractedMedia: TObjectList): TJSONObject;
var
  LParams  : TJSONObject;
  RawResult: TJSONObject;
begin
  LParams := TJSONObject.Create;
  LParams.Add('name', TJSONString.Create(AToolName));
  if Assigned(AArguments) then
    LParams.Add('arguments', AArguments)
  else
    LParams.Add('arguments', TJSONObject.Create);

  RawResult := InternalSendRequest('tools/call', LParams);

  if Assigned(RawResult) and Assigned(AExtractedMedia) then
    Result := ProcessAndExtractMedia(RawResult, AExtractedMedia)
  else
    Result := RawResult;

  if (Result <> RawResult) and Assigned(RawResult) then
    RawResult.Free;
end;

function TMCPClientHttp.CallTool(const AToolName: string;
    AArguments: TStrings; AExtractedMedia: TObjectList): TJSONObject;
var
  ArgsObj: TJSONObject;
  I: Integer;
begin
  ArgsObj := TJSONObject.Create;
  if Assigned(AArguments) then
  begin
    for I := 0 to AArguments.Count - 1 do
      ArgsObj.Add(AArguments.Names[I],
          TJSONString.Create(AArguments.ValueFromIndex[I]));
  end;
  Result := CallTool(AToolName, ArgsObj, AExtractedMedia);
end;

// ===========================================================================
// TStdioReadThread
// ===========================================================================

constructor TStdioReadThread.Create(AClient: TMCPClientStdIo);
begin
  inherited Create(True);
  FClient := AClient;
  FreeOnTerminate := False;
end;

procedure TStdioReadThread.Execute;
var
  Line  : string;
  JData : TJSONData;
  JObj  : TJSONObject;
  C     : Byte;
  Res   : Integer;
begin
  Line := '';
  while not Terminated do
  begin
    if not Assigned(FClient.FProcess) or
       not FClient.FProcess.IsRunning then
      Break;

    // Leer byte a byte del pipe de stdout del proceso
    if not Assigned(FClient.FProcess.OutputPipe) then
    begin
      Sleep(100);
      Continue;
    end;

    try
      Res := FClient.FProcess.OutputPipe.Read(C, 1);
      if Res <= 0 then
      begin
        Sleep(10);
        Continue;
      end;

      if Chr(C) = #10 then
      begin
        // Linea completa
        Line := Trim(Line);
        if Line <> '' then
        begin
          try
            JData := GetJSON(Line);
            if JData is TJSONObject then
            begin
              JObj := TJSONObject(JData);
              FClient.FIncomingMessages.Enqueue(JObj);
            end
            else
              JData.Free;
          except
            // JSON invalido — ignorar
          end;
        end;
        Line := '';
      end
      else if Chr(C) <> #13 then
        Line := Line + Chr(C);

    except
      Break;
    end;
  end;
end;

// ===========================================================================
// TMCPClientStdIo
// ===========================================================================

constructor TMCPClientStdIo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransportType      := tpStdIo;
  FProcess            := nil;
  FReadThread         := nil;
  FIncomingMessages   := TJsonMessageQueue.Create;
  FRequestIDCounter   := 0;
  FIsRunning          := False;
end;

destructor TMCPClientStdIo.Destroy;
begin
  InternalStopServerProcess;
  FIncomingMessages.Free;
  inherited;
end;

procedure TMCPClientStdIo.InternalStartServerProcess;
var
  Proc     : TProcess;
  Command  : string;
  Arguments: string;
  RootDir  : string;
  I        : Integer;
  ArgList  : TStringList;
begin
  Command   := FParams.Values['Command'];
  Arguments := FParams.Values['Arguments'];
  RootDir   := FParams.Values['RootDir'];

  if Command = '' then
    raise EMCPClientException.Create(
        'Parametro "Command" no configurado.');

  Proc := TProcess.Create(nil);
  try
    Proc.Executable := Command;

    if Arguments <> '' then
    begin
      // Separar argumentos por espacios usando TStringList
      ArgList := TStringList.Create;
      try
        ArgList.Delimiter := ' ';
        ArgList.DelimitedText := Arguments;
        for I := 0 to ArgList.Count - 1 do
          if Trim(ArgList[I]) <> '' then
            Proc.Parameters.Add(Trim(ArgList[I]));
      finally
        ArgList.Free;
      end;
    end;

    if RootDir <> '' then
      Proc.CurrentDirectory := RootDir;

    // Configurar variables de entorno adicionales
    if FEnvVars.Count > 0 then
    begin
      Proc.Environment.Assign(FEnvVars);
    end;

    Proc.Options := [poUsePipes, poStdErrToOutput];
    Proc.Execute;

    FProcess := TInteractiveProcessInfo.Create;
{$IFNDEF MSWINDOWS}
    FProcess.FProcess   := Proc;
{$ENDIF}
    FProcess.InputPipe  := Proc.Input;
    FProcess.OutputPipe := Proc.Output;
    FProcess.IsRunning  := True;

    // Pausa breve para que el proceso arranque
    Sleep(500);

    // Iniciar hilo de lectura
    FReadThread := TStdioReadThread.Create(Self);
    FReadThread.Start;

    FIsRunning := True;
    DoLog('Proceso MCP iniciado: ' + Command);

  except
    on E: Exception do
    begin
      Proc.Free;
      FreeAndNil(FProcess);
      raise EMCPClientException.Create(
          'Error iniciando proceso MCP: ' + E.Message);
    end;
  end;
end;

procedure TMCPClientStdIo.InternalStopServerProcess;
begin
  FIsRunning := False;

  if Assigned(FReadThread) then
  begin
    FReadThread.Terminate;
    FReadThread.WaitFor;
    FreeAndNil(FReadThread);
  end;

  if Assigned(FProcess) then
  begin
{$IFNDEF MSWINDOWS}
    if Assigned(FProcess.FProcess) then
    begin
      TProcess(FProcess.FProcess).Terminate(0);
    end;
{$ENDIF}
    FreeAndNil(FProcess);
  end;
end;

procedure TMCPClientStdIo.InternalSendRawMessage(const AJsonString: string);
var
  Line: string;
begin
  if not Assigned(FProcess) or not FProcess.IsRunning then
    raise EMCPClientException.Create('Proceso MCP no esta corriendo.');

  Line := AJsonString + #10;
  FProcess.InputPipe.WriteBuffer(Line[1], Length(Line));
end;

function TMCPClientStdIo.InternalReceiveResponse(AExpectedID: Integer;
    ATimeoutMs: Cardinal): TJSONObject;
var
  Elapsed: Cardinal;
  Msg    : TJSONObject;
  IDNode : TJSONData;
  IDVal  : Integer;
begin
  Result  := nil;
  Elapsed := 0;

  while Elapsed < ATimeoutMs do
  begin
    // Revisar mensajes en la cola
    Msg := FIncomingMessages.Dequeue(100);
    Inc(Elapsed, 100);

    if Assigned(Msg) then
    begin
      IDNode := Msg.Find('id');
      if Assigned(IDNode) then
        IDVal := IDNode.AsInteger
      else
        IDVal := -1;

      if IDVal = AExpectedID then
      begin
        Result := Msg;
        Exit;
      end
      else
      begin
        // Mensaje para otro ID — devolver a la cola (o descartar si es notificacion)
        if IDVal >= 0 then
          FIncomingMessages.Enqueue(Msg)
        else
          Msg.Free;
      end;
    end;
  end;
end;

function TMCPClientStdIo.InternalInitialize: TJSONObject;
var
  ReqObj    : TJSONObject;
  LParams   : TJSONObject;
  ClientInfo: TJSONObject;
  Caps      : TJSONObject;
begin
  Inc(FRequestIDCounter);

  LParams := TJSONObject.Create;
  LParams.Add('protocolVersion', TJSONString.Create('2025-06-18'));

  Caps := TJSONObject.Create;
  LParams.Add('capabilities', Caps);

  ClientInfo := TJSONObject.Create;
  ClientInfo.Add('name', TJSONString.Create('MakerAI FPC Client'));
  ClientInfo.Add('version', TJSONString.Create('1.0'));
  LParams.Add('clientInfo', ClientInfo);

  ReqObj := TJSONObject.Create;
  try
    ReqObj.Add('jsonrpc', TJSONString.Create('2.0'));
    ReqObj.Add('id', TJSONIntegerNumber.Create(FRequestIDCounter));
    ReqObj.Add('method', TJSONString.Create('initialize'));
    ReqObj.Add('params', LParams);
    InternalSendRawMessage(ReqObj.AsJSON);
  finally
    ReqObj.Free;
  end;

  // Esperar respuesta
  Result := InternalReceiveResponse(FRequestIDCounter, 15000);
end;

procedure TMCPClientStdIo.InternalSendInitializedNotification;
var
  NotifObj: TJSONObject;
begin
  NotifObj := TJSONObject.Create;
  try
    NotifObj.Add('jsonrpc', TJSONString.Create('2.0'));
    NotifObj.Add('method', TJSONString.Create('notifications/initialized'));
    NotifObj.Add('params', TJSONObject.Create);
    InternalSendRawMessage(NotifObj.AsJSON);
  finally
    NotifObj.Free;
  end;
end;

function TMCPClientStdIo.InternalListTools: TJSONObject;
var
  ReqObj: TJSONObject;
begin
  Inc(FRequestIDCounter);

  ReqObj := TJSONObject.Create;
  try
    ReqObj.Add('jsonrpc', TJSONString.Create('2.0'));
    ReqObj.Add('id', TJSONIntegerNumber.Create(FRequestIDCounter));
    ReqObj.Add('method', TJSONString.Create('tools/list'));
    ReqObj.Add('params', TJSONObject.Create);
    InternalSendRawMessage(ReqObj.AsJSON);
  finally
    ReqObj.Free;
  end;

  Result := InternalReceiveResponse(FRequestIDCounter, 15000);
end;

function TMCPClientStdIo.Initialize: Boolean;
var
  InitResult : TJSONObject;
begin
  Result := False;
  try
    InternalStartServerProcess;

    // Protocolo MCP initialize
    InitResult := InternalInitialize;
    if not Assigned(InitResult) then
    begin
      FLastError := 'Timeout esperando respuesta initialize del servidor MCP';
      DoLog(FLastError);
      InternalStopServerProcess;
      Exit;
    end;
    InitResult.Free;

    // Notificacion initialized (sin respuesta)
    InternalSendInitializedNotification;

    // Cargar tools
    FInitialized := True;
    Result := inherited Initialize;

  except
    on E: Exception do
    begin
      FLastError   := E.Message;
      FInitialized := True;
      FEnabled     := False;
      FAvailable   := False;
      DoLog('TMCPClientStdIo.Initialize failed: ' + E.Message);
    end;
  end;
end;

function TMCPClientStdIo.ListTools: TJSONObject;
var
  RespResult: TJSONObject;
  ResultNode: TJSONData;
begin
  Result := nil;

  RespResult := InternalListTools;
  if not Assigned(RespResult) then
    Exit;

  try
    // La respuesta es la respuesta JSON-RPC completa: {"jsonrpc":"2.0","id":X,"result":{...}}
    ResultNode := RespResult.Find('result');
    if Assigned(ResultNode) and (ResultNode is TJSONObject) then
      Result := TJSONObject(ResultNode.Clone)
    else
      Result := TJSONObject(RespResult.Clone);
  finally
    RespResult.Free;
  end;
end;

function TMCPClientStdIo.CallTool(const AToolName: string;
    AArguments: TJSONObject; AExtractedMedia: TObjectList): TJSONObject;
var
  ReqObj    : TJSONObject;
  LParams   : TJSONObject;
  RespResult: TJSONObject;
  ResultNode: TJSONData;
begin
  Result := nil;
  Inc(FRequestIDCounter);

  ReqObj := TJSONObject.Create;
  try
    ReqObj.Add('jsonrpc', TJSONString.Create('2.0'));
    ReqObj.Add('id', TJSONIntegerNumber.Create(FRequestIDCounter));
    ReqObj.Add('method', TJSONString.Create('tools/call'));

    LParams := TJSONObject.Create;
    LParams.Add('name', TJSONString.Create(AToolName));
    if Assigned(AArguments) then
      LParams.Add('arguments', AArguments)
    else
      LParams.Add('arguments', TJSONObject.Create);
    ReqObj.Add('params', LParams);

    InternalSendRawMessage(ReqObj.AsJSON);
  finally
    ReqObj.Free;
  end;

  RespResult := InternalReceiveResponse(FRequestIDCounter, 30000);
  if not Assigned(RespResult) then
    Exit;

  try
    ResultNode := RespResult.Find('result');
    if Assigned(ResultNode) and (ResultNode is TJSONObject) then
    begin
      if Assigned(AExtractedMedia) then
        Result := ProcessAndExtractMedia(TJSONObject(ResultNode), AExtractedMedia)
      else
        Result := TJSONObject(ResultNode.Clone);
    end;
  finally
    RespResult.Free;
  end;
end;

function TMCPClientStdIo.CallTool(const AToolName: string;
    AArguments: TStrings; AExtractedMedia: TObjectList): TJSONObject;
var
  ArgsObj: TJSONObject;
  I: Integer;
begin
  ArgsObj := TJSONObject.Create;
  if Assigned(AArguments) then
  begin
    for I := 0 to AArguments.Count - 1 do
      ArgsObj.Add(AArguments.Names[I],
          TJSONString.Create(AArguments.ValueFromIndex[I]));
  end;
  Result := CallTool(AToolName, ArgsObj, AExtractedMedia);
end;

// ===========================================================================
// TMCPClientSSE (EXPERIMENTAL)
// ===========================================================================
// Hilo interno que conecta al SSE stream y llena la cola de mensajes
type
  TSSEReadThread = class(TThread)
  private
    FClient : TMCPClientSSE;
    FBuffer : string;
    FEventType: string;
    FEventData: string;
    procedure ProcessLine(const ALine: string);
  public
    constructor Create(AClient: TMCPClientSSE);
    procedure Execute; override;
  end;

constructor TSSEReadThread.Create(AClient: TMCPClientSSE);
begin
  inherited Create(True);
  FClient    := AClient;
  FBuffer    := '';
  FEventType := '';
  FEventData := '';
  FreeOnTerminate := True;
end;

procedure TSSEReadThread.ProcessLine(const ALine: string);
var
  JData: TJSONData;
begin
  if AnsiStartsStr('event:', ALine) then
    FEventType := Trim(Copy(ALine, 7, MaxInt))
  else if AnsiStartsStr('data:', ALine) then
    FEventData := Trim(Copy(ALine, 6, MaxInt))
  else if ALine = '' then
  begin
    // Fin de evento SSE
    if (FEventType = 'endpoint') and (FEventData <> '') then
    begin
      // Guardar endpoint para POST
      FClient.FPostEndpoint := FEventData;
      FClient.FIsConnected  := True;
    end
    else if FEventData <> '' then
    begin
      try
        JData := GetJSON(FEventData);
        if JData is TJSONObject then
          FClient.FIncomingMessages.Enqueue(TJSONObject(JData))
        else
          JData.Free;
      except
        // Ignorar JSON invalido
      end;
    end;
    FEventType := '';
    FEventData := '';
  end;
end;

procedure TSSEReadThread.Execute;
var
  Http     : TFPHTTPClient;
  SS       : TStringStream;
  EffURL   : string;
  LineList : TStringList;
  Li       : Integer;
begin
  Http     := TFPHTTPClient.Create(nil);
  SS       := TStringStream.Create('');
  LineList := nil;
  try
    Http.AddHeader('Accept', 'text/event-stream');
    Http.AddHeader('Cache-Control', 'no-cache');

    EffURL := FClient.FURL;
    if not AnsiEndsStr('/sse', EffURL) then
      EffURL := EffURL + '/sse';

    try
      Http.Get(EffURL, SS);
    except
      // Conexion cerrada o error
    end;

    // Procesar el buffer recibido linea a linea
    LineList := TStringList.Create;
    try
      LineList.Text := SS.DataString;
      for Li := 0 to LineList.Count - 1 do
      begin
        if Terminated or FClient.FStopRequested then Break;
        ProcessLine(TrimRight(LineList[Li]));
      end;
    finally
      LineList.Free;
    end;

  finally
    Http.Free;
    SS.Free;
  end;
end;

// TMCPClientSSE

constructor TMCPClientSSE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransportType    := tpSSE;
  FHttpClient       := TFPHTTPClient.Create(nil);
  FHttpClient.AddHeader('Content-Type', 'application/json');
  FIncomingMessages := TJsonMessageQueue.Create;
  FRequestIDCounter := 0;
  FPostEndpoint     := '';
  FIsConnected      := False;
  FSSEThread        := nil;
  FStopRequested    := False;
end;

destructor TMCPClientSSE.Destroy;
begin
  StopEventStream;
  FHttpClient.Free;
  FIncomingMessages.Free;
  inherited;
end;

procedure TMCPClientSSE.StartEventStream;
var
  T      : TSSEReadThread;
  Elapsed: Cardinal;
begin
  FStopRequested := False;
  Elapsed := 0;
  T := TSSEReadThread.Create(Self);
  FSSEThread := T;
  T.Start;

  // Esperar hasta que obtengamos el endpoint (timeout 10s)
  while not FIsConnected and (Elapsed < 10000) do
  begin
    Sleep(100);
    Inc(Elapsed, 100);
  end;
end;

procedure TMCPClientSSE.StopEventStream;
begin
  FStopRequested := True;
  FIsConnected   := False;
  if Assigned(FSSEThread) then
  begin
    FSSEThread.Terminate;
    FSSEThread := nil; // FreeOnTerminate = True
  end;
end;

function TMCPClientSSE.InternalSendRequest(const AMethod: string;
    AParams: TJSONObject): TJSONObject;
var
  ReqObj    : TJSONObject;
  RequestJSON: string;
  PostURL   : string;
  ReqStream : TStringStream;
  P, P2     : Integer;
begin
  Result := nil;
  Inc(FRequestIDCounter);

  ReqObj := TJSONObject.Create;
  try
    ReqObj.Add('jsonrpc', TJSONString.Create('2.0'));
    ReqObj.Add('id', TJSONIntegerNumber.Create(FRequestIDCounter));
    ReqObj.Add('method', TJSONString.Create(AMethod));
    if Assigned(AParams) then
      ReqObj.Add('params', AParams)
    else
      ReqObj.Add('params', TJSONObject.Create);
    RequestJSON := ReqObj.AsJSON;
  finally
    ReqObj.Free;
  end;

  // Construir URL de POST
  PostURL := FURL;
  if FPostEndpoint <> '' then
  begin
    // FPostEndpoint puede ser relativo ("/messages?session_id=xxx")
    if Copy(FPostEndpoint, 1, 1) = '/' then
    begin
      // Extraer base URL (protocol + host)
      P := Pos('://', PostURL);
      if P > 0 then
      begin
        Inc(P, 3);
        // Buscar primer '/' despues del host
        P2 := P;
        while (P2 <= Length(PostURL)) and (PostURL[P2] <> '/') do
          Inc(P2);
        if P2 <= Length(PostURL) then
          PostURL := Copy(PostURL, 1, P2 - 1) + FPostEndpoint
        else
          PostURL := PostURL + FPostEndpoint;
      end;
    end
    else
      PostURL := FPostEndpoint;
  end;

  ReqStream := TStringStream.Create(RequestJSON);
  try
    FHttpClient.RequestBody := ReqStream;
    try
      FHttpClient.Post(PostURL); // respuesta 202 ignorada; respuesta real llega por SSE
    except
      on E: Exception do
        raise EMCPClientException.Create('SSE POST failed: ' + E.Message);
    end;
    FHttpClient.RequestBody := nil;

    // La respuesta real llega por SSE, no por este POST (que retorna 202)
    // Esperar mensaje en la cola con el ID correcto
    Result := InternalReceiveResponse(FRequestIDCounter, 15000);

  finally
    ReqStream.Free;
  end;
end;

function TMCPClientSSE.InternalReceiveResponse(AExpectedID: Integer;
    ATimeoutMs: Cardinal): TJSONObject;
var
  Elapsed: Cardinal;
  Msg    : TJSONObject;
  IDNode : TJSONData;
  IDVal  : Integer;
  ResultNode: TJSONData;
begin
  Result  := nil;
  Elapsed := 0;

  while Elapsed < ATimeoutMs do
  begin
    Msg := FIncomingMessages.Dequeue(100);
    Inc(Elapsed, 100);

    if Assigned(Msg) then
    begin
      IDNode := Msg.Find('id');
      if Assigned(IDNode) then
        IDVal := IDNode.AsInteger
      else
        IDVal := -1;

      if IDVal = AExpectedID then
      begin
        // Extraer result
        ResultNode := Msg.Find('result');
        if Assigned(ResultNode) and (ResultNode is TJSONObject) then
          Result := TJSONObject(ResultNode.Clone);
        Msg.Free;
        Exit;
      end
      else
      begin
        if IDVal >= 0 then
          FIncomingMessages.Enqueue(Msg)
        else
          Msg.Free;
      end;
    end;
  end;
end;

function TMCPClientSSE.Initialize: Boolean;
begin
  Result := False;
  try
    StartEventStream;

    if not FIsConnected then
    begin
      FLastError := 'No se pudo establecer conexion SSE con ' + FURL;
      DoLog(FLastError);
      Exit;
    end;

    FInitialized := True;
    Result := inherited Initialize;

  except
    on E: Exception do
    begin
      FLastError   := E.Message;
      FInitialized := True;
      FEnabled     := False;
      FAvailable   := False;
      DoLog('TMCPClientSSE.Initialize failed: ' + E.Message);
    end;
  end;
end;

function TMCPClientSSE.ListTools: TJSONObject;
begin
  Result := InternalSendRequest('tools/list', nil);
end;

function TMCPClientSSE.CallTool(const AToolName: string;
    AArguments: TJSONObject; AExtractedMedia: TObjectList): TJSONObject;
var
  LParams  : TJSONObject;
  RawResult: TJSONObject;
begin
  LParams := TJSONObject.Create;
  LParams.Add('name', TJSONString.Create(AToolName));
  if Assigned(AArguments) then
    LParams.Add('arguments', AArguments)
  else
    LParams.Add('arguments', TJSONObject.Create);

  RawResult := InternalSendRequest('tools/call', LParams);

  if Assigned(RawResult) and Assigned(AExtractedMedia) then
    Result := ProcessAndExtractMedia(RawResult, AExtractedMedia)
  else
    Result := RawResult;

  if (Result <> RawResult) and Assigned(RawResult) then
    RawResult.Free;
end;

function TMCPClientSSE.CallTool(const AToolName: string;
    AArguments: TStrings; AExtractedMedia: TObjectList): TJSONObject;
var
  ArgsObj: TJSONObject;
  I: Integer;
begin
  ArgsObj := TJSONObject.Create;
  if Assigned(AArguments) then
  begin
    for I := 0 to AArguments.Count - 1 do
      ArgsObj.Add(AArguments.Names[I],
          TJSONString.Create(AArguments.ValueFromIndex[I]));
  end;
  Result := CallTool(AToolName, ArgsObj, AExtractedMedia);
end;

end.
