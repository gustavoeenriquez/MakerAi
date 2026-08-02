// MIT License
//
// MakerAI - Telemetria OpenTelemetry (fase 1 de observabilidad)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Telemetry;

// -----------------------------------------------------------------------------
// TAiTelemetry: exportador de trazas OpenTelemetry (OTLP/HTTP con JSON).
//
// Diseno:
// - Componente opt-in: si no hay una instancia habilitada, TODA la
//   instrumentacion del framework es no-op (AiSpanStart devuelve nil y los
//   helpers toleran nil). Costo cero para quien no lo usa.
// - Exporta a cualquier collector OTLP/HTTP (Jaeger, Grafana Tempo, Langfuse,
//   Arize Phoenix, etc.) en el endpoint estandar http://localhost:4318/v1/traces.
// - Codificacion JSON del protobuf OTLP (no requiere dependencia protobuf).
// - Atributos segun las convenciones semanticas GenAI de OpenTelemetry
//   (gen_ai.system, gen_ai.request.model, gen_ai.usage.input_tokens, ...).
// - Los spans terminados se encolan y un hilo exportador los envia por lotes
//   (FlushIntervalMs); Flush fuerza el envio sincrono (tests/shutdown).
// - Anidamiento automatico por hilo: StartSpan toma como padre el span activo
//   del hilo actual salvo que se pase un traceparent W3C explicito.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.SyncObjs, System.DateUtils, System.Net.HttpClient, System.Net.URLClient;

type
  // Mapeo directo al enum SpanKind de OTLP (ver BuildOtlpKind)
  TAiSpanKind = (skInternal, skServer, skClient, skProducer, skConsumer);

  TAiSpan = class
  private
    FTraceId: string; // 32 hex
    FSpanId: string; // 16 hex
    FParentSpanId: string; // 16 hex o ''
    FName: string;
    FKind: TAiSpanKind;
    FStartUnixNano: Int64;
    FEndUnixNano: Int64;
    FAttributes: TJSONArray; // formato OTLP: [{key, value:{stringValue|...}}]
    FErrorMessage: string;
    FHasError: Boolean;
    FOwnerThreadId: TThreadID;
    procedure AddAttr(const AKey: string; AValue: TJSONObject);
  public
    constructor Create(const AName: string; AKind: TAiSpanKind);
    destructor Destroy; override;
    procedure SetAttribute(const AKey, AValue: string); overload;
    procedure SetAttribute(const AKey: string; AValue: Int64); overload;
    procedure SetAttribute(const AKey: string; AValue: Double); overload;
    procedure SetAttribute(const AKey: string; AValue: Boolean); overload;
    procedure SetError(const AMessage: string);
    // Header W3C trace-context para propagar la traza (p.ej. en _meta MCP)
    function TraceParent: string;
    property TraceId: string read FTraceId;
    property SpanId: string read FSpanId;
    property Name: string read FName;
  end;

  TAiTelemetryLogEvent = procedure(Sender: TObject; const AMsg: string) of object;

  TAiTelemetry = class(TComponent)
  private
    class var FActive: TAiTelemetry; // registro global opt-in (una instancia activa)
  private
    FEnabled: Boolean;
    FEndpoint: string;
    FServiceName: string;
    FHeaders: TStrings; // headers extra Nombre=Valor (p.ej. Authorization=Bearer x)
    FFlushIntervalMs: Integer;
    FMaxBatchSize: Integer;
    FLock: TCriticalSection;
    FPending: TObjectList<TAiSpan>; // spans terminados pendientes de exportar
    FWorker: TThread;
    FStopEvent: TEvent;
    FThreadStacks: TObjectDictionary<TThreadID, TStack<TAiSpan>>; // span activo por hilo
    FOnLog: TAiTelemetryLogEvent;
    FDroppedSpans: Int64;
    procedure SetEnabled(const Value: Boolean);
    procedure SetHeaders(const Value: TStrings);
    procedure StartWorker;
    procedure StopWorker;
    procedure ExportPending;
    function BuildOtlpPayload(ABatch: TObjectList<TAiSpan>): TJSONObject;
    procedure DoLog(const AMsg: string);
    procedure PushThreadSpan(ASpan: TAiSpan);
    procedure PopThreadSpan(ASpan: TAiSpan);
    function PeekThreadSpan: TAiSpan;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Instancia activa (habilitada) o nil. Toda la instrumentacion del
    // framework pasa por aqui: sin instancia activa no se crea nada.
    class function Active: TAiTelemetry; static;

    // Crea un span. Si ATraceParent viene vacio, el padre es el span activo
    // del hilo actual (si existe). Con traceparent W3C ('00-<trace>-<span>-..')
    // continua esa traza remota. El span queda como activo del hilo hasta
    // que se cierre con EndSpan.
    function StartSpan(const AName: string; AKind: TAiSpanKind = skInternal;
      const ATraceParent: string = ''): TAiSpan;

    // Cierra el span y lo encola para exportar (la telemetria toma posesion).
    // AErrorMsg <> '' marca el span con status ERROR.
    procedure EndSpan(ASpan: TAiSpan; const AErrorMsg: string = '');

    // Exporta sincronicamente todo lo pendiente (tests / shutdown).
    procedure Flush;

    property DroppedSpans: Int64 read FDroppedSpans;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Endpoint: string read FEndpoint write FEndpoint;
    property ServiceName: string read FServiceName write FServiceName;
    property Headers: TStrings read FHeaders write SetHeaders;
    property FlushIntervalMs: Integer read FFlushIntervalMs write FFlushIntervalMs default 2000;
    property MaxBatchSize: Integer read FMaxBatchSize write FMaxBatchSize default 200;
    property OnLog: TAiTelemetryLogEvent read FOnLog write FOnLog;
  end;

// --- Helpers no-op-safe para el codigo instrumentado del framework ---
// Devuelve nil si no hay telemetria activa; todos toleran ASpan = nil.
function AiSpanStart(const AName: string; AKind: TAiSpanKind = skInternal;
  const ATraceParent: string = ''): TAiSpan;
procedure AiSpanEnd(ASpan: TAiSpan; const AErrorMsg: string = '');
procedure AiSpanAttr(ASpan: TAiSpan; const AKey, AValue: string); overload;
procedure AiSpanAttr(ASpan: TAiSpan; const AKey: string; AValue: Int64); overload;
procedure AiSpanAttr(ASpan: TAiSpan; const AKey: string; AValue: Boolean); overload;
// Traceparent del span (o '' si nil) para propagacion (p.ej. _meta MCP)
function AiSpanTraceParent(ASpan: TAiSpan): string;

procedure Register;

implementation

const
  DEF_ENDPOINT = 'http://localhost:4318/v1/traces';
  OTEL_SCOPE_NAME = 'makerai';
  OTEL_SCOPE_VERSION = '3.5';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiTelemetry]);
end;

// ----------------------------------------------------------------------------
// Utilidades
// ----------------------------------------------------------------------------

function NewHexId(ADigits: Integer): string;
var
  G: TGUID;
  S: string;
begin
  // Un GUID aporta 32 digitos hex de aleatoriedad suficiente para ids OTLP
  CreateGUID(G);
  S := GUIDToString(G).Replace('{', '').Replace('}', '').Replace('-', '').ToLower;
  Result := Copy(S, 1, ADigits);
end;

function NowUnixNano: Int64;
var
  UtcNow: TDateTime;
begin
  UtcNow := TTimeZone.Local.ToUniversalTime(Now);
  // Precision de milisegundos expresada en nanosegundos (suficiente para trazas)
  Result := DateTimeToUnix(UtcNow, True) * 1000000000 + MilliSecondOf(UtcNow) * 1000000;
end;

// Parsea un traceparent W3C: '00-<32 hex traceId>-<16 hex spanId>-<flags>'
function ParseTraceParent(const AValue: string; out ATraceId, ASpanId: string): Boolean;
var
  Parts: TArray<string>;
begin
  Result := False;
  ATraceId := '';
  ASpanId := '';
  Parts := AValue.Split(['-']);
  if (Length(Parts) >= 3) and (Length(Parts[1]) = 32) and (Length(Parts[2]) = 16) then
  begin
    ATraceId := Parts[1].ToLower;
    ASpanId := Parts[2].ToLower;
    Result := True;
  end;
end;

// ----------------------------------------------------------------------------
// TAiSpan
// ----------------------------------------------------------------------------

constructor TAiSpan.Create(const AName: string; AKind: TAiSpanKind);
begin
  inherited Create;
  FName := AName;
  FKind := AKind;
  FTraceId := NewHexId(32);
  FSpanId := NewHexId(16);
  FStartUnixNano := NowUnixNano;
  FAttributes := TJSONArray.Create;
  FOwnerThreadId := TThread.CurrentThread.ThreadID;
end;

destructor TAiSpan.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

procedure TAiSpan.AddAttr(const AKey: string; AValue: TJSONObject);
var
  Pair: TJSONObject;
begin
  Pair := TJSONObject.Create;
  Pair.AddPair('key', AKey);
  Pair.AddPair('value', AValue);
  FAttributes.AddElement(Pair);
end;

procedure TAiSpan.SetAttribute(const AKey, AValue: string);
var
  V: TJSONObject;
begin
  V := TJSONObject.Create;
  V.AddPair('stringValue', AValue);
  AddAttr(AKey, V);
end;

procedure TAiSpan.SetAttribute(const AKey: string; AValue: Int64);
var
  V: TJSONObject;
begin
  // El mapeo JSON de proto3 serializa int64 como string decimal
  V := TJSONObject.Create;
  V.AddPair('intValue', IntToStr(AValue));
  AddAttr(AKey, V);
end;

procedure TAiSpan.SetAttribute(const AKey: string; AValue: Double);
var
  V: TJSONObject;
begin
  V := TJSONObject.Create;
  V.AddPair('doubleValue', TJSONNumber.Create(AValue));
  AddAttr(AKey, V);
end;

procedure TAiSpan.SetAttribute(const AKey: string; AValue: Boolean);
var
  V: TJSONObject;
begin
  V := TJSONObject.Create;
  V.AddPair('boolValue', TJSONBool.Create(AValue));
  AddAttr(AKey, V);
end;

procedure TAiSpan.SetError(const AMessage: string);
begin
  FHasError := True;
  FErrorMessage := AMessage;
end;

function TAiSpan.TraceParent: string;
begin
  Result := '00-' + FTraceId + '-' + FSpanId + '-01';
end;

// ----------------------------------------------------------------------------
// TAiTelemetry
// ----------------------------------------------------------------------------

constructor TAiTelemetry.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := False;
  FEndpoint := DEF_ENDPOINT;
  FServiceName := 'MakerAI';
  FHeaders := TStringList.Create;
  FFlushIntervalMs := 2000;
  FMaxBatchSize := 200;
  FLock := TCriticalSection.Create;
  FPending := TObjectList<TAiSpan>.Create(True);
  FStopEvent := TEvent.Create(nil, True, False, '');
  FThreadStacks := TObjectDictionary<TThreadID, TStack<TAiSpan>>.Create([doOwnsValues]);
end;

destructor TAiTelemetry.Destroy;
begin
  if FActive = Self then
    FActive := nil;
  StopWorker;
  // Ultimo intento de no perder lo pendiente
  try
    if FEnabled then
      ExportPending;
  except
    // el shutdown nunca debe fallar por telemetria
  end;
  FPending.Free;
  FThreadStacks.Free;
  FStopEvent.Free;
  FLock.Free;
  FHeaders.Free;
  inherited;
end;

class function TAiTelemetry.Active: TAiTelemetry;
begin
  Result := FActive;
  if Assigned(Result) and not Result.FEnabled then
    Result := nil;
end;

procedure TAiTelemetry.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    Exit;
  FEnabled := Value;
  if csDesigning in ComponentState then
    Exit;
  if FEnabled then
  begin
    FActive := Self; // ultima instancia habilitada gana
    StartWorker;
  end
  else
  begin
    if FActive = Self then
      FActive := nil;
    StopWorker;
  end;
end;

procedure TAiTelemetry.SetHeaders(const Value: TStrings);
begin
  FHeaders.Assign(Value);
end;

procedure TAiTelemetry.StartWorker;
begin
  if Assigned(FWorker) then
    Exit;
  FStopEvent.ResetEvent;
  FWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      while FStopEvent.WaitFor(Cardinal(FFlushIntervalMs)) = wrTimeout do
      begin
        try
          ExportPending;
        except
          // nunca tumbar el hilo exportador
        end;
      end;
    end);
  FWorker.FreeOnTerminate := False;
  FWorker.Start;
end;

procedure TAiTelemetry.StopWorker;
begin
  if not Assigned(FWorker) then
    Exit;
  FStopEvent.SetEvent;
  FWorker.WaitFor;
  FreeAndNil(FWorker);
end;

procedure TAiTelemetry.DoLog(const AMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, AMsg);
end;

// --- pila de spans activos por hilo (anidamiento automatico) ---

procedure TAiTelemetry.PushThreadSpan(ASpan: TAiSpan);
var
  Stack: TStack<TAiSpan>;
begin
  FLock.Enter;
  try
    if not FThreadStacks.TryGetValue(ASpan.FOwnerThreadId, Stack) then
    begin
      Stack := TStack<TAiSpan>.Create;
      FThreadStacks.Add(ASpan.FOwnerThreadId, Stack);
    end;
    Stack.Push(ASpan);
  finally
    FLock.Leave;
  end;
end;

procedure TAiTelemetry.PopThreadSpan(ASpan: TAiSpan);
var
  Stack: TStack<TAiSpan>;
begin
  FLock.Enter;
  try
    if FThreadStacks.TryGetValue(ASpan.FOwnerThreadId, Stack) then
      if (Stack.Count > 0) and (Stack.Peek = ASpan) then
        Stack.Pop;
    // Si el span cerrado no es el tope (cierres fuera de orden entre hilos),
    // simplemente no tocamos la pila: el costo es un parentesco menos exacto.
  finally
    FLock.Leave;
  end;
end;

function TAiTelemetry.PeekThreadSpan: TAiSpan;
var
  Stack: TStack<TAiSpan>;
begin
  Result := nil;
  FLock.Enter;
  try
    if FThreadStacks.TryGetValue(TThread.CurrentThread.ThreadID, Stack) then
      if Stack.Count > 0 then
        Result := Stack.Peek;
  finally
    FLock.Leave;
  end;
end;

function TAiTelemetry.StartSpan(const AName: string; AKind: TAiSpanKind;
  const ATraceParent: string): TAiSpan;
var
  Parent: TAiSpan;
  RemoteTrace, RemoteSpan: string;
begin
  Result := TAiSpan.Create(AName, AKind);
  if (ATraceParent <> '') and ParseTraceParent(ATraceParent, RemoteTrace, RemoteSpan) then
  begin
    // Continuar una traza remota (p.ej. traceparent recibido via _meta MCP)
    Result.FTraceId := RemoteTrace;
    Result.FParentSpanId := RemoteSpan;
  end
  else
  begin
    Parent := PeekThreadSpan;
    if Assigned(Parent) then
    begin
      Result.FTraceId := Parent.FTraceId;
      Result.FParentSpanId := Parent.FSpanId;
    end;
  end;
  PushThreadSpan(Result);
end;

procedure TAiTelemetry.EndSpan(ASpan: TAiSpan; const AErrorMsg: string);
begin
  if not Assigned(ASpan) then
    Exit;
  ASpan.FEndUnixNano := NowUnixNano;
  if AErrorMsg <> '' then
    ASpan.SetError(AErrorMsg);
  PopThreadSpan(ASpan);
  FLock.Enter;
  try
    if FPending.Count < FMaxBatchSize * 10 then
      FPending.Add(ASpan) // la lista toma posesion
    else
    begin
      Inc(FDroppedSpans); // backpressure: collector caido y cola llena
      ASpan.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TAiTelemetry.Flush;
begin
  ExportPending;
end;

function TAiTelemetry.BuildOtlpPayload(ABatch: TObjectList<TAiSpan>): TJSONObject;
var
  ResourceSpans, ScopeSpans, Resource, Scope, SpanObj, StatusObj, AttrVal: TJSONObject;
  RsArray, SsArray, SpansArray, ResAttrs: TJSONArray;
  Span: TAiSpan;
  KindVal: Integer;
begin
  // Estructura OTLP/JSON: resourceSpans -> scopeSpans -> spans
  Result := TJSONObject.Create;
  RsArray := TJSONArray.Create;
  Result.AddPair('resourceSpans', RsArray);

  ResourceSpans := TJSONObject.Create;
  RsArray.AddElement(ResourceSpans);

  Resource := TJSONObject.Create;
  ResourceSpans.AddPair('resource', Resource);
  ResAttrs := TJSONArray.Create;
  Resource.AddPair('attributes', ResAttrs);
  AttrVal := TJSONObject.Create;
  AttrVal.AddPair('stringValue', FServiceName);
  ResAttrs.AddElement(TJSONObject.Create
    .AddPair('key', 'service.name')
    .AddPair('value', AttrVal));

  SsArray := TJSONArray.Create;
  ResourceSpans.AddPair('scopeSpans', SsArray);
  ScopeSpans := TJSONObject.Create;
  SsArray.AddElement(ScopeSpans);
  Scope := TJSONObject.Create;
  Scope.AddPair('name', OTEL_SCOPE_NAME);
  Scope.AddPair('version', OTEL_SCOPE_VERSION);
  ScopeSpans.AddPair('scope', Scope);

  SpansArray := TJSONArray.Create;
  ScopeSpans.AddPair('spans', SpansArray);

  for Span in ABatch do
  begin
    SpanObj := TJSONObject.Create;
    SpanObj.AddPair('traceId', Span.FTraceId);
    SpanObj.AddPair('spanId', Span.FSpanId);
    if Span.FParentSpanId <> '' then
      SpanObj.AddPair('parentSpanId', Span.FParentSpanId);
    SpanObj.AddPair('name', Span.FName);
    case Span.FKind of
      skInternal: KindVal := 1;
      skServer:   KindVal := 2;
      skClient:   KindVal := 3;
      skProducer: KindVal := 4;
    else
      KindVal := 5; // skConsumer
    end;
    SpanObj.AddPair('kind', TJSONNumber.Create(KindVal));
    SpanObj.AddPair('startTimeUnixNano', IntToStr(Span.FStartUnixNano));
    SpanObj.AddPair('endTimeUnixNano', IntToStr(Span.FEndUnixNano));
    SpanObj.AddPair('attributes', TJSONArray(Span.FAttributes.Clone));
    StatusObj := TJSONObject.Create;
    if Span.FHasError then
    begin
      StatusObj.AddPair('code', TJSONNumber.Create(2)); // STATUS_CODE_ERROR
      StatusObj.AddPair('message', Span.FErrorMessage);
    end
    else
      StatusObj.AddPair('code', TJSONNumber.Create(1)); // STATUS_CODE_OK
    SpanObj.AddPair('status', StatusObj);
    SpansArray.AddElement(SpanObj);
  end;
end;

procedure TAiTelemetry.ExportPending;
var
  Batch: TObjectList<TAiSpan>;
  Payload: TJSONObject;
  Http: THTTPClient;
  Body: TStringStream;
  Resp: IHTTPResponse;
  i: Integer;
  LHeaders: TNetHeaders;
begin
  // Tomar el lote bajo lock y soltarlo rapido
  FLock.Enter;
  try
    if FPending.Count = 0 then
      Exit;
    Batch := FPending;
    FPending := TObjectList<TAiSpan>.Create(True);
  finally
    FLock.Leave;
  end;

  try
    Payload := BuildOtlpPayload(Batch);
    try
      Http := THTTPClient.Create;
      try
        Http.ConnectionTimeout := 3000;
        Http.ResponseTimeout := 5000;
        Http.ContentType := 'application/json';
        LHeaders := [];
        for i := 0 to FHeaders.Count - 1 do
          if FHeaders.Names[i] <> '' then
            LHeaders := LHeaders + [TNetHeader.Create(FHeaders.Names[i], FHeaders.ValueFromIndex[i])];
        Body := TStringStream.Create(Payload.ToJSON, TEncoding.UTF8);
        try
          Resp := Http.Post(FEndpoint, Body, nil, LHeaders);
          if (Resp.StatusCode < 200) or (Resp.StatusCode >= 300) then
            DoLog(Format('OTLP export fallo: HTTP %d %s', [Resp.StatusCode, Resp.StatusText]))
          else
            DoLog(Format('OTLP export: %d spans', [Batch.Count]));
        finally
          Body.Free;
        end;
      finally
        Http.Free;
      end;
    finally
      Payload.Free;
    end;
  except
    on E: Exception do
      // Collector caido: se descarta el lote (telemetria es best-effort)
      DoLog('OTLP export excepcion: ' + E.Message);
  end;
  Batch.Free;
end;

// ----------------------------------------------------------------------------
// Helpers no-op-safe
// ----------------------------------------------------------------------------

function AiSpanStart(const AName: string; AKind: TAiSpanKind; const ATraceParent: string): TAiSpan;
var
  T: TAiTelemetry;
begin
  Result := nil;
  T := TAiTelemetry.Active;
  if Assigned(T) then
    try
      Result := T.StartSpan(AName, AKind, ATraceParent);
    except
      Result := nil; // la telemetria jamas debe romper el flujo instrumentado
    end;
end;

procedure AiSpanEnd(ASpan: TAiSpan; const AErrorMsg: string);
var
  T: TAiTelemetry;
begin
  if not Assigned(ASpan) then
    Exit;
  T := TAiTelemetry.Active;
  if Assigned(T) then
    try
      T.EndSpan(ASpan, AErrorMsg);
    except
      // best-effort
    end
  else
    ASpan.Free; // la instancia se deshabilito entre Start y End
end;

procedure AiSpanAttr(ASpan: TAiSpan; const AKey, AValue: string);
begin
  if Assigned(ASpan) then
    ASpan.SetAttribute(AKey, AValue);
end;

procedure AiSpanAttr(ASpan: TAiSpan; const AKey: string; AValue: Int64);
begin
  if Assigned(ASpan) then
    ASpan.SetAttribute(AKey, AValue);
end;

procedure AiSpanAttr(ASpan: TAiSpan; const AKey: string; AValue: Boolean);
begin
  if Assigned(ASpan) then
    ASpan.SetAttribute(AKey, AValue);
end;

function AiSpanTraceParent(ASpan: TAiSpan): string;
begin
  if Assigned(ASpan) then
    Result := ASpan.TraceParent
  else
    Result := '';
end;

end.
