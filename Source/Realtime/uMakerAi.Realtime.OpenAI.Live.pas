// MakerAI Suite — Driver OpenAI GPT-Live (voz full-duplex)
// wss://api.openai.com/v1/live/sessions  (Authorization: Bearer)
//
// GPT-Live escucha mientras habla: el audio del usuario se envia en flujo
// continuo y el modelo decide cuando hablar (sin VAD, commits ni turnos).
// El razonamiento pesado se DELEGA a otro modelo:
//   - Delegation = ldResponses (default): OpenAI lo pasa a un modelo de la
//     API Responses (DelegationModel) con las tools declaradas. Las funciones
//     locales (AiFunctions / OnCallToolFunction) se ejecutan aqui y el
//     resultado vuelve con response.item.create + response.create.
//   - Delegation = ldClient, o DelegateChat asignado: la aplicacion resuelve
//     la tarea. Con DelegateChat (TAiChatConnection: Claude, Ollama, un grafo
//     de agentes...) el driver arma el pedido con la transcripcion, ejecuta el
//     chat en un hilo y devuelve la respuesta con session.commentary.append.
//     Sin DelegateChat se dispara OnDelegation y la aplicacion contesta con
//     AppendCommentary / AppendThinking.
//
// Protocolo (tipos del SDK oficial openai-python, src/openai/types/live):
//   cliente:  session.start (primero; esperar session.started),
//             session.input_audio.append, session.input_audio.mute/unmute,
//             session.commentary/thinking/instructions.append,
//             response.item.create, response.create, session.close
//   servidor: session.started, session.input_transcript.delta,
//             session.output_transcript.delta, session.output_audio.delta,
//             session.delegation.created, response.event (evento Responses
//             anidado), session.usage.updated, session.closed, error, info
//
// Diferencias con gpt-realtime que maneja el driver:
//   - Los transcripts no marcan turnos ni tienen evento "done", y en una
//     conversacion full-duplex las voces se superponen (el modelo puede
//     empezar antes de que el usuario termine). Los deltas se emiten al
//     llegar; los turnos completos (OnTranscriptCompleted / OnAssistantText)
//     se arman con la linea de tiempo de cada fragmento (start_ms/end_ms),
//     con la politica del agrupador del SDK oficial:
//       * el turno cambia solo si el otro empieza 500 ms o mas despues del
//         final del turno en curso; antes de eso su habla queda en espera y
//         el turno en curso sigue sumando;
//       * un fragmento que empezo antes que el turno actual es transcripcion
//         atrasada del turno anterior (se espera hasta 1 s);
//       * el turno del asistente se cierra tras 2 s sin texto nuevo NI voz
//         en el audio de salida (los transcripts llegan a rafagas: una pausa
//         de 1 s entre frases podia verse como 2 s y partir un cuento largo);
//         si el modelo sigue hablando, el turno se retoma en vez de abrir otro.
//     No se descartan "mhm"/"aja" (el SDK si): un acuse corto cuenta como turno.
//   - El audio de salida es un flujo continuo (incluye silencio), sin marcas
//     de tiempo ni evento de fin: OnAudioDone se dispara solo en session.closed.
//   - Voz, formato de audio e instrucciones no cambian despues de arrancar.
//   - El modelo va en session.start, no en la URL.
// Audio: PCM16 mono a 24 kHz (default) o 16 kHz (AudioRate). G.711 (8 kHz)
// no esta soportado por el driver.
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.OpenAI.Live;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  System.SyncObjs, System.Generics.Collections, System.Diagnostics, System.Math,
  uMakerAi.Core, uMakerAi.Chat.Messages, uMakerAi.Tools.Functions,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Realtime, uMakerAi.Realtime.WebSocket, uMakerAi.WebSocket.Client;

type
  // Frecuencia del audio PCM16 de entrada y salida
  TAiLiveAudioRate = (lar24k, lar16k);

  // Quien resuelve las tareas que delega el modelo de voz
  TAiLiveDelegation = (
    ldResponses, // un modelo de la API Responses gestionado por OpenAI
    ldClient     // la aplicacion (DelegateChat u OnDelegation)
  );

  // Esfuerzo de razonamiento del modelo delegado (lreDefault = no se envia)
  TAiLiveReasoningEffort = (lreDefault, lreNone, lreMinimal, lreLow,
    lreMedium, lreHigh, lreXHigh);

  // Fallback de function calling cuando AiFunctions no maneja la funcion.
  // El handler debe asignar ToolCall.Response con el resultado.
  TAiLiveToolCallEvent = procedure(Sender: TObject;
    ToolCall: TAiToolsFunction) of object;

  // Delegacion a la aplicacion. Context es la transcripcion desde la ultima
  // delegacion (el evento del servidor no trae el texto de la tarea).
  // Responder con AppendCommentary(Texto, DelegationId).
  TAiLiveDelegationEvent = procedure(Sender: TObject;
    const DelegationId, Context: string) of object;

  // Consumo acumulado: segundos de audio de la sesion y uso del contexto
  // (0..1; -1 si el servidor no lo informa)
  TAiLiveUsageEvent = procedure(Sender: TObject;
    Seconds, ContextRatio: Double) of object;

  // Fin de la sesion: close_requested, expired, content, remote_hangup o
  // connection_lost
  TAiLiveClosedEvent = procedure(Sender: TObject; const Reason: string;
    Seconds: Double) of object;

  // Evento crudo del modelo delegado (Responses), para UI o diagnostico
  TAiLiveResponseEvent = procedure(Sender: TObject;
    const DelegationId, EventType, EventJson: string) of object;

  // Turno de conversacion armado con los fragmentos de transcripcion
  TAiLiveTurn = record
    Speaker:  Integer; // 0 vacio, 1 usuario, 2 asistente
    Text:     string;
    SentText: string;  // texto ya enviado en una delegacion
    StartMs:  Int64;   // linea de tiempo de la sesion
    EndMs:    Int64;
    Wall:     Int64;   // reloj local del ultimo fragmento (NowMs)
  end;

  // Estado de las llamadas a funciones de una respuesta delegada
  TAiLiveToolTurn = record
    Pending:   Integer; // funciones en ejecucion
    HadCalls:  Boolean; // la respuesta pidio funciones
    Completed: Boolean; // llego response.completed
  end;

  TAiOpenAiLiveChat = class(TAiRealtimeVoiceBase)
  private
    FWebSocket:      TAiRealtimeWSClient;
    FConnectThread:  TThread;
    FB64:            TBase64Encoding; // sin saltos de linea
    FSessionStarted: Boolean;
    FClosedEvent:    TEvent;          // session.closed recibido
    // Configuracion
    FVoice:             string;
    FInstructions:      string;
    FAudioRate:         TAiLiveAudioRate;
    FDelegation:        TAiLiveDelegation;
    FDelegationModel:   string;
    FDelegationInstructions: string;
    FReasoningEffort:   TAiLiveReasoningEffort;
    FMaxOutputTokens:   Integer;
    FParallelToolCalls: Boolean;
    FToolChoice:        string;
    FEnableWebSearch:   Boolean;
    FStore:             Boolean;
    FCloseTimeoutMs:    Integer;
    FCustomToolsJson:   TStrings;
    FAiFunctions:       TAiFunctions;      // referencia externa
    FDelegateChat:      TAiChatConnection; // referencia externa
    // Eventos propios
    FOnCallToolFunction: TAiLiveToolCallEvent;
    FOnDelegation:       TAiLiveDelegationEvent;
    FOnUsage:            TAiLiveUsageEvent;
    FOnSessionClosed:    TAiLiveClosedEvent;
    FOnResponseEvent:    TAiLiveResponseEvent;
    // Estado de la sesion
    FSessionId:    string;
    FUsageSeconds: Double;
    FCloseReason:  string;
    FEventSeq:     Integer;
    // Turnos de la conversacion (hilo principal: TAiWSClient entrega cada
    // frame con TThread.Queue, asi que ProcessServerEvent corre ahi):
    //   FPrev: turno anterior, esperando fragmentos atrasados antes de cerrar
    //   FCur:  turno en curso
    //   FBuf:  habla del otro superpuesta al turno en curso (en espera)
    FClock:          TStopwatch;
    FPrev, FCur, FBuf: TAiLiveTurn;
    FTimelineMax:    Int64;       // fin del ultimo fragmento (ms de sesion)
    FVoiceWall:      Int64;       // ultimo audio de salida con voz (NowMs)
    FTranscript:     TStringList; // turnos cerrados 'User: ...' / 'Assistant: ...'
    FDelegatedLines: Integer;     // primera linea no revisada por DelegationContext
    // Hilos de trabajo (funciones y DelegateChat)
    FToolLock:      TCriticalSection;
    FToolTurns:     TDictionary<string, TAiLiveToolTurn>; // por delegation_id
    FDelegateLock:  TCriticalSection; // DelegateChat atiende una tarea a la vez
    FActiveWorkers: Integer;
    FShuttingDown:  Boolean;
    procedure OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
      const Data: TBytes; IsFinal: Boolean);
    procedure OnWSConnected(Sender: TObject);
    procedure OnWSDisconnected(Sender: TObject);
    procedure OnWSError(Sender: TObject; const ErrorMsg: string);
    procedure SetAiFunctions(const Value: TAiFunctions);
    procedure SetDelegateChat(const Value: TAiChatConnection);
    procedure SetCustomToolsJson(const Value: TStrings);
    function  NextEventId: string;
    function  BuildToolsArray: TJSONArray;
    function  BuildDelegation: TJSONObject;
    // Segmentos de transcripcion -> turnos
    procedure HandleFragment(ASpeaker: Integer; const ADelta: string;
      AStartMs, AEndMs: Int64);
    procedure AppendToTurn(var ATurn: TAiLiveTurn; ASpeaker: Integer;
      const ADelta: string; AStartMs, AEndMs: Int64);
    procedure FinishTurn(var ATurn: TAiLiveTurn);
    procedure PromoteBuffered;
    procedure AdvanceTimeline(ATimeMs: Int64);
    procedure CheckSegmentTimers;
    procedure FinishAllTurns;
    function  DelegationContext: string;
    // Delegacion
    procedure HandleDelegationCreated(const JObj: TJSONObject);
    procedure RunDelegateChat(const ADelegationId, AContext: string);
    procedure HandleResponseEvent(const JObj: TJSONObject);
    procedure HandleFunctionCall(const AKey: string; const AItem: TJSONObject);
    procedure TryContinueAfterTools(const AKey: string);
    procedure SendFunctionOutput(const ACallId, AOutput: string);
    procedure SendAppend(const AType, AContent, ADelegationId: string);
    procedure StartWorker(AProc: TProc);
    // Dispatchers de los eventos propios (TThread.Queue al hilo principal)
    procedure DoDelegation(const ADelegationId, AContext: string);
    procedure DoUsage(ASeconds, ARatio: Double);
    procedure DoSessionClosed(const AReason: string; ASeconds: Double);
    procedure DoResponseEvent(const ADelegationId, AType, AJson: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetTargetSampleRate: Integer; override;
    procedure InternalSendAudio(const ResampledPCM16: TBytes); override;
    procedure InternalConnect;    override;
    procedure InternalDisconnect; override;
    // Flujo continuo: el modelo decide cuando hablar, no hay turnos que cerrar
    procedure InternalCommitAudio; override;
    procedure InternalClearAudio;  override;
    // Procesa un evento JSON del servidor (protegido: la suite lo alimenta sin red)
    procedure ProcessServerEvent(const JObj: TJSONObject); virtual;
    // Reloj local en ms para cerrar turnos por inactividad (virtual: la suite
    // lo controla)
    function  NowMs: Int64; virtual;
    // Mensaje session.start completo (protegido: la suite lo inspecciona)
    function  BuildSessionStart: TJSONObject;
    // Envia un mensaje y lo libera (virtual: la suite captura lo enviado)
    procedure SendJson(AObj: TJSONObject); virtual;
    // Delegacion efectiva: DelegateChat asignado fuerza ldClient
    function  EffectiveDelegation: TAiLiveDelegation;
    // Parte un texto en fragmentos de hasta AMaxBytes en UTF-8 (las
    // inyecciones admiten 500 tokens); corta en espacios cuando puede
    class function SplitForAppend(const AText: string;
      AMaxBytes: Integer = 400): TArray<string>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    // El callback de WaveIn no puede tocar sockets: el envio se encola al hilo principal
    procedure SendAudioChunk(const PCM16Data: TBytes); override;
    // Deja de pasar el audio del microfono al modelo (y lo reanuda)
    procedure Mute;
    procedure Unmute;
    // Texto que el modelo puede decir al usuario (resultado de una tarea).
    // ADelegationId: el de OnDelegation, o '' para contexto general. Con
    // ldResponses solo se admite ''. Textos largos se parten en varios envios.
    procedure AppendCommentary(const AText: string; const ADelegationId: string = '');
    // Contexto silencioso o progreso (no pide que el modelo hable)
    procedure AppendThinking(const AText: string; const ADelegationId: string = '');
    // Instrucciones nuevas durante la conversacion (p.ej. cambiar de tema)
    procedure AppendInstructions(const AText: string; const ADelegationId: string = '');
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
    // Id de la sesion asignado por el servidor (session.started)
    property SessionId: string read FSessionId;
    // Segundos de audio acumulados (session.usage.updated / session.closed)
    property UsageSeconds: Double read FUsageSeconds;
    // Motivo del ultimo cierre de sesion
    property CloseReason: string read FCloseReason;
  published
    // Voz del modelo: marin (default del servidor), cedar, alloy, coral,
    // sage, verse... o el id de una voz propia. No cambia tras conectar.
    property Voice: string read FVoice write FVoice;
    // Instrucciones de conversacion (tono, interrupciones, cuando delegar).
    // Las reglas de negocio van en DelegationInstructions.
    property Instructions: string read FInstructions write FInstructions;
    property AudioRate: TAiLiveAudioRate read FAudioRate write FAudioRate default lar24k;
    property Delegation: TAiLiveDelegation read FDelegation write FDelegation default ldResponses;
    // Modelo de la API Responses que resuelve las tareas (ldResponses)
    property DelegationModel: string read FDelegationModel write FDelegationModel;
    // Prompt del modelo delegado (ldResponses), separado de Instructions
    property DelegationInstructions: string read FDelegationInstructions
      write FDelegationInstructions;
    property ReasoningEffort: TAiLiveReasoningEffort read FReasoningEffort
      write FReasoningEffort default lreDefault;
    // Tokens maximos por respuesta delegada (0 = no se envia)
    property MaxOutputTokens: Integer read FMaxOutputTokens write FMaxOutputTokens default 0;
    property ParallelToolCalls: Boolean read FParallelToolCalls
      write FParallelToolCalls default True;
    // '' (no se envia), 'auto', 'none', 'required' o el nombre de una funcion
    property ToolChoice: string read FToolChoice write FToolChoice;
    // Busqueda web del modelo delegado (la ejecuta OpenAI)
    property EnableWebSearch: Boolean read FEnableWebSearch write FEnableWebSearch default False;
    // Guardar la sesion en OpenAI (para forks y descarga de la grabacion)
    property Store: Boolean read FStore write FStore default False;
    // Espera maxima de session.closed al desconectar (la documentacion
    // sugiere hasta 15 s; 0 = no esperar)
    property CloseTimeoutMs: Integer read FCloseTimeoutMs write FCloseTimeoutMs default 5000;
    // Funciones locales y MCP del modelo delegado (ldResponses)
    property AiFunctions: TAiFunctions read FAiFunctions write SetAiFunctions;
    // Tools extra en JSON crudo (objeto o array de tools 'function')
    property CustomToolsJson: TStrings read FCustomToolsJson write SetCustomToolsJson;
    // Chat que resuelve las tareas delegadas (cualquier proveedor). Asignado,
    // la sesion usa ldClient. Se fuerza Asynchronous=False al conectar.
    property DelegateChat: TAiChatConnection read FDelegateChat write SetDelegateChat;
    property OnCallToolFunction: TAiLiveToolCallEvent read FOnCallToolFunction
      write FOnCallToolFunction;
    property OnDelegation: TAiLiveDelegationEvent read FOnDelegation write FOnDelegation;
    property OnUsage: TAiLiveUsageEvent read FOnUsage write FOnUsage;
    property OnSessionClosed: TAiLiveClosedEvent read FOnSessionClosed write FOnSessionClosed;
    property OnResponseEvent: TAiLiveResponseEvent read FOnResponseEvent
      write FOnResponseEvent;
  end;

procedure Register;

implementation

const
  CLIVE_WSS = 'wss://api.openai.com/v1/live/sessions';
  CLIVE_DEFAULT_DELEGATION_MODEL = 'gpt-6-luna';
  CSPEAKER_USER      = 1;
  CSPEAKER_ASSISTANT = 2;
  // Separacion minima para cambiar de turno (min_turn_separation_ms del SDK)
  CMIN_TURN_SEPARATION_MS = 500;
  // Silencio del asistente que cierra su turno (assistant_silence_ms del SDK)
  CASSISTANT_SILENCE_MS = 2000;
  // Reloj local: espera de fragmentos atrasados del turno anterior, y de un
  // turno en curso que dejo de recibir texto mientras el otro habla
  CLATE_FRAGMENT_MS = 1000;
  // Promedio absoluto PCM16 por encima del cual el audio de salida tiene voz
  // (el servidor manda silencio digital entre frases)
  COUTPUT_VOICE_LEVEL = 500;
  // Pedido que recibe DelegateChat: la transcripcion es automatica y la
  // respuesta se dira en voz alta
  CDELEGATE_PROMPT =
    'Voice conversation so far (automatic transcription, may contain errors):' +
    sLineBreak + '%s' + sLineBreak + sLineBreak +
    'The voice assistant delegated the user''s current request to you. ' +
    'Resolve it and reply with the result only, in the user''s language, as ' +
    'plain text that will be read aloud (no markdown, no lists).';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenAiLiveChat]);
end;

{ TAiOpenAiLiveChat }

constructor TAiOpenAiLiveChat.Create(AOwner: TComponent);
begin
  inherited;
  ApiKey := '@OPENAI_API_KEY';
  Model := GetDefaultModel;
  FAudioRate := lar24k;
  FDelegation := ldResponses;
  FDelegationModel := CLIVE_DEFAULT_DELEGATION_MODEL;
  FReasoningEffort := lreDefault;
  FParallelToolCalls := True;
  FCloseTimeoutMs := 5000;
  FCustomToolsJson := TStringList.Create;
  FTranscript := TStringList.Create;
  FToolLock := TCriticalSection.Create;
  FDelegateLock := TCriticalSection.Create;
  FToolTurns := TDictionary<string, TAiLiveToolTurn>.Create;
  FClosedEvent := TEvent.Create(nil, True, False, '');
  FB64 := TBase64Encoding.Create(0);
  FClock := TStopwatch.StartNew;
  FWebSocket := TAiRealtimeWSClient.Create;
  FWebSocket.OnFrame        := OnWSFrame;
  FWebSocket.OnConnected    := OnWSConnected;
  FWebSocket.OnDisconnected := OnWSDisconnected;
  FWebSocket.OnError        := OnWSError;
end;

destructor TAiOpenAiLiveChat.Destroy;
var
  Waited: Integer;
begin
  FShuttingDown := True;
  if IsConnected then InternalDisconnect;
  if Assigned(FConnectThread) then
  begin
    FWebSocket.Disconnect;
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  // Los hilos de funciones y de DelegateChat usan Self: esperarlos (hasta
  // 30 s). Desde el hilo principal se drena la cola por si sincronizan.
  Waited := 0;
  while (TInterlocked.CompareExchange(FActiveWorkers, 0, 0) > 0) and (Waited < 30000) do
  begin
    if TThread.CurrentThread.ThreadID = MainThreadID then
      CheckSynchronize(10)
    else
      Sleep(10);
    Inc(Waited, 10);
  end;
  FWebSocket.Free;
  FB64.Free;
  FClosedEvent.Free;
  FToolTurns.Free;
  FDelegateLock.Free;
  FToolLock.Free;
  FTranscript.Free;
  FCustomToolsJson.Free;
  inherited;
end;

procedure TAiOpenAiLiveChat.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FAiFunctions then
      FAiFunctions := nil;
    if AComponent = FDelegateChat then
      FDelegateChat := nil;
  end;
end;

procedure TAiOpenAiLiveChat.SetAiFunctions(const Value: TAiFunctions);
begin
  if FAiFunctions = Value then Exit;
  if Assigned(FAiFunctions) then
    FAiFunctions.RemoveFreeNotification(Self);
  FAiFunctions := Value;
  if Assigned(FAiFunctions) then
    FAiFunctions.FreeNotification(Self);
end;

procedure TAiOpenAiLiveChat.SetDelegateChat(const Value: TAiChatConnection);
begin
  if FDelegateChat = Value then Exit;
  if Assigned(FDelegateChat) then
    FDelegateChat.RemoveFreeNotification(Self);
  FDelegateChat := Value;
  if Assigned(FDelegateChat) then
    FDelegateChat.FreeNotification(Self);
end;

procedure TAiOpenAiLiveChat.SetCustomToolsJson(const Value: TStrings);
begin
  FCustomToolsJson.Assign(Value);
end;

class function TAiOpenAiLiveChat.GetDriverName: string;
begin
  Result := 'OpenAiLive';
end;

class function TAiOpenAiLiveChat.GetDefaultModel: string;
begin
  Result := 'gpt-live-1';
end;

function TAiOpenAiLiveChat.GetTargetSampleRate: Integer;
begin
  if FAudioRate = lar16k then
    Result := 16000
  else
    Result := 24000;
end;

function TAiOpenAiLiveChat.EffectiveDelegation: TAiLiveDelegation;
begin
  if Assigned(FDelegateChat) then
    Result := ldClient
  else
    Result := FDelegation;
end;

function TAiOpenAiLiveChat.NextEventId: string;
begin
  Result := 'mkai_' + IntToStr(TInterlocked.Increment(FEventSeq));
end;

procedure TAiOpenAiLiveChat.SendJson(AObj: TJSONObject);
begin
  try
    FWebSocket.SendText(AObj.ToJSON);
  finally
    AObj.Free;
  end;
end;

// -----------------------------------------------------------------------------
// session.start
// -----------------------------------------------------------------------------

function TAiOpenAiLiveChat.BuildToolsArray: TJSONArray;
var
  JTool: TJSONObject;
  JParsed: TJSONValue;
  ToolsStr: string;
  I: Integer;
begin
  Result := TJSONArray.Create;

  if FEnableWebSearch then
  begin
    JTool := TJSONObject.Create;
    JTool.AddPair('type', 'web_search');
    Result.Add(JTool);
  end;

  // Formato plano de la API Responses: {type:function, name, description, parameters}
  if Assigned(FAiFunctions) then
  begin
    ToolsStr := FAiFunctions.GetTools(tfOpenAIResponses);
    if ToolsStr <> '' then
    begin
      JParsed := TJSONObject.ParseJSONValue(ToolsStr);
      try
        if JParsed is TJSONArray then
          for I := 0 to TJSONArray(JParsed).Count - 1 do
            Result.Add(TJSONObject(TJSONArray(JParsed).Items[I].Clone));
      finally
        JParsed.Free;
      end;
    end;
  end;

  if Trim(FCustomToolsJson.Text) <> '' then
  begin
    JParsed := TJSONObject.ParseJSONValue(FCustomToolsJson.Text);
    try
      if JParsed is TJSONArray then
      begin
        for I := 0 to TJSONArray(JParsed).Count - 1 do
          Result.Add(TJSONObject(TJSONArray(JParsed).Items[I].Clone));
      end
      else if JParsed is TJSONObject then
        Result.Add(TJSONObject(JParsed.Clone));
    finally
      JParsed.Free;
    end;
  end;

  if Result.Count = 0 then
    FreeAndNil(Result);
end;

function TAiOpenAiLiveChat.BuildDelegation: TJSONObject;
const
  CEfforts: array[TAiLiveReasoningEffort] of string =
    ('', 'none', 'minimal', 'low', 'medium', 'high', 'xhigh');
var
  JResp, JReasoning, JChoice: TJSONObject;
  JTools: TJSONArray;
  LModel, LChoice: string;
begin
  Result := TJSONObject.Create;
  if EffectiveDelegation = ldClient then
  begin
    Result.AddPair('type', 'client');
    Exit;
  end;

  Result.AddPair('type', 'responses');
  JResp := TJSONObject.Create;
  LModel := Trim(FDelegationModel);
  if LModel = '' then
    LModel := CLIVE_DEFAULT_DELEGATION_MODEL;
  JResp.AddPair('model', LModel);
  if Trim(FDelegationInstructions) <> '' then
    JResp.AddPair('instructions', FDelegationInstructions);
  if FMaxOutputTokens > 0 then
    JResp.AddPair('max_output_tokens', TJSONNumber.Create(FMaxOutputTokens));
  if not FParallelToolCalls then
    JResp.AddPair('parallel_tool_calls', TJSONFalse.Create);
  if FReasoningEffort <> lreDefault then
  begin
    JReasoning := TJSONObject.Create;
    JReasoning.AddPair('effort', CEfforts[FReasoningEffort]);
    JResp.AddPair('reasoning', JReasoning);
  end;

  JTools := BuildToolsArray;
  if Assigned(JTools) then
    JResp.AddPair('tools', JTools);

  LChoice := Trim(FToolChoice);
  if LChoice <> '' then
  begin
    if SameText(LChoice, 'auto') or SameText(LChoice, 'none') or SameText(LChoice, 'required') then
      JResp.AddPair('tool_choice', LowerCase(LChoice))
    else
    begin
      JChoice := TJSONObject.Create;
      JChoice.AddPair('type', 'function');
      JChoice.AddPair('name', LChoice);
      JResp.AddPair('tool_choice', JChoice);
    end;
  end;

  Result.AddPair('responses', JResp);
end;

function TAiOpenAiLiveChat.BuildSessionStart: TJSONObject;
var
  JSession, JAudio, JFormat, JOutput: TJSONObject;
  LModel: string;
begin
  LModel := Model;
  if LModel = '' then
    LModel := GetDefaultModel;

  JSession := TJSONObject.Create;
  JSession.AddPair('model', LModel);
  if Trim(FInstructions) <> '' then
    JSession.AddPair('instructions', FInstructions);

  JAudio := TJSONObject.Create;
  JFormat := TJSONObject.Create;
  JFormat.AddPair('type', 'audio/pcm');
  JFormat.AddPair('rate', TJSONNumber.Create(GetTargetSampleRate));
  JAudio.AddPair('format', JFormat);
  if Trim(FVoice) <> '' then
  begin
    JOutput := TJSONObject.Create;
    JOutput.AddPair('voice', Trim(FVoice));
    JAudio.AddPair('output', JOutput);
  end;
  JSession.AddPair('audio', JAudio);

  JSession.AddPair('delegation', BuildDelegation);
  if FStore then
    JSession.AddPair('store', TJSONTrue.Create);

  Result := TJSONObject.Create;
  Result.AddPair('type', 'session.start');
  Result.AddPair('event_id', NextEventId);
  Result.AddPair('session', JSession);
end;

// -----------------------------------------------------------------------------
// Eventos del servidor
// -----------------------------------------------------------------------------

// Promedio del valor absoluto de las muestras PCM16
function AudioLevel(const AData: TBytes): Integer;
var
  I, N: Integer;
  Sum: Int64;
begin
  N := Length(AData) div 2;
  if N = 0 then
    Exit(0);
  Sum := 0;
  for I := 0 to N - 1 do
    Inc(Sum, Abs(PSmallInt(@AData[I * 2])^));
  Result := Sum div N;
end;

function TAiOpenAiLiveChat.NowMs: Int64;
begin
  Result := FClock.ElapsedMilliseconds;
end;

procedure TAiOpenAiLiveChat.ProcessServerEvent(const JObj: TJSONObject);
var
  EventType, S, Code: string;
  JSession, JUsage, JCtx, JError: TJSONObject;
  Seconds, Ratio: Double;
  StartMs, EndMs: Int64;
  Audio: TBytes;
begin
  if not JObj.TryGetValue<string>('type', EventType) then Exit;
  // El audio de salida llega cada ~100 ms: sirve de reloj para cerrar turnos
  CheckSegmentTimers;

  if EventType = 'session.started' then
  begin
    if JObj.TryGetValue<TJSONObject>('session', JSession) then
      JSession.TryGetValue<string>('id', FSessionId);
    FSessionStarted := True;
    DoSessionReady;
  end

  else if (EventType = 'session.input_transcript.delta') or
          (EventType = 'session.output_transcript.delta') then
  begin
    S := '';
    JObj.TryGetValue<string>('delta', S);
    // Sin marcas (-1) el fragmento se ubica despues del ultimo. TryGetValue
    // deja el valor en 0 si el campo falta: hay que mirar el resultado
    if not JObj.TryGetValue<Int64>('start_ms', StartMs) then
      StartMs := -1;
    if not JObj.TryGetValue<Int64>('end_ms', EndMs) then
      EndMs := -1;
    if S <> '' then
    begin
      if EventType = 'session.input_transcript.delta' then
        HandleFragment(CSPEAKER_USER, S, StartMs, EndMs)
      else
        HandleFragment(CSPEAKER_ASSISTANT, S, StartMs, EndMs);
    end;
  end

  else if EventType = 'session.output_audio.delta' then
  begin
    S := '';
    JObj.TryGetValue<string>('delta', S);
    if S <> '' then
    begin
      Audio := TNetEncoding.Base64.DecodeStringToBytes(S);
      // El audio llega a ritmo real (los transcripts, a rafagas): mientras
      // tenga voz, el asistente sigue hablando
      if AudioLevel(Audio) > COUTPUT_VOICE_LEVEL then
        FVoiceWall := NowMs;
      DoAudioChunk(Audio);
    end;
  end

  else if EventType = 'session.delegation.created' then
    HandleDelegationCreated(JObj)

  else if EventType = 'response.event' then
    HandleResponseEvent(JObj)

  else if EventType = 'session.usage.updated' then
  begin
    Seconds := FUsageSeconds;
    Ratio := -1;
    if JObj.TryGetValue<TJSONObject>('usage', JUsage) then
      JUsage.TryGetValue<Double>('seconds', Seconds);
    if JObj.TryGetValue<TJSONObject>('context_window', JCtx) then
      JCtx.TryGetValue<Double>('usage_ratio', Ratio);
    FUsageSeconds := Seconds;
    DoUsage(Seconds, Ratio);
  end

  else if EventType = 'session.closed' then
  begin
    FCloseReason := '';
    JObj.TryGetValue<string>('reason', FCloseReason);
    if JObj.TryGetValue<TJSONObject>('usage', JUsage) then
      JUsage.TryGetValue<Double>('seconds', FUsageSeconds);
    // Los transcripts no tienen evento de fin: cerrar lo que quede abierto
    FinishAllTurns;
    FSessionStarted := False;
    DoAudioDone;
    DoSessionClosed(FCloseReason, FUsageSeconds);
    FClosedEvent.SetEvent;
  end

  else if EventType = 'error' then
  begin
    S := 'Error desconocido';
    Code := '';
    if JObj.TryGetValue<TJSONObject>('error', JError) then
    begin
      JError.TryGetValue<string>('message', S);
      JError.TryGetValue<string>('code', Code);
    end;
    DoError(S, Code);
  end;
  // session.updated, info y los acuses (*.appended, input_audio.muted/unmuted)
  // no requieren accion
end;

procedure TAiOpenAiLiveChat.AppendToTurn(var ATurn: TAiLiveTurn; ASpeaker: Integer;
  const ADelta: string; AStartMs, AEndMs: Int64);
begin
  if ATurn.Speaker = 0 then
  begin
    ATurn.Speaker := ASpeaker;
    ATurn.StartMs := AStartMs;
    ATurn.EndMs := AEndMs;
  end;
  ATurn.Text := ATurn.Text + ADelta;
  if AEndMs > ATurn.EndMs then
    ATurn.EndMs := AEndMs;
  ATurn.Wall := NowMs;
end;

procedure TAiOpenAiLiveChat.FinishTurn(var ATurn: TAiLiveTurn);
var
  T: string;
  Idx: Integer;
begin
  T := Trim(ATurn.Text);
  if (ATurn.Speaker <> 0) and (T <> '') then
  begin
    if ATurn.Speaker = CSPEAKER_USER then
      Idx := FTranscript.Add('User: ' + T)
    else
      Idx := FTranscript.Add('Assistant: ' + T);
    // Ya viajo completo en una delegacion: no repetirlo en la siguiente
    if ATurn.SentText = ATurn.Text then
      FTranscript.Objects[Idx] := TObject(1);
    if ATurn.Speaker = CSPEAKER_USER then
      DoTranscriptCompleted(T, '')
    else
      DoAssistantText(T);
  end;
  ATurn := Default(TAiLiveTurn);
end;

procedure TAiOpenAiLiveChat.PromoteBuffered;
begin
  // El turno en curso termino: pasa a esperar fragmentos atrasados y el habla
  // en espera se vuelve el turno en curso
  FinishTurn(FPrev);
  FPrev := FCur;
  FCur := FBuf;
  FBuf := Default(TAiLiveTurn);
end;

procedure TAiOpenAiLiveChat.AdvanceTimeline(ATimeMs: Int64);
begin
  if FCur.Speaker = 0 then Exit;
  if (FBuf.Speaker <> 0) and (ATimeMs >= FCur.EndMs + CMIN_TURN_SEPARATION_MS) then
    PromoteBuffered
  else if (FCur.Speaker = CSPEAKER_ASSISTANT) and (FBuf.Speaker = 0) and
          (ATimeMs >= FCur.EndMs + CASSISTANT_SILENCE_MS) then
  begin
    FinishTurn(FPrev);
    FinishTurn(FCur);
  end;
end;

procedure TAiOpenAiLiveChat.HandleFragment(ASpeaker: Integer; const ADelta: string;
  AStartMs, AEndMs: Int64);
begin
  // Sin marcas de tiempo: despues de todo lo anterior (cambio de turno por llegada)
  if AStartMs < 0 then
    AStartMs := FTimelineMax + CMIN_TURN_SEPARATION_MS;
  if AEndMs < AStartMs then
    AEndMs := AStartMs;
  if AEndMs > FTimelineMax then
    FTimelineMax := AEndMs;

  // Los deltas se emiten al llegar; solo el cierre de turnos espera
  if ASpeaker = CSPEAKER_USER then
    DoTranscriptDelta(ADelta)
  else
    DoAssistantTextDelta(ADelta);

  AdvanceTimeline(AStartMs);

  // El turno se cerro por inactividad pero el mismo hablante sigue sin una
  // pausa real (segun la linea de tiempo): retomarlo en vez de abrir otro
  if (FCur.Speaker = 0) and (FPrev.Speaker = ASpeaker) and
     (AStartMs < FPrev.EndMs + CASSISTANT_SILENCE_MS) then
  begin
    FCur := FPrev;
    FPrev := Default(TAiLiveTurn);
  end;

  // Transcripcion atrasada del turno anterior
  if (FPrev.Speaker = ASpeaker) and (FCur.Speaker <> 0) and (AStartMs < FCur.StartMs) then
    AppendToTurn(FPrev, ASpeaker, ADelta, AStartMs, AEndMs)
  else if (FCur.Speaker = 0) or (FCur.Speaker = ASpeaker) then
    AppendToTurn(FCur, ASpeaker, ADelta, AStartMs, AEndMs)
  else if FBuf.Speaker <> 0 then
  begin
    AppendToTurn(FBuf, ASpeaker, ADelta, AStartMs, AEndMs);
    if AStartMs >= FCur.EndMs + CMIN_TURN_SEPARATION_MS then
      PromoteBuffered;
  end
  else if AStartMs >= FCur.EndMs + CMIN_TURN_SEPARATION_MS then
  begin
    // Cambio de turno: el actual espera fragmentos atrasados
    FinishTurn(FPrev);
    FPrev := FCur;
    FCur := Default(TAiLiveTurn);
    AppendToTurn(FCur, ASpeaker, ADelta, AStartMs, AEndMs);
  end
  else
    // Habla superpuesta: queda en espera hasta que el turno en curso termine
    AppendToTurn(FBuf, ASpeaker, ADelta, AStartMs, AEndMs);
end;

procedure TAiOpenAiLiveChat.CheckSegmentTimers;
var
  T: Int64;
begin
  if (FPrev.Speaker = 0) and (FCur.Speaker = 0) then Exit;
  T := NowMs;
  if (FPrev.Speaker <> 0) and (T - FPrev.Wall >= CLATE_FRAGMENT_MS) then
    FinishTurn(FPrev);
  if (FBuf.Speaker <> 0) and (T - FCur.Wall >= CLATE_FRAGMENT_MS) then
    PromoteBuffered
  else if (FCur.Speaker = CSPEAKER_ASSISTANT) and (FBuf.Speaker = 0) and
          (T - Max(FCur.Wall, FVoiceWall) >= CASSISTANT_SILENCE_MS) then
  begin
    // Sin texto ni voz: el turno pasa a esperar fragmentos atrasados (o a que
    // el modelo siga) y se cierra si no llegan
    FinishTurn(FPrev);
    FPrev := FCur;
    FPrev.Wall := T;
    FCur := Default(TAiLiveTurn);
  end;
end;

procedure TAiOpenAiLiveChat.FinishAllTurns;
begin
  FinishTurn(FPrev);
  FinishTurn(FCur);
  FinishTurn(FBuf);
end;

function TAiOpenAiLiveChat.DelegationContext: string;
var
  I: Integer;
  SB: TStringBuilder;

  procedure AddOpen(var ATurn: TAiLiveTurn);
  begin
    if (ATurn.Speaker = 0) or (Trim(ATurn.Text) = '') or (ATurn.SentText = ATurn.Text) then
      Exit;
    if ATurn.Speaker = CSPEAKER_USER then
      SB.AppendLine('User: ' + Trim(ATurn.Text))
    else
      SB.AppendLine('Assistant: ' + Trim(ATurn.Text));
    ATurn.SentText := ATurn.Text;
  end;

begin
  // Lo que DelegateChat todavia no vio (conserva su historial): turnos
  // cerrados y abiertos, en el orden de la linea de tiempo
  SB := TStringBuilder.Create;
  try
    for I := FDelegatedLines to FTranscript.Count - 1 do
      if FTranscript.Objects[I] = nil then
        SB.AppendLine(FTranscript[I]);
    FDelegatedLines := FTranscript.Count;
    AddOpen(FPrev);
    AddOpen(FCur);
    AddOpen(FBuf);
    Result := SB.ToString.TrimRight;
  finally
    SB.Free;
  end;
end;

// -----------------------------------------------------------------------------
// Delegacion a la aplicacion (ldClient)
// -----------------------------------------------------------------------------

procedure TAiOpenAiLiveChat.HandleDelegationCreated(const JObj: TJSONObject);
var
  JDeleg: TJSONObject;
  Id, Target, Context: string;
begin
  if not JObj.TryGetValue<TJSONObject>('delegation', JDeleg) then Exit;
  Id := '';
  Target := '';
  JDeleg.TryGetValue<string>('id', Id);
  JDeleg.TryGetValue<string>('target', Target);
  // Las delegaciones a Responses las resuelve OpenAI (ver HandleResponseEvent)
  if (Id = '') or (Target <> 'client') then Exit;

  Context := DelegationContext;
  DoDelegation(Id, Context);

  if Assigned(FDelegateChat) then
    RunDelegateChat(Id, Context)
  else if not Assigned(FOnDelegation) then
  begin
    // Nadie va a contestar: avisar al modelo para que no espere en silencio
    DoError('Delegacion sin DelegateChat ni OnDelegation', 'delegation_unhandled');
    AppendCommentary('This task cannot be completed: no handler is configured.', Id);
  end;
end;

procedure TAiOpenAiLiveChat.RunDelegateChat(const ADelegationId, AContext: string);
begin
  StartWorker(
    procedure
    var
      Answer: string;
    begin
      FDelegateLock.Enter;
      try
        try
          Answer := FDelegateChat.AddMessageAndRun(
            Format(CDELEGATE_PROMPT, [AContext]), 'user', []);
          if Trim(Answer) = '' then
          begin
            Answer := 'The task produced no answer.';
            if FDelegateChat.LastError <> '' then
              DoError('DelegateChat: ' + FDelegateChat.LastError, 'delegate_chat_error');
          end;
        except
          on E: Exception do
          begin
            DoError('DelegateChat: ' + E.Message, 'delegate_chat_error');
            Answer := 'The task failed: ' + E.Message;
          end;
        end;
      finally
        FDelegateLock.Leave;
      end;
      if not FShuttingDown then
        AppendCommentary(Answer, ADelegationId);
    end);
end;

// -----------------------------------------------------------------------------
// Delegacion a Responses (ldResponses): function calling
// -----------------------------------------------------------------------------

procedure TAiOpenAiLiveChat.HandleResponseEvent(const JObj: TJSONObject);
var
  JInner, JItem, JResp, JErr: TJSONObject;
  Key, IType, ItemType, Msg: string;
  Turn: TAiLiveToolTurn;
begin
  if not JObj.TryGetValue<TJSONObject>('event', JInner) then Exit;
  Key := '';
  JObj.TryGetValue<string>('delegation_id', Key);
  IType := '';
  JInner.TryGetValue<string>('type', IType);

  if Assigned(FOnResponseEvent) then
    DoResponseEvent(Key, IType, JInner.ToJSON);

  if IType = 'response.output_item.done' then
  begin
    if JInner.TryGetValue<TJSONObject>('item', JItem) then
    begin
      ItemType := '';
      JItem.TryGetValue<string>('type', ItemType);
      if ItemType = 'function_call' then
        HandleFunctionCall(Key, JItem);
    end;
  end

  else if (IType = 'response.completed') or (IType = 'response.incomplete') then
  begin
    FToolLock.Enter;
    try
      if FToolTurns.TryGetValue(Key, Turn) then
      begin
        Turn.Completed := True;
        FToolTurns[Key] := Turn;
      end;
    finally
      FToolLock.Leave;
    end;
    TryContinueAfterTools(Key);
  end

  else if IType = 'response.failed' then
  begin
    Msg := 'La respuesta delegada fallo';
    if JInner.TryGetValue<TJSONObject>('response', JResp) and
       JResp.TryGetValue<TJSONObject>('error', JErr) then
      JErr.TryGetValue<string>('message', Msg);
    FToolLock.Enter;
    try
      FToolTurns.Remove(Key);
    finally
      FToolLock.Leave;
    end;
    DoError(Msg, 'delegation_failed');
  end;
end;

procedure TAiOpenAiLiveChat.HandleFunctionCall(const AKey: string; const AItem: TJSONObject);
var
  FuncName, CallId, Args: string;
  Turn: TAiLiveToolTurn;
begin
  FuncName := '';
  CallId := '';
  Args := '';
  AItem.TryGetValue<string>('name', FuncName);
  AItem.TryGetValue<string>('call_id', CallId);
  AItem.TryGetValue<string>('arguments', Args);
  if (FuncName = '') or (CallId = '') then Exit;

  FToolLock.Enter;
  try
    if not FToolTurns.TryGetValue(AKey, Turn) then
      Turn := Default(TAiLiveToolTurn);
    Inc(Turn.Pending);
    Turn.HadCalls := True;
    FToolTurns.AddOrSetValue(AKey, Turn);
  finally
    FToolLock.Leave;
  end;

  StartWorker(
    procedure
    var
      ToolCall: TAiToolsFunction;
      Handled: Boolean;
      T: TAiLiveToolTurn;
    begin
      ToolCall := TAiToolsFunction.Create;
      try
        ToolCall.id := CallId;
        ToolCall.name := FuncName;
        ToolCall.Arguments := Args;
        Handled := False;
        try
          if Assigned(FAiFunctions) then
            Handled := FAiFunctions.DoCallFunction(ToolCall);
          if (not Handled) and Assigned(FOnCallToolFunction) then
          begin
            // El fallback corre en el hilo principal (puede tocar la UI)
            TThread.Synchronize(nil,
              procedure
              begin
                FOnCallToolFunction(Self, ToolCall);
              end);
            Handled := True;
          end;
          if not Handled then
            ToolCall.Response := 'Error: no handler for function ' + FuncName;
        except
          on E: Exception do
            ToolCall.Response := 'Error running ' + FuncName + ': ' + E.Message;
        end;
        if not FShuttingDown then
          SendFunctionOutput(CallId, ToolCall.Response);
      finally
        ToolCall.Free;
      end;
      FToolLock.Enter;
      try
        if FToolTurns.TryGetValue(AKey, T) then
        begin
          Dec(T.Pending);
          FToolTurns[AKey] := T;
        end;
      finally
        FToolLock.Leave;
      end;
      TryContinueAfterTools(AKey);
    end);
end;

procedure TAiOpenAiLiveChat.TryContinueAfterTools(const AKey: string);
var
  Turn: TAiLiveToolTurn;
  Ready: Boolean;
  J: TJSONObject;
begin
  // Una sola continuacion por respuesta: cuando termino la respuesta que pidio
  // las funciones y ya se enviaron todos los resultados
  FToolLock.Enter;
  try
    Ready := FToolTurns.TryGetValue(AKey, Turn) and Turn.HadCalls and
      Turn.Completed and (Turn.Pending <= 0);
    if Ready then
      FToolTurns.Remove(AKey);
  finally
    FToolLock.Leave;
  end;
  if (not Ready) or FShuttingDown then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'response.create');
  J.AddPair('event_id', NextEventId);
  SendJson(J);
end;

procedure TAiOpenAiLiveChat.SendFunctionOutput(const ACallId, AOutput: string);
var
  J, JItem: TJSONObject;
begin
  JItem := TJSONObject.Create;
  JItem.AddPair('type', 'function_call_output');
  JItem.AddPair('call_id', ACallId);
  JItem.AddPair('output', AOutput);
  J := TJSONObject.Create;
  J.AddPair('type', 'response.item.create');
  J.AddPair('event_id', NextEventId);
  J.AddPair('item', JItem);
  SendJson(J);
end;

procedure TAiOpenAiLiveChat.StartWorker(AProc: TProc);
begin
  TInterlocked.Increment(FActiveWorkers);
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        AProc();
      finally
        TInterlocked.Decrement(FActiveWorkers);
      end;
    end).Start;
end;

// -----------------------------------------------------------------------------
// Inyecciones de contexto
// -----------------------------------------------------------------------------

class function TAiOpenAiLiveChat.SplitForAppend(const AText: string;
  AMaxBytes: Integer): TArray<string>;
var
  Parts: TList<string>;
  Rest, Chunk: string;
  I, Bytes, CharBytes, Cut, LastSpace: Integer;
begin
  Parts := TList<string>.Create;
  try
    Rest := Trim(AText);
    while Rest <> '' do
    begin
      // Avanzar caracter a caracter sin partir pares sustitutos
      Bytes := 0;
      Cut := 0;
      LastSpace := 0;
      I := 1;
      while I <= Length(Rest) do
      begin
        if IsLeadChar(Rest[I]) and (I < Length(Rest)) then
          CharBytes := TEncoding.UTF8.GetByteCount(Copy(Rest, I, 2))
        else
          CharBytes := TEncoding.UTF8.GetByteCount(Rest[I]);
        if Bytes + CharBytes > AMaxBytes then
          Break;
        Inc(Bytes, CharBytes);
        if IsLeadChar(Rest[I]) and (I < Length(Rest)) then
          Inc(I, 2)
        else
          Inc(I);
        Cut := I - 1;
        if Rest[Cut] = ' ' then
          LastSpace := Cut;
      end;
      if Cut >= Length(Rest) then
      begin
        Parts.Add(Rest);
        Break;
      end;
      if Cut = 0 then
        Cut := 1; // AMaxBytes menor que un caracter: avanzar igual
      // Preferir cortar en un espacio de la segunda mitad del fragmento
      if LastSpace > Cut div 2 then
        Cut := LastSpace;
      Chunk := Trim(Copy(Rest, 1, Cut));
      if Chunk <> '' then
        Parts.Add(Chunk);
      Rest := Trim(Copy(Rest, Cut + 1, MaxInt));
    end;
    Result := Parts.ToArray;
  finally
    Parts.Free;
  end;
end;

procedure TAiOpenAiLiveChat.SendAppend(const AType, AContent, ADelegationId: string);
var
  J: TJSONObject;
  Part: string;
begin
  for Part in SplitForAppend(AContent) do
  begin
    J := TJSONObject.Create;
    J.AddPair('type', AType);
    J.AddPair('event_id', NextEventId);
    J.AddPair('content', Part);
    // Campo requerido aunque sea null (contexto general). Con delegacion a
    // Responses el servidor solo acepta null
    if (ADelegationId <> '') and (EffectiveDelegation = ldClient) then
      J.AddPair('delegation_id', ADelegationId)
    else
      J.AddPair('delegation_id', TJSONNull.Create);
    SendJson(J);
  end;
end;

procedure TAiOpenAiLiveChat.AppendCommentary(const AText, ADelegationId: string);
begin
  SendAppend('session.commentary.append', AText, ADelegationId);
end;

procedure TAiOpenAiLiveChat.AppendThinking(const AText, ADelegationId: string);
begin
  SendAppend('session.thinking.append', AText, ADelegationId);
end;

procedure TAiOpenAiLiveChat.AppendInstructions(const AText, ADelegationId: string);
begin
  SendAppend('session.instructions.append', AText, ADelegationId);
end;

procedure TAiOpenAiLiveChat.Mute;
var
  J: TJSONObject;
begin
  if not IsConnected then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'session.input_audio.mute');
  J.AddPair('event_id', NextEventId);
  SendJson(J);
end;

procedure TAiOpenAiLiveChat.Unmute;
var
  J: TJSONObject;
begin
  if not IsConnected then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'session.input_audio.unmute');
  J.AddPair('event_id', NextEventId);
  SendJson(J);
end;

// -----------------------------------------------------------------------------
// Dispatchers de los eventos propios
// -----------------------------------------------------------------------------

procedure TAiOpenAiLiveChat.DoDelegation(const ADelegationId, AContext: string);
begin
  if not Assigned(FOnDelegation) then Exit;
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnDelegation) then
        FOnDelegation(Self, ADelegationId, AContext);
    end);
end;

procedure TAiOpenAiLiveChat.DoUsage(ASeconds, ARatio: Double);
begin
  if not Assigned(FOnUsage) then Exit;
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnUsage) then
        FOnUsage(Self, ASeconds, ARatio);
    end);
end;

procedure TAiOpenAiLiveChat.DoSessionClosed(const AReason: string; ASeconds: Double);
begin
  if not Assigned(FOnSessionClosed) then Exit;
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, AReason, ASeconds);
    end);
end;

procedure TAiOpenAiLiveChat.DoResponseEvent(const ADelegationId, AType, AJson: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnResponseEvent) then
        FOnResponseEvent(Self, ADelegationId, AType, AJson);
    end);
end;

// -----------------------------------------------------------------------------
// WebSocket
// -----------------------------------------------------------------------------

procedure TAiOpenAiLiveChat.OnWSConnected(Sender: TObject);
begin
  DoConnected;
  // session.start va antes que cualquier otro comando
  SendJson(BuildSessionStart);
end;

procedure TAiOpenAiLiveChat.OnWSDisconnected(Sender: TObject);
begin
  FSessionStarted := False;
  FClosedEvent.SetEvent; // no esperar un session.closed que ya no llegara
  DoDisconnected;
end;

procedure TAiOpenAiLiveChat.OnWSError(Sender: TObject; const ErrorMsg: string);
begin
  DoError(ErrorMsg, 'websocket_error');
end;

procedure TAiOpenAiLiveChat.OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
  const Data: TBytes; IsFinal: Boolean);
var
  V: TJSONValue;
begin
  if Opcode <> rwsoText then Exit;
  V := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetString(Data));
  try
    if V is TJSONObject then
      ProcessServerEvent(TJSONObject(V));
  finally
    V.Free;
  end;
end;

procedure TAiOpenAiLiveChat.SendAudioChunk(const PCM16Data: TBytes);
var
  LData: TBytes;
begin
  if not IsConnected then Exit;
  if Length(PCM16Data) = 0 then Exit;
  LData := Copy(PCM16Data);
  TThread.Queue(nil,
    procedure
    var
      Resampled: TBytes;
    begin
      if not IsConnected then Exit;
      if InputSampleRate <> GetTargetSampleRate then
        Resampled := ResamplePCM16(LData, InputSampleRate, GetTargetSampleRate)
      else
        Resampled := LData;
      InternalSendAudio(Resampled);
    end);
end;

procedure TAiOpenAiLiveChat.InternalConnect;
begin
  // TAiRealtimeConnection copia su ApiKey/Model aunque esten vacios
  if ApiKey = '' then
    ApiKey := '@OPENAI_API_KEY';
  if Model = '' then
    Model := GetDefaultModel;
  // La delegacion se resuelve en un hilo con una llamada bloqueante
  if Assigned(FDelegateChat) then
    FDelegateChat.Params.Values['Asynchronous'] := 'False';

  FSessionStarted := False;
  FSessionId := '';
  FCloseReason := '';
  FUsageSeconds := 0;
  FPrev := Default(TAiLiveTurn);
  FCur := Default(TAiLiveTurn);
  FBuf := Default(TAiLiveTurn);
  FTimelineMax := 0;
  FVoiceWall := 0;
  FTranscript.Clear;
  FDelegatedLines := 0;
  FToolLock.Enter;
  try
    FToolTurns.Clear;
  finally
    FToolLock.Leave;
  end;
  FClosedEvent.ResetEvent;

  FWebSocket.ExtraHeaders.Values['Authorization'] := 'Bearer ' + ResolvedApiKey;
  FConnectThread := TThread.CreateAnonymousThread(
    procedure
    begin
      if not FWebSocket.Connect(CLIVE_WSS) then
        DoError('No se pudo conectar a GPT-Live', 'connection_failed');
    end);
  FConnectThread.FreeOnTerminate := False;
  FConnectThread.Start;
end;

procedure TAiOpenAiLiveChat.InternalDisconnect;
var
  J: TJSONObject;
  SW: TStopwatch;
begin
  // Cierre ordenado: session.closed trae el motivo y el consumo final
  if FSessionStarted and (FCloseTimeoutMs > 0) then
  begin
    FClosedEvent.ResetEvent;
    J := TJSONObject.Create;
    J.AddPair('type', 'session.close');
    J.AddPair('event_id', NextEventId);
    try
      SendJson(J);
      // Los frames se procesan en el hilo principal (TThread.Queue): si se
      // espera ahi sin drenar la cola, session.closed no llega nunca
      SW := TStopwatch.StartNew;
      while (FClosedEvent.WaitFor(0) <> wrSignaled) and
            (SW.ElapsedMilliseconds < FCloseTimeoutMs) do
      begin
        if TThread.CurrentThread.ThreadID = MainThreadID then
          CheckSynchronize(10)
        else
          FClosedEvent.WaitFor(10);
      end;
    except
      // El socket ya no esta: cerrar igual
    end;
  end;
  FWebSocket.SendClose(1000, '');
  FWebSocket.Disconnect;
  if Assigned(FConnectThread) then
  begin
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FSessionStarted := False;
  Connected := False;
end;

procedure TAiOpenAiLiveChat.InternalSendAudio(const ResampledPCM16: TBytes);
var
  J: TJSONObject;
begin
  // Antes de session.started el servidor no acepta comandos
  if (Length(ResampledPCM16) = 0) or (not FSessionStarted) then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'session.input_audio.append');
  J.AddPair('audio', FB64.EncodeBytesToString(ResampledPCM16));
  SendJson(J);
end;

procedure TAiOpenAiLiveChat.InternalCommitAudio;
begin
  // Flujo continuo: no aplica
end;

procedure TAiOpenAiLiveChat.InternalClearAudio;
begin
  // Flujo continuo: no aplica
end;

initialization
  TAiRealtimeFactory.Instance.RegisterDriver(
    TAiOpenAiLiveChat.GetDriverName, TAiOpenAiLiveChat);

end.
