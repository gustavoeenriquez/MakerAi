// MakerAI Suite — Driver xAI Grok Voice (speech-to-speech full-duplex)
// wss://api.x.ai/v1/realtime  (RFC 6455 + protocolo compatible OpenAI Realtime)
//
// Conversacion de voz completa en un solo WebSocket:
//   1. STT — el servidor transcribe el audio del usuario
//   2. LLM — genera la respuesta del asistente (razonamiento opcional)
//   3. TTS — devuelve la respuesta como audio PCM16 a 24 kHz
//
// Modelos: grok-voice-think-fast-2.0 (default), grok-voice-think-fast-1.0,
//          grok-voice-latest (alias movil — preferir version pineada)
// Audio:   PCM16, 24 kHz, mono, little-endian (base64 sobre JSON)
// ApiKey:  convencion @XAI_API_KEY
//
// Diferencias vs OpenAI Realtime que maneja este driver:
//   - La transcripcion del usuario llega ACUMULADA en
//     conversation.item.input_audio_transcription.updated (no .delta);
//     el driver calcula el delta por diferencia.
//   - No se envia Sec-WebSocket-Protocol: Grok reserva el subprotocolo
//     para tokens efimeros (xai-client-secret.*).
//   - language_hint requiere variante regional para es/pt (es-MX, pt-BR).
//   - Sin semantic_vad ni noise_reduction; VAD default threshold 0.85.
//
// Function calling: asignar AiFunctions (TAiFunctions, funciones locales +
// MCP) o el evento OnCallToolFunction. El driver declara las tools en
// session.update, ejecuta los calls en hilos propios y envia los outputs +
// response.create automaticamente. Tools nativas de xAI (las ejecuta el
// servidor): EnableWebSearch / EnableXSearch.
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.Grok;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  System.IOUtils, System.Math, System.SyncObjs, System.Net.HttpClient,
  uMakerAi.Core, uMakerAi.Chat.Messages, uMakerAi.Tools.Functions,
  uMakerAi.Realtime, uMakerAi.Realtime.WebSocket, uMakerAi.WebSocket.Client;

type
  // Nivel de razonamiento del modelo mientras habla
  TAiGrokReasoningEffort = (
    greHigh, // razonamiento completo (default del servidor)
    greNone  // sin razonamiento — menor latencia
  );

  // Fallback de function calling cuando AiFunctions no maneja la funcion.
  // El handler debe asignar ToolCall.Response con el resultado.
  TAiGrokToolCallEvent = procedure(Sender: TObject;
    ToolCall: TAiToolsFunction) of object;

  TAiGrokRealtimeChat = class(TAiRealtimeVoiceBase)
  private
    FWebSocket:         TAiRealtimeWSClient;
    FConnectThread:     TThread; // hilo de conexion — esperado antes de liberar FWebSocket
    FSessionConfigured: Boolean;
    // Propiedades especificas del driver Grok
    FVoice:           string;
    FInstructions:    string;
    FReasoningEffort: TAiGrokReasoningEffort;
    FIdleTimeoutMs:   Integer;
    FOutputSpeed:     Double;   // 0.7..1.5; 0 = default del servidor
    FKeyterms:        TStrings; // sesgo de transcripcion (max 100 terminos)
    FPronunciationReplace: TStrings; // Frase=ComoSePronuncia para el TTS
    // Fase 3: resumption, transporte binario, tokens efimeros, file_search
    FEnableResumption: Boolean;
    FConversationId:   string;  // asignado por el servidor (conversation.created)
    FBinaryAudio:      Boolean; // PCM crudo en frames binarios (in + out)
    FEphemeralToken:   string;  // auth client-side via subprotocolo WS
    FFileSearchCollections: TStrings; // vector_store_ids para file_search
    FFileSearchMaxResults:  Integer;
    FCustomToolsJson:  TStrings; // tools extra en JSON crudo (mcp remoto, etc.)
    // Function calling / tools
    FAiFunctions:        TAiFunctions; // referencia externa (FreeNotification)
    FOnCallToolFunction: TAiGrokToolCallEvent;
    FEnableWebSearch:    Boolean;
    FEnableXSearch:      Boolean;
    FPendingToolCalls:   Integer; // tool calls en ejecucion (TInterlocked)
    FHadToolCalls:       Boolean; // el response actual contiene function calls
    FToolsDone:          Boolean; // llego response.done del response con tools
    FContinuationSent:   Integer; // guard 0/1 para un unico response.create
    // Estado para convertir transcripcion acumulada -> deltas
    FLastItemId:     string;
    FLastTranscript: string;
    // Acumulador del texto del asistente en el turno actual
    FAssistantText:  string;
    // Texto hablado antes de un tool call — se conserva para que el
    // OnAssistantText final incluya el turno completo (pre + post tools)
    FCarryAssistantText: string;
    procedure OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
      const Data: TBytes; IsFinal: Boolean);
    procedure OnWSConnected(Sender: TObject);
    procedure OnWSDisconnected(Sender: TObject);
    procedure OnWSError(Sender: TObject; const ErrorMsg: string);
    procedure ProcessServerEvent(const JObj: TJSONObject);
    procedure SendSessionUpdate;
    function  VADObject: TJSONObject;
    function  BuildWssUrl: string;
    function  LanguageHint: string;
    procedure HandleTranscriptionUpdated(const JObj: TJSONObject);
    // Function calling
    procedure SetAiFunctions(const Value: TAiFunctions);
    procedure SetKeyterms(const Value: TStrings);
    procedure SetPronunciationReplace(const Value: TStrings);
    procedure SetFileSearchCollections(const Value: TStrings);
    procedure SetCustomToolsJson(const Value: TStrings);
    function  BuildToolsArray: TJSONArray;
    procedure HandleFunctionCall(const JObj: TJSONObject);
    procedure SendFunctionOutput(const ACallId, AOutput: string);
    procedure TryContinueAfterTools;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetTargetSampleRate: Integer; override;
    procedure InternalSendAudio(const ResampledPCM16: TBytes); override;
    procedure InternalConnect;    override;
    procedure InternalDisconnect; override;
    procedure InternalCommitAudio; override;
    procedure InternalClearAudio;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    // Despacha el envio al hilo principal: el callback de WaveIn no puede
    // llamar a SChannel/sockets directamente (misma razon que el driver MakerAI)
    procedure SendAudioChunk(const PCM16Data: TBytes); override;
    // Solicita explicitamente una respuesta del modelo (response.create).
    // Necesario en VADMode = rvmManual despues de CommitAudio; con server_vad
    // el servidor la dispara solo al detectar fin de turno.
    procedure CreateResponse;
    // Frase TTS forzada sin pasar por el modelo (avisos IVR, disclosures).
    // AInterruptible=False descarta el audio del usuario hasta terminar.
    procedure ForceMessage(const AText: string; AInterruptible: Boolean = True);
    // Mintea un token efimero via POST /v1/realtime/client_secrets usando la
    // API key (acepta la convencion @VAR_NAME). Pensado para backends que
    // entregan el token a clientes moviles/browser; asignarlo en
    // EphemeralToken. Lanza excepcion si el servidor rechaza la peticion.
    class function MintEphemeralToken(const AApiKey: string;
      AExpiresSeconds: Integer = 300): string;
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
    // Id de conversacion asignado por el servidor (conversation.created).
    // Persistirlo y reasignarlo antes de Connect (con EnableResumption=True)
    // reanuda la conversacion con replay de turnos (expira a los 30 min).
    property ConversationId: string read FConversationId write FConversationId;
  published
    // Voz TTS: eva, ara, rex, sal, leo... o voice_id de Custom Voices.
    // Vacio = default del servidor
    property Voice: string read FVoice write FVoice;
    // System prompt de la sesion
    property Instructions: string read FInstructions write FInstructions;
    // Razonamiento mientras habla (greNone reduce latencia)
    property ReasoningEffort: TAiGrokReasoningEffort
      read FReasoningEffort write FReasoningEffort default greHigh;
    // Si el usuario no habla en este lapso el modelo re-engancha (0 = omitir)
    property IdleTimeoutMs: Integer read FIdleTimeoutMs write FIdleTimeoutMs
      default 0;
    // Velocidad de reproduccion del TTS (0.7..1.5; 0 = default del servidor)
    property OutputSpeed: Double read FOutputSpeed write FOutputSpeed;
    // Componente de funciones (locales + MCP); se declaran en session.update
    // y el driver ejecuta los tool calls automaticamente
    property AiFunctions: TAiFunctions read FAiFunctions write SetAiFunctions;
    // Fallback cuando AiFunctions no maneja la funcion (o no esta asignado)
    property OnCallToolFunction: TAiGrokToolCallEvent
      read FOnCallToolFunction write FOnCallToolFunction;
    // Tools nativas de xAI (las ejecuta el servidor)
    property EnableWebSearch: Boolean read FEnableWebSearch
      write FEnableWebSearch default False;
    property EnableXSearch: Boolean read FEnableXSearch
      write FEnableXSearch default False;
    // Terminos de dominio para sesgar la transcripcion (max 100)
    property Keyterms: TStrings read FKeyterms write SetKeyterms;
    // Correcciones de pronunciacion TTS, formato Frase=ComoSePronuncia
    // (match case-insensitive por palabra completa)
    property PronunciationReplace: TStrings read FPronunciationReplace
      write SetPronunciationReplace;
    // Reanudacion de sesion: el servidor cachea los turnos y los reproduce
    // al reconectar con el mismo ConversationId (ambas sesiones deben
    // habilitarla; el historial expira a los 30 min de inactividad)
    property EnableResumption: Boolean read FEnableResumption
      write FEnableResumption default False;
    // Audio PCM crudo en frames binarios WebSocket (entrada y salida):
    // ~33% menos ancho de banda que base64. Los eventos JSON siguen en texto
    property BinaryAudio: Boolean read FBinaryAudio
      write FBinaryAudio default False;
    // Token efimero (con o sin prefijo xai-client-secret.). Si esta asignado
    // se autentica via subprotocolo WebSocket y NO se envia la ApiKey —
    // pensado para clientes moviles/browser. Ver MintEphemeralToken
    property EphemeralToken: string read FEphemeralToken write FEphemeralToken;
    // Colecciones (vector_store_ids) para la tool nativa file_search
    property FileSearchCollections: TStrings read FFileSearchCollections
      write SetFileSearchCollections;
    property FileSearchMaxResults: Integer read FFileSearchMaxResults
      write FFileSearchMaxResults default 0;
    // Tools adicionales en JSON crudo (objeto o array de objetos), p.ej.
    // {"type":"mcp","server_url":"https://...","server_label":"mis-tools"}
    property CustomToolsJson: TStrings read FCustomToolsJson
      write SetCustomToolsJson;
  end;

  procedure Register;

implementation

const
  CGROKREALTIME_WSS = 'wss://api.x.ai/v1/realtime';
  CGROKREALTIME_LOG = ''; // ruta de log diagnostico; '' para deshabilitar

procedure GkLog(const AMsg: string);
begin
  if CGROKREALTIME_LOG = '' then Exit;
  try
    TFile.AppendAllText(CGROKREALTIME_LOG,
      FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMsg + sLineBreak);
  except end;
end;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGrokRealtimeChat]);
end;

{ TAiGrokRealtimeChat }

constructor TAiGrokRealtimeChat.Create(AOwner: TComponent);
begin
  inherited;
  FSessionConfigured := False;
  FConnectThread     := nil;
  FReasoningEffort   := greHigh;
  FIdleTimeoutMs     := 0;
  FOutputSpeed       := 0;
  FKeyterms             := TStringList.Create;
  FPronunciationReplace := TStringList.Create;
  FFileSearchCollections := TStringList.Create;
  FCustomToolsJson       := TStringList.Create;
  FFileSearchMaxResults  := 0;
  // El default de VAD de Grok es 0.85; la base inicializa 0.5 (OpenAI)
  VADThreshold       := 0.85;
  FWebSocket         := TAiRealtimeWSClient.Create;
  FWebSocket.OnFrame        := OnWSFrame;
  FWebSocket.OnConnected    := OnWSConnected;
  FWebSocket.OnDisconnected := OnWSDisconnected;
  FWebSocket.OnError        := OnWSError;
end;

destructor TAiGrokRealtimeChat.Destroy;
begin
  if IsConnected then InternalDisconnect;
  // Abortar conexion pendiente aunque todavia no este conectado
  if Assigned(FConnectThread) then
  begin
    FWebSocket.Disconnect;
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FWebSocket.Free;
  FKeyterms.Free;
  FPronunciationReplace.Free;
  FFileSearchCollections.Free;
  FCustomToolsJson.Free;
  inherited;
end;

procedure TAiGrokRealtimeChat.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FAiFunctions) then
    FAiFunctions := nil;
end;

procedure TAiGrokRealtimeChat.SetAiFunctions(const Value: TAiFunctions);
begin
  if FAiFunctions = Value then Exit;
  FAiFunctions := Value;
  if Assigned(FAiFunctions) then
    FAiFunctions.FreeNotification(Self);
end;

procedure TAiGrokRealtimeChat.SetKeyterms(const Value: TStrings);
begin
  FKeyterms.Assign(Value);
end;

procedure TAiGrokRealtimeChat.SetPronunciationReplace(const Value: TStrings);
begin
  FPronunciationReplace.Assign(Value);
end;

procedure TAiGrokRealtimeChat.SetFileSearchCollections(const Value: TStrings);
begin
  FFileSearchCollections.Assign(Value);
end;

procedure TAiGrokRealtimeChat.SetCustomToolsJson(const Value: TStrings);
begin
  FCustomToolsJson.Assign(Value);
end;

class function TAiGrokRealtimeChat.MintEphemeralToken(const AApiKey: string;
  AExpiresSeconds: Integer): string;
var
  Http: THTTPClient;
  Body: TStringStream;
  Resp: IHTTPResponse;
  JObj: TJSONObject;
  Key, Content: string;
begin
  Result := '';
  Key := AApiKey;
  if (Length(Key) > 1) and (Key[1] = '@') then
    Key := GetEnvironmentVariable(Copy(Key, 2, MaxInt));
  Http := THTTPClient.Create;
  Body := TStringStream.Create(
    '{"expires_after":{"seconds":' + IntToStr(AExpiresSeconds) + '}}',
    TEncoding.UTF8);
  try
    Http.CustomHeaders['Authorization'] := 'Bearer ' + Key;
    Http.ContentType := 'application/json';
    Resp := Http.Post('https://api.x.ai/v1/realtime/client_secrets', Body);
    Content := Resp.ContentAsString(TEncoding.UTF8);
    if (Resp.StatusCode < 200) or (Resp.StatusCode >= 300) then
      raise Exception.CreateFmt(
        'MintEphemeralToken: HTTP %d — %s', [Resp.StatusCode, Content]);
    JObj := TJSONObject.ParseJSONValue(Content) as TJSONObject;
    if not Assigned(JObj) then
      raise Exception.Create('MintEphemeralToken: respuesta no es JSON valido');
    try
      // El esquema de la respuesta no esta documentado: buscar el token en
      // las ubicaciones tipicas de APIs OpenAI-compatibles
      if not JObj.TryGetValue<string>('client_secret.value', Result) then
        if not JObj.TryGetValue<string>('value', Result) then
          if not JObj.TryGetValue<string>('client_secret', Result) then
            if not JObj.TryGetValue<string>('token', Result) then
              raise Exception.Create(
                'MintEphemeralToken: no se encontro el token en la respuesta: ' +
                Copy(Content, 1, 300));
    finally
      JObj.Free;
    end;
  finally
    Body.Free;
    Http.Free;
  end;
end;

class function TAiGrokRealtimeChat.GetDriverName: string;
begin
  Result := 'Grok';
end;

class function TAiGrokRealtimeChat.GetDefaultModel: string;
begin
  // Version pineada para comportamiento estable entre releases
  Result := 'grok-voice-think-fast-2.0';
end;

function TAiGrokRealtimeChat.GetTargetSampleRate: Integer;
begin
  Result := 24000; // audio/pcm default de Grok Voice
end;

function TAiGrokRealtimeChat.BuildWssUrl: string;
var
  M: string;
begin
  M := Model;
  if M = '' then M := GetDefaultModel;
  Result := CGROKREALTIME_WSS + '?model=' + M;
  // Reanudar una conversacion previa (replay de turnos cacheados)
  if FEnableResumption and (FConversationId <> '') then
    Result := Result + '&conversation_id=' + FConversationId;
end;

// Grok exige variante regional BCP-47 para espanol y portugues
// (es-MX, no 'es'; pt-BR, no 'pt'). Se corrigen los dos casos documentados.
function TAiGrokRealtimeChat.LanguageHint: string;
begin
  Result := Language;
  if SameText(Result, 'es') then Result := 'es-MX'
  else if SameText(Result, 'pt') then Result := 'pt-BR';
end;

function TAiGrokRealtimeChat.VADObject: TJSONObject;
begin
  if VADMode = rvmManual then
  begin
    Result := nil;
    Exit;
  end;
  // Grok solo soporta server_vad; rvmSemanticVad se mapea a server_vad
  Result := TJSONObject.Create;
  Result.AddPair('type', 'server_vad');
  Result.AddPair('threshold',           TJSONNumber.Create(VADThreshold));
  Result.AddPair('prefix_padding_ms',   TJSONNumber.Create(PrefixPaddingMs));
  Result.AddPair('silence_duration_ms', TJSONNumber.Create(SilenceDurationMs));
  if FIdleTimeoutMs > 0 then
    Result.AddPair('idle_timeout_ms',   TJSONNumber.Create(FIdleTimeoutMs));
end;

procedure TAiGrokRealtimeChat.SendSessionUpdate;
// Esquema Grok: voice/instructions/turn_detection/reasoning al nivel de
// session; formatos y transcripcion anidados bajo session.audio.input/output.
var
  JMsg, JSession, JAudio, JInput, JOutput, JFormat, JOutFormat: TJSONObject;
  JTranscription, JVAD, JReasoning, JReplace: TJSONObject;
  JTools, JKeyterms: TJSONArray;
  I: Integer;
begin
  JMsg     := TJSONObject.Create;
  JSession := TJSONObject.Create;
  JAudio   := TJSONObject.Create;
  JInput   := TJSONObject.Create;
  JOutput  := TJSONObject.Create;

  JMsg.AddPair('type', 'session.update');

  if FVoice <> '' then
    JSession.AddPair('voice', FVoice);
  if FInstructions <> '' then
    JSession.AddPair('instructions', FInstructions);

  // VAD / turn detection
  JVAD := VADObject;
  if Assigned(JVAD) then
    JSession.AddPair('turn_detection', JVAD)
  else
    JSession.AddPair('turn_detection', TJSONNull.Create);

  // Razonamiento mientras habla
  JReasoning := TJSONObject.Create;
  if FReasoningEffort = greNone then
    JReasoning.AddPair('effort', 'none')
  else
    JReasoning.AddPair('effort', 'high');
  JSession.AddPair('reasoning', JReasoning);

  // Tools: funciones locales/MCP (AiFunctions) + tools nativas de xAI
  JTools := BuildToolsArray;
  if Assigned(JTools) then
    JSession.AddPair('tools', JTools);

  // Correcciones de pronunciacion del TTS (Frase=ComoSePronuncia)
  if FPronunciationReplace.Count > 0 then
  begin
    JReplace := TJSONObject.Create;
    for I := 0 to FPronunciationReplace.Count - 1 do
      if FPronunciationReplace.Names[I] <> '' then
        JReplace.AddPair(FPronunciationReplace.Names[I],
                         FPronunciationReplace.ValueFromIndex[I]);
    JSession.AddPair('replace', JReplace);
  end;

  // Reanudacion de sesion (debe habilitarse tambien al reconectar)
  if FEnableResumption then
  begin
    JReplace := TJSONObject.Create; // reutiliza la variable local
    JReplace.AddPair('enabled', TJSONBool.Create(True));
    JSession.AddPair('resumption', JReplace);
  end;

  // Formato de entrada: PCM16 a 24 kHz
  JFormat := TJSONObject.Create;
  JFormat.AddPair('type', 'audio/pcm');
  JFormat.AddPair('rate', TJSONNumber.Create(24000));
  JInput.AddPair('format', JFormat);
  if FBinaryAudio then
    JInput.AddPair('transport', 'binary');

  // Sesgo de transcripcion: idioma y terminos de dominio (opcionales)
  if (Language <> '') or (FKeyterms.Count > 0) then
  begin
    JTranscription := TJSONObject.Create;
    if Language <> '' then
      JTranscription.AddPair('language_hint', LanguageHint);
    if FKeyterms.Count > 0 then
    begin
      JKeyterms := TJSONArray.Create;
      for I := 0 to Min(FKeyterms.Count, 100) - 1 do
        JKeyterms.Add(FKeyterms[I]);
      JTranscription.AddPair('keyterms', JKeyterms);
    end;
    JInput.AddPair('transcription', JTranscription);
  end;

  // Formato de salida: PCM16 a 24 kHz
  JOutFormat := TJSONObject.Create;
  JOutFormat.AddPair('type', 'audio/pcm');
  JOutFormat.AddPair('rate', TJSONNumber.Create(24000));
  JOutput.AddPair('format', JOutFormat);
  if FBinaryAudio then
    JOutput.AddPair('transport', 'binary');
  // Velocidad de reproduccion (solo si el usuario la configuro en rango valido)
  if (FOutputSpeed >= 0.7) and (FOutputSpeed <= 1.5) then
    JOutput.AddPair('speed', TJSONNumber.Create(FOutputSpeed));

  JAudio.AddPair('input',  JInput);
  JAudio.AddPair('output', JOutput);
  JSession.AddPair('audio', JAudio);
  JMsg.AddPair('session', JSession);
  try
    FWebSocket.SendText(JMsg.ToJSON);
  finally
    JMsg.Free;
  end;
end;

// Transcripcion del usuario: Grok envia el texto ACUMULADO del item en
// cada evento .updated. Se calcula el delta contra lo ya recibido para
// mantener la semantica de OnTranscriptDelta del resto de drivers.
procedure TAiGrokRealtimeChat.HandleTranscriptionUpdated(const JObj: TJSONObject);
var
  ItemId, Transcript, Delta: string;
begin
  Transcript := '';
  ItemId     := '';
  if not JObj.TryGetValue<string>('transcript', Transcript) then
    JObj.TryGetValue<string>('delta', Transcript); // tolerancia al esquema
  JObj.TryGetValue<string>('item_id', ItemId);
  if Transcript = '' then Exit;

  if ItemId <> FLastItemId then
  begin
    // Nuevo item de audio: reiniciar acumulado
    FLastItemId     := ItemId;
    FLastTranscript := '';
  end;

  if Transcript.StartsWith(FLastTranscript) then
    Delta := Copy(Transcript, Length(FLastTranscript) + 1, MaxInt)
  else
    Delta := Transcript; // el servidor reescribio el texto: emitir completo

  FLastTranscript := Transcript;
  if Delta <> '' then
    DoTranscriptDelta(Delta);
end;

procedure TAiGrokRealtimeChat.ProcessServerEvent(const JObj: TJSONObject);
var
  EventType:  string;
  ItemId:     string;
  AudioMs:    Int64;
  Transcript: string;
  Delta:      string;
  Status:     string;
  AudioB64:   string;
  ErrMsg:     string;
  ErrCode:    string;
  JError:     TJSONObject;
begin
  if not JObj.TryGetValue<string>('type', EventType) then Exit;

  if EventType = 'session.created' then
  begin
    // El servidor confirmo la sesion — enviamos nuestra configuracion
    FSessionConfigured := False;
    SendSessionUpdate;
  end

  else if EventType = 'conversation.created' then
  begin
    // Guardar el id para reanudacion (?conversation_id= al reconectar)
    JObj.TryGetValue<string>('conversation.id', FConversationId);
    GkLog('CONVERSATION_ID=' + FConversationId);
  end

  else if (EventType = 'conversation.item.created') or
          (EventType = 'conversation.item.added') then
  begin
    // Items de conversacion. Al reanudar con conversation_id, los turnos
    // cacheados (user, assistant, tool calls y outputs) llegan replicados
    // aqui — verificado: llegan como .added aunque la doc dice .created.
    // El contexto se restaura en el servidor; el cliente solo lo registra
    GkLog('CONV_ITEM ' + EventType);
  end

  else if EventType = 'session.updated' then
  begin
    FSessionConfigured := True;
    DoSessionReady;
  end

  else if EventType = 'input_audio_buffer.speech_started' then
  begin
    AudioMs := 0;
    ItemId  := '';
    JObj.TryGetValue<Int64>('audio_start_ms', AudioMs);
    JObj.TryGetValue<string>('item_id', ItemId);
    DoSpeechStarted(AudioMs, ItemId);
  end

  else if EventType = 'input_audio_buffer.speech_stopped' then
  begin
    AudioMs := 0;
    ItemId  := '';
    JObj.TryGetValue<Int64>('audio_end_ms', AudioMs);
    JObj.TryGetValue<string>('item_id', ItemId);
    DoSpeechStopped(AudioMs, ItemId);
  end

  else if EventType = 'conversation.item.input_audio_transcription.updated' then
    HandleTranscriptionUpdated(JObj)

  else if EventType = 'conversation.item.input_audio_transcription.completed' then
  begin
    // Grok emite .completed tambien para segmentos parciales: el campo
    // status distingue 'in_progress' (parcial) de 'completed' (final).
    // Solo el final dispara OnTranscriptCompleted.
    Status := '';
    JObj.TryGetValue<string>('status', Status);
    if SameText(Status, 'in_progress') then
      HandleTranscriptionUpdated(JObj)
    else
    begin
      Transcript := '';
      ItemId     := '';
      JObj.TryGetValue<string>('transcript', Transcript);
      JObj.TryGetValue<string>('item_id', ItemId);
      // Reiniciar el acumulado del item cerrado
      FLastItemId     := '';
      FLastTranscript := '';
      DoTranscriptCompleted(Transcript, ItemId);
    end;
  end

  else if EventType = 'response.created' then
  begin
    // FCarryAssistantText (texto pre-tools) se conserva; FAssistantText
    // acumula solo el response actual
    FAssistantText     := '';
    FHadToolCalls      := False;
    FToolsDone         := False;
    FPendingToolCalls  := 0;
    FContinuationSent  := 0;
  end

  else if EventType = 'response.function_call_arguments.done' then
    HandleFunctionCall(JObj)

  // Grok entrega el texto hablado en response.output_audio_transcript.*;
  // response.text.delta se acepta tambien por compatibilidad OpenAI
  else if (EventType = 'response.output_audio_transcript.delta') or
          (EventType = 'response.text.delta') then
  begin
    Delta := '';
    JObj.TryGetValue<string>('delta', Delta);
    if Delta <> '' then
    begin
      FAssistantText := FAssistantText + Delta;
      DoAssistantTextDelta(Delta);
    end;
  end

  else if EventType = 'response.output_audio_transcript.done' then
  begin
    // Trae el transcript completo autoritativo del turno del asistente
    Transcript := '';
    JObj.TryGetValue<string>('transcript', Transcript);
    if Transcript <> '' then
      FAssistantText := Transcript;
  end

  else if EventType = 'response.output_audio.delta' then
  begin
    AudioB64 := '';
    JObj.TryGetValue<string>('delta', AudioB64);
    if AudioB64 <> '' then
      DoAudioChunk(TNetEncoding.Base64.DecodeStringToBytes(AudioB64));
  end

  else if EventType = 'response.done' then
  begin
    if FHadToolCalls then
    begin
      // Turno intermedio de tool calls: la respuesta hablada llega en el
      // siguiente response (tras enviar los outputs + response.create).
      // Conservar el texto ya hablado para el OnAssistantText final.
      if FAssistantText <> '' then
        FCarryAssistantText := FCarryAssistantText + FAssistantText + ' ';
      FToolsDone := True;
      TryContinueAfterTools;
    end
    else
    begin
      // Turno final: emitir el texto completo (pre-tools + este response)
      if FCarryAssistantText + FAssistantText <> '' then
        DoAssistantText(FCarryAssistantText + FAssistantText);
      FAssistantText      := '';
      FCarryAssistantText := '';
      DoAudioDone;
    end;
  end

  else if EventType = 'error' then
  begin
    ErrMsg  := 'Error desconocido';
    ErrCode := '';
    JError  := nil;
    JObj.TryGetValue<TJSONObject>('error', JError);
    if Assigned(JError) then
    begin
      JError.TryGetValue<string>('message', ErrMsg);
      JError.TryGetValue<string>('code',    ErrCode);
    end;
    DoError(ErrMsg, ErrCode);
  end;
end;

{ Handlers del WebSocket }

procedure TAiGrokRealtimeChat.OnWSConnected(Sender: TObject);
begin
  GkLog('WS_CONNECTED');
  // session.update se enviara cuando llegue session.created
  DoConnected;
end;

procedure TAiGrokRealtimeChat.OnWSDisconnected(Sender: TObject);
begin
  GkLog('WS_DISCONNECTED');
  FSessionConfigured := False;
  DoDisconnected;
end;

procedure TAiGrokRealtimeChat.OnWSError(Sender: TObject; const ErrorMsg: string);
begin
  GkLog('WS_ERROR: ' + ErrorMsg);
  DoError(ErrorMsg, 'websocket_error');
end;

procedure TAiGrokRealtimeChat.OnWSFrame(Sender: TObject;
  Opcode: TAiRealtimeWSOpcode; const Data: TBytes; IsFinal: Boolean);
var
  JsonStr: string;
  JObj:    TJSONObject;
begin
  // Audio de salida en modo binario: el frame ES el PCM crudo del asistente
  if Opcode = rwsoBinary then
  begin
    GkLog('FRAME binary len=' + IntToStr(Length(Data)));
    if Length(Data) > 0 then
      DoAudioChunk(Data);
    Exit;
  end;
  GkLog('FRAME opcode=' + IntToStr(Ord(Opcode)) +
        ' len=' + IntToStr(Length(Data)) +
        ' data=' + Copy(TEncoding.UTF8.GetString(Data), 1, 2000));
  if Opcode <> rwsoText then Exit;
  JsonStr := TEncoding.UTF8.GetString(Data);
  JObj    := TJSONObject.ParseJSONValue(JsonStr) as TJSONObject;
  if Assigned(JObj) then
  try
    ProcessServerEvent(JObj);
  finally
    JObj.Free;
  end;
end;

{ Override de SendAudioChunk: encola el resampling + envio al hilo principal.
  El VoiceMonitor llama a este metodo desde el callback de WaveIn (hilo del
  sistema de audio de Windows), donde NO esta permitido llamar a funciones
  de socket o SChannel. TThread.Queue mueve la operacion al hilo principal. }
procedure TAiGrokRealtimeChat.SendAudioChunk(const PCM16Data: TBytes);
var
  LData: TBytes;
begin
  if not IsConnected then Exit;
  if Length(PCM16Data) = 0 then Exit;
  LData := Copy(PCM16Data); // copia rapida antes de salir del callback
  TThread.Queue(nil, procedure
  var
    Resampled: TBytes;
    TargetHz:  Integer;
  begin
    if not IsConnected then Exit;
    TargetHz := GetTargetSampleRate;
    if InputSampleRate <> TargetHz then
      Resampled := ResamplePCM16(LData, InputSampleRate, TargetHz)
    else
      Resampled := LData;
    InternalSendAudio(Resampled);
  end);
end;

{ Metodos abstractos implementados }

procedure TAiGrokRealtimeChat.InternalConnect;
var
  URL, Token: string;
begin
  if FEphemeralToken <> '' then
  begin
    // Auth client-side: el token efimero viaja como subprotocolo WebSocket
    // y la API key NO se envia (puede ni existir en el cliente)
    Token := FEphemeralToken;
    if not Token.StartsWith('xai-client-secret.') then
      Token := 'xai-client-secret.' + Token;
    FWebSocket.ExtraHeaders.Values['Authorization']          := '';
    FWebSocket.ExtraHeaders.Values['Sec-WebSocket-Protocol'] := Token;
  end
  else
  begin
    // Auth server-side con API key. NO enviar Sec-WebSocket-Protocol:
    // Grok reserva el subprotocolo para los tokens efimeros
    FWebSocket.ExtraHeaders.Values['Authorization'] := 'Bearer ' + ResolvedApiKey;
    FWebSocket.ExtraHeaders.Values['Sec-WebSocket-Protocol'] := '';
  end;
  URL := BuildWssUrl;
  GkLog('CONNECT url=' + URL);
  FConnectThread := TThread.CreateAnonymousThread(procedure begin
    if not FWebSocket.Connect(URL) then
      DoError('No se pudo conectar al servidor WebSocket de xAI',
              'connection_failed');
  end);
  FConnectThread.FreeOnTerminate := False;
  FConnectThread.Start;
end;

procedure TAiGrokRealtimeChat.InternalDisconnect;
begin
  FWebSocket.SendClose(1000, '');
  FWebSocket.Disconnect;
  if Assigned(FConnectThread) then
  begin
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FSessionConfigured := False;
  Connected          := False;
end;

procedure TAiGrokRealtimeChat.InternalSendAudio(const ResampledPCM16: TBytes);
var
  JObj: TJSONObject;
  B64:  string;
begin
  if Length(ResampledPCM16) = 0 then Exit;
  // Modo binario: PCM crudo como frame binario, sin sobre JSON/base64
  if FBinaryAudio then
  begin
    FWebSocket.SendBinary(ResampledPCM16);
    Exit;
  end;
  B64  := TNetEncoding.Base64.EncodeBytesToString(ResampledPCM16);
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type',  'input_audio_buffer.append');
    JObj.AddPair('audio', B64);
    FWebSocket.SendText(JObj.ToJSON);
  finally
    JObj.Free;
  end;
end;

// Construye el array de tools para session.update:
// tools nativas de xAI (booleans) + funciones locales/MCP de AiFunctions
// en formato plano OpenAI Responses (el mismo que usa Grok).
// Retorna nil si no hay ninguna tool que declarar.
function TAiGrokRealtimeChat.BuildToolsArray: TJSONArray;
var
  JTool:    TJSONObject;
  ToolsStr: string;
  JParsed:  TJSONValue;
  I:        Integer;
begin
  Result := TJSONArray.Create;

  if FEnableWebSearch then
  begin
    JTool := TJSONObject.Create;
    JTool.AddPair('type', 'web_search');
    Result.Add(JTool);
  end;

  if FEnableXSearch then
  begin
    JTool := TJSONObject.Create;
    JTool.AddPair('type', 'x_search');
    Result.Add(JTool);
  end;

  // file_search sobre colecciones de xAI (vector stores)
  if FFileSearchCollections.Count > 0 then
  begin
    JTool := TJSONObject.Create;
    JTool.AddPair('type', 'file_search');
    var JIds := TJSONArray.Create;
    for I := 0 to FFileSearchCollections.Count - 1 do
      if Trim(FFileSearchCollections[I]) <> '' then
        JIds.Add(Trim(FFileSearchCollections[I]));
    JTool.AddPair('vector_store_ids', JIds);
    if FFileSearchMaxResults > 0 then
      JTool.AddPair('max_num_results', TJSONNumber.Create(FFileSearchMaxResults));
    Result.Add(JTool);
  end;

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

  // Tools en JSON crudo (mcp remoto, formas nuevas del API, etc.)
  if Trim(FCustomToolsJson.Text) <> '' then
  begin
    JParsed := TJSONObject.ParseJSONValue(FCustomToolsJson.Text);
    try
      if JParsed is TJSONArray then
      begin
        for I := 0 to TJSONArray(JParsed).Count - 1 do
          Result.Add(TJSONObject(TJSONArray(JParsed).Items[I].Clone))
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

// Ejecuta un tool call solicitado por el modelo. La ejecucion corre en un
// hilo propio para no bloquear el reader thread del WebSocket (los handlers
// del usuario pueden tardar). El envio de response.create se coordina con
// response.done via TryContinueAfterTools: xAI exige que TODOS los outputs
// esten enviados antes del response.create.
procedure TAiGrokRealtimeChat.HandleFunctionCall(const JObj: TJSONObject);
var
  FuncName, CallId, Args: string;
begin
  FuncName := '';
  CallId   := '';
  Args     := '';
  JObj.TryGetValue<string>('name', FuncName);
  JObj.TryGetValue<string>('call_id', CallId);
  JObj.TryGetValue<string>('arguments', Args);
  if FuncName = '' then Exit;

  GkLog('TOOL_CALL name=' + FuncName + ' call_id=' + CallId + ' args=' + Args);
  FHadToolCalls := True;
  TInterlocked.Increment(FPendingToolCalls);

  TThread.CreateAnonymousThread(
    procedure
    var
      ToolCall: TAiToolsFunction;
      Handled:  Boolean;
    begin
      ToolCall := TAiToolsFunction.Create;
      try
        ToolCall.id        := CallId;
        ToolCall.name      := FuncName;
        ToolCall.Arguments := Args;
        Handled := False;
        try
          if Assigned(FAiFunctions) then
            Handled := FAiFunctions.DoCallFunction(ToolCall);
          if (not Handled) and Assigned(FOnCallToolFunction) then
          begin
            // El fallback corre sincronizado al hilo principal (puede tocar UI)
            TThread.Synchronize(nil, procedure begin
              FOnCallToolFunction(Self, ToolCall);
            end);
            Handled := True;
          end;
          if not Handled then
            ToolCall.Response :=
              'Error: no hay un manejador para la funcion ' + FuncName;
        except
          on E: Exception do
            ToolCall.Response := 'Error ejecutando ' + FuncName + ': ' + E.Message;
        end;
        SendFunctionOutput(CallId, ToolCall.Response);
      finally
        ToolCall.Free;
        TInterlocked.Decrement(FPendingToolCalls);
        TryContinueAfterTools;
      end;
    end).Start;
end;

procedure TAiGrokRealtimeChat.SendFunctionOutput(const ACallId, AOutput: string);
var
  JMsg, JItem: TJSONObject;
begin
  JMsg := TJSONObject.Create;
  try
    JMsg.AddPair('type', 'conversation.item.create');
    JItem := TJSONObject.Create;
    JItem.AddPair('type', 'function_call_output');
    JItem.AddPair('call_id', ACallId);
    JItem.AddPair('output', AOutput);
    JMsg.AddPair('item', JItem);
    FWebSocket.SendText(JMsg.ToJSON);
  finally
    JMsg.Free;
  end;
end;

// Envia el response.create de continuacion exactamente una vez, solo cuando
// (a) ya llego el response.done del turno de tools y (b) no quedan tool calls
// en ejecucion. Puede dispararse desde el reader thread (response.done) o
// desde el hilo del ultimo tool call — el CompareExchange evita duplicados.
procedure TAiGrokRealtimeChat.TryContinueAfterTools;
begin
  if not FToolsDone then Exit;
  if FPendingToolCalls > 0 then Exit;
  if TInterlocked.CompareExchange(FContinuationSent, 1, 0) <> 0 then Exit;
  GkLog('TOOLS_COMPLETE -> response.create');
  CreateResponse;
end;

procedure TAiGrokRealtimeChat.ForceMessage(const AText: string;
  AInterruptible: Boolean);
var
  JMsg, JItem, JContent: TJSONObject;
  JArr: TJSONArray;
begin
  if not IsConnected then Exit;
  JMsg := TJSONObject.Create;
  try
    JMsg.AddPair('type', 'conversation.item.create');
    JItem := TJSONObject.Create;
    JItem.AddPair('type', 'force_message');
    JItem.AddPair('role', 'assistant');
    if not AInterruptible then
      JItem.AddPair('interruptible', TJSONBool.Create(False));
    JContent := TJSONObject.Create;
    JContent.AddPair('type', 'output_text');
    JContent.AddPair('text', AText);
    JArr := TJSONArray.Create;
    JArr.Add(JContent);
    JItem.AddPair('content', JArr);
    JMsg.AddPair('item', JItem);
    FWebSocket.SendText(JMsg.ToJSON);
  finally
    JMsg.Free;
  end;
end;

procedure TAiGrokRealtimeChat.CreateResponse;
var
  JObj: TJSONObject;
begin
  if not IsConnected then Exit;
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type', 'response.create');
    FWebSocket.SendText(JObj.ToJSON);
  finally
    JObj.Free;
  end;
end;

procedure TAiGrokRealtimeChat.InternalCommitAudio;
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type', 'input_audio_buffer.commit');
    FWebSocket.SendText(JObj.ToJSON);
  finally
    JObj.Free;
  end;
end;

procedure TAiGrokRealtimeChat.InternalClearAudio;
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type', 'input_audio_buffer.clear');
    FWebSocket.SendText(JObj.ToJSON);
  finally
    JObj.Free;
  end;
end;

initialization
  TAiRealtimeFactory.Instance.RegisterDriver(
    TAiGrokRealtimeChat.GetDriverName, TAiGrokRealtimeChat);

end.
