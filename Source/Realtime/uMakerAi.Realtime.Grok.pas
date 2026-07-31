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
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.Grok;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  System.IOUtils,
  uMakerAi.Realtime, uMakerAi.Realtime.WebSocket, uMakerAi.WebSocket.Client;

type
  // Nivel de razonamiento del modelo mientras habla
  TAiGrokReasoningEffort = (
    greHigh, // razonamiento completo (default del servidor)
    greNone  // sin razonamiento — menor latencia
  );

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
    // Estado para convertir transcripcion acumulada -> deltas
    FLastItemId:     string;
    FLastTranscript: string;
    // Acumulador del texto del asistente en el turno actual
    FAssistantText:  string;
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
  protected
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
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
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
  inherited;
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
  JTranscription, JVAD, JReasoning: TJSONObject;
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

  // Formato de entrada: PCM16 a 24 kHz
  JFormat := TJSONObject.Create;
  JFormat.AddPair('type', 'audio/pcm');
  JFormat.AddPair('rate', TJSONNumber.Create(24000));
  JInput.AddPair('format', JFormat);

  // Sesgo de transcripcion por idioma (opcional)
  if Language <> '' then
  begin
    JTranscription := TJSONObject.Create;
    JTranscription.AddPair('language_hint', LanguageHint);
    JInput.AddPair('transcription', JTranscription);
  end;

  // Formato de salida: PCM16 a 24 kHz
  JOutFormat := TJSONObject.Create;
  JOutFormat.AddPair('type', 'audio/pcm');
  JOutFormat.AddPair('rate', TJSONNumber.Create(24000));
  JOutput.AddPair('format', JOutFormat);

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
    FAssistantText := ''

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
    if FAssistantText <> '' then
      DoAssistantText(FAssistantText);
    FAssistantText := '';
    DoAudioDone;
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
  URL: string;
begin
  FWebSocket.ExtraHeaders.Values['Authorization'] := 'Bearer ' + ResolvedApiKey;
  // NO enviar Sec-WebSocket-Protocol: Grok reserva el subprotocolo para
  // tokens efimeros (xai-client-secret.*); con API key debe ir vacio
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
