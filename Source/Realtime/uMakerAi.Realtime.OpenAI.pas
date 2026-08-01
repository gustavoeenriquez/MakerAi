// MakerAI Suite — Driver OpenAI Realtime STT
// wss://api.openai.com/v1/realtime  (RFC 6455 + protocolo OpenAI Realtime v1)
//
// Modelos soportados: gpt-realtime-2.1 (default — mejor reconocimiento
//   alfanumerico y manejo de ruido/silencios), gpt-realtime-2.1-mini
//   (mas rapido/economico), gpt-realtime (anterior)
// Modelos de transcripcion:
//   gpt-live-transcribe (default — baja latencia, WER 9.60% vs 11.65% whisper)
//   gpt-transcribe (turnos confirmados; usa turnos previos como contexto)
//   gpt-4o-transcribe, gpt-4o-mini-transcribe, whisper-1 (legacy)
// Los modelos nuevos aceptan contexto: TranscriptionPrompt (tema libre),
// TranscriptionKeywords (terminos de dominio), Languages (multi-idioma) y
// LowDelay (parciales mas rapidos a costa de precision).
// Audio: PCM16, 24kHz, mono, little-endian
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.OpenAI;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  System.IOUtils,
  uMakerAi.Realtime, uMakerAi.Realtime.WebSocket, uMakerAi.WebSocket.Client;

type
  TAiOpenAiTranscriptionModel = (
    otmGpt4oTranscribe,     // gpt-4o-transcribe (generacion anterior)
    otmGpt4oMiniTranscribe, // gpt-4o-mini-transcribe (anterior, economico)
    otmWhisper1,            // whisper-1 (compatibilidad legacy)
    otmGptLiveTranscribe,   // gpt-live-transcribe (recomendado para vivo)
    otmGptTranscribe        // gpt-transcribe (turnos confirmados + contexto)
  );

  TAiOpenAiRealtimeSTT = class(TAiRealtimeBase)
  private
    FWebSocket:          TAiRealtimeWSClient;
    FConnectThread:      TThread; // hilo de conexion — esperado antes de liberar FWebSocket
    FTranscriptionModel: TAiOpenAiTranscriptionModel;
    FSessionConfigured:  Boolean;
    // Contexto de transcripcion (solo modelos gpt-live-transcribe/gpt-transcribe)
    FTranscriptionPrompt:   string;   // descripcion libre del tema/entorno
    FTranscriptionKeywords: TStrings; // terminos de dominio esperados
    FLanguages:             TStrings; // idiomas esperados (multi); si esta
                                      // vacio se usa Language (de la base)
    FLowDelay:              Boolean;  // delay:'low' — solo gpt-live-transcribe
    procedure SetTranscriptionKeywords(const Value: TStrings);
    procedure SetLanguages(const Value: TStrings);
    procedure OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
      const Data: TBytes; IsFinal: Boolean);
    procedure OnWSConnected(Sender: TObject);
    procedure OnWSDisconnected(Sender: TObject);
    procedure OnWSError(Sender: TObject; const ErrorMsg: string);
    procedure ProcessServerEvent(const JObj: TJSONObject);
    procedure SendSessionUpdate;
    function  VADObject: TJSONObject;
    function  TranscriptionModelStr: string;
    function  BuildWssUrl: string;
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
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
  published
    property TranscriptionModel: TAiOpenAiTranscriptionModel
      read  FTranscriptionModel
      write FTranscriptionModel
      default otmGptLiveTranscribe;
    // Contexto libre de la grabacion (tema, entorno) — mejora la precision.
    // Solo gpt-live-transcribe / gpt-transcribe
    property TranscriptionPrompt: string
      read FTranscriptionPrompt write FTranscriptionPrompt;
    // Terminos de dominio esperados (nombres, productos, codigos) — una linea
    // por termino, sin < > ni saltos. Solo modelos nuevos
    property TranscriptionKeywords: TStrings
      read FTranscriptionKeywords write SetTranscriptionKeywords;
    // Idiomas esperados cuando el audio puede mezclar varios (codigos BCP-47,
    // uno por linea). Vacio = se usa Language. Solo modelos nuevos
    property Languages: TStrings read FLanguages write SetLanguages;
    // Parciales mas rapidos a costa de precision (solo gpt-live-transcribe)
    property LowDelay: Boolean read FLowDelay write FLowDelay default False;
  end;

  // -------------------------------------------------------------------------
  // Traduccion de voz en streaming continuo (gpt-realtime-translate, may 2026)
  // wss://api.openai.com/v1/realtime/translations
  //
  // A diferencia del Realtime normal NO hay VAD ni turnos: se envia audio de
  // forma continua (incluidos los silencios) y el servidor emite en paralelo:
  //   session.input_transcript.delta  -> OnTranscriptDelta   (idioma origen)
  //   session.output_transcript.delta -> OnAssistantTextDelta (texto traducido)
  //   session.output_audio.delta      -> OnAudioChunk (TTS traducido, PCM16 24k)
  // Al desconectar se envia session.close; session.closed dispara
  // OnAssistantText (texto traducido completo) + OnAudioDone.
  // -------------------------------------------------------------------------
  TAiOpenAiRealtimeTranslate = class(TAiRealtimeVoiceBase)
  private
    FWebSocket:         TAiRealtimeWSClient;
    FConnectThread:     TThread;
    FSessionConfigured: Boolean;
    FTargetLanguage:    string;  // idioma de salida ('es', 'en', ...)
    FTranslatedText:    string;  // acumulado del transcript traducido
    FSourceTranscription: Boolean; // emitir tambien el transcript origen
    procedure OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
      const Data: TBytes; IsFinal: Boolean);
    procedure OnWSConnected(Sender: TObject);
    procedure OnWSDisconnected(Sender: TObject);
    procedure OnWSError(Sender: TObject; const ErrorMsg: string);
    procedure ProcessServerEvent(const JObj: TJSONObject);
    procedure SendSessionUpdate;
  protected
    function  GetTargetSampleRate: Integer; override;
    procedure InternalSendAudio(const ResampledPCM16: TBytes); override;
    procedure InternalConnect;    override;
    procedure InternalDisconnect; override;
    // Sin buffer de turnos: commit y clear no aplican al stream continuo
    procedure InternalCommitAudio; override;
    procedure InternalClearAudio;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
  published
    // Idioma al que se traduce el audio entrante (codigo ISO: 'es', 'en'...)
    property TargetLanguage: string read FTargetLanguage write FTargetLanguage;
    // Emitir tambien la transcripcion en el idioma ORIGEN via
    // OnTranscriptDelta (por defecto el endpoint solo entrega la traduccion)
    property SourceTranscription: Boolean read FSourceTranscription
      write FSourceTranscription default False;
  end;

  Procedure Register;

implementation


const
  CTRANSLATE_WSS = 'wss://api.openai.com/v1/realtime/translations';
  CTRANSLATE_LOG = ''; // ruta de log diagnostico; '' para deshabilitar

procedure TrLog(const AMsg: string);
begin
  if CTRANSLATE_LOG = '' then Exit;
  try
    TFile.AppendAllText(CTRANSLATE_LOG,
      FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMsg + sLineBreak);
  except end;
end;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenAiRealtimeSTT, TAiOpenAiRealtimeTranslate]);
end;

{ TAiOpenAiRealtimeSTT }

constructor TAiOpenAiRealtimeSTT.Create(AOwner: TComponent);
begin
  inherited;
  FTranscriptionModel := otmGptLiveTranscribe;
  FSessionConfigured  := False;
  FConnectThread      := nil;
  FLowDelay           := False;
  FTranscriptionKeywords := TStringList.Create;
  FLanguages             := TStringList.Create;
  FWebSocket          := TAiRealtimeWSClient.Create;
  FWebSocket.OnFrame       := OnWSFrame;
  FWebSocket.OnConnected   := OnWSConnected;
  FWebSocket.OnDisconnected:= OnWSDisconnected;
  FWebSocket.OnError       := OnWSError;
end;

destructor TAiOpenAiRealtimeSTT.Destroy;
begin
  if IsConnected then InternalDisconnect;
  // Abortar conexion pendiente aunque todavia no este conectado
  if Assigned(FConnectThread) then
  begin
    FWebSocket.Disconnect; // cierra handles WinHTTP para desbloquear el hilo
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FWebSocket.Free;
  FTranscriptionKeywords.Free;
  FLanguages.Free;
  inherited;
end;

procedure TAiOpenAiRealtimeSTT.SetTranscriptionKeywords(const Value: TStrings);
begin
  FTranscriptionKeywords.Assign(Value);
end;

procedure TAiOpenAiRealtimeSTT.SetLanguages(const Value: TStrings);
begin
  FLanguages.Assign(Value);
end;

class function TAiOpenAiRealtimeSTT.GetDriverName: string;
begin
  Result := 'OpenAI';
end;

class function TAiOpenAiRealtimeSTT.GetDefaultModel: string;
begin
  Result := 'gpt-realtime-2.1';
end;

function TAiOpenAiRealtimeSTT.GetTargetSampleRate: Integer;
begin
  Result := 24000; // OpenAI Realtime API requiere PCM16 a 24kHz
end;

function TAiOpenAiRealtimeSTT.TranscriptionModelStr: string;
begin
  case FTranscriptionModel of
    otmGpt4oTranscribe:     Result := 'gpt-4o-transcribe';
    otmGpt4oMiniTranscribe: Result := 'gpt-4o-mini-transcribe';
    otmWhisper1:            Result := 'whisper-1';
    otmGptTranscribe:       Result := 'gpt-transcribe';
  else
    Result := 'gpt-live-transcribe';
  end;
end;

function TAiOpenAiRealtimeSTT.BuildWssUrl: string;
var
  M: string;
begin
  M := Model;
  if M = '' then M := GetDefaultModel;
  Result := 'wss://api.openai.com/v1/realtime?model=' + M;
end;

function TAiOpenAiRealtimeSTT.VADObject: TJSONObject;
begin
  if VADMode = rvmManual then
  begin
    Result := nil;
    Exit;
  end;
  Result := TJSONObject.Create;
  if VADMode = rvmSemanticVad then
    Result.AddPair('type', 'semantic_vad')
  else
  begin
    Result.AddPair('type', 'server_vad');
    Result.AddPair('threshold',           TJSONNumber.Create(VADThreshold));
    Result.AddPair('prefix_padding_ms',   TJSONNumber.Create(PrefixPaddingMs));
    Result.AddPair('silence_duration_ms', TJSONNumber.Create(SilenceDurationMs));
  end;
  // Transcripcion pura — no generar respuesta LLM tras el turno
  Result.AddPair('create_response', TJSONFalse.Create);
end;

procedure TAiOpenAiRealtimeSTT.SendSessionUpdate;
// Esquema actualizado 2026: los campos van anidados bajo session.audio.input.
// session.type = 'realtime' para gpt-realtime.
// modalities sigue al nivel de session (fuera de audio).
var
  JMsg, JSession, JAudio, JInput, JTranscription, JVAD, JFormat, JNoise: TJSONObject;
  JLanguages, JKeywords: TJSONArray;
  I: Integer;
begin
  JMsg     := TJSONObject.Create;
  JSession := TJSONObject.Create;
  JAudio   := TJSONObject.Create;
  JInput   := TJSONObject.Create;

  JMsg.AddPair('type', 'session.update');
  JSession.AddPair('type', 'realtime');

  // Formato de audio de entrada: PCM16 a 24 kHz
  JFormat := TJSONObject.Create;
  JFormat.AddPair('type', 'audio/pcm');
  JFormat.AddPair('rate', TJSONNumber.Create(24000));
  JInput.AddPair('format', JFormat);

  // Modelo de transcripcion + contexto
  JTranscription := TJSONObject.Create;
  JTranscription.AddPair('model', TranscriptionModelStr);
  if FTranscriptionModel in [otmGptLiveTranscribe, otmGptTranscribe] then
  begin
    // Modelos nuevos: languages (array), prompt, keywords y delay
    JLanguages := nil;
    if FLanguages.Count > 0 then
    begin
      JLanguages := TJSONArray.Create;
      for I := 0 to FLanguages.Count - 1 do
        if Trim(FLanguages[I]) <> '' then
          JLanguages.Add(Trim(FLanguages[I]));
    end
    else if Language <> '' then
    begin
      JLanguages := TJSONArray.Create;
      JLanguages.Add(Language);
    end;
    if Assigned(JLanguages) then
      JTranscription.AddPair('languages', JLanguages);

    if FTranscriptionPrompt <> '' then
      JTranscription.AddPair('prompt', FTranscriptionPrompt);

    if FTranscriptionKeywords.Count > 0 then
    begin
      JKeywords := TJSONArray.Create;
      for I := 0 to FTranscriptionKeywords.Count - 1 do
        if Trim(FTranscriptionKeywords[I]) <> '' then
          JKeywords.Add(Trim(FTranscriptionKeywords[I]));
      JTranscription.AddPair('keywords', JKeywords);
    end;

    if FLowDelay and (FTranscriptionModel = otmGptLiveTranscribe) then
      JTranscription.AddPair('delay', 'low');
  end
  else if Language <> '' then
    // Modelos legacy: campo language singular
    JTranscription.AddPair('language', Language);
  JInput.AddPair('transcription', JTranscription);

  // VAD / turn detection
  JVAD := VADObject;
  if Assigned(JVAD) then
    JInput.AddPair('turn_detection', JVAD)
  else
    JInput.AddPair('turn_detection', TJSONNull.Create);

  // Reduccion de ruido (opcional)
  if NoiseReduction <> rnrNone then
  begin
    JNoise := TJSONObject.Create;
    if NoiseReduction = rnrNearField then
      JNoise.AddPair('type', 'near_field')
    else
      JNoise.AddPair('type', 'far_field');
    JInput.AddPair('noise_reduction', JNoise);
  end;

  JAudio.AddPair('input', JInput);
  JSession.AddPair('audio', JAudio);
  JMsg.AddPair('session', JSession);
  try
    FWebSocket.SendText(JMsg.ToJSON);
  finally
    JMsg.Free;
  end;
end;

procedure TAiOpenAiRealtimeSTT.ProcessServerEvent(const JObj: TJSONObject);
var
  EventType:  string;
  ItemId:     string;
  AudioMs:    Int64;
  Delta:      string;
  Transcript: string;
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

  else if EventType = 'conversation.item.input_audio_transcription.delta' then
  begin
    Delta := '';
    JObj.TryGetValue<string>('delta', Delta);
    if Delta <> '' then
      DoTranscriptDelta(Delta);
  end

  else if EventType = 'conversation.item.input_audio_transcription.completed' then
  begin
    Transcript := '';
    ItemId     := '';
    JObj.TryGetValue<string>('transcript', Transcript);
    JObj.TryGetValue<string>('item_id', ItemId);
    DoTranscriptCompleted(Transcript, ItemId);
  end

  else if EventType = 'conversation.item.input_audio_transcription.failed' then
  begin
    ErrMsg  := 'Transcripcion fallida';
    ErrCode := 'transcription_failed';
    JError  := JObj.GetValue<TJSONObject>('error');
    if Assigned(JError) then
    begin
      JError.TryGetValue<string>('message', ErrMsg);
      JError.TryGetValue<string>('code',    ErrCode);
    end;
    DoError(ErrMsg, ErrCode);
  end

  else if EventType = 'error' then
  begin
    ErrMsg  := 'Error desconocido';
    ErrCode := '';
    JError  := JObj.GetValue<TJSONObject>('error');
    if Assigned(JError) then
    begin
      JError.TryGetValue<string>('message', ErrMsg);
      JError.TryGetValue<string>('code',    ErrCode);
    end;
    DoError(ErrMsg, ErrCode);
  end;
end;

{ Eventos del WebSocket }

procedure TAiOpenAiRealtimeSTT.OnWSConnected(Sender: TObject);
begin
  // FConnected y el evento OnConnected se disparan desde DoConnected
  // que llamamos aqui; session.update se enviara cuando llegue session.created
  DoConnected;
end;

procedure TAiOpenAiRealtimeSTT.OnWSDisconnected(Sender: TObject);
begin
  FSessionConfigured := False;
  DoDisconnected;
end;

procedure TAiOpenAiRealtimeSTT.OnWSError(Sender: TObject; const ErrorMsg: string);
begin
  DoError(ErrorMsg, 'websocket_error');
end;

procedure TAiOpenAiRealtimeSTT.OnWSFrame(Sender: TObject;
  Opcode: TAiRealtimeWSOpcode; const Data: TBytes; IsFinal: Boolean);
var
  JsonStr: string;
  JObj:    TJSONObject;
begin
  if WS_DIAG_LOG <> '' then
    try TFile.AppendAllText(WS_DIAG_LOG,
      'FRAME opcode=' + IntToStr(Ord(Opcode)) + ' len=' + IntToStr(Length(Data)) +
      ' json=' + Copy(TEncoding.UTF8.GetString(Data), 1, 120) + sLineBreak);
    except end;
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

{ Metodos abstractos implementados }

procedure TAiOpenAiRealtimeSTT.InternalConnect;
var
  URL: string;
begin
  FWebSocket.ExtraHeaders.Values['Authorization']          := 'Bearer ' + ResolvedApiKey;
  FWebSocket.ExtraHeaders.Values['Sec-WebSocket-Protocol'] := 'realtime';
  URL := BuildWssUrl;
  // Conectar en background con TThread controlado (no fire-and-forget)
  // para poder esperar su finalizacion antes de liberar FWebSocket
  FConnectThread := TThread.CreateAnonymousThread(procedure begin
    if not FWebSocket.Connect(URL) then
      DoError('No se pudo conectar al servidor WebSocket', 'connection_failed');
  end);
  FConnectThread.FreeOnTerminate := False;
  FConnectThread.Start;
end;

procedure TAiOpenAiRealtimeSTT.InternalDisconnect;
begin
  FWebSocket.SendClose(1000, '');
  // Abortar la conexion en curso cerrando los handles WinHTTP —
  // las llamadas WinHTTP en FConnectThread fallaran y el hilo terminara
  FWebSocket.Disconnect;
  if Assigned(FConnectThread) then
  begin
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FSessionConfigured := False;
  Connected          := False;
end;

procedure TAiOpenAiRealtimeSTT.InternalSendAudio(const ResampledPCM16: TBytes);
var
  JObj: TJSONObject;
  B64:  string;
begin
  if Length(ResampledPCM16) = 0 then Exit;
  // Codificar PCM16 en Base64 y enviar como evento JSON
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

procedure TAiOpenAiRealtimeSTT.InternalCommitAudio;
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

procedure TAiOpenAiRealtimeSTT.InternalClearAudio;
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

{ TAiOpenAiRealtimeTranslate }

constructor TAiOpenAiRealtimeTranslate.Create(AOwner: TComponent);
begin
  inherited;
  FSessionConfigured := False;
  FConnectThread     := nil;
  FTargetLanguage    := 'en';
  FWebSocket         := TAiRealtimeWSClient.Create;
  FWebSocket.OnFrame        := OnWSFrame;
  FWebSocket.OnConnected    := OnWSConnected;
  FWebSocket.OnDisconnected := OnWSDisconnected;
  FWebSocket.OnError        := OnWSError;
end;

destructor TAiOpenAiRealtimeTranslate.Destroy;
begin
  if IsConnected then InternalDisconnect;
  if Assigned(FConnectThread) then
  begin
    FWebSocket.Disconnect;
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FWebSocket.Free;
  inherited;
end;

class function TAiOpenAiRealtimeTranslate.GetDriverName: string;
begin
  Result := 'OpenAiTranslate';
end;

class function TAiOpenAiRealtimeTranslate.GetDefaultModel: string;
begin
  Result := 'gpt-realtime-translate';
end;

function TAiOpenAiRealtimeTranslate.GetTargetSampleRate: Integer;
begin
  Result := 24000; // PCM16 24 kHz mono
end;

procedure TAiOpenAiRealtimeTranslate.SendSessionUpdate;
var
  JMsg, JSession, JAudio, JOutput, JInput, JTranscription: TJSONObject;
begin
  JMsg     := TJSONObject.Create;
  JSession := TJSONObject.Create;
  JAudio   := TJSONObject.Create;
  JOutput  := TJSONObject.Create;
  JMsg.AddPair('type', 'session.update');
  JOutput.AddPair('language', FTargetLanguage);
  JAudio.AddPair('output', JOutput);
  // La transcripcion del idioma origen es opt-in (el default del endpoint es
  // transcription:null — solo emite la traduccion)
  if FSourceTranscription then
  begin
    JInput := TJSONObject.Create;
    JTranscription := TJSONObject.Create;
    JTranscription.AddPair('model', 'gpt-live-transcribe');
    JInput.AddPair('transcription', JTranscription);
    JAudio.AddPair('input', JInput);
  end;
  JSession.AddPair('audio', JAudio);
  JMsg.AddPair('session', JSession);
  try
    FWebSocket.SendText(JMsg.ToJSON);
  finally
    JMsg.Free;
  end;
end;

procedure TAiOpenAiRealtimeTranslate.ProcessServerEvent(const JObj: TJSONObject);
var
  EventType, Delta, AudioB64: string;
  ErrMsg, ErrCode: string;
  JError: TJSONObject;
begin
  if not JObj.TryGetValue<string>('type', EventType) then Exit;

  if (EventType = 'session.created') or (EventType = 'session.updated') then
  begin
    // El endpoint de traduccion no documenta handshake; si el servidor
    // confirma la sesion se notifica una sola vez
    if not FSessionConfigured then
    begin
      FSessionConfigured := True;
      DoSessionReady;
    end;
  end

  else if EventType = 'session.input_transcript.delta' then
  begin
    Delta := '';
    JObj.TryGetValue<string>('delta', Delta);
    if Delta <> '' then
      DoTranscriptDelta(Delta); // transcripcion en el idioma origen
  end

  else if EventType = 'session.output_transcript.delta' then
  begin
    Delta := '';
    JObj.TryGetValue<string>('delta', Delta);
    if Delta <> '' then
    begin
      FTranslatedText := FTranslatedText + Delta;
      DoAssistantTextDelta(Delta); // texto traducido
    end;
  end

  else if EventType = 'session.output_audio.delta' then
  begin
    AudioB64 := '';
    if not JObj.TryGetValue<string>('delta', AudioB64) then
      JObj.TryGetValue<string>('audio', AudioB64); // tolerancia al esquema
    if AudioB64 <> '' then
      DoAudioChunk(TNetEncoding.Base64.DecodeStringToBytes(AudioB64));
  end

  else if EventType = 'session.closed' then
  begin
    if FTranslatedText <> '' then
      DoAssistantText(FTranslatedText);
    FTranslatedText := '';
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

procedure TAiOpenAiRealtimeTranslate.OnWSConnected(Sender: TObject);
begin
  TrLog('WS_CONNECTED');
  DoConnected;
  // Configurar el idioma de salida de inmediato: el endpoint acepta audio
  // desde el primer momento y no exige esperar confirmacion
  SendSessionUpdate;
  if not FSessionConfigured then
  begin
    FSessionConfigured := True;
    DoSessionReady;
  end;
end;

procedure TAiOpenAiRealtimeTranslate.OnWSDisconnected(Sender: TObject);
begin
  TrLog('WS_DISCONNECTED');
  FSessionConfigured := False;
  DoDisconnected;
end;

procedure TAiOpenAiRealtimeTranslate.OnWSError(Sender: TObject;
  const ErrorMsg: string);
begin
  TrLog('WS_ERROR: ' + ErrorMsg);
  DoError(ErrorMsg, 'websocket_error');
end;

procedure TAiOpenAiRealtimeTranslate.OnWSFrame(Sender: TObject;
  Opcode: TAiRealtimeWSOpcode; const Data: TBytes; IsFinal: Boolean);
var
  JsonStr: string;
  JObj:    TJSONObject;
begin
  TrLog('FRAME opcode=' + IntToStr(Ord(Opcode)) +
        ' len=' + IntToStr(Length(Data)) +
        ' data=' + Copy(TEncoding.UTF8.GetString(Data), 1, 300));
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

procedure TAiOpenAiRealtimeTranslate.InternalConnect;
var
  URL, M: string;
begin
  FWebSocket.ExtraHeaders.Values['Authorization'] := 'Bearer ' + ResolvedApiKey;
  M := Model;
  if M = '' then M := GetDefaultModel;
  URL := CTRANSLATE_WSS + '?model=' + M;
  TrLog('CONNECT url=' + URL + ' lang=' + FTargetLanguage);
  FConnectThread := TThread.CreateAnonymousThread(procedure begin
    if not FWebSocket.Connect(URL) then
      DoError('No se pudo conectar al servidor de traduccion',
              'connection_failed');
  end);
  FConnectThread.FreeOnTerminate := False;
  FConnectThread.Start;
end;

procedure TAiOpenAiRealtimeTranslate.InternalDisconnect;
var
  JObj: TJSONObject;
begin
  // Cierre ordenado del stream: el servidor emite session.closed con lo
  // pendiente antes de cerrar
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type', 'session.close');
    try FWebSocket.SendText(JObj.ToJSON); except end;
  finally
    JObj.Free;
  end;
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

procedure TAiOpenAiRealtimeTranslate.InternalSendAudio(
  const ResampledPCM16: TBytes);
var
  JObj: TJSONObject;
begin
  if Length(ResampledPCM16) = 0 then Exit;
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type',  'session.input_audio_buffer.append');
    JObj.AddPair('audio',
      TNetEncoding.Base64.EncodeBytesToString(ResampledPCM16));
    FWebSocket.SendText(JObj.ToJSON);
  finally
    JObj.Free;
  end;
end;

procedure TAiOpenAiRealtimeTranslate.InternalCommitAudio;
begin
  // Stream continuo sin turnos: no aplica
end;

procedure TAiOpenAiRealtimeTranslate.InternalClearAudio;
begin
  // Stream continuo sin turnos: no aplica
end;

initialization
  TAiRealtimeFactory.Instance.RegisterDriver(
    TAiOpenAiRealtimeSTT.GetDriverName, TAiOpenAiRealtimeSTT);
  TAiRealtimeFactory.Instance.RegisterDriver(
    TAiOpenAiRealtimeTranslate.GetDriverName, TAiOpenAiRealtimeTranslate);

end.
