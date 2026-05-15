// MakerAI Suite — Driver OpenAI Realtime STT
// wss://api.openai.com/v1/realtime  (RFC 6455 + protocolo OpenAI Realtime v1)
//
// Modelos soportados: gpt-4o-realtime-preview, gpt-4o-mini-realtime-preview
// Modelos de transcripcion: gpt-4o-transcribe, gpt-4o-mini-transcribe, whisper-1
// Audio: PCM16, 24kHz, mono, little-endian
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.OpenAI;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  System.IOUtils,
  uMakerAi.Realtime, uMakerAi.Realtime.WebSocket;

type
  TAiOpenAiTranscriptionModel = (
    otmGpt4oTranscribe,     // gpt-4o-transcribe (recomendado)
    otmGpt4oMiniTranscribe, // gpt-4o-mini-transcribe (mas rapido/economico)
    otmWhisper1             // whisper-1 (compatibilidad legacy)
  );

  TAiOpenAiRealtimeSTT = class(TAiRealtimeBase)
  private
    FWebSocket:          TAiRealtimeWSClient;
    FConnectThread:      TThread; // hilo de conexion — esperado antes de liberar FWebSocket
    FTranscriptionModel: TAiOpenAiTranscriptionModel;
    FSessionConfigured:  Boolean;
    procedure OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
      const Data: TBytes; IsFinal: Boolean);
    procedure OnWSConnected(Sender: TObject);
    procedure OnWSDisconnected(Sender: TObject);
    procedure OnWSError(Sender: TObject; const ErrorMsg: string);
    procedure ProcessServerEvent(const JObj: TJSONObject);
    procedure SendSessionUpdate;
    function  TranscriptionModelStr: string;
    function  BuildWssUrl: string;
    function  VADObject: TJSONObject;
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
      default otmGpt4oTranscribe;
  end;

implementation

{ TAiOpenAiRealtimeSTT }

constructor TAiOpenAiRealtimeSTT.Create(AOwner: TComponent);
begin
  inherited;
  FTranscriptionModel := otmGpt4oTranscribe;
  FSessionConfigured  := False;
  FConnectThread      := nil;
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
  inherited;
end;

class function TAiOpenAiRealtimeSTT.GetDriverName: string;
begin
  Result := 'OpenAI';
end;

class function TAiOpenAiRealtimeSTT.GetDefaultModel: string;
begin
  Result := 'gpt-4o-realtime-preview';
end;

function TAiOpenAiRealtimeSTT.GetTargetSampleRate: Integer;
begin
  Result := 24000; // OpenAI Realtime API requiere PCM16 a 24kHz
end;

function TAiOpenAiRealtimeSTT.TranscriptionModelStr: string;
begin
  case FTranscriptionModel of
    otmGpt4oMiniTranscribe: Result := 'gpt-4o-mini-transcribe';
    otmWhisper1:            Result := 'whisper-1';
  else
    Result := 'gpt-4o-transcribe';
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
  // Crea el objeto turn_detection segun el modo configurado
  if VADMode = rvmManual then
  begin
    Result := nil; // se usara TJSONNull en SendSessionUpdate
    Exit;
  end;
  Result := TJSONObject.Create;
  if VADMode = rvmSemanticVad then
    Result.AddPair('type', 'semantic_vad')
  else
  begin
    Result.AddPair('type', 'server_vad');
    Result.AddPair('threshold',          TJSONNumber.Create(VADThreshold));
    Result.AddPair('prefix_padding_ms',  TJSONNumber.Create(PrefixPaddingMs));
    Result.AddPair('silence_duration_ms', TJSONNumber.Create(SilenceDurationMs));
  end;
  // Transcripcion pura — no queremos que el servidor genere respuesta LLM
  Result.AddPair('create_response', TJSONFalse.Create);
end;

procedure TAiOpenAiRealtimeSTT.SendSessionUpdate;
var
  JMsg, JSession, JTranscription, JVAD, JNoise: TJSONObject;
  JMods: TJSONArray;
begin
  JMsg     := TJSONObject.Create;
  JSession := TJSONObject.Create;

  JMsg.AddPair('type', 'session.update');

  // Formato de audio de entrada
  JSession.AddPair('input_audio_format', 'pcm16');

  // Solo texto como modalidad de salida (sin audio generado por el modelo)
  JMods := TJSONArray.Create;
  JMods.Add('text');
  JSession.AddPair('modalities', JMods);

  // Configuracion del modelo de transcripcion
  JTranscription := TJSONObject.Create;
  JTranscription.AddPair('model', TranscriptionModelStr);
  if Language <> '' then
    JTranscription.AddPair('language', Language);
  JSession.AddPair('input_audio_transcription', JTranscription);

  // VAD
  JVAD := VADObject;
  if Assigned(JVAD) then
    JSession.AddPair('turn_detection', JVAD)
  else
    JSession.AddPair('turn_detection', TJSONNull.Create);

  // Reduccion de ruido (opcional)
  if NoiseReduction <> rnrNone then
  begin
    JNoise := TJSONObject.Create;
    if NoiseReduction = rnrNearField then
      JNoise.AddPair('type', 'near_field')
    else
      JNoise.AddPair('type', 'far_field');
    JSession.AddPair('input_audio_noise_reduction', JNoise);
  end;

  JMsg.AddPair('session', JSession); // JMsg toma ownership de JSession
  try
    FWebSocket.SendText(JMsg.ToJSON);
  finally
    JMsg.Free; // libera el arbol completo
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
  FWebSocket.ExtraHeaders.Values['OpenAI-Beta']            := 'realtime=v1';
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
  FWebSocket.SendClose;
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

initialization
  TAiRealtimeFactory.Instance.RegisterDriver(
    TAiOpenAiRealtimeSTT.GetDriverName, TAiOpenAiRealtimeSTT);

end.
