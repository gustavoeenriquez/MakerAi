// MakerAI Suite — Drivers realtime de Qwen (Alibaba Model Studio / DashScope)
// wss://dashscope-intl.aliyuncs.com/api-ws/v1/realtime?model=<modelo>
// Protocolo compatible con OpenAI Realtime (eventos beta: response.audio.delta,
// response.audio_transcript.*). Auth: Authorization: Bearer (@DASHSCOPE_API_KEY).
//
// Tres drivers, registrados en TAiRealtimeFactory (usables tal cual desde
// TAiRealtimeConnection con DriverName):
//   'Qwen'          TAiQwenRealtimeChat       voz a voz (omni): STT + LLM + TTS
//   'QwenSTT'       TAiQwenRealtimeSTT        transcripcion en vivo (asr)
//   'QwenTranslate' TAiQwenRealtimeTranslate  traduccion simultanea con voz
//
// Audio: entrada PCM16 mono 16 kHz (todos los modelos); salida PCM16 24 kHz.
//
// Diferencias que normaliza el driver (verificadas en vivo, sep 2026):
//   - La transcripcion del usuario llega en tres formas segun el modelo:
//       omni:      ...transcription.delta con text='' y stash=texto acumulado
//       asr:       ...transcription.text  con text=confirmado + stash=pendiente
//       translate: ...transcription.delta con delta incremental
//     Todo se convierte a la semantica de OnTranscriptDelta (incremental; si el
//     servidor reescribe el comienzo se emite el texto completo, igual que Grok).
//   - Cada modelo tiene su voz por defecto (qwen3.8-omni: Tina; qwen3-omni: Cherry)
//     y rechaza voces ajenas con un error. Voice vacia = la del servidor, salvo en
//     traduccion: si session.update no trae voz el servidor cae a una invalida
//     (Chelsie), por eso QwenTranslate manda Tina por defecto.
//   - VAD: server_vad (rvmSemanticVad se mapea a server_vad); rvmManual envia
//     turn_detection null y requiere CommitAudio + CreateResponse.
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.Qwen;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  uMakerAi.Realtime, uMakerAi.Realtime.WebSocket, uMakerAi.WebSocket.Client;

type
  // Base comun: WebSocket, sesion y traduccion de eventos del servidor
  TAiQwenRealtimeBase = class(TAiRealtimeVoiceBase)
  private
    FWebSocket:         TAiRealtimeWSClient;
    FConnectThread:     TThread;
    FSessionConfigured: Boolean;
    FUrl:               string;
    FVoice:             string;
    FB64:               TBase64Encoding; // sin saltos de linea
    // Transcripcion acumulada -> deltas
    FLastItemId:     string;
    FLastTranscript: string;
    // Texto del asistente en el turno actual
    FAssistantText:  string;
    procedure OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
      const Data: TBytes; IsFinal: Boolean);
    procedure OnWSConnected(Sender: TObject);
    procedure OnWSDisconnected(Sender: TObject);
    procedure OnWSError(Sender: TObject; const ErrorMsg: string);
    procedure SendJson(AObj: TJSONObject);
    procedure EmitCumulative(const ItemId, Full: string);
  protected
    FInstructions: string; // publicado solo en el driver de conversacion
    function  VADObject: TJSONValue;
    // Cuerpo de 'session' en session.update; cada driver agrega lo suyo
    function  BuildSession: TJSONObject; virtual; abstract;
    function  GetTargetSampleRate: Integer; override;
    procedure InternalSendAudio(const ResampledPCM16: TBytes); override;
    procedure InternalConnect;    override;
    procedure InternalDisconnect; override;
    procedure InternalCommitAudio; override;
    procedure InternalClearAudio;  override;
    // Procesa un evento JSON del servidor (protegido: la suite lo alimenta sin red)
    procedure ProcessServerEvent(const JObj: TJSONObject); virtual;
    // Mensaje session.update completo (protegido: la suite lo inspecciona)
    function  BuildSessionUpdate: TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    // El callback de WaveIn no puede tocar sockets: el envio se encola al hilo principal
    procedure SendAudioChunk(const PCM16Data: TBytes); override;
    // Pide una respuesta (response.create). Necesario con VADMode = rvmManual
    // despues de CommitAudio, o tras enviar audio de un archivo en rafaga.
    procedure CreateResponse;
  published
    // Endpoint base (otra region: wss://dashscope.aliyuncs.com/api-ws/v1/realtime)
    property Url: string read FUrl write FUrl;
    // Voz del TTS; vacia = la del servidor para el modelo
    property Voice: string read FVoice write FVoice;
  end;

  // 'Qwen' — conversacion de voz (omni realtime)
  TAiQwenRealtimeChat = class(TAiQwenRealtimeBase)
  protected
    function BuildSession: TJSONObject; override;
  public
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
  published
    // System prompt de la sesion
    property Instructions: string read FInstructions write FInstructions;
  end;

  // 'QwenSTT' — transcripcion en vivo (qwen3-asr-flash-realtime)
  TAiQwenRealtimeSTT = class(TAiQwenRealtimeBase)
  protected
    function BuildSession: TJSONObject; override;
  public
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
  end;

  // 'QwenTranslate' — traduccion simultanea: OnTranscriptDelta/Completed = idioma
  // origen; OnAssistantTextDelta/Text = traduccion; OnAudioChunk = voz traducida
  TAiQwenRealtimeTranslate = class(TAiQwenRealtimeBase)
  private
    FTargetLanguage: string;
  protected
    function BuildSession: TJSONObject; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
  published
    // Idioma destino ('en', 'es', 'zh', 'ja'...)
    property TargetLanguage: string read FTargetLanguage write FTargetLanguage;
  end;

procedure Register;

implementation

const
  CQWENREALTIME_WSS = 'wss://dashscope-intl.aliyuncs.com/api-ws/v1/realtime';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiQwenRealtimeChat, TAiQwenRealtimeSTT, TAiQwenRealtimeTranslate]);
end;

{ TAiQwenRealtimeBase }

constructor TAiQwenRealtimeBase.Create(AOwner: TComponent);
begin
  inherited;
  ApiKey := '@DASHSCOPE_API_KEY';
  Model := GetDefaultModel;
  FUrl := CQWENREALTIME_WSS;
  FSessionConfigured := False;
  FConnectThread := nil;
  FB64 := TBase64Encoding.Create(0);
  FWebSocket := TAiRealtimeWSClient.Create;
  FWebSocket.OnFrame        := OnWSFrame;
  FWebSocket.OnConnected    := OnWSConnected;
  FWebSocket.OnDisconnected := OnWSDisconnected;
  FWebSocket.OnError        := OnWSError;
end;

destructor TAiQwenRealtimeBase.Destroy;
begin
  if IsConnected then InternalDisconnect;
  if Assigned(FConnectThread) then
  begin
    FWebSocket.Disconnect;
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FWebSocket.Free;
  FB64.Free;
  inherited;
end;

function TAiQwenRealtimeBase.GetTargetSampleRate: Integer;
begin
  Result := 16000; // entrada de todos los modelos realtime de Qwen
end;

procedure TAiQwenRealtimeBase.SendJson(AObj: TJSONObject);
begin
  try
    FWebSocket.SendText(AObj.ToJSON);
  finally
    AObj.Free;
  end;
end;

function TAiQwenRealtimeBase.VADObject: TJSONValue;
var
  J: TJSONObject;
begin
  if VADMode = rvmManual then
    Exit(TJSONNull.Create);
  J := TJSONObject.Create;
  J.AddPair('type', 'server_vad');
  J.AddPair('threshold', TJSONNumber.Create(VADThreshold));
  J.AddPair('silence_duration_ms', TJSONNumber.Create(SilenceDurationMs));
  J.AddPair('prefix_padding_ms', TJSONNumber.Create(PrefixPaddingMs));
  Result := J;
end;

function TAiQwenRealtimeBase.BuildSessionUpdate: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('type', 'session.update');
  Result.AddPair('session', BuildSession);
end;

procedure TAiQwenRealtimeBase.EmitCumulative(const ItemId, Full: string);
var
  Delta: string;
begin
  if ItemId <> FLastItemId then
  begin
    FLastItemId := ItemId;
    FLastTranscript := '';
  end;
  if Full = FLastTranscript then
    Exit;
  if Full.StartsWith(FLastTranscript) then
    Delta := Copy(Full, Length(FLastTranscript) + 1, MaxInt)
  else
    Delta := Full; // el servidor reescribio el comienzo: se emite completo
  FLastTranscript := Full;
  if Delta <> '' then
    DoTranscriptDelta(Delta);
end;

procedure TAiQwenRealtimeBase.ProcessServerEvent(const JObj: TJSONObject);
var
  EventType, ItemId, Text, Stash, Delta, Transcript, AudioB64, ErrMsg, ErrCode: string;
  AudioMs: Int64;
  JError: TJSONObject;
begin
  if not JObj.TryGetValue<string>('type', EventType) then Exit;
  ItemId := JObj.GetValue<string>('item_id', '');

  if EventType = 'session.created' then
  begin
    FSessionConfigured := False;
    SendJson(BuildSessionUpdate);
  end

  else if EventType = 'session.updated' then
  begin
    if not FSessionConfigured then
    begin
      FSessionConfigured := True;
      DoSessionReady;
    end;
  end

  else if EventType = 'input_audio_buffer.speech_started' then
  begin
    AudioMs := JObj.GetValue<Int64>('audio_start_ms', 0);
    DoSpeechStarted(AudioMs, ItemId);
  end

  else if EventType = 'input_audio_buffer.speech_stopped' then
  begin
    AudioMs := JObj.GetValue<Int64>('audio_end_ms', 0);
    DoSpeechStopped(AudioMs, ItemId);
  end

  // Transcripcion parcial del usuario: delta incremental (traduccion) o
  // text + stash acumulados (omni, asr)
  else if (EventType = 'conversation.item.input_audio_transcription.delta') or
          (EventType = 'conversation.item.input_audio_transcription.text') then
  begin
    Delta := JObj.GetValue<string>('delta', '');
    if Delta <> '' then
    begin
      FLastItemId := ItemId;
      FLastTranscript := FLastTranscript + Delta;
      DoTranscriptDelta(Delta);
    end
    else
    begin
      Text := JObj.GetValue<string>('text', '');
      Stash := JObj.GetValue<string>('stash', '');
      EmitCumulative(ItemId, Text + Stash);
    end;
  end

  else if EventType = 'conversation.item.input_audio_transcription.completed' then
  begin
    Transcript := JObj.GetValue<string>('transcript', '');
    FLastItemId := '';
    FLastTranscript := '';
    DoTranscriptCompleted(Trim(Transcript), ItemId);
  end

  else if EventType = 'response.created' then
    FAssistantText := ''

  else if (EventType = 'response.audio_transcript.delta') or (EventType = 'response.text.delta') or
          (EventType = 'response.output_audio_transcript.delta') then
  begin
    Delta := JObj.GetValue<string>('delta', '');
    if Delta <> '' then
    begin
      FAssistantText := FAssistantText + Delta;
      DoAssistantTextDelta(Delta);
    end;
  end

  else if (EventType = 'response.audio_transcript.done') or (EventType = 'response.text.done') or
          (EventType = 'response.output_audio_transcript.done') then
  begin
    // Texto completo autoritativo del turno
    Transcript := JObj.GetValue<string>('transcript', JObj.GetValue<string>('text', ''));
    if Transcript <> '' then
      FAssistantText := Transcript;
  end

  else if (EventType = 'response.audio.delta') or (EventType = 'response.output_audio.delta') then
  begin
    AudioB64 := JObj.GetValue<string>('delta', '');
    if AudioB64 <> '' then
      DoAudioChunk(TNetEncoding.Base64.DecodeStringToBytes(AudioB64));
  end

  else if EventType = 'response.done' then
  begin
    if Trim(FAssistantText) <> '' then
      DoAssistantText(Trim(FAssistantText));
    FAssistantText := '';
    DoAudioDone;
  end

  else if EventType = 'error' then
  begin
    ErrMsg := 'Error desconocido';
    ErrCode := '';
    if JObj.TryGetValue<TJSONObject>('error', JError) then
    begin
      JError.TryGetValue<string>('message', ErrMsg);
      JError.TryGetValue<string>('code', ErrCode);
    end;
    DoError(ErrMsg, ErrCode);
  end;
end;

procedure TAiQwenRealtimeBase.OnWSConnected(Sender: TObject);
begin
  // session.update se envia al llegar session.created
  DoConnected;
end;

procedure TAiQwenRealtimeBase.OnWSDisconnected(Sender: TObject);
begin
  FSessionConfigured := False;
  DoDisconnected;
end;

procedure TAiQwenRealtimeBase.OnWSError(Sender: TObject; const ErrorMsg: string);
begin
  DoError(ErrorMsg, 'websocket_error');
end;

procedure TAiQwenRealtimeBase.OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
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

procedure TAiQwenRealtimeBase.SendAudioChunk(const PCM16Data: TBytes);
var
  LData: TBytes;
begin
  if not IsConnected then Exit;
  if Length(PCM16Data) = 0 then Exit;
  LData := Copy(PCM16Data);
  TThread.Queue(nil, procedure
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

procedure TAiQwenRealtimeBase.InternalConnect;
var
  LUrl: string;
begin
  // TAiRealtimeConnection copia su ApiKey/Model aunque esten vacios
  if ApiKey = '' then
    ApiKey := '@DASHSCOPE_API_KEY';
  FWebSocket.ExtraHeaders.Values['Authorization'] := 'Bearer ' + ResolvedApiKey;
  if Model = '' then
    Model := GetDefaultModel;
  LUrl := FUrl + '?model=' + Model;
  FConnectThread := TThread.CreateAnonymousThread(procedure begin
    if not FWebSocket.Connect(LUrl) then
      DoError('No se pudo conectar al servidor realtime de Qwen', 'connection_failed');
  end);
  FConnectThread.FreeOnTerminate := False;
  FConnectThread.Start;
end;

procedure TAiQwenRealtimeBase.InternalDisconnect;
begin
  FWebSocket.SendClose(1000, '');
  FWebSocket.Disconnect;
  if Assigned(FConnectThread) then
  begin
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FSessionConfigured := False;
  Connected := False;
end;

procedure TAiQwenRealtimeBase.InternalSendAudio(const ResampledPCM16: TBytes);
var
  J: TJSONObject;
begin
  if Length(ResampledPCM16) = 0 then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'input_audio_buffer.append');
  // Base64 SIN saltos de linea: TNetEncoding.Base64 corta cada 76 caracteres y
  // DashScope responde "Illegal base64 character d" (xAI y OpenAI si los toleran)
  J.AddPair('audio', FB64.EncodeBytesToString(ResampledPCM16));
  SendJson(J);
end;

procedure TAiQwenRealtimeBase.InternalCommitAudio;
var
  J: TJSONObject;
begin
  J := TJSONObject.Create;
  J.AddPair('type', 'input_audio_buffer.commit');
  SendJson(J);
end;

procedure TAiQwenRealtimeBase.InternalClearAudio;
var
  J: TJSONObject;
begin
  J := TJSONObject.Create;
  J.AddPair('type', 'input_audio_buffer.clear');
  SendJson(J);
end;

procedure TAiQwenRealtimeBase.CreateResponse;
var
  J: TJSONObject;
begin
  if not IsConnected then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'response.create');
  SendJson(J);
end;

{ TAiQwenRealtimeChat }

class function TAiQwenRealtimeChat.GetDriverName: string;
begin
  Result := 'Qwen';
end;

class function TAiQwenRealtimeChat.GetDefaultModel: string;
begin
  Result := 'qwen3.8-omni-flash-realtime';
end;

function TAiQwenRealtimeChat.BuildSession: TJSONObject;
var
  jMods: TJSONArray;
begin
  Result := TJSONObject.Create;
  jMods := TJSONArray.Create;
  jMods.Add('text');
  jMods.Add('audio');
  Result.AddPair('modalities', jMods);
  if FInstructions <> '' then
    Result.AddPair('instructions', FInstructions);
  if FVoice <> '' then
    Result.AddPair('voice', FVoice);
  Result.AddPair('input_audio_format', 'pcm16');
  Result.AddPair('output_audio_format', 'pcm24');
  Result.AddPair('turn_detection', VADObject);
end;

{ TAiQwenRealtimeSTT }

class function TAiQwenRealtimeSTT.GetDriverName: string;
begin
  Result := 'QwenSTT';
end;

class function TAiQwenRealtimeSTT.GetDefaultModel: string;
begin
  Result := 'qwen3-asr-flash-realtime';
end;

function TAiQwenRealtimeSTT.BuildSession: TJSONObject;
var
  jMods: TJSONArray;
  jTr: TJSONObject;
begin
  Result := TJSONObject.Create;
  jMods := TJSONArray.Create;
  jMods.Add('text');
  Result.AddPair('modalities', jMods);
  Result.AddPair('input_audio_format', 'pcm');
  Result.AddPair('sample_rate', TJSONNumber.Create(GetTargetSampleRate));
  if Language <> '' then
  begin
    jTr := TJSONObject.Create;
    jTr.AddPair('language', Language);
    Result.AddPair('input_audio_transcription', jTr);
  end;
  Result.AddPair('turn_detection', VADObject);
end;

{ TAiQwenRealtimeTranslate }

constructor TAiQwenRealtimeTranslate.Create(AOwner: TComponent);
begin
  inherited;
  FTargetLanguage := 'en';
  Voice := 'Tina';
end;

class function TAiQwenRealtimeTranslate.GetDriverName: string;
begin
  Result := 'QwenTranslate';
end;

class function TAiQwenRealtimeTranslate.GetDefaultModel: string;
begin
  Result := 'qwen3.8-livetranslate-flash-realtime';
end;

function TAiQwenRealtimeTranslate.BuildSession: TJSONObject;
var
  jTr: TJSONObject;
begin
  Result := TJSONObject.Create;
  jTr := TJSONObject.Create;
  jTr.AddPair('language', FTargetLanguage);
  Result.AddPair('translation', jTr);
  // Sin voz explicita el servidor cae a Chelsie, que este modelo rechaza
  if Voice <> '' then
    Result.AddPair('voice', Voice)
  else
    Result.AddPair('voice', 'Tina');
end;

initialization
  TAiRealtimeFactory.Instance.RegisterDriver(TAiQwenRealtimeChat.GetDriverName, TAiQwenRealtimeChat);
  TAiRealtimeFactory.Instance.RegisterDriver(TAiQwenRealtimeSTT.GetDriverName, TAiQwenRealtimeSTT);
  TAiRealtimeFactory.Instance.RegisterDriver(TAiQwenRealtimeTranslate.GetDriverName, TAiQwenRealtimeTranslate);

end.
