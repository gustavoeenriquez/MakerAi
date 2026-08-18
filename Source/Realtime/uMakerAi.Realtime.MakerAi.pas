// MakerAI Suite — Driver MakerAI Realtime (STT + LLM + TTS)
// wss://api.cimamaker.com/v1/audio/realtime  (RFC 6455 + subprotocolo "realtime")
//
// Conversacion de voz completa en un solo WebSocket:
//   1. STT  — el servidor transcribe el audio del usuario (Whisper)
//   2. LLM  — genera la respuesta del asistente (mk-scout, mk-pro, etc.)
//   3. TTS  — devuelve la respuesta como audio PCM16 a 24 kHz
//
// Protocolo simplificado (sin buffer de audio separado como OpenAI):
//   Cliente → {type:"session", model, voice?, language?, ...}   (post-connect)
//   Servidor→ {type:"ready"}                                    → OnSessionReady
//   Cliente → {type:"audio",  data:"<base64 PCM16>"}           (chunks de voz)
//   Cliente → {type:"commit"}                                   (fin de turno)
//   Servidor→ {type:"user_text",      text:"..."}               → OnTranscriptCompleted
//   Servidor→ {type:"assistant_text", text:"..."}               → OnAssistantText
//   Servidor→ {type:"audio",          data:"<base64 PCM16>"}    → OnAudioChunk
//   Servidor→ {type:"audio_done"}                               → OnAudioDone
//   Servidor→ {type:"error",          message:"..."}            → OnError
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.MakerAi;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  System.IOUtils,
  uMakerAi.Realtime, uMakerAi.Realtime.WebSocket, uMakerAi.WebSocket.Client;

type
  // Aliases de compatibilidad — los tipos de evento viven ahora en
  // uMakerAi.Realtime (compartidos por todos los drivers de voz full-duplex)
  TAiRealtimeAssistantTextEvent = uMakerAi.Realtime.TAiRealtimeAssistantTextEvent;
  TAiRealtimeAudioChunkEvent    = uMakerAi.Realtime.TAiRealtimeAudioChunkEvent;

  TAiMakerAiRealtimeChat = class(TAiRealtimeVoiceBase)
  private
    FWebSocket:     TAiRealtimeWSClient;
    FConnectThread: TThread;
    FSessionSent:   Boolean;
    // Propiedades especificas del driver MakerAI
    FVoice:         string;
    FSttModel:      string;
    FTtsModel:      string;
    FInstructions:  string;
    FRagId:         string;
    FStateless:     Boolean;
    FVadMode:       string;  // '' | 'server' | 'disabled'
    // Eventos VAD simplificados (sin parametros, propios de este protocolo)
    FOnSpeechStart:   TNotifyEvent;
    FOnSpeechEnd:     TNotifyEvent;
    // Callbacks internos del WebSocket
    procedure OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
      const Data: TBytes; IsFinal: Boolean);
    procedure OnWSConnected(Sender: TObject);
    procedure OnWSDisconnected(Sender: TObject);
    procedure OnWSError(Sender: TObject; const ErrorMsg: string);
    // Procesamiento de eventos del servidor
    procedure ProcessServerEvent(const JObj: TJSONObject);
    procedure SendSessionMessage;
    // Dispatchers thread-safe para los eventos VAD simplificados
    // (DoAssistantText/DoAudioChunk/DoAudioDone se heredan de la base)
    procedure DoSpeechStart;
    procedure DoSpeechEnd;
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
    // Override: despacha el envio al hilo principal para no bloquear el
    // callback de WaveIn (que no puede llamar a SChannel/sockets directamente).
    procedure SendAudioChunk(const PCM16Data: TBytes); override;
    // TTS directo en el servidor sin pasar por STT/LLM ({"type":"say"}).
    // Ideal para saludos deterministas al contestar una llamada; el server
    // responde con chunks de audio + audio_done y lo anota en el historial.
    procedure Say(const AText: string);
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
  published
    // Parametros de sesion (opcionales; todos resueltos por el servidor si se omiten)
    property Voice:        string read FVoice        write FVoice;
    property SttModel:     string read FSttModel     write FSttModel;
    property TtsModel:     string read FTtsModel     write FTtsModel;
    property Instructions: string read FInstructions write FInstructions;
    property RagId:        string read FRagId        write FRagId;
    // Cada commit corre con las instructions frescas, sin historial multi-turn.
    // Util para sesiones de transformacion pura (traduccion, dictado, etc.)
    property Stateless:    Boolean read FStateless write FStateless default False;
    // VAD del servidor: '' o 'disabled' → commit manual del cliente;
    // 'server' → el servidor detecta turnos (OpenAI turn_detection server_vad,
    //             threshold 0.5, silence 500ms, prefix 300ms). El cliente NO
    //             debe enviar type:commit — el servidor lo dispara solo.
    property VadMode:      string  read FVadMode  write FVadMode;
    // OnAssistantText / OnAudioChunk / OnAudioDone se heredan de
    // TAiRealtimeVoiceBase (published en la base)
    // Eventos VAD del servidor (speech_start / speech_end de OpenAI)
    property OnSpeechStart: TNotifyEvent read FOnSpeechStart write FOnSpeechStart;
    property OnSpeechEnd:   TNotifyEvent read FOnSpeechEnd   write FOnSpeechEnd;
  end;

  procedure Register;

implementation

const
  CMAKERAIREALTIME_WSS  = 'wss://api.cimamaker.com/v1/audio/realtime';
  CMAKERAIREALTIME_SUBP = 'realtime';
  CMAKERAIREALTIME_LOG  = {$IFDEF MSWINDOWS}'C:\Temp\makerai_ws.log'{$ELSE}'/tmp/makerai_ws.log'{$ENDIF}; // '' para deshabilitar

procedure MkLog(const AMsg: string);
begin
  if CMAKERAIREALTIME_LOG = '' then Exit;
  try
    TFile.AppendAllText(CMAKERAIREALTIME_LOG,
      FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AMsg + sLineBreak);
  except end;
end;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMakerAiRealtimeChat]);
end;

{ TAiMakerAiRealtimeChat }

constructor TAiMakerAiRealtimeChat.Create(AOwner: TComponent);
begin
  inherited;
  FSessionSent   := False;
  FConnectThread := nil;
  FSttModel      := 'mk-whisper-large';
  FWebSocket     := TAiRealtimeWSClient.Create;
  FWebSocket.OnFrame        := OnWSFrame;
  FWebSocket.OnConnected    := OnWSConnected;
  FWebSocket.OnDisconnected := OnWSDisconnected;
  FWebSocket.OnError        := OnWSError;
end;

destructor TAiMakerAiRealtimeChat.Destroy;
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

class function TAiMakerAiRealtimeChat.GetDriverName: string;
begin
  Result := 'MakerAi';
end;

class function TAiMakerAiRealtimeChat.GetDefaultModel: string;
begin
  Result := 'mk-gpt-oss-20b';
end;

function TAiMakerAiRealtimeChat.GetTargetSampleRate: Integer;
begin
  Result := 24000;
end;

procedure TAiMakerAiRealtimeChat.Say(const AText: string);
var
  JMsg: TJSONObject;
begin
  if (AText = '') or not IsConnected then Exit;
  JMsg := TJSONObject.Create;
  try
    JMsg.AddPair('type', 'say');
    JMsg.AddPair('text', AText);
    FWebSocket.SendText(JMsg.ToJSON);
  finally
    JMsg.Free;
  end;
  MkLog('SAY sent len=' + IntToStr(Length(AText)));
end;

procedure TAiMakerAiRealtimeChat.SendSessionMessage;
var
  JMsg:   TJSONObject;
  LModel: string;
begin
  LModel := Model;
  if LModel = '' then LModel := GetDefaultModel;
  JMsg := TJSONObject.Create;
  try
    JMsg.AddPair('type',  'session');
    JMsg.AddPair('model', LModel);
    if Language      <> '' then JMsg.AddPair('language',     Language);
    if FVoice        <> '' then JMsg.AddPair('voice',        FVoice);
    if FSttModel     <> '' then JMsg.AddPair('stt_model',    FSttModel);
    if FTtsModel     <> '' then JMsg.AddPair('tts_model',    FTtsModel);
    if FInstructions <> '' then JMsg.AddPair('instructions', FInstructions);
    if FRagId        <> '' then JMsg.AddPair('rag_id',       FRagId);
    if FStateless then JMsg.AddPair('stateless', TJSONBool.Create(True));
    if FVadMode      <> '' then JMsg.AddPair('vad',          FVadMode);
    MkLog('SESSION_JSON ' + JMsg.ToJSON);
    FWebSocket.SendText(JMsg.ToJSON);
  finally
    JMsg.Free;
  end;
  FSessionSent := True;
end;

procedure TAiMakerAiRealtimeChat.ProcessServerEvent(const JObj: TJSONObject);
var
  EventType: string;
  AText:     string;
  AudioB64:  string;
  AudioData: TBytes;
  ErrMsg:    string;
begin
  if not JObj.TryGetValue<string>('type', EventType) then Exit;

  if EventType = 'ready' then
    DoSessionReady

  else if EventType = 'user_text' then
  begin
    AText := '';
    JObj.TryGetValue<string>('text', AText);
    if AText <> '' then
      DoTranscriptCompleted(AText, '');
  end

  else if EventType = 'assistant_text' then
  begin
    AText := '';
    JObj.TryGetValue<string>('text', AText);
    if AText <> '' then
      DoAssistantText(AText);
  end

  else if EventType = 'audio' then
  begin
    AudioB64 := '';
    JObj.TryGetValue<string>('data', AudioB64);
    if AudioB64 <> '' then
    begin
      AudioData := TNetEncoding.Base64.DecodeStringToBytes(AudioB64);
      DoAudioChunk(AudioData);
    end;
  end

  else if EventType = 'audio_done' then
    DoAudioDone

  else if EventType = 'speech_start' then
    DoSpeechStart

  else if EventType = 'speech_end' then
    DoSpeechEnd

  else if EventType = 'error' then
  begin
    ErrMsg := 'Error del servidor MakerAI';
    if not JObj.TryGetValue<string>('message', ErrMsg) then
      JObj.TryGetValue<string>('error', ErrMsg);
    DoError(ErrMsg, 'server_error');
  end;
end;

{ Dispatchers thread-safe para los eventos VAD simplificados }

procedure TAiMakerAiRealtimeChat.DoSpeechStart;
begin
  MkLog('SPEECH_START');
  if not Assigned(FOnSpeechStart) then Exit;
  TThread.Queue(nil, procedure begin
    if Assigned(FOnSpeechStart) then FOnSpeechStart(Self);
  end);
end;

procedure TAiMakerAiRealtimeChat.DoSpeechEnd;
begin
  MkLog('SPEECH_END');
  if not Assigned(FOnSpeechEnd) then Exit;
  TThread.Queue(nil, procedure begin
    if Assigned(FOnSpeechEnd) then FOnSpeechEnd(Self);
  end);
end;

{ Handlers del WebSocket }

{ Override de SendAudioChunk: encola el resampling + envio al hilo principal.
  El VoiceMonitor llama a este metodo desde el callback de WaveIn (hilo del
  sistema de audio de Windows), donde NO esta permitido llamar a funciones
  de socket o SChannel. TThread.Queue mueve la operacion al hilo principal. }
procedure TAiMakerAiRealtimeChat.SendAudioChunk(const PCM16Data: TBytes);
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

procedure TAiMakerAiRealtimeChat.OnWSConnected(Sender: TObject);
begin
  MkLog('WS_CONNECTED');
  DoConnected;
  SendSessionMessage;
  MkLog('SESSION_SENT model=' + Model);
end;

procedure TAiMakerAiRealtimeChat.OnWSDisconnected(Sender: TObject);
begin
  MkLog('WS_DISCONNECTED');
  FSessionSent := False;
  DoDisconnected;
end;

procedure TAiMakerAiRealtimeChat.OnWSError(Sender: TObject;
  const ErrorMsg: string);
begin
  MkLog('WS_ERROR: ' + ErrorMsg);
  DoError(ErrorMsg, 'websocket_error');
end;

procedure TAiMakerAiRealtimeChat.OnWSFrame(Sender: TObject;
  Opcode: TAiRealtimeWSOpcode; const Data: TBytes; IsFinal: Boolean);
var
  JsonStr: string;
  JObj:    TJSONObject;
begin
  MkLog('FRAME opcode=' + IntToStr(Ord(Opcode)) +
        ' len=' + IntToStr(Length(Data)) +
        ' data=' + Copy(TEncoding.UTF8.GetString(Data), 1, 200));
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

{ Implementacion de metodos abstractos }

procedure TAiMakerAiRealtimeChat.InternalConnect;
begin
  MkLog('CONNECT url=' + CMAKERAIREALTIME_WSS + ' key_prefix=' +
        Copy(ResolvedApiKey, 1, 8) + '...');
  FWebSocket.ExtraHeaders.Values['Authorization']          := 'Bearer ' + ResolvedApiKey;
  FWebSocket.ExtraHeaders.Values['Sec-WebSocket-Protocol'] := CMAKERAIREALTIME_SUBP;
  FConnectThread := TThread.CreateAnonymousThread(procedure begin
    MkLog('THREAD_CONNECT_START');
    if not FWebSocket.Connect(CMAKERAIREALTIME_WSS) then
    begin
      MkLog('CONNECT_FAILED');
      DoError('No se pudo conectar al servidor WebSocket de MakerAI',
              'connection_failed');
    end
    else
      MkLog('THREAD_CONNECT_END');
  end);
  FConnectThread.FreeOnTerminate := False;
  FConnectThread.Start;
end;

procedure TAiMakerAiRealtimeChat.InternalDisconnect;
begin
  FWebSocket.SendClose(1000, '');
  FWebSocket.Disconnect;
  if Assigned(FConnectThread) then
  begin
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FSessionSent := False;
  Connected    := False;
end;

procedure TAiMakerAiRealtimeChat.InternalSendAudio(
  const ResampledPCM16: TBytes);
var
  JObj: TJSONObject;
  B64:  string;
begin
  if Length(ResampledPCM16) = 0 then Exit;
  B64  := TNetEncoding.Base64.EncodeBytesToString(ResampledPCM16);
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type', 'audio');
    JObj.AddPair('data', B64);
    FWebSocket.SendText(JObj.ToJSON);
    MkLog('AUDIO_SENT bytes=' + IntToStr(Length(ResampledPCM16)));
  finally
    JObj.Free;
  end;
end;

procedure TAiMakerAiRealtimeChat.InternalCommitAudio;
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type', 'commit');
    FWebSocket.SendText(JObj.ToJSON);
  finally
    JObj.Free;
  end;
end;

procedure TAiMakerAiRealtimeChat.InternalClearAudio;
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('type', 'clear');
    FWebSocket.SendText(JObj.ToJSON);
  finally
    JObj.Free;
  end;
end;

initialization
  TAiRealtimeFactory.Instance.RegisterDriver(
    TAiMakerAiRealtimeChat.GetDriverName, TAiMakerAiRealtimeChat);

end.
