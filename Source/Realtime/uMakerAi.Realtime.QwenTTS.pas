// MakerAI Suite — Qwen TTS en tiempo real (texto -> audio en vivo)
// wss://dashscope-intl.aliyuncs.com/api-ws/v1/realtime?model=qwen3-tts-flash-realtime
//
// Componente aparte de TAiRealtimeBase: aquella jerarquia es de audio de entrada
// (microfono -> servidor); esta va al reves, texto de entrada y audio de salida.
// Uso tipico: leer en voz alta la respuesta de un LLM mientras se genera —
// AppendText con cada delta de OnReceiveData y Finish al terminar. El primer
// audio llega ~0.7 s despues del primer texto.
//
// Protocolo (verificado en vivo, sep 2026):
//   cliente: session.update {voice, mode, response_format, sample_rate,
//            language_type, instructions}, input_text_buffer.append {text},
//            input_text_buffer.commit, session.finish
//   servidor: response.created, response.audio.delta (PCM base64),
//            response.done (por enunciado), session.finished, error
// Modos: tmServerCommit (el servidor decide cuando sintetizar; lo pendiente sale
// con Finish) y tmCommit (el cliente cierra cada enunciado con Commit; varias
// respuestas en la misma sesion).
// Voces propias (TAiQwenVoices): hay que crearlas para el modelo realtime
// (CloneModel := 'qwen3-tts-vc-realtime-2026-01-15'); el componente elige ese
// modelo por el prefijo del id.
//
// Todos los eventos se despachan con TThread.Queue: en consola hay que drenar la
// cola con CheckSynchronize.
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.QwenTTS;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  uMakerAi.Realtime, uMakerAi.Realtime.WebSocket, uMakerAi.WebSocket.Client;

type
  TAiQwenTTSMode = (tmServerCommit, tmCommit);

  TAiQwenRealtimeTTS = class(TComponent)
  private
    FWebSocket: TAiRealtimeWSClient;
    FConnectThread: TThread;
    FB64: TBase64Encoding;
    FConnected: Boolean;
    FSessionReady: Boolean;
    FApiKey, FUrl, FModel, FVoice, FLanguage, FInstructions: string;
    FMode: TAiQwenTTSMode;
    FSampleRate: Integer;
    FOnConnected, FOnDisconnected, FOnSessionReady, FOnResponseDone, FOnFinished: TNotifyEvent;
    FOnAudioChunk: TAiRealtimeAudioChunkEvent;
    FOnError: TAiRealtimeErrorEvent;
    function ResolvedApiKey: string;
    procedure SendJson(AObj: TJSONObject);
    procedure OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode; const Data: TBytes; IsFinal: Boolean);
    procedure OnWSConnected(Sender: TObject);
    procedure OnWSDisconnected(Sender: TObject);
    procedure OnWSError(Sender: TObject; const ErrorMsg: string);
    procedure Notify(AEvent: TNotifyEvent);
    procedure DoError(const AMsg, ACode: string);
  protected
    // Modelo efectivo (las voces propias exigen su modelo realtime)
    function EffectiveModel: string;
    // session.update completo (protegido: la suite lo inspecciona)
    function BuildSessionUpdate: TJSONObject;
    // Procesa un evento JSON del servidor (protegido: la suite lo alimenta sin red)
    procedure ProcessServerEvent(const JObj: TJSONObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    // Agrega texto al buffer; puede llamarse muchas veces con fragmentos
    procedure AppendText(const AText: string);
    // tmCommit: sintetiza lo acumulado como un enunciado (OnResponseDone al terminar)
    procedure Commit;
    // Atajo: AppendText + Commit (en tmServerCommit equivale a AppendText)
    procedure Speak(const AText: string);
    // Sintetiza lo pendiente y cierra la sesion en el servidor (OnFinished)
    procedure Finish;
    property IsConnected: Boolean read FConnected;
    property IsSessionReady: Boolean read FSessionReady;
  published
    // '@VARIABLE' lee la key de esa variable de entorno
    property ApiKey: string read FApiKey write FApiKey;
    property Url: string read FUrl write FUrl;
    // qwen3-tts-flash-realtime [default], qwen3-tts-instruct-flash-realtime,
    // qwen3-tts-vc-realtime-2026-01-15, qwen3-tts-vd-realtime-2026-01-15
    property Model: string read FModel write FModel;
    // Voz: Cherry, Ethan... o el id de una voz propia creada para el modelo realtime
    property Voice: string read FVoice write FVoice;
    // Idioma ('es' o 'Spanish'); vacio = Auto
    property Language: string read FLanguage write FLanguage;
    // Estilo de lectura (solo modelos instruct), p.ej. 'Habla rapido y con entusiasmo'
    property Instructions: string read FInstructions write FInstructions;
    property Mode: TAiQwenTTSMode read FMode write FMode default tmServerCommit;
    // Frecuencia del PCM de salida (default 24000)
    property SampleRate: Integer read FSampleRate write FSampleRate default 24000;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnSessionReady: TNotifyEvent read FOnSessionReady write FOnSessionReady;
    // PCM16 mono a SampleRate, a medida que se genera
    property OnAudioChunk: TAiRealtimeAudioChunkEvent read FOnAudioChunk write FOnAudioChunk;
    // Termino un enunciado
    property OnResponseDone: TNotifyEvent read FOnResponseDone write FOnResponseDone;
    // El servidor cerro la sesion tras Finish
    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
    property OnError: TAiRealtimeErrorEvent read FOnError write FOnError;
  end;

procedure Register;

implementation

const
  CQWENTTS_WSS = 'wss://dashscope-intl.aliyuncs.com/api-ws/v1/realtime';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiQwenRealtimeTTS]);
end;

{ TAiQwenRealtimeTTS }

constructor TAiQwenRealtimeTTS.Create(AOwner: TComponent);
begin
  inherited;
  FApiKey := '@DASHSCOPE_API_KEY';
  FUrl := CQWENTTS_WSS;
  FModel := 'qwen3-tts-flash-realtime';
  FVoice := 'Cherry';
  FMode := tmServerCommit;
  FSampleRate := 24000;
  FB64 := TBase64Encoding.Create(0);
  FWebSocket := TAiRealtimeWSClient.Create;
  FWebSocket.OnFrame := OnWSFrame;
  FWebSocket.OnConnected := OnWSConnected;
  FWebSocket.OnDisconnected := OnWSDisconnected;
  FWebSocket.OnError := OnWSError;
end;

destructor TAiQwenRealtimeTTS.Destroy;
begin
  if FConnected then
    Disconnect;
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

function TAiQwenRealtimeTTS.ResolvedApiKey: string;
begin
  if FApiKey.StartsWith('@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

function TAiQwenRealtimeTTS.EffectiveModel: string;
begin
  Result := FModel;
  if Result = '' then
    Result := 'qwen3-tts-flash-realtime';
  if FVoice.StartsWith('qwen-tts-vc-') and not Result.Contains('tts-vc') then
    Result := 'qwen3-tts-vc-realtime-2026-01-15'
  else if FVoice.StartsWith('qwen-tts-vd-') and not Result.Contains('tts-vd') then
    Result := 'qwen3-tts-vd-realtime-2026-01-15';
end;

function TAiQwenRealtimeTTS.BuildSessionUpdate: TJSONObject;
const
  Codes: array[0..9] of string = ('es', 'en', 'zh', 'fr', 'de', 'it', 'pt', 'ja', 'ko', 'ru');
  Names: array[0..9] of string = ('Spanish', 'English', 'Chinese', 'French', 'German', 'Italian',
    'Portuguese', 'Japanese', 'Korean', 'Russian');
var
  jSess: TJSONObject;
  LLang: string;
  I: Integer;
begin
  jSess := TJSONObject.Create;
  if FVoice <> '' then
    jSess.AddPair('voice', FVoice);
  if FMode = tmCommit then
    jSess.AddPair('mode', 'commit')
  else
    jSess.AddPair('mode', 'server_commit');
  jSess.AddPair('response_format', 'pcm');
  jSess.AddPair('sample_rate', TJSONNumber.Create(FSampleRate));
  LLang := FLanguage;
  for I := Low(Codes) to High(Codes) do
    if SameText(LLang, Codes[I]) then
      LLang := Names[I];
  if LLang <> '' then
    jSess.AddPair('language_type', LLang);
  if FInstructions <> '' then
    jSess.AddPair('instructions', FInstructions);
  Result := TJSONObject.Create;
  Result.AddPair('type', 'session.update');
  Result.AddPair('session', jSess);
end;

procedure TAiQwenRealtimeTTS.Notify(AEvent: TNotifyEvent);
begin
  if Assigned(AEvent) then
    TThread.Queue(nil, procedure begin AEvent(Self); end);
end;

procedure TAiQwenRealtimeTTS.DoError(const AMsg, ACode: string);
begin
  if Assigned(FOnError) then
    TThread.Queue(nil, procedure begin
      if Assigned(FOnError) then FOnError(Self, AMsg, ACode);
    end);
end;

procedure TAiQwenRealtimeTTS.SendJson(AObj: TJSONObject);
begin
  try
    FWebSocket.SendText(AObj.ToJSON);
  finally
    AObj.Free;
  end;
end;

procedure TAiQwenRealtimeTTS.ProcessServerEvent(const JObj: TJSONObject);
var
  EventType, B64, ErrMsg, ErrCode: string;
  LData: TBytes;
  JError: TJSONObject;
begin
  if not JObj.TryGetValue<string>('type', EventType) then Exit;

  if EventType = 'session.created' then
  begin
    FSessionReady := False;
    if FConnected then
      SendJson(BuildSessionUpdate);
  end
  else if EventType = 'session.updated' then
  begin
    if not FSessionReady then
    begin
      FSessionReady := True;
      Notify(FOnSessionReady);
    end;
  end
  else if EventType = 'response.audio.delta' then
  begin
    B64 := JObj.GetValue<string>('delta', '');
    if (B64 <> '') and Assigned(FOnAudioChunk) then
    begin
      LData := TNetEncoding.Base64.DecodeStringToBytes(B64);
      TThread.Queue(nil, procedure begin
        if Assigned(FOnAudioChunk) then FOnAudioChunk(Self, LData);
      end);
    end;
  end
  else if EventType = 'response.done' then
    Notify(FOnResponseDone)
  else if EventType = 'session.finished' then
    Notify(FOnFinished)
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

procedure TAiQwenRealtimeTTS.OnWSFrame(Sender: TObject; Opcode: TAiRealtimeWSOpcode;
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

procedure TAiQwenRealtimeTTS.OnWSConnected(Sender: TObject);
begin
  FConnected := True;
  Notify(FOnConnected);
end;

procedure TAiQwenRealtimeTTS.OnWSDisconnected(Sender: TObject);
begin
  FConnected := False;
  FSessionReady := False;
  Notify(FOnDisconnected);
end;

procedure TAiQwenRealtimeTTS.OnWSError(Sender: TObject; const ErrorMsg: string);
begin
  DoError(ErrorMsg, 'websocket_error');
end;

procedure TAiQwenRealtimeTTS.Connect;
var
  LUrl: string;
begin
  if FConnected then Exit;
  if Assigned(FConnectThread) then
  begin
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FWebSocket.ExtraHeaders.Values['Authorization'] := 'Bearer ' + ResolvedApiKey;
  LUrl := FUrl + '?model=' + EffectiveModel;
  FConnectThread := TThread.CreateAnonymousThread(procedure begin
    if not FWebSocket.Connect(LUrl) then
      DoError('No se pudo conectar al TTS realtime de Qwen', 'connection_failed');
  end);
  FConnectThread.FreeOnTerminate := False;
  FConnectThread.Start;
end;

procedure TAiQwenRealtimeTTS.Disconnect;
begin
  if FConnected then
    FWebSocket.SendClose(1000, '');
  FWebSocket.Disconnect;
  if Assigned(FConnectThread) then
  begin
    FConnectThread.WaitFor;
    FreeAndNil(FConnectThread);
  end;
  FConnected := False;
  FSessionReady := False;
end;

procedure TAiQwenRealtimeTTS.AppendText(const AText: string);
var
  J: TJSONObject;
begin
  if not FConnected or (AText = '') then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'input_text_buffer.append');
  J.AddPair('text', AText);
  SendJson(J);
end;

procedure TAiQwenRealtimeTTS.Commit;
var
  J: TJSONObject;
begin
  if not FConnected then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'input_text_buffer.commit');
  SendJson(J);
end;

procedure TAiQwenRealtimeTTS.Speak(const AText: string);
begin
  AppendText(AText);
  if FMode = tmCommit then
    Commit;
end;

procedure TAiQwenRealtimeTTS.Finish;
var
  J: TJSONObject;
begin
  if not FConnected then Exit;
  J := TJSONObject.Create;
  J.AddPair('type', 'session.finish');
  SendJson(J);
end;

end.
