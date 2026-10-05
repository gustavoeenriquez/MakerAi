unit uMainGPTLive;

// MakerAI — Demo 092: OpenAI GPT-Live (voz full-duplex con delegacion)
//
// Conversacion de voz con TAiOpenAiLiveChat: el modelo escucha mientras habla
// (se le puede interrumpir) y delega el razonamiento:
//   - "Responses (OpenAI)": un modelo de la API Responses gestionado por
//     OpenAI, con busqueda web opcional y las funciones locales de este demo.
//   - "Mi chat (DelegateChat)": cualquier TAiChatConnection (Driver/Modelo de
//     la pantalla) resuelve la tarea con las mismas funciones; el driver le
//     pasa la transcripcion y dice la respuesta.
//
// Funciones de ejemplo (TAiFunctions): hora_actual(zona) y luz(habitacion,
// encendida), que cambia el indicador de la pantalla.
//
// RAG por MCP (opcional): el servidor del demo 037 (MCPServerRAG, HTTP) se
// agrega como cliente MCP del mismo TAiFunctions, asi que su herramienta
// rag_vector llega al modelo junto con las funciones locales. Un
// TAiGuardrails deja pasar solo las consultas (search, list_docs, stats): el
// modelo de voz no puede borrar ni indexar. Ver CLAUDE.md para preparar el 037
// con PostgreSQL y el documento de ejemplo conocimiento_cafe_la_ceiba.txt.
//
// Audio: TAiAudioCapture (microfono, PCM16 24 kHz) -> driver -> TAiAudioPlayer.
// Con parlantes el microfono oye al asistente: "Modo altavoz" silencia el
// microfono mientras suena su voz (detectada por la energia del audio que
// llega). Con auriculares dejarlo apagado: asi se le puede interrumpir.
//
// Requiere OPENAI_API_KEY (y la key del proveedor elegido para DelegateChat).
// Costo: US$0.05 por minuto de sesion mas los tokens del modelo delegado.

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.Diagnostics, System.DateUtils, System.StrUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.Layouts, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Objects,
  uMakerAi.Core, uMakerAi.Chat.Messages, uMakerAi.Tools.Functions,
  uMakerAi.Chat.AiConnection, uMakerAi.Chat.Initializations,
  uMakerAi.Realtime, uMakerAi.Realtime.OpenAI.Live,
  uMakerAi.Utils.AudioCapture, uMakerAi.Utils.AudioPlayback,
  uMakerAi.MCPClient.Core, uMakerAi.Guardrails;

type
  TFormGPTLive = class(TForm)
    LayConfig: TLayout;
    LayRow1: TLayout;
    LblMic: TLabel;
    CbxMic: TComboBox;
    LblSpeaker: TLabel;
    CbxSpeaker: TComboBox;
    LblVoice: TLabel;
    CbxVoice: TComboBox;
    LayRow2: TLayout;
    LblDelegation: TLabel;
    CbxDelegation: TComboBox;
    LblChatDriver: TLabel;
    EdChatDriver: TEdit;
    LblChatModel: TLabel;
    EdChatModel: TEdit;
    ChkWebSearch: TCheckBox;
    LayRow3: TLayout;
    LblInstructions: TLabel;
    EdInstructions: TEdit;
    ChkSpeakerMode: TCheckBox;
    LayRow4: TLayout;
    ChkRag: TCheckBox;
    LblRagUrl: TLabel;
    EdRagUrl: TEdit;
    LblRagHint: TLabel;
    LayButtons: TLayout;
    BtnConnect: TButton;
    BtnMute: TButton;
    PbMicLevel: TProgressBar;
    CircleLight: TCircle;
    LblLight: TLabel;
    LblStatus: TLabel;
    LayGuide: TLayout;
    LblGuide: TLabel;
    MemoGuide: TMemo;
    LayLive: TLayout;
    LblLiveUser: TLabel;
    LblLiveAssistant: TLabel;
    MemoConversation: TMemo;
    Splitter1: TSplitter;
    MemoLog: TMemo;
    LayStatusBar: TLayout;
    LblUsage: TLabel;
    TimerSpeaker: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnMuteClick(Sender: TObject);
    procedure TimerSpeakerTimer(Sender: TObject);
  private
    FLive: TAiOpenAiLiveChat;
    FCapture: TAiAudioCapture;
    FPlayer: TAiAudioPlayer;
    FFunctions: TAiFunctions;
    FGuard: TAiGuardrails;
    FChat: TAiChatConnection;
    FMicDevices: TArray<TAiAudioDeviceInfo>;
    FSpeakerDevices: TArray<TAiAudioDeviceInfo>;
    FClock: TStopwatch;
    FConnected: Boolean;
    FMuted: Boolean;
    FLiveUser: string;
    FLiveAssistant: string;
    FLastUserMs: Int64;      // ultimo texto del usuario (para la latencia)
    FWaitingAnswer: Boolean; // el usuario hablo y el asistente aun no
    FLastVoiceOutMs: Int64;  // ultima vez que la salida tuvo voz (modo altavoz)
    FHeardWhileSpeaking: string; // lo que el modelo oyo mientras hablaba
    procedure Log(const S: string);
    procedure LoadDevices;
    procedure SetConnectedUI(AConnected: Boolean);
    procedure SetupFunctions;
    procedure FillGuide;
    function SetupRag: Boolean;
    procedure SetLight(const ARoom: string; AOn: Boolean);
    // Guardrail: el modelo de voz solo puede consultar el RAG
    procedure GuardCheck(Sender: TObject; const AToolName, AArguments: string;
      var AAllow: Boolean; var AReason: string);
    // Eventos del driver
    procedure LiveConnected(Sender: TObject);
    procedure LiveDisconnected(Sender: TObject);
    procedure LiveSessionReady(Sender: TObject);
    procedure LiveUserDelta(Sender: TObject; const Delta: string);
    procedure LiveUserDone(Sender: TObject; const Transcript, ItemId: string);
    procedure LiveAssistantDelta(Sender: TObject; const AText: string);
    procedure LiveAssistantDone(Sender: TObject; const AText: string);
    procedure LiveAudio(Sender: TObject; const AData: TBytes);
    procedure LiveError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure LiveUsage(Sender: TObject; Seconds, ContextRatio: Double);
    procedure LiveClosed(Sender: TObject; const Reason: string; Seconds: Double);
    procedure LiveDelegation(Sender: TObject; const DelegationId, Context: string);
    procedure LiveResponseEvent(Sender: TObject; const DelegationId, EventType, EventJson: string);
    // Audio
    procedure CaptureLevel(Sender: TObject; const aSoundLevel: Int64);
    procedure CaptureError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerError(Sender: TObject; const ErrorMessage: string);
    // Funciones locales (corren en un hilo del driver)
    procedure FuncHoraActual(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FuncLuz(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  end;

var
  FormGPTLive: TFormGPTLive;

implementation

{$R *.fmx}

const
  // Promedio absoluto (PCM16) por encima del cual la salida "tiene voz"
  CVOICE_OUT_LEVEL = 300;
  // Margen tras la ultima voz del asistente: buffer del reproductor + cola
  CSPEAKER_TAIL_MS = 800;
  CPRICE_PER_MINUTE = 0.05;
  CVOICES: array[0..9] of string = ('marin', 'cedar', 'alloy', 'ash', 'ballad',
    'coral', 'echo', 'sage', 'shimmer', 'verse');
  // Credenciales del servidor del demo 037 (quemadas en su uTool.RAG.pas)
  CRAG_TOKEN = 'admin:MakerAi2026*';
  CRAG_LIVE_INSTRUCTIONS = ' Para preguntas sobre el Cafe La Ceiba (horarios, ' +
    'precios, wifi, mascotas, eventos, politicas) consulta la base de conocimiento.';
  CRAG_BACKEND_INSTRUCTIONS = 'Para preguntas sobre el Cafe La Ceiba usa la ' +
    'herramienta rag_vector con operation=search y topK=3, y responde solo con lo ' +
    'que diga el resultado; si no aparece, dilo.';

{ TFormGPTLive }

procedure TFormGPTLive.FormCreate(Sender: TObject);
var
  V: string;
begin
  FClock := TStopwatch.StartNew;

  for V in CVOICES do
    CbxVoice.Items.Add(V);
  CbxVoice.ItemIndex := 0;
  CbxDelegation.Items.Add('Responses (OpenAI)');
  CbxDelegation.Items.Add('Mi chat (DelegateChat)');
  CbxDelegation.ItemIndex := 0;
  EdChatDriver.Text := 'OpenAi';
  EdChatModel.Text := 'gpt-6-luna';
  EdInstructions.Text := 'Habla en español, en frases cortas y naturales. ' +
    'Para la hora o las luces de la casa usa las herramientas.';
  EdRagUrl.Text := 'http://localhost:8093/mcp';

  FLive := TAiOpenAiLiveChat.Create(Self);
  FLive.OnConnected := LiveConnected;
  FLive.OnDisconnected := LiveDisconnected;
  FLive.OnSessionReady := LiveSessionReady;
  FLive.OnTranscriptDelta := LiveUserDelta;
  FLive.OnTranscriptCompleted := LiveUserDone;
  FLive.OnAssistantTextDelta := LiveAssistantDelta;
  FLive.OnAssistantText := LiveAssistantDone;
  FLive.OnAudioChunk := LiveAudio;
  FLive.OnError := LiveError;
  FLive.OnUsage := LiveUsage;
  FLive.OnSessionClosed := LiveClosed;
  FLive.OnDelegation := LiveDelegation;
  FLive.OnResponseEvent := LiveResponseEvent;

  FCapture := TAiAudioCapture.Create(Self);
  FCapture.Source := asMicrophone;
  FCapture.OutputSampleRate := 24000; // lo que espera GPT-Live: sin remuestreo
  FCapture.OutputChannels := 1;
  FCapture.OnUpdate := CaptureLevel;
  FCapture.OnError := CaptureError;

  FPlayer := TAiAudioPlayer.Create(Self);
  FPlayer.OnError := PlayerError;

  SetupFunctions;
  FillGuide;
  LoadDevices;
  SetLight('', False);
  SetConnectedUI(False);
  Log('Listo. Con parlantes active "Modo altavoz"; con auriculares, '
    + 'déjelo apagado para poder interrumpir al asistente.');
end;

procedure TFormGPTLive.FormDestroy(Sender: TObject);
begin
  TimerSpeaker.Enabled := False;
  if FConnected then
  begin
    FCapture.Active := False;
    FLive.Disconnect;
  end;
  FPlayer.Active := False;
end;

procedure TFormGPTLive.Log(const S: string);
begin
  MemoLog.Lines.Add(Format('[%6.1f s] %s', [FClock.ElapsedMilliseconds / 1000, S]));
  MemoLog.GoToTextEnd;
end;

procedure TFormGPTLive.LoadDevices;
var
  D: TAiAudioDeviceInfo;
  I: Integer;
begin
  CbxMic.Items.Clear;
  CbxSpeaker.Items.Clear;
  CbxMic.Items.Add('(predeterminado)');
  CbxSpeaker.Items.Add('(predeterminado)');
  try
    FMicDevices := TAiAudioCapture.GetAudioDevices(asMicrophone);
    FSpeakerDevices := TAiAudioPlayer.GetPlaybackDevices;
  except
    on E: Exception do
      Log('No se pudieron listar los dispositivos: ' + E.Message);
  end;
  for D in FMicDevices do
    CbxMic.Items.Add(D.DeviceName);
  for D in FSpeakerDevices do
    CbxSpeaker.Items.Add(D.DeviceName);
  CbxMic.ItemIndex := 0;
  CbxSpeaker.ItemIndex := 0;
  // Preseleccionar los predeterminados por nombre (para que se vean)
  for I := 0 to High(FMicDevices) do
    if FMicDevices[I].IsDefault then
      CbxMic.ItemIndex := I + 1;
  for I := 0 to High(FSpeakerDevices) do
    if FSpeakerDevices[I].IsDefault then
      CbxSpeaker.ItemIndex := I + 1;
end;

procedure TFormGPTLive.SetupFunctions;
var
  Fn: TFunctionActionItem;
  P: TFunctionParamsItem;
begin
  FFunctions := TAiFunctions.Create(Self);
  // Todas las llamadas (locales y MCP) pasan por el guardrail
  FGuard := TAiGuardrails.Create(Self);
  FGuard.OnCheckToolCall := GuardCheck;
  FFunctions.Guardrails := FGuard;

  Fn := FFunctions.Functions.Add;
  Fn.FunctionName := 'hora_actual';
  Fn.Enabled := True;
  Fn.Description.Text := 'Devuelve la fecha y la hora actual en una zona horaria.';
  Fn.OnAction := FuncHoraActual;
  P := Fn.Parameters.Add;
  P.Name := 'zona';
  P.ParamType := ptString;
  P.Description.Text := 'Zona horaria IANA, por ejemplo America/Bogota o Europe/Madrid.';
  P.Required := True;

  Fn := FFunctions.Functions.Add;
  Fn.FunctionName := 'luz';
  Fn.Enabled := True;
  Fn.Description.Text := 'Enciende o apaga la luz de una habitacion de la casa.';
  Fn.OnAction := FuncLuz;
  P := Fn.Parameters.Add;
  P.Name := 'habitacion';
  P.ParamType := ptString;
  P.Description.Text := 'Habitacion: sala, cocina, estudio o dormitorio.';
  P.Required := True;
  P := Fn.Parameters.Add;
  P.Name := 'encendida';
  P.ParamType := ptBoolean;
  P.Description.Text := 'true para encender, false para apagar.';
  P.Required := True;
end;

procedure TFormGPTLive.FillGuide;
begin
  // Preguntas de ejemplo para recorrer lo que muestra el demo
  MemoGuide.Lines.Text :=
    'CONVERSACIÓN' + sLineBreak +
    '• Hola, ¿cómo estás?' + sLineBreak +
    '• Explícame en dos frases qué es la fotosíntesis.' + sLineBreak +
    sLineBreak +
    'INTERRUMPIR (con auriculares)' + sLineBreak +
    '• Cuéntame un cuento largo sobre un dragón.' + sLineBreak +
    '  ...y a mitad: "Espera, mejor que sea sobre un gato".' + sLineBreak +
    sLineBreak +
    'FUNCIONES LOCALES' + sLineBreak +
    '• ¿Qué hora es en Madrid?' + sLineBreak +
    '• Enciende la luz de la sala.' + sLineBreak +
    '• Apaga la luz.' + sLineBreak +
    sLineBreak +
    'RAG POR MCP (marcar "RAG del demo 037")' + sLineBreak +
    '• ¿Cuál es la contraseña del wifi del Café La Ceiba?' + sLineBreak +
    '• ¿A qué hora abren los sábados?' + sLineBreak +
    '• ¿Cuánto cuesta el capuchino?' + sLineBreak +
    '• ¿Puedo llevar a mi perro?' + sLineBreak +
    '• ¿Qué hay los jueves por la tarde?' + sLineBreak +
    '• ¿Cuánto cuesta la cata de café y cuántos cupos hay?' + sLineBreak +
    sLineBreak +
    'GUARDRAIL (debe negarse)' + sLineBreak +
    '• Borra toda la base de conocimiento.' + sLineBreak +
    '  El log muestra "Guardrail BLOQUEÓ".' + sLineBreak +
    sLineBreak +
    'BÚSQUEDA WEB (marcar "Búsqueda web")' + sLineBreak +
    '• ¿Qué noticias hay hoy de tecnología?' + sLineBreak +
    sLineBreak +
    'DELEGAR A TU CHAT' + sLineBreak +
    'Elegir "Mi chat (DelegateChat)" y repetir las de funciones o RAG: ' +
    'responde el modelo de Driver/Modelo. El log muestra la delegación.';
end;

function TFormGPTLive.SetupRag: Boolean;
var
  Item: TMCPClientItem;
begin
  // Se arma en cada conexion: la URL pudo cambiar
  FFunctions.MCPClients.Clear;
  if not ChkRag.IsChecked then
    Exit(True);
  Item := FFunctions.MCPClients.Add;
  Item.Name := 'cafe'; // la herramienta llega al modelo como cafe_99_rag_vector
  Item.TransportType := tpHttp;
  Item.Params.Values['URL'] := Trim(EdRagUrl.Text);
  Item.Params.Values['ApiBearerToken'] := CRAG_TOKEN;
  Item.Enabled := True;
  // Comprobar ahora: si el 037 no esta corriendo, avisar antes de conectar
  Result := Item.MCPClient.Initialize and Item.MCPClient.Available;
  if Result then
    Log('RAG por MCP listo: ' + Item.Params.Values['URL'])
  else
  begin
    Log('No se pudo conectar al servidor RAG (' + Item.Params.Values['URL'] +
      '). ¿Está corriendo el demo 037 con --protocol http --port 8093?');
    FFunctions.MCPClients.Clear;
  end;
end;

procedure TFormGPTLive.GuardCheck(Sender: TObject; const AToolName, AArguments: string;
  var AAllow: Boolean; var AReason: string);
var
  Args, Msg: string;
begin
  if Pos('rag_vector', AToolName) = 0 then
    Exit; // funciones locales: sin restriccion
  Args := StringReplace(AArguments, ' ', '', [rfReplaceAll]);
  AAllow := (Pos('"operation":"search"', Args) > 0) or
    (Pos('"operation":"list_docs"', Args) > 0) or (Pos('"operation":"stats"', Args) > 0);
  if not AAllow then
    AReason := 'el asistente de voz solo puede consultar (search, list_docs, stats)';
  // Los parametros var no se pueden capturar: el mensaje se arma antes
  if AAllow then
    Msg := 'RAG: ' + AArguments
  else
    Msg := 'Guardrail BLOQUEÓ: ' + AArguments;
  TThread.Queue(nil,
    procedure
    begin
      Log(Msg);
    end);
end;

procedure TFormGPTLive.SetLight(const ARoom: string; AOn: Boolean);
begin
  if AOn then
    CircleLight.Fill.Color := TAlphaColors.Gold
  else
    CircleLight.Fill.Color := TAlphaColors.Dimgray;
  if ARoom = '' then
    LblLight.Text := 'Luz: apagada'
  else
    LblLight.Text := 'Luz ' + ARoom + ': ' + IfThen(AOn, 'encendida', 'apagada');
end;

procedure TFormGPTLive.SetConnectedUI(AConnected: Boolean);
begin
  FConnected := AConnected;
  BtnConnect.Text := IfThen(AConnected, 'Desconectar', 'Conectar');
  BtnMute.Enabled := AConnected;
  // Voz, audio e instrucciones no cambian con la sesion abierta
  CbxMic.Enabled := not AConnected;
  CbxSpeaker.Enabled := not AConnected;
  CbxVoice.Enabled := not AConnected;
  CbxDelegation.Enabled := not AConnected;
  EdChatDriver.Enabled := not AConnected;
  EdChatModel.Enabled := not AConnected;
  ChkWebSearch.Enabled := not AConnected;
  EdInstructions.Enabled := not AConnected;
  ChkRag.Enabled := not AConnected;
  EdRagUrl.Enabled := not AConnected;
  if not AConnected then
  begin
    FMuted := False;
    BtnMute.Text := 'Silenciar micrófono';
  end;
end;

procedure TFormGPTLive.BtnConnectClick(Sender: TObject);
begin
  if FConnected then
  begin
    Log('Desconectando...');
    TimerSpeaker.Enabled := False;
    FCapture.Active := False;
    FLive.Disconnect; // espera session.closed (motivo y consumo final)
    FPlayer.ClearQueue;
    FPlayer.Active := False;
    SetConnectedUI(False);
    LblStatus.Text := 'Desconectado';
    Exit;
  end;

  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Log('Falta la variable de entorno OPENAI_API_KEY');
    Exit;
  end;

  if not SetupRag then
    Exit;
  FLive.Voice := CbxVoice.Selected.Text;
  FLive.Instructions := EdInstructions.Text;
  FLive.DelegationInstructions := '';
  if ChkRag.IsChecked then
  begin
    // El modelo de voz decide cuando delegar; el delegado, como usar la herramienta
    FLive.Instructions := FLive.Instructions + CRAG_LIVE_INSTRUCTIONS;
    FLive.DelegationInstructions := CRAG_BACKEND_INSTRUCTIONS;
  end;
  FLive.EnableWebSearch := ChkWebSearch.IsChecked;
  FLive.AiFunctions := FFunctions;

  FreeAndNil(FChat);
  if CbxDelegation.ItemIndex = 1 then
  begin
    // El chat delegado tiene las mismas funciones; el driver le pasa la
    // transcripcion y dice su respuesta
    FChat := TAiChatConnection.Create(Self);
    FChat.DriverName := Trim(EdChatDriver.Text);
    if Trim(EdChatModel.Text) <> '' then
      FChat.Model := Trim(EdChatModel.Text);
    FChat.AiFunctions := FFunctions;
    FChat.SystemPrompt.Text := 'Eres el backend de un asistente de voz. ' +
      'Responde con el resultado, breve y sin formato, en el idioma del usuario.';
    if ChkRag.IsChecked then
      FChat.SystemPrompt.Add(CRAG_BACKEND_INSTRUCTIONS);
    FLive.DelegateChat := FChat;
    Log(Format('Delegación: DelegateChat (%s / %s)', [FChat.DriverName, FChat.Model]));
  end
  else
  begin
    FLive.DelegateChat := nil;
    Log('Delegación: Responses (' + FLive.DelegationModel + ')');
  end;

  // Reproductor y captura (la captura se activa al llegar session.started)
  if CbxSpeaker.ItemIndex > 0 then
    FPlayer.DeviceId := FSpeakerDevices[CbxSpeaker.ItemIndex - 1].EndpointId
  else
    FPlayer.DeviceId := '';
  FPlayer.Active := True;
  if CbxMic.ItemIndex > 0 then
    FCapture.DeviceId := FMicDevices[CbxMic.ItemIndex - 1].EndpointId
  else
    FCapture.DeviceId := '';
  FCapture.RealtimeSTT := FLive;

  FLiveUser := '';
  FLiveAssistant := '';
  FWaitingAnswer := False;
  LblLiveUser.Text := '';
  LblLiveAssistant.Text := '';
  LblStatus.Text := 'Conectando...';
  SetConnectedUI(True);
  TimerSpeaker.Enabled := True;
  FLive.Connect;
end;

procedure TFormGPTLive.BtnMuteClick(Sender: TObject);
begin
  // Silencio del lado del servidor: el modelo deja de recibir el microfono
  FMuted := not FMuted;
  if FMuted then
    FLive.Mute
  else
    FLive.Unmute;
  BtnMute.Text := IfThen(FMuted, 'Activar micrófono', 'Silenciar micrófono');
  Log(IfThen(FMuted, 'Micrófono silenciado (session.input_audio.mute)', 'Micrófono activo'));
end;

procedure TFormGPTLive.TimerSpeakerTimer(Sender: TObject);
var
  Speaking: Boolean;
begin
  // Modo altavoz: silenciar el microfono mientras suena la voz del asistente
  // para que no se escuche a si mismo (Muted manda silencio, no corta el flujo)
  Speaking := FClock.ElapsedMilliseconds - FLastVoiceOutMs < CSPEAKER_TAIL_MS;
  FCapture.Muted := ChkSpeakerMode.IsChecked and Speaking;
  if FConnected then
    LblStatus.Text := IfThen(Speaking, 'Asistente hablando', 'Escuchando') +
      IfThen(FCapture.Muted, ' (micrófono en pausa)', '');
end;

// -----------------------------------------------------------------------------
// Eventos del driver (llegan en el hilo principal)
// -----------------------------------------------------------------------------

procedure TFormGPTLive.LiveConnected(Sender: TObject);
begin
  Log('WebSocket conectado; enviando session.start');
end;

procedure TFormGPTLive.LiveDisconnected(Sender: TObject);
begin
  Log('WebSocket cerrado');
  if FConnected then
  begin
    // Cierre del lado del servidor (expiro, error de red...)
    TimerSpeaker.Enabled := False;
    FCapture.Active := False;
    FPlayer.Active := False;
    SetConnectedUI(False);
    LblStatus.Text := 'Desconectado';
  end;
end;

procedure TFormGPTLive.LiveSessionReady(Sender: TObject);
begin
  Log('Sesión iniciada: ' + FLive.SessionId + ' — hable cuando quiera');
  FCapture.Active := True;
  LblStatus.Text := 'Escuchando';
end;

procedure TFormGPTLive.LiveUserDelta(Sender: TObject; const Delta: string);
begin
  // Full-duplex: lo que el modelo oye mientras habla puede interrumpirlo
  // (una tos, un "mm", o su propia voz si el microfono la capta)
  if FClock.ElapsedMilliseconds - FLastVoiceOutMs < CSPEAKER_TAIL_MS then
    FHeardWhileSpeaking := FHeardWhileSpeaking + Delta
  else if FHeardWhileSpeaking <> '' then
  begin
    Log('El modelo te escuchó mientras hablaba: "' + Trim(FHeardWhileSpeaking) +
      '" (si no fuiste tú, es eco: baja el volumen o activa "Modo altavoz")');
    FHeardWhileSpeaking := '';
  end;
  FLiveUser := FLiveUser + Delta;
  LblLiveUser.Text := 'Tú: ' + Trim(FLiveUser);
  FLastUserMs := FClock.ElapsedMilliseconds;
  FWaitingAnswer := True;
end;

procedure TFormGPTLive.LiveUserDone(Sender: TObject; const Transcript, ItemId: string);
begin
  MemoConversation.Lines.Add('Tú: ' + Transcript);
  MemoConversation.GoToTextEnd;
  FLiveUser := '';
  LblLiveUser.Text := '';
end;

procedure TFormGPTLive.LiveAssistantDelta(Sender: TObject; const AText: string);
begin
  if FWaitingAnswer then
  begin
    // Aproximada: incluye el retraso de la transcripcion del usuario
    Log(Format('Primer texto del asistente %d ms después del último texto del usuario',
      [FClock.ElapsedMilliseconds - FLastUserMs]));
    FWaitingAnswer := False;
  end;
  FLiveAssistant := FLiveAssistant + AText;
  LblLiveAssistant.Text := 'Asistente: ' + Trim(FLiveAssistant);
end;

procedure TFormGPTLive.LiveAssistantDone(Sender: TObject; const AText: string);
begin
  if FHeardWhileSpeaking <> '' then
  begin
    Log('El modelo te escuchó mientras hablaba: "' + Trim(FHeardWhileSpeaking) +
      '" (si no fuiste tú, es eco: baja el volumen o activa "Modo altavoz")');
    FHeardWhileSpeaking := '';
  end;
  MemoConversation.Lines.Add('Asistente: ' + AText);
  MemoConversation.GoToTextEnd;
  FLiveAssistant := '';
  LblLiveAssistant.Text := '';
end;

procedure TFormGPTLive.LiveAudio(Sender: TObject; const AData: TBytes);
var
  I, N: Integer;
  Sum: Int64;
begin
  // El servidor manda audio continuo (con silencio) a ritmo real
  FPlayer.PlayPCM16(AData, FLive.TargetSampleRate, 1);
  N := Length(AData) div 2;
  if N = 0 then Exit;
  Sum := 0;
  for I := 0 to N - 1 do
    Inc(Sum, Abs(PSmallInt(@AData[I * 2])^));
  if Sum div N > CVOICE_OUT_LEVEL then
    FLastVoiceOutMs := FClock.ElapsedMilliseconds;
end;

procedure TFormGPTLive.LiveError(Sender: TObject; const ErrorMsg, ErrorCode: string);
begin
  Log('ERROR [' + ErrorCode + '] ' + ErrorMsg);
end;

procedure TFormGPTLive.LiveUsage(Sender: TObject; Seconds, ContextRatio: Double);
begin
  LblUsage.Text := Format('Sesión: %.0f s — US$%.3f de voz — contexto %.1f%%',
    [Seconds, Seconds / 60 * CPRICE_PER_MINUTE, Max(ContextRatio, 0) * 100]);
end;

procedure TFormGPTLive.LiveClosed(Sender: TObject; const Reason: string; Seconds: Double);
begin
  Log(Format('Sesión cerrada (%s): %.0f s, US$%.3f de voz',
    [Reason, Seconds, Seconds / 60 * CPRICE_PER_MINUTE]));
end;

procedure TFormGPTLive.LiveDelegation(Sender: TObject; const DelegationId, Context: string);
begin
  Log('Delegación ' + DelegationId + ' a DelegateChat: ' +
    StringReplace(Context, sLineBreak, ' | ', [rfReplaceAll]));
end;

procedure TFormGPTLive.LiveResponseEvent(Sender: TObject; const DelegationId, EventType, EventJson: string);
begin
  // Solo los hitos del modelo delegado (sin los deltas)
  if (EventType = 'response.created') or (EventType = 'response.completed') or
     (EventType = 'response.failed') then
    Log('Responses: ' + EventType);
end;

// -----------------------------------------------------------------------------
// Audio
// -----------------------------------------------------------------------------

procedure TFormGPTLive.CaptureLevel(Sender: TObject; const aSoundLevel: Int64);
begin
  PbMicLevel.Value := Min(100, aSoundLevel * 100 div 3000);
end;

procedure TFormGPTLive.CaptureError(Sender: TObject; const ErrorMessage: string);
begin
  Log('Micrófono: ' + ErrorMessage);
end;

procedure TFormGPTLive.PlayerError(Sender: TObject; const ErrorMessage: string);
begin
  Log('Parlante: ' + ErrorMessage);
end;

// -----------------------------------------------------------------------------
// Funciones locales: corren en un hilo del driver; la UI va por TThread.Queue
// -----------------------------------------------------------------------------

procedure TFormGPTLive.FuncHoraActual(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Zona: string;
begin
  Zona := ToolCall.Params.Values['zona'];
  // Demo: hora local del equipo; un servicio real convertiria a la zona pedida
  ToolCall.Response := Format('{"zona":"%s","hora_local_del_equipo":"%s","nota":"%s"}',
    [Zona, FormatDateTime('yyyy-mm-dd hh:nn', Now),
     'hora del equipo donde corre el demo']);
  Handled := True;
  TThread.Queue(nil,
    procedure
    begin
      Log('Función hora_actual(' + Zona + ')');
    end);
end;

procedure TFormGPTLive.FuncLuz(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Room: string;
  IsOn: Boolean;
begin
  Room := ToolCall.Params.Values['habitacion'];
  IsOn := SameText(ToolCall.Params.Values['encendida'], 'true');
  ToolCall.Response := Format('{"habitacion":"%s","encendida":%s}',
    [Room, IfThen(IsOn, 'true', 'false')]);
  Handled := True;
  TThread.Queue(nil,
    procedure
    begin
      SetLight(Room, IsOn);
      Log(Format('Función luz(%s, %s)', [Room, IfThen(IsOn, 'encendida', 'apagada')]));
    end);
end;

end.
