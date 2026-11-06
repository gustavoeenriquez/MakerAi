unit uFrmVoiceChat;

// =============================================================================
// MakerAI — Demo FMX Voice Chat (STT + LLM + TTS)
// Conversacion de voz completa con MakerAI Realtime API.
//
// Componentes: TAiMakerAiRealtimeChat + TAIVoiceMonitor + TAudioPushStream
//
// Flujo:
//   1. TAIVoiceMonitor captura audio del microfono (PCM16 44100 Hz)
//   2. TAiMakerAiRealtimeChat resamplea a 24 kHz y envia por WebSocket
//   3. Servidor transcribe (STT) y muestra en "Tu voz"
//   4. LLM genera respuesta → OnAssistantText
//   5. TTS devuelve audio PCM16 → OnAudioChunk → TAudioPushStream reproduce
//
// Requiere: variable de entorno MAKERAI_API_KEY configurada.
// =============================================================================

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Memo.Types,
  FMX.Edit,
  FMX.Layouts,
  FMX.Objects,
  FMX.ListBox,
  uMakerAi.Realtime,
  uMakerAi.Realtime.MakerAi,
  uMakerAi.Utils.VoiceMonitor,
  uMakerAi.Utils.AudioPushStream;

type
  TFrmVoiceChat = class(TForm)
    LayoutTop:          TLayout;
    LblModelo:          TLabel;
    CbxModel:           TComboBox;
    LblIdioma:          TLabel;
    EdLanguage:         TEdit;
    LblVoz:             TLabel;
    EdVoice:            TEdit;
    LblSttModel:        TLabel;
    EdSttModel:         TEdit;
    BtnConnect:         TButton;
    CircleState:        TCircle;
    LayoutStatus:       TLayout;
    LblStatus:          TLabel;
    LayoutLive:         TLayout;
    LblLive:            TLabel;
    MemoLive:           TMemo;
    LayoutConversation: TLayout;
    LblConversation:    TLabel;
    MemoConversation:   TMemo;
    LayoutBottom:       TLayout;
    BtnClear:           TButton;
    ProgressBarLevel:   TProgressBar;
    LblVADInfo:         TLabel;
    BtnPushToTalk:      TButton;
    LblMic:             TLabel;
    CbxMic:             TComboBox;
    LblSens:            TLabel;
    EdSensitivity:      TEdit;
    BtnRecalibrar:      TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnPushToTalkClick(Sender: TObject);
    procedure BtnRecalibrarClick(Sender: TObject);
  private
    FChat:           TAiMakerAiRealtimeChat;
    FMic:            TAIVoiceMonitor;
    FPlayer:         TAudioPushStream;
    FLiveText:       string;
    FPlayerStarted:  Boolean;
    FCalibThreshold: Integer;
    FMicDevices:     TArray<TWaveInDeviceInfo>;
    procedure SetStatus(const Msg: string);
    procedure SetStateColor(AColor: TAlphaColor);
    procedure AddConvLine(const APrefix, AText: string);
    procedure Log(const AMsg: string);
    // Eventos del driver MakerAI
    procedure OnChatConnected(Sender: TObject);
    procedure OnChatDisconnected(Sender: TObject);
    procedure OnChatSessionReady(Sender: TObject);
    procedure OnChatTranscriptDelta(Sender: TObject; const Delta: string);
    procedure OnChatTranscriptCompleted(Sender: TObject; const Transcript, ItemId: string);
    procedure OnChatAssistantText(Sender: TObject; const AText: string);
    procedure OnChatAudioChunk(Sender: TObject; const AData: TBytes);
    procedure OnChatAudioDone(Sender: TObject);
    procedure OnChatError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    // Eventos del microfono
    procedure OnMicCalibrated(Sender: TObject; const NoiseLevel, Sensitivity, StopSensitivity: Integer);
    procedure OnMicError(Sender: TObject; const ErrorMessage: string);
    procedure OnMicSpeechChange(Sender: TObject; aUserSpeak, aIsValidForIA: Boolean; aStream: TMemoryStream);
    procedure OnMicUpdate(Sender: TObject; const aSoundLevel: Int64);
  end;

var
  FrmVoiceChat: TFrmVoiceChat;

implementation

{$R *.fmx}

const
  CLR_GRAY   = TAlphaColor($FF808080); // desconectado
  CLR_ORANGE = TAlphaColor($FFFFA500); // conectando / calibrando
  CLR_GREEN  = TAlphaColor($FF22AA22); // listo / escuchando
  CLR_RED    = TAlphaColor($FFCC2222); // hablando
  CLR_BLUE   = TAlphaColor($FF2255CC); // asistente respondiendo

{ TFrmVoiceChat }

procedure TFrmVoiceChat.FormCreate(Sender: TObject);
begin
  CbxModel.Items.Add('mk-gpt-oss-20b');
  CbxModel.Items.Add('mk-scout');
  CbxModel.Items.Add('mk-pro');
  CbxModel.Items.Add('mk-basic-8b');
  CbxModel.ItemIndex := 0;

  FChat := TAiMakerAiRealtimeChat.Create(nil);
  FChat.ApiKey        := '@MAKERAI_API_KEY';
  FChat.Language      := 'es';
  FChat.Voice         := 'nova';
  FChat.SttModel      := 'mk-whisper-large';
  FChat.Instructions  := 'Responde siempre en español.';

  FChat.OnConnected           := OnChatConnected;
  FChat.OnDisconnected        := OnChatDisconnected;
  FChat.OnSessionReady        := OnChatSessionReady;
  FChat.OnTranscriptDelta     := OnChatTranscriptDelta;
  FChat.OnTranscriptCompleted := OnChatTranscriptCompleted;
  FChat.OnAssistantText       := OnChatAssistantText;
  FChat.OnAudioChunk          := OnChatAudioChunk;
  FChat.OnAudioDone           := OnChatAudioDone;
  FChat.OnError               := OnChatError;

  FMic := TAIVoiceMonitor.Create(nil);
  FMic.SampleRate                := 44100;
  FMic.SilenceDuration           := 800;
  FMic.SensitivityMultiplier     := 1.5;
  FMic.StopSensitivityMultiplier := 1.0;
  FMic.RealtimeSTT               := FChat;
  FMic.OnCalibrated              := OnMicCalibrated;
  FMic.OnError                   := OnMicError;
  FMic.OnChangeState             := OnMicSpeechChange;
  FMic.OnUpdate                  := OnMicUpdate;

  FPlayer          := TAudioPushStream.Create;
  FPlayerStarted   := False;
  FLiveText        := '';
  FCalibThreshold  := 0;
  ProgressBarLevel.Max := 5000;

  // Poblar lista de micrófonos disponibles
  FMicDevices := TAIVoiceMonitor.GetWaveInDevices;
  for var I := 0 to High(FMicDevices) do
    CbxMic.Items.Add(FMicDevices[I].DeviceName);
  if CbxMic.Items.Count > 0 then
    CbxMic.ItemIndex := 0;

  SetStateColor(CLR_GRAY);
  CircleState.Stroke.Kind := TBrushKind.None;
end;

procedure TFrmVoiceChat.FormDestroy(Sender: TObject);
begin
  FMic.Active := False;
  FMic.Free;
  FChat.Disconnect;
  FChat.Free;
  if FPlayerStarted then
    FPlayer.Stop;
  FPlayer.Free;
end;

procedure TFrmVoiceChat.SetStatus(const Msg: string);
begin
  LblStatus.Text := Msg;
end;

procedure TFrmVoiceChat.SetStateColor(AColor: TAlphaColor);
begin
  CircleState.Fill.Color := AColor;
end;

procedure TFrmVoiceChat.AddConvLine(const APrefix, AText: string);
begin
  MemoConversation.Lines.Add(
    Format('[%s] %s %s', [FormatDateTime('hh:nn:ss', Now), APrefix, AText]));
  MemoConversation.GoToTextEnd;
end;

procedure TFrmVoiceChat.Log(const AMsg: string);
begin
  MemoConversation.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), AMsg]));
  MemoConversation.GoToTextEnd;
end;

procedure TFrmVoiceChat.BtnConnectClick(Sender: TObject);
begin
  if FChat.IsConnected then
  begin
    FMic.Active := False;
    FChat.Disconnect;
    BtnConnect.Text := 'Conectar';
  end
  else
  begin
    FChat.Model    := CbxModel.Items[CbxModel.ItemIndex];
    FChat.Language := Trim(EdLanguage.Text);
    FChat.Voice    := Trim(EdVoice.Text);
    FChat.SttModel := Trim(EdSttModel.Text);
    SetStatus('Conectando...');
    SetStateColor(CLR_ORANGE);
    BtnConnect.Text := 'Desconectar';
    FChat.Connect;
  end;
end;

procedure TFrmVoiceChat.BtnClearClick(Sender: TObject);
begin
  MemoConversation.Lines.Clear;
end;

{ Eventos del driver — todos llegan en el hilo principal (TThread.Queue) }

procedure TFrmVoiceChat.OnChatConnected(Sender: TObject);
begin
  Log('>> WS conectado - enviando sesion...');
  SetStatus('WebSocket conectado. Enviando configuracion de sesion...');
  SetStateColor(CLR_ORANGE);
end;

procedure TFrmVoiceChat.OnChatDisconnected(Sender: TObject);
begin
  Log('>> WS desconectado');
  FMic.Active := False;
  BtnRecalibrar.Enabled := False;
  if FPlayerStarted then
  begin
    FPlayer.Stop;
    FPlayerStarted := False;
  end;
  SetStatus('Desconectado. Presione Conectar para iniciar.');
  SetStateColor(CLR_GRAY);
  BtnConnect.Text := 'Conectar';
end;

procedure TFrmVoiceChat.OnChatSessionReady(Sender: TObject);
begin
  Log('>> Sesion ready - iniciando microfono');
  if not FPlayerStarted then
  begin
    FPlayer.Start(24000, 1, 16);
    FPlayerStarted := True;
  end;
  // Asignar el micrófono seleccionado
  if (CbxMic.ItemIndex >= 0) and (CbxMic.ItemIndex < Length(FMicDevices)) then
  begin
    FMic.DeviceID := FMicDevices[CbxMic.ItemIndex].DeviceID;
    Log('>> [MIC] Dispositivo: ' + FMicDevices[CbxMic.ItemIndex].DeviceName);
  end;
  // Leer umbral mínimo del campo de UI (valor absoluto: 200=muy sensible, 500=normal, 1000=poco sensible)
  FMic.MinSensitivity    := StrToIntDef(Trim(EdSensitivity.Text), 500);
  FMic.MinStopSensitivity := FMic.MinSensitivity div 2;
  Log(Format('>> [SENS] SensMin=%d  StopMin=%d', [FMic.MinSensitivity, FMic.MinStopSensitivity]));
  SetStatus('Sesion lista. Calibrando microfono...');
  SetStateColor(CLR_ORANGE);
  BtnRecalibrar.Enabled := True;
  FMic.Active := True;
end;

procedure TFrmVoiceChat.OnMicCalibrated(Sender: TObject;
  const NoiseLevel, Sensitivity, StopSensitivity: Integer);
begin
  FCalibThreshold      := Sensitivity;
  ProgressBarLevel.Max := Sensitivity * 5;
  LblVADInfo.Text      := Format('Ruido: %d | Umbral inicio: %d | Umbral stop: %d',
    [NoiseLevel, Sensitivity, StopSensitivity]);
  SetStatus(Format('Listo - habla en el microfono  [ruido=%d  umbral=%d]',
    [NoiseLevel, Sensitivity]));
  SetStateColor(CLR_GREEN);
  Log(Format('>> [CAL] Calibrado — ruido=%d  umbral=%d  (tu voz debe superar %d)',
    [NoiseLevel, Sensitivity, Sensitivity]));
end;

procedure TFrmVoiceChat.OnChatTranscriptDelta(Sender: TObject;
  const Delta: string);
begin
  FLiveText     := FLiveText + Delta;
  MemoLive.Text := FLiveText;
  SetStateColor(CLR_RED);
end;

procedure TFrmVoiceChat.OnChatTranscriptCompleted(Sender: TObject;
  const Transcript, ItemId: string);
begin
  FLiveText     := '';
  MemoLive.Text := '';
  AddConvLine('Usuario:', Transcript);
  SetStatus('Asistente procesando...');
  SetStateColor(CLR_BLUE);
end;

procedure TFrmVoiceChat.OnChatAssistantText(Sender: TObject;
  const AText: string);
begin
  AddConvLine('Asistente:', AText);
  SetStatus('Asistente respondiendo...');
end;

procedure TFrmVoiceChat.OnChatAudioChunk(Sender: TObject;
  const AData: TBytes);
begin
  if FPlayerStarted then
    FPlayer.PushPCMData(AData);
end;

procedure TFrmVoiceChat.OnChatAudioDone(Sender: TObject);
begin
  SetStatus(Format('Listo - habla en el microfono  (audio terminado %s)',
    [FormatDateTime('hh:nn:ss', Now)]));
  SetStateColor(CLR_GREEN);
end;

procedure TFrmVoiceChat.OnChatError(Sender: TObject;
  const ErrorMsg, ErrorCode: string);
var
  FullMsg: string;
begin
  FullMsg := Format('[ERROR] code=%s | %s', [ErrorCode, ErrorMsg]);
  Log('>> ' + FullMsg);
  SetStatus(FullMsg);
  SetStateColor(CLR_RED);
  BtnConnect.Text := 'Conectar';
  ShowMessage(FullMsg);
end;

procedure TFrmVoiceChat.OnMicError(Sender: TObject; const ErrorMessage: string);
begin
  Log('>> [MIC ERROR] ' + ErrorMessage);
  SetStatus('Error de microfono: ' + ErrorMessage);
  SetStateColor(CLR_RED);
end;

procedure TFrmVoiceChat.OnMicSpeechChange(Sender: TObject;
  aUserSpeak, aIsValidForIA: Boolean; aStream: TMemoryStream);
begin
  if aUserSpeak then
  begin
    SetStateColor(CLR_RED);
    Log('>> [MIC] habla detectada');
  end
  else
  begin
    // Fin de turno del usuario: avisar al servidor para procesar STT→LLM→TTS
    if FChat.IsConnected then
    begin
      FChat.CommitAudio;
      Log('>> [MIC] silencio detectado - commit enviado');
    end;
    SetStateColor(CLR_BLUE);
  end;
end;

procedure TFrmVoiceChat.OnMicUpdate(Sender: TObject; const aSoundLevel: Int64);
begin
  if aSoundLevel > Trunc(ProgressBarLevel.Max) then
    ProgressBarLevel.Value := ProgressBarLevel.Max
  else
    ProgressBarLevel.Value := aSoundLevel;
end;

procedure TFrmVoiceChat.BtnPushToTalkClick(Sender: TObject);
begin
  if FChat.IsConnected then
  begin
    FChat.CommitAudio;
    Log('>> [MANUAL] commit enviado');
  end;
end;

procedure TFrmVoiceChat.BtnRecalibrarClick(Sender: TObject);
begin
  if not FChat.IsConnected then Exit;
  FMic.Active := False;
  if (CbxMic.ItemIndex >= 0) and (CbxMic.ItemIndex < Length(FMicDevices)) then
    FMic.DeviceID := FMicDevices[CbxMic.ItemIndex].DeviceID;
  FMic.MinSensitivity    := StrToIntDef(Trim(EdSensitivity.Text), 500);
  FMic.MinStopSensitivity := FMic.MinSensitivity div 2;
  Log(Format('>> [RECAL] SensMin=%d  recalibrando...', [FMic.MinSensitivity]));
  SetStatus('Recalibrando microfono...');
  SetStateColor(CLR_ORANGE);
  FMic.Active := True;
end;

end.
