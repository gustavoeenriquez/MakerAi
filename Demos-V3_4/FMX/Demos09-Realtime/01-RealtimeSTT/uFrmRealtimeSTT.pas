unit uFrmRealtimeSTT;

// =============================================================================
// MakerAI — Demo FMX Realtime STT
// Transcripcion de voz en tiempo real usando OpenAI Realtime API.
//
// Componentes: TAiOpenAiRealtimeSTT + TAIVoiceMonitor
//
// Flujo:
//   1. TAIVoiceMonitor captura audio del microfono (PCM16 44100 Hz)
//   2. RealtimeSTT resamplea a 24 kHz y envia por WebSocket
//   3. VAD del servidor detecta inicio/fin de habla
//   4. Transcripcion llega en tiempo real (delta) y como texto final
//
// Requiere: variable de entorno OPENAI_API_KEY configurada.
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
  uMakerAi.Realtime.OpenAI,
  uMakerAi.Utils.VoiceMonitor;

type
  TFrmRealtimeSTT = class(TForm)
    LayoutTop:      TLayout;
    LblModelo:      TLabel;
    CbxModel:       TComboBox;
    LblIdioma:      TLabel;
    EdLanguage:     TEdit;
    BtnConnect:     TButton;
    CircleState:    TCircle;
    LayoutStatus:   TLayout;
    LblStatus:      TLabel;
    LayoutLive:     TLayout;
    LblLive:        TLabel;
    MemoLive:       TMemo;
    LayoutHistory:  TLayout;
    LblHistory:     TLabel;
    MemoHistory:    TMemo;
    LayoutClear:    TLayout;
    BtnClear:       TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
  private
    FSTT:      TAiOpenAiRealtimeSTT;
    FMic:      TAIVoiceMonitor;
    FLiveText: string;
    procedure SetStatus(const Msg: string);
    procedure SetStateColor(AColor: TAlphaColor);
    procedure OnSttConnected(Sender: TObject);
    procedure OnSttDisconnected(Sender: TObject);
    procedure OnSttSessionReady(Sender: TObject);
    procedure OnSttSpeechStarted(Sender: TObject; AudioMs: Int64; const ItemId: string);
    procedure OnSttSpeechStopped(Sender: TObject; AudioMs: Int64; const ItemId: string);
    procedure OnSttTranscriptDelta(Sender: TObject; const Delta: string);
    procedure OnSttTranscriptCompleted(Sender: TObject; const Transcript, ItemId: string);
    procedure OnSttError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure OnMicCalibrated(Sender: TObject; const NoiseLevel, Sensitivity, StopSensitivity: Integer);
  end;

var
  FrmRealtimeSTT: TFrmRealtimeSTT;

implementation

{$R *.fmx}

const
  CLR_GRAY   = TAlphaColor($FF808080); // desconectado
  CLR_ORANGE = TAlphaColor($FFFFA500); // conectando / calibrando
  CLR_GREEN  = TAlphaColor($FF22AA22); // listo / escuchando
  CLR_RED    = TAlphaColor($FFCC2222); // hablando / error

{ TFrmRealtimeSTT }

procedure TFrmRealtimeSTT.FormCreate(Sender: TObject);
begin
  CbxModel.Items.Add('gpt-4o-realtime-preview');
  CbxModel.Items.Add('gpt-4o-mini-realtime-preview');
  CbxModel.ItemIndex := 0;

  FSTT := TAiOpenAiRealtimeSTT.Create(nil);
  FSTT.ApiKey             := '@OPENAI_API_KEY';
  FSTT.Language           := 'es';
  FSTT.VADMode            := rvmServerVad;
  FSTT.SilenceDurationMs  := 600;
  FSTT.PrefixPaddingMs    := 300;
  FSTT.NoiseReduction     := rnrNearField;
  FSTT.TranscriptionModel := otmGpt4oTranscribe;

  FSTT.OnConnected           := OnSttConnected;
  FSTT.OnDisconnected        := OnSttDisconnected;
  FSTT.OnSessionReady        := OnSttSessionReady;
  FSTT.OnSpeechStarted       := OnSttSpeechStarted;
  FSTT.OnSpeechStopped       := OnSttSpeechStopped;
  FSTT.OnTranscriptDelta     := OnSttTranscriptDelta;
  FSTT.OnTranscriptCompleted := OnSttTranscriptCompleted;
  FSTT.OnError               := OnSttError;

  FMic := TAIVoiceMonitor.Create(nil);
  FMic.SampleRate      := 44100;
  FMic.SilenceDuration := 800;
  FMic.RealtimeSTT     := FSTT;
  FMic.OnCalibrated    := OnMicCalibrated;

  FLiveText := '';
  SetStateColor(CLR_GRAY);
  // Eliminar borde del circulo indicador
  CircleState.Stroke.Kind := TBrushKind.None;
end;

procedure TFrmRealtimeSTT.FormDestroy(Sender: TObject);
begin
  FMic.Active := False;
  FMic.Free;
  FSTT.Disconnect;
  FSTT.Free;
end;

procedure TFrmRealtimeSTT.SetStatus(const Msg: string);
begin
  LblStatus.Text := Msg;
end;

procedure TFrmRealtimeSTT.SetStateColor(AColor: TAlphaColor);
begin
  CircleState.Fill.Color := AColor;
end;

procedure TFrmRealtimeSTT.BtnConnectClick(Sender: TObject);
begin
  if FSTT.IsConnected then
  begin
    FMic.Active := False;
    FSTT.Disconnect;
    BtnConnect.Text := 'Conectar';
  end
  else
  begin
    FSTT.Model    := CbxModel.Items[CbxModel.ItemIndex];
    FSTT.Language := Trim(EdLanguage.Text);
    SetStatus('Conectando...');
    SetStateColor(CLR_ORANGE);
    BtnConnect.Text := 'Desconectar';
    FSTT.Connect;
  end;
end;

procedure TFrmRealtimeSTT.BtnClearClick(Sender: TObject);
begin
  MemoHistory.Lines.Clear;
end;

{ Eventos STT — todos llegan ya en el hilo principal (TThread.Queue en base class) }

procedure TFrmRealtimeSTT.OnSttConnected(Sender: TObject);
begin
  SetStatus('WebSocket conectado. Configurando sesion...');
  SetStateColor(CLR_ORANGE);
end;

procedure TFrmRealtimeSTT.OnSttDisconnected(Sender: TObject);
begin
  FMic.Active := False;
  SetStatus('Desconectado. Presione Conectar para iniciar.');
  SetStateColor(CLR_GRAY);
  BtnConnect.Text := 'Conectar';
end;

procedure TFrmRealtimeSTT.OnSttSessionReady(Sender: TObject);
begin
  SetStatus('Sesion lista. Calibrando microfono...');
  SetStateColor(CLR_ORANGE);
  FMic.Active := True;
end;

procedure TFrmRealtimeSTT.OnMicCalibrated(Sender: TObject;
  const NoiseLevel, Sensitivity, StopSensitivity: Integer);
begin
  SetStatus(Format('Listo — habla en el microfono  [ruido=%d  umbral=%d]',
    [NoiseLevel, Sensitivity]));
  SetStateColor(CLR_GREEN);
end;

procedure TFrmRealtimeSTT.OnSttSpeechStarted(Sender: TObject; AudioMs: Int64;
  const ItemId: string);
begin
  FLiveText     := '';
  MemoLive.Text := '';
  SetStateColor(CLR_RED);
end;

procedure TFrmRealtimeSTT.OnSttSpeechStopped(Sender: TObject; AudioMs: Int64;
  const ItemId: string);
begin
  SetStateColor(CLR_GREEN);
end;

procedure TFrmRealtimeSTT.OnSttTranscriptDelta(Sender: TObject; const Delta: string);
begin
  FLiveText     := FLiveText + Delta;
  MemoLive.Text := FLiveText;
end;

procedure TFrmRealtimeSTT.OnSttTranscriptCompleted(Sender: TObject;
  const Transcript, ItemId: string);
begin
  FLiveText     := '';
  MemoLive.Text := '';
  MemoHistory.Lines.Add(
    Format('[%s]  %s', [FormatDateTime('hh:nn:ss', Now), Transcript]));
  MemoHistory.GoToTextEnd;
end;

procedure TFrmRealtimeSTT.OnSttError(Sender: TObject;
  const ErrorMsg, ErrorCode: string);
begin
  SetStatus(Format('[ERROR] (%s) %s', [ErrorCode, ErrorMsg]));
  SetStateColor(CLR_RED);
  BtnConnect.Text := 'Conectar';
end;

end.
