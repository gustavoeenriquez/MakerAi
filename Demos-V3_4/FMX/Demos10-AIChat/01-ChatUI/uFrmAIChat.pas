unit uFrmAIChat;

// =============================================================================
// MakerAI — Demo FMX Chat UI
// Muestra TAIChatView + TAIChatInput sobre un formulario FMX.
//
// Componentes declarados en uFrmAIChat.fmx (diseñador de formularios):
//   TAIChatView  — renderizado Skia de burbujas con Markdown
//   TAIChatInput — barra de entrada con adjuntos, captura de pantalla y micrófono
//
// Flujo de streaming simulado:
//   BtnStream → BeginAssistantMessage → TTimer (30 ms) → AppendToken → FinishMessage
// =============================================================================

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts,
  AIChat.Types,
  AIChat.Control.FMX,
  AIChat.Input;

type
  TFrmAIChat = class(TForm)
    ToolBar1  : TToolBar;
    BtnLight  : TButton;
    BtnDark   : TButton;
    BtnClear  : TButton;
    BtnLoad   : TButton;
    BtnStream : TButton;
    ChatView  : TAIChatView;
    ChatInput : TAIChatInput;
    procedure FormCreate(Sender: TObject);
    procedure BtnLight_Click (Sender: TObject);
    procedure BtnDark_Click  (Sender: TObject);
    procedure BtnClear_Click (Sender: TObject);
    procedure BtnLoad_Click  (Sender: TObject);
    procedure BtnStream_Click(Sender: TObject);
    procedure ChatView_LinkClick (Sender: TObject; AMsg: TAIChatMessage;
                                   const AURL: string);
    procedure ChatView_AttachOpen(Sender: TObject; AMsg: TAIChatMessage;
                                   AAttach: TAIChatAttachment);
    procedure ChatInput_Send     (Sender: TObject; const APrompt: string;
                                   AAttachments: TAIChatAttachments;
                                   AAudio: TMemoryStream);
    procedure ChatInput_Cancel   (Sender: TObject);
    procedure ChatInput_MicToggle(Sender: TObject; AActivate: Boolean);
  private
    FStreamTimer    : TTimer;
    FStreamMsgIndex : Integer;
    FStreamText     : string;
    FStreamPos      : Integer;
    procedure StreamTimerFired(Sender: TObject);
    procedure LoadSampleConversation;
  end;

var
  FrmAIChat: TFrmAIChat;

implementation

{$R *.fmx}

const
  STREAM_SAMPLE =
    '## Respuesta generada por IA' + #10 +
    '' + #10 +
    'Este es un ejemplo de **respuesta en streaming** del asistente.' + #10 +
    '' + #10 +
    'Las características incluyen:' + #10 +
    '' + #10 +
    '- Renderizado **Markdown** completo' + #10 +
    '- Burbujas diferenciadas por rol' + #10 +
    '- Scroll virtualizado para conversaciones largas' + #10 +
    '- Soporte multiplataforma (Windows, macOS, Android, iOS)' + #10 +
    '' + #10 +
    '```pascal' + #10 +
    'var Msg := ChatView.BeginAssistantMessage;' + #10 +
    'ChatView.AppendToken(Msg.Index, ''token'');' + #10 +
    'ChatView.FinishMessage(Msg.Index);' + #10 +
    '```' + #10 +
    '' + #10 +
    '> **Nota**: el componente renderiza Markdown con Skia sin controles hijos.' + #10;

{ TFrmAIChat }

procedure TFrmAIChat.FormCreate(Sender: TObject);
begin
  FStreamMsgIndex := -1;

  FStreamTimer          := TTimer.Create(Self);
  FStreamTimer.Interval := 30;
  FStreamTimer.Enabled  := False;
  FStreamTimer.OnTimer  := StreamTimerFired;

  LoadSampleConversation;
end;

{ ── Toolbar ──────────────────────────────────────────────────────────────── }

procedure TFrmAIChat.BtnLight_Click(Sender: TObject);
begin
  ChatView.ThemeDark := False;
end;

procedure TFrmAIChat.BtnDark_Click(Sender: TObject);
begin
  ChatView.ThemeDark := True;
end;

procedure TFrmAIChat.BtnClear_Click(Sender: TObject);
begin
  FStreamTimer.Enabled := False;
  FStreamMsgIndex      := -1;
  BtnStream.Text       := 'Stream AI';
  ChatView.ClearMessages;
end;

procedure TFrmAIChat.BtnLoad_Click(Sender: TObject);
begin
  FStreamTimer.Enabled := False;
  FStreamMsgIndex      := -1;
  BtnStream.Text       := 'Stream AI';
  ChatView.ClearMessages;
  LoadSampleConversation;
end;

{ ── Streaming simulation ─────────────────────────────────────────────────── }

procedure TFrmAIChat.BtnStream_Click(Sender: TObject);
begin
  if FStreamTimer.Enabled then
  begin
    FStreamTimer.Enabled := False;
    if FStreamMsgIndex >= 0 then
      ChatView.FinishMessage(FStreamMsgIndex);
    FStreamMsgIndex  := -1;
    BtnStream.Text   := 'Stream AI';
    Exit;
  end;

  FStreamText := STREAM_SAMPLE;
  FStreamPos  := 1;
  ChatView.BeginAssistantMessage;
  FStreamMsgIndex := ChatView.MessageCount - 1;

  FStreamTimer.Enabled := True;
  BtnStream.Text       := 'Stop';
end;

procedure TFrmAIChat.StreamTimerFired(Sender: TObject);
const
  CHUNK = 3;
var
  Remaining : Integer;
  Piece     : string;
begin
  if FStreamMsgIndex < 0 then
  begin
    FStreamTimer.Enabled := False;
    BtnStream.Text       := 'Stream AI';
    Exit;
  end;

  Remaining := Length(FStreamText) - FStreamPos + 1;
  if Remaining <= 0 then
  begin
    FStreamTimer.Enabled := False;
    ChatView.FinishMessage(FStreamMsgIndex);
    FStreamMsgIndex := -1;
    BtnStream.Text  := 'Stream AI';
    Exit;
  end;

  Piece := Copy(FStreamText, FStreamPos, Min(CHUNK, Remaining));
  ChatView.AppendToken(FStreamMsgIndex, Piece);
  Inc(FStreamPos, CHUNK);
end;

{ ── TAIChatInput handlers ─────────────────────────────────────────────────── }

procedure TFrmAIChat.ChatInput_Send(Sender: TObject; const APrompt: string;
  AAttachments: TAIChatAttachments; AAudio: TMemoryStream);
begin
  if APrompt <> '' then
    ChatView.AddUserMessage(APrompt)
  else if AAudio <> nil then
    ChatView.AddUserMessage('[Voice message]');
  ChatInput.Busy := False;
  ChatView.ScrollToBottom;
end;

procedure TFrmAIChat.ChatInput_Cancel(Sender: TObject);
begin
  FStreamTimer.Enabled := False;
  if FStreamMsgIndex >= 0 then
    ChatView.FinishMessage(FStreamMsgIndex);
  FStreamMsgIndex  := -1;
  BtnStream.Text   := 'Stream AI';
  ChatInput.Busy   := False;
end;

procedure TFrmAIChat.ChatInput_MicToggle(Sender: TObject; AActivate: Boolean);
begin
  if AActivate then
    ChatInput.NotifyVoiceCalibrated
  else
    ChatInput.NotifyVoiceError;
end;

{ ── TAIChatView events ────────────────────────────────────────────────────── }

procedure TFrmAIChat.ChatView_LinkClick(Sender: TObject; AMsg: TAIChatMessage;
  const AURL: string);
begin
  ShowMessage('Link: ' + AURL);
end;

procedure TFrmAIChat.ChatView_AttachOpen(Sender: TObject; AMsg: TAIChatMessage;
  AAttach: TAIChatAttachment);
begin
  ShowMessage('Attachment: ' + AAttach.FileName);
end;

{ ── Sample conversation ──────────────────────────────────────────────────── }

procedure TFrmAIChat.LoadSampleConversation;
begin
  ChatView.AddUserMessage('Hola, ¿qué es TAIChatView?');

  ChatView.AddMessage(TChatRole.crAssistant,
    '**TAIChatView** es un componente FMX que renderiza conversaciones de chat con IA ' +
    'usando Skia como motor de pintado.' + #10 +
    '' + #10 +
    'Características principales:' + #10 +
    '- Burbujas de usuario y asistente diferenciadas' + #10 +
    '- Markdown completo en respuestas del asistente' + #10 +
    '- Streaming de tokens en tiempo real' + #10 +
    '- Virtualización para conversaciones largas' + #10 +
    '- Multiplataforma: Windows, Android, macOS, iOS');

  ChatView.AddUserMessage('¿Cómo lo uso en streaming?');

  ChatView.AddMessage(TChatRole.crAssistant,
    'El flujo es sencillo:' + #10 +
    '' + #10 +
    '```pascal' + #10 +
    '// 1. Iniciar mensaje de asistente' + #10 +
    'ChatView.BeginAssistantMessage;' + #10 +
    'var MsgIdx := ChatView.MessageCount - 1;' + #10 +
    '' + #10 +
    '// 2. Enviar tokens conforme llegan de la API' + #10 +
    'ChatView.AppendToken(MsgIdx, token);' + #10 +
    '' + #10 +
    '// 3. Finalizar' + #10 +
    'ChatView.FinishMessage(MsgIdx);' + #10 +
    '```' + #10 +
    '' + #10 +
    'Pulsa **Stream AI** para verlo en acción.');

  ChatView.ScrollToBottom;
end;

end.
