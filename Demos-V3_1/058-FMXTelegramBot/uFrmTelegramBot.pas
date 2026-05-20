unit uFrmTelegramBot;

// =============================================================================
// MakerAI - Demo 058: FMX Telegram Bot
// =============================================================================

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.StrUtils,
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
  FMX.Layouts;

type
  TFrmTelegramBot = class(TForm)
    LayoutTop    : TLayout;
    LblToken     : TLabel;
    EdToken      : TEdit;
    BtnStart     : TButton;
    BtnStop      : TButton;
    LayoutInfo   : TLayout;
    LblStatus    : TLabel;
    LblUsers     : TLabel;
    LayoutLog    : TLayout;
    LblLog       : TLabel;
    MemoLog      : TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnConfigClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
  private
    procedure OnBotLog(const ADirection, AText: string);
    procedure UpdateStats;
  end;

var
  FrmTelegramBot: TFrmTelegramBot;

implementation

{$R *.fmx}

uses
  uDmTelegramBot,
  uFrmConfig;

procedure TFrmTelegramBot.FormCreate(Sender: TObject);
begin
  DmTelegramBot := TDmTelegramBot.Create(Self);
  DmTelegramBot.OnLog := OnBotLog;

  if DmTelegramBot.LastToken <> '' then
    EdToken.Text := DmTelegramBot.LastToken
  else
    EdToken.Text := '@TELEGRAM_BOT_TOKEN';
  BtnStop.Enabled := False;
  LblStatus.Text  := 'Ingresa el token y presiona Start.';
  LblUsers.Text   := 'Skills: shell_exec | file_read | file_write | file_list | memory | agent_run';
end;

procedure TFrmTelegramBot.FormDestroy(Sender: TObject);
begin
  DmTelegramBot.Free;
  DmTelegramBot := nil;
end;

procedure TFrmTelegramBot.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  DmTelegramBot.Stop;
  CanClose := True;
end;

procedure TFrmTelegramBot.BtnConfigClick(Sender: TObject);
var Frm: TFrmConfig;
begin
  Frm := TFrmConfig.CreateConfig(Self, DmTelegramBot);
  try Frm.ShowModal;
  finally Frm.Free; end;
end;

procedure TFrmTelegramBot.BtnStartClick(Sender: TObject);
var
  Token: string;
begin
  Token := Trim(EdToken.Text);
  if Token = '' then
  begin
    ShowMessage('Ingresa el token del bot (@TELEGRAM_BOT_TOKEN o token literal).');
    Exit;
  end;

  MemoLog.Lines.Clear;
  BtnStart.Enabled := False;
  BtnStop.Enabled  := True;
  EdToken.Enabled  := False;
  LblStatus.Text   := 'Bot activo — esperando mensajes...';
  LblUsers.Text    := 'Skills: shell | files | memory | agent_run  |  Mode: open  |  Owner: -';

  DmTelegramBot.Start(Token);
end;

procedure TFrmTelegramBot.BtnStopClick(Sender: TObject);
begin
  DmTelegramBot.Stop;
  BtnStart.Enabled := True;
  BtnStop.Enabled  := False;
  EdToken.Enabled  := True;
  LblStatus.Text   := 'Bot detenido.';
  LblUsers.Text    := 'Skills: shell_exec | file_read | file_write | file_list | memory | agent_run';
end;

procedure TFrmTelegramBot.UpdateStats;
var
  OwnerStr: string;
begin
  if DmTelegramBot.OwnerChatId > 0 then
    OwnerStr := IntToStr(DmTelegramBot.OwnerChatId)
  else
    OwnerStr := 'pending';

  LblUsers.Text := Format(
    'Sessions: %d  |  Msgs: %d  |  Tools: %d  |  Uptime: %s  |  Mode: %s  |  Owner: %s',
    [DmTelegramBot.UserCount,
     DmTelegramBot.MessageCount,
     DmTelegramBot.ToolCallCount,
     DmTelegramBot.Uptime,
     IfThen(DmTelegramBot.WhitelistMode, 'whitelist', 'open'),
     OwnerStr]);
end;

procedure TFrmTelegramBot.OnBotLog(const ADirection, AText: string);
var
  Prefix, Time: string;
begin
  Time := FormatDateTime('hh:nn:ss', Now);

  if      ADirection = 'IN'   then Prefix := '[' + Time + '] >>> '
  else if ADirection = 'OUT'  then Prefix := '[' + Time + '] <<< '
  else if ADirection = 'TOOL' then Prefix := '[' + Time + '] [T] '
  else                             Prefix := '[' + Time + '] *** ';

  MemoLog.Lines.Add(Prefix + AText);
  MemoLog.GoToTextEnd;

  // Actualizar stats en cada evento de log
  if BtnStop.Enabled then
    UpdateStats;
end;

end.
