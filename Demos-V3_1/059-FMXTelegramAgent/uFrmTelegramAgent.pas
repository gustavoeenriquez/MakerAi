unit uFrmTelegramAgent;

// =============================================================================
// MakerAI - Demo 059: FMX Telegram Agent
//
// Main form. Minimal UI: token input, Start/Stop, activity log.
// All logic lives in TDmTelegram + TDmAgent.
// =============================================================================

interface

uses
  System.SysUtils, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.Memo, FMX.Memo.Types,
  FMX.Layouts, FMX.ScrollBox, FMX.Controls.Presentation,
  uDmTelegram;

type
  TFrmTelegramAgent = class(TForm)
    LayoutTop : TLayout;
    LblToken  : TLabel;
    EdToken   : TEdit;
    BtnStart  : TButton;
    BtnStop   : TButton;
    LblStatus : TLabel;
    MemoLog   : TMemo;
    procedure FormCreate (Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick (Sender: TObject);
  private
    FDmTelegram: TDmTelegram;
    procedure AddToLog(const ADir, AText: string);
    procedure OnBotLog(const ADir, AText: string);
  end;

var
  FrmTelegramAgent: TFrmTelegramAgent;

implementation

{$R *.fmx}

procedure TFrmTelegramAgent.FormCreate(Sender: TObject);
begin
  FDmTelegram          := TDmTelegram.Create(Self);
  FDmTelegram.OnLog    := OnBotLog;
  BtnStop.Enabled      := False;
  LblStatus.Text       := 'Enter your bot token and press Start.';
end;

procedure TFrmTelegramAgent.FormDestroy(Sender: TObject);
begin
  // Stop the bot here, before form components are freed.
  // OnDestroy fires before owned components (MemoLog, etc.) are destroyed,
  // so it is safe to call Stop and flush any last log callbacks.
  FDmTelegram.OnLog := nil;   // prevent callbacks into components being freed
  FDmTelegram.Stop;
  // TDmTelegram is owned by Self — freed automatically after this.
end;

procedure TFrmTelegramAgent.BtnStartClick(Sender: TObject);
var
  Token: string;
begin
  Token := Trim(EdToken.Text);
  if Token = '' then
  begin
    ShowMessage('Enter the bot token first.');
    Exit;
  end;

  FDmTelegram.Start(Token);

  BtnStart.Enabled := False;
  BtnStop.Enabled  := True;
  EdToken.Enabled  := False;
  LblStatus.Text   := 'Bot active.';
end;

procedure TFrmTelegramAgent.BtnStopClick(Sender: TObject);
begin
  FDmTelegram.Stop;
  BtnStart.Enabled := True;
  BtnStop.Enabled  := False;
  EdToken.Enabled  := True;
  LblStatus.Text   := 'Bot stopped.';
end;

procedure TFrmTelegramAgent.AddToLog(const ADir, AText: string);
var
  Prefix, Time: string;
begin
  Time := FormatDateTime('hh:nn:ss', Now);
  if      ADir = 'IN'   then Prefix := '[' + Time + '] >>> '
  else if ADir = 'OUT'  then Prefix := '[' + Time + '] <<< '
  else if ADir = 'TOOL' then Prefix := '[' + Time + '] [T] '
  else                        Prefix := '[' + Time + '] *** ';
  MemoLog.Lines.Add(Prefix + AText);
  MemoLog.GoToTextEnd;
end;

procedure TFrmTelegramAgent.OnBotLog(const ADir, AText: string);
var
  LDir, LText: string;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    AddToLog(ADir, AText)
  else
  begin
    LDir  := ADir;
    LText := AText;
    TThread.Synchronize(nil, procedure begin AddToLog(LDir, LText); end);
  end;
end;

end.
