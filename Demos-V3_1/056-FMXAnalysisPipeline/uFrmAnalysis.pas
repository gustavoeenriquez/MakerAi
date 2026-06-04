unit uFrmAnalysis;

// =============================================================================
// MakerAI - Demo 056: FMX Analysis Pipeline
// Formulario principal.
// =============================================================================

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Memo.Types, FMX.Edit, FMX.Layouts,
  uMakerAi.Agents;

type
  TFrmAnalysis = class(TForm)
    LayoutTop      : TLayout;
    LblTopic       : TLabel;
    EdTopic        : TEdit;
    BtnStart       : TButton;
    LblStatus      : TLabel;
    LayoutLog      : TLayout;
    LblLog         : TLabel;
    MemoLog        : TMemo;
    LayoutResult   : TLayout;
    LblResult      : TLabel;
    MemoResult     : TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
  private
    procedure OnAgentLog(const Value: string);
    procedure OnAgentFinished(const Value: string);
  end;

var
  FrmAnalysis: TFrmAnalysis;

implementation

{$R *.fmx}

uses uDmAnalysis;

procedure TFrmAnalysis.FormCreate(Sender: TObject);
begin
  DmAnalysis := TDmAnalysis.Create(Self);
  DmAnalysis.OnLog      := OnAgentLog;
  DmAnalysis.OnFinished := OnAgentFinished;
  EdTopic.Text := 'Climate change and its economic impacts';
  LblStatus.Text := 'Enter a topic and press Analyze.';
end;

procedure TFrmAnalysis.FormDestroy(Sender: TObject);
begin
  DmAnalysis.Free;
  DmAnalysis := nil;
end;

procedure TFrmAnalysis.BtnStartClick(Sender: TObject);
var
  Topic: string;
begin
  Topic := Trim(EdTopic.Text);
  if Topic = '' then
  begin
    ShowMessage('Enter a topic to analyze.');
    Exit;
  end;

  MemoLog.Lines.Clear;
  MemoResult.Lines.Clear;
  BtnStart.Enabled := False;
  LblStatus.Text := 'Analysis in progress...';

  MemoLog.Lines.Add('TOPIC: ' + Topic);
  MemoLog.Lines.Add(StringOfChar('=', 60));

  DmAnalysis.RunAnalysis(Topic);
end;

procedure TFrmAnalysis.OnAgentLog(const Value: string);
begin
  MemoLog.Lines.Add(Value);
  MemoLog.GoToTextEnd;
end;

procedure TFrmAnalysis.OnAgentFinished(const Value: string);
begin
  MemoResult.Lines.Text := Value;
  LblStatus.Text := 'Analysis complete.';
  BtnStart.Enabled := True;
  MemoLog.Lines.Add(StringOfChar('=', 60));
  MemoLog.Lines.Add('Pipeline complete.');
end;

end.
