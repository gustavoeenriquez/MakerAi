unit uFrmCodeReview;

// =============================================================================
// MakerAI - Demo 057: FMX Code Reviewer
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
  TFrmCodeReview = class(TForm)
    LayoutTop    : TLayout;
    LblReq       : TLabel;
    EdRequirement: TEdit;
    BtnStart     : TButton;
    LblStatus    : TLabel;
    LayoutLog    : TLayout;
    LblLog       : TLabel;
    MemoLog      : TMemo;
    LayoutResult : TLayout;
    LblResult    : TLabel;
    MemoResult   : TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
  private
    procedure OnAgentLog(const Value: string);
    procedure OnAgentFinished(const Value: string);
  end;

var
  FrmCodeReview: TFrmCodeReview;

implementation

{$R *.fmx}

uses uDmCodeReview;

procedure TFrmCodeReview.FormCreate(Sender: TObject);
begin
  DmCodeReview := TDmCodeReview.Create(Self);
  DmCodeReview.OnLog      := OnAgentLog;
  DmCodeReview.OnFinished := OnAgentFinished;
  EdRequirement.Text := 'A function that validates email addresses using regex';
  LblStatus.Text := 'Enter a requirement and press Review.';
end;

procedure TFrmCodeReview.FormDestroy(Sender: TObject);
begin
  DmCodeReview.Free;
  DmCodeReview := nil;
end;

procedure TFrmCodeReview.BtnStartClick(Sender: TObject);
var
  Req: string;
begin
  Req := Trim(EdRequirement.Text);
  if Req = '' then
  begin
    ShowMessage('Enter a code requirement.');
    Exit;
  end;

  MemoLog.Lines.Clear;
  MemoResult.Lines.Clear;
  BtnStart.Enabled := False;
  LblStatus.Text := 'Processing...';

  MemoLog.Lines.Add('REQUIREMENT: ' + Req);
  MemoLog.Lines.Add(StringOfChar('=', 60));

  DmCodeReview.RunReview(Req);
end;

procedure TFrmCodeReview.OnAgentLog(const Value: string);
begin
  MemoLog.Lines.Add(Value);
  MemoLog.GoToTextEnd;
end;

procedure TFrmCodeReview.OnAgentFinished(const Value: string);
begin
  MemoResult.Lines.Text := Value;
  LblStatus.Text := 'Review complete.';
  BtnStart.Enabled := True;
  MemoLog.Lines.Add(StringOfChar('=', 60));
  MemoLog.Lines.Add('Pipeline complete.');
end;

end.
