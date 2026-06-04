unit uFrmDebate;

// =============================================================================
// MakerAI - Demo 055: FMX Multi-Agent Debate
// Formulario principal - interfaz visual del debate.
//
// UI:
//   [Top]    Tema del debate + boton Iniciar + estado
//   [Client] Transcripcion del debate (TMemo, read-only)
//   [Bottom] Veredicto final (TMemo, read-only)
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
  uMakerAi.Agents;

type
  TFrmDebate = class(TForm)
    LayoutTop       : TLayout;
    LblTema         : TLabel;
    EdTopic         : TEdit;
    BtnStart        : TButton;
    LayoutStatus    : TLayout;
    LblStatus       : TLabel;
    LayoutVeredicto : TLayout;
    LblVeredicto    : TLabel;
    MemoVeredicto   : TMemo;
    LayoutLog       : TLayout;
    LblLog          : TLabel;
    MemoLog         : TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
  private
    procedure OnAgentLog(const Value: string);
    procedure OnAgentFinished(const Value: string);
    // Los eventos son del tipo TDebateLogEvent / TDebateFinishedEvent (procedure of object)
  end;

var
  FrmDebate: TFrmDebate;

implementation

{$R *.fmx}

uses
  uDmDebate;

{ TFrmDebate }

procedure TFrmDebate.FormCreate(Sender: TObject);
begin
  DmDebate := TDmDebate.Create(Self);
  DmDebate.OnLog      := OnAgentLog;
  DmDebate.OnFinished := OnAgentFinished;
  LblStatus.Text := 'Listo. Ingresa un tema y presiona Iniciar Debate.';
end;

procedure TFrmDebate.FormDestroy(Sender: TObject);
begin
  DmDebate.Free;
  DmDebate := nil;
end;

procedure TFrmDebate.BtnStartClick(Sender: TObject);
var
  Topic: string;
begin
  Topic := Trim(EdTopic.Text);
  if Topic = '' then
  begin
    ShowMessage('Ingresa un tema para el debate.');
    Exit;
  end;

  MemoLog.Lines.Clear;
  MemoVeredicto.Lines.Clear;
  BtnStart.Enabled := False;
  LblStatus.Text := 'Debate en progreso...';

  MemoLog.Lines.Add('TEMA: ' + Topic);
  MemoLog.Lines.Add(StringOfChar('=', 60));

  DmDebate.RunDebate(Topic);
end;

procedure TFrmDebate.OnAgentLog(const Value: string);
begin
  MemoLog.Lines.Add(Value);
  // Scroll al final
  MemoLog.GoToTextEnd;
end;

procedure TFrmDebate.OnAgentFinished(const Value: string);
begin
  MemoVeredicto.Lines.Text := Value;
  LblStatus.Text := 'Debate finalizado.';
  BtnStart.Enabled := True;
  MemoLog.Lines.Add(StringOfChar('=', 60));
  MemoLog.Lines.Add('Debate completado.');
end;

end.
