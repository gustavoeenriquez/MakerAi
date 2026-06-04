program FMXMultiAgentDebate;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmDebate in 'uFrmDebate.pas' {FrmDebate},
  uDmDebate in 'uDmDebate.pas' {DmDebate: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmDebate, FrmDebate);
  Application.Run;
end.
