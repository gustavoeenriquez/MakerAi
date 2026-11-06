program FMXCodeReviewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmCodeReview in 'uFrmCodeReview.pas' {FrmCodeReview},
  uDmCodeReview in 'uDmCodeReview.pas' {DmCodeReview: TDataModule},
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude,
  uMakerAi.Agents.Node.LLM;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmCodeReview, FrmCodeReview);
  Application.Run;
end.
