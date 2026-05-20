program FMXAnalysisPipeline;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmAnalysis in 'uFrmAnalysis.pas' {FrmAnalysis},
  uDmAnalysis in 'uDmAnalysis.pas' {DmAnalysis: TDataModule},
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude,
  uMakerAi.Agents.Node.LLM;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmAnalysis, FrmAnalysis);
  Application.Run;
end.
