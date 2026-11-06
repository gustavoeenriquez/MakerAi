program RagGraphDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainRagGraphDemo in 'uMainRagGraphDemo.pas' {MainRagGraphDemo},
  uMakerAi.RAG.Graph.Builder in '..\..\Source\RAG\uMakerAi.RAG.Graph.Builder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainRagGraphDemo, MainRagGraphDemo);
  Application.Run;
end.
