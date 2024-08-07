program RagDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uRagChatMain in 'uRagChatMain.pas' {Form69},
  uAiModule in '..\..\..\..\..\..\Delphi11\Proyectos\Sitis\DemoIA\uAiModule.pas' {AiModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm69, Form69);
  Application.CreateForm(TAiModule, AiModule);
  Application.Run;
end.
