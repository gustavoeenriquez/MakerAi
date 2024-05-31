program RagDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uRagChatMain in 'uRagChatMain.pas' {Form69};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm69, Form69);
  Application.Run;
end.
