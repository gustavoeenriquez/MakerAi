program AIChatDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmAIChat in 'uFrmAIChat.pas' {FrmAIChat};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmAIChat, FrmAIChat);
  Application.Run;
end.
