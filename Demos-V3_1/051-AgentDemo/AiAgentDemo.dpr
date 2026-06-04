program AiAgentDemo;



uses
  System.StartUpCopy,
  FMX.Forms,
  uAiAssistantDemo in 'uAiAssistantDemo.pas' {Form70};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm70, Form70);
  Application.Run;
end.
