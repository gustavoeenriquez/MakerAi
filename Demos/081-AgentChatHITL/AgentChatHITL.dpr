program AgentChatHITL;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainAgentChatHITL in 'uMainAgentChatHITL.pas' {FormAgentChatHITL};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormAgentChatHITL, FormAgentChatHITL);
  Application.Run;
end.
