program MCPServerAiFunctions;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Bridge in 'uMakerAi.MCPServer.Bridge.pas',
  uMCPServerAiFunctionsDm in 'uMCPServerAiFunctionsDm.pas' {MCPServerAiFunctionsDm: TDataModule};

begin
  try

    MCPServerAiFunctionsDm := TMCPServerAiFunctionsDm.Create(Nil);
    Try
      MCPServerAiFunctionsDm.StartServer;

      while True do
        Sleep(1000);
    Finally
      MCPServerAiFunctionsDm.Free;
    End;

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
