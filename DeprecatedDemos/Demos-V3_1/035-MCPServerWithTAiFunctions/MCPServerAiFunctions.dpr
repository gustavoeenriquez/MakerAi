program MCPServerAiFunctions;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core,
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
