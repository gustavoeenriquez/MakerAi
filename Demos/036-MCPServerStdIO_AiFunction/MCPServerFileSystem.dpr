program MCPServerFileSystem;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Bridge in 'uMakerAi.MCPServer.Bridge.pas',
  uMCPServerFileSystem_Tool in 'uMCPServerFileSystem_Tool.pas' {FMCPServerFileSystem_Tool: TDataModule};

begin
  try

    FMCPServerFileSystem_Tool := TFMCPServerFileSystem_Tool.Create(Nil);
    Try

      FMCPServerFileSystem_Tool.AiMCPStdioServer1.Start;

      WriteLn(ErrOutput, 'Esperando peticiones JSON-RPC...');

      while True do
        Sleep(1000);

    Finally
      FMCPServerFileSystem_Tool.Free;
    End;

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
