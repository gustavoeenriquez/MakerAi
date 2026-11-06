program FileSystemMCP;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uDMFunction in 'uDMFunction.pas' {DmFunction: TDataModule},
  MCP.Filesystem in 'MCP.Filesystem.pas';

begin

  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, '=== MCP Server con TAiFunctions System File Demo ===');

  try
    DmFunction := TDmFunction.Create(Nil);
    Try
      //DmFunction.AiMCPHttpServer1.Start;
      DmFunction.AiMCPStdioServer1.Start;

      WriteLn(ErrOutput, '=== Server Started at  localhost:3000/mcp ===');
      //WriteLn(ErrOutput, '=== Server Started StdIO ===');

      while True do
        Sleep(1000);
    Finally
      DmFunction.AiMCPHttpServer1.Free;
    End;

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
