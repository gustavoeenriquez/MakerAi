program mcp_calculator;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uCalculatorTool in 'uCalculatorTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP Calculator';

      Server.RegisterTool('calculate',
        function: IAiMCPTool begin Result := TCalculatorTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-calculator] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-calculator] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
