program mcp_datetime;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uDateTimeTool in 'uDateTimeTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP DateTime';

      Server.RegisterTool('datetime_now',
        function: IAiMCPTool begin Result := TDateTimeNowTool.Create; end);
      Server.RegisterTool('datetime_add',
        function: IAiMCPTool begin Result := TDateTimeAddTool.Create; end);
      Server.RegisterTool('datetime_diff',
        function: IAiMCPTool begin Result := TDateTimeDiffTool.Create; end);
      Server.RegisterTool('datetime_format',
        function: IAiMCPTool begin Result := TDateTimeFormatTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-datetime] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-datetime] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
