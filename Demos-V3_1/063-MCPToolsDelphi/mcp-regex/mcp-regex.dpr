program mcp_regex;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uRegexTool in 'uRegexTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP Regex';

      Server.RegisterTool('regex_match',
        function: IAiMCPTool begin Result := TRegexMatchTool.Create; end);
      Server.RegisterTool('regex_replace',
        function: IAiMCPTool begin Result := TRegexReplaceTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-regex] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-regex] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
