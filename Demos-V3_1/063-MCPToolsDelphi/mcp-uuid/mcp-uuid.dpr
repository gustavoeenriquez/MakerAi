program mcp_uuid;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uUuidTool in 'uUuidTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP UUID Server';

      Server.RegisterTool('uuid_generate',
        function: IAiMCPTool begin Result := TUuidGenerateTool.Create; end);
      Server.RegisterTool('uuid_validate',
        function: IAiMCPTool begin Result := TUuidValidateTool.Create; end);
      Server.RegisterTool('uuid_parse',
        function: IAiMCPTool begin Result := TUuidParseTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-uuid] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-uuid] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
