program mcp_text_transform;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uTextTransformTool in 'uTextTransformTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP Text Transform';

      Server.RegisterTool('base64_encode',
        function: IAiMCPTool begin Result := TBase64EncodeTool.Create; end);
      Server.RegisterTool('base64_decode',
        function: IAiMCPTool begin Result := TBase64DecodeTool.Create; end);
      Server.RegisterTool('compute_hash',
        function: IAiMCPTool begin Result := TComputeHashTool.Create; end);
      Server.RegisterTool('string_transform',
        function: IAiMCPTool begin Result := TStringTransformTool.Create; end);
      Server.RegisterTool('url_encode',
        function: IAiMCPTool begin Result := TUrlEncodeTool.Create; end);
      Server.RegisterTool('url_decode',
        function: IAiMCPTool begin Result := TUrlDecodeTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-text-transform] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-text-transform] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
