program mcp_base64;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uBase64Tool in 'uBase64Tool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP Base64 Server';

      Server.RegisterTool('base64_encode',
        function: IAiMCPTool begin Result := TBase64EncodeTool.Create; end);
      Server.RegisterTool('base64_decode',
        function: IAiMCPTool begin Result := TBase64DecodeTool.Create; end);
      Server.RegisterTool('base64_encode_file',
        function: IAiMCPTool begin Result := TBase64EncodeFileTool.Create; end);
      Server.RegisterTool('base64_decode_file',
        function: IAiMCPTool begin Result := TBase64DecodeFileTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-base64] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-base64] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
