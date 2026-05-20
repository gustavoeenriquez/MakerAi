program mcp_tokenizer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uTokenizerTool in 'uTokenizerTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP Tokenizer';

      Server.RegisterTool('tokenize',
        function: IAiMCPTool begin Result := TTokenizeTool.Create; end);
      Server.RegisterTool('split_text',
        function: IAiMCPTool begin Result := TSplitTextTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-tokenizer] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-tokenizer] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
