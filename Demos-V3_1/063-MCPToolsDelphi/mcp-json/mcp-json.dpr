program mcp_json;

// =============================================================================
// mcp-json  —  JSON file and string manipulation MCP server
//
// Tools exposed:
//   json_read    — Read a JSON file from disk and return formatted content
//   json_write   — Write JSON content to a file
//   json_query   — Query a value inside JSON using dot-path notation
//   json_format  — Format (pretty-print) or minify a JSON string
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uJsonTool in 'uJsonTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP JSON Server';

      Server.RegisterTool('json_read',
        function: IAiMCPTool begin Result := TJsonReadTool.Create; end);

      Server.RegisterTool('json_write',
        function: IAiMCPTool begin Result := TJsonWriteTool.Create; end);

      Server.RegisterTool('json_query',
        function: IAiMCPTool begin Result := TJsonQueryTool.Create; end);

      Server.RegisterTool('json_format',
        function: IAiMCPTool begin Result := TJsonFormatTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-json] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-json] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
