program mcp_csv;

// =============================================================================
// mcp-csv  —  CSV file and string manipulation MCP server
//
// Tools exposed:
//   csv_parse   — Parse CSV and return as ASCII table or JSON
//   csv_to_json — Convert CSV to JSON array of objects
//   csv_filter  — Filter CSV rows where a column matches a value
//   csv_info    — Get metadata about a CSV file or string
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uCsvTool in 'uCsvTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP CSV Server';

      Server.RegisterTool('csv_parse',
        function: IAiMCPTool begin Result := TCsvParseTool.Create; end);

      Server.RegisterTool('csv_to_json',
        function: IAiMCPTool begin Result := TCsvToJsonTool.Create; end);

      Server.RegisterTool('csv_filter',
        function: IAiMCPTool begin Result := TCsvFilterTool.Create; end);

      Server.RegisterTool('csv_info',
        function: IAiMCPTool begin Result := TCsvInfoTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-csv] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-csv] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
