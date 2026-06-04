program mcp_ini;

// =============================================================================
// mcp-ini  --  INI file read/write/delete/list/read-section MCP server
//
// Tools exposed:
//   ini_read         -- Read a value from an INI file
//   ini_write        -- Write a value to an INI file
//   ini_delete       -- Delete a key or entire section from an INI file
//   ini_list         -- List all sections, or all keys in a section
//   ini_read_section -- Read all key=value pairs in a section
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uIniTool in 'uIniTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP INI Server';

      Server.RegisterTool('ini_read',
        function: IAiMCPTool begin Result := TIniReadTool.Create; end);

      Server.RegisterTool('ini_write',
        function: IAiMCPTool begin Result := TIniWriteTool.Create; end);

      Server.RegisterTool('ini_delete',
        function: IAiMCPTool begin Result := TIniDeleteTool.Create; end);

      Server.RegisterTool('ini_list',
        function: IAiMCPTool begin Result := TIniListTool.Create; end);

      Server.RegisterTool('ini_read_section',
        function: IAiMCPTool begin Result := TIniReadSectionTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-ini] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-ini] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
