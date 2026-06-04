program mcp_zip;

// =============================================================================
// mcp-zip  --  ZIP file list/extract/create/add MCP server
//
// Tools exposed:
//   zip_list    -- List contents of a ZIP file
//   zip_extract -- Extract contents of a ZIP file
//   zip_create  -- Create a new ZIP file from a list of files
//   zip_add     -- Add a file to an existing ZIP (or create if not exists)
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uZipTool in 'uZipTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP ZIP Server';

      Server.RegisterTool('zip_list',
        function: IAiMCPTool begin Result := TZipListTool.Create; end);

      Server.RegisterTool('zip_extract',
        function: IAiMCPTool begin Result := TZipExtractTool.Create; end);

      Server.RegisterTool('zip_create',
        function: IAiMCPTool begin Result := TZipCreateTool.Create; end);

      Server.RegisterTool('zip_add',
        function: IAiMCPTool begin Result := TZipAddTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-zip] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-zip] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
