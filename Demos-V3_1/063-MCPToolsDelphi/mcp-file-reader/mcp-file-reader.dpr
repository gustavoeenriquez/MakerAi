program mcp_file_reader;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uFileReaderTool in 'uFileReaderTool.pas';

var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP File Reader';

      Server.RegisterTool('file_read',
        function: IAiMCPTool begin Result := TFileReadTool.Create; end);
      Server.RegisterTool('file_list',
        function: IAiMCPTool begin Result := TFileListTool.Create; end);
      Server.RegisterTool('file_info',
        function: IAiMCPTool begin Result := TFileInfoTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-file-reader] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-file-reader] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
