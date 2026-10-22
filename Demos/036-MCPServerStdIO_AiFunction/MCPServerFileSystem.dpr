program MCPServerFileSystem;

// Servidor MCP por stdio que expone un TAiFunctions (fs_listar) a cualquier
// cliente MCP: Claude Desktop, Claude Code, o un TAiChatConnection con
// MCPClients. Ver CLAUDE.md para la configuracion de Claude Desktop.
//
// Regla de stdio: stdout es el canal JSON-RPC. Todo log va a ErrOutput.

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core,
  uMCPServerFileSystem_Tool in 'uMCPServerFileSystem_Tool.pas' {FMCPServerFileSystem_Tool: TDataModule};

begin
  try
    FMCPServerFileSystem_Tool := TFMCPServerFileSystem_Tool.Create(nil);
    try
      // Carpeta sandbox: primer parametro, o "sandbox" junto al ejecutable
      if ParamCount >= 1 then
        FMCPServerFileSystem_Tool.SandboxDir := ParamStr(1);
      FMCPServerFileSystem_Tool.PrepareSandbox;

      FMCPServerFileSystem_Tool.AiMCPStdioServer1.Start;
      WriteLn(ErrOutput, 'MCP FileSystem listo (stdio). Sandbox: ', FMCPServerFileSystem_Tool.SandboxDir);

      // Termina cuando el cliente cierra stdin (antes: while True, y el
      // proceso quedaba vivo al cerrar Claude Desktop)
      FMCPServerFileSystem_Tool.AiMCPStdioServer1.WaitForInputClosed;
      FMCPServerFileSystem_Tool.AiMCPStdioServer1.Stop;
    finally
      FMCPServerFileSystem_Tool.Free;
    end;
  except
    on E: Exception do
      WriteLn(ErrOutput, E.ClassName, ': ', E.Message);
  end;
end.
