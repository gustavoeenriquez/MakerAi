program McpFileServerStIO;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  uMakerAi.MCPServer.Stdio in '..\Source\MCPServer\uMakerAi.MCPServer.Stdio.pas',
  uTool.Sandbox in 'uTool.Sandbox.pas',
  uTool.WorldTime in 'uTool.WorldTime.pas';

var
  MCPServer: TAiMCPStdioServer;

begin
  try
    // 1. Crear el servidor (Modo Stdio para integración local)
    // Nota: Usamos Stdio porque es el estándar para herramientas locales de IA
    MCPServer := TAiMCPStdioServer.Create(nil);
    try
      // 2. Configuración básica
      MCPServer.ServerName := 'Delphi-FileManager-MCP';

      // 3. Registrar las herramientas del Sandbox
      WriteLn(ErrOutput, 'Registrando herramientas de archivo...'); // ErrOutput para no ensuciar Stdin/Stdout
      uTool.Sandbox.RegisterSandboxTools(MCPServer);
      uTool.WorldTime.RegisterWorldTimeTool(MCPServer);

      // 4. Iniciar servidor
      MCPServer.Start;
      WriteLn(ErrOutput, '✅ Servidor MCP de Archivos iniciado.');
      WriteLn(ErrOutput, '📂 Carpeta controlada: ' + uTool.Sandbox.SANDBOX_ROOT);
      WriteLn(ErrOutput, 'Esperando comandos JSON-RPC vía Stdin...');

      // 5. Bucle infinito para mantener la app viva
      while True do
        Sleep(1000);

    except
      on E: Exception do
        WriteLn(ErrOutput, 'Error Fatal: ' + E.Message);
    end;
  finally
    MCPServer.Free;
  end;
end.
