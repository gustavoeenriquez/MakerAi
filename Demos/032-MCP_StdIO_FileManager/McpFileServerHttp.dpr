program McpFileServerHttp;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.MCPServer.Core in '..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  uMakerAi.MCPServer.Http in '..\Source\MCPServer\uMakerAi.MCPServer.Http.pas',
  uTool.Sandbox in 'uTool.Sandbox.pas',
  uTool.WorldTime in 'uTool.WorldTime.pas';

var
  MCPServer: TAiMCPHttpServer; // Cambiamos Stdio por Http

begin
  try
    // 1. Instanciar el servidor HTTP
    MCPServer := TAiMCPHttpServer.Create(nil);
    try
      // 2. Configuración
      MCPServer.Port := 8080; // Puerto donde escuchará
      MCPServer.CorsEnabled := True; // Permitir acceso desde navegadores
      MCPServer.CorsAllowedOrigins := '*';
      MCPServer.ServerName := 'Delphi-MCP-Http-Files';

      // 3. Registrar las herramientas (Exactamente igual que antes)
      WriteLn('Registrando herramientas de archivo...');
      uTool.Sandbox.RegisterSandboxTools(MCPServer);
      uTool.WorldTime.RegisterWorldTimeTool(MCPServer);

      // 4. Iniciar
      MCPServer.Start;

      WriteLn('');
      WriteLn('==================================================');
      WriteLn(Format('✅ Servidor HTTP Escuchando en el puerto %d', [MCPServer.Port]));
      WriteLn(Format('📂 Carpeta controlada: %s', [uTool.Sandbox.SANDBOX_ROOT]));
      WriteLn('==================================================');
      WriteLn('Endpoints disponibles:');
      WriteLn('  GET  http://localhost:8080/mcp  -> Info del servidor');
      WriteLn('  POST http://localhost:8080/mcp  -> Enviar comandos JSON-RPC');
      WriteLn('==================================================');
      WriteLn('Presiona Enter para salir...');

      ReadLn; // Esperar a que el usuario presione Enter para cerrar

    except
      on E: Exception do
        WriteLn('Error Fatal: ' + E.Message);
    end;
  finally
    MCPServer.Stop;
    MCPServer.Free;
  end;
end.
