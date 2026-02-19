program McpFileServerSSE;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  // 1. Unidades del Servidor MCP
  uMakerAi.MCPServer.Core in '..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  uMakerAi.MCPServer.SSE in '..\Source\MCPServer\UMakerAi.MCPServer.SSE.pas', // La unidad SSE corregida

  // 2. Unidades de Herramientas
  uTool.Sandbox in 'uTool.Sandbox.pas'; // Tus herramientas de archivos

var
  MCPServer: TAiMCPSSEHttpServer;

begin
  try
    // --- 1. Crear Instancia del Servidor SSE ---
    MCPServer := TAiMCPSSEHttpServer.Create(nil);
    try
      // --- 2. Configuración ---
      MCPServer.Port := 8080;          // Puerto TCP
      MCPServer.CorsEnabled := True;   // Necesario para Claude Desktop
      MCPServer.CorsAllowedOrigins := '*';
      MCPServer.ServerName := 'Delphi-MCP-SSE-Files';

      // Configuración opcional de rutas (estos son los defaults)
      // MCPServer.SseEndpoint := '/sse';
      // MCPServer.MessagesEndpoint := '/messages';

      // --- 3. Registrar Herramientas ---
      WriteLn('Registrando herramientas de archivo...');
      uTool.Sandbox.RegisterSandboxTools(MCPServer);

      // --- 4. Iniciar Servidor ---
      MCPServer.Start;

      WriteLn('');
      WriteLn('===========================================================');
      WriteLn('✅ SERVIDOR MCP SSE ACTIVO');
      WriteLn(Format('   Puerto: %d', [MCPServer.Port]));
      WriteLn(Format('   Carpeta: %s', [uTool.Sandbox.SANDBOX_ROOT]));
      WriteLn('===========================================================');
      WriteLn('');
      WriteLn('--- CONFIGURACIÓN PARA CLAUDE DESKTOP ---');
      WriteLn('Copia y pega esto en tu "claude_desktop_config.json":');
      WriteLn('');
      WriteLn('{');
      WriteLn('  "mcpServers": {');
      WriteLn('    "delphi-sse-files": {');
      WriteLn('      "command": "ruta\\al\\ejecutable\\McpFileServerSSE.exe",');
      WriteLn('      "args": []');
      WriteLn('    }');
      WriteLn('  }');
      WriteLn('}');
      WriteLn('');
      WriteLn('NOTA: Claude Desktop lanzará este ejecutable automáticamente.');
      WriteLn('Si quieres probarlo manualmente vía HTTP, el endpoint SSE es:');
      WriteLn('http://localhost:8080/sse');
      WriteLn('');
      WriteLn('Presiona Enter para detener el servidor...');

      // Mantener vivo hasta que el usuario decida salir
      ReadLn;

    except
      on E: Exception do
        WriteLn('Error Fatal al iniciar: ' + E.Message);
    end;
  finally
    // Limpieza
    if Assigned(MCPServer) then
    begin
      MCPServer.Stop;
      MCPServer.Free;
    end;
  end;
end.
