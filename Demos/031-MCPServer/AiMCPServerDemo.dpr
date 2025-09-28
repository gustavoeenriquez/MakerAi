program AiMCPServerDemo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  uTool.FileAccess in 'uTool.FileAccess.pas',
  uTool.SysInfo in 'uTool.SysInfo.pas',
  uMakerAi.MCPServer.Core in '..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Http in '..\..\Source\MCPServer\UMakerAi.MCPServer.Http.pas',
  UMakerAi.MCPServer.Stdio in '..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas';

// --- Añade aquí las unidades de tus herramientas y recursos ---
// uMyCoolTool in 'uMyCoolTool.pas',
// uMyAwesomeResource in 'uMyAwesomeResource.pas';

// --- Declaración de variables para los servidores ---
var
  MCPServer: TAiMCPServer;


  // Registro de funciones de las diferentes clases.

  {
    procedure RegisterFileAccessTools(ALogicServer: TAiMCPServer);
    begin
    if not Assigned(ALogicServer) then
    raise Exception.Create('LogicServer no puede ser nulo para registrar herramientas.');

    // 1. Registrar la herramienta para listar archivos
    ALogicServer.RegisterTool('list_files',
    function: IAiMCPTool
    begin
    Result := TListFilesTool.Create;
    end);

    // 2. Registrar la herramienta para leer archivos
    ALogicServer.RegisterTool('read_file',
    function: IAiMCPTool
    begin
    Result := TReadFileTool.Create;
    end);

    // 3. Registrar la herramienta para escribir archivos
    ALogicServer.RegisterTool('write_file',
    function: IAiMCPTool
    begin
    Result := TWriteFileTool.Create;
    end);
    end;
  }

  // --- Muestra la información de ayuda en la consola ---
procedure ShowHelp;
begin
  WriteLn('MakerAI MCP Console Server');
  WriteLn('--------------------------');
  WriteLn('Usage: MCPConsoleServer.exe [options]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --protocol <http|stdio>  Specifies the communication protocol. Default: http');
  WriteLn;
  WriteLn('HTTP Server Options:');
  WriteLn('  --port <number>          The port for the HTTP server. Default: 3000');
  WriteLn('  --cors-enabled <true|false>');
  WriteLn('                           Enables or disables CORS. Default: true');
  WriteLn('  --cors-origins "<origins>"');
  WriteLn('                           Comma-separated list of allowed origins.');
  WriteLn('                           Use "*" for all. Example: "http://localhost:8000,https://my-app.com"');
  WriteLn('                           Default: *');
  WriteLn;
  WriteLn('General Options:');
  WriteLn('  -h, --help               Displays this help information and exits.');
  WriteLn;
  WriteLn('Example (HTTP): MCPConsoleServer.exe --port 8080 --cors-origins "http://my.domain.com"');
  WriteLn('Example (Stdio): MCPConsoleServer.exe --protocol stdio');
end;

// --- Lugar centralizado para registrar todas tus herramientas y recursos ---
procedure RegisterAllToolsAndResources(ALogicServer: TAiMCPServer);
begin
  if not Assigned(ALogicServer) then
    Exit;

  WriteLn('Registering tools and resources...');

  uTool.FileAccess.RegisterTools(ALogicServer);
  uTool.SysInfo.RegisterTools(ALogicServer);

  WriteLn('Registration complete.');
end;

// --- Programa Principal ---
var
  Protocol: string;
  Port: Integer;
  CorsEnabled: Boolean;
  CorsOrigins: string;
  ShowHelpFlag: Boolean;
  FileSettings : String;
  LoadSettings : Boolean;
  i: Integer;

begin
  // --- 1. Valores por defecto ---
  Protocol := 'http';
  Port := 3000;
  ShowHelpFlag := False;
  // --- NUEVOS VALORES POR DEFECTO PARA CORS ---
  CorsEnabled := False;
  CorsOrigins := '*';

  MCPServer := nil;

  // --- 2. Parsear los parámetros de la línea de comandos (sección actualizada) ---
  If ParamCount >= 1 then
  Begin
  i := 1;
    while i <= ParamCount do
    begin
      var
      Param := LowerCase(ParamStr(i));
      if Param = '--protocol' then
      begin
        if (i + 1) <= ParamCount then
        begin
          Inc(i);
          Protocol := LowerCase(ParamStr(i));
        end;
      end
      else if Param = '--port' then
      begin
        if (i + 1) <= ParamCount then
        begin
          Inc(i);
          Port := StrToIntDef(ParamStr(i), Port);
        end;
      end
      // --- NUEVO: PARSEO DE PARÁMETROS CORS ---
      else if Param = '--cors-enabled' then
      begin
        if (i + 1) <= ParamCount then
        begin
          Inc(i);
          if SameText(ParamStr(i), 'false') then
            CorsEnabled := False
          else
            CorsEnabled := True; // Cualquier otro valor (incluido 'true') lo activa
        end;
      end
      else if Param = '--cors-origins' then
      begin
        if (i + 1) <= ParamCount then
        begin
          Inc(i);
          CorsOrigins := ParamStr(i); // Mantener mayúsculas/minúsculas de los dominios
        end;
      end
      // ---------------------------------------
      else if (Param = '--help') or (Param = '-h') then
      begin
        ShowHelpFlag := True;
      end;
      Inc(i);
    end;
  End
  Else
  Begin
     //aquí puede asignar el nombre del archivo con los parámetros por defecto
     //Se asigna el mismo nombre del ejecutable.ini
     FileSettings  := ChangeFileExt(ParamStr(0),'.ini');
     LoadSettings := FileExists(FileSettings);  //Si el archivo existe lo marca para cargarlo
  End;

  // --- 3. Mostrar ayuda y salir ---
  if ShowHelpFlag then
  begin
    ShowHelp;
    Exit;
  end;

  try
    Try
      WriteLn(Format('Starting server with protocol: %s', [Protocol]));

      // --- 4. Crear la instancia del servidor ---
      if SameText(Protocol, 'http') then
      begin
        MCPServer := TAiMCPHttpServer.Create(nil);
      end
      else if SameText(Protocol, 'stdio') then
      begin
        MCPServer := TAiMCPStdioServer.Create(nil);
      end
      else
      begin
        WriteLn(Format('Error: Unknown protocol "%s". Use "http" or "stdio".', [Protocol]));
        ShowHelp;
        Halt(1);
      end;



      If LoadSettings then
      Begin
         MCPServer.SettingsFile := FileSettings;
         MCPServer.LoadSettingsFromFile;

      End
      Else
      Begin
      // --- 5. APLICAR CONFIGURACIÓN DESDE LÍNEA DE COMANDOS ---
      if Assigned(MCPServer) then
      begin
        MCPServer.Port := Port;
        MCPServer.CorsEnabled := CorsEnabled;
        MCPServer.CorsAllowedOrigins := CorsOrigins;
        MCPServer.User := '';
      end;
      End;

      // --- 6. Registrar herramientas ---
      RegisterAllToolsAndResources(MCPServer);

      // --- 7. Iniciar el servidor ---
      if Assigned(MCPServer) then
      begin
        MCPServer.Start; // Start ahora usará el puerto actualizado
        WriteLn(Format('HTTP server listening on port %d.', [MCPServer.Port]));
        WriteLn(Format('CORS Enabled: %s, Allowed Origins: %s', [BoolToStr(CorsEnabled, True), CorsOrigins]));
      end
      else if Assigned(MCPServer) then
      begin
        MCPServer.Start;
        WriteLn('Stdio server listening on standard input/output.');
      end;

      WriteLn('Server is running. Press Ctrl+C to stop.');

      // --- 8. Bucle infinito ---
      while True do
      begin
        Sleep(1000);
      end;

    except
      on E: EControlC do
      begin
        WriteLn;
        WriteLn('Ctrl+C detected. Shutting down gracefully...');
      end;
      on E: Exception do
      begin
        WriteLn(Format('FATAL ERROR: %s', [E.Message]));
        Halt(1);
      end;
    end;

    // --- 9. Limpieza final ---
  finally
    WriteLn('Stopping server...');
    if Assigned(MCPServer) then
    begin
      MCPServer.Stop;
      MCPServer.Free;
    end;
    WriteLn('Server stopped. Goodbye!');
  end;

end.
