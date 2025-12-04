// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

program AiMCPSseServerDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  // Unidades Core
  uMakerAi.MCPServer.Core in '..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  uMakerAi.MCPServer.Http in '..\..\Source\MCPServer\UMakerAi.MCPServer.Http.pas',
  uMakerAi.MCPServer.Stdio in '..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',
  uMakerAi.MCPServer.SSE in '..\..\Source\MCPServer\UMakerAi.MCPServer.SSE.pas', // <--- NUEVA UNIDAD

  // Unidades de Herramientas (Reutilizamos las que ya tienes)
  uTool.FileAccess in 'uTool.FileAccess.pas',
  uTool.SysInfo in 'uTool.SysInfo.pas';

var
  MCPServer: TAiMCPServer;

  // --- Muestra la información de ayuda ---
procedure ShowHelp;
begin
  WriteLn('');
  WriteLn('==============================================================================');
  WriteLn('  MAKERAI MCP SERVER DEMO (Multi-Protocol)');
  WriteLn('==============================================================================');
  WriteLn('');
  WriteLn('Usage:');
  WriteLn('  AiMCPSseServerDemo.exe [options]');
  WriteLn('');
  WriteLn('------------------------------------------------------------------------------');
  WriteLn('  AVAILABLE PROTOCOLS (--protocol)');
  WriteLn('------------------------------------------------------------------------------');
  WriteLn('  1. sse    (Default) Server-Sent Events over HTTP.');
  WriteLn('            Best for: Modern MCP Clients (Claude Desktop, MakerAi Clients).');
  WriteLn('            Endpoints:');
  WriteLn('              GET  /sse      -> Event Stream (Connect here first)');
  WriteLn('              POST /messages -> JSON-RPC Commands');
  WriteLn('');
  WriteLn('  2. http   Standard JSON-RPC over HTTP (Request/Response).');
  WriteLn('            Best for: Simple REST-like clients, stateless interactions.');
  WriteLn('            Endpoints:');
  WriteLn('              POST /mcp      -> JSON-RPC Commands');
  WriteLn('              GET  /mcp      -> Server Info');
  WriteLn('');
  WriteLn('  3. stdio  Standard Input/Output (Console Pipe).');
  WriteLn('            Best for: Local integration, subprocess execution.');
  WriteLn('            Mechanism: Reads JSON-RPC from Stdin, writes to Stdout.');
  WriteLn('');
  WriteLn('------------------------------------------------------------------------------');
  WriteLn('  CONFIGURATION OPTIONS');
  WriteLn('------------------------------------------------------------------------------');
  WriteLn('  --port <number>');
  WriteLn('      Sets the listening port for SSE/HTTP servers.');
  WriteLn('      Default: 8080');
  WriteLn('');
  WriteLn('  --cors-origins "<origins>"');
  WriteLn('      Comma-separated list of allowed origins for CORS.');
  WriteLn('      Use "*" to allow all.');
  WriteLn('      Default: *');
  WriteLn('');
  WriteLn('  -h, --help');
  WriteLn('      Displays this help information.');
  WriteLn('');
  WriteLn('------------------------------------------------------------------------------');
  WriteLn('  EXAMPLES');
  WriteLn('------------------------------------------------------------------------------');
  WriteLn('  > Start SSE Server on port 3000:');
  WriteLn('      AiMCPSseServerDemo.exe --protocol sse --port 3000');
  WriteLn('');
  WriteLn('  > Start standard HTTP Server:');
  WriteLn('      AiMCPSseServerDemo.exe --protocol http --port 8080');
  WriteLn('');
  WriteLn('  > Start Stdio Server (for pipe usage):');
  WriteLn('      AiMCPSseServerDemo.exe --protocol stdio');
  WriteLn('');
  WriteLn('==============================================================================');
end;

// --- Registro centralizado de herramientas ---
procedure RegisterAllToolsAndResources(ALogicServer: TAiMCPServer);
begin
  if not Assigned(ALogicServer) then
    Exit;
  WriteLn('Registering tools...');

  // Reutilizamos tus herramientas existentes sin modificar una sola línea de código
  uTool.FileAccess.RegisterTools(ALogicServer);
  uTool.SysInfo.RegisterTools(ALogicServer);

  WriteLn('Registration complete.');
end;

// --- Programa Principal ---
var
  Protocol: string;
  Port: Integer;
  CorsOrigins: string;
  ShowHelpFlag: Boolean;
  FileSettings: String;
  LoadSettings: Boolean;
  i: Integer;

begin

  ShowHelp;

  // 1. Valores por defecto
  Protocol := 'sse'; // Por defecto SSE en este demo
  Port := 8080; // SSE suele usar 8080 u 8000
  CorsOrigins := '*';
  ShowHelpFlag := False;
  LoadSettings := False;

  MCPServer := nil;

  // 2. Parsear parámetros
  if ParamCount >= 1 then
  begin
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
      else if Param = '--cors-origins' then
      begin
        if (i + 1) <= ParamCount then
        begin
          Inc(i);
          CorsOrigins := ParamStr(i);
        end;
      end
      else if (Param = '--help') or (Param = '-h') then
        ShowHelpFlag := True;

      Inc(i);
    end;
  end
  else
  begin
    FileSettings := ChangeFileExt(ParamStr(0), '.ini');
    LoadSettings := FileExists(FileSettings);
  end;

  if ShowHelpFlag then
  begin
    ShowHelp;
    Exit;
  end;

  try
    try
      WriteLn(Format('Starting server with protocol: %s', [Protocol]));

      // 3. Crear instancia según protocolo
      if SameText(Protocol, 'sse') then
      begin
        // --- AQUÍ INSTANCIAMOS EL SERVIDOR SSE ---
        MCPServer := TAiMCPSSEHttpServer.Create(nil);
      end
      else if SameText(Protocol, 'http') then
      begin
        MCPServer := TAiMCPHttpServer.Create(nil);
      end
      else if SameText(Protocol, 'stdio') then
      begin
        MCPServer := TAiMCPStdioServer.Create(nil);
      end
      else
      begin
        WriteLn(Format('Error: Unknown protocol "%s".', [Protocol]));
        Halt(1);
      end;

      // 4. Configuración
      if LoadSettings then
      begin
        MCPServer.SettingsFile := FileSettings;
        MCPServer.LoadSettingsFromFile;
      end
      else
      begin
        if Assigned(MCPServer) then
        begin
          MCPServer.Port := Port;
          MCPServer.CorsEnabled := True; // SSE suele necesitar CORS
          MCPServer.CorsAllowedOrigins := CorsOrigins;
        end;
      end;

      // 5. Registrar herramientas
      RegisterAllToolsAndResources(MCPServer);

      // 6. Iniciar
      if Assigned(MCPServer) then
      begin
        MCPServer.Start;

        if MCPServer is TAiMCPSSEHttpServer then
        begin
          WriteLn(Format('✅ MCP SSE Server listening on http://localhost:%d/sse', [MCPServer.Port]));
          WriteLn(Format('   POST Endpoint: http://localhost:%d/messages', [MCPServer.Port]));
        end
        else if MCPServer is TAiMCPHttpServer then
          WriteLn(Format('✅ MCP HTTP Server listening on port %d.', [MCPServer.Port]))
        else
          WriteLn('✅ MCP Stdio Server running.');
      end;

      WriteLn('Press Ctrl+C to stop.');

      // Bucle infinito
      while True do
        Sleep(1000);

    except
      on E: Exception do
      begin
        WriteLn('Error: ' + E.Message);
        Halt(1);
      end;
    end;
  finally
    if Assigned(MCPServer) then
    begin
      MCPServer.Stop;
      MCPServer.Free;
    end;
  end;

end.
