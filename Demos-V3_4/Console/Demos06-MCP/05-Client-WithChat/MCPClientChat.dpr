program MCPClientChat;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 06-MCP / 05-Client-WithChat
// =============================================================================
// Integra un servidor MCP con un chat LLM via TAiFunctions.
// El LLM puede llamar herramientas MCP de forma transparente.
//
// Conceptos que cubre:
//   - TAiFunctions: coleccion de herramientas para el LLM
//   - TAiFunctions.MCPClients.Add: registrar un servidor MCP manualmente
//   - TMCPClientItem.TransportType := tpStdIo
//   - TMCPClientItem.Params.Values['Command']: ruta al servidor
//   - TAiChatConnection: conexion de chat con soporte de tools
//   - FAiConn.AiFunctions: enlazar herramientas al chat
//   - El LLM llama automaticamente las herramientas MCP cuando las necesita
//
// Prerequisito: compilar 01-Server-StdIO (Win64 Release).
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Tools.Functions,
  uMakerAi.MCPClient.Core,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude;

const
  DRIVER     = 'Claude';
  MODEL      = 'claude-haiku-4-5-20251001';
  API_KEY    = '@CLAUDE_API_KEY';
  SERVER_EXE_REL = '..\01-Server-StdIO\Win64\Release\MCPServerStdIO.exe';

// =============================================================================
//  Demo
// =============================================================================
procedure RunDemo;
var
  Functions   : TAiFunctions;
  AiConn      : TAiChatConnection;
  ClientItem  : TMCPClientItem;
  ServerPath  : String;
  Pregunta    : String;
  Respuesta   : String;
begin
  Writeln('=== MCPClientChat ===');
  Writeln('LLM + herramientas MCP (get_datetime, greet)');
  Writeln;

  ServerPath := ExpandFileName(ExtractFilePath(ParamStr(0)) + SERVER_EXE_REL);
  Writeln('Servidor MCP: ', ServerPath);
  Writeln;

  Functions := TAiFunctions.Create(nil);
  AiConn    := TAiChatConnection.Create(nil);
  try
    // Registrar el servidor MCP StdIO manualmente en TAiFunctions
    if TFile.Exists(ServerPath) then
    begin
      ClientItem := Functions.MCPClients.Add;
      ClientItem.Name                    := 'makerai-demo';
      ClientItem.TransportType           := tpStdIo;
      ClientItem.Params.Values['Command'] := ServerPath;

      // Inicializar el proceso del servidor
      if not ClientItem.MCPClient.Initialized then
        ClientItem.MCPClient.Initialize;

      if ClientItem.MCPClient.Available then
        Writeln('Servidor MCP conectado y disponible.')
      else
        Writeln('AVISO: Servidor MCP no disponible. Verificar que exista el exe.');
    end
    else
    begin
      Writeln('AVISO: Servidor no encontrado. Continuando sin herramientas MCP.');
      Writeln('Compila primero 01-Server-StdIO (Win64 Release).');
    end;
    Writeln;

    // Configurar TAiChatConnection con las funciones MCP
    AiConn.AiFunctions              := Functions;
    AiConn.DriverName               := DRIVER;
    AiConn.Model                    := MODEL;
    AiConn.Params.Values['ApiKey']  := API_KEY;
    AiConn.RegisterUserParam(DRIVER, 'Tool_Active',     'True');
    AiConn.RegisterUserParam(DRIVER, 'Asynchronous',    'False');
    AiConn.RegisterUserParam(DRIVER, 'Max_Tokens',      '2048');
    AiConn.RegisterUserParam(DRIVER, 'ResponseTimeOut', '60000');

    AiConn.SystemPrompt.Text :=
      'Eres un asistente util. Tienes acceso a herramientas MCP: ' +
      'get_datetime (para saber la fecha y hora) y greet (para saludar). ' +
      'Usa las herramientas cuando sea apropiado. Responde en espanol.';

    // Ejecutar consultas que usan las herramientas MCP
    Writeln(StringOfChar('-', 60));
    Pregunta := 'Que fecha y hora es ahora?';
    Writeln('Pregunta: ', Pregunta);
    Respuesta := AiConn.AddMessageAndRun(Pregunta, 'user', []);
    Writeln('Respuesta: ', Respuesta);
    Writeln;

    Writeln(StringOfChar('-', 60));
    Pregunta := 'Saluda a "MakerAI" en espanol.';
    Writeln('Pregunta: ', Pregunta);
    Respuesta := AiConn.AddMessageAndRun(Pregunta, 'user', []);
    Writeln('Respuesta: ', Respuesta);
    Writeln;

    Writeln(StringOfChar('-', 60));
    Pregunta := 'Saluda a "Developer" en ingles y dime tambien la hora actual en formato ISO.';
    Writeln('Pregunta: ', Pregunta);
    Respuesta := AiConn.AddMessageAndRun(Pregunta, 'user', []);
    Writeln('Respuesta: ', Respuesta);
    Writeln;

  finally
    AiConn.Free;
    Functions.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
