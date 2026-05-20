program AutoMCP;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 06-MCP / 06-AutoMCP
// =============================================================================
// AutoMCP: descarga e instala servidores MCP desde el registro PPM
// (https://registry.pascalai.org) automaticamente.
// El LLM puede usar las herramientas instaladas sin configuracion manual.
//
// Conceptos que cubre:
//   - FFunctions.AutoMCPConfig.Active: activa el sistema AutoMCP
//   - FFunctions.AutoMCPConfig.RegistryUrl: URL del registro PPM
//   - FFunctions.InstallMCPFromPPM(name, params, env, registryUrl)
//   - FFunctions.GetAutoMCPSystemPrompt(): prompt de sistema para AutoMCP
//   - TMCPClientItem.MCPClient.Initialize: iniciar el proceso del servidor
//   - TMCPClientItem.MCPClient.Available: verificar disponibilidad
//   - TAiChatConnection con herramientas MCP autoinstaladas
//
// Paquetes PPM instalados: mcp-calculator (si esta disponible en el registro)
// Registro PPM: https://registry.pascalai.org
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
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
  DRIVER      = 'Claude';
  MODEL       = 'claude-haiku-4-5-20251001';
  API_KEY     = '@CLAUDE_API_KEY';
  REGISTRY    = 'https://registry.pascalai.org';
  MCP_PACKAGE = 'mcp-calculator';

// =============================================================================
//  Demo
// =============================================================================
type
  TAutoMCPDemo = class
  private
    FFunctions: TAiFunctions;
    FAiConn   : TAiChatConnection;
    FLastError: String;

    procedure OnLog   (Sender: TObject; const Msg: String);
    procedure OnStatus(Sender: TObject; const Msg: String);
    procedure OnError (Sender: TObject; const ErrorMsg: String;
      AException: Exception; const AResponse: IHTTPResponse);
    procedure OnState (Sender: TObject; State: TAiChatState; const Desc: String);

    procedure InstalarPaquete;
    procedure ListarHerramientas;
    procedure ChatConHerramientas;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Run;
  end;

constructor TAutoMCPDemo.Create;
begin
  inherited;

  FFunctions := TAiFunctions.Create(nil);
  FFunctions.OnLog          := OnLog;
  FFunctions.OnStatusUpdate := OnStatus;

  // Activar AutoMCP con el registro PPM
  FFunctions.AutoMCPConfig.Active      := True;
  FFunctions.AutoMCPConfig.RegistryUrl := REGISTRY;

  FAiConn := TAiChatConnection.Create(nil);
  FAiConn.AiFunctions              := FFunctions;
  FAiConn.OnError                  := OnError;
  FAiConn.OnStateChange            := OnState;
  FAiConn.DriverName               := DRIVER;
  FAiConn.Model                    := MODEL;
  FAiConn.Params.Values['ApiKey']  := API_KEY;
  FAiConn.RegisterUserParam(DRIVER, 'Tool_Active',     'True');
  FAiConn.RegisterUserParam(DRIVER, 'Asynchronous',    'False');
  FAiConn.RegisterUserParam(DRIVER, 'Max_Tokens',      '4096');
  FAiConn.RegisterUserParam(DRIVER, 'ResponseTimeOut', '120000');

  FAiConn.SystemPrompt.Text := FFunctions.GetAutoMCPSystemPrompt;
end;

destructor TAutoMCPDemo.Destroy;
begin
  FAiConn.Free;
  FFunctions.Free;
  inherited;
end;

procedure TAutoMCPDemo.OnLog(Sender: TObject; const Msg: String);
begin
  Writeln('  [MCP] ', Msg);
end;

procedure TAutoMCPDemo.OnStatus(Sender: TObject; const Msg: String);
begin
  Writeln('  [STS] ', Msg);
end;

procedure TAutoMCPDemo.OnError(Sender: TObject; const ErrorMsg: String;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  FLastError := ErrorMsg;
  Writeln('  [ERR] ', ErrorMsg);
end;

procedure TAutoMCPDemo.OnState(Sender: TObject; State: TAiChatState;
  const Desc: String);
begin
  case State of
    acsToolCalling  : Writeln('  [LLM] Llamando herramienta...');
    acsToolExecuting: Writeln('  [LLM] Ejecutando...');
  end;
end;

procedure TAutoMCPDemo.InstalarPaquete;
var
  LItem: TMCPClientItem;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('PASO 1 — Instalar ', MCP_PACKAGE, ' desde PPM');
  Writeln(StringOfChar('-', 60));

  // Verificar si ya esta instalado
  LItem := FFunctions.MCPClients.GetClientByName(MCP_PACKAGE);
  if Assigned(LItem) then
  begin
    Writeln('  Ya instalado: ', LItem.Params.Values['Command']);
    if not LItem.MCPClient.Initialized then
      LItem.MCPClient.Initialize;
    Writeln('  Disponible: ', BoolToStr(LItem.MCPClient.Available, True));
    Exit;
  end;

  // Instalar desde PPM
  Writeln('  Descargando e instalando ', MCP_PACKAGE, '...');
  try
    LItem := FFunctions.InstallMCPFromPPM(MCP_PACKAGE, '', '', REGISTRY);
    if not Assigned(LItem) then
    begin
      Writeln('  AVISO: InstallMCPFromPPM retorno nil.');
      Writeln('  Posibles causas: sin conectividad o paquete no disponible.');
      Exit;
    end;

    Writeln('  Instalado en: ', LItem.Params.Values['Command']);

    // Inicializar el proceso
    if not LItem.MCPClient.Initialized then
      LItem.MCPClient.Initialize;

    if LItem.MCPClient.Available then
      Writeln('  Servidor MCP activo y disponible.')
    else
      Writeln('  AVISO: El servidor MCP no quedo disponible.');
  except
    on E: Exception do
      Writeln('  ERROR al instalar: ', E.Message);
  end;
end;

procedure TAutoMCPDemo.ListarHerramientas;
var
  LItem    : TMCPClientItem;
  LToolsJ  : TJSONObject;
  LArr     : TJSONArray;
  LName, LDesc: String;
  T        : TJSONValue;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('PASO 2 — Listar herramientas de ', MCP_PACKAGE);
  Writeln(StringOfChar('-', 60));

  LItem := FFunctions.MCPClients.GetClientByName(MCP_PACKAGE);
  if not Assigned(LItem) or not LItem.MCPClient.Available then
  begin
    Writeln('  Paquete no disponible, omitiendo.');
    Exit;
  end;

  LToolsJ := LItem.MCPClient.ListTools;
  try
    if Assigned(LToolsJ) and LToolsJ.TryGetValue<TJSONArray>('tools', LArr) then
    begin
      Writeln(Format('  Herramientas encontradas: %d', [LArr.Count]));
      for T in LArr do
        if T is TJSONObject then
        begin
          TJSONObject(T).TryGetValue<String>('name',        LName);
          TJSONObject(T).TryGetValue<String>('description', LDesc);
          Writeln(Format('    [%s] %s', [LName, LDesc]));
        end;
    end;
  finally
    LToolsJ.Free;
  end;
end;

procedure TAutoMCPDemo.ChatConHerramientas;
var
  Pregunta, Resp: String;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('PASO 3 — Chat LLM + herramientas AutoMCP');
  Writeln(StringOfChar('-', 60));

  // Actualizar el system prompt con las herramientas disponibles
  FAiConn.SystemPrompt.Text := FFunctions.GetAutoMCPSystemPrompt;

  Pregunta := 'Cuanto es 1234 por 5678?';
  Writeln('Pregunta: ', Pregunta);
  try
    Resp := FAiConn.AddMessageAndRun(Pregunta, 'user', []);
    Writeln('Respuesta: ', Resp);
  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;
  Writeln;

  Pregunta := 'Cuanto es la raiz cuadrada de 144?';
  Writeln('Pregunta: ', Pregunta);
  try
    Resp := FAiConn.AddMessageAndRun(Pregunta, 'user', []);
    Writeln('Respuesta: ', Resp);
  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;
end;

procedure TAutoMCPDemo.Run;
begin
  Writeln('=== AutoMCP Demo ===');
  Writeln('Registro PPM: ', REGISTRY);
  Writeln('Paquete a instalar: ', MCP_PACKAGE);
  Writeln;

  InstalarPaquete;
  Writeln;
  ListarHerramientas;
  Writeln;
  ChatConHerramientas;
end;

var
  Demo: TAutoMCPDemo;

begin
  try
    Demo := TAutoMCPDemo.Create;
    try
      Demo.Run;
    finally
      Demo.Free;
    end;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
