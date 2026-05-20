program MCPServerStdIO;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 06-MCP / 01-Server-StdIO
// =============================================================================
// Servidor MCP via protocolo StdIO (entrada/salida estandar).
// Protocolo ideal para integracion local como subproceso.
//
// Conceptos que cubre:
//   - TAiMCPStdioServer: servidor MCP via stdin/stdout
//   - TAiMCPToolBase<TParams>: clase base para implementar herramientas MCP
//   - Server.RegisterTool(name, factory): registrar herramientas
//   - Server.Start: iniciar el loop de JSON-RPC
//   - Usar ErrOutput para logs (no contamina el canal JSON)
//
// Herramientas: get_datetime, greet
// Protocolo: StdIO (JSON-RPC via stdin/stdout)
//
// Para probar: compilar y ejecutar directamente, o apuntar el
//              Cliente (04-Client-Basic) a este ejecutable.
// =============================================================================

uses
  System.SysUtils,
  System.JSON,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Stdio;

// =============================================================================
//  Herramienta: get_datetime
//  Devuelve la fecha y hora actual
// =============================================================================
type
  TDateTimeParams = class
  private
    FFormat: String;
  public
    [AiMCPSchemaDescription('Formato deseado: "iso" (por defecto) o "readable"')]
    property Format: String read FFormat write FFormat;
  end;

  TDateTimeMCPTool = class(TAiMCPToolBase<TDateTimeParams>)
  protected
    function ExecuteWithParams(const AParams: TDateTimeParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

constructor TDateTimeMCPTool.Create;
begin
  inherited;
  FName        := 'get_datetime';
  FDescription := 'Retorna la fecha y hora actual del servidor. ' +
                  'Formato "iso" devuelve ISO-8601; "readable" devuelve texto legible.';
end;

function TDateTimeMCPTool.ExecuteWithParams(const AParams: TDateTimeParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Fmt, Resultado: String;
begin
  Fmt := Trim(LowerCase(AParams.Format));
  if Fmt = 'readable' then
    Resultado := FormatDateTime('"Hoy es" dddd, d "de" mmmm "de" yyyy, "a las" hh:nn:ss', Now)
  else
    Resultado := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);

  Result := TAiMCPResponseBuilder.New.AddText(Resultado).Build;
end;

// =============================================================================
//  Herramienta: greet
//  Devuelve un saludo personalizado
// =============================================================================
type
  TGreetParams = class
  private
    FName    : String;
    FLanguage: String;
  public
    [AiMCPSchemaDescription('Nombre de la persona a saludar')]
    property Name: String read FName write FName;
    [AiMCPSchemaDescription('Idioma del saludo: "es" espanol (defecto), "en" ingles')]
    property Language: String read FLanguage write FLanguage;
  end;

  TGreetMCPTool = class(TAiMCPToolBase<TGreetParams>)
  protected
    function ExecuteWithParams(const AParams: TGreetParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

constructor TGreetMCPTool.Create;
begin
  inherited;
  FName        := 'greet';
  FDescription := 'Genera un saludo personalizado. ' +
                  'Soporta idiomas: "es" (espanol) y "en" (ingles).';
end;

function TGreetMCPTool.ExecuteWithParams(const AParams: TGreetParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  NombrePersona, Lang, Saludo: String;
begin
  NombrePersona := Trim(AParams.Name);
  if NombrePersona = '' then NombrePersona := 'mundo';
  Lang := Trim(LowerCase(AParams.Language));

  if Lang = 'en' then
    Saludo := Format('Hello, %s! Welcome to MakerAI MCP Server.', [NombrePersona])
  else
    Saludo := Format('Hola, %s! Bienvenido al servidor MCP de MakerAI.', [NombrePersona]);

  Result := TAiMCPResponseBuilder.New.AddText(Saludo).Build;
end;

// =============================================================================
//  MAIN
// =============================================================================
var
  Server: TAiMCPStdioServer;

begin
  try
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MakerAI-Demo-StdIO';

      // Registrar herramientas via factory
      Server.RegisterTool('get_datetime',
        function: IAiMCPTool begin Result := TDateTimeMCPTool.Create; end);

      Server.RegisterTool('greet',
        function: IAiMCPTool begin Result := TGreetMCPTool.Create; end);

      // Iniciar el loop de JSON-RPC (bloqueante)
      Server.Start;

      // Los logs van a ErrOutput para no contaminar el canal JSON en StdIO
      WriteLn(ErrOutput, '[MCPServerStdIO] Servidor iniciado. Esperando comandos JSON-RPC...');
      WriteLn(ErrOutput, '[MCPServerStdIO] Herramientas: get_datetime, greet');

      // Mantener el proceso vivo
      while True do
        Sleep(1000);

    finally
      Server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[MCPServerStdIO] Error fatal: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
