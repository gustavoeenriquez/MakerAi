program MCPServerHTTP;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 06-MCP / 02-Server-HTTP
// =============================================================================
// Servidor MCP via protocolo HTTP (JSON-RPC sobre HTTP).
// Protocolo ideal para clientes REST o integraciones remotas.
//
// Conceptos que cubre:
//   - TAiMCPHttpServer: servidor MCP via HTTP
//   - Server.Port: puerto de escucha (defecto 8080)
//   - Server.CorsEnabled / CorsAllowedOrigins: CORS para acceso web
//   - Mismo conjunto de herramientas que el servidor StdIO
//
// Endpoints:
//   GET  http://localhost:8080/mcp  -> Info del servidor
//   POST http://localhost:8080/mcp  -> Comandos JSON-RPC
//
// Herramientas: get_datetime, greet (identicas al 01-Server-StdIO)
// =============================================================================

uses
  System.SysUtils,
  System.JSON,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Http;

// =============================================================================
//  Herramienta: get_datetime
// =============================================================================
type
  TDateTimeParams = class
  private
    FFormat: String;
  public
    [AiMCPSchemaDescription('Formato: "iso" (ISO-8601, defecto) o "readable" (texto legible)')]
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
  FDescription := 'Retorna la fecha y hora actual del servidor en el formato solicitado.';
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
// =============================================================================
type
  TGreetParams = class
  private
    FName    : String;
    FLanguage: String;
  public
    [AiMCPSchemaDescription('Nombre de la persona a saludar')]
    property Name: String read FName write FName;
    [AiMCPSchemaDescription('Idioma: "es" espanol (defecto), "en" ingles')]
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
  FDescription := 'Genera un saludo personalizado en espanol o ingles.';
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
    Saludo := Format('Hello, %s! Welcome to MakerAI MCP HTTP Server.', [NombrePersona])
  else
    Saludo := Format('Hola, %s! Bienvenido al servidor MCP HTTP de MakerAI.', [NombrePersona]);
  Result := TAiMCPResponseBuilder.New.AddText(Saludo).Build;
end;

// =============================================================================
//  MAIN
// =============================================================================
var
  Server: TAiMCPHttpServer;

begin
  try
    Server := TAiMCPHttpServer.Create(nil);
    try
      Server.ServerName       := 'MakerAI-Demo-HTTP';
      Server.Port             := 8080;
      Server.CorsEnabled      := True;
      Server.CorsAllowedOrigins := '*';

      // Registrar herramientas
      Server.RegisterTool('get_datetime',
        function: IAiMCPTool begin Result := TDateTimeMCPTool.Create; end);
      Server.RegisterTool('greet',
        function: IAiMCPTool begin Result := TGreetMCPTool.Create; end);

      Server.Start;

      Writeln;
      Writeln('=== MCPServerHTTP ===');
      Writeln(Format('Servidor HTTP escuchando en puerto %d', [Server.Port]));
      Writeln('Herramientas registradas: get_datetime, greet');
      Writeln;
      Writeln('Endpoints:');
      Writeln(Format('  GET  http://localhost:%d/mcp  -> Info del servidor', [Server.Port]));
      Writeln(Format('  POST http://localhost:%d/mcp  -> Comandos JSON-RPC', [Server.Port]));
      Writeln;
      Writeln('Presiona Enter para detener...');
      Readln;

    finally
      Server.Stop;
      Server.Free;
    end;
  except
    on E: Exception do
      Writeln('Error fatal: ', E.ClassName, ': ', E.Message);
  end;
end.
