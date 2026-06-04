program SmartDispatch;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI - 02-ChatTools / 08-SmartDispatch
// =============================================================================
// Valida el modo cmSmartDispatch: despacho inteligente para modelos pequeños
// sin function calling nativo.
//
// Flujo por mensaje:
//   Pase 1 (silencioso) — el LLM recibe un system prompt especial y elige
//                         herramienta + reescribe el prompt.
//   Pase 2              — se ejecuta la herramienta elegida, o se usa la
//                         respuesta del Pase 1 directamente si elige [CHAT].
//
// Herramientas simuladas (mock):
//   TMockImageTool     — imprime el prompt en lugar de generar imagen real
//   TMockWebSearchTool — imprime la consulta en lugar de buscar en internet
//
// Requisitos:
//   - Ollama corriendo en http://localhost:11434
//   - Modelo gemma3:4b (o cualquier modelo local sin function calling)
//   - Cambiar DRIVER/MODEL/OLLAMA_URL si se usa otro proveedor
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Core,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Ollama;

const
  DRIVER     = 'Ollama';
  MODEL      = 'gemma4:e4b';
  OLLAMA_URL = 'http://localhost:11434/';

// =============================================================================
//  Mock tools — simulan herramientas sin llamadas reales a APIs
// =============================================================================

type
  TMockImageTool = class(TAiImageToolBase)
  protected
    procedure ExecuteImageGeneration(const APrompt: string; ResMsg, AskMsg: TAiChatMessage); override;
  end;

  TMockWebSearchTool = class(TAiWebSearchToolBase)
  protected
    procedure ExecuteSearch(const AQuery: string; ResMsg, AskMsg: TAiChatMessage); override;
  end;

procedure TMockImageTool.ExecuteImageGeneration(const APrompt: string; ResMsg, AskMsg: TAiChatMessage);
begin
  Writeln('  >> [ImageTool] prompt recibido: "', APrompt, '"');
  ResMsg.Prompt := '[IMAGEN SIMULADA] ' + APrompt;
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

procedure TMockWebSearchTool.ExecuteSearch(const AQuery: string; ResMsg, AskMsg: TAiChatMessage);
begin
  Writeln('  >> [WebSearchTool] consulta recibida: "', AQuery, '"');
  ResMsg.Prompt := '[BUSQUEDA WEB SIMULADA] ' + AQuery;
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

// =============================================================================
//  Helper: ejecuta un mensaje y muestra resultado
// =============================================================================

procedure Probar(Conn: TAiChatConnection; const Etiqueta, Mensaje: String);
var
  Resp: String;
begin
  Writeln(StringOfChar('-', 65));
  Writeln('[', Etiqueta, '] ', Mensaje);
  try
    Resp := Conn.AddMessageAndRun(Mensaje, 'user', []);
    Writeln('  Resultado: ', Copy(Resp, 1, 120));
  except
    on E: Exception do
      Writeln('  ERROR: ', E.ClassName, ': ', E.Message);
  end;
  Writeln;
end;

// =============================================================================
//  Demo principal
// =============================================================================

procedure RunDemo;
var
  Conn      : TAiChatConnection;
  ImageTool : TMockImageTool;
  SearchTool: TMockWebSearchTool;
begin
  Writeln('=== SmartDispatch Demo ===');
  Writeln('Driver : ', DRIVER, ' / ', MODEL);
  Writeln('Modo   : cmSmartDispatch');
  Writeln('Nota   : Asynchronous=False requerido en este modo');
  Writeln;

  ImageTool  := TMockImageTool.Create(nil);
  SearchTool := TMockWebSearchTool.Create(nil);
  Conn       := TAiChatConnection.Create(nil);
  try
    // Herramientas asignadas ANTES de DriverName (patron requerido)
    Conn.ImageTool     := ImageTool;
    Conn.WebSearchTool := SearchTool;

    Conn.DriverName := DRIVER;

    // Params del driver
    Conn.Params.Values['URL']          := OLLAMA_URL;
    Conn.Params.Values['Model']        := MODEL;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '512';

    // Modo SmartDispatch
    Conn.ChatMode := cmSmartDispatch;
    Conn.SystemPrompt.Text := 'Eres un asistente util. Responde en espanol.';

    Writeln('Herramientas disponibles para el despachador:');
    Writeln('  - ImageTool     => tag [IMAGEGEN]');
    Writeln('  - WebSearchTool => tag [WEBSEARCH]');
    Writeln('  - Sin tool      => tag [CHAT]');
    Writeln;

    // --- Casos de prueba ---

    // Debe ir a [CHAT] — pregunta de conversacion
    Probar(Conn, 'CHAT esperado',
      'Cual es la capital de Francia?');

    // Debe ir a [IMAGEGEN] — solicitud de imagen
    Probar(Conn, 'IMAGEGEN esperado',
      'Genera una imagen de un gato con sombrero de cowboy');

    // Debe ir a [WEBSEARCH] — solicitud de busqueda
    Probar(Conn, 'WEBSEARCH esperado',
      'Busca las ultimas noticias sobre inteligencia artificial en 2025');

    // Debe ir a [CHAT] — calculo matematico
    Probar(Conn, 'CHAT esperado',
      'Cuanto es 15 multiplicado por 7?');

    // Ambiguo: imagen o chat?
    Probar(Conn, 'AMBIGUO (imagen o chat?)',
      'Muestrame como se ve un arcoiris');

    Writeln(StringOfChar('=', 65));
    Writeln('Demo finalizado. Total mensajes en historial: ',
      Conn.AiChat.Messages.Count);

  finally
    Conn.Free;
    ImageTool.Free;
    SearchTool.Free;
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
  Write('Presiona Enter para salir...');
  Readln;
end.
