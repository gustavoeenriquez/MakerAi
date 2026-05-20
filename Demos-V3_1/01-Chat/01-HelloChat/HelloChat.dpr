program HelloChat;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 01-Chat / 01-HelloChat
// =============================================================================
// El demo más simple posible: configurar una conexión, enviar un mensaje,
// imprimir la respuesta.
//
// Conceptos que cubre:
//   - Crear y configurar TAiChatConnection
//   - Importar el driver del proveedor deseado
//   - Llamada síncrona con AddMessageAndRun()
//   - Resolución de API Keys desde variables de entorno (@VARNAME)
//
// Cómo cambiar de proveedor:
//   Solo modifica DRIVER, MODEL y API_KEY.
//   El resto del código no cambia — esa es la propuesta de valor central
//   de MakerAI: un conector universal para todos los LLMs.
//
// Configurar antes de compilar (o definir como variables de entorno):
//   CLAUDE_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY...
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  // ── Drivers disponibles ──────────────────────────────────────────────────
  // Importar el driver activa su registro automático en el factory.
  // Solo se necesita importar; TAiChatConnection lo encontrará por nombre.
  uMakerAi.Chat.Claude,    // DriverName = 'Claude'
  uMakerAi.Chat.OpenAi,    // DriverName = 'OpenAI'
  uMakerAi.Chat.Gemini,    // DriverName = 'Gemini'
  uMakerAi.Chat.Ollama,    // DriverName = 'Ollama'  (local, sin API key)
  uMakerAi.Chat.Groq,      // DriverName = 'Groq'
  uMakerAi.Chat.Mistral;   // DriverName = 'Mistral'

const
  // ── Cambiar aquí para usar otro proveedor ────────────────────────────────
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';   // '@VAR' → GetEnvironmentVariable('VAR')
  // ────────────────────────────────────────────────────────────────────────

  SYSTEM_PROMPT = 'Eres un asistente útil y conciso. Responde siempre en español.';

// =============================================================================
//  DEMO
// =============================================================================

procedure RunDemo;
var
  Conn    : TAiChatConnection;
  Prompt  : String;
  Response: String;
begin
  Writeln('=== HelloChat ===');
  Writeln('Driver : ', DRIVER);
  Writeln('Model  : ', MODEL);
  Writeln;

  Conn := TAiChatConnection.Create(nil);
  try
    // ── Configuración mínima ─────────────────────────────────────────────
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;

    Conn.Params.Values['ApiKey']       := API_KEY;
    Conn.Params.Values['Asynchronous'] := 'False';  // llamada síncrona
    Conn.Params.Values['Max_Tokens']   := '1024';

    Conn.SystemPrompt.Text := SYSTEM_PROMPT;

    // ── Pregunta 1: texto simple ─────────────────────────────────────────
    Prompt := '¿Cuál es la capital de Francia? Responde en una sola oración.';
    Writeln('Pregunta: ', Prompt);

    Response := Conn.AddMessageAndRun(Prompt, 'user', []);
    Writeln('Respuesta: ', Response);
    Writeln;

    // ── Pregunta 2: más elaborada ────────────────────────────────────────
    // Nota: AddMessageAndRun() acumula historial. La segunda pregunta
    // puede referirse a la primera. Ver demo 04-ChatHistory para detalle.
    Prompt := 'Dame ahora 3 datos curiosos sobre esa ciudad.';
    Writeln('Pregunta: ', Prompt);

    Response := Conn.AddMessageAndRun(Prompt, 'user', []);
    Writeln('Respuesta: ', Response);

  finally
    Conn.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
