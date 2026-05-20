program PrimerChat;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — Capítulo 3: Tu Primer Chat con MakerAI
// =============================================================================
// El demo más simple posible con TAiChatConnection: configurar el proveedor,
// enviar mensajes en un bucle y recibir respuestas.
//
// Conceptos que cubre:
//   - Crear y configurar TAiChatConnection
//   - Seleccionar provider con DriverName
//   - API key desde variable de entorno con la sintaxis @VARNAME
//   - Llamada síncrona con AddMessageAndRun()
//   - Historial automático: el modelo "recuerda" sin código adicional
//   - Limpiar historial con NewChat
//   - Inspeccionar Messages.Count
//
// Cómo cambiar de provider:
//   Modifica DRIVER, MODEL y API_KEY. El resto del código no cambia.
//   Esa es la propuesta de valor central de MakerAI.
//
// Configurar antes de compilar:
//   Define GROQ_API_KEY como variable de entorno del sistema.
//   Alternativas gratuitas: Ollama (local, sin key), Gemini (GEMINI_API_KEY).
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  // ── Importar el driver activa su auto-registro en el factory ──────────────
  // Solo se necesita importar la unidad; TAiChatConnection lo encuentra por nombre.
  uMakerAi.Chat.Groq;      // DriverName = 'Groq'
  // uMakerAi.Chat.Claude;  // DriverName = 'Claude'
  // uMakerAi.Chat.OpenAi;  // DriverName = 'OpenAI'
  // uMakerAi.Chat.Gemini;  // DriverName = 'Gemini'
  // uMakerAi.Chat.Ollama;  // DriverName = 'Ollama'  (local, sin API key)

const
  // ── Cambiar aquí para usar otro provider ──────────────────────────────────
  DRIVER  = 'Groq';
  MODEL   = 'llama-3.3-70b-versatile';
  API_KEY = '@GROQ_API_KEY';   // '@VAR' → GetEnvironmentVariable('VAR')

procedure RunDemo;
var
  Conn    : TAiChatConnection;
  Prompt  : string;
  Respuesta: string;
begin
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;

    Conn.Params.Values['ApiKey']       := API_KEY;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '512';

    Conn.SystemPrompt.Text :=
      'Eres un asistente amable y conciso. Responde siempre en español.';

    WriteLn('');
    WriteLn('=== MakerAI — Capítulo 3: Primer Chat ===');
    WriteLn('Provider : ', DRIVER);
    WriteLn('Modelo   : ', MODEL);
    WriteLn('Comandos : "salir", "limpiar", "mensajes"');
    WriteLn('');

    repeat
      Write('Tú > ');
      ReadLn(Prompt);
      Prompt := Trim(Prompt);

      if Prompt = '' then
        Continue;

      if SameText(Prompt, 'salir') then
        Break;

      if SameText(Prompt, 'limpiar') then
      begin
        Conn.NewChat;   // Libera los objetos correctamente y limpia el historial
        WriteLn('   [Historial borrado]');
        WriteLn('');
        Continue;
      end;

      if SameText(Prompt, 'mensajes') then
      begin
        WriteLn('   [Mensajes en historial: ', Conn.Messages.Count, ']');
        WriteLn('');
        Continue;
      end;

      try
        Respuesta := Conn.AddMessageAndRun(Prompt, 'user', []);
        WriteLn('IA  > ', Respuesta);
      except
        on E: Exception do
          WriteLn('ERROR: ', E.ClassName, ' — ', E.Message);
      end;
      WriteLn('');

    until False;

    WriteLn('');
    WriteLn('Sesión terminada. Total mensajes: ', Conn.Messages.Count);

  finally
    Conn.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
