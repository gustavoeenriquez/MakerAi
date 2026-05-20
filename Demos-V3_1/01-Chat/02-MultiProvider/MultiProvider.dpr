program MultiProvider;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 01-Chat / 02-MultiProvider
// =============================================================================
// Misma pregunta, enviada a múltiples proveedores LLM uno tras otro.
// Demuestra que el código de aplicación NO cambia al cambiar de proveedor.
//
// Conceptos que cubre:
//   - Patrón de conector universal (TAiChatConnection)
//   - Cambio de proveedor en tiempo de ejecución
//   - Comparación de respuestas entre modelos
//   - Manejo de errores por proveedor (clave inválida, modelo no disponible)
//
// Requisitos:
//   Define las API keys como variables de entorno:
//   CLAUDE_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY, GROQ_API_KEY
//   Los proveedores locales (Ollama) no requieren API key.
// =============================================================================

uses
  System.SysUtils,
  System.Diagnostics,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Ollama,
  uMakerAi.Chat.Groq,
  uMakerAi.Chat.Mistral,
  uMakerAi.Chat.DeepSeek;

// ── Definición de los proveedores a probar ───────────────────────────────────
type
  TProviderConfig = record
    Driver : String;
    Model  : String;
    ApiKey : String;
    Enabled: Boolean;
  end;

const
  PROVIDERS: array[0..4] of TProviderConfig = (
    (Driver: 'Claude';   Model: 'claude-haiku-4-5-20251001';   ApiKey: '@CLAUDE_API_KEY';  Enabled: True),
    (Driver: 'OpenAI';   Model: 'gpt-5.4-mini';                ApiKey: '@OPENAI_API_KEY';  Enabled: True),
    (Driver: 'Gemini';   Model: 'gemini-3-flash-preview';       ApiKey: '@GEMINI_API_KEY';  Enabled: True),
    (Driver: 'Groq';     Model: 'llama-3.3-70b-versatile';     ApiKey: '@GROQ_API_KEY';    Enabled: True),
    (Driver: 'Ollama';   Model: 'gemma3:4b';                   ApiKey: '';                 Enabled: True)
  );

  QUESTION = 'En exactamente 2 oraciones: ¿Qué es la inteligencia artificial?';

// =============================================================================
//  Función que envía la misma pregunta a un proveedor y mide el tiempo
// =============================================================================
procedure AskProvider(const Cfg: TProviderConfig; const Question: String);
var
  Conn    : TAiChatConnection;
  Response: String;
  SW      : TStopwatch;
begin
  if not Cfg.Enabled then
  begin
    Writeln(Format('[%s] — Deshabilitado', [Cfg.Driver]));
    Exit;
  end;

  Writeln(StringOfChar('-', 60));
  Writeln(Format('▶  %s  (%s)', [Cfg.Driver, Cfg.Model]));

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := Cfg.Driver;
    Conn.Model      := Cfg.Model;

    Conn.Params.Values['ApiKey']       := Cfg.ApiKey;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '256';

    Conn.SystemPrompt.Text := 'Responde siempre en español de forma concisa.';

    SW := TStopwatch.StartNew;
    try
      Response := Conn.AddMessageAndRun(Question, 'user', []);
      SW.Stop;
      Writeln(Format('   Tiempo: %d ms', [SW.ElapsedMilliseconds]));
      Writeln('   Respuesta: ', Response);
    except
      on E: Exception do
        Writeln(Format('   ERROR: %s', [E.Message]));
    end;
  finally
    Conn.Free;
  end;
  Writeln;
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  I: Integer;
begin
  Writeln('=== MultiProvider ===');
  Writeln('Pregunta: ', QUESTION);
  Writeln;

  for I := Low(PROVIDERS) to High(PROVIDERS) do
    AskProvider(PROVIDERS[I], QUESTION);

  Writeln(StringOfChar('=', 60));
  Writeln('Todos los proveedores respondieron la misma pregunta.');
  Writeln('El código de consulta es idéntico para todos ellos.');
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
