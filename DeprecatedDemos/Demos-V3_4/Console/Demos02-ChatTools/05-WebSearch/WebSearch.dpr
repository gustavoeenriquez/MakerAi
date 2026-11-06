program WebSearch;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 02-ChatTools / 05-WebSearch
// =============================================================================
// El LLM busca en internet para responder preguntas con información reciente,
// superando la limitación del "knowledge cutoff".
//
// Conceptos que cubre:
//   - Activar búsqueda web con cap_WebSearch en Capabilities
//   - Proveedores con búsqueda nativa: Gemini (Google Search), Grok (X/Twitter)
//   - Proveedor Claude: usa Brave Search si se configura la API key
//   - Diferencia entre respuestas con y sin búsqueda web
//   - Las citas/fuentes que devuelve el LLM
//
// Proveedores soportados:
//   - Gemini    → Google Search integrado, sin configuración extra
//   - Grok      → X/Twitter + web, sin configuración extra
//   - Claude    → Requiere BRAVE_API_KEY (o Bing) configurado en el proveedor
//   - OpenAI    → Requiere configuración adicional de herramientas
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Grok;

const
  // Gemini tiene búsqueda web nativa sin configuración extra
  DRIVER  = 'Gemini';
  MODEL   = 'gemini-2.0-flash';
  API_KEY = '@GEMINI_API_KEY';

// Preguntas que requieren información actual
const
  PREGUNTAS: array[0..3] of String = (
    '¿Cuáles son las últimas noticias de inteligencia artificial de esta semana?',
    '¿Cuál es el precio actual del dólar respecto al peso mexicano?',
    '¿Cuál es el último modelo de LLM lanzado por Anthropic?',
    '¿Qué eventos tecnológicos importantes ocurrieron este mes?'
  );

// =============================================================================
//  Compara respuesta CON y SIN búsqueda web para la misma pregunta
// =============================================================================
procedure CompararConSinBusqueda(Conn: TAiChatConnection; const Question: String);
var
  Res: String;
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', Question);
  Writeln;

  // ── SIN búsqueda web ─────────────────────────────────────────────────
  Conn.Messages.Clear;
  Conn.Params.Values['WebSearch'] := 'False';

  Res := Conn.AddMessageAndRun(Question, 'user', []);
  Writeln('SIN búsqueda web:');
  Writeln('  ', Copy(Res, 1, 200), '...');
  Writeln;

  // ── CON búsqueda web ─────────────────────────────────────────────────
  Conn.Messages.Clear;
  Conn.Params.Values['WebSearch'] := 'True';

  Res := Conn.AddMessageAndRun(Question, 'user', []);
  Writeln('CON búsqueda web:');
  Writeln('  ', Copy(Res, 1, 400));
  Writeln;
end;

// =============================================================================
//  DEMO principal
// =============================================================================
procedure RunDemo;
var
  Conn: TAiChatConnection;
  Res : String;
  I   : Integer;
begin
  Writeln('=== WebSearch ===');
  Writeln('Driver: ', DRIVER, ' / Model: ', MODEL);
  Writeln;
  Writeln('Gemini tiene acceso a Google Search de forma nativa.');
  Writeln('Grok usa la web de X/Twitter. Claude requiere Brave API key.');
  Writeln;

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;

    Conn.Params.Values['ApiKey']       := API_KEY;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '1024';

    Conn.SystemPrompt.Text :=
      'Eres un asistente que puede buscar en internet. ' +
      'Cuando uses búsqueda web, menciona las fuentes consultadas. ' +
      'Responde en español.';

    // ── Demo 1: Comparación con/sin búsqueda ─────────────────────────────
    Writeln('=== COMPARACIÓN: con vs. sin búsqueda web ===');
    CompararConSinBusqueda(Conn,
      '¿Cuál es el LLM más potente disponible hoy?');

    // ── Demo 2: Preguntas que necesitan info actual ───────────────────────
    Writeln('=== PREGUNTAS QUE REQUIEREN INFORMACIÓN ACTUAL ===');
    Conn.Params.Values['WebSearch'] := 'True';

    for I := Low(PREGUNTAS) to High(PREGUNTAS) do
    begin
      Conn.Messages.Clear;
      Writeln(StringOfChar('-', 60));
      Writeln(Format('Pregunta %d: %s', [I+1, PREGUNTAS[I]]));
      Res := Conn.AddMessageAndRun(PREGUNTAS[I], 'user', []);
      Writeln('Respuesta: ');
      Writeln(Copy(Res, 1, 500));
      Writeln;
    end;

    // ── Demo 3: Citar fuentes ─────────────────────────────────────────────
    Writeln(StringOfChar('-', 60));
    Writeln('Demo 3: Respuesta con fuentes citadas');
    Conn.Messages.Clear;
    Conn.SystemPrompt.Text :=
      'Responde con fuentes. Al final incluye una sección "Fuentes:" con ' +
      'las URLs consultadas. Responde en español.';

    Res := Conn.AddMessageAndRun(
      '¿Qué es Model Context Protocol (MCP) de Anthropic y para qué sirve?',
      'user', []);
    Writeln(Res);

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
