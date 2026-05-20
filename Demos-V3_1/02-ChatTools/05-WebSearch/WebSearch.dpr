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
//   - cap_WebSearch en ModelCaps activa la búsqueda automáticamente
//   - Gemini (Google Search) tiene cap_WebSearch nativo — siempre activo
//   - Claude usa solo su conocimiento interno (sin búsqueda web nativa)
//   - Diferencia entre respuestas con y sin búsqueda web
//
// Comparación: Claude (sin web) vs Gemini (con web integrado)
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.Gemini;

const
  // Claude — sin búsqueda web (solo conocimiento interno)
  DRIVER_SIN  = 'Claude';
  MODEL_SIN   = 'claude-haiku-4-5-20251001';
  API_KEY_SIN = '@CLAUDE_API_KEY';

  // Gemini — con búsqueda web nativa (cap_WebSearch en ModelCaps)
  DRIVER_CON  = 'Gemini';
  MODEL_CON   = 'gemini-3-flash-preview';
  API_KEY_CON = '@GEMINI_API_KEY';

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
procedure CompararConSinBusqueda(ConnSin, ConnCon: TAiChatConnection;
                                  const Question: String);
var
  Res: String;
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', Question);
  Writeln;

  // ── SIN búsqueda web (Claude) ─────────────────────────────────────────
  ConnSin.Messages.Clear;
  Res := ConnSin.AddMessageAndRun(Question, 'user', []);
  Writeln('SIN búsqueda web (Claude — solo conocimiento interno):');
  Writeln('  ', Copy(Res, 1, 200), '...');
  Writeln;

  // ── CON búsqueda web (Gemini) ─────────────────────────────────────────
  ConnCon.Messages.Clear;
  Res := ConnCon.AddMessageAndRun(Question, 'user', []);
  Writeln('CON búsqueda web (Gemini — Google Search integrado):');
  Writeln('  ', Copy(Res, 1, 400));
  Writeln;
end;

// =============================================================================
//  DEMO principal
// =============================================================================
procedure RunDemo;
var
  ConnSin : TAiChatConnection;   // Claude — sin web search
  ConnCon : TAiChatConnection;   // Gemini — con web search
  Res     : String;
  I       : Integer;
begin
  Writeln('=== WebSearch ===');
  Writeln('SIN búsqueda: ', DRIVER_SIN, ' (', MODEL_SIN, ')');
  Writeln('CON búsqueda: ', DRIVER_CON, ' (', MODEL_CON, ')');
  Writeln;
  Writeln('Gemini tiene cap_WebSearch en ModelCaps: Google Search siempre activo.');
  Writeln('Claude usa solo su conocimiento interno (sin búsqueda web nativa).');
  Writeln;

  ConnSin := TAiChatConnection.Create(nil);
  ConnCon := TAiChatConnection.Create(nil);
  try
    // Conexión SIN web search — Claude
    ConnSin.DriverName := DRIVER_SIN;
    ConnSin.Model      := MODEL_SIN;
    ConnSin.Params.Values['ApiKey']       := API_KEY_SIN;
    ConnSin.Params.Values['Asynchronous'] := 'False';
    ConnSin.Params.Values['Max_Tokens']   := '1024';
    ConnSin.SystemPrompt.Text :=
      'Responde basándote únicamente en tu conocimiento interno. Responde en español.';

    // Conexión CON web search — Gemini
    ConnCon.DriverName := DRIVER_CON;
    ConnCon.Model      := MODEL_CON;
    ConnCon.Params.Values['ApiKey']       := API_KEY_CON;
    ConnCon.Params.Values['Asynchronous'] := 'False';
    ConnCon.Params.Values['Max_Tokens']   := '1024';
    ConnCon.SystemPrompt.Text :=
      'Eres un asistente con acceso a búsqueda web. ' +
      'Cuando uses búsqueda web, menciona las fuentes consultadas. ' +
      'Responde en español.';

    // ── Demo 1: Comparación con/sin búsqueda ─────────────────────────────
    Writeln('=== COMPARACIÓN: con vs. sin búsqueda web ===');
    CompararConSinBusqueda(ConnSin, ConnCon,
      '¿Cuál es el LLM más potente disponible hoy?');

    // ── Demo 2: Preguntas que necesitan info actual ───────────────────────
    Writeln('=== PREGUNTAS QUE REQUIEREN INFORMACIÓN ACTUAL (Gemini) ===');
    for I := Low(PREGUNTAS) to High(PREGUNTAS) do
    begin
      ConnCon.Messages.Clear;
      Writeln(StringOfChar('-', 60));
      Writeln(Format('Pregunta %d: %s', [I+1, PREGUNTAS[I]]));
      Res := ConnCon.AddMessageAndRun(PREGUNTAS[I], 'user', []);
      Writeln('Respuesta: ');
      Writeln(Copy(Res, 1, 500));
      Writeln;
    end;

    // ── Demo 3: Citar fuentes ─────────────────────────────────────────────
    Writeln(StringOfChar('-', 60));
    Writeln('Demo 3: Respuesta con fuentes citadas (Gemini)');
    ConnCon.Messages.Clear;
    ConnCon.SystemPrompt.Text :=
      'Responde con fuentes. Al final incluye una sección "Fuentes:" con ' +
      'las URLs consultadas. Responde en español.';

    Res := ConnCon.AddMessageAndRun(
      '¿Qué es Model Context Protocol (MCP) de Anthropic y para qué sirve?',
      'user', []);
    Writeln(Res);

  finally
    ConnSin.Free;
    ConnCon.Free;
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
