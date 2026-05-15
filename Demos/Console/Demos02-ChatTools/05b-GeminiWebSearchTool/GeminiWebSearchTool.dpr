program GeminiWebSearchTool;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI - 02-ChatTools / 05b-GeminiWebSearchTool
// =============================================================================
// Prueba directa de TAiGeminiWebSearchTool como ChatTools.WebSearchTool.
// Usa el metodo estatico Search() y tambien integrado en TAiChatConnection
// con cmSmartDispatch.
//
// Requiere:
//   - Variable de entorno GEMINI_API_KEY configurada
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Ollama,
  uMakerAi.Gemini.WebSearch;

const
  GEMINI_KEY = '@GEMINI_API_KEY';

// =============================================================================
//  Demo 1 — Llamada directa via metodo estatico
// =============================================================================
procedure TestDirecto;
var
  Msg: TAiChatMessage;
  I  : Integer;
begin
  Writeln(StringOfChar('=', 65));
  Writeln('DEMO 1: Llamada directa (TAiGeminiWebSearchTool.Search)');
  Writeln(StringOfChar('=', 65));
  Writeln;

  try
    Msg := TAiGeminiWebSearchTool.Search(
      GEMINI_KEY,
      'Cuales son los modelos LLM mas potentes disponibles en abril 2025?',
      'gemini-2.0-flash');
    try
      Writeln('Respuesta:');
      Writeln(Copy(Msg.Prompt, 1, 600));
      Writeln;
      Writeln(Format('  Tokens — prompt: %d  completion: %d  total: %d',
        [Msg.Prompt_tokens, Msg.Completion_tokens, Msg.Total_tokens]));
      Writeln;

      if Msg.Citations.Count > 0 then
      begin
        Writeln(Format('  Citaciones: %d', [Msg.Citations.Count]));
        for I := 0 to Min(2, Msg.Citations.Count - 1) do
          if Msg.Citations[I].Sources.Count > 0 then
            Writeln(Format('    [%d] %s', [I+1, Msg.Citations[I].Sources[0].DataSource.Url]));
      end
      else
        Writeln('  (sin citaciones en metadata)');
    finally
      Msg.Free;
    end;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ': ', E.Message);
  end;
  Writeln;
end;

// =============================================================================
//  Demo 2 — Integrado en TAiChatConnection con Ollama + SmartDispatch
// =============================================================================
procedure TestSmartDispatch;
var
  Conn      : TAiChatConnection;
  SearchTool: TAiGeminiWebSearchTool;
  Resp      : String;
begin
  Writeln(StringOfChar('=', 65));
  Writeln('DEMO 2: TAiGeminiWebSearchTool + Ollama cmSmartDispatch');
  Writeln(StringOfChar('=', 65));
  Writeln('  Clasifica con modelo local, busca con Gemini Google Search');
  Writeln;

  SearchTool := TAiGeminiWebSearchTool.Create(nil);
  Conn       := TAiChatConnection.Create(nil);
  try
    SearchTool.ApiKey := GEMINI_KEY;
    SearchTool.Model  := 'gemini-2.0-flash';

    Conn.WebSearchTool := SearchTool;

    Conn.DriverName := 'Ollama';
    Conn.Params.Values['URL']          := 'http://localhost:11434/';
    Conn.Params.Values['Model']        := 'gemma3:4b';
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '512';

    Conn.ChatMode          := cmSmartDispatch;
    Conn.SystemPrompt.Text := 'Eres un asistente util. Responde en espanol.';

    // CHAT — no debe buscar
    Writeln(StringOfChar('-', 65));
    Write('[CHAT] Cuanto es 25 por 4? => ');
    Resp := Conn.AddMessageAndRun('Cuanto es 25 por 4?', 'user', []);
    Writeln(Copy(Resp, 1, 150));

    // WEBSEARCH — debe buscar
    Writeln(StringOfChar('-', 65));
    Writeln('[WEBSEARCH] Noticias recientes de IA en 2025:');
    try
      Resp := Conn.AddMessageAndRun(
        'Busca las ultimas noticias sobre inteligencia artificial en 2025',
        'user', []);
      Writeln(Copy(Resp, 1, 500));
    except
      on E: Exception do
        Writeln('ERROR en busqueda: ', E.Message);
    end;

  finally
    Conn.Free;
    SearchTool.Free;
  end;
  Writeln;
end;

begin
  try
    TestDirecto;
    TestSmartDispatch;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Write('Presiona Enter para salir...');
  Readln;
end.
