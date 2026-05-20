program SingleFunction;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 02-ChatTools / 01-SingleFunction
// =============================================================================
// Demuestra cómo exponer UNA función Delphi al LLM para que la invoque
// cuando lo necesite (Tool Use / Function Calling).
//
// Conceptos que cubre:
//   - Crear TAiFunctions y registrar una función con AddFunction()
//   - Asignar TAiFunctions a TAiChatConnection.AiFunctions
//   - Implementar el handler como método de clase (TFunctionEvent)
//   - El LLM decide CUÁNDO llamar la función según el contexto
//   - La función devuelve un resultado en ToolCall.Response
//
// Flujo:
//   Usuario → LLM detecta que necesita la función → llama get_weather →
//   handler Delphi produce JSON → LLM redacta respuesta final
// =============================================================================

uses
  System.SysUtils,
  System.JSON,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Groq;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

// =============================================================================
//  TSingleFunctionDemo
// =============================================================================
type
  TSingleFunctionDemo = class
  private
    FAiFunctions: TAiFunctions;

    // ── Handler de la función get_weather ────────────────────────────────────
    procedure OnFnGetWeather(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
  public
    procedure Run;
  end;

// =============================================================================
//  Handler: simula consultar el clima de una ciudad
// =============================================================================
procedure TSingleFunctionDemo.OnFnGetWeather(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: String;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Args   : TJSONObject;
  City   : String;
  TempC  : Integer;
  Cond   : String;
  Hum    : Integer;
begin
  Writeln(Format('  [Tool call] get_weather(%s)', [ToolCall.Arguments]));

  Args := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if not Assigned(Args) then
  begin
    ToolCall.Response := '{"error":"No se pudo parsear los argumentos"}';
    Handled := True;
    Exit;
  end;
  try
    City := Args.GetValue<String>('city', 'desconocida');
  finally
    Args.Free;
  end;

  // Datos simulados según la ciudad
  if City.ToLower.Contains('monterrey') or City.ToLower.Contains('guadalajara') then
  begin
    TempC := 28; Cond := 'Soleado'; Hum := 45;
  end
  else if City.ToLower.Contains('méxico') or City.ToLower.Contains('cdmx') or
          City.ToLower.Contains('mexico') then
  begin
    TempC := 18; Cond := 'Nublado con lluvia ligera'; Hum := 75;
  end
  else
  begin
    TempC := 22; Cond := 'Parcialmente nublado'; Hum := 60;
  end;

  ToolCall.Response := Format(
    '{"city":"%s","temp_c":%d,"condition":"%s","humidity_pct":%d}',
    [City, TempC, Cond, Hum]);
  Handled := True;
end;

// =============================================================================
//  Run
// =============================================================================
procedure TSingleFunctionDemo.Run;
var
  Conn : TAiChatConnection;
  LFn  : TFunctionActionItem;
  LParam: TFunctionParamsItem;
  Res  : String;
begin
  Writeln('=== SingleFunction ===');
  Writeln('Driver: ', DRIVER, ' / Model: ', MODEL);
  Writeln;

  // ── Crear y configurar TAiFunctions ────────────────────────────────────────
  FAiFunctions := TAiFunctions.Create(nil);
  try
    LFn := FAiFunctions.Functions.AddFunction('get_weather', True, OnFnGetWeather);
    LFn.Description.Text :=
      'Obtiene el clima actual de una ciudad. Devuelve temperatura, ' +
      'condición y humedad.';

    LParam := LFn.Parameters.Add;
    LParam.Name        := 'city';
    LParam.ParamType   := ptString;
    LParam.Required    := True;
    LParam.Description.Text := 'Nombre de la ciudad (ej: "Monterrey", "Ciudad de México")';

    // ── Crear conexión ────────────────────────────────────────────────────────
    Conn := TAiChatConnection.Create(nil);
    try
      Conn.DriverName := DRIVER;
      Conn.Model      := MODEL;
      Conn.Params.Values['ApiKey']       := API_KEY;
      Conn.Params.Values['Asynchronous'] := 'False';
      Conn.Params.Values['Max_Tokens']   := '512';
      Conn.Params.Values['Tool_Active']  := 'True';
      Conn.AiFunctions := FAiFunctions;

      Conn.SystemPrompt.Text :=
        'Eres un asistente de clima. Cuando el usuario pregunte por el clima, ' +
        'usa la función get_weather para obtener datos reales. Responde en español.';

      // ── Pregunta 1: necesita la función ────────────────────────────────────
      Writeln('── Pregunta 1 (necesita la función) ──');
      Res := Conn.AddMessageAndRun('¿Cómo está el clima en Monterrey hoy?', 'user', []);
      Writeln('Respuesta: ', Res);
      Writeln;
      Conn.Messages.Clear;

      // ── Pregunta 2: NO necesita la función ─────────────────────────────────
      Writeln('── Pregunta 2 (no necesita la función) ──');
      Res := Conn.AddMessageAndRun('¿Cuál es la capital de México?', 'user', []);
      Writeln('Respuesta: ', Res);
      Writeln;
      Conn.Messages.Clear;

      // ── Pregunta 3: función con otra ciudad ────────────────────────────────
      Writeln('── Pregunta 3 (invoca función con otra ciudad) ──');
      Res := Conn.AddMessageAndRun(
        '¿Necesito llevar paraguas si salgo en CDMX?', 'user', []);
      Writeln('Respuesta: ', Res);

    finally
      Conn.Free;
    end;
  finally
    FAiFunctions.Free;
  end;
end;

// =============================================================================
//  Punto de entrada
// =============================================================================
begin
  try
    with TSingleFunctionDemo.Create do
    try
      Run;
    finally
      Free;
    end;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
