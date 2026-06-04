program AdvancedParams;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 01-Chat / 05-AdvancedParams
// =============================================================================
// Exploración de todos los parámetros avanzados de TAiChatConnection.
//
// Conceptos que cubre:
//   - Temperature     : controla aleatoriedad (0=determinista, 1=creativo)
//   - MaxTokens       : límite de tokens en la respuesta
//   - SystemPrompt    : instrucciones de sistema (personalidad, restricciones)
//   - ThinkingLevel   : razonamiento extendido (Claude, Gemini)
//   - ResponseFormat  : texto libre vs. JSON estructurado
//   - Stop sequences  : detener generación en un token específico
//   - Timeout         : tiempo máximo de espera de respuesta
//   - Model switching : cambiar modelo sin recrear la conexión
// =============================================================================

uses
  System.SysUtils,
  System.JSON,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Ollama,
  uMakerAi.Chat.Groq;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

// Helper para separar secciones en la salida
procedure Section(const Title: String);
begin
  Writeln;
  Writeln(StringOfChar('=', 60));
  Writeln('  ', Title);
  Writeln(StringOfChar('=', 60));
end;

// =============================================================================
//  DEMO 1 — Efecto de Temperature
// =============================================================================
procedure DemoTemperature(Conn: TAiChatConnection);
const
  PROMPT = 'Dame una metáfora creativa para describir la programación.';
  TEMPS  : array[0..2] of String = ('0.0', '0.7', '1.5');
var
  T   : String;
  Res : String;
begin
  Section('Temperature (0.0 = determinista  /  1.5 = muy creativo)');

  for T in TEMPS do
  begin
    Conn.Messages.Clear;
    Conn.Params.Values['Temperature'] := T;
    Res := Conn.AddMessageAndRun(PROMPT, 'user', []);
    Writeln(Format('  Temperature=%s:', [T]));
    Writeln('  ', Res);
    Writeln;
  end;

  // Restaurar valor normal
  Conn.Params.Values['Temperature'] := '0.7';
end;

// =============================================================================
//  DEMO 2 — MaxTokens: respuesta corta vs. larga
// =============================================================================
procedure DemoMaxTokens(Conn: TAiChatConnection);
const
  PROMPT = 'Explica qué es machine learning.';
var
  Tokens : String;
  Res    : String;
begin
  Section('MaxTokens (controla la longitud máxima de la respuesta)');

  for Tokens in ['50', '200', '1000'] do
  begin
    Conn.Messages.Clear;
    Conn.Params.Values['Max_Tokens'] := Tokens;
    Res := Conn.AddMessageAndRun(PROMPT, 'user', []);
    Writeln(Format('  MaxTokens=%s → %d caracteres', [Tokens, Length(Res)]));
    Writeln('  ', Copy(Res, 1, 120), '...');
    Writeln;
  end;

  // Restaurar
  Conn.Params.Values['Max_Tokens'] := '1024';
end;

// =============================================================================
//  DEMO 3 — SystemPrompt: cambia la personalidad del LLM
// =============================================================================
procedure DemoSystemPrompt(Conn: TAiChatConnection);
const
  PREGUNTA = '¿Cómo aprender a programar?';
  SISTEMAS : array[0..2] of String = (
    'Eres un profesor universitario formal. Usa terminología técnica.',
    'Eres un amigo de 14 años. Habla como adolescente, con emojis.',
    'Eres un pirata del Caribe del siglo XVII. Solo hablas como pirata.'
  );
var
  S   : String;
  Res : String;
  I   : Integer;
begin
  Section('SystemPrompt (cambia la personalidad sin cambiar la pregunta)');
  Writeln('  Misma pregunta: "', PREGUNTA, '"');
  Writeln;

  I := 1;
  for S in SISTEMAS do
  begin
    Conn.Messages.Clear;
    Conn.SystemPrompt.Text := S;
    Res := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
    Writeln(Format('  Persona %d: %s', [I, Copy(S, 1, 45), '...']));
    Writeln('  Respuesta: ', Copy(Res, 1, 150));
    Writeln;
    Inc(I);
  end;

  // Restaurar
  Conn.SystemPrompt.Text := 'Responde siempre en español.';
end;

// =============================================================================
//  DEMO 4 — Respuesta en formato JSON estructurado
// =============================================================================
procedure DemoJSONMode(Conn: TAiChatConnection);
var
  Res    : String;
  JObj   : TJSONObject;
begin
  Section('JSON Mode (respuesta estructurada como JSON)');

  Conn.Messages.Clear;
  Conn.SystemPrompt.Text :=
    'Responde SIEMPRE en JSON válido, sin texto adicional. ' +
    'El JSON debe tener exactamente estos campos: ' +
    '"nombre", "capital", "poblacion_millones", "idioma_oficial".';

  Conn.Params.Values['Response_format'] := 'json';
  Conn.Params.Values['Max_Tokens']      := '256';

  Res := Conn.AddMessageAndRun('Dame info sobre México.', 'user', []);
  Writeln('  Respuesta raw:');
  Writeln('  ', Res);

  // Parsear y acceder a campos
  try
    JObj := TJSONObject.ParseJSONValue(Res) as TJSONObject;
    if Assigned(JObj) then
    try
      Writeln;
      Writeln('  Parsed:');
      Writeln('    Nombre    : ', JObj.GetValue<String>('nombre', '?'));
      Writeln('    Capital   : ', JObj.GetValue<String>('capital', '?'));
      Writeln('    Población : ', JObj.GetValue<Double>('poblacion_millones', 0):0:1, 'M');
      Writeln('    Idioma    : ', JObj.GetValue<String>('idioma_oficial', '?'));
    finally
      JObj.Free;
    end;
  except
    Writeln('  (No se pudo parsear el JSON)');
  end;

  // Restaurar
  Conn.Params.Values['Response_format'] := '';
end;

// =============================================================================
//  DEMO 5 — ThinkingLevel: razonamiento extendido
//  (Solo soportado en Claude con modelos que soporten extended thinking)
// =============================================================================
procedure DemoThinkingLevel(Conn: TAiChatConnection);
var
  Res: String;
begin
  Section('ThinkingLevel (razonamiento extendido — Claude)');
  Writeln('  Nota: Requiere modelo que soporte Extended Thinking.');
  Writeln('  En modelos que no lo soporten, se ignora el parámetro.');
  Writeln;

  Conn.Messages.Clear;
  Conn.Params.Values['ThinkingLevel'] := 'tlMedium';  // tlLow, tlMedium, tlHigh
  Conn.Params.Values['Max_Tokens']    := '2048';

  Res := Conn.AddMessageAndRun(
    'Resuelve paso a paso: Si tengo 3 cajas con 4 pelotas cada una, ' +
    'y regalo la mitad de todas las pelotas, ¿cuántas me quedan?',
    'user', []);

  Writeln('  Respuesta (con razonamiento):');
  Writeln('  ', Res);

  // Restaurar
  Conn.Params.Values['ThinkingLevel'] := 'tlDefault';
  Conn.Params.Values['Max_Tokens']    := '1024';
end;

// =============================================================================
//  DEMO principal
// =============================================================================
procedure RunDemo;
var
  Conn: TAiChatConnection;
begin
  Writeln('=== AdvancedParams ===');
  Writeln('Driver: ', DRIVER, ' / Model: ', MODEL);

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;

    Conn.Params.Values['ApiKey']            := API_KEY;
    Conn.Params.Values['Asynchronous']      := 'False';
    Conn.Params.Values['Max_Tokens']        := '1024';
    Conn.Params.Values['Temperature']       := '0.7';
    Conn.Params.Values['ResponseTimeOut']   := '120000';  // 2 min

    Conn.SystemPrompt.Text := 'Responde siempre en español.';

    DemoTemperature(Conn);
    DemoMaxTokens(Conn);
    DemoSystemPrompt(Conn);
    DemoJSONMode(Conn);
    DemoThinkingLevel(Conn);

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
