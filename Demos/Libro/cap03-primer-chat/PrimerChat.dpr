program PrimerChat;

// =============================================================================
// LIBRO "IA con Delphi", capitulo 3 - El primer chat con Lucia (consola)
// =============================================================================
// Lo minimo que hace falta para hablar con un modelo desde Delphi:
//   1. un TAiChatConnection,
//   2. el driver y el modelo,
//   3. un prompt de sistema (quien es Lucia, que vende, que reglas sigue),
//   4. AddMessageAndRun: agrega la pregunta al historial, llama al modelo y
//      devuelve la respuesta.
//
// Uso:
//   PrimerChat.exe                                  OpenAI (@OPENAI_API_KEY)
//   PrimerChat.exe --driver Ollama --model gemma4   local, sin clave
//
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;   // registra todos los drivers

const
  PROMPT_SISTEMA =
    'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano.' + sLineBreak +
    'Carta (bolsa de 340 g): Huila Reserva 38.000 COP (panela, frutos rojos); ' +
    'Nariño Altura 42.000 COP (cítricos, caramelo); Sierra Nevada Orgánico 36.000 COP ' +
    '(chocolate amargo, nuez); Descafeinado Suave 34.000 COP (cacao suave).' + sLineBreak +
    'Reglas: responde en español, cálido y breve (máximo 4 líneas). ' +
    'Solo hablas de café y de la tienda; si te preguntan otra cosa, vuelve con amabilidad al café. ' +
    'No inventes productos ni precios fuera de la carta.';

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

var
  Conn: TAiChatConnection;
  Pregunta, Respuesta: string;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);   // tildes y enies en la consola
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := ArgValue('--driver', 'OpenAi');   // el proveedor
    Conn.Model := ArgValue('--model', '');               // vacio = el modelo por defecto del driver
    Conn.Params.Values['Asynchronous'] := 'False';       // respuesta completa, de una vez
    Conn.SystemPrompt.Text := PROMPT_SISTEMA;            // quien es Lucia

    Writeln('Chat: ', Conn.DriverName, ' / ', Conn.AiChat.Model);
    Writeln;
    Pregunta := 'Hola, ¿qué café me recomiendas si me gusta el chocolate?';
    Writeln('Tú: ', Pregunta);
    Respuesta := Conn.AddMessageAndRun(Pregunta, 'user', []);   // bloquea hasta la respuesta
    if Respuesta = '' then
      Writeln('[error] ', Conn.LastError)
    else
      Writeln('Lucía: ', Respuesta.Trim);
    Writeln;
    Writeln('Mensajes en el historial: ', Conn.Messages.Count,
      '  (sistema, tu pregunta y la respuesta)');
    Writeln('Tokens: ', Conn.Prompt_tokens, ' de entrada, ', Conn.Completion_tokens, ' de salida');
  finally
    Conn.Free;
  end;
end.
