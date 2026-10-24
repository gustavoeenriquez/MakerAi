program PromptsLadera;

// =============================================================================
// LIBRO "IA con Delphi", capitulo 5 - Prompt engineering con TAiPrompts
// =============================================================================
// La misma pregunta con cuatro prompts de sistema distintos, para ver que cambia:
//   1. generico      : "Eres un asistente util" (sin negocio, sin reglas)
//   2. lucia         : la barista con la carta y las reglas (el del capitulo 3)
//   3. lucia-json    : Lucia obligada a responder en JSON para que el programa lo lea
//   4. plantilla     : el prompt de Lucia como plantilla con <#variables> de TAiPrompts
//
// Uso: PromptsLadera.exe [--driver X] [--model Y]
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils, System.JSON,
  uMakerAi.Prompts,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

const
  CARTA = 'Huila Reserva 38.000 COP (panela, frutos rojos); Nariño Altura 42.000 COP (cítricos, caramelo); ' +
    'Sierra Nevada Orgánico 36.000 COP (chocolate amargo, nuez); Descafeinado Suave 34.000 COP (cacao suave)';
  REGLAS = 'Reglas: responde en español, cálido y breve (máximo 4 líneas). ' +
    'Solo hablas de café y de la tienda; si te preguntan otra cosa, vuelve con amabilidad al café. ' +
    'No inventes productos ni precios fuera de la carta.';
  PREGUNTAS: array [0 .. 1] of string = (
    'Hola, ¿qué café me recomiendas si me gusta el chocolate?',
    '¿Me ayudas con la tarea de matemáticas?');

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
  Prompts: TAiPrompts;
  Conn: TAiChatConnection;

procedure Probar(const ATitulo, APromptSistema: string; const AJson: Boolean = False);
var
  I: Integer;
  R: string;
begin
  Writeln('==== ', ATitulo, ' ', StringOfChar('=', 60 - Length(ATitulo)));
  Writeln('SystemPrompt (', Length(APromptSistema), ' caracteres):');
  Writeln('  ', APromptSistema.Replace(sLineBreak, sLineBreak + '  '));
  Writeln;
  Conn.NewChat;                                   // historial limpio para cada prompt
  Conn.ResetTokenCounters;
  Conn.SystemPrompt.Text := APromptSistema;
  if AJson then
    Conn.Params.Values['Response_format'] := 'json_object'
  else
    Conn.Params.Values['Response_format'] := '';
  for I := 0 to High(PREGUNTAS) do
  begin
    Writeln('Tú: ', PREGUNTAS[I]);
    R := Conn.AddMessageAndRun(PREGUNTAS[I], 'user', []);
    if R = '' then
      Writeln('[error] ', Conn.LastError)
    else
      Writeln('Lucía: ', R.Trim);
    Writeln;
  end;
  Writeln(Format('  Tokens: %d de entrada, %d de salida', [Conn.Prompt_tokens, Conn.Completion_tokens]));
  Writeln;
end;

var
  Json: TJSONObject;
  Texto: string;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  Prompts := TAiPrompts.Create(nil);
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := ArgValue('--driver', 'OpenAi');
    Conn.Model := ArgValue('--model', '');
    Conn.Params.Values['Asynchronous'] := 'False';
    Writeln('Chat: ', Conn.DriverName, ' / ', Conn.AiChat.Model);
    Writeln;

    // Los prompts viven en el componente, no sueltos por el codigo
    Prompts.AddString('generico', 'Eres un asistente útil.');
    Prompts.AddString('lucia',
      'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano.' + sLineBreak +
      'Carta (bolsa de 340 g): ' + CARTA + '.' + sLineBreak + REGLAS);
    Prompts.AddString('lucia-json',
      Prompts.GetString('lucia') + sLineBreak +
      'Responde SOLO con un objeto JSON con las claves "recomendacion" (nombre del café o null), ' +
      '"precio_cop" (número o null) y "mensaje" (lo que le dirías al cliente, máximo 2 líneas).');
    // Una plantilla: lo que cambia entre tiendas va en <#variables>
    Prompts.AddString('plantilla',
      'Eres <#nombre>, <#rol> de <#tienda>.' + sLineBreak +
      'Carta: <#carta>.' + sLineBreak + REGLAS);

    Probar('1. Prompt genérico', Prompts.GetString('generico'));
    Probar('2. Lucía', Prompts.GetString('lucia'));
    Probar('3. Lucía en JSON', Prompts.GetString('lucia-json'), True);

    // La plantilla rellenada con otra tienda y otro nombre
    Texto := Prompts.GetTemplate('plantilla', ['nombre=Tomás', 'rol=el tostador', 'tienda=Café La Ladera',
      'carta=' + CARTA]);
    Probar('4. Plantilla rellenada', Texto);

    // Lo que un programa hace con la respuesta JSON del caso 3: leerla
    Conn.NewChat;
    Conn.SystemPrompt.Text := Prompts.GetString('lucia-json');
    Conn.Params.Values['Response_format'] := 'json_object';
    Texto := Conn.AddMessageAndRun(PREGUNTAS[0], 'user', []);
    Json := TJSONObject.ParseJSONValue(Texto) as TJSONObject;
    if Assigned(Json) then
    try
      Writeln('==== 5. El programa lee el JSON ', StringOfChar('=', 33));
      Writeln('  recomendacion = ', Json.GetValue<string>('recomendacion', '?'));
      Writeln('  precio_cop    = ', Json.GetValue<Integer>('precio_cop', 0));
      Writeln('  mensaje       = ', Json.GetValue<string>('mensaje', '?'));
    finally
      Json.Free;
    end
    else
      Writeln('[error] la respuesta no es JSON: ', Texto);
  finally
    Conn.Free;
    Prompts.Free;
  end;
end.
