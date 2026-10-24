program ProveedoresLadera;

// =============================================================================
// LIBRO "IA con Delphi", capitulo 6 - Cambiar de cerebro
// =============================================================================
// A. La misma pregunta de Lucia a varios proveedores: respuesta, tiempo y tokens.
// B. Fallback: si el primero falla, se intenta el siguiente.
// C. Los modelos que ofrece un proveedor (GetModels), p. ej. los de tu Ollama.
//
// Uso: ProveedoresLadera.exe [--solo "OpenAi,Ollama:gemma4:e4b"]
// Un proveedor se escribe Driver o Driver:modelo. Los que no tienen clave se saltan.
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils, System.Classes, System.Diagnostics, System.StrUtils, System.Math,
  uMakerAi.Core,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

const
  PROMPT_SISTEMA =
    'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano.' + sLineBreak +
    'Carta (bolsa de 340 g): Huila Reserva 38.000 COP (panela, frutos rojos); ' +
    'Nariño Altura 42.000 COP (cítricos, caramelo); Sierra Nevada Orgánico 36.000 COP ' +
    '(chocolate amargo, nuez); Descafeinado Suave 34.000 COP (cacao suave).' + sLineBreak +
    'Reglas: responde en español, cálido y breve (máximo 4 líneas). ' +
    'Solo hablas de café y de la tienda; si te preguntan otra cosa, vuelve con amabilidad al café. ' +
    'No inventes productos ni precios fuera de la carta.';
  PREGUNTA = 'Hola, ¿qué café me recomiendas si me gusta el chocolate?';
  // Driver[:modelo]. Vacio = el modelo por defecto del driver.
  PROVEEDORES = 'OpenAi,Claude,Gemini,Groq,DeepSeek,Mistral,Ollama:gemma4:e4b,Ollama:gpt-oss:20b';

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

// Crea una conexion lista para Lucia con el driver (y modelo) pedidos
function Conectar(const ADriver, AModel: string): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := ADriver;
  Result.Model := AModel;
  Result.Params.Values['Asynchronous'] := 'False';
  Result.SystemPrompt.Text := PROMPT_SISTEMA;
  // ModelCaps/SessionCaps del conector son lo que TU fijas; vacios, se usa el catalogo del
  // modelo. Claude trae cap_WebSearch en el catalogo y declara la busqueda web en cada
  // llamada: unos 2.250 tokens de entrada por pregunta. Esta comparacion es solo de texto,
  // asi que se fija el conjunto completo vacio (no se puede "restar" del catalogo).
  Result.ModelCaps := [];
  Result.SessionCaps := [];
end;

function TieneClave(const ADriver: string): Boolean;
var
  V: string;
begin
  if MatchText(ADriver, ['Ollama', 'LMStudio', 'GenericLLM']) then
    Exit(True);
  if SameText(ADriver, 'OpenAi') then
    V := 'OPENAI_API_KEY'
  else if SameText(ADriver, 'Qwen') then
    V := 'DASHSCOPE_API_KEY'
  else
    V := UpperCase(ADriver) + '_API_KEY';
  Result := GetEnvironmentVariable(V) <> '';
end;

procedure Titulo(const S: string);
begin
  Writeln('==== ', S, ' ', StringOfChar('=', 60 - Length(S)));
  Writeln;
end;

{ A. La misma pregunta a todos }

procedure CompararProveedores(const ALista: string);
var
  Lista: TArray<string>;
  Item, Driver, Model, R: string;
  P: Integer;
  Conn: TAiChatConnection;
  Reloj: TStopwatch;
begin
  Titulo('A. La misma pregunta, varios cerebros');
  Writeln('Tú: ', PREGUNTA);
  Writeln;
  Lista := ALista.Split([',']);
  for Item in Lista do
  begin
    P := Pos(':', Item);
    if P > 0 then
    begin
      Driver := Copy(Item, 1, P - 1);
      Model := Copy(Item, P + 1, MaxInt);
    end
    else
    begin
      Driver := Item;
      Model := '';
    end;
    if not TieneClave(Driver) then
    begin
      Writeln('-- ', Driver, ': sin clave en el entorno, se salta.');
      Writeln;
      Continue;
    end;
    Conn := Conectar(Driver, Model);
    try
      Reloj := TStopwatch.StartNew;
      R := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
      Writeln('-- ', Conn.DriverName, ' / ', Conn.AiChat.Model, Format('  (%d ms, %d/%d tokens)',
        [Reloj.ElapsedMilliseconds, Conn.Prompt_tokens, Conn.Completion_tokens]));
      if R = '' then
        Writeln('   [error] ', Conn.LastError)
      else
        Writeln('   ', R.Trim.Replace(sLineBreak, ' '));
      Writeln;
    finally
      Conn.Free;
    end;
  end;
end;

{ B. Fallback: si uno falla, sigue el siguiente }

function PreguntarConFallback(const ACadena: array of string; const APregunta: string): string;
var
  I, P: Integer;
  Driver, Model: string;
  Conn: TAiChatConnection;
begin
  Result := '';
  for I := 0 to High(ACadena) do
  begin
    P := Pos(':', ACadena[I]);
    Driver := IfThen(P > 0, Copy(ACadena[I], 1, P - 1), ACadena[I]);
    Model := IfThen(P > 0, Copy(ACadena[I], P + 1, MaxInt), '');
    Conn := Conectar(Driver, Model);
    try
      Writeln('   intento ', I + 1, ': ', Driver, IfThen(Model <> '', ' / ' + Model));
      Result := Conn.AddMessageAndRun(APregunta, 'user', []);
      if Result <> '' then
        Exit;
      Writeln('   falló: ', Conn.LastError.Replace(sLineBreak, ' ').Substring(0, 120));
    finally
      Conn.Free;
    end;
  end;
end;

{ C. Que modelos ofrece un proveedor }

procedure ListarModelos(const ADriver: string);
var
  Conn: TAiChatConnection;
  Modelos: TStringList;
  I: Integer;
begin
  Titulo('C. Modelos disponibles en ' + ADriver);
  Conn := Conectar(ADriver, '');
  try
    Modelos := Conn.GetModels;
    try
      Writeln('   ', Modelos.Count, ' modelos. Los primeros:');
      for I := 0 to Min(Modelos.Count, 8) - 1 do
        Writeln('   - ', Modelos[I]);
    finally
      Modelos.Free;
    end;
  finally
    Conn.Free;
  end;
  Writeln;
end;

var
  R: string;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  try
    CompararProveedores(ArgValue('--solo', PROVEEDORES));

    Titulo('B. Fallback: un modelo que no existe, y el siguiente responde');
    R := PreguntarConFallback(['OpenAi:gpt-no-existe', 'Groq'], PREGUNTA);
    Writeln('   Lucía: ', R.Trim.Replace(sLineBreak, ' '));
    Writeln;

    ListarModelos('Ollama');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
