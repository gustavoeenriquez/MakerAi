program FuncionesLadera;

// =============================================================================
// LIBRO "IA con Delphi", capitulo 11 - Herramientas: el modelo llama funciones de Delphi
// =============================================================================
// Dos funciones de la tienda, declaradas en un TAiFunctions:
//   consultar_inventario(cafe)              -> bolsas disponibles
//   calcular_total(cafe, bolsas, ciudad)    -> subtotal, envio y total
// El modelo lee las descripciones, decide cuando llamarlas y con que argumentos;
// cada OnAction hace el trabajo y devuelve el resultado en ToolCall.Response.
//
// A. Las mismas preguntas SIN herramientas (Tool_Active := False): el modelo adivina o se disculpa.
// B. Con herramientas: cada llamada se imprime; al final, el historial con los mensajes 'tool'.
//
// Uso: FuncionesLadera.exe [--driver X]
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils, System.Classes, System.SyncObjs,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

const
  PROMPT_SISTEMA =
    'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano.' + sLineBreak +
    'Carta (bolsa de 340 g): Huila Reserva 38.000 COP; Nariño Altura 42.000 COP; Sierra Nevada Orgánico 36.000 COP; ' +
    'Descafeinado Suave 34.000 COP.' + sLineBreak +
    'Tienes herramientas: consultar_inventario para saber cuántas bolsas hay de un café, y calcular_total para ' +
    'cotizar un pedido con su envío. Úsalas en vez de adivinar; si un café está agotado, dilo y ofrece otro.' + sLineBreak +
    'Responde en español, breve.';
  PREGUNTAS: array [0 .. 1] of string = (
    '¿Cuántas bolsas de Sierra Nevada Orgánico les quedan?',
    'Quiero 2 bolsas de Huila Reserva para Pitalito. ¿Cuánto pago en total?');

var
  Candado: TCriticalSection;   // el modelo puede pedir varias herramientas a la vez, en hilos distintos

procedure Log(const S: string);
begin
  Candado.Enter;
  try
    Writeln(S);
  finally
    Candado.Leave;
  end;
end;

type
  // Los eventos de MakerAI son "of object": viven en una clase
  TTienda = class
    Llamadas: Integer;
    procedure ConsultarInventario(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure CalcularTotal(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  end;

function BolsasDe(const ACafe: string): Integer;
begin
  // El inventario de hoy (ficticio). En una app real: la base de datos.
  if ACafe.Contains('Huila') then Exit(24);
  if ACafe.Contains('Nari') then Exit(10);
  if ACafe.Contains('Sierra') then Exit(0);
  if ACafe.Contains('Descaf') then Exit(15);
  Result := -1;
end;

function PrecioDe(const ACafe: string): Integer;
begin
  if ACafe.Contains('Huila') then Exit(38000);
  if ACafe.Contains('Nari') then Exit(42000);
  if ACafe.Contains('Sierra') then Exit(36000);
  if ACafe.Contains('Descaf') then Exit(34000);
  Result := 0;
end;

procedure TTienda.ConsultarInventario(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Cafe: string;
  Bolsas: Integer;
begin
  Handled := True;                                   // la aplicacion atendio la llamada
  TInterlocked.Increment(Llamadas);
  Cafe := ToolCall.Params.Values['cafe'];            // los argumentos, ya parseados
  Bolsas := BolsasDe(Cafe);
  if Bolsas < 0 then
    ToolCall.Response := 'No existe ese café en la carta.'
  else
    ToolCall.Response := Format('%s: %d bolsas de 340 g disponibles.', [Cafe, Bolsas]);
  Log('   [herramienta] consultar_inventario(cafe=' + Cafe + ')   argumentos crudos: ' + ToolCall.Arguments +
    sLineBreak + '   [herramienta]   -> ' + ToolCall.Response);
end;

procedure TTienda.CalcularTotal(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Cafe, Ciudad: string;
  Bolsas, Subtotal, Envio: Integer;
begin
  Handled := True;
  TInterlocked.Increment(Llamadas);
  Cafe := ToolCall.Params.Values['cafe'];
  Bolsas := StrToIntDef(ToolCall.Params.Values['bolsas'], 0);
  Ciudad := ToolCall.Params.Values['ciudad'];
  Subtotal := PrecioDe(Cafe) * Bolsas;
  if SameText(Ciudad, 'Pitalito') then
    Envio := 5000
  else
    Envio := 12000;
  ToolCall.Response := Format('%d x %s = %d COP; envío a %s: %d COP; total: %d COP.',
    [Bolsas, Cafe, Subtotal, Ciudad, Envio, Subtotal + Envio]);
  Log(Format('   [herramienta] calcular_total(cafe=%s, bolsas=%d, ciudad=%s)', [Cafe, Bolsas, Ciudad]) +
    sLineBreak + '   [herramienta]   -> ' + ToolCall.Response);
end;

function CrearFunciones(ATienda: TTienda): TAiFunctions;

  procedure Parametro(F: TFunctionActionItem; const ANombre, ADescripcion: string; ATipo: TToolsParamType);
  var
    P: TFunctionParamsItem;
  begin
    P := F.Parameters.Add;
    P.Name := ANombre;
    P.ParamType := ATipo;
    P.Description.Text := ADescripcion;
    P.Required := True;
  end;

var
  F: TFunctionActionItem;
begin
  Result := TAiFunctions.Create(nil);

  F := Result.Functions.Add;
  F.FunctionName := 'consultar_inventario';
  F.Description.Text := 'Cuántas bolsas de 340 g hay disponibles de un café de la carta.';
  F.OnAction := ATienda.ConsultarInventario;
  Parametro(F, 'cafe', 'Nombre del café tal como está en la carta.', ptString);

  F := Result.Functions.Add;
  F.FunctionName := 'calcular_total';
  F.Description.Text := 'Cotiza un pedido de un café: precio por las bolsas más el envío a la ciudad. Devuelve subtotal, envío y total en COP.';
  F.OnAction := ATienda.CalcularTotal;
  Parametro(F, 'cafe', 'Nombre del café tal como está en la carta.', ptString);
  Parametro(F, 'bolsas', 'Número de bolsas de 340 g.', ptInteger);
  Parametro(F, 'ciudad', 'Ciudad de entrega en Colombia.', ptString);
end;

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

procedure Titulo(const S: string);
begin
  Writeln('==== ', S, ' ', StringOfChar('=', 62 - Length(S)));
  Writeln;
end;

procedure Charla(Conn: TAiChatConnection; ATienda: TTienda);
var
  I: Integer;
  R: string;
  Antes: Integer;
begin
  for I := 0 to High(PREGUNTAS) do
  begin
    Writeln('Tú: ', PREGUNTAS[I]);
    Antes := Conn.Total_tokens;
    R := Conn.AddMessageAndRun(PREGUNTAS[I], 'user', []);
    if R = '' then
      Writeln('[error] ', Conn.LastError)
    else
      Writeln('Lucía: ', R.Trim);
    Writeln(Format('   (%d tokens en este turno)', [Conn.Total_tokens - Antes]));
    Writeln;
  end;
end;

procedure MostrarHistorial(Conn: TAiChatConnection);
var
  I: Integer;
  M: TAiChatMessage;
  Texto: string;
begin
  Writeln('--- Historial: ', Conn.Messages.Count, ' mensajes ---');
  for I := 0 to Conn.Messages.Count - 1 do
  begin
    M := Conn.Messages[I];
    Texto := M.Prompt.Replace(sLineBreak, ' ').Trim;
    if (Texto = '') and (M.ToolCallId <> '') then
      Texto := '(llamada a herramienta)';
    if Texto.Length > 72 then
      Texto := Texto.Substring(0, 72) + '...';
    Writeln(Format('  %d. %-9s %s', [I + 1, M.Role, Texto]));
  end;
  Writeln;
end;

var
  Conn: TAiChatConnection;
  Funciones: TAiFunctions;
  Tienda: TTienda;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  Candado := TCriticalSection.Create;
  Tienda := TTienda.Create;
  Funciones := CrearFunciones(Tienda);
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := ArgValue('--driver', 'OpenAi');
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.SystemPrompt.Text := PROMPT_SISTEMA;
    Writeln('Chat: ', Conn.DriverName, ' / ', Conn.AiChat.Model);
    Writeln;

    Titulo('A. Sin herramientas: el modelo no tiene cómo saberlo');
    Conn.Tool_Active := False;                       // las funciones no se declaran
    Charla(Conn, Tienda);

    Titulo('B. Con herramientas: el modelo pregunta y la aplicación responde');
    Conn.NewChat;
    Conn.AiFunctions := Funciones;                   // el catalogo de herramientas
    Conn.Tool_Active := True;                        // se declaran al modelo
    Charla(Conn, Tienda);
    Writeln('Llamadas a herramientas: ', Tienda.Llamadas);
    Writeln;
    MostrarHistorial(Conn);
  finally
    Conn.Free;
    Funciones.Free;
    Tienda.Free;
    Candado.Free;
  end;
end.
