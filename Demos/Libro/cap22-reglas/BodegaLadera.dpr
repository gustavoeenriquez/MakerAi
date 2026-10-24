program BodegaLadera;

// =============================================================================
// LIBRO "IA con Delphi", capitulo 16 - Conectores: la bodega como servidor MCP (= demo 100)
// =============================================================================
// Las funciones de la bodega de Café La Ladera se publican con MCP (Model
// Context Protocol): cualquier cliente MCP las usa sin saber que son Delphi.
// El mismo .exe hace los dos papeles:
//
//   BodegaLadera.exe --servidor  Servidor MCP por stdio. Es lo que lanza LaderaApp y lo que se
//                                configura en Claude Desktop o en Claude Code.
//   BodegaLadera.exe             Chat que lanza el servidor como proceso hijo,
//                              descubre sus herramientas y las usa. Muestra
//                              cada tools/call que viaja por stdio.
//
//   - TAiMCPStdioServer: publica un TAiFunctions (inventario, precio_mayorista).
//   - MCPClients       : en el TAiFunctions del chat; las herramientas del
//                        servidor llegan al modelo junto con las locales.
//
// Regla de stdio: en modo servidor stdout es el canal JSON-RPC. Nada de
// Writeln ahi; los mensajes van a ErrOutput.
//
// Requiere OPENAI_API_KEY (solo el chat; el servidor no llama a ningun modelo).
// Empresa, productos, existencias y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.StrUtils,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.MCPServer.Core,
  UMakerAi.MCPServer.Stdio,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;   // registra todos los drivers

type
  TCafe = record
    Nombre: string;
    Kilos: Integer;        // en bodega
    PrecioKilo: Integer;   // COP, precio mayorista base
  end;

const
  CAFES: array [0 .. 3] of TCafe = (
    (Nombre: 'Huila Reserva'; Kilos: 18; PrecioKilo: 98000),
    (Nombre: 'Nariño Altura'; Kilos: 6; PrecioKilo: 108000),
    (Nombre: 'Sierra Nevada Orgánico'; Kilos: 30; PrecioKilo: 92000),
    (Nombre: 'Descafeinado Suave'; Kilos: 0; PrecioKilo: 88000));

  PREGUNTAS: array [0 .. 2] of string = (
    '¿Cuántos kilos de Nariño Altura nos quedan en bodega?',
    'Un hotel quiere 12 kilos de Huila Reserva. ¿Los tenemos y cuánto le cobramos?',
    'También pregunta por 5 kilos de Descafeinado Suave.');

type
  // Los eventos del framework son 'of object': los handlers viven en una clase
  TBodega = class
  public
    procedure Inventario(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure PrecioMayorista(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  end;

  // Cliente: muestra las llamadas tools/call que viajan por stdio
  TTrafico = class
  private
    FPendientes: TStringList;   // ids de tools/call sin respuesta
  public
    constructor Create;
    destructor Destroy; override;
    procedure McpLog(Sender: TObject; const Msg: string);
  end;

var
  FmtCOP: TFormatSettings;

function Pesos(AValor: Double): string;
begin
  Result := FormatFloat('#,##0', AValor, FmtCOP) + ' COP';
end;

// Sin tildes y en minusculas: el modelo puede escribir "Narino" o "nariño"
function Normalizar(const S: string): string;
const
  CON = 'áéíóúüñ';
  SIN = 'aeiouun';
var
  I, P: Integer;
begin
  Result := LowerCase(Trim(S));
  for I := 1 to Length(Result) do
  begin
    P := Pos(Result[I], CON);
    if P > 0 then
      Result[I] := SIN[P];
  end;
end;

function BuscarCafe(const ANombre: string; out ACafe: TCafe): Boolean;
var
  C: TCafe;
  Buscado: string;
begin
  Buscado := Normalizar(ANombre);
  for C in CAFES do
    if (Buscado <> '') and (Normalizar(C.Nombre).StartsWith(Buscado) or
      Buscado.StartsWith(Normalizar(C.Nombre).Split([' '])[0])) then
    begin
      ACafe := C;
      Exit(True);
    end;
  Result := False;
end;

function NombresCafes: string;
var
  C: TCafe;
begin
  Result := '';
  for C in CAFES do
    Result := Result + IfThen(Result = '', '', ', ') + C.Nombre;
end;

{ TBodega }

procedure TBodega.Inventario(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  C: TCafe;
begin
  Handled := True;
  if not BuscarCafe(ToolCall.Params.Values['cafe'], C) then
    ToolCall.Response := 'No existe ese café. Cafés de la carta: ' + NombresCafes + '.'
  else if C.Kilos = 0 then
    ToolCall.Response := C.Nombre + ': agotado. Próximo tueste: martes.'
  else
    ToolCall.Response := Format('%s: %d kg en bodega.', [C.Nombre, C.Kilos]);
end;

procedure TBodega.PrecioMayorista(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  C: TCafe;
  Kilos, Descuento: Integer;
  Bruto, Total: Double;
begin
  Handled := True;
  if not BuscarCafe(ToolCall.Params.Values['cafe'], C) then
  begin
    ToolCall.Response := 'No existe ese café. Cafés de la carta: ' + NombresCafes + '.';
    Exit;
  end;
  Kilos := StrToIntDef(ToolCall.Params.Values['kilos'], 0);
  if Kilos <= 0 then
  begin
    ToolCall.Response := 'Los kilos deben ser un número entero mayor que cero.';
    Exit;
  end;
  // Descuento mayorista: 10 % desde 10 kg, 15 % desde 25 kg
  if Kilos >= 25 then
    Descuento := 15
  else if Kilos >= 10 then
    Descuento := 10
  else
    Descuento := 0;
  Bruto := Kilos * C.PrecioKilo;
  Total := Bruto * (100 - Descuento) / 100;
  ToolCall.Response := Format('%d kg de %s a %s el kilo = %s; descuento %d %%; total %s.',
    [Kilos, C.Nombre, Pesos(C.PrecioKilo), Pesos(Bruto), Descuento, Pesos(Total)]);
end;

{ TTrafico }

constructor TTrafico.Create;
begin
  inherited;
  FPendientes := TStringList.Create;
end;

destructor TTrafico.Destroy;
begin
  FPendientes.Free;
  inherited;
end;

procedure TTrafico.McpLog(Sender: TObject; const Msg: string);
const
  ENVIO = 'CLIENT -> SERVER: ';
  RESPUESTA = 'SERVER -> CLIENT: ';
var
  J: TJSONValue;
  Params, Res: TJSONObject;
  Contenido: TJSONArray;
  Id, Texto: string;
begin
  if Msg.StartsWith(ENVIO) then
  begin
    J := TJSONObject.ParseJSONValue(Msg.Substring(Length(ENVIO)));
    try
      if (J is TJSONObject) and (TJSONObject(J).GetValue<string>('method', '') = 'tools/call') then
      begin
        Params := TJSONObject(J).GetValue<TJSONObject>('params');
        FPendientes.Add(TJSONObject(J).GetValue<string>('id', ''));
        Writeln('   [MCP] tools/call ', Params.GetValue<string>('name', ''), ' ',
          Params.GetValue<TJSONObject>('arguments').ToString);   // ToString no escapa las tildes
      end;
    finally
      J.Free;
    end;
  end
  else if Msg.StartsWith(RESPUESTA) then
  begin
    J := TJSONObject.ParseJSONValue(Msg.Substring(Length(RESPUESTA)));
    try
      if J is TJSONObject then
      begin
        Id := TJSONObject(J).GetValue<string>('id', '');
        if (Id <> '') and (FPendientes.IndexOf(Id) >= 0) then
        begin
          FPendientes.Delete(FPendientes.IndexOf(Id));
          Texto := '';
          if TJSONObject(J).TryGetValue<TJSONObject>('result', Res) and
            Res.TryGetValue<TJSONArray>('content', Contenido) and (Contenido.Count > 0) then
            Texto := TJSONObject(Contenido.Items[0]).GetValue<string>('text', '');
          Writeln('   [MCP] resultado: ', Texto);
        end;
      end;
    finally
      J.Free;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// Las funciones de la bodega: las mismas que se usarian dentro de un chat
// ---------------------------------------------------------------------------
function CrearFunciones(ABodega: TBodega): TAiFunctions;
var
  F: TFunctionActionItem;
  P: TFunctionParamsItem;
begin
  Result := TAiFunctions.Create(nil);

  F := Result.Functions.Add;
  F.FunctionName := 'inventario';
  F.Description.Text := 'Kilos de un café que hay en la bodega de Café La Ladera.';
  F.OnAction := ABodega.Inventario;
  P := F.Parameters.Add;
  P.Name := 'cafe';
  P.ParamType := ptString;
  P.Description.Text := 'Nombre del café: ' + NombresCafes + '.';
  P.Required := True;

  F := Result.Functions.Add;
  F.FunctionName := 'precio_mayorista';
  F.Description.Text := 'Precio de una venta al por mayor, con el descuento por volumen.';
  F.OnAction := ABodega.PrecioMayorista;
  P := F.Parameters.Add;
  P.Name := 'cafe';
  P.ParamType := ptString;
  P.Description.Text := 'Nombre del café.';
  P.Required := True;
  P := F.Parameters.Add;
  P.Name := 'kilos';
  P.ParamType := ptInteger;
  P.Description.Text := 'Kilos que se van a vender.';
  P.Required := True;
end;

// ---------------------------------------------------------------------------
// Modo servidor: lo lanza el cliente MCP (Claude Desktop, Claude Code, el chat)
// ---------------------------------------------------------------------------
procedure CorrerServidor;
var
  Bodega: TBodega;
  Funciones: TAiFunctions;
  Servidor: TAiMCPStdioServer;
begin
  Bodega := TBodega.Create;
  Funciones := CrearFunciones(Bodega);
  Servidor := TAiMCPStdioServer.Create(nil);
  try
    Servidor.ServerName := 'ladera';
    Servidor.AiFunctions := Funciones;
    Servidor.Start;
    WriteLn(ErrOutput, 'Servidor MCP de Café La Ladera listo (stdio).');
    // Termina cuando el cliente cierra stdin
    Servidor.WaitForInputClosed;
    Servidor.Stop;
  finally
    Servidor.Free;
    Funciones.Free;
    Bodega.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Modo cliente: un chat que usa el servidor como herramienta
// ---------------------------------------------------------------------------
procedure CorrerCliente;
var
  Trafico: TTrafico;
  Funciones: TAiFunctions;
  Servidor: TMCPClientItem;
  Chat: TAiChatConnection;
  Lista: TJSONObject;
  Tools: TJSONArray;
  T: TJSONValue;
  Pregunta, R: string;
begin
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('Falta la variable OPENAI_API_KEY.');
    ExitCode := 1;
    Exit;
  end;

  Trafico := TTrafico.Create;
  Funciones := TAiFunctions.Create(nil);
  Chat := TAiChatConnection.Create(nil);
  try
    // El servidor MCP: este mismo .exe con --servidor, como proceso hijo
    Funciones.OnLog := Trafico.McpLog;
    Servidor := Funciones.MCPClients.Add;
    Servidor.Name := 'ladera';
    Servidor.TransportType := tpStdIo;
    Servidor.Params.Values['Command'] := ParamStr(0);
    Servidor.Params.Values['Arguments'] := '--servidor';
    Servidor.Enabled := True;
    if not (Servidor.MCPClient.Initialize and Servidor.MCPClient.Available) then
    begin
      Writeln('No se pudo arrancar el servidor MCP.');
      ExitCode := 2;
      Exit;
    end;

    Writeln('Servidor MCP "ladera" (stdio). Herramientas que publica:');
    Lista := Servidor.MCPClient.ListTools;
    try
      if Assigned(Lista) and Lista.TryGetValue<TJSONArray>('tools', Tools) then
        for T in Tools do
          Writeln('  - ', T.GetValue<string>('name'), ': ', T.GetValue<string>('description'));
    finally
      Lista.Free;
    end;
    Writeln;

    Chat.DriverName := 'OpenAi';
    Chat.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
    Chat.Params.Values['Asynchronous'] := 'False';
    Chat.Tool_Active := True;
    Chat.SystemPrompt.Text :=
      'Eres Lucía, la asistente de ventas de Café La Ladera. Para existencias y precios ' +
      'mayoristas usa siempre las herramientas de la bodega; no inventes cifras. Responde en ' +
      'español, breve (máximo 3 líneas) y en texto plano sin Markdown: la respuesta sale en una consola.';
    // Las herramientas del servidor llegan al modelo junto con las locales
    Chat.AiFunctions := Funciones;

    for Pregunta in PREGUNTAS do
    begin
      Writeln('Tú: ', Pregunta);
      R := Chat.AddMessageAndRun(Pregunta, 'user', []);
      if R = '' then
        R := '(sin respuesta) ' + Chat.LastError;
      Writeln('Lucía: ', R.Trim.Replace(sLineBreak, sLineBreak + '       '));
      Writeln;
    end;
  finally
    Chat.AiFunctions := nil;
    Chat.Free;
    Funciones.Free;   // cierra stdin del hijo: el servidor termina solo
    Trafico.Free;
  end;
end;

function HasFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
      Exit(True);
end;

var
  BufSalida: array [0 .. 0] of Byte;

begin
  FmtCOP := TFormatSettings.Create('es-CO');
  FmtCOP.ThousandSeparator := '.';
  try
    if HasFlag('--servidor') then
    begin
      {$IFDEF MSWINDOWS}
      SetTextCodePage(ErrOutput, CP_UTF8);   // stdout lo maneja el servidor
      {$ENDIF}
      CorrerServidor;
    end
    else
    begin
      {$IFDEF MSWINDOWS}
      SetConsoleOutputCP(CP_UTF8);
      SetTextCodePage(Output, CP_UTF8);
      {$ENDIF}
      // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes.
      // Con un buffer de 1 byte cada caracter sale al escribirlo.
      SetTextBuf(Output, BufSalida, 1);
      CorrerCliente;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
