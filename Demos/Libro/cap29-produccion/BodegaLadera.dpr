program BodegaLadera;

// =============================================================================
// LIBRO "IA con Delphi", capitulos 16 y 25 - La bodega como servidor MCP y como agente A2A
// =============================================================================
// Capitulo 25: BodegaLadera.exe --a2a [--port 8301]
//   La bodega como AGENTE (protocolo A2A 1.0): un grafo de un nodo (Apartar) publicado con
//   TAiA2AServer. Su tarjeta esta en http://localhost:8301/.well-known/agent-card.json y
//   recibe tareas por JSON-RPC (SendMessage). A diferencia de las funciones MCP, el agente
//   tiene estado: lleva la cuenta de lo que ya aparto y numera las reservas. Lo usa
//   LaderaApp por medio de TAiA2AAgentTool (y lo arranca si no hay nadie en el puerto).
//
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
  System.SyncObjs,
  System.JSON,
  System.StrUtils,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.MCPServer.Core,
  UMakerAi.MCPServer.Stdio,
  uMakerAi.Agents,
  uMakerAi.A2A.Server,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;   // registra todos los drivers

type
  TCafe = record
    Nombre: string;
    Kilos: Integer;        // en bodega, a granel (mayorista)
    PrecioKilo: Integer;   // COP, precio mayorista base
    Bolsas: Integer;       // bolsas de 340 g empacadas (lo que aparta el agente)
  end;

const
  CAFES: array [0 .. 3] of TCafe = (
    (Nombre: 'Huila Reserva'; Kilos: 18; PrecioKilo: 98000; Bolsas: 24),
    (Nombre: 'Nariño Altura'; Kilos: 6; PrecioKilo: 108000; Bolsas: 10),
    (Nombre: 'Sierra Nevada Orgánico'; Kilos: 30; PrecioKilo: 92000; Bolsas: 0),
    (Nombre: 'Descafeinado Suave'; Kilos: 0; PrecioKilo: 88000; Bolsas: 15));

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

function IndiceCafe(const ANombre: string): Integer;
var
  I: Integer;
  Buscado: string;
begin
  Buscado := Normalizar(ANombre);
  for I := 0 to High(CAFES) do
    if (Buscado <> '') and (Normalizar(CAFES[I].Nombre).StartsWith(Buscado) or
      Buscado.StartsWith(Normalizar(CAFES[I].Nombre).Split([' '])[0])) then
      Exit(I);
  Result := -1;
end;

function BuscarCafe(const ANombre: string; out ACafe: TCafe): Boolean;
var
  I: Integer;
begin
  I := IndiceCafe(ANombre);
  Result := I >= 0;
  if Result then
    ACafe := CAFES[I];
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

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

// ---------------------------------------------------------------------------
// Modo agente (capitulo 25): la bodega publicada como agente A2A
// ---------------------------------------------------------------------------
type
  TBodegaAgente = class
  private
    FLock: TCriticalSection;
    FApartadas: array [0 .. 3] of Integer;   // bolsas ya apartadas por cafe: el agente tiene estado
    FSiguiente: Integer;                      // numero de la proxima reserva
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apartar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
  end;

constructor TBodegaAgente.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FSiguiente := 1001;
end;

destructor TBodegaAgente.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TBodegaAgente.Apartar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Pedido, Item, Nombre, Apartado, Agotado, Desconocido: string;
  Cant, I, Libres: Integer;
begin
  // Corre en un hilo del servidor A2A, un task por pedido. Entrada: "cantidad producto, ..."
  // (lo que siga a '|' se ignora: el flujo de pedidos manda tambien la ciudad).
  Pedido := Input.Split(['|'])[0].Trim;
  FLock.Enter;
  try
    for Item in Pedido.Split([',']) do
    begin
      if Item.Trim = '' then
        Continue;
      Cant := StrToIntDef(Item.Trim.Split([' '])[0], 0);
      Nombre := Item.Trim.Substring(Item.Trim.IndexOf(' ') + 1).Trim;
      if Cant <= 0 then
      begin
        Cant := 1;              // "Huila Reserva" a secas: una bolsa
        Nombre := Item.Trim;
      end;
      I := IndiceCafe(Nombre);
      if I < 0 then
        Desconocido := Desconocido + IfThen(Desconocido <> '', ', ') + Nombre
      else
      begin
        Libres := CAFES[I].Bolsas - FApartadas[I];
        if Libres >= Cant then
        begin
          Inc(FApartadas[I], Cant);
          Apartado := Apartado + IfThen(Apartado <> '', ', ') + Format('%d %s', [Cant, CAFES[I].Nombre]);
        end
        else
          Agotado := Agotado + IfThen(Agotado <> '', ', ') +
            Format('%s (quedan %d; próximo tueste el martes)', [CAFES[I].Nombre, Libres]);
      end;
    end;
    if Apartado <> '' then
    begin
      Output := Format('RESERVA R-%d: %s. Vigente hasta mañana a las 6 p. m.', [FSiguiente, Apartado]);
      Inc(FSiguiente);
    end
    else
      Output := 'SIN RESERVA.';
    if Agotado <> '' then
      Output := Output + ' AGOTADO: ' + Agotado + '.';
    if Desconocido <> '' then
      Output := Output + ' NO ES DE LA CARTA: ' + Desconocido + '. Cafés: ' + NombresCafes + '.';
  finally
    FLock.Leave;
  end;
  try
    Writeln('[A2A] ', Pedido, ' -> ', Output);
  except
    // sin consola (lo arranco la app): no importa
  end;
end;

procedure CorrerAgente(APuerto: Integer);
var
  Bodega: TBodegaAgente;
  Grafo: TAIAgentManager;
  Servidor: TAiA2AServer;
  {$IFDEF MSWINDOWS} Modo: DWORD; {$ENDIF}
begin
  Bodega := TBodegaAgente.Create;
  Grafo := TAIAgentManager.Create(nil);
  Servidor := TAiA2AServer.Create(nil);
  try
    // El agente es un grafo (aqui, de un solo nodo); el servidor lo publica con su tarjeta
    Grafo.Name := 'AgenteBodega';
    Grafo.AddNode('Apartar', Bodega.Apartar).SetEntryPoint('Apartar').SetFinishPoint('Apartar');
    Servidor.AgentManager := Grafo;
    Servidor.AgentName := 'Bodega de Café La Ladera';
    Servidor.AgentDescription := 'Verifica existencias de bolsas de 340 g y las aparta para un pedido';
    Servidor.AgentVersion := '1.0';
    Servidor.Skills.AddSkill('apartar', 'Apartar existencias',
      'Recibe "cantidad producto, cantidad producto" y responde el número de reserva y lo que está agotado',
      'bodega, existencias, reserva');
    Servidor.Port := APuerto;
    Servidor.Active := True;
    Writeln(Format('Agente de bodega A2A en http://localhost:%d  (tarjeta: /.well-known/agent-card.json)', [APuerto]));
    // Hasta que el usuario pulse Enter o la app que lo arranco lo termine
    {$IFDEF MSWINDOWS}
    if GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Modo) then
    begin
      Writeln('Enter para terminar.');
      Readln;
    end
    else
    {$ENDIF}
      while True do
        Sleep(1000);
    Servidor.Active := False;
  finally
    Servidor.Free;
    Grafo.Free;
    Bodega.Free;
  end;
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
    else if HasFlag('--a2a') then
    begin
      {$IFDEF MSWINDOWS}
      SetConsoleOutputCP(CP_UTF8);
      SetTextCodePage(Output, CP_UTF8);
      {$ENDIF}
      CorrerAgente(StrToIntDef(ArgValue('--port', '8301'), 8301));
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
