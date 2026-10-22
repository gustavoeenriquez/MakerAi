program AgenteLadera;

// =============================================================================
// DEMO 102 - El primer agente: TLLMNode
// =============================================================================
// Un chat responde; un agente actúa. TLLMNode es un nodo de agente con un
// modelo, unas instrucciones y herramientas: decide qué herramienta usar,
// lee el resultado y sigue hasta terminar la tarea (el ciclo ReAct lo maneja
// el function calling del proveedor).
//
//   Atencion (TLLMNode)  skill respuesta-reclamo del demo 101 + dos
//                        herramientas: consultar_pedido y programar_cambio
//        |
//   Revisor  (TLLMNode)  revisa la respuesta contra las reglas del skill
//
//   - Skill    : TAiSkill.FromFolder; su SKILL.md es la base del prompt del nodo.
//   - Registry : TAiToolRegistry propio, llenado desde un TAiFunctions
//                (RegisterFromTAiFunctions): las mismas funciones de un chat.
//   - Grafo    : TAIAgentManager con la API fluida; la salida de un nodo es la
//                entrada del siguiente.
//
// Requiere OPENAI_API_KEY. Empresa, clientes, pedidos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Agents,
  uMakerAi.Agents.Skill,
  uMakerAi.Agents.ToolRegistry,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations;   // drivers para los TLLMNode

const
  MENSAJE_CLIENTA =
    'Mensaje de la clienta Marta Gómez: "Hola, es el pedido 1047. Pedí el Huila Reserva en grano ' +
    'y me llegó molido. Lo necesito para el sábado."';

type
  // Las herramientas del agente y los eventos del grafo ('of object')
  TTienda = class
  public
    procedure ConsultarPedido(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure ProgramarCambio(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure EntraNodo(Sender: TObject; Node: TAIAgentsNode);
    procedure SaleNodo(Sender: TObject; Node: TAIAgentsNode);
  end;

procedure TTienda.ConsultarPedido(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Numero: string;
begin
  Handled := True;
  Numero := ToolCall.Params.Values['numero'].Trim;
  Writeln('   [herramienta] consultar_pedido ', Numero);
  if Numero = '1047' then
    ToolCall.Response := 'Pedido 1047 de Marta Gómez, Medellín: 2 bolsas de Huila Reserva (340 g). ' +
      'Molienda pedida: en grano. Despachado: molido para prensa francesa (error de empaque). ' +
      'Entregado el 20 de octubre.'
  else
    ToolCall.Response := 'No existe el pedido ' + Numero + '.';
end;

procedure TTienda.ProgramarCambio(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Numero: string;
begin
  Handled := True;
  Numero := ToolCall.Params.Values['numero'].Trim;
  Writeln('   [herramienta] programar_cambio ', Numero, ' (', ToolCall.Params.Values['motivo'].Trim, ')');
  if Numero = '1047' then
    ToolCall.Response := 'Cambio programado sin costo: 2 bolsas de Huila Reserva en grano. ' +
      'Guía LAD-55821, sale mañana jueves y llega a Medellín el viernes. ' +
      'La clienta entrega las bolsas molidas al mensajero.'
  else
    ToolCall.Response := 'No existe el pedido ' + Numero + '.';
end;

procedure TTienda.EntraNodo(Sender: TObject; Node: TAIAgentsNode);
begin
  Writeln('-> nodo ', Node.Name);
end;

// La salida de cada nodo, al terminar: es la entrada del siguiente
procedure TTienda.SaleNodo(Sender: TObject; Node: TAIAgentsNode);
begin
  Writeln('   ', Node.Name, ': ', AdjustLineBreaks(Node.Output.Trim).Replace(sLineBreak, sLineBreak + '   '));
  Writeln;
end;

function CrearFunciones(ATienda: TTienda): TAiFunctions;
var
  F: TFunctionActionItem;
  P: TFunctionParamsItem;
begin
  Result := TAiFunctions.Create(nil);

  F := Result.Functions.Add;
  F.FunctionName := 'consultar_pedido';
  F.Description.Text := 'Datos de un pedido de Café La Ladera: cliente, productos, molienda y entrega.';
  F.OnAction := ATienda.ConsultarPedido;
  P := F.Parameters.Add;
  P.Name := 'numero';
  P.ParamType := ptString;
  P.Description.Text := 'Número del pedido.';
  P.Required := True;

  F := Result.Functions.Add;
  F.FunctionName := 'programar_cambio';
  F.Description.Text := 'Programa el cambio de un pedido equivocado, sin costo para el cliente. ' +
    'Devuelve la guía y la fecha de entrega.';
  F.OnAction := ATienda.ProgramarCambio;
  P := F.Parameters.Add;
  P.Name := 'numero';
  P.ParamType := ptString;
  P.Description.Text := 'Número del pedido.';
  P.Required := True;
  P := F.Parameters.Add;
  P.Name := 'motivo';
  P.ParamType := ptString;
  P.Description.Text := 'Qué salió mal.';
  P.Required := True;
end;

// La carpeta de skills del demo 101 (el .exe queda en Win64\Release)
function CarpetaSkill(const ANombre: string): string;
var
  Dir: string;
  I: Integer;
begin
  Dir := ExtractFilePath(ParamStr(0));
  for I := 0 to 4 do
  begin
    Result := TPath.Combine(Dir, '101-SkillsLadera\skills\' + ANombre);
    if TDirectory.Exists(Result) then
      Exit;
    Dir := TPath.GetDirectoryName(ExcludeTrailingPathDelimiter(Dir));
  end;
  raise Exception.Create('No se encontró el skill ' + ANombre + ' del demo 101');
end;

function NuevoNodo(AGrafo: TAIAgentManager; const ANombre: string): TLLMNode;
begin
  Result := TLLMNode.Create(AGrafo);
  Result.Name := ANombre;
  Result.DriverName := 'OpenAi';
  Result.ApiKey := '@OPENAI_API_KEY';
end;

procedure Esperar(AGrafo: TAIAgentManager);
begin
  while AGrafo.Busy do
    CheckSynchronize(50);
  CheckSynchronize;
end;

var
  Tienda: TTienda;
  Funciones: TAiFunctions;
  Registro: TAiToolRegistry;
  Grafo: TAIAgentManager;
  Atencion, Revisor: TLLMNode;
var
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes.
  // Con un buffer de 1 byte cada caracter sale al escribirlo.
  SetTextBuf(Output, BufSalida, 1);
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('Falta la variable OPENAI_API_KEY.');
    ExitCode := 1;
    Exit;
  end;

  Tienda := TTienda.Create;
  Funciones := CrearFunciones(Tienda);
  Registro := TAiToolRegistry.Create;
  Grafo := TAIAgentManager.Create(nil);
  try
    try
      // Las herramientas del agente: las mismas funciones que usaría un chat
      Registro.RegisterFromTAiFunctions(Funciones);

      // El agente: modelo + skill + herramientas
      Atencion := NuevoNodo(Grafo, 'Atencion');
      Atencion.Skill := TAiSkill.FromFolder(CarpetaSkill('respuesta-reclamo'));   // el nodo lo libera
      Atencion.SystemPrompt :=
        'Antes de responder, consulta el pedido y, si fue un error de la tienda, programa el cambio. ' +
        'Usa los datos reales de las herramientas. Texto plano, sin Markdown.';
      Atencion.Registry := Registro;

      // El revisor: sin herramientas, solo revisa
      Revisor := NuevoNodo(Grafo, 'Revisor');
      Revisor.UseAllTools := False;
      Revisor.SystemPrompt :=
        'Revisas la respuesta que otro agente preparó para una clienta de Café La Ladera. ' +
        'Reglas: máximo 4 frases, sin emojis, firma "Lucía, Café La Ladera", y debe decir el número ' +
        'de guía y el día de entrega. Si cumple todo, escribe APROBADA en la primera línea y debajo la ' +
        'respuesta tal cual. Si no, escribe CORREGIDA y debajo la versión corregida. Texto plano.';

      Grafo.OnEnterNode := Tienda.EntraNodo;
      Grafo.OnExitNode := Tienda.SaleNodo;
      Grafo.AddNode(Atencion)
        .AddNode(Revisor)
        .AddEdge('Atencion', 'Revisor')
        .SetEntryPoint('Atencion')
        .SetFinishPoint('Revisor');

      Writeln(MENSAJE_CLIENTA);
      Writeln;
      Grafo.Run(MENSAJE_CLIENTA, nil);
      Esperar(Grafo);
      ExitCode := 0;
    except
      on E: Exception do
      begin
        Writeln('ERROR: ', E.ClassName, ': ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    Grafo.Free;
    Registro.Free;
    Funciones.Free;
    Tienda.Free;
  end;
end.
