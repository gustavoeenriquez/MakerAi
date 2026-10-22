program AgentGraphLadera;

// =============================================================================
// DEMO 098 - Orquestacion: el pedido de Cafe La Ladera como grafo de agentes
// =============================================================================
// Un pedido pasa por varios pasos. Unos pueden ir en paralelo, otros deciden
// el camino. Todo se arma con la API fluida de TAIAgentManager:
//
//   Recibir --+--> Inventario --+
//             +--> Cotizar -----+--> Consolidar (jmAll: espera las 3 ramas)
//             +--> Envio -------+         |
//                                         | lmExpression: 'faltantes > 0'
//                         +---------------+---------------+
//                         v                               v (fallback, NextNo)
//                      Avisar                         Despachar
//                         |                    lmConditional (next_route)
//                         |                  'local' /            \ 'nacional'
//                         |               Mensajero          Transportadora
//                         +-----------------+----------------------+
//                                           v
//                                 Redactor (TLLMNode, jmAny)
//
//   - Fanout  : varios AddEdge desde el mismo nodo = ramas en paralelo.
//   - Join    : SetJoinMode(jmAll) espera todas las ramas.
//   - Blackboard: memoria compartida del grafo (faltantes, total, zona...).
//   - Expresion: AddExpressionEdge evalua contra el blackboard;
//                AddFallbackEdge es el camino si ninguna se cumple.
//   - Condicional: AddConditionalEdge elige por el valor de 'next_route'.
//   - TLLMNode: el ultimo nodo es un LLM que redacta el mensaje al cliente
//               (sin OPENAI_API_KEY, o con --sin-llm, lo redacta el codigo).
//
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.StrUtils,
  System.Generics.Collections,
  uMakerAi.Agents,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations;   // drivers para el TLLMNode

type
  TProducto = record
    Nombre: string;
    Precio, Existencias: Integer;
  end;

const
  CATALOGO: array [0 .. 3] of TProducto = (
    (Nombre: 'Huila Reserva'; Precio: 38000; Existencias: 24),
    (Nombre: 'Nariño Altura'; Precio: 42000; Existencias: 10),
    (Nombre: 'Sierra Nevada Orgánico'; Precio: 36000; Existencias: 0),
    (Nombre: 'Descafeinado Suave'; Precio: 34000; Existencias: 15));

  // "cantidad producto, cantidad producto | ciudad"
  PEDIDOS: array [0 .. 2] of string = (
    '2 Huila Reserva, 1 Nariño Altura | Pitalito',
    '3 Descafeinado Suave | Bogotá',
    '1 Sierra Nevada Orgánico, 1 Huila Reserva | Medellín');

  CIUDAD_LOCAL = 'Pitalito';

type
  TPedido = class
  private
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const S: string);
    // Nodos del grafo
    procedure Recibir(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Inventario(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Cotizar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Envio(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Consolidar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Avisar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Despachar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Mensajero(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Transportadora(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure RedactarSinLLM(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
  end;

constructor TPedido.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
end;

destructor TPedido.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TPedido.Log(const S: string);
begin
  // Las ramas corren en hilos del pool: la consola va con candado
  FLock.Enter;
  try
    Writeln(S);
  finally
    FLock.Leave;
  end;
end;

function BuscarProducto(const ANombre: string; out P: TProducto): Boolean;
var
  X: TProducto;
begin
  for X in CATALOGO do
    if SameText(X.Nombre, ANombre) then
    begin
      P := X;
      Exit(True);
    end;
  Result := False;
end;

function Pesos(AValor: Integer): string;
begin
  Result := FormatFloat('#,##0', AValor).Replace(',', '.') + ' COP';
end;

procedure TPedido.Recibir(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Partes: TArray<string>;
begin
  // Separa productos y ciudad, y los deja en el blackboard para todas las ramas
  Partes := Input.Split(['|']);
  Node.Graph.Blackboard.SetString('items', Partes[0].Trim);
  Node.Graph.Blackboard.SetString('ciudad', Partes[1].Trim);
  Log('  [Recibir] productos: ' + Partes[0].Trim + '  ciudad: ' + Partes[1].Trim);
  Output := Input;
end;

procedure TPedido.Inventario(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Item, Nombre, Faltan: string;
  Cant, Faltantes: Integer;
  P: TProducto;
begin
  Faltantes := 0;
  Faltan := '';
  for Item in Node.Graph.Blackboard.GetString('items').Split([',']) do
  begin
    Cant := StrToIntDef(Item.Trim.Split([' '])[0], 0);
    Nombre := Item.Trim.Substring(Item.Trim.IndexOf(' ') + 1);
    if not BuscarProducto(Nombre, P) or (P.Existencias < Cant) then
    begin
      Inc(Faltantes);
      Faltan := Faltan + IfThen(Faltan <> '', ', ') + Nombre;
    end;
  end;
  Node.Graph.Blackboard.SetInteger('faltantes', Faltantes);
  Node.Graph.Blackboard.SetString('agotados', Faltan);
  Log(Format('  [Inventario] faltantes: %d %s', [Faltantes, Faltan]));
  Output := 'faltantes=' + IntToStr(Faltantes);
end;

procedure TPedido.Cotizar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Item, Nombre: string;
  Subtotal: Integer;
  P: TProducto;
begin
  Subtotal := 0;
  for Item in Node.Graph.Blackboard.GetString('items').Split([',']) do
  begin
    Nombre := Item.Trim.Substring(Item.Trim.IndexOf(' ') + 1);
    if BuscarProducto(Nombre, P) then
      Inc(Subtotal, StrToIntDef(Item.Trim.Split([' '])[0], 0) * P.Precio);
  end;
  Node.Graph.Blackboard.SetInteger('subtotal', Subtotal);
  Log('  [Cotizar] subtotal: ' + Pesos(Subtotal));
  Output := 'subtotal=' + IntToStr(Subtotal);
end;

procedure TPedido.Envio(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Costo: Integer;
begin
  if SameText(Node.Graph.Blackboard.GetString('ciudad'), CIUDAD_LOCAL) then
    Costo := 5000
  else
    Costo := 15000;
  Node.Graph.Blackboard.SetInteger('envio', Costo);
  Log('  [Envio] costo: ' + Pesos(Costo));
  Output := 'envio=' + IntToStr(Costo);
end;

procedure TPedido.Consolidar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  BB: TAIBlackboard;
  Total: Integer;
begin
  // jmAll: llega aqui una sola vez, cuando las tres ramas terminaron
  BB := Node.Graph.Blackboard;
  Total := BB.GetInteger('subtotal') + BB.GetInteger('envio');
  BB.SetInteger('total', Total);
  Log(Format('  [Consolidar] total %s, faltantes %d', [Pesos(Total), BB.GetInteger('faltantes')]));
  Output := Input;
end;

procedure TPedido.Avisar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Log('  [Avisar] no se despacha: falta ' + Node.Graph.Blackboard.GetString('agotados'));
  Output := Format('Pedido en espera. Productos agotados: %s. Pedido: %s. Ciudad: %s.',
    [Node.Graph.Blackboard.GetString('agotados'), Node.Graph.Blackboard.GetString('items'),
     Node.Graph.Blackboard.GetString('ciudad')]);
end;

procedure TPedido.Despachar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Zona: string;
begin
  // El link condicional de este nodo lee 'next_route' (su ConditionalKey por defecto)
  if SameText(Node.Graph.Blackboard.GetString('ciudad'), CIUDAD_LOCAL) then
    Zona := 'local'
  else
    Zona := 'nacional';
  Node.Graph.Blackboard.SetString('next_route', Zona);
  Log('  [Despachar] zona: ' + Zona);
  Output := Input;
end;

procedure TPedido.Mensajero(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Log('  [Mensajero] entrega hoy en ' + Node.Graph.Blackboard.GetString('ciudad'));
  Output := Format('Pedido confirmado: %s. Total %s con envío. Entrega hoy con mensajero en %s.',
    [Node.Graph.Blackboard.GetString('items'), Pesos(Node.Graph.Blackboard.GetInteger('total')),
     Node.Graph.Blackboard.GetString('ciudad')]);
end;

procedure TPedido.Transportadora(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string;
  var Output: string);
begin
  Log('  [Transportadora] guía nacional a ' + Node.Graph.Blackboard.GetString('ciudad'));
  Output := Format('Pedido confirmado: %s. Total %s con envío. Sale por transportadora a %s, llega en 2 a 3 días.',
    [Node.Graph.Blackboard.GetString('items'), Pesos(Node.Graph.Blackboard.GetInteger('total')),
     Node.Graph.Blackboard.GetString('ciudad')]);
end;

procedure TPedido.RedactarSinLLM(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string;
  var Output: string);
begin
  Output := 'Hola, te escribimos de Café La Ladera. ' + Input;
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

procedure Esperar(AGrafo: TAIAgentManager);
begin
  // Las ramas corren en el pool de hilos; la consola atiende lo sincronizado
  while AGrafo.Busy do
    CheckSynchronize(50);
  CheckSynchronize;
end;

var
  Pedido: TPedido;
  Grafo: TAIAgentManager;
  Redactor: TLLMNode;
  Rutas: TDictionary<string, string>;
  Texto, Resultado: string;
  ConLLM: Boolean;
begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  ConLLM := not HasFlag('--sin-llm') and (GetEnvironmentVariable('OPENAI_API_KEY') <> '');
  Pedido := TPedido.Create;
  Grafo := TAIAgentManager.Create(nil);
  Rutas := TDictionary<string, string>.Create;
  try
    try
      Grafo.Name := 'PedidoLadera';

      // --- Nodos ---------------------------------------------------------------
      Grafo.AddNode('Recibir', Pedido.Recibir)
        .AddNode('Inventario', Pedido.Inventario)
        .AddNode('Cotizar', Pedido.Cotizar)
        .AddNode('Envio', Pedido.Envio)
        .AddNode('Consolidar', Pedido.Consolidar)
        .AddNode('Avisar', Pedido.Avisar)
        .AddNode('Despachar', Pedido.Despachar)
        .AddNode('Mensajero', Pedido.Mensajero)
        .AddNode('Transportadora', Pedido.Transportadora);

      // El ultimo nodo: un LLM que redacta el mensaje al cliente
      if ConLLM then
      begin
        Redactor := TLLMNode.Create(Grafo);
        Redactor.Name := 'Redactor';
        Redactor.DriverName := 'OpenAi';
        Redactor.ApiKey := '@OPENAI_API_KEY';
        Redactor.SystemPrompt :=
          'Eres Lucía, de Café La Ladera. Convierte el estado del pedido en un mensaje de WhatsApp ' +
          'para el cliente: cálido, en español, máximo 3 líneas, sin inventar datos.';
        Grafo.AddNode(Redactor);
      end
      else
        Grafo.AddNode('Redactor', Pedido.RedactarSinLLM);

      // --- Aristas -------------------------------------------------------------
      // Fanout: tres AddEdge desde Recibir = tres ramas en paralelo
      Grafo.AddEdge('Recibir', 'Inventario')
        .AddEdge('Recibir', 'Cotizar')
        .AddEdge('Recibir', 'Envio')
        // Join: las tres ramas llegan a Consolidar, que espera a todas
        .AddEdge('Inventario', 'Consolidar')
        .AddEdge('Cotizar', 'Consolidar')
        .AddEdge('Envio', 'Consolidar')
        .SetJoinMode('Consolidar', jmAll)
        // Expresion contra el blackboard, con camino por defecto
        .AddExpressionEdge('Consolidar', 'Avisar', 'faltantes > 0')
        .AddFallbackEdge('Consolidar', 'Despachar');

      // Condicional: el valor de 'next_route' elige la rama
      Rutas.Add('local', 'Mensajero');
      Rutas.Add('nacional', 'Transportadora');
      Grafo.AddConditionalEdge('Despachar', 'PorZona', Rutas);

      // Todos los finales llegan al Redactor; corre con la primera llegada
      Grafo.AddEdge('Avisar', 'Redactor')
        .AddEdge('Mensajero', 'Redactor')
        .AddEdge('Transportadora', 'Redactor')
        .SetJoinMode('Redactor', jmAny)
        .SetEntryPoint('Recibir')
        .SetFinishPoint('Redactor');

      Writeln('Redactor: ', IfThen(ConLLM, 'TLLMNode (OpenAI)', 'código (sin LLM)'));
      Writeln;

      for Texto in PEDIDOS do
      begin
        Writeln('==== Pedido: ', Texto, ' ', StringOfChar('=', 20));
        // Corrida limpia: blackboard y estado de nodos en cero para cada pedido
        Grafo.Run(Texto, nil);
        Esperar(Grafo);
        Resultado := Grafo.FindNode('Redactor').Output;
        Writeln('  >> ', AdjustLineBreaks(Resultado.Trim).Replace(sLineBreak, sLineBreak + '     '));
        Writeln;
      end;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    Rutas.Free;
    Grafo.Free;
    Pedido.Free;
  end;
end.
