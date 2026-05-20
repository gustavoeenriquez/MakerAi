program MultiAgent;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 08-Orquestacion / 04-MultiAgent
// =============================================================================
// Cinco agentes especializados realizan un debate estructurado.
// Cada agente recibe el historial completo del debate hasta ese momento
// gracias al callback OnEnterNode que inyecta el contexto acumulado.
//
// Conceptos que cubre:
//   - Manager.OnEnterNode: callback antes de ejecutar cada nodo
//   - Inyeccion de historial desde el blackboard
//   - Contexto acumulativo entre nodos
//   - Pipeline lineal de 5 LLMNodes especializados
//
// Pipeline: [Moderador] -> [Ponente] -> [Critico] -> [Replica] -> [Juez]
// =============================================================================

uses
  System.SysUtils,
  Winapi.Windows,
  uMakerAi.Agents,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

// =============================================================================
//  TContextoDebate — inyecta historial acumulado en cada nodo
// =============================================================================
type
  TContextoDebate = class
  private
    FManager   : TAIAgentManager;
    FOrdenNodos: TArray<String>;
  public
    constructor Create(AManager: TAIAgentManager);
    procedure InjectarHistorial(Sender: TObject; Node: TAIAgentsNode);
  end;

constructor TContextoDebate.Create(AManager: TAIAgentManager);
begin
  FManager    := AManager;
  FOrdenNodos := ['Moderador', 'Ponente', 'Critico', 'Replica', 'Juez'];
end;

procedure TContextoDebate.InjectarHistorial(Sender: TObject; Node: TAIAgentsNode);
var
  NombreNodo, PrevOut, Historial: String;
begin
  Historial := '';
  for NombreNodo in FOrdenNodos do
  begin
    if NombreNodo = Node.Name then Break;
    PrevOut := FManager.Blackboard.GetString(NombreNodo + '.output');
    if PrevOut <> '' then
      Historial := Historial + '=== ' + NombreNodo + ' ===' + #10 + PrevOut + #10#10;
  end;
  if Historial <> '' then
    Node.Input := '--- TRANSCRIPCION DEL DEBATE ---' + #10 + Historial +
                  '--- REALIZA TU ROL ASIGNADO AHORA ---';
end;

// =============================================================================
//  Helper
// =============================================================================
function MakeLLM(Manager: TAIAgentManager; const AName, APrompt: String): TLLMNode;
begin
  Result := TLLMNode.Create(Manager);
  Result.Name         := AName;
  Result.Graph        := Manager;
  Result.DriverName   := DRIVER;
  Result.Model        := MODEL;
  Result.ApiKey       := API_KEY;
  Result.UseAllTools  := False;
  Result.SystemPrompt := APrompt;
end;

procedure RunDebate(const ATema: String);
var
  Manager   : TAIAgentManager;
  Contexto  : TContextoDebate;
  Moderador : TLLMNode;
  Ponente   : TLLMNode;
  Critico   : TLLMNode;
  Replica   : TLLMNode;
  Juez      : TLLMNode;
  L1, L2, L3, L4: TAIAgentsLink;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tema del debate: ', ATema);
  Writeln(StringOfChar('=', 60));

  Manager  := TAIAgentManager.Create(nil);
  Contexto := TContextoDebate.Create(Manager);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 180000;
    Manager.OnEnterNode  := Contexto.InjectarHistorial;

    // Nodo 1: Moderador
    Moderador := MakeLLM(Manager, 'Moderador',
      'Eres el moderador del debate. Dado un tema, formula una proposicion ' +
      'clara (afirmacion) e introduce el debate en 2 oraciones. ' +
      'Formato: "PROPOSICION: [enunciado]\nINTRODUCCION: [contexto breve]"');

    // Nodo 2: Ponente (A FAVOR)
    Ponente := MakeLLM(Manager, 'Ponente',
      'Eres un debatiente experto A FAVOR de la proposicion. ' +
      'Argumenta con 2 razones solidas. Cada argumento en 2-3 oraciones. ' +
      'Formato: "ARGUMENTOS A FAVOR:\n1. [argumento]\n2. [argumento]"');

    // Nodo 3: Critico (EN CONTRA)
    Critico := MakeLLM(Manager, 'Critico',
      'Eres un debatiente experto EN CONTRA de la proposicion. ' +
      'Refuta cada argumento del ponente y agrega una objecion nueva. ' +
      'Formato: "ARGUMENTOS EN CONTRA:\n1. [refutacion 1]\n2. [refutacion 2]\n3. [nueva objecion]"');

    // Nodo 4: Replica
    Replica := MakeLLM(Manager, 'Replica',
      'Eres el ponente original. Responde a la objecion mas fuerte del critico. ' +
      '2-3 oraciones. Formato: "REPLICA: [tu respuesta]"');

    // Nodo 5: Juez
    Juez := MakeLLM(Manager, 'Juez',
      'Eres un juez imparcial. Evalua ambos lados del debate en: ' +
      'solidez de argumentos, evidencia y persuasion. ' +
      'Da puntuaciones sobre 10 y declara un ganador con justificacion. ' +
      'Formato: "PUNTAJES:\n- Lado A favor: X/10\n- Lado En contra: X/10\nVEREDICTO: [ganador y razon]"');

    // Encadenar linealmente
    L1 := TAIAgentsLink.Create(Manager); L1.Name := 'L1'; L1.Graph := Manager; L1.NextA := Ponente;   Moderador.Next := L1;
    L2 := TAIAgentsLink.Create(Manager); L2.Name := 'L2'; L2.Graph := Manager; L2.NextA := Critico;   Ponente.Next   := L2;
    L3 := TAIAgentsLink.Create(Manager); L3.Name := 'L3'; L3.Graph := Manager; L3.NextA := Replica;   Critico.Next   := L3;
    L4 := TAIAgentsLink.Create(Manager); L4.Name := 'L4'; L4.Graph := Manager; L4.NextA := Juez;      Replica.Next   := L4;

    Manager.StartNode := Moderador;
    Manager.EndNode   := Juez;
    Manager.Compile;

    Writeln;
    var Veredicto := Manager.Run(ATema);

    // Mostrar el debate completo
    Writeln;
    Writeln('--- Moderador ---');
    Writeln(Manager.Blackboard.GetString('Moderador.output'));
    Writeln;
    Writeln('--- Ponente ---');
    Writeln(Manager.Blackboard.GetString('Ponente.output'));
    Writeln;
    Writeln('--- Critico ---');
    Writeln(Manager.Blackboard.GetString('Critico.output'));
    Writeln;
    Writeln('--- Replica ---');
    Writeln(Manager.Blackboard.GetString('Replica.output'));
    Writeln;
    Writeln('--- Veredicto del Juez ---');
    Writeln(Veredicto);

  finally
    Contexto.Free;
    Manager.Free;
  end;
  Writeln;
end;

procedure RunDemo;
begin
  Writeln('=== MultiAgent ===');
  Writeln('5 agentes: Moderador -> Ponente -> Critico -> Replica -> Juez');
  Writeln;

  RunDebate('La inteligencia artificial eliminara mas empleos de los que creara');
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
