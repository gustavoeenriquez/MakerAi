program AgentDebate;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — 07-AgenticLLM / 10-MultiAgentDebate
// =============================================================================
// Cinco TLLMNodes simulan un debate estructurado:
//
//   [Moderator] -> [Proposer] -> [Critic] -> [Rebuttal] -> [Judge]
//
// Cada nodo construye sobre el output del anterior.
// TDebateContext inyecta el historial completo del debate antes de cada nodo
// via Manager.OnEnterNode, dando a cada participante todo el contexto previo.
//
// Conceptos que cubre:
//   - Manager.OnEnterNode: callback que se dispara antes de cada nodo
//   - Modificacion de Node.Input desde OnEnterNode (inyeccion de contexto)
//   - Manager.Blackboard.GetString(NodeName + '.output') para leer historial
//   - Pipeline lineal de 5 nodos LLM con roles diferenciados
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Agents,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

// -----------------------------------------------------------------------------
// TDebateContext — inyecta el historial acumulado del debate en cada nodo
// -----------------------------------------------------------------------------
type
  TDebateContext = class
  private
    FManager   : TAIAgentManager;
    FNodeOrder : TArray<String>;
  public
    constructor Create(AManager: TAIAgentManager);
    procedure InjectHistory(Sender: TObject; Node: TAIAgentsNode);
  end;

constructor TDebateContext.Create(AManager: TAIAgentManager);
begin
  FManager   := AManager;
  FNodeOrder := ['Moderator', 'Proposer', 'Critic', 'Rebuttal', 'Judge'];
end;

procedure TDebateContext.InjectHistory(Sender: TObject; Node: TAIAgentsNode);
var
  NodeName, PrevOutput, History: String;
begin
  History := '';
  for NodeName in FNodeOrder do
  begin
    if NodeName = Node.Name then Break;
    PrevOutput := FManager.Blackboard.GetString(NodeName + '.output');
    if PrevOutput <> '' then
      History := History + '=== ' + NodeName + ' ===' + #10 + PrevOutput + #10#10;
  end;
  // Reemplazar el input: transcript acumulado + directiva de rol
  if History <> '' then
    Node.Input := '--- FULL DEBATE TRANSCRIPT ---' + #10 + History +
                  '--- PERFORM YOUR ASSIGNED ROLE NOW ---';
end;

// -----------------------------------------------------------------------------

function MakeLLMNode(Manager: TAIAgentManager; const AName, APrompt: String): TLLMNode;
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

procedure RunDebate(const ATopic: String);
var
  Manager   : TAIAgentManager;
  DebateCtx : TDebateContext;
  Moderator : TLLMNode;
  Proposer  : TLLMNode;
  Critic    : TLLMNode;
  Rebuttal  : TLLMNode;
  Judge     : TLLMNode;
  L1, L2, L3, L4: TAIAgentsLink;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tema del debate: ', ATopic);
  Writeln(StringOfChar('=', 60));

  Manager   := TAIAgentManager.Create(nil);
  DebateCtx := TDebateContext.Create(Manager);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 180000;
    Manager.OnEnterNode  := DebateCtx.InjectHistory;

    Moderator := MakeLLMNode(Manager, 'Moderator',
      'You are a debate moderator. Given a debate topic, formulate a clear proposition ' +
      '(affirmative statement) and introduce the debate in 2 sentences. ' +
      'Format: "PROPOSITION: [statement]\nINTRODUCTION: [context]"');

    Proposer := MakeLLMNode(Manager, 'Proposer',
      'You are an expert debater arguing FOR the proposition. ' +
      'Give 2 strong arguments supporting it. Keep each argument to 2-3 sentences. ' +
      'Format: "ARGUMENT FOR:\n1. [first argument]\n2. [second argument]"');

    Critic := MakeLLMNode(Manager, 'Critic',
      'You are an expert debater arguing AGAINST the proposition. ' +
      'Counter each pro-argument and add a new objection. ' +
      'Format: "ARGUMENT AGAINST:\n1. [counter to arg 1]\n2. [counter to arg 2]\n3. [new objection]"');

    Rebuttal := MakeLLMNode(Manager, 'Rebuttal',
      'You are the original proposer. Address the opponent''s strongest objection in 2-3 sentences. ' +
      'Format: "REBUTTAL: [your response]"');

    Judge := MakeLLMNode(Manager, 'Judge',
      'You are an impartial judge. Evaluate both sides on: argument strength, evidence, persuasiveness. ' +
      'Give scores out of 10 and declare a winner. ' +
      'Format: "SCORES:\n- Pro side: X/10\n- Con side: X/10\nVERDICT: [winner and why]"');

    // Cadena lineal: Moderator -> Proposer -> Critic -> Rebuttal -> Judge
    L1 := TAIAgentsLink.Create(Manager); L1.Name := 'L1'; L1.Graph := Manager; L1.NextA := Proposer; Moderator.Next := L1;
    L2 := TAIAgentsLink.Create(Manager); L2.Name := 'L2'; L2.Graph := Manager; L2.NextA := Critic;   Proposer.Next  := L2;
    L3 := TAIAgentsLink.Create(Manager); L3.Name := 'L3'; L3.Graph := Manager; L3.NextA := Rebuttal; Critic.Next    := L3;
    L4 := TAIAgentsLink.Create(Manager); L4.Name := 'L4'; L4.Graph := Manager; L4.NextA := Judge;    Rebuttal.Next  := L4;

    Manager.StartNode := Moderator;
    Manager.EndNode   := Judge;
    Manager.Compile;

    Writeln;
    var Verdict := Manager.Run(ATopic);

    Writeln;
    Writeln('--- Moderador ---');
    Writeln(Manager.Blackboard.GetString('Moderator.output'));
    Writeln;
    Writeln('--- Proponente ---');
    Writeln(Manager.Blackboard.GetString('Proposer.output'));
    Writeln;
    Writeln('--- Critico ---');
    Writeln(Manager.Blackboard.GetString('Critic.output'));
    Writeln;
    Writeln('--- Replica ---');
    Writeln(Manager.Blackboard.GetString('Rebuttal.output'));
    Writeln;
    Writeln('--- Veredicto del Juez ---');
    Writeln(Verdict);

  finally
    DebateCtx.Free;
    Manager.Free;
  end;
  Writeln;
end;

begin
  Writeln('=== AgentDebate ===');
  Writeln('5 nodos: Moderator -> Proposer -> Critic -> Rebuttal -> Judge');
  Writeln('OnEnterNode inyecta historial completo del debate en cada nodo.');
  Writeln;
  try
    RunDebate('Artificial intelligence will eliminate more jobs than it creates');
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
