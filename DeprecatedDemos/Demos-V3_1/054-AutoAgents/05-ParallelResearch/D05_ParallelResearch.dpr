program D05_ParallelResearch;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 05: ParallelResearch
// =============================================================================
// Patron Fork/Join con TLLMNodes:
//
//   [InputPrep] --ForkLink--> [ProsAnalyst]  --JoinLinkA--\
//                         \-> [ConsAnalyst]  --JoinLinkB--> [Synthesizer] (jmAll)
//
// Dos nodos analizan el mismo tema en paralelo (ventajas y desventajas).
// El nodo sintetizador espera ambos resultados (jmAll) y los integra.
// =============================================================================

uses
  System.SysUtils,
  System.Threading,
  uMakerAi.Agents,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

procedure RunParallelResearch(const ATopic: String);
var
  Manager    : TAIAgentManager;
  InputPrep  : TLLMNode;
  ProsNode   : TLLMNode;
  ConsNode   : TLLMNode;
  SynthNode  : TLLMNode;
  ForkLink   : TAIAgentsLink;
  JoinLinkA  : TAIAgentsLink;
  JoinLinkB  : TAIAgentsLink;
  FinalOut   : String;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tema: ', ATopic);
  Writeln(StringOfChar('=', 60));

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 120000;

    // --- Nodo de preparacion de input ---
    InputPrep := TLLMNode.Create(Manager);
    InputPrep.Name         := 'InputPrep';
    InputPrep.Graph        := Manager;
    InputPrep.DriverName   := DRIVER;
    InputPrep.Model        := MODEL;
    InputPrep.ApiKey       := API_KEY;
    InputPrep.UseAllTools  := False;
    InputPrep.SystemPrompt :=
      'You are a research coordinator. Given a topic, write a single clear ' +
      'research question about it. Output ONLY the question, nothing else.';

    // --- Nodo de analisis de ventajas (rama A) ---
    ProsNode := TLLMNode.Create(Manager);
    ProsNode.Name         := 'ProsAnalyst';
    ProsNode.Graph        := Manager;
    ProsNode.DriverName   := DRIVER;
    ProsNode.Model        := MODEL;
    ProsNode.ApiKey       := API_KEY;
    ProsNode.UseAllTools  := False;
    ProsNode.SystemPrompt :=
      'You are a PRO analyst. Your output must be STRICTLY formatted — no intro, no extra text.' + #13#10 +
      'Given a research question, respond with EXACTLY this structure:' + #13#10 +
      'PROS:' + #13#10 +
      '1. [one sentence advantage]' + #13#10 +
      '2. [one sentence advantage]' + #13#10 +
      '3. [one sentence advantage]' + #13#10 +
      'Nothing else. No explanations before or after.';

    // --- Nodo de analisis de desventajas (rama B) ---
    ConsNode := TLLMNode.Create(Manager);
    ConsNode.Name         := 'ConsAnalyst';
    ConsNode.Graph        := Manager;
    ConsNode.DriverName   := DRIVER;
    ConsNode.Model        := MODEL;
    ConsNode.ApiKey       := API_KEY;
    ConsNode.UseAllTools  := False;
    ConsNode.SystemPrompt :=
      'You are a CON analyst. Your output must be STRICTLY formatted — no intro, no extra text.' + #13#10 +
      'Given a research question, respond with EXACTLY this structure:' + #13#10 +
      'CONS:' + #13#10 +
      '1. [one sentence disadvantage]' + #13#10 +
      '2. [one sentence disadvantage]' + #13#10 +
      '3. [one sentence disadvantage]' + #13#10 +
      'Nothing else. No explanations before or after.';

    // --- Nodo sintetizador (espera ambas ramas: jmAll) ---
    SynthNode := TLLMNode.Create(Manager);
    SynthNode.Name         := 'Synthesizer';
    SynthNode.Graph        := Manager;
    SynthNode.DriverName   := DRIVER;
    SynthNode.Model        := MODEL;
    SynthNode.ApiKey       := API_KEY;
    SynthNode.UseAllTools  := False;
    SynthNode.JoinMode     := jmAll;   // espera ProsNode Y ConsNode
    SynthNode.SystemPrompt :=
      'You are a balanced analyst. You receive the combined output of two researchers: ' +
      'one listed PROS and another listed CONS about the same research question. ' +
      'Find the PROS: section and the CONS: section in the input. ' +
      'Synthesize them into a balanced verdict paragraph of 4-5 sentences. ' +
      'Start your response with "VERDICT:" and end with a clear recommendation.';

    // --- Fork: InputPrep -> (ProsNode, ConsNode) en paralelo ---
    ForkLink := TAIAgentsLink.Create(Manager);
    ForkLink.Name  := 'ForkLink';
    ForkLink.Graph := Manager;
    ForkLink.NextA := ProsNode;
    ForkLink.NextB := ConsNode;
    InputPrep.Next := ForkLink;

    // --- Join A: ProsNode -> Synthesizer ---
    JoinLinkA := TAIAgentsLink.Create(Manager);
    JoinLinkA.Name  := 'JoinLinkFromPros';
    JoinLinkA.Graph := Manager;
    JoinLinkA.NextA := SynthNode;
    ProsNode.Next   := JoinLinkA;

    // --- Join B: ConsNode -> Synthesizer ---
    JoinLinkB := TAIAgentsLink.Create(Manager);
    JoinLinkB.Name  := 'JoinLinkFromCons';
    JoinLinkB.Graph := Manager;
    JoinLinkB.NextA := SynthNode;
    ConsNode.Next   := JoinLinkB;

    // --- Configurar grafo ---
    Manager.StartNode := InputPrep;
    Manager.EndNode   := SynthNode;
    Manager.Compile;

    // --- Ejecutar ---
    Writeln;
    Writeln('Ejecutando investigacion en paralelo...');
    FinalOut := Manager.Run(ATopic);

    Writeln;
    Writeln('--- Pregunta de investigacion ---');
    Writeln(Manager.Blackboard.GetString('InputPrep.output'));
    Writeln;
    Writeln('--- Analisis PROS ---');
    Writeln(Manager.Blackboard.GetString('ProsAnalyst.output'));
    Writeln;
    Writeln('--- Analisis CONS ---');
    Writeln(Manager.Blackboard.GetString('ConsAnalyst.output'));
    Writeln;
    Writeln('--- Veredicto final (Synthesizer) ---');
    Writeln(FinalOut);

  finally
    Manager.Free;
  end;
  Writeln;
end;

procedure RunDemo;
begin
  Writeln('=== Demo 05 - ParallelResearch ===');
  Writeln('Patron: Fork/Join con dos ramas paralelas + sintetizador (jmAll)');
  Writeln;

  RunParallelResearch('Artificial Intelligence in education');
  Writeln('Pausa entre temas (15s)...');
  Sleep(15000);
  RunParallelResearch('Remote work for software developers');
end;

procedure PrintException(E: Exception; const APrefix: string = '');
var
  AggE: EAggregateException;
  I: Integer;
begin
  if E is EAggregateException then
  begin
    AggE := EAggregateException(E);
    Writeln(APrefix, 'AggregateException (', AggE.Count, ' inner):');
    for I := 0 to AggE.Count - 1 do
      Writeln(APrefix, '  [', I, '] ', AggE.InnerExceptions[I].ClassName,
              ': ', AggE.InnerExceptions[I].Message);
  end
  else
    Writeln(APrefix, E.ClassName, ': ', E.Message);
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
    begin
      Writeln('FATAL: ');
      PrintException(E, '  ');
    end;
  end;
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
