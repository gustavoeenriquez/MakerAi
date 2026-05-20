program D04_SequentialPipeline;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 04: SequentialPipeline
// =============================================================================
// Tres TLLMNodes encadenados linealmente:
//   [Extractor] -> [Expander] -> [Reporter]
// La salida de cada nodo es la entrada del siguiente.
// Demuestra composicion de agentes especializados en pipeline.
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

procedure RunPipeline(const ATopic: String);
var
  Manager  : TAIAgentManager;
  Extractor: TLLMNode;
  Expander : TLLMNode;
  Reporter : TLLMNode;
  Link1    : TAIAgentsLink;
  Link2    : TAIAgentsLink;
  FinalOut : String;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tema: ', ATopic);
  Writeln(StringOfChar('=', 60));

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 120000;

    // --- Nodo 1: Extractor de conceptos clave ---
    Extractor := TLLMNode.Create(Manager);
    Extractor.Name         := 'Extractor';
    Extractor.Graph        := Manager;
    Extractor.DriverName   := DRIVER;
    Extractor.Model        := MODEL;
    Extractor.ApiKey       := API_KEY;
    Extractor.UseAllTools  := False;
    Extractor.SystemPrompt :=
      'You are a concept extractor. Given a topic, list exactly 5 key concepts ' +
      'related to it. Output ONLY the 5 concepts, one per line, no numbering, no extra text.';

    // --- Nodo 2: Expander de conceptos ---
    Expander := TLLMNode.Create(Manager);
    Expander.Name         := 'Expander';
    Expander.Graph        := Manager;
    Expander.DriverName   := DRIVER;
    Expander.Model        := MODEL;
    Expander.ApiKey       := API_KEY;
    Expander.UseAllTools  := False;
    Expander.SystemPrompt :=
      'You are a concept explainer. You receive a list of concepts (one per line). ' +
      'For each concept write a single clear sentence definition. ' +
      'Format: "ConceptName: definition sentence."';

    // --- Nodo 3: Generador de reporte ---
    Reporter := TLLMNode.Create(Manager);
    Reporter.Name         := 'Reporter';
    Reporter.Graph        := Manager;
    Reporter.DriverName   := DRIVER;
    Reporter.Model        := MODEL;
    Reporter.ApiKey       := API_KEY;
    Reporter.UseAllTools  := False;
    Reporter.SystemPrompt :=
      'You are a report writer. You receive a set of concept definitions. ' +
      'Write a cohesive 3-paragraph summary that integrates all concepts naturally. ' +
      'The summary should read as a mini-article about the original topic.';

    // --- Enlazar nodos: Extractor -> Expander -> Reporter ---
    Link1 := TAIAgentsLink.Create(Manager);
    Link1.Name  := 'Link_Extract_Expand';
    Link1.Graph := Manager;
    Link1.NextA := Expander;
    Extractor.Next := Link1;

    Link2 := TAIAgentsLink.Create(Manager);
    Link2.Name  := 'Link_Expand_Report';
    Link2.Graph := Manager;
    Link2.NextA := Reporter;
    Expander.Next := Link2;

    // --- Configurar grafo ---
    Manager.StartNode := Extractor;
    Manager.EndNode   := Reporter;
    Manager.Compile;

    // --- Ejecutar ---
    Writeln;
    Writeln('[Extractor] Extrayendo conceptos...');
    FinalOut := Manager.Run(ATopic);

    // Mostrar salidas intermedias via blackboard
    Writeln;
    Writeln('--- Conceptos extraidos (Extractor.output) ---');
    Writeln(Manager.Blackboard.GetString('Extractor.output'));
    Writeln;
    Writeln('--- Conceptos expandidos (Expander.output) ---');
    Writeln(Manager.Blackboard.GetString('Expander.output'));
    Writeln;
    Writeln('--- Reporte final (Reporter.output) ---');
    Writeln(FinalOut);

  finally
    Manager.Free;
  end;
  Writeln;
end;

procedure RunDemo;
begin
  Writeln('=== Demo 04 - SequentialPipeline ===');
  Writeln('Patron: [Extractor] -> [Expander] -> [Reporter]');
  Writeln;

  RunPipeline('Machine Learning');
  RunPipeline('Blockchain technology');
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
