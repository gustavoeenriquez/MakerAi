program AgentCheckpoints;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 07-AgenticLLM / 04-Checkpoints
// =============================================================================
// Pipeline secuencial de 3 nodos con inspeccion del blackboard en cada paso.
// Demuestra como cada nodo escribe su output al blackboard y como leerlo
// para seguir el progreso del pipeline.
//
// Conceptos que cubre:
//   - Manager.Blackboard.GetString('NombreNodo.output')
//   - Pipeline de 3 TLLMNodes: Extractor -> Traductor -> Resumen
//   - Lectura de checkpoints intermedios tras Manager.Run()
//   - TAIAgentsLink encadenando nodos
//
// Estructura: [Extractor] -> [Translator] -> [Summarizer]
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
  Manager    : TAIAgentManager;
  Extractor  : TLLMNode;
  Translator : TLLMNode;
  Summarizer : TLLMNode;
  Link1, Link2: TAIAgentsLink;
  FinalOut   : String;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tema: ', ATopic);
  Writeln(StringOfChar('=', 60));

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 120000;

    // Nodo 1: Extractor de palabras clave
    Extractor := TLLMNode.Create(Manager);
    Extractor.Name         := 'Extractor';
    Extractor.Graph        := Manager;
    Extractor.DriverName   := DRIVER;
    Extractor.Model        := MODEL;
    Extractor.ApiKey       := API_KEY;
    Extractor.UseAllTools  := False;
    Extractor.SystemPrompt :=
      'You are a keyword extractor. Given a topic, list exactly 5 key terms ' +
      'in English. Output ONLY the 5 terms, one per line, no numbering.';

    // Nodo 2: Traductor (ingles -> espanol)
    Translator := TLLMNode.Create(Manager);
    Translator.Name         := 'Translator';
    Translator.Graph        := Manager;
    Translator.DriverName   := DRIVER;
    Translator.Model        := MODEL;
    Translator.ApiKey       := API_KEY;
    Translator.UseAllTools  := False;
    Translator.SystemPrompt :=
      'You are a translator from English to Spanish. ' +
      'Translate each keyword on its own line. ' +
      'Format: "English term → termino en espanol". One per line.';

    // Nodo 3: Generador de resumen
    Summarizer := TLLMNode.Create(Manager);
    Summarizer.Name         := 'Summarizer';
    Summarizer.Graph        := Manager;
    Summarizer.DriverName   := DRIVER;
    Summarizer.Model        := MODEL;
    Summarizer.ApiKey       := API_KEY;
    Summarizer.UseAllTools  := False;
    Summarizer.SystemPrompt :=
      'You are a Spanish writer. You receive a list of translated keywords. ' +
      'Write a short paragraph (3-4 sentences) in Spanish that integrates ' +
      'all those concepts naturally.';

    // Enlazar: Extractor -> Translator -> Summarizer
    Link1 := TAIAgentsLink.Create(Manager);
    Link1.Name  := 'L1'; Link1.Graph := Manager; Link1.NextA := Translator;
    Extractor.Next := Link1;

    Link2 := TAIAgentsLink.Create(Manager);
    Link2.Name  := 'L2'; Link2.Graph := Manager; Link2.NextA := Summarizer;
    Translator.Next := Link2;

    Manager.StartNode := Extractor;
    Manager.EndNode   := Summarizer;
    Manager.Compile;

    // Ejecutar el pipeline completo
    Writeln;
    Writeln('Ejecutando pipeline...');
    FinalOut := Manager.Run(ATopic);

    // Inspeccionar checkpoints del blackboard
    Writeln;
    Writeln(StringOfChar('-', 40));
    Writeln('CHECKPOINT 1 — Extractor.output:');
    Writeln(Manager.Blackboard.GetString('Extractor.output'));
    Writeln;
    Writeln('CHECKPOINT 2 — Translator.output:');
    Writeln(Manager.Blackboard.GetString('Translator.output'));
    Writeln;
    Writeln('CHECKPOINT 3 — Summarizer.output (salida final):');
    Writeln(FinalOut);

  finally
    Manager.Free;
  end;
  Writeln;
end;

procedure RunDemo;
begin
  Writeln('=== AgentCheckpoints ===');
  Writeln('Pipeline: [Extractor] -> [Translator] -> [Summarizer]');
  Writeln('Blackboard checkpoints visibles despues de cada nodo.');
  Writeln;

  RunPipeline('Artificial Intelligence');
  RunPipeline('Quantum Computing');
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
