program ReActBasic;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 07-AgenticLLM / 01-ReAct-Basic
// =============================================================================
// El patrón ReAct (Reasoning + Acting) es la base de todos los agentes LLM.
// En MakerAI se implementa con TAIAgentManager + TLLMNode.
//
// Conceptos que cubre:
//   - TAIAgentManager: orquestador del grafo de agentes
//   - TLLMNode: nodo que contiene el loop ReAct con el proveedor LLM
//   - TAIAgentsNode: nodo genérico (pasante o con OnExecute)
//   - TAIAgentsLink: arista que conecta nodos
//   - Manager.StartNode / EndNode / Compile / Run
//   - Manager.Blackboard: estado compartido entre nodos
//   - Manager.Asynchronous=False: ejecución síncrona
//
// Estructura del grafo:
//   [Start] --> [LLMAgent] --> [End]
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

// =============================================================================
//  Ejecuta una pregunta y muestra la respuesta
// =============================================================================
procedure Preguntar(Manager: TAIAgentManager; const Pregunta: String);
begin
  Manager.Blackboard.Clear;
  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', Pregunta);
  try
    Writeln('Respuesta: ', Manager.Run(Pregunta));
  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;
  Writeln;
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  Manager  : TAIAgentManager;
  Start    : TAIAgentsNode;
  LLMAgent : TLLMNode;
  Finish   : TAIAgentsNode;
begin
  Writeln('=== ReActBasic ===');
  Writeln('Driver: ', DRIVER, ' / ', MODEL);
  Writeln;

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 60000;

    // Nodo de entrada (pasante: propaga Input -> Output)
    Start       := TAIAgentsNode.Create(Manager);
    Start.Name  := 'Start';
    Start.Graph := Manager;

    // Nodo LLM: el agente ejecuta el ciclo ReAct
    LLMAgent              := TLLMNode.Create(Manager);
    LLMAgent.Name         := 'LLMAgent';
    LLMAgent.Graph        := Manager;
    LLMAgent.DriverName   := DRIVER;
    LLMAgent.Model        := MODEL;
    LLMAgent.ApiKey       := API_KEY;
    LLMAgent.UseAllTools  := False;
    LLMAgent.SystemPrompt :=
      'Eres un asistente conciso que responde en espanol. ' +
      'Responde siempre en una o dos frases directas y al grano.';

    // Nodo de salida (pasante)
    Finish       := TAIAgentsNode.Create(Manager);
    Finish.Name  := 'End';
    Finish.Graph := Manager;

    // Conectar: Start -> LLMAgent -> End
    Manager.AddEdge('Start',    'LLMAgent');
    Manager.AddEdge('LLMAgent', 'End');

    Manager.StartNode := Start;
    Manager.EndNode   := Finish;
    Manager.Compile;

    Writeln('Grafo compilado. Iniciando preguntas...');
    Writeln;

    Preguntar(Manager, 'Cual es la capital de Mexico?');
    Preguntar(Manager, 'Quien escribio Cien anos de soledad?');
    Preguntar(Manager, 'Cuanto es 2 elevado a la decima potencia?');
    Preguntar(Manager, 'Explica en una frase que es un algoritmo.');
    Preguntar(Manager, 'Cual es el lenguaje de programacion mas antiguo aun en uso?');

    // Mostrar estado del blackboard tras la última ejecución
    Writeln(StringOfChar('=', 60));
    Writeln('Blackboard tras ultima ejecucion:');
    Writeln('  LLMAgent.output = "',
      Copy(Manager.Blackboard.GetString('LLMAgent.output'), 1, 80), '..."');

  finally
    Manager.Free;
  end;
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
