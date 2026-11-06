program D01_HelloLLM;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 01: HelloLLM
// =============================================================================
// Grafo minimo de 3 nodos: StartNode -> TLLMNode -> EndNode.
// Demuestra la estructura basica de un grafo de agentes con un nodo LLM
// central sin herramientas.
//
// Estructura:
//   [Start] --(lmFanout)--> [LLMNode] --(lmFanout)--> [End]
//
// Start y End son nodos pasantes (sin OnExecute): propagan Input -> Output.
// LLMNode contiene el loop ReAct con el proveedor LLM configurado.
// =============================================================================

uses
  System.SysUtils,
  System.Generics.Collections,
  uMakerAi.Agents,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

procedure RunDemo;
var
  Manager  : TAIAgentManager;
  StartNode: TAIAgentsNode;
  LLMNode  : TLLMNode;
  EndNode  : TAIAgentsNode;
  Questions: array[0..2] of string;
  I: Integer;
begin
  Writeln('=== Demo 01 — HelloLLM ===');
  Writeln('Driver : ', DRIVER);
  Writeln('Model  : ', MODEL);
  Writeln;

  Questions[0] := 'Cual es la capital de Francia?';
  Questions[1] := 'Quien escribio Don Quijote de la Mancha?';
  Questions[2] := 'Explica en una frase que es la fotosintesis.';

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 30000;

    // Nodo de entrada: pasante (sin OnExecute propaga Input -> Output)
    StartNode       := TAIAgentsNode.Create(Manager);
    StartNode.Name  := 'Start';
    StartNode.Graph := Manager;

    // Nodo LLM central: ejecuta el loop ReAct con el proveedor configurado
    LLMNode              := TLLMNode.Create(Manager);
    LLMNode.Name         := 'LLMNode';
    LLMNode.Graph        := Manager;
    LLMNode.DriverName   := DRIVER;
    LLMNode.Model        := MODEL;
    LLMNode.ApiKey       := API_KEY;
    LLMNode.UseAllTools  := False;
    LLMNode.SystemPrompt :=
      'Eres un asistente conciso. Responde siempre en una sola frase corta y directa.';

    // Nodo de salida: pasante (recibe Output del LLMNode)
    EndNode       := TAIAgentsNode.Create(Manager);
    EndNode.Name  := 'End';
    EndNode.Graph := Manager;

    // Conectar: Start -> LLMNode -> End
    Manager.AddEdge('Start', 'LLMNode');
    Manager.AddEdge('LLMNode', 'End');

    Manager.StartNode := StartNode;
    Manager.EndNode   := EndNode;
    Manager.Compile;

    // Ejecutar varias preguntas independientes
    for I := 0 to High(Questions) do
    begin
      // Limpiar estado del blackboard entre preguntas (libera AskMsg/ResMsg previos
      // y permite que Run cree un AskMsg fresco desde el nuevo prompt)
      Manager.Blackboard.Clear;

      Writeln(StringOfChar('-', 60));
      Writeln('Pregunta : ', Questions[I]);
      try
        Writeln('Respuesta: ', Manager.Run(Questions[I]));
      except
        on E: Exception do
          Writeln('ERROR: ', E.Message);
      end;
      Writeln;
    end;

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
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
