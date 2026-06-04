program Sequential;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 08-Orquestacion / 01-Sequential
// =============================================================================
// Pipeline lineal de tres TLLMNodes especializados.
// La salida de cada nodo es la entrada del siguiente.
// Patron fundamental de composicion de agentes.
//
// Conceptos que cubre:
//   - TAIAgentsLink encadenando nodos linealmente
//   - Salidas intermedias via Blackboard.GetString('Nodo.output')
//   - Especializacion por SystemPrompt
//   - Manager.Run() -> salida del nodo final
//
// Pipeline: [Analizador] -> [Expansor] -> [Redactor]
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

procedure RunPipeline(const ATema: String);
var
  Manager   : TAIAgentManager;
  Analizador: TLLMNode;
  Expansor  : TLLMNode;
  Redactor  : TLLMNode;
  L1, L2    : TAIAgentsLink;
  Resultado : String;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tema: ', ATema);
  Writeln(StringOfChar('=', 60));

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 120000;

    // Nodo 1: Extrae 5 conceptos clave
    Analizador := TLLMNode.Create(Manager);
    Analizador.Name         := 'Analizador';
    Analizador.Graph        := Manager;
    Analizador.DriverName   := DRIVER;
    Analizador.Model        := MODEL;
    Analizador.ApiKey       := API_KEY;
    Analizador.UseAllTools  := False;
    Analizador.SystemPrompt :=
      'Eres un analista de conceptos. Dado un tema, lista exactamente 5 conceptos ' +
      'clave relacionados. Salida SOLO los 5 conceptos, uno por linea, sin numeracion.';

    // Nodo 2: Expande cada concepto con una definicion
    Expansor := TLLMNode.Create(Manager);
    Expansor.Name         := 'Expansor';
    Expansor.Graph        := Manager;
    Expansor.DriverName   := DRIVER;
    Expansor.Model        := MODEL;
    Expansor.ApiKey       := API_KEY;
    Expansor.UseAllTools  := False;
    Expansor.SystemPrompt :=
      'Eres un explicador de conceptos. Recibes una lista de conceptos (uno por linea). ' +
      'Para cada concepto escribe una definicion breve de una sola frase en espanol. ' +
      'Formato: "Concepto: definicion."';

    // Nodo 3: Genera un articulo corto integrando todo
    Redactor := TLLMNode.Create(Manager);
    Redactor.Name         := 'Redactor';
    Redactor.Graph        := Manager;
    Redactor.DriverName   := DRIVER;
    Redactor.Model        := MODEL;
    Redactor.ApiKey       := API_KEY;
    Redactor.UseAllTools  := False;
    Redactor.SystemPrompt :=
      'Eres un redactor tecnico. Recibes definiciones de conceptos. ' +
      'Escribe un articulo corto (3 parrafos) en espanol que integre ' +
      'todos los conceptos de forma natural y coherente.';

    // Encadenar: Analizador -> Expansor -> Redactor
    L1 := TAIAgentsLink.Create(Manager);
    L1.Name := 'L1'; L1.Graph := Manager; L1.NextA := Expansor;
    Analizador.Next := L1;

    L2 := TAIAgentsLink.Create(Manager);
    L2.Name := 'L2'; L2.Graph := Manager; L2.NextA := Redactor;
    Expansor.Next := L2;

    Manager.StartNode := Analizador;
    Manager.EndNode   := Redactor;
    Manager.Compile;

    Writeln;
    Writeln('Ejecutando pipeline secuencial...');
    Resultado := Manager.Run(ATema);

    // Mostrar salidas intermedias del blackboard
    Writeln;
    Writeln('--- Conceptos clave (Analizador) ---');
    Writeln(Manager.Blackboard.GetString('Analizador.output'));
    Writeln;
    Writeln('--- Conceptos expandidos (Expansor) ---');
    Writeln(Manager.Blackboard.GetString('Expansor.output'));
    Writeln;
    Writeln('--- Articulo final (Redactor) ---');
    Writeln(Resultado);

  finally
    Manager.Free;
  end;
  Writeln;
end;

procedure RunDemo;
begin
  Writeln('=== Sequential ===');
  Writeln('Patron: [Analizador] -> [Expansor] -> [Redactor]');
  Writeln;

  RunPipeline('Inteligencia Artificial Generativa');
  RunPipeline('Computacion en la nube');
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
