program ForkJoin;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 08-Orquestacion / 02-Fork-Join
// =============================================================================
// Patron Fork/Join: una entrada se bifurca en dos ramas paralelas que se
// reunen en un nodo de sintesis que espera ambos resultados (jmAll).
//
// Conceptos que cubre:
//   - TAIAgentsLink con NextA + NextB: bifurcacion (fork)
//   - TLLMNode.JoinMode := jmAll: espera TODOS los inputs entrantes
//   - Ejecucion paralela de dos ramas independientes
//   - Blackboard para inspeccionar cada rama por separado
//
// Estructura:
//   [Preparador] --fork--> [AnalistaPros]  --join--\
//                      \-> [AnalistaCons] --join--> [Sintetizador (jmAll)]
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

procedure RunForkJoin(const ATema: String);
var
  Manager     : TAIAgentManager;
  Preparador  : TLLMNode;
  AnalistaPros: TLLMNode;
  AnalistaCons: TLLMNode;
  Sintetizador: TLLMNode;
  ForkLink    : TAIAgentsLink;
  JoinLinkA   : TAIAgentsLink;
  JoinLinkB   : TAIAgentsLink;
  Resultado   : String;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tema: ', ATema);
  Writeln(StringOfChar('=', 60));

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 120000;

    // Nodo 0: Preparador — formula la pregunta de investigacion
    Preparador := TLLMNode.Create(Manager);
    Preparador.Name         := 'Preparador';
    Preparador.Graph        := Manager;
    Preparador.DriverName   := DRIVER;
    Preparador.Model        := MODEL;
    Preparador.ApiKey       := API_KEY;
    Preparador.UseAllTools  := False;
    Preparador.SystemPrompt :=
      'Eres un coordinador de investigacion. Dado un tema, formula una ' +
      'pregunta de investigacion clara y concisa. Escribe SOLO la pregunta, nada mas.';

    // Rama A: Analista de ventajas
    AnalistaPros := TLLMNode.Create(Manager);
    AnalistaPros.Name         := 'AnalistaPros';
    AnalistaPros.Graph        := Manager;
    AnalistaPros.DriverName   := DRIVER;
    AnalistaPros.Model        := MODEL;
    AnalistaPros.ApiKey       := API_KEY;
    AnalistaPros.UseAllTools  := False;
    AnalistaPros.SystemPrompt :=
      'Eres un analista de ventajas. Dada una pregunta, responde EXACTAMENTE con:' + #10 +
      'VENTAJAS:' + #10 +
      '1. [ventaja en una frase]' + #10 +
      '2. [ventaja en una frase]' + #10 +
      '3. [ventaja en una frase]' + #10 +
      'Solo eso. Sin texto adicional.';

    // Rama B: Analista de desventajas
    AnalistaCons := TLLMNode.Create(Manager);
    AnalistaCons.Name         := 'AnalistaCons';
    AnalistaCons.Graph        := Manager;
    AnalistaCons.DriverName   := DRIVER;
    AnalistaCons.Model        := MODEL;
    AnalistaCons.ApiKey       := API_KEY;
    AnalistaCons.UseAllTools  := False;
    AnalistaCons.SystemPrompt :=
      'Eres un analista de desventajas. Dada una pregunta, responde EXACTAMENTE con:' + #10 +
      'DESVENTAJAS:' + #10 +
      '1. [desventaja en una frase]' + #10 +
      '2. [desventaja en una frase]' + #10 +
      '3. [desventaja en una frase]' + #10 +
      'Solo eso. Sin texto adicional.';

    // Nodo Join: Sintetizador (jmAll — espera ambas ramas)
    Sintetizador := TLLMNode.Create(Manager);
    Sintetizador.Name         := 'Sintetizador';
    Sintetizador.Graph        := Manager;
    Sintetizador.DriverName   := DRIVER;
    Sintetizador.Model        := MODEL;
    Sintetizador.ApiKey       := API_KEY;
    Sintetizador.UseAllTools  := False;
    Sintetizador.JoinMode     := jmAll;
    Sintetizador.SystemPrompt :=
      'Eres un analista equilibrado. Recibes la salida combinada de dos investigadores: ' +
      'uno listo VENTAJAS y otro listo DESVENTAJAS. ' +
      'Sintetiza ambas en un veredicto balanceado de 3-4 oraciones en espanol. ' +
      'Termina con una recomendacion clara.';

    // Fork: Preparador -> (AnalistaPros, AnalistaCons) en paralelo
    ForkLink := TAIAgentsLink.Create(Manager);
    ForkLink.Name  := 'Fork'; ForkLink.Graph := Manager;
    ForkLink.NextA := AnalistaPros;
    ForkLink.NextB := AnalistaCons;
    Preparador.Next := ForkLink;

    // Join A: AnalistaPros -> Sintetizador
    JoinLinkA := TAIAgentsLink.Create(Manager);
    JoinLinkA.Name := 'JoinA'; JoinLinkA.Graph := Manager; JoinLinkA.NextA := Sintetizador;
    AnalistaPros.Next := JoinLinkA;

    // Join B: AnalistaCons -> Sintetizador
    JoinLinkB := TAIAgentsLink.Create(Manager);
    JoinLinkB.Name := 'JoinB'; JoinLinkB.Graph := Manager; JoinLinkB.NextA := Sintetizador;
    AnalistaCons.Next := JoinLinkB;

    Manager.StartNode := Preparador;
    Manager.EndNode   := Sintetizador;
    Manager.Compile;

    Writeln;
    Writeln('Ejecutando Fork/Join en paralelo...');
    Resultado := Manager.Run(ATema);

    Writeln;
    Writeln('--- Pregunta de investigacion (Preparador) ---');
    Writeln(Manager.Blackboard.GetString('Preparador.output'));
    Writeln;
    Writeln('--- Analisis VENTAJAS (AnalistaPros) ---');
    Writeln(Manager.Blackboard.GetString('AnalistaPros.output'));
    Writeln;
    Writeln('--- Analisis DESVENTAJAS (AnalistaCons) ---');
    Writeln(Manager.Blackboard.GetString('AnalistaCons.output'));
    Writeln;
    Writeln('--- Veredicto final (Sintetizador) ---');
    Writeln(Resultado);

  finally
    Manager.Free;
  end;
  Writeln;
end;

procedure PrintException(E: Exception);
var
  Agg: EAggregateException;
  I  : Integer;
begin
  if E is EAggregateException then
  begin
    Agg := EAggregateException(E);
    for I := 0 to Agg.Count - 1 do
      Writeln('  [', I, '] ', Agg.InnerExceptions[I].ClassName, ': ',
        Agg.InnerExceptions[I].Message);
  end
  else
    Writeln(E.ClassName, ': ', E.Message);
end;

procedure RunDemo;
begin
  Writeln('=== ForkJoin ===');
  Writeln('Patron: [Preparador] --fork--> [Pros + Cons] --join--> [Sintetizador]');
  Writeln;

  RunForkJoin('Inteligencia Artificial en la educacion');
  Writeln('Pausa entre temas (10s)...');
  Sleep(10000);
  RunForkJoin('Trabajo remoto para desarrolladores de software');
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
    begin
      Writeln('FATAL:');
      PrintException(E);
    end;
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
