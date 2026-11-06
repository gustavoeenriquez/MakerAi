program ComplexPipeline;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 08-Orquestacion / 05-ComplexPipeline
// =============================================================================
// Pipeline complejo que combina todos los patrones:
//   1. Sequential: Clasificador -> Contextualizador
//   2. Fork/Join: dos ramas paralelas (Pros + Cons) -> Sintetizador
//   3. Sequential final: Sintetizador -> Redactor
//
// Dado un tema:
//   [Contextualizador] --fork--> [AnalistaPros]  \
//                             \-> [AnalistaCons] --> [Sintetizador (jmAll)] -> [Redactor]
//
// Conceptos que cubre:
//   - Combinacion de patrones Sequential + Fork/Join en un solo grafo
//   - Pipeline con 5 nodos LLM en topologia mixta
//   - Inspección completa del blackboard
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

procedure RunComplexPipeline(const ATema: String);
var
  Manager          : TAIAgentManager;
  Contextualizador : TLLMNode;
  AnalistaPros     : TLLMNode;
  AnalistaCons     : TLLMNode;
  Sintetizador     : TLLMNode;
  Redactor         : TLLMNode;
  ForkLink         : TAIAgentsLink;
  JoinA, JoinB     : TAIAgentsLink;
  SintToRedactor   : TAIAgentsLink;
  Resultado        : String;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tema: ', ATema);
  Writeln(StringOfChar('=', 60));

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 180000;

    // Nodo 1 (secuencial): Contextualizador — prepara el contexto del tema
    Contextualizador := MakeLLM(Manager, 'Contextualizador',
      'Eres un analista de contexto. Dado un tema, escribe una ' +
      'descripcion breve (2 oraciones) que establezca el contexto para ' +
      'un analisis de pros y contras. Responde en espanol.');

    // Rama A (fork): Analista de ventajas
    AnalistaPros := MakeLLM(Manager, 'AnalistaPros',
      'Eres un analista de ventajas. Recibes contexto sobre un tema. ' +
      'Lista 3 ventajas principales, una por linea. ' +
      'Formato: "PROS:\n+ [ventaja 1]\n+ [ventaja 2]\n+ [ventaja 3]"');

    // Rama B (fork): Analista de desventajas
    AnalistaCons := MakeLLM(Manager, 'AnalistaCons',
      'Eres un analista de desventajas. Recibes contexto sobre un tema. ' +
      'Lista 3 desventajas principales, una por linea. ' +
      'Formato: "CONS:\n- [desventaja 1]\n- [desventaja 2]\n- [desventaja 3]"');

    // Nodo join (jmAll): Sintetizador — combina pros y cons
    Sintetizador := MakeLLM(Manager, 'Sintetizador',
      'Eres un sintetizador. Recibes la salida combinada de dos analistas: ' +
      'uno con PROS y otro con CONS. Sintetiza en 2 parrafos en espanol: ' +
      'el primero resume las ventajas, el segundo las desventajas.');
    Sintetizador.JoinMode := jmAll;

    // Nodo final (secuencial): Redactor — genera el articulo final
    Redactor := MakeLLM(Manager, 'Redactor',
      'Eres un redactor. Recibes un analisis de pros y cons sintetizado. ' +
      'Escribe un articulo de opinion de 3 parrafos en espanol que ' +
      'presente el tema de forma equilibrada y concluya con una recomendacion.');

    // Enlace secuencial: Contextualizador -> Fork
    ForkLink := TAIAgentsLink.Create(Manager);
    ForkLink.Name := 'Fork'; ForkLink.Graph := Manager;
    ForkLink.NextA := AnalistaPros;
    ForkLink.NextB := AnalistaCons;
    Contextualizador.Next := ForkLink;

    // Join A: AnalistaPros -> Sintetizador
    JoinA := TAIAgentsLink.Create(Manager);
    JoinA.Name := 'JoinA'; JoinA.Graph := Manager; JoinA.NextA := Sintetizador;
    AnalistaPros.Next := JoinA;

    // Join B: AnalistaCons -> Sintetizador
    JoinB := TAIAgentsLink.Create(Manager);
    JoinB.Name := 'JoinB'; JoinB.Graph := Manager; JoinB.NextA := Sintetizador;
    AnalistaCons.Next := JoinB;

    // Enlace secuencial: Sintetizador -> Redactor
    SintToRedactor := TAIAgentsLink.Create(Manager);
    SintToRedactor.Name := 'SintToRed'; SintToRedactor.Graph := Manager;
    SintToRedactor.NextA := Redactor;
    Sintetizador.Next := SintToRedactor;

    Manager.StartNode := Contextualizador;
    Manager.EndNode   := Redactor;
    Manager.Compile;

    Writeln;
    Writeln('Ejecutando pipeline complejo...');
    Resultado := Manager.Run(ATema);

    // Inspeccionar todos los checkpoints
    Writeln;
    Writeln('--- Contexto (Contextualizador) ---');
    Writeln(Manager.Blackboard.GetString('Contextualizador.output'));
    Writeln;
    Writeln('--- Ventajas (AnalistaPros) ---');
    Writeln(Manager.Blackboard.GetString('AnalistaPros.output'));
    Writeln;
    Writeln('--- Desventajas (AnalistaCons) ---');
    Writeln(Manager.Blackboard.GetString('AnalistaCons.output'));
    Writeln;
    Writeln('--- Sintesis (Sintetizador) ---');
    Writeln(Manager.Blackboard.GetString('Sintetizador.output'));
    Writeln;
    Writeln('--- Articulo final (Redactor) ---');
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
  Writeln('=== ComplexPipeline ===');
  Writeln('Patrones combinados: Sequential + Fork/Join');
  Writeln('Contextualizador -> [Pros + Cons] -> Sintetizador -> Redactor');
  Writeln;

  RunComplexPipeline('Automatizacion industrial con robotica e IA');
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
