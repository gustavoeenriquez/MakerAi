program AgentRouter;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — 07-AgenticLLM / 07-ConditionalRouter
// =============================================================================
// Un clasificador LLM determina la ruta y un link condicional despacha
// al especialista correcto:
//
//   [Classifier] -> [RouterNode] --lmConditional--> [MathSpecialist]
//                                               \-> [ScienceSpecialist]
//                                               \-> [HistorySpecialist]
//                                                         |
//                                                  [FinalFormatter] (jmAny)
//
// Conceptos que cubre:
//   - TAIAgentsLink.Mode = lmConditional: enrutamiento por valor del blackboard
//   - TAIAgentsLink.ConditionalKey: clave que se evalua en el blackboard
//   - TAIAgentsLink.AddConditionalTarget: mapeo valor -> nodo destino
//   - Nodo plain con OnExecute: logica de enrutamiento sin LLM
//   - JoinMode = jmAny: el formateador acepta cualquiera de los tres especialistas
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

// -----------------------------------------------------------------------------
// TRouterHelper: lee la clasificacion del blackboard y escribe la clave 'route'
// -----------------------------------------------------------------------------
type
  TRouterHelper = class
  public
    procedure Execute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink;
      Input: String; var Output: String);
  end;

procedure TRouterHelper.Execute(Node, BeforeNode: TAIAgentsNode;
  Link: TAIAgentsLink; Input: String; var Output: String);
var
  Classification: String;
  Route         : String;
  FirstWord     : String;
begin
  Classification := Trim(LowerCase(Node.Graph.Blackboard.GetString('Classifier.output')));

  // Normalizar: tomar la primera palabra (el LLM a veces genera texto extra)
  Route     := 'history';  // valor por defecto
  FirstWord := '';
  if Length(Classification) > 0 then
    FirstWord := Trim(Classification.Split([' ', #10, #13, #9, '.', ','])[0]);

  if      FirstWord = 'math'    then Route := 'math'
  else if FirstWord = 'science' then Route := 'science'
  else if FirstWord = 'history' then Route := 'history'
  else begin
    // Fallback: buscar en el texto completo
    if      Pos('math',    Classification) > 0 then Route := 'math'
    else if Pos('science', Classification) > 0 then Route := 'science';
  end;

  Node.Graph.Blackboard.SetString('route', Route);
  Writeln(Format('  [Router] Clasificacion="%s" -> Ruta="%s"', [Classification, Route]));

  // Pasar la pregunta original (no el texto de clasificacion)
  if Assigned(BeforeNode) then
    Output := BeforeNode.Input
  else
    Output := Input;
end;

// -----------------------------------------------------------------------------

function MakeLLMNode(Manager: TAIAgentManager; const AName, ASystemPrompt: String): TLLMNode;
begin
  Result := TLLMNode.Create(Manager);
  Result.Name         := AName;
  Result.Graph        := Manager;
  Result.DriverName   := DRIVER;
  Result.Model        := MODEL;
  Result.ApiKey       := API_KEY;
  Result.UseAllTools  := False;
  Result.SystemPrompt := ASystemPrompt;
end;

procedure RunQuery(Manager: TAIAgentManager; const AQuestion: String);
begin
  Manager.Blackboard.Clear;
  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', AQuestion);
  try
    Writeln('Respuesta: ', Manager.Run(AQuestion));
  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;
  Writeln;
end;

procedure RunDemo;
var
  Manager      : TAIAgentManager;
  RouterHelper : TRouterHelper;
  Classifier   : TLLMNode;
  MathSpec     : TLLMNode;
  ScienceSpec  : TLLMNode;
  HistorySpec  : TLLMNode;
  FinalFmt     : TLLMNode;
  RouterNode   : TAIAgentsNode;
  RouterLink   : TAIAgentsLink;
  CondLink     : TAIAgentsLink;
  JoinMath     : TAIAgentsLink;
  JoinSci      : TAIAgentsLink;
  JoinHist     : TAIAgentsLink;
  CondTargets  : TDictionary<String, TAIAgentsNode>;
begin
  RouterHelper := TRouterHelper.Create;
  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 90000;

    // Clasificador: emite UNA palabra (math/science/history)
    Classifier := MakeLLMNode(Manager, 'Classifier',
      'You are a CLASSIFIER. Output ONE word only. Do not answer the question.' + #10 +
      'Given a question, output EXACTLY one of: math  science  history' + #10 +
      'No explanations. No punctuation. Just one word.');
    Classifier.MaxTokens := 3;

    // Nodo router: lee la clasificacion y escribe 'route' en el blackboard
    RouterNode := TAIAgentsNode.Create(Manager);
    RouterNode.Name      := 'Router';
    RouterNode.Graph     := Manager;
    RouterNode.OnExecute := RouterHelper.Execute;

    // Especialistas
    MathSpec := MakeLLMNode(Manager, 'MathSpecialist',
      'You are a math expert. Answer clearly, showing all steps if needed.');

    ScienceSpec := MakeLLMNode(Manager, 'ScienceSpecialist',
      'You are a science expert. Answer with scientific accuracy and clear language.');

    HistorySpec := MakeLLMNode(Manager, 'HistorySpecialist',
      'You are a history expert. Answer with historical context and key facts.');

    // Formateador final: acepta el primero que llege (jmAny, solo uno activa)
    FinalFmt := MakeLLMNode(Manager, 'FinalFormatter',
      'You receive an expert answer. Format it nicely: add a brief title, ' +
      'then the answer. Keep it concise and well-structured.');
    FinalFmt.JoinMode := jmAny;

    // Classifier -> RouterNode
    RouterLink := TAIAgentsLink.Create(Manager);
    RouterLink.Name  := 'ToRouter';
    RouterLink.Graph := Manager;
    RouterLink.NextA := RouterNode;
    Classifier.Next  := RouterLink;

    // RouterNode -> especialista segun 'route'
    CondLink := TAIAgentsLink.Create(Manager);
    CondLink.Name           := 'CondLink';
    CondLink.Graph          := Manager;
    CondLink.Mode           := lmConditional;
    CondLink.ConditionalKey := 'route';

    CondTargets := TDictionary<String, TAIAgentsNode>.Create;
    try
      CondTargets.Add('math',    MathSpec);
      CondTargets.Add('science', ScienceSpec);
      CondTargets.Add('history', HistorySpec);
      for var Pair in CondTargets do
        CondLink.AddConditionalTarget(Pair.Key, Pair.Value);
    finally
      CondTargets.Free;
    end;
    RouterNode.Next := CondLink;

    // Cada especialista -> FinalFormatter
    JoinMath := TAIAgentsLink.Create(Manager);
    JoinMath.Name := 'JoinMath'; JoinMath.Graph := Manager; JoinMath.NextA := FinalFmt;
    MathSpec.Next := JoinMath;

    JoinSci := TAIAgentsLink.Create(Manager);
    JoinSci.Name := 'JoinSci'; JoinSci.Graph := Manager; JoinSci.NextA := FinalFmt;
    ScienceSpec.Next := JoinSci;

    JoinHist := TAIAgentsLink.Create(Manager);
    JoinHist.Name := 'JoinHist'; JoinHist.Graph := Manager; JoinHist.NextA := FinalFmt;
    HistorySpec.Next := JoinHist;

    Manager.StartNode := Classifier;
    Manager.EndNode   := FinalFmt;
    Manager.Compile;

    RunQuery(Manager, 'What is the square root of 144?');
    RunQuery(Manager, 'How does photosynthesis work?');
    RunQuery(Manager, 'When did World War II end?');
    RunQuery(Manager, 'What is 15 percent of 240?');
    RunQuery(Manager, 'What causes a solar eclipse?');

  finally
    Manager.Free;
    RouterHelper.Free;
  end;
end;

begin
  Writeln('=== AgentRouter ===');
  Writeln('Patron: Clasificador -> Router (lmConditional) -> Especialista -> Formateador');
  Writeln;
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
