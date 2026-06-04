program D06_ConditionalRouter;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 06: ConditionalRouter
// =============================================================================
// Un clasificador LLM determina la ruta: 'math', 'science' o 'history'.
// Un nodo router (TRouterHelper) lee la clasificacion del blackboard y escribe
// la clave 'route'. Un link condicional despacha al especialista correcto.
//
//   [Classifier] --> [RouterNode] --lmConditional--> [MathSpecialist]
//                                                \-> [ScienceSpecialist]
//                                                \-> [HistorySpecialist]
//                                                          |
//                                                   [FinalFormatter]
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

// =============================================================================
// TRouterHelper: nodo de enrutamiento sin LLM
// Lee la clasificacion del blackboard y escribe la clave 'route'
// =============================================================================
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
begin
  // La clasificacion viene del blackboard (escrita por el TLLMNode anterior)
  Classification := Trim(LowerCase(Node.Graph.Blackboard.GetString('Classifier.output')));

  // Normalizar: tomar la primera palabra del output (el LLM a veces genera texto extra)
  Route := 'history';  // default
  var FirstWord := Trim(Classification.Split([' ', #10, #13, #9, '.', ','])[0]);
  if FirstWord = 'math' then
    Route := 'math'
  else if FirstWord = 'science' then
    Route := 'science'
  else if FirstWord = 'history' then
    Route := 'history'
  else
  begin
    // fallback: buscar en todo el texto si la primera palabra no coincide
    if Pos('math', Classification) > 0 then
      Route := 'math'
    else if Pos('science', Classification) > 0 then
      Route := 'science';
  end;

  // Escribir la clave que usa el link condicional
  Node.Graph.Blackboard.SetString('route', Route);

  Writeln(Format('  [Router] Clasificacion="%s" -> Ruta="%s"', [Classification, Route]));
  // Pasar la pregunta ORIGINAL (BeforeNode.Input = Classifier.Input = pregunta del usuario)
  // no el texto de clasificacion que devolvio el LLM
  if Assigned(BeforeNode) then
    Output := BeforeNode.Input
  else
    Output := Input;
end;

// =============================================================================
// Helper: crea y configura un TLLMNode
// =============================================================================
function MakeLLMNode(Manager: TAIAgentManager; const AName, ASystemPrompt: String;
  UseTools: Boolean = False): TLLMNode;
begin
  Result := TLLMNode.Create(Manager);
  Result.Name         := AName;
  Result.Graph        := Manager;
  Result.DriverName   := DRIVER;
  Result.Model        := MODEL;
  Result.ApiKey       := API_KEY;
  Result.UseAllTools  := UseTools;
  Result.SystemPrompt := ASystemPrompt;
end;

procedure RunQuery(Manager: TAIAgentManager; const AQuestion: String);
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', AQuestion);
  try
    Writeln;
    Writeln('Respuesta: ', Manager.Run(AQuestion));
  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;
  Writeln;
end;

procedure RunDemo;
var
  Manager       : TAIAgentManager;
  RouterHelper  : TRouterHelper;
  Classifier    : TLLMNode;
  MathSpec      : TLLMNode;
  ScienceSpec   : TLLMNode;
  HistorySpec   : TLLMNode;
  FinalFmt      : TLLMNode;
  RouterNode    : TAIAgentsNode;
  RouterLink    : TAIAgentsLink;
  CondLink      : TAIAgentsLink;
  JoinLinkMath  : TAIAgentsLink;
  JoinLinkSci   : TAIAgentsLink;
  JoinLinkHist  : TAIAgentsLink;
  CondTargets   : TDictionary<String, TAIAgentsNode>;
begin
  Writeln('=== Demo 06 - ConditionalRouter ===');
  Writeln('Patron: Clasificador -> Router -> Especialista (math|science|history) -> Formatter');
  Writeln;

  RouterHelper := TRouterHelper.Create;
  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 90000;

    // --- Nodo clasificador (TLLMNode) ---
    Classifier := MakeLLMNode(Manager, 'Classifier',
      'You are a CLASSIFIER. Your job is to output ONE word only. Do not answer the question.' + #10 +
      'Given a question, output ONLY one of these exact words: math  science  history' + #10 +
      'No explanations. No punctuation. Just one word.');
    Classifier.MaxTokens := 3;  // forzar 1 token

    // --- Nodo router (TRouterHelper.Execute como metodo de objeto) ---
    RouterNode := TAIAgentsNode.Create(Manager);
    RouterNode.Name      := 'Router';
    RouterNode.Graph     := Manager;
    RouterNode.OnExecute := RouterHelper.Execute;

    // --- Especialistas ---
    MathSpec := MakeLLMNode(Manager, 'MathSpecialist',
      'You are a math expert. Answer the question clearly, showing all steps if calculation is involved.');

    ScienceSpec := MakeLLMNode(Manager, 'ScienceSpecialist',
      'You are a science expert. Answer the question with scientific accuracy, using clear language.');

    HistorySpec := MakeLLMNode(Manager, 'HistorySpecialist',
      'You are a history expert. Answer the question with historical context and key facts.');

    // --- Formateador final (jmAny: solo un especialista llega) ---
    FinalFmt := MakeLLMNode(Manager, 'FinalFormatter',
      'You receive an expert answer. Format it nicely: add a brief title line, ' +
      'then the answer. Keep it concise and well-structured.');
    FinalFmt.JoinMode := jmAny;

    // --- Enlace: Classifier -> RouterNode ---
    RouterLink := TAIAgentsLink.Create(Manager);
    RouterLink.Name  := 'ToRouter';
    RouterLink.Graph := Manager;
    RouterLink.NextA := RouterNode;
    Classifier.Next  := RouterLink;

    // --- Link condicional: RouterNode -> (MathSpec | ScienceSpec | HistorySpec) ---
    CondLink      := TAIAgentsLink.Create(Manager);
    CondLink.Name := 'CondLink';
    CondLink.Graph := Manager;
    CondLink.Mode := lmConditional;
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

    // --- Joins: cada especialista -> FinalFormatter ---
    JoinLinkMath := TAIAgentsLink.Create(Manager);
    JoinLinkMath.Name := 'JoinMath'; JoinLinkMath.Graph := Manager; JoinLinkMath.NextA := FinalFmt;
    MathSpec.Next := JoinLinkMath;

    JoinLinkSci := TAIAgentsLink.Create(Manager);
    JoinLinkSci.Name := 'JoinSci'; JoinLinkSci.Graph := Manager; JoinLinkSci.NextA := FinalFmt;
    ScienceSpec.Next := JoinLinkSci;

    JoinLinkHist := TAIAgentsLink.Create(Manager);
    JoinLinkHist.Name := 'JoinHist'; JoinLinkHist.Graph := Manager; JoinLinkHist.NextA := FinalFmt;
    HistorySpec.Next := JoinLinkHist;

    // --- Configurar grafo ---
    Manager.StartNode := Classifier;
    Manager.EndNode   := FinalFmt;
    Manager.Compile;

    // --- Ejecutar consultas de distintas categorias ---
    RunQuery(Manager, 'What is the square root of 144?');
    RunQuery(Manager, 'How does photosynthesis work?');
    RunQuery(Manager, 'When did World War II end and who signed the surrender?');
    RunQuery(Manager, 'What is 15 percent of 240?');
    RunQuery(Manager, 'What causes a solar eclipse?');

  finally
    Manager.Free;
    RouterHelper.Free;
  end;
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
