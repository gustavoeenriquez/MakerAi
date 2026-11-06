program ConditionalRouter;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 08-Orquestacion / 03-ConditionalRouter
// =============================================================================
// Un clasificador LLM determina la ruta en tiempo de ejecucion.
// Un nodo router (plain node con OnExecute) lee la clasificacion del
// blackboard y escribe la clave 'ruta'. Un link condicional despacha
// al especialista correcto.
//
// Conceptos que cubre:
//   - TAIAgentsLink.Mode := lmConditional
//   - TAIAgentsLink.ConditionalKey: clave en el blackboard que decide la ruta
//   - TAIAgentsLink.AddConditionalTarget(key, node)
//   - TAIAgentsNode.OnExecute: logica de enrutamiento sin LLM
//   - TLLMNode.JoinMode := jmAny: el formatter solo recibe un especialista
//
// Estructura:
//   [Clasificador] -> [Router (OnExecute)] --lmConditional-->
//     math    -> [EspecialistaMath]     \
//     science -> [EspecialistaCiencia]  --> [Formatter (jmAny)]
//     history -> [EspecialistaHistoria] /
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
//  TRouterLogic: logica de enrutamiento sin LLM
// =============================================================================
type
  TRouterLogic = class
  public
    procedure Execute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink;
      Input: String; var Output: String);
  end;

procedure TRouterLogic.Execute(Node, BeforeNode: TAIAgentsNode;
  Link: TAIAgentsLink; Input: String; var Output: String);
var
  Clasificacion, Ruta, PrimeraP: String;
begin
  Clasificacion := Trim(LowerCase(Node.Graph.Blackboard.GetString('Clasificador.output')));

  // Tomar la primera palabra de la clasificacion
  Ruta := 'history'; // default
  PrimeraP := '';
  if Length(Clasificacion) > 0 then
  begin
    var Parts := Clasificacion.Split([' ', #10, #13, #9, '.', ',']);
    if Length(Parts) > 0 then
      PrimeraP := Trim(Parts[0]);
  end;

  if PrimeraP = 'math' then Ruta := 'math'
  else if PrimeraP = 'science' then Ruta := 'science'
  else if PrimeraP = 'history' then Ruta := 'history'
  else
  begin
    if Clasificacion.Contains('math') then Ruta := 'math'
    else if Clasificacion.Contains('science') then Ruta := 'science'
    else if Clasificacion.Contains('history') then Ruta := 'history';
  end;

  Node.Graph.Blackboard.SetString('ruta', Ruta);
  Writeln(Format('  [Router] Clasificacion="%s" -> Ruta="%s"', [Clasificacion, Ruta]));

  // Pasar la pregunta original (no la clasificacion)
  if Assigned(BeforeNode) then
    Output := BeforeNode.Input
  else
    Output := Input;
end;

// =============================================================================
//  Helper: crear TLLMNode
// =============================================================================
function MakeLLM(Manager: TAIAgentManager; const AName, APrompt: String;
  UseTools: Boolean = False): TLLMNode;
begin
  Result := TLLMNode.Create(Manager);
  Result.Name         := AName;
  Result.Graph        := Manager;
  Result.DriverName   := DRIVER;
  Result.Model        := MODEL;
  Result.ApiKey       := API_KEY;
  Result.UseAllTools  := UseTools;
  Result.SystemPrompt := APrompt;
end;

procedure RunQuery(Manager: TAIAgentManager; const APregunta: String);
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', APregunta);
  try
    Writeln;
    Writeln('Respuesta: ', Manager.Run(APregunta));
  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;
  Writeln;
end;

procedure RunDemo;
var
  Manager       : TAIAgentManager;
  RouterLogic   : TRouterLogic;
  Clasificador  : TLLMNode;
  RouterNode    : TAIAgentsNode;
  EspMath       : TLLMNode;
  EspCiencia    : TLLMNode;
  EspHistoria   : TLLMNode;
  Formatter     : TLLMNode;
  ToRouter      : TAIAgentsLink;
  CondLink      : TAIAgentsLink;
  JoinMath      : TAIAgentsLink;
  JoinCiencia   : TAIAgentsLink;
  JoinHistoria  : TAIAgentsLink;
  Targets       : TDictionary<String, TAIAgentsNode>;
begin
  Writeln('=== ConditionalRouter ===');
  Writeln('Patron: Clasificador -> Router -> Especialista (math|science|history) -> Formatter');
  Writeln;

  RouterLogic := TRouterLogic.Create;
  Manager     := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 90000;

    // Clasificador: devuelve UNA palabra
    Clasificador := MakeLLM(Manager, 'Clasificador',
      'Eres un CLASIFICADOR. Tu trabajo es devolver UNA sola palabra.' + #10 +
      'Dada una pregunta, responde SOLO con una de estas palabras exactas: math  science  history' + #10 +
      'Sin explicaciones. Sin puntuacion. Solo una palabra.');
    Clasificador.MaxTokens := 3;

    // Router: nodo sin LLM que lee el blackboard
    RouterNode := TAIAgentsNode.Create(Manager);
    RouterNode.Name      := 'Router';
    RouterNode.Graph     := Manager;
    RouterNode.OnExecute := RouterLogic.Execute;

    // Especialistas
    EspMath := MakeLLM(Manager, 'EspecialistaMath',
      'Eres un experto en matematicas. Responde la pregunta claramente ' +
      'mostrando todos los pasos si hay calculos. Responde en espanol.');

    EspCiencia := MakeLLM(Manager, 'EspecialistaCiencia',
      'Eres un experto en ciencias. Responde con precision cientifica ' +
      'usando lenguaje claro. Responde en espanol.');

    EspHistoria := MakeLLM(Manager, 'EspecialistaHistoria',
      'Eres un experto en historia. Responde con contexto historico ' +
      'y datos clave. Responde en espanol.');

    // Formatter final: jmAny (solo un especialista llega)
    Formatter := MakeLLM(Manager, 'Formatter',
      'Recibes la respuesta de un experto. ' +
      'Formatéala: agrega un titulo breve, luego la respuesta estructurada. ' +
      'Mantén la concision. Responde en espanol.');
    Formatter.JoinMode := jmAny;

    // Enlace: Clasificador -> Router
    ToRouter := TAIAgentsLink.Create(Manager);
    ToRouter.Name := 'ToRouter'; ToRouter.Graph := Manager; ToRouter.NextA := RouterNode;
    Clasificador.Next := ToRouter;

    // Link condicional: Router -> (EspMath | EspCiencia | EspHistoria)
    CondLink := TAIAgentsLink.Create(Manager);
    CondLink.Name := 'CondLink'; CondLink.Graph := Manager;
    CondLink.Mode := lmConditional;
    CondLink.ConditionalKey := 'ruta';

    Targets := TDictionary<String, TAIAgentsNode>.Create;
    try
      Targets.Add('math',    EspMath);
      Targets.Add('science', EspCiencia);
      Targets.Add('history', EspHistoria);
      for var Pair in Targets do
        CondLink.AddConditionalTarget(Pair.Key, Pair.Value);
    finally
      Targets.Free;
    end;
    RouterNode.Next := CondLink;

    // Joins: cada especialista -> Formatter
    JoinMath := TAIAgentsLink.Create(Manager);
    JoinMath.Name := 'JoinMath'; JoinMath.Graph := Manager; JoinMath.NextA := Formatter;
    EspMath.Next := JoinMath;

    JoinCiencia := TAIAgentsLink.Create(Manager);
    JoinCiencia.Name := 'JoinCiencia'; JoinCiencia.Graph := Manager; JoinCiencia.NextA := Formatter;
    EspCiencia.Next := JoinCiencia;

    JoinHistoria := TAIAgentsLink.Create(Manager);
    JoinHistoria.Name := 'JoinHistoria'; JoinHistoria.Graph := Manager; JoinHistoria.NextA := Formatter;
    EspHistoria.Next := JoinHistoria;

    Manager.StartNode := Clasificador;
    Manager.EndNode   := Formatter;
    Manager.Compile;

    // Consultas de diferentes categorias
    RunQuery(Manager, 'Cuanto es la raiz cuadrada de 225?');
    RunQuery(Manager, 'Como funciona la fotosintesis?');
    RunQuery(Manager, 'Cuando termino la Segunda Guerra Mundial?');
    RunQuery(Manager, 'Cuanto es el 18 por ciento de 350?');
    RunQuery(Manager, 'Que causa un eclipse solar?');

  finally
    Manager.Free;
    RouterLogic.Free;
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
