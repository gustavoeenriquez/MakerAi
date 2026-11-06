program D02_SingleTool;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 02: SingleTool
// =============================================================================
// Introduce TAiToolRegistry con una sola herramienta (calculadora).
// El LLM decide cuando llamarla; el ciclo ReAct es completamente automatico.
//
// Estructura del grafo:
//   [Start] --(lmFanout)--> [CalculatorAgent] --(lmFanout)--> [End]
// =============================================================================

uses
  System.SysUtils,
  System.JSON,
  uMakerAi.Agents,
  uMakerAi.Agents.IAiTool,
  uMakerAi.Agents.ToolRegistry,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

// -----------------------------------------------------------------------------
// TCalculatorTool
// Operaciones: add, subtract, multiply, divide
// Args: { "operation": string, "a": number, "b": number }
// -----------------------------------------------------------------------------
type
  TCalculatorTool = class(TInterfacedObject, IAiTool)
  private
    FSchema: TJSONObject;
  public
    constructor Create;
    destructor  Destroy; override;
    function GetName       : String;
    function GetDescription: String;
    function GetCategory   : String;
    function GetSchema     : TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable   : Boolean;
  end;

constructor TCalculatorTool.Create;
var
  Props, Op, Num: TJSONObject;
  Enum, Req     : TJSONArray;
begin
  inherited Create;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');

  Props := TJSONObject.Create;

  Op := TJSONObject.Create;
  Op.AddPair('type', 'string');
  Op.AddPair('description', 'Arithmetic operation to perform');
  Enum := TJSONArray.Create;
  Enum.Add('add'); Enum.Add('subtract'); Enum.Add('multiply'); Enum.Add('divide');
  Op.AddPair('enum', Enum);
  Props.AddPair('operation', Op);

  Num := TJSONObject.Create;
  Num.AddPair('type', 'number');
  Num.AddPair('description', 'First operand');
  Props.AddPair('a', Num);

  Num := TJSONObject.Create;
  Num.AddPair('type', 'number');
  Num.AddPair('description', 'Second operand');
  Props.AddPair('b', Num);

  FSchema.AddPair('properties', Props);

  Req := TJSONArray.Create;
  Req.Add('operation'); Req.Add('a'); Req.Add('b');
  FSchema.AddPair('required', Req);
end;

destructor TCalculatorTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TCalculatorTool.GetName       : String; begin Result := 'calculator'; end;
function TCalculatorTool.GetDescription: String; begin Result := 'Performs add, subtract, multiply or divide on two numbers'; end;
function TCalculatorTool.GetCategory   : String; begin Result := 'Math'; end;
function TCalculatorTool.GetSchema     : TJSONObject; begin Result := FSchema; end;
function TCalculatorTool.IsAvailable   : Boolean; begin Result := True; end;

function TCalculatorTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Op      : String;
  A, B    : Double;
  Res     : Double;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;

  AArgs.TryGetValue<String>('operation', Op);
  AArgs.TryGetValue<Double>('a', A);
  AArgs.TryGetValue<Double>('b', B);

  Writeln(Format('  [calculator] %s(%g, %g)', [Op, A, B]));

  Res := 0;
  if      Op = 'add'      then Res := A + B
  else if Op = 'subtract' then Res := A - B
  else if Op = 'multiply' then Res := A * B
  else if Op = 'divide'   then
  begin
    if B = 0 then
    begin
      Result := TJSONObject.Create;
      Result.AddPair('error', 'Division by zero');
      Exit;
    end;
    Res := A / B;
  end;

  Writeln(Format('  [calculator] => %g', [Res]));

  Result := TJSONObject.Create;
  Result.AddPair('result', TJSONNumber.Create(Res));
end;

// =============================================================================
// DEMO PRINCIPAL
// =============================================================================

procedure RunQuery(Manager: TAIAgentManager; const AQuestion: String);
begin
  // Limpiar estado del blackboard entre preguntas independientes
  Manager.Blackboard.Clear;

  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', AQuestion);
  Writeln;
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
  Manager  : TAIAgentManager;
  StartNode: TAIAgentsNode;
  LLMNode  : TLLMNode;
  EndNode  : TAIAgentsNode;
begin
  Writeln('=== Demo 02 — SingleTool (Calculadora) ===');
  Writeln('Driver: ', DRIVER, '  Model: ', MODEL);
  Writeln;

  // Registrar la unica herramienta
  TAiToolRegistry.Instance.Register(TCalculatorTool.Create, 'local', 'math');
  Writeln('Registry: ', TAiToolRegistry.Instance.Count, ' herramienta(s) registrada(s).');
  Writeln;

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 60000;

    StartNode       := TAIAgentsNode.Create(Manager);
    StartNode.Name  := 'Start';
    StartNode.Graph := Manager;

    LLMNode              := TLLMNode.Create(Manager);
    LLMNode.Name         := 'CalculatorAgent';
    LLMNode.Graph        := Manager;
    LLMNode.DriverName   := DRIVER;
    LLMNode.Model        := MODEL;
    LLMNode.ApiKey       := API_KEY;
    LLMNode.UseAllTools  := True;
    LLMNode.SystemPrompt :=
      'You are a math assistant. ALWAYS use the calculator tool for any arithmetic. ' +
      'Never compute numbers mentally. Show the operation you performed.';

    EndNode       := TAIAgentsNode.Create(Manager);
    EndNode.Name  := 'End';
    EndNode.Graph := Manager;

    Manager.AddEdge('Start', 'CalculatorAgent');
    Manager.AddEdge('CalculatorAgent', 'End');

    Manager.StartNode := StartNode;
    Manager.EndNode   := EndNode;
    Manager.Compile;

    RunQuery(Manager, 'Cuanto es 1234 multiplicado por 5678?');
    RunQuery(Manager, 'Cuanto es (100 + 200) / 15?');
    RunQuery(Manager, 'Calcula el 15 por ciento de 840.');
    RunQuery(Manager, 'Cuanto es 999 menos 456, y luego multiplica ese resultado por 3?');

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
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
