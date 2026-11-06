program AgentSingleTool;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 07-AgenticLLM / 02-SingleTool
// =============================================================================
// El agente usa una herramienta personalizada vía TAiToolRegistry.
// El LLM decide automáticamente cuándo llamarla (ciclo ReAct automático).
//
// Conceptos que cubre:
//   - IAiTool: interfaz de herramienta para agentes
//   - TAiToolRegistry.Instance.Register: registrar herramientas globalmente
//   - TLLMNode.UseAllTools=True: exponer todas las herramientas registradas
//   - GetSchema: JSON Schema de parámetros de la herramienta
//   - Execute: implementación de la herramienta
//   - Ciclo ReAct: LLM decide → llama tool → LLM interpreta resultado
//
// Herramienta: calculadora (add, subtract, multiply, divide)
// Estructura: [Start] --> [CalculatorAgent] --> [End]
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

// =============================================================================
//  TCalculatorTool
// =============================================================================
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
  Op.AddPair('description', 'Operacion a realizar');
  Enum := TJSONArray.Create;
  Enum.Add('add'); Enum.Add('subtract'); Enum.Add('multiply'); Enum.Add('divide');
  Op.AddPair('enum', Enum);
  Props.AddPair('operation', Op);

  Num := TJSONObject.Create;
  Num.AddPair('type', 'number');
  Num.AddPair('description', 'Primer operando');
  Props.AddPair('a', Num);

  Num := TJSONObject.Create;
  Num.AddPair('type', 'number');
  Num.AddPair('description', 'Segundo operando');
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
function TCalculatorTool.GetDescription: String; begin Result := 'Realiza operaciones aritmeticas: suma, resta, multiplicacion y division'; end;
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
      Result.AddPair('error', 'Division por cero');
      Exit;
    end;
    Res := A / B;
  end;

  Writeln(Format('  [calculator] => %g', [Res]));

  Result := TJSONObject.Create;
  Result.AddPair('result', TJSONNumber.Create(Res));
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunQuery(Manager: TAIAgentManager; const AQuestion: String);
begin
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
  Start    : TAIAgentsNode;
  LLMAgent : TLLMNode;
  Finish   : TAIAgentsNode;
begin
  Writeln('=== AgentSingleTool ===');
  Writeln('Driver: ', DRIVER, ' / ', MODEL);
  Writeln;

  // Registrar la herramienta en el registry global
  TAiToolRegistry.Instance.Register(TCalculatorTool.Create, 'local', 'math');
  Writeln('Herramientas registradas: ', TAiToolRegistry.Instance.Count);
  Writeln;

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 60000;

    Start       := TAIAgentsNode.Create(Manager);
    Start.Name  := 'Start';
    Start.Graph := Manager;

    LLMAgent              := TLLMNode.Create(Manager);
    LLMAgent.Name         := 'CalculatorAgent';
    LLMAgent.Graph        := Manager;
    LLMAgent.DriverName   := DRIVER;
    LLMAgent.Model        := MODEL;
    LLMAgent.ApiKey       := API_KEY;
    LLMAgent.UseAllTools  := True; // expone todas las herramientas del registry
    LLMAgent.SystemPrompt :=
      'Eres un asistente de matematicas. ' +
      'SIEMPRE usa la herramienta calculator para cualquier operacion aritmetica. ' +
      'Nunca calcules mentalmente. Responde en espanol.';

    Finish       := TAIAgentsNode.Create(Manager);
    Finish.Name  := 'End';
    Finish.Graph := Manager;

    Manager.AddEdge('Start',            'CalculatorAgent');
    Manager.AddEdge('CalculatorAgent',  'End');

    Manager.StartNode := Start;
    Manager.EndNode   := Finish;
    Manager.Compile;

    RunQuery(Manager, 'Cuanto es 1234 multiplicado por 5678?');
    RunQuery(Manager, 'Cuanto es (100 + 200) dividido entre 15?');
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
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
