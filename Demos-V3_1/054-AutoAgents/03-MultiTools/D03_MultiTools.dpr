program D03_MultiTools;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 03: MultiTools
// =============================================================================
// Un TLLMNode con cuatro herramientas distintas. Demuestra llamadas paralelas
// a herramientas, el blackboard y consultas que requieren varias herramientas.
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
// -----------------------------------------------------------------------------
type
  TCalculatorTool = class(TInterfacedObject, IAiTool)
  private
    FSchema: TJSONObject;
  public
    constructor Create;
    destructor  Destroy; override;
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

constructor TCalculatorTool.Create;
var
  Props, Op, Num: TJSONObject;
  Enum, Req: TJSONArray;
begin
  inherited;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Op := TJSONObject.Create;
  Op.AddPair('type', 'string');
  Enum := TJSONArray.Create;
  Enum.Add('add'); Enum.Add('subtract'); Enum.Add('multiply'); Enum.Add('divide');
  Op.AddPair('enum', Enum);
  Props.AddPair('operation', Op);
  Num := TJSONObject.Create; Num.AddPair('type', 'number'); Props.AddPair('a', Num);
  Num := TJSONObject.Create; Num.AddPair('type', 'number'); Props.AddPair('b', Num);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('operation'); Req.Add('a'); Req.Add('b');
  FSchema.AddPair('required', Req);
end;

destructor TCalculatorTool.Destroy; begin FSchema.Free; inherited; end;
function TCalculatorTool.GetName: String; begin Result := 'calculator'; end;
function TCalculatorTool.GetDescription: String; begin Result := 'Arithmetic: add, subtract, multiply, divide'; end;
function TCalculatorTool.GetCategory: String; begin Result := 'Math'; end;
function TCalculatorTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TCalculatorTool.IsAvailable: Boolean; begin Result := True; end;

function TCalculatorTool.Execute(const AArgs: TJSONObject): TJSONObject;
var Op: String; A, B, Res: Double;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('operation', Op);
  AArgs.TryGetValue<Double>('a', A);
  AArgs.TryGetValue<Double>('b', B);
  Writeln(Format('  [calculator] %s(%g, %g)', [Op, A, B]));
  Res := 0;
  if Op = 'add' then Res := A + B
  else if Op = 'subtract' then Res := A - B
  else if Op = 'multiply' then Res := A * B
  else if Op = 'divide' then begin
    if B = 0 then begin Result := TJSONObject.Create; Result.AddPair('error', 'Division by zero'); Exit; end;
    Res := A / B;
  end;
  Writeln(Format('  [calculator] => %g', [Res]));
  Result := TJSONObject.Create;
  Result.AddPair('result', TJSONNumber.Create(Res));
end;

// -----------------------------------------------------------------------------
// TDateTimeTool — fecha y hora actual
// -----------------------------------------------------------------------------
type
  TDateTimeTool = class(TInterfacedObject, IAiTool)
  private
    FSchema: TJSONObject;
  public
    constructor Create;
    destructor  Destroy; override;
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

constructor TDateTimeTool.Create;
begin
  inherited;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');
  FSchema.AddPair('properties', TJSONObject.Create);
end;

destructor TDateTimeTool.Destroy; begin FSchema.Free; inherited; end;
function TDateTimeTool.GetName: String; begin Result := 'get_datetime'; end;
function TDateTimeTool.GetDescription: String; begin Result := 'Returns the current date and time (ISO-8601)'; end;
function TDateTimeTool.GetCategory: String; begin Result := 'Utility'; end;
function TDateTimeTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TDateTimeTool.IsAvailable: Boolean; begin Result := True; end;

function TDateTimeTool.Execute(const AArgs: TJSONObject): TJSONObject;
var Dt: String;
begin
  Dt := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
  Writeln('  [get_datetime] => ', Dt);
  Result := TJSONObject.Create;
  Result.AddPair('datetime', Dt);
  Result.AddPair('timezone', 'local');
end;

// -----------------------------------------------------------------------------
// TTemperatureTool — conversor Celsius <-> Fahrenheit
// -----------------------------------------------------------------------------
type
  TTemperatureTool = class(TInterfacedObject, IAiTool)
  private
    FSchema: TJSONObject;
  public
    constructor Create;
    destructor  Destroy; override;
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

constructor TTemperatureTool.Create;
var
  Props, Val, Scale: TJSONObject;
  ScaleEnum: TJSONArray;
  Req: TJSONArray;
begin
  inherited;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Val := TJSONObject.Create; Val.AddPair('type', 'number'); Val.AddPair('description', 'Temperature value');
  Props.AddPair('value', Val);
  ScaleEnum := TJSONArray.Create; ScaleEnum.Add('C'); ScaleEnum.Add('F');
  Scale := TJSONObject.Create; Scale.AddPair('type', 'string'); Scale.AddPair('description', 'Source scale'); Scale.AddPair('enum', ScaleEnum);
  Props.AddPair('from_scale', Scale);
  ScaleEnum := TJSONArray.Create; ScaleEnum.Add('C'); ScaleEnum.Add('F');
  Scale := TJSONObject.Create; Scale.AddPair('type', 'string'); Scale.AddPair('description', 'Target scale'); Scale.AddPair('enum', ScaleEnum);
  Props.AddPair('to_scale', Scale);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('value'); Req.Add('from_scale'); Req.Add('to_scale');
  FSchema.AddPair('required', Req);
end;

destructor TTemperatureTool.Destroy; begin FSchema.Free; inherited; end;
function TTemperatureTool.GetName: String; begin Result := 'convert_temperature'; end;
function TTemperatureTool.GetDescription: String; begin Result := 'Converts temperatures between Celsius (C) and Fahrenheit (F)'; end;
function TTemperatureTool.GetCategory: String; begin Result := 'Utility'; end;
function TTemperatureTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TTemperatureTool.IsAvailable: Boolean; begin Result := True; end;

function TTemperatureTool.Execute(const AArgs: TJSONObject): TJSONObject;
var Val, Res: Double; From, ToSc: String;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<Double>('value', Val);
  AArgs.TryGetValue<String>('from_scale', From);
  AArgs.TryGetValue<String>('to_scale', ToSc);
  if (From = 'C') and (ToSc = 'F') then Res := Val * 9/5 + 32
  else if (From = 'F') and (ToSc = 'C') then Res := (Val - 32) * 5/9
  else Res := Val;
  Writeln(Format('  [convert_temperature] %.2f %s => %.2f %s', [Val, From, Res, ToSc]));
  Result := TJSONObject.Create;
  Result.AddPair('result', TJSONNumber.Create(Res));
  Result.AddPair('from_scale', From);
  Result.AddPair('to_scale', ToSc);
end;

// -----------------------------------------------------------------------------
// TWordCountTool — cuenta palabras en un texto
// -----------------------------------------------------------------------------
type
  TWordCountTool = class(TInterfacedObject, IAiTool)
  private
    FSchema: TJSONObject;
  public
    constructor Create;
    destructor  Destroy; override;
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

constructor TWordCountTool.Create;
var Props, Txt: TJSONObject; Req: TJSONArray;
begin
  inherited;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Txt := TJSONObject.Create; Txt.AddPair('type', 'string'); Txt.AddPair('description', 'Text to count words in');
  Props.AddPair('text', Txt);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('text');
  FSchema.AddPair('required', Req);
end;

destructor TWordCountTool.Destroy; begin FSchema.Free; inherited; end;
function TWordCountTool.GetName: String; begin Result := 'count_words'; end;
function TWordCountTool.GetDescription: String; begin Result := 'Counts the number of words in a given text'; end;
function TWordCountTool.GetCategory: String; begin Result := 'Text'; end;
function TWordCountTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TWordCountTool.IsAvailable: Boolean; begin Result := True; end;

function TWordCountTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Txt: String;
  Words: TArray<String>;
  Count: Integer;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('text', Txt);
  Words := Txt.Trim.Split([' ', #9, #13, #10], TStringSplitOptions.ExcludeEmpty);
  Count := Length(Words);
  Writeln(Format('  [count_words] "%s..." => %d words', [Copy(Txt, 1, 30), Count]));
  Result := TJSONObject.Create;
  Result.AddPair('word_count', TJSONNumber.Create(Count));
  Result.AddPair('char_count', TJSONNumber.Create(Length(Txt)));
end;

// =============================================================================
// DEMO PRINCIPAL
// =============================================================================

procedure RunQuery(Manager: TAIAgentManager; const AQuestion: String);
begin
  Manager.Blackboard.Clear;
  Writeln(StringOfChar('-', 60));
  Writeln('Pregunta: ', AQuestion);
  Writeln;
  try
    Writeln('Respuesta:', sLineBreak, Manager.Run(AQuestion));
  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;
  Writeln;
end;

procedure ShowBlackboard(Manager: TAIAgentManager);
begin
  Writeln('  [Blackboard] MultiToolsNode.output = "',
    Copy(Manager.Blackboard.GetString('MultiToolsNode.output'), 1, 80), '..."');
end;

procedure RunDemo;
var
  Manager  : TAIAgentManager;
  StartNode: TAIAgentsNode;
  LLMNode  : TLLMNode;
  EndNode  : TAIAgentsNode;
begin
  Writeln('=== Demo 03 — MultiTools ===');
  Writeln('4 herramientas: calculadora, datetime, temperatura, conteo de palabras');
  Writeln;

  // Registrar todas las herramientas
  TAiToolRegistry.Instance.Register(TCalculatorTool.Create,   'local', 'math');
  TAiToolRegistry.Instance.Register(TDateTimeTool.Create,     'local', 'utility');
  TAiToolRegistry.Instance.Register(TTemperatureTool.Create,  'local', 'utility');
  TAiToolRegistry.Instance.Register(TWordCountTool.Create,    'local', 'text');
  Writeln('Registry: ', TAiToolRegistry.Instance.Count, ' herramientas registradas.');
  Writeln;

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 90000;

    StartNode       := TAIAgentsNode.Create(Manager);
    StartNode.Name  := 'Start';
    StartNode.Graph := Manager;

    LLMNode              := TLLMNode.Create(Manager);
    LLMNode.Name         := 'MultiToolsNode';
    LLMNode.Graph        := Manager;
    LLMNode.DriverName   := DRIVER;
    LLMNode.Model        := MODEL;
    LLMNode.ApiKey       := API_KEY;
    LLMNode.UseAllTools  := True;
    LLMNode.SystemPrompt :=
      'You are a helpful assistant with tools. Use them whenever appropriate. ' +
      'For math always use calculator. For dates use get_datetime. ' +
      'For temperatures use convert_temperature. For word counts use count_words.';

    EndNode       := TAIAgentsNode.Create(Manager);
    EndNode.Name  := 'End';
    EndNode.Graph := Manager;

    Manager.AddEdge('Start', 'MultiToolsNode');
    Manager.AddEdge('MultiToolsNode', 'End');

    Manager.StartNode := StartNode;
    Manager.EndNode   := EndNode;
    Manager.Compile;

    // Pregunta 1: solo calculadora
    RunQuery(Manager, 'Cuanto es 347 por 28?');

    // Pregunta 2: solo datetime
    RunQuery(Manager, 'Que fecha y hora es ahora?');

    // Pregunta 3: dos herramientas (temperatura + calculadora)
    RunQuery(Manager, 'Cuanto es 100 grados Fahrenheit en Celsius? Y cuanto es ese valor al cuadrado?');

    // Pregunta 4: word count
    RunQuery(Manager, 'Cuantas palabras tiene esta frase: El rapido zorro marron salta sobre el perro perezoso');

    // Mostrar contenido del blackboard tras la ultima ejecucion
    Writeln('--- Blackboard ---');
    ShowBlackboard(Manager);

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
