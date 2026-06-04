program AgentMultiTools;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 07-AgenticLLM / 03-MultiTools
// =============================================================================
// Un agente con múltiples herramientas disponibles.
// El LLM elige qué herramienta(s) usar según la consulta.
//
// Conceptos que cubre:
//   - Múltiples herramientas en el mismo TLLMNode
//   - GetCategory: organizar herramientas por dominio
//   - Llamadas encadenadas: LLM puede usar varias tools en secuencia
//   - Tool result → LLM integra los resultados → respuesta final
//
// Herramientas: calculadora, fecha/hora, temperatura, URL encode
// Estructura: [Start] --> [MultiAgent] --> [End]
// =============================================================================

uses
  System.SysUtils,
  System.JSON,
  System.NetEncoding,
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
  private FSchema: TJSONObject;
  public
    constructor Create;
    destructor  Destroy; override;
    function GetName: String; function GetDescription: String;
    function GetCategory: String; function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

constructor TCalculatorTool.Create;
var Props, Op, Num: TJSONObject; Enum, Req: TJSONArray;
begin
  inherited;
  FSchema := TJSONObject.Create; FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Op := TJSONObject.Create; Op.AddPair('type', 'string');
  Enum := TJSONArray.Create; Enum.Add('add'); Enum.Add('subtract'); Enum.Add('multiply'); Enum.Add('divide');
  Op.AddPair('enum', Enum); Props.AddPair('operation', Op);
  Num := TJSONObject.Create; Num.AddPair('type', 'number'); Props.AddPair('a', Num);
  Num := TJSONObject.Create; Num.AddPair('type', 'number'); Props.AddPair('b', Num);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('operation'); Req.Add('a'); Req.Add('b');
  FSchema.AddPair('required', Req);
end;
destructor TCalculatorTool.Destroy; begin FSchema.Free; inherited; end;
function TCalculatorTool.GetName: String; begin Result := 'calculator'; end;
function TCalculatorTool.GetDescription: String; begin Result := 'Operaciones aritmeticas: add, subtract, multiply, divide'; end;
function TCalculatorTool.GetCategory: String; begin Result := 'Math'; end;
function TCalculatorTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TCalculatorTool.IsAvailable: Boolean; begin Result := True; end;

function TCalculatorTool.Execute(const AArgs: TJSONObject): TJSONObject;
var Op: String; A, B, Res: Double;
begin
  Result := nil; if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('operation', Op);
  AArgs.TryGetValue<Double>('a', A);
  AArgs.TryGetValue<Double>('b', B);
  Writeln(Format('  [calculator] %s(%g, %g)', [Op, A, B]));
  Res := 0;
  if Op = 'add' then Res := A + B
  else if Op = 'subtract' then Res := A - B
  else if Op = 'multiply' then Res := A * B
  else if Op = 'divide' then begin
    if B = 0 then begin Result := TJSONObject.Create; Result.AddPair('error', 'Division por cero'); Exit; end;
    Res := A / B;
  end;
  Writeln(Format('  [calculator] => %g', [Res]));
  Result := TJSONObject.Create; Result.AddPair('result', TJSONNumber.Create(Res));
end;

// =============================================================================
//  TDateTimeTool
// =============================================================================
type
  TDateTimeTool = class(TInterfacedObject, IAiTool)
  private FSchema: TJSONObject;
  public
    constructor Create; destructor Destroy; override;
    function GetName: String; function GetDescription: String;
    function GetCategory: String; function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

constructor TDateTimeTool.Create;
begin inherited; FSchema := TJSONObject.Create; FSchema.AddPair('type', 'object'); FSchema.AddPair('properties', TJSONObject.Create); end;
destructor TDateTimeTool.Destroy; begin FSchema.Free; inherited; end;
function TDateTimeTool.GetName: String; begin Result := 'get_datetime'; end;
function TDateTimeTool.GetDescription: String; begin Result := 'Retorna la fecha y hora actual en formato ISO-8601'; end;
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
  Result.AddPair('day_of_week', FormatDateTime('dddd', Now));
end;

// =============================================================================
//  TTemperatureTool
// =============================================================================
type
  TTemperatureTool = class(TInterfacedObject, IAiTool)
  private FSchema: TJSONObject;
  public
    constructor Create; destructor Destroy; override;
    function GetName: String; function GetDescription: String;
    function GetCategory: String; function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

constructor TTemperatureTool.Create;
var Props, Val, Scale: TJSONObject; ScaleEnum, Req: TJSONArray;
begin
  inherited; FSchema := TJSONObject.Create; FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Val := TJSONObject.Create; Val.AddPair('type', 'number'); Props.AddPair('value', Val);
  ScaleEnum := TJSONArray.Create; ScaleEnum.Add('C'); ScaleEnum.Add('F');
  Scale := TJSONObject.Create; Scale.AddPair('type', 'string'); Scale.AddPair('enum', ScaleEnum); Props.AddPair('from_scale', Scale);
  ScaleEnum := TJSONArray.Create; ScaleEnum.Add('C'); ScaleEnum.Add('F');
  Scale := TJSONObject.Create; Scale.AddPair('type', 'string'); Scale.AddPair('enum', ScaleEnum); Props.AddPair('to_scale', Scale);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('value'); Req.Add('from_scale'); Req.Add('to_scale');
  FSchema.AddPair('required', Req);
end;
destructor TTemperatureTool.Destroy; begin FSchema.Free; inherited; end;
function TTemperatureTool.GetName: String; begin Result := 'convert_temperature'; end;
function TTemperatureTool.GetDescription: String; begin Result := 'Convierte temperatura entre Celsius (C) y Fahrenheit (F)'; end;
function TTemperatureTool.GetCategory: String; begin Result := 'Utility'; end;
function TTemperatureTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TTemperatureTool.IsAvailable: Boolean; begin Result := True; end;

function TTemperatureTool.Execute(const AArgs: TJSONObject): TJSONObject;
var Val, Res: Double; From, ToSc: String;
begin
  Result := nil; if not Assigned(AArgs) then Exit;
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

// =============================================================================
//  TUrlEncodeTool
// =============================================================================
type
  TUrlEncodeTool = class(TInterfacedObject, IAiTool)
  private FSchema: TJSONObject;
  public
    constructor Create; destructor Destroy; override;
    function GetName: String; function GetDescription: String;
    function GetCategory: String; function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

constructor TUrlEncodeTool.Create;
var Props, Txt: TJSONObject; Req: TJSONArray;
begin
  inherited; FSchema := TJSONObject.Create; FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Txt := TJSONObject.Create; Txt.AddPair('type', 'string'); Txt.AddPair('description', 'Texto a codificar como URL'); Props.AddPair('text', Txt);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('text'); FSchema.AddPair('required', Req);
end;
destructor TUrlEncodeTool.Destroy; begin FSchema.Free; inherited; end;
function TUrlEncodeTool.GetName: String; begin Result := 'url_encode'; end;
function TUrlEncodeTool.GetDescription: String; begin Result := 'Codifica un texto como URL-safe (percent-encoding)'; end;
function TUrlEncodeTool.GetCategory: String; begin Result := 'Text'; end;
function TUrlEncodeTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TUrlEncodeTool.IsAvailable: Boolean; begin Result := True; end;

function TUrlEncodeTool.Execute(const AArgs: TJSONObject): TJSONObject;
var Txt, Encoded: String;
begin
  Result := nil; if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('text', Txt);
  Encoded := TNetEncoding.URL.Encode(Txt);
  Writeln(Format('  [url_encode] "%s" => "%s"', [Copy(Txt, 1, 30), Copy(Encoded, 1, 40)]));
  Result := TJSONObject.Create;
  Result.AddPair('encoded', Encoded);
  Result.AddPair('original', Txt);
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
    Writeln('Respuesta:', sLineBreak, Manager.Run(AQuestion));
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
  Writeln('=== AgentMultiTools ===');
  Writeln('4 herramientas: calculator, datetime, temperature, url_encode');
  Writeln;

  // Registrar herramientas en el registry global
  TAiToolRegistry.Instance.Register(TCalculatorTool.Create,   'local', 'math');
  TAiToolRegistry.Instance.Register(TDateTimeTool.Create,     'local', 'utility');
  TAiToolRegistry.Instance.Register(TTemperatureTool.Create,  'local', 'utility');
  TAiToolRegistry.Instance.Register(TUrlEncodeTool.Create,    'local', 'text');
  Writeln('Herramientas registradas: ', TAiToolRegistry.Instance.Count);
  Writeln;

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 90000;

    Start       := TAIAgentsNode.Create(Manager);
    Start.Name  := 'Start';
    Start.Graph := Manager;

    LLMAgent              := TLLMNode.Create(Manager);
    LLMAgent.Name         := 'MultiAgent';
    LLMAgent.Graph        := Manager;
    LLMAgent.DriverName   := DRIVER;
    LLMAgent.Model        := MODEL;
    LLMAgent.ApiKey       := API_KEY;
    LLMAgent.UseAllTools  := True;
    LLMAgent.SystemPrompt :=
      'Eres un asistente util con herramientas. Usaias cuando sea apropiado. ' +
      'Para matematicas usa calculator. Para fechas usa get_datetime. ' +
      'Para temperaturas usa convert_temperature. Para URLs usa url_encode. ' +
      'Responde en espanol.';

    Finish       := TAIAgentsNode.Create(Manager);
    Finish.Name  := 'End';
    Finish.Graph := Manager;

    Manager.AddEdge('Start',      'MultiAgent');
    Manager.AddEdge('MultiAgent', 'End');

    Manager.StartNode := Start;
    Manager.EndNode   := Finish;
    Manager.Compile;

    // Cada pregunta requiere una herramienta diferente
    RunQuery(Manager, 'Cuanto es 347 por 28?');
    RunQuery(Manager, 'Que fecha y hora es ahora?');
    RunQuery(Manager, 'Cuanto es 37 grados Celsius en Fahrenheit?');
    RunQuery(Manager, 'Codifica esta URL: busqueda de inteligencia artificial');
    // Esta requiere dos herramientas: temperatura + calculadora
    RunQuery(Manager, 'Cuanto es 100 Fahrenheit en Celsius? Y cuanto es ese valor al cuadrado?');

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
