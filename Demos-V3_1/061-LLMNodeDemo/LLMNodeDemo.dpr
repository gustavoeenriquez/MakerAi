program LLMNodeDemo;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - Demo 061: TLLMNode con TAiToolRegistry
// =============================================================================
// Demuestra la Phase 2 del rediseno de agentes: un nodo TLLMNode que usa el
// loop ReAct integrado para resolver preguntas que requieren herramientas.
//
// Herramientas registradas en este demo:
//   - Calculadora: suma, resta, multiplicacion, division
//   - Fecha y hora actual
//   - Conversor de temperaturas: Celsius <-> Fahrenheit
//
// Configura DRIVER / MODEL / API_KEY antes de compilar.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.DateUtils,
  uMakerAi.Agents,
  uMakerAi.Agents.IAiTool,
  uMakerAi.Agents.ToolRegistry,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations, // parametros de modelos
  // ── Importar los drivers que se vayan a usar ────────────────────────────
  // Cada unit registra su driver en initialization via TAiChatFactory.
  // Sin este import el driver no se linkea y TAiChatConnection falla.
  uMakerAi.Chat.Claude,          // 'Claude'
  uMakerAi.Chat.OpenAi,          // 'OpenAI'
  uMakerAi.Chat.Ollama,          // 'Ollama'
  uMakerAi.Chat.Gemini,          // 'Gemini'
  uMakerAi.Chat.Groq;            // 'Groq'

const
  // ── Cambiar segun el proveedor disponible ──────────────────────────────────
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';          // resuelto via GetEnvironmentVariable
  // ──────────────────────────────────────────────────────────────────────────

// =============================================================================
//  HERRAMIENTAS DEMO
// =============================================================================

// -----------------------------------------------------------------------------
//  TCalculatorTool
//  Realiza operaciones aritmeticas simples.
//  Args: { "operation": "multiply"|"add"|"subtract"|"divide", "a": N, "b": N }
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
  Req: TJSONArray;
begin
  inherited Create;
  // Construir inputSchema (formato OpenAI)
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');

  Props := TJSONObject.Create;

  Op := TJSONObject.Create;
  Op.AddPair('type', 'string');
  Op.AddPair('description', 'Operation to perform');
  var Enum := TJSONArray.Create;
  Enum.Add('add');
  Enum.Add('subtract');
  Enum.Add('multiply');
  Enum.Add('divide');
  Op.AddPair('enum', Enum);
  Props.AddPair('operation', Op);

  Num := TJSONObject.Create;
  Num.AddPair('type', 'number');
  Num.AddPair('description', 'First number');
  Props.AddPair('a', Num);

  Num := TJSONObject.Create;
  Num.AddPair('type', 'number');
  Num.AddPair('description', 'Second number');
  Props.AddPair('b', Num);

  FSchema.AddPair('properties', Props);

  Req := TJSONArray.Create;
  Req.Add('operation');
  Req.Add('a');
  Req.Add('b');
  FSchema.AddPair('required', Req);
end;

destructor TCalculatorTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TCalculatorTool.GetName: String;        begin Result := 'calculator';               end;
function TCalculatorTool.GetDescription: String;  begin Result := 'Performs arithmetic operations (add, subtract, multiply, divide) on two numbers'; end;
function TCalculatorTool.GetCategory: String;     begin Result := 'Math';                    end;
function TCalculatorTool.GetSchema: TJSONObject;  begin Result := FSchema;                   end;
function TCalculatorTool.IsAvailable: Boolean;    begin Result := True;                      end;

function TCalculatorTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Op   : String;
  A, B : Double;
  Res  : Double;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;

  AArgs.TryGetValue<String>('operation', Op);
  AArgs.TryGetValue<Double>('a', A);
  AArgs.TryGetValue<Double>('b', B);

  Writeln(Format('  [Tool] calculator(%s, %.4g, %.4g)', [Op, A, B]));

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

  Writeln(Format('  [Tool] → %.6g', [Res]));

  Result := TJSONObject.Create;
  Result.AddPair('result', TJSONNumber.Create(Res));
end;

// -----------------------------------------------------------------------------
//  TCurrentDateTimeTool
//  Devuelve la fecha y hora actuales en ISO-8601.
//  Args: {} (sin argumentos)
// -----------------------------------------------------------------------------
type
  TCurrentDateTimeTool = class(TInterfacedObject, IAiTool)
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

constructor TCurrentDateTimeTool.Create;
begin
  inherited Create;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');
  FSchema.AddPair('properties', TJSONObject.Create);
end;

destructor TCurrentDateTimeTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TCurrentDateTimeTool.GetName: String;        begin Result := 'get_current_datetime'; end;
function TCurrentDateTimeTool.GetDescription: String;  begin Result := 'Returns the current date and time in ISO-8601 format';  end;
function TCurrentDateTimeTool.GetCategory: String;     begin Result := 'Utility';              end;
function TCurrentDateTimeTool.GetSchema: TJSONObject;  begin Result := FSchema;                end;
function TCurrentDateTimeTool.IsAvailable: Boolean;    begin Result := True;                   end;

function TCurrentDateTimeTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Dt: String;
begin
  Dt := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
  Writeln(Format('  [Tool] get_current_datetime → %s', [Dt]));
  Result := TJSONObject.Create;
  Result.AddPair('datetime', Dt);
  Result.AddPair('timezone', 'local');
end;

// -----------------------------------------------------------------------------
//  TTempConverterTool
//  Convierte temperaturas entre Celsius y Fahrenheit.
//  Args: { "value": N, "from": "C"|"F", "to": "C"|"F" }
// -----------------------------------------------------------------------------
type
  TTempConverterTool = class(TInterfacedObject, IAiTool)
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

constructor TTempConverterTool.Create;
var
  Props, Val, Scale: TJSONObject;
  Req: TJSONArray;
  ScaleEnum: TJSONArray;
begin
  inherited Create;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');

  Props := TJSONObject.Create;

  Val := TJSONObject.Create;
  Val.AddPair('type', 'number');
  Val.AddPair('description', 'Temperature value to convert');
  Props.AddPair('value', Val);

  ScaleEnum := TJSONArray.Create;
  ScaleEnum.Add('C');
  ScaleEnum.Add('F');

  Scale := TJSONObject.Create;
  Scale.AddPair('type', 'string');
  Scale.AddPair('description', 'Source scale (C=Celsius, F=Fahrenheit)');
  Scale.AddPair('enum', ScaleEnum);
  Props.AddPair('from_scale', Scale);

  ScaleEnum := TJSONArray.Create;
  ScaleEnum.Add('C');
  ScaleEnum.Add('F');

  Scale := TJSONObject.Create;
  Scale.AddPair('type', 'string');
  Scale.AddPair('description', 'Target scale (C=Celsius, F=Fahrenheit)');
  Scale.AddPair('enum', ScaleEnum);
  Props.AddPair('to_scale', Scale);

  FSchema.AddPair('properties', Props);

  Req := TJSONArray.Create;
  Req.Add('value');
  Req.Add('from_scale');
  Req.Add('to_scale');
  FSchema.AddPair('required', Req);
end;

destructor TTempConverterTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TTempConverterTool.GetName: String;        begin Result := 'convert_temperature';   end;
function TTempConverterTool.GetDescription: String;  begin Result := 'Converts temperatures between Celsius (C) and Fahrenheit (F)'; end;
function TTempConverterTool.GetCategory: String;     begin Result := 'Utility';               end;
function TTempConverterTool.GetSchema: TJSONObject;  begin Result := FSchema;                 end;
function TTempConverterTool.IsAvailable: Boolean;    begin Result := True;                    end;

function TTempConverterTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Val  : Double;
  From : String;
  ToSc : String;
  Res  : Double;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;

  AArgs.TryGetValue<Double>('value',      Val);
  AArgs.TryGetValue<String>('from_scale', From);
  AArgs.TryGetValue<String>('to_scale',   ToSc);

  if (From = 'C') and (ToSc = 'F') then
    Res := Val * 9 / 5 + 32
  else if (From = 'F') and (ToSc = 'C') then
    Res := (Val - 32) * 5 / 9
  else
    Res := Val;  // misma escala

  Writeln(Format('  [Tool] convert_temperature(%.2f %s → %s) → %.2f', [Val, From, ToSc, Res]));

  Result := TJSONObject.Create;
  Result.AddPair('result', TJSONNumber.Create(Res));
  Result.AddPair('from_scale', From);
  Result.AddPair('to_scale', ToSc);
end;

// =============================================================================
//  DEMO PRINCIPAL
// =============================================================================

procedure RunQuery(AManager: TAIAgentManager; const AQuestion: String);
var
  Response: String;
begin
  Writeln;
  Writeln(StringOfChar('-', 70));
  Writeln('Pregunta: ', AQuestion);
  Writeln(StringOfChar('-', 70));
  try
    Response := AManager.Run(AQuestion);
    Writeln;
    Writeln('Respuesta: ', Response);
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
end;

procedure RunDemo;
var
  Registry : TAiToolRegistry;
  Manager  : TAIAgentManager;
  LLMNode  : TLLMNode;
begin
  Writeln('=== Demo 061 — TLLMNode + TAiToolRegistry ===');
  Writeln('Driver : ', DRIVER);
  Writeln('Model  : ', MODEL);
  Writeln;

  // ── 1. Registrar herramientas en el registry global ─────────────────────
  Registry := TAiToolRegistry.Instance;
  Registry.Register(TCalculatorTool.Create,    'local', 'demo');
  Registry.Register(TCurrentDateTimeTool.Create,'local', 'demo');
  Registry.Register(TTempConverterTool.Create,  'local', 'demo');

  Writeln(Format('Registry: %d herramientas registradas.', [Registry.Count]));
  Writeln;

  // ── 2. Crear el agente ───────────────────────────────────────────────────
  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 120000;

    // ── 3. Crear y configurar el TLLMNode ──────────────────────────────────
    LLMNode := TLLMNode.Create(Manager);    // Manager = Owner (auto-add)
    LLMNode.Name         := 'ReActAgent';
    LLMNode.Graph        := Manager;
    LLMNode.DriverName   := DRIVER;
    LLMNode.Model        := MODEL;
    LLMNode.ApiKey       := API_KEY;
    LLMNode.UseAllTools  := True;           // usa todo el Registry global
    LLMNode.SystemPrompt :=
      'You are a helpful assistant with access to tools. ' +
      'When needed, use the tools to give accurate, precise answers. ' +
      'Always use the calculator for math, never guess numerical results.';

    // Grafo mínimo: un solo nodo que es inicio y fin
    Manager.StartNode := LLMNode;
    Manager.EndNode   := LLMNode;
    Manager.Compile;

    // ── 4. Ejecutar consultas ──────────────────────────────────────────────
    RunQuery(Manager, '¿Cuánto es 347 multiplicado por 28?');
    RunQuery(Manager, '¿Qué fecha y hora es ahora?');
    RunQuery(Manager, '¿Cuánto es 100°F en Celsius? ¿Y 37°C en Fahrenheit?');
    RunQuery(Manager,
      '¿Cuánto es (15 + 27) * 4? Y también, ¿cuánto es 1000 / 8? ' +
      'Dame ambos resultados.');

  finally
    Manager.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
