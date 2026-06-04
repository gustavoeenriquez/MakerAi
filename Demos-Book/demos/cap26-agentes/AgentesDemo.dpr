program AgentesDemo;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI -- Capitulo 26: Construyendo Agentes con TLLMNode
// =============================================================================
// Dos escenarios de orquestacion con TAIAgentManager:
//
// Escenario A -- Sin TLLMNode: pipeline manual con TAIAgentsNode + OnExecute.
//   Cada nodo invoca TAiChatConnection directamente dentro del handler.
//   Los handlers son metodos de clase porque OnExecute es "of object".
//   Muestra que nodos pueden mezclar logica LLM y logica ordinaria.
//
// Escenario B -- Con TLLMNode: agente autonomo con herramientas IAiTool.
//   TLLMNode maneja el loop ReAct (Razonar->Actuar->Observar) internamente.
//   Las herramientas se implementan como TInterfacedObject + IAiTool.
//   TAiToolRegistry es el catalogo central de herramientas.
//
// Driver: Claude (CLAUDE_API_KEY). Cambiar DRV/MDL/APIKEY para otro provider.
// =============================================================================

uses
  System.SysUtils, System.Classes, System.JSON, System.Math,
  uMakerAi.Agents,               // TAIAgentManager, TAIAgentsNode, TAIBlackboard
  uMakerAi.Agents.Node.LLM,     // TLLMNode
  uMakerAi.Agents.IAiTool,      // IAiTool
  uMakerAi.Agents.ToolRegistry, // TAiToolRegistry
  uMakerAi.Chat.AiConnection,   // TAiChatConnection (conector universal)
  uMakerAi.Chat.Initializations; // auto-registra todos los drivers

// ---------------------------------------------------------------------------
//  Configuracion del provider
// ---------------------------------------------------------------------------

const
  DRV    = 'Claude';
  MDL    = 'claude-haiku-4-5-20251001';
  APIKEY = '@CLAUDE_API_KEY';

// Texto de prueba para el pipeline editorial
const
  TEXTO_PRUEBA =
    'Los modelos de lenguaje grande estan transformando el desarrollo de ' +
    'software empresarial. Frameworks como MakerAI permiten a los equipos ' +
    'de Delphi integrar inteligencia artificial sin abandonar su stack ' +
    'tecnologico existente. La combinacion de componentes FMX con agentes ' +
    'autonomos abre posibilidades nuevas para la automatizacion de procesos.';

// ---------------------------------------------------------------------------
//  Helpers de presentacion
// ---------------------------------------------------------------------------

procedure Separador(const Titulo: string);
begin
  WriteLn;
  WriteLn(StringOfChar('-', 64));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 64));
end;

// ===========================================================================
//  ESCENARIO A -- Pipeline manual sin TLLMNode
// ===========================================================================
//  Clase wrapper requerida porque TAIAgentsNodeOnExecute es "of object":
//  los handlers deben ser metodos de una instancia de clase.
//
//  Grafo: Analizar --> Resumir --> Formatear
//    Nodo 1 (Analizar):  llama al LLM para detectar tema y tono
//    Nodo 2 (Resumir):   llama al LLM para generar resumen ejecutivo
//    Nodo 3 (Formatear): logica local -- no necesita LLM
//
//  TAIAgentManager.Asynchronous := False --> Manager.Run() bloquea.
// ===========================================================================

type
  TDemoA = class
  private
    FDriver : String;
    FModel  : String;
    FApiKey : String;
    // Llamada LLM sincronica -- segura desde worker threads del manager
    function LlmCall(const AInput, ASystem: String): String;
    // Handlers "of object" -- uno por nodo del pipeline
    procedure OnAnalizar(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
      Input: String; var Output: String);
    procedure OnResumir(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
      Input: String; var Output: String);
    procedure OnFormatear(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
      Input: String; var Output: String);
  public
    constructor Create(const ADriver, AModel, AApiKey: String);
    function Run(const ATexto: String): String;
  end;

{ TDemoA }

constructor TDemoA.Create(const ADriver, AModel, AApiKey: String);
begin
  FDriver := ADriver;
  FModel  := AModel;
  FApiKey := AApiKey;
end;

function TDemoA.LlmCall(const AInput, ASystem: String): String;
var
  Chat: TAiChatConnection;
begin
  Chat := TAiChatConnection.Create(nil);
  try
    Chat.DriverName                    := FDriver;
    Chat.Model                         := FModel;
    Chat.Params.Values['ApiKey']       := FApiKey;
    Chat.Params.Values['Asynchronous'] := 'False'; // sincrono en worker thread
    Chat.SystemPrompt.Text             := ASystem;
    Result := Chat.AddMessageAndRun(AInput, 'user', []);
  finally
    Chat.Free;
  end;
end;

procedure TDemoA.OnAnalizar(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
  Input: String; var Output: String);
begin
  WriteLn('    [Analizar] identificando tema y tono...');
  Output := LlmCall(
    'Analiza el siguiente texto. Identifica en 3 lineas: ' +
    '1) Tema principal, 2) Tono (formal o informal), 3) Audiencia objetivo.' +
    #13#10 + 'Texto: ' + Input,
    'Eres un analista editorial experto. Responde en espanol, maximo 3 lineas.');
end;

procedure TDemoA.OnResumir(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
  Input: String; var Output: String);
begin
  WriteLn('    [Resumir] generando resumen ejecutivo...');
  Output := LlmCall(
    'A partir del siguiente analisis editorial, escribe un resumen ejecutivo ' +
    'de exactamente 2 oraciones que capture la esencia del contenido.' +
    #13#10 + 'Analisis: ' + Input,
    'Eres un redactor experto. Sé preciso. Responde en espanol.');
end;

procedure TDemoA.OnFormatear(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
  Input: String; var Output: String);
begin
  WriteLn('    [Formatear] aplicando estructura final (sin LLM)...');
  // Este nodo usa logica local -- muestra que los nodos pueden mezclar LLM y codigo
  Output :=
    StringOfChar('=', 50) + sLineBreak +
    '  RESUMEN EDITORIAL' + sLineBreak +
    StringOfChar('=', 50) + sLineBreak +
    Input + sLineBreak +
    StringOfChar('-', 50);
end;

function TDemoA.Run(const ATexto: String): String;
var
  Manager: TAIAgentManager;
begin
  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;   // Manager.Run() bloquea hasta completar
    Manager
      .AddNode('Analizar',  OnAnalizar)
      .AddNode('Resumir',   OnResumir)
      .AddNode('Formatear', OnFormatear)
      .AddEdge('Analizar',  'Resumir')
      .AddEdge('Resumir',   'Formatear')
      .SetEntryPoint('Analizar')
      .SetFinishPoint('Formatear')
      .Compile;
    Result := Manager.Run(ATexto);
  finally
    Manager.Free;
  end;
end;

// ===========================================================================
//  ESCENARIO B -- Herramientas IAiTool para TLLMNode
// ===========================================================================
//  Cada herramienta implementa IAiTool como TInterfacedObject.
//  Contrato de memoria (definido en uMakerAi.Agents.IAiTool):
//    GetSchema --> NO liberar; el TJSONObject es propiedad de la herramienta
//    Execute   --> el llamador libera el TJSONObject retornado
// ===========================================================================

type
  // Herramienta 1: Fecha y hora del sistema (sin parametros)
  TFechaHoraTool = class(TInterfacedObject, IAiTool)
  private
    FSchema: TJSONObject;
  public
    destructor Destroy; override;
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

  // Herramienta 2: Calculadora aritmetica (a, b, operacion)
  TCalculadoraTool = class(TInterfacedObject, IAiTool)
  private
    FSchema: TJSONObject;
  public
    destructor Destroy; override;
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

{ TFechaHoraTool }

destructor TFechaHoraTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TFechaHoraTool.GetName: String;
begin
  Result := 'FechaHoraActual';
end;

function TFechaHoraTool.GetDescription: String;
begin
  Result := 'Obtiene la fecha y hora actuales del sistema';
end;

function TFechaHoraTool.GetCategory: String;
begin
  Result := 'Sistema';
end;

function TFechaHoraTool.GetSchema: TJSONObject;
begin
  if FSchema = nil then
  begin
    FSchema := TJSONObject.Create;
    FSchema.AddPair('type', 'object');
    FSchema.AddPair('properties', TJSONObject.Create); // sin parametros
  end;
  Result := FSchema;
end;

function TFechaHoraTool.Execute(const AArgs: TJSONObject): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('fecha_hora', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
end;

function TFechaHoraTool.IsAvailable: Boolean;
begin
  Result := True;
end;

{ TCalculadoraTool }

destructor TCalculadoraTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TCalculadoraTool.GetName: String;
begin
  Result := 'Calculadora';
end;

function TCalculadoraTool.GetDescription: String;
begin
  Result := 'Calcula operaciones aritmeticas basicas entre dos numeros';
end;

function TCalculadoraTool.GetCategory: String;
begin
  Result := 'Matematica';
end;

function TCalculadoraTool.GetSchema: TJSONObject;
var
  Props, AProp, BProp, OpProp: TJSONObject;
  EnumArr, Required: TJSONArray;
begin
  if FSchema = nil then
  begin
    FSchema := TJSONObject.Create;
    FSchema.AddPair('type', 'object');

    Props := TJSONObject.Create;

    AProp := TJSONObject.Create;
    AProp.AddPair('type', 'number');
    AProp.AddPair('description', 'Primer operando');
    Props.AddPair('a', AProp);

    BProp := TJSONObject.Create;
    BProp.AddPair('type', 'number');
    BProp.AddPair('description', 'Segundo operando');
    Props.AddPair('b', BProp);

    EnumArr := TJSONArray.Create;
    EnumArr.Add('sumar');
    EnumArr.Add('restar');
    EnumArr.Add('multiplicar');
    EnumArr.Add('dividir');

    OpProp := TJSONObject.Create;
    OpProp.AddPair('type', 'string');
    OpProp.AddPair('description', 'Operacion aritmetica a realizar');
    OpProp.AddPair('enum', EnumArr);
    Props.AddPair('operacion', OpProp);

    FSchema.AddPair('properties', Props);

    Required := TJSONArray.Create;
    Required.Add('a');
    Required.Add('b');
    Required.Add('operacion');
    FSchema.AddPair('required', Required);
  end;
  Result := FSchema;
end;

function TCalculadoraTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  A, B, Res: Double;
  Op: String;
begin
  Result := TJSONObject.Create;
  if not Assigned(AArgs) then
  begin
    Result.AddPair('error', 'Argumentos requeridos');
    Exit;
  end;
  try
    A  := AArgs.GetValue<Double>('a');
    B  := AArgs.GetValue<Double>('b');
    Op := AArgs.GetValue<String>('operacion').ToLower.Trim;
  except
    Result.AddPair('error', 'Parametros invalidos');
    Exit;
  end;

  if      Op = 'sumar'       then Res := A + B
  else if Op = 'restar'      then Res := A - B
  else if Op = 'multiplicar' then Res := A * B
  else if Op = 'dividir' then
  begin
    if B = 0 then
    begin
      Result.AddPair('error', 'Division por cero');
      Exit;
    end;
    Res := A / B;
  end
  else
  begin
    Result.AddPair('error', 'Operacion desconocida: ' + Op);
    Exit;
  end;

  Result.AddPair('resultado', TJSONNumber.Create(Res));
end;

function TCalculadoraTool.IsAvailable: Boolean;
begin
  Result := True;
end;

// ---------------------------------------------------------------------------
//  Demo A -- Pipeline editorial de 3 nodos
// ---------------------------------------------------------------------------

procedure DemoA;
var
  Demo: TDemoA;
  Resultado: String;
begin
  Separador('Demo A -- Pipeline editorial sin TLLMNode (3 nodos en secuencia)');
  WriteLn('  Texto: "', Copy(TEXTO_PRUEBA, 1, 58), '..."');
  WriteLn('  Grafo: Analizar --> Resumir --> Formatear');
  WriteLn;

  Demo := TDemoA.Create(DRV, MDL, APIKEY);
  try
    Resultado := Demo.Run(TEXTO_PRUEBA);
    WriteLn;
    WriteLn(Resultado);
  finally
    Demo.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  Demo B -- TLLMNode con herramientas IAiTool
// ---------------------------------------------------------------------------

procedure DemoB;
var
  Manager : TAIAgentManager;
  Node    : TLLMNode;
  P, R    : String;
  Preguntas: array[0..1] of string;
begin
  Separador('Demo B -- TLLMNode con herramientas IAiTool (agente autonomo)');

  // 1. Registrar herramientas en el singleton global
  TAiToolRegistry.Instance.Register(TFechaHoraTool.Create,  'local');
  TAiToolRegistry.Instance.Register(TCalculadoraTool.Create, 'local');
  WriteLn(Format('  Herramientas en registry: %d', [TAiToolRegistry.Instance.Count]));
  WriteLn;

  Manager := TAIAgentManager.Create(nil);
  try
    Manager.Asynchronous := False;

    // 2. Crear TLLMNode como componente owned por el manager.
    //    Al usar Manager como owner, el nodo se auto-registra en la lista
    //    de componentes via el mecanismo de Notification de TComponent.
    Node := TLLMNode.Create(Manager);
    Node.Name         := 'Agente';
    Node.Graph        := Manager;    // necesario para Blackboard y link traversal
    Node.DriverName   := DRV;
    Node.ApiKey       := APIKEY;
    Node.Model        := MDL;
    Node.SystemPrompt :=
      'Eres un asistente con acceso a herramientas. ' +
      'Para la fecha/hora actual usa FechaHoraActual. ' +
      'Para calculos aritmeticos usa Calculadora. ' +
      'Muestra el resultado de forma clara y concisa.';
    Node.UseAllTools  := True;   // inyecta todo el TAiToolRegistry.Instance

    Manager
      .SetEntryPoint('Agente')
      .SetFinishPoint('Agente')
      .Compile;

    // 3. Consultas que requieren el uso de herramientas
    Preguntas[0] := 'Cual es la fecha y hora actual del sistema?';
    Preguntas[1] := 'Cuanto es 1247 multiplicado por 38? Y ese resultado dividido entre 7?';

    for P in Preguntas do
    begin
      WriteLn('  Pregunta: ', P);
      R := Manager.Run(P);
      WriteLn('  Respuesta: ', R);
      WriteLn;
    end;

  finally
    TAiToolRegistry.Instance.Clear;   // limpiar el registry al terminar el demo
    Manager.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  Programa principal
// ---------------------------------------------------------------------------

begin
  WriteLn('=== MakerAI -- Capitulo 26: Construyendo Agentes con TLLMNode ===');
  WriteLn;
  WriteLn('  Demo A: TAIAgentsNode + OnExecute manual (code-first, sin TLLMNode)');
  WriteLn('  Demo B: TLLMNode + IAiTool (component-first, agente autonomo)');
  WriteLn;
  WriteLn('  Driver: ', DRV, '  Modelo: ', MDL);
  WriteLn('  API Key: ', APIKEY, '  (variable de entorno CLAUDE_API_KEY)');

  try
    DemoA;
    DemoB;

    WriteLn;
    WriteLn(StringOfChar('=', 64));
    WriteLn('  Demos completados.');
    WriteLn(StringOfChar('=', 64));
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('[ERROR] ', E.ClassName, ': ', E.Message);
    end;
  end;

  WriteLn;
  Write('Presiona Enter para salir...');
  ReadLn;
end.
