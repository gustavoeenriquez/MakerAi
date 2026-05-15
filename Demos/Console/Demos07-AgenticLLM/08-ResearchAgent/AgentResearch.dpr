program AgentResearch;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — 07-AgenticLLM / 08-ResearchAgent
// =============================================================================
// Agente de investigacion multi-hop con herramientas especializadas:
//
//   [Planner] -> [Researcher (web_search + save_note)] -> [Analyst] -> [Writer]
//
// El Researcher usa un registry propio con dos herramientas:
//   - web_search: busqueda mock con datos predefinidos por palabra clave
//   - save_note:  persiste hallazgos en el blackboard por clave
//
// Conceptos que cubre:
//   - TLLMNode.Registry: registry de herramientas propio por nodo
//   - TAiToolRegistry.Create (instancia propia, no singleton)
//   - IAiTool con acceso al blackboard (TNoteTool recibe referencia)
//   - Pipeline multi-nodo donde el output de cada nodo alimenta al siguiente
//   - Inspeccion del blackboard post-run (Planner.output, note.findingX)
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
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
// TMockSearchTool — busqueda web simulada con datos predefinidos
// Args: { "query": string, "max_results": number (opcional) }
// -----------------------------------------------------------------------------
type
  TMockSearchTool = class(TInterfacedObject, IAiTool)
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

constructor TMockSearchTool.Create;
var Props, Q, MaxR: TJSONObject; Req: TJSONArray;
begin
  inherited;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Q := TJSONObject.Create; Q.AddPair('type', 'string'); Q.AddPair('description', 'Search query');
  Props.AddPair('query', Q);
  MaxR := TJSONObject.Create; MaxR.AddPair('type', 'number'); MaxR.AddPair('description', 'Maximum results (default 3)');
  Props.AddPair('max_results', MaxR);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('query');
  FSchema.AddPair('required', Req);
end;

destructor TMockSearchTool.Destroy; begin FSchema.Free; inherited; end;
function TMockSearchTool.GetName       : String; begin Result := 'web_search'; end;
function TMockSearchTool.GetDescription: String; begin Result := 'Searches the web for information. Returns relevant snippets.'; end;
function TMockSearchTool.GetCategory   : String; begin Result := 'Research'; end;
function TMockSearchTool.GetSchema     : TJSONObject; begin Result := FSchema; end;
function TMockSearchTool.IsAvailable   : Boolean; begin Result := True; end;

function TMockSearchTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Query  : String;
  Results: TJSONArray;
  Snippet: TJSONObject;
  QLower : String;

  procedure AddResult(const ATitle, ASnippet: String);
  var S: TJSONObject;
  begin
    S := TJSONObject.Create;
    S.AddPair('title',   ATitle);
    S.AddPair('snippet', ASnippet);
    Results.Add(S);
  end;

begin
  Result := nil;
  if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('query', Query);
  QLower := LowerCase(Query);
  Writeln(Format('  [web_search] query="%s"', [Query]));

  Results := TJSONArray.Create;

  if (Pos('climate', QLower) > 0) or (Pos('global warming', QLower) > 0) then
  begin
    AddResult('Climate Change Overview',
      'Global temperatures have risen 1.1C since pre-industrial times. CO2 at 421 ppm (2023).');
    AddResult('IPCC Report 2023',
      'Without action, warming could reach 2.5-3C by 2100. Renewable energy must triple by 2030.');
    AddResult('Economic Impact',
      'Climate change could reduce global GDP by 10-23% by 2100. Green transition creates 24M jobs.');
  end
  else if (Pos('artificial intelligence', QLower) > 0) or (Pos(' ai ', QLower) > 0)
       or (Pos('machine learning', QLower) > 0) then
  begin
    AddResult('AI Market Growth',
      'Global AI market at $207B in 2023, expected to reach $1.8T by 2030.');
    AddResult('AI Applications',
      'AI used in healthcare, finance (fraud detection), manufacturing, and autonomous vehicles.');
    AddResult('AI Risks',
      'Key concerns: job displacement (14% at high risk), bias in models, and privacy issues.');
  end
  else if Pos('quantum', QLower) > 0 then
  begin
    AddResult('Quantum Computing Basics',
      'Quantum computers use qubits. IBM has 1000+ qubit processors. Google achieved supremacy in 2019.');
    AddResult('Quantum Applications',
      'Potential uses: cryptography, drug discovery, financial optimization, materials science.');
  end
  else
  begin
    AddResult('General Information',
      Format('Research on "%s": Complex topic with multiple dimensions. Significant developments in 2023-2024.', [Query]));
    AddResult('Recent Developments',
      Format('Latest findings on "%s" show growing research interest from academia and industry.', [Query]));
  end;

  Writeln(Format('  [web_search] => %d resultados', [Results.Count]));
  Result := TJSONObject.Create;
  Result.AddPair('results', Results);
  Result.AddPair('count',   TJSONNumber.Create(Results.Count));
  Result.AddPair('query',   Query);
end;

// -----------------------------------------------------------------------------
// TNoteTool — guarda notas de investigacion en el blackboard
// Args: { "key": string, "content": string }
// -----------------------------------------------------------------------------
type
  TNoteTool = class(TInterfacedObject, IAiTool)
  private
    FSchema    : TJSONObject;
    FBlackboard: TAIBlackboard;
  public
    constructor Create(ABlackboard: TAIBlackboard);
    destructor  Destroy; override;
    function GetName       : String;
    function GetDescription: String;
    function GetCategory   : String;
    function GetSchema     : TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable   : Boolean;
  end;

constructor TNoteTool.Create(ABlackboard: TAIBlackboard);
var Props, K, C: TJSONObject; Req: TJSONArray;
begin
  inherited Create;
  FBlackboard := ABlackboard;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  K := TJSONObject.Create; K.AddPair('type', 'string'); K.AddPair('description', 'Note identifier key');
  Props.AddPair('key', K);
  C := TJSONObject.Create; C.AddPair('type', 'string'); C.AddPair('description', 'Note content to save');
  Props.AddPair('content', C);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('key'); Req.Add('content');
  FSchema.AddPair('required', Req);
end;

destructor TNoteTool.Destroy; begin FSchema.Free; inherited; end;
function TNoteTool.GetName       : String; begin Result := 'save_note'; end;
function TNoteTool.GetDescription: String; begin Result := 'Saves a research note with a key for later retrieval'; end;
function TNoteTool.GetCategory   : String; begin Result := 'Research'; end;
function TNoteTool.GetSchema     : TJSONObject; begin Result := FSchema; end;
function TNoteTool.IsAvailable   : Boolean; begin Result := True; end;

function TNoteTool.Execute(const AArgs: TJSONObject): TJSONObject;
var Key, Content: String;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('key',     Key);
  AArgs.TryGetValue<String>('content', Content);
  if Assigned(FBlackboard) then
    FBlackboard.SetString('note.' + Key, Content);
  Writeln(Format('  [save_note] key="%s" guardado (%d chars)', [Key, Length(Content)]));
  Result := TJSONObject.Create;
  Result.AddPair('status', 'saved');
  Result.AddPair('key',    Key);
end;

// =============================================================================
// DEMO PRINCIPAL
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

procedure RunResearch(const ATopic: String);
var
  Manager          : TAIAgentManager;
  PlannerNode      : TLLMNode;
  ResearchNode     : TLLMNode;
  AnalystNode      : TLLMNode;
  WriterNode       : TLLMNode;
  ResearchRegistry : TAiToolRegistry;
  Link1, Link2, Link3: TAIAgentsLink;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Investigacion: ', ATopic);
  Writeln(StringOfChar('=', 60));

  Manager          := TAIAgentManager.Create(nil);
  ResearchRegistry := TAiToolRegistry.Create;  // instancia propia (no singleton)
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 180000;

    // Registrar herramientas en el registry del investigador
    ResearchRegistry.Register(TMockSearchTool.Create, 'local', 'research');
    ResearchRegistry.Register(TNoteTool.Create(Manager.Blackboard), 'local', 'research');

    // Nodo 1: Planner — genera consultas de busqueda
    PlannerNode := MakeLLMNode(Manager, 'Planner',
      'You are a research planner. Given a topic, output a research plan with ' +
      '3 specific search queries. Format: "QUERY 1: ...\nQUERY 2: ...\nQUERY 3: ..."');

    // Nodo 2: Researcher — ejecuta busquedas y guarda hallazgos
    ResearchNode := MakeLLMNode(Manager, 'Researcher',
      'You are a researcher. Execute the 3 search queries using web_search. ' +
      'After each search, save key findings using save_note with keys "finding1", "finding2", "finding3". ' +
      'Then output a summary of all findings.',
      True);  // UseAllTools=True
    ResearchNode.Registry := ResearchRegistry;  // usa su propio registry

    // Nodo 3: Analyst — identifica insights y gaps
    AnalystNode := MakeLLMNode(Manager, 'Analyst',
      'You are a critical analyst. Identify 3 key insights and 2 gaps. ' +
      'Format: "KEY INSIGHTS:" and "GAPS:"');

    // Nodo 4: Writer — escribe el reporte final
    WriterNode := MakeLLMNode(Manager, 'Writer',
      'You are a technical writer. Write a structured mini-report with: ' +
      'Executive Summary, Key Findings, Conclusion. Keep it to 3 short paragraphs.');

    // Encadenar: Planner -> Researcher -> Analyst -> Writer
    Link1 := TAIAgentsLink.Create(Manager); Link1.Name := 'L1'; Link1.Graph := Manager; Link1.NextA := ResearchNode; PlannerNode.Next  := Link1;
    Link2 := TAIAgentsLink.Create(Manager); Link2.Name := 'L2'; Link2.Graph := Manager; Link2.NextA := AnalystNode;  ResearchNode.Next := Link2;
    Link3 := TAIAgentsLink.Create(Manager); Link3.Name := 'L3'; Link3.Graph := Manager; Link3.NextA := WriterNode;   AnalystNode.Next  := Link3;

    Manager.StartNode := PlannerNode;
    Manager.EndNode   := WriterNode;
    Manager.Compile;

    Writeln;
    Writeln('[Pipeline] Planner -> Researcher (tools) -> Analyst -> Writer');
    Writeln;

    var FinalReport := Manager.Run(ATopic);

    Writeln;
    Writeln('--- Plan de investigacion ---');
    Writeln(Manager.Blackboard.GetString('Planner.output'));
    Writeln;
    Writeln('--- Reporte final ---');
    Writeln(FinalReport);

  finally
    ResearchRegistry.Free;
    Manager.Free;
  end;
  Writeln;
end;

begin
  Writeln('=== AgentResearch ===');
  Writeln('Pipeline: Planner -> Researcher (web_search + save_note) -> Analyst -> Writer');
  Writeln;
  try
    RunResearch('Artificial intelligence trends in 2024');
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
