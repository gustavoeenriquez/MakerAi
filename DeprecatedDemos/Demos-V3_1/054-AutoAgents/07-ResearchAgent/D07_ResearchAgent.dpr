program D07_ResearchAgent;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 07: ResearchAgent
// =============================================================================
// Agente de investigacion multi-hop con herramientas:
//   [Planner] -> [Researcher] (con mock search tool) -> [Analyst] -> [Writer]
//
// El Researcher usa una herramienta de busqueda mock que devuelve datos
// predefinidos segun palabras clave. Demuestra el uso de herramientas
// especializadas en un pipeline multi-nodo.
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
// TMockSearchTool — busqueda web simulada
// Devuelve datos predefinidos segun el query
// Args: { "query": string, "max_results": number (optional) }
// -----------------------------------------------------------------------------
type
  TMockSearchTool = class(TInterfacedObject, IAiTool)
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
function TMockSearchTool.GetName: String; begin Result := 'web_search'; end;
function TMockSearchTool.GetDescription: String; begin Result := 'Searches the web for information about a topic. Returns relevant snippets.'; end;
function TMockSearchTool.GetCategory: String; begin Result := 'Research'; end;
function TMockSearchTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TMockSearchTool.IsAvailable: Boolean; begin Result := True; end;

function TMockSearchTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Query  : String;
  Results: TJSONArray;
  Snippet: TJSONObject;
  QLower : String;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('query', Query);
  QLower := LowerCase(Query);
  Writeln(Format('  [web_search] query="%s"', [Query]));

  Results := TJSONArray.Create;

  // Datos mock basados en palabras clave
  if (Pos('climate', QLower) > 0) or (Pos('global warming', QLower) > 0) then
  begin
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'Climate Change Overview'); Snippet.AddPair('snippet', 'Global temperatures have risen 1.1C since pre-industrial times. CO2 levels at 421 ppm (2023). Arctic warming 3x faster than global average.'); Results.Add(Snippet);
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'IPCC Report 2023'); Snippet.AddPair('snippet', 'Without immediate action, warming could reach 2.5-3C by 2100. Renewable energy adoption must triple by 2030.'); Results.Add(Snippet);
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'Economic Impact'); Snippet.AddPair('snippet', 'Climate change could reduce global GDP by 10-23% by 2100. Green transition creates 24M jobs by 2030.'); Results.Add(Snippet);
  end
  else if (Pos('artificial intelligence', QLower) > 0) or (Pos(' ai ', QLower) > 0) or (Pos('machine learning', QLower) > 0) then
  begin
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'AI Market Growth'); Snippet.AddPair('snippet', 'Global AI market valued at $207B in 2023, expected to reach $1.8T by 2030. GPT-4 and Claude lead large language models.'); Results.Add(Snippet);
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'AI Applications'); Snippet.AddPair('snippet', 'AI used in healthcare (diagnostics), finance (fraud detection), manufacturing (predictive maintenance) and autonomous vehicles.'); Results.Add(Snippet);
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'AI Risks'); Snippet.AddPair('snippet', 'Key concerns: job displacement (14% of jobs at high risk), bias in models, privacy issues, and existential risk debates.'); Results.Add(Snippet);
  end
  else if (Pos('quantum', QLower) > 0) then
  begin
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'Quantum Computing Basics'); Snippet.AddPair('snippet', 'Quantum computers use qubits instead of bits. IBM has 1000+ qubit processors. Google achieved quantum supremacy in 2019.'); Results.Add(Snippet);
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'Quantum Applications'); Snippet.AddPair('snippet', 'Potential uses: cryptography breaking, drug discovery, financial optimization, materials science.'); Results.Add(Snippet);
  end
  else
  begin
    // Generic mock result
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'General Information'); Snippet.AddPair('snippet', Format('Research data on "%s": This is a complex topic with multiple dimensions. Studies show significant developments in recent years.', [Query])); Results.Add(Snippet);
    Snippet := TJSONObject.Create; Snippet.AddPair('title', 'Recent Developments'); Snippet.AddPair('snippet', Format('Latest findings on "%s" indicate growing interest from researchers and industry. Multiple studies published in 2023-2024.', [Query])); Results.Add(Snippet);
  end;

  Writeln(Format('  [web_search] => %d results', [Results.Count]));

  Result := TJSONObject.Create;
  Result.AddPair('results', Results);
  Result.AddPair('count', TJSONNumber.Create(Results.Count));
  Result.AddPair('query', Query);
end;

// -----------------------------------------------------------------------------
// TNoteTool — guarda notas en el blackboard durante la investigacion
// Args: { "key": string, "content": string }
// -----------------------------------------------------------------------------
type
  TNoteTool = class(TInterfacedObject, IAiTool)
  private
    FSchema: TJSONObject;
    FBlackboard: TAIBlackboard;
  public
    constructor Create(ABlackboard: TAIBlackboard);
    destructor  Destroy; override;
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
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
function TNoteTool.GetName: String; begin Result := 'save_note'; end;
function TNoteTool.GetDescription: String; begin Result := 'Saves a research note with a key for later retrieval'; end;
function TNoteTool.GetCategory: String; begin Result := 'Research'; end;
function TNoteTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TNoteTool.IsAvailable: Boolean; begin Result := True; end;

function TNoteTool.Execute(const AArgs: TJSONObject): TJSONObject;
var Key, Content: String;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('key', Key);
  AArgs.TryGetValue<String>('content', Content);
  if Assigned(FBlackboard) then
    FBlackboard.SetString('note.' + Key, Content);
  Writeln(Format('  [save_note] key="%s" saved (%d chars)', [Key, Length(Content)]));
  Result := TJSONObject.Create;
  Result.AddPair('status', 'saved');
  Result.AddPair('key', Key);
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
  Manager    : TAIAgentManager;
  PlannerNode: TLLMNode;
  ResearchNode: TLLMNode;
  AnalystNode: TLLMNode;
  WriterNode : TLLMNode;
  ResearchRegistry: TAiToolRegistry;
  Link1, Link2, Link3: TAIAgentsLink;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Investigacion: ', ATopic);
  Writeln(StringOfChar('=', 60));

  Manager := TAIAgentManager.Create(nil);
  ResearchRegistry := TAiToolRegistry.Create;
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 180000;

    // Registry propio para el nodo de investigacion
    ResearchRegistry.Register(TMockSearchTool.Create, 'local', 'research');
    ResearchRegistry.Register(TNoteTool.Create(Manager.Blackboard), 'local', 'research');

    // Nodo 1: Planner — genera plan de investigacion
    PlannerNode := MakeLLMNode(Manager, 'Planner',
      'You are a research planner. Given a topic, output a research plan with ' +
      '3 specific search queries to investigate it. Format: "QUERY 1: ...\nQUERY 2: ...\nQUERY 3: ..."',
      False);

    // Nodo 2: Researcher — usa web_search y save_note
    ResearchNode := MakeLLMNode(Manager, 'Researcher',
      'You are a researcher. Execute the 3 search queries from the plan using web_search tool. ' +
      'After each search, save key findings using save_note tool with keys "finding1", "finding2", "finding3". ' +
      'Then output a summary of all findings.',
      True);  // UseAllTools=True pero usara el registry propio
    ResearchNode.Registry := ResearchRegistry;  // registry especializado

    // Nodo 3: Analyst — analiza los hallazgos
    AnalystNode := MakeLLMNode(Manager, 'Analyst',
      'You are a critical analyst. You receive research findings. ' +
      'Identify 3 key insights and 2 potential gaps or uncertainties. ' +
      'Format with clear headers: "KEY INSIGHTS:" and "GAPS:"',
      False);

    // Nodo 4: Writer — escribe el reporte final
    WriterNode := MakeLLMNode(Manager, 'Writer',
      'You are a technical writer. You receive analysis of research findings. ' +
      'Write a structured mini-report with sections: Executive Summary, Key Findings, Conclusion. ' +
      'Keep it concise (3 short paragraphs total).',
      False);

    // Encadenar nodos
    Link1 := TAIAgentsLink.Create(Manager); Link1.Name := 'L1'; Link1.Graph := Manager; Link1.NextA := ResearchNode; PlannerNode.Next := Link1;
    Link2 := TAIAgentsLink.Create(Manager); Link2.Name := 'L2'; Link2.Graph := Manager; Link2.NextA := AnalystNode;  ResearchNode.Next := Link2;
    Link3 := TAIAgentsLink.Create(Manager); Link3.Name := 'L3'; Link3.Graph := Manager; Link3.NextA := WriterNode;   AnalystNode.Next := Link3;

    Manager.StartNode := PlannerNode;
    Manager.EndNode   := WriterNode;
    Manager.Compile;

    Writeln;
    Writeln('[Pipeline] Planner -> Researcher -> Analyst -> Writer');
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

procedure RunDemo;
begin
  Writeln('=== Demo 07 - ResearchAgent ===');
  Writeln('Pipeline: Planner -> Researcher (tools: web_search, save_note) -> Analyst -> Writer');
  Writeln;

  RunResearch('Artificial intelligence trends in 2024');
  RunResearch('Climate change impact on agriculture');
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
