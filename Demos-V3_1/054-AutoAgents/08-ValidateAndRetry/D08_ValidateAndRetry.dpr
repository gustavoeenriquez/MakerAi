program D08_ValidateAndRetry;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 08: ValidateAndRetry
// =============================================================================
// Pipeline de generacion con validacion y reescritura:
//   [DraftWriter] -> [Critic] -> [Rewriter]
//
// El Critic evalua el borrador con criterios especificos y proporciona
// feedback estructurado. El Rewriter incorpora el feedback para mejorar.
// Demuestra paso de contexto y retroalimentacion entre nodos LLM.
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
// TRubyCheckerTool — verifica criterios de calidad en un texto
// Args: { "text": string, "criteria": string }
// Returns: { "score": number, "issues": string, "passed": bool }
// -----------------------------------------------------------------------------
type
  TQualityCheckerTool = class(TInterfacedObject, IAiTool)
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

constructor TQualityCheckerTool.Create;
var Props, Txt, Crit: TJSONObject; Req: TJSONArray;
begin
  inherited;
  FSchema := TJSONObject.Create;
  FSchema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Txt  := TJSONObject.Create; Txt.AddPair('type', 'string');  Txt.AddPair('description',  'Text to evaluate');
  Props.AddPair('text', Txt);
  Crit := TJSONObject.Create; Crit.AddPair('type', 'string'); Crit.AddPair('description', 'Quality criteria to check');
  Props.AddPair('criteria', Crit);
  FSchema.AddPair('properties', Props);
  Req := TJSONArray.Create; Req.Add('text'); Req.Add('criteria');
  FSchema.AddPair('required', Req);
end;

destructor TQualityCheckerTool.Destroy; begin FSchema.Free; inherited; end;
function TQualityCheckerTool.GetName: String; begin Result := 'check_quality'; end;
function TQualityCheckerTool.GetDescription: String; begin Result := 'Checks text quality against given criteria. Returns score (0-10), issues found, and pass/fail.'; end;
function TQualityCheckerTool.GetCategory: String; begin Result := 'Quality'; end;
function TQualityCheckerTool.GetSchema: TJSONObject; begin Result := FSchema; end;
function TQualityCheckerTool.IsAvailable: Boolean; begin Result := True; end;

function TQualityCheckerTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Txt, Crit : String;
  WordCount : Integer;
  Score     : Integer;
  Issues    : String;
  Words     : TArray<String>;
begin
  Result := nil;
  if not Assigned(AArgs) then Exit;
  AArgs.TryGetValue<String>('text',     Txt);
  AArgs.TryGetValue<String>('criteria', Crit);

  Words     := Txt.Trim.Split([' ', #9, #10, #13], TStringSplitOptions.ExcludeEmpty);
  WordCount := Length(Words);
  Score     := 10;
  Issues    := '';

  // Verificaciones automaticas basicas
  if WordCount < 50 then begin
    Dec(Score, 3);
    Issues := Issues + 'Too short (< 50 words). ';
  end;
  if WordCount > 500 then begin
    Dec(Score, 1);
    Issues := Issues + 'Too long (> 500 words). ';
  end;
  if Pos('?', Txt) = 0 then begin
    Dec(Score, 1);
    Issues := Issues + 'No rhetorical questions found. ';
  end;
  if (Pos('however', LowerCase(Txt)) = 0) and (Pos('but', LowerCase(Txt)) = 0) and (Pos('although', LowerCase(Txt)) = 0) then begin
    Dec(Score, 1);
    Issues := Issues + 'No contrast words (however/but/although). ';
  end;
  if Score < 0 then Score := 0;

  Writeln(Format('  [check_quality] words=%d score=%d/10 issues: %s', [WordCount, Score, Issues]));

  Result := TJSONObject.Create;
  Result.AddPair('score',  TJSONNumber.Create(Score));
  Result.AddPair('issues', Issues);
  Result.AddPair('passed', TJSONBool.Create(Score >= 7));
  Result.AddPair('word_count', TJSONNumber.Create(WordCount));
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

procedure RunPipeline(const ATask: String);
var
  Manager    : TAIAgentManager;
  DraftNode  : TLLMNode;
  CriticNode : TLLMNode;
  RewriteNode: TLLMNode;
  CriticRegistry: TAiToolRegistry;
  Link1, Link2: TAIAgentsLink;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tarea: ', ATask);
  Writeln(StringOfChar('=', 60));

  Manager := TAIAgentManager.Create(nil);
  CriticRegistry := TAiToolRegistry.Create;
  try
    Manager.Asynchronous := False;
    Manager.TimeoutMs    := 120000;

    CriticRegistry.Register(TQualityCheckerTool.Create, 'local', 'quality');

    // Nodo 1: Escritor de borrador
    DraftNode := MakeLLMNode(Manager, 'DraftWriter',
      'You are a content writer. Write a draft paragraph (80-150 words) about the given topic. ' +
      'Use at least one contrast word (however/but/although) and one rhetorical question.',
      False);

    // Nodo 2: Critico con herramienta de calidad
    CriticNode := MakeLLMNode(Manager, 'Critic',
      'You are a strict editor. Evaluate the draft using the check_quality tool. ' +
      'Then provide specific improvement suggestions in this format: ' +
      '"DRAFT:\n[the draft]\n\nFEEDBACK:\n[your specific suggestions]\n\nIMPROVEMENT INSTRUCTIONS:\n[3 concrete actions to improve]"',
      True);
    CriticNode.Registry := CriticRegistry;

    // Nodo 3: Reescritor con el feedback del critico
    RewriteNode := MakeLLMNode(Manager, 'Rewriter',
      'You are an expert writer. You receive a draft and detailed feedback. ' +
      'Rewrite the draft incorporating ALL the improvement instructions. ' +
      'The rewritten version should be clearly better than the original.',
      False);

    // Encadenar
    Link1 := TAIAgentsLink.Create(Manager); Link1.Name := 'L1'; Link1.Graph := Manager; Link1.NextA := CriticNode;  DraftNode.Next  := Link1;
    Link2 := TAIAgentsLink.Create(Manager); Link2.Name := 'L2'; Link2.Graph := Manager; Link2.NextA := RewriteNode; CriticNode.Next := Link2;

    Manager.StartNode := DraftNode;
    Manager.EndNode   := RewriteNode;
    Manager.Compile;

    Writeln;
    var FinalText := Manager.Run(ATask);

    Writeln;
    Writeln('--- Borrador inicial ---');
    Writeln(Manager.Blackboard.GetString('DraftWriter.output'));
    Writeln;
    Writeln('--- Feedback del critico ---');
    Writeln(Manager.Blackboard.GetString('Critic.output'));
    Writeln;
    Writeln('--- Version final reescrita ---');
    Writeln(FinalText);

  finally
    CriticRegistry.Free;
    Manager.Free;
  end;
  Writeln;
end;

procedure RunDemo;
begin
  Writeln('=== Demo 08 - ValidateAndRetry ===');
  Writeln('Pipeline: DraftWriter -> Critic (con check_quality tool) -> Rewriter');
  Writeln;

  RunPipeline('The importance of sleep for productivity');
  RunPipeline('How social media affects mental health');
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
