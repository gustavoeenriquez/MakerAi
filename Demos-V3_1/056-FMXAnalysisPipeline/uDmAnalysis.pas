unit uDmAnalysis;

// =============================================================================
// MakerAI - Demo 056: FMX Analysis Pipeline
// DataModule con el grafo de analisis.
//
// Pipeline:
//   [Analyzer] -> [Extractor] -> [Summarizer]
//
// Cada nodo es un TLLMNode con su propio SystemPrompt.
// El historial se acumula via Blackboard (OnEnterNode/OnExitNode).
// =============================================================================

interface

uses
  System.SysUtils,
  System.Classes,
  uMakerAi.Agents,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude;

type
  TAnalysisLogEvent      = procedure(const Value: string) of object;
  TAnalysisFinishedEvent = procedure(const Value: string) of object;

  TDmAnalysis = class(TDataModule)
    AgentManager : TAIAgents;
    Analyzer     : TLLMNode;
    Link1        : TAIAgentsLink;
    Extractor    : TLLMNode;
    Link2        : TAIAgentsLink;
    Summarizer   : TLLMNode;
    procedure DataModuleCreate(Sender: TObject);
    procedure AgentManagerFinish(Sender: TObject; const Input, Output: string;
      Status: TAgentExecutionStatus; E: Exception);
    procedure AgentManagerEnterNode(Sender: TObject; Node: TAIAgentsNode);
    procedure AgentManagerExitNode(Sender: TObject; Node: TAIAgentsNode);
  private
    FOnLog         : TAnalysisLogEvent;
    FOnFinished    : TAnalysisFinishedEvent;
    FHistory       : string;
    function NodeRoleLabel(const ANodeName: string): string;
  public
    procedure RunAnalysis(const ATopic: string);
    property OnLog      : TAnalysisLogEvent      read FOnLog      write FOnLog;
    property OnFinished : TAnalysisFinishedEvent read FOnFinished write FOnFinished;
  end;

var
  DmAnalysis: TDmAnalysis;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

function TDmAnalysis.NodeRoleLabel(const ANodeName: string): string;
begin
  if SameText(ANodeName, 'Analyzer')   then Result := 'ANALYSIS'
  else if SameText(ANodeName, 'Extractor') then Result := 'ENTITIES'
  else if SameText(ANodeName, 'Summarizer') then Result := 'SUMMARY'
  else Result := ANodeName.ToUpper;
end;

procedure TDmAnalysis.DataModuleCreate(Sender: TObject);

  procedure ConfigNode(Node: TLLMNode; const APrompt: string);
  begin
    Node.DriverName   := DRIVER;
    Node.Model        := MODEL;
    Node.ApiKey       := API_KEY;
    Node.UseAllTools  := False;
    Node.MaxTokens    := 1024;
    Node.SystemPrompt := APrompt;
  end;

begin
  ConfigNode(Analyzer,
    'You are a professional text analyst. Analyze the given topic and identify: ' +
    '(1) Main subject and scope, (2) Key themes — list 3-5, ' +
    '(3) Tone: objective/critical/supportive/neutral, ' +
    '(4) Main arguments or points. Be structured and concise.');

  ConfigNode(Extractor,
    'You are an entity extraction specialist. ' +
    'The analysis so far is provided as context. ' +
    'Extract and categorize key entities related to the topic: ' +
    'PEOPLE (relevant figures), ORGANIZATIONS, LOCATIONS, ' +
    'DATES/PERIODS, KEY CONCEPTS. ' +
    'Format each category as a labeled list. Do not ask questions — extract immediately.');

  ConfigNode(Summarizer,
    'You are an executive summarizer. ' +
    'The full analysis pipeline results are provided as context. ' +
    'Write a concise executive summary (max 150 words). ' +
    'Start with "SUMMARY:". Highlight the most important insights and implications. ' +
    'Do not request additional information — summarize what is available.');

  AgentManager.Compile;
end;

procedure TDmAnalysis.AgentManagerEnterNode(Sender: TObject; Node: TAIAgentsNode);
var
  Sep, Hdr: string;
begin
  if FHistory <> '' then
    Node.Input := FHistory;

  Sep := StringOfChar('=', 50);
  Hdr := '>> ' + NodeRoleLabel(Node.Name);
  if Assigned(FOnLog) then FOnLog(Sep);
  if Assigned(FOnLog) then FOnLog(Hdr);
end;

procedure TDmAnalysis.AgentManagerExitNode(Sender: TObject; Node: TAIAgentsNode);
var
  Label_: string;
begin
  Label_ := NodeRoleLabel(Node.Name);

  FHistory := FHistory +
    '[' + Label_ + ']:' + sLineBreak +
    Node.Output + sLineBreak + sLineBreak;

  if Assigned(FOnLog) then
  begin
    FOnLog('[' + Label_ + ']:');
    FOnLog(Node.Output);
  end;
end;

procedure TDmAnalysis.AgentManagerFinish(Sender: TObject; const Input, Output: string;
  Status: TAgentExecutionStatus; E: Exception);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnFinished) then
        FOnFinished(Output);
    end);
end;

procedure TDmAnalysis.RunAnalysis(const ATopic: string);
begin
  FHistory := 'TOPIC: ' + ATopic + sLineBreak + sLineBreak;
  AgentManager.Run(ATopic);
end;

end.
