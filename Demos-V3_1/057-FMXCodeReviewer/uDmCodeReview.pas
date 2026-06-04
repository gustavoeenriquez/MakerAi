unit uDmCodeReview;

// =============================================================================
// MakerAI - Demo 057: FMX Code Reviewer
// DataModule con el grafo de revision de codigo.
//
// Pipeline:
//   [Generator] -> [Reviewer] -> [Refactorer]
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
  TCodeReviewLogEvent      = procedure(const Value: string) of object;
  TCodeReviewFinishedEvent = procedure(const Value: string) of object;

  TDmCodeReview = class(TDataModule)
    AgentManager : TAIAgents;
    Generator    : TLLMNode;
    Link1        : TAIAgentsLink;
    Reviewer     : TLLMNode;
    Link2        : TAIAgentsLink;
    Refactorer   : TLLMNode;
    procedure DataModuleCreate(Sender: TObject);
    procedure AgentManagerFinish(Sender: TObject; const Input, Output: string;
      Status: TAgentExecutionStatus; E: Exception);
    procedure AgentManagerEnterNode(Sender: TObject; Node: TAIAgentsNode);
    procedure AgentManagerExitNode(Sender: TObject; Node: TAIAgentsNode);
  private
    FOnLog         : TCodeReviewLogEvent;
    FOnFinished    : TCodeReviewFinishedEvent;
    FHistory       : string;
    function NodeRoleLabel(const ANodeName: string): string;
  public
    procedure RunReview(const ARequirement: string);
    property OnLog      : TCodeReviewLogEvent      read FOnLog      write FOnLog;
    property OnFinished : TCodeReviewFinishedEvent read FOnFinished write FOnFinished;
  end;

var
  DmCodeReview: TDmCodeReview;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

function TDmCodeReview.NodeRoleLabel(const ANodeName: string): string;
begin
  if SameText(ANodeName, 'Generator')  then Result := 'GENERATED CODE'
  else if SameText(ANodeName, 'Reviewer')   then Result := 'CODE REVIEW'
  else if SameText(ANodeName, 'Refactorer') then Result := 'IMPROVED CODE'
  else Result := ANodeName.ToUpper;
end;

procedure TDmCodeReview.DataModuleCreate(Sender: TObject);

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
  ConfigNode(Generator,
    'You are an expert software engineer. ' +
    'Generate clean, well-commented code based on the requirement provided. ' +
    'Choose the most appropriate language (prefer Python unless specified). ' +
    'Include: function/class definition, implementation, and a brief usage example. ' +
    'Show only the code with inline comments — no prose explanation.');

  ConfigNode(Reviewer,
    'You are a senior code reviewer. ' +
    'The generated code is provided as context. ' +
    'Review it thoroughly and identify issues in these categories: ' +
    'BUGS, SECURITY, PERFORMANCE, ERROR HANDLING, STYLE. ' +
    'Label each finding as "Issue N: [CATEGORY] description". ' +
    'Be specific. Do not rewrite the code — only list issues.');

  ConfigNode(Refactorer,
    'You are a refactoring expert. ' +
    'The original code and the review are provided as context. ' +
    'Produce an improved version that addresses all identified issues. ' +
    'Show ONLY the improved code (no prose before it). ' +
    'After the code, add a brief "Changes:" section listing what was fixed. ' +
    'Do not ask questions — refactor immediately.');

  AgentManager.Compile;
end;

procedure TDmCodeReview.AgentManagerEnterNode(Sender: TObject; Node: TAIAgentsNode);
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

procedure TDmCodeReview.AgentManagerExitNode(Sender: TObject; Node: TAIAgentsNode);
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

procedure TDmCodeReview.AgentManagerFinish(Sender: TObject; const Input, Output: string;
  Status: TAgentExecutionStatus; E: Exception);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnFinished) then
        FOnFinished(Output);
    end);
end;

procedure TDmCodeReview.RunReview(const ARequirement: string);
begin
  FHistory := 'REQUIREMENT: ' + ARequirement + sLineBreak + sLineBreak;
  AgentManager.Run(ARequirement);
end;

end.
