program AgentConsoleDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  System.DateUtils,
  uMakerAi.Agents;

type
  TDemo = class
  private
    FAgents: TAIAgents;
    procedure Println(const S: string);

    // --- Event Handlers for the Agents ---
    procedure DoPrint(Sender: TObject; Value: string);
    procedure DoEnd(Node: TAIAgentsNode; Value: string);
    procedure DoError(Sender: TObject; Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception; var Abort: Boolean);
    procedure DoConfirm(Sender: TObject; Node: TAIAgentsNode; const AQuestion: string; Buttons: TMsgStates; var AResponse: string;
      var AModalResult: TMsgState);

    // --- Node Executor Procedures ---
    procedure StartExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure WorkExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure DecideExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure OkExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure FailExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure FinishExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure ParallelA_Exec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure ParallelB_Exec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);

    // --- Test Runners ---
    procedure WaitForCompletion;
    procedure RunConditionalFlow;
    procedure RunParallelFlow;

  public
    constructor Create;
    destructor Destroy;
    procedure Run;
  end;

  { TDemo }

constructor TDemo.Create;
begin
  FAgents := TAIAgents.Create(nil);
  // Assign global events once
  FAgents.OnPrint := DoPrint;
  FAgents.OnEnd := DoEnd;
  FAgents.OnError := DoError;
  FAgents.OnConfirm := DoConfirm;
end;

destructor TDemo.Destroy;
begin
  FAgents.Free;
end;

procedure TDemo.Println(const S: string);
begin
  Writeln(FormatDateTime('hh:nn:ss.zzz', Now), ' | ', S);
end;

procedure TDemo.DoConfirm(Sender: TObject; Node: TAIAgentsNode; const AQuestion: string; Buttons: TMsgStates; var AResponse: string;
  var AModalResult: TMsgState);
begin
  AModalResult := msCancel; // Default behavior for console demo
end;

procedure TDemo.DoEnd(Node: TAIAgentsNode; Value: string);
begin
  Println('✅ OnEnd: ' + Node.Name + ' -> ' + Value);
end;

procedure TDemo.DoError(Sender: TObject; Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception; var Abort: Boolean);
var
  NodeName, LinkName: string;
begin
  if Assigned(Node) then
    NodeName := Node.Name
  else
    NodeName := '<nil>';
  if Assigned(Link) then
    LinkName := Link.Name
  else
    LinkName := '<nil>';
  Println(Format('❌ ERROR (Node=%s, Link=%s): %s', [NodeName, LinkName, E.Message]));
  Abort := True;
end;

procedure TDemo.DoPrint(Sender: TObject; Value: string);
var
  Who: string;
begin
  if Sender is TAIAgentsNode then
    Who := 'Node:' + TAIAgentsNode(Sender).Name
  else if Sender is TAIAgentsLink then
    Who := 'Link:' + TAIAgentsLink(Sender).Name
  else
    Who := Sender.ClassName;
  Println(Format('  [%s] %s', [Who, Value]));
end;

// --- Node Executors Implementation ---

procedure TDemo.StartExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print('StartExec: preparing input...');
  TThread.Sleep(200);
  Output := 'Initial Data: ' + Input;
end;

procedure TDemo.WorkExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print('WorkExec: simulating model call...');
  TThread.Sleep(500);
  Output := Format('Model response to "%s"', [Input]);
end;

procedure TDemo.DecideExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Decision: string;
begin
  Node.Print('DecideExec: evaluating decision...');
  TThread.Sleep(100);
  // Decision based on random value
  if Random(100) < 60 then
    Decision := 'ok'
  else
    Decision := 'fail';
  Node.Graph.Blackboard.SetString('next_route', Decision);
  Output := 'decision=' + Decision;
  Node.Print('Decision made: next_route=' + Decision);
end;

procedure TDemo.OkExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print('OkExec: Processing OK branch.');
  TThread.Sleep(150);
  Output := 'OK -> ' + Input;
end;

procedure TDemo.FailExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print('FailExec: Processing FAIL branch.');
  TThread.Sleep(150);
  Output := 'FAIL -> ' + Input;
end;

procedure TDemo.FinishExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print('FinishExec: joining results...');
  TThread.Sleep(100);
  // Input here contains combined outputs from all incoming branches
  Output := 'FINAL RESULT:' + sLineBreak + Input;
end;

procedure TDemo.ParallelA_Exec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print('Branch A: Starting long task...');
  TThread.Sleep(500); // Simulate a long job
  Output := 'Result from Task A';
  Node.Print('Branch A: Task finished.');
end;

procedure TDemo.ParallelB_Exec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print('Branch B: Starting short task...');
  TThread.Sleep(1200); // Simulate a shorter job
  Output := 'Result from Task B';
  Node.Print('Branch B: Task finished.');
end;


// --- Test Runners Implementation ---

procedure TDemo.WaitForCompletion;
begin
  // Wait until the Agents indicates it is no longer busy.
  while FAgents.Busy do
  begin
    // Allow TThread.Synchronize to process pending messages
    CheckSynchronize;
    Sleep(50);
  end;
  // Final check to ensure all synchronized calls are processed
  CheckSynchronize;
  Println('--- Execution cycle finished. ---');
end;

procedure TDemo.RunConditionalFlow;
var
  Cond: TDictionary<string, string>;
begin
  Println('--- Starting CONDITIONAL Flow (IF/ELSE) ---');
  Writeln;
  Writeln('          [StartNode]');
  Writeln('               |');
  Writeln('          [WorkNode]');
  Writeln('               |');
  Writeln('          [DecideNode]');
  Writeln('               |');
  Writeln('        (LinkDecision)');
  Writeln('              / \');
  Writeln('             /   \');
  Writeln('   ''ok'' -> /     \ <- ''fail''');
  Writeln('         /       \');
  Writeln('  [OkNode]     [FailNode]');
  Writeln('         \       /');
  Writeln('          \     /');
  Writeln('           \   /');
  Writeln('         [FinishNode]');
  Writeln;

  FAgents.ClearGraph;

  // 1. Build the nodes
  FAgents.AddNode('Start', StartExec).AddNode('Work', WorkExec).AddNode('Decide', DecideExec).AddNode('OkNode', OkExec)
    .AddNode('FailNode', FailExec).AddNode('Finish', FinishExec);

  // 2. Link the nodes
  FAgents.AddEdge('Start', 'Work');
  FAgents.AddEdge('Work', 'Decide');

  // Conditional link from 'Decide'
  Cond := TDictionary<string, string>.Create;
  try
    Cond.Add('ok', 'OkNode');
    Cond.Add('fail', 'FailNode');
    FAgents.AddConditionalEdge('Decide', 'LinkDecision', Cond);
  finally
    Cond.Free;
  end;

  // Both branches converge on 'Finish'
  FAgents.AddEdge('OkNode', 'Finish');
  FAgents.AddEdge('FailNode', 'Finish');

  // 3. Define entry/finish points
  FAgents.SetEntryPoint('Start').SetFinishPoint('Finish');

  // 4. Run
  FAgents.Run('Conditional Test');
  WaitForCompletion;
end;

procedure TDemo.RunParallelFlow;
var
  ForkLink: TAIAgentsLink;
  JoinLinkFromA, JoinLinkFromB: TAIAgentsLink;
  TaskANode, TaskBNode, JoinNode: TAIAgentsNode;
begin
  Println('--- Starting PARALLEL Flow (Fork/Join) ---');
  Writeln;
  Writeln('          [StartNode]');
  Writeln('               |');
  Writeln('           (ForkLink)');
  Writeln('              / \');
  Writeln('             /   \');
  Writeln('            /     \');
  Writeln('    [TaskA]       [TaskB]');
  Writeln('       |             |');
  Writeln(' (JoinLinkA)   (JoinLinkB)');
  Writeln('            \     /');
  Writeln('             \   /');
  Writeln('              \ /');
  Writeln('           [JoinNode]');
  Writeln;

  FAgents.ClearGraph;

  // 1. Build the nodes
  FAgents.AddNode('Start', StartExec).AddNode('TaskA', ParallelA_Exec).AddNode('TaskB', ParallelB_Exec).AddNode('JoinNode', FinishExec);

  // Cache node lookups for clarity
  TaskANode := FAgents.FindNode('TaskA');
  TaskBNode := FAgents.FindNode('TaskB');
  JoinNode := FAgents.FindNode('JoinNode');

  // 2. Define the "Fork" manually from Start -> (TaskA, TaskB)
  ForkLink := TAIAgentsLink.Create(FAgents);
  ForkLink.Name := 'ForkLink';
  ForkLink.Graph := FAgents;
  ForkLink.NextA := TaskANode; // Branch 1
  ForkLink.NextB := TaskBNode; // Branch 2
  FAgents.FindNode('Start').Next := ForkLink;

  // 3. Define the "Join" manually. Each parallel task needs its own link to the join node.
  // This is the CRITICAL part that was wrong before.
  // We cannot use AddEdge because it's too simplistic.

  // Link from TaskA -> JoinNode
  JoinLinkFromA := TAIAgentsLink.Create(FAgents);
  JoinLinkFromA.Name := 'JoinLinkFromA';
  JoinLinkFromA.Graph := FAgents;
  JoinLinkFromA.NextA := JoinNode;
  TaskANode.Next := JoinLinkFromA;

  // Link from TaskB -> JoinNode
  JoinLinkFromB := TAIAgentsLink.Create(FAgents);
  JoinLinkFromB.Name := 'JoinLinkFromB';
  JoinLinkFromB.Graph := FAgents;
  JoinLinkFromB.NextA := JoinNode;
  TaskBNode.Next := JoinLinkFromB;

  FAgents.FindNode('JoinNode').JoinMode := TJoinMode.jmAll;

  // 4. Define entry/finish points
  FAgents.SetEntryPoint('Start').SetFinishPoint('JoinNode');

  // 5. Run
  FAgents.Run('Parallel Test');
  WaitForCompletion;
end;

procedure TDemo.Run;
var
  Choice: string;
begin
  Randomize;
  repeat
    Writeln;
    Writeln('===================================');
    Writeln('       MakerAI Agents Demo      ');
    Writeln('===================================');
    Writeln('Choose a flow to run:');
    Writeln('  1. Conditional Flow (IF/ELSE)');
    Writeln('  2. Parallel Flow (Fork/Join)');
    Writeln('  Q. Quit');
    Write('Enter your choice (1, 2, or Q): ');
    Readln(Choice);
    Writeln;

    if Choice.Trim.ToUpper = '1' then
      RunConditionalFlow
    else if Choice.Trim.ToUpper = '2' then
      RunParallelFlow
    else if Choice.Trim.ToUpper = 'Q' then
      Break
    else
      Println('Invalid choice.');

  until Choice.Trim.ToUpper = 'Q';

  Println('Demo finished. Goodbye!');
end;

// --- Main Program Block ---

var
  Demo: TDemo;

begin
  try
    Demo := TDemo.Create;
    try
      Demo.Run;
    finally
      Demo.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Fatal Error: ', E.ClassName, ' - ', E.Message);
      Readln;
    end;
  end;

end.
