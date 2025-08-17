unit uMakerAi.Agents;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  System.Threading, System.Rtti, System.SyncObjs;

type

  TMsgState = (msYes, msNo, msOK, msCancel, msAbort, msRetry, msIgnore, msAll, msNoToAll, msYesToAll, msHelp, msClose);
  TMsgStates = set of TMsgState;

  // --> NUEVO TIPO: Modo de unión para los nodos
  TJoinMode = (jmAny, jmAll);

  // Forward Declarations
  TAIAgents = class;
  TAIAgentsBase = class;
  TAIAgentsNode = class;
  TAIAgentsLink = class;
  TAIBlackboard = class;
  TAiToolBase = Class;


  // ... (Event Types sin cambios) ...
  TAIAgentsOnPrint = procedure(Sender: TObject; Value: String) of object;
  TAIAgentsOnEnd = procedure(Node: TAIAgentsNode; Value: string) of object;
  TAIAgentsNodeOnExecute = procedure(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: String; var Output: String) of object;
  TAIAgentsLinkOnExecute = procedure(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk: Boolean; var Handled: Boolean) of object;
  TAIAgentsOnError = procedure(Sender: TObject; Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception; var Abort: Boolean) of object;
  TAIAgentsOnConfirm = procedure(Sender: TObject; Node: TAIAgentsNode; const AQuestion: string; Buttons: TMsgStates; var AResponse: string;
    var AModalResult: TMsgState) of object;

  // --- Blackboard Class ---
  TAIBlackboard = class(TObject)
  private
    FData: TDictionary<string, TValue>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetValue(const AKey: string; const AValue: TValue);
    function TryGetValue(const AKey: string; out AValue: TValue): Boolean;
    procedure SetString(const AKey, AValue: string);
    function GetString(const AKey: string; const ADefault: string = ''): string;
    procedure SetInteger(const AKey: string; AValue: Integer);
    function GetInteger(const AKey: string; const ADefault: Integer = 0): Integer;
    procedure SetBoolean(const AKey: string; AValue: Boolean);
    function GetBoolean(const AKey: string; const ADefault: Boolean = False): Boolean;
  end;



 TAiToolBase = class(TComponent)
  private
    FDescription: string;
    procedure SetDescription(const Value: String);
  protected
    // Este es el método que cada herramienta específica deberá sobreescribir.
    // Es abstracto, forzando a las clases descendientes a implementarlo.
    procedure Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string); virtual; abstract;
  public
    // Este es el punto de entrada público. Puede ser usado para añadir
    // lógica común en el futuro (logging, error handling, etc.) antes de llamar a Execute.
    procedure Run(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
  published
    property Description: string read FDescription write SetDescription;
  end;

  // --> NUEVO: Clase de ejemplo para mostrar cómo se crearía una herramienta.
  // No hace nada útil aún, pero sirve como plantilla.
  TAiSampleTool = class(TAiToolBase)
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string); override;
  end;

  // --- Base Component Class ---
  TAIAgentsBase = class(TComponent)
  private
    FDescription: String;
    procedure SetDescription(const Value: String);
  Published
    Property Description: String read FDescription write SetDescription;
  end;

  // --- Link (Edge) Class ---
  TAIAgentsLink = class(TAIAgentsBase)
  private
    FNextNo: TAIAgentsNode;
    FNextB: TAIAgentsNode;
    FNextC: TAIAgentsNode;
    FNextA: TAIAgentsNode;
    FNextD: TAIAgentsNode;
    FGraph: TAIAgents;
    FOnExecute: TAIAgentsLinkOnExecute;
    FNoCicles: Integer;
    FMaxCicles: Integer;
    FReady: Boolean;
    FSourceNode: TAIAgentsNode;
    FConditionalTargets: TDictionary<string, TAIAgentsNode>;
    procedure SetNextA(const Value: TAIAgentsNode);
    procedure SetNextB(const Value: TAIAgentsNode);
    procedure SetNextC(const Value: TAIAgentsNode);
    procedure SetNextD(const Value: TAIAgentsNode);
    procedure SetNextNo(const Value: TAIAgentsNode);
    procedure SetGraph(const Value: TAIAgents);
    procedure SetOnExecute(const Value: TAIAgentsLinkOnExecute);
    procedure SetMaxCicles(const Value: Integer);
  protected
    property Ready: Boolean read FReady write FReady;
    property NoCicles: Integer read FNoCicles write FNoCicles;
    procedure DoExecuteDestinationOk(aError: Boolean; aMsgError: String);
    procedure DoExecuteDestinationNo;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Print(Value: String);
    procedure DoExecute(Sender: TAIAgentsNode);
    procedure AddConditionalTarget(const AKey: string; ANode: TAIAgentsNode);
  published
    property NextA: TAIAgentsNode read FNextA write SetNextA;
    property NextB: TAIAgentsNode read FNextB write SetNextB;
    property NextC: TAIAgentsNode read FNextC write SetNextC;
    property NextD: TAIAgentsNode read FNextD write SetNextD;
    property NextNo: TAIAgentsNode read FNextNo write SetNextNo;
    property Graph: TAIAgents read FGraph write SetGraph;
    property OnExecute: TAIAgentsLinkOnExecute read FOnExecute write SetOnExecute;
    property MaxCicles: Integer read FMaxCicles write SetMaxCicles default 1;
  end;

  // --- Node Class ---
  TAIAgentsNode = class(TAIAgentsBase)
  private
    FOutput: String;
    FInput: String;
    FNext: TAIAgentsLink;
    FGraph: TAIAgents;
    FInEdges: TList<TAIAgentsLink>;
    FOnExecute: TAIAgentsNodeOnExecute;
    FPromptName: String;
    FMsgError: String;
    FError: Boolean;
    FJoinLock: TCriticalSection;
    FArrivalCount: Integer;
    FJoinMode: TJoinMode;
    FTool: TAiToolBase;
    procedure SetInput(const Value: String);
    procedure SetNext(const Value: TAIAgentsLink);
    procedure SetOutput(const Value: String);
    procedure SetGraph(const Value: TAIAgents);
    procedure SetOnExecute(const Value: TAIAgentsNodeOnExecute);
    procedure SetPromptName(const Value: String);
    procedure SetJoinMode(const Value: TJoinMode);
    procedure SetTool(const Value: TAiToolBase);
  protected
    procedure DoExecute(aBeforeNode: TAIAgentsNode; aLink: TAIAgentsLink); virtual;
    procedure Reset;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Print(Value: String);
    procedure ForceFinalExecute;
    function RequestConfirmation(const AQuestion: string; Buttons: TMsgStates; var AResponse: string): TMsgState;
    function RequestInput(const ACaption, APrompt: string; var AValue: string): Boolean;
    property Error: Boolean read FError;
    property MsgError: String read FMsgError;
  published
    property Input: String read FInput write SetInput;
    property Output: String read FOutput write SetOutput;
    property Next: TAIAgentsLink read FNext write SetNext;
    property Graph: TAIAgents read FGraph write SetGraph;
    property OnExecute: TAIAgentsNodeOnExecute read FOnExecute write SetOnExecute;
    property PromptName: String read FPromptName write SetPromptName;
    property JoinMode: TJoinMode read FJoinMode write SetJoinMode default jmAny;
    property Tool : TAiToolBase read FTool write SetTool;
  end;

  // --- Main Orchestrator Class ---
  TAIAgents = class(TComponent)
  private
    FEndNode: TAIAgentsNode;
    FStartNode: TAIAgentsNode;
    FOnPrint: TAIAgentsOnPrint;
    FNodes: TList<TAIAgentsNode>;
    FLinks: TList<TAIAgentsLink>;
    FOnEnd: TAIAgentsOnEnd;
    FOnError: TAIAgentsOnError;
    FOnConfirm: TAIAgentsOnConfirm;
    FBusy: Boolean;
    FAbort: Boolean;
    FBlackboard: TAIBlackboard;
    FCompiled: Boolean;
    FActiveTasks: TList<ITask>;
    FActiveTasksLock: TCriticalSection;

    procedure SetEndNode(const Value: TAIAgentsNode);
    procedure SetStartNode(const Value: TAIAgentsNode);
    procedure SetOnPrint(const Value: TAIAgentsOnPrint);
    procedure SetOnEnd(const Value: TAIAgentsOnEnd);
    procedure SetOnError(const Value: TAIAgentsOnError);
    procedure SetOnConfirm(const Value: TAIAgentsOnConfirm);
  protected
    procedure DoPrint(Sender: TObject; Value: String);
    procedure AddComponentToList(AComponent: TAIAgentsBase);
    procedure RemoveComponentFromList(AComponent: TAIAgentsBase);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Abort;
    function Run(Msg: String): ITask;
    function FindNode(const AName: string): TAIAgentsNode;
    procedure DoError(Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception);
    function DoConfirm(Node: TAIAgentsNode; const AQuestion: string; Buttons: TMsgStates; var AResponse: string): TMsgState;
    // --- Runtime Builder API ---
    procedure ClearGraph;
    function AddNode(const AName: string; AExecuteProc: TAIAgentsNodeOnExecute): TAIAgents;
    function AddEdge(const AStartNodeName, AEndNodeName: string): TAIAgents;
    function AddConditionalEdge(const AStartNodeName: string; const AConditionalLinkName: string;
      AConditionalTargets: TDictionary<string, string>): TAIAgents;
    function SetEntryPoint(const ANodeName: string): TAIAgents;
    function SetFinishPoint(const ANodeName: string): TAIAgents;
    procedure Compile;
    // --- Properties ---
    property Busy: Boolean read FBusy;
    property Blackboard: TAIBlackboard read FBlackboard;
  published
    property StartNode: TAIAgentsNode read FStartNode write SetStartNode;
    property EndNode: TAIAgentsNode read FEndNode write SetEndNode;
    property OnPrint: TAIAgentsOnPrint read FOnPrint write SetOnPrint;
    property OnEnd: TAIAgentsOnEnd read FOnEnd write SetOnEnd;
    property OnError: TAIAgentsOnError read FOnError write SetOnError;
    property OnConfirm: TAIAgentsOnConfirm read FOnConfirm write SetOnConfirm;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAIAgents, TAIAgentsNode, TAIAgentsLink, TAiSampleTool]);
end;

{ TAIBlackboard }

procedure TAIBlackboard.Clear;
begin
  FLock.Enter;
  try
    FData.Clear;
  finally
    FLock.Leave;
  end;
end;

constructor TAIBlackboard.Create;
begin
  inherited;
  FData := TDictionary<string, TValue>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TAIBlackboard.Destroy;
begin
  FData.Free;
  FLock.Free;
  inherited;
end;

function TAIBlackboard.GetBoolean(const AKey: string; const ADefault: Boolean): Boolean;
var
  LValue: TValue;
begin
  if TryGetValue(AKey, LValue) and LValue.IsType<Boolean> then
    Result := LValue.AsBoolean
  else
    Result := ADefault;
end;

function TAIBlackboard.GetInteger(const AKey: string; const ADefault: Integer): Integer;
var
  LValue: TValue;
begin
  if TryGetValue(AKey, LValue) and LValue.IsType<Integer> then
    Result := LValue.AsInteger
  else
    Result := ADefault;
end;

function TAIBlackboard.GetString(const AKey, ADefault: string): string;
var
  LValue: TValue;
begin
  if TryGetValue(AKey, LValue) and LValue.IsType<string> then
    Result := LValue.AsString
  else
    Result := ADefault;
end;

procedure TAIBlackboard.SetBoolean(const AKey: string; AValue: Boolean);
begin
  SetValue(AKey, AValue);
end;

procedure TAIBlackboard.SetInteger(const AKey: string; AValue: Integer);
begin
  SetValue(AKey, AValue);
end;

procedure TAIBlackboard.SetString(const AKey, AValue: string);
begin
  SetValue(AKey, AValue);
end;

procedure TAIBlackboard.SetValue(const AKey: string; const AValue: TValue);
begin
  FLock.Enter;
  try
    FData.AddOrSetValue(AKey, AValue);
  finally
    FLock.Leave;
  end;
end;

function TAIBlackboard.TryGetValue(const AKey: string; out AValue: TValue): Boolean;
begin
  FLock.Enter;
  try
    Result := FData.TryGetValue(AKey, AValue);
  finally
    FLock.Leave;
  end;
end;

{ TAIAgents }

procedure TAIAgents.Abort;
begin
  FAbort := True;
end;

function TAIAgents.AddEdge(const AStartNodeName, AEndNodeName: string): TAIAgents;
var
  StartNode, EndNode: TAIAgentsNode;
  Link: TAIAgentsLink;
begin
  FCompiled := False;
  StartNode := FindNode(AStartNodeName);
  EndNode := FindNode(AEndNodeName);
  if not Assigned(StartNode) then
    raise Exception.CreateFmt('Edge creation failed: Start node "%s" not found.', [AStartNodeName]);
  if not Assigned(EndNode) then
    raise Exception.CreateFmt('Edge creation failed: End node "%s" not found.', [AEndNodeName]);

  Link := TAIAgentsLink.Create(Self);
  Link.Name := 'Link_' + AStartNodeName + '_to_' + AEndNodeName;
  Link.Graph := Self;
  StartNode.Next := Link;
  Link.NextA := EndNode;
  Result := Self;
end;

function TAIAgents.AddNode(const AName: string; AExecuteProc: TAIAgentsNodeOnExecute): TAIAgents;
var
  Node: TAIAgentsNode;
begin
  FCompiled := False;
  if FindNode(AName) <> nil then
    raise Exception.CreateFmt('A node with the name "%s" already exists.', [AName]);

  Node := TAIAgentsNode.Create(Self);
  Node.Name := AName;
  Node.Graph := Self;
  Node.OnExecute := AExecuteProc;
  Result := Self;
end;

procedure TAIAgents.AddComponentToList(AComponent: TAIAgentsBase);
begin
  if AComponent is TAIAgentsNode then
  begin
    if FNodes.IndexOf(TAIAgentsNode(AComponent)) = -1 then
      FNodes.Add(TAIAgentsNode(AComponent));
  end
  else if AComponent is TAIAgentsLink then
  begin
    if FLinks.IndexOf(TAIAgentsLink(AComponent)) = -1 then
      FLinks.Add(TAIAgentsLink(AComponent));
  end;
end;

function TAIAgents.AddConditionalEdge(const AStartNodeName, AConditionalLinkName: string; AConditionalTargets: TDictionary<string, string>)
  : TAIAgents;
var
  StartNode, TargetNode: TAIAgentsNode;
  Link: TAIAgentsLink;
  DecisionKey, TargetNodeName: string;
begin
  FCompiled := False;
  StartNode := FindNode(AStartNodeName);
  if not Assigned(StartNode) then
    raise Exception.CreateFmt('Conditional edge creation failed: Start node "%s" not found.', [AStartNodeName]);

  Link := TAIAgentsLink.Create(Self);
  Link.Name := AConditionalLinkName;
  Link.Graph := Self;

  for DecisionKey in AConditionalTargets.Keys do
  begin
    TargetNodeName := AConditionalTargets.Items[DecisionKey];
    TargetNode := FindNode(TargetNodeName);
    if not Assigned(TargetNode) then
      raise Exception.CreateFmt('Conditional edge creation failed: Target node "%s" for decision "%s" not found.',
        [TargetNodeName, DecisionKey]);
    Link.AddConditionalTarget(DecisionKey, TargetNode);
  end;

  StartNode.Next := Link;
  Result := Self;
end;

procedure TAIAgents.ClearGraph;
begin
  FStartNode := nil;
  FEndNode := nil;
  FNodes.Clear;
  FLinks.Clear;
  for var i := ComponentCount - 1 downto 0 do
  begin
    if Components[i] is TAIAgentsBase then
      Components[i].Free;
  end;
  FCompiled := False;
end;

procedure TAIAgents.Compile;
var
  Node: TAIAgentsNode;
  Link: TAIAgentsLink;
  TargetNode: TAIAgentsNode;
begin
  if FCompiled then
    Exit;
  try
    FAbort := False;
    FBlackboard.Clear;

    if not Assigned(FStartNode) then
      raise Exception.Create('StartNode is not assigned.');
    if not Assigned(FEndNode) then
      raise Exception.Create('EndNode is not assigned.');

    for Node in FNodes do
      Node.Reset; // <-- Importante: Resetea el estado de todos los nodos

    for Link in FLinks do
    begin
      Link.Ready := False;
      Link.NoCicles := 0;
      if not Assigned(Link.FSourceNode) then
        raise Exception.CreateFmt('Link "%s" is not connected from any source node.', [Link.Name]);

      if Assigned(Link.NextA) then
        Link.NextA.FInEdges.Add(Link);
      if Assigned(Link.NextB) then
        Link.NextB.FInEdges.Add(Link);
      if Assigned(Link.NextC) then
        Link.NextC.FInEdges.Add(Link);
      if Assigned(Link.NextD) then
        Link.NextD.FInEdges.Add(Link);
      if Assigned(Link.NextNo) then
        Link.NextNo.FInEdges.Add(Link);

      if Assigned(Link.FConditionalTargets) then
      begin
        for TargetNode in Link.FConditionalTargets.Values do
          TargetNode.FInEdges.Add(Link);
      end;
    end;
    FCompiled := True;
  except
    on E: Exception do
    begin
      DoError(nil, nil, E);
      raise;
    end;
  end;
end;

constructor TAIAgents.Create(aOwner: TComponent);
begin
  inherited;
  FBlackboard := TAIBlackboard.Create;
  FNodes := TList<TAIAgentsNode>.Create;
  FLinks := TList<TAIAgentsLink>.Create;
  FActiveTasks := TList<ITask>.Create;
  FActiveTasksLock := TCriticalSection.Create;
  FCompiled := False;
end;

destructor TAIAgents.Destroy;
begin
  FNodes.Free;
  FLinks.Free;
  FActiveTasks.Free;
  FActiveTasksLock.Free;
  FBlackboard.Free;
  inherited;
end;

function TAIAgents.DoConfirm(Node: TAIAgentsNode; const AQuestion: string; Buttons: TMsgStates; var AResponse: string): TMsgState;
begin
  Result := msCancel;
  if Assigned(FOnConfirm) then
  begin
    FOnConfirm(Self, Node, AQuestion, Buttons, AResponse, Result);
  end
  else
  begin
    DoError(Node, nil, Exception.Create('User confirmation required, but OnConfirm event is not assigned.'));
    Result := msAbort;
  end;
end;

procedure TAIAgents.DoError(Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception);
var
  LAbort: Boolean;
begin
  LAbort := True;
  if Assigned(FOnError) then
  begin
    FOnError(Self, Node, Link, E, LAbort);
  end;

  if LAbort then
    Abort;
end;

procedure TAIAgents.DoPrint(Sender: TObject; Value: String);
begin
  if Assigned(FOnPrint) then
    FOnPrint(Sender, Value);
end;

function TAIAgents.FindNode(const AName: string): TAIAgentsNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ComponentCount - 1 do
    if (Components[i] is TAIAgentsNode) and SameText(Components[i].Name, AName) then
    begin
      Result := TAIAgentsNode(Components[i]);
      Exit;
    end;
end;

procedure TAIAgents.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent is TAIAgentsBase) then
  begin
    if Operation = opInsert then
      AddComponentToList(TAIAgentsBase(AComponent))
    else if Operation = opRemove then
      RemoveComponentFromList(TAIAgentsBase(AComponent));
  end;
end;

procedure TAIAgents.RemoveComponentFromList(AComponent: TAIAgentsBase);
begin
  if AComponent is TAIAgentsNode then
    FNodes.Remove(TAIAgentsNode(AComponent))
  else if AComponent is TAIAgentsLink then
    FLinks.Remove(TAIAgentsLink(AComponent));
end;

function TAIAgents.Run(Msg: String): ITask;
begin
  if Busy then
  begin
    DoError(nil, nil, Exception.Create('The Agents is already busy.'));
    Result := nil;
    Exit;
  end;

  Compile;

  FBusy := True;
  FAbort := False;

  FActiveTasksLock.Enter;
  try
    FActiveTasks.Clear;
  finally
    FActiveTasksLock.Leave;
  end;

  Result := TTask.Run(
    procedure
    var
      TasksToWaitOn: TArray<ITask>;
    begin
      try
        try
          if FAbort then
            Exit;
          if Assigned(FStartNode) then
          begin
            FStartNode.Input := Msg;
            FStartNode.DoExecute(nil, nil);
          end;

          FActiveTasksLock.Enter;
          try
            TasksToWaitOn := FActiveTasks.ToArray;
          finally
            FActiveTasksLock.Leave;
          end;

          if Length(TasksToWaitOn) > 0 then
            TTask.WaitForAll(TasksToWaitOn);

          if not FAbort then
            Blackboard.SetString('Execution.Status', 'Completed');

        except
          on E: Exception do
          begin
            DoError(nil, nil, E);
          end;
        end;
      finally
        if FAbort and Assigned(FEndNode) then
        begin
          Blackboard.SetString('Execution.Status', 'Aborted');
          FEndNode.ForceFinalExecute;
        end;
        FBusy := False;
      end;
    end);
end;

procedure TAIAgents.SetEndNode(const Value: TAIAgentsNode);
begin
  if FEndNode <> Value then
  begin
    FEndNode := Value;
    FCompiled := False;
  end;
end;

function TAIAgents.SetEntryPoint(const ANodeName: string): TAIAgents;
begin
  Self.StartNode := FindNode(ANodeName);
  if not Assigned(Self.StartNode) then
    raise Exception.CreateFmt('Entry point node "%s" not found.', [ANodeName]);
  Result := Self;
end;

function TAIAgents.SetFinishPoint(const ANodeName: string): TAIAgents;
begin
  Self.EndNode := FindNode(ANodeName);
  if not Assigned(Self.EndNode) then
    raise Exception.CreateFmt('Finish point node "%s" not found.', [ANodeName]);
  Result := Self;
end;

procedure TAIAgents.SetOnConfirm(const Value: TAIAgentsOnConfirm);
begin
  FOnConfirm := Value;
end;

procedure TAIAgents.SetOnEnd(const Value: TAIAgentsOnEnd);
begin
  FOnEnd := Value;
end;

procedure TAIAgents.SetOnError(const Value: TAIAgentsOnError);
begin
  FOnError := Value;
end;

procedure TAIAgents.SetOnPrint(const Value: TAIAgentsOnPrint);
begin
  FOnPrint := Value;
end;

procedure TAIAgents.SetStartNode(const Value: TAIAgentsNode);
begin
  if FStartNode <> Value then
  begin
    FStartNode := Value;
    FCompiled := False;
  end;
end;

{ TAIAgentsLink }

procedure TAIAgentsLink.AddConditionalTarget(const AKey: string; ANode: TAIAgentsNode);
begin
  if not Assigned(FConditionalTargets) then
    FConditionalTargets := TDictionary<string, TAIAgentsNode>.Create;
  FConditionalTargets.Add(AKey, ANode);
end;

constructor TAIAgentsLink.Create(aOwner: TComponent);
begin
  inherited;
  FMaxCicles := 1;
end;

destructor TAIAgentsLink.Destroy;
begin
  if Assigned(FConditionalTargets) then
    FConditionalTargets.Free;
  inherited;
end;

procedure TAIAgentsLink.DoExecute(Sender: TAIAgentsNode);
var
  IsOk, Handled: Boolean;
  Decision: string;
  TargetNode: TAIAgentsNode;
begin
  if (FGraph = nil) or FGraph.FAbort then
    Exit;

  if Assigned(FConditionalTargets) and (FConditionalTargets.Count > 0) then
  begin
    Decision := FGraph.Blackboard.GetString('next_route');
    if FConditionalTargets.TryGetValue(Decision, TargetNode) then
    begin
      TargetNode.DoExecute(Sender, Self);
    end
    else
    begin
      FGraph.DoError(Sender, Self, Exception.CreateFmt('Conditional route decision "%s" is invalid for link "%s".', [Decision, Name]));
    end;
    Exit;
  end;

  Handled := False;
  if Assigned(FOnExecute) then
  begin
    IsOk := True; // OJO estaba en False, pero si no se asigna en el evento no continúa.
    try
      FOnExecute(Sender, Self, IsOk, Handled);
    except
      on E: Exception do
      begin
        FGraph.DoError(Sender, Self, E);
        Exit;
      end;
    end;
  end
  else
  begin
    IsOk := True;
  end;

  if Handled then
    Exit;

  if IsOk then
  begin
    DoExecuteDestinationOk(False, '');
  end
  else
  begin
    Inc(FNoCicles);
    if (FNoCicles >= FMaxCicles) and Assigned(FNextNo) then
    begin
      DoExecuteDestinationNo;
    end
    else if (FNoCicles >= FMaxCicles) then
    begin
      FGraph.DoError(Sender, Self, Exception.CreateFmt('Maximum cycles (%d) reached on link "%s".', [FMaxCicles, Name]));
    end
    else
    begin
      DoExecuteDestinationOk(False, '');
    end;
  end;
end;

procedure TAIAgentsLink.DoExecuteDestinationNo;
begin
  if Assigned(FNextNo) then
    FNextNo.DoExecute(FSourceNode, Self);
end;

procedure TAIAgentsLink.DoExecuteDestinationOk(aError: Boolean; aMsgError: String);
var
  NodesToRun: TList<TAIAgentsNode>;
  Node: TAIAgentsNode;
begin
  NodesToRun := TList<TAIAgentsNode>.Create;
  try
    if Assigned(FNextA) then
      NodesToRun.Add(FNextA);
    if Assigned(FNextB) then
      NodesToRun.Add(FNextB);
    if Assigned(FNextC) then
      NodesToRun.Add(FNextC);
    if Assigned(FNextD) then
      NodesToRun.Add(FNextD);

    if NodesToRun.Count = 1 then
    begin
      Node := NodesToRun[0];
      Node.FError := aError;
      Node.FMsgError := aMsgError;
      Node.DoExecute(FSourceNode, Self);
    end
    else if NodesToRun.Count > 1 then
    begin
      for Node in NodesToRun do
      begin
        var
        LNode := Node;
        var
        LTask := TTask.Run(
          procedure
          begin
            LNode.FError := aError;
            LNode.FMsgError := aMsgError;
            LNode.DoExecute(FSourceNode, Self);
          end);

        Sleep(50); // Este sleep le da tiempo al sistema para encolar las tareas.
        FGraph.FActiveTasksLock.Enter;
        try
          FGraph.FActiveTasks.Add(LTask);
        finally
          FGraph.FActiveTasksLock.Leave;
        end;
      end;
    end;
  finally
    NodesToRun.Free;
  end;
end;

procedure TAIAgentsLink.Print(Value: String);
begin
  if Assigned(FGraph) then
    FGraph.DoPrint(Self, Value);
end;

procedure TAIAgentsLink.SetGraph(const Value: TAIAgents);
begin
  if Value <> FGraph then
  begin
    FGraph := Value;
    if Assigned(FGraph) then
      FGraph.FCompiled := False;
  end;
end;

procedure TAIAgentsLink.SetMaxCicles(const Value: Integer);
begin
  FMaxCicles := Value;
  if FMaxCicles < 1 then
    FMaxCicles := 1;
end;

procedure TAIAgentsLink.SetNextA(const Value: TAIAgentsNode);
begin
  FNextA := Value;
end;

procedure TAIAgentsLink.SetNextB(const Value: TAIAgentsNode);
begin
  FNextB := Value;
end;

procedure TAIAgentsLink.SetNextC(const Value: TAIAgentsNode);
begin
  FNextC := Value;
end;

procedure TAIAgentsLink.SetNextD(const Value: TAIAgentsNode);
begin
  FNextD := Value;
end;

procedure TAIAgentsLink.SetNextNo(const Value: TAIAgentsNode);
begin
  FNextNo := Value;
end;

procedure TAIAgentsLink.SetOnExecute(const Value: TAIAgentsLinkOnExecute);
begin
  FOnExecute := Value;
end;

{ TAIAgentsNode }

constructor TAIAgentsNode.Create(aOwner: TComponent);
begin
  inherited;
  FInEdges := TList<TAIAgentsLink>.Create;
  FJoinLock := TCriticalSection.Create;
  FJoinMode := jmAny; // Default behavior
  FArrivalCount := 0;
end;

destructor TAIAgentsNode.Destroy;
begin
  FInEdges.Free;
  FJoinLock.Free;
  inherited;
end;

procedure TAIAgentsNode.Reset;
begin
  FInEdges.Clear;
  Input := '';
  Output := '';
  FError := False;
  FMsgError := '';
  FArrivalCount := 0;
end;

{procedure TAIAgentsNode.DoExecute(aBeforeNode: TAIAgentsNode; aLink: TAIAgentsLink);
var
  CanExecute: Boolean;
  CombinedInput: TStringBuilder;
begin
  if (FGraph = nil) or FGraph.FAbort then
    Exit;

  CanExecute := False;
  // Solo aplicamos la lógica de unión si el nodo tiene múltiples entradas.
  if FInEdges.Count > 1 then
  begin
    FJoinLock.Enter;
    try
      case FJoinMode of
        jmAny:
          begin
            // En modo 'Any', siempre se puede ejecutar.
            CanExecute := True;
          end;
        jmAll:
          begin
            // En modo 'All', comprobamos si han llegado todas las ramas.
            Inc(FArrivalCount);
            if FArrivalCount >= FInEdges.Count then
            begin
              CanExecute := True;
              // Una vez que se ejecuta, reseteamos el contador para futuras ejecuciones del grafo.
              FArrivalCount := 0;
            end
            else
            begin
              CanExecute := False; // Esperar a más ramas.
            end;
          end;
      end;
    finally
      FJoinLock.Leave;
    end;
  end
  else
  begin
    // Si tiene 0 o 1 entrada, siempre se puede ejecutar.
    CanExecute := True;
  end;

  // Si no se cumplen las condiciones de unión, el hilo termina aquí.
  if not CanExecute then
    Exit;

  // --- EJECUCIÓN DEL NODO ---
  CombinedInput := TStringBuilder.Create;
  try
    // Para un JoinMode=jmAll, tiene sentido combinar las entradas.
    // Para jmAny (ciclos), solo importa la entrada del activador.
    if (FJoinMode = jmAll) and (FInEdges.Count > 1) then
    begin
      // Aquí se puede implementar una lógica para combinar las salidas
      // de todos los FInEdges.SourceNode.Output, posiblemente usando el Blackboard.
      // Por ahora, usamos la entrada del último activador como antes.
      if aBeforeNode <> nil then
        Input := aBeforeNode.Output;
    end
    else
    begin
      // Para jmAny o nodos no-join, la entrada es simplemente la del nodo anterior.
      if aBeforeNode <> nil then
        Input := aBeforeNode.Output;
    end;
  finally
    CombinedInput.Free;
  end;

  try
    if Assigned(FOnExecute) then
      FOnExecute(Self, aBeforeNode, aLink, Self.Input, FOutput);

    if FOutput = '' then
      FOutput := Input;

    if Assigned(Next) then
    begin
      Next.FSourceNode := Self;
      Next.DoExecute(Self);
    end;
  except
    on E: Exception do
    begin
      FGraph.DoError(Self, aLink, E);
    end;
  end;
end;
}


procedure TAIAgentsNode.DoExecute(aBeforeNode: TAIAgentsNode; aLink: TAIAgentsLink);
var
  CanExecute: Boolean;
begin
  if (FGraph = nil) or FGraph.FAbort then
    Exit;

  CanExecute := False;
  // Lógica de unión (Join)
  if FInEdges.Count > 1 then
  begin
    FJoinLock.Enter;
    try
      case FJoinMode of
        jmAny: CanExecute := True;
        jmAll:
          begin
            Inc(FArrivalCount);
            if FArrivalCount >= FInEdges.Count then
            begin
              CanExecute := True;
              FArrivalCount := 0;
            end
            else
              CanExecute := False;
          end;
      end;
    finally
      FJoinLock.Leave;
    end;
  end
  else
    CanExecute := True;

  if not CanExecute then
    Exit;

  // Combinación de entradas (si es necesario)
  if aBeforeNode <> nil then
    Input := aBeforeNode.Output;
  // (La lógica de TStringBuilder se puede refinar si se combinan múltiples entradas para jmAll)

  try
    // --- LÓGICA DE EJECUCIÓN CON PRIORIDAD ---
    // 1. Si el evento OnExecute está asignado, tiene la máxima prioridad.
    if Assigned(FOnExecute) then
    begin
      FOnExecute(Self, aBeforeNode, aLink, Self.Input, FOutput)
    end
    // 2. Si no, y si hay una herramienta (Tool) asignada, se ejecuta la herramienta.
    else if Assigned(FTool) then
    begin
      FTool.Run(Self, Self.Input, FOutput);
    end;
    // 3. Si no hay ni evento ni herramienta, la salida será igual a la entrada por defecto.

    // Si la salida sigue vacía, la igualamos a la entrada para que el flujo continúe.
    if FOutput = '' then
      FOutput := Input;

    // Continuar con el siguiente eslabón (Link)
    if Assigned(Next) then
    begin
      Next.FSourceNode := Self;
      Next.DoExecute(Self);
    end;
  except
    on E: Exception do
    begin
      FGraph.DoError(Self, aLink, E);
    end;
  end;
end;


procedure TAIAgentsNode.ForceFinalExecute;
begin
  try
    if Assigned(FOnExecute) then
      FOnExecute(Self, nil, nil, Self.Input, FOutput);

    if Assigned(FGraph.OnEnd) then
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          FGraph.OnEnd(Self, Output);
        end);
    end;
  except
    on E: Exception do
    begin
      FGraph.DoError(Self, nil, E);
    end;
  end;
end;

procedure TAIAgentsNode.Print(Value: String);
begin
  if Assigned(FGraph) then
    FGraph.DoPrint(Self, Value);
end;

function TAIAgentsNode.RequestConfirmation(const AQuestion: string; Buttons: TMsgStates; var AResponse: string): TMsgState;
begin
  if Assigned(FGraph) then
    Result := FGraph.DoConfirm(Self, AQuestion, Buttons, AResponse)
  else
    Result := msAbort;
end;

function TAIAgentsNode.RequestInput(const ACaption, APrompt: string; var AValue: string): Boolean;
var
  Response: string;
  ModalResult: TMsgState;
begin
  Response := AValue;
  ModalResult := RequestConfirmation(ACaption + '|' + APrompt, [msOK, msCancel], Response);
  if ModalResult = msOK then
  begin
    AValue := Response;
    Result := True;
  end
  else
    Result := False;
end;

procedure TAIAgentsNode.SetGraph(const Value: TAIAgents);
begin
  if Value <> FGraph then
  begin
    if Assigned(FGraph) then
      FGraph.RemoveComponentFromList(Self);
    FGraph := Value;
    if Assigned(FGraph) then
    begin
      FGraph.AddComponentToList(Self);
      FGraph.FCompiled := False;
    end;
  end;
end;

procedure TAIAgentsNode.SetInput(const Value: String);
begin
  FInput := Value;
end;

procedure TAIAgentsNode.SetJoinMode(const Value: TJoinMode);
begin
  FJoinMode := Value;
end;

procedure TAIAgentsNode.SetNext(const Value: TAIAgentsLink);
begin
  if Value <> FNext then
  begin
    if Assigned(FNext) then
      FNext.FSourceNode := nil;
    FNext := Value;
    if Assigned(FNext) then
      FNext.FSourceNode := Self;
    if Assigned(Graph) then
      Graph.FCompiled := False;
  end;
end;

procedure TAIAgentsNode.SetOnExecute(const Value: TAIAgentsNodeOnExecute);
begin
  FOnExecute := Value;
end;

procedure TAIAgentsNode.SetOutput(const Value: String);
begin
  FOutput := Value;
end;

procedure TAIAgentsNode.SetPromptName(const Value: String);
begin
  FPromptName := Value;
end;

procedure TAIAgentsNode.SetTool(const Value: TAiToolBase);
begin

  FTool := Value;
end;

{ TAIAgentsBase }

procedure TAIAgentsBase.SetDescription(const Value: String);
begin
  FDescription := Value;
end;



{ TAiToolBase }

procedure TAiToolBase.Run(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
begin
  // Wrapper para llamar al método virtual.
  // Aquí se podría añadir telemetría, logging, etc. en el futuro.
  Execute(ANode, AInput, AOutput);
end;

procedure TAiToolBase.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

{ TAiSampleTool }

procedure TAiSampleTool.Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
begin
  // --- Implementación de ejemplo ---
  // En el futuro, aquí iría la lógica real de la herramienta.
  // Por ejemplo, leer un archivo, procesar un texto, llamar a una API, etc.
  // Por ahora, solo simula un trabajo y lo notifica.
  ANode.Print(Format('TAiSampleTool: Executing with input "%s"', [AInput]));

  // Asigna un resultado de ejemplo a la salida
  AOutput := 'Output from TAiSampleTool: ' + AInput;
end;

end.
