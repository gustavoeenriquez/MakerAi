unit uMakerAi.Graph;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  System.Threading, uMakerAi.Prompts;

type
  TAIGraph = class;
  TAIGraphBase = Class;
  TAIGraphNode = class;
  TAIGraphLink = class;

  TAIGraphNodeProc = Reference to procedure(Sender: TObject; Var IsOk: Boolean);
  TAIGraphOnPrint = Procedure(Sender: TObject; Value: String) of object;
  TAIGraphOnEnd = Procedure(Node: TAIGraphNode; Value: String) of object;
  TAIGraphNodeOnExecute = Procedure(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: String; Var Output: String) of object;
  TAIGraphLinkOnExecute = Procedure(Node: TAIGraphNode; Link: TAIGraphLink; Var IsOk: Boolean; Var Handled: Boolean) of object;

  TAIGraphBase = Class(TComponent)
  Private
  Protected
  Public
  End;

  TAIGraphLink = class(TAIGraphBase)
  private
    FNextNo: TAIGraphNode;
    FNextB: TAIGraphNode;
    FNextC: TAIGraphNode;
    FNextA: TAIGraphNode;
    FNextD: TAIGraphNode;
    FGraph: TAIGraph;
    FOnExecute: TAIGraphLinkOnExecute;
    FNoCicles: Integer;
    FMaxCicles: Integer;
    FReady: Boolean;
    FResponse: String;
    procedure SetNextA(const Value: TAIGraphNode);
    procedure SetNextB(const Value: TAIGraphNode);
    procedure SetNextC(const Value: TAIGraphNode);
    procedure SetNextD(const Value: TAIGraphNode);
    procedure SetNextNo(const Value: TAIGraphNode);
    procedure SetGraph(const Value: TAIGraph);
    procedure SetOnExecute(const Value: TAIGraphLinkOnExecute);
    procedure SetNoCicles(const Value: Integer);
    procedure SetMaxCicles(const Value: Integer);
    procedure SetReady(const Value: Boolean);
    // procedure SetResponse(const Value: String);
    Procedure DoExecuteDestinationOk(Link: TAIGraphLink; aError: Boolean; aMsgError: String);
    Procedure DoExecuteDestinationNo(Link: TAIGraphLink);
    { Private declarations }
  protected
    { Protected declarations }
    FSourceNode: TAIGraphNode;
    Property NoCicles: Integer read FNoCicles write SetNoCicles;
    Property Ready: Boolean read FReady write SetReady;
  public
    Constructor Create(aOwner: TComponent); Override;
    Procedure Print(Value: String);
    Procedure DoExecute(Sender: TAIGraphNode);
  published
    Property NextA: TAIGraphNode read FNextA write SetNextA;
    Property NextB: TAIGraphNode read FNextB write SetNextB;
    Property NextC: TAIGraphNode read FNextC write SetNextC;
    Property NextD: TAIGraphNode read FNextD write SetNextD;
    Property NextNo: TAIGraphNode read FNextNo write SetNextNo;
    Property Graph: TAIGraph read FGraph write SetGraph;
    Property OnExecute: TAIGraphLinkOnExecute read FOnExecute write SetOnExecute;
    Property MaxCicles: Integer read FMaxCicles write SetMaxCicles;
    // Property Response: String read FResponse write SetResponse;
  end;

  TAIGraphNode = class(TAIGraphBase)
  private
    FOutput: String;
    FInput: String;
    FNext: TAIGraphLink;
    FGraph: TAIGraph;
    FInEdges: TList<TAIGraphLink>;
    FOnExecute: TAIGraphNodeOnExecute;
    FPromptName: String;
    FMsgError: String;
    FError: Boolean;
    procedure SetInput(const Value: String);
    procedure SetNext(const Value: TAIGraphLink);
    procedure SetOutput(const Value: String);
    procedure SetGraph(const Value: TAIGraph);
    procedure SetOnExecute(const Value: TAIGraphNodeOnExecute);
    procedure SetPromptName(const Value: String);
    { Private declarations }
  protected
    Procedure DoExecute(aNode: TAIGraphNode; aLink: TAIGraphLink); Virtual;
  public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure Print(Value: String);
    Property Error: Boolean read FError;
    Property MsgError: String read FMsgError;
  published
    Property Input: String read FInput write SetInput;
    Property Output: String read FOutput write SetOutput;
    Property Next: TAIGraphLink read FNext write SetNext;
    Property Graph: TAIGraph read FGraph write SetGraph;
    Property OnExecute: TAIGraphNodeOnExecute read FOnExecute write SetOnExecute;
    Property PromptName: String read FPromptName write SetPromptName;
  end;

  TAIGraph = class(TComponent)
  private
    FEndNode: TAIGraphNode;
    FStartNode: TAIGraphNode;
    FOnPrint: TAIGraphOnPrint;
    FNodes: TList<TAIGraphNode>;
    FLinks: TList<TAIGraphLink>;
    FAiPrompts: TAiPrompts;
    FOnEnd: TAIGraphOnEnd;
    FBusy: Boolean;
    FAbort: Boolean;
    procedure SetEndNode(const Value: TAIGraphNode);
    procedure SetStartNode(const Value: TAIGraphNode);
    procedure SetOnPrint(const Value: TAIGraphOnPrint);
    procedure SetAiPrompts(const Value: TAiPrompts);
    procedure SetOnEnd(const Value: TAIGraphOnEnd);
    { Private declarations }
  protected
    Procedure DoPrint(Sender: TObject; Value: String);
    Procedure AddNode(Node: TAIGraphBase);
    Procedure RemoveNode(Node: TAIGraphBase);
    Procedure InitGrafo;
  public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function Run(Msg: String): String;
    Procedure Abort;

    Property Busy: Boolean read FBusy;
  published
    Property StartNode: TAIGraphNode read FStartNode write SetStartNode;
    Property EndNode: TAIGraphNode read FEndNode write SetEndNode;
    Property OnPrint: TAIGraphOnPrint read FOnPrint write SetOnPrint;
    Property AiPrompts: TAiPrompts read FAiPrompts write SetAiPrompts;
    Property OnEnd: TAIGraphOnEnd read FOnEnd write SetOnEnd;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAIGraph, TAIGraphNode, TAIGraphLink]);
end;

{ TAIGraph }

procedure TAIGraph.Abort;
begin
   FAbort := True;
end;

procedure TAIGraph.AddNode(Node: TAIGraphBase);
begin
  If Node is TAIGraphNode then
  Begin
    If FNodes.IndexOf(TAIGraphNode(Node)) <= 0 then
      FNodes.Add(TAIGraphNode(Node));
  End;

  If Node is TAIGraphLink then
  Begin
    If FLinks.IndexOf(TAIGraphLink(Node)) <= 0 then
      FLinks.Add(TAIGraphLink(Node));
  End;
end;

procedure TAIGraph.RemoveNode(Node: TAIGraphBase);
begin

end;

constructor TAIGraph.Create(aOwner: TComponent);
begin
  inherited;
  FNodes := TList<TAIGraphNode>.Create;
  FLinks := TList<TAIGraphLink>.Create;
end;

destructor TAIGraph.Destroy;
begin
  FNodes.Free;
  FLinks.Free;
  inherited;
end;

procedure TAIGraph.DoPrint(Sender: TObject; Value: String);
begin
  If Assigned(FOnPrint) then
    FOnPrint(Sender, Value);
end;

procedure TAIGraph.InitGrafo;
Var
  Nodo: TAIGraphNode;
  Link: TAIGraphLink;
begin

  FAbort := False;

  For Nodo in FNodes do
  Begin
    Nodo.FInEdges.Clear;
    Nodo.Input := '';
    Nodo.Output := '';
    Nodo.FError := False;
    Nodo.FMsgError := '';

    If (Nodo <> FEndNode) and (Not Assigned(Nodo.Next)) then
      Raise Exception.Create('El nodo "' + Nodo.Name + '" no tiene asignado el link Next correspondiente');
  End;

  For Link in FLinks do
  Begin
    Link.Ready := False;
    Link.NoCicles := 1;
    // Link.Response := '';

    If Not Assigned(Link.FSourceNode) then
      Raise Exception.Create('El link "' + Link.Name + '" no está enlazado correctamente');

    If Assigned(Link.NextA) then // Debe agregar todos los Links InEdges de cada nodo
      Link.NextA.FInEdges.Add(Link);

    If Assigned(Link.NextB) then // Debe agregar todos los Links InEdges de cada nodo
      Link.NextB.FInEdges.Add(Link);

    If Assigned(Link.NextC) then // Debe agregar todos los Links InEdges de cada nodo
      Link.NextC.FInEdges.Add(Link);

    If Assigned(Link.NextD) then // Debe agregar todos los Links InEdges de cada nodo
      Link.NextD.FInEdges.Add(Link);

    If Assigned(Link.FNextNo) then // Debe agregar todos los Links InEdges de cada nodo
      Link.FNextNo.FInEdges.Add(Link);
  End;
end;

function TAIGraph.Run(Msg: String): String;
begin
  If Not Assigned(FStartNode) then
    Raise Exception.Create('Falta asignar el nodo inicial StartNode');

  If Not Assigned(FEndNode) then
    Raise Exception.Create('Falta asignar el nodo final EndNode');

  InitGrafo;

  FBusy := True;
  TTask.Run(
    Procedure
    Begin

      If Assigned(FStartNode) then
      Begin
        FStartNode.Input := Msg;
        FStartNode.DoExecute(Nil, Nil);
      End;
    End);

  // While Busy do
  // Sleep(100);

  Result := FEndNode.Output;
end;

procedure TAIGraph.SetAiPrompts(const Value: TAiPrompts);
begin
  FAiPrompts := Value;
end;

procedure TAIGraph.SetEndNode(const Value: TAIGraphNode);
begin
  FEndNode := Value;
end;

procedure TAIGraph.SetOnEnd(const Value: TAIGraphOnEnd);
begin
  FOnEnd := Value;
end;

procedure TAIGraph.SetOnPrint(const Value: TAIGraphOnPrint);
begin
  FOnPrint := Value;
end;

procedure TAIGraph.SetStartNode(const Value: TAIGraphNode);
begin
  FStartNode := Value;
end;

{ TAIGraphLink }

constructor TAIGraphLink.Create(aOwner: TComponent);
begin
  inherited;
  FNoCicles := 0;
  FMaxCicles := 1;
end;

procedure TAIGraphLink.DoExecute(Sender: TAIGraphNode);
Var
  IsOk, Handled: Boolean;
begin
  If FGraph.FAbort then
    Exit;

  IsOk := False;
  Handled := False;

  If (Not Assigned(FNextNo)) then
  Begin
    DoExecuteDestinationOk(Self, False, '');
  End
  else If (FNoCicles >= FMaxCicles) then
  Begin
    DoExecuteDestinationOk(Self, True, 'Superado el máximo número de ciclos');
  End
  else
  Begin
    If Assigned(FOnExecute) then
    Begin
      FOnExecute(Sender, Self, IsOk, Handled);
      If Handled = False then
      Begin
        If IsOk then
        Begin
          DoExecuteDestinationOk(Self, False, '');
        End
        Else If (FNoCicles >= FMaxCicles) then
        Begin
          DoExecuteDestinationOk(Self, True, 'Superado el máximo número de ciclos');
        End
        Else
        Begin
          Inc(FNoCicles);
          DoExecuteDestinationNo(Self);
        End;
      End;
    End
    else
    Begin
      Inc(FNoCicles);
      DoExecuteDestinationNo(Self);
    End;
  End;
end;

procedure TAIGraphLink.DoExecuteDestinationNo(Link: TAIGraphLink);
begin
  If Assigned(FNextNo) then
    FNextNo.DoExecute(FSourceNode, Self);
end;

procedure TAIGraphLink.DoExecuteDestinationOk(Link: TAIGraphLink; aError: Boolean; aMsgError: String);
begin
  If Assigned(FNextA) then
  Begin
    FNextA.FError := aError;
    FNextA.FMsgError := aMsgError;
    FNextA.DoExecute(FSourceNode, Self);
  End;

  If Assigned(FNextB) then
  Begin
    FNextB.DoExecute(FSourceNode, Self);
  End;

  If Assigned(FNextC) then
  Begin
    FNextC.DoExecute(FSourceNode, Self);
  End;

  If Assigned(FNextD) then
  Begin
    FNextD.DoExecute(FSourceNode, Self);
  End;
end;

procedure TAIGraphLink.Print(Value: String);
begin
  If Assigned(FGraph) then
    FGraph.DoPrint(Self, Value);
end;

procedure TAIGraphLink.SetGraph(const Value: TAIGraph);
begin
  If Value <> FGraph then
  Begin
    If Assigned(FGraph) then // Si ya está asignado el FGraph, primero remueve el link del Graph y luego adiciona al nuevo
    Begin
      FGraph.RemoveNode(Self);
    End;

    Value.AddNode(Self);
    FGraph := Value;
  End;
end;

procedure TAIGraphLink.SetMaxCicles(const Value: Integer);
begin
  If Value <> FMaxCicles then
  Begin
    If FMaxCicles <= 0 then
      FMaxCicles := 1
    Else
      FMaxCicles := Value;
  End;
end;

procedure TAIGraphLink.SetNextA(const Value: TAIGraphNode);
begin
  FNextA := Value;
end;

procedure TAIGraphLink.SetNextB(const Value: TAIGraphNode);
begin
  FNextB := Value;
end;

procedure TAIGraphLink.SetNextC(const Value: TAIGraphNode);
begin
  FNextC := Value;
end;

procedure TAIGraphLink.SetNextD(const Value: TAIGraphNode);
begin
  FNextD := Value;
end;

procedure TAIGraphLink.SetNextNo(const Value: TAIGraphNode);
begin
  FNextNo := Value;
end;

procedure TAIGraphLink.SetNoCicles(const Value: Integer);
begin
  If Value < 0 then
    FNoCicles := 0
  Else
    FNoCicles := Value;
end;

procedure TAIGraphLink.SetOnExecute(const Value: TAIGraphLinkOnExecute);
begin
  FOnExecute := Value;
end;

procedure TAIGraphLink.SetReady(const Value: Boolean);
begin
  FReady := Value;
end;

{ procedure TAIGraphLink.SetResponse(const Value: String);
  begin
  FResponse := Value;
  end;
}

{ TAIGraphNode }

constructor TAIGraphNode.Create(aOwner: TComponent);
begin
  FInEdges := TList<TAIGraphLink>.Create;
  inherited;
end;

destructor TAIGraphNode.Destroy;
begin
  FInEdges.Free;
  inherited;
end;

procedure TAIGraphNode.DoExecute(aNode: TAIGraphNode; aLink: TAIGraphLink);
Var
  Link: TAIGraphLink;
  Ready, IsOk, IsCicle: Boolean;
  texto, TextoCic: String;
begin
  // Si llegan varios nodos al tiempo, debe esperar que todos estén ejecutados, excepto si es un ciclo

  If FGraph.FAbort then
  Begin
    Self.FError := True;
    Self.FMsgError := 'Proceso cancelado por el usuario';

    If Assigned(FGraph.OnEnd) then
    Begin
      FGraph.FBusy := False;
      FGraph.OnEnd(Self, Self.FMsgError);
    End;
    Exit;
  End;

  Ready := True;
  IsCicle := False;

  If FInEdges.Count > 0 then
  Begin

    For Link in Self.FInEdges do
    Begin
      IsCicle := IsCicle or Assigned(Link.NextNo); // Si tiene un desvió por nextno es un ciclo
      Ready := Ready and Link.Ready;
    End;

    For Link in Self.FInEdges do
    Begin
      If (texto <> '') or (Link.FSourceNode.Output <> '') then
      Begin
        If Assigned(Link.FNextNo) then
          TextoCic := TextoCic + ' ' + Trim(Link.FSourceNode.Output)
        Else
          texto := texto + ' ' + Trim(Link.FSourceNode.Output);
      End;
    End;
  End
  Else
  Begin
    texto := Input;
  End;

  If Ready or IsCicle then
  Begin
    If IsCicle and (Trim(TextoCic) <> '') then
      Input := TextoCic
    else
      Input := texto;

    If Assigned(FOnExecute) then
      FOnExecute(Self, aNode, aLink, Input, FOutput);

    If Output = '' then // Si el nodo no realizó ninguna acción pasa el mensaje simplemente
      Output := Input;

    If Assigned(Next) then
    Begin
      Next.Ready := True; // Si llegó hasta aquí es porque el nodo está ok.
      Next.DoExecute(Self);
    End;
  End;

  If Assigned(FGraph) and Assigned(FGraph.EndNode) and (FGraph.EndNode = Self) then
  Begin
    If Assigned(FGraph.OnEnd) then
    Begin
      FGraph.FBusy := False;
      FGraph.OnEnd(Self, Output);
    End;
  End;
end;

procedure TAIGraphNode.Print(Value: String);
begin
  If Assigned(FGraph) then
    FGraph.DoPrint(Self, Value);
end;

procedure TAIGraphNode.SetGraph(const Value: TAIGraph);
begin
  If Value <> FGraph then
  Begin
    If Assigned(FGraph) then // Si ya está asignado el FGraph, primero remueve el link del Graph y luego adiciona al nuevo
    Begin
      FGraph.RemoveNode(Self);
    End;

    Value.AddNode(Self);
    FGraph := Value;
  End;
end;

procedure TAIGraphNode.SetInput(const Value: String);
begin
  FInput := Value;
end;

procedure TAIGraphNode.SetNext(const Value: TAIGraphLink);
begin

  If Value <> FNext then
  Begin
    If Assigned(FNext) then // Si ya está asignado el FGraph, primero remueve el link del Graph y luego adiciona al nuevo
    Begin
      FNext.FSourceNode := Nil;
    End;
    Value.FSourceNode := Self;
    FNext := Value;
  End;
end;

procedure TAIGraphNode.SetOnExecute(const Value: TAIGraphNodeOnExecute);
begin
  FOnExecute := Value;
end;

procedure TAIGraphNode.SetOutput(const Value: String);
begin
  FOutput := Value;
end;

procedure TAIGraphNode.SetPromptName(const Value: String);
begin
  FPromptName := Value;
end;

end.
