// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Framework de orquestacion de agentes basado en grafo.
//
// Adaptaciones FPC:
//   - ITask/TTask.Run/TThreadPool → TAiNodeTask + TAiOrchestratorThread
//   - TDictionary<K,V>  → specialize TDictionary<K,V> (generics.collections)
//   - TAIBlackboard usa Variant en lugar de TValue + campos FAskMsg/FResMsg
//   - TThread.Synchronize(nil, proc) → llamada directa (sin GUI assumption)
//   - TThread.Queue(nil, proc)       → llamada directa
//   - TInterlocked.Exchange(FBusy)   → TCriticalSection
//   - TInterlocked.Increment/Decrement → InterlockedIncrement/Decrement (System)
//   - System.Bindings (lmExpression) → not implemented (raise exception)
//   - TStringBuilder → string concatenation
//   - TStreamReader/TStreamWriter → TStringStream
//   - GetEnumName/GetEnumValue → TypInfo (same in FPC)

unit uMakerAi.Agents;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs, TypInfo, Variants,
  generics.collections,
  fpjson, jsonparser,
  uMakerAi.Chat, uMakerAi.Core, uMakerAi.Chat.Messages,
  uMakerAi.Agents.Checkpoint;

type

  TMsgState = (msYes, msNo, msOK, msCancel, msAbort, msRetry, msIgnore,
               msAll, msNoToAll, msYesToAll, msHelp, msClose);
  TMsgStates = set of TMsgState;

  TAgentExecutionStatus = (esUnknown, esRunning, esCompleted, esError,
                           esTimeout, esAborted, esSuspended);

  TJoinMode = (jmAny, jmAll);
  TLinkMode = (lmFanout, lmConditional, lmManual, lmExpression);

  // Forward declarations
  TAIAgentManager  = class;
  TAIAgentsBase    = class;
  TAIAgentsNode    = class;
  TAIAgentsLink    = class;
  TAIBlackboard    = class;
  TAiToolBase      = class;

  TAIAgentsOnPrint   = procedure(Sender: TObject; Value: string) of object;
  TAIAgentsOnEnd     = procedure(Node: TAIAgentsNode; Value: string) of object;
  TAIAgentsNodeOnExecute = procedure(Node, BeforeNode: TAIAgentsNode;
                                     Link: TAIAgentsLink;
                                     Input: string; var Output: string) of object;
  TAIAgentsLinkOnExecute = procedure(Node: TAIAgentsNode; Link: TAIAgentsLink;
                                     var IsOk: Boolean; var Handled: Boolean) of object;
  TAIAgentsOnError   = procedure(Sender: TObject; Node: TAIAgentsNode;
                                 Link: TAIAgentsLink; E: Exception;
                                 var Abort: Boolean) of object;
  TAIAgentsOnConfirm = procedure(Sender: TObject; Node: TAIAgentsNode;
                                 const AQuestion: string; Buttons: TMsgStates;
                                 var AResponse: string;
                                 var AModalResult: TMsgState) of object;
  TAIAgentsOnEnterNode = procedure(Sender: TObject; Node: TAIAgentsNode) of object;
  TAIAgentsOnExitNode  = procedure(Sender: TObject; Node: TAIAgentsNode) of object;
  TAIAgentsOnStart     = procedure(Sender: TObject; const Input: string) of object;
  TAIAgentsOnFinish    = procedure(Sender: TObject; const Input, Output: string;
                                   Status: TAgentExecutionStatus; E: Exception) of object;
  TAIAgentsOnSuspend   = procedure(Sender: TObject;
                                   const AThreadID: string;
                                   const ANodeName:  string;
                                   const AReason:    string;
                                   const AContext:   string) of object;

  // -------------------------------------------------------------------------
  // TAIBlackboard — estado compartido del grafo, thread-safe
  // FPC: usa Variant (en lugar de TValue) + campos separados para mensajes
  // -------------------------------------------------------------------------
  TAIBlackboard = class
  private
    FLock:    TCriticalSection;
    FData:    specialize TDictionary<string, Variant>;
    FAskMsg:  TAiChatMessage;  // owned
    FResMsg:  TAiChatMessage;  // owned
    function  GetAskMsg: TAiChatMessage;
    function  GetResMsg: TAiChatMessage;
    procedure SetAskMsg(const Value: TAiChatMessage);
    procedure SetResMsg(const Value: TAiChatMessage);
  protected
    // Acceso interno para serializacion (bajo FLock del caller)
    function GetDataForSerialization: specialize TDictionary<string, Variant>;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   SetValue(const AKey: string; const AValue: Variant);
    function    TryGetValue(const AKey: string; out AValue: Variant): Boolean;
    procedure   SetString(const AKey, AValue: string);
    function    GetString(const AKey: string; const ADefault: string = ''): string;
    procedure   SetInteger(const AKey: string; AValue: Integer);
    function    GetInteger(const AKey: string; const ADefault: Integer = 0): Integer;
    procedure   SetBoolean(const AKey: string; AValue: Boolean);
    function    GetBoolean(const AKey: string; const ADefault: Boolean = False): Boolean;
    procedure   SetStatus(Value: TAgentExecutionStatus);
    function    GetStatus: TAgentExecutionStatus;
    property    AskMsg: TAiChatMessage read GetAskMsg write SetAskMsg;
    property    ResMsg: TAiChatMessage read GetResMsg write SetResMsg;
    // Acceso con lock para serializacion (FLock expuesto solo para SerializeBlackboard)
    property    Lock:   TCriticalSection read FLock;
  end;

  // -------------------------------------------------------------------------
  // TAiToolBase — base para herramientas de nodo
  // -------------------------------------------------------------------------
  TAiToolBase = class(TComponent)
  private
    FDescription: string;
    FID:          string;
    procedure SetDescription(const Value: string);
    procedure SetID(const Value: string);
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string;
                      var AOutput: string); virtual; abstract;
  public
    procedure Run(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
  published
    property Description: string read FDescription write SetDescription;
    property ID:          string read FID          write SetID;
  end;

  TAiAgentsToolSample = class(TAiToolBase)
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string;
                      var AOutput: string); override;
  end;

  // -------------------------------------------------------------------------
  // TAIAgentsBase — componente base para nodos y enlaces
  // -------------------------------------------------------------------------
  TAIAgentsBase = class(TComponent)
  private
    FDescription: string;
    FID:          string;
    procedure SetDescription(const Value: string);
    procedure SetID(const Value: string);
  published
    property Description: string read FDescription write SetDescription;
    property ID:          string read FID          write SetID;
  end;

  // -------------------------------------------------------------------------
  // TAIAgentsLink — arista del grafo
  // -------------------------------------------------------------------------
  TAIAgentsLinkNodeMap = specialize TDictionary<string, TAIAgentsNode>;
  TAIAgentsNodeList    = specialize TList<TAIAgentsNode>;

  TAIAgentsLink = class(TAIAgentsBase)
  private
    FNextA:             TAIAgentsNode;
    FNextB:             TAIAgentsNode;
    FNextC:             TAIAgentsNode;
    FNextD:             TAIAgentsNode;
    FNextNo:            TAIAgentsNode;
    FGraph:             TAIAgentManager;
    FOnExecute:         TAIAgentsLinkOnExecute;
    FNoCycles:          Integer;
    FMaxCycles:         Integer;
    FReady:             Boolean;
    FSourceNode:        TAIAgentsNode;
    FConditionalTargets: TAIAgentsLinkNodeMap;
    FMode:              TLinkMode;
    FConditionalKey:    string;
    FManualTargetsKey:  string;
    FExpressionA:       string;
    FExpressionB:       string;
    FExpressionC:       string;
    FExpressionD:       string;
    procedure SetNextA(const Value: TAIAgentsNode);
    procedure SetNextB(const Value: TAIAgentsNode);
    procedure SetNextC(const Value: TAIAgentsNode);
    procedure SetNextD(const Value: TAIAgentsNode);
    procedure SetNextNo(const Value: TAIAgentsNode);
    procedure SetGraph(const Value: TAIAgentManager);
    procedure SetOnExecute(const Value: TAIAgentsLinkOnExecute);
    procedure SetMaxCycles(const Value: Integer);
    procedure SetMode(const Value: TLinkMode);
  protected
    property Ready: Boolean read FReady write FReady;
    procedure BuildManualTargets(const TargetsCSV: string;
                                  out Nodes: TAIAgentsNodeList);
    procedure CreateAndQueueTask(ANodeToExecute, ASourceNode: TAIAgentsNode;
                                  ACurrentLink: TAIAgentsLink);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Print(Value: string);
    procedure   DoExecute(Sender: TAIAgentsNode);
    procedure   AddConditionalTarget(const AKey: string; ANode: TAIAgentsNode);
    property    NoCycles: Integer read FNoCycles write FNoCycles;
  published
    property NextA:           TAIAgentsNode      read FNextA           write SetNextA;
    property NextB:           TAIAgentsNode      read FNextB           write SetNextB;
    property NextC:           TAIAgentsNode      read FNextC           write SetNextC;
    property NextD:           TAIAgentsNode      read FNextD           write SetNextD;
    property NextNo:          TAIAgentsNode      read FNextNo          write SetNextNo;
    property Graph:           TAIAgentManager    read FGraph           write SetGraph;
    property OnExecute:       TAIAgentsLinkOnExecute read FOnExecute   write SetOnExecute;
    property MaxCycles:       Integer            read FMaxCycles       write SetMaxCycles default 1;
    property Mode:            TLinkMode          read FMode            write SetMode      default lmFanout;
    property ConditionalKey:  string             read FConditionalKey  write FConditionalKey;
    property ManualTargetsKey:string             read FManualTargetsKey write FManualTargetsKey;
    property ExpressionA:     string             read FExpressionA     write FExpressionA;
    property ExpressionB:     string             read FExpressionB     write FExpressionB;
    property ExpressionC:     string             read FExpressionC     write FExpressionC;
    property ExpressionD:     string             read FExpressionD     write FExpressionD;
  end;

  // -------------------------------------------------------------------------
  // TAIAgentsNode — vertice del grafo
  // -------------------------------------------------------------------------
  TAIAgentsLinkList    = specialize TList<TAIAgentsLink>;
  TAIAgentsJoinInputs  = specialize TDictionary<TAIAgentsLink, string>;

  TAIAgentsNode = class(TAIAgentsBase)
  private
    FOutput:         string;
    FInput:          string;
    FNext:           TAIAgentsLink;
    FGraph:          TAIAgentManager;
    FInEdges:        TAIAgentsLinkList;
    FOnExecute:      TAIAgentsNodeOnExecute;
    FPromptName:     string;
    FMsgError:       string;
    FError:          Boolean;
    FJoinLock:       TCriticalSection;
    FJoinMode:       TJoinMode;
    FTool:           TAiToolBase;
    FJoinInputs:     TAIAgentsJoinInputs;
    FSuspended:      Boolean;
    FSuspendReason:  string;
    FSuspendContext: string;
    procedure SetInput(const Value: string);
    procedure SetNext(const Value: TAIAgentsLink);
    procedure SetOutput(const Value: string);
    procedure SetGraph(const Value: TAIAgentManager);
    procedure SetOnExecute(const Value: TAIAgentsNodeOnExecute);
    procedure SetPromptName(const Value: string);
    procedure SetJoinMode(const Value: TJoinMode);
    procedure SetTool(const Value: TAiToolBase);
    procedure SetError(const Value: Boolean);
    procedure SetMsgError(const Value: string);
  protected
    procedure DoExecute(ABeforeNode: TAIAgentsNode; ALink: TAIAgentsLink); virtual;
    procedure Reset;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Print(Value: string);
    procedure   ForceFinalExecute;
    function    RequestConfirmation(const AQuestion: string; Buttons: TMsgStates;
                                    var AResponse: string): TMsgState;
    function    RequestInput(const ACaption, APrompt: string;
                             var AValue: string): Boolean;
    procedure   Suspend(const AReason: string; const AContext: string = '');
    property    Error:          Boolean read FError         write SetError;
    property    MsgError:       string  read FMsgError      write SetMsgError;
    property    Suspended:      Boolean read FSuspended;
    property    SuspendReason:  string  read FSuspendReason;
    property    SuspendContext: string  read FSuspendContext;
  published
    property Input:      string              read FInput      write SetInput;
    property Output:     string              read FOutput     write SetOutput;
    property Next:       TAIAgentsLink       read FNext       write SetNext;
    property Graph:      TAIAgentManager     read FGraph      write SetGraph;
    property OnExecute:  TAIAgentsNodeOnExecute read FOnExecute write SetOnExecute;
    property PromptName: string              read FPromptName write SetPromptName;
    property JoinMode:   TJoinMode           read FJoinMode   write SetJoinMode default jmAny;
    property Tool:       TAiToolBase         read FTool       write SetTool;
  end;

  // -------------------------------------------------------------------------
  // TAiNodeTask — hilo para ejecucion paralela de nodos
  // -------------------------------------------------------------------------
  TAiNodeTask = class(TThread)
  private
    FManager:    TAIAgentManager;
    FNode:       TAIAgentsNode;
    FSourceNode: TAIAgentsNode;
    FLink:       TAIAgentsLink;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TAIAgentManager;
                       ANode, ASource: TAIAgentsNode;
                       ALink: TAIAgentsLink);
  end;

  // -------------------------------------------------------------------------
  // TAiOrchestratorThread — hilo principal de orquestacion del grafo
  // -------------------------------------------------------------------------
  TAiOrchestratorThread = class(TThread)
  private
    FManager:      TAIAgentManager;
    FInitialInput: string;
    FStartNode:    TAIAgentsNode; // StartNode o nodo de reanudacion
    FIsResume:     Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TAIAgentManager;
                       const AInput: string;
                       AStartNode: TAIAgentsNode;
                       AIsResume: Boolean = False);
  end;

  // -------------------------------------------------------------------------
  // TAIAgentManager — orquestador principal
  // -------------------------------------------------------------------------
  TAIAgentsNodeObjList = specialize TObjectList<TAIAgentsNode>; // helper alias
  TAIAgentNodeList     = specialize TList<TAIAgentsNode>;
  TAIAgentLinkList     = specialize TList<TAIAgentsLink>;

  TAIAgentManager = class(TComponent)
  private
    FEndNode:            TAIAgentsNode;
    FStartNode:          TAIAgentsNode;
    FOnPrint:            TAIAgentsOnPrint;
    FNodes:              TAIAgentNodeList;
    FLinks:              TAIAgentLinkList;
    FOnEnd:              TAIAgentsOnEnd;
    FOnError:            TAIAgentsOnError;
    FOnConfirm:          TAIAgentsOnConfirm;
    FBusy:               Boolean;
    FBusyLock:           TCriticalSection;
    FAbort:              Boolean;
    FBlackboard:         TAIBlackboard;
    FCompiled:           Boolean;
    FOnExitNode:         TAIAgentsOnExitNode;
    FOnEnterNode:        TAIAgentsOnEnterNode;
    FMaxConcurrentTasks: Integer;
    FTimeoutMs:          Cardinal;
    FOnFinish:           TAIAgentsOnFinish;
    FOnStart:            TAIAgentsOnStart;
    FDescription:        string;
    FAsynchronous:       Boolean;
    // Contador atomico de tasks paralelas en vuelo
    FActiveTaskCount:    Integer;
    // Checkpoint / suspend-resume
    FCheckpointer:       IAiCheckpointer;
    FCurrentThreadID:    string;
    FCheckpointSeq:      Integer;
    FSuspendedSteps:     TAiPendingStepList;
    FSuspendedStepsLock: TCriticalSection;
    FOnSuspend:          TAIAgentsOnSuspend;
    // Hilo orquestador (async mode)
    FOrchestratorThread: TAiOrchestratorThread;

    function  TrySetBusy: Boolean;
    procedure ClearBusy;
    procedure SetCheckpointer(const Value: IAiCheckpointer);
    procedure SetOnSuspend(const Value: TAIAgentsOnSuspend);
    procedure SetMaxConcurrentTasks(const Value: Integer);
    procedure SetEndNode(const Value: TAIAgentsNode);
    procedure SetStartNode(const Value: TAIAgentsNode);
    procedure SetOnPrint(const Value: TAIAgentsOnPrint);
    procedure SetOnEnd(const Value: TAIAgentsOnEnd);
    procedure SetOnError(const Value: TAIAgentsOnError);
    procedure SetOnConfirm(const Value: TAIAgentsOnConfirm);
    procedure SetOnEnterNode(const Value: TAIAgentsOnEnterNode);
    procedure SetOnExitNode(const Value: TAIAgentsOnExitNode);
    procedure SetOnFinish(const Value: TAIAgentsOnFinish);
    procedure SetOnStart(const Value: TAIAgentsOnStart);
    procedure SetDescription(const Value: string);
    procedure SetAsynchronous(const Value: Boolean);
  protected
    procedure DoPrint(Sender: TObject; Value: string);
    procedure DoNodeCompleted(ANode: TAIAgentsNode);
    procedure DoNodeSuspended(ANode: TAIAgentsNode;
                              ABeforeNode: TAIAgentsNode;
                              ALink: TAIAgentsLink);
    function  BuildSnapshot: TAiCheckpointSnapshot;
    procedure RestoreFromSnapshot(ASnapshot: TAiCheckpointSnapshot);
    function  FindLink(const AName: string): TAIAgentsLink;
    procedure AddComponentToList(AComponent: TAIAgentsBase);
    procedure RemoveComponentFromList(AComponent: TAIAgentsBase);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Cuerpo del orquestador (usado desde TAiOrchestratorThread)
    procedure RunOrchestratorBody(const AInitialInput: string;
                                   AStartNode: TAIAgentsNode;
                                   AIsResume: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Abort;
    function    FindNode(const AName: string): TAIAgentsNode;
    procedure   DoError(Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception);
    function    DoConfirm(Node: TAIAgentsNode; const AQuestion: string;
                          Buttons: TMsgStates; var AResponse: string): TMsgState;
    procedure   ClearGraph;
    function    AddNode(const AName: string;
                        AExecuteProc: TAIAgentsNodeOnExecute): TAIAgentManager;
    function    AddEdge(const AStartNodeName, AEndNodeName: string): TAIAgentManager;
    function    AddConditionalEdge(const AStartNodeName: string;
                                   const AConditionalLinkName: string;
                                   AConditionalTargets: specialize TDictionary<string, string>): TAIAgentManager;
    function    SetEntryPoint(const ANodeName: string): TAIAgentManager;
    function    SetFinishPoint(const ANodeName: string): TAIAgentManager;
    procedure   Compile;
    procedure   SaveToStream(AStream: TStream);
    procedure   LoadFromStream(AStream: TStream);
    procedure   SaveStateToStream(AStream: TStream);
    procedure   LoadStateFromStream(AStream: TStream);
    function    Run(APrompt: string): string;
    function    AddMessageAndRun(APrompt, ARole: string;
                                 AMediaFiles: TAiMediaFilesArray): string;
    function    AddMessageAndRunMsg(APrompt, ARole: string;
                                    AMediaFiles: TAiMediaFilesArray): TAiChatMessage;
    function    NewMessage(APrompt, ARole: string;
                           AMediaFiles: TAiMediaFilesArray): TAiChatMessage;
    function    ResumeThread(const AThreadID, ANodeName, AInput: string): Boolean;
    function    GetActiveThreads: TStringDynArray;
    property    Busy:            Boolean          read FBusy;
    property    Blackboard:      TAIBlackboard    read FBlackboard;
    property    CurrentThreadID: string           read FCurrentThreadID;
    property    Checkpointer:    IAiCheckpointer  read FCheckpointer write SetCheckpointer;
    // Acceso interno para TAiNodeTask y TAiOrchestratorThread
    property    ActiveTaskCount: Integer          read FActiveTaskCount write FActiveTaskCount;
    property    IsAborted:       Boolean          read FAbort;
  published
    property StartNode:          TAIAgentsNode      read FStartNode          write SetStartNode;
    property EndNode:            TAIAgentsNode      read FEndNode            write SetEndNode;
    property OnPrint:            TAIAgentsOnPrint   read FOnPrint            write SetOnPrint;
    property OnEnd:              TAIAgentsOnEnd     read FOnEnd              write SetOnEnd;
    property OnError:            TAIAgentsOnError   read FOnError            write SetOnError;
    property OnConfirm:          TAIAgentsOnConfirm read FOnConfirm          write SetOnConfirm;
    property OnEnterNode:        TAIAgentsOnEnterNode read FOnEnterNode      write SetOnEnterNode;
    property OnExitNode:         TAIAgentsOnExitNode  read FOnExitNode       write SetOnExitNode;
    property MaxConcurrentTasks: Integer             read FMaxConcurrentTasks write SetMaxConcurrentTasks default 4;
    property OnStart:            TAIAgentsOnStart    read FOnStart            write SetOnStart;
    property OnFinish:           TAIAgentsOnFinish   read FOnFinish           write SetOnFinish;
    property TimeoutMs:          Cardinal            read FTimeoutMs          write FTimeoutMs default 60000;
    property Description:        string              read FDescription        write SetDescription;
    property Asynchronous:       Boolean             read FAsynchronous       write SetAsynchronous default True;
    property OnSuspend:          TAIAgentsOnSuspend  read FOnSuspend          write SetOnSuspend;
  end;

  // Alias de compatibilidad
  TAIAgents = class(TAIAgentManager)
  end;

procedure Register;

implementation

uses
  uMakerAi.Agents.EngineRegistry;

// ---------------------------------------------------------------------------
// Helpers de serializacion de Blackboard
// ---------------------------------------------------------------------------

procedure SerializeBlackboard(ABB: TAIBlackboard; AObj: TJSONObject);
var
  LData: specialize TDictionary<string, Variant>;
  LEnum: specialize TDictionary<string, Variant>.TPairEnumerator;
  VT:    TVarType;
  V:     Variant;
  LKey:  string;
  I:     Integer;
begin
  LData := ABB.GetDataForSerialization;
  ABB.Lock.Enter;
  try
    LEnum := LData.GetEnumerator;
    try
      while LEnum.MoveNext do
      begin
        LKey := LEnum.Current.Key;
        V    := LEnum.Current.Value;
        VT   := VarType(V) and varTypeMask;
        case VT of
          varByte, varWord, varLongWord, varInteger, varInt64,
          varShortInt, varSmallint:
            AObj.Add(LKey, Integer(V));
          varSingle, varDouble, varCurrency:
            AObj.Add(LKey, Double(V));
          varString, varUString, varOleStr:
            AObj.Add(LKey, string(V));
          varBoolean:
            AObj.Add(LKey, Boolean(V));
        end;
      end;
    finally
      LEnum.Free;
    end;
  finally
    ABB.Lock.Leave;
  end;
end;

procedure DeserializeBlackboard(AObj: TJSONObject; ABB: TAIBlackboard);
var
  I:  Integer;
  LV: TJSONData;
begin
  ABB.Clear;
  for I := 0 to AObj.Count - 1 do
  begin
    LV := AObj.Items[I];
    if LV is TJSONString then
      ABB.SetString(AObj.Names[I], TJSONString(LV).AsString)
    else if LV is TJSONIntegerNumber then
      ABB.SetInteger(AObj.Names[I], TJSONIntegerNumber(LV).AsInteger)
    else if LV is TJSONBoolean then
      ABB.SetBoolean(AObj.Names[I], TJSONBoolean(LV).AsBoolean)
    else if LV is TJSONNumber then
      ABB.SetValue(AObj.Names[I], TJSONNumber(LV).AsFloat);
  end;
end;

procedure SerializeToolProperties(ATool: TAiToolBase; AObj: TJSONObject);
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Count, I: Integer;
begin
  if not Assigned(ATool) then Exit;
  Count := GetPropList(ATool.ClassInfo, tkProperties, nil);
  if Count <= 0 then Exit;
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropList(ATool.ClassInfo, tkProperties, PropList);
    for I := 0 to Count - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo^.PropProcs and 3 = 0 then Continue; // no readable
      case PropInfo^.PropType^.Kind of
        tkAString, tkSString, tkLString:
          AObj.Add(PropInfo^.Name, GetStrProp(ATool, PropInfo));
        tkInteger, tkBool:
          AObj.Add(PropInfo^.Name, GetOrdProp(ATool, PropInfo));
        tkFloat:
          AObj.Add(PropInfo^.Name, GetFloatProp(ATool, PropInfo));
        tkEnumeration:
          AObj.Add(PropInfo^.Name,
            GetEnumName(PropInfo^.PropType, GetOrdProp(ATool, PropInfo)));
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

procedure DeserializeToolProperties(ATool: TAiToolBase; APropsJSON: TJSONObject);
var
  I:        Integer;
  PropInfo: PPropInfo;
  LVal:     TJSONData;
  OrdV:     Integer;
begin
  if not Assigned(ATool) or not Assigned(APropsJSON) then Exit;
  for I := 0 to APropsJSON.Count - 1 do
  begin
    PropInfo := GetPropInfo(ATool.ClassInfo, APropsJSON.Names[I]);
    if not Assigned(PropInfo) then Continue;
    LVal := APropsJSON.Items[I];
    case PropInfo^.PropType^.Kind of
      tkAString, tkSString, tkLString:
        SetStrProp(ATool, PropInfo, LVal.AsString);
      tkInteger:
        SetOrdProp(ATool, PropInfo, LVal.AsInteger);
      tkBool:
        SetOrdProp(ATool, PropInfo, Ord(LVal.AsBoolean));
      tkFloat:
        SetFloatProp(ATool, PropInfo, LVal.AsFloat);
      tkEnumeration:
        begin
          OrdV := GetEnumValue(PropInfo^.PropType, LVal.AsString);
          if OrdV >= 0 then
            SetOrdProp(ATool, PropInfo, OrdV);
        end;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// TAIBlackboard
// ---------------------------------------------------------------------------

constructor TAIBlackboard.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FData := specialize TDictionary<string, Variant>.Create;
end;

destructor TAIBlackboard.Destroy;
begin
  Clear;
  FData.Free;
  FLock.Free;
  inherited;
end;

procedure TAIBlackboard.Clear;
begin
  FLock.Enter;
  try
    FreeAndNil(FAskMsg);
    FreeAndNil(FResMsg);
    FData.Clear;
  finally
    FLock.Leave;
  end;
end;

function TAIBlackboard.GetDataForSerialization: specialize TDictionary<string, Variant>;
begin
  Result := FData;
end;

function TAIBlackboard.GetAskMsg: TAiChatMessage;
begin
  FLock.Enter;
  try
    Result := FAskMsg;
  finally
    FLock.Leave;
  end;
end;

function TAIBlackboard.GetResMsg: TAiChatMessage;
begin
  FLock.Enter;
  try
    Result := FResMsg;
  finally
    FLock.Leave;
  end;
end;

procedure TAIBlackboard.SetAskMsg(const Value: TAiChatMessage);
begin
  FLock.Enter;
  try
    if FAskMsg <> Value then
      FreeAndNil(FAskMsg);
    FAskMsg := Value;
  finally
    FLock.Leave;
  end;
end;

procedure TAIBlackboard.SetResMsg(const Value: TAiChatMessage);
begin
  FLock.Enter;
  try
    if FResMsg <> Value then
      FreeAndNil(FResMsg);
    FResMsg := Value;
  finally
    FLock.Leave;
  end;
end;

procedure TAIBlackboard.SetValue(const AKey: string; const AValue: Variant);
begin
  FLock.Enter;
  try
    FData.AddOrSetValue(AKey, AValue);
  finally
    FLock.Leave;
  end;
end;

function TAIBlackboard.TryGetValue(const AKey: string; out AValue: Variant): Boolean;
begin
  FLock.Enter;
  try
    Result := FData.TryGetValue(AKey, AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TAIBlackboard.SetString(const AKey, AValue: string);
begin
  SetValue(AKey, AValue);
end;

function TAIBlackboard.GetString(const AKey: string; const ADefault: string): string;
var
  V: Variant;
begin
  if TryGetValue(AKey, V) and not VarIsNull(V) and not VarIsEmpty(V) then
    Result := VarToStr(V)
  else
    Result := ADefault;
end;

procedure TAIBlackboard.SetInteger(const AKey: string; AValue: Integer);
begin
  SetValue(AKey, AValue);
end;

function TAIBlackboard.GetInteger(const AKey: string; const ADefault: Integer): Integer;
var
  V: Variant;
begin
  if TryGetValue(AKey, V) and not VarIsNull(V) and not VarIsEmpty(V) then
    Result := Integer(V)
  else
    Result := ADefault;
end;

procedure TAIBlackboard.SetBoolean(const AKey: string; AValue: Boolean);
begin
  SetValue(AKey, AValue);
end;

function TAIBlackboard.GetBoolean(const AKey: string; const ADefault: Boolean): Boolean;
var
  V: Variant;
begin
  if TryGetValue(AKey, V) and not VarIsNull(V) and not VarIsEmpty(V) then
    Result := Boolean(V)
  else
    Result := ADefault;
end;

procedure TAIBlackboard.SetStatus(Value: TAgentExecutionStatus);
begin
  SetInteger('Execution.Status', Ord(Value));
end;

function TAIBlackboard.GetStatus: TAgentExecutionStatus;
var
  I: Integer;
begin
  I := GetInteger('Execution.Status', Ord(esUnknown));
  if (I >= Ord(Low(TAgentExecutionStatus))) and
     (I <= Ord(High(TAgentExecutionStatus))) then
    Result := TAgentExecutionStatus(I)
  else
    Result := esUnknown;
end;

// ---------------------------------------------------------------------------
// TAiToolBase
// ---------------------------------------------------------------------------

procedure TAiToolBase.Run(ANode: TAIAgentsNode; const AInput: string;
  var AOutput: string);
begin
  Execute(ANode, AInput, AOutput);
end;

procedure TAiToolBase.SetDescription(const Value: string);
begin FDescription := Value; end;

procedure TAiToolBase.SetID(const Value: string);
begin FID := Value; end;

{ TAiAgentsToolSample }
procedure TAiAgentsToolSample.Execute(ANode: TAIAgentsNode; const AInput: string;
  var AOutput: string);
begin
  if Assigned(ANode) then
    ANode.Print(Format('TAiSampleTool: input="%s"', [AInput]));
  AOutput := 'SampleTool: ' + AInput;
end;

// ---------------------------------------------------------------------------
// TAIAgentsBase
// ---------------------------------------------------------------------------

procedure TAIAgentsBase.SetDescription(const Value: string);
begin FDescription := Value; end;

procedure TAIAgentsBase.SetID(const Value: string);
begin FID := Value; end;

// ---------------------------------------------------------------------------
// TAiNodeTask
// ---------------------------------------------------------------------------

constructor TAiNodeTask.Create(AManager: TAIAgentManager;
  ANode, ASource: TAIAgentsNode; ALink: TAIAgentsLink);
begin
  inherited Create(True); // suspended
  FManager    := AManager;
  FNode       := ANode;
  FSourceNode := ASource;
  FLink       := ALink;
  FreeOnTerminate := True;
end;

procedure TAiNodeTask.Execute;
begin
  try
    if not FManager.IsAborted then
      FNode.DoExecute(FSourceNode, FLink);
  finally
    InterlockedDecrement(FManager.FActiveTaskCount);
  end;
end;

// ---------------------------------------------------------------------------
// TAiOrchestratorThread
// ---------------------------------------------------------------------------

constructor TAiOrchestratorThread.Create(AManager: TAIAgentManager;
  const AInput: string; AStartNode: TAIAgentsNode; AIsResume: Boolean);
begin
  inherited Create(True); // suspended
  FManager      := AManager;
  FInitialInput := AInput;
  FStartNode    := AStartNode;
  FIsResume     := AIsResume;
  FreeOnTerminate := False; // managed by TAIAgentManager
end;

procedure TAiOrchestratorThread.Execute;
begin
  FManager.RunOrchestratorBody(FInitialInput, FStartNode, FIsResume);
end;

// ---------------------------------------------------------------------------
// TAIAgentsLink
// ---------------------------------------------------------------------------

constructor TAIAgentsLink.Create(AOwner: TComponent);
begin
  inherited;
  FMaxCycles        := 1;
  FMode             := lmFanout;
  FConditionalKey   := 'next_route';
  FManualTargetsKey := 'next_targets';
end;

destructor TAIAgentsLink.Destroy;
begin
  FConditionalTargets.Free;
  if Assigned(FGraph) then
    FGraph.RemoveComponentFromList(Self);
  inherited;
end;

procedure TAIAgentsLink.AddConditionalTarget(const AKey: string;
  ANode: TAIAgentsNode);
begin
  if not Assigned(FConditionalTargets) then
    FConditionalTargets := TAIAgentsLinkNodeMap.Create;
  FConditionalTargets.AddOrSetValue(AKey, ANode);
end;

procedure TAIAgentsLink.BuildManualTargets(const TargetsCSV: string;
  out Nodes: TAIAgentsNodeList);

  function ResolveToken(const S: string): TAIAgentsNode;
  begin
    Result := nil;
    if SameText(S, 'A') then Exit(FNextA);
    if SameText(S, 'B') then Exit(FNextB);
    if SameText(S, 'C') then Exit(FNextC);
    if SameText(S, 'D') then Exit(FNextD);
    if Assigned(FGraph) then
      Result := FGraph.FindNode(S);
  end;

var
  Parts:  TStringList;
  I:      Integer;
  LNode:  TAIAgentsNode;
begin
  Nodes := TAIAgentsNodeList.Create;
  if Trim(TargetsCSV) = '' then Exit;
  Parts := TStringList.Create;
  try
    Parts.Delimiter       := ',';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText   := TargetsCSV;
    for I := 0 to Parts.Count - 1 do
    begin
      LNode := ResolveToken(Trim(Parts[I]));
      if Assigned(LNode) then
        Nodes.Add(LNode);
    end;
  finally
    Parts.Free;
  end;
end;

procedure TAIAgentsLink.CreateAndQueueTask(ANodeToExecute, ASourceNode: TAIAgentsNode;
  ACurrentLink: TAIAgentsLink);
var
  LTask: TAiNodeTask;
begin
  InterlockedIncrement(FGraph.FActiveTaskCount);
  LTask := TAiNodeTask.Create(FGraph, ANodeToExecute, ASourceNode, ACurrentLink);
  LTask.Start;
end;

procedure TAIAgentsLink.DoExecute(Sender: TAIAgentsNode);
var
  IsOk, Handled: Boolean;
  NodesToRun:    TAIAgentsNodeList;
  ManualNodes:   TAIAgentsNodeList;
  Decision:      string;
  TargetNode:    TAIAgentsNode;
  LNode:         TAIAgentsNode;
  I:             Integer;
begin
  if (FGraph = nil) or FGraph.IsAborted then
    Exit;

  // PASO 1: evento OnExecute para intervencion manual
  Handled := False;
  IsOk    := not Sender.FError;
  if Assigned(FOnExecute) then
  try
    FOnExecute(Sender, Self, IsOk, Handled);
  except
    on E: Exception do
    begin
      FGraph.DoError(Sender, Self, E);
      Exit;
    end;
  end;

  if Handled then Exit;

  // PASO 2: manejar errores y reintentos
  if not IsOk then
  begin
    Inc(FNoCycles);
    if FNoCycles >= FMaxCycles then
    begin
      FGraph.DoError(Sender, Self,
        Exception.CreateFmt('Max retry cycles (%d) reached on link "%s" from "%s".',
          [FMaxCycles, Name, Sender.Name]));
      Exit;
    end
    else
    begin
      if Assigned(FNextNo) then
        CreateAndQueueTask(FNextNo, FSourceNode, Self);
      Exit;
    end;
  end;

  // PASO 3: construir lista de nodos destino
  NodesToRun := TAIAgentsNodeList.Create;
  try
    case FMode of
      lmFanout:
        begin
          if Assigned(FNextA) then NodesToRun.Add(FNextA);
          if Assigned(FNextB) then NodesToRun.Add(FNextB);
          if Assigned(FNextC) then NodesToRun.Add(FNextC);
          if Assigned(FNextD) then NodesToRun.Add(FNextD);
        end;

      lmConditional:
        begin
          if FConditionalKey <> '' then
            Decision := FGraph.Blackboard.GetString(FConditionalKey)
          else
            Decision := FGraph.Blackboard.GetString('next_route');
          if Assigned(FConditionalTargets) and
             FConditionalTargets.TryGetValue(Decision, TargetNode) and
             Assigned(TargetNode) then
            NodesToRun.Add(TargetNode)
          else if Assigned(FNextNo) then
            NodesToRun.Add(FNextNo);
        end;

      lmManual:
        begin
          if FManualTargetsKey <> '' then
            Decision := FGraph.Blackboard.GetString(FManualTargetsKey)
          else
            Decision := FGraph.Blackboard.GetString('next_targets');
          ManualNodes := nil;
          BuildManualTargets(Decision, ManualNodes);
          try
            for I := 0 to ManualNodes.Count - 1 do
              NodesToRun.Add(ManualNodes[I]);
          finally
            ManualNodes.Free;
          end;
        end;

      lmExpression:
        raise Exception.Create(
          '[FPC] lmExpression mode not implemented: requires System.Bindings (Delphi only). ' +
          'Use lmConditional or lmManual instead.');
    end;

  except
    on E: Exception do
    begin
      NodesToRun.Free;
      FGraph.DoError(Sender, Self, E);
      Exit;
    end;
  end;

  // PASO 4: despachar ejecucion
  try
    if NodesToRun.Count = 0 then
      Exit;

    if NodesToRun.Count = 1 then
      NodesToRun[0].DoExecute(FSourceNode, Self)
    else
    begin
      for I := 0 to NodesToRun.Count - 1 do
        CreateAndQueueTask(NodesToRun[I], FSourceNode, Self);
    end;
  finally
    NodesToRun.Free;
  end;
end;

procedure TAIAgentsLink.Print(Value: string);
begin
  if Assigned(FGraph) then
    FGraph.DoPrint(Self, Value);
end;

procedure TAIAgentsLink.SetGraph(const Value: TAIAgentManager);
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

procedure TAIAgentsLink.SetMaxCycles(const Value: Integer);
begin
  FMaxCycles := Value;
  if FMaxCycles < 1 then FMaxCycles := 1;
end;

procedure TAIAgentsLink.SetMode(const Value: TLinkMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    if Assigned(FGraph) then FGraph.FCompiled := False;
  end;
end;

procedure TAIAgentsLink.SetNextA(const Value: TAIAgentsNode); begin FNextA := Value; end;
procedure TAIAgentsLink.SetNextB(const Value: TAIAgentsNode); begin FNextB := Value; end;
procedure TAIAgentsLink.SetNextC(const Value: TAIAgentsNode); begin FNextC := Value; end;
procedure TAIAgentsLink.SetNextD(const Value: TAIAgentsNode); begin FNextD := Value; end;
procedure TAIAgentsLink.SetNextNo(const Value: TAIAgentsNode); begin FNextNo := Value; end;
procedure TAIAgentsLink.SetOnExecute(const Value: TAIAgentsLinkOnExecute);
begin FOnExecute := Value; end;

// ---------------------------------------------------------------------------
// TAIAgentsNode
// ---------------------------------------------------------------------------

constructor TAIAgentsNode.Create(AOwner: TComponent);
begin
  inherited;
  FInEdges   := TAIAgentsLinkList.Create;
  FJoinLock  := TCriticalSection.Create;
  FJoinInputs := TAIAgentsJoinInputs.Create;
  FJoinMode   := jmAny;
end;

destructor TAIAgentsNode.Destroy;
begin
  FInEdges.Free;
  FJoinLock.Free;
  FJoinInputs.Free;
  inherited;
end;

procedure TAIAgentsNode.Reset;
begin
  FInEdges.Clear;
  FJoinInputs.Clear;
  FInput          := '';
  FOutput         := '';
  FError          := False;
  FMsgError       := '';
  FSuspended      := False;
  FSuspendReason  := '';
  FSuspendContext := '';
end;

procedure TAIAgentsNode.Suspend(const AReason: string; const AContext: string);
begin
  FSuspended      := True;
  FSuspendReason  := AReason;
  FSuspendContext := AContext;
end;

procedure TAIAgentsNode.DoExecute(ABeforeNode: TAIAgentsNode; ALink: TAIAgentsLink);
var
  CanExecute:  Boolean;
  CombinedIn:  string;
  LEnum:       TAIAgentsJoinInputs.TValueEnumerator;
  V:           string;
begin
  if (FGraph = nil) or FGraph.IsAborted then
    Exit;

  if Assigned(FGraph.OnEnterNode) then
    FGraph.OnEnterNode(FGraph, Self);

  CanExecute := False;

  // Reanudacion desde ResumeThread (aBeforeNode=nil, aLink=nil, nodo join)
  if (ABeforeNode = nil) and (ALink = nil) and (FInEdges.Count > 1) then
  begin
    CanExecute := True;
  end
  else if FInEdges.Count > 1 then
  begin
    FJoinLock.Enter;
    try
      if (ABeforeNode <> nil) and (ALink <> nil) then
        FJoinInputs.AddOrSetValue(ALink, ABeforeNode.Output);

      case FJoinMode of
        jmAny:
          begin
            CanExecute := True;
            if ABeforeNode <> nil then
              FInput := ABeforeNode.Output;
          end;
        jmAll:
          begin
            if FJoinInputs.Count >= FInEdges.Count then
            begin
              CanExecute  := True;
              CombinedIn  := '';
              LEnum := FJoinInputs.GetValueEnumerator;
              try
                while LEnum.MoveNext do
                begin
                  V := LEnum.Current;
                  if CombinedIn <> '' then CombinedIn := CombinedIn + #10;
                  CombinedIn := CombinedIn + V;
                end;
              finally
                LEnum.Free;
              end;
              FInput := CombinedIn;
            end;
          end;
      end;
    finally
      FJoinLock.Leave;
    end;
  end
  else
  begin
    CanExecute := True;
    if ABeforeNode <> nil then
      FInput := ABeforeNode.Output;
  end;

  if not CanExecute then Exit;

  try
    FSuspended      := False;
    FSuspendReason  := '';
    FSuspendContext := '';

    if Assigned(FOnExecute) then
      FOnExecute(Self, ABeforeNode, ALink, FInput, FOutput)
    else if Assigned(FTool) then
      FTool.Run(Self, FInput, FOutput);

    if FOutput = '' then
      FOutput := FInput;

    // Verificar suspension
    if FSuspended then
    begin
      if Assigned(FGraph) then
        FGraph.DoNodeSuspended(Self, ABeforeNode, ALink);
      Exit;
    end;

    if Assigned(FGraph.OnExitNode) then
      FGraph.OnExitNode(FGraph, Self);

    if Assigned(FNext) then
    begin
      FNext.FSourceNode := Self;
      FNext.DoExecute(Self);
    end;

    if Assigned(FGraph) then
      FGraph.DoNodeCompleted(Self);
  finally
    if CanExecute and (FInEdges.Count > 1) then
    begin
      FJoinLock.Enter;
      try
        if FJoinMode = jmAny then
          FJoinInputs.Clear;
        // jmAll: no limpiar (CombineLatest pattern)
      finally
        FJoinLock.Leave;
      end;
    end;
  end;
end;

procedure TAIAgentsNode.ForceFinalExecute;
begin
  try
    if Assigned(FOnExecute) then
      FOnExecute(Self, nil, nil, FInput, FOutput);
    if Assigned(FGraph) and Assigned(FGraph.OnEnd) then
      FGraph.OnEnd(Self, FOutput);
  except
    on E: Exception do
      FGraph.DoError(Self, nil, E);
  end;
end;

procedure TAIAgentsNode.Print(Value: string);
begin
  if Assigned(FGraph) then
    FGraph.DoPrint(Self, Value);
end;

function TAIAgentsNode.RequestConfirmation(const AQuestion: string;
  Buttons: TMsgStates; var AResponse: string): TMsgState;
begin
  if Assigned(FGraph) then
    Result := FGraph.DoConfirm(Self, AQuestion, Buttons, AResponse)
  else
    Result := msAbort;
end;

function TAIAgentsNode.RequestInput(const ACaption, APrompt: string;
  var AValue: string): Boolean;
var
  Response:    string;
  ModalResult: TMsgState;
begin
  Response    := AValue;
  ModalResult := RequestConfirmation(ACaption + '|' + APrompt, [msOK, msCancel], Response);
  if ModalResult = msOK then
  begin
    AValue := Response;
    Result := True;
  end
  else
    Result := False;
end;

procedure TAIAgentsNode.SetError(const Value: Boolean);   begin FError    := Value; end;
procedure TAIAgentsNode.SetGraph(const Value: TAIAgentManager);
begin
  if Value <> FGraph then
  begin
    if Assigned(FGraph) then FGraph.RemoveComponentFromList(Self);
    FGraph := Value;
    if Assigned(FGraph) then
    begin
      FGraph.AddComponentToList(Self);
      FGraph.FCompiled := False;
    end;
  end;
end;
procedure TAIAgentsNode.SetInput(const Value: string);    begin FInput    := Value; end;
procedure TAIAgentsNode.SetJoinMode(const Value: TJoinMode); begin FJoinMode := Value; end;
procedure TAIAgentsNode.SetMsgError(const Value: string); begin FMsgError  := Value; end;
procedure TAIAgentsNode.SetNext(const Value: TAIAgentsLink);
begin
  if Value <> FNext then
  begin
    if Assigned(FNext) then FNext.FSourceNode := nil;
    FNext := Value;
    if Assigned(FNext) then FNext.FSourceNode := Self;
    if Assigned(FGraph) then FGraph.FCompiled := False;
  end;
end;
procedure TAIAgentsNode.SetOnExecute(const Value: TAIAgentsNodeOnExecute);
begin FOnExecute := Value; end;
procedure TAIAgentsNode.SetOutput(const Value: string);   begin FOutput   := Value; end;
procedure TAIAgentsNode.SetPromptName(const Value: string); begin FPromptName := Value; end;
procedure TAIAgentsNode.SetTool(const Value: TAiToolBase); begin FTool := Value; end;

// ---------------------------------------------------------------------------
// TAIAgentManager — helpers internos
// ---------------------------------------------------------------------------

function TAIAgentManager.TrySetBusy: Boolean;
begin
  FBusyLock.Enter;
  try
    Result := FBusy;
    if not FBusy then FBusy := True;
  finally
    FBusyLock.Leave;
  end;
end;

procedure TAIAgentManager.ClearBusy;
begin
  FBusyLock.Enter;
  try
    FBusy := False;
  finally
    FBusyLock.Leave;
  end;
end;

// ---------------------------------------------------------------------------
// TAIAgentManager — RunOrchestratorBody (nucleo de ejecucion)
// ---------------------------------------------------------------------------

procedure TAIAgentManager.RunOrchestratorBody(const AInitialInput: string;
  AStartNode: TAIAgentsNode; AIsResume: Boolean);
var
  FinalStatus:    TAgentExecutionStatus;
  FinalException: Exception;
  FinalOutput:    string;
  StartTime:      QWord;
  HasActive:      Boolean;
begin
  FinalStatus    := esUnknown;
  FinalException := nil;
  try
    try
      if Assigned(FOnStart) then
        FOnStart(Self, AInitialInput);

      if FAbort then
      begin
        FinalStatus := esAborted;
        Exit;
      end;

      // Ejecutar nodo inicial
      if Assigned(AStartNode) then
      begin
        AStartNode.Input := AInitialInput;
        AStartNode.DoExecute(nil, nil);
      end;

      // Esperar a que todas las tasks paralelas terminen
      StartTime := GetTickCount64;
      repeat
        if FAbort then Break;

        HasActive := FActiveTaskCount > 0;
        if not HasActive then Break;

        if (FTimeoutMs > 0) and
           (GetTickCount64 - StartTime > QWord(FTimeoutMs)) then
          raise Exception.CreateFmt(
            'Graph execution timed out after %d ms.', [FTimeoutMs]);

        Sleep(10);
      until False;

      // Determinar estado final
      if not FAbort then
      begin
        FSuspendedStepsLock.Enter;
        try
          if FSuspendedSteps.Count > 0 then
            FinalStatus := esSuspended
          else
            FinalStatus := esCompleted;
        finally
          FSuspendedStepsLock.Leave;
        end;
      end
      else
        FinalStatus := esAborted;

    except
      on E: Exception do
      begin
        Abort;
        FinalException := E;
        if Pos('timed out', E.Message) > 0 then
          FinalStatus := esTimeout
        else
          FinalStatus := esError;
        DoError(nil, nil, E);
      end;
    end;
  finally
    if FinalStatus = esUnknown then
      FinalStatus := esAborted;

    Blackboard.SetStatus(FinalStatus);

    if (FinalStatus = esCompleted) and Assigned(FCheckpointer) then
      FCheckpointer.DeleteCheckpoint(FCurrentThreadID);

    FinalOutput := '';
    if Assigned(FEndNode) then
      FinalOutput := FEndNode.Output;

    if Assigned(FOnFinish) then
      FOnFinish(Self, AInitialInput, FinalOutput, FinalStatus, FinalException);

    ClearBusy;
  end;
end;

// ---------------------------------------------------------------------------
// TAIAgentManager
// ---------------------------------------------------------------------------

constructor TAIAgentManager.Create(AOwner: TComponent);
begin
  inherited;
  FBlackboard         := TAIBlackboard.Create;
  FNodes              := TAIAgentNodeList.Create;
  FLinks              := TAIAgentLinkList.Create;
  FBusyLock           := TCriticalSection.Create;
  FCompiled           := False;
  FAsynchronous       := True;
  FBusy               := False;
  FAbort              := False;
  FActiveTaskCount    := 0;
  FMaxConcurrentTasks := 4;
  FTimeoutMs          := 60000;
  FSuspendedSteps     := TAiPendingStepList.Create(True);
  FSuspendedStepsLock := TCriticalSection.Create;
  FCheckpointer       := nil;
  FCurrentThreadID    := '';
  FCheckpointSeq      := 0;
  FOrchestratorThread := nil;
end;

destructor TAIAgentManager.Destroy;
begin
  // Esperar al orquestador si sigue corriendo
  if Assigned(FOrchestratorThread) then
  begin
    FAbort := True;
    FOrchestratorThread.WaitFor;
    FreeAndNil(FOrchestratorThread);
  end;
  FSuspendedSteps.Free;
  FSuspendedStepsLock.Free;
  FNodes.Free;
  FLinks.Free;
  FBusyLock.Free;
  FBlackboard.Free;
  inherited;
end;

procedure TAIAgentManager.Abort;
begin
  FAbort := True;
end;

procedure TAIAgentManager.AddComponentToList(AComponent: TAIAgentsBase);
begin
  if AComponent is TAIAgentsNode then
  begin
    if FNodes.IndexOf(TAIAgentsNode(AComponent)) < 0 then
      FNodes.Add(TAIAgentsNode(AComponent));
  end
  else if AComponent is TAIAgentsLink then
  begin
    if FLinks.IndexOf(TAIAgentsLink(AComponent)) < 0 then
      FLinks.Add(TAIAgentsLink(AComponent));
  end;
end;

procedure TAIAgentManager.RemoveComponentFromList(AComponent: TAIAgentsBase);
begin
  if AComponent is TAIAgentsNode then
    FNodes.Remove(TAIAgentsNode(AComponent))
  else if AComponent is TAIAgentsLink then
    FLinks.Remove(TAIAgentsLink(AComponent));
end;

procedure TAIAgentManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if AComponent is TAIAgentsBase then
  begin
    if Operation = opInsert then
      AddComponentToList(TAIAgentsBase(AComponent))
    else if Operation = opRemove then
      RemoveComponentFromList(TAIAgentsBase(AComponent));
  end;
end;

procedure TAIAgentManager.DoPrint(Sender: TObject; Value: string);
begin
  if Assigned(FOnPrint) then
    FOnPrint(Sender, Value);
end;

procedure TAIAgentManager.DoError(Node: TAIAgentsNode; Link: TAIAgentsLink;
  E: Exception);
var
  LAbort: Boolean;
begin
  Blackboard.SetStatus(esError);
  Blackboard.SetString('Execution.ErrorMessage', E.Message);
  LAbort := True;
  if Assigned(FOnError) then
    FOnError(Self, Node, Link, E, LAbort);
  if LAbort then
    Abort;
end;

function TAIAgentManager.DoConfirm(Node: TAIAgentsNode; const AQuestion: string;
  Buttons: TMsgStates; var AResponse: string): TMsgState;
begin
  Result := msCancel;
  if Assigned(FOnConfirm) then
    FOnConfirm(Self, Node, AQuestion, Buttons, AResponse, Result)
  else
  begin
    DoError(Node, nil, Exception.Create(
      'User confirmation required but OnConfirm event is not assigned.'));
    Result := msAbort;
  end;
end;

procedure TAIAgentManager.DoNodeCompleted(ANode: TAIAgentsNode);
var
  LSnap: TAiCheckpointSnapshot;
begin
  if not Assigned(FCheckpointer) then Exit;
  InterlockedIncrement(FCheckpointSeq);
  LSnap := BuildSnapshot;
  try
    LSnap.CheckpointID := FCheckpointSeq;
    FCheckpointer.SaveCheckpoint(FCurrentThreadID, LSnap);
  finally
    LSnap.Free;
  end;
end;

procedure TAIAgentManager.DoNodeSuspended(ANode: TAIAgentsNode;
  ABeforeNode: TAIAgentsNode; ALink: TAIAgentsLink);
var
  LStep:    TAiPendingStep;
  LSnap:    TAiCheckpointSnapshot;
  SrcName:  string;
  LnkName:  string;
begin
  if Assigned(ABeforeNode) then SrcName := ABeforeNode.Name else SrcName := '';
  if Assigned(ALink)       then LnkName := ALink.Name       else LnkName := '';

  LStep := TAiPendingStep.Create(
    ANode.Name, SrcName, LnkName,
    ANode.Input, 'Suspended',
    ANode.SuspendReason, ANode.SuspendContext);

  FSuspendedStepsLock.Enter;
  try
    FSuspendedSteps.Add(LStep);
  finally
    FSuspendedStepsLock.Leave;
  end;

  Blackboard.SetStatus(esSuspended);

  if Assigned(FCheckpointer) then
  begin
    InterlockedIncrement(FCheckpointSeq);
    LSnap := BuildSnapshot;
    try
      LSnap.CheckpointID := FCheckpointSeq;
      FCheckpointer.SaveCheckpoint(FCurrentThreadID, LSnap);
    finally
      LSnap.Free;
    end;
  end;

  // Disparar OnSuspend directamente (sin GUI/TThread.Queue)
  if Assigned(FOnSuspend) then
    FOnSuspend(Self, FCurrentThreadID, ANode.Name,
               ANode.SuspendReason, ANode.SuspendContext);
end;

function TAIAgentManager.BuildSnapshot: TAiCheckpointSnapshot;
var
  LNode:     TAIAgentsNode;
  LLink:     TAIAgentsLink;
  LNodeObj:  TJSONObject;
  LJoinObj:  TJSONObject;
  LNodeStates, LLinkStates, LBBObj: TJSONObject;
  LEnum:     TAIAgentsJoinInputs.TPairEnumerator;
  LLinkObj:  TJSONObject;
  I:         Integer;
begin
  Result          := TAiCheckpointSnapshot.Create;
  Result.ThreadID := FCurrentThreadID;
  Result.GraphID  := Name;
  Result.CreatedAt := Now;

  // Blackboard
  LBBObj := TJSONObject.Create;
  SerializeBlackboard(FBlackboard, LBBObj);
  Result.Blackboard := LBBObj;

  // Estado de nodos
  LNodeStates := TJSONObject.Create;
  for I := 0 to FNodes.Count - 1 do
  begin
    LNode    := FNodes[I];
    LNodeObj := TJSONObject.Create;
    LNodeObj.Add('input',          LNode.FInput);
    LNodeObj.Add('output',         LNode.FOutput);
    LNodeObj.Add('suspended',      LNode.FSuspended);
    LNodeObj.Add('suspendReason',  LNode.FSuspendReason);
    LNodeObj.Add('suspendContext', LNode.FSuspendContext);
    LJoinObj := TJSONObject.Create;
    LNode.FJoinLock.Enter;
    try
      LEnum := LNode.FJoinInputs.GetPairEnumerator;
      try
        while LEnum.MoveNext do
          LJoinObj.Add(LEnum.Current.Key.Name, LEnum.Current.Value);
      finally
        LEnum.Free;
      end;
    finally
      LNode.FJoinLock.Leave;
    end;
    LNodeObj.Add('joinInputs', LJoinObj);
    LNodeStates.Add(LNode.Name, LNodeObj);
  end;
  Result.NodeStates := LNodeStates;

  // Estado de enlaces
  LLinkStates := TJSONObject.Create;
  for I := 0 to FLinks.Count - 1 do
  begin
    LLink    := FLinks[I];
    LLinkObj := TJSONObject.Create;
    LLinkObj.Add('noCycles', LLink.FNoCycles);
    LLinkStates.Add(LLink.Name, LLinkObj);
  end;
  Result.LinkStates := LLinkStates;

  // Pasos suspendidos
  FSuspendedStepsLock.Enter;
  try
    for I := 0 to FSuspendedSteps.Count - 1 do
    begin
      with FSuspendedSteps[I] do
        Result.PendingSteps.Add(TAiPendingStep.Create(
          NodeName, SourceNodeName, LinkName,
          Input, Status, SuspendReason, SuspendContext));
    end;
  finally
    FSuspendedStepsLock.Leave;
  end;
end;

procedure TAIAgentManager.RestoreFromSnapshot(ASnapshot: TAiCheckpointSnapshot);
var
  LNode:    TAIAgentsNode;
  LLink:    TAIAgentsLink;
  LNodeObj: TJSONObject;
  LJoinObj: TJSONObject;
  LSrcLink: TAIAgentsLink;
  LLinkObj: TJSONObject;
  I, J:     Integer;
begin
  // Blackboard
  if Assigned(ASnapshot.Blackboard) then
    DeserializeBlackboard(ASnapshot.Blackboard, FBlackboard);

  // Estado de nodos
  if Assigned(ASnapshot.NodeStates) then
    for I := 0 to FNodes.Count - 1 do
    begin
      LNode    := FNodes[I];
      LNodeObj := ASnapshot.NodeStates.Find(LNode.Name) as TJSONObject;
      if not Assigned(LNodeObj) then Continue;
      LNode.FInput          := LNodeObj.Get('input',          '');
      LNode.FOutput         := LNodeObj.Get('output',         '');
      LNode.FSuspended      := LNodeObj.Get('suspended',      False);
      LNode.FSuspendReason  := LNodeObj.Get('suspendReason',  '');
      LNode.FSuspendContext := LNodeObj.Get('suspendContext',  '');
      LJoinObj := LNodeObj.Find('joinInputs') as TJSONObject;
      if Assigned(LJoinObj) then
      begin
        LNode.FJoinLock.Enter;
        try
          LNode.FJoinInputs.Clear;
          for J := 0 to LJoinObj.Count - 1 do
          begin
            LSrcLink := FindLink(LJoinObj.Names[J]);
            if Assigned(LSrcLink) then
              LNode.FJoinInputs.AddOrSetValue(LSrcLink, LJoinObj.Items[J].AsString);
          end;
        finally
          LNode.FJoinLock.Leave;
        end;
      end;
    end;

  // Estado de enlaces
  if Assigned(ASnapshot.LinkStates) then
    for I := 0 to FLinks.Count - 1 do
    begin
      LLink    := FLinks[I];
      LLinkObj := ASnapshot.LinkStates.Find(LLink.Name) as TJSONObject;
      if Assigned(LLinkObj) then
        LLink.FNoCycles := LLinkObj.Get('noCycles', 0);
    end;

  // Pasos suspendidos
  FSuspendedStepsLock.Enter;
  try
    FSuspendedSteps.Clear;
    for I := 0 to ASnapshot.PendingSteps.Count - 1 do
      with ASnapshot.PendingSteps[I] do
        FSuspendedSteps.Add(TAiPendingStep.Create(
          NodeName, SourceNodeName, LinkName,
          Input, Status, SuspendReason, SuspendContext));
  finally
    FSuspendedStepsLock.Leave;
  end;
end;

function TAIAgentManager.FindLink(const AName: string): TAIAgentsLink;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FLinks.Count - 1 do
    if SameText(FLinks[I].Name, AName) then
      Exit(FLinks[I]);
end;

function TAIAgentManager.FindNode(const AName: string): TAIAgentsNode;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is TAIAgentsNode) and
       SameText(Components[I].Name, AName) then
      Exit(TAIAgentsNode(Components[I]));
end;

procedure TAIAgentManager.ClearGraph;
var
  I: Integer;
begin
  FStartNode := nil;
  FEndNode   := nil;
  FNodes.Clear;
  FLinks.Clear;
  for I := ComponentCount - 1 downto 0 do
    if Components[I] is TAIAgentsBase then
      Components[I].Free;
  FCompiled := False;
end;

procedure TAIAgentManager.Compile;
var
  LNode:       TAIAgentsNode;
  LLink:       TAIAgentsLink;
  LTargetNode: TAIAgentsNode;
  StartCount, EndCount, I: Integer;
  LEnum:       TAIAgentsLinkNodeMap.TValueEnumerator;
begin
  if FCompiled then Exit;
  try
    FAbort := False;
    FBlackboard.Clear;

    if not Assigned(FStartNode) then
      raise Exception.Create('StartNode is not assigned.');
    if not Assigned(FEndNode) then
      raise Exception.Create('EndNode is not assigned.');

    StartCount := 0;
    EndCount   := 0;
    for I := 0 to FNodes.Count - 1 do
    begin
      LNode := FNodes[I];
      if LNode = FStartNode then Inc(StartCount);
      if LNode = FEndNode   then Inc(EndCount);
      LNode.Reset;
    end;
    if StartCount <> 1 then
      raise Exception.CreateFmt('Invalid StartNode count: %d (expected 1).', [StartCount]);
    if EndCount <> 1 then
      raise Exception.CreateFmt('Invalid EndNode count: %d (expected 1).', [EndCount]);

    for I := 0 to FLinks.Count - 1 do
    begin
      LLink := FLinks[I];
      LLink.Ready    := False;
      LLink.NoCycles := 0;
      if not Assigned(LLink.FSourceNode) then
        raise Exception.CreateFmt('Link "%s" has no source node.', [LLink.Name]);

      if Assigned(LLink.NextA)  then LLink.NextA.FInEdges.Add(LLink);
      if Assigned(LLink.NextB)  then LLink.NextB.FInEdges.Add(LLink);
      if Assigned(LLink.NextC)  then LLink.NextC.FInEdges.Add(LLink);
      if Assigned(LLink.NextD)  then LLink.NextD.FInEdges.Add(LLink);
      if Assigned(LLink.NextNo) then LLink.NextNo.FInEdges.Add(LLink);

      if Assigned(LLink.FConditionalTargets) then
      begin
        LEnum := LLink.FConditionalTargets.GetValueEnumerator;
        try
          while LEnum.MoveNext do
          begin
            LTargetNode := LEnum.Current;
            LTargetNode.FInEdges.Add(LLink);
          end;
        finally
          LEnum.Free;
        end;
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

function TAIAgentManager.AddNode(const AName: string;
  AExecuteProc: TAIAgentsNodeOnExecute): TAIAgentManager;
var
  LNode: TAIAgentsNode;
begin
  FCompiled := False;
  if FindNode(AName) <> nil then
    raise Exception.CreateFmt('Node "%s" already exists.', [AName]);
  LNode := TAIAgentsNode.Create(Self);
  LNode.Name      := AName;
  LNode.Graph     := Self;
  LNode.OnExecute := AExecuteProc;
  Result := Self;
end;

function TAIAgentManager.AddEdge(const AStartNodeName, AEndNodeName: string): TAIAgentManager;
var
  LStart, LEnd: TAIAgentsNode;
  LLink:        TAIAgentsLink;
begin
  FCompiled := False;
  LStart := FindNode(AStartNodeName);
  LEnd   := FindNode(AEndNodeName);
  if not Assigned(LStart) then
    raise Exception.CreateFmt('AddEdge: start node "%s" not found.', [AStartNodeName]);
  if not Assigned(LEnd) then
    raise Exception.CreateFmt('AddEdge: end node "%s" not found.', [AEndNodeName]);
  LLink := TAIAgentsLink.Create(Self);
  LLink.Name   := 'Link_' + AStartNodeName + '_to_' + AEndNodeName;
  LLink.Graph  := Self;
  LLink.Mode   := lmFanout;
  LStart.Next  := LLink;
  LLink.NextA  := LEnd;
  Result := Self;
end;

function TAIAgentManager.AddConditionalEdge(const AStartNodeName: string;
  const AConditionalLinkName: string;
  AConditionalTargets: specialize TDictionary<string, string>): TAIAgentManager;
var
  LStart, LTarget: TAIAgentsNode;
  LLink:           TAIAgentsLink;
  LEnum:           specialize TDictionary<string, string>.TPairEnumerator;
begin
  FCompiled := False;
  LStart := FindNode(AStartNodeName);
  if not Assigned(LStart) then
    raise Exception.CreateFmt('AddConditionalEdge: node "%s" not found.', [AStartNodeName]);

  LLink      := TAIAgentsLink.Create(Self);
  LLink.Name := AConditionalLinkName;
  LLink.Graph := Self;
  LLink.Mode := lmConditional;

  LEnum := AConditionalTargets.GetPairEnumerator;
  try
    while LEnum.MoveNext do
    begin
      LTarget := FindNode(LEnum.Current.Value);
      if not Assigned(LTarget) then
        raise Exception.CreateFmt('AddConditionalEdge: target node "%s" not found.',
          [LEnum.Current.Value]);
      LLink.AddConditionalTarget(LEnum.Current.Key, LTarget);
    end;
  finally
    LEnum.Free;
  end;

  LStart.Next := LLink;
  Result := Self;
end;

function TAIAgentManager.SetEntryPoint(const ANodeName: string): TAIAgentManager;
begin
  StartNode := FindNode(ANodeName);
  if not Assigned(StartNode) then
    raise Exception.CreateFmt('Entry point "%s" not found.', [ANodeName]);
  Result := Self;
end;

function TAIAgentManager.SetFinishPoint(const ANodeName: string): TAIAgentManager;
begin
  EndNode := FindNode(ANodeName);
  if not Assigned(EndNode) then
    raise Exception.CreateFmt('Finish point "%s" not found.', [ANodeName]);
  Result := Self;
end;

function TAIAgentManager.NewMessage(APrompt, ARole: string;
  AMediaFiles: TAiMediaFilesArray): TAiChatMessage;
var
  MF: TAiMediaFile;
begin
  Result := TAiChatMessage.Create(APrompt, ARole);
  try
    for MF in AMediaFiles do
      if Assigned(MF) then
        Result.AddMediaFile(MF);
  except
    Result.Free;
    raise;
  end;
end;

function TAIAgentManager.Run(APrompt: string): string;
var
  LNewGUID: TGUID;
begin
  Result := '';

  if TrySetBusy then
    raise Exception.Create('The Agent Manager is currently busy.');

  // Si no hay mensaje previo, crear uno
  if not Assigned(Blackboard.AskMsg) then
    Blackboard.AskMsg := NewMessage(APrompt, 'user', []);
  // Reiniciar respuesta
  Blackboard.ResMsg := TAiChatMessage.Create('', 'assistant');

  // Generar ThreadID unico
  CreateGUID(LNewGUID);
  FCurrentThreadID := GUIDToString(LNewGUID);
  FCheckpointSeq   := 0;
  FActiveTaskCount := 0;
  FAbort           := False;

  FSuspendedStepsLock.Enter;
  try
    FSuspendedSteps.Clear;
  finally
    FSuspendedStepsLock.Leave;
  end;

  Blackboard.SetStatus(esRunning);
  Compile;

  // Liberar orquestador previo si existe
  FreeAndNil(FOrchestratorThread);

  if FAsynchronous then
  begin
    FOrchestratorThread := TAiOrchestratorThread.Create(
      Self, APrompt, FStartNode, False);
    FOrchestratorThread.Start;
    Result := '';
  end
  else
  begin
    // Modo sincrono: crear hilo, esperar, liberar
    FOrchestratorThread := TAiOrchestratorThread.Create(
      Self, APrompt, FStartNode, False);
    FOrchestratorThread.Start;
    FOrchestratorThread.WaitFor;
    FreeAndNil(FOrchestratorThread);

    // Verificar status
    case Blackboard.GetStatus of
      esError:
        raise Exception.Create('Execution failed: ' +
          Blackboard.GetString('Execution.ErrorMessage'));
      esTimeout:
        raise Exception.Create('Execution timed out.');
      esAborted:
        raise Exception.Create('Execution aborted.');
    end;

    if Assigned(Blackboard.ResMsg) and (Blackboard.ResMsg.Content <> '') then
      Result := Blackboard.ResMsg.Content
    else if Assigned(FEndNode) then
      Result := FEndNode.Output;
  end;
end;

function TAIAgentManager.AddMessageAndRun(APrompt, ARole: string;
  AMediaFiles: TAiMediaFilesArray): string;
begin
  Blackboard.AskMsg := NewMessage(APrompt, ARole, AMediaFiles);
  Result := Run(APrompt);
end;

function TAIAgentManager.AddMessageAndRunMsg(APrompt, ARole: string;
  AMediaFiles: TAiMediaFilesArray): TAiChatMessage;
begin
  if FAsynchronous then
    raise Exception.Create(
      'AddMessageAndRunMsg only works in synchronous mode (Asynchronous = False).');
  AddMessageAndRun(APrompt, ARole, AMediaFiles);
  Result := Blackboard.ResMsg;
end;

function TAIAgentManager.ResumeThread(const AThreadID, ANodeName,
  AInput: string): Boolean;
var
  LSnap: TAiCheckpointSnapshot;
  LNode: TAIAgentsNode;
  I:     Integer;
begin
  Result := False;

  if TrySetBusy then
    raise Exception.Create('Agent is busy.');

  try
    // Cargar checkpoint del disco
    LSnap := nil;
    if Assigned(FCheckpointer) then
      LSnap := FCheckpointer.LoadCheckpoint(AThreadID);

    try
      if Assigned(LSnap) then
      begin
        RestoreFromSnapshot(LSnap);
        FCurrentThreadID := AThreadID;
      end
      else if FCurrentThreadID <> AThreadID then
      begin
        ClearBusy;
        Exit;
      end;
    finally
      LSnap.Free;
    end;

    LNode := FindNode(ANodeName);
    if not Assigned(LNode) then
    begin
      ClearBusy;
      raise Exception.CreateFmt('Node "%s" not found.', [ANodeName]);
    end;

    // Preparar nodo para reanudacion
    LNode.FInput          := AInput;
    LNode.FSuspended      := False;
    LNode.FSuspendReason  := '';
    LNode.FSuspendContext := '';

    FSuspendedStepsLock.Enter;
    try
      for I := FSuspendedSteps.Count - 1 downto 0 do
        if SameText(FSuspendedSteps[I].NodeName, ANodeName) then
          FSuspendedSteps.Delete(I);
    finally
      FSuspendedStepsLock.Leave;
    end;

    FAbort           := False;
    FActiveTaskCount := 0;
    Blackboard.SetStatus(esRunning);

    // Liberar orquestador previo
    FreeAndNil(FOrchestratorThread);

    FOrchestratorThread := TAiOrchestratorThread.Create(Self, AInput, LNode, True);
    FOrchestratorThread.Start;

    if not FAsynchronous then
    begin
      FOrchestratorThread.WaitFor;
      FreeAndNil(FOrchestratorThread);
    end;

    Result := True;
  except
    ClearBusy;
    raise;
  end;
end;

function TAIAgentManager.GetActiveThreads: TStringDynArray;
begin
  if Assigned(FCheckpointer) then
    Result := FCheckpointer.GetActiveThreadIDs
  else
  begin
    if (FCurrentThreadID <> '') and
       (Blackboard.GetStatus = esSuspended) then
    begin
      SetLength(Result, 1);
      Result[0] := FCurrentThreadID;
    end
    else
      SetLength(Result, 0);
  end;
end;

// ---------------------------------------------------------------------------
// SaveToStream / LoadFromStream (JSON grafo completo)
// ---------------------------------------------------------------------------

procedure TAIAgentManager.SaveToStream(AStream: TStream);
var
  LRoot, LGraphObj, LNodeObj, LLinkObj,
  LTargetsObj, LExpObj, LCondTargetsObj,
  LToolDataObj: TJSONObject;
  LNodesArr, LLinksArr: TJSONArray;
  LNode:   TAIAgentsNode;
  LLink:   TAIAgentsLink;
  LWriter: TStringStream;
  LEnumC:  TAIAgentsLinkNodeMap.TPairEnumerator;
  LPropsJ: TJSONObject;
  I:       Integer;
begin
  LRoot := TJSONObject.Create;
  try
    // graph
    LGraphObj := TJSONObject.Create;
    LGraphObj.Add('description', FDescription);
    if Assigned(FStartNode) then LGraphObj.Add('startNode', FStartNode.Name)
    else                         LGraphObj.Add('startNode', TJSONNull.Create);
    if Assigned(FEndNode)   then LGraphObj.Add('endNode',   FEndNode.Name)
    else                         LGraphObj.Add('endNode',   TJSONNull.Create);
    LGraphObj.Add('maxConcurrentTasks', FMaxConcurrentTasks);
    LGraphObj.Add('timeoutMs',          Integer(FTimeoutMs));
    LRoot.Add('graph', LGraphObj);

    // nodes
    LNodesArr := TJSONArray.Create;
    for I := 0 to FNodes.Count - 1 do
    begin
      LNode    := FNodes[I];
      LNodeObj := TJSONObject.Create;
      LNodeObj.Add('name',        LNode.Name);
      LNodeObj.Add('description', LNode.Description);
      LNodeObj.Add('joinMode', GetEnumName(TypeInfo(TJoinMode), Ord(LNode.JoinMode)));
      LNodeObj.Add('promptName',  LNode.PromptName);

      if Assigned(LNode.Tool) then
      begin
        LToolDataObj := TJSONObject.Create;
        LToolDataObj.Add('className', LNode.Tool.ClassName);
        LPropsJ := TJSONObject.Create;
        SerializeToolProperties(LNode.Tool, LPropsJ);
        LToolDataObj.Add('properties', LPropsJ);
        LNodeObj.Add('tool', LToolDataObj);
      end
      else
        LNodeObj.Add('tool', TJSONNull.Create);

      if Assigned(LNode.Next) then LNodeObj.Add('nextLink', LNode.Next.Name)
      else                         LNodeObj.Add('nextLink', TJSONNull.Create);

      LNodesArr.Add(LNodeObj);
    end;
    LRoot.Add('nodes', LNodesArr);

    // links
    LLinksArr := TJSONArray.Create;
    for I := 0 to FLinks.Count - 1 do
    begin
      LLink    := FLinks[I];
      LLinkObj := TJSONObject.Create;
      LLinkObj.Add('name',        LLink.Name);
      LLinkObj.Add('description', LLink.Description);
      LLinkObj.Add('mode', GetEnumName(TypeInfo(TLinkMode), Ord(LLink.Mode)));
      LLinkObj.Add('maxCycles', LLink.MaxCycles);
      if Assigned(LLink.FSourceNode) then
        LLinkObj.Add('sourceNode', LLink.FSourceNode.Name)
      else
        LLinkObj.Add('sourceNode', TJSONNull.Create);

      LTargetsObj := TJSONObject.Create;
      if Assigned(LLink.NextA)  then LTargetsObj.Add('nextA',  LLink.NextA.Name)  else LTargetsObj.Add('nextA',  TJSONNull.Create);
      if Assigned(LLink.NextB)  then LTargetsObj.Add('nextB',  LLink.NextB.Name)  else LTargetsObj.Add('nextB',  TJSONNull.Create);
      if Assigned(LLink.NextC)  then LTargetsObj.Add('nextC',  LLink.NextC.Name)  else LTargetsObj.Add('nextC',  TJSONNull.Create);
      if Assigned(LLink.NextD)  then LTargetsObj.Add('nextD',  LLink.NextD.Name)  else LTargetsObj.Add('nextD',  TJSONNull.Create);
      if Assigned(LLink.NextNo) then LTargetsObj.Add('nextNo', LLink.NextNo.Name) else LTargetsObj.Add('nextNo', TJSONNull.Create);
      LLinkObj.Add('targets', LTargetsObj);

      if LLink.Mode = lmExpression then
      begin
        LExpObj := TJSONObject.Create;
        LExpObj.Add('expressionA', LLink.ExpressionA);
        LExpObj.Add('expressionB', LLink.ExpressionB);
        LExpObj.Add('expressionC', LLink.ExpressionC);
        LExpObj.Add('expressionD', LLink.ExpressionD);
        LLinkObj.Add('expressions', LExpObj);
      end;

      if LLink.Mode = lmConditional then
      begin
        LLinkObj.Add('conditionalKey', LLink.ConditionalKey);
        if Assigned(LLink.FConditionalTargets) then
        begin
          LCondTargetsObj := TJSONObject.Create;
          LEnumC := LLink.FConditionalTargets.GetPairEnumerator;
          try
            while LEnumC.MoveNext do
              LCondTargetsObj.Add(LEnumC.Current.Key, LEnumC.Current.Value.Name);
          finally
            LEnumC.Free;
          end;
          LLinkObj.Add('conditionalTargets', LCondTargetsObj);
        end;
      end;

      LLinksArr.Add(LLinkObj);
    end;
    LRoot.Add('links', LLinksArr);

    LWriter := TStringStream.Create(LRoot.AsJSON);
    try
      AStream.CopyFrom(LWriter, 0);
    finally
      LWriter.Free;
    end;
  finally
    LRoot.Free;
  end;
end;

procedure TAIAgentManager.LoadFromStream(AStream: TStream);
var
  LRoot, LGraphJSON, LNodeJSON, LLinkJSON, LTargetsJSON,
  LCondTargetsJSON, LToolDataObj, LPropsJSON: TJSONObject;
  LNodesArr, LLinksArr: TJSONArray;
  LContent: TStringStream;
  LJsonData: TJSONData;
  LNode: TAIAgentsNode;
  LLink: TAIAgentsLink;
  LNodeMap: specialize TDictionary<string, TAIAgentsNode>;
  LLinkMap: specialize TDictionary<string, TAIAgentsLink>;
  LKey, LToolClassName: string;
  LToolClass: TClass;
  LJoinModeOrd, LLinkModeOrd, I, J: Integer;
begin
  if not Assigned(AStream) or (AStream.Size = 0) then Exit;
  ClearGraph;

  LNodeMap := specialize TDictionary<string, TAIAgentsNode>.Create;
  LLinkMap := specialize TDictionary<string, TAIAgentsLink>.Create;
  LContent := TStringStream.Create('');
  try
    LContent.CopyFrom(AStream, 0);
    LJsonData := GetJSON(LContent.DataString);
    if not (LJsonData is TJSONObject) then
      raise Exception.Create('Invalid JSON format.');
    LRoot := TJSONObject(LJsonData);
    try
      // Pasada 1A: crear nodos
      LNodesArr := LRoot.Find('nodes') as TJSONArray;
      if Assigned(LNodesArr) then
        for I := 0 to LNodesArr.Count - 1 do
        begin
          LNodeJSON := LNodesArr.Items[I] as TJSONObject;
          LKey := LNodeJSON.Get('name', '');
          if LKey = '' then raise Exception.Create('Node without name.');

          LNode := TAIAgentsNode.Create(Self);
          LNode.Name        := LKey;
          LNode.Description := LNodeJSON.Get('description', '');
          LNode.PromptName  := LNodeJSON.Get('promptName',  '');

          LJoinModeOrd := GetEnumValue(TypeInfo(TJoinMode),
                            LNodeJSON.Get('joinMode', 'jmAny'));
          if LJoinModeOrd >= 0 then
            LNode.JoinMode := TJoinMode(LJoinModeOrd);

          // Tool
          LToolDataObj := LNodeJSON.Find('tool') as TJSONObject;
          if Assigned(LToolDataObj) then
          begin
            LToolClassName := LToolDataObj.Get('className', '');
            if LToolClassName <> '' then
            begin
              LToolClass := TEngineRegistry.Instance.FindToolClass(LToolClassName);
              if Assigned(LToolClass) and LToolClass.InheritsFrom(TAiToolBase) then
              begin
                LNode.Tool := TAiToolBase(TComponentClass(LToolClass).Create(LNode));
                LPropsJSON := LToolDataObj.Find('properties') as TJSONObject;
                if Assigned(LPropsJSON) then
                  DeserializeToolProperties(LNode.Tool, LPropsJSON);
              end;
            end;
          end;

          LNodeMap.Add(LKey, LNode);
        end;

      // Pasada 1B: crear enlaces
      LLinksArr := LRoot.Find('links') as TJSONArray;
      if Assigned(LLinksArr) then
        for I := 0 to LLinksArr.Count - 1 do
        begin
          LLinkJSON := LLinksArr.Items[I] as TJSONObject;
          LKey := LLinkJSON.Get('name', '');
          if LKey = '' then raise Exception.Create('Link without name.');

          LLink := TAIAgentsLink.Create(Self);
          LLink.Name        := LKey;
          LLink.Description := LLinkJSON.Get('description', '');
          LLink.MaxCycles   := LLinkJSON.Get('maxCycles',   1);

          LLinkModeOrd := GetEnumValue(TypeInfo(TLinkMode),
                            LLinkJSON.Get('mode', 'lmFanout'));
          if LLinkModeOrd >= 0 then
            LLink.Mode := TLinkMode(LLinkModeOrd);

          if LLink.Mode = lmConditional then
            LLink.ConditionalKey := LLinkJSON.Get('conditionalKey', 'next_route');

          LLinkMap.Add(LKey, LLink);
        end;

      // Pasada 2: conectar
      if Assigned(LNodesArr) then
        for I := 0 to LNodesArr.Count - 1 do
        begin
          LNodeJSON := LNodesArr.Items[I] as TJSONObject;
          LNode     := LNodeMap[LNodeJSON.Get('name', '')];
          LKey      := LNodeJSON.Get('nextLink', '');
          if (LKey <> '') and LLinkMap.TryGetValue(LKey, LLink) then
            LNode.Next := LLink;
        end;

      if Assigned(LLinksArr) then
        for I := 0 to LLinksArr.Count - 1 do
        begin
          LLinkJSON  := LLinksArr.Items[I] as TJSONObject;
          LLink      := LLinkMap[LLinkJSON.Get('name', '')];
          LKey := LLinkJSON.Get('sourceNode', '');
          if (LKey <> '') and LNodeMap.TryGetValue(LKey, LNode) then
            LLink.FSourceNode := LNode;

          LTargetsJSON := LLinkJSON.Find('targets') as TJSONObject;
          if Assigned(LTargetsJSON) then
          begin
            LKey := LTargetsJSON.Get('nextA', '');  if (LKey <> '') and LNodeMap.TryGetValue(LKey, LNode) then LLink.NextA := LNode;
            LKey := LTargetsJSON.Get('nextB', '');  if (LKey <> '') and LNodeMap.TryGetValue(LKey, LNode) then LLink.NextB := LNode;
            LKey := LTargetsJSON.Get('nextC', '');  if (LKey <> '') and LNodeMap.TryGetValue(LKey, LNode) then LLink.NextC := LNode;
            LKey := LTargetsJSON.Get('nextD', '');  if (LKey <> '') and LNodeMap.TryGetValue(LKey, LNode) then LLink.NextD := LNode;
            LKey := LTargetsJSON.Get('nextNo', ''); if (LKey <> '') and LNodeMap.TryGetValue(LKey, LNode) then LLink.NextNo := LNode;
          end;

          LCondTargetsJSON := LLinkJSON.Find('conditionalTargets') as TJSONObject;
          if Assigned(LCondTargetsJSON) then
            for J := 0 to LCondTargetsJSON.Count - 1 do
            begin
              LKey := LCondTargetsJSON.Items[J].AsString;
              if LNodeMap.TryGetValue(LKey, LNode) then
                LLink.AddConditionalTarget(LCondTargetsJSON.Names[J], LNode);
            end;
        end;

      // Graph config
      LGraphJSON := LRoot.Find('graph') as TJSONObject;
      if Assigned(LGraphJSON) then
      begin
        FDescription        := LGraphJSON.Get('description',        '');
        FMaxConcurrentTasks := LGraphJSON.Get('maxConcurrentTasks', 4);
        FTimeoutMs          := Cardinal(LGraphJSON.Get('timeoutMs', 60000));
        LKey := LGraphJSON.Get('startNode', '');
        if (LKey <> '') and LNodeMap.TryGetValue(LKey, LNode) then StartNode := LNode;
        LKey := LGraphJSON.Get('endNode', '');
        if (LKey <> '') and LNodeMap.TryGetValue(LKey, LNode) then EndNode   := LNode;
      end;

      FCompiled := False;
    finally
      LRoot.Free;
    end;
  finally
    LContent.Free;
    LLinkMap.Free;
    LNodeMap.Free;
  end;
end;

procedure TAIAgentManager.SaveStateToStream(AStream: TStream);
var
  LRoot, LStateObj, LBBObj, LNodeStatesObj, LLinkStatesObj,
  LNodeStateObj, LJoinInputsObj, LLinkStateObj: TJSONObject;
  LWriter: TStringStream;
  LNode:   TAIAgentsNode;
  LLink:   TAIAgentsLink;
  LEnum:   TAIAgentsJoinInputs.TPairEnumerator;
  I:       Integer;
begin
  LRoot := TJSONObject.Create;
  try
    LStateObj := TJSONObject.Create;
    LRoot.Add('executionState', LStateObj);

    LBBObj := TJSONObject.Create;
    SerializeBlackboard(FBlackboard, LBBObj);
    LStateObj.Add('blackboard', LBBObj);

    LNodeStatesObj := TJSONObject.Create;
    for I := 0 to FNodes.Count - 1 do
    begin
      LNode        := FNodes[I];
      LNodeStateObj := TJSONObject.Create;
      LNodeStateObj.Add('input',  LNode.Input);
      LNodeStateObj.Add('output', LNode.Output);

      LJoinInputsObj := TJSONObject.Create;
      LNode.FJoinLock.Enter;
      try
        LEnum := LNode.FJoinInputs.GetPairEnumerator;
        try
          while LEnum.MoveNext do
            LJoinInputsObj.Add(LEnum.Current.Key.Name, LEnum.Current.Value);
        finally
          LEnum.Free;
        end;
      finally
        LNode.FJoinLock.Leave;
      end;
      LNodeStateObj.Add('joinInputs', LJoinInputsObj);
      LNodeStatesObj.Add(LNode.Name, LNodeStateObj);
    end;
    LStateObj.Add('nodeStates', LNodeStatesObj);

    LLinkStatesObj := TJSONObject.Create;
    for I := 0 to FLinks.Count - 1 do
    begin
      LLink        := FLinks[I];
      LLinkStateObj := TJSONObject.Create;
      LLinkStateObj.Add('noCycles', LLink.NoCycles);
      LLinkStatesObj.Add(LLink.Name, LLinkStateObj);
    end;
    LStateObj.Add('linkStates', LLinkStatesObj);

    LWriter := TStringStream.Create(LRoot.AsJSON);
    try
      AStream.CopyFrom(LWriter, 0);
    finally
      LWriter.Free;
    end;
  finally
    LRoot.Free;
  end;
end;

procedure TAIAgentManager.LoadStateFromStream(AStream: TStream);
var
  LRoot, LStateObj, LBBObj, LNodeStatesObj,
  LLinkStatesObj, LNodeStateObj, LJoinInputsObj, LLinkStateObj: TJSONObject;
  LContent: TStringStream;
  LJsonData: TJSONData;
  LNode:    TAIAgentsNode;
  LLink, LSrcLink: TAIAgentsLink;
  LLinkMap: specialize TDictionary<string, TAIAgentsLink>;
  I, J:     Integer;
  LKey:     string;
begin
  if ComponentCount = 0 then
    raise Exception.Create('Cannot load state into empty graph.');

  LLinkMap := specialize TDictionary<string, TAIAgentsLink>.Create;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TAIAgentsLink then
      LLinkMap.Add(Components[I].Name, TAIAgentsLink(Components[I]));

  LContent := TStringStream.Create('');
  try
    LContent.CopyFrom(AStream, 0);
    LJsonData := GetJSON(LContent.DataString);
    if not (LJsonData is TJSONObject) then
      raise Exception.Create('Invalid state JSON.');
    LRoot := TJSONObject(LJsonData);
    try
      LStateObj := LRoot.Find('executionState') as TJSONObject;
      if not Assigned(LStateObj) then Exit;

      LBBObj := LStateObj.Find('blackboard') as TJSONObject;
      if Assigned(LBBObj) then
        DeserializeBlackboard(LBBObj, FBlackboard);

      LNodeStatesObj := LStateObj.Find('nodeStates') as TJSONObject;
      if Assigned(LNodeStatesObj) then
        for I := 0 to LNodeStatesObj.Count - 1 do
        begin
          LNode := FindNode(LNodeStatesObj.Names[I]);
          if not Assigned(LNode) then Continue;
          LNodeStateObj := LNodeStatesObj.Items[I] as TJSONObject;
          LNode.Input  := LNodeStateObj.Get('input',  '');
          LNode.Output := LNodeStateObj.Get('output', '');

          LNode.FJoinInputs.Clear;
          LJoinInputsObj := LNodeStateObj.Find('joinInputs') as TJSONObject;
          if Assigned(LJoinInputsObj) then
            for J := 0 to LJoinInputsObj.Count - 1 do
            begin
              LKey := LJoinInputsObj.Names[J];
              if LLinkMap.TryGetValue(LKey, LSrcLink) then
                LNode.FJoinInputs.AddOrSetValue(LSrcLink,
                  LJoinInputsObj.Items[J].AsString);
            end;
        end;

      LLinkStatesObj := LStateObj.Find('linkStates') as TJSONObject;
      if Assigned(LLinkStatesObj) then
        for I := 0 to LLinkStatesObj.Count - 1 do
        begin
          LKey := LLinkStatesObj.Names[I];
          if LLinkMap.TryGetValue(LKey, LLink) then
          begin
            LLinkStateObj := LLinkStatesObj.Items[I] as TJSONObject;
            LLink.FNoCycles := LLinkStateObj.Get('noCycles', 0);
          end;
        end;
    finally
      LRoot.Free;
    end;
  finally
    LContent.Free;
    LLinkMap.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Setters TAIAgentManager
// ---------------------------------------------------------------------------

procedure TAIAgentManager.SetAsynchronous(const Value: Boolean);
begin FAsynchronous := Value; end;

procedure TAIAgentManager.SetCheckpointer(const Value: IAiCheckpointer);
begin FCheckpointer := Value; end;

procedure TAIAgentManager.SetDescription(const Value: string);
begin FDescription := Value; end;

procedure TAIAgentManager.SetEndNode(const Value: TAIAgentsNode);
begin
  if FEndNode <> Value then
  begin
    FEndNode  := Value;
    FCompiled := False;
  end;
end;

procedure TAIAgentManager.SetMaxConcurrentTasks(const Value: Integer);
begin
  if Value >= 1 then
    FMaxConcurrentTasks := Value
  else
    FMaxConcurrentTasks := 1;
end;

procedure TAIAgentManager.SetOnConfirm(const Value: TAIAgentsOnConfirm);
begin FOnConfirm := Value; end;

procedure TAIAgentManager.SetOnEnd(const Value: TAIAgentsOnEnd);
begin FOnEnd := Value; end;

procedure TAIAgentManager.SetOnEnterNode(const Value: TAIAgentsOnEnterNode);
begin FOnEnterNode := Value; end;

procedure TAIAgentManager.SetOnError(const Value: TAIAgentsOnError);
begin FOnError := Value; end;

procedure TAIAgentManager.SetOnExitNode(const Value: TAIAgentsOnExitNode);
begin FOnExitNode := Value; end;

procedure TAIAgentManager.SetOnFinish(const Value: TAIAgentsOnFinish);
begin FOnFinish := Value; end;

procedure TAIAgentManager.SetOnPrint(const Value: TAIAgentsOnPrint);
begin FOnPrint := Value; end;

procedure TAIAgentManager.SetOnStart(const Value: TAIAgentsOnStart);
begin FOnStart := Value; end;

procedure TAIAgentManager.SetOnSuspend(const Value: TAIAgentsOnSuspend);
begin FOnSuspend := Value; end;

procedure TAIAgentManager.SetStartNode(const Value: TAIAgentsNode);
begin
  if FStartNode <> Value then
  begin
    FStartNode := Value;
    FCompiled  := False;
  end;
end;

// ---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('MakerAI',
    [TAIAgentManager, TAIAgentsNode, TAIAgentsLink, TAiAgentsToolSample, TAIAgents]);
end;

end.
