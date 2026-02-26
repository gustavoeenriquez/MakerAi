// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Agents;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults, System.TypInfo,
  System.Bindings.Evaluator, System.Bindings.Helper, System.Bindings.Expression, System.Bindings.Consts,
  System.JSON, System.Math,
  System.Bindings.EvalSys, System.Bindings.Factories, System.Bindings.EvalProtocol, System.Bindings.ObjEval,
  System.Threading, System.Rtti, System.SyncObjs, System.Types, System.StrUtils,

  uMakerAi.Chat, uMakerAi.Core, uMakerAi.Chat.Messages,
  uMakerAi.Agents.Checkpoint;  // IAiCheckpointer, TAiCheckpointSnapshot, TAiPendingStep

type

{$RTTI INHERIT}
  TMsgState = (msYes, msNo, msOK, msCancel, msAbort, msRetry, msIgnore, msAll, msNoToAll, msYesToAll, msHelp, msClose);
  TMsgStates = set of TMsgState;

  TAgentExecutionStatus = (esUnknown, esRunning, esCompleted, esError,
                           esTimeout, esAborted, esSuspended);

  // Modo de uni?n para los nodos
  TJoinMode = (jmAny, jmAll);

  // Modo de ejecuci?n del enlace
  TLinkMode = (lmFanout, lmConditional, lmManual, lmExpression); // --- NUEVO: Modo lmExpression ---

  // Forward Declarations
  TAIAgentManager = class;
  TAIAgentsBase = class;
  TAIAgentsNode = class;
  TAIAgentsLink = class;
  TAIBlackboard = class;
  TAiToolBase = Class;

  TAIAgentsOnPrint = procedure(Sender: TObject; Value: String) of object;
  TAgentPrintRef = reference to procedure(Sender: TObject; Value: String);

  TAIAgentsOnEnd = procedure(Node: TAIAgentsNode; Value: string) of object;
  TAIAgentsNodeOnExecute = procedure(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: String; var Output: String) of object;
  TAIAgentsLinkOnExecute = procedure(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk: Boolean; var Handled: Boolean) of object;
  TAIAgentsOnError = procedure(Sender: TObject; Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception; var Abort: Boolean) of object;
  TAIAgentsOnConfirm = procedure(Sender: TObject; Node: TAIAgentsNode; const AQuestion: string; Buttons: TMsgStates; var AResponse: string; var AModalResult: TMsgState) of object;

  TAIAgentsOnEnterNode = procedure(Sender: TObject; Node: TAIAgentsNode) of object;
  TAIAgentsOnExitNode = procedure(Sender: TObject; Node: TAIAgentsNode) of object;
  TAIAgentsOnStart = procedure(Sender: TObject; const Input: string) of object;
  // TAIAgentsOnFinish = procedure(Sender: TObject; const Input, Output: string; Status: string; E: Exception) of object;
  TAIAgentsOnFinish = procedure(Sender: TObject; const Input, Output: string; Status: TAgentExecutionStatus; E: Exception) of object;

  // Disparado cuando un nodo se suspende esperando input humano
  TAIAgentsOnSuspend = procedure(Sender: TObject;
                                 const AThreadID:  string;
                                 const ANodeName:  string;
                                 const AReason:    string;
                                 const AContext:   string) of object;

  // --- Blackboard ---
  TAIBlackboard = class(TObject)
  private
    FLock: TCriticalSection;
    function GetAskMsg: TAiChatMessage;
    function GetResMsg: TAiChatMessage;
    procedure SetAskMsg(const Value: TAiChatMessage);
    procedure SetResMsg(const Value: TAiChatMessage);
  protected // --- MODIFICADO: protected para acceso desde la misma unidad ---
    FData: TDictionary<string, TValue>;
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
    procedure SetStatus(Value: TAgentExecutionStatus);
    function GetStatus: TAgentExecutionStatus;
    function GetBoolean(const AKey: string; const ADefault: Boolean = False): Boolean;
    Property AskMsg: TAiChatMessage read GetAskMsg write SetAskMsg;
    Property ResMsg: TAiChatMessage read GetResMsg write SetResMsg;
  end;

  TAiToolBase = class(TComponent)
  private
    FDescription: string;
    FID: String;
    procedure SetDescription(const Value: String);
    procedure SetID(const Value: String);
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string); virtual; abstract;
  public
    procedure Run(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
  published
    property Description: string read FDescription write SetDescription;
    Property ID: String read FID write SetID; // GUID que identifica cada nodo
  end;

  TAiAgentsToolSample = class(TAiToolBase)
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string); override;
  end;

  // --- Base Component ---
  TAIAgentsBase = class(TComponent)
  private
    FDescription: String;
    FID: String;
    procedure SetDescription(const Value: String);
    procedure SetID(const Value: String);
  Published
    Property Description: String read FDescription write SetDescription;
    Property ID: String read FID write SetID; // GUID que identifica cada nodo
  end;

  // --- Link (Edge) ---
  TAIAgentsLink = class(TAIAgentsBase)
  private
    FNextNo: TAIAgentsNode;
    FNextB: TAIAgentsNode;
    FNextC: TAIAgentsNode;
    FNextA: TAIAgentsNode;
    FNextD: TAIAgentsNode;
    FGraph: TAIAgentManager;
    FOnExecute: TAIAgentsLinkOnExecute;
    FNoCycles: Integer;
    FMaxCycles: Integer;
    FReady: Boolean;
    FSourceNode: TAIAgentsNode;
    FConditionalTargets: TDictionary<string, TAIAgentsNode>;
    FMode: TLinkMode;
    FConditionalKey: string;
    FManualTargetsKey: string;
    // --- NUEVO: Campos para el modo lmExpression ---
    FExpressionA: string;
    FExpressionB: string;
    FExpressionC: string;
    FExpressionD: string;
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
    procedure BuildManualTargets(const TargetsCSV: string; out Nodes: TList<TAIAgentsNode>);
    procedure CreateAndQueueTask(ANodeToExecute, ASourceNode: TAIAgentsNode; ACurrentLink: TAIAgentsLink);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Print(Value: String);
    procedure DoExecute(Sender: TAIAgentsNode); // --- MODIFICADO: Ahora es el ?nico m?todo de ejecuci?n
    procedure AddConditionalTarget(const AKey: string; ANode: TAIAgentsNode);
    property NoCycles: Integer read FNoCycles write FNoCycles;
  published
    property NextA: TAIAgentsNode read FNextA write SetNextA;
    property NextB: TAIAgentsNode read FNextB write SetNextB;
    property NextC: TAIAgentsNode read FNextC write SetNextC;
    property NextD: TAIAgentsNode read FNextD write SetNextD;
    property NextNo: TAIAgentsNode read FNextNo write SetNextNo;
    property Graph: TAIAgentManager read FGraph write SetGraph;
    property OnExecute: TAIAgentsLinkOnExecute read FOnExecute write SetOnExecute;
    property MaxCycles: Integer read FMaxCycles write SetMaxCycles default 1;
    property Mode: TLinkMode read FMode write SetMode default lmFanout;
    property ConditionalKey: string read FConditionalKey write FConditionalKey;
    property ManualTargetsKey: string read FManualTargetsKey write FManualTargetsKey;
    // --- NUEVO: Propiedades para el modo lmExpression ---
    property ExpressionA: string read FExpressionA write FExpressionA;
    property ExpressionB: string read FExpressionB write FExpressionB;
    property ExpressionC: string read FExpressionC write FExpressionC;
    property ExpressionD: string read FExpressionD write FExpressionD;
  end;

  // --- Node ---
  TAIAgentsNode = class(TAIAgentsBase)
  private
    FOutput: String;
    FInput: String;
    FNext: TAIAgentsLink;
    FGraph: TAIAgentManager;
    FInEdges: TList<TAIAgentsLink>;
    FOnExecute: TAIAgentsNodeOnExecute;
    FPromptName: String;
    FMsgError: String;
    FError: Boolean;
    FJoinLock: TCriticalSection;
    FJoinMode: TJoinMode;
    FTool: TAiToolBase;
    FJoinInputs: TDictionary<TAIAgentsLink, string>;
    // --- Suspensi?n human-in-the-loop ---
    FSuspended:      Boolean;
    FSuspendReason:  string;
    FSuspendContext: string;
    procedure SetInput(const Value: String);
    procedure SetNext(const Value: TAIAgentsLink);
    procedure SetOutput(const Value: String);
    procedure SetGraph(const Value: TAIAgentManager);
    procedure SetOnExecute(const Value: TAIAgentsNodeOnExecute);
    procedure SetPromptName(const Value: String);
    procedure SetJoinMode(const Value: TJoinMode);
    procedure SetTool(const Value: TAiToolBase);
    procedure SetError(const Value: Boolean);
    procedure SetMsgError(const Value: String);
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
    // Suspende este nodo. Llamar desde OnExecute o desde Execute de una Tool.
    // DoExecute verifica FSuspended al retornar y no enruta al siguiente Link.
    procedure Suspend(const AReason: string; const AContext: string = '');
    property Error:          Boolean read FError        Write SetError;
    property MsgError:       String  read FMsgError     write SetMsgError;
    property Suspended:      Boolean read FSuspended;
    property SuspendReason:  string  read FSuspendReason;
    property SuspendContext: string  read FSuspendContext;
  published
    property Input: String read FInput write SetInput;
    property Output: String read FOutput write SetOutput;
    property Next: TAIAgentsLink read FNext write SetNext;
    property Graph: TAIAgentManager read FGraph write SetGraph;
    property OnExecute: TAIAgentsNodeOnExecute read FOnExecute write SetOnExecute;
    property PromptName: String read FPromptName write SetPromptName;
    property JoinMode: TJoinMode read FJoinMode write SetJoinMode default jmAny;
    property Tool: TAiToolBase read FTool write SetTool;
  end;

  // --- Orchestrator ---

  TAIAgentManager = class(TComponent)
  private
    FEndNode: TAIAgentsNode;
    FStartNode: TAIAgentsNode;
    FOnPrint: TAIAgentsOnPrint;
    FOnPrintRef: TAgentPrintRef;
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
    FOnExitNode: TAIAgentsOnExitNode;
    FOnEnterNode: TAIAgentsOnEnterNode;
    FMaxConcurrentTasks: Integer;
    FTimeoutMs: Cardinal;
    FOnFinish: TAIAgentsOnFinish;
    FOnStart: TAIAgentsOnStart;
    FDescription: String;
    FAsynchronous: Boolean;
    // --- Ejecuci?n durable (checkpoint / suspend-resume) ---
    FCheckpointer:       IAiCheckpointer;
    FCurrentThreadID:    string;
    FCheckpointSeq:      Integer;  // protegido por FActiveTasksLock
    FSuspendedSteps:     TObjectList<TAiPendingStep>; // owned
    FSuspendedStepsLock: TCriticalSection;
    FOnSuspend:          TAIAgentsOnSuspend;
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
    procedure SetDescription(const Value: String);
    procedure SetAsynchronous(const Value: Boolean);
  protected
    // --- CORREGIDO: TThreadPool ---
    FThreadPool: TThreadPool;
    procedure DoPrint(Sender: TObject; Value: String);
    procedure DoPrintFromRef(Sender: TObject; Value: String);
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
    function InternalRun(Msg: String): ITask; Virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Abort;
    function FindNode(const AName: string): TAIAgentsNode;
    procedure DoError(Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception);
    function DoConfirm(Node: TAIAgentsNode; const AQuestion: string; Buttons: TMsgStates; var AResponse: string): TMsgState;

    procedure SetOnPrintEvent(const APrintProc: TAgentPrintRef);
    procedure ClearGraph;
    function AddNode(const AName: string; AExecuteProc: TAIAgentsNodeOnExecute): TAIAgentManager;
    function AddEdge(const AStartNodeName, AEndNodeName: string): TAIAgentManager;
    function AddConditionalEdge(const AStartNodeName: string; const AConditionalLinkName: string; AConditionalTargets: TDictionary<string, string>): TAIAgentManager;
    function SetEntryPoint(const ANodeName: string): TAIAgentManager;
    function SetFinishPoint(const ANodeName: string): TAIAgentManager;
    procedure Compile;
    procedure SaveToStream(AStream: TStream);
    Procedure LoadFromStream(AStream: TStream);
    procedure SaveStateToStream(AStream: TStream);
    procedure LoadStateFromStream(AStream: TStream);

    function Run(APrompt: String): String; overload; // Reemplaza al anterior

    function AddMessageAndRun(APrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): String;
    function AddMessageAndRunMsg(APrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage;

    function NewMessage(APrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage;

    property Busy: Boolean read FBusy;
    property Blackboard: TAIBlackboard read FBlackboard;
    property CurrentThreadID: string read FCurrentThreadID;
    // Reanuda un hilo suspendido. AInput es la respuesta/aprobaci?n del humano.
    // Devuelve True si el hilo fue encontrado y la reanudaci?n se inici?.
    function ResumeThread(const AThreadID, ANodeName, AInput: string): Boolean;
    // Lista thread IDs con checkpoints activos (suspendidos o en progreso)
    function GetActiveThreads: TArray<string>;
    property Checkpointer: IAiCheckpointer read FCheckpointer write SetCheckpointer;
  published
    property StartNode: TAIAgentsNode read FStartNode write SetStartNode;
    property EndNode: TAIAgentsNode read FEndNode write SetEndNode;
    property OnPrint: TAIAgentsOnPrint read FOnPrint write SetOnPrint;
    property OnEnd: TAIAgentsOnEnd read FOnEnd write SetOnEnd;
    property OnError: TAIAgentsOnError read FOnError write SetOnError;
    property OnConfirm: TAIAgentsOnConfirm read FOnConfirm write SetOnConfirm;
    property OnEnterNode: TAIAgentsOnEnterNode read FOnEnterNode write SetOnEnterNode;
    property OnExitNode: TAIAgentsOnExitNode read FOnExitNode write SetOnExitNode;
    property MaxConcurrentTasks: Integer read FMaxConcurrentTasks write SetMaxConcurrentTasks default 4;
    property OnStart: TAIAgentsOnStart read FOnStart write SetOnStart;
    property OnFinish: TAIAgentsOnFinish read FOnFinish write SetOnFinish;
    property TimeoutMs: Cardinal read FTimeoutMs write FTimeoutMs default 60000;
    Property Description: String read FDescription write SetDescription;
    property Asynchronous: Boolean read FAsynchronous write SetAsynchronous default True;
    property OnSuspend: TAIAgentsOnSuspend read FOnSuspend write SetOnSuspend;
  end;

  TAIAgents = Class(TAIAgentManager)
  End deprecated 'Use TAIAgentManager instead';

function EvalCondition(const Expr: string; Vars: TDictionary<string, TValue>): Boolean;

procedure Register;

implementation

uses uMakerAi.Agents.EngineRegistry;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAIAgentManager, TAIAgentsNode, TAIAgentsLink, TAiAgentsToolSample, TAIAgents]);
end;

function EvalCondition(const Expr: string; Vars: TDictionary<string, TValue>): Boolean;
var
  LScope: IScope; // Mantiene la referencia viva (Interface)
  LDictScope: TDictionaryScope; // Referencia de clase para acceder a .Map
  BindingExpression: TBindingExpression;
  Pair: TPair<string, TValue>;
  Value: TValue;
  ValueWrapper: IValue;
begin
  // 1. Crear la instancia de clase
  LDictScope := TDictionaryScope.Create;

  // 2. VITAL: Asignar a una variable de Interfaz inmediatamente.
  // Esto sube el RefCount a 1 y evita que se destruya prematuramente.
  LScope := LDictScope;

  // No necesitamos try..finally para el Scope, las interfaces se limpian solas.

  // 3. Llenar las variables usando la referencia de clase (LDictScope)
  for Pair in Vars do
  begin
    ValueWrapper := TValueWrapper.Create(Pair.Value);
    LDictScope.Map.Add(Pair.Key, ValueWrapper);
  end;

  // 4. Crear la expresi?n pasando la INTERFAZ (LScope)
  BindingExpression := TBindings.CreateExpression([LScope], Expr);
  try
    ValueWrapper := BindingExpression.Evaluate;
    Value := ValueWrapper.GetValue;

    if Value.IsType<Boolean> then
      Result := Value.AsBoolean
    else
      raise Exception.CreateFmt('La expresi?n "%s" no devolvi? un Boolean', [Expr]);
  finally
    BindingExpression.Free;
  end;
end;

function SerializeToolProperties(ATool: TAiToolBase): TJSONObject;
var
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LProp: TRttiProperty;
  LValue: TValue;
begin
  Result := TJSONObject.Create;
  if not Assigned(ATool) then
    Exit;

  LContext := TRttiContext.Create;
  try
    LRttiType := LContext.GetType(ATool.ClassType);
    for LProp in LRttiType.GetProperties do
    begin
      // Solo nos interesan las propiedades que se pueden leer y escribir
      if not(LProp.IsReadable and LProp.IsWritable and (LProp.Visibility = mvPublished)) then
        Continue;

      LValue := LProp.GetValue(ATool);
      if LValue.IsEmpty then
        Continue;

      // Convertir el TValue a un tipo JSON compatible
      case LValue.Kind of
        tkInteger, tkInt64:
          Result.AddPair(LProp.Name, TJSONNumber.Create(LValue.AsInt64));
        tkFloat:
          Result.AddPair(LProp.Name, TJSONNumber.Create(LValue.AsExtended));
        tkString, tkUString, tkLString, tkWString:
          Result.AddPair(LProp.Name, TJSONString.Create(LValue.AsString));
        tkEnumeration:
          if LProp.PropertyType.Handle = TypeInfo(Boolean) then
            Result.AddPair(LProp.Name, TJSONBool.Create(LValue.AsBoolean))
          else // Guardar otros enums por su nombre de texto
            Result.AddPair(LProp.Name, TJSONString.Create(GetEnumName(LProp.PropertyType.Handle, LValue.AsOrdinal)));
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure DeserializeToolProperties(ATool: TAiToolBase; APropertiesJSON: TJSONObject);
var
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LProp: TRttiProperty;
  LValue: TValue;
  LPair: TJSONPair;
  LJsonValue: TJSONValue;
  OrdValue: Integer;
begin
  if not Assigned(ATool) or not Assigned(APropertiesJSON) then
    Exit;

  LContext := TRttiContext.Create;
  try
    LRttiType := LContext.GetType(ATool.ClassType);
    for LPair in APropertiesJSON do
    begin
      LProp := LRttiType.GetProperty(LPair.JsonString.Value);
      if Assigned(LProp) and LProp.IsWritable then
      begin
        LJsonValue := LPair.JsonValue;
        LValue := TValue.Empty; // Inicializar

        case LProp.PropertyType.TypeKind of
          tkInteger:
            LValue := TValue.From<Integer>((LJsonValue as TJSONNumber).AsInt);
          tkInt64:
            LValue := TValue.From<Int64>((LJsonValue as TJSONNumber).AsInt64);
          tkFloat:
            LValue := TValue.From<Double>((LJsonValue as TJSONNumber).AsDouble);
          tkString, tkUString, tkLString, tkWString:
            LValue := TValue.From<string>((LJsonValue as TJSONString).Value);
          tkEnumeration:
            if LProp.PropertyType.Handle = TypeInfo(Boolean) then
              LValue := TValue.From<Boolean>((LJsonValue as TJSONBool).AsBoolean)
            else
            begin
              OrdValue := GetEnumValue(LProp.PropertyType.Handle, (LJsonValue as TJSONString).Value);
              if OrdValue >= 0 then
                LValue := TValue.FromOrdinal(LProp.PropertyType.Handle, OrdValue);
            end;
        end;

        if not LValue.IsEmpty then
          LProp.SetValue(ATool, LValue);
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure SerializeBlackboard(ABlackboard: TAIBlackboard; AJSONObject: TJSONObject);
var
  LPair: TPair<string, TValue>;
begin
  // ADVERTENCIA: Esto solo funcionar? para tipos de TValue simples.
  // No se pueden serializar objetos complejos, punteros o records.
  ABlackboard.FLock.Enter;
  try
    for LPair in ABlackboard.FData do
    begin
      var
      LValue := LPair.Value;
      case LValue.Kind of
        tkInteger, tkInt64:
          AJSONObject.AddPair(LPair.Key, TJSONNumber.Create(LValue.AsInt64));
        tkFloat:
          AJSONObject.AddPair(LPair.Key, TJSONNumber.Create(LValue.AsExtended));
        tkString, tkUString:
          AJSONObject.AddPair(LPair.Key, TJSONString.Create(LValue.AsString));
        tkEnumeration:
          if LValue.TypeInfo = System.TypeInfo(Boolean) then
            AJSONObject.AddPair(LPair.Key, TJSONBool.Create(LValue.AsBoolean));
      end;
    end;
  finally
    ABlackboard.FLock.Leave;
  end;
end;

procedure DeserializeBlackboard(AJSONObject: TJSONObject; ABlackboard: TAIBlackboard);
var
  LPair: TJSONPair;
begin
  ABlackboard.Clear;
  for LPair in AJSONObject do
  begin
    var
    LJsonValue := LPair.JsonValue;
    if LJsonValue is TJSONString then
      ABlackboard.SetString(LPair.JsonString.Value, LJsonValue.Value)
    else if LJsonValue is TJSONNumber then
      // Simplificaci?n: lo guardamos como float, se puede refinar
      ABlackboard.SetValue(LPair.JsonString.Value, StrToFloat(LJsonValue.Value))
    else if LJsonValue is TJSONBool then
      ABlackboard.SetBoolean(LPair.JsonString.Value, (LJsonValue as TJSONBool).AsBoolean);
  end;
end;

{ TAIBlackboard }

procedure TAIBlackboard.Clear;
var
  LValue: TValue;
  LMsg: TAiChatMessage;
begin
  FLock.Enter;
  try
    // 1. Buscar y Liberar AskMsg
    // Usamos las claves internas que definimos en los Getters/Setters ('Sys.AskMsg')
    if FData.TryGetValue('Sys.AskMsg', LValue) then
    begin
      LMsg := TAiChatMessage(LValue.AsObject);

      // Verificamos si es un objeto v?lido antes de liberar
      if LValue.IsObject and Assigned(LMsg) then
        LMsg.Free;
    end;

    // 2. Buscar y Liberar ResMsg
    if FData.TryGetValue('Sys.ResMsg', LValue) then
    begin
      LMsg := TAiChatMessage(LValue.AsObject);

      if LValue.IsObject and Assigned(LMsg) then
        LMsg.Free;
    end;

    // 3. Limpiar el diccionario (elimina las claves y punteros)
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
  Clear;
  FData.Free;
  FLock.Free;
  inherited;
end;

function TAIBlackboard.GetAskMsg: TAiChatMessage;
var
  Val: TValue;
begin
  // Usamos TryGetValue que ya es Thread-Safe (tiene su propio FLock)
  // 'Sys.AskMsg' es la clave interna donde guardaremos el objeto
  if TryGetValue('Sys.AskMsg', Val) and (not Val.IsEmpty) then
    Result := Val.AsType<TAiChatMessage>
  else
    Result := nil;
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

function TAIBlackboard.GetResMsg: TAiChatMessage;
var
  Val: TValue;
begin
  if TryGetValue('Sys.ResMsg', Val) and (not Val.IsEmpty) then
    Result := Val.AsType<TAiChatMessage>
  else
    Result := nil;
end;

function TAIBlackboard.GetStatus: TAgentExecutionStatus;
var
  Val: TValue;
begin
  // Recuperamos el Enum, o devolvemos esUnknown si no existe
  if TryGetValue('Execution.Status', Val) then
    Result := Val.AsType<TAgentExecutionStatus>
  else
    Result := esUnknown;
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

procedure TAIBlackboard.SetAskMsg(const Value: TAiChatMessage);
begin
  // SetValue tambi?n es Thread-Safe
  SetValue('Sys.AskMsg', TValue.From(Value));
end;

procedure TAIBlackboard.SetBoolean(const AKey: string; AValue: Boolean);
begin
  SetValue(AKey, AValue);
end;

procedure TAIBlackboard.SetInteger(const AKey: string; AValue: Integer);
begin
  SetValue(AKey, AValue);
end;

procedure TAIBlackboard.SetResMsg(const Value: TAiChatMessage);
begin
  SetValue('Sys.ResMsg', TValue.From(Value));
end;

procedure TAIBlackboard.SetStatus(Value: TAgentExecutionStatus);
begin
  // Guardamos el Enum dentro del TValue
  SetValue('Execution.Status', TValue.From(Value));
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

procedure TAIAgentManager.Abort;
begin
  FAbort := True;
end;

function TAIAgentManager.AddEdge(const AStartNodeName, AEndNodeName: string): TAIAgentManager;
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
  Link.Mode := lmFanout;
  StartNode.Next := Link;
  Link.NextA := EndNode;
  Result := Self;
end;

function TAIAgentManager.AddMessageAndRun(APrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): String;
begin
  // Configuramos el mensaje en el Blackboard
  Blackboard.AskMsg := NewMessage(APrompt, aRole, aMediaFiles);

  // Llamamos a Run, que manejar? la l?gica Sync/Async
  Result := Run(APrompt);
end;

function TAIAgentManager.AddMessageAndRunMsg(APrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage;
begin
  if FAsynchronous then
    raise Exception.Create('AddMessageAndRunMsg is only compatible with Asynchronous = False (Synchronous mode).');

  // Ejecutamos (esto esperar? a terminar)
  AddMessageAndRun(APrompt, aRole, aMediaFiles);

  // Retornamos el objeto completo del Blackboard
  Result := Blackboard.ResMsg;
end;

function TAIAgentManager.AddNode(const AName: string; AExecuteProc: TAIAgentsNodeOnExecute): TAIAgentManager;
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

procedure TAIAgentManager.AddComponentToList(AComponent: TAIAgentsBase);
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

function TAIAgentManager.AddConditionalEdge(const AStartNodeName, AConditionalLinkName: string; AConditionalTargets: TDictionary<string, string>): TAIAgentManager;
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
  Link.Mode := lmConditional;

  for DecisionKey in AConditionalTargets.Keys do
  begin
    TargetNodeName := AConditionalTargets.Items[DecisionKey];
    TargetNode := FindNode(TargetNodeName);
    if not Assigned(TargetNode) then
      raise Exception.CreateFmt('Conditional edge creation failed: Target node "%s" for decision "%s" not found.', [TargetNodeName, DecisionKey]);
    Link.AddConditionalTarget(DecisionKey, TargetNode);
  end;

  StartNode.Next := Link;
  Result := Self;
end;

procedure TAIAgentManager.ClearGraph;
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

procedure TAIAgentManager.Compile;
var
  Node: TAIAgentsNode;
  Link: TAIAgentsLink;
  TargetNode: TAIAgentsNode;
  StartCount, EndCount: Integer;
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

    StartCount := 0;
    EndCount := 0;
    for Node in FNodes do
    begin
      if Node = FStartNode then
        Inc(StartCount);
      if Node = FEndNode then
        Inc(EndCount);
      Node.Reset;
    end;
    if StartCount <> 1 then
      raise Exception.CreateFmt('Invalid number of StartNode references: %d (expected 1).', [StartCount]);
    if EndCount <> 1 then
      raise Exception.CreateFmt('Invalid number of EndNode references: %d (expected 1).', [EndCount]);

    for Link in FLinks do
    begin
      Link.Ready := False;
      Link.NoCycles := 0;
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

constructor TAIAgentManager.Create(aOwner: TComponent);
begin
  inherited;
  FBlackboard := TAIBlackboard.Create;
  FNodes := TList<TAIAgentsNode>.Create;
  FLinks := TList<TAIAgentsLink>.Create;
  FActiveTasks := TList<ITask>.Create;
  FActiveTasksLock := TCriticalSection.Create;
  FCompiled := False;
  FAsynchronous := True; // Default VCL behavior
  FBusy := False;

  // --- NUEVO: Inicializaci?n del Scheduler ---
  FMaxConcurrentTasks := 4;
  FTimeoutMs := 60000;
  FThreadPool := TThreadPool.Create;
  FThreadPool.SetMaxWorkerThreads(FMaxConcurrentTasks);

  // --- Ejecuci?n durable ---
  FSuspendedSteps     := TObjectList<TAiPendingStep>.Create(True);
  FSuspendedStepsLock := TCriticalSection.Create;
  FCheckpointer       := nil;
  FCurrentThreadID    := '';
  FCheckpointSeq      := 0;
end;

destructor TAIAgentManager.Destroy;
begin
  FSuspendedSteps.Free;
  FSuspendedStepsLock.Free;
  FNodes.Free;
  FLinks.Free;
  FActiveTasks.Free;
  FActiveTasksLock.Free;
  FThreadPool.Free; // --- NUEVO: Liberaci?n del Scheduler ---
  FBlackboard.Free;
  inherited;
end;

procedure TAIAgentManager.DoError(Node: TAIAgentsNode; Link: TAIAgentsLink; E: Exception);
var
  LAbort: Boolean;
begin
  // CAMBIO: Usar SetStatus con Enum
  Blackboard.SetStatus(esError);
  Blackboard.SetString('Execution.ErrorMessage', E.Message);

  LAbort := True;
  if Assigned(FOnError) then
  begin
    FOnError(Self, Node, Link, E, LAbort);
  end;

  if LAbort then
    Abort;
end;

function TAIAgentManager.DoConfirm(Node: TAIAgentsNode; const AQuestion: string; Buttons: TMsgStates; var AResponse: string): TMsgState;
begin
  Result := msCancel;
  if Assigned(FOnConfirm) then
    FOnConfirm(Self, Node, AQuestion, Buttons, AResponse, Result)
  else
  begin
    DoError(Node, nil, Exception.Create('User confirmation required, but OnConfirm event is not assigned.'));
    Result := msAbort;
  end;
end;

procedure TAIAgentManager.DoPrint(Sender: TObject; Value: String);
begin
  if Assigned(FOnPrint) then
    FOnPrint(Sender, Value);
end;

procedure TAIAgentManager.DoPrintFromRef(Sender: TObject; Value: String);
begin
  if Assigned(FOnPrintRef) then
    FOnPrintRef(Sender, Value);
end;

function TAIAgentManager.FindNode(const AName: string): TAIAgentsNode;
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

// Aseg?rate de tener estas unidades en la cl?usula 'uses' de la implementation:
// System.JSON, System.JSON.Types, System.Rtti, System.TypInfo, uEngineRegistry

// ... (El helper DeserializeToolProperties y la clase TAgentHandlerRegistry se mantienen como antes) ...

// -----------------------------------------------------------------------------
// IMPLEMENTACI?N FINAL DE TAIAgentManager.LoadFromStream
// -----------------------------------------------------------------------------
procedure TAIAgentManager.LoadFromStream(AStream: TStream);
var
  LRoot, LGraphJSON, LNodeJSON, LLinkJSON, LTargetsJSON: TJSONObject;
  LNodesArray, LLinksArray: TJSONArray;
  LReader: TStreamReader;
  LJsonValue: TJSONValue;
  LNode: TAIAgentsNode;
  LLink: TAIAgentsLink;
  LNodeMap: TDictionary<string, TAIAgentsNode>;
  LLinkMap: TDictionary<string, TAIAgentsLink>;
  LKey: string;
  LToolClass: TClass; // Variable para almacenar el tipo de clase de la herramienta
begin
  if not Assigned(AStream) or (AStream.Size = 0) then
    Exit;

  ClearGraph;

  LNodeMap := TDictionary<string, TAIAgentsNode>.Create;
  LLinkMap := TDictionary<string, TAIAgentsLink>.Create;
  LReader := TStreamReader.Create(AStream, TEncoding.UTF8);
  try
    LJsonValue := TJSONObject.ParseJSONValue(LReader.ReadToEnd);
    if not Assigned(LJsonValue) or not(LJsonValue is TJSONObject) then
      raise Exception.Create('Invalid JSON format: root is not an object.');
    LRoot := LJsonValue as TJSONObject;
    try
      // =======================================================================
      // PASADA 1: CREAR TODAS LAS INSTANCIAS DE NODOS Y ENLACES
      // =======================================================================

      // --- 1.A: Crear Nodos ---
      LNodesArray := LRoot.GetValue('nodes') as TJSONArray;
      if Assigned(LNodesArray) then
      begin
        for LJsonValue in LNodesArray do
        begin
          LNodeJSON := LJsonValue as TJSONObject;
          LKey := LNodeJSON.GetValue<string>('name', '');
          if LKey = '' then
            raise Exception.Create('Node found without a name in JSON.');
          if LNodeMap.ContainsKey(LKey) then
            raise Exception.CreateFmt('Duplicate node name found in JSON: "%s"', [LKey]);

          LNode := TAIAgentsNode.Create(Self);
          LNode.Name := LKey;
          LNode.Description := LNodeJSON.GetValue<string>('description', '');
          LNode.PromptName := LNodeJSON.GetValue<string>('promptName', '');

          var
          LJoinModeStr := LNodeJSON.GetValue<string>('joinMode', 'jmAny');
          LNode.JoinMode := TJoinMode(GetEnumValue(TypeInfo(TJoinMode), LJoinModeStr));

          // --- SECCI?N CORREGIDA ---
          var
          LToolValue := LNodeJSON.GetValue('tool');
          if Assigned(LToolValue) and (LToolValue is TJSONObject) then
          begin
            var
            LToolDataObj := LToolValue as TJSONObject;
            var
            LToolClassName := LToolDataObj.GetValue<string>('className');
            if LToolClassName <> '' then
            begin
              // 1. Buscar el TIPO de clase en TU registro
              LToolClass := TEngineRegistry.Instance.FindToolClass(LToolClassName);

              // 2. Si se encontr?, crear una instancia de esa clase
              if Assigned(LToolClass) and LToolClass.InheritsFrom(TAiToolBase) then
              begin
                // Creamos la instancia usando el NODO como propietario
                LNode.Tool := TAiToolBase(LToolClass).Create(LNode);

                // 3. Deserializar las propiedades en la nueva instancia
                var
                LPropertiesJSON := LToolDataObj.GetValue('properties') as TJSONObject;
                if Assigned(LPropertiesJSON) then
                  DeserializeToolProperties(LNode.Tool, LPropertiesJSON);
              end;
            end;
          end;
          // --- FIN DE LA CORRECCI?N ---

          LNodeMap.Add(LKey, LNode);
        end;
      end;

      // --- 1.B: Crear Enlaces --- (Sin cambios)
      LLinksArray := LRoot.GetValue('links') as TJSONArray;
      if Assigned(LLinksArray) then
      begin
        for LJsonValue in LLinksArray do
        begin
          LLinkJSON := LJsonValue as TJSONObject;
          LKey := LLinkJSON.GetValue<string>('name', '');
          if LKey = '' then
            raise Exception.Create('Link found without a name in JSON.');
          if LLinkMap.ContainsKey(LKey) then
            raise Exception.CreateFmt('Duplicate link name found in JSON: "%s"', [LKey]);

          LLink := TAIAgentsLink.Create(Self);
          LLink.Name := LKey;
          LLink.Description := LLinkJSON.GetValue<string>('description', '');
          LLink.MaxCycles := LLinkJSON.GetValue<Integer>('maxCycles', 1);

          var
          LLinkModeStr := LLinkJSON.GetValue<string>('mode', 'lmFanout');
          LLink.Mode := TLinkMode(GetEnumValue(TypeInfo(TLinkMode), LLinkModeStr));

          if LLink.Mode = lmConditional then
            LLink.ConditionalKey := LLinkJSON.GetValue<string>('conditionalKey', 'next_route');

          if LLink.Mode = lmExpression then
          begin
            var
            LExpressionsJSON := LLinkJSON.GetValue('expressions') as TJSONObject;
            if Assigned(LExpressionsJSON) then
            begin
              LLink.ExpressionA := LExpressionsJSON.GetValue<string>('expressionA', '');
              LLink.ExpressionB := LExpressionsJSON.GetValue<string>('expressionB', '');
              LLink.ExpressionC := LExpressionsJSON.GetValue<string>('expressionC', '');
              LLink.ExpressionD := LExpressionsJSON.GetValue<string>('expressionD', '');
            end;
          end;

          LLinkMap.Add(LKey, LLink);
        end;
      end;

      // =======================================================================
      // PASADA 2: CONECTAR TODO (Sin cambios)
      // =======================================================================

      if Assigned(LNodesArray) then
      begin
        for LJsonValue in LNodesArray do
        begin
          LNodeJSON := LJsonValue as TJSONObject;
          LNode := LNodeMap[LNodeJSON.GetValue<string>('name')];
          var
          LNextLinkName := LNodeJSON.GetValue<string>('nextLink', '');
          if (LNextLinkName <> '') and LLinkMap.TryGetValue(LNextLinkName, LLink) then
            LNode.Next := LLink;
          var
          LHandlerName := LNodeJSON.GetValue<string>('onExecuteHandler', '');
          if LHandlerName <> '' then
            LNode.OnExecute := TAgentHandlerRegistry.Instance.FindNodeHandler(LHandlerName);
        end;
      end;

      if Assigned(LLinksArray) then
      begin
        for LJsonValue in LLinksArray do
        begin
          LLinkJSON := LJsonValue as TJSONObject;
          LLink := LLinkMap[LLinkJSON.GetValue<string>('name')];
          var
          LSourceNodeName := LLinkJSON.GetValue<string>('sourceNode', '');
          if (LSourceNodeName <> '') and LNodeMap.TryGetValue(LSourceNodeName, LNode) then
            LLink.FSourceNode := LNode;
          LTargetsJSON := LLinkJSON.GetValue('targets') as TJSONObject;
          if Assigned(LTargetsJSON) then
          begin
            if LNodeMap.TryGetValue(LTargetsJSON.GetValue<string>('nextA', ''), LNode) then
              LLink.NextA := LNode;
            if LNodeMap.TryGetValue(LTargetsJSON.GetValue<string>('nextB', ''), LNode) then
              LLink.NextB := LNode;
            if LNodeMap.TryGetValue(LTargetsJSON.GetValue<string>('nextC', ''), LNode) then
              LLink.NextC := LNode;
            if LNodeMap.TryGetValue(LTargetsJSON.GetValue<string>('nextD', ''), LNode) then
              LLink.NextD := LNode;
            if LNodeMap.TryGetValue(LTargetsJSON.GetValue<string>('nextNo', ''), LNode) then
              LLink.NextNo := LNode;
          end;
          var
          LCondTargetsJSON := LLinkJSON.GetValue('conditionalTargets') as TJSONObject;
          if Assigned(LCondTargetsJSON) then
          begin
            for var LPair in LCondTargetsJSON do
            begin
              var
              LTargetNodeName := LPair.JsonValue.Value;
              if LNodeMap.TryGetValue(LTargetNodeName, LNode) then
                LLink.AddConditionalTarget(LPair.JsonString.Value, LNode);
            end;
          end;
        end;
      end;

      // =======================================================================
      // PASO FINAL: CONFIGURAR GRAFO (Sin cambios)
      // =======================================================================
      LGraphJSON := LRoot.GetValue('graph') as TJSONObject;
      if Assigned(LGraphJSON) then
      begin
        Self.Description := LGraphJSON.GetValue<string>('description', '');
        Self.MaxConcurrentTasks := LGraphJSON.GetValue<Integer>('maxConcurrentTasks', 4);
        Self.TimeoutMs := LGraphJSON.GetValue<Cardinal>('timeoutMs', 60000);
        var
        LStartNodeName := LGraphJSON.GetValue<string>('startNode', '');
        if (LStartNodeName <> '') and LNodeMap.TryGetValue(LStartNodeName, LNode) then
          Self.StartNode := LNode;
        var
        LEndNodeName := LGraphJSON.GetValue<string>('endNode', '');
        if (LEndNodeName <> '') and LNodeMap.TryGetValue(LEndNodeName, LNode) then
          Self.EndNode := LNode;
      end;
      FCompiled := False;
    finally
      LRoot.Free;
    end;
  finally
    LReader.Free;
    LLinkMap.Free;
    LNodeMap.Free;
  end;
end;

procedure TAIAgentManager.LoadStateFromStream(AStream: TStream);
var
  LRoot, LStateObj, LBlackboardObj, LNodeStatesObj, LLinkStatesObj, LNodeStateObj, LJoinInputsObj: TJSONObject;
  LReader: TStreamReader;
  LJsonValue: TJSONValue;
  LNode: TAIAgentsNode;
  LLink: TAIAgentsLink;
  LPair: TJSONPair;
  LLinkMap: TDictionary<string, TAIAgentsLink>; // Mapa temporal para buscar links por nombre
begin
  if ComponentCount = 0 then
    raise Exception.Create('Cannot load state into an empty graph. Load the graph structure first.');

  // Crear mapa de links para una b?squeda r?pida
  LLinkMap := TDictionary<string, TAIAgentsLink>.Create;
  for var i := 0 to ComponentCount - 1 do
    if Components[i] is TAIAgentsLink then
      LLinkMap.Add(Components[i].Name, TAIAgentsLink(Components[i]));

  LReader := TStreamReader.Create(AStream, TEncoding.UTF8);
  try
    LJsonValue := TJSONObject.ParseJSONValue(LReader.ReadToEnd);
    LRoot := LJsonValue as TJSONObject;
    try
      LStateObj := LRoot.GetValue('executionState') as TJSONObject;
      if not Assigned(LStateObj) then
        Exit;

      // 1. Restaurar el Blackboard
      LBlackboardObj := LStateObj.GetValue('blackboard') as TJSONObject;
      if Assigned(LBlackboardObj) then
        DeserializeBlackboard(LBlackboardObj, Self.Blackboard);

      // 2. Restaurar el estado de los nodos
      LNodeStatesObj := LStateObj.GetValue('nodeStates') as TJSONObject;
      if Assigned(LNodeStatesObj) then
      begin
        for LPair in LNodeStatesObj do
        begin
          LNode := FindNode(LPair.JsonString.Value);
          if Assigned(LNode) then
          begin
            LNodeStateObj := LPair.JsonValue as TJSONObject;
            LNode.Input := LNodeStateObj.GetValue<string>('input', '');
            LNode.Output := LNodeStateObj.GetValue<string>('output', '');

            // Restaurar entradas de Join
            LNode.FJoinInputs.Clear;
            LJoinInputsObj := LNodeStateObj.GetValue('joinInputs') as TJSONObject;
            if Assigned(LJoinInputsObj) then
            begin
              for var LJoinPair in LJoinInputsObj do
              begin
                if LLinkMap.TryGetValue(LJoinPair.JsonString.Value, LLink) then
                  LNode.FJoinInputs.Add(LLink, LJoinPair.JsonValue.Value);
              end;
            end;
          end;
        end;
      end;

      // 3. Restaurar el estado de los enlaces
      LLinkStatesObj := LStateObj.GetValue('linkStates') as TJSONObject;
      if Assigned(LLinkStatesObj) then
      begin
        for LPair in LLinkStatesObj do
        begin
          if LLinkMap.TryGetValue(LPair.JsonString.Value, LLink) then
          begin
            var
            LLinkStateObj := LPair.JsonValue as TJSONObject;
            LLink.FNoCycles := LLinkStateObj.GetValue<Integer>('noCycles', 0);
          end;
        end;
      end;

    finally
      LRoot.Free;
    end;
  finally
    LReader.Free;
    LLinkMap.Free;
  end;
end;

function TAIAgentManager.NewMessage(APrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage;
var
  MF: TAiMediaFile;
begin
  // Crea el mensaje
  Result := TAiChatMessage.Create(APrompt, aRole);
  try
    if Length(aMediaFiles) > 0 then
    begin
      for MF in aMediaFiles do
      begin
        if Assigned(MF) then
          Result.AddMediaFile(MF);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TAIAgentManager.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TAIAgentManager.RemoveComponentFromList(AComponent: TAIAgentsBase);
begin
  if AComponent is TAIAgentsNode then
    FNodes.Remove(TAIAgentsNode(AComponent))
  else if AComponent is TAIAgentsLink then
    FLinks.Remove(TAIAgentsLink(AComponent));
end;

function TAIAgentManager.Run(APrompt: String): String;
var
  LTask: ITask;
  LStatus: TAgentExecutionStatus; // Variable para el nuevo enum
begin
  Result := '';

  // 1. CHEQUEO DE BUSY (Thread safe check simple)
  if FBusy then
    raise Exception.Create('The Agent Manager is currently busy.');

  // 2. GESTI?N DE MENSAJES
  // Si no se provey? un mensaje previo, creamos uno nuevo b?sico
  if Not Assigned(Blackboard.AskMsg) then
    Blackboard.AskMsg := NewMessage(APrompt, 'user', []);

  // Siempre reiniciamos el mensaje de respuesta para esta ejecuci?n
  Blackboard.ResMsg := TAiChatMessage.Create('', 'assistant');

  // 3. EJECUCI?N
  // Llama a la versi?n corregida de InternalRun que usa el bucle din?mico
  LTask := InternalRun(APrompt);

  // 4. L?GICA S?NCRONA / AS?NCRONA
  if FAsynchronous then
  begin
    // MODO AS?NCRONO (Default): Retornamos vac?o inmediatamente.
    // El resultado llegar? v?a eventos (OnFinish).
    Result := '';
  end
  else
  begin
    // MODO S?NCRONO (Servicios REST): Esperamos.
    if Assigned(LTask) then
    begin
      try
        LTask.Wait(INFINITE); // Esperamos a que InternalRun termine
      except
        on E: Exception do
          raise Exception.Create('Error waiting for agent execution: ' + E.Message);
      end;

      // --- CAMBIO PRINCIPAL AQU? ---
      // Recuperamos el estado como Enum en lugar de String.
      // Esto asume que implementaste el helper GetStatus en TAIBlackboard.
      LStatus := Blackboard.GetStatus;

      if LStatus = esError then
        raise Exception.Create('Execution Failed: ' + Blackboard.GetString('Execution.ErrorMessage'));

      if LStatus = esTimeout then
        raise Exception.Create('Execution Timed Out');

      // Opcional: Manejar esAborted expl?citamente si lo deseas
      if LStatus = esAborted then
        raise Exception.Create('Execution Aborted');

      // -----------------------------

      // Retornamos el contenido generado
      if Assigned(Blackboard.ResMsg) and (Blackboard.ResMsg.Content <> '') then
        Result := Blackboard.ResMsg.Content
      else if Assigned(FEndNode) then
        Result := FEndNode.Output;
    end;
  end;
end;

function TAIAgentManager.InternalRun(Msg: String): ITask;
var
  InitialInput: String;
begin
  // 1. Verificaci?n de estado ocupado
  if TInterlocked.Exchange(FBusy, True) then
    raise Exception.Create('Agent is busy (InternalRun check).');

  Compile; // Asegura que el grafo est? listo

  FBusy := True;
  FAbort := False;

  // --- Generar ThreadID ?nico para esta ejecuci?n ---
  var LNewGUID: TGUID;
  CreateGUID(LNewGUID);
  FCurrentThreadID := GUIDToString(LNewGUID);
  FCheckpointSeq   := 0;
  FSuspendedStepsLock.Enter;
  try
    FSuspendedSteps.Clear;
  finally
    FSuspendedStepsLock.Leave;
  end;

  // Establecemos estado inicial en el Blackboard
  Blackboard.SetStatus(esRunning);

  // 2. Limpieza de tareas previas
  FActiveTasksLock.Enter;
  try
    FActiveTasks.Clear;
  finally
    FActiveTasksLock.Leave;
  end;

  InitialInput := Msg;

  // 3. Crear la tarea principal del orquestador
  Result := TTask.Run(
    procedure
    var
      TasksToWaitOn: TArray<ITask>;
      WaitResult: Boolean;
      // CAMBIO: Variable de tipo Enum
      FinalStatus: TAgentExecutionStatus;
      FinalException: Exception;
      FinalOutput: String;
      HasPendingTasks: Boolean;
      CurrentTask: ITask;
    begin
      FinalException := nil;
      FinalStatus := esUnknown; // Valor inicial seguro

      try
        try
          // EVENTO ONSTART (Llamada directa, sin SafeSync)
          if Assigned(FOnStart) then
            FOnStart(Self, InitialInput);

          if FAbort then
          begin
            FinalStatus := esAborted;
            Exit;
          end;

          // EJECUCI?N DEL NODO INICIAL
          if Assigned(FStartNode) then
          begin
            FStartNode.Input := InitialInput;
            FStartNode.DoExecute(nil, nil);
          end;

          // --- INICIO DEL BUCLE DE ESPERA DIN?MICA ---
          repeat
            // A. Obtener instant?nea de las tareas actuales
            FActiveTasksLock.Enter;
            try
              TasksToWaitOn := FActiveTasks.ToArray;
            finally
              FActiveTasksLock.Leave;
            end;

            // B. Verificar si hay alguna tarea activa
            HasPendingTasks := False;
            for CurrentTask in TasksToWaitOn do
            begin
              if (CurrentTask.Status <> TTaskStatus.Completed) and (CurrentTask.Status <> TTaskStatus.Canceled) and (CurrentTask.Status <> TTaskStatus.Exception) then
              begin
                HasPendingTasks := True;
                Break;
              end;
            end;

            // C. Si no hay nada pendiente, salimos del bucle
            if not HasPendingTasks then
              Break;

            // D. Esperar por el lote actual de tareas.
            WaitResult := TTask.WaitForAll(TasksToWaitOn, FTimeoutMs);

            if not WaitResult then
              raise Exception.CreateFmt('Graph execution timed out after %d ms.', [FTimeoutMs]);

          until FAbort;
          // --- FIN DEL BUCLE DE ESPERA DIN?MICA ---

          // Definir estado final exitoso si no se abort?
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
            Abort; // Detener cualquier nueva ejecuci?n
            FinalException := E;

            if E.Message.Contains('timed out') then
              FinalStatus := esTimeout
            else
              FinalStatus := esError;

            // Llamada directa a DoError (sin SafeSync)
            DoError(nil, nil, E);
          end;
        end;
      finally
        // Asegurar que tenemos un estado v?lido antes de salir
        if FinalStatus = esUnknown then
          FinalStatus := esAborted;

        // Guardar el estado en el Blackboard usando el m?todo Helper del Enum
        Blackboard.SetStatus(FinalStatus);

        // Eliminar checkpoint solo si complet? exitosamente
        if (FinalStatus = esCompleted) and Assigned(FCheckpointer) then
          FCheckpointer.DeleteCheckpoint(FCurrentThreadID);

        // OBTENER RESULTADO FINAL
        FinalOutput := '';
        if Assigned(FEndNode) then
          FinalOutput := FEndNode.Output;

        // EVENTO ONFINISH
        // Pasamos el Enum (FinalStatus) en lugar del string
        if Assigned(FOnFinish) then
          FOnFinish(Self, InitialInput, FinalOutput, FinalStatus, FinalException);

        // Liberar el flag de ocupado
        TInterlocked.Exchange(FBusy, False);
      end;
    end);
end;

// --- NUEVO: Setter para MaxConcurrentTasks ---
procedure TAIAgentManager.SetMaxConcurrentTasks(const Value: Integer);
var
  LNewValue: Integer;
begin
  LNewValue := Value;
  if LNewValue < 1 then
    LNewValue := 1;

  if FMaxConcurrentTasks <> LNewValue then
  begin
    FMaxConcurrentTasks := LNewValue;
    // --- CORREGIDO: TThreadPool ---
    // En lugar de recrear, simplemente ajustamos el pool existente.
    // Esto es m?s seguro si hay tareas en ejecuci?n.
    if Assigned(FThreadPool) then
      FThreadPool.SetMaxWorkerThreads(FMaxConcurrentTasks);
  end;
end;

procedure TAIAgentManager.SaveStateToStream(AStream: TStream);
var
  LRoot, LStateObj, LBlackboardObj, LNodeStatesObj, LLinkStatesObj, LNodeStateObj, LJoinInputsObj: TJSONObject;
  LNode: TAIAgentsNode;
  LLink: TAIAgentsLink;
  LWriter: TStreamWriter;
  LPair: TPair<TAIAgentsLink, string>;
begin
  LRoot := TJSONObject.Create;
  try
    LStateObj := TJSONObject.Create;
    LRoot.AddPair('executionState', LStateObj);

    // 1. Guardar el Blackboard
    LBlackboardObj := TJSONObject.Create;
    SerializeBlackboard(Self.Blackboard, LBlackboardObj);
    LStateObj.AddPair('blackboard', LBlackboardObj);

    // 2. Guardar el estado de cada nodo
    LNodeStatesObj := TJSONObject.Create;
    for LNode in FNodes do
    begin
      LNodeStateObj := TJSONObject.Create;
      LNodeStateObj.AddPair('input', LNode.Input);
      LNodeStateObj.AddPair('output', LNode.Output);

      // Guardar las entradas pendientes en nodos de tipo Join
      LJoinInputsObj := TJSONObject.Create;
      if LNode.FJoinInputs.Count > 0 then
      begin
        for LPair in LNode.FJoinInputs do
        begin
          LJoinInputsObj.AddPair(LPair.Key.Name, LPair.Value);
        end;
      end;
      LNodeStateObj.AddPair('joinInputs', LJoinInputsObj);

      LNodeStatesObj.AddPair(LNode.Name, LNodeStateObj);
    end;
    LStateObj.AddPair('nodeStates', LNodeStatesObj);

    // 3. Guardar el estado de cada enlace
    LLinkStatesObj := TJSONObject.Create;
    for LLink in FLinks do
    begin
      var
      LLinkStateObj := TJSONObject.Create;
      LLinkStateObj.AddPair('noCycles', LLink.NoCycles);
      LLinkStatesObj.AddPair(LLink.Name, LLinkStateObj);
    end;
    LStateObj.AddPair('linkStates', LLinkStatesObj);

    // Escribir al Stream
    LWriter := TStreamWriter.Create(AStream, TEncoding.UTF8);
    try
      LWriter.Write(LRoot.ToString);
    finally
      LWriter.Free;
    end;

  finally
    LRoot.Free;
  end;
end;

procedure TAIAgentManager.SaveToStream(AStream: TStream);
var
  LRoot, LGraphObj, LNodeObj, LLinkObj, LTargetsObj, LExpressionsObj, LCondTargetsObj: TJSONObject;
  LNodesArray, LLinksArray: TJSONArray;
  LNode: TAIAgentsNode;
  LLink: TAIAgentsLink;
  LWriter: TStreamWriter;
  LPair: TPair<string, TAIAgentsNode>;
begin
  LRoot := TJSONObject.Create;
  try
    // 1. Secci?n "graph"
    LGraphObj := TJSONObject.Create;
    LGraphObj.AddPair('description', Self.Description);
    if Assigned(FStartNode) then
      LGraphObj.AddPair('startNode', FStartNode.Name)
    else
      LGraphObj.AddPair('startNode', TJSONNull.Create);

    if Assigned(FEndNode) then
      LGraphObj.AddPair('endNode', FEndNode.Name)
    else
      LGraphObj.AddPair('endNode', TJSONNull.Create);

    LGraphObj.AddPair('maxConcurrentTasks', TJSONNumber.Create(FMaxConcurrentTasks));
    LGraphObj.AddPair('timeoutMs', TJSONNumber.Create(FTimeoutMs));
    LRoot.AddPair('graph', LGraphObj);

    // 2. Secci?n "nodes"
    LNodesArray := TJSONArray.Create;
    for LNode in FNodes do
    begin
      LNodeObj := TJSONObject.Create;
      LNodeObj.AddPair('name', LNode.Name);
      LNodeObj.AddPair('description', LNode.Description);
      LNodeObj.AddPair('joinMode', GetEnumName(TypeInfo(TJoinMode), Ord(LNode.JoinMode)));
      LNodeObj.AddPair('promptName', LNode.PromptName);

      if Assigned(LNode.Tool) then
      begin
        // --- SECCI?N MODIFICADA ---
        var
        LToolDataObj := TJSONObject.Create;
        LToolDataObj.AddPair('className', LNode.Tool.ClassName);
        LToolDataObj.AddPair('properties', SerializeToolProperties(LNode.Tool));
        LNodeObj.AddPair('tool', LToolDataObj);
        // --- FIN DE LA MODIFICACI?N ---
      end
      else
      begin
        LNodeObj.AddPair('tool', TJSONNull.Create);
      end;

      // --- MANEJO DE EVENTOS (requiere l?gica adicional) ---
      // Aqu? guardar?as un identificador del evento. Por ahora, un placeholder.
      if Assigned(LNode.OnExecute) then
        LNodeObj.AddPair('onExecuteHandler', 'HandlerFor_' + LNode.Name) // Placeholder
      else
        LNodeObj.AddPair('onExecuteHandler', TJSONNull.Create);

      if Assigned(LNode.Next) then
        LNodeObj.AddPair('nextLink', LNode.Next.Name)
      else
        LNodeObj.AddPair('nextLink', TJSONNull.Create);

      LNodesArray.Add(LNodeObj);
    end;
    LRoot.AddPair('nodes', LNodesArray);

    // 3. Secci?n "links"
    LLinksArray := TJSONArray.Create;
    for LLink in FLinks do
    begin
      LLinkObj := TJSONObject.Create;
      LLinkObj.AddPair('name', LLink.Name);
      LLinkObj.AddPair('description', LLink.Description);
      LLinkObj.AddPair('mode', GetEnumName(TypeInfo(TLinkMode), Ord(LLink.Mode)));
      LLinkObj.AddPair('maxCycles', LLink.MaxCycles);

      if Assigned(LLink.FSourceNode) then
        LLinkObj.AddPair('sourceNode', LLink.FSourceNode.Name)
      else
        LLinkObj.AddPair('sourceNode', TJSONNull.Create);

      // Guardar destinos en un sub-objeto "targets"
      LTargetsObj := TJSONObject.Create;

      if Assigned(LLink.NextA) then
        LTargetsObj.AddPair('nextA', LLink.NextA.Name)
      else
        LTargetsObj.AddPair('nextA', TJSONNull.Create);

      if Assigned(LLink.NextB) then
        LTargetsObj.AddPair('nextB', LLink.NextB.Name)
      else
        LTargetsObj.AddPair('nextB', TJSONNull.Create);

      if Assigned(LLink.NextC) then
        LTargetsObj.AddPair('nextC', LLink.NextC.Name)
      else
        LTargetsObj.AddPair('nextC', TJSONNull.Create);

      if Assigned(LLink.NextD) then
        LTargetsObj.AddPair('nextD', LLink.NextD.Name)
      else
        LTargetsObj.AddPair('nextD', TJSONNull.Create);

      if Assigned(LLink.NextNo) then
        LTargetsObj.AddPair('nextNo', LLink.NextNo.Name)
      else
        LTargetsObj.AddPair('nextNo', TJSONNull.Create);

      LLinkObj.AddPair('targets', LTargetsObj);

      // Guardar expresiones para el modo lmExpression
      if LLink.Mode = lmExpression then
      begin
        LExpressionsObj := TJSONObject.Create;
        LExpressionsObj.AddPair('expressionA', LLink.ExpressionA);
        LExpressionsObj.AddPair('expressionB', LLink.ExpressionB);
        LExpressionsObj.AddPair('expressionC', LLink.ExpressionC);
        LExpressionsObj.AddPair('expressionD', LLink.ExpressionD);
        LLinkObj.AddPair('expressions', LExpressionsObj);
      end;

      // Guardar destinos condicionales
      if LLink.Mode = lmConditional then
      begin
        LLinkObj.AddPair('conditionalKey', LLink.ConditionalKey);
        if Assigned(LLink.FConditionalTargets) then
        begin
          LCondTargetsObj := TJSONObject.Create;
          for LPair in LLink.FConditionalTargets do
          begin
            LCondTargetsObj.AddPair(LPair.Key, LPair.Value.Name);
          end;
          LLinkObj.AddPair('conditionalTargets', LCondTargetsObj);
        end;
      end;

      LLinksArray.Add(LLinkObj);
    end;
    LRoot.AddPair('links', LLinksArray);

    // 4. Escribir el JSON al Stream
    LWriter := TStreamWriter.Create(AStream, TEncoding.UTF8);
    try
      LWriter.Write(LRoot.ToString); // O LRoot.ToJSON para una versi?n m?s compacta
    finally
      LWriter.Free;
    end;

  finally
    LRoot.Free;
  end;
end;

procedure TAIAgentManager.SetAsynchronous(const Value: Boolean);
begin
  FAsynchronous := Value;
end;

procedure TAIAgentManager.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

// ... (Setters restantes sin cambios) ...
procedure TAIAgentManager.SetEndNode(const Value: TAIAgentsNode);
begin
  if FEndNode <> Value then
  begin
    FEndNode := Value;
    FCompiled := False;
  end;
end;

function TAIAgentManager.SetEntryPoint(const ANodeName: string): TAIAgentManager;
begin
  Self.StartNode := FindNode(ANodeName);
  if not Assigned(Self.StartNode) then
    raise Exception.CreateFmt('Entry point node "%s" not found.', [ANodeName]);
  Result := Self;
end;

function TAIAgentManager.SetFinishPoint(const ANodeName: string): TAIAgentManager;
begin
  Self.EndNode := FindNode(ANodeName);
  if not Assigned(Self.EndNode) then
    raise Exception.CreateFmt('Finish point node "%s" not found.', [ANodeName]);
  Result := Self;
end;

procedure TAIAgentManager.SetOnConfirm(const Value: TAIAgentsOnConfirm);
begin
  FOnConfirm := Value;
end;

procedure TAIAgentManager.SetOnEnd(const Value: TAIAgentsOnEnd);
begin
  FOnEnd := Value;
end;

procedure TAIAgentManager.SetOnEnterNode(const Value: TAIAgentsOnEnterNode);
begin
  FOnEnterNode := Value;
end;

procedure TAIAgentManager.SetOnError(const Value: TAIAgentsOnError);
begin
  FOnError := Value;
end;

procedure TAIAgentManager.SetOnExitNode(const Value: TAIAgentsOnExitNode);
begin
  FOnExitNode := Value;
end;

procedure TAIAgentManager.SetOnFinish(const Value: TAIAgentsOnFinish);
begin
  FOnFinish := Value;
end;

procedure TAIAgentManager.SetOnPrint(const Value: TAIAgentsOnPrint);
begin
  if Assigned(Value) then
    FOnPrintRef := nil;
  FOnPrint := Value;
end;

procedure TAIAgentManager.SetOnPrintEvent(const APrintProc: TAgentPrintRef);
begin
  FOnPrintRef := APrintProc;
  if Assigned(FOnPrintRef) then
    FOnPrint := DoPrintFromRef
  else
    FOnPrint := nil;
end;

procedure TAIAgentManager.SetOnStart(const Value: TAIAgentsOnStart);
begin
  FOnStart := Value;
end;

procedure TAIAgentManager.SetStartNode(const Value: TAIAgentsNode);
begin
  if FStartNode <> Value then
  begin
    FStartNode := Value;
    FCompiled := False;
  end;
end;

procedure TAIAgentManager.SetCheckpointer(const Value: IAiCheckpointer);
begin
  FCheckpointer := Value;
end;

procedure TAIAgentManager.SetOnSuspend(const Value: TAIAgentsOnSuspend);
begin
  FOnSuspend := Value;
end;

// ---------------------------------------------------------------------------
// DoNodeCompleted  -- checkpoint autom?tico tras nodo exitoso
// ---------------------------------------------------------------------------
procedure TAIAgentManager.DoNodeCompleted(ANode: TAIAgentsNode);
var
  LSnap: TAiCheckpointSnapshot;
begin
  if not Assigned(FCheckpointer) then Exit;
  TInterlocked.Increment(FCheckpointSeq);
  LSnap := BuildSnapshot;
  try
    LSnap.CheckpointID := FCheckpointSeq;
    FCheckpointer.SaveCheckpoint(FCurrentThreadID, LSnap);
  finally
    LSnap.Free;
  end;
end;

// ---------------------------------------------------------------------------
// DoNodeSuspended  -- gestiona la suspensi?n de un nodo
// ---------------------------------------------------------------------------
procedure TAIAgentManager.DoNodeSuspended(ANode: TAIAgentsNode;
  ABeforeNode: TAIAgentsNode; ALink: TAIAgentsLink);
var
  LStep: TAiPendingStep;
  LSnap: TAiCheckpointSnapshot;
begin
  LStep := TAiPendingStep.Create(
    ANode.Name,
    IfThen(Assigned(ABeforeNode), ABeforeNode.Name, ''),
    IfThen(Assigned(ALink), ALink.Name, ''),
    ANode.Input,
    'Suspended',
    ANode.SuspendReason,
    ANode.SuspendContext);

  FSuspendedStepsLock.Enter;
  try
    FSuspendedSteps.Add(LStep);
  finally
    FSuspendedStepsLock.Leave;
  end;

  Blackboard.SetStatus(esSuspended);

  if Assigned(FCheckpointer) then
  begin
    TInterlocked.Increment(FCheckpointSeq);
    LSnap := BuildSnapshot;
    try
      LSnap.CheckpointID := FCheckpointSeq;
      FCheckpointer.SaveCheckpoint(FCurrentThreadID, LSnap);
    finally
      LSnap.Free;
    end;
  end;

  // Disparar OnSuspend en el hilo principal (TThread.Queue = no bloqueante)
  if Assigned(FOnSuspend) then
  begin
    var LThreadID   := FCurrentThreadID;
    var LNodeName   := ANode.Name;
    var LReason     := ANode.SuspendReason;
    var LContext    := ANode.SuspendContext;
    TThread.Queue(nil,
      procedure
      begin
        FOnSuspend(Self, LThreadID, LNodeName, LReason, LContext);
      end);
  end;
end;

// ---------------------------------------------------------------------------
// BuildSnapshot  -- captura el estado completo del grafo
// ---------------------------------------------------------------------------
function TAIAgentManager.BuildSnapshot: TAiCheckpointSnapshot;
var
  LNode:     TAIAgentsNode;
  LLink:     TAIAgentsLink;
  LNodeObj:  TJSONObject;
  LJoinObj:  TJSONObject;
  LNodeStates, LLinkStates, LBBObj: TJSONObject;
  LPair:     TPair<TAIAgentsLink, string>;
begin
  Result := TAiCheckpointSnapshot.Create;
  Result.ThreadID  := FCurrentThreadID;
  Result.GraphID   := Self.Name;
  Result.CreatedAt := Now;

  // 1. Blackboard
  LBBObj := TJSONObject.Create;
  SerializeBlackboard(FBlackboard, LBBObj);
  Result.Blackboard := LBBObj;

  // 2. Estado de nodos
  LNodeStates := TJSONObject.Create;
  for LNode in FNodes do
  begin
    LNodeObj := TJSONObject.Create;
    LNodeObj.AddPair('input',          LNode.Input);
    LNodeObj.AddPair('output',         LNode.Output);
    LNodeObj.AddPair('suspended',      TJSONBool.Create(LNode.FSuspended));
    LNodeObj.AddPair('suspendReason',  LNode.FSuspendReason);
    LNodeObj.AddPair('suspendContext', LNode.FSuspendContext);
    LJoinObj := TJSONObject.Create;
    LNode.FJoinLock.Enter;
    try
      for LPair in LNode.FJoinInputs do
        LJoinObj.AddPair(LPair.Key.Name, LPair.Value);
    finally
      LNode.FJoinLock.Leave;
    end;
    LNodeObj.AddPair('joinInputs', LJoinObj);
    LNodeStates.AddPair(LNode.Name, LNodeObj);
  end;
  Result.NodeStates := LNodeStates;

  // 3. Estado de links
  LLinkStates := TJSONObject.Create;
  for LLink in FLinks do
  begin
    var LLinkObj := TJSONObject.Create;
    LLinkObj.AddPair('noCycles', TJSONNumber.Create(LLink.FNoCycles));
    LLinkStates.AddPair(LLink.Name, LLinkObj);
  end;
  Result.LinkStates := LLinkStates;

  // 4. Copia de pasos suspendidos
  FSuspendedStepsLock.Enter;
  try
    for var LStep in FSuspendedSteps do
      Result.PendingSteps.Add(TAiPendingStep.Create(
        LStep.NodeName, LStep.SourceNodeName, LStep.LinkName,
        LStep.Input, LStep.Status, LStep.SuspendReason, LStep.SuspendContext));
  finally
    FSuspendedStepsLock.Leave;
  end;
end;

// ---------------------------------------------------------------------------
// RestoreFromSnapshot  -- restaura el estado desde un snapshot
// ---------------------------------------------------------------------------
procedure TAIAgentManager.RestoreFromSnapshot(ASnapshot: TAiCheckpointSnapshot);
var
  LNode:    TAIAgentsNode;
  LLink:    TAIAgentsLink;
  LNodeObj: TJSONObject;
  LJoinObj: TJSONObject;
  LPair:    TJSONPair;
  LSrcLink: TAIAgentsLink;
begin
  // 1. Blackboard
  if Assigned(ASnapshot.Blackboard) then
    DeserializeBlackboard(ASnapshot.Blackboard, FBlackboard);

  // 2. Estado de nodos
  if Assigned(ASnapshot.NodeStates) then
    for LNode in FNodes do
    begin
      LNodeObj := ASnapshot.NodeStates.GetValue(LNode.Name) as TJSONObject;
      if not Assigned(LNodeObj) then Continue;
      LNode.FInput          := LNodeObj.GetValue<string>('input',          '');
      LNode.FOutput         := LNodeObj.GetValue<string>('output',         '');
      LNode.FSuspended      := LNodeObj.GetValue<Boolean>('suspended',     False);
      LNode.FSuspendReason  := LNodeObj.GetValue<string>('suspendReason',  '');
      LNode.FSuspendContext := LNodeObj.GetValue<string>('suspendContext', '');
      LJoinObj := LNodeObj.GetValue('joinInputs') as TJSONObject;
      if Assigned(LJoinObj) then
      begin
        LNode.FJoinLock.Enter;
        try
          LNode.FJoinInputs.Clear;
          for LPair in LJoinObj do
          begin
            LSrcLink := FindLink(LPair.JsonString.Value);
            if Assigned(LSrcLink) then
              LNode.FJoinInputs.AddOrSetValue(LSrcLink, LPair.JsonValue.Value);
          end;
        finally
          LNode.FJoinLock.Leave;
        end;
      end;
    end;

  // 3. Estado de links
  if Assigned(ASnapshot.LinkStates) then
    for LLink in FLinks do
    begin
      var LLinkObj := ASnapshot.LinkStates.GetValue(LLink.Name) as TJSONObject;
      if Assigned(LLinkObj) then
        LLink.FNoCycles := LLinkObj.GetValue<Integer>('noCycles', 0);
    end;

  // 4. Reconstruir lista de pasos suspendidos
  FSuspendedStepsLock.Enter;
  try
    FSuspendedSteps.Clear;
    for var LStep in ASnapshot.PendingSteps do
      FSuspendedSteps.Add(TAiPendingStep.Create(
        LStep.NodeName, LStep.SourceNodeName, LStep.LinkName,
        LStep.Input, LStep.Status, LStep.SuspendReason, LStep.SuspendContext));
  finally
    FSuspendedStepsLock.Leave;
  end;
end;

// ---------------------------------------------------------------------------
// FindLink  -- busca un link por nombre
// ---------------------------------------------------------------------------
function TAIAgentManager.FindLink(const AName: string): TAIAgentsLink;
var
  LLink: TAIAgentsLink;
begin
  Result := nil;
  for LLink in FLinks do
    if SameText(LLink.Name, AName) then
      Exit(LLink);
end;

// ---------------------------------------------------------------------------
// GetActiveThreads  -- lista de thread IDs activos
// ---------------------------------------------------------------------------
function TAIAgentManager.GetActiveThreads: TArray<string>;
begin
  if Assigned(FCheckpointer) then
    Result := FCheckpointer.GetActiveThreadIDs
  else
  begin
    if (FCurrentThreadID <> '') and (Blackboard.GetStatus = esSuspended) then
    begin
      SetLength(Result, 1);
      Result[0] := FCurrentThreadID;
    end
    else
      SetLength(Result, 0);
  end;
end;

// ---------------------------------------------------------------------------
// ResumeThread  -- reanuda un hilo suspendido
// ---------------------------------------------------------------------------
function TAIAgentManager.ResumeThread(const AThreadID, ANodeName,
  AInput: string): Boolean;
var
  LSnap:        TAiCheckpointSnapshot;
  LNode:        TAIAgentsNode;
  LResumeInput: string;
  LResumeNode:  TAIAgentsNode;
  LOrchTask:    ITask;
begin
  Result := False;

  if TInterlocked.Exchange(FBusy, True) then
    raise Exception.Create('Agent is busy');

  try
    // 1. Cargar checkpoint del disco (si existe)
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
        TInterlocked.Exchange(FBusy, False);
        Exit; // Thread no encontrado ni en disco ni en memoria
      end;
    finally
      LSnap.Free;
    end;

    // 2. Localizar el nodo
    LNode := FindNode(ANodeName);
    if not Assigned(LNode) then
    begin
      TInterlocked.Exchange(FBusy, False);
      raise Exception.CreateFmt('Nodo "%s" no encontrado en el grafo', [ANodeName]);
    end;

    // 3. Preparar el nodo para reanudaci?n
    LNode.FInput          := AInput;
    LNode.FSuspended      := False;
    LNode.FSuspendReason  := '';
    LNode.FSuspendContext := '';

    FSuspendedStepsLock.Enter;
    try
      for var i := FSuspendedSteps.Count - 1 downto 0 do
        if SameText(FSuspendedSteps[i].NodeName, ANodeName) then
          FSuspendedSteps.Delete(i);
    finally
      FSuspendedStepsLock.Leave;
    end;

    // 4. Preparar el grafo
    FAbort := False;
    Blackboard.SetStatus(esRunning);
    FActiveTasksLock.Enter;
    try
      FActiveTasks.Clear;
    finally
      FActiveTasksLock.Leave;
    end;

    // 5. Lanzar nuevo orquestador desde el nodo reanudado
    LResumeInput := AInput;
    LResumeNode  := LNode;

    LOrchTask := TTask.Run(
      procedure
      var
        TasksToWaitOn:   TArray<ITask>;
        WaitResult:      Boolean;
        FinalStatus:     TAgentExecutionStatus;
        HasPendingTasks: Boolean;
        CurrentTask:     ITask;
        FinalOutput:     string;
        FinalException:  Exception;
      begin
        FinalStatus    := esUnknown;
        FinalException := nil;
        try
          try
            // Ejecutar desde el nodo suspendido; aBeforeNode=nil,aLink=nil
            // salta la puerta de join (manejado en DoExecute)
            LResumeNode.DoExecute(nil, nil);

            repeat
              FActiveTasksLock.Enter;
              try
                TasksToWaitOn := FActiveTasks.ToArray;
              finally
                FActiveTasksLock.Leave;
              end;
              HasPendingTasks := False;
              for CurrentTask in TasksToWaitOn do
              begin
                if (CurrentTask.Status <> TTaskStatus.Completed) and
                   (CurrentTask.Status <> TTaskStatus.Canceled)  and
                   (CurrentTask.Status <> TTaskStatus.Exception) then
                begin
                  HasPendingTasks := True;
                  Break;
                end;
              end;
              if not HasPendingTasks then Break;
              WaitResult := TTask.WaitForAll(TasksToWaitOn, FTimeoutMs);
              if not WaitResult then
                raise Exception.CreateFmt('Graph execution timed out after %d ms.', [FTimeoutMs]);
            until FAbort;

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
              if E.Message.Contains('timed out') then
                FinalStatus := esTimeout
              else
                FinalStatus := esError;
              DoError(nil, nil, E);
            end;
          end;
        finally
          if FinalStatus = esUnknown then FinalStatus := esAborted;
          Blackboard.SetStatus(FinalStatus);

          if (FinalStatus = esCompleted) and Assigned(FCheckpointer) then
            FCheckpointer.DeleteCheckpoint(FCurrentThreadID);

          FinalOutput := '';
          if Assigned(FEndNode) then
            FinalOutput := FEndNode.Output;

          if Assigned(FOnFinish) then
            FOnFinish(Self, LResumeInput, FinalOutput, FinalStatus, FinalException);

          TInterlocked.Exchange(FBusy, False);
        end;
      end, FThreadPool);

    if not FAsynchronous then
      LOrchTask.Wait;

    Result := True;
  except
    TInterlocked.Exchange(FBusy, False);
    raise;
  end;
end;

{ TAIAgentsLink }

procedure TAIAgentsLink.AddConditionalTarget(const AKey: string; ANode: TAIAgentsNode);
begin
  if not Assigned(FConditionalTargets) then
    FConditionalTargets := TDictionary<string, TAIAgentsNode>.Create;
  FConditionalTargets.AddOrSetValue(AKey, ANode);
end;

procedure TAIAgentsLink.BuildManualTargets(const TargetsCSV: string; out Nodes: TList<TAIAgentsNode>);
// ... (sin cambios) ...
  function ResolveToken(const S: string): TAIAgentsNode;
  begin
    Result := nil;
    if SameText(S, 'A') then
      Exit(FNextA);
    if SameText(S, 'B') then
      Exit(FNextB);
    if SameText(S, 'C') then
      Exit(FNextC);
    if SameText(S, 'D') then
      Exit(FNextD);
    if Assigned(FGraph) then
      Result := FGraph.FindNode(S);
  end;

var
  Parts: TArray<string>;
  T: string;
  N: TAIAgentsNode;
begin
  Nodes := TList<TAIAgentsNode>.Create;
  if TargetsCSV.Trim = '' then
    Exit;
  Parts := TargetsCSV.Split([',']);
  for T in Parts do
  begin
    N := ResolveToken(Trim(T));
    if Assigned(N) then
      Nodes.Add(N);
  end;
end;

constructor TAIAgentsLink.Create(aOwner: TComponent);
begin
  inherited;
  // --- MODIFICADO: Constructor limpiado de duplicados ---
  FMaxCycles := 1;
  FMode := lmFanout;
  FConditionalKey := 'next_route';
  FManualTargetsKey := 'next_targets';
end;

procedure TAIAgentsLink.CreateAndQueueTask(ANodeToExecute, ASourceNode: TAIAgentsNode; ACurrentLink: TAIAgentsLink);
begin
  var
  LTask := TTask.Run(
    procedure
    begin
      // Esta clausura ahora captura los par?metros del m?todo, que son estables y ?nicos.
      if (ACurrentLink.FGraph <> nil) and ACurrentLink.FGraph.FAbort then
        Exit;
      ANodeToExecute.DoExecute(ASourceNode, ACurrentLink);
    end, FGraph.FThreadPool);

  FGraph.FActiveTasksLock.Enter;
  try
    FGraph.FActiveTasks.Add(LTask);
  finally
    FGraph.FActiveTasksLock.Leave;
  end;
end;

destructor TAIAgentsLink.Destroy;
begin
  if Assigned(FConditionalTargets) then
    FConditionalTargets.Free;

  // Desvincular del grafo al destruir
  if Assigned(FGraph) then
    FGraph.RemoveComponentFromList(Self);

  inherited;
end;

procedure TAIAgentsLink.DoExecute(Sender: TAIAgentsNode);
var
  IsOk, Handled: Boolean;
  NodesToRun: TList<TAIAgentsNode>;
  Decision: string;
begin
  if (FGraph = nil) or FGraph.FAbort then
    Exit;

  // PASO 1: Ejecutar evento OnExecute para intervenci?n manual
  Handled := False;
  IsOk := Not Sender.FError;
  if Assigned(FOnExecute) then
  begin
    try
      FOnExecute(Sender, Self, IsOk, Handled);
    except
      on E: Exception do
      begin
        FGraph.DoError(Sender, Self, E);
        Exit;
      end;
    end;
  end;

  if Handled then
    Exit;

  // PASO 2: Evaluar el resultado de IsOk y manejar fallos/reintentos
  if not IsOk then
  begin
    Inc(FNoCycles);
    if FNoCycles >= FMaxCycles then
    begin
      var
      E := Exception.CreateFmt('Maximum retry cycles (%d) reached on link "%s" from node "%s". Aborting this path.', [FMaxCycles, Self.Name, Sender.Name]);
      FGraph.DoError(Sender, Self, E);
      Exit;
    end
    else
    begin
      if Assigned(FNextNo) then
      begin
        var
        LTask := TTask.Run(
          procedure
          begin
            if (FGraph <> nil) and not FGraph.FAbort then
              FNextNo.DoExecute(FSourceNode, Self);
          end, FGraph.FThreadPool);

        FGraph.FActiveTasksLock.Enter;
        try
          FGraph.FActiveTasks.Add(LTask);
        finally
          FGraph.FActiveTasksLock.Leave;
        end;
      end;
      Exit;
    end;
  end;

  // PASO 3: IsOk es TRUE. Construir la lista de nodos de destino seg?n el modo.
  NodesToRun := nil;
  try
    NodesToRun := TList<TAIAgentsNode>.Create;
    case FMode of
      lmFanout:
        begin
          if Assigned(FNextA) then
            NodesToRun.Add(FNextA);
          if Assigned(FNextB) then
            NodesToRun.Add(FNextB);
          if Assigned(FNextC) then
            NodesToRun.Add(FNextC);
          if Assigned(FNextD) then
            NodesToRun.Add(FNextD);
        end;
      lmConditional:
        begin
          Decision := FGraph.Blackboard.GetString(IfThen(FConditionalKey <> '', FConditionalKey, 'next_route'));
          var
            TargetNode: TAIAgentsNode;
          if Assigned(FConditionalTargets) and FConditionalTargets.TryGetValue(Decision, TargetNode) and Assigned(TargetNode) then
            NodesToRun.Add(TargetNode)
          else if Assigned(FNextNo) then
            NodesToRun.Add(FNextNo);
        end;
      lmManual:
        begin
          Decision := FGraph.Blackboard.GetString(IfThen(FManualTargetsKey <> '', FManualTargetsKey, 'next_targets'));
          var
          ManualNodes := TList<TAIAgentsNode>.Create;
          try
            BuildManualTargets(Decision, ManualNodes);
            NodesToRun.AddRange(ManualNodes);
          finally
            ManualNodes.Free;
          end;
        end;
      lmExpression:
        begin
          var
          LBlackboardData := FGraph.Blackboard.FData;

          // --- Evaluaciones Independientes (L?gica Paralela) ---

          // 1. Evaluar A
          if Assigned(FNextA) and (FExpressionA <> '') and EvalCondition(FExpressionA, LBlackboardData) then
            NodesToRun.Add(FNextA);

          // 2. Evaluar B (Se eval?a SIEMPRE, sin importar si A fue verdadero)
          if Assigned(FNextB) and (FExpressionB <> '') and EvalCondition(FExpressionB, LBlackboardData) then
            NodesToRun.Add(FNextB);

          // 3. Evaluar C
          if Assigned(FNextC) and (FExpressionC <> '') and EvalCondition(FExpressionC, LBlackboardData) then
            NodesToRun.Add(FNextC);

          // 4. Evaluar D
          if Assigned(FNextD) and (FExpressionD <> '') and EvalCondition(FExpressionD, LBlackboardData) then
            NodesToRun.Add(FNextD);

          // --- Fallback (Camino por defecto) ---
          // Solo si NINGUNA de las anteriores se cumpli? (la lista est? vac?a)
          // ejecutamos el camino "NextNo".
          if (NodesToRun.Count = 0) and Assigned(FNextNo) then
            NodesToRun.Add(FNextNo);
        end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(NodesToRun) then
        NodesToRun.Free;
      FGraph.DoError(Sender, Self, E);
      Exit;
    end;
  end;

  // ---------------------------------------------------------------------------
  // PASO 4: Despachar la ejecuci?n a los nodos de destino.
  // ---------------------------------------------------------------------------
  try
    if (NodesToRun = nil) or (NodesToRun.Count = 0) then
      Exit;

    if NodesToRun.Count = 1 then
    begin
      NodesToRun[0].DoExecute(FSourceNode, Self);
    end
    else // Fork paralelo con scheduler
    begin
      for var Node in NodesToRun do
      begin
        CreateAndQueueTask(Node, FSourceNode, Self);
      end;
    end;
  finally
    if Assigned(NodesToRun) then
      NodesToRun.Free;
  end;
end;

{ procedure TAIAgentsLink.DoExecute(Sender: TAIAgentsNode);
  var
  IsOk, Handled: Boolean;
  NodesToRun: TList<TAIAgentsNode>;
  Decision: string;
  TargetNode: TAIAgentsNode;
  ManualNodes: TList<TAIAgentsNode>;
  LBlackboardData: TDictionary<string, TValue>;
  Node: TAIAgentsNode;
  begin
  // Validaci?n de seguridad inicial
  if (FGraph = nil) or FGraph.FAbort then
  Exit;

  // PASO 1: Ejecutar evento OnExecute para intervenci?n manual (el programador decide en c?digo)
  Handled := False;

  IsOk := Sender.FError; //Toma el estado de error del nodo como valor inicial;


  if Assigned(FOnExecute) then
  begin
  try
  FOnExecute(Sender, Self, IsOk, Handled);
  except
  on E: Exception do
  begin
  FGraph.DoError(Sender, Self, E);
  Exit;
  end;
  end;
  end;

  // Si el programador marc? 'Handled' en el evento, se asume que la l?gica
  // de navegaci?n ya fue gestionada externamente y salimos.
  if Handled then
  Exit;

  // PASO 2: Evaluar el resultado de IsOk y manejar fallos o reintentos
  if not IsOk then
  begin
  Inc(FNoCycles);
  if FNoCycles >= FMaxCycles then
  begin
  var E := Exception.CreateFmt('Maximum retry cycles (%d) reached on link "%s" from node "%s". Aborting this path.',
  [FMaxCycles, Self.Name, Sender.Name]);
  FGraph.DoError(Sender, Self, E);
  Exit;
  end
  else
  begin
  // Si el enlace fall? pero tiene una ruta de escape/error (NextNo), la seguimos.
  if Assigned(FNextNo) then
  begin
  var LTask := TTask.Run(
  procedure
  begin
  if (FGraph <> nil) and not FGraph.FAbort then
  FNextNo.DoExecute(FSourceNode, Self);
  end, FGraph.FThreadPool);

  FGraph.FActiveTasksLock.Enter;
  try
  FGraph.FActiveTasks.Add(LTask);
  finally
  FGraph.FActiveTasksLock.Leave;
  end;
  end;
  Exit;
  end;
  end;

  // PASO 3: IsOk es TRUE. Construir la lista de nodos de destino seg?n el modo de enlace.
  NodesToRun := TList<TAIAgentsNode>.Create;
  try
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
  Decision := FGraph.Blackboard.GetString(IfThen(FConditionalKey <> '', FConditionalKey, 'next_route'));
  if Assigned(FConditionalTargets) and FConditionalTargets.TryGetValue(Decision, TargetNode) and Assigned(TargetNode) then
  NodesToRun.Add(TargetNode)
  else if Assigned(FNextNo) then
  NodesToRun.Add(FNextNo);
  end;

  lmManual:
  begin
  Decision := FGraph.Blackboard.GetString(IfThen(FManualTargetsKey <> '', FManualTargetsKey, 'next_targets'));
  ManualNodes := TList<TAIAgentsNode>.Create;
  try
  BuildManualTargets(Decision, ManualNodes);
  for Node in ManualNodes do
  NodesToRun.Add(Node);
  finally
  ManualNodes.Free;
  end;
  end;

  lmExpression:
  begin
  LBlackboardData := FGraph.Blackboard.FData;
  // Evaluamos expresiones secuencialmente. Se a?ade el primer nodo cuya condici?n se cumpla.
  if Assigned(FNextA) and (FExpressionA <> '') and EvalCondition(FExpressionA, LBlackboardData) then
  NodesToRun.Add(FNextA)
  else if Assigned(FNextB) and (FExpressionB <> '') and EvalCondition(FExpressionB, LBlackboardData) then
  NodesToRun.Add(FNextB)
  else if Assigned(FNextC) and (FExpressionC <> '') and EvalCondition(FExpressionC, LBlackboardData) then
  NodesToRun.Add(FNextC)
  else if Assigned(FNextD) and (FExpressionD <> '') and EvalCondition(FExpressionD, LBlackboardData) then
  NodesToRun.Add(FNextD)
  else if Assigned(FNextNo) then
  NodesToRun.Add(FNextNo);
  end;
  end;
  except
  on E: Exception do
  begin
  FGraph.DoError(Sender, Self, E);
  Exit;
  end;
  end;

  // ---------------------------------------------------------------------------
  // PASO 4: Despachar la ejecuci?n a los nodos de destino.
  // ---------------------------------------------------------------------------
  if NodesToRun.Count = 0 then
  Exit;

  if NodesToRun.Count = 1 then
  begin
  // Si solo hay un destino, continuamos la ejecuci?n de forma secuencial en este hilo
  NodesToRun[0].DoExecute(FSourceNode, Self);
  end
  else
  begin
  // Fork paralelo: Si hay m?ltiples destinos, cada uno se convierte en una tarea del pool
  for Node in NodesToRun do
  begin
  CreateAndQueueTask(Node, FSourceNode, Self);
  end;
  end;

  finally
  NodesToRun.Free;
  end;
  end;
}

procedure TAIAgentsLink.Print(Value: String);
begin
  if Assigned(FGraph) then
    FGraph.DoPrint(Self, Value);
end;

procedure TAIAgentsLink.SetGraph(const Value: TAIAgentManager);
begin
  if Value <> FGraph then
  begin
    // 1. Desvincular del grafo anterior si exist?a
    if Assigned(FGraph) then
      FGraph.RemoveComponentFromList(Self);

    FGraph := Value;

    // 2. Registrar en el nuevo grafo
    if Assigned(FGraph) then
    begin
      FGraph.AddComponentToList(Self); // <--- ESTO FALTABA
      FGraph.FCompiled := False;
    end;
  end;
end;

procedure TAIAgentsLink.SetMaxCycles(const Value: Integer);
begin
  FMaxCycles := Value;
  if FMaxCycles < 1 then
    FMaxCycles := 1;
end;

procedure TAIAgentsLink.SetMode(const Value: TLinkMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    if Assigned(FGraph) then
      FGraph.FCompiled := False;
  end;
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
  // --- NUEVO: Inicializar diccionario de join ---
  FJoinInputs := TDictionary<TAIAgentsLink, string>.Create;
  FJoinMode := jmAny;
end;

destructor TAIAgentsNode.Destroy;
begin
  FInEdges.Free;
  FJoinLock.Free;
  FJoinInputs.Free; // --- NUEVO: Liberar diccionario de join ---
  inherited;
end;

procedure TAIAgentsNode.Reset;
begin
  FInEdges.Clear;
  FJoinInputs.Clear; // --- NUEVO: Limpiar diccionario de join ---
  Input     := '';
  Output    := '';
  FError    := False;
  FMsgError := '';
  // --- Limpiar estado de suspensi?n ---
  FSuspended      := False;
  FSuspendReason  := '';
  FSuspendContext := '';
end;

procedure TAIAgentsNode.Suspend(const AReason: string; const AContext: string);
begin
  // Llamado desde hilo de trabajo. Solo marca el estado.
  // DoExecute verifica este flag antes de llamar a OnExitNode y enrutar.
  FSuspended      := True;
  FSuspendReason  := AReason;
  FSuspendContext := AContext;
end;

// --- MODIFICADO: M?todo DoExecute con l?gica de JOIN corregida ---
procedure TAIAgentsNode.DoExecute(aBeforeNode: TAIAgentsNode; aLink: TAIAgentsLink);
var
  CanExecute: Boolean;
begin
  if (FGraph = nil) or FGraph.FAbort then
    Exit;

  if Assigned(FGraph.OnEnterNode) then
    TThread.Synchronize(nil,
      procedure
      begin
        FGraph.OnEnterNode(FGraph, Self);
      end);

  CanExecute := False;
  // Caso especial: reanudaci?n desde ResumeThread (aBeforeNode=nil, aLink=nil)
  // con nodo join (FInEdges.Count > 1). Saltamos la puerta de join.
  if (aBeforeNode = nil) and (aLink = nil) and (FInEdges.Count > 1) then
  begin
    CanExecute := True;
    // FInput ya fue seteado por ResumeThread antes de llamar a DoExecute
  end
  else if FInEdges.Count > 1 then
  begin
    FJoinLock.Enter;
    try
      // Almacenar (o actualizar) la entrada del camino que llega.
      // Esto es vital: si ya exist?a un valor de una vuelta anterior, se sobrescribe con el nuevo.
      if (aBeforeNode <> nil) and (aLink <> nil) then
        FJoinInputs.AddOrSetValue(aLink, aBeforeNode.Output);

      case FJoinMode of
        jmAny:
          begin
            CanExecute := True;
            // Para jmAny, tomamos la entrada del primero que llega
            if aBeforeNode <> nil then
              Input := aBeforeNode.Output;
          end;
        jmAll:
          begin
            // Se ejecuta si tenemos datos de TODAS las entradas requeridas.
            if FJoinInputs.Count >= FInEdges.Count then
            begin
              CanExecute := True;
              // Consolidamos todas las entradas en un ?nico string
              var
              sb := TStringBuilder.Create;
              try
                for var Value in FJoinInputs.Values do
                  sb.AppendLine(Value);
                Input := sb.ToString;
              finally
                sb.Free;
              end;
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
    if aBeforeNode <> nil then
      Input := aBeforeNode.Output; // Comportamiento normal para nodos sin join
  end;

  if not CanExecute then
    Exit;

  try
    // Limpiar estado de suspensi?n al inicio de cada ejecuci?n
    FSuspended      := False;
    FSuspendReason  := '';
    FSuspendContext := '';

    if Assigned(FOnExecute) then
      FOnExecute(Self, aBeforeNode, aLink, Self.Input, FOutput)
    else if Assigned(FTool) then
      FTool.Run(Self, Self.Input, FOutput);

    if FOutput = '' then
      FOutput := Input;

    // Verificar suspensi?n antes de enrutar al siguiente link
    if FSuspended then
    begin
      if Assigned(FGraph) then
        FGraph.DoNodeSuspended(Self, aBeforeNode, aLink);
      Exit; // No llamar OnExitNode, no enrutar al siguiente Link
    end;

    if Assigned(FGraph.OnExitNode) then
      TThread.Synchronize(nil,
        procedure
        begin
          FGraph.OnExitNode(FGraph, Self);
        end);

    if Assigned(Next) then
    begin
      Next.FSourceNode := Self;
      Next.DoExecute(Self);
    end;

    // Checkpoint tras nodo exitoso
    if Assigned(FGraph) then
      FGraph.DoNodeCompleted(Self);
  finally
    // --- LIMPIEZA DE ESTADO ---
    if CanExecute and (FInEdges.Count > 1) then
    begin
      FJoinLock.Enter;
      try
        // Para jmAny: Limpiamos siempre para que el pr?ximo evento dispare de nuevo limpiamente.
        if FJoinMode = jmAny then
          FJoinInputs.Clear;

        // Para jmAll: NO LIMPIAMOS.
        // Esto permite el patr?n "CombineLatest". Si un flujo se repite (loop),
        // el nodo recordar? los valores de los otros flujos que no cambiaron.
        // La limpieza total solo ocurre al iniciar el grafo (Reset).

        { CODIGO ELIMINADO:
          if (FJoinMode = jmAll) and (FJoinInputs.Count >= FInEdges.Count) then
          FJoinInputs.Clear;
        }
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
      FOnExecute(Self, nil, nil, Self.Input, FOutput);

    if Assigned(FGraph.OnEnd) then
      TThread.Synchronize(nil,
        procedure
        begin
          FGraph.OnEnd(Self, Output);
        end);
  except
    on E: Exception do
      FGraph.DoError(Self, nil, E);
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

procedure TAIAgentsNode.SetError(const Value: Boolean);
begin
  FError := Value;
end;

procedure TAIAgentsNode.SetGraph(const Value: TAIAgentManager);
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

procedure TAIAgentsNode.SetMsgError(const Value: String);
begin
  FMsgError := Value;
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

procedure TAIAgentsBase.SetID(const Value: String);
begin
  FID := Value;
end;

{ TAiToolBase }
procedure TAiToolBase.Run(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
begin
  Execute(ANode, AInput, AOutput);
end;

procedure TAiToolBase.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TAiToolBase.SetID(const Value: String);
begin
  FID := Value;
end;

{ TAiSampleTool }
procedure TAiAgentsToolSample.Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
begin
  ANode.Print(Format('TAiSampleTool: Executing with input "%s"', [AInput]));
  AOutput := 'Output from TAiSampleTool: ' + AInput;
end;

end.
