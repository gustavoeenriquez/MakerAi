// MIT License
//
// MakerAI - Servidor A2A (Agent-to-Agent Protocol, Linux Foundation)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.A2A.Server;

// -----------------------------------------------------------------------------
// TAiA2AServer (spec A2A 1.0): expone uno o varios TAIAgentManager como agente
// A2A para flujos de orquestacion / federacion.
//
// - Agent Card en GET /.well-known/agent-card.json (y el alias legacy
//   /.well-known/agent.json).
// - Binding JSON-RPC 2.0 en POST /: metodos 1.0 (SendMessage, GetTask,
//   CancelTask) con tolerancia a los aliases 0.x (message/send, tasks/get,
//   tasks/cancel).
//
// CONCURRENCIA
//   Un unico TAIAgentManager no puede ejecutar dos grafos a la vez, asi que un
//   servidor con un solo manager serializa los tasks (el segundo recibe
//   AgentBusy). Para orquestacion real (fan-out de varios nodos contra el mismo
//   agente remoto) asignar OnAcquireManager: la fabrica devuelve una instancia
//   nueva del grafo y el servidor mantiene un pool de hasta MaxConcurrentTasks.
//   La reserva del slot es atomica, de modo que dos peticiones simultaneas no
//   pueden quedarse con el mismo manager.
//
// CICLO DE VIDA DEL TASK
//   SendMessage crea el task y lanza el grafo. Si configuration.blocking es
//   false (o vence WaitTimeoutMs) responde con el task en estado working y el
//   cliente sigue con GetTask; el estado se refresca de forma perezosa desde el
//   manager, de modo que un task nunca queda mintiendo. El slot se libera solo
//   al llegar a un estado terminal.
//
// HUMAN-IN-THE-LOOP
//   Una suspension del grafo (Node.Suspend) se publica como input-required y el
//   task conserva su manager. Un SendMessage posterior que incluya ese taskId
//   reanuda el grafo (ResumeThread) con el texto recibido como respuesta
//   humana, de modo que la conversacion multi-turno cruza A2A.
//
// OBSERVABILIDAD
//   El span de servidor continua la traza del llamante si llega el header
//   traceparent (W3C), igual que hace el servidor MCP con _meta.
//
// Sin streaming ni push notifications: capabilities=false y la spec exige
// rechazar SendStreamingMessage con UnsupportedOperationError.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.SyncObjs, System.DateUtils,
  IdContext, IdCustomHTTPServer, IdHTTPServer,
  uMakerAi.Agents;

type
  // Excepcion con codigo JSON-RPC explicito (evita mapear errores por prefijo
  // del mensaje, que era fragil y perdia el codigo ante cualquier retoque).
  EA2AServerError = class(Exception)
  private
    FCode: Integer;
  public
    constructor CreateCode(ACode: Integer; const AMsg: string);
    property Code: Integer read FCode;
  end;

  // Forma en que se serializan los enums de la spec:
  //   anProto -> TASK_STATE_COMPLETED / ROLE_AGENT  (v1.0: ProtoJSON)
  //   anLower -> completed / agent                  (era 0.x, kebab-case)
  // v1.0 paso de kebab-case a SCREAMING_SNAKE_CASE por conformidad con
  // ProtoJSON, asi que anProto es el correcto y anLower el de compatibilidad.
  // La lectura siempre tolera ambas.
  TAiA2ANaming = (anProto, anLower);

  // Era del formato de cable. Verificado contra a2a-sdk 1.1.2:
  //   weV1  -> SendMessage devuelve SendMessageResponse, con el Task ENVUELTO
  //            en {"task": {...}}, y la Agent Card publica supportedInterfaces[].
  //   weV03 -> el Task viaja plano en result y la card lleva url/protocolVersion
  //            en la raiz (lo que emitia MakerAI antes de agosto 2026).
  // OJO: GetTask y CancelTask devuelven el Task DIRECTO en ambas eras; en el
  // proto son "returns (Task)" y no tienen mensaje de respuesta propio.
  TAiA2AWireEra = (weV1, weV03);

  TAiA2AServer = class;

  // Slot del pool: un manager + su reserva. La reserva es propia del servidor y
  // no depende de Manager.Busy, para cerrar la ventana TOCTOU entre "vi que
  // estaba libre" y "lo puse a correr".
  TAiA2AManagerSlot = class
  public
    Manager: TAIAgentManager;
    Owned: Boolean; // creado por la fabrica -> lo liberamos nosotros
    InUse: Boolean;
  end;

  TAiA2ATaskInfo = class
  public
    Id: string;
    ContextId: string;
    State: string; // canonico en minusculas: working, completed, ...
    OutputText: string;
    ErrorText: string;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
    // Vinculo con la ejecucion viva (nil cuando el task es terminal)
    Slot: TAiA2AManagerSlot;
    ThreadID: string;
    SuspendNode: string;
    SuspendReason: string;
    SuspendContext: string;
    History: TStringList; // 'role<TAB>texto'
    constructor Create;
    destructor Destroy; override;
  end;

  // Fabrica de managers para el pool. Debe devolver un grafo nuevo y completo
  // (mismo shape que AgentManager); el servidor se hace cargo de liberarlo.
  TAiA2AAcquireManager = procedure(Sender: TObject; var AManager: TAIAgentManager) of object;
  // Ultimo retoque de la Agent Card antes de publicarla.
  TAiA2ACardEvent = procedure(Sender: TObject; ACard: TJSONObject) of object;
  // Autorizacion propia. Se llama antes de procesar la peticion; AAllow=False
  // responde 401. AAuthHeader es el valor crudo de Authorization.
  TAiA2AAuthEvent = procedure(Sender: TObject; const AAuthHeader: string; var AAllow: Boolean) of object;

  TAiA2AServer = class(TComponent)
  private
    FHttpServer: TIdHTTPServer;
    FAgentManager: TAIAgentManager;
    FPort: Integer;
    FActive: Boolean;
    FAgentName: string;
    FAgentDescription: string;
    FAgentVersion: string;
    FWaitTimeoutMs: Integer;
    FMaxConcurrentTasks: Integer;
    FTaskTtlSeconds: Integer;
    FMaxTasks: Integer;
    FApiKey: string;
    FPublicUrl: string;
    FStateNaming: TAiA2ANaming;
    FWireEra: TAiA2AWireEra;
    FTasks: TObjectDictionary<string, TAiA2ATaskInfo>;
    FTasksLock: TCriticalSection;
    FSlots: TObjectList<TAiA2AManagerSlot>;
    FSlotsLock: TCriticalSection;
    FOnAcquireManager: TAiA2AAcquireManager;
    FOnCustomizeCard: TAiA2ACardEvent;
    FOnAuthorize: TAiA2AAuthEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetAgentManager(const Value: TAIAgentManager);
    procedure HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    // --- pool ---
    function AcquireSlot: TAiA2AManagerSlot;
    procedure ReleaseSlot(ASlot: TAiA2AManagerSlot);
    procedure ClearSlots;
    // --- tasks ---
    function NewTask(const AContextId: string): TAiA2ATaskInfo;
    function FindTask(const AId: string): TAiA2ATaskInfo;
    procedure RefreshTask(AInfo: TAiA2ATaskInfo); // exige FTasksLock tomado
    procedure StartTask(AInfo: TAiA2ATaskInfo; const AInput: string);
    procedure ResumeTask(AInfo: TAiA2ATaskInfo; const AInput: string);
    procedure WaitTask(AInfo: TAiA2ATaskInfo);
    procedure PurgeTasks;
    // --- json ---
    function BuildAgentCard(const ABaseUrl: string): TJSONObject;
    function BuildTaskJson(AInfo: TAiA2ATaskInfo): TJSONObject;
    // Envuelve el Task como SendMessageResponse cuando la era es v1.0.
    function WrapSendMessageResult(ATask: TJSONObject): TJSONObject;
    function HandleJsonRpc(const ABody, ATraceParent: string): string;
    function RpcError(AId: TJSONValue; ACode: Integer; const AMsg: string): string;
    function RpcResult(AId: TJSONValue; AResult: TJSONObject): string;
    function DoSendMessage(AParams: TJSONObject): TJSONObject;
    function DoGetTask(AParams: TJSONObject): TJSONObject;
    function DoCancelTask(AParams: TJSONObject): TJSONObject;
    function EmitState(const ACanonical: string): string;
    function EmitRole(const ACanonical: string): string;
    class function ExtractTextFromParts(AMessage: TJSONObject): string; static;
    class function StatusToA2AState(AStatus: TAgentExecutionStatus): string; static;
    class function IsTerminal(const AState: string): Boolean; static;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Active: Boolean read FActive write SetActive;
  published
    property Port: Integer read FPort write FPort default 8280;
    property AgentManager: TAIAgentManager read FAgentManager write SetAgentManager;
    property AgentName: string read FAgentName write FAgentName;
    property AgentDescription: string read FAgentDescription write FAgentDescription;
    property AgentVersion: string read FAgentVersion write FAgentVersion;
    // Espera maxima de SendMessage bloqueante. Al vencer NO se falla el task:
    // se devuelve en working para que el cliente siga con GetTask.
    property WaitTimeoutMs: Integer read FWaitTimeoutMs write FWaitTimeoutMs default 60000;
    // Alias del nombre anterior: los DFM ya escritos siguen cargando. No se
    // vuelve a serializar (stored False) para que el nombre nuevo sea el unico
    // que quede en disco.
    property RunTimeoutMs: Integer read FWaitTimeoutMs write FWaitTimeoutMs stored False;
    // Tope del pool. Solo se supera 1 si OnAcquireManager esta asignado.
    property MaxConcurrentTasks: Integer read FMaxConcurrentTasks write FMaxConcurrentTasks default 4;
    // Retencion de tasks terminados (segundos) y tope duro del registry.
    property TaskTtlSeconds: Integer read FTaskTtlSeconds write FTaskTtlSeconds default 3600;
    property MaxTasks: Integer read FMaxTasks write FMaxTasks default 1000;
    // Si no esta vacio se exige Authorization: Bearer <ApiKey> (o X-API-Key).
    property ApiKey: string read FApiKey write FApiKey;
    // URL publica a anunciar en la card (util detras de un reverse proxy).
    property PublicUrl: string read FPublicUrl write FPublicUrl;
    property StateNaming: TAiA2ANaming read FStateNaming write FStateNaming default anProto;
    // Formato de cable. weV1 es el de la spec 1.0 y el default; weV03 reproduce
    // lo que emitia MakerAI antes de agosto 2026, para no romper integraciones
    // que ya dependieran de aquella forma.
    property WireEra: TAiA2AWireEra read FWireEra write FWireEra default weV1;
    property OnAcquireManager: TAiA2AAcquireManager read FOnAcquireManager write FOnAcquireManager;
    property OnCustomizeCard: TAiA2ACardEvent read FOnCustomizeCard write FOnCustomizeCard;
    property OnAuthorize: TAiA2AAuthEvent read FOnAuthorize write FOnAuthorize;
  end;

const
  // Estados canonicos de la spec (forma JSON-RPC en minusculas)
  A2A_STATE_SUBMITTED = 'submitted';
  A2A_STATE_WORKING = 'working';
  A2A_STATE_INPUT_REQUIRED = 'input-required';
  A2A_STATE_COMPLETED = 'completed';
  A2A_STATE_CANCELED = 'canceled';
  A2A_STATE_FAILED = 'failed';

  // Codigos de error JSON-RPC especificos A2A (rango servidor)
  A2A_ERR_TASK_NOT_FOUND = -32001;
  A2A_ERR_TASK_NOT_CANCELABLE = -32002;
  A2A_ERR_UNSUPPORTED_OPERATION = -32004;
  A2A_ERR_AGENT_BUSY = -32000;

// Convierte cualquiera de las dos formas ('TASK_STATE_INPUT_REQUIRED' o
// 'input-required') al canonico en minusculas. Publica porque el cliente la usa.
function A2ACanonicalState(const AValue: string): string;

procedure Register;

implementation

uses uMakerAi.Telemetry;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiA2AServer]);
end;

function A2ACanonicalState(const AValue: string): string;
var
  S: string;
begin
  S := Trim(AValue).ToLower;
  if S.StartsWith('task_state_') then
    S := S.Substring(Length('task_state_'));
  S := S.Replace('_', '-');
  Result := S;
end;

function NewGuidStr: string;
var
  G: TGUID;
begin
  CreateGUID(G);
  Result := GUIDToString(G).Replace('{', '').Replace('}', '').ToLower;
end;

{ EA2AServerError }

constructor EA2AServerError.CreateCode(ACode: Integer; const AMsg: string);
begin
  inherited Create(AMsg);
  FCode := ACode;
end;

{ TAiA2ATaskInfo }

constructor TAiA2ATaskInfo.Create;
begin
  inherited Create;
  History := TStringList.Create;
end;

destructor TAiA2ATaskInfo.Destroy;
begin
  History.Free;
  inherited;
end;

{ TAiA2AServer }

constructor TAiA2AServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort := 8280;
  FAgentVersion := '1.0.0';
  FWaitTimeoutMs := 60000;
  FMaxConcurrentTasks := 4;
  FTaskTtlSeconds := 3600;
  FMaxTasks := 1000;
  FStateNaming := anProto;
  FWireEra := weV1;
  FTasks := TObjectDictionary<string, TAiA2ATaskInfo>.Create([doOwnsValues]);
  FTasksLock := TCriticalSection.Create;
  FSlots := TObjectList<TAiA2AManagerSlot>.Create(True);
  FSlotsLock := TCriticalSection.Create;
  FHttpServer := TIdHTTPServer.Create(Self);
  FHttpServer.OnCommandGet := HttpCommand;
  FHttpServer.OnCommandOther := HttpCommand;
end;

destructor TAiA2AServer.Destroy;
begin
  Stop;
  FTasks.Free;
  FTasksLock.Free;
  ClearSlots;
  FSlots.Free;
  FSlotsLock.Free;
  inherited;
end;

procedure TAiA2AServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FAgentManager) then
  begin
    FAgentManager := nil;
    ClearSlots;
  end;
end;

procedure TAiA2AServer.SetAgentManager(const Value: TAIAgentManager);
begin
  if FAgentManager <> Value then
  begin
    FAgentManager := Value;
    ClearSlots; // el pool se reconstruye con el manager nuevo
    if Assigned(FAgentManager) then
      FAgentManager.FreeNotification(Self);
  end;
end;

procedure TAiA2AServer.SetActive(const Value: Boolean);
begin
  if Value = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    FActive := Value;
    Exit;
  end;
  if Value then
    Start
  else
    Stop;
end;

procedure TAiA2AServer.Start;
begin
  if FActive then
    Exit;
  FHttpServer.DefaultPort := FPort;
  FHttpServer.Active := True;
  FActive := True;
end;

procedure TAiA2AServer.Stop;
begin
  if not FActive then
    Exit;
  FHttpServer.Active := False;
  FActive := False;
end;

// ---------------------------------------------------------------------------
// Pool de managers
// ---------------------------------------------------------------------------

procedure TAiA2AServer.ClearSlots;
var
  Slot: TAiA2AManagerSlot;
begin
  FSlotsLock.Enter;
  try
    for Slot in FSlots do
      if Slot.Owned and Assigned(Slot.Manager) then
      begin
        Slot.Manager.Free;
        Slot.Manager := nil;
      end;
    FSlots.Clear;
  finally
    FSlotsLock.Leave;
  end;
end;

function TAiA2AServer.AcquireSlot: TAiA2AManagerSlot;
var
  Slot: TAiA2AManagerSlot;
  NewMgr: TAIAgentManager;
begin
  Result := nil;
  FSlotsLock.Enter;
  try
    // El manager de diseno es siempre el slot 0.
    if (FSlots.Count = 0) and Assigned(FAgentManager) then
    begin
      Slot := TAiA2AManagerSlot.Create;
      Slot.Manager := FAgentManager;
      Slot.Owned := False;
      FSlots.Add(Slot);
    end;

    for Slot in FSlots do
      if (not Slot.InUse) and Assigned(Slot.Manager) and (not Slot.Manager.Busy) then
      begin
        Slot.InUse := True;
        Exit(Slot);
      end;

    // Crecer el pool solo si hay fabrica: sin ella no hay forma de tener un
    // segundo grafo equivalente.
    if Assigned(FOnAcquireManager) and (FSlots.Count < FMaxConcurrentTasks) then
    begin
      NewMgr := nil;
      FOnAcquireManager(Self, NewMgr);
      if Assigned(NewMgr) then
      begin
        Slot := TAiA2AManagerSlot.Create;
        Slot.Manager := NewMgr;
        Slot.Owned := True;
        Slot.InUse := True;
        FSlots.Add(Slot);
        Exit(Slot);
      end;
    end;
  finally
    FSlotsLock.Leave;
  end;

  if Result = nil then
    raise EA2AServerError.CreateCode(A2A_ERR_AGENT_BUSY,
      'Agent busy: no free agent slot (MaxConcurrentTasks=' + IntToStr(FMaxConcurrentTasks) + ')');
end;

procedure TAiA2AServer.ReleaseSlot(ASlot: TAiA2AManagerSlot);
begin
  if ASlot = nil then
    Exit;
  FSlotsLock.Enter;
  try
    ASlot.InUse := False;
  finally
    FSlotsLock.Leave;
  end;
end;

// ---------------------------------------------------------------------------
// Registry de tasks
// ---------------------------------------------------------------------------

function TAiA2AServer.NewTask(const AContextId: string): TAiA2ATaskInfo;
begin
  Result := TAiA2ATaskInfo.Create;
  Result.Id := NewGuidStr;
  if AContextId <> '' then
    Result.ContextId := AContextId
  else
    Result.ContextId := NewGuidStr;
  Result.State := A2A_STATE_SUBMITTED;
  Result.CreatedAt := Now;
  Result.UpdatedAt := Result.CreatedAt;
  FTasksLock.Enter;
  try
    FTasks.Add(Result.Id, Result);
  finally
    FTasksLock.Leave;
  end;
end;

// v1.0: SendMessage responde un SendMessageResponse, que es un oneof
// {task | message}. El Task por tanto va ENVUELTO. GetTask y CancelTask no
// llevan wrapper: en el proto devuelven Task directamente.
function TAiA2AServer.WrapSendMessageResult(ATask: TJSONObject): TJSONObject;
begin
  if FWireEra = weV03 then
    Exit(ATask);
  Result := TJSONObject.Create;
  Result.AddPair('task', ATask); // toma posesion
end;

function TAiA2AServer.FindTask(const AId: string): TAiA2ATaskInfo;
begin
  if not FTasks.TryGetValue(AId, Result) then
    Result := nil;
end;

class function TAiA2AServer.IsTerminal(const AState: string): Boolean;
begin
  Result := (AState = A2A_STATE_COMPLETED) or (AState = A2A_STATE_FAILED) or (AState = A2A_STATE_CANCELED);
end;

class function TAiA2AServer.StatusToA2AState(AStatus: TAgentExecutionStatus): string;
begin
  case AStatus of
    esCompleted:
      Result := A2A_STATE_COMPLETED;
    esSuspended:
      Result := A2A_STATE_INPUT_REQUIRED; // human-in-the-loop del grafo
    esAborted:
      Result := A2A_STATE_CANCELED;
    esRunning:
      Result := A2A_STATE_WORKING;
  else
    Result := A2A_STATE_FAILED; // esError, esTimeout, esUnknown
  end;
end;

// Refresca el estado del task desde su manager. Se llama con FTasksLock tomado.
// Es la pieza que evita que un task quede mintiendo: mientras la ejecucion siga
// viva, el estado publicado sale siempre del grafo, no de lo que se anoto al
// responder el SendMessage.
procedure TAiA2AServer.RefreshTask(AInfo: TAiA2ATaskInfo);
var
  Mgr: TAIAgentManager;
  St: TAgentExecutionStatus;
  Names: TArray<string>;
  Node: TAIAgentsNode;
begin
  if (AInfo = nil) or (AInfo.Slot = nil) then
    Exit;
  if IsTerminal(AInfo.State) then
    Exit;

  Mgr := AInfo.Slot.Manager;
  if not Assigned(Mgr) then
    Exit;

  if Mgr.Busy then
  begin
    AInfo.State := A2A_STATE_WORKING;
    Exit;
  end;

  St := Mgr.Blackboard.GetStatus;
  AInfo.State := StatusToA2AState(St);
  AInfo.UpdatedAt := Now;

  if St = esSuspended then
  begin
    // Conservamos el slot: el grafo suspendido vive dentro de ese manager y es
    // el unico que puede reanudarlo.
    AInfo.ThreadID := Mgr.CurrentThreadID;
    Names := Mgr.GetSuspendedNodeNames;
    if Length(Names) > 0 then
    begin
      AInfo.SuspendNode := Names[0];
      Node := Mgr.FindNode(AInfo.SuspendNode);
      if Assigned(Node) then
      begin
        AInfo.SuspendReason := Node.SuspendReason;
        AInfo.SuspendContext := Node.SuspendContext;
      end;
    end;
    Exit;
  end;

  if St = esCompleted then
  begin
    if Assigned(Mgr.EndNode) then
      AInfo.OutputText := Mgr.EndNode.Output;
    if AInfo.OutputText <> '' then
      AInfo.History.Add('agent'#9 + AInfo.OutputText);
  end
  else if AInfo.State = A2A_STATE_FAILED then
  begin
    if AInfo.ErrorText = '' then
      AInfo.ErrorText := Mgr.Blackboard.GetString('Execution.ErrorMessage');
    if AInfo.ErrorText = '' then
      AInfo.ErrorText := 'Graph execution failed';
  end;

  ReleaseSlot(AInfo.Slot);
  AInfo.Slot := nil;
end;

procedure TAiA2AServer.StartTask(AInfo: TAiA2ATaskInfo; const AInput: string);
var
  Slot: TAiA2AManagerSlot;
begin
  Slot := AcquireSlot; // lanza AgentBusy si no hay cupo
  try
    // El servidor conduce la ejecucion: necesita que Run retorne de inmediato
    // para poder aplicar su propio timeout y publicar el task en working. En
    // modo sincrono Run haria Wait(INFINITE) y ademas lanzaria excepcion en
    // lugar de dejar el estado en el blackboard.
    Slot.Manager.Asynchronous := True;
    FTasksLock.Enter;
    try
      AInfo.Slot := Slot;
      AInfo.State := A2A_STATE_WORKING;
      AInfo.UpdatedAt := Now;
      AInfo.History.Add('user'#9 + AInput);
    finally
      FTasksLock.Leave;
    end;
    // Sobrecarga con siembra: garantiza Compile + ResetExecutionState, sin lo
    // cual un manager reutilizado del pool arrastraria el estado del task
    // anterior (blackboard, Output de nodos, NoCycles de los links).
    Slot.Manager.Run(AInput, nil);
  except
    on E: Exception do
    begin
      FTasksLock.Enter;
      try
        AInfo.Slot := nil;
        AInfo.State := A2A_STATE_FAILED;
        AInfo.ErrorText := E.Message;
        AInfo.UpdatedAt := Now;
      finally
        FTasksLock.Leave;
      end;
      ReleaseSlot(Slot);
      // No se propaga: un fallo de ejecucion es estado del task, no error de
      // transporte (la spec distingue una cosa de la otra).
    end;
  end;
end;

procedure TAiA2AServer.ResumeTask(AInfo: TAiA2ATaskInfo; const AInput: string);
var
  Slot: TAiA2AManagerSlot;
  ThreadID, NodeName: string;
begin
  FTasksLock.Enter;
  try
    Slot := AInfo.Slot;
    ThreadID := AInfo.ThreadID;
    NodeName := AInfo.SuspendNode;
  finally
    FTasksLock.Leave;
  end;

  if (Slot = nil) or (not Assigned(Slot.Manager)) or (NodeName = '') then
    raise EA2AServerError.CreateCode(A2A_ERR_UNSUPPORTED_OPERATION,
      'Task ' + AInfo.Id + ' cannot be resumed (no live execution)');

  try
    FTasksLock.Enter;
    try
      AInfo.State := A2A_STATE_WORKING;
      AInfo.SuspendReason := '';
      AInfo.SuspendContext := '';
      AInfo.UpdatedAt := Now;
      AInfo.History.Add('user'#9 + AInput);
    finally
      FTasksLock.Leave;
    end;
    Slot.Manager.ResumeThread(ThreadID, NodeName, AInput);
  except
    on E: Exception do
    begin
      FTasksLock.Enter;
      try
        AInfo.Slot := nil;
        AInfo.State := A2A_STATE_FAILED;
        AInfo.ErrorText := E.Message;
        AInfo.UpdatedAt := Now;
      finally
        FTasksLock.Leave;
      end;
      ReleaseSlot(Slot);
    end;
  end;
end;

// Espera activa acotada. No sostiene FTasksLock: bloquear el registry durante
// toda la corrida dejaria colgado cualquier GetTask concurrente.
procedure TAiA2AServer.WaitTask(AInfo: TAiA2ATaskInfo);
var
  Slot: TAiA2AManagerSlot;
  Waited: Integer;
begin
  FTasksLock.Enter;
  try
    Slot := AInfo.Slot;
  finally
    FTasksLock.Leave;
  end;
  if (Slot = nil) or (not Assigned(Slot.Manager)) then
    Exit;

  Waited := 0;
  while Slot.Manager.Busy and (Waited < FWaitTimeoutMs) do
  begin
    Sleep(15);
    Inc(Waited, 15);
  end;

  FTasksLock.Enter;
  try
    RefreshTask(AInfo);
  finally
    FTasksLock.Leave;
  end;
end;

// Descarta tasks terminados vencidos y acota el registry. Sin esto un servidor
// de larga vida acumula memoria sin techo.
procedure TAiA2AServer.PurgeTasks;
var
  Pair: TPair<string, TAiA2ATaskInfo>;
  Doomed: TList<string>;
  Oldest: TAiA2ATaskInfo;
  OldestId: string;
  Limit: TDateTime;
begin
  Doomed := TList<string>.Create;
  try
    FTasksLock.Enter;
    try
      Limit := IncSecond(Now, -FTaskTtlSeconds);
      for Pair in FTasks do
        if IsTerminal(Pair.Value.State) and (Pair.Value.UpdatedAt < Limit) then
          Doomed.Add(Pair.Key);
      for var Id in Doomed do
        FTasks.Remove(Id);

      // Tope duro: si aun sobra, cae el terminal mas antiguo.
      while (FMaxTasks > 0) and (FTasks.Count > FMaxTasks) do
      begin
        Oldest := nil;
        OldestId := '';
        for Pair in FTasks do
          if IsTerminal(Pair.Value.State) and ((Oldest = nil) or (Pair.Value.UpdatedAt < Oldest.UpdatedAt)) then
          begin
            Oldest := Pair.Value;
            OldestId := Pair.Key;
          end;
        if OldestId = '' then
          Break; // todos vivos: no hay nada que descartar
        FTasks.Remove(OldestId);
      end;
    finally
      FTasksLock.Leave;
    end;
  finally
    Doomed.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Serializacion JSON
// ---------------------------------------------------------------------------

function TAiA2AServer.EmitState(const ACanonical: string): string;
begin
  if FStateNaming = anLower then
    Result := ACanonical
  else
    Result := 'TASK_STATE_' + ACanonical.Replace('-', '_').ToUpper;
end;

function TAiA2AServer.EmitRole(const ACanonical: string): string;
begin
  if FStateNaming = anLower then
    Result := ACanonical
  else
    Result := 'ROLE_' + ACanonical.ToUpper;
end;

function TAiA2AServer.BuildAgentCard(const ABaseUrl: string): TJSONObject;
var
  Caps, Skill, Schemes, Bearer: TJSONObject;
  Skills, Modes: TJSONArray;
  LName, LDesc: string;
begin
  LName := FAgentName;
  if LName = '' then
    LName := Name; // nombre del componente como fallback
  LDesc := FAgentDescription;
  if (LDesc = '') and Assigned(FAgentManager) then
    LDesc := FAgentManager.Description;

  Result := TJSONObject.Create;
  Result.AddPair('name', LName);
  Result.AddPair('description', LDesc);
  Result.AddPair('version', FAgentVersion);

  if FWireEra = weV1 then
  begin
    // v1.0: la raiz NO tiene url ni protocolVersion ni preferredTransport.
    // Todo eso vive en supportedInterfaces[], que es lo que un cliente v1.0
    // consulta para saber a que URL hablar. Sin esto, no sabe donde llamar.
    var Ifaces := TJSONArray.Create;
    var Iface := TJSONObject.Create;
    Iface.AddPair('url', ABaseUrl);
    Iface.AddPair('protocolBinding', 'JSONRPC'); // es un string, no un enum
    Iface.AddPair('protocolVersion', '1.0.0');
    Ifaces.AddElement(Iface);
    Result.AddPair('supportedInterfaces', Ifaces);
  end
  else
  begin
    Result.AddPair('protocolVersion', '1.0.0');
    Result.AddPair('url', ABaseUrl);
    Result.AddPair('preferredTransport', 'JSONRPC');
  end;

  Caps := TJSONObject.Create;
  Caps.AddPair('streaming', TJSONBool.Create(False));
  Caps.AddPair('pushNotifications', TJSONBool.Create(False));
  Caps.AddPair('extendedAgentCard', TJSONBool.Create(False));
  // stateTransitionHistory no existe en AgentCapabilities de v1.0
  if FWireEra = weV03 then
    Caps.AddPair('stateTransitionHistory', TJSONBool.Create(False));
  Result.AddPair('capabilities', Caps);

  Modes := TJSONArray.Create;
  Modes.Add('text/plain');
  Result.AddPair('defaultInputModes', Modes);
  Modes := TJSONArray.Create;
  Modes.Add('text/plain');
  Result.AddPair('defaultOutputModes', Modes);

  if FApiKey <> '' then
  begin
    Bearer := TJSONObject.Create;
    Bearer.AddPair('type', 'http');
    Bearer.AddPair('scheme', 'bearer');
    Schemes := TJSONObject.Create;
    Schemes.AddPair('bearer', Bearer);
    Result.AddPair('securitySchemes', Schemes);
    var SecArr := TJSONArray.Create;
    var SecItem := TJSONObject.Create;
    if FWireEra = weV1 then
    begin
      // v1.0: SecurityRequirement tiene un campo 'schemes'
      var SchemesMap := TJSONObject.Create;
      SchemesMap.AddPair('bearer', TJSONArray.Create);
      SecItem.AddPair('schemes', SchemesMap);
    end
    else
      SecItem.AddPair('bearer', TJSONArray.Create);
    SecArr.AddElement(SecItem);
    if FWireEra = weV1 then
      Result.AddPair('securityRequirements', SecArr) // renombrado en v1.0
    else
      Result.AddPair('security', SecArr);
  end;

  // Un unico skill que representa la ejecucion del grafo completo
  Skills := TJSONArray.Create;
  Skill := TJSONObject.Create;
  Skill.AddPair('id', 'run-graph');
  Skill.AddPair('name', LName);
  Skill.AddPair('description', LDesc);
  Skill.AddPair('tags', TJSONArray.Create);
  Skills.AddElement(Skill);
  Result.AddPair('skills', Skills);

  if Assigned(FOnCustomizeCard) then
    FOnCustomizeCard(Self, Result);
end;

class function TAiA2AServer.ExtractTextFromParts(AMessage: TJSONObject): string;
var
  Parts: TJSONArray;
  V, DataVal: TJSONValue;
  PartObj: TJSONObject;
  SB: TStringBuilder;
  S: string;

  procedure Append(const AText: string);
  begin
    if AText = '' then
      Exit;
    if SB.Length > 0 then
      SB.Append(sLineBreak);
    SB.Append(AText);
  end;

begin
  Result := '';
  if not Assigned(AMessage) then
    Exit;
  if not AMessage.TryGetValue<TJSONArray>('parts', Parts) then
    Exit;
  SB := TStringBuilder.Create;
  try
    for V in Parts do
      if V is TJSONObject then
      begin
        PartObj := TJSONObject(V);
        // spec 1.0: {"text": "..."}; toleramos tambien el estilo 0.x
        // {"type":"text","text":"..."}
        if PartObj.TryGetValue<string>('text', S) then
          Append(S)
        // DataPart: el payload estructurado viaja como JSON crudo, de modo que
        // una orquestacion pueda pasar objetos y no solo prosa.
        else if PartObj.TryGetValue<TJSONValue>('data', DataVal) then
          Append(DataVal.ToJSON);
      end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TAiA2AServer.BuildTaskJson(AInfo: TAiA2ATaskInfo): TJSONObject;
var
  StatusObj, Artifact, PartObj, MsgObj: TJSONObject;
  Artifacts, PartsArr, HistArr: TJSONArray;
  StatusText: string;
  I: Integer;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', AInfo.Id);
  Result.AddPair('contextId', AInfo.ContextId);

  StatusObj := TJSONObject.Create;
  StatusObj.AddPair('state', EmitState(AInfo.State));
  StatusObj.AddPair('timestamp', DateToISO8601(TTimeZone.Local.ToUniversalTime(AInfo.UpdatedAt), True));

  // En input-required el mensaje de status lleva la pregunta al humano; en
  // failed lleva el error. Es lo que el cliente muestra o reenvia.
  StatusText := '';
  if AInfo.State = A2A_STATE_INPUT_REQUIRED then
  begin
    StatusText := AInfo.SuspendReason;
    if AInfo.SuspendContext <> '' then
      StatusText := Trim(StatusText + sLineBreak + AInfo.SuspendContext);
    if StatusText = '' then
      StatusText := 'The agent requires additional input to continue';
  end
  else if AInfo.ErrorText <> '' then
    StatusText := AInfo.ErrorText;

  if StatusText <> '' then
  begin
    MsgObj := TJSONObject.Create;
    MsgObj.AddPair('messageId', NewGuidStr);
    MsgObj.AddPair('role', EmitRole('agent'));
    MsgObj.AddPair('taskId', AInfo.Id);
    MsgObj.AddPair('contextId', AInfo.ContextId);
    var MParts := TJSONArray.Create;
    var MPart := TJSONObject.Create;
    MPart.AddPair('text', StatusText);
    MParts.AddElement(MPart);
    MsgObj.AddPair('parts', MParts);
    StatusObj.AddPair('message', MsgObj);
  end;
  Result.AddPair('status', StatusObj);

  Artifacts := TJSONArray.Create;
  if (AInfo.State = A2A_STATE_COMPLETED) and (AInfo.OutputText <> '') then
  begin
    Artifact := TJSONObject.Create;
    Artifact.AddPair('artifactId', AInfo.Id + '-result');
    Artifact.AddPair('name', 'result');
    PartsArr := TJSONArray.Create;
    PartObj := TJSONObject.Create;
    PartObj.AddPair('text', AInfo.OutputText);
    PartsArr.AddElement(PartObj);
    Artifact.AddPair('parts', PartsArr);
    Artifacts.AddElement(Artifact);
  end;
  Result.AddPair('artifacts', Artifacts);

  HistArr := TJSONArray.Create;
  for I := 0 to AInfo.History.Count - 1 do
  begin
    var Line := AInfo.History[I];
    var Sep := Pos(#9, Line);
    if Sep <= 0 then
      Continue;
    MsgObj := TJSONObject.Create;
    MsgObj.AddPair('messageId', AInfo.Id + '-h' + IntToStr(I));
    MsgObj.AddPair('role', EmitRole(Copy(Line, 1, Sep - 1)));
    MsgObj.AddPair('taskId', AInfo.Id);
    MsgObj.AddPair('contextId', AInfo.ContextId);
    var HParts := TJSONArray.Create;
    var HPart := TJSONObject.Create;
    HPart.AddPair('text', Copy(Line, Sep + 1, MaxInt));
    HParts.AddElement(HPart);
    MsgObj.AddPair('parts', HParts);
    HistArr.AddElement(MsgObj);
  end;
  Result.AddPair('history', HistArr);
end;

// ---------------------------------------------------------------------------
// Metodos JSON-RPC
// ---------------------------------------------------------------------------

function TAiA2AServer.DoSendMessage(AParams: TJSONObject): TJSONObject;
var
  MsgObj, ConfigObj: TJSONObject;
  InputText, TaskId, ContextId: string;
  Info: TAiA2ATaskInfo;
  Blocking: Boolean;
begin
  if not(Assigned(FAgentManager) or Assigned(FOnAcquireManager)) then
    raise EA2AServerError.CreateCode(-32603, 'No AgentManager assigned to this A2A server');

  MsgObj := nil;
  if Assigned(AParams) then
    AParams.TryGetValue<TJSONObject>('message', MsgObj);
  InputText := ExtractTextFromParts(MsgObj);

  // taskId/contextId viajan en el Message; toleramos verlos tambien sueltos en
  // params, que es como los mandan varios clientes de la era 0.x.
  TaskId := '';
  ContextId := '';
  if Assigned(MsgObj) then
  begin
    TaskId := MsgObj.GetValue<string>('taskId', '');
    ContextId := MsgObj.GetValue<string>('contextId', '');
  end;
  if Assigned(AParams) then
  begin
    if TaskId = '' then
      TaskId := AParams.GetValue<string>('taskId', '');
    if ContextId = '' then
      ContextId := AParams.GetValue<string>('contextId', '');
  end;

  Blocking := True;
  if Assigned(AParams) and AParams.TryGetValue<TJSONObject>('configuration', ConfigObj) then
    Blocking := ConfigObj.GetValue<Boolean>('blocking', True);

  PurgeTasks;

  if TaskId <> '' then
  begin
    FTasksLock.Enter;
    try
      Info := FindTask(TaskId);
      if Assigned(Info) then
        RefreshTask(Info);
    finally
      FTasksLock.Leave;
    end;
    if Info = nil then
      raise EA2AServerError.CreateCode(A2A_ERR_TASK_NOT_FOUND, 'Task not found: ' + TaskId);

    if Info.State = A2A_STATE_INPUT_REQUIRED then
      ResumeTask(Info, InputText)
    else if IsTerminal(Info.State) then
    begin
      // Nada que continuar: se devuelve el task tal cual, que es mas util para
      // el llamante que un error de transporte.
      FTasksLock.Enter;
      try
        Exit(BuildTaskJson(Info));
      finally
        FTasksLock.Leave;
      end;
    end;
  end
  else
  begin
    Info := NewTask(ContextId);
    StartTask(Info, InputText);
  end;

  // Si TaskId apuntaba a un task ya en working y no hubo reanudacion, no hay
  // nada nuevo que lanzar: se espera / refresca ese mismo task.
  if Blocking then
    WaitTask(Info)
  else
  begin
    FTasksLock.Enter;
    try
      RefreshTask(Info);
    finally
      FTasksLock.Leave;
    end;
  end;

  FTasksLock.Enter;
  try
    Result := BuildTaskJson(Info);
  finally
    FTasksLock.Leave;
  end;
end;

function TAiA2AServer.DoGetTask(AParams: TJSONObject): TJSONObject;
var
  Id: string;
  Info: TAiA2ATaskInfo;
begin
  Id := '';
  if Assigned(AParams) then
  begin
    Id := AParams.GetValue<string>('id', '');
    if Id = '' then
      Id := AParams.GetValue<string>('taskId', '');
  end;

  FTasksLock.Enter;
  try
    Info := FindTask(Id);
    if not Assigned(Info) then
      raise EA2AServerError.CreateCode(A2A_ERR_TASK_NOT_FOUND, 'Task not found: ' + Id);
    RefreshTask(Info); // el estado publicado sale siempre del grafo vivo
    Result := BuildTaskJson(Info);
  finally
    FTasksLock.Leave;
  end;
end;

function TAiA2AServer.DoCancelTask(AParams: TJSONObject): TJSONObject;
var
  Id: string;
  Info: TAiA2ATaskInfo;
  Slot: TAiA2AManagerSlot;
begin
  Id := '';
  if Assigned(AParams) then
  begin
    Id := AParams.GetValue<string>('id', '');
    if Id = '' then
      Id := AParams.GetValue<string>('taskId', '');
  end;

  FTasksLock.Enter;
  try
    Info := FindTask(Id);
    if not Assigned(Info) then
      raise EA2AServerError.CreateCode(A2A_ERR_TASK_NOT_FOUND, 'Task not found: ' + Id);
    RefreshTask(Info);
    if IsTerminal(Info.State) then
      raise EA2AServerError.CreateCode(A2A_ERR_TASK_NOT_CANCELABLE,
        'Task ' + Id + ' is already in a terminal state (' + Info.State + ')');
    Slot := Info.Slot;
  finally
    FTasksLock.Leave;
  end;

  // Aborta SOLO el manager de este task. Con un pool, abortar el manager de
  // diseno cancelaria el task de otro llamante.
  if Assigned(Slot) and Assigned(Slot.Manager) then
    Slot.Manager.Abort;

  FTasksLock.Enter;
  try
    Info.State := A2A_STATE_CANCELED;
    Info.UpdatedAt := Now;
    if Assigned(Info.Slot) then
    begin
      ReleaseSlot(Info.Slot);
      Info.Slot := nil;
    end;
    Result := BuildTaskJson(Info);
  finally
    FTasksLock.Leave;
  end;
end;

// ---------------------------------------------------------------------------
// Capa JSON-RPC / HTTP
// ---------------------------------------------------------------------------

function TAiA2AServer.RpcError(AId: TJSONValue; ACode: Integer; const AMsg: string): string;
var
  Resp, Err: TJSONObject;
begin
  Resp := TJSONObject.Create;
  try
    Resp.AddPair('jsonrpc', '2.0');
    Err := TJSONObject.Create;
    Err.AddPair('code', TJSONNumber.Create(ACode));
    Err.AddPair('message', AMsg);
    Resp.AddPair('error', Err);
    if Assigned(AId) then
      Resp.AddPair('id', TJSONValue(AId.Clone))
    else
      Resp.AddPair('id', TJSONNull.Create);
    Result := Resp.ToJSON;
  finally
    Resp.Free;
  end;
end;

function TAiA2AServer.RpcResult(AId: TJSONValue; AResult: TJSONObject): string;
var
  Resp: TJSONObject;
begin
  Resp := TJSONObject.Create;
  try
    Resp.AddPair('jsonrpc', '2.0');
    Resp.AddPair('result', AResult); // toma posesion
    if Assigned(AId) then
      Resp.AddPair('id', TJSONValue(AId.Clone))
    else
      Resp.AddPair('id', TJSONNull.Create);
    Result := Resp.ToJSON;
  finally
    Resp.Free;
  end;
end;

function TAiA2AServer.HandleJsonRpc(const ABody, ATraceParent: string): string;
var
  Root: TJSONValue;
  Req, Params: TJSONObject;
  Method: string;
  IdVal: TJSONValue;
  LSpan: TAiSpan;
begin
  Root := TJSONObject.ParseJSONValue(ABody);
  if not(Root is TJSONObject) then
  begin
    Root.Free;
    Exit(RpcError(nil, -32700, 'Parse error: body is not a JSON object'));
  end;
  Req := TJSONObject(Root);
  try
    Method := Req.GetValue<string>('method', '');
    IdVal := Req.GetValue('id');
    Params := Req.GetValue<TJSONObject>('params', nil);

    // Continua la traza del llamante si vino traceparent: sin esto cada salto
    // de una federacion arranca una traza nueva y el flujo no es observable.
    LSpan := AiSpanStart('a2a.server ' + Method, skServer, ATraceParent);
    AiSpanAttr(LSpan, 'a2a.method', Method);
    try
      // Metodos 1.0 + aliases 0.x (message/send, tasks/get, tasks/cancel)
      if SameText(Method, 'SendMessage') or SameText(Method, 'message/send') then
        Result := RpcResult(IdVal, WrapSendMessageResult(DoSendMessage(Params)))
      else if SameText(Method, 'GetTask') or SameText(Method, 'tasks/get') then
        Result := RpcResult(IdVal, DoGetTask(Params))
      else if SameText(Method, 'CancelTask') or SameText(Method, 'tasks/cancel') then
        Result := RpcResult(IdVal, DoCancelTask(Params))
      else if SameText(Method, 'SendStreamingMessage') or SameText(Method, 'SubscribeToTask') or
        SameText(Method, 'message/stream') or SameText(Method, 'tasks/resubscribe') then
        // La spec exige UnsupportedOperationError si capabilities.streaming=false
        Result := RpcError(IdVal, A2A_ERR_UNSUPPORTED_OPERATION, 'Streaming not supported by this agent')
      else
        Result := RpcError(IdVal, -32601, 'Method not found: ' + Method);
      AiSpanEnd(LSpan);
      LSpan := nil;
    except
      on E: EA2AServerError do
      begin
        AiSpanEnd(LSpan, E.Message);
        Result := RpcError(IdVal, E.Code, E.Message);
      end;
      on E: Exception do
      begin
        AiSpanEnd(LSpan, E.Message);
        // El motor de grafos senala la contencion con este texto; sin mapearlo
        // un fan-out concurrente devolveria -32603 en vez de AgentBusy.
        if E.Message.ToLower.Contains('busy') then
          Result := RpcError(IdVal, A2A_ERR_AGENT_BUSY, E.Message)
        else
          Result := RpcError(IdVal, -32603, E.Message);
      end;
    end;
  finally
    Req.Free;
  end;
end;

procedure TAiA2AServer.HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Body, BaseUrl, Scheme, AuthHdr: string;
  Card: TJSONObject;
  Allowed: Boolean;
begin
  AResponseInfo.ContentType := 'application/json; charset=utf-8';
  AResponseInfo.CharSet := 'utf-8';

  // --- Autorizacion ---
  AuthHdr := ARequestInfo.RawHeaders.Values['Authorization'];
  Allowed := True;
  if FApiKey <> '' then
    Allowed := SameText(Trim(AuthHdr), 'Bearer ' + FApiKey) or
      (ARequestInfo.RawHeaders.Values['X-API-Key'] = FApiKey);
  if Assigned(FOnAuthorize) then
    FOnAuthorize(Self, AuthHdr, Allowed);
  if not Allowed then
  begin
    AResponseInfo.ResponseNo := 401;
    AResponseInfo.CustomHeaders.Values['WWW-Authenticate'] := 'Bearer';
    AResponseInfo.ContentText := '{"error":"unauthorized"}';
    Exit;
  end;

  // Agent Card discovery (path 1.0 + alias legacy)
  if SameText(ARequestInfo.Command, 'GET') and
    (SameText(ARequestInfo.URI, '/.well-known/agent-card.json') or SameText(ARequestInfo.URI, '/.well-known/agent.json')) then
  begin
    if FPublicUrl <> '' then
      BaseUrl := FPublicUrl
    else
    begin
      // Detras de un reverse proxy TLS el esquema real llega por cabecera; sin
      // esto la card anunciaria http:// y el cliente no podria volver.
      Scheme := LowerCase(ARequestInfo.RawHeaders.Values['X-Forwarded-Proto']);
      if Scheme = '' then
        Scheme := 'http';
      BaseUrl := Scheme + '://' + ARequestInfo.Host + '/';
    end;
    Card := BuildAgentCard(BaseUrl);
    try
      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ContentText := Card.ToJSON;
    finally
      Card.Free;
    end;
    Exit;
  end;

  if SameText(ARequestInfo.Command, 'POST') then
  begin
    Body := '';
    if Assigned(ARequestInfo.PostStream) then
    begin
      ARequestInfo.PostStream.Position := 0;
      with TStringStream.Create('', TEncoding.UTF8) do
        try
          CopyFrom(ARequestInfo.PostStream, 0);
          Body := DataString;
        finally
          Free;
        end;
    end;
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ContentText := HandleJsonRpc(Body, ARequestInfo.RawHeaders.Values['traceparent']);
    Exit;
  end;

  AResponseInfo.ResponseNo := 404;
  AResponseInfo.ContentText := '{"error":"not found"}';
end;

end.
