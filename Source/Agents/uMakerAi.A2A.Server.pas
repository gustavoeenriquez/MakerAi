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
  System.SyncObjs, System.DateUtils, System.Threading,
  System.Net.HttpClient, System.Net.URLClient,
  IdContext, IdCustomHTTPServer, IdHTTPServer, IdGlobal, IdGlobalProtocols,
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
    PrevOnFinish: TAIAgentsOnFinish;
    HookedFinish: Boolean;
  end;

  // Config de push notification registrada para un task. La forma de v1.0 es
  // PLANA: {id, taskId, url, token, authentication:{scheme, credentials}}.
  TAiA2APushConfig = class
  public
    Id: string;
    TaskId: string;
    Url: string;
    Token: string;
    AuthScheme: string;
    AuthCredentials: string;
    function ToJson: TJSONObject;
  end;

  TAiA2ATaskInfo = class
  public
    Id: string;
    MessageId: string;  // messageId del mensaje que creo el task
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
    // Respuesta estructurada que dejo el grafo en el blackboard (ver
    // A2A_BB_ARTIFACTS / A2A_BB_MESSAGE). Vacias = respuesta de texto normal.
    ArtifactsJson: string;
    MessageJson: string;
    History: TStringList; // 'role<TAB>texto'
    constructor Create;
    destructor Destroy; override;
  end;

  // ---------------------------------------------------------------------------
  // Skills declaradas del Agent Card
  // ---------------------------------------------------------------------------
  // Una skill A2A es lo que un cliente lee para DECIDIR si este agente le sirve.
  // No es un punto de entrada: la spec no lleva selector de skill en
  // SendMessage, asi que las skills describen, no enrutan.
  //
  // Por eso NO se derivan de los nodos automaticamente: un grafo normal tiene
  // nodos llamados 'Nodo1'/'Nodo2' sin descripcion, y publicarlos seria ruido
  // que ademas insinua una granularidad de invocacion que no existe. Quien
  // quiera skills utiles las declara aqui; ver tambien PublishNodesAsSkills
  // para el caso en que los nodos SI traen descripcion en su Tool.
  TAiA2ASkill = class(TCollectionItem)
  private
    FId: string;
    FTitle: string;
    FDescription: string;
    FTags: string;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Id: string read FId write FId;
    // 'Title' y no 'Name': TCollectionItem no tiene Name, pero el campo del
    // Agent Card se llama 'name'.
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    // Separadas por coma. Se emiten como array JSON.
    property Tags: string read FTags write FTags;
  end;

  TAiA2ASkills = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TAiA2ASkill;
    procedure SetItem(AIndex: Integer; const Value: TAiA2ASkill);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TAiA2ASkill;
    // Atajo para declarar una skill en una linea desde codigo.
    function AddSkill(const AId, ATitle, ADescription: string;
      const ATags: string = ''): TAiA2ASkill;
    property Items[AIndex: Integer]: TAiA2ASkill read GetItem write SetItem; default;
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
    FPublishExtendedCard: Boolean;
    FSkills: TAiA2ASkills;
    FPublishNodesAsSkills: Boolean;
    FCardCacheSeconds: Integer;
    FAcquireTimeoutMs: Integer;
    FEnableStreaming: Boolean;
    FEnablePushNotifications: Boolean;
    FPushConfigs: TObjectList<TAiA2APushConfig>;
    FPushLock: TCriticalSection;

    // Ultima URL base servida, para poder responder GetExtendedAgentCard sin
    // tener el contexto HTTP delante.
    FLastBaseUrl: string;
    // Puente entre DoSendMessage y HandleJsonRpc para el brazo 'message'
    FLastMessageJson: string;
    FTasks: TObjectDictionary<string, TAiA2ATaskInfo>;
    FTasksLock: TCriticalSection;
    FSlots: TObjectList<TAiA2AManagerSlot>;
    FSlotsLock: TCriticalSection;
    FOnAcquireManager: TAiA2AAcquireManager;
    FOnCustomizeCard: TAiA2ACardEvent;
    FOnAuthorize: TAiA2AAuthEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetAgentManager(const Value: TAIAgentManager);
    procedure SetSkills(const Value: TAiA2ASkills);
    // TIdHTTPServer solo entiende autenticacion Basic. Con cualquier otro
    // esquema (Bearer, que es el que usa A2A) responde 401 el solo y
    // HttpCommand no llega a ejecutarse nunca. Marcandolo como atendido la
    // peticion sigue su curso y la autorizacion la decidimos nosotros.
    procedure ParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string;
      var VUsername, VPassword: string; var VHandled: Boolean);
    procedure HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    // --- pool ---
    function AcquireSlot: TAiA2AManagerSlot;
    procedure ReleaseSlot(ASlot: TAiA2AManagerSlot);
    procedure ClearSlots;
    // --- tasks ---
    function NewTask(const AContextId: string): TAiA2ATaskInfo;
    function FindTask(const AId: string): TAiA2ATaskInfo;
    // AIgnoreBusy: saltarse la comprobacion de Manager.Busy. Solo tiene
    // sentido desde OnGraphFinished, donde ya se sabe que el grafo acabo.
    procedure RefreshTask(AInfo: TAiA2ATaskInfo; AIgnoreBusy: Boolean = False); // exige FTasksLock tomado
    procedure StartTask(AInfo: TAiA2ATaskInfo; const AInput: string);
    procedure ResumeTask(AInfo: TAiA2ATaskInfo; const AInput: string; const AMessageId: string);
    procedure WaitTask(AInfo: TAiA2ATaskInfo);
    procedure PurgeTasks;
    // --- json ---
    function BuildAgentCard(const ABaseUrl: string): TJSONObject;
    // AHistoryLength: -1 todo el historial, 0 ninguno, N los ultimos N.
    function BuildTaskJson(AInfo: TAiA2ATaskInfo; AHistoryLength: Integer = -1): TJSONObject;
    // Envuelve el Task como SendMessageResponse cuando la era es v1.0.
    function WrapSendMessageResult(ATask: TJSONObject): TJSONObject;
    function WrapSendMessageAsMessage(const AMessageJson: string; ATask: TJSONObject): TJSONObject;
    // --- streaming SSE ---
    procedure SseWriteEvent(AContext: TIdContext; AId: TJSONValue; APayload: TJSONObject);
    function StreamStatusEvent(AInfo: TAiA2ATaskInfo): TJSONObject;
    // Emite los artifacts del task (solo tiene sentido si completo). Van
    // ANTES del status final: es el orden del ejemplo de la spec.
    procedure SseEmitArtifacts(AContext: TIdContext; AId: TJSONValue; AInfo: TAiA2ATaskInfo);
    procedure HandleStreamingRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo; const AMethod: string; AParams: TJSONObject; AId: TJSONValue);
    function HandleJsonRpc(const ABody, ATraceParent: string): string;
    function RpcError(AId: TJSONValue; ACode: Integer; const AMsg: string): string;
    function RpcResult(AId: TJSONValue; AResult: TJSONObject): string;
    function DoSendMessage(AParams: TJSONObject): TJSONObject;
    function DoGetTask(AParams: TJSONObject): TJSONObject;
    function DoCancelTask(AParams: TJSONObject): TJSONObject;
    function DoListTasks(AParams: TJSONObject): TJSONObject;
    // --- push notifications ---
    function DoCreatePushConfig(AParams: TJSONObject): TJSONObject;
    function DoGetPushConfig(AParams: TJSONObject): TJSONObject;
    function DoListPushConfigs(AParams: TJSONObject): TJSONObject;
    function DoDeletePushConfig(AParams: TJSONObject): TJSONObject;
    procedure DeliverPushNotifications(AInfo: TAiA2ATaskInfo);
    // Se engancha a TAIAgentManager.OnFinish: es el aviso de que un grafo
    // termino. Sustituye al sondeo, que era la unica forma de enterarse y
    // dependia de que alguien consultara el task.
    procedure RefreshLiveTasks;
    procedure OnGraphFinished(Sender: TObject; const Input, Output: string;
      Status: TAgentExecutionStatus; E: Exception);
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
    // Habilita GetExtendedAgentCard. La card extendida se construye igual que
    // la publica, asi que para diferenciarla hay que enriquecerla en
    // OnCustomizeCard (que recibe la card antes de publicarse).
    property PublishExtendedCard: Boolean read FPublishExtendedCard write FPublishExtendedCard default False;
    // Skills declaradas. Si esta vacia se publica una unica skill
    // 'run-graph' que representa la ejecucion del grafo entero.
    property Skills: TAiA2ASkills read FSkills write SetSkills;
    // Anade una skill por nodo del grafo que tenga descripcion (la de su
    // Tool). Los nodos sin descripcion se omiten a proposito: publicar
    // 'Nodo1' sin mas no ayuda a nadie a elegir agente.
    property PublishNodesAsSkills: Boolean read FPublishNodesAsSkills write FPublishNodesAsSkills default False;
    // Segundos de Cache-Control/max-age de la Agent Card (SHOULD de la spec).
    property CardCacheSeconds: Integer read FCardCacheSeconds write FCardCacheSeconds default 3600;
    // Cuanto se espera un hueco del pool antes de devolver AgentBusy.
    property AcquireTimeoutMs: Integer read FAcquireTimeoutMs write FAcquireTimeoutMs default 10000;
    // Habilita SendStreamingMessage y SubscribeToTask, y lo anuncia en
    // capabilities.streaming. Con False se rechazan con UnsupportedOperation.
    property EnableStreaming: Boolean read FEnableStreaming write FEnableStreaming default True;
    // Habilita el CRUD de configs y la entrega al webhook, y lo anuncia en
    // capabilities.pushNotifications.
    property EnablePushNotifications: Boolean read FEnablePushNotifications
      write FEnablePushNotifications default True;
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

  // Codigos de error JSON-RPC especificos A2A (spec 1.0, seccion 5.4)
  A2A_ERR_AGENT_BUSY = -32000;
  A2A_ERR_TASK_NOT_FOUND = -32001;
  A2A_ERR_TASK_NOT_CANCELABLE = -32002;
  A2A_ERR_PUSH_NOT_SUPPORTED = -32003;
  A2A_ERR_UNSUPPORTED_OPERATION = -32004;
  A2A_ERR_CONTENT_TYPE_NOT_SUPPORTED = -32005;
  A2A_ERR_INVALID_AGENT_RESPONSE = -32006;
  A2A_ERR_EXTENDED_CARD_NOT_CONFIGURED = -32007;
  A2A_ERR_EXTENSION_SUPPORT_REQUIRED = -32008;
  A2A_ERR_VERSION_NOT_SUPPORTED = -32009;
  A2A_ERR_INVALID_PARAMS = -32602;

  // ErrorInfo de google.rpc: la spec exige que error.data sea un ARRAY que lo
  // contenga, con la convencion ProtoJSON de @type.
  A2A_ERRORINFO_TYPE = 'type.googleapis.com/google.rpc.ErrorInfo';
  A2A_ERROR_DOMAIN = 'a2a-protocol.org';

  // Version del protocolo que anunciamos y aceptamos en la cabecera A2A-Version
  A2A_VERSION_HEADER = 'A2A-Version';
  A2A_VERSION = '1.0';

  // --- Canal blackboard <-> A2A -------------------------------------------
  // El servidor siembra estas claves antes de correr el grafo, para que los
  // nodos sepan en que conversacion estan:
  A2A_BB_MESSAGE_ID = 'A2A.MessageId';
  A2A_BB_TASK_ID = 'A2A.TaskId';
  A2A_BB_CONTEXT_ID = 'A2A.ContextId';
  // Y lee estas al terminar, para responder algo mas rico que texto plano:
  //   A2A.Artifacts -> array JSON de artifacts, se emite tal cual. Permite
  //                    DataPart, FilePart (bytes o url) y varios artifacts.
  //   A2A.Message   -> objeto Message JSON. Si esta, SendMessage responde
  //                    {"message": ...} en vez de {"task": ...}, que es el
  //                    otro brazo del oneof SendMessageResponse.
  A2A_BB_ARTIFACTS = 'A2A.Artifacts';
  A2A_BB_MESSAGE = 'A2A.Message';

// Convierte cualquiera de las dos formas ('TASK_STATE_INPUT_REQUIRED' o
// 'input-required') al canonico en minusculas. Publica porque el cliente la usa.
function A2ACanonicalState(const AValue: string): string;

procedure Register;

implementation

uses System.Hash, System.StrUtils, uMakerAi.Telemetry;

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

// -----------------------------------------------------------------------------
// Skills declaradas
// -----------------------------------------------------------------------------

function TAiA2ASkill.GetDisplayName: string;
begin
  if FId <> '' then
    Result := FId
  else
    Result := inherited GetDisplayName;
end;

procedure TAiA2ASkill.Assign(Source: TPersistent);
begin
  if Source is TAiA2ASkill then
  begin
    FId          := TAiA2ASkill(Source).Id;
    FTitle       := TAiA2ASkill(Source).Title;
    FDescription := TAiA2ASkill(Source).Description;
    FTags        := TAiA2ASkill(Source).Tags;
  end
  else
    inherited Assign(Source);
end;

constructor TAiA2ASkills.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TAiA2ASkill);
end;

function TAiA2ASkills.GetItem(AIndex: Integer): TAiA2ASkill;
begin
  Result := TAiA2ASkill(inherited Items[AIndex]);
end;

procedure TAiA2ASkills.SetItem(AIndex: Integer; const Value: TAiA2ASkill);
begin
  inherited Items[AIndex] := Value;
end;

function TAiA2ASkills.Add: TAiA2ASkill;
begin
  Result := TAiA2ASkill(inherited Add);
end;

function TAiA2ASkills.AddSkill(const AId, ATitle, ADescription: string;
  const ATags: string): TAiA2ASkill;
begin
  Result := Add;
  Result.Id := AId;
  Result.Title := ATitle;
  Result.Description := ADescription;
  Result.Tags := ATags;
end;

procedure TAiA2AServer.SetSkills(const Value: TAiA2ASkills);
begin
  FSkills.Assign(Value);
end;

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
  FCardCacheSeconds := 3600;
  FAcquireTimeoutMs := 10000;
  FEnableStreaming := True;
  FEnablePushNotifications := True;
  FSkills := TAiA2ASkills.Create(Self);
  FPushConfigs := TObjectList<TAiA2APushConfig>.Create(True);
  FPushLock := TCriticalSection.Create;
  FTasks := TObjectDictionary<string, TAiA2ATaskInfo>.Create([doOwnsValues]);
  FTasksLock := TCriticalSection.Create;
  FSlots := TObjectList<TAiA2AManagerSlot>.Create(True);
  FSlotsLock := TCriticalSection.Create;
  FHttpServer := TIdHTTPServer.Create(Self);
  FHttpServer.OnCommandGet := HttpCommand;
  FHttpServer.OnCommandOther := HttpCommand;
  FHttpServer.OnParseAuthentication := ParseAuthentication;
end;

destructor TAiA2AServer.Destroy;
begin
  Stop;
  FSkills.Free;
  FTasks.Free;
  FPushConfigs.Free;
  FPushLock.Free;
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

  // Ya no hay hilo de sondeo: la deteccion del fin de un grafo va por el evento
  // OnFinish del propio TAIAgentManager, que es determinista y no cuesta nada.
  // RefreshLiveTasks se conserva por si un integrador quiere forzar un repaso.
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
  Esperado: Integer;
begin
  Result := nil;
  Esperado := 0;
  repeat
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
    if Result <> nil then
      Break;
    Sleep(25);
    Inc(Esperado, 25);
  until Esperado >= FAcquireTimeoutMs;

  // Sin hueco libre: en vez de rechazar de inmediato se espera un poco. Un
  // pico de concurrencia es normal y los grafos suelen durar poco; devolver
  // AgentBusy a la primera convierte una espera en un error para el cliente.
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

// El otro brazo del oneof: responder con un Message en vez de con un Task,
// para agentes que contestan de una sin crear tarea.
function TAiA2AServer.WrapSendMessageAsMessage(const AMessageJson: string; ATask: TJSONObject): TJSONObject;
var
  MsgObj: TJSONValue;
begin
  MsgObj := TJSONObject.ParseJSONValue(AMessageJson);
  if not(MsgObj is TJSONObject) then
  begin
    MsgObj.Free;
    Exit(WrapSendMessageResult(ATask)); // JSON invalido: se responde el task
  end;
  ATask.Free; // no se usa en esta rama
  if FWireEra = weV03 then
    Exit(TJSONObject(MsgObj));
  Result := TJSONObject.Create;
  Result.AddPair('message', MsgObj);
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
procedure TAiA2AServer.RefreshTask(AInfo: TAiA2ATaskInfo; AIgnoreBusy: Boolean);
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

  // OJO con el orden del motor: TAIAgentManager dispara OnFinish y solo
  // DESPUES pone FBusy a 0. Un refresco lanzado desde ese evento veria el
  // manager como ocupado y se perderia la transicion final.
  if Mgr.Busy and not AIgnoreBusy then
  begin
    AInfo.State := A2A_STATE_WORKING;
    Exit;
  end;

  St := Mgr.Blackboard.GetStatus;
  // UpdatedAt solo se mueve cuando el estado CAMBIA de verdad. Sellarlo en cada
  // consulta tenia dos efectos malos: dos snapshots del mismo task salian con
  // timestamps distintos (y los clientes suscritos no podian deduplicarlos), y
  // el TTL de purga, que se mide contra este campo, no vencia nunca.
  var LCambio := AInfo.State <> StatusToA2AState(St);
  if LCambio then
  begin
    AInfo.State := StatusToA2AState(St);
    AInfo.UpdatedAt := Now;
  end;

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
    // Respuesta estructurada opcional dejada por el grafo
    AInfo.ArtifactsJson := Mgr.Blackboard.GetString(A2A_BB_ARTIFACTS);
    AInfo.MessageJson := Mgr.Blackboard.GetString(A2A_BB_MESSAGE);
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

  // Notificar a los webhooks registrados. El POST se hace en un TTask aparte,
  // asi que esto no bloquea aunque tengamos FTasksLock tomado.
  if LCambio then
    DeliverPushNotifications(AInfo);
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
    // Engancharse al fin del grafo para poder notificar sin sondear.
    if not Slot.HookedFinish then
    begin
      Slot.PrevOnFinish := Slot.Manager.OnFinish; // se encadena, no se pisa
      Slot.Manager.OnFinish := OnGraphFinished;
      Slot.HookedFinish := True;
    end;
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
    // anterior (blackboard, Output de nodos, NoCycles de los links). La siembra
    // corre DESPUES del reset, asi que estos valores sobreviven.
    var LMsgId := AInfo.MessageId;
    var LTaskId := AInfo.Id;
    var LCtxId := AInfo.ContextId;
    Slot.Manager.Run(AInput,
      procedure(B: TAIBlackboard)
      begin
        // El grafo necesita saber en que conversacion esta: sin esto un nodo
        // no puede distinguir turnos ni correlacionar con sistemas externos.
        B.SetString(A2A_BB_MESSAGE_ID, LMsgId);
        B.SetString(A2A_BB_TASK_ID, LTaskId);
        B.SetString(A2A_BB_CONTEXT_ID, LCtxId);
      end);
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

procedure TAiA2AServer.ResumeTask(AInfo: TAiA2ATaskInfo; const AInput: string; const AMessageId: string);
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
    // El grafo reanudado tiene que ver la metadata del turno NUEVO. Sin esto
    // un nodo que decide por el messageId repite la decision del turno
    // anterior: el que se suspendio volvia a suspenderse indefinidamente.
    if Assigned(Slot.Manager) then
    begin
      Slot.Manager.Blackboard.SetString(A2A_BB_MESSAGE_ID, AMessageId);
      Slot.Manager.Blackboard.SetString(A2A_BB_TASK_ID, AInfo.Id);
      Slot.Manager.Blackboard.SetString(A2A_BB_CONTEXT_ID, AInfo.ContextId);
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
  Caps.AddPair('streaming', TJSONBool.Create(FEnableStreaming));
  Caps.AddPair('pushNotifications', TJSONBool.Create(FEnablePushNotifications));
  Caps.AddPair('extendedAgentCard', TJSONBool.Create(FPublishExtendedCard));
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

  Skills := TJSONArray.Create;

  // 1. Las skills declaradas por el desarrollador mandan.
  for var I := 0 to FSkills.Count - 1 do
  begin
    var LSk := FSkills[I];
    if Trim(LSk.Id) = '' then
      Continue; // sin id no es una skill valida
    Skill := TJSONObject.Create;
    Skill.AddPair('id', LSk.Id);
    if LSk.Title <> '' then
      Skill.AddPair('name', LSk.Title)
    else
      Skill.AddPair('name', LSk.Id);
    Skill.AddPair('description', LSk.Description);
    var LTags := TJSONArray.Create;
    for var LTag in SplitString(LSk.Tags, ',') do
      if Trim(LTag) <> '' then
        LTags.Add(Trim(LTag));
    Skill.AddPair('tags', LTags);
    Skills.AddElement(Skill);
  end;

  // 2. Nodos con descripcion, solo si se pide. Los que no la tienen se omiten:
  //    publicar 'Nodo1' sin descripcion no ayuda a elegir agente.
  if FPublishNodesAsSkills and Assigned(FAgentManager) then
    for var LNode in FAgentManager.GetNodes do
    begin
      if not Assigned(LNode) or not Assigned(LNode.Tool) then
        Continue;
      if Trim(LNode.Tool.Description) = '' then
        Continue;
      Skill := TJSONObject.Create;
      Skill.AddPair('id', 'node-' + LNode.Name);
      Skill.AddPair('name', LNode.Name);
      Skill.AddPair('description', LNode.Tool.Description);
      Skill.AddPair('tags', TJSONArray.Create);
      Skills.AddElement(Skill);
    end;

  // 3. Si no quedo ninguna, la skill por defecto: ejecutar el grafo entero.
  //    La Agent Card NUNCA debe salir sin skills.
  if Skills.Count = 0 then
  begin
    Skill := TJSONObject.Create;
    Skill.AddPair('id', 'run-graph');
    Skill.AddPair('name', LName);
    Skill.AddPair('description', LDesc);
    Skill.AddPair('tags', TJSONArray.Create);
    Skills.AddElement(Skill);
  end;

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

function TAiA2AServer.BuildTaskJson(AInfo: TAiA2ATaskInfo; AHistoryLength: Integer): TJSONObject;
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
    // Determinista a proposito: dos snapshots del MISMO task tienen que salir
    // identicos. Con un GUID nuevo por llamada, dos clientes suscritos al mismo
    // task recibian eventos distintos y no podian deduplicar.
    MsgObj.AddPair('messageId', AInfo.Id + '-status');
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

  // Si el grafo dejo artifacts estructurados, mandan ellos: permiten DataPart,
  // FilePart (bytes o url) y varios artifacts, cosa que el texto plano no.
  Artifacts := nil;
  if (AInfo.State = A2A_STATE_COMPLETED) and (Trim(AInfo.ArtifactsJson) <> '') then
    Artifacts := TJSONArray(TJSONObject.ParseJSONValue(AInfo.ArtifactsJson));
  if not(Artifacts is TJSONArray) then
  begin
    Artifacts.Free;
    Artifacts := nil;
  end;

  if Artifacts = nil then
  begin
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
  end;
  Result.AddPair('artifacts', Artifacts);

  HistArr := TJSONArray.Create;
  // historyLength recorta por el final: 0 = sin historial, N = los N ultimos.
  var LDesde := 0;
  if AHistoryLength = 0 then
    LDesde := AInfo.History.Count
  else if (AHistoryLength > 0) and (AInfo.History.Count > AHistoryLength) then
    LDesde := AInfo.History.Count - AHistoryLength;
  for I := LDesde to AInfo.History.Count - 1 do
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
  MsgObj, ConfigObj, PushObj, PushParams: TJSONObject;
  InputText, TaskId, ContextId, MessageId: string;
  Info: TAiA2ATaskInfo;
  Blocking: Boolean;
  LHistLen: Integer;
begin
  if not(Assigned(FAgentManager) or Assigned(FOnAcquireManager)) then
    raise EA2AServerError.CreateCode(-32603, 'No AgentManager assigned to this A2A server');

  MsgObj := nil;
  if Assigned(AParams) then
    AParams.TryGetValue<TJSONObject>('message', MsgObj);
  // 'message' es obligatorio: sin el la peticion esta malformada y hay que
  // rechazarla, no crear un task con texto vacio (CORE-ERR-002).
  if not Assigned(MsgObj) then
    raise EA2AServerError.CreateCode(A2A_ERR_INVALID_PARAMS, 'SendMessage requires a "message" parameter');
  InputText := ExtractTextFromParts(MsgObj);

  // taskId/contextId viajan en el Message; toleramos verlos tambien sueltos en
  // params, que es como los mandan varios clientes de la era 0.x.
  TaskId := '';
  ContextId := '';
  MessageId := '';
  if Assigned(MsgObj) then
  begin
    TaskId := MsgObj.GetValue<string>('taskId', '');
    MessageId := MsgObj.GetValue<string>('messageId', '');
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
  ConfigObj := nil;
  LHistLen := -1;
  if Assigned(AParams) and AParams.TryGetValue<TJSONObject>('configuration', ConfigObj) then
  begin
    Blocking := ConfigObj.GetValue<Boolean>('blocking', True);
    LHistLen := ConfigObj.GetValue<Integer>('historyLength', -1);
    if LHistLen < 0 then
      LHistLen := ConfigObj.GetValue<Integer>('history_length', -1);
  end;

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

    // Si el llamante manda contextId, tiene que ser el del task. Un par
    // taskId/contextId cruzado es un error, no algo a ignorar (CORE-MULTI-006).
    if (ContextId <> '') and (Info.ContextId <> ContextId) then
      raise EA2AServerError.CreateCode(A2A_ERR_INVALID_PARAMS,
        Format('contextId "%s" does not belong to task "%s"', [ContextId, TaskId]));

    if Info.State = A2A_STATE_INPUT_REQUIRED then
      ResumeTask(Info, InputText, MessageId)
    else if IsTerminal(Info.State) then
      // La spec lo exige asi (CORE-SEND-002): continuar un task ya terminal es
      // UnsupportedOperation. Antes se devolvia el task tal cual, por parecer
      // mas util al llamante; el TCK lo marca como incumplimiento MUST.
      raise EA2AServerError.CreateCode(A2A_ERR_UNSUPPORTED_OPERATION,
        Format('Task "%s" is in a terminal state (%s) and cannot be continued', [TaskId, Info.State]));
  end
  else
  begin
    Info := NewTask(ContextId);
    Info.MessageId := MessageId;

    // El webhook puede venir en la propia peticion, no solo por el CRUD:
    // configuration.taskPushNotificationConfig registra la config para este
    // task. Es la via que usan los clientes para no hacer dos llamadas.
    if Assigned(ConfigObj) and ConfigObj.TryGetValue<TJSONObject>('taskPushNotificationConfig', PushObj) then
    begin
      PushParams := TJSONObject.Create;
      try
        PushParams.AddPair('taskId', Info.Id);
        PushParams.AddPair('config', TJSONObject(PushObj.Clone));
        DoCreatePushConfig(PushParams).Free;
      finally
        PushParams.Free;
      end;
    end;

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
    FLastMessageJson := Info.MessageJson;
    Result := BuildTaskJson(Info, LHistLen);
  finally
    FTasksLock.Leave;
  end;
end;

function TAiA2AServer.DoGetTask(AParams: TJSONObject): TJSONObject;
var
  Id: string;
  Info: TAiA2ATaskInfo;
  LHistLen: Integer;
begin
  Id := '';
  LHistLen := -1;
  if Assigned(AParams) then
  begin
    Id := AParams.GetValue<string>('id', '');
    if Id = '' then
      Id := AParams.GetValue<string>('taskId', '');
    LHistLen := AParams.GetValue<Integer>('historyLength', -1);
    if LHistLen < 0 then
      LHistLen := AParams.GetValue<Integer>('history_length', -1);
  end;

  FTasksLock.Enter;
  try
    Info := FindTask(Id);
    if not Assigned(Info) then
      raise EA2AServerError.CreateCode(A2A_ERR_TASK_NOT_FOUND, 'Task not found: ' + Id);
    RefreshTask(Info); // el estado publicado sale siempre del grafo vivo
    Result := BuildTaskJson(Info, LHistLen);
  finally
    FTasksLock.Leave;
  end;
end;

// ListTasks (v1.0). Filtra por contextId y/o estado, con paginado simple por
// pageSize. ListTasksResponse = {tasks, nextPageToken, pageSize, totalSize}.
function TAiA2AServer.DoListTasks(AParams: TJSONObject): TJSONObject;
var
  Pair: TPair<string, TAiA2ATaskInfo>;
  Arr: TJSONArray;
  FiltroCtx, FiltroEstado: string;
  PageSize, Total, Emitidos: Integer;
begin
  FiltroCtx := '';
  FiltroEstado := '';
  PageSize := 0;
  if Assigned(AParams) then
  begin
    FiltroCtx := AParams.GetValue<string>('contextId', '');
    // El filtro llega como literal de TaskState; se normaliza para poder
    // comparar con el estado interno sea cual sea la era del llamante.
    FiltroEstado := A2ACanonicalState(AParams.GetValue<string>('status', ''));
    PageSize := AParams.GetValue<Integer>('pageSize', 0);
  end;

  Arr := TJSONArray.Create;
  Total := 0;
  Emitidos := 0;
  FTasksLock.Enter;
  try
    for Pair in FTasks do
    begin
      RefreshTask(Pair.Value); // el listado tampoco debe mentir
      if (FiltroCtx <> '') and (Pair.Value.ContextId <> FiltroCtx) then
        Continue;
      if (FiltroEstado <> '') and (Pair.Value.State <> FiltroEstado) then
        Continue;
      Inc(Total);
      if (PageSize > 0) and (Emitidos >= PageSize) then
        Continue; // se cuenta en totalSize pero no se emite
      Arr.AddElement(BuildTaskJson(Pair.Value));
      Inc(Emitidos);
    end;
  finally
    FTasksLock.Leave;
  end;

  Result := TJSONObject.Create;
  Result.AddPair('tasks', Arr);
  Result.AddPair('totalSize', TJSONNumber.Create(Total));
  if PageSize > 0 then
    Result.AddPair('pageSize', TJSONNumber.Create(PageSize));
end;

{ TAiA2APushConfig }

function TAiA2APushConfig.ToJson: TJSONObject;
var
  Auth: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', Id);
  Result.AddPair('taskId', TaskId);
  Result.AddPair('url', Url);
  if Token <> '' then
    Result.AddPair('token', Token);
  if (AuthScheme <> '') or (AuthCredentials <> '') then
  begin
    Auth := TJSONObject.Create;
    Auth.AddPair('scheme', AuthScheme);
    Auth.AddPair('credentials', AuthCredentials);
    Result.AddPair('authentication', Auth);
  end;
end;

// -----------------------------------------------------------------------------
// Push notifications
// -----------------------------------------------------------------------------
// La forma de v1.0 es PLANA: TaskPushNotificationConfig = {tenant, id, taskId,
// url, token, authentication}. No hay un objeto pushNotificationConfig anidado
// como en la era 0.x. Se acepta igualmente el anidado {taskId, config:{...}}
// porque es lo que mandan varios clientes.

// Lee una clave tolerando las dos convenciones. El binding JSON-RPC usa
// camelCase, pero varios clientes -el propio TCK entre ellos- mandan los
// nombres del proto en snake_case.
function ParamStr2(AParams: TJSONObject; const ACamel, ASnake: string): string;
begin
  Result := '';
  if not Assigned(AParams) then
    Exit;
  Result := AParams.GetValue<string>(ACamel, '');
  if Result = '' then
    Result := AParams.GetValue<string>(ASnake, '');
end;
function TAiA2AServer.DoCreatePushConfig(AParams: TJSONObject): TJSONObject;
var
  Cfg: TAiA2APushConfig;
  Src, AuthObj: TJSONObject;
  TaskId: string;
begin
  if not FEnablePushNotifications then
    raise EA2AServerError.CreateCode(A2A_ERR_PUSH_NOT_SUPPORTED, 'Push notifications are not supported');
  if not Assigned(AParams) then
    raise EA2AServerError.CreateCode(A2A_ERR_INVALID_PARAMS, 'Missing params');

  TaskId := ParamStr2(AParams, 'taskId', 'task_id');
  // El cuerpo puede venir anidado en 'config' o plano en los propios params
  if not AParams.TryGetValue<TJSONObject>('config', Src) then
    Src := AParams;
  if TaskId = '' then
    TaskId := ParamStr2(Src, 'taskId', 'task_id');
  if TaskId = '' then
    raise EA2AServerError.CreateCode(A2A_ERR_INVALID_PARAMS, 'taskId is required');

  Cfg := TAiA2APushConfig.Create;
  Cfg.TaskId := TaskId;
  Cfg.Id := Src.GetValue<string>('id', '');
  if Cfg.Id = '' then
    Cfg.Id := NewGuidStr;
  Cfg.Url := Src.GetValue<string>('url', '');
  Cfg.Token := Src.GetValue<string>('token', '');
  if Src.TryGetValue<TJSONObject>('authentication', AuthObj) then
  begin
    Cfg.AuthScheme := AuthObj.GetValue<string>('scheme', '');
    Cfg.AuthCredentials := AuthObj.GetValue<string>('credentials', '');
  end;

  FPushLock.Enter;
  try
    // Registrar de nuevo el mismo id reemplaza: crear duplicados haria que el
    // webhook recibiera la misma notificacion dos veces.
    for var I := FPushConfigs.Count - 1 downto 0 do
      if (FPushConfigs[I].TaskId = Cfg.TaskId) and (FPushConfigs[I].Id = Cfg.Id) then
        FPushConfigs.Delete(I);
    FPushConfigs.Add(Cfg);
    Result := Cfg.ToJson;
  finally
    FPushLock.Leave;
  end;
end;

function TAiA2AServer.DoGetPushConfig(AParams: TJSONObject): TJSONObject;
var
  TaskId, Id: string;
begin
  if not FEnablePushNotifications then
    raise EA2AServerError.CreateCode(A2A_ERR_PUSH_NOT_SUPPORTED, 'Push notifications are not supported');
  TaskId := '';
  Id := '';
  TaskId := ParamStr2(AParams, 'taskId', 'task_id');
  if Assigned(AParams) then
    Id := AParams.GetValue<string>('id', '');

  FPushLock.Enter;
  try
    for var Cfg in FPushConfigs do
      if (Cfg.TaskId = TaskId) and (Cfg.Id = Id) then
        Exit(Cfg.ToJson);
  finally
    FPushLock.Leave;
  end;
  raise EA2AServerError.CreateCode(A2A_ERR_TASK_NOT_FOUND,
    Format('No push notification config "%s" for task "%s"', [Id, TaskId]));
end;

function TAiA2AServer.DoListPushConfigs(AParams: TJSONObject): TJSONObject;
var
  TaskId: string;
  Arr: TJSONArray;
begin
  if not FEnablePushNotifications then
    raise EA2AServerError.CreateCode(A2A_ERR_PUSH_NOT_SUPPORTED, 'Push notifications are not supported');
  TaskId := '';
  TaskId := ParamStr2(AParams, 'taskId', 'task_id');

  Arr := TJSONArray.Create;
  FPushLock.Enter;
  try
    for var Cfg in FPushConfigs do
      if (TaskId = '') or (Cfg.TaskId = TaskId) then
        Arr.AddElement(Cfg.ToJson);
  finally
    FPushLock.Leave;
  end;
  Result := TJSONObject.Create;
  Result.AddPair('configs', Arr);
end;

function TAiA2AServer.DoDeletePushConfig(AParams: TJSONObject): TJSONObject;
var
  TaskId, Id: string;
begin
  if not FEnablePushNotifications then
    raise EA2AServerError.CreateCode(A2A_ERR_PUSH_NOT_SUPPORTED, 'Push notifications are not supported');
  TaskId := '';
  Id := '';
  TaskId := ParamStr2(AParams, 'taskId', 'task_id');
  if Assigned(AParams) then
    Id := AParams.GetValue<string>('id', '');

  FPushLock.Enter;
  try
    for var I := FPushConfigs.Count - 1 downto 0 do
      if (FPushConfigs[I].TaskId = TaskId) and (FPushConfigs[I].Id = Id) then
        FPushConfigs.Delete(I);
  finally
    FPushLock.Leave;
  end;
  // Delete es IDEMPOTENTE: borrar algo que ya no esta no es un error.
  Result := TJSONObject.Create;
end;

// Entrega el estado del task a los webhooks registrados. Se llama al detectar
// un cambio de estado. El POST va en un TTask aparte: bloquear aqui pararia el
// refresco del task y, con el lock tomado, todo el registry.
// Refresca los tasks con ejecucion viva. Hace falta un vigilante activo: la
// entrega push se dispara al detectar un cambio de estado, y RefreshTask solo
// corria cuando un cliente preguntaba. Si nadie llamaba a GetTask, el task
// terminaba y el webhook no se enteraba nunca.
// Un grafo acaba de terminar. Se localiza su task y se refresca, lo que
// dispara la entrega push si el estado cambio. Antes esto dependia de un hilo
// que sondeaba cada 100 ms: funcionaba solo si alguien consultaba el task, asi
// que con blocking=false el webhook no se enteraba nunca.
procedure TAiA2AServer.OnGraphFinished(Sender: TObject; const Input, Output: string;
  Status: TAgentExecutionStatus; E: Exception);
var
  Pair: TPair<string, TAiA2ATaskInfo>;
  Afectado: TAiA2ATaskInfo;
  Previo: TAIAgentsOnFinish;
begin
  Afectado := nil;
  Previo := nil;
  FTasksLock.Enter;
  try
    for Pair in FTasks do
      if Assigned(Pair.Value.Slot) and (Pair.Value.Slot.Manager = Sender) then
      begin
        Afectado := Pair.Value;
        Previo := Pair.Value.Slot.PrevOnFinish;
        Break;
      end;
    if Assigned(Afectado) then
      // AIgnoreBusy: el motor dispara OnFinish ANTES de poner FBusy a 0,
      // asi que aqui el manager todavia se declara ocupado. Sin esto,
      // RefreshTask salia por la rama 'sigue trabajando' y nunca detectaba
      // el paso a completed: el webhook no se enteraba jamas.
      RefreshTask(Afectado, True);
  finally
    FTasksLock.Leave;
  end;

  // Encadenar el handler que el integrador pudiera tener puesto: apropiarse
  // del evento sin devolverlo romperia su codigo.
  if Assigned(Previo) then
    Previo(Sender, Input, Output, Status, E);
end;
procedure TAiA2AServer.RefreshLiveTasks;
var
  Pair: TPair<string, TAiA2ATaskInfo>;
  Vivos: TArray<TAiA2ATaskInfo>;
  Lista: TList<TAiA2ATaskInfo>;
begin
  Lista := TList<TAiA2ATaskInfo>.Create;
  try
    FTasksLock.Enter;
    try
      for Pair in FTasks do
        if Assigned(Pair.Value.Slot) and not IsTerminal(Pair.Value.State) then
          Lista.Add(Pair.Value);
      Vivos := Lista.ToArray;
      for var Info in Vivos do
        RefreshTask(Info); // dispara DeliverPushNotifications si cambio
    finally
      FTasksLock.Leave;
    end;
  finally
    Lista.Free;
  end;
end;
procedure TAiA2AServer.DeliverPushNotifications(AInfo: TAiA2ATaskInfo);
var
  Destinos: TArray<TAiA2APushConfig>;
  Payload: string;
  Lista: TList<TAiA2APushConfig>;
begin
  if not FEnablePushNotifications then
    Exit;

  Lista := TList<TAiA2APushConfig>.Create;
  try
    FPushLock.Enter;
    try
      for var Cfg in FPushConfigs do
        if Cfg.TaskId = AInfo.Id then
          Lista.Add(Cfg);
      Destinos := Lista.ToArray;
    finally
      FPushLock.Leave;
    end;
  finally
    Lista.Free;
  end;
  if Length(Destinos) = 0 then
    Exit;

  // El payload NO es el Task desnudo sino un StreamResponse, el mismo oneof
  // que usa el streaming: {task | message | statusUpdate | artifactUpdate}.
  // Aqui se manda el snapshot completo, asi que va por el brazo 'task'.
  var Envuelto := TJSONObject.Create;
  try
    Envuelto.AddPair('task', BuildTaskJson(AInfo));
    Payload := Envuelto.ToJSON;
  finally
    Envuelto.Free;
  end;

  for var Cfg in Destinos do
  begin
    var LUrl := Cfg.Url;
    var LScheme := Cfg.AuthScheme;
    var LCreds := Cfg.AuthCredentials;
    var LToken := Cfg.Token;
    if LUrl = '' then
      Continue;
    TTask.Run(
      procedure
      var
        Http: THTTPClient;
        Body: TStringStream;
        Headers: TNetHeaders;
      begin
        Http := THTTPClient.Create;
        Body := TStringStream.Create(Payload, TEncoding.UTF8);
        try
          Http.ConnectionTimeout := 5000;
          Http.ResponseTimeout := 5000;
          Http.ContentType := 'application/json';
          SetLength(Headers, 0);
          if LScheme <> '' then
            Headers := Headers + [TNameValuePair.Create('Authorization', LScheme + ' ' + LCreds)];
          if LToken <> '' then
            Headers := Headers + [TNameValuePair.Create('X-A2A-Notification-Token', LToken)];
          try
            Http.Post(LUrl, Body, nil, Headers);
          except
            // Un webhook caido no puede tumbar la ejecucion del agente.
          end;
        finally
          Body.Free;
          Http.Free;
        end;
      end);
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

// Razon canonica de ErrorInfo para cada codigo A2A: el nombre del error en
// UPPER_SNAKE_CASE y sin el sufijo "Error". Los errores estandar de JSON-RPC
// (-32600 y siguientes) no llevan razon.
function A2AReasonForCode(ACode: Integer): string;
begin
  case ACode of
    A2A_ERR_TASK_NOT_FOUND: Result := 'TASK_NOT_FOUND';
    A2A_ERR_TASK_NOT_CANCELABLE: Result := 'TASK_NOT_CANCELABLE';
    A2A_ERR_PUSH_NOT_SUPPORTED: Result := 'PUSH_NOTIFICATION_NOT_SUPPORTED';
    A2A_ERR_UNSUPPORTED_OPERATION: Result := 'UNSUPPORTED_OPERATION';
    A2A_ERR_CONTENT_TYPE_NOT_SUPPORTED: Result := 'CONTENT_TYPE_NOT_SUPPORTED';
    A2A_ERR_INVALID_AGENT_RESPONSE: Result := 'INVALID_AGENT_RESPONSE';
    A2A_ERR_EXTENDED_CARD_NOT_CONFIGURED: Result := 'EXTENDED_AGENT_CARD_NOT_CONFIGURED';
    A2A_ERR_EXTENSION_SUPPORT_REQUIRED: Result := 'EXTENSION_SUPPORT_REQUIRED';
    A2A_ERR_VERSION_NOT_SUPPORTED: Result := 'VERSION_NOT_SUPPORTED';
  else
    Result := ''; // error estandar de JSON-RPC: sin ErrorInfo
  end;
end;

function TAiA2AServer.RpcError(AId: TJSONValue; ACode: Integer; const AMsg: string): string;
var
  Resp, Err, Info: TJSONObject;
  Data: TJSONArray;
  Reason: string;
begin
  Resp := TJSONObject.Create;
  try
    Resp.AddPair('jsonrpc', '2.0');
    Err := TJSONObject.Create;
    Err.AddPair('code', TJSONNumber.Create(ACode));
    Err.AddPair('message', AMsg);

    // La spec exige ErrorInfo dentro de un array en error.data para los
    // errores propios de A2A (seccion 9.5).
    Reason := A2AReasonForCode(ACode);
    if Reason <> '' then
    begin
      Info := TJSONObject.Create;
      Info.AddPair('@type', A2A_ERRORINFO_TYPE);
      Info.AddPair('domain', A2A_ERROR_DOMAIN);
      Info.AddPair('reason', Reason);
      Data := TJSONArray.Create;
      Data.AddElement(Info);
      Err.AddPair('data', Data);
    end;

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
  LTaskJson: TJSONObject;
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
      begin
        // DoSendMessage deja en FLastMessageJson lo que el grafo haya puesto en
        // A2A.Message; si hay algo, la respuesta va por el brazo 'message'.
        FLastMessageJson := '';
        LTaskJson := DoSendMessage(Params);
        if FLastMessageJson <> '' then
          Result := RpcResult(IdVal, WrapSendMessageAsMessage(FLastMessageJson, LTaskJson))
        else
          Result := RpcResult(IdVal, WrapSendMessageResult(LTaskJson));
      end
      else if SameText(Method, 'GetTask') or SameText(Method, 'tasks/get') then
        Result := RpcResult(IdVal, DoGetTask(Params))
      else if SameText(Method, 'CancelTask') or SameText(Method, 'tasks/cancel') then
        Result := RpcResult(IdVal, DoCancelTask(Params))
      else if SameText(Method, 'ListTasks') or SameText(Method, 'tasks/list') then
        Result := RpcResult(IdVal, DoListTasks(Params))
      else if SameText(Method, 'GetExtendedAgentCard') or SameText(Method, 'agent/getExtendedCard') then
      begin
        // Solo si el integrador la habilito: la card extendida esta pensada
        // para clientes ya autenticados, con mas detalle que la publica.
        if not FPublishExtendedCard then
          Result := RpcError(IdVal, A2A_ERR_EXTENDED_CARD_NOT_CONFIGURED, 'Extended agent card is not configured')
        else
          Result := RpcResult(IdVal, BuildAgentCard(FLastBaseUrl));
      end
      else if SameText(Method, 'SendStreamingMessage') or SameText(Method, 'SubscribeToTask') or
        SameText(Method, 'message/stream') or SameText(Method, 'tasks/resubscribe') then
        // La spec exige UnsupportedOperationError si capabilities.streaming=false
        Result := RpcError(IdVal, A2A_ERR_UNSUPPORTED_OPERATION, 'Streaming not supported by this agent')
      else if SameText(Method, 'CreateTaskPushNotificationConfig') then
        Result := RpcResult(IdVal, DoCreatePushConfig(Params))
      else if SameText(Method, 'GetTaskPushNotificationConfig') then
        Result := RpcResult(IdVal, DoGetPushConfig(Params))
      else if SameText(Method, 'ListTaskPushNotificationConfigs') then
        Result := RpcResult(IdVal, DoListPushConfigs(Params))
      else if SameText(Method, 'DeleteTaskPushNotificationConfig') then
        Result := RpcResult(IdVal, DoDeletePushConfig(Params))
      else if Method.ToLower.Contains('pushnotification') then
        // Variantes que no implementamos: con capabilities.pushNotifications a
        // false la spec pide este codigo concreto, no un MethodNotFound.
        Result := RpcError(IdVal, A2A_ERR_PUSH_NOT_SUPPORTED, 'Push notification method not supported')
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

// -----------------------------------------------------------------------------
// Streaming SSE (binding JSON-RPC)
// -----------------------------------------------------------------------------
// Cada evento viaja como una linea 'data:' con el ENVOLTORIO JSON-RPC dentro,
// segun la seccion 9.4.2 de la spec:
//     data: {jsonrpc, id, result: <StreamResponse>}
// OJO: el binding REST manda el StreamResponse desnudo; el de JSON-RPC no.
// StreamResponse es un oneof: task | message | statusUpdate | artifactUpdate.

procedure TAiA2AServer.SseWriteEvent(AContext: TIdContext; AId: TJSONValue; APayload: TJSONObject);
var
  Env: TJSONObject;
begin
  Env := TJSONObject.Create;
  try
    Env.AddPair('jsonrpc', '2.0');
    if Assigned(AId) then
      Env.AddPair('id', TJSONValue(AId.Clone))
    else
      Env.AddPair('id', TJSONNull.Create);
    Env.AddPair('result', APayload); // toma posesion
    // El doble salto de linea es lo que cierra un evento en SSE
    AContext.Connection.IOHandler.Write(ToBytes('data: ' + Env.ToJSON + #10#10, IndyTextEncoding_UTF8));
  finally
    Env.Free;
  end;
end;

function TAiA2AServer.StreamStatusEvent(AInfo: TAiA2ATaskInfo): TJSONObject;
var
  Ev, StatusObj: TJSONObject;
begin
  StatusObj := TJSONObject.Create;
  StatusObj.AddPair('state', EmitState(AInfo.State));
  StatusObj.AddPair('timestamp', DateToISO8601(TTimeZone.Local.ToUniversalTime(AInfo.UpdatedAt), True));
  Ev := TJSONObject.Create;
  Ev.AddPair('taskId', AInfo.Id);
  Ev.AddPair('contextId', AInfo.ContextId);
  Ev.AddPair('status', StatusObj);
  Result := TJSONObject.Create;
  Result.AddPair('statusUpdate', Ev);
end;

procedure TAiA2AServer.ParseAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: string; var VUsername, VPassword: string;
  var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TAiA2AServer.SseEmitArtifacts(AContext: TIdContext; AId: TJSONValue;
  AInfo: TAiA2ATaskInfo);
var
  TaskJson, ArtEv, ArtPayload: TJSONObject;
  Artifacts: TJSONArray;
  I: Integer;
begin
  FTasksLock.Enter;
  try
    TaskJson := BuildTaskJson(AInfo);
  finally
    FTasksLock.Leave;
  end;
  try
    if TaskJson.TryGetValue<TJSONArray>('artifacts', Artifacts) then
      for I := 0 to Artifacts.Count - 1 do
      begin
        ArtEv := TJSONObject.Create;
        ArtEv.AddPair('taskId', AInfo.Id);
        ArtEv.AddPair('contextId', AInfo.ContextId);
        ArtEv.AddPair('artifact', TJSONObject(Artifacts.Items[I].Clone));
        ArtEv.AddPair('lastChunk', TJSONBool.Create(I = Artifacts.Count - 1));
        ArtPayload := TJSONObject.Create;
        ArtPayload.AddPair('artifactUpdate', ArtEv);
        SseWriteEvent(AContext, AId, ArtPayload);
      end;
  finally
    TaskJson.Free;
  end;
end;

procedure TAiA2AServer.HandleStreamingRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; const AMethod: string; AParams: TJSONObject; AId: TJSONValue);
var
  Info: TAiA2ATaskInfo;
  MsgObj, TaskJson, Payload, ArtEv, ArtPayload: TJSONObject;
  Artifacts: TJSONArray;
  InputText, TaskId, ContextId, MessageId, EstadoPrevio, Hdr: string;
  Esperado, I: Integer;
  Suscripcion: Boolean;
begin
  Suscripcion := SameText(AMethod, 'SubscribeToTask') or SameText(AMethod, 'tasks/resubscribe');

  // Resolver el task ANTES de abrir el stream: si hay que devolver un error,
  // se responde JSON normal. Una vez abierto el SSE ya no se puede.
  Info := nil;
  if Suscripcion then
  begin
    TaskId := '';
    if Assigned(AParams) then
      TaskId := AParams.GetValue<string>('id', '');
    FTasksLock.Enter;
    try
      Info := FindTask(TaskId);
      if Assigned(Info) then
        RefreshTask(Info);
    finally
      FTasksLock.Leave;
    end;
    if Info = nil then
    begin
      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ContentText := RpcError(AId, A2A_ERR_TASK_NOT_FOUND, 'Task not found: ' + TaskId);
      Exit;
    end;
    if IsTerminal(Info.State) then
    begin
      // Suscribirse a un task ya terminal es un error segun la spec
      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ContentText := RpcError(AId, A2A_ERR_UNSUPPORTED_OPERATION,
        'Task is already in a terminal state: ' + Info.State);
      Exit;
    end;
  end
  else
  begin
    MsgObj := nil;
    if Assigned(AParams) then
      AParams.TryGetValue<TJSONObject>('message', MsgObj);
    if not Assigned(MsgObj) then
    begin
      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ContentText := RpcError(AId, A2A_ERR_INVALID_PARAMS,
        'SendStreamingMessage requires a message parameter');
      Exit;
    end;
    InputText := ExtractTextFromParts(MsgObj);
    ContextId := MsgObj.GetValue<string>('contextId', '');
    MessageId := MsgObj.GetValue<string>('messageId', '');
    TaskId    := MsgObj.GetValue<string>('taskId', '');
    if Assigned(AParams) then
    begin
      if TaskId = '' then
        TaskId := AParams.GetValue<string>('taskId', '');
      if ContextId = '' then
        ContextId := AParams.GetValue<string>('contextId', '');
    end;
    PurgeTasks;
    try
      if TaskId <> '' then
      begin
        // Mismo contrato que SendMessage: con taskId se REANUDA, no se abre un
        // task nuevo. Sin esto un flujo human-in-the-loop por streaming perdia
        // el grafo suspendido en cada turno y empezaba de cero.
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

        // Un par taskId/contextId cruzado es error, no algo a ignorar.
        if (ContextId <> '') and (Info.ContextId <> ContextId) then
          raise EA2AServerError.CreateCode(A2A_ERR_INVALID_PARAMS,
            Format('contextId "%s" does not belong to task "%s"', [ContextId, TaskId]));

        if Info.State = A2A_STATE_INPUT_REQUIRED then
          ResumeTask(Info, InputText, MessageId)
        else if IsTerminal(Info.State) then
          raise EA2AServerError.CreateCode(A2A_ERR_UNSUPPORTED_OPERATION,
            Format('Task "%s" is in a terminal state (%s) and cannot be continued',
              [TaskId, Info.State]));
      end
      else
      begin
        Info := NewTask(ContextId);
        Info.MessageId := MessageId;
        StartTask(Info, InputText);
      end;
    except
      on E: EA2AServerError do
      begin
        AResponseInfo.ResponseNo := 200;
        AResponseInfo.ContentText := RpcError(AId, E.Code, E.Message);
        Exit;
      end;
    end;
  end;

  // --- A partir de aqui el stream esta abierto ---
  Hdr := 'HTTP/1.1 200 OK' + #13#10 + 'Content-Type: text/event-stream; charset=utf-8' + #13#10 +
    'Cache-Control: no-cache' + #13#10 + 'Connection: keep-alive' + #13#10 +
    'X-Accel-Buffering: no' + #13#10 + #13#10;
  AContext.Connection.IOHandler.Write(ToBytes(Hdr, IndyTextEncoding_UTF8));
  AResponseInfo.HeaderHasBeenWritten := True; // que Indy no escriba los suyos

  // 1. Snapshot inicial del task
  FTasksLock.Enter;
  try
    RefreshTask(Info);
    EstadoPrevio := Info.State;
    TaskJson := BuildTaskJson(Info);
  finally
    FTasksLock.Leave;
  end;
  Payload := TJSONObject.Create;
  Payload.AddPair('task', TaskJson);
  SseWriteEvent(AContext, AId, Payload);

  // 1b. El task puede haber llegado a su estado final ANTES de este primer
  // snapshot: un grafo rapido, o un resume que completa enseguida. Entonces no
  // queda ningun cambio que detectar y el bucle de abajo se quedaria dando
  // vueltas hasta agotar WaitTimeoutMs con el cliente esperando. Se cierra ya.
  //
  // input-required solo cierra si NO es una suscripcion: en
  // SendStreamingMessage significa "te toca a ti", pero quien hace
  // SubscribeToTask es un observador y tiene que seguir mirando las
  // transiciones posteriores.
  if IsTerminal(EstadoPrevio) or
    ((EstadoPrevio = A2A_STATE_INPUT_REQUIRED) and not Suscripcion) then
  begin
    if EstadoPrevio = A2A_STATE_COMPLETED then
      SseEmitArtifacts(AContext, AId, Info);
    FTasksLock.Enter;
    try
      Payload := StreamStatusEvent(Info);
    finally
      FTasksLock.Leave;
    end;
    SseWriteEvent(AContext, AId, Payload);
    AContext.Connection.Disconnect;
    Exit;
  end;

  // 2. Seguir la ejecucion y emitir cada cambio de estado
  Esperado := 0;
  while (Esperado < FWaitTimeoutMs) and AContext.Connection.Connected do
  begin
    Sleep(50);
    Inc(Esperado, 50);

    FTasksLock.Enter;
    try
      RefreshTask(Info);
      if Info.State <> EstadoPrevio then
      begin
        EstadoPrevio := Info.State;
        Payload := StreamStatusEvent(Info);
      end
      else
        Payload := nil;
    finally
      FTasksLock.Leave;
    end;

    if not Assigned(Payload) then
      Continue;

    // 3. Al completar, primero los artifacts y luego el status final.
    if EstadoPrevio = A2A_STATE_COMPLETED then
      SseEmitArtifacts(AContext, AId, Info);

    SseWriteEvent(AContext, AId, Payload);

    // input-required tambien cierra el stream: la pelota pasa al cliente
    if IsTerminal(EstadoPrevio) or (EstadoPrevio = A2A_STATE_INPUT_REQUIRED) then
      Break;
  end;

  AContext.Connection.Disconnect;
end;
procedure TAiA2AServer.HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Body, BaseUrl, Scheme, AuthHdr, VerHdr, CtHdr, CardJson, ETag, PeekMethod: string;
  Token: string;
  CardPublica: Boolean;
  PeekRoot: TJSONValue;
  Card: TJSONObject;
  Allowed: Boolean;
begin
  AResponseInfo.ContentType := 'application/json; charset=utf-8';
  AResponseInfo.CharSet := 'utf-8';

  // --- Autorizacion ---
  // La Agent Card base queda FUERA de la comprobacion de ApiKey: es un
  // documento de descubrimiento, y es justo donde el cliente lee QUE esquema
  // de seguridad tiene que usar. Protegerla es un circulo vicioso -para saber
  // como autenticarse habria que estar ya autenticado- y deja al agente
  // invisible para cualquier cliente que no traiga la clave de antemano.
  // Para publicar datos solo a clientes autenticados esta la extended card
  // (GetExtendedAgentCard), que si pasa por aqui.
  //
  // OnAuthorize SI se sigue llamando: si alguien quiere cerrar tambien la
  // card, es una decision suya y explicita.
  CardPublica := SameText(ARequestInfo.Command, 'GET') and
    (SameText(ARequestInfo.URI, '/.well-known/agent-card.json') or
     SameText(ARequestInfo.URI, '/.well-known/agent.json'));

  AuthHdr := ARequestInfo.RawHeaders.Values['Authorization'];
  Allowed := True;
  if (FApiKey <> '') and not CardPublica then
  begin
    Token := Trim(AuthHdr);
    if StartsText('Bearer ', Token) then
      Token := Trim(Copy(Token, Length('Bearer ') + 1, MaxInt))
    else
      Token := '';
    // El secreto se compara EXACTO. Con SameText una clave 'AbC' la validaba
    // 'abc', que es tirar a la basura buena parte de su entropia; solo el
    // nombre del esquema va sin distinguir mayusculas.
    Allowed := (Token <> '') and (Token = FApiKey);
    if not Allowed then
      Allowed := ARequestInfo.RawHeaders.Values['X-API-Key'] = FApiKey;
  end;
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
    FLastBaseUrl := BaseUrl;
    Card := BuildAgentCard(BaseUrl);
    try
      CardJson := Card.ToJSON;
    finally
      Card.Free;
    end;

    // La spec recomienda (SHOULD) que la card sea cacheable: sin esto cada
    // cliente la vuelve a pedir en cada descubrimiento.
    ETag := '"' + THashSHA2.GetHashString(CardJson).Substring(0, 32) + '"';
    AResponseInfo.CustomHeaders.Values['Cache-Control'] :=
      Format('public, max-age=%d', [FCardCacheSeconds]);
    AResponseInfo.CustomHeaders.Values['ETag'] := ETag;

    // Revalidacion condicional: si el cliente ya tiene esta version, 304.
    if SameText(Trim(ARequestInfo.RawHeaders.Values['If-None-Match']), ETag) then
    begin
      AResponseInfo.ResponseNo := 304;
      AResponseInfo.ContentText := '';
      Exit;
    end;

    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ContentText := CardJson;
    Exit;
  end;

  if SameText(ARequestInfo.Command, 'POST') then
  begin
    // --- Validaciones de cabecera exigidas por la spec (seccion 5.4) ---
    // Version: si el cliente declara una que no soportamos, hay que rechazarla
    // con su codigo propio. Ausente = se asume compatible (clientes 0.x).
    VerHdr := Trim(ARequestInfo.RawHeaders.Values[A2A_VERSION_HEADER]);
    if (VerHdr <> '') and (not SameText(VerHdr, A2A_VERSION)) and (not VerHdr.StartsWith('1.')) then
    begin
      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ContentText := RpcError(nil, A2A_ERR_VERSION_NOT_SUPPORTED,
        Format('A2A version "%s" is not supported; this agent speaks %s', [VerHdr, A2A_VERSION]));
      Exit;
    end;

    // Content-Type: tiene que ser JSON. Devolver ParseError aqui seria enganoso,
    // porque el cuerpo puede estar perfectamente bien formado.
    CtHdr := LowerCase(ARequestInfo.ContentType);
    if (CtHdr <> '') and (not CtHdr.Contains('json')) then
    begin
      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ContentText := RpcError(nil, A2A_ERR_CONTENT_TYPE_NOT_SUPPORTED,
        Format('Content-Type "%s" is not supported; use application/json', [ARequestInfo.ContentType]));
      Exit;
    end;

    // Se recuerda para poder construir la card en GetExtendedAgentCard, que
    // llega por JSON-RPC y no trae contexto de URL.
    if FLastBaseUrl = '' then
      if FPublicUrl <> '' then
        FLastBaseUrl := FPublicUrl
      else
        FLastBaseUrl := 'http://' + ARequestInfo.Host + '/';
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
    // Los metodos de streaming no responden JSON sino un SSE, asi que se
    // desvian antes de entrar al despachador normal.
    if FEnableStreaming then
    begin
      PeekRoot := TJSONObject.ParseJSONValue(Body);
      try
        if PeekRoot is TJSONObject then
        begin
          PeekMethod := TJSONObject(PeekRoot).GetValue<string>('method', '');
          if SameText(PeekMethod, 'SendStreamingMessage') or SameText(PeekMethod, 'message/stream') or
            SameText(PeekMethod, 'SubscribeToTask') or SameText(PeekMethod, 'tasks/resubscribe') then
          begin
            HandleStreamingRequest(AContext, ARequestInfo, AResponseInfo, PeekMethod,
              TJSONObject(PeekRoot).GetValue<TJSONObject>('params', nil),
              TJSONObject(PeekRoot).GetValue('id'));
            Exit;
          end;
        end;
      finally
        PeekRoot.Free;
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
