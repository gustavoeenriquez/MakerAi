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
// TAiA2AServer (MVP, spec A2A 1.0): expone un TAIAgentManager como agente A2A.
//
// - Agent Card en GET /.well-known/agent-card.json (y el alias legacy
//   /.well-known/agent.json).
// - Binding JSON-RPC 2.0 en POST /: metodos 1.0 (SendMessage, GetTask,
//   CancelTask) con tolerancia a los aliases 0.x (message/send, tasks/get,
//   tasks/cancel).
// - SendMessage ejecuta el grafo del TAIAgentManager vinculado (bloqueante,
//   con timeout) y mapea el status final:
//     esCompleted            -> TASK_STATE_COMPLETED (artifact con el output)
//     esSuspended            -> TASK_STATE_INPUT_REQUIRED (human-in-the-loop)
//     esError / esTimeout    -> TASK_STATE_FAILED
//     esAborted              -> TASK_STATE_CANCELED
// - Registry de tasks en memoria para GetTask/CancelTask.
// - Sin streaming ni push notifications en este MVP (capabilities=false; la
//   spec exige rechazar SendStreamingMessage con UnsupportedOperationError).
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.SyncObjs, System.DateUtils,
  IdContext, IdCustomHTTPServer, IdHTTPServer,
  uMakerAi.Agents;

type
  TAiA2ATaskInfo = class
  public
    Id: string;
    ContextId: string;
    State: string; // TASK_STATE_*
    OutputText: string;
    ErrorText: string;
    CreatedAt: TDateTime;
  end;

  TAiA2AServer = class(TComponent)
  private
    FHttpServer: TIdHTTPServer;
    FAgentManager: TAIAgentManager;
    FPort: Integer;
    FActive: Boolean;
    FAgentName: string;
    FAgentDescription: string;
    FAgentVersion: string;
    FRunTimeoutMs: Integer;
    FTasks: TObjectDictionary<string, TAiA2ATaskInfo>;
    FTasksLock: TCriticalSection;
    procedure SetActive(const Value: Boolean);
    procedure SetAgentManager(const Value: TAIAgentManager);
    procedure HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    function BuildAgentCard(const ABaseUrl: string): TJSONObject;
    function BuildTaskJson(AInfo: TAiA2ATaskInfo): TJSONObject;
    function HandleJsonRpc(const ABody: string): string;
    function RpcError(AId: TJSONValue; ACode: Integer; const AMsg: string): string;
    function RpcResult(AId: TJSONValue; AResult: TJSONObject): string;
    function DoSendMessage(AParams: TJSONObject): TJSONObject;
    function DoGetTask(AParams: TJSONObject): TJSONObject;
    function DoCancelTask(AParams: TJSONObject): TJSONObject;
    function FindTask(const AId: string): TAiA2ATaskInfo;
    class function ExtractTextFromParts(AMessage: TJSONObject): string; static;
    class function StatusToA2AState(AStatus: TAgentExecutionStatus): string; static;
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
    // Timeout de espera del run del grafo por SendMessage (ms)
    property RunTimeoutMs: Integer read FRunTimeoutMs write FRunTimeoutMs default 60000;
  end;

procedure Register;

implementation

uses uMakerAi.Telemetry;

const
  // Codigos de error JSON-RPC especificos A2A (rango servidor)
  A2A_ERR_TASK_NOT_FOUND = -32001;
  A2A_ERR_UNSUPPORTED_OPERATION = -32004;
  A2A_ERR_AGENT_BUSY = -32000;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiA2AServer]);
end;

{ TAiA2AServer }

constructor TAiA2AServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort := 8280;
  FAgentVersion := '1.0.0';
  FRunTimeoutMs := 60000;
  FTasks := TObjectDictionary<string, TAiA2ATaskInfo>.Create([doOwnsValues]);
  FTasksLock := TCriticalSection.Create;
  FHttpServer := TIdHTTPServer.Create(Self);
  FHttpServer.OnCommandGet := HttpCommand;
  FHttpServer.OnCommandOther := HttpCommand;
end;

destructor TAiA2AServer.Destroy;
begin
  Stop;
  FTasks.Free;
  FTasksLock.Free;
  inherited;
end;

procedure TAiA2AServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FAgentManager) then
    FAgentManager := nil;
end;

procedure TAiA2AServer.SetAgentManager(const Value: TAIAgentManager);
begin
  if FAgentManager <> Value then
  begin
    FAgentManager := Value;
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

function TAiA2AServer.BuildAgentCard(const ABaseUrl: string): TJSONObject;
var
  Caps, Skill: TJSONObject;
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
  Result.AddPair('protocolVersion', '1.0.0');
  Result.AddPair('name', LName);
  Result.AddPair('description', LDesc);
  Result.AddPair('url', ABaseUrl);
  Result.AddPair('version', FAgentVersion);

  Caps := TJSONObject.Create;
  Caps.AddPair('streaming', TJSONBool.Create(False));
  Caps.AddPair('pushNotifications', TJSONBool.Create(False));
  Caps.AddPair('extendedAgentCard', TJSONBool.Create(False));
  Result.AddPair('capabilities', Caps);

  Modes := TJSONArray.Create;
  Modes.Add('text/plain');
  Result.AddPair('defaultInputModes', Modes);
  Modes := TJSONArray.Create;
  Modes.Add('text/plain');
  Result.AddPair('defaultOutputModes', Modes);

  // MVP: un unico skill que representa la ejecucion del grafo completo
  Skills := TJSONArray.Create;
  Skill := TJSONObject.Create;
  Skill.AddPair('id', 'run-graph');
  Skill.AddPair('name', LName);
  Skill.AddPair('description', LDesc);
  Skill.AddPair('tags', TJSONArray.Create);
  Skills.AddElement(Skill);
  Result.AddPair('skills', Skills);
end;

class function TAiA2AServer.StatusToA2AState(AStatus: TAgentExecutionStatus): string;
begin
  case AStatus of
    esCompleted: Result := 'TASK_STATE_COMPLETED';
    esSuspended: Result := 'TASK_STATE_INPUT_REQUIRED'; // human-in-the-loop del grafo
    esAborted:   Result := 'TASK_STATE_CANCELED';
    esRunning:   Result := 'TASK_STATE_WORKING';
  else
    Result := 'TASK_STATE_FAILED'; // esError, esTimeout, esUnknown
  end;
end;

class function TAiA2AServer.ExtractTextFromParts(AMessage: TJSONObject): string;
var
  Parts: TJSONArray;
  V: TJSONValue;
  PartObj: TJSONObject;
  SB: TStringBuilder;
  S: string;
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
        begin
          if SB.Length > 0 then
            SB.Append(sLineBreak);
          SB.Append(S);
        end;
      end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TAiA2AServer.BuildTaskJson(AInfo: TAiA2ATaskInfo): TJSONObject;
var
  StatusObj, Artifact, PartObj: TJSONObject;
  Artifacts, PartsArr: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', AInfo.Id);
  Result.AddPair('contextId', AInfo.ContextId);

  StatusObj := TJSONObject.Create;
  StatusObj.AddPair('state', AInfo.State);
  StatusObj.AddPair('timestamp', DateToISO8601(TTimeZone.Local.ToUniversalTime(AInfo.CreatedAt), True));
  if AInfo.ErrorText <> '' then
  begin
    var MsgObj := TJSONObject.Create;
    MsgObj.AddPair('role', 'ROLE_AGENT');
    var MParts := TJSONArray.Create;
    var MPart := TJSONObject.Create;
    MPart.AddPair('text', AInfo.ErrorText);
    MParts.AddElement(MPart);
    MsgObj.AddPair('parts', MParts);
    StatusObj.AddPair('message', MsgObj);
  end;
  Result.AddPair('status', StatusObj);

  Artifacts := TJSONArray.Create;
  if (AInfo.State = 'TASK_STATE_COMPLETED') and (AInfo.OutputText <> '') then
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
  Result.AddPair('history', TJSONArray.Create);
end;

function TAiA2AServer.FindTask(const AId: string): TAiA2ATaskInfo;
begin
  FTasksLock.Enter;
  try
    if not FTasks.TryGetValue(AId, Result) then
      Result := nil;
  finally
    FTasksLock.Leave;
  end;
end;

function TAiA2AServer.DoSendMessage(AParams: TJSONObject): TJSONObject;
var
  MsgObj: TJSONObject;
  InputText: string;
  Info: TAiA2ATaskInfo;
  G: TGUID;
  Waited: Integer;
  FinalStatus: TAgentExecutionStatus;
begin
  if not Assigned(FAgentManager) then
    raise Exception.Create('No AgentManager assigned to this A2A server');

  MsgObj := nil;
  if Assigned(AParams) then
    AParams.TryGetValue<TJSONObject>('message', MsgObj);
  InputText := ExtractTextFromParts(MsgObj);

  if FAgentManager.Busy then
    raise Exception.Create('BUSY: agent is already executing a task');

  Info := TAiA2ATaskInfo.Create;
  CreateGUID(G);
  Info.Id := GUIDToString(G).Replace('{', '').Replace('}', '').ToLower;
  CreateGUID(G);
  Info.ContextId := GUIDToString(G).Replace('{', '').Replace('}', '').ToLower;
  Info.State := 'TASK_STATE_WORKING';
  Info.CreatedAt := Now;
  FTasksLock.Enter;
  try
    FTasks.Add(Info.Id, Info);
  finally
    FTasksLock.Leave;
  end;

  // MVP bloqueante: ejecutar el grafo y esperar el resultado (con timeout).
  // Los grafos sin eventos de UI asignados no requieren bombear Synchronize.
  FAgentManager.Run(InputText);
  Waited := 0;
  while FAgentManager.Busy and (Waited < FRunTimeoutMs) do
  begin
    Sleep(25);
    Inc(Waited, 25);
  end;

  if FAgentManager.Busy then
  begin
    // Timeout de espera: el grafo sigue corriendo; el estado real llegara a
    // GetTask cuando termine (MVP: se marca FAILED por timeout de espera).
    Info.State := 'TASK_STATE_FAILED';
    Info.ErrorText := 'A2A wait timeout after ' + IntToStr(FRunTimeoutMs) + ' ms';
  end
  else
  begin
    FinalStatus := FAgentManager.Blackboard.GetStatus;
    Info.State := StatusToA2AState(FinalStatus);
    if (FinalStatus = esCompleted) and Assigned(FAgentManager.EndNode) then
      Info.OutputText := FAgentManager.EndNode.Output;
    if FinalStatus in [esError, esTimeout] then
      Info.ErrorText := FAgentManager.Blackboard.GetString('Execution.ErrorMessage');
  end;

  Result := BuildTaskJson(Info);
end;

function TAiA2AServer.DoGetTask(AParams: TJSONObject): TJSONObject;
var
  Id: string;
  Info: TAiA2ATaskInfo;
begin
  Id := '';
  if Assigned(AParams) then
    Id := AParams.GetValue<string>('id', '');
  Info := FindTask(Id);
  if not Assigned(Info) then
    raise Exception.Create('TASK_NOT_FOUND: ' + Id);
  Result := BuildTaskJson(Info);
end;

function TAiA2AServer.DoCancelTask(AParams: TJSONObject): TJSONObject;
var
  Id: string;
  Info: TAiA2ATaskInfo;
begin
  Id := '';
  if Assigned(AParams) then
    Id := AParams.GetValue<string>('id', '');
  Info := FindTask(Id);
  if not Assigned(Info) then
    raise Exception.Create('TASK_NOT_FOUND: ' + Id);
  if Info.State = 'TASK_STATE_WORKING' then
  begin
    if Assigned(FAgentManager) then
      FAgentManager.Abort;
    Info.State := 'TASK_STATE_CANCELED';
  end;
  Result := BuildTaskJson(Info);
end;

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

function TAiA2AServer.HandleJsonRpc(const ABody: string): string;
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

    LSpan := AiSpanStart('a2a.server ' + Method, skServer);
    AiSpanAttr(LSpan, 'a2a.method', Method);
    try
      // Metodos 1.0 + aliases 0.x (message/send, tasks/get, tasks/cancel)
      if SameText(Method, 'SendMessage') or SameText(Method, 'message/send') then
        Result := RpcResult(IdVal, DoSendMessage(Params))
      else if SameText(Method, 'GetTask') or SameText(Method, 'tasks/get') then
        Result := RpcResult(IdVal, DoGetTask(Params))
      else if SameText(Method, 'CancelTask') or SameText(Method, 'tasks/cancel') then
        Result := RpcResult(IdVal, DoCancelTask(Params))
      else if SameText(Method, 'SendStreamingMessage') or SameText(Method, 'SubscribeToTask') then
        // La spec exige UnsupportedOperationError si capabilities.streaming=false
        Result := RpcError(IdVal, A2A_ERR_UNSUPPORTED_OPERATION, 'Streaming not supported by this agent')
      else
        Result := RpcError(IdVal, -32601, 'Method not found: ' + Method);
      AiSpanEnd(LSpan);
      LSpan := nil;
    except
      on E: Exception do
      begin
        AiSpanEnd(LSpan, E.Message);
        if E.Message.StartsWith('TASK_NOT_FOUND') then
          Result := RpcError(IdVal, A2A_ERR_TASK_NOT_FOUND, E.Message)
        else if E.Message.StartsWith('BUSY') then
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
  Body: string;
  Card: TJSONObject;
  BaseUrl: string;
begin
  AResponseInfo.ContentType := 'application/json; charset=utf-8';
  AResponseInfo.CharSet := 'utf-8';

  // Agent Card discovery (path 1.0 + alias legacy)
  if SameText(ARequestInfo.Command, 'GET') and
    (SameText(ARequestInfo.URI, '/.well-known/agent-card.json') or
     SameText(ARequestInfo.URI, '/.well-known/agent.json')) then
  begin
    BaseUrl := 'http://' + ARequestInfo.Host + '/';
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
    AResponseInfo.ContentText := HandleJsonRpc(Body);
    Exit;
  end;

  AResponseInfo.ResponseNo := 404;
  AResponseInfo.ContentText := '{"error":"not found"}';
end;

end.
