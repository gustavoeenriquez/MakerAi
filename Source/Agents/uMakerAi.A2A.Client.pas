// MIT License
//
// MakerAI - Cliente A2A (Agent-to-Agent Protocol, Linux Foundation)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.A2A.Client;

// -----------------------------------------------------------------------------
// TAiA2AClient (spec A2A 1.0): consume agentes A2A remotos.
//
// - FetchAgentCard: GET /.well-known/agent-card.json (con fallback al alias
//   legacy /.well-known/agent.json). El campo url de la card pasa a ser el
//   endpoint JSON-RPC: un agente de terceros puede publicar la card en un host
//   y atender el RPC en otra ruta.
// - SendText / SendTextEx: SendMessage con un TextPart. SendTextEx admite
//   taskId y contextId, que es lo que permite continuar una conversacion o
//   responder a un task en input-required.
// - GetTask / CancelTask.
// - Tolerancia de literales: los estados y roles se leen tanto en forma
//   protobuf (TASK_STATE_COMPLETED / ROLE_AGENT) como en forma JSON-RPC
//   (completed / agent), porque el ecosistema usa ambas.
// - Telemetria: spans 'a2a.client <metodo>' (skClient) y propagacion del
//   traceparent W3C al agente remoto.
//
// TAiA2ARemoteAgentTool federa un nodo de grafo contra un agente remoto. Si el
// agente remoto pide input humano (input-required), el nodo LOCAL se suspende
// en vez de fallar: al reanudarlo, la respuesta viaja al task remoto y el flujo
// continua donde estaba. Es lo que hace que un human-in-the-loop cruce la
// federacion en vez de morir en el borde.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Net.HttpClient, System.Net.URLClient,
  uMakerAi.Agents, uMakerAi.Agents.Attributes,
  uMakerAi.Tools.Functions, uMakerAi.Chat.Messages;

type
  EA2AClientException = class(Exception);
  // El agente remoto respondio, pero el task no termino bien. Se distingue del
  // error de transporte para que un orquestador pueda decidir si reintentar.
  EA2ARemoteTaskError = class(EA2AClientException);

  TAiA2ATaskState = (tsUnknown, tsSubmitted, tsWorking, tsInputRequired, tsCompleted, tsCanceled, tsFailed, tsRejected,
    tsAuthRequired);

  TAiA2AClient = class(TComponent)
  private
    FUrl: string;      // base del agente, p.ej. http://localhost:8280
    FEndpoint: string; // endpoint JSON-RPC efectivo (lo fija la Agent Card)
    FTimeout: Integer; // ms
    FRetryCount: Integer;
    FApiKey: string;
    FLastTaskId: string;
    FLastContextId: string;
    FLastState: string;
    FLastTaskState: TAiA2ATaskState;
    FLastStatusMessage: string;
    function JsonRpcCall(const AMethod: string; AParams: TJSONObject): TJSONObject;
    function ResolveEndpoint: string;
    function BuildHeaders(const ATraceParent: string): TNetHeaders;
    procedure ReadTaskStatus(ATask: TJSONObject);
  public
    constructor Create(AOwner: TComponent); override;

    // Descarga y devuelve el Agent Card (el llamador libera el objeto). Como
    // efecto util fija Endpoint con el campo url de la card.
    function FetchAgentCard: TJSONObject;

    // Envia un mensaje de texto y devuelve el Task resultante (el llamador
    // libera el objeto). El texto de los artifacts queda en AOutputText.
    function SendText(const AText: string; out AOutputText: string): TJSONObject;
    // Igual, pero continuando un task existente: con ATaskId de un task en
    // input-required, AText es la respuesta humana que lo reanuda.
    function SendTextEx(const AText, ATaskId, AContextId: string; out AOutputText: string; ABlocking: Boolean = True)
      : TJSONObject;

    function GetTask(const ATaskId: string): TJSONObject;
    function CancelTask(const ATaskId: string): TJSONObject;

    // Extrae el texto de los artifacts de un Task ya obtenido.
    class function ArtifactsText(ATask: TJSONObject): string; static;
    // Normaliza el result de SendMessage a un objeto con forma de Task,
    // tolerando el wrapper {task}/{message} de v1.0 y el Task plano de 0.x.
    class function UnwrapSendMessageResult(ARawResult: TJSONObject): TJSONObject; static;

    // Ultimo task enviado (conveniencia)
    property LastTaskId: string read FLastTaskId;
    property LastContextId: string read FLastContextId;
    // Literal crudo tal como lo mando el agente remoto.
    property LastState: string read FLastState;
    // Estado normalizado, independiente de la forma del literal.
    property LastTaskState: TAiA2ATaskState read FLastTaskState;
    // Texto de status.message: la pregunta al humano en input-required, o el
    // detalle del error en failed.
    property LastStatusMessage: string read FLastStatusMessage;
    // Endpoint JSON-RPC efectivo (Url + '/' hasta que la card diga otra cosa).
    property Endpoint: string read ResolveEndpoint;
  published
    property Url: string read FUrl write FUrl;
    property Timeout: Integer read FTimeout write FTimeout default 60000;
    // Reintentos ante fallo de transporte (no ante task fallido), con espera
    // creciente. 0 = sin reintento.
    property RetryCount: Integer read FRetryCount write FRetryCount default 2;
    // Si no esta vacio se envia como Authorization: Bearer <ApiKey>.
    property ApiKey: string read FApiKey write FApiKey;
  end;

  // Federacion de agentes: asignar como Tool de un TAIAgentsNode para que ese
  // nodo delegue su entrada en un agente A2A remoto. El input del nodo viaja
  // como mensaje de texto y el output es el texto de los artifacts del task.
  [TToolAttribute('A2ARemoteAgent', 'Delega la entrada del nodo en un agente A2A remoto y devuelve el texto de sus artifacts',
    'A2A')]
  TAiA2ARemoteAgentTool = class(TAiToolBase)
  private
    [TToolParameterAttribute('Agent URL', 'URL base del agente A2A remoto (ej: http://host:8280)', '')]
    FAgentUrl: string;
    [TToolParameterAttribute('Timeout ms', 'Timeout de la llamada remota en milisegundos', '60000')]
    FTimeoutMs: Integer;
    [TToolParameterAttribute('Suspend on input required',
      'Si el agente remoto pide input humano, suspende el nodo local en vez de fallar', 'True')]
    FSuspendOnInputRequired: Boolean;
    [TToolParameterAttribute('Api Key', 'Bearer token del agente remoto', '')]
    FApiKey: string;
    FClient: TAiA2AClient;
    function GetClient: TAiA2AClient;
    function StateKey(ANode: TAIAgentsNode): string;
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AgentUrl: string read FAgentUrl write FAgentUrl;
    property TimeoutMs: Integer read FTimeoutMs write FTimeoutMs default 60000;
    property SuspendOnInputRequired: Boolean read FSuspendOnInputRequired write FSuspendOnInputRequired default True;
    property ApiKey: string read FApiKey write FApiKey;
  end;

  // Expone un agente A2A remoto como HERRAMIENTA de chat: al asignarle un
  // TAiFunctions, el LLM puede invocar al agente igual que a cualquier otra
  // funcion. Complementa a TAiA2ARemoteAgentTool, que hace lo mismo pero para
  // un nodo de grafo en vez de para un chat.
  TAiA2AAgentTool = class(TComponent)
  private
    FFunctions: TAiFunctions;
    FAgentUrl: string;
    FToolName: string;
    FDescription: string;
    FApiKey: string;
    FTimeoutMs: Integer;
    FItem: TFunctionActionItem;
    procedure SetFunctions(const Value: TAiFunctions);
    procedure DoToolAction(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    // Registra la funcion en el TAiFunctions asignado. Se llama solo al
    // asignar Functions; se expone por si hay que rehacerlo tras cambiar
    // ToolName o Description en runtime.
    procedure RegisterTool;
    // Rellena ToolName y Description desde la Agent Card del agente remoto.
    // Asi el LLM ve la descripcion que el propio agente publica de si mismo.
    procedure DiscoverFromCard;
  published
    property Functions: TAiFunctions read FFunctions write SetFunctions;
    property AgentUrl: string read FAgentUrl write FAgentUrl;
    // Nombre con el que el LLM ve la herramienta. Si se deja vacio se usa
    // 'ask_remote_agent'.
    property ToolName: string read FToolName write FToolName;
    property Description: string read FDescription write FDescription;
    property ApiKey: string read FApiKey write FApiKey;
    property TimeoutMs: Integer read FTimeoutMs write FTimeoutMs default 60000;
  end;

// Normaliza cualquiera de las dos formas del literal ('TASK_STATE_INPUT_REQUIRED'
// o 'input-required') al enum.
function A2AParseState(const AValue: string): TAiA2ATaskState;

procedure Register;

implementation

uses System.Math, System.StrUtils, uMakerAi.Telemetry, uMakerAi.Agents.EngineRegistry;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiA2AClient, TAiA2AAgentTool]);
end;

function A2AParseState(const AValue: string): TAiA2ATaskState;
var
  S: string;
begin
  S := Trim(AValue).ToLower;
  if S.StartsWith('task_state_') then
    S := S.Substring(Length('task_state_'));
  S := S.Replace('_', '-');

  if S = 'submitted' then
    Result := tsSubmitted
  else if S = 'working' then
    Result := tsWorking
  else if S = 'input-required' then
    Result := tsInputRequired
  else if S = 'completed' then
    Result := tsCompleted
  else if S = 'canceled' then
    Result := tsCanceled
  else if (S = 'failed') or (S = 'error') then
    Result := tsFailed
  else if S = 'rejected' then
    Result := tsRejected
  else if S = 'auth-required' then
    Result := tsAuthRequired
  else
    Result := tsUnknown;
end;

{ TAiA2AClient }

constructor TAiA2AClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeout := 60000;
  FRetryCount := 2;
  FLastTaskState := tsUnknown;
end;

function TAiA2AClient.ResolveEndpoint: string;
begin
  if FEndpoint <> '' then
    Result := FEndpoint
  else
    Result := FUrl.TrimRight(['/']) + '/';
end;

function TAiA2AClient.BuildHeaders(const ATraceParent: string): TNetHeaders;
begin
  SetLength(Result, 0);
  if ATraceParent <> '' then
    Result := Result + [TNameValuePair.Create('traceparent', ATraceParent)];
  if FApiKey <> '' then
    Result := Result + [TNameValuePair.Create('Authorization', 'Bearer ' + FApiKey)];
end;

function TAiA2AClient.FetchAgentCard: TJSONObject;
var
  Http: THTTPClient;
  Resp: IHTTPResponse;
  BaseUrl, Content, CardUrl, S: string;
  V, VCard: TJSONValue;
  Ifaces: TJSONArray;
  IfaceObj: TJSONObject;
  LSpan: TAiSpan;
begin
  Result := nil;
  BaseUrl := FUrl.TrimRight(['/']);
  LSpan := AiSpanStart('a2a.client agent-card', skClient);
  AiSpanAttr(LSpan, 'a2a.url', BaseUrl);
  Http := THTTPClient.Create;
  try
    try
      Http.ConnectionTimeout := FTimeout;
      Http.ResponseTimeout := FTimeout;
      Resp := Http.Get(BaseUrl + '/.well-known/agent-card.json', nil, BuildHeaders(AiSpanTraceParent(LSpan)));
      if Resp.StatusCode <> 200 then
        Resp := Http.Get(BaseUrl + '/.well-known/agent.json', nil, BuildHeaders(AiSpanTraceParent(LSpan)));
      // alias legacy
      if Resp.StatusCode <> 200 then
        raise EA2AClientException.CreateFmt('Agent card not found (HTTP %d)', [Resp.StatusCode]);
      Content := Resp.ContentAsString(TEncoding.UTF8);
      V := TJSONObject.ParseJSONValue(Content);
      if not(V is TJSONObject) then
      begin
        V.Free;
        raise EA2AClientException.Create('Agent card is not a JSON object');
      end;
      Result := TJSONObject(V);

      // La card manda sobre la URL base: el endpoint RPC puede no ser la raiz.
      // En v1.0 la URL vive en supportedInterfaces[]; el campo 'url' de la raiz
      // es de la era 0.x y ya no existe en el AgentCard de la spec.
      CardUrl := '';
      if Result.TryGetValue<TJSONArray>('supportedInterfaces', Ifaces) then
        for VCard in Ifaces do
          if VCard is TJSONObject then
          begin
            IfaceObj := TJSONObject(VCard);
            // Nos interesa el binding JSON-RPC; si no dice cual, se acepta.
            S := IfaceObj.GetValue<string>('protocolBinding', '');
            if (S = '') or SameText(S, 'JSONRPC') or S.ToUpper.Contains('JSONRPC') then
            begin
              CardUrl := IfaceObj.GetValue<string>('url', '');
              if CardUrl <> '' then
                Break;
            end;
          end;
      if CardUrl = '' then
        CardUrl := Result.GetValue<string>('url', ''); // fallback era 0.x
      if CardUrl <> '' then
        FEndpoint := CardUrl;

      AiSpanEnd(LSpan);
      LSpan := nil;
    except
      on E: Exception do
      begin
        AiSpanEnd(LSpan, E.Message);
        raise;
      end;
    end;
  finally
    Http.Free;
  end;
end;

function TAiA2AClient.JsonRpcCall(const AMethod: string; AParams: TJSONObject): TJSONObject;
var
  Http: THTTPClient;
  Req: TJSONObject;
  Body: TStringStream;
  Resp: IHTTPResponse;
  Root: TJSONValue;
  RespObj, ErrObj: TJSONObject;
  ResultVal: TJSONValue;
  LSpan: TAiSpan;
  Attempt: Integer;
  LastErr: string;
  Payload: string;
begin
  Result := nil;
  Req := TJSONObject.Create;
  LSpan := AiSpanStart('a2a.client ' + AMethod, skClient);
  AiSpanAttr(LSpan, 'a2a.method', AMethod);
  try
    try
      Req.AddPair('jsonrpc', '2.0');
      Req.AddPair('id', TJSONNumber.Create(1));
      Req.AddPair('method', AMethod);
      if Assigned(AParams) then
        Req.AddPair('params', AParams); // toma posesion
      Payload := Req.ToJSON;

      Attempt := 0;
      LastErr := '';
      while True do
      begin
        Http := THTTPClient.Create;
        try
          Http.ConnectionTimeout := FTimeout;
          Http.ResponseTimeout := FTimeout;
          Http.ContentType := 'application/json';
          Body := TStringStream.Create(Payload, TEncoding.UTF8);
          try
            try
              Resp := Http.Post(ResolveEndpoint, Body, nil, BuildHeaders(AiSpanTraceParent(LSpan)));
              LastErr := '';
            except
              on E: Exception do
              begin
                Resp := nil;
                LastErr := E.Message;
              end;
            end;
          finally
            Body.Free;
          end;
        finally
          Http.Free;
        end;

        if Assigned(Resp) and (Resp.StatusCode = 200) then
          Break;

        if Assigned(Resp) then
          LastErr := Format('HTTP %d: %s', [Resp.StatusCode, Resp.ContentAsString(TEncoding.UTF8)]);

        // Solo se reintenta el transporte. Un task fallido no es un fallo de
        // red y repetirlo duplicaria trabajo del agente remoto.
        Inc(Attempt);
        if Attempt > FRetryCount then
          raise EA2AClientException.Create(LastErr);
        Sleep(Min(200 * Attempt, 2000));
      end;

      Root := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8));
      if not(Root is TJSONObject) then
      begin
        Root.Free;
        raise EA2AClientException.Create('Invalid JSON-RPC response');
      end;
      RespObj := TJSONObject(Root);
      try
        if RespObj.TryGetValue<TJSONObject>('error', ErrObj) then
          raise EA2ARemoteTaskError.Create('A2A error: ' + ErrObj.ToJSON);
        if RespObj.TryGetValue('result', ResultVal) and (ResultVal is TJSONObject) then
          Result := TJSONObject(ResultVal.Clone)
        else
          raise EA2AClientException.Create('A2A response has no result object');
      finally
        RespObj.Free;
      end;
      AiSpanEnd(LSpan);
      LSpan := nil;
    except
      on E: Exception do
      begin
        AiSpanEnd(LSpan, E.Message);
        raise;
      end;
    end;
  finally
    Req.Free;
  end;
end;

class function TAiA2AClient.ArtifactsText(ATask: TJSONObject): string;
var
  Artifacts, AParts: TJSONArray;
  ArtObj, PObj: TJSONObject;
  AV, PV, DataVal: TJSONValue;
  SB: TStringBuilder;
  S: string;
begin
  Result := '';
  if not Assigned(ATask) then
    Exit;
  SB := TStringBuilder.Create;
  try
    if ATask.TryGetValue<TJSONArray>('artifacts', Artifacts) then
      for AV in Artifacts do
        if AV is TJSONObject then
        begin
          ArtObj := TJSONObject(AV);
          if ArtObj.TryGetValue<TJSONArray>('parts', AParts) then
            for PV in AParts do
              if (PV is TJSONObject) then
              begin
                PObj := TJSONObject(PV);
                if PObj.TryGetValue<string>('text', S) then
                begin
                  if SB.Length > 0 then
                    SB.Append(sLineBreak);
                  SB.Append(S);
                end
                else if PObj.TryGetValue<TJSONValue>('data', DataVal) then
                begin
                  if SB.Length > 0 then
                    SB.Append(sLineBreak);
                  SB.Append(DataVal.ToJSON);
                end;
              end;
        end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

// Lee id/contextId/status del Task y deja el estado normalizado a mano.
procedure TAiA2AClient.ReadTaskStatus(ATask: TJSONObject);
var
  StatusObj, MsgObj: TJSONObject;
  Parts: TJSONArray;
  PV: TJSONValue;
  S: string;
begin
  FLastTaskId := '';
  FLastContextId := '';
  FLastState := '';
  FLastTaskState := tsUnknown;
  FLastStatusMessage := '';
  if not Assigned(ATask) then
    Exit;

  FLastTaskId := ATask.GetValue<string>('id', '');
  FLastContextId := ATask.GetValue<string>('contextId', '');

  if ATask.TryGetValue<TJSONObject>('status', StatusObj) then
  begin
    FLastState := StatusObj.GetValue<string>('state', '');
    FLastTaskState := A2AParseState(FLastState);
    if StatusObj.TryGetValue<TJSONObject>('message', MsgObj) and MsgObj.TryGetValue<TJSONArray>('parts', Parts) then
      for PV in Parts do
        if (PV is TJSONObject) and TJSONObject(PV).TryGetValue<string>('text', S) then
        begin
          if FLastStatusMessage <> '' then
            FLastStatusMessage := FLastStatusMessage + sLineBreak;
          FLastStatusMessage := FLastStatusMessage + S;
        end;
  end;
end;

function TAiA2AClient.SendTextEx(const AText, ATaskId, AContextId: string; out AOutputText: string; ABlocking: Boolean)
  : TJSONObject;
var
  Params, Msg, PartObj, Config, Raw: TJSONObject;
  Parts: TJSONArray;
  G: TGUID;
begin
  AOutputText := '';

  Params := TJSONObject.Create;
  Msg := TJSONObject.Create;
  Params.AddPair('message', Msg);
  CreateGUID(G);
  Msg.AddPair('messageId', GUIDToString(G).Replace('{', '').Replace('}', '').ToLower);
  Msg.AddPair('role', 'ROLE_USER');
  if ATaskId <> '' then
    Msg.AddPair('taskId', ATaskId);
  if AContextId <> '' then
    Msg.AddPair('contextId', AContextId);
  Parts := TJSONArray.Create;
  Msg.AddPair('parts', Parts);
  PartObj := TJSONObject.Create;
  PartObj.AddPair('text', AText);
  Parts.AddElement(PartObj);

  if not ABlocking then
  begin
    Config := TJSONObject.Create;
    Config.AddPair('blocking', TJSONBool.Create(False));
    Params.AddPair('configuration', Config);
  end;

  Raw := JsonRpcCall('SendMessage', Params); // JsonRpcCall libera Params
  try
    Result := UnwrapSendMessageResult(Raw);
  finally
    Raw.Free;
  end;
  ReadTaskStatus(Result);
  AOutputText := ArtifactsText(Result);
end;

// El result de SendMessage es un SendMessageResponse: un oneof {task | message}.
// Esta funcion devuelve SIEMPRE un objeto con forma de Task, para que el resto
// del cliente no tenga que saber en que era habla el agente remoto.
//   {"task": {...}}     -> v1.0, el caso normal
//   {"message": {...}}  -> v1.0, el agente respondio de una sin crear tarea
//   {...campos task...} -> era 0.x, el Task viajaba plano
class function TAiA2AClient.UnwrapSendMessageResult(ARawResult: TJSONObject): TJSONObject;
var
  Inner, MsgObj, StatusObj, ArtObj: TJSONObject;
  Parts, Artifacts: TJSONArray;
begin
  if not Assigned(ARawResult) then
    Exit(nil);

  if ARawResult.TryGetValue<TJSONObject>('task', Inner) then
    Exit(TJSONObject(Inner.Clone));

  if ARawResult.TryGetValue<TJSONObject>('message', MsgObj) then
  begin
    // Respuesta inmediata sin tarea: se sintetiza un Task completado con el
    // texto del mensaje como artifact, para no romper el contrato de SendText.
    Result := TJSONObject.Create;
    Result.AddPair('id', MsgObj.GetValue<string>('taskId', ''));
    Result.AddPair('contextId', MsgObj.GetValue<string>('contextId', ''));
    StatusObj := TJSONObject.Create;
    StatusObj.AddPair('state', 'TASK_STATE_COMPLETED');
    Result.AddPair('status', StatusObj);
    Artifacts := TJSONArray.Create;
    if MsgObj.TryGetValue<TJSONArray>('parts', Parts) then
    begin
      ArtObj := TJSONObject.Create;
      ArtObj.AddPair('artifactId', 'inline-message');
      ArtObj.AddPair('name', 'result');
      ArtObj.AddPair('parts', TJSONArray(Parts.Clone));
      Artifacts.AddElement(ArtObj);
    end;
    Result.AddPair('artifacts', Artifacts);
    Exit;
  end;

  Result := TJSONObject(ARawResult.Clone);
end;

function TAiA2AClient.SendText(const AText: string; out AOutputText: string): TJSONObject;
begin
  Result := SendTextEx(AText, '', '', AOutputText, True);
end;

function TAiA2AClient.GetTask(const ATaskId: string): TJSONObject;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('id', ATaskId);
  Result := JsonRpcCall('GetTask', Params);
  ReadTaskStatus(Result);
end;

function TAiA2AClient.CancelTask(const ATaskId: string): TJSONObject;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('id', ATaskId);
  Result := JsonRpcCall('CancelTask', Params);
  ReadTaskStatus(Result);
end;

{ TAiA2ARemoteAgentTool }

constructor TAiA2ARemoteAgentTool.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTimeoutMs := 60000;
  FSuspendOnInputRequired := True;
end;

destructor TAiA2ARemoteAgentTool.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TAiA2ARemoteAgentTool.GetClient: TAiA2AClient;
begin
  if not Assigned(FClient) then
    FClient := TAiA2AClient.Create(nil);
  FClient.Url := FAgentUrl;
  FClient.Timeout := FTimeoutMs;
  FClient.ApiKey := FApiKey;
  Result := FClient;
end;

// Clave del blackboard donde se recuerda el task remoto pendiente de este nodo.
// Va por nodo, no por tool: la misma instancia de tool puede estar asignada a
// varios nodos del grafo.
function TAiA2ARemoteAgentTool.StateKey(ANode: TAIAgentsNode): string;
begin
  Result := 'A2A.' + ANode.Name + '.PendingTask';
end;

procedure TAiA2ARemoteAgentTool.Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
var
  Client: TAiA2AClient;
  Task: TJSONObject;
  OutText, Pending, TaskId, ContextId: string;
  Sep: Integer;
  Board: TAIBlackboard;
begin
  if Trim(FAgentUrl) = '' then
    raise EA2AClientException.Create('TAiA2ARemoteAgentTool: AgentUrl no configurada');

  Board := nil;
  if Assigned(ANode) and Assigned(ANode.Graph) then
    Board := ANode.Graph.Blackboard;

  // Si este nodo ya tenia un task remoto en input-required, esta llamada es la
  // reanudacion: AInput es la respuesta humana y viaja al MISMO task remoto.
  TaskId := '';
  ContextId := '';
  if Assigned(Board) then
  begin
    Pending := Board.GetString(StateKey(ANode));
    Sep := Pos('|', Pending);
    if Sep > 0 then
    begin
      TaskId := Copy(Pending, 1, Sep - 1);
      ContextId := Copy(Pending, Sep + 1, MaxInt);
    end;
  end;

  Client := GetClient;
  Task := Client.SendTextEx(AInput, TaskId, ContextId, OutText);
  try
    case Client.LastTaskState of
      tsCompleted:
        begin
          if Assigned(Board) then
            Board.SetString(StateKey(ANode), '');
          AOutput := OutText;
        end;

      tsInputRequired:
        begin
          if not FSuspendOnInputRequired then
            raise EA2ARemoteTaskError.CreateFmt('El agente A2A remoto requiere input adicional: %s',
              [Client.LastStatusMessage]);
          // Se recuerda el task remoto y se suspende el nodo local. Al reanudar
          // (ResumeThread) este Execute vuelve a correr y continua alla mismo.
          if Assigned(Board) then
            Board.SetString(StateKey(ANode), Client.LastTaskId + '|' + Client.LastContextId);
          AOutput := AInput; // pass-through mientras se espera al humano
          ANode.Suspend(Client.LastStatusMessage, 'A2A task ' + Client.LastTaskId + ' @ ' + FAgentUrl);
        end;
    else
      raise EA2ARemoteTaskError.CreateFmt('El agente A2A remoto termino en %s%s',
        [Client.LastState, IfThen(Client.LastStatusMessage <> '', ': ' + Client.LastStatusMessage, '')]);
    end;
  finally
    Task.Free;
  end;
end;

{ TAiA2AAgentTool }

constructor TAiA2AAgentTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeoutMs := 60000;
  FToolName := 'ask_remote_agent';
  FDescription := 'Delega una consulta en un agente remoto especializado y devuelve su respuesta';
end;

procedure TAiA2AAgentTool.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FFunctions) then
  begin
    FFunctions := nil;
    FItem := nil; // lo destruye la coleccion, aqui solo se suelta la referencia
  end;
end;

procedure TAiA2AAgentTool.SetFunctions(const Value: TAiFunctions);
begin
  if FFunctions = Value then
    Exit;
  FFunctions := Value;
  if Assigned(FFunctions) then
  begin
    FFunctions.FreeNotification(Self);
    RegisterTool;
  end;
end;

procedure TAiA2AAgentTool.RegisterTool;
var
  Param: TFunctionParamsItem;
begin
  if not Assigned(FFunctions) then
    Exit;
  if not Assigned(FItem) then
    FItem := FFunctions.Functions.Add;

  FItem.FunctionName := FToolName;
  FItem.Description.Text := FDescription;
  FItem.Enabled := True;
  FItem.OnAction := DoToolAction;

  // Un unico parametro: la consulta en lenguaje natural. El agente remoto es
  // opaco por diseno, asi que no tiene sentido exponerle un schema detallado.
  if FItem.Parameters.Count = 0 then
  begin
    Param := FItem.Parameters.Add;
    Param.Name := 'query';
    Param.Description.Text := 'La consulta o instruccion para el agente remoto';
    Param.ParamType := TToolsParamType.ptString;
    Param.Required := True;
  end;
end;

procedure TAiA2AAgentTool.DiscoverFromCard;
var
  Client: TAiA2AClient;
  Card: TJSONObject;
begin
  Client := TAiA2AClient.Create(nil);
  try
    Client.Url := FAgentUrl;
    Client.ApiKey := FApiKey;
    Client.Timeout := FTimeoutMs;
    Card := Client.FetchAgentCard;
    try
      if FDescription = '' then
        FDescription := Card.GetValue<string>('description', '')
      else
        FDescription := FDescription + ' (' + Card.GetValue<string>('description', '') + ')';
      RegisterTool;
    finally
      Card.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TAiA2AAgentTool.DoToolAction(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Client: TAiA2AClient;
  Task: TJSONObject;
  Consulta, Salida: string;
begin
  Handled := True;
  if Trim(FAgentUrl) = '' then
  begin
    ToolCall.Response := '{"error":"TAiA2AAgentTool: AgentUrl no configurada"}';
    Exit;
  end;

  Consulta := ToolCall.Params.Values['query'];
  if Consulta = '' then
    Consulta := ToolCall.Arguments; // ultimo recurso: el JSON crudo

  Client := TAiA2AClient.Create(nil);
  try
    Client.Url := FAgentUrl;
    Client.ApiKey := FApiKey;
    Client.Timeout := FTimeoutMs;
    try
      Task := Client.SendText(Consulta, Salida);
      try
        case Client.LastTaskState of
          tsCompleted:
            ToolCall.Response := Salida;
          tsInputRequired:
            // El agente remoto pide mas datos. Se le devuelve al LLM como
            // texto: es el LLM quien decide si preguntar al usuario.
            ToolCall.Response := 'El agente remoto necesita mas informacion: ' + Client.LastStatusMessage;
        else
          ToolCall.Response := 'El agente remoto no completo la tarea (' + Client.LastState + '): ' +
            Client.LastStatusMessage;
        end;
      finally
        Task.Free;
      end;
    except
      on E: Exception do
        // Un fallo del agente remoto se le cuenta al LLM en vez de romper el
        // turno de chat: puede reintentar o seguir sin esa informacion.
        ToolCall.Response := 'Error llamando al agente remoto: ' + E.Message;
    end;
  finally
    Client.Free;
  end;
end;
initialization

TEngineRegistry.Instance.RegisterTool(TAiA2ARemoteAgentTool, 'uMakerAi.A2A.Client');

end.
