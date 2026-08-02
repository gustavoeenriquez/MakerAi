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
// TAiA2AClient (MVP, spec A2A 1.0): consume agentes A2A remotos.
//
// - FetchAgentCard: GET /.well-known/agent-card.json (con fallback al alias
//   legacy /.well-known/agent.json).
// - SendText: SendMessage JSON-RPC con un TextPart; devuelve el texto de los
//   artifacts del Task resultante y expone el Task completo.
// - GetTask / CancelTask.
// - Telemetria: spans 'a2a.client <metodo>' (skClient).
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Net.HttpClient, System.Net.URLClient,
  uMakerAi.Agents, uMakerAi.Agents.Attributes;

type
  EA2AClientException = class(Exception);

  TAiA2AClient = class(TComponent)
  private
    FUrl: string;      // base del agente, p.ej. http://localhost:8280
    FTimeout: Integer; // ms
    FLastTaskId: string;
    FLastState: string;
    function JsonRpcCall(const AMethod: string; AParams: TJSONObject): TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;

    // Descarga y devuelve el Agent Card (el llamador libera el objeto)
    function FetchAgentCard: TJSONObject;

    // Envia un mensaje de texto y devuelve el Task resultante (el llamador
    // libera el objeto). El texto de los artifacts queda en AOutputText.
    function SendText(const AText: string; out AOutputText: string): TJSONObject;

    function GetTask(const ATaskId: string): TJSONObject;
    function CancelTask(const ATaskId: string): TJSONObject;

    // Ultimo task enviado (conveniencia)
    property LastTaskId: string read FLastTaskId;
    property LastState: string read FLastState;
  published
    property Url: string read FUrl write FUrl;
    property Timeout: Integer read FTimeout write FTimeout default 60000;
  end;

  // Federacion de agentes: asignar como Tool de un TAIAgentsNode para que ese
  // nodo delegue su entrada en un agente A2A remoto. El input del nodo viaja
  // como mensaje de texto y el output es el texto de los artifacts del task.
  [TToolAttribute('A2ARemoteAgent', 'Delega la entrada del nodo en un agente A2A remoto y devuelve el texto de sus artifacts', 'A2A')]
  TAiA2ARemoteAgentTool = class(TAiToolBase)
  private
    [TToolParameterAttribute('Agent URL', 'URL base del agente A2A remoto (ej: http://host:8280)', '')]
    FAgentUrl: string;
    [TToolParameterAttribute('Timeout ms', 'Timeout de la llamada remota en milisegundos', '60000')]
    FTimeoutMs: Integer;
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string); override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property AgentUrl: string read FAgentUrl write FAgentUrl;
    property TimeoutMs: Integer read FTimeoutMs write FTimeoutMs default 60000;
  end;

procedure Register;

implementation

uses uMakerAi.Telemetry, uMakerAi.Agents.EngineRegistry;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiA2AClient]);
end;

{ TAiA2AClient }

constructor TAiA2AClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeout := 60000;
end;

function TAiA2AClient.FetchAgentCard: TJSONObject;
var
  Http: THTTPClient;
  Resp: IHTTPResponse;
  BaseUrl, Content: string;
  V: TJSONValue;
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
      Resp := Http.Get(BaseUrl + '/.well-known/agent-card.json');
      if Resp.StatusCode <> 200 then
        Resp := Http.Get(BaseUrl + '/.well-known/agent.json'); // alias legacy
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

      Http := THTTPClient.Create;
      try
        Http.ConnectionTimeout := FTimeout;
        Http.ResponseTimeout := FTimeout;
        Http.ContentType := 'application/json';
        Body := TStringStream.Create(Req.ToJSON, TEncoding.UTF8);
        try
          Resp := Http.Post(FUrl.TrimRight(['/']) + '/', Body);
        finally
          Body.Free;
        end;
        if Resp.StatusCode <> 200 then
          raise EA2AClientException.CreateFmt('HTTP %d: %s', [Resp.StatusCode, Resp.ContentAsString(TEncoding.UTF8)]);

        Root := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8));
        if not(Root is TJSONObject) then
        begin
          Root.Free;
          raise EA2AClientException.Create('Invalid JSON-RPC response');
        end;
        RespObj := TJSONObject(Root);
        try
          if RespObj.TryGetValue<TJSONObject>('error', ErrObj) then
            raise EA2AClientException.Create('A2A error: ' + ErrObj.ToJSON);
          if RespObj.TryGetValue('result', ResultVal) and (ResultVal is TJSONObject) then
            Result := TJSONObject(ResultVal.Clone)
          else
            raise EA2AClientException.Create('A2A response has no result object');
        finally
          RespObj.Free;
        end;
      finally
        Http.Free;
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

function TAiA2AClient.SendText(const AText: string; out AOutputText: string): TJSONObject;
var
  Params, Msg, PartObj, StatusObj, ArtObj, PObj: TJSONObject;
  Parts, Artifacts, AParts: TJSONArray;
  G: TGUID;
  V, AV, PV: TJSONValue;
  SB: TStringBuilder;
  S: string;
begin
  AOutputText := '';

  Params := TJSONObject.Create;
  Msg := TJSONObject.Create;
  Params.AddPair('message', Msg);
  CreateGUID(G);
  Msg.AddPair('messageId', GUIDToString(G).Replace('{', '').Replace('}', '').ToLower);
  Msg.AddPair('role', 'ROLE_USER');
  Parts := TJSONArray.Create;
  Msg.AddPair('parts', Parts);
  PartObj := TJSONObject.Create;
  PartObj.AddPair('text', AText);
  Parts.AddElement(PartObj);

  Result := JsonRpcCall('SendMessage', Params); // JsonRpcCall libera Params

  // Extraer estado y texto de los artifacts
  FLastTaskId := Result.GetValue<string>('id', '');
  FLastState := '';
  if Result.TryGetValue<TJSONObject>('status', StatusObj) then
    FLastState := StatusObj.GetValue<string>('state', '');

  SB := TStringBuilder.Create;
  try
    if Result.TryGetValue<TJSONArray>('artifacts', Artifacts) then
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
                end;
              end;
        end;
    AOutputText := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TAiA2AClient.GetTask(const ATaskId: string): TJSONObject;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('id', ATaskId);
  Result := JsonRpcCall('GetTask', Params);
end;

function TAiA2AClient.CancelTask(const ATaskId: string): TJSONObject;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('id', ATaskId);
  Result := JsonRpcCall('CancelTask', Params);
end;

{ TAiA2ARemoteAgentTool }

constructor TAiA2ARemoteAgentTool.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTimeoutMs := 60000;
end;

procedure TAiA2ARemoteAgentTool.Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string);
var
  Client: TAiA2AClient;
  Task: TJSONObject;
  OutText: string;
begin
  if Trim(FAgentUrl) = '' then
    raise EA2AClientException.Create('TAiA2ARemoteAgentTool: AgentUrl no configurada');

  Client := TAiA2AClient.Create(nil);
  try
    Client.Url := FAgentUrl;
    Client.Timeout := FTimeoutMs;
    Task := Client.SendText(AInput, OutText);
    Task.Free;
    if Client.LastState <> 'TASK_STATE_COMPLETED' then
      raise EA2AClientException.CreateFmt('El agente A2A remoto termino en %s', [Client.LastState]);
    AOutput := OutText;
  finally
    Client.Free;
  end;
end;

initialization

TEngineRegistry.Instance.RegisterTool(TAiA2ARemoteAgentTool, 'uMakerAi.A2A.Client');

end.
