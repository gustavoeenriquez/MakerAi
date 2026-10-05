unit uRegression.Fixtures;

// -----------------------------------------------------------------------------
// Fixtures de la suite de regresion de MakerAI.
//
// Todo corre in-process: no depende de demos compilados ni de servicios
// externos. Contiene:
//   - Tools MCP de prueba (echo determinista y confirm con patron MRTR).
//   - Un servidor MCP "solo legacy" (Indy) que responde -32601 a
//     server/discover, para validar el fallback dual-era del cliente.
//   - Handlers 'of object' para grafos de agentes y guardrails.
//   - TFakeJev: TAiJev sin red, con respuestas HTTP encoladas.
//   - TFakeDispatchClassifier / TFakeImageTool: SmartDispatch sin red.
//   - TPassageFakeJev: responde segun el pasaje (reranker de RAG sin red).
//   - TFakePromptGuard: guardrail de entrada con veredicto fijo.
//   - TFakeOpenAiAudio: TAiOpenAiAudio sin red (multipart capturado).
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  IdContext, IdCustomHTTPServer, IdHTTPServer,
  System.Generics.Collections,
  uMakerAi.MCPServer.Core, uMakerAi.Agents, uMakerAi.Tools.Functions,
  uMakerAi.Chat.Messages, uMakerAi.Chat.Tools, uMakerAi.Jev, uMakerAi.Embeddings.core,
  UMakerAi.Chat, System.Net.HttpClient, uMakerAi.Jev.Batch, uMakerAi.Tools.Skills,
  System.Net.Mime, uMakerAi.OpenAI.Audio;

type
  // --- Tool MCP determinista: devuelve el texto en mayusculas ---
  TEchoParams = class
  private
    FText: string;
  public
    [AiMCPSchemaDescription('Texto a devolver en mayusculas')]
    property Text: string read FText write FText;
  end;

  TEchoTool = class(TAiMCPToolBase<TEchoParams>)
  protected
    function ExecuteWithParams(const AParams: TEchoParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool MCP con patron MRTR (spec 2026-07-28) ---
  // Primera llamada: input_required con elicitation. Reintento con
  // action=accept: ejecuta. Sirve para validar el loop de reintento del cliente.
  TConfirmParams = class
  private
    FOperation: string;
  public
    [AiMCPSchemaDescription('Operacion que requiere confirmacion')]
    property Operation: string read FOperation write FOperation;
  end;

  TConfirmTool = class(TAiMCPToolBase<TConfirmParams>)
  private
    function BuildInputRequired(const AOperation: string): TJSONObject;
  protected
    function ExecuteWithParams(const AParams: TConfirmParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Servidor MCP "solo legacy" ---
  // Responde -32601 a server/discover y atiende initialize/tools/list al estilo
  // pre-2026: el cliente dual-era debe detectarlo y caer al handshake.
  TLegacyOnlyMCPServer = class
  private
    FHttp: TIdHTTPServer;
    procedure HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
  end;

  // --- Registry PPM falso (skills) ---
  // skill-demo con versiones 1.2.0, 1.10.0 y 2.0.0 (retirada): la buena es
  // 1.10.0, que solo sale comparando por semver (como texto gana 1.2.0).
  // 'un-prompt' es de otro tipo. Todo lo que cuelga de /html responde una
  // pagina HTML con 200, como un sitio web ante una ruta que no es suya.
  TFakePPMRegistry = class
  private
    FHttp: TIdHTTPServer;
    FRequests: Integer;
    procedure HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
    property Requests: Integer read FRequests;
  end;

  // --- TAiJev sin red ---
  // Sustituye DoPost: guarda el cuerpo enviado y devuelve las respuestas
  // encoladas en orden. Sin respuestas encoladas devuelve 500.
  TFakeJev = class(TAiJev)
  private
    FResponses: TQueue<TPair<Integer, string>>;
    FLastBody: string;
    FCalls: Integer;
  protected
    function DoPost(const ABody: string; out AResponse: string): Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Enqueue(AStatus: Integer; const ABody: string);
    property LastBody: string read FLastBody;
    property Calls: Integer read FCalls;
  end;

  // --- TAiOpenAiAudio sin red ---
  // Sustituye PostMultipart: guarda el multipart enviado y devuelve Response
  // con Status. FieldValues lee un campo del multipart ('a,b' si se repite).
  TFakeOpenAiAudio = class(TAiOpenAiAudio)
  private
    FLastBody: string;
    FLastUrl: string;
  protected
    function PostMultipart(const AUrl: string; ABody: TMultipartFormData;
      out AContent: string): Integer; override;
  public
    Response: string;
    Status: Integer;
    constructor Create(AOwner: TComponent); override;
    function FieldValues(const AName: string): string;
    property LastBody: string read FLastBody;
    property LastUrl: string read FLastUrl;
  end;

  // --- Reranker de RAG sin red ---
  // Responde segun el contenido del pasaje (no segun el orden de llegada):
  // 'NIC 16' -> evidencia 0.95; 'IGNORA' -> inyeccion 0.98; resto -> 0.10.
  // Con FailAll devuelve 500 (para probar la caida al rerank por coseno).
  TPassageFakeJev = class(TAiJev)
  public
    FailAll: Boolean;
    Calls: Integer;
  protected
    function DoPost(const ABody: string; out AResponse: string): Integer; override;
  end;

  // --- Guardrail de entrada sin red ---
  TFakePromptGuard = class(TAiPromptGuardBase)
  public
    Block: Boolean;
    RaiseError: Boolean;
    Calls: Integer;
    function CheckPrompt(const APrompt: string): TAiPromptVerdict; override;
  end;

  // --- SmartDispatch sin red ---
  // Clasificador que responde lo que se le diga y anota con que tags lo llamaron
  TFakeDispatchClassifier = class(TAiDispatchClassifierBase)
  public
    Answer: string;
    LastTags: string;
    Calls: Integer;
  protected
    function ClassifyDispatch(const APrompt: string; const ATags: TArray<string>): string; override;
  end;

  // Tool de imagen que solo anota el prompt recibido
  TFakeImageTool = class(TAiImageToolBase)
  public
    LastPrompt: string;
    Calls: Integer;
  protected
    procedure ExecuteImageGeneration(const APrompt: string; ResMsg, AskMsg: TAiChatMessage); override;
  end;

  // --- Handlers 'of object' para grafos y guardrails ---
  TFixtureHandlers = class
  public
    ToolExecuted: Boolean;   // marca si un tool bloqueado llego a ejecutarse
    BlockedFired: Boolean;   // marca si OnBlocked se disparo
    LastElicitMessage: string;
    LastChatError: string;       // ultimo error reportado por un chat (OnError)
    LastGuardCategory: string;   // categoria recibida en OnPromptGuard
    LastDataEnd: string;         // texto recibido en OnReceiveDataEnd
    SkillsLoaded: Integer;       // skills entregados por TAiSkills (OnSkillLoaded)

    // Chat: texto recibido en OnReceiveDataEnd
    procedure ChatDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    // Chat: ultimo error reportado (OnError)
    procedure ChatError(Sender: TObject; const ErrorMsg: string; Exception: Exception;
      const AResponse: IHTTPResponse);
    // OnProgress de TAiJevBatchLabeler: cancela el lote tras la primera fila
    procedure BatchCancelAfterFirst(Sender: TObject; ADone, ATotal: Integer);
    // OnCategorized de TAiJevGuardrailClassifier: deja pasar y anota la categoria
    procedure JevCategorizedAllow(Sender: TObject; const AToolName, AArguments, ACategory: string;
      AConfidence: Double; var AAllow: Boolean; var AReason: string);
    // OnPromptGuard que deja pasar el mensaje y anota la categoria
    procedure PromptGuardAllow(Sender: TObject; const AVerdict: TAiPromptVerdict;
      var AAction: TAiSanitizeAction);
    // Embeddings deterministas sin red: el mismo vector para cualquier texto
    procedure FakeEmbedding(Sender: TObject; const aInput, aUser, aModel, aEncodingFormat: String;
      aDimensions: Integer; var aEmbedding: TAiEmbeddingData);
    // Nodo de grafo: encadena el nombre del nodo al input
    procedure NodeExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Igual pero lento: fuerza solapamiento real entre tasks concurrentes y
    // deja ver el estado 'working' en el modo no bloqueante.
    procedure NodeExecSlow(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Human-in-the-loop: se suspende la primera vez y en la reanudacion
    // concatena la respuesta humana.
    procedure NodeSuspendOnce(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Fabrica de managers para el pool del servidor A2A (un grafo nuevo por slot)
    procedure AcquireManager(Sender: TObject; var AManager: TAIAgentManager);
    // Tool local de TAiFunctions (para la prueba de integracion de guardrails)
    procedure ToolAction(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
    // Guardrails
    procedure GuardBlocked(Sender: TObject; const AToolName, AArguments, AReason: string);
    procedure GuardCheck(Sender: TObject; const AToolName, AArguments: string;
      var AAllow: Boolean; var AReason: string);
    // MRTR: responde la elicitation aceptando
    procedure InputRequired(Sender: TObject; const AToolName: string;
      AInputRequests, AInputResponses: TJSONObject; var AHandled: Boolean);
    // TAiSkills: veta el skill 'vetado' y cuenta los skills entregados
    procedure SkillVeto(Sender: TObject; ASkill: TAiSkillItem; var Allow: Boolean);
    procedure SkillLoaded(Sender: TObject; ASkill: TAiSkillItem);
  end;

implementation

uses uMakerAi.MCPClient.Core;

{ TEchoTool }

constructor TEchoTool.Create;
begin
  inherited;
  FName := 'echo_upper';
  FDescription := 'Devuelve el texto recibido en mayusculas (determinista)';
end;

function TEchoTool.ExecuteWithParams(const AParams: TEchoParams; const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New.AddText(UpperCase(AParams.Text)).Build;
end;

{ TConfirmTool }

constructor TConfirmTool.Create;
begin
  inherited;
  FName := 'confirm_op';
  FDescription := 'Pide confirmacion via elicitation (patron MRTR) y ejecuta al aceptar';
end;

function TConfirmTool.BuildInputRequired(const AOperation: string): TJSONObject;
var
  Reqs, Elicit, ElicitParams, Schema, Props, ConfirmProp: TJSONObject;
  Required: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.AddPair('resultType', 'input_required');
  Reqs := TJSONObject.Create;
  Result.AddPair('inputRequests', Reqs);
  Elicit := TJSONObject.Create;
  Reqs.AddPair('user_confirmation', Elicit);
  Elicit.AddPair('method', 'elicitation/create');
  ElicitParams := TJSONObject.Create;
  Elicit.AddPair('params', ElicitParams);
  ElicitParams.AddPair('mode', 'form');
  ElicitParams.AddPair('message', 'Confirma la operacion: ' + AOperation);
  Schema := TJSONObject.Create;
  ElicitParams.AddPair('requestedSchema', Schema);
  Schema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Schema.AddPair('properties', Props);
  ConfirmProp := TJSONObject.Create;
  Props.AddPair('confirm', ConfirmProp);
  ConfirmProp.AddPair('type', 'boolean');
  Required := TJSONArray.Create;
  Required.Add('confirm');
  Schema.AddPair('required', Required);
  Result.AddPair('requestState', TNetEncoding.Base64.Encode(AOperation));
end;

function TConfirmTool.ExecuteWithParams(const AParams: TConfirmParams; const AuthContext: TAiAuthContext): TJSONObject;
var
  V: TJSONValue;
  Action, DecodedState: string;
begin
  if Assigned(AuthContext.InputResponses) then
  begin
    V := AuthContext.InputResponses.GetValue('user_confirmation');
    if not(V is TJSONObject) then
      Exit(BuildInputRequired(AParams.Operation)); // falta la respuesta: re-pedir

    Action := TJSONObject(V).GetValue<string>('action', '');
    DecodedState := '';
    if AuthContext.RequestState <> '' then
      try
        DecodedState := TNetEncoding.Base64.Decode(AuthContext.RequestState);
      except
        DecodedState := ''; // estado corrupto => rechazo limpio
      end;
    if DecodedState <> AParams.Operation then
      Exit(TAiMCPResponseBuilder.New.AddText('ESTADO_INVALIDO').Build);

    if SameText(Action, 'accept') then
      Exit(TAiMCPResponseBuilder.New.AddText('CONFIRMADO:' + AParams.Operation).Build)
    else
      Exit(TAiMCPResponseBuilder.New.AddText('CANCELADO:' + AParams.Operation).Build);
  end;
  Result := BuildInputRequired(AParams.Operation);
end;

{ TFixtureHandlers: TAiSkills }

procedure TFixtureHandlers.SkillVeto(Sender: TObject; ASkill: TAiSkillItem; var Allow: Boolean);
begin
  Allow := not SameText(ASkill.Name, 'vetado');
end;

procedure TFixtureHandlers.SkillLoaded(Sender: TObject; ASkill: TAiSkillItem);
begin
  Inc(SkillsLoaded);
end;

{ TFakePPMRegistry }

constructor TFakePPMRegistry.Create(APort: Integer);
begin
  inherited Create;
  FHttp := TIdHTTPServer.Create(nil);
  FHttp.OnCommandGet := HttpCommand;
  FHttp.DefaultPort := APort;
  FHttp.Active := True;
end;

destructor TFakePPMRegistry.Destroy;
begin
  FHttp.Active := False;
  FHttp.Free;
  inherited;
end;

procedure TFakePPMRegistry.HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
const
  SKILL_MD =
    '---'#10 +
    'name: demo'#10 +
    'description: Skill de prueba del registry falso'#10 +
    'allowed-tools:'#10 +
    '  - Read'#10 +
    'model: claude-opus-4-6'#10 +
    '---'#10 +
    'Instrucciones de la version 1.10.0'#10;
var
  Doc: string;
begin
  AtomicIncrement(FRequests);
  Doc := ARequestInfo.Document;
  AResponseInfo.CharSet := 'utf-8';

  if Doc.StartsWith('/html') then
  begin
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ContentType := 'text/html';
    AResponseInfo.ContentText := '<!DOCTYPE html><html><body>PPM</body></html>';
    Exit;
  end;

  AResponseInfo.ContentType := 'application/json';
  if Doc = '/v1/packages/skill-demo' then
    AResponseInfo.ContentText :=
      '{"package":{"name":"skill-demo","type":"skill","versions":[' +
      '{"version":"1.2.0","yanked":false},{"version":"1.10.0","yanked":false},' +
      '{"version":"2.0.0","yanked":true}]}}'
  else if Doc = '/v1/packages/un-prompt' then
    AResponseInfo.ContentText :=
      '{"package":{"name":"un-prompt","type":"prompt","versions":[{"version":"1.0.0","yanked":false}]}}'
  else if Doc = '/v1/packages/skill-demo/1.10.0/skill' then
  begin
    AResponseInfo.ContentType := 'text/plain';
    AResponseInfo.ContentText := SKILL_MD;
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '{"status":"error","message":"Package not found","code":404}';
  end;
end;

{ TLegacyOnlyMCPServer }

constructor TLegacyOnlyMCPServer.Create(APort: Integer);
begin
  inherited Create;
  FHttp := TIdHTTPServer.Create(nil);
  FHttp.OnCommandGet := HttpCommand;
  FHttp.OnCommandOther := HttpCommand;
  FHttp.DefaultPort := APort;
  FHttp.Active := True;
end;

destructor TLegacyOnlyMCPServer.Destroy;
begin
  FHttp.Active := False;
  FHttp.Free;
  inherited;
end;

procedure TLegacyOnlyMCPServer.HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  Body, Method, Out_: string;
  Root: TJSONValue;
  Req: TJSONObject;
  IdNum: Integer;
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

  Method := '';
  IdNum := 0;
  Root := TJSONObject.ParseJSONValue(Body);
  if Root is TJSONObject then
  begin
    Req := TJSONObject(Root);
    try
      Method := Req.GetValue<string>('method', '');
      IdNum := Req.GetValue<Integer>('id', 0);
    finally
      Req.Free;
    end;
  end
  else
    Root.Free;

  if SameText(Method, 'server/discover') then
    // Servidor legacy: no conoce el metodo moderno
    Out_ := Format('{"jsonrpc":"2.0","error":{"code":-32601,"message":"Method not found"},"id":%d}', [IdNum])
  else if SameText(Method, 'initialize') then
    Out_ := Format('{"jsonrpc":"2.0","id":%d,"result":{"protocolVersion":"2025-03-26",' +
      '"capabilities":{"tools":{}},"serverInfo":{"name":"LegacyOnly","version":"1.0"}}}', [IdNum])
  else if SameText(Method, 'tools/list') then
    Out_ := Format('{"jsonrpc":"2.0","id":%d,"result":{"tools":[{"name":"legacy_tool",' +
      '"description":"tool legacy","inputSchema":{"type":"object","properties":{}}}]}}', [IdNum])
  else
    Out_ := Format('{"jsonrpc":"2.0","error":{"code":-32601,"message":"Method not found"},"id":%d}', [IdNum]);

  AResponseInfo.ResponseNo := 200;
  AResponseInfo.ContentType := 'application/json; charset=utf-8';
  AResponseInfo.CharSet := 'utf-8';
  AResponseInfo.ContentText := Out_;
end;

{ TFixtureHandlers }

procedure TFixtureHandlers.ChatDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
begin
  LastDataEnd := aText;
end;

procedure TFixtureHandlers.ChatError(Sender: TObject; const ErrorMsg: string; Exception: Exception;
  const AResponse: IHTTPResponse);
begin
  LastChatError := ErrorMsg;
end;

procedure TFixtureHandlers.BatchCancelAfterFirst(Sender: TObject; ADone, ATotal: Integer);
begin
  if ADone = 1 then
    TAiJevBatchLabeler(Sender).Cancel;
end;

procedure TFixtureHandlers.JevCategorizedAllow(Sender: TObject; const AToolName, AArguments,
  ACategory: string; AConfidence: Double; var AAllow: Boolean; var AReason: string);
begin
  LastGuardCategory := ACategory;
  AAllow := True;
  AReason := '';
end;

procedure TFixtureHandlers.PromptGuardAllow(Sender: TObject; const AVerdict: TAiPromptVerdict;
  var AAction: TAiSanitizeAction);
begin
  LastGuardCategory := AVerdict.Category;
  AAction := saAllow;
end;

procedure TFixtureHandlers.FakeEmbedding(Sender: TObject; const aInput, aUser, aModel,
  aEncodingFormat: String; aDimensions: Integer; var aEmbedding: TAiEmbeddingData);
var
  I: Integer;
begin
  SetLength(aEmbedding, 4);
  for I := 0 to 3 do
    aEmbedding[I] := 0.5;
end;

procedure TFixtureHandlers.NodeExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Input + '>' + Node.Name;
end;

procedure TFixtureHandlers.NodeExecSlow(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Sleep(150);
  Output := Input + '>' + Node.Name;
end;

procedure TFixtureHandlers.NodeSuspendOnce(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
const
  KEY_PREFIX = 'suite.resumed.';
begin
  // La marca vive en el blackboard, no en el fixture: ResumeThread reejecuta
  // este mismo nodo y hay que distinguir la primera pasada de la reanudacion.
  if Node.Graph.Blackboard.GetString(KEY_PREFIX + Node.Name) = '' then
  begin
    Node.Graph.Blackboard.SetString(KEY_PREFIX + Node.Name, '1');
    Output := Input;
    Node.Suspend('Se requiere aprobacion humana', 'ctx:' + Input);
  end
  else
    Output := Input + '>aprobado';
end;

procedure TFixtureHandlers.AcquireManager(Sender: TObject; var AManager: TAIAgentManager);
begin
  AManager := TAIAgentManager.Create(nil);
  AManager.AddNode('Uno', NodeExecSlow).AddNode('Dos', NodeExec);
  AManager.AddEdge('Uno', 'Dos');
  AManager.SetEntryPoint('Uno').SetFinishPoint('Dos');
end;

procedure TFixtureHandlers.ToolAction(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolExecuted := True; // si esto corre, el guardrail no bloqueo
  ToolCall.Response := '{"ok":true}';
  Handled := True;
end;

procedure TFixtureHandlers.GuardBlocked(Sender: TObject; const AToolName, AArguments, AReason: string);
begin
  BlockedFired := True;
end;

procedure TFixtureHandlers.GuardCheck(Sender: TObject; const AToolName, AArguments: string;
  var AAllow: Boolean; var AReason: string);
begin
  if AArguments.ToLower.Contains('produccion') then
  begin
    AAllow := False;
    AReason := 'entorno de produccion protegido';
  end;
end;

procedure TFixtureHandlers.InputRequired(Sender: TObject; const AToolName: string;
  AInputRequests, AInputResponses: TJSONObject; var AHandled: Boolean);
var
  V: TJSONValue;
  Resp, Content: TJSONObject;
begin
  V := AInputRequests.GetValue('user_confirmation');
  if V is TJSONObject then
    LastElicitMessage := TJSONObject(V).GetValue<string>('params.message', '');

  Resp := TJSONObject.Create;
  Resp.AddPair('action', 'accept');
  Content := TJSONObject.Create;
  Content.AddPair('confirm', TJSONBool.Create(True));
  Resp.AddPair('content', Content);
  AInputResponses.AddPair('user_confirmation', Resp);
  AHandled := True;
end;

{ TFakeJev }

constructor TFakeJev.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResponses := TQueue<TPair<Integer, string>>.Create;
  RetryDelay := 0; // los reintentos no esperan en la suite
end;

destructor TFakeJev.Destroy;
begin
  FResponses.Free;
  inherited;
end;

procedure TFakeJev.Enqueue(AStatus: Integer; const ABody: string);
begin
  FResponses.Enqueue(TPair<Integer, string>.Create(AStatus, ABody));
end;

function TFakeJev.DoPost(const ABody: string; out AResponse: string): Integer;
var
  R: TPair<Integer, string>;
begin
  Inc(FCalls);
  FLastBody := ABody;
  if FResponses.Count = 0 then
  begin
    AResponse := '{"detail":"sin respuesta encolada"}';
    Exit(500);
  end;
  R := FResponses.Dequeue;
  AResponse := R.Value;
  Result := R.Key;
end;

{ TFakeOpenAiAudio }

constructor TFakeOpenAiAudio.Create(AOwner: TComponent);
begin
  inherited;
  Status := 200;
end;

function TFakeOpenAiAudio.PostMultipart(const AUrl: string;
  ABody: TMultipartFormData; out AContent: string): Integer;
var
  Bytes: TBytes;
begin
  FLastUrl := AUrl;
  ABody.Stream.Position := 0;
  SetLength(Bytes, ABody.Stream.Size);
  if Length(Bytes) > 0 then
    ABody.Stream.ReadBuffer(Bytes[0], Length(Bytes));
  // El audio de prueba es ASCII: el multipart completo se lee como texto
  FLastBody := TEncoding.ANSI.GetString(Bytes);
  AContent := Response;
  Result := Status;
end;

function TFakeOpenAiAudio.FieldValues(const AName: string): string;
var
  Key: string;
  P, VStart, VEnd: Integer;
begin
  Result := '';
  Key := 'name="' + AName + '"';
  P := Pos(Key, FLastBody);
  while P > 0 do
  begin
    VStart := Pos(#13#10#13#10, FLastBody, P);
    if VStart = 0 then
      Break;
    Inc(VStart, 4);
    VEnd := Pos(#13#10, FLastBody, VStart);
    if VEnd = 0 then
      VEnd := Length(FLastBody) + 1;
    if Result <> '' then
      Result := Result + ',';
    Result := Result + Copy(FLastBody, VStart, VEnd - VStart);
    P := Pos(Key, FLastBody, VEnd);
  end;
end;

{ TFakeDispatchClassifier }

function TFakeDispatchClassifier.ClassifyDispatch(const APrompt: string;
  const ATags: TArray<string>): string;
begin
  Inc(Calls);
  LastTags := string.Join(',', ATags);
  Result := Answer;
end;

{ TFakeImageTool }

procedure TFakeImageTool.ExecuteImageGeneration(const APrompt: string; ResMsg, AskMsg: TAiChatMessage);
begin
  Inc(Calls);
  LastPrompt := APrompt;
  ResMsg.Prompt := 'IMG:' + APrompt;
end;

{ TPassageFakeJev }

function TPassageFakeJev.DoPost(const ABody: string; out AResponse: string): Integer;
var
  Body: TJSONValue;
  Passage: string;
  Evidence, Injection: string;
begin
  Inc(Calls);
  if FailAll then
  begin
    AResponse := '{"detail":"caido"}';
    Exit(500);
  end;
  Body := TJSONObject.ParseJSONValue(ABody);
  try
    Passage := (Body as TJSONObject).GetValue<TJSONObject>('state').GetValue<string>('passage', '');
  finally
    Body.Free;
  end;
  Evidence := '0.10';
  Injection := '0.05';
  if Passage.Contains('NIC 16') then
    Evidence := '0.95';
  if Passage.Contains('IGNORA') then
    Injection := '0.98';
  AResponse := '{"model":"jev-1.13.0","answers":{' +
    '"evidence":{"type":"noul","noul":' + Evidence + '},' +
    '"injection":{"type":"noul","noul":' + Injection + '}}}';
  Result := 200;
end;

{ TFakePromptGuard }

function TFakePromptGuard.CheckPrompt(const APrompt: string): TAiPromptVerdict;
begin
  Inc(Calls);
  if RaiseError then
    raise Exception.Create('guard caido');
  Result.Allowed := not Block;
  Result.Category := '';
  Result.Score := 0;
  Result.Reason := '';
  if Block then
  begin
    Result.Category := 'injection';
    Result.Score := 0.95;
    Result.Reason := 'fake injection 0.95';
  end;
end;

end.
