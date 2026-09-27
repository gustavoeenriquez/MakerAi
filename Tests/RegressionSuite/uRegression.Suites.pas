unit uRegression.Suites;

// -----------------------------------------------------------------------------
// Definicion de los casos de regresion de MakerAI.
//
// La suite se apoya en TAiEvalRunner (uMakerAi.Evals): cada caso declara un
// escenario (Input) y las condiciones que debe cumplir su salida. El target es
// un dispatcher que ejecuta el escenario contra los componentes reales
// levantados in-process y devuelve un string con el resultado observable.
//
// Ese diseno hace que la misma maquinaria que evalua respuestas de un LLM sirva
// como suite de regresion del framework (dogfooding de TAiEvalRunner).
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  uMakerAi.Evals;

type
  TRegressionSuite = class
  private
    FRunner: TAiEvalRunner;
    FConnFirstError: string; // primer fallo del escenario de concurrencia
    procedure DefineCases;
    function Dispatch(const AScenario: string): string;
    // Escenarios agrupados por area
    function RunMcpScenario(const AScenario: string): string;
    function RunAgentScenario(const AScenario: string): string;
    // Flujos de orquestacion A2A: pool concurrente, human-in-the-loop,
    // no bloqueante, tolerancia de literales y cancelacion.
    function RunA2AFlowScenario(const AScenario: string): string;
    function RunPolicyScenario(const AScenario: string): string;
    function RunRagScenario(const AScenario: string): string;
    // Serializacion de tool results en la familia OpenAI-compatible
    function RunChatScenario(const AScenario: string): string;
    // Montaje CONCURRENTE de conexiones, tal y como lo hace un servidor que
    // atiende varios requests a la vez. Sin red: solo ejercita el camino
    // DriverName/Model/Params/AiFunctions, que pasa por el registro global.
    function RunConnScenario(const AScenario: string): string;
    // TAiJev (TypeSafe) sin red: forma del request, parseo, reintentos, validacion
    function RunJevScenario(const AScenario: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Run: TAiEvalReport; // el llamador libera el reporte
  end;

implementation

uses
  System.TypInfo, System.Rtti, System.StrUtils, System.SyncObjs, System.Threading, System.NetEncoding,
  System.Net.HttpClient, System.Net.URLClient,
  uMakerAi.Core,
  uMakerAi.MCPServer.Core, UMakerAi.MCPServer.Http,
  uMakerAi.MCPClient.Core,
  uMakerAi.Agents,
  uMakerAi.A2A.Server, uMakerAi.A2A.Client,
  uMakerAi.Tools.Functions, uMakerAi.Chat.Messages,
  // Montaje de conexiones concurrente: la unidad de Initializations es la que
  // registra los drivers reales en la factoria (sin ella no hay 'Groq' ni
  // 'Claude' que resolver).
  uMakerAi.Chat.AiConnection, uMakerAi.Chat.Initializations,
  uMakerAi.Guardrails, uMakerAi.Jev, uMakerAi.Agents.Tools.JevRouter,
  uMakerAi.Jev.SmartDispatch, uMakerAi.Jev.Guardrails, uMakerAi.Chat.Tools,
  UMakerAi.Chat, uMakerAi.Chat.OpenAi,
  System.IOUtils, uMakerAi.Embeddings.Core,
  uMakerAi.RAG.Vectors, uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vector.Driver.BinFile,
  uRegression.Fixtures;

const
  // Puertos altos para no chocar con servicios de desarrollo
  PORT_MCP_MODERN = 18790;
  PORT_MCP_LEGACY = 18791;
  PORT_A2A        = 18792;
  PORT_A2A_REMOTE = 18793;

{ TRegressionSuite }

constructor TRegressionSuite.Create;
begin
  inherited Create;
  FRunner := TAiEvalRunner.Create(nil);
  DefineCases;
end;

destructor TRegressionSuite.Destroy;
begin
  FRunner.Free;
  inherited;
end;

procedure TRegressionSuite.DefineCases;
begin
  // --- MCP: dual-era (spec 2026-07-28 + legacy) ---
  FRunner.AddCase('mcp.negotiate.modern')
    .Input('mcp:negotiate-modern')
    .ExpectEquals('2026-07-28');

  FRunner.AddCase('mcp.negotiate.legacy-fallback')
    .Input('mcp:negotiate-legacy')
    .ExpectEquals('legacy');

  FRunner.AddCase('mcp.tools.list')
    .Input('mcp:tools-list')
    .ExpectContains('echo_upper')
    .ExpectContains('confirm_op');

  FRunner.AddCase('mcp.tools.call')
    .Input('mcp:call-echo')
    .ExpectContains('HOLA MUNDO');

  // --- MCP: patron MRTR (elicitation + reintento) ---
  FRunner.AddCase('mcp.mrtr.accept')
    .Input('mcp:mrtr-accept')
    .ExpectContains('CONFIRMADO:borrar')
    .ExpectNotContains('input_required');

  FRunner.AddCase('mcp.mrtr.elicit-message')
    .Input('mcp:mrtr-message')
    .ExpectContains('Confirma la operacion');

  FRunner.AddCase('mcp.mrtr.no-handler')
    .Input('mcp:mrtr-nohandler')
    .ExpectContains('input_required');

  // --- Agentes ---
  FRunner.AddCase('agents.graph.sequential')
    .Input('agents:graph')
    .ExpectEquals('esCompleted|ping>A>B>C');

  // --- A2A (spec 1.0) ---
  FRunner.AddCase('a2a.agent-card')
    .Input('a2a:card')
    .ExpectContains('Suite Agent');

  FRunner.AddCase('a2a.send-message')
    .Input('a2a:send')
    .ExpectContains('TASK_STATE_COMPLETED')
    .ExpectContains('hola>Uno>Dos');

  // La cadena completa prueba que el texto cruzo local -> A2A -> remoto -> local:
  // 'fed' >Origen (local) >Uno >Dos (grafo remoto via A2A)
  FRunner.AddCase('a2a.federation')
    .Input('a2a:federation')
    .ExpectContains('esCompleted')
    .ExpectContains('fed>Origen>Uno>Dos');

  // --- A2A: flujos de orquestacion ---

  // Tres tasks simultaneos contra el mismo agente. Con un solo manager dos
  // habrian recibido AgentBusy; con el pool los tres completan.
  FRunner.AddCase('a2a.pool.concurrent')
    .Input('a2a:concurrency')
    .ExpectEquals('completed=3|failed=0');

  // Human-in-the-loop cruzando A2A: el grafo suspende -> input-required con la
  // pregunta, y un SendMessage con el mismo taskId lo reanuda hasta completed.
  FRunner.AddCase('a2a.hitl.resume')
    .Input('a2a:hitl')
    .ExpectContains('tsInputRequired')
    .ExpectContains('aprobacion humana')
    .ExpectContains('tsCompleted')
    .ExpectContains('>aprobado');

  // Lo mismo pero federado: el nodo local se suspende en vez de fallar, y al
  // reanudarlo la respuesta viaja al task remoto y el grafo local termina.
  FRunner.AddCase('a2a.hitl.federated')
    .Input('a2a:fedhitl')
    .ExpectEquals('esSuspended|esCompleted|si>aprobado');

  // blocking=false devuelve el task en working y GetTask lo lleva a terminal
  // sin que el estado quede mintiendo.
  FRunner.AddCase('a2a.task.nonblocking')
    .Input('a2a:nonblocking')
    .ExpectEquals('tsWorking|tsCompleted|lento>Uno>Dos');

  // Interop: el agente publica los literales en forma JSON-RPC y el cliente los
  // entiende igual que la forma protobuf.
  FRunner.AddCase('a2a.state.naming-tolerance')
    .Input('a2a:naming')
    .ExpectEquals('completed|tsCompleted|hola>Uno>Dos');

  // Cancelar un task ya terminado es -32002, no un exito silencioso.
  FRunner.AddCase('a2a.cancel.terminal')
    .Input('a2a:cancelterm')
    .ExpectContains('-32002');

  // --- A2A: formato de cable (spec 1.0) ---
  // Estos casos hablan HTTP CRUDO contra el servidor, sin usar TAiA2AClient.
  // Es deliberado: los demas casos A2A pasaban con el formato equivocado porque
  // cliente y servidor compartian el error. Verificado contra a2a-sdk 1.1.2.
  FRunner.AddCase('a2a.wire.v1')
    .Input('a2a:wire-v1')
    .ExpectEquals('card.supportedInterfaces=1|card.rootUrl=0|card.protocolVersion=0|' +
    'send.result.task=1|send.result.id=0|gettask.id=1|gettask.task=0');

  // La era 0.x sigue disponible para no romper integraciones previas
  FRunner.AddCase('a2a.wire.v03')
    .Input('a2a:wire-v03')
    .ExpectEquals('card.supportedInterfaces=0|card.rootUrl=1|send.result.task=0|send.result.id=1');

  // ListTasks: listado, filtro por estado y GetExtendedAgentCard deshabilitada.
  // -32007 es ExtendedAgentCardNotConfiguredError, el codigo que pide la spec
  // para este caso concreto (no el -32004 generico de UnsupportedOperation).
  FRunner.AddCase('a2a.listtasks')
    .Input('a2a:listtasks')
    .ExpectEquals('total=2|filtrado=2|inexistente=0|extcard=-32007');

  // Codigos de error y ErrorInfo exigidos por la spec, medidos con HTTP crudo.
  // Los codigos salen del TCK oficial (seccion 5.4 de la spec).
  // 'push' comprueba CreateTaskPushNotificationConfig SIN taskId: como las push
  // notifications si estan implementadas, el error correcto es InvalidParams.
  // TAiA2AAgentTool: el LLM ve el agente remoto como una funcion mas.
  // Se invoca el tool directamente (sin LLM) por el mismo choke point que
  // usaria el modelo, TAiFunctions.DoCallFunction.
  FRunner.AddCase('a2a.agenttool.chat')
    .Input('a2a:agenttool')
    .ExpectEquals('consulta>Uno');

  FRunner.AddCase('a2a.errors.codes')
    .Input('a2a:errorcodes')
    .ExpectEquals('push=-32602|version=-32009|ctype=-32005|sinmensaje=-32602|terminal=-32004|errorinfo=ok');

  // --- Guardrails ---
  FRunner.AddCase('policy.guard.blocklist')
    .Input('policy:blocklist')
    .ExpectEquals('blocked');

  FRunner.AddCase('policy.guard.allowlist')
    .Input('policy:allowlist')
    .ExpectEquals('allowed|blocked');

  FRunner.AddCase('policy.guard.arg-pattern')
    .Input('policy:argpattern')
    .ExpectEquals('blocked');

  FRunner.AddCase('policy.guard.programmatic-veto')
    .Input('policy:veto')
    .ExpectEquals('blocked');

  FRunner.AddCase('policy.guard.integration-not-executed')
    .Input('policy:integration')
    .ExpectContains('Blocked by guardrails')
    .ExpectContains('not-executed');

  // --- A2A: autorizacion ---
  // La card base se sirve sin credenciales (es descubrimiento: ahi es donde el
  // cliente lee que esquema usar). Y el secreto se compara EXACTO: con
  // SameText, 'clavesecreta' valia por 'ClaveSecreta'.
  FRunner.AddCase('a2a.auth.card-public-key-exact')
    .Input('a2a:auth')
    .ExpectEquals('card=200|sinclave=401|otrocase=401|conclave=200');

  // --- A2A: streaming con reanudacion ---
  FRunner.AddCase('a2a.stream.resume-input-required')
    .Input('a2a:stream-resume')
    .ExpectEquals('estado1=input-required|mismoTask=si|estado2=completed');

  // --- A2A: skills del Agent Card ---
  // Las skills se DECLARAN; no se derivan de los nodos. Un grafo normal tiene
  // nodos 'Nodo1'/'Nodo2' sin descripcion y publicarlos seria ruido.
  FRunner.AddCase('a2a.card.skills-declared')
    .Input('a2a:card-skills')
    .ExpectEquals('2|traducir|resumir|tags=2');

  FRunner.AddCase('a2a.card.skills-default')
    .Input('a2a:card-skills-default')
    .ExpectEquals('1|run-graph');

  // --- RAG: busqueda sin SearchOptions ---
  // Reventaba con AV: IfThen evalua las dos ramas, asi que
  // IfThen(Assigned(LOptions), LOptions.MinAbsoluteScoreEmbedding, 0.0)
  // desreferenciaba LOptions aunque fuera nil. Es alcanzable de verdad:
  // basta con no pasar Options y que el driver no tenga Owner.
  FRunner.AddCase('rag.search.nil-options')
    .Input('rag:search-nil-options')
    .ExpectEquals('1');

  // --- Chat: serializacion de tool results (familia OpenAI-compatible) ---
  // Chat Completions no admite imagenes en role 'tool' de ninguna forma, y
  // Groq ademas rechaza cualquier array ahi. La media nativa sale como un
  // mensaje 'user' sintetico DESPUES del grupo de tool results: con tool calls
  // paralelas, meterlo entre dos 'tool' es un 400.
  FRunner.AddCase('chat.toolresult.parallel-media')
    .Input('chat:toolresult-parallel')
    .ExpectEquals('5|tool|tool|user|imgs=2');

  // Sin cap_Image no se emite nada de media: se cae a la transcripcion que
  // dejo el bridge de vision.
  FRunner.AddCase('chat.toolresult.no-vision')
    .Input('chat:toolresult-novision')
    .ExpectEquals('2|trans=si|img=no');

  // TAiChat.RunNew ya inyecta la transcripcion en el Prompt del AskMsg, y en
  // una continuacion el AskMsg ES el mensaje tool: sin guarda, se duplicaba.
  FRunner.AddCase('chat.toolresult.no-dup-transcription')
    .Input('chat:toolresult-dup')
    .ExpectEquals('1');

  // Los adjuntos de texto se inlinean en el propio string del resultado: no
  // necesitan mensaje extra y antes se perdian enteros.
  FRunner.AddCase('chat.toolresult.text-inline')
    .Input('chat:toolresult-text')
    .ExpectEquals('1|inline=si|string=si');

  // --- Evals (autoprueba del propio runner) ---
  FRunner.AddCase('evals.self-check')
    .Input('policy:evals-self')
    .ExpectEquals('2|1');

  // --- Montaje concurrente de conexiones ---
  // Reproduce lo que hace un servidor HTTP con varios requests a la vez: cada
  // uno crea su TAiChatConnection y fija DriverName/Model/Params/AiFunctions.
  // Ese camino escribe en el registro global (SetModel -> RegisterUserParam) y
  // lo lee (UpdateAndApplyParams -> GetDriverParams) desde todos los hilos.
  // Sin red ni API keys: solo el montaje.
  FRunner.AddCase('conn.concurrent.setup')
    .Input('conn:concurrent-setup')
    .ExpectEquals('hung=0|errors=0|badmodel=0');

  // --- Jev (TypeSafe) ---
  // Sin red: TFakeJev sustituye DoPost. Validan el contrato con la API
  // (https://docs.typesafe.ai/api), no la calidad del modelo.
  FRunner.AddCase('jev.request.shape')
    .Input('jev:request')
    .ExpectEquals('model=jev-1.13.0|consulta=hola|dominio=choice|contable=Asientos y PUC|general=null|levels=3|noul.true=si|noul.false=no');

  FRunner.AddCase('jev.response.parse')
    .Input('jev:parse')
    .ExpectEquals('model=jev-1.13.0|choice=billing|conf=0.81|top2=billing,technical|sales=0.00|score=1.05|nivel1=0.95|noul=0.95|in=318|out=34');

  FRunner.AddCase('jev.retry.transient')
    .Input('jev:retry')
    .ExpectEquals('calls=3|choice=billing');

  FRunner.AddCase('jev.retry.exhausted')
    .Input('jev:retry-exhausted')
    .ExpectEquals('calls=2|status=429|lasterror=si');

  FRunner.AddCase('jev.validate.local')
    .Input('jev:validate')
    .ExpectEquals('una-opcion=error|nombre-repetido=error|score-11=error|sin-preguntas=error|calls=0');

  FRunner.AddCase('jev.error.http')
    .Input('jev:http-error')
    .ExpectEquals('status=401|lasterror=si|reintentos=no');

  // TAiJevRouterTool dentro de un grafo real: Router -(lmConditional)->
  // contable | tributario, con NextNo -> humano; todos desembocan en Fin.
  FRunner.AddCase('agents.jevrouter.route')
    .Input('jev:router-route')
    .ExpectEquals('esCompleted|hola>contable>Fin|choice=contable|conf=0.85|fuentes=0.80|error=no');

  // Confianza bajo MinConfidence: la ruta cae a NextNo aunque haya eleccion
  FRunner.AddCase('agents.jevrouter.low-confidence')
    .Input('jev:router-lowconf')
    .ExpectEquals('esCompleted|hola>humano>Fin|choice=tributario|conf=0.30|fuentes=0.10|error=no');

  // Jev caido (401): el grafo NO falla, toma la ruta de respaldo y deja el error
  FRunner.AddCase('agents.jevrouter.api-error')
    .Input('jev:router-error')
    .ExpectEquals('esCompleted|hola>humano>Fin|choice=|conf=|fuentes=|error=si');

  // SmartDispatch con ChatTools.DispatchClassifier sobre un TAiChat real: el
  // tag decidido por el clasificador va directo a la tool, sin pase por LLM
  // (la URL apunta a un puerto cerrado: cualquier llamada de red fallaria).
  FRunner.AddCase('chat.smartdispatch.classifier')
    .Input('jev:dispatch-chat')
    .ExpectEquals('tags=IMAGEGEN,CHAT|clasificador=1|imagen=1|prompt=dibuja un gato rojo');

  // TAiJevDispatchClassifier: solo ofrece los tags recibidos, respeta
  // MinConfidence y no llama a Jev cuando solo queda CHAT
  FRunner.AddCase('jev.dispatch.classifier')
    .Input('jev:dispatch')
    .ExpectEquals('alta=IMAGEGEN|opciones=IMAGEGEN,WEBSEARCH,CHAT|baja=|ultima=IMAGEGEN|solo-chat=CHAT|calls=2');

  // TAiGuardrails.Classifier con TAiJevGuardrailClassifier
  FRunner.AddCase('policy.guard.jev-classifier')
    .Input('jev:guard')
    .ExpectEquals('riesgo=blocked|motivo=Jev risk 0.95 >= 0.50|seguro=allowed|lista=blocked|jev-consultado=no|' +
      'error-cerrado=blocked|error-abierto=allowed');

  // lmExpression con punto decimal en un Windows con coma decimal: antes
  // '10.25 > 9.5' se comparaba como texto y daba False
  FRunner.AddCase('agents.expression.decimal-point')
    .Input('agents:expr-decimal')
    .ExpectEquals('ge=True|gt=True|lt=False');
end;

function TRegressionSuite.Dispatch(const AScenario: string): string;
begin
  if AScenario.StartsWith('mcp:') then
    Result := RunMcpScenario(AScenario)
  else if AScenario.StartsWith('agents:') or AScenario.StartsWith('a2a:') then
    Result := RunAgentScenario(AScenario)
  else if AScenario.StartsWith('rag:') then
    Result := RunRagScenario(AScenario)
  else if AScenario.StartsWith('chat:') then
    Result := RunChatScenario(AScenario)
  else if AScenario.StartsWith('policy:') then
    Result := RunPolicyScenario(AScenario)
  else if AScenario.StartsWith('conn:') then
    Result := RunConnScenario(AScenario)
  else if AScenario.StartsWith('jev:') then
    Result := RunJevScenario(AScenario)
  else
    raise Exception.Create('Escenario desconocido: ' + AScenario);
end;

// -----------------------------------------------------------------------------
// MCP
// -----------------------------------------------------------------------------

function TRegressionSuite.RunMcpScenario(const AScenario: string): string;
var
  Server: TAiMCPHttpServer;
  Legacy: TLegacyOnlyMCPServer;
  Client: TMCPClientHttp;
  Handlers: TFixtureHandlers;
  Args, Res: TJSONObject;
  Media: TObjectList<TAiMediaFile>;
  Tools: TJSONObject;
begin
  Result := '';
  Handlers := TFixtureHandlers.Create;
  Media := TObjectList<TAiMediaFile>.Create(True);
  Client := TMCPClientHttp.Create(nil);
  try
    Client.Params.Values['RpcEndpointSuffix'] := '';
    Client.Params.Values['InitializeEndpointSuffix'] := '';
    Client.Params.Values['NotificationEndpointSuffix'] := '';
    Client.Params.Values['Timeout'] := '15000';

    if AScenario = 'mcp:negotiate-legacy' then
    begin
      // Servidor que solo entiende el handshake antiguo
      Legacy := TLegacyOnlyMCPServer.Create(PORT_MCP_LEGACY);
      try
        Client.Params.Values['URL'] := Format('http://localhost:%d/mcp', [PORT_MCP_LEGACY]);
        Client.Initialize;
        Result := Client.NegotiatedProtocol;
      finally
        Legacy.Free;
      end;
      Exit;
    end;

    // Resto de escenarios: servidor MakerAI dual-era in-process
    Server := TAiMCPHttpServer.Create(nil);
    try
      Server.Port := PORT_MCP_MODERN;
      Server.RegisterTool('echo_upper',
        function: IAiMCPTool
        begin
          Result := TEchoTool.Create;
        end);
      Server.RegisterTool('confirm_op',
        function: IAiMCPTool
        begin
          Result := TConfirmTool.Create;
        end);
      Server.Start;

      Client.Params.Values['URL'] := Format('http://localhost:%d%s', [PORT_MCP_MODERN, Server.Endpoint]);
      Client.Initialize;

      if AScenario = 'mcp:negotiate-modern' then
        Result := Client.NegotiatedProtocol

      else if AScenario = 'mcp:tools-list' then
      begin
        Tools := Client.ListTools;
        try
          if Assigned(Tools) then
            Result := Tools.ToJSON;
        finally
          Tools.Free;
        end;
      end

      else if AScenario = 'mcp:call-echo' then
      begin
        Args := TJSONObject.Create;
        Args.AddPair('text', 'hola mundo');
        Res := Client.CallTool('echo_upper', Args, Media);
        try
          if Assigned(Res) then
            Result := Res.ToJSON;
        finally
          Res.Free;
        end;
      end

      else if (AScenario = 'mcp:mrtr-accept') or (AScenario = 'mcp:mrtr-message') then
      begin
        Client.OnInputRequired := Handlers.InputRequired;
        Args := TJSONObject.Create;
        Args.AddPair('operation', 'borrar');
        Res := Client.CallTool('confirm_op', Args, Media);
        try
          if AScenario = 'mcp:mrtr-message' then
            Result := Handlers.LastElicitMessage
          else if Assigned(Res) then
            Result := Res.ToJSON;
        finally
          Res.Free;
        end;
      end

      else if AScenario = 'mcp:mrtr-nohandler' then
      begin
        // Sin handler asignado: el guard debe devolver un error explicito
        Args := TJSONObject.Create;
        Args.AddPair('operation', 'borrar');
        Res := Client.CallTool('confirm_op', Args, Media);
        try
          if Assigned(Res) then
            Result := Res.ToJSON;
        finally
          Res.Free;
        end;
      end

      else
        raise Exception.Create('Escenario MCP desconocido: ' + AScenario);
    finally
      Server.Stop;
      Server.Free;
    end;
  finally
    Client.Free;
    Media.Free;
    Handlers.Free;
  end;
end;

// -----------------------------------------------------------------------------
// Agentes y A2A
// -----------------------------------------------------------------------------

function TRegressionSuite.RunAgentScenario(const AScenario: string): string;
var
  Handlers: TFixtureHandlers;
  Agents, Remoto, Local: TAIAgentManager;
  Server: TAiA2AServer;
  Client: TAiA2AClient;
  Card, Task: TJSONObject;
  OutText: string;
  Tool: TAiA2ARemoteAgentTool;

  procedure WaitGraph(A: TAIAgentManager);
  begin
    while A.Busy do
    begin
      CheckSynchronize;
      Sleep(20);
    end;
    CheckSynchronize;
  end;

begin
  // Los flujos de orquestacion arman su propia topologia (pool, suspension,
  // no bloqueante) y viven en su propia rutina.
  if MatchStr(AScenario, ['a2a:concurrency', 'a2a:hitl', 'a2a:fedhitl', 'a2a:nonblocking', 'a2a:naming',
    'a2a:cancelterm', 'a2a:wire-v1', 'a2a:wire-v03', 'a2a:listtasks', 'a2a:errorcodes', 'a2a:agenttool',
    'a2a:card-skills', 'a2a:card-skills-default', 'a2a:stream-resume',
    'a2a:auth']) then
    Exit(RunA2AFlowScenario(AScenario));

  if AScenario = 'agents:expr-decimal' then
  begin
    var Vars := TDictionary<string, TValue>.Create;
    var OldSep := FormatSettings.DecimalSeparator;
    try
      Vars.Add('p', TValue.From<string>('0.85'));
      Vars.Add('q', TValue.From<string>('10.25'));
      FormatSettings.DecimalSeparator := ',';
      Result := 'ge=' + BoolToStr(EvalCondition('p >= 0.7', Vars), True) +
        '|gt=' + BoolToStr(EvalCondition('q > 9.5', Vars), True) +
        '|lt=' + BoolToStr(EvalCondition('q < 9.5', Vars), True);
    finally
      FormatSettings.DecimalSeparator := OldSep;
      Vars.Free;
    end;
    Exit;
  end;

  Result := '';
  Handlers := TFixtureHandlers.Create;
  try
    if AScenario = 'agents:graph' then
    begin
      Agents := TAIAgentManager.Create(nil);
      try
        Agents.Name := 'SuiteGraph';
        Agents.AddNode('A', Handlers.NodeExec).AddNode('B', Handlers.NodeExec).AddNode('C', Handlers.NodeExec);
        Agents.AddEdge('A', 'B');
        Agents.AddEdge('B', 'C');
        Agents.SetEntryPoint('A').SetFinishPoint('C');
        Agents.Run('ping');
        WaitGraph(Agents);
        Result := GetEnumName(TypeInfo(TAgentExecutionStatus), Ord(Agents.Blackboard.GetStatus)) +
          '|' + Agents.EndNode.Output;
      finally
        Agents.Free;
      end;
      Exit;
    end;

    if AScenario.StartsWith('a2a:') then
    begin
      Agents := TAIAgentManager.Create(nil);
      Server := TAiA2AServer.Create(nil);
      Client := TAiA2AClient.Create(nil);
      try
        Agents.Name := 'SuiteA2AGraph';
        Agents.AddNode('Uno', Handlers.NodeExec).AddNode('Dos', Handlers.NodeExec);
        Agents.AddEdge('Uno', 'Dos');
        Agents.SetEntryPoint('Uno').SetFinishPoint('Dos');

        Server.AgentManager := Agents;
        Server.AgentName := 'Suite Agent';
        Server.AgentDescription := 'Agente de la suite de regresion';
        Server.Port := PORT_A2A;
        Server.Active := True;
        Client.Url := Format('http://localhost:%d', [PORT_A2A]);

        if AScenario = 'a2a:card' then
        begin
          Card := Client.FetchAgentCard;
          try
            Result := Card.ToJSON;
          finally
            Card.Free;
          end;
        end

        else if AScenario = 'a2a:send' then
        begin
          Task := Client.SendText('hola', OutText);
          try
            Result := Client.LastState + '|' + OutText;
          finally
            Task.Free;
          end;
        end

        else if AScenario = 'a2a:federation' then
        begin
          // El agente publicado pasa a ser el "remoto"; un grafo local delega en el
          Remoto := Agents;
          Local := TAIAgentManager.Create(nil);
          try
            Local.Name := 'SuiteLocalGraph';
            Local.AddNode('Origen', Handlers.NodeExec);
            Local.AddNode('Delegado', nil);
            Tool := TAiA2ARemoteAgentTool.Create(Local);
            Tool.AgentUrl := Format('http://localhost:%d', [PORT_A2A]);
            Local.FindNode('Delegado').Tool := Tool;
            Local.AddEdge('Origen', 'Delegado');
            Local.SetEntryPoint('Origen').SetFinishPoint('Delegado');
            Local.Run('fed');
            WaitGraph(Local);
            Result := GetEnumName(TypeInfo(TAgentExecutionStatus), Ord(Local.Blackboard.GetStatus)) +
              '|' + Local.EndNode.Output;
          finally
            Local.Free;
          end;
          if Remoto = nil then ; // (silencia hint: Remoto es alias de Agents)
        end

        else
          raise Exception.Create('Escenario A2A desconocido: ' + AScenario);

        Server.Active := False;
      finally
        Client.Free;
        Server.Free;
        Agents.Free;
      end;
      Exit;
    end;

    raise Exception.Create('Escenario de agentes desconocido: ' + AScenario);
  finally
    Handlers.Free;
  end;
end;

// -----------------------------------------------------------------------------
// A2A: flujos de orquestacion
// -----------------------------------------------------------------------------

function TRegressionSuite.RunA2AFlowScenario(const AScenario: string): string;
var
  Handlers: TFixtureHandlers;
  Server: TAiA2AServer;
  Client: TAiA2AClient;
  Agents, Local: TAIAgentManager;
  Tool: TAiA2ARemoteAgentTool;
  Task: TJSONObject;
  OutText, TaskId, CtxId, Url: string;
  OkCount, BadCount: Integer;
  Tasks: TArray<ITask>;
  I, Waited: Integer;
  St1, St2: TAiA2ATaskState;
  // Formato de cable: HTTP crudo, sin pasar por TAiA2AClient
  Http: THTTPClient;
  Body: TStringStream;
  Card, RawObj, ResObj: TJSONObject;
  Funcs: TAiFunctions;
  AgentTool: TAiA2AAgentTool;
  ToolCall: TAiToolsFunction;

  function StateName(AValue: TAiA2ATaskState): string;
  begin
    Result := GetEnumName(TypeInfo(TAiA2ATaskState), Ord(AValue));
  end;

  procedure WaitGraph(A: TAIAgentManager);
  begin
    while A.Busy do
    begin
      CheckSynchronize;
      Sleep(20);
    end;
    CheckSynchronize;
  end;

begin
  Result := '';
  Url := Format('http://localhost:%d', [PORT_A2A]);
  Handlers := TFixtureHandlers.Create;
  Server := TAiA2AServer.Create(nil);
  Agents := nil;
  try
    Server.Port := PORT_A2A;
    Server.AgentName := 'Suite Flow Agent';

    // --- Pool: tres tasks simultaneos contra el mismo agente ---
    if AScenario = 'a2a:concurrency' then
    begin
      Server.OnAcquireManager := Handlers.AcquireManager;
      Server.MaxConcurrentTasks := 3;
      Server.Active := True;

      OkCount := 0;
      BadCount := 0;
      SetLength(Tasks, 3);
      for I := 0 to 2 do
        Tasks[I] := TTask.Run(
          procedure
          var
            C: TAiA2AClient;
            T: TJSONObject;
            S: string;
          begin
            C := TAiA2AClient.Create(nil);
            try
              C.Url := Url;
              try
                T := C.SendText('p', S);
                try
                  if C.LastTaskState = tsCompleted then
                    TInterlocked.Increment(OkCount)
                  else
                    TInterlocked.Increment(BadCount);
                finally
                  T.Free;
                end;
              except
                TInterlocked.Increment(BadCount); // AgentBusy o transporte
              end;
            finally
              C.Free;
            end;
          end);
      TTask.WaitForAll(Tasks);
      Result := Format('completed=%d|failed=%d', [OkCount, BadCount]);
    end

    // --- Autorizacion por ApiKey ---
    else if AScenario = 'a2a:auth' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteAuthGraph';
      Agents.AddNode('Uno', Handlers.NodeExec);
      Agents.SetEntryPoint('Uno').SetFinishPoint('Uno');
      Server.AgentManager := Agents;
      Server.ApiKey := 'ClaveSecreta';
      Server.Active := True;

      Http := THTTPClient.Create;
      try
        Http.ConnectionTimeout := 15000;
        Http.ResponseTimeout := 15000;

        // 1. La card base se lee SIN credenciales: es descubrimiento.
        Result := 'card=' + IntToStr(Http.Get(Url + '/.well-known/agent-card.json').StatusCode);

        // 2. Un RPC sin credenciales, no.
        Http.ContentType := 'application/json';
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":1,"method":"GetTask","params":{"taskId":"x"}}', TEncoding.UTF8);
        try
          Result := Result + '|sinclave=' + IntToStr(Http.Post(Url + '/', Body).StatusCode);
        finally
          Body.Free;
        end;

        // 3. La clave con otras mayusculas NO vale: el secreto se compara exacto.
        Http.CustomHeaders['Authorization'] := 'Bearer clavesecreta';
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":2,"method":"GetTask","params":{"taskId":"x"}}', TEncoding.UTF8);
        try
          Result := Result + '|otrocase=' + IntToStr(Http.Post(Url + '/', Body).StatusCode);
        finally
          Body.Free;
        end;

        // 4. Con la clave exacta pasa (el task no existe, pero eso ya es 200 +
        // error RPC). Cliente nuevo a proposito: CustomHeaders de THTTPClient
        // no reemplaza, acumula, y se enviarian las dos Authorization.
        Http.Free;
        Http := THTTPClient.Create;
        Http.ConnectionTimeout := 15000;
        Http.ResponseTimeout := 15000;
        Http.ContentType := 'application/json';
        Http.CustomHeaders['Authorization'] := 'Bearer ClaveSecreta';
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":3,"method":"GetTask","params":{"taskId":"x"}}', TEncoding.UTF8);
        try
          Result := Result + '|conclave=' + IntToStr(Http.Post(Url + '/', Body).StatusCode);
        finally
          Body.Free;
        end;
      finally
        Http.Free;
      end;
    end

    // --- Streaming: reanudar un task suspendido con el mismo taskId ---
    // Se habla SSE crudo a proposito, sin TAiA2AClient: es la unica forma de
    // comprobar que el servidor reanuda de verdad y no abre un task nuevo.
    else if AScenario = 'a2a:stream-resume' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteStreamResumeGraph';
      Agents.AddNode('Espera', Handlers.NodeSuspendOnce);
      Agents.SetEntryPoint('Espera').SetFinishPoint('Espera');
      Server.AgentManager := Agents;
      Server.Active := True;

      Http := THTTPClient.Create;
      try
        Http.ConnectionTimeout := 30000;
        Http.ResponseTimeout := 30000;
        Http.ContentType := 'application/json';

        // Turno 1: el grafo se suspende y el stream cierra en input-required.
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":1,"method":"SendStreamingMessage","params":{"message":' +
          '{"messageId":"s1","role":"ROLE_USER","parts":[{"text":"hola"}]}}}', TEncoding.UTF8);
        var Sse1: string;
        try
          Sse1 := Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8);
        finally
          Body.Free;
        end;

        // El primer evento del stream es el snapshot {"task":{...}}
        var LLine := Copy(Sse1, Pos('data: ', Sse1) + 6, MaxInt);
        LLine := Trim(Copy(LLine, 1, Pos(#10, LLine) - 1));
        var LEv := TJSONObject.ParseJSONValue(LLine) as TJSONObject;
        try
          TaskId := (LEv.GetValue('result') as TJSONObject)
            .GetValue<TJSONObject>('task').GetValue<string>('id');
        finally
          LEv.Free;
        end;
        Result := 'estado1=' + IfThen(Pos('INPUT_REQUIRED', Sse1) > 0, 'input-required', '?');

        // Turno 2: mismo taskId. Debe REANUDAR el grafo suspendido.
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":2,"method":"SendStreamingMessage","params":{"message":' +
          '{"messageId":"s2","taskId":"' + TaskId + '","role":"ROLE_USER",' +
          '"parts":[{"text":"ok"}]}}}', TEncoding.UTF8);
        var Sse2: string;
        try
          Sse2 := Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8);
        finally
          Body.Free;
        end;

        Result := Result + '|mismoTask=' + IfThen(Pos(TaskId, Sse2) > 0, 'si', 'no');
        Result := Result + '|estado2=' + IfThen(Pos('COMPLETED', Sse2) > 0, 'completed', '?');
      finally
        Http.Free;
      end;
    end

    // --- Skills declaradas en la Agent Card ---
    else if AScenario = 'a2a:card-skills' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteSkillsGraph';
      Agents.AddNode('Uno', Handlers.NodeExec);
      Agents.SetEntryPoint('Uno').SetFinishPoint('Uno');
      Server.AgentManager := Agents;
      Server.Skills.AddSkill('traducir', 'Traductor', 'Traduce texto', 'idiomas, texto');
      Server.Skills.AddSkill('resumir', 'Resumidor', 'Resume documentos');
      Server.Active := True;

      Http := THTTPClient.Create;
      try
        Http.ConnectionTimeout := 15000;
        Http.ResponseTimeout := 15000;
        Card := TJSONObject(TJSONObject.ParseJSONValue(Http.Get(Url + '/.well-known/agent-card.json')
          .ContentAsString(TEncoding.UTF8)));
        try
          var LArr := Card.GetValue('skills') as TJSONArray;
          Result := IntToStr(LArr.Count);
          for var K := 0 to LArr.Count - 1 do
            Result := Result + '|' + (LArr.Items[K] as TJSONObject).GetValue<string>('id');
          // Los tags separados por coma se emiten como array.
          Result := Result + '|tags=' +
            IntToStr(((LArr.Items[0] as TJSONObject).GetValue('tags') as TJSONArray).Count);
        finally
          Card.Free;
        end;
      finally
        Http.Free;
      end;
    end

    // --- Sin skills declaradas: la card nunca sale vacia ---
    else if AScenario = 'a2a:card-skills-default' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteSkillsDefGraph';
      Agents.AddNode('Uno', Handlers.NodeExec);
      Agents.SetEntryPoint('Uno').SetFinishPoint('Uno');
      Server.AgentManager := Agents;
      Server.Active := True;

      Http := THTTPClient.Create;
      try
        Http.ConnectionTimeout := 15000;
        Http.ResponseTimeout := 15000;
        Card := TJSONObject(TJSONObject.ParseJSONValue(Http.Get(Url + '/.well-known/agent-card.json')
          .ContentAsString(TEncoding.UTF8)));
        try
          var LArr := Card.GetValue('skills') as TJSONArray;
          Result := IntToStr(LArr.Count) + '|' +
            (LArr.Items[0] as TJSONObject).GetValue<string>('id');
        finally
          Card.Free;
        end;
      finally
        Http.Free;
      end;
    end

    // --- Human-in-the-loop directo sobre A2A ---
    else if AScenario = 'a2a:hitl' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteHitlGraph';
      Agents.AddNode('Uno', Handlers.NodeExec).AddNode('Espera', Handlers.NodeSuspendOnce);
      Agents.AddEdge('Uno', 'Espera');
      Agents.SetEntryPoint('Uno').SetFinishPoint('Espera');
      Server.AgentManager := Agents;
      Server.Active := True;

      Client := TAiA2AClient.Create(nil);
      try
        Client.Url := Url;
        Task := Client.SendText('hola', OutText);
        try
          St1 := Client.LastTaskState;
          TaskId := Client.LastTaskId;
          CtxId := Client.LastContextId;
          Result := StateName(St1) + ' [' + Client.LastStatusMessage + ']';
        finally
          Task.Free;
        end;

        // Mismo taskId => reanudacion del grafo suspendido, no un task nuevo.
        Task := Client.SendTextEx('ok', TaskId, CtxId, OutText);
        try
          Result := Result + '|' + StateName(Client.LastTaskState) + '|' + OutText;
        finally
          Task.Free;
        end;
      finally
        Client.Free;
      end;
    end

    // --- Human-in-the-loop atravesando una federacion ---
    else if AScenario = 'a2a:fedhitl' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteFedRemote';
      Agents.AddNode('Espera', Handlers.NodeSuspendOnce);
      Agents.SetEntryPoint('Espera').SetFinishPoint('Espera');
      Server.AgentManager := Agents;
      Server.Active := True;

      Local := TAIAgentManager.Create(nil);
      try
        Local.Name := 'SuiteFedLocal';
        Local.AddNode('Origen', Handlers.NodeExec);
        Local.AddNode('Delegado', nil);
        Tool := TAiA2ARemoteAgentTool.Create(Local);
        Tool.AgentUrl := Url;
        Local.FindNode('Delegado').Tool := Tool;
        Local.AddEdge('Origen', 'Delegado');
        Local.SetEntryPoint('Origen').SetFinishPoint('Delegado');

        Local.Run('fed');
        WaitGraph(Local);
        Result := GetEnumName(TypeInfo(TAgentExecutionStatus), Ord(Local.Blackboard.GetStatus));

        // El nodo local quedo suspendido esperando al humano: al reanudarlo la
        // respuesta viaja al MISMO task remoto.
        Local.ResumeThread(Local.CurrentThreadID, 'Delegado', 'si');
        WaitGraph(Local);
        Result := Result + '|' + GetEnumName(TypeInfo(TAgentExecutionStatus), Ord(Local.Blackboard.GetStatus)) + '|' +
          Local.EndNode.Output;
      finally
        Local.Free;
      end;
    end

    // --- blocking=false + GetTask ---
    else if AScenario = 'a2a:nonblocking' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteSlowGraph';
      Agents.AddNode('Uno', Handlers.NodeExecSlow).AddNode('Dos', Handlers.NodeExec);
      Agents.AddEdge('Uno', 'Dos');
      Agents.SetEntryPoint('Uno').SetFinishPoint('Dos');
      Server.AgentManager := Agents;
      Server.Active := True;

      Client := TAiA2AClient.Create(nil);
      try
        Client.Url := Url;
        Task := Client.SendTextEx('lento', '', '', OutText, False);
        try
          St1 := Client.LastTaskState;
          TaskId := Client.LastTaskId;
        finally
          Task.Free;
        end;

        St2 := tsUnknown;
        OutText := '';
        Waited := 0;
        while Waited < 10000 do
        begin
          Task := Client.GetTask(TaskId);
          try
            St2 := Client.LastTaskState;
            if St2 in [tsCompleted, tsFailed, tsCanceled] then
            begin
              OutText := TAiA2AClient.ArtifactsText(Task);
              Break;
            end;
          finally
            Task.Free;
          end;
          Sleep(30);
          Inc(Waited, 30);
        end;
        Result := StateName(St1) + '|' + StateName(St2) + '|' + OutText;
      finally
        Client.Free;
      end;
    end

    // --- Tolerancia de literales (forma JSON-RPC) ---
    else if AScenario = 'a2a:naming' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteNamingGraph';
      Agents.AddNode('Uno', Handlers.NodeExec).AddNode('Dos', Handlers.NodeExec);
      Agents.AddEdge('Uno', 'Dos');
      Agents.SetEntryPoint('Uno').SetFinishPoint('Dos');
      Server.AgentManager := Agents;
      Server.StateNaming := anLower;
      Server.Active := True;

      Client := TAiA2AClient.Create(nil);
      try
        Client.Url := Url;
        Task := Client.SendText('hola', OutText);
        try
          Result := Client.LastState + '|' + StateName(Client.LastTaskState) + '|' + OutText;
        finally
          Task.Free;
        end;
      finally
        Client.Free;
      end;
    end

    // --- Cancelar un task ya terminal ---
    else if AScenario = 'a2a:cancelterm' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteCancelGraph';
      Agents.AddNode('Uno', Handlers.NodeExec);
      Agents.SetEntryPoint('Uno').SetFinishPoint('Uno');
      Server.AgentManager := Agents;
      Server.Active := True;

      Client := TAiA2AClient.Create(nil);
      try
        Client.Url := Url;
        Task := Client.SendText('hola', OutText);
        try
          TaskId := Client.LastTaskId;
        finally
          Task.Free;
        end;
        try
          Task := Client.CancelTask(TaskId);
          Task.Free;
          Result := 'cancelado (inesperado)';
        except
          on E: Exception do
            Result := E.Message;
        end;
      finally
        Client.Free;
      end;
    end

    // --- Formato de cable, hablando HTTP crudo (sin TAiA2AClient) ---
    else if AScenario.StartsWith('a2a:wire-') then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteWireGraph';
      Agents.AddNode('Uno', Handlers.NodeExec);
      Agents.SetEntryPoint('Uno').SetFinishPoint('Uno');
      Server.AgentManager := Agents;
      if AScenario = 'a2a:wire-v03' then
        Server.WireEra := weV03
      else
        Server.WireEra := weV1;
      Server.Active := True;

      Http := THTTPClient.Create;
      try
        Http.ConnectionTimeout := 15000;
        Http.ResponseTimeout := 15000;
        Http.ContentType := 'application/json';

        // 1. Agent Card cruda
        Card := TJSONObject(TJSONObject.ParseJSONValue(Http.Get(Url + '/.well-known/agent-card.json')
          .ContentAsString(TEncoding.UTF8)));
        try
          Result := 'card.supportedInterfaces=' + IfThen(Card.GetValue('supportedInterfaces') <> nil, '1', '0');
          Result := Result + '|card.rootUrl=' + IfThen(Card.GetValue('url') <> nil, '1', '0');
          if AScenario = 'a2a:wire-v1' then
            Result := Result + '|card.protocolVersion=' + IfThen(Card.GetValue('protocolVersion') <> nil, '1', '0');
        finally
          Card.Free;
        end;

        // 2. SendMessage crudo: el Task debe ir envuelto en {"task": ...}
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":1,"method":"SendMessage","params":{"message":{"messageId":"m1",' +
          '"role":"ROLE_USER","parts":[{"text":"wire"}]}}}', TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body)
            .ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          RawObj.TryGetValue<TJSONObject>('result', ResObj);
          Result := Result + '|send.result.task=' + IfThen(ResObj.GetValue('task') <> nil, '1', '0');
          Result := Result + '|send.result.id=' + IfThen(ResObj.GetValue('id') <> nil, '1', '0');
          if ResObj.GetValue('task') <> nil then
            TaskId := ResObj.GetValue<TJSONObject>('task').GetValue<string>('id', '')
          else
            TaskId := ResObj.GetValue<string>('id', '');
        finally
          RawObj.Free;
        end;

        // 3. GetTask crudo: el Task va DIRECTO, sin wrapper, en ambas eras
        if AScenario = 'a2a:wire-v1' then
        begin
          Body := TStringStream.Create(Format(
            '{"jsonrpc":"2.0","id":2,"method":"GetTask","params":{"id":"%s"}}', [TaskId]), TEncoding.UTF8);
          try
            RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body)
              .ContentAsString(TEncoding.UTF8)));
          finally
            Body.Free;
          end;
          try
            RawObj.TryGetValue<TJSONObject>('result', ResObj);
            Result := Result + '|gettask.id=' + IfThen(ResObj.GetValue('id') <> nil, '1', '0');
            Result := Result + '|gettask.task=' + IfThen(ResObj.GetValue('task') <> nil, '1', '0');
          finally
            RawObj.Free;
          end;
        end;
      finally
        Http.Free;
      end;
    end

    // --- ListTasks y GetExtendedAgentCard, tambien por HTTP crudo ---
    else if AScenario = 'a2a:listtasks' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteListGraph';
      Agents.AddNode('Uno', Handlers.NodeExec);
      Agents.SetEntryPoint('Uno').SetFinishPoint('Uno');
      Server.AgentManager := Agents;
      Server.Active := True;

      Client := TAiA2AClient.Create(nil);
      Http := THTTPClient.Create;
      try
        Client.Url := Url;
        Http.ConnectionTimeout := 15000;
        Http.ResponseTimeout := 15000;
        Http.ContentType := 'application/json';

        // Dos tasks para tener algo que listar
        for I := 1 to 2 do
        begin
          Task := Client.SendText('t' + IntToStr(I), OutText);
          Task.Free;
        end;

        // Listado completo
        Body := TStringStream.Create('{"jsonrpc":"2.0","id":1,"method":"ListTasks","params":{}}', TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          RawObj.TryGetValue<TJSONObject>('result', ResObj);
          Result := 'total=' + IntToStr(ResObj.GetValue<TJSONArray>('tasks').Count);
        finally
          RawObj.Free;
        end;

        // Filtro por estado: los dos completaron
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":2,"method":"ListTasks","params":{"status":"TASK_STATE_COMPLETED"}}', TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          RawObj.TryGetValue<TJSONObject>('result', ResObj);
          Result := Result + '|filtrado=' + IntToStr(ResObj.GetValue<TJSONArray>('tasks').Count);
        finally
          RawObj.Free;
        end;

        // Filtro por un contextId que no existe
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":3,"method":"ListTasks","params":{"contextId":"no-existe"}}', TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          RawObj.TryGetValue<TJSONObject>('result', ResObj);
          Result := Result + '|inexistente=' + IntToStr(ResObj.GetValue<TJSONArray>('tasks').Count);
        finally
          RawObj.Free;
        end;

        // GetExtendedAgentCard sin habilitar -> UnsupportedOperation
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":4,"method":"GetExtendedAgentCard","params":{}}', TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          Result := Result + '|extcard=' +
            IntToStr(RawObj.GetValue<TJSONObject>('error').GetValue<Integer>('code'));
        finally
          RawObj.Free;
        end;
      finally
        Http.Free;
        Client.Free;
      end;
    end

    // --- Codigos de error y ErrorInfo, con HTTP crudo ---
    else if AScenario = 'a2a:errorcodes' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteErrGraph';
      Agents.AddNode('Uno', Handlers.NodeExec);
      Agents.SetEntryPoint('Uno').SetFinishPoint('Uno');
      Server.AgentManager := Agents;
      Server.Active := True;

      Client := TAiA2AClient.Create(nil);
      Http := THTTPClient.Create;
      try
        Client.Url := Url;
        Http.ConnectionTimeout := 15000;
        Http.ResponseTimeout := 15000;

        // Helper local: postea y devuelve el codigo de error
        // (se repite el patron por claridad, no hay closures 'of object' aqui)
        Http.ContentType := 'application/json';

        // Push notifications no soportadas -> -32003, no MethodNotFound
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":1,"method":"CreateTaskPushNotificationConfig","params":{}}', TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          Result := 'push=' + IntToStr(RawObj.GetValue<TJSONObject>('error').GetValue<Integer>('code'));
        finally
          RawObj.Free;
        end;

        // Version no soportada -> -32009
        Body := TStringStream.Create(
          '{"jsonrpc":"2.0","id":2,"method":"GetTask","params":{"id":"x"}}', TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(
            Http.Post(Url + '/', Body, nil, [TNameValuePair.Create('A2A-Version', '9.9')])
            .ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          Result := Result + '|version=' + IntToStr(RawObj.GetValue<TJSONObject>('error').GetValue<Integer>('code'));
          // ErrorInfo va en un ARRAY, y solo en los errores PROPIOS de A2A:
          // los estandar de JSON-RPC (-32602 y similares) no lo llevan.
          if (RawObj.GetValue<TJSONObject>('error').GetValue('data') is TJSONArray) then
            OutText := 'ok'
          else
            OutText := 'falta-array';
        finally
          RawObj.Free;
        end;

        // Content-Type equivocado -> -32005, no ParseError
        Http.ContentType := 'text/plain';
        Body := TStringStream.Create('{"jsonrpc":"2.0","id":3,"method":"GetTask","params":{"id":"x"}}',
          TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          Result := Result + '|ctype=' + IntToStr(RawObj.GetValue<TJSONObject>('error').GetValue<Integer>('code'));
        finally
          RawObj.Free;
        end;
        Http.ContentType := 'application/json';

        // SendMessage sin 'message' -> InvalidParams
        Body := TStringStream.Create('{"jsonrpc":"2.0","id":4,"method":"SendMessage","params":{}}', TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          Result := Result + '|sinmensaje=' + IntToStr(RawObj.GetValue<TJSONObject>('error').GetValue<Integer>('code'));
        finally
          RawObj.Free;
        end;

        // Continuar un task terminal -> UnsupportedOperation
        Task := Client.SendText('hola', TaskId);
        try
          TaskId := Client.LastTaskId;
        finally
          Task.Free;
        end;
        Body := TStringStream.Create(Format(
          '{"jsonrpc":"2.0","id":5,"method":"SendMessage","params":{"message":{"messageId":"m2",' +
          '"role":"ROLE_USER","taskId":"%s","parts":[{"text":"otra vez"}]}}}', [TaskId]), TEncoding.UTF8);
        try
          RawObj := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(Url + '/', Body).ContentAsString(TEncoding.UTF8)));
        finally
          Body.Free;
        end;
        try
          Result := Result + '|terminal=' + IntToStr(RawObj.GetValue<TJSONObject>('error').GetValue<Integer>('code'));
        finally
          RawObj.Free;
        end;

        Result := Result + '|errorinfo=' + OutText;
      finally
        Http.Free;
        Client.Free;
      end;
    end

    // --- El agente remoto expuesto como herramienta de chat ---
    else if AScenario = 'a2a:agenttool' then
    begin
      Agents := TAIAgentManager.Create(nil);
      Agents.Name := 'SuiteToolGraph';
      Agents.AddNode('Uno', Handlers.NodeExec);
      Agents.SetEntryPoint('Uno').SetFinishPoint('Uno');
      Server.AgentManager := Agents;
      Server.Active := True;

      Funcs := TAiFunctions.Create(nil);
      AgentTool := TAiA2AAgentTool.Create(nil);
      ToolCall := TAiToolsFunction.Create;
      try
        AgentTool.AgentUrl := Url;
        AgentTool.ToolName := 'preguntar_agente';
        AgentTool.Functions := Funcs; // al asignarlo se registra la funcion

        ToolCall.Name := 'preguntar_agente';
        ToolCall.Arguments := '{"query":"consulta"}';
        Funcs.DoCallFunction(ToolCall);
        Result := ToolCall.Response;
      finally
        ToolCall.Free;
        AgentTool.Free;
        Funcs.Free;
      end;
    end

    else
      raise Exception.Create('Escenario A2A de flujo desconocido: ' + AScenario);

    Server.Active := False;
  finally
    Server.Free;
    Agents.Free;
    Handlers.Free;
  end;
end;

// -----------------------------------------------------------------------------
// Guardrails y evals
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Chat: serializacion de tool results
// -----------------------------------------------------------------------------

function TRegressionSuite.RunChatScenario(const AScenario: string): string;

  function NewImage(const AName: string): TAiMediaFile;
  begin
    Result := TAiMediaFile.Create;
    // PNG 1x1 valido
    Result.LoadFromBase64(AName, 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJ' +
      'AAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==');
  end;

  function NewText(const AName, AContent: string): TAiMediaFile;
  begin
    Result := TAiMediaFile.Create;
    Result.LoadFromBase64(AName,
      TNetEncoding.Base64.EncodeBytesToString(TEncoding.UTF8.GetBytes(AContent)));
  end;

  function RoleOf(AArr: TJSONArray; AIndex: Integer): string;
  begin
    Result := (AArr.Items[AIndex] as TJSONObject).GetValue<string>('role');
  end;

var
  Msgs: TAiChatMessages;
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
  Arr: TJSONArray;
  Raw, Content: string;
  Hits, P: Integer;
begin
  Msgs := TAiChatMessages.Create;
  try
    if AScenario = 'chat:toolresult-parallel' then
    begin
      Msgs.ModelCaps := [cap_Image];

      Msgs.Add(TAiChatMessage.Create('dibuja algo', 'user'));

      Msg := TAiChatMessage.Create('', 'assistant');
      Msg.Tool_calls := '[{"id":"c1","type":"function","function":{"name":"snap","arguments":"{}"}}]';
      Msgs.Add(Msg);

      Msg := TAiChatMessage.Create('snapshot 1', 'tool', 'c1', 'snap');
      Msg.AddMediaFile(NewImage('a.png'));
      Msgs.Add(Msg);

      Msg := TAiChatMessage.Create('snapshot 2', 'tool', 'c2', 'snap2');
      Msg.AddMediaFile(NewImage('b.png'));
      Msgs.Add(Msg);

      Arr := Msgs.ToJSon;
      try
        // count | rol[2] | rol[3] | rol[4] | numero de partes image_url
        Hits := 0;
        Raw := Arr.ToJSON;
        P := Pos('"image_url"', Raw);
        while P > 0 do
        begin
          Inc(Hits);
          P := Pos('"image_url"', Raw, P + 1);
        end;
        // cada parte aporta dos apariciones ("type":"image_url" y la clave)
        Result := Format('%d|%s|%s|%s|imgs=%d',
          [Arr.Count, RoleOf(Arr, 2), RoleOf(Arr, 3), RoleOf(Arr, 4), Hits div 2]);
      finally
        Arr.Free;
      end;
    end

    else if AScenario = 'chat:toolresult-novision' then
    begin
      Msgs.ModelCaps := []; // modelo sin vision

      Msgs.Add(TAiChatMessage.Create('hola', 'user'));

      Msg := TAiChatMessage.Create('resultado', 'tool', 'c1', 'snap');
      MF := NewImage('a.png');
      MF.Procesado := True;
      MF.Transcription := 'un cuadrado rojo';
      Msg.AddMediaFile(MF);
      Msgs.Add(Msg);

      Arr := Msgs.ToJSon;
      try
        Raw := Arr.ToJSON;
        Result := Format('%d|trans=%s|img=%s',
          [Arr.Count,
           IfThen(Pos('un cuadrado rojo', Raw) > 0, 'si', 'no'),
           IfThen(Pos('image_url', Raw) > 0, 'si', 'no')]);
      finally
        Arr.Free;
      end;
    end

    else if AScenario = 'chat:toolresult-dup' then
    begin
      Msgs.ModelCaps := [];

      // Prompt que YA trae la transcripcion inyectada por RunNew
      Msg := TAiChatMessage.Create('resultado' + sLineBreak + 'un cuadrado rojo',
        'tool', 'c1', 'snap');
      MF := NewImage('a.png');
      MF.Procesado := True;
      MF.Transcription := 'un cuadrado rojo';
      Msg.AddMediaFile(MF);
      Msgs.Add(Msg);

      Arr := Msgs.ToJSon;
      try
        Content := (Arr.Items[0] as TJSONObject).GetValue<string>('content');
        Hits := 0;
        P := Pos('un cuadrado rojo', Content);
        while P > 0 do
        begin
          Inc(Hits);
          P := Pos('un cuadrado rojo', Content, P + 1);
        end;
        Result := IntToStr(Hits);
      finally
        Arr.Free;
      end;
    end

    else if AScenario = 'chat:toolresult-text' then
    begin
      Msgs.ModelCaps := [cap_Image];

      Msg := TAiChatMessage.Create('lei el archivo', 'tool', 'c1', 'read_file');
      Msg.AddMediaFile(NewText('datos.txt', 'col_a;col_b' + sLineBreak + '1;2'));
      Msgs.Add(Msg);

      Arr := Msgs.ToJSon;
      try
        Raw := Arr.ToJSON;
        Result := Format('%d|inline=%s|string=%s',
          [Arr.Count,
           IfThen((Pos('col_a;col_b', Raw) > 0) and (Pos('datos.txt', Raw) > 0), 'si', 'no'),
           IfThen((Arr.Items[0] as TJSONObject).GetValue('content') is TJSONString, 'si', 'no')]);
      finally
        Arr.Free;
      end;
    end

    else
      raise Exception.Create('Escenario de chat desconocido: ' + AScenario);

  finally
    for Msg in Msgs do
      Msg.Free;
    Msgs.Free;
  end;
end;

// -----------------------------------------------------------------------------
// Montaje concurrente de conexiones
// -----------------------------------------------------------------------------
// Reproduce, SIN RED, lo que hace un servidor HTTP atendiendo varios requests a
// la vez: cada request crea su TAiChatConnection y fija DriverName, Model,
// Params y AiFunctions. Ese camino ESCRIBE en el registro global
// (SetModel -> TAiChatFactory.RegisterUserParam) y lo LEE
// (UpdateAndApplyParams -> GetDriverParams) desde todos los hilos a la vez.
//
// El fallo que se busca no es una excepcion sino un CUELGUE: si las estructuras
// del registro se corrompen, un hilo se queda girando y su iteracion no termina
// nunca. Por eso no se usa TTask.WaitForAll sin limite (colgaria la suite
// entera): se cuentan las iteraciones completadas contra un plazo maximo y lo
// que falte se reporta como 'hung'.
// -----------------------------------------------------------------------------
function TRegressionSuite.RunConnScenario(const AScenario: string): string;
var
  HILOS, VUELTAS, PLAZO_MS: Integer;
  Tasks: array of ITask;
  Hechas, Errores, Total, I, ModeloMal: Integer;
  Inicio: Cardinal;
begin
  if AScenario <> 'conn:concurrent-setup' then
    raise Exception.Create('Escenario conn desconocido: ' + AScenario);

  // Ajustables por entorno para poder apretar la carrera sin recompilar.
  // Los valores por defecto mantienen la suite rapida (decimas de segundo).
  HILOS    := StrToIntDef(GetEnvironmentVariable('MAKERAI_CONN_THREADS'), 8);
  VUELTAS  := StrToIntDef(GetEnvironmentVariable('MAKERAI_CONN_ITERS'), 60);
  PLAZO_MS := StrToIntDef(GetEnvironmentVariable('MAKERAI_CONN_DEADLINE_MS'), 30000);

  Hechas  := 0;
  Errores := 0;
  ModeloMal := 0;
  Total   := HILOS * VUELTAS;
  SetLength(Tasks, HILOS);

  for I := 0 to HILOS - 1 do
    Tasks[I] := TTask.Run(
      procedure
      var
        K, Sel: Integer;
        Conn: TAiChatConnection;
        Funcs: TAiFunctions;
        Driver, Modelo: string;
      begin
        for K := 1 to VUELTAS do
        begin
          try
            // Se rotan tres drivers reales para que el registro reciba claves
            // distintas ademas de repetidas (Add + rehash, no solo update).
            Sel := K mod 3;
            case Sel of
              0: begin Driver := 'Groq';   Modelo := 'qwen/qwen3.6-27b'; end;
              1: begin Driver := 'Claude'; Modelo := 'claude-haiku-4-5-20251001'; end;
            else  begin Driver := 'GLM';    Modelo := 'glm-4.7'; end;
            end;

            Conn := TAiChatConnection.Create(nil);
            try
              Conn.DriverName := Driver;   // -> UpdateAndApplyParams (lee)
              Conn.Model      := Modelo;   // -> RegisterUserParam (ESCRIBE) + lee

              Conn.Params.Values['ApiKey']          := 'sk-de-prueba';
              Conn.Params.Values['Temperature']     := '1';
              Conn.Params.Values['Max_tokens']      := '400';
              Conn.Params.Values['Tool_Active']     := 'True';
              Conn.Params.Values['ResponseTimeOut'] := '840000';

              Funcs := TAiFunctions.Create(nil);
              try
                Funcs.Functions.AddFunction('get_current_time', True, nil);
                Conn.AiFunctions := Funcs;  // el paso donde se colgaba en prod
                if Assigned(Conn.AiChat) then
                begin
                  Conn.AiChat.Tool_choice := '"auto"';
                  // El modelo TIENE que haber llegado al driver. Se comprueba
                  // aqui porque la via por la que llega cambio: antes daba la
                  // vuelta por el registro global y ahora se impone sobre los
                  // params locales de la conexion. Si se rompiera, el broker
                  // llamaria al modelo por defecto del driver en vez de al
                  // pedido — un fallo silencioso y caro.
                  if Conn.AiChat.Model <> Modelo then
                    TInterlocked.Increment(ModeloMal);
                end;
              finally
                Conn.AiFunctions := nil;
                Funcs.Free;
              end;
            finally
              Conn.Free;
            end;

            TInterlocked.Increment(Hechas);
          except
            on E: Exception do
            begin
              TInterlocked.Increment(Errores);
              // El primer fallo es el que importa: dice QUE se rompe. Los
              // siguientes suelen ser danos colaterales del mismo destrozo.
              TMonitor.Enter(Self);
              try
                if FConnFirstError = '' then
                  FConnFirstError := E.ClassName + ': ' + E.Message;
              finally
                TMonitor.Exit(Self);
              end;
            end;
          end;
        end;
      end);

  Inicio := TThread.GetTickCount;
  while (TInterlocked.CompareExchange(Hechas, 0, 0) +
         TInterlocked.CompareExchange(Errores, 0, 0) < Total) and
        (TThread.GetTickCount - Inicio < PLAZO_MS) do
    TThread.Sleep(20);

  Hechas  := TInterlocked.CompareExchange(Hechas, 0, 0);
  Errores := TInterlocked.CompareExchange(Errores, 0, 0);

  // Detalle a consola: el veredicto del runner es un si/no, y aqui lo que
  // interesa es CUANTO y POR QUE.
  ModeloMal := TInterlocked.CompareExchange(ModeloMal, 0, 0);
  Writeln(Format('      [conn] hilos=%d vueltas=%d -> ok=%d errores=%d colgadas=%d modelo_mal=%d',
    [HILOS, VUELTAS, Hechas, Errores, Total - Hechas - Errores, ModeloMal]));
  if FConnFirstError <> '' then
    Writeln('      [conn] primer fallo: ' + FConnFirstError);

  Result := Format('hung=%d|errors=%d|badmodel=%d',
    [Total - Hechas - Errores, Errores, ModeloMal]);
end;

// -----------------------------------------------------------------------------
// RAG
// -----------------------------------------------------------------------------

function TRegressionSuite.RunRagScenario(const AScenario: string): string;
var
  Drv: TAiMkVecDriver;
  Node, Target: TAiEmbeddingNode;
  Res: TAiRAGVector;
  Vec: TAiEmbeddingData;
  LPath: string;
  I: Integer;
begin
  if AScenario <> 'rag:search-nil-options' then
    raise Exception.Create('Escenario RAG desconocido: ' + AScenario);

  LPath := TPath.Combine(TPath.GetTempPath, 'makerai_regress_rag.mkai');
  if TFile.Exists(LPath) then
    TFile.Delete(LPath);

  SetLength(Vec, 4);
  for I := 0 to 3 do
    Vec[I] := 0.5;

  Drv := TAiMkVecDriver.Create(nil);
  try
    Drv.Dim := 4;
    Drv.FilePath := LPath;
    Drv.Open;

    Node := TAiEmbeddingNode.Create(4);
    try
      Node.Tag := 'n1';
      Node.Text := 'delphi orquesta agentes';
      Node.Data := Vec;
      Drv.Add(Node, 'DEFAULT');
    finally
      Node.Free;
    end;

    Target := TAiEmbeddingNode.Create(4);
    try
      Target.Text := 'delphi';
      Target.Data := Vec;

      // Lo que se prueba: Options en nil y driver sin Owner, asi que dentro de
      // Search LOptions se queda en nil. Antes esto era un AV, no un 0 nodos.
      Res := Drv.Search(Target, 'DEFAULT', 5, 0, nil, nil);
      try
        Result := IntToStr(Res.Items.Count);
      finally
        Res.Free;
      end;
    finally
      Target.Free;
    end;
  finally
    Drv.Free;
    if TFile.Exists(LPath) then
      TFile.Delete(LPath);
  end;
end;

function TRegressionSuite.RunPolicyScenario(const AScenario: string): string;
var
  G: TAiGuardrails;
  Handlers: TFixtureHandlers;
  Reason: string;
  Funcs: TAiFunctions;
  Item: TFunctionActionItem;
  ToolCall: TAiToolsFunction;
  InnerRunner: TAiEvalRunner;
  InnerReport: TAiEvalReport;

  function Verdict(AAllowed: Boolean): string;
  begin
    if AAllowed then
      Result := 'allowed'
    else
      Result := 'blocked';
  end;

begin
  Result := '';
  G := TAiGuardrails.Create(nil);
  Handlers := TFixtureHandlers.Create;
  try
    G.OnBlocked := Handlers.GuardBlocked;

    if AScenario = 'policy:blocklist' then
    begin
      G.BlockedTools.Add('shell_*');
      Result := Verdict(G.CheckToolCall('shell_exec', '{}', Reason));
    end

    else if AScenario = 'policy:allowlist' then
    begin
      G.AllowedTools.Add('safe_*');
      Result := Verdict(G.CheckToolCall('safe_read', '{}', Reason)) + '|' +
        Verdict(G.CheckToolCall('otro', '{}', Reason));
    end

    else if AScenario = 'policy:argpattern' then
    begin
      G.BlockedArgPatterns.Add('DROP TABLE');
      Result := Verdict(G.CheckToolCall('sql', '{"q":"drop table users"}', Reason));
    end

    else if AScenario = 'policy:veto' then
    begin
      G.OnCheckToolCall := Handlers.GuardCheck;
      Result := Verdict(G.CheckToolCall('deploy', '{"env":"produccion"}', Reason));
    end

    else if AScenario = 'policy:integration' then
    begin
      // El tool bloqueado NO debe ejecutarse y el LLM debe recibir el motivo
      Funcs := TAiFunctions.Create(nil);
      ToolCall := TAiToolsFunction.Create;
      try
        Handlers.ToolExecuted := False;
        Item := Funcs.Functions.Add;
        Item.FunctionName := 'peligroso';
        Item.Enabled := True;
        Item.OnAction := Handlers.ToolAction;
        G.BlockedTools.Add('peligroso');
        Funcs.Guardrails := G;

        ToolCall.Name := 'peligroso';
        ToolCall.Arguments := '{}';
        Funcs.DoCallFunction(ToolCall);

        Result := ToolCall.Response;
        if Handlers.ToolExecuted then
          Result := Result + ' |executed'
        else
          Result := Result + ' |not-executed';
      finally
        ToolCall.Free;
        Funcs.Free;
      end;
    end

    else if AScenario = 'policy:evals-self' then
    begin
      // Autoprueba: el runner debe contar bien PASS y FAIL
      InnerRunner := TAiEvalRunner.Create(nil);
      try
        InnerRunner.AddCase('ok-contains').Input('hola').ExpectContains('HOLA');
        InnerRunner.AddCase('ok-regex').Input('abc').ExpectRegex('^[A-Z]+$');
        InnerRunner.AddCase('debe-fallar').Input('x').ExpectContains('inexistente');
        InnerReport := InnerRunner.Run(
          function(const AInput: string): string
          begin
            Result := AInput.ToUpper;
          end);
        try
          Result := Format('%d|%d', [InnerReport.Passed, InnerReport.Failed]);
        finally
          InnerReport.Free;
        end;
      finally
        InnerRunner.Free;
      end;
    end

    else
      raise Exception.Create('Escenario de politica desconocido: ' + AScenario);
  finally
    Handlers.Free;
    G.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TRegressionSuite.Run: TAiEvalReport;
begin
  Result := FRunner.Run(
    function(const AInput: string): string
    begin
      Result := Dispatch(AInput);
    end);
end;

// -----------------------------------------------------------------------------
// Jev (TypeSafe AI)
// -----------------------------------------------------------------------------

function TRegressionSuite.RunJevScenario(const AScenario: string): string;
const
  // Respuesta tomada de los ejemplos de la documentacion de la API
  OK_JSON =
    '{"model":"jev-1.13.0","answers":{' +
    '"dominio":{"type":"choice","choice":"billing",' +
      '"probabilities":{"billing":0.88,"technical":0.12,"sales":0.0},"confidence":0.81},' +
    '"dificultad":{"type":"score","score":1.05,"legend":{"0":"a","1":"b","2":"c"},' +
      '"probabilities":{"0":0.0,"1":0.95,"2":0.05},"confidence":0.92},' +
    '"fuentes":{"type":"noul","noul":0.95}},' +
    '"usage":{"input_tokens":318,"output_tokens":34}}';
var
  J: TFakeJev;
  Q: TAiJevQuestions;
  R: TAiJevResult;
  State: TJSONObject;
  Body, Qs, Crit, Fuentes: TJSONObject;
  Parsed: TJSONValue;

  function F2(AValue: Double): string;
  begin
    Result := FormatFloat('0.00', AValue, TFormatSettings.Invariant);
  end;

  function SiNo(AValue: Boolean): string;
  begin
    if AValue then
      Result := 'si'
    else
      Result := 'no';
  end;

  // Grafo Router -> contable | tributario (NextNo -> humano) -> Fin, con el
  // router usando J (TFakeJev) ya cargado con la respuesta del escenario.
  function RunRouterGraph: string;
  var
    Agents: TAIAgentManager;
    Handlers: TFixtureHandlers;
    Router: TAiJevRouterTool;
    Targets: TDictionary<string, string>;
    BB: TAIBlackboard;
  begin
    Agents := TAIAgentManager.Create(nil);
    Handlers := TFixtureHandlers.Create;
    try
      Agents.Name := 'SuiteJevRouter';
      Agents.AddNode('Router', nil)
        .AddNode('contable', Handlers.NodeExec)
        .AddNode('tributario', Handlers.NodeExec)
        .AddNode('humano', Handlers.NodeExec)
        .AddNode('Fin', Handlers.NodeExec);

      Router := TAiJevRouterTool.Create(Agents);
      Router.Jev := J;
      Router.AddRoute('contable', 'Asientos, PUC, estados financieros');
      Router.AddRoute('tributario', 'Impuestos, declaraciones, DIAN');
      Router.AddRoute('general', '');
      Router.AddFlag('fuentes', 'Exige citar una norma concreta?');
      Agents.FindNode('Router').Tool := Router;

      Targets := TDictionary<string, string>.Create;
      try
        Targets.Add('contable', 'contable');
        Targets.Add('tributario', 'tributario');
        Agents.AddConditionalEdge('Router', 'RouterLink', Targets);
      finally
        Targets.Free;
      end;
      Agents.FindNode('Router').Next.NextNo := Agents.FindNode('humano');
      Agents.AddEdge('contable', 'Fin').AddEdge('tributario', 'Fin').AddEdge('humano', 'Fin');
      Agents.SetEntryPoint('Router').SetFinishPoint('Fin');

      Agents.Run('hola');
      while Agents.Busy do
      begin
        CheckSynchronize;
        Sleep(20);
      end;
      CheckSynchronize;

      BB := Agents.Blackboard;
      Result := GetEnumName(TypeInfo(TAgentExecutionStatus), Ord(BB.GetStatus)) +
        '|' + Agents.EndNode.Output +
        '|choice=' + BB.GetString('Router.jev.choice') +
        '|conf=' + BB.GetString('Router.jev.confidence') +
        '|fuentes=' + BB.GetString('Router.jev.fuentes') +
        '|error=' + SiNo(BB.GetString('Router.jev.error') <> '');
    finally
      Agents.Free;
      Handlers.Free;
    end;
  end;

  // Ejecuta Ask esperando un error de validacion; devuelve 'error' u 'ok'
  function AskFails(AQ: TAiJevQuestions): string;
  begin
    try
      J.Ask('x', AQ).Free;
      Result := 'ok';
    except
      on E: EAiJevError do
        Result := 'error';
    end;
  end;

begin
  Result := '';
  J := TFakeJev.Create(nil);
  Q := TAiJevQuestions.Create(nil);
  try
    Q.AddChoice('dominio', 'Que especialista debe responder `consulta`?',
      ['contable=Asientos y PUC', 'tributario=Impuestos', 'general']);
    Q.AddScore('dificultad', 'Que tan dificil es responder `consulta`?',
      ['Trivial', 'Estandar', 'Experta']);
    Q.AddNoul('fuentes', 'Exige citar una norma concreta?', 'Depende de una norma');

    if AScenario = 'jev:request' then
    begin
      J.Enqueue(200, OK_JSON);
      State := TJSONObject.Create;
      try
        State.AddPair('consulta', 'hola');
        J.Ask(State, Q).Free;
      finally
        State.Free;
      end;
      Parsed := TJSONObject.ParseJSONValue(J.LastBody);
      try
        Body := Parsed as TJSONObject;
        Qs := Body.GetValue<TJSONObject>('questions');
        Crit := Qs.GetValue<TJSONObject>('dominio').GetValue<TJSONObject>('criteria');
        Fuentes := Qs.GetValue<TJSONObject>('fuentes').GetValue<TJSONObject>('criteria');
        Result := 'model=' + Body.GetValue<string>('model') +
          '|consulta=' + Body.GetValue<TJSONObject>('state').GetValue<string>('consulta') +
          '|dominio=' + Qs.GetValue<TJSONObject>('dominio').GetValue<string>('type') +
          '|contable=' + Crit.GetValue<string>('contable') +
          '|general=' + IfThen(Crit.GetValue('general') is TJSONNull, 'null', 'otro') +
          '|levels=' + Qs.GetValue<TJSONObject>('dificultad').GetValue<TJSONArray>('criteria').Count.ToString +
          '|noul.true=' + SiNo(Fuentes.GetValue('true') <> nil) +
          '|noul.false=' + SiNo(Fuentes.GetValue('false') <> nil);
      finally
        Parsed.Free;
      end;
    end

    else if AScenario = 'jev:parse' then
    begin
      J.Enqueue(200, OK_JSON);
      R := J.Ask('hola', Q);
      try
        Result := 'model=' + R.Model +
          '|choice=' + R['dominio'].Choice +
          '|conf=' + F2(R['dominio'].Confidence) +
          '|top2=' + string.Join(',', R['dominio'].Top(2)) +
          '|sales=' + F2(R['dominio'].Probability('sales')) +
          '|score=' + F2(R['dificultad'].Score) +
          '|nivel1=' + F2(R['dificultad'].Probability('1')) +
          '|noul=' + F2(R['fuentes'].Noul) +
          '|in=' + R.InputTokens.ToString +
          '|out=' + R.OutputTokens.ToString;
      finally
        R.Free;
      end;
    end

    else if AScenario = 'jev:retry' then
    begin
      J.Enqueue(429, '{"detail":"rate limit"}');
      J.Enqueue(529, '{"detail":"overloaded"}');
      J.Enqueue(200, OK_JSON);
      R := J.Ask('hola', Q);
      try
        Result := 'calls=' + J.Calls.ToString + '|choice=' + R['dominio'].Choice;
      finally
        R.Free;
      end;
    end

    else if AScenario = 'jev:retry-exhausted' then
    begin
      J.MaxRetries := 1;
      J.Enqueue(429, '{"detail":"rate limit"}');
      J.Enqueue(429, '{"detail":"rate limit"}');
      J.Enqueue(200, OK_JSON);
      try
        J.Ask('hola', Q).Free;
        Result := 'sin-error';
      except
        on E: EAiJevError do
          Result := 'calls=' + J.Calls.ToString + '|status=' + E.StatusCode.ToString +
            '|lasterror=' + SiNo(J.LastError <> '');
      end;
    end

    else if AScenario = 'jev:validate' then
    begin
      // Ninguno de estos debe llegar a la red
      Q.Clear;
      Q.AddChoice('a', 'pregunta', ['unica']);
      Result := 'una-opcion=' + AskFails(Q);

      Q.Clear;
      Q.AddNoul('a', 'pregunta');
      Q.AddNoul('a', 'otra');
      Result := Result + '|nombre-repetido=' + AskFails(Q);

      Q.Clear;
      Q.AddScore('a', 'pregunta', ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11']);
      Result := Result + '|score-11=' + AskFails(Q);

      Q.Clear;
      Result := Result + '|sin-preguntas=' + AskFails(Q);
      Result := Result + '|calls=' + J.Calls.ToString;
    end

    else if AScenario = 'jev:http-error' then
    begin
      J.Enqueue(401, '{"detail":"invalid key"}');
      J.Enqueue(200, OK_JSON);
      try
        J.Ask('hola', Q).Free;
        Result := 'sin-error';
      except
        on E: EAiJevError do
          // 401 no es transitorio: una sola llamada
          Result := 'status=' + E.StatusCode.ToString + '|lasterror=' +
            SiNo(J.LastError.Contains('401')) + '|reintentos=' + SiNo(J.Calls > 1);
      end;
    end

    else if AScenario = 'jev:router-route' then
    begin
      J.Enqueue(200, '{"model":"jev-1.13.0","answers":{' +
        '"route":{"type":"choice","choice":"contable","confidence":0.85,' +
          '"probabilities":{"contable":0.9,"tributario":0.08,"general":0.02}},' +
        '"fuentes":{"type":"noul","noul":0.8}},"usage":{"input_tokens":400,"output_tokens":30}}');
      Result := RunRouterGraph;
    end

    else if AScenario = 'jev:router-lowconf' then
    begin
      J.Enqueue(200, '{"model":"jev-1.13.0","answers":{' +
        '"route":{"type":"choice","choice":"tributario","confidence":0.3,' +
          '"probabilities":{"contable":0.3,"tributario":0.45,"general":0.25}},' +
        '"fuentes":{"type":"noul","noul":0.1}},"usage":{"input_tokens":400,"output_tokens":30}}');
      Result := RunRouterGraph;
    end

    else if AScenario = 'jev:router-error' then
    begin
      J.Enqueue(401, '{"detail":"invalid key"}');
      Result := RunRouterGraph;
    end

    else if AScenario = 'jev:dispatch-chat' then
    begin
      var Chat := TAiOpenChat.Create(nil);
      var Cls := TFakeDispatchClassifier.Create(nil);
      var Img := TFakeImageTool.Create(nil);
      try
        Chat.ApiKey := 'sin-red';
        Chat.Url := 'http://127.0.0.1:1/';
        Chat.Asynchronous := False;
        Chat.ChatMode := cmSmartDispatch;
        Chat.ChatTools.ImageTool := Img;
        Chat.ChatTools.DispatchClassifier := Cls;
        Cls.Answer := 'IMAGEGEN';
        Chat.AddMessageAndRun('dibuja un gato rojo', 'user', []);
        Result := 'tags=' + Cls.LastTags + '|clasificador=' + Cls.Calls.ToString +
          '|imagen=' + Img.Calls.ToString + '|prompt=' + Img.LastPrompt;
      finally
        Chat.Free;
        Cls.Free;
        Img.Free;
      end;
    end

    else if AScenario = 'jev:dispatch' then
    begin
      var D := TAiJevDispatchClassifier.Create(nil);
      try
        D.Jev := J;
        var Intf: IAiDispatchClassifier;
        Supports(D, IAiDispatchClassifier, Intf);
        // Confianza alta: decide
        J.Enqueue(200, '{"model":"jev-1.13.0","answers":{"tag":{"type":"choice","choice":"IMAGEGEN",' +
          '"confidence":0.95,"probabilities":{"IMAGEGEN":0.97,"WEBSEARCH":0.02,"CHAT":0.01}}}}');
        Result := 'alta=' + Intf.ClassifyDispatch('dibuja un gato', ['IMAGEGEN', 'WEBSEARCH', 'CHAT']);
        Parsed := TJSONObject.ParseJSONValue(J.LastBody);
        try
          var Opciones := '';
          for var Pair in (Parsed as TJSONObject).GetValue<TJSONObject>('questions')
            .GetValue<TJSONObject>('tag').GetValue<TJSONObject>('criteria') do
            Opciones := Opciones + IfThen(Opciones <> '', ',', '') + Pair.JsonString.Value;
          Result := Result + '|opciones=' + Opciones;
        finally
          Parsed.Free;
        end;
        // Confianza baja: no decide ('') pero recuerda la eleccion
        J.Enqueue(200, '{"model":"jev-1.13.0","answers":{"tag":{"type":"choice","choice":"IMAGEGEN",' +
          '"confidence":0.4,"probabilities":{"IMAGEGEN":0.6,"CHAT":0.4}}}}');
        Result := Result + '|baja=' + Intf.ClassifyDispatch('algo', ['IMAGEGEN', 'CHAT']) +
          '|ultima=' + D.LastTag;
        // Solo CHAT: responde sin consultar a Jev
        Result := Result + '|solo-chat=' + Intf.ClassifyDispatch('hola', ['CHAT']) +
          '|calls=' + J.Calls.ToString;
        Intf := nil; // soltar la interfaz antes de liberar el componente
      finally
        D.Free;
      end;
    end

    else if AScenario = 'jev:guard' then
    begin
      var G := TAiGuardrails.Create(nil);
      var C := TAiJevGuardrailClassifier.Create(nil);
      try
        C.Jev := J;
        G.Classifier := C;
        var Reason: string;
        const NOUL_ALTO = '{"model":"jev-1.13.0","answers":{"risk":{"type":"noul","noul":0.95}}}';
        const NOUL_BAJO = '{"model":"jev-1.13.0","answers":{"risk":{"type":"noul","noul":0.10}}}';

        J.Enqueue(200, NOUL_ALTO);
        Result := 'riesgo=' + IfThen(G.CheckToolCall('send_email', '{"body":"la clave es X"}', Reason),
          'allowed', 'blocked') + '|motivo=' + Reason;

        J.Enqueue(200, NOUL_BAJO);
        Result := Result + '|seguro=' + IfThen(G.CheckToolCall('get_weather', '{"city":"Cali"}', Reason),
          'allowed', 'blocked');

        // Lo que ya bloquean las listas no llega a Jev
        var CallsAntes := J.Calls;
        G.BlockedTools.Add('shell_*');
        Result := Result + '|lista=' + IfThen(G.CheckToolCall('shell_exec', '{}', Reason), 'allowed', 'blocked') +
          '|jev-consultado=' + SiNo(J.Calls > CallsAntes);
        G.BlockedTools.Clear;

        // Jev caido: BlockOnError decide
        J.Enqueue(401, '{"detail":"invalid key"}');
        Result := Result + '|error-cerrado=' + IfThen(G.CheckToolCall('read_file', '{}', Reason),
          'allowed', 'blocked');
        C.BlockOnError := False;
        J.Enqueue(401, '{"detail":"invalid key"}');
        Result := Result + '|error-abierto=' + IfThen(G.CheckToolCall('read_file', '{}', Reason),
          'allowed', 'blocked');
      finally
        G.Free;
        C.Free;
      end;
    end

    else
      raise Exception.Create('Escenario jev desconocido: ' + AScenario);
  finally
    Q.Free;
    J.Free;
  end;
end;

end.
