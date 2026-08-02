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
    procedure DefineCases;
    function Dispatch(const AScenario: string): string;
    // Escenarios agrupados por area
    function RunMcpScenario(const AScenario: string): string;
    function RunAgentScenario(const AScenario: string): string;
    function RunPolicyScenario(const AScenario: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Run: TAiEvalReport; // el llamador libera el reporte
  end;

implementation

uses
  System.TypInfo,
  uMakerAi.Core,
  uMakerAi.MCPServer.Core, UMakerAi.MCPServer.Http,
  uMakerAi.MCPClient.Core,
  uMakerAi.Agents,
  uMakerAi.A2A.Server, uMakerAi.A2A.Client,
  uMakerAi.Tools.Functions, uMakerAi.Chat.Messages,
  uMakerAi.Guardrails,
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

  // --- Evals (autoprueba del propio runner) ---
  FRunner.AddCase('evals.self-check')
    .Input('policy:evals-self')
    .ExpectEquals('2|1');
end;

function TRegressionSuite.Dispatch(const AScenario: string): string;
begin
  if AScenario.StartsWith('mcp:') then
    Result := RunMcpScenario(AScenario)
  else if AScenario.StartsWith('agents:') or AScenario.StartsWith('a2a:') then
    Result := RunAgentScenario(AScenario)
  else if AScenario.StartsWith('policy:') then
    Result := RunPolicyScenario(AScenario)
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
// Guardrails y evals
// -----------------------------------------------------------------------------

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

end.
