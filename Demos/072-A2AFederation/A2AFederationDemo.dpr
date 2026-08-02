program A2AFederationDemo;

// =============================================================================
// DEMO 072 - Federacion de agentes con el protocolo A2A (spec 1.0)
// =============================================================================
// Muestra los tres roles del stack A2A de MakerAI, sin necesidad de LLM ni
// API keys (los nodos son transformaciones de texto deterministas):
//
//   1. SERVIDOR : un grafo TAIAgentManager ("Agente Editor") expuesto como
//                 agente A2A via TAiA2AServer (Agent Card + JSON-RPC).
//   2. CLIENTE  : TAiA2AClient descubre la Agent Card y envia un mensaje.
//   3. FEDERACION: un grafo local ("Agente Redactor") delega uno de sus nodos
//                 en el agente remoto usando TAiA2ARemoteAgentTool.
//
// Modos de uso:
//   A2AFederationDemo.exe                        -> demo completo en un proceso
//   A2AFederationDemo.exe --serve [--port 8280]  -> solo el servidor (Ctrl+C)
//   A2AFederationDemo.exe --client --url http://host:8280 [texto...]
//   A2AFederationDemo.exe --otel                 -> exporta trazas OTLP a :4318
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.TypInfo,
  uMakerAi.Agents,
  uMakerAi.A2A.Server in '..\..\Source\Agents\uMakerAi.A2A.Server.pas',
  uMakerAi.A2A.Client in '..\..\Source\Agents\uMakerAi.A2A.Client.pas',
  uMakerAi.Telemetry;

type
  // ---------------------------------------------------------------------------
  // Nodos del agente REMOTO ("Agente Editor"): limpia y firma un texto
  // ---------------------------------------------------------------------------
  TEditorHandlers = class
  public
    procedure LimpiarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure FirmarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
  end;

  // ---------------------------------------------------------------------------
  // Nodos del agente LOCAL ("Agente Redactor"): prepara, delega y publica
  // ---------------------------------------------------------------------------
  TRedactorHandlers = class
  public
    procedure PrepararExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure PublicarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
  end;

procedure TEditorHandlers.LimpiarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  // Normaliza espacios multiples y recorta extremos
  Output := Input.Trim;
  while Output.Contains('  ') do
    Output := Output.Replace('  ', ' ');
end;

procedure TEditorHandlers.FirmarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Input + ' [revisado por el Agente Editor]';
end;

procedure TRedactorHandlers.PrepararExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := 'BORRADOR: ' + Input;
end;

procedure TRedactorHandlers.PublicarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := '>>> PUBLICADO: ' + Input;
end;

// -----------------------------------------------------------------------------

procedure WaitForGraph(AAgents: TAIAgentManager);
begin
  while AAgents.Busy do
  begin
    CheckSynchronize;
    Sleep(50);
  end;
  CheckSynchronize;
end;

function BuildEditorAgent(AHandlers: TEditorHandlers): TAIAgentManager;
begin
  Result := TAIAgentManager.Create(nil);
  Result.Name := 'AgenteEditor';
  Result.AddNode('Limpiar', AHandlers.LimpiarExec).AddNode('Firmar', AHandlers.FirmarExec);
  Result.AddEdge('Limpiar', 'Firmar');
  Result.SetEntryPoint('Limpiar').SetFinishPoint('Firmar');
end;

procedure RunServeMode(APort: Integer);
var
  Editor: TAIAgentManager;
  Handlers: TEditorHandlers;
  Server: TAiA2AServer;
begin
  Handlers := TEditorHandlers.Create;
  Editor := BuildEditorAgent(Handlers);
  Server := TAiA2AServer.Create(nil);
  try
    Server.AgentManager := Editor;
    Server.AgentName := 'Agente Editor';
    Server.AgentDescription := 'Limpia y firma textos (demo A2A de MakerAI)';
    Server.Port := APort;
    Server.Active := True;
    Writeln(Format('Agente A2A escuchando en http://localhost:%d', [APort]));
    Writeln('  Agent Card: http://localhost:' + IntToStr(APort) + '/.well-known/agent-card.json');
    Writeln('Ctrl+C para detener.');
    while True do
    begin
      CheckSynchronize;
      Sleep(200);
    end;
  finally
    Server.Free;
    Editor.Free;
    Handlers.Free;
  end;
end;

procedure RunClientMode(const AUrl, AText: string);
var
  Client: TAiA2AClient;
  Card, Task: TJSONObject;
  OutText: string;
begin
  Client := TAiA2AClient.Create(nil);
  try
    Client.Url := AUrl;
    Writeln('1) Descargando Agent Card de ' + AUrl + ' ...');
    Card := Client.FetchAgentCard;
    try
      Writeln('   Agente: ' + Card.GetValue<string>('name', '?') + ' — ' +
        Card.GetValue<string>('description', ''));
    finally
      Card.Free;
    end;

    Writeln('2) Enviando mensaje: "' + AText + '"');
    Task := Client.SendText(AText, OutText);
    try
      Writeln('   Task ' + Client.LastTaskId + ' -> ' + Client.LastState);
      Writeln('   Respuesta: ' + OutText);
    finally
      Task.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure RunFullDemo(APort: Integer; const AText: string);
var
  EditorHandlers: TEditorHandlers;
  RedactorHandlers: TRedactorHandlers;
  Editor, Redactor: TAIAgentManager;
  Server: TAiA2AServer;
  Tool: TAiA2ARemoteAgentTool;
begin
  Writeln('=== DEMO A2A: federacion de agentes en un solo proceso ===');
  Writeln;

  EditorHandlers := TEditorHandlers.Create;
  RedactorHandlers := TRedactorHandlers.Create;
  Editor := BuildEditorAgent(EditorHandlers);
  Redactor := TAIAgentManager.Create(nil);
  Server := TAiA2AServer.Create(nil);
  try
    // 1) Exponer el Agente Editor via A2A
    Server.AgentManager := Editor;
    Server.AgentName := 'Agente Editor';
    Server.AgentDescription := 'Limpia y firma textos (demo A2A de MakerAI)';
    Server.Port := APort;
    Server.Active := True;
    Writeln(Format('[servidor] Agente Editor publicado en http://localhost:%d', [APort]));

    // 2) Probar el cliente puro contra el servidor
    Writeln;
    Writeln('[cliente] Consumiendo el agente remoto directamente:');
    RunClientMode('http://localhost:' + IntToStr(APort), AText);

    // 3) Federacion: el Agente Redactor delega su nodo 'Editar' en el remoto
    Writeln;
    Writeln('[federacion] Grafo local: Preparar -> Editar(A2A remoto) -> Publicar');
    Redactor.Name := 'AgenteRedactor';
    Redactor.AddNode('Preparar', RedactorHandlers.PrepararExec);
    Redactor.AddNode('Editar', nil);
    Redactor.AddNode('Publicar', RedactorHandlers.PublicarExec);
    Tool := TAiA2ARemoteAgentTool.Create(Redactor);
    Tool.AgentUrl := 'http://localhost:' + IntToStr(APort);
    Redactor.FindNode('Editar').Tool := Tool;
    Redactor.AddEdge('Preparar', 'Editar');
    Redactor.AddEdge('Editar', 'Publicar');
    Redactor.SetEntryPoint('Preparar').SetFinishPoint('Publicar');

    Redactor.Run(AText);
    WaitForGraph(Redactor);

    Writeln('  Status final: ' + GetEnumName(TypeInfo(TAgentExecutionStatus),
      Ord(Redactor.Blackboard.GetStatus)));
    Writeln('  Resultado   : ' + Redactor.EndNode.Output);
    Writeln;
    if Redactor.Blackboard.GetStatus = esCompleted then
      Writeln('OK: la federacion A2A funciono de extremo a extremo.')
    else
      Writeln('ERROR: el grafo local no completo.');

    Server.Active := False;
  finally
    Server.Free;
    Redactor.Free;
    Editor.Free;
    RedactorHandlers.Free;
    EditorHandlers.Free;
  end;
end;

// -----------------------------------------------------------------------------

var
  i: Integer;
  Param, Url, InputText: string;
  Port: Integer;
  ServeMode, ClientMode, OtelMode: Boolean;
  Telemetry: TAiTelemetry;

begin
  try
    Port := 8280;
    Url := '';
    InputText := '';
    ServeMode := False;
    ClientMode := False;
    OtelMode := False;
    Telemetry := nil;

    i := 1;
    while i <= ParamCount do
    begin
      Param := LowerCase(ParamStr(i));
      if Param = '--serve' then
        ServeMode := True
      else if Param = '--client' then
        ClientMode := True
      else if Param = '--otel' then
        OtelMode := True
      else if (Param = '--port') and (i < ParamCount) then
      begin
        Inc(i);
        Port := StrToIntDef(ParamStr(i), Port);
      end
      else if (Param = '--url') and (i < ParamCount) then
      begin
        Inc(i);
        Url := ParamStr(i);
      end
      else
        InputText := Trim(InputText + ' ' + ParamStr(i));
      Inc(i);
    end;

    if InputText = '' then
      InputText := 'hola   mundo  federado';

    if OtelMode then
    begin
      Telemetry := TAiTelemetry.Create(nil);
      Telemetry.ServiceName := 'A2AFederationDemo';
      Telemetry.Enabled := True;
      Writeln('[otel] Exportando trazas a ' + Telemetry.Endpoint);
    end;

    try
      if ServeMode then
        RunServeMode(Port)
      else if ClientMode then
      begin
        if Url = '' then
          Url := 'http://localhost:' + IntToStr(Port);
        RunClientMode(Url, InputText);
      end
      else
        RunFullDemo(Port, InputText);
    finally
      Telemetry.Free; // flush de spans pendientes en el destructor
    end;
  except
    on E: Exception do
    begin
      Writeln('ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
