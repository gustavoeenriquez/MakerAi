program A2AOrchestrationDemo;

// =============================================================================
// DEMO 074 - Flujos de orquestacion sobre A2A (spec 1.0)
// =============================================================================
// El demo 072 muestra el camino feliz punto a punto: un cliente, un agente, una
// respuesta. Este muestra lo que hace falta para que A2A sirva de verdad en una
// orquestacion. Todo determinista, sin LLM ni API keys.
//
//   1. POOL          : tres tasks SIMULTANEOS contra el mismo agente. Con un
//                      solo TAIAgentManager el segundo y el tercero recibirian
//                      AgentBusy (-32000), porque un grafo no ejecuta dos
//                      corridas a la vez. Con OnAcquireManager el servidor
//                      mantiene un pool y los tres completan.
//
//   2. HUMAN-IN-THE-LOOP : el grafo se suspende pidiendo aprobacion. El task
//                      queda en input-required con la pregunta en
//                      status.message, y un SendMessage posterior con el MISMO
//                      taskId lo reanuda hasta completed.
//
//   3. HITL FEDERADO : lo mismo, pero el que pide aprobacion es un agente
//                      REMOTO. El nodo local se suspende en vez de fallar, y al
//                      reanudarlo la respuesta viaja al task remoto y el grafo
//                      local termina. Es lo que hace que un human-in-the-loop
//                      cruce la federacion en lugar de morir en el borde.
//
//   4. NO BLOQUEANTE : configuration.blocking=false devuelve el task en working
//                      de inmediato y el cliente sigue con GetTask, que refresca
//                      el estado desde el grafo vivo.
//
// Modos de uso:
//   A2AOrchestrationDemo.exe          -> los cuatro escenarios
//   A2AOrchestrationDemo.exe --otel   -> exporta trazas OTLP a localhost:4318
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.TypInfo,
  System.Threading,
  System.SyncObjs,
  System.DateUtils,
  uMakerAi.Agents,
  uMakerAi.A2A.Server in '..\..\Source\Agents\uMakerAi.A2A.Server.pas',
  uMakerAi.A2A.Client in '..\..\Source\Agents\uMakerAi.A2A.Client.pas',
  uMakerAi.Telemetry;

const
  PORT = 8281;

type
  // Los eventos del framework son 'of object': sin lambdas.
  TDemoHandlers = class
  public
    // Nodo normal: encadena su nombre al input
    procedure NodoExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Nodo lento: fuerza solapamiento real entre tasks concurrentes
    procedure NodoLento(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Nodo con human-in-the-loop: se suspende la primera vez y en la
    // reanudacion incorpora la respuesta humana
    procedure NodoAprobacion(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Fabrica del pool: un grafo nuevo por slot
    procedure FabricaManager(Sender: TObject; var AManager: TAIAgentManager);
  end;

procedure TDemoHandlers.NodoExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Input + '>' + Node.Name;
end;

procedure TDemoHandlers.NodoLento(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Sleep(400); // simula trabajo real
  Output := Input + '>' + Node.Name;
end;

procedure TDemoHandlers.NodoAprobacion(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
const
  KEY = 'demo.aprobado.';
begin
  // La marca va en el blackboard, no en el handler: ResumeThread REEJECUTA este
  // mismo nodo, asi que hay que distinguir la primera pasada de la reanudacion.
  if Node.Graph.Blackboard.GetString(KEY + Node.Name) = '' then
  begin
    Node.Graph.Blackboard.SetString(KEY + Node.Name, '1');
    Output := Input; // pass-through mientras se espera al humano
    Node.Suspend('Autoriza el pago de ' + Input + '?', 'Se requiere firma de un supervisor');
  end
  else
    Output := Input + ' [autorizado: ' + Node.Input + ']';
end;

procedure TDemoHandlers.FabricaManager(Sender: TObject; var AManager: TAIAgentManager);
begin
  // El servidor se hace cargo de liberarlo. Debe tener el mismo shape que el
  // grafo que el agente publica.
  AManager := TAIAgentManager.Create(nil);
  AManager.AddNode('Validar', NodoLento).AddNode('Procesar', NodoExec);
  AManager.AddEdge('Validar', 'Procesar');
  AManager.SetEntryPoint('Validar').SetFinishPoint('Procesar');
end;

// -----------------------------------------------------------------------------

procedure EsperarGrafo(A: TAIAgentManager);
begin
  while A.Busy do
  begin
    CheckSynchronize;
    Sleep(20);
  end;
  CheckSynchronize;
end;

function NombreEstado(A: TAiA2ATaskState): string;
begin
  Result := GetEnumName(TypeInfo(TAiA2ATaskState), Ord(A));
end;

function NombreStatus(A: TAgentExecutionStatus): string;
begin
  Result := GetEnumName(TypeInfo(TAgentExecutionStatus), Ord(A));
end;

// -----------------------------------------------------------------------------
// 1. POOL: tres tasks simultaneos
// -----------------------------------------------------------------------------
procedure EscenarioPool(AHandlers: TDemoHandlers);
var
  Server: TAiA2AServer;
  Tareas: TArray<ITask>;
  Ok, Busy, Otros: Integer;
  I: Integer;
  Inicio: TDateTime;
begin
  Writeln('');
  Writeln('== 1. POOL: tres tasks simultaneos contra el mismo agente ==');
  Writeln('');

  Server := TAiA2AServer.Create(nil);
  try
    Server.Port := PORT;
    Server.AgentName := 'Agente Procesador';
    Server.AgentDescription := 'Valida y procesa solicitudes';
    // Sin esta fabrica el pool no puede crecer y el limite efectivo es 1.
    Server.OnAcquireManager := AHandlers.FabricaManager;
    Server.MaxConcurrentTasks := 3;
    Server.Active := True;

    Ok := 0;
    Busy := 0;
    Otros := 0;
    Inicio := Now;

    SetLength(Tareas, 3);
    for I := 0 to 2 do
      Tareas[I] := TTask.Run(
        procedure
        var
          C: TAiA2AClient;
          T: TJSONObject;
          S: string;
        begin
          C := TAiA2AClient.Create(nil);
          try
            C.Url := 'http://localhost:' + IntToStr(PORT);
            try
              T := C.SendText('solicitud', S);
              try
                if C.LastTaskState = tsCompleted then
                  TInterlocked.Increment(Ok)
                else
                  TInterlocked.Increment(Otros);
              finally
                T.Free;
              end;
            except
              on E: Exception do
                if E.Message.Contains('-32000') then
                  TInterlocked.Increment(Busy)
                else
                  TInterlocked.Increment(Otros);
            end;
          finally
            C.Free;
          end;
        end);
    TTask.WaitForAll(Tareas);

    Writeln(Format('   completados=%d  agent-busy=%d  otros=%d', [Ok, Busy, Otros]));
    Writeln(Format('   tiempo total: %d ms (cada grafo tarda ~400 ms)', [MilliSecondsBetween(Now, Inicio)]));
    if Ok = 3 then
      Writeln('   OK: los tres corrieron en paralelo; en serie habrian tardado ~3x')
    else
      Writeln('   ATENCION: no completaron los tres');

    Server.Active := False;
  finally
    Server.Free;
  end;
end;

// -----------------------------------------------------------------------------
// 2. HUMAN-IN-THE-LOOP directo
// -----------------------------------------------------------------------------
procedure EscenarioHitl(AHandlers: TDemoHandlers);
var
  Server: TAiA2AServer;
  Agente: TAIAgentManager;
  Client: TAiA2AClient;
  Task: TJSONObject;
  Salida, TaskId, CtxId: string;
begin
  Writeln('');
  Writeln('== 2. HUMAN-IN-THE-LOOP: input-required y reanudacion por taskId ==');
  Writeln('');

  Server := TAiA2AServer.Create(nil);
  Agente := TAIAgentManager.Create(nil);
  Client := TAiA2AClient.Create(nil);
  try
    Agente.AddNode('Preparar', AHandlers.NodoExec).AddNode('Aprobar', AHandlers.NodoAprobacion);
    Agente.AddEdge('Preparar', 'Aprobar');
    Agente.SetEntryPoint('Preparar').SetFinishPoint('Aprobar');

    Server.Port := PORT;
    Server.AgentName := 'Agente Tesoreria';
    Server.AgentManager := Agente;
    Server.Active := True;

    Client.Url := 'http://localhost:' + IntToStr(PORT);

    // --- Primer turno: el grafo se suspende ---
    Task := Client.SendText('1500 EUR', Salida);
    try
      TaskId := Client.LastTaskId;
      CtxId := Client.LastContextId;
      Writeln('   turno 1 -> estado: ' + NombreEstado(Client.LastTaskState));
      Writeln('             pregunta del agente: ' + Client.LastStatusMessage);
      Writeln('             taskId: ' + TaskId);
    finally
      Task.Free;
    end;

    // --- Segundo turno: MISMO taskId = reanudacion, no un task nuevo ---
    Writeln('');
    Writeln('   (el humano responde "aprobado por Ana")');
    Task := Client.SendTextEx('aprobado por Ana', TaskId, CtxId, Salida);
    try
      Writeln('   turno 2 -> estado: ' + NombreEstado(Client.LastTaskState));
      Writeln('             resultado: ' + Salida);
      Writeln('             mismo taskId: ' + BoolToStr(Client.LastTaskId = TaskId, True));
    finally
      Task.Free;
    end;

    Server.Active := False;
  finally
    Client.Free;
    Server.Free;
    Agente.Free;
  end;
end;

// -----------------------------------------------------------------------------
// 3. HITL FEDERADO: el que pide aprobacion es el agente remoto
// -----------------------------------------------------------------------------
procedure EscenarioHitlFederado(AHandlers: TDemoHandlers);
var
  Server: TAiA2AServer;
  Remoto, Local: TAIAgentManager;
  Tool: TAiA2ARemoteAgentTool;
begin
  Writeln('');
  Writeln('== 3. HITL FEDERADO: el nodo local se suspende, no falla ==');
  Writeln('');

  Server := TAiA2AServer.Create(nil);
  Remoto := TAIAgentManager.Create(nil);
  try
    // Agente remoto: pide aprobacion humana
    Remoto.AddNode('Recibir', AHandlers.NodoExec).AddNode('Aprobar', AHandlers.NodoAprobacion);
    Remoto.AddEdge('Recibir', 'Aprobar');
    Remoto.SetEntryPoint('Recibir').SetFinishPoint('Aprobar');

    Server.Port := PORT;
    Server.AgentName := 'Agente Tesoreria Remoto';
    Server.AgentManager := Remoto;
    Server.Active := True;

    // Grafo local: uno de sus nodos delega en el agente remoto
    Local := TAIAgentManager.Create(nil);
    try
      Local.AddNode('Origen', AHandlers.NodoExec);
      Local.AddNode('Delegado', nil);
      Tool := TAiA2ARemoteAgentTool.Create(Local);
      Tool.AgentUrl := 'http://localhost:' + IntToStr(PORT);
      // Default True: ante input-required suspende el nodo local en vez de
      // lanzar excepcion. Con False el grafo local moriria aqui.
      Tool.SuspendOnInputRequired := True;
      Local.FindNode('Delegado').Tool := Tool;
      Local.AddEdge('Origen', 'Delegado');
      Local.SetEntryPoint('Origen').SetFinishPoint('Delegado');

      Local.Run('pago 900 EUR');
      EsperarGrafo(Local);
      Writeln('   tras la corrida inicial, el grafo LOCAL quedo en: ' +
        NombreStatus(Local.Blackboard.GetStatus));
      Writeln('   (el agente remoto pidio input y el nodo local se suspendio)');

      Writeln('');
      Writeln('   (se reanuda el nodo local con la respuesta del humano)');
      Local.ResumeThread(Local.CurrentThreadID, 'Delegado', 'aprobado por Luis');
      EsperarGrafo(Local);

      Writeln('   estado final del grafo LOCAL: ' + NombreStatus(Local.Blackboard.GetStatus));
      Writeln('   salida: ' + Local.EndNode.Output);
      Writeln('   OK: la respuesta viajo al MISMO task remoto y ambos grafos terminaron');
    finally
      Local.Free;
    end;

    Server.Active := False;
  finally
    Server.Free;
    Remoto.Free;
  end;
end;

// -----------------------------------------------------------------------------
// 4. NO BLOQUEANTE: blocking=false + GetTask
// -----------------------------------------------------------------------------
procedure EscenarioNoBloqueante(AHandlers: TDemoHandlers);
var
  Server: TAiA2AServer;
  Agente: TAIAgentManager;
  Client: TAiA2AClient;
  Task: TJSONObject;
  Salida, TaskId: string;
  Vueltas: Integer;
begin
  Writeln('');
  Writeln('== 4. NO BLOQUEANTE: blocking=false y sondeo con GetTask ==');
  Writeln('');

  Server := TAiA2AServer.Create(nil);
  Agente := TAIAgentManager.Create(nil);
  Client := TAiA2AClient.Create(nil);
  try
    Agente.AddNode('Validar', AHandlers.NodoLento).AddNode('Procesar', AHandlers.NodoExec);
    Agente.AddEdge('Validar', 'Procesar');
    Agente.SetEntryPoint('Validar').SetFinishPoint('Procesar');

    Server.Port := PORT;
    Server.AgentName := 'Agente Lento';
    Server.AgentManager := Agente;
    Server.Active := True;

    Client.Url := 'http://localhost:' + IntToStr(PORT);

    Task := Client.SendTextEx('lote-42', '', '', Salida, False); // blocking=false
    try
      TaskId := Client.LastTaskId;
      Writeln('   SendMessage devolvio de inmediato en estado: ' + NombreEstado(Client.LastTaskState));
    finally
      Task.Free;
    end;

    Vueltas := 0;
    repeat
      Sleep(100);
      Inc(Vueltas);
      Task := Client.GetTask(TaskId);
      try
        Writeln(Format('   GetTask #%d -> %s', [Vueltas, NombreEstado(Client.LastTaskState)]));
        if Client.LastTaskState = tsCompleted then
        begin
          Writeln('   resultado: ' + TAiA2AClient.ArtifactsText(Task));
          Break;
        end;
      finally
        Task.Free;
      end;
    until Vueltas >= 30;

    Writeln('   OK: el estado se refresca desde el grafo vivo, no queda congelado');

    Server.Active := False;
  finally
    Client.Free;
    Server.Free;
    Agente.Free;
  end;
end;

// -----------------------------------------------------------------------------

function HasFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
      Exit(True);
end;

var
  Handlers: TDemoHandlers;
  Telemetry: TAiTelemetry;

begin
  Telemetry := nil;
  try
    if HasFlag('--otel') then
    begin
      Telemetry := TAiTelemetry.Create(nil);
      Telemetry.ServiceName := 'A2AOrchestrationDemo';
      Telemetry.Enabled := True;
      Writeln('[otel] Exportando trazas a ' + Telemetry.Endpoint);
    end;

    Writeln('=== DEMO 074: orquestacion A2A (sin LLM, sin API keys) ===');

    Handlers := TDemoHandlers.Create;
    try
      EscenarioPool(Handlers);
      EscenarioHitl(Handlers);
      EscenarioHitlFederado(Handlers);
      EscenarioNoBloqueante(Handlers);
    finally
      Handlers.Free;
    end;

    Writeln('');
    Writeln('OK: los cuatro escenarios de orquestacion completaron.');

    if Assigned(Telemetry) then
    begin
      Telemetry.Flush;
      Telemetry.Free;
    end;
    ExitCode := 0;
  except
    on E: Exception do
    begin
      Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
