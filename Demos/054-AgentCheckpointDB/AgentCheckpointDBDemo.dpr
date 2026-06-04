program AgentCheckpointDBDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.Client,
  FireDAC.ConsoleUI.Wait,
  Data.DB,
  uMakerAi.Agents,
  uMakerAi.Agents.Checkpoint,
  uMakerAi.Agents.Checkpoint.DB;

// ---------------------------------------------------------------------------
// Demo 054 — TAiDatabaseCheckpointer con SQLite
//
// Flujo del agente:
//   Inicio -> Procesar -> Aprobar (SUSPEND) -> Finalizar
//
// Fase 1: El agente se ejecuta y se suspende en el nodo "Aprobar".
//         El checkpoint se guarda automaticamente en la BD SQLite.
//
// Fase 2: El usuario presiona ENTER (simula aprobacion del operador).
//         Se llama a ResumeThread para reanudar desde "Finalizar".
//         El checkpoint se elimina al completar.
//
// Si hay checkpoints previos en la BD, el demo ofrece reanudarlos.
// ---------------------------------------------------------------------------

type
  TDemo = class
  private
    FAgents: TAIAgentManager;
    FConnection: TFDConnection;
    FCheckpointer: IAiCheckpointer;
    FSuspendedThreadID: string;
    procedure SetupDatabase;
    procedure BuildGraph;
    procedure WaitForCompletion;
    procedure Println(const Msg: string);
    // Eventos del agente — firmas exactas segun TAIAgentsOnXxx
    procedure DoPrint(Sender: TObject; Value: string);
    procedure DoEnd(Node: TAIAgentsNode; Value: string);
    procedure DoError(Sender: TObject; Node: TAIAgentsNode;
      Link: TAIAgentsLink; E: Exception; var Abort: Boolean);
    procedure DoSuspend(Sender: TObject;
      const AThreadID, ANodeName, AReason, AContext: string);
    // Executors de nodos — firma TAIAgentsNodeOnExecute
    procedure InicioExec(Node, BeforeNode: TAIAgentsNode;
      Link: TAIAgentsLink; Input: string; var Output: string);
    procedure ProcesarExec(Node, BeforeNode: TAIAgentsNode;
      Link: TAIAgentsLink; Input: string; var Output: string);
    procedure AprobarExec(Node, BeforeNode: TAIAgentsNode;
      Link: TAIAgentsLink; Input: string; var Output: string);
    procedure FinalizarExec(Node, BeforeNode: TAIAgentsNode;
      Link: TAIAgentsLink; Input: string; var Output: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TDemo }

procedure TDemo.Println(const Msg: string);
begin
  Writeln('[DEMO] ' + Msg);
end;

constructor TDemo.Create;
var
  LCp: TAiDatabaseCheckpointer;
begin
  inherited Create;
  FSuspendedThreadID := '';
  SetupDatabase;
  LCp := TAiDatabaseCheckpointer.Create(FConnection);
  LCp.CreateSchema;
  FCheckpointer := LCp;
  FAgents := TAIAgentManager.Create(nil);
  FAgents.Checkpointer := FCheckpointer;
  FAgents.OnPrint   := DoPrint;
  FAgents.OnEnd     := DoEnd;
  FAgents.OnError   := DoError;
  FAgents.OnSuspend := DoSuspend;
end;

destructor TDemo.Destroy;
begin
  FAgents.Free;
  FCheckpointer := nil;
  FConnection.Free;
  inherited;
end;

procedure TDemo.SetupDatabase;
var
  DBPath: string;
begin
  DBPath := ChangeFileExt(ParamStr(0), '.db');
  FConnection := TFDConnection.Create(nil);
  FConnection.DriverName := 'SQLite';
  FConnection.Params.Values['Database'] := DBPath;
  FConnection.Params.Values['OpenMode'] := 'CreateUTF8';
  FConnection.LoginPrompt := False;
  FConnection.Open;
  Println('BD SQLite: ' + DBPath);
end;

procedure TDemo.BuildGraph;
begin
  FAgents.ClearGraph;
  FAgents
    .AddNode('Inicio',    InicioExec)
    .AddNode('Procesar',  ProcesarExec)
    .AddNode('Aprobar',   AprobarExec)
    .AddNode('Finalizar', FinalizarExec);
  FAgents.AddEdge('Inicio',   'Procesar');
  FAgents.AddEdge('Procesar', 'Aprobar');
  FAgents.AddEdge('Aprobar',  'Finalizar');
  FAgents.SetEntryPoint('Inicio').SetFinishPoint('Finalizar');
end;

procedure TDemo.WaitForCompletion;
begin
  while FAgents.Busy do
  begin
    CheckSynchronize;
    Sleep(50);
  end;
  CheckSynchronize;
  Println('--- Ciclo de ejecucion terminado. ---');
end;

procedure TDemo.DoPrint(Sender: TObject; Value: string);
begin
  TThread.Synchronize(nil, procedure begin
    Writeln('[AGENT] ' + Value);
  end);
end;

procedure TDemo.DoEnd(Node: TAIAgentsNode; Value: string);
begin
  TThread.Synchronize(nil, procedure begin
    Writeln;
    Println('Flujo completado con exito.');
    Println('Output final: "' + Value + '"');
  end);
end;

procedure TDemo.DoError(Sender: TObject; Node: TAIAgentsNode;
  Link: TAIAgentsLink; E: Exception; var Abort: Boolean);
var
  LNodeName, LMsg: string;
begin
  LNodeName := '';
  if Assigned(Node) then LNodeName := Node.Name;
  LMsg := '';
  if Assigned(E) then LMsg := E.ClassName + ': ' + E.Message;
  TThread.Synchronize(nil, procedure begin
    Writeln('[ERROR] Nodo: ' + LNodeName);
    if LMsg <> '' then
      Writeln('[ERROR] ' + LMsg);
  end);
  Abort := False;
end;

procedure TDemo.DoSuspend(Sender: TObject;
  const AThreadID, ANodeName, AReason, AContext: string);
begin
  TThread.Synchronize(nil, procedure begin
    Writeln;
    Println('*** AGENTE SUSPENDIDO — Accion humana requerida ***');
    Println('  Thread ID  : ' + AThreadID);
    Println('  Nodo       : ' + ANodeName);
    Println('  Razon      : ' + AReason);
    Println('  Contexto   : ' + AContext);
    FSuspendedThreadID := AThreadID;
  end);
end;

{ Executors de nodos }

procedure TDemo.InicioExec(Node, BeforeNode: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  TThread.Synchronize(nil, procedure begin
    Println('[Inicio] Proceso recibido: "' + Input + '"');
  end);
  Output := 'Proceso iniciado: ' + Input;
end;

procedure TDemo.ProcesarExec(Node, BeforeNode: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  TThread.Synchronize(nil, procedure begin
    Println('[Procesar] Procesando datos...');
  end);
  Sleep(400);
  Output := 'Datos procesados. ' + Input;
end;

procedure TDemo.AprobarExec(Node, BeforeNode: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  TThread.Synchronize(nil, procedure begin
    Println('[Aprobar] Solicitud enviada al operador. Suspendiendo...');
  end);
  Output := Input;
  // Suspende la ejecucion; el framework guarda el checkpoint automaticamente
  Node.Suspend('Aprobacion requerida por operador', 'Datos listos: ' + Input);
end;

procedure TDemo.FinalizarExec(Node, BeforeNode: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  TThread.Synchronize(nil, procedure begin
    Println('[Finalizar] Proceso completado. Input: "' + Input + '"');
  end);
  Output := 'COMPLETADO: ' + Input;
end;

{ Logica principal }

procedure TDemo.Run;
var
  ActiveThreads: TArray<string>;
  Choice: string;
  I: Integer;
begin
  Writeln;
  Writeln('============================================');
  Writeln('   MakerAI — Demo 054: Checkpoint DB      ');
  Writeln('============================================');
  Writeln('Flujo: Inicio -> Procesar -> Aprobar (SUSPEND) -> Finalizar');
  Writeln('Checkpointer: TAiDatabaseCheckpointer (SQLite via FireDAC)');
  Writeln;

  // Verificar si hay threads suspendidos de ejecuciones anteriores
  ActiveThreads := FCheckpointer.GetActiveThreadIDs;
  if Length(ActiveThreads) > 0 then
  begin
    Println(Format('Se encontraron %d thread(s) suspendido(s) en BD:',
      [Length(ActiveThreads)]));
    for I := 0 to High(ActiveThreads) do
      Println(Format('  [%d] %s', [I + 1, ActiveThreads[I]]));
    Writeln;
    Write('Reanudar el primer thread? (S/N): ');
    Readln(Choice);
    if Choice.Trim.ToUpper = 'S' then
    begin
      BuildGraph;
      Println('Reanudando thread: ' + ActiveThreads[0]);
      // 'Finalizar' es el nodo que sigue al punto de suspension ('Aprobar')
      FAgents.ResumeThread(ActiveThreads[0], 'Finalizar', 'Aprobado desde BD');
      WaitForCompletion;
      FCheckpointer.DeleteCheckpoint(ActiveThreads[0]);
      Println('Checkpoint eliminado de la BD.');
    end
    else
      Println('Operacion cancelada.');
    Exit;
  end;

  // =========================================================================
  // FASE 1: Primera ejecucion — el agente se suspende en "Aprobar"
  // =========================================================================
  Writeln;
  Println('=== FASE 1: Primera ejecucion ===');
  Writeln;
  Writeln('  Inicio -> Procesar -> Aprobar (se suspende) -> Finalizar (pendiente)');
  Writeln;

  BuildGraph;
  FAgents.Run('Solicitud-2026-001');
  WaitForCompletion;

  if FSuspendedThreadID = '' then
  begin
    Println('El agente termino sin suspenderse (inesperado).');
    Exit;
  end;

  // Mostrar estado guardado en BD
  Writeln;
  Println('=== CHECKPOINT GUARDADO EN BD SQLITE ===');
  Println('Thread ID : ' + FSuspendedThreadID);
  Println('BD file   : ' + ChangeFileExt(ParamStr(0), '.db'));
  Println('El estado persiste. Puedes reiniciar la app y el thread');
  Println('aparecera como suspendido al volver a ejecutar el demo.');
  Writeln;

  // =========================================================================
  // FASE 2: Resume — simula aprobacion del operador
  // =========================================================================
  Println('=== FASE 2: Reanudar tras aprobacion ===');
  Write('Presione ENTER para simular la aprobacion del operador...');
  Readln;

  Writeln;
  Println('Reanudando ejecucion desde "Finalizar"...');
  // 'Finalizar' es el primer nodo pendiente tras el punto de suspension
  FAgents.ResumeThread(FSuspendedThreadID, 'Finalizar',
    'Aprobado por operador humano');
  WaitForCompletion;

  // Cleanup
  FCheckpointer.DeleteCheckpoint(FSuspendedThreadID);
  Writeln;
  Println('Checkpoint eliminado de la BD.');
  Println('Demo finalizado correctamente.');
end;

// ---------------------------------------------------------------------------

var
  Demo: TDemo;
begin
  try
    Demo := TDemo.Create;
    try
      Demo.Run;
    finally
      Demo.Free;
    end;
    Writeln;
    Write('Presione ENTER para salir...');
    Readln;
  except
    on E: Exception do
    begin
      Writeln('Error fatal: ', E.ClassName, ' - ', E.Message);
      Write('Presione ENTER para salir...');
      Readln;
    end;
  end;
end.
