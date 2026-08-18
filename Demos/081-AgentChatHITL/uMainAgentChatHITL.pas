// Demo 081 - Human-in-the-loop integrado en el chat
//
// Muestra como un grafo de agentes pide intervencion del usuario SIN ventanas
// modales ni threads bloqueados: el nodo de aprobacion suspende el grafo
// (TAiWaitApprovalTool -> Node.Suspend), la pregunta aparece como un mensaje
// mas del chat (TAIChatView) y la siguiente respuesta del usuario en
// TAIChatInput reanuda la ejecucion con ResumeThread. Como nada queda en
// espera, el mismo patron funciona en Windows, Android y Linux.
//
// Ademas, con TAiFileCheckpointer la suspension sobrevive al cierre de la app:
// al arrancar se detectan los hilos pendientes (GetActiveThreads) y la proxima
// respuesta del chat los reanuda, incluso tras reiniciar el proceso.
//
// No requiere API keys: los nodos hacen trabajo deterministico para que el
// flujo de suspension/reanudacion se vea claro. En una app real, "Preparar" y
// "Ejecutar" usarian TAiChatConnection u otras tools.

unit uMainAgentChatHITL;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation,
  uMakerAi.Core,
  uMakerAi.Agents,
  uMakerAi.Agents.Checkpoint,
  uMakerAi.Agents.Tools.Approval,
  AIChat.Types,
  AIChat.Control.FMX,
  AIChat.Input;

type
  TFormAgentChatHITL = class(TForm)
    LayoutTop: TLayout;
    lblEstado: TLabel;
    ChatView1: TAIChatView;
    ChatInput1: TAIChatInput;
    procedure FormCreate(Sender: TObject);
    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
    procedure ChatInput1Cancel(Sender: TObject);
  private
    FAgent: TAIAgentManager;
    FPendingThreadID: string; // <> '' cuando el grafo espera la respuesta del usuario
    FPendingNodeName: string; // nodo que se debe reanudar (aqui siempre 'Aprobar')
    procedure BuildGraph;
    procedure NodePreparar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure NodeEjecutar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure AgentSuspended(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
    procedure AgentFinished(Sender: TObject; const Input, Output: string; Status: TAgentExecutionStatus; E: Exception);
    procedure AgentSay(const AText: string);
    procedure SetEstado(const AText: string);
    function CheckpointDir: string;
  end;

var
  FormAgentChatHITL: TFormAgentChatHITL;

implementation

{$R *.fmx}

const
  NODO_APROBACION = 'Aprobar';

procedure TFormAgentChatHITL.FormCreate(Sender: TObject);
var
  LThreads: TArray<string>;
begin
  BuildGraph;

  // La suspension se persiste en disco: si la app se cierra con una pregunta
  // pendiente, al arrancar se recupera y se puede reanudar.
  ForceDirectories(CheckpointDir);
  FAgent.Checkpointer := TAiFileCheckpointer.Create(CheckpointDir);
  FAgent.OnSuspend := AgentSuspended;
  FAgent.OnFinish := AgentFinished;

  AgentSay('Hola. Pideme una tarea (por ejemplo: **publica el informe de ventas**).' + sLineBreak +
    'Preparare un plan y te pedire aprobacion aqui mismo, en el chat, antes de ejecutarlo.');
  SetEstado('Listo');

  // Human-in-the-loop durable: hilos suspendidos de una sesion anterior
  LThreads := FAgent.GetActiveThreads;
  if Length(LThreads) > 0 then
  begin
    FPendingThreadID := LThreads[0];
    FPendingNodeName := NODO_APROBACION;
    AgentSay('Tengo una tarea suspendida de una sesion anterior esperando tu aprobacion. ' +
      'Tu proxima respuesta la reanuda ("si" aprueba, cualquier otra cosa cancela).');
    SetEstado('Esperando tu respuesta (tarea recuperada del checkpoint)');
  end;
end;

procedure TFormAgentChatHITL.BuildGraph;
var
  LNode: TAIAgentsNode;
  LTool: TAiWaitApprovalTool;
begin
  FAgent := TAIAgentManager.Create(Self);
  // Asincrono: Run devuelve de inmediato y el resultado llega por eventos.
  // Nada bloquea el hilo de la UI ni queda un worker en espera.
  FAgent.Asynchronous := True;

  // Preparar -> Aprobar (suspende) -> Ejecutar
  FAgent.AddNode('Preparar', NodePreparar);
  FAgent.AddNode(NODO_APROBACION, nil);
  FAgent.AddNode('Ejecutar', NodeEjecutar);
  FAgent.AddEdge('Preparar', NODO_APROBACION);
  FAgent.AddEdge(NODO_APROBACION, 'Ejecutar');
  FAgent.SetEntryPoint('Preparar');
  FAgent.SetFinishPoint('Ejecutar');

  // La tool de fabrica para HITL: al ejecutarse suspende el grafo y dispara
  // OnSuspend; al reanudar, deja pasar la respuesta humana como Output.
  LNode := FAgent.FindNode(NODO_APROBACION);
  LTool := TAiWaitApprovalTool.Create(LNode);
  LTool.SuspendReason := 'Necesito tu aprobacion antes de ejecutar este plan:';
  LTool.ContextKey := 'plan'; // clave del blackboard que viaja como contexto
  LNode.Tool := LTool;
end;

// ---------------------------------------------------------------------------
// Nodos del grafo (corren en threads del pool; no tocan la UI)
// ---------------------------------------------------------------------------

procedure TFormAgentChatHITL.NodePreparar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  LPlan: string;
begin
  // Aqui iria el trabajo real (un TAiChatConnection que genere el plan, etc.)
  LPlan := 'Plan para "' + Trim(Input) + '":' + sLineBreak +
    '1. Recopilar la informacion necesaria' + sLineBreak +
    '2. Preparar el borrador del resultado' + sLineBreak +
    '3. Publicar la version final';
  Node.Graph.Blackboard.SetString('plan', LPlan);
  Output := LPlan;
end;

procedure TFormAgentChatHITL.NodeEjecutar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  LResp: string;
begin
  // Input es la respuesta que el usuario escribio en el chat al reanudar
  LResp := Trim(Input).ToLower;
  if LResp.StartsWith('si') or LResp.StartsWith('ok') or LResp.StartsWith('yes') or
     LResp.StartsWith('dale') or (Pos('aprob', LResp) > 0) then
    Output := 'Aprobado. Ejecute el plan:' + sLineBreak +
      Node.Graph.Blackboard.GetString('plan') + sLineBreak +
      '**Tarea completada.**'
  else
    Output := 'Entendido, no ejecuto el plan (dijiste: "' + Trim(Input) + '"). ' +
      'Escribe una nueva peticion cuando quieras.';
end;

// ---------------------------------------------------------------------------
// Eventos del agente (llegan en threads del pool -> TThread.Queue para la UI)
// ---------------------------------------------------------------------------

procedure TFormAgentChatHITL.AgentSuspended(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      FPendingThreadID := AThreadID;
      FPendingNodeName := ANodeName;
      ChatInput1.Busy := False;
      AgentSay(AReason + sLineBreak + sLineBreak + AContext + sLineBreak + sLineBreak +
        'Responde **si** para aprobar; cualquier otra respuesta cancela.');
      SetEstado('El agente espera tu respuesta');
    end);
end;

procedure TFormAgentChatHITL.AgentFinished(Sender: TObject; const Input, Output: string; Status: TAgentExecutionStatus; E: Exception);
var
  LError: string;
begin
  // No capturar E dentro del closure: la excepcion se libera al volver
  if Assigned(E) then
    LError := E.Message
  else
    LError := '';

  TThread.Queue(nil,
    procedure
    begin
      case Status of
        esSuspended:
          ; // la UI ya se actualizo en AgentSuspended
        esCompleted:
          begin
            ChatInput1.Busy := False;
            AgentSay(Output);
            SetEstado('Listo');
          end;
      else
        begin
          ChatInput1.Busy := False;
          if LError <> '' then
            AgentSay('Error del agente: ' + LError)
          else
            AgentSay('El grafo termino con estado inesperado.');
          SetEstado('Error');
        end;
      end;
    end);
end;

// ---------------------------------------------------------------------------
// Chat
// ---------------------------------------------------------------------------

procedure TFormAgentChatHITL.ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
var
  LThreadID: string;
begin
  if Trim(APrompt) = '' then
  begin
    ChatInput1.Busy := False;
    Exit;
  end;

  ChatView1.AddUserMessage(APrompt);
  ChatInput1.Busy := True;

  if FPendingThreadID <> '' then
  begin
    // Hay un grafo suspendido: esta respuesta lo reanuda donde quedo
    LThreadID := FPendingThreadID;
    FPendingThreadID := '';
    SetEstado('Reanudando el agente...');
    if not FAgent.ResumeThread(LThreadID, FPendingNodeName, APrompt) then
    begin
      ChatInput1.Busy := False;
      AgentSay('No pude reanudar la tarea suspendida; empieza una nueva peticion.');
      SetEstado('Listo');
    end;
  end
  else
  begin
    // Corrida nueva y limpia (el seed vacio fuerza ResetExecutionState)
    SetEstado('Agente trabajando...');
    FAgent.Run(APrompt,
      procedure(B: TAIBlackboard)
      begin
        // sin claves precargadas; el reset previo garantiza estado limpio
      end);
  end;
end;

procedure TFormAgentChatHITL.ChatInput1Cancel(Sender: TObject);
begin
  ChatInput1.Busy := False;
end;

// ---------------------------------------------------------------------------
// Utilidades
// ---------------------------------------------------------------------------

procedure TFormAgentChatHITL.AgentSay(const AText: string);
begin
  ChatView1.AddMessage(TChatRole.crAssistant, AText);
end;

procedure TFormAgentChatHITL.SetEstado(const AText: string);
begin
  lblEstado.Text := 'Estado: ' + AText;
end;

function TFormAgentChatHITL.CheckpointDir: string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'Checkpoints');
end;

end.
