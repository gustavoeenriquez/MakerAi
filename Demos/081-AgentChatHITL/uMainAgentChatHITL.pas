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
// Idioma (v4.0): el combo de arriba a la derecha cambia TAiLocale.Language.
// Los textos del framework (menu contextual del chat, "N pasos", errores) ya
// vienen en ingles, espanol y portugues; los del demo se registran abajo con
// TAiLocale.RegisterTable y se piden con AiStr/AiFmt, igual que en el
// framework. Arranca en el idioma del sistema o con --idioma en|es|pt.
//
// No requiere API keys: los nodos hacen trabajo deterministico para que el
// flujo de suspension/reanudacion se vea claro. En una app real, "Preparar" y
// "Ejecutar" usarian TAiChatConnection u otras tools.

unit uMainAgentChatHITL;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ListBox,
  uMakerAi.Core,
  uMakerAi.Localization,
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
    ComboIdioma: TComboBox;
    ChatView1: TAIChatView;
    ChatInput1: TAIChatInput;
    procedure FormCreate(Sender: TObject);
    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
    procedure ChatInput1Cancel(Sender: TObject);
    procedure ComboIdiomaChange(Sender: TObject);
  private
    FAgent: TAIAgentManager;
    FAprobacion: TAiWaitApprovalTool;
    FPendingThreadID: string; // <> '' cuando el grafo espera la respuesta del usuario
    FPendingNodeName: string; // nodo que se debe reanudar (aqui siempre 'Aprobar')
    FEstado: string;          // clave del ultimo estado, para repintarlo al cambiar de idioma
    FSaludando: Boolean;      // el chat solo tiene el saludo: se rehace en el idioma nuevo
    procedure BuildGraph;
    procedure AplicarIdioma(const ALang: string);
    procedure Saludar;
    procedure NodePreparar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure NodeEjecutar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure AgentSuspended(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
    procedure AgentFinished(Sender: TObject; const Input, Output: string; Status: TAgentExecutionStatus; E: Exception);
    procedure AgentSay(const AText: string);
    procedure SetEstado(const AKey: string);
    function CheckpointDir: string;
  private
    // Prueba sin teclado: --prueba "mensaje" "mensaje" ... [--captura x.png]
    FPrueba: TStringList;
    FCaptura: string;
    FTimerPrueba: TTimer;
    FEsperaMs: Cardinal;
    procedure IniciarPrueba;
    procedure PasoPrueba(Sender: TObject);
  end;

var
  FormAgentChatHITL: TFormAgentChatHITL;

implementation

{$R *.fmx}

const
  NODO_APROBACION = 'Aprobar';
  IDIOMAS: array [0 .. 2] of string = ('es', 'en', 'pt');

// ---------------------------------------------------------------------------
// Textos del demo. Mismo mecanismo que el framework: pares clave/texto por
// idioma; una clave que falte en un idioma cae al ingles.
// ---------------------------------------------------------------------------

procedure RegistrarTextos;
begin
  TAiLocale.RegisterTable('en', [
    'demo081.caption',      'Demo 081 - Agent with human-in-the-loop in the chat',
    'demo081.status',       'Status: %s',
    'demo081.ready',        'Ready',
    'demo081.greeting1',    'Hi. Give me a task (for example: **publish the sales report**).',
    'demo081.greeting2',    'I''ll prepare a plan and ask for your approval right here in the chat before running it.',
    'demo081.recovered',    'I have a task suspended from a previous session waiting for your approval. Your next reply resumes it ("yes" approves, anything else cancels).',
    'demo081.waiting_recovered', 'Waiting for your reply (task recovered from the checkpoint)',
    'demo081.reason',       'I need your approval before running this plan:',
    'demo081.plan',         'Plan for "%s":',
    'demo081.step1',        '1. Gather the information needed',
    'demo081.step2',        '2. Prepare a draft of the result',
    'demo081.step3',        '3. Publish the final version',
    'demo081.approved',     'Approved. I ran the plan:',
    'demo081.done',         '**Task completed.**',
    'demo081.declined',     'Understood, I won''t run the plan (you said: "%s"). Write a new request whenever you like.',
    'demo081.answer_hint',  'Reply **yes** to approve; any other reply cancels.',
    'demo081.waiting',      'The agent is waiting for your reply',
    'demo081.agent_error',  'Agent error: %s',
    'demo081.unexpected',   'The graph ended with an unexpected status.',
    'demo081.error',        'Error',
    'demo081.resuming',     'Resuming the agent...',
    'demo081.resume_failed', 'I could not resume the suspended task; start a new request.',
    'demo081.working',      'Agent working...'
  ]);
  TAiLocale.RegisterTable('es', [
    'demo081.caption',      'Demo 081 - Agente con human-in-the-loop en el chat',
    'demo081.status',       'Estado: %s',
    'demo081.ready',        'Listo',
    'demo081.greeting1',    'Hola. Pídeme una tarea (por ejemplo: **publica el informe de ventas**).',
    'demo081.greeting2',    'Prepararé un plan y te pediré aprobación aquí mismo, en el chat, antes de ejecutarlo.',
    'demo081.recovered',    'Tengo una tarea suspendida de una sesión anterior esperando tu aprobación. Tu próxima respuesta la reanuda ("sí" aprueba, cualquier otra cosa cancela).',
    'demo081.waiting_recovered', 'Esperando tu respuesta (tarea recuperada del checkpoint)',
    'demo081.reason',       'Necesito tu aprobación antes de ejecutar este plan:',
    'demo081.plan',         'Plan para "%s":',
    'demo081.step1',        '1. Recopilar la información necesaria',
    'demo081.step2',        '2. Preparar el borrador del resultado',
    'demo081.step3',        '3. Publicar la versión final',
    'demo081.approved',     'Aprobado. Ejecuté el plan:',
    'demo081.done',         '**Tarea completada.**',
    'demo081.declined',     'Entendido, no ejecuto el plan (dijiste: "%s"). Escribe una nueva petición cuando quieras.',
    'demo081.answer_hint',  'Responde **sí** para aprobar; cualquier otra respuesta cancela.',
    'demo081.waiting',      'El agente espera tu respuesta',
    'demo081.agent_error',  'Error del agente: %s',
    'demo081.unexpected',   'El grafo terminó con un estado inesperado.',
    'demo081.error',        'Error',
    'demo081.resuming',     'Reanudando el agente...',
    'demo081.resume_failed', 'No pude reanudar la tarea suspendida; empieza una nueva petición.',
    'demo081.working',      'Agente trabajando...'
  ]);
  TAiLocale.RegisterTable('pt', [
    'demo081.caption',      'Demo 081 - Agente com human-in-the-loop no chat',
    'demo081.status',       'Status: %s',
    'demo081.ready',        'Pronto',
    'demo081.greeting1',    'Olá. Peça uma tarefa (por exemplo: **publique o relatório de vendas**).',
    'demo081.greeting2',    'Vou preparar um plano e pedir sua aprovação aqui mesmo, no chat, antes de executá-lo.',
    'demo081.recovered',    'Tenho uma tarefa suspensa de uma sessão anterior aguardando sua aprovação. Sua próxima resposta a retoma ("sim" aprova, qualquer outra coisa cancela).',
    'demo081.waiting_recovered', 'Aguardando sua resposta (tarefa recuperada do checkpoint)',
    'demo081.reason',       'Preciso da sua aprovação antes de executar este plano:',
    'demo081.plan',         'Plano para "%s":',
    'demo081.step1',        '1. Reunir as informações necessárias',
    'demo081.step2',        '2. Preparar o rascunho do resultado',
    'demo081.step3',        '3. Publicar a versão final',
    'demo081.approved',     'Aprovado. Executei o plano:',
    'demo081.done',         '**Tarefa concluída.**',
    'demo081.declined',     'Entendido, não vou executar o plano (você disse: "%s"). Escreva um novo pedido quando quiser.',
    'demo081.answer_hint',  'Responda **sim** para aprovar; qualquer outra resposta cancela.',
    'demo081.waiting',      'O agente aguarda sua resposta',
    'demo081.agent_error',  'Erro do agente: %s',
    'demo081.unexpected',   'O grafo terminou com um status inesperado.',
    'demo081.error',        'Erro',
    'demo081.resuming',     'Retomando o agente...',
    'demo081.resume_failed', 'Não consegui retomar a tarefa suspensa; comece um novo pedido.',
    'demo081.working',      'Agente trabalhando...'
  ]);
end;

procedure TFormAgentChatHITL.FormCreate(Sender: TObject);
var
  LThreads: TArray<string>;
  LLang: string;
  I: Integer;
begin
  BuildGraph;

  // La suspension se persiste en disco: si la app se cierra con una pregunta
  // pendiente, al arrancar se recupera y se puede reanudar.
  ForceDirectories(CheckpointDir);
  FAgent.Checkpointer := TAiFileCheckpointer.Create(CheckpointDir);
  FAgent.OnSuspend := AgentSuspended;
  FAgent.OnFinish := AgentFinished;

  // Idioma: el del sistema (TAiLocale ya lo detecto), salvo --idioma xx
  LLang := TAiLocale.PrimaryLanguage(TAiLocale.Language);
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), '--idioma') then
      LLang := TAiLocale.PrimaryLanguage(ParamStr(I + 1));
  ComboIdioma.Items.CommaText := 'Español,English,Português';
  ComboIdioma.ItemIndex := 1; // ingles si el sistema esta en otro idioma
  for I := 0 to High(IDIOMAS) do
    if IDIOMAS[I] = LLang then
      ComboIdioma.ItemIndex := I;   // dispara ComboIdiomaChange -> AplicarIdioma
  if TAiLocale.Language <> IDIOMAS[ComboIdioma.ItemIndex] then
    AplicarIdioma(IDIOMAS[ComboIdioma.ItemIndex]);

  Saludar;
  SetEstado('demo081.ready');

  // Human-in-the-loop durable: hilos suspendidos de una sesion anterior
  LThreads := FAgent.GetActiveThreads;
  if Length(LThreads) > 0 then
  begin
    FPendingThreadID := LThreads[0];
    FPendingNodeName := NODO_APROBACION;
    FSaludando := False;
    AgentSay(AiStr('demo081.recovered'));
    SetEstado('demo081.waiting_recovered');
  end;
  IniciarPrueba;
end;

procedure TFormAgentChatHITL.IniciarPrueba;
var
  I: Integer;
begin
  I := 1;
  while I <= ParamCount do
  begin
    if SameText(ParamStr(I), '--captura') and (I < ParamCount) then
    begin
      FCaptura := ParamStr(I + 1);
      Inc(I);
    end
    else if SameText(ParamStr(I), '--idioma') then
      Inc(I) // ya se aplico en FormCreate
    else if not SameText(ParamStr(I), '--prueba') then
    begin
      if not Assigned(FPrueba) then
        FPrueba := TStringList.Create;
      FPrueba.Add(ParamStr(I));
    end;
    Inc(I);
  end;
  if not Assigned(FPrueba) and (FCaptura = '') then
    Exit;
  FTimerPrueba := TTimer.Create(Self);
  FTimerPrueba.Interval := 100;
  FEsperaMs := TThread.GetTickCount + 1000;   // que se vea la ventana antes del primer mensaje
  FTimerPrueba.OnTimer := PasoPrueba;
end;

procedure TFormAgentChatHITL.PasoPrueba(Sender: TObject);
begin
  // Un mensaje a la vez, cuando el agente terminó con el anterior
  if ChatInput1.Busy or (TThread.GetTickCount < FEsperaMs) then
    Exit;
  if Assigned(FPrueba) and (FPrueba.Count > 0) then
  begin
    ChatInput1SendEvent(ChatInput1, FPrueba[0], nil, nil);
    FPrueba.Delete(0);
    FEsperaMs := TThread.GetTickCount + 600;
    Exit;
  end;
  FTimerPrueba.Enabled := False;
  if FCaptura <> '' then
    with ChatView1.MakeScreenshot do
    try
      SaveToFile(FCaptura);
    finally
      Free;
    end;
  FPrueba.Free;
  Close;
end;

procedure TFormAgentChatHITL.BuildGraph;
var
  LNode: TAIAgentsNode;
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
  // SuspendReason se pone en AplicarIdioma (depende del idioma).
  LNode := FAgent.FindNode(NODO_APROBACION);
  FAprobacion := TAiWaitApprovalTool.Create(LNode);
  FAprobacion.ContextKey := 'plan'; // clave del blackboard que viaja como contexto
  LNode.Tool := FAprobacion;
end;

// ---------------------------------------------------------------------------
// Idioma
// ---------------------------------------------------------------------------

procedure TFormAgentChatHITL.ComboIdiomaChange(Sender: TObject);
begin
  if ComboIdioma.ItemIndex >= 0 then
    AplicarIdioma(IDIOMAS[ComboIdioma.ItemIndex]);
end;

procedure TFormAgentChatHITL.AplicarIdioma(const ALang: string);
begin
  // Cambia el idioma de TODO: framework (menu del chat, errores) y demo.
  // El menu contextual de TAIChatView se rehace solo al abrirse.
  TAiLocale.Language := ALang;
  Caption := AiStr('demo081.caption');
  FAprobacion.SuspendReason := AiStr('demo081.reason');
  if FEstado <> '' then
    SetEstado(FEstado);
  // Si todavia no hay conversacion, el saludo se rehace en el idioma nuevo
  if FSaludando and (ChatView1.MessageCount = 1) then
  begin
    ChatView1.ClearMessages;
    Saludar;
  end;
end;

procedure TFormAgentChatHITL.Saludar;
begin
  AgentSay(AiStr('demo081.greeting1') + sLineBreak + AiStr('demo081.greeting2'));
  FSaludando := True;
end;

// ---------------------------------------------------------------------------
// Nodos del grafo (corren en threads del pool; no tocan la UI)
// ---------------------------------------------------------------------------

procedure TFormAgentChatHITL.NodePreparar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  LPlan: string;
begin
  // Aqui iria el trabajo real (un TAiChatConnection que genere el plan, etc.).
  // AiStr/AiFmt se pueden llamar desde cualquier hilo.
  LPlan := AiFmt('demo081.plan', [Trim(Input)]) + sLineBreak +
    AiStr('demo081.step1') + sLineBreak +
    AiStr('demo081.step2') + sLineBreak +
    AiStr('demo081.step3');
  Node.Graph.Blackboard.SetString('plan', LPlan);
  Output := LPlan;
end;

procedure TFormAgentChatHITL.NodeEjecutar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  LResp: string;
begin
  // Input es la respuesta que el usuario escribio en el chat al reanudar.
  // Se acepta "si"/"sí" (es), "yes"/"ok" (en) y "sim" (pt).
  LResp := Trim(Input).ToLower;
  if LResp.StartsWith('si') or LResp.StartsWith('sí') or LResp.StartsWith('ok') or
     LResp.StartsWith('yes') or LResp.StartsWith('dale') or
     (Pos('aprob', LResp) > 0) or (Pos('aprov', LResp) > 0) or (Pos('approv', LResp) > 0) then
    // Salto doble: en Markdown uno solo une la linea con la primera del plan
    Output := AiStr('demo081.approved') + sLineBreak + sLineBreak +
      Node.Graph.Blackboard.GetString('plan') + sLineBreak + sLineBreak +
      AiStr('demo081.done')
  else
    Output := AiFmt('demo081.declined', [Trim(Input)]);
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
        AiStr('demo081.answer_hint'));
      SetEstado('demo081.waiting');
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
            SetEstado('demo081.ready');
          end;
      else
        begin
          ChatInput1.Busy := False;
          if LError <> '' then
            AgentSay(AiFmt('demo081.agent_error', [LError]))
          else
            AgentSay(AiStr('demo081.unexpected'));
          SetEstado('demo081.error');
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

  FSaludando := False;
  ChatView1.AddUserMessage(APrompt);
  ChatInput1.Busy := True;

  if FPendingThreadID <> '' then
  begin
    // Hay un grafo suspendido: esta respuesta lo reanuda donde quedo
    LThreadID := FPendingThreadID;
    FPendingThreadID := '';
    SetEstado('demo081.resuming');
    if not FAgent.ResumeThread(LThreadID, FPendingNodeName, APrompt) then
    begin
      ChatInput1.Busy := False;
      AgentSay(AiStr('demo081.resume_failed'));
      SetEstado('demo081.ready');
    end;
  end
  else
  begin
    // Corrida nueva y limpia (el seed vacio fuerza ResetExecutionState)
    SetEstado('demo081.working');
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

procedure TFormAgentChatHITL.SetEstado(const AKey: string);
begin
  // Se guarda la clave, no el texto: al cambiar de idioma se vuelve a pintar
  FEstado := AKey;
  lblEstado.Text := AiFmt('demo081.status', [AiStr(AKey)]);
end;

function TFormAgentChatHITL.CheckpointDir: string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'Checkpoints');
end;

initialization
  RegistrarTextos;

end.
