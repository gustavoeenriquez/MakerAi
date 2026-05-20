program D10_HumanInTheLoop;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - AutoAgents Demo 10: HumanInTheLoop
// =============================================================================
// Agente con aprobacion humana en el flujo:
//   [Drafter] --suspend--> [Human Review] --> [Finalizer]
//
// El nodo de revision llama Node.Suspend() mostrando el borrador.
// El usuario aprueba o rechaza con feedback desde la consola.
// Si aprueba: el Finalizer pule el texto.
// Si rechaza: el Drafter regenera con las instrucciones del usuario.
//
// Usa Asynchronous=True + OnSuspend + ResumeThread para el ciclo de aprobacion.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  uMakerAi.Agents,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

// =============================================================================
// Estado compartido para el ciclo suspend/resume
// =============================================================================
var
  GSuspendThreadID : String = '';
  GSuspendNodeName : String = '';
  GSuspendReason   : String = '';
  GSuspendContext  : String = '';
  GSuspended       : Boolean = False;
  GFinished        : Boolean = False;
  GFinalOutput     : String = '';

// =============================================================================
// TDemoApp — encapsula el agente y el ciclo de aprobacion
// =============================================================================
type
  TDemoApp = class
  private
    FManager   : TAIAgentManager;
    FDrafter   : TLLMNode;
    FReviewer  : TAIAgentsNode;  // nodo plain que llama Suspend
    FFinalizer : TLLMNode;
    FLink1     : TAIAgentsLink;
    FLink2     : TAIAgentsLink;

    procedure OnSuspend(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
    procedure OnFinish(Sender: TObject; const AInput, AOutput: string;
                       Status: TAgentExecutionStatus; E: Exception);
    procedure ReviewerExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink;
                           Input: String; var Output: String);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Run(const ATask: String);
  end;

procedure TDemoApp.OnSuspend(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
begin
  TThread.Queue(nil, procedure
  begin
    GSuspendThreadID := AThreadID;
    GSuspendNodeName := ANodeName;
    GSuspendReason   := AReason;
    GSuspendContext  := AContext;
    GSuspended       := True;
  end);
end;

procedure TDemoApp.OnFinish(Sender: TObject; const AInput, AOutput: string;
  Status: TAgentExecutionStatus; E: Exception);
begin
  // esSuspended no es terminacion: el agente espera aprobacion humana.
  // OnSuspend ya notifica al main loop. Ignorar aqui para no adelantar GFinished.
  if Status = esSuspended then Exit;

  TThread.Queue(nil, procedure
  begin
    GFinalOutput := AOutput;
    GFinished    := True;
  end);
end;

procedure TDemoApp.ReviewerExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink;
  Input: String; var Output: String);
var
  ReviewResult: String;
begin
  // Node.Suspend() solo marca un flag y retorna; NO bloquea.
  // DoTraverseLinks detecta FSuspended=True y no enruta al siguiente nodo.
  // ResumeThread volvera a llamar DoExecute (con aBeforeNode=nil).
  // Usamos review_result en el blackboard para distinguir primera llamada vs. reanudacion.
  ReviewResult := Node.Graph.Blackboard.GetString('review_result');
  if ReviewResult = '' then
  begin
    // Primera llamada: guardar borrador y suspender para aprobacion humana
    Node.Graph.Blackboard.SetString('current_draft', Input);
    Node.Suspend('Human review required', Input);
    // Output se ignora (DoTraverseLinks no enrutara gracias a FSuspended=True)
  end
  else
  begin
    // Reanudacion: el usuario aprobo; pasar el borrador aprobado al Finalizer
    Output := ReviewResult;
    Node.Graph.Blackboard.SetString('review_result', ''); // limpiar para proxima vuelta
  end;
end;

constructor TDemoApp.Create;
begin
  inherited Create;

  FManager := TAIAgentManager.Create(nil);
  FManager.Asynchronous := True;   // NECESARIO para suspend/resume
  FManager.TimeoutMs    := 300000; // 5 minutos (incluye tiempo de revision humana)
  FManager.OnSuspend    := OnSuspend;
  FManager.OnFinish     := OnFinish;

  // Nodo 1: Redactor — genera el borrador inicial
  FDrafter := TLLMNode.Create(FManager);
  FDrafter.Name         := 'Drafter';
  FDrafter.Graph        := FManager;
  FDrafter.DriverName   := DRIVER;
  FDrafter.Model        := MODEL;
  FDrafter.ApiKey       := API_KEY;
  FDrafter.UseAllTools  := False;
  FDrafter.SystemPrompt :=
    'You are a professional writer. Write a clear, engaging paragraph (100-150 words) ' +
    'about the given topic. If you receive feedback/instructions, incorporate them fully.';

  // Nodo 2: Revisor humano — suspende la ejecucion para aprobacion
  FReviewer := TAIAgentsNode.Create(FManager);
  FReviewer.Name     := 'HumanReviewer';
  FReviewer.Graph    := FManager;
  FReviewer.OnExecute := ReviewerExec;

  // Nodo 3: Finalizador — pule el texto aprobado
  FFinalizer := TLLMNode.Create(FManager);
  FFinalizer.Name         := 'Finalizer';
  FFinalizer.Graph        := FManager;
  FFinalizer.DriverName   := DRIVER;
  FFinalizer.Model        := MODEL;
  FFinalizer.ApiKey       := API_KEY;
  FFinalizer.UseAllTools  := False;
  FFinalizer.SystemPrompt :=
    'You are a copy editor. You receive an approved draft. ' +
    'Polish it: fix any awkward phrasing, improve word choice, ensure good flow. ' +
    'Keep the same meaning and length. Output only the polished text.';

  // Encadenar: Drafter -> HumanReviewer -> Finalizer
  FLink1 := TAIAgentsLink.Create(FManager);
  FLink1.Name := 'L1'; FLink1.Graph := FManager; FLink1.NextA := FReviewer;
  FDrafter.Next := FLink1;

  FLink2 := TAIAgentsLink.Create(FManager);
  FLink2.Name := 'L2'; FLink2.Graph := FManager; FLink2.NextA := FFinalizer;
  FReviewer.Next := FLink2;

  FManager.StartNode := FDrafter;
  FManager.EndNode   := FFinalizer;
  FManager.Compile;
end;

destructor TDemoApp.Destroy;
begin
  FManager.Free;
  inherited;
end;

procedure TDemoApp.Run(const ATask: String);
var
  UserInput   : String;
  Approved    : Boolean;
  Iteration   : Integer;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tarea: ', ATask);
  Writeln(StringOfChar('=', 60));

  GSuspended := False;
  GFinished  := False;
  GFinalOutput := '';

  Iteration := 0;
  Approved := False;

  // Lanzar el agente (retorna inmediatamente por Asynchronous=True)
  FManager.Run(ATask);

  repeat
    Inc(Iteration);

    // Esperar hasta que el agente se suspenda o termine
    while (not GSuspended) and (not GFinished) do
    begin
      CheckSynchronize;
      Sleep(100);
    end;

    if GFinished then
      Break;

    if GSuspended then
    begin
      GSuspended := False;

      // Mostrar el borrador al usuario
      Writeln;
      Writeln(Format('--- Borrador (iteracion %d) ---', [Iteration]));
      Writeln(GSuspendContext);
      Writeln;
      Write('Apruebas este borrador? (s=si, n=no con feedback): ');
      Readln(UserInput);
      Writeln;

      if (Trim(LowerCase(UserInput)) = 's') or (Trim(LowerCase(UserInput)) = 'si') or (Trim(LowerCase(UserInput)) = 'y') then
      begin
        Approved := True;
        // Guardar el resultado de la revision en el blackboard
        FManager.Blackboard.SetString('review_result', GSuspendContext);
        FManager.ResumeThread(GSuspendThreadID, GSuspendNodeName, GSuspendContext);
      end
      else
      begin
        // Feedback del usuario: regenerar con instrucciones
        if Trim(UserInput) = '' then
          UserInput := 'Please improve the text quality.';

        Writeln('Regenerando con tu feedback...');
        // Cancelar la ejecucion actual y relanzar con instrucciones
        FManager.Abort;
        Sleep(200);

        // Reiniciar flags
        GSuspended := False;
        GFinished  := False;
        GFinalOutput := '';

        // Relanzar con las instrucciones del usuario como parte del prompt
        var NewPrompt := ATask + sLineBreak + sLineBreak +
          'PREVIOUS DRAFT (needs improvement):' + sLineBreak + GSuspendContext + sLineBreak + sLineBreak +
          'IMPROVEMENT INSTRUCTIONS: ' + UserInput;

        FManager.Run(NewPrompt);
      end;
    end;

  until Approved or GFinished;

  // Esperar resultado final si el agente sigue corriendo
  while not GFinished do
  begin
    CheckSynchronize;
    Sleep(100);
  end;
  CheckSynchronize;

  Writeln;
  Writeln('--- Texto final (pulido por Finalizer) ---');
  Writeln(GFinalOutput);
  Writeln;
end;

procedure RunDemo;
var
  App: TDemoApp;
begin
  Writeln('=== Demo 10 - HumanInTheLoop ===');
  Writeln('Patron: Drafter -> [HUMAN REVIEW] -> Finalizer');
  Writeln('El agente se pausara esperando tu aprobacion.');
  Writeln;

  App := TDemoApp.Create;
  try
    App.Run('The benefits of learning a second language');
    App.Run('Why regular exercise improves mental health');
  finally
    App.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
