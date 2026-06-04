program HumanApproval;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 07-AgenticLLM / 05-HumanApproval
// =============================================================================
// Human-in-the-loop: el agente se pausa esperando aprobacion humana.
// El usuario puede aprobar el borrador o rechazarlo con feedback,
// lo que relanza el agente con instrucciones de mejora.
//
// Conceptos que cubre:
//   - Manager.Asynchronous = True: ejecucion en hilo separado
//   - Manager.OnSuspend: callback cuando el agente se suspende
//   - Manager.OnFinish: callback cuando termina
//   - Node.Suspend(reason, context): pausar la ejecucion
//   - Manager.ResumeThread(threadId, nodeName, context): reanudar
//   - Manager.Abort: cancelar ejecucion actual
//   - Blackboard para pasar datos entre OnExecute y el loop principal
//
// Estructura: [Drafter] -> [HumanReviewer (suspend)] -> [Finalizer]
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

// Estado global del ciclo suspend/resume
var
  GSuspendThreadID : String  = '';
  GSuspendNodeName : String  = '';
  GSuspendContext  : String  = '';
  GSuspended       : Boolean = False;
  GFinished        : Boolean = False;
  GFinalOutput     : String  = '';

// =============================================================================
//  TApprovalApp
// =============================================================================
type
  TApprovalApp = class
  private
    FManager   : TAIAgentManager;
    FDrafter   : TLLMNode;
    FReviewer  : TAIAgentsNode;
    FFinalizer : TLLMNode;
    FLink1, FLink2: TAIAgentsLink;

    procedure OnSuspend(Sender: TObject; const AThreadID, ANodeName,
      AReason, AContext: String);
    procedure OnFinish(Sender: TObject; const AInput, AOutput: String;
      Status: TAgentExecutionStatus; E: Exception);
    procedure ReviewerExec(Node, BeforeNode: TAIAgentsNode;
      Link: TAIAgentsLink; Input: String; var Output: String);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Run(const ATask: String);
  end;

procedure TApprovalApp.OnSuspend(Sender: TObject;
  const AThreadID, ANodeName, AReason, AContext: String);
begin
  TThread.Queue(nil, procedure
  begin
    GSuspendThreadID := AThreadID;
    GSuspendNodeName := ANodeName;
    GSuspendContext  := AContext;
    GSuspended       := True;
  end);
end;

procedure TApprovalApp.OnFinish(Sender: TObject; const AInput, AOutput: String;
  Status: TAgentExecutionStatus; E: Exception);
begin
  if Status = esSuspended then Exit;
  TThread.Queue(nil, procedure
  begin
    GFinalOutput := AOutput;
    GFinished    := True;
  end);
end;

procedure TApprovalApp.ReviewerExec(Node, BeforeNode: TAIAgentsNode;
  Link: TAIAgentsLink; Input: String; var Output: String);
var
  ReviewResult: String;
begin
  ReviewResult := Node.Graph.Blackboard.GetString('review_result');
  if ReviewResult = '' then
  begin
    // Primera llamada: suspender para revision humana
    Node.Graph.Blackboard.SetString('current_draft', Input);
    Node.Suspend('Human review required', Input);
    // Output ignorado; el agente no avanza mientras FSuspended=True
  end
  else
  begin
    // Reanudacion: usuario aprobo
    Output := ReviewResult;
    Node.Graph.Blackboard.SetString('review_result', '');
  end;
end;

constructor TApprovalApp.Create;
begin
  inherited;

  FManager := TAIAgentManager.Create(nil);
  FManager.Asynchronous := True;
  FManager.TimeoutMs    := 300000;
  FManager.OnSuspend    := OnSuspend;
  FManager.OnFinish     := OnFinish;

  // Nodo 1: Redactor
  FDrafter := TLLMNode.Create(FManager);
  FDrafter.Name         := 'Drafter';
  FDrafter.Graph        := FManager;
  FDrafter.DriverName   := DRIVER;
  FDrafter.Model        := MODEL;
  FDrafter.ApiKey       := API_KEY;
  FDrafter.UseAllTools  := False;
  FDrafter.SystemPrompt :=
    'Eres un escritor profesional. Escribe un parrafo claro y atractivo ' +
    '(80-120 palabras) sobre el tema dado. Si recibes instrucciones de ' +
    'mejora, incorpóralas completamente. Responde en espanol.';

  // Nodo 2: Revisor humano (suspende para aprobacion)
  FReviewer := TAIAgentsNode.Create(FManager);
  FReviewer.Name      := 'HumanReviewer';
  FReviewer.Graph     := FManager;
  FReviewer.OnExecute := ReviewerExec;

  // Nodo 3: Finalizador
  FFinalizer := TLLMNode.Create(FManager);
  FFinalizer.Name         := 'Finalizer';
  FFinalizer.Graph        := FManager;
  FFinalizer.DriverName   := DRIVER;
  FFinalizer.Model        := MODEL;
  FFinalizer.ApiKey       := API_KEY;
  FFinalizer.UseAllTools  := False;
  FFinalizer.SystemPrompt :=
    'Eres un editor. Recibes un borrador aprobado. ' +
    'Pulelo: mejora el flujo, corrige frases torpes, optimiza la eleccion ' +
    'de palabras. Mantén el mismo significado y longitud. ' +
    'Devuelve solo el texto pulido en espanol.';

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

destructor TApprovalApp.Destroy;
begin
  FManager.Free;
  inherited;
end;

procedure TApprovalApp.Run(const ATask: String);
var
  UserInput : String;
  Approved  : Boolean;
  Iteration : Integer;
  NewPrompt : String;
begin
  Writeln(StringOfChar('=', 60));
  Writeln('Tarea: ', ATask);
  Writeln(StringOfChar('=', 60));

  GSuspended   := False;
  GFinished    := False;
  GFinalOutput := '';
  Iteration    := 0;
  Approved     := False;

  // Lanzar el agente (retorna inmediatamente por Asynchronous=True)
  FManager.Run(ATask);

  repeat
    Inc(Iteration);

    // Esperar hasta suspension o fin
    while (not GSuspended) and (not GFinished) do
    begin
      CheckSynchronize;
      Sleep(100);
    end;

    if GFinished then Break;

    if GSuspended then
    begin
      GSuspended := False;

      Writeln;
      Writeln(Format('--- Borrador (iteracion %d) ---', [Iteration]));
      Writeln(GSuspendContext);
      Writeln;
      Write('Apruebas este borrador? (s=si / escribe feedback para mejorar): ');
      Readln(UserInput);
      Writeln;

      if (Trim(LowerCase(UserInput)) = 's') or
         (Trim(LowerCase(UserInput)) = 'si') or
         (Trim(LowerCase(UserInput)) = 'y') then
      begin
        Approved := True;
        FManager.Blackboard.SetString('review_result', GSuspendContext);
        FManager.ResumeThread(GSuspendThreadID, GSuspendNodeName, GSuspendContext);
      end
      else
      begin
        // Rechazado: relanzar con feedback
        if Trim(UserInput) = '' then
          UserInput := 'Por favor mejora la calidad del texto.';

        Writeln('Regenerando con tu feedback...');
        FManager.Abort;
        Sleep(200);

        GSuspended   := False;
        GFinished    := False;
        GFinalOutput := '';

        NewPrompt := ATask + sLineBreak + sLineBreak +
          'BORRADOR PREVIO (necesita mejora):' + sLineBreak + GSuspendContext + sLineBreak + sLineBreak +
          'INSTRUCCIONES DE MEJORA: ' + UserInput;

        FManager.Run(NewPrompt);
      end;
    end;

  until Approved or GFinished;

  // Esperar resultado final
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
  App: TApprovalApp;
begin
  Writeln('=== HumanApproval ===');
  Writeln('Patron: Drafter -> [REVISION HUMANA] -> Finalizer');
  Writeln('El agente se pausara esperando tu aprobacion.');
  Writeln;

  App := TApprovalApp.Create;
  try
    App.Run('Los beneficios de aprender a programar desde nino');
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
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
