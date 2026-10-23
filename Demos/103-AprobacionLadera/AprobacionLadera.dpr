program AprobacionLadera;

// =============================================================================
// DEMO 103 - El humano en el ciclo, con checkpoints
// =============================================================================
// Un pedido grande no se despacha sin que lo apruebe el encargado. El grafo se
// detiene en el nodo Aprobar, guarda su estado en disco y el programa termina:
// nada queda esperando en memoria. Horas después (otra corrida del .exe) llega
// la respuesta del encargado y el grafo sigue donde quedó.
//
//   Cotizar (código) -> Aprobar (TAiWaitApprovalTool) -> Decidir (código)
//                                                     -> Redactar (TLLMNode)
//
//   - TAiWaitApprovalTool : suspende el grafo y dispara OnSuspend con la pregunta
//                           y el contexto (la cotización del blackboard).
//   - TAiFileCheckpointer : guarda el hilo suspendido en checkpoints\.
//   - ResumeThread        : reanuda el hilo con la respuesta humana como Input.
//
// Uso:
//   AprobacionLadera.exe                       cotiza y queda esperando aprobación
//   AprobacionLadera.exe --responder "texto"   el encargado responde; el grafo sigue
//
// Requiere OPENAI_API_KEY para el Redactor. Empresa, clientes y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  uMakerAi.Agents,
  uMakerAi.Agents.Checkpoint,
  uMakerAi.Agents.Tools.Approval,
  uMakerAi.Agents.Node.LLM,
  uMakerAi.Chat.Initializations;   // drivers para el TLLMNode

const
  NODO_APROBACION = 'Aprobar';
  TOPE_SIN_APROBACION = 1500000;   // COP: por encima, decide el encargado
  PEDIDO = 'Hotel Casa Verde: 25 kilos de Sierra Nevada Orgánico';
  PRECIO_KILO = 92000;
  KILOS = 25;
  DESCUENTO = 15;                  // % desde 25 kg

type
  TVenta = class
  public
    Suspendido: Boolean;
    procedure Cotizar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Decidir(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Suspende(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
    procedure SaleNodo(Sender: TObject; Node: TAIAgentsNode);
  end;

function Pesos(AValor: Int64): string;
begin
  Result := FormatFloat('#,##0', AValor).Replace(',', '.') + ' COP';
end;

procedure TVenta.Cotizar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Bruto, Total: Int64;
begin
  Bruto := Int64(KILOS) * PRECIO_KILO;
  Total := Bruto * (100 - DESCUENTO) div 100;
  Output := Format('%s. Subtotal %s, descuento %d %%, total %s.',
    [Input, Pesos(Bruto), DESCUENTO, Pesos(Total)]);
  Node.Graph.Blackboard.SetString('cotizacion', Output);
  Writeln('  [Cotizar] ', Output);
  if Total > TOPE_SIN_APROBACION then
    Writeln('  [Cotizar] supera ', Pesos(TOPE_SIN_APROBACION), ': necesita aprobación del encargado');
end;

// La respuesta humana llega como Input: se arma el pedido de redacción
procedure TVenta.Decidir(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Writeln('  [Decidir] respuesta del encargado: ', Input.Trim);
  Output := 'Cotización: ' + Node.Graph.Blackboard.GetString('cotizacion') + sLineBreak +
    'Respuesta del encargado: ' + Input.Trim;
end;

procedure TVenta.Suspende(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
begin
  Suspendido := True;
  Writeln('  [', ANodeName, '] ', AReason);
  Writeln('  El grafo se detiene y guarda su estado. Hilo: ', AThreadID);
end;

procedure TVenta.SaleNodo(Sender: TObject; Node: TAIAgentsNode);
begin
  if Node is TLLMNode then
    Writeln('  [', Node.Name, '] ', AdjustLineBreaks(Node.Output.Trim).Replace(sLineBreak, sLineBreak + '  '));
end;

function CarpetaCheckpoints: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'checkpoints');
end;

function ValorDe(const AName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

procedure Esperar(AGrafo: TAIAgentManager);
begin
  while AGrafo.Busy do
    CheckSynchronize(50);
  CheckSynchronize;
end;

var
  Venta: TVenta;
  Grafo: TAIAgentManager;
  Aprobacion: TAiWaitApprovalTool;
  Redactar: TLLMNode;
  Pendientes: TArray<string>;
  Respuesta: string;
var
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes.
  // Con un buffer de 1 byte cada caracter sale al escribirlo.
  SetTextBuf(Output, BufSalida, 1);
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('Falta la variable OPENAI_API_KEY.');
    ExitCode := 1;
    Exit;
  end;

  Venta := TVenta.Create;
  Grafo := TAIAgentManager.Create(nil);
  try
    try
      // El mismo grafo en las dos corridas: el checkpoint guarda el estado, no el código
      Grafo.Name := 'VentaMayorista';
      Grafo.AddNode('Cotizar', Venta.Cotizar)
        .AddNode(NODO_APROBACION, nil)
        .AddNode('Decidir', Venta.Decidir);

      // Aprobar: suspende el grafo y deja la cotización como contexto de la pregunta
      Aprobacion := TAiWaitApprovalTool.Create(Grafo.FindNode(NODO_APROBACION));
      Aprobacion.SuspendReason := 'Pedido mayor a ' + Pesos(TOPE_SIN_APROBACION) + ': ¿lo apruebas?';
      Aprobacion.ContextKey := 'cotizacion';
      Grafo.FindNode(NODO_APROBACION).Tool := Aprobacion;

      Redactar := TLLMNode.Create(Grafo);
      Redactar.Name := 'Redactar';
      Redactar.DriverName := 'OpenAi';
      Redactar.ApiKey := '@OPENAI_API_KEY';
      Redactar.SystemPrompt :=
        'Eres Lucía, de Café La Ladera. Con la cotización y la respuesta del encargado, escribe el ' +
        'mensaje para el cliente: si aprobó, confirma el pedido con el total y lo que diga el encargado; ' +
        'si no, explícalo con amabilidad. Máximo 3 frases, texto plano, sin inventar datos.';
      Grafo.AddNode(Redactar)
        .AddEdge('Cotizar', NODO_APROBACION)
        .AddEdge(NODO_APROBACION, 'Decidir')
        .AddEdge('Decidir', 'Redactar')
        .SetEntryPoint('Cotizar')
        .SetFinishPoint('Redactar');

      // La suspensión va a disco: sobrevive al cierre del programa
      ForceDirectories(CarpetaCheckpoints);
      Grafo.Checkpointer := TAiFileCheckpointer.Create(CarpetaCheckpoints);
      Grafo.OnSuspend := Venta.Suspende;
      Grafo.OnExitNode := Venta.SaleNodo;

      Respuesta := ValorDe('--responder');
      Pendientes := Grafo.GetActiveThreads;
      if Respuesta = '' then
      begin
        Writeln('Pedido: ', PEDIDO);
        Grafo.Run(PEDIDO, nil);
        Esperar(Grafo);
        if Venta.Suspendido then
          Writeln('  Checkpoint en checkpoints\. El programa termina; nada queda esperando en memoria.');
      end
      else if Length(Pendientes) = 0 then
        Writeln('No hay pedidos esperando aprobación.')
      else
      begin
        Writeln('Hilo recuperado del checkpoint: ', Pendientes[0]);
        if not Grafo.ResumeThread(Pendientes[0], NODO_APROBACION, Respuesta) then
          raise Exception.Create('No se pudo reanudar el hilo ' + Pendientes[0]);
        Esperar(Grafo);
        Writeln('  Pedidos esperando aprobación: ', Length(Grafo.GetActiveThreads));
      end;
      ExitCode := 0;
    except
      on E: Exception do
      begin
        Writeln('ERROR: ', E.ClassName, ': ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    Grafo.Free;
    Venta.Free;
  end;
end.
