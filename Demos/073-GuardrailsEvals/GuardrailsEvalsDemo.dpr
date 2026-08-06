program GuardrailsEvalsDemo;

// =============================================================================
// DEMO 073 - Guardrails de tool calls y framework de evals
// =============================================================================
// Dos componentes que trabajan juntos para poner una IA en produccion con algo
// de red de seguridad, sin necesidad de LLM ni API keys:
//
//   1. TAiGuardrails : politica de tool calls. Se consulta en el choke point de
//                      TAiFunctions.DoCallFunction, de modo que cubre por igual
//                      los tools locales, los de MCP y los de AutoMCP.
//   2. TAiEvalRunner : evals sobre un target generico function(Input): string,
//                      con checks deterministas y reporte para consola o CI.
//
// El demo corre tres bloques:
//   A. Guardrails en aislado  -> allowlist, blocklist con comodin, patron
//                                prohibido en argumentos y veto programatico.
//   B. Guardrails integrado   -> se registra un tool real en TAiFunctions y se
//                                comprueba que el bloqueo IMPIDE su ejecucion
//                                (no basta con avisar: el handler no corre).
//   C. Evals                  -> se evalua un clasificador determinista y se
//                                imprime el reporte; el exit code refleja si
//                                todos los casos pasaron.
//
// Modos de uso:
//   GuardrailsEvalsDemo.exe            -> los tres bloques
//   GuardrailsEvalsDemo.exe --json r.json -> ademas vuelca el reporte de evals
//   GuardrailsEvalsDemo.exe --otel     -> exporta trazas OTLP a localhost:4318
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Math,
  uMakerAi.Guardrails in '..\..\Source\Tools\uMakerAi.Guardrails.pas',
  uMakerAi.Evals in '..\..\Source\Core\uMakerAi.Evals.pas',
  uMakerAi.Tools.Functions,
  uMakerAi.Chat.Messages,
  uMakerAi.Telemetry;

type
  // Los eventos del framework son 'of object': no aceptan lambdas, por eso los
  // handlers viven en una clase.
  TDemoHandlers = class
  public
    ToolEjecutado: Boolean;
    BloqueosVistos: Integer;
    // Tool real registrado en TAiFunctions
    procedure ToolAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: String;
      ToolCall: TAiToolsFunction; var Handled: Boolean);
    // Guardrails
    procedure GuardBlocked(Sender: TObject; const AToolName, AArguments, AReason: string);
    procedure GuardCheck(Sender: TObject; const AToolName, AArguments: string; var AAllow: Boolean; var AReason: string);
  end;

procedure TDemoHandlers.ToolAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: String;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  // Si esto corre, el guardrail NO cumplio su trabajo.
  ToolEjecutado := True;
  ToolCall.Response := 'El tool se ejecuto y borro los archivos';
  Handled := True;
end;

procedure TDemoHandlers.GuardBlocked(Sender: TObject; const AToolName, AArguments, AReason: string);
begin
  Inc(BloqueosVistos);
  Writeln(Format('      [OnBlocked] %-18s %s', [AToolName, AReason]));
end;

procedure TDemoHandlers.GuardCheck(Sender: TObject; const AToolName, AArguments: string;
  var AAllow: Boolean; var AReason: string);
var
  Args: string;
begin
  // OJO: AArguments es el JSON CRUDO que mando el modelo, no un valor ya
  // parseado. En una ruta de Windows las barras vienen escapadas por JSON
  // ("c:\\windows"), asi que comparar contra 'c:\windows' no dispara nunca.
  // Hay que desescaparlas primero. Es el error tipico al escribir un filtro
  // sobre argumentos, y deja el guardrail silenciosamente inutil.
  Args := AArguments.ToLower.Replace('\\', '\');

  // Veto programatico: la ultima palabra la tiene el codigo de la aplicacion,
  // por encima de las listas. Aqui: nada que toque la carpeta de sistema.
  if Args.Contains('c:\windows') then
  begin
    AAllow := False;
    AReason := 'veto programatico: ruta de sistema';
  end;
end;

// -----------------------------------------------------------------------------

procedure Probar(AGuard: TAiGuardrails; const ATool, AArgs: string);
var
  Reason: string;
begin
  if AGuard.CheckToolCall(ATool, AArgs, Reason) then
    Writeln(Format('   PERMITIDO  %-18s %s', [ATool, AArgs]))
  else
    Writeln(Format('   BLOQUEADO  %-18s %s', [ATool, AArgs]));
end;

// -----------------------------------------------------------------------------
// A. Guardrails en aislado
// -----------------------------------------------------------------------------
procedure BloqueGuardrails(AHandlers: TDemoHandlers);
var
  Guard: TAiGuardrails;
begin
  Writeln('');
  Writeln('== A. Guardrails: politica de tool calls ==');
  Writeln('');

  Guard := TAiGuardrails.Create(nil);
  try
    Guard.OnBlocked := AHandlers.GuardBlocked;

    // --- A.1 Blocklist con comodines de mascara ---
    Writeln('   A.1 Blocklist: shell_* y borrar_archivo');
    Guard.BlockedTools.Add('shell_*');
    Guard.BlockedTools.Add('borrar_archivo');
    Probar(Guard, 'consultar_saldo', '{}');
    Probar(Guard, 'shell_exec', '{"cmd":"ls"}');
    Probar(Guard, 'borrar_archivo', '{"path":"a.txt"}');
    Writeln('');

    // --- A.2 Allowlist estricta: si no esta vacia, manda ella ---
    Writeln('   A.2 Allowlist estricta: solo consultar_*');
    Guard.BlockedTools.Clear;
    Guard.AllowedTools.Add('consultar_*');
    Probar(Guard, 'consultar_saldo', '{}');
    Probar(Guard, 'transferir', '{"monto":1000}');
    Writeln('');

    // --- A.3 Patrones prohibidos en los ARGUMENTOS ---
    // El nombre del tool puede ser inocuo y el argumento no serlo.
    Writeln('   A.3 Patron prohibido en argumentos: DROP TABLE');
    Guard.AllowedTools.Clear;
    Guard.BlockedArgPatterns.Add('DROP TABLE');
    Probar(Guard, 'ejecutar_sql', '{"q":"select * from clientes"}');
    Probar(Guard, 'ejecutar_sql', '{"q":"drop table clientes"}'); // case-insensitive
    Writeln('');

    // --- A.4 Veto programatico ---
    Writeln('   A.4 Veto programatico via OnCheckToolCall');
    Guard.BlockedArgPatterns.Clear;
    Guard.OnCheckToolCall := AHandlers.GuardCheck;
    Probar(Guard, 'leer_archivo', '{"path":"d:\\datos\\informe.txt"}');
    Probar(Guard, 'leer_archivo', '{"path":"c:\\windows\\system32\\config"}');
    Writeln('');

    Writeln(Format('   Total bloqueos registrados: %d (BlockedCount=%d)', [AHandlers.BloqueosVistos, Guard.BlockedCount]));
  finally
    Guard.Free;
  end;
end;

// -----------------------------------------------------------------------------
// B. Guardrails integrado en TAiFunctions
// -----------------------------------------------------------------------------
procedure BloqueIntegracion(AHandlers: TDemoHandlers);
var
  Guard: TAiGuardrails;
  Funcs: TAiFunctions;
  Item: TFunctionActionItem;
  ToolCall: TAiToolsFunction;
begin
  Writeln('');
  Writeln('== B. Integracion real: el tool bloqueado NO se ejecuta ==');
  Writeln('');

  Guard := TAiGuardrails.Create(nil);
  Funcs := TAiFunctions.Create(nil);
  ToolCall := TAiToolsFunction.Create;
  try
    // Un tool de verdad, con su handler
    Item := Funcs.Functions.Add;
    Item.FunctionName := 'borrar_todo';
    Item.Enabled := True;
    Item.OnAction := AHandlers.ToolAction;

    Guard.BlockedTools.Add('borrar_todo');
    Funcs.Guardrails := Guard; // opt-in: sin esto no hay politica

    AHandlers.ToolEjecutado := False;
    ToolCall.Name := 'borrar_todo';
    ToolCall.Arguments := '{}';

    // Este es el choke point que usan tools locales, MCP y AutoMCP
    Funcs.DoCallFunction(ToolCall);

    Writeln('   Respuesta devuelta al LLM:');
    Writeln('     ' + ToolCall.Response);
    if AHandlers.ToolEjecutado then
      Writeln('   FALLO: el handler del tool llego a ejecutarse')
    else
      Writeln('   OK: el handler NUNCA se ejecuto, y el LLM recibio el motivo');
  finally
    ToolCall.Free;
    Funcs.Free;
    Guard.Free;
  end;
end;

// -----------------------------------------------------------------------------
// C. Evals
// -----------------------------------------------------------------------------

// El "sistema bajo prueba". En un caso real seria una llamada a un LLM, un
// agente o una cadena RAG; aqui es determinista para que el demo no dependa de
// claves ni de red.
function ClasificarTicket(const AInput: string): string;
var
  S: string;
begin
  S := AInput.ToLower;
  if S.Contains('factura') or S.Contains('cobro') or S.Contains('pago') then
    Result := 'categoria: facturacion'
  else if S.Contains('error') or S.Contains('falla') or S.Contains('no funciona') then
    Result := 'categoria: soporte tecnico'
  else
    Result := 'categoria: general';
end;

function BloqueEvals(const AJsonPath: string): Boolean;
var
  Runner: TAiEvalRunner;
  Report: TAiEvalReport;
  Json: TJSONObject;
begin
  Writeln('');
  Writeln('== C. Evals: TAiEvalRunner sobre un clasificador ==');
  Writeln('');

  Runner := TAiEvalRunner.Create(nil);
  try
    Runner.AddCase('clasifica.facturacion')
      .Input('Me llego una factura duplicada este mes')
      .ExpectContains('facturacion');

    Runner.AddCase('clasifica.soporte')
      .Input('La aplicacion muestra un error al iniciar')
      .ExpectEquals('categoria: soporte tecnico');

    Runner.AddCase('clasifica.general')
      .Input('Queria saber el horario de atencion')
      .ExpectContains('general')
      .ExpectNotContains('soporte');

    Runner.AddCase('clasifica.formato')
      .Input('No funciona el boton de pago')
      .ExpectRegex('^categoria: .+$')
      .ExpectMaxLength(60);

    // Para usar LLM-as-judge basta asignar Runner.Judge := <un TAiChat> y
    // agregar .ExpectJudge('el texto debe...'). Se omite aqui para no exigir
    // una API key.

    Report := Runner.Run(
      function(const AInput: string): string
      begin
        Result := ClasificarTicket(AInput);
      end);
    try
      Writeln(Report.ToText);
      Result := Report.AllPassed;

      if AJsonPath <> '' then
      begin
        Json := Report.ToJSON;
        try
          TFile.WriteAllText(AJsonPath, Json.ToJSON, TEncoding.UTF8);
          Writeln('   Reporte JSON escrito en: ' + AJsonPath);
        finally
          Json.Free;
        end;
      end;
    finally
      Report.Free;
    end;
  finally
    Runner.Free;
  end;
end;

// -----------------------------------------------------------------------------

function ArgValue(const AName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

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
  TodoOk: Boolean;

begin
  Telemetry := nil;
  try
    if HasFlag('--otel') then
    begin
      Telemetry := TAiTelemetry.Create(nil);
      Telemetry.ServiceName := 'GuardrailsEvalsDemo';
      Telemetry.Enabled := True;
      Writeln('[otel] Exportando trazas a ' + Telemetry.Endpoint);
    end;

    Writeln('=== DEMO 073: Guardrails y Evals (sin LLM, sin API keys) ===');

    Handlers := TDemoHandlers.Create;
    try
      BloqueGuardrails(Handlers);
      BloqueIntegracion(Handlers);
      TodoOk := BloqueEvals(ArgValue('--json'));
    finally
      Handlers.Free;
    end;

    Writeln('');
    if TodoOk then
      Writeln('OK: todos los casos de eval pasaron.')
    else
      Writeln('ATENCION: hay casos de eval fallidos.');

    if Assigned(Telemetry) then
    begin
      Telemetry.Flush;
      Telemetry.Free;
    end;

    ExitCode := IfThen(TodoOk, 0, 1);
  except
    on E: Exception do
    begin
      Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 2;
    end;
  end;
end.
