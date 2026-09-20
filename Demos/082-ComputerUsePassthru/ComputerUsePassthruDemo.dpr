program ComputerUsePassthruDemo;

{
  Demo 082 - Computer Use: quien ejecuta la accion.

  Un computer_call se puede resolver de tres maneras, y este demo las ejerce
  todas contra el API real, sin tocar la pantalla:

    1. DELEGADA  - OnCallToolFunction llena ToolCall.Response, asi que el
                   driver NO ejecuta nada en local. Es lo que hace un broker
                   headless que reenvia la llamada a su cliente remoto.
    2. SIN TOOL  - cap_ComputerUse esta activo pero falta
                   ChatTools.ComputerUseTool: configuracion incompleta, el
                   turno se corta con LastError en vez de mandar un output
                   invalido.
    3. LOCAL     - hay ejecutor: el driver ejecuta, captura, responde un
                   computer_call_output y recurre (bucle agentico normal).

  Cada caso se corre en modo sincrono y en streaming, porque el driver de
  OpenAI tiene DOS implementaciones separadas del computer_call.

  El ejecutor de los casos locales es simulado: devuelve exito y un PNG de
  1x1 como "screenshot". Asi se valida el round-trip completo sin mover el
  raton ni mandarle tu escritorio al proveedor (el demo 066 si hace eso).

  Salida: exit code 0 si todo lo que se pudo verificar paso; 1 si algo fallo.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.JSON,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.ComputerUse,
  uMakerAi.ParamsRegistry,
  uMakerAi.Chat.Initializations;

const
  // PNG 1x1 valido. Hace de screenshot sin tocar la pantalla real.
  CPng1x1 = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8' +
            'BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==';

  CDefaultModel = 'gpt-6-astra';
  CDefaultPrompt = 'Toma el control de la pantalla: haz una captura y dime ' +
                   'brevemente que ves.';

type
  // Que se espera del turno, segun quien deba ejecutar la accion.
  TCaseKind = (ckDelegated, ckMissingTool, ckLocal);

  TSpy = class
  public
    Calls: Integer; // veces que se disparo OnCallToolFunction
    Execs: Integer; // veces que se disparo OnExecuteAction
    Shots: Integer; // veces que se disparo OnRequestScreenshot
    Names: string;
    Done: Boolean;
    Intercept: Boolean;
    // Si es 1, delega desde la primera accion. Con un valor mayor deja que el
    // bucle agentico haga N-1 rondas locales antes de cortarlo.
    DelegateFrom: Integer;
    procedure ToolHandler(Sender: TObject; AiToolCall: TAiToolsFunction);
    procedure DataEnd(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSonObject; aRole, aText: String);
    procedure ExecuteAction(Sender: TObject; const ActionData: TAiActionData;
      var Result: TAiActionResult);
    procedure RequestShot(Sender: TObject; var MediaFile: TAiMediaFile);
  end;

var
  GModel: string = CDefaultModel;
  GPrompt: string = CDefaultPrompt;
  GFailures: Integer = 0;
  GSkips: Integer = 0;
  GPasses: Integer = 0;

procedure TSpy.ToolHandler(Sender: TObject; AiToolCall: TAiToolsFunction);
begin
  Inc(Calls);
  if Names <> '' then
    Names := Names + ', ';
  Names := Names + AiToolCall.Name;

  // El gate del framework: si el handler deja Response lleno, el driver da la
  // accion por ejecutada en otro sitio y no la toca. Mismo contrato en Claude,
  // en Gemini y en el bridge generico de TAiChat.
  if Intercept and (Calls >= DelegateFrom) then
    AiToolCall.Response := '{"output":"delegated_to_remote_client"}';
end;

procedure TSpy.DataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: String);
begin
  Done := True;
end;

procedure TSpy.ExecuteAction(Sender: TObject; const ActionData: TAiActionData;
  var Result: TAiActionResult);
begin
  Inc(Execs);
  Result.Success := True; // ejecutor simulado: nada se mueve de verdad
end;

procedure TSpy.RequestShot(Sender: TObject; var MediaFile: TAiMediaFile);
begin
  Inc(Shots);
  MediaFile := TAiMediaFile.Create;
  MediaFile.LoadFromBase64('shot.png', CPng1x1);
end;

function Kind(const S: string): string;
begin
  if ContainsText(S, '"type":"computer_call_output"') then
    Result := 'computer_call_output'
  else if ContainsText(S, '"type":"computer_call"') then
    Result := 'computer_call'
  else
    Result := 'texto';
end;

procedure RunCase(AKind: TCaseKind; AAsync: Boolean);
var
  Chat: TAiOpenChat;
  CU: TAiComputerUseTool;
  Spy: TSpy;
  Res, Title, Why: string;
  I, Waited, NOut, NCall: Integer;
  HasTool, WithExecutor, Ok: Boolean;
begin
  case AKind of
    ckDelegated:
      Title := 'DELEGADA (broker headless)';
    ckMissingTool:
      Title := 'SIN ComputerUseTool asignado';
  else
    Title := 'LOCAL (ejecutor simulado)';
  end;
  Title := Title + IfThen(AAsync, ' - streaming', ' - sincrono');

  HasTool := AKind <> ckMissingTool;
  WithExecutor := AKind = ckLocal;

  WriteLn('');
  WriteLn('==============================================================');
  WriteLn('CASO: ', Title);
  WriteLn('==============================================================');

  CU := nil;
  Spy := TSpy.Create;
  Chat := TAiOpenChat.Create(nil);
  try
    Spy.Intercept := AKind <> ckMissingTool;
    // En el caso local se dejan dos rondas reales antes de cortar el bucle.
    // (IfThen entero vive en System.Math y chocaria con el de System.StrUtils.)
    if AKind = ckLocal then
      Spy.DelegateFrom := 3
    else
      Spy.DelegateFrom := 1;

    Chat.ApiKey := '@OPENAI_API_KEY';
    Chat.Model := GModel;
    // Computer Use es OPT-IN deliberado: sin cap_ComputerUse el tool 'computer'
    // ni siquiera se declara. La alternativa documentada es el registry, antes
    // de crear el componente:
    //   TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-6-astra',
    //     'ModelCaps', '[cap_Image, cap_Reasoning, cap_ComputerUse]');
    Chat.ModelCaps := [cap_Image, cap_Reasoning, cap_ComputerUse];
    Chat.SessionCaps := [cap_Image, cap_Reasoning, cap_ComputerUse];
    Chat.Asynchronous := AAsync; // True = streaming
    Chat.OnCallToolFunction := Spy.ToolHandler;
    Chat.OnReceiveDataEnd := Spy.DataEnd;

    if HasTool then
    begin
      CU := TAiComputerUseTool.Create(nil);
      if WithExecutor then
      begin
        CU.OnExecuteAction := Spy.ExecuteAction;
        CU.OnRequestScreenshot := Spy.RequestShot;
      end;
      Chat.ChatTools.ComputerUseTool := CU;
    end;

    try
      Res := Chat.AddMessageAndRun(GPrompt, 'user', []);

      if AAsync then
      begin
        Waited := 0;
        while (not Spy.Done) and (Waited < 180000) do
        begin
          Sleep(200);
          Inc(Waited, 200);
        end;
        Sleep(3000); // gracia: el ultimo tramo del stream corre en su hilo
      end;

      if Res.Trim <> '' then
        WriteLn('Respuesta: ', Copy(Res.Trim, 1, 160));
    except
      on E: Exception do
      begin
        WriteLn('EXCEPCION: ', E.ClassName, ': ', E.Message);
        Inc(GFailures);
        Exit;
      end;
    end;

    if Chat.LastError <> '' then
      WriteLn('LastError: ', Copy(Chat.LastError, 1, 220));

    WriteLn(Format('Eventos: OnCallToolFunction=%d (%s)  OnExecuteAction=%d  OnRequestScreenshot=%d',
      [Spy.Calls, Spy.Names, Spy.Execs, Spy.Shots]));

    NOut := 0;
    NCall := 0;
    WriteLn('Historial (', Chat.Messages.Count, ' mensajes):');
    for I := 0 to Chat.Messages.Count - 1 do
    begin
      WriteLn(Format('  [%d] role=%-9s len=%-6d %s', [I, Chat.Messages[I].Role,
        Length(Chat.Messages[I].Prompt), Kind(Chat.Messages[I].Prompt)]));
      if Kind(Chat.Messages[I].Prompt) = 'computer_call_output' then
        Inc(NOut);
      if Kind(Chat.Messages[I].Prompt) = 'computer_call' then
        Inc(NCall);
    end;
    WriteLn('Resumen: computer_call=', NCall, '  computer_call_output=', NOut);

    if NCall = 0 then
    begin
      // El modelo decide si usa la herramienta. Si no la uso no hay nada que
      // verificar; no es un fallo del framework.
      WriteLn('SKIP: el modelo no uso el tool computer en este turno.');
      Inc(GSkips);
      Exit;
    end;

    Ok := True;
    Why := '';
    case AKind of
      ckDelegated:
        begin
          if NOut <> 0 then
          begin
            Ok := False;
            Why := 'se emitio computer_call_output pese a la delegacion';
          end
          else if (Spy.Execs <> 0) or (Spy.Shots <> 0) then
          begin
            Ok := False;
            Why := 'se ejecuto en local pese a la delegacion';
          end;
        end;
      ckMissingTool:
        begin
          if NOut <> 0 then
          begin
            Ok := False;
            Why := 'se emitio un computer_call_output sin imagen (400 seguro)';
          end
          else if Chat.LastError = '' then
          begin
            Ok := False;
            Why := 'el turno se corto sin avisar por LastError';
          end;
        end;
      ckLocal:
        begin
          if NOut = 0 then
          begin
            Ok := False;
            Why := 'no se emitio ningun computer_call_output';
          end
          else if (Spy.Execs = 0) or (Spy.Shots = 0) then
          begin
            Ok := False;
            Why := 'no se llamo al ejecutor local';
          end;
        end;
    end;

    if Ok then
    begin
      WriteLn('PASS');
      Inc(GPasses);
    end
    else
    begin
      WriteLn('FAIL: ', Why);
      Inc(GFailures);
    end;
  finally
    Chat.Free;
    if Assigned(CU) then
      CU.Free;
    Spy.Free;
  end;
end;

procedure Usage;
begin
  WriteLn('Demo 082 - Computer Use: quien ejecuta la accion');
  WriteLn('');
  WriteLn('  ComputerUsePassthruDemo.exe [opciones]');
  WriteLn('');
  WriteLn('  --case=all|delegated|missing|local   (default: all)');
  WriteLn('  --mode=both|sync|stream              (default: both)');
  WriteLn('  --model=<id>                         (default: ', CDefaultModel, ')');
  WriteLn('  --prompt=<texto>');
  WriteLn('  --help');
  WriteLn('');
  WriteLn('Requiere la variable de entorno OPENAI_API_KEY.');
  WriteLn('Hace llamadas REALES al API. No toca la pantalla.');
end;

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
  S: string;
begin
  Result := ADefault;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if S.StartsWith(AName + '=') then
      Exit(S.Substring(Length(AName) + 1));
  end;
end;

function HasArg(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
      Exit(True);
end;

var
  LCase, LMode: string;
  LDoSync, LDoStream: Boolean;

begin
  try
    if HasArg('--help') or HasArg('-h') then
    begin
      Usage;
      Halt(0);
    end;

    if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
    begin
      WriteLn('ERROR: falta la variable de entorno OPENAI_API_KEY.');
      Halt(2);
    end;

    LCase := LowerCase(ArgValue('--case', 'all'));
    LMode := LowerCase(ArgValue('--mode', 'both'));
    GModel := ArgValue('--model', CDefaultModel);
    GPrompt := ArgValue('--prompt', CDefaultPrompt);

    LDoSync := (LMode = 'both') or (LMode = 'sync');
    LDoStream := (LMode = 'both') or (LMode = 'stream');

    WriteLn('Modelo: ', GModel);

    if (LCase = 'all') or (LCase = 'delegated') then
    begin
      if LDoSync then
        RunCase(ckDelegated, False);
      if LDoStream then
        RunCase(ckDelegated, True);
    end;

    if (LCase = 'all') or (LCase = 'missing') then
    begin
      if LDoSync then
        RunCase(ckMissingTool, False);
      if LDoStream then
        RunCase(ckMissingTool, True);
    end;

    if (LCase = 'all') or (LCase = 'local') then
    begin
      if LDoSync then
        RunCase(ckLocal, False);
      if LDoStream then
        RunCase(ckLocal, True);
    end;

    WriteLn('');
    WriteLn('==============================================================');
    WriteLn(Format('RESULTADO: %d PASS, %d FAIL, %d SKIP',
      [GPasses, GFailures, GSkips]));
    WriteLn('==============================================================');

    if GFailures > 0 then
      Halt(1);
  except
    on E: Exception do
    begin
      WriteLn('FATAL: ', E.ClassName, ': ', E.Message);
      Halt(3);
    end;
  end;

end.
