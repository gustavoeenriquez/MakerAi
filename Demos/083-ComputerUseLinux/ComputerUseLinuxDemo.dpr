program ComputerUseLinuxDemo;

{
  Demo 083 - Computer Use en Linux (X11).

  Ejecuta un ciclo agentico real de Computer Use sobre una pantalla X: el modelo
  pide capturas, hace clic y teclea, y el demo le devuelve el nuevo estado.

  Es el mismo subsistema que el demo 066 de Windows; lo unico que cambia son los
  dos handlers del TAiComputerUseTool, que aqui apuntan a TAiLinuxExecutor
  (xdotool + scrot) en vez de al executor Win32.

  PENSADO PARA UNA PANTALLA VIRTUAL (Xvfb). Asi es como corre un broker en un
  VPS headless, y ademas evita el riesgo del 066: alli la captura es el
  escritorio REAL del usuario y se envia entero al proveedor. Aqui solo se ve lo
  que haya dentro del display virtual.

    sudo apt-get install -y xvfb xdotool scrot x11-apps
    Xvfb :99 -screen 0 1280x800x24 &
    DISPLAY=:99 xedit /tmp/prueba.txt &
    DISPLAY=:99 ./ComputerUseLinuxDemo

  Si se apunta a un display con pantalla fisica (:0), se captura el escritorio
  entero y se envia al proveedor. Revisar que hay en pantalla ANTES.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.IOUtils,
  System.StrUtils,
  System.JSON,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.ComputerUse,
{$IFDEF LINUX}
  uMakerAi.Tools.ComputerUse.Linux,
{$ENDIF}
  uMakerAi.ParamsRegistry,
  uMakerAi.Chat.Initializations;

{$IFDEF LINUX}

const
  CDefaultPrompt =
    'Eres un agente de automatizacion de escritorio. ' +
    'Tarea: toma un screenshot, localiza el area de texto del editor que hay en ' +
    'pantalla, haz click en ella y escribe exactamente: ' +
    '"Hola MakerAI desde Linux". Luego toma otro screenshot para confirmar y ' +
    'describe brevemente lo que ves.';

type
  THandlers = class
  public
    ShotDir: string;
    ShotCount: Integer;
    Actions: Integer;
    procedure ExecuteAction(Sender: TObject; const ActionData: TAiActionData;
      var Result: TAiActionResult);
    procedure RequestShot(Sender: TObject; var MediaFile: TAiMediaFile);
  end;

procedure Log(const S: string);
begin
  WriteLn(FormatDateTime('hh:nn:ss  ', Now), S);
end;

procedure THandlers.ExecuteAction(Sender: TObject; const ActionData: TAiActionData;
  var Result: TAiActionResult);
begin
  Inc(Actions);
  Log(Format('ACCION %-18s x=%d y=%d combo="%s" texto="%s"',
    [ActionData.FunctionName, ActionData.X, ActionData.Y, ActionData.KeyCombo,
     Copy(ActionData.TextToType, 1, 40)]));

  Result := TAiLinuxExecutor.Execute(ActionData);

  if not Result.Success then
    Log('   -> FALLO: ' + Result.ErrorMessage)
  else if Result.CustomOutput <> '' then
    Log('   -> ' + StringReplace(Result.CustomOutput, sLineBreak, ' ', [rfReplaceAll]));
end;

procedure THandlers.RequestShot(Sender: TObject; var MediaFile: TAiMediaFile);
var
  LTool: TAiComputerUseTool;
  LArea: TRect;
begin
  LTool := Sender as TAiComputerUseTool;
  LArea := TRect.Create(LTool.AreaLeft, LTool.AreaTop,
    LTool.AreaLeft + LTool.AreaWidth, LTool.AreaTop + LTool.AreaHeight);

  TAiLinuxExecutor.CaptureScreen(MediaFile, LArea);

  if Assigned(MediaFile) then
  begin
    Inc(ShotCount);
    // Copia de depuracion: permite ver exactamente lo que se le mando al modelo.
    try
      MediaFile.SaveToFile(TPath.Combine(ShotDir,
        Format('shot_%.3d.png', [ShotCount])));
    except
    end;
    Log(Format('   -> screenshot %d (%d bytes)', [ShotCount, MediaFile.bytes]));
  end;
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
  LConn: TAiChatConnection;
  LTool: TAiComputerUseTool;
  LH: THandlers;
  LProvider, LPrompt, LDisplay, LMissing, LRes: string;
  LW, LH2: Integer;

begin
  try
    if HasArg('--help') or HasArg('-h') then
    begin
      WriteLn('Demo 083 - Computer Use en Linux');
      WriteLn('  --provider=openai|claude   (default: openai)');
      WriteLn('  --display=:99              (default: el DISPLAY del entorno)');
      WriteLn('  --width=1280 --height=800  (tamano de la pantalla X)');
      WriteLn('  --prompt=<texto>');
      WriteLn('');
      WriteLn('Requiere OPENAI_API_KEY o CLAUDE_API_KEY, y xdotool + scrot.');
      Halt(0);
    end;

    LProvider := LowerCase(ArgValue('--provider', 'openai'));
    LPrompt := ArgValue('--prompt', CDefaultPrompt);
    LDisplay := ArgValue('--display', '');
    LW := StrToIntDef(ArgValue('--width', '1280'), 1280);
    LH2 := StrToIntDef(ArgValue('--height', '800'), 800);

    if LDisplay <> '' then
      TAiLinuxExecutor.Display := LDisplay;

    if not TAiLinuxExecutor.ToolsAvailable(LMissing) then
    begin
      WriteLn('ERROR: falta ', LMissing, '. Instalar con:');
      WriteLn('  sudo apt-get install -y xdotool scrot');
      Halt(2);
    end;

    LH := THandlers.Create;
    LTool := TAiComputerUseTool.Create(nil);
    LConn := TAiChatConnection.Create(nil);
    try
      LH.ShotDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'shots');
      ForceDirectories(LH.ShotDir);

      // Area capturada = pantalla X completa. Como es <= 1280 de ancho, la
      // resolucion DECLARADA al modelo coincide con la imagen y no hay que
      // reescalar: los clics caen donde el modelo cree.
      LTool.AreaLeft := 0;
      LTool.AreaTop := 0;
      LTool.AreaWidth := LW;
      LTool.AreaHeight := LH2;
      LTool.ScreenWidth := LW;
      LTool.ScreenHeight := LH2;
      LTool.OnExecuteAction := LH.ExecuteAction;
      LTool.OnRequestScreenshot := LH.RequestShot;

      LConn.ChatTools.ComputerUseTool := LTool;

      if LProvider = 'claude' then
      begin
        LConn.DriverName := 'Claude';
        LConn.Model := 'claude-opus-4-8';
        LConn.Params.Values['ApiKey'] := '@CLAUDE_API_KEY';
        TAiChatFactory.Instance.RegisterUserParam('Claude', 'claude-opus-4-8',
          'ModelCaps', '[cap_Image, cap_ComputerUse]');
        TAiChatFactory.Instance.RegisterUserParam('Claude', 'claude-opus-4-8',
          'SessionCaps', '[cap_Image, cap_ComputerUse]');
      end
      else
      begin
        LConn.DriverName := 'OpenAi';
        LConn.Model := 'gpt-6-astra';
        LConn.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
        TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-6-astra',
          'ModelCaps', '[cap_Image, cap_Reasoning, cap_ComputerUse]');
        TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-6-astra',
          'SessionCaps', '[cap_Image, cap_Reasoning, cap_ComputerUse]');
      end;

      LConn.Params.Values['Asynchronous'] := 'False';

      Log(Format('Proveedor: %s  Modelo: %s', [LConn.DriverName, LConn.Model]));
      Log(Format('Pantalla X: %dx%d  DISPLAY=%s', [LW, LH2,
        IfThen(LDisplay <> '', LDisplay, GetEnvironmentVariable('DISPLAY'))]));
      Log('Screenshots en: ' + LH.ShotDir);
      Log('--- inicio ---');

      LRes := LConn.AddMessageAndRun(LPrompt, 'user', []);

      Log('--- fin ---');
      if LConn.LastError <> '' then
        Log('LastError: ' + LConn.LastError);
      Log(Format('Acciones ejecutadas: %d   Screenshots: %d',
        [LH.Actions, LH.ShotCount]));
      WriteLn;
      WriteLn('Respuesta del modelo:');
      WriteLn(LRes);

      if (LH.Actions = 0) then
      begin
        WriteLn;
        WriteLn('AVISO: el modelo no uso la herramienta en este turno.');
        Halt(3);
      end;
    finally
      LConn.Free;
      LTool.Free;
      LH.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('FATAL: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.

{$ELSE}

begin
  WriteLn('Demo 083 solo corre en Linux: usa xdotool y scrot sobre X11.');
  WriteLn('Compila en Windows a proposito, para no romper el group project.');
end.

{$ENDIF}
