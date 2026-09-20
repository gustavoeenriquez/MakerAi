unit uMakerAi.Tools.ComputerUse.Linux;

// =============================================================================
//  Executor de Computer Use para Linux (X11).
//
//  - Input (raton / teclado / scroll) delegado en xdotool.
//  - Captura de pantalla delegada en scrot.
//
//  Por que por shell y no con libX11/XTest directamente: el ejecutor son dos
//  handlers (OnExecuteAction / OnRequestScreenshot), asi que cambiar de motor
//  despues no toca nada mas del framework. Esta version arranca funcionando con
//  dos paquetes de apt; una variante sobre libX11+XTest por dlopen (sin
//  binarios externos, patron de TOpenSSLTransport) puede sustituirla sin que
//  la aplicacion se entere.
//
//  DEPENDENCIAS EN LA MAQUINA DESTINO:
//    sudo apt-get install -y xdotool scrot
//    # y, si no hay pantalla fisica (VPS headless):
//    sudo apt-get install -y xvfb
//    Xvfb :99 -screen 0 1280x800x24 &
//    export DISPLAY=:99
//
//  DISPLAY: por defecto se hereda del proceso. Si la aplicacion corre como
//  servicio y no lo tiene en el entorno, asignar TAiLinuxExecutor.Display.
//
//  LIMITACIONES CONOCIDAS:
//   * Solo X11. Bajo Wayland nativo xdotool no controla las ventanas; hace
//     falta XWayland (y aun asi solo alcanza a los clientes X).
//   * catNavigate usa xdg-open, asi que necesita un navegador instalado y un
//     manejador registrado. En un Xvfb pelado no hay ninguno.
//   * Sin window manager (el caso tipico de Xvfb) no hay foco por clic en el
//     marco ni activacion de ventanas: las apps reciben el foco por puntero.
//     Si hace falta comportamiento de escritorio, levantar un WM ligero.
//
//  La interfaz publica (Execute / CaptureScreen) es identica a la de los
//  executors de Windows y macOS, para que el handler sea intercambiable por
//  plataforma sin tocar la aplicacion.
// =============================================================================

interface

{$IFDEF LINUX}

uses
  System.SysUtils, System.Classes, System.Types, System.Math, System.StrUtils,
  System.IOUtils,
  uMakerAi.Tools.ComputerUse, uMakerAi.Core;

type
  TAiLinuxExecutor = class
  private
    class var FDisplay: string;
    // Ejecuta un comando y devuelve el codigo de salida.
    class function Sh(const ACmd: string): Integer;
    // Ejecuta un comando y devuelve su salida estandar.
    class function ShOut(const ACmd: string): string;
    // Envuelve el comando con DISPLAY=... si se fijo explicitamente.
    class function WithDisplay(const ACmd: string): string;
    // Escapa un argumento para el shell (comillas simples).
    class function Q(const S: string): string;
    // Traduce un nombre de tecla al keysym que entiende xdotool.
    class function MapKey(const AName: string): string;
    // 'CTRL+A' -> 'ctrl+a'; tambien vale para una sola tecla.
    class function MapCombo(const ACombo: string): string;
    // Botones de rueda de X11: 4 arriba, 5 abajo, 6 izquierda, 7 derecha.
    class function ScrollButton(const ADirection: string): Integer;
  public
    // Si se deja vacio, se usa el DISPLAY del entorno del proceso.
    class property Display: string read FDisplay write FDisplay;

    class function Execute(const Action: TAiActionData): TAiActionResult;
    class procedure CaptureScreen(var MediaFile: TAiMediaFile; TargetArea: TRect;
      Quality: Integer = 70);

    // True si xdotool y scrot estan disponibles. Conviene llamarlo al arrancar
    // para dar un mensaje claro en vez de fallar accion por accion.
    class function ToolsAvailable(out AMissing: string): Boolean;
  end;

{$ENDIF}

implementation

{$IFDEF LINUX}

uses
  Posix.Stdlib;

{ TAiLinuxExecutor }

class function TAiLinuxExecutor.Sh(const ACmd: string): Integer;
var
  M: TMarshaller;
begin
  Result := _system(M.AsAnsi(ACmd).ToPointer);
end;

class function TAiLinuxExecutor.ShOut(const ACmd: string): string;
var
  LTmp: string;
begin
  Result := '';
  LTmp := TPath.GetTempFileName;
  try
    Sh(ACmd + ' > ' + Q(LTmp) + ' 2>/dev/null');
    if TFile.Exists(LTmp) then
      Result := TFile.ReadAllText(LTmp);
  finally
    try
      TFile.Delete(LTmp);
    except
    end;
  end;
end;

class function TAiLinuxExecutor.WithDisplay(const ACmd: string): string;
begin
  if FDisplay <> '' then
    Result := 'DISPLAY=' + Q(FDisplay) + ' ' + ACmd
  else
    Result := ACmd;
end;

class function TAiLinuxExecutor.Q(const S: string): string;
begin
  // Comillas simples: dentro no se interpreta nada. Una comilla simple del
  // texto se cierra, se escapa y se reabre ('\'' es el idioma habitual).
  Result := '''' + StringReplace(S, '''', '''\''''', [rfReplaceAll]) + '''';
end;

class function TAiLinuxExecutor.MapKey(const AName: string): string;
var
  LK: string;
  LFn: Integer;
begin
  LK := LowerCase(Trim(AName));

  if (LK = 'ctrl') or (LK = 'control') or (LK = 'ctl') then Exit('ctrl');
  if (LK = 'alt') or (LK = 'option') then Exit('alt');
  if LK = 'shift' then Exit('shift');
  if (LK = 'cmd') or (LK = 'command') or (LK = 'win') or (LK = 'super') or (LK = 'meta') then Exit('super');

  if (LK = 'enter') or (LK = 'return') then Exit('Return');
  if (LK = 'esc') or (LK = 'escape') then Exit('Escape');
  if LK = 'tab' then Exit('Tab');
  if (LK = 'backspace') or (LK = 'back') then Exit('BackSpace');
  if (LK = 'delete') or (LK = 'del') then Exit('Delete');
  if LK = 'insert' then Exit('Insert');
  if LK = 'space' then Exit('space');
  if LK = 'up' then Exit('Up');
  if LK = 'down' then Exit('Down');
  if LK = 'left' then Exit('Left');
  if LK = 'right' then Exit('Right');
  if LK = 'home' then Exit('Home');
  if LK = 'end' then Exit('End');
  if (LK = 'pageup') or (LK = 'page_up') or (LK = 'pgup') then Exit('Page_Up');
  if (LK = 'pagedown') or (LK = 'page_down') or (LK = 'pgdn') then Exit('Page_Down');

  // F1..F24
  if (Length(LK) >= 2) and (LK[1] = 'f') and
     TryStrToInt(Copy(LK, 2, MaxInt), LFn) and (LFn >= 1) and (LFn <= 24) then
    Exit('F' + IntToStr(LFn));

  Result := LK;
end;

class function TAiLinuxExecutor.MapCombo(const ACombo: string): string;
var
  LParts: TArray<string>;
  I: Integer;
begin
  Result := '';
  LParts := SplitString(ACombo, '+');
  for I := 0 to High(LParts) do
  begin
    if Trim(LParts[I]) = '' then
      Continue;
    if Result <> '' then
      Result := Result + '+';
    Result := Result + MapKey(LParts[I]);
  end;
end;

class function TAiLinuxExecutor.ScrollButton(const ADirection: string): Integer;
var
  LD: string;
begin
  LD := LowerCase(Trim(ADirection));
  if LD = 'up' then Result := 4
  else if LD = 'left' then Result := 6
  else if LD = 'right' then Result := 7
  else Result := 5; // down por defecto
end;

class function TAiLinuxExecutor.ToolsAvailable(out AMissing: string): Boolean;
begin
  AMissing := '';
  if Sh('command -v xdotool >/dev/null 2>&1') <> 0 then
    AMissing := 'xdotool';
  if Sh('command -v scrot >/dev/null 2>&1') <> 0 then
  begin
    if AMissing <> '' then
      AMissing := AMissing + ' y ';
    AMissing := AMissing + 'scrot';
  end;
  Result := AMissing = '';
end;

class function TAiLinuxExecutor.Execute(const Action: TAiActionData): TAiActionResult;
var
  LCmd, LMods, LKey: string;
  LBtn, LClicks, I: Integer;
  LModList: TArray<string>;
begin
  Result.Success := True;
  Result.ErrorMessage := '';
  Result.CustomOutput := '';

  try
    // Modificadores (click / scroll con shift, ctrl...): se mantienen pulsados
    // alrededor de la accion, igual que en el executor de Windows.
    LMods := '';
    if Trim(Action.Modifiers) <> '' then
    begin
      LModList := SplitString(Action.Modifiers, '+');
      for I := 0 to High(LModList) do
        if Trim(LModList[I]) <> '' then
        begin
          if LMods <> '' then
            LMods := LMods + ' ';
          LMods := LMods + MapKey(LModList[I]);
        end;
    end;

    if LMods <> '' then
      Sh(WithDisplay('xdotool keydown ' + LMods));
    try
      case Action.ActionType of

        catClick, catRightClick, catMiddleClick, catDoubleClick, catTripleClick:
          begin
            case Action.ActionType of
              catRightClick:  LBtn := 3;
              catMiddleClick: LBtn := 2;
            else
              LBtn := 1;
            end;
            case Action.ActionType of
              catDoubleClick: LClicks := 2;
              catTripleClick: LClicks := 3;
            else
              LClicks := 1;
            end;
            LCmd := Format('xdotool mousemove %d %d click --repeat %d --delay 60 %d',
              [Action.X, Action.Y, LClicks, LBtn]);
            Result.Success := Sh(WithDisplay(LCmd)) = 0;
          end;

        catHover:
          Result.Success := Sh(WithDisplay(Format('xdotool mousemove %d %d',
            [Action.X, Action.Y]))) = 0;

        catType:
          begin
            // Pre-click SOLO si viene posicion (Gemini 'type_text_at' la trae;
            // el 'type' de Claude no, y clicar en (0,0) quitaria el foco).
            // Identico al executor de Windows, ClearBeforeTyping incluido: alli
            // tampoco se aplica, para no divergir entre plataformas.
            if (Action.X <> 0) or (Action.Y <> 0) then
            begin
              Sh(WithDisplay(Format('xdotool mousemove %d %d click 1',
                [Action.X, Action.Y])));
              Sleep(80);
            end;

            if Action.TextToType <> '' then
              Result.Success := Sh(WithDisplay('xdotool type --delay 12 -- ' +
                Q(Action.TextToType))) = 0;

            if Action.PressEnter then
            begin
              Sleep(50);
              Sh(WithDisplay('xdotool key Return'));
            end;
          end;

        catKeyCombination:
          begin
            LKey := MapCombo(Action.KeyCombo);
            if LKey = '' then
            begin
              Result.Success := False;
              Result.ErrorMessage := 'key_combination sin teclas';
            end
            else
              Result.Success := Sh(WithDisplay('xdotool key ' + Q(LKey))) = 0;
          end;

        catHoldKey:
          begin
            LKey := MapCombo(Action.KeyCombo);
            if LKey = '' then
            begin
              Result.Success := False;
              Result.ErrorMessage := 'hold_key sin teclas';
            end
            else
            begin
              Sh(WithDisplay('xdotool keydown ' + Q(LKey)));
              Sleep(Round(Max(0.1, Action.HoldDuration) * 1000));
              Result.Success := Sh(WithDisplay('xdotool keyup ' + Q(LKey))) = 0;
            end;
          end;

        catScroll:
          begin
            LBtn := ScrollButton(Action.ScrollDirection);
            // ScrollAmount viene en pixeles (default 800). X11 solo sabe de
            // "clics" de rueda, asi que se aproxima ~100 px por clic.
            LClicks := EnsureRange(Round(Max(1, Action.ScrollAmount) / 100), 1, 15);
            if (Action.X <> 0) or (Action.Y <> 0) then
              Sh(WithDisplay(Format('xdotool mousemove %d %d', [Action.X, Action.Y])));
            Result.Success := Sh(WithDisplay(Format('xdotool click --repeat %d --delay 30 %d',
              [LClicks, LBtn]))) = 0;
          end;

        catDrag:
          begin
            LCmd := Format('xdotool mousemove %d %d mousedown 1 mousemove %d %d',
              [Action.X, Action.Y, Action.DestX, Action.DestY]);
            Sh(WithDisplay(LCmd));
            Sleep(120); // dar tiempo a que la app procese el arrastre
            Result.Success := Sh(WithDisplay('xdotool mouseup 1')) = 0;
          end;

        catCursorPosition:
          begin
            // Devuelve 'X=123 Y=456 SCREEN=0 WINDOW=...'
            Result.CustomOutput := Trim(ShOut(WithDisplay('xdotool getmouselocation --shell')));
            Result.Success := Result.CustomOutput <> '';
            if not Result.Success then
              Result.ErrorMessage := 'no se pudo leer la posicion del cursor';
          end;

        catNavigate:
          begin
            if Trim(Action.Url) = '' then
            begin
              Result.Success := False;
              Result.ErrorMessage := 'navigate sin url';
            end
            else
            begin
              Result.Success := Sh(WithDisplay('xdg-open ' + Q(Action.Url) +
                ' >/dev/null 2>&1')) = 0;
              if not Result.Success then
                Result.ErrorMessage :=
                  'xdg-open fallo: no hay navegador instalado o no hay manejador registrado';
              Sleep(1500); // margen para que el navegador pinte algo
            end;
          end;

        catGoBack:
          Result.Success := Sh(WithDisplay('xdotool key alt+Left')) = 0;

        catGoForward:
          Result.Success := Sh(WithDisplay('xdotool key alt+Right')) = 0;

        catWait:
          Sleep(5000);

        // No mueven nada: la captura la hace despues OnRequestScreenshot, y el
        // recorte de zoom lo lee de CurrentAction.ZoomRect.
        catScreenshot, catZoom, catTerminate:
          Result.Success := True;

      else
        Result.Success := False;
        Result.ErrorMessage := 'Accion no soportada por TAiLinuxExecutor: ' +
          Action.FunctionName;
      end;
    finally
      if LMods <> '' then
        Sh(WithDisplay('xdotool keyup ' + LMods));
    end;
  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := E.Message;
    end;
  end;
end;

class procedure TAiLinuxExecutor.CaptureScreen(var MediaFile: TAiMediaFile;
  TargetArea: TRect; Quality: Integer);
var
  LTmp, LCmd: string;
  LW, LH: Integer;
begin
  LW := TargetArea.Width;
  LH := TargetArea.Height;

  LTmp := TPath.Combine(TPath.GetTempPath,
    'makerai_shot_' + FormatDateTime('hhnnsszzz', Now) + '.png');
  try
    if (LW > 0) and (LH > 0) then
      LCmd := Format('scrot -o -a %d,%d,%d,%d %s',
        [TargetArea.Left, TargetArea.Top, LW, LH, Q(LTmp)])
    else
      LCmd := 'scrot -o ' + Q(LTmp);

    if (Sh(WithDisplay(LCmd)) <> 0) or (not TFile.Exists(LTmp)) then
      raise Exception.Create('scrot no pudo capturar la pantalla (revise DISPLAY y que scrot este instalado)');

    if not Assigned(MediaFile) then
      MediaFile := TAiMediaFile.Create;
    MediaFile.LoadFromfile(LTmp);
  finally
    try
      if TFile.Exists(LTmp) then
        TFile.Delete(LTmp);
    except
    end;
  end;
end;

{$ENDIF}

end.
