program DespachoLadera;

{
  Capitulo 35 - El modelo que ve la pantalla (Computer Use en una pantalla virtual).

  La transportadora de Cafe La Ladera tiene un programa de escritorio viejo, "Andina
  Despachos", sin API: la guia de cada envio se registra a mano. Aqui lo registra Lucia:
  el modelo pide una captura, hace clic en cada campo, escribe, pulsa "Registrar guia" y
  lee el numero que aparece. Nada de esto toca el escritorio de nadie: corre sobre una
  pantalla X virtual (Xvfb) en Linux, que es como corre un agente en un servidor.

  Las piezas: TAiComputerUseTool (traduce lo que pide el modelo a acciones canonicas y le
  devuelve capturas) y TAiLinuxExecutor (xdotool + scrot), conectado por los dos eventos
  del tool. En Windows el ejecutor seria TAiWindowsFMXExecutor con el mismo contrato,
  pero ahi el raton y la pantalla son los del usuario (ver el capitulo).

    sudo apt-get install -y xvfb xdotool scrot python3-tk
    pantalla/iniciar.sh                    # Xvfb :99 a 1280x800 + Andina Despachos
    DISPLAY=:99 ./DespachoLadera           # --proveedor=openai|claude  --tarea="..."
    pantalla/detener.sh

  Cada captura que se le manda al modelo queda en shots/ (para ver lo que vio).
  Cafe La Ladera: empresa, productos y precios ficticios.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.IOUtils,
  System.StrUtils,
  System.Diagnostics,
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
  TAREA_DEFAULT =
    'Eres Lucia, la asistente de Cafe La Ladera. En la pantalla esta abierto "Andina Despachos", ' +
    'el programa de escritorio de la transportadora; no tiene API, asi que la guia se registra a mano. ' +
    'Registra la guia de este pedido: Destinatario: Marta Rios. Ciudad: Cali. Direccion: Calle 5 No. 38-20. ' +
    'Peso (kg): 0.7. Contenido: 2 bolsas Huila Reserva 340 g. ' +
    'Toma una captura, haz clic en cada campo y escribe su valor (los campos empiezan vacios), ' +
    'pulsa el boton "Registrar guia", toma otra captura y lee el numero de guia que aparece. ' +
    'Responde solo con: "Guia registrada: <numero>".';

type
  TOperador = class
  public
    CarpetaCapturas: string;
    Capturas, Acciones: Integer;
    procedure EjecutarAccion(Sender: TObject; const ActionData: TAiActionData; var Result: TAiActionResult);
    procedure PedirCaptura(Sender: TObject; var MediaFile: TAiMediaFile);
    procedure LlamadaCruda(Sender: TObject; AiToolCall: TAiToolsFunction);
  end;

procedure Log(const S: string);
begin
  WriteLn(FormatDateTime('hh:nn:ss  ', Now), S);
end;

procedure TOperador.EjecutarAccion(Sender: TObject; const ActionData: TAiActionData; var Result: TAiActionResult);
begin
  // El tool ya tradujo la peticion del modelo (pixeles de la captura, nombres de cada proveedor)
  // al formato canonico: aqui solo se ejecuta. Un ejecutor son estos dos eventos.
  Inc(Acciones);
  Log(Format('accion %-16s x=%d y=%d%s%s%s', [ActionData.FunctionName, ActionData.X, ActionData.Y,
    IfThen(ActionData.TextToType <> '', '  texto="' + Copy(ActionData.TextToType, 1, 40) + '"'),
    IfThen(ActionData.KeyCombo <> '', '  teclas=' + ActionData.KeyCombo),
    IfThen(ActionData.ActionType = catScroll, '  scroll=' + ActionData.ScrollDirection)]));
  Result := TAiLinuxExecutor.Execute(ActionData);
  if not Result.Success then
    Log('   -> fallo: ' + Result.ErrorMessage);
end;

procedure TOperador.PedirCaptura(Sender: TObject; var MediaFile: TAiMediaFile);
var
  Tool: TAiComputerUseTool;
begin
  Tool := Sender as TAiComputerUseTool;
  TAiLinuxExecutor.CaptureScreen(MediaFile, TRect.Create(Tool.AreaLeft, Tool.AreaTop,
    Tool.AreaLeft + Tool.AreaWidth, Tool.AreaTop + Tool.AreaHeight));
  if Assigned(MediaFile) then
  begin
    Inc(Capturas);
    try
      MediaFile.SaveToFile(TPath.Combine(CarpetaCapturas, Format('captura_%.2d.png', [Capturas])));
    except
    end;
    Log(Format('   -> captura %d (%d KB)', [Capturas, MediaFile.Bytes div 1024]));
  end;
end;

procedure TOperador.LlamadaCruda(Sender: TObject; AiToolCall: TAiToolsFunction);
begin
  // Solo diagnostico: la llamada TAL CUAL llega del proveedor, antes de la traduccion. Si una
  // accion cae donde no debia, lo primero es mirar esto. No se toca Response (delegaria la accion).
  Log(Format('   raw %s %s', [AiToolCall.Name, Copy(AiToolCall.Arguments, 1, 160)]));
end;

function Arg(const ANombre, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount do
    if ParamStr(I).StartsWith(ANombre + '=') then
      Exit(ParamStr(I).Substring(Length(ANombre) + 1));
end;

var
  Conn: TAiChatConnection;
  Tool: TAiComputerUseTool;
  Op: TOperador;
  Proveedor, Tarea, Display, Falta, R: string;
  Ancho, Alto: Integer;
  Reloj: TStopwatch;

begin
  try
    if FindCmdLineSwitch('help') or FindCmdLineSwitch('h') then
    begin
      WriteLn('DespachoLadera  --proveedor=openai|claude  --display=:99  --ancho=1280 --alto=800  --tarea="..."');
      Halt(0);
    end;
    Proveedor := LowerCase(Arg('--proveedor', 'openai'));
    Tarea := Arg('--tarea', TAREA_DEFAULT);
    Display := Arg('--display', '');
    Ancho := StrToIntDef(Arg('--ancho', '1280'), 1280);
    Alto := StrToIntDef(Arg('--alto', '800'), 800);
    if Display <> '' then
      TAiLinuxExecutor.Display := Display;
    if not TAiLinuxExecutor.ToolsAvailable(Falta) then
    begin
      WriteLn('Falta ', Falta, ': sudo apt-get install -y xdotool scrot');
      Halt(2);
    end;

    Op := TOperador.Create;
    Tool := TAiComputerUseTool.Create(nil);
    Conn := TAiChatConnection.Create(nil);
    try
      Op.CarpetaCapturas := TPath.Combine(ExtractFilePath(ParamStr(0)), 'shots');
      ForceDirectories(Op.CarpetaCapturas);

      // La pantalla X mide 1280x800 y eso mismo se le declara al modelo: la captura va sin
      // reescalar y los clics caen donde el modelo cree. (En un monitor de 3440 px se captura
      // en fisico y se declara 1280: el tool convierte de vuelta.)
      Tool.AreaLeft := 0;
      Tool.AreaTop := 0;
      Tool.AreaWidth := Ancho;
      Tool.AreaHeight := Alto;
      Tool.ScreenWidth := Ancho;
      Tool.ScreenHeight := Alto;
      Tool.OnExecuteAction := Op.EjecutarAccion;
      Tool.OnRequestScreenshot := Op.PedirCaptura;
      Conn.ChatTools.ComputerUseTool := Tool;
      Conn.OnCallToolFunction := Op.LlamadaCruda;

      // Computer Use NO viene activo en el catalogo (le da al modelo el raton y el teclado):
      // se enciende a proposito, con el conjunto completo de capacidades en los dos lados.
      if Proveedor = 'claude' then
      begin
        Conn.DriverName := 'Claude';
        Conn.Model := 'claude-sonnet-5-5';
        Conn.Params.Values['ApiKey'] := '@CLAUDE_API_KEY';
        Conn.ModelCaps := [cap_Image, cap_Reasoning, cap_ComputerUse];
        Conn.SessionCaps := Conn.ModelCaps;
      end
      else
      begin
        Conn.DriverName := 'OpenAi';
        Conn.Model := 'gpt-6-astra';
        Conn.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
        Conn.ModelCaps := [cap_Image, cap_Reasoning, cap_ComputerUse];
        Conn.SessionCaps := Conn.ModelCaps;
      end;
      Conn.Params.Values['Asynchronous'] := 'False';
      Conn.AiChat.ResponseTimeOut := 180000;

      Log(Format('%s / %s  pantalla X %dx%d (%s)', [Conn.DriverName, Conn.Model, Ancho, Alto,
        IfThen(Display <> '', Display, GetEnvironmentVariable('DISPLAY'))]));
      Log('--- Lucia toma el control de Andina Despachos ---');
      Reloj := TStopwatch.StartNew;
      R := Conn.AddMessageAndRun(Tarea, 'user', []);
      Log('--- fin ---');
      if Conn.LastError <> '' then
        Log('LastError: ' + Conn.LastError);
      // Los contadores de la conexion son disjuntos: Prompt_tokens es lo nuevo y Cached_tokens lo leido
      // de cache (en OpenAI casi todo el historial de capturas va por cache)
      Log(Format('%d acciones, %d capturas, %.1f s, %d tokens de entrada (%d de caché) / %d de salida', [Op.Acciones, Op.Capturas,
        Reloj.ElapsedMilliseconds / 1000, Conn.Prompt_tokens + Conn.AiChat.Cached_tokens, Conn.AiChat.Cached_tokens, Conn.Completion_tokens]));
      WriteLn;
      WriteLn('Lucia: ', R);
      if Op.Acciones = 0 then
        Halt(3);
    finally
      Conn.Free;
      Tool.Free;
      Op.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.

{$ELSE}

begin
  WriteLn('DespachoLadera corre en Linux (xdotool + scrot sobre X11). Compila en Windows para no romper el grupo.');
end.

{$ENDIF}
