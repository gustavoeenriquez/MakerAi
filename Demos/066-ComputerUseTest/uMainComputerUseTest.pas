unit uMainComputerUseTest;

// =============================================================================
//  Demo de prueba de Computer Use (Claude computer_20251124 / Gemini computerUse).
//
//  Cómo usar:
//   1. Abre una app objetivo inofensiva (ej. Notepad) y déjala visible.
//   2. Elige proveedor (Claude / Gemini) y pulsa "Ejecutar".
//   3. El modelo tomará screenshots y controlará el ratón/teclado REAL para
//      completar la tarea descrita en el prompt.
//
//  El ejecutor físico es TAiWindowsFMXExecutor (SendInput + captura GDI/FMX).
//  La captura corre en el hilo principal (Synchronize) porque usa FMX.Canvas.
//
//  AVISO: controla el escritorio real. No dejes ventanas sensibles en primer plano.
// =============================================================================

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.JSON, System.Net.HttpClient, System.Math,
  {$IFDEF MSWINDOWS} Winapi.Windows, FMX.Platform.Win, {$ENDIF}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Surfaces, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Memo.Types, FMX.ListBox, FMX.Layouts, FMX.Edit,
  uMakerAi.Core, uMakerAi.Chat.Messages, uMakerAi.Chat.AiConnection,
  uMakerAi.Tools.ComputerUse, uMakerAi.Tools.ComputerUse.WindowsFMX,
  uMakerAi.Utils.ScreenCapture, uMakerAi.ParamsRegistry,
  uMakerAi.Chat.Initializations; // registra todos los drivers (Claude, Gemini, ...)

type
  TFormComputerUse = class(TForm)
    tbTop: TToolBar;
    lblProvider: TLabel;
    cbProvider: TComboBox;
    btnRun: TButton;
    lblStatus: TLabel;
    mePrompt: TMemo;
    meLog: TMemo;
    splV: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    FConn: TAiChatConnection;
    FCU: TAiComputerUseTool;
    FShotDir: string;   // carpeta donde se guardan los screenshots para depurar
    FShotCount: Integer;
    procedure Log(const S: string);
    procedure SetStatus(const S: string);
    // Captura el área física y la devuelve como JPG escalado a OutW x OutH.
    function CaptureScaled(const Area: TRect; OutW, OutH: Integer): TAiMediaFile;
    // Eventos de Computer Use
    procedure DoExecuteAction(Sender: TObject; const ActionData: TAiActionData; var Result: TAiActionResult);
    procedure DoRequestScreenshot(Sender: TObject; var MediaFile: TAiMediaFile);
    procedure DoSafetyConfirmation(Sender: TObject; const Explanation: string; var Allow: Boolean);
    // Eventos de la conexión
    procedure DoReceiveEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: String);
    procedure DoError(Sender: TObject; const ErrorMsg: string; E: Exception; const AResponse: IHTTPResponse);
  end;

var
  FormComputerUse: TFormComputerUse;

implementation

{$R *.fmx}

const
  DEF_PROMPT =
    'Eres un agente de automatización de escritorio.' + sLineBreak +
    'Tarea: toma un screenshot, localiza el área de texto de la ventana en primer plano ' +
    '(p.ej. el Bloc de notas), haz click en ella y escribe exactamente: ' +
    '"Hola MakerAI desde Computer Use". Luego toma otro screenshot para confirmar y ' +
    'describe brevemente lo que ves.';

procedure TFormComputerUse.FormCreate(Sender: TObject);
begin
  // Forzar la ventana al centro de la pantalla principal (evita que aparezca
  // fuera del área visible: se ve en la barra de tareas pero no en pantalla).
  Self.Position := TFormPosition.ScreenCenter;
  Self.Width := 900;
  Self.Height := 680;

  // Componente de Computer Use
  FCU := TAiComputerUseTool.Create(Self);
  FCU.OnExecuteAction := DoExecuteAction;
  FCU.OnRequestScreenshot := DoRequestScreenshot;
  FCU.OnSafetyConfirmation := DoSafetyConfirmation;
  FCU.EnableZoom := True; // permite la acción zoom de Claude (computer_20251124)

  // Área de captura = monitor primario físico (origen 0,0).
  FCU.AreaLeft := 0;
  FCU.AreaTop := 0;
  {$IFDEF MSWINDOWS}
  FCU.AreaWidth := GetSystemMetrics(SM_CXSCREEN);
  FCU.AreaHeight := GetSystemMetrics(SM_CYSCREEN);
  {$ELSE}
  FCU.AreaWidth := 1920;
  FCU.AreaHeight := 1080;
  {$ENDIF}
  // Resolución DECLARADA a la IA: la imagen se envía ya reducida a <=1280px de
  // ancho. Anthropic/Gemini reducen imágenes grandes en el servidor; si la
  // resolución declarada no coincide con la imagen real, los clicks se desvían.
  // El executor sigue clicando en píxeles físicos (DenormalizeCoordinate usa AreaWidth).
  FCU.ScreenWidth := 1280;
  FCU.ScreenHeight := Round(FCU.AreaHeight * 1280 / FCU.AreaWidth);

  // Conexión universal
  FConn := TAiChatConnection.Create(Self);
  FConn.ComputerUseTool := FCU;
  FConn.OnReceiveDataEnd := DoReceiveEnd;
  FConn.OnError := DoError;

  // Habilitar cap_ComputerUse en los modelos que vamos a usar
  // (claude-opus-4-8 trae [cap_Image] por defecto; gemini-2.5-computer-use ya lo trae).
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'claude-opus-4-8', 'ModelCaps',   '[cap_Image, cap_ComputerUse]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'claude-opus-4-8', 'SessionCaps', '[cap_Image, cap_ComputerUse]');

  cbProvider.Items.Clear;
  cbProvider.Items.Add('Claude');
  cbProvider.Items.Add('Gemini');
  cbProvider.ItemIndex := 0;

  // Carpeta de depuración: cada screenshot enviado a la IA se guarda aquí.
  FShotDir := ExtractFilePath(ParamStr(0)) + 'shots' + PathDelim;
  ForceDirectories(FShotDir);
  FShotCount := 0;

  mePrompt.Lines.Text := DEF_PROMPT;
  SetStatus('Listo. Abre la app objetivo y pulsa Ejecutar.');
  Log(Format('Pantalla: %dx%d', [FCU.ScreenWidth, FCU.ScreenHeight]));
  Log('Screenshots guardados en: ' + FShotDir);
end;

procedure TFormComputerUse.FormShow(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  H: HWND;
  WA: TRect;
  W, Ht, X, Y: Integer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  // Reposiciona la ventana real (HWND) dentro del área de trabajo del monitor
  // principal y la trae al frente. Soluciona el caso "se ve en la barra de
  // tareas pero no en pantalla" (ventana fuera del área visible).
  H := FormToHWND(Self);
  if H = 0 then
    Exit;
  W := 900;
  Ht := 680;
  if not SystemParametersInfo(SPI_GETWORKAREA, 0, @WA, 0) then
    WA := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
  X := WA.Left + ((WA.Width  - W)  div 2);
  Y := WA.Top  + ((WA.Height - Ht) div 2);
  if X < WA.Left then X := WA.Left;
  if Y < WA.Top  then Y := WA.Top;
  SetWindowPos(H, HWND_TOP, X, Y, W, Ht, SWP_SHOWWINDOW);
  ShowWindow(H, SW_SHOW);
  SetForegroundWindow(H);
  {$ENDIF}
end;

procedure TFormComputerUse.Log(const S: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      meLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + S);
      meLog.GoToTextEnd;
    end);
end;

procedure TFormComputerUse.SetStatus(const S: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      lblStatus.Text := S;
    end);
end;

// -----------------------------------------------------------------------------
// Computer Use
// -----------------------------------------------------------------------------
procedure TFormComputerUse.DoExecuteAction(Sender: TObject; const ActionData: TAiActionData; var Result: TAiActionResult);
begin
  Log(Format('ACCIÓN: %s  (x=%d y=%d mod="%s" keys="%s")',
    [ActionData.FunctionName, ActionData.X, ActionData.Y, ActionData.Modifiers, ActionData.KeyCombo]));
  Result := TAiWindowsFMXExecutor.Execute(ActionData);
  if not Result.Success then
    Log('  -> ERROR ejecutando: ' + Result.ErrorMessage)
  else if ActionData.ActionType = catCursorPosition then
    Log('  -> cursor: ' + Result.CustomOutput);
end;

function TFormComputerUse.CaptureScaled(const Area: TRect; OutW, OutH: Integer): TAiMediaFile;
var
  Full, Scaled: FMX.Graphics.TBitmap;
  Surf: TBitmapSurface;
  SP: TBitmapCodecSaveParams;
  Strm: TMemoryStream;
  {$IFDEF MSWINDOWS} Cur: TPoint; {$ENDIF}
  cx, cy: Single;
  R: TRectF;
begin
  Result := TAiMediaFile.Create;
  if (OutW <= 0) or (OutH <= 0) then Exit;
  Full := TScreenCapture.CaptureArea(Area);
  if not Assigned(Full) then Exit;
  try
    Scaled := FMX.Graphics.TBitmap.Create(OutW, OutH);
    try
      if Scaled.Canvas.BeginScene then
      try
        Scaled.Canvas.DrawBitmap(Full, RectF(0, 0, Full.Width, Full.Height),
          RectF(0, 0, OutW, OutH), 1, False);
        // Cursor (escalado al tamaño de salida) para que la IA vea el puntero.
        {$IFDEF MSWINDOWS}
        if GetCursorPos(Cur) then
        begin
          cx := (Cur.X - Area.Left) * OutW / Max(1, Area.Width);
          cy := (Cur.Y - Area.Top)  * OutH / Max(1, Area.Height);
          if (cx >= 0) and (cy >= 0) and (cx < OutW) and (cy < OutH) then
          begin
            R := RectF(cx - 7, cy - 7, cx + 7, cy + 7);
            Scaled.Canvas.Fill.Color := $80FF0000;
            Scaled.Canvas.FillEllipse(R, 1);
            Scaled.Canvas.Stroke.Color := TAlphaColors.White;
            Scaled.Canvas.Stroke.Thickness := 2;
            Scaled.Canvas.DrawEllipse(R, 1);
          end;
        end;
        {$ENDIF}
      finally
        Scaled.Canvas.EndScene;
      end;

      Surf := TBitmapSurface.Create;
      try
        Surf.Assign(Scaled);
        Strm := TMemoryStream.Create;
        try
          SP.Quality := 80;
          if TBitmapCodecManager.SaveToStream(Strm, Surf, '.jpg', @SP) then
          begin
            // Copia a disco (exactamente lo que se envía a la IA, con el cursor).
            if FShotDir <> '' then
            begin
              Inc(FShotCount);
              try
                Strm.Position := 0;
                Strm.SaveToFile(FShotDir + Format('shot_%.3d_%dx%d.jpg', [FShotCount, OutW, OutH]));
              except
                // no bloquear la captura por un fallo de escritura a disco
              end;
            end;
            Strm.Position := 0;
            Result.LoadFromStream('screenshot.jpg', Strm);
          end;
        finally
          Strm.Free;
        end;
      finally
        Surf.Free;
      end;
    finally
      Scaled.Free;
    end;
  finally
    Full.Free;
  end;
end;

procedure TFormComputerUse.DoRequestScreenshot(Sender: TObject; var MediaFile: TAiMediaFile);
var
  Area: TRect;
  OutW, OutH: Integer;
  MF: TAiMediaFile;
begin
  if (FCU.CurrentAction.ActionType = catZoom) and not FCU.CurrentAction.ZoomRect.IsEmpty then
  begin
    // Zoom: enviar la región a resolución nativa (la IA inspecciona detalle).
    Area := FCU.CurrentAction.ZoomRect;
    OutW := Area.Width;
    OutH := Area.Height;
  end
  else
  begin
    // Pantalla completa: capturar el área física, enviar reducida a la
    // resolución declarada (ScreenWidth x ScreenHeight) para que coincida.
    Area := Rect(FCU.AreaLeft, FCU.AreaTop, FCU.AreaLeft + FCU.AreaWidth, FCU.AreaTop + FCU.AreaHeight);
    OutW := FCU.ScreenWidth;
    OutH := FCU.ScreenHeight;
  end;

  MF := nil;
  // FMX.Canvas -> debe correr en el hilo principal.
  TThread.Synchronize(nil,
    procedure
    begin
      MF := CaptureScaled(Area, OutW, OutH);
    end);
  MediaFile := MF;
  Log(Format('  -> screenshot %dx%d (origen %dx%d)', [OutW, OutH, Area.Width, Area.Height]));
end;

procedure TFormComputerUse.DoSafetyConfirmation(Sender: TObject; const Explanation: string; var Allow: Boolean);
begin
  // En esta prueba con el usuario presente, auto-permitimos pero lo registramos.
  Log('SAFETY (auto-permitido): ' + Explanation);
  Allow := True;
end;

// -----------------------------------------------------------------------------
// Conexión
// -----------------------------------------------------------------------------
procedure TFormComputerUse.DoReceiveEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: String);
begin
  Log('=== FIN ===');
  if aText <> '' then
    Log('Modelo: ' + aText);
  SetStatus('Completado.');
  TThread.Queue(nil, procedure begin btnRun.Enabled := True; end);
end;

procedure TFormComputerUse.DoError(Sender: TObject; const ErrorMsg: string; E: Exception; const AResponse: IHTTPResponse);
begin
  Log('ERROR: ' + ErrorMsg);
  SetStatus('Error.');
  TThread.Queue(nil, procedure begin btnRun.Enabled := True; end);
end;

procedure TFormComputerUse.btnRunClick(Sender: TObject);
begin
  meLog.Lines.Clear;
  FShotCount := 0; // los screenshots de esta corrida empiezan en shot_001
  btnRun.Enabled := False;

  if cbProvider.ItemIndex = 1 then
  begin
    FConn.DriverName := 'Gemini';
    FConn.Model := 'gemini-2.5-computer-use-preview-10-2025';
    FConn.Params.Values['ApiKey'] := '@GEMINI_API_KEY';
  end
  else
  begin
    FConn.DriverName := 'Claude';
    FConn.Model := 'claude-opus-4-8';
    FConn.Params.Values['ApiKey'] := '@CLAUDE_API_KEY';
  end;

  SetStatus('Ejecutando con ' + FConn.DriverName + '...');
  Log('Proveedor: ' + FConn.DriverName + '  Modelo: ' + FConn.Model);
  Log('Tienes 4 segundos para llevar el foco a la app objetivo...');

  // Pequeña espera para que el usuario active la ventana objetivo.
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(4000);
      try
        FConn.AddMessageAndRun(mePrompt.Lines.Text, 'user', []);
      except
        on E: Exception do
          DoError(FConn, E.Message, E, nil);
      end;
    end).Start;
end;

end.
