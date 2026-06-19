unit uMainCUServer;

// =============================================================================
//  Computer Use contra el SERVIDOR cimamaker (no directo a Claude).
//
//  Diferencia con uMainComputerUseTest:
//   - Aquel usa TAiChatConnection con loop agentico INTERNO directo a Claude/Gemini.
//   - Este implementa el loop A MANO contra api.cimamaker.com (formato OpenAI):
//       1. POST /v1/chat/completions con mk_tools:{computer_use:{display_width,display_height}}
//       2. El server (cerebro Claude) responde finish_reason="tool_calls" con un
//          tool_call name="computer" y argumentos en formato NATIVO de Claude
//          (ej. {"action":"left_click","coordinate":[x_px,y_px]}).
//       3. El cliente traduce y ejecuta la accion REAL + captura screenshot:
//            FCU.TranslateClaudeToolCall(tc);   // pixeles Claude -> formato interno
//            Res := FCU.ProcessToolCall(tc, Shot);
//       4. POST con el historial: assistant(tool_calls) + tool(result + image_url
//          data:image/jpeg;base64), y repite hasta finish_reason="stop".
//
//  El screenshot se envia EXACTAMENTE a display_width x display_height (CaptureScaled).
//  La captura corre en el hilo principal (FMX.Canvas no es thread-safe).
//  La API key sale de la variable de entorno MAKERAI_API_KEY (igual que el Service).
//
//  AVISO: controla el escritorio real. No dejes ventanas sensibles en primer plano.
// =============================================================================

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.JSON, System.Net.HttpClient, System.Net.URLClient, System.Math,
  {$IFDEF MSWINDOWS} Winapi.Windows, FMX.Platform.Win, {$ENDIF}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Surfaces,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Memo.Types, FMX.Layouts,
  uMakerAi.Core, uMakerAi.Chat.Messages,
  uMakerAi.Tools.ComputerUse, uMakerAi.Tools.ComputerUse.WindowsFMX,
  uMakerAi.Utils.ScreenCapture;

type
  TFormCUServer = class(TForm)
  private
    FCU: TAiComputerUseTool;
    FApiKey: string;
    FModel: string;
    FDisplayW, FDisplayH: Integer;
    FShotDir: string;
    FShotCount: Integer;
    FMessages: TJSONArray;     // historial de la conversacion (lo poseemos)
    // UI (creada en codigo)
    FPrompt: TMemo;
    FLog: TMemo;
    FStatus: TLabel;
    FRun: TButton;
    procedure BuildUI;
    procedure Log(const S: string);
    procedure SetStatus(const S: string);
    function CaptureScaled(const Area: TRect; OutW, OutH: Integer): TAiMediaFile;
    // Eventos de Computer Use
    procedure DoExecuteAction(Sender: TObject; const ActionData: TAiActionData; var Result: TAiActionResult);
    procedure DoRequestScreenshot(Sender: TObject; var MediaFile: TAiMediaFile);
    procedure DoSafetyConfirmation(Sender: TObject; const Explanation: string; var Allow: Boolean);
    // Loop manual
    procedure RunClick(Sender: TObject);
    procedure RunLoop(const APrompt: string);
    function  PostChat(const ABody: string): string;
    procedure AddUserMsg(const ARole, AText: string);
    function  BuildRequestBody: string;
    procedure EndRun(const AMsg: string);
  public
    constructor CreateNew(AOwner: TComponent; Dummy: NativeInt = 0); override;
    destructor Destroy; override;
  end;

var
  FormCUServer: TFormCUServer;

implementation

const
  MAKERAI_URL = 'https://api.cimamaker.com';
  CU_MODEL    = 'mk-claude-sonnet';   // confirmado via GET /v1/models
  MAX_STEPS   = 40;                   // tope de iteraciones del loop
  DEF_PROMPT  =
    'Eres un agente de automatizacion de escritorio en Windows.' + sLineBreak +
    'Tarea: toma primero un screenshot para ver la pantalla, luego abre el menu Inicio ' +
    'haciendo click en el icono de Windows de la barra de tareas. Toma otro screenshot ' +
    'para confirmar que el menu se abrio y describe brevemente lo que ves.';

{ TFormCUServer }

constructor TFormCUServer.CreateNew(AOwner: TComponent; Dummy: NativeInt);
begin
  inherited CreateNew(AOwner, Dummy);

  Caption := 'Computer Use vs Servidor cimamaker';
  Position := TFormPosition.ScreenCenter;
  Width := 900;
  Height := 680;

  FMessages := TJSONArray.Create;
  FModel := CU_MODEL;
  FApiKey := GetEnvironmentVariable('MAKERAI_API_KEY').Trim;

  // Componente de Computer Use (igual que el demo directo)
  FCU := TAiComputerUseTool.Create(Self);
  FCU.OnExecuteAction := DoExecuteAction;
  FCU.OnRequestScreenshot := DoRequestScreenshot;
  FCU.OnSafetyConfirmation := DoSafetyConfirmation;
  FCU.EnableZoom := True; // accion zoom de Claude (computer_20251124)

  // Area de captura = monitor primario fisico (origen 0,0).
  FCU.AreaLeft := 0;
  FCU.AreaTop := 0;
  {$IFDEF MSWINDOWS}
  FCU.AreaWidth := GetSystemMetrics(SM_CXSCREEN);
  FCU.AreaHeight := GetSystemMetrics(SM_CYSCREEN);
  {$ELSE}
  FCU.AreaWidth := 1920;
  FCU.AreaHeight := 1080;
  {$ENDIF}
  // Resolucion declarada a la IA = la del screenshot que enviamos.
  // display_width/display_height del mk_tools DEBEN coincidir con esto.
  FCU.ScreenWidth := 1280;
  FCU.ScreenHeight := Round(FCU.AreaHeight * 1280 / FCU.AreaWidth);
  FDisplayW := FCU.ScreenWidth;
  FDisplayH := FCU.ScreenHeight;

  FShotDir := ExtractFilePath(ParamStr(0)) + 'shots_server' + PathDelim;
  ForceDirectories(FShotDir);
  FShotCount := 0;

  BuildUI;

  FPrompt.Lines.Text := DEF_PROMPT;
  if FApiKey = '' then
    SetStatus('FALTA la variable de entorno MAKERAI_API_KEY.')
  else
    SetStatus('Listo. Abre la app objetivo y pulsa Ejecutar.');
  Log(Format('Modelo: %s   display: %dx%d', [FModel, FDisplayW, FDisplayH]));
  Log('Screenshots guardados en: ' + FShotDir);
end;

destructor TFormCUServer.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TFormCUServer.BuildUI;
var
  Top: TToolBar;
  Spl: TSplitter;
begin
  Top := TToolBar.Create(Self);
  Top.Parent := Self;
  Top.Align := TAlignLayout.Top;
  Top.Height := 44;

  FRun := TButton.Create(Self);
  FRun.Parent := Top;
  FRun.Align := TAlignLayout.Left;
  FRun.Width := 120;
  FRun.Margins.Rect := RectF(6, 6, 6, 6);
  FRun.Text := 'Ejecutar';
  FRun.OnClick := RunClick;

  FStatus := TLabel.Create(Self);
  FStatus.Parent := Top;
  FStatus.Align := TAlignLayout.Client;
  FStatus.Margins.Rect := RectF(8, 0, 8, 0);

  FPrompt := TMemo.Create(Self);
  FPrompt.Parent := Self;
  FPrompt.Align := TAlignLayout.Top;
  FPrompt.Height := 120;
  FPrompt.Margins.Rect := RectF(6, 6, 6, 0);

  Spl := TSplitter.Create(Self);
  Spl.Parent := Self;
  Spl.Align := TAlignLayout.Top;
  Spl.Height := 6;

  FLog := TMemo.Create(Self);
  FLog.Parent := Self;
  FLog.Align := TAlignLayout.Client;
  FLog.Margins.Rect := RectF(6, 0, 6, 6);
  FLog.ReadOnly := True;
end;

procedure TFormCUServer.Log(const S: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      FLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + S);
      FLog.GoToTextEnd;
    end);
end;

procedure TFormCUServer.SetStatus(const S: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      FStatus.Text := S;
    end);
end;

// -----------------------------------------------------------------------------
// Eventos de Computer Use (identicos al demo directo)
// -----------------------------------------------------------------------------
procedure TFormCUServer.DoExecuteAction(Sender: TObject; const ActionData: TAiActionData; var Result: TAiActionResult);
begin
  Log(Format('ACCION: %s  (x=%d y=%d mod="%s" keys="%s")',
    [ActionData.FunctionName, ActionData.X, ActionData.Y, ActionData.Modifiers, ActionData.KeyCombo]));
  Result := TAiWindowsFMXExecutor.Execute(ActionData);
  if not Result.Success then
    Log('  -> ERROR ejecutando: ' + Result.ErrorMessage)
  else if ActionData.ActionType = catCursorPosition then
    Log('  -> cursor: ' + Result.CustomOutput);
end;

function TFormCUServer.CaptureScaled(const Area: TRect; OutW, OutH: Integer): TAiMediaFile;
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
            if FShotDir <> '' then
            begin
              Inc(FShotCount);
              try
                Strm.Position := 0;
                Strm.SaveToFile(FShotDir + Format('shot_%.3d_%dx%d.jpg', [FShotCount, OutW, OutH]));
              except
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

procedure TFormCUServer.DoRequestScreenshot(Sender: TObject; var MediaFile: TAiMediaFile);
var
  Area: TRect;
  OutW, OutH: Integer;
  MF: TAiMediaFile;
begin
  if (FCU.CurrentAction.ActionType = catZoom) and not FCU.CurrentAction.ZoomRect.IsEmpty then
  begin
    Area := FCU.CurrentAction.ZoomRect;
    OutW := Area.Width;
    OutH := Area.Height;
  end
  else
  begin
    Area := Rect(FCU.AreaLeft, FCU.AreaTop, FCU.AreaLeft + FCU.AreaWidth, FCU.AreaTop + FCU.AreaHeight);
    OutW := FCU.ScreenWidth;
    OutH := FCU.ScreenHeight;
  end;

  MF := nil;
  TThread.Synchronize(nil,
    procedure
    begin
      MF := CaptureScaled(Area, OutW, OutH);
    end);
  MediaFile := MF;
  Log(Format('  -> screenshot %dx%d (origen %dx%d)', [OutW, OutH, Area.Width, Area.Height]));
end;

procedure TFormCUServer.DoSafetyConfirmation(Sender: TObject; const Explanation: string; var Allow: Boolean);
begin
  Log('SAFETY (auto-permitido): ' + Explanation);
  Allow := True;
end;

// -----------------------------------------------------------------------------
// Loop manual contra el servidor
// -----------------------------------------------------------------------------
procedure TFormCUServer.AddUserMsg(const ARole, AText: string);
var
  M: TJSONObject;
begin
  M := TJSONObject.Create;
  M.AddPair('role', ARole);
  M.AddPair('content', AText);
  FMessages.AddElement(M);
end;

function TFormCUServer.BuildRequestBody: string;
var
  Req, Mk, Cu: TJSONObject;
begin
  Req := TJSONObject.Create;
  try
    Req.AddPair('model', FModel);
    Req.AddPair('messages', FMessages.Clone as TJSONValue);
    Cu := TJSONObject.Create;
    Cu.AddPair('display_width', TJSONNumber.Create(FDisplayW));
    Cu.AddPair('display_height', TJSONNumber.Create(FDisplayH));
    Mk := TJSONObject.Create;
    Mk.AddPair('computer_use', Cu);
    Req.AddPair('mk_tools', Mk);
    Req.AddPair('stream', TJSONBool.Create(False));
    Result := Req.ToJSON;
  finally
    Req.Free;
  end;
end;

function TFormCUServer.PostChat(const ABody: string): string;
var
  Client: THTTPClient;
  Body: TStringStream;
  Resp: IHTTPResponse;
  Headers: TNetHeaders;
begin
  Client := THTTPClient.Create;
  Body := TStringStream.Create(ABody, TEncoding.UTF8);
  try
    Client.ConnectionTimeout := 30000;
    Client.ResponseTimeout := 180000;
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey),
                TNetHeader.Create('Content-Type', 'application/json')];
    Resp := Client.Post(MAKERAI_URL + '/v1/chat/completions', Body, nil, Headers);
    Result := Resp.ContentAsString(TEncoding.UTF8);
    if (Resp.StatusCode < 200) or (Resp.StatusCode >= 300) then
      raise Exception.CreateFmt('HTTP %d: %s', [Resp.StatusCode, Copy(Result, 1, 500)]);
  finally
    Body.Free;
    Client.Free;
  end;
end;

procedure TFormCUServer.RunLoop(const APrompt: string);
var
  Step: Integer;
  RespStr, FinishReason, ToolId, ToolName, ToolArgs, AsstText: string;
  JResp, JChoice, JMsg, JTc, JFunc: TJSONObject;
  JChoices, JToolCalls: TJSONArray;
  ToolCall: TAiToolsFunction;
  Shot: TAiMediaFile;
  ResultStr, DataUri: string;
  ToolMsg, TxtPart, ImgPart, ImgUrl: TJSONObject;
  ContentArr: TJSONArray;
  I: Integer;
  JV: TJSONValue;
begin
  try
    // mensaje inicial (el system va embebido por el server; mandamos user con la tarea)
    AddUserMsg('user', APrompt);

    for Step := 1 to MAX_STEPS do
    begin
      SetStatus(Format('Paso %d/%d: consultando al servidor...', [Step, MAX_STEPS]));
      RespStr := PostChat(BuildRequestBody);

      JResp := TJSONObject.ParseJSONValue(RespStr) as TJSONObject;
      if not Assigned(JResp) then
        raise Exception.Create('Respuesta no es JSON: ' + Copy(RespStr, 1, 300));
      try
        JChoices := JResp.GetValue('choices') as TJSONArray;
        if (not Assigned(JChoices)) or (JChoices.Count = 0) then
          raise Exception.Create('Sin choices en la respuesta: ' + Copy(RespStr, 1, 300));

        JChoice := JChoices.Items[0] as TJSONObject;
        FinishReason := '';
        JChoice.TryGetValue<string>('finish_reason', FinishReason);
        JMsg := JChoice.GetValue('message') as TJSONObject;

        AsstText := '';
        if Assigned(JMsg) then
          JMsg.TryGetValue<string>('content', AsstText);

        // tool_calls?
        JToolCalls := nil;
        if Assigned(JMsg) then
          JMsg.GetValue('tool_calls', JToolCalls); // queda nil si no existe

        if (FinishReason = 'tool_calls') and Assigned(JToolCalls) and (JToolCalls.Count > 0) then
        begin
          if AsstText <> '' then
            Log('Modelo: ' + AsstText);

          // 1) Echo del mensaje assistant TAL CUAL (role+content+tool_calls)
          FMessages.AddElement(JMsg.Clone as TJSONValue);

          // 2) Ejecutar cada tool_call y anexar su tool result
          for I := 0 to JToolCalls.Count - 1 do
          begin
            JTc := JToolCalls.Items[I] as TJSONObject;
            ToolId := '';
            JTc.TryGetValue<string>('id', ToolId);
            JFunc := JTc.GetValue('function') as TJSONObject;
            ToolName := '';
            ToolArgs := '';
            if Assigned(JFunc) then
            begin
              JFunc.TryGetValue<string>('name', ToolName);
              JFunc.TryGetValue<string>('arguments', ToolArgs);
            end;

            Log(Format('TOOL_CALL[%d] name=%s args=%s', [I, ToolName, ToolArgs]));

            Shot := nil;
            ToolCall := TAiToolsFunction.Create;
            try
              ToolCall.id := ToolId;
              ToolCall.name := ToolName;            // 'computer'
              ToolCall.Arguments := ToolArgs;       // JSON nativo Claude

              // pixeles Claude -> formato interno (muta Name y Arguments)
              FCU.TranslateClaudeToolCall(ToolCall);
              // ejecuta accion REAL + captura screenshot (out Shot)
              ResultStr := FCU.ProcessToolCall(ToolCall, Shot);

              // tool message con content array [texto, imagen]
              ToolMsg := TJSONObject.Create;
              ToolMsg.AddPair('role', 'tool');
              ToolMsg.AddPair('tool_call_id', ToolId);
              ContentArr := TJSONArray.Create;

              TxtPart := TJSONObject.Create;
              TxtPart.AddPair('type', 'text');
              TxtPart.AddPair('text', ResultStr);
              ContentArr.AddElement(TxtPart);

              if Assigned(Shot) and (Shot.Base64 <> '') then
              begin
                DataUri := 'data:' + Shot.MimeType + ';base64,' + Shot.Base64;
                ImgUrl := TJSONObject.Create;
                ImgUrl.AddPair('url', DataUri);
                ImgPart := TJSONObject.Create;
                ImgPart.AddPair('type', 'image_url');
                ImgPart.AddPair('image_url', ImgUrl);
                ContentArr.AddElement(ImgPart);
              end;

              ToolMsg.AddPair('content', ContentArr);
              FMessages.AddElement(ToolMsg);
            finally
              Shot.Free;
              ToolCall.Free;
            end;
          end;
          // siguiente iteracion del loop
        end
        else
        begin
          // finish_reason = 'stop' (u otro): fin del loop
          EndRun('=== FIN (' + FinishReason + ') ===' + sLineBreak +
                 'Modelo: ' + AsstText);
          Exit;
        end;
      finally
        JResp.Free;
      end;
    end;

    EndRun('=== Tope de pasos alcanzado (' + IntToStr(MAX_STEPS) + ') ===');
  except
    on E: Exception do
      EndRun('ERROR: ' + E.Message);
  end;
end;

procedure TFormCUServer.EndRun(const AMsg: string);
begin
  Log(AMsg);
  SetStatus('Completado.');
  TThread.Queue(nil, procedure begin FRun.Enabled := True; end);
end;

procedure TFormCUServer.RunClick(Sender: TObject);
var
  Prompt: string;
begin
  if FApiKey = '' then
  begin
    Log('No hay API key (MAKERAI_API_KEY). Aborta.');
    Exit;
  end;

  FLog.Lines.Clear;
  FShotCount := 0;
  FRun.Enabled := False;

  // limpiar historial previo
  FMessages.Free;
  FMessages := TJSONArray.Create;

  Prompt := FPrompt.Lines.Text;
  SetStatus('Ejecutando contra ' + MAKERAI_URL + ' ...');
  Log('Servidor: ' + MAKERAI_URL + '   Modelo: ' + FModel);
  Log('Tienes 4 segundos para llevar el foco a la app objetivo...');

  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(4000);
      RunLoop(Prompt);
    end).Start;
end;

end.
