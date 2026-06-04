unit uMakerAi.Tools.ComputerUse.WindowsFMX;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Math, System.UITypes, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages,
  FMX.Graphics, FMX.Surfaces, FMX.Types,
  uMakerAi.Tools.ComputerUse, uMakerAi.Core, uMakerAi.Utils.ScreenCapture;

type
  TAiWindowsFMXExecutor = class
  private
    // M�todos de Input (API Win32 pura)
    class procedure SendMouseInput(Flags: DWORD; Data: DWORD = 0);
    class procedure SendKeyboardInput(VK: Word; Up: Boolean);
    class procedure SendUnicodeChar(C: Char);
    class function GetVirtualKey(const KeyName: string): Word;
    class procedure ParseAndExecuteCombo(const Combo: string);
    class procedure SmoothMouseMove(DestX, DestY: Integer);

    // Helper para dibujar un cursor simulado sobre el Bitmap FMX
    // (Ya que la captura por BitBlt pura no incluye el cursor flotante)
    class procedure DrawCursorOnBitmap(Bmp: FMX.Graphics.TBitmap; AreaLeft, AreaTop: Integer);
  public
    // Ejecuta la acci�n f�sica
    class function Execute(const Action: TAiActionData): TAiActionResult;

    // Captura pantalla delegando en uMakerAi.Utils.ScreenCapture
    class procedure CaptureScreen(var MediaFile: TAiMediaFile; TargetArea: TRect; Quality: Integer = 70);
  end;

implementation

{ TAiWindowsFMXExecutor }

// =============================================================================
// SECCI�N DE INPUTS (Id�ntica a la anterior - Interact�a con Kernel)
// =============================================================================

class procedure TAiWindowsFMXExecutor.SendMouseInput(Flags: DWORD; Data: DWORD);
var
  Input: TInput;
begin
  ZeroMemory(@Input, SizeOf(Input));
  Input.Itype := INPUT_MOUSE;
  Input.mi.dwFlags := Flags;
  Input.mi.mouseData := Data;
  SendInput(1, Input, SizeOf(TInput));
end;

class procedure TAiWindowsFMXExecutor.SendKeyboardInput(VK: Word; Up: Boolean);
var
  Input: TInput;
begin
  ZeroMemory(@Input, SizeOf(Input));
  Input.Itype := INPUT_KEYBOARD;
  Input.ki.wVk := VK;
  if Up then
    Input.ki.dwFlags := KEYEVENTF_KEYUP;
  SendInput(1, Input, SizeOf(TInput));
end;

class procedure TAiWindowsFMXExecutor.SendUnicodeChar(C: Char);
var
  Input: TInput;
begin
  ZeroMemory(@Input, SizeOf(Input));
  Input.Itype := INPUT_KEYBOARD;
  Input.ki.wScan := Ord(C);
  Input.ki.dwFlags := KEYEVENTF_UNICODE;
  SendInput(1, Input, SizeOf(TInput));

  Input.ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
  SendInput(1, Input, SizeOf(TInput));
end;

class procedure TAiWindowsFMXExecutor.SmoothMouseMove(DestX, DestY: Integer);
begin
  // SetCursorPos maneja coordenadas virtuales autom�ticamente en Windows modernos
  SetCursorPos(DestX, DestY);
end;

class function TAiWindowsFMXExecutor.GetVirtualKey(const KeyName: string): Word;
var
  K: string;
begin
  K := LowerCase(Trim(KeyName));
  if K = 'enter' then Result := VK_RETURN
  else if K = 'tab' then Result := VK_TAB
  else if (K = 'ctrl') or (K = 'control') then Result := VK_CONTROL
  else if (K = 'alt') then Result := VK_MENU
  else if (K = 'shift') then Result := VK_SHIFT
  else if (K = 'esc') or (K = 'escape') then Result := VK_ESCAPE
  else if (K = 'space') then Result := VK_SPACE
  else if (K = 'backspace') then Result := VK_BACK
  else if (K = 'delete') then Result := VK_DELETE
  else if (K = 'win') or (K = 'windows') or (K = 'meta') then Result := VK_LWIN
  else if (Length(K) = 1) and (K[1] >= 'a') and (K[1] <= 'z') then
    Result := Ord(UpCase(K[1]))
  else if (Length(K) >= 2) and (K[1] = 'f') then
    Result := VK_F1 + StrToIntDef(Copy(K, 2, 2), 1) - 1
  else
    Result := 0;
end;

class procedure TAiWindowsFMXExecutor.ParseAndExecuteCombo(const Combo: string);
var
  Parts: TArray<string>;
  Part: string;
  Keys: TList<Word>;
  VK: Word;
  I: Integer;
begin
  Parts := Combo.Replace('-', '+').Split(['+']);
  Keys := TList<Word>.Create;
  try
    for Part in Parts do
    begin
      VK := GetVirtualKey(Part);
      if VK <> 0 then
      begin
        SendKeyboardInput(VK, False);
        Keys.Add(VK);
      end;
    end;
    Sleep(50);
    for I := Keys.Count - 1 downto 0 do
      SendKeyboardInput(Keys[I], True);
  finally
    Keys.Free;
  end;
end;

class function TAiWindowsFMXExecutor.Execute(const Action: TAiActionData): TAiActionResult;
var
  I: Integer;
begin
  Result.Success := True;
  Result.ErrorMessage := '';
  Result.CustomOutput := '';

  try
    case Action.ActionType of
      catHover: SmoothMouseMove(Action.X, Action.Y);

      catClick: begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
      end;

      catRightClick: begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_RIGHTDOWN);
        SendMouseInput(MOUSEEVENTF_RIGHTUP);
      end;

      catMiddleClick: begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_MIDDLEDOWN);
        SendMouseInput(MOUSEEVENTF_MIDDLEUP);
      end;

      catDoubleClick: begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
        Sleep(100);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
      end;

      catDrag: begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        Sleep(100);
        SmoothMouseMove((Action.X + Action.DestX) div 2, (Action.Y + Action.DestY) div 2);
        Sleep(50);
        SmoothMouseMove(Action.DestX, Action.DestY);
        Sleep(100);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
      end;

      catScroll: begin
        SmoothMouseMove(Action.X, Action.Y);
        if (Action.ScrollDirection = 'down') then
          SendMouseInput(MOUSEEVENTF_WHEEL, DWORD(-Action.ScrollAmount))
        else if (Action.ScrollDirection = 'up') then
          SendMouseInput(MOUSEEVENTF_WHEEL, DWORD(Action.ScrollAmount))
        else if (Action.ScrollDirection = 'left') then
          SendMouseInput(MOUSEEVENTF_HWHEEL, DWORD(-Action.ScrollAmount))
        else if (Action.ScrollDirection = 'right') then
          SendMouseInput(MOUSEEVENTF_HWHEEL, DWORD(Action.ScrollAmount));
      end;

      catType: begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
        Sleep(50);
        for I := 1 to Length(Action.TextToType) do
          SendUnicodeChar(Action.TextToType[I]);
        if Action.PressEnter then
        begin
          Sleep(50);
          SendKeyboardInput(VK_RETURN, False);
          SendKeyboardInput(VK_RETURN, True);
        end;
      end;

      catKeyCombination: ParseAndExecuteCombo(Action.KeyCombo);

      catWait: Sleep(5000);
    end;
  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := E.Message;
    end;
  end;
end;

// =============================================================================
// SECCI�N DE CAPTURA (Integraci�n con uMakerAi.Utils.ScreenCapture)
// =============================================================================

class procedure TAiWindowsFMXExecutor.DrawCursorOnBitmap(Bmp: FMX.Graphics.TBitmap; AreaLeft, AreaTop: Integer);
var
  CursorPos: TPoint;
  RelX, RelY: Single;
  R: TRectF;
begin
  // Obtener posici�n global del mouse
  if not GetCursorPos(CursorPos) then Exit;

  // Calcular posici�n relativa al �rea capturada
  RelX := CursorPos.X - AreaLeft;
  RelY := CursorPos.Y - AreaTop;

  // Si el cursor est� fuera de la imagen, no dibujamos nada
  if (RelX < 0) or (RelY < 0) or (RelX >= Bmp.Width) or (RelY >= Bmp.Height) then
    Exit;

  // Dibujamos un indicador visual (C�rculo rojo con borde blanco)
  // Esto es m�s f�cil en FMX que convertir HICONs y suficiente para la IA.
  if Bmp.Canvas.BeginScene then
  try
    R := TRectF.Create(RelX - 10, RelY - 10, RelX + 10, RelY + 10);

    // Relleno Rojo semitransparente
    Bmp.Canvas.Fill.Color := $80FF0000; // Alpha=128, Red
    Bmp.Canvas.FillEllipse(R, 1);

    // Borde Blanco s�lido para contraste
    Bmp.Canvas.Stroke.Color := TAlphaColors.White;
    Bmp.Canvas.Stroke.Thickness := 2;
    Bmp.Canvas.DrawEllipse(R, 1);
  finally
    Bmp.Canvas.EndScene;
  end;
end;

class procedure TAiWindowsFMXExecutor.CaptureScreen(var MediaFile: TAiMediaFile; TargetArea: TRect; Quality: Integer);
var
  Bmp: FMX.Graphics.TBitmap;
  Surf: TBitmapSurface;
  SaveParams: TBitmapCodecSaveParams;
  Stream: TMemoryStream;
  VirtualLeft, VirtualTop: Integer;
begin
  if not Assigned(MediaFile) then
    MediaFile := TAiMediaFile.Create;

  // 1. Validar el �rea
  if TargetArea.IsEmpty then
  begin
    VirtualLeft := GetSystemMetrics(SM_XVIRTUALSCREEN);
    VirtualTop := GetSystemMetrics(SM_YVIRTUALSCREEN);
    // Si viene vac�o, capturamos todo el virtual screen
    // (Aun as� se recomienda que el componente TAiComputerUseTool pase un �rea expl�cita)
    TargetArea := Rect(
      VirtualLeft,
      VirtualTop,
      VirtualLeft + GetSystemMetrics(SM_CXVIRTUALSCREEN),
      VirtualTop + GetSystemMetrics(SM_CYVIRTUALSCREEN)
    );
  end;

  // 2. Usar tu librer�a existente para la captura de p�xeles
  Bmp := TScreenCapture.CaptureArea(TargetArea);
  if not Assigned(Bmp) then Exit;

  try
    // 3. Dibujar el cursor sobre el Bitmap capturado
    // Es vital para que Gemini sepa d�nde est� el mouse
    DrawCursorOnBitmap(Bmp, TargetArea.Left, TargetArea.Top);

    // 4. Guardar como JPG comprimido
    Surf := TBitmapSurface.Create;
    try
      Surf.Assign(Bmp);

      Stream := TMemoryStream.Create;
      try
        SaveParams.Quality := Quality;

        if TBitmapCodecManager.SaveToStream(Stream, Surf, '.jpg', @SaveParams) then
        begin
          Stream.Position := 0;
          MediaFile.LoadFromStream('screenshot.jpg', Stream);
        end;
      finally
        Stream.Free;
      end;
    finally
      Surf.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

end.
