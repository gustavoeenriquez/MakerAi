unit uMakerAi.Tools.ComputerUse.Windows;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Generics.Collections,
  System.StrUtils, Vcl.Forms, Vcl.Graphics, System.Types,
  uMakerAi.Tools.ComputerUse, uMakerAi.Core;

type
  TAiWindowsExecutor = class
  private
    class procedure SendMouseInput(Flags: DWORD; Data: DWORD = 0);
    class procedure SendKeyboardInput(VK: Word; Up: Boolean);
    class procedure SendUnicodeChar(C: Char);
    class function GetVirtualKey(const KeyName: string): Word;
    class procedure ParseAndExecuteCombo(const Combo: string);
    class procedure SmoothMouseMove(DestX, DestY: Integer);
  public
    // Ejecuta la acci�n f�sica basada en los datos procesados
    class function Execute(const Action: TAiActionData): TAiActionResult;

    // Helper para capturar la pantalla y devolverla como TAiMediaFile
    // Quality: 1-100 (JPG compression). Recomendado: 60-75 para velocidad.
    class procedure CaptureScreen(var MediaFile: TAiMediaFile; TargetArea: TRect; Quality: Integer = 70);
  end;

implementation

uses
  System.IOUtils, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage;

{ TAiWindowsExecutor }

// -----------------------------------------------------------------------------
// HELPER: SendInput Wrapper
// -----------------------------------------------------------------------------
class procedure TAiWindowsExecutor.SendMouseInput(Flags: DWORD; Data: DWORD);
var
  Input: TInput;
begin
  ZeroMemory(@Input, SizeOf(Input));
  Input.Itype := INPUT_MOUSE;
  Input.mi.dwFlags := Flags;
  Input.mi.mouseData := Data;
  SendInput(1, Input, SizeOf(TInput));
end;

class procedure TAiWindowsExecutor.SendKeyboardInput(VK: Word; Up: Boolean);
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

class procedure TAiWindowsExecutor.SendUnicodeChar(C: Char);
var
  Input: TInput;
begin
  ZeroMemory(@Input, SizeOf(Input));
  Input.Itype := INPUT_KEYBOARD;
  Input.ki.wScan := Ord(C);
  Input.ki.dwFlags := KEYEVENTF_UNICODE;
  SendInput(1, Input, SizeOf(TInput));

  // Release (necesario para algunos inputs unicode)
  Input.ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
  SendInput(1, Input, SizeOf(TInput));
end;

class procedure TAiWindowsExecutor.SmoothMouseMove(DestX, DestY: Integer);
begin
  // SetCursorPos es instant�neo y es lo que Gemini espera generalmente.
  // Si se quisiera simular movimiento humano, aqu� ir�a un algoritmo de interpolaci�n.
  SetCursorPos(DestX, DestY);
end;

// -----------------------------------------------------------------------------
// HELPER: Key Mapping
// -----------------------------------------------------------------------------
class function TAiWindowsExecutor.GetVirtualKey(const KeyName: string): Word;
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
  else if (K = 'up') then Result := VK_UP
  else if (K = 'down') then Result := VK_DOWN
  else if (K = 'left') then Result := VK_LEFT
  else if (K = 'right') then Result := VK_RIGHT
  else if (K = 'home') then Result := VK_HOME
  else if (K = 'end') then Result := VK_END
  else if (K = 'page_up') then Result := VK_PRIOR
  else if (K = 'page_down') then Result := VK_NEXT
  else if (K = 'win') or (K = 'windows') or (K = 'meta') or (K = 'super') then Result := VK_LWIN
  else if (Length(K) = 1) and (K[1] >= 'a') and (K[1] <= 'z') then
    Result := Ord(UpCase(K[1])) // A-Z
  else if (Length(K) >= 2) and (K[1] = 'f') then // F1-F12
    Result := VK_F1 + StrToIntDef(Copy(K, 2, 2), 1) - 1
  else
    Result := 0;
end;

class procedure TAiWindowsExecutor.ParseAndExecuteCombo(const Combo: string);
var
  Parts: TArray<string>;
  Part: string;
  Keys: TList<Word>;
  VK: Word;
  I: Integer;
begin
  // Ejemplo: "Control+Shift+S"
  // Reemplazar separadores comunes
  Parts := Combo.Replace('-', '+').Split(['+']);
  Keys := TList<Word>.Create;
  try
    // 1. Press Down Sequence
    for Part in Parts do
    begin
      VK := GetVirtualKey(Part);
      if VK <> 0 then
      begin
        SendKeyboardInput(VK, False); // Key Down
        Keys.Add(VK);
      end;
    end;

    // Peque�a pausa para asegurar que la app detecte el combo
    Sleep(50);

    // 2. Release Sequence (Reverse Order)
    for I := Keys.Count - 1 downto 0 do
    begin
      SendKeyboardInput(Keys[I], True); // Key Up
    end;
  finally
    Keys.Free;
  end;
end;

// -----------------------------------------------------------------------------
// MAIN EXECUTION LOGIC
// -----------------------------------------------------------------------------
class function TAiWindowsExecutor.Execute(const Action: TAiActionData): TAiActionResult;
var
  I: Integer;
begin
  Result.Success := True;
  Result.ErrorMessage := '';
  Result.CustomOutput := '';

  try
    case Action.ActionType of
      // --- MOUSE ---
      catHover:
      begin
        SmoothMouseMove(Action.X, Action.Y);
      end;

      catClick:
      begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
      end;

      catRightClick:
      begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_RIGHTDOWN);
        SendMouseInput(MOUSEEVENTF_RIGHTUP);
      end;

      catMiddleClick:
      begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_MIDDLEDOWN);
        SendMouseInput(MOUSEEVENTF_MIDDLEUP);
      end;

      catDoubleClick:
      begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
        Sleep(100); // Pausa t�pica para doble clic
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
      end;

      catDrag:
      begin
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        Sleep(100); // Pausa para iniciar drag

        // Movimiento interpolado simple para que el OS detecte el arrastre
        // Mover a mitad de camino
        SmoothMouseMove((Action.X + Action.DestX) div 2, (Action.Y + Action.DestY) div 2);
        Sleep(50);
        // Mover a destino
        SmoothMouseMove(Action.DestX, Action.DestY);
        Sleep(100);

        SendMouseInput(MOUSEEVENTF_LEFTUP);
      end;

      catScroll:
      begin
        // Mover mouse a posici�n para asegurar que el scroll afecte a la ventana correcta
        SmoothMouseMove(Action.X, Action.Y);

        // Magnitude default en Gemini es 800 (WHEEL_DELTA es 120)
        // 800 unidades normalizadas ~ varios pasos de rueda.

        if (Action.ScrollDirection = 'down') then
          SendMouseInput(MOUSEEVENTF_WHEEL, DWORD(-Action.ScrollAmount))
        else if (Action.ScrollDirection = 'up') then
          SendMouseInput(MOUSEEVENTF_WHEEL, DWORD(Action.ScrollAmount))
        else if (Action.ScrollDirection = 'left') then
          SendMouseInput(MOUSEEVENTF_HWHEEL, DWORD(-Action.ScrollAmount))
        else if (Action.ScrollDirection = 'right') then
          SendMouseInput(MOUSEEVENTF_HWHEEL, DWORD(Action.ScrollAmount));
      end;

      // --- TECLADO ---
      catType:
      begin
        // Si la IA pide click antes, lo hacemos
        // (Aunque Gemini 2.5 suele separar las acciones, a veces asume que type incluye click)
        SmoothMouseMove(Action.X, Action.Y);
        SendMouseInput(MOUSEEVENTF_LEFTDOWN);
        SendMouseInput(MOUSEEVENTF_LEFTUP);
        Sleep(50); // Esperar foco

        // Escribir texto Unicode
        for I := 1 to Length(Action.TextToType) do
          SendUnicodeChar(Action.TextToType[I]);

        if Action.PressEnter then
        begin
          Sleep(50);
          SendKeyboardInput(VK_RETURN, False);
          SendKeyboardInput(VK_RETURN, True);
        end;
      end;

      catKeyCombination:
      begin
        ParseAndExecuteCombo(Action.KeyCombo);
      end;

      // --- OTROS ---
      catWait:
      begin
        Sleep(5000); // Gemini define wait_5_seconds
      end;

      catNavigate:
      begin
         // Aqu� no hacemos nada f�sico en Desktop,
         // pero podr�amos abrir el navegador default si quisi�ramos.
         // Por ahora, asumimos �xito l�gico y retornamos la URL en el componente principal.
      end;

    else
      // catScreenshot y otros se manejan fuera o no requieren acci�n de input
    end;

  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := E.Message;
    end;
  end;
end;

// -----------------------------------------------------------------------------
// HELPER: Screen Capture
// -----------------------------------------------------------------------------
{class procedure TAiWindowsExecutor.CaptureScreen(var MediaFile: TAiMediaFile; TargetArea: TRect; Quality: Integer = 70);
var
  DC: HDC;
  Bmp: TBitmap;
  Jpg: TJPEGImage;
  Stream: TMemoryStream;
  ScreenWidth, ScreenHeight: Integer;
begin
  if not Assigned(MediaFile) then
    MediaFile := TAiMediaFile.Create;

  // 1. Obtener dimensiones reales del monitor principal
  // Nota: Para soporte multimonitor, se requerir�a l�gica extra (EnumDisplayMonitors)
  ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
  ScreenHeight := GetSystemMetrics(SM_CYSCREEN);

  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(ScreenWidth, ScreenHeight);
    DC := GetDC(0); // 0 = Desktop HWND
    try
      // Copia r�pida BitBlt
      BitBlt(Bmp.Canvas.Handle, 0, 0, ScreenWidth, ScreenHeight, DC, 0, 0, SRCCOPY);
    finally
      ReleaseDC(0, DC);
    end;

    // Opcional: Dibujar cursor (Gemini ayuda a saber d�nde est� el mouse)

    var CursorPos: TPoint;
    var IconInfo: TIconInfo;
    GetCursorPos(CursorPos);
    if GetIconInfo(GetCursor, IconInfo) then
    begin
      DrawIcon(Bmp.Canvas.Handle, CursorPos.X - IconInfo.xHotspot, CursorPos.Y - IconInfo.yHotspot, GetCursor);
      DeleteObject(IconInfo.hbmMask);
      DeleteObject(IconInfo.hbmColor);
    end;


    // 2. Comprimir a JPEG
    Jpg := TJPEGImage.Create;
    try
      Jpg.Assign(Bmp);
      Jpg.CompressionQuality := Quality; // 70 es buen balance calidad/tama�o
      Jpg.Compress;

      Stream := TMemoryStream.Create;
      Jpg.SaveToStream(Stream);
      Stream.Position := 0;

      // 3. Cargar en MediaFile
      MediaFile.LoadFromStream('screenshot.jpg', Stream);

      // Liberar Stream (LoadFromStream suele hacer copia, o TAiMediaFile se adue�a, revisar implementaci�n base)
      // Asumiendo que LoadFromStream copia:
      Stream.Free;
    finally
      Jpg.Free;
    end;
  finally
    Bmp.Free;
  end;
end;
}


class procedure TAiWindowsExecutor.CaptureScreen(var MediaFile: TAiMediaFile; TargetArea: TRect; Quality: Integer);
var
  DC: HDC;
  Bmp: TBitmap;
  Jpg: TJPEGImage;
  Stream: TMemoryStream;
  W, H: Integer;

  // Variables para capturar el cursor
  CursorInfo: TCursorInfo;
  IconInfo: TIconInfo;
//  hIcon: HICON;
begin
  // 1. Asegurar que el objeto de salida existe
  if not Assigned(MediaFile) then
    MediaFile := TAiMediaFile.Create;

  // 2. Calcular dimensiones basadas en el Rect�ngulo solicitado
  W := TargetArea.Width;
  H := TargetArea.Height;

  // Fallback de seguridad: Si el �rea es inv�lida, capturar monitor principal completo
  if (W <= 0) or (H <= 0) then
  begin
    W := GetSystemMetrics(SM_CXSCREEN);
    H := GetSystemMetrics(SM_CYSCREEN);
    TargetArea := Rect(0, 0, W, H);
  end;

  Bmp := TBitmap.Create;
  try
    // pf24bit es suficiente y m�s r�pido para JPEGs
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(W, H);

    DC := GetDC(0); // 0 = Handle del Escritorio completo
    try
      // 3. Captura de P�xeles (BitBlt)
      // Copiamos desde el DC de pantalla hacia el Canvas del Bitmap.
      // Usamos TargetArea.Left y Top como coordenadas origen (Source X, Source Y).
      BitBlt(Bmp.Canvas.Handle, 0, 0, W, H, DC, TargetArea.Left, TargetArea.Top, SRCCOPY);

      // 4. Dibujar el Cursor (Overlay)
      // Esto es crucial para que la IA sepa donde est� el puntero relativo a la imagen.
      {
      CursorInfo.cbSize := SizeOf(CursorInfo);
      if GetCursorInfo(CursorInfo) and (CursorInfo.flags = CURSOR_SHOWING) then
      begin
        hIcon := CopyIcon(CursorInfo.hCursor);
        if hIcon <> 0 then
        try
          if GetIconInfo(hIcon, IconInfo) then
          begin
            // Calcular posici�n relativa:
            // PosicionRealMouse - InicioDeCaptura - OffsetDelHotspotIcono
            DrawIcon(Bmp.Canvas.Handle,
                     CursorInfo.ptScreenPos.x - TargetArea.Left - IconInfo.xHotspot,
                     CursorInfo.ptScreenPos.y - TargetArea.Top - IconInfo.yHotspot,
                     hIcon);

            // Limpieza de recursos de icono
            DeleteObject(IconInfo.hbmMask);
            DeleteObject(IconInfo.hbmColor);
          end;
        finally
          DestroyIcon(hIcon);
        end;
      end;
      }

    finally
      ReleaseDC(0, DC);
    end;

    // 5. Compresi�n y Guardado
    Jpg := TJPEGImage.Create;
    try
      Jpg.Assign(Bmp);
      Jpg.CompressionQuality := Quality; // Recomendado: 60-75 para velocidad/tama�o
      Jpg.Compress;

      Stream := TMemoryStream.Create;
      try
        Jpg.SaveToStream(Stream);
        Stream.Position := 0;

        // Cargar en el MediaFile
        // El nombre 'screenshot.jpg' ayuda a TAiMediaFile a detectar el MimeType correcto
        MediaFile.LoadFromStream('screenshot.jpg', Stream);
      finally
        Stream.Free;
      end;
    finally
      Jpg.Free;
    end;

  finally
    Bmp.Free;
  end;
end;

end.
