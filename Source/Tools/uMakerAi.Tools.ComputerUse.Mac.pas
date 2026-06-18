unit uMakerAi.Tools.ComputerUse.Mac;

// =============================================================================
//  Executor de Computer Use para macOS.
//
//  - Input (mouse / teclado / scroll) vía Quartz Event Services (CGEvent*).
//  - Captura de pantalla delegada en TScreenCapture (uMakerAi.Utils.ScreenCapture),
//    que ya implementa la captura nativa de macOS (CGDisplayCreateImageForRect).
//  - Cursor dibujado sobre el bitmap (CGEventGetLocation) como en el executor FMX.
//
//  IMPORTANTE:
//   * macOS está marcado como NO soportado oficialmente en uMakerAi.Version.inc
//     (MAKERAI_SUPPORT_MACOS = False). Esta unidad compila solo bajo {$IFDEF MACOS};
//     en Windows/Linux queda como una unidad vacía válida.
//   * CGEventPost requiere que la app tenga permisos de Accesibilidad
//     (System Settings > Privacy & Security > Accessibility). Sin ellos los
//     eventos se crean pero el sistema los ignora.
//   * NO probado en hardware macOS todavía — pendiente de validación runtime,
//     igual que los transports OpenSSL/Android del módulo WebSocket.
//
//  La interfaz pública (Execute / CaptureScreen) es idéntica a TAiWindowsFMXExecutor
//  para que el manejador OnExecuteAction / OnRequestScreenshot sea intercambiable
//  por plataforma.
// =============================================================================

interface

{$IFDEF MACOS}

uses
  System.SysUtils, System.Classes, System.Types, System.Math, System.UITypes,
  System.Generics.Collections, System.StrUtils,
  Macapi.CoreFoundation, Macapi.CoreGraphics,
  FMX.Graphics, FMX.Surfaces, FMX.Types,
  uMakerAi.Tools.ComputerUse, uMakerAi.Core, uMakerAi.Utils.ScreenCapture;

type
  TAiMacExecutor = class
  private
    // --- Helpers de input (CGEvent) ---
    class procedure PostMouse(EvType, Btn: Integer; X, Y: Double; ClickState: Integer; Flags: CGEventFlags);
    class procedure MoveMouse(X, Y: Double);
    class procedure PostScroll(WheelV, WheelH: Integer; Flags: CGEventFlags);
    class procedure PostKey(KeyCode: Word; KeyDown: Boolean; Flags: CGEventFlags);
    class procedure TypeUnicode(const S: string);
    class function GetKeyCode(const KeyName: string; out IsModifier: Boolean): Word;
    class function ModifierFlag(const KeyName: string): CGEventFlags;
    class function FlagsFromMods(const Mods: string): CGEventFlags;
    class procedure ExecuteCombo(const Combo: string);
    class procedure HoldKeys(const Combo: string; DurationSecs: Double);
    class function CursorPos: TPointF;
    class procedure DrawCursorOnBitmap(Bmp: FMX.Graphics.TBitmap; AreaLeft, AreaTop: Integer);
  public
    class function Execute(const Action: TAiActionData): TAiActionResult;
    class procedure CaptureScreen(var MediaFile: TAiMediaFile; TargetArea: TRect; Quality: Integer = 70);
  end;

{$ENDIF}

implementation

{$IFDEF MACOS}

const
  // --- Quartz Event Services (valores estables de la API; se castean al enum
  //     correspondiente al invocar, para no depender de los identificadores
  //     concretos que exponga la versión de Macapi.CoreGraphics instalada). ---

  // CGEventTapLocation
  cHIDTap = 0; // kCGHIDEventTap

  // CGEventType (mouse)
  cLeftDown    = 1;  // kCGEventLeftMouseDown
  cLeftUp      = 2;  // kCGEventLeftMouseUp
  cRightDown   = 3;  // kCGEventRightMouseDown
  cRightUp     = 4;  // kCGEventRightMouseUp
  cMouseMoved  = 5;  // kCGEventMouseMoved
  cLeftDragged = 6;  // kCGEventLeftMouseDragged
  cOtherDown   = 25; // kCGEventOtherMouseDown
  cOtherUp     = 26; // kCGEventOtherMouseUp

  // CGMouseButton
  cBtnLeft   = 0; // kCGMouseButtonLeft
  cBtnRight  = 1; // kCGMouseButtonRight
  cBtnCenter = 2; // kCGMouseButtonCenter

  // CGScrollEventUnit
  cScrollPixel = 0; // kCGScrollEventUnitPixel

  // CGEventField
  cFieldClickState = 1; // kCGMouseEventClickState

  // CGEventFlags (máscara de modificadores)
  cFlagShift   = CGEventFlags($00020000); // kCGEventFlagMaskShift
  cFlagControl = CGEventFlags($00040000); // kCGEventFlagMaskControl
  cFlagAlt     = CGEventFlags($00080000); // kCGEventFlagMaskAlternate
  cFlagCmd     = CGEventFlags($00100000); // kCGEventFlagMaskCommand

  // Keycodes virtuales ANSI de macOS para modificadores
  kVK_Command = 55;
  kVK_Shift   = 56;
  kVK_Option  = 58;
  kVK_Control = 59;

// -----------------------------------------------------------------------------
// Helpers de input
// -----------------------------------------------------------------------------
class procedure TAiMacExecutor.PostMouse(EvType, Btn: Integer; X, Y: Double; ClickState: Integer; Flags: CGEventFlags);
var
  P: CGPoint;
  Ev: CGEventRef;
begin
  P.X := X;
  P.Y := Y;
  Ev := CGEventCreateMouseEvent(nil, CGEventType(EvType), P, CGMouseButton(Btn));
  if Ev = nil then
    Exit;
  try
    if ClickState > 1 then
      CGEventSetIntegerValueField(Ev, CGEventField(cFieldClickState), ClickState);
    if Flags <> 0 then
      CGEventSetFlags(Ev, Flags);
    CGEventPost(CGEventTapLocation(cHIDTap), Ev);
  finally
    CFRelease(Ev);
  end;
end;

class procedure TAiMacExecutor.MoveMouse(X, Y: Double);
var
  P: CGPoint;
begin
  P.X := X;
  P.Y := Y;
  CGWarpMouseCursorPosition(P);
  // Un MouseMoved adicional ayuda a que las apps detecten el hover.
  PostMouse(cMouseMoved, cBtnLeft, X, Y, 0, 0);
end;

class procedure TAiMacExecutor.PostScroll(WheelV, WheelH: Integer; Flags: CGEventFlags);
var
  Ev: CGEventRef;
begin
  if WheelH <> 0 then
    // wheelCount=2: wheel1 = vertical, wheel2 = horizontal
    Ev := CGEventCreateScrollWheelEvent(nil, CGScrollEventUnit(cScrollPixel), 2, WheelV, WheelH)
  else
    Ev := CGEventCreateScrollWheelEvent(nil, CGScrollEventUnit(cScrollPixel), 1, WheelV);
  if Ev = nil then
    Exit;
  try
    if Flags <> 0 then
      CGEventSetFlags(Ev, Flags);
    CGEventPost(CGEventTapLocation(cHIDTap), Ev);
  finally
    CFRelease(Ev);
  end;
end;

class procedure TAiMacExecutor.PostKey(KeyCode: Word; KeyDown: Boolean; Flags: CGEventFlags);
var
  Ev: CGEventRef;
begin
  Ev := CGEventCreateKeyboardEvent(nil, CGKeyCode(KeyCode), KeyDown);
  if Ev = nil then
    Exit;
  try
    if Flags <> 0 then
      CGEventSetFlags(Ev, Flags);
    CGEventPost(CGEventTapLocation(cHIDTap), Ev);
  finally
    CFRelease(Ev);
  end;
end;

class procedure TAiMacExecutor.TypeUnicode(const S: string);
var
  I: Integer;
  Ch: UInt16;
  Ev: CGEventRef;
begin
  for I := 1 to Length(S) do
  begin
    Ch := UInt16(Ord(S[I]));

    // Key down + unicode
    Ev := CGEventCreateKeyboardEvent(nil, 0, True);
    if Ev <> nil then
    try
      CGEventKeyboardSetUnicodeString(Ev, 1, @Ch);
      CGEventPost(CGEventTapLocation(cHIDTap), Ev);
    finally
      CFRelease(Ev);
    end;

    // Key up
    Ev := CGEventCreateKeyboardEvent(nil, 0, False);
    if Ev <> nil then
    try
      CGEventKeyboardSetUnicodeString(Ev, 1, @Ch);
      CGEventPost(CGEventTapLocation(cHIDTap), Ev);
    finally
      CFRelease(Ev);
    end;
  end;
end;

class function TAiMacExecutor.ModifierFlag(const KeyName: string): CGEventFlags;
var
  K: string;
begin
  K := LowerCase(Trim(KeyName));
  if (K = 'shift') then
    Result := cFlagShift
  else if (K = 'ctrl') or (K = 'control') then
    Result := cFlagControl
  else if (K = 'alt') or (K = 'option') then
    Result := cFlagAlt
  else if (K = 'cmd') or (K = 'command') or (K = 'win') or (K = 'meta') or (K = 'super') then
    Result := cFlagCmd
  else
    Result := 0;
end;

class function TAiMacExecutor.FlagsFromMods(const Mods: string): CGEventFlags;
var
  Part: string;
begin
  Result := 0;
  if Trim(Mods) = '' then
    Exit;
  for Part in Mods.Replace('-', '+').Split(['+']) do
    Result := Result or ModifierFlag(Part);
end;

class function TAiMacExecutor.GetKeyCode(const KeyName: string; out IsModifier: Boolean): Word;
var
  K: string;
begin
  IsModifier := False;
  K := LowerCase(Trim(KeyName));

  // Modificadores (devuelven keycode real para hold_key)
  if (K = 'shift') then begin IsModifier := True; Exit(kVK_Shift); end;
  if (K = 'ctrl') or (K = 'control') then begin IsModifier := True; Exit(kVK_Control); end;
  if (K = 'alt') or (K = 'option') then begin IsModifier := True; Exit(kVK_Option); end;
  if (K = 'cmd') or (K = 'command') or (K = 'win') or (K = 'meta') or (K = 'super') then
  begin IsModifier := True; Exit(kVK_Command); end;

  // Teclas comunes (kVK_ANSI_*)
  if K = 'enter' then Exit(36)        // kVK_Return
  else if K = 'return' then Exit(36)
  else if K = 'tab' then Exit(48)
  else if K = 'space' then Exit(49)
  else if (K = 'backspace') then Exit(51)  // kVK_Delete
  else if (K = 'delete') then Exit(117)    // kVK_ForwardDelete
  else if (K = 'esc') or (K = 'escape') then Exit(53)
  else if K = 'left' then Exit(123)
  else if K = 'right' then Exit(124)
  else if K = 'down' then Exit(125)
  else if K = 'up' then Exit(126)
  else if K = 'home' then Exit(115)
  else if K = 'end' then Exit(119)
  else if K = 'page_up' then Exit(116)
  else if K = 'page_down' then Exit(121);

  // Función F1..F12
  if (Length(K) >= 2) and (K[1] = 'f') then
  begin
    case StrToIntDef(Copy(K, 2, 2), 0) of
      1: Exit(122); 2: Exit(120); 3: Exit(99); 4: Exit(118);
      5: Exit(96);  6: Exit(97);  7: Exit(98); 8: Exit(100);
      9: Exit(101); 10: Exit(109); 11: Exit(103); 12: Exit(111);
    end;
  end;

  // Letras a-z (mapa ANSI no secuencial)
  if (Length(K) = 1) and (K[1] >= 'a') and (K[1] <= 'z') then
  begin
    case K[1] of
      'a': Exit(0);  'b': Exit(11); 'c': Exit(8);  'd': Exit(2);  'e': Exit(14);
      'f': Exit(3);  'g': Exit(5);  'h': Exit(4);  'i': Exit(34); 'j': Exit(38);
      'k': Exit(40); 'l': Exit(37); 'm': Exit(46); 'n': Exit(45); 'o': Exit(31);
      'p': Exit(35); 'q': Exit(12); 'r': Exit(15); 's': Exit(1);  't': Exit(17);
      'u': Exit(32); 'v': Exit(9);  'w': Exit(13); 'x': Exit(7);  'y': Exit(16);
      'z': Exit(6);
    end;
  end;

  // Dígitos 0-9
  if (Length(K) = 1) and (K[1] >= '0') and (K[1] <= '9') then
  begin
    case K[1] of
      '1': Exit(18); '2': Exit(19); '3': Exit(20); '4': Exit(21); '5': Exit(23);
      '6': Exit(22); '7': Exit(26); '8': Exit(28); '9': Exit(25); '0': Exit(29);
    end;
  end;

  Result := 0;
end;

class procedure TAiMacExecutor.ExecuteCombo(const Combo: string);
var
  Parts: TArray<string>;
  Part: string;
  Flags: CGEventFlags;
  IsMod: Boolean;
  VK: Word;
  MainKeys: TList<Word>;
  I: Integer;
begin
  // En macOS los atajos se expresan como flags de modificador sobre la tecla
  // principal. Acumulamos los modificadores como flags y posteamos las teclas
  // principales con esos flags.
  Parts := Combo.Replace('-', '+').Split(['+']);
  Flags := 0;
  MainKeys := TList<Word>.Create;
  try
    for Part in Parts do
    begin
      VK := GetKeyCode(Part, IsMod);
      if IsMod then
        Flags := Flags or ModifierFlag(Part)
      else if VK <> 0 then
        MainKeys.Add(VK);
    end;

    if MainKeys.Count = 0 then
    begin
      // Solo modificadores: pulsar/soltar el primero con su keycode.
      if Flags <> 0 then
      begin
        // No hay tecla principal; nada que disparar de forma fiable.
        Exit;
      end;
    end;

    for I := 0 to MainKeys.Count - 1 do
    begin
      PostKey(MainKeys[I], True, Flags);
      PostKey(MainKeys[I], False, Flags);
    end;
  finally
    MainKeys.Free;
  end;
end;

class procedure TAiMacExecutor.HoldKeys(const Combo: string; DurationSecs: Double);
var
  Parts: TArray<string>;
  Part: string;
  IsMod: Boolean;
  VK: Word;
  Keys: TList<Word>;
  I: Integer;
begin
  // Mantiene presionada(s) la(s) tecla(s) durante DurationSecs. Presiona todas
  // (modificadores incluidos) por keycode, espera y suelta en orden inverso.
  Parts := Combo.Replace('-', '+').Split(['+']);
  Keys := TList<Word>.Create;
  try
    for Part in Parts do
    begin
      VK := GetKeyCode(Part, IsMod);
      if VK <> 0 then
      begin
        PostKey(VK, True, 0);
        Keys.Add(VK);
      end;
    end;

    if DurationSecs > 0 then
      Sleep(Round(DurationSecs * 1000))
    else
      Sleep(1000);

    for I := Keys.Count - 1 downto 0 do
      PostKey(Keys[I], False, 0);
  finally
    Keys.Free;
  end;
end;

class function TAiMacExecutor.CursorPos: TPointF;
var
  Ev: CGEventRef;
  P: CGPoint;
begin
  Result := TPointF.Create(0, 0);
  Ev := CGEventCreate(nil);
  if Ev = nil then
    Exit;
  try
    P := CGEventGetLocation(Ev);
    Result := TPointF.Create(P.X, P.Y);
  finally
    CFRelease(Ev);
  end;
end;

// -----------------------------------------------------------------------------
// MAIN EXECUTION LOGIC
// -----------------------------------------------------------------------------
class function TAiMacExecutor.Execute(const Action: TAiActionData): TAiActionResult;
var
  I: Integer;
  Flags: CGEventFlags;
  Pt: TPointF;
begin
  Result.Success := True;
  Result.ErrorMessage := '';
  Result.CustomOutput := '';

  try
    Flags := FlagsFromMods(Action.Modifiers);

    case Action.ActionType of
      catHover:
        MoveMouse(Action.X, Action.Y);

      catClick:
      begin
        MoveMouse(Action.X, Action.Y);
        PostMouse(cLeftDown, cBtnLeft, Action.X, Action.Y, 1, Flags);
        PostMouse(cLeftUp,   cBtnLeft, Action.X, Action.Y, 1, Flags);
      end;

      catRightClick:
      begin
        MoveMouse(Action.X, Action.Y);
        PostMouse(cRightDown, cBtnRight, Action.X, Action.Y, 1, Flags);
        PostMouse(cRightUp,   cBtnRight, Action.X, Action.Y, 1, Flags);
      end;

      catMiddleClick:
      begin
        MoveMouse(Action.X, Action.Y);
        PostMouse(cOtherDown, cBtnCenter, Action.X, Action.Y, 1, Flags);
        PostMouse(cOtherUp,   cBtnCenter, Action.X, Action.Y, 1, Flags);
      end;

      catDoubleClick:
      begin
        MoveMouse(Action.X, Action.Y);
        PostMouse(cLeftDown, cBtnLeft, Action.X, Action.Y, 1, Flags);
        PostMouse(cLeftUp,   cBtnLeft, Action.X, Action.Y, 1, Flags);
        PostMouse(cLeftDown, cBtnLeft, Action.X, Action.Y, 2, Flags);
        PostMouse(cLeftUp,   cBtnLeft, Action.X, Action.Y, 2, Flags);
      end;

      catTripleClick:
      begin
        MoveMouse(Action.X, Action.Y);
        for I := 1 to 3 do
        begin
          PostMouse(cLeftDown, cBtnLeft, Action.X, Action.Y, I, Flags);
          PostMouse(cLeftUp,   cBtnLeft, Action.X, Action.Y, I, Flags);
        end;
      end;

      catDrag:
      begin
        MoveMouse(Action.X, Action.Y);
        PostMouse(cLeftDown, cBtnLeft, Action.X, Action.Y, 1, Flags);
        Sleep(80);
        PostMouse(cLeftDragged, cBtnLeft, (Action.X + Action.DestX) / 2, (Action.Y + Action.DestY) / 2, 0, Flags);
        Sleep(50);
        PostMouse(cLeftDragged, cBtnLeft, Action.DestX, Action.DestY, 0, Flags);
        Sleep(80);
        PostMouse(cLeftUp, cBtnLeft, Action.DestX, Action.DestY, 1, Flags);
      end;

      catScroll:
      begin
        MoveMouse(Action.X, Action.Y);
        if (Action.ScrollDirection = 'down') then
          PostScroll(-Action.ScrollAmount, 0, Flags)
        else if (Action.ScrollDirection = 'up') then
          PostScroll(Action.ScrollAmount, 0, Flags)
        else if (Action.ScrollDirection = 'left') then
          PostScroll(0, -Action.ScrollAmount, Flags)
        else if (Action.ScrollDirection = 'right') then
          PostScroll(0, Action.ScrollAmount, Flags);
      end;

      catType:
      begin
        // Pre-click solo si hay posición (Gemini). Claude 'type' llega (0,0) y NO
        // debe clicar: quitaría el foco del control donde hay que escribir.
        if (Action.X <> 0) or (Action.Y <> 0) then
        begin
          MoveMouse(Action.X, Action.Y);
          PostMouse(cLeftDown, cBtnLeft, Action.X, Action.Y, 1, 0);
          PostMouse(cLeftUp,   cBtnLeft, Action.X, Action.Y, 1, 0);
          Sleep(50);
        end;
        TypeUnicode(Action.TextToType);
        if Action.PressEnter then
        begin
          Sleep(50);
          PostKey(36, True, 0);   // Return
          PostKey(36, False, 0);
        end;
      end;

      catKeyCombination:
        ExecuteCombo(Action.KeyCombo);

      catHoldKey:
        HoldKeys(Action.KeyCombo, Action.HoldDuration);

      catCursorPosition:
      begin
        Pt := CursorPos;
        Result.CustomOutput := Format('{"x":%d,"y":%d}', [Round(Pt.X), Round(Pt.Y)]);
      end;

      catZoom:
        ; // Sin input físico: la región (CurrentAction.ZoomRect) se captura ampliada

      catWait:
        Sleep(5000);

      catNavigate:
        ; // En escritorio no hay acción física; el componente principal gestiona la URL
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
// Captura de pantalla (delegada en uMakerAi.Utils.ScreenCapture)
// -----------------------------------------------------------------------------
class procedure TAiMacExecutor.DrawCursorOnBitmap(Bmp: FMX.Graphics.TBitmap; AreaLeft, AreaTop: Integer);
var
  Cur: TPointF;
  RelX, RelY: Single;
  R: TRectF;
begin
  Cur := CursorPos;
  RelX := Cur.X - AreaLeft;
  RelY := Cur.Y - AreaTop;

  if (RelX < 0) or (RelY < 0) or (RelX >= Bmp.Width) or (RelY >= Bmp.Height) then
    Exit;

  if Bmp.Canvas.BeginScene then
  try
    R := TRectF.Create(RelX - 10, RelY - 10, RelX + 10, RelY + 10);
    Bmp.Canvas.Fill.Color := $80FF0000;
    Bmp.Canvas.FillEllipse(R, 1);
    Bmp.Canvas.Stroke.Color := TAlphaColors.White;
    Bmp.Canvas.Stroke.Thickness := 2;
    Bmp.Canvas.DrawEllipse(R, 1);
  finally
    Bmp.Canvas.EndScene;
  end;
end;

class procedure TAiMacExecutor.CaptureScreen(var MediaFile: TAiMediaFile; TargetArea: TRect; Quality: Integer);
var
  Bmp: FMX.Graphics.TBitmap;
  Surf: TBitmapSurface;
  SaveParams: TBitmapCodecSaveParams;
  Stream: TMemoryStream;
  Sz: TPoint;
begin
  if not Assigned(MediaFile) then
    MediaFile := TAiMediaFile.Create;

  // Área por defecto: pantalla principal completa.
  if TargetArea.IsEmpty then
  begin
    Sz := TScreenCapture.GetScreenSize;
    TargetArea := Rect(0, 0, Sz.X, Sz.Y);
  end;

  // Captura nativa macOS (CGDisplayCreateImageForRect) vía TScreenCapture.
  Bmp := TScreenCapture.CaptureArea(TargetArea);
  if not Assigned(Bmp) then
    Exit;

  try
    DrawCursorOnBitmap(Bmp, TargetArea.Left, TargetArea.Top);

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

{$ENDIF}

end.
