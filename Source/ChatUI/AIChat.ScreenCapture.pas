unit AIChat.ScreenCapture;

// MIT License — Gustavo Enríquez <gustavoeenriquez@gmail.com>
// Screen-capture utilities for TAIChatInput.

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math, System.UITypes,
  FMX.Graphics, FMX.Types, FMX.Platform, FMX.Forms, FMX.Objects, FMX.Surfaces,
  FMX.Controls;

type
  TSelectionForm = class(TForm)
  private
    FStartPt  : TPointF;
    FEndPt    : TPointF;
    FDragging : Boolean;
    FSelected : Boolean;
    FSelRect  : TRectF;
    FOverlay  : TRectangle;
    FSelBox   : TRectangle;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure UpdateSelBox;
  public
    constructor Create(AOwner: TComponent); override;
    property Selected : Boolean read FSelected;
    property SelRect  : TRectF  read FSelRect;
  end;

  TScreenCapture = class
  private
    {$IFDEF MSWINDOWS}
    class function CaptureWindows(const ARect: TRect): TBitmap;
    {$ENDIF}
    {$IFDEF MACOS}
    class function CaptureMacOS(const ARect: TRect): TBitmap;
    {$ENDIF}
  public
    class function CaptureFullScreen: TBitmap;
    class function CaptureArea(const ARect: TRect): TBitmap;
    class function GetScreenSize: TPoint;
    class function SelectArea(out ASelectedRect: TRect): Boolean;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;
{$ENDIF}
{$IFDEF MACOS}
uses
  Macapi.CoreGraphics, Macapi.CoreFoundation, Macapi.Foundation;
{$ENDIF}

{ TSelectionForm }

constructor TSelectionForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  BorderStyle  := TFmxFormBorderStyle.None;
  Transparency := True;
  FormStyle    := TFormStyle.StayOnTop;
  Left   := 0;
  Top    := 0;
  Width  := Round(Screen.Width);
  Height := Round(Screen.Height);

  FOverlay             := TRectangle.Create(Self);
  FOverlay.Parent      := Self;
  FOverlay.Align       := TAlignLayout.Contents;
  FOverlay.Fill.Color  := $AA000000;
  FOverlay.Stroke.Kind := TBrushKind.None;
  FOverlay.HitTest     := False;

  FSelBox                  := TRectangle.Create(Self);
  FSelBox.Parent           := Self;
  FSelBox.Fill.Kind        := TBrushKind.None;
  FSelBox.Stroke.Color     := TAlphaColors.White;
  FSelBox.Stroke.Thickness := 2;
  FSelBox.Visible          := False;
  FSelBox.HitTest          := False;

  OnMouseDown := FormMouseDown;
  OnMouseMove := FormMouseMove;
  OnMouseUp   := FormMouseUp;
  OnKeyDown   := FormKeyDown;
  Cursor      := crCross;
end;

procedure TSelectionForm.UpdateSelBox;
var
  R: TRectF;
begin
  R := TRectF.Create(
    Min(FStartPt.X, FEndPt.X), Min(FStartPt.Y, FEndPt.Y),
    Max(FStartPt.X, FEndPt.X), Max(FStartPt.Y, FEndPt.Y));
  FSelBox.Position.X := R.Left;
  FSelBox.Position.Y := R.Top;
  FSelBox.Width      := R.Width;
  FSelBox.Height     := R.Height;
  FSelBox.Visible    := (R.Width > 4) and (R.Height > 4);
end;

procedure TSelectionForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
  begin
    FStartPt        := TPointF.Create(X, Y);
    FEndPt          := FStartPt;
    FDragging       := True;
    FSelBox.Visible := False;
  end;
end;

procedure TSelectionForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  if FDragging then
  begin
    FEndPt := TPointF.Create(X, Y);
    UpdateSelBox;
  end;
end;

procedure TSelectionForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if FDragging and (Button = TMouseButton.mbLeft) then
  begin
    FEndPt    := TPointF.Create(X, Y);
    FDragging := False;
    FSelRect  := TRectF.Create(
      Min(FStartPt.X, FEndPt.X), Min(FStartPt.Y, FEndPt.Y),
      Max(FStartPt.X, FEndPt.X), Max(FStartPt.Y, FEndPt.Y));
    FSelected := (FSelRect.Width > 4) and (FSelRect.Height > 4);
    Close;
  end;
end;

procedure TSelectionForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 27 then  // Escape
  begin
    FSelected := False;
    Close;
  end;
end;

{ ── Windows ──────────────────────────────────────────────────────────────── }

{$IFDEF MSWINDOWS}

class function TScreenCapture.CaptureWindows(const ARect: TRect): FMX.Graphics.TBitmap;
var
  ScreenDC, MemDC : HDC;
  HBmp, OldBmp   : HBITMAP;
  BmpInfo         : TBitmapInfo;
  BmpData         : Pointer;
  CaptureRect     : TRect;
  VLeft, VTop, VW, VH: Integer;
  BytesPerPx, DataSize, PixCount, I: Integer;
  Surface         : TBitmapSurface;
  P               : System.PByte;
begin
  Result := nil;
  VLeft := GetSystemMetrics(SM_XVIRTUALSCREEN);
  VTop  := GetSystemMetrics(SM_YVIRTUALSCREEN);
  VW    := GetSystemMetrics(SM_CXVIRTUALSCREEN);
  VH    := GetSystemMetrics(SM_CYVIRTUALSCREEN);

  if ARect.IsEmpty then
    CaptureRect := TRect.Create(VLeft, VTop, VLeft + VW, VTop + VH)
  else
    CaptureRect := ARect;

  CaptureRect.Left   := Max(VLeft, CaptureRect.Left);
  CaptureRect.Top    := Max(VTop,  CaptureRect.Top);
  CaptureRect.Right  := Min(VLeft + VW, CaptureRect.Right);
  CaptureRect.Bottom := Min(VTop  + VH, CaptureRect.Bottom);
  if (CaptureRect.Width <= 0) or (CaptureRect.Height <= 0) then Exit;

  ScreenDC := GetDC(0);
  try
    MemDC := CreateCompatibleDC(ScreenDC);
    try
      HBmp := CreateCompatibleBitmap(ScreenDC, CaptureRect.Width, CaptureRect.Height);
      try
        OldBmp := SelectObject(MemDC, HBmp);
        try
          BitBlt(MemDC, 0, 0, CaptureRect.Width, CaptureRect.Height,
                 ScreenDC, CaptureRect.Left, CaptureRect.Top, SRCCOPY);

          FillChar(BmpInfo, SizeOf(BmpInfo), 0);
          BmpInfo.bmiHeader.biSize        := SizeOf(TBitmapInfoHeader);
          BmpInfo.bmiHeader.biWidth       := CaptureRect.Width;
          BmpInfo.bmiHeader.biHeight      := -CaptureRect.Height;
          BmpInfo.bmiHeader.biPlanes      := 1;
          BmpInfo.bmiHeader.biBitCount    := 32;
          BmpInfo.bmiHeader.biCompression := BI_RGB;

          PixCount   := CaptureRect.Width * CaptureRect.Height;
          BytesPerPx := 4;
          DataSize   := PixCount * BytesPerPx;
          GetMem(BmpData, DataSize);
          try
            if GetDIBits(MemDC, HBmp, 0, CaptureRect.Height,
                         BmpData, BmpInfo, DIB_RGB_COLORS) = 0 then Exit;
            P := System.PByte(BmpData);
            for I := 0 to PixCount - 1 do
            begin
              P[3] := 255;
              Inc(P, 4);
            end;
            Surface := TBitmapSurface.Create;
            try
              Surface.SetSize(CaptureRect.Width, CaptureRect.Height, TPixelFormat.BGRA);
              Move(BmpData^, Surface.Bits^, DataSize);
              Result := FMX.Graphics.TBitmap.Create;
              Result.Assign(Surface);
            finally
              Surface.Free;
            end;
          finally
            FreeMem(BmpData);
          end;
        finally
          SelectObject(MemDC, OldBmp);
        end;
      finally
        DeleteObject(HBmp);
      end;
    finally
      DeleteDC(MemDC);
    end;
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;

{$ENDIF}

{ ── macOS implementation ─────────────────────────────────────────────────── }

{$IFDEF MACOS}
class function TScreenCapture.CaptureMacOS(const ARect: TRect): FMX.Graphics.TBitmap;
var
  ImageRef    : CGImageRef;
  CaptureRect : CGRect;
  ScreenBounds: CGRect;
  DataProvider: CGDataProviderRef;
  Data        : CFDataRef;
  BmpData     : TBitmapData;
  I, J        : Integer;
  SrcP, DstP  : PByte;
begin
  Result := nil;
  ScreenBounds := CGDisplayBounds(CGMainDisplayID);
  if ARect.IsEmpty then
    CaptureRect := ScreenBounds
  else
  begin
    CaptureRect.origin.x    := ARect.Left;
    CaptureRect.origin.y    := ARect.Top;
    CaptureRect.size.width  := ARect.Width;
    CaptureRect.size.height := ARect.Height;
  end;
  ImageRef := CGDisplayCreateImageForRect(CGMainDisplayID, CaptureRect);
  if ImageRef = nil then Exit;
  try
    DataProvider := CGImageGetDataProvider(ImageRef);
    if DataProvider = nil then Exit;
    Data := CGDataProviderCopyData(DataProvider);
    if Data = nil then Exit;
    try
      Result := FMX.Graphics.TBitmap.Create(Round(CaptureRect.size.width),
                                             Round(CaptureRect.size.height));
      if Result.Map(TMapAccess.Write, BmpData) then
      try
        SrcP := CFDataGetBytePtr(Data);
        DstP := BmpData.Data;
        for I := 0 to Result.Height - 1 do
          for J := 0 to Result.Width - 1 do
          begin
            DstP[0] := SrcP[2]; DstP[1] := SrcP[1];
            DstP[2] := SrcP[0]; DstP[3] := SrcP[3];
            Inc(SrcP, 4); Inc(DstP, 4);
          end;
      finally
        Result.Unmap(BmpData);
      end;
    finally
      CFRelease(Data);
    end;
  finally
    CGImageRelease(ImageRef);
  end;
end;
{$ENDIF}

{ ── Public API ────────────────────────────────────────────────────────────── }

class function TScreenCapture.CaptureFullScreen: FMX.Graphics.TBitmap;
begin
  Result := CaptureArea(TRect.Empty);
end;

class function TScreenCapture.CaptureArea(const ARect: TRect): FMX.Graphics.TBitmap;
begin
  {$IFDEF MSWINDOWS}
  Result := CaptureWindows(ARect);
  {$ELSE}
  {$IFDEF MACOS}
  Result := CaptureMacOS(ARect);
  {$ELSE}
  Result := nil;
  {$ENDIF}
  {$ENDIF}
end;

class function TScreenCapture.GetScreenSize: TPoint;
begin
  {$IFDEF MSWINDOWS}
  Result.X := GetSystemMetrics(SM_CXSCREEN);
  Result.Y := GetSystemMetrics(SM_CYSCREEN);
  {$ELSE}
  Result := TPoint.Zero;
  {$ENDIF}
end;

class function TScreenCapture.SelectArea(out ASelectedRect: TRect): Boolean;
var
  F: TSelectionForm;
begin
  Result := False;
  ASelectedRect := TRect.Empty;
  F := TSelectionForm.Create(nil);
  try
    F.ShowModal;
    if F.Selected then
    begin
      ASelectedRect := F.SelRect.Round;
      Result := not ASelectedRect.IsEmpty;
    end;
  finally
    F.Free;
  end;
end;

end.
