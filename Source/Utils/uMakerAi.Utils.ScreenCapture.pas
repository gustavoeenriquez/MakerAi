// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Utils.ScreenCapture;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math, System.UITypes,
  FMX.Graphics, FMX.Types, FMX.Platform, FMX.Forms, FMX.Objects, FMX.Surfaces
  {$IFDEF MSWINDOWS}
  , Winapi.Windows, Winapi.Messages, FMX.Platform.Win
  {$ENDIF}
  {$IFDEF MACOS}
  , Macapi.CoreGraphics, Macapi.CoreFoundation, Macapi.Foundation
  {$ENDIF}
  ;

type
  TScreenCapture = class
  private
    {$IFDEF MSWINDOWS}
    class function CaptureScreenWindows(const ARect: TRect): FMX.Graphics.TBitmap;
    {$ENDIF}
    {$IFDEF MACOS}
    class function CaptureScreenMacOS(const ARect: TRect = TRect.Empty): FMX.Graphics.TBitmap;
    {$ENDIF}
    {$IFDEF ANDROID}
    class function CaptureScreenAndroid(const ARect: TRect = TRect.Empty): FMX.Graphics.TBitmap;
    {$ENDIF}
  public
    // Captura toda la pantalla
    class function CaptureFullScreen: FMX.Graphics.TBitmap;

    // Captura un área específica de la pantalla
    class function CaptureArea(const ARect: TRect): FMX.Graphics.TBitmap;

    // Obtiene las dimensiones de la pantalla
    class function GetScreenSize: TPoint;

    // Guarda el bitmap en un archivo
    class procedure SaveToFile(ABitmap: FMX.Graphics.TBitmap; const AFileName: string);
    class function SelectArea(out ASelectedRect: TRect): Boolean;
  end;


 TSelectionForm = class(TForm)
  private
    IsDragging: Boolean;
    StartPoint, EndPoint: TPointF;
    VirtualScreenLeft, VirtualScreenTop: Integer;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  public
     constructor Create(AOwner: TComponent); override;
    function Execute(out ASelectedRect: TRect): Boolean;
  end;



implementation

{$IFDEF MSWINDOWS}
{
class function TScreenCapture.CaptureScreenWindows(const ARect: TRect): FMX.Graphics.TBitmap;
var
  ScreenDC, MemDC: HDC;
  ScreenBmp: HBITMAP;
  OldBmp: HBITMAP;
  BmpInfo: TBitmapInfo;
  BmpData: Pointer;
  CaptureRect: TRect;
  ScreenWidth, ScreenHeight: Integer;
  FMXBmpData: TBitmapData;
  BytesPerPixel: Integer;
  SrcPtr, DstPtr: PByte;
  x, y: Integer;
begin
  Result := nil;

  // Obtener dimensiones de la pantalla
  ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
  ScreenHeight := GetSystemMetrics(SM_CYSCREEN);

  // Determinar el área a capturar
  if ARect.IsEmpty then
    CaptureRect := TRect.Create(0, 0, ScreenWidth, ScreenHeight)
  else
    CaptureRect := ARect;

  // Validar que el rectángulo esté dentro de los límites
  CaptureRect.Left := Max(0, CaptureRect.Left);
  CaptureRect.Top := Max(0, CaptureRect.Top);
  CaptureRect.Right := Min(ScreenWidth, CaptureRect.Right);
  CaptureRect.Bottom := Min(ScreenHeight, CaptureRect.Bottom);

  if (CaptureRect.Width <= 0) or (CaptureRect.Height <= 0) then
    Exit;

  ScreenDC := GetDC(0);
  try
    MemDC := CreateCompatibleDC(ScreenDC);
    try
      ScreenBmp := CreateCompatibleBitmap(ScreenDC, CaptureRect.Width, CaptureRect.Height);
      try
        OldBmp := SelectObject(MemDC, ScreenBmp);
        try
          // Copiar la pantalla al bitmap
          BitBlt(MemDC, 0, 0, CaptureRect.Width, CaptureRect.Height,
                 ScreenDC, CaptureRect.Left, CaptureRect.Top, SRCCOPY);

          // Preparar la estructura BitmapInfo
          FillChar(BmpInfo, SizeOf(BmpInfo), 0);
          BmpInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
          BmpInfo.bmiHeader.biWidth := CaptureRect.Width;
          BmpInfo.bmiHeader.biHeight := -CaptureRect.Height; // Negativo para top-down
          BmpInfo.bmiHeader.biPlanes := 1;
          BmpInfo.bmiHeader.biBitCount := 32;
          BmpInfo.bmiHeader.biCompression := BI_RGB;

          // Crear el bitmap FMX
          Result := FMX.Graphics.TBitmap.Create(CaptureRect.Width, CaptureRect.Height);

          // Obtener los datos del bitmap de Windows
          BytesPerPixel := 4;
          GetMem(BmpData, CaptureRect.Width * CaptureRect.Height * BytesPerPixel);
          try
            GetDIBits(MemDC, ScreenBmp, 0, CaptureRect.Height, BmpData, BmpInfo, DIB_RGB_COLORS);

            // Mapear el bitmap FMX para escritura
            if Result.Map(TMapAccess.Write, FMXBmpData) then
            try
              SrcPtr := PByte(BmpData);
              DstPtr := PByte(FMXBmpData.Data);

              // Convertir de BGR a RGBA (FMX usa RGBA)
              for y := 0 to CaptureRect.Height - 1 do
              begin
                for x := 0 to CaptureRect.Width - 1 do
                begin
                  // Windows DIB es BGRA, FMX necesita RGBA
                  DstPtr[0] := SrcPtr[2]; // R
                  DstPtr[1] := SrcPtr[1]; // G
                  DstPtr[2] := SrcPtr[0]; // B
                  DstPtr[3] := 255;       // A (opaco)

                  Inc(SrcPtr, 4);
                  Inc(DstPtr, 4);
                end;
              end;
            finally
              Result.Unmap(FMXBmpData);
            end;

          finally
            FreeMem(BmpData);
          end;

        finally
          SelectObject(MemDC, OldBmp);
        end;
      finally
        DeleteObject(ScreenBmp);
      end;
    finally
      DeleteDC(MemDC);
    end;
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;
}



class function TScreenCapture.CaptureScreenWindows(const ARect: TRect): FMX.Graphics.TBitmap;
var
  ScreenDC, MemDC: HDC;
  HBmp, OldBmp: HBITMAP;
  BmpInfo: TBitmapInfo;
  BmpData: Pointer;
  CaptureRect: TRect;
  VirtualLeft, VirtualTop, VirtualWidth, VirtualHeight: Integer;
  BytesPerPixel, DataSize, PixelCount, i: Integer;
  Surface: TBitmapSurface;
  pByte: System.PByte;
begin
  Result := nil;

  // Métricas de pantalla virtual (soporte multi-monitor)
  VirtualLeft := GetSystemMetrics(SM_XVIRTUALSCREEN);
  VirtualTop := GetSystemMetrics(SM_YVIRTUALSCREEN);
  VirtualWidth := GetSystemMetrics(SM_CXVIRTUALSCREEN);
  VirtualHeight := GetSystemMetrics(SM_CYVIRTUALSCREEN);

  if ARect.IsEmpty then
    CaptureRect := TRect.Create(VirtualLeft, VirtualTop, VirtualLeft + VirtualWidth, VirtualTop + VirtualHeight)
  else
    CaptureRect := ARect;

  // Recortar al área virtual
  CaptureRect.Left := Max(VirtualLeft, CaptureRect.Left);
  CaptureRect.Top := Max(VirtualTop, CaptureRect.Top);
  CaptureRect.Right := Min(VirtualLeft + VirtualWidth, CaptureRect.Right);
  CaptureRect.Bottom := Min(VirtualTop + VirtualHeight, CaptureRect.Bottom);

  if (CaptureRect.Width <= 0) or (CaptureRect.Height <= 0) then
    Exit;

  ScreenDC := GetDC(0);
  try
    MemDC := CreateCompatibleDC(ScreenDC);
    try
      HBmp := CreateCompatibleBitmap(ScreenDC, CaptureRect.Width, CaptureRect.Height);
      try
        OldBmp := SelectObject(MemDC, HBmp);
        try
          // Copiar desde la pantalla virtual (puede tener coordenadas negativas)
          BitBlt(MemDC, 0, 0, CaptureRect.Width, CaptureRect.Height,
                 ScreenDC, CaptureRect.Left, CaptureRect.Top, SRCCOPY);

          // Preparar BITMAPINFO para 32bpp top-down
          FillChar(BmpInfo, SizeOf(BmpInfo), 0);
          BmpInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
          BmpInfo.bmiHeader.biWidth := CaptureRect.Width;
          BmpInfo.bmiHeader.biHeight := -CaptureRect.Height; // top-down
          BmpInfo.bmiHeader.biPlanes := 1;
          BmpInfo.bmiHeader.biBitCount := 32;
          BmpInfo.bmiHeader.biCompression := BI_RGB;

          BytesPerPixel := 4;
          PixelCount := CaptureRect.Width * CaptureRect.Height;
          DataSize := PixelCount * BytesPerPixel;

          GetMem(BmpData, DataSize);
          try
            if GetDIBits(MemDC, HBmp, 0, CaptureRect.Height, BmpData, BmpInfo, DIB_RGB_COLORS) = 0 then
              Exit; // GetDIBits falló

            // Asegurar alfa opaco (GetDIBits suele dejar alfa = 0 o basura)
            pByte := System.PByte(BmpData);
            for i := 0 to PixelCount - 1 do
            begin
              pByte[3] := 255; // A = 255
              Inc(pByte, 4);
            end;

            // Crear TBitmapSurface y copiar los bytes (formato BGRA)
            Surface := TBitmapSurface.Create;
            try
              Surface.SetSize(CaptureRect.Width, CaptureRect.Height, TPixelFormat.BGRA);
              // Copia en bloque (Surface.Bits apunta al buffer interno)
              Move(BmpData^, Surface.Bits^, DataSize);

              // Convertir a TBitmap FMX (Assign hace la conversión nativa)
              Result := FMX.Graphics.TBitmap.Create;
              Result.Assign(Surface);

            finally
              Surface.Free;
            end;

          finally
            FreeMem(BmpData);
          end;

        finally
          // restaurar el bitmap original seleccionado
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

{$IFDEF MACOS}
class function TScreenCapture.CaptureScreenMacOS(const ARect: TRect): FMX.Graphics.TBitmap;
var
  ImageRef: CGImageRef;
  CaptureRect: CGRect;
  ScreenBounds: CGRect;
  DataProvider: CGDataProviderRef;
  Data: CFDataRef;
  BmpData: TBitmapData;
  i, j: Integer;
  SrcPtr, DstPtr: PByte;
begin
  Result := nil;

  // Obtener las dimensiones de la pantalla principal
  ScreenBounds := CGDisplayBounds(CGMainDisplayID);

  // Determinar el área a capturar
  if ARect.IsEmpty then
    CaptureRect := ScreenBounds
  else
  begin
    CaptureRect.origin.x := ARect.Left;
    CaptureRect.origin.y := ARect.Top;
    CaptureRect.size.width := ARect.Width;
    CaptureRect.size.height := ARect.Height;
  end;

  // Capturar la imagen
  ImageRef := CGDisplayCreateImageForRect(CGMainDisplayID, CaptureRect);
  if ImageRef = nil then
    Exit;

  try
    DataProvider := CGImageGetDataProvider(ImageRef);
    if DataProvider = nil then
      Exit;

    Data := CGDataProviderCopyData(DataProvider);
    if Data = nil then
      Exit;

    try
      Result := FMX.Graphics.TBitmap.Create(Round(CaptureRect.size.width), Round(CaptureRect.size.height));

      if Result.Map(TMapAccess.Write, BmpData) then
      try
        SrcPtr := CFDataGetBytePtr(Data);
        DstPtr := BmpData.Data;

        // Convertir de ARGB a RGBA (formato FMX)
        for i := 0 to Result.Height - 1 do
        begin
          for j := 0 to Result.Width - 1 do
          begin
            DstPtr[0] := SrcPtr[2]; // R
            DstPtr[1] := SrcPtr[1]; // G
            DstPtr[2] := SrcPtr[0]; // B
            DstPtr[3] := SrcPtr[3]; // A

            Inc(SrcPtr, 4);
            Inc(DstPtr, 4);
          end;
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

{$IFDEF ANDROID}
class function TScreenCapture.CaptureScreenAndroid(const ARect: TRect): FMX.Graphics.TBitmap;
begin
  // En Android, la captura de pantalla requiere permisos especiales
  // y generalmente se hace a través de Media Projection API
  // Esta es una implementación básica que requiere permisos de root
  // o implementación nativa más compleja
  Result := nil;
  // TODO: Implementar captura para Android
  raise Exception.Create('Screen capture not implemented for Android platform');
end;
{$ENDIF}

class function TScreenCapture.CaptureFullScreen: FMX.Graphics.TBitmap;
begin
  {$IFDEF MSWINDOWS}
  Result := CaptureScreenWindows(TRect.Create(0,0,0,0));
  {$ENDIF}
  {$IFDEF MACOS}
  Result := CaptureScreenMacOS;
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := CaptureScreenAndroid;
  {$ENDIF}
  {$IFDEF IOS}
  // iOS no permite captura de pantalla del sistema por seguridad
  Result := nil;
  {$ENDIF}
end;

class function TScreenCapture.CaptureArea(const ARect: TRect): FMX.Graphics.TBitmap;
begin
  {$IFDEF MSWINDOWS}
  Result := CaptureScreenWindows(ARect);
  {$ENDIF}
  {$IFDEF MACOS}
  Result := CaptureScreenMacOS(ARect);
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := CaptureScreenAndroid(ARect);
  {$ENDIF}
  {$IFDEF IOS}
  Result := nil;
  {$ENDIF}
end;

class function TScreenCapture.GetScreenSize: TPoint;
begin
  {$IFDEF MSWINDOWS}
  Result.X := GetSystemMetrics(SM_CXSCREEN);
  Result.Y := GetSystemMetrics(SM_CYSCREEN);
  {$ENDIF}
  {$IFDEF MACOS}
  var Bounds := CGDisplayBounds(CGMainDisplayID);
  Result.X := Round(Bounds.size.width);
  Result.Y := Round(Bounds.size.height);
  {$ENDIF}
  {$IFDEF ANDROID}
  // Implementación para Android requeriría JNI
  Result := TPoint.Zero;
  {$ENDIF}
  {$IFDEF IOS}
  Result := TPoint.Zero;
  {$ENDIF}
end;

class procedure TScreenCapture.SaveToFile(ABitmap: FMX.Graphics.TBitmap; const AFileName: string);
begin
  if Assigned(ABitmap) then
    ABitmap.SaveToFile(AFileName);
end;


class function TScreenCapture.SelectArea(out ASelectedRect: TRect): Boolean;
var
  F: TSelectionForm;
begin
  F := TSelectionForm.Create(nil);
  try
    Result := F.Execute(ASelectedRect);
  finally
    F.Free;
  end;
End;


{ TSelectionForm }

procedure TSelectionForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
  begin
    IsDragging := True;
    StartPoint := TPointF.Create(X, Y);
    EndPoint := StartPoint;
  end
  else
  begin
    ModalResult := mrCancel;
    Close;
  end;
end;

procedure TSelectionForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if IsDragging then
  begin
    EndPoint := TPointF.Create(X, Y);
    Invalidate;
  end;
end;

procedure TSelectionForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (Button = TMouseButton.mbLeft) and IsDragging then
  begin
    IsDragging := False;
    if (Abs(EndPoint.X - StartPoint.X) > 2) and (Abs(EndPoint.Y - StartPoint.Y) > 2) then
      ModalResult := mrOk
    else
      ModalResult := mrCancel;
  end;
end;

procedure TSelectionForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkEscape then
  begin
    ModalResult := mrCancel;
    Close;
  end;
end;

procedure TSelectionForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  PaintRect: TRectF;
begin
  if IsDragging then
  begin
    PaintRect := TRectF.Create(
      Min(StartPoint.X, EndPoint.X),
      Min(StartPoint.Y, EndPoint.Y),
      Max(StartPoint.X, EndPoint.X),
      Max(StartPoint.Y, EndPoint.Y)
    );

    Canvas.Fill.Color := TAlphaColors.Null;
    Canvas.FillRect(PaintRect, 0, 0, [], 1);

    Canvas.Stroke.Color := TAlphaColors.White;
    Canvas.Stroke.Thickness := 1;
    Canvas.DrawRect(PaintRect, 0, 0, [], 1);
  end;
end;

constructor TSelectionForm.Create(AOwner: TComponent);
Var
  R : TRectangle;
begin
  inherited CreateNew(AOwner); // 👈 evita buscar .fmx/.dfm

  // configuración básica que antes hacías en Execute
  BorderStyle := TFmxFormBorderStyle.None;
  FormStyle := TFormStyle.StayOnTop;
  Fill.Color := TAlphaColorRec.Black;
  Transparency := True;
  Cursor := crCross;

  R := TRectangle.Create(Self);
  R.Align := TAlignLayout.Client;
  R.Opacity := 0.3;
  R.Fill.Color := TAlphaColorRec.Black;
  R.HitTest := False;

  Self.AddObject(R);



  OnMouseDown := FormMouseDown;
  OnMouseMove := FormMouseMove;
  OnMouseUp   := FormMouseUp;
  OnKeyDown   := FormKeyDown;
  OnPaint     := FormPaint;
end;

function TSelectionForm.Execute(out ASelectedRect: TRect): Boolean;
begin
  IsDragging := False;

  VirtualScreenLeft := GetSystemMetrics(SM_XVIRTUALSCREEN);
  VirtualScreenTop  := GetSystemMetrics(SM_YVIRTUALSCREEN);

  SetBounds(
    VirtualScreenLeft,
    VirtualScreenTop,
    GetSystemMetrics(SM_CXVIRTUALSCREEN),
    GetSystemMetrics(SM_CYVIRTUALSCREEN)
  );


  if ShowModal = mrOk then
  begin
    ASelectedRect := TRect.Create(
      VirtualScreenLeft + Round(Min(StartPoint.X, EndPoint.X)),
      VirtualScreenTop  + Round(Min(StartPoint.Y, EndPoint.Y)),
      VirtualScreenLeft + Round(Max(StartPoint.X, EndPoint.X)),
      VirtualScreenTop  + Round(Max(StartPoint.Y, EndPoint.Y))
    );
    Result := True;
  end
  else
    Result := False;
end;





end.
