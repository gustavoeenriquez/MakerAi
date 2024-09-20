unit uMainDalleEdicion;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Objects, uMakerAi.Dalle, FMX.ListBox, FMX.Edit, uMakerAi.Core;

type
  TForm76 = class(TForm)
    MainLayout: TLayout;
    Layout3: TLayout;
    BtnPlay: TSpeedButton;
    Layout1: TLayout;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Layout2: TLayout;
    AiDalle: TAiDalle;
    Layout4: TLayout;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    Label3: TLabel;
    EditApiKey: TEdit;
    ListBoxItem2: TListBoxItem;
    Label5: TLabel;
    EditQuality: TComboBox;
    ListBoxItem3: TListBoxItem;
    Label6: TLabel;
    EditResponseFormat: TComboBox;
    ListBoxItem4: TListBoxItem;
    Label7: TLabel;
    EditStyleFormat: TComboBox;
    ListBoxItem5: TListBoxItem;
    Label8: TLabel;
    EditUseDalle3: TComboBox;
    ListBoxItem6: TListBoxItem;
    Label10: TLabel;
    EditImageSize: TComboBox;
    Splitter1: TSplitter;
    Label2: TLabel;
    Layout6: TLayout;
    Image1: TImage;
    Label11: TLabel;
    Layout7: TLayout;
    Image2: TImage;
    Label12: TLabel;
    OpenDialog1: TOpenDialog;
    SpeedButton1: TSpeedButton;
    Selection1: TSelection;
    Layout5: TLayout;
    MemoPrompt: TMemo;
    Label4: TLabel;
    procedure BtnPlayClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure MakeTransparentArea(Bitmap: TBitmap; P1, P2: TPointF);
    Function CreaMascara: TMemoryStream;
    procedure AjustarSeleccionFit(RecOrigen: TRectF; var RecDestino: TRectF);
    function EscalarDimensiones(AnchoOriginal, AltoOriginal, MaxValor: Single): TPointF;

  end;

var
  Form76: TForm76;

implementation

{$R *.fmx}

procedure TForm76.BtnPlayClick(Sender: TObject);
Var
  AiSize: TAiImageSize;
  Prompt, Revised: String;
  Res: TAiDalleFile;
  aImage, aMask: TMemoryStream;
begin
  AiDalle.ApiKey := EditApiKey.Text;
  AiDalle.HdQuality := EditQuality.ItemIndex = 0; // 0=Hd 1= Low

  If EditResponseFormat.Text = 'tiaRUrl' then
    AiDalle.ResponseFormat := tiaRUrl
  Else
    AiDalle.ResponseFormat := tiaRB64;

  If EditStyleFormat.ItemIndex = 0 then
    AiDalle.StyleFormat := tiaStyleNatural
  Else
    AiDalle.StyleFormat := tiaStyleVivid;

  AiDalle.UseDalle3 := EditUseDalle3.ItemIndex = 1;

  Case EditImageSize.ItemIndex of
    0:
      AiSize := TAiImageSize.TiaSize256;
    1:
      AiSize := TAiImageSize.TiaSize512;
    2:
      AiSize := TAiImageSize.TiaSize1024;
    3:
      AiSize := TAiImageSize.TiaSize1024_1792;
    4:
      AiSize := TAiImageSize.TiaSize1792_1024;
  End;

  Prompt := MemoPrompt.Lines.Text;

  aImage := TMemoryStream.Create;
  aMask := TMemoryStream.Create;

  Try

    Image1.Bitmap.SaveToStream(aImage);
    aImage.Position := 0;

    aMask := CreaMascara;
    aMask.Position := 0;

    Res := AiDalle.Edit(aImage, aMask, Prompt, AiSize, 1);
  Finally
    aImage.Free;
    aMask.Free;
  End;

  Image2.Bitmap.LoadFromStream(Res.Image);
end;

procedure TForm76.MakeTransparentArea(Bitmap: TBitmap; P1, P2: TPointF);
var
  x, y: Integer;
  PixelColor: TAlphaColor;
  RectArea: TRectF;
  BitMapData: TBitMapData;
begin
  if Assigned(Bitmap) then
  begin
    // Asegúrate de que el bitmap tenga acceso a los píxeles
    Bitmap.Map(TMapAccess.ReadWrite, BitMapData);

    try
      // Define el rectángulo con los puntos P1 y P2
      RectArea := TRectF.Create(P1, P2);
      RectArea.NormalizeRect; // Normaliza el rectángulo para asegurarse que los puntos estén correctos

      // Recorre los píxeles dentro del área definida
      for y := Round(RectArea.Top) to Round(RectArea.Bottom) do
      begin
        for x := Round(RectArea.Left) to Round(RectArea.Right) do
        begin
          BitMapData.SetPixel(x, y, 0);
        end;
      end;

    finally
      // Libera el acceso a los píxeles
      Bitmap.Unmap(BitMapData);
    end;
  end;
end;

function TForm76.EscalarDimensiones(AnchoOriginal, AltoOriginal, MaxValor: Single): TPointF;
var
  Escala: Single;
begin
  // Verificar si se debe escalar
  if (AnchoOriginal <= MaxValor) and (AltoOriginal <= MaxValor) then
  begin
    // No es necesario escalar, las dimensiones originales son menores o iguales al máximo
    Result := TPointF.Create(AnchoOriginal, AltoOriginal);
  end
  else
  begin
    // Determinar cuál dimensión es la más grande y calcular la escala
    if AnchoOriginal > AltoOriginal then
      Escala := MaxValor / AnchoOriginal // Escalar basado en el ancho
    else
      Escala := MaxValor / AltoOriginal; // Escalar basado en el alto

    // Calcular las nuevas dimensiones escaladas
    Result := TPointF.Create(AnchoOriginal * Escala, AltoOriginal * Escala);
  end;
end;

procedure TForm76.AjustarSeleccionFit(RecOrigen: TRectF; var RecDestino: TRectF);
var
  ImagenRealWidth, ImagenRealHeight: Single;
  EscalaX, EscalaY: Single;
  ImagenEscaladaWidth, ImagenEscaladaHeight: Single;
  OffsetX, OffsetY: Single;
begin
  // Tamaño original del Bitmap de la imagen
  ImagenRealWidth := Image1.Bitmap.Width;
  ImagenRealHeight := Image1.Bitmap.Height;

  // Calcular la escala en X e Y
  EscalaX := Image1.Width / ImagenRealWidth;
  EscalaY := Image1.Height / ImagenRealHeight;

  // Usar la menor escala para mantener la relación de aspecto
  if EscalaX < EscalaY then
    EscalaY := EscalaX
  else
    EscalaX := EscalaY;

  // Calcular el tamaño escalado de la imagen
  ImagenEscaladaWidth := ImagenRealWidth * EscalaX;
  ImagenEscaladaHeight := ImagenRealHeight * EscalaY;

  // Calcular los offsets (espacio sobrante en los bordes)
  OffsetX := (Image1.Width - ImagenEscaladaWidth) / 2;
  OffsetY := (Image1.Height - ImagenEscaladaHeight) / 2;

  // Ahora convertimos RecOrigen al área dentro del bitmap original (RecDestino)
  RecDestino.Left := (RecOrigen.Left - OffsetX) / EscalaX;
  RecDestino.Top := (RecOrigen.Top - OffsetY) / EscalaY;
  RecDestino.Right := (RecOrigen.Right - OffsetX) / EscalaX;
  RecDestino.Bottom := (RecOrigen.Bottom - OffsetY) / EscalaY;
end;

function TForm76.CreaMascara: TMemoryStream;
Var
  Bitmap: TBitmap;
  P1, P2: TPointF;
  RecOrigen, RecDestino: TRectF;
begin
  Bitmap := TBitmap.Create;

  Try
    Bitmap.Assign(Image1.Bitmap);

    RecOrigen.TopLeft := Selection1.Position.Point;
    RecOrigen.BottomRight := TPointF.Create(Selection1.Position.x + Selection1.Width, Selection1.Position.y + Selection1.Height);

    AjustarSeleccionFit(RecOrigen, RecDestino);

    MakeTransparentArea(Bitmap, RecDestino.TopLeft, RecDestino.BottomRight);

    Result := TMemoryStream.Create;
    Bitmap.SaveToStream(Result);
    Result.Position := 0;
  Finally
    Bitmap.Free;
  End;
end;

procedure TForm76.SpeedButton1Click(Sender: TObject);
Var
  Bitmap, Bm1: TBitmap;
  P : TPointF;
begin
  If OpenDialog1.Execute then
  Begin
    Bitmap := TBitmap.Create;
    Try
      Bitmap.LoadFromFile(OpenDialog1.FileName);
      P := EscalarDimensiones(BitMap.Width, BitMap.Height, 512);
      Bm1 := BitMap.CreateThumbnail(Round(P.X), Round(P.Y));
      Try
      Image1.Bitmap.Assign(Bm1);
      Finally
        Bm1.Free;
      End;
    Finally
      Bitmap.Free;
    End;
  End;
end;

end.
