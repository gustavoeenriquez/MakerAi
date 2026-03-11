unit uMainDalleGenerar;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Objects, FMX.ListBox, FMX.Edit, uMakerAi.Core, uMakerAi.OpenAi.Dalle;

type
  TForm76 = class(TForm)
    MainLayout: TLayout;
    Layout3: TLayout;
    Layout1: TLayout;
    Rectangle1: TRectangle;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    Layout2: TLayout;
    Image1: TImage;
    Label2: TLabel;
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
    ListBoxItem6: TListBoxItem;
    Label10: TLabel;
    EditImageSize: TComboBox;
    SaveDialog1: TSaveDialog;
    Layout5: TLayout;
    Layout6: TLayout;
    Label4: TLabel;
    MemoPrompt: TMemo;
    Label8: TLabel;
    MemoPromptNegativo: TMemo;
    Layout7: TLayout;
    BtnSend: TButton;
    Image2: TImage;
    Label9: TLabel;
    BtnLoadImage: TButton;
    Image3: TImage;
    Label11: TLabel;
    OpenDialog1: TOpenDialog;
    ListBoxItem5: TListBoxItem;
    Label12: TLabel;
    ChUserRefiner: TCheckBox;
    ChEnhanceFace: TCheckBox;
    ChAutoUpscale: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure BtnLoadImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTmpSourceImage: TMemoryStream;
  public
    { Public declarations }
  end;

var
  Form76: TForm76;

implementation

{$R *.fmx}

procedure TForm76.BtnLoadImageClick(Sender: TObject);
begin
  If OpenDialog1.Execute then
  Begin
    If Assigned(FTmpSourceImage) then
      FreeAndNil(FTmpSourceImage);

    FTmpSourceImage := TMemoryStream.Create;
    FTmpSourceImage.LoadFromFile(OpenDialog1.FileName);
  End;
end;

procedure TForm76.BtnSendClick(Sender: TObject);
Var
  AiSize: TAiImageSize;
  Prompt, NegativePrompt, Revised: String;
  Res: TAiDalleImage;
begin
  AiDalle.ApiKey := EditApiKey.Text;

  If EditResponseFormat.Selected.Text = 'tiaRUrl' then
    AiDalle.ResponseFormat := irfUrl
  Else
    AiDalle.ResponseFormat := irfBase64Json;

  If EditStyleFormat.ItemIndex = 0 then
    AiDalle.Style := isNatural
  Else
    AiDalle.Style := isVivid;

  Case EditImageSize.ItemIndex of
    0:
      AiSize := TAiImageSize.is1024x1024; // Soportado por TODOS los modelos
    1:
      AiSize := TAiImageSize.is1792x1024; // Solo para DALL-E 3 (Formato panor�mico/horizontal)
    2:
      AiSize := TAiImageSize.is1024x1792; // Solo para DALL-E 3 (Formato retrato/vertical)
    3:
      AiSize := TAiImageSize.is1536x1024; // Solo para gpt-image-1 (Formato panor�mico/horizontal)
    4:
      AiSize := TAiImageSize.is1024x1536; // Solo para gpt-image-1 (Formato retrato/vertical)
  End;


  AiDalle.EnhanceFace := ChEnhanceFace.IsChecked;
  AiDalle.AutoUpscale := ChAutoUpscale.IsChecked;
  AIDalle.UseRefiner := ChUserRefiner.IsChecked;

  //Faltar�a por implementar

  {
    property Steps: Integer read FSteps write FSteps default 30;
    property GuidanceScale: Single read FGuidanceScale write FGuidanceScale;
    property Seed: Int64 read FSeed write FSeed Default -1;
    Property Strength: Single read FStrength write SetStrength;
    Property LoraPath: String read FLoraPath write SetLoraPath;
}


  Prompt := MemoPrompt.Lines.Text;
  NegativePrompt := MemoPromptNegativo.Lines.Text;

  Res := AiDalle.Generate(Prompt, NegativePrompt, AiSize, 1, FTmpSourceImage);

  Image1.Bitmap.LoadFromStream(Res.Image);

  If Assigned(FTmpSourceImage) then
    FreeAndNil(FTmpSourceImage);

end;

procedure TForm76.FormCreate(Sender: TObject);
begin
  FTmpSourceImage := Nil;
end;

procedure TForm76.SpeedButton1Click(Sender: TObject);
begin
  If SaveDialog1.Execute then
  Begin
    Image1.Bitmap.SaveToFile(SaveDialog1.FileName);
  End;
end;

end.
