unit uMainDalleVariation;

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
    procedure BtnPlayClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form76: TForm76;

implementation

{$R *.fmx}

procedure TForm76.BtnPlayClick(Sender: TObject);
Var
  AiSize : TAiImageSize;
  Prompt, Revised : String;
  Res : TAiDalleFile;
  St : TMemoryStream;
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
     AiDalle.StyleFormat :=  tiaStyleVivid;


  AiDalle.UseDalle3 := EditUseDalle3.ItemIndex = 1;


  Case EditImageSize.ItemIndex of
    0 :  AiSize := TAiImageSize.TiaSize256;
    1 :  AiSize := TAiImageSize.TiaSize512;
    2 :  AiSize := TAiImageSize.TiaSize1024;
    3 :  AiSize := TAiImageSize.TiaSize1024_1792;
    4 :  AiSize := TAiImageSize.TiaSize1792_1024;
  End;

  St := TMemoryStream.Create;
  Image1.Bitmap.SaveToStream(St);
  St.Position := 0;

  Res := AiDalle.Variation(St, AISize,1);

  Image2.Bitmap.LoadFromStream(Res.Image);
end;

procedure TForm76.SpeedButton1Click(Sender: TObject);
begin
   If OpenDialog1.Execute then
   Begin
     Image1.Bitmap.LoadFromFile(OpenDialog1.FileName);
   End;
end;

end.
