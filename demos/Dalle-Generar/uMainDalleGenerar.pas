unit uMainDalleGenerar;

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
    MemoPrompt: TMemo;
    BtnPlay: TSpeedButton;
    Label4: TLabel;
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
    ListBoxItem5: TListBoxItem;
    Label8: TLabel;
    EditUseDalle3: TComboBox;
    Layout5: TLayout;
    MemoRevisedPrompt: TMemo;
    Label9: TLabel;
    ListBoxItem6: TListBoxItem;
    Label10: TLabel;
    EditImageSize: TComboBox;
    procedure BtnPlayClick(Sender: TObject);
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

  Prompt := MemoPrompt.Lines.Text;

  Res := AiDalle.Generate(Prompt, AiSize,1);

  MemoRevisedPrompt.Lines.Text := Res.Revised_Prompt;

  Image1.Bitmap.LoadFromStream(Res.Image);

end;

end.
