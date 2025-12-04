unit uMemoPropertiesEdit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TFMemoPropertiesEdit = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Rectangle9: TRectangle;
    LblMemo: TLabel;
    MemoData: TMemo;
    BtnOk: TButton;
    Image11: TImage;
    BtnCancel: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    Procedure SetData(aCaption, aContent : String; aVideoVisible, aImageVisible : Boolean);
    Function GetContent : String;
  end;

var
  FMemoPropertiesEdit: TFMemoPropertiesEdit;

implementation

{$R *.fmx}

{ TGMemoPropertiesEdit }

function TFMemoPropertiesEdit.GetContent: String;
Var
  Content : String;
begin
   Content := MemoData.Lines.Text;
   Content := StringReplace(Content,sLineBreak,'\n',[rfReplaceAll]);
   Result := Content;
end;

procedure TFMemoPropertiesEdit.SetData(aCaption, aContent: String; aVideoVisible, aImageVisible: Boolean);
begin
   Caption := aCaption;
   LblMemo.Text := aCaption;
   aContent := StringReplace(aContent, '\n',sLineBreak,[rfReplaceAll]);
   MemoData.Lines.Text := aContent;
end;

end.
