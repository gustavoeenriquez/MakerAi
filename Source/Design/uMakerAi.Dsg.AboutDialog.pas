unit uMakerAi.Dsg.AboutDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TAboutDialog = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    lblVersion: TLabel;
    Image1: TImage;
    lblCopyright: TLabel;
    lblDescription: TLabel;
    memoVersionInfo: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutDialog: TAboutDialog;

implementation

{$I uMakerAi.Version.inc}

{$R *.dfm}

function GetMakerAiVersionInfo: string;
begin
  Result := Format('%s %s (Build: %s %s) [%s]', [
    MAKERAI_PRODUCT_NAME,
    MAKERAI_VERSION_FULL,
    MAKERAI_BUILD_DATE,
    MAKERAI_BUILD_TIME,
    MAKERAI_BUILD_PLATFORM
  ]);
end;


procedure TAboutDialog.FormShow(Sender: TObject);
begin
  lblVersion.Caption := MAKERAI_VERSION_STRING;
  lblCopyright.Caption := MAKERAI_COPYRIGHT;
  lblDescription.Caption := MAKERAI_DESCRIPTION;
  memoVersionInfo.Caption := GetMakerAiVersionInfo;
end;

end.
