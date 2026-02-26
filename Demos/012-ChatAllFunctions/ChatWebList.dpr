program ChatWebList;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainChatWebList in 'uMainChatWebList.pas' {Form25};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm25, Form25);
  Application.Run;
end.
