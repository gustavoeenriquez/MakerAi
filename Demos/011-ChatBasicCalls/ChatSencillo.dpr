program ChatSencillo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainChatSencillo in 'uMainChatSencillo.pas' {Form7};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
