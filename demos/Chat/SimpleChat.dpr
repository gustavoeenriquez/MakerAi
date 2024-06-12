program SimpleChat;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainSimpleChat in 'uMainSimpleChat.pas' {Form64};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm64, Form64);
  Application.Run;
end.
