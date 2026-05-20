program MinimalChat;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainMinimalChat in 'uMainMinimalChat.pas' {Form12};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
