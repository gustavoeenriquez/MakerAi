program AiChatDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainAiChat in 'uMainAiChat.pas' {Form64};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm64, Form64);
  Application.Run;
end.
