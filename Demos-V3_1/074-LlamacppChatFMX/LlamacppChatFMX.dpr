program LlamacppChatFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainLlamacppChat in 'uMainLlamacppChat.pas' {frmLlamacppChat};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmLlamacppChat, frmLlamacppChat);
  Application.Run;
end.
