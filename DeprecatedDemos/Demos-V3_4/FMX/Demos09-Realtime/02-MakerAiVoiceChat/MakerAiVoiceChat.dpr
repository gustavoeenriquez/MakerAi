program MakerAiVoiceChat;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmVoiceChat in 'uFrmVoiceChat.pas' {FrmVoiceChat};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmVoiceChat, FrmVoiceChat);
  Application.Run;
end.
