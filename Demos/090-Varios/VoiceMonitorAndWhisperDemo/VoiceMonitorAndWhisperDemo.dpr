program VoiceMonitorAndWhisperDemo;

uses
  Vcl.Forms,
  uMainAudioMonitor in 'uMainAudioMonitor.pas' {Form6},
  uMakerAi.Utils.VoiceMonitor in '..\..\..\Source\Utils\uMakerAi.Utils.VoiceMonitor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
