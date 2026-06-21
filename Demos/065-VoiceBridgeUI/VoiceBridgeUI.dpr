program VoiceBridgeUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainVoiceBridgeUI in 'uMainVoiceBridgeUI.pas' {FormVoiceBridge};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormVoiceBridge, FormVoiceBridge);
  Application.Run;
end.
