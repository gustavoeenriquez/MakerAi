program ElevenLabsTTSDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uElevenLabsTTSDemo in 'uElevenLabsTTSDemo.pas' {FrmElevenLabs};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmElevenLabs, FrmElevenLabs);
  Application.Run;
end.
