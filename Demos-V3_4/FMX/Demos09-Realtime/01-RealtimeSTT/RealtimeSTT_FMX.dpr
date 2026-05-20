program RealtimeSTT_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmRealtimeSTT in 'uFrmRealtimeSTT.pas' {FrmRealtimeSTT};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmRealtimeSTT, FrmRealtimeSTT);
  Application.Run;
end.
