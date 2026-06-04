program DemoAgentesTools;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainDemoAgenteTools in 'uMainDemoAgenteTools.pas' {FMainDemo},
  uDmAgentesTool in 'uDmAgentesTool.pas' {FDataModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMainDemo, FMainDemo);
  Application.Run;
end.
