program ComputerUseTest01;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainComputerUseTest in 'uMainComputerUseTest.pas' {FormComputerUse};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormComputerUse, FormComputerUse);
  Application.Run;
end.
