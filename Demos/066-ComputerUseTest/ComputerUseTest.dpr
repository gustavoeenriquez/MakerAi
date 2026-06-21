program ComputerUseTest;

uses
  System.StartUpCopy,
  FMX.Types,
  FMX.Forms,
  uMainComputerUseTest in 'uMainComputerUseTest.pas' {FormComputerUse};

{$R *.res}

begin
  {$IFDEF MSWINDOWS}
  // Render por software (WARP). Necesario en Escritorio Remoto (RDP), maquinas
  // virtuales o GPUs cuyo Direct2D no compone la ventana (se ve la miniatura en
  // la barra de tareas pero no la ventana). Sin efecto visible en hardware normal.
  GlobalUseDXSoftware := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormComputerUse, FormComputerUse);
  Application.Run;
end.
