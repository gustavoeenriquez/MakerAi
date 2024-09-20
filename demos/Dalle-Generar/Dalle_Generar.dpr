program Dalle_Generar;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainDalleGenerar in 'uMainDalleGenerar.pas' {Form76};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm76, Form76);
  Application.Run;
end.
