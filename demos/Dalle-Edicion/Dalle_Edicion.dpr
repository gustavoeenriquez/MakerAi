program Dalle_Edicion;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainDalleEdicion in 'uMainDalleEdicion.pas' {Form76};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm76, Form76);
  Application.Run;
end.
