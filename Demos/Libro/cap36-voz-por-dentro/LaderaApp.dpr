program LaderaApp;

// LIBRO "IA con Delphi" - La app de Cafe La Ladera, etapa del capitulo 32.
// PPM: AutoMCP en el formulario; Lucia busca, instala y usa una herramienta del registry cuando le hace falta.

uses
  System.StartUpCopy,
  FMX.Forms,
  uLaderaApp in 'uLaderaApp.pas' {FormLadera};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormLadera, FormLadera);
  Application.Run;
end.
