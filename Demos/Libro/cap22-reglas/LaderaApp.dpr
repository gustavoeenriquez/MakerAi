program LaderaApp;

// LIBRO "IA con Delphi" - La app de Cafe La Ladera, etapa del capitulo 22.
// Reglas: TAiGuardrails revisa cada llamada a herramienta de Lucia y del agente antes de ejecutarla.

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
