program LaderaApp;

// LIBRO "IA con Delphi" - La app de Cafe La Ladera, etapa del capitulo 17.
// Habilidades: tres SKILL.md de la tienda, cargados bajo demanda con TAiSkills (use_skill / read_skill_file).

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
