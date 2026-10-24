program LaderaApp;

// LIBRO "IA con Delphi" - La app de Cafe La Ladera, etapa del capitulo 21.
// Aprobacion: un pedido grande se suspende (TAiWaitApprovalTool + checkpoint en disco) y la respuesta del chat lo reanuda.

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
