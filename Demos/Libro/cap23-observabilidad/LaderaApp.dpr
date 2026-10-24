program LaderaApp;

// LIBRO "IA con Delphi" - La app de Cafe La Ladera, etapa del capitulo 23.
// Observabilidad: TAiTelemetry exporta trazas OpenTelemetry de chats, herramientas, MCP, grafos y RAG.

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
