// MIT License
// MakerAI - Demo 064: PPM Auto Tools
// El LLM descubre, instala y ejecuta herramientas MCP del registry PPM
// de forma autónoma, sin intervención del usuario.

program PPMAutoTools;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainPPMAutoTools in 'uMainPPMAutoTools.pas' {frmPPMAutoTools};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPPMAutoTools, frmPPMAutoTools);
  Application.Run;
end.
