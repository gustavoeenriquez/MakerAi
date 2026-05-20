program DocumentManagerDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainDocumentManager in 'uMainDocumentManager.pas' {FormDocManager};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDocManager, FormDocManager);
  Application.Run;
end.
