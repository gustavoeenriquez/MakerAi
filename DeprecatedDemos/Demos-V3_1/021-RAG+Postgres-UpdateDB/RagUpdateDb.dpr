program RagUpdateDb;

uses
  System.StartUpCopy,
  FMX.Forms,
  uRagUpdateDBMain in 'uRagUpdateDBMain.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
