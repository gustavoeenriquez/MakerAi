program BasicRAGGraph;

uses
  Vcl.Forms,
  uMainBasicRagGraph in 'uMainBasicRagGraph.pas' {Form20};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm20, Form20);
  Application.Run;
end.
