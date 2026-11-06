program CompareEmbeddings;

uses
  Vcl.Forms,
  uMainCompareEmbeddings in 'uMainCompareEmbeddings.pas' {Form14};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
