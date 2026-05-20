program RagVectorQL;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainRAGVQL in 'uMainRAGVQL.pas' {Form21};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm21, Form21);
  Application.Run;
end.
