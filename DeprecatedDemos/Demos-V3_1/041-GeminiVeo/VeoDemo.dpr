program VeoDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uVeoDemo in 'uVeoDemo.pas' {Form11};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
