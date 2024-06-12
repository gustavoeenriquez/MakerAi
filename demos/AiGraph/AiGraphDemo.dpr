program AiGraphDemo;



uses
  System.StartUpCopy,
  FMX.Forms,
  uAiGraphDemo in 'uAiGraphDemo.pas' {Form70};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm70, Form70);
  Application.Run;
end.
