program MinimalCall;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainMinimalCall in 'uMainMinimalCall.pas' {Form12};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
