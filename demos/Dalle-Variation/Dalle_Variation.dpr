program Dalle_Variation;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainDalleVariation in 'uMainDalleVariation.pas' {Form76};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm76, Form76);
  Application.Run;
end.
