program ChatViewLadera;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainChatLadera in 'uMainChatLadera.pas' {FormChatLadera};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormChatLadera, FormChatLadera);
  Application.Run;
end.
