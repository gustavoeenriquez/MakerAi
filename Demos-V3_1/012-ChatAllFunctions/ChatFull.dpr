program ChatFull;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainChatFull in 'uMainChatFull.pas' {Form2},
  uMemoPropertiesEdit in 'uMemoPropertiesEdit.pas' {FMemoPropertiesEdit};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
