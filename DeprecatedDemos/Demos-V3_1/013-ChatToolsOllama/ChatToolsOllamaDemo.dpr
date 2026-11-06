program ChatToolsOllamaDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainChatToolsOllamaDemo in 'uMainChatToolsOllamaDemo.pas' {Form26};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm26, Form26);
  Application.Run;
end.
