program ComputerUseServer;

// Computer Use contra el servidor cimamaker (loop manual, formato OpenAI).
// El form se construye 100% en codigo (sin .fmx) -> CreateNew + MainForm manual.

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainCUServer in 'uMainCUServer.pas';

{$R *.res}

begin
  Application.Initialize;
  FormCUServer := TFormCUServer.CreateNew(Application);
  Application.MainForm := FormCUServer;
  FormCUServer.Show;
  Application.Run;
end.
