program FMXTelegramBot;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmTelegramBot in 'uFrmTelegramBot.pas' {FrmTelegramBot},
  uDmTelegramBot in 'uDmTelegramBot.pas' {DmTelegramBot: TDataModule},
  uFrmConfig in 'uFrmConfig.pas' {FrmConfig},
  uDmAgent in 'uDmAgent.pas' {DmAgent: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmTelegramBot, FrmTelegramBot);
  Application.CreateForm(TDmTelegramBot, DmTelegramBot);
  Application.CreateForm(TDmAgent, DmAgent);
  Application.Run;
end.
