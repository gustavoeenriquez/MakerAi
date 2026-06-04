program FMXTelegramAgent;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmTelegramAgent in 'uFrmTelegramAgent.pas' {FrmTelegramAgent},
  uDMAgent          in 'uDMAgent.pas'          {DmAgent: TDataModule},
  uDmTelegram       in 'uDmTelegram.pas'        {DmTelegram: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmTelegramAgent, FrmTelegramAgent);
  Application.Run;
end.
