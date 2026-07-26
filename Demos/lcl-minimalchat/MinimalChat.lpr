program MinimalChat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,           // LCL widgetset
  Forms,
  SysUtils,
  openssl,              // necesario para HTTPS con proveedores cloud
  opensslsockets,
  uMainMinimalChat;

{$R *.res}

begin
  // Mismos arreglos que Demos/uDemoHelper.pas, en version GUI:
  //  1) separador decimal '.' — si no, Temperature 0.7 viaja como "0,7"
  //     y varios proveedores rechazan el JSON.
  //  2) InitSSLInterface — FPC no inicializa OpenSSL solo; sin esto las
  //     conexiones HTTPS fallan en silencio (Ollama local por HTTP no lo
  //     necesita, pero si cambias de driver si).
  DefaultFormatSettings.DecimalSeparator  := '.';
  DefaultFormatSettings.ThousandSeparator := ',';
  try
    InitSSLInterface;
  except
    // Sin OpenSSL solo funcionan los proveedores por HTTP (Ollama local).
  end;

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFrmMinimalChat, FrmMinimalChat);
  Application.Run;
end.
