program TestMemory;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  // Registro de FireDAC SQLite (factories y driver)
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.DApt,
  FireDAC.VCLUI.Wait,
  uTestMemory in 'uTestMemory.pas';

begin
  try
    RunTests;
  except
    on E: Exception do
    begin
      Writeln('FATAL: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
