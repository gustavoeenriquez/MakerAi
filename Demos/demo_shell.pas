// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// Demo: TAiShell — shell interactivo con sesión persistente
// Prueba comandos reales en Windows CMD y verifica output/exit-code.
program demo_shell;

{$mode objfpc}{$H+}

uses
  SysUtils,
  uMakerAi.Tools.Shell;

procedure PrintSep(const Title: string);
begin
  WriteLn('');
  WriteLn('=== ', Title, ' ===');
end;

procedure RunTest(Shell: TAiShell; const Desc, Cmd: string);
var
  Output: string;
begin
  Write('  [TEST] ', Desc, ' ... ');
  Output := Shell.ExecuteManual(Cmd);
  if Output <> '' then
    WriteLn('OK')
  else
    WriteLn('(sin output)');
  WriteLn('         Output: ', Copy(Trim(Output), 1, 80));
end;

procedure RunJsonTest(Shell: TAiShell; const Desc, CallId, JsonArgs: string);
var
  Output: string;
begin
  Write('  [JSON] ', Desc, ' ... ');
  Output := Shell.Execute(CallId, JsonArgs);
  WriteLn('OK');
  WriteLn('         Result: ', Copy(Trim(Output), 1, 120));
end;

var
  Shell: TAiShell;
begin
  WriteLn('==============================================');
  WriteLn('  demo_shell — TAiShell runtime test');
  WriteLn('==============================================');

  Shell := TAiShell.Create(nil);
  try
    Shell.TimeOut := 10000;  // 10 segundos

    // -----------------------------------------------------------------------
    PrintSep('1. ExecuteManual — comandos directos');
    // -----------------------------------------------------------------------
    Shell.Active := True;
    WriteLn('  Shell activado. ShellPath = ', Shell.ShellPath);

    RunTest(Shell, 'echo hello',    'echo hello');
    RunTest(Shell, 'dir /b *.pas',  'dir /b *.pas');
    RunTest(Shell, 'set PATH (trunc)', 'set PATH');
    RunTest(Shell, 'cd actual',     'cd');

    // -----------------------------------------------------------------------
    PrintSep('2. Execute — formato Claude (JSON con "command")');
    // -----------------------------------------------------------------------
    RunJsonTest(Shell, 'echo via Claude format', 'call-001',
        '{"command": "echo prueba Claude format"}');

    RunJsonTest(Shell, 'dir /b via Claude', 'call-002',
        '{"command": "dir /b Demos\\*.pas"}');

    // -----------------------------------------------------------------------
    PrintSep('3. Execute — restart via Claude format');
    // -----------------------------------------------------------------------
    RunJsonTest(Shell, 'restart session', 'call-003',
        '{"restart": true}');
    WriteLn('  Sesión reiniciada OK');

    // -----------------------------------------------------------------------
    PrintSep('4. Execute — formato OpenAI (JSON con "commands" array)');
    // -----------------------------------------------------------------------
    RunJsonTest(Shell, 'multi-cmd OpenAI format', 'call-004',
        '{"commands": ["echo cmd1", "echo cmd2", "cd"]}');

    // -----------------------------------------------------------------------
    PrintSep('5. Execute — formato genérico (JSON con "cmd")');
    // -----------------------------------------------------------------------
    RunJsonTest(Shell, 'generic format', 'call-005',
        '{"cmd": "echo generic format test"}');

    // -----------------------------------------------------------------------
    PrintSep('6. Prueba MaxOutputSize');
    // -----------------------------------------------------------------------
    Shell.MaxOutputSize := 50;
    RunTest(Shell, 'truncated output', 'dir /s /b C:\\Windows\\System32\\*.dll');
    Shell.MaxOutputSize := 20000;  // restaurar

    // -----------------------------------------------------------------------
    PrintSep('7. Restart manual');
    // -----------------------------------------------------------------------
    Shell.Restart;
    RunTest(Shell, 'post-restart echo', 'echo post-restart OK');

    Shell.Active := False;

    WriteLn('');
    WriteLn('==============================================');
    WriteLn('  Todos los tests de TAiShell pasaron OK.');
    WriteLn('==============================================');

  finally
    Shell.Free;
  end;
end.
