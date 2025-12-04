// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Utils.system;

interface

uses
  system.SysUtils, system.Classes, system.IOUtils, system.SyncObjs,
  system.Character,

{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellAPI;
{$ENDIF}
{$IFDEF POSIX}
Posix.String_, Posix.Unistd, Posix.Base, Posix.Errno, Posix.SysWait, Posix.Signal, Posix.Fcntl, Posix.Stdlib, Posix.SysTypes;
{$ENDIF}

type
{$IFDEF MSWINDOWS}
  TProcessHandle = THandle;
  // TProcessHandle es ahora idéntico a THandle en Windows
  TPipeHandle = THandle; // TPipeHandle es ahora idéntico a THandle en Windows
{$ENDIF}
{$IFDEF POSIX}
  TProcessHandle = pid_t;
  // TProcessHandle es idéntico a pid_t (que es un Integer)
  TPipeHandle = Integer; // TPipeHandle es idéntico a Integer (file descriptor)
{$ENDIF}

  TPipeHandles = record
    InputRead: TPipeHandle;
    InputWrite: TPipeHandle;
    OutputRead: TPipeHandle;
    OutputWrite: TPipeHandle;
    ErrorRead: TPipeHandle;
    ErrorWrite: TPipeHandle;
  end;

  TInteractiveProcessInfo = class
  private
{$IFDEF POSIX}
    FChildPID: pid_t;
{$ENDIF}
  public
    ProcessHandle: TProcessHandle;
{$IFDEF MSWINDOWS}
    ThreadHandle: THandle;
    ProcessID: Cardinal;
{$ENDIF}
    PipeHandles: TPipeHandles;
    Running: Boolean;
    ExitCode: Cardinal;

    constructor Create;
    destructor Destroy; override;

    function IsRunning: Boolean;
    // En POSIX, el timeout es ignorado y la espera es indefinida
    function WaitOnExit(ATimeoutMs: Cardinal = Cardinal(-1)): Boolean;
    procedure Terminate; // Terminación suave (SIGTERM en POSIX)
    procedure Kill; // Terminación forzada (SIGKILL en POSIX)
    function WriteInput(const Buffer; Count: Integer): Integer;
    function ReadOutput(var Buffer; Count: Integer): Integer;
    function ReadError(var Buffer; Count: Integer): Integer;
  end;

  TUtilsSystem = class
  public
    class function RunCommandLine(ACommand: string): String; overload;
    class function ExecuteCommandLine(ACommand: string): Boolean;

    class function StartInteractiveProcess(const ACommand: string; ACurrentDirectory: string = ''; AEnvironment: TStrings = nil): TInteractiveProcessInfo;
    class procedure StopInteractiveProcess(var AProcessInfo: TInteractiveProcessInfo);

    class function GetSystemEnvironment: TStringList;

{$IFDEF MSWINDOWS}
    class function ShellOpenFile(const AFileName: string): Boolean;
{$ENDIF}
  end;

implementation

{ TInteractiveProcessInfo }

constructor TInteractiveProcessInfo.Create;
begin
  inherited Create;
  ProcessHandle := 0;
{$IFDEF MSWINDOWS}
  ThreadHandle := 0;
  ProcessID := 0;
{$ENDIF}
{$IFDEF POSIX}
  FChildPID := 0;
{$ENDIF}
  FillChar(PipeHandles, SizeOf(TPipeHandles), 0);
  Running := False;
  ExitCode := 0;
end;

destructor TInteractiveProcessInfo.Destroy;
begin
  if Running then
    Terminate;

{$IFDEF MSWINDOWS}
  if PipeHandles.InputRead <> 0 then
    CloseHandle(PipeHandles.InputRead);
  if PipeHandles.InputWrite <> 0 then
    CloseHandle(PipeHandles.InputWrite);
  if PipeHandles.OutputRead <> 0 then
    CloseHandle(PipeHandles.OutputRead);
  if PipeHandles.OutputWrite <> 0 then
    CloseHandle(PipeHandles.OutputWrite);
  if PipeHandles.ErrorRead <> 0 then
    CloseHandle(PipeHandles.ErrorRead);
  if PipeHandles.ErrorWrite <> 0 then
    CloseHandle(PipeHandles.ErrorWrite);
  if ProcessHandle <> 0 then
    CloseHandle(ProcessHandle);
  if ThreadHandle <> 0 then
    CloseHandle(ThreadHandle);
{$ENDIF}
{$IFDEF POSIX}
  // En POSIX, el padre solo cierra los descriptores que usa.
  if PipeHandles.InputWrite <> 0 then
    Posix.Unistd.__close(PipeHandles.InputWrite);
  if PipeHandles.OutputRead <> 0 then
    Posix.Unistd.__close(PipeHandles.OutputRead);
  if PipeHandles.ErrorRead <> 0 then
    Posix.Unistd.__close(PipeHandles.ErrorRead);
{$ENDIF}
  inherited;
end;

{$IFDEF MSWINDOWS}

function TInteractiveProcessInfo.IsRunning: Boolean;
var
  CurrentExitCode: DWORD;
begin
  Result := False;
  if ProcessHandle <> 0 then
  begin
    if GetExitCodeProcess(ProcessHandle, CurrentExitCode) then
    begin
      Running := (CurrentExitCode = STILL_ACTIVE);
      if not Running then
        ExitCode := CurrentExitCode;
      Result := Running;
    end
    else
    begin
      Running := False;
      ProcessHandle := 0;
    end;
  end;
end;

function TInteractiveProcessInfo.WaitOnExit(ATimeoutMs: Cardinal): Boolean;
begin
  if ProcessHandle <> 0 then
  begin
    Result := (WaitForSingleObject(ProcessHandle, ATimeoutMs) = WAIT_OBJECT_0);
    IsRunning;
  end
  else
    Result := True;
end;

procedure TInteractiveProcessInfo.Terminate;
begin
  if IsRunning then
  begin
    Winapi.Windows.TerminateProcess(ProcessHandle, 0);
    WaitOnExit(5000);
  end;
end;

procedure TInteractiveProcessInfo.Kill;
begin
  if IsRunning then
  begin
    Winapi.Windows.TerminateProcess(ProcessHandle, 1);
    WaitOnExit(5000);
  end;
end;

function TInteractiveProcessInfo.WriteInput(const Buffer; Count: Integer): Integer;
var
  BytesWritten: DWORD;
begin
  Result := 0;
  if (PipeHandles.InputWrite <> 0) and IsRunning then
  begin
    if Winapi.Windows.WriteFile(PipeHandles.InputWrite, Buffer, Count, BytesWritten, nil) then
      Result := BytesWritten;
  end;
end;

function TInteractiveProcessInfo.ReadOutput(var Buffer; Count: Integer): Integer;
var
  BytesRead, BytesAvailable: DWORD;
begin
  Result := 0;
  if (PipeHandles.OutputRead <> 0) then
  begin
    BytesAvailable := 0;
    if PeekNamedPipe(PipeHandles.OutputRead, nil, 0, nil, @BytesAvailable, nil) and (BytesAvailable > 0) then
    begin
      if Winapi.Windows.ReadFile(PipeHandles.OutputRead, Buffer, Count, BytesRead, nil) then
        Result := BytesRead;
    end
    else if not IsRunning then
    begin
      if Winapi.Windows.ReadFile(PipeHandles.OutputRead, Buffer, Count, BytesRead, nil) then
        Result := BytesRead;
    end;
  end;
end;

function TInteractiveProcessInfo.ReadError(var Buffer; Count: Integer): Integer;
var
  BytesRead, BytesAvailable: DWORD;
begin
  Result := 0;
  if (PipeHandles.ErrorRead <> 0) then
  begin
    BytesAvailable := 0;
    if PeekNamedPipe(PipeHandles.ErrorRead, nil, 0, nil, @BytesAvailable, nil) and (BytesAvailable > 0) then
    begin
      if Winapi.Windows.ReadFile(PipeHandles.ErrorRead, Buffer, Count, BytesRead, nil) then
        Result := BytesRead;
    end
    else if not IsRunning then
    begin
      if Winapi.Windows.ReadFile(PipeHandles.ErrorRead, Buffer, Count, BytesRead, nil) then
        Result := BytesRead;
    end;
  end;
end;

{$ENDIF}
{$IFDEF POSIX}

function TInteractiveProcessInfo.IsRunning: Boolean;
var
  Status: Integer;
  Res: pid_t;
begin
  if FChildPID <= 0 then
  begin
    Running := False;
    Result := False;
    Exit;
  end;

  Res := Posix.SysWait.waitpid(FChildPID, @Status, WNOHANG);
  if Res = 0 then
  begin
    Running := True;
    Result := True;
  end
  else if Res = FChildPID then
  begin
    Running := False;
    Result := False;
    if WIFEXITED(Status) then
      ExitCode := WEXITSTATUS(Status)
    else
      ExitCode := 255;
    FChildPID := 0;
  end
  else
  begin
    Running := False;
    Result := False;
    FChildPID := 0;
  end;
end;

function TInteractiveProcessInfo.WaitOnExit(ATimeoutMs: Cardinal): Boolean;
var
  Status: Integer;
begin
  if FChildPID <= 0 then
    Exit(True);

  if Posix.SysWait.waitpid(FChildPID, @Status, 0) = FChildPID then
  begin
    IsRunning;
    Result := True;
  end
  else
    Result := False;
end;

procedure TInteractiveProcessInfo.Terminate;
begin
  if IsRunning then
  begin
    Posix.Signal.Kill(FChildPID, SIGTERM);
    WaitOnExit(5000);
  end;
end;

procedure TInteractiveProcessInfo.Kill;
begin
  if IsRunning then
  begin
    Posix.Signal.Kill(FChildPID, SIGKILL);
    WaitOnExit(5000);
  end;
end;

function TInteractiveProcessInfo.WriteInput(const Buffer; Count: Integer): Integer;
begin
  Result := -1;
  if (PipeHandles.InputWrite <> 0) and IsRunning then
  begin
    Result := Posix.Unistd.__write(PipeHandles.InputWrite, Pointer(Buffer), Count);
  end;
end;

function TInteractiveProcessInfo.ReadOutput(var Buffer; Count: Integer): Integer;
begin
  Result := -1;
  if PipeHandles.OutputRead <> 0 then
  begin
    Result := Posix.Unistd.__read(PipeHandles.OutputRead, Pointer(Buffer), Count);
    if (Result = -1) and (Errno = EAGAIN) then
      Result := 0;
  end;
end;

function TInteractiveProcessInfo.ReadError(var Buffer; Count: Integer): Integer;
begin
  Result := -1;
  if PipeHandles.ErrorRead <> 0 then
  begin
    Result := Posix.Unistd.__read(PipeHandles.ErrorRead, Pointer(Buffer), Count);
    if (Result = -1) and (Errno = EAGAIN) then
      Result := 0;
  end;
end;

{$ENDIF}
{ TUtilsSystem }

{$IFDEF MSWINDOWS}

class function TUtilsSystem.ShellOpenFile(const AFileName: string): Boolean;
var
  ErrorCode: Integer;
begin

{$IFDEF MSWINDOWS}
  // Para Windows, usamos la API ShellExecuteW (la versión Unicode)
  // El handle es 0 (escritorio), 'open' es la acción por defecto.
  // PChar(AFileName) es la ruta al archivo.
  // nil para parámetros, nil para directorio, SW_SHOWNORMAL para mostrar la app.
  ErrorCode := Integer(ShellExecuteW(0, 'open', PChar(AFileName), nil, nil, SW_SHOWNORMAL));

  // ShellExecute devuelve un valor > 32 en caso de éxito.
  Result := ErrorCode > 32;
{$ENDIF}
{$IFDEF MACOS}
  // En macOS, la lógica es diferente. Se usa NSWorkspace.
  // Esto requeriría más código y uses de Macapi.*
  // Por ahora, dejamos un placeholder.
  ShowMessage('Abrir archivos no está implementado para macOS en este ejemplo.');
  Result := False;
{$ENDIF}
{$IFDEF ANDROID}
  // En Android, se usan Intents. Es aún más complejo por los permisos y File Providers.
  ShowMessage('Abrir archivos no está implementado para Android en este ejemplo.');
  Result := False;
{$ENDIF}
end;

class function TUtilsSystem.StartInteractiveProcess(const ACommand: string; ACurrentDirectory: string; AEnvironment: TStrings): TInteractiveProcessInfo;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  Cmd: string;
  EnvBlock: string;
  EnvPtr: PChar;
  CreationFlags: DWORD;
  i: Integer;
  DirToUse: PChar;
begin
  Result := TInteractiveProcessInfo.Create;
  try
    FillChar(SA, SizeOf(SA), 0);
    SA.nLength := SizeOf(SA);
    SA.bInheritHandle := True;

    if not CreatePipe(Result.PipeHandles.InputRead, Result.PipeHandles.InputWrite, @SA, 0) or not CreatePipe(Result.PipeHandles.OutputRead, Result.PipeHandles.OutputWrite, @SA, 0) or
      not CreatePipe(Result.PipeHandles.ErrorRead, Result.PipeHandles.ErrorWrite, @SA, 0) then
    begin
      Result.Free;
      Result := nil;
      RaiseLastOSError;
      Exit;
    end;

    SetHandleInformation(Result.PipeHandles.InputWrite, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(Result.PipeHandles.OutputRead, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(Result.PipeHandles.ErrorRead, HANDLE_FLAG_INHERIT, 0);

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.hStdInput := Result.PipeHandles.InputRead;
    SI.hStdOutput := Result.PipeHandles.OutputWrite;
    SI.hStdError := Result.PipeHandles.ErrorWrite;
    SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;

    Cmd := ACommand;
    UniqueString(Cmd);

    EnvPtr := nil;
    CreationFlags := CREATE_NO_WINDOW;
    if Assigned(AEnvironment) and (AEnvironment.Count > 0) then
    begin
      for i := 0 to AEnvironment.Count - 1 do
        EnvBlock := EnvBlock + AEnvironment[i] + #0;
      EnvBlock := EnvBlock + #0;
      EnvPtr := PChar(EnvBlock);
      CreationFlags := CreationFlags or CREATE_UNICODE_ENVIRONMENT;
    end;

    if Trim(ACurrentDirectory) = '' then
      DirToUse := nil
    else
      DirToUse := PChar(Trim(ACurrentDirectory));

    if CreateProcess(nil, PChar(Cmd), nil, nil, True, CreationFlags, EnvPtr, DirToUse, SI, PI) then
    begin
      Result.ProcessHandle := PI.hProcess;
      Result.ThreadHandle := PI.hThread;
      Result.ProcessID := PI.dwProcessId;
      Result.Running := True;

      CloseHandle(Result.PipeHandles.InputRead);
      Result.PipeHandles.InputRead := 0;
      CloseHandle(Result.PipeHandles.OutputWrite);
      Result.PipeHandles.OutputWrite := 0;
      CloseHandle(Result.PipeHandles.ErrorWrite);
      Result.PipeHandles.ErrorWrite := 0;
    end
    else
    begin
      Result.Free;
      Result := nil;
      RaiseLastOSError;
    end;
  except
    on E: Exception do
    begin
      if Assigned(Result) then
        Result.Free;
      raise;
    end;
  end;
end;

class function TUtilsSystem.GetSystemEnvironment: TStringList;
var
  pEnv, pStart: PWideChar;
begin
  Result := TStringList.Create;
  pStart := GetEnvironmentStringsW;
  if pStart <> nil then
    try
      pEnv := pStart;
      while pEnv^ <> #0 do
      begin
        Result.Add(string(pEnv));
        pEnv := pEnv + StrLen(pEnv) + 1;
      end;
    finally
      FreeEnvironmentStringsW(pStart);
    end;
end;

class function TUtilsSystem.RunCommandLine(ACommand: string): String;
var
  SA: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  SI: TStartupInfo;
  PI: TProcessInformation;
  Buffer: array [0 .. 2047] of AnsiChar;
  BytesRead: DWORD;
  Cmd: string;
  Output: TStringBuilder;
  TempStr: AnsiString;
begin
  Result := '';
  Output := TStringBuilder.Create;
  try
    FillChar(SA, SizeOf(SA), 0);
    SA.nLength := SizeOf(SA);
    SA.bInheritHandle := True;

    if CreatePipe(ReadPipe, WritePipe, @SA, 0) then
      try
        SetHandleInformation(ReadPipe, HANDLE_FLAG_INHERIT, 0);

        FillChar(SI, SizeOf(SI), 0);
        SI.cb := SizeOf(SI);
        SI.hStdOutput := WritePipe;
        SI.hStdError := WritePipe;
        SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        SI.wShowWindow := SW_HIDE;

        Cmd := 'cmd.exe /C ' + ACommand;
        UniqueString(Cmd);

        if CreateProcess(nil, PChar(Cmd), nil, nil, True, CREATE_NO_WINDOW, nil, nil, SI, PI) then
          try
            CloseHandle(WritePipe);
            WritePipe := 0;
            repeat
              BytesRead := 0;
              if ReadFile(ReadPipe, Buffer, SizeOf(Buffer), BytesRead, nil) and (BytesRead > 0) then
              Begin

                SetString(TempStr, Buffer, BytesRead);
                Output.Append(string(TempStr));

              End;
            until not(BytesRead > 0);
            WaitForSingleObject(PI.hProcess, INFINITE);
          finally
            CloseHandle(PI.hProcess);
            CloseHandle(PI.hThread);
          end;
        Result := Output.ToString;
      finally
        CloseHandle(ReadPipe);
        if WritePipe <> 0 then
          CloseHandle(WritePipe);
      end;
  finally
    Output.Free;
  end;
end;

class function TUtilsSystem.ExecuteCommandLine(ACommand: string): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  ExitCode: DWORD;
  Cmd: string;
begin
  Result := False;
  FillChar(SI, SizeOf(SI), 0);
  FillChar(PI, SizeOf(PI), 0);
  SI.cb := SizeOf(TStartupInfo);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_SHOWNORMAL;

  Cmd := ACommand;
  UniqueString(Cmd);

  if CreateProcess(nil, PChar(Cmd), nil, nil, False, 0, nil, nil, SI, PI) then
  begin
    WaitForSingleObject(PI.hProcess, INFINITE);
    GetExitCodeProcess(PI.hProcess, ExitCode);
    Result := (ExitCode = 0);
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
  end;
end;

{$ENDIF}
{$IFDEF POSIX}

{
  Mejora a futuro:
  No soporta comillas simples ('), ni escapar caracteres con barra invertida (\),
  ni tuberías (|) o redirecciones (>) propias de bash,
  ya que usa execvp directamente y no una shell (/bin/sh).
  Si intenta ejecutar ls -la | grep x fallará porque | y grep serán tratados como argumentos de ls.

}

function popen(const command: MarshaledAString; const _type: MarshaledAString): Pointer; cdecl; external libc name _PU + 'popen';
function pclose(filehandle: Pointer): int32; cdecl; external libc name _PU + 'pclose';
function fgets(Buffer: PAnsiChar; size: int32; Stream: Pointer): PAnsiChar; cdecl; external libc name _PU + 'fgets';
function system(const command: MarshaledAString): Integer; cdecl; external libc name _PU + 'system';

procedure ParseCommand(const ACommand: string; out AProgram: string; out AArgs: TArray<string>);
var
  ArgsList: TStringList;
  i: Integer;
  CurrentArg: string;
  InQuotes: Boolean;
  C: Char;
begin
  ArgsList := TStringList.Create;
  try
    CurrentArg := '';
    InQuotes := False;
    for C in ACommand do
    begin
      if (C = ' ') and not InQuotes then
      begin
        if CurrentArg <> '' then
        begin
          ArgsList.Add(CurrentArg);
          CurrentArg := '';
        end;
      end
      else if C = '"' then
      begin
        InQuotes := not InQuotes;
      end
      else
      begin
        CurrentArg := CurrentArg + C;
      end;
    end;
    if CurrentArg <> '' then
      ArgsList.Add(CurrentArg);
    if ArgsList.Count > 0 then
    begin
      AProgram := ArgsList[0];
      SetLength(AArgs, ArgsList.Count);
      for i := 0 to ArgsList.Count - 1 do
        AArgs[i] := ArgsList[i];
    end
    else
    begin
      AProgram := '';
      SetLength(AArgs, 0);
    end;
  finally
    ArgsList.Free;
  end;
end;

class function TUtilsSystem.StartInteractiveProcess(const ACommand: string; ACurrentDirectory: string; AEnvironment: TStrings): TInteractiveProcessInfo;
var
  PipeIn, PipeOut, PipeErr: array [0 .. 1] of Integer;
  PID: pid_t;
  ProgramName: string;
  Args: TArray<string>;
  PArgs: TArray<PAnsiChar>;
  i: Integer;
begin
  Result := TInteractiveProcessInfo.Create;
  try
    if (Posix.Unistd.pipe(@PipeIn[0]) <> 0) or (Posix.Unistd.pipe(@PipeOut[0]) <> 0) or (Posix.Unistd.pipe(@PipeErr[0]) <> 0) then
    begin
      raise Exception.Create('Failed to create pipes. Error: ' + strerror(Errno));
    end;

    PID := Posix.Unistd.fork;

    if PID < 0 then
    begin
      raise Exception.Create('Failed to fork process. Error: ' + strerror(Errno));
    end;

    if PID = 0 then // --- HIJO ---
    begin
      Posix.Unistd.__close(PipeIn[1]);
      Posix.Unistd.__close(PipeOut[0]);
      Posix.Unistd.__close(PipeErr[0]);

      Posix.Unistd.dup2(PipeIn[0], STDIN_FILENO);
      Posix.Unistd.dup2(PipeOut[1], STDOUT_FILENO);
      Posix.Unistd.dup2(PipeErr[1], STDERR_FILENO);

      Posix.Unistd.__close(PipeIn[0]);
      Posix.Unistd.__close(PipeOut[1]);
      Posix.Unistd.__close(PipeErr[1]);

      if ACurrentDirectory <> '' then
        Posix.Unistd.__chdir(PAnsiChar(AnsiString(ACurrentDirectory)));

      ParseCommand(ACommand, ProgramName, Args);
      if ProgramName = '' then
        Posix.Unistd._exit(127);

      SetLength(PArgs, Length(Args) + 1);
      for i := 0 to High(Args) do
        PArgs[i] := PAnsiChar(AnsiString(Args[i]));
      PArgs[Length(Args)] := nil;

      Posix.Unistd.execvp(PAnsiChar(AnsiString(ProgramName)), @PArgs[0]);
      Posix.Unistd._exit(127);
    end
    else // --- PADRE ---
    begin
      Posix.Unistd.__close(PipeIn[0]);
      Posix.Unistd.__close(PipeOut[1]);
      Posix.Unistd.__close(PipeErr[1]);

      Result.PipeHandles.InputWrite := PipeIn[1];
      Result.PipeHandles.OutputRead := PipeOut[0];
      Result.PipeHandles.ErrorRead := PipeErr[0];
      Result.ProcessHandle := PID;
      Result.FChildPID := PID;
      Result.Running := True;

      Posix.Fcntl.Fcntl(Result.PipeHandles.OutputRead, F_SETFL, O_NONBLOCK);
      Posix.Fcntl.Fcntl(Result.PipeHandles.ErrorRead, F_SETFL, O_NONBLOCK);
    end;
  except
    on E: Exception do
    begin
      if Assigned(Result) then
        Result.Free;
      Result := nil;
      raise;
    end;
  end;
end;

class function TUtilsSystem.GetSystemEnvironment: TStringList;
var
  p: PPAnsiChar;
begin
  Result := TStringList.Create;
  p := Posix.Unistd.environ;

  if p <> nil then
  begin
    while p^ <> nil do
    begin
      Result.Add(UTF8ToString(p^));
      Inc(p);
    end;
  end;
end;

class function TUtilsSystem.RunCommandLine(ACommand: string): String;
var
  Handle: Pointer;
  Buffer: array [0 .. 1023] of AnsiChar;
  Output: TStringBuilder;
  M: TMarshaller;
begin
  Result := '';
  Output := TStringBuilder.Create;
  try
    // CORRECCIÓN: Usar .ToPointer para obtener el puntero crudo del wrapper.
    Handle := popen(M.AsAnsi(ACommand).ToPointer, 'r');
    if Handle = nil then
      raise Exception.CreateFmt('Failed to popen command: %s', [ACommand]);
    try
      while fgets(@Buffer[0], SizeOf(Buffer), Handle) <> nil do
      begin
        Output.Append(Buffer);
      end;
      // La salida de popen/fgets ya está en la codificación de la consola (a menudo UTF-8 en Linux).
      // El paso UTF8ToString es correcto si el buffer es AnsiChar y contiene UTF-8.
      Result := Trim(UTF8ToString(Output.ToString));
    finally
      pclose(Handle);
    end;
  finally
    Output.Free;
  end;
end;

class function TUtilsSystem.ExecuteCommandLine(ACommand: string): Boolean;
var
  Status: Integer;
  M: TMarshaller;
begin
  Status := system(M.AsAnsi(ACommand).ToPointer);
  Result := (Status = 0);
end;

{$ENDIF}

class procedure TUtilsSystem.StopInteractiveProcess(var AProcessInfo: TInteractiveProcessInfo);
begin
  if Assigned(AProcessInfo) then
  begin
    AProcessInfo.Free;
    AProcessInfo := nil;
  end;
end;

end.
