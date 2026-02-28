// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.Utils.System
// Gestión de procesos interactivos con stdin/stdout/stderr pipes.
unit uMakerAi.Utils.System;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes
{$IFDEF MSWINDOWS}
  , Windows, ShellApi
{$ENDIF}
{$IFDEF UNIX}
  , BaseUnix, Unix
{$ENDIF}
  ;

type

{$IFDEF MSWINDOWS}
  TProcessHandle = THandle;
  TPipeHandle    = THandle;
{$ENDIF}
{$IFDEF UNIX}
  TProcessHandle = LongInt;   // pid_t equivalente
  TPipeHandle    = LongInt;   // file descriptor (cint)
{$ENDIF}

  TPipeHandles = record
    InputRead  : TPipeHandle;
    InputWrite : TPipeHandle;
    OutputRead : TPipeHandle;
    OutputWrite: TPipeHandle;
    ErrorRead  : TPipeHandle;
    ErrorWrite : TPipeHandle;
  end;

  TInteractiveProcessInfo = class
  private
{$IFDEF UNIX}
    FChildPID: LongInt;
{$ENDIF}
  public
    ProcessHandle: TProcessHandle;
{$IFDEF MSWINDOWS}
    ThreadHandle: THandle;
    ProcessID   : Cardinal;
{$ENDIF}
    PipeHandles: TPipeHandles;
    Running    : Boolean;
    ExitCode   : Cardinal;

    constructor Create;
    destructor  Destroy; override;

    function  IsRunning: Boolean;
    function  WaitOnExit(ATimeoutMs: Cardinal = Cardinal(-1)): Boolean;
    procedure Terminate;
    procedure Kill;
    function  WriteInput(const Buffer; Count: Integer): Integer;
    function  ReadOutput(var Buffer; Count: Integer): Integer;
    function  ReadError(var Buffer; Count: Integer): Integer;
  end;

  TUtilsSystem = class
  public
    class function  RunCommandLine(ACommand: string): string;
    class function  ExecuteCommandLine(ACommand: string): Boolean;
    class function  StartInteractiveProcess(const ACommand: string;
        ACurrentDirectory: string = '';
        AEnvironment: TStrings = nil): TInteractiveProcessInfo;
    class procedure StopInteractiveProcess(var AProcessInfo: TInteractiveProcessInfo);
    class function  GetSystemEnvironment: TStringList;
{$IFDEF MSWINDOWS}
    class function  ShellOpenFile(const AFileName: string): Boolean;
{$ENDIF}
  end;

implementation

{$IFDEF UNIX}
// Funciones de la libc POSIX que no expone FPC directamente
function  execvp(const path: PAnsiChar; const argv: PPAnsiChar): cint;
    cdecl; external 'c' name 'execvp';
function  setenv(const name: PAnsiChar; const value: PAnsiChar; overwrite: cint): cint;
    cdecl; external 'c' name 'setenv';
procedure unix_exit(status: cint);
    cdecl; external 'c' name '_exit';
function  strerror(errnum: cint): PAnsiChar;
    cdecl; external 'c' name 'strerror';
function  popen(const command: PAnsiChar; const _type: PAnsiChar): Pointer;
    cdecl; external 'c' name 'popen';
function  pclose(f: Pointer): cint;
    cdecl; external 'c' name 'pclose';
function  fgets(buf: PAnsiChar; size: cint; f: Pointer): PAnsiChar;
    cdecl; external 'c' name 'fgets';
function  c_system(const command: PAnsiChar): cint;
    cdecl; external 'c' name 'system';
var
  environ: PPAnsiChar; external name 'environ';
{$ENDIF}

// =============================================================================
// TInteractiveProcessInfo
// =============================================================================

constructor TInteractiveProcessInfo.Create;
begin
  inherited Create;
  ProcessHandle := 0;
{$IFDEF MSWINDOWS}
  ThreadHandle := 0;
  ProcessID    := 0;
{$ENDIF}
{$IFDEF UNIX}
  FChildPID := 0;
{$ENDIF}
  FillChar(PipeHandles, SizeOf(TPipeHandles), 0);
  Running  := False;
  ExitCode := 0;
end;

destructor TInteractiveProcessInfo.Destroy;
begin
  if Running then
    Terminate;

{$IFDEF MSWINDOWS}
  if PipeHandles.InputRead   <> 0 then CloseHandle(PipeHandles.InputRead);
  if PipeHandles.InputWrite  <> 0 then CloseHandle(PipeHandles.InputWrite);
  if PipeHandles.OutputRead  <> 0 then CloseHandle(PipeHandles.OutputRead);
  if PipeHandles.OutputWrite <> 0 then CloseHandle(PipeHandles.OutputWrite);
  if PipeHandles.ErrorRead   <> 0 then CloseHandle(PipeHandles.ErrorRead);
  if PipeHandles.ErrorWrite  <> 0 then CloseHandle(PipeHandles.ErrorWrite);
  if ProcessHandle           <> 0 then CloseHandle(ProcessHandle);
  if ThreadHandle            <> 0 then CloseHandle(ThreadHandle);
{$ENDIF}
{$IFDEF UNIX}
  if PipeHandles.InputWrite  <> 0 then FpClose(PipeHandles.InputWrite);
  if PipeHandles.OutputRead  <> 0 then FpClose(PipeHandles.OutputRead);
  if PipeHandles.ErrorRead   <> 0 then FpClose(PipeHandles.ErrorRead);
{$ENDIF}
  inherited;
end;

// =============================================================================
// Implementación Windows
// =============================================================================

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
      Running       := False;
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
    Windows.TerminateProcess(ProcessHandle, 0);
    WaitOnExit(5000);
  end;
end;

procedure TInteractiveProcessInfo.Kill;
begin
  if IsRunning then
  begin
    Windows.TerminateProcess(ProcessHandle, 1);
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
    if Windows.WriteFile(PipeHandles.InputWrite, Buffer, Count, BytesWritten, nil) then
      Result := BytesWritten;
  end;
end;

function TInteractiveProcessInfo.ReadOutput(var Buffer; Count: Integer): Integer;
var
  BytesRead, BytesAvailable: DWORD;
begin
  Result := 0;
  if PipeHandles.OutputRead <> 0 then
  begin
    BytesAvailable := 0;
    if PeekNamedPipe(PipeHandles.OutputRead, nil, 0, nil, @BytesAvailable, nil) and
       (BytesAvailable > 0) then
    begin
      if Windows.ReadFile(PipeHandles.OutputRead, Buffer, Count, BytesRead, nil) then
        Result := BytesRead;
    end
    else if not IsRunning then
    begin
      if Windows.ReadFile(PipeHandles.OutputRead, Buffer, Count, BytesRead, nil) then
        Result := BytesRead;
    end;
  end;
end;

function TInteractiveProcessInfo.ReadError(var Buffer; Count: Integer): Integer;
var
  BytesRead, BytesAvailable: DWORD;
begin
  Result := 0;
  if PipeHandles.ErrorRead <> 0 then
  begin
    BytesAvailable := 0;
    if PeekNamedPipe(PipeHandles.ErrorRead, nil, 0, nil, @BytesAvailable, nil) and
       (BytesAvailable > 0) then
    begin
      if Windows.ReadFile(PipeHandles.ErrorRead, Buffer, Count, BytesRead, nil) then
        Result := BytesRead;
    end
    else if not IsRunning then
    begin
      if Windows.ReadFile(PipeHandles.ErrorRead, Buffer, Count, BytesRead, nil) then
        Result := BytesRead;
    end;
  end;
end;

{$ENDIF MSWINDOWS}

// =============================================================================
// Implementación UNIX
// =============================================================================

{$IFDEF UNIX}

function TInteractiveProcessInfo.IsRunning: Boolean;
var
  Status: cint;
  Res   : TPid;
begin
  if FChildPID <= 0 then
  begin
    Running := False;
    Result  := False;
    Exit;
  end;

  Res := FpWaitPid(FChildPID, Status, WNOHANG);
  if Res = 0 then
  begin
    Running := True;
    Result  := True;
  end
  else if Res = FChildPID then
  begin
    Running := False;
    Result  := False;
    if wifexited(Status) then
      ExitCode := wexitstatus(Status)
    else
      ExitCode := 255;
    FChildPID := 0;
  end
  else
  begin
    Running   := False;
    Result    := False;
    FChildPID := 0;
  end;
end;

function TInteractiveProcessInfo.WaitOnExit(ATimeoutMs: Cardinal): Boolean;
var
  Status: cint;
begin
  if FChildPID <= 0 then
  begin
    Result := True;
    Exit;
  end;
  if FpWaitPid(FChildPID, Status, 0) = FChildPID then
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
    FpKill(FChildPID, SIGTERM);
    WaitOnExit(5000);
  end;
end;

procedure TInteractiveProcessInfo.Kill;
begin
  if IsRunning then
  begin
    FpKill(FChildPID, SIGKILL);
    WaitOnExit(5000);
  end;
end;

function TInteractiveProcessInfo.WriteInput(const Buffer; Count: Integer): Integer;
begin
  Result := -1;
  if (PipeHandles.InputWrite <> 0) and IsRunning then
    Result := Integer(FpWrite(PipeHandles.InputWrite, Buffer, LongWord(Count)));
end;

function TInteractiveProcessInfo.ReadOutput(var Buffer; Count: Integer): Integer;
begin
  Result := -1;
  if PipeHandles.OutputRead <> 0 then
  begin
    Result := Integer(FpRead(PipeHandles.OutputRead, Buffer, LongWord(Count)));
    if (Result = -1) and (fpGetErrno = ESysEAGAIN) then
      Result := 0;
  end;
end;

function TInteractiveProcessInfo.ReadError(var Buffer; Count: Integer): Integer;
begin
  Result := -1;
  if PipeHandles.ErrorRead <> 0 then
  begin
    Result := Integer(FpRead(PipeHandles.ErrorRead, Buffer, LongWord(Count)));
    if (Result = -1) and (fpGetErrno = ESysEAGAIN) then
      Result := 0;
  end;
end;

{$ENDIF UNIX}

// =============================================================================
// TUtilsSystem — Windows
// =============================================================================

{$IFDEF MSWINDOWS}

class function TUtilsSystem.ShellOpenFile(const AFileName: string): Boolean;
var
  ErrorCode: Integer;
begin
  ErrorCode := Integer(ShellExecute(0, 'open', PChar(AFileName), nil, nil, SW_SHOWNORMAL));
  Result := ErrorCode > 32;
end;

class function TUtilsSystem.StartInteractiveProcess(const ACommand: string;
    ACurrentDirectory: string; AEnvironment: TStrings): TInteractiveProcessInfo;
var
  SA          : TSecurityAttributes;
  SI          : TStartupInfo;
  PI          : TProcessInformation;
  Cmd         : string;
  EnvBlock    : string;
  EnvPtr      : PChar;
  CreationFlags: DWORD;
  I, EqPos    : Integer;
  SysEnv      : TStringList;
  DirToUse    : PChar;
begin
  Result := TInteractiveProcessInfo.Create;
  try
    FillChar(SA, SizeOf(SA), 0);
    SA.nLength        := SizeOf(SA);
    SA.bInheritHandle := True;

    if not CreatePipe(Result.PipeHandles.InputRead,  Result.PipeHandles.InputWrite,  @SA, 0) or
       not CreatePipe(Result.PipeHandles.OutputRead, Result.PipeHandles.OutputWrite, @SA, 0) or
       not CreatePipe(Result.PipeHandles.ErrorRead,  Result.PipeHandles.ErrorWrite,  @SA, 0) then
    begin
      Result.Free;
      Result := nil;
      RaiseLastOSError;
      Exit;
    end;

    SetHandleInformation(Result.PipeHandles.InputWrite,  HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(Result.PipeHandles.OutputRead,  HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(Result.PipeHandles.ErrorRead,   HANDLE_FLAG_INHERIT, 0);

    FillChar(SI, SizeOf(SI), 0);
    SI.cb          := SizeOf(SI);
    SI.hStdInput   := Result.PipeHandles.InputRead;
    SI.hStdOutput  := Result.PipeHandles.OutputWrite;
    SI.hStdError   := Result.PipeHandles.ErrorWrite;
    SI.dwFlags     := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;

    Cmd := ACommand;
    UniqueString(Cmd);

    EnvPtr        := nil;
    CreationFlags := CREATE_NO_WINDOW;
    EnvBlock      := '';

    if Assigned(AEnvironment) and (AEnvironment.Count > 0) then
    begin
      SysEnv := GetSystemEnvironment;
      try
        for I := 0 to AEnvironment.Count - 1 do
        begin
          EqPos := Pos('=', AEnvironment[I]);
          if EqPos > 0 then
            SysEnv.Values[Copy(AEnvironment[I], 1, EqPos - 1)] :=
                Copy(AEnvironment[I], EqPos + 1, MaxInt)
          else
            SysEnv.Add(AEnvironment[I]);
        end;
        for I := 0 to SysEnv.Count - 1 do
          EnvBlock := EnvBlock + SysEnv[I] + #0;
        EnvBlock      := EnvBlock + #0;
        EnvPtr        := PChar(EnvBlock);
        CreationFlags := CreationFlags or CREATE_UNICODE_ENVIRONMENT;
      finally
        SysEnv.Free;
      end;
    end;

    if Trim(ACurrentDirectory) = '' then
      DirToUse := nil
    else
      DirToUse := PChar(Trim(ACurrentDirectory));

    if CreateProcess(nil, PChar(Cmd), nil, nil, True, CreationFlags,
        EnvPtr, DirToUse, SI, PI) then
    begin
      Result.ProcessHandle := PI.hProcess;
      Result.ThreadHandle  := PI.hThread;
      Result.ProcessID     := PI.dwProcessId;
      Result.Running       := True;

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
  pEnv, pStart: PChar;
begin
  Result := TStringList.Create;
  pStart := GetEnvironmentStrings;
  if pStart <> nil then
    try
      pEnv := pStart;
      while pEnv^ <> #0 do
      begin
        Result.Add(String(pEnv));
        Inc(pEnv, StrLen(pEnv) + 1);
      end;
    finally
      FreeEnvironmentStrings(pStart);
    end;
end;

class function TUtilsSystem.RunCommandLine(ACommand: string): string;
var
  SA          : TSecurityAttributes;
  ReadPipe    : THandle;
  WritePipe   : THandle;
  SI          : TStartupInfo;
  PI          : TProcessInformation;
  Buffer      : array[0..2047] of Byte;
  BytesRead   : DWORD;
  Cmd         : string;
  Output      : TMemoryStream;
  S           : AnsiString;
begin
  Result := '';
  WritePipe := 0;
  Output := TMemoryStream.Create;
  try
    FillChar(SA, SizeOf(SA), 0);
    SA.nLength        := SizeOf(SA);
    SA.bInheritHandle := True;

    if CreatePipe(ReadPipe, WritePipe, @SA, 0) then
      try
        SetHandleInformation(ReadPipe, HANDLE_FLAG_INHERIT, 0);

        FillChar(SI, SizeOf(SI), 0);
        SI.cb          := SizeOf(SI);
        SI.hStdOutput  := WritePipe;
        SI.hStdError   := WritePipe;
        SI.dwFlags     := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        SI.wShowWindow := SW_HIDE;

        Cmd := 'cmd.exe /C ' + ACommand;
        UniqueString(Cmd);

        if CreateProcess(nil, PChar(Cmd), nil, nil, True, CREATE_NO_WINDOW,
            nil, nil, SI, PI) then
          try
            CloseHandle(WritePipe);
            WritePipe := 0;
            repeat
              BytesRead := 0;
              if Windows.ReadFile(ReadPipe, Buffer, SizeOf(Buffer), BytesRead, nil) and
                 (BytesRead > 0) then
                Output.WriteBuffer(Buffer, BytesRead);
            until not (BytesRead > 0);
            WaitForSingleObject(PI.hProcess, INFINITE);
          finally
            CloseHandle(PI.hProcess);
            CloseHandle(PI.hThread);
          end;
        SetString(S, PAnsiChar(Output.Memory), Output.Size);
        Result := String(S);
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
  SI     : TStartupInfo;
  PI     : TProcessInformation;
  ExCode : DWORD;
  Cmd    : string;
begin
  Result := False;
  FillChar(SI, SizeOf(SI), 0);
  FillChar(PI, SizeOf(PI), 0);
  SI.cb          := SizeOf(TStartupInfo);
  SI.dwFlags     := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_SHOWNORMAL;

  Cmd := ACommand;
  UniqueString(Cmd);

  if CreateProcess(nil, PChar(Cmd), nil, nil, False, 0, nil, nil, SI, PI) then
  begin
    WaitForSingleObject(PI.hProcess, INFINITE);
    GetExitCodeProcess(PI.hProcess, ExCode);
    Result := (ExCode = 0);
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
  end;
end;

{$ENDIF MSWINDOWS}

// =============================================================================
// TUtilsSystem — UNIX
// =============================================================================

{$IFDEF UNIX}

type
  TStringDynArray  = array of string;
  TAnsiStrDynArray = array of AnsiString;
  TPCharDynArray   = array of PAnsiChar;

procedure ParseCommand(const ACommand: string; out AProgram: string;
    out AArgs: TStringDynArray);
var
  ArgsList  : TStringList;
  I         : Integer;
  CurrentArg: string;
  InQuotes  : Boolean;
  C         : Char;
begin
  ArgsList := TStringList.Create;
  try
    CurrentArg := '';
    InQuotes   := False;
    for I := 1 to Length(ACommand) do
    begin
      C := ACommand[I];
      if (C = ' ') and not InQuotes then
      begin
        if CurrentArg <> '' then
        begin
          ArgsList.Add(CurrentArg);
          CurrentArg := '';
        end;
      end
      else if C = '"' then
        InQuotes := not InQuotes
      else
        CurrentArg := CurrentArg + C;
    end;
    if CurrentArg <> '' then
      ArgsList.Add(CurrentArg);

    if ArgsList.Count > 0 then
    begin
      AProgram := ArgsList[0];
      SetLength(AArgs, ArgsList.Count);
      for I := 0 to ArgsList.Count - 1 do
        AArgs[I] := ArgsList[I];
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

class function TUtilsSystem.StartInteractiveProcess(const ACommand: string;
    ACurrentDirectory: string; AEnvironment: TStrings): TInteractiveProcessInfo;
var
  PipeIn    : array[0..1] of cint;
  PipeOut   : array[0..1] of cint;
  PipeErr   : array[0..1] of cint;
  PID       : TPid;
  ProgramName: string;
  Args      : TStringDynArray;
  AnsiArgs  : TAnsiStrDynArray;
  PArgs     : TPCharDynArray;
  I, EqPos  : Integer;
  EnvName   : AnsiString;
  EnvValue  : AnsiString;
begin
  Result := TInteractiveProcessInfo.Create;
  try
    if FpPipe(PipeIn[0], PipeIn[1]) <> 0 then
      raise Exception.Create('Failed to create input pipe. Error: ' +
          String(strerror(fpGetErrno)));

    if FpPipe(PipeOut[0], PipeOut[1]) <> 0 then
    begin
      FpClose(PipeIn[0]); FpClose(PipeIn[1]);
      raise Exception.Create('Failed to create output pipe. Error: ' +
          String(strerror(fpGetErrno)));
    end;

    if FpPipe(PipeErr[0], PipeErr[1]) <> 0 then
    begin
      FpClose(PipeIn[0]);  FpClose(PipeIn[1]);
      FpClose(PipeOut[0]); FpClose(PipeOut[1]);
      raise Exception.Create('Failed to create error pipe. Error: ' +
          String(strerror(fpGetErrno)));
    end;

    PID := FpFork;

    if PID < 0 then
    begin
      FpClose(PipeIn[0]);  FpClose(PipeIn[1]);
      FpClose(PipeOut[0]); FpClose(PipeOut[1]);
      FpClose(PipeErr[0]); FpClose(PipeErr[1]);
      raise Exception.Create('Failed to fork process. Error: ' +
          String(strerror(fpGetErrno)));
    end;

    if PID = 0 then  // Proceso hijo
    begin
      FpClose(PipeIn[1]);
      FpClose(PipeOut[0]);
      FpClose(PipeErr[0]);

      FpDup2(PipeIn[0],  STDIN_FILENO);
      FpDup2(PipeOut[1], STDOUT_FILENO);
      FpDup2(PipeErr[1], STDERR_FILENO);

      FpClose(PipeIn[0]);
      FpClose(PipeOut[1]);
      FpClose(PipeErr[1]);

      if ACurrentDirectory <> '' then
        FpChdir(ACurrentDirectory);

      if Assigned(AEnvironment) then
        for I := 0 to AEnvironment.Count - 1 do
        begin
          EqPos := Pos('=', AEnvironment[I]);
          if EqPos > 0 then
          begin
            EnvName  := AnsiString(Copy(AEnvironment[I], 1, EqPos - 1));
            EnvValue := AnsiString(Copy(AEnvironment[I], EqPos + 1, MaxInt));
            setenv(PAnsiChar(EnvName), PAnsiChar(EnvValue), 1);
          end;
        end;

      ParseCommand(ACommand, ProgramName, Args);
      if ProgramName = '' then
        unix_exit(127);

      SetLength(AnsiArgs, Length(Args));
      for I := 0 to High(Args) do
        AnsiArgs[I] := AnsiString(Args[I]);

      SetLength(PArgs, Length(AnsiArgs) + 1);
      for I := 0 to High(AnsiArgs) do
        PArgs[I] := PAnsiChar(AnsiArgs[I]);
      PArgs[Length(AnsiArgs)] := nil;

      execvp(PAnsiChar(AnsiString(ProgramName)), @PArgs[0]);
      unix_exit(127);
    end
    else  // Proceso padre
    begin
      FpClose(PipeIn[0]);
      FpClose(PipeOut[1]);
      FpClose(PipeErr[1]);

      Result.PipeHandles.InputWrite  := PipeIn[1];
      Result.PipeHandles.OutputRead  := PipeOut[0];
      Result.PipeHandles.ErrorRead   := PipeErr[0];
      Result.ProcessHandle           := PID;
      Result.FChildPID               := PID;
      Result.Running                 := True;

      FpFcntl(Result.PipeHandles.OutputRead, F_SETFL, O_NONBLOCK);
      FpFcntl(Result.PipeHandles.ErrorRead,  F_SETFL, O_NONBLOCK);
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
  P: PPAnsiChar;
begin
  Result := TStringList.Create;
  P := environ;
  if P <> nil then
    while P^ <> nil do
    begin
      Result.Add(String(P^));
      Inc(P);
    end;
end;

class function TUtilsSystem.RunCommandLine(ACommand: string): string;
var
  Handle: Pointer;
  Buffer: array[0..1023] of AnsiChar;
  Output: string;
begin
  Result := '';
  Output := '';
  Handle := popen(PAnsiChar(AnsiString(ACommand)), PAnsiChar(AnsiString('r')));
  if Handle = nil then
    raise Exception.CreateFmt('Failed to popen command: %s', [ACommand]);
  try
    while fgets(@Buffer[0], SizeOf(Buffer), Handle) <> nil do
      Output := Output + String(Buffer);
    Result := Trim(Output);
  finally
    pclose(Handle);
  end;
end;

class function TUtilsSystem.ExecuteCommandLine(ACommand: string): Boolean;
var
  Status: Integer;
begin
  Status := c_system(PAnsiChar(AnsiString(ACommand)));
  Result := (Status = 0);
end;

{$ENDIF UNIX}

// =============================================================================
// TUtilsSystem — Común
// =============================================================================

class procedure TUtilsSystem.StopInteractiveProcess(
    var AProcessInfo: TInteractiveProcessInfo);
begin
  if Assigned(AProcessInfo) then
  begin
    AProcessInfo.Free;
    AProcessInfo := nil;
  end;
end;

end.
