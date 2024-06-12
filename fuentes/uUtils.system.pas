unit uUtils.system;

interface

uses
  system.SysUtils, system.Classes,
{$IFDEF POSIX}
  Posix.Base, Posix.Fcntl;
{$ENDIF}
{$IFDEF MSWINDOWS}
WinApi.ShellAPI, WinApi.Windows,
{$ENDIF}
system.IOUtils;

type
  TStreamHandle = pointer;

  TUtilsSystem = class
  public
    class function RunCommandLine(ACommand: string): String; overload;
    class function ExcecuteCommandLine(ACommand: string): Boolean;
{$IFDEF POSIX}
    class function RunCommandLine(ACommand: string; Return: TProc<String>): Boolean; overload;
{$ENDIF}
  end;

{$IFDEF POSIX}

function popen(const command: MarshaledAString; const _type: MarshaledAString): TStreamHandle; cdecl; external libc name _PU + 'popen';
function pclose(filehandle: TStreamHandle): int32; cdecl; external libc name _PU + 'pclose';
function fgets(buffer: pointer; size: int32; Stream: TStreamHandle): pointer; cdecl; external libc name _PU + 'fgets';
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

class function TUtilsSystem.RunCommandLine(ACommand: string): String;
var
  SecurityAttributes: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  buffer: array [0 .. 2047] of Byte;
  BytesRead: DWORD;
  CommandOutput: TStringStream;
begin
  Result := '';
  FillChar(SecurityAttributes, SizeOf(SecurityAttributes), 0);
  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.bInheritHandle := True;

  if CreatePipe(ReadPipe, WritePipe, @SecurityAttributes, 0) then
    try
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      StartupInfo.cb := SizeOf(StartupInfo);
      StartupInfo.hStdOutput := WritePipe;
      StartupInfo.hStdError := WritePipe;
      StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      StartupInfo.wShowWindow := SW_HIDE; // No mostrar la ventana de la consola

      if CreateProcess(nil, PChar('cmd /C ' + ACommand), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
        try
          CloseHandle(WritePipe);
          CommandOutput := TStringStream.Create('', TEncoding.Ansi);
          try
            repeat
              BytesRead := 0;
              if ReadFile(ReadPipe, buffer, SizeOf(buffer), BytesRead, nil) then
                if BytesRead > 0 then
                  CommandOutput.WriteBuffer(buffer, BytesRead);
            until BytesRead = 0;
            Result := CommandOutput.DataString;
          finally
            CommandOutput.Free;
          end;
          WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
        finally
          CloseHandle(ProcessInfo.hProcess);
          CloseHandle(ProcessInfo.hThread);
        end;
    finally
      CloseHandle(ReadPipe);
    end;
end;

class function TUtilsSystem.ExcecuteCommandLine(ACommand: string): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: DWORD;
begin
  Result := False;
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
  StartupInfo.cb := SizeOf(TStartupInfo);

  // Asegurarse de que el proceso se inicie en el primer plano.
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOWNORMAL;

  if CreateProcess(nil, PChar(ACommand), nil, nil, False, 0, nil, nil, StartupInfo, ProcessInfo) then
  begin
    // Esperar a que termine el proceso.
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);

    // Comprobar si el comando fue exitoso según el código de salida.
    Result := (ExitCode = 0);

    // Cerrar los handles del proceso.
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;


{$ENDIF}
{$IFDEF POSIX}

class function TUtilsSystem.RunCommandLine(ACommand: string): String;
var
  Handle: TStreamHandle;
  Data: array [0 .. 511] of uint8;
  M: TMarshaller;

begin
  Result := TStringList.Create;
  try
    Handle := popen(M.AsAnsi(PWideChar(ACommand)).ToPointer, 'r');
    try
      while fgets(@Data[0], SizeOf(Data), Handle) <> nil do
      begin
        Result.Add(Copy(UTF8ToString(@Data[0]), 1, UTF8ToString(@Data[0]).Length - 1));
      end;
    finally
      pclose(Handle);
    end;
  except
    on E: Exception do
      Result.Add(E.ClassName + ': ' + E.Message);
  end;
end;

class function TUtilsSystem.RunCommandLine(ACommand: string; Return: TProc<string>): Boolean;
var
  Handle: TStreamHandle;
  Data: array [0 .. 511] of uint8;
  M: TMarshaller;

begin
  Result := False;
  try
    Handle := popen(M.AsAnsi(PWideChar(ACommand)).ToPointer, 'r');
    try
      while fgets(@Data[0], SizeOf(Data), Handle) <> nil do
      begin
        Return(Copy(UTF8ToString(@Data[0]), 1, UTF8ToString(@Data[0]).Length - 1));
      end;
    finally
      pclose(Handle);
    end;
  except
    on E: Exception do
      Return(E.ClassName + ': ' + E.Message);
  end;
end;

class function TUtilsSystem.ExcecuteCommandLine(ACommand: string): Boolean;
var
  AProcess: TProcess;
begin
  Result := False;
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := '/bin/sh'; // Asumiendo que /bin/sh está disponible
    AProcess.Parameters.Add('-c');
    AProcess.Parameters.Add(ACommand);

    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
      AProcess.Execute;
      Result := AProcess.ExitStatus = 0;
    except
      // Manejar excepciones si ocurren
      on E: Exception do
        Result := False;
    end;
  finally
    AProcess.Free;
  end;
End;

{$ENDIF}

end.
