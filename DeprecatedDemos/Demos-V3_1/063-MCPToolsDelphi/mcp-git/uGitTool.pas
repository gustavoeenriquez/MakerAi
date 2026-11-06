unit uGitTool;

interface

uses
  System.SysUtils, System.StrUtils, System.JSON,
  uMakerAi.MCPServer.Core;

type
  TGitCommandParams = class
  private
    FCommand: string;
    FWorkingDir: string;
    FTimeout: Integer;
  public
    [AiMCPSchemaDescription('Git subcommand and arguments. Do NOT include the "git" prefix. ' +
      'Examples: "status", "log --oneline -10", "diff HEAD", "branch -a", "show HEAD:file.txt"')]
    property Command: string read FCommand write FCommand;
    [AiMCPSchemaDescription('Working directory (repository root). Default: current directory')]
    [AiMCPOptional]
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    [AiMCPSchemaDescription('Command timeout in seconds. Default: 15')]
    [AiMCPOptional]
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  TGitCommandTool = class(TAiMCPToolBase<TGitCommandParams>)
  protected
    function ExecuteWithParams(const AParams: TGitCommandParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 2: git_clone ---
  TGitCloneParams = class
  private
    FUrl: string;
    FDestination: string;
    FBranch: string;
    FDepth: Integer;
  public
    [AiMCPSchemaDescription('Repository URL to clone')]
    property Url: string read FUrl write FUrl;
    [AiMCPSchemaDescription('Local destination directory. Default: directory name derived from URL')]
    [AiMCPOptional]
    property Destination: string read FDestination write FDestination;
    [AiMCPSchemaDescription('Branch or tag to checkout. Default: repository default branch')]
    [AiMCPOptional]
    property Branch: string read FBranch write FBranch;
    [AiMCPSchemaDescription('Shallow clone depth (0 = full history). Default: 0')]
    [AiMCPOptional]
    property Depth: Integer read FDepth write FDepth;
  end;

  TGitCloneTool = class(TAiMCPToolBase<TGitCloneParams>)
  protected
    function ExecuteWithParams(const AParams: TGitCloneParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

function ExecAndCapture(const ACmdLine: string; const AWorkDir: string; ATimeoutMs: Integer): string;

implementation

uses
  Winapi.Windows, System.IOUtils;

// -------------------------------------------------------------------------
// Process execution helper — runs a command, captures stdout+stderr
// -------------------------------------------------------------------------

function ExecAndCapture(const ACmdLine: string; const AWorkDir: string; ATimeoutMs: Integer): string;
var
  SA:          TSecurityAttributes;
  hRead, hWrite: THandle;
  SI:          TStartupInfo;
  PI:          TProcessInformation;
  Buf:         array[0..8191] of AnsiChar;
  BytesRead:   DWORD;
  Captured:    RawByteString;
  WorkDirPtr:  PChar;
  CmdBuf:      string;
  ExitCode:    DWORD;
begin
  Result := '';
  if ATimeoutMs <= 0 then ATimeoutMs := 15000;

  SA.nLength              := SizeOf(SA);
  SA.bInheritHandle       := True;
  SA.lpSecurityDescriptor := nil;

  if not CreatePipe(hRead, hWrite, @SA, 0) then
    raise Exception.CreateFmt('CreatePipe failed: %s', [SysErrorMessage(GetLastError)]);

  // Prevent the read end from being inherited by child
  SetHandleInformation(hRead, HANDLE_FLAG_INHERIT, 0);

  FillChar(SI, SizeOf(SI), 0);
  SI.cb          := SizeOf(SI);
  SI.dwFlags     := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;
  SI.hStdOutput  := hWrite;
  SI.hStdError   := hWrite;
  SI.hStdInput   := GetStdHandle(STD_INPUT_HANDLE);

  // Use 'cmd.exe /C' so we get PATH resolution for 'git'
  CmdBuf := 'cmd.exe /C ' + ACmdLine;

  if Trim(AWorkDir) <> '' then
    WorkDirPtr := PChar(AWorkDir)
  else
    WorkDirPtr := nil;

  FillChar(PI, SizeOf(PI), 0);
  if not CreateProcess(nil, PChar(CmdBuf), nil, nil, True,
                       CREATE_NO_WINDOW, nil, WorkDirPtr, SI, PI) then
  begin
    CloseHandle(hRead);
    CloseHandle(hWrite);
    raise Exception.CreateFmt('Cannot start process "%s": %s',
      [ACmdLine, SysErrorMessage(GetLastError)]);
  end;

  CloseHandle(hWrite);
  CloseHandle(PI.hThread);

  Captured := '';
  repeat
    FillChar(Buf[0], SizeOf(Buf), 0);
    if not ReadFile(hRead, Buf[0], SizeOf(Buf) - 1, BytesRead, nil) then Break;
    if BytesRead = 0 then Break;
    Buf[BytesRead] := #0;
    Captured := Captured + PAnsiChar(@Buf[0]);
  until False;

  WaitForSingleObject(PI.hProcess, ATimeoutMs);
  GetExitCodeProcess(PI.hProcess, ExitCode);
  CloseHandle(PI.hProcess);
  CloseHandle(hRead);

  Result := UTF8ToString(Captured);
  if Result = '' then
    Result := '(no output)';
  if ExitCode <> 0 then
    Result := Format('[exit code: %d]'#10, [ExitCode]) + Result;
end;

// -------------------------------------------------------------------------
// TGitCommandTool
// -------------------------------------------------------------------------

constructor TGitCommandTool.Create;
begin
  inherited;
  FName        := 'git_command';
  FDescription := 'Execute any git command and return its output. ' +
                  'Provide only the subcommand (without "git" prefix). ' +
                  'Examples: "status", "log --oneline -5", "diff", "branch -a". ' +
                  'CAUTION: Destructive commands (reset --hard, push --force) are allowed — use with care.';
end;

function TGitCommandTool.ExecuteWithParams(const AParams: TGitCommandParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Cmd:     string;
  WorkDir: string;
  Timeout: Integer;
  Output:  string;
begin
  if Trim(AParams.Command) = '' then
    raise Exception.Create('Command cannot be empty');

  // Disallow some obviously dangerous patterns
  var CmdLower := LowerCase(Trim(AParams.Command));
  if ContainsText(CmdLower, 'remote add') or
     ContainsText(CmdLower, 'push -f') or
     ContainsText(CmdLower, 'push --force') then
    raise Exception.Create('This git command is restricted for safety');

  Cmd     := 'git ' + Trim(AParams.Command);
  WorkDir := Trim(AParams.WorkingDir);
  Timeout := AParams.Timeout;
  if Timeout <= 0 then Timeout := 15;

  // Validate working dir if provided
  if (WorkDir <> '') and not TDirectory.Exists(WorkDir) then
    raise Exception.CreateFmt('Working directory not found: "%s"', [WorkDir]);

  Output := ExecAndCapture(Cmd, WorkDir, Timeout * 1000);
  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

// -------------------------------------------------------------------------
// TGitCloneTool
// -------------------------------------------------------------------------

constructor TGitCloneTool.Create;
begin
  inherited;
  FName        := 'git_clone';
  FDescription := 'Clone a git repository. Supports shallow clones and specific branch checkout.';
end;

function TGitCloneTool.ExecuteWithParams(const AParams: TGitCloneParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Cmd: string;
begin
  if Trim(AParams.Url) = '' then
    raise Exception.Create('Repository URL cannot be empty');

  Cmd := 'git clone';
  if Trim(AParams.Branch) <> '' then
    Cmd := Cmd + ' --branch ' + AParams.Branch;
  if AParams.Depth > 0 then
    Cmd := Cmd + ' --depth ' + IntToStr(AParams.Depth);
  Cmd := Cmd + ' ' + AParams.Url;
  if Trim(AParams.Destination) <> '' then
    Cmd := Cmd + ' "' + AParams.Destination + '"';

  var Output := ExecAndCapture(Cmd, '', 120000); // 2 min timeout for clone
  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

end.
