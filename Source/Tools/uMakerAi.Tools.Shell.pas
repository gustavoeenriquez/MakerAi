// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.Tools.Shell
// Shell interactivo con sesión persistente y detección automática de formato JSON.
unit uMakerAi.Tools.Shell;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  uMakerAi.Utils.System;

type

  // Resultado interno de un comando
  TShellExecutionResult = record
    StdOut   : string;
    StdErr   : string;
    ExitCode : Integer;
    TimedOut : Boolean;
  end;

  // Evento para interceptar/auditar comandos antes de ejecutar
  TAiShellCommandEvent = procedure(Sender: TObject; const Command: string;
      const CallId: string; var Result: TShellExecutionResult;
      var Handled: Boolean) of object;
  TAiShellLogEvent = procedure(Sender: TObject; const Command: string;
      const StdOut, StdErr: string; ExitCode: Integer) of object;

  TAiShell = class(TComponent)
  private
    FSession      : TInteractiveProcessInfo;
    FTimeOut      : Cardinal;
    FShellPath    : string;
    FOnCommand    : TAiShellCommandEvent;
    FActive       : Boolean;
    FEnvironment  : TStringList;
    FMaxOutputSize: Integer;
    FOnConsoleLog : TAiShellLogEvent;

    procedure SetActive(const Value: Boolean);
    procedure SetShellPath(const Value: string);
    procedure SetEnvironment(const Value: TStringList);
    function  GenerateSentinel: string;
    function  CleanOutput(const RawOutput, Sentinel: string): string;
    procedure StartSession;
    procedure StopSession;

    // Ejecución de bajo nivel
    function InternalExecuteCommand(const ACommand: string;
        TimeOutMs: Cardinal): TShellExecutionResult;

    // Métodos específicos por proveedor
    function ExecuteClaudeAction (const CallId: string; JArgs: TJSONObject): string;
    function ExecuteOpenAIAction (const CallId: string; JArgs: TJSONObject): string;
    function ExecuteGenericAction(const CallId: string; JArgs: TJSONObject): string;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    // Punto de entrada único — detecta formato y delega
    function Execute(const CallId: string;
        const JsonArguments: string): string; overload;
    function Execute(const CallId: string;
        JArgs: TJSONObject): string; overload;
    function ExecuteManual(const Command: string): string;
    procedure Restart;

  published
    property Active       : Boolean             read FActive        write SetActive        default False;
    property TimeOut      : Cardinal            read FTimeOut       write FTimeOut         default 30000;
    property ShellPath    : string              read FShellPath     write SetShellPath;
    property MaxOutputSize: Integer             read FMaxOutputSize write FMaxOutputSize   default 20000;
    property Environment  : TStringList         read FEnvironment   write SetEnvironment;
    property OnCommand    : TAiShellCommandEvent read FOnCommand    write FOnCommand;
    property OnConsoleLog : TAiShellLogEvent    read FOnConsoleLog  write FOnConsoleLog;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiShell]);
end;

{ TAiShell }

constructor TAiShell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeOut       := 30000;
  FMaxOutputSize := 20000;
  FActive        := False;
  FEnvironment   := TStringList.Create;
  FSession       := nil;
{$IFDEF MSWINDOWS}
  FShellPath := 'cmd.exe';
{$ENDIF}
{$IFDEF UNIX}
  FShellPath := '/bin/bash';
{$ENDIF}
end;

destructor TAiShell.Destroy;
begin
  StopSession;
  FEnvironment.Free;
  inherited;
end;

procedure TAiShell.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      StartSession
    else
      StopSession;
  end;
end;

procedure TAiShell.SetShellPath(const Value: string);
var
  WasRunning: Boolean;
begin
  if SameText(FShellPath, Value) then
    Exit;

  WasRunning := Assigned(FSession);
  StopSession;
  FShellPath := Value;

  if WasRunning or FActive then
    StartSession;
end;

procedure TAiShell.SetEnvironment(const Value: TStringList);
begin
  FEnvironment.Assign(Value);
end;

procedure TAiShell.StartSession;
begin
  if Assigned(FSession) then
  begin
    if FSession.IsRunning then
      Exit;
    // Sesión muerta: limpiar antes de recrear
    StopSession;
  end;
  if Trim(FShellPath) = '' then
    raise Exception.Create('ShellPath cannot be empty.');
  FSession := TUtilsSystem.StartInteractiveProcess(FShellPath, '', FEnvironment);
end;

procedure TAiShell.StopSession;
begin
  if Assigned(FSession) then
  begin
    TUtilsSystem.StopInteractiveProcess(FSession);
    FSession := nil;
  end;
end;

procedure TAiShell.Restart;
begin
  StopSession;
  if FActive then
    StartSession;
end;

function TAiShell.GenerateSentinel: string;
begin
  Result := 'MKAI_END_' + IntToHex(Random(MaxInt), 8);
end;

function TAiShell.CleanOutput(const RawOutput, Sentinel: string): string;
begin
  Result := StringReplace(RawOutput, Sentinel, '', [rfReplaceAll]);
  Result := Trim(Result);
end;

// =============================================================================
// NUCLEO DE EJECUCIÓN (Bajo Nivel)
// =============================================================================

function TAiShell.InternalExecuteCommand(const ACommand: string;
    TimeOutMs: Cardinal): TShellExecutionResult;
var
  FullCommand : string;
  Sentinel    : string;
  Buffer      : array of Byte;
  BytesRead   : Integer;
  OutStr      : string;
  ErrStr      : string;
  RawStr      : string;
  RawUtf8     : AnsiString;
  InputBytes  : array of Byte;
  StartTick   : QWord;
  IsLinuxStyle: Boolean;
  P, SpacePos : Integer;
  CodeStr     : string;
begin
  Result.ExitCode := 0;
  Result.TimedOut := False;
  Result.StdOut   := '';
  Result.StdErr   := '';

  if not Assigned(FSession) or not FSession.IsRunning then
    StartSession;

  Sentinel := GenerateSentinel;
  SetLength(Buffer, 4096);
  OutStr := '';
  ErrStr := '';

  // -------------------------------------------------------------------------
  // 1. DETERMINAR SINTAXIS SEGÚN EL SHELL (NO SEGÚN EL SO)
  // -------------------------------------------------------------------------
  IsLinuxStyle := False;
{$IFDEF UNIX}
  IsLinuxStyle := True;
{$ENDIF}
  if (Pos('wsl',  LowerCase(ExtractFileName(FShellPath))) > 0) or
     (Pos('bash', LowerCase(ExtractFileName(FShellPath))) > 0) or
     (Pos('sh',   LowerCase(ExtractFileName(FShellPath))) > 0) then
    IsLinuxStyle := True;

  if IsLinuxStyle then
    FullCommand := ACommand + '; echo "EC:$? ' + Sentinel + '"' + #10
  else
    FullCommand := ACommand + LineEnding + '@echo EC:%ERRORLEVEL% ' + Sentinel + LineEnding;

  // -------------------------------------------------------------------------
  // 2. CODIFICACIÓN DE INPUT
  // -------------------------------------------------------------------------
  RawUtf8 := AnsiString(FullCommand);
  SetLength(InputBytes, Length(RawUtf8));
  if Length(RawUtf8) > 0 then
    Move(RawUtf8[1], InputBytes[0], Length(RawUtf8));

  if Length(InputBytes) > 0 then
    FSession.WriteInput(InputBytes[0], Length(InputBytes));

  // -------------------------------------------------------------------------
  // 3. BUCLE DE LECTURA CON SENTINEL
  // -------------------------------------------------------------------------
  StartTick := GetTickCount64;

  while (GetTickCount64 - StartTick) < QWord(TimeOutMs) do
  begin
    // Leer STDOUT
    BytesRead := FSession.ReadOutput(Buffer[0], Length(Buffer));
    if BytesRead > 0 then
    begin
      SetString(RawStr, PAnsiChar(@Buffer[0]), BytesRead);
      OutStr := OutStr + RawStr;

      if Pos(Sentinel, OutStr) > 0 then
        Break;

      StartTick := GetTickCount64;  // Reset timeout en actividad
    end;

    // Leer STDERR
    BytesRead := FSession.ReadError(Buffer[0], Length(Buffer));
    if BytesRead > 0 then
    begin
      SetString(RawStr, PAnsiChar(@Buffer[0]), BytesRead);
      ErrStr := ErrStr + RawStr;
    end;

    Sleep(20);
  end;

  if (GetTickCount64 - StartTick) >= QWord(TimeOutMs) then
    Result.TimedOut := True;

  // -------------------------------------------------------------------------
  // 4. PROCESAMIENTO DE SALIDA
  // -------------------------------------------------------------------------
  Result.StdOut := CleanOutput(OutStr, Sentinel);
  Result.StdErr := ErrStr;

  // Capturar Exit Code (Windows CMD y Linux/WSL)
  P := Pos('EC:', OutStr);
  if P > 0 then
  begin
    CodeStr  := Copy(OutStr, P + 3, 5);
    SpacePos := Pos(' ', CodeStr);
    if SpacePos > 0 then
    begin
      CodeStr         := Copy(CodeStr, 1, SpacePos - 1);
      Result.ExitCode := StrToIntDef(CodeStr, 0);
      Result.StdOut   := StringReplace(Result.StdOut, 'EC:' + CodeStr, '', []);
    end;
  end;

  if Length(Result.StdOut) > FMaxOutputSize then
    Result.StdOut := Copy(Result.StdOut, 1, FMaxOutputSize) + '... [Truncated]';

  if Assigned(FOnConsoleLog) then
    FOnConsoleLog(Self, ACommand, Result.StdOut, Result.StdErr, Result.ExitCode);
end;

// =============================================================================
// DISPATCHER PRINCIPAL (Smart Detection)
// =============================================================================

function TAiShell.Execute(const CallId: string;
    const JsonArguments: string): string;
var
  JVal : TJSONData;
  JArgs: TJSONObject;
begin
  JVal := GetJSON(JsonArguments);
  try
    if not (JVal is TJSONObject) then
    begin
      Result := 'Error: Invalid JSON arguments.';
      Exit;
    end;
    JArgs  := TJSONObject(JVal);
    Result := Execute(CallId, JArgs);
  finally
    JVal.Free;
  end;
end;

function TAiShell.Execute(const CallId: string; JArgs: TJSONObject): string;
begin
  // CASO 1: OPENAI — tiene array 'commands'
  if JArgs.Find('commands') is TJSONArray then
    Result := ExecuteOpenAIAction(CallId, JArgs)
  // CASO 2: CLAUDE — tiene string 'command' o flag 'restart'
  else if (JArgs.Find('command') <> nil) or (JArgs.Find('restart') <> nil) then
    Result := ExecuteClaudeAction(CallId, JArgs)
  // CASO 3: GENÉRICO — fallback
  else
    Result := ExecuteGenericAction(CallId, JArgs);
end;

// =============================================================================
// IMPLEMENTACIONES ESPECÍFICAS
// =============================================================================

function TAiShell.ExecuteClaudeAction(const CallId: string;
    JArgs: TJSONObject): string;
var
  Cmd    : string;
  ExecRes: TShellExecutionResult;
  Handled: Boolean;
  D      : TJSONData;
begin
  // Verificar restart — if anidados para evitar evaluación Variant del 'and'
  D := JArgs.Find('restart');
  if D is TJSONBoolean then
    if TJSONBoolean(D).Value then
    begin
      Restart;
      Result := 'Shell session restarted.';
      Exit;
    end;

  D := JArgs.Find('command');
  if D = nil then
  begin
    Result := 'Error: No command provided.';
    Exit;
  end;
  Cmd := D.AsString;

  ExecRes.StdOut   := '';
  ExecRes.StdErr   := '';
  ExecRes.ExitCode := 0;
  ExecRes.TimedOut := False;
  Handled := False;

  if Assigned(FOnCommand) then
    FOnCommand(Self, Cmd, CallId, ExecRes, Handled);

  if not Handled then
    ExecRes := InternalExecuteCommand(Cmd, FTimeOut);

  if ExecRes.TimedOut then
    Result := 'Error: Command timed out.' + LineEnding + ExecRes.StdOut
  else if ExecRes.StdErr <> '' then
    Result := ExecRes.StdOut + LineEnding + 'STDERR: ' + ExecRes.StdErr
  else
  begin
    if ExecRes.StdOut = '' then
      Result := '[Command executed successfully]'
    else
      Result := ExecRes.StdOut;
  end;
end;

function TAiShell.ExecuteOpenAIAction(const CallId: string;
    JArgs: TJSONObject): string;
var
  CommandsArray: TJSONArray;
  OutputObj    : TJSONObject;
  OutputArray  : TJSONArray;
  ItemOut      : TJSONObject;
  OutcomeObj   : TJSONObject;
  I            : Integer;
  Cmd          : string;
  ExecRes      : TShellExecutionResult;
  LocalTimeOut : Cardinal;
  Handled      : Boolean;
  D            : TJSONData;
begin
  OutputObj := TJSONObject.Create;
  try
    OutputObj.Add('type', 'shell_call_output');
    OutputObj.Add('call_id', CallId);

    LocalTimeOut := FTimeOut;
    D := JArgs.Find('timeout_ms');
    if (D <> nil) and not (D is TJSONNull) then
      try
        LocalTimeOut := Cardinal(D.AsInt64);
      except
        LocalTimeOut := FTimeOut;
      end;

    D := JArgs.Find('max_output_length');
    if D <> nil then
      OutputObj.Add('max_output_length', D.AsInteger);

    OutputArray := TJSONArray.Create;
    OutputObj.Add('output', OutputArray);

    D := JArgs.Find('commands');
    if (D <> nil) and (D is TJSONArray) then
    begin
      CommandsArray := TJSONArray(D);
      for I := 0 to CommandsArray.Count - 1 do
      begin
        Cmd    := CommandsArray.Items[I].AsString;
        ItemOut := TJSONObject.Create;

        ExecRes.StdOut   := '';
        ExecRes.StdErr   := '';
        ExecRes.ExitCode := 0;
        ExecRes.TimedOut := False;
        Handled := False;

        if Assigned(FOnCommand) then
          FOnCommand(Self, Cmd, CallId, ExecRes, Handled);

        if not Handled then
          ExecRes := InternalExecuteCommand(Cmd, LocalTimeOut);

        ItemOut.Add('stdout', ExecRes.StdOut);
        ItemOut.Add('stderr', ExecRes.StdErr);

        OutcomeObj := TJSONObject.Create;
        if ExecRes.TimedOut then
          OutcomeObj.Add('type', 'timeout')
        else
        begin
          OutcomeObj.Add('type', 'exit');
          OutcomeObj.Add('exit_code', ExecRes.ExitCode);
        end;
        ItemOut.Add('outcome', OutcomeObj);
        OutputArray.Add(ItemOut);
      end;
    end;

    Result := OutputObj.AsJSON;
  finally
    OutputObj.Free;
  end;
end;

function TAiShell.ExecuteGenericAction(const CallId: string;
    JArgs: TJSONObject): string;
var
  Cmd    : string;
  ExecRes: TShellExecutionResult;
  Handled: Boolean;
  D      : TJSONData;
begin
  D := JArgs.Find('command');
  if D = nil then
    D := JArgs.Find('cmd');
  if D = nil then
  begin
    Result := 'Error: Unknown command format.';
    Exit;
  end;
  Cmd := D.AsString;

  ExecRes.StdOut   := '';
  ExecRes.StdErr   := '';
  ExecRes.ExitCode := 0;
  ExecRes.TimedOut := False;
  Handled := False;

  if Assigned(FOnCommand) then
    FOnCommand(Self, Cmd, CallId, ExecRes, Handled);

  if not Handled then
    ExecRes := InternalExecuteCommand(Cmd, FTimeOut);

  if ExecRes.StdErr <> '' then
    Result := 'STDOUT: ' + ExecRes.StdOut + LineEnding + 'STDERR: ' + ExecRes.StdErr
  else
    Result := ExecRes.StdOut;
end;

function TAiShell.ExecuteManual(const Command: string): string;
var
  ExecRes: TShellExecutionResult;
begin
  if not Active then
    Active := True;

  ExecRes := InternalExecuteCommand(Command, FTimeOut);

  if ExecRes.TimedOut then
    Result := 'Error: Command timed out.' + LineEnding + ExecRes.StdOut
  else if ExecRes.StdErr <> '' then
    Result := ExecRes.StdOut + LineEnding + 'STDERR: ' + ExecRes.StdErr
  else
    Result := ExecRes.StdOut;
end;

end.
