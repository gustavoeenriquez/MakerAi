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

unit uMakerAi.Tools.Shell;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils, System.Diagnostics,
  System.Generics.Collections, uMakerAi.Utils.System;

type
  // Estructura interna del resultado de un comando
  TShellExecutionResult = record
    StdOut: string;
    StdErr: string;
    ExitCode: Integer;
    TimedOut: Boolean;
  end;

  // Evento para interceptar/auditar comandos antes de ejecutar
  TAiShellCommandEvent = procedure(Sender: TObject; const Command: string; const CallId: string; var Result: TShellExecutionResult; var Handled: Boolean) of object;
  TAiShellLogEvent = procedure(Sender: TObject; const Command: string; const StdOut, StdErr: string; ExitCode: Integer) of object;

  // Modos soportados (para generar la definición del Tool correcta)

  TAiShell = class(TComponent)
  private
    FSession: TInteractiveProcessInfo;
    FTimeOut: Cardinal;
    FShellPath: string;
    FOnCommand: TAiShellCommandEvent;
    FActive: Boolean;
    FEnvironment: TStringList;
    FMaxOutputSize: Integer;
    FOnConsoleLog: TAiShellLogEvent;

    procedure SetActive(const Value: Boolean);
    function GenerateSentinel: string;
    function CleanOutput(const RawOutput, Sentinel: string): string;
    procedure StartSession;
    procedure StopSession;

    // Ejecución de bajo nivel (Atómica)
    function InternalExecuteCommand(const ACommand: string; TimeOutMs: Cardinal): TShellExecutionResult;

    // Métodos específicos por proveedor
    function ExecuteClaudeAction(const CallId: string; JArgs: TJSONObject): string;
    function ExecuteOpenAIAction(const CallId: string; JArgs: TJSONObject): string;
    function ExecuteGenericAction(const CallId: string; JArgs: TJSONObject): string;
    procedure SetShellPath(const Value: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // --- PUNTO DE ENTRADA ÚNICO ---
    // Detecta automáticamente el formato del JSON y delega.
    function Execute(const CallId: string; const JsonArguments: string): string; overload;
    function Execute(const CallId: string; JArgs: TJSONObject): string; overload;
    function ExecuteManual(const Command: string): string;

    procedure Restart;
    // function GetToolDefinition: string;

  published
    property Active: Boolean read FActive write SetActive default False;
    property TimeOut: Cardinal read FTimeOut write FTimeOut default 30000;
    property ShellPath: string read FShellPath write SetShellPath;
    property MaxOutputSize: Integer read FMaxOutputSize write FMaxOutputSize default 20000;

    property OnCommand: TAiShellCommandEvent read FOnCommand write FOnCommand;
    property OnConsoleLog: TAiShellLogEvent read FOnConsoleLog write FOnConsoleLog;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiShell]);
end;

procedure LogDebug(const Mensaje: string);
var
  Archivo: TextFile;
  RutaLog: string;
begin
  RutaLog := 'c:\temp\ialog.txt';

  try
    AssignFile(Archivo, RutaLog);

    // Si el archivo existe, lo abre para agregar; si no, lo crea
    if FileExists(RutaLog) then
      Append(Archivo)
    else
      Rewrite(Archivo);

    // Escribe la línea con fecha/hora
    // WriteLn(Archivo, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Mensaje);
    WriteLn(Archivo, Mensaje);

  finally
    CloseFile(Archivo);
  end;
end;

{ TAiShell }

constructor TAiShell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeOut := 30000;
  FMaxOutputSize := 20000;
  FActive := False;
  FEnvironment := TStringList.Create;

{$IFDEF MSWINDOWS}
  FShellPath := 'cmd.exe';
{$ENDIF}
{$IFDEF POSIX}
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
  // Si el valor es el mismo, no hacemos nada
  if SameText(FShellPath, Value) then
    Exit;

  // Guardamos si estaba corriendo para restaurar el estado
  WasRunning := Assigned(FSession);

  // 1. Matar la sesión actual (porque tiene el ejecutable incorrecto)
  StopSession;

  // 2. Actualizar la ruta
  FShellPath := Value;

  // 3. Si estaba corriendo (o Active estaba True), reiniciamos con el nuevo ejecutable
  if WasRunning or FActive then
  begin
    StartSession;
  end;
end;

procedure TAiShell.StartSession;
begin
  if Assigned(FSession) then
    Exit;
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
function TAiShell.InternalExecuteCommand(const ACommand: string; TimeOutMs: Cardinal): TShellExecutionResult;
var
  FullCommand, Sentinel: string;
  Buffer: TBytes;
  BytesRead: Integer;
  OutputBuilder, ErrorBuilder: TStringBuilder;
  StopWatch: TStopwatch;
  RawStr: string;

  InputBytes: TBytes;
  RawUtf8: UTF8String;

  IsLinuxStyle: Boolean;
begin
  Result.ExitCode := 0;
  Result.TimedOut := False;
  Result.StdOut := '';
  Result.StdErr := '';

  if not Assigned(FSession) or not FSession.IsRunning then
    StartSession;

  Sentinel := GenerateSentinel;
  OutputBuilder := TStringBuilder.Create;
  ErrorBuilder := TStringBuilder.Create;
  SetLength(Buffer, 4096);

  try
    // -------------------------------------------------------------------------
    // 1. DETERMINAR SINTAXIS SEGÚN EL SHELL (NO SEGÚN EL SO)
    // -------------------------------------------------------------------------
    // Si el ejecutable contiene 'wsl', 'bash', 'sh' o estamos en POSIX, usamos sintaxis Linux.
    // Si es 'cmd.exe' o 'powershell', usamos sintaxis Windows.
    IsLinuxStyle := False;
{$IFDEF POSIX}
    IsLinuxStyle := True;
{$ENDIF}
    if ContainsText(ExtractFileName(FShellPath), 'wsl') or ContainsText(ExtractFileName(FShellPath), 'bash') or ContainsText(ExtractFileName(FShellPath), 'sh') then
    begin
      IsLinuxStyle := True;
    end;

    if IsLinuxStyle then
    begin
      // Sintaxis Linux: ; para secuencial (o &&), echo $? para exit code
      // Importante: En WSL, el comando debe terminar con salto de linea Linux si es posible,
      // pero el StreamWriter de Delphi se encarga.
      FullCommand := ACommand + '; echo "EC:$? ' + Sentinel + '"' + sLineBreak;
    end
    else
    begin
      // Sintaxis Windows CMD: & para secuencial
      FullCommand := ACommand + ' & echo ' + Sentinel + sLineBreak;
    end;

    // -------------------------------------------------------------------------
    // 2. CORRECCIÓN DE CODIFICACIÓN (INPUT)
    // -------------------------------------------------------------------------
    RawUtf8 := UTF8String(FullCommand);
    SetLength(InputBytes, Length(RawUtf8));
    if Length(RawUtf8) > 0 then
      Move(RawUtf8[1], InputBytes[0], Length(RawUtf8));

    FSession.WriteInput(InputBytes[0], Length(InputBytes));

    // -------------------------------------------------------------------------
    // 3. BUCLE DE LECTURA
    // -------------------------------------------------------------------------
    StopWatch := TStopwatch.StartNew;

    while StopWatch.ElapsedMilliseconds < TimeOutMs do
    begin
      // Leer STDOUT
      BytesRead := FSession.ReadOutput(Buffer[0], Length(Buffer));
      if BytesRead > 0 then
      begin
        // IMPORTANTE: WSL y las herramientas modernas usan UTF-8.
        // 'Default' (ANSI) romperá acentos y bordes. Usamos UTF8.
        // RawStr := TEncoding.UTF8.GetString(Buffer, 0, BytesRead);
        RawStr := TEncoding.Default.GetString(Buffer, 0, BytesRead);

        OutputBuilder.Append(RawStr);

        if Pos(Sentinel, OutputBuilder.ToString) > 0 then
          Break;

        StopWatch.Reset;
        StopWatch.Start;
      end;

      // Leer STDERR
      BytesRead := FSession.ReadError(Buffer[0], Length(Buffer));
      if BytesRead > 0 then
      begin
        RawStr := TEncoding.UTF8.GetString(Buffer, 0, BytesRead);
        ErrorBuilder.Append(RawStr);
      end;

      Sleep(20);
    end;

    if StopWatch.ElapsedMilliseconds >= TimeOutMs then
    begin
      Result.TimedOut := True;
      // Restart; // Descomentar si se desea matar sesión colgada
    end;

    // -------------------------------------------------------------------------
    // 4. PROCESAMIENTO DE SALIDA
    // -------------------------------------------------------------------------
    Result.StdOut := CleanOutput(OutputBuilder.ToString, Sentinel);
    Result.StdErr := ErrorBuilder.ToString;

    // Intentar capturar Exit Code (Funciona en WSL y Linux nativo)
    if IsLinuxStyle then
    begin
      var
      OutStr := OutputBuilder.ToString;
      var
      P := Pos('EC:', OutStr);
      if P > 0 then
      begin
        var
        CodeStr := Copy(OutStr, P + 3, 5);
        var
        SpacePos := Pos(' ', CodeStr);
        if SpacePos > 0 then
        begin
          CodeStr := Copy(CodeStr, 1, SpacePos - 1);
          Result.ExitCode := StrToIntDef(CodeStr, 0);
          // Limpiamos el artifacto del ExitCode de la salida visual
          Result.StdOut := StringReplace(Result.StdOut, 'EC:' + CodeStr, '', []);
        end;
      end;
    end;

    if Result.StdOut.Length > FMaxOutputSize then
      Result.StdOut := Result.StdOut.Substring(0, FMaxOutputSize) + '... [Truncated]';

    if Assigned(FOnConsoleLog) then
      FOnConsoleLog(Self, ACommand, Result.StdOut, Result.StdErr, Result.ExitCode);

  finally
    OutputBuilder.Free;
    ErrorBuilder.Free;
  end;
end;

// =============================================================================
// DISPATCHER PRINCIPAL (Smart Detection)
// =============================================================================

function TAiShell.Execute(const CallId: string; const JsonArguments: string): string;
var
  JArgs: TJSONObject;
begin
  JArgs := TJSONObject.ParseJSONValue(JsonArguments) as TJSONObject;
  try
    if not Assigned(JArgs) then
      Exit('Error: Invalid JSON arguments.');

    Result := Execute(CallId, JArgs);
  finally
    JArgs.Free;
  end;
end;

function TAiShell.Execute(const CallId: string; JArgs: TJSONObject): string;
begin
  // Detección de formato basada en la presencia de campos clave

  // CASO 1: OPENAI (Tiene array 'commands')
  if JArgs.GetValue('commands') is TJSonArray then
  begin
    Result := ExecuteOpenAIAction(CallId, JArgs);
  end
  // CASO 2: CLAUDE (Tiene string 'command' y opcional 'restart')
  else if (JArgs.GetValue('command') <> nil) or (JArgs.GetValue('restart') <> nil) then
  begin
    Result := ExecuteClaudeAction(CallId, JArgs);
  end
  // CASO 3: GENERICO (Fallback a un solo comando 'command' o 'cmd')
  else
  begin
    Result := ExecuteGenericAction(CallId, JArgs);
  end;
end;

// =============================================================================
// IMPLEMENTACIONES ESPECÍFICAS
// =============================================================================

// --- CLAUDE ---
function TAiShell.ExecuteClaudeAction(const CallId: string; JArgs: TJSONObject): string;
var
  Cmd: string;
  ShouldRestart: Boolean;
  ExecRes: TShellExecutionResult;
  Handled: Boolean;
begin
  if JArgs.TryGetValue<Boolean>('restart', ShouldRestart) and ShouldRestart then
  begin
    Restart;
    Exit('Shell session restarted.');
  end;

  if not JArgs.TryGetValue<string>('command', Cmd) then
    Exit('Error: No command provided.');

  Handled := False;
  if Assigned(FOnCommand) then
    FOnCommand(Self, Cmd, CallId, ExecRes, Handled);

  if not Handled then
    ExecRes := InternalExecuteCommand(Cmd, FTimeOut);

  // Formato de salida Claude: Texto plano simple
  if ExecRes.TimedOut then
    Result := 'Error: Command timed out.' + sLineBreak + ExecRes.StdOut
  else if ExecRes.StdErr <> '' then
    Result := ExecRes.StdOut + sLineBreak + 'STDERR: ' + ExecRes.StdErr
  else
  begin
    if ExecRes.StdOut = '' then
      Result := '[Command executed successfully]'
    else
      Result := ExecRes.StdOut;
  end;
end;

// --- OPENAI (GPT-5.1 Shell Tool) ---
function TAiShell.ExecuteOpenAIAction(const CallId: string; JArgs: TJSONObject): string;
var
  CommandsArray: TJSonArray;
  OutputObj, ItemOut, OutcomeObj: TJSONObject;
  OutputArray: TJSonArray;
  CmdVal: TJSONValue;
  Cmd: string;
  ExecRes: TShellExecutionResult;
  LocalTimeOut: Cardinal;
  ReqMaxOutput: Integer;
  Handled: Boolean;
  jVal: TJSONValue;
begin
  // Construir el objeto de salida complejo
  OutputObj := TJSONObject.Create;

  // LogDebug('--- Shell Params ---');
  // LogDebug(JArgs.format);

  try
    OutputObj.AddPair('type', 'shell_call_output');
    OutputObj.AddPair('call_id', CallId);

    // 3. Verificar que existe y NO es TJSONNull

    LocalTimeOut := Self.FTimeOut;

    If JArgs.TryGetValue('timeout_ms', jVal) then
    Begin
      if Assigned(jVal) and not(jVal is TJSONNull) then
      begin
        // Intentar convertir solo si es un valor válido
        if not jVal.TryGetValue<Cardinal>(LocalTimeOut) then
          LocalTimeOut := FTimeOut; // Fallback si la conversión falla
      end;
    End;

    if JArgs.TryGetValue<Integer>('max_output_length', ReqMaxOutput) then
      OutputObj.AddPair('max_output_length', TJSONNumber.Create(ReqMaxOutput));

    OutputArray := TJSonArray.Create;
    OutputObj.AddPair('output', OutputArray);

    if JArgs.TryGetValue<TJSonArray>('commands', CommandsArray) then
    begin
      for CmdVal in CommandsArray do
      begin
        Cmd := CmdVal.Value;
        ItemOut := TJSONObject.Create;
        Handled := False;

        if Assigned(FOnCommand) then
          FOnCommand(Self, Cmd, CallId, ExecRes, Handled);

        if not Handled then
          ExecRes := InternalExecuteCommand(Cmd, LocalTimeOut);

        // Estructura específica OpenAI
        ItemOut.AddPair('stdout', ExecRes.StdOut);
        ItemOut.AddPair('stderr', ExecRes.StdErr);

        OutcomeObj := TJSONObject.Create;
        if ExecRes.TimedOut then
          OutcomeObj.AddPair('type', 'timeout')
        else
        begin
          OutcomeObj.AddPair('type', 'exit');
          OutcomeObj.AddPair('exit_code', TJSONNumber.Create(ExecRes.ExitCode));
        end;
        ItemOut.AddPair('outcome', OutcomeObj);

        OutputArray.Add(ItemOut);
      end;
    end;

    // Retornamos JSON Stringificado (OpenAI Chat Component lo parseará si necesita, o enviará raw)
    Result := OutputObj.ToJSON;
  finally
    OutputObj.Free;
  end;
end;

// --- GENERIC / FALLBACK ---
function TAiShell.ExecuteGenericAction(const CallId: string; JArgs: TJSONObject): string;
var
  Cmd: string;
  ExecRes: TShellExecutionResult;
  Handled: Boolean;
begin
  // Intenta encontrar 'cmd' o 'command'
  if not JArgs.TryGetValue<string>('command', Cmd) then
    if not JArgs.TryGetValue<string>('cmd', Cmd) then
      Exit('Error: Unknown command format.');

  Handled := False;
  if Assigned(FOnCommand) then
    FOnCommand(Self, Cmd, CallId, ExecRes, Handled);

  if not Handled then
    ExecRes := InternalExecuteCommand(Cmd, FTimeOut);

  // Retorna texto plano por defecto
  if ExecRes.StdErr <> '' then
    Result := 'STDOUT: ' + ExecRes.StdOut + sLineBreak + 'STDERR: ' + ExecRes.StdErr
  else
    Result := ExecRes.StdOut;
end;

function TAiShell.ExecuteManual(const Command: string): string;
var
  ExecRes: TShellExecutionResult;
begin
  // 1. Asegurar que la sesión esté viva
  if not Active then
    Active := True;

  // 2. Ejecutar directamente (saltando la intercepción de seguridad OnCommand)
  // Usamos el mismo TimeOut configurado en el componente
  ExecRes := InternalExecuteCommand(Command, FTimeOut);

  // 3. Formatear el resultado para devolverlo como string
  // (Aunque la UI se actualizará sola vía OnConsoleLog)
  if ExecRes.TimedOut then
    Result := 'Error: Command timed out.' + sLineBreak + ExecRes.StdOut
  else if ExecRes.StdErr <> '' then
    Result := ExecRes.StdOut + sLineBreak + 'STDERR: ' + ExecRes.StdErr
  else
    Result := ExecRes.StdOut;
end;

//
// function TAiShell.GetToolDefinition: string;
// begin
// case FToolMode of
// tmClaudeNative:
// Result := Format('{"type": "%s", "name": "bash"}', [FClaudeToolVersion]);
// tmOpenAINative:
// Result := '{"type": "shell"}';
// tmStandard:
// Result := '{ "type": "function", "function": { "name": "bash", "description": "Execute shell commands", "parameters": { "type": "object", "properties": { "command": { "type": "string" } }, "required": ["command"] } } }';
// end;
// end;

end.
