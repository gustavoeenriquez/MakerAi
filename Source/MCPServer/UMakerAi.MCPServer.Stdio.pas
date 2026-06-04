// MIT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit UMakerAi.MCPServer.Stdio;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Threading, System.AnsiStrings, System.IOUtils,
  UMakerAi.MCPServer.Core;

type
  // Declaraci?n adelantada para el hilo
  TAiMCPStdioServer = class;

  { TStdioWorkerThread
    Este hilo se encarga de la tarea bloqueante de leer desde Standard Input }
  TStdioWorkerThread = class(TThread)
  private
    FServer: TAiMCPStdioServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TAiMCPStdioServer);
  end;

  { TAiMCPStdioServer
    El componente principal que gestiona la comunicaci?n Stdio }
  TAiMCPStdioServer = class(TAiMCPServer)
  private
    FWorkerThread: TStdioWorkerThread;
    FOutputLock: TCriticalSection; // Para escrituras seguras a Stdout desde m?ltiples hilos

    procedure ProcessRequest(const ARequestJson: string);
    procedure SendResponse(const AResponseJson: string);
    procedure SetConsoleIOToUTF8;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; Override;
    procedure Stop; Override;
  end;

procedure Register;

implementation

uses System.Character {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF};

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMCPStdioServer]);
end;

{ TStdioWorkerThread }

constructor TStdioWorkerThread.Create(AServer: TAiMCPStdioServer);
begin
  inherited Create(True); // El hilo se crea suspendido
  FServer := AServer;
  FreeOnTerminate := False;
end;

{
procedure TStdioWorkerThread.Execute;
var
  JsonRequestLine: string;
begin
  while not Terminated do
  begin
    try
      // Leemos una l?nea completa desde Standard Input.
      // Esta llamada es bloqueante y esperar? hasta recibir un LF (#10).
      System.ReadLn(JsonRequestLine);

      // Si el hilo fue terminado mientras esperaba o la l?nea est? vac?a, continuamos.
      if Terminated or (JsonRequestLine = '') then
        Continue;

      // Cada l?nea es un request JSON completo. Lo procesamos.
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FServer) and FServer.IsActive then
            FServer.ProcessRequest(JsonRequestLine);
        end);
    except
      on E: EInOutError do
      begin
        // Esto ocurre si el pipe de Stdin se cierra. Es la forma normal de terminar.
        if not Terminated then
          Break;
      end;
      on E: Exception do
      begin
        // Otro tipo de error, terminamos el bucle.
        if not Terminated then
          Break;
      end;
    end;
  end;
end;

}

procedure TStdioWorkerThread.Execute;
var
  JsonRequestLine: string;
{$IFDEF MSWINDOWS}
  StdinHandle: THandle;
  RawLine: TBytes;
  B: Byte;
  BytesRead: DWORD;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // Read raw bytes from stdin pipe and decode as UTF-8.
  // System.ReadLn converts using the ANSI code page (CP1252 on most Windows
  // systems), which corrupts multi-byte UTF-8 sequences. ReadFile + UTF-8
  // decoding is the correct approach when stdin is a pipe.
  StdinHandle := GetStdHandle(STD_INPUT_HANDLE);
  SetLength(RawLine, 0);
{$ENDIF}
  while not Terminated do
  begin
    try
{$IFDEF MSWINDOWS}
      // Accumulate bytes until LF, then decode the complete line as UTF-8.
      repeat
        if not ReadFile(StdinHandle, B, 1, BytesRead, nil) or (BytesRead = 0) then
        begin
          // Pipe closed (EOF)
          if Length(RawLine) > 0 then
            Break; // process the last partial line
          Exit;
        end;
        if B = 10 then Break; // LF = end of line
        if B <> 13 then // skip CR
        begin
          SetLength(RawLine, Length(RawLine) + 1);
          RawLine[High(RawLine)] := B;
        end;
      until False;
      JsonRequestLine := TEncoding.UTF8.GetString(RawLine);
      SetLength(RawLine, 0);
{$ELSE}
      System.ReadLn(JsonRequestLine);
{$ENDIF}

      if Terminated or (JsonRequestLine = '') then
        Continue;

      if Assigned(FServer) and FServer.IsActive then
      begin
        try
          FServer.ProcessRequest(JsonRequestLine);
        except
          // Capturamos excepciones para que un error de logica no mate al hilo de lectura
        end;
      end;
    except
      on E: EInOutError do
      begin
        if not Terminated then Break;
      end;
      on E: Exception do
      begin
        if not Terminated then Break;
      end;
    end;
  end;
end;


{ TAiMCPStdioServer }

constructor TAiMCPStdioServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOutputLock := TCriticalSection.Create; // Para proteger Stdout
end;

destructor TAiMCPStdioServer.Destroy;
begin
  FOutputLock.Free;
  inherited Destroy;
end;

procedure TAiMCPStdioServer.Start;
begin

  if IsActive then
    Exit;

  SetConsoleIOToUTF8;

  Inherited Start;
  FWorkerThread := TStdioWorkerThread.Create(Self);
  FWorkerThread.Start;
end;

procedure TAiMCPStdioServer.Stop;
begin
  if not IsActive then
    Exit;

  if Assigned(FWorkerThread) then
  begin
    FWorkerThread.Terminate;
    // No llamamos WaitFor: ReadLn(Input) es bloqueante e ininterrumpible.
    // El thread saldr� naturalmente al detectar EOF en stdin.
    FWorkerThread := nil;
  end;

  inherited Stop;
end;

procedure TAiMCPStdioServer.ProcessRequest(const ARequestJson: string);
var
  ResponseBody: string;
begin
  if not IsActive then
    Exit;

  // Delegamos el trabajo pesado al servidor l?gico
  ResponseBody := FLogicServer.ExecuteRequest(ARequestJson, ''); // La sesi?n no aplica en Stdio

  // Si hay una respuesta que enviar (no es una notificaci?n)
  if ResponseBody <> '' then
  begin
    SendResponse(ResponseBody);
  end;
end;

procedure TAiMCPStdioServer.SendResponse(const AResponseJson: string);
{$IFDEF MSWINDOWS}
var
  StdoutHandle: THandle;
  Bytes: TBytes;
  Written: DWORD;
{$ENDIF}
begin
  // Write directly to the Windows stdout handle so the response reaches
  // the MCP client even when the process is started without a console window
  // (e.g., spawned headless by Claude Code). System.WriteLn + Flush(Output)
  // does not flush the pipe in that scenario.
  FOutputLock.Enter;
  try
{$IFDEF MSWINDOWS}
    StdoutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
    Bytes := TEncoding.UTF8.GetBytes(AResponseJson + #10);
    WriteFile(StdoutHandle, Bytes[0], Length(Bytes), Written, nil);
{$ELSE}
    System.WriteLn(AResponseJson);
    Flush(Output);
{$ENDIF}
  finally
    FOutputLock.Leave;
  end;
end;

procedure TAiMCPStdioServer.SetConsoleIOToUTF8;
begin
{$IFDEF MSWINDOWS}
  // Forzar UTF-8 (Codepage 65001) para que los JSON no se rompan
  SetConsoleOutputCP(CP_UTF8);
  SetConsoleCP(CP_UTF8);
{$ENDIF}
end;

end.
