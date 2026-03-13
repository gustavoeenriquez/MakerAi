// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Transporte StdIO para el servidor MCP.
// Lee peticiones JSON-RPC de stdin, linea por linea, y escribe respuestas
// en stdout. Ideal para integracion con Claude Desktop y otros hosts MCP.
//
// Adaptaciones respecto a la version Delphi:
//   - TThread anonimo → TStdioWorkerThread con metodo Execute
//   - SetConsoleOutputCP(CP_UTF8) en Windows → {$IFDEF MSWINDOWS}
//   - TThread.Queue(nil, proc) → llamada directa (aplicacion de consola)

unit uMakerAi.MCPServer.Stdio;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs,
  uMakerAi.MCPServer.Core;

{$IFDEF MSWINDOWS}
// Declaramos solo las funciones necesarias de kernel32 para evitar
// importar la unit Windows completa (que define TCriticalSection como
// record y colisiona con SyncObjs.TCriticalSection clase).
function SetConsoleOutputCP(wCodePageID: LongWord): LongBool; stdcall;
    external 'kernel32.dll' name 'SetConsoleOutputCP';
function SetConsoleCP(wCodePageID: LongWord): LongBool; stdcall;
    external 'kernel32.dll' name 'SetConsoleCP';
{$ENDIF}

type
  // Forward
  TAiMCPStdioServer = class;

  // -------------------------------------------------------------------------
  // TStdioWorkerThread - lee stdin linea a linea y despacha al servidor
  // -------------------------------------------------------------------------
  TStdioWorkerThread = class(TThread)
  private
    FServer: TAiMCPStdioServer;
  public
    constructor Create(AServer: TAiMCPStdioServer);
    procedure Execute; override;
  end;

  // -------------------------------------------------------------------------
  // TAiMCPStdioServer - servidor MCP sobre stdin/stdout
  // -------------------------------------------------------------------------
  TAiMCPStdioServer = class(TAiMCPServer)
  private
    FWorkerThread : TStdioWorkerThread;
    FOutputLock   : TCriticalSection;

    procedure SetConsoleIOToUTF8;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override;
    procedure Stop;  override;

    // Procesa una linea JSON recibida de stdin
    procedure ProcessRequest(const ALine: string);

    // Envia una respuesta JSON a stdout (thread-safe)
    procedure SendResponse(const AJsonStr: string);

  published
    property ApiKey;
    property ServerName;
    property OnValidateRequest;
  end;

implementation

// ---------------------------------------------------------------------------
// TStdioWorkerThread
// ---------------------------------------------------------------------------

constructor TStdioWorkerThread.Create(AServer: TAiMCPStdioServer);
begin
  inherited Create(True); // suspendido
  FServer := AServer;
  FreeOnTerminate := False;
end;

procedure TStdioWorkerThread.Execute;
var
  Line: string;
begin
  // Leer stdin linea a linea de forma bloqueante
  while not Terminated do
  begin
    try
      ReadLn(Input, Line);
      if Terminated then
        Break;
      Line := Trim(Line);
      if Line <> '' then
        FServer.ProcessRequest(Line);
    except
      on E: Exception do
      begin
        // EOF o error de lectura — terminar el hilo
        Break;
      end;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// TAiMCPStdioServer
// ---------------------------------------------------------------------------

constructor TAiMCPStdioServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWorkerThread := nil;
  FOutputLock   := TCriticalSection.Create;
end;

destructor TAiMCPStdioServer.Destroy;
begin
  Stop;
  FOutputLock.Free;
  inherited;
end;

procedure TAiMCPStdioServer.SetConsoleIOToUTF8;
begin
{$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetConsoleCP(CP_UTF8);
{$ENDIF}
  // En Linux/macOS, UTF-8 es el encoding nativo de consola
end;

procedure TAiMCPStdioServer.Start;
begin
  inherited Start; // LogicServer.Start, FActive := True

  SetConsoleIOToUTF8;

  FWorkerThread := TStdioWorkerThread.Create(Self);
  FWorkerThread.Start;
end;

procedure TAiMCPStdioServer.Stop;
begin
  if Assigned(FWorkerThread) then
  begin
    FWorkerThread.Terminate;
    // No podemos desbloquear ReadLn facilmente — simplemente esperamos con timeout
    // En produccion, el proceso termina junto con el host MCP
    FWorkerThread.WaitFor;
    FreeAndNil(FWorkerThread);
  end;
  inherited Stop;
end;

procedure TAiMCPStdioServer.ProcessRequest(const ALine: string);
var
  AuthCtx: TAiAuthContext;
  Response: string;
begin
  // Para StdIO, la autenticacion no aplica (proceso local de confianza)
  AuthCtx.IsAuthenticated := True;
  AuthCtx.UserID          := 'stdio';
  AuthCtx.UserName        := 'stdio';
  AuthCtx.Roles           := 'admin';

  Response := FLogicServer.ExecuteRequest(ALine, 'stdio', AuthCtx);

  if Response <> '' then
    SendResponse(Response);
end;

procedure TAiMCPStdioServer.SendResponse(const AJsonStr: string);
begin
  FOutputLock.Enter;
  try
    WriteLn(AJsonStr);
    Flush(Output);
  finally
    FOutputLock.Leave;
  end;
end;

end.
