unit UMakerAi.MCPServer.Stdio;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Threading, System.AnsiStrings, system.IOUtils,
  uMakerAi.MCPServer.Core;

type
  // Declaraci�n adelantada para el hilo
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
    El componente principal que gestiona la comunicaci�n Stdio }
  TAiMCPStdioServer = class(TAiMCPServer)
  private
    FWorkerThread: TStdioWorkerThread;
    FOutputLock: TCriticalSection; // Para escrituras seguras a Stdout desde m�ltiples hilos

    procedure ProcessRequest(const ARequestJson: string);
    procedure SendResponse(const AResponseJson: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; Override;
    procedure Stop; Override;
  end;

procedure Register;

implementation

uses System.Character;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMCPStdioServer]);
end;

{ TStdioWorkerThread }

constructor TStdioWorkerThread.Create(AServer: TAiMCPStdioServer);
begin
  inherited Create(False); // El hilo se crea suspendido
  FServer := AServer;
  FreeOnTerminate := True; // El hilo se liberar� autom�ticamente al terminar
end;

procedure TStdioWorkerThread.Execute;
var
  JsonRequestLine: string;
begin
  while not Terminated do
  begin
    try
      // Leemos una l�nea completa desde Standard Input.
      // Esta llamada es bloqueante y esperar� hasta recibir un LF (#10).
      System.ReadLn(JsonRequestLine);

      // Si el hilo fue terminado mientras esperaba o la l�nea est� vac�a, continuamos.
      if Terminated or (JsonRequestLine = '') then
        Continue;

      // Cada l�nea es un request JSON completo. Lo procesamos.
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
  Inherited Start;
  FWorkerThread := TStdioWorkerThread.Create(Self);
  FWorkerThread.Start;
end;

procedure TAiMCPStdioServer.Stop;
begin
  if not IsActive then Exit;

  if Assigned(FWorkerThread) then
    FWorkerThread.Terminate;

  inherited Stop;
end;

procedure TAiMCPStdioServer.ProcessRequest(const ARequestJson: string);
var
  ResponseBody: string;
begin
  if not IsActive then Exit;

  // Delegamos el trabajo pesado al servidor l�gico
  ResponseBody := FLogicServer.ExecuteRequest(ARequestJson, ''); // La sesi�n no aplica en Stdio

  // Si hay una respuesta que enviar (no es una notificaci�n)
  if ResponseBody <> '' then
  begin
    SendResponse(ResponseBody);
  end;
end;

procedure TAiMCPStdioServer.SendResponse(const AResponseJson: string);
begin
  // El cliente espera el JSON seguido de un salto de l�nea (#10).
  // WriteLn hace esto autom�ticamente.

  FOutputLock.Enter;
  try
    // System.WriteLn es la forma m�s simple y correcta aqu�.
    // Env�a el string y el terminador de l�nea apropiado.
    System.WriteLn(AResponseJson);

    // Usar TOutput.Flush para asegurar que se env�e inmediatamente.
    Flush(Output);
  finally
    FOutputLock.Leave;
  end;
end;



end.
