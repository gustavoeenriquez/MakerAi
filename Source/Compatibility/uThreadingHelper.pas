// =============================================================================
// MakerAI Cross-Platform Compatibility Layer - Threading Helper
// =============================================================================
//
// Purpose: Unifica threading/tasks entre Delphi (System.Threading) y FPC (manual)
//
// Compatibility:
//   - Delphi XE7+ (Primary development environment, usa System.Threading nativo)
//   - Free Pascal 3.3+ (Full support con anonymous methods "reference to procedure")
//   - Free Pascal 3.2+ (Limited: anonymous methods como procedures normales)
//
// Usage:
//   uses uThreadingHelper;
//   var
//     Task: ITask;
//   begin
//     Task := TTask.Run(procedure
//       begin
//         // Background work
//         Sleep(1000);
//       end);
//     Task.Wait;
//   end;
//
// Developer Notes (Delphi):
//   - TTask, ITask, TThreadPool son aliases directos a System.Threading en Delphi
//   - Esta unidad NO afecta el código Delphi existente
//   - En FPC implementa TTask/ITask manualmente usando TThread
//   - TThreadProc soporta "reference to procedure" (FPC 3.3+) o procedures (FPC 3.2)
//
// =============================================================================

unit uThreadingHelper;

{$include ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, SyncObjs, SysUtils, Generics.Collections;
  {$ELSE}
  System.Classes, System.SyncObjs, System.SysUtils, System.Threading;
  {$ENDIF}

type
  // Compatible Delphi y FPC 3.3+ (reference to)
  TThreadProc = reference to procedure;

  // TProc: Alias compatible con System.SysUtils.TProc de Delphi
  // En FPC no existe TProc, lo definimos como sinónimo de TThreadProc
  {$IFDEF FPC}
  TProc = TThreadProc;
  {$ENDIF}

  /// <summary>
  /// Clase base para threads de tareas con manejo automático de excepciones.
  /// Los threads se crean en modo suspended. Llamar Start() para ejecutar.
  /// </summary>
  TTaskThread = class(TThread)
  private
    FException: Exception;
    FCompleted: Boolean;
    FCompletedEvent: TEvent;
    FStarted: Boolean;
  protected
    procedure Execute; override;
    procedure DoExecute; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; 
    procedure WaitForCompletion;
    function GetException: Exception;
    property Completed: Boolean read FCompleted;
    property Started: Boolean read FStarted;
  end;

  /// <summary>
  /// Gestor de pool de threads que limita concurrencia mediante semáforos
  /// </summary>
  TThreadPoolManager = class
  private
    FMaxThreads: Integer;
    FCount: Integer;
    FSlotAvailable: TEvent;
    FLock: TCriticalSection;
  public
    constructor Create(AMaxThreads: Integer = 4);
    destructor Destroy; override;
    procedure AcquireSlot;
    procedure ReleaseSlot;
    /// <summary>
    /// Ajusta el número máximo de threads.
    /// ADVERTENCIA: No llamar desde un thread del pool, causaría deadlock.
    /// </summary>
    procedure SetMaxThreads(AValue: Integer);
    /// <summary>
    /// Alias para SetMaxThreads - compatible con Delphi TThreadPool.SetMaxWorkerThreads
    /// </summary>
    procedure SetMaxWorkerThreads(AValue: Integer);
    function GetMaxThreads: Integer;
  end;

  {$IFDEF FPC}
  // Helper para extender TThread con Queue anónimo en FPC
  TThreadHelper = class helper for TThread
  public
    class procedure Queue(AThread: TThread; AProc: TThreadProc); overload;
    class procedure Synchronize(AThread: TThread; AProc: TThreadProc); overload;
    class function WaitForAll(const AThreads: array of TTaskThread; ATimeout: Cardinal = INFINITE): Boolean;
  end;
  {$ELSE}
  TThreadHelper = class
  public
    class function WaitForAll(const AThreads: array of TTaskThread; 
      ATimeout: Cardinal = INFINITE): Boolean;
  end;
  {$ENDIF}

  TAnonymousThread = class(TTaskThread)
  private
    FProc: TThreadProc;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AProc: TThreadProc);
  end;

  {$IFDEF FPC}
  // TTaskStatus: Enum compatible con System.Threading.TTaskStatus de Delphi
  // Nota: 'Exception' renombrado a 'Faulted' para evitar conflicto con clase Exception de FPC
  TTaskStatus = (Created, WaitingToRun, Running, Completed, WaitingForChildren, Canceled, Faulted);

  // Interface ITask para compatibilidad exacta con System.Threading de Delphi
  ITask = interface
    ['{A8B9C0D1-E2F3-4567-89AB-CDEF01234567}']
    procedure Start;
    procedure Wait; overload;
    function Wait(ATimeout: Cardinal): Boolean; overload;
    function GetCompleted: Boolean;
    function GetStarted: Boolean;
    function GetStatus: TTaskStatus;
    property Status: TTaskStatus read GetStatus;
  end;

  // Implementación de ITask que envuelve TTaskThread
  TTaskWrapper = class(TInterfacedObject, ITask)
  private
    FThread: TTaskThread;
    FStatus: TTaskStatus;
  public
    constructor Create(AThread: TTaskThread);
    destructor Destroy; override;
    procedure Start;
    procedure Wait; overload;
    function Wait(ATimeout: Cardinal): Boolean; overload;
    function GetCompleted: Boolean;
    function GetStarted: Boolean;
    function GetStatus: TTaskStatus;
  end;

  // TTask para FPC - retorna ITask (igual que Delphi)
  TTask = class
  public
    class function Create(const AProc: TThreadProc): ITask;
    class function Run(const AProc: TThreadProc): ITask; overload;
    // Sobrecarga con pool para paridad con Delphi System.Threading.TTask.Run
    class function Run(const AProc: TThreadProc; APool: TThreadPoolManager): ITask; overload;
    class function WaitForAll(const ATasks: array of ITask; ATimeout: Cardinal = INFINITE): Boolean;
  end;

  // TThreadPool: Alias para compatibilidad con código Delphi que usa TThreadPool
  TThreadPool = TThreadPoolManager;
  {$ELSE}
  // Delphi - usa System.Threading nativo
  TTask = System.Threading.TTask;
  ITask = System.Threading.ITask;
  TThreadPool = System.Threading.TThreadPool;
  TTaskStatus = System.Threading.TTaskStatus;
  {$ENDIF}

  // Helper unificado para excepción en tareas
  const
    TaskStatusFaulted = {$IFDEF FPC}TTaskStatus.Faulted{$ELSE}TTaskStatus.Exception{$ENDIF};

  // Helper para añadir sobrecarga de Boolean a TInterlocked en Delphi
  // (Algunas versiones de Delphi no la tienen o hay conflictos de scope)
  {$IFNDEF FPC}
  type
  TInterlockedHelper = class helper for System.SyncObjs.TInterlocked
  public
    class function ExchangeBool(var Target: Boolean; Value: Boolean): Boolean; static;
  end;
  {$ENDIF}

  // Función helper universal para Exchange de Boolean
  // Funciona en Delphi y FPC con la misma firma
  function AtomicExchangeBool(var Target: Boolean; Value: Boolean): Boolean;

  {$IFDEF FPC}
  type
  // TInterlocked: Operaciones atómicas compatibles con System.SyncObjs.TInterlocked
  TInterlocked = class
  public
    class function Increment(var Target: Integer): Integer; static; inline;
    class function Decrement(var Target: Integer): Integer; static; inline;
    class function Exchange(var Target: Integer; Value: Integer): Integer; overload; static;
    class function Exchange(var Target: Boolean; Value: Boolean): Boolean; overload; static;
    class function Exchange(var Target: Pointer; Value: Pointer): Pointer; overload; static;
    class function CompareExchange(var Target: Integer; Value, Comparand: Integer): Integer; static;
  end;
  {$ENDIF}

  {$IFDEF FPC}
  // Implementación compatible de TThreadedQueue<T> para FPC
  // Basada en TQueue<T> + TCriticalSection + TEvent
  TThreadedQueue<T> = class
  private
    FQueue: TQueue<T>;
    FLock: TCriticalSection;
    FEvent: TEvent; // Evento para notificar que hay items (Pop)
    FShutDown: Boolean;
    FQueueDepth: Integer;
    FPushTimeout: Cardinal;
    FPopTimeout: Cardinal;
  public
    constructor Create(ADepth: Integer = 10; ATimeout: Cardinal = INFINITE; APushTimeout: Cardinal = INFINITE);
    destructor Destroy; override;
    function PushItem(const AItem: T): TWaitResult;
    function PopItem(out AItem: T; ATimeout: Cardinal = INFINITE): TWaitResult;
    procedure DoShutDown;
  end;
  {$ENDIF}

  function CreateCriticalSection: TCriticalSection;
  
  // Wrapper para TThread.Queue compatible con ambos
  procedure QueueInMainThread(AProc: TThreadProc);
  // Wrapper para TThread.Synchronize compatible con ambos
  procedure SynchronizeInMainThread(AProc: TThreadProc);

implementation

function CreateCriticalSection: TCriticalSection;
begin
  Result := TCriticalSection.Create;
end;

procedure QueueInMainThread(AProc: TThreadProc);
begin
  {$IFDEF FPC}
  TThread.Queue(nil, AProc);
  {$ELSE}
  TThread.Queue(nil, TThreadProcedure(AProc));
  {$ENDIF}
end;

procedure SynchronizeInMainThread(AProc: TThreadProc);
begin
  {$IFDEF FPC}
  TThread.Synchronize(nil, AProc);
  {$ELSE}
  TThread.Synchronize(nil, TThreadProcedure(AProc));
  {$ENDIF}
end;

{ TTaskThread }

constructor TTaskThread.Create;
begin
  inherited Create(True); // Suspended
  FreeOnTerminate := False;
  FCompleted := False;
  FStarted := False;
  FException := nil;
  FCompletedEvent := TEvent.Create(nil, True, False, '');
end;

destructor TTaskThread.Destroy;
begin
  FCompletedEvent.Free;
  if Assigned(FException) then
    FException.Free;
  inherited;
end;

procedure TTaskThread.Execute;
begin
  try
    try
      DoExecute;
    except
      on E: Exception do
      begin
        // Preservar clase y mensaje de la excepción original
        FException := Exception.Create(E.ClassName + ': ' + string(E.Message));
      end;
    end;
  finally
    FCompleted := True;
    FCompletedEvent.SetEvent;
  end;
end;

procedure TTaskThread.Start;
begin
  FStarted := True;
  inherited Start;
end;

procedure TTaskThread.WaitForCompletion;
begin
  FCompletedEvent.WaitFor(INFINITE);
end;

function TTaskThread.GetException: Exception;
begin
  Result := FException;
end;

{ TThreadPoolManager }

constructor TThreadPoolManager.Create(AMaxThreads: Integer);
begin
  inherited Create;
  if AMaxThreads < 1 then
    AMaxThreads := 1;
  FMaxThreads := AMaxThreads;
  FCount := 0;
  FSlotAvailable := TEvent.Create(nil, False, True, ''); // Auto-reset, initially signaled (if max > 0)
  if AMaxThreads <= 0 then FSlotAvailable.ResetEvent;
  FLock := TCriticalSection.Create;
end;

destructor TThreadPoolManager.Destroy;
begin
  FSlotAvailable.Free;
  FLock.Free;
  inherited;
end;

procedure TThreadPoolManager.AcquireSlot;
begin
  FLock.Enter;
  try
    while FCount >= FMaxThreads do
    begin
      FLock.Leave;
      FSlotAvailable.WaitFor(INFINITE);
      FLock.Enter;
    end;
    Inc(FCount);
    if FCount >= FMaxThreads then
      FSlotAvailable.ResetEvent;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolManager.ReleaseSlot;
begin
  FLock.Enter;
  try
    if FCount > 0 then
      Dec(FCount);
    if FCount < FMaxThreads then
      FSlotAvailable.SetEvent;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolManager.SetMaxThreads(AValue: Integer);
begin
  FLock.Enter;
  try
    if AValue < 1 then
      FMaxThreads := 1
    else
      FMaxThreads := AValue;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadPoolManager.SetMaxWorkerThreads(AValue: Integer);
begin
  // Alias para SetMaxThreads - compatible con Delphi TThreadPool API
  SetMaxThreads(AValue);
end;

function TThreadPoolManager.GetMaxThreads: Integer;
begin
  FLock.Enter;
  try
    Result := FMaxThreads;
  finally
    FLock.Leave;
  end;
end;

{ TThreadHelper }

{$IFDEF FPC}
type
  TSyncProcWrapper = class
  private
    FProc: TThreadProc;
  public
    constructor Create(AProc: TThreadProc);
    procedure Execute;
  end;

constructor TSyncProcWrapper.Create(AProc: TThreadProc);
begin
  inherited Create;
  FProc := AProc;
end;

procedure TSyncProcWrapper.Execute;
begin
  try
    if Assigned(FProc) then
      FProc();
  finally
    Free; // Auto-free after execution
  end;
end;

class procedure TThreadHelper.Queue(AThread: TThread; AProc: TThreadProc);
var
  Wrapper: TSyncProcWrapper;
begin
  Wrapper := TSyncProcWrapper.Create(AProc);
  // En FPC {$mode delphi} pasamos el metodo directamente sin @
  TThread.Queue(AThread, Wrapper.Execute);
end;

class procedure TThreadHelper.Synchronize(AThread: TThread; AProc: TThreadProc);
var
  Wrapper: TSyncProcWrapper;
begin
  // Safe way:
  Wrapper := TSyncProcWrapper.Create(AProc);
  TThread.Synchronize(AThread, Wrapper.Execute);
end;

class function TThreadHelper.WaitForAll(const AThreads: array of TTaskThread; 
  ATimeout: Cardinal): Boolean;
var
  I: Integer;
  StartTime: QWord;
  Elapsed: QWord;
begin
  Result := True;
  StartTime := GetTickCount64;
  
  for I := Low(AThreads) to High(AThreads) do
  begin
    if not Assigned(AThreads[I]) then
      Continue;
    
    // Verificar que el thread fue iniciado antes de esperar
    if not AThreads[I].Started then
      Continue;
      
    if ATimeout <> INFINITE then
    begin
      Elapsed := GetTickCount64 - StartTime;
      if Elapsed >= ATimeout then
        Exit(False);
    end;
      
    AThreads[I].WaitForCompletion;
  end;
end;
{$ELSE}
class function TThreadHelper.WaitForAll(const AThreads: array of TTaskThread; 
  ATimeout: Cardinal): Boolean;
var
  I: Integer;
  StartTime: UInt64;
  Elapsed: UInt64;
begin
  Result := True;
  StartTime := TThread.GetTickCount;
  
  for I := Low(AThreads) to High(AThreads) do
  begin
    if not Assigned(AThreads[I]) then
      Continue;
    
    // Verificar que el thread fue iniciado antes de esperar
    if not AThreads[I].Started then
      Continue;
      
    if ATimeout <> INFINITE then
    begin
      Elapsed := TThread.GetTickCount - StartTime;
      
      if Elapsed >= ATimeout then
        Exit(False);
    end;
      
    AThreads[I].WaitForCompletion;
  end;
end;
{$ENDIF}

{ TAnonymousThread }

constructor TAnonymousThread.Create(AProc: TThreadProc);
begin
  inherited Create;
  FProc := AProc;
end;

procedure TAnonymousThread.DoExecute;
begin
  if Assigned(FProc) then
    FProc();
end;

{$IFDEF FPC}
{ TTaskWrapper - Implementación de ITask }

constructor TTaskWrapper.Create(AThread: TTaskThread);
begin
  inherited Create;
  FThread := AThread;
  FStatus := Created;
end;

destructor TTaskWrapper.Destroy;
begin
  if Assigned(FThread) then
  begin
    if FThread.Started and not FThread.Completed then
      FThread.WaitForCompletion;
    FThread.Free;
  end;
  inherited;
end;

procedure TTaskWrapper.Start;
begin
  if Assigned(FThread) then
  begin
    FStatus := Running;
    FThread.Start;
  end;
end;

procedure TTaskWrapper.Wait;
begin
  if Assigned(FThread) then
  begin
    FThread.WaitForCompletion;
    if FThread.GetException <> nil then
      FStatus := Faulted
    else
      FStatus := Completed;
  end;
end;

function TTaskWrapper.Wait(ATimeout: Cardinal): Boolean;
var
  StartTime: QWord;
begin
  Result := True;
  if not Assigned(FThread) then
    Exit;
    
  if ATimeout = INFINITE then
  begin
    // Llamada al procedimiento Wait sin timeout
    if Assigned(FThread) then
    begin
      FThread.WaitForCompletion;
      if FThread.GetException <> nil then
        FStatus := Faulted
      else
        FStatus := Completed;
    end;
    Exit(True);
  end;
  
  StartTime := GetTickCount64;
  while not FThread.Completed do
  begin
    if (GetTickCount64 - StartTime) >= ATimeout then
      Exit(False);
    Sleep(10);
  end;
  
  if FThread.GetException <> nil then
    FStatus := Faulted
  else
    FStatus := Completed;
end;

function TTaskWrapper.GetCompleted: Boolean;
begin
  Result := Assigned(FThread) and FThread.Completed;
end;

function TTaskWrapper.GetStarted: Boolean;
begin
  Result := Assigned(FThread) and FThread.Started;
end;

function TTaskWrapper.GetStatus: TTaskStatus;
begin
  if not Assigned(FThread) then
    Result := Created
  else if not FThread.Started then
    Result := Created
  else if FThread.Completed then
  begin
    if FThread.GetException <> nil then
      Result := Faulted
    else
      Result := Completed;
  end
  else
    Result := Running;
end;

{ TTask - FPC }

class function TTask.Create(const AProc: TThreadProc): ITask;
begin
  Result := TTaskWrapper.Create(TAnonymousThread.Create(AProc));
end;

class function TTask.Run(const AProc: TThreadProc): ITask;
begin
  Result := TTask.Create(AProc);
  Result.Start;
end;

class function TTask.Run(const AProc: TThreadProc; APool: TThreadPoolManager): ITask;
var
  LPooledProc: TThreadProc;
begin
  // Envolver el procedimiento para usar el pool de forma funcional
  // AcquireSlot bloquea si se alcanza el límite de concurrencia
  LPooledProc := procedure
  begin
    if Assigned(APool) then
      APool.AcquireSlot;
    try
      AProc();
    finally
      if Assigned(APool) then
        APool.ReleaseSlot;
    end;
  end;
  Result := TTask.Create(LPooledProc);
  Result.Start;
end;

class function TTask.WaitForAll(const ATasks: array of ITask; ATimeout: Cardinal): Boolean;
var
  I: Integer;
  StartTime: QWord;
  Elapsed: QWord;
begin
  Result := True;
  StartTime := GetTickCount64;
  
  for I := Low(ATasks) to High(ATasks) do
  begin
    if not Assigned(ATasks[I]) then
      Continue;
    
    if not ATasks[I].GetStarted then
      Continue;
      
    if ATimeout <> INFINITE then
    begin
      Elapsed := GetTickCount64 - StartTime;
      if Elapsed >= ATimeout then
        Exit(False);
    end;
      
    ATasks[I].Wait;
  end;
end;

{ TThreadedQueue<T> }

constructor TThreadedQueue<T>.Create(ADepth: Integer; ATimeout: Cardinal; APushTimeout: Cardinal);
begin
  inherited Create;
  FQueue := TQueue<T>.Create;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, True, False, ''); // Manual reset
  FShutDown := False;
  FQueueDepth := ADepth;
  FPushTimeout := APushTimeout;
  FPopTimeout := ATimeout;
end;

destructor TThreadedQueue<T>.Destroy;
begin
  DoShutDown;
  FLock.Free;
  FEvent.Free;
  FQueue.Free;
  inherited;
end;

function TThreadedQueue<T>.PushItem(const AItem: T): TWaitResult;
begin
  if FShutDown then Exit(wrAbandoned);
  
  FLock.Enter;
  try
    FQueue.Enqueue(AItem);
    FEvent.SetEvent;
    Result := wrSignaled;
  finally
    FLock.Leave;
  end;
end;

function TThreadedQueue<T>.PopItem(out AItem: T; ATimeout: Cardinal): TWaitResult;
var
  StartTime: QWord;
  Elapsed: QWord;
begin
  if FShutDown then Exit(wrAbandoned);
  
  StartTime := GetTickCount64;
  
  while not FShutDown do
  begin
    FLock.Enter;
    if FQueue.Count > 0 then
    begin
      AItem := FQueue.Dequeue;
      if FQueue.Count = 0 then FEvent.ResetEvent;
      FLock.Leave;
      Exit(wrSignaled);
    end;
    FEvent.ResetEvent;
    FLock.Leave;
    
    // Check Timeout
    Elapsed := GetTickCount64 - StartTime;
    if (ATimeout <> INFINITE) and (Elapsed >= ATimeout) then
      Exit(wrTimeout);
      
    // Wait
    if FEvent.WaitFor(100) = wrSignaled then
      Continue; 
      
    if FShutDown then Exit(wrAbandoned);
  end;
  Result := wrAbandoned;
end;

procedure TThreadedQueue<T>.DoShutDown;
begin
  FShutDown := True;
  FEvent.SetEvent;
end;

{ TInterlocked - Operaciones atómicas para FPC }

class function TInterlocked.Increment(var Target: Integer): Integer;
begin
  Result := InterlockedIncrement(Target);
end;

class function TInterlocked.Decrement(var Target: Integer): Integer;
begin
  Result := InterlockedDecrement(Target);
end;

class function TInterlocked.Exchange(var Target: Integer; Value: Integer): Integer;
begin
  Result := InterlockedExchange(Target, Value);
end;

class function TInterlocked.Exchange(var Target: Boolean; Value: Boolean): Boolean;
var
  OldVal, NewVal: Integer;
begin
  // Boolean como Integer: False=0, True=1
  if Value then NewVal := 1 else NewVal := 0;
  OldVal := InterlockedExchange(PInteger(@Target)^, NewVal);
  Result := (OldVal <> 0);
end;

class function TInterlocked.Exchange(var Target: Pointer; Value: Pointer): Pointer;
begin
  // FPC: Usar InterLockedExchange64 para punteros en x64
  {$IFDEF CPU64}
  Result := Pointer(System.InterLockedExchange64(Int64(Target), Int64(Value)));
  {$ELSE}
  Result := Pointer(InterlockedExchange(LongWord(Target), LongWord(Value)));
  {$ENDIF}
end;

class function TInterlocked.CompareExchange(var Target: Integer; Value, Comparand: Integer): Integer;
begin
  Result := InterlockedCompareExchange(Target, Value, Comparand);
end;
{$ENDIF}

{$IFNDEF FPC}
{ TInterlockedHelper - Delphi }
class function TInterlockedHelper.ExchangeBool(var Target: Boolean; Value: Boolean): Boolean;
var
  OldVal: Integer;
begin
  // Usamos Integer para el exchange atómico y convertimos
  if Value then
    OldVal := TInterlocked.Exchange(PInteger(@Target)^, 1)
  else
    OldVal := TInterlocked.Exchange(PInteger(@Target)^, 0);
  Result := (OldVal <> 0);
end;
{$ENDIF}

{ AtomicExchangeBool - Función universal para exchange de Boolean }
function AtomicExchangeBool(var Target: Boolean; Value: Boolean): Boolean;
{$IFDEF FPC}
begin
  Result := TInterlocked.Exchange(Target, Value);
end;
{$ELSE}
var
  OldVal: Integer;
begin
  if Value then
    OldVal := TInterlocked.Exchange(PInteger(@Target)^, 1)
  else
    OldVal := TInterlocked.Exchange(PInteger(@Target)^, 0);
  Result := (OldVal <> 0);
end;
{$ENDIF}

end.
