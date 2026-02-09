unit uPosixHelper;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  BaseUnix, Unix, Classes, SysUtils,
  initc; // Para popen, pclose, etc si es necesario
  {$ELSE}
  Posix.Unistd, Posix.SysWait, Posix.Signal, Posix.Fcntl, Posix.Stdlib, Posix.SysTypes, Posix.Base, Posix.Errno;
  {$ENDIF}

{$IFDEF FPC}
type
  // Tipos compatibles
  TRlimit = BaseUnix.TRlimit;
  pid_t = BaseUnix.pid_t;
  
const
  // Mapeo de constantes
  STDIN_FILENO  = 0;
  STDOUT_FILENO = 1;
  STDERR_FILENO = 2;
  
  // Constantes de Wait
  WNOHANG = BaseUnix.WNOHANG;
  
  // Constantes de Open/Fcntl
  O_NONBLOCK = BaseUnix.O_NONBLOCK;
  F_SETFL    = BaseUnix.F_SETFL;
  
  // Señales
  SIGTERM = BaseUnix.SIGTERM;
  SIGKILL = BaseUnix.SIGKILL;

  // Errores
  EAGAIN = BaseUnix.ESysEAGAIN;

type
  // Wrapper estático para simular namespaces si fuera necesario, 
  // pero usaremos funciones planas para facilitar el reemplazo en el código destino.
  TPosixHelper = class
  end;

// Funciones planas que imitan a Posix.Unistd y otros
function __close(fildes: cint): cint; inline;
function __write(fildes: cint; buf: pointer; nbyte: size_t): ssize_t; inline;
function __read(fildes: cint; buf: pointer; nbyte: size_t): ssize_t; inline;
function __chdir(path: PChar): cint; inline;
function pipe(var fildes: array of cint): cint; inline;
function fork: pid_t; inline;
function waitpid(pid: pid_t; stat_loc: pcint; options: cint): pid_t; inline;
function dup2(fildes: cint; fildes2: cint): cint; inline;
function _exit(status: cint): cint; inline;
function execvp(file_: PChar; argv: PPChar): cint; inline;
function strerror(errnum: cint): string;

// Funciones de Signal/Fcntl
function Kill(pid: pid_t; sig: cint): cint; inline;
function Fcntl(fildes: cint; cmd: cint; arg: cint): cint; inline;

// Macros de status (simuladas)
function WIFEXITED(Status: Integer): Boolean;
function WEXITSTATUS(Status: Integer): Integer;

// Variables externas simuladas/wrappers
function environ: PPChar; 

// Funciones libc externas (popen no está en BaseUnix directamente por defecto en algunas versiones, usamos libc)
function popen(const command: PChar; const modes: PChar): Pointer; cdecl; external 'c' name 'popen';
function pclose(stream: Pointer): cint; cdecl; external 'c' name 'pclose';
function fgets(s: PChar; size: cint; stream: Pointer): PChar; cdecl; external 'c' name 'fgets';
function system(const command: PChar): cint; cdecl; external 'c' name 'system';

{$ENDIF}

implementation

{$IFDEF FPC}

function __close(fildes: cint): cint;
begin
  Result := FpClose(fildes);
end;

function __write(fildes: cint; buf: pointer; nbyte: size_t): ssize_t;
begin
  Result := FpWrite(fildes, buf, nbyte);
end;

function __read(fildes: cint; buf: pointer; nbyte: size_t): ssize_t;
begin
  Result := FpRead(fildes, buf, nbyte);
end;

function __chdir(path: PChar): cint;
begin
  Result := FpChdir(path);
end;

function pipe(var fildes: array of cint): cint;
var
  tpipe: TFilDes; // Array[0..1] of cint en BaseUnix
begin
  // FpPipe espera un array accesible.
  // En BaseUnix: function FpPipe(var fildes : tfildes):cint;
  // tfildes = array[0..1] of cint;
  if Length(fildes) < 2 then Exit(-1);
  Result := FpPipe(tpipe);
  fildes[0] := tpipe[0];
  fildes[1] := tpipe[1];
end;

function fork: pid_t;
begin
  Result := FpFork;
end;

function waitpid(pid: pid_t; stat_loc: pcint; options: cint): pid_t;
begin
  Result := FpWaitPid(pid, stat_loc, options);
end;

function dup2(fildes: cint; fildes2: cint): cint;
begin
  Result := FpDup2(fildes, fildes2);
end;

function _exit(status: cint): cint;
begin
  // FPC no tiene _exit directo en BaseUnix que retorne, llama a syscall exit.
  // FpExit no retorna.
  FpExit(status);
  Result := 0; 
end;

function execvp(file_: PChar; argv: PPChar): cint;
begin
  Result := FpExecVP(file_, argv);
end;

function strerror(errnum: cint): string;
begin
  // FPC sysutils tiene StrError? O BaseUnix?
  // BaseUnix no tiene strerror. SysUtils.SysErrorMessage usa OS error.
  Result := SysUtils.SysErrorMessage(errnum);
end;

function Kill(pid: pid_t; sig: cint): cint;
begin
  Result := FpKill(pid, sig);
end;

function Fcntl(fildes: cint; cmd: cint; arg: cint): cint;
begin
  Result := FpFcntl(fildes, cmd, arg);
end;

function WIFEXITED(Status: Integer): Boolean;
begin
  Result := BaseUnix.WIFEXITED(Status);
end;

function WEXITSTATUS(Status: Integer): Integer;
begin
  Result := BaseUnix.WEXITSTATUS(Status);
end;

function environ: PPChar;
begin
  // En FPC Linux, 'envp' es una variable global en System o InitC.
  // Acceso directo suele funcionar mejor si System. qualif falla.
  Result := envp;
end;

{$ENDIF}
end.

