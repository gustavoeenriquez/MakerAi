// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
//
// Logger profesional para MakerAI Framework
//
// Casos de uso:
// - Aplicaciones de servidor que necesitan logging confiable 24/7
// - Sistemas que requieren auditoría y trazabilidad de eventos
// - Servicios en producción con alta carga (thread-safe)
// - Daemons/servicios Windows/Linux con logs rotantes automáticos
//
// Características:
// - Thread-safe: Múltiples hilos pueden escribir simultáneamente
// - 5 niveles de severidad: Debug, Info, Warning, Error, Fatal
// - Rotación automática: Divide archivos grandes (default 100MB)
// - Logs diarios: Un archivo por día (makerai_yyyy-mm-dd.log)
// - Singleton: Acceso global sin necesidad de pasar instancias
//
// Ejemplos de uso real:
// - Servidores MCP que ejecutan comandos shell
// - APIs REST que procesan requests de AI
// - Aplicaciones de escritorio que necesitan diagnóstico
// - Servicios batch que procesan datos en segundo plano

unit uMakerAi.Logger;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  // FPC: Unidades estándar de FPC sin prefijo System
  {$IFDEF FPC}
  Classes, SysUtils, SyncObjs;
  {$ELSE}
  // Delphi: Unidades con namespace System
  System.Classes, System.SysUtils, System.SyncObjs;
  {$ENDIF}

type
  // Niveles de logging en orden ascendente de severidad
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);

  { TMakerAiLogger - Logger thread-safe con rotación automática
    
    Características:
    - Singleton: Usar Logger.Info(), Logger.Error(), etc.
    - Thread-safe: Usa TCriticalSection para acceso concurrente
    - Rotación: Cuando el log alcanza MaxFileSizeMB, rota a .log.1, .log.2, ...
    - Logs diarios: Crea un archivo por día (makerai_yyyy-mm-dd.log)
    - Flexible: Nivel mínimo configurable (Debug, Info, Warning, Error, Fatal)
    
    Uso:
      TMakerAiLogger.Initialize('logs', llInfo);
      Logger.Info('Aplicación iniciada');
      Logger.Error('Error crítico');
  }
  TMakerAiLogger = class
  private
    FLogFile: TextFile;                 // Archivo de texto actual
    FLogDir: string;                     // Directorio de logs
    FLogFileName: string;                // Nombre completo del archivo actual
    FMinLevel: TLogLevel;                // Nivel mínimo para escribir (ej: llInfo ignora Debug)
    FLock: TCriticalSection;             // Lock para thread-safety
    FMaxFileSizeMB: Integer;             // Tamaño máximo antes de rotar (default: 100MB)
    FRotateCount: Integer;               // Cantidad de archivos históricos a mantener
    FCurrentSize: Int64;                 // Tamaño actual del archivo en bytes
    FIsOpen: Boolean;                    // Estado del archivo
    
    class var FInstance: TMakerAiLogger; // Singleton instance
    
    procedure OpenLogFile;
    procedure CloseLogFile;
    procedure RotateLogIfNeeded;
    function LevelToString(Level: TLogLevel): string;
    function GetLogFileName: string;
    
  public
    constructor Create(const LogDir: string; MinLevel: TLogLevel = llInfo);
    destructor Destroy; override;
    
    class function Instance: TMakerAiLogger;
    class procedure Initialize(const LogDir: string; MinLevel: TLogLevel = llInfo);
    class procedure Finalize;
    
    procedure Log(Level: TLogLevel; const Msg: string);
    procedure Debug(const Msg: string);
    procedure Info(const Msg: string);
    procedure Warning(const Msg: string);
    procedure Error(const Msg: string);
    procedure Fatal(const Msg: string);
    
    property MinLevel: TLogLevel read FMinLevel write FMinLevel;
    property MaxFileSizeMB: Integer read FMaxFileSizeMB write FMaxFileSizeMB;
  end;

// Función helper global para acceso rápido al singleton
// Uso: Logger.Info('mensaje'), Logger.Error('error'), etc.
function Logger: TMakerAiLogger; inline;

implementation

uses
  {$IFDEF FPC}
  DateUtils;
  {$ELSE}
  System.DateUtils;
  {$ENDIF}

function Logger: TMakerAiLogger;
begin
  Result := TMakerAiLogger.Instance;
end;

{ TMakerAiLogger }

constructor TMakerAiLogger.Create(const LogDir: string; MinLevel: TLogLevel);
begin
  inherited Create;
  FLogDir := LogDir;
  FMinLevel := MinLevel;
  FMaxFileSizeMB := 100;  // 100MB por defecto
  FRotateCount := 5;
  FLock := TCriticalSection.Create;
  FIsOpen := False;
  FCurrentSize := 0;
  
  if not DirectoryExists(FLogDir) then
    ForceDirectories(FLogDir);
    
  OpenLogFile;
end;

destructor TMakerAiLogger.Destroy;
begin
  CloseLogFile;
  FLock.Free;
  inherited;
end;

class function TMakerAiLogger.Instance: TMakerAiLogger;
begin
  if FInstance = nil then
    FInstance := TMakerAiLogger.Create(ExtractFilePath(ParamStr(0)) + 'logs', llInfo);
  Result := FInstance;
end;

class procedure TMakerAiLogger.Initialize(const LogDir: string; MinLevel: TLogLevel);
begin
  if FInstance <> nil then
    FInstance.Free;
  FInstance := TMakerAiLogger.Create(LogDir, MinLevel);
end;

class procedure TMakerAiLogger.Finalize;
begin
  if FInstance <> nil then
  begin
    FInstance.Free;
   FInstance := nil;
  end;
end;

function TMakerAiLogger.GetLogFileName: string;
var
  ExeName: string;
begin
  // Usar nombre del ejecutable + fecha
  ExeName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Result := FLogDir + PathDelim + ExeName + '_' + 
            FormatDateTime('yyyy-mm-dd', Now) + '.log';
end;

procedure TMakerAiLogger.OpenLogFile;
var
  FileInfo: TSearchRec;
begin
  FLock.Enter;
  try
    if FIsOpen then
      CloseLogFile;
      
    FLogFileName := GetLogFileName;
    AssignFile(FLogFile, FLogFileName);
    
    if FileExists(FLogFileName) then
    begin
      Append(FLogFile);
      // Obtener tamaño del archivo
      if FindFirst(FLogFileName, faAnyFile, FileInfo) = 0 then
      begin
        FCurrentSize := FileInfo.Size;
        FindClose(FileInfo);
      end
      else
        FCurrentSize := 0;
    end
    else
    begin
      Rewrite(FLogFile);
      FCurrentSize := 0;
    end;
    
    FIsOpen := True;
  finally
    FLock.Leave;
  end;
end;

procedure TMakerAiLogger.CloseLogFile;
begin
  if FIsOpen then
  begin
    CloseFile(FLogFile);
    FIsOpen := False;
  end;
end;

procedure TMakerAiLogger.RotateLogIfNeeded;
var
  i: Integer;
  OldName, NewName: string;
begin
  if FCurrentSize < (FMaxFileSizeMB * 1024 * 1024) then
    Exit;
    
  CloseLogFile;
  
  // Rotar logs: .log.4 -> .log.5, .log.3 -> .log.4, etc
  for i := FRotateCount - 1 downto 1 do
  begin
    OldName := FLogFileName + '.' + IntToStr(i);
    NewName := FLogFileName + '.' + IntToStr(i + 1);
    if FileExists(NewName) then
      DeleteFile(NewName);
    if FileExists(OldName) then
      RenameFile(OldName, NewName);
  end;
  
  // Mover log actual a .log.1
  if FileExists(FLogFileName) then
    RenameFile(FLogFileName, FLogFileName + '.1');
    
  OpenLogFile;
end;

function TMakerAiLogger.LevelToString(Level: TLogLevel): string;
begin
  case Level of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO ';
    llWarning: Result := 'WARN ';
    llError: Result := 'ERROR';
    llFatal: Result := 'FATAL';
  end;
end;

procedure TMakerAiLogger.Log(Level: TLogLevel; const Msg: string);
var
  LogMsg: string;
  MsgBytes: Integer;
begin
  if Level < FMinLevel then
    Exit;
    
  FLock.Enter;
  try
    if not FIsOpen then
      OpenLogFile;
      
    LogMsg := Format('[%s] [%s] %s', [
      FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
      LevelToString(Level),
      Msg
    ]);
    
    WriteLn(FLogFile, LogMsg);
    Flush(FLogFile);
    
    // Actualizar tamaño
    MsgBytes := Length(LogMsg) + 2;  // +2 for CRLF
    Inc(FCurrentSize, MsgBytes);
    
    // Verificar si necesita rotación
    RotateLogIfNeeded;
    
    // También output a consola para errores y fatales
    if Level >= llError then
      WriteLn(LogMsg);
  finally
    FLock.Leave;
  end;
end;

procedure TMakerAiLogger.Debug(const Msg: string);
begin
  Log(llDebug, Msg);
end;

procedure TMakerAiLogger.Info(const Msg: string);
begin
  Log(llInfo, Msg);
end;

procedure TMakerAiLogger.Warning(const Msg: string);
begin
  Log(llWarning, Msg);
end;

procedure TMakerAiLogger.Error(const Msg: string);
begin
  Log(llError, Msg);
end;

procedure TMakerAiLogger.Fatal(const Msg: string);
begin
  Log(llFatal, Msg);
end;

initialization
  // Se creará al primer uso

finalization
  TMakerAiLogger.Finalize;

end.
