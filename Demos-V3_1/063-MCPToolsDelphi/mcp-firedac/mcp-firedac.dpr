program mcp_firedac;

// =============================================================================
// mcp-firedac  —  Universal FireDAC database MCP server
//
// Uso:
//   mcp-firedac.exe --driver SQLite --database C:\data\mydb.sqlite
//   mcp-firedac.exe --driver PG --host localhost --port 5432 --database mydb --username user --password pass
//   mcp-firedac.exe --driver FB --host localhost --database /path/to/db.fdb --username SYSDBA --password masterkey
//   mcp-firedac.exe --driver MySQL --host localhost --database mydb --username root --password pass
//   mcp-firedac.exe --driver MSSQL --host SERVER\INSTANCE --database mydb --username sa --password pass
//   mcp-firedac.exe --connection-string "DriverID=SQLite;Database=C:\data\mydb.sqlite"
//   mcp-firedac.exe --max-rows 500 --driver SQLite --database mydb.sqlite
//
// Herramientas MCP expuestas:
//   db_query    — SELECT con salida table/json/csv
//   db_execute  — INSERT/UPDATE/DELETE/DDL
//   db_tables   — listar tablas y vistas
//   db_describe — describir columnas de una tabla
//   db_info     — información de la conexión
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.StrUtils,

  // FireDAC core
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Phys.Intf,
  FireDAC.DApt,

  // ── Drivers FireDAC (incluir todos para que se registren automáticamente) ──
  // SQLite (embebido estático — sin DLL)
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLiteWrapper.Stat,

  // PostgreSQL
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,

  // Firebird / InterBase
  FireDAC.Phys.FB,
  FireDAC.Phys.FBDef,

  // MySQL / MariaDB
  FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef,

  // Microsoft SQL Server
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef,

  // Oracle
  FireDAC.Phys.Oracle,
  FireDAC.Phys.OracleDef,

  // Console wait cursor (evita diálogos UI en apps de consola)
  FireDAC.ConsoleUI.Wait,

  // MakerAI MCP server
  uMakerAi.MCPServer.Core in '..\..\..\..\..\Source\MCPServer\uMakerAi.MCPServer.Core.pas',
  UMakerAi.MCPServer.Stdio in '..\..\..\..\..\Source\MCPServer\UMakerAi.MCPServer.Stdio.pas',

  uFireDACTool in 'uFireDACTool.pas';

// =============================================================================
// Parser de argumentos CLI
// =============================================================================

function GetArg(const AName: string; const ADefault: string = ''): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
    begin
      Result := ParamStr(I + 1);
      Exit;
    end;
end;

function GetArgInt(const AName: string; ADefault: Integer): Integer;
var
  S: string;
begin
  S := GetArg(AName, '');
  if S = '' then
    Result := ADefault
  else if not TryStrToInt(S, Result) then
    Result := ADefault;
end;

function HasFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure PrintUsage;
begin
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'mcp-firedac  —  Universal FireDAC database MCP server');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Usage:');
  WriteLn(ErrOutput, '  mcp-firedac.exe --driver <ID> [connection options]');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'FireDAC Driver IDs:');
  WriteLn(ErrOutput, '  SQLite   — SQLite file-based database (embedded, no DLL)');
  WriteLn(ErrOutput, '  PG       — PostgreSQL');
  WriteLn(ErrOutput, '  FB       — Firebird');
  WriteLn(ErrOutput, '  MySQL    — MySQL / MariaDB');
  WriteLn(ErrOutput, '  MSSQL    — Microsoft SQL Server');
  WriteLn(ErrOutput, '  Oracle   — Oracle Database');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Connection options:');
  WriteLn(ErrOutput, '  --database <path|name>   Database file path (SQLite) or name (others)');
  WriteLn(ErrOutput, '  --host <host>            Server hostname (network drivers)');
  WriteLn(ErrOutput, '  --port <port>            Server port (0 = driver default)');
  WriteLn(ErrOutput, '  --username <user>        Login username');
  WriteLn(ErrOutput, '  --password <pass>        Login password');
  WriteLn(ErrOutput, '  --charset <charset>      Character set (e.g. UTF8)');
  WriteLn(ErrOutput, '  --connection-string <cs> Full FireDAC connection string');
  WriteLn(ErrOutput, '  --max-rows <n>           Max rows per query (default: 500)');
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Examples:');
  WriteLn(ErrOutput, '  mcp-firedac.exe --driver SQLite --database C:\data\mydb.sqlite');
  WriteLn(ErrOutput, '  mcp-firedac.exe --driver PG --host localhost --database mydb --username app --password secret');
  WriteLn(ErrOutput, '  mcp-firedac.exe --driver FB --host srv --database /db/prod.fdb --username SYSDBA --password masterkey');
end;

// =============================================================================
// Main
// =============================================================================

var
  Server: TAiMCPStdioServer;

begin
  try
    // ── Mostrar ayuda si se pide ────────────────────────────────────────────
    if HasFlag('--help') or HasFlag('-h') or HasFlag('/?') then
    begin
      PrintUsage;
      ExitCode := 0;
      Exit;
    end;

    // ── Leer configuración de CLI ───────────────────────────────────────────
    GDbConfig.Driver       := GetArg('--driver',            'SQLite');
    GDbConfig.Database     := GetArg('--database',          '');
    GDbConfig.Host         := GetArg('--host',              '');
    GDbConfig.Port         := GetArgInt('--port',           0);
    GDbConfig.Username     := GetArg('--username',          '');
    GDbConfig.Password     := GetArg('--password',          '');
    GDbConfig.Charset      := GetArg('--charset',           '');
    GDbConfig.ConnString   := GetArg('--connection-string', '');
    GDbConfig.MaxRows      := GetArgInt('--max-rows',       500);

    // Validación mínima
    if (GDbConfig.ConnString = '') and (GDbConfig.Database = '') and
       SameText(GDbConfig.Driver, 'SQLite') then
    begin
      WriteLn(ErrOutput, '[mcp-firedac] ERROR: --database is required for SQLite driver.');
      PrintUsage;
      ExitCode := 1;
      Exit;
    end;

    // ── Conectar a la BD ────────────────────────────────────────────────────
    WriteLn(ErrOutput, Format('[mcp-firedac] Connecting: driver=%s database=%s host=%s',
      [GDbConfig.Driver, GDbConfig.Database, GDbConfig.Host]));
    try
      DbConnect;
      WriteLn(ErrOutput, '[mcp-firedac] Database connection established.');
    except on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-firedac] Connection failed: ' + E.Message);
      ExitCode := 1;
      Exit;
    end;
    end;

    // ── Crear y configurar servidor MCP ─────────────────────────────────────
    Server := TAiMCPStdioServer.Create(nil);
    try
      Server.ServerName := 'MCP FireDAC (' + GDbConfig.Driver + ')';

      Server.RegisterTool('db_query',
        function: IAiMCPTool begin Result := TDbQueryTool.Create; end);

      Server.RegisterTool('db_execute',
        function: IAiMCPTool begin Result := TDbExecuteTool.Create; end);

      Server.RegisterTool('db_tables',
        function: IAiMCPTool begin Result := TDbTablesTool.Create; end);

      Server.RegisterTool('db_describe',
        function: IAiMCPTool begin Result := TDbDescribeTool.Create; end);

      Server.RegisterTool('db_info',
        function: IAiMCPTool begin Result := TDbInfoTool.Create; end);

      Server.Start;
      WriteLn(ErrOutput, '[mcp-firedac] Ready. Waiting for JSON-RPC requests...');

      while True do
        Sleep(1000);
    finally
      Server.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-firedac] Fatal error: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
