program demo_mcp_client_stdio;
{$mode objfpc}{$H+}

// Demo: TMCPClientStdIo — cliente MCP que lanza un servidor como subproceso
//
// Lanza demo_mcp_server_stdio.exe como subproceso y se comunica con el
// a traves de pipes (stdin/stdout), usando el protocolo MCP JSON-RPC.
//
// Pasos:
//   1. Compilar primero demo_mcp_server_stdio.pas
//   2. Ejecutar este demo (no necesita que el servidor este corriendo)
//      El cliente lo lanza automaticamente.
//
// Compilar con:
//   fpc demo_mcp_client_stdio.pas -Fu../Source/Core -Fu../Source/MCPClient

uses
  SysUtils, Classes, Contnrs,
  fpjson, jsonparser,
  uMakerAi.Core,
  uMakerAi.MCPClient.Core;

// ---------------------------------------------------------------------------
// Helpers de impresion
// ---------------------------------------------------------------------------

procedure PrintToolsList(ATools: TJSONObject);
var
  ToolsNode: TJSONData;
  ToolsArr : TJSONArray;
  ToolObj  : TJSONObject;
  I        : Integer;
begin
  if not Assigned(ATools) then
  begin
    WriteLn('  (respuesta vacia)');
    Exit;
  end;

  ToolsNode := ATools.Find('tools');
  if not (ToolsNode is TJSONArray) then
  begin
    WriteLn('  (sin array "tools")');
    Exit;
  end;

  ToolsArr := TJSONArray(ToolsNode);
  WriteLn('  Total: ', ToolsArr.Count, ' herramienta(s)');

  for I := 0 to ToolsArr.Count - 1 do
  begin
    if ToolsArr.Items[I] is TJSONObject then
    begin
      ToolObj := TJSONObject(ToolsArr.Items[I]);
      WriteLn('  [', I+1, '] ', ToolObj.Get('name', '?'),
              ' — ', ToolObj.Get('description', ''));
    end;
  end;
end;

procedure PrintToolResult(const ALabel: string; AResult: TJSONObject);
var
  ContentNode: TJSONData;
  ContentArr : TJSONArray;
  Item       : TJSONObject;
  I          : Integer;
begin
  Write('  ', ALabel, ': ');
  if not Assigned(AResult) then
  begin
    WriteLn('(nil)');
    Exit;
  end;

  ContentNode := AResult.Find('content');
  if not (ContentNode is TJSONArray) then
  begin
    WriteLn(AResult.AsJSON);
    Exit;
  end;

  ContentArr := TJSONArray(ContentNode);
  for I := 0 to ContentArr.Count - 1 do
  begin
    if ContentArr.Items[I] is TJSONObject then
    begin
      Item := TJSONObject(ContentArr.Items[I]);
      if Item.Get('type', '') = 'text' then
        WriteLn(Item.Get('text', ''));
    end;
  end;
end;

// ---------------------------------------------------------------------------
// Programa principal
// ---------------------------------------------------------------------------

var
  Client   : TMCPClientStdIo;
  Tools    : TJSONObject;
  Result   : TJSONObject;
  Args     : TJSONObject;
  ExePath  : string;

begin
  WriteLn('=== MakerAI FPC — Demo MCP StdIO Client ===');
  WriteLn;

  // Ruta al servidor StdIO (mismo directorio que este exe)
  ExePath := ExtractFilePath(ParamStr(0));
  if ExePath = '' then
    ExePath := './';

  Client := TMCPClientStdIo.Create(nil);
  try
    // Configurar el comando a ejecutar (el servidor StdIO)
    Client.Params.Values['Command'] := ExePath + 'demo_mcp_server_stdio.exe';
    Client.Params.Values['Arguments'] := '';  // sin argumentos extra
    Client.Params.Values['RootDir']   := ExePath;
    Client.Params.Values['Timeout']   := '15000';

    WriteLn('Lanzando servidor MCP (StdIO): ',
        Client.Params.Values['Command']);
    WriteLn;

    // Initialize: lanza el proceso, hace handshake MCP, carga tools
    Write('Inicializando cliente MCP StdIO... ');
    if not Client.Initialize then
    begin
      WriteLn('FALLO');
      WriteLn('Error: ', Client.LastError);
      ExitCode := 1;
      Exit;
    end;
    WriteLn('OK');
    WriteLn;

    // --- tools/list ---
    WriteLn('--- tools/list ---');
    Tools := Client.ListTools;
    try
      PrintToolsList(Tools);
    finally
      Tools.Free;
    end;
    WriteLn;

    // --- tools/call: echo ---
    WriteLn('--- tools/call: echo ---');
    Args := TJSONObject.Create;
    Args.Add('message', TJSONString.Create('Hola desde FPC StdIO client!'));
    Result := Client.CallTool('echo', Args, nil);
    try
      PrintToolResult('Respuesta', Result);
    finally
      Result.Free;
    end;
    WriteLn;

    // --- tools/call: get_time ---
    WriteLn('--- tools/call: get_time ---');
    Result := Client.CallTool('get_time', TJSONObject.Create, nil);
    try
      PrintToolResult('Respuesta', Result);
    finally
      Result.Free;
    end;
    WriteLn;

    // --- tools/call: calc_add ---
    WriteLn('--- tools/call: calc_add (12 + 30) ---');
    Args := TJSONObject.Create;
    Args.Add('a', TJSONFloatNumber.Create(Double(12)));
    Args.Add('b', TJSONFloatNumber.Create(Double(30)));
    Result := Client.CallTool('calc_add', Args, nil);
    try
      PrintToolResult('Resultado', Result);
    finally
      Result.Free;
    end;
    WriteLn;

    // --- error esperado ---
    WriteLn('--- tools/call: herramienta_inexistente (debe dar error) ---');
    try
      Result := Client.CallTool('herramienta_inexistente', TJSONObject(nil), nil);
      if Assigned(Result) then
        Result.Free;
      WriteLn('  [Error: se esperaba excepcion]');
    except
      on E: Exception do
        WriteLn('  [Excepcion esperada]: ', E.Message);
    end;
    WriteLn;

    WriteLn('Demo finalizado correctamente.');

  finally
    // El destructor de TMCPClientStdIo termina el subproceso
    Client.Free;
  end;
end.
