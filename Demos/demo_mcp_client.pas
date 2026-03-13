program demo_mcp_client;
{$mode objfpc}{$H+}

// Demo: TMCPClientHttp — cliente MCP HTTP
//
// Se conecta al servidor demo_mcp_server.pas en http://localhost:8088/mcp.
// Requiere que demo_mcp_server.pas este corriendo antes de ejecutar este demo.
//
// Pasos:
//   1. Ejecutar demo_mcp_server (en otra terminal)
//   2. Ejecutar este demo
//
// Compilar con:
//   fpc demo_mcp_client.pas -Fu../Source/Core -Fu../Source/MCPClient -Fu../Source/Tools

uses
  SysUtils, Classes, Contnrs,
  fpjson, jsonparser,
  uMakerAi.Core,
  uMakerAi.MCPClient.Core;

procedure PrintToolsList(ATools: TJSONObject);
var
  ToolsNode: TJSONData;
  ToolsArr : TJSONArray;
  ToolObj  : TJSONObject;
  I        : Integer;
begin
  if not Assigned(ATools) then
  begin
    WriteLn('  (sin herramientas o error)');
    Exit;
  end;

  ToolsNode := ATools.Find('tools');
  if not (Assigned(ToolsNode) and (ToolsNode is TJSONArray)) then
  begin
    WriteLn('  (respuesta sin campo "tools")');
    Exit;
  end;

  ToolsArr := TJSONArray(ToolsNode);
  WriteLn('  Total: ', ToolsArr.Count, ' herramienta(s)');
  for I := 0 to ToolsArr.Count - 1 do
  begin
    if ToolsArr[I] is TJSONObject then
    begin
      ToolObj := TJSONObject(ToolsArr[I]);
      WriteLn('  [', I+1, '] ', ToolObj.Get('name', '?'),
              ' — ', ToolObj.Get('description', ''));
    end;
  end;
end;

procedure PrintCallResult(AResult: TJSONObject);
var
  ContentNode: TJSONData;
  ContentArr : TJSONArray;
  Item       : TJSONObject;
  I          : Integer;
begin
  if not Assigned(AResult) then
  begin
    WriteLn('  (resultado nulo o error)');
    Exit;
  end;

  ContentNode := AResult.Find('content');
  if not (Assigned(ContentNode) and (ContentNode is TJSONArray)) then
  begin
    WriteLn('  ', AResult.AsJSON);
    Exit;
  end;

  ContentArr := TJSONArray(ContentNode);
  for I := 0 to ContentArr.Count - 1 do
  begin
    if ContentArr[I] is TJSONObject then
    begin
      Item := TJSONObject(ContentArr[I]);
      if SameText(Item.Get('type', ''), 'text') then
        WriteLn('  ', Item.Get('text', ''))
      else
        WriteLn('  [', Item.Get('type', '?'), '] ', Item.AsJSON);
    end;
  end;

  // Indicar si es error
  if AResult.Get('isError', False) then
    WriteLn('  [ERROR MCP]');
end;

var
  Client    : TMCPClientHttp;
  ToolsJSON : TJSONObject;
  CallResult: TJSONObject;
  Args      : TJSONObject;

const
  SERVER_URL = 'http://localhost:8088/mcp';

begin
  WriteLn('=== MakerAI FPC — Demo MCP HTTP Client ===');
  WriteLn;
  WriteLn('Conectando a: ', SERVER_URL);
  WriteLn;

  Client := TMCPClientHttp.Create(nil);
  try
    Client.URL := SERVER_URL;

    // --- Inicializar (handshake MCP) ---
    Write('Inicializando cliente MCP... ');
    if Client.Initialize then
    begin
      WriteLn('OK');
      WriteLn;
    end
    else
    begin
      WriteLn('FALLO');
      WriteLn;
      WriteLn('[ERROR] No se pudo conectar al servidor MCP.');
      WriteLn('Verifique que demo_mcp_server esta corriendo en ' + SERVER_URL);
      WriteLn;
      WriteLn('Error: ', Client.LastError);
      Exit;
    end;

    // --- Listar herramientas ---
    WriteLn('--- tools/list ---');
    ToolsJSON := Client.ListTools;
    PrintToolsList(ToolsJSON);
    if Assigned(ToolsJSON) then
      ToolsJSON.Free;
    WriteLn;

    // --- Llamar echo ---
    WriteLn('--- tools/call: echo ---');
    Args := TJSONObject.Create;
    Args.Add('message', TJSONString.Create('Hello from FPC client!'));
    CallResult := Client.CallTool('echo', Args, nil);
    PrintCallResult(CallResult);
    if Assigned(CallResult) then
      CallResult.Free;
    WriteLn;

    // --- Llamar get_time ---
    WriteLn('--- tools/call: get_time ---');
    CallResult := Client.CallTool('get_time', TJSONObject(nil), nil);
    PrintCallResult(CallResult);
    if Assigned(CallResult) then
      CallResult.Free;
    WriteLn;

    // --- Llamar calc_add ---
    WriteLn('--- tools/call: calc_add (7 + 8) ---');
    Args := TJSONObject.Create;
    Args.Add('a', TJSONFloatNumber.Create(7));
    Args.Add('b', TJSONFloatNumber.Create(8));
    CallResult := Client.CallTool('calc_add', Args, nil);
    PrintCallResult(CallResult);
    if Assigned(CallResult) then
      CallResult.Free;
    WriteLn;

    // --- Llamar herramienta inexistente (debe dar error MCP) ---
    WriteLn('--- tools/call: herramienta_inexistente (debe dar error) ---');
    try
      CallResult := Client.CallTool('herramienta_inexistente',
          TJSONObject(nil), nil);
      PrintCallResult(CallResult);
      if Assigned(CallResult) then
        CallResult.Free;
    except
      on E: Exception do
        WriteLn('  [Excepcion esperada]: ', E.Message);
    end;
    WriteLn;

    WriteLn('Demo finalizado correctamente.');

  finally
    Client.Free;
  end;
end.
