program demo_mcp_direct;
{$mode objfpc}{$H+}

// Demo: TAiMCPDirectConnection — conexion in-process sin red
//
// Demuestra como:
//   1. Crear herramientas MCP simples (TAiEchoTool, TAiGetTimeTool)
//   2. Registrarlas en un servidor directo
//   3. Llamarlas desde el mismo proceso
//
// No requiere red ni variables de entorno.
// Compilar con:
//   fpc demo_mcp_direct.pas -Fu../Source/Core -Fu../Source/MCPServer -Fu../Source/Tools

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Direct,
  uMakerAi.Tools.Functions;

type
  // -------------------------------------------------------------------------
  // TAiEchoTool - devuelve el mensaje recibido
  // -------------------------------------------------------------------------
  TAiEchoTool = class(TAiMCPToolBase)
  public
    function GetInputSchema: TJSONObject; override;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; override;
  end;

  // -------------------------------------------------------------------------
  // TAiGetTimeTool - devuelve la hora actual del sistema
  // -------------------------------------------------------------------------
  TAiGetTimeTool = class(TAiMCPToolBase)
  public
    function GetInputSchema: TJSONObject; override;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; override;
  end;

// ---------------------------------------------------------------------------
// TAiEchoTool
// ---------------------------------------------------------------------------

function TAiEchoTool.GetInputSchema: TJSONObject;
var
  Props, MsgProp: TJSONObject;
  Required: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.Add('type', TJSONString.Create('object'));

  Props   := TJSONObject.Create;
  MsgProp := TJSONObject.Create;
  MsgProp.Add('type', TJSONString.Create('string'));
  MsgProp.Add('description', TJSONString.Create('Mensaje a repetir'));
  Props.Add('message', MsgProp);
  Result.Add('properties', Props);

  Required := TJSONArray.Create;
  Required.Add(TJSONString.Create('message'));
  Result.Add('required', Required);
end;

function TAiEchoTool.Execute(const Arguments: TJSONObject;
    const AuthContext: TAiAuthContext): TJSONObject;
var
  Msg: string;
begin
  if Assigned(Arguments) then
    Msg := Arguments.Get('message', '(sin mensaje)')
  else
    Msg := '(sin argumentos)';

  Result := TAiMCPResponseBuilder.New
      .AddText('Echo: ' + Msg)
      .Build;
end;

// ---------------------------------------------------------------------------
// TAiGetTimeTool
// ---------------------------------------------------------------------------

function TAiGetTimeTool.GetInputSchema: TJSONObject;
begin
  // Sin parametros requeridos
  Result := TJSONObject.Create;
  Result.Add('type', TJSONString.Create('object'));
  Result.Add('properties', TJSONObject.Create);
end;

function TAiGetTimeTool.Execute(const Arguments: TJSONObject;
    const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New
      .AddText('Hora actual: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
      .Build;
end;

// ---------------------------------------------------------------------------
// Programa principal
// ---------------------------------------------------------------------------

var
  Server   : TAiMCPDirectConnection;
  EchoTool : TAiEchoTool;
  TimeTool : TAiGetTimeTool;
  ToolsJSON: TJSONObject;
  CallResult: TJSONObject;
  Args     : TJSONObject;
  ContentNode: TJSONData;
  ContentArr : TJSONArray;
  I          : Integer;

begin
  WriteLn('=== MakerAI FPC — Demo MCP Direct Connection ===');
  WriteLn;

  Server   := TAiMCPDirectConnection.Create(nil);
  EchoTool := TAiEchoTool.Create('echo', 'Repite el mensaje recibido');
  TimeTool := TAiGetTimeTool.Create('get_time',
      'Devuelve la hora actual del sistema');
  try
    // Registrar herramientas
    Server.RegisterTool('echo', EchoTool);
    Server.RegisterTool('get_time', TimeTool);

    // Iniciar el servidor (in-process)
    Server.Start;

    WriteLn('Servidor MCP directo iniciado.');
    WriteLn;

    // --- 1. ListTools ---
    WriteLn('--- tools/list ---');
    ToolsJSON := Server.ListTools;
    if Assigned(ToolsJSON) then
    begin
      try
        ContentNode := ToolsJSON.Find('tools');
        if Assigned(ContentNode) and (ContentNode is TJSONArray) then
        begin
          WriteLn('Herramientas disponibles (', TJSONArray(ContentNode).Count, '):');
          for I := 0 to TJSONArray(ContentNode).Count - 1 do
          begin
            if TJSONArray(ContentNode)[I] is TJSONObject then
              WriteLn('  - ', TJSONObject(TJSONArray(ContentNode)[I]).Get('name', '?'));
          end;
        end;
      finally
        ToolsJSON.Free;
      end;
    end;

    WriteLn;

    // --- 2. CallTool echo ---
    WriteLn('--- tools/call echo ---');
    Args := TJSONObject.Create;
    Args.Add('message', TJSONString.Create('Hello MCP!'));
    // NOTA: Args es consumido por CallTool (no liberar despues)
    CallResult := Server.CallTool('echo', Args);
    if Assigned(CallResult) then
    begin
      try
        ContentNode := CallResult.Find('content');
        if Assigned(ContentNode) and (ContentNode is TJSONArray) then
        begin
          ContentArr := TJSONArray(ContentNode);
          for I := 0 to ContentArr.Count - 1 do
          begin
            if ContentArr[I] is TJSONObject then
              WriteLn('  ', TJSONObject(ContentArr[I]).Get('text', ''));
          end;
        end;
      finally
        CallResult.Free;
      end;
    end;

    WriteLn;

    // --- 3. CallTool get_time ---
    WriteLn('--- tools/call get_time ---');
    CallResult := Server.CallTool('get_time', TJSONObject(nil));
    if Assigned(CallResult) then
    begin
      try
        ContentNode := CallResult.Find('content');
        if Assigned(ContentNode) and (ContentNode is TJSONArray) then
        begin
          ContentArr := TJSONArray(ContentNode);
          for I := 0 to ContentArr.Count - 1 do
          begin
            if ContentArr[I] is TJSONObject then
              WriteLn('  ', TJSONObject(ContentArr[I]).Get('text', ''));
          end;
        end;
      finally
        CallResult.Free;
      end;
    end;

    WriteLn;
    WriteLn('Demo finalizado correctamente.');

    Server.Stop;

  finally
    // EchoTool y TimeTool son interfaces — liberados automaticamente
    Server.Free;
  end;
end.
