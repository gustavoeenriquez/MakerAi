program demo_mcp_server;
{$mode objfpc}{$H+}

// Demo: TAiMCPHttpServer — servidor MCP HTTP
//
// Levanta un servidor MCP en http://localhost:8088/mcp con 3 herramientas:
//   - echo        : repite el mensaje recibido
//   - get_time    : devuelve la hora actual
//   - calc_add    : suma dos numeros a + b
//
// Para probar con curl:
//   curl -X POST http://localhost:8088/mcp \
//     -H "Content-Type: application/json" \
//     -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'
//
//   curl -X POST http://localhost:8088/mcp \
//     -H "Content-Type: application/json" \
//     -d '{"jsonrpc":"2.0","method":"tools/call","id":2,"params":{"name":"echo","arguments":{"message":"Hola!"}}}'
//
//   curl -X POST http://localhost:8088/mcp \
//     -H "Content-Type: application/json" \
//     -d '{"jsonrpc":"2.0","method":"tools/call","id":3,"params":{"name":"calc_add","arguments":{"a":5,"b":3}}}'
//
// Compilar con:
//   fpc demo_mcp_server.pas -Fu../Source/Core -Fu../Source/MCPServer -Fu../Source/Tools

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Http,
  uMakerAi.Tools.Functions;

type
  // -------------------------------------------------------------------------
  // TAiEchoTool
  // -------------------------------------------------------------------------
  TAiEchoTool = class(TAiMCPToolBase)
  public
    function GetInputSchema: TJSONObject; override;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; override;
  end;

  // -------------------------------------------------------------------------
  // TAiGetTimeTool
  // -------------------------------------------------------------------------
  TAiGetTimeTool = class(TAiMCPToolBase)
  public
    function GetInputSchema: TJSONObject; override;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; override;
  end;

  // -------------------------------------------------------------------------
  // TAiCalcTool - suma dos numeros
  // -------------------------------------------------------------------------
  TAiCalcTool = class(TAiMCPToolBase)
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
  Msg := '';
  if Assigned(Arguments) then
    Msg := Arguments.Get('message', '');
  if Msg = '' then
    Msg := '(sin mensaje)';
  Result := TAiMCPResponseBuilder.New.AddText('Echo: ' + Msg).Build;
end;

// ---------------------------------------------------------------------------
// TAiGetTimeTool
// ---------------------------------------------------------------------------

function TAiGetTimeTool.GetInputSchema: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('type', TJSONString.Create('object'));
  Result.Add('properties', TJSONObject.Create);
end;

function TAiGetTimeTool.Execute(const Arguments: TJSONObject;
    const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New
      .AddText('Hora del servidor: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
      .Build;
end;

// ---------------------------------------------------------------------------
// TAiCalcTool
// ---------------------------------------------------------------------------

function TAiCalcTool.GetInputSchema: TJSONObject;
var
  Props, AProp, BProp: TJSONObject;
  Required: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.Add('type', TJSONString.Create('object'));

  Props := TJSONObject.Create;

  AProp := TJSONObject.Create;
  AProp.Add('type', TJSONString.Create('number'));
  AProp.Add('description', TJSONString.Create('Primer operando'));
  Props.Add('a', AProp);

  BProp := TJSONObject.Create;
  BProp.Add('type', TJSONString.Create('number'));
  BProp.Add('description', TJSONString.Create('Segundo operando'));
  Props.Add('b', BProp);

  Result.Add('properties', Props);

  Required := TJSONArray.Create;
  Required.Add(TJSONString.Create('a'));
  Required.Add(TJSONString.Create('b'));
  Result.Add('required', Required);
end;

function TAiCalcTool.Execute(const Arguments: TJSONObject;
    const AuthContext: TAiAuthContext): TJSONObject;
var
  A, B, Sum: Double;
begin
  A := 0;
  B := 0;
  if Assigned(Arguments) then
  begin
    A := Arguments.Get('a', Double(0.0));
    B := Arguments.Get('b', Double(0.0));
  end;
  Sum := A + B;
  Result := TAiMCPResponseBuilder.New
      .AddText(Format('%.4g + %.4g = %.4g', [A, B, Sum]))
      .Build;
end;

// ---------------------------------------------------------------------------
// Programa principal
// ---------------------------------------------------------------------------

const
  SERVER_PORT = 8088;

var
  Server   : TAiMCPHttpServer;
  EchoTool : TAiEchoTool;
  TimeTool : TAiGetTimeTool;
  CalcTool : TAiCalcTool;
  Line     : string;

begin
  WriteLn('=== MakerAI FPC — Demo MCP HTTP Server ===');
  WriteLn;

  Server   := TAiMCPHttpServer.Create(nil);
  EchoTool := TAiEchoTool.Create('echo', 'Repite el mensaje recibido');
  TimeTool := TAiGetTimeTool.Create('get_time', 'Devuelve la hora actual del servidor');
  CalcTool := TAiCalcTool.Create('calc_add', 'Suma dos numeros (a + b)');
  try
    Server.Port       := SERVER_PORT;
    Server.ServerName := 'MakerAI Demo Server';

    // Registrar herramientas
    Server.RegisterTool('echo',     EchoTool);
    Server.RegisterTool('get_time', TimeTool);
    Server.RegisterTool('calc_add', CalcTool);

    Server.Start;

    WriteLn('Servidor MCP corriendo en: http://localhost:', SERVER_PORT, '/mcp');
    WriteLn;
    WriteLn('Ejemplos de prueba con curl:');
    WriteLn;
    WriteLn('  # Listar herramientas:');
    WriteLn('  curl -X POST http://localhost:', SERVER_PORT, '/mcp \');
    WriteLn('    -H "Content-Type: application/json" \');
    WriteLn('    -d ''{"jsonrpc":"2.0","method":"tools/list","id":1}''');
    WriteLn;
    WriteLn('  # Llamar echo:');
    WriteLn('  curl -X POST http://localhost:', SERVER_PORT, '/mcp \');
    WriteLn('    -H "Content-Type: application/json" \');
    WriteLn('    -d ''{"jsonrpc":"2.0","method":"tools/call","id":2,"params":{"name":"echo","arguments":{"message":"Hola!"}}}''');
    WriteLn;
    WriteLn('  # Llamar calc_add:');
    WriteLn('  curl -X POST http://localhost:', SERVER_PORT, '/mcp \');
    WriteLn('    -H "Content-Type: application/json" \');
    WriteLn('    -d ''{"jsonrpc":"2.0","method":"tools/call","id":3,"params":{"name":"calc_add","arguments":{"a":5,"b":3}}}''');
    WriteLn;
    WriteLn('Presiona ENTER para detener el servidor...');
    ReadLn(Line);

    Server.Stop;
    WriteLn('Servidor detenido.');

  finally
    // Tools son interfaces — liberadas automaticamente
    Server.Free;
  end;
end.
