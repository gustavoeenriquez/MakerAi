program demo_mcp_server_stdio;
{$mode objfpc}{$H+}

// Demo: TAiMCPStdioServer — servidor MCP sobre stdin/stdout
//
// Este ejecutable actua como servidor MCP usando el transporte StdIO.
// Es lanzado como subproceso por TMCPClientStdIo (demo_mcp_client_stdio).
//
// Protocolo: Lee peticiones JSON-RPC de stdin (una por linea)
//            y escribe respuestas JSON-RPC en stdout (una por linea).
//
// IMPORTANTE: No escribir nada en stdout excepto respuestas JSON-RPC.
//             Los mensajes de debug deben ir a stderr.
//
// Compilar con:
//   fpc demo_mcp_server_stdio.pas -Fu../Source/Core -Fu../Source/MCPServer

uses
  SysUtils, Classes,
  fpjson,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Stdio;

// ---------------------------------------------------------------------------
// Herramientas de demo
// ---------------------------------------------------------------------------

type
  { TAiEchoTool: repite el mensaje recibido }
  TAiEchoTool = class(TAiMCPToolBase)
  public
    function GetInputSchema: TJSONObject; override;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; override;
  end;

  { TAiGetTimeTool: devuelve la hora actual }
  TAiGetTimeTool = class(TAiMCPToolBase)
  public
    function GetInputSchema: TJSONObject; override;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; override;
  end;

  { TAiCalcAddTool: suma dos numeros }
  TAiCalcAddTool = class(TAiMCPToolBase)
  public
    function GetInputSchema: TJSONObject; override;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; override;
  end;

{ TAiEchoTool }

function TAiEchoTool.GetInputSchema: TJSONObject;
var
  Props: TJSONObject;
  MsgProp: TJSONObject;
  Required: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.Add('type', TJSONString.Create('object'));

  Props := TJSONObject.Create;
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
    Msg := Arguments.Get('message', '')
  else
    Msg := '';

  Result := TAiMCPResponseBuilder.New
    .AddText('Echo: ' + Msg)
    .Build;
end;

{ TAiGetTimeTool }

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
    .AddText('Hora actual: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
    .Build;
end;

{ TAiCalcAddTool }

function TAiCalcAddTool.GetInputSchema: TJSONObject;
var
  Props: TJSONObject;
  AProp, BProp: TJSONObject;
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

function TAiCalcAddTool.Execute(const Arguments: TJSONObject;
    const AuthContext: TAiAuthContext): TJSONObject;
var
  A, B: Double;
  Sum: Double;
begin
  A := 0; B := 0;
  if Assigned(Arguments) then
  begin
    A := Arguments.Get('a', Double(0.0));
    B := Arguments.Get('b', Double(0.0));
  end;
  Sum := A + B;

  Result := TAiMCPResponseBuilder.New
    .AddText(FormatFloat('0.##', A) + ' + ' + FormatFloat('0.##', B) +
             ' = ' + FormatFloat('0.##', Sum))
    .Build;
end;

// ---------------------------------------------------------------------------
// Programa principal
// ---------------------------------------------------------------------------

var
  Server: TAiMCPStdioServer;

begin
  // En modo StdIO, debug va a stderr para no contaminar el protocolo
  WriteLn(StdErr, '[StdIO Server] Iniciando MakerAI MCP StdIO Server...');

  Server := TAiMCPStdioServer.Create(nil);
  try
    Server.ServerName := 'MakerAI StdIO Demo Server';

    // Registrar herramientas
    Server.RegisterTool('echo',
        TAiEchoTool.Create('echo', 'Repite el mensaje recibido'));
    Server.RegisterTool('get_time',
        TAiGetTimeTool.Create('get_time', 'Devuelve la hora actual del servidor'));
    Server.RegisterTool('calc_add',
        TAiCalcAddTool.Create('calc_add', 'Suma dos numeros (a + b)'));

    Server.Start;

    WriteLn(StdErr, '[StdIO Server] Listo. Esperando peticiones en stdin...');

    // El servidor lee stdin en su hilo interno (TStdioWorkerThread).
    // El proceso padre (TMCPClientStdIo) nos terminara cuando haya terminado.
    while Server.IsActive do
      Sleep(200);

  finally
    Server.Stop;
    Server.Free;
  end;

  WriteLn(StdErr, '[StdIO Server] Terminado.');
end.
