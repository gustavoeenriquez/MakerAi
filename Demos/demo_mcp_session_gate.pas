program demo_mcp_session_gate;
{$mode objfpc}{$H+}

// Demo/test del gate de sesion MCP sobre HTTP (ISSUE #110).
//
// El gating es 100% opt-in: solo se activa cuando se asigna OnClientConnect.
// Con el handler puesto:
//   - 'initialize' pasa por el vetting del cliente y, si se acepta, emite un
//     Mcp-Session-Id que devuelve en la cabecera de la respuesta.
//   - tools/, resources/ y prompts/ exigen ese Mcp-Session-Id.
//   - initialize/ping siguen sin requerir sesion (initialize es quien la crea).
//
// El demo levanta el servidor, se conecta a si mismo con TFPHTTPClient y
// verifica los 6 casos. Termina con exit code 0 si todos pasan, 1 si falla alguno.
//
// Compilar con:
//   fpc demo_mcp_session_gate.pas -Fu../Source/Core -Fu../Source/MCPServer -Fu../Source/Tools

uses
  uDemoHelper,
  SysUtils, Classes,
  fphttpclient, fpjson, jsonparser,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Http;

const
  SERVER_PORT   = 8099;
  BLOCKED_CLIENT = 'cliente-bloqueado';

type
  // -------------------------------------------------------------------------
  // Herramienta trivial para tener algo que listar
  // -------------------------------------------------------------------------
  TAiPingTool = class(TAiMCPToolBase)
  public
    function GetInputSchema: TJSONObject; override;
    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject; override;
  end;

  // -------------------------------------------------------------------------
  // Portador de los handlers (los eventos son metodos de objeto)
  // -------------------------------------------------------------------------
  TGatePolicy = class
  public
    procedure ClientConnect(Sender: TObject;
        const AClientName, AClientVersion, AProtocolVersion: string;
        var AAllow: Boolean; var AReason: string);
  end;

var
  Failures: Integer = 0;

function TAiPingTool.GetInputSchema: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('type', TJSONString.Create('object'));
  Result.Add('properties', TJSONObject.Create);
end;

function TAiPingTool.Execute(const Arguments: TJSONObject;
    const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New.AddText('pong').Build;
end;

procedure TGatePolicy.ClientConnect(Sender: TObject;
    const AClientName, AClientVersion, AProtocolVersion: string;
    var AAllow: Boolean; var AReason: string);
begin
  WriteLn('  [OnClientConnect] name="', AClientName, '" version="', AClientVersion,
      '" protocol="', AProtocolVersion, '"');
  if AClientName = BLOCKED_CLIENT then
  begin
    AAllow  := False;
    AReason := 'Cliente no autorizado por politica del servidor';
  end;
end;

// ---------------------------------------------------------------------------
// Helpers de prueba
// ---------------------------------------------------------------------------

// POST al endpoint MCP. ASessionID vacio = no mandar cabecera.
// Devuelve el body; AOutSession recibe el Mcp-Session-Id de la respuesta.
function PostMcp(const ABody, ASessionID: string; out AOutSession: string): string;
var
  Client : TFPHTTPClient;
  Req    : TStringStream;
  Resp   : TStringStream;
begin
  AOutSession := '';
  Result      := '';
  Client := TFPHTTPClient.Create(nil);
  Req    := TStringStream.Create(ABody);
  Resp   := TStringStream.Create('');
  try
    Client.AddHeader('Content-Type', 'application/json');
    if ASessionID <> '' then
      Client.AddHeader('Mcp-Session-Id', ASessionID);
    Client.RequestBody := Req;
    try
      Client.HTTPMethod('POST', 'http://127.0.0.1:' + IntToStr(SERVER_PORT) + '/mcp',
          Resp, [200, 204]);
      Result      := Resp.DataString;
      AOutSession := Client.GetHeader(Client.ResponseHeaders, 'Mcp-Session-Id');
    except
      on E: Exception do
        Result := '{"transport_error":"' + E.Message + '"}';
    end;
  finally
    Resp.Free;
    Req.Free;
    Client.Free;
  end;
end;

// Extrae error.code de una respuesta JSON-RPC. Devuelve 0 si no hay error.
function ErrorCode(const AJson: string): Integer;
var
  jData : TJSONData;
  jErr  : TJSONData;
begin
  Result := 0;
  if AJson = '' then Exit;
  try
    jData := GetJSON(AJson);
  except
    Exit;
  end;
  if not Assigned(jData) then Exit;
  try
    if jData.JSONType <> jtObject then Exit;
    jErr := TJSONObject(jData).Find('error');
    if Assigned(jErr) and (jErr.JSONType = jtObject) then
      Result := TJSONObject(jErr).Get('code', 0);
  finally
    jData.Free;
  end;
end;

function HasResult(const AJson: string): Boolean;
var
  jData: TJSONData;
begin
  Result := False;
  if AJson = '' then Exit;
  try
    jData := GetJSON(AJson);
  except
    Exit;
  end;
  if not Assigned(jData) then Exit;
  try
    Result := (jData.JSONType = jtObject) and
              Assigned(TJSONObject(jData).Find('result'));
  finally
    jData.Free;
  end;
end;

procedure Check(const ADescription: string; ACondition: Boolean;
    const ADetail: string);
begin
  if ACondition then
    WriteLn('  [OK]   ', ADescription)
  else
  begin
    WriteLn('  [FAIL] ', ADescription);
    WriteLn('         ', ADetail);
    Inc(Failures);
  end;
end;

function InitializeBody(const AClientName: string): string;
begin
  Result := '{"jsonrpc":"2.0","method":"initialize","id":1,"params":' +
            '{"protocolVersion":"2025-06-18","clientInfo":{"name":"' +
            AClientName + '","version":"1.0"}}}';
end;

// ---------------------------------------------------------------------------
// Programa
// ---------------------------------------------------------------------------
var
  Server   : TAiMCPHttpServer;
  PingTool : TAiPingTool;
  Policy   : TGatePolicy;
  Resp     : string;
  Session  : string;
  Issued   : string;

begin
  WriteLn('=== MakerAI FPC - Gate de sesion MCP (ISSUE #110) ===');
  WriteLn;

  Server   := TAiMCPHttpServer.Create(nil);
  PingTool := TAiPingTool.Create('ping_tool', 'Devuelve pong');
  Policy   := TGatePolicy.Create;
  try
    Server.Port       := SERVER_PORT;
    Server.ServerName := 'MakerAI Session Gate Test';
    Server.RegisterTool('ping_tool', PingTool);

    // Asignar el handler es lo que activa el gating.
    Server.OnClientConnect := @Policy.ClientConnect;

    Server.Start;
    Sleep(500);
    WriteLn('Servidor en http://127.0.0.1:', SERVER_PORT, '/mcp  (gating ACTIVO)');
    WriteLn;

    // -- 1. tools/list sin sesion -> rechazado -------------------------------
    WriteLn('1) tools/list sin Mcp-Session-Id');
    Resp := PostMcp('{"jsonrpc":"2.0","method":"tools/list","id":10}', '', Issued);
    Check('rechazado con -32001', ErrorCode(Resp) = -32001, Resp);

    // -- 2. ping sin sesion -> permitido -------------------------------------
    WriteLn('2) ping sin Mcp-Session-Id (no requiere sesion)');
    Resp := PostMcp('{"jsonrpc":"2.0","method":"ping","id":11}', '', Issued);
    Check('respondido sin error', HasResult(Resp), Resp);

    // -- 3. initialize de un cliente permitido -> emite sesion ---------------
    WriteLn('3) initialize de un cliente permitido');
    Resp := PostMcp(InitializeBody('cliente-ok'), '', Session);
    Check('initialize aceptado', HasResult(Resp), Resp);
    Check('devuelve cabecera Mcp-Session-Id', Session <> '',
        'cabecera vacia');

    // -- 4. tools/list con la sesion emitida -> permitido --------------------
    WriteLn('4) tools/list con la sesion emitida');
    Resp := PostMcp('{"jsonrpc":"2.0","method":"tools/list","id":12}', Session, Issued);
    Check('permitido', HasResult(Resp), Resp);

    // -- 5. tools/list con una sesion inventada -> rechazado -----------------
    WriteLn('5) tools/list con una sesion invalida');
    Resp := PostMcp('{"jsonrpc":"2.0","method":"tools/list","id":13}',
        'sesion-que-no-existe', Issued);
    Check('rechazado con -32001', ErrorCode(Resp) = -32001, Resp);

    // -- 6. initialize de un cliente bloqueado -> rechazado ------------------
    WriteLn('6) initialize del cliente bloqueado por la politica');
    Resp := PostMcp(InitializeBody(BLOCKED_CLIENT), '', Issued);
    Check('rechazado con -32001', ErrorCode(Resp) = -32001, Resp);
    Check('no emite sesion', Issued = '', 'emitio: ' + Issued);

    WriteLn;
    if Failures = 0 then
      WriteLn('Todas las comprobaciones pasaron.')
    else
      WriteLn(Failures, ' comprobacion(es) fallaron.');

    Server.Stop;
  finally
    Policy.Free;
    Server.Free;
  end;

  if Failures > 0 then
    Halt(1);
end.
