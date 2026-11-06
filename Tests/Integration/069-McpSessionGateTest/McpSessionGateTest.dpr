program McpSessionGateTest;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// Test runtime del ISSUE #110: vetting de cliente MCP (OnClientConnect) + gate
// de sesion HTTP por Mcp-Session-Id (Parte A + Parte B).
//
// Arranca un TAiMCPHttpServer real (Indy) con una tool 'echo' y lo prueba como
// cliente HTTP (THTTPClient). Valida 4 escenarios con asserts automaticos:
//   FASE 1  gating OFF (sin OnClientConnect) -> tools/call funciona sin sesion.
//   FASE 2  gating ON  -> tools/call sin sesion = -32001; initialize emite
//           Mcp-Session-Id; tools/call con esa sesion = OK; sesion invalida = -32001.
//   FASE 3  OnUnauthorizedRequest concede acceso (AAllow:=True) -> tool ejecuta.
//   FASE 4  OnClientConnect rechaza (AAllow:=False) -> initialize = error, sin sesion.
//
// 100% local, no requiere API keys. Codigo de salida 0 si todo pasa.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,
  System.Net.URLClient,
  UMakerAi.MCPServer.Http,
  uMakerAi.MCPServer.Core;

const
  TEST_PORT = 8765;
  BASE_URL  = 'http://localhost:8765/mcp';
  ECHO_MARK = 'ECHO_OK'; // marcador fijo: prueba que el tool corrio sin depender del param

type
  // Parametros del tool echo (deserializados desde 'arguments' por RTTI)
  TEchoParams = class
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TEchoTool = class(TAiMCPToolBase<TEchoParams>)
  protected
    function ExecuteWithParams(const AParams: TEchoParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // Aloja los handlers de eventos (of object) y contadores observables.
  THarness = class
  public
    ConnectFired: Boolean;
    LastClientName: string;
    RejectClient: string; // si clientInfo.name coincide (case-insensitive) -> rechaza
    UnauthFired: Boolean;
    AllowUnauth: Boolean; // valor que el handler pone en AAllow
    procedure HandleClientConnect(Sender: TObject; const AClientName, AClientVersion, AProtocolVersion: string;
      var AAllow: Boolean; var AReason: string);
    procedure HandleUnauthorized(Sender: TObject; const ASessionID, AMethod: string;
      var AAllow: Boolean; var AReason: string);
  end;

var
  GPass, GFail: Integer;

{ TEchoTool }

constructor TEchoTool.Create;
begin
  inherited;
  FName := 'echo';
  FDescription := 'Devuelve el texto recibido (con marcador fijo de ejecucion)';
end;

function TEchoTool.ExecuteWithParams(const AParams: TEchoParams; const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New.AddText(ECHO_MARK + ':' + AParams.Text).Build;
end;

{ THarness }

procedure THarness.HandleClientConnect(Sender: TObject; const AClientName, AClientVersion, AProtocolVersion: string;
  var AAllow: Boolean; var AReason: string);
begin
  ConnectFired := True;
  LastClientName := AClientName;
  if (RejectClient <> '') and SameText(AClientName, RejectClient) then
  begin
    AAllow := False;
    AReason := 'Client ' + AClientName + ' is not allowed by server policy';
  end;
end;

procedure THarness.HandleUnauthorized(Sender: TObject; const ASessionID, AMethod: string;
  var AAllow: Boolean; var AReason: string);
begin
  UnauthFired := True;
  AAllow := AllowUnauth;
end;

{ Helpers }

procedure Check(ACond: Boolean; const ALabel: string);
begin
  if ACond then
  begin
    Inc(GPass);
    WriteLn('  [PASS] ' + ALabel);
  end
  else
  begin
    Inc(GFail);
    WriteLn('  [FAIL] ' + ALabel);
  end;
end;

function GetRespHeader(const Resp: IHTTPResponse; const AName: string): string;
var
  H: TNameValuePair;
begin
  Result := '';
  for H in Resp.Headers do
    if SameText(H.Name, AName) then
      Exit(H.Value);
end;

function DoPost(const ABody, ASessionId: string; out AStatus: Integer; out ARespBody, ARespSession: string): Boolean;
var
  C: THTTPClient;
  SS: TStringStream;
  Resp: IHTTPResponse;
  Headers: TNetHeaders;
begin
  Result := False;
  AStatus := 0;
  ARespBody := '';
  ARespSession := '';
  C := THTTPClient.Create;
  SS := TStringStream.Create(ABody, TEncoding.UTF8);
  try
    try
      if ASessionId <> '' then
        Headers := [TNameValuePair.Create('Content-Type', 'application/json'),
          TNameValuePair.Create('Mcp-Session-Id', ASessionId)]
      else
        Headers := [TNameValuePair.Create('Content-Type', 'application/json')];
      Resp := C.Post(BASE_URL, SS, nil, Headers);
      AStatus := Resp.StatusCode;
      ARespBody := Resp.ContentAsString(TEncoding.UTF8);
      ARespSession := GetRespHeader(Resp, 'Mcp-Session-Id');
      Result := True;
    except
      on E: Exception do
        WriteLn('  [HTTP ERR] ' + E.Message);
    end;
  finally
    SS.Free;
    C.Free;
  end;
end;

function InitBody(const AClientName: string): string;
begin
  Result := Format('{"jsonrpc":"2.0","id":1,"method":"initialize","params":' +
    '{"protocolVersion":"2025-06-18","clientInfo":{"name":"%s","version":"1.0"}}}', [AClientName]);
end;

function CallEchoBody: string;
begin
  Result := '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":' +
    '{"name":"echo","arguments":{"text":"hola"}}}';
end;

var
  Server: TAiMCPHttpServer;
  H: THarness;
  st: Integer;
  body, sess, sid: string;

begin
  GPass := 0;
  GFail := 0;
  try
    H := THarness.Create;
    Server := TAiMCPHttpServer.Create(nil);
    try
      Server.Port := TEST_PORT;
      Server.CorsEnabled := True;
      Server.RegisterTool('echo',
        function: IAiMCPTool
        begin
          Result := TEchoTool.Create;
        end);
      Server.Start;
      WriteLn('=== ISSUE #110: MCP OnClientConnect + gate de sesion ===');
      WriteLn('Servidor MCP HTTP escuchando en ' + BASE_URL);
      WriteLn('');

      // ===================== FASE 1: gating OFF =====================
      WriteLn('== FASE 1: gating OFF (sin OnClientConnect) ==');
      if DoPost(CallEchoBody, '', st, body, sess) then
      begin
        Check(Pos(ECHO_MARK, body) > 0, 'tools/call sin sesion ejecuta el tool (backward compat)');
        Check(Pos('-32001', body) = 0, 'no hay error de autorizacion');
      end;
      WriteLn('');

      // ===================== FASE 2: gating ON =====================
      WriteLn('== FASE 2: gating ON (OnClientConnect asignado) ==');
      Server.OnClientConnect := H.HandleClientConnect;
      Server.OnUnauthorizedRequest := H.HandleUnauthorized;
      H.AllowUnauth := False;

      // 2a. tools/call sin sesion -> rechazado
      H.UnauthFired := False;
      if DoPost(CallEchoBody, '', st, body, sess) then
      begin
        Check(Pos('-32001', body) > 0, '2a. tools/call sin sesion -> error -32001');
        Check(H.UnauthFired, '2a. OnUnauthorizedRequest se disparo');
        Check(Pos(ECHO_MARK, body) = 0, '2a. el tool NO se ejecuto');
      end;

      // 2b. initialize -> emite Mcp-Session-Id
      H.ConnectFired := False;
      H.LastClientName := '';
      sid := '';
      if DoPost(InitBody('GoodClient'), '', st, body, sess) then
      begin
        Check(st = 200, '2b. initialize responde HTTP 200');
        Check(H.ConnectFired, '2b. OnClientConnect se disparo');
        Check(H.LastClientName = 'GoodClient', '2b. recibio clientInfo.name = GoodClient');
        Check(sess <> '', '2b. respuesta trae cabecera Mcp-Session-Id');
        sid := sess;
      end;

      // 2c. tools/call con la sesion emitida -> permitido
      if DoPost(CallEchoBody, sid, st, body, sess) then
      begin
        Check(Pos(ECHO_MARK, body) > 0, '2c. tools/call con Mcp-Session-Id valido ejecuta el tool');
        Check(Pos('-32001', body) = 0, '2c. no hay error de autorizacion');
      end;

      // 2d. tools/call con sesion invalida -> rechazado
      H.UnauthFired := False;
      if DoPost(CallEchoBody, 'sesion-invalida-xyz', st, body, sess) then
      begin
        Check(Pos('-32001', body) > 0, '2d. tools/call con sesion invalida -> -32001');
        Check(H.UnauthFired, '2d. OnUnauthorizedRequest se disparo (sesion invalida)');
      end;
      WriteLn('');

      // ============ FASE 3: el evento concede acceso ============
      WriteLn('== FASE 3: OnUnauthorizedRequest concede acceso (AAllow:=True) ==');
      H.AllowUnauth := True;
      H.UnauthFired := False;
      if DoPost(CallEchoBody, '', st, body, sess) then
      begin
        Check(H.UnauthFired, '3. OnUnauthorizedRequest se disparo');
        Check(Pos(ECHO_MARK, body) > 0, '3. el handler permitio la llamada -> tool ejecutado');
      end;
      H.AllowUnauth := False;
      WriteLn('');

      // ============ FASE 4: OnClientConnect rechaza ============
      WriteLn('== FASE 4: OnClientConnect rechaza el cliente (AAllow:=False) ==');
      H.RejectClient := 'EvilClient';
      H.ConnectFired := False;
      if DoPost(InitBody('EvilClient'), '', st, body, sess) then
      begin
        Check(H.ConnectFired, '4. OnClientConnect se disparo');
        Check((Pos('"error"', body) > 0) and (Pos('"result"', body) = 0),
          '4. initialize rechazado -> error JSON-RPC, sin result');
        Check(sess = '', '4. no se emite Mcp-Session-Id al cliente rechazado');
      end;
      H.RejectClient := '';
      WriteLn('');

      // ===================== Resumen =====================
      WriteLn('========================================');
      WriteLn(Format('  PASS: %d    FAIL: %d', [GPass, GFail]));
      if GFail = 0 then
        WriteLn('  RESULTADO: OK - ISSUE #110 validado')
      else
        WriteLn('  RESULTADO: FALLOS detectados');
      WriteLn('========================================');

      Server.Stop;
    finally
      Server.Free;
      H.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('FATAL: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 2;
    end;
  end;

  if GFail > 0 then
    ExitCode := 1;
end.
