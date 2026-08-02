unit uRegression.Fixtures;

// -----------------------------------------------------------------------------
// Fixtures de la suite de regresion de MakerAI.
//
// Todo corre in-process: no depende de demos compilados ni de servicios
// externos. Contiene:
//   - Tools MCP de prueba (echo determinista y confirm con patron MRTR).
//   - Un servidor MCP "solo legacy" (Indy) que responde -32601 a
//     server/discover, para validar el fallback dual-era del cliente.
//   - Handlers 'of object' para grafos de agentes y guardrails.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.NetEncoding,
  IdContext, IdCustomHTTPServer, IdHTTPServer,
  uMakerAi.MCPServer.Core, uMakerAi.Agents, uMakerAi.Tools.Functions,
  uMakerAi.Chat.Messages;

type
  // --- Tool MCP determinista: devuelve el texto en mayusculas ---
  TEchoParams = class
  private
    FText: string;
  public
    [AiMCPSchemaDescription('Texto a devolver en mayusculas')]
    property Text: string read FText write FText;
  end;

  TEchoTool = class(TAiMCPToolBase<TEchoParams>)
  protected
    function ExecuteWithParams(const AParams: TEchoParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool MCP con patron MRTR (spec 2026-07-28) ---
  // Primera llamada: input_required con elicitation. Reintento con
  // action=accept: ejecuta. Sirve para validar el loop de reintento del cliente.
  TConfirmParams = class
  private
    FOperation: string;
  public
    [AiMCPSchemaDescription('Operacion que requiere confirmacion')]
    property Operation: string read FOperation write FOperation;
  end;

  TConfirmTool = class(TAiMCPToolBase<TConfirmParams>)
  private
    function BuildInputRequired(const AOperation: string): TJSONObject;
  protected
    function ExecuteWithParams(const AParams: TConfirmParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Servidor MCP "solo legacy" ---
  // Responde -32601 a server/discover y atiende initialize/tools/list al estilo
  // pre-2026: el cliente dual-era debe detectarlo y caer al handshake.
  TLegacyOnlyMCPServer = class
  private
    FHttp: TIdHTTPServer;
    procedure HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
  end;

  // --- Handlers 'of object' para grafos y guardrails ---
  TFixtureHandlers = class
  public
    ToolExecuted: Boolean;   // marca si un tool bloqueado llego a ejecutarse
    BlockedFired: Boolean;   // marca si OnBlocked se disparo
    LastElicitMessage: string;

    // Nodo de grafo: encadena el nombre del nodo al input
    procedure NodeExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Tool local de TAiFunctions (para la prueba de integracion de guardrails)
    procedure ToolAction(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
    // Guardrails
    procedure GuardBlocked(Sender: TObject; const AToolName, AArguments, AReason: string);
    procedure GuardCheck(Sender: TObject; const AToolName, AArguments: string;
      var AAllow: Boolean; var AReason: string);
    // MRTR: responde la elicitation aceptando
    procedure InputRequired(Sender: TObject; const AToolName: string;
      AInputRequests, AInputResponses: TJSONObject; var AHandled: Boolean);
  end;

implementation

uses uMakerAi.MCPClient.Core;

{ TEchoTool }

constructor TEchoTool.Create;
begin
  inherited;
  FName := 'echo_upper';
  FDescription := 'Devuelve el texto recibido en mayusculas (determinista)';
end;

function TEchoTool.ExecuteWithParams(const AParams: TEchoParams; const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New.AddText(UpperCase(AParams.Text)).Build;
end;

{ TConfirmTool }

constructor TConfirmTool.Create;
begin
  inherited;
  FName := 'confirm_op';
  FDescription := 'Pide confirmacion via elicitation (patron MRTR) y ejecuta al aceptar';
end;

function TConfirmTool.BuildInputRequired(const AOperation: string): TJSONObject;
var
  Reqs, Elicit, ElicitParams, Schema, Props, ConfirmProp: TJSONObject;
  Required: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.AddPair('resultType', 'input_required');
  Reqs := TJSONObject.Create;
  Result.AddPair('inputRequests', Reqs);
  Elicit := TJSONObject.Create;
  Reqs.AddPair('user_confirmation', Elicit);
  Elicit.AddPair('method', 'elicitation/create');
  ElicitParams := TJSONObject.Create;
  Elicit.AddPair('params', ElicitParams);
  ElicitParams.AddPair('mode', 'form');
  ElicitParams.AddPair('message', 'Confirma la operacion: ' + AOperation);
  Schema := TJSONObject.Create;
  ElicitParams.AddPair('requestedSchema', Schema);
  Schema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Schema.AddPair('properties', Props);
  ConfirmProp := TJSONObject.Create;
  Props.AddPair('confirm', ConfirmProp);
  ConfirmProp.AddPair('type', 'boolean');
  Required := TJSONArray.Create;
  Required.Add('confirm');
  Schema.AddPair('required', Required);
  Result.AddPair('requestState', TNetEncoding.Base64.Encode(AOperation));
end;

function TConfirmTool.ExecuteWithParams(const AParams: TConfirmParams; const AuthContext: TAiAuthContext): TJSONObject;
var
  V: TJSONValue;
  Action, DecodedState: string;
begin
  if Assigned(AuthContext.InputResponses) then
  begin
    V := AuthContext.InputResponses.GetValue('user_confirmation');
    if not(V is TJSONObject) then
      Exit(BuildInputRequired(AParams.Operation)); // falta la respuesta: re-pedir

    Action := TJSONObject(V).GetValue<string>('action', '');
    DecodedState := '';
    if AuthContext.RequestState <> '' then
      try
        DecodedState := TNetEncoding.Base64.Decode(AuthContext.RequestState);
      except
        DecodedState := ''; // estado corrupto => rechazo limpio
      end;
    if DecodedState <> AParams.Operation then
      Exit(TAiMCPResponseBuilder.New.AddText('ESTADO_INVALIDO').Build);

    if SameText(Action, 'accept') then
      Exit(TAiMCPResponseBuilder.New.AddText('CONFIRMADO:' + AParams.Operation).Build)
    else
      Exit(TAiMCPResponseBuilder.New.AddText('CANCELADO:' + AParams.Operation).Build);
  end;
  Result := BuildInputRequired(AParams.Operation);
end;

{ TLegacyOnlyMCPServer }

constructor TLegacyOnlyMCPServer.Create(APort: Integer);
begin
  inherited Create;
  FHttp := TIdHTTPServer.Create(nil);
  FHttp.OnCommandGet := HttpCommand;
  FHttp.OnCommandOther := HttpCommand;
  FHttp.DefaultPort := APort;
  FHttp.Active := True;
end;

destructor TLegacyOnlyMCPServer.Destroy;
begin
  FHttp.Active := False;
  FHttp.Free;
  inherited;
end;

procedure TLegacyOnlyMCPServer.HttpCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  Body, Method, Out_: string;
  Root: TJSONValue;
  Req: TJSONObject;
  IdNum: Integer;
begin
  Body := '';
  if Assigned(ARequestInfo.PostStream) then
  begin
    ARequestInfo.PostStream.Position := 0;
    with TStringStream.Create('', TEncoding.UTF8) do
      try
        CopyFrom(ARequestInfo.PostStream, 0);
        Body := DataString;
      finally
        Free;
      end;
  end;

  Method := '';
  IdNum := 0;
  Root := TJSONObject.ParseJSONValue(Body);
  if Root is TJSONObject then
  begin
    Req := TJSONObject(Root);
    try
      Method := Req.GetValue<string>('method', '');
      IdNum := Req.GetValue<Integer>('id', 0);
    finally
      Req.Free;
    end;
  end
  else
    Root.Free;

  if SameText(Method, 'server/discover') then
    // Servidor legacy: no conoce el metodo moderno
    Out_ := Format('{"jsonrpc":"2.0","error":{"code":-32601,"message":"Method not found"},"id":%d}', [IdNum])
  else if SameText(Method, 'initialize') then
    Out_ := Format('{"jsonrpc":"2.0","id":%d,"result":{"protocolVersion":"2025-03-26",' +
      '"capabilities":{"tools":{}},"serverInfo":{"name":"LegacyOnly","version":"1.0"}}}', [IdNum])
  else if SameText(Method, 'tools/list') then
    Out_ := Format('{"jsonrpc":"2.0","id":%d,"result":{"tools":[{"name":"legacy_tool",' +
      '"description":"tool legacy","inputSchema":{"type":"object","properties":{}}}]}}', [IdNum])
  else
    Out_ := Format('{"jsonrpc":"2.0","error":{"code":-32601,"message":"Method not found"},"id":%d}', [IdNum]);

  AResponseInfo.ResponseNo := 200;
  AResponseInfo.ContentType := 'application/json; charset=utf-8';
  AResponseInfo.CharSet := 'utf-8';
  AResponseInfo.ContentText := Out_;
end;

{ TFixtureHandlers }

procedure TFixtureHandlers.NodeExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Input + '>' + Node.Name;
end;

procedure TFixtureHandlers.ToolAction(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolExecuted := True; // si esto corre, el guardrail no bloqueo
  ToolCall.Response := '{"ok":true}';
  Handled := True;
end;

procedure TFixtureHandlers.GuardBlocked(Sender: TObject; const AToolName, AArguments, AReason: string);
begin
  BlockedFired := True;
end;

procedure TFixtureHandlers.GuardCheck(Sender: TObject; const AToolName, AArguments: string;
  var AAllow: Boolean; var AReason: string);
begin
  if AArguments.ToLower.Contains('produccion') then
  begin
    AAllow := False;
    AReason := 'entorno de produccion protegido';
  end;
end;

procedure TFixtureHandlers.InputRequired(Sender: TObject; const AToolName: string;
  AInputRequests, AInputResponses: TJSONObject; var AHandled: Boolean);
var
  V: TJSONValue;
  Resp, Content: TJSONObject;
begin
  V := AInputRequests.GetValue('user_confirmation');
  if V is TJSONObject then
    LastElicitMessage := TJSONObject(V).GetValue<string>('params.message', '');

  Resp := TJSONObject.Create;
  Resp.AddPair('action', 'accept');
  Content := TJSONObject.Create;
  Content.AddPair('confirm', TJSONBool.Create(True));
  Resp.AddPair('content', Content);
  AInputResponses.AddPair('user_confirmation', Resp);
  AHandled := True;
end;

end.
