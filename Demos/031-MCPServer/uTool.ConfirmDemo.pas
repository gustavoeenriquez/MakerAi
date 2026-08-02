unit uTool.ConfirmDemo;

interface

uses
  uMakerAi.MCPServer.Core,
  System.SysUtils, System.NetEncoding,
  System.JSON;

type
  // =============================================================================
  // HERRAMIENTA DEMO DEL PATRON MRTR (MCP spec 2026-07-28)
  // =============================================================================
  // Primera llamada: el tool no ejecuta nada y devuelve un InputRequiredResult
  // (resultType='input_required') con un ElicitRequest pidiendo confirmacion al
  // usuario, mas un requestState opaco.
  // Reintento: el cliente repite tools/call agregando inputResponses (con el
  // ElicitResult) y el eco de requestState; el tool valida y ejecuta.
  //
  // NOTA de seguridad: requestState viaja por el cliente y es entrada NO
  // confiable. En produccion debe protegerse con HMAC/AEAD si influye en
  // autorizacion o logica de negocio. Aqui solo transporta el mensaje en
  // Base64 con fines demostrativos (una alteracion solo hace fallar el request).

  TConfirmDemoParams = class
  private
    FOperation: string;
  public
    [AiMCPSchemaDescription('Operacion que requiere confirmacion del usuario (ej: "borrar archivo temp.txt")')]
    property Operation: string read FOperation write FOperation;
  end;

  TConfirmDemoTool = class(TAiMCPToolBase<TConfirmDemoParams>)
  private
    function BuildInputRequired(const AOperation: string): TJSONObject;
  protected
    function ExecuteWithParams(const AParams: TConfirmDemoParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

procedure RegisterConfirmDemoTool(ALogicServer: TAiMCPServer);

implementation

{ TConfirmDemoTool }

constructor TConfirmDemoTool.Create;
begin
  inherited;
  FName := 'confirm_demo';
  FDescription := 'Demo MRTR: ejecuta una operacion solo despues de que el usuario la confirme via elicitation (spec 2026-07-28)';
end;

// Construye el InputRequiredResult con un ElicitRequest 'user_confirmation'.
function TConfirmDemoTool.BuildInputRequired(const AOperation: string): TJSONObject;
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

  // Estado opaco que el cliente debe ecoar en el reintento (demo: Base64).
  Result.AddPair('requestState', TNetEncoding.Base64.Encode(AOperation));
end;

function TConfirmDemoTool.ExecuteWithParams(const AParams: TConfirmDemoParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  V: TJSONValue;
  ElicitResult: TJSONObject;
  Action, DecodedState: string;
begin
  if AParams.Operation.IsEmpty then
    Exit(TAiMCPResponseBuilder.New.AddText('Error: el parametro "Operation" es obligatorio').Build);

  // Es un reintento MRTR con la respuesta de la elicitation?
  if Assigned(AuthContext.InputResponses) then
  begin
    V := AuthContext.InputResponses.GetValue('user_confirmation');
    if not(V is TJSONObject) then
      // Falta la respuesta pedida: la spec dice re-pedir, no fallar.
      Exit(BuildInputRequired(AParams.Operation));

    ElicitResult := TJSONObject(V);
    Action := ElicitResult.GetValue<string>('action', '');

    // Verificar el eco del requestState (demo de round-trip integro).
    // requestState es entrada no confiable: un valor corrupto solo debe
    // producir un rechazo limpio, nunca una excepcion interna.
    DecodedState := '';
    if AuthContext.RequestState <> '' then
      try
        DecodedState := TNetEncoding.Base64.Decode(AuthContext.RequestState);
      except
        DecodedState := '';
      end;
    if DecodedState <> AParams.Operation then
      Exit(TAiMCPResponseBuilder.New.AddText('Error: requestState invalido o ausente en el reintento').Build);

    if SameText(Action, 'accept') then
      Exit(TAiMCPResponseBuilder.New.AddText('CONFIRMADO y ejecutado: ' + AParams.Operation).Build)
    else
      Exit(TAiMCPResponseBuilder.New.AddText('CANCELADO por el usuario: ' + AParams.Operation).Build);
  end;

  // Primera llamada: pedir confirmacion via MRTR.
  Result := BuildInputRequired(AParams.Operation);
end;

procedure RegisterConfirmDemoTool(ALogicServer: TAiMCPServer);
begin
  if not Assigned(ALogicServer) then
    raise Exception.Create('LogicServer no puede ser nulo para registrar herramientas.');

  ALogicServer.RegisterTool('confirm_demo',
    function: IAiMCPTool
    begin
      Result := TConfirmDemoTool.Create;
    end);
end;

end.
