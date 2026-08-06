unit uTool.Aprobacion;

// =============================================================================
// Tool MCP con patron MRTR (Multi-Round Tool Request, spec 2026-07-28)
// =============================================================================
// Lado SERVIDOR de la elicitation. El flujo tiene dos vueltas:
//
//   Vuelta 1: el tool NO ejecuta nada. Devuelve un InputRequiredResult
//             (resultType='input_required') con un ElicitRequest describiendo
//             que necesita del usuario, mas un requestState opaco.
//   Vuelta 2: el cliente repite tools/call agregando inputResponses (con la
//             respuesta del usuario) y el eco de requestState. Recien ahi el
//             tool valida y ejecuta.
//
// Es el mecanismo que permite que un tool pida confirmacion sin que el servidor
// tenga que mantener sesion: todo el estado viaja en el requestState.
//
// SEGURIDAD: requestState pasa por el cliente, asi que es entrada NO CONFIABLE.
// Aqui solo lleva el monto en Base64 con fines demostrativos. En produccion,
// si influye en autorizacion o en logica de negocio, debe ir firmado con
// HMAC/AEAD; de lo contrario el cliente puede alterarlo a voluntad.
// =============================================================================

interface

uses
  uMakerAi.MCPServer.Core,
  System.SysUtils, System.NetEncoding, System.JSON;

type
  TAprobacionParams = class
  private
    FMonto: string;
  public
    [AiMCPSchemaDescription('Monto de la transferencia a autorizar (ej: "1500 EUR")')]
    property Monto: string read FMonto write FMonto;
  end;

  TAprobacionTool = class(TAiMCPToolBase<TAprobacionParams>)
  private
    function ConstruirInputRequired(const AMonto: string): TJSONObject;
  protected
    function ExecuteWithParams(const AParams: TAprobacionParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

constructor TAprobacionTool.Create;
begin
  inherited;
  FName := 'autorizar_transferencia';
  FDescription := 'Ejecuta una transferencia solo despues de que el usuario la autorice via elicitation (MRTR)';
end;

function TAprobacionTool.ConstruirInputRequired(const AMonto: string): TJSONObject;
var
  Reqs, Elicit, ElicitParams, Schema, Props, PropConfirm: TJSONObject;
  Required: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.AddPair('resultType', 'input_required');

  Reqs := TJSONObject.Create;
  Result.AddPair('inputRequests', Reqs);

  // La clave ('autorizacion') es el identificador con el que el cliente debe
  // devolver su respuesta en inputResponses.
  Elicit := TJSONObject.Create;
  Reqs.AddPair('autorizacion', Elicit);
  Elicit.AddPair('method', 'elicitation/create');

  ElicitParams := TJSONObject.Create;
  Elicit.AddPair('params', ElicitParams);
  ElicitParams.AddPair('mode', 'form');
  ElicitParams.AddPair('message', 'Autoriza la transferencia de ' + AMonto + '?');

  // El schema le dice al cliente que forma tiene la respuesta esperada
  Schema := TJSONObject.Create;
  ElicitParams.AddPair('requestedSchema', Schema);
  Schema.AddPair('type', 'object');
  Props := TJSONObject.Create;
  Schema.AddPair('properties', Props);
  PropConfirm := TJSONObject.Create;
  Props.AddPair('confirmar', PropConfirm);
  PropConfirm.AddPair('type', 'boolean');
  Required := TJSONArray.Create;
  Required.Add('confirmar');
  Schema.AddPair('required', Required);

  Result.AddPair('requestState', TNetEncoding.Base64.Encode(AMonto));
end;

function TAprobacionTool.ExecuteWithParams(const AParams: TAprobacionParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  V: TJSONValue;
  Respuesta: TJSONObject;
  Accion, EstadoDecodificado: string;
begin
  if AParams.Monto.IsEmpty then
    Exit(TAiMCPResponseBuilder.New.AddText('Error: el parametro "Monto" es obligatorio').Build);

  // Es el reintento con la respuesta del usuario?
  if Assigned(AuthContext.InputResponses) then
  begin
    V := AuthContext.InputResponses.GetValue('autorizacion');
    if not(V is TJSONObject) then
      // Falta lo que se pidio: la spec dice volver a pedirlo, no fallar.
      Exit(ConstruirInputRequired(AParams.Monto));

    Respuesta := TJSONObject(V);
    Accion := Respuesta.GetValue<string>('action', '');

    // Verificar el eco del requestState. Es entrada no confiable: un valor
    // corrupto debe producir un rechazo limpio, nunca una excepcion interna.
    EstadoDecodificado := '';
    if AuthContext.RequestState <> '' then
      try
        EstadoDecodificado := TNetEncoding.Base64.Decode(AuthContext.RequestState);
      except
        EstadoDecodificado := '';
      end;
    if EstadoDecodificado <> AParams.Monto then
      Exit(TAiMCPResponseBuilder.New.AddText('Error: requestState invalido o ausente en el reintento').Build);

    if SameText(Accion, 'accept') then
      Exit(TAiMCPResponseBuilder.New.AddText('AUTORIZADA y ejecutada la transferencia de ' + AParams.Monto).Build)
    else
      Exit(TAiMCPResponseBuilder.New.AddText('RECHAZADA por el usuario: ' + AParams.Monto).Build);
  end;

  // Primera llamada: pedir autorizacion.
  Result := ConstruirInputRequired(AParams.Monto);
end;

end.
