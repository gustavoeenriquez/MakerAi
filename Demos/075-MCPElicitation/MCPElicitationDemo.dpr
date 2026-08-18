program MCPElicitationDemo;

// =============================================================================
// DEMO 075 - Elicitation MCP (patron MRTR) desde el lado del CLIENTE
// =============================================================================
// El demo 031 muestra el lado servidor: un tool que pide confirmacion. Este
// muestra la otra mitad, que es la que hay que implementar cuando MakerAI
// CONSUME un servidor MCP: como responder a esa peticion.
//
// El contrato: cuando un tool devuelve resultType='input_required', el cliente
// dispara OnInputRequired con los inputRequests del servidor. El handler debe
// rellenar AInputResponses y poner AHandled := True; el cliente entonces repite
// la llamada automaticamente, ecoando el requestState. Si nadie atiende el
// evento, la llamada termina con el input_required sin resolver.
//
// Tres escenarios, todos in-process (servidor y cliente en el mismo exe), sin
// LLM ni API keys:
//
//   A. El usuario ACEPTA   -> el tool ejecuta la operacion.
//   B. El usuario RECHAZA  -> el tool NO ejecuta y lo reporta.
//   C. SIN handler         -> la llamada devuelve el input_required crudo; es
//                             el fallo tipico al integrar un servidor que usa
//                             elicitation, y conviene verlo explicito.
//
// Modos de uso:
//   MCPElicitationDemo.exe          -> los tres escenarios
//   MCPElicitationDemo.exe --otel   -> exporta trazas OTLP a localhost:4318
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  uMakerAi.Core,
  uMakerAi.MCPServer.Core,
  UMakerAi.MCPServer.Http,
  uMakerAi.MCPClient.Core,
  uMakerAi.Telemetry,
  uTool.Aprobacion in 'uTool.Aprobacion.pas';

const
  PORT = 8285;

type
  // Los eventos del framework son 'of object': sin lambdas.
  TDemoHandlers = class
  public
    Aceptar: Boolean;        // que responde el "usuario"
    MensajeRecibido: string; // la pregunta que mando el servidor
    Invocaciones: Integer;
    procedure InputRequired(Sender: TObject; const AToolName: string;
      AInputRequests, AInputResponses: TJSONObject; var AHandled: Boolean);
  end;

procedure TDemoHandlers.InputRequired(Sender: TObject; const AToolName: string;
  AInputRequests, AInputResponses: TJSONObject; var AHandled: Boolean);
var
  V: TJSONValue;
  Respuesta, Contenido: TJSONObject;
begin
  Inc(Invocaciones);

  // 1. Leer que pide el servidor. La clave ('autorizacion') la elige el tool;
  //    en un cliente real se recorren todas las de inputRequests.
  V := AInputRequests.GetValue('autorizacion');
  if V is TJSONObject then
    MensajeRecibido := TJSONObject(V).GetValue<string>('params.message', '');

  Writeln('      [OnInputRequired] tool="' + AToolName + '"');
  Writeln('      [OnInputRequired] pregunta: ' + MensajeRecibido);

  // 2. Aqui iria la interaccion real con el usuario (un dialogo, una tarjeta
  //    en el chat, una notificacion...). El demo responde de forma automatica.
  Respuesta := TJSONObject.Create;
  if Aceptar then
  begin
    Respuesta.AddPair('action', 'accept');
    Contenido := TJSONObject.Create;
    Contenido.AddPair('confirmar', TJSONBool.Create(True));
    Respuesta.AddPair('content', Contenido);
    Writeln('      [OnInputRequired] el usuario ACEPTA');
  end
  else
  begin
    // 'decline' = el usuario dijo que no. ('cancel' seria abortar el flujo.)
    Respuesta.AddPair('action', 'decline');
    Writeln('      [OnInputRequired] el usuario RECHAZA');
  end;

  // 3. La respuesta se devuelve bajo la MISMA clave que uso el servidor.
  AInputResponses.AddPair('autorizacion', Respuesta);

  // 4. Sin esto el cliente no reintenta y la llamada queda a medias.
  AHandled := True;
end;

// -----------------------------------------------------------------------------

function TextoDeRespuesta(ARes: TJSONObject): string;
var
  Arr: TJSONArray;
  V: TJSONValue;
  S: string;
begin
  Result := '';
  if not Assigned(ARes) then
    Exit('(sin respuesta)');
  // El builder del servidor devuelve {"content":[{"type":"text","text":"..."}]}
  if ARes.TryGetValue<TJSONArray>('content', Arr) then
    for V in Arr do
      if (V is TJSONObject) and TJSONObject(V).TryGetValue<string>('text', S) then
      begin
        if Result <> '' then
          Result := Result + ' ';
        Result := Result + S;
      end;
  if Result = '' then
    Result := ARes.ToJSON;
end;

procedure Escenario(const ATitulo: string; AServer: TAiMCPHttpServer;
  AHandlers: TDemoHandlers; AUsarHandler, AAceptar: Boolean);
var
  Client: TMCPClientHttp;
  Args, Res: TJSONObject;
  Media: TObjectList<TAiMediaFile>;
begin
  Writeln('');
  Writeln('   --- ' + ATitulo + ' ---');

  Client := TMCPClientHttp.Create(nil);
  Media := TObjectList<TAiMediaFile>.Create(True);
  try
    // El servidor de este demo publica el RPC en la raiz de su Endpoint
    Client.Params.Values['RpcEndpointSuffix'] := '';
    Client.Params.Values['InitializeEndpointSuffix'] := '';
    Client.Params.Values['NotificationEndpointSuffix'] := '';
    Client.Params.Values['Timeout'] := '15000';
    Client.Params.Values['URL'] := Format('http://localhost:%d%s', [PORT, AServer.Endpoint]);
    Client.Initialize;

    AHandlers.Aceptar := AAceptar;
    AHandlers.Invocaciones := 0;
    if AUsarHandler then
      Client.OnInputRequired := AHandlers.InputRequired;

    Args := TJSONObject.Create;
    Args.AddPair('monto', '1500 EUR');
    Res := Client.CallTool('autorizar_transferencia', Args, Media);
    try
      Writeln('   resultado: ' + TextoDeRespuesta(Res));
      Writeln(Format('   veces que se pidio input al usuario: %d', [AHandlers.Invocaciones]));
    finally
      Res.Free;
    end;
  finally
    Media.Free;
    Client.Free;
  end;
end;

// -----------------------------------------------------------------------------

function HasFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
      Exit(True);
end;

var
  Server: TAiMCPHttpServer;
  Handlers: TDemoHandlers;
  Telemetry: TAiTelemetry;

begin
  Telemetry := nil;
  try
    if HasFlag('--otel') then
    begin
      Telemetry := TAiTelemetry.Create(nil);
      Telemetry.ServiceName := 'MCPElicitationDemo';
      Telemetry.Enabled := True;
      Writeln('[otel] Exportando trazas a ' + Telemetry.Endpoint);
    end;

    Writeln('=== DEMO 075: elicitation MCP (MRTR) desde el cliente ===');
    Writeln('');
    Writeln('== Servidor MCP in-process con un tool que exige autorizacion ==');

    Server := TAiMCPHttpServer.Create(nil);
    Handlers := TDemoHandlers.Create;
    try
      Server.Port := PORT;
      Server.RegisterTool('autorizar_transferencia',
        function: IAiMCPTool
        begin
          Result := TAprobacionTool.Create;
        end);
      Server.Start;
      Writeln(Format('   escuchando en http://localhost:%d%s', [PORT, Server.Endpoint]));

      Escenario('A. El usuario ACEPTA', Server, Handlers, True, True);
      Escenario('B. El usuario RECHAZA', Server, Handlers, True, False);
      Escenario('C. SIN handler de OnInputRequired', Server, Handlers, False, False);

      Writeln('');
      Writeln('   En C el tool nunca llega a ejecutarse: la respuesta trae el');
      Writeln('   input_required sin resolver. Si un servidor MCP usa elicitation');
      Writeln('   y el cliente no atiende OnInputRequired, esto es lo que se ve.');

      Server.Stop;
    finally
      Handlers.Free;
      Server.Free;
    end;

    Writeln('');
    Writeln('OK: los tres escenarios de elicitation completaron.');

    if Assigned(Telemetry) then
    begin
      Telemetry.Flush;
      Telemetry.Free;
    end;
    ExitCode := 0;
  except
    on E: Exception do
    begin
      Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
