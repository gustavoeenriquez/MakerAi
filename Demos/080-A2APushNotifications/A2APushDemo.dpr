program A2APushDemo;

// =============================================================================
// DEMO 080 - Push notifications de A2A: el agente avisa a tu webhook
// =============================================================================
// El streaming SSE sirve cuando el cliente puede quedarse conectado esperando.
// Las push notifications son para lo contrario: tareas largas en las que el
// cliente se desconecta y quiere que le avisen cuando terminen. El agente hace
// un POST a la URL que le registres.
//
// Todo ocurre en un solo proceso y sin claves de API:
//   - un agente A2A (grafo con un nodo lento) en el puerto 8282
//   - un receptor de webhook propio en el 8283, para ver lo que llega
//
// Recorrido:
//   1. Config por el CRUD: CreateTaskPushNotificationConfig sobre un task ya
//      creado, y luego Get / List / Delete.
//   2. Config EN la propia peticion: configuration.taskPushNotificationConfig
//      dentro de SendMessage, que evita la segunda llamada.
//   3. Se comprueba lo que recibe el webhook: cabecera Authorization y payload.
//
// Uso:
//   A2APushDemo.exe [--port 8282] [--hook-port 8283]
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.SyncObjs,
  System.Generics.Collections,
  System.Net.HttpClient,
  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer,
  uMakerAi.Agents,
  uMakerAi.A2A.Server in '..\..\Source\Agents\uMakerAi.A2A.Server.pas',
  uMakerAi.A2A.Client in '..\..\Source\Agents\uMakerAi.A2A.Client.pas';

type
  // Lo que recibio el webhook, para poder enseñarlo
  TAviso = record
    Auth: string;
    Cuerpo: string;
  end;

  // Receptor de webhook: un HTTP server minimo que apunta lo que le llega.
  TReceptorWebhook = class
  private
    FHttp: TIdHTTPServer;
    FLock: TCriticalSection;
    FAvisos: TList<TAviso>;
    procedure OnPost(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
    function Cuantos: Integer;
    function Ultimo: TAviso;
    procedure Limpiar;
    // Espera a que llegue algo, hasta ATimeoutMs. Devuelve False si no llego.
    function EsperarAviso(ATimeoutMs: Integer): Boolean;
  end;

  THandlers = class
  public
    procedure NodoLento(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Fabrica(Sender: TObject; var AManager: TAIAgentManager);
  end;

  { TReceptorWebhook }

constructor TReceptorWebhook.Create(APort: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FAvisos := TList<TAviso>.Create;
  FHttp := TIdHTTPServer.Create(nil);
  FHttp.DefaultPort := APort;
  FHttp.OnCommandGet := OnPost;
  FHttp.OnCommandOther := OnPost;
  FHttp.Active := True;
end;

destructor TReceptorWebhook.Destroy;
begin
  FHttp.Active := False;
  FHttp.Free;
  FAvisos.Free;
  FLock.Free;
  inherited;
end;

procedure TReceptorWebhook.OnPost(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  Aviso: TAviso;
  SS: TStringStream;
begin
  Aviso.Auth := ARequestInfo.RawHeaders.Values['Authorization'];
  Aviso.Cuerpo := '';
  if Assigned(ARequestInfo.PostStream) then
  begin
    ARequestInfo.PostStream.Position := 0;
    SS := TStringStream.Create('', TEncoding.UTF8);
    try
      SS.CopyFrom(ARequestInfo.PostStream, 0);
      Aviso.Cuerpo := SS.DataString;
    finally
      SS.Free;
    end;
  end;

  FLock.Enter;
  try
    FAvisos.Add(Aviso);
  finally
    FLock.Leave;
  end;

  AResponseInfo.ResponseNo := 200;
  AResponseInfo.ContentText := '{"ok":true}';
  AResponseInfo.ContentType := 'application/json';
end;

function TReceptorWebhook.Cuantos: Integer;
begin
  FLock.Enter;
  try
    Result := FAvisos.Count;
  finally
    FLock.Leave;
  end;
end;

function TReceptorWebhook.Ultimo: TAviso;
begin
  FLock.Enter;
  try
    Result := FAvisos.Last;
  finally
    FLock.Leave;
  end;
end;

procedure TReceptorWebhook.Limpiar;
begin
  FLock.Enter;
  try
    FAvisos.Clear;
  finally
    FLock.Leave;
  end;
end;

function TReceptorWebhook.EsperarAviso(ATimeoutMs: Integer): Boolean;
var
  Esperado: Integer;
begin
  Esperado := 0;
  while (Cuantos = 0) and (Esperado < ATimeoutMs) do
  begin
    Sleep(50);
    Inc(Esperado, 50);
  end;
  Result := Cuantos > 0;
end;

{ THandlers }

procedure THandlers.NodoLento(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Sleep(600); // simula trabajo largo: es el caso de uso de las push
  Output := Input + ' [procesado]';
end;

procedure THandlers.Fabrica(Sender: TObject; var AManager: TAIAgentManager);
begin
  AManager := TAIAgentManager.Create(nil);
  AManager.AddNode('Procesar', NodoLento);
  AManager.SetEntryPoint('Procesar').SetFinishPoint('Procesar');
end;

// -----------------------------------------------------------------------------

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

function Rpc(const AUrl, AJson: string): TJSONObject;
var
  Http: THTTPClient;
  Body: TStringStream;
begin
  Http := THTTPClient.Create;
  Body := TStringStream.Create(AJson, TEncoding.UTF8);
  try
    Http.ContentType := 'application/json';
    Http.ResponseTimeout := 60000;
    Result := TJSONObject(TJSONObject.ParseJSONValue(Http.Post(AUrl, Body).ContentAsString(TEncoding.UTF8)));
  finally
    Body.Free;
    Http.Free;
  end;
end;

var
  Server: TAiA2AServer;
  Agente: TAIAgentManager;
  Handlers: THandlers;
  Receptor: TReceptorWebhook;
  Puerto, PuertoHook: Integer;
  Url, HookUrl, TaskId: string;
  R: TJSONObject;
  Aviso: TAviso;
  Recibidos: Integer;

begin
  try
    Puerto := StrToIntDef(ArgValue('--port', '8282'), 8282);
    PuertoHook := StrToIntDef(ArgValue('--hook-port', '8283'), 8283);
    Url := Format('http://localhost:%d/', [Puerto]);
    HookUrl := Format('http://localhost:%d/webhook', [PuertoHook]);

    Writeln('=== DEMO 080: push notifications de A2A ===');
    Writeln('');

    Recibidos := 0;
    Handlers := THandlers.Create;
    Agente := TAIAgentManager.Create(nil);
    Server := TAiA2AServer.Create(nil);
    Receptor := TReceptorWebhook.Create(PuertoHook);
    try
      Agente.AddNode('Procesar', Handlers.NodoLento);
      Agente.SetEntryPoint('Procesar').SetFinishPoint('Procesar');
      Server.AgentManager := Agente;
      Server.AgentName := 'Agente con avisos';
      Server.OnAcquireManager := Handlers.Fabrica;
      Server.MaxConcurrentTasks := 8;
      Server.Port := Puerto;
      Server.Active := True;

      Writeln('   agente A2A  : ' + Url);
      Writeln('   webhook     : ' + HookUrl);

      // ---------------------------------------------------------------------
      Writeln('');
      Writeln('== 1. Registrar el webhook con el CRUD ==');
      Writeln('');

      // Un task que se queda trabajando, para registrar el aviso antes de que
      // termine. Con blocking=false SendMessage vuelve enseguida.
      R := Rpc(Url, '{"jsonrpc":"2.0","id":1,"method":"SendMessage","params":{' +
        '"message":{"messageId":"m1","role":"ROLE_USER","parts":[{"text":"lote-1"}]},' +
        '"configuration":{"blocking":false}}}');
      try
        TaskId := R.GetValue<TJSONObject>('result').GetValue<TJSONObject>('task').GetValue<string>('id');
      finally
        R.Free;
      end;
      Writeln('   task creado: ' + TaskId);

      R := Rpc(Url, Format('{"jsonrpc":"2.0","id":2,"method":"CreateTaskPushNotificationConfig","params":' +
        '{"taskId":"%s","config":{"id":"cfg-1","url":"%s",' +
        '"authentication":{"scheme":"Bearer","credentials":"mi-token-secreto"}}}}', [TaskId, HookUrl]));
      try
        Writeln('   config registrada: ' + R.GetValue<TJSONObject>('result').ToJSON);
      finally
        R.Free;
      end;

      R := Rpc(Url, Format('{"jsonrpc":"2.0","id":3,"method":"ListTaskPushNotificationConfigs",' +
        '"params":{"taskId":"%s"}}', [TaskId]));
      try
        Writeln('   listado: ' + R.GetValue<TJSONObject>('result').ToJSON);
      finally
        R.Free;
      end;

      Writeln('');
      Writeln('   esperando el aviso...');
      if Receptor.EsperarAviso(15000) then
      begin
        Aviso := Receptor.Ultimo;
        Inc(Recibidos);
        Writeln('   AVISO RECIBIDO');
        Writeln('     Authorization: ' + Aviso.Auth);
        Writeln('     cuerpo: ' + Copy(Aviso.Cuerpo, 1, 150) + '...');
        Writeln('   (el payload es un StreamResponse, con el task dentro de "task")');
      end
      else
        Writeln('   ATENCION: no llego ningun aviso');

      // ---------------------------------------------------------------------
      Writeln('');
      Writeln('== 2. Registrar el webhook EN la propia peticion ==');
      Writeln('');
      Writeln('   (configuration.taskPushNotificationConfig, sin segunda llamada)');
      Receptor.Limpiar;

      R := Rpc(Url, Format('{"jsonrpc":"2.0","id":4,"method":"SendMessage","params":{' +
        '"message":{"messageId":"m2","role":"ROLE_USER","parts":[{"text":"lote-2"}]},' +
        '"configuration":{"blocking":false,"taskPushNotificationConfig":' +
        '{"id":"cfg-2","url":"%s","authentication":{"scheme":"Bearer","credentials":"otro-token"}}}}}',
        [HookUrl]));
      try
        Writeln('   task: ' + R.GetValue<TJSONObject>('result').GetValue<TJSONObject>('task')
          .GetValue<string>('id'));
      finally
        R.Free;
      end;

      Writeln('   esperando el aviso...');
      if Receptor.EsperarAviso(15000) then
      begin
        Aviso := Receptor.Ultimo;
        Inc(Recibidos);
        Writeln('   AVISO RECIBIDO con Authorization: ' + Aviso.Auth);
      end
      else
        Writeln('   ATENCION: no llego ningun aviso');

      // ---------------------------------------------------------------------
      Writeln('');
      Writeln('== 3. Delete es idempotente ==');
      Writeln('');
      R := Rpc(Url, Format('{"jsonrpc":"2.0","id":5,"method":"DeleteTaskPushNotificationConfig",' +
        '"params":{"taskId":"%s","id":"cfg-1"}}', [TaskId]));
      try
        Writeln('   primer delete : ' + BoolToStr(R.GetValue('error') = nil, True) + ' (sin error)');
      finally
        R.Free;
      end;
      R := Rpc(Url, Format('{"jsonrpc":"2.0","id":6,"method":"DeleteTaskPushNotificationConfig",' +
        '"params":{"taskId":"%s","id":"cfg-1"}}', [TaskId]));
      try
        Writeln('   segundo delete: ' + BoolToStr(R.GetValue('error') = nil, True) +
          ' (borrar lo que ya no esta tampoco es error)');
      finally
        R.Free;
      end;

      Writeln('');
      if Recibidos = 2 then
      begin
        Writeln('OK: el agente notifico a un webhook externo al completar sus tareas.');
        ExitCode := 0;
      end
      else
      begin
        Writeln(Format('FALLO: se esperaban 2 avisos y llegaron %d.', [Recibidos]));
        Writeln('El CRUD de configuraciones SI funciona; lo que falla es la ENTREGA');
        Writeln('cuando nadie consulta el task (blocking=false). Ver el CLAUDE.md');
        Writeln('de este demo para el estado real de la limitacion.');
        ExitCode := 1;
      end;
      Server.Active := False;
    finally
      Receptor.Free;
      Server.Free;
      Agente.Free;
      Handlers.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
