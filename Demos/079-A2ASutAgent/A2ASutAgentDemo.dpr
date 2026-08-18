program A2ASutAgentDemo;

// =============================================================================
// DEMO 079 - Agente SUT conforme al TCK oficial de A2A
// =============================================================================
// El TCK (a2aproject/a2a-tck) no solo valida el protocolo: para poder probar
// ciertas cosas necesita que el agente bajo prueba implemente un contrato de
// escenarios. Despacha por el PREFIJO DEL messageId, no por el texto:
//
//   tck-artifact-text      -> artifact con un TextPart "Generated text content"
//   tck-artifact-data      -> artifact con un DataPart {"key":"value","count":42}
//   tck-artifact-file      -> artifact con un FilePart de bytes (raw + filename)
//   tck-artifact-file-url  -> artifact con un FilePart por URL
//   tck-message-response   -> responde con un Message, no con un Task
//   tck-input-required     -> deja el task en input-required
//   tck-complete-task      -> completa con un mensaje de agente
//
// Este demo sirve para dos cosas a la vez:
//   1. Es el System Under Test con el que se corre el TCK completo.
//   2. Es el ejemplo de como emitir artifacts ESTRUCTURADOS desde un grafo,
//      que es la capacidad que hacia falta: hasta ahora un agente MakerAI solo
//      podia devolver texto plano.
//
// El canal es el blackboard. El servidor siembra A2A.MessageId / A2A.TaskId /
// A2A.ContextId antes de correr el grafo, y al terminar lee:
//   A2A.Artifacts -> array JSON de artifacts, se emite tal cual
//   A2A.Message   -> objeto Message; si esta, SendMessage responde con el brazo
//                    {"message": ...} del oneof en vez de {"task": ...}
//
// Uso:
//   A2ASutAgentDemo.exe [--port 9999]
//
// Y en otra consola, el TCK:
//   python run_tck.py --sut-host http://localhost:9999 --level must
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.NetEncoding,
  uMakerAi.Agents,
  uMakerAi.A2A.Server in '..\..\Source\Agents\uMakerAi.A2A.Server.pas',
  uMakerAi.Telemetry;

type
  TSutHandlers = class
  public
    procedure Escenario(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Fabrica del pool. Sin esto el TCK falla: lanza peticiones concurrentes y
    // un unico TAIAgentManager no ejecuta dos grafos a la vez, asi que la
    // segunda recibe AgentBusy (-32000) y rompe la validacion del envelope.
    procedure FabricaManager(Sender: TObject; var AManager: TAIAgentManager);
  end;

  // ---------------------------------------------------------------------------
  // Helpers para construir artifacts conformes con la spec 1.0.
  // En v1.0 no hay TextPart/DataPart/FilePart como tipos distintos: hay un unico
  // Part y el contenido lo determina QUE campo esta presente (text, data, raw,
  // url), con mediaType y filename opcionales.
  // ---------------------------------------------------------------------------

function NuevoArtifact(const AId, ANombre: string; APart: TJSONObject): string;
var
  Arr: TJSONArray;
  Art: TJSONObject;
  Parts: TJSONArray;
begin
  Art := TJSONObject.Create;
  Art.AddPair('artifactId', AId);
  Art.AddPair('name', ANombre);
  Parts := TJSONArray.Create;
  Parts.AddElement(APart);
  Art.AddPair('parts', Parts);
  Arr := TJSONArray.Create;
  Arr.AddElement(Art);
  try
    Result := Arr.ToJSON;
  finally
    Arr.Free;
  end;
end;

function PartTexto(const ATexto: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('text', ATexto);
end;

function PartDatos(const AJson: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('data', TJSONObject.ParseJSONValue(AJson));
end;

function PartArchivoBytes(const ABytes, AMediaType, ANombre: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  // 'raw' viaja en base64 por ProtoJSON (los bytes de proto se serializan asi)
  Result.AddPair('raw', TNetEncoding.Base64.Encode(ABytes));
  Result.AddPair('mediaType', AMediaType);
  Result.AddPair('filename', ANombre);
end;

function PartArchivoUrl(const AUrl, AMediaType, ANombre: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('url', AUrl);
  Result.AddPair('mediaType', AMediaType);
  Result.AddPair('filename', ANombre);
end;

function MensajeAgente(const ATexto, ATaskId, AContextId: string): string;
var
  Msg: TJSONObject;
  Parts: TJSONArray;
begin
  Msg := TJSONObject.Create;
  try
    Msg.AddPair('messageId', 'sut-' + ATaskId);
    Msg.AddPair('role', 'ROLE_AGENT');
    if ATaskId <> '' then
      Msg.AddPair('taskId', ATaskId);
    if AContextId <> '' then
      Msg.AddPair('contextId', AContextId);
    Parts := TJSONArray.Create;
    Parts.AddElement(PartTexto(ATexto));
    Msg.AddPair('parts', Parts);
    Result := Msg.ToJSON;
  finally
    Msg.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSutHandlers.Escenario(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string;
  var Output: string);
var
  B: TAIBlackboard;
  MsgId, TaskId, CtxId: string;
begin
  B := Node.Graph.Blackboard;
  // El TCK despacha por el prefijo del messageId, no por el contenido
  MsgId := B.GetString(A2A_BB_MESSAGE_ID);
  TaskId := B.GetString(A2A_BB_TASK_ID);
  CtxId := B.GetString(A2A_BB_CONTEXT_ID);
  Output := Input;

  Writeln(Format('  [SUT] messageId=%s', [MsgId]));

  // OJO al orden: 'tck-artifact-file-url' empieza por 'tck-artifact-file',
  // asi que la variante mas especifica va primero.
  if MsgId.StartsWith('tck-artifact-file-url') then
    B.SetString(A2A_BB_ARTIFACTS, NuevoArtifact(TaskId + '-a', 'output',
      PartArchivoUrl('https://example.com/output.txt', 'text/plain', 'output.txt')))

  else if MsgId.StartsWith('tck-artifact-file') then
    B.SetString(A2A_BB_ARTIFACTS, NuevoArtifact(TaskId + '-a', 'output',
      PartArchivoBytes('tck', 'text/plain', 'output.txt')))

  else if MsgId.StartsWith('tck-artifact-text') then
    B.SetString(A2A_BB_ARTIFACTS, NuevoArtifact(TaskId + '-a', 'output',
      PartTexto('Generated text content')))

  else if MsgId.StartsWith('tck-artifact-data') then
    B.SetString(A2A_BB_ARTIFACTS, NuevoArtifact(TaskId + '-a', 'output',
      PartDatos('{"key": "value", "count": 42}')))

  else if MsgId.StartsWith('tck-message-response') then
    // Brazo 'message' del oneof: se responde sin crear tarea
    B.SetString(A2A_BB_MESSAGE, MensajeAgente('Direct message response', TaskId, CtxId))

  else if MsgId.StartsWith('tck-complete-task') then
    Output := 'Hello from TCK'

  else if MsgId.StartsWith('tck-input-required') then
    Node.Suspend('El TCK pidió input', 'tck-input-required')

  else
    Output := 'echo: ' + Input;
end;

procedure TSutHandlers.FabricaManager(Sender: TObject; var AManager: TAIAgentManager);
begin
  AManager := TAIAgentManager.Create(nil); // el servidor lo libera
  AManager.AddNode('Escenario', Escenario);
  AManager.SetEntryPoint('Escenario').SetFinishPoint('Escenario');
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

var
  Server: TAiA2AServer;
  Agente: TAIAgentManager;
  Handlers: TSutHandlers;
  Port: Integer;

begin
  try
    Port := StrToIntDef(ArgValue('--port', '9999'), 9999);

    Handlers := TSutHandlers.Create;
    Agente := TAIAgentManager.Create(nil);
    Server := TAiA2AServer.Create(nil);
    try
      Agente.Name := 'SutAgent';
      Agente.AddNode('Escenario', Handlers.Escenario);
      Agente.SetEntryPoint('Escenario').SetFinishPoint('Escenario');

      Server.AgentManager := Agente;
      Server.AgentName := 'MakerAI TCK SUT';
      Server.AgentDescription := 'Agente de conformidad para el TCK oficial de A2A';
      Server.Port := Port;
      Server.PublishExtendedCard := True;
      Server.OnAcquireManager := Handlers.FabricaManager;
      // Alto a proposito: un task suspendido en input-required RETIENE su
      // manager -es el unico que puede reanudarlo- y el TCK crea muchos.
      Server.MaxConcurrentTasks := 64;
      Server.Active := True;

      Writeln('=== DEMO 079: agente SUT conforme al TCK de A2A ===');
      Writeln('');
      Writeln(Format('Escuchando en http://localhost:%d/', [Port]));
      Writeln(Format('Agent Card:    http://localhost:%d/.well-known/agent-card.json', [Port]));
      Writeln('');
      Writeln('Ahora, desde el TCK:');
      Writeln(Format('  python run_tck.py --sut-host http://localhost:%d --level must', [Port]));
      Writeln('');
      Writeln('Ctrl+C para terminar.');
      Writeln('');

      while True do
      begin
        CheckSynchronize(200);
        Sleep(50);
      end;
    finally
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
