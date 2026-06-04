program GrokTest;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - Demo 072: Grok Async Diagnostic
// =============================================================================
// Prueba el driver TAiGrokChat en modo sincrono y asincronico (streaming).
// Verifica:
//   - Texto basico sync/async con grok-3
//   - Reasoning con grok-3-mini (reasoning_effort=low/high, sin error 400)
//   - Que los parametros prohibidos NO se envian en modelos grok-4 / grok-3-mini
//   - Vision con grok-4-0709 (si el usuario tiene acceso)
//   - Multi-turno async
//
// Configurar GROK_API_KEY como variable de entorno antes de ejecutar.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.SyncObjs,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Grok;

const
  API_KEY = '@GROK_API_KEY';
  // Modelos disponibles:
  //   'grok-3'               - texto + tools, 131K ctx
  //   'grok-3-fast'          - texto + tools, 131K ctx (mismas caps, infra rapida)
  //   'grok-3-mini'          - texto + reasoning + tools (reasoning_effort: low/high)
  //   'grok-3-mini-fast'     - idem grok-3-mini
  //   'grok-2-vision-1212'   - vision + tools
  //   'grok-4-0709'          - vision + reasoning siempre activo + tools, 256K ctx
  //   'grok-4-fast-reasoning'- vision + reasoning + tools, 2M ctx
  MODEL_TEXT    = 'grok-3';
  MODEL_REASON  = 'grok-3-mini';
  MODEL_VISION  = 'grok-4-0709';

// =============================================================================
//  TGrokTester
// =============================================================================

type
  TGrokTester = class
  private
    FConn        : TAiChatConnection;
    FDoneEvent   : TEvent;
    FBuffer      : String;
    FTokenCount  : Integer;
    FThinkBuffer : String;
    FThinkCount  : Integer;
    FError       : String;
    FHasError    : Boolean;
    FRawChunkLog : TStringList;

    procedure OnData(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: String);
    procedure OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: String);
    procedure OnThinking(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: String);
    procedure OnError(Sender: TObject; const ErrorMsg: string;
      AException: Exception; const AResponse: IHTTPResponse);

    procedure PrintSeparator(const ATitle: String);
    procedure SetAsync(AValue: Boolean);
    procedure ResetBuffers;
    procedure PrintAsyncResult;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure TestSync(const AQuestion, AModel, ALabel: String);
    procedure TestAsync(const AQuestion, AModel, ALabel: String;
      ATimeoutMs: Integer = 120000);
    procedure TestMultiTurnAsync(const AModel: String);
  end;

{ TGrokTester }

constructor TGrokTester.Create;
begin
  FDoneEvent   := TEvent.Create(nil, True, False, '');
  FRawChunkLog := TStringList.Create;

  FConn := TAiChatConnection.Create(nil);
  FConn.DriverName := 'Grok';
  FConn.Model      := MODEL_TEXT;
  FConn.Params.Values['ApiKey'] := API_KEY;

  FConn.OnReceiveData     := OnData;
  FConn.OnReceiveDataEnd  := OnDataEnd;
  FConn.OnReceiveThinking := OnThinking;
  FConn.OnError           := OnError;
end;

destructor TGrokTester.Destroy;
begin
  FConn.Free;
  FDoneEvent.Free;
  FRawChunkLog.Free;
  inherited;
end;

procedure TGrokTester.ResetBuffers;
begin
  FBuffer      := '';
  FTokenCount  := 0;
  FThinkBuffer := '';
  FThinkCount  := 0;
  FHasError    := False;
  FError       := '';
  FRawChunkLog.Clear;
end;

procedure TGrokTester.SetAsync(AValue: Boolean);
begin
  if AValue then
    FConn.Params.Values['Asynchronous'] := 'True'
  else
    FConn.Params.Values['Asynchronous'] := 'False';
end;

procedure TGrokTester.OnData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  // Capturar primeros 3 chunks raw para ver la estructura SSE de Grok
  if Assigned(aResponse) and (FRawChunkLog.Count < 3) then
    FRawChunkLog.Add(Copy(aResponse.ToString, 1, 250));

  if aText <> '' then
  begin
    FBuffer := FBuffer + aText;
    Inc(FTokenCount);
    Write(aText);
  end;
end;

procedure TGrokTester.OnThinking(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  if aText <> '' then
  begin
    FThinkBuffer := FThinkBuffer + aText;
    Inc(FThinkCount);
  end;
end;

procedure TGrokTester.OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  FDoneEvent.SetEvent;
end;

procedure TGrokTester.OnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  FHasError := True;
  FError    := ErrorMsg;
  if Assigned(AException) then
    FError := FError + ' [' + AException.ClassName + ': ' + AException.Message + ']';
  FDoneEvent.SetEvent;
end;

procedure TGrokTester.PrintSeparator(const ATitle: String);
begin
  Writeln;
  Writeln(StringOfChar('=', 70));
  Writeln('  ', ATitle);
  Writeln(StringOfChar('=', 70));
end;

procedure TGrokTester.PrintAsyncResult;
var
  LHistorial, LReasoning: String;
begin
  Writeln;
  Writeln(StringOfChar('-', 70));

  if FHasError then
  begin
    Writeln('ERROR: ', FError);
    Exit;
  end;

  // Raw chunks
  if FRawChunkLog.Count > 0 then
  begin
    Writeln('--- RAW SSE chunks (primeros ' + IntToStr(FRawChunkLog.Count) + ') ---');
    var i: Integer;
    for i := 0 to FRawChunkLog.Count - 1 do
      Writeln('  [' + IntToStr(i) + '] ' + FRawChunkLog[i]);
    Writeln;
  end;

  // Thinking
  if FThinkCount > 0 then
  begin
    Writeln(Format('Thinking chunks : %d  (%d chars)', [FThinkCount, Length(FThinkBuffer)]));
    Writeln('Thinking preview: ' + Copy(FThinkBuffer, 1, 120) + '...');
    Writeln;
  end
  else
    Writeln('Thinking chunks : 0  (sin reasoning en este modelo/modo)');

  Writeln(Format('Texto chunks    : %d  (%d chars en buffer)', [FTokenCount, Length(FBuffer)]));

  // Coherencia con historial
  if FConn.GetLastMessage <> nil then
  begin
    LHistorial := FConn.GetLastMessage.Prompt;
    LReasoning := FConn.GetLastMessage.ReasoningContent;
    Writeln(Format('Historial Prompt: %d chars', [Length(LHistorial)]));
    if LReasoning <> '' then
      Writeln(Format('Historial Reason: %d chars', [Length(LReasoning)]));

    Writeln;
    Writeln('--- Respuesta ---');
    Writeln(FBuffer);

    if LHistorial <> FBuffer then
    begin
      Writeln;
      Writeln('*** DISCREPANCIA buffer vs historial ***');
      Writeln(Format('  Buffer    : %d chars', [Length(FBuffer)]));
      Writeln(Format('  Historial : %d chars', [Length(LHistorial)]));
      Writeln('  Buffer    inicio: [' + Copy(FBuffer, 1, 40) + ']');
      Writeln('  Historial inicio: [' + Copy(LHistorial, 1, 40) + ']');
      Writeln('  Buffer    final : [' + Copy(FBuffer, Length(FBuffer)-39, 40) + ']');
      Writeln('  Historial final : [' + Copy(LHistorial, Length(LHistorial)-39, 40) + ']');
    end
    else
      Writeln('  [OK] Buffer y historial coinciden.');
  end;
end;

// -----------------------------------------------------------------------------
//  TestSync
// -----------------------------------------------------------------------------
procedure TGrokTester.TestSync(const AQuestion, AModel, ALabel: String);
var
  Response: String;
begin
  PrintSeparator(ALabel + ' [' + AModel + ']');
  Writeln('Pregunta : ', AQuestion);
  Writeln(StringOfChar('-', 70));

  ResetBuffers;
  FConn.Model := AModel;
  FConn.NewChat;
  SetAsync(False);
  try
    Response := FConn.AddMessageAndRun(AQuestion, 'user', []);
    if FHasError then
      Writeln('ERROR: ', FError)
    else if Response = '' then
      Writeln('Respuesta: <vacia>')
    else
      Writeln('Respuesta: ', Response);
  except
    on E: Exception do
      Writeln('EXCEPCION: ', E.ClassName, ' - ', E.Message);
  end;
end;

// -----------------------------------------------------------------------------
//  TestAsync
// -----------------------------------------------------------------------------
procedure TGrokTester.TestAsync(const AQuestion, AModel, ALabel: String;
  ATimeoutMs: Integer);
var
  WaitResult: TWaitResult;
begin
  PrintSeparator(ALabel + ' [' + AModel + ']');
  Writeln('Pregunta : ', AQuestion);
  Writeln(StringOfChar('-', 70));

  ResetBuffers;
  FConn.Model := AModel;
  FConn.NewChat;
  SetAsync(True);
  FDoneEvent.ResetEvent;

  Write('[streaming] ');
  try
    FConn.AddMessageAndRun(AQuestion, 'user', []);
  except
    on E: Exception do
    begin
      Writeln;
      Writeln('ERROR al llamar AddMessageAndRun: ', E.Message);
      Exit;
    end;
  end;

  WaitResult := FDoneEvent.WaitFor(ATimeoutMs);
  if WaitResult = wrTimeout then
  begin
    Writeln;
    Writeln('TIMEOUT (' + IntToStr(ATimeoutMs) + ' ms)');
    Exit;
  end;

  PrintAsyncResult;
end;

// -----------------------------------------------------------------------------
//  TestMultiTurnAsync
// -----------------------------------------------------------------------------
procedure TGrokTester.TestMultiTurnAsync(const AModel: String);

  procedure SendTurn(const AQuestion: String; ATurn: Integer);
  var
    WaitResult: TWaitResult;
  begin
    Writeln;
    Writeln(Format('-- Turno %d --', [ATurn]));
    Writeln('Usuario: ', AQuestion);

    ResetBuffers;
    FDoneEvent.ResetEvent;
    SetAsync(True);

    Write('Grok: ');
    FConn.AddMessageAndRun(AQuestion, 'user', []);
    WaitResult := FDoneEvent.WaitFor(120000);

    Writeln;
    if WaitResult = wrSignaled then
    begin
      if FHasError then
        Writeln('  ERROR: ', FError)
      else
      begin
        if FThinkCount > 0 then
          Writeln(Format('  [thinking: %d chunks]', [FThinkCount]));
        Writeln(Format('  [texto: %d chunks, %d chars]', [FTokenCount, Length(FBuffer)]));
      end;
    end
    else
      Writeln('  TIMEOUT');
  end;

begin
  PrintSeparator('Multi-turno Async [' + AModel + ']');
  FConn.Model := AModel;
  FConn.NewChat;

  SendTurn('Hola! Mi nombre es Gustavo. Recuerdalo.', 1);
  SendTurn('Cual es mi nombre?', 2);
  SendTurn('Cuantos mensajes llevamos en esta conversacion?', 3);

  Writeln;
  Writeln(Format('Mensajes en historial: %d', [FConn.Messages.Count]));
end;

// =============================================================================
//  MAIN
// =============================================================================

var
  Tester: TGrokTester;

begin
  Writeln('=== Demo 072 - Grok Async Diagnostic ===');
  Writeln('APIKey : ', API_KEY);
  Writeln;

  Tester := TGrokTester.Create;
  try
    // --- grok-3 (texto + tools, sin reasoning) ---
    Tester.TestSync(
      'En una sola oracion: cual es la capital de Francia?',
      MODEL_TEXT, 'Test 1 - Sync basico');

    Tester.TestAsync(
      'En una sola oracion: cual es la capital de Japon?',
      MODEL_TEXT, 'Test 2 - Async basico');

    Tester.TestAsync(
      'Explica en 3 parrafos breves que es la inteligencia artificial.',
      MODEL_TEXT, 'Test 3 - Async respuesta larga');

    // --- grok-3-mini (reasoning: reasoning_effort=low, sin vision) ---
    // Verifica que NO se envia frequency/presence/stop (error 400 si se enviaran)
    Tester.TestSync(
      'Cuanto es 347 multiplicado por 28? Muestra el razonamiento paso a paso.',
      MODEL_REASON, 'Test 4 - Sync reasoning');

    Tester.TestAsync(
      'Cuanto es 347 multiplicado por 28? Muestra el razonamiento paso a paso.',
      MODEL_REASON, 'Test 5 - Async reasoning');

    // --- grok-4-0709 (vision + reasoning siempre activo) ---
    // Verifica que NO se envia reasoning_effort (error si se enviara)
    Tester.TestSync(
      'En una sola oracion: cual es la capital de Australia?',
      MODEL_VISION, 'Test 6 - Sync grok-4 (sin reasoning_effort)');

    Tester.TestAsync(
      'Explica en 2 oraciones breves que es el machine learning.',
      MODEL_VISION, 'Test 7 - Async grok-4');

    // --- Multi-turno con grok-3 ---
    Tester.TestMultiTurnAsync(MODEL_TEXT);

  finally
    Tester.Free;
  end;

  Writeln;
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
