program GroqTest;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - Demo 073: Groq Async Diagnostic
// =============================================================================
// Prueba el driver TAiGroqChat en modo sincrono y asincronico (streaming).
// Verifica modelos actuales (Abr 2026):
//   - llama-3.1-8b-instant  (texto, produccion)
//   - llama-3.3-70b-versatile (texto, produccion)
//   - openai/gpt-oss-120b   (texto + reasoning, produccion)
//   - qwen/qwen3-32b        (reasoning, preview)
//   - meta-llama/llama-4-scout-17b-16e-instruct (vision, preview)
//
// Configurar GROQ_API_KEY como variable de entorno antes de ejecutar.
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
  uMakerAi.Chat.Groq;

const
  API_KEY = '@GROQ_API_KEY';
  MODEL_FAST    = 'llama-3.1-8b-instant';
  MODEL_QUALITY = 'llama-3.3-70b-versatile';
  MODEL_REASON  = 'qwen/qwen3-32b';
  MODEL_REASON2 = 'openai/gpt-oss-120b';

// =============================================================================
//  TGroqTester
// =============================================================================

type
  TGroqTester = class
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

{ TGroqTester }

constructor TGroqTester.Create;
begin
  FDoneEvent   := TEvent.Create(nil, True, False, '');
  FRawChunkLog := TStringList.Create;

  FConn := TAiChatConnection.Create(nil);
  FConn.DriverName := 'Groq';
  FConn.Model      := MODEL_FAST;
  FConn.Params.Values['ApiKey'] := API_KEY;

  FConn.OnReceiveData     := OnData;
  FConn.OnReceiveDataEnd  := OnDataEnd;
  FConn.OnReceiveThinking := OnThinking;
  FConn.OnError           := OnError;
end;

destructor TGroqTester.Destroy;
begin
  FConn.Free;
  FDoneEvent.Free;
  FRawChunkLog.Free;
  inherited;
end;

procedure TGroqTester.ResetBuffers;
begin
  FBuffer      := '';
  FTokenCount  := 0;
  FThinkBuffer := '';
  FThinkCount  := 0;
  FHasError    := False;
  FError       := '';
  FRawChunkLog.Clear;
end;

procedure TGroqTester.SetAsync(AValue: Boolean);
begin
  if AValue then
    FConn.Params.Values['Asynchronous'] := 'True'
  else
    FConn.Params.Values['Asynchronous'] := 'False';
end;

procedure TGroqTester.OnData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  if Assigned(aResponse) and (FRawChunkLog.Count < 3) then
    FRawChunkLog.Add(Copy(aResponse.ToString, 1, 200));

  if aText <> '' then
  begin
    FBuffer := FBuffer + aText;
    Inc(FTokenCount);
    Write(aText);
  end;
end;

procedure TGroqTester.OnThinking(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  if aText <> '' then
  begin
    FThinkBuffer := FThinkBuffer + aText;
    Inc(FThinkCount);
  end;
end;

procedure TGroqTester.OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  FDoneEvent.SetEvent;
end;

procedure TGroqTester.OnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  FHasError := True;
  FError    := ErrorMsg;
  if Assigned(AException) then
    FError := FError + ' [' + AException.ClassName + ': ' + AException.Message + ']';
  FDoneEvent.SetEvent;
end;

procedure TGroqTester.PrintSeparator(const ATitle: String);
begin
  Writeln;
  Writeln(StringOfChar('=', 70));
  Writeln('  ', ATitle);
  Writeln(StringOfChar('=', 70));
end;

procedure TGroqTester.PrintAsyncResult;
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

  if FRawChunkLog.Count > 0 then
  begin
    Writeln('--- RAW SSE chunks (primeros ' + IntToStr(FRawChunkLog.Count) + ') ---');
    var i: Integer;
    for i := 0 to FRawChunkLog.Count - 1 do
      Writeln('  [' + IntToStr(i) + '] ' + FRawChunkLog[i]);
    Writeln;
  end;

  if FThinkCount > 0 then
  begin
    Writeln(Format('Thinking chunks : %d  (%d chars)', [FThinkCount, Length(FThinkBuffer)]));
    Writeln('Thinking preview: ' + Copy(FThinkBuffer, 1, 120) + '...');
    Writeln;
  end
  else
    Writeln('Thinking chunks : 0  (sin reasoning en este modelo/modo)');

  Writeln(Format('Texto chunks    : %d  (%d chars en buffer)', [FTokenCount, Length(FBuffer)]));

  if FConn.GetLastMessage <> nil then
  begin
    LHistorial := FConn.GetLastMessage.Prompt;
    LReasoning := FConn.GetLastMessage.ReasoningContent;
    Writeln(Format('Historial Prompt: %d chars', [Length(LHistorial)]));
    if LReasoning <> '' then
      Writeln(Format('Historial Reason: %d chars', [Length(LReasoning)]));

    if LHistorial <> FBuffer then
    begin
      Writeln;
      Writeln('*** DISCREPANCIA buffer vs historial ***');
      Writeln(Format('  Buffer    : %d chars  |  Historial : %d chars', [Length(FBuffer), Length(LHistorial)]));
    end
    else
      Writeln('  [OK] Buffer y historial coinciden.');
  end;
end;

// -----------------------------------------------------------------------------
procedure TGroqTester.TestSync(const AQuestion, AModel, ALabel: String);
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
procedure TGroqTester.TestAsync(const AQuestion, AModel, ALabel: String;
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
procedure TGroqTester.TestMultiTurnAsync(const AModel: String);

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

    Write('Groq: ');
    FConn.AddMessageAndRun(AQuestion, 'user', []);
    WaitResult := FDoneEvent.WaitFor(60000);

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
  Tester: TGroqTester;

begin
  Writeln('=== Demo 073 - Groq Async Diagnostic ===');
  Writeln('APIKey : ', API_KEY);
  Writeln;

  Tester := TGroqTester.Create;
  try
    // --- Produccion: llama-3.1-8b-instant (ultra-rapido) ---
    Tester.TestSync(
      'En una sola oracion: cual es la capital de Francia?',
      MODEL_FAST, 'Test 1 - Sync basico (llama-3.1-8b)');

    Tester.TestAsync(
      'En una sola oracion: cual es la capital de Japon?',
      MODEL_FAST, 'Test 2 - Async basico (llama-3.1-8b)');

    // --- Produccion: llama-3.3-70b-versatile ---
    Tester.TestAsync(
      'Explica en 2 parrafos breves que es el machine learning.',
      MODEL_QUALITY, 'Test 3 - Async calidad (llama-3.3-70b)');

    // --- Reasoning: qwen/qwen-3-32b (reasoning_effort=medium) ---
    Tester.TestSync(
      'Cuanto es 347 multiplicado por 28? Muestra el razonamiento.',
      MODEL_REASON, 'Test 4 - Sync reasoning (qwen-3-32b)');

    Tester.TestAsync(
      'Cuanto es 347 multiplicado por 28? Muestra el razonamiento.',
      MODEL_REASON, 'Test 5 - Async reasoning (qwen-3-32b)');

    // --- Produccion: openai/gpt-oss-120b (reasoning nativo) ---
    Tester.TestSync(
      'Cuanto es 347 multiplicado por 28? Muestra el razonamiento.',
      MODEL_REASON2, 'Test 6 - Sync gpt-oss-120b (reasoning)');

    Tester.TestAsync(
      'Explica en 2 parrafos breves que es la inteligencia artificial.',
      MODEL_REASON2, 'Test 7 - Async gpt-oss-120b');

    // --- Multi-turno con llama-3.3-70b ---
    Tester.TestMultiTurnAsync(MODEL_QUALITY);

  finally
    Tester.Free;
  end;

  Writeln;
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
