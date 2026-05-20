program KimiTest;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - Demo 070: Kimi Async Diagnostic
// =============================================================================
// Prueba el driver TAiKimiChat en modo sincrono y asincronico (streaming).
// Muestra cada token recibido en async para detectar problemas de parseo.
//
// Configurar KIMI_API_KEY como variable de entorno antes de ejecutar.
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
  uMakerAi.Chat.Kimi;

const
  API_KEY = '@KIMI_API_KEY';
  // Modelos disponibles:
  //   'kimi-k2'          - texto + tools, 256K ctx
  //   'kimi-k2.5'        - vision + PDF + reasoning + tools
  //   'kimi-k2-thinking' - reasoning + tools, sin vision
  //   'moonshot-v1-8k'   - legacy estable
  MODEL   = 'kimi-k2.5';

// =============================================================================
//  TKimiTester
// =============================================================================

type
  TKimiTester = class
  private
    FConn        : TAiChatConnection;
    FDoneEvent   : TEvent;
    FBuffer      : String;
    FTokenCount  : Integer;
    FThinkBuffer  : String;
    FThinkCount   : Integer;
    FError        : String;
    FHasError     : Boolean;
    FRawChunkLog  : TStringList;  // primeros N chunks raw para diagnostico

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
  public
    constructor Create;
    destructor  Destroy; override;

    procedure TestSync(const AQuestion: String; const ALabel: String = '');
    procedure TestAsync(const AQuestion: String; const ALabel: String = '';
      ATimeoutMs: Integer = 120000);
    procedure TestMultiTurnAsync;
  end;

{ TKimiTester }

constructor TKimiTester.Create;
begin
  FDoneEvent    := TEvent.Create(nil, True, False, '');
  FRawChunkLog  := TStringList.Create;

  FConn := TAiChatConnection.Create(nil);
  FConn.DriverName := 'Kimi';
  FConn.Model      := MODEL;
  FConn.Params.Values['ApiKey'] := API_KEY;

  FConn.OnReceiveData    := OnData;
  FConn.OnReceiveDataEnd := OnDataEnd;
  FConn.OnReceiveThinking := OnThinking;
  FConn.OnError          := OnError;
end;

destructor TKimiTester.Destroy;
begin
  FConn.Free;
  FDoneEvent.Free;
  FRawChunkLog.Free;
  inherited;
end;

procedure TKimiTester.ResetBuffers;
begin
  FBuffer      := '';
  FTokenCount  := 0;
  FThinkBuffer := '';
  FThinkCount  := 0;
  FHasError    := False;
  FError       := '';
  FRawChunkLog.Clear;
end;

procedure TKimiTester.SetAsync(AValue: Boolean);
begin
  if AValue then
    FConn.Params.Values['Asynchronous'] := 'True'
  else
    FConn.Params.Values['Asynchronous'] := 'False';
end;

procedure TKimiTester.OnData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  // Capturar primeros 5 chunks raw para ver la estructura JSON de Kimi
  if Assigned(aResponse) and (FRawChunkLog.Count < 5) then
    FRawChunkLog.Add(aResponse.ToString);

  if aText <> '' then
  begin
    FBuffer := FBuffer + aText;
    Inc(FTokenCount);
    Write(aText);
  end;
end;

procedure TKimiTester.OnThinking(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  if aText <> '' then
  begin
    FThinkBuffer := FThinkBuffer + aText;
    Inc(FThinkCount);
  end;
end;

procedure TKimiTester.OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  FDoneEvent.SetEvent;
end;

procedure TKimiTester.OnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  FHasError := True;
  FError    := ErrorMsg;
  if Assigned(AException) then
    FError := FError + ' [' + AException.ClassName + ': ' + AException.Message + ']';
  FDoneEvent.SetEvent;
end;

procedure TKimiTester.PrintSeparator(const ATitle: String);
begin
  Writeln;
  Writeln(StringOfChar('=', 70));
  Writeln('  ', ATitle);
  Writeln(StringOfChar('=', 70));
end;

// -----------------------------------------------------------------------------
//  TestSync
// -----------------------------------------------------------------------------
procedure TKimiTester.TestSync(const AQuestion: String; const ALabel: String);
var
  Response: String;
  LLabel: String;
begin
  if ALabel <> '' then LLabel := ALabel else LLabel := 'Sync';
  PrintSeparator(LLabel);
  Writeln('Pregunta : ', AQuestion);
  Writeln(StringOfChar('-', 70));

  ResetBuffers;
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
procedure TKimiTester.TestAsync(const AQuestion: String; const ALabel: String;
  ATimeoutMs: Integer);
var
  WaitResult: TWaitResult;
  LLabel: String;
begin
  if ALabel <> '' then LLabel := ALabel else LLabel := 'Async';
  PrintSeparator(LLabel);
  Writeln('Pregunta : ', AQuestion);
  Writeln(StringOfChar('-', 70));

  ResetBuffers;
  FConn.NewChat;
  SetAsync(True);
  FDoneEvent.ResetEvent;

  Writeln('[thinking...]');
  try
    FConn.AddMessageAndRun(AQuestion, 'user', []);
  except
    on E: Exception do
    begin
      Writeln('ERROR al llamar AddMessageAndRun: ', E.Message);
      Exit;
    end;
  end;

  WaitResult := FDoneEvent.WaitFor(ATimeoutMs);

  Writeln;
  Writeln(StringOfChar('-', 70));

  case WaitResult of
    wrSignaled:
      if FHasError then
        Writeln('ERROR recibido: ', FError)
      else
      begin
        // Dump de los primeros chunks raw para diagnostico de campo de reasoning
        if FRawChunkLog.Count > 0 then
        begin
          Writeln('--- RAW SSE chunks (primeros ' + IntToStr(FRawChunkLog.Count) + ') ---');
          var i: Integer;
          for i := 0 to FRawChunkLog.Count - 1 do
            Writeln('  [' + IntToStr(i) + '] ' + Copy(FRawChunkLog[i], 1, 200));
          Writeln;
        end;

        // Resumen de thinking (si hubo)
        if FThinkCount > 0 then
        begin
          Writeln(Format('Thinking chunks : %d  (%d chars)', [FThinkCount, Length(FThinkBuffer)]));
          Writeln('Thinking preview: ', Copy(FThinkBuffer, 1, 120), '...');
          Writeln;
        end
        else
          Writeln('Thinking chunks : 0  (sin reasoning en este modelo/modo)');

        Writeln(Format('Texto chunks    : %d  (%d chars en buffer)', [FTokenCount, Length(FBuffer)]));

        // Verificar coherencia con historial
        if FConn.GetLastMessage <> nil then
        begin
          var LHistorial := FConn.GetLastMessage.Prompt;
          var LReasoning := FConn.GetLastMessage.ReasoningContent;
          Writeln(Format('Historial Prompt: %d chars', [Length(LHistorial)]));
          if LReasoning <> '' then
            Writeln(Format('Historial Reason: %d chars', [Length(LReasoning)]));

          Writeln;
          Writeln('--- Respuesta (buffer streaming) ---');
          Writeln(FBuffer);

          if LHistorial <> FBuffer then
          begin
            Writeln;
            Writeln('*** DISCREPANCIA buffer vs historial ***');
            Writeln(Format('  Buffer    : %d chars', [Length(FBuffer)]));
            Writeln(Format('  Historial : %d chars', [Length(LHistorial)]));
            // Mostrar primeros/ultimos chars para ver diferencia
            Writeln('  Buffer    inicio: [' + Copy(FBuffer, 1, 30) + ']');
            Writeln('  Historial inicio: [' + Copy(LHistorial, 1, 30) + ']');
            Writeln('  Buffer    final : [' + Copy(FBuffer, Length(FBuffer)-29, 30) + ']');
            Writeln('  Historial final : [' + Copy(LHistorial, Length(LHistorial)-29, 30) + ']');
          end
          else
            Writeln('  [OK] Buffer y historial coinciden.');
        end;
      end;
    wrTimeout:
      Writeln('TIMEOUT (' + IntToStr(ATimeoutMs) + ' ms)');
  else
    Writeln('WaitFor: ', Ord(WaitResult));
  end;
end;

// -----------------------------------------------------------------------------
//  TestMultiTurnAsync
// -----------------------------------------------------------------------------
procedure TKimiTester.TestMultiTurnAsync;

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

    Write('Kimi: ');
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
  PrintSeparator('Multi-turno Async');
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
  Tester: TKimiTester;

begin
  Writeln('=== Demo 070 - Kimi Async Diagnostic ===');
  Writeln('Model  : ', MODEL);
  Writeln('APIKey : ', API_KEY);
  Writeln;

  Tester := TKimiTester.Create;
  try
    Tester.TestSync(
      'En una sola oracion: cual es la capital de Francia?',
      'Test 1 - Sync basico');

    Tester.TestAsync(
      'En una sola oracion: cual es la capital de Japon?',
      'Test 2 - Async basico');

    Tester.TestAsync(
      'Explica en 3 parrafos breves que es la inteligencia artificial.',
      'Test 3 - Async respuesta larga');

    Tester.TestMultiTurnAsync;

  finally
    Tester.Free;
  end;

  Writeln;
  Writeln('Demo finalizado. Presiona Enter para salir.');
  Readln;
end.
