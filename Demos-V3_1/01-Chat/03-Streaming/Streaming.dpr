program Streaming;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 01-Chat / 03-Streaming
// =============================================================================
// Muestra la respuesta del LLM token a token conforme llega (streaming),
// sin esperar a que termine la generación.
//
// Conceptos que cubre:
//   - Evento OnReceiveData    (dispara por cada fragmento recibido)
//   - Evento OnReceiveDataEnd (dispara al terminar el stream)
//   - Diferencia visual entre esperar respuesta completa vs. streaming
//   - Medición de tiempo hasta primer token (TTFT) y tiempo total
//
// Nota: Asynchronous = False sigue siendo síncrono en cuanto a que
// AddMessageAndRun() no retorna hasta terminar, pero los eventos
// OnReceiveData se disparan en hilo principal conforme llegan
// los fragmentos → se puede imprimir progresivamente.
// =============================================================================

uses
  System.SysUtils,
  System.Diagnostics,
  System.JSON,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Ollama,
  uMakerAi.Chat.Groq;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

// =============================================================================
//  TStreamingDemo — encapsula la conexión y los handlers de streaming
// =============================================================================
type
  TStreamingDemo = class
  private
    FConn        : TAiChatConnection;
    // Métricas para Demo 2
    FSW          : TStopwatch;
    FFirstTokenMs: Int64;
    FTokenCount  : Integer;
    // Estado para Demo 3
    FFullResponse: String;
    FFragCount   : Integer;

    // ── Handlers Demo 2: imprime cada fragmento en tiempo real ──────────────
    procedure OnChunk(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: String);
    procedure OnDone(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: String);

    // ── Handlers Demo 3: acumula + muestra puntos de progreso ───────────────
    procedure OnProgressChunk(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: String);
    procedure OnProgressDone(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: String);

    procedure DemoSinStreaming;
    procedure DemoConStreaming;
    procedure DemoStreamingConProgreso;
  public
    procedure Run;
  end;

// =============================================================================
//  Handlers para Demo 2
// =============================================================================

procedure TStreamingDemo.OnChunk(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  if FFirstTokenMs = 0 then
    FFirstTokenMs := FSW.ElapsedMilliseconds;
  Write(aText);
  Inc(FTokenCount);
end;

procedure TStreamingDemo.OnDone(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  FSW.Stop;
end;

// =============================================================================
//  Handlers para Demo 3
// =============================================================================

procedure TStreamingDemo.OnProgressChunk(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  FFullResponse := FFullResponse + aText;
  Inc(FFragCount);
  Write('.');
  if FFragCount mod 40 = 0 then Writeln;
end;

procedure TStreamingDemo.OnProgressDone(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: String);
begin
  Writeln;
  Writeln;
  Writeln('Respuesta completa:');
  Writeln(FFullResponse);
  Writeln(Format('(Total: %d fragmentos)', [FFragCount]));
end;

// =============================================================================
//  DEMO 1 — Sin streaming (bloquea hasta recibir respuesta completa)
// =============================================================================
procedure TStreamingDemo.DemoSinStreaming;
var
  SW      : TStopwatch;
  Response: String;
begin
  Writeln('--- SIN STREAMING ---');
  Writeln('(Espera hasta que la respuesta esté completa antes de mostrarla)');
  Writeln;

  FConn.Messages.Clear;
  // Sin eventos de streaming asignados
  FConn.OnReceiveData    := nil;
  FConn.OnReceiveDataEnd := nil;

  SW := TStopwatch.StartNew;
  Response := FConn.AddMessageAndRun(
    'Escribe un párrafo de 5 oraciones describiendo el ciclo del agua.',
    'user', []);
  SW.Stop;

  Writeln(Response);
  Writeln;
  Writeln(Format('Tiempo total: %d ms', [SW.ElapsedMilliseconds]));
  Writeln;
end;

// =============================================================================
//  DEMO 2 — Con streaming (imprime cada fragmento conforme llega)
// =============================================================================
procedure TStreamingDemo.DemoConStreaming;
var
  SW: TStopwatch;
begin
  Writeln('--- CON STREAMING ---');
  Writeln('(Cada fragmento se imprime conforme llega del servidor)');
  Writeln;

  FConn.Messages.Clear;
  FFirstTokenMs := 0;
  FTokenCount   := 0;
  FSW           := TStopwatch.StartNew;

  FConn.OnReceiveData    := OnChunk;
  FConn.OnReceiveDataEnd := OnDone;

  SW := TStopwatch.StartNew;
  FConn.AddMessageAndRun(
    'Escribe un párrafo de 5 oraciones describiendo el ciclo del agua.',
    'user', []);
  SW.Stop;

  Writeln;
  Writeln;
  Writeln(Format('Tiempo hasta primer token: %d ms', [FFirstTokenMs]));
  Writeln(Format('Tiempo total             : %d ms', [SW.ElapsedMilliseconds]));
  Writeln(Format('Fragmentos recibidos     : %d', [FTokenCount]));
  Writeln;
end;

// =============================================================================
//  DEMO 3 — Streaming con progreso visual (puntos)
// =============================================================================
procedure TStreamingDemo.DemoStreamingConProgreso;
begin
  Writeln('--- STREAMING CON PROGRESO VISUAL ---');
  Writeln('(Los puntos representan fragmentos; la respuesta se muestra al final)');
  Writeln;

  FConn.Messages.Clear;
  FFullResponse := '';
  FFragCount    := 0;

  FConn.OnReceiveData    := OnProgressChunk;
  FConn.OnReceiveDataEnd := OnProgressDone;

  FConn.AddMessageAndRun(
    '¿Cuáles son los 5 planetas más grandes del sistema solar? Lista solo sus nombres.',
    'user', []);
  Writeln;
end;

// =============================================================================
//  Run — punto de entrada del demo
// =============================================================================
procedure TStreamingDemo.Run;
begin
  Writeln('=== Streaming ===');
  Writeln('Driver: ', DRIVER, ' / Model: ', MODEL);
  Writeln;

  FConn := TAiChatConnection.Create(nil);
  try
    FConn.DriverName := DRIVER;
    FConn.Model      := MODEL;

    FConn.Params.Values['ApiKey']       := API_KEY;
    FConn.Params.Values['Asynchronous'] := 'False';
    FConn.Params.Values['Max_Tokens']   := '512';

    FConn.SystemPrompt.Text := 'Responde siempre en español.';

    DemoSinStreaming;
    DemoConStreaming;
    DemoStreamingConProgreso;

  finally
    FConn.Free;
  end;
end;

// =============================================================================
//  Punto de entrada
// =============================================================================
begin
  try
    with TStreamingDemo.Create do
    try
      Run;
    finally
      Free;
    end;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
