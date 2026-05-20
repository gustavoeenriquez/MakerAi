program RealtimeSTT;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 09-Realtime / 01-RealtimeSTT
// =============================================================================
// Transcripcion en tiempo real via OpenAI Realtime API (WebSocket).
//
// Flujo:
//   1. TAIVoiceMonitor captura audio del microfono (PCM16 44100 Hz)
//   2. El audio se reenvía automaticamente a TAiOpenAiRealtimeSTT
//   3. El componente resamplea a 24kHz y lo envía al servidor via WebSocket
//   4. El VAD del servidor detecta inicio/fin de habla
//   5. La transcripcion llega en tiempo real (delta) y como texto final
//
// Controles:
//   - Presionar Enter para detener y salir
//
// Requiere:
//   - Variable de entorno OPENAI_API_KEY configurada
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  uMakerAi.Realtime,
  uMakerAi.Realtime.OpenAI,
  uMakerAi.Utils.VoiceMonitor;

const
  API_KEY = '@OPENAI_API_KEY';
  MODEL   = 'gpt-4o-realtime-preview';

var
  GDone: Boolean = False;

// =============================================================================
//  Objeto demo — agrupa el estado y los handlers de eventos
// =============================================================================

type
  TRealtimeDemo = class
  private
    FSTT:   TAiOpenAiRealtimeSTT;
    FMic:   TAIVoiceMonitor;
    FLine:  string; // acumula el delta actual (se limpia al completar)
    procedure OnConnected(Sender: TObject);
    procedure OnDisconnected(Sender: TObject);
    procedure OnSessionReady(Sender: TObject);
    procedure OnSpeechStarted(Sender: TObject; AudioMs: Int64;
      const ItemId: string);
    procedure OnSpeechStopped(Sender: TObject; AudioMs: Int64;
      const ItemId: string);
    procedure OnTranscriptDelta(Sender: TObject; const Delta: string);
    procedure OnTranscriptCompleted(Sender: TObject;
      const Transcript, ItemId: string);
    procedure OnError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure OnCalibrated(Sender: TObject;
      const NoiseLevel, Sensitivity, StopSensitivity: Integer);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Run;
  end;

constructor TRealtimeDemo.Create;
begin
  inherited;

  // ── Componente STT ──────────────────────────────────────────────────────
  FSTT := TAiOpenAiRealtimeSTT.Create(nil);
  FSTT.ApiKey             := API_KEY;
  FSTT.Model              := MODEL;
  FSTT.Language           := 'es';          // 'es' = español, '' = auto-detect
  FSTT.VADMode            := rvmServerVad;  // VAD en el servidor
  FSTT.SilenceDurationMs  := 600;           // ms de silencio para cortar turno
  FSTT.PrefixPaddingMs    := 300;
  FSTT.NoiseReduction     := rnrNearField;  // headset / auriculares
  FSTT.TranscriptionModel := otmGpt4oTranscribe;

  FSTT.OnConnected         := OnConnected;
  FSTT.OnDisconnected      := OnDisconnected;
  FSTT.OnSessionReady      := OnSessionReady;
  FSTT.OnSpeechStarted     := OnSpeechStarted;
  FSTT.OnSpeechStopped     := OnSpeechStopped;
  FSTT.OnTranscriptDelta   := OnTranscriptDelta;
  FSTT.OnTranscriptCompleted := OnTranscriptCompleted;
  FSTT.OnError             := OnError;

  // ── Microfono ────────────────────────────────────────────────────────────
  FMic := TAIVoiceMonitor.Create(nil);
  FMic.SampleRate         := 44100;
  FMic.SilenceDuration    := 800;
  FMic.RealtimeSTT        := FSTT;  // conectar mic → STT
  FMic.OnCalibrated       := OnCalibrated;

  FLine := '';
end;

destructor TRealtimeDemo.Destroy;
begin
  FMic.Active := False;
  FMic.Free;
  FSTT.Disconnect;
  FSTT.Free;
  inherited;
end;

procedure TRealtimeDemo.Run;
begin
  Writeln('=== MakerAI — Realtime STT (OpenAI) ===');
  Writeln('Modelo : ', MODEL);
  Writeln('Idioma : es (español)');
  Writeln;
  Writeln('Conectando...');

  // Conectar al servidor (async, no bloquea)
  FSTT.Connect;

  // Esperar un momento a que el WebSocket se establezca antes de activar el mic
  // El evento OnSessionReady arranca la captura cuando el servidor confirma sesion
  // (ver OnSessionReady abajo)
end;

{ Handlers de eventos }

procedure TRealtimeDemo.OnConnected(Sender: TObject);
begin
  Writeln('[+] WebSocket conectado');
end;

procedure TRealtimeDemo.OnDisconnected(Sender: TObject);
begin
  Writeln('[x] Desconectado');
end;

procedure TRealtimeDemo.OnSessionReady(Sender: TObject);
begin
  Writeln('[✓] Sesion configurada — iniciando calibracion del microfono...');
  // Activar el mic DESPUES de que el servidor confirme la sesion
  FMic.Active := True;
end;

procedure TRealtimeDemo.OnCalibrated(Sender: TObject;
  const NoiseLevel, Sensitivity, StopSensitivity: Integer);
begin
  Writeln(Format('[mic] Calibrado — ruido=%d  umbral-inicio=%d  umbral-stop=%d',
    [NoiseLevel, Sensitivity, StopSensitivity]));
  Writeln;
  Writeln('Listo. Habla en el microfono. Presiona Enter para salir.');
  Writeln;
end;

procedure TRealtimeDemo.OnSpeechStarted(Sender: TObject; AudioMs: Int64;
  const ItemId: string);
begin
  Write('[habla] ');
  FLine := '';
end;

procedure TRealtimeDemo.OnSpeechStopped(Sender: TObject; AudioMs: Int64;
  const ItemId: string);
begin
  Writeln; // nueva linea al terminar el habla
end;

procedure TRealtimeDemo.OnTranscriptDelta(Sender: TObject; const Delta: string);
begin
  // Escribir el delta sin salto de linea para efecto de escritura en tiempo real
  FLine := FLine + Delta;
  Write(Delta);
end;

procedure TRealtimeDemo.OnTranscriptCompleted(Sender: TObject;
  const Transcript, ItemId: string);
begin
  // Ya mostramos los deltas; aqui solo confirmamos la transcripcion final
  if Transcript <> FLine then
  begin
    // El servidor envio una version diferente al acumulado de deltas
    Writeln;
    Writeln('[ok] ', Transcript);
  end
  else
    Writeln;  // solo salto de linea para separar frases
  FLine := '';
end;

procedure TRealtimeDemo.OnError(Sender: TObject;
  const ErrorMsg, ErrorCode: string);
begin
  Writeln;
  Writeln('[ERROR] (', ErrorCode, ') ', ErrorMsg);
  if ErrorCode = 'connection_failed' then
    GDone := True;
end;

// =============================================================================
//  Main
// =============================================================================

var
  Demo: TRealtimeDemo;

begin
  Demo := TRealtimeDemo.Create;
  try
    Demo.Run;

    // Hilo auxiliar: espera Enter mientras el main pump procesa los callbacks
    TThread.CreateAnonymousThread(procedure begin
      Readln;
      GDone := True;
    end).Start;

    // Pump principal: procesa TThread.Queue callbacks
    while not GDone do
      CheckSynchronize(50);

    Writeln;
    Writeln('Deteniendo...');
  finally
    Demo.Free;
  end;

  Writeln('Listo.');
  Readln;
end.
