// MakerAI Suite — Conector universal Realtime STT
// Permite cambiar de provider en tiempo de diseno sin modificar el codigo.
// Equivalente a TAiChatConnection pero para el modulo Realtime.
//
// Uso tipico:
//   RealtimeConn.DriverName := 'OpenAI';
//   RealtimeConn.ApiKey     := '@OPENAI_API_KEY';
//   RealtimeConn.Model      := 'gpt-4o-realtime-preview';
//   VoiceMonitor.RealtimeSTT := RealtimeConn;
//   RealtimeConn.Connect;
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.AiConnection;

interface

uses
  System.SysUtils, System.Classes,
  uMakerAi.Realtime;

type
  TAiRealtimeConnection = class(TAiRealtimeBase)
  private
    FInstance:   TAiRealtimeBase;
    FDriverName: string;
    procedure SetDriverName(const Value: string);
    procedure RecreateInstance;
    procedure SyncToInstance;
    // Handlers que reenvian los eventos de FInstance a Self
    procedure OnInstConnected(Sender: TObject);
    procedure OnInstDisconnected(Sender: TObject);
    procedure OnInstSessionReady(Sender: TObject);
    procedure OnInstSpeechStarted(Sender: TObject; AudioMs: Int64; const ItemId: string);
    procedure OnInstSpeechStopped(Sender: TObject; AudioMs: Int64; const ItemId: string);
    procedure OnInstTranscriptDelta(Sender: TObject; const Delta: string);
    procedure OnInstTranscriptCompleted(Sender: TObject; const Transcript, ItemId: string);
    procedure OnInstError(Sender: TObject; const ErrorMsg, ErrorCode: string);
  protected
    function  GetTargetSampleRate: Integer; override;
    procedure InternalSendAudio(const ResampledPCM16: TBytes); override;
    procedure InternalConnect;    override;
    procedure InternalDisconnect; override;
    procedure InternalCommitAudio; override;
    procedure InternalClearAudio;  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
    function  GetModels: TArray<string>;
    procedure SendAudioChunk(const PCM16Data: TBytes); override;
    property  Instance: TAiRealtimeBase read FInstance;
  published
    // Al cambiar DriverName se crea/destruye la instancia interna
    property DriverName: string read FDriverName write SetDriverName;
  end;

implementation

{ TAiRealtimeConnection }

constructor TAiRealtimeConnection.Create(AOwner: TComponent);
begin
  inherited;
  FInstance   := nil;
  FDriverName := '';
end;

destructor TAiRealtimeConnection.Destroy;
begin
  FreeAndNil(FInstance);
  inherited;
end;

class function TAiRealtimeConnection.GetDriverName: string;
begin
  Result := 'Connection';
end;

class function TAiRealtimeConnection.GetDefaultModel: string;
begin
  Result := '';
end;

procedure TAiRealtimeConnection.SetDriverName(const Value: string);
begin
  if FDriverName = Value then Exit;
  FDriverName := Value;
  RecreateInstance;
end;

procedure TAiRealtimeConnection.RecreateInstance;
begin
  if IsConnected and Assigned(FInstance) then
    FInstance.Disconnect;
  FreeAndNil(FInstance);
  if FDriverName = '' then Exit;
  try
    FInstance := TAiRealtimeFactory.Instance.CreateDriver(FDriverName, Self);
    // Cablear todos los eventos al reenviador
    FInstance.OnConnected         := OnInstConnected;
    FInstance.OnDisconnected      := OnInstDisconnected;
    FInstance.OnSessionReady      := OnInstSessionReady;
    FInstance.OnSpeechStarted     := OnInstSpeechStarted;
    FInstance.OnSpeechStopped     := OnInstSpeechStopped;
    FInstance.OnTranscriptDelta   := OnInstTranscriptDelta;
    FInstance.OnTranscriptCompleted := OnInstTranscriptCompleted;
    FInstance.OnError             := OnInstError;
    SyncToInstance;
  except
    on E: Exception do
      DoError(E.Message, 'driver_not_found');
  end;
end;

procedure TAiRealtimeConnection.SyncToInstance;
begin
  if not Assigned(FInstance) then Exit;
  FInstance.ApiKey            := ApiKey;
  FInstance.Model             := Model;
  FInstance.Language          := Language;
  FInstance.InputSampleRate   := InputSampleRate;
  FInstance.VADMode           := VADMode;
  FInstance.VADThreshold      := VADThreshold;
  FInstance.SilenceDurationMs := SilenceDurationMs;
  FInstance.PrefixPaddingMs   := PrefixPaddingMs;
  FInstance.NoiseReduction    := NoiseReduction;
end;

function TAiRealtimeConnection.GetModels: TArray<string>;
begin
  SetLength(Result, 0);
end;

{ Metodos abstractos — delegan en FInstance }

function TAiRealtimeConnection.GetTargetSampleRate: Integer;
begin
  if Assigned(FInstance) then
    Result := FInstance.TargetSampleRate
  else
    Result := 24000;
end;

procedure TAiRealtimeConnection.InternalConnect;
begin
  if not Assigned(FInstance) then
    raise EInvalidOperation.Create(
      'TAiRealtimeConnection: DriverName no esta configurado');
  SyncToInstance;
  FInstance.Connect;
end;

procedure TAiRealtimeConnection.InternalDisconnect;
begin
  if Assigned(FInstance) then
    FInstance.Disconnect;
end;

procedure TAiRealtimeConnection.InternalSendAudio(const ResampledPCM16: TBytes);
begin
  // No se usa: SendAudioChunk va directo a FInstance.SendAudioChunk
  // para evitar doble resampling
end;

procedure TAiRealtimeConnection.SendAudioChunk(const PCM16Data: TBytes);
begin
  if Assigned(FInstance) then
    FInstance.SendAudioChunk(PCM16Data);
end;

procedure TAiRealtimeConnection.InternalCommitAudio;
begin
  if Assigned(FInstance) then FInstance.CommitAudio;
end;

procedure TAiRealtimeConnection.InternalClearAudio;
begin
  if Assigned(FInstance) then FInstance.ClearAudio;
end;

{ Reenviadores de eventos de FInstance a Self }

procedure TAiRealtimeConnection.OnInstConnected(Sender: TObject);
begin
  DoConnected;
end;

procedure TAiRealtimeConnection.OnInstDisconnected(Sender: TObject);
begin
  DoDisconnected;
end;

procedure TAiRealtimeConnection.OnInstSessionReady(Sender: TObject);
begin
  DoSessionReady;
end;

procedure TAiRealtimeConnection.OnInstSpeechStarted(Sender: TObject;
  AudioMs: Int64; const ItemId: string);
begin
  DoSpeechStarted(AudioMs, ItemId);
end;

procedure TAiRealtimeConnection.OnInstSpeechStopped(Sender: TObject;
  AudioMs: Int64; const ItemId: string);
begin
  DoSpeechStopped(AudioMs, ItemId);
end;

procedure TAiRealtimeConnection.OnInstTranscriptDelta(Sender: TObject;
  const Delta: string);
begin
  DoTranscriptDelta(Delta);
end;

procedure TAiRealtimeConnection.OnInstTranscriptCompleted(Sender: TObject;
  const Transcript, ItemId: string);
begin
  DoTranscriptCompleted(Transcript, ItemId);
end;

procedure TAiRealtimeConnection.OnInstError(Sender: TObject;
  const ErrorMsg, ErrorCode: string);
begin
  DoError(ErrorMsg, ErrorCode);
end;

end.
