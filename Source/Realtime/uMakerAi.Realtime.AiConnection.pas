// MakerAI Suite — Conector universal Realtime STT
// Permite cambiar de provider en tiempo de diseno sin modificar el codigo.
// Equivalente a TAiChatConnection pero para el modulo Realtime.
//
// Uso tipico:
//   RealtimeConn.DriverName := 'OpenAI';
//   RealtimeConn.ApiKey     := '@OPENAI_API_KEY';
//   RealtimeConn.Model      := 'gpt-realtime-2.1';
//   VoiceMonitor.RealtimeSTT := RealtimeConn;
//   RealtimeConn.Connect;
//
// Propiedades propias de cada driver (voz, instrucciones, idioma destino...):
// DriverParams, una por linea 'Propiedad=Valor', se aplica al driver por RTTI
// al crearlo y al conectar:
//   RealtimeConn.DriverParams.Values['Voice'] := 'Tina';
//   RealtimeConn.DriverParams.Values['Instructions'] := 'Responde breve';
// Lo que no se expresa como texto (AiFunctions, eventos propios) va por
// (RealtimeConn.Instance as TAiGrokRealtimeChat).
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.AiConnection;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.TypInfo,
  uMakerAi.Realtime;

type
  // Hereda de TAiRealtimeVoiceBase para re-exponer tambien los eventos de
  // los drivers de voz full-duplex (OnAssistantText, OnAudioChunk, ...).
  // Con drivers STT puros esos eventos simplemente nunca se disparan.
  TAiRealtimeConnection = class(TAiRealtimeVoiceBase)
  private
    FInstance:   TAiRealtimeBase;
    FDriverName: string;
    FDriverParams: TStrings;
    procedure SetDriverName(const Value: string);
    procedure SetDriverParams(const Value: TStrings);
    procedure RecreateInstance;
    // AReportUnknown: al conectar, una clave que el driver no tiene se informa por
    // OnError (al crear el driver no: DriverParams puede traer claves de otro)
    procedure SyncToInstance(AReportUnknown: Boolean = False);
    procedure ApplyDriverParams(AReportUnknown: Boolean);
    // Handlers que reenvian los eventos de FInstance a Self
    procedure OnInstConnected(Sender: TObject);
    procedure OnInstDisconnected(Sender: TObject);
    procedure OnInstSessionReady(Sender: TObject);
    procedure OnInstSpeechStarted(Sender: TObject; AudioMs: Int64; const ItemId: string);
    procedure OnInstSpeechStopped(Sender: TObject; AudioMs: Int64; const ItemId: string);
    procedure OnInstTranscriptDelta(Sender: TObject; const Delta: string);
    procedure OnInstTranscriptCompleted(Sender: TObject; const Transcript, ItemId: string);
    procedure OnInstError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    // Reenviadores de los eventos de voz (solo drivers TAiRealtimeVoiceBase)
    procedure OnInstAssistantText(Sender: TObject; const AText: string);
    procedure OnInstAssistantTextDelta(Sender: TObject; const ADelta: string);
    procedure OnInstAudioChunk(Sender: TObject; const AData: TBytes);
    procedure OnInstAudioDone(Sender: TObject);
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
    // Propiedades propias del driver, 'Propiedad=Valor' por linea: Voice,
    // Instructions, TargetLanguage, Mode, ReasoningEffort... Texto, numeros,
    // enumerados por nombre (greNone, tmCommit), booleanos y listas (TStrings)
    // con elementos separados por '|'
    property DriverParams: TStrings read FDriverParams write SetDriverParams;
  end;

implementation

{ TAiRealtimeConnection }

constructor TAiRealtimeConnection.Create(AOwner: TComponent);
begin
  inherited;
  FInstance   := nil;
  FDriverName := '';
  FDriverParams := TStringList.Create;
end;

destructor TAiRealtimeConnection.Destroy;
begin
  FreeAndNil(FInstance);
  FDriverParams.Free;
  inherited;
end;

procedure TAiRealtimeConnection.SetDriverParams(const Value: TStrings);
begin
  FDriverParams.Assign(Value);
end;

procedure TAiRealtimeConnection.ApplyDriverParams(AReportUnknown: Boolean);
var
  Ctx: TRttiContext;
  T: TRttiType;
  P: TRttiProperty;
  I, E: Integer;
  LName, LVal: string;
  LInt: Int64;
  LFloat: Double;
  LObj: TObject;
  LOk: Boolean;
begin
  if not Assigned(FInstance) or (FDriverParams.Count = 0) then Exit;
  Ctx := TRttiContext.Create;
  try
    T := Ctx.GetType(FInstance.ClassType);
    for I := 0 to FDriverParams.Count - 1 do
    begin
      LName := Trim(FDriverParams.Names[I]);
      if LName = '' then Continue;
      LVal := Trim(FDriverParams.ValueFromIndex[I]);
      P := T.GetProperty(LName);
      LOk := False;
      if Assigned(P) then
        case P.PropertyType.TypeKind of
          tkUString, tkString, tkWString, tkLString:
            if P.IsWritable then
            begin
              P.SetValue(FInstance, LVal);
              LOk := True;
            end;
          tkInteger, tkInt64:
            if P.IsWritable and TryStrToInt64(LVal, LInt) then
            begin
              if P.PropertyType.TypeKind = tkInteger then
                P.SetValue(FInstance, TValue.From<Integer>(Integer(LInt)))
              else
                P.SetValue(FInstance, LInt);
              LOk := True;
            end;
          tkFloat:
            if P.IsWritable and TryStrToFloat(LVal, LFloat, TFormatSettings.Invariant) then
            begin
              P.SetValue(FInstance, LFloat);
              LOk := True;
            end;
          tkEnumeration:
            if P.IsWritable then
            begin
              if P.PropertyType.Handle = TypeInfo(Boolean) then
              begin
                P.SetValue(FInstance, SameText(LVal, 'true') or (LVal = '1'));
                LOk := True;
              end
              else
              begin
                E := GetEnumValue(P.PropertyType.Handle, LVal);
                if E >= 0 then
                begin
                  P.SetValue(FInstance, TValue.FromOrdinal(P.PropertyType.Handle, E));
                  LOk := True;
                end;
              end;
            end;
          tkClass:
            begin
              // TStrings (Keyterms, CustomToolsJson...): elementos separados por '|'
              LObj := P.GetValue(FInstance).AsObject;
              if LObj is TStrings then
              begin
                TStrings(LObj).Text := StringReplace(LVal, '|', sLineBreak, [rfReplaceAll]);
                LOk := True;
              end;
            end;
        end;
      if not LOk and AReportUnknown then
        DoError(Format('DriverParams: el driver %s no tiene la propiedad "%s" o el valor "%s" no es valido',
          [FDriverName, LName, LVal]), 'driver_param');
    end;
  finally
    Ctx.Free;
  end;
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
    // Eventos de voz full-duplex (MakerAi, Grok, ...)
    if FInstance is TAiRealtimeVoiceBase then
    begin
      TAiRealtimeVoiceBase(FInstance).OnAssistantText      := OnInstAssistantText;
      TAiRealtimeVoiceBase(FInstance).OnAssistantTextDelta := OnInstAssistantTextDelta;
      TAiRealtimeVoiceBase(FInstance).OnAudioChunk         := OnInstAudioChunk;
      TAiRealtimeVoiceBase(FInstance).OnAudioDone          := OnInstAudioDone;
    end;
    SyncToInstance;
  except
    on E: Exception do
      DoError(E.Message, 'driver_not_found');
  end;
end;

procedure TAiRealtimeConnection.SyncToInstance(AReportUnknown: Boolean);
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
  ApplyDriverParams(AReportUnknown);
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
  SyncToInstance(True);
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

procedure TAiRealtimeConnection.OnInstAssistantText(Sender: TObject;
  const AText: string);
begin
  DoAssistantText(AText);
end;

procedure TAiRealtimeConnection.OnInstAssistantTextDelta(Sender: TObject;
  const ADelta: string);
begin
  DoAssistantTextDelta(ADelta);
end;

procedure TAiRealtimeConnection.OnInstAudioChunk(Sender: TObject;
  const AData: TBytes);
begin
  DoAudioChunk(AData);
end;

procedure TAiRealtimeConnection.OnInstAudioDone(Sender: TObject);
begin
  DoAudioDone;
end;

end.
