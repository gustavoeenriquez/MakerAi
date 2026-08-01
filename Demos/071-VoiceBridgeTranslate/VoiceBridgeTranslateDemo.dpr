// PUENTE DE VOZ BIDIRECCIONAL con gpt-realtime-translate (consola)
// ================================================================
//
// REFACTOR del demo 063: el pipeline STT -> LLM(traducir) -> TTS de cada
// direccion se reemplaza por UN SOLO WebSocket de traduccion de voz
// (TAiOpenAiRealtimeTranslate). El servidor recibe el audio continuo y
// devuelve directamente el texto traducido Y el audio TTS traducido:
//
//   063 (3 saltos):  loopback -> STT -> chat traduce -> TTS -> parlante
//   071 (1 salto):   loopback -> gpt-realtime-translate -> parlante
//
// Ventajas: menos latencia (el audio traducido llega en streaming mientras
// la frase avanza), menos codigo, un solo punto de fallo por direccion.
// Diferencia: la voz TTS la elige el servidor (no hay voz configurable).
//
//   [ELLOS] El audio de la reunion (loopback) se traduce al espanol y suena
//           en tus auriculares (texto + voz).
//   [YO]    Hablas en espanol al microfono; la traduccion al ingles suena en
//           el cable virtual que la reunion usa como microfono.
//
// Configuracion necesaria (igual que 063):
//   1. OPENAI_API_KEY en el entorno.
//   2. VB-CABLE instalado; en la reunion seleccionar "CABLE Output" como mic.
//      Sin cable: modo prueba por el dispositivo predeterminado.
//   3. AURICULARES para que el microfono no capture la reunion.
//
// Anti-realimentacion: mientras suena el audio traducido en el dispositivo
// que el loopback captura, la captura se silencia (TAiAudioCapture.Muted).

program VoiceBridgeTranslateDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Threading,
  Winapi.Windows,
  uMakerAi.Utils.AudioCapture,
  uMakerAi.Utils.AudioPlayback,
  uMakerAi.Realtime,
  uMakerAi.Realtime.OpenAI;

const
  // ==========================================================================
  // CONFIGURACION
  // ==========================================================================
  TARGET_LANG_REMOTE = 'es'; // lo que dice el otro lado -> espanol (para mi)
  TARGET_LANG_LOCAL  = 'en'; // lo que digo yo -> ingles (para la reunion)

  TRANSLATE_APIKEY = '@OPENAI_API_KEY';

  // El audio traducido de gpt-realtime-translate es PCM16 24 kHz mono
  TR_PCM_RATE = 24000;

  MEETING_DEVICE_HINT = 'CABLE Input';

var
  GConsoleLock: TCriticalSection;

procedure SafeWriteLn(const aText: string);
begin
  GConsoleLock.Enter;
  try
    WriteLn(aText);
  finally
    GConsoleLock.Leave;
  end;
end;

type
  // Un "lado" del puente: captura -> traduccion streaming -> reproduccion
  TBridgeSide = class
  private
    FTag: string;
    FClosing: Boolean;
    FCapture: TAiAudioCapture;
    FTr: TAiOpenAiRealtimeTranslate;
    FPlayer: TAiAudioPlayer;
    FMuteCapture: TAiAudioCapture; // captura a silenciar mientras suena MI audio
    FSrcText: string;              // linea en curso del idioma origen
    FDstText: string;              // linea en curso de la traduccion
    procedure TrSessionReady(Sender: TObject);
    procedure TrSourceDelta(Sender: TObject; const Delta: string);
    procedure TrTranslatedDelta(Sender: TObject; const Delta: string);
    procedure TrAudioChunk(Sender: TObject; const AData: TBytes);
    procedure TrError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure CaptureError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerStateChange(Sender: TObject; aIsPlaying: Boolean);
    procedure FlushIfSentence(var aLine: string; const aPrefix: string);
  public
    constructor Create(aSource: TAiAudioSource;
      const aTag, aTargetLang, aPlayerDeviceId: string);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Capture: TAiAudioCapture read FCapture;
    property MuteCapture: TAiAudioCapture read FMuteCapture write FMuteCapture;
  end;

constructor TBridgeSide.Create(aSource: TAiAudioSource;
  const aTag, aTargetLang, aPlayerDeviceId: string);
begin
  inherited Create;
  FTag := aTag;
  FClosing := False;

  // --- Traduccion de voz en streaming (1 socket por direccion) ---
  FTr := TAiOpenAiRealtimeTranslate.Create(nil);
  FTr.ApiKey := TRANSLATE_APIKEY;
  FTr.TargetLanguage := aTargetLang;
  FTr.SourceTranscription := True; // mostrar tambien lo que se escucho
  FTr.InputSampleRate := 16000;    // la captura entrega 16 kHz; el driver resamplea
  FTr.OnSessionReady := TrSessionReady;
  FTr.OnTranscriptDelta := TrSourceDelta;
  FTr.OnAssistantTextDelta := TrTranslatedDelta;
  FTr.OnAudioChunk := TrAudioChunk;
  FTr.OnError := TrError;

  // --- Reproductor (dispositivo seleccionable) ---
  FPlayer := TAiAudioPlayer.Create(nil);
  FPlayer.DeviceId := aPlayerDeviceId;
  FPlayer.OnError := PlayerError;
  FPlayer.OnStateChange := PlayerStateChange;

  // --- Captura ---
  FCapture := TAiAudioCapture.Create(nil);
  FCapture.Source := aSource;
  FCapture.OutputSampleRate := 16000;
  FCapture.OutputChannels := 1;
  FCapture.RealtimeSTT := FTr; // cada chunk PCM16 se reenvia al traductor
  FCapture.OnError := CaptureError;
end;

destructor TBridgeSide.Destroy;
begin
  Stop;
  FCapture.Free;
  FTr.Free;
  FPlayer.Free;
  inherited;
end;

procedure TBridgeSide.Start;
begin
  FPlayer.Active := True;
  FTr.Connect;              // asincrono; los chunks fluyen al conectar
  FCapture.Active := True;
end;

procedure TBridgeSide.Stop;
begin
  FClosing := True;
  if Assigned(FCapture) then
    FCapture.Active := False;
  if Assigned(FTr) and FTr.IsConnected then
    FTr.Disconnect;         // envia session.close antes de cerrar
  if Assigned(FPlayer) then
    FPlayer.Active := False;
end;

procedure TBridgeSide.TrSessionReady(Sender: TObject);
begin
  SafeWriteLn(FTag + ' traductor conectado (gpt-realtime-translate).');
end;

procedure TBridgeSide.TrError(Sender: TObject; const ErrorMsg, ErrorCode: string);
begin
  SafeWriteLn(Format('%s ERROR traductor [%s]: %s', [FTag, ErrorCode, ErrorMsg]));
end;

procedure TBridgeSide.CaptureError(Sender: TObject; const ErrorMessage: string);
begin
  SafeWriteLn(FTag + ' ERROR captura: ' + ErrorMessage);
end;

procedure TBridgeSide.PlayerError(Sender: TObject; const ErrorMessage: string);
begin
  SafeWriteLn(FTag + ' ERROR reproduccion: ' + ErrorMessage);
end;

procedure TBridgeSide.PlayerStateChange(Sender: TObject; aIsPlaying: Boolean);
begin
  // Al vaciarse la cola de reproduccion, reactivar la captura silenciada
  if (not aIsPlaying) and Assigned(FMuteCapture) then
    FMuteCapture.Muted := False;
end;

// Acumula deltas y escribe la linea completa al cerrar la frase
// (puntuacion final) o si crece demasiado.
procedure TBridgeSide.FlushIfSentence(var aLine: string; const aPrefix: string);
var
  LastCh: Char;
begin
  if aLine = '' then Exit;
  LastCh := aLine[High(aLine)];
  if CharInSet(LastCh, ['.', '?', '!']) or (Length(aLine) > 160) then
  begin
    SafeWriteLn(aPrefix + Trim(aLine));
    aLine := '';
  end;
end;

procedure TBridgeSide.TrSourceDelta(Sender: TObject; const Delta: string);
begin
  FSrcText := FSrcText + Delta;
  FlushIfSentence(FSrcText, FTag + ' ');
end;

procedure TBridgeSide.TrTranslatedDelta(Sender: TObject; const Delta: string);
begin
  FDstText := FDstText + Delta;
  FlushIfSentence(FDstText, FTag + '    -> ');
end;

procedure TBridgeSide.TrAudioChunk(Sender: TObject; const AData: TBytes);
begin
  if FClosing then Exit;
  // Silenciar la captura que nos escucharia mientras suena la traduccion;
  // se reactiva en PlayerStateChange cuando la cola queda vacia
  if Assigned(FMuteCapture) then
    FMuteCapture.Muted := True;
  FPlayer.PlayPCM16(AData, TR_PCM_RATE, 1);
end;

// ============================================================================

function FindMeetingDevice(out aDeviceId, aDeviceName: string): Boolean;
var
  D: TAiAudioDeviceInfo;
begin
  Result := False;
  aDeviceId := '';
  aDeviceName := '';
  for D in TAiAudioPlayer.GetPlaybackDevices do
    if Pos(LowerCase(MEETING_DEVICE_HINT), LowerCase(D.DeviceName)) > 0 then
    begin
      aDeviceId := D.EndpointId;
      aDeviceName := D.DeviceName;
      Exit(True);
    end;
end;

var
  Remote, Local: TBridgeSide;
  StopRequested: Boolean;
  MeetingDeviceId, MeetingDeviceName: string;
  HasCable: Boolean;
  D: TAiAudioDeviceInfo;

begin
  SetConsoleOutputCP(CP_UTF8);
  StopRequested := False;
  GConsoleLock := TCriticalSection.Create;
  try
    try
      WriteLn('=== Puente de voz con gpt-realtime-translate (MakerAI) ===');
      WriteLn('    (1 WebSocket por direccion: audio -> texto + voz traducidos)');
      WriteLn;

      if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
      begin
        WriteLn('ERROR: la variable de entorno OPENAI_API_KEY no esta definida.');
        Exit;
      end;

      WriteLn('Dispositivos de salida:');
      for D in TAiAudioPlayer.GetPlaybackDevices do
        if D.IsDefault then
          WriteLn('  * ' + D.DeviceName + '  (predeterminado)')
        else
          WriteLn('  - ' + D.DeviceName);
      WriteLn;

      HasCable := FindMeetingDevice(MeetingDeviceId, MeetingDeviceName);
      if HasCable then
      begin
        WriteLn('Cable virtual encontrado: ' + MeetingDeviceName);
        WriteLn('  -> En la reunion selecciona "CABLE Output" como MICROFONO.');
        // Ver demo 063 / uMakerAi-AudioBridge.md: si "CABLE Input" quedo como
        // salida predeterminada de Windows no escucharas nada
        for D in TAiAudioPlayer.GetPlaybackDevices do
          if D.IsDefault and SameText(D.EndpointId, MeetingDeviceId) then
          begin
            WriteLn;
            WriteLn('*** ATENCION: "CABLE Input" es la salida PREDETERMINADA de Windows. ***');
            WriteLn('*** Pon tus AURICULARES como salida predeterminada antes de seguir. ***');
            Break;
          end;
      end
      else
      begin
        WriteLn('AVISO: no se encontro "' + MEETING_DEVICE_HINT + '" (VB-CABLE no instalado?).');
        WriteLn('  MODO PRUEBA: la traduccion al ingles sonara por el dispositivo');
        WriteLn('  predeterminado y NO llegara a la reunion.');
        MeetingDeviceId := '';
      end;
      WriteLn;

      // [ELLOS]: loopback -> es; el audio traducido suena en MIS auriculares
      // (mismo dispositivo que captura el loopback -> silenciar mientras suena)
      Remote := TBridgeSide.Create(asLoopback, '[ELLOS]', TARGET_LANG_REMOTE, '');
      Remote.MuteCapture := Remote.Capture;

      // [YO]: microfono -> en; el audio traducido va al cable virtual
      Local := TBridgeSide.Create(asMicrophone, '[YO]   ', TARGET_LANG_LOCAL,
        MeetingDeviceId);
      if not HasCable then
        Local.MuteCapture := Remote.Capture; // en modo prueba suena por el default

      try
        Remote.Start;
        Local.Start;

        WriteLn('Puente activo... pulsa ENTER para terminar.');
        WriteLn;

        TTask.Run(
          procedure
          begin
            ReadLn;
            StopRequested := True;
          end);

        while not StopRequested do
          CheckSynchronize(50);

        WriteLn('Deteniendo...');
        Remote.Stop;
        Local.Stop;
        CheckSynchronize(200);
      finally
        Remote.Free;
        Local.Free;
      end;
    except
      on E: Exception do
        WriteLn(E.ClassName, ': ', E.Message);
    end;
  finally
    GConsoleLock.Free;
  end;

end.
