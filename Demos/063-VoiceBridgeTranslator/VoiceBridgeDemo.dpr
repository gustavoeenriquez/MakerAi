// PUENTE DE VOZ BIDIRECCIONAL en tiempo real (consola)
// =====================================================
//
// Escenario: estas en una reunion (Zoom/Meet/Teams) con un hablante de ingles.
//
//   [ELLOS] El audio de la reunion (loopback) se transcribe (EN), se traduce
//           al espanol y se REPRODUCE con TTS en tus auriculares.
//   [YO]    Hablas en espanol al microfono; se transcribe, se traduce al
//           ingles y el TTS se REPRODUCE en el cable virtual que la reunion
//           usa como microfono -> el otro lado te escucha en ingles.
//
//   loopback --> STT(en) --> traducir(es) --> TTS --> auriculares (yo escucho)
//   microfono -> STT(es) --> traducir(en) --> TTS --> CABLE Input (reunion escucha)
//
// Configuracion necesaria:
//   1. OPENAI_API_KEY en el entorno (STT + traduccion + TTS).
//   2. VB-CABLE instalado (https://vb-audio.com/Cable/) o cualquier cable
//      virtual. En la reunion seleccionar "CABLE Output" como MICROFONO.
//      Si no hay cable, el demo avisa y reproduce todo en el dispositivo
//      predeterminado (modo prueba).
//   3. AURICULARES: el audio de la reunion y el TTS-es deben salir por
//      auriculares para que el microfono no los capture.
//
// Anti-realimentacion: mientras suena nuestro propio TTS en el dispositivo
// que el loopback captura, la captura se silencia (TAiAudioCapture.Muted)
// para no retraducir nuestra propia voz sintetica.

program VoiceBridgeDemo;

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
  uMakerAi.Realtime.OpenAI,
  uMakerAi.OpenAI.Audio,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.OpenAi; // importa y auto-registra el driver 'OpenAi' (v3.4)

const
  // ==========================================================================
  // CONFIGURACION
  // ==========================================================================
  TARGET_LANG_REMOTE = 'espanol'; // lo que dice el otro lado -> espanol (para mi)
  TARGET_LANG_LOCAL = 'ingles'; // lo que digo yo -> ingles (para la reunion)

  STT_LANG_REMOTE = 'en'; // idioma esperado del otro lado ('' = autodetectar)
  STT_LANG_LOCAL = 'es'; // idioma en el que hablo yo

  TRANSLATOR_DRIVER = 'OpenAi';
  TRANSLATOR_MODEL = 'gpt-4o-mini';
  TRANSLATOR_URL = '';
  TRANSLATOR_APIKEY = '@OPENAI_API_KEY';

  STT_APIKEY = '@OPENAI_API_KEY';
  STT_MODEL = 'gpt-realtime';

  // El TTS de OpenAI con formato trfPcm devuelve PCM16 a 24 kHz mono
  TTS_PCM_RATE = 24000;

  // Subcadena para localizar el cable virtual entre los dispositivos de
  // salida. En la reunion se selecciona el extremo "CABLE Output" como mic.
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
  // Un "lado" del puente: captura + STT + traduccion + TTS + reproduccion
  TBridgeSide = class
  private
    FTag: string;
    FTargetLang: string;
    FClosing: Boolean;
    FCapture: TAiAudioCapture;
    FSTT: TAiOpenAiRealtimeSTT;
    FChat: TAiChatConnection;
    FTTS: TAiOpenAiAudio;
    FPlayer: TAiAudioPlayer;
    FMuteCapture: TAiAudioCapture; // captura a silenciar mientras suena MI TTS
    FChatLock: TCriticalSection;
    procedure SttSessionReady(Sender: TObject);
    procedure SttTranscriptCompleted(Sender: TObject; const Transcript: string; const ItemId: string);
    procedure SttError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure CaptureError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerStateChange(Sender: TObject; aIsPlaying: Boolean);
    procedure TranslateAndSpeak(const aText: string);
  public
    constructor Create(aSource: TAiAudioSource; const aTag, aTargetLang, aSttLanguage: string;
      aVoice: TAiTTSVoice; const aPlayerDeviceId: string);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Capture: TAiAudioCapture read FCapture;
    // Captura que debe silenciarse mientras este lado reproduce su TTS
    // (tipicamente el loopback, si el TTS suena por un dispositivo capturado).
    property MuteCapture: TAiAudioCapture read FMuteCapture write FMuteCapture;
  end;

constructor TBridgeSide.Create(aSource: TAiAudioSource; const aTag, aTargetLang, aSttLanguage: string;
  aVoice: TAiTTSVoice; const aPlayerDeviceId: string);
begin
  inherited Create;
  FTag := aTag;
  FTargetLang := aTargetLang;
  FClosing := False;
  FChatLock := TCriticalSection.Create;

  // --- Traductor ---
  FChat := TAiChatConnection.Create(nil);
  FChat.DriverName := TRANSLATOR_DRIVER;
  FChat.Model := TRANSLATOR_MODEL;
  FChat.Params.Values['Asynchronous'] := 'False';
  if TRANSLATOR_APIKEY <> '' then
    FChat.Params.Values['ApiKey'] := TRANSLATOR_APIKEY;
  if TRANSLATOR_URL <> '' then
    FChat.Params.Values['Url'] := TRANSLATOR_URL;

  // --- TTS ---
  FTTS := TAiOpenAiAudio.Create(nil);
  FTTS.ApiKey := '@OPENAI_API_KEY';
  FTTS.TTSModel := TAiTTSModel.gpt_4o_mini_tts;
  FTTS.TTSVoice := aVoice;
  FTTS.TTSResponseFormat := TAiTTSResponseFormat.trfPcm; // PCM16 24 kHz mono
  FTTS.TTSSpeed := 1.0;

  // --- Reproductor (dispositivo seleccionable) ---
  FPlayer := TAiAudioPlayer.Create(nil);
  FPlayer.DeviceId := aPlayerDeviceId;
  FPlayer.OnError := PlayerError;
  FPlayer.OnStateChange := PlayerStateChange;

  // --- STT en tiempo real ---
  FSTT := TAiOpenAiRealtimeSTT.Create(nil);
  FSTT.ApiKey := STT_APIKEY;
  FSTT.Model := STT_MODEL;
  FSTT.Language := aSttLanguage;
  FSTT.OnSessionReady := SttSessionReady;
  FSTT.OnTranscriptCompleted := SttTranscriptCompleted;
  FSTT.OnError := SttError;

  // --- Captura ---
  FCapture := TAiAudioCapture.Create(nil);
  FCapture.Source := aSource;
  FCapture.OutputSampleRate := 16000;
  FCapture.OutputChannels := 1;
  FCapture.RealtimeSTT := FSTT;
  FCapture.OnError := CaptureError;
end;

destructor TBridgeSide.Destroy;
begin
  Stop;
  FChatLock.Enter;
  try
    FClosing := True;
  finally
    FChatLock.Leave;
  end;
  FCapture.Free;
  FSTT.Free;
  FPlayer.Free;
  FTTS.Free;
  FChat.Free;
  FChatLock.Free;
  inherited;
end;

procedure TBridgeSide.Start;
begin
  FPlayer.Active := True;
  FSTT.Connect;
  FCapture.Active := True;
end;

procedure TBridgeSide.Stop;
begin
  FClosing := True;
  if Assigned(FCapture) then
    FCapture.Active := False;
  if Assigned(FSTT) and FSTT.IsConnected then
    FSTT.Disconnect;
  if Assigned(FPlayer) then
    FPlayer.Active := False;
end;

procedure TBridgeSide.SttSessionReady(Sender: TObject);
begin
  SafeWriteLn(FTag + ' STT conectado y listo.');
end;

procedure TBridgeSide.SttError(Sender: TObject; const ErrorMsg, ErrorCode: string);
begin
  SafeWriteLn(Format('%s ERROR STT [%s]: %s', [FTag, ErrorCode, ErrorMsg]));
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
  // Al terminar de sonar nuestro TTS, reactivar la captura silenciada
  if (not aIsPlaying) and Assigned(FMuteCapture) then
    FMuteCapture.Muted := False;
end;

procedure TBridgeSide.SttTranscriptCompleted(Sender: TObject; const Transcript: string; const ItemId: string);
var
  Text: string;
begin
  Text := Trim(Transcript);
  if Text = '' then
    Exit;
  SafeWriteLn(Format('%s %s', [FTag, Text]));
  TranslateAndSpeak(Text);
end;

procedure TBridgeSide.TranslateAndSpeak(const aText: string);
begin
  TTask.Run(
    procedure
    var
      Prompt, Res: string;
      PcmStream: TMemoryStream;
      Pcm: TBytes;
    begin
      try
        // --- 1) Traducir ---
        FChatLock.Enter;
        try
          if FClosing then
            Exit;
          FChat.NewChat; // cada frase es independiente
          Prompt := Format('Traduce al %s el siguiente texto. Responde UNICAMENTE con la traduccion, ' +
            'sin comentarios ni comillas:'#10'%s', [FTargetLang, aText]);
          Res := FChat.AddMessageAndRun(Prompt, 'user', []);
        finally
          FChatLock.Leave;
        end;
        Res := Trim(Res);
        if Res = '' then
          Exit;
        SafeWriteLn(Format('%s    -> %s', [FTag, Res]));

        // --- 2) TTS ---
        PcmStream := FTTS.Speech(Res); // PCM16 24 kHz mono (trfPcm)
        try
          if (PcmStream = nil) or (PcmStream.Size = 0) then
            Exit;
          SetLength(Pcm, PcmStream.Size);
          PcmStream.Position := 0;
          PcmStream.ReadBuffer(Pcm[0], PcmStream.Size);
        finally
          PcmStream.Free;
        end;

        // --- 3) Reproducir (silenciando la captura que nos escucharia) ---
        if FClosing then
          Exit;
        if Assigned(FMuteCapture) then
          FMuteCapture.Muted := True; // se reactiva en PlayerStateChange
        FPlayer.PlayPCM16(Pcm, TTS_PCM_RATE, 1);
      except
        on E: Exception do
          SafeWriteLn(Format('%s ERROR traduciendo/TTS: %s', [FTag, E.Message]));
      end;
    end);
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
  SetConsoleOutputCP(CP_UTF8); // tildes y caracteres especiales en consola
  StopRequested := False;
  GConsoleLock := TCriticalSection.Create;
  try
    try
      WriteLn('=== Puente de voz bidireccional en tiempo real (MakerAI) ===');
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

        // TRAMPA CONOCIDA: el instalador de VB-CABLE puede dejar "CABLE Input"
        // como dispositivo de salida PREDETERMINADO de Windows. En ese caso todo
        // el audio del sistema (reunion + TTS local) se va al cable y el usuario
        // no escucha nada (ver Docs/Version 3/uMakerAi-AudioBridge.md, seccion 4.2).
        for D in TAiAudioPlayer.GetPlaybackDevices do
          if D.IsDefault and SameText(D.EndpointId, MeetingDeviceId) then
          begin
            WriteLn;
            WriteLn('*** ATENCION: "CABLE Input" es el dispositivo de salida PREDETERMINADO. ***');
            WriteLn('*** Asi NO escucharas ni la reunion ni la traduccion.                  ***');
            WriteLn('*** Ve a Configuracion > Sistema > Sonido y pon tus AURICULARES        ***');
            WriteLn('*** como salida predeterminada antes de continuar.                     ***');
            Break;
          end;
      end
      else
      begin
        WriteLn('AVISO: no se encontro "' + MEETING_DEVICE_HINT + '" (VB-CABLE no instalado?).');
        WriteLn('  MODO PRUEBA: el TTS en ingles sonara por el dispositivo predeterminado');
        WriteLn('  y NO llegara a la reunion. Instala VB-CABLE para el flujo completo.');
        MeetingDeviceId := '';
      end;
      WriteLn;

      // [ELLOS]: loopback -> es; su TTS suena en MIS auriculares (predeterminado),
      // que es el mismo dispositivo que captura el loopback -> hay que silenciarlo
      // mientras suena (anti-realimentacion).
      Remote := TBridgeSide.Create(asLoopback, '[ELLOS]', TARGET_LANG_REMOTE, STT_LANG_REMOTE,
        TAiTTSVoice.tvNova, '');
      Remote.MuteCapture := Remote.Capture;

      // [YO]: microfono -> en; su TTS va al cable virtual (la reunion lo oye).
      Local := TBridgeSide.Create(asMicrophone, '[YO]   ', TARGET_LANG_LOCAL, STT_LANG_LOCAL,
        TAiTTSVoice.tvOnyx, MeetingDeviceId);
      if not HasCable then
        Local.MuteCapture := Remote.Capture; // en modo prueba tambien sonara por el default

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
