// PUENTE DE VOZ BIDIRECCIONAL CON DIARIZACION (consola)
// =======================================================
//
// Evolucion del demo 063: el canal [ELLOS] identifica QUIEN habla en la
// reunion (diarizacion con gpt-4o-transcribe-diarize) y cada hablante se
// traduce y se lee con una VOZ TTS DISTINTA.
//
//   [ELLOS] loopback -> VAD local (segmentos de habla)
//                    -> Transcribe diarizado (gpt-4o-transcribe-diarize)
//                    -> auto-registro de hablantes (known_speakers, max 4)
//                    -> traducir(es) por hablante -> TTS (voz por hablante)
//                    -> auriculares
//
//   [YO]    microfono -> Realtime STT (baja latencia, un solo hablante)
//                     -> traducir(en) -> TTS -> CABLE Input (la reunion escucha)
//
// AUTO-REGISTRO DE HABLANTES: las etiquetas A/B/C de la diarizacion solo son
// consistentes dentro de una misma peticion. La primera vez que aparece una
// voz nueva, se recorta su audio del segmento (>= 2 s) y se registra con
// AddKnownSpeaker como 'Hablante N'; a partir de ahi la API devuelve ese
// nombre de forma estable en todos los segmentos siguientes.
//
// Requisitos: OPENAI_API_KEY, VB-CABLE (opcional, ver demo 063), auriculares.
// Documentacion: Docs/Version 3/uMakerAi-AudioBridge.md

program VoiceBridgeDiarizedDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Threading,
  System.Generics.Collections,
  System.Math,
  Winapi.Windows,
  uMakerAi.Utils.AudioCapture,
  uMakerAi.Utils.AudioPlayback,
  uMakerAi.Utils.PcmToWav,
  uMakerAi.Core,
  uMakerAi.OpenAI.Audio,
  uMakerAi.Realtime,
  uMakerAi.Realtime.OpenAI,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.OpenAi;

const
  // ==========================================================================
  // CONFIGURACION
  // ==========================================================================
  TARGET_LANG_REMOTE = 'espanol';
  TARGET_LANG_LOCAL = 'ingles';
  STT_LANG_LOCAL = 'es';

  TRANSLATOR_DRIVER = 'OpenAi';
  TRANSLATOR_MODEL = 'gpt-4o-mini';
  TRANSLATOR_APIKEY = '@OPENAI_API_KEY';

  STT_APIKEY = '@OPENAI_API_KEY';
  STT_MODEL = 'gpt-realtime-2.1';

  TTS_PCM_RATE = 24000;
  MEETING_DEVICE_HINT = 'CABLE Input';

  // --- Segmentador VAD local del canal [ELLOS] (PCM16 16 kHz mono) ---
  SEG_SAMPLE_RATE = 16000;
  SEG_START_LEVEL = 400; // nivel medio para considerar inicio de habla
  SEG_STOP_LEVEL = 250; // por debajo se acumula silencio
  SEG_SILENCE_MS = 900; // silencio que cierra el segmento
  SEG_MIN_MS = 700; // segmentos mas cortos se descartan
  SEG_MAX_MS = 25000; // corte forzado de segmentos largos
  SEG_PREBUF_MS = 400; // pre-buffer para no perder el arranque de la frase

  // Voz TTS por hablante registrado (Hablante 1..4)
  SPEAKER_VOICES: array [0 .. 3] of TAiTTSVoice = (TAiTTSVoice.tvNova, TAiTTSVoice.tvShimmer,
    TAiTTSVoice.tvCoral, TAiTTSVoice.tvSage);

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
  // --------------------------------------------------------------------------
  // Segmentador de habla por nivel (VAD local) sobre chunks PCM16 mono.
  // Alimentado desde OnData (hilo principal); dispara OnSegment con el PCM
  // completo de cada frase detectada.
  // --------------------------------------------------------------------------
  TSpeechSegmenter = class
  private
    FBuffer: TMemoryStream; // segmento en curso
    FPreBuffer: TMemoryStream; // ultimos SEG_PREBUF_MS de silencio
    FInSpeech: Boolean;
    FSilenceMs: Integer;
    FBytesPerMs: Integer;
    FOnSegment: TProc<TBytes>;
    procedure CloseSegment;
  public
    constructor Create(aOnSegment: TProc<TBytes>);
    destructor Destroy; override;
    procedure Feed(const aChunk: TBytes; aChunkMs: Integer);
  end;

constructor TSpeechSegmenter.Create(aOnSegment: TProc<TBytes>);
begin
  inherited Create;
  FOnSegment := aOnSegment;
  FBuffer := TMemoryStream.Create;
  FPreBuffer := TMemoryStream.Create;
  FBytesPerMs := (SEG_SAMPLE_RATE * 2) div 1000;
end;

destructor TSpeechSegmenter.Destroy;
begin
  FBuffer.Free;
  FPreBuffer.Free;
  inherited;
end;

procedure TSpeechSegmenter.CloseSegment;
var
  Pcm: TBytes;
begin
  if FBuffer.Size >= SEG_MIN_MS * FBytesPerMs then
  begin
    SetLength(Pcm, FBuffer.Size);
    FBuffer.Position := 0;
    FBuffer.ReadBuffer(Pcm[0], FBuffer.Size);
    if Assigned(FOnSegment) then
      FOnSegment(Pcm);
  end;
  FBuffer.Clear;
  FPreBuffer.Clear;
  FInSpeech := False;
  FSilenceMs := 0;
end;

procedure TSpeechSegmenter.Feed(const aChunk: TBytes; aChunkMs: Integer);
var
  I, NumSamples: Integer;
  Sum, Level: Int64;
  MaxPre: Int64;
  Tmp: TMemoryStream;
begin
  if Length(aChunk) = 0 then
    Exit;

  // Nivel medio del chunk
  NumSamples := Length(aChunk) div 2;
  Sum := 0;
  for I := 0 to NumSamples - 1 do
    Sum := Sum + Abs(PSmallInt(@aChunk[I * 2])^);
  Level := Sum div NumSamples;

  if not FInSpeech then
  begin
    // Mantener pre-buffer rodante
    FPreBuffer.Position := FPreBuffer.Size;
    FPreBuffer.WriteBuffer(aChunk[0], Length(aChunk));
    MaxPre := SEG_PREBUF_MS * FBytesPerMs;
    if FPreBuffer.Size > MaxPre then
    begin
      Tmp := TMemoryStream.Create;
      try
        FPreBuffer.Position := FPreBuffer.Size - MaxPre;
        Tmp.CopyFrom(FPreBuffer, MaxPre);
        FPreBuffer.Clear;
        Tmp.Position := 0;
        FPreBuffer.CopyFrom(Tmp, 0);
      finally
        Tmp.Free;
      end;
    end;

    if Level >= SEG_START_LEVEL then
    begin
      // Inicio de habla: el segmento arranca con el pre-buffer
      FInSpeech := True;
      FSilenceMs := 0;
      FBuffer.Clear;
      FPreBuffer.Position := 0;
      FBuffer.CopyFrom(FPreBuffer, 0);
      FPreBuffer.Clear;
    end;
  end
  else
  begin
    FBuffer.Position := FBuffer.Size;
    FBuffer.WriteBuffer(aChunk[0], Length(aChunk));

    if Level < SEG_STOP_LEVEL then
      Inc(FSilenceMs, aChunkMs)
    else
      FSilenceMs := 0;

    if (FSilenceMs >= SEG_SILENCE_MS) or (FBuffer.Size >= SEG_MAX_MS * FBytesPerMs) then
      CloseSegment;
  end;
end;

type
  // --------------------------------------------------------------------------
  // Canal [ELLOS]: loopback -> VAD -> diarizacion -> traduccion -> TTS
  // --------------------------------------------------------------------------
  TDiarizedRemoteSide = class
  private
    FTag: string;
    FClosing: Boolean;
    FCapture: TAiAudioCapture;
    FChat: TAiChatConnection;
    FAudioApi: TAiOpenAiAudio; // diarizacion + TTS
    FPlayer: TAiAudioPlayer;
    FSegmenter: TSpeechSegmenter;
    FLock: TCriticalSection; // serializa diarizar/traducir/TTS
    FSpeakerNames: TArray<string>; // 'Hablante 1'.. en orden de registro
    procedure CaptureData(Sender: TObject; const aBuffer: TBytes; aSampleRate, aChannels: Integer);
    procedure CaptureError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerStateChange(Sender: TObject; aIsPlaying: Boolean);
    procedure ProcessSegment(const aPcm: TBytes);
    procedure ProcessSegmentWork(const aPcm: TBytes);
    function SpeakerIndex(const aName: string): Integer;
    function TryEnrollSpeaker(const aPcm: TBytes; const aRawSpeaker: string;
      const aSegments: TDiarizedSegments; out aName: string): Boolean;
    function Translate(const aText: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Capture: TAiAudioCapture read FCapture;
  end;

constructor TDiarizedRemoteSide.Create;
begin
  inherited Create;
  FTag := '[ELLOS]';
  FLock := TCriticalSection.Create;

  FChat := TAiChatConnection.Create(nil);
  FChat.DriverName := TRANSLATOR_DRIVER;
  FChat.Model := TRANSLATOR_MODEL;
  FChat.Params.Values['Asynchronous'] := 'False';
  FChat.Params.Values['ApiKey'] := TRANSLATOR_APIKEY;

  FAudioApi := TAiOpenAiAudio.Create(nil);
  FAudioApi.ApiKey := '@OPENAI_API_KEY';
  FAudioApi.TranscriptionModel := TAiTranscriptionModel.tmGpt4oDiarize;
  FAudioApi.TranscriptionResponseFormat := TAiTranscriptionResponseFormat.trfDiarizedJson;
  FAudioApi.TTSModel := TAiTTSModel.gpt_4o_mini_tts;
  FAudioApi.TTSResponseFormat := TAiTTSResponseFormat.trfPcm;
  FAudioApi.TTSSpeed := 1.0;

  FPlayer := TAiAudioPlayer.Create(nil);
  FPlayer.DeviceId := ''; // auriculares (predeterminado)
  FPlayer.OnError := PlayerError;
  FPlayer.OnStateChange := PlayerStateChange;

  FSegmenter := TSpeechSegmenter.Create(
    procedure(aPcm: TBytes)
    begin
      ProcessSegment(aPcm);
    end);

  FCapture := TAiAudioCapture.Create(nil);
  FCapture.Source := asLoopback;
  FCapture.OutputSampleRate := SEG_SAMPLE_RATE;
  FCapture.OutputChannels := 1;
  FCapture.OnData := CaptureData;
  FCapture.OnError := CaptureError;
end;

destructor TDiarizedRemoteSide.Destroy;
begin
  Stop;
  FLock.Enter;
  try
    FClosing := True;
  finally
    FLock.Leave;
  end;
  FCapture.Free;
  FSegmenter.Free;
  FPlayer.Free;
  FAudioApi.Free;
  FChat.Free;
  FLock.Free;
  inherited;
end;

procedure TDiarizedRemoteSide.Start;
begin
  FPlayer.Active := True;
  FCapture.Active := True;
  SafeWriteLn(FTag + ' canal diarizado activo (VAD local + gpt-4o-transcribe-diarize).');
end;

procedure TDiarizedRemoteSide.Stop;
begin
  FClosing := True;
  if Assigned(FCapture) then
    FCapture.Active := False;
  if Assigned(FPlayer) then
    FPlayer.Active := False;
end;

procedure TDiarizedRemoteSide.CaptureData(Sender: TObject; const aBuffer: TBytes; aSampleRate, aChannels: Integer);
begin
  // OnData llega en el hilo principal: alimentar el VAD local
  if not FClosing then
    FSegmenter.Feed(aBuffer, FCapture.ChunkDurationMs);
end;

procedure TDiarizedRemoteSide.CaptureError(Sender: TObject; const ErrorMessage: string);
begin
  SafeWriteLn(FTag + ' ERROR captura: ' + ErrorMessage);
end;

procedure TDiarizedRemoteSide.PlayerError(Sender: TObject; const ErrorMessage: string);
begin
  SafeWriteLn(FTag + ' ERROR reproduccion: ' + ErrorMessage);
end;

procedure TDiarizedRemoteSide.PlayerStateChange(Sender: TObject; aIsPlaying: Boolean);
begin
  // Anti-realimentacion: nuestro TTS suena por el dispositivo que captura el
  // loopback; al terminar la cola se reactiva la captura.
  if not aIsPlaying then
    FCapture.Muted := False;
end;

function TDiarizedRemoteSide.SpeakerIndex(const aName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FSpeakerNames) do
    if SameText(FSpeakerNames[I], aName) then
      Exit(I);
end;

// Recorta del segmento el tramo mas largo del hablante aRawSpeaker y lo
// registra como known_speaker. Devuelve el nombre asignado ('Hablante N').
function TDiarizedRemoteSide.TryEnrollSpeaker(const aPcm: TBytes; const aRawSpeaker: string;
  const aSegments: TDiarizedSegments; out aName: string): Boolean;
const
  // La API exige muestras de 1.2 a 10.0 s; nos quedamos lejos de los bordes
  // (10.0 exactos puede rechazarse por redondeo).
  MIN_SAMPLE_SEC = 2.0;
  MAX_SAMPLE_SEC = 9.0;
var
  I: Integer;
  RunStart, RunEnd, BestStart, BestEnd: Double;
  InRun: Boolean;
  StartByte, LenBytes: Integer;
  Pcm: TMemoryStream;
  Wav: TMemoryStream;
begin
  Result := False;
  aName := '';
  if FAudioApi.KnownSpeakerCount >= 4 then
    Exit;

  // Buscar el tramo contiguo mas largo de este hablante
  BestStart := 0;
  BestEnd := 0;
  InRun := False;
  RunStart := 0;
  RunEnd := 0;
  for I := 0 to High(aSegments) do
  begin
    if SameText(aSegments[I].Speaker, aRawSpeaker) then
    begin
      if not InRun then
      begin
        InRun := True;
        RunStart := aSegments[I].StartTime;
      end;
      RunEnd := aSegments[I].EndTime;
    end
    else if InRun then
    begin
      if (RunEnd - RunStart) > (BestEnd - BestStart) then
      begin
        BestStart := RunStart;
        BestEnd := RunEnd;
      end;
      InRun := False;
    end;
  end;
  if InRun and ((RunEnd - RunStart) > (BestEnd - BestStart)) then
  begin
    BestStart := RunStart;
    BestEnd := RunEnd;
  end;

  if (BestEnd - BestStart) < MIN_SAMPLE_SEC then
    Exit; // muestra demasiado corta: se intentara en un proximo segmento

  if (BestEnd - BestStart) > MAX_SAMPLE_SEC then
    BestEnd := BestStart + MAX_SAMPLE_SEC;

  // Recortar el PCM (16 kHz mono PCM16 -> 32000 bytes/s, alineado a muestra)
  StartByte := (Round(BestStart * SEG_SAMPLE_RATE) * 2);
  LenBytes := (Round((BestEnd - BestStart) * SEG_SAMPLE_RATE) * 2);
  if StartByte + LenBytes > Length(aPcm) then
    LenBytes := Length(aPcm) - StartByte;
  if LenBytes <= 0 then
    Exit;

  Pcm := TMemoryStream.Create;
  try
    Pcm.WriteBuffer(aPcm[StartByte], LenBytes);
    if not ConvertPCMStreamToWAVStream(Pcm, Wav, SEG_SAMPLE_RATE, 1, 16) then
      Exit;
    try
      aName := Format('Hablante %d', [FAudioApi.KnownSpeakerCount + 1]);
      FAudioApi.AddKnownSpeaker(aName, Wav, 'audio/wav');
      FSpeakerNames := FSpeakerNames + [aName];
      Result := True;
      SafeWriteLn(Format('%s (nuevo hablante registrado: %s)', [FTag, aName]));
    finally
      Wav.Free;
    end;
  finally
    Pcm.Free;
  end;
end;

function TDiarizedRemoteSide.Translate(const aText: string): string;
var
  Prompt: string;
begin
  FChat.NewChat;
  Prompt := Format('Traduce al %s el siguiente texto. Responde UNICAMENTE con la traduccion, ' +
    'sin comentarios ni comillas:'#10'%s', [TARGET_LANG_REMOTE, aText]);
  Result := Trim(FChat.AddMessageAndRun(Prompt, 'user', []));
end;

procedure TDiarizedRemoteSide.ProcessSegment(const aPcm: TBytes);
var
  Data: TBytes;
begin
  Data := aPcm; // copia de referencia para la captura del closure
  TTask.Run(
    procedure
    begin
      try
        ProcessSegmentWork(Data);
      except
        on E: Exception do
          SafeWriteLn(Format('%s ERROR diarizando/traduciendo: %s', [FTag, E.Message]));
      end;
    end);
end;

procedure TDiarizedRemoteSide.ProcessSegmentWork(const aPcm: TBytes);
var
  PcmStream, WavStream, TtsStream: TMemoryStream;
  Media: TAiMediaFile;
  Res: TTranscriptionResult;
  Batch: TDictionary<string, string>; // speaker crudo -> nombre a mostrar
  I, Idx: Integer;
  Raw, Display, GroupSpeaker, GroupText, Translated: string;
  TtsPcm: TBytes;

  procedure EmitGroup;
  var
    VIdx: Integer;
  begin
    if (GroupText = '') or FClosing then
      Exit;
    SafeWriteLn(Format('%s %s: %s', [FTag, GroupSpeaker, GroupText]));
    Translated := Translate(GroupText);
    if Translated = '' then
      Exit;
    SafeWriteLn(Format('%s    -> %s', [FTag, Translated]));

    // Voz TTS segun el hablante registrado
    VIdx := SpeakerIndex(GroupSpeaker);
    if (VIdx >= 0) and (VIdx <= High(SPEAKER_VOICES)) then
      FAudioApi.TTSVoice := SPEAKER_VOICES[VIdx]
    else
      FAudioApi.TTSVoice := TAiTTSVoice.tvNova;

    TtsStream := FAudioApi.Speech(Translated);
    try
      if (TtsStream <> nil) and (TtsStream.Size > 0) and not FClosing and FPlayer.Active then
      begin
        SetLength(TtsPcm, TtsStream.Size);
        TtsStream.Position := 0;
        TtsStream.ReadBuffer(TtsPcm[0], TtsStream.Size);
        FCapture.Muted := True; // anti-realimentacion (se libera en OnStateChange)
        FPlayer.PlayPCM16(TtsPcm, TTS_PCM_RATE, 1);
      end;
    finally
      TtsStream.Free;
    end;
  end;

begin
  FLock.Enter;
  try
    if FClosing then
      Exit;

    // 1) PCM -> WAV -> transcripcion diarizada
    PcmStream := TMemoryStream.Create;
    try
      PcmStream.WriteBuffer(aPcm[0], Length(aPcm));
      if not ConvertPCMStreamToWAVStream(PcmStream, WavStream, SEG_SAMPLE_RATE, 1, 16) then
        Exit;
    finally
      PcmStream.Free;
    end;

    Media := TAiMediaFile.Create;
    try
      Media.LoadFromStream('segment.wav', WavStream);
    finally
      WavStream.Free;
    end;

    try
      try
        Res := FAudioApi.Transcribe(Media);
      except
        on E: Exception do
        begin
          // Si la API rechaza las muestras de hablantes registradas, limpiar
          // el registro y reintentar sin ellas (se re-registraran mas adelante).
          if Pos('known_speaker', LowerCase(E.Message)) > 0 then
          begin
            SafeWriteLn(FTag + ' (muestras de hablantes rechazadas por la API: reiniciando registro)');
            FAudioApi.ClearKnownSpeakers;
            FSpeakerNames := nil;
            Res := FAudioApi.Transcribe(Media);
          end
          else
            raise;
        end;
      end;
    finally
      Media.Free;
    end;

    try
      if Length(Res.Segments) = 0 then
      begin
        // Sin segmentos (silencio/musica): nada que traducir
        if Trim(Res.Text) <> '' then
          SafeWriteLn(Format('%s (sin hablantes) %s', [FTag, Trim(Res.Text)]));
        Exit;
      end;

      // 2) Mapear hablantes: conocidos -> su nombre; nuevos -> registrar
      Batch := TDictionary<string, string>.Create;
      try
        for I := 0 to High(Res.Segments) do
        begin
          Raw := Res.Segments[I].Speaker;
          if (Raw = '') or Batch.ContainsKey(Raw) then
            Continue;
          Idx := SpeakerIndex(Raw);
          if Idx >= 0 then
            Batch.Add(Raw, FSpeakerNames[Idx]) // ya registrado: nombre estable
          else if TryEnrollSpeaker(aPcm, Raw, Res.Segments, Display) then
            Batch.Add(Raw, Display)
          else
            Batch.Add(Raw, 'Hablante ' + Raw); // sin registrar (muestra corta o cupo lleno)
        end;

        // 3) Agrupar segmentos consecutivos del mismo hablante y emitir
        GroupSpeaker := '';
        GroupText := '';
        for I := 0 to High(Res.Segments) do
        begin
          Raw := Res.Segments[I].Speaker;
          if not Batch.TryGetValue(Raw, Display) then
            Display := 'Hablante ?';
          if Display <> GroupSpeaker then
          begin
            EmitGroup;
            GroupSpeaker := Display;
            GroupText := Trim(Res.Segments[I].Text);
          end
          else
            GroupText := GroupText + ' ' + Trim(Res.Segments[I].Text);
        end;
        EmitGroup;
      finally
        Batch.Free;
      end;
    finally
      Res.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

type
  // --------------------------------------------------------------------------
  // Canal [YO]: microfono -> Realtime STT -> traduccion -> TTS al cable
  // (identico al demo 063)
  // --------------------------------------------------------------------------
  TLocalSide = class
  private
    FTag: string;
    FClosing: Boolean;
    FCapture: TAiAudioCapture;
    FSTT: TAiOpenAiRealtimeSTT;
    FChat: TAiChatConnection;
    FTTS: TAiOpenAiAudio;
    FPlayer: TAiAudioPlayer;
    FMuteCapture: TAiAudioCapture;
    FChatLock: TCriticalSection;
    procedure SttSessionReady(Sender: TObject);
    procedure SttTranscriptCompleted(Sender: TObject; const Transcript: string; const ItemId: string);
    procedure SttError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure CaptureError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerError(Sender: TObject; const ErrorMessage: string);
    procedure PlayerStateChange(Sender: TObject; aIsPlaying: Boolean);
  public
    constructor Create(const aPlayerDeviceId: string);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property MuteCapture: TAiAudioCapture read FMuteCapture write FMuteCapture;
  end;

constructor TLocalSide.Create(const aPlayerDeviceId: string);
begin
  inherited Create;
  FTag := '[YO]   ';
  FChatLock := TCriticalSection.Create;

  FChat := TAiChatConnection.Create(nil);
  FChat.DriverName := TRANSLATOR_DRIVER;
  FChat.Model := TRANSLATOR_MODEL;
  FChat.Params.Values['Asynchronous'] := 'False';
  FChat.Params.Values['ApiKey'] := TRANSLATOR_APIKEY;

  FTTS := TAiOpenAiAudio.Create(nil);
  FTTS.ApiKey := '@OPENAI_API_KEY';
  FTTS.TTSModel := TAiTTSModel.gpt_4o_mini_tts;
  FTTS.TTSVoice := TAiTTSVoice.tvOnyx;
  FTTS.TTSResponseFormat := TAiTTSResponseFormat.trfPcm;
  FTTS.TTSSpeed := 1.0;

  FPlayer := TAiAudioPlayer.Create(nil);
  FPlayer.DeviceId := aPlayerDeviceId;
  FPlayer.OnError := PlayerError;
  FPlayer.OnStateChange := PlayerStateChange;

  FSTT := TAiOpenAiRealtimeSTT.Create(nil);
  FSTT.ApiKey := STT_APIKEY;
  FSTT.Model := STT_MODEL;
  // Canal [YO] (un solo hablante): gpt-live-transcribe (ago 2026).
  // El canal remoto sigue con gpt-4o-transcribe-diarize porque los modelos
  // nuevos NO soportan diarizacion.
  FSTT.TranscriptionModel := otmGptLiveTranscribe;
  FSTT.TranscriptionPrompt := 'Mi voz en una reunion de trabajo traducida';
  FSTT.Language := STT_LANG_LOCAL;
  FSTT.OnSessionReady := SttSessionReady;
  FSTT.OnTranscriptCompleted := SttTranscriptCompleted;
  FSTT.OnError := SttError;

  FCapture := TAiAudioCapture.Create(nil);
  FCapture.Source := asMicrophone;
  FCapture.OutputSampleRate := 16000;
  FCapture.OutputChannels := 1;
  FCapture.RealtimeSTT := FSTT;
  FCapture.OnError := CaptureError;
end;

destructor TLocalSide.Destroy;
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

procedure TLocalSide.Start;
begin
  FPlayer.Active := True;
  FSTT.Connect;
  FCapture.Active := True;
end;

procedure TLocalSide.Stop;
begin
  FClosing := True;
  if Assigned(FCapture) then
    FCapture.Active := False;
  if Assigned(FSTT) and FSTT.IsConnected then
    FSTT.Disconnect;
  if Assigned(FPlayer) then
    FPlayer.Active := False;
end;

procedure TLocalSide.SttSessionReady(Sender: TObject);
begin
  SafeWriteLn(FTag + ' STT conectado y listo.');
end;

procedure TLocalSide.SttError(Sender: TObject; const ErrorMsg, ErrorCode: string);
begin
  SafeWriteLn(Format('%s ERROR STT [%s]: %s', [FTag, ErrorCode, ErrorMsg]));
end;

procedure TLocalSide.CaptureError(Sender: TObject; const ErrorMessage: string);
begin
  SafeWriteLn(FTag + ' ERROR captura: ' + ErrorMessage);
end;

procedure TLocalSide.PlayerError(Sender: TObject; const ErrorMessage: string);
begin
  SafeWriteLn(FTag + ' ERROR reproduccion: ' + ErrorMessage);
end;

procedure TLocalSide.PlayerStateChange(Sender: TObject; aIsPlaying: Boolean);
begin
  if (not aIsPlaying) and Assigned(FMuteCapture) then
    FMuteCapture.Muted := False;
end;

procedure TLocalSide.SttTranscriptCompleted(Sender: TObject; const Transcript: string; const ItemId: string);
var
  Text: string;
begin
  Text := Trim(Transcript);
  if Text = '' then
    Exit;
  SafeWriteLn(Format('%s %s', [FTag, Text]));
  TTask.Run(
    procedure
    var
      Prompt, Res: string;
      TtsStream: TMemoryStream;
      Pcm: TBytes;
    begin
      try
        FChatLock.Enter;
        try
          if FClosing then
            Exit;
          FChat.NewChat;
          Prompt := Format('Traduce al %s el siguiente texto. Responde UNICAMENTE con la traduccion, ' +
            'sin comentarios ni comillas:'#10'%s', [TARGET_LANG_LOCAL, Text]);
          Res := Trim(FChat.AddMessageAndRun(Prompt, 'user', []));
          if Res = '' then
            Exit;
          SafeWriteLn(Format('%s    -> %s', [FTag, Res]));

          TtsStream := FTTS.Speech(Res);
          try
            if (TtsStream = nil) or (TtsStream.Size = 0) then
              Exit;
            SetLength(Pcm, TtsStream.Size);
            TtsStream.Position := 0;
            TtsStream.ReadBuffer(Pcm[0], TtsStream.Size);
          finally
            TtsStream.Free;
          end;

          if FClosing or not FPlayer.Active then
            Exit;
          if Assigned(FMuteCapture) then
            FMuteCapture.Muted := True;
          FPlayer.PlayPCM16(Pcm, TTS_PCM_RATE, 1);
        finally
          FChatLock.Leave;
        end;
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
  Remote: TDiarizedRemoteSide;
  Local: TLocalSide;
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
      WriteLn('=== Puente de voz con DIARIZACION (MakerAI) ===');
      WriteLn;
      WriteLn('  [ELLOS] loopback -> diarizacion -> traduccion por hablante -> TTS');
      WriteLn('          (cada hablante de la reunion con una voz distinta)');
      WriteLn('  [YO]    microfono -> Realtime -> traduccion -> TTS al cable');
      WriteLn;

      if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
      begin
        WriteLn('ERROR: la variable de entorno OPENAI_API_KEY no esta definida.');
        Exit;
      end;

      HasCable := FindMeetingDevice(MeetingDeviceId, MeetingDeviceName);
      if HasCable then
      begin
        WriteLn('Cable virtual: ' + MeetingDeviceName + '  (mic de la reunion = CABLE Output)');
        for D in TAiAudioPlayer.GetPlaybackDevices do
          if D.IsDefault and SameText(D.EndpointId, MeetingDeviceId) then
          begin
            WriteLn;
            WriteLn('*** ATENCION: "CABLE Input" es la salida PREDETERMINADA de Windows.   ***');
            WriteLn('*** Pon tus AURICULARES como salida predeterminada o no oiras nada.   ***');
            Break;
          end;
      end
      else
      begin
        WriteLn('AVISO: sin cable virtual ("' + MEETING_DEVICE_HINT + '"): MODO PRUEBA,');
        WriteLn('  el TTS en ingles sonara por el dispositivo predeterminado.');
        MeetingDeviceId := '';
      end;
      WriteLn;

      Remote := TDiarizedRemoteSide.Create;
      Local := TLocalSide.Create(MeetingDeviceId);
      if not HasCable then
        Local.MuteCapture := Remote.Capture;
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
