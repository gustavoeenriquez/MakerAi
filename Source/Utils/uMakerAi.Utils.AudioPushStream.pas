unit uMakerAi.Utils.AudioPushStream;

{******************************************************************************
  UAudioPushStream - Push-Stream Audio Player for Delphi FMX (Cross-Platform)
  ============================================================================
  Reproduce audio WAV en modo streaming sin archivos en disco.

  Windows  : API waveOut de Windows (MMSystem)
  Android  : JAudioTrack via JNI
  macOS/iOS: AudioQueue (AudioToolbox)

  Uso tipico con TTS:
    FAudio := TAudioPushStream.Create;
    FAudio.Start(44100, 2, 16);  // o dejarlo que auto-detecte del primer WAV

    // Cada vez que llega un chunk WAV del TTS:
    FAudio.PushWavData(WavBytes, Length(WavBytes));

    // Al terminar:
    FAudio.Stop;
    FAudio.Free;

  Autor: Generado para uso con TTS de IA
  Plataforma: Windows, Android, macOS, iOS
******************************************************************************}

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections
{$IFDEF MSWINDOWS}
  , Winapi.Windows
  , Winapi.MMSystem
{$ENDIF}
{$IFDEF ANDROID}
  , Androidapi.JNI.Media
  , Androidapi.JNIBridge
  , Androidapi.JNI.JavaTypes
  , Androidapi.Helpers
{$ENDIF}
{$IFDEF IOS}
  , iOSapi.AudioToolbox
{$ENDIF}
{$IFDEF MACOS}
  , Macapi.AudioToolbox
  , Macapi.CoreFoundation
{$ENDIF}
  ;

type
  { Descriptor de formato de audio independiente de plataforma }
  TAudioFormat = record
    SampleRate   : Cardinal;
    Channels     : Word;
    BitsPerSample: Word;
    BlockAlign   : Word;
    ByteRate     : Cardinal;
  end;

{$IFDEF MSWINDOWS}
  TWaveBuffer = record
    Header: TWaveHdr;
    Data  : TBytes;
  end;
  PWaveBuffer = ^TWaveBuffer;
{$ENDIF}

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
  TAQBuffer = record
    AQBuf: AudioQueueBufferRef;
    InUse: Boolean;
  end;
{$ENDIF}

  { Forward declaration }
  TAudioPushStream = class;

{$IFDEF ANDROID}
  TAudioWriterThread = class(TThread)
  private
    FOwner    : TAudioPushStream;
    FDataReady: TEvent;
    FStopFlag : PBoolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TAudioPushStream; ADataReady: TEvent;
      AStopFlag: PBoolean);
  end;
{$ENDIF}

  TAudioPushStream = class
  private const
    MAX_BUFFERS        = 8;
    BUFFER_DURATION_MS = 200;
  private
    { Campos comunes a todas las plataformas }
    FFormat     : TAudioFormat;
    FIsPlaying  : Boolean;
    FIsStarted  : Boolean;
    FIsPaused   : Boolean;
    FLock       : TCriticalSection;
    FPendingPCM : TBytes;
    FPendingSize: Integer;
    FBufferSize : Integer;
    FOnFinished   : TNotifyEvent;
    FOutputDevice : Integer;   // -1 = dispositivo predeterminado del sistema

{$IFDEF MSWINDOWS}
    FDevice : HWAVEOUT;
    FBuffers: array[0..MAX_BUFFERS - 1] of TWaveBuffer;
    procedure AllocateBuffers;
    procedure FreeBuffers;
{$ENDIF}

{$IFDEF ANDROID}
    FAudioTrack  : JAudioTrack;
    FWriterThread: TAudioWriterThread;
    FStopWriter  : Boolean;
    FDataReady   : TEvent;
{$ENDIF}

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
    FAudioQueue: AudioQueueRef;
    FAQBuffers : array[0..MAX_BUFFERS - 1] of TAQBuffer;
    procedure InitAudioQueue;
{$ENDIF}

    procedure InternalFeedBuffers;
    procedure AppendPCM(const Data: Pointer; Size: Integer);
    function  ConsumePCM(Dest: Pointer; MaxSize: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Devuelve la lista de dispositivos de salida disponibles en la plataforma
    /// actual. El indice de cada elemento corresponde al valor de OutputDevice.
    /// </summary>
    class function EnumerateDevices: TArray<string>; static;

    /// <summary>
    /// Inicia el dispositivo de audio con el formato especificado.
    /// Llamar antes de PushWavData o PushPCMData.
    /// </summary>
    procedure Start(SampleRate: Integer = 24000;
                    Channels: Word = 1;
                    BitsPerSample: Word = 16);

    /// <summary>
    /// Empuja un bloque completo WAV (con header). El header se parsea
    /// automaticamente. Si Start no fue llamado, se auto-inicia con el
    /// formato del primer WAV recibido.
    /// </summary>
    procedure PushWavData(const WavData: Pointer; WavDataSize: Integer); overload;
    procedure PushWavData(const WavBytes: TBytes); overload;
    procedure PushWavData(const AStream: TStream); overload;

    /// <summary>
    /// Empuja PCM raw directamente (sin header WAV).
    /// Start debe haber sido llamado antes.
    /// </summary>
    procedure PushPCMData(const PCMData: Pointer; PCMSize: Integer); overload;
    procedure PushPCMData(const PCMBytes: TBytes); overload;

    /// <summary>
    /// Pausa la reproduccion. Se puede reanudar con Resume.
    /// </summary>
    procedure Pause;

    /// <summary>
    /// Reanuda la reproduccion despues de Pause.
    /// </summary>
    procedure Resume;

    /// <summary>
    /// Descarta todo el audio pendiente sin cerrar el dispositivo.
    /// Lo que ya esta sonando termina, pero no se encola mas.
    /// </summary>
    procedure Flush;

    /// <summary>
    /// Detiene la reproduccion y libera el dispositivo.
    /// </summary>
    procedure Stop;

    property IsPlaying    : Boolean      read FIsPlaying;
    property IsPaused     : Boolean      read FIsPaused;
    property OnFinished   : TNotifyEvent read FOnFinished  write FOnFinished;

    /// <summary>
    /// Indice del dispositivo de salida. -1 = predeterminado del sistema.
    /// Usar EnumerateDevices para obtener la lista. Asignar antes de Start.
    /// </summary>
    property OutputDevice : Integer      read FOutputDevice write FOutputDevice;
  end;

implementation

{ ============================================================================
  macOS: tipos auxiliares � deben estar antes de InitAudioQueue
  ============================================================================ }
{$IFDEF MACOS}
type
  TAudioObjectPropertyAddress_Helper = record
    mSelector: UInt32;
    mScope   : UInt32;
    mElement : UInt32;
  end;

  TMacAudioOutputDevice = record
    Name: string;
    UID : string;
  end;

function GetMacOutputDevices: TArray<TMacAudioOutputDevice>; forward;
{$ENDIF MACOS}

{ ============================================================================
  Estructuras para parsear el header WAV
  ============================================================================ }
type
  TRiffHeader = packed record
    RIFF    : array[0..3] of AnsiChar;
    FileSize: UInt32;
    WAVE    : array[0..3] of AnsiChar;
  end;

  TChunkHeader = packed record
    ID  : array[0..3] of AnsiChar;
    Size: UInt32;
  end;

  TFmtChunk = packed record
    AudioFormat  : UInt16;
    Channels     : UInt16;
    SampleRate   : UInt32;
    ByteRate     : UInt32;
    BlockAlign   : UInt16;
    BitsPerSample: UInt16;
  end;

{ ============================================================================
  ParseWavHeader - extrae offset y tamano del chunk "data"
  Devuelve TAudioFormat en lugar de TWaveFormatEx para ser multiplataforma.
  ============================================================================ }
function ParseWavHeader(const Data: Pointer; DataSize: Integer;
  out Fmt: TAudioFormat; out PCMOffset, PCMSize: Integer): Boolean;
var
  P      : PByte;
  Riff   : TRiffHeader;
  Chunk  : TChunkHeader;
  FmtData: TFmtChunk;
  Offset : Integer;
begin
  Result    := False;
  PCMOffset := 0;
  PCMSize   := 0;

  if DataSize < SizeOf(TRiffHeader) then
    Exit;

  P := PByte(Data);
  Move(P^, Riff, SizeOf(TRiffHeader));

  if (Riff.RIFF <> 'RIFF') or (Riff.WAVE <> 'WAVE') then
    Exit;

  Offset := SizeOf(TRiffHeader);
  FillChar(Fmt, SizeOf(Fmt), 0);

  while Offset + SizeOf(TChunkHeader) <= DataSize do
  begin
    Move(P[Offset], Chunk, SizeOf(TChunkHeader));
    Inc(Offset, SizeOf(TChunkHeader));

    if Chunk.ID = 'fmt ' then
    begin
      if Offset + SizeOf(TFmtChunk) > DataSize then
        Exit;
      Move(P[Offset], FmtData, SizeOf(TFmtChunk));

      Fmt.Channels      := FmtData.Channels;
      Fmt.SampleRate    := FmtData.SampleRate;
      Fmt.BitsPerSample := FmtData.BitsPerSample;
      Fmt.BlockAlign    := FmtData.Channels * (FmtData.BitsPerSample div 8);
      Fmt.ByteRate      := FmtData.SampleRate * Fmt.BlockAlign;
    end
    else if Chunk.ID = 'data' then
    begin
      PCMOffset := Offset;
      // WAV streaming (OpenAI TTS): chunk size = 0xFFFFFFFF → checar ANTES de asignar a Integer
      // para evitar Range check error (UInt32 $FFFFFFFF no cabe en Integer con range check ON)
      if Chunk.Size = $FFFFFFFF then
        PCMSize := DataSize - PCMOffset
      else
      begin
        PCMSize := Integer(Chunk.Size);
        if PCMOffset + PCMSize > DataSize then
          PCMSize := DataSize - PCMOffset;
      end;
      Result := True;
      Exit;
    end;

    Inc(Offset, (Chunk.Size + 1) and not 1);
  end;
end;

{ ============================================================================
  Windows: callback de waveOut
  ============================================================================ }
{$IFDEF MSWINDOWS}
procedure WaveOutCallback(hwo: HWAVEOUT; uMsg: UINT; dwInstance: DWORD_PTR;
  dwParam1, dwParam2: DWORD_PTR); stdcall;
begin
  if uMsg = WOM_DONE then
    TAudioPushStream(dwInstance).InternalFeedBuffers;
end;
{$ENDIF}

{ ============================================================================
  macOS/iOS: callback de AudioQueue
  ============================================================================ }
{$IF DEFINED(IOS) OR DEFINED(MACOS)}
procedure AudioQueueOutputCallback(inUserData: Pointer; inAQ: AudioQueueRef;
  inBuffer: AudioQueueBufferRef); cdecl;
var
  Player: TAudioPushStream;
  I: Integer;
begin
  Player := TAudioPushStream(inUserData);
  { Marcar el buffer como libre }
  for I := 0 to TAudioPushStream.MAX_BUFFERS - 1 do
    if Player.FAQBuffers[I].AQBuf = inBuffer then
    begin
      Player.FAQBuffers[I].InUse := False;
      Break;
    end;
  Player.InternalFeedBuffers;
end;
{$ENDIF}

{ ============================================================================
  Android: hilo escritor de audio
  ============================================================================ }
{$IFDEF ANDROID}

constructor TAudioWriterThread.Create(AOwner: TAudioPushStream;
  ADataReady: TEvent; AStopFlag: PBoolean);
begin
  inherited Create(False); { arrancar inmediatamente }
  FOwner         := AOwner;
  FDataReady     := ADataReady;
  FStopFlag      := AStopFlag;
  FreeOnTerminate := False;
end;

procedure TAudioWriterThread.Execute;
var
  LocalBuf    : TBytes;
  DataSize    : Integer;
  JavaArray   : TJavaArray<Byte>;
  ShouldFinish: Boolean;
begin
  SetLength(LocalBuf, 65536);
  ShouldFinish := False;

  while (not Terminated) and (not ShouldFinish) do
  begin
    FDataReady.WaitFor(50);
    FDataReady.ResetEvent;

    { Vaciar todos los datos pendientes antes de volver a esperar }
    repeat
      FOwner.FLock.Enter;
      try
        DataSize := FOwner.ConsumePCM(@LocalBuf[0], Length(LocalBuf));
        if DataSize = 0 then
          ShouldFinish := FStopFlag^ and (FOwner.FPendingSize = 0);
      finally
        FOwner.FLock.Leave;
      end;

      if DataSize > 0 then
      begin
        JavaArray := TJavaArray<Byte>.Create(DataSize);
        try
          Move(LocalBuf[0], JavaArray.Data^, DataSize);
          FOwner.FAudioTrack.write(JavaArray, 0, DataSize);
        finally
          JavaArray.Free;
        end;
      end;
    until DataSize = 0;

    if ShouldFinish and Assigned(FOwner.FOnFinished) then
      TThread.Queue(nil, procedure begin FOwner.FOnFinished(FOwner) end);
  end;
end;

{$ENDIF ANDROID}

{ ============================================================================
  TAudioPushStream
  ============================================================================ }

constructor TAudioPushStream.Create;
begin
  inherited Create;
  FLock        := TCriticalSection.Create;
  FIsPlaying    := False;
  FIsStarted    := False;
  FIsPaused     := False;
  FPendingSize  := 0;
  FOutputDevice := -1;
{$IFDEF MSWINDOWS}
  FDevice := 0;
{$ENDIF}
{$IFDEF ANDROID}
  FAudioTrack   := nil;
  FWriterThread := nil;
  FDataReady    := nil;
  FStopWriter   := False;
{$ENDIF}
{$IF DEFINED(IOS) OR DEFINED(MACOS)}
  FAudioQueue := nil;
{$ENDIF}
end;

destructor TAudioPushStream.Destroy;
begin
  Stop;
  FLock.Free;
  inherited;
end;

{ --- Start --- }

procedure TAudioPushStream.Start(SampleRate: Integer; Channels: Word;
  BitsPerSample: Word);
{$IFDEF MSWINDOWS}
var
  WinFmt  : TWaveFormatEx;
  Res     : MMRESULT;
  DeviceID: UINT;
{$ENDIF}
{$IFDEF ANDROID}
var
  ChannelConfig, StreamType, AudioFmt, MinBuf: Integer;
{$ENDIF}
begin
  FLock.Enter;
  try
    if FIsStarted then
    begin
      FLock.Leave;
      try
        Stop;
      finally
        FLock.Enter;
      end;
    end;

    { Rellenar FFormat (independiente de plataforma) }
    FFormat.SampleRate    := SampleRate;
    FFormat.Channels      := Channels;
    FFormat.BitsPerSample := BitsPerSample;
    FFormat.BlockAlign    := Channels * (BitsPerSample div 8);
    FFormat.ByteRate      := SampleRate * FFormat.BlockAlign;

    FBufferSize := (FFormat.ByteRate * BUFFER_DURATION_MS) div 1000;
    FBufferSize := (FBufferSize div FFormat.BlockAlign) * FFormat.BlockAlign;

    FPendingSize := 0;
    SetLength(FPendingPCM, 0);

{$IFDEF MSWINDOWS}
    FillChar(WinFmt, SizeOf(WinFmt), 0);
    WinFmt.wFormatTag      := WAVE_FORMAT_PCM;
    WinFmt.nChannels       := Channels;
    WinFmt.nSamplesPerSec  := SampleRate;
    WinFmt.wBitsPerSample  := BitsPerSample;
    WinFmt.nBlockAlign     := FFormat.BlockAlign;
    WinFmt.nAvgBytesPerSec := FFormat.ByteRate;
    WinFmt.cbSize          := 0;

    if FOutputDevice < 0 then
      DeviceID := WAVE_MAPPER
    else
      DeviceID := UINT(FOutputDevice);

    Res := waveOutOpen(@FDevice, DeviceID, @WinFmt,
      DWORD_PTR(@WaveOutCallback), DWORD_PTR(Self),
      CALLBACK_FUNCTION);

    if Res <> MMSYSERR_NOERROR then
      raise Exception.CreateFmt('waveOutOpen fallo con error %d', [Res]);

    AllocateBuffers;
{$ENDIF}

{$IFDEF ANDROID}
    StreamType := 3;  { AudioManager.STREAM_MUSIC }
    AudioFmt   := 2;  { AudioFormat.ENCODING_PCM_16BIT }
    if Channels = 1 then
      ChannelConfig := 4    { AudioFormat.CHANNEL_OUT_MONO }
    else
      ChannelConfig := 12;  { AudioFormat.CHANNEL_OUT_STEREO }

    MinBuf := TJAudioTrack.JavaClass.getMinBufferSize(
      SampleRate, ChannelConfig, AudioFmt);

    FAudioTrack := TJAudioTrack.JavaClass.init(
      StreamType, SampleRate, ChannelConfig, AudioFmt, MinBuf * 4,
      1 { MODE_STREAM });

    FAudioTrack.play;

    { Seleccionar dispositivo de salida (requiere Android API 23 / Android 6.0+) }
    if (FOutputDevice >= 0) and TOSVersion.Check(6) then
    begin
      var AM   := TJAudioManager.Wrap(
        TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
      var Devs := AM.getDevices(TJAudioManager.JavaClass.GET_DEVICES_OUTPUTS);
      if FOutputDevice < Devs.Length then
        FAudioTrack.setPreferredDevice(Devs[FOutputDevice]);
    end;

    FStopWriter   := False;
    FDataReady    := TEvent.Create(nil, True, False, '');
    FWriterThread := TAudioWriterThread.Create(Self, FDataReady, @FStopWriter);
{$ENDIF}

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
    InitAudioQueue;
{$ENDIF}

    FIsStarted := True;
    FIsPlaying := True;
    FIsPaused  := False;
  finally
    FLock.Leave;
  end;
end;

{ --- Stop --- }

procedure TAudioPushStream.Stop;
{$IFDEF MSWINDOWS}
var
  I: Integer;
{$ENDIF}
{$IF DEFINED(IOS) OR DEFINED(MACOS)}
var
  I: Integer;
{$ENDIF}
begin
  FLock.Enter;
  try
    if not FIsStarted then
      Exit;

    FIsPlaying := False;
    FIsStarted := False;

{$IFDEF MSWINDOWS}
    if FDevice <> 0 then
    begin
      waveOutReset(FDevice);
      for I := 0 to MAX_BUFFERS - 1 do
        if (FBuffers[I].Header.dwFlags and WHDR_PREPARED) <> 0 then
          waveOutUnprepareHeader(FDevice, @FBuffers[I].Header, SizeOf(TWaveHdr));
      waveOutClose(FDevice);
      FDevice := 0;
    end;
    FreeBuffers;
{$ENDIF}

{$IFDEF ANDROID}
    FStopWriter := True;
    if Assigned(FDataReady) then
      FDataReady.SetEvent;
    { Liberar lock mientras esperamos el hilo para evitar deadlock }
    FLock.Leave;
    try
      if Assigned(FWriterThread) then
      begin
        FWriterThread.WaitFor;
        FreeAndNil(FWriterThread);
      end;
      FreeAndNil(FDataReady);
    finally
      FLock.Enter;
    end;
    if Assigned(FAudioTrack) then
    begin
      FAudioTrack.stop;
      FAudioTrack.release;
      FAudioTrack := nil;
    end;
{$ENDIF}

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
    if FAudioQueue <> nil then
    begin
      AudioQueueStop(FAudioQueue, True);
      AudioQueueDispose(FAudioQueue, True);
      FAudioQueue := nil;
    end;
    for I := 0 to MAX_BUFFERS - 1 do
    begin
      FAQBuffers[I].AQBuf := nil;
      FAQBuffers[I].InUse := False;
    end;
{$ENDIF}

    FPendingSize := 0;
    SetLength(FPendingPCM, 0);
  finally
    FLock.Leave;
  end;
end;

{ --- Pause --- }

procedure TAudioPushStream.Pause;
begin
  FLock.Enter;
  try
    if (not FIsStarted) or FIsPaused then
      Exit;

{$IFDEF MSWINDOWS}
    if FDevice <> 0 then
      waveOutPause(FDevice);
{$ENDIF}

{$IFDEF ANDROID}
    if Assigned(FAudioTrack) then
      FAudioTrack.pause;
{$ENDIF}

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
    if FAudioQueue <> nil then
      AudioQueuePause(FAudioQueue);
{$ENDIF}

    FIsPaused := True;
  finally
    FLock.Leave;
  end;
end;

{ --- Resume --- }

procedure TAudioPushStream.Resume;
begin
  FLock.Enter;
  try
    if (not FIsStarted) or (not FIsPaused) then
      Exit;

{$IFDEF MSWINDOWS}
    if FDevice <> 0 then
      waveOutRestart(FDevice);
{$ENDIF}

{$IFDEF ANDROID}
    if Assigned(FAudioTrack) then
    begin
      FAudioTrack.play;
      if Assigned(FDataReady) then
        FDataReady.SetEvent;
    end;
{$ENDIF}

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
    if FAudioQueue <> nil then
      AudioQueueStart(FAudioQueue, nil);
{$ENDIF}

    FIsPaused := False;
  finally
    FLock.Leave;
  end;

  { Realimentar buffers por si llegaron datos durante la pausa }
  InternalFeedBuffers;
end;

{ --- Flush --- }

procedure TAudioPushStream.Flush;
{$IFDEF MSWINDOWS}
var
  I: Integer;
{$ENDIF}
begin
  FLock.Enter;
  try
    if not FIsStarted then
      Exit;

    FPendingSize := 0;

{$IFDEF MSWINDOWS}
    if FDevice <> 0 then
    begin
      waveOutReset(FDevice);
      for I := 0 to MAX_BUFFERS - 1 do
      begin
        if (FBuffers[I].Header.dwFlags and WHDR_PREPARED) <> 0 then
          waveOutUnprepareHeader(FDevice, @FBuffers[I].Header, SizeOf(TWaveHdr));
        FBuffers[I].Header.dwFlags := 0;
      end;
    end;
    FIsPaused := False;
{$ENDIF}

{$IFDEF ANDROID}
    if Assigned(FAudioTrack) then
    begin
      FAudioTrack.pause;
      FAudioTrack.flush;
      FAudioTrack.play;
    end;
    FIsPaused := False;
{$ENDIF}

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
    if FAudioQueue <> nil then
    begin
      AudioQueueFlush(FAudioQueue);
      AudioQueueReset(FAudioQueue);
    end;
    FIsPaused := False;
{$ENDIF}
  finally
    FLock.Leave;
  end;
end;

{ --- Windows: gestion de buffers --- }

{$IFDEF MSWINDOWS}
procedure TAudioPushStream.AllocateBuffers;
var
  I: Integer;
begin
  for I := 0 to MAX_BUFFERS - 1 do
  begin
    FillChar(FBuffers[I].Header, SizeOf(TWaveHdr), 0);
    SetLength(FBuffers[I].Data, FBufferSize);
    FBuffers[I].Header.lpData        := @FBuffers[I].Data[0];
    FBuffers[I].Header.dwBufferLength := FBufferSize;
    FBuffers[I].Header.dwUser        := DWORD_PTR(@FBuffers[I]);
  end;
end;

procedure TAudioPushStream.FreeBuffers;
var
  I: Integer;
begin
  for I := 0 to MAX_BUFFERS - 1 do
    SetLength(FBuffers[I].Data, 0);
end;
{$ENDIF}

{ --- macOS/iOS: inicializacion de AudioQueue --- }

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
procedure TAudioPushStream.InitAudioQueue;
var
  Desc  : AudioStreamBasicDescription;
  I     : Integer;
  Res   : OSStatus;
{$IFDEF MACOS}
  MacDevs: TArray<TMacAudioOutputDevice>;
  UID    : string;
  UIDRef : CFStringRef;
  UTF8UID: UTF8String;
{$ENDIF}
begin
  FillChar(Desc, SizeOf(Desc), 0);
  Desc.mSampleRate       := FFormat.SampleRate;
  Desc.mFormatID         := kAudioFormatLinearPCM;
  Desc.mFormatFlags      := kAudioFormatFlagIsSignedInteger or kAudioFormatFlagIsPacked;
  Desc.mFramesPerPacket  := 1;
  Desc.mChannelsPerFrame := FFormat.Channels;
  Desc.mBitsPerChannel   := FFormat.BitsPerSample;
  Desc.mBytesPerFrame    := FFormat.BlockAlign;
  Desc.mBytesPerPacket   := FFormat.BlockAlign;

  Res := AudioQueueNewOutput(@Desc, AudioQueueOutputCallback, Self,
    nil, nil, 0, FAudioQueue);
  if Res <> 0 then
    raise Exception.CreateFmt('AudioQueueNewOutput fallo: %d', [Res]);

  for I := 0 to MAX_BUFFERS - 1 do
  begin
    AudioQueueAllocateBuffer(FAudioQueue, FBufferSize, FAQBuffers[I].AQBuf);
    FAQBuffers[I].InUse := False;
  end;

  AudioQueueStart(FAudioQueue, nil);

{$IFDEF MACOS}
  { Seleccionar dispositivo de salida especifico mediante su UID }
  if FOutputDevice >= 0 then
  begin
    MacDevs := GetMacOutputDevices;
    if FOutputDevice < Length(MacDevs) then
    begin
      UID := MacDevs[FOutputDevice].UID;
      if UID <> '' then
      begin
        UTF8UID := UTF8String(UID);
        UIDRef  := CFStringCreateWithCString(nil, PAnsiChar(UTF8UID),
          $08000100 { kCFStringEncodingUTF8 });
        if UIDRef <> nil then
        try
          AudioQueueSetProperty(FAudioQueue,
            $61716364 { kAudioQueueProperty_CurrentDevice },
            @UIDRef, SizeOf(CFStringRef));
        finally
          CFRelease(UIDRef);
        end;
      end;
    end;
  end;
{$ENDIF MACOS}
end;
{$ENDIF}

{ --- AppendPCM / ConsumePCM (independientes de plataforma) --- }

procedure TAudioPushStream.AppendPCM(const Data: Pointer; Size: Integer);
var
  NewLen: Integer;
begin
  if Size <= 0 then
    Exit;

  NewLen := FPendingSize + Size;
  if NewLen > Length(FPendingPCM) then
  begin
    if NewLen < 65536 then
      SetLength(FPendingPCM, 65536)
    else
      SetLength(FPendingPCM, NewLen + (NewLen div 2));
  end;

  Move(Data^, FPendingPCM[FPendingSize], Size);
  Inc(FPendingSize, Size);
end;

function TAudioPushStream.ConsumePCM(Dest: Pointer; MaxSize: Integer): Integer;
begin
  Result := MaxSize;
  if Result > FPendingSize then
    Result := FPendingSize;

  if Result <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  Move(FPendingPCM[0], Dest^, Result);

  Dec(FPendingSize, Result);
  if FPendingSize > 0 then
    Move(FPendingPCM[Result], FPendingPCM[0], FPendingSize);
end;

{ --- InternalFeedBuffers --- }

procedure TAudioPushStream.InternalFeedBuffers;
var
{$IFDEF MSWINDOWS}
  I, Filled: Integer;
  Res      : MMRESULT;
  AllDone  : Boolean;
{$ENDIF}
{$IF DEFINED(IOS) OR DEFINED(MACOS)}
  I, Filled: Integer;
  AllFree  : Boolean;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  FLock.Enter;
  try
    if (not FIsStarted) or (FDevice = 0) or FIsPaused then
      Exit;

    for I := 0 to MAX_BUFFERS - 1 do
    begin
      if (FBuffers[I].Header.dwFlags and WHDR_DONE) <> 0 then
        waveOutUnprepareHeader(FDevice, @FBuffers[I].Header, SizeOf(TWaveHdr));

      if (FBuffers[I].Header.dwFlags and WHDR_PREPARED) = 0 then
      begin
        Filled := ConsumePCM(@FBuffers[I].Data[0], FBufferSize);
        if Filled = 0 then
          Break;

        FBuffers[I].Header.dwBufferLength := Filled;
        FBuffers[I].Header.dwFlags        := 0;

        Res := waveOutPrepareHeader(FDevice, @FBuffers[I].Header, SizeOf(TWaveHdr));
        if Res = MMSYSERR_NOERROR then
          waveOutWrite(FDevice, @FBuffers[I].Header, SizeOf(TWaveHdr));
      end;
    end;

    { Detectar fin de reproduccion }
    if FPendingSize = 0 then
    begin
      AllDone := True;
      for I := 0 to MAX_BUFFERS - 1 do
        if (FBuffers[I].Header.dwFlags and WHDR_PREPARED) <> 0 then
        begin
          AllDone := False;
          Break;
        end;
      if AllDone and FIsStarted and Assigned(FOnFinished) then
        TThread.Queue(nil, procedure begin FOnFinished(Self) end);
    end;
  finally
    FLock.Leave;
  end;
{$ENDIF MSWINDOWS}

{$IFDEF ANDROID}
  { Solo senal al hilo escritor; el consume/write ocurre alli }
  if FIsStarted and (not FIsPaused) and Assigned(FDataReady) then
    FDataReady.SetEvent;
{$ENDIF ANDROID}

{$IF DEFINED(IOS) OR DEFINED(MACOS)}
  FLock.Enter;
  try
    if (not FIsStarted) or (FAudioQueue = nil) or FIsPaused then
      Exit;

    for I := 0 to MAX_BUFFERS - 1 do
    begin
      if not FAQBuffers[I].InUse then
      begin
        Filled := ConsumePCM(FAQBuffers[I].AQBuf^.mAudioData, FBufferSize);
        if Filled = 0 then
          Break;

        FAQBuffers[I].AQBuf^.mAudioDataByteSize := Filled;
        FAQBuffers[I].InUse                     := True;
        AudioQueueEnqueueBuffer(FAudioQueue, FAQBuffers[I].AQBuf, 0, nil);
      end;
    end;

    { Detectar fin de reproduccion }
    if FPendingSize = 0 then
    begin
      AllFree := True;
      for I := 0 to MAX_BUFFERS - 1 do
        if FAQBuffers[I].InUse then
        begin
          AllFree := False;
          Break;
        end;
      if AllFree and FIsStarted and Assigned(FOnFinished) then
        TThread.Queue(nil, procedure begin FOnFinished(Self) end);
    end;
  finally
    FLock.Leave;
  end;
{$ENDIF}
end;

{ --- PushWavData --- }

procedure TAudioPushStream.PushWavData(const WavData: Pointer;
  WavDataSize: Integer);
var
  Fmt      : TAudioFormat;
  PCMOffset: Integer;
  PCMSize  : Integer;
begin
  if not ParseWavHeader(WavData, WavDataSize, Fmt, PCMOffset, PCMSize) then
    raise Exception.Create('Formato WAV invalido o no reconocido');

  FLock.Enter;
  try
    if not FIsStarted then
    begin
      FLock.Leave;
      try
        Start(Fmt.SampleRate, Fmt.Channels, Fmt.BitsPerSample);
      finally
        FLock.Enter;
      end;
    end;

    AppendPCM(PByte(WavData) + PCMOffset, PCMSize);
  finally
    FLock.Leave;
  end;

  InternalFeedBuffers;
end;

procedure TAudioPushStream.PushWavData(const WavBytes: TBytes);
begin
  if Length(WavBytes) > 0 then
    PushWavData(@WavBytes[0], Length(WavBytes));
end;

procedure TAudioPushStream.PushWavData(const AStream: TStream);
var
  WavBytes: TBytes;
  OldPos  : Int64;
begin
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  OldPos := AStream.Position;
  AStream.Position := 0;
  SetLength(WavBytes, AStream.Size);
  AStream.ReadBuffer(WavBytes[0], AStream.Size);
  AStream.Position := OldPos;
  PushWavData(@WavBytes[0], Length(WavBytes));
end;

{ --- PushPCMData --- }

procedure TAudioPushStream.PushPCMData(const PCMData: Pointer; PCMSize: Integer);
begin
  if not FIsStarted then
    raise Exception.Create('Llamar a Start antes de PushPCMData');

  FLock.Enter;
  try
    AppendPCM(PCMData, PCMSize);
  finally
    FLock.Leave;
  end;

  InternalFeedBuffers;
end;

procedure TAudioPushStream.PushPCMData(const PCMBytes: TBytes);
begin
  if Length(PCMBytes) > 0 then
    PushPCMData(@PCMBytes[0], Length(PCMBytes));
end;

{ ============================================================================
  macOS: declaraciones de CoreAudio para enumeracion de dispositivos fisicos
  ============================================================================ }
{$IFDEF MACOS}
const
  { AudioHardware selectors }
  kMAI_AudioObjectSystemObject      = UInt32(1);
  kMAI_AudioObjectScopeGlobal       = UInt32($676C6F62); // 'glob'
  kMAI_AudioObjectScopeOutput       = UInt32($6F757470); // 'outp'
  kMAI_AudioObjectElementMain       = UInt32(0);
  kMAI_AudioHardwarePropertyDevices = UInt32($64657623); // 'dev#'
  kMAI_AudioDevicePropertyUID       = UInt32($75696420); // 'uid '
  kMAI_AudioObjectPropertyName      = UInt32($6C6E616D); // 'lnam'
  kMAI_AudioDevicePropertyStreams   = UInt32($73746D23); // 'stm#'
  kMAI_CFStringEncodingUTF8         = UInt32($08000100);

{ CoreAudio hardware API � no esta en Macapi.AudioToolbox }
function _AO_GetPropertyDataSize(inObjectID: UInt32; inAddress: Pointer;
  inQualifierDataSize: UInt32; inQualifierData: Pointer;
  out outDataSize: UInt32): Integer; cdecl;
  external '/System/Library/Frameworks/CoreAudio.framework/CoreAudio'
  name 'AudioObjectGetPropertyDataSize';

function _AO_GetPropertyData(inObjectID: UInt32; inAddress: Pointer;
  inQualifierDataSize: UInt32; inQualifierData: Pointer;
  var ioDataSize: UInt32; outData: Pointer): Integer; cdecl;
  external '/System/Library/Frameworks/CoreAudio.framework/CoreAudio'
  name 'AudioObjectGetPropertyData';

{ Convierte un CFStringRef a string Delphi via UTF-8 }
function CFStringRefToStr(S: CFStringRef): string;
var
  Buf: array[0..1023] of Byte;
begin
  Result := '';
  if (S = nil) then Exit;
  FillChar(Buf, SizeOf(Buf), 0);
  if CFStringGetCString(S, @Buf[0], SizeOf(Buf), kMAI_CFStringEncodingUTF8) then
    Result := UTF8ToString(PAnsiChar(@Buf[0]));
end;

{ Enumera todos los dispositivos macOS que tienen streams de salida }
function GetMacOutputDevices: TArray<TMacAudioOutputDevice>;
var
  Addr    : TAudioObjectPropertyAddress_Helper;
  DataSize: UInt32;
  DevIDs  : TArray<UInt32>;
  I       : Integer;
  HasOut  : UInt32;
  UIDRef  : CFStringRef;
  NameRef : CFStringRef;
  Dev     : TMacAudioOutputDevice;
begin
  SetLength(Result, 0);

  Addr.mSelector := kMAI_AudioHardwarePropertyDevices;
  Addr.mScope    := kMAI_AudioObjectScopeGlobal;
  Addr.mElement  := kMAI_AudioObjectElementMain;

  DataSize := 0;
  if _AO_GetPropertyDataSize(kMAI_AudioObjectSystemObject, @Addr, 0, nil, DataSize) <> 0 then
    Exit;
  if DataSize = 0 then Exit;

  SetLength(DevIDs, DataSize div SizeOf(UInt32));
  if _AO_GetPropertyData(kMAI_AudioObjectSystemObject, @Addr, 0, nil, DataSize,
    @DevIDs[0]) <> 0 then Exit;

  for I := 0 to High(DevIDs) do
  begin
    { Verificar que el dispositivo tenga streams de salida }
    Addr.mSelector := kMAI_AudioDevicePropertyStreams;
    Addr.mScope    := kMAI_AudioObjectScopeOutput;
    HasOut := 0;
    if _AO_GetPropertyDataSize(DevIDs[I], @Addr, 0, nil, HasOut) <> 0 then Continue;
    if HasOut = 0 then Continue;

    { UID del dispositivo }
    Addr.mSelector := kMAI_AudioDevicePropertyUID;
    Addr.mScope    := kMAI_AudioObjectScopeGlobal;
    DataSize := SizeOf(CFStringRef);
    UIDRef := nil;
    if _AO_GetPropertyData(DevIDs[I], @Addr, 0, nil, DataSize, @UIDRef) = 0 then
    begin
      Dev.UID := CFStringRefToStr(UIDRef);
      if UIDRef <> nil then CFRelease(UIDRef);
    end else
      Dev.UID := '';

    { Nombre del dispositivo }
    Addr.mSelector := kMAI_AudioObjectPropertyName;
    Addr.mScope    := kMAI_AudioObjectScopeGlobal;
    DataSize := SizeOf(CFStringRef);
    NameRef := nil;
    if _AO_GetPropertyData(DevIDs[I], @Addr, 0, nil, DataSize, @NameRef) = 0 then
    begin
      Dev.Name := CFStringRefToStr(NameRef);
      if NameRef <> nil then CFRelease(NameRef);
    end else
      Dev.Name := Format('Device %d', [I]);

    if Dev.Name = '' then Dev.Name := Format('Device %d', [I]);

    Result := Result + [Dev];
  end;
end;
{$ENDIF MACOS}

{ ============================================================================
  EnumerateDevices � lista de dispositivos de salida por plataforma
  ============================================================================ }
class function TAudioPushStream.EnumerateDevices: TArray<string>;
var
{$IFDEF MSWINDOWS}
  I    : Integer;
  Count: UINT;
  Caps : TWaveOutCaps;
{$ENDIF}
{$IFDEF ANDROID}
  AM     : JAudioManager;
  Devs   : TJavaObjectArray<JAudioDeviceInfo>;
  I, Typ : Integer;
  TypStr : string;
{$ENDIF}
{$IFDEF MACOS}
  MacDevs: TArray<TMacAudioOutputDevice>;
  I      : Integer;
{$ENDIF}
begin
  SetLength(Result, 0);

{$IFDEF MSWINDOWS}
  Count := waveOutGetNumDevs;
  SetLength(Result, Count);
  for I := 0 to Integer(Count) - 1 do
  begin
    FillChar(Caps, SizeOf(Caps), 0);
    waveOutGetDevCaps(I, @Caps, SizeOf(Caps));
    Result[I] := Format('[%d] %s', [I, string(Caps.szPname)]);
  end;
{$ENDIF}

{$IFDEF ANDROID}
  if not TOSVersion.Check(6) then Exit;  { getDevices requiere API 23 }
  AM   := TJAudioManager.Wrap(
    TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  Devs := AM.getDevices(TJAudioManager.JavaClass.GET_DEVICES_OUTPUTS);
  SetLength(Result, Devs.Length);
  for I := 0 to Devs.Length - 1 do
  begin
    Typ := Devs[I].getType;
    case Typ of
      1 : TypStr := 'Earpiece';
      2 : TypStr := 'Speaker';
      3 : TypStr := 'Wired Headset';
      4 : TypStr := 'Wired Headphones';
      7 : TypStr := 'Bluetooth SCO';
      8 : TypStr := 'Bluetooth A2DP';
      9 : TypStr := 'HDMI';
      10: TypStr := 'HDMI ARC';
      11: TypStr := 'USB Device';
      12: TypStr := 'USB Accessory';
      22: TypStr := 'USB Headset';
    else  TypStr := Format('Audio Device (type %d)', [Typ]);
    end;
    Result[I] := Format('[%d] %s', [I, TypStr]);
  end;
{$ENDIF}

{$IFDEF MACOS}
  MacDevs := GetMacOutputDevices;
  SetLength(Result, Length(MacDevs));
  for I := 0 to High(MacDevs) do
    Result[I] := Format('[%d] %s', [I, MacDevs[I].Name]);
{$ENDIF}

{ iOS: el routing es gestionado por AVAudioSession; retorna lista vacia }
end;

end.
