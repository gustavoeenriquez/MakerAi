unit uWinSoundMonitor;

interface

Uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, System.SyncObjs,

{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.MMSystem,
{$ENDIF}
{$IFDEF ANDROID}
  AndroidApi.JNI.Media, AndroidApi.JNIBridge, AndroidApi.Helpers,
  FMX.Helpers.Android, AndroidApi.JNI.JavaTypes, AndroidApi.JNI.GraphicsContentViewText,
  MobilePermissions.Model.Standard, MobilePermissions.Component,
  MobilePermissions.Model.Signature, MobilePermissions.Model.Dangerous,
  AndroidApi.JNI.Net, // Añadir esta unidad para TJnet_Uri
{$ENDIF}
  System.Math;

const
  BUFFER_SIZE = 8820; // 100 ms buffer size for 44100 Hz, 16-bit mono
  THRESHOLD = 1000; // Example threshold, adjust as necessary
  SAMPLE_RATE = 44100;

{$IFDEF ANDROID}
{$ALIGN 1}
{$ENDIF}

type
  TAudioMonitorOnChange = Procedure(Sender: TObject; aState: Boolean; aStream: TMemoryStream) of object;


  // Formato WAV

{$IFDEF ANDROID}

  TRiffHeader = packed record
    ChunkID: array [0 .. 3] of AnsiChar;
    ChunkSize: LongWord;
    Format: array [0 .. 3] of AnsiChar;
  end;

  TFmtChunk = packed record
    Subchunk1ID: array [0 .. 3] of AnsiChar;
    Subchunk1Size: LongWord;
    AudioFormat: SmallInt;
    NumChannels: SmallInt;
    SampleRate: LongWord;
    ByteRate: LongWord;
    BlockAlign: SmallInt;
    BitsPerSample: SmallInt;
  end;

  TDataChunk = packed record
    Subchunk2ID: array [0 .. 3] of AnsiChar;
    Subchunk2Size: LongWord;
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}

  TRiffHeader = packed record
    ChunkID: array [0 .. 3] of AnsiChar;
    ChunkSize: LongWord;
    Format: array [0 .. 3] of AnsiChar;
  end;

  TFmtChunk = packed record
    Subchunk1ID: array [0 .. 3] of AnsiChar;
    Subchunk1Size: LongWord;
    AudioFormat: SmallInt;
    NumChannels: SmallInt;
    SampleRate: LongWord;
    ByteRate: LongWord;
    BlockAlign: SmallInt;
    BitsPerSample: SmallInt;
  end;

  TDataChunk = packed record
    Subchunk2ID: array [0 .. 3] of AnsiChar;
    Subchunk2Size: LongWord;
  end;

{$ENDIF}

  TAudioMonitor = class(TObject)
  private
{$IFDEF MSWINDOWS}
    hWaveIn: hWaveIn;
    WaveHdr: TWaveHdr;
{$ENDIF}
{$IFDEF ANDROID}
    FAudioRecord: JAudioRecord;
    FCaptureThread: TThread;
{$ENDIF}
    Buffer: array [0 .. BUFFER_SIZE - 1] of Byte;
    ArrBuf: array [0 .. 9] of Boolean;
    Done: Boolean;
    FSoundLevel: Int64;
    IdxArr: Integer;
    FSensitivity: Integer;
    FIsSpeaking: Boolean;
    FOnChangeState: TAudioMonitorOnChange;
    FileStream: TMemoryStream;
    FActive: Boolean;

    procedure SetSoundLevel(const Value: Int64);
    function GetSoundLevel: Int64;
    procedure SetSensitivity(const Value: Integer);
    function GetIsSpeaking: Boolean;
    procedure SetOnChangeState(const Value: TAudioMonitorOnChange);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  Protected
    Procedure CalcSilencio;
    Procedure DoChangeState(aActive: Boolean);

{$IFDEF MSWINDOWS}
    Procedure StartCaptureAudioWindows;
    Procedure StopCaptureAudioWindows;
{$ENDIF}
{$IFDEF ANDROID}
    Procedure StartCaptureAudioAndroid;
    Procedure StopCaptureAudioAndroid;
{$ENDIF}
  Public
    Constructor Create;
    Destructor Destroy; Override;
    procedure Start;
    procedure Stop;
    Class Procedure ConvertPCMToWAV(PCMFile, WAVFile: TMemoryStream);
    Procedure CalcSoundLevel;
    Property SoundLevel: Int64 read GetSoundLevel write SetSoundLevel;
    Property Sensitivity: Integer read FSensitivity write SetSensitivity;
    Property IsSpeaking: Boolean Read GetIsSpeaking;
    Property OnChangeState: TAudioMonitorOnChange read FOnChangeState write SetOnChangeState;
    Property Active: Boolean read GetActive Write SetActive;
  end;

Var
  FSoundMonitor: TAudioMonitor;
  // FCS: TCriticalSection;

implementation

{$IFDEF MSWINDOWS}

procedure AudioCallback(hWaveIn: hWaveIn; uMsg: UINT; dwInstance, dwParam1, dwParam2: DWORD_PTR); stdcall;
begin
  if uMsg = WIM_DATA then
  begin
    FSoundMonitor.CalcSoundLevel; // FSoundMonitor.CalcSoundLevel(@FSoundMonitor.Buffer[0], BUFFER_SIZE);
    // Re-add the buffer for continuous capture
    waveInAddBuffer(hWaveIn, @FSoundMonitor.WaveHdr, SizeOf(TWaveHdr));
  end;
end;
{$ENDIF}
{ TAudioCaptureForm }

constructor TAudioMonitor.Create;
var
  SampleRate, ChannelConfig, AudioFormat, BufferSize: Integer;

begin
  Inherited;
  FActive := False;
  FillChar(ArrBuf, Length(ArrBuf), 0);
  IdxArr := 0;
  FSensitivity := 55;
  FIsSpeaking := False; // no hay ruido en el microfono
  FileStream := TMemoryStream.Create;

{$IFDEF ANDROID}
  SampleRate := SAMPLE_RATE; // Frecuencia de muestreo de 44.1 kHz
  ChannelConfig := TJAudioFormat.JavaClass.CHANNEL_IN_MONO;
  AudioFormat := TJAudioFormat.JavaClass.ENCODING_PCM_16BIT;
  BufferSize := TJAudioRecord.JavaClass.getMinBufferSize(SampleRate, ChannelConfig, AudioFormat);

  FAudioRecord := TJAudioRecord.JavaClass.init(TJMediaRecorder_AudioSource.JavaClass.MIC, // Fuente de audio (micrófono)
    SampleRate, ChannelConfig, AudioFormat, BufferSize);

{$ENDIF}
end;

destructor TAudioMonitor.Destroy;
begin
  Stop;
  inherited;
end;

procedure TAudioMonitor.DoChangeState(aActive: Boolean);
Var
  WavStream: TMemoryStream;
begin
  // Si Active = True es que inicia la conversación
  // Si Active = False es que termina la conversación

  If aActive = True then // Debe
  Begin
    FileStream.Clear; // Limpia el Buffer para continuar con la grabación

    If Assigned(FOnChangeState) then // Guarda solo si está activo, en las pausas no guarda
      FOnChangeState(Self, aActive, Nil);
  End
  Else // Si active es false debe convertir el stream en wave
  Begin
    WavStream := TMemoryStream.Create;
    ConvertPCMToWAV(FileStream, WavStream);

    If Assigned(FOnChangeState) then // Guarda solo si está activo, en las pausas no guarda
    Begin
      Try
        FOnChangeState(Self, aActive, WavStream);
      Finally
        // WavStream.Free;  //lo debe eliminar el usuario
      End;
    End;
  End;

end;

function TAudioMonitor.GetActive: Boolean;
begin
  Result := FActive;
end;

function TAudioMonitor.GetIsSpeaking: Boolean;
begin
  // Se considera activo si los dos últimos registros son activos
  // Se considera inactivo si todos los registros son Inactivos.
  // Entre tanto no cambia el estado
  Result := FIsSpeaking;
end;

function TAudioMonitor.GetSoundLevel: Int64;
begin
  // FCS.Acquire;
  Try
    Result := FSoundLevel;
  Finally
    // FCS.Release;
  End;
end;

procedure TAudioMonitor.CalcSilencio;
Var
  I: Integer;
  Res: Boolean;
begin
  // Si los dos últimos registros están activos se activa el component
  // si todos los registros son falsos marca como en silencio.

  If (ArrBuf[Length(ArrBuf) - 2] = True) and (ArrBuf[Length(ArrBuf) - 1] = True) then
  Begin
    Res := True;
    If FIsSpeaking <> Res then
    Begin
      FIsSpeaking := Res;
      DoChangeState(FIsSpeaking);
    End;
  End
  Else
  Begin
    Res := False;
    For I := 0 to Length(ArrBuf) - 1 do
      Res := Res or ArrBuf[I]; // Si todos son falso entra

    If Res = False then
    Begin
      If FIsSpeaking <> Res then
      Begin
        FIsSpeaking := Res;
        DoChangeState(FIsSpeaking);
      End;
    End;
  End;
end;

Procedure TAudioMonitor.CalcSoundLevel;
var
  { Sample: SmallInt;
    AmplitudeSum: Int64;
    SampleCount, I: Integer;
  }

  I: Integer;
  Sum: Double;
  RMS: Double;
  aBufferSize: Integer;
  Valor: SmallInt;

begin
  { AmplitudeSum := 0;

    SampleCount := aBufferSize div SizeOf(SmallInt);

    for I := 0 to SampleCount - 1 do
    begin
    Sample := PSmallInt(aBuffer + I * SizeOf(SmallInt))^;
    AmplitudeSum := AmplitudeSum + Abs(Sample);
    end;
    SoundLevel := (AmplitudeSum div SampleCount);
  }

  FileStream.WriteBuffer(Buffer[0], Length(Buffer));

  Sum := 0.0;
  aBufferSize := Length(Buffer) div 2;

  // Calcular la suma del cuadrado de los valores del buffer
  for I := 0 to aBufferSize - 1 do
  begin
    Valor := PSmallInt(@Buffer[I * 2])^;
    Sum := Sum + (Valor * Valor);
  end;

  // Calcular RMS (Root Mean Square)
  RMS := Sqrt(Sum / aBufferSize);

  // Convertir RMS a decibelios (dB)
  if RMS > 0 then
    SoundLevel := Round(20 * Log10(RMS))
  else
    SoundLevel := 0;

end;

{
  Class procedure TAudioMonitor.ConvertPCMToWAV(PCMFile, WAVFile: TMemoryStream);
  var
  WaveHeader: TWaveFormatEx;
  RIFFHeader: array [0 .. 3] of AnsiChar;
  WaveChunk: array [0 .. 3] of AnsiChar;
  FmtChunk: array [0 .. 3] of AnsiChar;
  DataChunk: array [0 .. 3] of AnsiChar;
  ChunkSize, FormatSize, DataSize: DWORD;
  begin

  try
  try
  // Prepare the RIFF header
  RIFFHeader := 'RIFF';
  WAVFile.Write(RIFFHeader, SizeOf(RIFFHeader));
  ChunkSize := 36 + PCMFile.Size;
  WAVFile.Write(ChunkSize, SizeOf(ChunkSize));

  WaveChunk := 'WAVE';
  WAVFile.Write(WaveChunk, SizeOf(WaveChunk));

  // Prepare the fmt subchunk
  FmtChunk := 'fmt ';
  WAVFile.Write(FmtChunk, SizeOf(FmtChunk));
  FormatSize := SizeOf(TWaveFormatEx);
  WAVFile.Write(FormatSize, SizeOf(FormatSize));

  // Setup WAVE format
  FillChar(WaveHeader, SizeOf(TWaveFormatEx), 0);
  WaveHeader.wFormatTag := WAVE_FORMAT_PCM;
  WaveHeader.nChannels := 1; // Change as required
  WaveHeader.nSamplesPerSec := 44100; // Change as required
  WaveHeader.nAvgBytesPerSec := 44100 * 1 * 2; // sample_rate * num_channels * bits_per_sample/8
  WaveHeader.nBlockAlign := 2; // num_channels * bits_per_sample/8
  WaveHeader.wBitsPerSample := 16; // Change as required

  WAVFile.Write(WaveHeader, SizeOf(TWaveFormatEx));

  // Prepare the data subchunk
  DataChunk := 'data';
  WAVFile.Write(DataChunk, SizeOf(DataChunk));
  DataSize := PCMFile.Size;
  WAVFile.Write(DataSize, SizeOf(DataSize));

  // Copy PCM data to WAV file
  WAVFile.CopyFrom(PCMFile, 0);
  finally
  end;
  finally
  end;
  end;
}

{$IFDEF ANDROID}

class procedure TAudioMonitor.ConvertPCMToWAV(PCMFile, WAVFile: TMemoryStream);
var
  RiffHeader: TRiffHeader;
  FmtChunk: TFmtChunk;
  DataChunk: TDataChunk;
  PCMDataSize: LongWord;
begin

  Var
    FileName: String;

  FileName := 'Pregunta' + Random(22000).ToString + '.pcm';
  FileName := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetSharedDocumentsPath, FileName);
  PCMFile.Position := 0;
  PCMFile.SaveToFile(FileName);

  PCMFile.Position := 0;

  // Riff Header
  RiffHeader.ChunkID := 'RIFF';
  RiffHeader.ChunkSize := 36 + PCMFile.Size;
  RiffHeader.Format := 'WAVE';

  // Fmt Chunk
  FmtChunk.Subchunk1ID := 'fmt ';
  FmtChunk.Subchunk1Size := 16;
  FmtChunk.AudioFormat := 1; // PCM
  FmtChunk.NumChannels := 1; // Mono
  FmtChunk.SampleRate := 44100;
  FmtChunk.ByteRate := FmtChunk.SampleRate * FmtChunk.NumChannels * 2; // SampleRate * NumChannels * BytesPerSample
  FmtChunk.BlockAlign := FmtChunk.NumChannels * 2; // NumChannels * BytesPerSample
  FmtChunk.BitsPerSample := 16;

  // Data Chunk
  DataChunk.Subchunk2ID := 'data';
  DataChunk.Subchunk2Size := PCMFile.Size;

  // Escribir las cabeceras al archivo
  WAVFile.Write(RiffHeader, SizeOf(RiffHeader));
  WAVFile.Write(FmtChunk, SizeOf(FmtChunk));
  WAVFile.Write(DataChunk, SizeOf(DataChunk));

  // Escribir los datos PCM al archivo
  WAVFile.CopyFrom(PCMFile, 0);
  WAVFile.Position := 0;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}

class procedure TAudioMonitor.ConvertPCMToWAV(PCMFile, WAVFile: TMemoryStream);
var
  RiffHeader: TRiffHeader;
  FmtChunk: TFmtChunk;
  DataChunk: TDataChunk;
  PCMDataSize: LongWord;
begin

  Var
    FileName: String;

  FileName := 'Pregunta' + Random(22000).ToString + '.pcm';
  FileName := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetSharedDocumentsPath, FileName);
  PCMFile.Position := 0;
  PCMFile.SaveToFile(FileName);

  PCMFile.Position := 0;

  // Riff Header
  RiffHeader.ChunkID := 'RIFF';
  RiffHeader.ChunkSize := 36 + PCMFile.Size;
  RiffHeader.Format := 'WAVE';

  // Fmt Chunk
  FmtChunk.Subchunk1ID := 'fmt ';
  FmtChunk.Subchunk1Size := 16;
  FmtChunk.AudioFormat := 1; // PCM
  FmtChunk.NumChannels := 1; // Mono
  FmtChunk.SampleRate := 44100;
  FmtChunk.ByteRate := FmtChunk.SampleRate * FmtChunk.NumChannels * 2; // SampleRate * NumChannels * BytesPerSample
  FmtChunk.BlockAlign := FmtChunk.NumChannels * 2; // NumChannels * BytesPerSample
  FmtChunk.BitsPerSample := 16;

  // Data Chunk
  DataChunk.Subchunk2ID := 'data';
  DataChunk.Subchunk2Size := PCMFile.Size;

  // Escribir las cabeceras al archivo
  WAVFile.Write(RiffHeader, SizeOf(RiffHeader));
  WAVFile.Write(FmtChunk, SizeOf(FmtChunk));
  WAVFile.Write(DataChunk, SizeOf(DataChunk));

  // Escribir los datos PCM al archivo
  WAVFile.CopyFrom(PCMFile, 0);
  WAVFile.Position := 0;

end;
{$ENDIF}

procedure TAudioMonitor.SetSensitivity(const Value: Integer);
begin
  FSensitivity := Value;
end;

procedure TAudioMonitor.SetActive(const Value: Boolean);
begin
  If Value <> FActive then
  Begin
    If Value then
      Start
    Else
      Stop;
    FActive := Value;
  End;
end;

procedure TAudioMonitor.SetOnChangeState(const Value: TAudioMonitorOnChange);
begin
  FOnChangeState := Value;
end;

procedure TAudioMonitor.SetSoundLevel(const Value: Int64);
Var
  I: Integer;
begin
  // FCS.Acquire;
  Try
    FSoundLevel := Value;

    For I := 0 to Length(ArrBuf) - 2 do
      ArrBuf[I] := ArrBuf[I + 1];

    If FSoundLevel > FSensitivity then
      ArrBuf[Length(ArrBuf) - 1] := True
    Else
      ArrBuf[Length(ArrBuf) - 1] := False;

    CalcSilencio;
  Finally
    // FCS.Release;
  End;
end;

procedure TAudioMonitor.Start;
Begin
{$IFDEF MSWINDOWS}
  StartCaptureAudioWindows;
{$ENDIF}
{$IFDEF ANDROID}
  if FAudioRecord = nil then
    Exit;

  // Crear un hilo para capturar el audio
  FCaptureThread := TThread.CreateAnonymousThread(StartCaptureAudioAndroid);
  FCaptureThread.FreeOnTerminate := True;

  FAudioRecord.startRecording;
  FCaptureThread.Start;
{$ENDIF}
end;

procedure TAudioMonitor.Stop;
begin
{$IFDEF MSWINDOWS}
  StopCaptureAudioWindows;
{$ENDIF}
{$IFDEF ANDROID}
  StopCaptureAudioAndroid;

  if FAudioRecord <> nil then
    FAudioRecord.release;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

procedure TAudioMonitor.StartCaptureAudioWindows;
var
  WaveFormat: TWaveFormatEx;
  Res: MMRESULT;
begin
  FActive := True;

  Done := False;

  // Define the audio format
  with WaveFormat do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    nChannels := 1;
    nSamplesPerSec := 44100;
    nAvgBytesPerSec := nSamplesPerSec * 2; // 44100 samples/sec * 2 bytes/sample
    nBlockAlign := 2;
    wBitsPerSample := 16;
    cbSize := 0;
  end;

  // Open the audio input device with callback
  Res := waveInOpen(@hWaveIn, WAVE_MAPPER, @WaveFormat, DWORD_PTR(@AudioCallback), DWORD_PTR(Self), CALLBACK_FUNCTION);
  if Res <> MMSYSERR_NOERROR then
  begin
    Raise Exception.Create('Error al abrir el dispositivo de entrada de audio.');
    Exit;
  end;

  // Prepare the buffer header
  FillChar(WaveHdr, SizeOf(WaveHdr), 0);
  FillChar(Buffer[0], SizeOf(Buffer), 0);
  WaveHdr.lpData := @Buffer[0];
  WaveHdr.dwBufferLength := BUFFER_SIZE;
  waveInPrepareHeader(hWaveIn, @WaveHdr, SizeOf(WaveHdr));

  // Add the buffer to the input queue
  waveInAddBuffer(hWaveIn, @WaveHdr, SizeOf(WaveHdr));

  // Start capturing audio
  Res := waveInStart(hWaveIn);
  if Res <> MMSYSERR_NOERROR then
  begin
    Raise Exception.Create('Error al iniciar la captura de audio.');
    waveInClose(hWaveIn);
    Exit;
  end;
end;

procedure TAudioMonitor.StopCaptureAudioWindows;
begin
  Done := True;
  FActive := False;

  // Stop capturing audio
  waveInStop(hWaveIn);

  // Unprepare the buffer
  waveInUnprepareHeader(hWaveIn, @WaveHdr, SizeOf(WaveHdr));

  // Close the audio input device
  waveInClose(hWaveIn);
end;

{$ENDIF}
{$IFDEF ANDROID}

Procedure TAudioMonitor.StartCaptureAudioAndroid;
const
  blockSizeMillisec = 100;
  SampleRate = SAMPLE_RATE;
var
  blockSize: Integer;
  JBuffer: TJavaArray<SmallInt>;
  I: Integer;
begin
  blockSize := (SampleRate div 1000) * blockSizeMillisec; // 2 bytes per sample for 16-bit PCM data
  JBuffer := TJavaArray<SmallInt>.Create(blockSize);

  while not TThread.CurrentThread.CheckTerminated do
  begin
    if FAudioRecord.read(JBuffer, 0, blockSize) = blockSize then
    begin
      // Copiar los datos desde el buffer de Java al array de Delphi

      FillChar(Buffer[0], BUFFER_SIZE, 0);

      for I := 0 to blockSize - 1 do
      Begin
        Buffer[2 * I] := Lo(JBuffer.Items[I]);
        Buffer[2 * I + 1] := Hi(JBuffer.Items[I]);
      End;

      // Usar TThread.Queue para no bloquear el hilo de captura
      TThread.Queue(nil, CalcSoundLevel);
    end;
  end;
end;

procedure TAudioMonitor.StopCaptureAudioAndroid;
Begin
  if FAudioRecord = nil then
    Exit;

  FAudioRecord.Stop;
  if FCaptureThread <> nil then
  begin
    FCaptureThread.Terminate;
    FCaptureThread := nil;
  end;
End;

{$ENDIF}

Initialization

FSoundMonitor := TAudioMonitor.Create;
// FCS := TCriticalSection.Create;

Finalization

FSoundMonitor.Free;
// FCS.Free;

end.
