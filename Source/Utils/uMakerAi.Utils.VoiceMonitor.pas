// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

{

  ## 📊 Códigos de error de AudioRecord:

  | Código | Constante | Significado | Acción |
  |--------|-----------|-------------|--------|
  | **-3** | `ERROR_INVALID_OPERATION` | AudioRecord en estado inválido | ❌ Abortar |
  | **-2** | `ERROR_BAD_VALUE` | Parámetros inválidos | ❌ Abortar |
  | **-6** | `ERROR_DEAD_OBJECT` | El objeto murió | ❌ Abortar |
  | **-1** | `ERROR` | Error genérico | ⚠️ Reintentar |
  | **0** | - | No se leyó nada | ⚠️ Reintentar |
  | **>0** | - | Bytes leídos exitosamente | ✅ Procesar |

  ---

  ## 🔄 Flujo de la función:
  ```
  Inicio del loop
  ↓
  ¿AudioRecord válido y grabando?
  ├─ NO → Abortar con error
  └─ SÍ ↓
  Intentar leer buffer
  ↓
  ¿BytesRead > 0?
  ├─ SÍ → Procesar audio, resetear errores, continuar
  ├─ = 0 → Incrementar errores, ¿>20? → Abortar : Reintentar
  └─ < 0 → Analizar código de error
  ├─ Error grave (-3, -2, -6) → Abortar
  └─ Error transitorio (-1) → ¿>10 consecutivos?
  ├─ SÍ → Abortar
  └─ NO → Sleep(50), Reintentar
  ↓
  ¿Terminado o error fatal?
  ├─ SÍ → Salir del loop
  └─ NO → Continuar loop
  ↓
  Finally: Liberar JBuffer


  🎛️ Tabla de valores recomendados:

  Entorno	SensitivityMultiplier	StopSensitivityMultiplier
  Silencioso (oficina)	3.0 - 4.0	2.0 - 2.5
  Normal (casa)	2.0 - 2.5	1.5 - 1.8
  Ruidoso (calle)	1.5 - 2.0	1.2 - 1.5
  Micrófono lejano	1.2 - 1.8	1.0 - 1.2



}

unit uMakerAi.Utils.VoiceMonitor;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  System.IOUtils, System.SyncObjs, System.Math, System.Permissions,
  System.Threading, System.Diagnostics,

{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.MMSystem;
{$ENDIF}
{$IFDEF ANDROID}
AndroidApi.JNI.Media, AndroidApi.JNIBridge, AndroidApi.Helpers, FMX.Helpers.Android, AndroidApi.JNI.JavaTypes;
{$ENDIF}

const
  DEFAULT_SAMPLE_RATE = 44100;
  DEFAULT_CHANNELS = 1;
  DEFAULT_BITS_PER_SAMPLE = 16;
  DEFAULT_BUFFER_DURATION_MS = 100;
  DEFAULT_CALIBRATION_DURATION_SEC = 3;
  DEFAULT_SILENCE_DURATION_MS = 700;
  DEFAULT_WAKE_WORD_DURATION_MS = 2500;
  DEFAULT_TRANSCRIPTION_INTERVAL_MS = 3000;
  DEFAULT_TRANSCRIPTION_MAX_WAIT_MS = 4000;
  DEFAULT_FRAGMENT_SPLIT_RATIO = 0.35;

type

{$IFDEF MSWINDOWS}
  // Listar los dispositivos de windows disponibles para captura de audio
  TWaveInDeviceInfo = record
    DeviceID: UINT;
    DeviceName: string;
  end;
{$ENDIF}

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

  TAiMonitorState = (msIdle, msRequestingPermission, msCalibrating, msMonitoring, msError);

  // IMPORTANTE: El stream (aStream) es válido SOLO durante la ejecución del evento.
  // Si necesita retener los datos, debe copiar el stream a una instancia local.
  // El stream será liberado automáticamente después de que retorne el evento.
  TAIVoiceMonitorOnChange = procedure(Sender: TObject; aUserSpeak: Boolean; aIsValidForIA: Boolean; aStream: TMemoryStream) of object;

  // IMPORTANTE: El stream (aStream) es válido SOLO durante la ejecución del evento.
  // Si necesita retener los datos, debe copiar el stream a una instancia local.
  // El stream será liberado automáticamente después de que retorne el evento.
  TSpeechEndEvent = procedure(Sender: TObject; aIsValidForIA: Boolean; aStream: TMemoryStream) of object;

  // IMPORTANTE: El stream (aFragmentStream) es válido SOLO durante la ejecución del evento.
  // Si necesita retener los datos, debe copiar el stream a una instancia local.
  // El stream será liberado automáticamente después de que retorne el evento.
  TTranscriptionFragmentEvent = procedure(Sender: TObject; aFragmentStream: TMemoryStream) of object;

  // IMPORTANTE: El stream (aWakeWordStream) es válido SOLO durante la ejecución del evento.
  // Si necesita retener los datos, debe copiar el stream a una instancia local.
  TWakeWordCheckEvent = procedure(Sender: TObject; aWakeWordStream: TMemoryStream; var IsValid: Boolean) of object;

  TAIVoiceMonitorOnCalibrated = procedure(Sender: TObject; const aNoiseLevel, aSensitivity, aStopSensitivity: Integer) of object;
  TAIVoiceMonitorOnUpdate = procedure(Sender: TObject; const aSoundLevel: Int64) of object;
  TAIVoiceMonitorOnError = procedure(Sender: TObject; const ErrorMessage: string) of object;

  TAIVoiceMonitor = class(TComponent)
  private
    FCS: TCriticalSection;
    FBuffer: TBytes;
    FArrBuf: TArray<Boolean>;
    FFileStream: TMemoryStream;

    FIsSpeaking: Boolean;
    FActive: Boolean;
    FMonitorState: TAiMonitorState;
    FSoundLevel: Int64;
    FInDestroy: Boolean;

    FSensitivity: Integer;
    FStopSensitivity: Integer;
    FCalibrationSamples: Integer;
    FCalibrationAccumulator: Int64;
    FCalibrationDurationSec: Integer;
    FWakeWordChecked: Boolean;
    FIsWakeWordValid: Boolean;
    FWakeWordDurationMs: Integer;
    FSilenceDuration: Integer;
    FSampleRate: Integer;
    FChannels: Integer;
    FBitsPerSample: Integer;
    FBufferSize: Integer;
    FSensitivityMultiplier: Double;
    FStopSensitivityMultiplier: Double;

    FTranscriptionStopwatch: TStopwatch;
    FTranscriptionIntervalMs: Integer;
    FTranscriptionMaxWaitMs: Integer;
    FWaitingForFragmentSplit: Boolean;
    FLastTranscriptionPosition: Int64;
    FPeakLevelInFragment: Int64;
    FFragmentSplitRatio: Double;

    FPreBuffer: TMemoryStream; // Buffer de pre-grabación
    FPreBufferDurationMs: Integer; // Duración del pre-buffer (ej: 500ms)
    FPreBufferMaxSize: Int64; // Tamaño máximo del pre-buffer

    FOnTranscriptionFragment: TTranscriptionFragmentEvent;
    FOnChangeState: TAIVoiceMonitorOnChange;
    FOnCalibrated: TAIVoiceMonitorOnCalibrated;
    FOnUpdate: TAIVoiceMonitorOnUpdate;
    FOnError: TAIVoiceMonitorOnError;
    FOnWakeWordCheck: TWakeWordCheckEvent;

{$IFDEF MSWINDOWS}
    FhWaveIn: HWAVEIN;
    FWaveHdr: TWaveHdr;
    FNoiseLevel: Integer;
    FDeviceID: UINT;
    FWakeWordActive: Boolean;
    FWakeWord: String;
    FOnSpeechEnd: TSpeechEndEvent;
{$ENDIF}
{$IFDEF ANDROID}
    FAudioRecord: JAudioRecord;
    FCaptureThread: TThread;
{$ENDIF}
    procedure SetActive(const Value: Boolean);
    procedure SetSilenceDuration(const Value: Integer);
    procedure DoError(const aMessage: string);

    procedure StartCapture;
    procedure StopCapture;
    procedure UpdateAudioBuffers;
    procedure FireTranscriptionFragment;
    procedure SetBitsPerSample(const Value: Integer);
    procedure SetChannels(const Value: Integer);
    procedure SetSampleRate(const Value: Integer);
    procedure SetDeviceID(const Value: UINT);
    procedure SetWakeWordActive(const Value: Boolean);
    procedure SetWakeWord(const Value: String);
    procedure SetOnSpeechEnd(const Value: TSpeechEndEvent);
    procedure SetPreBufferDurationMs(const Value: Integer);

  protected
    procedure Loaded; override;
    procedure DoChangeState(aIsSpeaking: Boolean);
    procedure ConvertPCMToWAV(PCMStream, WAVStream: TMemoryStream);
    procedure ProcessAudioBuffer(const aBuffer: TBytes; aSize: Integer);
    procedure CalcSilencio;
    procedure UpdatePreBufferSize;
    function GetCurrentDeviceName: string;

{$IFDEF MSWINDOWS}
    procedure StartCaptureAudioWindows;
    procedure StopCaptureAudioWindows;
{$ENDIF}
{$IFDEF ANDROID}
    procedure StartCaptureAudioAndroid;
    procedure StopCaptureAudioAndroid;
    procedure AndroidCaptureLoop;
    procedure HandlePermissionRequest(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
{$ENDIF}
  public
    constructor Create(aOwner: TComponent); Override;
    destructor Destroy; override;
    class function GetWaveInDevices: TArray<TWaveInDeviceInfo>;

    property Active: Boolean read FActive write SetActive;
    property IsSpeaking: Boolean read FIsSpeaking;
    property Sensitivity: Integer read FSensitivity;
    property StopSensitivity: Integer read FStopSensitivity;
    property State: TAiMonitorState read FMonitorState;
    property SoundLevel: Int64 read FSoundLevel;
    property NoiseLevel: Integer read FNoiseLevel;

  published
    property SilenceDuration: Integer read FSilenceDuration write SetSilenceDuration default DEFAULT_SILENCE_DURATION_MS;
    property SensitivityMultiplier: Double read FSensitivityMultiplier write FSensitivityMultiplier;
    property StopSensitivityMultiplier: Double read FStopSensitivityMultiplier write FStopSensitivityMultiplier;
    property WakeWordDurationMs: Integer read FWakeWordDurationMs write FWakeWordDurationMs default DEFAULT_WAKE_WORD_DURATION_MS;
    property TranscriptionIntervalMs: Integer read FTranscriptionIntervalMs write FTranscriptionIntervalMs default DEFAULT_TRANSCRIPTION_INTERVAL_MS;
    property TranscriptionMaxWaitMs: Integer read FTranscriptionMaxWaitMs write FTranscriptionMaxWaitMs default DEFAULT_TRANSCRIPTION_MAX_WAIT_MS;
    property FragmentSplitRatio: Double read FFragmentSplitRatio write FFragmentSplitRatio;
    property OnChangeState: TAIVoiceMonitorOnChange read FOnChangeState write FOnChangeState;
    property OnCalibrated: TAIVoiceMonitorOnCalibrated read FOnCalibrated write FOnCalibrated;
    property OnUpdate: TAIVoiceMonitorOnUpdate read FOnUpdate write FOnUpdate;
    property OnError: TAIVoiceMonitorOnError read FOnError write FOnError;
    property OnWakeWordCheck: TWakeWordCheckEvent read FOnWakeWordCheck write FOnWakeWordCheck;
    property OnSpeechEnd: TSpeechEndEvent read FOnSpeechEnd write SetOnSpeechEnd;
    property OnTranscriptionFragment: TTranscriptionFragmentEvent read FOnTranscriptionFragment write FOnTranscriptionFragment;
    property SampleRate: Integer read FSampleRate write SetSampleRate default DEFAULT_SAMPLE_RATE;
    property Channels: Integer read FChannels write SetChannels default DEFAULT_CHANNELS;
    property BitsPerSample: Integer read FBitsPerSample write SetBitsPerSample default DEFAULT_BITS_PER_SAMPLE;
    property WakeWord: String read FWakeWord write SetWakeWord;
    property WakeWordActive: Boolean read FWakeWordActive write SetWakeWordActive;
    property PreBufferDurationMs: Integer read FPreBufferDurationMs write SetPreBufferDurationMs default 500;
    property DeviceID: UINT read FDeviceID write SetDeviceID default WAVE_MAPPER;
    property CurrentDeviceName: string read GetCurrentDeviceName;
  end;

{$IFDEF MSWINDOWS}
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAIVoiceMonitor]);
end;

{$IFDEF MSWINDOWS}

procedure AudioCallback(HWAVEIN: HWAVEIN; uMsg: UINT; dwInstance, dwParam1, dwParam2: DWORD_PTR); stdcall;
var
  Monitor: TAIVoiceMonitor;
  WaveHdr: PWAVEHDR;
begin
  if uMsg = WIM_DATA then
  begin
    Monitor := TAIVoiceMonitor(dwInstance);
    WaveHdr := PWAVEHDR(dwParam1);
    if Assigned(Monitor) and Assigned(WaveHdr) then
    begin
      Monitor.ProcessAudioBuffer(Monitor.FBuffer, WaveHdr^.dwBytesRecorded);
      if Monitor.Active then
        //waveInAddBuffer(HWAVEIN, @Monitor.FWaveHdr, SizeOf(TWaveHdr));
        waveInAddBuffer(HWAVEIN, WaveHdr, SizeOf(TWaveHdr));
    end;
  end;
end;

class function TAIVoiceMonitor.GetWaveInDevices: TArray<TWaveInDeviceInfo>;
var
  NumDevs, I: Integer;
  Caps: TWaveInCaps;
  // No necesitamos declarar DeviceInfo aquí, usaremos acceso directo al array
begin
  NumDevs := waveInGetNumDevs;

  // Reservamos espacio para los dispositivos físicos + 1 (el Default/Mapper)
  SetLength(Result, NumDevs + 1);

  // 1. Agregamos el Dispositivo Predeterminado (WAVE_MAPPER) en la posición 0
  Result[0].DeviceID := WAVE_MAPPER;
  Result[0].DeviceName := 'Predeterminado del Sistema (WAVE_MAPPER)';

  // 2. Agregamos los dispositivos físicos
  if NumDevs > 0 then
  begin
    for I := 0 to NumDevs - 1 do
    begin
      // Obtenemos capacidades. Nota: Los índices reales de dispositivo empiezan en 0
      if waveInGetDevCaps(I, @Caps, SizeOf(Caps)) = MMSYSERR_NOERROR then
      begin
        // Guardamos en la posición I + 1 del array resultante
        Result[I + 1].DeviceID := I;
        Result[I + 1].DeviceName := Caps.szPname;
      end;
    end;
  end;
end;

{$ENDIF}
{ TAIVoiceMonitor }

constructor TAIVoiceMonitor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FCS := TCriticalSection.Create;
  FFileStream := TMemoryStream.Create;
  FPreBuffer := TMemoryStream.Create;
  FPreBufferDurationMs := 500;

  FInDestroy := False;
  FSampleRate := DEFAULT_SAMPLE_RATE;
  FChannels := DEFAULT_CHANNELS;
  FBitsPerSample := DEFAULT_BITS_PER_SAMPLE;
  FSensitivityMultiplier := 2.0;
  FStopSensitivityMultiplier := 1.5;
  FWakeWordDurationMs := DEFAULT_WAKE_WORD_DURATION_MS;
  FCalibrationDurationSec := DEFAULT_CALIBRATION_DURATION_SEC;
  FSilenceDuration := DEFAULT_SILENCE_DURATION_MS;

  FTranscriptionIntervalMs := DEFAULT_TRANSCRIPTION_INTERVAL_MS;
  FTranscriptionMaxWaitMs := DEFAULT_TRANSCRIPTION_MAX_WAIT_MS;
  FLastTranscriptionPosition := 0;
  FWaitingForFragmentSplit := False;
  FPeakLevelInFragment := 0;
  FFragmentSplitRatio := DEFAULT_FRAGMENT_SPLIT_RATIO;

  FNoiseLevel := 0;

  FIsSpeaking := False;
  FMonitorState := msIdle;
  FSensitivity := 2000;
  FStopSensitivity := 1000;

  FWakeWordActive := False;
  FWakeWord := 'andrea';

{$IFDEF MSWINDOWS}
  FDeviceID := WAVE_MAPPER;
{$ENDIF}
  UpdateAudioBuffers;
end;

destructor TAIVoiceMonitor.Destroy;
begin
  FInDestroy := True;
  Active := False;
  FFileStream.Free;
  FPreBuffer.Free;
  FCS.Free;
  inherited;
end;

procedure TAIVoiceMonitor.UpdateAudioBuffers;
var
  NewSilenceArraySize: Integer;
begin
  // Calcular tamaño del buffer de audio
  FBufferSize := (FSampleRate * FChannels * (FBitsPerSample div 8) * DEFAULT_BUFFER_DURATION_MS) div 1000;

  // Validar que el buffer tenga un tamaño razonable
  if FBufferSize < 100 then
    raise EInvalidOperation.Create('Calculated buffer size is too small. Check audio parameters.');

  if FBufferSize > 1048576 then // 1MB
    raise EInvalidOperation.Create('Calculated buffer size is too large. Check audio parameters.');

  SetLength(FBuffer, FBufferSize);

  // Calcular tamaño del array de detección de silencio
  NewSilenceArraySize := FSilenceDuration div DEFAULT_BUFFER_DURATION_MS;

  // Mínimo 2 buffers para detección
  if NewSilenceArraySize < 2 then
    NewSilenceArraySize := 2;

  SetLength(FArrBuf, NewSilenceArraySize);
end;

procedure TAIVoiceMonitor.UpdatePreBufferSize;
var
  BytesPerSecond: Int64;
begin
  // Calcular cuántos bytes ocupan FPreBufferDurationMs milisegundos
  BytesPerSecond := FSampleRate * FChannels * (FBitsPerSample div 8);
  FPreBufferMaxSize := (BytesPerSecond * FPreBufferDurationMs) div 1000;
end;

procedure TAIVoiceMonitor.Loaded;
begin
  inherited;
  if FActive then
    StartCapture;
end;

procedure TAIVoiceMonitor.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    Exit;

  FActive := Value;

  if Not FActive then
    StopCapture;

  if (csDesigning in ComponentState) or (csLoading in ComponentState) or FInDestroy then
    Exit;
  if FActive then
    StartCapture
  else
    StopCapture;
end;

procedure TAIVoiceMonitor.SetBitsPerSample(const Value: Integer);
begin
  if FBitsPerSample = Value then
    Exit;

  if Active then
    raise EInvalidOperation.Create('Cannot change BitsPerSample while monitor is active. ' + 'Set Active to False first.');

  if not(Value in [8, 16]) then
    raise EArgumentException.CreateFmt('Invalid bits per sample: %d. ' + 'Valid values are: 8 or 16 bits.', [Value]);

  FBitsPerSample := Value;
  UpdateAudioBuffers;
end;

procedure TAIVoiceMonitor.SetChannels(const Value: Integer);
begin
  if FChannels = Value then
    Exit;

  if Active then
    raise EInvalidOperation.Create('Cannot change Channels while monitor is active. ' + 'Set Active to False first.');

  if not(Value in [1, 2]) then
    raise EArgumentException.CreateFmt('Invalid number of channels: %d. ' + 'Valid values are: 1 (mono) or 2 (stereo).', [Value]);

  FChannels := Value;
  UpdateAudioBuffers;
end;

procedure TAIVoiceMonitor.SetDeviceID(const Value: UINT);
{$IFDEF MSWINDOWS}
var
  NumDevs: Integer;
  IsWaveMapper: Boolean;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if FDeviceID = Value then
    Exit;

  if Active then
    raise EInvalidOperation.Create('Cannot change DeviceID while monitor is active. Set Active to False first.');

  // Comprobación robusta para WAVE_MAPPER
  IsWaveMapper := (Value = WAVE_MAPPER);

  if not IsWaveMapper then
  begin
    NumDevs := waveInGetNumDevs;
    // Validamos que el ID esté dentro del rango de dispositivos físicos disponibles
    if Integer(Value) >= NumDevs then
      raise EArgumentOutOfRangeException.CreateFmt('Invalid DeviceID: %d. Valid range is 0 to %d, or WAVE_MAPPER.', [Value, NumDevs - 1]);
  end;

  FDeviceID := Value;
{$ENDIF}
end;

procedure TAIVoiceMonitor.SetOnSpeechEnd(const Value: TSpeechEndEvent);
begin
  FOnSpeechEnd := Value;
end;

procedure TAIVoiceMonitor.SetPreBufferDurationMs(const Value: Integer);
begin
  if FPreBufferDurationMs = Value then
    Exit;

  if Active then
    raise EInvalidOperation.Create('Cannot change PreBufferDurationMs while monitor is active.');

  if (Value < 100) or (Value > 2000) then
    raise EArgumentOutOfRangeException.CreateFmt('PreBufferDurationMs must be between 100 and 2000 ms. Got: %d', [Value]);

  FPreBufferDurationMs := Value;
  UpdatePreBufferSize;
end;

procedure TAIVoiceMonitor.SetSampleRate(const Value: Integer);
const
  // Tasas de muestreo estándar soportadas
  VALID_SAMPLE_RATES: array [0 .. 6] of Integer = (8000, // Telefonía
    11025, // Baja calidad
    16000, // Telefonía wideband / Whisper óptimo
    22050, // Media calidad
    32000, // Radio FM
    44100, // CD Audio
    48000 // Audio profesional / DVD
    );
var
  I: Integer;
  IsValid: Boolean;
  ValidRatesStr: string;
begin
  // Si el valor no cambia, no hacer nada
  if FSampleRate = Value then
    Exit;

  // No permitir cambios mientras el monitor está activo
  if Active then
    raise EInvalidOperation.Create('Cannot change SampleRate while monitor is active. ' + 'Set Active to False first.');

  // Validar que sea una tasa de muestreo válida
  IsValid := False;
  for I := Low(VALID_SAMPLE_RATES) to High(VALID_SAMPLE_RATES) do
  begin
    if Value = VALID_SAMPLE_RATES[I] then
    begin
      IsValid := True;
      Break;
    end;
  end;

  // Si no es válida, lanzar excepción con información útil
  if not IsValid then
  begin
    // Construir string con tasas válidas
    ValidRatesStr := '';
    for I := Low(VALID_SAMPLE_RATES) to High(VALID_SAMPLE_RATES) do
    begin
      if I > 0 then
        ValidRatesStr := ValidRatesStr + ', ';
      ValidRatesStr := ValidRatesStr + IntToStr(VALID_SAMPLE_RATES[I]);
    end;

    raise EArgumentException.CreateFmt('Invalid sample rate: %d Hz. ' + 'Valid rates are: %s Hz.', [Value, ValidRatesStr]);
  end;

  // Asignar el nuevo valor
  FSampleRate := Value;

  // Recalcular los buffers con la nueva tasa de muestreo
  UpdateAudioBuffers;
end;

procedure TAIVoiceMonitor.SetSilenceDuration(const Value: Integer);
begin
  FSilenceDuration := Max(300, Value);
end;

procedure TAIVoiceMonitor.SetWakeWordActive(const Value: Boolean);
begin
  FWakeWordActive := Value;
end;

procedure TAIVoiceMonitor.SetWakeWord(const Value: String);
begin
  FWakeWord := Value;
end;

procedure TAIVoiceMonitor.StartCapture;
begin
  UpdateAudioBuffers;
  FCS.Enter;
  try
    FMonitorState := msCalibrating;
    FCalibrationSamples := 0;
    FCalibrationAccumulator := 0;
    FNoiseLevel := 0;
    FIsSpeaking := False;
    FWakeWordChecked := False;
    FIsWakeWordValid := False;
    if Length(FArrBuf) > 0 then
      FillChar(FArrBuf[0], Length(FArrBuf) * SizeOf(Boolean), 0);
    FFileStream.Clear;
  finally
    FCS.Leave;
  end;
{$IFDEF MSWINDOWS}
  StartCaptureAudioWindows;
{$ENDIF}
{$IFDEF ANDROID}
  FMonitorState := msRequestingPermission;
  PermissionsService.RequestPermissions([TPermission.RecordAudio], HandlePermissionRequest);
{$ENDIF}
end;

procedure TAIVoiceMonitor.StopCapture;
begin
{$IFDEF MSWINDOWS}
  StopCaptureAudioWindows;
{$ENDIF}
{$IFDEF ANDROID}
  StopCaptureAudioAndroid;
{$ENDIF}
  FCS.Enter;
  try
    if FIsSpeaking then
    begin
      FIsSpeaking := False;
      TThread.Queue(nil,
        procedure
        begin
          if not(csDestroying in ComponentState) then
            DoChangeState(False);
        end);
    end;
    FMonitorState := msIdle;
  finally
    FCS.Leave;
  end;
end;

procedure TAIVoiceMonitor.ProcessAudioBuffer(const aBuffer: TBytes; aSize: Integer);
var
  Sum, CurrentLevel, BytesPerSecond, CurrentDurationMs: Int64;
  NumSamples, I, TotalCalibrationSamplesNeeded, NoiseLevel: Integer;
  SampleValue: SmallInt;
  isSilentMoment, isWaitTooLong: Boolean;
  TempBuffer: TMemoryStream;
begin
  if aSize = 0 then
    Exit;

  Sum := 0;
  NumSamples := aSize div (FBitsPerSample div 8);

  if NumSamples = 0 then
    Exit;

  // Calcular el nivel de audio actual
  for I := 0 to NumSamples - 1 do
  begin
    SampleValue := PSmallInt(@aBuffer[I * 2])^;
    Sum := Sum + Abs(SampleValue);
  end;

  CurrentLevel := Sum div NumSamples;

  FCS.Enter;
  try
    FSoundLevel := CurrentLevel;

    case FMonitorState of
      msCalibrating:
        begin
          FCalibrationAccumulator := FCalibrationAccumulator + CurrentLevel;
          Inc(FCalibrationSamples);
          TotalCalibrationSamplesNeeded := (FCalibrationDurationSec * 1000) div DEFAULT_BUFFER_DURATION_MS;

          if FCalibrationSamples >= TotalCalibrationSamplesNeeded then
          begin
            if FCalibrationSamples > 0 then
            begin
              NoiseLevel := FCalibrationAccumulator div FCalibrationSamples;
              FNoiseLevel := NoiseLevel;
              FSensitivity := Max(500, Round(NoiseLevel * FSensitivityMultiplier));
              FStopSensitivity := Max(250, Round(NoiseLevel * FStopSensitivityMultiplier));

              if FStopSensitivity >= FSensitivity then
                FStopSensitivity := FSensitivity - 1;
            end
            else
            begin
              FNoiseLevel := 0;
              FSensitivity := 1500;
              FStopSensitivity := 750;
            end;

            FMonitorState := msMonitoring;

            TThread.Queue(nil,
              procedure
              begin
                if Assigned(FOnCalibrated) then
                  FOnCalibrated(Self, NoiseLevel, FSensitivity, FStopSensitivity);
              end);
          end;
        end;

      msMonitoring:
        begin
          // *** GESTIÓN DEL PRE-BUFFER ***
          if not FIsSpeaking then
          begin
            // Mientras NO estamos hablando, acumular audio en el pre-buffer
            FPreBuffer.WriteBuffer(aBuffer, aSize);

            // Si el pre-buffer excede el tamaño máximo, mantener solo los últimos datos
            if FPreBuffer.Size > FPreBufferMaxSize then
            begin
              // Crear buffer temporal para mantener solo los últimos FPreBufferMaxSize bytes
              TempBuffer := TMemoryStream.Create;
              try
                // Posicionarse en el punto donde comienzan los últimos N bytes
                FPreBuffer.Position := FPreBuffer.Size - FPreBufferMaxSize;

                // Copiar solo los últimos bytes al buffer temporal
                TempBuffer.CopyFrom(FPreBuffer, FPreBufferMaxSize);

                // Limpiar el pre-buffer y copiar de vuelta el contenido reducido
                FPreBuffer.Clear;
                TempBuffer.Position := 0;
                FPreBuffer.CopyFrom(TempBuffer, 0);
              finally
                TempBuffer.Free;
              end;
            end;
          end
          else
          begin
            // Ya estamos hablando, guardar directamente en FFileStream
            FFileStream.WriteBuffer(aBuffer, aSize);

            // --- FRAGMENTACIÓN PARA TRANSCRIPCIÓN EN STREAMING ---
            if Assigned(FOnTranscriptionFragment) then
            begin
              // Actualizar el pico de nivel en el fragmento actual
              if CurrentLevel > FPeakLevelInFragment then
                FPeakLevelInFragment := CurrentLevel;

              // Verificar si ya pasó el intervalo mínimo para enviar fragmento
              if not FWaitingForFragmentSplit and (FTranscriptionStopwatch.ElapsedMilliseconds >= FTranscriptionIntervalMs) then
                FWaitingForFragmentSplit := True;

              // Si estamos esperando para dividir, buscar un momento apropiado
              if FWaitingForFragmentSplit then
              begin
                // Momento silencioso relativo: el nivel actual bajó significativamente respecto al pico
                isSilentMoment := (FPeakLevelInFragment > FSensitivity) and (CurrentLevel < (FPeakLevelInFragment * FFragmentSplitRatio));

                // Tiempo máximo de espera excedido
                isWaitTooLong := FTranscriptionStopwatch.ElapsedMilliseconds >= FTranscriptionMaxWaitMs;

                // Enviar fragmento si encontramos un momento silencioso o esperamos demasiado
                if isSilentMoment or isWaitTooLong then
                begin
                  FireTranscriptionFragment;
                  FWaitingForFragmentSplit := False;
                end;
              end;
            end;

            // --- VERIFICACIÓN DE WAKE WORD ---
            if not FWakeWordChecked and Assigned(FOnWakeWordCheck) then
            begin
              BytesPerSecond := FSampleRate * FChannels * (FBitsPerSample div 8);
              CurrentDurationMs := 0;

              if BytesPerSecond > 0 then
                CurrentDurationMs := (FFileStream.Size * 1000) div BytesPerSecond;

              // Si ya tenemos suficiente audio para verificar la wake word
              if CurrentDurationMs >= FWakeWordDurationMs then
              begin
                FWakeWordChecked := True;

                // Ejecutar verificación en tarea asíncrona para no bloquear captura
                TTask.Run(
                  procedure
                  var
                    WakeStreamPCM, WakeStreamWAV: TMemoryStream;
                    FragmentSize: Int64;
                    IsValid: Boolean;
                  begin
                    WakeStreamPCM := TMemoryStream.Create;
                    try
                      // Extraer el fragmento de wake word del inicio del audio
                      FCS.Enter;
                      try
                        FFileStream.Position := 0;
                        FragmentSize := (FWakeWordDurationMs * BytesPerSecond) div 1000;

                        if FragmentSize > FFileStream.Size then
                          FragmentSize := FFileStream.Size;

                        WakeStreamPCM.CopyFrom(FFileStream, FragmentSize);
                        FFileStream.Position := FFileStream.Size;
                      finally
                        FCS.Leave;
                      end;

                      // Convertir a WAV para enviar al verificador
                      WakeStreamWAV := TMemoryStream.Create;
                      try
                        ConvertPCMToWAV(WakeStreamPCM, WakeStreamWAV);
                        IsValid := False;

                        // Solo llamar al evento si la wake word está activa
                        if Assigned(FOnWakeWordCheck) and FWakeWordActive then
                          FOnWakeWordCheck(Self, WakeStreamWAV, IsValid);
                      finally
                        WakeStreamWAV.Free;
                      end;

                      // Guardar el resultado de la validación
                      FCS.Enter;
                      try
                        FIsWakeWordValid := IsValid;
                      finally
                        FCS.Leave;
                      end;
                    finally
                      WakeStreamPCM.Free;
                    end;
                  end);
              end;
            end;
          end;

          // --- ACTUALIZACIÓN DEL BUFFER DE DETECCIÓN DE SILENCIO ---
          if Length(FArrBuf) > 0 then
          begin
            // Desplazar array una posición a la izquierda
            for I := 0 to High(FArrBuf) - 1 do
              FArrBuf[I] := FArrBuf[I + 1];

            // Agregar nueva medición al final
            if FIsSpeaking then
              FArrBuf[High(FArrBuf)] := (CurrentLevel > FStopSensitivity)
            else
              FArrBuf[High(FArrBuf)] := (CurrentLevel > FSensitivity);
          end;
        end;
    end;
  finally
    FCS.Leave;
  end;

  // --- NOTIFICAR NIVEL DE AUDIO AL EVENTO OnUpdate ---
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnUpdate) then
        FOnUpdate(Self, CurrentLevel);
    end);

  // --- CALCULAR DETECCIÓN DE SILENCIO ---
  if FMonitorState = msMonitoring then
    CalcSilencio;
end;

procedure TAIVoiceMonitor.CalcSilencio;
var
  I: Integer;
  AllFalse, PrevState, NewState: Boolean;
begin
  FCS.Enter;
  try
    PrevState := FIsSpeaking;
    NewState := FIsSpeaking;
    if (not FIsSpeaking) and (Length(FArrBuf) >= 2) and (FArrBuf[High(FArrBuf) - 1]) and (FArrBuf[High(FArrBuf)]) then
      NewState := True
    else if FIsSpeaking then
    begin
      AllFalse := True;
      for I := 0 to High(FArrBuf) do
      begin
        if FArrBuf[I] then
        begin
          AllFalse := False;
          Break;
        end;
      end;
      if AllFalse then
        NewState := False;
    end;
    if NewState <> PrevState then
    begin
      FIsSpeaking := NewState;
      TThread.Queue(nil,
        procedure
        begin
          DoChangeState(FIsSpeaking);
        end);
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TAIVoiceMonitor.DoChangeState(aIsSpeaking: Boolean);
var
  WAVStream: TMemoryStream;
begin
  if aIsSpeaking then
  begin
    FCS.Enter;
    try
      FWakeWordChecked := False;
      FIsWakeWordValid := False;
      FFileStream.Clear;

      // *** COPIAR PRE-BUFFER AL INICIO DEL FILESTREAM ***
      if FPreBuffer.Size > 0 then
      begin
        FPreBuffer.Position := 0;
        FFileStream.CopyFrom(FPreBuffer, 0);
        // *** LIMPIAR EL PRE-BUFFER DESPUÉS DE USARLO ***
        FPreBuffer.Clear;
      end;

      FLastTranscriptionPosition := 0;
      FWaitingForFragmentSplit := False;
      FPeakLevelInFragment := 0;
      FTranscriptionStopwatch.Reset;
      FTranscriptionStopwatch.Start;
    finally
      FCS.Leave;
    end;

    if Assigned(FOnChangeState) then
      FOnChangeState(Self, True, False, nil);
  end
  else
  begin
    FTranscriptionStopwatch.Stop;
    FTranscriptionStopwatch.Reset;

    if Assigned(FOnTranscriptionFragment) then
      FireTranscriptionFragment;

    FWaitingForFragmentSplit := False;
    FPeakLevelInFragment := 0;

    FCS.Enter;
    try
      FPreBuffer.Clear;
    finally
      FCS.Leave;
    end;

    WAVStream := TMemoryStream.Create;
    Try
      FCS.Enter;
      try
        ConvertPCMToWAV(FFileStream, WAVStream);
      finally
        FCS.Leave;
      end;

      WAVStream.Position := 0;

      if Assigned(FOnSpeechEnd) then
        FOnSpeechEnd(Self, FIsWakeWordValid, WAVStream);

      if Assigned(FOnChangeState) then
        FOnChangeState(Self, False, FIsWakeWordValid, WAVStream)
    Finally
      WAVStream.Free;
    End;
  end;
end;

procedure TAIVoiceMonitor.FireTranscriptionFragment;
var
  FragmentPCM, FragmentWAV: TMemoryStream;
  FragmentSize: Int64;
  PeakLevel: Int64;
begin
  // Adquirimos el nivel pico del fragmento actual de forma segura
  FCS.Enter;
  try
    PeakLevel := FPeakLevelInFragment;
  finally
    FCS.Leave;
  end;

  // --- VALIDACIÓN DE FRAGMENTO ---
  // Si el pico de sonido en este fragmento no superó la sensibilidad,
  // lo consideramos silencio y no lo enviamos a la IA.
  if PeakLevel < FSensitivity then
  begin
    // El fragmento es silencio. Lo descartamos, pero debemos actualizar
    // la posición y reiniciar los contadores para el siguiente fragmento.
    FCS.Enter;
    try
      // Marcamos el audio silencioso como 'procesado' para no revisarlo de nuevo
      FLastTranscriptionPosition := FFileStream.Size;
    finally
      FCS.Leave;
    end;

    // Reiniciamos todo para el siguiente ciclo de detección.
    FTranscriptionStopwatch.Reset;
    FTranscriptionStopwatch.Start;
    FPeakLevelInFragment := 0;
    Exit; // Salimos del procedimiento, no hay nada que enviar.
  end;

  // --- EXTRACCIÓN DEL FRAGMENTO ---
  // Si llegamos aquí, el fragmento contiene audio significativo.
  // Procedemos a extraerlo y enviarlo.
  FragmentPCM := TMemoryStream.Create;
  try
    FCS.Enter;
    try
      // Comprobación de seguridad: ¿hay datos nuevos para procesar?
      if FFileStream.Size <= FLastTranscriptionPosition then
        Exit;

      // Calcular tamaño del nuevo fragmento
      FragmentSize := FFileStream.Size - FLastTranscriptionPosition;

      // Copiar el fragmento desde la última posición procesada
      FFileStream.Position := FLastTranscriptionPosition;
      FragmentPCM.CopyFrom(FFileStream, FragmentSize);

      // Actualizar la posición para el próximo fragmento
      FLastTranscriptionPosition := FFileStream.Size;
    finally
      FCS.Leave;
    end;

    // Reiniciamos contadores para el *próximo* fragmento.
    FTranscriptionStopwatch.Reset;
    FTranscriptionStopwatch.Start;
    FPeakLevelInFragment := 0;

    // Si por alguna razón el fragmento está vacío, no continuamos.
    if FragmentPCM.Size = 0 then
      Exit;

    // --- CONVERSIÓN A WAV Y ENVÍO AL EVENTO ---
    FragmentWAV := TMemoryStream.Create;
    try
      // Convertir PCM a formato WAV
      ConvertPCMToWAV(FragmentPCM, FragmentWAV);
      FragmentWAV.Position := 0;

      // Enviar al hilo principal para que dispare el evento
      // IMPORTANTE: El manejador del evento DEBE copiar el stream si necesita
      // retener los datos, ya que será liberado inmediatamente después del evento
      TThread.Queue(nil,
        procedure
        var
          StreamToFree: TMemoryStream;
        begin
          StreamToFree := FragmentWAV;
          try
            // Solo disparar el evento si el componente no se está destruyendo
            if not(csDestroying in ComponentState) and Assigned(FOnTranscriptionFragment) then
              FOnTranscriptionFragment(Self, FragmentWAV);
          finally
            // Siempre liberamos el stream después del evento
            StreamToFree.Free;
          end;
        end);
    except
      // Si hay error en la conversión, liberar el WAV stream
      FragmentWAV.Free;
      raise;
    end;
  finally
    // Siempre liberar el stream PCM
    FragmentPCM.Free;
  end;
end;

function TAIVoiceMonitor.GetCurrentDeviceName: string;
{$IFDEF MSWINDOWS}
var
  Caps: TWaveInCaps;
  IsWaveMapper: Boolean;
{$ENDIF}
begin
  Result := 'Unknown Device';

{$IFDEF MSWINDOWS}
  // Verificar si es WAVE_MAPPER
  IsWaveMapper := (FDeviceID = UINT(-1)) or (Integer(FDeviceID) = -1) or (FDeviceID = $FFFFFFFF);

  if IsWaveMapper then
  begin
    Result := 'Default Device (WAVE_MAPPER)';
    Exit;
  end;

  if waveInGetDevCaps(FDeviceID, @Caps, SizeOf(Caps)) = MMSYSERR_NOERROR then
    Result := Caps.szPname
  else
    Result := Format('Device ID %d (Error getting info)', [FDeviceID]);
{$ENDIF}
end;

procedure TAIVoiceMonitor.DoError(const aMessage: string);
begin
  FMonitorState := msError;
  StopCapture;
  FActive := False;
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnError) then
        FOnError(Self, aMessage);
    end);
end;

procedure TAIVoiceMonitor.ConvertPCMToWAV(PCMStream, WAVStream: TMemoryStream);
var
  RiffHeader: TRiffHeader;
  FmtChunk: TFmtChunk;
  DataChunk: TDataChunk;
begin
  // Validación de parámetros
  if not Assigned(PCMStream) then
    raise EArgumentNilException.Create('PCMStream cannot be nil');
  if not Assigned(WAVStream) then
    raise EArgumentNilException.Create('WAVStream cannot be nil');
  if PCMStream.Size = 0 then
    raise EArgumentException.Create('PCMStream cannot be empty');

  // Preparar streams
  PCMStream.Position := 0;
  WAVStream.Clear;

  // Construir el chunk FMT (24 bytes totales: 8 header + 16 datos)
  FmtChunk.Subchunk1ID := 'fmt ';
  FmtChunk.Subchunk1Size := 16; // Tamaño de los datos del fmt chunk (sin header)
  FmtChunk.AudioFormat := 1; // PCM sin comprimir
  FmtChunk.NumChannels := FChannels;
  FmtChunk.SampleRate := FSampleRate;
  FmtChunk.BitsPerSample := FBitsPerSample;
  FmtChunk.BlockAlign := FChannels * (FBitsPerSample div 8);
  FmtChunk.ByteRate := FSampleRate * FmtChunk.BlockAlign;

  // Construir el chunk DATA (8 bytes header + datos)
  DataChunk.Subchunk2ID := 'data';
  DataChunk.Subchunk2Size := PCMStream.Size;

  // Construir el header RIFF
  RiffHeader.ChunkID := 'RIFF';
  RiffHeader.Format := 'WAVE';

  // ChunkSize = tamaño total del archivo - 8 bytes (ChunkID + ChunkSize)
  // = 4 (WAVE) + 24 (fmt chunk completo) + 8 (data header) + datos PCM
  // = 36 + datos PCM
  RiffHeader.ChunkSize := 36 + PCMStream.Size;

  // Escribir todos los chunks en orden
  WAVStream.WriteBuffer(RiffHeader, SizeOf(RiffHeader));
  WAVStream.WriteBuffer(FmtChunk, SizeOf(FmtChunk));
  WAVStream.WriteBuffer(DataChunk, SizeOf(DataChunk));
  WAVStream.CopyFrom(PCMStream, 0);

  // Posicionar al inicio para lectura posterior
  WAVStream.Position := 0;
end;

{$IFDEF MSWINDOWS}
{
  procedure TAIVoiceMonitor.StartCaptureAudioWindows;
  var
  WaveFormat: TWaveFormatEx;
  Res: MMRESULT;
  begin
  if not FActive then
  Exit;
  WaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  WaveFormat.nChannels := FChannels;
  WaveFormat.nSamplesPerSec := FSampleRate;
  WaveFormat.wBitsPerSample := FBitsPerSample;
  WaveFormat.nBlockAlign := FChannels * (WaveFormat.wBitsPerSample div 8);
  WaveFormat.nAvgBytesPerSec := FSampleRate * WaveFormat.nBlockAlign;
  WaveFormat.cbSize := 0;
  // Res := waveInOpen(@FhWaveIn, WAVE_MAPPER, @WaveFormat, DWORD_PTR(@AudioCallback), DWORD_PTR(Self), CALLBACK_FUNCTION);
  Res := waveInOpen(@FhWaveIn, FDeviceID, @WaveFormat, DWORD_PTR(@AudioCallback), DWORD_PTR(Self), CALLBACK_FUNCTION);
  if Res <> MMSYSERR_NOERROR then
  begin
  DoError('Error al abrir dispositivo de audio: ' + IntToStr(Res));
  Exit;
  end;
  FillChar(FWaveHdr, SizeOf(FWaveHdr), 0);
  FWaveHdr.lpData := @FBuffer[0];
  FWaveHdr.dwBufferLength := FBufferSize;
  waveInPrepareHeader(FhWaveIn, @FWaveHdr, SizeOf(FWaveHdr));
  waveInAddBuffer(FhWaveIn, @FWaveHdr, SizeOf(FWaveHdr));
  Res := waveInStart(FhWaveIn);
  if Res <> MMSYSERR_NOERROR then
  begin
  waveInClose(FhWaveIn);
  DoError('Error al iniciar la captura de audio: ' + IntToStr(Res));
  end;
  end;
}

procedure TAIVoiceMonitor.StartCaptureAudioWindows;
var
  WaveFormat: TWaveFormatEx;
  Res: MMRESULT;
begin
  if not FActive then
    Exit;

  // 1. Configuración del formato de Audio (PCM)
  WaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  WaveFormat.nChannels := FChannels;
  WaveFormat.nSamplesPerSec := FSampleRate;
  WaveFormat.wBitsPerSample := FBitsPerSample;
  WaveFormat.nBlockAlign := FChannels * (WaveFormat.wBitsPerSample div 8);
  WaveFormat.nAvgBytesPerSec := FSampleRate * WaveFormat.nBlockAlign;
  WaveFormat.cbSize := 0;

  // 2. Abrir el dispositivo
  // FDeviceID debe contener el ID del dispositivo específico (0, 1, 2...)
  // O el valor WAVE_MAPPER ($FFFFFFFF) para el dispositivo por defecto.

  //FDeviceID := WAVE_MAPPER;

  Res := waveInOpen(@FhWaveIn, FDeviceID, @WaveFormat, DWORD_PTR(@AudioCallback), DWORD_PTR(Self), CALLBACK_FUNCTION);

  if Res <> MMSYSERR_NOERROR then
  begin
    FhWaveIn := 0;
    DoError('Error al abrir dispositivo de audio (Code: ' + IntToStr(Res) + ').');
    Exit;
  end;

  // 3. Preparar la estructura del Header
  FillChar(FWaveHdr, SizeOf(FWaveHdr), 0);
  FWaveHdr.lpData := @FBuffer[0]; // Puntero al array de bytes
  FWaveHdr.dwBufferLength := FBufferSize; // Tamaño del buffer
  FWaveHdr.dwFlags := 0;

  // 4. Preparar el Header (PrepareHeader)
  Res := waveInPrepareHeader(FhWaveIn, @FWaveHdr, SizeOf(TWaveHdr));
  if Res <> MMSYSERR_NOERROR then
  begin
    // Si falla, cerramos el dispositivo antes de salir
    waveInClose(FhWaveIn);
    FhWaveIn := 0;
    DoError('Error al preparar el header de audio: ' + IntToStr(Res));
    Exit;
  end;

  // 5. Añadir el Buffer a la cola de grabación
  Res := waveInAddBuffer(FhWaveIn, @FWaveHdr, SizeOf(TWaveHdr));
  if Res <> MMSYSERR_NOERROR then
  begin
    // Si falla, "despreparamos" y cerramos
    waveInUnprepareHeader(FhWaveIn, @FWaveHdr, SizeOf(TWaveHdr));
    waveInClose(FhWaveIn);
    FhWaveIn := 0;
    DoError('Error al añadir buffer de audio: ' + IntToStr(Res));
    Exit;
  end;

  // 6. Iniciar la grabación
  Res := waveInStart(FhWaveIn);
  if Res <> MMSYSERR_NOERROR then
  begin
    // Limpieza completa en caso de error crítico al iniciar start
    waveInReset(FhWaveIn);
    waveInUnprepareHeader(FhWaveIn, @FWaveHdr, SizeOf(TWaveHdr));
    waveInClose(FhWaveIn);
    FhWaveIn := 0;
    DoError('Error al iniciar la captura de audio: ' + IntToStr(Res));
  end;
end;

procedure TAIVoiceMonitor.StopCaptureAudioWindows;
begin
  if FhWaveIn <> 0 then
  begin
    waveInStop(FhWaveIn);
    waveInReset(FhWaveIn);
    waveInUnprepareHeader(FhWaveIn, @FWaveHdr, SizeOf(FWaveHdr));
    waveInClose(FhWaveIn);
    FhWaveIn := 0;
  end;
end;
{$ENDIF}
{$IFDEF ANDROID}

procedure TAIVoiceMonitor.HandlePermissionRequest(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  if (Length(AGrantResults) > 0) and (AGrantResults[0] = TPermissionStatus.Granted) then
    StartCaptureAudioAndroid
  else
    DoError('Permiso para grabar audio denegado.');
end;

procedure TAIVoiceMonitor.StartCaptureAudioAndroid;
var
  AudioSource, ChannelConfig, AudioFormat, MinBufferSize: Integer;
begin
  if not FActive then
    Exit;
  try
    AudioSource := TJMediaRecorder_AudioSource.JavaClass.MIC;
    ChannelConfig := TJAudioFormat.JavaClass.CHANNEL_IN_MONO;
    AudioFormat := TJAudioFormat.JavaClass.ENCODING_PCM_16BIT;
    MinBufferSize := TJAudioRecord.JavaClass.getMinBufferSize(FSampleRate, ChannelConfig, AudioFormat);
    if FBufferSize < MinBufferSize then
      FBufferSize := MinBufferSize;
    SetLength(FBuffer, FBufferSize);
    FAudioRecord := TJAudioRecord.JavaClass.init(AudioSource, FSampleRate, ChannelConfig, AudioFormat, FBufferSize);
    if FAudioRecord.getState <> TJAudioRecord.JavaClass.STATE_INITIALIZED then
    begin
      DoError('No se pudo inicializar AudioRecord.');
      Exit;
    end;
    if Assigned(FCaptureThread) then
    begin
      FCaptureThread.Terminate;
      FCaptureThread.WaitFor;
      FCaptureThread := nil;
    end;
    FCaptureThread := TThread.CreateAnonymousThread(AndroidCaptureLoop);
    FCaptureThread.FreeOnTerminate := True;
    FAudioRecord.startRecording;
    FCaptureThread.Start;
  except
    on E: Exception do
      DoError('Excepción al iniciar captura en Android: ' + E.Message);
  end;
end;

procedure TAIVoiceMonitor.StopCaptureAudioAndroid;
var
  LThread: TThread;
begin
  if Assigned(FAudioRecord) then
  begin
    if FAudioRecord.getRecordingState = TJAudioRecord.JavaClass.RECORDSTATE_RECORDING then
      FAudioRecord.Stop;
    if FAudioRecord.getState = TJAudioRecord.JavaClass.STATE_INITIALIZED then
      FAudioRecord.release;
    FAudioRecord := nil;
  end;

  LThread := FCaptureThread;
  if Assigned(LThread) then
  begin
    LThread.Terminate;
    FCaptureThread := nil;
  end;
end;

procedure TAIVoiceMonitor.AndroidCaptureLoop;
var
  JBuffer: TJavaArray<Byte>;
  BytesRead: Integer;
  ErrorCount: Integer;
  ConsecutiveErrors: Integer;
  LastErrorCode: Integer;
begin
  JBuffer := TJavaArray<Byte>.Create(FBufferSize);
  ErrorCount := 0;
  ConsecutiveErrors := 0;
  LastErrorCode := 0;

  try
    while not TThread.CheckTerminated do
    begin
      // Verificar que el AudioRecord siga válido
      if not Assigned(FAudioRecord) then
      begin
        TThread.Queue(nil,
          procedure
          begin
            DoError('AudioRecord instance was released unexpectedly.');
          end);
        Break;
      end;

      // Verificar que esté en estado de grabación
      if FAudioRecord.getRecordingState <> TJAudioRecord.JavaClass.RECORDSTATE_RECORDING then
      begin
        TThread.Queue(nil,
          procedure
          begin
            DoError('AudioRecord is not in recording state.');
          end);
        Break;
      end;

      try
        // Intentar leer datos del buffer de audio
        BytesRead := FAudioRecord.read(JBuffer, 0, FBufferSize);

        // --- CASO 1: Lectura exitosa ---
        if BytesRead > 0 then
        begin
          // Resetear contadores de error en lectura exitosa
          ErrorCount := 0;
          ConsecutiveErrors := 0;
          LastErrorCode := 0;

          // Copiar datos del buffer Java al buffer nativo
          JNI.GetByteArrayRegion(JBuffer.GetObjectID, 0, BytesRead, PByte(FBuffer));

          // Procesar el buffer de audio
          ProcessAudioBuffer(FBuffer, BytesRead);
        end
        // --- CASO 2: Error de lectura ---
        else if BytesRead < 0 then
        begin
          Inc(ErrorCount);
          Inc(ConsecutiveErrors);
          LastErrorCode := BytesRead;

          // Determinar el tipo de error según el código
          case BytesRead of
            TJAudioRecord.JavaClass.ERROR_INVALID_OPERATION:
              begin
                // Error grave: AudioRecord en estado inválido
                TThread.Queue(nil,
                  procedure
                  begin
                    DoError('AudioRecord ERROR_INVALID_OPERATION. Recording stopped.');
                  end);
                Break;
              end;

            TJAudioRecord.JavaClass.ERROR_BAD_VALUE:
              begin
                // Parámetros inválidos
                TThread.Queue(nil,
                  procedure
                  begin
                    DoError('AudioRecord ERROR_BAD_VALUE. Invalid parameters.');
                  end);
                Break;
              end;

            TJAudioRecord.JavaClass.ERROR_DEAD_OBJECT:
              begin
                // El AudioRecord murió
                TThread.Queue(nil,
                  procedure
                  begin
                    DoError('AudioRecord ERROR_DEAD_OBJECT. Recording stopped.');
                  end);
                Break;
              end;

            TJAudioRecord.JavaClass.ERROR:
              begin
                // Error genérico - puede ser transitorio
                if ConsecutiveErrors >= 10 then
                begin
                  // Demasiados errores consecutivos
                  TThread.Queue(nil,
                    procedure
                    begin
                      DoError(Format('Persistent AudioRecord ERROR after %d attempts. Recording stopped.', [ConsecutiveErrors]));
                    end);
                  Break;
                end
                else
                begin
                  // Error transitorio, esperar antes de reintentar
                  Sleep(50);
                end;
              end;

          else
            // Código de error desconocido
            if ConsecutiveErrors >= 10 then
            begin
              TThread.Queue(nil,
                procedure
                begin
                  DoError(Format('Unknown AudioRecord error code: %d after %d attempts. Recording stopped.', [BytesRead, ConsecutiveErrors]));
                end);
              Break;
            end
            else
            begin
              // Esperar antes de reintentar
              Sleep(50);
            end;
          end;

          // Log de diagnóstico cada 5 errores (opcional)
          if (ErrorCount mod 5 = 0) and (ErrorCount > 0) then
          begin
            TThread.Queue(nil,
              procedure
              begin
                if Assigned(FOnError) then
                  FOnError(Self, Format('AudioRecord warning: %d errors detected (last code: %d)', [ErrorCount, LastErrorCode]));
              end);
          end;
        end
        // --- CASO 3: No se leyó nada (BytesRead = 0) ---
        else
        begin
          // BytesRead = 0 es raro pero no necesariamente un error
          Inc(ConsecutiveErrors);

          if ConsecutiveErrors >= 20 then
          begin
            TThread.Queue(nil,
              procedure
              begin
                DoError('AudioRecord returned 0 bytes too many times. Recording stopped.');
              end);
            Break;
          end;

          // Pequeña pausa antes de reintentar
          Sleep(10);
        end;

      except
        on E: Exception do
        begin
          // Excepción en el procesamiento
          Inc(ErrorCount);
          Inc(ConsecutiveErrors);

          if ConsecutiveErrors >= 5 then
          begin
            // Demasiadas excepciones, abortar
            TThread.Queue(nil,
              procedure
              begin
                DoError(Format('Critical exception in audio capture loop: %s. Recording stopped.', [E.Message]));
              end);
            Break;
          end
          else
          begin
            // Log y continuar
            TThread.Queue(nil,
              procedure
              begin
                if Assigned(FOnError) then
                  FOnError(Self, Format('Exception in audio capture (attempt %d): %s', [ConsecutiveErrors, E.Message]));
              end);
            Sleep(100);
          end;
        end;
      end;

      // Pequeña pausa entre iteraciones para no saturar CPU
      // (solo si no hubo errores, ya que los errores ya tienen Sleep)
      if (BytesRead > 0) and (ConsecutiveErrors = 0) then
        Sleep(1);

    end; // while

  finally
    // Liberar el buffer Java
    JBuffer := nil;

    // Log de finalización
    TThread.Queue(nil,
      procedure
      begin
        if ErrorCount > 0 then
        begin
          if Assigned(FOnError) then
            FOnError(Self, Format('Audio capture loop ended. Total errors: %d', [ErrorCount]));
        end;
      end);
  end;
end;

{$ENDIF}

end.
