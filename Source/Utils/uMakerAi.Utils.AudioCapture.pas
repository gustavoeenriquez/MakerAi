// MIT License
//
// Copyright (c) 2026 Gustavo Enriquez
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enriquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

{
  uMakerAi.Utils.AudioCapture
  ===========================

  TAiAudioCapture: componente de captura de audio del SISTEMA (lo que se esta
  reproduciendo por los altavoces/auriculares) usando WASAPI en modo loopback.
  Tambien puede capturar el microfono via WASAPI (Source = asMicrophone).

  Caso de uso tipico: traducir/transcribir el audio de una videollamada
  (Zoom, Meet, Teams, YouTube...) sin cables virtuales ni drivers adicionales.

  Caracteristicas:
  - WASAPI loopback nativo (Windows Vista+), sin dependencias externas.
  - Entrega chunks PCM16 listos para STT: remuestreo (lineal, con continuidad
    entre paquetes) y mezcla a mono integrados.
  - InjectSilence: WASAPI loopback NO entrega paquetes cuando no suena nada;
    el componente puede inyectar silencio para mantener un flujo continuo
    (necesario para VAD / transcripcion en streaming).
  - Integracion directa con el modulo Realtime STT via la propiedad
    RealtimeSTT (igual que TAIVoiceMonitor).

  Uso minimo (traductor):

    Capture := TAiAudioCapture.Create(Self);
    Capture.Source := asLoopback;          // audio de reproduccion
    Capture.OutputSampleRate := 16000;     // optimo para Whisper / Realtime
    Capture.OutputChannels := 1;           // mono
    Capture.OnData := MiManejadorOnData;   // chunks PCM16 cada 100 ms
    Capture.Active := True;

  IMPORTANTE: el evento OnData entrega un TBytes (copia segura); los eventos
  se despachan al hilo principal con TThread.Queue, igual que el resto de la
  suite. En aplicaciones de consola hay que llamar CheckSynchronize.

  Plataformas: solo Windows. En otras plataformas el componente compila pero
  Active := True dispara OnError.
}

unit uMakerAi.Utils.AudioCapture;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.SyncObjs, System.Math,
  System.Diagnostics,
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ActiveX, Winapi.MMSystem,
{$ENDIF}
  uMakerAi.Realtime;

const
  DEFAULT_AC_CHUNK_DURATION_MS = 100;
  DEFAULT_AC_OUTPUT_SAMPLE_RATE = 16000;
  DEFAULT_AC_OUTPUT_CHANNELS = 1;

type
  // Fuente de captura:
  // asLoopback   = lo que el sistema esta reproduciendo (altavoces/auriculares)
  // asMicrophone = dispositivo de grabacion (microfono) via WASAPI
  TAiAudioSource = (asLoopback, asMicrophone);

  TAiAudioDeviceInfo = record
    EndpointId: string; // ID unico WASAPI del endpoint (usar en DeviceId)
    DeviceName: string; // Nombre amigable del dispositivo
    IsDefault: Boolean; // True si es el dispositivo predeterminado
  end;

  // aBuffer es PCM16 entrelazado en el formato indicado (copia segura,
  // se puede retener sin problemas).
  TAiAudioDataEvent = procedure(Sender: TObject; const aBuffer: TBytes; aSampleRate, aChannels: Integer) of object;
  TAiAudioLevelEvent = procedure(Sender: TObject; const aSoundLevel: Int64) of object;
  TAiAudioErrorEvent = procedure(Sender: TObject; const ErrorMessage: string) of object;
  // Se dispara al iniciar la captura, cuando ya se conoce el formato nativo
  // del dispositivo y el formato efectivo de salida.
  TAiAudioFormatEvent = procedure(Sender: TObject; aNativeSampleRate, aNativeChannels, aOutSampleRate, aOutChannels: Integer) of object;

  TAiAudioCapture = class(TComponent)
  private
    FActive: Boolean;
    FInDestroy: Boolean;
    FSource: TAiAudioSource;
    FDeviceId: string;
    FOutputSampleRate: Integer;
    FOutputChannels: Integer;
    FChunkDurationMs: Integer;
    FInjectSilence: Boolean;
    FMuted: Boolean;
    FSoundLevel: Int64;
    FNativeSampleRate: Integer;
    FNativeChannels: Integer;
    FEffectiveSampleRate: Integer;
    FEffectiveChannels: Integer;
    FRealtimeSTT: TAiRealtimeBase;
    FCaptureThread: TThread;
    FOnData: TAiAudioDataEvent;
    FOnUpdate: TAiAudioLevelEvent;
    FOnError: TAiAudioErrorEvent;
    FOnFormat: TAiAudioFormatEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetSource(const Value: TAiAudioSource);
    procedure SetDeviceId(const Value: string);
    procedure SetOutputSampleRate(const Value: Integer);
    procedure SetOutputChannels(const Value: Integer);
    procedure SetChunkDurationMs(const Value: Integer);
    procedure SetRealtimeSTT(const Value: TAiRealtimeBase);
    procedure CheckNotActive(const aPropName: string);
    procedure StartCapture;
    procedure StopCapture;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoError(const aMessage: string);
    // Llamados desde el hilo de captura:
    procedure HandleFormatReady(aNativeRate, aNativeChannels, aOutRate, aOutChannels: Integer);
    procedure HandleChunk(const aChunk: TBytes; aSampleRate, aChannels: Integer);
    procedure HandleThreadError(const aMessage: string);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Lista los dispositivos disponibles para la fuente indicada.
    // Para asLoopback se listan los dispositivos de REPRODUCCION (render),
    // ya que el loopback captura lo que sale por ellos.
    class function GetAudioDevices(aSource: TAiAudioSource): TArray<TAiAudioDeviceInfo>;

    property SoundLevel: Int64 read FSoundLevel;
    // Formato nativo del dispositivo (disponible tras activar la captura)
    property NativeSampleRate: Integer read FNativeSampleRate;
    property NativeChannels: Integer read FNativeChannels;
    // Formato real de los chunks entregados en OnData
    property EffectiveSampleRate: Integer read FEffectiveSampleRate;
    property EffectiveChannels: Integer read FEffectiveChannels;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Source: TAiAudioSource read FSource write SetSource default asLoopback;
    // ID WASAPI del endpoint (ver GetAudioDevices). '' = predeterminado.
    property DeviceId: string read FDeviceId write SetDeviceId;
    // Sample rate de salida. 0 = nativo del dispositivo (tipicamente 48000).
    property OutputSampleRate: Integer read FOutputSampleRate write SetOutputSampleRate default DEFAULT_AC_OUTPUT_SAMPLE_RATE;
    // Canales de salida: 0 = nativo (max 2), 1 = mono, 2 = estereo.
    property OutputChannels: Integer read FOutputChannels write SetOutputChannels default DEFAULT_AC_OUTPUT_CHANNELS;
    // Duracion de cada chunk entregado en OnData.
    property ChunkDurationMs: Integer read FChunkDurationMs write SetChunkDurationMs default DEFAULT_AC_CHUNK_DURATION_MS;
    // En loopback WASAPI no entrega datos si no suena nada. Con True se
    // inyectan chunks de silencio para mantener el flujo continuo.
    property InjectSilence: Boolean read FInjectSilence write FInjectSilence default True;
    // Con True los chunks se sustituyen por silencio SIN cortar el flujo.
    // Util para que el STT no escuche el propio TTS del traductor mientras
    // se reproduce (se puede cambiar con la captura activa).
    property Muted: Boolean read FMuted write FMuted default False;
    // Cuando esta asignado, cada chunk capturado se reenvia (en mono) al
    // componente Realtime STT, que gestiona su propio VAD y transcripcion.
    property RealtimeSTT: TAiRealtimeBase read FRealtimeSTT write SetRealtimeSTT;
    property OnData: TAiAudioDataEvent read FOnData write FOnData;
    property OnUpdate: TAiAudioLevelEvent read FOnUpdate write FOnUpdate;
    property OnError: TAiAudioErrorEvent read FOnError write FOnError;
    property OnFormat: TAiAudioFormatEvent read FOnFormat write FOnFormat;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiAudioCapture]);
end;

// Mezcla PCM16 entrelazado de N canales a mono (promedio).
function DownmixToMono16(const aBuffer: TBytes; aChannels: Integer): TBytes;
var
  Frames, I, C: Integer;
  Acc: Integer;
begin
  if aChannels <= 1 then
    Exit(aBuffer);
  Frames := Length(aBuffer) div (aChannels * 2);
  SetLength(Result, Frames * 2);
  for I := 0 to Frames - 1 do
  begin
    Acc := 0;
    for C := 0 to aChannels - 1 do
      Acc := Acc + PSmallInt(@aBuffer[(I * aChannels + C) * 2])^;
    PSmallInt(@Result[I * 2])^ := SmallInt(Acc div aChannels);
  end;
end;

{$IFDEF MSWINDOWS}

// ---------------------------------------------------------------------------
// Declaraciones WASAPI (Core Audio API) - no incluidas en la RTL de Delphi.
// Se declaran con prefijo IAi* para no colisionar con MfPack u otras librerias.
// ---------------------------------------------------------------------------

const
  CLSID_MMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  IID_IAudioClient: TGUID = '{1CB9AD4C-DBFA-4C32-B178-C2F568A703B2}';
  IID_IAudioCaptureClient: TGUID = '{C8ADBD64-E71E-48A0-A4DE-185C395CD317}';
  KSDATAFORMAT_SUBTYPE_IEEE_FLOAT: TGUID = '{00000003-0000-0010-8000-00AA00389B71}';
  KSDATAFORMAT_SUBTYPE_PCM: TGUID = '{00000001-0000-0010-8000-00AA00389B71}';

  AUDCLNT_SHAREMODE_SHARED = 0;
  AUDCLNT_STREAMFLAGS_LOOPBACK = $00020000;
  AUDCLNT_BUFFERFLAGS_SILENT = $00000002;
  DEVICE_STATE_ACTIVE = $00000001;
  eRender = 0;
  eCapture = 1;
  eConsole = 0;
  WF_EXTENSIBLE = $FFFE; // WAVE_FORMAT_EXTENSIBLE
  WF_IEEE_FLOAT = 3; // WAVE_FORMAT_IEEE_FLOAT
  REFTIMES_PER_MS = 10000; // unidades de 100 ns por milisegundo

type
  TAiPropertyKey = packed record
    fmtid: TGUID;
    pid: DWORD;
  end;

  IAiPropertyStore = interface(IUnknown)
    ['{886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99}']
    function GetCount(out cProps: DWORD): HResult; stdcall;
    function GetAt(iProp: DWORD; out pkey: TAiPropertyKey): HResult; stdcall;
    function GetValue(const key: TAiPropertyKey; out pv: TPropVariant): HResult; stdcall;
    function SetValue(const key: TAiPropertyKey; const propvar: TPropVariant): HResult; stdcall;
    function Commit: HResult; stdcall;
  end;

  IAiMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: DWORD; pActivationParams: Pointer; out ppInterface): HResult; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD; out ppProperties: IAiPropertyStore): HResult; stdcall;
    function GetId(out ppstrId: PWideChar): HResult; stdcall;
    function GetState(out pdwState: DWORD): HResult; stdcall;
  end;

  IAiMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(out pcDevices: UINT): HResult; stdcall;
    function Item(nDevice: UINT; out ppDevice: IAiMMDevice): HResult; stdcall;
  end;

  IAiMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: Integer; dwStateMask: DWORD; out ppDevices: IAiMMDeviceCollection): HResult; stdcall;
    function GetDefaultAudioEndpoint(dataFlow: Integer; role: Integer; out ppEndpoint: IAiMMDevice): HResult; stdcall;
    function GetDevice(pwstrId: PWideChar; out ppDevice: IAiMMDevice): HResult; stdcall;
    function RegisterEndpointNotificationCallback(pClient: IUnknown): HResult; stdcall;
    function UnregisterEndpointNotificationCallback(pClient: IUnknown): HResult; stdcall;
  end;

  IAiAudioClient = interface(IUnknown)
    ['{1CB9AD4C-DBFA-4C32-B178-C2F568A703B2}']
    function Initialize(ShareMode: Integer; StreamFlags: DWORD; hnsBufferDuration: Int64; hnsPeriodicity: Int64;
      pFormat: PWaveFormatEx; AudioSessionGuid: PGUID): HResult; stdcall;
    function GetBufferSize(out pNumBufferFrames: UINT): HResult; stdcall;
    function GetStreamLatency(out phnsLatency: Int64): HResult; stdcall;
    function GetCurrentPadding(out pNumPaddingFrames: UINT): HResult; stdcall;
    function IsFormatSupported(ShareMode: Integer; pFormat: PWaveFormatEx; out ppClosestMatch: PWaveFormatEx): HResult; stdcall;
    function GetMixFormat(out ppDeviceFormat: PWaveFormatEx): HResult; stdcall;
    function GetDevicePeriod(out phnsDefaultDevicePeriod: Int64; out phnsMinimumDevicePeriod: Int64): HResult; stdcall;
    function Start: HResult; stdcall;
    function Stop: HResult; stdcall;
    function Reset: HResult; stdcall;
    function SetEventHandle(eventHandle: THandle): HResult; stdcall;
    function GetService(const riid: TGUID; out ppv): HResult; stdcall;
  end;

  IAiAudioCaptureClient = interface(IUnknown)
    ['{C8ADBD64-E71E-48A0-A4DE-185C395CD317}']
    function GetBuffer(out ppData: PByte; out pNumFramesToRead: UINT; out pdwFlags: DWORD;
      pu64DevicePosition: PUInt64; pu64QPCPosition: PUInt64): HResult; stdcall;
    function ReleaseBuffer(NumFramesRead: UINT): HResult; stdcall;
    function GetNextPacketSize(out pNumFramesInNextPacket: UINT): HResult; stdcall;
  end;

  TWaveFormatExtensible = packed record
    Format: TWaveFormatEx;
    wValidBitsPerSample: Word;
    dwChannelMask: DWORD;
    SubFormat: TGUID;
  end;

  PWaveFormatExtensible = ^TWaveFormatExtensible;

const
  PKEY_Device_FriendlyName: TAiPropertyKey = (fmtid: (D1: $A45C254E; D2: $DF1C; D3: $4EFD;
    D4: ($80, $20, $67, $D1, $46, $A8, $50, $E0)); pid: 14);

type
  // ---------------------------------------------------------------------------
  // Hilo de captura WASAPI (polling cada ~ChunkMs/4)
  // ---------------------------------------------------------------------------
  TAiWasapiCaptureThread = class(TThread)
  private
    FOwner: TAiAudioCapture;
    FSource: TAiAudioSource;
    FDeviceId: string;
    FChunkMs: Integer;
    FOutRateCfg: Integer; // 0 = nativo
    FOutChCfg: Integer; // 0 = nativo
    FInjectSilence: Boolean;
    // Formato nativo (mix format) detectado
    FMixRate: Integer;
    FMixCh: Integer;
    FMixBits: Integer;
    FMixFloat: Boolean;
    // Formato efectivo de salida
    FOutRate: Integer;
    FOutCh: Integer;
    FChunkBytes: Integer;
    // Estado del resampler lineal (continuidad entre paquetes)
    FResamplePos: Double;
    FLastSamples: array [0 .. 1] of Single;
    FHasLast: Boolean;
    // Acumulador de PCM16 ya en formato de salida
    FChunkData: TMemoryStream;
    FEmitWatch: TStopwatch;
    function ReadNativeSample(pData: PByte; aIndex: Integer): Single;
    procedure ProcessPacket(pData: PByte; aFrames: Integer; aSilent: Boolean);
    procedure EmitChunk(aPad: Boolean);
    procedure RaiseIfFailed(hr: HResult; const aContext: string);
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TAiAudioCapture);
    destructor Destroy; override;
  end;

constructor TAiWasapiCaptureThread.Create(aOwner: TAiAudioCapture);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FOwner := aOwner;
  FSource := aOwner.FSource;
  FDeviceId := aOwner.FDeviceId;
  FChunkMs := aOwner.FChunkDurationMs;
  FOutRateCfg := aOwner.FOutputSampleRate;
  FOutChCfg := aOwner.FOutputChannels;
  FInjectSilence := aOwner.FInjectSilence;
  FChunkData := TMemoryStream.Create;
end;

destructor TAiWasapiCaptureThread.Destroy;
begin
  FChunkData.Free;
  inherited;
end;

procedure TAiWasapiCaptureThread.RaiseIfFailed(hr: HResult; const aContext: string);
begin
  if Failed(hr) then
    raise Exception.CreateFmt('%s fallo (HRESULT=0x%.8x)', [aContext, Cardinal(hr)]);
end;

function TAiWasapiCaptureThread.ReadNativeSample(pData: PByte; aIndex: Integer): Single;
begin
  if FMixFloat then
    Result := PSingle(pData + aIndex * 4)^
  else if FMixBits = 16 then
    Result := PSmallInt(pData + aIndex * 2)^ / 32768
  else // PCM entero de 32 bits
    Result := PInteger(pData + aIndex * 4)^ / 2147483648.0;
end;

procedure TAiWasapiCaptureThread.ProcessPacket(pData: PByte; aFrames: Integer; aSilent: Boolean);
var
  I, C, K, Total, OutIdx, MaxOut: Integer;
  Sum: Single;
  InBuf: TArray<Single>; // entrelazado, FOutCh canales, rate nativo
  OutBuf: TArray<Single>; // entrelazado, FOutCh canales, rate de salida
  Ratio: Double;
  Idx: Integer;
  Frac, S0, S1: Single;
  OutPCM: TBytes;
  V: Integer;

  // Acceso al "stream extendido": la ultima muestra del paquete anterior
  // (FLastSamples) seguida de las muestras del paquete actual.
  function ExtFrame(aFrame, aChan: Integer): Single;
  begin
    if FHasLast then
    begin
      if aFrame = 0 then
        Exit(FLastSamples[aChan]);
      Dec(aFrame);
    end;
    Result := InBuf[aFrame * FOutCh + aChan];
  end;

begin
  if aFrames <= 0 then
    Exit;

  // 1) Convertir a float y mezclar al numero de canales de salida
  SetLength(InBuf, aFrames * FOutCh);
  if aSilent or (pData = nil) then
    FillChar(InBuf[0], Length(InBuf) * SizeOf(Single), 0)
  else
    for I := 0 to aFrames - 1 do
      for C := 0 to FOutCh - 1 do
      begin
        if FOutCh = 1 then
        begin
          // Promedio de todos los canales nativos
          Sum := 0;
          for K := 0 to FMixCh - 1 do
            Sum := Sum + ReadNativeSample(pData, I * FMixCh + K);
          InBuf[I] := Sum / FMixCh;
        end
        else
        begin
          // Tomar el canal correspondiente (o duplicar si nativo es mono)
          K := Min(C, FMixCh - 1);
          InBuf[I * FOutCh + C] := ReadNativeSample(pData, I * FMixCh + K);
        end;
      end;

  // 2) Remuestreo lineal con continuidad entre paquetes
  if FOutRate = FMixRate then
    OutBuf := InBuf
  else
  begin
    if FHasLast then
      Total := aFrames + 1 // el frame 0 es el remanente del paquete anterior
    else
    begin
      Total := aFrames;
      FResamplePos := 0;
    end;

    Ratio := FMixRate / FOutRate;
    MaxOut := Trunc((Total - 1 - FResamplePos) / Ratio) + 2;
    if MaxOut < 0 then
      MaxOut := 0;
    SetLength(OutBuf, MaxOut * FOutCh);
    OutIdx := 0;

    while FResamplePos < Total - 1 do
    begin
      Idx := Trunc(FResamplePos);
      Frac := FResamplePos - Idx;
      for C := 0 to FOutCh - 1 do
      begin
        S0 := ExtFrame(Idx, C);
        S1 := ExtFrame(Idx + 1, C);
        OutBuf[OutIdx * FOutCh + C] := S0 + (S1 - S0) * Frac;
      end;
      Inc(OutIdx);
      FResamplePos := FResamplePos + Ratio;
    end;

    // Guardar la ultima muestra para interpolar con el proximo paquete
    for C := 0 to FOutCh - 1 do
      FLastSamples[C] := ExtFrame(Total - 1, C);
    FResamplePos := FResamplePos - (Total - 1);
    FHasLast := True;
    SetLength(OutBuf, OutIdx * FOutCh);
  end;

  if Length(OutBuf) = 0 then
    Exit;

  // 3) Convertir a PCM16 y acumular
  SetLength(OutPCM, Length(OutBuf) * 2);
  for I := 0 to High(OutBuf) do
  begin
    V := Round(OutBuf[I] * 32767);
    if V > 32767 then
      V := 32767
    else if V < -32768 then
      V := -32768;
    PSmallInt(@OutPCM[I * 2])^ := SmallInt(V);
  end;

  FChunkData.Position := FChunkData.Size;
  FChunkData.WriteBuffer(OutPCM[0], Length(OutPCM));

  while FChunkData.Size >= FChunkBytes do
    EmitChunk(False);
end;

procedure TAiWasapiCaptureThread.EmitChunk(aPad: Boolean);
var
  Chunk, Remaining: TBytes;
  RemSize: Integer;
begin
  if FChunkData.Size < FChunkBytes then
  begin
    if not aPad then
      Exit;
    // Completar con silencio hasta el tamano del chunk
    SetLength(Chunk, FChunkBytes);
    FillChar(Chunk[0], FChunkBytes, 0);
    if FChunkData.Size > 0 then
    begin
      FChunkData.Position := 0;
      FChunkData.ReadBuffer(Chunk[0], FChunkData.Size);
    end;
    FChunkData.Clear;
  end
  else
  begin
    SetLength(Chunk, FChunkBytes);
    FChunkData.Position := 0;
    FChunkData.ReadBuffer(Chunk[0], FChunkBytes);
    // Conservar el sobrante para el proximo chunk
    RemSize := FChunkData.Size - FChunkBytes;
    if RemSize > 0 then
    begin
      SetLength(Remaining, RemSize);
      FChunkData.ReadBuffer(Remaining[0], RemSize);
    end;
    FChunkData.Clear;
    if RemSize > 0 then
      FChunkData.WriteBuffer(Remaining[0], RemSize);
  end;

  FEmitWatch.Reset;
  FEmitWatch.Start;

  if not Terminated then
    FOwner.HandleChunk(Chunk, FOutRate, FOutCh);
end;

procedure TAiWasapiCaptureThread.Execute;
var
  Enumerator: IAiMMDeviceEnumerator;
  Device: IAiMMDevice;
  AudioClient: IAiAudioClient;
  CaptureClient: IAiAudioCaptureClient;
  pwfx: PWaveFormatEx;
  pExt: PWaveFormatExtensible;
  StreamFlags: DWORD;
  DataFlow: Integer;
  PacketFrames, FramesRead: UINT;
  pData: PByte;
  dwFlags: DWORD;
  CoInit: Boolean;
begin
  CoInit := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
  try
    try
      RaiseIfFailed(CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IAiMMDeviceEnumerator, Enumerator),
        'CoCreateInstance(MMDeviceEnumerator)');

      if FSource = asLoopback then
        DataFlow := eRender
      else
        DataFlow := eCapture;

      if FDeviceId = '' then
        RaiseIfFailed(Enumerator.GetDefaultAudioEndpoint(DataFlow, eConsole, Device), 'GetDefaultAudioEndpoint')
      else
        RaiseIfFailed(Enumerator.GetDevice(PWideChar(FDeviceId), Device), 'GetDevice');

      RaiseIfFailed(Device.Activate(IID_IAudioClient, CLSCTX_ALL, nil, AudioClient), 'IMMDevice.Activate');

      RaiseIfFailed(AudioClient.GetMixFormat(pwfx), 'GetMixFormat');
      try
        FMixRate := pwfx.nSamplesPerSec;
        FMixCh := pwfx.nChannels;
        FMixBits := pwfx.wBitsPerSample;

        if pwfx.wFormatTag = WF_IEEE_FLOAT then
          FMixFloat := True
        else if pwfx.wFormatTag = WAVE_FORMAT_PCM then
          FMixFloat := False
        else if pwfx.wFormatTag = WF_EXTENSIBLE then
        begin
          pExt := PWaveFormatExtensible(pwfx);
          if IsEqualGUID(pExt.SubFormat, KSDATAFORMAT_SUBTYPE_IEEE_FLOAT) then
            FMixFloat := True
          else if IsEqualGUID(pExt.SubFormat, KSDATAFORMAT_SUBTYPE_PCM) then
            FMixFloat := False
          else
            raise Exception.Create('Formato de mezcla no soportado (SubFormat desconocido)');
        end
        else
          raise Exception.CreateFmt('Formato de mezcla no soportado (tag=%d)', [pwfx.wFormatTag]);

        if FMixFloat and (FMixBits <> 32) then
          raise Exception.CreateFmt('Formato float de %d bits no soportado', [FMixBits]);
        if (not FMixFloat) and (FMixBits <> 16) and (FMixBits <> 32) then
          raise Exception.CreateFmt('Formato PCM entero de %d bits no soportado', [FMixBits]);

        if FSource = asLoopback then
          StreamFlags := AUDCLNT_STREAMFLAGS_LOOPBACK
        else
          StreamFlags := 0;

        // Buffer interno de 500 ms: margen de sobra para el polling
        RaiseIfFailed(AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, StreamFlags, 500 * REFTIMES_PER_MS, 0, pwfx, nil),
          'IAudioClient.Initialize');
      finally
        CoTaskMemFree(pwfx);
      end;

      // Resolver formato efectivo de salida
      if FOutRateCfg > 0 then
        FOutRate := FOutRateCfg
      else
        FOutRate := FMixRate;
      if FOutChCfg > 0 then
        FOutCh := Min(FOutChCfg, 2)
      else
        FOutCh := Min(FMixCh, 2);

      FChunkBytes := (FOutRate * FOutCh * 2 * FChunkMs) div 1000;
      FChunkBytes := FChunkBytes - (FChunkBytes mod (FOutCh * 2)); // alinear a frame
      if FChunkBytes <= 0 then
        raise Exception.Create('Configuracion de chunk invalida');

      RaiseIfFailed(AudioClient.GetService(IID_IAudioCaptureClient, CaptureClient), 'GetService(IAudioCaptureClient)');
      RaiseIfFailed(AudioClient.Start, 'IAudioClient.Start');
      try
        FOwner.HandleFormatReady(FMixRate, FMixCh, FOutRate, FOutCh);
        FEmitWatch := TStopwatch.StartNew;

        while not Terminated do
        begin
          Sleep(Max(5, FChunkMs div 4));

          RaiseIfFailed(CaptureClient.GetNextPacketSize(PacketFrames), 'GetNextPacketSize');
          while (PacketFrames > 0) and not Terminated do
          begin
            RaiseIfFailed(CaptureClient.GetBuffer(pData, FramesRead, dwFlags, nil, nil), 'GetBuffer');
            try
              ProcessPacket(pData, FramesRead, (dwFlags and AUDCLNT_BUFFERFLAGS_SILENT) <> 0);
            finally
              CaptureClient.ReleaseBuffer(FramesRead);
            end;
            RaiseIfFailed(CaptureClient.GetNextPacketSize(PacketFrames), 'GetNextPacketSize');
          end;

          // En loopback no llegan paquetes si no suena nada: inyectar silencio
          // para mantener el flujo continuo hacia VAD/STT.
          if FInjectSilence and (FEmitWatch.ElapsedMilliseconds >= 2 * FChunkMs) then
            EmitChunk(True);
        end;
      finally
        AudioClient.Stop;
      end;
    except
      on E: Exception do
        if not Terminated then
          FOwner.HandleThreadError(E.Message);
    end;
  finally
    // Liberar las interfaces COM ANTES de CoUninitialize
    CaptureClient := nil;
    AudioClient := nil;
    Device := nil;
    Enumerator := nil;
    if CoInit then
      CoUninitialize;
  end;
end;

{$ENDIF}

{ TAiAudioCapture }

constructor TAiAudioCapture.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FInDestroy := False;
  FActive := False;
  FSource := asLoopback;
  FDeviceId := '';
  FOutputSampleRate := DEFAULT_AC_OUTPUT_SAMPLE_RATE;
  FOutputChannels := DEFAULT_AC_OUTPUT_CHANNELS;
  FChunkDurationMs := DEFAULT_AC_CHUNK_DURATION_MS;
  FInjectSilence := True;
end;

destructor TAiAudioCapture.Destroy;
begin
  FInDestroy := True;
  StopCapture;
  inherited;
end;

procedure TAiAudioCapture.Loaded;
begin
  inherited;
  if FActive and not(csDesigning in ComponentState) then
    StartCapture;
end;

procedure TAiAudioCapture.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRealtimeSTT) then
    FRealtimeSTT := nil;
end;

procedure TAiAudioCapture.CheckNotActive(const aPropName: string);
begin
  if FActive then
    raise EInvalidOperation.CreateFmt('Cannot change %s while capture is active. Set Active to False first.', [aPropName]);
end;

procedure TAiAudioCapture.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    Exit;
  FActive := Value;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) or FInDestroy then
    Exit;
  if FActive then
    StartCapture
  else
    StopCapture;
end;

procedure TAiAudioCapture.SetSource(const Value: TAiAudioSource);
begin
  if FSource = Value then
    Exit;
  CheckNotActive('Source');
  FSource := Value;
end;

procedure TAiAudioCapture.SetDeviceId(const Value: string);
begin
  if FDeviceId = Value then
    Exit;
  CheckNotActive('DeviceId');
  FDeviceId := Value;
end;

procedure TAiAudioCapture.SetOutputSampleRate(const Value: Integer);
const
  VALID_RATES: array [0 .. 7] of Integer = (8000, 11025, 16000, 22050, 24000, 32000, 44100, 48000);
var
  I: Integer;
  IsValid: Boolean;
begin
  if FOutputSampleRate = Value then
    Exit;
  CheckNotActive('OutputSampleRate');

  IsValid := (Value = 0); // 0 = usar el rate nativo del dispositivo
  for I := Low(VALID_RATES) to High(VALID_RATES) do
    if Value = VALID_RATES[I] then
    begin
      IsValid := True;
      Break;
    end;
  if not IsValid then
    raise EArgumentException.CreateFmt('Invalid output sample rate: %d Hz. Use 0 (native) or a standard rate.', [Value]);

  FOutputSampleRate := Value;
end;

procedure TAiAudioCapture.SetOutputChannels(const Value: Integer);
begin
  if FOutputChannels = Value then
    Exit;
  CheckNotActive('OutputChannels');
  if (Value < 0) or (Value > 2) then
    raise EArgumentException.Create('OutputChannels must be 0 (native), 1 (mono) or 2 (stereo).');
  FOutputChannels := Value;
end;

procedure TAiAudioCapture.SetChunkDurationMs(const Value: Integer);
begin
  if FChunkDurationMs = Value then
    Exit;
  CheckNotActive('ChunkDurationMs');
  if (Value < 20) or (Value > 1000) then
    raise EArgumentOutOfRangeException.CreateFmt('ChunkDurationMs must be between 20 and 1000 ms. Got: %d', [Value]);
  FChunkDurationMs := Value;
end;

procedure TAiAudioCapture.SetRealtimeSTT(const Value: TAiRealtimeBase);
begin
  if FRealtimeSTT = Value then
    Exit;
  FRealtimeSTT := Value;
  if Assigned(FRealtimeSTT) then
  begin
    FRealtimeSTT.FreeNotification(Self);
    // Sincronizar el sample rate de entrada del resampler del Realtime
    if FEffectiveSampleRate > 0 then
      FRealtimeSTT.InputSampleRate := FEffectiveSampleRate
    else if FOutputSampleRate > 0 then
      FRealtimeSTT.InputSampleRate := FOutputSampleRate;
  end;
end;

procedure TAiAudioCapture.StartCapture;
begin
{$IFDEF MSWINDOWS}
  if Assigned(FCaptureThread) then
    Exit;
  FSoundLevel := 0;
  FNativeSampleRate := 0;
  FNativeChannels := 0;
  FEffectiveSampleRate := 0;
  FEffectiveChannels := 0;
  FCaptureThread := TAiWasapiCaptureThread.Create(Self);
{$ELSE}
  FActive := False;
  DoError('TAiAudioCapture solo esta disponible en Windows (WASAPI).');
{$ENDIF}
end;

procedure TAiAudioCapture.StopCapture;
begin
  if Assigned(FCaptureThread) then
  begin
    FCaptureThread.Terminate;
    FCaptureThread.WaitFor;
    FreeAndNil(FCaptureThread);
  end;
end;

procedure TAiAudioCapture.DoError(const aMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if not(csDestroying in ComponentState) and Assigned(FOnError) then
        FOnError(Self, aMessage);
    end);
end;

procedure TAiAudioCapture.HandleThreadError(const aMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if csDestroying in ComponentState then
        Exit;
      FActive := False;
      StopCapture; // el hilo ya termino: WaitFor retorna de inmediato
      if Assigned(FOnError) then
        FOnError(Self, aMessage);
    end);
end;

procedure TAiAudioCapture.HandleFormatReady(aNativeRate, aNativeChannels, aOutRate, aOutChannels: Integer);
begin
  FNativeSampleRate := aNativeRate;
  FNativeChannels := aNativeChannels;
  FEffectiveSampleRate := aOutRate;
  FEffectiveChannels := aOutChannels;

  // El resampler interno del Realtime STT necesita conocer el rate de entrada
  if Assigned(FRealtimeSTT) then
    FRealtimeSTT.InputSampleRate := aOutRate;

  TThread.Queue(nil,
    procedure
    begin
      if not(csDestroying in ComponentState) and Assigned(FOnFormat) then
        FOnFormat(Self, aNativeRate, aNativeChannels, aOutRate, aOutChannels);
    end);
end;

procedure TAiAudioCapture.HandleChunk(const aChunk: TBytes; aSampleRate, aChannels: Integer);
var
  Level: Int64;
  I, NumSamples: Integer;
  Chunk, MonoChunk: TBytes;
begin
  // Si esta silenciado, sustituir por ceros manteniendo el flujo continuo
  if FMuted then
  begin
    SetLength(Chunk, Length(aChunk));
    if Length(Chunk) > 0 then
      FillChar(Chunk[0], Length(Chunk), 0);
  end
  else
    Chunk := aChunk;

  // Nivel promedio del chunk (PCM16)
  NumSamples := Length(Chunk) div 2;
  Level := 0;
  if NumSamples > 0 then
  begin
    for I := 0 to NumSamples - 1 do
      Level := Level + Abs(PSmallInt(@Chunk[I * 2])^);
    Level := Level div NumSamples;
  end;
  FSoundLevel := Level;

  // Reenviar al componente Realtime STT (su resampler espera mono)
  if Assigned(FRealtimeSTT) and FRealtimeSTT.IsConnected then
  begin
    if aChannels > 1 then
      MonoChunk := DownmixToMono16(Chunk, aChannels)
    else
      MonoChunk := Chunk;
    FRealtimeSTT.SendAudioChunk(MonoChunk);
  end;

  TThread.Queue(nil,
    procedure
    begin
      if csDestroying in ComponentState then
        Exit;
      if Assigned(FOnUpdate) then
        FOnUpdate(Self, Level);
      if Assigned(FOnData) then
        FOnData(Self, Chunk, aSampleRate, aChannels);
    end);
end;

class function TAiAudioCapture.GetAudioDevices(aSource: TAiAudioSource): TArray<TAiAudioDeviceInfo>;
{$IFDEF MSWINDOWS}
var
  Enumerator: IAiMMDeviceEnumerator;
  Collection: IAiMMDeviceCollection;
  Device, DefaultDevice: IAiMMDevice;
  Props: IAiPropertyStore;
  pv: TPropVariant;
  Count: UINT;
  I: Integer;
  pId: PWideChar;
  DefaultId: string;
  DataFlow: Integer;
  CoInit: Boolean;
begin
  SetLength(Result, 0);
  CoInit := Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED));
  try
    if Failed(CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IAiMMDeviceEnumerator, Enumerator)) then
      Exit;

    if aSource = asLoopback then
      DataFlow := eRender
    else
      DataFlow := eCapture;

    DefaultId := '';
    if Succeeded(Enumerator.GetDefaultAudioEndpoint(DataFlow, eConsole, DefaultDevice)) then
      if Succeeded(DefaultDevice.GetId(pId)) then
      begin
        DefaultId := pId;
        CoTaskMemFree(pId);
      end;

    if Failed(Enumerator.EnumAudioEndpoints(DataFlow, DEVICE_STATE_ACTIVE, Collection)) then
      Exit;

    if Failed(Collection.GetCount(Count)) then
      Exit;

    SetLength(Result, Count);
    for I := 0 to Integer(Count) - 1 do
    begin
      Result[I].EndpointId := '';
      Result[I].DeviceName := '(desconocido)';
      Result[I].IsDefault := False;

      if Failed(Collection.Item(I, Device)) then
        Continue;

      if Succeeded(Device.GetId(pId)) then
      begin
        Result[I].EndpointId := pId;
        CoTaskMemFree(pId);
      end;

      if Succeeded(Device.OpenPropertyStore(STGM_READ, Props)) then
      begin
        FillChar(pv, SizeOf(pv), 0);
        if Succeeded(Props.GetValue(PKEY_Device_FriendlyName, pv)) then
        begin
          if (pv.vt = VT_LPWSTR) and (pv.pwszVal <> nil) then
            Result[I].DeviceName := pv.pwszVal;
          PropVariantClear(pv);
        end;
        Props := nil;
      end;

      Result[I].IsDefault := (DefaultId <> '') and SameText(Result[I].EndpointId, DefaultId);
      Device := nil;
    end;
  finally
    // Liberar las interfaces COM ANTES de CoUninitialize
    Props := nil;
    Device := nil;
    DefaultDevice := nil;
    Collection := nil;
    Enumerator := nil;
    if CoInit then
      CoUninitialize;
  end;
end;
{$ELSE}

begin
  SetLength(Result, 0);
end;
{$ENDIF}

end.
