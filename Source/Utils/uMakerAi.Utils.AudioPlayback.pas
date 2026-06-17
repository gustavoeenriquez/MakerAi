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
  uMakerAi.Utils.AudioPlayback
  ============================

  TAiAudioPlayer: componente de reproduccion de audio PCM16 sobre un
  dispositivo de salida WASAPI SELECCIONABLE. Es la pieza simetrica de
  TAiAudioCapture (uMakerAi.Utils.AudioCapture).

  Caso de uso tipico: reproducir el TTS de un traductor en tiempo real:
  - La traduccion para MI se reproduce en mis auriculares (predeterminado).
  - La traduccion para la REUNION se reproduce en un cable virtual
    (p.ej. VB-CABLE "CABLE Input"); en Zoom/Meet se selecciona el extremo
    "CABLE Output" como microfono, y el otro lado escucha el TTS.

  Caracteristicas:
  - Cola de reproduccion: cada PlayPCM16() encola una frase; se reproducen
    en orden, sin solaparse, en un hilo dedicado.
  - Conversion automatica al formato del dispositivo (remuestreo lineal y
    mapeo de canales), igual que hace el capturador.
  - OnStateChange(IsPlaying): util para silenciar la captura loopback
    mientras suena el propio TTS (propiedad Muted de TAiAudioCapture).

  Uso minimo:

    Player := TAiAudioPlayer.Create(Self);
    Player.DeviceId := '';      // '' = dispositivo de salida predeterminado
    Player.Active := True;
    Player.PlayPCM16(PcmBytes, 24000, 1);  // p.ej. TTS de OpenAI (trfPcm)

  Plataformas: solo Windows. En otras plataformas el componente compila pero
  Active := True dispara OnError.
}

unit uMakerAi.Utils.AudioPlayback;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.SyncObjs, System.Math,
  System.Diagnostics, System.Generics.Collections,
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ActiveX, Winapi.MMSystem,
{$ENDIF}
  uMakerAi.Utils.AudioCapture; // TAiAudioDeviceInfo + enumeracion de endpoints

type
  TAiAudioPlayItem = record
    Data: TBytes; // PCM16 entrelazado
    SampleRate: Integer;
    Channels: Integer;
  end;

  TAiPlayerStateEvent = procedure(Sender: TObject; aIsPlaying: Boolean) of object;
  TAiPlayerErrorEvent = procedure(Sender: TObject; const ErrorMessage: string) of object;

  TAiAudioPlayer = class(TComponent)
  private
    FActive: Boolean;
    FInDestroy: Boolean;
    FDeviceId: string;
    FIsPlaying: Boolean;
    FQueue: TThreadedQueue<TAiAudioPlayItem>;
    FPlayThread: TThread;
    FOnStateChange: TAiPlayerStateEvent;
    FOnError: TAiPlayerErrorEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetDeviceId(const Value: string);
    procedure StartPlayback;
    procedure StopPlayback;
  protected
    procedure Loaded; override;
    procedure DoError(const aMessage: string);
    // Llamados desde el hilo de reproduccion:
    procedure HandleStateChange(aIsPlaying: Boolean);
    procedure HandleThreadError(const aMessage: string);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Encola PCM16 para reproducir (no bloquea; las frases suenan en orden).
    procedure PlayPCM16(const aData: TBytes; aSampleRate, aChannels: Integer);
    // Vacia la cola (no corta la frase que ya esta sonando).
    procedure ClearQueue;

    // Dispositivos de SALIDA disponibles (delega en TAiAudioCapture).
    class function GetPlaybackDevices: TArray<TAiAudioDeviceInfo>;

    property IsPlaying: Boolean read FIsPlaying;
  published
    property Active: Boolean read FActive write SetActive default False;
    // ID WASAPI del endpoint de salida (ver GetPlaybackDevices). '' = predeterminado.
    property DeviceId: string read FDeviceId write SetDeviceId;
    property OnStateChange: TAiPlayerStateEvent read FOnStateChange write FOnStateChange;
    property OnError: TAiPlayerErrorEvent read FOnError write FOnError;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiAudioPlayer]);
end;

{$IFDEF MSWINDOWS}

// ---------------------------------------------------------------------------
// Declaraciones WASAPI minimas para render (autocontenidas, prefijo IAiPb*
// para no colisionar con las del capturador ni con MfPack).
// ---------------------------------------------------------------------------

const
  CLSID_MMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  IID_IAudioClient: TGUID = '{1CB9AD4C-DBFA-4C32-B178-C2F568A703B2}';
  IID_IAudioRenderClient: TGUID = '{F294ACFC-3146-4483-A7BF-ADDCA7C260E2}';
  KSDATAFORMAT_SUBTYPE_IEEE_FLOAT: TGUID = '{00000003-0000-0010-8000-00AA00389B71}';
  KSDATAFORMAT_SUBTYPE_PCM: TGUID = '{00000001-0000-0010-8000-00AA00389B71}';

  AUDCLNT_SHAREMODE_SHARED = 0;
  eRender = 0;
  eConsole = 0;
  WF_EXTENSIBLE = $FFFE;
  WF_IEEE_FLOAT = 3;
  REFTIMES_PER_MS = 10000;

type
  IAiPbMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: DWORD; pActivationParams: Pointer; out ppInterface): HResult; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD; out ppProperties: IUnknown): HResult; stdcall;
    function GetId(out ppstrId: PWideChar): HResult; stdcall;
    function GetState(out pdwState: DWORD): HResult; stdcall;
  end;

  IAiPbMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(out pcDevices: UINT): HResult; stdcall;
    function Item(nDevice: UINT; out ppDevice: IAiPbMMDevice): HResult; stdcall;
  end;

  IAiPbMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: Integer; dwStateMask: DWORD; out ppDevices: IAiPbMMDeviceCollection): HResult; stdcall;
    function GetDefaultAudioEndpoint(dataFlow: Integer; role: Integer; out ppEndpoint: IAiPbMMDevice): HResult; stdcall;
    function GetDevice(pwstrId: PWideChar; out ppDevice: IAiPbMMDevice): HResult; stdcall;
    function RegisterEndpointNotificationCallback(pClient: IUnknown): HResult; stdcall;
    function UnregisterEndpointNotificationCallback(pClient: IUnknown): HResult; stdcall;
  end;

  IAiPbAudioClient = interface(IUnknown)
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

  IAiPbAudioRenderClient = interface(IUnknown)
    ['{F294ACFC-3146-4483-A7BF-ADDCA7C260E2}']
    function GetBuffer(NumFramesRequested: UINT; out ppData: PByte): HResult; stdcall;
    function ReleaseBuffer(NumFramesWritten: UINT; dwFlags: DWORD): HResult; stdcall;
  end;

  TPbWaveFormatExtensible = packed record
    Format: TWaveFormatEx;
    wValidBitsPerSample: Word;
    dwChannelMask: DWORD;
    SubFormat: TGUID;
  end;

  PPbWaveFormatExtensible = ^TPbWaveFormatExtensible;

  // ---------------------------------------------------------------------------
  // Hilo de reproduccion: consume la cola y escribe en el buffer WASAPI
  // ---------------------------------------------------------------------------
  TAiWasapiPlayThread = class(TThread)
  private
    FOwner: TAiAudioPlayer;
    FDeviceId: string;
    // Formato de mezcla del dispositivo
    FMixRate: Integer;
    FMixCh: Integer;
    FMixBits: Integer;
    FMixFloat: Boolean;
    FBufFrames: UINT;
    procedure RaiseIfFailed(hr: HResult; const aContext: string);
    // Convierte un item PCM16 al formato float interleaved del dispositivo
    function ConvertItem(const aItem: TAiAudioPlayItem): TArray<Single>;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TAiAudioPlayer);
  end;

constructor TAiWasapiPlayThread.Create(aOwner: TAiAudioPlayer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FOwner := aOwner;
  FDeviceId := aOwner.FDeviceId;
end;

procedure TAiWasapiPlayThread.RaiseIfFailed(hr: HResult; const aContext: string);
begin
  if Failed(hr) then
    raise Exception.CreateFmt('%s fallo (HRESULT=0x%.8x)', [aContext, Cardinal(hr)]);
end;

function TAiWasapiPlayThread.ConvertItem(const aItem: TAiAudioPlayItem): TArray<Single>;
var
  InFrames, OutFrames, I, C, Idx: Integer;
  SrcPos, Ratio: Double;
  Frac, S0, S1, S: Single;

  function InSample(aFrame, aChan: Integer): Single;
  begin
    // mapeo de canales: mono se duplica, sobrantes se repiten ciclicamente
    Result := PSmallInt(@aItem.Data[(aFrame * aItem.Channels + (aChan mod aItem.Channels)) * 2])^ / 32768;
  end;

begin
  Result := nil;
  if (aItem.SampleRate <= 0) or (aItem.Channels <= 0) then
    Exit;
  InFrames := Length(aItem.Data) div (aItem.Channels * 2);
  if InFrames = 0 then
    Exit;

  if aItem.SampleRate = FMixRate then
  begin
    // Solo mapeo de canales
    SetLength(Result, InFrames * FMixCh);
    for I := 0 to InFrames - 1 do
      for C := 0 to FMixCh - 1 do
        Result[I * FMixCh + C] := InSample(I, C);
  end
  else
  begin
    // Remuestreo lineal + mapeo de canales
    Ratio := aItem.SampleRate / FMixRate;
    OutFrames := Trunc((InFrames - 1) / Ratio) + 1;
    SetLength(Result, OutFrames * FMixCh);
    for I := 0 to OutFrames - 1 do
    begin
      SrcPos := I * Ratio;
      Idx := Trunc(SrcPos);
      if Idx >= InFrames - 1 then
      begin
        Idx := InFrames - 1;
        Frac := 0;
      end
      else
        Frac := SrcPos - Idx;
      for C := 0 to FMixCh - 1 do
      begin
        S0 := InSample(Idx, C);
        if Frac > 0 then
        begin
          S1 := InSample(Idx + 1, C);
          S := S0 + (S1 - S0) * Frac;
        end
        else
          S := S0;
        Result[I * FMixCh + C] := S;
      end;
    end;
  end;
end;

procedure TAiWasapiPlayThread.Execute;
var
  Enumerator: IAiPbMMDeviceEnumerator;
  Device: IAiPbMMDevice;
  AudioClient: IAiPbAudioClient;
  RenderClient: IAiPbAudioRenderClient;
  pwfx: PWaveFormatEx;
  pExt: PPbWaveFormatExtensible;
  CoInit: Boolean;
  Item: TAiAudioPlayItem;
  Samples: TArray<Single>;
  TotalFrames, Pos, Todo, I: Integer;
  Padding, Avail: UINT;
  pData: PByte;
  V: Integer;
  S: Single;
  WasPlaying: Boolean;
begin
  CoInit := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
  try
    try
      RaiseIfFailed(CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IAiPbMMDeviceEnumerator, Enumerator),
        'CoCreateInstance(MMDeviceEnumerator)');

      if FDeviceId = '' then
        RaiseIfFailed(Enumerator.GetDefaultAudioEndpoint(eRender, eConsole, Device), 'GetDefaultAudioEndpoint')
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
          pExt := PPbWaveFormatExtensible(pwfx);
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

        // Buffer de 300 ms
        RaiseIfFailed(AudioClient.Initialize(AUDCLNT_SHAREMODE_SHARED, 0, 300 * REFTIMES_PER_MS, 0, pwfx, nil),
          'IAudioClient.Initialize');
      finally
        CoTaskMemFree(pwfx);
      end;

      RaiseIfFailed(AudioClient.GetBufferSize(FBufFrames), 'GetBufferSize');
      RaiseIfFailed(AudioClient.GetService(IID_IAudioRenderClient, RenderClient), 'GetService(IAudioRenderClient)');
      RaiseIfFailed(AudioClient.Start, 'IAudioClient.Start');
      try
        WasPlaying := False;

        while not Terminated do
        begin
          if FOwner.FQueue.PopItem(Item) <> TWaitResult.wrSignaled then
          begin
            // Cola vacia: si estabamos reproduciendo, esperar a que drene
            if WasPlaying then
            begin
              if (AudioClient.GetCurrentPadding(Padding) = S_OK) and (Padding = 0) then
              begin
                WasPlaying := False;
                FOwner.HandleStateChange(False);
              end;
            end;
            Continue;
          end;

          if Terminated then
            Break;

          Samples := ConvertItem(Item);
          if Length(Samples) = 0 then
            Continue;

          if not WasPlaying then
          begin
            WasPlaying := True;
            FOwner.HandleStateChange(True);
          end;

          // Escribir el item completo en el buffer WASAPI por tramos
          TotalFrames := Length(Samples) div FMixCh;
          Pos := 0;
          while (Pos < TotalFrames) and not Terminated do
          begin
            RaiseIfFailed(AudioClient.GetCurrentPadding(Padding), 'GetCurrentPadding');
            Avail := FBufFrames - Padding;
            if Avail = 0 then
            begin
              Sleep(5);
              Continue;
            end;
            Todo := Min(Integer(Avail), TotalFrames - Pos);
            RaiseIfFailed(RenderClient.GetBuffer(Todo, pData), 'IAudioRenderClient.GetBuffer');
            for I := 0 to Todo * FMixCh - 1 do
            begin
              S := Samples[Pos * FMixCh + I];
              if FMixFloat then
                PSingle(pData + I * 4)^ := S
              else if FMixBits = 16 then
              begin
                V := Round(S * 32767);
                if V > 32767 then
                  V := 32767
                else if V < -32768 then
                  V := -32768;
                PSmallInt(pData + I * 2)^ := SmallInt(V);
              end
              else // PCM entero de 32 bits
              begin
                if S > 1.0 then
                  S := 1.0
                else if S < -1.0 then
                  S := -1.0;
                PInteger(pData + I * 4)^ := Round(S * 2147483647.0);
              end;
            end;
            RaiseIfFailed(RenderClient.ReleaseBuffer(Todo, 0), 'IAudioRenderClient.ReleaseBuffer');
            Inc(Pos, Todo);
          end;
        end;
      finally
        AudioClient.Stop;
        if WasPlaying then
          FOwner.HandleStateChange(False);
      end;
    except
      on E: Exception do
        if not Terminated then
          FOwner.HandleThreadError(E.Message);
    end;
  finally
    // Liberar interfaces COM ANTES de CoUninitialize
    RenderClient := nil;
    AudioClient := nil;
    Device := nil;
    Enumerator := nil;
    if CoInit then
      CoUninitialize;
  end;
end;

{$ENDIF}

{ TAiAudioPlayer }

constructor TAiAudioPlayer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FInDestroy := False;
  FActive := False;
  FDeviceId := '';
  FIsPlaying := False;
  // Profundidad 256 frases; PopItem con timeout de 100 ms para poder terminar
  FQueue := TThreadedQueue<TAiAudioPlayItem>.Create(256, INFINITE, 100);
end;

destructor TAiAudioPlayer.Destroy;
begin
  FInDestroy := True;
  StopPlayback;
  FQueue.Free;
  inherited;
end;

procedure TAiAudioPlayer.Loaded;
begin
  inherited;
  if FActive and not(csDesigning in ComponentState) then
    StartPlayback;
end;

procedure TAiAudioPlayer.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    Exit;
  FActive := Value;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) or FInDestroy then
    Exit;
  if FActive then
    StartPlayback
  else
    StopPlayback;
end;

procedure TAiAudioPlayer.SetDeviceId(const Value: string);
begin
  if FDeviceId = Value then
    Exit;
  if FActive then
    raise EInvalidOperation.Create('Cannot change DeviceId while player is active. Set Active to False first.');
  FDeviceId := Value;
end;

procedure TAiAudioPlayer.StartPlayback;
begin
{$IFDEF MSWINDOWS}
  if Assigned(FPlayThread) then
    Exit;
  FPlayThread := TAiWasapiPlayThread.Create(Self);
{$ELSE}
  FActive := False;
  DoError('TAiAudioPlayer solo esta disponible en Windows (WASAPI).');
{$ENDIF}
end;

procedure TAiAudioPlayer.StopPlayback;
begin
  if Assigned(FPlayThread) then
  begin
    FPlayThread.Terminate;
    FQueue.DoShutDown; // desbloquear PopItem
    FPlayThread.WaitFor;
    FreeAndNil(FPlayThread);
{$IFDEF MSWINDOWS}
    // Restaurar la cola tras el shutdown para poder reactivar el componente
    FQueue.Free;
    FQueue := TThreadedQueue<TAiAudioPlayItem>.Create(256, INFINITE, 100);
{$ENDIF}
  end;
  FIsPlaying := False;
end;

procedure TAiAudioPlayer.PlayPCM16(const aData: TBytes; aSampleRate, aChannels: Integer);
var
  Item: TAiAudioPlayItem;
begin
  if not FActive then
    raise EInvalidOperation.Create('TAiAudioPlayer is not active. Set Active to True first.');
  if Length(aData) = 0 then
    Exit;
  Item.Data := aData;
  Item.SampleRate := aSampleRate;
  Item.Channels := aChannels;
  FQueue.PushItem(Item);
end;

procedure TAiAudioPlayer.ClearQueue;
var
  Item: TAiAudioPlayItem;
begin
  while FQueue.PopItem(Item) = TWaitResult.wrSignaled do; // drena con timeout corto
end;

class function TAiAudioPlayer.GetPlaybackDevices: TArray<TAiAudioDeviceInfo>;
begin
  // Los dispositivos de salida son los mismos que enumera el capturador
  // para loopback (endpoints de render).
  Result := TAiAudioCapture.GetAudioDevices(asLoopback);
end;

procedure TAiAudioPlayer.DoError(const aMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if not(csDestroying in ComponentState) and Assigned(FOnError) then
        FOnError(Self, aMessage);
    end);
end;

procedure TAiAudioPlayer.HandleThreadError(const aMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if csDestroying in ComponentState then
        Exit;
      FActive := False;
      StopPlayback; // el hilo ya termino: WaitFor retorna de inmediato
      if Assigned(FOnError) then
        FOnError(Self, aMessage);
    end);
end;

procedure TAiAudioPlayer.HandleStateChange(aIsPlaying: Boolean);
begin
  FIsPlaying := aIsPlaying;
  TThread.Queue(nil,
    procedure
    begin
      if not(csDestroying in ComponentState) and Assigned(FOnStateChange) then
        FOnStateChange(Self, aIsPlaying);
    end);
end;

end.
