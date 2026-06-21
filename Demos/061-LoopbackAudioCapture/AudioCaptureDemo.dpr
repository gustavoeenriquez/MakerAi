// Demo de TAiAudioCapture: captura el audio que el sistema esta REPRODUCIENDO
// (WASAPI loopback) y lo guarda como WAV de 16 kHz mono, listo para Whisper
// o un Realtime STT.
//
// Uso: ejecutar, reproducir algo (YouTube, Zoom, musica...) y esperar.
// El resultado queda en loopback.wav junto al ejecutable.

program AudioCaptureDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  uMakerAi.Utils.AudioCapture in '..\..\Source\Utils\uMakerAi.Utils.AudioCapture.pas',
  uMakerAi.Utils.PcmToWav in '..\..\Source\Core\uMakerAi.Utils.PcmToWav.pas';

const
  RECORD_SECONDS = 10;

type
  // Los eventos del componente requieren metodos de objeto
  TCaptureHandler = class
  public
    Pcm: TMemoryStream;
    SampleRate: Integer;
    Channels: Integer;
    HasError: Boolean;
    procedure HandleFormat(Sender: TObject; aNativeSampleRate, aNativeChannels, aOutSampleRate, aOutChannels: Integer);
    procedure HandleData(Sender: TObject; const aBuffer: TBytes; aSampleRate, aChannels: Integer);
    procedure HandleError(Sender: TObject; const ErrorMessage: string);
  end;

procedure TCaptureHandler.HandleFormat(Sender: TObject; aNativeSampleRate, aNativeChannels, aOutSampleRate, aOutChannels: Integer);
begin
  WriteLn(Format('Formato nativo: %d Hz x %d canales  ->  salida: %d Hz x %d canales (PCM16)',
    [aNativeSampleRate, aNativeChannels, aOutSampleRate, aOutChannels]));
end;

procedure TCaptureHandler.HandleData(Sender: TObject; const aBuffer: TBytes; aSampleRate, aChannels: Integer);
begin
  if Length(aBuffer) > 0 then
    Pcm.WriteBuffer(aBuffer[0], Length(aBuffer));
  SampleRate := aSampleRate;
  Channels := aChannels;
  Write(Format(#13'Capturando... %7d bytes  (nivel: %5d)   ', [Pcm.Size, TAiAudioCapture(Sender).SoundLevel]));
end;

procedure TCaptureHandler.HandleError(Sender: TObject; const ErrorMessage: string);
begin
  WriteLn;
  WriteLn('ERROR: ' + ErrorMessage);
  HasError := True;
end;

var
  Capture: TAiAudioCapture;
  Handler: TCaptureHandler;
  Devices: TArray<TAiAudioDeviceInfo>;
  D: TAiAudioDeviceInfo;
  Watch: TStopwatch;
  Wav: TMemoryStream;
  OutFile: string;

begin
  try
    WriteLn('=== Demo TAiAudioCapture (WASAPI loopback) ===');
    WriteLn;
    WriteLn('Dispositivos de reproduccion disponibles:');
    Devices := TAiAudioCapture.GetAudioDevices(asLoopback);
    for D in Devices do
      if D.IsDefault then
        WriteLn('  * ' + D.DeviceName + '  (predeterminado)')
      else
        WriteLn('  - ' + D.DeviceName);
    WriteLn;

    Handler := TCaptureHandler.Create;
    Capture := TAiAudioCapture.Create(nil);
    try
      Handler.Pcm := TMemoryStream.Create;
      try
        Capture.Source := asLoopback; // audio de reproduccion del sistema
        Capture.OutputSampleRate := 16000; // optimo para Whisper / Realtime STT
        Capture.OutputChannels := 1; // mono
        Capture.OnFormat := Handler.HandleFormat;
        Capture.OnData := Handler.HandleData;
        Capture.OnError := Handler.HandleError;

        WriteLn(Format('Grabando %d segundos del audio del sistema (reproduce algo!)...', [RECORD_SECONDS]));
        Capture.Active := True;

        Watch := TStopwatch.StartNew;
        while (Watch.Elapsed.TotalSeconds < RECORD_SECONDS) and not Handler.HasError do
          CheckSynchronize(50); // procesa los eventos TThread.Queue en consola

        Capture.Active := False;
        CheckSynchronize(100);
        WriteLn;

        if Handler.Pcm.Size > 0 then
        begin
          OutFile := ExtractFilePath(ParamStr(0)) + 'loopback.wav';
          if ConvertPCMStreamToWAVStream(Handler.Pcm, Wav, Handler.SampleRate, Handler.Channels, 16) then
            try
              Wav.SaveToFile(OutFile);
              WriteLn('Audio guardado en: ' + OutFile);
            finally
              Wav.Free;
            end
          else
            WriteLn('Error convirtiendo PCM a WAV.');
        end
        else
          WriteLn('No se capturo audio.');
      finally
        Handler.Pcm.Free;
      end;
    finally
      Capture.Free;
      Handler.Free;
    end;

    WriteLn;
    WriteLn('Pulsa ENTER para salir...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;

end.
