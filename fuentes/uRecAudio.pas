unit uRecAudio;

interface

Uses System.SysUtils, FMX.Media, System.IOUtils,
{$IFDEF ANDROID}
  Androidapi.JNI.Media, Androidapi.Helpers,
{$ENDIF}
  FMX.types;

const

{$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  AUDIO_FILENAME = 'audioia.3gp';
{$ELSE}
  AUDIO_FILENAME = 'audioia.wav';
{$ENDIF}

Type
  TRecStatus = (rsNone, rsRecRecording, rsRecStop, rsPlaying, rsPlayPause,
    rsPlayStop);
  TRecEvent = Procedure(Sender: TObject; Status: TRecStatus) of object;

  TRecAudio = Class(TObject)
  Private
    FStatus: TRecStatus;
    FOnEvent: TRecEvent;
    procedure SetStatus(const Value: TRecStatus);
    procedure SetOnEvent(const Value: TRecEvent);
    procedure SetFileName(const Value: String);
  Protected
    FMicrophone: TAudioCaptureDevice;
    FFileName: String;
    FTiempo: TTime;
    FMediaPlayer: TMediaPlayer;
    Procedure DoStatus(aStatus: TRecStatus);
    procedure MicrophonePermissionRequest(Sender: TObject;
      const &Message: string; const AccessGranted: Boolean);
  Public
    Constructor Create(AOwner: TFmxObject); Reintroduce;
    Destructor Destroy; Override;
    function IsMicrophoneRecording: Boolean;
    Function RecStart: Boolean;
    Function RecStop: Boolean;
    Function PlayStart: Boolean;
    Function PlayStop: Boolean;
    Function PlayPause: Boolean;
    Function Duration: Double;
    Property Status: TRecStatus read FStatus write SetStatus;
    Property OnEvent: TRecEvent read FOnEvent write SetOnEvent;
    Property FileName: String read FFileName write SetFileName;

  End;

Var
  FRecAudio: TRecAudio;

implementation

{ TRecAudio }

{ GetAudioFileName resolves the audio file path for either platform. }

function GetAudioFileName(const AFileName: string): string;
begin
{$IFDEF ANDROID}
  Result := TPath.GetTempPath + '/' + AFileName;
{$ELSE}
{$IFDEF IOS}
  Result := TPath.GetHomePath + '/Documents/' + AFileName;
{$ELSE}
  Result := TPath.Combine(TPath.GetTempPath, AFileName);
{$ENDIF}
{$ENDIF}
end;

constructor TRecAudio.Create(AOwner: TFmxObject);
begin

  FMediaPlayer := TMediaPlayer.Create(AOwner);

  FileName := GetAudioFileName(AUDIO_FILENAME);

  {
    If FileExists(FFileName) then
    Try
    TFile.Delete(FFileName);
    Except
    End;

    // get the microphone device
    FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
    FMicrophone.FileName := FFileName;
    FMicrophone.OnPermissionRequest := MicrophonePermissionRequest;
    RecStop;
    PlayStop;
  }

{$IFDEF IOS}
  if GetUserInterfaceStyle = UIUserInterfaceStyleDark then
  begin
    InvertEffect1.Enabled := True;
    InvertEffect2.Enabled := True;
  end;
{$ENDIF}
end;

destructor TRecAudio.Destroy;
begin
  If Assigned(FMediaPlayer) then
    FMediaPlayer.Free;
  inherited;
end;

procedure TRecAudio.DoStatus(aStatus: TRecStatus);
begin
  if Assigned(FOnEvent) then
    FOnEvent(Self, aStatus);
end;

function TRecAudio.Duration: Double;
begin
  Result := FMediaPlayer.Duration / MediaTimeScale;
end;

function TRecAudio.IsMicrophoneRecording: Boolean;
begin
  Result := FMicrophone.State = TCaptureDeviceState.Capturing;
end;

procedure TRecAudio.MicrophonePermissionRequest(Sender: TObject;
  const Message: string; const AccessGranted: Boolean);
begin
  FMicrophone.StartCapture;
  FTiempo := EncodeTime(0, 0, 0, 0);
end;

function TRecAudio.PlayPause: Boolean;
begin
  FMediaPlayer.Stop;
  Status := TRecStatus.rsPlayPause;
end;

function TRecAudio.PlayStart: Boolean;
begin
  if IsMicrophoneRecording then
    RecStop;

  FMediaPlayer.FileName := FFileName;
  FMediaPlayer.Play;
  Status := TRecStatus.rsPlaying;

  FTiempo := EncodeTime(0, 0, 0, 0);
end;

function TRecAudio.PlayStop: Boolean;
begin
  FMediaPlayer.Stop;
  Status := TRecStatus.rsPlayStop;
end;

function TRecAudio.RecStart: Boolean;
begin
  RecStop;
  FMicrophone.RequestPermission;
  Status := TRecStatus.rsRecRecording;
end;

function TRecAudio.RecStop: Boolean;
begin
  if IsMicrophoneRecording then
  Begin
    FMicrophone.StopCapture;
    Status := TRecStatus.rsRecStop;
  End;
end;

procedure TRecAudio.SetFileName(const Value: String);
begin
  If FFileName <> Value then
  Begin

    FFileName := Value;

    {//Esto no va, ya que si quiero reproducir algo que ya está grabado lo borra y no se debe hacer
    If FileExists(FFileName) then
      Try
        TFile.Delete(FFileName);
      Except
      End;
      }

    { get the microphone device }
    FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
    FMicrophone.FileName := FFileName;
    FMicrophone.OnPermissionRequest := MicrophonePermissionRequest;
    RecStop;
    PlayStop;
  End;
end;

procedure TRecAudio.SetOnEvent(const Value: TRecEvent);
begin
  FOnEvent := Value;
end;

procedure TRecAudio.SetStatus(const Value: TRecStatus);
begin
  FStatus := Value;
  DoStatus(FStatus);
end;

initialization

FRecAudio := TRecAudio.Create(Nil);

finalization

FRecAudio.Free;

end.
