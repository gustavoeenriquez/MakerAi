unit uAudio.Player;

// Audio playback helper for MakerAIChat.
// Supported internally:  WAV, FLAC, Ogg-Opus (.opus / .ogg)
// Unsupported (returns False → caller uses ShellExecute): MP3, AAC, WMA, etc.

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  uMakerAi.Utils.AudioPushStream;

type
  TAudioPlayer = class
  private
    FPush: TAudioPushStream;
    function GetIsPlaying: Boolean;
    function PlayWAV(const APath: string): Boolean;
    function PlayFLAC(const APath: string): Boolean;
    function PlayOpus(const APath: string): Boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    // Returns True = played internally; False = caller should fall back to OS.
    function Play(const APath: string; const AMime: string = ''): Boolean;
    procedure Stop;

    property IsPlaying: Boolean read GetIsPlaying;
  end;

implementation

uses
  AudioTypes,
  WAVWriter,
  FLACStreamDecoder,
  OpusDecoder;

{ TAudioPlayer }

constructor TAudioPlayer.Create;
begin
  inherited;
  FPush := TAudioPushStream.Create;
end;

destructor TAudioPlayer.Destroy;
begin
  FPush.Stop;
  FPush.Free;
  inherited;
end;

function TAudioPlayer.GetIsPlaying: Boolean;
begin
  Result := Assigned(FPush) and FPush.IsPlaying;
end;

procedure TAudioPlayer.Stop;
begin
  FPush.Stop;
end;

{ ── WAV ────────────────────────────────────────────────────────────────────── }

function TAudioPlayer.PlayWAV(const APath: string): Boolean;
begin
  Result := False;
  try
    FPush.Stop;
    FPush.PushWavData(TFile.ReadAllBytes(APath));
    Result := True;
  except
  end;
end;

{ ── FLAC → in-memory WAV → Push ─────────────────────────────────────────── }

function TAudioPlayer.PlayFLAC(const APath: string): Boolean;
var
  Decoder: TFLACStreamDecoder;
  Writer : TWAVWriter;
  Stream : TMemoryStream;
  Buffer : TAudioBuffer;
  Res    : TAudioDecodeResult;
begin
  Result := False;
  Decoder := TFLACStreamDecoder.Create;
  try
    if not Decoder.Open(APath) then Exit;
    Stream := TMemoryStream.Create;
    try
      Writer := TWAVWriter.Create(Stream,
        Decoder.StreamInfo.SampleRate, Decoder.StreamInfo.Channels);
      try
        repeat
          Res := Decoder.Decode(Buffer);
          if Res in [adrOK, adrEndOfStream] then
            Writer.WriteSamples(Buffer);
        until Res <> adrOK;
        Writer.Finalize;
      finally
        Writer.Free;
      end;
      Stream.Position := 0;
      FPush.Stop;
      FPush.PushWavData(Stream);
      Result := True;
    finally
      Stream.Free;
    end;
  finally
    Decoder.Free;
  end;
end;

{ ── Ogg-Opus → in-memory WAV → Push ─────────────────────────────────────── }

function TAudioPlayer.PlayOpus(const APath: string): Boolean;
var
  FileStr: TFileStream;
  Decoder: TOpusDecoder;
  Writer : TWAVWriter;
  Stream : TMemoryStream;
  Buffer : TAudioBuffer;
  Samples: Integer;
  Res    : TAudioDecodeResult;
begin
  Result := False;
  FileStr := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  Decoder := TOpusDecoder.Create(FileStr, True); // Decoder owns & frees FileStr
  try
    if not Decoder.Ready then Exit;
    Stream := TMemoryStream.Create;
    try
      // Opus always decodes to 48 000 Hz; Channels comes from the Opus header.
      Writer := TWAVWriter.Create(Stream, 48000, Decoder.Channels);
      try
        repeat
          Res := Decoder.Decode(Buffer, Samples);
          if (Res in [adrOK, adrEndOfStream]) and (Samples > 0) then
            Writer.WriteSamples(Buffer, Samples);
        until Res <> adrOK;
        Writer.Finalize;
      finally
        Writer.Free;
      end;
      Stream.Position := 0;
      FPush.Stop;
      FPush.PushWavData(Stream);
      Result := True;
    finally
      Stream.Free;
    end;
  finally
    Decoder.Free;
  end;
end;

{ ── Format dispatch ─────────────────────────────────────────────────────── }

function TAudioPlayer.Play(const APath: string; const AMime: string = ''): Boolean;
var
  Ext  : string;
  LMime: string;
begin
  Result := False;
  if not TFile.Exists(APath) then Exit;

  Ext   := LowerCase(TPath.GetExtension(APath));
  LMime := LowerCase(AMime);

  if (Ext = '.wav')
     or LMime.StartsWith('audio/wav')
     or LMime.StartsWith('audio/x-wav')
  then
    Result := PlayWAV(APath)

  else if (Ext = '.flac')
     or LMime.StartsWith('audio/flac')
     or LMime.StartsWith('audio/x-flac')
  then
    Result := PlayFLAC(APath)

  else if (Ext = '.opus')
     or LMime.StartsWith('audio/opus')
  then
    Result := PlayOpus(APath)

  else if Ext = '.ogg' then
    // .ogg may be Ogg-Opus or Ogg-Vorbis; try Opus first, fails silently if Vorbis
    Result := PlayOpus(APath);

  // MP3 / AAC / WMA / M4A → Result stays False → caller uses ShellExecute
end;

end.
