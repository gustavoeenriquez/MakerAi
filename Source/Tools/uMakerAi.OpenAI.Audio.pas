// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// TAiOpenAiAudio: TTS y STT con enums para modelos/voces/formatos,
// soporte de streaming simplificado (POST sincrono + eventos al final),
// y conversion de audio via ffmpeg.
//
// Adaptaciones FPC:
//   - TNetHTTPClient          -> TFPHTTPClient (fphttpclient)
//   - TMultipartFormData      -> multipart manual con TMemoryStream
//   - TStringBuilder          -> string acumulado
//   - TThread.Queue           -> llamada directa (ya en hilo correcto)
//   - TPath / TFile           -> SysUtils equivalentes
//   - TJSONObject.ParseJSONValue -> GetJSON (fpjson/jsonparser)
//   - .TryGetValue<T>         -> .Find + cast
//   - Inline var declarations -> variables pre-declaradas
//
// NOTA streaming: TFPHTTPClient no expone OnReceiveData. SpeechStreamed y
// TranscribeStreamed hacen POST completo y luego disparan los eventos de
// bloque. Para streaming real se requiere un cliente HTTP custom.

unit uMakerAi.OpenAI.Audio;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fphttpclient,
  fpjson, jsonparser,
  uMakerAi.Core;

type
  // --- Enums de parametros de la API ---
  TAiTTSModel      = (tts_1, tts_1_hd, gpt_4o_mini_tts);
  TAiTTSVoice      = (tvAlloy, tvAsh, tvBallad, tvCoral, tvEcho, tvFable,
                      tvOnyx, tvNova, tvSage, tvShimmer, tvVerse);
  TAiTTSResponseFormat = (trfMp3, trfOpus, trfAac, trfFlac, trfWav, trfPcm);

  TAiTranscriptionModel = (tmWhisper1, tmGpt4o, tmGpt4oMini, tmGpt4oDiarize);
  TAiTranscriptionResponseFormat = (trfJson, trfText, trfSrt, trfVerboseJson,
                                    trfVtt, trfDiarizedJson);

  // --- Clase para resultados de transcripcion ---
  TTranscriptionResult = class
  private
    FText:       string;
    FJsonObject: TJSONObject;
    FDuration:   Double;
    FLanguage:   string;
  public
    constructor Create(const AResponse: string;
                       AFormat: TAiTranscriptionResponseFormat);
    destructor Destroy; override;
    property Text:     string      read FText;
    property Duration: Double      read FDuration;
    property Language: string      read FLanguage;
    property RawJson:  TJSONObject read FJsonObject;
  end;

  // --- Eventos de streaming ---
  TOnAudioChunkReceived     = procedure(Sender: TObject;
                                        const AAudioChunk: TBytes) of object;
  TOnSpeechCompleted        = procedure(Sender: TObject) of object;
  TOnTranscriptDeltaReceived = procedure(Sender: TObject;
                                         const ATextDelta: string) of object;
  TOnTranscriptionCompleted = procedure(Sender: TObject;
                                        const AFinalResult: TTranscriptionResult) of object;
  TOnAudioError             = procedure(Sender: TObject;
                                        const AMessage: string) of object;

  TAiOpenAiAudio = class(TComponent)
  private
    FApiKey: string;
    FUrl:    string;
    // TTS
    FTTSModel:              TAiTTSModel;
    FTTSVoice:              TAiTTSVoice;
    FTTSResponseFormat:     TAiTTSResponseFormat;
    FTTSSpeed:              Double;
    FTTSInstructions:       string;
    // Transcripcion
    FTranscriptionModel:    TAiTranscriptionModel;
    FTranscriptionResponseFormat: TAiTranscriptionResponseFormat;
    FTranscriptionLanguage: string;
    FTranscriptionTemperature: Double;
    FTranscriptionTimestampGranularities: TAiTimestampGranularities;
    // Eventos
    FOnAudioChunkReceived:     TOnAudioChunkReceived;
    FOnSpeechCompleted:        TOnSpeechCompleted;
    FOnTranscriptDeltaReceived: TOnTranscriptDeltaReceived;
    FOnTranscriptionCompleted: TOnTranscriptionCompleted;
    FOnAudioError:             TOnAudioError;

    function GetApiKey: string;
    procedure SetApiKey(const Value: string);
    procedure SetUrl(const Value: string);

    function  TTSModelStr:  string;
    function  TTSVoiceStr:  string;
    function  TTSFormatStr: string;
    function  TranscriptionModelStr:  string;
    function  TranscriptionFormatStr: string;
  protected
    procedure ConvertAudioIfNeeded(aMediaFile: TAiMediaFile);
    procedure BuildTranscriptionBody(AStream: TMemoryStream;
      const ABoundary: string; const AAudioFile: TAiMediaFile;
      const APrompt: string = '');
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // --- TTS ---
    function  Speech(const AInput: string): TMemoryStream; overload;
    procedure Speech(const AInput: string; const AOutputStream: TStream); overload;
    procedure SpeechStreamed(const AInput: string);

    // --- STT ---
    function  Transcribe(const AAudioFile: TAiMediaFile;
                         const APrompt: string = ''): TTranscriptionResult;
    procedure TranscribeStreamed(const AAudioFile: TAiMediaFile);

    // --- Traduccion ---
    function TranslateToEnglish(const AAudioFile: TAiMediaFile;
                                const APrompt: string = ''): TTranscriptionResult;
  published
    property ApiKey: string read GetApiKey write SetApiKey;
    property Url:    string read FUrl      write SetUrl;

    // TTS
    property TTSModel:          TAiTTSModel
      read FTTSModel write FTTSModel default tts_1;
    property TTSVoice:          TAiTTSVoice
      read FTTSVoice write FTTSVoice default tvAlloy;
    property TTSResponseFormat: TAiTTSResponseFormat
      read FTTSResponseFormat write FTTSResponseFormat default trfMp3;
    property TTSSpeed:          Double
      read FTTSSpeed write FTTSSpeed;
    property TTSInstructions:   string
      read FTTSInstructions write FTTSInstructions;

    // Transcripcion
    property TranscriptionModel: TAiTranscriptionModel
      read FTranscriptionModel write FTranscriptionModel default tmWhisper1;
    property TranscriptionResponseFormat: TAiTranscriptionResponseFormat
      read FTranscriptionResponseFormat write FTranscriptionResponseFormat
      default trfJson;
    property TranscriptionLanguage: string
      read FTranscriptionLanguage write FTranscriptionLanguage;
    property TranscriptionTemperature: Double
      read FTranscriptionTemperature write FTranscriptionTemperature;
    property TranscriptionTimestampGranularities: TAiTimestampGranularities
      read FTranscriptionTimestampGranularities
      write FTranscriptionTimestampGranularities;

    // Eventos
    property OnAudioChunkReceived:     TOnAudioChunkReceived
      read FOnAudioChunkReceived     write FOnAudioChunkReceived;
    property OnSpeechCompleted:        TOnSpeechCompleted
      read FOnSpeechCompleted        write FOnSpeechCompleted;
    property OnTranscriptDeltaReceived: TOnTranscriptDeltaReceived
      read FOnTranscriptDeltaReceived write FOnTranscriptDeltaReceived;
    property OnTranscriptionCompleted: TOnTranscriptionCompleted
      read FOnTranscriptionCompleted write FOnTranscriptionCompleted;
    property OnAudioError:             TOnAudioError
      read FOnAudioError             write FOnAudioError;
  end;

procedure Register;

implementation

uses
  process, Math;

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

// ---------------------------------------------------------------------------
// Conversion de audio via ffmpeg
// ---------------------------------------------------------------------------

procedure ConvertAudioFileFormat(const ASourceStream: TStream;
  const ASourceFilename: string;
  out ADestStream: TMemoryStream; out ADestFilename: string);
var
  TmpDir, FSrc, FDst: string;
  Proc: TProcess;
  FS: TFileStream;
  Buf: array of Byte;
begin
  ADestStream := nil;
  ADestFilename := IntToStr(Random(MaxInt)) + '_' +
                   ChangeFileExt(ExtractFileName(ASourceFilename), '.mp3');
  TmpDir := IncludeTrailingPathDelimiter(GetTempDir(False));
  FSrc   := TmpDir + ExtractFileName(ASourceFilename);
  FDst   := TmpDir + ADestFilename;

  // Escribir origen a archivo temporal
  SetLength(Buf, ASourceStream.Size);
  ASourceStream.Position := 0;
  if ASourceStream.Size > 0 then
    ASourceStream.ReadBuffer(Buf[0], ASourceStream.Size);
  FS := TFileStream.Create(FSrc, fmCreate);
  try
    if Length(Buf) > 0 then
      FS.WriteBuffer(Buf[0], Length(Buf));
  finally
    FS.Free;
  end;

  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'ffmpeg';
    Proc.Parameters.Add('-i');
    Proc.Parameters.Add(FSrc);
    Proc.Parameters.Add('-y');
    Proc.Parameters.Add(FDst);
    Proc.Options    := [poWaitOnExit, poNoConsole];
    Proc.ShowWindow := swoHide;
    Proc.Execute;
  finally
    Proc.Free;
  end;

  if FileExists(FDst) then
  begin
    ADestStream := TMemoryStream.Create;
    ADestStream.LoadFromFile(FDst);
    ADestStream.Position := 0;
  end;

  try SysUtils.DeleteFile(FSrc); except end;
  try SysUtils.DeleteFile(FDst); except end;
end;

// ---------------------------------------------------------------------------
// Multipart helpers
// ---------------------------------------------------------------------------

procedure OAAppndField(AStream: TMemoryStream;
  const ABoundary, AName, AValue: string);
var S: string;
begin
  S := '--' + ABoundary + #13#10 +
       'Content-Disposition: form-data; name="' + AName + '"' + #13#10 +
       #13#10 + AValue + #13#10;
  AStream.WriteBuffer(PChar(S)^, Length(S));
end;

procedure OAAppndFile(AStream: TMemoryStream;
  const ABoundary, AName, AFileName, AMimeType: string; AContent: TStream);
var S, Crlf: string;
begin
  S := '--' + ABoundary + #13#10 +
       'Content-Disposition: form-data; name="' + AName +
       '"; filename="' + AFileName + '"' + #13#10 +
       'Content-Type: ' + AMimeType + #13#10 + #13#10;
  AStream.WriteBuffer(PChar(S)^, Length(S));
  AContent.Position := 0;
  AStream.CopyFrom(AContent, AContent.Size);
  Crlf := #13#10;
  AStream.WriteBuffer(PChar(Crlf)^, Length(Crlf));
end;

// ---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenAiAudio]);
end;

{ TTranscriptionResult }

constructor TTranscriptionResult.Create(const AResponse: string;
  AFormat: TAiTranscriptionResponseFormat);
var
  JRoot: TJSONData;
  JObj:  TJSONObject;
  JVal:  TJSONData;
begin
  inherited Create;
  FText := '';
  FDuration := 0;
  FLanguage := '';
  FJsonObject := nil;

  if AFormat in [trfJson, trfVerboseJson, trfDiarizedJson] then
  begin
    JRoot := GetJSON(AResponse);
    if Assigned(JRoot) and (JRoot.JSONType = jtObject) then
    begin
      JObj := TJSONObject(JRoot);
      FJsonObject := JObj;     // transfers ownership
      JVal := JObj.Find('text');
      if Assigned(JVal) then FText := JVal.AsString;
      JVal := JObj.Find('duration');
      if Assigned(JVal) then FDuration := JVal.AsFloat;
      JVal := JObj.Find('language');
      if Assigned(JVal) then FLanguage := JVal.AsString;
    end
    else
      JRoot.Free;
  end
  else
    FText := AResponse;
end;

destructor TTranscriptionResult.Destroy;
begin
  FJsonObject.Free;
  inherited;
end;

{ TAiOpenAiAudio }

constructor TAiOpenAiAudio.Create(AOwner: TComponent);
begin
  inherited;
  FUrl                       := GlOpenAIUrl;
  FApiKey                    := '@OPENAI_API_KEY';
  FTTSModel                  := tts_1;
  FTTSVoice                  := tvAlloy;
  FTTSResponseFormat         := trfMp3;
  FTTSSpeed                  := 1.0;
  FTranscriptionModel        := tmWhisper1;
  FTranscriptionResponseFormat := trfJson;
  FTranscriptionTemperature  := 0.0;
end;

destructor TAiOpenAiAudio.Destroy;
begin
  inherited;
end;

function TAiOpenAiAudio.GetApiKey: string;
begin
  if csDesigning in ComponentState then
    Exit(FApiKey);
  if (FApiKey <> '') and (Copy(FApiKey, 1, 1) = '@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, Length(FApiKey)))
  else
    Result := FApiKey;
end;

procedure TAiOpenAiAudio.SetApiKey(const Value: string);
begin
  FApiKey := Value;
end;

procedure TAiOpenAiAudio.SetUrl(const Value: string);
begin
  FUrl := Value;
end;

function TAiOpenAiAudio.TTSModelStr: string;
begin
  case FTTSModel of
    tts_1:          Result := 'tts-1';
    tts_1_hd:       Result := 'tts-1-hd';
    gpt_4o_mini_tts: Result := 'gpt-4o-mini-tts';
  else
    Result := 'tts-1';
  end;
end;

function TAiOpenAiAudio.TTSVoiceStr: string;
begin
  case FTTSVoice of
    tvAlloy:   Result := 'alloy';
    tvAsh:     Result := 'ash';
    tvBallad:  Result := 'ballad';
    tvCoral:   Result := 'coral';
    tvEcho:    Result := 'echo';
    tvFable:   Result := 'fable';
    tvOnyx:    Result := 'onyx';
    tvNova:    Result := 'nova';
    tvSage:    Result := 'sage';
    tvShimmer: Result := 'shimmer';
    tvVerse:   Result := 'verse';
  else
    Result := 'alloy';
  end;
end;

function TAiOpenAiAudio.TTSFormatStr: string;
begin
  case FTTSResponseFormat of
    trfMp3:  Result := 'mp3';
    trfOpus: Result := 'opus';
    trfAac:  Result := 'aac';
    trfFlac: Result := 'flac';
    trfWav:  Result := 'wav';
    trfPcm:  Result := 'pcm';
  else
    Result := 'mp3';
  end;
end;

function TAiOpenAiAudio.TranscriptionModelStr: string;
begin
  case FTranscriptionModel of
    tmWhisper1:    Result := 'whisper-1';
    tmGpt4o:       Result := 'gpt-4o-transcribe';
    tmGpt4oMini:   Result := 'gpt-4o-mini-transcribe';
    tmGpt4oDiarize: Result := 'gpt-4o-transcribe-diarize';
  else
    Result := 'whisper-1';
  end;
end;

function TAiOpenAiAudio.TranscriptionFormatStr: string;
begin
  case FTranscriptionResponseFormat of
    trfJson:        Result := 'json';
    trfText:        Result := 'text';
    trfSrt:         Result := 'srt';
    trfVerboseJson: Result := 'verbose_json';
    trfVtt:         Result := 'vtt';
    trfDiarizedJson: Result := 'diarized_json';
  else
    Result := 'json';
  end;
end;

procedure TAiOpenAiAudio.ConvertAudioIfNeeded(aMediaFile: TAiMediaFile);
const
  VALID_EXTS: array[0..8] of string = (
    '.flac', '.mp3', '.mp4', '.mpeg', '.mpga', '.m4a', '.ogg', '.wav', '.webm');
var
  Ext:      string;
  I:        Integer;
  Found:    Boolean;
  NewStream: TMemoryStream;
  NewName:  string;
begin
  Ext   := LowerCase(ExtractFileExt(aMediaFile.FileName));
  Found := False;
  for I := Low(VALID_EXTS) to High(VALID_EXTS) do
    if VALID_EXTS[I] = Ext then
    begin
      Found := True;
      Break;
    end;

  if not Found then
  begin
    ConvertAudioFileFormat(aMediaFile.Content, aMediaFile.FileName,
      NewStream, NewName);
    if Assigned(NewStream) then
    begin
      aMediaFile.LoadFromStream(NewName, NewStream);
      NewStream.Free;
    end;
  end;
end;

procedure TAiOpenAiAudio.BuildTranscriptionBody(AStream: TMemoryStream;
  const ABoundary: string; const AAudioFile: TAiMediaFile;
  const APrompt: string = '');
var
  MimeType: string;
begin
  MimeType := GetMimeTypeFromFileName(ExtractFileExt(AAudioFile.FileName));
  if MimeType = '' then
    MimeType := 'audio/mpeg';

  AAudioFile.Content.Position := 0;
  OAAppndFile(AStream, ABoundary, 'file',
    AAudioFile.FileName, MimeType, AAudioFile.Content);

  OAAppndField(AStream, ABoundary, 'model', TranscriptionModelStr);
  if APrompt <> '' then
    OAAppndField(AStream, ABoundary, 'prompt', APrompt);
  if FTranscriptionLanguage <> '' then
    OAAppndField(AStream, ABoundary, 'language', FTranscriptionLanguage);
  if FTranscriptionTemperature <> 0.0 then
    OAAppndField(AStream, ABoundary, 'temperature',
      Format('%f', [FTranscriptionTemperature]));
  OAAppndField(AStream, ABoundary, 'response_format', TranscriptionFormatStr);

  if tsgWord in FTranscriptionTimestampGranularities then
    OAAppndField(AStream, ABoundary, 'timestamp_granularities[]', 'word');
  if tsgSegment in FTranscriptionTimestampGranularities then
    OAAppndField(AStream, ABoundary, 'timestamp_granularities[]', 'segment');
end;

// ---------------------------------------------------------------------------
// TTS
// ---------------------------------------------------------------------------

function TAiOpenAiAudio.Speech(const AInput: string): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    Speech(AInput, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TAiOpenAiAudio.Speech(const AInput: string; const AOutputStream: TStream);
var
  Client:    TFPHTTPClient;
  JObj:      TJSONObject;
  ReqStream: TStringStream;
  sUrl:      string;
begin
  sUrl := FUrl + 'audio/speech';
  JObj := TJSONObject.Create;
  try
    JObj.Add('input', AInput);
    JObj.Add('model', TTSModelStr);
    JObj.Add('voice', TTSVoiceStr);
    JObj.Add('response_format', TTSFormatStr);
    if FTTSSpeed <> 1.0 then
      JObj.Add('speed', FTTSSpeed);
    if (FTTSModel = gpt_4o_mini_tts) and (FTTSInstructions <> '') then
      JObj.Add('instructions', FTTSInstructions);
    ReqStream := TStringStream.Create(JObj.AsJSON);
  finally
    JObj.Free;
  end;

  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + GetApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    Client.RequestBody := ReqStream;
    Client.HTTPMethod('POST', sUrl, AOutputStream, [200]);
  finally
    Client.Free;
    ReqStream.Free;
  end;
end;

procedure TAiOpenAiAudio.SpeechStreamed(const AInput: string);
var
  AudioStream: TMemoryStream;
  Chunk:       TBytes;
begin
  // TFPHTTPClient no expone OnReceiveData. Hacemos POST completo
  // y luego disparamos el evento con el audio completo en un solo chunk.
  AudioStream := TMemoryStream.Create;
  try
    Speech(AInput, AudioStream);
    AudioStream.Position := 0;
    if (AudioStream.Size > 0) and Assigned(FOnAudioChunkReceived) then
    begin
      SetLength(Chunk, AudioStream.Size);
      AudioStream.ReadBuffer(Chunk[0], AudioStream.Size);
      FOnAudioChunkReceived(Self, Chunk);
    end;
  finally
    AudioStream.Free;
  end;

  if Assigned(FOnSpeechCompleted) then
    FOnSpeechCompleted(Self);
end;

// ---------------------------------------------------------------------------
// STT
// ---------------------------------------------------------------------------

function TAiOpenAiAudio.Transcribe(const AAudioFile: TAiMediaFile;
  const APrompt: string = ''): TTranscriptionResult;
var
  Client:    TFPHTTPClient;
  MPStream:  TMemoryStream;
  RespStream: TStringStream;
  Boundary, CloseStr, sUrl: string;
begin
  ConvertAudioIfNeeded(AAudioFile);
  sUrl     := FUrl + 'audio/transcriptions';
  Boundary := 'OABound' + IntToStr(Random(999999));
  MPStream := TMemoryStream.Create;
  try
    BuildTranscriptionBody(MPStream, Boundary, AAudioFile, APrompt);
    CloseStr := '--' + Boundary + '--' + #13#10;
    MPStream.WriteBuffer(PChar(CloseStr)^, Length(CloseStr));
    MPStream.Position := 0;

    RespStream := TStringStream.Create('');
    Client     := TFPHTTPClient.Create(nil);
    try
      Client.AddHeader('Authorization', 'Bearer ' + GetApiKey);
      Client.AddHeader('Content-Type',
        'multipart/form-data; boundary=' + Boundary);
      Client.RequestBody := MPStream;
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
      Result := TTranscriptionResult.Create(
        RespStream.DataString, FTranscriptionResponseFormat);
    finally
      Client.Free;
      RespStream.Free;
    end;
  finally
    MPStream.Free;
  end;
end;

procedure TAiOpenAiAudio.TranscribeStreamed(const AAudioFile: TAiMediaFile);
var
  LResult: TTranscriptionResult;
begin
  // TFPHTTPClient no expone OnReceiveData.
  // Hacemos transcripcion completa y disparamos OnTranscriptionCompleted.
  LResult := Transcribe(AAudioFile);
  try
    if Assigned(FOnTranscriptionCompleted) then
      FOnTranscriptionCompleted(Self, LResult);
  finally
    LResult.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Traduccion
// ---------------------------------------------------------------------------

function TAiOpenAiAudio.TranslateToEnglish(const AAudioFile: TAiMediaFile;
  const APrompt: string = ''): TTranscriptionResult;
var
  Client:     TFPHTTPClient;
  MPStream:   TMemoryStream;
  RespStream: TStringStream;
  Boundary, CloseStr, sUrl, FormatStr: string;
  MimeType: string;
begin
  ConvertAudioIfNeeded(AAudioFile);
  sUrl     := FUrl + 'audio/translations';
  Boundary := 'OABound' + IntToStr(Random(999999));
  MPStream := TMemoryStream.Create;
  try
    MimeType := GetMimeTypeFromFileName(ExtractFileExt(AAudioFile.FileName));
    if MimeType = '' then MimeType := 'audio/mpeg';

    AAudioFile.Content.Position := 0;
    OAAppndFile(MPStream, Boundary, 'file',
      AAudioFile.FileName, MimeType, AAudioFile.Content);

    // Translations solo soporta whisper-1
    OAAppndField(MPStream, Boundary, 'model', 'whisper-1');
    if APrompt <> '' then
      OAAppndField(MPStream, Boundary, 'prompt', APrompt);
    if FTranscriptionTemperature <> 0.0 then
      OAAppndField(MPStream, Boundary, 'temperature',
        Format('%f', [FTranscriptionTemperature]));

    case FTranscriptionResponseFormat of
      trfJson:         FormatStr := 'json';
      trfText:         FormatStr := 'text';
      trfSrt:          FormatStr := 'srt';
      trfVerboseJson:  FormatStr := 'verbose_json';
      trfVtt:          FormatStr := 'vtt';
      trfDiarizedJson: FormatStr := 'json'; // no soportado en traduccion
    else
      FormatStr := 'json';
    end;
    OAAppndField(MPStream, Boundary, 'response_format', FormatStr);

    CloseStr := '--' + Boundary + '--' + #13#10;
    MPStream.WriteBuffer(PChar(CloseStr)^, Length(CloseStr));
    MPStream.Position := 0;

    RespStream := TStringStream.Create('');
    Client     := TFPHTTPClient.Create(nil);
    try
      Client.AddHeader('Authorization', 'Bearer ' + GetApiKey);
      Client.AddHeader('Content-Type',
        'multipart/form-data; boundary=' + Boundary);
      Client.RequestBody := MPStream;
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
      Result := TTranscriptionResult.Create(
        RespStream.DataString, FTranscriptionResponseFormat);
    finally
      Client.Free;
      RespStream.Free;
    end;
  finally
    MPStream.Free;
  end;
end;

end.
