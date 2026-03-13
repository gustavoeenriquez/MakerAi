// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Whisper: TTS y STT via endpoints de audio de OpenAI.
// Mantiene compatibilidad con la version de Github de Whisper opensource.
// El modelo estandar de OpenAi con nuevas caracteristicas esta en
// uMakerAi.OpenAi.Audio.

unit uMakerAi.Whisper;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, StrUtils,
  fphttpclient,
  fpjson, jsonparser,
  uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat.Messages;

type
  TAIWhisper = class(TAiSpeechToolBase)
  private
    FApiKey:     string;
    FUrl:        string;
    FModel:      string;
    FVoice:      string;
    FFormat:     string;
    FLanguaje:   string;
    FTemperature: Single;
    FSpeed:      Single;
    FResponseFormat: string;
    FQuality:    string;
    Ftimestamp_granularities: string;
    procedure SetApiKey(const Value: string);
    procedure SetUrl(const Value: string);
    procedure SetModel(const Value: string);
    procedure SetVoice(const Value: string);
    procedure SetFormat(const Value: string);
    procedure SetLanguaje(const Value: string);
    procedure SetSpeed(const Value: Single);
    procedure SetTemperature(const Value: Single);
    procedure SetResponseFormat(const Value: string);
    procedure SetQuality(const Value: string);
    procedure Settimestamp_granularities(const Value: string);
    function  GetApiKey: string;
  protected
    function  IsValidExtension(const AFileExtension: string): Boolean;
    procedure ConvertAudioIfNeeded(var aStream: TMemoryStream; var aFileName: string);
    procedure ExecuteTranscription(aMediaFile: TAiMediaFile;
      ResMsg, AskMsg: TAiChatMessage); override;
    procedure ExecuteSpeechGeneration(const AText: string;
      ResMsg, AskMsg: TAiChatMessage); override;
    function  InternalTranscription(aStream: TMemoryStream;
      const aFileName, aPrompt: string): string;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Proxy publicos para que TAiWhisperThread acceda a los metodos protegidos
    procedure PubReportState(AState: TAiChatState; const ADesc: string);
    procedure PubReportDataEnd(AMsg: TAiChatMessage; const ARole, AText: string);
    procedure PubReportError(const AMsg: string; E: Exception);

    function  Speech(const aText: string; const aVoice: string = ''): TMemoryStream;
    function  Transcription(aStream: TMemoryStream;
      aFileName, aPrompt: string): string;
    function  Translation(aStream: TMemoryStream;
      aFileName, aPrompt: string): string;
  published
    property ApiKey:    string read GetApiKey write SetApiKey;
    property Url:       string read FUrl      write SetUrl;
    property Model:     string read FModel    write SetModel;
    property Voice:     string read FVoice    write SetVoice;
    property Format:    string read FFormat   write SetFormat;
    property Languaje:  string read FLanguaje write SetLanguaje;
    property Speed:     Single read FSpeed    write SetSpeed;
    property Temperature: Single read FTemperature write SetTemperature;
    property ResponseFormat: string read FResponseFormat write SetResponseFormat;
    property Quality:   string read FQuality  write SetQuality;
    property timestamp_granularities: string
      read Ftimestamp_granularities write Settimestamp_granularities;
  end;

procedure Register;

implementation

uses
  process;

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

// ---------------------------------------------------------------------------
// Multipart helpers (standalone)
// ---------------------------------------------------------------------------

procedure WAppndField(AStream: TMemoryStream;
  const ABoundary, AName, AValue: string);
var
  S: string;
begin
  S := '--' + ABoundary + #13#10 +
       'Content-Disposition: form-data; name="' + AName + '"' + #13#10 +
       #13#10 + AValue + #13#10;
  AStream.WriteBuffer(PChar(S)^, Length(S));
end;

procedure WAppndFile(AStream: TMemoryStream;
  const ABoundary, AName, AFileName, AMimeType: string; AContent: TStream);
var
  S, Crlf: string;
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
// Conversion de audio via ffmpeg (TProcess — multiplataforma)
// ---------------------------------------------------------------------------

procedure ConvertAudioFileFormat(ASource: TMemoryStream; const AFileName: string;
  out ADest: TMemoryStream; out ADestFileName: string);
var
  TmpDir, FSrc, FDst: string;
  Proc: TProcess;
begin
  ADest := nil;
  ADestFileName := '';
  TmpDir  := IncludeTrailingPathDelimiter(GetTempDir(False));
  FSrc    := TmpDir + ExtractFileName(AFileName);
  ADestFileName := ChangeFileExt(ExtractFileName(AFileName), '.mp3');
  FDst    := TmpDir + ADestFileName;

  ASource.Position := 0;
  ASource.SaveToFile(FSrc);

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
    ADest := TMemoryStream.Create;
    ADest.LoadFromFile(FDst);
    ADest.Position := 0;
  end;
  SysUtils.DeleteFile(FSrc);
  SysUtils.DeleteFile(FDst);
end;

// ---------------------------------------------------------------------------
// Hilo background para el caso sincrono (IsAsync = False)
// Nota: accede a TAIWhisper via los proxies Pub* publicos.
// ---------------------------------------------------------------------------

type
  TAiWhisperMode = (whmTranscription, whmSpeech);

  TAiWhisperThread = class(TThread)
  private
    FOwner:     TAIWhisper;
    FMode:      TAiWhisperMode;
    FMediaFile: TAiMediaFile;
    FText:      string;
    FResMsg:    TAiChatMessage;
  public
    constructor Create(AOwner: TAIWhisper; AMode: TAiWhisperMode;
      AMediaFile: TAiMediaFile; const AText: string;
      AResMsg: TAiChatMessage);
    procedure Execute; override;
  end;

constructor TAiWhisperThread.Create(AOwner: TAIWhisper; AMode: TAiWhisperMode;
  AMediaFile: TAiMediaFile; const AText: string;
  AResMsg: TAiChatMessage);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FOwner     := AOwner;
  FMode      := AMode;
  FMediaFile := AMediaFile;
  FText      := AText;
  FResMsg    := AResMsg;
end;

procedure TAiWhisperThread.Execute;
var
  LText:    string;
  LStream:  TMemoryStream;
  LNewFile: TAiMediaFile;
begin
  case FMode of
    whmTranscription:
      try
        FOwner.PubReportState(acsReasoning, 'Transcribiendo audio...');
        LText := FOwner.InternalTranscription(
          FMediaFile.Content, FMediaFile.FileName, '');
        FMediaFile.Transcription := LText;
        FMediaFile.Procesado := True;
        FOwner.PubReportDataEnd(FResMsg, 'assistant', LText);
      except
        on E: Exception do
          FOwner.PubReportError('Error en transcripcion Whisper: ' + E.Message, E);
      end;
    whmSpeech:
      try
        FOwner.PubReportState(acsWriting, 'Generando voz...');
        LStream := FOwner.Speech(FText);
        try
          LNewFile := TAiMediaFile.Create;
          LNewFile.LoadFromStream('speech.' + FOwner.Format, LStream);
          FOwner.PubReportDataEnd(FResMsg, 'assistant', '[Audio generado]');
        finally
          LStream.Free;
        end;
      except
        on E: Exception do
          FOwner.PubReportError('Error generando voz Whisper: ' + E.Message, E);
      end;
  end;
end;

// ---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('MakerAI', [TAIWhisper]);
end;

{ TAIWhisper }

constructor TAIWhisper.Create(aOwner: TComponent);
begin
  inherited;
  FUrl         := GlOpenAIUrl;
  FApiKey      := '@OPENAI_API_KEY';
  FModel       := 'whisper-1';
  FVoice       := 'nova';
  FFormat      := 'mp3';
  FLanguaje    := 'es';
  FSpeed       := 1;
  FTemperature := 0;
  FResponseFormat := 'text';
  FQuality     := 'tts-1';
  Ftimestamp_granularities := '';
end;

destructor TAIWhisper.Destroy;
begin
  inherited;
end;

function TAIWhisper.GetApiKey: string;
begin
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
    Exit(FApiKey);
  if (FApiKey <> '') and (Copy(FApiKey, 1, 1) = '@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, Length(FApiKey)))
  else
    Result := FApiKey;
end;

procedure TAIWhisper.PubReportState(AState: TAiChatState; const ADesc: string);
begin
  ReportState(AState, ADesc);
end;

procedure TAIWhisper.PubReportDataEnd(AMsg: TAiChatMessage;
  const ARole, AText: string);
begin
  ReportDataEnd(AMsg, ARole, AText);
end;

procedure TAIWhisper.PubReportError(const AMsg: string; E: Exception);
begin
  ReportError(AMsg, E);
end;

function TAIWhisper.IsValidExtension(const AFileExtension: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(Trim(StringReplace(AFileExtension, '.', '', [rfReplaceAll])));
  Result := (Ext = 'mp3') or (Ext = 'mp4') or (Ext = 'mpeg') or
            (Ext = 'mpga') or (Ext = 'm4a') or (Ext = 'ogg') or
            (Ext = 'wav') or (Ext = 'webm');
end;

procedure TAIWhisper.ConvertAudioIfNeeded(var aStream: TMemoryStream;
  var aFileName: string);
var
  NewStream: TMemoryStream;
  NewName:   string;
begin
  if not IsValidExtension(ExtractFileExt(aFileName)) then
  begin
    ConvertAudioFileFormat(aStream, aFileName, NewStream, NewName);
    if Assigned(NewStream) then
    begin
      aStream.Clear;
      aStream.LoadFromStream(NewStream);
      aStream.Position := 0;
      aFileName := NewName;
      NewStream.Free;
    end;
  end;
end;

procedure TAIWhisper.ExecuteTranscription(aMediaFile: TAiMediaFile;
  ResMsg, AskMsg: TAiChatMessage);

  procedure DoTranscription;
  var
    LText: string;
  begin
    try
      ReportState(acsReasoning, 'Transcribiendo audio...');
      LText := InternalTranscription(
        aMediaFile.Content, aMediaFile.FileName, '');
      aMediaFile.Transcription := LText;
      aMediaFile.Procesado := True;
      ReportDataEnd(ResMsg, 'assistant', LText);
    except
      on E: Exception do
        ReportError('Error en transcripcion Whisper: ' + E.Message, E);
    end;
  end;

begin
  if IsAsync then
    DoTranscription
  else
    TAiWhisperThread.Create(Self, whmTranscription, aMediaFile, '', ResMsg);
end;

procedure TAIWhisper.ExecuteSpeechGeneration(const AText: string;
  ResMsg, AskMsg: TAiChatMessage);

  procedure DoSpeechGeneration;
  var
    LStream:  TMemoryStream;
    LNewFile: TAiMediaFile;
  begin
    try
      ReportState(acsWriting, 'Generando voz...');
      LStream := Speech(AText);
      try
        LNewFile := TAiMediaFile.Create;
        LNewFile.LoadFromStream('speech.' + FFormat, LStream);
        ReportDataEnd(ResMsg, 'assistant', '[Audio generado]');
      finally
        LStream.Free;
      end;
    except
      on E: Exception do
        ReportError('Error generando voz Whisper: ' + E.Message, E);
    end;
  end;

begin
  if IsAsync then
    DoSpeechGeneration
  else
    TAiWhisperThread.Create(Self, whmSpeech, nil, AText, ResMsg);
end;

function TAIWhisper.InternalTranscription(aStream: TMemoryStream;
  const aFileName, aPrompt: string): string;
begin
  Result := Transcription(aStream, aFileName, aPrompt);
end;

function TAIWhisper.Speech(const aText: string; const aVoice: string = ''): TMemoryStream;
var
  Client:      TFPHTTPClient;
  JObj:        TJSONObject;
  ReqStream:   TStringStream;
  Response:    TMemoryStream;
  sUrl:        string;
  SelVoice:    string;
begin
  if aVoice = '' then
    SelVoice := FVoice
  else
    SelVoice := aVoice;

  sUrl := FUrl + 'audio/speech';

  JObj := TJSONObject.Create;
  try
    JObj.Add('model', FModel);
    JObj.Add('input', aText);
    JObj.Add('voice', SelVoice);
    JObj.Add('response_format', FFormat);
    JObj.Add('speed', Double(FSpeed));
    ReqStream := TStringStream.Create(JObj.AsJSON);
  finally
    JObj.Free;
  end;

  Response := TMemoryStream.Create;
  Client   := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + GetApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    Client.RequestBody := ReqStream;
    Client.HTTPMethod('POST', sUrl, Response, [200]);
    Response.Position := 0;
    Result   := Response;
    Response := nil; // transfiere ownership al caller
  finally
    Client.Free;
    ReqStream.Free;
    Response.Free; // nil si se transfiere, o limpieza en excepcion
  end;
end;

function TAIWhisper.Transcription(aStream: TMemoryStream;
  aFileName, aPrompt: string): string;
var
  Client:    TFPHTTPClient;
  MPStream:  TMemoryStream;
  RespStream: TStringStream;
  Boundary, CloseStr, MimeType, sUrl: string;
begin
  ConvertAudioIfNeeded(aStream, aFileName);
  sUrl     := FUrl + 'audio/transcriptions';
  Boundary := 'WHBound' + IntToStr(Random(999999));
  MPStream := TMemoryStream.Create;
  try
    MimeType := GetMimeTypeFromFileName(ExtractFileExt(aFileName));
    if MimeType = '' then
      MimeType := 'audio/mpeg';

    aStream.Position := 0;
    WAppndFile(MPStream, Boundary, 'file', aFileName, MimeType, aStream);
    WAppndField(MPStream, Boundary, 'model', FModel);
    if aPrompt <> '' then
      WAppndField(MPStream, Boundary, 'prompt', aPrompt);
    WAppndField(MPStream, Boundary, 'response_format', FResponseFormat);
    WAppndField(MPStream, Boundary, 'temperature',
      FormatFloat('0.0', FTemperature));
    if FLanguaje <> '' then
      WAppndField(MPStream, Boundary, 'language', FLanguaje);
    if Pos('word', Ftimestamp_granularities) > 0 then
      WAppndField(MPStream, Boundary, 'timestamp_granularities[]', 'word');
    if Pos('segment', Ftimestamp_granularities) > 0 then
      WAppndField(MPStream, Boundary, 'timestamp_granularities[]', 'segment');

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
      Result := RespStream.DataString;
    finally
      Client.Free;
      RespStream.Free;
    end;
  finally
    MPStream.Free;
  end;
end;

function TAIWhisper.Translation(aStream: TMemoryStream;
  aFileName, aPrompt: string): string;
var
  Client:     TFPHTTPClient;
  MPStream:   TMemoryStream;
  RespStream: TStringStream;
  Boundary, CloseStr, MimeType, sUrl: string;
begin
  ConvertAudioIfNeeded(aStream, aFileName);
  sUrl     := FUrl + 'audio/translations';
  Boundary := 'WHBound' + IntToStr(Random(999999));
  MPStream := TMemoryStream.Create;
  try
    MimeType := GetMimeTypeFromFileName(ExtractFileExt(aFileName));
    if MimeType = '' then
      MimeType := 'audio/mpeg';

    aStream.Position := 0;
    WAppndFile(MPStream, Boundary, 'file', aFileName, MimeType, aStream);
    WAppndField(MPStream, Boundary, 'model', FModel);
    if aPrompt <> '' then
      WAppndField(MPStream, Boundary, 'prompt', aPrompt);
    WAppndField(MPStream, Boundary, 'response_format', FResponseFormat);
    WAppndField(MPStream, Boundary, 'temperature',
      FormatFloat('0.0', FTemperature));

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
      Result := RespStream.DataString;
    finally
      Client.Free;
      RespStream.Free;
    end;
  finally
    MPStream.Free;
  end;
end;

// Property setters

procedure TAIWhisper.SetApiKey(const Value: string);
begin
  FApiKey := Value;
end;

procedure TAIWhisper.SetFormat(const Value: string);
begin
  FFormat := Value;
end;

procedure TAIWhisper.SetLanguaje(const Value: string);
begin
  FLanguaje := Value;
end;

procedure TAIWhisper.SetModel(const Value: string);
begin
  FModel := Value;
end;

procedure TAIWhisper.SetQuality(const Value: string);
begin
  FQuality := Value;
end;

procedure TAIWhisper.SetResponseFormat(const Value: string);
begin
  FResponseFormat := Value;
end;

procedure TAIWhisper.SetSpeed(const Value: Single);
begin
  FSpeed := Trunc(Value * 10) / 10;
end;

procedure TAIWhisper.SetTemperature(const Value: Single);
begin
  FTemperature := Trunc(Value * 10) / 10;
end;

procedure TAIWhisper.Settimestamp_granularities(const Value: string);
begin
  Ftimestamp_granularities := Value;
end;

procedure TAIWhisper.SetUrl(const Value: string);
begin
  if Value <> '' then
    FUrl := Value
  else
    FUrl := GlOpenAIUrl;
end;

procedure TAIWhisper.SetVoice(const Value: string);
begin
  FVoice := Value;
end;

end.
