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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- CAMBIOS --------------------
// 04/11/2025 - Renombrado de TAIWhisper a TAiAudio y unidad a uMakerAi.OpenAI.Audio.pas.
// 04/11/2025 - Uso de Enums para modelos, voces y formatos para mayor seguridad.
// 04/11/2025 - Integración con TAiMediaFile de uMakerAi.Core.
// 04/11/2025 - Añadido soporte para modelos GPT-4o, diarización e 'instructions' en TTS.
// 04/11/2025 - Implementación completa de streaming para TTS y Transcripción con eventos.
// 04/11/2025 - Métodos de Transcripción/Traducción devuelven un objeto TTranscriptionResult.
// 04/11/2025 - Mantenida la lógica de conversión de audio con ffmpeg.

unit uMakerAi.OpenAI.Audio;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.Variants, System.Net.Mime, System.IOUtils,
  System.Generics.Collections, System.NetEncoding, System.JSON, System.Rtti,
  System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, REST.JSON, REST.Types, REST.Client,
  uMakerAi.Core;

type
  // --- Enums for API Parameters ---
  TAiTTSModel = (tts_1, tts_1_hd, gpt_4o_mini_tts);
  TAiTTSVoice = (tvAlloy, tvAsh, tvBallad, tvCoral, tvEcho, tvFable, tvOnyx, tvNova, tvSage, tvShimmer, tvVerse);
  TAiTTSResponseFormat = (trfMp3, trfOpus, trfAac, trfFlac, trfWav, trfPcm);

  TAiTranscriptionModel = (tmWhisper1, tmGpt4o, tmGpt4oMini, tmGpt4oDiarize);
  TAiTranscriptionResponseFormat = (trfJson, trfText, trfSrt, trfVerboseJson, trfVtt, trfDiarizedJson);
  TStreamOperation = (soNone, soSpeech, soTranscription);

  // --- Class for Transcription Results ---
  TTranscriptionResult = class
  private
    FText: string;
    FJsonObject: TJSONObject;
    FDuration: Double;
    FLanguage: string;
  public
    constructor Create(const AResponse: string; AFormat: TAiTranscriptionResponseFormat);
    destructor Destroy; override;
    property Text: string read FText;
    property Duration: Double read FDuration;
    property Language: string read FLanguage;
    property RawJson: TJSONObject read FJsonObject;
  end;

  // --- Events for Streaming ---
  TOnAudioChunkReceived = procedure(Sender: TObject; const AAudioChunk: TBytes) of object;
  TOnSpeechCompleted = procedure(Sender: TObject) of object;
  TOnTranscriptDeltaReceived = procedure(Sender: TObject; const ATextDelta: string) of object;
  TOnTranscriptionCompleted = procedure(Sender: TObject; const AFinalResult: TTranscriptionResult) of object;
  TOnAudioError = procedure(Sender: TObject; const AMessage: string) of object;

  TAiOpenAiAudio = class(TComponent)
  private
    FApiKey: string;
    FUrl: string;
    // TTS Properties
    FTTSModel: TAiTTSModel;
    FTTSVoice: TAiTTSVoice;
    FTTSResponseFormat: TAiTTSResponseFormat;
    FTTSSpeed: Double;
    FTTSInstructions: string;
    // Transcription Properties
    FTranscriptionModel: TAiTranscriptionModel;
    FTranscriptionResponseFormat: TAiTranscriptionResponseFormat;
    FTranscriptionLanguage: string;
    FTranscriptionTemperature: Double;
    FTranscriptionTimestampGranularities: TAiTimestampGranularities;
    // Streaming Events
    FOnAudioChunkReceived: TOnAudioChunkReceived;
    FOnSpeechCompleted: TOnSpeechCompleted;
    FOnTranscriptDeltaReceived: TOnTranscriptDeltaReceived;
    FOnTranscriptionCompleted: TOnTranscriptionCompleted;
    FOnAudioError: TOnAudioError;
    // Internal state for streaming
    FStreamBuffer: TStringBuilder;
    FBytesProcessed: Int64;
    FActiveResponseStream: TMemoryStream;
    FCurrentStreamOperation: TStreamOperation;

    function GetApiKey: string;
    procedure SetApiKey(const Value: string);
    procedure SetUrl(const Value: string);

  protected
    function ConvertAudioIfNeeded(aMediaFile: TAiMediaFile): Boolean;
    // Stream Handlers
    procedure HandleStreamEvent(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
    procedure ProcessSpeechStreamBuffer;
    procedure ProcessTranscriptionStreamBuffer;
    // Helper para construir la petición de transcripción
    procedure BuildTranscriptionBody(const ABody: TMultipartFormData; const AAudioFile: TAiMediaFile; const APrompt: string = '');

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // --- Text-to-Speech (TTS) ---
    function Speech(const AInput: string): TMemoryStream; overload;
    procedure Speech(const AInput: string; const AOutputStream: TStream); overload;
    procedure SpeechStreamed(const AInput: string);

    // --- Speech-to-Text (STT) ---
    function Transcribe(const AAudioFile: TAiMediaFile; const APrompt: string = ''): TTranscriptionResult;
    procedure TranscribeStreamed(const AAudioFile: TAiMediaFile);

    // --- Translation ---
    function TranslateToEnglish(const AAudioFile: TAiMediaFile; const APrompt: string = ''): TTranscriptionResult;

  published
    property ApiKey: string read GetApiKey write SetApiKey;
    property Url: string read FUrl write SetUrl;

    // --- Text-to-Speech Properties ---
    property TTSModel: TAiTTSModel read FTTSModel write FTTSModel default TAiTTSModel.tts_1;
    property TTSVoice: TAiTTSVoice read FTTSVoice write FTTSVoice default TAiTTSVoice.tvAlloy;
    property TTSResponseFormat: TAiTTSResponseFormat read FTTSResponseFormat write FTTSResponseFormat default TAiTTSResponseFormat.trfMp3;
    property TTSSpeed: Double read FTTSSpeed write FTTSSpeed;
    property TTSInstructions: string read FTTSInstructions write FTTSInstructions;

    // --- Transcription Properties ---
    property TranscriptionModel: TAiTranscriptionModel read FTranscriptionModel write FTranscriptionModel default TAiTranscriptionModel.tmWhisper1;
    property TranscriptionResponseFormat: TAiTranscriptionResponseFormat read FTranscriptionResponseFormat write FTranscriptionResponseFormat default TAiTranscriptionResponseFormat.trfJson;
    property TranscriptionLanguage: string read FTranscriptionLanguage write FTranscriptionLanguage;
    property TranscriptionTemperature: Double read FTranscriptionTemperature write FTranscriptionTemperature;
    property TranscriptionTimestampGranularities: TAiTimestampGranularities read FTranscriptionTimestampGranularities write FTranscriptionTimestampGranularities;

    // --- Streaming Events ---
    property OnAudioChunkReceived: TOnAudioChunkReceived read FOnAudioChunkReceived write FOnAudioChunkReceived;
    property OnSpeechCompleted: TOnSpeechCompleted read FOnSpeechCompleted write FOnSpeechCompleted;
    property OnTranscriptDeltaReceived: TOnTranscriptDeltaReceived read FOnTranscriptDeltaReceived write FOnTranscriptDeltaReceived;
    property OnTranscriptionCompleted: TOnTranscriptionCompleted read FOnTranscriptionCompleted write FOnTranscriptionCompleted;
    property OnAudioError: TOnAudioError read FOnAudioError write FOnAudioError;
  end;

procedure Register;

implementation

uses

{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
{$ENDIF}
  System.Math;

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

procedure RunCommand(const Command: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, nil, 'cmd.exe', PChar('/C ' + Command), nil, SW_HIDE);
{$ELSE}
  // Implementación para otras plataformas si es necesario
{$ENDIF}
end;

procedure ConvertAudioFileFormat(const ASourceStream: TStream; const ASourceFilename: string; out ADestStream: TMemoryStream; out ADestFilename: string);
var
  TempSourcePath, TempDestPath, Command: string;
  Buffer: TBytes;
begin
  ADestStream := nil;
  // Aseguramos un nombre de archivo único para evitar conflictos
  ADestFilename := ChangeFileExt(TPath.GetRandomFileName + '_' + ASourceFilename, '.mp3');
  TempSourcePath := TPath.Combine(TPath.GetTempPath, ASourceFilename);
  TempDestPath := TPath.Combine(TPath.GetTempPath, ADestFilename);

  ASourceStream.Position := 0;
  SetLength(Buffer, ASourceStream.Size);
  if ASourceStream.Size > 0 then
    ASourceStream.ReadBuffer(Buffer, Length(Buffer));
  TFile.WriteAllBytes(TempSourcePath, Buffer);

  // El -y en ffmpeg sobreescribe el archivo de destino si ya existe.
  Command := 'ffmpeg -i "' + TempSourcePath + '" -y "' + TempDestPath + '"';
  RunCommand(Command);

  if TFile.Exists(TempDestPath) then
  begin
    ADestStream := TMemoryStream.Create;
    ADestStream.LoadFromFile(TempDestPath);
    ADestStream.Position := 0;
  end;

  try
    TFile.Delete(TempSourcePath);
    TFile.Delete(TempDestPath);
  except
    // Ignorar errores al borrar archivos temporales, pueden estar bloqueados
  end;
end;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenAiAudio]);
end;

{ TTranscriptionResult }

constructor TTranscriptionResult.Create(const AResponse: string; AFormat: TAiTranscriptionResponseFormat);
begin
  inherited Create;
  if AFormat in [trfJson, trfVerboseJson, trfDiarizedJson] then
  begin
    FJsonObject := TJSONObject.ParseJSONValue(AResponse) as TJSONObject;
    FJsonObject.TryGetValue<string>('text', FText);
    FJsonObject.TryGetValue<Double>('duration', FDuration);
    FJsonObject.TryGetValue<string>('language', FLanguage);
  end
  else
  begin
    FText := AResponse;
  end;
end;

destructor TTranscriptionResult.Destroy;
begin
  FJsonObject.Free;
  inherited;
end;

{ TAiAudio }

constructor TAiOpenAiAudio.Create(AOwner: TComponent);
begin
  inherited;
  FUrl := GlOpenAIUrl;
  FApiKey := '@OPENAI_API_KEY';
  FStreamBuffer := TStringBuilder.Create;
  FTTSModel := tts_1;
  FTTSVoice := tvAlloy;
  FTTSResponseFormat := trfMp3;
  FTTSSpeed := 1.0;
  FTranscriptionModel := tmWhisper1;
  FTranscriptionResponseFormat := trfJson;
  FTranscriptionTemperature := 0.0;
  FCurrentStreamOperation := soNone;
end;

destructor TAiOpenAiAudio.Destroy;
begin
  FStreamBuffer.Free;
  inherited;
end;

procedure TAiOpenAiAudio.SetApiKey(const Value: string);
begin
  FApiKey := Value;
end;

procedure TAiOpenAiAudio.SetUrl(const Value: string);
begin
  FUrl := Value;
end;

function TAiOpenAiAudio.GetApiKey: string;
begin
  if (csDesigning in ComponentState) then
    Result := FApiKey
  else if (FApiKey <> '') and (FApiKey.StartsWith('@')) then
    Result := GetEnvironmentVariable(FApiKey.Substring(1))
  else
    Result := FApiKey;
end;

function TAiOpenAiAudio.ConvertAudioIfNeeded(aMediaFile: TAiMediaFile): Boolean;
const
  VALID_EXTS: array [0 .. 8] of string = ('.flac', '.mp3', '.mp4', '.mpeg', '.mpga', '.m4a', '.ogg', '.wav', '.webm');
var
  Ext: string;
  NewStream: TMemoryStream;
  NewFilename: string;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExt(aMediaFile.Filename));
  if not TArray.Contains<string>(VALID_EXTS, Ext) then
  begin
    ConvertAudioFileFormat(aMediaFile.Content, aMediaFile.Filename, NewStream, NewFilename);
    if Assigned(NewStream) then
    begin
      aMediaFile.LoadFromStream(NewFilename, NewStream);
      NewStream.Free;
      Result := True;
    end;
  end;
end;

procedure TAiOpenAiAudio.BuildTranscriptionBody(const ABody: TMultipartFormData; const AAudioFile: TAiMediaFile; const APrompt: string = '');
var
  ModelStr: string;
  FormatStr: string;
begin
  // --- CORRECCIÓN: Reemplazar TEnum con una sentencia 'case' ---
  case FTranscriptionModel of
    tmWhisper1:
      ModelStr := 'whisper-1';
    tmGpt4o:
      ModelStr := 'gpt-4o-transcribe';
    tmGpt4oMini:
      ModelStr := 'gpt-4o-mini-transcribe';
    tmGpt4oDiarize:
      ModelStr := 'gpt-4o-transcribe-diarize';
  else
    ModelStr := 'whisper-1'; // Default seguro
  end;
  // --- FIN DE LA CORRECCIÓN ---

  ABody.AddField('model', ModelStr);
  if APrompt <> '' then
    ABody.AddField('prompt', APrompt);
  if FTranscriptionLanguage <> '' then
    ABody.AddField('language', FTranscriptionLanguage);
  // Usamos Format en lugar de ToString para mejor control sobre el separador decimal
  if FTranscriptionTemperature <> 0.0 then
    ABody.AddField('temperature', Format('%f', [FTranscriptionTemperature]));

  // --- CORRECCIÓN: Reemplazar TEnum con una sentencia 'case' ---
  case FTranscriptionResponseFormat of
    trfJson:
      FormatStr := 'json';
    trfText:
      FormatStr := 'text';
    trfSrt:
      FormatStr := 'srt';
    trfVerboseJson:
      FormatStr := 'verbose_json';
    trfVtt:
      FormatStr := 'vtt';
    trfDiarizedJson:
      FormatStr := 'diarized_json';
  else
    FormatStr := 'json'; // Default seguro
  end;
  // --- FIN DE LA CORRECCIÓN ---

  ABody.AddField('response_format', FormatStr);

  if tsgWord in FTranscriptionTimestampGranularities then
    ABody.AddField('timestamp_granularities[]', 'word');
  if tsgSegment in FTranscriptionTimestampGranularities then
    ABody.AddField('timestamp_granularities[]', 'segment');
end;

procedure TAiOpenAiAudio.HandleStreamEvent(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  NewBytes: TBytes;
  NewDataSize: Int64;
begin
  if not Assigned(FActiveResponseStream) then
    Exit;
  NewDataSize := AReadCount - FBytesProcessed;
  if NewDataSize > 0 then
  begin
    FActiveResponseStream.Position := FBytesProcessed;
    SetLength(NewBytes, NewDataSize);
    FActiveResponseStream.ReadBuffer(Pointer(NewBytes)^, NewDataSize);

    // En lugar de comprobar el ContentType, comprobamos nuestro estado interno
    case FCurrentStreamOperation of
      soSpeech: // Estamos recibiendo un stream de audio binario para TTS
        begin
          if Assigned(FOnAudioChunkReceived) then
            TThread.Queue(nil,
              procedure
              begin
                FOnAudioChunkReceived(Self, NewBytes);
              end);
        end;
      soTranscription: // Estamos recibiendo un stream de texto SSE para Transcripción
        begin
          FStreamBuffer.Append(TEncoding.UTF8.GetString(NewBytes));
          ProcessTranscriptionStreamBuffer; // Llamamos al parser de texto
        end;
    end;

    FBytesProcessed := AReadCount;
  end;
end;

procedure TAiOpenAiAudio.ProcessSpeechStreamBuffer;
// Procesa el buffer para eventos de TTS (sse)
begin
  // TODO: Implementar lógica de parseo para speech.audio.delta y speech.audio.done
end;

procedure TAiOpenAiAudio.ProcessTranscriptionStreamBuffer;
// Procesa el buffer para eventos de Transcripción (sse)
begin
  // TODO: Implementar lógica de parseo para transcript.text.delta y transcript.text.done
end;

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
  Client: TNetHTTPClient;
  JObj: TJSONObject;
  ReqStream: TStringStream;
  Res: IHTTPResponse;
  ModelStr, VoiceStr, FormatStr: string;
  sUrl: string; // Declarar sUrl aquí
begin
  sUrl := FUrl + 'audio/speech';
  Client := TNetHTTPClient.Create(nil);
  JObj := TJSONObject.Create;
  ReqStream := TStringStream.Create('', TEncoding.UTF8);
  try
    case FTTSModel of
      tts_1:
        ModelStr := 'tts-1';
      tts_1_hd:
        ModelStr := 'tts-1-hd';
      gpt_4o_mini_tts:
        ModelStr := 'gpt-4o-mini-tts';
    else
      ModelStr := 'tts-1';
    end;

    case FTTSVoice of
      tvAlloy:
        VoiceStr := 'alloy';
      tvAsh:
        VoiceStr := 'ash';
      tvBallad:
        VoiceStr := 'ballad';
      tvCoral:
        VoiceStr := 'coral';
      tvEcho:
        VoiceStr := 'echo';
      tvFable:
        VoiceStr := 'fable';
      tvOnyx:
        VoiceStr := 'onyx';
      tvNova:
        VoiceStr := 'nova';
      tvSage:
        VoiceStr := 'sage';
      tvShimmer:
        VoiceStr := 'shimmer';
      tvVerse:
        VoiceStr := 'verse';
    else
      VoiceStr := 'alloy';
    end;

    case FTTSResponseFormat of
      trfMp3:
        FormatStr := 'mp3';
      trfOpus:
        FormatStr := 'opus';
      trfAac:
        FormatStr := 'aac';
      trfFlac:
        FormatStr := 'flac';
      trfWav:
        FormatStr := 'wav';
      trfPcm:
        FormatStr := 'pcm';
    else
      FormatStr := 'mp3';
    end;

    JObj.AddPair('input', AInput).AddPair('model', ModelStr).AddPair('voice', VoiceStr).AddPair('response_format', FormatStr);

    if FTTSSpeed <> 1.0 then
      JObj.AddPair('speed', TJSONNumber.Create(FTTSSpeed));
    if (FTTSModel = gpt_4o_mini_tts) and (FTTSInstructions <> '') then
      JObj.AddPair('instructions', FTTSInstructions);

    ReqStream.WriteString(JObj.ToJSON);
    ReqStream.Position := 0;

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, ReqStream, AOutputStream);

    if Res.StatusCode <> 200 then
    begin
      AOutputStream.Position := 0;
      var
      ErrorMsg := TStreamReader.Create(AOutputStream, TEncoding.UTF8).ReadToEnd;
      raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, ErrorMsg]);
    end;
  finally
    Client.Free;
    JObj.Free;
    ReqStream.Free;
  end;
end;

procedure TAiOpenAiAudio.SpeechStreamed(const AInput: string);
var
  Client: TNetHTTPClient;
  JObj: TJSONObject;
  ReqStream: TStringStream;
  AbortFlag: Boolean;
  sUrl: string;
  ModelStr, VoiceStr, FormatStr: string;
begin
  sUrl := FUrl + 'audio/speech';
  Client := TNetHTTPClient.Create(nil);
  JObj := TJSONObject.Create;
  ReqStream := TStringStream.Create('', TEncoding.UTF8);
  FActiveResponseStream := nil;
  FCurrentStreamOperation := soNone; // Asegurar estado limpio
  try
    FCurrentStreamOperation := soSpeech; // <-- ESTABLECER ESTADO

    case FTTSModel of
      tts_1:
        ModelStr := 'tts-1';
      tts_1_hd:
        ModelStr := 'tts-1-hd';
      gpt_4o_mini_tts:
        ModelStr := 'gpt-4o-mini-tts';
    else
      ModelStr := 'tts-1';
    end;
    case FTTSVoice of
      tvAlloy:
        VoiceStr := 'alloy';
      tvAsh:
        VoiceStr := 'ash';
      tvBallad:
        VoiceStr := 'ballad';
      tvCoral:
        VoiceStr := 'coral';
      tvEcho:
        VoiceStr := 'echo';
      tvFable:
        VoiceStr := 'fable';
      tvOnyx:
        VoiceStr := 'onyx';
      tvNova:
        VoiceStr := 'nova';
      tvSage:
        VoiceStr := 'sage';
      tvShimmer:
        VoiceStr := 'shimmer';
      tvVerse:
        VoiceStr := 'verse';
    else
      VoiceStr := 'alloy';
    end;
    case FTTSResponseFormat of
      trfMp3:
        FormatStr := 'mp3';
      trfOpus:
        FormatStr := 'opus';
      trfAac:
        FormatStr := 'aac';
      trfFlac:
        FormatStr := 'flac';
      trfWav:
        FormatStr := 'wav';
      trfPcm:
        FormatStr := 'pcm';
    else
      FormatStr := 'mp3';
    end;

    JObj.AddPair('input', AInput).AddPair('model', ModelStr).AddPair('voice', VoiceStr).AddPair('response_format', FormatStr);

    if FTTSSpeed <> 1.0 then
      JObj.AddPair('speed', TJSONNumber.Create(FTTSSpeed));
    if (FTTSModel = gpt_4o_mini_tts) and (FTTSInstructions <> '') then
      JObj.AddPair('instructions', FTTSInstructions);

    ReqStream.WriteString(JObj.ToJSON);
    ReqStream.Position := 0;

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
    Client.ContentType := 'application/json';

    FStreamBuffer.Clear;
    FBytesProcessed := 0;
    Client.OnReceiveData := HandleStreamEvent;

    FActiveResponseStream := TMemoryStream.Create;
    Client.Post(sUrl, ReqStream, FActiveResponseStream);

    if Assigned(FActiveResponseStream) and (FActiveResponseStream.Size > FBytesProcessed) then
    begin
      AbortFlag := False;
      HandleStreamEvent(Client, FActiveResponseStream.Size, FActiveResponseStream.Size, AbortFlag);
    end;

    if Assigned(FOnSpeechCompleted) then
      TThread.Queue(nil,
        procedure
        begin
          FOnSpeechCompleted(Self);
        end);

  finally
    FCurrentStreamOperation := soNone; // <-- LIMPIAR ESTADO
    Client.Free;
    JObj.Free;
    ReqStream.Free;
    if Assigned(FActiveResponseStream) then
    begin
      FActiveResponseStream.Free;
      FActiveResponseStream := nil;
    end;
  end;
end;

function TAiOpenAiAudio.Transcribe(const AAudioFile: TAiMediaFile; const APrompt: string = ''): TTranscriptionResult;
var
  Client: TNetHTTPClient;
  Body: TMultipartFormData;
  Res: IHTTPResponse;
  sUrl: string;
begin
  sUrl := FUrl + 'audio/transcriptions';
  Client := TNetHTTPClient.Create(nil);
  Body := TMultipartFormData.Create;
  try
    ConvertAudioIfNeeded(AAudioFile);

    AAudioFile.Content.Position := 0;
    Body.AddStream('file', AAudioFile.Content, AAudioFile.Filename);
    BuildTranscriptionBody(Body, AAudioFile, APrompt);

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

    Res := Client.Post(sUrl, Body);

    if Res.StatusCode = 200 then
      Result := TTranscriptionResult.Create(Res.ContentAsString, FTranscriptionResponseFormat)
    else
      raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    Client.Free;
    Body.Free;
  end;
end;

procedure TAiOpenAiAudio.TranscribeStreamed(const AAudioFile: TAiMediaFile);
var
  Client: TNetHTTPClient;
  Body: TMultipartFormData;
  AbortFlag: Boolean;
  sUrl: string;
begin
  sUrl := FUrl + 'audio/transcriptions';
  Client := TNetHTTPClient.Create(nil);
  Body := TMultipartFormData.Create;
  FActiveResponseStream := nil;
  FCurrentStreamOperation := soNone; // Asegurar estado limpio
  try
    FCurrentStreamOperation := soTranscription; // <-- ESTABLECER ESTADO

    ConvertAudioIfNeeded(AAudioFile);
    AAudioFile.Content.Position := 0;
    Body.AddStream('file', AAudioFile.Content, AAudioFile.Filename);
    BuildTranscriptionBody(Body, AAudioFile);
    Body.AddField('stream', 'true');

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
    FStreamBuffer.Clear;
    FBytesProcessed := 0;
    Client.OnReceiveData := HandleStreamEvent;

    FActiveResponseStream := TMemoryStream.Create;
    Client.Post(sUrl, Body, FActiveResponseStream);

    if Assigned(FActiveResponseStream) and (FActiveResponseStream.Size > FBytesProcessed) then
    begin
      AbortFlag := False;
      HandleStreamEvent(Client, FActiveResponseStream.Size, FActiveResponseStream.Size, AbortFlag);
    end;

  finally
    FCurrentStreamOperation := soNone; // <-- LIMPIAR ESTADO
    Client.Free;
    Body.Free;
    if Assigned(FActiveResponseStream) then
    begin
      FActiveResponseStream.Free;
      FActiveResponseStream := nil;
    end;
  end;
end;

function TAiOpenAiAudio.TranslateToEnglish(const AAudioFile: TAiMediaFile; const APrompt: string = ''): TTranscriptionResult;
var
  Client: TNetHTTPClient;
  Body: TMultipartFormData;
  Res: IHTTPResponse;
  sUrl: string;
  FormatStr: string; // Variable para almacenar el formato
begin
  sUrl := FUrl + 'audio/translations';
  Client := TNetHTTPClient.Create(nil);
  Body := TMultipartFormData.Create;
  try
    ConvertAudioIfNeeded(AAudioFile);
    AAudioFile.Content.Position := 0;
    Body.AddStream('file', AAudioFile.Content, AAudioFile.Filename);

    // El modelo es fijo para traducciones según la API
    Body.AddField('model', 'whisper-1');

    if APrompt <> '' then Body.AddField('prompt', APrompt);
    // Usamos Format para asegurar el separador decimal correcto (punto)
    if FTranscriptionTemperature <> 0.0 then Body.AddField('temperature', Format('%f', [FTranscriptionTemperature]));

    // Reemplazamos TEnum.GetName con un case statement
    case FTranscriptionResponseFormat of
      trfJson: FormatStr := 'json';
      trfText: FormatStr := 'text';
      trfSrt: FormatStr := 'srt';
      trfVerboseJson: FormatStr := 'verbose_json';
      trfVtt: FormatStr := 'vtt';
      // trfDiarizedJson no es soportado por traducciones, pero lo manejamos por si acaso
      trfDiarizedJson: FormatStr := 'json';
    else
      FormatStr := 'json'; // Default seguro
    end;

    Body.AddField('response_format', FormatStr);

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
    Res := Client.Post(sUrl, Body);

    if Res.StatusCode = 200 then
      Result := TTranscriptionResult.Create(Res.ContentAsString, FTranscriptionResponseFormat)
    else
      raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    Client.Free;
    Body.Free;
  end;
end;

end.
