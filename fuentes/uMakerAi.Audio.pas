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
// Nombre: Gustavo Enr�quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Audio;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client, uMakerAi.Core;


Type

  { TODO : Falta implementar el Streaming mode }
  TAiAudio = Class(TComponent)
  Private
    FApiKey: String;
    FUrl: String;
    FModel: String;
    FVoice: String;
    FFormat: String;
    FLanguaje: String;
    FTemperature: Single;
    FSpeed: Single;
    FResponseFormat: String;
    FQuality: String;
    Ftimestamp_granularities: String;
    procedure SetApiKey(const Value: String);
    procedure SetUrl(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetVoice(const Value: String);
    procedure SetFormat(const Value: String);
    procedure SetLanguaje(const Value: String);
    procedure SetSpeed(const Value: Single);
    procedure SetTemperature(const Value: Single);
    procedure SetResponseFormat(const Value: String);
    procedure SetQuality(const Value: String);
    procedure Settimestamp_granularities(const Value: String);
  Protected
    Function IsValidExtension(FileExtension: String): Boolean;
    procedure ConvertAudioIfNeeded(var aStream: TMemoryStream; var aFileName: String);
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function Speech(aText: String; aVoice: String = ''): TMemoryStream;
    Function Transcription(aStream: TMemoryStream; aFileName, aPrompt: String): String;
    Function Translation(aStream: TMemoryStream; aFileName, aPrompt: String): String;
  Published
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Url: String read FUrl write SetUrl;
    Property Model: String read FModel write SetModel;
    Property Voice: String read FVoice write SetVoice;
    Property Format: String read FFormat write SetFormat;
    Property Languaje: String read FLanguaje write SetLanguaje;
    Property Speed: Single read FSpeed write SetSpeed;
    Property Temperature: Single read FTemperature write SetTemperature;
    Property ResponseFormat: String read FResponseFormat write SetResponseFormat;
    Property Quality: String read FQuality write SetQuality;
    Property timestamp_granularities: String read Ftimestamp_granularities write Settimestamp_granularities;
  End;



implementation

{ TAiAudio }

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';


procedure TAiAudio.ConvertAudioIfNeeded(var aStream: TMemoryStream;
  var aFileName: String);
var
  FileNameDestino: String;
  Destino: TMemoryStream;
begin
  if not IsValidExtension(ExtractFileExt(aFileName)) then
  begin
    ConvertAudioFileFormat(aStream, aFileName, Destino, FileNameDestino);
    aStream.Clear;
    aStream.LoadFromStream(Destino);
    aStream.Position := 0;
    aFileName := FileNameDestino;
    Destino.Free;
  end;
end;

constructor TAiAudio.Create(aOwner: TComponent);
begin
  Inherited;
  FUrl := GlOpenAIUrl;
  FModel := 'whisper-1'; // whisper-1 por defecto y tts para spech nada mas tts-1, tts-1-hd
  FVoice := 'nova'; // alloy, echo, fable, onyx, nova, shimmer
  FFormat := 'mp3'; // "mp3", opus", "aac", "flac", and "pcm"
  FLanguaje := 'es';  // ISO-639-1
  FSpeed := 1;  //entre 0.25 u 5
  FTemperature := 0;  //entre 0 y 1
  FResponseFormat := 'text'; // json, text, srt, verbose_json, or vtt
  FQuality := 'tts-1'; // tts-1, tts-1-hd
  Ftimestamp_granularities :=  ''; //Empty, word, or segment obligatorio ResponseFormat = verbose_json
end;

destructor TAiAudio.Destroy;
begin

  inherited;
end;

function TAiAudio.IsValidExtension(FileExtension: String): Boolean;
begin
  FileExtension := LowerCase(Trim(StringReplace(FileExtension, '.', '', [rfReplaceAll])));
  //Result := (FileExtension = 'mp3') or (FileExtension = 'mp4') or (FileExtension = 'mpeg') or (FileExtension = 'mpga') or (FileExtension = 'm4a') or (FileExtension = 'ogg') or (FileExtension = 'wav') or (FileExtension = 'webm');
  Result := MatchText(LowerCase(Trim(FileExtension)), ['mp3', 'mp4', 'mpeg', 'mpga', 'm4a', 'ogg', 'wav', 'webm']);
end;

procedure TAiAudio.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiAudio.SetFormat(const Value: String);
begin
  FFormat := Value;
end;

procedure TAiAudio.SetLanguaje(const Value: String);
begin
  FLanguaje := Value;
end;

procedure TAiAudio.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiAudio.SetQuality(const Value: String);
begin
  FQuality := Value;
end;

procedure TAiAudio.SetResponseFormat(const Value: String);
begin
  FResponseFormat := Value;
end;

procedure TAiAudio.SetSpeed(const Value: Single);
begin
  FSpeed := Trunc(Value * 10) / 10;
end;

procedure TAiAudio.SetTemperature(const Value: Single);
begin
  FTemperature := Trunc(Value * 10) / 10;
end;

procedure TAiAudio.Settimestamp_granularities(const Value: String);
begin
  Ftimestamp_granularities := Value;
end;

procedure TAiAudio.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

procedure TAiAudio.SetVoice(const Value: String);
begin
  FVoice := Value;
end;

function TAiAudio.Speech(aText: String; aVoice: String = ''): TMemoryStream;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TMemoryStream;
  St: TStringStream;
  SelectedVoice, sUrl: String;
begin

   SelectedVoice := IfThen(aVoice = '', FVoice, aVoice);

  /// Output Format Outputs
  /// The default response format is "mp3", but other formats like "opus", "aac", "flac", and "pcm" are available.
  /// Opus: For internet streaming and communication, low latency.
  /// AAC: For digital audio compression, preferred by YouTube, Android, iOS.
  /// FLAC: For lossless audio compression, favored by audio enthusiasts for archiving.
  /// WAV: Uncompressed WAV audio, suitable for low-latency applications to avoid decoding overhead.
  /// PCM: Similar to WAV but containing the raw samples in 24kHz (16-bit signed, low-endian), without the header.

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TMemoryStream.Create;
  sUrl := FUrl + 'audio/speech';
  // https://api.openai.com/v1/audio/speech
  JObj := TJSonObject.Create;

  FSpeed := Trunc(FSpeed * 10) / 10;

  Try
    JObj.AddPair('model', FModel);

    JObj.AddPair('input', aText);
    JObj.AddPair('voice', SelectedVoice); // alloy, echo, fable, onyx, nova, and shimmer
    JObj.AddPair('response_format', FFormat);
    JObj.AddPair('speed', FSpeed);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    // St.SaveToFile('c:\temp\peticion.json.txt');
    // St.Position := 0;

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Response.Position := 0;
      Result := Response;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    // Response.Free;  //No se libera, se pasa al usuario
    JObj.Free;
  End;
end;

function TAiAudio.Transcription(aStream: TMemoryStream; aFileName, aPrompt: String): String;
Var

  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
begin


  //Valida que la extensi�n del audio sea compatible, sino utiliza ffmpeg para convertirla
  {
  var Destino: TMemoryStream;
  var FileNameDestino: String;
  If IsValidExtension(ExtractFileExt(aFileName)) = False then
  Begin
    ConvertAudioFileFormat(aStream, aFileName, Destino, FileNameDestino);
    aStream.Clear;
    aStream.LoadFromStream(Destino);
    aStream.Position := 0;
    aFileName := FileNameDestino;
    Destino.Free;
  End;
  }

  ConvertAudioIfNeeded(aStream, aFileName);

  sUrl := FUrl + 'audio/transcriptions';

  Client := THTTPClient.Create;
  Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
  Client.ContentType := 'application/json';
  Body := TMultipartFormData.Create;

  Try
    aStream.Position := 0;

    Body.AddStream('file', aStream, aFileName, GetMimeTypeFromFileName(ExtractFileExt(aFileName)));
    Body.AddField('model', FModel);
    Body.AddField('prompt', aPrompt);
    Body.AddField('response_format', FResponseFormat);
    // json, text, srt, verbose_json, or vtt
    Body.AddField('temperature', FormatFloat('0.0', FTemperature));
    Body.AddField('language', FLanguaje);

    If pos('word', Ftimestamp_granularities) > 0 then //response_format debe ser "verbose_json"
      Body.AddField('timestamp_granularities[]', 'word');

    If pos('segment', Ftimestamp_granularities) > 0 then  //response_format debe ser "verbose_json"
      Body.AddField('timestamp_granularities[]', 'segment');

    Client.Accept := 'application/text';
    Client.ContentType := 'multipart/form-data';

    Res := Client.Post(sUrl, Body, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := Res.ContentAsString
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Body.Free;
    Client.Free;
  End;
end;

function TAiAudio.Translation(aStream: TMemoryStream; aFileName, aPrompt: String): String;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
begin
{
  var FileNameDestino: string;
  var Destino: TMemoryStream;
  If IsValidExtension(ExtractFileExt(aFileName)) = False then
  Begin
    ConvertAudioFileFormat(aStream, aFileName, Destino, FileNameDestino);
    aStream.Clear;
    aStream.LoadFromStream(Destino);
    aStream.Position := 0;
    aFileName := FileNameDestino;
    Destino.Free;
  End;
  }

  ConvertAudioIfNeeded(aStream, aFileName);

  sUrl := FUrl + 'audio/translations';

  Client := THTTPClient.Create;
  Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
  Client.ContentType := 'application/json';
  Body := TMultipartFormData.Create;

  Try
    aStream.Position := 0;

    Body.AddStream('file', aStream, aFileName, GetMimeTypeFromFileName(ExtractFileExt(aFileName)));
    Body.AddField('model', FModel);
    Body.AddField('prompt', aPrompt);
    Body.AddField('response_format', FResponseFormat);
    // json, text, srt, verbose_json, or vtt
    Body.AddField('temperature', FormatFloat('0.0', Temperature));

    Client.Accept := 'application/text';
    Client.ContentType := 'multipart/form-data';

    Res := Client.Post(sUrl, Body, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := Res.ContentAsString
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Body.Free;
    Client.Free;
  End;
end;



end.
