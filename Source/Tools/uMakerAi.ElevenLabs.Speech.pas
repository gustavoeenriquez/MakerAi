// MIT License
//
// Copyright (c) 2024 Gustavo Enriquez
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
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

// -------------------------------------------------------------------------
// TAiElevenLabsSpeechTool: TTS y STT via ElevenLabs API.
//
// TTS:  POST https://api.elevenlabs.io/v1/text-to-speech/{voice_id}
// STT:  POST https://api.elevenlabs.io/v1/speech-to-text
//
// Uso como ChatTool:
//   AiChat.ChatTools.SpeechTool := TAiElevenLabsSpeechTool.Create(Self);
//   (AiChat.SpeechTool as TAiElevenLabsSpeechTool).ApiKey := '@ELEVENLABS_API_KEY';
//
// Uso directo:
//   LFile := TAiElevenLabsSpeechTool.GenerateSpeech(ApiKey, VoiceId, '', 'Hola mundo');
// -------------------------------------------------------------------------

unit uMakerAi.ElevenLabs.Speech;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Threading,
  System.Net.HttpClient, System.Net.HttpClientComponent,
  System.Net.URLClient, System.Net.Mime,
  uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat.Messages;

type
  // Formato de audio de salida para TTS.
  // Formatos PCM se convierten automaticamente a WAV para compatibilidad.
  TAiElevenLabsOutputFormat = (
    elofMp3_44100_128,  // mp3_44100_128 — recomendado por defecto
    elofMp3_44100_192,  // mp3_44100_192 — mayor calidad
    elofPcm_16000,      // pcm_16000 — minima latencia
    elofPcm_22050,      // pcm_22050
    elofPcm_24000,      // pcm_24000
    elofPcm_44100       // pcm_44100 — maxima calidad PCM
  );

  { TAiElevenLabsSpeechTool
    Implementa IAiSpeechTool con el motor de ElevenLabs.
    - TTS: devuelve TAiMediaFile con audio MP3 o WAV (PCM convertido)
    - STT: transcribe via Scribe (modelo dedicado de ElevenLabs) }
  TAiElevenLabsSpeechTool = class(TAiSpeechToolBase)
  private
    FApiKey: string;
    FVoiceId: string;
    FModelId: string;
    FStability: Double;
    FSimilarityBoost: Double;
    FStyle: Double;
    FUseSpeakerBoost: Boolean;
    FOutputFormat: TAiElevenLabsOutputFormat;
    FSpeechToTextModel: string;
    FPrompt_tokens: Integer;
    FCompletion_tokens: Integer;
    FTotal_tokens: Integer;

    function GetApiKey: string;
    function OutputFormatToString: string;
    function OutputFormatToExtension: string;
    function IsPcmFormat: Boolean;
    function PcmSampleRate: Integer;
  protected
    procedure ExecuteTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage); override;
    procedure ExecuteSpeechGeneration(const AText: string; ResMsg, AskMsg: TAiChatMessage); override;

    function InternalRunTTS(const AText: string; ResMsg: TAiChatMessage): string;
    function InternalRunSTT(aMediaFile: TAiMediaFile): string;
  public
    constructor Create(AOwner: TComponent); override;

    { Genera audio directamente sin necesitar un chat activo.
      Retorna un TAiMediaFile que el llamador debe liberar. }
    class function GenerateSpeech(const AApiKey, AVoiceId, AModelId, AText: string): TAiMediaFile;

    { Transcribe un archivo de audio a texto directamente sin necesitar un chat activo. }
    function Transcribe(aMediaFile: TAiMediaFile): string;
  published
    { API key de ElevenLabs. Soporta convencion @ELEVENLABS_API_KEY. }
    property ApiKey: string read GetApiKey write FApiKey;

    { ID de la voz ElevenLabs. Default: Rachel (21m00Tcm4TlvDq8ikWAM). }
    property VoiceId: string read FVoiceId write FVoiceId;

    { Modelo TTS. Default: eleven_multilingual_v2.
      Otros: eleven_flash_v2_5, eleven_turbo_v2_5, eleven_turbo_v2. }
    property ModelId: string read FModelId write FModelId;

    { Estabilidad de la voz: 0.0 (variable) .. 1.0 (estable). Default: 0.5. }
    property Stability: Double read FStability write FStability;

    { Similitud con la voz original: 0.0 .. 1.0. Default: 0.75. }
    property SimilarityBoost: Double read FSimilarityBoost write FSimilarityBoost;

    { Estilo expresivo (solo modelos v2+): 0.0 .. 1.0. Default: 0.0. }
    property Style: Double read FStyle write FStyle;

    { Boosting del hablante para mayor parecido. Default: True. }
    property UseSpeakerBoost: Boolean read FUseSpeakerBoost write FUseSpeakerBoost default True;

    { Formato de salida del audio. Default: elofMp3_44100_128. }
    property OutputFormat: TAiElevenLabsOutputFormat read FOutputFormat write FOutputFormat default elofMp3_44100_128;

    { Modelo STT para transcripcion. Default: scribe_v1. }
    property SpeechToTextModel: string read FSpeechToTextModel write FSpeechToTextModel;

    { Tokens acumulados de las llamadas a la API. }
    property Prompt_tokens: Integer read FPrompt_tokens write FPrompt_tokens;
    property Completion_tokens: Integer read FCompletion_tokens write FCompletion_tokens;
    property Total_tokens: Integer read FTotal_tokens write FTotal_tokens;
  end;

procedure Register;

implementation

uses
  uMakerAi.Utils.PcmToWav;

const
  ELEVENLABS_TTS_URL = 'https://api.elevenlabs.io/v1/text-to-speech/';
  ELEVENLABS_STT_URL = 'https://api.elevenlabs.io/v1/speech-to-text';

  OutputFormatStrings: array[TAiElevenLabsOutputFormat] of string = (
    'mp3_44100_128', 'mp3_44100_192',
    'pcm_16000', 'pcm_22050', 'pcm_24000', 'pcm_44100'
  );

  OutputFormatExtensions: array[TAiElevenLabsOutputFormat] of string = (
    'mp3', 'mp3',
    'wav', 'wav', 'wav', 'wav'
  );

  PCMSampleRates: array[TAiElevenLabsOutputFormat] of Integer = (
    0, 0, 16000, 22050, 24000, 44100
  );

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiElevenLabsSpeechTool]);
end;

{ TAiElevenLabsSpeechTool }

constructor TAiElevenLabsSpeechTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApiKey          := '@ELEVENLABS_API_KEY';
  FVoiceId         := '21m00Tcm4TlvDq8ikWAM'; // Rachel
  FModelId         := 'eleven_multilingual_v2';
  FStability       := 0.5;
  FSimilarityBoost := 0.75;
  FStyle           := 0.0;
  FUseSpeakerBoost := True;
  FOutputFormat    := elofMp3_44100_128;
  FSpeechToTextModel := 'scribe_v1';
end;

function TAiElevenLabsSpeechTool.GetApiKey: string;
begin
  if csDesigning in ComponentState then
    Exit(FApiKey);
  if FApiKey.StartsWith('@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

function TAiElevenLabsSpeechTool.OutputFormatToString: string;
begin
  Result := OutputFormatStrings[FOutputFormat];
end;

function TAiElevenLabsSpeechTool.OutputFormatToExtension: string;
begin
  Result := OutputFormatExtensions[FOutputFormat];
end;

function TAiElevenLabsSpeechTool.IsPcmFormat: Boolean;
begin
  Result := FOutputFormat in [elofPcm_16000, elofPcm_22050, elofPcm_24000, elofPcm_44100];
end;

function TAiElevenLabsSpeechTool.PcmSampleRate: Integer;
begin
  Result := PCMSampleRates[FOutputFormat];
end;

{ --- Logica principal TTS --- }

function TAiElevenLabsSpeechTool.InternalRunTTS(const AText: string; ResMsg: TAiChatMessage): string;
var
  HTTP: TNetHTTPClient;
  LUrl: string;
  LRequestJson, LVoiceSettings: TJSONObject;
  LBody: TStringStream;
  LResponse: IHTTPResponse;
  LAudioStream, LWavStream: TMemoryStream;
  LNewFile: TAiMediaFile;
  LApiHeader: TNetHeaders;
  LMsg: TAiChatMessage;
begin
  Result := '';
  LMsg := TAiChatMessage(ResMsg);
  if not Assigned(LMsg) then
    Exit;

  LUrl := Format('%s%s?output_format=%s', [ELEVENLABS_TTS_URL, FVoiceId, OutputFormatToString]);

  HTTP := TNetHTTPClient.Create(nil);
  LRequestJson := TJSONObject.Create;
  LAudioStream := TMemoryStream.Create;
  try
    LVoiceSettings := TJSONObject.Create;
    LVoiceSettings.AddPair('stability',        TJSONNumber.Create(FStability));
    LVoiceSettings.AddPair('similarity_boost', TJSONNumber.Create(FSimilarityBoost));
    LVoiceSettings.AddPair('style',            TJSONNumber.Create(FStyle));
    LVoiceSettings.AddPair('use_speaker_boost', TJSONBool.Create(FUseSpeakerBoost));

    LRequestJson.AddPair('text',           AText);
    LRequestJson.AddPair('model_id',       FModelId);
    LRequestJson.AddPair('voice_settings', LVoiceSettings);

    LBody := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    try
      HTTP.ContentType := 'application/json';
      LApiHeader := [TNameValuePair.Create('xi-api-key', GetApiKey)];

      ReportState(acsWriting, 'ElevenLabs generando voz...');
      LResponse := HTTP.Post(LUrl, LBody, LAudioStream, LApiHeader);
    finally
      LBody.Free;
    end;

    if LResponse.StatusCode = 200 then
    begin
      LNewFile := TAiMediaFile.Create;
      LAudioStream.Position := 0;

      if IsPcmFormat then
      begin
        // ElevenLabs PCM: 16-bit, mono, little-endian — convertir a WAV
        if ConvertPCMStreamToWAVStream(LAudioStream, LWavStream, PcmSampleRate, 1, 16) then
        try
          LNewFile.LoadFromStream('elevenlabs_audio.wav', LWavStream);
        finally
          LWavStream.Free;
        end
        else
          LNewFile.LoadFromStream('elevenlabs_audio.pcm', LAudioStream);
      end
      else
        LNewFile.LoadFromStream('elevenlabs_audio.' + OutputFormatToExtension, LAudioStream);

      LMsg.MediaFiles.Add(LNewFile);
      Result := '[Audio Generado]';
      LMsg.Prompt := Result;
      ReportDataEnd(ResMsg, 'assistant', Result);
    end
    else
      ReportError(Format('ElevenLabs TTS Error %d: %s',
        [LResponse.StatusCode, LResponse.ContentAsString]), nil);

  finally
    LRequestJson.Free;
    LAudioStream.Free;
    HTTP.Free;
  end;
end;

{ --- Logica principal STT --- }

function TAiElevenLabsSpeechTool.InternalRunSTT(aMediaFile: TAiMediaFile): string;
var
  HTTP: TNetHTTPClient;
  LFormData: TMultipartFormData;
  LResponse: IHTTPResponse;
  LResponseJson: TJSONObject;
  LMimeType, LFileName: string;
begin
  Result := '';

  LMimeType := GetMimeTypeFromFileName(ExtractFileExt(aMediaFile.FileName));
  if LMimeType = '' then
    LMimeType := 'audio/wav';
  LFileName := aMediaFile.FileName;
  if LFileName = '' then
    LFileName := 'audio.wav';

  HTTP := TNetHTTPClient.Create(nil);
  LFormData := TMultipartFormData.Create;
  try
    HTTP.CustomHeaders['xi-api-key'] := GetApiKey;

    aMediaFile.Content.Position := 0;
    LFormData.AddStream('file', aMediaFile.Content, LFileName, LMimeType);
    LFormData.AddField('model_id', FSpeechToTextModel);

    LResponse := HTTP.Post(ELEVENLABS_STT_URL, LFormData);

    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      if Assigned(LResponseJson) then
      try
        LResponseJson.TryGetValue<string>('text', Result);
      finally
        LResponseJson.Free;
      end;
    end
    else
      raise Exception.CreateFmt('ElevenLabs STT Error %d: %s',
        [LResponse.StatusCode, LResponse.ContentAsString]);
  finally
    LFormData.Free;
    HTTP.Free;
  end;
end;

{ --- Implementacion IAiSpeechTool --- }

procedure TAiElevenLabsSpeechTool.ExecuteTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage);
var
  LText: string;
begin
  // Llamada directa — ver comentario identico en TAiGeminiSpeechTool.
  // IsAsync=True: ya en hilo background. IsAsync=False: sync, bloquear es correcto.
  try
    ReportState(acsReasoning, 'Transcribiendo audio con ElevenLabs...');
    LText := InternalRunSTT(aMediaFile);
    aMediaFile.Transcription := LText;
    aMediaFile.Procesado := True;
    ResMsg.Prompt := LText;
    ReportDataEnd(ResMsg, 'assistant', LText);
  except
    on E: Exception do
      ReportError('Error en transcripcion ElevenLabs: ' + E.Message, E);
  end;
end;

procedure TAiElevenLabsSpeechTool.ExecuteSpeechGeneration(const AText: string; ResMsg, AskMsg: TAiChatMessage);
begin
  if IsAsync then
    InternalRunTTS(AText, ResMsg)
  else
    TThread.CreateAnonymousThread(procedure
    begin
      InternalRunTTS(AText, ResMsg);
    end).Start;
end;

{ --- Metodos publicos para uso directo sin chat --- }

function TAiElevenLabsSpeechTool.Transcribe(aMediaFile: TAiMediaFile): string;
begin
  Result := InternalRunSTT(aMediaFile);
end;



class function TAiElevenLabsSpeechTool.GenerateSpeech(const AApiKey, AVoiceId, AModelId, AText: string): TAiMediaFile;
var
  LInstance: TAiElevenLabsSpeechTool;
  LDummyMsg: TAiChatMessage;
begin
  Result := nil;
  LInstance := TAiElevenLabsSpeechTool.Create(nil);
  LDummyMsg := TAiChatMessage.Create('', 'assistant');
  try
    LInstance.FApiKey  := AApiKey;
    LInstance.FVoiceId := AVoiceId;
    if AModelId <> '' then
      LInstance.FModelId := AModelId;
    LInstance.InternalRunTTS(AText, LDummyMsg);
    if LDummyMsg.MediaFiles.Count > 0 then
    begin
      Result := LDummyMsg.MediaFiles[0];
      LDummyMsg.MediaFiles.Extract(Result);
    end;
  finally
    LDummyMsg.Free;
    LInstance.Free;
  end;
end;

end.
