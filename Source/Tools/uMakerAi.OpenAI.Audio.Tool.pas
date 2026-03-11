// IT License
//
// Copyright (c) <year> <copyright holders>
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
// TAiOpenAiSpeechTool: Wrapper de TAiOpenAiAudio que implementa IAiSpeechTool
// para ser usado como ChatTool en el orquestador de TAiChat (Run / SpeechTool).
//
// Uso:
//   AiChat.SpeechTool := TAiOpenAiSpeechTool.Create(Self);
//   (TAiOpenAiSpeechTool(AiChat.SpeechTool)).ApiKey := '@OPENAI_API_KEY';
//
// ExecuteTranscription: convierte audio en texto (STT - Fase 1 del orquestador)
// ExecuteSpeechGeneration: convierte texto en audio (TTS - Fase 3 del orquestador)
// -------------------------------------------------------------------------

unit uMakerAi.OpenAI.Audio.Tool;

interface

uses
  System.SysUtils, System.Classes, System.Threading,
  uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat.Messages,
  uMakerAi.OpenAI.Audio;

type
  { TAiOpenAiSpeechTool
    Wrapper de TAiOpenAiAudio que implementa IAiSpeechTool.
    Delega todas las llamadas a la API a un TAiOpenAiAudio interno. }
  TAiOpenAiSpeechTool = class(TAiSpeechToolBase)
  private
    FAudio: TAiOpenAiAudio;

    function GetApiKey: string;
    procedure SetApiKey(const Value: string);
    function GetUrl: string;
    procedure SetUrl(const Value: string);

    // TTS
    function GetTTSModel: TAiTTSModel;
    procedure SetTTSModel(const Value: TAiTTSModel);
    function GetTTSVoice: TAiTTSVoice;
    procedure SetTTSVoice(const Value: TAiTTSVoice);
    function GetTTSResponseFormat: TAiTTSResponseFormat;
    procedure SetTTSResponseFormat(const Value: TAiTTSResponseFormat);
    function GetTTSSpeed: Double;
    procedure SetTTSSpeed(const Value: Double);
    function GetTTSInstructions: string;
    procedure SetTTSInstructions(const Value: string);

    // STT
    function GetTranscriptionModel: TAiTranscriptionModel;
    procedure SetTranscriptionModel(const Value: TAiTranscriptionModel);
    function GetTranscriptionLanguage: string;
    procedure SetTranscriptionLanguage(const Value: string);
    function GetTranscriptionTemperature: Double;
    procedure SetTranscriptionTemperature(const Value: Double);
    function GetTranscriptionTimestampGranularities: TAiTimestampGranularities;
    procedure SetTranscriptionTimestampGranularities(const Value: TAiTimestampGranularities);

    function TTSFormatExtension: string;
  protected
    procedure ExecuteTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage); override;
    procedure ExecuteSpeechGeneration(const AText: string; ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Acceso directo al componente interno para configuracion avanzada }
    property Audio: TAiOpenAiAudio read FAudio;
  published
    property ApiKey: string read GetApiKey write SetApiKey;
    property Url: string read GetUrl write SetUrl;

    { Text-to-Speech }
    property TTSModel: TAiTTSModel read GetTTSModel write SetTTSModel default tts_1;
    property TTSVoice: TAiTTSVoice read GetTTSVoice write SetTTSVoice default tvAlloy;
    property TTSResponseFormat: TAiTTSResponseFormat read GetTTSResponseFormat write SetTTSResponseFormat default trfMp3;
    property TTSSpeed: Double read GetTTSSpeed write SetTTSSpeed;
    property TTSInstructions: string read GetTTSInstructions write SetTTSInstructions;

    { Speech-to-Text }
    property TranscriptionModel: TAiTranscriptionModel read GetTranscriptionModel write SetTranscriptionModel default tmWhisper1;
    property TranscriptionLanguage: string read GetTranscriptionLanguage write SetTranscriptionLanguage;
    property TranscriptionTemperature: Double read GetTranscriptionTemperature write SetTranscriptionTemperature;
    property TranscriptionTimestampGranularities: TAiTimestampGranularities read GetTranscriptionTimestampGranularities write SetTranscriptionTimestampGranularities;
  end;

procedure Register;

implementation

const
  TTSFormatExtensions: array[TAiTTSResponseFormat] of string = (
    'mp3', 'opus', 'aac', 'flac', 'wav', 'pcm'
  );

procedure Register;
begin
  RegisterComponents('MakerAI Tools', [TAiOpenAiSpeechTool]);
end;

{ TAiOpenAiSpeechTool }

constructor TAiOpenAiSpeechTool.Create(AOwner: TComponent);
begin
  inherited;
  FAudio := TAiOpenAiAudio.Create(nil);
end;

destructor TAiOpenAiSpeechTool.Destroy;
begin
  FAudio.Free;
  inherited;
end;

function TAiOpenAiSpeechTool.TTSFormatExtension: string;
begin
  Result := TTSFormatExtensions[FAudio.TTSResponseFormat];
end;

{ --- Propiedades delegadas --- }

function TAiOpenAiSpeechTool.GetApiKey: string;
begin
  Result := FAudio.ApiKey;
end;

procedure TAiOpenAiSpeechTool.SetApiKey(const Value: string);
begin
  FAudio.ApiKey := Value;
end;

function TAiOpenAiSpeechTool.GetUrl: string;
begin
  Result := FAudio.Url;
end;

procedure TAiOpenAiSpeechTool.SetUrl(const Value: string);
begin
  FAudio.Url := Value;
end;

function TAiOpenAiSpeechTool.GetTTSModel: TAiTTSModel;
begin
  Result := FAudio.TTSModel;
end;

procedure TAiOpenAiSpeechTool.SetTTSModel(const Value: TAiTTSModel);
begin
  FAudio.TTSModel := Value;
end;

function TAiOpenAiSpeechTool.GetTTSVoice: TAiTTSVoice;
begin
  Result := FAudio.TTSVoice;
end;

procedure TAiOpenAiSpeechTool.SetTTSVoice(const Value: TAiTTSVoice);
begin
  FAudio.TTSVoice := Value;
end;

function TAiOpenAiSpeechTool.GetTTSResponseFormat: TAiTTSResponseFormat;
begin
  Result := FAudio.TTSResponseFormat;
end;

procedure TAiOpenAiSpeechTool.SetTTSResponseFormat(const Value: TAiTTSResponseFormat);
begin
  FAudio.TTSResponseFormat := Value;
end;

function TAiOpenAiSpeechTool.GetTTSSpeed: Double;
begin
  Result := FAudio.TTSSpeed;
end;

procedure TAiOpenAiSpeechTool.SetTTSSpeed(const Value: Double);
begin
  FAudio.TTSSpeed := Value;
end;

function TAiOpenAiSpeechTool.GetTTSInstructions: string;
begin
  Result := FAudio.TTSInstructions;
end;

procedure TAiOpenAiSpeechTool.SetTTSInstructions(const Value: string);
begin
  FAudio.TTSInstructions := Value;
end;

function TAiOpenAiSpeechTool.GetTranscriptionModel: TAiTranscriptionModel;
begin
  Result := FAudio.TranscriptionModel;
end;

procedure TAiOpenAiSpeechTool.SetTranscriptionModel(const Value: TAiTranscriptionModel);
begin
  FAudio.TranscriptionModel := Value;
end;

function TAiOpenAiSpeechTool.GetTranscriptionLanguage: string;
begin
  Result := FAudio.TranscriptionLanguage;
end;

procedure TAiOpenAiSpeechTool.SetTranscriptionLanguage(const Value: string);
begin
  FAudio.TranscriptionLanguage := Value;
end;

function TAiOpenAiSpeechTool.GetTranscriptionTemperature: Double;
begin
  Result := FAudio.TranscriptionTemperature;
end;

procedure TAiOpenAiSpeechTool.SetTranscriptionTemperature(const Value: Double);
begin
  FAudio.TranscriptionTemperature := Value;
end;

function TAiOpenAiSpeechTool.GetTranscriptionTimestampGranularities: TAiTimestampGranularities;
begin
  Result := FAudio.TranscriptionTimestampGranularities;
end;

procedure TAiOpenAiSpeechTool.SetTranscriptionTimestampGranularities(const Value: TAiTimestampGranularities);
begin
  FAudio.TranscriptionTimestampGranularities := Value;
end;

{ --- Implementacion IAiSpeechTool --- }

procedure TAiOpenAiSpeechTool.ExecuteTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage);

  procedure DoTranscription;
  var
    LResult: TTranscriptionResult;
    LText: string;
  begin
    try
      ReportState(acsReasoning, 'Transcribiendo audio...');
      LResult := FAudio.Transcribe(aMediaFile);
      try
        LText := LResult.Text;
      finally
        LResult.Free;
      end;
      aMediaFile.Transcription := LText;
      aMediaFile.Procesado := True;
      ReportDataEnd(ResMsg, 'assistant', LText);
    except
      on E: Exception do
        ReportError('Error en transcripcion OpenAI: ' + E.Message, E);
    end;
  end;

begin
  if IsAsync then
    DoTranscription
  else
    TTask.Run(
      procedure
      var
        LResult: TTranscriptionResult;
        LText: string;
      begin
        try
          ReportState(acsReasoning, 'Transcribiendo audio...');
          LResult := FAudio.Transcribe(aMediaFile);
          try
            LText := LResult.Text;
          finally
            LResult.Free;
          end;
          aMediaFile.Transcription := LText;
          aMediaFile.Procesado := True;
          ReportDataEnd(ResMsg, 'assistant', LText);
        except
          on E: Exception do
            ReportError('Error en transcripcion OpenAI: ' + E.Message, E);
        end;
      end);
end;

procedure TAiOpenAiSpeechTool.ExecuteSpeechGeneration(const AText: string; ResMsg, AskMsg: TAiChatMessage);

  procedure DoSpeechGeneration;
  var
    LStream: TMemoryStream;
    LNewFile: TAiMediaFile;
  begin
    try
      ReportState(acsWriting, 'Generando voz...');
      LStream := FAudio.Speech(AText);
      try
        LNewFile := TAiMediaFile.Create;
        LNewFile.LoadFromStream('speech.' + TTSFormatExtension, LStream);
        ReportDataEnd(ResMsg, 'assistant', '[Audio generado]');
      finally
        LStream.Free;
      end;
    except
      on E: Exception do
        ReportError('Error generando voz OpenAI: ' + E.Message, E);
    end;
  end;

begin
  if IsAsync then
    DoSpeechGeneration
  else
    TTask.Run(
      procedure
      var
        LStream: TMemoryStream;
        LNewFile: TAiMediaFile;
      begin
        try
          ReportState(acsWriting, 'Generando voz...');
          LStream := FAudio.Speech(AText);
          try
            LNewFile := TAiMediaFile.Create;
            LNewFile.LoadFromStream('speech.' + TTSFormatExtension, LStream);
            ReportDataEnd(ResMsg, 'assistant', '[Audio generado]');
          finally
            LStream.Free;
          end;
        except
          on E: Exception do
            ReportError('Error generando voz OpenAI: ' + E.Message, E);
        end;
      end);
end;

end.
