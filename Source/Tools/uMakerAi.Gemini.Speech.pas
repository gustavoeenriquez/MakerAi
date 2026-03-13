// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// TAiGeminiSpeechTool: TTS y transcripcion via API de Gemini.
// TTS usa generateContent con responseModalities=["AUDIO"], recibe PCM
// y lo convierte a WAV con uMakerAi.Utils.PcmToWav.
// Transcripcion envia audio como inlineData a un modelo multimodal.
//
// Adaptaciones FPC:
//   - TNetHTTPClient       -> TFPHTTPClient (fphttpclient)
//   - TStringBuilder       -> concatenacion de strings
//   - Inline var           -> variables pre-declaradas
//   - .TryGetValue<T>      -> .Find + cast (fpjson)
//   - .AddPair             -> .Add  (fpjson)
//   - TTask.Run(proc)      -> TThread descendiente (caso sincrono)
//   - GetValue<string>('a.b.c') -> navegacion manual del arbol JSON
//
// TODO: Costo de generacion no disponible en la respuesta de Gemini TTS.

unit uMakerAi.Gemini.Speech;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fphttpclient,
  fpjson, jsonparser,
  uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat.Messages,
  uMakerAi.Utils.PcmToWav;

type
  TAiGeminiSpeechTool = class(TAiSpeechToolBase)
  private
    FApiKey:              string;
    FModel:               string;
    FUrl:                 string;
    FVoice:               string; // "Puck" o "Anya=Kore, Liam=Puck"
    FAudioProfile:        TStrings;
    FScene:               TStrings;
    FDirectorsNotes:      TStrings;
    FPrompt_tokens:       Integer;
    FCompletion_tokens:   Integer;
    FTotal_tokens:        Integer;
    FTranscriptionModel:  string;
    FTranscriptionPrompt: string;

    procedure SetAudioProfile(const Value: TStrings);
    procedure SetScene(const Value: TStrings);
    procedure SetDirectorsNotes(const Value: TStrings);
    function  BuildFullPrompt(const AText: string): string;
    function  GetApiKey: string;
    function  BuildSpeechConfigJson: TJSONObject;
  protected
    procedure ExecuteTranscription(aMediaFile: TAiMediaFile;
      ResMsg, AskMsg: TAiChatMessage); override;
    procedure ExecuteSpeechGeneration(const AText: string;
      ResMsg, AskMsg: TAiChatMessage); override;

    function InternalRunGeminiTTS(const AText: string;
      ResMsg: TAiChatMessage): string;
    function InternalRunGeminiTranscription(
      aMediaFile: TAiMediaFile): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Proxies publicos para hilos de background
    procedure PubReportState(AState: TAiChatState; const ADesc: string);
    procedure PubReportDataEnd(AMsg: TAiChatMessage; const ARole, AText: string);
    procedure PubReportError(const AMsg: string; E: Exception);

    class function GenerateSpeech(const AApiKey, AText: string;
      const AVoice: string;
      AAudioProfile, AScene, ADirectorsNotes: TStrings): TAiMediaFile;
  published
    property ApiKey: string read GetApiKey write FApiKey;
    property Model:  string read FModel    write FModel;
    property Url:    string read FUrl      write FUrl;

    { Soporta: "Puck" o "Anya=Kore, Liam=Puck" }
    property Voice:  string read FVoice    write FVoice;

    property AudioProfile:   TStrings read FAudioProfile   write SetAudioProfile;
    property Scene:          TStrings read FScene          write SetScene;
    property DirectorsNotes: TStrings read FDirectorsNotes write SetDirectorsNotes;

    property Prompt_tokens:     Integer read FPrompt_tokens     write FPrompt_tokens;
    property Completion_tokens: Integer read FCompletion_tokens write FCompletion_tokens;
    property Total_tokens:      Integer read FTotal_tokens      write FTotal_tokens;

    property TranscriptionModel:  string
      read FTranscriptionModel  write FTranscriptionModel;
    property TranscriptionPrompt: string
      read FTranscriptionPrompt write FTranscriptionPrompt;
  end;

procedure Register;

implementation

// ---------------------------------------------------------------------------
// Hilo de background para el caso sincrono (IsAsync = False)
// ---------------------------------------------------------------------------

type
  TGeminiSpeechMode = (gsmTTS, gsmTranscription);

  TAiGeminiSpeechThread = class(TThread)
  private
    FOwner:     TAiGeminiSpeechTool;
    FMode:      TGeminiSpeechMode;
    FMediaFile: TAiMediaFile;
    FText:      string;
    FResMsg:    TAiChatMessage;
  public
    constructor Create(AOwner: TAiGeminiSpeechTool; AMode: TGeminiSpeechMode;
      AMediaFile: TAiMediaFile; const AText: string;
      AResMsg: TAiChatMessage);
    procedure Execute; override;
  end;

constructor TAiGeminiSpeechThread.Create(AOwner: TAiGeminiSpeechTool;
  AMode: TGeminiSpeechMode; AMediaFile: TAiMediaFile; const AText: string;
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

procedure TAiGeminiSpeechThread.Execute;
var
  LText: string;
begin
  case FMode of
    gsmTTS:
      try
        FOwner.InternalRunGeminiTTS(FText, FResMsg);
      except
        on E: Exception do
          FOwner.PubReportError('Error en Gemini TTS: ' + E.Message, E);
      end;
    gsmTranscription:
      try
        FOwner.PubReportState(acsReasoning, 'Transcribiendo audio con Gemini...');
        LText := FOwner.InternalRunGeminiTranscription(FMediaFile);
        FMediaFile.Transcription := LText;
        FMediaFile.Procesado := True;
        FOwner.PubReportDataEnd(FResMsg, 'assistant', LText);
      except
        on E: Exception do
          FOwner.PubReportError('Error en transcripcion Gemini: ' + E.Message, E);
      end;
  end;
end;

// ---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGeminiSpeechTool]);
end;

{ TAiGeminiSpeechTool }

constructor TAiGeminiSpeechTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApiKey              := '@GEMINI_API_KEY';
  FUrl                 := 'https://generativelanguage.googleapis.com/v1beta/';
  FModel               := 'gemini-2.5-flash-preview-tts';
  FVoice               := 'Puck';
  FAudioProfile        := TStringList.Create;
  FScene               := TStringList.Create;
  FDirectorsNotes      := TStringList.Create;
  FTranscriptionModel  := 'gemini-2.0-flash';
  FTranscriptionPrompt := 'Transcribe this audio accurately.';
end;

destructor TAiGeminiSpeechTool.Destroy;
begin
  FAudioProfile.Free;
  FScene.Free;
  FDirectorsNotes.Free;
  inherited;
end;

function TAiGeminiSpeechTool.GetApiKey: string;
begin
  if csDesigning in ComponentState then
    Exit(FApiKey);
  if (FApiKey <> '') and (Copy(FApiKey, 1, 1) = '@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, Length(FApiKey)))
  else
    Result := FApiKey;
end;

procedure TAiGeminiSpeechTool.PubReportState(AState: TAiChatState; const ADesc: string);
begin
  ReportState(AState, ADesc);
end;

procedure TAiGeminiSpeechTool.PubReportDataEnd(AMsg: TAiChatMessage;
  const ARole, AText: string);
begin
  ReportDataEnd(AMsg, ARole, AText);
end;

procedure TAiGeminiSpeechTool.PubReportError(const AMsg: string; E: Exception);
begin
  ReportError(AMsg, E);
end;

procedure TAiGeminiSpeechTool.SetAudioProfile(const Value: TStrings);
begin
  FAudioProfile.Assign(Value);
end;

procedure TAiGeminiSpeechTool.SetScene(const Value: TStrings);
begin
  FScene.Assign(Value);
end;

procedure TAiGeminiSpeechTool.SetDirectorsNotes(const Value: TStrings);
begin
  FDirectorsNotes.Assign(Value);
end;

function TAiGeminiSpeechTool.BuildFullPrompt(const AText: string): string;
begin
  Result := '';
  if FAudioProfile.Count > 0 then
    Result := Result + '# AUDIO PROFILE:' + LineEnding + FAudioProfile.Text;
  if FScene.Count > 0 then
    Result := Result + '## THE SCENE:' + LineEnding + FScene.Text;
  if FDirectorsNotes.Count > 0 then
    Result := Result + '### DIRECTOR''S NOTES:' + LineEnding + FDirectorsNotes.Text;
  Result := Result + '#### TRANSCRIPT:' + LineEnding + AText;
end;

function TAiGeminiSpeechTool.BuildSpeechConfigJson: TJSONObject;
var
  SL:            TStringList;
  I, EqPos:      Integer;
  RawVal, SpeakerName, VoiceName: string;
  LMultiConfig:  TJSONObject;
  LSpeakerList:  TJSONArray;
  LSpeakerCfg:   TJSONObject;
  LPrebuilt:     TJSONObject;
  LVoiceConfig:  TJSONObject;
begin
  Result := nil;
  if Trim(FVoice) = '' then
    Exit;

  SL := TStringList.Create;
  try
    SL.CommaText := FVoice;
    for I := SL.Count - 1 downto 0 do
      if Trim(SL[I]) = '' then
        SL.Delete(I);

    if SL.Count > 1 then
    begin
      // Multi-speaker
      LSpeakerList := TJSONArray.Create;
      LMultiConfig := TJSONObject.Create;
      LMultiConfig.Add('speakerVoiceConfigs', LSpeakerList);

      for I := 0 to SL.Count - 1 do
      begin
        RawVal := Trim(SL[I]);
        EqPos  := Pos('=', RawVal);
        if EqPos > 0 then
        begin
          SpeakerName := Trim(Copy(RawVal, 1, EqPos - 1));
          VoiceName   := Trim(Copy(RawVal, EqPos + 1, Length(RawVal)));
        end
        else
        begin
          SpeakerName := 'Speaker' + IntToStr(I + 1);
          VoiceName   := RawVal;
        end;

        LPrebuilt   := TJSONObject.Create;
        LPrebuilt.Add('voiceName', VoiceName);
        LVoiceConfig := TJSONObject.Create;
        LVoiceConfig.Add('prebuiltVoiceConfig', LPrebuilt);

        LSpeakerCfg := TJSONObject.Create;
        LSpeakerCfg.Add('speaker', SpeakerName);
        LSpeakerCfg.Add('voiceConfig', LVoiceConfig);
        LSpeakerList.Add(LSpeakerCfg);
      end;

      Result := TJSONObject.Create;
      Result.Add('multiSpeakerVoiceConfig', LMultiConfig);
    end
    else if SL.Count = 1 then
    begin
      // Single speaker
      RawVal := Trim(SL[0]);
      EqPos  := Pos('=', RawVal);
      if EqPos > 0 then
        VoiceName := Trim(Copy(RawVal, EqPos + 1, Length(RawVal)))
      else
        VoiceName := RawVal;

      LPrebuilt    := TJSONObject.Create;
      LPrebuilt.Add('voiceName', VoiceName);
      LVoiceConfig := TJSONObject.Create;
      LVoiceConfig.Add('prebuiltVoiceConfig', LPrebuilt);

      Result := TJSONObject.Create;
      Result.Add('voiceConfig', LVoiceConfig);
    end;
  finally
    SL.Free;
  end;
end;

// ---------------------------------------------------------------------------
// TTS interno
// ---------------------------------------------------------------------------

function TAiGeminiSpeechTool.InternalRunGeminiTTS(const AText: string;
  ResMsg: TAiChatMessage): string;
var
  HTTP:         TFPHTTPClient;
  LUrl:         string;
  LRequestJson: TJSONObject;
  LGenConfig:   TJSONObject;
  LSpeechConfig: TJSONObject;
  LRespModalities: TJSONArray;
  LContents:    TJSONArray;
  LContent:     TJSONObject;
  LParts:       TJSONArray;
  LPart:        TJSONObject;
  ReqStream:    TStringStream;
  RespStream:   TStringStream;
  LResponseJson: TJSONData;
  LRespObj:     TJSONObject;
  JUsage:       TJSONData;
  JUsageObj:    TJSONObject;
  JVal:         TJSONData;
  LPt, LCt, LTt: Integer;
  LBase64:      string;
  LNewFile:     TAiMediaFile;
  LWAVStream:   TMemoryStream;
  // para navegacion JSON
  JCandidates:  TJSONData;
  JCand0:       TJSONData;
  JContentNode: TJSONData;
  JPartsNode:   TJSONData;
  JPart0:       TJSONData;
  JInlineData:  TJSONData;
  JDataField:   TJSONData;
  LMsg:         TAiChatMessage;
begin
  Result := '';
  LMsg := TAiChatMessage(ResMsg);
  if not Assigned(LMsg) then
    Exit;

  LUrl := Format('%smodels/%s:generateContent?key=%s',
    [FUrl, FModel, GetApiKey]);

  // Construir request
  LPart := TJSONObject.Create;
  LPart.Add('text', BuildFullPrompt(AText));
  LParts := TJSONArray.Create;
  LParts.Add(LPart);
  LContent := TJSONObject.Create;
  LContent.Add('parts', LParts);
  LContents := TJSONArray.Create;
  LContents.Add(LContent);

  LRespModalities := TJSONArray.Create;
  LRespModalities.Add('AUDIO');
  LGenConfig := TJSONObject.Create;
  LGenConfig.Add('responseModalities', LRespModalities);

  LSpeechConfig := BuildSpeechConfigJson;
  if Assigned(LSpeechConfig) then
    LGenConfig.Add('speechConfig', LSpeechConfig);

  LRequestJson := TJSONObject.Create;
  LRequestJson.Add('contents', LContents);
  LRequestJson.Add('generationConfig', LGenConfig);

  ReqStream  := TStringStream.Create(LRequestJson.AsJSON);
  RespStream := TStringStream.Create('');
  HTTP       := TFPHTTPClient.Create(nil);
  try
    LRequestJson.Free;
    HTTP.AddHeader('Content-Type', 'application/json');
    HTTP.RequestBody := ReqStream;
    ReportState(acsWriting, 'Gemini generando voz...');
    HTTP.HTTPMethod('POST', LUrl, RespStream, [200]);

    // Parsear respuesta
    LResponseJson := GetJSON(RespStream.DataString);
    try
      if not (Assigned(LResponseJson) and (LResponseJson.JSONType = jtObject)) then
        Exit;
      LRespObj := TJSONObject(LResponseJson);

      // Tokens
      LPt := 0; LCt := 0; LTt := 0;
      JUsage := LRespObj.Find('usageMetadata');
      if Assigned(JUsage) and (JUsage.JSONType = jtObject) then
      begin
        JUsageObj := TJSONObject(JUsage);
        JVal := JUsageObj.Find('promptTokenCount');
        if Assigned(JVal) then LPt := JVal.AsInteger;
        JVal := JUsageObj.Find('candidatesTokenCount');
        if Assigned(JVal) then LCt := JVal.AsInteger;
        JVal := JUsageObj.Find('totalTokenCount');
        if Assigned(JVal) then LTt := JVal.AsInteger;
        LMsg.Prompt_tokens     := LMsg.Prompt_tokens     + LPt;
        LMsg.Completion_tokens := LMsg.Completion_tokens + LCt;
        LMsg.Total_tokens      := LMsg.Total_tokens      + LTt;
        FPrompt_tokens     := FPrompt_tokens     + LPt;
        FCompletion_tokens := FCompletion_tokens + LCt;
        FTotal_tokens      := FTotal_tokens      + LTt;
      end;

      // Extraer audio base64: candidates[0].content.parts[0].inlineData.data
      LBase64 := '';
      JCandidates := LRespObj.Find('candidates');
      if Assigned(JCandidates) and (JCandidates.JSONType = jtArray) and
         (TJSONArray(JCandidates).Count > 0) then
      begin
        JCand0 := TJSONArray(JCandidates).Items[0];
        if Assigned(JCand0) and (JCand0.JSONType = jtObject) then
        begin
          JContentNode := TJSONObject(JCand0).Find('content');
          if Assigned(JContentNode) and (JContentNode.JSONType = jtObject) then
          begin
            JPartsNode := TJSONObject(JContentNode).Find('parts');
            if Assigned(JPartsNode) and (JPartsNode.JSONType = jtArray) and
               (TJSONArray(JPartsNode).Count > 0) then
            begin
              JPart0 := TJSONArray(JPartsNode).Items[0];
              if Assigned(JPart0) and (JPart0.JSONType = jtObject) then
              begin
                JInlineData := TJSONObject(JPart0).Find('inlineData');
                if Assigned(JInlineData) and
                   (JInlineData.JSONType = jtObject) then
                begin
                  JDataField := TJSONObject(JInlineData).Find('data');
                  if Assigned(JDataField) then
                    LBase64 := JDataField.AsString;
                end;
              end;
            end;
          end;
        end;
      end;

      if LBase64 <> '' then
      begin
        LNewFile := TAiMediaFile.Create;
        LNewFile.LoadFromBase64('temp.pcm', LBase64);

        if ConvertPCMStreamToWAVStream(LNewFile.Content, LWAVStream,
          24000, 1, 16) then
        begin
          try
            LNewFile.Clear;
            LNewFile.LoadFromStream('gemini_audio.wav', LWAVStream);
            LMsg.MediaFiles.Add(LNewFile);
            LNewFile := nil; // MediaFiles es dueno
            Result := '[Audio Generado]';
            LMsg.Prompt := Result;
            ReportDataEnd(ResMsg, 'assistant', Result);
          finally
            LWAVStream.Free;
            LNewFile.Free;
          end;
        end
        else
          LNewFile.Free;
      end;
    finally
      LResponseJson.Free;
    end;
  finally
    HTTP.Free;
    ReqStream.Free;
    RespStream.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Transcripcion interna (envia audio como inlineData)
// ---------------------------------------------------------------------------

function TAiGeminiSpeechTool.InternalRunGeminiTranscription(
  aMediaFile: TAiMediaFile): string;
var
  HTTP:        TFPHTTPClient;
  LUrl:        string;
  LRequestJson: TJSONObject;
  LContents:   TJSONArray;
  LContent:    TJSONObject;
  LParts:      TJSONArray;
  LPartText:   TJSONObject;
  LPartAudio:  TJSONObject;
  LInlineData: TJSONObject;
  ReqStream:   TStringStream;
  RespStream:  TStringStream;
  LResponseJson: TJSONData;
  LRespObj:    TJSONObject;
  LBase64, LMimeType: string;
  JCandidates: TJSONData;
  JCand0:      TJSONData;
  JContentNode: TJSONData;
  JPartsNode:  TJSONData;
  JPart0:      TJSONData;
  JTextField:  TJSONData;
begin
  Result := '';
  LUrl := Format('%smodels/%s:generateContent?key=%s',
    [FUrl, FTranscriptionModel, GetApiKey]);

  aMediaFile.Content.Position := 0;
  LBase64   := StreamToBase64(aMediaFile.Content);
  LMimeType := GetMimeTypeFromFileName(ExtractFileExt(aMediaFile.FileName));
  if LMimeType = '' then
    LMimeType := 'audio/wav';

  LPartText := TJSONObject.Create;
  LPartText.Add('text', FTranscriptionPrompt);

  LInlineData := TJSONObject.Create;
  LInlineData.Add('mimeType', LMimeType);
  LInlineData.Add('data', LBase64);
  LPartAudio := TJSONObject.Create;
  LPartAudio.Add('inlineData', LInlineData);

  LParts := TJSONArray.Create;
  LParts.Add(LPartText);
  LParts.Add(LPartAudio);
  LContent := TJSONObject.Create;
  LContent.Add('parts', LParts);
  LContents := TJSONArray.Create;
  LContents.Add(LContent);

  LRequestJson := TJSONObject.Create;
  LRequestJson.Add('contents', LContents);

  ReqStream  := TStringStream.Create(LRequestJson.AsJSON);
  RespStream := TStringStream.Create('');
  HTTP       := TFPHTTPClient.Create(nil);
  try
    LRequestJson.Free;
    HTTP.AddHeader('Content-Type', 'application/json');
    HTTP.RequestBody := ReqStream;
    HTTP.HTTPMethod('POST', LUrl, RespStream, [200]);

    LResponseJson := GetJSON(RespStream.DataString);
    try
      if not (Assigned(LResponseJson) and (LResponseJson.JSONType = jtObject)) then
        Exit;
      LRespObj := TJSONObject(LResponseJson);

      // candidates[0].content.parts[0].text
      JCandidates := LRespObj.Find('candidates');
      if Assigned(JCandidates) and (JCandidates.JSONType = jtArray) and
         (TJSONArray(JCandidates).Count > 0) then
      begin
        JCand0 := TJSONArray(JCandidates).Items[0];
        if Assigned(JCand0) and (JCand0.JSONType = jtObject) then
        begin
          JContentNode := TJSONObject(JCand0).Find('content');
          if Assigned(JContentNode) and (JContentNode.JSONType = jtObject) then
          begin
            JPartsNode := TJSONObject(JContentNode).Find('parts');
            if Assigned(JPartsNode) and (JPartsNode.JSONType = jtArray) and
               (TJSONArray(JPartsNode).Count > 0) then
            begin
              JPart0 := TJSONArray(JPartsNode).Items[0];
              if Assigned(JPart0) and (JPart0.JSONType = jtObject) then
              begin
                JTextField := TJSONObject(JPart0).Find('text');
                if Assigned(JTextField) then
                  Result := JTextField.AsString;
              end;
            end;
          end;
        end;
      end;
    finally
      LResponseJson.Free;
    end;
  finally
    HTTP.Free;
    ReqStream.Free;
    RespStream.Free;
  end;
end;

// ---------------------------------------------------------------------------
// ExecuteXxx — dispachadores segun IsAsync
// ---------------------------------------------------------------------------

procedure TAiGeminiSpeechTool.ExecuteSpeechGeneration(const AText: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  if IsAsync then
    InternalRunGeminiTTS(AText, ResMsg)
  else
    TAiGeminiSpeechThread.Create(Self, gsmTTS, nil, AText, ResMsg);
end;

procedure TAiGeminiSpeechTool.ExecuteTranscription(aMediaFile: TAiMediaFile;
  ResMsg, AskMsg: TAiChatMessage);

  procedure DoTranscription;
  var
    LText: string;
  begin
    try
      ReportState(acsReasoning, 'Transcribiendo audio con Gemini...');
      LText := InternalRunGeminiTranscription(aMediaFile);
      aMediaFile.Transcription := LText;
      aMediaFile.Procesado := True;
      ReportDataEnd(ResMsg, 'assistant', LText);
    except
      on E: Exception do
        ReportError('Error en transcripcion Gemini: ' + E.Message, E);
    end;
  end;

begin
  if IsAsync then
    DoTranscription
  else
    TAiGeminiSpeechThread.Create(Self, gsmTranscription, aMediaFile, '', ResMsg);
end;

// ---------------------------------------------------------------------------
// Class function helper
// ---------------------------------------------------------------------------

class function TAiGeminiSpeechTool.GenerateSpeech(const AApiKey, AText: string;
  const AVoice: string;
  AAudioProfile, AScene, ADirectorsNotes: TStrings): TAiMediaFile;
var
  LInstance: TAiGeminiSpeechTool;
  LDummyMsg: TAiChatMessage;
  LFile:     TAiMediaFile;
begin
  Result    := nil;
  LInstance := TAiGeminiSpeechTool.Create(nil);
  LDummyMsg := TAiChatMessage.Create('', 'assistant');
  try
    LInstance.FApiKey := AApiKey;
    LInstance.FVoice  := AVoice;
    if Assigned(AAudioProfile)   then LInstance.AudioProfile.Assign(AAudioProfile);
    if Assigned(AScene)          then LInstance.Scene.Assign(AScene);
    if Assigned(ADirectorsNotes) then LInstance.DirectorsNotes.Assign(ADirectorsNotes);

    LInstance.InternalRunGeminiTTS(AText, LDummyMsg);

    if LDummyMsg.MediaFiles.Count > 0 then
    begin
      LFile := LDummyMsg.MediaFiles[0];
      LDummyMsg.MediaFiles.Extract(LFile);
      Result := LFile;
    end;
  finally
    LDummyMsg.Free;
    LInstance.Free;
  end;
end;

end.
