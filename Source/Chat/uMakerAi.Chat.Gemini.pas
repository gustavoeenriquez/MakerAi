// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Gemini (Google) usa una API propia diferente a OpenAI:
//   - URL base: https://generativelanguage.googleapis.com/v1beta/
//   - Endpoint: models/{model}:generateContent?key={ApiKey}  (sync)
//              models/{model}:streamGenerateContent?key={ApiKey}  (async)
//   - Sin header Authorization: la ApiKey va en la query string
//   - Request: {systemInstruction, contents, tools, generationConfig}
//   - Response: {candidates, usageMetadata}
//   - Streaming: JSON array plano (sin prefijo "data:"), requiere brace-counting
//
// Adaptaciones FPC vs Delphi:
//   - TNetHTTPClient  → TFPHTTPClient (fphttpclient)
//   - Async           → TAiHttpThread sin header Authorization
//   - OnInternalReceiveData → ProcessSSELine + brace-counting parser
//   - TTask/TTask.WaitForAll → bucle secuencial
//   - TMonitor.Enter/Exit → TCriticalSection
//   - TJSONObject.ParseJSONValue(s) → TJSONObject(GetJSON(s))
//   - JObj.TryGetValue<T> → JTryGetStr/JTryGetObj/etc.
//   - JObj.AddPair → JObj.Add
//   - JObj.ToJSON → JObj.AsJSON
//   - sLineBreak → LineEnding
//   - TGuid.NewGuid.ToString → GUIDToString(CreateGUID variant)
//   - var x := ... → vars declaradas al inicio del procedimiento
//   - {$IF CompilerVersion} → eliminado
//
// API key: variable de entorno GEMINI_API_KEY (o '@GEMINI_API_KEY')
// Modelos: gemini-2.5-flash (default), gemini-2.0-flash, gemini-1.5-pro, etc.

unit uMakerAi.Chat.Gemini;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs, Generics.Collections,
  fpjson, jsonparser,
  fphttpclient, opensslsockets,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Utils.CodeExtractor,
  UMakerAi.ParamsRegistry;

const
  GlGeminiUrl       = 'https://generativelanguage.googleapis.com/v1beta/';
  GlGeminiUploadUrl = 'https://generativelanguage.googleapis.com/upload/v1beta/';

type
  // Alias para generics especializados
  TThoughtSigsDict        = specialize TObjectDictionary<TAiChatMessage, TStringList>;
  TPendingScreenshotsDict = specialize TObjectDictionary<string, TAiMediaFile>;

  TAiGeminiChat = class(TAiChat)
  private
    FThinkingBudget         : Integer;
    FIncludeThoughts        : Boolean;
    FThoughtSignatures      : TThoughtSigsDict;
    FPendingScreenshots     : TPendingScreenshotsDict;
    FPendingScreenshotsLock : TCriticalSection;

    procedure AddThoughtSignature(Msg: TAiChatMessage; const Signature: string);
    function  GetToolJSon: TJSONArray;
    function  GetSystemInstructionJson: TJSONObject;
    function  BuildSpeechConfigJson: TJSONObject;
    function  MediaTypeToResolutionString(Res: TAiMediaResolution): string;
    procedure InternalExtractCodeFiles(const AText: string;
                AMessage: TAiChatMessage);
    procedure ParseGroundingMetadata(jCandidate: TJSONObject;
                ResMsg: TAiChatMessage);
    procedure SetThinkingBudget(const Value: Integer);
    procedure SetIncludeThoughts(const Value: Boolean);

  protected
    procedure ProcessSSELine(const ALine: string); override;

    function  GetMessages: TJSONArray; override;
    function  InitChatCompletions: string; override;
    function  InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
                override;
    procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); override;
    function  ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions;
                override;

    function  InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): string;
                override;
    function  InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): string;
                override;
    function  InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage):
                string; override;
    function  InternalRunTranscription(aMediaFile: TAiMediaFile;
                ResMsg, AskMsg: TAiChatMessage): string; override;
    function  InternalRunImageDescription(aMediaFile: TAiMediaFile;
                ResMsg, AskMsg: TAiChatMessage): string; override;

  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;

    function  UploadFile(aMediaFile: TAiMediaFile): string; override;

  published
    property ThinkingBudget : Integer read FThinkingBudget  write SetThinkingBudget  default 0;
    property IncludeThoughts: Boolean read FIncludeThoughts write SetIncludeThoughts default False;
  end;

implementation

// ===========================================================================
//  TAiGeminiChat — constructor / destructor
// ===========================================================================

constructor TAiGeminiChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey      := '@GEMINI_API_KEY';
  Model       := 'gemini-2.5-flash';
  Url         := GlGeminiUrl;
  Top_p       := 0.95;
  Temperature := 1.0;

  NativeOutputFiles := [];
  ChatMediaSupports := [];

  FThinkingBudget  := 0;
  FIncludeThoughts := False;
  ThinkingLevel    := tlDefault;

  FThoughtSignatures      := TThoughtSigsDict.Create([doOwnsValues]);
  FPendingScreenshots     := TPendingScreenshotsDict.Create([doOwnsValues]);
  FPendingScreenshotsLock := TCriticalSection.Create;
end;

destructor TAiGeminiChat.Destroy;
begin
  FThoughtSignatures.Free;
  FPendingScreenshots.Free;
  FPendingScreenshotsLock.Free;
  inherited;
end;

// ===========================================================================
//  Metodos de clase
// ===========================================================================

class function TAiGeminiChat.GetDriverName: string;
begin
  Result := 'Gemini';
end;

class procedure TAiGeminiChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@GEMINI_API_KEY');
  Params.Add('Model=gemini-2.5-flash');
  Params.Add('MaxTokens=8192');
  Params.Add('URL=' + GlGeminiUrl);
end;

class function TAiGeminiChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiGeminiChat.Create(Sender);
end;

// ===========================================================================
//  Setters y helpers simples
// ===========================================================================

procedure TAiGeminiChat.SetThinkingBudget(const Value: Integer);
begin
  FThinkingBudget := Value;
end;

procedure TAiGeminiChat.SetIncludeThoughts(const Value: Boolean);
begin
  FIncludeThoughts := Value;
end;

function TAiGeminiChat.MediaTypeToResolutionString(Res: TAiMediaResolution): string;
begin
  case Res of
    mrLow:    Result := 'media_resolution_low';
    mrMedium: Result := 'media_resolution_medium';
    mrHigh:   Result := 'media_resolution_high';
  else
    Result := '';
  end;
end;

// ===========================================================================
//  AddThoughtSignature — asocia firmas de pensamiento (V3) a un mensaje
// ===========================================================================

procedure TAiGeminiChat.AddThoughtSignature(Msg: TAiChatMessage;
    const Signature: string);
var
  List: TStringList;
begin
  if Trim(Signature) = '' then
    Exit;
  if not FThoughtSignatures.TryGetValue(Msg, List) then
  begin
    List := TStringList.Create;
    FThoughtSignatures.Add(Msg, List);
  end;
  List.Add(Signature);
end;

// ===========================================================================
//  GetSystemInstructionJson — construye el objeto systemInstruction de Gemini
// ===========================================================================

function TAiGeminiChat.GetSystemInstructionJson: TJSONObject;
var
  I: Integer;
  FullSystemPrompt: string;
  Parts: TJSONArray;
  Part: TJSONObject;
begin
  Result := nil;
  FullSystemPrompt := '';
  for I := 0 to FMessages.Count - 1 do
    if SameText(FMessages[I].Role, 'system') then
      FullSystemPrompt := FullSystemPrompt + FMessages[I].Prompt + LineEnding;
  FullSystemPrompt := Trim(FullSystemPrompt);
  if FullSystemPrompt <> '' then
  begin
    Result := TJSONObject.Create;
    Parts  := TJSONArray.Create;
    Part   := TJSONObject.Create;
    Part.Add('text', FullSystemPrompt);
    Parts.Add(Part);
    Result.Add('parts', Parts);
  end;
end;

// ===========================================================================
//  BuildSpeechConfigJson — configura voces TTS (single / multi-speaker)
// ===========================================================================

function TAiGeminiChat.BuildSpeechConfigJson: TJSONObject;
var
  SL: TStringList;
  I, EqPos: Integer;
  RawVal, SpeakerName, VoiceName: string;
  LPrebuilt, LVoiceConfig, LMultiConfig, LSpeakerConfig: TJSONObject;
  LSpeakerList: TJSONArray;
begin
  Result := nil;
  if Trim(Voice) = '' then
    Exit;

  SL := TStringList.Create;
  try
    SL.CommaText := Voice;
    for I := SL.Count - 1 downto 0 do
      if Trim(SL[I]) = '' then
        SL.Delete(I);

    if SL.Count > 1 then
    begin
      // Multi-speaker
      Result       := TJSONObject.Create;
      LMultiConfig := TJSONObject.Create;
      LSpeakerList := TJSONArray.Create;

      for I := 0 to SL.Count - 1 do
      begin
        RawVal := Trim(SL[I]);
        EqPos  := Pos('=', RawVal);
        if EqPos > 0 then
        begin
          SpeakerName := Trim(Copy(RawVal, 1, EqPos - 1));
          VoiceName   := Trim(Copy(RawVal, EqPos + 1, MaxInt));
        end
        else
        begin
          SpeakerName := 'Speaker' + IntToStr(I + 1);
          VoiceName   := RawVal;
        end;

        LPrebuilt := TJSONObject.Create;
        LPrebuilt.Add('voiceName', VoiceName);

        LVoiceConfig := TJSONObject.Create;
        LVoiceConfig.Add('prebuiltVoiceConfig', LPrebuilt);

        LSpeakerConfig := TJSONObject.Create;
        LSpeakerConfig.Add('speaker', SpeakerName);
        LSpeakerConfig.Add('voiceConfig', LVoiceConfig);
        LSpeakerList.Add(LSpeakerConfig);
      end;

      LMultiConfig.Add('speakerVoiceConfigs', LSpeakerList);
      Result.Add('multiSpeakerVoiceConfig', LMultiConfig);
    end
    else if SL.Count = 1 then
    begin
      // Single speaker
      RawVal := Trim(SL[0]);
      EqPos  := Pos('=', RawVal);
      if EqPos > 0 then
        VoiceName := Trim(Copy(RawVal, EqPos + 1, MaxInt))
      else
        VoiceName := RawVal;

      Result := TJSONObject.Create;

      LPrebuilt := TJSONObject.Create;
      LPrebuilt.Add('voiceName', VoiceName);

      LVoiceConfig := TJSONObject.Create;
      LVoiceConfig.Add('prebuiltVoiceConfig', LPrebuilt);

      Result.Add('voiceConfig', LVoiceConfig);
    end;
  finally
    SL.Free;
  end;
end;

// ===========================================================================
//  GetToolJSon — convierte herramientas OpenAI al formato Gemini
//  Gemini usa: [{functionDeclarations: [{name, description, parameters}]}]
// ===========================================================================

function TAiGeminiChat.GetToolJSon: TJSONArray;
var
  sTools     : string;
  jSource    : TJSONData;
  jSourceArr : TJSONArray;
  jFuncDecls : TJSONArray;
  jToolWrapper: TJSONObject;
  jTool, jFunc: TJSONObject;
  I          : Integer;
begin
  Result := nil;
  if not Tool_Active then
    Exit;
  sTools := Trim(GetToolsStr(tfOpenAI));
  if sTools = '' then
    Exit;

  jSource := GetJSON(sTools);
  if not Assigned(jSource) then
    Exit;

  try
    jFuncDecls := TJSONArray.Create;
    try
      if jSource is TJSONArray then
        jSourceArr := TJSONArray(jSource)
      else
      begin
        jFuncDecls.Free;
        Exit;
      end;

      for I := 0 to jSourceArr.Count - 1 do
      begin
        if not (jSourceArr.Items[I] is TJSONObject) then
          Continue;
        jTool := TJSONObject(jSourceArr.Items[I]);
        jFunc := JGetObj(jTool, 'function');
        if Assigned(jFunc) then
          jFuncDecls.Add(TJSONObject(jFunc.Clone));
      end;

      if jFuncDecls.Count > 0 then
      begin
        jToolWrapper := TJSONObject.Create;
        jToolWrapper.Add('functionDeclarations', jFuncDecls);
        Result := TJSONArray.Create;
        Result.Add(jToolWrapper);
        jFuncDecls := nil; // ownership transferred
      end
      else
        FreeAndNil(jFuncDecls);
    except
      FreeAndNil(jFuncDecls);
    end;
  finally
    jSource.Free;
  end;
end;

// ===========================================================================
//  GetMessages — serializa el historial al formato "contents" de Gemini
//  Gemini usa: [{role: user|model|function, parts: [{text: ...}]}]
//  Los mensajes "system" se omiten (van en systemInstruction).
// ===========================================================================

function TAiGeminiChat.GetMessages: TJSONArray;
var
  I          : Integer;
  Msg        : TAiChatMessage;
  jEntry, jPartItem, jInlineData, jFuncResponse, jFuncResponseContent,
  jFuncCall, jWrapper, WrapperObj, ClonedPart: TJSONObject;
  jParts     : TJSONArray;
  MediaFile  : TAiMediaFile;
  MediaArr   : TAiMediaFilesArray;
  jToolCallVal: TJSONData;
  JGeminiParts: TJSONArray;
  ResArr     : TJSONArray;
  IsModelRole: Boolean;
  SignaturesList: TStringList;
  SigIndex   : Integer;
  LResolutionStr: string;
  JValArgs   : TJSONData;
  LPartSig   : string;
  jPartVal   : TJSONData;
  PartK      : Integer;
begin
  ResArr := TJSONArray.Create;
  try
    for I := 0 to FMessages.Count - 1 do
    begin
      Msg := FMessages[I];
      // Gemini no incluye "system" en contents (va en systemInstruction)
      if SameText(Msg.Role, 'system') then
        Continue;

      jEntry := TJSONObject.Create;

      // Mapeo de roles
      IsModelRole := False;
      if SameText(Msg.Role, 'assistant') or SameText(Msg.Role, 'model') then
      begin
        jEntry.Add('role', 'model');
        IsModelRole := True;
      end
      else if SameText(Msg.Role, 'tool') then
        jEntry.Add('role', 'function')
      else
        jEntry.Add('role', 'user');

      jParts := TJSONArray.Create;
      jEntry.Add('parts', jParts);

      // Firmas de pensamiento V3
      SignaturesList := nil;
      SigIndex       := 0;
      if IsModelRole then
        FThoughtSignatures.TryGetValue(Msg, SignaturesList);

      // -------------------------------------------------------------------
      // CASO 1: Tool response (user/function → model)
      // -------------------------------------------------------------------
      if SameText(Msg.Role, 'tool') then
      begin
        jFuncResponse := TJSONObject.Create;
        jFuncResponse.Add('name', Msg.FunctionName);

        JValArgs := GetJSON(Msg.Prompt);
        try
          if Assigned(JValArgs) and (JValArgs is TJSONObject) then
          begin
            jFuncResponse.Add('response', TJSONObject(JValArgs.Clone));
          end
          else
          begin
            jFuncResponseContent := TJSONObject.Create;
            jFuncResponseContent.Add('result', Msg.Prompt);
            jFuncResponse.Add('response', jFuncResponseContent);
          end;
        finally
          if Assigned(JValArgs) then
            JValArgs.Free;
        end;

        jPartItem := TJSONObject.Create;
        jPartItem.Add('functionResponse', jFuncResponse);
        jParts.Add(jPartItem);

        // Imagenes del ComputerUse adjuntas al tool response
        MediaArr := Msg.MediaFiles.GetMediaList([Tfc_Image], False);
        for MediaFile in MediaArr do
        begin
          jPartItem := TJSONObject.Create;
          if (MediaFile.UrlMedia <> '') and
             (Pos('https://generativelanguage.googleapis.com',
                  MediaFile.UrlMedia) > 0) then
          begin
            jInlineData := TJSONObject.Create;
            jInlineData.Add('mimeType', MediaFile.MimeType);
            jInlineData.Add('fileUri', MediaFile.UrlMedia);
            jPartItem.Add('fileData', jInlineData);
          end
          else
          begin
            jInlineData := TJSONObject.Create;
            jInlineData.Add('mime_type', MediaFile.MimeType);
            jInlineData.Add('data', MediaFile.Base64);
            jPartItem.Add('inline_data', jInlineData);
          end;
          jParts.Add(jPartItem);
        end;
      end

      // -------------------------------------------------------------------
      // CASO 2: Model message con tool_calls (functionCall / gemini_parts)
      // -------------------------------------------------------------------
      else if IsModelRole and (Trim(Msg.Tool_calls) <> '') then
      begin
        jToolCallVal := GetJSON(Msg.Tool_calls);
        try
          if Assigned(jToolCallVal) and (jToolCallVal is TJSONObject) then
          begin
            WrapperObj := TJSONObject(jToolCallVal);

            // Caso A: gemini_parts (múltiples funciones)
            JGeminiParts := JGetArr(WrapperObj, 'gemini_parts');
            if Assigned(JGeminiParts) then
            begin
              // Texto previo (si existe)
              if Trim(Msg.Prompt) <> '' then
              begin
                jPartItem := TJSONObject.Create;
                jPartItem.Add('text', Msg.Prompt);
                if Assigned(SignaturesList) and (SigIndex < SignaturesList.Count) then
                begin
                  jPartItem.Add('thoughtSignature', SignaturesList[SigIndex]);
                  Inc(SigIndex);
                end;
                jParts.Add(jPartItem);
              end;

              // Partes del historial
              for PartK := 0 to JGeminiParts.Count - 1 do
              begin
                jPartVal := JGeminiParts.Items[PartK];
                if not (jPartVal is TJSONObject) then
                  Continue;
                // No incluir inlineData (bytes ya guardados en el modelo)
                if TJSONObject(jPartVal).Find('inlineData') <> nil then
                  Continue;
                ClonedPart := TJSONObject(TJSONObject(jPartVal).Clone);
                if (ClonedPart.Find('thoughtSignature') = nil) and
                   Assigned(SignaturesList) and (SigIndex < SignaturesList.Count) then
                begin
                  ClonedPart.Add('thoughtSignature', SignaturesList[SigIndex]);
                  Inc(SigIndex);
                end;
                jParts.Add(ClonedPart);
              end;
            end
            else
            begin
              // Caso B: functionCall simple
              if WrapperObj.Find('functionCall') <> nil then
                jPartItem := TJSONObject(WrapperObj.Clone)
              else
              begin
                jPartItem := TJSONObject.Create;
                jFuncCall := TJSONObject(WrapperObj.Clone);
                jPartItem.Add('functionCall', jFuncCall);
              end;
              if Assigned(SignaturesList) and (SigIndex < SignaturesList.Count) then
                jPartItem.Add('thoughtSignature', SignaturesList[SigIndex]);
              jParts.Add(jPartItem);
            end;
          end;
        finally
          if Assigned(jToolCallVal) then
            jToolCallVal.Free;
        end;
      end

      // -------------------------------------------------------------------
      // CASO 3: Texto normal y/o media
      // -------------------------------------------------------------------
      else
      begin
        // Texto
        if Trim(Msg.Prompt) <> '' then
        begin
          jPartItem := TJSONObject.Create;
          jPartItem.Add('text', Msg.Prompt);
          if Assigned(SignaturesList) and (SigIndex < SignaturesList.Count) then
          begin
            jPartItem.Add('thoughtSignature', SignaturesList[SigIndex]);
            Inc(SigIndex);
          end;
          jParts.Add(jPartItem);
        end;

        // Media (solo para mensajes de usuario)
        if not IsModelRole then
        begin
          LResolutionStr := MediaTypeToResolutionString(MediaResolution);

          if Tcm_CodeInterpreter in ChatMediaSupports then
            MediaArr := Msg.MediaFiles.GetMediaList(
                [Low(TAiFileCategory)..High(TAiFileCategory)], False)
          else
            MediaArr := Msg.MediaFiles.GetMediaList(NativeInputFiles, False);

          for MediaFile in MediaArr do
          begin
            jPartItem := TJSONObject.Create;
            if (MediaFile.UrlMedia <> '') and
               (Pos('https://generativelanguage.googleapis.com',
                    MediaFile.UrlMedia) > 0) then
            begin
              jInlineData := TJSONObject.Create;
              jInlineData.Add('mimeType', MediaFile.MimeType);
              jInlineData.Add('fileUri', MediaFile.UrlMedia);
              jPartItem.Add('fileData', jInlineData);
            end
            else
            begin
              jInlineData := TJSONObject.Create;
              jInlineData.Add('mime_type', MediaFile.MimeType);
              jInlineData.Add('data', MediaFile.Base64);
              jPartItem.Add('inline_data', jInlineData);
            end;
            if LResolutionStr <> '' then
            begin
              jWrapper := TJSONObject.Create;
              jWrapper.Add('imageResolution', LResolutionStr);
              jPartItem.Add('videoMetadata', jWrapper);
            end;
            jParts.Add(jPartItem);
          end;
        end;
      end;

      if jParts.Count > 0 then
        ResArr.Add(jEntry)
      else
        jEntry.Free;
    end;

    Result := TJSONArray(ResArr.Clone);
  finally
    ResArr.Free;
  end;
end;

// ===========================================================================
//  InitChatCompletions — construye el JSON del request Gemini
// ===========================================================================

function TAiGeminiChat.InitChatCompletions: string;
var
  LRequest, JConfig, JSystemInst, LToolObj, JThinking,
  JImageConfig                : TJSONObject;
  JArrTools, JUserTools, JStop: TJSONArray;
  Lista                       : TStringList;
  I                           : Integer;
  LHasUserFunctions           : Boolean;
  LModelName                  : string;
  LBudget                     : Integer;
  sSchema                     : string;
  JSchema                     : TJSONData;
begin
  LRequest := TJSONObject.Create;
  Lista    := TStringList.Create;
  try
    // 1. System instruction
    JSystemInst := GetSystemInstructionJson;
    if Assigned(JSystemInst) then
      LRequest.Add('systemInstruction', JSystemInst);

    // 2. Contents
    LRequest.Add('contents', GetMessages);

    // 3. Tools
    JArrTools         := TJSONArray.Create;
    LHasUserFunctions := False;

    if Tool_Active then
    begin
      JUserTools := GetToolJSon;
      if Assigned(JUserTools) then
      begin
        LHasUserFunctions := True;
        for I := 0 to JUserTools.Count - 1 do
          if JUserTools.Items[I] is TJSONObject then
            JArrTools.Add(TJSONObject(JUserTools.Items[I].Clone));
        JUserTools.Free;
      end;
    end;

    // Code execution
    if (Tcm_CodeInterpreter in ChatMediaSupports) and
       not (Response_format in [tiaChatRfJson, tiaChatRfJsonSchema]) and
       not LHasUserFunctions then
    begin
      LToolObj := TJSONObject.Create;
      LToolObj.Add('codeExecution', TJSONObject.Create);
      JArrTools.Add(LToolObj);
    end;

    // Google Search
    if (tcm_WebSearch in ChatMediaSupports) and
       not (Response_format in [tiaChatRfJson, tiaChatRfJsonSchema]) and
       not LHasUserFunctions then
    begin
      LToolObj := TJSONObject.Create;
      LToolObj.Add('googleSearch', TJSONObject.Create);
      JArrTools.Add(LToolObj);
    end;

    // Computer Use
    if tcm_ComputerUse in ChatMediaSupports then
    begin
      LToolObj := TJSONObject.Create;
      LToolObj.Add('computer_use', TJSONObject.Create);
      JArrTools.Add(LToolObj);
    end;

    if JArrTools.Count > 0 then
      LRequest.Add('tools', JArrTools)
    else
      JArrTools.Free;

    // 4. Generation config
    JConfig := TJSONObject.Create;
    LRequest.Add('generationConfig', JConfig);

    if Temperature >= 0 then
      JConfig.Add('temperature', TJSONFloatNumber.Create(Temperature));
    if Top_p >= 0 then
      JConfig.Add('topP', TJSONFloatNumber.Create(Top_p));
    if Max_tokens > 0 then
      JConfig.Add('maxOutputTokens', TJSONIntegerNumber.Create(Max_tokens));

    // Stop sequences
    if Stop <> '' then
    begin
      Lista.CommaText := Stop;
      if Lista.Count > 0 then
      begin
        JStop := TJSONArray.Create;
        for I := 0 to Lista.Count - 1 do
          JStop.Add(Lista[I]);
        JConfig.Add('stopSequences', JStop);
      end;
    end;

    // Image config
    if ImageParams.Count > 0 then
    begin
      JImageConfig := TJSONObject.Create;
      if ImageParams.Values['aspectRatio'] <> '' then
        JImageConfig.Add('aspectRatio', ImageParams.Values['aspectRatio']);
      if ImageParams.Values['imageSize'] <> '' then
        JImageConfig.Add('imageSize', ImageParams.Values['imageSize']);
      if ImageParams.Values['personGeneration'] <> '' then
        JImageConfig.Add('personGeneration',
            ImageParams.Values['personGeneration']);
      if JImageConfig.Count > 0 then
        JConfig.Add('imageConfig', JImageConfig)
      else
        JImageConfig.Free;
    end;

    // Response format
    if (Response_format = tiaChatRfJsonSchema) and (JsonSchema.Count > 0) then
    begin
      JConfig.Add('responseMimeType', 'application/json');
      try
        sSchema := StringReplace(JsonSchema.Text, '\n', ' ', [rfReplaceAll]);
        JSchema := GetJSON(sSchema);
        if Assigned(JSchema) and (JSchema is TJSONObject) then
          JConfig.Add('responseJsonSchema', JSchema)
        else if Assigned(JSchema) then
          JSchema.Free;
      except
        // Ignorar schema invalido
      end;
    end
    else if Response_format = tiaChatRfJson then
      JConfig.Add('responseMimeType', 'application/json');

    // Thinking config
    if (ThinkingLevel <> tlDefault) or (FThinkingBudget > 0) then
    begin
      JThinking  := TJSONObject.Create;
      LModelName := LowerCase(Model);

      if Pos('gemini-3', LModelName) > 0 then
      begin
        // Gemini 3: thinkingLevel string
        case ThinkingLevel of
          tlLow:    JThinking.Add('thinkingLevel', 'LOW');
          tlMedium: JThinking.Add('thinkingLevel', 'MEDIUM');
          tlHigh:   JThinking.Add('thinkingLevel', 'HIGH');
        else
          JThinking.Add('thinkingLevel', 'HIGH');
        end;
      end
      else
      begin
        // Gemini 2.5 y anteriores: thinkingBudget numerico
        LBudget := 0;
        if FThinkingBudget > 0 then
          LBudget := FThinkingBudget
        else if Max_tokens > 0 then
        begin
          case ThinkingLevel of
            tlLow:    LBudget := Trunc(Max_tokens * 0.25);
            tlMedium: LBudget := Trunc(Max_tokens * 0.50);
            tlHigh:   LBudget := Trunc(Max_tokens * 0.80);
          end;
          if LBudget < 1024 then
            LBudget := 1024;
        end;
        if LBudget > 0 then
          JThinking.Add('thinkingBudget', TJSONIntegerNumber.Create(LBudget));
      end;

      JThinking.Add('includeThoughts', TJSONBoolean.Create(FIncludeThoughts));
      JConfig.Add('thinkingConfig', JThinking);
    end;

    Result := LRequest.AsJSON;
  finally
    LRequest.Free;
    Lista.Free;
  end;
end;

// ===========================================================================
//  InternalRunCompletions — POST a Gemini (override: URL propia, sin Bearer)
// ===========================================================================

function TAiGeminiChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
var
  ABody, sUrl, LModel, LMethod: string;
  BodyStream, RespStream: TStringStream;
  Client: TFPHTTPClient;
  jData: TJSONData;
  jObj : TJSONObject;
  MF   : TAiMediaFile;
begin
  Result       := '';
  FBusy        := True;
  FAbort       := False;
  FLastError   := '';
  FLastContent := '';
  if Assigned(AskMsg) then
    FLastPrompt := AskMsg.Prompt;

  // Auto-upload para documentos (Gemini no acepta Base64 para PDF, Excel, etc.)
  if Assigned(AskMsg) and Assigned(AskMsg.MediaFiles) then
  begin
    for MF in AskMsg.MediaFiles do
    begin
      if (not (MF.FileCategory in [Tfc_Image, Tfc_Audio])) and
         (MF.UrlMedia = '') then
      begin
        try
          UploadFile(MF);
        except
          on E: Exception do
            DoError('Error Auto-Uploading: ' + E.Message, E);
        end;
      end;
    end;
  end;

  if Assigned(FTmpToolCallBuffer) then
    FTmpToolCallBuffer.Clear;

  LModel  := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if Asynchronous then
    LMethod := 'streamGenerateContent'
  else
    LMethod := 'generateContent';

  sUrl := Url;
  if (Length(sUrl) > 0) and (sUrl[Length(sUrl)] <> '/') then
    sUrl := sUrl + '/';
  sUrl := sUrl + 'models/' + LModel + ':' + LMethod + '?key=' + ApiKey;

  ABody := InitChatCompletions;

  DoStateChange(acsConnecting, sUrl);

  if Asynchronous then
  begin
    // Modo async: TAiHttpThread SIN header Authorization (clave va en URL)
    StartHttpThread(sUrl, ABody, ['Content-Type', 'application/json']);
    // El resultado llega por ProcessSSELine
    Exit;
  end;

  // Modo sync
  BodyStream := TStringStream.Create(ABody);
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Content-Type', 'application/json');
    if ResponseTimeOut > 0 then
      Client.IOTimeout := ResponseTimeOut;
    try
      Client.RequestBody := BodyStream;
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
    except
      on E: Exception do
      begin
        FBusy      := False;
        FLastError := E.Message;
        DoStateChange(acsError, E.Message);
        DoError(E.Message, E);
        Exit;
      end;
    end;

    if Client.ResponseStatusCode = 200 then
    begin
      jData := GetJSON(RespStream.DataString);
      if not Assigned(jData) or not (jData is TJSONObject) then
      begin
        FreeAndNil(jData);
        FBusy := False;
        DoError('Respuesta JSON invalida de Gemini', nil);
        Exit;
      end;
      jObj := TJSONObject(jData);
      try
        FBusy := False;
        ParseChat(jObj, ResMsg);
        Result := FLastContent;
      finally
        FreeAndNil(jObj);
      end;
    end
    else
    begin
      FBusy := False;
      DoStateChange(acsError, RespStream.DataString);
      DoError(Format('Error Received: %d, %s',
          [Client.ResponseStatusCode, RespStream.DataString]), nil);
    end;
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

// ===========================================================================
//  ParseGroundingMetadata — parsea metadatos de grounding (Google Search)
// ===========================================================================

procedure TAiGeminiChat.ParseGroundingMetadata(jCandidate: TJSONObject;
    ResMsg: TAiChatMessage);
var
  jGroundingMeta, jChunk, jWeb: TJSONObject;
  jChunksArray, jSupportsArr   : TJSONArray;
  jSupportVal, jIdxVal         : TJSONData;
  ChunkItem                    : TAiWebSearchItem;
  I, ChunkIdx, ActualIdx       : Integer;
  WebTitle, WebUri             : string;
  jSupport, jSegment           : TJSONObject;
  jChunkIndices                : TJSONArray;
  LCitation                    : TAiMsgCitation;
  LSource                      : TAiCitationSource;
begin
  if not Assigned(ResMsg.WebSearchResponse) then
    ResMsg.WebSearchResponse := TAiWebSearch.Create;
  ResMsg.WebSearchResponse.annotations.Clear;

  jGroundingMeta := JGetObj(jCandidate, 'groundingMetadata');
  if not Assigned(jGroundingMeta) then
    Exit;

  jChunksArray := JGetArr(jGroundingMeta, 'groundingChunks');
  if Assigned(jChunksArray) then
  begin
    for I := 0 to jChunksArray.Count - 1 do
    begin
      if not (jChunksArray.Items[I] is TJSONObject) then
        Continue;
      jChunk := TJSONObject(jChunksArray.Items[I]);
      jWeb   := JGetObj(jChunk, 'web');
      if Assigned(jWeb) then
      begin
        ChunkItem       := TAiWebSearchItem.Create;
        WebTitle        := JGetStr(jWeb, 'title');
        WebUri          := JGetStr(jWeb, 'uri');
        ChunkItem.title := WebTitle;
        ChunkItem.Url   := WebUri;
        ChunkItem.&type := 'web_page';
        ResMsg.WebSearchResponse.annotations.Add(ChunkItem);
      end;
    end;
  end;

  jSupportsArr := JGetArr(jGroundingMeta, 'groundingSupports');
  if not Assigned(jSupportsArr) then
    Exit;

  for I := 0 to jSupportsArr.Count - 1 do
  begin
    jSupportVal := jSupportsArr.Items[I];
    if not (jSupportVal is TJSONObject) then
      Continue;
    jSupport  := TJSONObject(jSupportVal);
    LCitation := TAiMsgCitation.Create;

    jSegment := JGetObj(jSupport, 'segment');
    if Assigned(jSegment) then
    begin
      LCitation.StartIndex := JGetInt(jSegment, 'startIndex');
      LCitation.EndIndex   := JGetInt(jSegment, 'endIndex');
      LCitation.Text       := JGetStr(jSegment, 'text');
    end;

    jChunkIndices := JGetArr(jSupport, 'groundingChunkIndices');
    if Assigned(jChunkIndices) then
    begin
      for ChunkIdx := 0 to jChunkIndices.Count - 1 do
      begin
        jIdxVal   := jChunkIndices.Items[ChunkIdx];
        ActualIdx := jIdxVal.AsInteger;
        LSource   := TAiCitationSource.Create;
        LSource.SourceType := cstWeb;
        if (ActualIdx >= 0) and
           (ActualIdx < ResMsg.WebSearchResponse.annotations.Count) then
        begin
          LSource.DataSource.Title :=
              ResMsg.WebSearchResponse.annotations[ActualIdx].title;
          LSource.DataSource.Url   :=
              ResMsg.WebSearchResponse.annotations[ActualIdx].Url;
        end;
        LCitation.Sources.Add(LSource);
      end;
    end;

    ResMsg.Citations.Add(LCitation);
  end;
end;

// ===========================================================================
//  ParseChat — parsea la respuesta JSON de Gemini (formato candidates)
// ===========================================================================

procedure TAiGeminiChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
var
  LCandidates            : TJSONArray;
  LContent, LUso         : TJSONObject;
  LParts                 : TJSONArray;
  LRespuesta, LRole      : string;
  sText, LPartSig        : string;
  LLang, LCode, LCodeOutput: string;
  LMimeType, LBase64Data : string;
  LFileUri               : string;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens,
  aThoughts_tokens       : Integer;
  AskMsg                 : TAiChatMessage;
  LFunciones             : TAiToolsFunctions;
  ToolCall               : TAiToolsFunction;
  ToolMsg                : TAiChatMessage;
  Clave                  : string;
  I                      : Integer;
  LCandidate             : TJSONObject;
  jValPart               : TJSONData;
  LPartObj               : TJSONObject;
  LExecCodeObj, LCodeResultObj, LInlineData: TJSONObject;
  LNewMediaFile          : TAiMediaFile;
  ModelVersion           : string;
  jPromptFeedback        : TJSONObject;
  JToolsArr              : TJSONArray;
  jCall, jWrapper        : TJSONObject;
  jArgs                  : TJSONData;
  SrcList                : TStringList;
  Sig                    : string;
  MsgForTools            : TAiChatMessage;
begin
  LRespuesta := '';
  LRole      := 'model';
  AskMsg     := GetLastMessage;

  // 1. Validar candidatos
  LCandidates := JGetArr(jObj, 'candidates');
  if not Assigned(LCandidates) or (LCandidates.Count = 0) then
  begin
    jPromptFeedback := JGetObj(jObj, 'promptFeedback');
    if Assigned(jPromptFeedback) then
      DoError('Bloqueado por seguridad: ' + jPromptFeedback.AsJSON, nil)
    else
      DoError('Gemini: sin candidatos en la respuesta.', nil);
    FBusy := False;
    Exit;
  end;

  // 2. Metadatos
  ModelVersion := JGetStr(jObj, 'modelVersion');

  // 3. Tokens
  aPrompt_tokens     := 0;
  aCompletion_tokens := 0;
  aTotal_tokens      := 0;
  aThoughts_tokens   := 0;

  LUso := JGetObj(jObj, 'usageMetadata');
  if Assigned(LUso) then
  begin
    JTryGetInt(LUso, 'promptTokenCount',      aPrompt_tokens);
    JTryGetInt(LUso, 'candidatesTokenCount',  aCompletion_tokens);
    JTryGetInt(LUso, 'totalTokenCount',       aTotal_tokens);
    JTryGetInt(LUso, 'thoughtsTokenCount',    aThoughts_tokens);
  end;

  LCandidate := TJSONObject(LCandidates.Items[0]);

  // 4. Grounding
  ParseGroundingMetadata(LCandidate, ResMsg);

  // 5. Partes del contenido
  LContent := JGetObj(LCandidate, 'content');
  if Assigned(LContent) then
  begin
    LRole  := JGetStr(LContent, 'role', 'model');
    LParts := JGetArr(LContent, 'parts');
    if Assigned(LParts) then
    begin
      for I := 0 to LParts.Count - 1 do
      begin
        jValPart := LParts.Items[I];
        if not (jValPart is TJSONObject) then
          Continue;
        LPartObj := TJSONObject(jValPart);

        // Thought signature V3
        if JTryGetStr(LPartObj, 'thoughtSignature', LPartSig) then
          AddThoughtSignature(ResMsg, LPartSig);

        // A. Texto normal
        if JTryGetStr(LPartObj, 'text', sText) then
          LRespuesta := Trim(LRespuesta + sText);

        // B. Code execution: codigo generado
        LExecCodeObj := JGetObj(LPartObj, 'executableCode');
        if Assigned(LExecCodeObj) then
        begin
          LLang := JGetStr(LExecCodeObj, 'language');
          LCode := JGetStr(LExecCodeObj, 'code');
          if LCode <> '' then
            LRespuesta := LRespuesta + LineEnding +
                '```' + LowerCase(LLang) + LineEnding + Trim(LCode) +
                LineEnding + '```';
        end;

        // C. Code execution: resultado
        LCodeResultObj := JGetObj(LPartObj, 'codeExecutionResult');
        if Assigned(LCodeResultObj) then
        begin
          LCodeOutput := JGetStr(LCodeResultObj, 'output');
          if LCodeOutput <> '' then
            LRespuesta := LRespuesta + LineEnding + '> **Output:**' +
                LineEnding + '```' + LineEnding + Trim(LCodeOutput) +
                LineEnding + '```';
        end;

        // D. Inline data (imagenes/audio generados)
        LInlineData := JGetObj(LPartObj, 'inlineData');
        if Assigned(LInlineData) then
        begin
          LMimeType   := JGetStr(LInlineData, 'mimeType');
          LBase64Data := JGetStr(LInlineData, 'data');
          if LBase64Data <> '' then
          begin
            LNewMediaFile := TAiMediaFile.Create;
            try
              LNewMediaFile.LoadFromBase64(
                  'generated' + GetFileExtensionFromMimeType(LMimeType),
                  LBase64Data);
              ResMsg.MediaFiles.Add(LNewMediaFile);
            except
              LNewMediaFile.Free;
            end;
          end;
        end;

        // E. File data (URL de archivo generado)
        LInlineData := JGetObj(LPartObj, 'fileData');
        if Assigned(LInlineData) then
        begin
          LMimeType := JGetStr(LInlineData, 'mimeType');
          LFileUri  := JGetStr(LInlineData, 'fileUri');
          if LFileUri <> '' then
          begin
            LNewMediaFile := TAiMediaFile.Create;
            try
              LNewMediaFile.UrlMedia  := LFileUri;
              LNewMediaFile.FileName  :=
                  'generated' + GetFileExtensionFromMimeType(LMimeType);
              ResMsg.MediaFiles.Add(LNewMediaFile);
            except
              LNewMediaFile.Free;
            end;
          end;
        end;
      end;
    end;
  end;

  LRespuesta       := Trim(LRespuesta);
  Self.FLastContent := LRespuesta;

  // Acumular tokens globales
  Self.Prompt_tokens     := Self.Prompt_tokens     + aPrompt_tokens;
  Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
  Self.Total_tokens      := Self.Total_tokens      + aTotal_tokens;
  Self.Thinking_tokens   := Self.Thinking_tokens   + aThoughts_tokens;

  // Extraer codigo si esta configurado
  if tfc_ExtracttextFile in NativeOutputFiles then
    InternalExtractCodeFiles(LRespuesta, ResMsg);

  // Tool calls?
  LFunciones := ExtractToolCallFromJson(LCandidates);

  // --- CASO 1: Sin herramientas (respuesta final) ---
  if not Assigned(LFunciones) or (LFunciones.Count = 0) then
  begin
    if Assigned(LFunciones) then
      FreeAndNil(LFunciones);
    FBusy := False;

    ResMsg.Role              := LRole;
    ResMsg.Tool_calls        := '';
    ResMsg.Model             := ModelVersion;
    ResMsg.Prompt            := LRespuesta;
    ResMsg.Prompt_tokens     := aPrompt_tokens;
    ResMsg.Completion_tokens := aCompletion_tokens;
    ResMsg.Total_tokens      := aTotal_tokens;
    ResMsg.Thinking_tokens   := aThoughts_tokens;

    DoProcessResponse(AskMsg, ResMsg, LRespuesta);
    DoStateChange(acsFinished, 'Done');
    DoDataEnd(ResMsg, LRole, LRespuesta, jObj);
    Exit;
  end;

  // --- CASO 2: Con herramientas ---

  // Construir JSON de tool_calls para el historial
  MsgForTools := TAiChatMessage.Create(LRespuesta, LRole);

  JToolsArr := TJSONArray.Create;
  for Clave in LFunciones.Keys do
  begin
    ToolCall := LFunciones[Clave];
    jCall    := TJSONObject.Create;
    jCall.Add('name', ToolCall.Name);

    jArgs := GetJSON(ToolCall.Arguments);
    if not Assigned(jArgs) then
      jArgs := TJSONObject.Create;
    jCall.Add('args', jArgs);

    jWrapper := TJSONObject.Create;
    jWrapper.Add('functionCall', jCall);
    JToolsArr.Add(jWrapper);
  end;

  if JToolsArr.Count = 1 then
  begin
    MsgForTools.Tool_calls := JToolsArr.Items[0].AsJSON;
    JToolsArr.Free;
  end
  else
  begin
    jWrapper := TJSONObject.Create;
    jWrapper.Add('gemini_parts', JToolsArr);
    MsgForTools.Tool_calls := jWrapper.AsJSON;
    jWrapper.Free;
  end;

  MsgForTools.Id              := FMessages.Count + 1;
  MsgForTools.Prompt_tokens   := aPrompt_tokens;
  MsgForTools.Completion_tokens := aCompletion_tokens;
  MsgForTools.Total_tokens    := aTotal_tokens;
  MsgForTools.Thinking_tokens := aThoughts_tokens;

  // Transferir firmas de ResMsg → MsgForTools
  if FThoughtSignatures.TryGetValue(ResMsg, SrcList) then
  begin
    for Sig in SrcList do
      AddThoughtSignature(MsgForTools, Sig);
    FThoughtSignatures.Remove(ResMsg);
  end;

  FMessages.Add(MsgForTools);

  // Ejecutar herramientas secuencialmente
  try
    for Clave in LFunciones.Keys do
    begin
      ToolCall        := LFunciones[Clave];
      ToolCall.ResMsg := ResMsg;
      ToolCall.AskMsg := AskMsg;
      DoCallFunction(ToolCall);
    end;

    // Agregar respuestas de herramientas al historial
    for Clave in LFunciones.Keys do
    begin
      ToolCall := LFunciones[Clave];
      ToolMsg  := TAiChatMessage.Create(ToolCall.Response, 'tool',
                                         ToolCall.Id, ToolCall.Name);
      ToolMsg.Id := FMessages.Count + 1;
      FMessages.Add(ToolMsg);
    end;

    // Llamada recursiva para obtener respuesta final
    Self.Run(nil, ResMsg);

  finally
    LFunciones.Free;
  end;
end;

// ===========================================================================
//  ExtractToolCallFromJson — extrae functionCalls de candidates Gemini
// ===========================================================================

function TAiGeminiChat.ExtractToolCallFromJson(
    jChoices: TJSONArray): TAiToolsFunctions;
var
  jCandidateVal : TJSONData;
  jContent      : TJSONObject;
  jParts        : TJSONArray;
  jPartVal      : TJSONData;
  jFunctionCall, LArgsObject: TJSONObject;
  LFunction     : TAiToolsFunction;
  I, ArgIdx     : Integer;
  G             : TGUID;
begin
  Result := nil;
  if not Assigned(jChoices) or (jChoices.Count = 0) then
    Exit;

  jCandidateVal := jChoices.Items[0];
  if not (jCandidateVal is TJSONObject) then
    Exit;

  jContent := JGetObj(TJSONObject(jCandidateVal), 'content');
  if not Assigned(jContent) then
    Exit;
  jParts := JGetArr(jContent, 'parts');
  if not Assigned(jParts) then
    Exit;

  Result := TAiToolsFunctions.Create;

  for I := 0 to jParts.Count - 1 do
  begin
    jPartVal := jParts.Items[I];
    if not (jPartVal is TJSONObject) then
      Continue;

    jFunctionCall := JGetObj(TJSONObject(jPartVal), 'functionCall');
    if not Assigned(jFunctionCall) then
      Continue;

    LFunction      := TAiToolsFunction.Create;
    LFunction.Name := JGetStr(jFunctionCall, 'name');

    LArgsObject := JGetObj(jFunctionCall, 'args');
    if Assigned(LArgsObject) then
    begin
      LFunction.Arguments := LArgsObject.AsJSON;
      for ArgIdx := 0 to LArgsObject.Count - 1 do
        LFunction.Params.Values[LArgsObject.Names[ArgIdx]] :=
            LArgsObject.Items[ArgIdx].AsString;
    end
    else
      LFunction.Arguments := '{}';

    CreateGUID(G);
    LFunction.Id   := 'call_' + GUIDToString(G);
    LFunction.Tipo := 'function';
    Result.Add(LFunction.Id, LFunction);
  end;

  if Result.Count = 0 then
    FreeAndNil(Result);
end;

// ===========================================================================
//  InternalExtractCodeFiles — extrae bloques de codigo Markdown a MediaFiles
// ===========================================================================

procedure TAiGeminiChat.InternalExtractCodeFiles(const AText: string;
    AMessage: TAiChatMessage);
var
  Code      : TMarkdownCodeExtractor;
  CodeFiles : TCodeFileList;
  CodeFile  : TCodeFile;
  MF        : TAiMediaFile;
  St        : TStringStream;
  FileName  : string;
begin
  if not (tfc_ExtracttextFile in NativeOutputFiles) then
    Exit;
  if Trim(AText) = '' then
    Exit;

  Code := TMarkdownCodeExtractor.Create;
  try
    CodeFiles := Code.ExtractCodeFiles(AText);
    for CodeFile in CodeFiles do
    begin
      St := TStringStream.Create(CodeFile.Code);
      try
        St.Position := 0;
        MF := TAiMediaFile.Create;

        if CodeFile.FileName <> '' then
          FileName := CodeFile.FileName
        else
          FileName := 'snippet_' + IntToStr(AMessage.MediaFiles.Count + 1) +
                      '.' + CodeFile.FileType;

        MF.LoadFromStream(FileName, St);
        AMessage.MediaFiles.Add(MF);
      finally
        St.Free;
      end;
    end;
  finally
    Code.Free;
  end;
end;

// ===========================================================================
//  ProcessSSELine — parser de streaming Gemini (brace-counting)
//  Gemini envia un JSON array plano, sin prefijo "data:".
//  TAiSSEStream.Write ya recorta las lineas por #10 y llama aqui.
//  Acumulamos en FTmpResponseText y usamos brace-counting para encontrar
//  objetos JSON completos.
// ===========================================================================

procedure TAiGeminiChat.ProcessSSELine(const ALine: string);
var
  S, JsonStr           : string;
  P_End, Level, I      : Integer;
  jObj, LContent, LUso : TJSONObject;
  LCandidates, LParts  : TJSONArray;
  jValPart             : TJSONData;
  LPartObj             : TJSONObject;
  sText, LPartSig      : string;
  sFinishReason        : string;
  LModelVersion        : string;
  IsStreamFinished     : Boolean;
  IsThought            : Boolean;
  InString, Escape     : Boolean;
  JThoughtVal          : TJSONData;
  LUsoTmp              : TJSONObject;

  // Finalization vars
  ResMsg           : TAiChatMessage;
  JArrParts        : TJSONArray;
  Wrapper          : TJSONObject;
  Keys             : specialize TList<Integer>;
  BufKey           : Integer;
  PartObj          : TJSONObject;
  jInlineData, jFuncCall, jArgs: TJSONObject;
  LNewMediaFile    : TAiMediaFile;
  LBase64Data, LMimeType, LSig: string;
  LFileUri         : string;
  HasTools         : Boolean;
  LFunciones       : TAiToolsFunctions;
  LFunction        : TAiToolsFunction;
  LName, LArgsStr  : string;
  Clave            : string;
  ArgIdx, tmpInt   : Integer;
  G                : TGUID;
  ToolMsg          : TAiChatMessage;
  ToolCall         : TAiToolsFunction;
  SaveAsync        : Boolean;
  LCandidate       : TJSONObject;
begin
  if FAbort then
    Exit;

  // Acumular la linea en el buffer (sin prefijo "data:")
  S := ALine;
  if (Length(S) >= 5) and (Copy(S, 1, 5) = 'data:') then
    S := Trim(Copy(S, 6, MaxInt));

  if S = '' then
    Exit;

  FTmpResponseText := FTmpResponseText + S;

  // Bucle de brace-counting
  while True do
  begin
    // Limpiar basura inicial: espacios, comas, corchetes
    while (FTmpResponseText <> '') and (FTmpResponseText[1] <= ' ') do
      Delete(FTmpResponseText, 1, 1);
    while (FTmpResponseText <> '') and
          ((FTmpResponseText[1] = ',') or (FTmpResponseText[1] = '[') or
           (FTmpResponseText[1] = ']')) do
      Delete(FTmpResponseText, 1, 1);

    if (FTmpResponseText = '') or (FTmpResponseText[1] <> '{') then
      Break;

    // Buscar el final del objeto JSON con parser inteligente
    Level    := 0;
    P_End    := 0;
    InString := False;
    Escape   := False;

    for I := 1 to Length(FTmpResponseText) do
    begin
      if (FTmpResponseText[I] = '"') and (not Escape) then
        InString := not InString;

      if InString then
      begin
        if FTmpResponseText[I] = '\' then
          Escape := not Escape
        else
          Escape := False;
      end
      else
      begin
        Escape := False;
        if FTmpResponseText[I] = '{' then
          Inc(Level)
        else if FTmpResponseText[I] = '}' then
        begin
          Dec(Level);
          if Level = 0 then
          begin
            P_End := I;
            Break;
          end;
        end;
      end;
    end;

    if P_End = 0 then
      Break; // JSON incompleto, esperar mas datos

    JsonStr := Copy(FTmpResponseText, 1, P_End);

    jObj := TJSONObject(GetJSON(JsonStr));
    if not Assigned(jObj) then
    begin
      DoError('Gemini: JSON Parse Error en chunk asincrono', nil);
      Delete(FTmpResponseText, 1, P_End);
      Continue;
    end;

    // Parseo exitoso: consumir del buffer
    Delete(FTmpResponseText, 1, P_End);

    try
      LModelVersion    := JGetStr(jObj, 'modelVersion');
      IsStreamFinished := False;

      // Procesar candidatos
      LCandidates := JGetArr(jObj, 'candidates');
      if Assigned(LCandidates) and (LCandidates.Count > 0) and
         (LCandidates.Items[0] is TJSONObject) then
      begin
        LCandidate := TJSONObject(LCandidates.Items[0]);

        // Detectar finishReason
        if JTryGetStr(LCandidate, 'finishReason', sFinishReason) then
          if (sFinishReason <> '') and (sFinishReason <> 'null') then
            IsStreamFinished := True;

        // Procesar partes
        LContent := JGetObj(LCandidate, 'content');
        if Assigned(LContent) then
        begin
          LParts := JGetArr(LContent, 'parts');
          if Assigned(LParts) then
          begin
            for I := 0 to LParts.Count - 1 do
            begin
              jValPart := LParts.Items[I];
              if not (jValPart is TJSONObject) then
                Continue;
              LPartObj := TJSONObject(jValPart);

              // Buffer de partes estructurales (tools, code, firmas, archivos)
              if (LPartObj.Find('functionCall') <> nil) or
                 (LPartObj.Find('executableCode') <> nil) or
                 (LPartObj.Find('codeExecutionResult') <> nil) or
                 (LPartObj.Find('inlineData') <> nil) or
                 (LPartObj.Find('fileData') <> nil) or
                 JTryGetStr(LPartObj, 'thoughtSignature', LPartSig) then
              begin
                FTmpToolCallBuffer.Add(FTmpToolCallBuffer.Count,
                    TJSONObject(LPartObj.Clone));
              end;

              // Texto / thinking
              IsThought   := False;
              JThoughtVal := LPartObj.Find('thought');
              if Assigned(JThoughtVal) and (JThoughtVal is TJSONBoolean) then
                IsThought := TJSONBoolean(JThoughtVal).AsBoolean;

              if JTryGetStr(LPartObj, 'text', sText) then
              begin
                if IsThought then
                  DoThinking(nil, 'model', sText, nil)
                else if sText <> '' then
                begin
                  FLastContent := FLastContent + sText;
                  DoData(nil, 'model', sText, nil);
                end;
              end;
            end;
          end;
        end;
      end;

      // Actualizar tokens
      LUsoTmp := JGetObj(jObj, 'usageMetadata');
      if Assigned(LUsoTmp) then
      begin
        if JTryGetInt(LUsoTmp, 'promptTokenCount',     tmpInt) then Prompt_tokens     := tmpInt;
        if JTryGetInt(LUsoTmp, 'candidatesTokenCount', tmpInt) then Completion_tokens := tmpInt;
        if JTryGetInt(LUsoTmp, 'totalTokenCount',      tmpInt) then Total_tokens      := tmpInt;
        if JTryGetInt(LUsoTmp, 'thoughtsTokenCount',   tmpInt) then Thinking_tokens   := tmpInt;
      end;

      // -----------------------------------------------------------------------
      // Finalizacion cuando llega finishReason
      // -----------------------------------------------------------------------
      if IsStreamFinished then
      begin
        FBusy  := False;
        FAbort := True; // Ignorar lineas posteriores

        // Crear mensaje de respuesta final
        ResMsg := TAiChatMessage.Create(FLastContent, 'model');
        ResMsg.Prompt_tokens     := Self.Prompt_tokens;
        ResMsg.Completion_tokens := Self.Completion_tokens;
        ResMsg.Total_tokens      := Self.Total_tokens;
        ResMsg.Thinking_tokens   := Self.Thinking_tokens;
        if LModelVersion <> '' then
          ResMsg.Model := LModelVersion;

        HasTools   := False;
        LFunciones := nil;

        // Procesar buffer de partes estructurales
        if FTmpToolCallBuffer.Count > 0 then
        begin
          JArrParts := TJSONArray.Create;
          Keys := specialize TList<Integer>.Create;
          try
            for BufKey in FTmpToolCallBuffer.Keys do
              Keys.Add(BufKey);
            Keys.Sort;

            LFunciones := TAiToolsFunctions.Create;

            for I := 0 to Keys.Count - 1 do
            begin
              BufKey  := Keys[I];
              PartObj := FTmpToolCallBuffer[BufKey];

              // Thought signature
              if JTryGetStr(PartObj, 'thoughtSignature', LSig) then
                AddThoughtSignature(ResMsg, LSig);

              // Function call
              jFuncCall := JGetObj(PartObj, 'functionCall');
              if Assigned(jFuncCall) then
              begin
                HasTools  := True;
                LFunction := TAiToolsFunction.Create;
                LName     := JGetStr(jFuncCall, 'name');
                LFunction.Name := LName;

                jArgs := JGetObj(jFuncCall, 'args');
                if Assigned(jArgs) then
                begin
                  LArgsStr := jArgs.AsJSON;
                  LFunction.Arguments := LArgsStr;
                  for ArgIdx := 0 to jArgs.Count - 1 do
                    LFunction.Params.Values[jArgs.Names[ArgIdx]] :=
                        jArgs.Items[ArgIdx].AsString;
                end
                else
                  LFunction.Arguments := '{}';

                CreateGUID(G);
                LFunction.Id   := 'call_' + GUIDToString(G);
                LFunction.Tipo := 'function';
                LFunciones.Add(LFunction.Id, LFunction);
              end;

              // Inline data (archivos generados)
              jInlineData := JGetObj(PartObj, 'inlineData');
              if Assigned(jInlineData) then
              begin
                LMimeType   := JGetStr(jInlineData, 'mimeType');
                LBase64Data := JGetStr(jInlineData, 'data');
                if LBase64Data <> '' then
                begin
                  LNewMediaFile := TAiMediaFile.Create;
                  try
                    LNewMediaFile.LoadFromBase64(
                        'generated' + GetFileExtensionFromMimeType(LMimeType),
                        LBase64Data);
                    ResMsg.MediaFiles.Add(LNewMediaFile);
                  except
                    LNewMediaFile.Free;
                  end;
                end;
              end;

              // File data (URL de archivo)
              jInlineData := JGetObj(PartObj, 'fileData');
              if Assigned(jInlineData) then
              begin
                LMimeType := JGetStr(jInlineData, 'mimeType');
                LFileUri  := JGetStr(jInlineData, 'fileUri');
                if LFileUri <> '' then
                begin
                  LNewMediaFile := TAiMediaFile.Create;
                  try
                    LNewMediaFile.UrlMedia := LFileUri;
                    LNewMediaFile.FileName :=
                        'generated' + GetFileExtensionFromMimeType(LMimeType);
                    ResMsg.MediaFiles.Add(LNewMediaFile);
                  except
                    LNewMediaFile.Free;
                  end;
                end;
              end;

              JArrParts.Add(TJSONObject(PartObj.Clone));
            end;

            // Empaquetar como gemini_parts
            Wrapper := TJSONObject.Create;
            try
              Wrapper.Add('gemini_parts', JArrParts);
              ResMsg.Tool_calls := Wrapper.AsJSON;
              JArrParts := nil; // ownership transferred
            finally
              Wrapper.Free;
            end;

            FTmpToolCallBuffer.Clear;
          finally
            Keys.Free;
            if Assigned(JArrParts) then
              JArrParts.Free;
          end;
        end;

        // Branch: herramientas vs texto final
        if HasTools and Assigned(LFunciones) and (LFunciones.Count > 0) then
        begin
          ResMsg.Id := FMessages.Count + 1;
          FMessages.Add(ResMsg);

          // Ejecucion secuencial de herramientas
          for Clave in LFunciones.Keys do
          begin
            ToolCall        := LFunciones[Clave];
            ToolCall.ResMsg := ResMsg;
            DoCallFunction(ToolCall);
          end;

          // Agregar respuestas al historial
          for Clave in LFunciones.Keys do
          begin
            ToolCall := LFunciones[Clave];
            ToolMsg  := TAiChatMessage.Create(ToolCall.Response, 'tool',
                                               ToolCall.Id, ToolCall.Name);
            ToolMsg.Id := FMessages.Count + 1;
            FMessages.Add(ToolMsg);
          end;
          LFunciones.Free;

          // Llamada recursiva forzando modo sincrono (evita deadlock de thread)
          SaveAsync     := Asynchronous;
          Asynchronous := False;
          FBusy         := True;
          FAbort        := False;
          try
            Self.Run(nil, ResMsg);
          finally
            Asynchronous := SaveAsync;
          end;
        end
        else
        begin
          if Assigned(LFunciones) then
            LFunciones.Free;

          ResMsg.Id := FMessages.Count + 1;
          FMessages.Add(ResMsg);

          InternalExtractCodeFiles(FLastContent, ResMsg);
          DoStateChange(acsFinished, 'Done');
          DoDataEnd(ResMsg, 'model', FLastContent, nil);
        end;
      end; // IsStreamFinished

    finally
      jObj.Free;
    end;
  end; // while True
end;

// ===========================================================================
//  Stubs de InternalRun* (Phase 1)
// ===========================================================================

function TAiGeminiChat.InternalRunImageGeneration(ResMsg,
    AskMsg: TAiChatMessage): string;
begin
  // Gemini usa generateContent para generacion de imagen tambien
  Result := InternalRunCompletions(ResMsg, AskMsg);
end;

function TAiGeminiChat.InternalRunSpeechGeneration(ResMsg,
    AskMsg: TAiChatMessage): string;
begin
  Result := '';
end;

function TAiGeminiChat.InternalRunImageVideoGeneration(ResMsg,
    AskMsg: TAiChatMessage): string;
begin
  Result := '';
end;

function TAiGeminiChat.InternalRunTranscription(aMediaFile: TAiMediaFile;
    ResMsg, AskMsg: TAiChatMessage): string;
begin
  Result := '';
end;

function TAiGeminiChat.InternalRunImageDescription(aMediaFile: TAiMediaFile;
    ResMsg, AskMsg: TAiChatMessage): string;
begin
  Result := '';
end;

// ===========================================================================
//  UploadFile — sube un archivo a la Gemini File API (resumable upload)
// ===========================================================================

function TAiGeminiChat.UploadFile(aMediaFile: TAiMediaFile): string;
var
  Client       : TFPHTTPClient;
  StartUrl, UploadUrl, FileUri, CloudState: string;
  RespStream, BodyStream: TStringStream;
  jBody, jResp, jFileObj, jFile2: TJSONObject;
  NumBytes     : Int64;
begin
  Result  := '';
  Client  := TFPHTTPClient.Create(nil);
  try
    StartUrl := GlGeminiUploadUrl + 'files?key=' + ApiKey;

    // Paso 1: iniciar upload resumable
    NumBytes   := aMediaFile.Content.Size;
    if NumBytes = 0 then
    begin
      DoError('UploadFile: archivo vacio', nil);
      Exit;
    end;

    jBody    := TJSONObject.Create;
    jFileObj := TJSONObject.Create;
    jFileObj.Add('display_name', aMediaFile.FileName);
    jBody.Add('file', jFileObj);

    BodyStream := TStringStream.Create(jBody.AsJSON);
    RespStream := TStringStream.Create('');
    Client.AddHeader('Content-Type', 'application/json');
    Client.AddHeader('X-Goog-Upload-Protocol', 'resumable');
    Client.AddHeader('X-Goog-Upload-Command', 'start');
    Client.AddHeader('X-Goog-Upload-Header-Content-Length', IntToStr(NumBytes));
    Client.AddHeader('X-Goog-Upload-Header-Content-Type', aMediaFile.MimeType);
    try
      Client.RequestBody := BodyStream;
      Client.HTTPMethod('POST', StartUrl, RespStream, [200]);
      UploadUrl := Client.ResponseHeaders.Values['X-Goog-Upload-URL'];
      if UploadUrl = '' then
      begin
        DoError('UploadFile: no se recibio X-Goog-Upload-URL', nil);
        Exit;
      end;
    finally
      BodyStream.Free;
      RespStream.Free;
      jBody.Free;
    end;

    // Paso 2: subir el contenido binario
    Client.RequestHeaders.Clear;
    Client.RequestBody := nil;
    Client.AddHeader('Content-Type', aMediaFile.MimeType);
    Client.AddHeader('X-Goog-Upload-Command', 'upload, finalize');
    Client.AddHeader('X-Goog-Upload-Offset', '0');

    aMediaFile.Content.Position := 0;
    RespStream := TStringStream.Create('');
    try
      Client.RequestBody := aMediaFile.Content;
      Client.HTTPMethod('POST', UploadUrl, RespStream, [200]);
      Client.RequestBody := nil; // No liberar: Content es propiedad de aMediaFile

      jResp := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jResp) then
      begin
        jFile2  := JGetObj(jResp, 'file');
        if Assigned(jFile2) then
        begin
          FileUri              := JGetStr(jFile2, 'uri');
          CloudState           := JGetStr(jFile2, 'state');
          aMediaFile.UrlMedia  := FileUri;
          aMediaFile.CloudName := JGetStr(jFile2, 'name');
        end;
        jResp.Free;
      end;
      Result := FileUri;
    finally
      RespStream.Free;
    end;
  finally
    Client.Free;
  end;
end;

// ===========================================================================
//  initialization — registra el driver en el factory
// ===========================================================================

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiGeminiChat);

end.
