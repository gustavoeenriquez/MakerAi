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

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/
//
// Actualización: Integración API Gemini 3 (Thinking Level, Signatures, Media Resolution)

unit uMakerAi.Chat.Gemini;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.NetConsts,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Tools.Functions, uMakerAi.Core,
  uMakerAi.Utils.PcmToWav, uMakerAi.Utils.CodeExtractor, uMakerAi.Embeddings,
  uMakerAi.Embeddings.Core;

type

  TAiMediaFileGemini = Class(TAiMediaFile)
  Protected
    Procedure DownloadFileFromUrl(Url: String); Override;
  end;

  TAiGeminiChat = Class(TAiChat)
  Private
    FThinkingBudget: Integer;
    FIncludeThoughts: Boolean;
    FMediaResolution: TAiMediaResolution;
    // Diccionario para almacenar las firmas de pensamiento asociadas a los mensajes del modelo
    FThoughtSignatures: TObjectDictionary<TAiChatMessage, TStringList>;

    // Nuevo helper para agregar firmas fácilmente
    procedure AddThoughtSignature(Msg: TAiChatMessage; const Signature: string);

    Function GetToolJSon: TJSonArray;
    function GetSystemInstructionJson: TJSONObject;
    procedure SetThinkingBudget(const Value: Integer);
    procedure SetIncludeThoughts(const Value: Boolean);
    // [V3 UPDATE] Setters

    // Helpers internos
    function MediaTypeToResolutionString(Res: TAiMediaResolution): string;
    // function ThinkingLevelToString(Level: TAiThinkingLevel): string;
    procedure InternalCompleteRequest;
    procedure SetMediaResolution(const Value: TAiMediaResolution);

  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;

    function InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;

    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;

    function InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;

    Function InternalAddMessage(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = ''): TAiChatMessage; Override;
    Function InternalAddMessage(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage; Override;
    function InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage; Override;

    procedure InternalExtractCodeFiles(const AText: string; AMessage: TAiChatMessage);

    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); Override;
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    function BuildSpeechConfigJson: TJSONObject;
    procedure ParseGroundingMetadata(jCandidate: TJSONObject; ResMsg: TAiChatMessage);
    procedure OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse); Override;

  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;

    Function GetMessages: TJSonArray; Override;
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Function UploadFile(aMediaFile: TAiMediaFile): String; Override;
    Function DownLoadFile(aMediaFile: TAiMediaFile): String; Override;
    Function DeleteFile(aMediaFile: TAiMediaFile): String; Override;
    Function CheckFileState(aMediaFile: TAiMediaFile): String; Override;
    Function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer = 3600): String; Override;
    function RetrieveCache(const CacheName: string): TJSONObject;
    function ListCaches: TJSONObject;
    function DeleteCache(const CacheName: string): Boolean;

    function CreateBatchJob(const SourceFileUri: string; const OutputUri: string = ''): string;
    function GetBatchJob(const JobName: string): string;

    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

  Published

    // [V3 UPDATE] Propiedades actualizadas
    // Gemini 3 prefiere ThinkingLevel. ThinkingBudget es legacy/alternativo.
    Property ThinkingBudget: Integer read FThinkingBudget write SetThinkingBudget default 0;
    Property IncludeThoughts: Boolean read FIncludeThoughts write SetIncludeThoughts default False;
    Property MediaResolution: TAiMediaResolution read FMediaResolution write SetMediaResolution;
  End;

  TAiGeminiEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;
    Procedure ParseEmbedding(jObj: TJSONObject); Override;
  end;

procedure Register;

implementation

Const
  GlAIUrl = 'https://generativelanguage.googleapis.com/v1beta/';
  GlAIUploadUrl = 'https://generativelanguage.googleapis.com/upload/v1beta/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGeminiChat, TAiGeminiEmbeddings]);
end;

{ TAiGeminiChat }

class function TAiGeminiChat.GetDriverName: string;
Begin
  Result := 'Gemini';
End;

class procedure TAiGeminiChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@GEMINI_API_KEY');
  // [V3 UPDATE] Modelo recomendado por defecto actualizado
  Params.Add('Model=gemini-2.0-flash');
  Params.Add('MaxTokens=8192');
  Params.Add('URL=' + GlAIUrl);
End;

// [V3 UPDATE] Helpers para conversión de enums
function TAiGeminiChat.MediaTypeToResolutionString(Res: TAiMediaResolution): string;
begin
  case Res of
    mrLow:
      Result := 'media_resolution_low';
    mrMedium:
      Result := 'media_resolution_medium';
    mrHigh:
      Result := 'media_resolution_high';
  else
    Result := '';
  end;
end;

{ function TAiGeminiChat.ThinkingLevelToString(Level: TAiThinkingLevel): string;
  begin
  case Level of
  tlLow:
  Result := 'LOW';
  tlHigh:
  Result := 'HIGH'; // Gemini solo soporta high y low
  else
  Result := ''; // tlDefault y tlMedium no se envían
  end;
  end;
}

// Métodos de Cache existentes (RetrieveCache, DeleteCache, ListCaches)...

function TAiGeminiChat.RetrieveCache(const CacheName: string): TJSONObject;
var
  LHttpClient: TNetHTTPClient;
  LUrl: string;
  LResponse: IHTTPResponse;
begin
  LHttpClient := TNetHTTPClient.Create(Nil);
  try
    LUrl := GlAIUrl + CacheName + '?key=' + Self.ApiKey;
    LResponse := LHttpClient.Get(LUrl);
    if LResponse.StatusCode = 200 then
      Result := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject
    else
      raise Exception.CreateFmt('Error RetrieveCache: %d %s', [LResponse.StatusCode, LResponse.ContentAsString]);
  finally
    LHttpClient.Free;
  end;
end;

function TAiGeminiChat.DeleteCache(const CacheName: string): Boolean;
var
  LHttpClient: TNetHTTPClient;
  LUrl: string;
  LResponse: IHTTPResponse;
begin
  LHttpClient := TNetHTTPClient.Create(Nil);
  try
    LUrl := GlAIUrl + CacheName + '?key=' + Self.ApiKey;
    LResponse := LHttpClient.Delete(LUrl);
    Result := (LResponse.StatusCode = 200) or (LResponse.StatusCode = 204);
  finally
    LHttpClient.Free;
  end;
end;

function TAiGeminiChat.ListCaches: TJSONObject;
var
  LHttpClient: TNetHTTPClient;
  LUrl: string;
  LResponse: IHTTPResponse;
begin
  Result := nil;
  LHttpClient := TNetHTTPClient.Create(Nil);
  try
    LUrl := GlAIUrl + 'cachedContents?key=' + Self.ApiKey;
    LResponse := LHttpClient.Get(LUrl);
    if LResponse.StatusCode = 200 then
      Result := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
  finally
    LHttpClient.Free;
  end;
end;

class function TAiGeminiChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiGeminiChat.Create(Sender);
End;

constructor TAiGeminiChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@GEMINI_API_KEY';
  Model := 'gemini-2.0-flash';
  Url := GlAIUrl;

  // [V3 UPDATE] Gemini 3 recomienda Temperature 1.0 por defecto para razonamiento
  Top_p := 0.95;
  Temperature := 1.0;

  NativeOutputFiles := [];
  NativeOutputFiles := [];
  ChatMediaSupports := [];

  {
    VideoParams.AddPair('aspectRatio', '16:9');
    VideoParams.AddPair('personGeneration', 'allow_all');

    ImageParams.AddPair('aspectRatio', '1:1');
  }

  FThinkingBudget := 0;
  ThinkingLevel := tlDefault;
  FIncludeThoughts := False;

  FThoughtSignatures := TObjectDictionary<TAiChatMessage, TStringList>.Create([doOwnsValues]);
end;

destructor TAiGeminiChat.Destroy;
begin
  FThoughtSignatures.Free;
  inherited;
end;

// Métodos Batch (CreateBatchJob, GetBatchJob)
function TAiGeminiChat.CreateBatchJob(const SourceFileUri: string; const OutputUri: string = ''): string;
var
  LHttpClient: TNetHTTPClient;
  LUrl, LModel: string;
  LRequest, LInput, LOutput, LGcsDest: TJSONObject;
  LResponse: IHTTPResponse;
  LStream: TStringStream;
begin
  Result := '';
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  LUrl := GlAIUrl + 'jobs?key=' + Self.ApiKey;
  LHttpClient := TNetHTTPClient.Create(Nil);
  LRequest := TJSONObject.Create;
  try
    LRequest.AddPair('displayName', 'Batch Job from Delphi');
    LRequest.AddPair('model', 'models/' + LModel);
    LInput := TJSONObject.Create;
    LInput.AddPair('gcsSource', TJSONObject.Create.AddPair('uris', TJSonArray.Create.Add(SourceFileUri)));
    LRequest.AddPair('inputConfig', LInput);
    LOutput := TJSONObject.Create;
    LGcsDest := TJSONObject.Create;
    LGcsDest.AddPair('outputUriPrefix', OutputUri);
    LOutput.AddPair('gcsDestination', LGcsDest);
    LRequest.AddPair('outputConfig', LOutput);
    LStream := TStringStream.Create(LRequest.ToJSON, TEncoding.UTF8);
    try
      LHttpClient.ContentType := 'application/json';
      LResponse := LHttpClient.Post(LUrl, LStream);
      if LResponse.StatusCode = 200 then
      begin
        var
        JResp := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
        try
          JResp.TryGetValue<string>('name', Result);
        finally
          JResp.Free;
        end;
      end;
    finally
      LStream.Free;
    end;
  finally
    LRequest.Free;
    LHttpClient.Free;
  end;
end;

function TAiGeminiChat.GetBatchJob(const JobName: string): string;
var
  LHttpClient: TNetHTTPClient;
  LUrl, LState: string;
  LResponse: IHTTPResponse;
begin
  Result := 'UNKNOWN';
  LHttpClient := TNetHTTPClient.Create(Nil);
  try
    LUrl := GlAIUrl + JobName + '?key=' + Self.ApiKey;
    LResponse := LHttpClient.Get(LUrl);
    if LResponse.StatusCode = 200 then
    begin
      var
      JResp := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      try
        if JResp.TryGetValue<string>('state', LState) then
          Result := LState;
      finally
        JResp.Free;
      end;
    end;
  finally
    LHttpClient.Free;
  end;
end;

procedure TAiGeminiChat.SetIncludeThoughts(const Value: Boolean);
begin
  FIncludeThoughts := Value;
end;

procedure TAiGeminiChat.SetMediaResolution(const Value: TAiMediaResolution);
begin
  FMediaResolution := Value;
end;

procedure TAiGeminiChat.SetThinkingBudget(const Value: Integer);
begin
  FThinkingBudget := Value;
end;

// --- SYSTEM INSTRUCTION HELPER --- [Igual]
function TAiGeminiChat.GetSystemInstructionJson: TJSONObject;
var
  I: Integer;
  FullSystemPrompt: string;
begin
  Result := nil;
  FullSystemPrompt := '';
  for I := 0 to FMessages.Count - 1 do
  begin
    if SameText(FMessages[I].Role, 'system') then
      FullSystemPrompt := FullSystemPrompt + FMessages[I].Prompt + sLineBreak;
  end;
  FullSystemPrompt := Trim(FullSystemPrompt);
  if FullSystemPrompt <> '' then
  begin
    Result := TJSONObject.Create;
    var
    Parts := TJSonArray.Create;
    var
    Part := TJSONObject.Create;
    Part.AddPair('text', FullSystemPrompt);
    Parts.Add(Part);
    Result.AddPair('parts', Parts);
  end;
end;

procedure TAiGeminiChat.AddThoughtSignature(Msg: TAiChatMessage; const Signature: string);
var
  List: TStringList;
begin
  if Signature.Trim = '' then
    Exit;

  if not FThoughtSignatures.TryGetValue(Msg, List) then
  begin
    List := TStringList.Create;
    FThoughtSignatures.Add(Msg, List);
  end;
  List.Add(Signature);

end;

// --- SPEECH CONFIG HELPER ---
function TAiGeminiChat.BuildSpeechConfigJson: TJSONObject;
var
  LPrebuilt, LVoiceConfig, LMultiConfig, LSpeakerConfig: TJSONObject;
  LSpeakerList: TJSonArray;
  SL: TStringList;
  I, EqPos: Integer;
  RawVal, SpeakerName, VoiceName: string;
begin
  Result := nil;
  if Trim(Voice) = '' then
    Exit;

  SL := TStringList.Create;
  try
    // 1. Separar por comas (esto suele funcionar bien para la lista principal)
    // Ej: "Sol=Kore, Gustavo=Puck"
    SL.CommaText := Voice;

    // Limpiar entradas vacías
    for I := SL.Count - 1 downto 0 do
      if Trim(SL[I]) = '' then
        SL.Delete(I);

    // -------------------------------------------------------------------------
    // CASO A: MÚLTIPLES VOCES (> 1 elemento) -> MULTI-SPEAKER
    // -------------------------------------------------------------------------
    if SL.Count > 1 then
    begin
      Result := TJSONObject.Create;
      LMultiConfig := TJSONObject.Create;
      LSpeakerList := TJSonArray.Create;

      for I := 0 to SL.Count - 1 do
      begin
        RawVal := Trim(SL[I]); // Ej: "Sol=Kore" o "Puck"

        // --- NUEVA LÓGICA DE PARSEO MANUAL (MÁS SEGURA) ---
        EqPos := Pos('=', RawVal);

        if EqPos > 0 then
        begin
          // Encontró el '=', separamos:
          // Speaker: desde el inicio hasta antes del '='
          SpeakerName := Trim(Copy(RawVal, 1, EqPos - 1));
          // Voice: desde después del '=' hasta el final
          VoiceName := Trim(Copy(RawVal, EqPos + 1, Length(RawVal)));
        end
        else
        begin
          // No hay '=', es solo el nombre de la voz
          SpeakerName := 'Speaker' + IntToStr(I + 1); // Generamos nombre
          VoiceName := RawVal;
        end;
        // ---------------------------------------------------

        LSpeakerConfig := TJSONObject.Create;
        LSpeakerConfig.AddPair('speaker', SpeakerName);

        LPrebuilt := TJSONObject.Create;
        LPrebuilt.AddPair('voiceName', VoiceName);

        LVoiceConfig := TJSONObject.Create;
        LVoiceConfig.AddPair('prebuiltVoiceConfig', LPrebuilt);

        LSpeakerConfig.AddPair('voiceConfig', LVoiceConfig);
        LSpeakerList.Add(LSpeakerConfig);
      end;

      LMultiConfig.AddPair('speakerVoiceConfigs', LSpeakerList);
      Result.AddPair('multiSpeakerVoiceConfig', LMultiConfig);
    end

    // -------------------------------------------------------------------------
    // CASO B: UNA SOLA VOZ (= 1 elemento) -> SINGLE SPEAKER
    // -------------------------------------------------------------------------
    else if SL.Count = 1 then
    begin
      RawVal := Trim(SL[0]);

      // Parseo manual también aquí para limpiar "Gustavo=Puck" -> "Puck"
      EqPos := Pos('=', RawVal);

      if EqPos > 0 then
        VoiceName := Trim(Copy(RawVal, EqPos + 1, Length(RawVal)))
      else
        VoiceName := RawVal;

      Result := TJSONObject.Create;

      LPrebuilt := TJSONObject.Create;
      LPrebuilt.AddPair('voiceName', VoiceName);

      LVoiceConfig := TJSONObject.Create;
      LVoiceConfig.AddPair('prebuiltVoiceConfig', LPrebuilt);

      Result.AddPair('voiceConfig', LVoiceConfig);
    end;

  finally
    SL.Free;
  end;
end;

// --- TOOLS HELPER ---
function TAiGeminiChat.GetToolJSon: TJSonArray;
var
  LJsonFunctionsStr: String;
  LSourceJson: TJSONObject;
  LNormalizedTools: TList<TNormalizedTool>;
  LFinalToolsObj: TJSONObject;
  LToolsArray: TJSonArray;
begin
  Result := Nil;
  LJsonFunctionsStr := Trim(inherited GetTools(tfOpenAI).Text);
  if (LJsonFunctionsStr = '') or (not Tool_Active) then
    Exit;
  var
  LParsed := TJSONObject.ParseJSONValue(LJsonFunctionsStr);
  if not Assigned(LParsed) then
    Exit;
  LNormalizedTools := TList<TNormalizedTool>.Create;
  try
    if LParsed is TJSonArray then
    begin
      LSourceJson := TJSONObject.Create;
      LSourceJson.AddPair('tools', LParsed as TJSonArray);
    end
    else if LParsed is TJSONObject then
      LSourceJson := LParsed as TJSONObject
    else
    begin
      LParsed.Free;
      Exit;
    end;
    try
      TJsonToolUtils.NormalizeToolsFromSource('', LSourceJson, LNormalizedTools);
      LFinalToolsObj := TJsonToolUtils.FormatToolList(LNormalizedTools, tfGemini);
      try
        if LFinalToolsObj.TryGetValue<TJSonArray>('tools', LToolsArray) then
          Result := LToolsArray.Clone as TJSonArray;
      finally
        LFinalToolsObj.Free;
      end;
    finally
      LSourceJson.Free;
    end;
  finally
    LNormalizedTools.Free;
  end;
end;

// --- GET MESSAGES ---
// Convierte el historial interno al formato "parts" de Gemini
function TAiGeminiChat.GetMessages: TJSonArray;
Var
  I: Integer;
  Msg: TAiChatMessage;
  jObj, jPartItem, jInlineData, jFuncResponse, jResponseContent: TJSONObject;
  jParts: TJSonArray;
  MediaFile: TAiMediaFile;
  MediaArr: TAiMediaFilesArray;
  jToolCallValue, jPartVal: TJSONValue;
  JGeminiParts: TJSonArray;
  ResArr: TJSonArray;
  WrapperObj: TJSONObject;

  // Variables para gestión de firmas V3
  SignaturesList: TStringList;
  SigIndex: Integer;
  LResolutionStr: String;
  IsModelRole: Boolean;
begin
  ResArr := TJSonArray.Create;
  Try
    For I := 0 to Messages.Count - 1 do
    Begin
      Msg := Messages.Items[I];
      if SameText(Msg.Role, 'system') then
        Continue;

      jObj := TJSONObject.Create;

      // 1. Roles
      IsModelRole := False;
      if SameText(Msg.Role, 'assistant') or SameText(Msg.Role, 'model') then
      begin
        jObj.AddPair('role', 'model');
        IsModelRole := True;
      end
      else if SameText(Msg.Role, 'tool') then
        jObj.AddPair('role', 'function')
      else
        jObj.AddPair('role', 'user');

      jParts := TJSonArray.Create;
      jObj.AddPair('parts', jParts);

      // Firmas V3
      SignaturesList := nil;
      SigIndex := 0;
      if IsModelRole then
        FThoughtSignatures.TryGetValue(Msg, SignaturesList);

      // -----------------------------------------------------------------------
      // 2. TOOL RESPONSE (User -> Model)
      // -----------------------------------------------------------------------
      if Msg.Role = 'tool' then
      begin
        jFuncResponse := TJSONObject.Create;
        jFuncResponse.AddPair('name', Msg.FunctionName);
        jResponseContent := TJSONObject.Create;

        var
          JValArgs: TJSONValue := TJSONObject.ParseJSONValue(Msg.Prompt);
        try
          if Assigned(JValArgs) and (JValArgs is TJSONObject) then
            jResponseContent.AddPair('content', JValArgs.Clone as TJSONObject)
          else
          begin
            var
            SimpleContent := TJSONObject.Create;
            SimpleContent.AddPair('result', Msg.Prompt);
            jResponseContent.AddPair('content', SimpleContent);
          end;
        finally
          if Assigned(JValArgs) then
            JValArgs.Free;
        end;

        jFuncResponse.AddPair('response', jResponseContent);
        jPartItem := TJSONObject.Create.AddPair('functionResponse', jFuncResponse);
        jParts.Add(jPartItem);
      end

      // -----------------------------------------------------------------------
      // 3. MODEL TOOLS / CODE EXECUTION (Model -> User)
      // -----------------------------------------------------------------------
      else if (IsModelRole) and (not Msg.Tool_calls.IsEmpty) then
      begin
        jToolCallValue := TJSONObject.ParseJSONValue(Msg.Tool_calls);
        try
          if (jToolCallValue is TJSONObject) then
          begin
            WrapperObj := jToolCallValue as TJSONObject;

            // Caso A: Historial crudo de Gemini (gemini_parts)
            if WrapperObj.TryGetValue<TJSonArray>('gemini_parts', JGeminiParts) then
            begin
              // Texto explicativo previo (si existe)
              if not Msg.Prompt.IsEmpty then
              begin
                jPartItem := TJSONObject.Create.AddPair('text', Msg.Prompt);
                if Assigned(SignaturesList) and (SigIndex < SignaturesList.Count) then
                begin
                  jPartItem.AddPair('thoughtSignature', SignaturesList[SigIndex]);
                  Inc(SigIndex);
                end;
                jParts.Add(jPartItem);
              end;

              // Procesar partes del historial (Code, Results, Files...)
              for jPartVal in JGeminiParts do
              begin
                if jPartVal is TJSONObject then
                begin
                  // --- CORRECCIÓN CRÍTICA AQUÍ ---
                  // Si esta parte es un archivo binario (inlineData), NO la agregamos al historial de envío.
                  // El modelo ya sabe que generó el archivo, no necesitamos devolverle los bytes.
                  if TJSONObject(jPartVal).GetValue('inlineData') <> nil then
                    Continue;
                  // -------------------------------

                  var
                  ClonedPart := TJSONObject(jPartVal).Clone as TJSONObject;

                  // Inyectar firma si falta
                  if (ClonedPart.GetValue('thoughtSignature') = nil) and Assigned(SignaturesList) and (SigIndex < SignaturesList.Count) then
                  begin
                    ClonedPart.AddPair('thoughtSignature', SignaturesList[SigIndex]);
                    Inc(SigIndex);
                  end;

                  jParts.Add(ClonedPart);
                end;
              end;
            end
            // Caso B: Standard Function Call
            else
            begin
              if WrapperObj.GetValue('functionCall') <> nil then
                jPartItem := WrapperObj.Clone as TJSONObject
              else
                jPartItem := TJSONObject.Create.AddPair('functionCall', WrapperObj.Clone as TJSONObject);

              if Assigned(SignaturesList) and (SigIndex < SignaturesList.Count) then
              begin
                jPartItem.AddPair('thoughtSignature', SignaturesList[SigIndex]);
              end;
              jParts.Add(jPartItem);
            end;
          end;
        finally
          if Assigned(jToolCallValue) then
            jToolCallValue.Free;
        end;
      end

      // -----------------------------------------------------------------------
      // 4. TEXT & MEDIA (Standard)
      // -----------------------------------------------------------------------
      else
      begin
        // Texto
        if not Msg.Prompt.IsEmpty then
        begin
          jPartItem := TJSONObject.Create.AddPair('text', Msg.Prompt);
          if Assigned(SignaturesList) and (SigIndex < SignaturesList.Count) then
          begin
            jPartItem.AddPair('thoughtSignature', SignaturesList[SigIndex]);
          end;
          jParts.Add(jPartItem);
        end;

        // Media (Solo si es User)
        if not IsModelRole then
        begin

          Var
            TargetCategories: TAiFileCategories;

          if (tcm_code_interpreter in ChatMediaSupports) then
            TargetCategories := [Low(TAiFileCategory) .. High(TAiFileCategory)] // Permitir todo
          else
            TargetCategories := NativeInputFiles; // Filtro estricto estándar

          MediaArr := Msg.MediaFiles.GetMediaList(TargetCategories, False);

          if Length(MediaArr) > 0 then
          begin
            LResolutionStr := MediaTypeToResolutionString(FMediaResolution);
            for MediaFile in MediaArr do
            begin
              jPartItem := TJSONObject.Create;

              if (MediaFile.UrlMedia <> '') and (MediaFile.UrlMedia.StartsWith('https://generativelanguage.googleapis.com')) then
              begin
                var
                jFileData := TJSONObject.Create;
                jFileData.AddPair('mimeType', MediaFile.MimeType);
                jFileData.AddPair('fileUri', MediaFile.UrlMedia);
                jPartItem.AddPair('fileData', jFileData);
              end
              else
              begin
                jInlineData := TJSONObject.Create;
                jInlineData.AddPair('mime_type', MediaFile.MimeType);
                jInlineData.AddPair('data', MediaFile.Base64);
                jPartItem.AddPair('inline_data', jInlineData);
              end;

              jParts.Add(jPartItem);
            end;
          end;
        end;
      end;

      if jParts.Count > 0 then
        ResArr.Add(jObj)
      else
        jObj.Free;
    End;

    Result := TJSonArray(ResArr.Clone);
  Finally
    ResArr.Free;
  End;
end;

class function TAiGeminiChat.GetModels(aApiKey: String; aUrl: String = ''): TStringList;
Var
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  BaseEndPoint, RequestUrl, sNextPageToken: String;
  jRes: TJSONObject;
  JArr: TJSonArray;
  JVal, JMethod: TJSONValue;
  sModel: string;
  CustomModels: TArray<string>;
  I: Integer;
  SupportedMethods: TJSonArray;
  IsGenerative: Boolean;

  // Variables para extracción de parámetros
  LName, LVersion, LDisplayName, LDescription: string;
  LInputTokenLimit, LOutputTokenLimit: Int64;
  LTemperature, LTopP, LMaxTemperature: Double;
  LTopK: Integer;
  LThinking: Boolean;
begin
  Result := TStringList.Create;

  // 1. Configurar URL Base
  if aUrl <> '' then
    BaseEndPoint := aUrl
  Else
    BaseEndPoint := GlAIUrl;
  if not BaseEndPoint.EndsWith('/') then
    BaseEndPoint := BaseEndPoint + '/';

  Client := TNetHTTPClient.Create(Nil);
  try
    Client.ContentType := 'application/json';
    sNextPageToken := '';

    // --- BUCLE DE PAGINACIÓN ---
    repeat
      // 2. Construir URL con el token de página si existe
      RequestUrl := BaseEndPoint + 'models?key=' + aApiKey;
      if sNextPageToken <> '' then
        RequestUrl := RequestUrl + '&pageToken=' + sNextPageToken;

      // 3. Petición GET
      Res := Client.Get(RequestUrl);

      if Res.StatusCode = 200 then
      Begin
        jRes := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
        try
          if Assigned(jRes) then
          begin
            // A. Procesar Modelos de esta página
            if jRes.TryGetValue<TJSonArray>('models', JArr) then
            Begin
              For JVal in JArr do
              Begin
                if (JVal is TJSONObject) then
                begin
                  var
                  jObj := TJSONObject(JVal);

                  // --- EXTRACCIÓN DE VARIABLES ---
                  // Reiniciar defaults
                  LName := '';
                  LVersion := '';
                  LDisplayName := '';
                  LDescription := '';
                  LInputTokenLimit := 0;
                  LOutputTokenLimit := 0;
                  LTemperature := 0.0;
                  LTopP := 0.0;
                  LTopK := 0;
                  LMaxTemperature := 0.0;
                  LThinking := False;
                  IsGenerative := False;

                  // 1. Datos Básicos
                  jObj.TryGetValue<string>('name', LName);
                  jObj.TryGetValue<string>('version', LVersion);
                  jObj.TryGetValue<string>('displayName', LDisplayName);
                  jObj.TryGetValue<string>('description', LDescription);

                  // 2. Limits
                  jObj.TryGetValue<Int64>('inputTokenLimit', LInputTokenLimit);
                  jObj.TryGetValue<Int64>('outputTokenLimit', LOutputTokenLimit);

                  // 3. Params
                  jObj.TryGetValue<Double>('temperature', LTemperature);
                  jObj.TryGetValue<Double>('maxTemperature', LMaxTemperature);
                  jObj.TryGetValue<Double>('topP', LTopP);
                  jObj.TryGetValue<Integer>('topK', LTopK);
                  jObj.TryGetValue<Boolean>('thinking', LThinking);

                  // --- FILTRADO DE MÉTODOS ---
                  if jObj.TryGetValue<TJSonArray>('supportedGenerationMethods', SupportedMethods) then
                  begin
                    for JMethod in SupportedMethods do
                    begin
                      // ACEPTAMOS MÁS MÉTODOS AHORA:
                      // - generateContent: Chat/Texto standard
                      // - predict: Imagen (Imagen 3/4)
                      // - predictLongRunning: Video (Veo)
                      if SameText(JMethod.Value, 'generateContent') or SameText(JMethod.Value, 'predict') or SameText(JMethod.Value, 'predictLongRunning') then
                      begin
                        IsGenerative := True;
                        Break;
                      end;
                    end;
                  end;

                  // B. Agregar si es válido
                  if IsGenerative and (LName <> '') then
                  begin
                    sModel := StringReplace(LName, 'models/', '', [rfIgnoreCase]);
                    Result.Add(sModel);

                    // *AQUI* tienes todas las variables (LThinking, LInputTokenLimit, etc)
                    // listas para guardar en una estructura si lo deseas.
                  end;
                end;
              End;
            End;

            // C. Obtener el Token para la siguiente página (si hay)
            if not jRes.TryGetValue<string>('nextPageToken', sNextPageToken) then
              sNextPageToken := ''; // Si no hay token, terminamos el bucle
          end;
        finally
          jRes.Free;
        end;
      End
      else
      begin
        // Error de conexión, rompemos el bucle
        sNextPageToken := '';
      end;

    until sNextPageToken = ''; // Repetir mientras haya más páginas

    // 4. Agregar Modelos Custom del Factory
    CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);
    for I := Low(CustomModels) to High(CustomModels) do
    begin
      if Result.IndexOf(CustomModels[I]) = -1 then
        Result.Add(CustomModels[I]);
    end;

  finally
    Client.Free;
  end;
end;

// --- INIT CHAT COMPLETIONS (Payload Construction) ---

function TAiGeminiChat.InitChatCompletions: String;
var
  LRequest, JConfig, JSystemInst: TJSONObject;
  JArrTools, JUserTools: TJSonArray;
  JStop: TJSonArray;
  LToolObj: TJSONObject;
  Lista: TStringList;
  I: Integer;
  ActiveCacheName: string;
begin
  LRequest := TJSONObject.Create;
  Lista := TStringList.Create;

  try
    // -------------------------------------------------------------------------
    // 1. CONTEXTO Y CACHÉ
    // -------------------------------------------------------------------------
    ActiveCacheName := '';
    for I := FMessages.Count - 1 downto 0 do
    begin
      var
      Msg := FMessages[I];
      if Assigned(Msg.MediaFiles) then
        for var MediaFile in Msg.MediaFiles do
          if not MediaFile.CacheName.IsEmpty then
          begin
            ActiveCacheName := MediaFile.CacheName;
            Break;
          end;
      if not ActiveCacheName.IsEmpty then
        Break;
    end;

    // -------------------------------------------------------------------------
    // 2. SYSTEM INSTRUCTION
    // -------------------------------------------------------------------------
    JSystemInst := GetSystemInstructionJson;
    if Assigned(JSystemInst) then
      LRequest.AddPair('systemInstruction', JSystemInst);

    // -------------------------------------------------------------------------
    // 3. CONTENTS
    // -------------------------------------------------------------------------
    LRequest.AddPair('contents', GetMessages);

    if not ActiveCacheName.IsEmpty then
      LRequest.AddPair('cachedContent', TJSONString.Create(ActiveCacheName));

    // -------------------------------------------------------------------------
    // 4. TOOLS (Validación Estricta)
    // -------------------------------------------------------------------------
    JArrTools := TJSonArray.Create;

    // A. Funciones de Usuario (Function Calling)
    if Tool_Active then
    begin
      JUserTools := GetToolJSon;
      if Assigned(JUserTools) then
      begin
        // Clonamos las funciones de usuario al array principal
        for var JVal in JUserTools do
          JArrTools.Add(JVal.Clone as TJSONObject);
        JUserTools.Free;
      end;
    end;

    // B. Code Execution (Solo si está explícitamente en el Set)
    if (tcm_code_interpreter in ChatMediaSupports) then
    begin
      LToolObj := TJSONObject.Create;
      LToolObj.AddPair('codeExecution', TJSONObject.Create);
      JArrTools.Add(LToolObj);
    end;

    // C. Google Search (Solo si está explícitamente en el Set)
    if (tcm_WebSearch in ChatMediaSupports) then
    begin
      LToolObj := TJSONObject.Create;
      LToolObj.AddPair('googleSearch', TJSONObject.Create);
      JArrTools.Add(LToolObj);
    end;

    // IMPORTANTE: Solo enviamos "tools" si hay algo dentro.
    if JArrTools.Count > 0 then
      LRequest.AddPair('tools', JArrTools)
    else
      JArrTools.Free;

    // -------------------------------------------------------------------------
    // 5. GENERATION CONFIG
    // -------------------------------------------------------------------------
    JConfig := TJSONObject.Create;
    LRequest.AddPair('generationConfig', JConfig);

    // A. Parámetros Estándar (No perder funcionalidad básica)
    if Temperature >= 0 then
      JConfig.AddPair('temperature', TJSONNumber.Create(Temperature));

    if Top_p >= 0 then
      JConfig.AddPair('topP', TJSONNumber.Create(Top_p));

    if Max_tokens > 0 then
      JConfig.AddPair('maxOutputTokens', TJSONNumber.Create(Max_tokens));

    // Stop Sequences
    if Stop <> '' then
    begin
      Lista.CommaText := Stop;
      if Lista.Count > 0 then
      begin
        JStop := TJSonArray.Create;
        for I := 0 to Lista.Count - 1 do
          JStop.Add(Lista[I]);
        JConfig.AddPair('stopSequences', JStop);
      end;
    end;

    // B. Configuración de Imagen (Gemini 3 Image / Imagen 3)
    if ImageParams.Count > 0 then
    begin
      var
      JImageConfig := TJSONObject.Create;
      if ImageParams.Values['aspectRatio'] <> '' then
        JImageConfig.AddPair('aspectRatio', ImageParams.Values['aspectRatio']);
      if ImageParams.Values['imageSize'] <> '' then
        JImageConfig.AddPair('imageSize', ImageParams.Values['imageSize']);
      if ImageParams.Values['personGeneration'] <> '' then
        JImageConfig.AddPair('personGeneration', ImageParams.Values['personGeneration']);

      if JImageConfig.Count > 0 then
        JConfig.AddPair('imageConfig', JImageConfig)
      else
        JImageConfig.Free;
    end;

    // C. Response Format (JSON Mode / Schema)
    if (Response_format = tiaChatRfJsonSchema) and (JsonSchema.Count > 0) then
    begin
      JConfig.AddPair('responseMimeType', 'application/json');
      try
        Var sShema := StringReplace(JsonSchema.Text,'\n',' ',[rfReplaceAll]);
        var
        JSchema := TJSONObject.ParseJSONValue(sShema) as TJSONObject;
        if Assigned(JSchema) then
          // CORRECCIÓN: La documentación exige 'responseJsonSchema', no 'responseSchema'
          JConfig.AddPair('responseJsonSchema', JSchema);
      except
      end;
    end
    else if (Response_format = tiaChatRfJson) then
    begin
      JConfig.AddPair('responseMimeType', 'application/json');
    end;

    if (ThinkingLevel <> tlDefault) or (FThinkingBudget > 0) then
    begin
      var
      JThinking := TJSONObject.Create;

      // 1. Calcular Budget
      var
        LBudget: Integer := 0;

      if FThinkingBudget > 0 then
        LBudget := FThinkingBudget // Prioridad al valor manual
      else if Max_tokens > 0 then
      begin
        // Mapeo proporcional si no se define presupuesto fijo
        case ThinkingLevel of
          tlLow:
            LBudget := Trunc(Max_tokens * 0.25); // 25% del total
          tlMedium:
            LBudget := Trunc(Max_tokens * 0.50); // 50% del total
          tlHigh:
            LBudget := Trunc(Max_tokens * 0.80); // 80% del total
        end;
        // Safety: Gemini suele requerir un mínimo (ej. 1024) para pensar efectivamente
        if LBudget < 1024 then
          LBudget := 1024;
      end;

      // Solo añadimos budget si es mayor a 0 (0 deshabilita thinking en Flash 2.5)
      if LBudget > 0 then
        JThinking.AddPair('thinkingBudget', TJSONNumber.Create(LBudget));

      JThinking.AddPair('includeThoughts', TJSONBool.Create(FIncludeThoughts));
      JConfig.AddPair('thinkingConfig', JThinking);
    end;

    Result := LRequest.Format;

  finally
    LRequest.Free;
    Lista.Free;
  end;
end;

// --- EXECUTE REQUEST (InternalRunCompletions) --- [Igual]
function TAiGeminiChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody, sUrl, LModel, LMethod: String;
  Res: IHTTPResponse;
  St: TStringStream;
  jObj: TJSONObject;
  MF: TAiMediaFile;
begin
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // ---------------------------------------------------------------------------
  // AUTO-UPLOAD PARA DOCUMENTOS (Code Interpreter / Multimodal)
  // Gemini exige File API para Excel, PDF, CSV, etc. No admite Base64 (inline).
  // ---------------------------------------------------------------------------
  if Assigned(AskMsg.MediaFiles) then
  begin
    for MF in AskMsg.MediaFiles do
    begin
      // Si el archivo NO es imagen ni audio, y aún no tiene URL de nube...
      if (not(MF.FileCategory in [Tfc_Image, Tfc_Audio])) and (MF.UrlMedia = '') then
      begin
        try
          UploadFile(MF); // Esto actualiza MF.UrlMedia con la uri 'https://generativelanguage...'
        except
          on E: Exception do
            // Opcional: Loguear error o dejar que falle, pero mejor intentamos seguir
            DoError('Error Auto-Uploading file for Code Interpreter: ' + E.Message, E);
        end;
      end;
    end;
  end;
  // ---------------------------------------------------------------------------

  if Assigned(FTmpToolCallBuffer) then
    FTmpToolCallBuffer.Clear;

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  if FClient.Asynchronous then
    LMethod := 'streamGenerateContent'
  else
    LMethod := 'generateContent';

  sUrl := Url + 'models/' + LModel + ':' + LMethod + '?key=' + ApiKey;
  St := TStringStream.Create('', TEncoding.UTF8);

  try
    DoStateChange(acsConnecting, 'Sending request...');

    FClient.ContentType := 'application/json';
    ABody := InitChatCompletions;

    LogDebug('--Request Body--');
    LogDebug(ABody);

    St.WriteString(ABody);
    St.Position := 0;
    FResponse.Clear;
    FResponse.Position := 0;
    FTmpResponseText := '';

    Res := FClient.Post(sUrl, St, FResponse);

    if FClient.Asynchronous = False then
    begin
      FResponse.Position := 0;
      LogDebug('--Response Sincrono--');
      LogDebug(Res.ContentAsString);

      if Res.StatusCode = 200 then
      begin
        jObj := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
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
        var
        ErrMsg := Format('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
        DoError(ErrMsg, nil);
        Result := '';
      end;
    end
    else
    begin
      Result := '';
    end;

  finally
    if FClient.Asynchronous = False then
      St.Free;
  end;
end;

// --- PARSE CHAT RESPONSE ---
procedure TAiGeminiChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
Var
  LCandidates: TJSonArray;
  LContent, LUso: TJSONObject;
  LRespuesta, LRole, sText, LPartSig: String;
  aPrompt_tokens, aCompletion_tokens, atotal_tokens, aThoughts_tokens: Integer;
  AskMsg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;
  ModelVersion, ResponseId: String;

  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave: String;

  JPromptFeedback: TJSONObject;

  // Variables auxiliares para partes
  jValPart: TJSONValue;
  LPartObj, LExecCodeObj, LCodeResultObj, LInlineData: TJSONObject;
  LCode, LLang, LCodeOutput, LMimeType, LBase64Data: String;
  LExt: String;
begin
  LRespuesta := '';
  LRole := 'model';
  AskMsg := GetLastMessage;

  // 1. Validaciones de Seguridad y Candidatos
  if not jObj.TryGetValue<TJSonArray>('candidates', LCandidates) or (LCandidates.Count = 0) then
  begin
    if jObj.TryGetValue<TJSONObject>('promptFeedback', JPromptFeedback) then
    begin
      DoError('Bloqueado por seguridad: ' + JPromptFeedback.ToJSON, nil);
      Exit;
    end;
    FBusy := False;
    // Si no hay candidatos ni feedback, puede ser un error de estructura o un finishReason raro
    DoError('La respuesta de Gemini no contiene candidatos válidos.', nil);
    Exit;
  end;

  // 2. Extracción de Metadatos Generales
  jObj.TryGetValue<String>('modelVersion', ModelVersion);
  jObj.TryGetValue<String>('responseId', ResponseId);

  // 3. Conteo de Tokens (Standard + Thinking)
  aPrompt_tokens := 0;
  aCompletion_tokens := 0;
  atotal_tokens := 0;
  aThoughts_tokens := 0;

  if jObj.TryGetValue<TJSONObject>('usageMetadata', LUso) then
  begin
    LUso.TryGetValue<Integer>('promptTokenCount', aPrompt_tokens);
    LUso.TryGetValue<Integer>('candidatesTokenCount', aCompletion_tokens);
    LUso.TryGetValue<Integer>('totalTokenCount', atotal_tokens);
    // [V3] Token count específico para pensamientos
    LUso.TryGetValue<Integer>('thoughtsTokenCount', aThoughts_tokens);
  end;

  var
  LCandidate := LCandidates.Items[0] as TJSONObject;

  // 4. Procesar Grounding (Búsqueda Web)
  ParseGroundingMetadata(LCandidate, ResMsg);

  // 5. Procesar Contenido (Partes del mensaje)
  if LCandidate.TryGetValue<TJSONObject>('content', LContent) then
  begin
    LContent.TryGetValue<string>('role', LRole);
    var
      LParts: TJSonArray;

    if LContent.TryGetValue<TJSonArray>('parts', LParts) then
    Begin
      for jValPart in LParts do
      begin
        if not(jValPart is TJSONObject) then
          Continue;
        LPartObj := jValPart as TJSONObject;

        // [V3] CAPTURAR THOUGHT SIGNATURE (De cualquier parte)
        // Se guarda temporalmente en ResMsg. Si luego resulta ser un ToolCall,
        // transferiremos estas firmas al mensaje intermedio.
        if LPartObj.TryGetValue<string>('thoughtSignature', LPartSig) then
        begin
          AddThoughtSignature(ResMsg, LPartSig);
        end;

        // A. Texto Normal
        if LPartObj.TryGetValue<string>('text', sText) then
          LRespuesta := Trim(LRespuesta + sText);

        // B. Code Execution: Código generado (Historial)
        if LPartObj.TryGetValue<TJSONObject>('executableCode', LExecCodeObj) then
        begin
          LExecCodeObj.TryGetValue<string>('language', LLang);
          if LExecCodeObj.TryGetValue<string>('code', LCode) then
            LRespuesta := LRespuesta + sLineBreak + '```' + LowerCase(LLang) + sLineBreak + Trim(LCode) + sLineBreak + '```';
        end;

        // C. Code Execution: Resultado (Historial)
        if LPartObj.TryGetValue<TJSONObject>('codeExecutionResult', LCodeResultObj) then
        begin
          if LCodeResultObj.TryGetValue<string>('output', LCodeOutput) then
            LRespuesta := LRespuesta + sLineBreak + '> **Output:**' + sLineBreak + '```' + sLineBreak + Trim(LCodeOutput) + sLineBreak + '```';
        end;

        // D. Multimedia Inline (Imágenes/Audio generados)
        if LPartObj.TryGetValue<TJSONObject>('inlineData', LInlineData) then
        begin
          LInlineData.TryGetValue<string>('mimeType', LMimeType);
          LInlineData.TryGetValue<string>('data', LBase64Data);

          if (LBase64Data <> '') then
          begin
            var
            LNewMediaFile := TAiMediaFile.Create;
            try
              // Determinar extensión aproximada para guardar
              LExt := 'bin';
              if LMimeType.Contains('wav') then
                LExt := 'wav'
              else if LMimeType.Contains('mp3') then
                LExt := 'mp3'
              else if LMimeType.Contains('png') then
                LExt := 'png'
              else if LMimeType.Contains('jpeg') then
                LExt := 'jpg';

              LNewMediaFile.LoadFromBase64('generated.' + LExt, LBase64Data);
              // LNewMediaFile.MimeType := LMimeType; // Propiedad readonly derivada del nombre, o ajustar clase base
              ResMsg.MediaFiles.Add(LNewMediaFile);
            except
              LNewMediaFile.Free;
            end;
          end;
        end;
      end;
    End;
  end;

  LRespuesta := Trim(LRespuesta);
  Self.FLastContent := LRespuesta;

  // Actualizar acumuladores globales del Chat
  Self.Prompt_tokens := Self.Prompt_tokens + aPrompt_tokens;
  Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
  Self.Total_tokens := Self.Total_tokens + atotal_tokens;
  Self.Thinking_tokens := Self.Thinking_tokens + aThoughts_tokens;

  // Si se solicitó extracción de archivos de texto (native output)
  If tfc_ExtracttextFile in NativeOutputFiles then
    InternalExtractCodeFiles(LRespuesta, ResMsg);

  // --- FUNCTION CALLING / TOOLS ---
  LFunciones := ExtractToolCallFromJson(LCandidates);

  // -------------------------------------------------------------------------
  // CASO 1: NO HAY FUNCIONES (RESPUESTA FINAL)
  // -------------------------------------------------------------------------
  If (Not Assigned(LFunciones)) or (LFunciones.Count <= 0) then
  Begin
    FBusy := False;

    // Asignar propiedades al mensaje de respuesta
    ResMsg.Role := LRole;
    ResMsg.Tool_calls := '';
    ResMsg.Model := ModelVersion;
    // Acumulamos el texto. Nota: Si ya traía algo (streaming), concatenamos o seteamos.
    // En ParseChat normal (síncrono o final de async), Prompt suele estar vacío al inicio del parseo de este frame.
    ResMsg.Prompt := LRespuesta;

    // Asignar Tokens al mensaje individual
    ResMsg.Prompt_tokens := aPrompt_tokens;
    ResMsg.Completion_tokens := aCompletion_tokens;
    ResMsg.Total_tokens := atotal_tokens;
    ResMsg.Thinking_tokens := aThoughts_tokens;

    // Procesar audio, links, etc.
    DoProcessResponse(AskMsg, ResMsg, LRespuesta);

    // Eventos Finales
    DoStateChange(acsFinished, 'Done');

    If Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, ResMsg, jObj, LRole, LRespuesta);
  End
  // -------------------------------------------------------------------------
  // CASO 2: HAY FUNCIONES (EJECUCIÓN DE HERRAMIENTAS)
  // -------------------------------------------------------------------------
  Else
  Begin

    var
    Msg := TAiChatMessage.Create(LRespuesta, LRole);

    // Construir el JSON de tool_calls para el historial
    var
    JToolsArr := TJSonArray.Create;
    for Clave in LFunciones.Keys do
    begin
      ToolCall := LFunciones[Clave];
      var
      jCall := TJSONObject.Create;
      jCall.AddPair('name', ToolCall.Name);

      var
      jArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments);
      if not Assigned(jArgs) then
        jArgs := TJSONObject.Create;

      jCall.AddPair('args', jArgs);

      // Wrapper de Gemini para FunctionCall
      var
      jWrapper := TJSONObject.Create;
      jWrapper.AddPair('functionCall', jCall);
      JToolsArr.Add(jWrapper);
    end;

    // Guardar en Msg.Tool_calls
    if JToolsArr.Count = 1 then
    begin
      // Guardamos el objeto simple: {"functionCall": ...}
      // GetMessages lo detectará y NO lo volverá a envolver gracias a la corrección.
      Msg.Tool_calls := JToolsArr.Items[0].ToJSON;
      JToolsArr.Free;
    end
    else
    begin
      // Múltiples funciones: usamos gemini_parts
      var
      JWrapperParts := TJSONObject.Create;
      JWrapperParts.AddPair('gemini_parts', JToolsArr);
      Msg.Tool_calls := JWrapperParts.ToJSON;
      JWrapperParts.Free;
    end;

    Msg.Id := FMessages.Count + 1;
    Msg.Prompt_tokens := aPrompt_tokens;
    Msg.Completion_tokens := aCompletion_tokens;
    Msg.Total_tokens := atotal_tokens;
    Msg.Thinking_tokens := aThoughts_tokens;

    // [V3 CRITICAL] TRANSFERENCIA DE FIRMAS
    // Las firmas capturadas arriba se guardaron en 'ResMsg' (el objeto pasado por referencia).
    // Pero en el flujo de herramientas, 'ResMsg' se recicla para el resultado final de la recursión.
    // El mensaje que contiene la llamada a la función (y por tanto la firma) es 'Msg'.
    // Debemos mover las firmas de ResMsg a Msg.
    var
      SrcList: TStringList;
    if FThoughtSignatures.TryGetValue(ResMsg, SrcList) then
    begin
      for var Sig in SrcList do
        AddThoughtSignature(Msg, Sig);

      // Limpiamos las firmas de ResMsg para que no se dupliquen cuando se llene con la respuesta final
      FThoughtSignatures.Remove(ResMsg);
    end;

    FMessages.Add(Msg);

    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, Msg, jObj, Msg.Role, '');

    // Ejecutar las funciones en hilos paralelos
    Try
      NumTasks := LFunciones.Count;
      SetLength(TaskList, NumTasks);
      I := 0;
      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];
        ToolCall.ResMsg := ResMsg; // Pasamos referencias para que la tool pueda escribir si quiere
        ToolCall.AskMsg := AskMsg;

        TaskList[I] := TTask.Create(
          procedure
          begin
            Try
              DoCallFunction(ToolCall);
            Except
              On E: Exception do
                TThread.Queue(nil,
                  procedure
                  begin
                    DoError('Error in "' + ToolCall.Name + '"', E);
                  end);
            End;
          end);
        TaskList[I].Start;
        Inc(I);
      End;
      TTask.WaitForAll(TaskList);

      // Crear los mensajes de respuesta de las herramientas
      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];
        var
        ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
        ToolMsg.Id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
      End;

      // Llamada recursiva para obtener la respuesta final del modelo
      // El resultado final llenará 'ResMsg' correctamente.
      Self.Run(Nil, ResMsg);

      // Limpieza cosmética: A veces la recursión añade el texto al final,
      // limpiamos si es necesario o dejamos que ParseChat recursivo maneje el Prompt.
      // ResMsg.Content := '';

    Finally
      LFunciones.Free;
    End;
  End;
end;

procedure TAiGeminiChat.ParseGroundingMetadata(jCandidate: TJSONObject; ResMsg: TAiChatMessage);
var
  jGroundingMeta, jChunk, jWeb: TJSONObject;
  jChunksArray: TJSonArray;
  ChunkItem: TAiWebSearchItem;
  I: Integer;
  WebTitle, WebUri: string;
begin
  if not Assigned(ResMsg.WebSearchResponse) then
    ResMsg.WebSearchResponse := TAiWebSearch.Create;
  ResMsg.WebSearchResponse.annotations.Clear;
  if not jCandidate.TryGetValue<TJSONObject>('groundingMetadata', jGroundingMeta) then
    Exit;
  if jGroundingMeta.TryGetValue<TJSonArray>('groundingChunks', jChunksArray) then
  begin
    for I := 0 to jChunksArray.Count - 1 do
    begin
      jChunk := jChunksArray.Items[I] as TJSONObject;
      if jChunk.TryGetValue<TJSONObject>('web', jWeb) then
      begin
        ChunkItem := TAiWebSearchItem.Create;
        jWeb.TryGetValue<string>('title', WebTitle);
        jWeb.TryGetValue<string>('uri', WebUri);
        ChunkItem.title := WebTitle;
        ChunkItem.Url := WebUri;
        ChunkItem.&type := 'web_page';
        ResMsg.WebSearchResponse.annotations.Add(ChunkItem);
      end;
    end;
  end;
end;

function TAiGeminiChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
var
  jCandidateValue, jPartValue: TJSONValue;
  jContent, jFunctionCall, LArgsObject: TJSONObject;
  jParts: TJSonArray;
  LFunction: TAiToolsFunction;
  I: Integer;
  Nom, Valor: String;
begin
  Result := nil;
  if not Assigned(jChoices) or (jChoices.Count = 0) then
    Exit;
  jCandidateValue := jChoices.Items[0];
  if not(jCandidateValue is TJSONObject) then
    Exit;
  Result := TAiToolsFunctions.Create;
  if not TJSONObject(jCandidateValue).TryGetValue<TJSONObject>('content', jContent) or not jContent.TryGetValue<TJSonArray>('parts', jParts) then
  begin
    FreeAndNil(Result);
    Exit;
  end;
  for jPartValue in jParts do
  begin
    if (jPartValue is TJSONObject) and TJSONObject(jPartValue).TryGetValue<TJSONObject>('functionCall', jFunctionCall) then
    begin
      LFunction := TAiToolsFunction.Create;
      try
        jFunctionCall.TryGetValue<string>('name', LFunction.Name);
        if jFunctionCall.TryGetValue<TJSONObject>('args', LArgsObject) then
          LFunction.Arguments := LArgsObject.ToJSON
        else
          LFunction.Arguments := '{}';
        if Assigned(LArgsObject) then
          For I := 0 to LArgsObject.Count - 1 do
          Begin
            Nom := LArgsObject.Pairs[I].JsonString.Value;
            Valor := LArgsObject.Pairs[I].JsonValue.Value;
            LFunction.Params.Values[Nom] := Valor;
          End;
        LFunction.Id := 'call_' + TGuid.NewGuid.ToString;
        LFunction.Tipo := 'function';
        Result.Add(LFunction.Id, LFunction);
      except
        LFunction.Free;
        raise;
      end;
    end;
  end;
  if Result.Count = 0 then
    FreeAndNil(Result);
end;

// [InternalAddMessage methods iguales, solo para contexto]
Function TAiGeminiChat.InternalAddMessage(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = ''): TAiChatMessage;
Var
  Msg: TAiChatMessage;
  MensajeInicial: String;
begin
  Try
    If (FMessages.Count = 0) then
    Begin
      MensajeInicial := Self.PrepareSystemMsg;
      if Trim(MensajeInicial) <> '' then
      begin
        Msg := TAiChatMessage.Create(MensajeInicial, 'system');
        Msg.Id := FMessages.Count + 1;
        FMessages.Add(Msg);
        if Assigned(FOnAddMessage) then
          FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
      end;
    End;
    Msg := TAiChatMessage.Create(aPrompt, aRole, aToolCallId, aFunctionName);
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);
    FLastPrompt := aPrompt;
    Result := Msg;
  Finally
  End;
end;

Function TAiGeminiChat.InternalAddMessage(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage;
Var
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
begin
  Try
    Msg := TAiChatMessage.Create(aPrompt, aRole);
    For MF in aMediaFiles do
      Msg.AddMediaFile(MF);
    Result := InternalAddMessage(Msg);
  Finally
  End;
end;

function TAiGeminiChat.InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
Var
  MensajeInicial: String;
  InitMsg: TAiChatMessage;
  MF: TAiMediaFile;
  Procesado: Boolean;
  Respuesta: String;
begin
  Procesado := False;
  Respuesta := '';
  Try
    if (FMessages.Count = 0) then
    Begin
      MensajeInicial := Self.PrepareSystemMsg;
      if Trim(MensajeInicial) <> '' then
      begin
        InitMsg := TAiChatMessage.Create(MensajeInicial, 'system');
        InitMsg.Id := FMessages.Count + 1;
        FMessages.Add(InitMsg);
        if Assigned(FOnAddMessage) then
          FOnAddMessage(Self, InitMsg, Nil, 'system', MensajeInicial);
      end;
    End;
    aMsg.Id := FMessages.Count + 1;
    FMessages.Add(aMsg);
    for MF in aMsg.MediaFiles do
    Begin
      Procesado := False;
      DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado);
      if Procesado then
      begin
        aMsg.Prompt := aMsg.Prompt + sLineBreak + Respuesta;
        MF.Procesado := True;
        MF.Transcription := Respuesta;
      end;
    End;
    FLastPrompt := aMsg.Prompt;
    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, aMsg, Nil, aMsg.Role, aMsg.Prompt);
    if Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, aMsg);
    Result := aMsg;
  Finally
  End;
end;

// Añadir en la sección Private o Protected de la clase
procedure TAiGeminiChat.InternalCompleteRequest;
Var
  ResMsg: TAiChatMessage;
  JArrParts: TJSonArray;
  Wrapper: TJSONObject;
  Keys: TArray<Integer>;
  K: Integer;
  PartObj, jInlineData: TJSONObject;
  LNewMediaFile: TAiMediaFile;
  LBase64Data, LMimeType, LSig: String;

  Code: TMarkdownCodeExtractor;
  CodeFile: TCodeFile;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;

begin
  if not FBusy then
    Exit; // Ya se procesó
  FBusy := False;

  // 1. Crear el mensaje final
  ResMsg := TAiChatMessage.Create(FLastContent, 'model');

  // Recuperar tokens acumulados durante el stream
  ResMsg.Prompt_tokens := Self.Prompt_tokens;
  ResMsg.Completion_tokens := Self.Completion_tokens;
  ResMsg.Total_tokens := Self.Total_tokens;
  ResMsg.Thinking_tokens := Self.Thinking_tokens;

  // 2. Procesar Buffer de Herramientas/Archivos/Firmas acumulados
  if FTmpToolCallBuffer.Count > 0 then
  begin
    JArrParts := TJSonArray.Create;
    Keys := FTmpToolCallBuffer.Keys.ToArray;
    TArray.Sort<Integer>(Keys);

    for K in Keys do
    begin
      PartObj := FTmpToolCallBuffer[K];

      // [Gemini 3] Recuperar firmas guardadas en el buffer
      if PartObj.TryGetValue<string>('thoughtSignature', LSig) then
        AddThoughtSignature(ResMsg, LSig);

      // Recuperar Archivos generados
      if PartObj.TryGetValue<TJSONObject>('inlineData', jInlineData) then
      begin
        jInlineData.TryGetValue<string>('mimeType', LMimeType);
        jInlineData.TryGetValue<string>('data', LBase64Data);

        if (LBase64Data <> '') then
        begin
          LNewMediaFile := TAiMediaFile.Create;
          try
            // Lógica simple de extensión
            var
            LExt := 'bin';
            if LMimeType.Contains('png') then
              LExt := 'png'
            else if LMimeType.Contains('jpeg') then
              LExt := 'jpg'
            else if LMimeType.Contains('wav') then
              LExt := 'wav';

            LNewMediaFile.LoadFromBase64('generated.' + LExt, LBase64Data);
            ResMsg.MediaFiles.Add(LNewMediaFile);
          except
            LNewMediaFile.Free;
          end;
        end;
        JArrParts.Add(PartObj);
      end
      else
        JArrParts.Add(PartObj);
    end;

    // Empaquetar historial
    Wrapper := TJSONObject.Create;
    try
      Wrapper.AddPair('gemini_parts', JArrParts);
      ResMsg.Tool_calls := Wrapper.ToJSON;
    finally
      Wrapper.Free;
    end;

    FTmpToolCallBuffer.Clear;
  end;

  // 3. Finalizar
  ResMsg.Id := FMessages.Count + 1;
  FMessages.Add(ResMsg);

  If tfc_ExtracttextFile in NativeOutputFiles then
  Begin
    Code := TMarkdownCodeExtractor.Create;
    Try

      CodeFiles := Code.ExtractCodeFiles(FLastContent);
      For CodeFile in CodeFiles do
      Begin
        St := TStringStream.Create(CodeFile.Code);
        Try
          St.Position := 0;

          MF := TAiMediaFile.Create;
          MF.LoadFromStream('file.' + CodeFile.FileType, St);
          ResMsg.MediaFiles.Add(MF);
        Finally
          St.Free;
        End;

      End;
    Finally
      Code.Free;
    End;
  End;

  DoStateChange(acsFinished, 'Done');

  if Assigned(FOnReceiveDataEnd) then
    FOnReceiveDataEnd(Self, ResMsg, nil, 'model', FLastContent);
end;

procedure TAiGeminiChat.InternalExtractCodeFiles(const AText: string; AMessage: TAiChatMessage);
var
  Code: TMarkdownCodeExtractor;
  CodeFiles: TCodeFileList;
  CodeFile: TCodeFile;
  MF: TAiMediaFile;
  St: TStringStream;
  FileName: string;
begin
  // Solo procesar si el usuario lo configuró en NativeOutputFiles
  if not(tfc_ExtracttextFile in NativeOutputFiles) then
    Exit;

  if AText.Trim.IsEmpty then
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

        // Intentar dar un nombre lógico
        if CodeFile.FileName <> '' then
          FileName := CodeFile.FileName
        else
          FileName := 'snippet_' + IntToStr(AMessage.MediaFiles.Count + 1) + '.' + CodeFile.FileType;

        MF.LoadFromStream(FileName, St);

        // Opcional: Marcar que es un archivo generado
        // MF.MimeType := 'text/plain';

        AMessage.MediaFiles.Add(MF);
      finally
        St.Free;
      end;
    end;
  finally
    Code.Free;
  end;
end;

// [Métodos de archivo (UploadFile, etc.) se mantienen igual]
function TAiGeminiChat.UploadFile(aMediaFile: TAiMediaFile): String;
var
  LHttpClient: TNetHTTPClient;
  LStartUrl, LUploadUrl, LFileUri, CloudName, CloudState: string;
  LResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
  LJsonBody, LUploadResponseObj, LFileObj: TJSONObject;
  LFileStream: TStream;
  LNumBytes: Int64;
begin
  Result := '';
  LHttpClient := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  LHttpClient.SynchronizeEvents := False;
{$ENDIF}
  try
    LStartUrl := GlAIUploadUrl + 'files?key=' + Self.ApiKey;
    LFileStream := aMediaFile.Content;
    LFileStream.Position := 0;
    LNumBytes := LFileStream.Size;
    if LNumBytes = 0 then
      raise Exception.Create('No se pudo obtener el contenido del archivo para subir.');
    try
      LHeaders := [TNetHeader.Create('X-Goog-Upload-Protocol', 'resumable'), TNetHeader.Create('X-Goog-Upload-Command', 'start'), TNetHeader.Create('X-Goog-Upload-Header-Content-Length', LNumBytes.ToString),
        TNetHeader.Create('X-Goog-Upload-Header-Content-Type', aMediaFile.MimeType)];
      LJsonBody := TJSONObject.Create;
      try
        LFileObj := TJSONObject.Create;
        LFileObj.AddPair('display_name', TJSONString.Create(aMediaFile.FileName));
        LJsonBody.AddPair('file', LFileObj);
        LHttpClient.ContentType := 'application/json';
        LResponse := LHttpClient.Post(LStartUrl, TStringStream.Create(LJsonBody.ToJSON, TEncoding.UTF8), nil, LHeaders);
      finally
        FreeAndNil(LJsonBody);
      end;
      if LResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('Error start upload: %d %s', [LResponse.StatusCode, LResponse.StatusText]);
      LUploadUrl := LResponse.HeaderValue['X-Goog-Upload-Url'];
      LFileStream.Position := 0;
      LHeaders := [TNetHeader.Create('Content-Length', LNumBytes.ToString), TNetHeader.Create('X-Goog-Upload-Offset', '0'), TNetHeader.Create('X-Goog-Upload-Command', 'upload, finalize')];
      LHttpClient.ContentType := 'application/octet-stream';
      LResponse := LHttpClient.Post(LUploadUrl, LFileStream, nil, LHeaders);
      if LResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('Error upload bytes: %d', [LResponse.StatusCode]);
      LUploadResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      if Assigned(LUploadResponseObj) then
        try
          if LUploadResponseObj.TryGetValue<TJSONObject>('file', LFileObj) then
          begin
            if LFileObj.TryGetValue<string>('uri', LFileUri) then
              Result := LFileUri;
            LFileObj.TryGetValue<string>('name', CloudName);
            LFileObj.TryGetValue<string>('state', CloudState);
            aMediaFile.UrlMedia := LFileUri;
            aMediaFile.CloudName := CloudName;
            aMediaFile.CloudState := CloudState;
          end;
        finally
          LUploadResponseObj.Free;
        end;
    finally
    end;
  finally
    LHttpClient.Free;
  end;
end;

function TAiGeminiChat.CheckFileState(aMediaFile: TAiMediaFile): String;
var
  LHttpClient: TNetHTTPClient;
  LCheckUrl, CloudState: string;
  LResponse: IHTTPResponse;
  LResponseObj, LFileObj: TJSONObject;
begin
  CloudState := 'UNDEFINED';
  if aMediaFile.CloudName = '' then
    raise Exception.Create('No CloudName');
  LHttpClient := TNetHTTPClient.Create(Nil);
  try
    LCheckUrl := GlAIUrl + aMediaFile.CloudName + '?key=' + Self.ApiKey;
    LResponse := LHttpClient.Get(LCheckUrl);
    if LResponse.StatusCode = 200 then
    begin
      LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      if Assigned(LResponseObj) then
        try
          if LResponseObj.TryGetValue<TJSONObject>('file', LFileObj) then
            LFileObj.TryGetValue<string>('state', CloudState);
        finally
          LResponseObj.Free;
        end;
    end
    else
      CloudState := 'FAILED';
  finally
    LHttpClient.Free;
  end;
  aMediaFile.CloudState := CloudState;
  Result := CloudState;
end;

function TAiGeminiChat.DeleteFile(aMediaFile: TAiMediaFile): String;
var
  LHttpClient: TNetHTTPClient;
  LDeleteUrl: string;
  LResponse: IHTTPResponse;
begin
  Result := '';
  if aMediaFile.CloudName = '' then
    Exit;
  LHttpClient := TNetHTTPClient.Create(Nil);
  try
    LDeleteUrl := GlAIUrl + aMediaFile.CloudName + '?key=' + Self.ApiKey;
    LResponse := LHttpClient.Delete(LDeleteUrl);
    if LResponse.StatusCode = 200 then
      Result := 'DELETED';
  finally
    LHttpClient.Free;
  end;
end;

function TAiGeminiChat.DownLoadFile(aMediaFile: TAiMediaFile): String;
var
  LHttpClient: TNetHTTPClient;
  LDownloadUrl: string;
  LResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
  LFileStream: TMemoryStream;
begin
  Result := 'FAILED';
  if aMediaFile.UrlMedia = '' then
    Exit;
  LHttpClient := TNetHTTPClient.Create(Nil);
  LFileStream := TMemoryStream.Create;
  try
    LDownloadUrl := aMediaFile.UrlMedia;
    LHeaders := [TNetHeader.Create('x-goog-api-key', Self.ApiKey)];
    LResponse := LHttpClient.Get(LDownloadUrl, LFileStream, LHeaders);
    if LResponse.StatusCode = 200 then
    begin
      LFileStream.Position := 0;
      aMediaFile.Content.Clear;
      aMediaFile.Content.CopyFrom(LFileStream, 0);
      aMediaFile.Content.Position := 0;
      if aMediaFile.FileName = '' then
        aMediaFile.FileName := ExtractFileName(TURI.Create(LDownloadUrl).Path);
      Result := 'DOWNLOADED';
    end;
  finally
    LHttpClient.Free;
    LFileStream.Free;
  end;
end;

function TAiGeminiChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer): String;
var
  LHttpClient: TNetHTTPClient;
  LUrl, CacheName, LModel: string;
  LResponse: IHTTPResponse;
  LRequestBody, LJson, LPart, LInlineData: TJSONObject;
  LPartsArray, LContentsArray: TJSonArray;
begin
  Result := '';
  if aMediaFile.Base64 = '' then
    Exit;
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  LHttpClient := TNetHTTPClient.Create(Nil);
  try
    LUrl := Url + 'cachedContents?key=' + Self.ApiKey;
    LRequestBody := TJSONObject.Create;
    try
      LRequestBody.AddPair('model', TJSONString.Create(LModel));
      LRequestBody.AddPair('ttl', TJSONString.Create(aTTL_Seconds.ToString + 's'));
      LContentsArray := TJSonArray.Create;
      LJson := TJSONObject.Create;
      LJson.AddPair('role', TJSONString.Create('user'));
      LPartsArray := TJSonArray.Create;
      LPart := TJSONObject.Create;
      LInlineData := TJSONObject.Create;
      LInlineData.AddPair('mime_type', TJSONString.Create(aMediaFile.MimeType));
      LInlineData.AddPair('data', TJSONString.Create(aMediaFile.Base64));
      LPart.AddPair('inline_data', LInlineData);
      LPartsArray.Add(LPart);
      LJson.AddPair('parts', LPartsArray);
      LContentsArray.Add(LJson);
      LRequestBody.AddPair('contents', LContentsArray);
      LHttpClient.ContentType := 'application/json';
      var
      LBodyStream := TStringStream.Create(LRequestBody.ToJSON, TEncoding.UTF8);
      try
        LResponse := LHttpClient.Post(LUrl, LBodyStream);
      finally
        LBodyStream.Free;
      end;
    finally
      LRequestBody.Free;
    end;
    if LResponse.StatusCode = 200 then
    begin
      var
      LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      if Assigned(LResponseObj) then
        try
          if LResponseObj.TryGetValue<string>('name', CacheName) then
          begin
            aMediaFile.CacheName := CacheName;
            Result := CacheName;
          end;
        finally
          LResponseObj.Free;
        end;
    end;
  finally
    LHttpClient.Free;
  end;
end;

// --- VIDEO GENERATION (VEO) ---
// Implementación basada en predictLongRunning y Polling
function TAiGeminiChat.InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl, LModelName, LOpName, PollingUrl: string;
  LResponse: IHTTPResponse;
  // LHeaders: TNetHeaders;
  LRequest, LParams, LInstance, LImagePart: TJSONObject;
  LInstances: TJSonArray;
  LBodyStream: TStringStream;
  LInitialResponse: TJSONObject;
  I: Integer;
  LKey, LValueStr: string;
  VideoTask: ITask;
  MediaArr: TAiMediaFilesArray;
  // HasImageInput: Boolean;
begin
  Result := '';
  FBusy := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // 1. Preparar el Modelo y la URL
  // Veo usa el endpoint 'predictLongRunning'
  LModelName := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  LUrl := Format('%smodels/%s:predictLongRunning?key=%s', [Self.Url, LModelName, ApiKey]);

  LRequest := TJSONObject.Create;
  try
    // 2. Construir "instances" (Prompt + Imagen opcional)
    LInstances := TJSonArray.Create;
    LRequest.AddPair('instances', LInstances);

    LInstance := TJSONObject.Create;
    LInstances.Add(LInstance);

    // Prompt de texto
    LInstance.AddPair('prompt', TJSONString.Create(AskMsg.Prompt));

    // Input de Imagen (Image-to-Video)
    // HasImageInput := False;
    MediaArr := AskMsg.MediaFiles.GetMediaList([Tfc_Image], False);
    if Length(MediaArr) > 0 then
    begin
      LImagePart := TJSONObject.Create;

      // Preferir URI si ya está subido (File API), si no, usar Base64 (para imágenes pequeñas)
      if (MediaArr[0].UrlMedia <> '') and (MediaArr[0].UrlMedia.StartsWith('https://')) then
      begin
        // Veo acepta 'uri' o 'gcsUri'. Para File API usamos 'uri'
        LImagePart.AddPair('uri', MediaArr[0].UrlMedia);
        // Opcional: mimeType ayuda al modelo
        LImagePart.AddPair('mimeType', MediaArr[0].MimeType);
      end
      else if MediaArr[0].Base64 <> '' then
      begin
        LImagePart.AddPair('bytesBase64Encoded', MediaArr[0].Base64);
        LImagePart.AddPair('mimeType', MediaArr[0].MimeType);
      end;

      if LImagePart.Count > 0 then
      begin
        LInstance.AddPair('image', LImagePart);
        // HasImageInput := True;
      end
      else
        LImagePart.Free;
    end;

    // 3. Construir "parameters" (Configuración de Veo)
    LParams := TJSONObject.Create;

    // Valores por defecto si no existen en VideoParams
    if VideoParams.IndexOfName('aspectRatio') = -1 then
      LParams.AddPair('aspectRatio', '16:9'); // Default seguro

    // Iterar parámetros definidos por el usuario en el componente
    for I := 0 to VideoParams.Count - 1 do
    begin
      LKey := VideoParams.Names[I];
      LValueStr := VideoParams.ValueFromIndex[I];

      var
        LNumInt: Integer;
      var
        LNumFloat: Extended;

        // Lógica específica para strings de Veo que NO deben ser números
      if SameText(LKey, 'aspectRatio') or SameText(LKey, 'resolution') or SameText(LKey, 'personGeneration') then
      begin
        LParams.AddPair(LKey, TJSONString.Create(LValueStr));
      end
      // Detección de tipos para el resto (seed, durationSeconds, etc)
      else if TryStrToInt(LValueStr, LNumInt) then
        LParams.AddPair(LKey, TJSONNumber.Create(LNumInt))
      else if TryStrToFloat(LValueStr, LNumFloat) then
        LParams.AddPair(LKey, TJSONNumber.Create(LNumFloat))
      else if SameText(LValueStr, 'true') then
        LParams.AddPair(LKey, TJSONBool.Create(True))
      else if SameText(LValueStr, 'false') then
        LParams.AddPair(LKey, TJSONBool.Create(False))
      else
        LParams.AddPair(LKey, TJSONString.Create(LValueStr));
    end;

    LRequest.AddPair('parameters', LParams);

    // 4. Enviar Petición Inicial
    FClient.ContentType := 'application/json';
    LBodyStream := TStringStream.Create(LRequest.ToJSON, TEncoding.UTF8);
    try
      // Logging para debug
      LogDebug('Veo Request URL: ' + LUrl);
      LogDebug('Veo Request Body: ' + LRequest.ToJSON);

      LResponse := FClient.Post(LUrl, LBodyStream, nil, []);
    finally
      LBodyStream.Free;
    end;

    if (LResponse.StatusCode <> 200) then
    begin
      FBusy := False;
      var
      ErrMsg := Format('Video Generation Error (Start): %d, %s', [LResponse.StatusCode, LResponse.ContentAsString]);
      LogDebug(ErrMsg);
      DoError(ErrMsg, nil);
      Exit;
    end;

    // 5. Obtener Operation Name (ID de la tarea de larga duración)
    LInitialResponse := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
    try
      if not LInitialResponse.TryGetValue<string>('name', LOpName) then
      begin
        FBusy := False;
        DoError('API Error: No operation name returned in response.', nil);
        Exit;
      end;
    finally
      LInitialResponse.Free;
    end;

    // La URL de polling es base + nombre de operación
    PollingUrl := Url + LOpName + '?key=' + ApiKey;

    // 6. Iniciar Tarea Asíncrona de Polling
    VideoTask := TTask.Run(
      procedure
      var
        TaskClient: TNetHTTPClient;
        TaskResp: IHTTPResponse;
        TaskPollingResponse, TaskFinalResponse, TaskErrorObj: TJSONObject;
        TaskIsDone: Boolean;
        LVideoUri: String;
        LFileStream: TMemoryStream;
      begin
        TaskClient := TNetHTTPClient.Create(Nil);
        TaskIsDone := False;
        TaskFinalResponse := nil;
        LVideoUri := '';

        try
          try
            // Bucle de espera
            while not TaskIsDone do
            begin
              // Esperar 5 segundos entre consultas (Veo tarda ~1-2 min)
              Sleep(5000);

              // Notificar progreso a la UI
              TThread.Queue(nil,
                procedure
                begin
                  if Assigned(FOnReceiveDataEvent) then
                    FOnReceiveDataEvent(Self, ResMsg, nil, 'model', 'Generando video (Veo)... por favor espere.');
                end);

              // Consultar estado
              TaskResp := TaskClient.Get(PollingUrl);

              if TaskResp.StatusCode <> 200 then
              begin
                TThread.Queue(nil,
                  procedure
                  begin
                    DoError('Polling Error: ' + TaskResp.StatusText, nil);
                  end);
                Break;
              end;

              TaskPollingResponse := TJSONObject.ParseJSONValue(TaskResp.ContentAsString) as TJSONObject;
              try
                // Verificar si terminó ("done": true)
                if TaskPollingResponse.TryGetValue<Boolean>('done', TaskIsDone) and TaskIsDone then
                begin
                  // Clonar respuesta final porque TaskPollingResponse se liberará
                  TaskFinalResponse := TaskPollingResponse.Clone as TJSONObject;
                end;
              finally
                TaskPollingResponse.Free;
              end;
            end; // Fin While

            // Procesar Resultado Final
            if Assigned(TaskFinalResponse) then
            begin
              // Verificar errores devueltos por la operación
              if TaskFinalResponse.TryGetValue<TJSONObject>('error', TaskErrorObj) then
              begin
                var
                ErrMsg := TaskErrorObj.GetValue<string>('message', 'Unknown error');
                raise Exception.Create('Veo Operation Failed: ' + ErrMsg);
              end;

              // Extraer URI del video
              // Ruta JSON: response.generateVideoResponse.generatedSamples[0].video.uri
              var
              LVideoResponse := TaskFinalResponse.GetValue<TJSONObject>('response.generateVideoResponse');
              if Assigned(LVideoResponse) then
              begin
                var
                LSamples := LVideoResponse.GetValue<TJSonArray>('generatedSamples');
                if Assigned(LSamples) and (LSamples.Count > 0) then
                begin
                  var
                  LSample0 := LSamples.Items[0] as TJSONObject;
                  LVideoUri := LSample0.GetValue<string>('video.uri', '');
                end;
              end;

              if LVideoUri <> '' then
              begin
                // DESCARGAR EL VIDEO
                LFileStream := TMemoryStream.Create;
                try
                  // --- CORRECCIÓN AQUÍ ---
                  // 1. Preparamos los headers con la API Key
                  var
                    DownloadHeaders: TNetHeaders;
                  DownloadHeaders := [TNetHeader.Create('x-goog-api-key', Self.ApiKey)];

                  // 2. Pasamos los headers en el GET
                  TaskResp := TaskClient.Get(LVideoUri, nil, DownloadHeaders);

                  if TaskResp.StatusCode = 200 then
                  begin
                    LFileStream.CopyFrom(TaskResp.ContentStream, 0);
                    LFileStream.Position := 0;

                    // Actualizar UI en hilo principal
                    TThread.Synchronize(nil,
                      procedure
                      var
                        NewVideoFile: TAiMediaFile;
                      begin
                        NewVideoFile := TAiMediaFile.Create;
                        NewVideoFile.UrlMedia := LVideoUri;

                        // Asignar Stream y MimeType es importante para que el componente visual sepa qué es
                        NewVideoFile.LoadFromStream('veo_generated.mp4', LFileStream);
                        // NewVideoFile.MimeType := 'video/mp4';

                        ResMsg.MediaFiles.Add(NewVideoFile);

                        ResMsg.Content := 'Video generado exitosamente.';
                        FLastContent := ResMsg.Content;
                      end);
                  end
                  else
                    // Ahora verás el mensaje de error real si falla de nuevo
                    raise Exception.CreateFmt('Error downloading generated video: %d %s', [TaskResp.StatusCode, TaskResp.StatusText]);
                finally
                  LFileStream.Free;
                end;
              end
              else
              begin
                raise Exception.Create('API finished but no video URI was found in response.');
              end;
            end;

          except
            On E: Exception do
              TThread.Queue(nil,
                procedure
                begin
                  DoError('Video Generation Task Exception: ' + E.Message, E);
                end);
          end;
        finally
          TaskClient.Free;
          if Assigned(TaskFinalResponse) then
            TaskFinalResponse.Free;

          FBusy := False;

          // Notificación final
          TThread.Queue(nil,
            procedure
            begin
              DoStateChange(acsFinished, 'Done');
              if Assigned(FOnReceiveDataEnd) then
                FOnReceiveDataEnd(Self, ResMsg, nil, 'model', FLastContent);
            end);
        end;
      end); // Fin TTask

    // No hacemos VideoTask.Wait para no bloquear la UI principal.
    // El TTask se ejecutará en segundo plano y notificará vía eventos.

  finally
    LRequest.Free;
  end;
end;

// --- SPEECH GENERATION (TTS) --- [Igual]
function TAiGeminiChat.InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl, LModelName: string;
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LRequestJson, LGenConfigJson, LSpeechConfigObj: TJSONObject;
  LContentsArray, LPartsArray, JResponseModalities: TJSonArray;
  LPartObj, LMessageObj: TJSONObject;
  LResponse: IHTTPResponse;
  LResponseJson: TJSONObject;
  LBase64AudioData, LErrorResponse: string;
  LNewAudioFile: TAiMediaFile;
  LResponseReader: TStreamReader;
  WavStream: TMemoryStream;
  OldAsync: Boolean;
begin
  Result := '';
  FBusy := True;
  FLastError := '';
  FLastContent := '';

  // 1. Obtener Modelo (Asegúrate de que 'gemini-2.5-flash' o similar esté seleccionado en el componente)
  LModelName := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  LUrl := Format('%smodels/%s:generateContent?key=%s', [Self.Url, LModelName, ApiKey]);

  LRequestJson := TJSONObject.Create;
  LBodyStream := nil;
  LResponseStream := TMemoryStream.Create;

  try
    // 2. Construir Payload
    LContentsArray := TJSonArray.Create;
    LMessageObj := TJSONObject.Create;
    LPartsArray := TJSonArray.Create;
    LPartObj := TJSONObject.Create;
    LPartObj.AddPair('text', TJSONString.Create(AskMsg.Prompt));
    LPartsArray.Add(LPartObj);
    LMessageObj.AddPair('role', TJSONString.Create('user'));
    LMessageObj.AddPair('parts', LPartsArray);
    LContentsArray.Add(LMessageObj);
    LRequestJson.AddPair('contents', LContentsArray);

    // Configuración de Generación
    LGenConfigJson := TJSONObject.Create;

    // --- AQUÍ LLAMAMOS A LA NUEVA LÓGICA DE VOCES ---
    LSpeechConfigObj := Self.BuildSpeechConfigJson;
    if Assigned(LSpeechConfigObj) then
      LGenConfigJson.AddPair('speechConfig', LSpeechConfigObj)
    else
    begin
      // Si no hay voz configurada, usamos una por defecto 'Puck' para que no falle
      // O lanzamos error según tu preferencia. Aquí pongo un default seguro.
      var
      LDefaultVoice := TJSONObject.Create;
      var
      LPre := TJSONObject.Create;
      LPre.AddPair('voiceName', 'Puck');
      LDefaultVoice.AddPair('prebuiltVoiceConfig', LPre);
      var
      LConf := TJSONObject.Create;
      LConf.AddPair('voiceConfig', LDefaultVoice);
      LGenConfigJson.AddPair('speechConfig', LConf);
    end;

    // Obligatorio: responseModalities = ["AUDIO"]
    JResponseModalities := TJSonArray.Create;
    JResponseModalities.Add('AUDIO');
    LGenConfigJson.AddPair('responseModalities', JResponseModalities);

    LRequestJson.AddPair('generationConfig', LGenConfigJson);

    // 3. Ejecutar Request
    LBodyStream := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    FClient.ContentType := 'application/json';

    LogDebug('--- Voice Peticion ---');
    LogDebug(LRequestJson.Format);

    // Nos aseguramos que no esté en modo asincrónico y luego lo restauramos
    OldAsync := FClient.Asynchronous;
    FClient.Asynchronous := False;

    Try
      LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, []);
    Finally
      FClient.Asynchronous := OldAsync;
    End;

    LogDebug('--- Voice Response ---');
    LogDebug(LResponse.ContentAsString);

    // 4. Procesar Respuesta
    LResponseStream.Position := 0;
    LResponseReader := TStreamReader.Create(LResponseStream, TEncoding.UTF8);
    try
      LErrorResponse := LResponseReader.ReadToEnd;
    finally
      LResponseReader.Free;
    end;

    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSONObject.ParseJSONValue(LErrorResponse) as TJSONObject;
      try
        if Assigned(LResponseJson) then
        begin
          LBase64AudioData := LResponseJson.GetValue<string>('candidates[0].content.parts[0].inlineData.data', '');

          if not LBase64AudioData.IsEmpty then
          begin
            LNewAudioFile := TAiMediaFile.Create;
            try
              // Cargar PCM crudo (Base64)
              LNewAudioFile.LoadFromBase64('generated_audio.pcm', LBase64AudioData);

              // Convertir a WAV (24kHz, Mono, 16bit es el estándar de Gemini TTS)
              WavStream := Nil;
              if ConvertPCMStreamToWAVStream(LNewAudioFile.Content, WavStream, 24000, 1, 16) then
              begin
                try
                  LNewAudioFile.Clear; // Borrar PCM
                  WavStream.Position := 0;
                  LNewAudioFile.LoadFromStream('generated_audio.wav', WavStream);
                  // IMPORTANTE: Definir MimeType para que el reproductor sepa qué es
                  // (Aunque la clase base suele inferirlo por extensión, es buena práctica)
                  // LNewAudioFile.MimeType := 'audio/wav';
                finally
                  WavStream.Free;
                end;
              end;

              ResMsg.MediaFiles.Add(LNewAudioFile);

              if Assigned(FOnReceiveDataEnd) then
                FOnReceiveDataEnd(Self, ResMsg, nil, 'model', 'Audio generated successfully');
            except
              LNewAudioFile.Free;
              raise;
            end;
          end
          else
          begin
            // A veces Gemini devuelve finishReason: SAFETY sin datos
            DoError('No audio data received. Check safety settings or prompt.', nil);
          end;
        end;
      finally
        LResponseJson.Free;
      end;
    end
    else
    begin
      FLastError := LErrorResponse;
      DoError(Format('Error Audio Generation: %d - %s', [LResponse.StatusCode, FLastError]), nil);
    end;

  finally
    LRequestJson.Free;
    LBodyStream.Free;
    LResponseStream.Free;
    FBusy := False;
  end;
end;


// [V3 UPDATE] Implementación de Image Generation específica para V3 (si es diferente)
// Gemini 3 Pro Image usa generateContent con imageConfig, a diferencia de Veo.
// Como el método InternalRunImageGeneration actualmente deriva a Completions,
// debemos asegurarnos de que InitChatCompletions soporte imageConfig.
// Por ahora, InternalRunCompletions cubre Gemini 3 Image si se pasan los params correctos en CustomParams si fuese necesario,
// pero aquí añadimos soporte básico si el modelo es gemini-3-pro-image-preview.

function TAiGeminiChat.InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String;
begin
  // Gemini 3 usa generateContent, igual que texto.
  Result := InternalRunCompletions(ResMsg, AskMsg);
end;

function TAiGeminiChat.InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
begin
  Result := '';
end;

function TAiGeminiChat.InternalRunImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
begin
  Result := '';
end;

// LogDebug('--OnInternalReceiveData--');
// LogDebug(FResponse.DataString);

procedure TAiGeminiChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  S, JsonStr, sText, sThoughtContent, sFinishReason: String;
  P_End, Level, I: Integer;
  jObj, LContent, LUso, jFuncCall: TJSONObject;
  LCandidates, LParts: TJSonArray;
  BytesBuffer: TBytes;
  JThoughtVal: TJSONValue;
  IsThought, IsStreamFinished: Boolean;
  LPartSig: String;
  InString, Escape: Boolean; // Para el parser inteligente

  // Variables para la lógica de finalización
  ResMsg, AskMsg: TAiChatMessage;
  JArrParts: TJSonArray;
  Wrapper: TJSONObject;
  Keys: TArray<Integer>;
  K: Integer;
  PartObj, jInlineData, jArgs: TJSONObject;
  LNewMediaFile: TAiMediaFile;
  LBase64Data, LMimeType, LSig: String;
  HasTools: Boolean;
  LFunciones: TAiToolsFunctions;
  LFunction: TAiToolsFunction;
  LName, LArgsStr: String;
  LModelVersion: String;
begin
  if (not FClient.Asynchronous) or AAbort then
    Exit;

  LogDebug('--OnInternalReceiveData--');
  LogDebug(FResponse.DataString);

  // 1. LECTURA DEL STREAM Y ACUMULACIÓN EN BUFFER
  if FResponse.Size > 0 then
  begin
    FResponse.Position := 0;
    SetLength(BytesBuffer, FResponse.Size);
    FResponse.ReadBuffer(BytesBuffer[0], FResponse.Size);
    S := TEncoding.UTF8.GetString(BytesBuffer);
    FTmpResponseText := FTmpResponseText + S;
    FResponse.Size := 0;
    FResponse.Position := 0;
  end;

  // 2. BUCLE DE PROCESAMIENTO
  // Procesamos mientras haya datos suficientes en el buffer
  while True do
  begin
    // A. Limpieza de basura inicial (comas, corchetes, espacios de chunks anteriores)
    while (FTmpResponseText <> '') and (FTmpResponseText[1] <= ' ') do // Espacios, saltos de linea
      Delete(FTmpResponseText, 1, 1);

    while (FTmpResponseText <> '') and ((FTmpResponseText[1] = ',') or (FTmpResponseText[1] = '[')) do
      Delete(FTmpResponseText, 1, 1);

    if (FTmpResponseText = '') or (FTmpResponseText[1] <> '{') then
      Break; // Esperar más datos o buffer vacío

    // B. Buscar el final del objeto JSON (Parser Inteligente)
    // Debemos ignorar llaves {} que estén dentro de cadenas (Quotes)
    Level := 0;
    P_End := 0;
    InString := False;
    Escape := False;

    for I := 1 to Length(FTmpResponseText) do
    begin
      // Manejo de cadenas para no contar llaves falsas
      if (FTmpResponseText[I] = '"') and (not Escape) then
        InString := not InString;

      if InString then
      begin
        // Detectar escape para la siguiente iteración (ej: \")
        if FTmpResponseText[I] = '\' then
          Escape := not Escape // Toggle por si es doble barra \\
        else
          Escape := False;
      end
      else
      begin
        // Fuera de cadena, contamos llaves
        Escape := False; // Reset
        if FTmpResponseText[I] = '{' then
          Inc(Level)
        else if FTmpResponseText[I] = '}' then
        begin
          Dec(Level);
          if Level = 0 then
          begin
            P_End := I;
            Break; // Encontramos el cierre del objeto principal
          end;
        end;
      end;
    end;

    // Si no encontramos el final (Level > 0 o P_End = 0), salimos y esperamos el siguiente chunk
    if P_End = 0 then
      Break;

    // C. Extracción y Parseo Seguro
    JsonStr := Copy(FTmpResponseText, 1, P_End);

    // Intentamos parsear. Si falla, podría ser un JSON malformado, pero asumimos que Gemini envía bien.
    // Lo importante es NO borrar del buffer hasta confirmar que parseó bien.
    jObj := TJSONObject.ParseJSONValue(JsonStr) as TJSONObject;

    if not Assigned(jObj) then
    begin
      // Raro: Estructura parecía bien balanceada pero falló el parseo.
      // Opcion: Borrar este bloque corrupto para no bloquear, o esperar.
      // En este caso, asumimos corrupción y avanzamos para no colgar el bucle.
      DoError('JSON Parse Error en chunk asíncrono', nil);
      Delete(FTmpResponseText, 1, P_End);
      Continue;
    end;

    // PARSEO EXITOSO: Ahora sí borramos del buffer
    Delete(FTmpResponseText, 1, P_End);

    try

      jObj.TryGetValue<string>('modelVersion', LModelVersion);

      IsStreamFinished := False;

      // D. Procesar Candidatos
      if jObj.TryGetValue<TJSonArray>('candidates', LCandidates) and (LCandidates.Count > 0) then
      begin
        var
        LCandidate := LCandidates.Items[0] as TJSONObject;

        // 1. Detección de Finalización (FinishReason)
        if LCandidate.TryGetValue<string>('finishReason', sFinishReason) then
        begin
          if (sFinishReason <> '') and (sFinishReason <> 'null') then
            IsStreamFinished := True;
        end;

        // 2. Procesar Contenido
        if LCandidate.TryGetValue<TJSONObject>('content', LContent) and LContent.TryGetValue<TJSonArray>('parts', LParts) then
        begin
          for var jValPart in LParts do
          begin
            var
            LPartObj := jValPart as TJSONObject;

            // [BUFFER] Guardar partes estructurales (Tools, Code, Signatures)
            if (LPartObj.GetValue('functionCall') <> nil) or (LPartObj.GetValue('executableCode') <> nil) or (LPartObj.GetValue('codeExecutionResult') <> nil) or (LPartObj.GetValue('inlineData') <> nil) or
              (LPartObj.TryGetValue<string>('thoughtSignature', LPartSig)) then
            begin
              // Clonamos para asegurar persistencia
              FTmpToolCallBuffer.Add(FTmpToolCallBuffer.Count, LPartObj.Clone as TJSONObject);
            end;

            // [TEXTO/THINKING]
            IsThought := False;
            sThoughtContent := '';

            if LPartObj.TryGetValue('thought', JThoughtVal) then
            begin
              if (JThoughtVal is TJSONBool) and (TJSONBool(JThoughtVal).AsBoolean) then
                IsThought := True
              else if (JThoughtVal is TJSONString) then
              begin
                IsThought := True;
                sThoughtContent := JThoughtVal.Value;
              end;
            end;

            if LPartObj.TryGetValue<string>('text', sText) then
            begin
              if IsThought or (sThoughtContent <> '') then
              begin
                if Assigned(OnReceiveThinking) then
                  OnReceiveThinking(Self, nil, jObj, 'model', IfThen(sThoughtContent <> '', sThoughtContent, sText));

                // Fix visual: si trae texto real mezclado con flag de thought (raro pero posible)
                if (not IsThought) and (sText <> '') then
                begin
                  FLastContent := FLastContent + sText;
                  if Assigned(FOnReceiveDataEvent) then
                    FOnReceiveDataEvent(Self, nil, jObj, 'model', sText);
                end;
              end
              else
              begin
                // Texto normal al usuario
                if sText <> '' then
                begin
                  FLastContent := FLastContent + sText;
                  if Assigned(FOnReceiveDataEvent) then
                    FOnReceiveDataEvent(Self, nil, jObj, 'model', sText);
                end;
              end;
            end
            else if (sThoughtContent <> '') and Assigned(OnReceiveThinking) then
            begin
              OnReceiveThinking(Self, nil, jObj, 'model', sThoughtContent);
            end;
          end;
        end;
      end;

      // E. Actualizar Tokens
      if jObj.TryGetValue<TJSONObject>('usageMetadata', LUso) then
      begin
        var
          tmpPrompt, tmpCand, tmpTotal, tmpThought: Integer;
        if LUso.TryGetValue<Integer>('promptTokenCount', tmpPrompt) then
          Self.Prompt_tokens := tmpPrompt;
        if LUso.TryGetValue<Integer>('candidatesTokenCount', tmpCand) then
          Self.Completion_tokens := tmpCand;
        if LUso.TryGetValue<Integer>('totalTokenCount', tmpTotal) then
          Self.Total_tokens := tmpTotal;
        if LUso.TryGetValue<Integer>('thoughtsTokenCount', tmpThought) then
          Self.Thinking_tokens := tmpThought;
      end;

      // -----------------------------------------------------------------------
      // 3. FINALIZACIÓN IN-PLACE
      // Aquí ejecutamos lo que antes hacía InternalCompleteRequest
      // -----------------------------------------------------------------------
      if IsStreamFinished then
      begin
        FBusy := False;
        AskMsg := GetLastMessage;

        // Crear mensaje de respuesta final
        ResMsg := TAiChatMessage.Create(FLastContent, 'model');
        ResMsg.Prompt_tokens := Self.Prompt_tokens;
        ResMsg.Completion_tokens := Self.Completion_tokens;
        ResMsg.Total_tokens := Self.Total_tokens;
        ResMsg.Thinking_tokens := Self.Thinking_tokens;

        if LModelVersion <> '' then
          ResMsg.Model := LModelVersion;

        HasTools := False;
        LFunciones := nil;

        // --- RECONSTRUCCIÓN DEL HISTORIAL (GEMINI PARTS / FIRMAS) ---
        if FTmpToolCallBuffer.Count > 0 then
        begin
          JArrParts := TJSonArray.Create;
          Keys := FTmpToolCallBuffer.Keys.ToArray;
          TArray.Sort<Integer>(Keys);

          LFunciones := TAiToolsFunctions.Create;

          for K in Keys do
          begin
            PartObj := FTmpToolCallBuffer[K];

            // Recuperar Firmas (V3)
            if PartObj.TryGetValue<string>('thoughtSignature', LSig) then
              AddThoughtSignature(ResMsg, LSig);

            // Detectar Function Call
            if PartObj.TryGetValue<TJSONObject>('functionCall', jFuncCall) then
            begin
              HasTools := True;
              LFunction := TAiToolsFunction.Create;
              jFuncCall.TryGetValue<string>('name', LName);
              LFunction.Name := LName;

              if jFuncCall.TryGetValue<TJSONObject>('args', jArgs) then
                LArgsStr := jArgs.ToJSON
              else
                LArgsStr := '{}';

              LFunction.Arguments := LArgsStr;

              if Assigned(jArgs) then
                for var ArgIdx := 0 to jArgs.Count - 1 do
                  LFunction.Params.Values[jArgs.Pairs[ArgIdx].JsonString.Value] := jArgs.Pairs[ArgIdx].JsonValue.Value;

              LFunction.Id := 'call_' + TGuid.NewGuid.ToString;
              LFunction.Tipo := 'function';
              LFunciones.Add(LFunction.Id, LFunction);
            end;

            // Recuperar Inline Data (Archivos Generados)
            if PartObj.TryGetValue<TJSONObject>('inlineData', jInlineData) then
            begin
              jInlineData.TryGetValue<string>('mimeType', LMimeType);
              jInlineData.TryGetValue<string>('data', LBase64Data);

              if (LBase64Data <> '') then
              begin
                LNewMediaFile := TAiMediaFile.Create;
                try
                  var
                  LExt := 'bin';
                  if LMimeType.Contains('png') then
                    LExt := 'png'
                  else if LMimeType.Contains('jpeg') then
                    LExt := 'jpg'
                  else if LMimeType.Contains('wav') then
                    LExt := 'wav';

                  LNewMediaFile.LoadFromBase64('generated.' + LExt, LBase64Data);
                  ResMsg.MediaFiles.Add(LNewMediaFile);
                except
                  LNewMediaFile.Free;
                end;
              end;
            end;

            JArrParts.Add(PartObj);
          end;

          // Empaquetar historial gemini_parts
          Wrapper := TJSONObject.Create;
          try
            Wrapper.AddPair('gemini_parts', JArrParts);
            ResMsg.Tool_calls := Wrapper.ToJSON;
          finally
            Wrapper.Free;
          end;

          FTmpToolCallBuffer.Clear;
        end;

        // --- BRANCH: HERRAMIENTAS VS TEXTO FINAL ---
        if HasTools then
        begin
          ResMsg.Id := FMessages.Count + 1;
          FMessages.Add(ResMsg);

          if Assigned(FOnAddMessage) then
            FOnAddMessage(Self, ResMsg, nil, ResMsg.Role, '');

          // Ejecutar Tools en Tarea separada
          TTask.Run(
            procedure
            var
              LocalFuncs: TAiToolsFunctions;
              LocalTasks: array of ITask;
              LocalFn: TAiToolsFunction;
              TaskIdx: Integer;
            begin
              LocalFuncs := LFunciones;
              try
                SetLength(LocalTasks, LocalFuncs.Count);
                TaskIdx := 0;
                for var LocalKey in LocalFuncs.Keys do
                begin
                  LocalFn := LocalFuncs[LocalKey];
                  LocalFn.ResMsg := ResMsg;
                  LocalFn.AskMsg := AskMsg;

                  LocalTasks[TaskIdx] := TTask.Create(
                    procedure
                    begin
                      try
                        DoCallFunction(LocalFn);
                      except
                        on E: Exception do
                          TThread.Queue(nil,
                            procedure
                            begin
                              DoError('Error Tool: ' + LocalFn.Name, E);
                            end);
                      end;
                    end);
                  LocalTasks[TaskIdx].Start;
                  Inc(TaskIdx);
                end;

                TTask.WaitForAll(LocalTasks);

                TThread.Synchronize(nil,
                  procedure
                  begin
                    for var LocalKey in LocalFuncs.Keys do
                    begin
                      LocalFn := LocalFuncs[LocalKey];
                      var
                      ToolMsg := TAiChatMessage.Create(LocalFn.Response, 'tool', LocalFn.Id, LocalFn.Name);
                      ToolMsg.Id := FMessages.Count + 1;
                      FMessages.Add(ToolMsg);
                    end;
                  end);

                // RECURSIÓN
                TThread.Queue(nil,
                  procedure
                  begin
                    Self.Run(nil, ResMsg);
                  end);

              finally
                LocalFuncs.Free;
              end;
            end);
        end
        else
        begin
          // Flujo Normal (Texto/JSON Final)
          if Assigned(LFunciones) then
            LFunciones.Free;

          ResMsg.Id := FMessages.Count + 1;
          FMessages.Add(ResMsg);

          if tfc_ExtracttextFile in NativeOutputFiles then
            InternalExtractCodeFiles(FLastContent, ResMsg);

          DoStateChange(acsFinished, 'Done');
          if Assigned(FOnReceiveDataEnd) then
            FOnReceiveDataEnd(Self, ResMsg, nil, 'model', FLastContent);
        end;

        // FINALIZAR: Cortar conexión HTTP
        AAbort := True;
        jObj.Free;
        Exit;
      end;

    finally
      // jObj.Free;
    end;
  end;
end;

procedure TAiGeminiChat.OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse);
begin
  // Si ya se procesó el final en el stream (finishReason detectado), FBusy será False.
  if not FBusy then
    Exit;

  // Si llegamos aquí, el stream terminó HTTP-wise pero no detectamos finishReason antes.
  // Forzamos el cierre.
  InternalCompleteRequest;

  // Llamamos al padre por si acaso tiene lógica genérica
  inherited OnRequestCompletedEvent(Sender, aResponse);
end;

{ TAiMediaFileGemini }

procedure TAiMediaFileGemini.DownloadFileFromUrl(Url: String);
begin
end;

{ TAiGeminiEmbeddings }
// [Embeddings se mantienen igual]

constructor TAiGeminiEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := '@GEMINI_API_KEY';
  Url := GlAIUrl;
  FDimensions := 768;
  FModel := 'text-embedding-004';
end;

function TAiGeminiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
var
  Client: THTTPClient;
  jRequestRoot, jContent, jPart: TJSONObject;
  jParts: TJSonArray;
  jResponseRoot: TJSONObject;
  Res: IHTTPResponse;
  St: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  jRequestRoot := TJSONObject.Create;
  try
    if aModel.IsEmpty then
      aModel := FModel;
    sUrl := FUrl + 'models/' + aModel + ':embedContent?key=' + FApiKey;
    jPart := TJSONObject.Create;
    jPart.AddPair('text', TJSONString.Create(aInput));
    jParts := TJSonArray.Create;
    jParts.AddElement(jPart);
    jContent := TJSONObject.Create;
    jContent.AddPair('parts', jParts);
    jRequestRoot.AddPair('model', TJSONString.Create('models/' + aModel));
    jRequestRoot.AddPair('content', jContent);
    if aDimensions > 0 then
      jRequestRoot.AddPair('outputDimensionality', TJSONNumber.Create(aDimensions));

    St := TStringStream.Create(jRequestRoot.ToString, TEncoding.UTF8);
    try
      Client.ContentType := 'application/json';
      Res := Client.Post(sUrl, St);
    finally
      St.Free;
    end;

    if Res.StatusCode = 200 then
    begin
      jResponseRoot := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      try
        ParseEmbedding(jResponseRoot);
        Result := Self.FData;
      finally
        jResponseRoot.Free;
      end;
    end
    else
      Raise Exception.CreateFmt('Error Gemini Embeddings: %d - %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    jRequestRoot.Free;
    Client.Free;
  end;
end;

procedure TAiGeminiEmbeddings.ParseEmbedding(jObj: TJSONObject);
var
  LEmbeddingObj: TJSONObject;
  LValuesArray: TJSonArray;
  Emb: TAiEmbeddingData;
  I: Integer;
begin
  if not jObj.TryGetValue<TJSONObject>('embedding', LEmbeddingObj) then
    Exit;
  if not LEmbeddingObj.TryGetValue<TJSonArray>('values', LValuesArray) then
    Exit;
  SetLength(Emb, LValuesArray.Count);
  for I := 0 to LValuesArray.Count - 1 do
    Emb[I] := LValuesArray.Items[I].GetValue<Double>;
  FData := Emb;
end;

destructor TAiGeminiEmbeddings.Destroy;
begin
  inherited;
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiGeminiChat);

end.
