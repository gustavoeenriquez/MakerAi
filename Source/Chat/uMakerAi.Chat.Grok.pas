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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


unit uMakerAi.Chat.Grok;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Embeddings, uMakerAi.Core, uMakerAi.Chat.Messages;

Type

  TAiGrokChat = Class(TAiChat)
  Private
  Protected
    Function InitChatCompletions: String; Override;
    function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;

  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.x.ai/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGrokChat]);
end;

{ TAiGrokChat }

class function TAiGrokChat.GetDriverName: string;
Begin
  Result := 'Grok';
End;

class procedure TAiGrokChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@GROK_API_KEY');
  Params.Add('Model=grok-3');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=https://api.x.ai/v1/');
End;

class function TAiGrokChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiGrokChat.Create(Sender);
End;

constructor TAiGrokChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@GROK_API_KEY';
  Model := 'grok-3';
  Url := GlAIUrl;
end;

destructor TAiGrokChat.Destroy;
begin

  inherited;
end;

function TAiGrokChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res, LModel: String;
begin

  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := 'grok-3';

  LAsincronico := Self.Asynchronous;

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAI).Text) <> '') then
    Begin

{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAI).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAI).Text));
{$ENDIF}
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools est?n mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(Tool_choice) <> '') then
      Begin

{$IF CompilerVersion < 35}
        jToolChoice := TJSONUtils.ParseAsObject(Tool_choice);
{$ELSE}
        jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(Tool_choice));
{$ENDIF}
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tool_choice', jToolChoice);
      End;
    End;

    AJSONObject.AddPair('messages', GetMessages);

    AJSONObject.AddPair('model', LModel);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    if Frequency_penalty <> 0 then
      AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    if Presence_penalty <> 0 then
      AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(Presence_penalty * 100) / 100));

    if ReasoningFormat <> '' then
      AJSONObject.AddPair('reasoning_format', ReasoningFormat); // 'parsed, raw, hidden';

    if ThinkingLevel <> tlDefault then
    begin
      case ThinkingLevel of
        tlLow:
          AJSONObject.AddPair('reasoning_effort', 'low');
        tlMedium:
          AJSONObject.AddPair('reasoning_effort', 'medium');
        tlHigh:
          AJSONObject.AddPair('reasoning_effort', 'high');
      end;
    end;

    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(N));

    if tcm_WebSearch in ChatMediaSupports then
    begin
      Var
      jWebSearchOptions := TJSonObject.Create;
      jWebSearchOptions.AddPair('mode', 'auto');
      // jWebSearchOptions.AddPair('return_citations', 'true');
      AJSONObject.AddPair('search_parameters', jWebSearchOptions);
    end;

    if FResponse_format = tiaChatRfJsonSchema then
    begin
      var JFormatConfig := TJSONObject.Create;
      JFormatConfig.AddPair('type', 'json_schema');
      var sSchema := Trim(JsonSchema.Text);
      if sSchema <> '' then
      begin
        var JSchemaObj := TJSONObject.Create;
        var JInnerSchema := TJSONObject.ParseJSONValue(sSchema) as TJSONObject;
        if Assigned(JInnerSchema) then
        begin
          JSchemaObj.AddPair('schema', JInnerSchema);
          JSchemaObj.AddPair('strict', TJSONBool.Create(True));
          JFormatConfig.AddPair('json_schema', JSchemaObj);
        end
        else
          JSchemaObj.Free;
      end;
      AJSONObject.AddPair('response_format', JFormatConfig);
    end
    else if FResponse_format = tiaChatRfJson then
      AJSONObject.AddPair('response_format', TJSONObject.Create.AddPair('type', 'json_object'));

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    If Logprobs = True then
    Begin
      If Logit_bias <> '' then
        AJSONObject.AddPair('logit_bias', TJSONNumber.Create(Logit_bias));

      AJSONObject.AddPair('logprobs', TJSONBool.Create(Logprobs));

      If Top_logprobs <> '' then
        AJSONObject.AddPair('top_logprobs', TJSONNumber.Create(Top_logprobs));
    End;

    If Seed > 0 then
      AJSONObject.AddPair('seed', TJSONNumber.Create(Seed));

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSON));

    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

// Respuestas API de xAI para búsqueda web
// search_parameters fue deprecado enero 2026 (devuelve 410 "Live search is deprecated").
// Nueva API: POST /v1/responses  con tools=[{type:"web_search"}]
// Formato respuesta: output[].type="message" → content[].type="output_text" → text
function TAiGrokChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
var
  ABody, sUrl, SType, SVal, sRole, sContent: String;
  Res: IHTTPResponse;
  FHeaders: TNetHeaders;
  jReq, jRes, jItem, jContentItem: TJSonObject;
  jInput, jMessages, jOutput, jContent, jTools: TJSonArray;
  jTool: TJSonObject;
  St: TStringStream;
  LModel: String;
  I, K: Integer;
begin
  // Sin web search activo → flujo normal de chat/completions
  if not (tcm_WebSearch in ChatMediaSupports) then
  begin
    Result := inherited InternalRunCompletions(ResMsg, AskMsg);
    Exit;
  end;

  // Web search activo → xAI Agent Tools API (POST /v1/responses)
  FBusy        := True;
  FAbort       := False;
  FLastError   := '';
  FLastContent := '';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'grok-3';

  sUrl := Url;
  if not sUrl.EndsWith('/') then
    sUrl := sUrl + '/';
  sUrl := sUrl + 'responses';

  jInput    := nil;
  jMessages := nil;
  jTools    := nil;
  jTool     := nil;
  jReq      := nil;
  St        := nil;
  try
    // Construir input: mensajes del historial con "system" → "developer"
    jInput    := TJSonArray.Create;
    jMessages := GetMessages;
    for I := 0 to jMessages.Count - 1 do
    begin
      var jOrig := jMessages.Items[I] as TJSonObject;
      var jNew  := TJSonObject.Create;
      jOrig.TryGetValue<String>('role', sRole);
      if sRole = 'system' then
        jNew.AddPair('role', 'developer')
      else
        jNew.AddPair('role', sRole);
      if jOrig.TryGetValue<String>('content', sContent) then
        jNew.AddPair('content', sContent)
      else
      begin
        var jCont: TJSONValue;
        if jOrig.TryGetValue<TJSONValue>('content', jCont) then
          jNew.AddPair('content', TJSONObject.ParseJSONValue(jCont.ToJSON));
      end;
      jInput.Add(jNew);
    end;
    FreeAndNil(jMessages);

    // Construir request body
    jTools := TJSonArray.Create;
    jTool  := TJSonObject.Create;
    jTool.AddPair('type', 'web_search');
    jTools.Add(jTool); jTool := nil;

    jReq := TJSonObject.Create;
    jReq.AddPair('model',  LModel);
    jReq.AddPair('input',  jInput);  jInput := nil;
    jReq.AddPair('tools',  jTools);  jTools := nil;
    jReq.AddPair('stream', TJSONBool.Create(False));

    ABody := jReq.ToJSON;
    St    := TStringStream.Create(ABody, TEncoding.UTF8);

    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType  := 'application/json';
    FClient.Asynchronous := False;
    FResponse.Clear;
    DoStateChange(acsConnecting, 'Enviando búsqueda web...');

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    if Res.StatusCode = 200 then
    begin
      var LParsed := TJSonObject.ParseJSONValue(Res.ContentAsString);
      if not (LParsed is TJSonObject) then
      begin
        LParsed.Free;
        raise Exception.CreateFmt('Respuesta JSON inválida: %s', [Res.ContentAsString]);
      end;
      jRes := TJSonObject(LParsed);
      try
        FLastContent := '';
        if jRes.TryGetValue<TJSonArray>('output', jOutput) then
          for I := 0 to jOutput.Count - 1 do
          begin
            if not (jOutput.Items[I] is TJSonObject) then Continue;
            jItem := jOutput.Items[I] as TJSonObject;
            if not jItem.TryGetValue<String>('type', SType) then Continue;
            if SType = 'message' then
              if jItem.TryGetValue<TJSonArray>('content', jContent) then
                for K := 0 to jContent.Count - 1 do
                begin
                  if not (jContent.Items[K] is TJSonObject) then Continue;
                  jContentItem := jContent.Items[K] as TJSonObject;
                  if jContentItem.TryGetValue<String>('type', SVal) and (SVal = 'output_text') then
                    if jContentItem.TryGetValue<String>('text', SVal) then
                      FLastContent := FLastContent + SVal;
                end;
          end;
        ResMsg.Prompt := FLastContent;
        FBusy         := False;
        Result        := FLastContent;
        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, jRes, 'assistant', FLastContent);
      finally
        FreeAndNil(jRes);
      end;
    end
    else
      raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    FreeAndNil(jMessages);
    FreeAndNil(jInput);
    FreeAndNil(jTools);
    FreeAndNil(jTool);
    FreeAndNil(jReq);
    FreeAndNil(St);
  end;
end;

function TAiGrokChat.InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LBodyJson, LResponseJson, LImageObject: TJSonObject;
  LDataArray: TJSonArray;
  LBodyStream: TStringStream;
  LUrl: String;
  LHeaders: TNetHeaders;
  LResponse: IHTTPResponse;
  LNewMediaFile: TAiMediaFile;
  LImageUrl, LRevisedPrompt, LBase64Data: string;
  LModel: String;
begin
  Result := ''; // La salida principal es el MediaFile en ResMsg
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // 1. Validaciones y configuraci?n
  if AskMsg.Prompt.IsEmpty then
    raise Exception.Create('Se requiere un prompt para generar una imagen.');

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  if LModel = '' then
    LModel := 'grok-2-image'; // Asignar un modelo de imagen por defecto

  LUrl := Url + 'images/generations'; // Url base + endpoint

  // 2. Construir el cuerpo de la petici?n JSON
  LBodyJson := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LBodyJson.AddPair('prompt', AskMsg.Prompt);
    LBodyJson.AddPair('model', LModel);

    if Self.N > 0 then
      LBodyJson.AddPair('n', TJSONNumber.Create(Self.N));

    // if not Self.ImageResponseFormat.IsEmpty then
    // LBodyJson.AddPair('response_format', Self.ImageResponseFormat);

    // 3. Preparar y ejecutar la llamada HTTP
    LBodyStream.WriteString(LBodyJson.ToJSON);
    LBodyStream.Position := 0;
{$IFDEF APIDEBUG}
    LBodyStream.SaveToFile('c:\temp\grok_image_request.json');
    LBodyStream.Position := 0;
{$ENDIF}
    // Grok usa Bearer token para la autorizaci?n
    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';
    FResponse.Clear;

    LResponse := FClient.Post(LUrl, LBodyStream, FResponse, LHeaders);

    // 4. Procesar la respuesta
    FResponse.Position := 0;
    FLastContent := LResponse.ContentAsString(TEncoding.UTF8);
{$IFDEF APIDEBUG}
    FResponse.SaveToFile('c:\temp\grok_image_response.json');
{$ENDIF}
    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSonObject.ParseJSONValue(FLastContent) as TJSonObject;
      try
        if LResponseJson.TryGetValue<TJSonArray>('data', LDataArray) then
        begin
          for var LJsonValue in LDataArray do
          begin
            if not(LJsonValue is TJSonObject) then
              continue;

            LImageObject := LJsonValue as TJSonObject;
            LNewMediaFile := TAiMediaFile.Create;
            try
              // Extraer el prompt revisado y guardarlo (es informaci?n ?til)
              LImageObject.TryGetValue<string>('revised_prompt', LRevisedPrompt);
              LNewMediaFile.Transcription := LRevisedPrompt;
              FLastContent := LRevisedPrompt;
              ResMsg.Prompt := FLastContent;

              // CASO A: La respuesta es una URL
              if LImageObject.TryGetValue<string>('url', LImageUrl) then
              begin
                // Descargamos la imagen desde la URL y la cargamos en el MediaFile
                // Necesitar?s una funci?n para descargar, por ejemplo:
                LNewMediaFile.LoadFromUrl(LImageUrl); // Asumiendo que tienes esta funci?n
              end
              // CASO B: La respuesta es Base64
              else if LImageObject.TryGetValue<string>('b64_json', LBase64Data) then
              begin
                LNewMediaFile.LoadFromBase64('generated_image.png', LBase64Data);
              end;

              // A?adir el MediaFile al mensaje de respuesta
              ResMsg.MediaFiles.Add(LNewMediaFile);
            except
              LNewMediaFile.Free;
              raise;
            end;
          end;
        end;

        // Disparamos el evento de finalizaci?n
        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, LResponseJson, 'model', '');

      finally
        LResponseJson.Free;
      end;
    end
    else
    begin
      FLastError := Format('Error generando imagen con Grok: %d, %s', [LResponse.StatusCode, FLastContent]);
      DoError(FLastError, nil);
    end;

  finally
    LBodyJson.Free;
    LBodyStream.Free;
    FBusy := False;
  end;
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiGrokChat);

end.
