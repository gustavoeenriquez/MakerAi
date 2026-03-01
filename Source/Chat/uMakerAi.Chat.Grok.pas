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
// Driver para xAI Grok (https://api.x.ai/v1/)
//
// Compatible con OpenAI /chat/completions, con las siguientes diferencias:
//   - reasoning_format ('parsed', 'raw', 'hidden') como campo raiz
//   - reasoning_effort ('low', 'medium', 'high') como campo raiz (no objeto anidado)
//   - max_tokens siempre (no max_completion_tokens aunque ThinkingLevel este activo)
//   - Web search: POST /v1/responses con tools=[{type:"web_search"}]
//     (search_parameters fue deprecado en enero 2026 — devuelve 410)
//   - Generacion de imagenes: POST /v1/images/generations (modelo grok-2-image)
//
// Modelos principales:
//   grok-3               — modelo conversacional (chat/completions)
//   grok-3-mini          — variante pequeña
//   grok-2-image         — generacion de imagenes
//
// API key: variable de entorno GROK_API_KEY (o '@GROK_API_KEY')

unit uMakerAi.Chat.Grok;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  fphttpclient, opensslsockets,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Tools.Functions,
  UMakerAi.ParamsRegistry;

type

  // ---------------------------------------------------------------------------
  //  TAiGrokChat — driver xAI Grok
  // ---------------------------------------------------------------------------
  TAiGrokChat = class(TAiChat)
  protected
    function  InitChatCompletions: string; override;
    function  InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string; override;
    function  InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): string; override;

  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
  end;

implementation

const
  GlGrokUrl = 'https://api.x.ai/v1/';

{ ---------------------------------------------------------------------------
  Ciclo de vida
  --------------------------------------------------------------------------- }

constructor TAiGrokChat.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  ApiKey := '@GROK_API_KEY';
  Model  := 'grok-3';
  Url    := GlGrokUrl;
end;

destructor TAiGrokChat.Destroy;
begin
  inherited Destroy;
end;

{ ---------------------------------------------------------------------------
  Metodos de clase
  --------------------------------------------------------------------------- }

class function TAiGrokChat.GetDriverName: string;
begin
  Result := 'Grok';
end;

class procedure TAiGrokChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@GROK_API_KEY');
  Params.Add('Model=grok-3');
  Params.Add('Max_Tokens=4096');
  Params.Add('URL=' + GlGrokUrl);
end;

class function TAiGrokChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiGrokChat.Create(Sender);
end;

{ ---------------------------------------------------------------------------
  InitChatCompletions — construye el JSON del request para Grok

  Diferencias clave respecto a la clase base (TAiChat.InitChatCompletions):
    * reasoning_format: campo raiz string ('parsed', 'raw', 'hidden')
    * reasoning_effort: campo raiz ('low'/'medium'/'high'), NO objeto {reasoning: {effort}}
    * max_tokens siempre (nunca max_completion_tokens)
    * search_parameters legacy (deprecado en ene-2026, se omite — usar web search via
      InternalRunCompletions con el endpoint /v1/responses)
  --------------------------------------------------------------------------- }

function TAiGrokChat.InitChatCompletions: string;
var
  AJSONObject, jToolChoice, JStreamOpts: TJSONObject;
  JArr, JStop             : TJSONArray;
  JFormatConfig, JSchemaObj, JInnerSchema: TJSONObject;
  Lista   : TStringList;
  I       : Integer;
  LModel, sTools, sSchema: string;
begin
  if User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'grok-3';

  AJSONObject := TJSONObject.Create;
  Lista       := TStringList.Create;
  try
    // 1. Streaming
    AJSONObject.Add('stream', TJSONBoolean.Create(Asynchronous));
    if Asynchronous then
    begin
      JStreamOpts := TJSONObject.Create;
      JStreamOpts.Add('include_usage', TJSONBoolean.Create(True));
      AJSONObject.Add('stream_options', JStreamOpts);
    end;

    // 2. Herramientas
    if Tool_Active then
    begin
      sTools := GetToolsStr(tfOpenAI);
      if Trim(sTools) <> '' then
      begin
        JArr := TJSONArray(GetJSON(sTools));
        if not Assigned(JArr) then
          raise Exception.Create(
              'La propiedad Tools esta mal definida, debe ser un JsonArray');
        AJSONObject.Add('tools', JArr);

        if Trim(Tool_choice) <> '' then
        begin
          jToolChoice := TJSONObject(GetJSON(Tool_choice));
          if Assigned(jToolChoice) then
            AJSONObject.Add('tool_choice', jToolChoice);
        end;
      end;
    end;

    // 3. Mensajes
    AJSONObject.Add('messages', GetMessages);

    // 4. Modelo
    AJSONObject.Add('model', LModel);

    // 5. Razonamiento Grok — campos raiz (NO objeto {reasoning: {effort: ...}})
    if ReasoningFormat <> '' then
      AJSONObject.Add('reasoning_format', ReasoningFormat);

    case ThinkingLevel of
      tlLow:    AJSONObject.Add('reasoning_effort', 'low');
      tlMedium: AJSONObject.Add('reasoning_effort', 'medium');
      tlHigh:   AJSONObject.Add('reasoning_effort', 'high');
    end;

    // 6. Parametros numericos — max_tokens siempre (Grok no usa max_completion_tokens)
    AJSONObject.Add('temperature',
        TJSONFloatNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.Add('max_tokens', TJSONIntegerNumber.Create(Max_tokens));

    if Top_p <> 0 then
      AJSONObject.Add('top_p', TJSONFloatNumber.Create(Top_p));
    AJSONObject.Add('frequency_penalty',
        TJSONFloatNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    AJSONObject.Add('presence_penalty',
        TJSONFloatNumber.Create(Trunc(Presence_penalty * 100) / 100));
    AJSONObject.Add('user', User);
    AJSONObject.Add('n', TJSONIntegerNumber.Create(N));

    // 7. Logprobs
    if Logprobs then
    begin
      if Logit_bias <> '' then
        AJSONObject.Add('logit_bias',
            TJSONIntegerNumber.Create(StrToIntDef(Logit_bias, 0)));
      AJSONObject.Add('logprobs', TJSONBoolean.Create(True));
      if Top_logprobs <> '' then
        AJSONObject.Add('top_logprobs',
            TJSONIntegerNumber.Create(StrToIntDef(Top_logprobs, 0)));
    end;

    // 8. Seed
    if Seed > 0 then
      AJSONObject.Add('seed', TJSONIntegerNumber.Create(Seed));

    // 9. Response format
    case Response_format of
      tiaChatRfJsonSchema:
      begin
        JFormatConfig := TJSONObject.Create;
        JFormatConfig.Add('type', 'json_schema');
        sSchema := Trim(JsonSchema.Text);
        if sSchema <> '' then
        begin
          JSchemaObj   := TJSONObject.Create;
          JInnerSchema := TJSONObject(GetJSON(sSchema));
          if Assigned(JInnerSchema) then
          begin
            JSchemaObj.Add('schema', JInnerSchema);
            JSchemaObj.Add('strict', TJSONBoolean.Create(True));
            JFormatConfig.Add('json_schema', JSchemaObj);
          end
          else
            JSchemaObj.Free;
        end;
        AJSONObject.Add('response_format', JFormatConfig);
      end;
      tiaChatRfJson:
      begin
        JFormatConfig := TJSONObject.Create;
        JFormatConfig.Add('type', 'json_object');
        AJSONObject.Add('response_format', JFormatConfig);
      end;
    end;

    // 10. Stop words
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      JStop := TJSONArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.Add('stop', JStop);
    end;

    Result := AJSONObject.AsJSON;
    Result := StringReplace(Result, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Result, '\r\n', '', [rfReplaceAll]);
  finally
    AJSONObject.Free;
    Lista.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  InternalRunCompletions — flujo normal o web search con Responses API

  Si tcm_WebSearch esta activo en ChatMediaSupports, usa la xAI Responses API
  (POST /v1/responses con tools=[{type:"web_search"}]).
  En modo normal delega al inherited (TAiChat.InternalRunCompletions).
  --------------------------------------------------------------------------- }

function TAiGrokChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
var
  ABody     : string;
  sUrl      : string;
  LModel    : string;
  BodyStream: TStringStream;
  RespStream: TStringStream;
  Client    : TFPHTTPClient;
  jReq      : TJSONObject;
  jRes      : TJSONObject;
  jInput    : TJSONArray;
  jMessages : TJSONArray;
  jTools    : TJSONArray;
  jTool     : TJSONObject;
  jOutput   : TJSONArray;
  jContent  : TJSONArray;
  jItem     : TJSONObject;
  jCntItem  : TJSONObject;
  jOrig     : TJSONObject;
  jNew      : TJSONObject;
  JContentVal: TJSONData;
  SType, SRole, SVal: string;
  I, ContentIdx: Integer;
begin
  // Sin web search activo → flujo normal de chat/completions
  if not (tcm_WebSearch in ChatMediaSupports) then
  begin
    Result := inherited InternalRunCompletions(ResMsg, AskMsg);
    Exit;
  end;

  // Web search activo → xAI Responses API (POST /v1/responses)
  FBusy      := True;
  FAbort     := False;
  FLastError := '';
  FLastContent := '';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'grok-3';

  sUrl := Url;
  if (Length(sUrl) > 0) and (sUrl[Length(sUrl)] <> '/') then
    sUrl := sUrl + '/';
  sUrl := sUrl + 'responses';

  BodyStream := TStringStream.Create('');
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  jReq       := nil;
  jInput     := nil;
  jMessages  := nil;
  jTools     := nil;
  jTool      := nil;
  try
    DoStateChange(acsConnecting, 'Enviando busqueda web Grok...');

    // Construir input: mensajes del historial con "system" → "developer"
    jInput    := TJSONArray.Create;
    jMessages := GetMessages;
    try
      for I := 0 to jMessages.Count - 1 do
      begin
        jOrig := TJSONObject(jMessages.Items[I]);
        jNew  := TJSONObject.Create;

        SRole := JGetStr(jOrig, 'role', 'user');
        if SRole = 'system' then
          jNew.Add('role', 'developer')
        else
          jNew.Add('role', SRole);

        // Content: string o array
        JContentVal := jOrig.Find('content');
        if Assigned(JContentVal) then
        begin
          if JContentVal is TJSONString then
            jNew.Add('content', JContentVal.AsString)
          else
            jNew.Add('content', TJSONData(GetJSON(JContentVal.AsJSON)));
        end;

        jInput.Add(jNew);
      end;
    finally
      jMessages.Free;
      jMessages := nil;
    end;

    // Construir tools
    jTools := TJSONArray.Create;
    jTool  := TJSONObject.Create;
    jTool.Add('type', 'web_search');
    jTools.Add(jTool);
    jTool := nil;

    // Request body
    jReq := TJSONObject.Create;
    jReq.Add('model',  LModel);
    jReq.Add('input',  jInput);  jInput := nil;
    jReq.Add('tools',  jTools);  jTools := nil;
    jReq.Add('stream', TJSONBoolean.Create(False));

    ABody := jReq.AsJSON;
    BodyStream.WriteString(ABody);
    BodyStream.Position := 0;

    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    Client.IOTimeout   := ResponseTimeOut;
    Client.RequestBody := BodyStream;
    try
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);

      jRes := TJSONObject(GetJSON(RespStream.DataString));
      if not Assigned(jRes) then
        raise Exception.CreateFmt('Respuesta JSON invalida: %s',
            [RespStream.DataString]);
      try
        FLastContent := '';
        if JTryGetArr(jRes, 'output', jOutput) then
          for I := 0 to jOutput.Count - 1 do
          begin
            if not (jOutput.Items[I] is TJSONObject) then Continue;
            jItem := TJSONObject(jOutput.Items[I]);
            if not JTryGetStr(jItem, 'type', SType) then Continue;
            if SType = 'message' then
              if JTryGetArr(jItem, 'content', jContent) then
                for ContentIdx := 0 to jContent.Count - 1 do
                begin
                  if not (jContent.Items[ContentIdx] is TJSONObject) then Continue;
                  jCntItem := TJSONObject(jContent.Items[ContentIdx]);
                  if JTryGetStr(jCntItem, 'type', SVal) and (SVal = 'output_text') then
                    if JTryGetStr(jCntItem, 'text', SVal) then
                      FLastContent := FLastContent + SVal;
                end;
          end;

        ResMsg.Prompt := FLastContent;
        Result        := FLastContent;
        DoDataEnd(ResMsg, 'assistant', FLastContent, jRes);
      finally
        jRes.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        DoError('Error en busqueda web Grok: ' + E.Message, E);
        Result := '';
      end;
    end;
  finally
    jReq.Free;
    jInput.Free;
    jMessages.Free;
    jTools.Free;
    jTool.Free;
    BodyStream.Free;
    RespStream.Free;
    Client.Free;
    FBusy := False;
  end;
end;

{ ---------------------------------------------------------------------------
  InternalRunImageGeneration — genera imagenes via /v1/images/generations
  Responde con URL o base64 (b64_json) para cada imagen generada.
  --------------------------------------------------------------------------- }

function TAiGrokChat.InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): string;
var
  sUrl      : string;
  LModel    : string;
  BodyStream: TStringStream;
  RespStream: TStringStream;
  Client    : TFPHTTPClient;
  LBodyJson : TJSONObject;
  LRespJson : TJSONObject;
  LDataArr  : TJSONArray;
  LImgObj   : TJSONObject;
  LImgData  : TJSONData;
  NewMedia  : TAiMediaFile;
  LImageUrl, LBase64, LRevised: string;
  I         : Integer;
begin
  Result := '';
  if AskMsg.Prompt = '' then
    raise Exception.Create('Se requiere un prompt para generar una imagen con Grok.');

  FBusy      := True;
  FAbort     := False;
  FLastError := '';
  FLastContent := '';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'grok-2-image';

  sUrl := Url;
  if (Length(sUrl) > 0) and (sUrl[Length(sUrl)] <> '/') then
    sUrl := sUrl + '/';
  sUrl := sUrl + 'images/generations';

  LBodyJson  := TJSONObject.Create;
  BodyStream := TStringStream.Create('');
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  try
    // Construir body
    LBodyJson.Add('prompt', AskMsg.Prompt);
    LBodyJson.Add('model', LModel);
    if N > 0 then
      LBodyJson.Add('n', TJSONIntegerNumber.Create(N));

    BodyStream.WriteString(LBodyJson.AsJSON);
    BodyStream.Position := 0;

    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    Client.IOTimeout   := ResponseTimeOut;
    Client.RequestBody := BodyStream;
    try
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);

      LRespJson := TJSONObject(GetJSON(RespStream.DataString));
      if not Assigned(LRespJson) then
        raise Exception.CreateFmt('Respuesta JSON invalida de /images/generations: %s',
            [RespStream.DataString]);
      try
        if JTryGetArr(LRespJson, 'data', LDataArr) then
          for I := 0 to LDataArr.Count - 1 do
          begin
            LImgData := LDataArr.Items[I];
            if not (LImgData is TJSONObject) then Continue;
            LImgObj := TJSONObject(LImgData);

            NewMedia := TAiMediaFile.Create;
            try
              // Prompt revisado
              LRevised := '';
              if JTryGetStr(LImgObj, 'revised_prompt', LRevised) then
              begin
                NewMedia.Transcription := LRevised;
                FLastContent := LRevised;
                ResMsg.Prompt := FLastContent;
              end;

              // Caso A: URL de imagen
              LImageUrl := '';
              if JTryGetStr(LImgObj, 'url', LImageUrl) and (LImageUrl <> '') then
                NewMedia.LoadFromUrl(LImageUrl)
              // Caso B: base64
              else if JTryGetStr(LImgObj, 'b64_json', LBase64) and (LBase64 <> '') then
                NewMedia.LoadFromBase64('generated_image.png', LBase64);

              ResMsg.MediaFiles.Add(NewMedia);
            except
              NewMedia.Free;
              raise;
            end;
          end;

        DoDataEnd(ResMsg, 'model', '', LRespJson);
      finally
        LRespJson.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := 'Error generando imagen con Grok: ' + E.Message;
        DoError(FLastError, E);
      end;
    end;
  finally
    LBodyJson.Free;
    BodyStream.Free;
    RespStream.Free;
    Client.Free;
    FBusy := False;
  end;
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiGrokChat);

end.
