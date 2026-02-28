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
// Groq expone un endpoint OpenAI-compatible en https://api.groq.com/openai/v1/
//
// Solo sobrescribe InitChatCompletions para agregar:
//   - reasoning_format / reasoning_effort
//   - Validacion: Raw mode incompatible con Tools/JSON
//   - Optimizacion imagen unica (single image per request)
//   - JSON Schema wrapper {name: 'structured_response', schema: {...}}
//   - max_tokens (no max_completion_tokens, aunque ThinkingLevel este activo)
//
// API key: variable de entorno GROQ_API_KEY (o '@GROQ_API_KEY')
// Modelos vision: llama-3.2-11b-vision-preview (una sola imagen por request)

unit uMakerAi.Chat.Groq;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  uMakerAi.Chat,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  UMakerAi.ParamsRegistry;

const
  GlGroqUrl = 'https://api.groq.com/openai/v1/';

type

  TAiGroqChat = class(TAiChat)
  protected
    function InitChatCompletions: string; override;
  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;
    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
  published
  end;

implementation

{ TAiGroqChat }

class function TAiGroqChat.GetDriverName: string;
begin
  Result := 'Groq';
end;

class procedure TAiGroqChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@GROQ_API_KEY');
  Params.Add('Model=llama-3.1-8b-instant');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=' + GlGroqUrl);
end;

class function TAiGroqChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiGroqChat.Create(Sender);
end;

constructor TAiGroqChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@GROQ_API_KEY';
  Model  := 'llama-3.1-8b-instant';
  Url    := GlGroqUrl;
end;

destructor TAiGroqChat.Destroy;
begin
  inherited;
end;

function TAiGroqChat.InitChatCompletions: string;
var
  AJSONObject, jToolChoice, JResponseFormat, JSchemaWrapper,
  JInnerSchema: TJSONObject;
  JArr       : TJSONArray;
  JStop      : TJSONArray;
  Lista      : TStringList;
  I          : Integer;
  LModel, sTools, sSchema, Res: string;
  LastMsg    : TAiChatMessage;
begin
  if User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'llama-3.1-8b-instant';

  // Validacion: ReasoningFormat='Raw' es incompatible con Tools o JSON mode
  if (ReasoningFormat = 'Raw') and
     (Tool_Active or (Response_format = tiaChatRfJson) or
      (Response_format = tiaChatRfJsonSchema)) then
    raise Exception.Create(
      'Groq Error: ReasoningFormat no puede ser "raw" cuando se usan ' +
      'Tools o JSON mode. Use "parsed" o "hidden".');

  AJSONObject := TJSONObject.Create;
  Lista       := TStringList.Create;
  try
    AJSONObject.Add('stream', TJSONBoolean.Create(Asynchronous));

    // Tools
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

    // Mensajes — Groq: si el ultimo mensaje tiene imagenes, solo enviar ese
    // (limitacion del modelo vision: una imagen por request)
    LastMsg := Messages.Last;
    if Assigned(LastMsg) and (LastMsg.MediaFiles.Count > 0) then
      AJSONObject.Add('messages', LastMsg.ToJson)
    else
      AJSONObject.Add('messages', GetMessages);

    AJSONObject.Add('model', LModel);

    // reasoning_format (string directo: 'parsed', 'raw', 'hidden')
    if ReasoningFormat <> '' then
      AJSONObject.Add('reasoning_format', ReasoningFormat);

    // reasoning_effort (basado en ThinkingLevel, como campo raiz — no como objeto)
    case ThinkingLevel of
      tlLow:    AJSONObject.Add('reasoning_effort', 'low');
      tlMedium: AJSONObject.Add('reasoning_effort', 'medium');
      tlHigh:   AJSONObject.Add('reasoning_effort', 'high');
    end;

    // Temperatura con truncado a 2 decimales
    AJSONObject.Add('temperature',
        TJSONFloatNumber.Create(Trunc(Temperature * 100) / 100));

    // max_tokens (Groq siempre usa max_tokens, no max_completion_tokens)
    AJSONObject.Add('max_tokens', TJSONIntegerNumber.Create(Max_tokens));

    if Top_p <> 0 then
      AJSONObject.Add('top_p', TJSONFloatNumber.Create(Top_p));

    AJSONObject.Add('frequency_penalty',
        TJSONFloatNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    AJSONObject.Add('presence_penalty',
        TJSONFloatNumber.Create(Trunc(Presence_penalty * 100) / 100));
    AJSONObject.Add('user', User);
    AJSONObject.Add('n', TJSONIntegerNumber.Create(N));

    // response_format — Groq necesita wrapper especifico para json_schema
    if Response_format = tiaChatRfJsonSchema then
    begin
      JResponseFormat := TJSONObject.Create;
      JResponseFormat.Add('type', 'json_schema');

      sSchema := JsonSchema.Text;
      if sSchema <> '' then
      begin
        sSchema := StringReplace(sSchema, '\n', ' ', [rfReplaceAll]);
        JInnerSchema := TJSONObject(GetJSON(sSchema));
        if Assigned(JInnerSchema) then
        begin
          // Wrapper estilo OpenAI/Groq: {name, schema}
          JSchemaWrapper := TJSONObject.Create;
          JSchemaWrapper.Add('name', 'structured_response');
          JSchemaWrapper.Add('schema', JInnerSchema);
          JResponseFormat.Add('json_schema', JSchemaWrapper);
        end;
      end;

      AJSONObject.Add('response_format', JResponseFormat);
    end
    else if Response_format = tiaChatRfJson then
    begin
      JResponseFormat := TJSONObject.Create;
      JResponseFormat.Add('type', 'json_object');
      AJSONObject.Add('response_format', JResponseFormat);
    end
    else if Response_format = tiaChatRfText then
    begin
      JResponseFormat := TJSONObject.Create;
      JResponseFormat.Add('type', 'text');
      AJSONObject.Add('response_format', JResponseFormat);
    end;

    // Stop words
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      JStop := TJSONArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.Add('stop', JStop);
    end;

    // Logprobs
    if Logprobs then
    begin
      if Logit_bias <> '' then
        AJSONObject.Add('logit_bias',
            TJSONIntegerNumber.Create(StrToIntDef(Logit_bias, 0)));
      AJSONObject.Add('logprobs', TJSONBoolean.Create(Logprobs));
      if Top_logprobs <> '' then
        AJSONObject.Add('top_logprobs',
            TJSONIntegerNumber.Create(StrToIntDef(Top_logprobs, 0)));
    end;

    if Seed > 0 then
      AJSONObject.Add('seed', TJSONIntegerNumber.Create(Seed));

    Res := AJSONObject.AsJSON;
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  finally
    AJSONObject.Free;
    Lista.Free;
  end;
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiGroqChat);

end.
