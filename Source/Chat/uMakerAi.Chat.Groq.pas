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

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// Modelos con vision actualmente en Groq (Abr 2026):
//   meta-llama/llama-4-scout-17b-16e-instruct  (131K ctx, 8K output, vision + tools)
//   openai/gpt-oss-120b                        (131K ctx, 65K output, vision + reasoning)
// Limites de vision en Groq:
//   - Imagen maxima por URL: 20MB | por base64: 4MB
//   - Maximo 5 imagenes por request (llama-4-scout)

unit uMakerAi.Chat.Groq;

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
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Embeddings, uMakerAi.Core, uMakerAi.Embeddings.Core, uMakerAi.Chat.Messages;

Type
  // Este modelo de reasoning por ahora solo se ha detectado en Groq, as� que se implementa solo aqu�

  TAiReasoningFormat = (rfAuto, rfParsed, rfRaw, rfHidden);
  TAiReasoningEffort = (reAuto, reNone, reDefault);

  TAiGroqChat = Class(TAiChat)
  Private
    FReasoningFormat: TAiReasoningFormat;
    FReasoningEffort: TAiReasoningEffort;
  Protected
    Function InitChatCompletions: String; Override;
    Function InternalRunNativeTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
  End;

  TAiGroqEmbeddings = Class(TAiEmbeddings)
  Public
    // groq actualmente no maneja modelos de embeddings
    Function CreateEmbedding(Input, User: String; Dimensions: Integer = 1536; Model: String = 'Llama3-8b-8192'; EncodingFormat: String = 'float'): TAiEmbeddingData; Override;
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.groq.com/openai/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGroqChat]);
end;

{ TAiOllamaChat }

class function TAiGroqChat.GetDriverName: string;
Begin
  Result := 'Groq';
End;

class procedure TAiGroqChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@GROQ_API_KEY');
  Params.Add('Model=llama-3.1-8b-instant');
  Params.Add('Max_Tokens=4096');
  Params.Add('URL=https://api.groq.com/openai/v1/');
End;

class function TAiGroqChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiGroqChat.Create(Sender);
End;

constructor TAiGroqChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@GROQ_API_KEY';
  Model := 'llama-3.1-8b-instant';
  Url := GlAIUrl;
  FReasoningFormat := rfAuto;
  FReasoningEffort := reAuto;
end;

destructor TAiGroqChat.Destroy;
begin

  inherited;
end;

function TAiGroqChat.InitChatCompletions: String;
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
    LModel := 'llama-3.1-8b-instant';

  // Las funciones no trabajan en modo ascincrono
  // LAsincronico := Self.Asynchronous and (not Self.Tool_Active);
  LAsincronico := Self.Asynchronous;

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    if (ModelConfig.Format = 'Raw') and (Tool_Active or (Response_format = tiaChatRfJson) or (Response_format = tiaChatRfJsonSchema)) then
    begin
      Raise Exception.Create('Groq Error: ReasoningFormat no puede ser "raw" cuando se usan Tools o JSON mode. Use "parsed" o "hidden".');
    end;

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    Begin
{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
{$ENDIF}
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools est�n mal definido, debe ser un JsonArray');
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

    // Reasoning: guardado estrictamente por familia de modelos para evitar param leak al cambiar modelo
    // - openai/gpt-oss-*: include_reasoning:true + reasoning_effort (low/medium/high)
    // - qwen/*:           reasoning_format (parsed/raw/hidden) + reasoning_effort (default/none)
    // - otros modelos:    ninguno de estos parametros (causarian error 422)
    if LModel.StartsWith('openai/gpt-oss') then
    begin
      if ModelConfig.ThinkingLevel <> tlDefault then
      begin
        AJSONObject.AddPair('include_reasoning', TJSONBool.Create(True));
        case ModelConfig.ThinkingLevel of
          tlLow:    AJSONObject.AddPair('reasoning_effort', 'low');
          tlMedium: AJSONObject.AddPair('reasoning_effort', 'medium');
          tlHigh:   AJSONObject.AddPair('reasoning_effort', 'high');
        end;
      end;
    end
    else if LModel.StartsWith('qwen/') then
    begin
      if ModelConfig.ThinkingLevel <> tlDefault then
      begin
        // Thinking activo: reasoning_format parsed + reasoning_effort=default
        var LFormat: String := ModelConfig.Format;
        if LFormat = '' then
          LFormat := 'parsed'; // default: reasoning en campo separado message.reasoning
        AJSONObject.AddPair('reasoning_format', LFormat);
        AJSONObject.AddPair('reasoning_effort', 'default');
      end
      else if ModelConfig.Format <> '' then
      begin
        // Format explicitamente seteado sin ThinkingLevel (ej: 'hidden' para non-thinking)
        AJSONObject.AddPair('reasoning_format', ModelConfig.Format);
      end;
    end;
    // Otros modelos (llama, mistral, kimi, etc.): sin params de reasoning

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));

    // Groq docs: reasoning models usan max_completion_tokens (incluye reasoning tokens en el budget)
    if LModel.StartsWith('openai/gpt-oss') or LModel.StartsWith('qwen/') then
      AJSONObject.AddPair('max_completion_tokens', TJSONNumber.Create(Max_tokens))
    else
      AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(Presence_penalty * 100) / 100));
    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(N));

    // 1. JSON Schema (Structured Outputs)
    if (FResponse_format = tiaChatRfJsonSchema) then
    begin
      var
      JResponseFormat := TJSonObject.Create;
      JResponseFormat.AddPair('type', 'json_schema');

      if JsonSchema.Text <> '' then
      begin
        Var sShema := StringReplace(JsonSchema.Text,'\n',' ',[rfReplaceAll]);

        var
        JInnerSchema := TJSonObject.ParseJSONValue(sShema) as TJSonObject;
        if Assigned(JInnerSchema) then
        begin
          // Wrapper para Groq (Estilo OpenAI Classic)
          var
          JSchemaWrapper := TJSonObject.Create;

          // 'name' es OBLIGATORIO en esta estructura
          JSchemaWrapper.AddPair('name', 'structured_response');

          // El esquema va dentro de 'schema'
          JSchemaWrapper.AddPair('schema', JInnerSchema);

          // NOTA: No enviamos "strict": true por defecto para maximizar compatibilidad
          // con modelos Groq que no soportan constrained decoding completo a�n.

          JResponseFormat.AddPair('json_schema', JSchemaWrapper);
        end;
      end;

      AJSONObject.AddPair('response_format', JResponseFormat);
    end

    // 2. JSON Mode (Simple)
    else if (FResponse_format = tiaChatRfJson) then
    begin
      var
      JResponseFormat := TJSonObject.Create;
      JResponseFormat.AddPair('type', 'json_object');
      AJSONObject.AddPair('response_format', JResponseFormat);
    end

    // 3. Text Mode (Solo si se especifica expl�citamente, o dejar por defecto)
    else if (FResponse_format = tiaChatRfText) then
    begin
      var
      JResponseFormat := TJSonObject.Create;
      JResponseFormat.AddPair('type', 'text');
      AJSONObject.AddPair('response_format', JResponseFormat);
    end;

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    // NOTA: Groq no soporta logprobs, logit_bias ni top_logprobs en chat completions (error 400)

    If Seed > 0 then
      AJSONObject.AddPair('seed', TJSONNumber.Create(Seed));

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSon));
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

{ TAiGroqEmbeddings }

function TAiGroqEmbeddings.CreateEmbedding(Input, User: String; Dimensions: Integer; Model, EncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  // OJO OJO OJO OJO
  Raise Exception.Create('Actualmente Groq no maneja modelos de embeddings');

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSonObject.Create;

  Try
    jObj.AddPair('input', Input);
    jObj.AddPair('model', Model);
    jObj.AddPair('user', User);
    jObj.AddPair('encoding_format', EncodingFormat);

    // St.WriteString(UTF8Encode(jObj.Format));
    St.WriteString(jObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);
    Response.Position := 0;

{$IFDEF APIDEBUG}
    Response.SaveToFile('c:\temp\response.txt');
{$ENDIF}
    if Res.StatusCode = 200 then
    Begin
      jObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(jObj);
      Result := Self.FData;

    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    jObj.Free;
  End;
end;

function TAiGroqChat.InternalRunNativeTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  Body: TMultipartFormData;
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
  LResponseStream: TMemoryStream;
  LTempStream: TMemoryStream;
  LResponseObj: TJSonObject;
  Granularities: TStringList;
  I: Integer;
  LModel: String;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo de audio con contenido para la transcripci?n.');

  sUrl := Url + 'audio/transcriptions';
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
  LResponseStream := TMemoryStream.Create;
  Body := TMultipartFormData.Create;
  Granularities := TStringList.Create;
  LTempStream := TMemoryStream.Create;
  try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

    aMediaFile.Content.Position := 0;
    LTempStream.LoadFromStream(aMediaFile.Content);
    LTempStream.Position := 0;

{$IF CompilerVersion >= 35}
    Body.AddStream('file', LTempStream, False, aMediaFile.FileName, aMediaFile.MimeType);
{$ELSE}
    Body.AddStream('file', LTempStream, aMediaFile.FileName, aMediaFile.MimeType);
{$ENDIF}
    Body.AddField('model', LModel);

    if not AskMsg.Prompt.IsEmpty then
      Body.AddField('prompt', AskMsg.Prompt);

    if not TranscriptionParams.ResponseFormat.IsEmpty then
      Body.AddField('response_format', TranscriptionParams.ResponseFormat)
    else
      Body.AddField('response_format', 'json');

    if not TranscriptionParams.Language.IsEmpty then
      Body.AddField('language', TranscriptionParams.Language);

    if Self.Temperature > 0 then
      Body.AddField('temperature', FormatFloat('0.0', Self.Temperature));

    if not TranscriptionParams.TimestampGranularities.IsEmpty then
    begin
      Granularities.CommaText := TranscriptionParams.TimestampGranularities;
      for I := 0 to Granularities.Count - 1 do
        Body.AddField('timestamp_granularities[]', Trim(Granularities[I]));
    end;

    Res := Client.Post(sUrl, Body, LResponseStream, Headers);

    if Res.StatusCode = 200 then
    begin
      LResponseObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      if not Assigned(LResponseObj) then
        LResponseObj := TJSonObject.Create(TJSonPair.Create('text', Res.ContentAsString));
      try
        ParseJsonTranscript(LResponseObj, ResMsg, aMediaFile);
      finally
        LResponseObj.Free;
      end;
      Result := ResMsg.Prompt;
    end
    else
      raise Exception.CreateFmt('Error en la transcripci?n: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  finally
    Body.Free;
    Client.Free;
    LResponseStream.Free;
    LTempStream.Free;
    Granularities.Free;
  end;
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiGroqChat);

end.
