unit uMakerAi.Chat.Groq;
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

// A Octubre del 2024 estas son las limitaciones de visi�n de groq
{
  Model ID: llama-3.2-11b-vision-preview
  Description: Llama 3.2 11B Vision is a powerful multimodal model capable of processing both text and image inputs. It supports multilingual, multi-turn conversations, tool use, and JSON mode.
  Context Window: 8,192 tokens
  Limitations:
  Preview Model: Currently in preview and should be used for experimentation.
  Image Size Limit: The maximum allowed size for a request containing an image URL as input is 20MB. Requests larger than this limit will return a 400 error.
  Single Image per Request: Only one image can be processed per request in the preview release. Requests with multiple images will return a 400 error.
  System Prompt: The model does not support system prompts and images in the same request.
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client, uMakerAi.Chat, uMakerAi.Embeddings;

Type

  TAiGroqEmbeddings = Class(TAiEmbeddings)
  Public
    // groq actualmente no maneja modelos de embeddings
    Function CreateEmbedding(Input, User: String; Dimensions: Integer = 1536; Model: String = 'Llama3-8b-8192'; EncodingFormat: String = 'float'): TAiEmbeddingData; Override;
  End;

  TAiGroqChat = Class(TAiChat)
  Private
  Protected
    Function InitChatCompletions: String; Override;
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
  Published
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

constructor TAiGroqChat.Create(Sender: TComponent);
begin
  inherited;
  Model := 'Llama3-8b-8192';
  Url := GlAIUrl;
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
  LastMsg: TAiChatMessage;
  Res: String;
begin

  If User = '' then
    User := 'user';

  If Model = '' then
    Model := 'llama-3.2-11b-text-preview';

  // Las funciones no trabajan en modo ascincrono
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  // En groq hay una restricci�n sobre las im�genes

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(Tools.Text) <> '') then
    Begin
      JArr := TJSonArray(TJSonArray.ParseJSONValue(Tools.Text));
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools est�n mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(Tool_choice) <> '') then
      Begin
        jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(Tool_choice));
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tools_choice', jToolChoice);
      End;
    End;

    LastMsg := Messages.Last;
    If Assigned(LastMsg) then
    Begin
      If LastMsg.MediaFiles.Count > 0 then
      Begin
        AJSONObject.AddPair('messages', LastMsg.ToJSon); // Si tiene im�genes solo envia una entrada
      End
      Else
      Begin
        AJSONObject.AddPair('messages', GetMessages); // Si no tiene im�genes env�a todos los mensajes
      End;
    End;

    AJSONObject.AddPair('model', Model);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(Presence_penalty * 100) / 100));
    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(N));

    If (FResponse_format = tiaChatRfJsonSchema) then
    Begin
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_schema'))
    End
    Else If { LAsincronico or } (FResponse_format = tiaChatRfJson) then
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_object'))
    Else If (FResponse_format = tiaChatRfText) then
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'text'))
    Else
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'text'));

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

    Res := UTF8ToString(AJSONObject.ToJSon);
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
  Client: THTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  // OJO OJO OJO OJO
  Raise Exception.Create('Actualmente Groq no maneja modelos de embeddings');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSonObject.Create;

  Try
    jObj.AddPair('input', Input);
    jObj.AddPair('model', Model);
    jObj.AddPair('user', User);
    jObj.AddPair('encoding_format', EncodingFormat);

    St.WriteString(UTF8Encode(jObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);
    Response.Position := 0;

    // Response.SaveToFile('c:\temp\response.txt');

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

end.
