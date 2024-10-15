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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Chat.Mistral;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,
  uMakerAi.Chat, uMakerAi.ToolFunctions, uMakerAi.Core, uMakerAi.Embeddings;

  { TODO : Falta crear las siguientes funciones de Mistral }
///-----------------------------------------------------------------------------
///  Falta crear las siguientes funciones
///  https://docs.mistral.ai/api/#operation/jobs_api_routes_fine_tuning_create_fine_tuning_job
///Delete Model
///POST Upload File
///GET List Files
///GET Retrieve File
///DEL Delete File
///GET List Fine Tuning Jobs
///POST Create Fine Tuning Job
///GET  Get Fine Tuning Job
///POST Cancel Fine Tuning Job
///-----------------------------------------------------------------------------

type

  TAiMistralChat = Class(TAiChat)
  Private
  Protected
    Function InitChatCompletions: String; Override;
    Function InitChatCompletionsFim(aPrompt, aSuffix: String): String; Virtual; // Se introduce solo acá
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
  Public
    Constructor Create(Sender: TComponent); Override;
    Function AddMessageFimAndRun(aPrompt, aSuffix: String): String; Virtual;
  Published
  End;

  TAiMistralEmbeddings = Class(TAiEmbeddings)
  private
    FSuffix: String;
  Public
    // groq actualmente no maneja modelos de embeddings
    Constructor Create(aOwner: TComponent); Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = 1536; aModel: String = 'mistral-embed'; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;

  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.mistral.ai/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMistralChat, TAiMistralEmbeddings]);
end;

{ TAiMistralChat }

function TAiMistralChat.AddMessageFimAndRun(aPrompt, aSuffix: String): String;
Var
  ABody: String;
  sUrl, sRes: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  jObj: TJSonObject;
  Respuesta: String;
  Procesado: Boolean;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'fim/completions';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    ABody := InitChatCompletionsFim(aPrompt, aSuffix);

    St.WriteString(ABody);
    St.Position := 0;
    // St.SaveToFile('c:\temp\peticion.txt');
    // St.Position := 0;

    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    // FResponse.SaveToFile('c:\temp\respuesta.txt');
    FResponse.Position := 0;

    FLastContent := '';

    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        sRes := Res.ContentAsString;
        jObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(jObj);
          Result := FLastContent;

        Finally
          FreeAndNil(jObj);
        End;
      End
      else
      begin
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    End;
  Finally
    St.Free; // Esto no funciona en multiarea, así que se libera cuando no lo es.
  End;
end;

constructor TAiMistralChat.Create(Sender: TComponent);
begin
  inherited;

  Model := 'open-mistral-7b';
  Url := GlAIUrl;
end;

function TAiMistralChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  jObj, Msg, jFunc, Arg: TJSonObject;
  ArgVal, JVal, JVal1: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
  Nom, Valor: String;
  I: Integer;
begin
  Result := TAiToolsFunctions.Create;

  For JVal1 in jChoices do
  Begin
    Msg := TJSonObject(JVal1).GetValue<TJSonObject>('message');

    If Msg.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
    Begin
      For JVal in JToolCalls do
      Begin
        jObj := TJSonObject(JVal);
        // If jObj.GetValue<String>('type') = 'function' then //Mistral no maneja este parámetro
        Begin
          Fun := TAiToolsFunction.Create;
          Fun.Id := jObj.GetValue<String>('id');
          // Fun.Tipo := jObj.GetValue<String>('type'); //Mistral no maneja este parámetro

          If jObj.TryGetValue<TJSonObject>('function', jFunc) then
          Begin
            Fun.Name := jFunc.GetValue<String>('name');

            Fun.Arguments := jObj.GetValue<TJSonObject>('function').GetValue<String>('arguments');
          End;

          Try
            If (Fun.Arguments <> '') and (Fun.Arguments <> '{}') then
            Begin
              Arg := TJSonObject(TJSonObject.ParseJSONValue(Fun.Arguments));
              If Assigned(Arg) then
              Begin
                For I := 0 to Arg.Count - 1 do
                Begin
                  Nom := Arg.Pairs[I].JsonString.Value;
                  Valor := Arg.Pairs[I].JsonValue.Value;
                  Fun.Params.Values[Nom] := Valor;
                End;
              End;
            End;

          Except
            // Si no hay parámetros no marca error
          End;

          Result.Add(Fun.Id, Fun);
        End;
      End;
    End;
  End;
end;

function TAiMistralChat.InitChatCompletions: String;
Var
  AJSONObject, jObj, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  LastMsg: TAiChatMessage;
  Res : String;
begin

  If User = '' then
    User := 'user';

  If Model = '' then
    Model := 'open-mistral-7b';

  // Las funciones no trabajan en modo ascincrono
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(Tools.Text) <> '') then
    Begin
      JArr := TJSonArray(TJSonArray.ParseJSONValue(Tools.Text));
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools están mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(Tool_choice) <> '') then
      Begin
        jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(Tool_choice));
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tools_choice', jToolChoice);
      End;
    End;

    AJSONObject.AddPair('messages', GetMessages); // FMessages.ToJSon);
    AJSONObject.AddPair('model', Model);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    // AJSONObject.AddPair('user', User);

    If { LAsincronico or } (FResponse_format = tiaChatRfJson) then
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_object'))
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

    If Seed > 0 then
      AJSONObject.AddPair('random_seed', TJSONNumber.Create(Seed));

    Res := UTF8ToString(AJSONObject.ToJSon);
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);


  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

function TAiMistralChat.InitChatCompletionsFim(aPrompt, aSuffix: String): String;
Var
  AJSONObject, jObj, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  LastMsg: TAiChatMessage;
begin

  If Model = '' then
    Model := 'open-mistral-7b';

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try
    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    AJSONObject.AddPair('prompt', aPrompt);
    AJSONObject.AddPair('suffix', aSuffix);
    AJSONObject.AddPair('model', Model);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    If Seed > 0 then
      AJSONObject.AddPair('random_seed', TJSONNumber.Create(Seed));

    Result := UTF8ToString(AJSONObject.ToJSon);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

{ TAiMistralEmbeddings }

constructor TAiMistralEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  Url := GlAIUrl;
  FDimensions := -1;
  FModel := 'mistral-embed';
end;

function TAiMistralEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  jInput: TJSonArray;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSonObject.Create;

  If aModel = '' then
    aModel := FModel;

  Try
    jObj.AddPair('model', aModel);

    jInput := TJSonArray.Create;
    jInput.Add(aInput);

    jObj.AddPair('input', jInput); // Este se adiciona por compatibilidad con ollama
    // JObj.AddPair('prompt', aInput);
    // JObj.AddPair('user', aUser);
    // JObj.AddPair('dimensions', aDimensions);
    jObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(UTF8Encode(jObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
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
