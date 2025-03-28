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

unit uMakerAi.Chat.Gemini;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client, uMakerAi.Chat, uMakerAi.ToolFunctions, uMakerAi.Core;

type

  TAiGeminiChat = Class(TAiChat)
  Private
    Function GetToolJSon: TJSonArray;
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Function InternalAddMessage(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = ''): String; Override;
    Function InternalAddMessage(aPrompt, aRole: String; aMediaFiles: Array of TAiMediaFile): String; Override;

    Function InitChatCompletions: String; Override;
    Procedure ParseChat(JObj: TJSonObject); Override;
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
//    function ExtractToolCallJson(jChoices: TJSonArray): TJSonArray; // construye el llamado a las funciones en el mensaje


  Public
    Function GetMessages: TJSonArray; Override;
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function Run(aMsg: TAiChatMessage = Nil): String; Override;
  Published
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://generativelanguage.googleapis.com/v1beta/';
  // https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent?key=$GOOGLE_API_KEY
  //https://ai.google.dev/gemini-api/docs/document-processing?hl=es-419&lang=python

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGeminiChat]);
end;

{ TAiGeminiChat }

constructor TAiGeminiChat.Create(Sender: TComponent);
begin
  inherited;
  Model := 'gemini-pro';
  Url := GlAIUrl;
  Top_p := 1;
end;

destructor TAiGeminiChat.Destroy;
begin

  inherited;
end;

function TAiGeminiChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
begin
  result:=nil;
end;

//function TAiGeminiChat.ExtractToolCallJson(jChoices: TJSonArray): TJSonArray;
//begin
//
//end;

function TAiGeminiChat.GetMessages: TJSonArray;
Var
  I: Integer;
  Msg: TAiChatMessage;
  JObj1, jPartItem: TJSonObject;
  JParts: TJSonArray;
begin
  Result := TJSonArray.Create;

  For I := 0 to Messages.Count - 1 do
  Begin
    Msg := Messages.Items[I];

    Result := TJSonArray.Create;

    If Msg.Prompt <> '' then
    Begin
      JObj1 := TJSonObject.Create;
      Result.Add(JObj1);

      JParts := TJSonArray.Create;
      jPartItem := TJSonObject.Create;
      JParts.Add(jPartItem);

      jPartItem.addPair('text', Msg.Prompt);

      JObj1.addPair('role', Msg.Role);
      JObj1.addPair('parts', JParts);
    End;
  End;

end;

class function TAiGeminiChat.GetModels(aApiKey, aUrl: String): TStringList;
begin

end;

function TAiGeminiChat.GetToolJSon: TJSonArray;
begin

end;

function TAiGeminiChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice, JConfig: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
begin

  If User = '' then
    User := 'user';

  If Model = '' then
    Model := 'gemini-pro';

  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    // AJSONObject.addPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active then
    Begin
      JArr := GetToolJSon;

      If Assigned(JArr) then
      Begin
        AJSONObject.addPair('tools', JArr);

        If (Trim(Tool_choice) <> '') then
        Begin
          jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(Tool_choice));
          If Assigned(jToolChoice) then
            AJSONObject.addPair('tools_choice', jToolChoice);
        End;
      End;
    End;

    AJSONObject.addPair('contents', GetMessages); // FMessages.ToJSon);
    // AJSONObject.addPair('model', Model);

    JConfig := TJSonObject.Create;

    Begin // Sección de Config
      Lista.CommaText := Self.Stop;
      If Lista.Count > 0 then
      Begin
        JStop := TJSonArray.Create;
        For I := 0 to Lista.Count - 1 do
          JStop.Add(Lista[I]);
        AJSONObject.addPair('stopSequences', JStop);
      End;

      JConfig.addPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
      JConfig.addPair('maxOutputTokens', TJSONNumber.Create(Max_tokens));

      If Top_p <> 0 then
        JConfig.addPair('top_p', TJSONNumber.Create(Top_p));

      AJSONObject.addPair('generationConfig', JConfig); // FMessages.ToJSon);
    End;

    Result := UTF8ToString(AJSONObject.ToJSon);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

function TAiGeminiChat.InternalAddMessage(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): String;
Var
  Msg: TAiChatMessage;
  MensajeInicial: String;
  MF: TAiMediaFile;
  Procesado: Boolean;
  Respuesta: String;
begin

  Try
    If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then //Solo acepta esto al iniciar el chat
    Begin
      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiChatMessage.Create(MensajeInicial, 'user');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      Msg := TAiChatMessage.Create('De acuerdo, seguiré las instrucciones', 'model');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
    End;

    // Adiciona el mensaje a la lista
    Msg := TAiChatMessage.Create(aPrompt, aRole);
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);

    If Assigned(FOnAddMessage) then
    Begin
      FOnAddMessage(Self, Msg, Nil, aRole, aPrompt);
    End;

    For MF in aMediaFiles do
    Begin
      DoProcessMediaFile(aPrompt, MF, Respuesta, Procesado); // Envía el archivo por si lo quiere procesar otra AI especializada, Ej.
      If Procesado then
        Msg.Prompt := Msg.Prompt + sLineBreak + Respuesta;

      Msg.AddMediaFile(MF);
    End;

    FLastPrompt := Msg.Prompt;

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
end;

function TAiGeminiChat.InternalAddMessage(aPrompt, aRole, aToolCallId, aFunctionName: String): String;
Var
  Msg: TAiChatMessage;
  MensajeInicial: String;
begin

  Try
    If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then //Solo acepta esto al iniciar el chat
    Begin
      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiChatMessage.Create(MensajeInicial, 'user');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      Msg := TAiChatMessage.Create('De acuerdo, seguiré las instrucciones', 'model');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
    End;

    // Adiciona el mensaje a la lista
    Msg := TAiChatMessage.Create(aPrompt, aRole, aToolCallId, aFunctionName);
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);
    FLastPrompt := aPrompt;

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
end;

procedure TAiGeminiChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  inherited;

end;

procedure TAiGeminiChat.ParseChat(JObj: TJSonObject);
Var
  Candidates, Parts, safetyRatings: TJSonArray;
  Content, Uso: TJSonObject;
  jVal, jValParts: TJSonValue;
  finishReason, Respuesta, Role: String;
  aIndex: Integer;
  aPrompt_tokens, aCompletion_tokens, atotal_tokens: Integer;
  Msg: TAiChatMessage;

begin
  If JObj.TryGetValue<TJSonArray>('candidates', Candidates) then
  Begin
    For jVal in Candidates do
    Begin
      If jVal.TryGetValue<TJSonObject>('content', Content) then
      Begin
        Content.TryGetValue<String>('role', Role);
        If Content.TryGetValue<TJSonArray>('parts', Parts) then
        Begin
          For jValParts in Parts do
          Begin
            Respuesta := Respuesta + jValParts.GetValue<String>('text') + sLineBreak;
          End;
        End;

        jVal.TryGetValue<String>('finishReason', finishReason);
        jVal.TryGetValue<Integer>('index', aIndex);

        { TODO : Falta implementar los SaretyRaitings }
        jVal.TryGetValue<TJSonArray>('safetyRatings', safetyRatings);

      End;
    End;
  end;

  If JObj.TryGetValue<TJSonObject>('usageMetadata', Uso) then
  Begin
    aPrompt_tokens := Uso.GetValue<Integer>('promptTokenCount');
    aCompletion_tokens := Uso.GetValue<Integer>('candidatesTokenCount');
    atotal_tokens := Uso.GetValue<Integer>('totalTokenCount');
  End
  else
  Begin
    aPrompt_tokens := 0;
    aCompletion_tokens := 0;
    atotal_tokens := 0;
  End;

  Respuesta := Trim(Respuesta);
  Self.FLastContent := Respuesta;
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + atotal_tokens;

  Msg := TAiChatMessage.Create(Respuesta, Role);
  Msg.Prompt := Respuesta;
  // Msg.Tool_calls := sToolCalls;
  Msg.Prompt_tokens := aPrompt_tokens;
  Msg.Completion_tokens := aCompletion_tokens;
  Msg.Total_tokens := atotal_tokens;
  Msg.Id := FMessages.Count + 1;
  FMessages.Add(Msg);
end;

function TAiGeminiChat.Run(aMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
//  MensajeInicial: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  JObj: TJSonObject;
//  Msg: TAiChatMessage;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'models/gemini-pro:generateContent?key=' + ApiKey;

  Try
    // FHeaders := [TNetHeader.Create('x-api-key', ApiKey)];
    // FHeaders := FHeaders + [TNetHeader.Create('anthropic-version', '2023-06-01')];
    // FHeaders := FHeaders + [TNetHeader.Create('content-type', 'application/json')];

    // If Tool_Active then
    // FHeaders := FHeaders + [TNetHeader.Create('anthropic-beta', 'tools-2024-05-16')];

    FClient.ContentType := 'application/json';

    { If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then  //solo acepta esto al comienzo como primer mensaje
      Begin

      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiOpenChatMessage.Create(MensajeInicial, 'user');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      Msg := TAiOpenChatMessage.Create('De acuerdo, seguiré las instrucciones', 'assistant');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
      FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
      End;

      If Assigned(aMsg) then
      Begin

      If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);

      aMsg.Id := FMessages.Count + 1;
      FMessages.Add(aMsg);
      FLastPrompt := aMsg.Prompt;

      If Assigned(FOnAddMessage) then
      FOnAddMessage(Self, Msg, Nil, aMsg.Role, aMsg.Prompt);

      End;
    }

    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;
    //St.SaveToFile('c:\temp\peticion.txt');
    //St.Position := 0;

    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    //FResponse.SaveToFile('c:\temp\respuesta.txt');
    //FResponse.Position := 0;

    FLastContent := '';

    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(JObj);
          Result := FLastContent;

        Finally
          FreeAndNil(JObj);
        End;
      End
      else
      begin
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    End;
  Finally
    If FClient.Asynchronous = False then
      St.Free;
    // Esto no funciona en multiarea, así que se libera cuando no lo es.
  End;
end;

end.
