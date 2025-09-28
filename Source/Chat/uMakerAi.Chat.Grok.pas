unit uMakerAi.Chat.Grok;
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


//--Cambios--
//--10-Jul-2025---se habilita la b�squeda en internet nativo del modelo

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

  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Embeddings, uMakerAi.Core;

Type

  TAiGrokChat = Class(TAiChat)
  Private
  Protected
    Function InitChatCompletions: String; Override;
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

{ TAiOllamaChat }

class function TAiGrokChat.GetDriverName: string;
Begin
  Result := 'Grok';
End;

class procedure TAiGrokChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@GROK_API_KEY');
  Params.Add('Model=grok-2-1212');
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
  Model := 'grok-2-1212';
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
    LModel := 'grok-2-1212';

  // Las funciones no trabajan en modo ascincrono
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  // En groq hay una restricci�n sobre las im�genes

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAI).Text) <> '') then
    Begin

{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAI).Text));
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
          AJSONObject.AddPair('tools_choice', jToolChoice);
      End;
    End;

    AJSONObject.AddPair('messages', GetMessages);

    AJSONObject.AddPair('model', LModel);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    //AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    //AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(Presence_penalty * 100) / 100));

    if ReasoningFormat <> '' then
      AJSONObject.AddPair('reasoning_format', ReasoningFormat); // 'parsed, raw, hidden';

    if ReasoningEffort <> '' then
      AJSONObject.AddPair('reasoning_effort', ReasoningEffort); // 'none, default');


    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(N));


    if tcm_WebSearch in ChatMediaSupports then
    begin
      Var
      jWebSearchOptions := TJSonObject.Create;
      jWebSearchOptions.AddPair('mode', 'auto');
      //jWebSearchOptions.AddPair('return_citations', 'true');
      AJSONObject.AddPair('search_parameters', jWebSearchOptions);
    end;


    {
      If (FResponse_format = tiaChatRfJsonSchema) then
      Begin
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_schema'))
      End
      Else If  (FResponse_format = tiaChatRfJson) then
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_object'))
      Else If (FResponse_format = tiaChatRfText) then
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'text'))
      Else
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'text'));
    }

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
  LModel : String;
begin
  Result := ''; // La salida principal es el MediaFile en ResMsg
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // 1. Validaciones y configuraci�n
  if AskMsg.Prompt.IsEmpty then
    raise Exception.Create('Se requiere un prompt para generar una imagen.');

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  if LModel = '' then
    LModel := 'grok-2-image'; // Asignar un modelo de imagen por defecto

  LUrl := Url + 'images/generations'; // Url base + endpoint

  // 2. Construir el cuerpo de la petici�n JSON
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
    LBodyStream.WriteString(LBodyJson.ToJSon);
    LBodyStream.Position := 0;
{$IFDEF APIDEBUG}
    LBodyStream.SaveToFile('c:\temp\grok_image_request.json');
    LBodyStream.Position := 0;
{$ENDIF}
    // Grok usa Bearer token para la autorizaci�n
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
              // Extraer el prompt revisado y guardarlo (es informaci�n �til)
              LImageObject.TryGetValue<string>('revised_prompt', LRevisedPrompt);
              LNewMediaFile.Transcription := LRevisedPrompt;
              FLastContent := LRevisedPrompt;
              ResMsg.Prompt := FLastContent;

              // CASO A: La respuesta es una URL
              if LImageObject.TryGetValue<string>('url', LImageUrl) then
              begin
                // Descargamos la imagen desde la URL y la cargamos en el MediaFile
                // Necesitar�s una funci�n para descargar, por ejemplo:
                LNewMediaFile.LoadFromUrl(LImageUrl); // Asumiendo que tienes esta funci�n
              end
              // CASO B: La respuesta es Base64
              else if LImageObject.TryGetValue<string>('b64_json', LBase64Data) then
              begin
                LNewMediaFile.LoadFromBase64('generated_image.png', LBase64Data);
              end;

              // A�adir el MediaFile al mensaje de respuesta
              ResMsg.MediaFiles.Add(LNewMediaFile);
            except
              LNewMediaFile.Free;
              raise;
            end;
          end;
        end;

        // Disparamos el evento de finalizaci�n
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
