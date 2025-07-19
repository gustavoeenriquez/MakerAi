unit uMakerAi.Chat.OpenAi;

// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
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


// --------- CAMBIOS --------------------
// 29/08/2024 - Se adiciona el manejo de response_format = json_schema
// 04/11/2024 - Se adiciona la propiedad TAiOpenChat.Modalities para el manejo de audio
// 04/11/2024 - Se adicionan la propiedades TAiOpenChat.voice y voice_format
// 04/11/2024 - se habilita la opción de recibir y generar audio utilizando TMediaFile
// 05/11/2024 - se adiciona la propiedad TAiOpenChat.Store

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, System.NetEncoding, System.Types, System.NetConsts,
  System.Net.Mime,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.JSON, Rest.JSON,
  uMakerAi.ParamsRegistry, uMakerAi.ToolFunctions, uMakerAi.Core, uMakerAi.Chat,
  uMakerAi.Embeddings, uMakerAi.Utils.CodeExtractor;

type

  TAiOpenChat = class(TAiChat)
  Private
    // FModalities: TAiModilities;
    // FVoice: String;
    // Fvoice_format: String;
    FStore: Boolean;
    FParallel_ToolCalls: Boolean;
    FService_Tier: String;
    // procedure SetModalities(const Value: TAiModilities);
    // procedure SetVoice(const Value: String);
    // procedure Setvoice_format(const Value: String);
    procedure SetStore(const Value: Boolean);
    procedure SetParallel_ToolCalls(const Value: Boolean);
    procedure SetService_Tier(const Value: String);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;

    function InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunImageDalleGeneration(ResMsg, AskMsg: TAiChatMessage): String;
    function InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunWebSearch(ResMsg, AskMsg: TAiChatMessage): String; Override;

    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;
    // function InternalRunImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunPDFDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;

    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    procedure ParseJsonTranscript(jObj: TJSonObject; ResMsg: TAiChatMessage; aMediaFile: TAiMediaFile);
    procedure ParseResponse(jObj: TJSonObject; ResMsg: TAiChatMessage);

    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    Procedure DoCallFunction(ToolCall: TAiToolsFunction); Override;
    function GetTools: TStrings; Override;
    Function PrepareSystemMsg: String; Override; // Crea el primer mensaje del chat para system, para configurar el asistente
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Overload; Override;
    Function GetModels: TStringList; Overload; Override;
    Function GetMessages: TJSonArray; Override;

    Function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: integer = 3600): String; Virtual;

    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

  Published
    Property Store: Boolean read FStore write SetStore;
    Property Parallel_ToolCalls: Boolean read FParallel_ToolCalls write SetParallel_ToolCalls;
    Property Service_Tier: String read FService_Tier write SetService_Tier;
  end;

  TAiOpenAiEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: integer = -1; aModel: String = ''; aEncodingFormat: String = 'float')
      : TAiEmbeddingData; Override;
    Procedure ParseEmbedding(jObj: TJSonObject); Override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenChat, TAiOpenAiEmbeddings]);
end;

{ TAiChat }

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

class function TAiOpenChat.GetDriverName: string;
Begin
  Result := 'OpenAi';
End;

class procedure TAiOpenChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@OPENAI_API_KEY');
  Params.Add('Model=gpt-4o');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=https://api.openai.com/v1/');
End;

class function TAiOpenChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiOpenChat.Create(Sender);
End;

function TAiOpenChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  jObj: TJSonObject;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'chat/completions';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;

{$IFDEF APIDEBUG}
    St.SaveToFile('c:\temp\peticion.txt');
    St.Position := 0;
{$ENDIF}
    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    FResponse.Position := 0;
{$IFDEF APIDEBUG}
    FResponse.SaveToFile('c:\temp\respuesta.txt');
    FResponse.Position := 0;
{$ENDIF}
    FLastContent := '';

    // If Self.Asynchronous = False then
    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        jObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(jObj, ResMsg);
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
    If FClient.Asynchronous = False then
      St.Free; // Esto no funciona en multiarea, así que se libera cuando no lo es.
  End;
end;

{ function TAiOpenChat.InternalRunImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
  begin

  end;
}

function TAiOpenChat.InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl: string;
  LBodyStream, LResponseStream: TStringStream;
  LHeaders: TNetHeaders;
  LResponse: IHTTPResponse;
  LJsonObject, LResponseJson, LImageObject: TJSonObject;
  LDataArray: TJSonArray;
  LBase64Data: string;
  // LBinaryStream: TMemoryStream;
  LNewImageFile: TAiMediaFile;
  LErrorResponse: string;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: integer;
  LModel: String;
begin

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  if (Pos('dall-e', LowerCase(LModel)) > 0) then
  begin
    // Usa la API de DALL-E legacy
    Result := InternalRunImageDalleGeneration(ResMsg, AskMsg);
    Exit
  end;

  Result := ''; // La función principal no devuelve texto, la imagen va en ResMsg
  FBusy := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // 1. Añadir el mensaje del usuario al historial
  if FMessages.IndexOf(AskMsg) < 0 then
  begin
    AskMsg.Id := FMessages.Count + 1;
    FMessages.Add(AskMsg);
    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, AskMsg, Nil, AskMsg.Role, AskMsg.Prompt);
  end;

  // 2. Preparar parámetros para la API de DALL-E
  LUrl := Url + 'images/generations'; // Asume que 'Url' es la URL base de OpenAI
  LJsonObject := TJSonObject.Create;
  LBodyStream := nil;
  LResponseStream := nil;
  LResponseJson := nil;

  try
    LJsonObject.AddPair('model', LModel); // Debería ser 'gpt-image-1'
    LJsonObject.AddPair('prompt', TJSONString.Create(AskMsg.Prompt));
    LJsonObject.AddPair('n', TJSONNumber.Create(n));
    // LJsonObject.AddPair('response_format', TJSONString.Create('b64_json'));
    // LJsonObject.AddPair('size', '1024x1024');

    // Parámetros opcionales,  puedes agregar controles para esto.
    // LJsonObject.AddPair('size', '1024x1024');
    // LJsonObject.AddPair('quality', 'high');
    // LJsonObject.AddPair('output_format', 'png');

    if LModel = 'dall-e-2' then
    begin
      // LJsonObject.AddPair('quality', TJSONString.Create('hd'));
      // LJsonObject.AddPair('style', TJSONString.Create('vivid')); // vivid or natural
    end
    Else if LModel = 'dall-e-3' then
    begin
      // LJsonObject.AddPair('quality', TJSONString.Create('hd'));
      // LJsonObject.AddPair('style', TJSONString.Create('vivid')); // vivid or natural
    end;

    // Añadir identificador de usuario
    if not User.IsEmpty then
      LJsonObject.AddPair('user', TJSONString.Create(User));

    // 4. Ejecutar la petición HTTP
    LBodyStream := TStringStream.Create(LJsonObject.ToString, TEncoding.UTF8);
    LResponseStream := TStringStream.Create('', TEncoding.UTF8);

    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

    LResponseStream.SaveToFile('c:\temp\respuesta.txt');
    LResponseStream.Position := 0;

    // 5. Procesar la respuesta
    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSonObject.ParseJSONValue(LResponseStream.DataString) as TJSonObject;
      if LResponseJson = nil then
        raise Exception.Create('Error parsing JSON response from DALL-E.');

      // El objeto 'usage' puede no venir en respuestas con errores
      Var
        uso: TJSonObject;

      if LResponseJson.TryGetValue<TJSonObject>('usage', uso) then
      begin
        aPrompt_tokens := uso.GetValue<integer>('input_tokens');
        aCompletion_tokens := uso.GetValue<integer>('output_tokens');
        aTotal_tokens := uso.GetValue<integer>('total_tokens');
      end
      else
      begin
        aPrompt_tokens := 0;
        aCompletion_tokens := 0;
        aTotal_tokens := 0;
      end;

      Self.Prompt_tokens := Self.Prompt_tokens + aPrompt_tokens;
      Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
      Self.Total_tokens := Self.Total_tokens + aTotal_tokens;

      LDataArray := LResponseJson.GetValue<TJSonArray>('data');

      if (LDataArray <> nil) and (LDataArray.Count > 0) then
      begin
        LImageObject := LDataArray.Items[0] as TJSonObject;
        LBase64Data := LImageObject.GetValue<string>('b64_json');

        Var
          RevisedPrompt: String;
        LImageObject.TryGetValue<String>('revised_prompt', RevisedPrompt);

        FLastContent := Trim(FLastContent + sLineBreak + RevisedPrompt);

        // Crear el archivo de media y adjuntarlo al mensaje de respuesta
        LNewImageFile := TAiMediaFile.Create;
        try
          // DALL-E genera imágenes en formato PNG
          LNewImageFile.LoadFromBase64('generated_image.png', LBase64Data);
          ResMsg.MediaFiles.Add(LNewImageFile);
          ResMsg.Prompt := FLastContent;

          // Disparar el evento de fin de recepción de datos
          if Assigned(FOnReceiveDataEnd) then
            FOnReceiveDataEnd(Self, ResMsg, nil, 'model', '');

        except
          LNewImageFile.Free; // Liberar si algo falla después de crearlo
          raise;
        end;
      end
      else
      begin
        FLastError := LModel + ' response OK but no image data received.';
        DoError(FLastError, nil);
      end;
    end
    else
    begin
      LErrorResponse := LResponseStream.DataString;
      FLastError := Format('Error generating image: %d, %s', [LResponse.StatusCode, LErrorResponse]);
      DoError(FLastError, nil);
    end;
  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
    // LBinaryStream.Free;
    LResponseJson.Free; // ParseJSONValue crea un objeto que debemos liberar
    FBusy := False;
  end;
end;

{ function TAiOpenChat.InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String;
  Var
  LBodyJson, LToolObject: TJSonObject;
  LToolsArray: TJSonArray;
  LBodyStream: TStringStream;
  LUrl: String;
  LHeaders: TNetHeaders;
  LastMessage: TAiChatMessage;
  LResponse: IHTTPResponse;
  LResponseJson: TJSonObject;
  Res: String;
  begin

  if (Pos('dall-e', LowerCase(Self.Model)) > 0) then
  begin
  // Usa la API de DALL-E legacy
  Result := InternalRunImageDalleGeneration(ResMsg, AskMsg);
  Exit
  end;

  if FMessages.Count = 0 then
  raise Exception.Create('No hay mensajes en el historial para generar una imagen.');
  LastMessage := GetLastMessage;

  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := LastMessage.Prompt;

  LBodyJson := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);

  try
  LBodyJson.AddPair('model', Self.Model); // Debería ser 'gpt-image-1'
  LBodyJson.AddPair('prompt', LastMessage.Prompt); // Usar el prompt del último mensaje
  LBodyJson.AddPair('n', 1); // Cantidad de imágenes.

  // Parámetros opcionales,  puedes agregar controles para esto.
  // LBodyJson.AddPair('size', '1024x1024');
  // LBodyJson.AddPair('quality', 'high');
  // LBodyJson.AddPair('output_format', 'png');

  // LUrl := Url + 'responses';
  LUrl := Url + 'images/generations'; // Asume que 'Url' es la URL base de OpenAI

  LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
  FClient.ContentType := 'application/json';

  Res := UTF8ToString(LBodyJson.ToJSon);
  LBodyStream.WriteString(Res);
  LBodyStream.Position := 0;

  FResponse.Clear;
  FResponse.Position := 0;

  LResponse := FClient.Post(LUrl, LBodyStream, FResponse, LHeaders);

  $IFDEF APIDEBUG
  FResponse.SaveToFile('c:\temp\respuesta.txt');
  FResponse.Position := 0;
  $ENDIF

  FLastContent := '';

  if LResponse.StatusCode = 200 then
  begin
  LResponseJson := TJSonObject(TJSonObject.ParseJSONValue(LResponse.ContentAsString));
  try
  FBusy := False;
  ParseResponse(LResponseJson, ResMsg);
  Result := FLastContent;
  finally
  FreeAndNil(LResponseJson);
  end;
  end
  else
  begin
  FBusy := False;
  Raise Exception.CreateFmt('Error desde el endpoint /v1/images/generations: %d, %s',
  [LResponse.StatusCode, LResponse.ContentAsString]);
  end;
  finally
  LBodyJson.Free;
  LBodyStream.Free;
  end;
  end;
}

function TAiOpenChat.InternalRunImageDalleGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl: string;
  LBodyStream, LResponseStream: TStringStream;
  LHeaders: TNetHeaders;
  LResponse: IHTTPResponse;
  LJsonObject, LResponseJson, LImageObject: TJSonObject;
  LDataArray: TJSonArray;
  LBase64Data: string;
  // LBinaryStream: TMemoryStream;
  LNewImageFile: TAiMediaFile;
  LErrorResponse: string;
begin

  Result := ''; // La función principal no devuelve texto, la imagen va en ResMsg
  FBusy := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // 1. Añadir el mensaje del usuario al historial
  if FMessages.IndexOf(AskMsg) < 0 then
  begin
    AskMsg.Id := FMessages.Count + 1;
    FMessages.Add(AskMsg);
    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, AskMsg, Nil, AskMsg.Role, AskMsg.Prompt);
  end;

  // 2. Preparar parámetros para la API de DALL-E
  LUrl := Url + 'images/generations'; // Asume que 'Url' es la URL base de OpenAI
  LJsonObject := TJSonObject.Create;
  LBodyStream := nil;
  LResponseStream := nil;
  // LBinaryStream := nil;
  LResponseJson := nil;

  try
    LJsonObject.AddPair('prompt', TJSONString.Create(AskMsg.Prompt));
    LJsonObject.AddPair('n', TJSONNumber.Create(n));
    LJsonObject.AddPair('response_format', TJSONString.Create('b64_json'));
    LJsonObject.AddPair('size', '1024x1024');

    if Model = 'dall-e-2' then
    begin
      // LJsonObject.AddPair('quality', TJSONString.Create('hd'));
      // LJsonObject.AddPair('style', TJSONString.Create('vivid')); // vivid or natural
    end
    Else if Model = 'dall-e-3' then
    begin
      // LJsonObject.AddPair('quality', TJSONString.Create('hd'));
      // LJsonObject.AddPair('style', TJSONString.Create('vivid')); // vivid or natural
    end;

    // Añadir identificador de usuario
    if not User.IsEmpty then
      LJsonObject.AddPair('user', TJSONString.Create(User));

    // 4. Ejecutar la petición HTTP
    LBodyStream := TStringStream.Create(LJsonObject.ToString, TEncoding.UTF8);
    LResponseStream := TStringStream.Create('', TEncoding.UTF8);

    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

    // 5. Procesar la respuesta
    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSonObject.ParseJSONValue(LResponseStream.DataString) as TJSonObject;
      if LResponseJson = nil then
        raise Exception.Create('Error parsing JSON response from DALL-E.');

      LDataArray := LResponseJson.GetValue<TJSonArray>('data');
      if (LDataArray <> nil) and (LDataArray.Count > 0) then
      begin
        LImageObject := LDataArray.Items[0] as TJSonObject;
        LBase64Data := LImageObject.GetValue<string>('b64_json');

        // Crear el archivo de media y adjuntarlo al mensaje de respuesta
        LNewImageFile := TAiMediaFile.Create;
        try
          // DALL-E genera imágenes en formato PNG
          LNewImageFile.LoadFromBase64('generated_image.png', LBase64Data);
          ResMsg.MediaFiles.Add(LNewImageFile);

          // Disparar el evento de fin de recepción de datos
          if Assigned(FOnReceiveDataEnd) then
            FOnReceiveDataEnd(Self, ResMsg, nil, 'model', '');

        except
          LNewImageFile.Free; // Liberar si algo falla después de crearlo
          raise;
        end;
      end
      else
      begin
        FLastError := Model + ' response OK but no image data received.';
        DoError(FLastError, nil);
      end;
    end
    else
    begin
      LErrorResponse := LResponseStream.DataString;
      FLastError := Format('Error generating image: %d, %s', [LResponse.StatusCode, LErrorResponse]);
      DoError(FLastError, nil);
    end;
  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
    // LBinaryStream.Free;
    LResponseJson.Free; // ParseJSONValue crea un objeto que debemos liberar
    FBusy := False;
  end;
end;

function TAiOpenChat.InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String;
begin
  Raise Exception.Create('Todavía no se ha implementado esta opción para este modelo');
end;

function TAiOpenChat.InternalRunPDFDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
begin
  Raise Exception.Create('Todavía no se ha implementado esta opción para este modelo, debe ejecutarlos desde Responses');
end;

{ //Tiene la misma estructura de la clase padre

  function TAiOpenChat.InternalAddMessage(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String;
  Var
  Msg: TAiChatMessage;
  begin
  Try
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

}

{ //Tiene la misma estructura de la clase padre
  function TAiOpenChat.InternalAddMessage(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): String;
  Var
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
  MensajeInicial: String;
  Respuesta: String;
  Procesado: Boolean;
  begin

  Try
  // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
  If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then
  Begin
  MensajeInicial := Self.PrepareSystemMsg;

  Msg := TAiChatMessage.Create(MensajeInicial, 'system');
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
  If Procesado then // Si el usuario convirtió el media file en un texto, para procesar el texto y no el archivo directamente
  Msg.Prompt := Respuesta + sLineBreak + Msg.Prompt;

  Msg.AddMediaFile(MF);
  End;

  FLastPrompt := Msg.Prompt; // aqui lleva el Prompt Inicial + la conversión de los MediaFiles a texto si el usuario lo permite

  If Assigned(FOnBeforeSendMessage) then
  FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
  end;
}

function TAiOpenChat.InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl, LModel, LVoice, LResponseFormat: string;
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LHeaders: TNetHeaders; // <--- Variable ya declarada, ahora la usaremos
  LResponse: IHTTPResponse;
  LJsonObject: TJSonObject;
  LErrorResponse: string;
  LNewAudioFile: TAiMediaFile;
begin
  Result := ''; // La función Run devuelve el texto, que en este caso es vacío.
  FBusy := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // 1. Añadir el mensaje del usuario al historial para mantener la consistencia
  if FMessages.IndexOf(AskMsg) < 0 then // Solo lo añadimos si no está ya en la lista
  begin
    AskMsg.Id := FMessages.Count + 1;
    FMessages.Add(AskMsg);
    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, AskMsg, Nil, AskMsg.Role, AskMsg.Prompt);
  end;

  // 2. Preparar parámetros para la API de TTS
  LUrl := Url + 'audio/speech';
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  // 'tts-1'; // O podrías tener una propiedad específica para el modelo TTS
  LVoice := Self.Voice; // Usamos la propiedad del componente
  LResponseFormat := Self.voice_format; // Usamos la propiedad del componente

  // 3. Construir y ejecutar la petición
  LJsonObject := TJSonObject.Create;
  LBodyStream := nil;
  LResponseStream := TMemoryStream.Create;
  try
    LJsonObject.AddPair('model', LModel);
    LJsonObject.AddPair('input', AskMsg.Prompt);
    LJsonObject.AddPair('voice', LVoice);
    LJsonObject.AddPair('response_format', LResponseFormat);

    LBodyStream := TStringStream.Create(UTF8ToString(LJsonObject.Format), TEncoding.UTF8);
    // LBodyStream := TStringStream.Create(LJsonObject.ToString, TEncoding.UTF8);

    // S := LJsonObject.ToString;

    // LBodyStream := TStringStream.Create;
    // LBodyStream.WriteString(S);

    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    LBodyStream.Position := 0;
{$IFDEF APIDEBUG}
    LBodyStream.SaveToFile('c:\temp\peticionAudio.json.txt');
    LBodyStream.Position := 0;
{$ENDIF}
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

    // 4. Procesar la respuesta
    if LResponse.StatusCode = 200 then
    begin
      // ... (el resto del código de procesamiento de la respuesta es correcto)
      LNewAudioFile := TAiMediaFile.Create;
      try
        LResponseStream.Position := 0;
        LNewAudioFile.LoadFromStream('generated_audio.' + LResponseFormat, LResponseStream);
        // LResponseMsg := TAiChatMessage.Create('', 'model');

        ResMsg.MediaFiles.Add(LNewAudioFile);
        // ResMsg.Id := FMessages.Count + 1; //Lo adiciona en el run al finalizar el proceso
        // FMessages.Add(ResMsg);

        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, nil, 'model', '');
      except
        LNewAudioFile.Free;
        raise;
      end;
    end
    else
    begin
      // ... (el resto del código de manejo de errores es correcto)
      LResponseStream.Position := 0;
      LErrorResponse := TStreamReader.Create(LResponseStream).ReadToEnd;
      FLastError := Format('Error generando audio: %d, %s', [LResponse.StatusCode, LErrorResponse]);
      DoError(FLastError, nil);
    end;
  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
  end;

  FBusy := False;
end;

function TAiOpenChat.InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
  LResponseStream: TMemoryStream;
  LTempStream: TMemoryStream;
  LResponseObj: TJSonObject;
  Granularities: TStringList; // Para procesar las granularidades
  I: integer;
  LModel: String;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo de audio con contenido para la transcripción.');

  sUrl := Url + 'audio/transcriptions';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  Client := THTTPClient.Create;
  LResponseStream := TMemoryStream.Create;
  Body := TMultipartFormData.Create;
  Granularities := TStringList.Create;

  try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

    // Crear un stream temporal para pasarlo al formulario multipart
    LTempStream := TMemoryStream.Create;
    aMediaFile.Content.Position := 0;
    LTempStream.LoadFromStream(aMediaFile.Content);
    LTempStream.Position := 0;

    // --- 1. CONSTRUCCIÓN DEL BODY MULTIPART CON PARÁMETROS GENÉRICOS ---
    Body.AddStream('file', LTempStream, aMediaFile.Filename, aMediaFile.MimeType);

    Body.AddField('model', LModel); // Default seguro

    if not AskMsg.Prompt.IsEmpty then
      Body.AddField('prompt', AskMsg.Prompt);

    // Formato de respuesta (genérico, como string)
    if not Self.Transcription_ResponseFormat.IsEmpty then
      Body.AddField('response_format', Self.Transcription_ResponseFormat)
    else
      Body.AddField('response_format', 'json'); // Default a JSON si no se especifica

    // Parámetros opcionales
    if not Self.Language.IsEmpty then
      Body.AddField('language', Self.Language);

    // Usamos la propiedad de Temperatura existente en el componente
    if Self.Temperature > 0 then
      Body.AddField('temperature', FormatFloat('0.0', Self.Temperature));

    // Timestamps Granularities (procesamos la cadena)
    if not Self.Transcription_TimestampGranularities.IsEmpty then
    begin
      // Dividimos la cadena por comas
      Granularities.CommaText := Self.Transcription_TimestampGranularities;
      for I := 0 to Granularities.Count - 1 do
      begin
        // Añadimos cada granularidad como un campo separado con '[]'
        Body.AddField('timestamp_granularities[]', Trim(Granularities[I]));
      end;
    end;

    { TODO : TODAVÍA NO ESTÁ LISTO PARA UTILIZAR LA TRANSCRIPCIÓN EN MODO ASCINCRÓNICO, Falta implementar a futuro }
    // Streaming
    // if Self.Asynchronous then
    // Body.AddField('stream', 'true');

    // --- 2. EJECUCIÓN DE LA PETICIÓN POST ---

    // (La lógica de streaming/síncrono se mantiene igual)
    { if Self.Asynchronous then
      begin
      FClient.Asynchronous := True;
      // La lógica de OnReceiveData se encargará del resto
      FClient.Post(sUrl, Body, FResponse, Headers);
      Result := '';
      end
      else
    }
    begin
      Res := Client.Post(sUrl, Body, LResponseStream, Headers);

      if Res.StatusCode = 200 then
      begin

        LResponseObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;

        If Not Assigned(LResponseObj) then
        Begin
          LResponseObj := TJSonObject.Create(TJSonPair.Create('text', Res.ContentAsString));
        End;

        try
          // Aquí llamas al procedimiento de parseo de transcripciones
          ParseJsonTranscript(LResponseObj, ResMsg, aMediaFile);
        finally
          LResponseObj.Free;
        end;

        Result := ResMsg.Prompt;
      end
      else
      begin
        Raise Exception.CreateFmt('Error en la transcripción: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    end;

  finally
    Body.Free;
    Client.Free;
    LResponseStream.Free;
    // Granularities.Free;
    // LTempStream es propiedad de Body
  end;
end;

function TAiOpenChat.InternalRunWebSearch(ResMsg, AskMsg: TAiChatMessage): String;
Var
  // Para la petición
  LBodyJson: TJSonObject;
  LToolsArray: TJSonArray;
  LToolObject: TJSonObject;
  LBodyStream: TStringStream;
  LUrl: String;
  LModel: String;
  LHeaders: TNetHeaders;
  LastMessage: TAiChatMessage;

  // Para la respuesta
  LResponse: IHTTPResponse;
  LResponseJson: TJSonObject;

  // Variables auxiliares
  Res: String;
begin
  // Verificación de mensaje
  if FMessages.Count = 0 then
    raise Exception.Create('No hay mensajes en el historial para realizar una búsqueda web.');

  LastMessage := GetLastMessage;
  if not Assigned(LastMessage) then
    raise Exception.Create('No se pudo obtener el último mensaje para la búsqueda web.');

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := LastMessage.Prompt;

  LBodyJson := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);

  try
    // --- 1. CONSTRUCCIÓN DEL CUERPO JSON PARA LA API DE /v1/responses ---

    // Parámetro 'model' (requerido)
    LBodyJson.AddPair('model', LModel); // Debe ser un modelo compatible

    // Parámetro 'input' - La API de 'Responses' usa 'input' con el prompt del usuario
    // A diferencia de 'messages', aquí solo se envía el último prompt.
    // Esta API es más "sin estado" (stateless) para la conversación.
    LBodyJson.AddPair('input', LastMessage.Prompt);

    // Parámetro 'tools' - Aquí activamos la búsqueda web
    LToolsArray := TJSonArray.Create;
    LToolObject := TJSonObject.Create;
    // Según tu ejemplo, el tipo para esta API es 'web_search_preview'
    LToolObject.AddPair('type', 'web_search_preview');
    LToolsArray.Add(LToolObject);
    LBodyJson.AddPair('tools', LToolsArray);

    // Parámetros de control de generación (si la API los soporta, hay que verificar la documentación de /v1/responses)
    // Suponemos que soporta los más comunes.
    // LBodyJson.AddPair('temperature', TJSONNumber.Create(Temperature));
    // LBodyJson.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    // La API de 'responses' puede no soportar streaming. Asumimos que no por ahora.
    // Si lo soportara, se añadiría aquí: LBodyJson.AddPair('stream', TJSONBool.Create(True));

    // Convertir el JSON a string y prepararlo para el envío
    Res := UTF8ToString(LBodyJson.ToJSon);
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Res := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
    LBodyStream.WriteString(Res);
    LBodyStream.Position := 0;

    // Guardar para depuración (opcional)
{$IFDEF APIDEBUG}
    LBodyStream.SaveToFile('c:\temp\peticion_responses_websearch.json.txt');
    LBodyStream.Position := 0;
{$ENDIF}
    // --- 2. EJECUCIÓN DE LA PETICIÓN HTTP AL ENDPOINT CORRECTO ---
    LUrl := Url + 'responses'; // <<< APUNTANDO AL NUEVO ENDPOINT
    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    FResponse.Clear;
    FResponse.Position := 0;

    LResponse := FClient.Post(LUrl, LBodyStream, FResponse, LHeaders);

    // --- 3. PROCESAMIENTO DE LA RESPUESTA ---
    FLastContent := '';

    FResponse.Position := 0;
{$IFDEF APIDEBUG}
    FResponse.SaveToFile('c:\temp\respuesta_responses_websearch.json.txt');
{$ENDIF}
    // Asumimos que la respuesta de este endpoint no es por stream (síncrona)
    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSonObject(TJSonObject.ParseJSONValue(LResponse.ContentAsString));
      try
        FBusy := False;
        // La estructura de la respuesta de /v1/responses puede ser diferente a la de /v1/chat/completions.
        // Necesitaremos un método de parseo adaptado. Por ahora, podemos intentar reutilizar
        // ParseChat, pero es probable que necesitemos una nueva función `ParseResponse`.
        // Vamos a asumir por ahora que `ParseChat` puede manejarlo o lo adaptaremos.
        ParseResponse(LResponseJson, ResMsg); // ATENCIÓN: Esto podría necesitar ajustes.
        Result := FLastContent;
      finally
        FreeAndNil(LResponseJson);
      end;
    end
    else
    begin
      FBusy := False;
      Raise Exception.CreateFmt('Error desde el endpoint /v1/responses: %d, %s', [LResponse.StatusCode, LResponse.ContentAsString]);
    end;

  finally
    // Liberar todos los recursos creados
    LBodyJson.Free;
    LBodyStream.Free;
  end;
end;

constructor TAiOpenChat.Create(Sender: TComponent);
begin
  inherited;

  ApiKey := '@OPENAI_API_KEY';
  Model := 'gpt4-o';
  n := 1;
  Response_format := TAiChatResponseFormat.tiaChatRfText;
  Temperature := 1;
  User := 'user';
  InitialInstructions.Text := 'Eres un asistente muy útil y servicial';
  Max_tokens := 300;
  Url := GlOpenAIUrl;
  Top_p := 1;
  ResponseTimeOut := 60000;
  Voice := 'alloy';
  voice_format := 'mp3';
  FStore := False; // no almacene la información para modelos de distilación o evaluaciones
  FParallel_ToolCalls := True; // Por defecto realiza el llamado en paralelo, esto ahorra tiempo en las respuestas
  FService_Tier := 'auto'; // posibles valore auto y default  ver API documentación.
end;

destructor TAiOpenChat.Destroy;
begin
  inherited;
end;

procedure TAiOpenChat.DoCallFunction(ToolCall: TAiToolsFunction);
Var
  LFuncion: TFunctionActionItem;
  Handle: Boolean;
begin
  If Not Assigned(AiFunctions) then
    Exit;

  LFuncion := AiFunctions.Functions.GetFunction(ToolCall.Name);

  If Assigned(LFuncion) then
  Begin
    LFuncion.OnAction(Self, LFuncion, ToolCall.Name, ToolCall, Handle);
    If Handle = False then
    Begin
      If Assigned(FOnCallToolFunction) then
        FOnCallToolFunction(Self, ToolCall)
    End;
  End
  Else
  Begin
    If Assigned(FOnCallToolFunction) then
      FOnCallToolFunction(Self, ToolCall)
  End;
end;

function TAiOpenChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  jObj, Msg, jFunc, Arg: TJSonObject;
  JVal, JVal1: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
  Nom, Valor: String;
  I: integer;
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
        If jObj.GetValue<String>('type') = 'function' then
        Begin
          jObj := TJSonObject(JVal);
          Fun := TAiToolsFunction.Create;
          Fun.Id := jObj.GetValue<String>('id');
          Fun.Tipo := jObj.GetValue<String>('type');

          If jObj.TryGetValue<TJSonObject>('function', jFunc) then
          Begin
            // Fun.Name := jObj.GetValue<TJSonObject>('function').GetValue<String>('name');
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

class function TAiOpenChat.GetModels(aApiKey, aUrl: String): TStringList;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl, EndPointUrl: String;
  jRes: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSonValue;
  sModel: string;
  CustomModels: TArray<string>;
  I: integer;
begin
  Result := TStringList.Create;

  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlOpenAIUrl;

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'models';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      jRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      If jRes.TryGetValue<TJSonArray>('data', JArr) then
      Begin
        For JVal in JArr do
        Begin
          sModel := JVal.GetValue<String>('id');
          If sModel <> '' then
            Result.Add(sModel);
        End;
      End;

      // Agregar modelos personalizados
      CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);

      for I := Low(CustomModels) to High(CustomModels) do
      begin
        if not Result.Contains(CustomModels[I]) then
          Result.Add(CustomModels[I]);
      end;

    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiOpenChat.GetModels: TStringList;
begin
  Result := GetModels(ApiKey, Url);
end;

function TAiOpenChat.GetTools: TStrings;
begin
  If Assigned(AiFunctions) and Tool_Active then // Si utiliza tools functions
  Begin
    FTools.Text := AiFunctions.GetTools;
    Result := FTools;
  End
  Else
  Begin
    FTools.Text := '';
    Result := FTools;
  End;
end;


// En uMakerAi.Chat.OpenAi.pas

function TAiOpenChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice, jAudio, jStrOptions, jResponseFormatObj: TJSonObject;
  JArr, jModalities: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: integer;
  LAsincronico: Boolean;
  Msg: TAiChatMessage;
  MediaFile: TAiMediaFile;
  ActiveFileId: String;
  Res: String;
  MessagesJson, NewContentArray: TJSonArray;
  LastMessageJson, JTextObj, JImageObj: TJSonObject;
  LModel: String;
begin
  // Inicialización de variables y valores por defecto
  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := 'gpt-4o'; // Un buen modelo por defecto que maneja multimodalidad

  // Las funciones y la generación de audio contextual no suelen funcionar en modo stream
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active) and (Not(Tfc_Audio in NativeOutputFiles)) and
    (Not(Tfc_Image in NativeOutputFiles)) and (Not(Tfc_Video in NativeOutputFiles));

  FClient.Asynchronous := LAsincronico;

  // Creamos los objetos principales que usaremos
  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  try

    for I := FMessages.Count - 1 downto 0 do
    begin
      Msg := FMessages[I];
      if Assigned(Msg.MediaFiles) then
        for MediaFile in Msg.MediaFiles do
          if not MediaFile.IdFile.IsEmpty then // Usamos IdFile como indicador
          begin
            ActiveFileId := MediaFile.IdFile;
            Break;
          end;
      if not ActiveFileId.IsEmpty then
        Break;
    end;

    MessagesJson := GetMessages;

    // Si encontramos un archivo activo y hay mensajes, modificamos el último mensaje
    if (not ActiveFileId.IsEmpty) and (MessagesJson.Count > 0) then
    begin
      LastMessageJson := MessagesJson.Items[MessagesJson.Count - 1] as TJSonObject;

      // Creamos un nuevo array de 'content' para el formato multimodal
      NewContentArray := TJSonArray.Create;

      // 1. Añadimos el texto del prompt original
      JTextObj := TJSonObject.Create;
      JTextObj.AddPair('type', 'text');
      JTextObj.AddPair('text', LastMessageJson.GetValue<string>('content'));
      NewContentArray.Add(JTextObj);

      // 2. Añadimos la referencia a la imagen/archivo subido
      JImageObj := TJSonObject.Create;
      JImageObj.AddPair('type', 'image_url'); // Para OpenAI, incluso los PDFs se tratan como 'image_url' con un file_id
      JImageObj.AddPair('image_url', TJSonObject.Create.AddPair('url', 'data:file_id:' + ActiveFileId));
      NewContentArray.Add(JImageObj);

      // Reemplazamos el 'content' antiguo (string) por el nuevo 'content' (array)
      LastMessageJson.RemovePair('content');
      LastMessageJson.AddPair('content', NewContentArray);
    end;

    // --- 1. PARÁMETROS BÁSICOS DE LA PETICIÓN ---
    AJSONObject.AddPair('model', LModel);
    AJSONObject.AddPair('messages', MessagesJson);
    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    // Opciones de streaming si están activas
    if (LAsincronico = True) and (Stream_Usage = True) then
    begin
      jStrOptions := TJSonObject.Create;
      jStrOptions.AddPair('include_usage', Stream_Usage);
      AJSONObject.AddPair('stream_options', jStrOptions);
    end;

    // --- 2. CONFIGURACIÓN DE HERRAMIENTAS (FUNCTION CALLING) ---
    if Tool_Active and (Trim(Tools.Text) <> '') then
    begin
      try
        JArr := TJSonArray.ParseJSONValue(Tools.Text) as TJSonArray;
        if not Assigned(JArr) then
          Raise Exception.Create('La propiedad Tools tiene un formato JSON inválido.')
        else
        begin
          AJSONObject.AddPair('tools', JArr);
          AJSONObject.AddPair('parallel_tool_calls', FParallel_ToolCalls);

          if (Trim(Tool_choice) <> '') then
          begin
            // Asumimos que Tool_choice puede ser un string 'auto', 'none' o un objeto JSON
            try
              jToolChoice := TJSonObject.ParseJSONValue(Tool_choice) as TJSonObject;
              AJSONObject.AddPair('tool_choice', jToolChoice);
            except
              // Si no es un objeto JSON, lo tratamos como un string simple
              AJSONObject.AddPair('tool_choice', Tool_choice);
            end;
          end;
        end;
      except
        on E: Exception do
          Raise Exception.Create('Error al procesar la propiedad Tools: ' + E.Message);
      end;
    end;

    // --- 3. CONFIGURACIÓN DE GENERACIÓN Y MODALIDADES ---

    If ReasoningEffort <> '' then // OpenAi reasoning effort
    Begin
      // var jeffort := TJSonObject.Create;
      // jeffort.AddPair('effort', ReasoningEffort);
      // AJSONObject.AddPair('reasoning', jeffort);

      AJSONObject.AddPair('max_completion_tokens', TJSONNumber.Create(Max_tokens));
    End
    Else
      AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('store', FStore); // Para modelos de evaluación

    // --- LÓGICA CLAVE: DETERMINAR EL TIPO DE RESPUESTA ---
    if (Tfc_Audio in NativeOutputFiles) then
    begin
      // Escenario: Chat con respuesta de AUDIO CONTEXTUAL
      jModalities := TJSonArray.Create;
      jModalities.Add('text');
      jModalities.Add('audio');
      AJSONObject.AddPair('modalities', jModalities);

      jAudio := TJSonObject.Create;
      jAudio.AddPair('voice', Voice);
      jAudio.AddPair('format', voice_format);
      AJSONObject.AddPair('audio', jAudio);
    end
    else if Tcm_Image in ChatMediaSupports then // Adiciona los parámetros de imágen
    Begin
      AJSONObject.AddPair('size', '1024x1024');
      var
      LToolsArray := TJSonArray.Create;
      var
      LToolObject := TJSonObject.Create;

      LToolObject.AddPair('type', 'image_generation');
      LToolsArray.Add(LToolObject);
      AJSONObject.AddPair('tools', LToolsArray);

    End
    Else
    begin
      // Escenario: Chat de solo TEXTO o solo JSON
      jResponseFormatObj := TJSonObject.Create;
      case FResponse_format of
        tiaChatRfJson:
          jResponseFormatObj.AddPair('type', 'json_object');
        tiaChatRfJsonSchema:
          jResponseFormatObj.AddPair('type', 'json_schema');
      else // tiaChatRfText y por defecto
        jResponseFormatObj.AddPair('type', 'text');
      end;
      AJSONObject.AddPair('response_format', jResponseFormatObj);
    end;

    // --- 4. PARÁMETROS ADICIONALES ---
    // Secuencias de parada
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      JStop := TJSonArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    end;

    // Configuración de Logprobs
    if Logprobs = True then
    begin
      AJSONObject.AddPair('logprobs', TJSONBool.Create(Logprobs));
      if Logit_bias <> '' then
        AJSONObject.AddPair('logit_bias', TJSONNumber.Create(Logit_bias));
      if Top_logprobs <> '' then
        AJSONObject.AddPair('top_logprobs', TJSONNumber.Create(Top_logprobs));
    end;

    // Semilla para reproducibilidad
    if Seed > 0 then
      AJSONObject.AddPair('seed', TJSONNumber.Create(Seed));

    if tcm_WebSearch in ChatMediaSupports then
    begin
      // La API de OpenAI espera un objeto para las opciones, incluso si está vacío.
      Var
      jWebSearchOptions := TJSonObject.Create;
      AJSONObject.AddPair('web_search_options', jWebSearchOptions);
    end
    Else
    Begin
      if Top_p <> 0 then
        AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

      AJSONObject.AddPair('temperature', TJSONNumber.Create(Temperature));
      AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Frequency_penalty));
      AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Presence_penalty));
      AJSONObject.AddPair('n', TJSONNumber.Create(n));

    End;

    // --- 5. FINALIZACIÓN Y RETORNO ---
    // Obtenemos el string JSON final. Usamos ToString que devuelve UTF8String
    // y luego convertimos a String de Delphi (Unicode).
    Res := UTF8ToString(AJSONObject.ToJSon);

    // Opcional: limpiar el JSON para algunos casos (como barras invertidas)
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);

  finally
    // Liberación de memoria para evitar fugas
    AJSONObject.Free;
    Lista.Free;
  end;
end;

{
  function TAiOpenChat.InitChatCompletions: String;
  Var
  AJSONObject, jToolChoice, jAudio, jStrOptions: TJSonObject;
  JArr, jModalities: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: integer;
  LAsincronico: Boolean;
  Res: String;
  begin

  If User = '' then
  User := 'user';

  If Model = '' then
  Model := 'gpt-4o';

  // Las funciones no trabajan en modo ascincrono
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  // estas líneas no hacen falta, se dejan como gúia para proximas implementaciones
  // LastMsg := Messages.Last;
  // If Assigned(LastMsg) then
  //  Begin
  //  LAsincronico := LAsincronico and (Not(LastMsg.VisionUrls.Count > 0) or (LastMsg.VisionBase64.Count > 0));
  //  End;


  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

  AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

  If (LAsincronico = True) and (Stream_Usage = True) then
  Begin
  jStrOptions := TJSonObject.Create;
  jStrOptions.AddPair('include_usage', Stream_Usage);
  AJSONObject.AddPair('stream_options', jStrOptions);
  End;

  AJSONObject.AddPair('store', FStore);

  If Tool_Active and (Trim(Tools.Text) <> '') then
  Begin
  JArr := TJSonArray(TJSonArray.ParseJSONValue(Tools.Text));
  If Not Assigned(JArr) then
  Raise Exception.Create('La propiedad Tools están mal definido, debe ser un JsonArray');
  AJSONObject.AddPair('tools', JArr);

  AJSONObject.AddPair('parallel_tool_calls', FParallel_ToolCalls);

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

  AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(Frequency_penalty * 100) / 100));
  AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(Presence_penalty * 100) / 100));
  AJSONObject.AddPair('user', User);
  AJSONObject.AddPair('n', TJSONNumber.Create(N));

  // Formato de salida puede generar texto y audio

  If NativeOutputFiles <> [] then
  Begin
  jModalities := TJSonArray.Create;

  If Tfc_Audio in NativeOutputFiles then
  Begin
  jModalities.Add('text');
  jModalities.Add('audio');
  End
  Else If Tfc_Text in NativeOutputFiles then
  jModalities.Add('text');

  AJSONObject.AddPair('modalities', jModalities);
  End;

  If Tfc_Audio in NativeOutputFiles then
  Begin
  jAudio := TJSonObject.Create;
  jAudio.AddPair('voice', Voice);
  jAudio.AddPair('format', voice_format);
  AJSONObject.AddPair('audio', jAudio);
  End;


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

  Res := AJSONObject.ToString;
  Result := UTF8ToString(Res);

  Finally
  // AJSONObject.Free;
  Lista.Free;
  End;
  end;

}

procedure AppendTextToFile(const FilePath, TextToAppend: string);
var
  LogFile: TextFile;
begin
  // Asignamos el archivo
  AssignFile(LogFile, FilePath);

  try
    // Abrimos el archivo en modo "append" para añadir al final
    if FileExists(FilePath) then
      Append(LogFile) // Abrimos en modo "append"
    else
      Rewrite(LogFile); // Creamos el archivo si no existe

    // Escribimos el texto al final del archivo
    WriteLn(LogFile, TextToAppend);
  finally
    // Cerramos el archivo
    CloseFile(LogFile);
  end;
end;

procedure TAiOpenChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  jObj, Delta: TJSonObject;
  sJson, Value, Role1: String;
  P: integer;
  Msg: TAiChatMessage;
begin

  If FClient.Asynchronous = False then
    Exit;

  AAbort := FAbort;

  If FAbort = True then
  Begin
    FBusy := False;
    If Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, Nil, Nil, 'system', 'abort');
  End;

  Try
    FTmpResponseText := FTmpResponseText + FResponse.DataString;
    FTmpResponseText1 := FTmpResponseText1 + FResponse.DataString;

    // AppendTextToFile('C:\temp\logdata.txt', FResponse.DataString);

    FResponse.Clear;
    FResponse.Position := 0;

    If Copy(FTmpResponseText, 1, 5) = 'data:' then
      FTmpResponseText := Copy(FTmpResponseText, 6, Length(FTmpResponseText));

    Repeat
      P := Pos('data:', FTmpResponseText);
      If P > 0 then
      Begin
        sJson := Trim(Copy(FTmpResponseText, 1, P - 1));
        FTmpResponseText := Copy(FTmpResponseText, P + 6, Length(FTmpResponseText));
      End
      Else
      Begin
        If Trim(FTmpResponseText) = '[DONE]' then // Terminó el proceso
        Begin
          sJson := Trim(FTmpResponseText);
          FTmpResponseText := '';
        End
        Else
          sJson := '';
      End;

      If sJson = '[DONE]' then // Terminó el proceso
      Begin
        sJson := '';
        Msg := TAiChatMessage.Create(FLastContent, FTmpRole);
        Msg.Id := FMessages.Count + 1;
        FMessages.Add(Msg);
        FBusy := False;

        If Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, Msg, Nil, FTmpRole, FLastContent);
      End
      Else If sJson <> '' then
      Begin
        jObj := TJSonObject(TJSonObject.ParseJSONValue(sJson));

        Try
          If Assigned(jObj) then
          Begin
            Delta := jObj.GetValue<TJSonArray>('choices')[0].GetValue<TJSonObject>('delta');
            Value := '';
            Delta.TryGetValue<String>('content', Value);
            Delta.TryGetValue<String>('role', Role1);

            If Role1 <> '' then
              FTmpRole := Role1;

            FLastContent := FLastContent + Value;

            If (Value <> '') and Assigned(FOnReceiveDataEvent) then
            Begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              FOnReceiveDataEvent(Self, Nil, jObj, FTmpRole, Value);
            End;
          End;
        Finally
          jObj.Free;
        End;
      End;

    Until sJson = '';

  Except

  End;
end;



// En uMakerAi.Chat.OpenAi.pas

procedure TAiOpenChat.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
var
  // JSON parsing
  choices, JToolCalls: TJSonArray;
  JItem, jMessage, uso, jAudio: TJSonObject;
  JVal: TJSonValue;

  // Datos del mensaje
  Role, Respuesta, sRes, sToolCalls: String;
  idAudio, AudioBase64, AudioTranscript: String;
  AudioExpiresAt: integer;

  // Tokens
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: integer;

  // Objetos del Framework
  ToolMsg, AskMsg: TAiChatMessage; // ResMsg es el mensaje de resultado, AskMsg es el mensaje del usuario con la solicitud
  MediaFile: TAiMediaFile;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  // Tareas para Function Calling
  TaskList: array of ITask;
  I, NumTasks: integer;
  Clave: String;

  jWebSearch, jAnnotationItem: TJSonObject;
  jAnnotationItemValue: TJSonValue;
  jAnnotations: TJSonArray;
  LItem: TAiWebSearchItem;
  LModel: String;

  Code: TMarkdownCodeExtractor;
  CodeFile: TCodeFile;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;

begin
  AskMsg := GetLastMessage; // Obtiene el mensaje de la solicitud

  // --- 1. EXTRACCIÓN DE METADATOS Y TOKENS ---
  LModel := jObj.GetValue<string>('model');

  // El objeto 'usage' puede no venir en respuestas con errores
  if jObj.TryGetValue<TJSonObject>('usage', uso) then
  begin
    aPrompt_tokens := uso.GetValue<integer>('prompt_tokens');
    aCompletion_tokens := uso.GetValue<integer>('completion_tokens');
    aTotal_tokens := uso.GetValue<integer>('total_tokens');
  end
  else
  begin
    aPrompt_tokens := 0;
    aCompletion_tokens := 0;
    aTotal_tokens := 0;
  end;

  // --- 2. PROCESAMIENTO DE LAS 'CHOICES' (RESPUESTAS) ---
  if not jObj.TryGetValue<TJSonArray>('choices', choices) or (choices.Count = 0) then
  begin
    // Si no hay 'choices', no hay nada que procesar.
    FLastError := 'La respuesta de la API no contiene ninguna "choice".';
    DoError(FLastError, nil);
    FBusy := False;
    Exit;
  end;

  // Creamos el mensaje que representará la respuesta del asistente.
  // Se poblará a medida que analicemos el JSON.
  // Msg := TAiChatMessage.Create('', ''); //Se crea en el run
  try
    // Normalmente solo procesamos la primera 'choice' (index 0), pero un bucle es más robusto.
    for JVal in choices do
    begin
      JItem := JVal as TJSonObject;
      if not JItem.TryGetValue<TJSonObject>('message', jMessage) then
        Continue; // Ir a la siguiente 'choice' si esta no tiene un mensaje

      // --- 2.1. Extraer los componentes del mensaje ---
      Role := jMessage.GetValue<string>('role');
      Respuesta := '';
      sToolCalls := '';

      // Primero, buscamos si hay contenido de texto.
      if jMessage.TryGetValue<string>('content', sRes) then
        Respuesta := Respuesta + sRes;

      // Segundo, buscamos si hay contenido de audio.
      if jMessage.TryGetValue<TJSonObject>('audio', jAudio) then
      begin
        AudioBase64 := '';
        AudioTranscript := ''; // Inicializamos

        jAudio.TryGetValue<string>('id', idAudio);
        jAudio.TryGetValue<string>('data', AudioBase64);
        jAudio.TryGetValue<integer>('expires_at', AudioExpiresAt);
        jAudio.TryGetValue<string>('transcript', AudioTranscript);

        // Si hay datos de audio, creamos un MediaFile y lo adjuntamos.
        if not AudioBase64.IsEmpty then
        begin
          MediaFile := TAiMediaFile.Create;
          MediaFile.LoadFromBase64('response_audio.' + voice_format, AudioBase64);
          MediaFile.Transcription := AudioTranscript;
          MediaFile.idAudio := idAudio;
          ResMsg.AddMediaFile(MediaFile);
        end;

        // Si hay una transcripción, la usamos como (o la añadimos a) la respuesta de texto.
        if not AudioTranscript.IsEmpty then
        begin
          if not Respuesta.IsEmpty then
            Respuesta := Respuesta + sLineBreak; // Añadir separador si ya había texto
          Respuesta := Respuesta + AudioTranscript;
        end;
      end;

      // Tercero, buscamos si hay llamadas a funciones.
      if jMessage.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
        sToolCalls := JToolCalls.ToJSon; // Guardamos el JSON de las tool_calls

      if jMessage.TryGetValue<TJSonObject>('web_search', jWebSearch) then
      begin
        // El objeto WebSearchResponse ya está creado en el constructor de TAiChatMessage
        if Assigned(ResMsg.WebSearchResponse) then
        begin
          // Poblamos los datos principales del objeto de búsqueda
          jWebSearch.TryGetValue<string>('type', ResMsg.WebSearchResponse.&type);
          jWebSearch.TryGetValue<string>('text', ResMsg.WebSearchResponse.Text);

          // Ahora, procesamos las anotaciones (los enlaces encontrados)
          if jWebSearch.TryGetValue<TJSonArray>('annotations', jAnnotations) then
          begin
            ResMsg.WebSearchResponse.annotations.Clear; // Limpiamos por si acaso
            for jAnnotationItemValue in jAnnotations do
            begin
              if not(jAnnotationItemValue is TJSonObject) then
                Continue;

              jAnnotationItem := jAnnotationItemValue as TJSonObject;
              LItem := TAiWebSearchItem.Create;
              try
                jAnnotationItem.TryGetValue<string>('type', LItem.&type);
                jAnnotationItem.TryGetValue<integer>('start_index', LItem.start_index);
                jAnnotationItem.TryGetValue<integer>('end_index', LItem.end_index);
                jAnnotationItem.TryGetValue<string>('url', LItem.Url);
                jAnnotationItem.TryGetValue<string>('title', LItem.title);
                ResMsg.WebSearchResponse.annotations.Add(LItem);
              except
                LItem.Free; // Liberar memoria si hay un error al poblar el item
                raise;
              end;
            end;
          end;
        end;
      end;

    end;

    // --- 3. CONSTRUCCIÓN Y ALMACENAMIENTO DEL MENSAJE DE RESPUESTA ---
    Respuesta := Trim(Respuesta);

    // Actualizamos los contadores de tokens globales del componente.
    Self.Prompt_tokens := Self.Prompt_tokens + aPrompt_tokens;
    Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
    Self.Total_tokens := Self.Total_tokens + aTotal_tokens;
    Self.FLastContent := Respuesta;

    if sToolCalls.IsEmpty then // Si es una respuesta normal adiciona los datos al ResMsg
    Begin
      ResMsg.Role := Role;
      ResMsg.Tool_calls := sToolCalls;
      ResMsg.Prompt := ResMsg.Prompt + Respuesta;
      ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aPrompt_tokens;
      ResMsg.Completion_tokens := ResMsg.Completion_tokens + aCompletion_tokens;
      ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;
      DoProcessResponse(AskMsg, ResMsg, Respuesta);
    End
    Else // Si tiene toolcall lo adiciona y ejecuta nuevamente el run para obtener la respuesta
    Begin
      Var
      Msg := TAiChatMessage.Create(Respuesta, Role);
      Msg.Tool_calls := sToolCalls;
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);
    End;



    // Poblamos el objeto de mensaje final con todos los datos recopilados.
    { ResMsg.Prompt := Trim(ResMsg.Prompt + sLineBreak + Respuesta);
      ResMsg.Content := Trim(ResMsg.Prompt + sLineBreak + Respuesta);
      ResMsg.Role := Role;
      ResMsg.Tool_calls := sToolCalls;
      ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aPrompt_tokens;
      ResMsg.Completion_tokens := ResMsg.Completion_tokens + aCompletion_tokens;
      ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;
    }

    // OJO ya no se adiciona aquí,  se adiciona en el run
    // Msg.Id := FMessages.Count + 1;
    // FMessages.Add(Msg);

    // --- 4. MANEJO DE FUNCTION CALLING ---
    LFunciones := ExtractToolCallFromJson(choices);
    try
      if (LFunciones <> nil) and (LFunciones.Count > 0) then
      begin
        // El modelo quiere llamar a una o más funciones.
        NumTasks := LFunciones.Count;
        SetLength(TaskList, NumTasks);
        I := 0;

        // Creamos y lanzamos una tarea para cada llamada a función.
        for Clave in LFunciones.Keys do
        begin
          ToolCall := LFunciones[Clave];
          ToolCall.ResMsg := ResMsg;
          ToolCall.AskMsg := AskMsg;

          TaskList[I] := TTask.Create(
            procedure
            begin
              try
                DoCallFunction(ToolCall);
              except
                on E: Exception do
                  TThread.Queue(nil,
                    procedure
                    begin
                      DoError('Error en la ejecución de la función "' + ToolCall.Name + '"', E);
                    end);
              end;
            end);
          TaskList[I].Start;
          Inc(I);
        end;

        // Esperamos a que todas las funciones terminen.
        TTask.WaitForAll(TaskList);

        // Creamos los mensajes de respuesta de tipo 'tool'.
        for Clave in LFunciones.Keys do
        begin
          ToolCall := LFunciones[Clave];
          ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
          ToolMsg.Id := FMessages.Count + 1;
          FMessages.Add(ToolMsg);
        end;

        // Volvemos a llamar a la API para que procese los resultados de las funciones.
        Self.Run(Nil, ResMsg);
      end
      else
      begin
        // Si no hay llamadas a funciones, la conversación termina aquí.

        // Permitir al usuario final procesar o modificar la respuesta antes de guardarla.
        // Se pasa el mensaje de solicitud y el mensaje de respuesta más la respuesta del modelo en texto

        If tfc_textFile in NativeOutputFiles then
        Begin
          Code := TMarkdownCodeExtractor.Create;
          Try

            CodeFiles := Code.ExtractCodeFiles(Respuesta);
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

        DoProcessResponse(AskMsg, ResMsg, Respuesta);

        ResMsg.Prompt := Respuesta;

        FBusy := False;
        if Assigned(FOnReceiveDataEnd) then
        Begin
          FOnReceiveDataEnd(Self, ResMsg, jObj, Role, Respuesta);
        End;
      end;
    finally
      if Assigned(LFunciones) then
        LFunciones.Free;
    end;

  except
    // Si ocurrió alguna excepción durante el proceso, liberamos el mensaje
    // que estábamos creando para evitar fugas de memoria.
    // if Assigned(Msg) then
    // Msg.Free;
    // Marcamos como no ocupado y relanzamos la excepción.
    FBusy := False;
    raise;
  end;
end;

procedure TAiOpenChat.ParseJsonTranscript(jObj: TJSonObject; ResMsg: TAiChatMessage; aMediaFile: TAiMediaFile);
var
  // JSON parsing
  jUsage, jInputTokenDetails: TJSonObject;
  jArrWords, jArrSegments: TJSonArray;

  // Datos extraídos
  sTextoTranscrito, sTextoWords, sTextoSegments: String;
  aTotal_tokens, aInput_tokens, aOutput_tokens: integer;
  aText_tokens, aAudio_tokens: integer; // Tokens detallados del input

begin
  sTextoTranscrito := '';
  aTotal_tokens := 0;
  aInput_tokens := 0;
  aOutput_tokens := 0;
  aText_tokens := 0;
  aAudio_tokens := 0;

  if not jObj.TryGetValue<string>('text', sTextoTranscrito) then
  begin
    FLastError := 'La respuesta de la API no contiene el campo "text" con la transcripción.';
    DoError(FLastError, nil);
    FBusy := False; // Asumiendo que usas FBusy como en ParseChat
    Exit;
  end;

  If jObj.TryGetValue<TJSonArray>('words', jArrWords) then
    sTextoWords := jArrWords.Format;

  If jObj.TryGetValue<TJSonArray>('segments', jArrSegments) then
    sTextoSegments := jArrSegments.Format;

  // --- 3. EXTRACCIÓN DE DATOS DE USO (TOKENS) ---
  // El objeto 'usage' podría no venir, así que lo manejamos de forma segura.
  if jObj.TryGetValue<TJSonObject>('usage', jUsage) then
  begin
    // Extraemos los tokens principales
    jUsage.TryGetValue<integer>('total_tokens', aTotal_tokens);
    jUsage.TryGetValue<integer>('input_tokens', aInput_tokens); // Costo del audio (y prompt si hubo)
    jUsage.TryGetValue<integer>('output_tokens', aOutput_tokens); // Costo del texto generado

    // Extraemos los detalles de los tokens de entrada (sub-objeto)
    // Esto nos dice cuántos tokens correspondieron al audio y cuántos a un posible prompt de texto.
    if jUsage.TryGetValue<TJSonObject>('input_token_details', jInputTokenDetails) then
    begin
      jInputTokenDetails.TryGetValue<integer>('text_tokens', aText_tokens);
      jInputTokenDetails.TryGetValue<integer>('audio_tokens', aAudio_tokens);
    end;
  end;

  // --- 4. ACTUALIZACIÓN DEL ESTADO DEL COMPONENTE ---
  // Actualizamos los contadores de tokens globales, sumando los de esta llamada.
  Self.Total_tokens := Self.Total_tokens + aTotal_tokens;
  Self.Prompt_tokens := Self.Prompt_tokens + aInput_tokens; // 'input' equivale a 'prompt'
  Self.Completion_tokens := Self.Completion_tokens + aOutput_tokens; // 'output' equivale a 'completion'

  // Guardamos el resultado principal
  // Self.FLastContent := sTextoTranscrito;

  If Trim(sTextoWords + sLineBreak + sTextoSegments) <> '' then
  Begin
    aMediaFile.Transcription := Trim(sTextoWords + sLineBreak + sTextoSegments);
    aMediaFile.Detail := Trim(sTextoTranscrito);
    ResMsg.Prompt := Trim(ResMsg.Prompt + aMediaFile.Transcription);
    ResMsg.Content := ResMsg.Content + sLineBreak + aMediaFile.Detail;
  End
  Else
  Begin
    aMediaFile.Transcription := sTextoTranscrito;
    ResMsg.Prompt := Trim(ResMsg.Prompt + sLineBreak + sTextoTranscrito);
    ResMsg.Content := Trim(ResMsg.Content + sLineBreak + sTextoTranscrito);
  End;

  ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aInput_tokens;
  ResMsg.Completion_tokens := ResMsg.Completion_tokens + aOutput_tokens;
  ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;
end;

// en uMakerAi.Chat.OpenAi.pas

procedure TAiOpenChat.ParseResponse(jObj: TJSonObject; ResMsg: TAiChatMessage);
var
  S, ResponseId, ImageBase64, RevisedPrompt: String;
  JOutput, jAnnotations, JContent: TJSonArray;
  JOutputItem, JContentItem, jAnnotationItem, jUsage: TJSonObject;
  WebSearch: TAiWebSearch;
  WebSearchItem: TAiWebSearchItem;
  MediaFile: TAiMediaFile;
  I, j, k: integer;
  aInput_tokens, aOutput_tokens, aTotal_tokens: integer;
begin
  if not Assigned(jObj) then
    Exit;

  // --- 1. Inicialización y extracción de datos principales ---
  FLastContent := '';
  WebSearch := nil;
  ResponseId := '';
  aInput_tokens := 0;
  aOutput_tokens := 0;
  aTotal_tokens := 0;

  try
    // Extraer el ResponseId del nivel superior
    jObj.TryGetValue<String>('id', ResponseId);

    // Extraer los datos de uso (tokens)
    if jObj.TryGetValue<TJSonObject>('usage', jUsage) then
    begin
      jUsage.TryGetValue<integer>('input_tokens', aInput_tokens);
      jUsage.TryGetValue<integer>('output_tokens', aOutput_tokens);
      jUsage.TryGetValue<integer>('total_tokens', aTotal_tokens);
    end;

    // --- 2. Procesar el array 'output' ---
    if jObj.TryGetValue<TJSonArray>('output', JOutput) then
    begin
      for I := 0 to JOutput.Count - 1 do
      begin
        if not(JOutput.Items[I] is TJSonObject) then
          Continue;
        JOutputItem := JOutput.Items[I] as TJSonObject;

        if not JOutputItem.TryGetValue<String>('type', S) then
          Continue;

        // --- MANEJO DE 'message' (TEXTO Y WEB SEARCH) ---
        if S = 'message' then
        begin
          if JOutputItem.TryGetValue<TJSonArray>('content', JContent) then
          begin
            for j := 0 to JContent.Count - 1 do
            begin
              if not(JContent.Items[j] is TJSonObject) then
                Continue;
              JContentItem := JContent.Items[j] as TJSonObject;

              if JContentItem.TryGetValue<string>('type', S) and (S = 'output_text') then
              begin
                JContentItem.TryGetValue<string>('text', FLastContent);

                if JContentItem.TryGetValue<TJSonArray>('annotations', jAnnotations) then
                begin
                  if not Assigned(WebSearch) then
                  begin
                    WebSearch := TAiWebSearch.Create;
                    WebSearch.annotations := TAiWebSearchArray.Create;
                  end;
                  WebSearch.Text := FLastContent;

                  for k := 0 to jAnnotations.Count - 1 do
                  begin
                    if not(jAnnotations.Items[k] is TJSonObject) then
                      Continue;
                    jAnnotationItem := jAnnotations.Items[k] as TJSonObject;
                    WebSearchItem := TAiWebSearchItem.Create;
                    try
                      jAnnotationItem.TryGetValue<string>('type', WebSearchItem.&type);
                      jAnnotationItem.TryGetValue<integer>('start_index', WebSearchItem.start_index);
                      jAnnotationItem.TryGetValue<integer>('end_index', WebSearchItem.end_index);
                      jAnnotationItem.TryGetValue<string>('url', WebSearchItem.Url);
                      jAnnotationItem.TryGetValue<string>('title', WebSearchItem.title);
                      WebSearch.annotations.Add(WebSearchItem);
                    except
                      WebSearchItem.Free;
                      raise;
                    end;
                  end;
                end;
                Break;
              end;
            end;
          end;
        end
        // --- NUEVO: MANEJO DE 'image_generation_call' ---
        else if S = 'image_generation_call' then
        begin
          ImageBase64 := '';
          RevisedPrompt := '';
          JOutputItem.TryGetValue<string>('result', ImageBase64);
          JOutputItem.TryGetValue<string>('revised_prompt', RevisedPrompt);

          if not ImageBase64.IsEmpty then
          begin
            MediaFile := TAiMediaFile.Create;
            try
              // La API devuelve un PNG por defecto. El nombre no es crucial.
              MediaFile.LoadFromBase64('generated_image.png', ImageBase64);
              // Usamos la propiedad 'Detail' para guardar el prompt revisado, una buena práctica.
              MediaFile.Detail := RevisedPrompt;
              ResMsg.AddMediaFile(MediaFile);
            except
              MediaFile.Free;
              raise;
            end;
          end;
        end;
      end;
    end;

    // --- 3. Poblar el objeto de mensaje de respuesta (ResMsg) ---
    FLastContent := Trim(FLastContent);
    // Si no se generó texto (solo una imagen), FLastContent estará vacío, lo cual es correcto.
    ResMsg.Prompt := FLastContent;
    ResMsg.Content := FLastContent;

    ResMsg.Role := 'assistant';
    ResMsg.PreviousResponseId := ResponseId;

    // Actualizar contadores de tokens en el mensaje y en el componente
    ResMsg.Prompt_tokens := aInput_tokens;
    ResMsg.Completion_tokens := aOutput_tokens;
    ResMsg.Total_tokens := aTotal_tokens;
    Self.Prompt_tokens := Self.Prompt_tokens + aInput_tokens;
    Self.Completion_tokens := Self.Completion_tokens + aOutput_tokens;
    Self.Total_tokens := Self.Total_tokens + aTotal_tokens;

    // Asignar los resultados de la búsqueda web
    if Assigned(WebSearch) then
    begin
      FreeAndNil(ResMsg.WebSearchResponse);
      ResMsg.WebSearchResponse := WebSearch;
      WebSearch := nil; // Evitar doble liberación
    end;

    // --- 4. Disparar eventos finales ---
    DoProcessResponse(GetLastMessage, ResMsg, FLastContent);
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, ResMsg, jObj, ResMsg.Role, FLastContent);

  except
    on E: Exception do
    begin
      if Assigned(WebSearch) then
        FreeAndNil(WebSearch);
      DoError('Error al parsear la respuesta del endpoint /v1/responses: ' + E.Message, E);
      raise;
    end;
  end;
end;

{
  procedure TAiOpenChat.ParseChat(jObj: TJSonObject);
  Var
  choices, JToolCalls: TJSonArray;
  JItem, jAudio: TJSonObject;
  JVal: TJSonValue;
  jMessage: TJSonObject;
  uso: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: integer;
  Role, Respuesta, idAudio: String;
  AudioExpiresAt: integer;
  Msg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  TaskList: array of ITask;
  I, NumTasks: integer;
  Clave, sToolCalls, sRes: String;
  AudioBase64: String;
  MediaFile: TAiMediaFile;

  begin

  // Id := JObj.GetValue('id').Value;
  // IdObject := JObj.GetValue('object').Value;
  // IdCreate := JObj.GetValue('created').GetValue<String>;
  Model := jObj.GetValue('model').Value;
  uso := jObj.GetValue('usage') as TJSonObject;
  aPrompt_tokens := uso.GetValue<integer>('prompt_tokens');
  aCompletion_tokens := uso.GetValue<integer>('completion_tokens');
  aTotal_tokens := uso.GetValue<integer>('total_tokens');

  jObj.TryGetValue<TJSonArray>('choices', choices);

  Msg := TAiChatMessage.Create('', '');

  For JVal in choices do
  Begin
  JItem := TJSonObject(JVal);
  jMessage := JItem.GetValue<TJSonObject>('message');
  Role := jMessage.GetValue<String>('role');

  If jMessage.TryGetValue<String>('content', sRes) then
  Respuesta := Respuesta + sRes + sLineBreak;

  If jMessage.TryGetValue<TJSonObject>('audio', jAudio) then
  Begin
  jAudio.TryGetValue<String>('id', idAudio);

  jAudio.TryGetValue<String>('data', AudioBase64);

  If jAudio.TryGetValue<String>('transcript', sRes) then
  Respuesta := Respuesta + sRes + sLineBreak;

  jAudio.TryGetValue<integer>('expires_at', AudioExpiresAt);

  If (AudioBase64 <> '') then
  Begin
  MediaFile := TAiMediaFile.Create;
  MediaFile.LoadFromBase64('archivo.' + voice_format, AudioBase64);
  MediaFile.Transcription := sRes;
  MediaFile.idAudio := idAudio;
  Msg.AddMediaFile(MediaFile);
  End;
  End;

  If jMessage.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
  sToolCalls := JToolCalls.Format;
  End;

  Respuesta := Trim(Respuesta);

  DoProcessResponse(GetLastMessage, Msg, Respuesta);

  Self.FLastContent := Respuesta;
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + aTotal_tokens;

  Msg.Prompt := Respuesta;
  Msg.Content := Respuesta;
  Msg.Role := Role;
  Msg.Tool_calls := sToolCalls;
  Msg.Prompt_tokens := aPrompt_tokens;
  Msg.Completion_tokens := aCompletion_tokens;
  Msg.Total_tokens := aTotal_tokens;
  Msg.Id := FMessages.Count + 1;

  FMessages.Add(Msg);

  // If Assigned(FOnAddMessage) then
  // FOnAddMessage(Self, jObj, Role, Respuesta);

  LFunciones := ExtractToolCallFromJson(choices);

  Try
  If LFunciones.Count > 0 then
  Begin

  NumTasks := LFunciones.Count;
  SetLength(TaskList, NumTasks);
  // Ajusta el tamaño del array para el número de tareas

  I := 0;
  For Clave in LFunciones.Keys do
  Begin
  ToolCall := LFunciones[Clave];

  TaskList[I] := TTask.Create(
  procedure
  begin
  DoCallFunction(ToolCall);
  end);
  TaskList[I].Start;
  Inc(I);

  End;
  TTask.WaitForAll(TaskList);

  For Clave in LFunciones.Keys do
  Begin
  ToolCall := LFunciones[Clave];
  Msg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
  Msg.Id := FMessages.Count + 1;
  FMessages.Add(Msg);
  End;

  Self.Run;

  End
  Else
  Begin
  FBusy := False;
  If Assigned(FOnReceiveDataEnd) then
  FOnReceiveDataEnd(Self, Msg, jObj, Role, Respuesta);
  End;
  Finally
  LFunciones.Free;
  End;
  end;

}

function TAiOpenChat.PrepareSystemMsg: String;
Var
  S, Key, Val, MensajeInicial: String;
  I: integer;
  JMemory: TJSonObject;
begin
  // Si el formato de respuesta es Json, siempre debe llevar en la instrucción que el formato sea json
  If Self.Response_format = TAiChatResponseFormat.tiaChatRfJson then
    S := 'Responde en formato json'
  Else
    S := '';

  MensajeInicial := InitialInstructions.Text + sLineBreak + S;

  JMemory := TJSonObject.Create;
  Try
    Try
      For I := 0 to Memory.Count - 1 do
      Begin
        Key := Memory.KeyNames[I];
        Val := Memory.Values[Key];
        JMemory.AddPair(Key, Val);
      End;
    Except
      ON E: Exception do
      Begin
        Raise Exception.Create('El formato de memoria debe ser Key=Value, no está bien configurado');
      End;
    End;

    If Assigned(OnInitChat) then // Da la oportunidad de inicializar el chat con parametros adicionales como la memoria
      OnInitChat(Self, 'system', MensajeInicial, JMemory);

    If Trim(MensajeInicial) <> '' then
      Result := MensajeInicial;

    If Length(Trim(JMemory.Format)) > 10 then
      Result := Result + sLineBreak + 'Para Recordar= ' + JMemory.Format;
  Finally
    JMemory.Free;
  End;
end;

{ function TAiOpenChat.Run(AskMsg, ResMsg: TAiChatMessage): String;
  Begin
  end;
}

procedure TAiOpenChat.SetParallel_ToolCalls(const Value: Boolean);
begin
  FParallel_ToolCalls := Value;
end;

procedure TAiOpenChat.SetService_Tier(const Value: String);
begin
  FService_Tier := Value;
end;

procedure TAiOpenChat.SetStore(const Value: Boolean);
begin
  FStore := Value;
end;

function TAiOpenChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: integer): String;
var
  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
  LResponseJson: TJSonObject;
  LTempStream: TMemoryStream;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo con contenido para subirlo.');

  // El endpoint para subir archivos es /v1/files
  sUrl := Url + 'files';

  Client := THTTPClient.Create;
  Body := TMultipartFormData.Create;

  try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

    // Crear un stream temporal para pasarlo al formulario multipart
    LTempStream := TMemoryStream.Create;
    aMediaFile.Content.Position := 0;
    LTempStream.LoadFromStream(aMediaFile.Content);
    LTempStream.Position := 0;

    Body.AddStream('file', LTempStream, aMediaFile.Filename, aMediaFile.MimeType);
    // El propósito para chat/completions debe ser 'vision' o similar
    Body.AddField('purpose', 'vision');

    Res := Client.Post(sUrl, Body, nil, Headers);

    if Res.StatusCode = 200 then
    begin
      LResponseJson := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      try
        if LResponseJson.TryGetValue<string>('id', Result) then
        begin
          // Guardamos el ID en el objeto MediaFile para usarlo después
          aMediaFile.IdFile := Result;
          aMediaFile.CloudState := LResponseJson.GetValue<string>('status');
          aMediaFile.CloudName := LResponseJson.GetValue<string>('filename');
          // 'CacheName' puede ser el mismo IdFile para marcarlo como activo
          aMediaFile.CacheName := Result;
        end;
      finally
        FreeAndNil(LResponseJson);
      end;
    end
    else
    begin
      Raise Exception.CreateFmt('Error subiendo archivo a OpenAI: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  finally
    Body.Free;
    Client.Free;
    // LTempStream es propiedad de Body
  end;
end;

{ procedure TAiOpenChat.SetVoice(const Value: String);
  begin
  FVoice := Value;
  end;

  procedure TAiOpenChat.Setvoice_format(const Value: String);
  begin
  Fvoice_format := Value;
  end;
}

function TAiOpenChat.GetMessages: TJSonArray;
begin
  Result := FMessages.ToJSon;
end;

{ TAiOpenAiEmbeddings }

constructor TAiOpenAiEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := '@OPENAI_API_KEY';
  Url := GlOpenAIUrl;
  FDimensions := 1536;
  FModel := 'text-embedding-3-small';
end;

function TAiOpenAiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: integer; aModel, aEncodingFormat: String)
  : TAiEmbeddingData;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSonObject.Create;

  If aModel = '' then
    aModel := FModel;

  if aDimensions <= 0 then
    aDimensions := FDimensions;

  Try
    jObj.AddPair('input', aInput); // Este se adiciona por compatibilidad con ollama
    jObj.AddPair('prompt', aInput);
    jObj.AddPair('model', aModel);
    jObj.AddPair('user', aUser);
    jObj.AddPair('dimensions', aDimensions);
    jObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(UTF8Encode(jObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
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

destructor TAiOpenAiEmbeddings.Destroy;
begin

  inherited;
end;

procedure TAiOpenAiEmbeddings.ParseEmbedding(jObj: TJSonObject);
Var
  JArr, jData: TJSonArray;
  Emb: TAiEmbeddingData;
  JVal: TJSonValue;
  j: integer;
  Usage: TJSonObject;

begin
  jObj.TryGetValue<String>('model', FModel);

  If jObj.TryGetValue<TJSonObject>('usage', Usage) then
  Begin
    Usage.TryGetValue<integer>('prompt_tokens', Fprompt_tokens);
    Usage.TryGetValue<integer>('total_tokens', Ftotal_tokens);
  End;

  jData := jObj.GetValue<TJSonArray>('data');

  SetLength(FData, jData.Count);

  // var i := 0;
  For JVal in jData do
  Begin
    // El embedding de OpenAi Retorna un array, pero solo se toma el primero de la fila
    JArr := TJSonObject(JVal).GetValue<TJSonArray>('embedding');
    j := JArr.Count;
    SetLength(Emb, j);
    // FillChar(Emb, Length(Emb) * SizeOf(Double), 0);

    For j := 0 to JArr.Count - 1 do
      Emb[j] := JArr.Items[j].GetValue<Double>;

    FData := Emb;
    // Inc(i);
    Break; // Si el embedding de OpenAI retorna varios solo tomamos el primero, usualmente solo hay uno
  End;
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiOpenChat);

end.
