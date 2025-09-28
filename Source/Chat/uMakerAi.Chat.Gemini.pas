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

// --------- CAMBIOS --------------------
// [FECHA ACTUAL] - Se incorpora la generación de video con el modelo Veo.
// - Se añade la propiedad VideoParams.
// - Se implementa InternalRunImageVideoGeneration con el flujo asíncrono de Veo.

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
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.ToolFunctions, uMakerAi.Core,
  uMakerAi.Utils.PcmToWav, uMakerAi.Utils.CodeExtractor, uMakerAi.Embeddings,
  uMakerAi.Embeddings.Core;

type

  TAiMediaFileGemini = Class(TAiMediaFile)
  Private
  Protected
    Procedure DownloadFileFromUrl(Url: String); Override;

  Public
  End;

  TAiGeminiChat = Class(TAiChat)
  Private
    FVideoParams: TStrings;
    Function GetToolJSon: TJSonArray;
    procedure SetVideoParams(const Value: TStrings);
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

    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    function BuildSpeechConfigJson: TJSonObject;

  Public
    Function GetMessages: TJSonArray; Override;
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;

    Function UploadFile(aMediaFile: TAiMediaFile): String; Override;
    Function DownLoadFile(aMediaFile: TAiMediaFile): String; Override;
    Function DeleteFile(aMediaFile: TAiMediaFile): String; Override;
    Function CheckFileState(aMediaFile: TAiMediaFile): String; Override;
    Function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer = 3600): String; Override;

    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
    // Parámetros específicos para la generación de video con VEO
    Property VideoParams: TStrings read FVideoParams Write SetVideoParams;
  End;

  TAiGeminiEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;
    Procedure ParseEmbedding(jObj: TJSonObject); Override;
  end;

procedure Register;

implementation

Const
  GlAIUrl = 'https://generativelanguage.googleapis.com/v1beta/';
  // Endpoints VEO
  GlAIUploadUrl = 'https://generativelanguage.googleapis.com/upload/v1beta/';
  // GlAIVeoModel = 'veo-2.0-generate-001';
  // GlAIVeoModel = 'veo-3.0-generate-preview';

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
  Params.Add('Model=gemini-1.5-flash-latest');
  Params.Add('MaxTokens=8192');
  Params.Add('URL=https://generativelanguage.googleapis.com/v1beta/');
End;

procedure TAiGeminiChat.SetVideoParams(const Value: TStrings);
begin
  FVideoParams.Assign(Value);
end;

class function TAiGeminiChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiGeminiChat.Create(Sender);
End;

function TAiGeminiChat.BuildSpeechConfigJson: TJSonObject;
var
  LSpeechConfig, LMultiSpeakerObj, LVoiceConfig, LPrebuiltVoice, LSpeakerConfigObj: TJSonObject;
  LVoiceConfigsArray: TJSonArray;
  Parser: TStringList;
  I: Integer;
  LSpeakerName, LVoiceName: string;
begin
  Result := nil;

  if Voice.IsEmpty then
    Exit; // Si no hay configuración, no hacemos nada.

  // Si la cadena FVoice NO contiene un '=', asumimos que es una voz única simple.
  if Voice.IndexOf('=') = -1 then
  begin
    // --- CASO 1: Single-Speaker (ej: FVoice = 'Kore') ---
    LSpeechConfig := TJSonObject.Create;
    LPrebuiltVoice := TJSonObject.Create.AddPair('voiceName', Voice);
    LVoiceConfig := TJSonObject.Create.AddPair('prebuiltVoiceConfig', LPrebuiltVoice);
    LSpeechConfig.AddPair('voiceConfig', LVoiceConfig);
    Result := LSpeechConfig;
    Exit;
  end;

  // --- CASO 2: Multi-Speaker ---
  // Si contiene '=', intentamos parsearlo como una lista de hablantes.
  Parser := TStringList.Create;
  try
    Parser.NameValueSeparator := '=';

    // Reemplazamos las comas por saltos de línea para que DelimitedText funcione.
    // También manejamos espacios alrededor de las comas.
    Parser.Text := StringReplace(Voice, ',', sLineBreak, [rfReplaceAll]);

    // Si después del reemplazo no hay pares, no es multi-voz (podría ser un caso borde)
    if Parser.Count = 0 then
      Exit;

    // Ahora procedemos a construir el JSON multi-voz.
    LSpeechConfig := TJSonObject.Create;
    LMultiSpeakerObj := TJSonObject.Create;
    LVoiceConfigsArray := TJSonArray.Create;

    for I := 0 to Parser.Count - 1 do
    begin
      LSpeakerName := Trim(Parser.Names[I]);
      LVoiceName := Trim(Parser.ValueFromIndex[I]);

      // Ignorar entradas inválidas
      if LSpeakerName.IsEmpty or LVoiceName.IsEmpty then
        Continue;

      // Crear el objeto para un hablante
      LSpeakerConfigObj := TJSonObject.Create;
      LSpeakerConfigObj.AddPair('speaker', LSpeakerName);

      // Crear el objeto interno voiceConfig
      LPrebuiltVoice := TJSonObject.Create.AddPair('voiceName', LVoiceName);
      LVoiceConfig := TJSonObject.Create.AddPair('prebuiltVoiceConfig', LPrebuiltVoice);
      LSpeakerConfigObj.AddPair('voiceConfig', LVoiceConfig);

      LVoiceConfigsArray.AddElement(LSpeakerConfigObj);
    end;

    // Si hemos añadido al menos un hablante válido...
    if LVoiceConfigsArray.Count > 0 then
    begin
      LMultiSpeakerObj.AddPair('speakerVoiceConfigs', LVoiceConfigsArray);
      LSpeechConfig.AddPair('multiSpeakerVoiceConfig', LMultiSpeakerObj);
      Result := LSpeechConfig;
    end
    else
    begin
      // Liberar si no se generó nada válido
      LVoiceConfigsArray.Free;
      LMultiSpeakerObj.Free;
      LSpeechConfig.Free;
    end;

  finally
    Parser.Free;
  end;
end;

function TAiGeminiChat.CheckFileState(aMediaFile: TAiMediaFile): String;
var
  LHttpClient: TNetHTTPClient;
  LCheckUrl, CloudState: string;
  LResponse: IHTTPResponse;
  LResponseObj, LFileObj: TJSonObject;
begin
  CloudState := 'UNDEFINED';

  if aMediaFile.CloudName = '' then
    raise Exception.Create('El archivo no tiene un nombre de recurso en la nube (CloudName). Primero debe ser subido con UploadFile.');

  LHttpClient := TNetHTTPClient.Create(Nil);

{$IF CompilerVersion >= 35}
  LHttpClient.SynchronizeEvents := False;
{$ENDIF}
  try
    LCheckUrl := 'https://generativelanguage.googleapis.com/v1beta/' + aMediaFile.CloudName + '?key=' + Self.ApiKey;
    LResponse := LHttpClient.Get(LCheckUrl);

    if LResponse.StatusCode = 200 then
    begin
      LResponseObj := TJSonObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
      if Assigned(LResponseObj) then
        try
          if LResponseObj.TryGetValue<TJSonObject>('file', LFileObj) then
          begin
            LFileObj.TryGetValue<string>('state', CloudState);
          end;
        finally
          LResponseObj.Free;
        end;
    end
    else
    begin
      CloudState := 'FAILED';
    end;
  finally
    LHttpClient.Free;
  end;

  aMediaFile.CloudState := CloudState;
  Result := CloudState;
end;

constructor TAiGeminiChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@GEMINI_API_KEY';
  Model := 'gemini-1.5-flash-latest';
  Url := GlAIUrl;
  Top_p := 1;
  // Habilitar la generación de video por defecto
  NativeOutputFiles := []; // NativeOutputFiles + [Tfc_Video];

  // Inicializar los parámetros de video con valores por defecto
  FVideoParams := TStringList.Create;
  FVideoParams.AddPair('aspectRatio', '16:9');
  FVideoParams.AddPair('personGeneration', 'allow_adult');
  // # "dont_allow" or "allow_adult" or allow_all
  // FVideoParams.AddPair('personGeneration', 'allow_all'); // # "dont_allow" or "allow_adult" or allow_all
  // FVideoParams.AddPair('numberOfVideos', '1');
  // FVideoParams.AddPair('durationSeconds', '5'); // Rango válido 5-8
  // FVideoParams.AddPair('enhance_prompt', 'true');

end;

function TAiGeminiChat.DeleteFile(aMediaFile: TAiMediaFile): String;
var
  LHttpClient: TNetHTTPClient;
  LDeleteUrl: string;
  LResponse: IHTTPResponse;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.CloudName = '') then
    raise Exception.Create('No se puede eliminar un archivo sin un nombre de recurso en la nube (CloudName).');

  LHttpClient := TNetHTTPClient.Create(Nil);

{$IF CompilerVersion >= 35}
  LHttpClient.SynchronizeEvents := False;
{$ENDIF}
  try
    LDeleteUrl := 'https://generativelanguage.googleapis.com/v1beta/' + aMediaFile.CloudName + '?key=' + Self.ApiKey;

    LResponse := LHttpClient.Delete(LDeleteUrl);

    if LResponse.StatusCode <> 200 then
    begin
      raise Exception.CreateFmt('Error al eliminar el archivo: %d %s'#13#10'%s', [LResponse.StatusCode, LResponse.StatusText, LResponse.ContentAsString]);
    end;
    Result := 'DELETED';
  finally
    LHttpClient.Free;
  end;
end;

destructor TAiGeminiChat.Destroy;
begin
  FVideoParams.Free;
  inherited;
end;

function TAiGeminiChat.DownLoadFile(aMediaFile: TAiMediaFile): String;
var
  LHttpClient: TNetHTTPClient;
  LDownloadUrl: string;
  LResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
  LFileStream: TMemoryStream;
begin
  Result := 'FAILED'; // Estado por defecto en caso de fallo

  // Validación: necesitamos el CloudUri que contiene la URL de descarga.
  if not Assigned(aMediaFile) or (aMediaFile.UrlMedia = '') then
    raise Exception.Create('No se puede descargar el archivo. La propiedad CloudUri está vacía.');

  LHttpClient := TNetHTTPClient.Create(Nil);

{$IF CompilerVersion >= 35}
  LHttpClient.SynchronizeEvents := False;
{$ENDIF}
  LFileStream := TMemoryStream.Create;
  try
    LDownloadUrl := aMediaFile.UrlMedia;

    // La autenticación para la descarga se hace con un header x-goog-api-key
    LHeaders := [TNetHeader.Create('x-goog-api-key', Self.ApiKey)];

    // Realizamos la petición GET, la respuesta binaria irá al LFileStream
    LResponse := LHttpClient.Get(LDownloadUrl, LFileStream, LHeaders);

    // Comprobamos si la descarga fue exitosa
    if LResponse.StatusCode = 200 then
    begin
      // Limpiamos el contenido anterior del TAiMediaFile y cargamos el nuevo
      // aMediaFile.Content.Clear;
      LFileStream.Position := 0;
      aMediaFile.Content.LoadFromStream(LFileStream);
      aMediaFile.Content.Position := 0;

      // Si el nombre del archivo no está definido, lo asignamos.
      if aMediaFile.filename = '' then
        aMediaFile.filename := ExtractFileName(TURI.Create(LDownloadUrl).Path);

      // Podríamos también intentar obtener el MimeType del header de respuesta
      // if LResponse.HeaderValue['Content-Type'] <> '' then
      // (Opcional) La propiedad MimeType de TAiMediaFile se actualiza automáticamente
      // si filename tiene extensión, pero esto sería más preciso.
      // aMediaFile.MimeType := LResponse.HeaderValue['Content-Type'];

      Result := 'DOWNLOADED'; // Éxito
    end
    else
    begin
      // Si falla, lanzamos una excepción con el detalle del error
      raise Exception.CreateFmt('Error al descargar el archivo: %d %s'#13#10'%s', [LResponse.StatusCode, LResponse.StatusText, LResponse.ContentAsString(TEncoding.UTF8)]);
    end;

  finally
    LHttpClient.Free;
    LFileStream.Free;
  end;
end;

function TAiGeminiChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
var
  jCandidateValue, jPartValue: TJSONValue;
  jContent, jFunctionCall, LArgsObject: TJSonObject;
  jParts: TJSonArray;
  LFunction: TAiToolsFunction;
begin
  Result := nil;
  if not Assigned(jChoices) or (jChoices.Count = 0) then
    Exit;

  jCandidateValue := jChoices.Items[0];
  if not(jCandidateValue is TJSonObject) then
    Exit;

  Result := TAiToolsFunctions.Create;

  if not TJSonObject(jCandidateValue).TryGetValue<TJSonObject>('content', jContent) or not jContent.TryGetValue<TJSonArray>('parts', jParts) then
  begin
    FreeAndNil(Result);
    Exit;
  end;

  for jPartValue in jParts do
  begin
    if (jPartValue is TJSonObject) and TJSonObject(jPartValue).TryGetValue<TJSonObject>('functionCall', jFunctionCall) then
    begin
      LFunction := TAiToolsFunction.Create;
      try
        jFunctionCall.TryGetValue<string>('name', LFunction.Name);
        if jFunctionCall.TryGetValue<TJSonObject>('args', LArgsObject) then
          LFunction.Arguments := LArgsObject.ToJSON
        else
          LFunction.Arguments := '{}';

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

function TAiGeminiChat.GetMessages: TJSonArray;
Var
  I: Integer;
  Msg: TAiChatMessage;
  jObj, jPartItem, jInlineData, jFuncResponse, jResponseContent: TJSonObject;
  jParts: TJSonArray;
  MediaFile: TAiMediaFile;
  MediaArr: TAiMediaFilesArray;
  jFunctionCallObj: TJSONValue;
  ResArr: TJSonArray;
begin
  ResArr := TJSonArray.Create;
  Try

    For I := 0 to Messages.Count - 1 do
    Begin
      Msg := Messages.Items[I];
      jObj := TJSonObject.Create;

      // --- MANEJO DE ROLES ---
      if AnsiLowerCase(Msg.Role) = 'system' then
      begin
        // Simular 'system' con un par de mensajes 'user' y 'model'
        var
        jUserObj := TJSonObject.Create;
        jUserObj.AddPair('role', 'user').AddPair('parts', TJSonArray.Create(TJSonObject.Create.AddPair('text', Msg.Prompt)));
        ResArr.Add(jUserObj);

        var
        jModelObj := TJSonObject.Create;
        jModelObj.AddPair('role', 'model').AddPair('parts', TJSonArray.Create(TJSonObject.Create.AddPair('text', 'De acuerdo, seguiré las instrucciones.')));
        ResArr.Add(jModelObj);

        jObj.Free;
        Continue;
      end
      else if AnsiLowerCase(Msg.Role) = 'assistant' then
        jObj.AddPair('role', 'model')
      else if AnsiLowerCase(Msg.Role) = 'tool' then
        jObj.AddPair('role', 'function')
      else
        jObj.AddPair('role', Msg.Role); // 'user', 'model'

      jParts := TJSonArray.Create;
      jObj.AddPair('parts', jParts);

      // --- CONSTRUCCIÓN DE LAS "PARTS" ---
      if Msg.Role = 'tool' then
      begin
        // A. Es la RESPUESTA de una función que ejecutamos localmente.
        jFuncResponse := TJSonObject.Create;
        jFuncResponse.AddPair('name', Msg.FunctionName);
        jResponseContent := TJSonObject.Create;
        // El prompt del mensaje 'tool' contiene la respuesta de la función
        jResponseContent.AddPair('content', Msg.Prompt);
        jFuncResponse.AddPair('response', jResponseContent);
        jPartItem := TJSonObject.Create.AddPair('functionResponse', jFuncResponse);
        jParts.Add(jPartItem);
      end
      else if (Msg.Role = 'model') and (not Msg.Tool_calls.IsEmpty) then
      begin
        // B. Es la PETICIÓN del modelo para que llamemos a una función.
        // Reconstruimos la estructura a partir del JSON guardado en Tool_calls.
        jFunctionCallObj := TJSonObject.ParseJSONValue(Msg.Tool_calls);
        if Assigned(jFunctionCallObj) and (jFunctionCallObj is TJSonObject) then
        begin
          jPartItem := TJSonObject.Create.AddPair('functionCall', jFunctionCallObj.Clone as TJSonObject);
          jParts.Add(jPartItem);
        end
        else if Assigned(jFunctionCallObj) then
          jFunctionCallObj.Free;
      end
      else
      begin
        // C. Es un mensaje de texto normal ('user' o 'model'), posiblemente con imágenes.
        if not Msg.Prompt.IsEmpty then
        begin
          jPartItem := TJSonObject.Create.AddPair('text', Msg.Prompt);
          jParts.Add(jPartItem);
        end;

        // Adjuntar imágenes si existen
        MediaArr := Msg.MediaFiles.GetMediaList(NativeInputFiles, False);
        if Length(MediaArr) > 0 then
        begin
          for MediaFile in MediaArr do
          begin
            jPartItem := TJSonObject.Create;
            jInlineData := TJSonObject.Create;
            jInlineData.AddPair('mime_type', MediaFile.MimeType);
            jInlineData.AddPair('data', MediaFile.Base64);
            jPartItem.AddPair('inline_data', jInlineData);
            jParts.Add(jPartItem);
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

class function TAiGeminiChat.GetModels(aApiKey, aUrl: String): TStringList;
Var
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  sUrl, EndPointUrl: String;
  jRes: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSONValue;
  sModel: string;
  CustomModels: TArray<string>;
  I: Integer;
begin
  Result := TStringList.Create;

  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlAIUrl;

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
  Try
    sUrl := TPath.Combine(EndPointUrl, 'models') + '?key=' + aApiKey;
    Client.ContentType := 'application/json';
    Res := Client.Get(sUrl);

    if Res.StatusCode = 200 then
    Begin
      jRes := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      if Assigned(jRes) then
        try
          if jRes.TryGetValue<TJSonArray>('models', JArr) then
          Begin
            For JVal in JArr do
            Begin
              if (JVal is TJSonObject) and TJSonObject(JVal).TryGetValue<string>('name', sModel) then
              begin
                if sModel <> '' then
                Begin
                  sModel := StringReplace(sModel, 'models/', '', []);
                  Result.Add(sModel);
                End;
              end;
            End;
          End;
        finally
          jRes.Free;
        end;

      // Agregar modelos personalizados
      CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);

      { for I := Low(CustomModels) to High(CustomModels) do
        begin
        if not Result.Contains(CustomModels[I]) then
        Result.Add(CustomModels[I]);
        end;
      }

      for I := Low(CustomModels) to High(CustomModels) do
      begin
{$IF CompilerVersion <= 35.0}
        if Result.IndexOf(CustomModels[I]) = -1 then
          Result.Add(CustomModels[I]);
{$ELSE}
        if not Result.Contains(CustomModels[I]) then
          Result.Add(CustomModels[I]);
{$ENDIF}
      end;

    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

function TAiGeminiChat.GetToolJSon: TJSonArray;
var
  LJsonFunctions: String;
  LOpenAITools: TJSonArray;
begin

  Result := Nil;

  LJsonFunctions := Trim(inherited GetTools(tfGemini).Text);

  if (LJsonFunctions = '') or (not Tool_Active) then
    Exit;

  LOpenAITools := TJSonObject.ParseJSONValue(LJsonFunctions) as TJSonArray;

  if not Assigned(LOpenAITools) then
    Exit;

  Result := LOpenAITools;

  {


    Result := nil;
    LJsonFunctions := inherited GetTools(tfOpenAi).Text;

    if (LJsonFunctions = '') or (not Tool_Active) then
    Exit;

    LOpenAITools := TJSonObject.ParseJSONValue(LJsonFunctions) as TJSonArray;
    if not Assigned(LOpenAITools) then
    Exit;

    try
    LToolsArray := TJSonArray.Create;
    LToolObject := TJSonObject.Create;
    LFuncDeclarations := TJSonArray.Create;

    LToolObject.AddPair('functionDeclarations', LFuncDeclarations);
    LToolsArray.Add(LToolObject);

    for var LOpenAIFuncValue in LOpenAITools do
    begin
    if (LOpenAIFuncValue is TJSonObject) and ((LOpenAIFuncValue as TJSonObject).TryGetValue<TJSonObject>('function', LFuncObject)) then
    begin
    LFuncDeclarations.Add(LFuncObject.Clone as TJSonObject);
    end;
    end;

    if LFuncDeclarations.Count > 0 then
    Result := LToolsArray
    else
    LToolsArray.Free;

    finally
    LOpenAITools.Free;
    end;
  }
end;

function TAiGeminiChat.InitChatCompletions: String;
var
  LRequest, JConfig: TJSonObject;
  JTools, JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  ActiveCacheName: string;
begin
  LRequest := TJSonObject.Create;
  Lista := TStringList.Create;
  ActiveCacheName := '';
  try
    for I := FMessages.Count - 1 downto 0 do
    begin
      var
      Msg := FMessages[I];
      if Assigned(Msg.MediaFiles) and (Msg.MediaFiles.Count > 0) then
      begin
        for var MediaFile in Msg.MediaFiles do
        begin
          if not MediaFile.CacheName.IsEmpty then
          begin
            ActiveCacheName := MediaFile.CacheName;
            Break;
          end;
        end;
      end;
      if not ActiveCacheName.IsEmpty then
        Break;
    end;

    LRequest.AddPair('contents', GetMessages);

    if not ActiveCacheName.IsEmpty then
      LRequest.AddPair('cachedContent', TJSONString.Create(ActiveCacheName));

    JTools := Nil;

    if Tool_Active then
    begin
      JTools := GetToolJSon;
    end;

    if tcm_code_interpreter in ChatMediaSupports then
    Begin
      If not Assigned(JTools) then
        JTools := TJSonArray.Create;
      var
      LCodeTool := TJSonObject.Create;
      LCodeTool.AddPair('code_execution', TJSonObject.Create);
      // {"code_execution": {}}
      JTools.Add(LCodeTool);
    End;

    if tcm_WebSearch in ChatMediaSupports then
    Begin
      If not Assigned(JTools) then
        JTools := TJSonArray.Create;
      var
      LCodeTool := TJSonObject.Create;
      LCodeTool.AddPair('google_search', TJSonObject.Create);
      // {"code_execution": {}}
      JTools.Add(LCodeTool);
    End;

    if Assigned(JTools) then
      LRequest.AddPair('tools', JTools);

    JConfig := TJSonObject.Create;
    LRequest.AddPair('generationConfig', JConfig);

    // Si se ha configurado la salida de audio

    if (Tfc_Audio in Self.NativeOutputFiles) then
    begin
      // Llama a la nueva función para construir el JSON de speechConfig
      var
      LSpeechConfigObj := Self.BuildSpeechConfigJson;

      // Si se generó una configuración de voz (simple o múltiple)...
      if Assigned(LSpeechConfigObj) then
      begin
        // 1. Añadir el objeto speechConfig
        JConfig.AddPair('speechConfig', LSpeechConfigObj);

        // 2. Especificar que la modalidad de respuesta es AUDIO
        var
        JResponseModalities := TJSonArray.Create;
        JResponseModalities.Add('AUDIO');
        JConfig.AddPair('responseModalities', JResponseModalities);

        // Los modelos TTS pueden no usar estos parámetros, pero los dejamos por si acaso
        JConfig.AddPair('temperature', TJSONNumber.Create(Self.Temperature));
        if Self.Top_p <> 0 then
          JConfig.AddPair('topP', TJSONNumber.Create(Self.Top_p));
      end
    end
    else if (Tfc_Image in Self.NativeOutputFiles) then
    begin

      var
      JResponseModalities := TJSonArray.Create;
      JResponseModalities.Add('TEXT');
      JResponseModalities.Add('IMAGE');
      JConfig.AddPair('responseModalities', JResponseModalities);

      // 2b. Crear el objeto de configuración de imagen
      var
      JImageConfig := TJSonObject.Create;

      JImageConfig.AddPair('imageCount', 1);
      // JImageConfig.AddPair('quality', TJSONString.Create(Self.ImageQuality));
      // JImageConfig.AddPair('cfgScale', TJSONNumber.Create(Self.ImageCfgScale));

      // JConfig.AddPair('imageGenerationConfig', JImageConfig)

    end
    else
    begin

      JConfig.AddPair('temperature', TJSONNumber.Create(Self.Temperature));
      JConfig.AddPair('maxOutputTokens', TJSONNumber.Create(Self.Max_tokens));
      if Self.Top_p <> 0 then
        JConfig.AddPair('topP', TJSONNumber.Create(Self.Top_p));

      Lista.CommaText := Self.Stop;
      if Lista.Count > 0 then
      begin
        JStop := TJSonArray.Create;
        for I := 0 to Lista.Count - 1 do
          JStop.Add(Lista[I]);
        JConfig.AddPair('stopSequences', JStop);
      end;

    end;

    Result := LRequest.Format;

  finally
    LRequest.Free;
    Lista.Free;
  end;
end;

function TAiGeminiChat.InternalAddMessage(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage;
Var
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
begin

  Try
    // Adiciona el mensaje a la lista
    Msg := TAiChatMessage.Create(aPrompt, aRole);

    For MF in aMediaFiles do
      Msg.AddMediaFile(MF);

    Result := InternalAddMessage(Msg);

  Finally
  End;
end;

function TAiGeminiChat.InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
Var
  InitMsg: TAiChatMessage;
  MensajeInicial: String;
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
        InitMsg := TAiChatMessage.Create(MensajeInicial, 'user');
        InitMsg.Id := FMessages.Count + 1;
        FMessages.Add(InitMsg);

        InitMsg := TAiChatMessage.Create('De acuerdo, seguiré las instrucciones', 'model');
        InitMsg.Id := FMessages.Count + 1;
        FMessages.Add(InitMsg);

        if Assigned(FOnAddMessage) then
          FOnAddMessage(Self, InitMsg, Nil, 'system', MensajeInicial);
      end;
    End;

    // aMsg := TAiChatMessage.Create(aMsg.Prompt, aMsg.Role);
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
      // aMsg.AddMediaFile(MF);
    End;

    // FMessages.Add(Msg);
    FLastPrompt := aMsg.Prompt;

    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, aMsg, Nil, aMsg.Role, aMsg.Prompt);

    if Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, aMsg);

    Result := aMsg;

  Finally
  End;
end;

function TAiGeminiChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody, sUrl, LModel: String;
  Res: IHTTPResponse;
  St: TStringStream;
  jObj: TJSonObject;
begin
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'models/' + LModel + ':generateContent?key=' + ApiKey;

  try
    FClient.ContentType := 'application/json';
    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;

    FResponse.Clear;
    FResponse.Position := 0;

    St.Position := 0;
{$IFDEF APIDEBUG}
    St.SaveToFile('c:\temp\peticion.json.txt');
    St.Position := 0;
{$ENDIF}
    Res := FClient.Post(sUrl, St, FResponse);

    FResponse.Position := 0;
{$IFDEF APIDEBUG}
    FResponse.SaveToFile('c:\temp\Respuesta.json.txt');
    FResponse.Position := 0;
{$ENDIF}
    FLastContent := '';

    if FClient.Asynchronous = False then
    begin
      if Res.StatusCode = 200 then
      begin
        jObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
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
        DoError(Format('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]), nil);
        Result := '';
      end;
    end
    else
    begin
      // Lógica asíncrona si es necesaria
      Result := '';
    end;
  finally
    If FClient.Asynchronous = False then
      St.Free;
  end;
end;

function TAiGeminiChat.InternalRunImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
begin
  Raise Exception.Create('La Descripción de imagenes es nativa del gemini')
end;

function TAiGeminiChat.InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String;
begin
  Raise Exception.Create('La generación de imagenes es nativa del gemini')
end;

procedure AppendTextToFile(const AText: string; const AFileName: string = 'c:\temp\stream.txt');
var
  FileStream: TFileStream;
  TextBytes: TBytes;
  Directory: string;
begin
  // Extraer el directorio del archivo
  Directory := ExtractFilePath(AFileName);

  // Crear el directorio si no existe
  if not DirectoryExists(Directory) then
    ForceDirectories(Directory);

  try
    // Si el archivo existe, abrirlo en modo append (al final)
    // Si no existe, crearlo
    if FileExists(AFileName) then
      FileStream := TFileStream.Create(AFileName, fmOpenWrite or fmShareDenyWrite)
    else
      FileStream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);

    try
      // Posicionarse al final del archivo para append
      FileStream.Seek(0, soFromEnd);

      // Convertir el string a bytes (UTF-8)
      TextBytes := TEncoding.UTF8.GetBytes(AText + sLineBreak);

      // Escribir los bytes al archivo
      FileStream.WriteBuffer(TextBytes, Length(TextBytes));

    finally
      FileStream.Free;
    end;

  except
    on E: Exception do
      raise Exception.Create('Error al escribir al archivo: ' + E.Message);
  end;
end;

function TAiGeminiChat.InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  // LHttpClient: THTTPClient;
  LUrl, LOpName, PollingUrl, FinalResultText: string;
  LResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
  LRequest, LParams, LInstance: TJSonObject;
  LInstances: TJSonArray;
  LBodyStream: TStringStream;
  LInitialResponse: TJSonObject;
  I: Integer;
  LKey, LValueStr, LModel: string;
  VideoTask: ITask;
  MediaArr: TAiMediaFilesArray;

begin
  Result := '';
  FBusy := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // --- PASO 1: Petición inicial ---
  // LHttpClient := THTTPClient.Create;

  LRequest := TJSonObject.Create;
  try

    LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

    LUrl := Url + 'models/' + LModel + ':predictLongRunning';
    LInstances := TJSonArray.Create;
    LRequest.AddPair('instances', LInstances);
    LInstance := TJSonObject.Create;
    LInstances.Add(LInstance);
    LInstance.AddPair('prompt', AskMsg.Prompt);

    // Garantiza los permisos para cada modelo, en Europa se debe cambiar estas restricciones
    If LModel = 'veo-2.0-generate-001' then
      FVideoParams.Values['personGeneration'] := 'allow_adult'
      // # "dont_allow" or "allow_adult" or allow_all
    Else If LModel = 'veo-3.0-generate-preview' then
      FVideoParams.Values['personGeneration'] := 'allow_all'
      // # "dont_allow" or "allow_adult" or
    Else
      FVideoParams.Values['personGeneration'] := '';
    // # "dont_allow" or "allow_adult" or


    // FVideoParams.AddPair('personGeneration', 'allow_all'); // # "dont_allow" or "allow_adult" or allow_all

    // Revisa si hay imagenes anexas al mensaje de entrada para generar el video a partir de esta imagen
    MediaArr := AskMsg.MediaFiles.GetMediaList([Tfc_Image], False);

    if Length(MediaArr) > 0 then
    Begin

      // ---- primera forma de hacerlo al parecer no funcionó

      begin
        var
        LImagePart := TJSonObject.Create;
        LImagePart.AddPair('mimeType', MediaArr[0].MimeType);
        LImagePart.AddPair('bytesBase64Encoded', MediaArr[0].Base64);
        LInstance.AddPair('image', LImagePart);
      end;


      // Buscamos un archivo de imagen en el mensaje de petición
      // ---------- segunda forma de hacerlo al parecer esta si funciona

      {
        if Assigned(MediaArr[0]) then
        begin
        // Creamos el objeto 'image' con el campo 'bytes_value'
        LImageObject := TJSonObject.Create;
        // La API espera los bytes de la imagen codificados en Base64
        LImageObject.AddPair('bytes_value', MediaArr[0].Base64);
        LInstance.AddPair('image', LImageObject);
        end;
      }

      // No funciona ninguna de las dos alternativas ni otras probadas,  no hay más información en la documentación oficial.

    End;

    LParams := TJSonObject.Create;
    for I := 0 to FVideoParams.Count - 1 do
    begin
      LKey := FVideoParams.Names[I];
      LValueStr := FVideoParams.ValueFromIndex[I];
      var
        LNumInt: Integer;
      var
        LNumFloat: Extended;

        // 1. Primero, intentar convertir a Entero
      if TryStrToInt(LValueStr, LNumInt) then
      begin
        LParams.AddPair(LKey, TJSONNumber.Create(LNumInt));
      end
      // 2. Si no es entero, intentar convertir a Flotante
      else if TryStrToFloat(LValueStr, LNumFloat) then
      begin
        LParams.AddPair(LKey, TJSONNumber.Create(LNumFloat));
      end
      // 3. Si no es numérico, comprobar si es Booleano
      else if SameText(LValueStr, 'true') or SameText(LValueStr, 'false') then
      begin
        LParams.AddPair(LKey, TJSONBool.Create(SameText(LValueStr, 'true')));
      end
      // 4. Si no, tratarlo como un String
      else
      begin
        LParams.AddPair(LKey, TJSONString.Create(LValueStr));
      end;
    end;
    LRequest.AddPair('parameters', LParams);

    FClient.ContentType := 'application/json';
    LHeaders := [TNetHeader.Create('x-goog-api-key', Self.ApiKey)];
    LBodyStream := TStringStream.Create(LRequest.ToJSON, TEncoding.UTF8);

    LBodyStream.Position := 0;
{$IFDEF APIDEBUG}
    LBodyStream.SaveToFile('c:\temp\videopeticion.json.txt');
    LBodyStream.Position := 0;
{$ENDIF}
    try
      LResponse := FClient.Post(LUrl, LBodyStream, nil, LHeaders);
    finally
      LBodyStream.Free;
    end;

    if (LResponse.StatusCode <> 200) then
    begin
      FBusy := False;
      DoError(Format('Error al iniciar la generación de video: %d, %s', [LResponse.StatusCode, LResponse.ContentAsString]), nil);
      Exit;
    end;

    LInitialResponse := TJSonObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
    try
      if not LInitialResponse.TryGetValue<string>('name', LOpName) then
      begin
        FBusy := False;
        DoError('No se recibió el nombre de la operación (op_name) de la API.', nil);
        Exit;
      end;
    finally
      LInitialResponse.Free;
    end;

    PollingUrl := Url + LOpName;

    // --- PASO 2: Ejecutar polling en background con Wait ---
    VideoTask := TTask.Run(
      procedure
      var
        TaskClient: TNetHTTPClient;
        TaskResp: IHTTPResponse;
        TaskPollingResponse, TaskFinalResponse, TaskErrorObj: TJSonObject;
        TaskIsDone: Boolean;
        TaskFinalResultText: String;
      begin
        TaskClient := TNetHTTPClient.Create(Nil);

{$IF CompilerVersion >= 35}
        TaskClient.SynchronizeEvents := False;
{$ENDIF}
        TaskIsDone := False;
        TaskFinalResponse := nil;
        Try
          Try
            // Polling hasta completar
            while not TaskIsDone do
            begin
              Sleep(10000); // Esperar 10 segundos entre consultas

              // Notificar progreso en hilo principal
              TThread.Queue(nil,
                procedure
                begin
                  if Assigned(FOnReceiveDataEvent) then
                    FOnReceiveDataEvent(Self, ResMsg, nil, 'model', 'Procesando Video...');
                end);

              TaskResp := TaskClient.Get(PollingUrl, nil, LHeaders);
              if TaskResp.StatusCode <> 200 then
                raise Exception.CreateFmt('Error durante sondeo: %d, %s', [TaskResp.StatusCode, TaskResp.ContentAsString]);

              Var
              S := TaskResp.ContentAsString;

              AppendTextToFile(S);

              TaskPollingResponse := TJSonObject.ParseJSONValue(TaskResp.ContentAsString) as TJSonObject;
              try
                TaskPollingResponse.TryGetValue<Boolean>('done', TaskIsDone);
                if TaskIsDone then
                  TaskFinalResponse := TaskPollingResponse.Clone as TJSonObject;
              finally
                TaskPollingResponse.Free;
              end;
            end;

            // Procesar resultado
            if not Assigned(TaskFinalResponse) then
              raise Exception.Create('Operación finalizó sin respuesta.');
            if TaskFinalResponse.TryGetValue<TJSonObject>('error', TaskErrorObj) then
              raise Exception.Create('Operación falló: ' + TaskErrorObj.ToJSON);

            // --- INICIO DE SECCIÓN CORREGIDA ---

            if not Assigned(TaskFinalResponse) then
              raise Exception.Create('Operación finalizó sin respuesta.');
            if TaskFinalResponse.TryGetValue<TJSonObject>('error', TaskErrorObj) then
              raise Exception.Create('Operación falló: ' + TaskErrorObj.ToJSON);

            // 1. Navegar hasta el objeto "response"
            var
              LResponseObj: TJSonObject;
            var
              LVideoResponseObj: TJSonObject;
            var
              LSamplesArray: TJSonArray;

            if TaskFinalResponse.TryGetValue<TJSonObject>('response', LResponseObj) then
            begin
              // 2. Dentro de "response", navegar hasta "generateVideoResponse"
              if LResponseObj.TryGetValue<TJSonObject>('generateVideoResponse', LVideoResponseObj) then
              begin
                // 3. Dentro de "generateVideoResponse", navegar hasta el array "generatedSamples"
                if LVideoResponseObj.TryGetValue<TJSonArray>('generatedSamples', LSamplesArray) then
                begin
                  // 4. Iterar sobre cada objeto en el array "generatedSamples"
                  for var LSampleValue in LSamplesArray do
                  begin
                    var
                    LSampleObj := LSampleValue as TJSonObject;
                    // 5. Dentro de cada sample, navegar hasta "video"
                    var
                      LVideoObj: TJSonObject;
                    var
                      LVideoUri: String;
                    if LSampleObj.TryGetValue<TJSonObject>('video', LVideoObj) then
                    begin
                      // 6. Finalmente, obtener la "uri"
                      if LVideoObj.TryGetValue<string>('uri', LVideoUri) then
                      begin
                        // Acumulamos los URIs para el resultado final
                        TaskFinalResultText := Trim(TaskFinalResultText + sLineBreak + LVideoUri);

                        // Creamos el TAiMediaFile dentro del hilo principal para seguridad
                        var
                          NewVideoFile: TAiMediaFile;
                        NewVideoFile := TAiMediaFileGemini.Create;
                        NewVideoFile.UrlMedia := LVideoUri;
                        DownLoadFile(NewVideoFile);
                        NewVideoFile.filename := ExtractFileName(LVideoUri) + '.mp4';
                        ResMsg.AddMediaFile(NewVideoFile);
                      end;
                    end;
                  end;
                end;
              end;

              ResMsg.Prompt := FinalResultText;
            end;
          Except
            On E: Exception do
            Begin
              TThread.Queue(nil,
                procedure
                begin
                  FBusy := False;

                  DoError('Error en generación de video: ' + E.Message, E);
                  Exit;
                end);
            End;
          End;

        finally
          TaskClient.Free;
          if Assigned(TaskFinalResponse) then
            TaskFinalResponse.Free;
          // Obtener el resultado final
          FBusy := False;

        end;
      end);

    // --- AQUÍ LA MAGIA: Esperar a que termine ---
    VideoTask.Wait;

    // Obtener el resultado final
    Result := FinalResultText;
    FBusy := False;

    // Notificar finalización
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, ResMsg, nil, 'model', FinalResultText);

  finally
    LRequest.Free;
  end;
end;

function TAiGeminiChat.InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl, LModelName: string;
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LRequestJson, LGenConfigJson, LSpeechConfigObj: TJSonObject;
  LContentsArray, LPartsArray, JResponseModalities: TJSonArray;
  LPartObj, LMessageObj: TJSonObject;
  LResponse: IHTTPResponse;
  LResponseJson: TJSonObject;
  LResponseReader: TStreamReader;
  LBase64AudioData, LErrorResponse: string;
  LNewAudioFile: TAiMediaFile;
begin
  Result := '';
  // La función devuelve texto, pero para TTS la salida principal es el audio en ResMsg.
  FBusy := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // No es necesario añadir AskMsg al historial de chat principal (FMessages)
  // ya que esta es una llamada de un solo turno que no afecta la conversación.
  // Sin embargo, si quieres que se muestre en la UI, puedes añadirlo.
  // Por ahora, lo omitimos para mantener la lógica limpia.

  // 1. Preparar parámetros para la API de Gemini TTS
  // Usamos un modelo TTS específico. Puedes hacerlo configurable si lo deseas.
  LModelName := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  LUrl := Format('%smodels/%s:generateContent?key=%s', [Self.Url, LModelName, ApiKey]);

  // 2. Construir el cuerpo de la petición JSON
  LRequestJson := TJSonObject.Create;
  LBodyStream := nil;
  LResponseStream := TMemoryStream.Create;
  try
    // --- Construir el objeto "contents" ---
    // Solo el mensaje actual, como discutimos.
    LContentsArray := TJSonArray.Create;
    LMessageObj := TJSonObject.Create;
    LPartsArray := TJSonArray.Create;
    LPartObj := TJSonObject.Create;
    LPartObj.AddPair('text', TJSONString.Create(AskMsg.Prompt));
    LPartsArray.Add(LPartObj);
    LMessageObj.AddPair('role', TJSONString.Create('user'));
    LMessageObj.AddPair('parts', LPartsArray);
    LContentsArray.Add(LMessageObj);
    LRequestJson.AddPair('contents', LContentsArray);

    // --- Construir el objeto "generationConfig" ---
    LGenConfigJson := TJSonObject.Create;

    // Llama a la función que ya creamos para construir el speechConfig
    LSpeechConfigObj := Self.BuildSpeechConfigJson;
    if Assigned(LSpeechConfigObj) then
    begin
      LGenConfigJson.AddPair('speechConfig', LSpeechConfigObj);
    end
    else
    begin
      // Si no hay configuración de voz, no podemos continuar.
      // Lanzamos un error claro.
      FLastError := 'Error: Se intentó generar audio sin configurar una voz (Voice o VoiceConfiguration).';
      DoError(FLastError, nil);
      LRequestJson.Free;
      LResponseStream.Free;
      FBusy := False;
      Exit;
    end;

    // Añadir la modalidad de respuesta AUDIO
    JResponseModalities := TJSonArray.Create;
    JResponseModalities.Add('AUDIO');
    LGenConfigJson.AddPair('responseModalities', JResponseModalities);

    // Añadir el objeto generationConfig completo a la petición principal
    LRequestJson.AddPair('generationConfig', LGenConfigJson);

    // 3. Ejecutar la petición
    LBodyStream := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    LBodyStream.Position := 0;

    // Debug: Guardar la petición para verificar
{$IFDEF APIDEBUG}
    LBodyStream.SaveToFile('c:\temp\peticion.json.txt');
    LBodyStream.Position := 0;
{$ENDIF}
    FClient.ContentType := 'application/json';

    // Para Gemini, la ApiKey ya está en la URL, por lo que no se necesita el header 'Authorization'.
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, []);

    // 4. Procesar la respuesta
    LResponseStream.Position := 0;
    LResponseReader := TStreamReader.Create(LResponseStream, TEncoding.UTF8);
    try
      LErrorResponse := LResponseReader.ReadToEnd;
      // Leemos toda la respuesta como texto
    finally
      LResponseReader.Free;
    end;

    if LResponse.StatusCode = 200 then
    begin
      // La respuesta es un JSON, necesitamos parsearlo
      LResponseJson := TJSonObject.ParseJSONValue(LErrorResponse) as TJSonObject;
      if not Assigned(LResponseJson) then
      begin
        FLastError := 'Error: La respuesta del servidor no es un JSON válido.';
        DoError(FLastError, nil);
        FBusy := False;
        Exit;
      end;
      try
        // Extraer los datos de audio en Base64
        // Ruta: candidates[0].content.parts[0].inlineData.data
        LBase64AudioData := LResponseJson.GetValue<string>('candidates[0].content.parts[0].inlineData.data', '');

        if LBase64AudioData.IsEmpty then
        begin
          FLastError := 'Error: La respuesta del servidor no contiene datos de audio.';
          // Opcional: guardar la respuesta JSON completa para depuración
          FLastContent := LResponseJson.ToJSON;
          DoError(FLastError, nil);
        end
        else
        begin
          // Crear el TAiMediaFile y cargarlo desde los bytes decodificados
          LNewAudioFile := TAiMediaFile.Create;
          try
            // Gemini produce PCM a 24000 Hz, 16-bit, mono.
            // Para guardarlo como .wav se necesitarían cabeceras, pero para
            // reproducirlo en memoria, el formato raw puede ser suficiente.
            // Le damos una extensión 'pcm' para ser descriptivos.
            LNewAudioFile.LoadFromBase64('generated_audio.pcm', LBase64AudioData);

            Var
              WavStream: TMemoryStream;
            WavStream := Nil;

            if ConvertPCMStreamToWAVStream(LNewAudioFile.Content, WavStream, 24000, 1, 16) then
            Begin
              Try
                LNewAudioFile.Clear;
                WavStream.Position := 0;
                LNewAudioFile.LoadFromStream('generated_audio.wav', WavStream);
              Finally
                FreeAndNil(WavStream);
              End;
            End;

            ResMsg.MediaFiles.Add(LNewAudioFile);

            // Disparamos el evento de finalización
            if Assigned(FOnReceiveDataEnd) then
              FOnReceiveDataEnd(Self, ResMsg, nil, 'model', '');
          except
            LNewAudioFile.Free;
            raise;
          end;
        end;
      finally
        LResponseJson.Free;
      end;
    end
    else
    begin
      // Manejo de errores de la API
      FLastError := Format('Error generando audio [HTTP %d]: %s', [LResponse.StatusCode, LErrorResponse]);
      FLastContent := LErrorResponse; // Guardar la respuesta de error completa
      DoError(FLastError, nil);
    end;
  finally
    LRequestJson.Free;
    LBodyStream.Free;
    LResponseStream.Free;
    FBusy := False;
  end;
end;

function TAiGeminiChat.InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
begin
  Result := ''; // Implementar si es necesario
end;

// ------- Mensaje de tool no debe llamar el BeforeSendMessage ------
function TAiGeminiChat.InternalAddMessage(aPrompt, aRole, aToolCallId, aFunctionName: String): TAiChatMessage;
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
        Msg := TAiChatMessage.Create(MensajeInicial, 'user');
        Msg.Id := FMessages.Count + 1;
        FMessages.Add(Msg);

        Msg := TAiChatMessage.Create('De acuerdo, seguiré las instrucciones', 'model');
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
    // El mensaje del tool no debe llamar un before
    // if Assigned(FOnBeforeSendMessage) then  FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
end;

procedure TAiGeminiChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  S: String;
  jObj: TJSonObject;
  jContent, LUso: TJSonObject;
  jParts, jCandidates: TJSonArray;
  jValPart, jCandidate: TJSONValue;
  LRespuesta, sText: String;
  aPrompt_tokens, aCompletion_tokens, atotal_tokens: Integer;

begin
  // El modo sincronización realmente no funciona en Gemini, simplemente va enviando la respuesta por partes pero solo
  // se puede leer el mensaje al finalizar el json, de lo contrario siempre será un json incompleto

  If FClient.Asynchronous = False then
    Exit;

  AAbort := FAbort;

  If FAbort = True then
  Begin
    FBusy := False;
    If Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, Nil, Nil, 'system', 'abort');
  End;

  S := Trim(UTF8Encode(FResponse.DataString));
  LRespuesta := '';
  If S <> '' then
  Begin
    jObj := TJSonObject.ParseJSONValue(S) as TJSonObject;
    if Assigned(jObj) and jObj.TryGetValue<TJSonArray>('candidates', jCandidates) then
    Begin
      // Iterar sobre los candidatos
      for jCandidate in jCandidates do
      begin
        if (jCandidate is TJSonObject) and (jCandidate as TJSonObject).TryGetValue<TJSonObject>('content', jContent) and jContent.TryGetValue<TJSonArray>('parts', jParts) then
        begin
          // Iterar sobre las partes
          for jValPart in jParts do
            if (jValPart is TJSonObject) and (jValPart as TJSonObject).TryGetValue<string>('text', sText) then
              LRespuesta := Trim(LRespuesta + sText);
        end;
      end;

      aPrompt_tokens := 0;
      aCompletion_tokens := 0;
      atotal_tokens := 0;
      if jObj.TryGetValue<TJSonObject>('usageMetadata', LUso) then
      begin
        LUso.TryGetValue<Integer>('promptTokenCount', aPrompt_tokens);
        LUso.TryGetValue<Integer>('candidatesTokenCount', aCompletion_tokens);
        LUso.TryGetValue<Integer>('totalTokenCount', atotal_tokens);
      end;

      Self.FLastContent := LRespuesta;
      Self.Prompt_tokens := Self.Prompt_tokens + aPrompt_tokens;
      Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
      Self.Total_tokens := Self.Total_tokens + atotal_tokens;

      Var
      Msg := TAiChatMessage.Create(FLastContent, 'model');
      Msg.Prompt := FLastContent;
      // Msg.Tool_calls := sToolCalls;
      Msg.Prompt_tokens := aPrompt_tokens;
      Msg.Completion_tokens := aCompletion_tokens;
      Msg.Total_tokens := atotal_tokens;
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);
      FBusy := False;

      If Assigned(FOnReceiveDataEvent) then
        FOnReceiveDataEvent(Self, Nil, jObj, 'model', FLastContent);

      If Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, Msg, Nil, 'model', FLastContent);

    End;

    // No olvides liberar la memoria
    jObj.Free;
  End;
end;

{
  procedure TAiGeminiChat.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
  Var
  LCandidates: TJSonArray;
  LContent, LUso, LPart, LInlineData: TJSonObject; // <-- Añadido LPart, LInlineData
  LRespuesta, LRole, sText, sAudioBase64, LClave: String; // <-- Añadido sAudioBase64
  aPrompt_tokens, aCompletion_tokens, atotal_tokens: Integer;
  ToolMsg, AskMsg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  LToolCall: TAiToolsFunction;
  IsAudioResponse: Boolean; // <-- Flag para saber qué tipo de respuesta es
  begin
  LRespuesta := '';
  sAudioBase64 := '';
  IsAudioResponse := False;
  LRole := 'model';
  // ... (el resto de las inicializaciones) ...

  if not jObj.TryGetValue<TJSonArray>('candidates', LCandidates) or (LCandidates.Count = 0) then
  begin
  // ... (el manejo de errores está bien) ...
  end;

  // ... (la extracción de tool calls está bien) ...

  // --- SECCIÓN MODIFICADA PARA PARSEAR TEXTO O AUDIO ---
  var LCandidate := LCandidates.Items[0] as TJSonObject;
  if LCandidate.TryGetValue<TJSonObject>('content', LContent) then
  begin
  LContent.TryGetValue<string>('role', LRole);
  var LParts: TJSonArray;
  if LContent.TryGetValue<TJSonArray>('parts', LParts) then
  begin
  if LParts.Count > 0 then
  begin
  // Tomamos la primera "part" para analizarla
  LPart := LParts.Items[0] as TJSonObject;
  if LPart.TryGetValue<TJSonObject>('inlineData', LInlineData) then
  begiVar
  S: String;
  jObj: TJSonObject;
  jContent: TJSonObject;
  jParts, jCandidates: TJSonArray;
  jValPart, jCandidate: TJSonValue;
  LRespuesta, sText: String;
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
  S := Trim(UTF8Encode(FResponse.DataString));
  LRespuesta := '';
  If S <> '' then
  Begin
  jObj := TJSonObject.ParseJSONValue(S) as TJSonObject;
  if Assigned(jObj) and jObj.TryGetValue<TJSonArray>('candidates', jCandidates) then
  Begin
  // Iterar sobre los candidatos
  for jCandidate in jCandidates do
  begin
  if (jCandidate is TJSonObject) and
  (jCandidate as TJSonObject).TryGetValue<TJSonObject>('content', jContent) and
  jContent.TryGetValue<TJSonArray>('parts', jParts) then
  begin
  // Iterar sobre las partes
  for jValPart in jParts do
  if (jValPart is TJSonObject) and
  (jValPart as TJSonObject).TryGetValue<string>('text', sText) then
  LRespuesta := Trim(LRespuesta + sText);
  end;
  end;
  End;

  // No olvides liberar la memoria
  jObj.Free;
  End;
  end;n
  // ¡Es una respuesta de AUDIO!
  IsAudioResponse := True;
  LInlineData.TryGetValue<string>('data', sAudioBase64);
  end
  else if LPart.TryGetValue<string>('text', sText) then
  begin
  // Es una respuesta de TEXTO
  LRespuesta := sText;
  end;
  end;
  end;
  end;
  // --- FIN DE LA SECCIÓN MODIFICADA ---

  // ... (el parseo de usageMetadata está bien) ...

  if IsAudioResponse then
  begin
  // --- FLUJO DE RESPUESTA DE AUDIO ---
  var AudioFile := TAiMediaFile.Create;
  try
  // La respuesta es audio PCM, lo guardamos en el MediaFile
  AudioFile.LoadFromBase64('generated_audio.pcm', sAudioBase64);
  ResMsg.AddMediaFile(AudioFile);
  // El contenido de texto de la respuesta es vacío
  Self.FLastContent := '';
  ResMsg.Prompt := '';
  ResMsg.Content := '';
  except
  AudioFile.Free;
  raise;
  end;
  end
  else
  begin
  // --- FLUJO DE RESPUESTA DE TEXTO (código existente) ---
  LRespuesta := Trim(LRespuesta);
  DoProcessResponse(AskMsg, ResMsg, LRespuesta);

  Self.FLastContent := LRespuesta;
  ResMsg.Prompt := LRespuesta;
  ResMsg.Content := LRespuesta;
  end;

  // Asignar tokens, rol, etc. (esto es común para ambos flujos)
  ResMsg.Role := LRole;
  ResMsg.Prompt_tokens := aPrompt_tokens;
  // ... (resto del código de asignación de tokens) ...

  // Notificar al final
  FBusy := False;
  if Assigned(FOnAddMessage) then
  FOnAddMessage(Self, ResMsg, jObj, LRole, LRespuesta);
  if Assigned(FOnReceiveDataEnd) then
  FOnReceiveDataEnd(Self, ResMsg, jObj, LRole, LRespuesta); // El evento se dispara igual, pero aText estará vacío si es audio
  end;
}

procedure TAiGeminiChat.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
Var
  LCandidates: TJSonArray;
  LContent, LUso: TJSonObject;
  LRespuesta, LRole, sText: String;
  aPrompt_tokens, aCompletion_tokens, atotal_tokens: Integer;
  ToolMsg, AskMsg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;
  ModelVersion, ResponseId: String;

  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave: String;

  Code: TMarkdownCodeExtractor;
  CodeFile: TCodeFile;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;

begin
  LRespuesta := '';
  LRole := 'model';
  AskMsg := GetLastMessage;

  if not jObj.TryGetValue<TJSonArray>('candidates', LCandidates) or (LCandidates.Count = 0) then
  begin
    FBusy := False;
    DoError('La respuesta de Gemini no contiene candidatos válidos.', nil);
    Exit;
  end;

  jObj.TryGetValue<String>('modelVersion', ModelVersion);
  jObj.TryGetValue<String>('responseId', ResponseId);

  aPrompt_tokens := 0;
  aCompletion_tokens := 0;
  atotal_tokens := 0;
  if jObj.TryGetValue<TJSonObject>('usageMetadata', LUso) then
  begin
    LUso.TryGetValue<Integer>('promptTokenCount', aPrompt_tokens);
    LUso.TryGetValue<Integer>('candidatesTokenCount', aCompletion_tokens);
    LUso.TryGetValue<Integer>('totalTokenCount', atotal_tokens);
  end;

  // Si contiene una respuesta la toma desde acá
  var
  LCandidate := LCandidates.Items[0] as TJSonObject;
  if LCandidate.TryGetValue<TJSonObject>('content', LContent) then
  begin
    LContent.TryGetValue<string>('role', LRole);
    var
      LParts: TJSonArray;
    if LContent.TryGetValue<TJSonArray>('parts', LParts) then
    Begin
      {
        for var jValPart in LParts do
        Begin
        if (jValPart is TJSonObject) and (jValPart as TJSonObject).TryGetValue<string>('text', sText) then
        LRespuesta := Trim(LRespuesta + sText);
        End;
      }
      for var jValPart in LParts do
      begin
        if not(jValPart is TJSonObject) then
          Continue;

        var
        LPartObj := jValPart as TJSonObject;

        // --- Parte 1: Buscar y acumular texto ---
        if LPartObj.TryGetValue<string>('text', sText) then
        begin
          LRespuesta := Trim(LRespuesta + sText);
        end;

        // 2. Buscamos CÓDIGO EJECUTABLE
        var
          LExecCodeObj: TJSonObject;
        var
          LCode: String;
        if LPartObj.TryGetValue<TJSonObject>('executableCode', LExecCodeObj) then
        begin
          if LExecCodeObj.TryGetValue<string>('code', LCode) then
          begin
            // Formateamos el código usando Markdown para una mejor visualización
            LRespuesta := LRespuesta + sLineBreak + '```python' + sLineBreak + Trim(LCode) + sLineBreak + '```';
          end;
        end;

        // 3. Buscamos el RESULTADO de la ejecución del código
        var
          LCodeResultObj: TJSonObject;
        var
          LCodeOutput: String;
        if LPartObj.TryGetValue<TJSonObject>('codeExecutionResult', LCodeResultObj) then
        begin
          if LCodeResultObj.TryGetValue<string>('output', LCodeOutput) then
          begin
            // Añadimos el resultado de forma clara
            LRespuesta := LRespuesta + sLineBreak + '**Resultado de la Ejecución:**' + sLineBreak + Trim(LCodeOutput);
          end;
        end;

        Var
          LInlineData: TJSonObject;
          // --- Parte 2: Buscar y procesar datos de medios ---
        if LPartObj.TryGetValue<TJSonObject>('inlineData', LInlineData) then
        begin
          // Extraer mimeType y data
          Var
            LMimeType: String;
          Var
            LBase64Data: String;

          LInlineData.TryGetValue<string>('mimeType', LMimeType);
          LInlineData.TryGetValue<string>('data', LBase64Data);

          if not LBase64Data.IsEmpty then
          begin
            try
              // Decodificar Base64 a TBytes
              // Crear el TAiMediaFile
              Var
              LNewMediaFile := TAiMediaFile.Create;
              try
                var
                LFileExtension := LMimeType.Substring(LMimeType.LastIndexOf('/') + 1);
                var
                LFileName := 'generated_media.' + LFileExtension;

                LNewMediaFile.LoadFromBase64(LFileName, LBase64Data);
                // Añadir el archivo de medios al mensaje de respuesta
                ResMsg.MediaFiles.Add(LNewMediaFile);
              except
                LNewMediaFile.Free; // Liberar si algo falla durante la carga
                raise;
              end;
            except
              on E: Exception do
              begin
                // Si la decodificación falla, registrar el error pero no detener todo
                DoError('Error decodificando datos de medios en Base64: ' + E.Message, E);
              end;
            end;
          end;
        end;
      end;

    End;

  end;

  LRespuesta := Trim(LRespuesta);

  Self.FLastContent := LRespuesta;
  Self.Prompt_tokens := Self.Prompt_tokens + aPrompt_tokens;
  Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
  Self.Total_tokens := Self.Total_tokens + atotal_tokens;

  LFunciones := ExtractToolCallFromJson(LCandidates);

  If (Not Assigned(LFunciones)) or (LFunciones.Count <= 0) then
  // Si no hay funciones debe continuar la conversación
  Begin
    ResMsg.Role := LRole;
    ResMsg.Tool_calls := '';
    ResMsg.Prompt := ResMsg.Prompt + LRespuesta;
    ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aPrompt_tokens;
    ResMsg.Completion_tokens := ResMsg.Completion_tokens + aCompletion_tokens;
    ResMsg.Total_tokens := ResMsg.Total_tokens + atotal_tokens;
    DoProcessResponse(AskMsg, ResMsg, LRespuesta);
  End
  Else
  Begin
    Var
    Msg := TAiChatMessage.Create(LRespuesta, LRole);

    var
    FirstToolCall := LFunciones.ToArray[0].Value;
    var
    jFunctionCall := TJSonObject.Create;
    try
      jFunctionCall.AddPair('name', FirstToolCall.Name);
      var
      jArgs := TJSonObject.ParseJSONValue(FirstToolCall.Arguments) as TJSonObject;
      if not Assigned(jArgs) then
        jArgs := TJSonObject.Create;
      jFunctionCall.AddPair('args', jArgs);
      Msg.Tool_calls := jFunctionCall.ToJSON;
    finally
      jFunctionCall.Free;
    end;

    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);

    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, Msg, jObj, Msg.Role, '');
  End;

  Try
    If Assigned(LFunciones) and (LFunciones.Count > 0) then
    Begin

      NumTasks := LFunciones.Count;
      SetLength(TaskList, NumTasks);
      // Ajusta el tamaño del array para el número de tareas

      I := 0;
      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];
        ToolCall.ResMsg := ResMsg;
        // Se pasan los mensajes por si desean procesarlos
        ToolCall.AskMsg := AskMsg;

        TaskList[I] := TTask.Create(
          procedure
          begin
            Try
              DoCallFunction(ToolCall);
            Except
              On E: Exception do
              Begin

                TThread.Queue(nil,
                  procedure
                  begin
                    DoError('Error in "' + ToolCall.Name + '"', E);
                  end);

              End;
            End;
          end);
        TaskList[I].Start;
        Inc(I);

      End;
      TTask.WaitForAll(TaskList);

      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];
        ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
        ToolMsg.Id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
      End;

      Self.Run(Nil, ResMsg);
      ResMsg.Content := '';

    End
    Else
    Begin

      If tfc_textFile in NativeOutputFiles then
      Begin
        Code := TMarkdownCodeExtractor.Create;
        Try

          CodeFiles := Code.ExtractCodeFiles(LRespuesta);
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

      DoProcessResponse(AskMsg, ResMsg, LRespuesta);
      ResMsg.Prompt := LRespuesta;

      FBusy := False;
      If Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, ResMsg, jObj, LRole, LRespuesta);
    End;
  Finally
    LFunciones.Free;
  End;
end;

function TAiGeminiChat.UploadFile(aMediaFile: TAiMediaFile): String;
var
  LHttpClient: TNetHTTPClient;
  LStartUrl, LUploadUrl, LFileUri, CloudName, CloudState: string;
  LResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
  LJsonBody, LUploadResponseObj, LFileObj: TJSonObject;
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

      LJsonBody := TJSonObject.Create;
      try
        LFileObj := TJSonObject.Create;
        LFileObj.AddPair('display_name', TJSONString.Create(aMediaFile.filename));
        LJsonBody.AddPair('file', LFileObj);
        LHttpClient.ContentType := 'application/json';
        LResponse := LHttpClient.Post(LStartUrl, TStringStream.Create(LJsonBody.ToJSON, TEncoding.UTF8), nil, LHeaders);
      finally
        FreeAndNil(LJsonBody);
      end;

      if LResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('Error al iniciar subida de archivo: %d %s', [LResponse.StatusCode, LResponse.StatusText]);

      LUploadUrl := LResponse.HeaderValue['X-Goog-Upload-Url'];
      if LUploadUrl = '' then
        raise Exception.Create('No se recibió la URL de subida desde la API de Google.');

      LFileStream.Position := 0;
      LHeaders := [TNetHeader.Create('Content-Length', LNumBytes.ToString), TNetHeader.Create('X-Goog-Upload-Offset', '0'), TNetHeader.Create('X-Goog-Upload-Command', 'upload, finalize')];

      LHttpClient.ContentType := 'application/octet-stream';
      LResponse := LHttpClient.Post(LUploadUrl, LFileStream, nil, LHeaders);

      if LResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('Error al subir los bytes del archivo: %d %s'#13#10'%s', [LResponse.StatusCode, LResponse.StatusText, LResponse.ContentAsString]);

      LUploadResponseObj := TJSonObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
      if Assigned(LUploadResponseObj) then
        try
          if LUploadResponseObj.TryGetValue<TJSonObject>('file', LFileObj) then
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
      if Result = '' then
        raise Exception.Create('La API subió el archivo pero no devolvió una URI.');
    finally
      // El stream no se libera aquí porque es propiedad de TAiMediaFile
    end;
  finally
    LHttpClient.Free;
  end;
end;

function TAiGeminiChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer): String;
var
  LHttpClient: TNetHTTPClient;
  LUrl: string;
  LResponse: IHTTPResponse;
  LRequestBody, LJson, LPart, LInlineData: TJSonObject;
  LPartsArray, LContentsArray: TJSonArray;
  CacheName, LModel: string;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Base64 = '') then
    raise Exception.Create('Se necesita un archivo con contenido Base64 para crear una caché.');

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  LHttpClient := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  LHttpClient.SynchronizeEvents := False;
{$ENDIF}
  try
    LUrl := Url + 'cachedContents?key=' + Self.ApiKey;

    LRequestBody := TJSonObject.Create;
    try
      LRequestBody.AddPair('model', TJSONString.Create(LModel));
      LRequestBody.AddPair('ttl', TJSONString.Create(aTTL_Seconds.ToString + 's'));

      LContentsArray := TJSonArray.Create;
      LJson := TJSonObject.Create;
      LJson.AddPair('role', TJSONString.Create('user'));
      LPartsArray := TJSonArray.Create;

      LPart := TJSonObject.Create;
      LInlineData := TJSonObject.Create;
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
      LResponseObj := TJSonObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
      if Assigned(LResponseObj) then
        try
          if LResponseObj.TryGetValue<string>('name', CacheName) then
          begin
            aMediaFile.CacheName := CacheName;
            Result := CacheName;
            if aMediaFile.CacheName = '' then
              raise Exception.Create('La API no devolvió un nombre de caché (CacheName).');
          end;
        finally
          LResponseObj.Free;
        end;
    end
    else
    begin
      raise Exception.CreateFmt('Error al crear la caché de contenido: %d %s'#13#10'%s', [LResponse.StatusCode, LResponse.StatusText, LResponse.ContentAsString]);
    end;
  finally
    LHttpClient.Free;
  end;
end;

{ TAiMediaFileGemini }

procedure TAiMediaFileGemini.DownloadFileFromUrl(Url: String);
begin
  // inherited;  //Aquí no hace nada, el descarga el modelo despues

end;

{ TAiGeminiEmbeddings }

constructor TAiGeminiEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := '@GEMINI_API_KEY';
  Url := GlAIUrl;
  FDimensions := 1536;
  FModel := 'gemini-embedding-001';
end;

{
  TAiGeminiEmbeddings.CreateEmbedding

  Crea un vector de embedding para un texto de entrada utilizando la API de Google Gemini.

  Parámetros:
  aInput: El texto que se va a convertir en embedding.
  aUser: (Ignorado por la API de Gemini) Identificador de usuario final.
  aDimensions:  El número de dimensiones para el embedding.
  aModel: El modelo a utilizar (ej. 'models/embedding-001').
  aEncodingFormat: (Ignorado por la API de Gemini) El formato del embedding.

  Devuelve:
  Un registro TAiEmbeddingData que contiene el vector de embedding y otra información.

  Notas:
  - FUrl debe ser 'https://generativelanguage.googleapis.com'.
  - FApiKey debe contener tu clave de la API de Google AI.

}
function TAiGeminiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
var
  Client: THTTPClient;
  jRequestRoot, jContent, jPart: TJSonObject;
  jParts: TJSonArray;
  jResponseRoot: TJSonObject;
  Res: IHTTPResponse;
  St: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  jRequestRoot := TJSonObject.Create;
  try
    if aModel.IsEmpty then
      aModel := FModel;

    // https://generativelanguage.googleapis.com/v1beta/models/text-embedding-004:embedContent?key=TU_API_KEY
    sUrl := FUrl + 'models/' + aModel + ':embedContent?key=' + FApiKey;

    // La estructura base es: { "model": "...", "content": { "parts": [{"text": "..."}] } }

    // Crear el objeto 'part'
    jPart := TJSonObject.Create;
    jPart.AddPair('text', TJSONString.Create(aInput));

    // Crear el array 'parts' y añadir el objeto 'part'
    jParts := TJSonArray.Create;
    jParts.AddElement(jPart);

    // Crear el objeto 'content' y añadir el array 'parts'
    jContent := TJSonObject.Create;
    jContent.AddPair('parts', jParts);

    // Construir el objeto JSON raíz
    jRequestRoot.AddPair('model', TJSONString.Create(aModel));
    jRequestRoot.AddPair('content', jContent);

    if aDimensions > 0 then
      jRequestRoot.AddPair('output_dimensionality', TJSONNumber.Create(aDimensions));

    St := TStringStream.Create(jRequestRoot.ToString, TEncoding.UTF8);
    try
      Client.ContentType := 'application/json';
      Res := Client.Post(sUrl, St);
    finally
      St.Free;
    end;

{$IFDEF APIDEBUG}
    TFile.WriteAllText('c:\temp\gemini_response.txt', Res.ContentAsString);
{$ENDIF}
    if Res.StatusCode = 200 then
    begin
      // La respuesta: { "embedding": { "values": [...] } }
      jResponseRoot := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      try
        // Llama a tu método para analizar la respuesta de Gemini.
        ParseEmbedding(jResponseRoot);
        Result := Self.FData;
      finally
        jResponseRoot.Free;
      end;
    end
    else
      Raise Exception.CreateFmt('Error al llamar a la API de Gemini: %d - %s', [Res.StatusCode, Res.ContentAsString]);

  finally
    jRequestRoot.Free;
    Client.Free;
  end;
end;

destructor TAiGeminiEmbeddings.Destroy;
begin

  inherited;
end;

procedure TAiGeminiEmbeddings.ParseEmbedding(jObj: TJSonObject);
var
  LEmbeddingObj: TJSonObject;
  LValuesArray: TJSonArray;
  Emb: TAiEmbeddingData;
  I: Integer;
begin
  if not jObj.TryGetValue<TJSonObject>('embedding', LEmbeddingObj) then
    Exit;

  if not LEmbeddingObj.TryGetValue<TJSonArray>('values', LValuesArray) then
    Exit;

  SetLength(Emb, LValuesArray.Count);

  for I := 0 to LValuesArray.Count - 1 do
    Emb[I] := LValuesArray.Items[I].GetValue<Double>;

  FData := Emb;

end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiGeminiChat);

end.
