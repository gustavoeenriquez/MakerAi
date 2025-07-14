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


// OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO
// No funciona Call Funcions
// OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO

unit uMakerAi.Chat.Claude;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.ToolFunctions, uMakerAi.Core, uMakerAi.Utils.CodeExtractor;

type

  TClaudeStreamContentBlock = class
  public
    BlockType: string; // 'text', 'tool_use', etc.
    TextContent: TStringBuilder;
    JsonContent: TStringBuilder;
    ToolFunction: TAiToolsFunction;
    constructor Create;
    destructor Destroy; override;
  end;

  TAiClaudeChat = Class(TAiChat)
  Private
    FStreamResponseMsg: TAiChatMessage;
    FStreamContentBlocks: TDictionary<Integer, TClaudeStreamContentBlock>;
    FStreamBuffer: TStringBuilder;
    FStreamLastEventType: string;

    // Function GetToolJSon: TJSonArray;
    function GetFileHeaders: TNetHeaders;
    procedure ClearStreamState;
    procedure ProcessStreamChunk(const AChunk: string);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;

    Function InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage; Overload; Override;

    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Override;

    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;

    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    function ExtractToolCallJson(jChoices: TJSonArray): TJSonArray; // construye el llamado a las funciones en el mensaje
    function GenerateClaudeToolsJson: TJSonArray;

  Public
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Function GetMessages: TJSonArray; Override;
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    // Function Run(aMsg: TAiChatMessage = Nil): String; Override;

    // --- Implementación de Gestión de Archivos para Claude ---
    Function UploadFile(aMediaFile: TAiMediaFile): String; Override;
    Function DownLoadFile(aMediaFile: TAiMediaFile): String; Override;
    Function CheckFileState(aMediaFile: TAiMediaFile): String; Override;
    Function DeleteFile(aMediaFile: TAiMediaFile): String; Override;
    function RetrieveFile(aFileId: string): TAiMediaFile; Override;
    function RetrieveFileList: TAiMediaFiles; Override;
    Function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer = 3600): String; Override;

    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.anthropic.com/v1/';
  CLAUDE_API_VERSION = '2023-06-01';
  CLAUDE_FILES_BETA_HEADER = 'files-api-2025-04-14';
  CLAUDE_TOOLS_BETA_HEADER = 'tools-2024-05-16'; // Beta para Tools

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiClaudeChat]);
end;

{ TAiClaudChat }

procedure PopulateMediaFileFromJson(aMediaFile: TAiMediaFile; jObj: TJSonObject);
begin
  if not Assigned(aMediaFile) or not Assigned(jObj) then
    Exit;

  aMediaFile.IdFile := jObj.GetValue<string>('id', '');
  aMediaFile.FileName := jObj.GetValue<string>('filename', '');
  // No hay un nombre de "cloud" separado, usamos el nombre de archivo.
  aMediaFile.CloudName := aMediaFile.FileName;
end;

function TAiClaudeChat.GetFileHeaders: TNetHeaders;
begin
  Result := [TNetHeader.Create('x-api-key', ApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION),
    TNetHeader.Create('anthropic-beta', CLAUDE_FILES_BETA_HEADER)];
end;

class function TAiClaudeChat.GetDriverName: string;
Begin
  Result := 'Claude';
End;

class procedure TAiClaudeChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@CLAUDE_API_KEY');
  Params.Add('Model=claude-3-sonnet-20240229');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=https://api.anthropic.com/v1/');
End;

function TAiClaudeChat.RetrieveFile(aFileId: string): TAiMediaFile;
var
  Client: THTTPClient;
  Headers: TNetHeaders;
  ResponseStream: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: string;
  jObj: TJSonObject;
begin
  if aFileId.IsEmpty then
    raise Exception.Create('Se requiere un ID de archivo (aFileId) para recuperarlo.');

  sUrl := Url + 'files/' + aFileId;
  Client := THTTPClient.Create;
  ResponseStream := TMemoryStream.Create;
  try
    Headers := GetFileHeaders;
    Res := Client.Get(sUrl, ResponseStream, Headers);

    if Res.StatusCode = 200 then
    begin
      ResponseStream.Position := 0;
      jObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      try
        Result := TAiMediaFile.Create;
        PopulateMediaFileFromJson(Result, jObj);
      finally
        jObj.Free;
      end;
    end
    else
    begin
      ResponseStream.Position := 0;
      raise Exception.CreateFmt('Error al recuperar el archivo "%s" de Claude: %d - %s',
        [aFileId, Res.StatusCode, Res.ContentAsString(TEncoding.UTF8)]);
    end;
  finally
    ResponseStream.Free;
    Client.Free;
  end;
end;

function TAiClaudeChat.RetrieveFileList: TAiMediaFiles;
var
  Client: THTTPClient;
  Headers: TNetHeaders;
  ResponseStream: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: string;
  jObj: TJSonObject;
  jArr: TJSonArray;
  jItem: TJSONValue;
  NewMediaFile: TAiMediaFile;
begin
  Result := TAiMediaFiles.Create;
  sUrl := Url + 'files';
  Client := THTTPClient.Create;
  ResponseStream := TMemoryStream.Create;
  try
    Headers := GetFileHeaders;
    Res := Client.Get(sUrl, ResponseStream, Headers);

    if Res.StatusCode = 200 then
    begin
      ResponseStream.Position := 0;
      jObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      try
        if jObj.TryGetValue<TJSonArray>('data', jArr) then
        begin
          for jItem in jArr do
          begin
            if jItem is TJSonObject then
            begin
              NewMediaFile := TAiMediaFile.Create;
              PopulateMediaFileFromJson(NewMediaFile, jItem as TJSonObject);
              Result.Add(NewMediaFile);
            end;
          end;
        end;
      finally
        jObj.Free;
      end;
    end
    else
    begin
      Result.Free;
      ResponseStream.Position := 0;
      raise Exception.CreateFmt('Error al listar archivos de Claude: %d - %s', [Res.StatusCode, Res.ContentAsString(TEncoding.UTF8)]);
    end;
  finally
    ResponseStream.Free;
    Client.Free;
  end;
end;

function TAiClaudeChat.UploadFile(aMediaFile: TAiMediaFile): String;
var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Body: TMultipartFormData;
  ResponseStream: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: string;
  jObj: TJSonObject;
  TempStream: TMemoryStream;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un TAiMediaFile con contenido para subirlo.');

  sUrl := Url + 'files';
  Client := THTTPClient.Create;
  ResponseStream := TMemoryStream.Create;
  Body := TMultipartFormData.Create;
  TempStream := TMemoryStream.Create;
  try
    Headers := GetFileHeaders;

    // Se copia el contenido a un stream temporal para asegurar que la posición sea 0
    // y no afectar el stream original.
    aMediaFile.Content.Position := 0;
    TempStream.LoadFromStream(aMediaFile.Content);
    TempStream.Position := 0;

    Body.AddStream('file', TempStream, aMediaFile.FileName, aMediaFile.MimeType);

    Res := Client.Post(sUrl, Body, ResponseStream, Headers);

    if Res.StatusCode = 200 then
    begin
      ResponseStream.Position := 0;
      jObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      try
        Result := jObj.GetValue<string>('id');
        // Actualizamos el objeto original con la información devuelta
        aMediaFile.IdFile := Result;
        aMediaFile.CloudName := jObj.GetValue<string>('filename');
      finally
        jObj.Free;
      end;
    end
    else
    begin
      ResponseStream.Position := 0;
      raise Exception.CreateFmt('Error al subir archivo a Claude: %d - %s', [Res.StatusCode, Res.ContentAsString(TEncoding.UTF8)]);
    end;

  finally
    Body.Free; // Body es dueño de TempStream, por lo que no necesita Free explícito.
    ResponseStream.Free;
    Client.Free;
  end;
end;

function TAiClaudeChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer): String;
begin
  // La API de Anthropic no tiene un concepto de "caché" o TTL.
  // Simplemente se sube un archivo. Esta función es un alias de UploadFile.
  Result := Self.UploadFile(aMediaFile);
end;

class function TAiClaudeChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiClaudeChat.Create(Sender);
End;

function TAiClaudeChat.CheckFileState(aMediaFile: TAiMediaFile): String;
var
  TempMediaFile: TAiMediaFile;
begin
  Result := '';
  if not Assigned(aMediaFile) or aMediaFile.IdFile.IsEmpty then
    raise Exception.Create('Se necesita un TAiMediaFile con un IdFile válido para comprobar su estado.');
  try
    // La "comprobación de estado" es ver si podemos recuperar el archivo.
    TempMediaFile := Self.RetrieveFile(aMediaFile.IdFile);
    try
      if Assigned(TempMediaFile) then
        Result := TempMediaFile.IdFile; // Si existe, devolvemos su ID.
    finally
      TempMediaFile.Free;
    end;
  except
    on E: Exception do
    begin
      // Si RetrieveFile lanza una excepción (ej. 404 Not Found), el archivo no existe o hay un problema.
      // Devolvemos una cadena vacía para indicar que no está disponible.
      Result := '';
      // Opcionalmente, podrías relanzar la excepción si el error no es un 404.
      // DoError(E.Message, E);
    end;
  end;
end;

constructor TAiClaudeChat.Create(Sender: TComponent);
begin
  inherited;

  ApiKey := '@CLAUDE_API_KEY';
  FClient.OnReceiveData := Self.OnInternalReceiveData;

  FStreamContentBlocks := TDictionary<Integer, TClaudeStreamContentBlock>.Create;
  FStreamBuffer := TStringBuilder.Create;
  FStreamResponseMsg := nil;
  FStreamLastEventType := '';

  FClient.ResponseTimeOut := 60000;

  Model := 'claude-3-haiku-20240307';
  N := 1;
  Response_format := TAiChatResponseFormat.tiaChatRfText;
  Temperature := 1;
  User := 'user';
  InitialInstructions.Text := 'Eres un asistente muy útil y servicial';
  Max_tokens := 300;
  Url := GlAIUrl;
  Top_p := 1;
  ResponseTimeOut := 60000;

end;

function TAiClaudeChat.DeleteFile(aMediaFile: TAiMediaFile): String;
var
  Client: THTTPClient;
  Headers: TNetHeaders;
  ResponseStream: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: string;
  jObj: TJSonObject;
begin
  Result := '';
  if not Assigned(aMediaFile) or aMediaFile.IdFile.IsEmpty then
    raise Exception.Create('Se necesita un TAiMediaFile con un IdFile válido para eliminarlo.');

  sUrl := Url + 'files/' + aMediaFile.IdFile;
  Client := THTTPClient.Create;
  ResponseStream := TMemoryStream.Create;
  try
    Headers := GetFileHeaders;
    Res := Client.Delete(sUrl, ResponseStream, Headers);

    if Res.StatusCode = 200 then
    begin
      ResponseStream.Position := 0;
      jObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      try
        Result := jObj.GetValue<string>('id');
      finally
        jObj.Free;
      end;
    end
    else
    begin
      ResponseStream.Position := 0;
      raise Exception.CreateFmt('Error al eliminar el archivo "%s" de Claude: %d - %s',
        [aMediaFile.IdFile, Res.StatusCode, Res.ContentAsString(TEncoding.UTF8)]);
    end;
  finally
    ResponseStream.Free;
    Client.Free;
  end;
end;

destructor TAiClaudeChat.Destroy;
begin
  ClearStreamState;
  FStreamContentBlocks.Free;
  FStreamBuffer.Free;
  inherited;
end;

procedure TAiClaudeChat.ClearStreamState;
begin
  FStreamBuffer.Clear;
  FStreamContentBlocks.Clear; // TDictionary se encarga de liberar los TClaudeStreamContentBlock
  FreeAndNil(FStreamResponseMsg);
  FStreamLastEventType := '';
end;

function TAiClaudeChat.DownLoadFile(aMediaFile: TAiMediaFile): String;
var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: string;
begin
  Result := '';
  if not Assigned(aMediaFile) or aMediaFile.IdFile.IsEmpty then
    raise Exception.Create('Se necesita un TAiMediaFile con un IdFile válido para descargarlo.');

  sUrl := Url + 'files/' + aMediaFile.IdFile + '/content';
  Client := THTTPClient.Create;
  try
    Headers := GetFileHeaders;
    aMediaFile.Content.Clear; // Preparamos el stream para recibir los datos

    Res := Client.Get(sUrl, aMediaFile.Content, Headers);

    if Res.StatusCode = 200 then
    begin
      aMediaFile.Content.Position := 0;
      Result := aMediaFile.IdFile; // Devolvemos el ID como confirmación
    end
    else
    begin
      raise Exception.CreateFmt('Error al descargar el contenido del archivo "%s" de Claude: %d', [aMediaFile.IdFile, Res.StatusCode]);
    end;
  finally
    Client.Free;
  end;
end;

function TAiClaudeChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  Arg: TJSonObject;
  JVal1: TJSONValue;
  Fun: TAiToolsFunction;
  I: Integer;
  Nom, Valor: String;
begin
  Result := TAiToolsFunctions.Create;

  For JVal1 in jChoices do
  Begin
    If JVal1.GetValue<String>('type') = 'tool_use' then
    Begin
      Fun := TAiToolsFunction.Create;
      Fun.Id := JVal1.GetValue<String>('id');
      Fun.Tipo := 'function';
      Fun.Name := JVal1.GetValue<String>('name');
      If JVal1.TryGetValue<TJSonObject>('input', Arg) then
      Begin
        Fun.Arguments := Arg.Format;
        For I := 0 to Arg.Count - 1 do
        Begin
          Nom := Arg.Pairs[I].JsonString.Value;
          Valor := Arg.Pairs[I].JsonValue.Value;
          Fun.Params.Values[Nom] := Valor;
        End;
      End;
      // aquí debe ir los parámetros
      Result.Add(Fun.Id, Fun);
    End;
  End;
end;

function TAiClaudeChat.ExtractToolCallJson(jChoices: TJSonArray): TJSonArray;
Var
  jObj, Arg: TJSonObject;
  JVal1: TJSONValue;
begin

  Result := TJSonArray.Create;

  For JVal1 in jChoices do
  Begin
    If JVal1.GetValue<String>('type') = 'tool_use' then
    Begin
      jObj := TJSonObject.Create;
      jObj.AddPair('type', 'tool_use');
      jObj.AddPair('id', JVal1.GetValue<String>('id'));
      jObj.AddPair('name', JVal1.GetValue<String>('name'));
      If JVal1.TryGetValue<TJSonObject>('input', Arg) then
        jObj.AddPair('input', Arg);
      Result.Add(jObj);
    End;
  End;
end;

function TAiClaudeChat.GetMessages: TJSonArray;
var
  LMessage: TAiChatMessage;
  LMessageObj, LPartObj, LSourceObj: TJSonObject;
  LContentArray: TJSonArray;
  LMediaFile: TAiMediaFile;
  MediaArr: TAiMediaFilesArray; // <-- Variable restaurada y en uso
begin
  Result := TJSonArray.Create;

  for LMessage in Self.Messages do
  begin
    LMessageObj := TJSonObject.Create;
    LMessageObj.AddPair('role', LMessage.Role);
    LContentArray := TJSonArray.Create;

    // --- Lógica Principal de Construcción de Contenido ---

    // CASO 1: Es la respuesta a una llamada de herramienta (tool_result)
    if (LMessage.Role = 'user') and (not LMessage.TollCallId.IsEmpty) then
    begin
      LPartObj := TJSonObject.Create;
      LPartObj.AddPair('type', 'tool_result');
      LPartObj.AddPair('tool_use_id', LMessage.TollCallId);
      LPartObj.AddPair('content', LMessage.Prompt);
      LContentArray.Add(LPartObj);
    end

    // CASO 2: Es un mensaje del asistente que pide usar una herramienta (tool_use)
    else if (LMessage.Role = 'assistant') and (LMessage.Tool_calls <> '') then
    begin
      try
        var
        LToolUseArray := TJSonArray.ParseJSONValue(LMessage.Tool_calls) as TJSonArray;
        LContentArray.Free;
        LContentArray := LToolUseArray;
      except
        LPartObj := TJSonObject.Create;
        LPartObj.AddPair('type', 'text');
        LPartObj.AddPair('text', 'Error: Tool_calls con formato JSON inválido.');
        LContentArray.Add(LPartObj);
      end;
    end

    // CASO 3: Es un mensaje normal del usuario (puede contener texto y/o medios)
    else
    begin
      var
      bHasContent := False;

      // 3a. Añadir el bloque de texto si existe
      if not LMessage.Prompt.IsEmpty then
      begin
        LPartObj := TJSonObject.Create;
        LPartObj.AddPair('type', 'text');
        LPartObj.AddPair('text', LMessage.Prompt);
        LContentArray.Add(LPartObj);
        bHasContent := True;
      end;

      // 3b. Obtener la lista de medios filtrados según NativeInputFiles
      MediaArr := LMessage.MediaFiles.GetMediaList(Self.NativeInputFiles, False);
      if Length(MediaArr) > 0 then
      begin
        // Iteramos sobre el array filtrado 'MediaArr'
        for LMediaFile in MediaArr do
        begin
          // Recordatorio: La API de Claude v1/messages solo soporta imágenes inline.
          // Tu GetMediaList ya debería filtrar por tipo, pero una doble comprobación no hace daño.

          LPartObj := TJSonObject.Create;
          Case LMediaFile.FileCategory of
            tfc_textFile:
              Begin
                LPartObj.AddPair('type', 'text');
              End;
            Tfc_Document:
              Begin
                LPartObj.AddPair('type', 'document');
              End;
            Tfc_Image:
              Begin
                LPartObj.AddPair('type', 'image');
              End;
            Tfc_pdf:
              Begin
                LPartObj.AddPair('type', 'document');
              End;
          End;

          LSourceObj := TJSonObject.Create;
          LSourceObj.AddPair('type', 'base64');
          LSourceObj.AddPair('media_type', LMediaFile.MimeType);
          LSourceObj.AddPair('data', LMediaFile.Base64);

          LPartObj.AddPair('source', LSourceObj);
          LContentArray.Add(LPartObj);
          bHasContent := True;
        end;
      end;

      // Si el mensaje está completamente vacío (sin texto ni medios válidos),
      // añadimos un bloque de texto vacío para evitar errores.
      if not bHasContent then
      begin
        LPartObj := TJSonObject.Create;
        LPartObj.AddPair('type', 'text');
        LPartObj.AddPair('text', '');
        LContentArray.Add(LPartObj);
      end;
    end;

    LMessageObj.AddPair('content', LContentArray);
    Result.Add(LMessageObj);
  end;
end;

class function TAiClaudeChat.GetModels(aApiKey, aUrl: String): TStringList;
var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  ResponseStream: TStringStream;
  sUrl, EndPointUrl, sModel: string;
  jRes: TJSonObject;
  jArr: TJSonArray;
  JVal: TJSONValue;
  CustomModels: TArray<string>;
  I: Integer;
begin
  Result := TStringList.Create;

  if aUrl <> '' then
    EndPointUrl := aUrl
  else
    EndPointUrl := GlAIUrl;

  Client := THTTPClient.Create;
  ResponseStream := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'models';

  try
    // La API de modelos de Claude solo requiere la clave y la versión
    Headers := [TNetHeader.Create('x-api-key', aApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, ResponseStream, Headers);

    if Res.StatusCode = 200 then
    begin
      jRes := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      if Assigned(jRes) then
        try
          // La estructura de respuesta es {"data": [...]}
          if jRes.TryGetValue<TJSonArray>('data', jArr) then
          begin
            for JVal in jArr do
            begin
              if JVal is TJSonObject then
              begin
                // El nombre del modelo está en el campo 'id'
                sModel := (JVal as TJSonObject).GetValue<string>('id', '');
                if sModel <> '' then
                  Result.Add(sModel);
              end;
            end;
          end;
        finally
          jRes.Free;
        end;

      // Agregar modelos personalizados
      CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);

      for I := Low(CustomModels) to High(CustomModels) do
      begin
        if not Result.Contains(CustomModels[I]) then
          Result.Add(CustomModels[I]);
      end;

    end
    else
    begin
      raise Exception.CreateFmt('Error al obtener modelos de Claude: %d - %s', [Res.StatusCode, Res.ContentAsString(TEncoding.UTF8)]);
    end;
  finally
    Client.Free;
    ResponseStream.Free;
  end;
end;

function TAiClaudeChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice: TJSonObject;
  jArrTools, jArrStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res, LModel: String;
begin
  // --- 1. CONFIGURACIÓN INICIAL Y VALORES POR DEFECTO ---
  if User = '' then
    User := 'user';
  // Usamos un modelo más reciente como default, ej. el nuevo Sonnet 3.5

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  if LModel = '' then
    LModel := 'claude-3-5-sonnet-20240620';

  // Deshabilitar streaming si se están usando herramientas es una práctica segura.
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);
  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;
  Try
    // --- 2. PARÁMETROS PRINCIPALES DE LA API DE CLAUDE ---
    AJSONObject.AddPair('model', LModel);
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    // Llamada a la función GetMessages corregida, que genera el 'content' como un array.
    AJSONObject.AddPair('messages', GetMessages);

    // --- 3. CONFIGURACIÓN DE HERRAMIENTAS (FUNCTION CALLING) ---
    if Tool_Active and (Trim(Tools.Text) <> '') then
    begin
      try
        jArrTools := GenerateClaudeToolsJson;

        if not Assigned(jArrTools) then
          Raise Exception.Create('La propiedad Tools tiene un formato JSON inválido.')
        else
        begin
          AJSONObject.AddPair('tools', jArrTools);

          if (Trim(Tool_choice) <> '') then
          begin
            // Intenta parsear como objeto, si falla, lo trata como string ('auto', 'any', 'none')
            try
              jToolChoice := TJSonObject.ParseJSONValue(Tool_choice) as TJSonObject;
              AJSONObject.AddPair('tool_choice', jToolChoice);
            except
              AJSONObject.AddPair('tool_choice', Tool_choice);
            end;
          end;
        end;
      except
        on E: Exception do
          Raise Exception.Create('Error al procesar la propiedad Tools: ' + E.Message);
      end;
    end;

    // --- 4. OTROS PARÁMETROS SOPORTADOS POR CLAUDE ---
    AJSONObject.AddPair('temperature', TJSONNumber.Create(Self.Temperature)); // Claude prefiere valores entre 0.0 y 1.0

    if Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    // CORRECCIÓN: El parámetro se llama 'stop_sequences'
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      jArrStop := TJSonArray.Create;
      for I := 0 to Lista.Count - 1 do
        jArrStop.Add(Lista[I]);
      AJSONObject.AddPair('stop_sequences', jArrStop);
    end;

    // Parámetro de streaming
    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    // --- 5. PARÁMETROS INCOMPATIBLES ELIMINADOS ---
    // Los parámetros 'logprobs', 'logit_bias', 'top_logprobs', 'seed' no son
    // soportados por la API de Claude y han sido eliminados de esta función.

    // --- 6. GENERACIÓN FINAL DEL JSON ---
    Res := UTF8ToString(AJSONObject.ToJSon);
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);

  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

{
  function TAiClaudeChat.InitChatCompletions: String;
  Var
  AJSONObject, jToolChoice: TJSonObject;
  jArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res: String;
  begin

  If User = '' then
  User := 'user';

  If Model = '' then
  Model := 'claude-3-haiku-20240307';

  // claude-3-opus-20240229
  // claude-3-sonnet-20240229
  // claude-3-haiku-20240307

  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

  AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));



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
  //AJSONObject.AddPair('parallel_tool_calls', FParallel_ToolCalls);

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


  AJSONObject.AddPair('messages', GetMessages); // FMessages.ToJSon);
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
}

function TAiClaudeChat.InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
Var
  InitMsg: TAiChatMessage;
  MensajeInicial: String;
  MF: TAiMediaFile;
  Procesado: Boolean;
  Respuesta: String;
begin

  Try
    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then //Solo acepta esto al iniciar el chat
    Begin

      // Si el formato de respuesta es Json, siempre debe llevar en la instrucción que el formato sea json
      MensajeInicial := Self.PrepareSystemMsg;

      InitMsg := TAiChatMessage.Create(MensajeInicial, 'user');
      InitMsg.Id := FMessages.Count + 1;
      FMessages.Add(InitMsg);

      InitMsg := TAiChatMessage.Create('De acuerdo, seguiré las instrucciones', 'assistant');
      InitMsg.Id := FMessages.Count + 1;
      FMessages.Add(InitMsg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, InitMsg, Nil, 'system', MensajeInicial);
    End;

    // Adiciona el mensaje a la lista
    aMsg.Id := FMessages.Count + 1;
    FMessages.Add(aMsg);

    If Assigned(FOnAddMessage) then
    Begin
      FOnAddMessage(Self, aMsg, Nil, aMsg.Role, aMsg.Prompt);
    End;

    For MF in aMsg.MediaFiles do
    Begin
      DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado); // Envía el archivo por si lo quiere procesar otra AI especializada, Ej.
      MF.Procesado := Procesado;
      MF.Transcription := Respuesta;
      // Guarda las transcripciones en los MediaFile,  luego construye la respuesta definitiva con todos los mediafiles
    End;

    FLastPrompt := aMsg.Prompt; // aqui lleva el Prompt Inicial + la conversión de los MediaFiles a texto si el usuario lo permite

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, aMsg);

    Result := aMsg;

  Finally

  End;
end;

function TAiClaudeChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  jObj: TJSonObject;
begin
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';
  ClearStreamState; // Limpia el estado del stream anterior

  sUrl := Url + 'messages';

  try
    FHeaders := [TNetHeader.Create('x-api-key', ApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION),
      TNetHeader.Create('content-type', 'application/json')];
    if Tool_Active then
      FHeaders := FHeaders + [TNetHeader.Create('anthropic-beta', CLAUDE_TOOLS_BETA_HEADER)];

    FClient.ContentType := 'application/json';
    FClient.Asynchronous := Self.Asynchronous; // Usar la propiedad del componente

    // Preparamos el mensaje de respuesta si es un stream
    if FClient.Asynchronous then
    begin
      FStreamResponseMsg := TAiChatMessage.Create('', 'assistant');
    end;

    ABody := InitChatCompletions;
    St := TStringStream.Create(ABody, TEncoding.UTF8);
    try
      St.Position := 0;
{$IFDEF APIDEBUG}
      St.SaveToFile('c:\temp\peticion.txt');
      St.Position := 0;
{$ENDIF}
      FResponse.Clear;

      Res := FClient.Post(sUrl, St, FResponse, FHeaders);

      FResponse.Position := 0;
{$IFDEF APIDEBUG}
      FResponse.SaveToFile('c:\temp\respuesta.txt');
      FResponse.Position := 0;
{$ENDIF}
      if not FClient.Asynchronous then
      begin
        // --- MODO SÍNCRONO ---
        if Res.StatusCode = 200 then
        begin
          jObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
          try
            ParseChat(jObj, ResMsg);
            Result := FLastContent;
          finally
            jObj.Free;
          end;
        end
        else
        begin
          raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
        end;
      end
      else
      begin
        // --- MODO ASÍNCRONO (STREAM) ---
        // La respuesta se manejará en OnInternalReceiveData.
        // OnRequestCompleted se encargará de los errores.
        Result := '';
      end;
    finally
      if not FClient.Asynchronous then
        St.Free;
    end;
  finally
    if not FClient.Asynchronous then
      FBusy := False;
  end;
End;

procedure TAiClaudeChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  line: string;
begin
  if not FClient.Asynchronous then
    Exit;

  AAbort := FAbort;
  if FAbort then
  begin
    FBusy := False;
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    ClearStreamState;
    Exit;
  end;

  // Acumular los datos recibidos en el buffer
  FStreamBuffer.Append(UTF8Encode(FResponse.DataString));
  FResponse.Clear;

  var
  bufferContent := FStreamBuffer.ToString;
  var
  lastLF := LastDelimiter(#10, bufferContent);

  if lastLF > 0 then
  begin
    var
    processableContent := Copy(bufferContent, 1, lastLF);
    FStreamBuffer.Remove(0, lastLF); // Dejar en el buffer lo que no se procesó

    // Dividir en líneas y procesar cada una
    // Lines :=
    for line in processableContent.Split([#10]) do
    begin
      var
      line1 := Trim(line);
      if not line1.IsEmpty then
        ProcessStreamChunk(line1);
    end;
  end;

end;

procedure TAiClaudeChat.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
Var
  choices: TJSonArray;
  jItem, JToolCalls: TJSonObject;
  JVal: TJSONValue;
  uso: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta: String;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;
  ToolMsg, AskMsg: TAiChatMessage;

  TaskList: array of ITask;
  I, NumTasks: Integer;
  Id, Clave, sToolCalls, LModel: String;

  Code: TMarkdownCodeExtractor;
  CodeFile: TCodeFile;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;

begin
  AskMsg := GetLastMessage; // Obtiene la pregunta, ya que ResMsg se adiciona a la lista si no hay errores.

  Id := jObj.GetValue('id').Value;
  LModel := jObj.GetValue('model').Value;
  Role := jObj.GetValue('role').Value;
  If jObj.TryGetValue<TJSonObject>('usage', uso) then
  Begin
    aPrompt_tokens := uso.GetValue<Integer>('input_tokens');
    aCompletion_tokens := uso.GetValue<Integer>('output_tokens');
    aTotal_tokens := aPrompt_tokens + aCompletion_tokens;
  End
  else
  Begin
    aPrompt_tokens := 0;
    aCompletion_tokens := 0;
    aTotal_tokens := 0;
  End;

  jObj.TryGetValue<TJSonArray>('content', choices);

  jItem := nil;
  For JVal in choices do
  Begin
    jItem := TJSonObject(JVal);
    If jItem.GetValue<String>('type') = 'text' then
      Respuesta := Respuesta + jItem.GetValue<String>('text') + sLineBreak;
  End;

  // Solo toma el último elemento de la lista para construir este item
  If Assigned(jItem) and (jItem.GetValue<String>('type') = 'tool_use') then
  Begin
    JToolCalls := TJSonObject.Create;
    JToolCalls.AddPair('role', Role);

    JToolCalls.AddPair('content', ExtractToolCallJson(choices));

    sToolCalls := JToolCalls.Format;
  End;

  LFunciones := ExtractToolCallFromJson(choices);

  Respuesta := Trim(Respuesta);

  Self.FLastContent := Respuesta;
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + aTotal_tokens;

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
        ToolCall.ResMsg := ResMsg;
        ToolCall.AskMsg := AskMsg;

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
        ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'user', ToolCall.Id, ToolCall.Name);
        ToolMsg.Id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
      End;

      Self.Run(Nil, ResMsg);

    End
    Else
    Begin

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

      If Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, ResMsg, jObj, Role, Respuesta);
    End;
  Finally
    LFunciones.Free;
  End;
end;

procedure TAiClaudeChat.ProcessStreamChunk(const AChunk: string);
var
  jData, jDelta, jBlock: TJSonObject;
  eventType, textDelta, jsonDelta: string;
  blockIndex: Integer;
  streamBlock: TClaudeStreamContentBlock;
begin
  if AChunk.StartsWith('event:') then
  begin
    FStreamLastEventType := Trim(Copy(AChunk, 7, Length(AChunk)));
    Exit;
  end;

  if AChunk.StartsWith('data:') then
  begin
    if FStreamLastEventType = '' then
      Exit; // Ignorar datos sin un evento previo

    var
    jsonDataStr := Trim(Copy(AChunk, 6, Length(AChunk)));
    if jsonDataStr.IsEmpty then
      Exit;

    jData := TJSonObject.ParseJSONValue(jsonDataStr) as TJSonObject;
    if not Assigned(jData) then
      Exit;

    try
      eventType := jData.GetValue<string>('type');

      If AnsiLowerCase(eventType) = 'message_start' then
      begin
        if Assigned(FStreamResponseMsg) then
        begin
          var
          jMessage := jData.GetValue<TJSonObject>('message');
          FStreamResponseMsg.TollCallId := jMessage.GetValue<string>('id'); // Usamos TollCallId para el ID de mensaje
          if Assigned(OnReceiveData) then
            OnReceiveData(Self, FStreamResponseMsg, jData, 'assistant', '');
        end;
      end
      Else

        If AnsiLowerCase(eventType) = 'content_block_start' then
      begin
        blockIndex := jData.GetValue<Integer>('index');
        jBlock := jData.GetValue<TJSonObject>('content_block');
        streamBlock := TClaudeStreamContentBlock.Create;
        streamBlock.BlockType := jBlock.GetValue<string>('type');
        if streamBlock.BlockType = 'tool_use' then
        begin
          streamBlock.ToolFunction := TAiToolsFunction.Create;
          streamBlock.ToolFunction.Id := jBlock.GetValue<string>('id');
          streamBlock.ToolFunction.Name := jBlock.GetValue<string>('name');
          streamBlock.ToolFunction.Tipo := 'function';
        end;
        FStreamContentBlocks.Add(blockIndex, streamBlock);
      end
      Else

        If AnsiLowerCase(eventType) = 'content_block_delta' then
      begin
        blockIndex := jData.GetValue<Integer>('index');
        if FStreamContentBlocks.TryGetValue(blockIndex, streamBlock) then
        begin
          jDelta := jData.GetValue<TJSonObject>('delta');
          if jDelta.GetValue<string>('type') = 'text_delta' then
          begin
            textDelta := jDelta.GetValue<string>('text');
            streamBlock.TextContent.Append(textDelta);
            FLastContent := FLastContent + textDelta;
            if Assigned(OnReceiveData) then
              OnReceiveData(Self, FStreamResponseMsg, jData, 'assistant', textDelta);
          end
          else if jDelta.GetValue<string>('type') = 'input_json_delta' then
          begin
            jsonDelta := jDelta.GetValue<string>('partial_json');
            streamBlock.JsonContent.Append(jsonDelta);
          end;
        end;
      end
      Else

        If AnsiLowerCase(eventType) = 'content_block_stop' then
      begin
        blockIndex := jData.GetValue<Integer>('index');
        if FStreamContentBlocks.TryGetValue(blockIndex, streamBlock) then
        begin
          if streamBlock.BlockType = 'tool_use' then
          begin
            // El JSON de los argumentos está completo, lo parseamos.
            try
              var
              jInput := TJSonObject.ParseJSONValue(streamBlock.JsonContent.ToString) as TJSonObject;
              if Assigned(jInput) then
              begin
                streamBlock.ToolFunction.Arguments := jInput.Format;
                for var Pair in jInput do
                  streamBlock.ToolFunction.Params.AddPair(Pair.JsonString.Value, Pair.JsonValue.Value);
                jInput.Free;
              end;
            except
              // Error parseando el JSON de argumentos
              DoError('Error parsing tool arguments JSON: ' + streamBlock.JsonContent.ToString, nil);
            end;
          end;
        end;
      end
      Else

        If AnsiLowerCase(eventType) = 'message_delta' then
      begin
        // Aquí podríamos actualizar el 'stop_reason' y el 'usage'
        if Assigned(FStreamResponseMsg) then
        begin
          var
          jUsage := jData.GetValue<TJSonObject>('usage');
          if Assigned(jUsage) then
          begin
            FStreamResponseMsg.Completion_tokens := jUsage.GetValue<Integer>('output_tokens');
          end;
        end;
      end
      Else

        If AnsiLowerCase(eventType) = 'message_stop' then
      begin
        // El stream ha terminado.
        if Assigned(FStreamResponseMsg) then
        begin
          var
          LFunciones := TAiToolsFunctions.Create;
          try
            // Recolectar el texto final y las funciones
            var
            fullText := TStringBuilder.Create;
            try
              for streamBlock in FStreamContentBlocks.Values do
              begin
                fullText.Append(streamBlock.TextContent.ToString);
                if Assigned(streamBlock.ToolFunction) then
                begin
                  LFunciones.Add(streamBlock.ToolFunction.Id, streamBlock.ToolFunction);
                  streamBlock.ToolFunction := nil; // Evitar doble free
                end;
              end;
              FStreamResponseMsg.Prompt := fullText.ToString;
              FStreamResponseMsg.Content := FStreamResponseMsg.Prompt;
            finally
              fullText.Free;
            end;

            InternalAddMessage(FStreamResponseMsg);

            if Assigned(FOnReceiveDataEnd) then
              FOnReceiveDataEnd(Self, FStreamResponseMsg, jData, 'assistant', FStreamResponseMsg.Content);

            // Si hubo herramientas, ejecutarlas ahora
            if LFunciones.Count > 0 then
            begin
              // La lógica de ejecución de herramientas en paralelo iría aquí
              // ... (similar a la de ParseChat síncrono) ...
              DoError('La ejecución de herramientas en modo Streaming aún no está completamente implementada.', nil);
            end;

          finally
            LFunciones.Free;
            ClearStreamState;
            FBusy := False;
          end;
        end;
      end
      Else If AnsiLowerCase(eventType) = 'ping' then
      Begin
      end
      else If AnsiLowerCase(eventType) = 'error' then
      begin
        var
        jError := jData.GetValue<TJSonObject>('error');
        DoError('Error en Stream de Claude: ' + jError.GetValue<string>('message'), nil);
        ClearStreamState;
        FBusy := False;
      end;
    finally
      jData.Free;
      FStreamLastEventType := ''; // Reset para el próximo par evento/data
    end;
  end;
end;

// Implementación
function TAiClaudeChat.GenerateClaudeToolsJson: TJSonArray;
var
  LFunctionAction: TFunctionActionItem;
  Col: TCollectionItem;
  LClaudeTool: TJSonObject;
  LInputSchema: TJSonObject;
begin

  if not Assigned(AiFunctions) then
  begin
    Result := nil;
    Exit;
  end;

  Result := TJSonArray.Create;

  // Iteramos a través de las funciones definidas en el componente TAiFunctions
  for Col in AiFunctions.Functions do
  begin
    LFunctionAction := TFunctionActionItem(Col);

    // Solo procesamos las herramientas que están habilitadas y son del tipo 'function'
    if (not LFunctionAction.Enabled) or (LFunctionAction.ToolType <> tt_function) then
      Continue;

    // Creamos el objeto JSON para la herramienta en formato Claude
    LClaudeTool := TJSonObject.Create;

    // 1. Mapear 'name'
    LClaudeTool.AddPair('name', LFunctionAction.FunctionName);

    // 2. Mapear 'description'
    LClaudeTool.AddPair('description', LFunctionAction.Description.Text.Trim);

    // 3. Mapear 'parameters' a 'input_schema'
    // Reutilizamos la lógica existente en TFunctionParamsItems que ya genera un JSON Schema válido.
    LInputSchema := LFunctionAction.Parameters.ToJSon(False);

    if Assigned(LInputSchema) then
    begin
      // Si la función tiene parámetros, usamos el JSON Schema generado
      LClaudeTool.AddPair('input_schema', LInputSchema);
    end
    else
    begin
      // Si no tiene parámetros, Claude requiere un schema vacío explícito
      LInputSchema := TJSonObject.Create;
      LInputSchema.AddPair('type', 'object');
      LInputSchema.AddPair('properties', TJSonObject.Create);
      LClaudeTool.AddPair('input_schema', LInputSchema);
    end;

    // Añadimos la herramienta traducida al array final
    Result.Add(LClaudeTool);
  end;

  // Si no se añadió ninguna herramienta, liberamos el array y devolvemos nil
  if Result.Count = 0 then
  begin
    FreeAndNil(Result);
  end;
end;

{ TClaudeStreamContentBlock }

constructor TClaudeStreamContentBlock.Create;
begin
  TextContent := TStringBuilder.Create;
  JsonContent := TStringBuilder.Create;
  ToolFunction := nil; // Se crea bajo demanda
end;

destructor TClaudeStreamContentBlock.Destroy;
begin
  TextContent.Free;
  JsonContent.Free;
  ToolFunction.Free;
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiClaudeChat);

end.
