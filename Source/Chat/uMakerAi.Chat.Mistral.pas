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

{
  -----------------------------------------------------------------------------
  TODO: Implementar Funcionalidad de Anotaciones de OCR de Mistral
  -----------------------------------------------------------------------------
  https://docs.mistral.ai/capabilities/OCR/annotations/

  Referencia API: https://docs.mistral.ai/api/#operation/ocr_api_routes_ocr_post

  CONTEXTO:
  La API de OCR de Mistral, además de extraer texto plano (Markdown), permite
  realizar "anotaciones". Esto significa que puede analizar el documento y

  extraer información estructurada en un formato JSON definido por el usuario.
  Es una funcionalidad de "Inteligencia Documental".

  Existen dos tipos de anotaciones que pueden usarse por separado o en conjunto:

  1. DOCUMENT ANNOTATION (`document_annotation_format`):
  - PROPOSITO: Extraer información global del documento completo (o de un
  subconjunto de páginas).
  - EJEMPLOS: "Clasifica el documento (factura, contrato)", "Extrae el
  nombre de la empresa y el año del informe", "Lista los títulos de los
  capítulos".
  - LIMITACIÓN: Funciona solo en las primeras 8 páginas del documento.

  2. BBOX ANNOTATION (`bbox_annotation_format`):
  - PROPOSITO: Analizar individualmente cada elemento visual (imágenes,
  gráficos, tablas visuales) que el OCR detecta en "cajas delimitadoras"
  (Bounding Boxes o BBoxes).
  - EJEMPLOS: "Para cada gráfico, descríbelo", "Convierte cada tabla visual
  a formato de datos", "Identifica si la imagen es un logo o una foto".
  - LIMITACIÓN: Sin límite de páginas (aplica a todo el documento).

  ---------------------------------
  PLAN DE IMPLEMENTACIÓN:
  ---------------------------------

  1. AÑADIR NUEVAS PROPIEDADES EN `TAiMistralChat`:
  - Se necesitan propiedades para que el usuario pueda definir los esquemas JSON
  y los parámetros de la anotación.

  property OcrDocumentAnnotationSchema: TStrings; // Almacena el schema JSON para la anotación del documento.
  property OcrBboxAnnotationSchema: TStrings;   // Almacena el schema JSON para la anotación de BBoxes.
  property OcrAnnotationPages: string;          // String con números de página separados por coma (ej: "0,1,2")
  // para `document_annotation`. Si está vacío, se aplica a todo
  // (respetando el límite de 8 páginas de la API).

  2. MODIFICAR `InternalRunOcr`:
  - Dentro de esta función, antes de construir el TJSONObject de la petición:
  a. Comprobar si `OcrDocumentAnnotationSchema` tiene contenido. Si es así,
  construir el objeto `document_annotation_format` completo (incluyendo
  "type": "json_schema" y el "json_schema" anidado) y añadirlo al
  JSON principal.
  b. Hacer lo mismo para `OcrBboxAnnotationSchema`.
  c. Si `OcrAnnotationPages` no está vacío, parsear el string y crear un
  TJSONArray de enteros para el parámetro `pages` de la petición.

  3. MODIFICAR `ParseOcrResponse`:
  - La respuesta de la API contendrá, además del array `pages` con el
  contenido Markdown, nuevos campos con las anotaciones.
  - Se debe buscar y parsear:
  * Un objeto `document_annotation` (si se solicitó).
  * Un array `bbox_annotations` dentro de cada elemento del array `pages`
  (si se solicitó).
  - La información extraída debe guardarse en nuevas propiedades del
  objeto `TAiMediaFile` que se pasa a la función.

  4. AÑADIR NUEVAS PROPIEDADES EN `TAiMediaFile`:
  - Para almacenar los resultados estructurados de las anotaciones.

  property DocumentAnnotation: string;     // Guarda el JSON resultante de la anotación del documento.
  property BboxAnnotations: TStrings;    // Guarda una lista de JSONs, uno por cada anotación de BBox.
  // O podría ser un TJSONArray directamente.

  -----------------------------------------------------------------------------
}

unit uMakerAi.Chat.Mistral;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.NetConsts,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.ToolFunctions, uMakerAi.Core, uMakerAi.Embeddings;

{ TODO : Falta crear las siguientes funciones de Mistral }
/// -----------------------------------------------------------------------------
/// Falta crear las siguientes funciones
/// https://docs.mistral.ai/api/#operation/jobs_api_routes_fine_tuning_create_fine_tuning_job
/// Delete Model
/// GET List Fine Tuning Jobs
/// POST Create Fine Tuning Job
/// GET  Get Fine Tuning Job
/// POST Cancel Fine Tuning Job
/// -----------------------------------------------------------------------------

type

  TMistralFile = record
    Id: String;
    &Object: String;
    Size_Bytes: Int64;
    Created_At: Int64;
    Filename: String;
    Purpose: String;
  end;

  TAiMistralChat = Class(TAiChat)
  Private
    FOcrIncludeImages: Boolean;
    FDocumentImageLimit: integer;
    FDocumentPageLimit: integer;
    FOcrBboxAnnotationSchema: TStringList;
    FOcrDocumentAnnotationSchema: TStringList;
    FOcrAnnotationPages: string;
    FOcrPagesNumbers: string;
    procedure SetOcrIncludeImages(const Value: Boolean);
    procedure SetDocumentImageLimit(const Value: integer);
    procedure SetDocumentPageLimit(const Value: integer);
    procedure SetOcrAnnotationPages(const Value: string);
    procedure SetOcrBboxAnnotationSchema(const Value: TStringList);
    procedure SetOcrDocumentAnnotationSchema(const Value: TStringList);
    procedure SetOcrPagesNumbers(const Value: string);
  Protected
    Function InitChatCompletions: String; Override;
    Function InitChatCompletionsFim(aPrompt, aSuffix: String): String; Virtual; // Se introduce solo acá
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;

    function InternalRunPDFDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;

    // function InternalRunOcr(aMediaFile: TAiMediaFile): String;
    function ParseOcrResponse(jResponse: TJSONObject; ResMsg: TAiChatMessage): string;

    function RetrieveFileMetadata(aFileId: string): TMistralFile; virtual;
    function GetSignedUrl(aMediaFile: TAiMediaFile): string;
    function GetSignedUrlById(const aFileId: string): string;
  Public
    Constructor Create(Sender: TComponent); Override;
    // Function AddMessageFimAndRun(aPrompt, aSuffix: String): String; Virtual;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

    // funciones de manejo de archivos

    Function UploadFile(aMediaFile: TAiMediaFile): String; Override;
    Function DeleteFile(aMediaFile: TAiMediaFile): String; Override;
    function RetrieveFile(aFileId: string): TAiMediaFile; Override;
    Function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: integer = 3600): String; Override;

  Published
    property OcrIncludeImages: Boolean read FOcrIncludeImages write SetOcrIncludeImages;
    property DocumentImageLimit: integer read FDocumentImageLimit write SetDocumentImageLimit;
    property DocumentPageLimit: integer read FDocumentPageLimit write SetDocumentPageLimit;

    property OcrDocumentAnnotationSchema: TStringList read FOcrDocumentAnnotationSchema write SetOcrDocumentAnnotationSchema;
    property OcrBboxAnnotationSchema: TStringList read FOcrBboxAnnotationSchema write SetOcrBboxAnnotationSchema;
    property OcrAnnotationPages: string read FOcrAnnotationPages write SetOcrAnnotationPages;
    property OcrPagesNumbers: string read FOcrPagesNumbers write SetOcrPagesNumbers;

  End;

  TAiMistralEmbeddings = Class(TAiEmbeddings)
  Public
    // groq actualmente no maneja modelos de embeddings
    Constructor Create(aOwner: TComponent); Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: integer = 1536; aModel: String = 'mistral-embed';
      aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;

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

class function TAiMistralChat.GetDriverName: string;
Begin
  Result := 'Mistral';
End;

function TAiMistralChat.GetSignedUrl(aMediaFile: TAiMediaFile): string;
var
  LResponse: IHTTPResponse;
  LUrl: string;
  LResponseObj: TJSONObject;
  LSignedUrl: string;
  LHeaders: TNetHeaders; // Añadido
begin
  Result := '';
  // MODIFICADO: Comprobar el campo correcto.
  if not Assigned(aMediaFile) or aMediaFile.IDFile.IsEmpty then
    raise Exception.Create('El TAiMediaFile debe tener un ID de archivo válido (en IDFile) para obtener una URL firmada.');

  // MODIFICADO: Usar el campo correcto para construir la URL.
  LUrl := Url + 'files/' + aMediaFile.IDFile + '/url';
  LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)]; // Añadir el Header de autorización
  LResponse := FClient.Get(LUrl, nil, LHeaders); // Añadir el header a la petición

  if LResponse.StatusCode = 200 then
  begin
    LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
    try
      LSignedUrl := LResponseObj.GetValue<string>('url');
      Result := LSignedUrl;

      // Actualizar el objeto MediaFile con la URL temporal.
      aMediaFile.UrlMedia := LSignedUrl;
      aMediaFile.CloudState := 'url-signed';
    finally
      LResponseObj.Free;
    end;
  end
  else
  begin
    aMediaFile.CloudState := 'url-sign-failed';
    FLastError := LResponse.ContentAsString;
    DoError(Format('Error al obtener URL firmada: %d - %s', [LResponse.StatusCode, FLastError]), nil);
  end;
end;

function TAiMistralChat.GetSignedUrlById(const aFileId: string): string;
var
  media: TAiMediaFile;
begin
  if aFileId.IsEmpty then
    Exit('');
  media := TAiMediaFile.Create;
  try
    media.IDFile := aFileId;
    Result := Self.GetSignedUrl(media);
  finally
    media.Free;
  end;
end;

class procedure TAiMistralChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@MISTRAL_API_KEY');
  Params.Add('Model=open-mistral-7b');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=https://api.mistral.ai/v1/');
End;

function TAiMistralChat.RetrieveFile(aFileId: string): TAiMediaFile;
var
  LMetadata: TMistralFile;
  LContentStream: TMemoryStream;
  LResponse: IHTTPResponse;
  LUrl: string;
begin
  Result := nil; // Por defecto, devolvemos nil si algo falla.
  if aFileId.IsEmpty then
    raise Exception.Create('Se requiere un ID de archivo para recuperarlo.');

  // 1. Obtener los metadatos primero para saber el nombre del archivo y otros detalles.
  LMetadata := RetrieveFileMetadata(aFileId);
  if LMetadata.Id.IsEmpty then
  begin
    // El error ya fue reportado por RetrieveFileMetadata.
    Exit;
  end;

  // 2. Ahora, obtener el contenido del archivo.
  LUrl := Url + 'files/' + aFileId + '/content';
  LContentStream := TMemoryStream.Create;
  try
    // La respuesta de este endpoint es el contenido binario, no JSON.
    // Lo capturamos directamente en un TMemoryStream.
    LResponse := FClient.Get(LUrl, LContentStream);

    if LResponse.StatusCode = 200 then
    begin
      LContentStream.Position := 0;

      // Crear y poblar el objeto TAiMediaFile.
      Result := TAiMediaFile.Create;
      try
        Result.LoadFromStream(LMetadata.Filename, LContentStream);
        // Guardamos el ID de Mistral para futuras referencias.
        // Reutilizamos IdAudio como en otras partes del código.
        Result.IDFile := aFileId;
      except
        // Si hay un error al crear o cargar, liberamos y devolvemos nil.
        FreeAndNil(Result);
        raise;
      end;
    end
    else
    begin
      // Si la descarga del contenido falla.
      LContentStream.Position := 0;
      Var
      LStringStream := TStringStream.Create('', TEncoding.UTF8);

      try
        LStringStream.CopyFrom(LContentStream, LContentStream.Size);
        FLastError := LStringStream.DataString;
      finally
        LStringStream.Free;
      end;
      DoError(Format('Error al recuperar contenido del archivo: %d - %s', [LResponse.StatusCode, FLastError]), nil);
      // Result sigue siendo nil, que es lo que queremos devolver en caso de error.
    end;
  finally
    LContentStream.Free;
  end;
end;

function TAiMistralChat.RetrieveFileMetadata(aFileId: string): TMistralFile;
var
  LResponse: IHTTPResponse;
  LUrl: string;
  LFileObj: TJSONObject;
begin
  // Inicializa el resultado
  FillChar(Result, SizeOf(TMistralFile), 0);
  Result.Id := '';

  if aFileId.IsEmpty then
    raise Exception.Create('Se requiere un ID de archivo para recuperar sus metadatos.');

  LUrl := Url + 'files/' + aFileId;
  LResponse := FClient.Get(LUrl);

  if LResponse.StatusCode = 200 then
  begin
    LFileObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
    try
      Result.Id := LFileObj.GetValue<string>('id');
      Result.&Object := LFileObj.GetValue<string>('object');
      Result.Size_Bytes := LFileObj.GetValue<Int64>('bytes');
      Result.Created_At := LFileObj.GetValue<Int64>('created_at');
      Result.Filename := LFileObj.GetValue<string>('filename');
      Result.Purpose := LFileObj.GetValue<string>('purpose');
    finally
      LFileObj.Free;
    end;
  end
  else
  begin
    FLastError := LResponse.ContentAsString;
    DoError(Format('Error al recuperar metadatos del archivo: %d - %s', [LResponse.StatusCode, FLastError]), nil);
  end;
end;

procedure TAiMistralChat.SetDocumentImageLimit(const Value: integer);
begin
  FDocumentImageLimit := Value;
end;

procedure TAiMistralChat.SetDocumentPageLimit(const Value: integer);
begin
  FDocumentPageLimit := Value;
end;

procedure TAiMistralChat.SetOcrAnnotationPages(const Value: string);
begin
  FOcrAnnotationPages := Value;
end;

procedure TAiMistralChat.SetOcrBboxAnnotationSchema(const Value: TStringList);
begin
  FOcrBboxAnnotationSchema := Value;
end;

procedure TAiMistralChat.SetOcrDocumentAnnotationSchema(const Value: TStringList);
begin
  FOcrDocumentAnnotationSchema := Value;
end;

procedure TAiMistralChat.SetOcrIncludeImages(const Value: Boolean);
begin
  FOcrIncludeImages := Value;
end;

procedure TAiMistralChat.SetOcrPagesNumbers(const Value: string);
begin
  FOcrPagesNumbers := Value;
end;

{ function TAiMistralChat.UploadFile(aMediaFile: TAiMediaFile): String;
  var
  LBody: TMultipartFormData;
  LTempStream: TMemoryStream;
  LResponse: IHTTPResponse;
  LUrl: string;
  LResponseObj: TJSONObject;
  begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
  raise Exception.Create('Se necesita un TAiMediaFile con contenido para subir.');

  LUrl := Url + 'files';
  LBody := TMultipartFormData.Create;
  LTempStream := TMemoryStream.Create;
  try
  aMediaFile.Content.Position := 0;
  LTempStream.LoadFromStream(aMediaFile.Content);
  LTempStream.Position := 0;

  // Se debe especificar el propósito, para OCR es 'ocr'
  LBody.AddField('purpose', 'ocr');
  LBody.AddStream('file', LTempStream, aMediaFile.Filename, aMediaFile.MimeType);

  // FClient.ContentType := LBody.ContentType;
  LResponse := FClient.Post(LUrl, LBody);

  if LResponse.StatusCode = 200 then
  begin
  LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
  try
  Result := LResponseObj.GetValue<string>('id');
  // Guardamos el ID en el objeto MediaFile para uso futuro
  aMediaFile.IDFile := Result;
  finally
  LResponseObj.Free;
  end;
  end
  else
  begin
  FLastError := LResponse.ContentAsString;
  DoError(Format('Error al subir archivo: %d - %s', [LResponse.StatusCode, FLastError]), nil);
  end;
  finally
  LBody.Free;
  end;
  end;
}

function TAiMistralChat.UploadFile(aMediaFile: TAiMediaFile): String;
var
  LBody: TMultipartFormData;
  LTempStream: TMemoryStream;
  LResponse: IHTTPResponse;
  LUrl: string;
  LResponseObj: TJSONObject;
  LHeaders: TNetHeaders; // Añadido
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un TAiMediaFile con contenido para subir.');

  LUrl := Url + 'files'; // URL correcta
  LBody := TMultipartFormData.Create;
  LTempStream := TMemoryStream.Create;
  try
    aMediaFile.Content.Position := 0;
    LTempStream.LoadFromStream(aMediaFile.Content);
    LTempStream.Position := 0;

    // Se debe especificar el propósito, para OCR es 'ocr'
    LBody.AddField('purpose', 'ocr');
    LBody.AddStream('file', LTempStream, aMediaFile.Filename, aMediaFile.MimeType);

    // FClient.ContentType := LBody.ContentType;
    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)]; // Añadir el Header de autorización

    Var
    LResponseStream := TMemoryStream.Create;
    // LResponse := FClient.Post(LUrl, LBody, LHeaders); //Añadir Header a la petición.
    LResponse := FClient.Post(LUrl, LBody, LResponseStream, LHeaders);

    if LResponse.StatusCode = 200 then
    begin
      LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      try
        Result := LResponseObj.GetValue<string>('id');
        // Guardamos el ID en el objeto MediaFile para uso futuro
        aMediaFile.IDFile := Result;
      finally
        LResponseObj.Free;
      end;
    end
    else
    begin
      FLastError := LResponse.ContentAsString;
      DoError(Format('Error al subir archivo: %d - %s', [LResponse.StatusCode, FLastError]), nil);
    end;
  finally
    LBody.Free;
    LTempStream.Free;
  end;
end;

function TAiMistralChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: integer = 3600): String;
var
  FileId: string;
begin
  Result := '';
  // Llama a la función de subida que ya implementamos
  FileId := Self.UploadFile(aMediaFile);

  if not FileId.IsEmpty then
  begin
    // Éxito. El ID ya está en aMediaFile.IDFile gracias a UploadFile.
    // Usamos CacheName como una bandera para indicar que este archivo
    // debe ser usado como contexto persistente.
    aMediaFile.CacheName := FileId; // El valor puede ser el propio FileId para referencia.
    aMediaFile.CloudState := 'cached';
    Result := FileId;
  end;
end;

class function TAiMistralChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiMistralChat.Create(Sender);
End;

function TAiMistralChat.DeleteFile(aMediaFile: TAiMediaFile): String;
var
  LResponse: IHTTPResponse;
  LUrl: string;
  LResponseObj: TJSONObject;
begin
  Result := '';
  if not Assigned(aMediaFile) or aMediaFile.IDFile.IsEmpty then
    raise Exception.Create('El TAiMediaFile debe tener un ID de archivo válido para ser borrado.');

  LUrl := Url + 'files/' + aMediaFile.IDFile;
  LResponse := FClient.Delete(LUrl, nil);

  if LResponse.StatusCode = 200 then
  begin
    LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
    try
      // La respuesta de borrado contiene el id y un flag "deleted: true"
      if LResponseObj.GetValue<Boolean>('deleted') then
        Result := LResponseObj.GetValue<string>('id')
      else
        Result := '';
    finally
      LResponseObj.Free;
    end;
  end
  else
  begin
    FLastError := LResponse.ContentAsString;
    DoError(Format('Error al borrar archivo: %d - %s', [LResponse.StatusCode, FLastError]), nil);
  end;
end;

constructor TAiMistralChat.Create(Sender: TComponent);
begin
  inherited;

  ApiKey := '@MISTRAL_API_KEY';
  Model := 'open-mistral-7b';
  FOcrIncludeImages := True;
  Url := GlAIUrl;
end;

function TAiMistralChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  jObj, Msg, jFunc, Arg: TJSONObject;
  JVal, JVal1, jValToolCall: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
  Nom, Valor: String;
  I: integer;
begin
  Result := TAiToolsFunctions.Create;

  For JVal1 in jChoices do
  Begin
    Msg := TJSONObject(JVal1).GetValue<TJSONObject>('message');

    If Msg.TryGetValue<TJSonValue>('tool_calls', jValToolCall) and (jValToolCall is TJSonArray) and
      Msg.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
    Begin
      For JVal in JToolCalls do
      Begin
        jObj := TJSONObject(JVal);
        // If jObj.GetValue<String>('type') = 'function' then //Mistral no maneja este parámetro
        Begin
          Fun := TAiToolsFunction.Create;
          Fun.Id := jObj.GetValue<String>('id');
          // Fun.Tipo := jObj.GetValue<String>('type'); //Mistral no maneja este parámetro

          If jObj.TryGetValue<TJSONObject>('function', jFunc) then
          Begin
            Fun.Name := jFunc.GetValue<String>('name');

            Fun.Arguments := jObj.GetValue<TJSONObject>('function').GetValue<String>('arguments');
          End;

          Try
            If (Fun.Arguments <> '') and (Fun.Arguments <> '{}') then
            Begin
              Arg := TJSONObject(TJSONObject.ParseJSONValue(Fun.Arguments));
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
var
  AJSONObject, jToolChoice: TJSONObject;
  JArr, JStop: TJSonArray;
  Lista: TStringList;
  I: integer;
  Res: String;
  // --- Variables para la lógica de Document QnA ---
  ActiveFileId: string;
  SignedUrl: string;
  MessagesJson: TJSonArray;
  LastMessageObj: TJSONObject;
  PromptText: string;
  NewContentArray: TJSonArray;
  Msg: TAiChatMessage;
  MediaFile: TAiMediaFile;
  JTextObj, JDocObj: TJSONObject;
  LModel: String;
begin
  // 1. --- Inicialización de variables ---
  AJSONObject := TJSONObject.Create;
  Lista := TStringList.Create;
  ActiveFileId := '';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  try
    // 2. --- Detección de "Caché Activa" para Document QnA ---
    // Recorremos el historial de mensajes desde el más reciente al más antiguo.
    // El primer archivo que encontremos con un 'CacheName' se usará como contexto.
    for I := FMessages.Count - 1 downto 0 do
    begin
      Msg := FMessages[I];
      if Assigned(Msg.MediaFiles) then
      begin
        for MediaFile in Msg.MediaFiles do
        begin
          // 'CacheName' es la bandera que indica que este archivo es un contexto persistente.
          if not MediaFile.CacheName.IsEmpty then
          begin
            ActiveFileId := MediaFile.IDFile; // El dato clave es el ID del archivo subido.
            Break; // Salimos del bucle de MediaFiles.
          end;
        end;
      end;
      if not ActiveFileId.IsEmpty then
        Break; // Encontramos la caché, salimos del bucle de Mensajes.
    end;

    // 3. --- Construcción del Array de Mensajes ---
    // Obtenemos el JSON de mensajes estándar generado por la clase base.
    MessagesJson := GetMessages;

    // Si encontramos un archivo en caché y hay mensajes, modificamos el último mensaje
    // para inyectarle la referencia al documento.
    if (not ActiveFileId.IsEmpty) and (MessagesJson.Count > 0) then
    begin
      // Obtenemos una URL firmada y temporal para el archivo.
      SignedUrl := Self.GetSignedUrlById(ActiveFileId);
      if not SignedUrl.IsEmpty then
      begin
        // a. Obtenemos el último objeto de mensaje del array JSON.
        LastMessageObj := MessagesJson.Items[MessagesJson.Count - 1] as TJSONObject;

        // b. Extraemos el texto original (el prompt del usuario).
        var
          LContent: TJSonValue;

        if LastMessageObj.TryGetValue('content', LContent) and (LContent is TJSONString) then
          PromptText := LContent.Value
        else
          PromptText := '';

        // c. Creamos el nuevo 'content' como un array [ {type: text}, {type: document_url} ].
        NewContentArray := TJSonArray.Create;

        // c.1. Creamos el objeto para el texto
        JTextObj := TJSONObject.Create;
        JTextObj.AddPair('type', 'text');
        JTextObj.AddPair('text', PromptText);
        NewContentArray.Add(JTextObj);

        // c.2. Creamos el objeto para el documento
        JDocObj := TJSONObject.Create;
        JDocObj.AddPair('type', 'document_url');
        JDocObj.AddPair('document_url', SignedUrl);
        NewContentArray.Add(JDocObj);

        // d. Reemplazamos el 'content' antiguo con el nuevo array.
        LastMessageObj.RemovePair('content');
        LastMessageObj.AddPair('content', NewContentArray);
      end;
    end;
    AJSONObject.AddPair('messages', MessagesJson);

    // 4. --- Añadir Parámetros Estándar a la Petición ---
    AJSONObject.AddPair('model', LModel);

    if Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    begin
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
      if Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools están mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      if (Trim(Tool_choice) <> '') then
      begin
        try
          jToolChoice := TJSONObject(TJSONObject.ParseJSONValue(Tool_choice));
          if Assigned(jToolChoice) then
            AJSONObject.AddPair('tool_choice', jToolChoice);
        except
          AJSONObject.AddPair('tool_choice', Tool_choice);
        end;
      end;
    end;

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Temperature));
    if Max_tokens > 0 then
      AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));
    if Top_p > 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));
    if Seed > 0 then
      AJSONObject.AddPair('random_seed', TJSONNumber.Create(Seed));

    // 5. --- Añadir Parámetros Específicos de Mistral ---
    if not ActiveFileId.IsEmpty then
    begin
      if DocumentImageLimit > 0 then
        AJSONObject.AddPair('document_image_limit', TJSONNumber.Create(DocumentImageLimit));
      if DocumentPageLimit > 0 then
        AJSONObject.AddPair('document_page_limit', TJSONNumber.Create(DocumentPageLimit));
    end;

    if FResponse_format = tiaChatRfJson then
      AJSONObject.AddPair('response_format', TJSONObject.Create.AddPair('type', 'json_object'))
    else
      AJSONObject.AddPair('response_format', TJSONObject.Create.AddPair('type', 'text'));

    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      JStop := TJSonArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    end;

    // 6. --- Configuración de Streaming ---
    FClient.Asynchronous := Self.Asynchronous and (not Self.Tool_Active);
    AJSONObject.AddPair('stream', TJSONBool.Create(FClient.Asynchronous));

    // 7. --- Finalización y Devolución del JSON ---
    Res := AJSONObject.ToJSON;
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  finally
    AJSONObject.Free;
    Lista.Free;
  end;
end;

{ function TAiMistralChat.InitChatCompletions: String;
  Var
  AJSONObject, jToolChoice: TJSONObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res: String;
  begin

  If User = '' then
  User := 'user';

  If Model = '' then
  Model := 'open-mistral-7b';

  // Las funciones no trabajan en modo ascincrono
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSONObject.Create;
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
  jToolChoice := TJSONObject(TJSonArray.ParseJSONValue(Tool_choice));
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

  If (FResponse_format = tiaChatRfJson) then
  AJSONObject.AddPair('response_format', TJSONObject.Create.AddPair('type', 'json_object'))
  Else
  AJSONObject.AddPair('response_format', TJSONObject.Create.AddPair('type', 'text'));

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
}

function TAiMistralChat.InitChatCompletionsFim(aPrompt, aSuffix: String): String;
Var
  AJSONObject: TJSONObject;
  JStop: TJSonArray;
  Lista: TStringList;
  I: integer;
  LAsincronico: Boolean;
  LModel: String;
begin

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := 'open-mistral-7b';

  AJSONObject := TJSONObject.Create;
  Lista := TStringList.Create;

  Try
    LAsincronico := True;
    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    AJSONObject.AddPair('prompt', aPrompt);
    AJSONObject.AddPair('suffix', aSuffix);
    AJSONObject.AddPair('model', LModel);

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

    Result := UTF8ToString(AJSONObject.ToJSON);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

{ function TAiMistralChat.InternalRunOcr(aMediaFile: TAiMediaFile): String;
  var
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LHeaders: TNetHeaders;
  LResponse: IHTTPResponse;
  LJsonObject, LDocumentObj: TJSONObject;
  LUrl: string;
  LDataUri: string;
  LResponseObj: TJSONObject;
  begin
  Result := '';
  if not Assigned(aMediaFile) then
  raise Exception.Create('Se requiere un TAiMediaFile para el proceso de OCR.');

  FBusy := True;
  FLastError := '';
  LUrl := Url + 'ocr/process';
  LJsonObject := TJSONObject.Create;
  LDocumentObj := TJSONObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TMemoryStream.Create;

  try
  // 1. Construir el cuerpo de la petición JSON
  LJsonObject.AddPair('model', TJSONString.Create('mistral-ocr-latest'));
  LJsonObject.AddPair('include_image_base64', TJSONBool.Create(FOcrIncludeImages));

  // 2. Determinar el tipo y origen del documento
  aMediaFile.CloudState := 'ocr-processing'; // Actualizar estado

  if not aMediaFile.IDFile.IsEmpty then
  begin
  // Escenario A: El archivo ya fue subido a Mistral.
  // Necesitamos una URL firmada para que el servicio de OCR pueda acceder a él.
  aMediaFile.CloudState := 'ocr-getting-signed-url';
  LDataUri := GetSignedUrl(aMediaFile); // Esta función actualiza aMediaFile.CloudUri

  if LDataUri.IsEmpty then
  begin
  // GetSignedUrl ya habrá reportado el error.
  DoError('No se pudo obtener la URL firmada para el archivo con ID: ' + aMediaFile.IDFile, nil);
  aMediaFile.CloudState := 'ocr-failed: signed-url';
  Exit;
  end;

  // La API de OCR espera una `document_url`, incluso para archivos subidos.
  LDocumentObj.AddPair('type', 'document_url');
  LDocumentObj.AddPair('document_url', LDataUri);
  end
  else if LowerCase(Copy(aMediaFile.Filename, 1, 4)) = 'http' then
  begin
  // Escenario B: El archivo está en una URL pública.
  if aMediaFile.FileCategory = Tfc_Image then
  begin
  LDocumentObj.AddPair('type', 'image_url');
  LDocumentObj.AddPair('image_url', aMediaFile.Filename);
  end
  else // Asumimos PDF u otro tipo de documento
  begin
  LDocumentObj.AddPair('type', 'document_url');
  LDocumentObj.AddPair('document_url', aMediaFile.Filename);
  end;
  end
  else
  begin
  // Escenario C: El archivo es local y se enviará como Base64.
  if aMediaFile.Base64.IsEmpty then
  begin
  DoError('El archivo local no tiene contenido en Base64 para enviar a OCR.', nil);
  aMediaFile.CloudState := 'ocr-failed: no-base64';
  Exit;
  end;

  LDataUri := 'data:' + aMediaFile.MimeType + ';base64,' + aMediaFile.Base64;

  if aMediaFile.FileCategory = Tfc_Image then
  begin
  LDocumentObj.AddPair('type', 'image_url');
  LDocumentObj.AddPair('image_url', LDataUri);
  end
  else // Documentos como PDF
  begin
  LDocumentObj.AddPair('type', 'document_url');
  LDocumentObj.AddPair('document_url', LDataUri);
  end;
  end;

  LJsonObject.AddPair('document', LDocumentObj);
  LBodyStream.WriteString(LJsonObject.ToJSON);
  LBodyStream.Position := 0;

  // 3. Realizar la petición POST a la API
  LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
  FClient.ContentType := 'application/json';

  LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

  // 4. Procesar la respuesta
  LResponseStream.Position := 0;
  if LResponse.StatusCode = 200 then
  begin
  LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;

  try
  Result := ParseOcrResponse(LResponseObj, ResMsg);
  aMediaFile.CloudState := 'ocr-completed';
  finally
  LResponseObj.Free;
  end;
  end
  else
  begin
  FLastError := LResponse.ContentAsString;
  aMediaFile.CloudState := Format('ocr-failed: http-%d', [LResponse.StatusCode]);
  DoError(Format('Error en OCR: %d - %s', [LResponse.StatusCode, FLastError]), nil);
  end;

  finally
  LBodyStream.Free;
  LResponseStream.Free;
  LJsonObject.Free; // LDocumentObj es propiedad de LJsonObject
  FBusy := False;
  end;
  end;
}

{
  function TAiMistralChat.InternalRunPDFDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
  var
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LHeaders: TNetHeaders;
  LResponse: IHTTPResponse;
  LJsonObject, LDocumentObj, LBboxAnnotationFormatObj, LDocumentAnnotationFormatObj: TJSONObject;
  LUrl: string;
  LDataUri: string;
  LResponseObj: TJSONObject;
  LPagesArray: TJSonArray;
  I: integer;
  LModel: String;

  PageNumbers: TStringDynArray;
  PageNumber: string;
  PageIndex: Integer;

  begin
  Result := '';
  if not Assigned(aMediaFile) then
  raise Exception.Create('Se requiere un TAiMediaFile para el proceso de OCR.');

  FBusy := True;
  FLastError := '';
  LUrl := Url + 'ocr'; // Endpoint correcto /v1/ocr omitiendo /process como estaba.
  LJsonObject := TJSONObject.Create;
  LDocumentObj := TJSONObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TMemoryStream.Create;
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model); // Obtener el modelo base.
  LBboxAnnotationFormatObj := nil;
  LDocumentAnnotationFormatObj := nil;

  try
  // 1. Construir el cuerpo de la petición JSON
  if LModel.IsEmpty then
  begin
  // Si el modelo base esta vacio, utilizar el modelo por defecto.
  LJsonObject.AddPair('model', TJSONString.Create('mistral-ocr-latest'));
  end
  else
  LJsonObject.AddPair('model', TJSONString.Create(LModel));

  // Id del documento.
  LJsonObject.AddPair('id', TJSONString.Create(aMediaFile.IDFile));

  // 2. Determinar el tipo y origen del documento
  aMediaFile.CloudState := 'ocr-processing'; // Actualizar estado

  if not aMediaFile.IDFile.IsEmpty then
  begin
  // Escenario A: El archivo ya fue subido a Mistral.
  // Necesitamos una URL firmada para que el servicio de OCR pueda acceder a él.
  aMediaFile.CloudState := 'ocr-getting-signed-url';
  LDataUri := GetSignedUrl(aMediaFile); // Esta función actualiza aMediaFile.CloudUri

  if LDataUri.IsEmpty then
  begin
  // GetSignedUrl ya habrá reportado el error.
  DoError('No se pudo obtener la URL firmada para el archivo con ID: ' + aMediaFile.IDFile, nil);
  aMediaFile.CloudState := 'ocr-failed: signed-url';
  Exit;
  end;

  // La API de OCR espera una `document_url`, incluso para archivos subidos.
  LDocumentObj.AddPair('document_url', LDataUri);
  LDocumentObj.AddPair('document_name', aMediaFile.Filename);
  LDocumentObj.AddPair('type', TJSONString.Create('document_url'));

  end
  else if LowerCase(Copy(aMediaFile.Filename, 1, 4)) = 'http' then
  begin
  // Escenario B: El archivo está en una URL pública.
  LDocumentObj.AddPair('document_url', aMediaFile.Filename);
  LDocumentObj.AddPair('document_name', aMediaFile.Filename);
  LDocumentObj.AddPair('type', TJSONString.Create('document_url'));
  end
  else
  begin
  // Escenario C: El archivo es local y se enviará como Base64.
  // ESTE ESCENARIO NO ESTÁ SOPORTADO POR EL API.
  DoError('El archivo local no puede ser enviado como Base64 para OCR.', nil);
  aMediaFile.CloudState := 'ocr-failed: no-base64';
  Exit;
  end;

  LJsonObject.AddPair('document', LDocumentObj);

  // Parámetros Opcionales
  LJsonObject.AddPair('include_image_base64', TJSONBool.Create(FOcrIncludeImages));

  // Pages
  LPagesArray := TJSonArray.Create;
  try
  // Verifica si OcrAnnotationPages está vacío
  if not OcrAnnotationPages.IsEmpty then
  begin
  // Divide el string OcrAnnotationPages por comas para obtener los números de página
  PageNumbers := SplitString(OcrAnnotationPages, ',');

  // Itera sobre los números de página y añádelos al array JSON
  for PageNumber in PageNumbers do
  begin
  // Elimina espacios en blanco al principio y al final del número de página


  // Verifica si el número de página es un entero válido
  if TryStrToInt(Trim(PageNumber), PageIndex) then
  begin
  // Añade el número de página al array JSON
  LPagesArray.Add(PageIndex);
  end
  else
  begin
  // Manejar el caso en que el número de página no es válido (opcional)
  DoError('Número de página no válido: ' + PageNumber, nil);
  end;
  end;
  end
  else
  begin
  // Si no se especifican las páginas, añade la primera página (página 0)
  LPagesArray.Add(0);
  end;
  LJsonObject.AddPair('pages', LPagesArray);
  finally
  // LPagesArray.Free; //Es controlado por LJsonObject
  end;

  // Anotaciones
  if (Assigned(OcrDocumentAnnotationSchema) and (OcrDocumentAnnotationSchema.Text <> '')) then
  begin
  LDocumentAnnotationFormatObj := TJSONObject.Create;
  LDocumentAnnotationFormatObj.AddPair('type', 'json_schema');
  LDocumentAnnotationFormatObj.AddPair('json_schema', TJSONObject.ParseJSONValue(OcrDocumentAnnotationSchema.Text));
  // Asumo que OcrDocumentAnnotationSchema es un string Json
  LJsonObject.AddPair('document_annotation_format', LDocumentAnnotationFormatObj);

  end;

  // bbox_annotation_format
  // Solo soportan jSon_schema

  // document_annotation_format
  // Solo soportan jSon_schema

  // 3. Realizar la petición POST a la API
  ResMsg.Prompt := LJsonObject.ToString; // Guardamos la petición
  LBodyStream.WriteString(LJsonObject.ToString);
  LBodyStream.Position := 0;

  LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
  FClient.ContentType := 'application/json';

  LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

  // 4. Procesar la respuesta
  LResponseStream.Position := 0;
  ResMsg.Content := LResponse.ContentAsString;
  if LResponse.StatusCode = 200 then
  begin
  LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;

  try
  Result := ParseOcrResponse(LResponseObj, aMediaFile);

  // Mover el contenido del aMediaFile a el ResMsg
  ResMsg.Prompt := aMediaFile.Transcription;
  aMediaFile.CloudState := 'ocr-completed';
  finally
  LResponseObj.Free;
  end;
  end
  else
  begin
  FLastError := LResponse.ContentAsString;
  aMediaFile.CloudState := Format('ocr-failed: http-%d', [LResponse.StatusCode]);
  DoError(Format('Error en OCR: %d - %s', [LResponse.StatusCode, FLastError]), nil);
  end;

  finally
  LBodyStream.Free;
  LResponseStream.Free;
  LJsonObject.Free; // LDocumentObj es propiedad de LJsonObject
  if Assigned(LBboxAnnotationFormatObj) then
  LBboxAnnotationFormatObj.Free;
  if Assigned(LDocumentAnnotationFormatObj) then
  LDocumentAnnotationFormatObj.Free;
  FBusy := False;
  end;
  end;
}

function TAiMistralChat.InternalRunPDFDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LHeaders: TNetHeaders;
  LResponse: IHTTPResponse;
  LJsonObject, LDocumentObj, LBboxAnnotationFormatObj, LDocumentAnnotationFormatObj: TJSONObject;
  LUrl: string;
  LDataUri: string;
  LResponseObj: TJSONObject;
  LPagesArray: TJSonArray;
  LModel: String;

  PageNumbers: TStringDynArray;
  PageNumber: string;
  PageIndex: integer;
  FileId: String;

begin
  Result := '';
  if not Assigned(aMediaFile) then
    raise Exception.Create('Se requiere un TAiMediaFile para el proceso de OCR.');

  FBusy := True;
  FLastError := '';
  LUrl := Url + 'ocr'; // Endpoint correcto /v1/ocr omitiendo /process como estaba.
  LJsonObject := TJSONObject.Create;
  LDocumentObj := TJSONObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TMemoryStream.Create;
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model); // Obtener el modelo base.
  LBboxAnnotationFormatObj := nil;
  LDocumentAnnotationFormatObj := nil;

  try
    // 1. Construir el cuerpo de la petición JSON
    if LModel.IsEmpty then
    begin
      // Si el modelo base esta vacio, utilizar el modelo por defecto.
      LJsonObject.AddPair('model', TJSONString.Create('mistral-ocr-latest'));
    end
    else
      LJsonObject.AddPair('model', TJSONString.Create(LModel));

    // Id del documento.
    // LJsonObject.AddPair('id', TJSONString.Create(aMediaFile.IDFile));  //Se comenta  porque se maneja internamente el proceso de subida.

    // 2. Determinar el tipo y origen del documento
    aMediaFile.CloudState := 'ocr-processing'; // Actualizar estado

    // **NUEVA LÓGICA DE SUBIDA AUTOMÁTICA**
    if aMediaFile.IDFile.IsEmpty then // Si no tiene un ID es porque no se ha subido.
    begin
      // Subir el archivo.
      FileId := UploadFile(aMediaFile); // Subir el archivo al API
      if FileId.IsEmpty then
      begin
        // Reportar error y salir.
        DoError('No se pudo subir el archivo al API', nil);
        Exit;
      end;
      aMediaFile.IDFile := FileId; // Asignar el ID al objeto MediaFile.
    end;

    // A partir de aquí el archivo está subido, sea que venía así o se acaba de subir.
    LJsonObject.AddPair('id', TJSONString.Create(aMediaFile.IDFile)); // Se adiciona luego de subir el archivo al API

    // if not aMediaFile.IDFile.IsEmpty then
    begin
      // Escenario A: El archivo ya fue subido a Mistral.
      // Necesitamos una URL firmada para que el servicio de OCR pueda acceder a él.
      aMediaFile.CloudState := 'ocr-getting-signed-url';
      LDataUri := GetSignedUrl(aMediaFile); // Esta función actualiza aMediaFile.CloudUri

      if LDataUri.IsEmpty then
      begin
        // GetSignedUrl ya habrá reportado el error.
        DoError('No se pudo obtener la URL firmada para el archivo con ID: ' + aMediaFile.IDFile, nil);
        aMediaFile.CloudState := 'ocr-failed: signed-url';
        Exit;
      end;

      // La API de OCR espera una `document_url`, incluso para archivos subidos.
      LDocumentObj.AddPair('document_url', LDataUri);
      LDocumentObj.AddPair('document_name', aMediaFile.Filename);
      LDocumentObj.AddPair('type', TJSONString.Create('document_url'));

    end;
    {
      else if LowerCase(Copy(aMediaFile.Filename, 1, 4)) = 'http' then
      begin
      // Escenario B: El archivo está en una URL pública.
      LDocumentObj.AddPair('document_url', aMediaFile.Filename);
      LDocumentObj.AddPair('document_name', aMediaFile.Filename);
      LDocumentObj.AddPair('type', TJSONString.Create('document_url'));
      end
      else
      begin
      // Escenario C: El archivo es local y se enviará como Base64.
      // ESTE ESCENARIO NO ESTÁ SOPORTADO POR EL API.
      DoError('El archivo local no puede ser enviado como Base64 para OCR.', nil);
      aMediaFile.CloudState := 'ocr-failed: no-base64';
      Exit;
      end;
    }

    LJsonObject.AddPair('document', LDocumentObj);

    // Parámetros Opcionales
    LJsonObject.AddPair('include_image_base64', TJSONBool.Create(FOcrIncludeImages));

    // Pages
    LPagesArray := TJSonArray.Create;
    try
      // Verifica si OcrAnnotationPages está vacío
      if not OcrAnnotationPages.IsEmpty then
      begin
        // Divide el string OcrAnnotationPages por comas para obtener los números de página
        PageNumbers := SplitString(OcrAnnotationPages, ',');

        // Itera sobre los números de página y añádelos al array JSON
        for PageNumber in PageNumbers do
        begin
          // Elimina espacios en blanco al principio y al final del número de página

          // Verifica si el número de página es un entero válido
          if TryStrToInt(Trim(PageNumber), PageIndex) then
          begin
            // Añade el número de página al array JSON
            LPagesArray.Add(PageIndex);
          end
          else
          begin
            // Manejar el caso en que el número de página no es válido (opcional)
            DoError('Número de página no válido: ' + PageNumber, nil);
          end;
        end;
      end
      else
      begin
        // Si no se especifican las páginas, añade la primera página (página 0)
        LPagesArray.Add(0);
      end;
      LJsonObject.AddPair('pages', LPagesArray);
    finally
      // LPagesArray.Free; //Es controlado por LJsonObject
    end;

    // Anotaciones
    if (Assigned(OcrDocumentAnnotationSchema) and (OcrDocumentAnnotationSchema.Text <> '')) then
    begin
      LDocumentAnnotationFormatObj := TJSONObject.Create;
      LDocumentAnnotationFormatObj.AddPair('type', 'json_schema');
      LDocumentAnnotationFormatObj.AddPair('json_schema', TJSONObject.ParseJSONValue(OcrDocumentAnnotationSchema.Text));
      // Asumo que OcrDocumentAnnotationSchema es un string Json
      LJsonObject.AddPair('document_annotation_format', LDocumentAnnotationFormatObj);

    end;

    // bbox_annotation_format
    // Solo soportan jSon_schema

    // document_annotation_format
    // Solo soportan jSon_schema

    // 3. Realizar la petición POST a la API
    ResMsg.Prompt := LJsonObject.ToString; // Guardamos la petición
    LBodyStream.WriteString(LJsonObject.ToString);
    LBodyStream.Position := 0;

    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

{$IFDEF APIDEBUG}
    LResponseStream.SaveToFile('c:\temp\respuesta_OCR.txt'); // Para Debug
    LResponseStream.Position := 0;
{$ENDIF}
    // 4. Procesar la respuesta
    LResponseStream.Position := 0;
    ResMsg.Content := LResponse.ContentAsString;
    if LResponse.StatusCode = 200 then
    begin
      LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;

      try
        Result := ParseOcrResponse(LResponseObj, ResMsg);

        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, nil, 'assistant', '');

        // Mover el contenido del aMediaFile a el ResMsg
        FLastContent := ResMsg.Prompt;
        aMediaFile.CloudState := 'ocr-completed';
      finally
        LResponseObj.Free;
      end;
    end
    else
    begin
      FLastError := LResponse.ContentAsString;
      aMediaFile.CloudState := Format('ocr-failed: http-%d', [LResponse.StatusCode]);
      DoError(Format('Error en OCR: %d - %s', [LResponse.StatusCode, FLastError]), nil);
    end;

  finally
    LBodyStream.Free;
    LResponseStream.Free;
    LJsonObject.Free; // LDocumentObj es propiedad de LJsonObject
    if Assigned(LBboxAnnotationFormatObj) then
      LBboxAnnotationFormatObj.Free;
    if Assigned(LDocumentAnnotationFormatObj) then
      LDocumentAnnotationFormatObj.Free;
    FBusy := False;
  end;
end;

function TAiMistralChat.ParseOcrResponse(jResponse: TJSONObject; ResMsg: TAiChatMessage): string;
var
  jPages: TJSonArray;
  jPage, jImage: TJSONObject;
  pageValue, blockValue: TJSonValue;
  sb: TStringBuilder;
  I: integer;
  imageB64, imageId: string;
  embeddedImage: TAiMediaFile;
  jImagesArray: TJSonArray;
  topLeftX, topLeftY, bottomRightX, bottomRightY: integer; // Para las coordenadas
begin
  Result := '';
  if not Assigned(jResponse) or not Assigned(ResMsg) then
    Exit;

  // El objeto aMediaFile ya existe, vamos a limpiarlo por si tiene datos previos.
  // aMediaFile.Transcription := '';
  // aMediaFile.Detail := '';
  // Limpiamos los MediaFiles anidados (imágenes extraídas del OCR)
  // aMediaFile.MediaFiles.Clear;

  sb := TStringBuilder.Create;
  try
    // La respuesta principal es un objeto que contiene un array 'pages'.
    if not jResponse.TryGetValue<TJSonArray>('pages', jPages) then
    begin
      // Si la estructura no es la esperada, devolvemos el JSON crudo.
      Result := jResponse.Format;
      ResMsg.Prompt := Result; // Guardar el detalle del error.
      Exit;
    end;

    // Iteramos por cada página del documento.

    for pageValue in jPages do
    begin

      if not(pageValue is TJSONObject) then
        continue;
      jPage := pageValue as TJSONObject;

      // Cada página tiene un campo "markdown" con el Markdown ya formateado.
      // Esta es la forma más rápida y sencilla de obtener el texto.
      if jPage.TryGetValue('markdown', blockValue) then // MODIFICADO de content a markdown
      begin
        sb.AppendLine(blockValue.Value);
        sb.AppendLine('--- Page Break ---'); // Añadir un separador de página.
      end;

      // Además, podemos procesar los datos estructurados si es necesario,
      // como las imágenes incrustadas en el documento.
      jImagesArray := TJSonArray.Create; // Se crea el objeto

      if jPage.TryGetValue<TJSonArray>('images', jImagesArray) then
      begin
        for I := 0 to jImagesArray.Count - 1 do
        begin
          if not(jImagesArray.Items[I] is TJSONObject) then
            continue;
          jImage := jImagesArray.Items[I] as TJSONObject;

          // Extraer información de la imagen
          if jImage.TryGetValue<string>('id', imageId) then;
          if jImage.TryGetValue<integer>('top_left_x', topLeftX) then;
          if jImage.TryGetValue<integer>('top_left_y', topLeftY) then;
          if jImage.TryGetValue<integer>('bottom_right_x', bottomRightX) then;
          if jImage.TryGetValue<integer>('bottom_right_y', bottomRightY) then;

          // La API devuelve la imagen en Base64 si 'include_image_base64' fue true.
          if jImage.TryGetValue<string>('image_base64', imageB64) and (imageB64 <> '') then
          begin

            Var
            PosBase64 := Pos(';base64,', imageB64);
            Var
              Base64Data: String;

            if PosBase64 > 0 then
              Base64Data := Copy(imageB64, PosBase64 + Length(';base64,'), Length(imageB64))
            else
              Base64Data := imageB64; // Si no se encuentra el prefijo, usar el string
            // Creamos un nuevo TAiMediaFile para la imagen extraída.
            embeddedImage := TAiMediaFile.Create;
            try
              // Le damos un nombre descriptivo.
              embeddedImage.LoadFromBase64(imageId, Base64Data); // Se usa el id de la imagen como nombre
              // embeddedImage.MimeType := imageMimeType;

              // Añadir información adicional a la descripción de la imagen
              embeddedImage.Detail := Format('top_left_x: %d, top_left_y: %d, bottom_right_x: %d, bottom_right_y: %d',
                [topLeftX, topLeftY, bottomRightX, bottomRightY]);

              // Añadimos la imagen extraída a la lista de media files del documento principal.
              ResMsg.MediaFiles.Add(embeddedImage);
            except
              embeddedImage.Free;
              // Manejar el error si es necesario.
            end;
          end;
        end;
      end;
    end;

    // El resultado principal es el contenido Markdown concatenado de todas las páginas.
    Result := sb.ToString;

    // Guardamos el texto en las propiedades correspondientes de TAiMediaFile.
    // Usamos 'Transcription' para el texto principal.
    ResMsg.Prompt := Result;

  finally
    sb.Free;
  end;
end;

{
  function TAiMistralChat.ParseOcrResponse(jResponse: TJSONObject; aMediaFile: TAiMediaFile): string;
  var
  jPages: TJSonArray;
  jPage, jImage: TJSONObject;
  pageValue, blockValue: TJSonValue;
  sb: TStringBuilder;
  I, K: integer;
  imageB64, imageMimeType: string;
  embeddedImage: TAiMediaFile;
  begin
  Result := '';
  if not Assigned(jResponse) or not Assigned(aMediaFile) then
  Exit;

  // El objeto aMediaFile ya existe, vamos a limpiarlo por si tiene datos previos.
  aMediaFile.Transcription := '';
  aMediaFile.Detail := '';
  // Limpiamos los MediaFiles anidados (imágenes extraídas del OCR)
  aMediaFile.MediaFiles.Clear;

  sb := TStringBuilder.Create;
  try
  // La respuesta principal es un objeto que contiene un array 'pages'.
  if not jResponse.TryGetValue<TJSonArray>('pages', jPages) then
  begin
  // Si la estructura no es la esperada, devolvemos el JSON crudo.
  Result := jResponse.Format;
  aMediaFile.Detail := Result; // Guardar el detalle del error.
  Exit;
  end;

  // Iteramos por cada página del documento.
  K := 0;
  for pageValue in jPages do
  begin
  Inc(K);
  if not(pageValue is TJSONObject) then
  continue;
  jPage := pageValue as TJSONObject;

  // Cada página tiene un campo "content" con el Markdown ya formateado.
  // Esta es la forma más rápida y sencilla de obtener el texto.
  if jPage.TryGetValue('content', blockValue) then
  begin
  sb.AppendLine(blockValue.Value);
  sb.AppendLine('--- Page Break ---'); // Añadir un separador de página.
  end;

  // Además, podemos procesar los datos estructurados si es necesario,
  // como las imágenes incrustadas en el documento.
  Var
  jImagesArray := TJSonArray.Create;

  if jPage.TryGetValue<TJSonArray>('images', jImagesArray) then
  begin
  for I := 0 to jImagesArray.Count - 1 do
  begin
  if not(jImagesArray.Items[I] is TJSONObject) then
  continue;
  jImage := jImagesArray.Items[I] as TJSONObject;

  // La API devuelve la imagen en Base64 si 'include_image_base64' fue true.
  if jImage.TryGetValue<string>('base64', imageB64) and (imageB64 <> '') then
  begin
  jImage.TryGetValue<string>('mime_type', imageMimeType);
  if imageMimeType = '' then
  imageMimeType := 'image/jpeg'; // Default

  // Creamos un nuevo TAiMediaFile para la imagen extraída.
  embeddedImage := TAiMediaFile.Create;
  try
  // Le damos un nombre descriptivo.
  embeddedImage.LoadFromBase64(Format('embedded_img_p%d_i%d.jpg', [K, I]), imageB64);
  // embeddedImage.MimeType := imageMimeType;

  // Añadimos la imagen extraída a la lista de media files del documento principal.
  aMediaFile.MediaFiles.Add(embeddedImage);
  except
  embeddedImage.Free;
  // Manejar el error si es necesario.
  end;
  end;
  end;
  end;
  end;

  // El resultado principal es el contenido Markdown concatenado de todas las páginas.
  Result := sb.ToString;

  // Guardamos el texto en las propiedades correspondientes de TAiMediaFile.
  // Usamos 'Transcription' para el texto principal.
  aMediaFile.Transcription := Result;
  // Podríamos usar 'Detail' para guardar el JSON original completo para depuración.
  aMediaFile.Detail := jResponse.Format;

  finally
  sb.Free;
  end;
  end;
}

{ TAiMistralEmbeddings }

constructor TAiMistralEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := '@MISTRAL_API_KEY';
  Url := GlAIUrl;
  FDimensions := -1;
  FModel := 'mistral-embed';
end;

function TAiMistralEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: integer; aModel, aEncodingFormat: String)
  : TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  jObj: TJSONObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  jInput: TJSonArray;
begin

  Client := TNetHTTPClient.Create(Nil);
  Client.SynchronizeEvents := False;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSONObject.Create;

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

{$IFDEF APIDEBUG}
    Response.SaveToFile('c:\temp\response.txt');
{$ENDIF}
    if Res.StatusCode = 200 then
    Begin
      jObj := TJSONObject(TJSONObject.ParseJSONValue(Res.ContentAsString));
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

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiMistralChat);

end.
