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

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/



///Esta librería está en proceso,  no funciona todavía.

///https://docs.cohere.com/reference/chat

unit uMakerAi.Chat.Cohere;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,
  System.Net.HttpClientComponent, System.Threading,
  System.Net.HttpClient, System.Net.URLClient, // Necesario para IHTTPResponse en TAiErrorEvent
  uMakerAi.Core, uMakerAi.Chat, uMakerAi.ParamsRegistry,
  uMakerAi.Embeddings, uMakerAi.Embeddings.Core;

type
  // Forward declarations para claridad
  TCohereDocument = class;
  TCohereDocuments = class;

  // --- Clases de Datos para Rerank ---
  TRerankResult = class
  private
    FIndex: Integer;
    FRelevanceScore: Double;
    FDocumentText: string;
  public
    property Index: Integer read FIndex write FIndex;
    property RelevanceScore: Double read FRelevanceScore write FRelevanceScore;
    property DocumentText: string read FDocumentText write FDocumentText;
  end;

  TRerankResults = class(TObjectList<TRerankResult>);

  TRerankResponse = class
  private
    FId: string;
    FSearchUnits: Integer;
    FResults: TRerankResults;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: string read FId write FId;
    property SearchUnits: Integer read FSearchUnits write FSearchUnits;
    property Results: TRerankResults read FResults;
  end;

  { ------------------------------------------------------------------------------ }
  { TCohereChat }
  { ------------------------------------------------------------------------------ }
  TCohereChat = class(TAiChat)
  private
    FStop_sequences: TStrings;
    FDocuments: TCohereDocuments;
    FRerankModel: string; // Propiedad para el modelo de Rerank

    FStreamBuffer: string; // Buffer para acumular datos del stream SSE
    FStreamLastRole: string; // Para guardar el rol ('assistant') recibido en message-start
    FStreamResponseMsg: TAiChatMessage; // Para acumular los datos finales (usage, etc.)
    FStreamingToolCalls: TDictionary<string, TAiToolsFunction>; // Para construir tool calls en streaming
    FStreamingCitations: TAiMsgCitations; // Para construir citaciones
    FToolResultsForNextCall: TJSONArray;

    procedure ProcessStreamBuffer; // Nuevo método helper para procesar el buffer

    procedure SetStop_sequences(const Value: TStrings);
    procedure SetDocuments(const Value: TCohereDocuments);
    procedure ExecuteAndRespondToToolCalls(ToolCalls: TEnumerable<TAiToolsFunction>; ResMsg: TAiChatMessage);
  protected
    // --- Sobrescribimos los métodos clave ---
    function InitChatCompletions: String; override;
    procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); override;
    function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; override;
    procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); override;

  public
    constructor Create(Sender: TComponent); override;
    destructor Destroy; override;

    // --- Nuevo Método para Rerank ---
    function Rerank(const AQuery: string; ADocuments: TStrings; ATopN: Integer = -1): TRerankResponse;

    // --- Métodos de Fábrica ---
    class function GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function CreateInstance(Sender: TComponent): TAiChat; override;
    class function GetModels(aApiKey: String; aUrl: String = ''): TStringList; override;
    function GetModels: TStringList; override;
  published
    // Re-publicamos propiedades de TAiChat para que aparezcan en el inspector
    property Temperature;
    property Top_p; // Se mapeará a 'p'

    // --- Propiedades específicas de Cohere ---
    property Stop_sequences: TStrings read FStop_sequences write SetStop_sequences;
    property Documents: TCohereDocuments read FDocuments write SetDocuments;
    property RerankModel: string read FRerankModel write FRerankModel;
  end;

  { ------------------------------------------------------------------------------ }
  { Helper Classes for Cohere 'documents' parameter }
  { ------------------------------------------------------------------------------ }
  TCohereDocument = class
  public
    title: String;
    snippet: String;
    function ToJsonObject: TJSonObject;
    procedure Assign(Source: TCohereDocument);
  end;

  TCohereDocuments = class(TObjectList<TCohereDocument>);

  { ------------------------------------------------------------------------------ }
  { TAiCohereEmbeddings }
  { ------------------------------------------------------------------------------ }
  {
    Esta clase proporciona una implementación para generar embeddings de texto
    utilizando la API v2 de Cohere. Hereda de TAiEmbeddings y se integra
    en el framework de MakerAi.

    **Consideraciones de Implementación y Compatibilidad:**

    La arquitectura actual del framework (a través de `TAiEmbeddingsCore`) está
    diseñada para generar y devolver un único vector de embedding por llamada al
    método `CreateEmbedding`.

    Para respetar esta interfaz y asegurar la compatibilidad entre diferentes
    "drivers" (OpenAI, Mistral, etc.), esta implementación de Cohere ha sido
    adaptada:

    1.  **Procesamiento Individual:** El método `CreateEmbedding` acepta un único
    string de entrada (`aInput`). Internamente, este string se empaqueta en
    un array de un solo elemento `["texto"]` para cumplir con el formato
    requerido por la API de Cohere.

    2.  **Respuesta Individual:** De la respuesta de la API, que contiene un lote
    de embeddings, solo se extrae y se devuelve el primer vector,
    correspondiente al texto de entrada.

    **Oportunidad de Optimización:**
    La API de Cohere está altamente optimizada para el procesamiento por lotes,
    aceptando un array de hasta 96 textos en una única petición (`"texts": ["t1", "t2", ...]`).
    Esto es significativamente más eficiente que realizar múltiples llamadas
    individuales. Para aprovechar esta capacidad, se podría extender esta clase
    en el futuro con un método específico para lotes, como por ejemplo:

    `function CreateEmbeddingsBatch(const aInputs: TStrings): TAiEmbeddingList;`
  }

  TAiCohereInputType = (citSearchDocument, citSearchQuery, citClassification, citClustering);

  TAiCohereEmbeddings = class(TAiEmbeddings)
  private
    FInputType: TAiCohereInputType;
    function GetInputTypeAsString: string;
  protected
    // Este método es para procesar la respuesta específica de Cohere.
    procedure ParseCohereEmbedding(jObj: TJSonObject);
  public
    constructor Create(aOwner: TComponent); override;
    // Sobrescribimos el método principal para la implementación de Cohere.
    function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; override;
  published
    property InputType: TAiCohereInputType read FInputType write FInputType;
  end;

procedure Register;

implementation

uses
  System.StrUtils, System.IOUtils;

procedure Register;
begin
  RegisterComponents('MakerAI', [TCohereChat, TAiCohereEmbeddings]);
end;

function MapRoleToCohere(const aRole: string): string;
var
  LowerRole: string;
begin
  LowerRole := LowerCase(aRole);
  if LowerRole = 'user' then
    Result := 'user'
  else if (LowerRole = 'assistant') or (LowerRole = 'model') then // Mantenemos 'model' por compatibilidad interna
    Result := 'assistant' // <-- CAMBIO CLAVE
  else if LowerRole = 'system' then
    Result := 'system'
  else if LowerRole = 'tool' then
    Result := 'tool'
  else
    Result := 'user';
end;

{ TRerankResponse }
constructor TRerankResponse.Create;
begin
  inherited;
  FResults := TRerankResults.Create(True);
end;

destructor TRerankResponse.Destroy;
begin
  FResults.Free;
  inherited;
end;

{ TCohereDocument }
procedure TCohereDocument.Assign(Source: TCohereDocument);
begin
  if not Assigned(Source) or (Source = Self) then
    Exit;

  Self.title := Source.title;
  Self.snippet := Source.snippet;
end;

function TCohereDocument.ToJsonObject: TJSonObject;
begin
  Result := TJSonObject.Create;
  Result.AddPair('title', Self.title);
  Result.AddPair('snippet', Self.snippet);
end;

{ ------------------------------------------------------------------------------ }
{ TCohereChat }
{ ------------------------------------------------------------------------------ }

constructor TCohereChat.Create(Sender: TComponent);
begin
  inherited;
  FStop_sequences := TStringList.Create;
  FDocuments := TCohereDocuments.Create(True);

  // Valores por defecto para Chat
  Self.ApiKey := '@COHERE_API_KEY';
  Self.Url := 'https://api.cohere.ai/v2/';
  Self.Model := 'command-r';
  Self.Temperature := 0.3;
  Self.Top_p := 0.75;

  // Valores por defecto para Rerank
  Self.FRerankModel := 'rerank-english-v3.0';
  FStreamBuffer := '';
  FStreamResponseMsg := nil;
  FStreamingToolCalls := TDictionary<string, TAiToolsFunction>.Create;
  FStreamingCitations := TAiMsgCitations.Create(True);
  FToolResultsForNextCall := nil;
end;

destructor TCohereChat.Destroy;
begin
  FStop_sequences.Free;
  FDocuments.Free;
  FStreamingToolCalls.Free;
  FStreamingCitations.Free;
  FreeAndNil(FToolResultsForNextCall);
  inherited;
end;

procedure TCohereChat.ExecuteAndRespondToToolCalls(ToolCalls: TEnumerable<TAiToolsFunction>; ResMsg: TAiChatMessage);
var
  ToolCall: TAiToolsFunction;
  ToolOutputMsg: TAiChatMessage;
  jToolOutput, jCall: TJSonObject;
  jToolOutputsArray, jOutputsArray: TJSONArray;
  jOutputValue: TJSONValue;
  AskMsg: TAiChatMessage;
  TaskList: array of ITask;
  I: Integer;
  ToolCallList: TList<TAiToolsFunction>;
begin
  // El mensaje anterior (AskMsg) es el del asistente que contenía los tool_calls.
  // Ya fue añadido al historial en ParseChat.
  AskMsg := GetLastMessage;

  // 1. EJECUTAR TODAS LAS FUNCIONES SOLICITADAS
  // Convertimos el enumerable a una lista para poder usar índices con TTask
  ToolCallList := TList<TAiToolsFunction>.Create(ToolCalls);
  try
    SetLength(TaskList, ToolCallList.Count);
    for I := 0 to ToolCallList.Count - 1 do
    begin
      ToolCall := ToolCallList[I];
      ToolCall.ResMsg := ResMsg; // Pasa el mensaje de respuesta (puede ser nil)
      ToolCall.AskMsg := AskMsg; // Pasa el mensaje de petición

      TaskList[I] := TTask.Create(
        procedure
        begin
          try
            // Usamos la propiedad AiFunctions del componente para llamar a la función
            if Assigned(Self.AiFunctions) then
              Self.AiFunctions.DoCallFunction(ToolCall)
            else
              ToolCall.Response := TJSonObject.Create.AddPair('error', 'AiFunctions component not assigned.').ToString;
          except
            on E: Exception do
              // En caso de error en la ejecución, devolvemos un JSON de error.
              ToolCall.Response := TJSonObject.Create.AddPair('error', E.Message).ToString;
          end;
        end);
      TaskList[I].Start;
    end;
    TTask.WaitForAll(TaskList); // Esperamos a que todas las funciones terminen

    // 2. CONSTRUIR EL MENSAJE DE RESULTADO DE HERRAMIENTA (role: 'tool')
    // Este mensaje contendrá los resultados de todas las herramientas ejecutadas.
    jToolOutputsArray := TJSONArray.Create;
    for ToolCall in ToolCallList do
    begin
      jToolOutput := TJSonObject.Create;

      // a) Construir el objeto "call" que hace referencia a la llamada original
      jCall := TJSonObject.Create;
      jCall.AddPair('name', ToolCall.Name);
      jCall.AddPair('arguments', ToolCall.Arguments); // El string JSON de argumentos
      jToolOutput.AddPair('call', jCall);

      // b) Construir el array "outputs" que contiene el resultado de la función
      jOutputsArray := TJSONArray.Create;
      // Parseamos la respuesta de la función para asegurar que sea un objeto JSON
      jOutputValue := TJSONValue.ParseJSONValue(ToolCall.Response, True); // Parseo seguro
      if not Assigned(jOutputValue) then
      begin
        // Si la respuesta no es un JSON válido (ej. un simple string),
        // la envolvemos en un objeto JSON estándar.
        jOutputValue := TJSonObject.Create.AddPair('result', ToolCall.Response);
      end;
      jOutputsArray.Add(TJSonObject(jOutputValue));
      jToolOutput.AddPair('outputs', jOutputsArray);

      // c) Añadir el resultado de esta herramienta al array principal de resultados
      jToolOutputsArray.Add(jToolOutput);
    end;

    // 3. AÑADIR EL NUEVO MENSAJE 'tool' A LA CONVERSACIÓN
    // El 'prompt' de nuestro TAiChatMessage contendrá el string JSON del array de resultados.
    ToolOutputMsg := TAiChatMessage.Create(jToolOutputsArray.ToJSon, 'tool');
    InternalAddMessage(ToolOutputMsg);
    jToolOutputsArray.Free; // ToJSon hizo una copia, podemos liberar el original

    // 4. VOLVER A LLAMAR A RUN PARA OBTENER LA RESPUESTA FINAL
    // Llamamos a Run sin parámetros para que tome el historial de mensajes actualizado
    // (que ahora incluye el mensaje 'tool') y haga la siguiente petición a la API.
    Self.Run(nil, nil);

  finally
    ToolCallList.Free;
  end;
end;

class function TCohereChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TCohereChat.Create(Sender);
end;

class function TCohereChat.GetDriverName: string;
begin
  Result := 'Cohere';
end;

function TCohereChat.GetModels: TStringList;
begin
  Result := TCohereChat.GetModels(Self.ApiKey, Self.Url);
end;

class function TCohereChat.GetModels(aApiKey, aUrl: String): TStringList;
var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  ResponseStream: TStringStream;
  HttpResponse: IHTTPResponse;
  BaseUrl, FullUrl: string;
  LResponseJson: TJSonObject;
  LModelsArray: TJSONArray;
  LModelValue: TJSONValue;
  LModelObj: TJSonObject;
  LModelName: string;
  LUri: TURI;
begin
  Result := TStringList.Create;
  Client := TNetHTTPClient.Create(nil);
  ResponseStream := TStringStream.Create('', TEncoding.UTF8);
  try
    // 1. Construir la URL correcta (usando v1)
    if aUrl <> '' then
      BaseUrl := aUrl
    else
      BaseUrl := 'https://api.cohere.ai/';

    LUri := TURI.Create(BaseUrl);
    FullUrl := LUri.Scheme + '://' + LUri.Host + '/v1/models';
    FullUrl := FullUrl + '?endpoint=chat';

    // 3. Preparar las cabeceras correctamente
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey), TNetHeader.Create('accept', 'application/json') // Añadido para ser como cURL
      ];

    // Se pasa el parámetro AHeaders a la llamada GET.
    HttpResponse := Client.Get(FullUrl, ResponseStream, Headers);

    // 4. Procesar la respuesta (sin cambios)
    if HttpResponse.StatusCode = 200 then
    begin
      ResponseStream.Position := 0;
      LResponseJson := TJSonObject.ParseJSONValue(ResponseStream.DataString) as TJSonObject;
      if Assigned(LResponseJson) then
        try
          if LResponseJson.TryGetValue<TJSONArray>('models', LModelsArray) then
          begin
            for LModelValue in LModelsArray do
            begin
              if LModelValue is TJSonObject then
              begin
                LModelObj := LModelValue as TJSonObject;
                if LModelObj.TryGetValue<string>('name', LModelName) then
                begin
                  Result.Add(LModelName);
                end;
              end;
            end;
          end;
        finally
          LResponseJson.Free;
        end;
    end
    else
    begin
      raise Exception.CreateFmt('Error al obtener la lista de modelos de Cohere: %d - %s', [HttpResponse.StatusCode, HttpResponse.ContentAsString]);
    end;
  finally
    Client.Free;
    ResponseStream.Free;
  end;
end;

// Implementación principal del método
function TCohereChat.InitChatCompletions: String;
var
  LJsonObject: TJSonObject;
  LMessagesArray, LStopArray, LDocsArray, LToolsJsonArray: TJSONArray;
  LMessage: TAiChatMessage;
  LDoc: TCohereDocument;
  I: Integer;
  LStopList: TStringList;
  LToolsJsonString: string;
  LJsonValue, LToolOutputsValue, LToolCallsValue: TJSONValue;
  LMsgObj: TJSonObject;
  LRoleStr: string;
  LMediaFile: TAiMediaFile;
  LTextPart, LImagePart, LImageUrlObj: TJSonObject;
  LContentArray: TJSONArray;
  HasToolResults: Boolean;
begin
  LJsonObject := TJSonObject.Create;
  LStopList := TStringList.Create;
  HasToolResults := False; // Para saber si estamos en la fase 2 del tool-use
  try
    // --- 1. CONFIGURACIÓN DEL MODELO Y PARÁMETROS DE GENERACIÓN ---
    LJsonObject.AddPair('model', Self.Model);
    if Self.Asynchronous then
      LJsonObject.AddPair('stream', TJSONBool.Create(True));
    LJsonObject.AddPair('temperature', TJSONNumber.Create(Self.Temperature));
    if Self.Max_tokens > 0 then
      LJsonObject.AddPair('max_tokens', TJSONNumber.Create(Self.Max_tokens));
    if Self.Top_p > 0 then
      LJsonObject.AddPair('p', TJSONNumber.Create(Self.Top_p));
    if Self.K > 0 then
      LJsonObject.AddPair('k', TJSONNumber.Create(Self.K));
    if Self.Frequency_penalty <> 0 then
      LJsonObject.AddPair('frequency_penalty', TJSONNumber.Create(Self.Frequency_penalty));
    if Self.Presence_penalty <> 0 then
      LJsonObject.AddPair('presence_penalty', TJSONNumber.Create(Self.Presence_penalty));
    if Self.Seed > 0 then
      LJsonObject.AddPair('seed', TJSONNumber.Create(Self.Seed));

    // --- 2. CONSTRUCCIÓN DEL HISTORIAL DE MENSAJES ('messages') ---
    LMessagesArray := TJSONArray.Create;
    for LMessage in Self.Messages do
    begin
      LMsgObj := TJSonObject.Create;
      LRoleStr := MapRoleToCohere(LMessage.Role);
      LMsgObj.AddPair('role', LRoleStr);

      if (LRoleStr = 'chatbot') and (not LMessage.Tool_calls.IsEmpty) then
      begin
        // Mensaje del asistente que CONTIENE la petición de tool_calls.
        LToolCallsValue := TJSONValue.ParseJSONValue(LMessage.Tool_calls, True);
        if Assigned(LToolCallsValue) and (LToolCallsValue is TJSONArray) then
          LMsgObj.AddPair('tool_calls', LToolCallsValue)
        else
        begin
          if Assigned(LToolCallsValue) then
            LToolCallsValue.Free;
          LMsgObj.AddPair('tool_calls', TJSONArray.Create);
        end;
        // Cohere permite un 'content' con 'thinking' junto a 'tool_calls'
        if not LMessage.Prompt.IsEmpty then
          LMsgObj.AddPair('content', LMessage.Prompt);

      end
      else if (LRoleStr = 'tool') then
      begin
        // Mensaje de resultado de herramienta.
        HasToolResults := True; // Marcamos que esta petición incluye resultados.
        LToolOutputsValue := TJSONValue.ParseJSONValue(LMessage.Prompt, True);
        if Assigned(LToolOutputsValue) and (LToolOutputsValue is TJSONArray) then
          LMsgObj.AddPair('tool_outputs', LToolOutputsValue)
        else
        begin
          if Assigned(LToolOutputsValue) then
            LToolOutputsValue.Free;
          LMsgObj.AddPair('tool_outputs', TJSONArray.Create);
        end;
      end
      else if (LMessage.MediaFiles.Count > 0) and (LRoleStr = 'user') then
      begin
        // Mensaje multimodal (con imágenes).
        LContentArray := TJSONArray.Create;
        LTextPart := TJSonObject.Create;
        LTextPart.AddPair('type', 'text');
        LTextPart.AddPair('text', LMessage.Prompt);
        LContentArray.Add(LTextPart);

        for LMediaFile in LMessage.MediaFiles do
        begin
          if LMediaFile.FileCategory = Tfc_Image then
          begin
            LImagePart := TJSonObject.Create;
            LImageUrlObj := TJSonObject.Create;
            LImagePart.AddPair('type', 'image_url');
            LImageUrlObj.AddPair('url', 'data:' + LMediaFile.MimeType + ';base64,' + LMediaFile.Base64);
            LImagePart.AddPair('image_url', LImageUrlObj);
            LContentArray.Add(LImagePart);
          end;
        end;
        LMsgObj.AddPair('content', LContentArray);
      end
      else
      begin
        // Mensaje normal (user, system, o chatbot sin tool_calls).
        LMsgObj.AddPair('content', LMessage.Prompt);
      end;

      LMessagesArray.Add(LMsgObj);
    end;
    LJsonObject.AddPair('messages', LMessagesArray);

    // --- 3. INCLUSIÓN DE HERRAMIENTAS ('tools' y 'tool_choice') ---
    // No envíes la definición de herramientas si ya estás enviando resultados.
    if not HasToolResults and Tool_Active and Assigned(AiFunctions) and (AiFunctions.Functions.Count > 0) then
    begin
      LToolsJsonString := AiFunctions.GetTools(tfOpenAI);
      if not LToolsJsonString.IsEmpty then
      begin
        LJsonValue := TJSONValue.ParseJSONValue(LToolsJsonString, True);
        if Assigned(LJsonValue) and (LJsonValue is TJSONArray) then
        begin
          LToolsJsonArray := LJsonValue as TJSONArray;
          if LToolsJsonArray.Count > 0 then
          begin
            LJsonObject.AddPair('tools', LToolsJsonArray);
            if (Self.Tool_choice <> '') and (UpperCase(Self.Tool_choice) <> 'AUTO') then
              LJsonObject.AddPair('tool_choice', UpperCase(Self.Tool_choice));
          end
          else
            LToolsJsonArray.Free;
        end
        else if Assigned(LJsonValue) then
          LJsonValue.Free;
      end;
    end;

    // --- 4. INCLUSIÓN DE DOCUMENTOS PARA RAG ('documents') ---
    if Assigned(Self.FDocuments) and (Self.FDocuments.Count > 0) then
    begin
      LDocsArray := TJSONArray.Create;
      for LDoc in Self.FDocuments do
        LDocsArray.Add(LDoc.ToJsonObject);
      LJsonObject.AddPair('documents', LDocsArray);
    end;

    // --- 5. SECUENCIAS DE PARADA ('stop_sequences') ---
    LStopList.CommaText := Self.Stop;
    if Assigned(FStop_sequences) then
      LStopList.AddStrings(FStop_sequences);
    if LStopList.Count > 0 then
    begin
      LStopArray := TJSONArray.Create;
      for I := 0 to LStopList.Count - 1 do
        LStopArray.Add(LStopList[I]);
      LJsonObject.AddPair('stop_sequences', LStopArray);
    end;

    // --- 6. GENERACIÓN DEL STRING FINAL ---
    Result := LJsonObject.ToJSon;

  finally
    LJsonObject.Free;
    LStopList.Free;
  end;
end;

function TCohereChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  Headers: TNetHeaders;
  jObj: TJSonObject;
begin
  // Inicialización estándar
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  St := TStringStream.Create('', TEncoding.UTF8);

  // Construcción de la URL correcta para el endpoint de Chat NATIVO de Cohere.
  // Usamos TPath.Combine para unir la URL base ('.../v2/') con el endpoint ('chat').
  sUrl := TPath.Combine(System.SysUtils.ExcludeTrailingPathDelimiter(Self.Url), 'chat');

  try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    // Obtenemos el cuerpo JSON de nuestro método `InitChatCompletions` que ya
    // está preparado para generar el formato nativo de Cohere.
    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;

    FResponse.Clear;
    FResponse.Position := 0;

    // $IFDEF APIDEBUG
    St.SaveToFile('c:\temp\peticion.txt');
    St.Position := 0;
    // $ENDIF

    // Lanzamos la petición POST a la URL correcta.
    Res := FClient.Post(sUrl, St, FResponse, Headers);

    FResponse.Position := 0;
    FLastContent := '';

    // $IFDEF APIDEBUG
    FResponse.SaveToFile('c:\temp\respuesta.txt');
    FResponse.Position := 0;
    // $ENDIF

    // Asumimos modo síncrono para esta función.
    // La lógica de modo asíncrono se manejaría en los eventos OnReceiveData.
    if FClient.Asynchronous = False then
    begin
      if Res.StatusCode = 200 then
      begin
        jObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
        try
          FBusy := False;
          // Llamamos a nuestro método de parseo `ParseChat`, que entiende
          // la respuesta nativa de Cohere.
          ParseChat(jObj, ResMsg);
          Result := FLastContent;
        finally
          FreeAndNil(jObj);
        end;
      end
      else
      begin
        // El cuerpo de la respuesta suele contener un JSON con el detalle del error.
        raise Exception.CreateFmt('Error en Cohere Chat API: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    end
    else
    begin
      // Si por alguna razón se llama en modo asíncrono, la respuesta se gestionará en los eventos.
      Result := '';
    end;
  finally
    if not FClient.Asynchronous then
    begin
      St.Free;
      FBusy := False;
    end;
  end;
end;

procedure TCohereChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  if FClient.Asynchronous = False then
    Exit;

  // Heredamos la lógica de aborto de la clase base
  AAbort := Self.FAbort;
  if AAbort then
  begin
    FBusy := False;
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    Exit;
  end;

  // 1. Acumular los datos recibidos en nuestro buffer.
  // FResponse es el TStringStream de la clase base.
  FStreamBuffer := FStreamBuffer + FResponse.DataString;
  FResponse.Clear;
  FResponse.Position := 0;

  // 2. Procesar el buffer en busca de mensajes completos.
  ProcessStreamBuffer;
end;

procedure TCohereChat.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
var
  jUsage, jTokensNode, jCitationObj, jDocObj, jToolCallObj: TJSonObject;
  jCitations, jToolCalls, jSourcesArray, jContentArray: TJSONArray;
  LResponseText, LFinishReason, LRole: string;
  LInputTokens, LOutputTokens: Integer;
  jCitationValue, jSourceValue, jToolCallValue, docId: TJSONValue;
  LMsgCitation: TAiMsgCitation;
  LSource: TAiCitationSource;
  LFuncionesList: TList<TAiToolsFunction>;
  ToolCall: TAiToolsFunction;
  jMessage: TJSonObject;
begin
  // --- 1. INICIALIZACIÓN Y EXTRACCIÓN DE DATOS GLOBALES ---
  LResponseText := '';
  LFinishReason := '';
  LRole := 'assistant';

  jObj.TryGetValue<string>('finish_reason', LFinishReason);

  jUsage := nil;
  if jObj.TryGetValue<TJSonObject>('usage', jUsage) then
  begin
    jTokensNode := nil;
    if not jUsage.TryGetValue<TJSonObject>('tokens', jTokensNode) then
      jTokensNode := jUsage;

    if Assigned(jTokensNode) then
    begin
      LInputTokens := 0;
      LOutputTokens := 0;
      jTokensNode.TryGetValue<Integer>('input_tokens', LInputTokens);
      jTokensNode.TryGetValue<Integer>('output_tokens', LOutputTokens);
      ResMsg.Prompt_tokens := LInputTokens;
      ResMsg.Completion_tokens := LOutputTokens;
      ResMsg.Total_tokens := LInputTokens + LOutputTokens;
      Self.Prompt_tokens := Self.Prompt_tokens + LInputTokens;
      Self.Completion_tokens := Self.Completion_tokens + LOutputTokens;
      Self.Total_tokens := Self.Total_tokens + ResMsg.Total_tokens;
    end;
  end;

  jMessage := nil;
  jObj.TryGetValue<TJSonObject>('message', jMessage);

  // El texto puede ser el 'thinking' o un 'text' normal
  if not jObj.TryGetValue<string>('text', LResponseText) then
  begin
    jContentArray := nil;
    if Assigned(jMessage) and jMessage.TryGetValue<TJSONArray>('content', jContentArray) and (jContentArray.Count > 0) then
    begin
      if (jContentArray.Items[0] is TJSonObject) then
        (jContentArray.Items[0] as TJSonObject).TryGetValue<string>('thinking', LResponseText);
    end;
  end;

  if Assigned(jMessage) then
    jMessage.TryGetValue<string>('role', LRole);
  ResMsg.Role := LRole;

  // --- 2. PROCESAMIENTO DE CITACIONES (RAG) ---
  jCitations := nil;
  if jObj.TryGetValue<TJSONArray>('citations', jCitations) then
  begin
    // (Esta lógica se mantiene igual, es correcta)
    ResMsg.Citations.Clear;
    for jCitationValue in jCitations do
    begin
      // ... (código de parseo de citaciones) ...
    end;
  end;

  // --- 3. MANEJO DE LLAMADAS A HERRAMIENTAS (TOOL CALLS) ---
  jToolCalls := nil;
  if (UpperCase(LFinishReason) = 'TOOL_CALL') and Assigned(jMessage) and jMessage.TryGetValue<TJSONArray>('tool_calls', jToolCalls) then
  begin
    LFuncionesList := TList<TAiToolsFunction>.Create;
    try
      for jToolCallValue in jToolCalls do
      begin
        if not(jToolCallValue is TJSonObject) then
          Continue;

        // La estructura es { "function": { "name": ..., "arguments": "..." } }
        jToolCallObj := (jToolCallValue as TJSonObject).GetValue<TJSonObject>('function');
        if not Assigned(jToolCallObj) then
          Continue;

        ToolCall := TAiToolsFunction.Create;
        (jToolCallValue as TJSonObject).TryGetValue<string>('id', ToolCall.Id);
        jToolCallObj.TryGetValue<string>('name', ToolCall.Name);
        if not jToolCallObj.TryGetValue<string>('arguments', ToolCall.Arguments) then
          ToolCall.Arguments := '{}';

        LFuncionesList.Add(ToolCall);
      end;

      if LFuncionesList.Count > 0 then
      begin
        // a) Asignar los datos al mensaje del 'assistant' (ResMsg)
        ResMsg.Content := LResponseText; // Guardamos el 'thinking' o texto previo
        ResMsg.Prompt := LResponseText;
        ResMsg.Tool_calls := jToolCalls.ToJSon; // Guardamos el JSON de las tool_calls

        // b) Añadir este mensaje del 'assistant' al historial de la conversación.
        // Este es el mensaje que dice "voy a llamar a estas herramientas".
        InternalAddMessage(ResMsg);

        // c) Llamar a la función que ejecutará las herramientas y continuará el ciclo.
        ExecuteAndRespondToToolCalls(LFuncionesList, nil);

        // d) Salir, porque el control del chat ahora lo tiene ExecuteAndRespondToToolCalls.
        Exit;
      end;
    finally
      for ToolCall in LFuncionesList do
        ToolCall.Free;
      LFuncionesList.Free;
    end;
  end;

  // --- 4. FINALIZACIÓN NORMAL (SI NO HUBO TOOL CALLS) ---
  Self.FLastContent := LResponseText;
  ResMsg.Content := LResponseText;
  ResMsg.Prompt := LResponseText;

  FBusy := False;
  if Assigned(FOnReceiveDataEnd) then
    FOnReceiveDataEnd(Self, ResMsg, jObj, LRole, LResponseText);
end;

procedure TCohereChat.ProcessStreamBuffer;
var
  MsgEndPos: Integer;
  FullMessage, EventName, EventData, Line, TextChunk: string;
  Lines: TStringList;
  JsonData, jCitation, jToolCall: TJSonObject;
  CurrentToolCall: TAiToolsFunction;
  CurrentCitation: TAiMsgCitation;
  Index: Integer;
begin
  Lines := TStringList.Create;
  try
    repeat
      MsgEndPos := Pos(#10#10, FStreamBuffer);
      if MsgEndPos <= 0 then
        Break; // No hay un mensaje SSE completo, salimos.

      // 1. Extraer y parsear el mensaje SSE
      FullMessage := Copy(FStreamBuffer, 1, MsgEndPos + 1);
      Delete(FStreamBuffer, 1, MsgEndPos + 1);
      Lines.Text := FullMessage;
      EventName := '';
      EventData := '';

      for Line in Lines do
      begin
        if Line.StartsWith('event:') then
          EventName := Trim(Copy(Line, 7, Length(Line)))
        else if Line.StartsWith('data:') then
          EventData := Trim(Copy(Line, 6, Length(Line)));
      end;

      if EventData = '' then
        Continue; // Ignorar pings o mensajes vacíos

      JsonData := TJSonObject.ParseJSONValue(EventData) as TJSonObject;
      if not Assigned(JsonData) then
        Continue;

      try
        // --- 2. PROCESAR EL EVENTO SEGÚN SU TIPO ---
        if EventName = 'stream-start' then
        begin
          // Reiniciar el estado para un nuevo stream.
          FLastContent := '';
          FStreamingToolCalls.Clear;
          FStreamingCitations.Clear;
          FStreamLastRole := 'assistant'; // Rol por defecto
        end
        else if EventName = 'text-generation' then // Evento principal para el texto
        begin
          TextChunk := '';
          if JsonData.TryGetValue<string>('text', TextChunk) then
          begin
            if (TextChunk <> '') and Assigned(FOnReceiveDataEvent) then
            begin
              FLastContent := FLastContent + TextChunk;
              FOnReceiveDataEvent(Self, nil, JsonData, FStreamLastRole, TextChunk);
            end;
          end;
        end
        else if EventName = 'citation-generation' then
        begin
          // En streaming, las citas vienen con su texto.
          // Las creamos y añadimos directamente.
          CurrentCitation := TAiMsgCitation.Create;
          try
            JsonData.TryGetValue<string>('text', CurrentCitation.Text);
            Var
              DocIds: TJSONArray;
            if JsonData.TryGetValue<TJSONArray>('document_ids', DocIds) then
            begin
              for var docId in DocIds do
              begin
                var
                LSource := TAiCitationSource.Create;
                LSource.SourceType := cstDocument;
                LSource.DataSource.Id := docId.Value;
                CurrentCitation.Sources.Add(LSource);
              end;
            end;
            FStreamingCitations.Add(CurrentCitation);
          except
            CurrentCitation.Free;
            raise;
          end;
        end
        else if EventName = 'tool-calls-generation' then
        begin
          // Este evento envía el bloque COMPLETO de tool_calls.
          // No es un delta, así que podemos parsearlo directamente.
          Var
            jToolCalls: TJSONArray;

          if JsonData.TryGetValue<TJSONArray>('tool_calls', jToolCalls) then
          begin
            FStreamingToolCalls.Clear; // Limpiamos por si acaso
            for var jToolCallValue in jToolCalls do
            begin
              if not(jToolCallValue is TJSonObject) then
                Continue;
              jToolCall := jToolCallValue as TJSonObject;

              CurrentToolCall := TAiToolsFunction.Create;
              jToolCall.TryGetValue<string>('name', CurrentToolCall.Name);

              Var
                jParams: TJSonObject;
              if jToolCall.TryGetValue<TJSonObject>('parameters', jParams) then
                CurrentToolCall.Arguments := jParams.ToJSon
              else
                CurrentToolCall.Arguments := '{}';

              if not FStreamingToolCalls.ContainsKey(CurrentToolCall.Name) then
                FStreamingToolCalls.Add(CurrentToolCall.Name, CurrentToolCall)
              else
                CurrentToolCall.Free; // Evitar duplicados
            end;
          end;
        end
        else if EventName = 'stream-end' then
        begin
          // El stream ha terminado. Decidimos qué hacer a continuación.
          FStreamResponseMsg := TAiChatMessage.Create(FLastContent, FStreamLastRole);
          try
            // Añadir las citaciones que se hayan acumulado
            FStreamResponseMsg.Citations.Assign(FStreamingCitations);

            var
              LFinishReason: string;
            JsonData.TryGetValue<string>('finish_reason', LFinishReason);

            if (UpperCase(LFinishReason) = 'TOOL_CALLS') and (FStreamingToolCalls.Count > 0) then
            begin
              // El stream terminó porque se generaron llamadas a herramientas.
              // 1. Guardamos el mensaje actual del asistente (que puede contener el plan)
              // y le adjuntamos las herramientas que se construyeron.
              if FStreamingToolCalls.Count > 0 then
              begin
                var
                jToolCallsArr := TJSONArray.Create;
                try
                  for CurrentToolCall in FStreamingToolCalls.Values do
                  begin
                    var
                    jFinalToolCall := TJSonObject.Create;
                    jFinalToolCall.AddPair('name', CurrentToolCall.Name);
                    jFinalToolCall.AddPair('parameters', TJSONValue.ParseJSONValue(CurrentToolCall.Arguments));
                    jToolCallsArr.Add(jFinalToolCall);
                  end;
                  FStreamResponseMsg.Tool_calls := jToolCallsArr.ToJSon;
                finally
                  jToolCallsArr.Free;
                end;
              end;

              InternalAddMessage(FStreamResponseMsg);
              FStreamResponseMsg := nil; // Evitar doble liberación

              // 2. Ejecutamos las herramientas
              ExecuteAndRespondToToolCalls(FStreamingToolCalls.Values, nil);
            end
            else
            begin
              // Flujo normal: el stream terminó con una respuesta de texto completa.
              Var
                jResponse: TJSonObject;
              Var
                jUsage: TJSonObject;

              if JsonData.TryGetValue<TJSonObject>('response', jResponse) then
                if jResponse.TryGetValue<TJSonObject>('usage', jUsage) then
                begin
                  // Parsear 'usage' final
                  var
                    LInputTokens, LOutputTokens: Integer;
                  jUsage.TryGetValue<Integer>('input_tokens', LInputTokens);
                  jUsage.TryGetValue<Integer>('output_tokens', LOutputTokens);
                  FStreamResponseMsg.Prompt_tokens := LInputTokens;
                  FStreamResponseMsg.Completion_tokens := LOutputTokens;
                  FStreamResponseMsg.Total_tokens := LInputTokens + LOutputTokens;
                end;

              InternalAddMessage(FStreamResponseMsg);

              if Assigned(FOnReceiveDataEnd) then
                FOnReceiveDataEnd(Self, FStreamResponseMsg, JsonData, FStreamLastRole, FLastContent);

              FStreamResponseMsg := nil;
            end;

            FBusy := False; // Marcamos como no ocupado al final del stream
          except
            if Assigned(FStreamResponseMsg) then
              FStreamResponseMsg.Free;
            FStreamResponseMsg := nil;
            raise;
          end;
        end;
      finally
        JsonData.Free;
      end;
    until False; // El 'Break' se encarga de salir.
  finally
    Lines.Free;
  end;
end;

class procedure TCohereChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@COHERE_API_KEY';
  Params.Values['Driver'] := 'Cohere';
  Params.Values['Model'] := 'command-r';
  Params.Values['RerankModel'] := 'rerank-english-v3.0';
  Params.Values['Url'] := 'https://api.cohere.ai/v2/';
end;

function TCohereChat.Rerank(const AQuery: string; ADocuments: TStrings; ATopN: Integer): TRerankResponse;
var
  LJsonObject, LResponseJson, LMeta, LBilledUnits, LResultJson: TJSonObject;
  LDocsArray, LResultsArray: TJSONArray;
  LBodyStream, LResponseStream: TStringStream;
  LHttpResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
  Doc, LErrorMsg, LUrl: string;
  LResultItem: TJSONValue;
  LNewResult: TRerankResult;
begin
  Result := nil;
  if not Assigned(ADocuments) or (ADocuments.Count = 0) then
    raise Exception.Create('La lista de documentos para Rerank no puede estar vacía.');

  // El endpoint de Rerank es diferente al de Chat
  LUrl := TPath.Combine(System.SysUtils.ExcludeTrailingPathDelimiter(Self.Url), 'rerank');

  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('model', Self.RerankModel);
    LJsonObject.AddPair('query', AQuery);

    LDocsArray := TJSONArray.Create;
    for Doc in ADocuments do
    begin
      LDocsArray.Add(Doc);
    end;
    LJsonObject.AddPair('documents', LDocsArray);

    if ATopN > 0 then
      LJsonObject.AddPair('top_n', TJSONNumber.Create(ATopN));

    LBodyStream.WriteString(LJsonObject.ToString);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + Self.ApiKey)];

    LHttpResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

    if LHttpResponse.StatusCode = 200 then
    begin
      Result := TRerankResponse.Create;
      LResponseJson := TJSonObject.ParseJSONValue(LResponseStream.DataString) as TJSonObject;
      try
        LResponseJson.TryGetValue<string>('id', Result.FId);
        if LResponseJson.TryGetValue<TJSonObject>('meta', LMeta) then
          if LMeta.TryGetValue<TJSonObject>('billed_units', LBilledUnits) then
            LBilledUnits.TryGetValue<Integer>('search_units', Result.FSearchUnits);

        if LResponseJson.TryGetValue<TJSONArray>('results', LResultsArray) then
        begin
          for LResultItem in LResultsArray do
          begin
            LResultJson := LResultItem as TJSonObject;
            LNewResult := TRerankResult.Create;
            LResultJson.TryGetValue<Integer>('index', LNewResult.FIndex);
            LResultJson.TryGetValue<Double>('relevance_score', LNewResult.FRelevanceScore);
            if (LNewResult.FIndex >= 0) and (LNewResult.FIndex < ADocuments.Count) then
              LNewResult.FDocumentText := ADocuments[LNewResult.FIndex];
            Result.Results.Add(LNewResult);
          end;
        end;
      finally
        LResponseJson.Free;
      end;
    end
    else
    begin
      LErrorMsg := Format('Error en la API de Rerank: %d - %s', [LHttpResponse.StatusCode, LHttpResponse.ContentAsString]);
      if Assigned(OnError) then
        OnError(Self, LHttpResponse.ContentAsString, nil, LHttpResponse)
      else
        raise Exception.Create(LErrorMsg);
    end;
  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
  end;
end;

procedure TCohereChat.SetDocuments(const Value: TCohereDocuments);
var
  SourceDoc, NewDoc: TCohereDocument;
begin
  FDocuments.Clear;
  if not Assigned(Value) or (Value.Count = 0) then
    Exit;

  for SourceDoc in Value do
  begin
    NewDoc := TCohereDocument.Create;
    try
      NewDoc.Assign(SourceDoc);
      FDocuments.Add(NewDoc);
    except
      NewDoc.Free;
      raise;
    end;
  end;
end;

procedure TCohereChat.SetStop_sequences(const Value: TStrings);
begin
  FStop_sequences.Assign(Value);
end;

{ TAiCohereEmbeddings }

constructor TAiCohereEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  // Valores por defecto para Cohere
  Self.ApiKey := '@COHERE_API_KEY';
  Self.Url := 'https://api.cohere.ai/v2/';
  Self.FModel := 'embed-english-v3.0'; // Modelo por defecto de Cohere
  Self.FInputType := citSearchQuery; // Un valor por defecto común
end;

function TAiCohereEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  JTexts, JEmbeddingTypes: TJSONArray;
  Res: IHTTPResponse;
  ResponseStream: TStringStream;
  BodyStream: TStringStream;
  sUrl: String;
  LModel: string;
  LResponseJson: TJSonObject;
begin
  // Si el evento está asignado, se delega la lógica (comportamiento de la clase base)
  if Assigned(OnGetEmbedding) then
  begin
    Result := inherited CreateEmbedding(aInput, aUser, aDimensions, aModel, aEncodingFormat);
    Exit;
  end;

  Client := TNetHTTPClient.Create(Nil);
  BodyStream := TStringStream.Create('', TEncoding.UTF8);
  ResponseStream := TStringStream.Create('', TEncoding.UTF8);
  jObj := TJSonObject.Create;
  try
    sUrl := FUrl + 'embed';

    if aModel <> '' then
      LModel := aModel
    else
      LModel := FModel;

    // 1. Construir el cuerpo de la petición JSON
    jObj.AddPair('model', LModel);
    jObj.AddPair('input_type', GetInputTypeAsString);

    // La API espera un array de textos. Creamos uno con el único input.
    JTexts := TJSONArray.Create;
    JTexts.Add(aInput);
    jObj.AddPair('texts', JTexts);

    // Solicitamos solo embeddings de tipo 'float'
    JEmbeddingTypes := TJSONArray.Create;
    JEmbeddingTypes.Add('float');
    jObj.AddPair('embedding_types', JEmbeddingTypes);

    // Opcional: Cohere no usa 'dimensions' como OpenAI, sino 'output_dimension'.
    // Lo añadimos si es un valor válido para Cohere.
    if (aDimensions = 256) or (aDimensions = 512) or (aDimensions = 1024) or (aDimensions = 1536) then
    begin
      jObj.AddPair('output_dimension', aDimensions);
    end
    else if (aDimensions > 0) then
    begin
      // Opcional: Lanzar un warning o un error si el usuario especifica una dimensión
      // que no es válida para este modelo, para evitar confusiones.
      // Por ahora, simplemente lo ignoramos.
    end;

    BodyStream.WriteString(jObj.ToString);
    BodyStream.Position := 0;

    // 2. Ejecutar la llamada a la API
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, BodyStream, ResponseStream, Headers);
    ResponseStream.Position := 0;

    // 3. Procesar la respuesta
    if Res.StatusCode = 200 then
    begin
      // 1. Parsear el string de respuesta y castearlo a un TJSONObject.
      LResponseJson := TJSonObject.ParseJSONValue(ResponseStream.DataString) as TJSonObject;
      try
        // 2. Pasar el objeto JSON parseado directamente al método de parseo.
        ParseCohereEmbedding(LResponseJson);
        Result := Self.FData;
      finally
        // 3. Liberar la memoria del objeto JSON que creamos.
        LResponseJson.Free;
      end;
    end
    Else
    begin
      raise Exception.CreateFmt('Error en Cohere Embed API: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  finally
    Client.Free;
    BodyStream.Free;
    ResponseStream.Free;
    jObj.Free;
  end;
end;

function TAiCohereEmbeddings.GetInputTypeAsString: string;
begin
  case FInputType of
    citSearchDocument:
      Result := 'search_document';
    citSearchQuery:
      Result := 'search_query';
    citClassification:
      Result := 'classification';
    citClustering:
      Result := 'clustering';
  else
    Result := 'search_query'; // Default seguro
  end;
end;

procedure TAiCohereEmbeddings.ParseCohereEmbedding(jObj: TJSonObject);
var
  JEmbeddingsObj: TJSonObject;
  JFloatEmbeddingsArray: TJSONArray;
  JFirstEmbedding: TJSONArray;
  J: Integer;
  JMeta, JBilledUnits: TJSonObject;
begin
  // Limpiar datos anteriores
  SetLength(FData, 0);
  Fprompt_tokens := 0;
  Ftotal_tokens := 0;

  // Extraer el uso de tokens
  if jObj.TryGetValue<TJSonObject>('meta', JMeta) then
    if JMeta.TryGetValue<TJSonObject>('billed_units', JBilledUnits) then
      JBilledUnits.TryGetValue<Integer>('input_tokens', Fprompt_tokens);

  Ftotal_tokens := Fprompt_tokens; // En embeddings, total = prompt

  // Navegar la estructura de respuesta de Cohere
  if jObj.TryGetValue<TJSonObject>('embeddings', JEmbeddingsObj) then
  begin
    // Buscamos el array de embeddings de tipo 'float'
    if JEmbeddingsObj.TryGetValue<TJSONArray>('float', JFloatEmbeddingsArray) then
    begin
      // Como solo pedimos un texto, solo nos interesa el primer vector
      if JFloatEmbeddingsArray.Count > 0 then
      begin
        JFirstEmbedding := JFloatEmbeddingsArray.Items[0] as TJSONArray;
        if Assigned(JFirstEmbedding) then
        begin
          // Convertir el TJSONArray a nuestro TAiEmbeddingData (TArray<Double>)
          SetLength(FData, JFirstEmbedding.Count);
          for J := 0 to JFirstEmbedding.Count - 1 do
            FData[J] := JFirstEmbedding.Items[J].GetValue<Double>;
        end;
      end;
    end;
  end;
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TCohereChat);

end.

  ParseChat, ProcessStreamBuffer
