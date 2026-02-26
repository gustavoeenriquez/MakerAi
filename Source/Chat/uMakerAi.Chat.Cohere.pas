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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/



/// https://docs.cohere.com/reference/chat
/// Cohere v2 Chat API driver

unit uMakerAi.Chat.Cohere;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,
  System.Net.HttpClientComponent, System.Threading,
  System.Net.HttpClient, System.Net.URLClient, // Necesario para IHTTPResponse en TAiErrorEvent
  uMakerAi.Core, uMakerAi.Chat, uMakerAi.ParamsRegistry,
  uMakerAi.Embeddings, uMakerAi.Embeddings.Core, uMakerAi.Chat.Messages;

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
    FStreamingToolCallsByIndex: TDictionary<Integer, string>;   // Mapeo index → id para streaming
    FStreamingCitations: TAiMsgCitations; // Para construir citaciones
    FToolResultsForNextCall: TJSONArray;

    procedure ProcessStreamBuffer; // Nuevo m?todo helper para procesar el buffer

    procedure SetStop_sequences(const Value: TStrings);
    procedure SetDocuments(const Value: TCohereDocuments);
    procedure ExecuteAndRespondToToolCalls(ToolCalls: TEnumerable<TAiToolsFunction>; ResMsg: TAiChatMessage);
  protected
    // --- Sobrescribimos los m?todos clave ---
    function InitChatCompletions: String; override;
    procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); override;
    function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; override;
    procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); override;

  public
    constructor Create(Sender: TComponent); override;
    destructor Destroy; override;

    // --- Nuevo M?todo para Rerank ---
    function Rerank(const AQuery: string; ADocuments: TStrings; ATopN: Integer = -1): TRerankResponse;

    // --- M?todos de F?brica ---
    class function GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function CreateInstance(Sender: TComponent): TAiChat; override;
    class function GetModels(aApiKey: String; aUrl: String = ''): TStringList; override;
    function GetModels: TStringList; override;
  published
    // Re-publicamos propiedades de TAiChat para que aparezcan en el inspector
    property Temperature;
    property Top_p; // Se mapear? a 'p'

    // --- Propiedades espec?ficas de Cohere ---
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
    Esta clase proporciona una implementaci?n para generar embeddings de texto
    utilizando la API v2 de Cohere. Hereda de TAiEmbeddings y se integra
    en el framework de MakerAi.

    **Consideraciones de Implementaci?n y Compatibilidad:**

    La arquitectura actual del framework (a trav?s de `TAiEmbeddingsCore`) est?
    dise?ada para generar y devolver un ?nico vector de embedding por llamada al
    m?todo `CreateEmbedding`.

    Para respetar esta interfaz y asegurar la compatibilidad entre diferentes
    "drivers" (OpenAI, Mistral, etc.), esta implementaci?n de Cohere ha sido
    adaptada:

    1.  **Procesamiento Individual:** El m?todo `CreateEmbedding` acepta un ?nico
    string de entrada (`aInput`). Internamente, este string se empaqueta en
    un array de un solo elemento `["texto"]` para cumplir con el formato
    requerido por la API de Cohere.

    2.  **Respuesta Individual:** De la respuesta de la API, que contiene un lote
    de embeddings, solo se extrae y se devuelve el primer vector,
    correspondiente al texto de entrada.

    **Oportunidad de Optimizaci?n:**
    La API de Cohere est? altamente optimizada para el procesamiento por lotes,
    aceptando un array de hasta 96 textos en una ?nica petici?n (`"texts": ["t1", "t2", ...]`).
    Esto es significativamente m?s eficiente que realizar m?ltiples llamadas
    individuales. Para aprovechar esta capacidad, se podr?a extender esta clase
    en el futuro con un m?todo espec?fico para lotes, como por ejemplo:

    `function CreateEmbeddingsBatch(const aInputs: TStrings): TAiEmbeddingList;`
  }

  TAiCohereInputType = (citSearchDocument, citSearchQuery, citClassification, citClustering);

  TAiCohereEmbeddings = class(TAiEmbeddings)
  private
    FInputType: TAiCohereInputType;
    function GetInputTypeAsString: string;
  protected
    // Este m?todo es para procesar la respuesta espec?fica de Cohere.
    procedure ParseCohereEmbedding(jObj: TJSonObject);
  public
    constructor Create(aOwner: TComponent); override;
    // Sobrescribimos el m?todo principal para la implementaci?n de Cohere.
    function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; override;
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
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
  Self.Url := 'https://api.cohere.com/v2/';
  Self.Model := 'command-a-03-2025';
  Self.Temperature := 0.3;
  Self.Top_p := 0.75;

  // Valores por defecto para Rerank
  Self.FRerankModel := 'rerank-english-v3.0';
  FStreamBuffer := '';
  FStreamResponseMsg := nil;
  FStreamingToolCalls := TDictionary<string, TAiToolsFunction>.Create;
  FStreamingToolCallsByIndex := TDictionary<Integer, string>.Create;
  FStreamingCitations := TAiMsgCitations.Create(True);
  FToolResultsForNextCall := nil;
end;

destructor TCohereChat.Destroy;
begin
  FStop_sequences.Free;
  FDocuments.Free;
  FStreamingToolCalls.Free;
  FStreamingToolCallsByIndex.Free;
  FStreamingCitations.Free;
  FreeAndNil(FToolResultsForNextCall);
  inherited;
end;

procedure TCohereChat.ExecuteAndRespondToToolCalls(ToolCalls: TEnumerable<TAiToolsFunction>; ResMsg: TAiChatMessage);
var
  ToolCall: TAiToolsFunction;
  AskMsg: TAiChatMessage;
  TaskList: array of ITask;
  I: Integer;
  ToolCallList: TList<TAiToolsFunction>;
begin
  AskMsg := GetLastMessage;

  // 1. Ejecutar todas las funciones solicitadas
  ToolCallList := TList<TAiToolsFunction>.Create(ToolCalls);
  try
    SetLength(TaskList, ToolCallList.Count);
    for I := 0 to ToolCallList.Count - 1 do
    begin
      ToolCall := ToolCallList[I];
      ToolCall.ResMsg := ResMsg;
      ToolCall.AskMsg := AskMsg;

      TaskList[I] := TTask.Create(
        procedure
        begin
          try
            if Assigned(Self.AiFunctions) then
              Self.AiFunctions.DoCallFunction(ToolCall)
            else
              ToolCall.Response := '{"error":"AiFunctions component not assigned."}';
          except
            on E: Exception do
              ToolCall.Response := '{"error":"' + E.Message + '"}';
          end;
        end);
      TaskList[I].Start;
    end;
    TTask.WaitForAll(TaskList);

    // 2. Agregar un mensaje 'tool' por cada resultado (formato v2: tool_call_id + content)
    for ToolCall in ToolCallList do
      InternalAddMessage(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);

    // 3. Volver a llamar a Run para obtener la respuesta final
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
      BaseUrl := 'https://api.cohere.com/';

    LUri := TURI.Create(BaseUrl);
    FullUrl := LUri.Scheme + '://' + LUri.Host + '/v1/models';
    FullUrl := FullUrl + '?endpoint=chat';

    // 3. Preparar las cabeceras correctamente
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey), TNetHeader.Create('accept', 'application/json') // A?adido para ser como cURL
      ];

    // Se pasa el par?metro AHeaders a la llamada GET.
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

// Implementaci?n principal del m?todo
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
    // --- 1. CONFIGURACI?N DEL MODELO Y PAR?METROS DE GENERACI?N ---
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

    // --- 2. CONSTRUCCI?N DEL HISTORIAL DE MENSAJES ('messages') ---
    LMessagesArray := TJSONArray.Create;
    for LMessage in Self.Messages do
    begin
      LMsgObj := TJSonObject.Create;
      LRoleStr := MapRoleToCohere(LMessage.Role);
      LMsgObj.AddPair('role', LRoleStr);

      if (LRoleStr = 'assistant') and (not LMessage.Tool_calls.IsEmpty) then
      begin
        // Mensaje del asistente que CONTIENE la petici?n de tool_calls.
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
        // Mensaje de resultado de herramienta (v2: tool_call_id + content)
        HasToolResults := True;
        if not LMessage.ToolCallId.IsEmpty then
          LMsgObj.AddPair('tool_call_id', LMessage.ToolCallId);
        LMsgObj.AddPair('content', LMessage.Prompt);
      end
      else if (LMessage.MediaFiles.Count > 0) and (LRoleStr = 'user') then
      begin
        // Mensaje multimodal (con im?genes).
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

    // --- 3. INCLUSI?N DE HERRAMIENTAS ('tools' y 'tool_choice') ---
    // No env?es la definici?n de herramientas si ya est?s enviando resultados.
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

    // --- 4. INCLUSI?N DE DOCUMENTOS PARA RAG ('documents') ---
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

    // --- 6. RESPONSE FORMAT ---
    if FResponse_format = tiaChatRfJson then
      LJsonObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_object'));

    // --- 7. GENERACI?N DEL STRING FINAL ---
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
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  St := TStringStream.Create('', TEncoding.UTF8);

  // Construir URL del endpoint chat
  sUrl := Self.Url;
  if not sUrl.EndsWith('/') then
    sUrl := sUrl + '/';
  sUrl := sUrl + 'chat';

  FClient.Asynchronous := Self.Asynchronous;

  try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;

    FResponse.Clear;
    FResponse.Position := 0;

{$IFDEF APIDEBUG}
    St.SaveToFile('c:\temp\cohere_peticion.txt');
    St.Position := 0;
{$ENDIF}

    Res := FClient.Post(sUrl, St, FResponse, Headers);

    FResponse.Position := 0;
    FLastContent := '';

{$IFDEF APIDEBUG}
    FResponse.SaveToFile('c:\temp\cohere_respuesta.txt');
    FResponse.Position := 0;
{$ENDIF}

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
        raise Exception.CreateFmt('Error en Cohere Chat API: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end
    else
      Result := '';
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

  // Heredamos la l?gica de aborto de la clase base
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
  jUsage, jTokensNode, jToolCallObj, jFuncObj: TJSonObject;
  jToolCalls, jContentArray, jCitations, jSourcesArray: TJSONArray;
  LResponseText, LFinishReason, LRole: string;
  LInputTokens, LOutputTokens: Integer;
  jToolCallValue, jContentValue, jCitationValue, jSourceValue: TJSONValue;
  LFuncionesList: TList<TAiToolsFunction>;
  ToolCall: TAiToolsFunction;
  jMessage: TJSonObject;
  LMsgCitation: TAiMsgCitation;
  LSource: TAiCitationSource;
  LContentType, LSourceType: string;
begin
  // --- 1. Datos globales ---
  LResponseText := '';
  LFinishReason := '';
  LRole := 'assistant';

  jObj.TryGetValue<string>('finish_reason', LFinishReason);

  // --- Usage (v2: usage.tokens.input_tokens / output_tokens) ---
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

  // --- Extraer message ---
  jMessage := nil;
  jObj.TryGetValue<TJSonObject>('message', jMessage);

  // --- Extraer texto de message.content[] (v2: array de {type, text}) ---
  if Assigned(jMessage) then
  begin
    jMessage.TryGetValue<string>('role', LRole);
    jContentArray := nil;
    if jMessage.TryGetValue<TJSONArray>('content', jContentArray) then
    begin
      for jContentValue in jContentArray do
      begin
        if not (jContentValue is TJSonObject) then
          Continue;
        LContentType := '';
        (jContentValue as TJSonObject).TryGetValue<string>('type', LContentType);
        if LContentType = 'text' then
        begin
          var LText: string;
          if (jContentValue as TJSonObject).TryGetValue<string>('text', LText) then
            LResponseText := LResponseText + LText;
        end;
      end;
    end;
  end;
  ResMsg.Role := LRole;

  // --- 2. Citations (v2: message.citations[]) ---
  jCitations := nil;
  if Assigned(jMessage) and jMessage.TryGetValue<TJSONArray>('citations', jCitations) then
  begin
    ResMsg.Citations.Clear;
    for jCitationValue in jCitations do
    begin
      if not (jCitationValue is TJSonObject) then
        Continue;
      LMsgCitation := TAiMsgCitation.Create;
      try
        (jCitationValue as TJSonObject).TryGetValue<string>('text', LMsgCitation.Text);
        (jCitationValue as TJSonObject).TryGetValue<Integer>('start', LMsgCitation.StartIndex);
        (jCitationValue as TJSonObject).TryGetValue<Integer>('end', LMsgCitation.EndIndex);

        jSourcesArray := nil;
        if (jCitationValue as TJSonObject).TryGetValue<TJSONArray>('sources', jSourcesArray) then
        begin
          for jSourceValue in jSourcesArray do
          begin
            if not (jSourceValue is TJSonObject) then
              Continue;
            LSource := TAiCitationSource.Create;
            LSourceType := '';
            (jSourceValue as TJSonObject).TryGetValue<string>('type', LSourceType);
            if LSourceType = 'document' then
              LSource.SourceType := cstDocument
            else
              LSource.SourceType := cstDocument;
            (jSourceValue as TJSonObject).TryGetValue<string>('id', LSource.DataSource.Id);
            LMsgCitation.Sources.Add(LSource);
          end;
        end;
        ResMsg.Citations.Add(LMsgCitation);
      except
        LMsgCitation.Free;
        raise;
      end;
    end;
  end;

  // --- 3. Tool Calls (v2: message.tool_calls[]) ---
  jToolCalls := nil;
  if (UpperCase(LFinishReason) = 'TOOL_CALL') and Assigned(jMessage) and
     jMessage.TryGetValue<TJSONArray>('tool_calls', jToolCalls) then
  begin
    LFuncionesList := TList<TAiToolsFunction>.Create;
    try
      for jToolCallValue in jToolCalls do
      begin
        if not (jToolCallValue is TJSonObject) then
          Continue;

        jToolCallObj := jToolCallValue as TJSonObject;
        // v2 format: { "id": "...", "type": "function", "function": { "name": "...", "arguments": "..." } }
        jFuncObj := nil;
        if not jToolCallObj.TryGetValue<TJSonObject>('function', jFuncObj) then
          Continue;

        ToolCall := TAiToolsFunction.Create;
        jToolCallObj.TryGetValue<string>('id', ToolCall.Id);
        jFuncObj.TryGetValue<string>('name', ToolCall.Name);
        if not jFuncObj.TryGetValue<string>('arguments', ToolCall.Arguments) then
          ToolCall.Arguments := '{}';

        LFuncionesList.Add(ToolCall);
      end;

      if LFuncionesList.Count > 0 then
      begin
        ResMsg.Content := LResponseText;
        ResMsg.Prompt := LResponseText;
        ResMsg.Tool_calls := jToolCalls.ToJSon;

        InternalAddMessage(ResMsg);
        ExecuteAndRespondToToolCalls(LFuncionesList, nil);
        Exit;
      end;
    finally
      for ToolCall in LFuncionesList do
        ToolCall.Free;
      LFuncionesList.Free;
    end;
  end;

  // --- 4. Respuesta normal ---
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
  JsonData, jDelta, jDeltaMessage, jToolCall, jFuncObj: TJSonObject;
  CurrentToolCall: TAiToolsFunction;
  CurrentCitation: TAiMsgCitation;
  LSource: TAiCitationSource;
begin
  Lines := TStringList.Create;
  try
    repeat
      MsgEndPos := Pos(#10#10, FStreamBuffer);
      if MsgEndPos <= 0 then
        Break;

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
        Continue;

      JsonData := TJSonObject.ParseJSONValue(EventData) as TJSonObject;
      if not Assigned(JsonData) then
        Continue;

      try
        // Cohere v2 usa 'type' en el JSON, no 'event:' SSE header en todos los casos
        // Pero el tipo viene en el campo 'type' del JSON data
        var LType: string;
        if not JsonData.TryGetValue<string>('type', LType) then
          LType := EventName; // Fallback al header SSE

        if LType = 'message-start' then
        begin
          FLastContent := '';
          FStreamingToolCalls.Clear;
          FStreamingToolCallsByIndex.Clear;
          FStreamingCitations.Clear;
          FStreamLastRole := 'assistant';
        end
        else if LType = 'content-delta' then
        begin
          // v2: delta.message.content.text
          TextChunk := '';
          jDelta := nil;
          if JsonData.TryGetValue<TJSonObject>('delta', jDelta) then
          begin
            jDeltaMessage := nil;
            if jDelta.TryGetValue<TJSonObject>('message', jDeltaMessage) then
            begin
              var jContent: TJSonObject;
              if jDeltaMessage.TryGetValue<TJSonObject>('content', jContent) then
                jContent.TryGetValue<string>('text', TextChunk);
            end;
          end;

          if (TextChunk <> '') and Assigned(FOnReceiveDataEvent) then
          begin
            FLastContent := FLastContent + TextChunk;
            FOnReceiveDataEvent(Self, nil, JsonData, FStreamLastRole, TextChunk);
          end;
        end
        else if LType = 'tool-plan-delta' then
        begin
          // Tool plan text (reasoning del modelo antes de tool calls)
          TextChunk := '';
          jDelta := nil;
          if JsonData.TryGetValue<TJSonObject>('delta', jDelta) then
            jDelta.TryGetValue<string>('message.tool_plan', TextChunk);

          if (TextChunk <> '') and Assigned(FOnReceiveDataEvent) then
          begin
            FLastContent := FLastContent + TextChunk;
            FOnReceiveDataEvent(Self, nil, JsonData, FStreamLastRole, TextChunk);
          end;
        end
        else if LType = 'tool-call-start' then
        begin
          // v2: tool_calls es un objeto (no array); index en nivel raíz identifica la posición
          jDelta := nil;
          if JsonData.TryGetValue<TJSonObject>('delta', jDelta) then
          begin
            jToolCall := nil;
            if jDelta.TryGetValue<TJSonObject>('message', jToolCall) then
            begin
              var jTC: TJSONObject;
              if jToolCall.TryGetValue<TJSONObject>('tool_calls', jTC) then
              begin
                CurrentToolCall := TAiToolsFunction.Create;
                jTC.TryGetValue<string>('id', CurrentToolCall.Id);
                jFuncObj := nil;
                if jTC.TryGetValue<TJSonObject>('function', jFuncObj) then
                  jFuncObj.TryGetValue<string>('name', CurrentToolCall.Name);
                CurrentToolCall.Arguments := '';

                if not FStreamingToolCalls.ContainsKey(CurrentToolCall.Id) then
                begin
                  FStreamingToolCalls.Add(CurrentToolCall.Id, CurrentToolCall);
                  // Registrar mapeo index → id para poder acumular args en tool-call-delta
                  var LIndex: Integer;
                  if JsonData.TryGetValue<Integer>('index', LIndex) then
                    FStreamingToolCallsByIndex.AddOrSetValue(LIndex, CurrentToolCall.Id);
                end
                else
                  CurrentToolCall.Free;
              end;
            end;
          end;
        end
        else if LType = 'tool-call-delta' then
        begin
          // v2: tool_calls es un objeto (no array) con solo function.arguments; sin id
          // Usamos index del nivel raíz para localizar el tool call iniciado previamente
          jDelta := nil;
          if JsonData.TryGetValue<TJSonObject>('delta', jDelta) then
          begin
            jToolCall := nil;
            if jDelta.TryGetValue<TJSonObject>('message', jToolCall) then
            begin
              var jTC: TJSONObject;
              if jToolCall.TryGetValue<TJSONObject>('tool_calls', jTC) then
              begin
                var LId: string;
                var LIndex: Integer;
                if JsonData.TryGetValue<Integer>('index', LIndex) then
                  FStreamingToolCallsByIndex.TryGetValue(LIndex, LId);
                jFuncObj := nil;
                if jTC.TryGetValue<TJSonObject>('function', jFuncObj) then
                begin
                  var LArgChunk: string;
                  if jFuncObj.TryGetValue<string>('arguments', LArgChunk) then
                  begin
                    if (LId <> '') and FStreamingToolCalls.ContainsKey(LId) then
                      FStreamingToolCalls[LId].Arguments := FStreamingToolCalls[LId].Arguments + LArgChunk;
                  end;
                end;
              end;
            end;
          end;
        end
        else if LType = 'citation-start' then
        begin
          // v2: citations es un objeto (no array); index en nivel raíz identifica la posición
          CurrentCitation := TAiMsgCitation.Create;
          try
            jDelta := nil;
            if JsonData.TryGetValue<TJSonObject>('delta', jDelta) then
            begin
              var jMsg: TJSonObject;
              if jDelta.TryGetValue<TJSonObject>('message', jMsg) then
              begin
                var jCit: TJSonObject;
                if jMsg.TryGetValue<TJSonObject>('citations', jCit) then
                begin
                  jCit.TryGetValue<string>('text', CurrentCitation.Text);
                  jCit.TryGetValue<Integer>('start', CurrentCitation.StartIndex);
                  jCit.TryGetValue<Integer>('end', CurrentCitation.EndIndex);

                  var jSources: TJSONArray;
                  if jCit.TryGetValue<TJSONArray>('sources', jSources) then
                  begin
                    for var jSrc in jSources do
                    begin
                      if not (jSrc is TJSonObject) then Continue;
                      LSource := TAiCitationSource.Create;
                      LSource.SourceType := cstDocument;
                      (jSrc as TJSonObject).TryGetValue<string>('id', LSource.DataSource.Id);
                      CurrentCitation.Sources.Add(LSource);
                    end;
                  end;
                end;
              end;
            end;
            FStreamingCitations.Add(CurrentCitation);
          except
            CurrentCitation.Free;
            raise;
          end;
        end
        else if LType = 'message-end' then
        begin
          FStreamResponseMsg := TAiChatMessage.Create(FLastContent, FStreamLastRole);
          try
            FStreamResponseMsg.Citations.Assign(FStreamingCitations);

            // Usage del evento final
            jDelta := nil;
            if JsonData.TryGetValue<TJSonObject>('delta', jDelta) then
            begin
              var jUsage: TJSonObject;
              if jDelta.TryGetValue<TJSonObject>('usage', jUsage) then
              begin
                var jTokens: TJSonObject;
                if jUsage.TryGetValue<TJSonObject>('tokens', jTokens) then
                begin
                  var LIn, LOut: Integer;
                  LIn := 0; LOut := 0;
                  jTokens.TryGetValue<Integer>('input_tokens', LIn);
                  jTokens.TryGetValue<Integer>('output_tokens', LOut);
                  FStreamResponseMsg.Prompt_tokens := LIn;
                  FStreamResponseMsg.Completion_tokens := LOut;
                  FStreamResponseMsg.Total_tokens := LIn + LOut;
                  Self.Prompt_tokens := Self.Prompt_tokens + LIn;
                  Self.Completion_tokens := Self.Completion_tokens + LOut;
                  Self.Total_tokens := Self.Total_tokens + FStreamResponseMsg.Total_tokens;
                end;
              end;

              var LFinishReason: string;
              jDelta.TryGetValue<string>('finish_reason', LFinishReason);

              if (UpperCase(LFinishReason) = 'TOOL_CALL') and (FStreamingToolCalls.Count > 0) then
              begin
                // Construir tool_calls JSON para el historial
                var jToolCallsArr := TJSONArray.Create;
                try
                  for CurrentToolCall in FStreamingToolCalls.Values do
                  begin
                    var jTC := TJSonObject.Create;
                    jTC.AddPair('id', CurrentToolCall.Id);
                    jTC.AddPair('type', 'function');
                    var jFunc := TJSonObject.Create;
                    jFunc.AddPair('name', CurrentToolCall.Name);
                    var LArgs := CurrentToolCall.Arguments;
                    if LArgs.IsEmpty then LArgs := '{}';
                    jFunc.AddPair('arguments', LArgs);
                    jTC.AddPair('function', jFunc);
                    jToolCallsArr.Add(jTC);
                  end;
                  FStreamResponseMsg.Tool_calls := jToolCallsArr.ToJSon;
                finally
                  jToolCallsArr.Free;
                end;

                InternalAddMessage(FStreamResponseMsg);
                FStreamResponseMsg := nil;

                ExecuteAndRespondToToolCalls(FStreamingToolCalls.Values, nil);
              end
              else
              begin
                InternalAddMessage(FStreamResponseMsg);

                if Assigned(FOnReceiveDataEnd) then
                  FOnReceiveDataEnd(Self, FStreamResponseMsg, JsonData, FStreamLastRole, FLastContent);

                FStreamResponseMsg := nil;
              end;
            end;

            FBusy := False;
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
    until False;
  finally
    Lines.Free;
  end;
end;

class procedure TCohereChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@COHERE_API_KEY';
  Params.Values['Driver'] := 'Cohere';
  Params.Values['Model'] := 'command-a-03-2025';
  Params.Values['RerankModel'] := 'rerank-english-v3.0';
  Params.Values['Url'] := 'https://api.cohere.com/v2/';
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
    raise Exception.Create('La lista de documentos para Rerank no puede estar vac?a.');

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
  Self.Url := 'https://api.cohere.com/v2/';
  Self.FModel := 'embed-english-v3.0'; // Modelo por defecto de Cohere
  Self.FInputType := citSearchQuery; // Un valor por defecto com?n
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
  // Si el evento est? asignado, se delega la l?gica (comportamiento de la clase base)
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

    // 1. Construir el cuerpo de la petici?n JSON
    jObj.AddPair('model', LModel);
    jObj.AddPair('input_type', GetInputTypeAsString);

    // La API espera un array de textos. Creamos uno con el ?nico input.
    JTexts := TJSONArray.Create;
    JTexts.Add(aInput);
    jObj.AddPair('texts', JTexts);

    // Solicitamos solo embeddings de tipo 'float'
    JEmbeddingTypes := TJSONArray.Create;
    JEmbeddingTypes.Add('float');
    jObj.AddPair('embedding_types', JEmbeddingTypes);

    // Opcional: Cohere no usa 'dimensions' como OpenAI, sino 'output_dimension'.
    // Lo a?adimos si es un valor v?lido para Cohere.
    if (aDimensions = 256) or (aDimensions = 512) or (aDimensions = 1024) or (aDimensions = 1536) then
    begin
      jObj.AddPair('output_dimension', aDimensions);
    end
    else if (aDimensions > 0) then
    begin
      // Opcional: Lanzar un warning o un error si el usuario especifica una dimensi?n
      // que no es v?lida para este modelo, para evitar confusiones.
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
        // 2. Pasar el objeto JSON parseado directamente al m?todo de parseo.
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

{ TAiCohereEmbeddings - Factory class methods }

class function TAiCohereEmbeddings.GetDriverName: string;
begin
  Result := 'Cohere';
end;

class function TAiCohereEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiCohereEmbeddings.Create(aOwner);
end;

class procedure TAiCohereEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@COHERE_API_KEY';
  Params.Values['Url'] := 'https://api.cohere.com/v2/';
  Params.Values['Model'] := 'embed-english-v3.0';
  Params.Values['Dimensions'] := '1024';
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TCohereChat);
TAiEmbeddingFactory.Instance.RegisterDriver(TAiCohereEmbeddings);

end.
