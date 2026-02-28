// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Driver para Cohere v2 Chat API (https://api.cohere.com/v2/)
//
// Diferencias clave respecto a OpenAI:
//   - Endpoint: /v2/chat (no /v1/chat/completions)
//   - top_p → 'p'; soporta 'k' para top-k sampling
//   - Tokens en usage.tokens.input_tokens / output_tokens
//   - Respuesta en message.content[] (array {type, text})
//   - Citations en message.citations[]
//   - Tool calls en message.tool_calls[] con finish_reason = 'TOOL_CALL'
//   - SSE streaming con bloques event:+data:+\n\n (no lineas data: sueltas)
//   - Rerank: POST /v2/rerank con modelo independiente
//
// API key: variable de entorno COHERE_API_KEY (o '@COHERE_API_KEY')

unit uMakerAi.Chat.Cohere;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fpjson, jsonparser,
  fphttpclient, opensslsockets,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Tools.Functions,
  UMakerAi.ParamsRegistry;

type
  // ---------------------------------------------------------------------------
  //  Rerank — clases de datos
  // ---------------------------------------------------------------------------
  TRerankResult = class
  private
    FIndex          : Integer;
    FRelevanceScore : Double;
    FDocumentText   : string;
  public
    property Index          : Integer read FIndex          write FIndex;
    property RelevanceScore : Double  read FRelevanceScore write FRelevanceScore;
    property DocumentText   : string  read FDocumentText   write FDocumentText;
  end;

  TRerankResults = specialize TObjectList<TRerankResult>;

  TRerankResponse = class
  private
    FId          : string;
    FSearchUnits : Integer;
    FResults     : TRerankResults;
  public
    constructor Create;
    destructor  Destroy; override;
    property Id          : string         read FId          write FId;
    property SearchUnits : Integer        read FSearchUnits write FSearchUnits;
    property Results     : TRerankResults read FResults;
  end;

  // ---------------------------------------------------------------------------
  //  TCohereDocument — soporte para RAG 'documents'
  // ---------------------------------------------------------------------------
  TCohereDocument = class
  public
    title   : string;
    snippet : string;
    function  ToJsonObject: TJSONObject;
    procedure Assign(Source: TCohereDocument);
  end;

  TCohereDocuments = specialize TObjectList<TCohereDocument>;

  // ---------------------------------------------------------------------------
  //  TCohereChat — driver principal Cohere v2
  // ---------------------------------------------------------------------------
  TCohereChat = class(TAiChat)
  private
    FStop_sequences : TStrings;   // creado como TStringList en constructor
    FDocuments      : TCohereDocuments;
    FRerankModel    : string;

    // Estado SSE streaming (bloques event:+data:+\n\n)
    FStreamEventName         : string;
    FStreamEventData         : string;
    FStreamLastRole          : string;
    FStreamingToolCalls      : specialize TDictionary<string, TAiToolsFunction>;
    FStreamingToolCallsByIdx : specialize TDictionary<Integer, string>;
    FStreamingCitations      : TAiMsgCitations;

    procedure ProcessEventBlock;
    procedure ExecuteAndRespondToToolCalls;
    procedure SetStop_sequences(const Value: TStrings);
    procedure SetDocuments(const Value: TCohereDocuments);

  protected
    function  InitChatCompletions: string; override;
    procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); override;
    function  InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string; override;
    procedure ProcessSSELine(const ALine: string); override;

  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    function Rerank(const AQuery: string; ADocuments: TStrings;
        ATopN: Integer = -1): TRerankResponse;

    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
    class function  GetModels(aApiKey, aUrl: string): TStringList; override;
    function GetModels: TStringList; override;

  published
    property Temperature;
    property Top_p;
    property Stop_sequences : TStrings         read FStop_sequences write SetStop_sequences;
    property Documents      : TCohereDocuments  read FDocuments      write SetDocuments;
    property RerankModel    : string            read FRerankModel    write FRerankModel;
  end;

implementation

const
  GlCohereUrl = 'https://api.cohere.com/v2/';

// ---------------------------------------------------------------------------
//  Helper: mapeo de roles al formato Cohere
// ---------------------------------------------------------------------------
function MapRoleToCohere(const aRole: string): string;
var
  LowerRole: string;
begin
  LowerRole := LowerCase(aRole);
  if LowerRole = 'user' then
    Result := 'user'
  else if (LowerRole = 'assistant') or (LowerRole = 'model') then
    Result := 'assistant'
  else if LowerRole = 'system' then
    Result := 'system'
  else if LowerRole = 'tool' then
    Result := 'tool'
  else
    Result := 'user';
end;

// ===========================================================================
//  TRerankResponse
// ===========================================================================

constructor TRerankResponse.Create;
begin
  inherited Create;
  FResults := TRerankResults.Create(True);
end;

destructor TRerankResponse.Destroy;
begin
  FResults.Free;
  inherited Destroy;
end;

// ===========================================================================
//  TCohereDocument
// ===========================================================================

function TCohereDocument.ToJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('title',   title);
  Result.Add('snippet', snippet);
end;

procedure TCohereDocument.Assign(Source: TCohereDocument);
begin
  if not Assigned(Source) or (Source = Self) then
    Exit;
  Self.title   := Source.title;
  Self.snippet := Source.snippet;
end;

// ===========================================================================
//  TCohereChat — ciclo de vida
// ===========================================================================

constructor TCohereChat.Create(Sender: TComponent);
begin
  inherited Create(Sender);

  FStop_sequences := TStringList.Create;
  FDocuments      := TCohereDocuments.Create(True);
  FRerankModel    := 'rerank-english-v3.0';

  ApiKey      := '@COHERE_API_KEY';
  Url         := GlCohereUrl;
  Model       := 'command-a-03-2025';
  Temperature := 0.3;
  Top_p       := 0.75;

  FStreamEventName         := '';
  FStreamEventData         := '';
  FStreamLastRole          := 'assistant';
  FStreamingToolCalls      := specialize TDictionary<string, TAiToolsFunction>.Create;
  FStreamingToolCallsByIdx := specialize TDictionary<Integer, string>.Create;
  FStreamingCitations      := TAiMsgCitations.Create(True);
end;

destructor TCohereChat.Destroy;
var
  ToolCall : TAiToolsFunction;
  ToolId   : string;
begin
  FStop_sequences.Free;
  FDocuments.Free;
  FStreamingCitations.Free;
  FStreamingToolCallsByIdx.Free;
  // Liberar manualmente los tool calls pendientes (TDictionary no es owning)
  for ToolId in FStreamingToolCalls.Keys do
  begin
    ToolCall := FStreamingToolCalls[ToolId];
    ToolCall.Free;
  end;
  FStreamingToolCalls.Free;
  inherited Destroy;
end;

// ===========================================================================
//  Factory
// ===========================================================================

class function TCohereChat.GetDriverName: string;
begin
  Result := 'Cohere';
end;

class procedure TCohereChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@COHERE_API_KEY');
  Params.Add('Driver=Cohere');
  Params.Add('Model=command-a-03-2025');
  Params.Add('RerankModel=rerank-english-v3.0');
  Params.Add('URL=' + GlCohereUrl);
end;

class function TCohereChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TCohereChat.Create(Sender);
end;

function TCohereChat.GetModels: TStringList;
begin
  Result := TCohereChat.GetModels(Self.ApiKey, Self.Url);
end;

class function TCohereChat.GetModels(aApiKey, aUrl: string): TStringList;
var
  Client    : TFPHTTPClient;
  RespStream: TStringStream;
  jObj      : TJSONObject;
  jArr      : TJSONArray;
  jModel    : TJSONObject;
  I         : Integer;
  LUrl, LName: string;
begin
  Result := TStringList.Create;
  LUrl   := 'https://api.cohere.com/v1/models?endpoint=chat';

  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + aApiKey);
    Client.AddHeader('Accept', 'application/json');
    try
      Client.Get(LUrl, RespStream);
    except
      // Devolver lista vacia en caso de error
      Exit;
    end;

    if Client.ResponseStatusCode = 200 then
    begin
      jObj := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jObj) then
      try
        if JTryGetArr(jObj, 'models', jArr) then
          for I := 0 to jArr.Count - 1 do
          begin
            if not (jArr.Items[I] is TJSONObject) then Continue;
            jModel := TJSONObject(jArr.Items[I]);
            if JTryGetStr(jModel, 'name', LName) then
              Result.Add(LName);
          end;
      finally
        jObj.Free;
      end;
    end;
  finally
    Client.Free;
    RespStream.Free;
  end;
end;

// ===========================================================================
//  InitChatCompletions — construye el JSON del request Cohere v2
// ===========================================================================

function TCohereChat.InitChatCompletions: string;
var
  LJsonObject    : TJSONObject;
  LMessagesArray : TJSONArray;
  LStopArray     : TJSONArray;
  LDocsArray     : TJSONArray;
  LToolsJsonArray: TJSONArray;
  LMsgObj        : TJSONObject;
  LContentArray  : TJSONArray;
  LTextPart      : TJSONObject;
  LImagePart     : TJSONObject;
  LImageUrlObj   : TJSONObject;
  LFormatObj     : TJSONObject;
  LMsg           : TAiChatMessage;
  LDoc           : TCohereDocument;
  LMediaFile     : TAiMediaFile;
  LStopList      : TStringList;
  LToolsStr      : string;
  LToolsJson     : TJSONData;
  LToolCallsJson : TJSONData;
  LRoleStr       : string;
  HasToolResults : Boolean;
  I              : Integer;
  MI             : Integer;
begin
  LJsonObject := TJSONObject.Create;
  LStopList   := TStringList.Create;
  HasToolResults := False;
  try
    // 1. Modelo y parametros de generacion
    LJsonObject.Add('model', Model);
    if Asynchronous then
      LJsonObject.Add('stream', TJSONBoolean.Create(True));
    LJsonObject.Add('temperature', TJSONFloatNumber.Create(
        Trunc(Temperature * 100) / 100));
    if Max_tokens > 0 then
      LJsonObject.Add('max_tokens', TJSONIntegerNumber.Create(Max_tokens));
    if Top_p > 0 then
      LJsonObject.Add('p', TJSONFloatNumber.Create(Top_p));   // Cohere: 'p' no 'top_p'
    if K > 0 then
      LJsonObject.Add('k', TJSONIntegerNumber.Create(K));
    if Frequency_penalty <> 0 then
      LJsonObject.Add('frequency_penalty',
          TJSONFloatNumber.Create(Frequency_penalty));
    if Presence_penalty <> 0 then
      LJsonObject.Add('presence_penalty',
          TJSONFloatNumber.Create(Presence_penalty));
    if Seed > 0 then
      LJsonObject.Add('seed', TJSONIntegerNumber.Create(Seed));

    // 2. Historial de mensajes
    LMessagesArray := TJSONArray.Create;
    for I := 0 to Messages.Count - 1 do
    begin
      LMsg    := Messages[I];
      LMsgObj := TJSONObject.Create;
      LRoleStr := MapRoleToCohere(LMsg.Role);
      LMsgObj.Add('role', LRoleStr);

      if (LRoleStr = 'assistant') and (LMsg.Tool_calls <> '') then
      begin
        // Mensaje del asistente con peticion de tool_calls
        LToolCallsJson := GetJSON(LMsg.Tool_calls);
        if Assigned(LToolCallsJson) and (LToolCallsJson is TJSONArray) then
          LMsgObj.Add('tool_calls', TJSONArray(LToolCallsJson))
        else
        begin
          FreeAndNil(LToolCallsJson);
          LMsgObj.Add('tool_calls', TJSONArray.Create);
        end;
        if LMsg.Prompt <> '' then
          LMsgObj.Add('content', LMsg.Prompt);
      end
      else if LRoleStr = 'tool' then
      begin
        // Resultado de herramienta (v2: tool_call_id + content)
        HasToolResults := True;
        if LMsg.ToolCallId <> '' then
          LMsgObj.Add('tool_call_id', LMsg.ToolCallId);
        LMsgObj.Add('content', LMsg.Prompt);
      end
      else if (LMsg.MediaFiles.Count > 0) and (LRoleStr = 'user') then
      begin
        // Mensaje multimodal con imagenes
        LContentArray := TJSONArray.Create;
        LTextPart := TJSONObject.Create;
        LTextPart.Add('type', 'text');
        LTextPart.Add('text', LMsg.Prompt);
        LContentArray.Add(LTextPart);

        for MI := 0 to LMsg.MediaFiles.Count - 1 do
        begin
          LMediaFile := LMsg.MediaFiles[MI];
          if LMediaFile.FileCategory = Tfc_Image then
          begin
            LImagePart   := TJSONObject.Create;
            LImageUrlObj := TJSONObject.Create;
            LImagePart.Add('type', 'image_url');
            LImageUrlObj.Add('url',
                'data:' + LMediaFile.MimeType + ';base64,' + LMediaFile.Base64);
            LImagePart.Add('image_url', LImageUrlObj);
            LContentArray.Add(LImagePart);
          end;
        end;
        LMsgObj.Add('content', LContentArray);
      end
      else
      begin
        // Mensaje normal
        LMsgObj.Add('content', LMsg.Prompt);
      end;

      LMessagesArray.Add(LMsgObj);
    end;
    LJsonObject.Add('messages', LMessagesArray);

    // 3. Herramientas — no enviar si ya hay resultados de tool
    if not HasToolResults and Tool_Active then
    begin
      LToolsStr := GetToolsStr(tfOpenAI);
      if Trim(LToolsStr) <> '' then
      begin
        LToolsJson := GetJSON(LToolsStr);
        if Assigned(LToolsJson) and (LToolsJson is TJSONArray) then
        begin
          LToolsJsonArray := TJSONArray(LToolsJson);
          if LToolsJsonArray.Count > 0 then
          begin
            LJsonObject.Add('tools', LToolsJsonArray);
            if (Tool_choice <> '') and
               (UpperCase(Tool_choice) <> 'AUTO') then
              LJsonObject.Add('tool_choice', UpperCase(Tool_choice));
          end
          else
            LToolsJsonArray.Free;
        end
        else
          FreeAndNil(LToolsJson);
      end;
    end;

    // 4. Documentos para RAG
    if Assigned(FDocuments) and (FDocuments.Count > 0) then
    begin
      LDocsArray := TJSONArray.Create;
      for I := 0 to FDocuments.Count - 1 do
        LDocsArray.Add(FDocuments[I].ToJsonObject);
      LJsonObject.Add('documents', LDocsArray);
    end;

    // 5. Stop sequences
    LStopList.CommaText := Stop;
    if Assigned(FStop_sequences) then
      LStopList.AddStrings(FStop_sequences);
    if LStopList.Count > 0 then
    begin
      LStopArray := TJSONArray.Create;
      for I := 0 to LStopList.Count - 1 do
        LStopArray.Add(LStopList[I]);
      LJsonObject.Add('stop_sequences', LStopArray);
    end;

    // 6. Response format
    if Response_format = tiaChatRfJson then
    begin
      LFormatObj := TJSONObject.Create;
      LFormatObj.Add('type', 'json_object');
      LJsonObject.Add('response_format', LFormatObj);
    end;

    Result := LJsonObject.AsJSON;
  finally
    LJsonObject.Free;
    LStopList.Free;
  end;
end;

// ===========================================================================
//  InternalRunCompletions — overrride para usar /v2/chat (no /chat/completions)
// ===========================================================================

function TCohereChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
var
  ABody      : string;
  sUrl       : string;
  BodyStream : TStringStream;
  RespStream : TStringStream;
  Client     : TFPHTTPClient;
  jData      : TJSONData;
  jObj       : TJSONObject;
begin
  Result       := '';
  FBusy        := True;
  FAbort       := False;
  FLastError   := '';
  FLastContent := '';
  FLastReasoning := '';

  if Assigned(FTmpToolCallBuffer) then
    FTmpToolCallBuffer.Clear;

  // URL Cohere: /v2/chat (no /v1/chat/completions de OpenAI)
  sUrl := Url;
  if (Length(sUrl) > 0) and (sUrl[Length(sUrl)] <> '/') then
    sUrl := sUrl + '/';
  sUrl := sUrl + 'chat';

  ABody := InitChatCompletions;

  DoStateChange(acsConnecting, sUrl);

  if Asynchronous then
  begin
    // Modo async: TAiHttpThread llama a ProcessSSELine por cada linea
    FStreamEventName := '';
    FStreamEventData := '';
    FStreamLastRole  := 'assistant';
    FStreamingToolCalls.Clear;
    FStreamingToolCallsByIdx.Clear;
    FStreamingCitations.Clear;

    StartHttpThread(sUrl, ABody, ['Authorization', 'Bearer ' + ApiKey]);
    Exit;
  end;

  // Modo sync
  BodyStream := TStringStream.Create(ABody);
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    if ResponseTimeOut > 0 then
      Client.IOTimeout := ResponseTimeOut;

    try
      Client.RequestBody := BodyStream;
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
    except
      on E: Exception do
      begin
        FBusy      := False;
        FLastError := E.Message;
        DoStateChange(acsError, E.Message);
        DoError(E.Message, E);
        Exit;
      end;
    end;

    if Client.ResponseStatusCode = 200 then
    begin
      jData := GetJSON(RespStream.DataString);
      if not Assigned(jData) or not (jData is TJSONObject) then
      begin
        FreeAndNil(jData);
        FBusy := False;
        raise Exception.CreateFmt('Error: Respuesta JSON invalida: %s',
            [Copy(RespStream.DataString, 1, 200)]);
      end;
      jObj := TJSONObject(jData);
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
      DoStateChange(acsError, RespStream.DataString);
      raise Exception.CreateFmt('Error Cohere Chat API: %d, %s',
          [Client.ResponseStatusCode, RespStream.DataString]);
    end;
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

// ===========================================================================
//  ParseChat — parsea la respuesta sync Cohere v2
//
//  Formato:
//    { finish_reason, usage: {tokens: {input_tokens, output_tokens}},
//      message: { role, content: [{type,text}], citations: [...],
//                 tool_calls: [{id, type, function: {name, arguments}}] } }
// ===========================================================================

procedure TCohereChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
var
  jUsage        : TJSONObject;
  jTokensNode   : TJSONObject;
  jMessage      : TJSONObject;
  jToolCallObj  : TJSONObject;
  jFuncObj      : TJSONObject;
  jContentArray : TJSONArray;
  jCitations    : TJSONArray;
  jSourcesArray : TJSONArray;
  jToolCalls    : TJSONArray;
  LResponseText : string;
  LFinishReason : string;
  LRole         : string;
  LContentType  : string;
  LSourceType   : string;
  LText         : string;
  LInputTokens  : Integer;
  LOutputTokens : Integer;
  LMsgCitation  : TAiMsgCitation;
  LSource       : TAiCitationSource;
  ToolCall      : TAiToolsFunction;
  LFuncList     : specialize TList<TAiToolsFunction>;
  I             : Integer;
  SI            : Integer;   // indice para sources (loop anidado)
begin
  LResponseText := '';
  LFinishReason := '';
  LRole         := 'assistant';

  JTryGetStr(jObj, 'finish_reason', LFinishReason);

  // --- Tokens (v2: usage.tokens.input_tokens / output_tokens) ---
  jUsage := nil;
  if JTryGetObj(jObj, 'usage', jUsage) then
  begin
    if not JTryGetObj(jUsage, 'tokens', jTokensNode) then
      jTokensNode := jUsage;
    if Assigned(jTokensNode) then
    begin
      LInputTokens  := 0;
      LOutputTokens := 0;
      JTryGetInt(jTokensNode, 'input_tokens',  LInputTokens);
      JTryGetInt(jTokensNode, 'output_tokens', LOutputTokens);
      if Assigned(ResMsg) then
      begin
        ResMsg.Prompt_tokens     := LInputTokens;
        ResMsg.Completion_tokens := LOutputTokens;
        ResMsg.Total_tokens      := LInputTokens + LOutputTokens;
      end;
      Prompt_tokens     := Prompt_tokens     + LInputTokens;
      Completion_tokens := Completion_tokens + LOutputTokens;
      Total_tokens      := Total_tokens      + LInputTokens + LOutputTokens;
    end;
  end;

  // --- Mensaje ---
  jMessage := nil;
  JTryGetObj(jObj, 'message', jMessage);

  // --- Texto: message.content[] ---
  if Assigned(jMessage) then
  begin
    JTryGetStr(jMessage, 'role', LRole);
    jContentArray := nil;
    if JTryGetArr(jMessage, 'content', jContentArray) then
      for I := 0 to jContentArray.Count - 1 do
      begin
        if not (jContentArray.Items[I] is TJSONObject) then Continue;
        jFuncObj := TJSONObject(jContentArray.Items[I]);
        LContentType := JGetStr(jFuncObj, 'type', '');
        if LContentType = 'text' then
        begin
          LText := JGetStr(jFuncObj, 'text', '');
          LResponseText := LResponseText + LText;
        end;
      end;
  end;
  if Assigned(ResMsg) then
    ResMsg.Role := LRole;

  // --- Citations: message.citations[] ---
  jCitations := nil;
  if Assigned(jMessage) and JTryGetArr(jMessage, 'citations', jCitations) then
  begin
    if Assigned(ResMsg) then
      ResMsg.Citations.Clear;
    for I := 0 to jCitations.Count - 1 do
    begin
      if not (jCitations.Items[I] is TJSONObject) then Continue;
      jToolCallObj := TJSONObject(jCitations.Items[I]);
      LMsgCitation := TAiMsgCitation.Create;
      try
        LMsgCitation.Text       := JGetStr(jToolCallObj, 'text', '');
        LMsgCitation.StartIndex := 0;
        LMsgCitation.EndIndex   := 0;
        JTryGetInt(jToolCallObj, 'start', LMsgCitation.StartIndex);
        JTryGetInt(jToolCallObj, 'end',   LMsgCitation.EndIndex);

        jSourcesArray := nil;
        if JTryGetArr(jToolCallObj, 'sources', jSourcesArray) then
          for SI := 0 to jSourcesArray.Count - 1 do
          begin
            if not (jSourcesArray.Items[SI] is TJSONObject) then Continue;
            jFuncObj    := TJSONObject(jSourcesArray.Items[SI]);
            LSource     := TAiCitationSource.Create;
            LSource.SourceType    := cstDocument;
            LSource.DataSource.Id := JGetStr(jFuncObj, 'id', '');
            LMsgCitation.Sources.Add(LSource);
          end;
        if Assigned(ResMsg) then
          ResMsg.Citations.Add(LMsgCitation);
      except
        LMsgCitation.Free;
        raise;
      end;
    end;
  end;

  // --- Tool Calls: message.tool_calls[] ---
  jToolCalls := nil;
  if (UpperCase(LFinishReason) = 'TOOL_CALL') and Assigned(jMessage)
     and JTryGetArr(jMessage, 'tool_calls', jToolCalls) then
  begin
    LFuncList := specialize TList<TAiToolsFunction>.Create;
    try
      for I := 0 to jToolCalls.Count - 1 do
      begin
        if not (jToolCalls.Items[I] is TJSONObject) then Continue;
        jToolCallObj := TJSONObject(jToolCalls.Items[I]);
        jFuncObj     := nil;
        if not JTryGetObj(jToolCallObj, 'function', jFuncObj) then Continue;

        ToolCall := TAiToolsFunction.Create;
        ToolCall.Id   := JGetStr(jToolCallObj, 'id', '');
        ToolCall.Name := JGetStr(jFuncObj, 'name', '');
        if not JTryGetStr(jFuncObj, 'arguments', ToolCall.Arguments) then
          ToolCall.Arguments := '{}';
        LFuncList.Add(ToolCall);
      end;

      if LFuncList.Count > 0 then
      begin
        if Assigned(ResMsg) then
        begin
          ResMsg.Content    := LResponseText;
          ResMsg.Prompt     := LResponseText;
          ResMsg.Tool_calls := jToolCalls.AsJSON;
          InternalAddMessage(ResMsg);
        end;

        // Ejecutar herramientas secuencialmente (FPC: sin TTask)
        for I := 0 to LFuncList.Count - 1 do
        begin
          ToolCall := LFuncList[I];
          if Assigned(AiFunctions) then
          begin
            try
              AiFunctions.DoCallFunction(ToolCall);
            except
              on E: Exception do
                ToolCall.Response := '{"error":"' + E.Message + '"}';
            end;
          end
          else
            ToolCall.Response := '{"error":"AiFunctions not assigned."}';
        end;

        // Agregar resultados al historial
        for I := 0 to LFuncList.Count - 1 do
        begin
          ToolCall := LFuncList[I];
          InternalAddMessage(ToolCall.Response, 'tool',
              ToolCall.Id, ToolCall.Name);
        end;

        // Continuar con la respuesta final
        Self.Run(nil, nil);
        Exit;
      end;
    finally
      for I := 0 to LFuncList.Count - 1 do
        LFuncList[I].Free;
      LFuncList.Free;
    end;
  end;

  // --- Respuesta normal ---
  FLastContent := LResponseText;
  if Assigned(ResMsg) then
  begin
    ResMsg.Content := LResponseText;
    ResMsg.Prompt  := LResponseText;
  end;

  DoStateChange(acsFinished, 'Done');
  if Assigned(ResMsg) then
    DoDataEnd(ResMsg, LRole, LResponseText, jObj)
  else
    DoDataEnd(nil, LRole, LResponseText, jObj);
end;

// ===========================================================================
//  ProcessSSELine — acumula bloques SSE Cohere (event:+data:+linea vacia)
//
//  Cohere SSE usa bloques multi-linea:
//    event: <type>
//    data:  <json>
//    <linea vacia>
//
//  El override acumula lineas y llama a ProcessEventBlock en la linea vacia.
// ===========================================================================

procedure TCohereChat.ProcessSSELine(const ALine: string);
var
  Line: string;
begin
  Line := ALine;
  // Limpiar CR de fin de linea Windows
  if (Length(Line) > 0) and (Line[Length(Line)] = #13) then
    Delete(Line, Length(Line), 1);

  if FAbort then
  begin
    FBusy := False;
    FStreamEventName := '';
    FStreamEventData := '';
    FStreamingToolCalls.Clear;
    FStreamingCitations.Clear;
    DoDataEnd(nil, 'system', 'abort', nil);
    Exit;
  end;

  if Copy(Line, 1, 6) = 'event:' then
    FStreamEventName := Trim(Copy(Line, 7, MaxInt))
  else if Copy(Line, 1, 5) = 'data:' then
    FStreamEventData := Trim(Copy(Line, 6, MaxInt))
  else if Line = '' then
  begin
    // Linea vacia = fin del bloque SSE → procesar si hay datos
    if FStreamEventData <> '' then
      ProcessEventBlock;
    FStreamEventName := '';
    FStreamEventData := '';
  end;
  // Otras lineas (comentarios, etc.) son ignoradas
end;

// ===========================================================================
//  ProcessEventBlock — procesa un bloque SSE Cohere completo
//
//  Tipos de evento manejados:
//    message-start     — inicializa estado de streaming
//    content-delta     — chunk de texto de la respuesta
//    tool-plan-delta   — texto de razonamiento previo a tool calls
//    tool-call-start   — inicio de un tool call (id + nombre)
//    tool-call-delta   — fragmento de argumentos JSON del tool call
//    citation-start    — inicio de una citacion
//    message-end       — fin del mensaje (usage, finish_reason, cierre)
// ===========================================================================

procedure TCohereChat.ProcessEventBlock;
var
  JsonData      : TJSONObject;
  jDelta        : TJSONObject;
  jDeltaMsg     : TJSONObject;
  jContent      : TJSONObject;
  jToolCallObj  : TJSONObject;
  jFuncObj      : TJSONObject;
  jCitObj       : TJSONObject;
  jSources      : TJSONArray;
  jUsage        : TJSONObject;
  jTokens       : TJSONObject;
  jToolCallsArr : TJSONArray;
  jTC           : TJSONObject;
  jFunc         : TJSONObject;
  CurrentToolCall : TAiToolsFunction;
  CurrentCitation : TAiMsgCitation;
  LSource       : TAiCitationSource;
  LResMsg       : TAiChatMessage;
  LType         : string;
  TextChunk     : string;
  LFinishReason : string;
  LId           : string;
  LArgChunk     : string;
  LIdx          : Integer;
  LIn, LOut     : Integer;
  I             : Integer;
  LToolIds      : specialize TList<string>;
begin
  JsonData := TJSONObject(GetJSON(FStreamEventData));
  if not Assigned(JsonData) then
    Exit;
  try
    // El tipo puede venir en el campo 'type' del JSON o en el header SSE
    if not JTryGetStr(JsonData, 'type', LType) then
      LType := FStreamEventName;

    // -----------------------------------------------------------------------
    if LType = 'message-start' then
    begin
      FLastContent    := '';
      FLastReasoning  := '';
      FStreamLastRole := 'assistant';
      FStreamingToolCalls.Clear;
      FStreamingToolCallsByIdx.Clear;
      FStreamingCitations.Clear;
      DoStateChange(acsWriting, 'Streaming Cohere...');
    end

    // -----------------------------------------------------------------------
    else if LType = 'content-delta' then
    begin
      // v2: delta.message.content.text
      TextChunk := '';
      jDelta    := nil;
      if JTryGetObj(JsonData, 'delta', jDelta) then
      begin
        jDeltaMsg := nil;
        if JTryGetObj(jDelta, 'message', jDeltaMsg) then
        begin
          jContent := nil;
          if JTryGetObj(jDeltaMsg, 'content', jContent) then
            JTryGetStr(jContent, 'text', TextChunk);
        end;
      end;

      if TextChunk <> '' then
      begin
        FLastContent := FLastContent + TextChunk;
        DoData(nil, FStreamLastRole, TextChunk, JsonData);
      end;
    end

    // -----------------------------------------------------------------------
    else if LType = 'tool-plan-delta' then
    begin
      // Texto de razonamiento previo a tool calls (flujo de pensamiento del modelo)
      TextChunk := '';
      jDelta    := nil;
      if JTryGetObj(JsonData, 'delta', jDelta) then
        JTryGetStr(jDelta, 'message.tool_plan', TextChunk);

      if TextChunk <> '' then
      begin
        FLastContent := FLastContent + TextChunk;
        DoData(nil, FStreamLastRole, TextChunk, JsonData);
      end;
    end

    // -----------------------------------------------------------------------
    else if LType = 'tool-call-start' then
    begin
      // v2: tool_calls es objeto (no array); index en raiz identifica posicion
      jDelta := nil;
      if JTryGetObj(JsonData, 'delta', jDelta) then
      begin
        jDeltaMsg := nil;
        if JTryGetObj(jDelta, 'message', jDeltaMsg) then
        begin
          jToolCallObj := nil;
          if JTryGetObj(jDeltaMsg, 'tool_calls', jToolCallObj) then
          begin
            CurrentToolCall := TAiToolsFunction.Create;
            CurrentToolCall.Id        := JGetStr(jToolCallObj, 'id', '');
            CurrentToolCall.Arguments := '';

            jFuncObj := nil;
            if JTryGetObj(jToolCallObj, 'function', jFuncObj) then
              CurrentToolCall.Name := JGetStr(jFuncObj, 'name', '');

            if not FStreamingToolCalls.ContainsKey(CurrentToolCall.Id) then
            begin
              FStreamingToolCalls.Add(CurrentToolCall.Id, CurrentToolCall);
              LIdx := 0;
              if JTryGetInt(JsonData, 'index', LIdx) then
                FStreamingToolCallsByIdx.AddOrSetValue(LIdx, CurrentToolCall.Id);
            end
            else
              CurrentToolCall.Free;
          end;
        end;
      end;
    end

    // -----------------------------------------------------------------------
    else if LType = 'tool-call-delta' then
    begin
      // v2: delta.message.tool_calls.function.arguments (fragmento)
      jDelta := nil;
      if JTryGetObj(JsonData, 'delta', jDelta) then
      begin
        jDeltaMsg := nil;
        if JTryGetObj(jDelta, 'message', jDeltaMsg) then
        begin
          jToolCallObj := nil;
          if JTryGetObj(jDeltaMsg, 'tool_calls', jToolCallObj) then
          begin
            LId  := '';
            LIdx := 0;
            if JTryGetInt(JsonData, 'index', LIdx) then
              FStreamingToolCallsByIdx.TryGetValue(LIdx, LId);

            jFuncObj := nil;
            if JTryGetObj(jToolCallObj, 'function', jFuncObj) then
            begin
              LArgChunk := '';
              if JTryGetStr(jFuncObj, 'arguments', LArgChunk) and
                 (LId <> '') and FStreamingToolCalls.ContainsKey(LId) then
                FStreamingToolCalls[LId].Arguments :=
                    FStreamingToolCalls[LId].Arguments + LArgChunk;
            end;
          end;
        end;
      end;
    end

    // -----------------------------------------------------------------------
    else if LType = 'citation-start' then
    begin
      CurrentCitation := TAiMsgCitation.Create;
      try
        jDelta := nil;
        if JTryGetObj(JsonData, 'delta', jDelta) then
        begin
          jDeltaMsg := nil;
          if JTryGetObj(jDelta, 'message', jDeltaMsg) then
          begin
            jCitObj := nil;
            if JTryGetObj(jDeltaMsg, 'citations', jCitObj) then
            begin
              CurrentCitation.Text       := JGetStr(jCitObj, 'text', '');
              CurrentCitation.StartIndex := 0;
              CurrentCitation.EndIndex   := 0;
              JTryGetInt(jCitObj, 'start', CurrentCitation.StartIndex);
              JTryGetInt(jCitObj, 'end',   CurrentCitation.EndIndex);

              jSources := nil;
              if JTryGetArr(jCitObj, 'sources', jSources) then
                for I := 0 to jSources.Count - 1 do
                begin
                  if not (jSources.Items[I] is TJSONObject) then Continue;
                  jFuncObj    := TJSONObject(jSources.Items[I]);
                  LSource     := TAiCitationSource.Create;
                  LSource.SourceType    := cstDocument;
                  LSource.DataSource.Id := JGetStr(jFuncObj, 'id', '');
                  CurrentCitation.Sources.Add(LSource);
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

    // -----------------------------------------------------------------------
    else if LType = 'message-end' then
    begin
      LResMsg := TAiChatMessage.Create(FLastContent, FStreamLastRole);
      try
        LResMsg.Citations.Assign(FStreamingCitations);

        // Tokens de uso del evento final
        jDelta := nil;
        if JTryGetObj(JsonData, 'delta', jDelta) then
        begin
          jUsage := nil;
          if JTryGetObj(jDelta, 'usage', jUsage) then
          begin
            jTokens := nil;
            if JTryGetObj(jUsage, 'tokens', jTokens) then
            begin
              LIn  := 0;
              LOut := 0;
              JTryGetInt(jTokens, 'input_tokens',  LIn);
              JTryGetInt(jTokens, 'output_tokens', LOut);
              LResMsg.Prompt_tokens     := LIn;
              LResMsg.Completion_tokens := LOut;
              LResMsg.Total_tokens      := LIn + LOut;
              Prompt_tokens     := Prompt_tokens     + LIn;
              Completion_tokens := Completion_tokens + LOut;
              Total_tokens      := Total_tokens      + LIn + LOut;
            end;
          end;

          LFinishReason := '';
          JTryGetStr(jDelta, 'finish_reason', LFinishReason);

          if (UpperCase(LFinishReason) = 'TOOL_CALL') and
             (FStreamingToolCalls.Count > 0) then
          begin
            // Construir tool_calls JSON para el historial
            jToolCallsArr := TJSONArray.Create;
            try
              LToolIds := specialize TList<string>.Create;
              try
                for LId in FStreamingToolCalls.Keys do
                  LToolIds.Add(LId);

                for I := 0 to LToolIds.Count - 1 do
                begin
                  CurrentToolCall := FStreamingToolCalls[LToolIds[I]];
                  jTC   := TJSONObject.Create;
                  jTC.Add('id',   CurrentToolCall.Id);
                  jTC.Add('type', 'function');
                  jFunc := TJSONObject.Create;
                  jFunc.Add('name', CurrentToolCall.Name);
                  LArgChunk := CurrentToolCall.Arguments;
                  if LArgChunk = '' then LArgChunk := '{}';
                  jFunc.Add('arguments', LArgChunk);
                  jTC.Add('function', jFunc);
                  jToolCallsArr.Add(jTC);
                end;
              finally
                LToolIds.Free;
              end;

              LResMsg.Tool_calls := jToolCallsArr.AsJSON;
            finally
              jToolCallsArr.Free;
            end;

            InternalAddMessage(LResMsg);
            LResMsg := nil; // InternalAddMessage took ownership

            // Ejecutar herramientas y continuar
            ExecuteAndRespondToToolCalls;
          end
          else
          begin
            // Respuesta normal
            InternalAddMessage(LResMsg);
            DoStateChange(acsFinished, 'Done');
            DoDataEnd(LResMsg, FStreamLastRole, FLastContent, JsonData);
            LResMsg := nil; // Historial es dueño ahora
          end;
        end;
      finally
        if Assigned(LResMsg) then
          LResMsg.Free;
      end;

      FBusy := False;
    end;

  finally
    JsonData.Free;
  end;
end;

// ===========================================================================
//  ExecuteAndRespondToToolCalls — ejecuta herramientas y llama Self.Run
//
//  Ejecuta secuencialmente (FPC no tiene TTask).
//  Llama self.Run(nil,nil) al final para obtener la respuesta del modelo.
// ===========================================================================

procedure TCohereChat.ExecuteAndRespondToToolCalls;
var
  LToolIds    : specialize TList<string>;
  CurrentCall : TAiToolsFunction;
  LId         : string;
  I           : Integer;
begin
  LToolIds := specialize TList<string>.Create;
  try
    for LId in FStreamingToolCalls.Keys do
      LToolIds.Add(LId);

    // Ejecutar cada tool call
    for I := 0 to LToolIds.Count - 1 do
    begin
      CurrentCall := FStreamingToolCalls[LToolIds[I]];
      if Assigned(AiFunctions) then
      begin
        try
          AiFunctions.DoCallFunction(CurrentCall);
        except
          on E: Exception do
            CurrentCall.Response :=
                '{"error":"' + E.Message + '"}';
        end;
      end
      else
        CurrentCall.Response := '{"error":"AiFunctions not assigned."}';
    end;

    // Agregar resultados al historial
    for I := 0 to LToolIds.Count - 1 do
    begin
      CurrentCall := FStreamingToolCalls[LToolIds[I]];
      InternalAddMessage(CurrentCall.Response, 'tool',
          CurrentCall.Id, CurrentCall.Name);
    end;

    // Liberar los tool calls (TDictionary no es owning)
    for I := 0 to LToolIds.Count - 1 do
      FStreamingToolCalls[LToolIds[I]].Free;

  finally
    LToolIds.Free;
  end;

  FStreamingToolCalls.Clear;
  FStreamingToolCallsByIdx.Clear;

  // Llamar de nuevo para obtener la respuesta final del modelo
  Self.Run(nil, nil);
end;

// ===========================================================================
//  Rerank — POST /v2/rerank
// ===========================================================================

function TCohereChat.Rerank(const AQuery: string; ADocuments: TStrings;
    ATopN: Integer): TRerankResponse;
var
  LJsonBody    : TJSONObject;
  LDocsArray   : TJSONArray;
  LResultsArr  : TJSONArray;
  LResultObj   : TJSONObject;
  LMeta        : TJSONObject;
  LBilledUnits : TJSONObject;
  LResponseJson: TJSONObject;
  BodyStream   : TStringStream;
  RespStream   : TStringStream;
  Client       : TFPHTTPClient;
  LNewResult   : TRerankResult;
  LUrl         : string;
  I            : Integer;
begin
  Result := nil;

  if not Assigned(ADocuments) or (ADocuments.Count = 0) then
    raise Exception.Create('La lista de documentos para Rerank no puede estar vacia.');

  LUrl := Url;
  if (Length(LUrl) > 0) and (LUrl[Length(LUrl)] = '/') then
    LUrl := Copy(LUrl, 1, Length(LUrl) - 1);
  LUrl := LUrl + '/rerank';

  LJsonBody  := TJSONObject.Create;
  BodyStream := TStringStream.Create('');
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  try
    LJsonBody.Add('model', FRerankModel);
    LJsonBody.Add('query', AQuery);

    LDocsArray := TJSONArray.Create;
    for I := 0 to ADocuments.Count - 1 do
      LDocsArray.Add(ADocuments[I]);
    LJsonBody.Add('documents', LDocsArray);

    if ATopN > 0 then
      LJsonBody.Add('top_n', TJSONIntegerNumber.Create(ATopN));

    BodyStream.WriteString(LJsonBody.AsJSON);
    BodyStream.Position := 0;

    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    if ResponseTimeOut > 0 then
      Client.IOTimeout := ResponseTimeOut;

    try
      Client.RequestBody := BodyStream;
      Client.HTTPMethod('POST', LUrl, RespStream, [200]);
    except
      on E: Exception do
      begin
        DoError(E.Message, E);
        Exit;
      end;
    end;

    if Client.ResponseStatusCode = 200 then
    begin
      Result        := TRerankResponse.Create;
      LResponseJson := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(LResponseJson) then
      try
        Result.Id := JGetStr(LResponseJson, 'id', '');

        LMeta := nil;
        if JTryGetObj(LResponseJson, 'meta', LMeta) then
        begin
          LBilledUnits := nil;
          if JTryGetObj(LMeta, 'billed_units', LBilledUnits) then
            Result.SearchUnits :=
                JGetInt(LBilledUnits, 'search_units', 0);
        end;

        LResultsArr := nil;
        if JTryGetArr(LResponseJson, 'results', LResultsArr) then
          for I := 0 to LResultsArr.Count - 1 do
          begin
            if not (LResultsArr.Items[I] is TJSONObject) then Continue;
            LResultObj := TJSONObject(LResultsArr.Items[I]);
            LNewResult := TRerankResult.Create;
            LNewResult.Index          := JGetInt(LResultObj, 'index', 0);
            LNewResult.RelevanceScore := 0.0;
            if Assigned(LResultObj.Find('relevance_score')) then
              LNewResult.RelevanceScore :=
                  LResultObj.Get('relevance_score', Double(0.0));
            if (LNewResult.Index >= 0) and
               (LNewResult.Index < ADocuments.Count) then
              LNewResult.DocumentText := ADocuments[LNewResult.Index];
            Result.Results.Add(LNewResult);
          end;
      finally
        LResponseJson.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Error Cohere Rerank: %d - %s',
          [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    LJsonBody.Free;
    BodyStream.Free;
    RespStream.Free;
    Client.Free;
  end;
end;

// ===========================================================================
//  Setters de propiedades
// ===========================================================================

procedure TCohereChat.SetStop_sequences(const Value: TStrings);
begin
  FStop_sequences.Assign(Value);
end;

procedure TCohereChat.SetDocuments(const Value: TCohereDocuments);
var
  SourceDoc : TCohereDocument;
  NewDoc    : TCohereDocument;
  I         : Integer;
begin
  FDocuments.Clear;
  if not Assigned(Value) or (Value.Count = 0) then
    Exit;

  for I := 0 to Value.Count - 1 do
  begin
    SourceDoc := Value[I];
    NewDoc    := TCohereDocument.Create;
    try
      NewDoc.Assign(SourceDoc);
      FDocuments.Add(NewDoc);
    except
      NewDoc.Free;
      raise;
    end;
  end;
end;

// ===========================================================================
//  Registro
// ===========================================================================

initialization
  TAiChatFactory.Instance.RegisterDriver(TCohereChat);

end.
