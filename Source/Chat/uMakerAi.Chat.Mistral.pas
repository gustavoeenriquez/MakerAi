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
// Driver para Mistral AI (https://api.mistral.ai/v1/)
//
// Compatible con OpenAI /chat/completions, con las siguientes diferencias:
//   - Magistral (modelo reasoning): delta.content es array tipado
//     [{type:"text",text:"..."}, {type:"thinking",thinking:[...]}]
//   - Document QnA: inyecta document_url firmada en el ultimo mensaje
//   - OCR: endpoint /v1/ocr con subida de archivos multipart
//   - random_seed en lugar de seed
//   - prompt_mode: "reasoning" en lugar del objeto "reasoning" de OpenAI
//
// Modelos principales:
//   mistral-small-latest   — rapido y economico
//   mistral-large-latest   — alta capacidad
//   magistral-medium-2506  — modelo reasoning (content como array)
//   mistral-ocr-latest     — procesamiento OCR de documentos

unit uMakerAi.Chat.Mistral;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
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
  //  TAiMistralChat — driver Mistral AI
  // ---------------------------------------------------------------------------
  TAiMistralChat = class(TAiChat)
  private
    FOcrIncludeImages           : Boolean;
    FDocumentImageLimit         : Integer;
    FDocumentPageLimit          : Integer;
    FOcrAnnotationPages         : string;
    FOcrPagesNumbers            : string;
    FOcrBboxAnnotationSchema    : TStringList;
    FOcrDocumentAnnotationSchema: TStringList;

    procedure SetOcrIncludeImages(const Value: Boolean);
    procedure SetDocumentImageLimit(const Value: Integer);
    procedure SetDocumentPageLimit(const Value: Integer);
    procedure SetOcrAnnotationPages(const Value: string);
    procedure SetOcrPagesNumbers(const Value: string);
    procedure SetOcrBboxAnnotationSchema(const Value: TStringList);
    procedure SetOcrDocumentAnnotationSchema(const Value: TStringList);

    // HTTP helpers de bajo nivel
    function DoHttpGetStr(const sUrl: string): string;
    function DoHttpDeleteStr(const sUrl: string): string;

    // Helpers internos Mistral
    function  GetSignedUrl(aMediaFile: TAiMediaFile): string;
    function  GetSignedUrlById(const aFileId: string): string;
    function  ParseOcrResponse(jResponse: TJSONObject; ResMsg: TAiChatMessage): string;

  protected
    function  InitChatCompletions: string; override;
    function  ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions; override;
    procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); override;
    procedure ParseDeltaContentArray(AContentArr: TJSONArray; jObj: TJSONObject); override;
    function  InternalRunPDFDescription(aMediaFile: TAiMediaFile;
                  ResMsg, AskMsg: TAiChatMessage): string; override;
    function  InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string; override;

  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;

    function UploadFile(aMediaFile: TAiMediaFile): string; override;
    function DeleteFile(aMediaFile: TAiMediaFile): string; override;
    function UploadFileToCache(aMediaFile: TAiMediaFile;
                 aTTL_Seconds: Integer = 3600): string; override;

  published
    property OcrIncludeImages           : Boolean     read FOcrIncludeImages
                                                      write SetOcrIncludeImages;
    property DocumentImageLimit         : Integer     read FDocumentImageLimit
                                                      write SetDocumentImageLimit;
    property DocumentPageLimit          : Integer     read FDocumentPageLimit
                                                      write SetDocumentPageLimit;
    property OcrAnnotationPages         : string      read FOcrAnnotationPages
                                                      write SetOcrAnnotationPages;
    property OcrPagesNumbers            : string      read FOcrPagesNumbers
                                                      write SetOcrPagesNumbers;
    property OcrBboxAnnotationSchema    : TStringList read FOcrBboxAnnotationSchema
                                                      write SetOcrBboxAnnotationSchema;
    property OcrDocumentAnnotationSchema: TStringList read FOcrDocumentAnnotationSchema
                                                      write SetOcrDocumentAnnotationSchema;
  end;

implementation

const
  GlMistralUrl = 'https://api.mistral.ai/v1/';

{ ---------------------------------------------------------------------------
  Setters triviales
  --------------------------------------------------------------------------- }

procedure TAiMistralChat.SetOcrIncludeImages(const Value: Boolean);
begin FOcrIncludeImages := Value; end;

procedure TAiMistralChat.SetDocumentImageLimit(const Value: Integer);
begin FDocumentImageLimit := Value; end;

procedure TAiMistralChat.SetDocumentPageLimit(const Value: Integer);
begin FDocumentPageLimit := Value; end;

procedure TAiMistralChat.SetOcrAnnotationPages(const Value: string);
begin FOcrAnnotationPages := Value; end;

procedure TAiMistralChat.SetOcrPagesNumbers(const Value: string);
begin FOcrPagesNumbers := Value; end;

procedure TAiMistralChat.SetOcrBboxAnnotationSchema(const Value: TStringList);
begin FOcrBboxAnnotationSchema := Value; end;

procedure TAiMistralChat.SetOcrDocumentAnnotationSchema(const Value: TStringList);
begin FOcrDocumentAnnotationSchema := Value; end;

{ ---------------------------------------------------------------------------
  Ciclo de vida
  --------------------------------------------------------------------------- }

constructor TAiMistralChat.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  ApiKey             := '@MISTRAL_API_KEY';
  Model              := 'mistral-small-latest';
  Url                := GlMistralUrl;
  FOcrIncludeImages  := True;
  FOcrBboxAnnotationSchema     := TStringList.Create;
  FOcrDocumentAnnotationSchema := TStringList.Create;
end;

destructor TAiMistralChat.Destroy;
begin
  FOcrBboxAnnotationSchema.Free;
  FOcrDocumentAnnotationSchema.Free;
  inherited Destroy;
end;

{ ---------------------------------------------------------------------------
  Metodos de clase
  --------------------------------------------------------------------------- }

class function TAiMistralChat.GetDriverName: string;
begin
  Result := 'Mistral';
end;

class procedure TAiMistralChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@MISTRAL_API_KEY');
  Params.Add('Model=mistral-small-latest');
  Params.Add('Max_Tokens=4096');
  Params.Add('URL=' + GlMistralUrl);
end;

class function TAiMistralChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiMistralChat.Create(Sender);
end;

{ ---------------------------------------------------------------------------
  HTTP helpers de bajo nivel
  --------------------------------------------------------------------------- }

function TAiMistralChat.DoHttpGetStr(const sUrl: string): string;
var
  Client    : TFPHTTPClient;
  RespStream: TStringStream;
begin
  Result := '';
  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.IOTimeout := ResponseTimeOut;
    try
      Client.Get(sUrl, RespStream);
      Result := RespStream.DataString;
    except
      on E: Exception do
        LogDebug('DoHttpGetStr error: ' + E.Message);
    end;
  finally
    Client.Free;
    RespStream.Free;
  end;
end;

function TAiMistralChat.DoHttpDeleteStr(const sUrl: string): string;
var
  Client    : TFPHTTPClient;
  RespStream: TStringStream;
begin
  Result := '';
  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.IOTimeout := ResponseTimeOut;
    try
      Client.HTTPMethod('DELETE', sUrl, RespStream, [200]);
      Result := RespStream.DataString;
    except
      on E: Exception do
        LogDebug('DoHttpDeleteStr error: ' + E.Message);
    end;
  finally
    Client.Free;
    RespStream.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  GetSignedUrl / GetSignedUrlById
  --------------------------------------------------------------------------- }

function TAiMistralChat.GetSignedUrl(aMediaFile: TAiMediaFile): string;
var
  sUrl    : string;
  sResp   : string;
  jResp   : TJSONObject;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.IdFile = '') then
    raise Exception.Create('El TAiMediaFile debe tener un ID valido para obtener URL firmada.');

  sUrl  := Url + 'files/' + aMediaFile.IdFile + '/url';
  sResp := DoHttpGetStr(sUrl);

  if sResp = '' then Exit;

  jResp := TJSONObject(GetJSON(sResp));
  if not Assigned(jResp) then Exit;
  try
    Result := JGetStr(jResp, 'url', '');
    if Result <> '' then
    begin
      aMediaFile.UrlMedia   := Result;
      aMediaFile.CloudState := 'url-signed';
    end
    else
    begin
      aMediaFile.CloudState := 'url-sign-failed';
      DoError('Error al obtener URL firmada para ID: ' + aMediaFile.IdFile, nil);
    end;
  finally
    jResp.Free;
  end;
end;

function TAiMistralChat.GetSignedUrlById(const aFileId: string): string;
var
  Media: TAiMediaFile;
begin
  Result := '';
  if aFileId = '' then Exit;
  Media := TAiMediaFile.Create;
  try
    Media.IdFile := aFileId;
    Result := GetSignedUrl(Media);
  finally
    Media.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  InitChatCompletions — construye el JSON del request para Mistral
  --------------------------------------------------------------------------- }

function TAiMistralChat.InitChatCompletions: string;
var
  AJSONObject, jToolChoice   : TJSONObject;
  JArr, JStop                : TJSONArray;
  Lista                      : TStringList;
  I                          : Integer;
  LModel                     : string;
  ActiveFileId, SignedUrl    : string;
  MessagesJson               : TJSONArray;
  LastMessageObj             : TJSONObject;
  PromptText                 : string;
  NewContentArray            : TJSONArray;
  JTextObj, JDocObj          : TJSONObject;
  JFormatConfig, JSchemaObj  : TJSONObject;
  JInnerSchema               : TJSONObject;
  sSchema, sTools            : string;
  Msg                        : TAiChatMessage;
  MediaFile                  : TAiMediaFile;
  JContentVal                : TJSONData;
begin
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := Model;
  if LModel = '' then
    LModel := 'mistral-small-latest';

  AJSONObject := TJSONObject.Create;
  Lista       := TStringList.Create;
  ActiveFileId := '';
  try
    // 1. Detectar archivo en cache (Document QnA)
    for I := FMessages.Count - 1 downto 0 do
    begin
      Msg := FMessages[I];
      if Assigned(Msg.MediaFiles) then
        for MediaFile in Msg.MediaFiles do
          if MediaFile.CacheName <> '' then
          begin
            ActiveFileId := MediaFile.IdFile;
            Break;
          end;
      if ActiveFileId <> '' then Break;
    end;

    // 2. Construir array de mensajes
    MessagesJson := GetMessages;

    // Inyectar document_url en el ultimo mensaje si hay archivo en cache
    if (ActiveFileId <> '') and (MessagesJson.Count > 0) then
    begin
      SignedUrl := GetSignedUrlById(ActiveFileId);
      if SignedUrl <> '' then
      begin
        LastMessageObj := TJSONObject(MessagesJson.Items[MessagesJson.Count - 1]);
        // Leer el prompt actual
        JContentVal := LastMessageObj.Find('content');
        if Assigned(JContentVal) and (JContentVal is TJSONString) then
          PromptText := JContentVal.AsString
        else
          PromptText := '';

        // Construir content array con text + document_url
        NewContentArray := TJSONArray.Create;
        JTextObj := TJSONObject.Create;
        JTextObj.Add('type', 'text');
        JTextObj.Add('text', PromptText);
        NewContentArray.Add(JTextObj);

        JDocObj := TJSONObject.Create;
        JDocObj.Add('type', 'document_url');
        JDocObj.Add('document_url', SignedUrl);
        NewContentArray.Add(JDocObj);

        // Reemplazar content
        LastMessageObj.Delete(LastMessageObj.IndexOfName('content'));
        LastMessageObj.Add('content', NewContentArray);
      end;
    end;
    AJSONObject.Add('messages', MessagesJson);

    // 3. Modelo
    AJSONObject.Add('model', LModel);

    // 4. Herramientas
    if Tool_Active then
    begin
      sTools := GetToolsStr(tfOpenAI);
      if Trim(sTools) <> '' then
      begin
        JArr := TJSONArray(GetJSON(sTools));
        if not Assigned(JArr) then
          raise Exception.Create('La propiedad Tools esta mal definida, debe ser un JsonArray');
        AJSONObject.Add('tools', JArr);

        if Trim(Tool_choice) <> '' then
        begin
          jToolChoice := TJSONObject(GetJSON(Tool_choice));
          if Assigned(jToolChoice) then
            AJSONObject.Add('tool_choice', jToolChoice);
        end;
      end;
    end;

    // 5. Parametros numericos
    AJSONObject.Add('temperature', TJSONFloatNumber.Create(Temperature));
    if Max_tokens > 0 then
      AJSONObject.Add('max_tokens', TJSONIntegerNumber.Create(Max_tokens));
    if Top_p > 0 then
      AJSONObject.Add('top_p', TJSONFloatNumber.Create(Top_p));
    if Frequency_penalty <> 0 then
      AJSONObject.Add('frequency_penalty',
          TJSONFloatNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    if Presence_penalty <> 0 then
      AJSONObject.Add('presence_penalty',
          TJSONFloatNumber.Create(Trunc(Presence_penalty * 100) / 100));
    if Seed > 0 then
      AJSONObject.Add('random_seed', TJSONIntegerNumber.Create(Seed)); // Mistral usa random_seed

    // 6. Stop words
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      JStop := TJSONArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.Add('stop', JStop);
    end;

    // 7. Streaming
    AJSONObject.Add('stream', TJSONBoolean.Create(Asynchronous));

    // 8. Razonamiento Magistral: prompt_mode en lugar de reasoning object
    if ThinkingLevel <> tlDefault then
      AJSONObject.Add('prompt_mode', 'reasoning');

    // 9. Parametros de Document QnA
    if ActiveFileId <> '' then
    begin
      if FDocumentImageLimit > 0 then
        AJSONObject.Add('document_image_limit',
            TJSONIntegerNumber.Create(FDocumentImageLimit));
      if FDocumentPageLimit > 0 then
        AJSONObject.Add('document_page_limit',
            TJSONIntegerNumber.Create(FDocumentPageLimit));
    end;

    // 10. Response format
    case Response_format of
      tiaChatRfJsonSchema:
      begin
        JFormatConfig := TJSONObject.Create;
        JFormatConfig.Add('type', 'json_schema');
        sSchema := Trim(JsonSchema.Text);
        if sSchema <> '' then
        begin
          JSchemaObj  := TJSONObject.Create;
          JInnerSchema := TJSONObject(GetJSON(sSchema));
          if Assigned(JInnerSchema) then
          begin
            JSchemaObj.Add('name', 'json_response');
            JSchemaObj.Add('schema', JInnerSchema);
            JSchemaObj.Add('strict', TJSONBoolean.Create(True));
            JFormatConfig.Add('json_schema', JSchemaObj);
          end
          else
            JSchemaObj.Free;
        end;
        AJSONObject.Add('response_format', JFormatConfig);
      end;
      tiaChatRfJson:
      begin
        JFormatConfig := TJSONObject.Create;
        JFormatConfig.Add('type', 'json_object');
        AJSONObject.Add('response_format', JFormatConfig);
      end;
      else
      begin
        JFormatConfig := TJSONObject.Create;
        JFormatConfig.Add('type', 'text');
        AJSONObject.Add('response_format', JFormatConfig);
      end;
    end;

    Result := AJSONObject.AsJSON;
    Result := StringReplace(Result, '\/', '/', [rfReplaceAll]);

  finally
    AJSONObject.Free;
    Lista.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  ExtractToolCallFromJson — tool_calls en formato Mistral
  --------------------------------------------------------------------------- }

function TAiMistralChat.ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions;
var
  JChoice, JMessage, JFunc, JArg: TJSONObject;
  JToolCalls   : TJSONArray;
  JToolCall    : TJSONData;
  JFuncData    : TJSONData;
  Fun          : TAiToolsFunction;
  SType, Nom, Valor: string;
  I, J, ParamIdx: Integer;
begin
  Result := TAiToolsFunctions.Create;

  for I := 0 to jChoices.Count - 1 do
  begin
    JChoice := TJSONObject(jChoices.Items[I]);
    if not JTryGetObj(JChoice, 'message', JMessage) then Continue;

    if not JTryGetArr(JMessage, 'tool_calls', JToolCalls) then Continue;

    for J := 0 to JToolCalls.Count - 1 do
    begin
      JToolCall := JToolCalls.Items[J];
      if not (JToolCall is TJSONObject) then Continue;

      if not JTryGetStr(TJSONObject(JToolCall), 'type', SType) then
        SType := 'function';

      if SType = 'function' then
      begin
        Fun      := TAiToolsFunction.Create;
        Fun.Id   := JGetStr(TJSONObject(JToolCall), 'id', '');
        Fun.Tipo := SType;

        JFuncData := TJSONObject(JToolCall).Find('function');
        if Assigned(JFuncData) and (JFuncData is TJSONObject) then
        begin
          JFunc         := TJSONObject(JFuncData);
          Fun.Name      := JGetStr(JFunc, 'name', '');
          Fun.Arguments := JGetStr(JFunc, 'arguments', '');
        end;

        try
          if (Fun.Arguments <> '') and (Fun.Arguments <> '{}') then
          begin
            JArg := TJSONObject(GetJSON(Fun.Arguments));
            if Assigned(JArg) then
            try
              for ParamIdx := 0 to JArg.Count - 1 do
              begin
                Nom   := JArg.Names[ParamIdx];
                Valor := JArg.Items[ParamIdx].AsString;
                Fun.Params.Values[Nom] := Valor;
              end;
            finally
              JArg.Free;
            end;
          end;
        except
          // Parametros mal formados — no bloquear
        end;

        Result.Add(Fun.Id, Fun);
      end;
    end;
  end;
end;

{ ---------------------------------------------------------------------------
  ParseDeltaContentArray — bloques tipados en modelos Magistral
  Formato: [{type:"text",text:"..."}, {type:"thinking",thinking:[...]}]
  --------------------------------------------------------------------------- }

procedure TAiMistralChat.ParseDeltaContentArray(AContentArr: TJSONArray;
    jObj: TJSONObject);
var
  Item        : TJSONData;
  ItemObj     : TJSONObject;
  JThinkArr   : TJSONArray;
  ThinkItem   : TJSONData;
  SType, SText, ThinkText, Value: string;
  I, J        : Integer;
begin
  for I := 0 to AContentArr.Count - 1 do
  begin
    Item := AContentArr.Items[I];
    if not (Item is TJSONObject) then Continue;
    ItemObj := TJSONObject(Item);

    if not JTryGetStr(ItemObj, 'type', SType) then Continue;

    if SType = 'text' then
    begin
      if JTryGetStr(ItemObj, 'text', SText) and (SText <> '') then
      begin
        FLastContent := FLastContent + SText;
        Value := StringReplace(SText, #$A, LineEnding, [rfReplaceAll]);
        DoData(nil, FTmpRole, Value, jObj);
      end;
    end
    else if SType = 'thinking' then
    begin
      ThinkText := '';
      if JTryGetArr(ItemObj, 'thinking', JThinkArr) then
        for J := 0 to JThinkArr.Count - 1 do
        begin
          ThinkItem := JThinkArr.Items[J];
          if ThinkItem is TJSONObject then
          begin
            if JTryGetStr(TJSONObject(ThinkItem), 'text', SText) then
              ThinkText := ThinkText + SText;
          end;
        end;

      if ThinkText <> '' then
      begin
        ThinkText := StringReplace(ThinkText, #$A, LineEnding, [rfReplaceAll]);
        DoThinking(nil, FTmpRole, ThinkText, jObj);
      end;
    end;
    // Otros tipos (reference, etc.) se ignoran
  end;
end;

{ ---------------------------------------------------------------------------
  ParseChat — detecta content array (Magistral) vs string (modelos estandar)
  --------------------------------------------------------------------------- }

procedure TAiMistralChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
var
  JChoices     : TJSONArray;
  JItem        : TJSONObject;
  JMessage     : TJSONObject;
  JContentVal  : TJSONData;
  JContentArr  : TJSONArray;
  JChunk       : TJSONObject;
  JThinkingArr : TJSONArray;
  ChunkItem    : TJSONData;
  ThinkVal     : TJSONData;
  SType, SText, sThinking: string;
  ChunkIdx, ThinkIdx: Integer;
begin
  // Detectar si choices[0].message.content es array (Magistral)
  if JTryGetArr(jObj, 'choices', JChoices) and (JChoices.Count > 0) then
  begin
    JItem := TJSONObject(JChoices.Items[0]);
    if JTryGetObj(JItem, 'message', JMessage) then
    begin
      JContentVal := JMessage.Find('content');
      if Assigned(JContentVal) and (JContentVal is TJSONArray) then
      begin
        // MAGISTRAL: content es array de bloques tipados
        JContentArr := TJSONArray(JContentVal);
        SText     := '';
        sThinking := '';

        for ChunkIdx := 0 to JContentArr.Count - 1 do
        begin
          ChunkItem := JContentArr.Items[ChunkIdx];
          if not (ChunkItem is TJSONObject) then Continue;
          JChunk := TJSONObject(ChunkItem);

          if not JTryGetStr(JChunk, 'type', SType) then Continue;

          if SType = 'thinking' then
          begin
            if JTryGetArr(JChunk, 'thinking', JThinkingArr) then
              for ThinkIdx := 0 to JThinkingArr.Count - 1 do
              begin
                ThinkVal := JThinkingArr.Items[ThinkIdx];
                if ThinkVal is TJSONObject then
                begin
                  if JTryGetStr(TJSONObject(ThinkVal), 'text', SType) then
                    sThinking := sThinking + SType;
                end;
              end;
          end;
        end;

        // Reconstruir SText correctamente
        SText := '';
        for ChunkIdx := 0 to JContentArr.Count - 1 do
        begin
          ChunkItem := JContentArr.Items[ChunkIdx];
          if not (ChunkItem is TJSONObject) then Continue;
          JChunk := TJSONObject(ChunkItem);
          if JTryGetStr(JChunk, 'type', SType) and (SType = 'text') then
            if JTryGetStr(JChunk, 'text', SType) then
              SText := SText + SType;
        end;

        // Reemplazar content array con string plano para que inherited funcione
        JMessage.Delete(JMessage.IndexOfName('content'));
        JMessage.Add('content', SText);

        // Agregar reasoning_content para que base class lo procese
        if sThinking <> '' then
        begin
          if JMessage.IndexOfName('reasoning_content') >= 0 then
            JMessage.Delete(JMessage.IndexOfName('reasoning_content'));
          JMessage.Add('reasoning_content', sThinking);
        end;

        // Delegar al ParseChat heredado
        inherited ParseChat(jObj, ResMsg);
        Exit;
      end;
    end;
  end;

  // MODELOS ESTANDAR: content es string, delegar directamente
  inherited ParseChat(jObj, ResMsg);
end;

{ ---------------------------------------------------------------------------
  InternalRunCompletions — bypass para mistral-ocr-latest
  --------------------------------------------------------------------------- }

function TAiMistralChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
var
  LBaseModel: string;
  MF        : TAiMediaFile;
begin
  // mistral-ocr-latest no soporta /v1/chat/completions.
  // Si el OCR ya se ejecuto (CloudState='ocr-completed'), devolver el resultado.
  LBaseModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LBaseModel = 'mistral-ocr-latest' then
  begin
    if Assigned(AskMsg) then
      for MF in AskMsg.MediaFiles do
        if (MF.FileCategory = tfc_Pdf) and (MF.CloudState = 'ocr-completed') then
        begin
          Result := ResMsg.Prompt;
          Exit;
        end;
  end;
  Result := inherited InternalRunCompletions(ResMsg, AskMsg);
end;

{ ---------------------------------------------------------------------------
  InternalRunPDFDescription — OCR via /v1/ocr
  --------------------------------------------------------------------------- }

function TAiMistralChat.InternalRunPDFDescription(aMediaFile: TAiMediaFile;
    ResMsg, AskMsg: TAiChatMessage): string;
var
  sUrl         : string;
  LModel       : string;
  FileId       : string;
  LDataUri     : string;
  JBody        : TJSONObject;
  JDocument    : TJSONObject;
  JPagesArr    : TJSONArray;
  JAnnotDoc    : TJSONObject;
  BodyStream   : TStringStream;
  RespStream   : TStringStream;
  Client       : TFPHTTPClient;
  jResp        : TJSONObject;
  Lista        : TStringList;
  PageIdx      : Integer;
  PageStr      : string;
begin
  Result := '';
  if not Assigned(aMediaFile) then
    raise Exception.Create('Se requiere un TAiMediaFile para OCR.');

  FBusy      := True;
  FLastError := '';

  sUrl   := Url + 'ocr';
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then LModel := 'mistral-ocr-latest';

  // Auto-subir si no tiene ID
  if aMediaFile.IdFile = '' then
  begin
    FileId := UploadFile(aMediaFile);
    if FileId = '' then
    begin
      FBusy := False;
      DoError('No se pudo subir el archivo al API Mistral', nil);
      Exit;
    end;
    aMediaFile.IdFile := FileId;
  end;

  // Obtener URL firmada para el documento
  aMediaFile.CloudState := 'ocr-getting-signed-url';
  LDataUri := GetSignedUrl(aMediaFile);
  if LDataUri = '' then
  begin
    FBusy := False;
    aMediaFile.CloudState := 'ocr-failed:signed-url';
    DoError('No se pudo obtener URL firmada para ID: ' + aMediaFile.IdFile, nil);
    Exit;
  end;

  JBody     := TJSONObject.Create;
  JDocument := TJSONObject.Create;
  BodyStream := TStringStream.Create('');
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  Lista      := TStringList.Create;
  try
    // Modelo
    JBody.Add('model', LModel);
    JBody.Add('id', aMediaFile.IdFile);

    // Documento
    JDocument.Add('type', 'document_url');
    JDocument.Add('document_url', LDataUri);
    JDocument.Add('document_name', aMediaFile.Filename);
    JBody.Add('document', JDocument);

    // Opciones
    JBody.Add('include_image_base64', TJSONBoolean.Create(FOcrIncludeImages));

    // Paginas
    JPagesArr := TJSONArray.Create;
    if FOcrAnnotationPages <> '' then
    begin
      Lista.CommaText := FOcrAnnotationPages;
      for PageStr in Lista do
        if TryStrToInt(Trim(PageStr), PageIdx) then
          JPagesArr.Add(PageIdx);
    end
    else
      JPagesArr.Add(0);
    JBody.Add('pages', JPagesArr);

    // Anotaciones de documento (OCR estructurado)
    if (Assigned(FOcrDocumentAnnotationSchema)) and
       (Trim(FOcrDocumentAnnotationSchema.Text) <> '') then
    begin
      JAnnotDoc := TJSONObject.Create;
      JAnnotDoc.Add('type', 'json_schema');
      JAnnotDoc.Add('json_schema',
          GetJSON(FOcrDocumentAnnotationSchema.Text));
      JBody.Add('document_annotation_format', JAnnotDoc);
    end;

    // Serializar y enviar
    BodyStream.WriteString(JBody.AsJSON);
    BodyStream.Position := 0;

    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    Client.IOTimeout   := ResponseTimeOut;
    Client.RequestBody := BodyStream;
    try
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
      jResp := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jResp) then
      try
        Result := ParseOcrResponse(jResp, ResMsg);
        aMediaFile.CloudState := 'ocr-completed';
        DoDataEnd(ResMsg, 'assistant', Result, nil);
      finally
        jResp.Free;
      end
      else
      begin
        ResMsg.Prompt := RespStream.DataString;
        Result        := ResMsg.Prompt;
      end;
    except
      on E: Exception do
      begin
        FLastError            := E.Message;
        aMediaFile.CloudState := 'ocr-failed';
        DoError('Error en OCR Mistral: ' + E.Message, E);
      end;
    end;
  finally
    JBody.Free;
    BodyStream.Free;
    RespStream.Free;
    Client.Free;
    Lista.Free;
    FBusy := False;
  end;
end;

{ ---------------------------------------------------------------------------
  ParseOcrResponse — extrae texto Markdown e imagenes de la respuesta OCR
  --------------------------------------------------------------------------- }

function TAiMistralChat.ParseOcrResponse(jResponse: TJSONObject;
    ResMsg: TAiChatMessage): string;
var
  JPages        : TJSONArray;
  JPage         : TJSONObject;
  JImagesArr    : TJSONArray;
  JImage        : TJSONObject;
  PageVal, ImgVal: TJSONData;
  EmbeddedImg   : TAiMediaFile;
  MarkdownAcc   : string;
  ImageB64      : string;
  ImageId       : string;
  PosB64        : Integer;
  Base64Data    : string;
  SMarkdown     : string;
  I, PageIdx    : Integer;
begin
  Result      := '';
  MarkdownAcc := '';

  if not Assigned(jResponse) or not Assigned(ResMsg) then Exit;

  if not JTryGetArr(jResponse, 'pages', JPages) then
  begin
    Result        := jResponse.AsJSON;
    ResMsg.Prompt := Result;
    Exit;
  end;

  for PageIdx := 0 to JPages.Count - 1 do
  begin
    PageVal := JPages.Items[PageIdx];
    if not (PageVal is TJSONObject) then Continue;
    JPage := TJSONObject(PageVal);

    // Texto markdown de la pagina
    if JTryGetStr(JPage, 'markdown', SMarkdown) then
    begin
      MarkdownAcc := MarkdownAcc + SMarkdown + LineEnding + '--- Page Break ---' + LineEnding;
    end;

    // Imagenes incrustadas (si include_image_base64=true)
    if JTryGetArr(JPage, 'images', JImagesArr) then
    begin
      for I := 0 to JImagesArr.Count - 1 do
      begin
        ImgVal := JImagesArr.Items[I];
        if not (ImgVal is TJSONObject) then Continue;
        JImage := TJSONObject(ImgVal);

        ImageId  := JGetStr(JImage, 'id', '');
        ImageB64 := JGetStr(JImage, 'image_base64', '');

        if ImageB64 <> '' then
        begin
          // Quitar prefijo data:xxx;base64, si existe
          PosB64 := Pos(';base64,', ImageB64);
          if PosB64 > 0 then
            Base64Data := Copy(ImageB64, PosB64 + Length(';base64,'), MaxInt)
          else
            Base64Data := ImageB64;

          EmbeddedImg := TAiMediaFile.Create;
          try
            EmbeddedImg.LoadFromBase64(ImageId, Base64Data);
            ResMsg.MediaFiles.Add(EmbeddedImg);
          except
            EmbeddedImg.Free;
          end;
        end;
      end;
    end;
  end;

  Result        := MarkdownAcc;
  ResMsg.Prompt := Result;
  FLastContent  := Result;
end;

{ ---------------------------------------------------------------------------
  UploadFile — multipart POST a /files (purpose=ocr)
  --------------------------------------------------------------------------- }

function TAiMistralChat.UploadFile(aMediaFile: TAiMediaFile): string;
var
  sUrl       : string;
  Boundary   : string;
  MPStream   : TMemoryStream;
  RespStream : TStringStream;
  Client     : TFPHTTPClient;
  jResp      : TJSONObject;
  S          : string;
  LFilename  : string;
  LMime      : string;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un TAiMediaFile con contenido para subir.');

  sUrl     := Url + 'files';
  Boundary := 'MistralBoundary' + IntToStr(Random(999999));
  MPStream   := TMemoryStream.Create;
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  LFilename  := aMediaFile.Filename;
  if LFilename = '' then LFilename := 'upload.bin';
  LMime     := aMediaFile.MimeType;
  if LMime = '' then LMime := 'application/octet-stream';
  try
    // Campo: purpose
    S := '--' + Boundary + #13#10 +
         'Content-Disposition: form-data; name="purpose"' + #13#10 + #13#10 +
         'ocr' + #13#10;
    MPStream.WriteBuffer(PChar(S)^, Length(S));

    // Campo: file
    S := '--' + Boundary + #13#10 +
         'Content-Disposition: form-data; name="file"; filename="' + LFilename + '"' + #13#10 +
         'Content-Type: ' + LMime + #13#10 + #13#10;
    MPStream.WriteBuffer(PChar(S)^, Length(S));
    aMediaFile.Content.Position := 0;
    MPStream.CopyFrom(aMediaFile.Content, aMediaFile.Content.Size);

    // Cierre
    S := #13#10 + '--' + Boundary + '--' + #13#10;
    MPStream.WriteBuffer(PChar(S)^, Length(S));
    MPStream.Position := 0;

    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'multipart/form-data; boundary=' + Boundary);
    Client.IOTimeout   := ResponseTimeOut;
    Client.RequestBody := MPStream;
    try
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
      jResp := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jResp) then
      try
        Result          := JGetStr(jResp, 'id', '');
        aMediaFile.IdFile := Result;
      finally
        jResp.Free;
      end;
    except
      on E: Exception do
      begin
        FLastError := E.Message;
        DoError('Error al subir archivo a Mistral: ' + E.Message, E);
      end;
    end;
  finally
    MPStream.Free;
    RespStream.Free;
    Client.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  DeleteFile — HTTP DELETE a /files/{id}
  --------------------------------------------------------------------------- }

function TAiMistralChat.DeleteFile(aMediaFile: TAiMediaFile): string;
var
  sUrl  : string;
  sResp : string;
  jResp : TJSONObject;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.IdFile = '') then
    raise Exception.Create('El TAiMediaFile debe tener un ID valido para borrar.');

  sUrl  := Url + 'files/' + aMediaFile.IdFile;
  sResp := DoHttpDeleteStr(sUrl);

  if sResp = '' then Exit;
  jResp := TJSONObject(GetJSON(sResp));
  if not Assigned(jResp) then Exit;
  try
    if JGetBool(jResp, 'deleted', False) then
      Result := JGetStr(jResp, 'id', '');
  finally
    jResp.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  UploadFileToCache — sube y marca como contexto persistente
  --------------------------------------------------------------------------- }

function TAiMistralChat.UploadFileToCache(aMediaFile: TAiMediaFile;
    aTTL_Seconds: Integer): string;
var
  FileId: string;
begin
  Result := '';
  FileId := UploadFile(aMediaFile);
  if FileId <> '' then
  begin
    aMediaFile.CacheName  := FileId;
    aMediaFile.CloudState := 'cached';
    Result                := FileId;
  end;
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiMistralChat);

end.
