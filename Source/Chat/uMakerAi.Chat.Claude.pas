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


// -----------------------------------------------------------------------------
// TODO: IMPLEMENTACIÓN FUTURA - CITATIONS (RAG Nativo)
// -----------------------------------------------------------------------------
// Falta implementar la funcionalidad de citas ('citations') de la API de Claude.
// Pasos requeridos:
// 1. Core: Añadir propiedades `EnableCitations`, `Title`, `Context` a TAiMediaFile.
// 2. Core: Crear clase `TAiCitation` y lista `TAiCitations` en TAiChatMessage.
// 3. Claude.GetMessages: Al serializar documentos (PDF/Text), inyectar bloque:
// "citations": {"enabled": true}
// 4. Claude.ProcessStreamChunk: Capturar evento `citations_delta` y parsear JSON.
// 5. Claude.ParseChat: Extraer array `citations` de los bloques de contenido.
// Ref: https://docs.anthropic.com/en/docs/build-with-claude/citations
// -----------------------------------------------------------------------------
// ------ Herramientas que no se implementarán por ahora --------------------
// 1. https://platform.claude.com/docs/es/agents-and-tools/tool-use/code-execution-tool
// 2. https://platform.claude.com/docs/es/agents-and-tools/tool-use/fine-grained-tool-streaming
// 3.




// https://platform.claude.com/docs/en/intro
// https://platform.claude.com/docs/en/api/beta/messages/create  //api de la última implementación {/beta/}

unit uMakerAi.Chat.Claude;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.Variants, System.Net.Mime, System.IOUtils,
  System.Generics.Collections, System.NetEncoding, System.JSON,
  System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, REST.JSON, REST.Types, REST.Client,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Tools.Functions, uMakerAi.Core,
  uMakerAi.Utils.CodeExtractor;

type

  // --- Clases para Gestión de Contexto (Context Editing) ---

  TClaudeContextTrigger = class
  public
    TriggerType: string; // Por defecto 'input_tokens'
    Value: Integer;
    constructor Create(aValue: Integer; aType: string = 'input_tokens');
  end;

  TClaudeContextEdit = class
  public
    EditType: string; // 'clear_tool_uses_20250919'
    Trigger: TClaudeContextTrigger;
    Keep_ToolUses: Integer;
    ClearAtLeast_InputTokens: Integer;
    constructor Create;
    destructor Destroy; override;
    function ToJSONObject: TJSONObject;
  end;

  TClaudeContextConfig = class
  private
    FEdits: TObjectList<TClaudeContextEdit>;
  public
    constructor Create;
    destructor Destroy; override;
    // Agrega una regla para limpiar herramientas cuando se alcanzan X tokens
    procedure AddRule_ClearTools(TriggerTokens: Integer; KeepCount: Integer = 0; ClearAtLeast: Integer = 0);
    function ToJSONObject: TJSONObject;
    function IsEmpty: Boolean;
    procedure Clear;
  end;

  // --- Clase auxiliar para procesar bloques en Streaming ---

  TClaudeStreamContentBlock = class
  public
    BlockType: string; // 'text', 'tool_use', 'thinking', 'redacted_thinking'
    TextContent: TStringBuilder;
    JsonContent: TStringBuilder; // Para acumular argumentos JSON parciales
    ToolFunction: TAiToolsFunction;
    Signature: TStringBuilder;
    CitationsBuffer: TJSonArray;
    ExtraData: TJSONObject;
    constructor Create;
    destructor Destroy; override;
  end;



  // --- Clase Principal del Chat Claude ---

  TAiClaudeChat = Class(TAiChat)
  Private
    FStreamResponseMsg: TAiChatMessage;
    FStreamContentBlocks: TDictionary<Integer, TClaudeStreamContentBlock>;
    FStreamBuffer: TStringBuilder;
    FStreamLastEventType: string;

    // Nuevas funcionalidades
    FEnableMemory: Boolean;
    FEnableThinking: Boolean;
    FThinkingBudget: Integer;
    FContextConfig: TClaudeContextConfig;
    FCacheSystemPrompt: Boolean;
    FServiceTier: String;

    function GetToolJson(aToolFormat: TToolFormat): TJSonArray;
    function GetDynamicHeaders: TNetHeaders; // Construye headers Beta dinámicamente
    function GetFileHeaders: TNetHeaders;
    procedure ClearStreamState;
    procedure ProcessStreamChunk(const AChunk: string);

    procedure SetEnableMemory(const Value: Boolean);
    procedure SetEnableThinking(const Value: Boolean);
    procedure SetThinkingBudget(const Value: Integer);

  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Function InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage; Overload; Override;

    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;

    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    procedure DoCallFunction(ToolCall: TAiToolsFunction); Override;

    function ExtractToolCallJson(jChoices: TJSonArray): TJSonArray;

  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;

    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Function GetMessages: TJSonArray; Override;

    // --- Gestión de Archivos (File API) ---
    Function UploadFile(aMediaFile: TAiMediaFile): String; Override;
    Function DownLoadFile(aMediaFile: TAiMediaFile): String; Override;
    Function CheckFileState(aMediaFile: TAiMediaFile): String; Override;
    Function DeleteFile(aMediaFile: TAiMediaFile): String; Override;
    function RetrieveFile(aFileId: string): TAiMediaFile; Override;
    function RetrieveFileList: TAiMediaFiles; Override;
    Function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer = 3600): String; Override;
    function CreateMessageBatch(InputFileId: string): string; // Permite procesar los archivos subidos

    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

    // Método fácil para configurar la limpieza automática de contexto
    // TriggerTokens: A partir de cuántos tokens de entrada se activa la limpieza (ej. 20000)
    // KeepLast: Cuántas interacciones de herramientas recientes conservar (ej. 3)
    procedure ConfigureAutoContextClearing(TriggerTokens: Integer; KeepLast: Integer = 3);

    // Acceso a la configuración de contexto
    property ContextConfig: TClaudeContextConfig read FContextConfig;

    // Permite cachear el System Prompt para ahorrar costos en instrucciones largas
    property CacheSystemPrompt: Boolean read FCacheSystemPrompt write FCacheSystemPrompt;

  Published
    property EnableMemory: Boolean read FEnableMemory write SetEnableMemory default False;
    property EnableThinking: Boolean read FEnableThinking write SetEnableThinking default False;
    property ThinkingBudget: Integer read FThinkingBudget write SetThinkingBudget default 1024;
    Property ServiceTier: String read FServiceTier write FServiceTier;
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.anthropic.com/v1/';
  CLAUDE_API_VERSION = '2023-06-01';

  // --- HEADERS BETA ACTUALIZADOS ---
  // Herramientas generales (se mantiene)
  BETA_HDR_TOOLS = 'tools-2024-05-16';
  // API de Archivos (se mantiene)
  BETA_HDR_FILES = 'files-api-2025-04-14';
  // Gestión de Contexto / Memoria (se mantiene)
  BETA_HDR_MEMORY = 'context-management-2025-06-27';
  // Thinking (se mantiene)
  BETA_HDR_THINKING = 'interleaved-thinking-2025-05-14';
  // Code Execution (Actualizado según lista de headers de la doc)
  BETA_HDR_CODE = 'code-execution-2025-05-22';
  // Computer Use (Actualizado a la última versión disponible en doc)
  BETA_HDR_COMPUTER = 'computer-use-2025-01-24';
  // PDFs (Nuevo: Para asegurar soporte nativo si se envía base64)
  BETA_HDR_PDFS = 'pdfs-2024-09-25';
  // Prompt Caching (Útil para CacheSystemPrompt)
  BETA_HDR_PROMPT_CACHING = 'prompt-caching-2024-07-31';
  // Header para Structured Outputs (JSON Schema & Strict Tools)
  BETA_HDR_STRUCTURED_OUTPUTS = 'structured-outputs-2025-11-13';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiClaudeChat]);
end;


// =============================================================================
// Context Management Helper Classes
// =============================================================================

constructor TClaudeContextTrigger.Create(aValue: Integer; aType: string);
begin
  Value := aValue;
  TriggerType := aType;
end;

constructor TClaudeContextEdit.Create;
begin
  EditType := 'clear_tool_uses_20250919';
  Trigger := nil;
  Keep_ToolUses := 0;
  ClearAtLeast_InputTokens := 0;
end;

destructor TClaudeContextEdit.Destroy;
begin
  Trigger.Free;
  inherited;
end;

function TClaudeContextEdit.ToJSONObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('type', EditType);

  if Assigned(Trigger) then
  begin
    var
    jTrigger := TJSONObject.Create;
    jTrigger.AddPair('type', Trigger.TriggerType);
    jTrigger.AddPair('value', TJSONNumber.Create(Trigger.Value));
    Result.AddPair('trigger', jTrigger);
  end;

  if Keep_ToolUses > 0 then
  begin
    var
    jKeep := TJSONObject.Create;
    jKeep.AddPair('type', 'tool_uses');
    jKeep.AddPair('value', TJSONNumber.Create(Keep_ToolUses));
    Result.AddPair('keep', jKeep);
  end;

  if ClearAtLeast_InputTokens > 0 then
  begin
    var
    jClear := TJSONObject.Create;
    jClear.AddPair('type', 'input_tokens');
    jClear.AddPair('value', TJSONNumber.Create(ClearAtLeast_InputTokens));
    Result.AddPair('clear_at_least', jClear);
  end;
end;

procedure TClaudeContextConfig.Clear;
begin
  FEdits.Clear;
end;

constructor TClaudeContextConfig.Create;
begin
  FEdits := TObjectList<TClaudeContextEdit>.Create(True);
end;

destructor TClaudeContextConfig.Destroy;
begin
  FEdits.Free;
  inherited;
end;

procedure TClaudeContextConfig.AddRule_ClearTools(TriggerTokens: Integer; KeepCount: Integer; ClearAtLeast: Integer);
var
  Rule: TClaudeContextEdit;
begin
  Rule := TClaudeContextEdit.Create;
  Rule.Trigger := TClaudeContextTrigger.Create(TriggerTokens);
  Rule.Keep_ToolUses := KeepCount;
  Rule.ClearAtLeast_InputTokens := ClearAtLeast;
  FEdits.Add(Rule);
end;

function TClaudeContextConfig.IsEmpty: Boolean;
begin
  Result := FEdits.Count = 0;
end;

function TClaudeContextConfig.ToJSONObject: TJSONObject;
var
  jArr: TJSonArray;
  Edit: TClaudeContextEdit;
begin
  if IsEmpty then
    Exit(nil);

  Result := TJSONObject.Create;
  jArr := TJSonArray.Create;
  for Edit in FEdits do
    jArr.Add(Edit.ToJSONObject);

  Result.AddPair('edits', jArr);
end;

// =============================================================================
// Streaming Helper Classes
// =============================================================================

constructor TClaudeStreamContentBlock.Create;
begin
  TextContent := TStringBuilder.Create;
  JsonContent := TStringBuilder.Create;
  Signature := TStringBuilder.Create;
  CitationsBuffer := TJSonArray.Create;
  ExtraData := TJSONObject.Create;
  ToolFunction := nil;
end;

destructor TClaudeStreamContentBlock.Destroy;
begin
  TextContent.Free;
  JsonContent.Free;
  ToolFunction.Free;
  Signature.Free;
  CitationsBuffer.Free;
  ExtraData.Free;
  inherited;
end;


// =============================================================================
// TAiClaudeChat Implementation
// =============================================================================

class function TAiClaudeChat.GetDriverName: string;
Begin
  Result := 'Claude';
End;

class procedure TAiClaudeChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@CLAUDE_API_KEY');
  Params.Add('Model=claude-3-5-sonnet-20240620');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=https://api.anthropic.com/v1/');
End;

class function TAiClaudeChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiClaudeChat.Create(Sender);
End;

function TAiClaudeChat.CreateMessageBatch(InputFileId: string): string;
var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  ReqStream: TStringStream;
  Res: IHTTPResponse;
  jReq, jRes: TJSONObject;
begin
  if InputFileId.IsEmpty then
    raise Exception.Create('InputFileId es requerido');

  Client := TNetHTTPClient.Create(Nil);
  try
    // Headers específicos para Batches
    Headers := [TNetHeader.Create('x-api-key', ApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION), TNetHeader.Create('anthropic-beta', 'message-batches-2024-09-24'), // Header obligatorio para Batches
    TNetHeader.Create('content-type', 'application/json')];

    // Construir el body
    jReq := TJSONObject.Create;
    try
      jReq.AddPair('input_file_id', InputFileId);
      jReq.AddPair('endpoint', '/v1/messages');

      ReqStream := TStringStream.Create(jReq.ToJSON, TEncoding.UTF8);
    finally
      jReq.Free;
    end;

    try
      Res := Client.Post(Url + 'messages/batches', ReqStream, nil, Headers);

      if Res.StatusCode = 200 then
      begin
        jRes := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
        try
          Result := jRes.GetValue<string>('id'); // Guardar este ID para consultar estado luego
        finally
          jRes.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Error creando Batch: %d - %s', [Res.StatusCode, Res.ContentAsString]);
    finally
      ReqStream.Free;
    end;
  finally
    Client.Free;
  end;
end;

constructor TAiClaudeChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@CLAUDE_API_KEY';
  FClient.OnReceiveData := Self.OnInternalReceiveData;
  FClient.ResponseTimeOut := 60000;

  FStreamContentBlocks := TDictionary<Integer, TClaudeStreamContentBlock>.Create;
  FStreamBuffer := TStringBuilder.Create;
  FStreamResponseMsg := nil;
  FContextConfig := TClaudeContextConfig.Create;

  // Valores por defecto
  Model := 'claude-3-5-sonnet-20240620';
  Max_tokens := 4096;
  Url := GlAIUrl;
  FEnableMemory := False;
  FEnableThinking := False;
  FThinkingBudget := 1024;

  FCacheSystemPrompt := False;

end;

destructor TAiClaudeChat.Destroy;
begin
  ClearStreamState;
  FStreamContentBlocks.Free;
  FStreamBuffer.Free;
  FContextConfig.Free;
  inherited;
end;

// --- Property Setters ---

procedure TAiClaudeChat.SetEnableMemory(const Value: Boolean);
begin
  FEnableMemory := Value;
  if Value then
    ChatMediaSupports := ChatMediaSupports + [tcm_Memory]
  else
    ChatMediaSupports := ChatMediaSupports - [tcm_Memory];
end;

procedure TAiClaudeChat.SetEnableThinking(const Value: Boolean);
begin
  FEnableThinking := Value;
end;

procedure TAiClaudeChat.SetThinkingBudget(const Value: Integer);
begin
  FThinkingBudget := Value;
end;

// --- Header Generation ---

function TAiClaudeChat.GetDynamicHeaders: TNetHeaders;
var
  BetaFeatures: TList<string>;
begin
  // Headers base obligatorios
  Result := [TNetHeader.Create('x-api-key', ApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION), TNetHeader.Create('content-type', 'application/json')];

  BetaFeatures := TList<string>.Create;
  try
    // 1. Tools (General) - Se añade si hay herramientas activas
    if Tool_Active then
      BetaFeatures.Add(BETA_HDR_TOOLS);

    // 2. Memory & Context
    if FEnableMemory or (not FContextConfig.IsEmpty) then
      BetaFeatures.Add(BETA_HDR_MEMORY);

    // 3. Code Interpreter (Code Execution)
    if tcm_code_interpreter in ChatMediaSupports then
      BetaFeatures.Add(BETA_HDR_CODE);

    // 4. Computer Use (NUEVO)
    if tcm_ComputerUse in ChatMediaSupports then
      BetaFeatures.Add(BETA_HDR_COMPUTER);

    // 5. Thinking / Output Config
    // Se activa si está habilitado manualmente O si hay un nivel de pensamiento definido
    if FEnableThinking or (ThinkingLevel <> tlDefault) then
      BetaFeatures.Add(BETA_HDR_THINKING);

    // 6. Prompt Caching
    // Siempre útil agregarlo si vamos a usar cache_control
    BetaFeatures.Add(BETA_HDR_PROMPT_CACHING);

    // 7. PDFs
    // Agregamos soporte explícito para PDFs
    if (tcm_pdf in ChatMediaSupports) then
      BetaFeatures.Add(BETA_HDR_PDFS);

    // 8. Files API (CRÍTICO)
    // Siempre anunciamos soporte de Files para el Auto-Upload
    BetaFeatures.Add(BETA_HDR_FILES);

    // Structured Outputs: Se activa si pedimos JSON Schema explícito
    // Opcionalmente también si Tool_Active es true para permitir "strict: true" en tools.
    if (Response_format = tiaChatRfJsonSchema) or Tool_Active then
      BetaFeatures.Add(BETA_HDR_STRUCTURED_OUTPUTS);

    // Construir header acumulado separando por comas
    if BetaFeatures.Count > 0 then
    begin
      // Eliminar duplicados por si acaso
      var
      UniqueBetas := TStringList.Create;
      try
        UniqueBetas.Duplicates := dupIgnore;
        UniqueBetas.Sorted := True;
        for var Beta in BetaFeatures do
          UniqueBetas.Add(Beta);

        var
        HeaderVal := string.Join(',', UniqueBetas.ToStringArray);
        Result := Result + [TNetHeader.Create('anthropic-beta', HeaderVal)];
      finally
        UniqueBetas.Free;
      end;
    end;

  finally
    BetaFeatures.Free;
  end;
end;

function TAiClaudeChat.GetFileHeaders: TNetHeaders;
begin
  Result := [TNetHeader.Create('x-api-key', ApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION), TNetHeader.Create('anthropic-beta', 'files-api-2025-04-14')];
end;


// --- Payload Construction ---

function TAiClaudeChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice: TJSONObject;
  jArrTools, jArrStop: TJSonArray;
  JTools: TJSONObject;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res, LModel: String;
  SystemPrompt: String;

  // Variables para iteración de mensajes (Auto-Upload)
  LMsg: TAiChatMessage;
  LMedia: TAiMediaFile;

  // Variables para nuevas funcionalidades
  jOutputFormat, jMetaData: TJSONObject;
  jSchemaParsed: TJSONValue;

  // Variables auxiliares para Thinking
  LActualThinkingBudget: Integer;
begin
  if User = '' then
    User := 'user';

  // 1. AUTO-UPLOAD
  for LMsg in Self.Messages do
  begin
    if (LMsg.Role = 'user') and (LMsg.MediaFiles.Count > 0) then
    begin
      for LMedia in LMsg.MediaFiles do
      begin
        if LMedia.IdFile.IsEmpty then
        begin
          if (tcm_code_interpreter in ChatMediaSupports) or (not(LMedia.FileCategory in [Tfc_Image, Tfc_pdf])) then
          begin
            try
              UploadFile(LMedia);
            except
            end;
          end;
        end;
      end;
    end;
  end;

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'claude-3-5-sonnet-20240620';

  // ---------------------------------------------------------------------------
  // 2. CÁLCULO DE THINKING BUDGET (Nuevo enfoque sin output_config)
  // ---------------------------------------------------------------------------
  if ThinkingLevel <> tlDefault then
    FEnableThinking := True;

  if FEnableThinking then
  begin
    // Asignar presupuesto según el nivel elegido o usar el manual
    case ThinkingLevel of
      tlLow:
        LActualThinkingBudget := 2048; // Rápido, poco profundo
      tlMedium:
        LActualThinkingBudget := 8192; // Estándar razonable
      tlHigh:
        LActualThinkingBudget := 16384; // Profundo (Cuidado con costos)
    else
      LActualThinkingBudget := FThinkingBudget; // Valor manual
    end;

    // Validación de mínimos API (1024)
    if LActualThinkingBudget < 1024 then
      LActualThinkingBudget := 1024;

    // Validación de MaxTokens: Debe ser estrictamente mayor que el budget
    // Le damos un margen para que pueda escribir la respuesta final
    if Max_tokens <= LActualThinkingBudget then
      Max_tokens := LActualThinkingBudget + 4096;
  end;

  LAsincronico := Self.Asynchronous;
  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSONObject.Create;
  Lista := TStringList.Create;
  Try
    AJSONObject.AddPair('model', LModel);

    // 3. SYSTEM PROMPT
    SystemPrompt := Self.PrepareSystemMsg;
    if SystemPrompt <> '' then
    begin
      if FCacheSystemPrompt then
      begin
        var
        jSysArr := TJSonArray.Create;
        var
        jSysBlock := TJSONObject.Create;
        jSysBlock.AddPair('type', 'text');
        jSysBlock.AddPair('text', SystemPrompt);

        var
        jCache := TJSONObject.Create;
        jCache.AddPair('type', 'ephemeral');
        jSysBlock.AddPair('cache_control', jCache);

        jSysArr.Add(jSysBlock);
        AJSONObject.AddPair('system', jSysArr);
      end
      else
        AJSONObject.AddPair('system', SystemPrompt);
    end;

    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));
    AJSONObject.AddPair('messages', GetMessages);

    // 4. JSON OUTPUT (STRUCTURED OUTPUTS)
    if (Response_format = tiaChatRfJsonSchema) then
    begin
      if (JsonSchema.Text <> '') then
      begin
        try
          var sShema := StringReplace(JsonSchema.Text,'\n',' ',[rfReplaceAll]);
          jSchemaParsed := TJSONObject.ParseJSONValue(sShema);
          if Assigned(jSchemaParsed) and (jSchemaParsed is TJSONObject) then
          begin
            var
            jRootSchema := jSchemaParsed as TJSONObject;

            // --- CORRECCIÓN AUTOMÁTICA PARA CLAUDE ---
            // Claude exige "additionalProperties": false en el nivel raíz si es type object.
            // Verificamos si es type object y si falta la propiedad, la agregamos.
            if (jRootSchema.GetValue<string>('type') = 'object') and (jRootSchema.GetValue('additionalProperties') = nil) then
            begin
              jRootSchema.AddPair('additionalProperties', TJSONBool.Create(False));
            end;
            // -----------------------------------------

            jOutputFormat := TJSONObject.Create;
            jOutputFormat.AddPair('type', 'json_schema');
            jOutputFormat.AddPair('schema', jRootSchema); // Usamos el objeto ya modificado
            AJSONObject.AddPair('output_format', jOutputFormat);
          end
          else if Assigned(jSchemaParsed) then
            jSchemaParsed.Free;
        except
        end;
      end;
    end;

    // -------------------------------------------------------------------------
    // 5. THINKING PARAMETERS (Solo budget, sin output_config)
    // -------------------------------------------------------------------------
    if FEnableThinking then
    begin
      var
      jThink := TJSONObject.Create;
      jThink.AddPair('type', 'enabled');
      jThink.AddPair('budget_tokens', TJSONNumber.Create(LActualThinkingBudget));
      AJSONObject.AddPair('thinking', jThink);

      // Temperatura Forzada a 1.0 (Requisito API para thinking)
      AJSONObject.AddPair('temperature', TJSONNumber.Create(1.0));
    end
    else
    begin
      // Modo Estándar
      if Self.Temperature > 0 then
        AJSONObject.AddPair('temperature', TJSONNumber.Create(Self.Temperature))
      Else if Top_p > 0 then
        AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

      if K > 0 then
        AJSONObject.AddPair('top_k', TJSONNumber.Create(K));
    end;

    // 6. METADATA & SERVICE TIER
    if (Self.User <> '') and (Self.User <> 'user') then
    begin
      jMetaData := TJSONObject.Create;
      jMetaData.AddPair('user_id', Self.User);
      AJSONObject.AddPair('metadata', jMetaData);
    end;

    if (FServiceTier <> '') then
      AJSONObject.AddPair('service_tier', FServiceTier);

    if not FContextConfig.IsEmpty then
    begin
      var
      jContext := FContextConfig.ToJSONObject;
      if Assigned(jContext) then
        AJSONObject.AddPair('context_management', jContext);
    end;

    // 7. TOOLS
    jArrTools := TJSonArray.Create;

    if Tool_Active and (Trim(GetTools(TToolFormat.tfClaude).Text) <> '') then
    begin
      var
      jUserTools := GetToolJson(TToolFormat.tfClaude);
      if Assigned(jUserTools) then
      begin
        for var Val in jUserTools do
          jArrTools.Add(Val.Clone as TJSONObject);
      end;
    end;

    if tcm_WebSearch in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'web_search_20250305');
      JTools.AddPair('name', 'web_search');
      jArrTools.Add(JTools);
    end;

    if tcm_code_interpreter in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'code_execution_20250522');
      JTools.AddPair('name', 'code_execution');
      jArrTools.Add(JTools);
    end;

    if FEnableMemory then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'memory_20250818');
      JTools.AddPair('name', 'memory');
      jArrTools.Add(JTools);
    end;

    if tcm_TextEditor in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'text_editor_20250728');
      JTools.AddPair('name', 'str_replace_based_edit_tool');
      jArrTools.Add(JTools);
    end;

    if tcm_ComputerUse in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'computer_20250124');
      JTools.AddPair('name', 'computer');
      JTools.AddPair('display_width_px', TJSONNumber.Create(1024));
      JTools.AddPair('display_height_px', TJSONNumber.Create(768));
      jArrTools.Add(JTools);
    end;

    if tcm_Shell in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'bash_20250124');
      JTools.AddPair('name', 'bash');
      jArrTools.Add(JTools);
    end;

    if jArrTools.Count > 0 then
    begin
      AJSONObject.AddPair('tools', jArrTools);

      if (Trim(Tool_choice) <> '') then
      begin
        try
          jToolChoice := TJSONObject.ParseJSONValue(Tool_choice) as TJSONObject;
          if Assigned(jToolChoice) then
            AJSONObject.AddPair('tool_choice', TJSONObject(jToolChoice.Clone));
        except
          AJSONObject.AddPair('tool_choice', Tool_choice);
        end;
      end;
    end
    else
      jArrTools.Free;

    // 8. FINALIZACIÓN
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      jArrStop := TJSonArray.Create;
      for I := 0 to Lista.Count - 1 do
        jArrStop.Add(Lista[I]);
      AJSONObject.AddPair('stop_sequences', jArrStop);
    end;

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSON));
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);

  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;


// --- Request Execution ---

Function TAiClaudeChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  jObj: TJSONObject;
begin
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';
  ClearStreamState;

  sUrl := Url + 'messages';

  try
    DoStateChange(acsConnecting, 'Sending request...');

    FHeaders := GetDynamicHeaders;
    FClient.ContentType := 'application/json';
    FClient.Asynchronous := Self.Asynchronous;

    if FClient.Asynchronous then
      FStreamResponseMsg := TAiChatMessage.Create('', 'assistant');

    ABody := InitChatCompletions;

    LogDebug('-- Request body --');
    LogDebug(ABody);

    St := TStringStream.Create(ABody, TEncoding.UTF8);
    try
      St.Position := 0;
{$IFDEF APIDEBUG}
      St.SaveToFile('c:\temp\claude_req.json');
      St.Position := 0;
{$ENDIF}
      FResponse.Clear;

      Res := FClient.Post(sUrl, St, FResponse, FHeaders);

      if not FClient.Asynchronous then
      begin

        LogDebug('-- Response Sinchronous  --');
        LogDebug(Res.ContentAsString);

        if Res.StatusCode = 200 then
        begin
          jObj := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
          try
            ParseChat(jObj, ResMsg);
            Result := FLastContent;
          finally
            jObj.Free;
          end;
        end
        else
          raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end
      else
      begin
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

// --- Synchronous Response Parsing ---

{ procedure TAiClaudeChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
  Var
  choices: TJSonArray;
  jContentItem: TJSONObject;
  JVal: TJSONValue;
  uso: TJSONObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta, StopR: String;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;
  ToolMsg, AskMsg: TAiChatMessage;
  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave, sToolCalls, LModel, cType: String;
  code: TMarkdownCodeExtractor;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;

  jCitationsArr: TJSonArray;
  jCitVal: TJSONValue;
  jCitObj: TJSONObject;
  SearchItem: TAiWebSearchItem;

  jInnerContent, jInnerItem: TJSONObject;
  jInnerArray: TJSonArray;
  NewFile: TAiMediaFile;

  ToolUseID, Cmd, FoundFileName: string;
  ScanItem: TJSONValue;
  ScanObj, InputObj: TJSONObject;
  CmdParts: TArray<string>;
  begin
  AskMsg := GetLastMessage;

  // Parse Metadata
  // Id := jObj.GetValue('id').Value;
  LModel := jObj.GetValue('model').Value;
  Role := jObj.GetValue('role').Value;
  StopR := jObj.GetValue<string>('stop_reason', '');

  ResMsg.StopReason := StopR;
  if StopR = 'refusal' then
  ResMsg.IsRefusal := True;

  if jObj.TryGetValue<TJSONObject>('usage', uso) then
  begin
  aPrompt_tokens := uso.GetValue<Integer>('input_tokens');
  aCompletion_tokens := uso.GetValue<Integer>('output_tokens');
  aTotal_tokens := aPrompt_tokens + aCompletion_tokens;
  end
  else
  begin
  aPrompt_tokens := 0;
  aCompletion_tokens := 0;
  aTotal_tokens := 0;
  end;

  // Parse Content (Interleaved)
  jObj.TryGetValue<TJSonArray>('content', choices);

  for JVal in choices do
  begin
  jContentItem := TJSONObject(JVal);
  cType := jContentItem.GetValue<string>('type');

  if cType = 'text' then
  Respuesta := Respuesta + jContentItem.GetValue<string>('text') + sLineBreak;

  if cType = 'thinking' then
  begin
  ResMsg.ReasoningContent := ResMsg.ReasoningContent + jContentItem.GetValue<string>('thinking');

  ResMsg.ThinkingSignature := jContentItem.GetValue<string>('signature');
  end;

  if jContentItem.TryGetValue<TJSonArray>('citations', jCitationsArr) then
  begin
  // Crear el contenedor principal si no existe
  if not Assigned(ResMsg.WebSearchResponse) then
  ResMsg.WebSearchResponse := TAiWebSearch.Create;

  for jCitVal in jCitationsArr do
  begin
  jCitObj := jCitVal as TJSONObject;

  // Filtramos solo resultados de búsqueda web
  if jCitObj.GetValue<string>('type') = 'web_search_result_location' then
  begin
  SearchItem := TAiWebSearchItem.Create;

  // Mapeo de propiedades a tus clases del Core
  SearchItem.&type := 'web_search_result_location';
  SearchItem.Title := jCitObj.GetValue<string>('title', '');
  SearchItem.Url := jCitObj.GetValue<string>('url', '');

  // Nota: Claude Beta actual devuelve 'cited_text' (snippet), pero no índices.
  // Tus clases tienen start/end_index, los dejamos en 0 o -1 por ahora.
  SearchItem.start_index := 0;
  SearchItem.end_index := 0;

  // Opcional: Si quisieras guardar el 'cited_text' (resumen),
  // podrías concatenarlo al título o necesitarías un campo extra en TAiWebSearchItem.
  // Por ahora nos ceñimos a tu estructura:
  // SearchItem.Snippet := jCitObj.GetValue<string>('cited_text');

  ResMsg.WebSearchResponse.annotations.Add(SearchItem);
  end;
  end;
  end;

  if ContainsText(cType, 'tool_result') or ContainsText(cType, 'code_execution') then
  begin
  // Navegar la estructura JSON profunda del sandbox
  // Estructura: content -> content (array) -> file_id

  if jContentItem.TryGetValue<TJSONObject>('content', jInnerContent) then
  begin
  if jInnerContent.TryGetValue<TJSonArray>('content', jInnerArray) then
  begin
  for var valInner in jInnerArray do
  begin
  jInnerItem := valInner as TJSONObject;

  // Buscamos el ID del archivo
  if jInnerItem.TryGetValue<string>('file_id', Clave) then // Reusamos var Clave o string temporal
  begin
  NewFile := TAiMediaFile.Create;
  NewFile.IdFile := Clave; // El ID del archivo (ej: file_011CVTV...)
  //NewFile.FileCategory := Tfc_Document; // O generico
  NewFile.FileName := 'generated_file_' + Copy(Clave, 1, 8); // Nombre temporal
  DownLoadFile(NewFile);

  // Agregamos al mensaje para que el usuario sepa que hay un archivo
  ResMsg.MediaFiles.Add(NewFile);
  end;
  end;
  end;
  end;
  end;

  if ContainsText(cType, 'tool_result') or ContainsText(cType, 'code_execution') then
  begin
  // Navegar la estructura profunda para encontrar el file_id
  if jContentItem.TryGetValue<TJSONObject>('content', jInnerContent) then
  begin
  if jInnerContent.TryGetValue<TJSonArray>('content', jInnerArray) then
  begin
  for var valInner in jInnerArray do
  begin
  jInnerItem := valInner as TJSONObject;

  // ¿Encontramos un file_id?
  if jInnerItem.TryGetValue<string>('file_id', Clave) then
  begin
  // --- RECUPERACIÓN INTELIGENTE DEL NOMBRE ---
  FoundFileName := 'generated_file_' + Copy(Clave, 1, 8); // Nombre temporal

  // 1. Obtenemos el ID de la herramienta que generó este resultado
  ToolUseID := jContentItem.GetValue<string>('tool_use_id', '');

  if ToolUseID <> '' then
  begin
  // 2. Buscamos en todo el array de contenidos el comando original
  // Nota: 'choices' es el TJSONArray que estamos recorriendo
  for ScanItem in choices do
  begin
  ScanObj := ScanItem as TJSONObject;
  // Coincidencia de ID y que sea un tool_use

  Var
  sId: String;
  If ScanObj.TryGetValue<string>('id', sId) and (sId = ToolUseID) then
  Begin
  Var
  sIdType: String;
  If ScanObj.TryGetValue<string>('type', sIdType) and (sIdType = 'server_tool_use') then
  begin
  if ScanObj.TryGetValue<TJSONObject>('input', InputObj) then
  begin
  Cmd := InputObj.GetValue<string>('command', '');
  // El patrón estándar es: cp /ruta/nombre.ext $OUTPUT_DIR/
  if Cmd.StartsWith('cp ') then
  begin

  // Simple parsing por espacios
  CmdParts := Cmd.Split([' ']);
  if Length(CmdParts) >= 2 then
  begin
  // CmdParts[1] debería ser "/tmp/frecuencia_333hz.wav"
  // Usamos ExtractFileName (System.SysUtils)

  Var
  sPath := CmdParts[1];
  FoundFileName := ExtractFileName(sPath);
  end;

  end;
  end;
  Break; // Encontramos el padre, salimos del scan
  end;
  End;

  end;
  end;
  // -------------------------------------------

  NewFile := TAiMediaFile.Create;
  NewFile.IdFile := Clave; // file_011CV...
  // NewFile.FileCategory := Tfc_Document;
  NewFile.FileName := FoundFileName; // ¡Nombre correcto asignado!

  DownLoadFile(NewFile);

  ResMsg.MediaFiles.Add(NewFile);
  end;
  end;
  end;
  end;
  end;

  end;

  Respuesta := Trim(Respuesta);

  // Parse Tools
  Var
  JToolCallArray := ExtractToolCallJson(choices);
  if JToolCallArray.Count > 0 then
  sToolCalls := JToolCallArray.Format;
  JToolCallArray.Free;

  LFunciones := ExtractToolCallFromJson(choices);

  // Update ResMsg
  Self.FLastContent := Respuesta;
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + aTotal_tokens;

  if sToolCalls.IsEmpty then
  begin
  ResMsg.Role := Role;
  ResMsg.Model := LModel;
  ResMsg.Tool_calls := sToolCalls;
  ResMsg.Prompt := ResMsg.Prompt + Respuesta;
  // (Metrics updates)
  DoProcessResponse(AskMsg, ResMsg, Respuesta);
  end
  else
  begin
  Var
  Msg := TAiChatMessage.Create(Respuesta, Role);
  Msg.Tool_calls := sToolCalls;
  Msg.ReasoningContent := ResMsg.ReasoningContent; // Preserve thinking
  Msg.ThinkingSignature := ResMsg.ThinkingSignature;
  Msg.Id := FMessages.Count + 1;
  FMessages.Add(Msg);
  end;

  try
  if LFunciones.Count > 0 then
  begin
  NumTasks := LFunciones.Count;
  SetLength(TaskList, NumTasks);
  I := 0;
  for Clave in LFunciones.Keys do
  begin
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
  end;
  TTask.WaitForAll(TaskList);

  for Clave in LFunciones.Keys do
  begin
  ToolCall := LFunciones[Clave];
  ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'user', ToolCall.Id, ToolCall.Name);
  ToolMsg.Id := FMessages.Count + 1;
  FMessages.Add(ToolMsg);
  end;
  Self.Run(Nil, ResMsg);
  end
  else
  begin
  if tfc_ExtracttextFile in NativeOutputFiles then
  begin
  code := TMarkdownCodeExtractor.Create;
  try
  CodeFiles := code.ExtractCodeFiles(Respuesta);
  for var CodeFile in CodeFiles do
  begin
  St := TStringStream.Create(CodeFile.code);
  try
  St.Position := 0;
  MF := TAiMediaFile.Create;
  MF.LoadFromStream('file.' + CodeFile.FileType, St);
  ResMsg.MediaFiles.Add(MF);
  finally
  St.Free;
  end;
  end;
  finally
  code.Free;
  end;
  end;

  DoProcessResponse(AskMsg, ResMsg, Respuesta);
  ResMsg.Prompt := Respuesta;
  FBusy := False;
  DoStateChange(acsFinished, 'Done'); // <--- ESTADO FINALIZADO
  if Assigned(FOnReceiveDataEnd) then
  FOnReceiveDataEnd(Self, ResMsg, jObj, Role, Respuesta);
  end;
  finally
  LFunciones.Free;
  end;
  end;
}

procedure TAiClaudeChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
Var
  choices: TJSonArray;
  jContentItem: TJSONObject;
  JVal: TJSONValue;
  uso: TJSONObject;
  // Variables de conteo
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens, aCached_tokens: Integer;
  Role, Respuesta, StopR: String;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;
  ToolMsg, AskMsg: TAiChatMessage;
  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave, sToolCalls, LModel, cType: String;
  code: TMarkdownCodeExtractor;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;

  jCitationsArr: TJSonArray;
  jCitVal: TJSONValue;
  jCitObj: TJSONObject;
  SearchItem: TAiWebSearchItem;

  // Variables para Navegación de Archivos (Code Execution)
  jInnerContent, jResultContent: TJSONObject;
  jInnerArray, jResultArray: TJSonArray;
  NewFile: TAiMediaFile;
  ToolUseID, Cmd, FoundFileName: string;
  ScanItem: TJSONValue;
  ScanObj, InputObj: TJSONObject;
  CmdParts: TArray<string>;
begin
  AskMsg := GetLastMessage;

  // 1. Parse Metadata
  LModel := jObj.GetValue<string>('model', '');
  Role := jObj.GetValue<string>('role', 'assistant');
  StopR := jObj.GetValue<string>('stop_reason', '');

  ResMsg.StopReason := StopR;
  if StopR = 'refusal' then
    ResMsg.IsRefusal := True;

  // 2. Parse Usage (CORREGIDO)
  aPrompt_tokens := 0;
  aCompletion_tokens := 0;
  aTotal_tokens := 0;
  aCached_tokens := 0;

  if jObj.TryGetValue<TJSONObject>('usage', uso) then
  begin
    aPrompt_tokens := uso.GetValue<Integer>('input_tokens', 0);
    aCompletion_tokens := uso.GetValue<Integer>('output_tokens', 0);

    // Captura de tokens cacheados (Beta Prompt Caching)
    aCached_tokens := uso.GetValue<Integer>('cache_read_input_tokens', 0);

    // Nota: Total tokens en Claude suele ser input + output.
    // Cache creation tokens ya están incluidos en input_tokens según la doc.
    aTotal_tokens := aPrompt_tokens + aCompletion_tokens;
  end;

  // 3. Parse Content (Interleaved Blocks)
  if jObj.TryGetValue<TJSonArray>('content', choices) then
  begin
    for JVal in choices do
    begin
      if not(JVal is TJSONObject) then
        Continue;

      jContentItem := TJSONObject(JVal);
      cType := jContentItem.GetValue<string>('type');

      // A. Texto Normal
      if cType = 'text' then
        Respuesta := Respuesta + jContentItem.GetValue<string>('text') + sLineBreak;

      // B. Thinking (Razonamiento Extendido)
      if cType = 'thinking' then
      begin
        Var
        sThinkingContent := jContentItem.GetValue<string>('thinking');

        ResMsg.ReasoningContent := ResMsg.ReasoningContent + jContentItem.GetValue<string>('thinking');
        if jContentItem.TryGetValue<string>('signature', Clave) then
          ResMsg.ThinkingSignature := Clave;

        // Disparar evento para UI
        if Assigned(OnReceiveThinking) then
          OnReceiveThinking(Self, ResMsg, jObj, 'assistant', sThinkingContent);

      end;

      // C. Citations (Búsqueda Web / RAG)
      if jContentItem.TryGetValue<TJSonArray>('citations', jCitationsArr) then
      begin
        if not Assigned(ResMsg.WebSearchResponse) then
          ResMsg.WebSearchResponse := TAiWebSearch.Create;

        for jCitVal in jCitationsArr do
        begin
          jCitObj := jCitVal as TJSONObject;
          if jCitObj.GetValue<string>('type') = 'web_search_result_location' then
          begin
            SearchItem := TAiWebSearchItem.Create;
            SearchItem.&type := 'web_search_result_location';
            SearchItem.Title := jCitObj.GetValue<string>('title', '');
            SearchItem.Url := jCitObj.GetValue<string>('url', '');
            ResMsg.WebSearchResponse.annotations.Add(SearchItem);
          end;
        end;
      end;

      // D. Code Execution Output
      if (cType = 'tool_result') or (cType = 'code_execution_tool_result') then
      begin

        if jContentItem.TryGetValue<TJSONObject>('content', jInnerContent) then
        begin
          // Verificamos que sea un resultado de ejecución de código
          if jInnerContent.GetValue<string>('type') = 'code_execution_result' then
          begin
            // 2. Buscamos el array interno 'content' que contiene los outputs (archivos)
            if jInnerContent.TryGetValue<TJSonArray>('content', jResultArray) then
            begin
              for var valRes in jResultArray do
              begin
                jResultContent := valRes as TJSONObject;

                // 3. Verificamos si hay un file_id
                if jResultContent.TryGetValue<string>('file_id', Clave) then
                begin
                  // --- LÓGICA DE RECUPERACIÓN DE NOMBRE ---
                  FoundFileName := 'generated_file_' + Copy(Clave, 1, 8) + '.bin';
                  ToolUseID := jContentItem.GetValue<string>('tool_use_id', '');

                  if ToolUseID <> '' then
                  begin
                    // Escanear hacia atrás para encontrar el input y deducir el nombre
                    for ScanItem in choices do
                    begin
                      if not(ScanItem is TJSONObject) then
                        Continue;
                      ScanObj := ScanItem as TJSONObject;

                      var
                        sId: String;
                      if ScanObj.TryGetValue<string>('id', sId) and (sId = ToolUseID) then
                      begin
                        if ScanObj.TryGetValue<TJSONObject>('input', InputObj) then
                        begin
                          // Buscamos en el código Python el nombre del archivo generado
                          var
                          CodeStr := InputObj.GetValue<string>('code', '');

                          // Heurística simple: buscar 'output_path = ...' o strings con extensiones
                          // Esto es más complejo en Python que en Bash, pero intentamos algo básico
                          // Si el código define output_path = ... 'archivo.wav'
                          var
                          PosExt := Pos('.wav', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.csv', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.png', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.pdf', CodeStr);

                          if PosExt > 0 then
                          begin
                            // Intentar extraer el nombre buscando hacia atrás desde la extensión
                            var
                            StartPos := LastDelimiter('''"', Copy(CodeStr, 1, PosExt));
                            if StartPos > 0 then
                            begin
                              var
                              EndPos := PosExt + 3; // Longitud tipica de ext
                              // Ajuste básico, se podría mejorar con RegEx
                              var
                              Candidate := Copy(CodeStr, StartPos + 1, (PosExt - StartPos) + 3);
                              if (Length(Candidate) > 0) and (Length(Candidate) < 50) then
                                FoundFileName := Candidate;
                            end;
                          end;
                        end;
                        Break;
                      end;
                    end;
                  end;
                  // ----------------------------------------

                  NewFile := TAiMediaFile.Create;
                  NewFile.IdFile := Clave;
                  NewFile.FileName := FoundFileName;

                  try
                    DownLoadFile(NewFile);
                    ResMsg.MediaFiles.Add(NewFile);
                  except
                    NewFile.Free;
                  end;
                end;
              end;
            end;
          end;
        end

      end;
    end;
  end;

  Respuesta := Trim(Respuesta);

  // 4. Parse Tools
  var
  JToolCallArray := ExtractToolCallJson(choices);
  if JToolCallArray.Count > 0 then
    sToolCalls := JToolCallArray.Format;
  JToolCallArray.Free;

  LFunciones := ExtractToolCallFromJson(choices);

  // 5. Update Component State & Response Message (AQUÍ ESTÁ LA CORRECCIÓN)
  Self.FLastContent := Respuesta;

  // Actualizar contadores globales del componente
  Self.Prompt_tokens := Self.Prompt_tokens + aPrompt_tokens;
  Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
  Self.Total_tokens := Self.Total_tokens + aTotal_tokens;

  // Actualizar contadores del Mensaje de Respuesta
  ResMsg.Prompt_tokens := aPrompt_tokens;
  ResMsg.Completion_tokens := aCompletion_tokens;
  ResMsg.Total_tokens := aTotal_tokens;
  ResMsg.Cached_tokens := aCached_tokens; // Nuevo: Soporte de caché

  if sToolCalls.IsEmpty then
  begin
    ResMsg.Role := Role;
    ResMsg.Model := LModel;
    ResMsg.Tool_calls := sToolCalls;
    ResMsg.Prompt := ResMsg.Prompt + Respuesta;

    DoProcessResponse(AskMsg, ResMsg, Respuesta);
  end
  else
  begin
    var
    Msg := TAiChatMessage.Create(Respuesta, Role);
    Msg.Tool_calls := sToolCalls;
    Msg.ReasoningContent := ResMsg.ReasoningContent;
    Msg.ThinkingSignature := ResMsg.ThinkingSignature;
    // Asignar tokens también al mensaje temporal si es necesario,
    // aunque generalmente se quedan en el ResMsg principal
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);
  end;

  // 6. Ejecución de Herramientas
  try
    if LFunciones.Count > 0 then
    begin
      NumTasks := LFunciones.Count;
      SetLength(TaskList, NumTasks);
      I := 0;
      for Clave in LFunciones.Keys do
      begin
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
      end;
      TTask.WaitForAll(TaskList);

      for Clave in LFunciones.Keys do
      begin
        ToolCall := LFunciones[Clave];
        ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'user', ToolCall.Id, ToolCall.Name);
        ToolMsg.Id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
      end;

      Self.Run(Nil, ResMsg);
    end
    else
    begin
      if tfc_ExtracttextFile in NativeOutputFiles then
      begin
        code := TMarkdownCodeExtractor.Create;
        try
          CodeFiles := code.ExtractCodeFiles(Respuesta);
          for var CodeFile in CodeFiles do
          begin
            St := TStringStream.Create(CodeFile.code);
            try
              St.Position := 0;
              MF := TAiMediaFile.Create;
              MF.LoadFromStream('file.' + CodeFile.FileType, St);
              ResMsg.MediaFiles.Add(MF);
            finally
              St.Free;
            end;
          end;
        finally
          code.Free;
        end;
      end;

      DoProcessResponse(AskMsg, ResMsg, Respuesta);
      ResMsg.Prompt := Respuesta;
      FBusy := False;
      DoStateChange(acsFinished, 'Done');

      if Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, ResMsg, jObj, Role, Respuesta);
    end;
  finally
    LFunciones.Free;
  end;
end;



// --- Streaming Parser (Async) ---

procedure TAiClaudeChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  line: string;
begin

  if not FClient.Asynchronous then
    Exit;

  LogDebug('-- OnInternalReceiveData--');
  LogDebug(FResponse.DataString);

  AAbort := FAbort;
  if FAbort then
  begin
    FBusy := False;
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    ClearStreamState;
    Exit;
  end;

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
    FStreamBuffer.Remove(0, lastLF);

    for line in processableContent.Split([#10]) do
    begin
      var
      line1 := Trim(line);
      if not line1.IsEmpty then
        ProcessStreamChunk(line1);
    end;
  end;
end;

procedure TAiClaudeChat.ProcessStreamChunk(const AChunk: string);
var
  jData, jDelta, jBlock, jMessage: TJSONObject;
  eventType, textDelta, jsonDelta, StopReason: string;
  blockIndex: Integer;
  streamBlock: TClaudeStreamContentBlock;
begin
  // 1. Procesar línea de evento (event: ...)
  if AChunk.StartsWith('event:') then
  begin
    FStreamLastEventType := Trim(Copy(AChunk, 7, Length(AChunk)));
    Exit;
  end;

  // 2. Procesar línea de datos (data: ...)
  if AChunk.StartsWith('data:') then
  begin
    if FStreamLastEventType = '' then
      Exit;

    var
    jsonDataStr := Trim(Copy(AChunk, 6, Length(AChunk)));
    if jsonDataStr.IsEmpty then
      Exit;

    jData := TJSONObject.ParseJSONValue(jsonDataStr) as TJSONObject;
    if not Assigned(jData) then
      Exit;

    try
      eventType := jData.GetValue<string>('type');

      // =======================================================================
      // EVENTO: message_start
      // Inicio de un nuevo mensaje del asistente
      // =======================================================================
      If AnsiLowerCase(eventType) = 'message_start' then
      begin
        if Assigned(FStreamResponseMsg) then
        begin
          jMessage := jData.GetValue<TJSONObject>('message');
          FStreamResponseMsg.TollCallId := jMessage.GetValue<string>('id'); // ID del mensaje de Claude
          FStreamResponseMsg.Model := jMessage.GetValue<string>('model');
          FStreamResponseMsg.Role := jMessage.GetValue<string>('role');

          // Notificar inicio de recepción
          if Assigned(OnReceiveData) then
            OnReceiveData(Self, FStreamResponseMsg, jData, 'assistant', '');
        end;
      end

      // =======================================================================
      // EVENTO: content_block_start
      // Inicio de un bloque (Texto, Tool Use, Thinking)
      // =======================================================================
      Else If AnsiLowerCase(eventType) = 'content_block_start' then
      begin
        blockIndex := jData.GetValue<Integer>('index');
        jBlock := jData.GetValue<TJSONObject>('content_block');

        streamBlock := TClaudeStreamContentBlock.Create;
        streamBlock.BlockType := jBlock.GetValue<string>('type');

        for var Pair in jBlock do
        begin
          // Evitamos duplicar 'type' que ya guardamos
          if Pair.JsonString.Value <> 'type' then
            streamBlock.ExtraData.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
        end;

        // Si es una herramienta, inicializamos la estructura
        if streamBlock.BlockType = 'tool_use' then
        begin
          streamBlock.ToolFunction := TAiToolsFunction.Create;
          streamBlock.ToolFunction.Id := jBlock.GetValue<string>('id');
          streamBlock.ToolFunction.Name := jBlock.GetValue<string>('name');
          streamBlock.ToolFunction.Tipo := 'function';
        end;

        FStreamContentBlocks.Add(blockIndex, streamBlock);
      end

      // =======================================================================
      // EVENTO: content_block_delta
      // Datos incrementales para un bloque existente
      // =======================================================================
      Else If AnsiLowerCase(eventType) = 'content_block_delta' then
      begin
        blockIndex := jData.GetValue<Integer>('index');
        if FStreamContentBlocks.TryGetValue(blockIndex, streamBlock) then
        begin
          jDelta := jData.GetValue<TJSONObject>('delta');
          var
          deltaType := jDelta.GetValue<string>('type');

          // Delta de Texto
          if deltaType = 'text_delta' then
          begin
            textDelta := jDelta.GetValue<string>('text');
            streamBlock.TextContent.Append(textDelta);

            // Solo agregamos al contenido principal si es un bloque de texto visible
            if streamBlock.BlockType = 'text' then
            begin
              FLastContent := FLastContent + textDelta;
              if Assigned(OnReceiveData) then
                OnReceiveData(Self, FStreamResponseMsg, jData, 'assistant', textDelta);
            end;
          end

          else if deltaType = 'citations_delta' then
          begin
            var
            jCitation := jDelta.GetValue<TJSONObject>('citation');
            if Assigned(jCitation) then
            begin
              // Acumulamos el JSON en el buffer del bloque.
              // TClaudeStreamContentBlock debe tener: CitationsBuffer: TJSonArray;
              streamBlock.CitationsBuffer.Add(jCitation.Clone as TJSONObject);
            end;
          end

          // Delta de JSON (Argumentos de Tool)
          else if deltaType = 'input_json_delta' then
          begin
            jsonDelta := jDelta.GetValue<string>('partial_json');
            streamBlock.JsonContent.Append(jsonDelta);
          end
          // Delta de Thinking (Pensamiento Extendido)
          else if deltaType = 'thinking_delta' then
          begin
            textDelta := jDelta.GetValue<string>('thinking');
            streamBlock.TextContent.Append(textDelta);

            if Assigned(OnReceiveThinking) then
              OnReceiveThinking(Self, FStreamResponseMsg, jData, 'assistant', textDelta);
          end
          // Delta de Firma (Signature para Thinking)
          else if deltaType = 'signature_delta' then
          begin
            var
            sig := jDelta.GetValue<string>('signature');
            streamBlock.Signature.Append(sig);
          end;
        end;
      end

      // =======================================================================
      // EVENTO: content_block_stop
      // Fin de un bloque específico
      // =======================================================================
      Else If AnsiLowerCase(eventType) = 'content_block_stop' then
      begin
        blockIndex := jData.GetValue<Integer>('index');
        if FStreamContentBlocks.TryGetValue(blockIndex, streamBlock) then
        begin
          // Si terminó un bloque de herramienta, parseamos los argumentos JSON acumulados
          if streamBlock.BlockType = 'tool_use' then
          begin
            try
              var
              jInput := TJSONObject.ParseJSONValue(streamBlock.JsonContent.ToString) as TJSONObject;
              if Assigned(jInput) then
              begin
                streamBlock.ToolFunction.Arguments := jInput.Format;
                // Llenar Params para compatibilidad con componentes visuales antiguos
                for var Pair in jInput do
                  streamBlock.ToolFunction.Params.AddPair(Pair.JsonString.Value, Pair.JsonValue.Value);
                jInput.Free;
              end;
            except
              // Ignorar error de parseo JSON en este punto, se manejará globalmente si falla
            end;
          end;
        end;
      end

      // =======================================================================
      // EVENTO: message_delta
      // Cambios a nivel de mensaje (Stop Reason, Usage)
      // =======================================================================
      Else If AnsiLowerCase(eventType) = 'message_delta' then
      begin
        jDelta := jData.GetValue<TJSONObject>('delta');
        if Assigned(jDelta) then
        begin
          StopReason := jDelta.GetValue<string>('stop_reason', '');
          if StopReason <> '' then
            FStreamResponseMsg.StopReason := StopReason;
        end;

        var
        jUsage := jData.GetValue<TJSONObject>('usage');
        if Assigned(jUsage) then
          FStreamResponseMsg.Completion_tokens := jUsage.GetValue<Integer>('output_tokens');
      end

      // =======================================================================
      // EVENTO: message_stop
      // Fin del mensaje completo. Reconstrucción y ejecución.
      // =======================================================================
      Else If AnsiLowerCase(eventType) = 'message_stop' then
      begin
        if Assigned(FStreamResponseMsg) then
        begin
          // 1. Guardamos referencia local para trabajar con seguridad.
          var
            MsgToProcess: TAiChatMessage := FStreamResponseMsg;

            // 2. IMPORTANTE: Desvinculamos la variable global ANTES de llamar a ParseChat.
            // Esto es vital porque ParseChat puede disparar recursividad (DoCallFunction -> Run)
            // y 'Run' creará un NUEVO FStreamResponseMsg. Si hacemos el nil después,
            // borraríamos ese nuevo objeto creado por la recursividad.
          FStreamResponseMsg := nil;

          var
          jSyntheticResponse := TJSONObject.Create;
          try
            // Usamos la variable local MsgToProcess en lugar de FStreamResponseMsg
            jSyntheticResponse.AddPair('id', MsgToProcess.TollCallId);
            jSyntheticResponse.AddPair('type', 'message');
            jSyntheticResponse.AddPair('role', 'assistant');
            jSyntheticResponse.AddPair('model', MsgToProcess.Model);

            if MsgToProcess.StopReason <> '' then
              jSyntheticResponse.AddPair('stop_reason', MsgToProcess.StopReason)
            else
              jSyntheticResponse.AddPair('stop_reason', TJSONNull.Create);

            var
            jUsage := TJSONObject.Create;
            jUsage.AddPair('input_tokens', TJSONNumber.Create(Prompt_tokens));
            jUsage.AddPair('output_tokens', TJSONNumber.Create(MsgToProcess.Completion_tokens));
            jSyntheticResponse.AddPair('usage', jUsage);

            var
            jContentArr := TJSonArray.Create;
            // Ordenamos los bloques para mantener la secuencia correcta
            var
            SortedKeys := TList<Integer>.Create(FStreamContentBlocks.Keys);
            try
              SortedKeys.Sort;
              for var Key in SortedKeys do
              begin
                streamBlock := FStreamContentBlocks[Key];
                var
                jBlockObj := TJSONObject.Create;

                // --- CASO 1: TEXTO ( + Citations) ---
                if (streamBlock.BlockType = 'text') then
                begin
                  jBlockObj.AddPair('type', 'text');
                  jBlockObj.AddPair('text', streamBlock.TextContent.ToString);

                  // Inyectar citas acumuladas (RAG / Web Search)
                  if streamBlock.CitationsBuffer.Count > 0 then
                    jBlockObj.AddPair('citations', streamBlock.CitationsBuffer.Clone as TJSonArray);

                  jContentArr.Add(jBlockObj);
                end

                // --- CASO 2: USO DE HERRAMIENTAS (Tool Use) ---
                else if (streamBlock.BlockType = 'tool_use') and Assigned(streamBlock.ToolFunction) then
                begin
                  jBlockObj.AddPair('type', 'tool_use');
                  jBlockObj.AddPair('id', streamBlock.ToolFunction.Id);
                  jBlockObj.AddPair('name', streamBlock.ToolFunction.Name);

                  var
                  sArgs := streamBlock.JsonContent.ToString;
                  if sArgs.IsEmpty then
                    sArgs := '{}';

                  try
                    var
                    jInputObj := TJSONObject.ParseJSONValue(sArgs);
                    if Assigned(jInputObj) then
                      jBlockObj.AddPair('input', jInputObj)
                    else
                      jBlockObj.AddPair('input', TJSONObject.Create);
                  except
                    jBlockObj.AddPair('input', TJSONObject.Create);
                  end;
                  jContentArr.Add(jBlockObj);
                end

                // --- CASO 3: THINKING (Claude 4.5 / 3.7) ---
                else if (streamBlock.BlockType = 'thinking') then
                begin
                  jBlockObj.AddPair('type', 'thinking');
                  jBlockObj.AddPair('thinking', streamBlock.TextContent.ToString);
                  if streamBlock.Signature.Length > 0 then
                    jBlockObj.AddPair('signature', streamBlock.Signature.ToString);
                  jContentArr.Add(jBlockObj);
                end

                // --- CASO 4: BLOQUES GENÉRICOS (Code Execution Results, etc.) ---
                // Captura 'bash_code_execution_tool_result' donde viene el file_id
                else
                begin
                  jBlockObj.AddPair('type', streamBlock.BlockType);

                  // A. Restaurar metadatos (ej: tool_use_id) capturados en content_block_start
                  if Assigned(streamBlock.ExtraData) and (streamBlock.ExtraData.Count > 0) then
                  begin
                    for var Pair in streamBlock.ExtraData do
                      jBlockObj.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
                  end;

                  // B. Restaurar contenido acumulado (JSON del resultado)
                  if streamBlock.JsonContent.Length > 0 then
                  begin
                    try
                      var
                      jCont := TJSONObject.ParseJSONValue(streamBlock.JsonContent.ToString);
                      if Assigned(jCont) then
                        jBlockObj.AddPair('content', jCont); // Generalmente es un objeto o array
                    except
                      // Fallback si no es JSON válido
                    end;
                  end;

                  jContentArr.Add(jBlockObj);
                end;
              end;
              jSyntheticResponse.AddPair('content', jContentArr);
            finally
              SortedKeys.Free;
            end;

            // 3. Llamamos a ParseChat con la referencia local.
            // Si esto dispara un nuevo Run, FStreamResponseMsg (Global) ya estará libre.
            ParseChat(jSyntheticResponse, MsgToProcess);

          finally
            jSyntheticResponse.Free;

            // Limpieza de buffers locales
            FStreamBuffer.Clear;
            FStreamContentBlocks.Clear;
            FStreamLastEventType := '';
            FBusy := False;
          end;
        end;
      end

      // =======================================================================
      // EVENTO: Error
      // =======================================================================
      Else If AnsiLowerCase(eventType) = 'error' then
      begin
        var
        jError := jData.GetValue<TJSONObject>('error');
        var
        ErrMsg := 'Unknown Error';
        if Assigned(jError) then
          ErrMsg := jError.GetValue<string>('message', 'Unknown');

        DoError('Claude Stream Error: ' + ErrMsg, nil);
        ClearStreamState;
        FBusy := False;
      end;

    finally
      jData.Free;
      FStreamLastEventType := '';
    end;
  end;
end;

procedure TAiClaudeChat.ClearStreamState;
begin
  FStreamBuffer.Clear;
  FStreamContentBlocks.Clear;
  FreeAndNil(FStreamResponseMsg);
  FStreamLastEventType := '';
end;

procedure TAiClaudeChat.ConfigureAutoContextClearing(TriggerTokens: Integer; KeepLast: Integer);
begin
  // Limpia configuraciones previas para evitar duplicados
  // (Asumiendo que FEdits es accesible o exponemos un método Clear en TClaudeContextConfig)
  // En la implementación actual FEdits es privado, así que usaremos una nueva instancia o agregamos Clear.

  // Opción A: Recrear la config (Más seguro/simple)
  FContextConfig.Free;
  FContextConfig := TClaudeContextConfig.Create;

  // También se puede utilizar en lugar de free y create anteriores
  // FContextConfig.Clear; // Limpia reglas anteriores

  // Agregar la regla de limpieza de herramientas
  // TriggerTokens: Cuando el prompt supere este tamaño
  // KeepLast: Mantener los últimos N usos de herramientas (para no perder contexto inmediato)
  // ClearAtLeast: 0 (Default, deja que Claude decida cuánto borrar)
  FContextConfig.AddRule_ClearTools(TriggerTokens, KeepLast, 0);
end;

// --- Internal Overrides ---

function TAiClaudeChat.InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
Var
  Respuesta: String;
  MF: TAiMediaFile;
  Procesado: Boolean;
begin
  Try

    // Adiciona el mensaje a la lista
    aMsg.Id := FMessages.Count + 1;
    FMessages.Add(aMsg);

    If Assigned(FOnAddMessage) then
    Begin
      FOnAddMessage(Self, aMsg, Nil, aMsg.Role, aMsg.Prompt);
    End;

    // Procesamiento de archivos adjuntos (MediaFiles)
    // Si hay archivos que requieren pre-procesamiento (ej: OCR local, conversión), se hace aquí.
    For MF in aMsg.MediaFiles do
    Begin
      Procesado := False;
      DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado); // Envía el archivo por si lo quiere procesar otra AI especializada
      MF.Procesado := Procesado;
      MF.Transcription := Respuesta;
      // Guarda las transcripciones en los MediaFile
    End;

    FLastPrompt := aMsg.Prompt; // Actualiza el último prompt registrado

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, aMsg);

    Result := aMsg;

  Except
    on E: Exception do
      raise Exception.Create('Error en InternalAddMessage: ' + E.Message);
  End;
end;

function TAiClaudeChat.GetMessages: TJSonArray;
var
  LMessage: TAiChatMessage;
  LMessageObj, LPartObj, LSourceObj, LThinkingObj: TJSONObject;
  LContentArray: TJSonArray;
  LMediaFile: TAiMediaFile;
  MediaArr: TAiMediaFilesArray;
  bHasContent: Boolean;
  IsCodeExecutionEnabled: Boolean;
  TargetCategories: TAiFileCategories;
begin
  Result := TJSonArray.Create;

  // Verificamos si el Code Interpreter está activo
  IsCodeExecutionEnabled := tcm_code_interpreter in ChatMediaSupports;

  // --- LÓGICA DE FILTRADO DINÁMICO ---
  if IsCodeExecutionEnabled then
    // Si hay Code Execution, permitimos TODO (Excel, Zip, Code, etc.)
    // Construimos un set con todos los valores posibles del enum
    TargetCategories := [Low(TAiFileCategory) .. High(TAiFileCategory)]
  else
    // Si no, somos estrictos y solo permitimos lo que el componente diga (Vision, PDF)
    TargetCategories := Self.NativeInputFiles;

  for LMessage in Self.Messages do
  begin
    LMessageObj := TJSONObject.Create;
    LMessageObj.AddPair('role', LMessage.Role);
    LContentArray := TJSonArray.Create;

    // -------------------------------------------------------------------------
    // CASO 1: Resultado de Herramienta (Role: User)
    // -------------------------------------------------------------------------
    if (LMessage.Role = 'user') and (not LMessage.TollCallId.IsEmpty) then
    begin
      LPartObj := TJSONObject.Create;
      LPartObj.AddPair('type', 'tool_result');
      LPartObj.AddPair('tool_use_id', LMessage.TollCallId);
      LPartObj.AddPair('content', LMessage.Prompt);
      LContentArray.Add(LPartObj);
    end

    // -------------------------------------------------------------------------
    // CASO 2: Mensaje del Asistente (Texto / Thinking / Tool Request)
    // -------------------------------------------------------------------------
    else if (LMessage.Role = 'assistant') then
    begin
      // A. Thinking Block
      if (FEnableThinking) and (LMessage.ReasoningContent <> '') and (LMessage.ThinkingSignature <> '') then
      begin
        LThinkingObj := TJSONObject.Create;
        LThinkingObj.AddPair('type', 'thinking');
        LThinkingObj.AddPair('thinking', LMessage.ReasoningContent);
        LThinkingObj.AddPair('signature', LMessage.ThinkingSignature);
        LContentArray.Add(LThinkingObj);
      end;

      // B. Texto visible
      if not LMessage.Prompt.IsEmpty then
      begin
        LPartObj := TJSONObject.Create;
        LPartObj.AddPair('type', 'text');
        LPartObj.AddPair('text', LMessage.Prompt);

        if LMessage.CacheControl then
        begin
          var
          jCache := TJSONObject.Create;
          jCache.AddPair('type', 'ephemeral');
          LPartObj.AddPair('cache_control', jCache);
        end;

        LContentArray.Add(LPartObj);
      end;

      // C. Tool Use Blocks
      if LMessage.Tool_calls <> '' then
      begin
        try
          var
          LToolUseArray := TJSonArray.ParseJSONValue(LMessage.Tool_calls) as TJSonArray;
          if Assigned(LToolUseArray) then
          begin
            for var Val in LToolUseArray do
              LContentArray.Add(Val.Clone as TJSONObject);
            LToolUseArray.Free;
          end;
        except
        end;
      end;
    end

    // -------------------------------------------------------------------------
    // CASO 3: Mensaje Estándar de Usuario (Texto + Archivos)
    // -------------------------------------------------------------------------
    else
    begin
      bHasContent := False;

      // A. Texto
      if not LMessage.Prompt.IsEmpty then
      begin
        LPartObj := TJSONObject.Create;
        LPartObj.AddPair('type', 'text');
        LPartObj.AddPair('text', LMessage.Prompt);

        if LMessage.CacheControl then
        begin
          var
          jCache := TJSONObject.Create;
          jCache.AddPair('type', 'ephemeral');
          LPartObj.AddPair('cache_control', jCache);
        end;

        LContentArray.Add(LPartObj);
        bHasContent := True;
      end;

      // B. Archivos (MediaFiles) - USANDO EL FILTRO DINÁMICO
      MediaArr := LMessage.MediaFiles.GetMediaList(TargetCategories, False);

      for LMediaFile in MediaArr do
      begin
        LPartObj := TJSONObject.Create;

        // --- SUB-CASO 3.1: IMÁGENES ---
        if LMediaFile.FileCategory = Tfc_Image then
        begin
          LPartObj.AddPair('type', 'image');
          LSourceObj := TJSONObject.Create;

          if not LMediaFile.IdFile.IsEmpty then
          begin
            LSourceObj.AddPair('type', 'file');
            LSourceObj.AddPair('file_id', LMediaFile.IdFile);
          end
          else
          begin
            LSourceObj.AddPair('type', 'base64');
            // Detección automática de MimeType (Como corregimos antes)
            var
              RealMime: string := LMediaFile.MimeType;
            var
              B64Head: string := Copy(LMediaFile.Base64, 1, 15);
            if StartsStr('iVBORw', B64Head) then
              RealMime := 'image/png'
            else if StartsStr('/9j/', B64Head) then
              RealMime := 'image/jpeg'
            else if StartsStr('R0lGOD', B64Head) then
              RealMime := 'image/gif'
            else if StartsStr('UklGR', B64Head) then
              RealMime := 'image/webp';

            LSourceObj.AddPair('media_type', RealMime);
            LSourceObj.AddPair('data', LMediaFile.Base64);
          end;
          LPartObj.AddPair('source', LSourceObj);
        end

        // --- SUB-CASO 3.2: CONTAINER UPLOAD (Code Interpreter + Data Files) ---
        // Aquí entrará el Excel, CSV, etc.
        else if IsCodeExecutionEnabled and (not LMediaFile.IdFile.IsEmpty) then
        begin
          LPartObj.AddPair('type', 'container_upload');
          LPartObj.AddPair('file_id', LMediaFile.IdFile);
        end

        // --- SUB-CASO 3.3: DOCUMENTOS (PDFs para visión) ---
        else
        begin
          LPartObj.AddPair('type', 'document');
          LSourceObj := TJSONObject.Create;

          if not LMediaFile.IdFile.IsEmpty then
          begin
            LSourceObj.AddPair('type', 'file');
            LSourceObj.AddPair('file_id', LMediaFile.IdFile);
          end
          else
          begin
            LSourceObj.AddPair('type', 'base64');
            LSourceObj.AddPair('media_type', LMediaFile.MimeType);
            LSourceObj.AddPair('data', LMediaFile.Base64);
          end;

          LPartObj.AddPair('source', LSourceObj);
          if not LMediaFile.FileName.IsEmpty then
            LPartObj.AddPair('title', LMediaFile.FileName);
        end;

        if LMediaFile.CacheControl then
        begin
          var
          jCache := TJSONObject.Create;
          jCache.AddPair('type', 'ephemeral');
          LPartObj.AddPair('cache_control', jCache);
        end;

        LContentArray.Add(LPartObj);
        bHasContent := True;
      end;

      if not bHasContent then
      begin
        LPartObj := TJSONObject.Create;
        LPartObj.AddPair('type', 'text');
        LPartObj.AddPair('text', ' ');
        LContentArray.Add(LPartObj);
      end;
    end;

    LMessageObj.AddPair('content', LContentArray);
    Result.Add(LMessageObj);
  end;
end;

class function TAiClaudeChat.GetModels(aApiKey, aUrl: String): TStringList;
var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl, EndPointUrl, sModel: string;
  jRes: TJSONObject;
  jArr: TJSonArray;
  JVal: TJSONValue;
  CustomModels: TArray<string>;
  I: Integer;
begin
  Result := TStringList.Create;

  // 1. Determinar la URL base
  if aUrl <> '' then
    EndPointUrl := aUrl
  else
    EndPointUrl := GlAIUrl;

  sUrl := EndPointUrl + 'models';

  Client := TNetHTTPClient.Create(Nil);
  try
    // 2. Configurar Headers específicos de Claude
    // Nota: Claude requiere 'x-api-key' en lugar de Bearer token, y la versión de la API
    Headers := [TNetHeader.Create('x-api-key', aApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION), TNetHeader.Create('content-type', 'application/json')];

    // 3. Ejecutar Petición GET
    Res := Client.Get(sUrl, nil, Headers);

    if Res.StatusCode = 200 then
    begin
      jRes := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      if Assigned(jRes) then
        try
          // 4. Parsear la respuesta JSON
          // La estructura es: { "data": [ {"id": "...", ...}, ... ] }
          if jRes.TryGetValue<TJSonArray>('data', jArr) then
          begin
            for JVal in jArr do
            begin
              if JVal is TJSONObject then
              begin
                // Extraer el ID del modelo (ej: "claude-3-5-sonnet-20240620")
                sModel := (JVal as TJSONObject).GetValue<string>('id', '');
                if sModel <> '' then
                  Result.Add(sModel);
              end;
            end;
          end;
        finally
          jRes.Free;
        end;

      // 5. Agregar modelos personalizados registrados localmente (si existen)
      // Esto es útil si la API no devuelve modelos finetuned o nuevos que aún no lista
      CustomModels := TAiChatFactory.Instance.GetCustomModels(GetDriverName);
      for I := Low(CustomModels) to High(CustomModels) do
      begin
        if Result.IndexOf(CustomModels[I]) = -1 then
          Result.Add(CustomModels[I]);
      end;

    end
    else
    begin
      // Manejo de errores HTTP
      raise Exception.CreateFmt('Error al obtener modelos de Claude: %d - %s', [Res.StatusCode, Res.ContentAsString(TEncoding.UTF8)]);
    end;

  finally
    Client.Free;
  end;
end;

function TAiClaudeChat.GetToolJson(aToolFormat: TToolFormat): TJSonArray;
begin
  Result := Nil;
  var
  JsonStr := Trim(inherited GetTools(tfClaude).Text);
  if (JsonStr = '') or (not Tool_Active) then
    Exit;
  Result := TJSONObject.ParseJSONValue(JsonStr) as TJSonArray;
end;

function TAiClaudeChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  Arg: TJSONObject;
  JVal1: TJSONValue;
  Fun: TAiToolsFunction;
  I: Integer;
begin
  Result := TAiToolsFunctions.Create;
  For JVal1 in jChoices do
  Begin
    if JVal1.GetValue<String>('type') = 'tool_use' then
    begin
      Fun := TAiToolsFunction.Create;
      Fun.Id := JVal1.GetValue<String>('id');
      Fun.Tipo := 'function';
      Fun.Name := JVal1.GetValue<String>('name');
      if JVal1.TryGetValue<TJSONObject>('input', Arg) then
      begin
        Fun.Arguments := Arg.Format;
        for I := 0 to Arg.Count - 1 do
          Fun.Params.Values[Arg.Pairs[I].JsonString.Value] := Arg.Pairs[I].JsonValue.Value;
      end;
      Result.Add(Fun.Id, Fun);
    end;
  End;
end;

function TAiClaudeChat.ExtractToolCallJson(jChoices: TJSonArray): TJSonArray;
Var
  jObj, Arg: TJSONObject;
  JVal1: TJSONValue;
begin
  Result := TJSonArray.Create;
  For JVal1 in jChoices do
  Begin
    if JVal1.GetValue<String>('type') = 'tool_use' then
    begin
      jObj := TJSONObject.Create;
      jObj.AddPair('type', 'tool_use');
      jObj.AddPair('id', JVal1.GetValue<String>('id'));
      jObj.AddPair('name', JVal1.GetValue<String>('name'));
      if JVal1.TryGetValue<TJSONObject>('input', Arg) then
        jObj.AddPair('input', TJSONObject(Arg.Clone));
      Result.Add(jObj);
    end;
  End;
end;

// --- File Management Implementation ---

function TAiClaudeChat.UploadFile(aMediaFile: TAiMediaFile): String;
var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  Body: TMultipartFormData;
  ResponseStream: TMemoryStream;
  Res: IHTTPResponse;
  jObj: TJSONObject;
  TempStream: TMemoryStream;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Empty file content');

  Client := TNetHTTPClient.Create(Nil);
  ResponseStream := TMemoryStream.Create;
  Body := TMultipartFormData.Create;
  TempStream := TMemoryStream.Create;
  try
    Headers := GetFileHeaders;
    aMediaFile.Content.Position := 0;
    TempStream.LoadFromStream(aMediaFile.Content);
    TempStream.Position := 0;

{$IF CompilerVersion >= 35}
    Body.AddStream('file', TempStream, False, aMediaFile.FileName, aMediaFile.MimeType);
{$ELSE}
    Body.AddStream('file', TempStream, aMediaFile.FileName, aMediaFile.MimeType);
{$ENDIF}
    Body.AddField('purpose', 'assistants'); // Claude usually infers, but good practice

    Res := Client.Post(Url + 'files', Body, ResponseStream, Headers);

    if Res.StatusCode = 200 then
    begin
      jObj := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      try
        Result := jObj.GetValue<string>('id');
        aMediaFile.IdFile := Result;
      finally
        jObj.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Upload Error: %d - %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    Body.Free;
    ResponseStream.Free;
    Client.Free;
    TempStream.Free;
  end;
end;

function TAiClaudeChat.RetrieveFile(aFileId: string): TAiMediaFile;
var
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  jObj: TJSONObject;
begin
  if aFileId.IsEmpty then
    Exit(nil);
  Client := TNetHTTPClient.Create(Nil);
  try
    Res := Client.Get(Url + 'files/' + aFileId, nil, GetFileHeaders);
    if Res.StatusCode = 200 then
    begin
      jObj := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      try
        Result := TAiMediaFile.Create;
        Result.IdFile := jObj.GetValue<string>('id');
        Result.FileName := jObj.GetValue<string>('filename');
      finally
        jObj.Free;
      end;
    end
    else
      raise Exception.Create('Retrieve File Error');
  finally
    Client.Free;
  end;
end;

function TAiClaudeChat.RetrieveFileList: TAiMediaFiles;
begin
  Result := TAiMediaFiles.Create;
  // Implementation simplified for brevity, follows RetrieveFile pattern looping 'data' array
end;

procedure TAiClaudeChat.DoCallFunction(ToolCall: TAiToolsFunction);
begin

  // ---------------------------------------------------------------------------
  // 1. Interceptar Herramienta BASH / SHELL
  // ---------------------------------------------------------------------------
  if (ToolCall.Name = 'bash') then
  begin
    // A. Prioridad: Evento de Usuario (OnCallToolFunction)
    // Permite al programador interceptar, modificar o bloquear el comando antes de ejecutarlo.
    if Assigned(FOnCallToolFunction) then
      FOnCallToolFunction(Self, ToolCall);

    // B. Ejecución Automática (Componente TAiShell)
    // Si el usuario no llenó la respuesta en el evento anterior y tenemos el componente:
    if (ToolCall.Response = '') and Assigned(ShellTool) then
    begin
      // Asegurar que esté activo
      if not ShellTool.Active then
        ShellTool.Active := True;

      // Ejecutar el comando en la sesión persistente
      ToolCall.Response := ShellTool.Execute(ToolCall.Id, ToolCall.Arguments);
    end;

    // Si no hay componente ni evento, Claude recibirá una respuesta vacía o error,
    // lo cual está bien, pero idealmente FShellTool debería estar asignado.
    Exit;
  end;

  // 2. Interceptar Herramienta de Edición Nativa
  if (ToolCall.Name = 'str_replace_based_edit_tool') or (ToolCall.Name = 'str_replace_editor') and Assigned(TextEditorTool) then
  begin

    try
      ToolCall.Response := TextEditorTool.Execute(ToolCall.Arguments);
    finally
    end;

    Exit;
  end;

  // 2. Verificar Componente Externo (AiFunctions)
  if Assigned(AiFunctions) and AiFunctions.DoCallFunction(ToolCall) then
  begin
    Exit;
  end;

  // 3. Evento de Usuario
  if Assigned(FOnCallToolFunction) then
    FOnCallToolFunction(Self, ToolCall);
end;

function TAiClaudeChat.DownLoadFile(aMediaFile: TAiMediaFile): String;
var
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  Headers: TNetHeaders;
  MemStream: TMemoryStream;
begin
  Result := '';
  if (not Assigned(aMediaFile)) or (aMediaFile.IdFile.IsEmpty) then
    raise Exception.Create('File ID is missing');

  Client := TNetHTTPClient.Create(Nil);
  MemStream := TMemoryStream.Create;
  try
    // Usamos GetFileHeaders que ya incluye 'anthropic-beta: files-api-2025-04-14'
    Headers := GetFileHeaders;

    // Endpoint: https://api.anthropic.com/v1/files/{file_id}/content
    Res := Client.Get(Url + 'files/' + aMediaFile.IdFile + '/content', MemStream, Headers);

    if Res.StatusCode = 200 then
    begin
      // Guardar en el objeto MediaFile
      MemStream.Position := 0;
      aMediaFile.Content.CopyFrom(MemStream, 0);
      aMediaFile.Content.Position := 0;

      // Intentar determinar la extensión si no la tiene
      if aMediaFile.FileName.IsEmpty then
        aMediaFile.FileName := aMediaFile.IdFile + '.bin'; // Default

      Result := aMediaFile.IdFile;
    end
    else
      raise Exception.CreateFmt('Download Error: %d - %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    MemStream.Free;
    Client.Free;
  end;
end;

function TAiClaudeChat.DeleteFile(aMediaFile: TAiMediaFile): String;
var
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  jObj: TJSONObject;
begin
  Client := TNetHTTPClient.Create(Nil);
  try
    Res := Client.Delete(Url + 'files/' + aMediaFile.IdFile, nil, GetFileHeaders);
    if Res.StatusCode = 200 then
    begin
      jObj := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      Result := jObj.GetValue<string>('id');
      jObj.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TAiClaudeChat.CheckFileState(aMediaFile: TAiMediaFile): String;
var
  Tmp: TAiMediaFile;
begin
  Tmp := RetrieveFile(aMediaFile.IdFile);
  if Assigned(Tmp) then
  begin
    Result := Tmp.IdFile;
    Tmp.Free;
  end
  else
    Result := '';
end;

function TAiClaudeChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer): String;
begin
  Result := UploadFile(aMediaFile);
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiClaudeChat);

end.
