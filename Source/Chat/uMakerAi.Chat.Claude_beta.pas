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
//https://platform.claude.com/docs/en/api/beta/messages/create  //api de la última implementación {/beta/}

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

  // Headers Beta (Constantes)
  BETA_HDR_TOOLS = 'tools-2024-05-16';
  BETA_HDR_FILES = 'files-api-2025-04-14';
  BETA_HDR_MEMORY = 'context-management-2025-06-27';
  BETA_HDR_THINKING = 'interleaved-thinking-2025-05-14';
  BETA_HDR_CODE = 'code-execution-2025-08-25';

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
  HeaderVal: string;
begin
  // 1. Headers Base (IMPORTANTE: La versión debe ser 2023-06-01 para que las betas funcionen encima)
  Result := [
    TNetHeader.Create('x-api-key', ApiKey),
    TNetHeader.Create('anthropic-version', '2023-06-01'),
    TNetHeader.Create('content-type', 'application/json')
  ];

  BetaFeatures := TList<string>.Create;
  try
    // -------------------------------------------------------------------------
    // ACTIVACIÓN DE BETAS (HEADERS) - CRÍTICO PARA EVITAR ERROR "EXTRA INPUTS"
    // -------------------------------------------------------------------------

    // 1. OBLIGATORIO: Activar herramientas modernas (base de structured output)
    // Según tu doc, esta es la versión más reciente para herramientas
    BetaFeatures.Add('token-efficient-tools-2025-02-19');

    // 2. OBLIGATORIO: Activar capacidades de Output extendidas
    // Es muy probable que 'output_format' dependa de esta beta o la anterior
    BetaFeatures.Add('output-128k-2025-02-19');

    // 3. Files API (Para subir archivos, imagenes, pdfs como 'file_id')
    // Lo activamos siempre para evitar errores si se sube un archivo
    BetaFeatures.Add('files-api-2025-04-14');

    // 4. Memory & Context Management (Solo si se usa, pero no hace daño tenerlo)
    if FEnableMemory or (not FContextConfig.IsEmpty) then
      BetaFeatures.Add('context-management-2025-06-27');

    // 5. Code Execution (Si está habilitado en ChatMediaSupports)
    if tcm_code_interpreter in ChatMediaSupports then
      BetaFeatures.Add('code-execution-2025-08-25');

    // 6. Thinking / Razonamiento (Si está habilitado)
    if FEnableThinking or (ThinkingLevel <> tlDefault) then
      BetaFeatures.Add('interleaved-thinking-2025-05-14');

    // 7. Prompt Caching
    if FCacheSystemPrompt then
      BetaFeatures.Add('prompt-caching-2024-07-31');

    // 8. Computer Use
    if tcm_ComputerUse in ChatMediaSupports then
      BetaFeatures.Add('computer-use-2025-01-24');

    // -------------------------------------------------------------------------
    // CONSTRUIR HEADER
    // -------------------------------------------------------------------------
    if BetaFeatures.Count > 0 then
    begin
      // Unimos las betas separadas por comas
      HeaderVal := string.Join(',', BetaFeatures.ToArray);
      Result := Result + [TNetHeader.Create('anthropic-beta', HeaderVal)];
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

  // Variables para nuevas funcionalidades (API 2025)
  jOutputConfig, jOutputFormat, jMetaData: TJSONObject;
  jSchemaParsed: TJSONValue;
begin
  // Heredamos el User del padre si no está definido
  if User = '' then
    User := 'user';

  // ---------------------------------------------------------------------------
  // AUTO-UPLOAD: Subida automática de archivos a Files API
  // ---------------------------------------------------------------------------
  for LMsg in Self.Messages do
  begin
    // Solo nos interesan los archivos que envía el usuario y que no estén procesados
    if (LMsg.Role = 'user') and (LMsg.MediaFiles.Count > 0) then
    begin
      for LMedia in LMsg.MediaFiles do
      begin
        // Si el archivo aún no tiene ID (no ha sido subido)
        if LMedia.IdFile.IsEmpty then
        begin
          // Condición de subida automática:
          // 1. Si Code Interpreter está activo (necesita file_id).
          // 2. O SI el archivo NO es PDF ni Imagen (Excel, CSV, etc obligan a usar Files API).
          if (tcm_code_interpreter in ChatMediaSupports) or (not(LMedia.FileCategory in [Tfc_Image, Tfc_pdf])) then
          begin
            try
              // UploadFile llena automáticamente LMedia.IdFile
              UploadFile(LMedia);
            except
              // Permitimos continuar; si falla la subida, quizás falle el prompt, pero no bloqueamos todo.
              on E: Exception do
                DoError('Auto-upload warning for ' + LMedia.FileName + ': ' + E.Message, E);
            end;
          end;
        end;
      end;
    end;
  end;
  // ---------------------------------------------------------------------------

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'claude-3-7-sonnet-20250219'; // Default seguro si no hay modelo

  // ---------------------------------------------------------------------------
  // LOGIC: Thinking (Razonamiento)
  // Sincronizamos la propiedad genérica ThinkingLevel con el flag interno de Claude
  // ---------------------------------------------------------------------------
  if ThinkingLevel <> tlDefault then
  begin
    FEnableThinking := True;
    // Si usamos thinking, aseguramos un max_tokens suficiente
    if Max_tokens < 4096 then
       Max_tokens := 8192;
    // Thinking fuerza temperatura 1.0
    Self.Temperature := 1.0;
  end;

  LAsincronico := Self.Asynchronous;
  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSONObject.Create;
  Lista := TStringList.Create;
  Try
    AJSONObject.AddPair('model', LModel);

    // -------------------------------------------------------------------------
    // SYSTEM PROMPT (con soporte para Prompt Caching)
    // -------------------------------------------------------------------------
    SystemPrompt := Self.PrepareSystemMsg;
    if SystemPrompt <> '' then
    begin
      // Si el caché de sistema está activo
      if FCacheSystemPrompt then
      begin
        var jSysArr := TJSonArray.Create;
        var jSysBlock := TJSONObject.Create;

        jSysBlock.AddPair('type', 'text');
        jSysBlock.AddPair('text', SystemPrompt);

        var jCache := TJSONObject.Create;
        jCache.AddPair('type', 'ephemeral');
        jSysBlock.AddPair('cache_control', jCache);

        jSysArr.Add(jSysBlock);
        AJSONObject.AddPair('system', jSysArr);
      end
      else
      begin
        // Formato estándar
        AJSONObject.AddPair('system', SystemPrompt);
      end;
    end;

    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));
    AJSONObject.AddPair('messages', GetMessages); // GetMessages gestiona el contenido de los mensajes

    // =========================================================================
    // NUEVAS FUNCIONALIDADES (API 2025)
    // =========================================================================

    // 1. JSON OUTPUT FORMAT / STRUCTURED OUTPUTS
    // Verifica si la propiedad heredada Response_format indica JSON o Schema
    //La api no lo soporta todavía, se habilitará cuando esté diponible en la api
    {
    if (Response_format in [tiaChatRfJson, tiaChatRfJsonSchema]) then
    begin
      jOutputFormat := TJSONObject.Create;
      jOutputFormat.AddPair('type', 'json_schema');

      // Intentar leer el esquema desde la propiedad heredada JsonSchema (TStrings)
      if (JsonSchema.Text <> '') then
      begin
        try
          jSchemaParsed := TJSONObject.ParseJSONValue(JsonSchema.Text);
          if Assigned(jSchemaParsed) and (jSchemaParsed is TJSONObject) then
            jOutputFormat.AddPair('schema', jSchemaParsed as TJSONObject)
          else
          begin
            if Assigned(jSchemaParsed) then jSchemaParsed.Free;
            // Esquema fallback
            var jGen := TJSONObject.Create;
            jGen.AddPair('type', 'object');
            jOutputFormat.AddPair('schema', jGen);
          end;
        except
           // Fallback en error de parseo
           var jGen := TJSONObject.Create;
           jGen.AddPair('type', 'object');
           jOutputFormat.AddPair('schema', jGen);
        end;
      end
      else
      begin
        // Si se pide JSON pero no hay esquema definido, enviamos Object genérico
        var jGen := TJSONObject.Create;
        jGen.AddPair('type', 'object');
        jOutputFormat.AddPair('schema', jGen);
      end;

      AJSONObject.AddPair('output_format', jOutputFormat);
    end;
    }

    // 2. OUTPUT CONFIG (Reasoning Effort)
    // Mapea la propiedad heredada ThinkingLevel al nuevo estándar de Claude
    if (ThinkingLevel <> tlDefault) then
    begin
      jOutputConfig := TJSONObject.Create;
      case ThinkingLevel of
        tlLow:    jOutputConfig.AddPair('effort', 'low');
        tlMedium: jOutputConfig.AddPair('effort', 'medium');
        tlHigh:   jOutputConfig.AddPair('effort', 'high');
      end;
      AJSONObject.AddPair('output_config', jOutputConfig);
    end;

    // 3. SERVICE TIER (Propiedad nueva de TAiClaudeChat)
    if (FServiceTier <> '') then
      AJSONObject.AddPair('service_tier', FServiceTier);

    // 4. METADATA (User ID)
    // Mapea la propiedad heredada User a metadata.user_id
    if (Self.User <> '') and (Self.User <> 'user') then
    begin
      jMetaData := TJSONObject.Create;
      jMetaData.AddPair('user_id', Self.User);
      AJSONObject.AddPair('metadata', jMetaData);
    end;

    // =========================================================================
    // CONFIGURACIONES ESPECÍFICAS Y LEGACY
    // =========================================================================

    // Thinking (Modo Manual Budget)
    // Solo lo añadimos si NO se usó OutputConfig (ThinkingLevel), para evitar conflicto.
    if (FEnableThinking) and (ThinkingLevel = tlDefault) then
    begin
      var jThink := TJSONObject.Create;
      jThink.AddPair('type', 'enabled');
      jThink.AddPair('budget_tokens', TJSONNumber.Create(FThinkingBudget));
      AJSONObject.AddPair('thinking', jThink);
    end;

    // Context Management (Garbage Collection)
    if not FContextConfig.IsEmpty then
    begin
      var jContext := FContextConfig.ToJSONObject;
      if Assigned(jContext) then
        AJSONObject.AddPair('context_management', jContext);
    end;

    // -------------------------------------------------------------------------
    // TOOLS CONSTRUCTION
    // -------------------------------------------------------------------------
    jArrTools := TJSonArray.Create;

    // 1. User Defined Tools (Desde la propiedad Tools del componente)
    if Tool_Active and (Trim(GetTools(TToolFormat.tfClaude).Text) <> '') then
    begin
      var jUserTools := GetToolJson(TToolFormat.tfClaude);
      if Assigned(jUserTools) then
      begin
        for var Val in jUserTools do
          jArrTools.Add(Val.Clone as TJSONObject);
      end;
    end;

    // 2. Native: Web Search
    if tcm_WebSearch in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'web_search_20250305');
      JTools.AddPair('name', 'web_search');
      // Opciones adicionales si existen en WebSearchParams
      jArrTools.Add(JTools);
    end;

    // 3. Native: Code Execution
    if tcm_code_interpreter in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'code_execution_20250825');
      JTools.AddPair('name', 'code_execution');
      jArrTools.Add(JTools);
    end;

    // 4. Native: Memory
    if FEnableMemory then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'memory_20250818');
      JTools.AddPair('name', 'memory');
      jArrTools.Add(JTools);
    end;

    // 5. Native: Text Editor
    if tcm_TextEditor in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'text_editor_20250728');
      JTools.AddPair('name', 'str_replace_based_edit_tool');
      jArrTools.Add(JTools);
    end;

    // 6. Native: Computer Use
    if tcm_ComputerUse in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'computer_20241022');
      JTools.AddPair('name', 'computer');
      // Valores por defecto requeridos por la API
      JTools.AddPair('display_width_px', TJSONNumber.Create(1024));
      JTools.AddPair('display_height_px', TJSONNumber.Create(768));
      jArrTools.Add(JTools);
    end;

    // 7. Native: Bash / Shell
    if tcm_Shell in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'bash_20250124');
      JTools.AddPair('name', 'bash');
      jArrTools.Add(JTools);
    end;

    // Inyectar herramientas al JSON principal
    if jArrTools.Count > 0 then
    begin
      AJSONObject.AddPair('tools', jArrTools);

      // Tool Choice (Auto, Any, Tool)
      if (Trim(Tool_choice) <> '') then
      begin
        try
          jToolChoice := TJSONObject.ParseJSONValue(Tool_choice) as TJSONObject;
          if Assigned(jToolChoice) then
            AJSONObject.AddPair('tool_choice', TJSONObject(jToolChoice.Clone));
        except
          // Si es un string simple como 'auto' o 'any'
          // Claude requiere un objeto para 'any' o 'tool', pero 'auto' es default implícito.
          // Aquí tratamos de adaptarlo, pero idealmente Tool_choice ya debería ser un JSON string válido para Claude.
          if (Tool_choice = 'auto') or (Tool_choice = 'any') then
          begin
             var jSimpleChoice := TJSONObject.Create;
             jSimpleChoice.AddPair('type', Tool_choice);
             AJSONObject.AddPair('tool_choice', jSimpleChoice);
          end
          else
             AJSONObject.AddPair('tool_choice', Tool_choice);
        end;
      end;
    end
    else
      jArrTools.Free;

    // -------------------------------------------------------------------------
    // STANDARD PARAMETERS
    // -------------------------------------------------------------------------

    // Temperatura: Thinking requiere 1.0 (o null).
    if not FEnableThinking then
    begin
      // Solo enviamos temperatura si fue modificada del default (usualmente 0 o 1)
      if Self.Temperature <> 0 then
        AJSONObject.AddPair('temperature', TJSONNumber.Create(Self.Temperature));
    end
    else
      AJSONObject.AddPair('temperature', TJSONNumber.Create(1.0));

    // Top P (Generalmente incompatible con Thinking)
    //if not FEnableThinking then
    //  if Top_p <> 0 then
    //    AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    // Stop Sequences
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      jArrStop := TJSonArray.Create;
      for I := 0 to Lista.Count - 1 do
        jArrStop.Add(Lista[I]);
      AJSONObject.AddPair('stop_sequences', jArrStop);
    end;

    // Stream
    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    // -------------------------------------------------------------------------
    // SERIALIZACIÓN FINAL
    // -------------------------------------------------------------------------
    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSON));
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]); // Limpieza de escapes
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

procedure TAiClaudeChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
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
    // Usamos TryGetValue para evitar fallos si el JSON number no se convierte directo
    if not uso.TryGetValue<Integer>('input_tokens', aPrompt_tokens) then
      aPrompt_tokens := 0;

    if not uso.TryGetValue<Integer>('output_tokens', aCompletion_tokens) then
      aCompletion_tokens := 0;

    // Claude NO envía 'total_tokens', debemos sumarlo manualmente
    aTotal_tokens := aPrompt_tokens + aCompletion_tokens;
  end
  else
  begin
    aPrompt_tokens := 0;
    aCompletion_tokens := 0;
    aTotal_tokens := 0;
  end;

  // 3. Acumular en las propiedades GLOBALES del componente (Estadísticas de la sesión)
  Self.Prompt_tokens := Self.Prompt_tokens + aPrompt_tokens;
  Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
  Self.Total_tokens := Self.Total_tokens + aTotal_tokens;

  // 4. Asignar al MENSAJE DE RESPUESTA (ResMsg)
  // Esto es crucial para que el mensaje devuelto tenga la info de consumo
  ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aPrompt_tokens;
  ResMsg.Completion_tokens := ResMsg.Completion_tokens + aCompletion_tokens;
  ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;

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

    {
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
    }

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
{$IFDEF MSWINDOWS}
                            sPath := StringReplace(sPath, '/', '\', [rfReplaceAll]);
{$ENDIF}
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

{ function TAiClaudeChat.GetMessages: TJSonArray;
  var
  LMessage: TAiChatMessage;
  LMessageObj, LPartObj, LSourceObj, LThinkingObj: TJSONObject;
  LContentArray: TJSonArray;
  LMediaFile: TAiMediaFile;
  MediaArr: TAiMediaFilesArray;
  bHasContent: Boolean;
  IsCodeExecutionEnabled: Boolean;

  begin
  Result := TJSonArray.Create;
  IsCodeExecutionEnabled := tcm_code_interpreter in ChatMediaSupports;

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
  // --- FIX: Inyectar Thinking Block ---
  // Si el mensaje tiene pensamiento Y firma, debe ir PRIMERO.
  // Claude requiere esto obligatoriamente si luego hay un tool_use.
  if (FEnableThinking) and (LMessage.ThinkingContent <> '') and (LMessage.ThinkingSignature <> '') then
  begin
  LThinkingObj := TJSONObject.Create;
  LThinkingObj.AddPair('type', 'thinking');
  LThinkingObj.AddPair('thinking', LMessage.ThinkingContent);
  LThinkingObj.AddPair('signature', LMessage.ThinkingSignature);
  LContentArray.Add(LThinkingObj);
  end;

  // A. Texto visible
  // Agregamos texto si existe, PERO si es un mensaje solo de tools,
  // LMessage.Prompt podría estar vacío o contener el texto previo.
  if not LMessage.Prompt.IsEmpty then
  begin
  LPartObj := TJSONObject.Create;
  LPartObj.AddPair('type', 'text');
  LPartObj.AddPair('text', LMessage.Prompt);

  // --- NEW: Cache Control Injection ---
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

  // B. Bloques de Uso de Herramientas (Tool Use)
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
  // Si falla el parseo del tool_calls, intentamos recuperar algo o ignoramos
  end;
  end;
  end

  // -------------------------------------------------------------------------
  // CASO 3: Mensaje Estándar de Usuario (Texto + Multimedia)
  // -------------------------------------------------------------------------
  else
  begin
  bHasContent := False;

  // Texto
  if not LMessage.Prompt.IsEmpty then
  begin
  LPartObj := TJSONObject.Create;
  LPartObj.AddPair('type', 'text');
  LPartObj.AddPair('text', LMessage.Prompt);
  LContentArray.Add(LPartObj);
  bHasContent := True;
  end;

  // Archivos adjuntos
  MediaArr := LMessage.MediaFiles.GetMediaList(Self.NativeInputFiles, False);


  for LMediaFile in MediaArr do
  begin
  LPartObj := TJSONObject.Create;

  // A. Definir el tipo de bloque
  case LMediaFile.FileCategory of
  Tfc_Image:
  LPartObj.AddPair('type', 'image');
  else
  // PDF, Texto, CSV, JSON -> Todos van como 'document'
  LPartObj.AddPair('type', 'document');
  end;

  LSourceObj := TJSONObject.Create;

  // B. Decidir si enviamos REFERENCIA (File API) o VALOR (Base64)
  if not LMediaFile.IdFile.IsEmpty then
  begin
  // --- NUEVO: Usar referencia de Files API (Más eficiente / Code Interpreter) ---
  LSourceObj.AddPair('type', 'file');
  LSourceObj.AddPair('file_id', LMediaFile.IdFile);
  end
  else
  begin
  // --- LEGACY: Enviar contenido en Base64 (Solo imágenes pequeñas o si no se subió) ---
  LSourceObj.AddPair('type', 'base64');
  LSourceObj.AddPair('media_type', LMediaFile.MimeType);
  LSourceObj.AddPair('data', LMediaFile.Base64);
  end;

  LPartObj.AddPair('source', LSourceObj);

  // C. Metadatos extra para Documentos (Citations, Cache, Context)
  if LPartObj.GetValue<string>('type') = 'document' then
  begin
  // Nombre del archivo (ayuda a Claude a identificarlo en el código Python)
  if not LMediaFile.FileName.IsEmpty then
  LPartObj.AddPair('title', LMediaFile.FileName);
  end;

  // Cache Control (Genérico)
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

  // Protección contra contenido vacío
  if not bHasContent then
  begin
  LPartObj := TJSONObject.Create;
  LPartObj.AddPair('type', 'text');
  LPartObj.AddPair('text', ' '); // Espacio vacío para evitar error de API
  LContentArray.Add(LPartObj);
  end;
  end;

  LMessageObj.AddPair('content', LContentArray);
  Result.Add(LMessageObj);
  end;
  end;
}

function TAiClaudeChat.GetMessages: TJSonArray;
var
  LMessage: TAiChatMessage;
  LMessageObj, LPartObj, LSourceObj, LThinkingObj: TJSONObject;
  LContentArray: TJSonArray;
  LMediaFile: TAiMediaFile;
  MediaArr: TAiMediaFilesArray;
  bHasContent: Boolean;
  IsCodeExecutionEnabled: Boolean;
begin
  Result := TJSonArray.Create;

  // Verificamos si el Code Interpreter está activo
  IsCodeExecutionEnabled := tcm_code_interpreter in ChatMediaSupports;

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
      // A. Thinking Block (Prioridad Alta para Claude 4.5/3.7)
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

        // Cache Control
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
          // Ignorar errores de parseo
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

        // Cache Control para texto de usuario
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

      // B. Archivos (MediaFiles)
      MediaArr := LMessage.MediaFiles.GetMediaList(Self.NativeInputFiles, False);

      for LMediaFile in MediaArr do
      begin
        LPartObj := TJSONObject.Create;

        // --- SUB-CASO 3.1: IMÁGENES ---
        // Las imágenes siempre van como bloque 'image', ya sea por FileID o Base64
        if LMediaFile.FileCategory = Tfc_Image then
        begin
          LPartObj.AddPair('type', 'image');
          LSourceObj := TJSONObject.Create;

          if not LMediaFile.IdFile.IsEmpty then
          begin
            LSourceObj.AddPair('type', 'file'); // Files API
            LSourceObj.AddPair('file_id', LMediaFile.IdFile);
          end
          else
          begin
            LSourceObj.AddPair('type', 'base64'); // Legacy Base64
            LSourceObj.AddPair('media_type', LMediaFile.MimeType);
            LSourceObj.AddPair('data', LMediaFile.Base64);
          end;
          LPartObj.AddPair('source', LSourceObj);
        end

        // --- SUB-CASO 3.2: CONTAINER UPLOAD (Code Interpreter + Data Files) ---
        // Si es Code Interpreter Y el archivo está subido (IdFile) Y NO es imagen.
        // Esto es obligatorio para Excel, CSV, JSON, etc.
        else if IsCodeExecutionEnabled and (not LMediaFile.IdFile.IsEmpty) then
        begin
          LPartObj.AddPair('type', 'container_upload');
          LPartObj.AddPair('file_id', LMediaFile.IdFile);

          // Nota: 'container_upload' NO soporta 'source', 'title' ni 'citations'.
          // Es un montaje directo en el sistema de archivos del sandbox.
        end

        // --- SUB-CASO 3.3: DOCUMENTOS (PDFs / Texto para Lectura) ---
        // Si no es imagen y no usamos Container Upload (o no tenemos ID), vamos por Document.
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
            // Fallback Base64 (Solo PDF soporta esto oficialmente)
            LSourceObj.AddPair('type', 'base64');
            LSourceObj.AddPair('media_type', LMediaFile.MimeType);
            LSourceObj.AddPair('data', LMediaFile.Base64);
          end;

          LPartObj.AddPair('source', LSourceObj);

          // Metadatos exclusivos de Document Block
          if not LMediaFile.FileName.IsEmpty then
            LPartObj.AddPair('title', LMediaFile.FileName);

          // Activar Citations (RAG) solo en bloques de documento
          // Todavía no se ha implementado
          { if Self.EnableCitations then
            begin
            var jCit := TJSONObject.Create;
            jCit.AddPair('enabled', TJSONBool.Create(True));
            LPartObj.AddPair('citations', jCit);
            end;
          }
        end;

        // Cache Control (Aplica a todos los bloques de medios)
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

      // Protección contra contenido vacío
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
