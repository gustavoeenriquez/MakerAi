// MIT License
//
// Copyright (c) <year> <copyright holders>
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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// -----------------------------------------------------------------------------
// CITATIONS (RAG Nativo) - Estado de implementaci?n
// -----------------------------------------------------------------------------
// 1. Core: TAiMediaFile.EnableCitations, Title, Context  [COMPLETADO]
// 2. Core: TAiMsgCitation, TAiMsgCitations en TAiChatMessage  [COMPLETADO]
// 3. Claude.GetMessages: Inyectar "citations": {"enabled": true}  [COMPLETADO]
// 4. Claude.ProcessStreamChunk: Captura citations_delta  [YA FUNCIONABA]
// 5. Claude.ParseChat: Parsear char_location, page_location, etc.  [COMPLETADO]
// Ref: https://docs.anthropic.com/en/docs/build-with-claude/citations
// -----------------------------------------------------------------------------
// ------ Herramientas que no se implementar?n por ahora --------------------
// 1. https://platform.claude.com/docs/es/agents-and-tools/tool-use/code-execution-tool
// 2. https://platform.claude.com/docs/es/agents-and-tools/tool-use/fine-grained-tool-streaming
// 3.




// https://platform.claude.com/docs/en/intro
// https://platform.claude.com/docs/en/api/beta/messages/create  //api de la ?ltima implementaci?n {/beta/}

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
  uMakerAi.Utils.CodeExtractor, uMakerAi.Chat.Messages;

type

  // --- Clases para Gesti?n de Contexto (Context Editing) ---

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
    FStreamContentBlocks: TObjectDictionary<Integer, TClaudeStreamContentBlock>;
    FStreamBuffer: TStringBuilder;
    FStreamLastEventType: string;

    // Nuevas funcionalidades
    FEnableMemory: Boolean;
    FEnableThinking: Boolean;
    FThinkingBudget: Integer;
    FContextConfig: TClaudeContextConfig;
    FCacheSystemPrompt: Boolean;
    FCacheTTL: String;
    // Fase ago 2026
    FFastMode: Boolean;             // speed:"fast" — solo opus-5/opus-4-8
    FEnableCompaction: Boolean;     // compaction server-side (beta)
    FRefusalFallbackModel: string;  // fallbacks server-side ante refusal
    // Bloques compaction recibidos, por mensaje assistant: deben reenviarse
    // integros para que el API reemplace el historial compactado
    FCompactionBlocks: TDictionary<TAiChatMessage, string>;
    FCacheCount: Integer; // breakpoints cache_control usados en el request actual (max 4 en la API)
    FCacheCtxActive: Boolean; // cacheo de contexto activo en este request (system/tools/ultimo turno)
    FServiceTier: String;

    function GetToolJson(aToolFormat: TToolFormat): TJSonArray;
    function GetDynamicHeaders: TNetHeaders; // Construye headers Beta din?micamente
    function GetFileHeaders: TNetHeaders;
    procedure ClearStreamState;
    procedure ProcessStreamChunk(const AChunk: string);

    procedure SetEnableMemory(const Value: Boolean);
    procedure SetEnableThinking(const Value: Boolean);
    procedure SetThinkingBudget(const Value: Integer);
    procedure TranslateClaudeComputerArgs(ToolCall: TAiToolsFunction);

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

    // --- Gesti?n de Archivos (File API) ---
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

    // M?todo f?cil para configurar la limpieza autom?tica de contexto
    // TriggerTokens: A partir de cu?ntos tokens de entrada se activa la limpieza (ej. 20000)
    // KeepLast: Cu?ntas interacciones de herramientas recientes conservar (ej. 3)
    procedure ConfigureAutoContextClearing(TriggerTokens: Integer; KeepLast: Integer = 3);

    // Acceso a la configuraci?n de contexto
    property ContextConfig: TClaudeContextConfig read FContextConfig;

    // Permite cachear el System Prompt para ahorrar costos en instrucciones largas
    property CacheSystemPrompt: Boolean read FCacheSystemPrompt write FCacheSystemPrompt;
    property CacheTTL: String read FCacheTTL write FCacheTTL;

  Published
    property EnableMemory: Boolean read FEnableMemory write SetEnableMemory default False;
    property EnableThinking: Boolean read FEnableThinking write SetEnableThinking default False;
    property ThinkingBudget: Integer read FThinkingBudget write SetThinkingBudget default 1024;
    Property ServiceTier: String read FServiceTier write FServiceTier;
    // Fast mode (research preview): ~2.5x mas tokens/segundo a 2x precio.
    // Solo claude-opus-5 / claude-opus-4-8 (en otros modelos se ignora)
    property FastMode: Boolean read FFastMode write FFastMode default False;
    // Compaction server-side (beta): al acercarse al limite de contexto el
    // API resume el historial antiguo automaticamente. El driver preserva y
    // reenvia los bloques compaction en los turnos siguientes
    property EnableCompaction: Boolean read FEnableCompaction
      write FEnableCompaction default False;
    // Si los clasificadores declinan (stop_reason refusal en opus-5/fable),
    // el API reintenta la MISMA peticion en este modelo dentro de la misma
    // llamada (ej: 'claude-opus-4-8'). Vacio = sin fallback
    property RefusalFallbackModel: string read FRefusalFallbackModel
      write FRefusalFallbackModel;
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
  // Gesti?n de Contexto / Memoria (se mantiene)
  BETA_HDR_MEMORY = 'context-management-2025-06-27';
  // Thinking (se mantiene)
  BETA_HDR_THINKING = 'interleaved-thinking-2025-05-14';
  // Fase ago 2026
  BETA_HDR_FASTMODE = 'fast-mode-2026-02-01';             // speed:"fast" (opus-5/4.8, research preview)
  BETA_HDR_COMPACT  = 'compact-2026-01-12';               // compaction server-side
  BETA_HDR_FALLBACK = 'server-side-fallback-2026-06-01';  // fallbacks ante refusal
  // Code Execution (Actualizado seg?n lista de headers de la doc)
  BETA_HDR_CODE = 'code-execution-2025-05-22';
  // Computer Use (Actualizado a la ?ltima versi?n disponible en doc)
  // computer_20251124: Opus 4.8/4.7/4.6, Sonnet 4.6, Opus 4.5 (a?ade acci?n zoom)
  BETA_HDR_COMPUTER = 'computer-use-2025-11-24';
  // PDFs (Nuevo: Para asegurar soporte nativo si se env?a base64)
  BETA_HDR_PDFS = 'pdfs-2024-09-25';
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
  Params.Add('Model=claude-haiku-4-5-20251001');
  Params.Add('Max_Tokens=4096');
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
    // Headers espec?ficos para Batches
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
  // Vía propiedad (no FClient directo) para que FResponseTimeOut quede consistente.
  // 300s: con code_execution nativo Anthropic ejecuta server-side y no envía ni un
  // byte hasta terminar (generar un Office tarda 50-70s; 60s cortaba a la mitad).
  ResponseTimeOut := 300000;

  FStreamContentBlocks := TObjectDictionary<Integer, TClaudeStreamContentBlock>.Create([doOwnsValues]);
  FStreamBuffer := TStringBuilder.Create;
  FStreamResponseMsg := nil;
  FContextConfig := TClaudeContextConfig.Create;
  FCompactionBlocks := TDictionary<TAiChatMessage, string>.Create;

  // Valores por defecto
  Model := 'claude-haiku-4-5-20251001';
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
  FCompactionBlocks.Free;
  inherited;
end;

// --- Property Setters ---

procedure TAiClaudeChat.SetEnableMemory(const Value: Boolean);
begin
  FEnableMemory := Value;
  if Value then
    ModelConfig.ModelCaps := ModelConfig.ModelCaps + [cap_Memory]
  else
    ModelConfig.ModelCaps := ModelConfig.ModelCaps - [cap_Memory];
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

// Familias de modelos segun la superficie de thinking del API (ago 2026):
// - Adaptive-only (opus 4.7/4.8, familia 5: opus/sonnet/fable/mythos):
//   budget_tokens y temperature/top_p/top_k devuelven 400. Thinking se pide
//   con {type:"adaptive"} y la profundidad con output_config.effort.
// - 4.6 (opus/sonnet): adaptive recomendado (budget deprecado pero funcional),
//   sampling permitido, effort GA.
// - Legacy (<=4.5, haiku): thinking {enabled, budget_tokens} clasico.
function IsClaudeAdaptiveOnly(const AModel: string): Boolean;
begin
  Result := AModel.StartsWith('claude-opus-4-7') or
            AModel.StartsWith('claude-opus-4-8') or
            AModel.StartsWith('claude-opus-5') or
            AModel.StartsWith('claude-sonnet-5') or
            AModel.StartsWith('claude-fable') or
            AModel.StartsWith('claude-mythos');
end;

function IsClaude46(const AModel: string): Boolean;
begin
  Result := AModel.StartsWith('claude-opus-4-6') or
            AModel.StartsWith('claude-sonnet-4-6');
end;

// Fast mode (speed:"fast") solo existe en opus-5 y opus-4-8
function IsClaudeFastCapable(const AModel: string): Boolean;
begin
  Result := AModel.StartsWith('claude-opus-5') or
            AModel.StartsWith('claude-opus-4-8');
end;

// Mensajes {role:"system"} dentro de messages[] (mid-conversation, preservan
// el prompt cache): opus-5, opus-4-8, fable, mythos. NO sonnet-5
function IsClaudeMidSystemCapable(const AModel: string): Boolean;
begin
  Result := AModel.StartsWith('claude-opus-5') or
            AModel.StartsWith('claude-opus-4-8') or
            AModel.StartsWith('claude-fable') or
            AModel.StartsWith('claude-mythos');
end;

function TAiClaudeChat.GetDynamicHeaders: TNetHeaders;
var
  BetaFeatures: TList<string>;
begin
  // Headers base obligatorios
  Result := [TNetHeader.Create('x-api-key', ApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION), TNetHeader.Create('content-type', 'application/json')];

  BetaFeatures := TList<string>.Create;
  try
    // 1. Tools (General) - Se a?ade si hay herramientas activas
    if Tool_Active then
      BetaFeatures.Add(BETA_HDR_TOOLS);

    // 2. Memory & Context
    if FEnableMemory or (not FContextConfig.IsEmpty) then
      BetaFeatures.Add(BETA_HDR_MEMORY);

    // 3. Code Interpreter (Code Execution)
    if cap_CodeInterpreter in ModelConfig.ModelCaps then
      BetaFeatures.Add(BETA_HDR_CODE);

    // 4. Computer Use (NUEVO)
    if cap_ComputerUse in ModelConfig.ModelCaps then
      BetaFeatures.Add(BETA_HDR_COMPUTER);

    // 5. Thinking: el header interleaved-thinking solo aplica al camino legacy
    // con budget_tokens (<=4.5). En 4.6+ el thinking adaptativo integra el
    // interleaving automaticamente y el header sobra.
    var LBaseModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
    if (FEnableThinking or (ModelConfig.ThinkingLevel <> tlDefault)) and
       (not IsClaudeAdaptiveOnly(LBaseModel)) and (not IsClaude46(LBaseModel)) then
      BetaFeatures.Add(BETA_HDR_THINKING);

    // 5b. Fase ago 2026: fast mode, compaction y fallbacks (betas)
    if FFastMode and IsClaudeFastCapable(LBaseModel) then
      BetaFeatures.Add(BETA_HDR_FASTMODE);
    if FEnableCompaction then
      BetaFeatures.Add(BETA_HDR_COMPACT);
    if FRefusalFallbackModel <> '' then
      BetaFeatures.Add(BETA_HDR_FALLBACK);

    // 6. Prompt Caching: GA, ya no requiere beta header (antes 'prompt-caching-2024-07-31').

    // 7. PDFs
    // Agregamos soporte expl?cito para PDFs
    if (cap_Pdf in ModelConfig.ModelCaps) then
      BetaFeatures.Add(BETA_HDR_PDFS);

    // 8. Files API (CR?TICO)
    // Siempre anunciamos soporte de Files para el Auto-Upload
    BetaFeatures.Add(BETA_HDR_FILES);

    // Structured Outputs: Se activa si pedimos JSON Schema expl?cito
    // Opcionalmente tambi?n si Tool_Active es true para permitir "strict: true" en tools.
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
  LIsAdaptiveThinking: Boolean;
  SystemPrompt: String;

  // Variables para iteraci?n de mensajes (Auto-Upload)
  LMsg: TAiChatMessage;
  LMedia: TAiMediaFile;

  // Variables para nuevas funcionalidades
  jOutputFormat, jMetaData: TJSONObject;
  jSchemaParsed: TJSONValue;
  LOutputConfig: TJSONObject; // output_config: format (json_schema) y/o effort

  // Variables auxiliares para Thinking
  LActualThinkingBudget: Integer;
  LIs46: Boolean;
begin
  LActualThinkingBudget := 0;
  LOutputConfig := nil;
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
          if (cap_CodeInterpreter in ModelConfig.ModelCaps) or (not(LMedia.FileCategory in [Tfc_Image, Tfc_pdf])) then
          begin
            try
              UploadFile(LMedia);
            except
              on E: Exception do LogDebug('Auto-Upload error: ' + E.Message);
            end;
          end;
        end;
      end;
    end;
  end;

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'claude-haiku-4-5-20251001';

  // Clasificacion por familia (ver IsClaudeAdaptiveOnly / IsClaude46 arriba):
  // adaptive-only = 4.7/4.8/5 (budget y sampling devuelven 400); 4.6 = adaptive
  // recomendado; resto = camino legacy con budget_tokens.
  LIsAdaptiveThinking := IsClaudeAdaptiveOnly(LModel);
  LIs46 := IsClaude46(LModel);

  // ---------------------------------------------------------------------------
  // 2. C?LCULO DE THINKING BUDGET (solo camino legacy <=4.5)
  // ---------------------------------------------------------------------------
  if ModelConfig.ThinkingLevel <> tlDefault then
    FEnableThinking := True;

  if FEnableThinking and (not LIsAdaptiveThinking) and (not LIs46) then
  begin
    // Asignar presupuesto seg?n el nivel elegido o usar el manual
    case ModelConfig.ThinkingLevel of
      tlLow:
        LActualThinkingBudget := 2048; // R?pido, poco profundo
      tlMedium:
        LActualThinkingBudget := 8192; // Est?ndar razonable
      tlHigh:
        LActualThinkingBudget := 16384; // Profundo (Cuidado con costos)
    else
      LActualThinkingBudget := FThinkingBudget; // Valor manual
    end;

    // Validaci?n de m?nimos API (1024)
    if LActualThinkingBudget < 1024 then
      LActualThinkingBudget := 1024;

    // Validaci?n de MaxTokens: Debe ser estrictamente mayor que el budget
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
    // Contador de breakpoints cache_control para este request (la API permite max 4).
    // Orden de prefijo: tools -> system -> messages. Reservamos los slots de system y
    // tools antes de construir messages para que tengan prioridad sobre los mensajes.
    FCacheCount := 0;
    // Caching del contexto estable activo si el usuario puso CacheSystemPrompt (Claude)
    // o el flag portable CacheContext (base).
    var
    LDoCache: Boolean := FCacheSystemPrompt or CacheContext;
    FCacheCtxActive := LDoCache; // visible para GetMessages (auto-cache del ultimo turno)
    SystemPrompt := Self.PrepareSystemMsg;
    if SystemPrompt <> '' then
    begin
      if LDoCache then
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
        if FCacheTTL <> '' then
          jCache.AddPair('ttl', FCacheTTL);
        jSysBlock.AddPair('cache_control', jCache);
        Inc(FCacheCount); // breakpoint de system (cachea tools+system por el orden de prefijo)

        jSysArr.Add(jSysBlock);
        AJSONObject.AddPair('system', jSysArr);
      end
      else
        AJSONObject.AddPair('system', SystemPrompt);
    end;

    // Reserva del slot de tools: si el cacheo esta activo, las definiciones de tools
    // se cachean (breakpoint en el ultimo tool, mas abajo). Se reserva aqui para que
    // los mensajes no consuman ese slot.
    if LDoCache then
      Inc(FCacheCount);

    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));
    AJSONObject.AddPair('messages', GetMessages);

    // 4. JSON OUTPUT (STRUCTURED OUTPUTS)
    if (Response_format = tiaChatRfJsonSchema) then
    begin
      if (JsonSchema.Text <> '') then
      begin
        try
          var
          sShema := StringReplace(JsonSchema.Text, '\n', ' ', [rfReplaceAll]);
          jSchemaParsed := TJSONObject.ParseJSONValue(sShema);
          if Assigned(jSchemaParsed) and (jSchemaParsed is TJSONObject) then
          begin
            var
            jRootSchema := jSchemaParsed as TJSONObject;

            // --- CORRECCI?N AUTOM?TICA PARA CLAUDE ---
            // Claude exige "additionalProperties": false en el nivel ra?z si es type object.
            // Verificamos si es type object y si falta la propiedad, la agregamos.
            if (jRootSchema.GetValue<string>('type') = 'object') and (jRootSchema.GetValue('additionalProperties') = nil) then
            begin
              jRootSchema.AddPair('additionalProperties', TJSONBool.Create(False));
            end;
            // -----------------------------------------

            jOutputFormat := TJSONObject.Create;
            jOutputFormat.AddPair('type', 'json_schema');
            jOutputFormat.AddPair('schema', jRootSchema); // Usamos el objeto ya modificado
            // output_format (top-level) esta deprecado API-wide: el canonico
            // es output_config.format (se agrega al final junto con effort)
            LOutputConfig := TJSONObject.Create;
            LOutputConfig.AddPair('format', jOutputFormat);
          end
          else if Assigned(jSchemaParsed) then
            jSchemaParsed.Free;
        except
          on E: Exception do LogDebug('JSON Schema parse error: ' + E.Message);
        end;
      end;
    end;

    // -------------------------------------------------------------------------
    // 5. THINKING PARAMETERS por familia de modelo
    // -------------------------------------------------------------------------
    if LIsAdaptiveThinking or LIs46 then
    begin
      // Familia 4.6+ : thinking adaptativo (auto-interleaved, sin beta header).
      // La profundidad se pide con output_config.effort (GA desde 4.6).
      // En opus-5/fable el thinking ya viene activo por defecto; enviar
      // {type:"adaptive"} explicito es equivalente e inofensivo.
      if FEnableThinking then
      begin
        var
        jThink := TJSONObject.Create;
        jThink.AddPair('type', 'adaptive');
        AJSONObject.AddPair('thinking', jThink);
      end;

      if ModelConfig.ThinkingLevel <> tlDefault then
      begin
        if not Assigned(LOutputConfig) then
          LOutputConfig := TJSONObject.Create;
        case ModelConfig.ThinkingLevel of
          tlLow:
            LOutputConfig.AddPair('effort', 'low');
          tlMedium:
            LOutputConfig.AddPair('effort', 'medium');
          tlHigh:
            LOutputConfig.AddPair('effort', 'high');
        end;
      end;

      // Sampling: en 4.7+/5 temperature/top_p/top_k devuelven 400 — nunca se
      // envian. En 4.6 siguen permitidos, pero solo sin thinking activo.
      if LIs46 and (not LIsAdaptiveThinking) and (not FEnableThinking) then
      begin
        if Self.Temperature > 0 then
          AJSONObject.AddPair('temperature', TJSONNumber.Create(Self.Temperature))
        Else if Top_p > 0 then
          AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));
        if K > 0 then
          AJSONObject.AddPair('top_k', TJSONNumber.Create(K));
      end;
    end
    else if FEnableThinking then
    begin
      // Camino legacy (<=4.5): thinking manual con budget_tokens
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
      // Modo Est?ndar legacy
      if Self.Temperature > 0 then
        AJSONObject.AddPair('temperature', TJSONNumber.Create(Self.Temperature))
      Else if Top_p > 0 then
        AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

      if K > 0 then
        AJSONObject.AddPair('top_k', TJSONNumber.Create(K));
    end;

    // output_config acumulado (format de structured outputs y/o effort)
    if Assigned(LOutputConfig) then
      AJSONObject.AddPair('output_config', LOutputConfig);

    // 6. METADATA & SERVICE TIER
    if (Self.User <> '') and (Self.User <> 'user') then
    begin
      jMetaData := TJSONObject.Create;
      jMetaData.AddPair('user_id', Self.User);
      AJSONObject.AddPair('metadata', jMetaData);
    end;

    if (FServiceTier <> '') then
      AJSONObject.AddPair('service_tier', FServiceTier);

    // context_management: configuracion del usuario (FContextConfig) y/o
    // compaction server-side (edits.compact_20260112)
    var jContext: TJSONObject := nil;
    if not FContextConfig.IsEmpty then
      jContext := FContextConfig.ToJSONObject;
    if FEnableCompaction then
    begin
      if not Assigned(jContext) then
        jContext := TJSONObject.Create;
      var jEdits: TJSonArray := nil;
      jContext.TryGetValue<TJSonArray>('edits', jEdits);
      if not Assigned(jEdits) then
      begin
        jEdits := TJSonArray.Create;
        jContext.AddPair('edits', jEdits);
      end;
      var jCompact := TJSONObject.Create;
      jCompact.AddPair('type', 'compact_20260112');
      jEdits.Add(jCompact);
    end;
    if Assigned(jContext) then
      AJSONObject.AddPair('context_management', jContext);

    // Fast mode (research preview): solo opus-5/opus-4-8; en otros se ignora
    if FFastMode then
    begin
      if IsClaudeFastCapable(LModel) then
        AJSONObject.AddPair('speed', 'fast')
      else
        LogDebug('FastMode ignorado: ' + LModel + ' no lo soporta');
    end;

    // Fallbacks server-side: si los clasificadores declinan, el API reintenta
    // la misma peticion en el modelo indicado dentro de la misma llamada
    if FRefusalFallbackModel <> '' then
    begin
      var jFallbacks := TJSonArray.Create;
      var jFb := TJSONObject.Create;
      jFb.AddPair('model', FRefusalFallbackModel);
      jFallbacks.Add(jFb);
      AJSONObject.AddPair('fallbacks', jFallbacks);
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
        jUserTools.Free;
      end;
    end;

    if cap_WebSearch in ModelConfig.ModelCaps then
    begin
      JTools := TJSONObject.Create;
      // Desde la familia 4.6 existe web_search_20260209 con filtrado dinamico
      // (el modelo filtra resultados con codigo antes de que entren al
      // contexto); los modelos previos siguen con la variante basica
      if LIsAdaptiveThinking or LIs46 then
        JTools.AddPair('type', 'web_search_20260209')
      else
        JTools.AddPair('type', 'web_search_20250305');
      JTools.AddPair('name', 'web_search');
      jArrTools.Add(JTools);
    end;

    if cap_CodeInterpreter in ModelConfig.ModelCaps then
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

    if cap_TextEditor in ModelConfig.ModelCaps then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'text_editor_20250728');
      JTools.AddPair('name', 'str_replace_based_edit_tool');
      jArrTools.Add(JTools);
    end;

    if cap_ComputerUse in ModelConfig.ModelCaps then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'computer_20251124');
      JTools.AddPair('name', 'computer');
      if Assigned(ChatTools.ComputerUseTool) then
      begin
        JTools.AddPair('display_width_px',  TJSONNumber.Create(ChatTools.ComputerUseTool.ScreenWidth));
        JTools.AddPair('display_height_px', TJSONNumber.Create(ChatTools.ComputerUseTool.ScreenHeight));
        // enable_zoom: solo v?lido en computer_20251124. Permite a Claude
        // ampliar una regi?n del screenshot para leer texto peque?o.
        if ChatTools.ComputerUseTool.EnableZoom then
          JTools.AddPair('enable_zoom', TJSONBool.Create(True));
      end
      else
      begin
        JTools.AddPair('display_width_px',  TJSONNumber.Create(1920));
        JTools.AddPair('display_height_px', TJSONNumber.Create(1080));
      end;
      jArrTools.Add(JTools);
    end;

    if cap_Shell in ModelConfig.ModelCaps then
    begin
      JTools := TJSONObject.Create;
      JTools.AddPair('type', 'bash_20250124');
      JTools.AddPair('name', 'bash');
      jArrTools.Add(JTools);
    end;

    if jArrTools.Count > 0 then
    begin
      AJSONObject.AddPair('tools', jArrTools);

      // Cache de las definiciones de tools: cache_control en el ultimo tool cachea
      // todo el bloque de tools (orden de prefijo tools->system->messages). El slot
      // ya fue reservado en FCacheCount junto al system.
      if LDoCache then
      begin
        var
        jToolCache := TJSONObject.Create;
        jToolCache.AddPair('type', 'ephemeral');
        if FCacheTTL <> '' then
          jToolCache.AddPair('ttl', FCacheTTL);
        (jArrTools.Items[jArrTools.Count - 1] as TJSONObject).AddPair('cache_control', jToolCache);
      end;

      if (Trim(Tool_choice) <> '') then
      begin
        // Anthropic exige tool_choice como OBJETO: {"type":"auto"|"any"|"none"}
        // o {"type":"tool","name":"x"}. La propiedad suele llegar en formato
        // OpenAI: "auto"/"none"/"required" (a veces JSON-quoted como '"auto"')
        // o {"type":"function","function":{"name":"x"}} — aqu? se traduce.
        // Enviarla cruda produce 400 "tool_choice: Input should be an object".
        var LChoiceObj: TJSONObject := nil;
        var LRaw := Trim(Tool_choice);
        var LVal := TJSONObject.ParseJSONValue(LRaw);
        try
          if LVal is TJSONObject then
          begin
            var LType := TJSONObject(LVal).GetValue<string>('type', '');
            if SameText(LType, 'function') then
            begin
              // Formato OpenAI con funci?n espec?fica → {"type":"tool","name":...}
              var LName := '';
              var LFn := TJSONObject(LVal).GetValue<TJSONObject>('function', nil);
              if Assigned(LFn) then
                LName := LFn.GetValue<string>('name', '');
              if LName <> '' then
              begin
                LChoiceObj := TJSONObject.Create;
                LChoiceObj.AddPair('type', 'tool');
                LChoiceObj.AddPair('name', LName);
              end;
            end
            else
              LChoiceObj := TJSONObject(LVal.Clone); // ya viene en formato Anthropic
          end
          else
          begin
            // String simple (con o sin comillas JSON): auto | none | required
            var LWord := LRaw.Replace('"', '').Trim.ToLower;
            if LWord = 'required' then
              LWord := 'any';
            if (LWord = 'auto') or (LWord = 'any') or (LWord = 'none') then
            begin
              LChoiceObj := TJSONObject.Create;
              LChoiceObj.AddPair('type', LWord);
            end;
          end;
        finally
          LVal.Free;
        end;
        if Assigned(LChoiceObj) then
          AJSONObject.AddPair('tool_choice', LChoiceObj);
      end;
    end
    else
      jArrTools.Free;

    // 8. FINALIZACI?N
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      jArrStop := TJSonArray.Create;
      for I := 0 to Lista.Count - 1 do
        jArrStop.Add(Lista[I]);
      AJSONObject.AddPair('stop_sequences', jArrStop);
    end;

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    ApplyExtraBodyParams(AJSONObject);
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
    begin
      // ISSUE #105/#118: el mensaje del assistant se acumula en FStreamResponseMsg durante
      // el stream y se archiva en FMessages al cerrar (message_stop, ver OnInternalReceiveData).
      // En async RunNew libera su propio ResMsg tras el POST, por lo que NO lo referenciamos.
      FStreamResponseMsg := TAiChatMessage.Create('', 'assistant');
    end;

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

      if not Assigned(Res) then
        raise Exception.CreateFmt('Connection failed: no response from %s', [sUrl]);

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
        St.Free
      else
      begin
        if Assigned(FCurrentPostStream) then
          FreeAndNil(FCurrentPostStream);
        FCurrentPostStream := St;
      end;
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

  // Variables para Navegaci?n de Archivos (Code Execution)
  jInnerContent, jResultContent: TJSONObject;
  jResultArray: TJSonArray;
  NewFile: TAiMediaFile;
  ToolUseID, FoundFileName: string;
  ScanItem: TJSONValue;
  ScanObj, InputObj: TJSONObject;

  // Subrutina local: garantiza captura independiente por valor en Delphi 10.4+
  procedure _CreateTask(TC: TAiToolsFunction; AIdx: Integer);
  begin
    TaskList[AIdx] := TTask.Create(
      procedure
      begin
        try
          DoCallFunction(TC);
        except
          on E: Exception do
          begin
            var LErrorMsg := 'Error en herramienta "' + TC.Name + '": ' + E.Message;
            TC.Response := '{"error": "' + E.Message.Replace('"', '''') + '"}';
            TThread.Queue(nil,
              procedure
              begin
                DoError(LErrorMsg, nil);
              end);
          end;
        end;
      end);
    TaskList[AIdx].Start;
  end;

begin
  AskMsg := GetLastMessage;

  // 1. Parse Metadata
  LModel := jObj.GetValue<string>('model', '');
  Role := jObj.GetValue<string>('role', 'assistant');
  StopR := jObj.GetValue<string>('stop_reason', '');

  ResMsg.StopReason := StopR;
  if StopR = 'refusal' then
  begin
    // Los clasificadores de seguridad (opus-5/fable-5) declinan con HTTP 200 +
    // stop_reason:"refusal" y un objeto stop_details {category, explanation}.
    // El content puede venir vacio (pre-output) o parcial (mid-stream).
    ResMsg.IsRefusal := True;
    var LRefusalMsg := 'La peticion fue declinada por los clasificadores de seguridad del modelo';
    var jStopDetails: TJSONObject := nil;
    jObj.TryGetValue<TJSONObject>('stop_details', jStopDetails);
    if Assigned(jStopDetails) then
    begin
      var LCategory := jStopDetails.GetValue<string>('category', '');
      var LExplanation := jStopDetails.GetValue<string>('explanation', '');
      if LCategory <> '' then
        LRefusalMsg := LRefusalMsg + ' (categoria: ' + LCategory + ')';
      if LExplanation <> '' then
        LRefusalMsg := LRefusalMsg + ': ' + LExplanation;
    end;
    DoError(LRefusalMsg, nil);
  end;

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
    ResMsg.Cache_write_tokens := uso.GetValue<Integer>('cache_creation_input_tokens', 0);

    // OJO: input_tokens, cache_read_input_tokens y cache_creation_input_tokens
    // son DISJUNTOS en la API de Anthropic — los cacheados NO estan incluidos en
    // input_tokens (el comentario anterior afirmaba lo contrario y es falso).
    // El prompt real del turno es la suma de los tres; quien facture debe sumarlos.
    aTotal_tokens := aPrompt_tokens + aCompletion_tokens
                   + aCached_tokens + ResMsg.Cache_write_tokens;
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

      // C. Citations (B?squeda Web / RAG / Documentos)
      if jContentItem.TryGetValue<TJSonArray>('citations', jCitationsArr) then
      begin
        for jCitVal in jCitationsArr do
        begin
          jCitObj := jCitVal as TJSONObject;
          var citType := jCitObj.GetValue<string>('type', '');

          // --- Citations de documentos -> TAiMsgCitation ---
          if (citType = 'char_location') or (citType = 'page_location')
             or (citType = 'content_block_location') then
          begin
            var LCitation := TAiMsgCitation.Create;
            LCitation.Text := jCitObj.GetValue<string>('cited_text', '');

            var LSource := TAiCitationSource.Create;
            LSource.SourceType := cstDocument;
            LSource.DataSource.Title := jCitObj.GetValue<string>('document_title', '');
            LSource.DataSource.Id := jCitObj.GetValue<Integer>('document_index', 0).ToString;

            if citType = 'char_location' then
            begin
              LCitation.StartIndex := jCitObj.GetValue<Integer>('start_char_index', 0);
              LCitation.EndIndex := jCitObj.GetValue<Integer>('end_char_index', 0);
            end
            else if citType = 'page_location' then
            begin
              LCitation.StartIndex := jCitObj.GetValue<Integer>('start_page_number', 0);
              LCitation.EndIndex := jCitObj.GetValue<Integer>('end_page_number', 0);
            end
            else if citType = 'content_block_location' then
            begin
              LCitation.StartIndex := jCitObj.GetValue<Integer>('start_block_index', 0);
              LCitation.EndIndex := jCitObj.GetValue<Integer>('end_block_index', 0);
            end;

            LCitation.Sources.Add(LSource);
            ResMsg.Citations.Add(LCitation);
          end

          // --- Citations web (compatibilidad existente) ---
          else if citType = 'web_search_result_location' then
          begin
            if not Assigned(ResMsg.WebSearchResponse) then
              ResMsg.WebSearchResponse := TAiWebSearch.Create;

            SearchItem := TAiWebSearchItem.Create;
            SearchItem.&type := 'web_search_result_location';
            SearchItem.Title := jCitObj.GetValue<string>('title', '');
            SearchItem.Url := jCitObj.GetValue<string>('url', '');
            ResMsg.WebSearchResponse.annotations.Add(SearchItem);

            // Tambi?n agregar a Citations para unificaci?n
            var LCitation := TAiMsgCitation.Create;
            LCitation.Text := jCitObj.GetValue<string>('cited_text', '');
            var LSource := TAiCitationSource.Create;
            LSource.SourceType := cstWeb;
            LSource.DataSource.Title := jCitObj.GetValue<string>('title', '');
            LSource.DataSource.Url := jCitObj.GetValue<string>('url', '');
            LCitation.Sources.Add(LSource);
            ResMsg.Citations.Add(LCitation);
          end;
        end;
      end;

      // C2. Bloque de compaction (beta compact-2026-01-12): se guarda integro
      // asociado al mensaje de respuesta; GetMessages lo reenvia en los
      // turnos siguientes para que el API reemplace el historial compactado
      if cType = 'compaction' then
        FCompactionBlocks.AddOrSetValue(ResMsg, jContentItem.ToJSON);

      // D. Code Execution Output
      if (cType = 'tool_result') or (cType = 'code_execution_tool_result') then
      begin

        if jContentItem.TryGetValue<TJSONObject>('content', jInnerContent) then
        begin
          // Verificamos que sea un resultado de ejecuci?n de c?digo
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
                  // --- L?GICA DE RECUPERACI?N DE NOMBRE ---
                  FoundFileName := 'generated_file_' + Copy(Clave, 1, 8) + '.bin';
                  ToolUseID := jContentItem.GetValue<string>('tool_use_id', '');

                  if ToolUseID <> '' then
                  begin
                    // Escanear hacia atr?s para encontrar el input y deducir el nombre
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
                          // Buscamos en el c?digo Python el nombre del archivo generado
                          var
                          CodeStr := InputObj.GetValue<string>('code', '');

                          // Heur?stica simple: buscar 'output_path = ...' o strings con extensiones
                          // Esto es m?s complejo en Python que en Bash, pero intentamos algo b?sico
                          // Si el c?digo define output_path = ... 'archivo.wav'
                          var
                          PosExt := Pos('.wav', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.csv', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.png', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.pdf', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.mp3', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.json', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.xlsx', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.xls', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.zip', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.txt', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.html', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.xml', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.jpg', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.jpeg', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.gif', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.mp4', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.py', CodeStr);
                          if PosExt = 0 then
                            PosExt := Pos('.js', CodeStr);

                          if PosExt > 0 then
                          begin
                            // Intentar extraer el nombre buscando hacia atr?s desde la extensi?n
                            var
                            StartPos := LastDelimiter('''"', Copy(CodeStr, 1, PosExt));
                            if StartPos > 0 then
                            begin
                              // Calcular longitud real de la extensi?n (avanzar desde el punto)
                              var
                              EndPos := PosExt + 1;
                              while (EndPos <= Length(CodeStr)) and CharInSet(CodeStr[EndPos], ['a'..'z','A'..'Z','0'..'9']) do
                                Inc(EndPos);
                              var
                              ExtLen := EndPos - PosExt;
                              var
                              Candidate := Copy(CodeStr, StartPos + 1, (PosExt - StartPos) + ExtLen - 1);
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

  // 5. Update Component State & Response Message (AQU? EST? LA CORRECCI?N)
  Self.FLastContent := Respuesta;

  // Actualizar contadores globales del componente.
  // Los de cache se acumulan IGUAL que los de input/output: en un bucle de tool
  // calling cada ronda es una llamada facturable y el consumidor solo ve el
  // ultimo mensaje, asi que sin acumular aqui se perdian las rondas 1..N-1.
  Self.Prompt_tokens := Self.Prompt_tokens + aPrompt_tokens;
  Self.Completion_tokens := Self.Completion_tokens + aCompletion_tokens;
  Self.Total_tokens := Self.Total_tokens + aTotal_tokens;
  Self.Cached_tokens := Self.Cached_tokens + aCached_tokens;
  Self.Cache_write_tokens := Self.Cache_write_tokens + ResMsg.Cache_write_tokens;

  // Actualizar contadores del Mensaje de Respuesta
  ResMsg.Prompt_tokens := aPrompt_tokens;
  ResMsg.Completion_tokens := aCompletion_tokens;
  ResMsg.Total_tokens := aTotal_tokens;
  ResMsg.Cached_tokens := aCached_tokens; // Nuevo: Soporte de cach?

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
    // Los contadores DEBEN copiarse: este mensaje temporal es el que devuelve
    // GetLastMessage cuando la respuesta trae tool_calls, y es de ahi de donde
    // el consumidor (broker/facturacion) lee el usage del turno. Sin esto, toda
    // llamada que termina en tool_calls se facturaba con cache_read/cache_write
    // en cero aunque Anthropic los hubiera reportado.
    Msg.Prompt_tokens := aPrompt_tokens;
    Msg.Completion_tokens := aCompletion_tokens;
    Msg.Total_tokens := aTotal_tokens;
    Msg.Cached_tokens := aCached_tokens;
    Msg.Cache_write_tokens := ResMsg.Cache_write_tokens;
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);
  end;

  // 6. Ejecuci?n de Herramientas
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

        _CreateTask(ToolCall, I); // subrutina local garantiza captura por valor
        Inc(I);
      end;
      // Bombear Synchronize/Queue mientras se espera, para no colgar la app si un
      // tool call accede a la VCL/FMX via TThread.Synchronize (issue #103).
      // OJO: CheckSynchronize SOLO es valido en el hilo principal; en hilos
      // secundarios (workers Indy de un servicio headless, TTask de agentes)
      // LANZA excepcion "CheckSynchronize called from thread X". Fuera del main
      // thread solo esperamos. Mismo criterio que TMCPClientSSE.WaitForInitialization.
      while not TTask.WaitForAll(TaskList, 10) do
        if TThread.CurrentThread.ThreadID = MainThreadID then
          CheckSynchronize(0)
        else
          TThread.Sleep(10);

      var LStopLoop := False;
      for Clave in LFunciones.Keys do
      begin
        ToolCall := LFunciones[Clave];
        ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'user', ToolCall.Id, ToolCall.Name);
        for var LMF in ToolCall.MediaFiles do
          ToolMsg.AddMediaFile(LMF);
        ToolCall.MediaFiles.OwnsObjects := False;
        ToolMsg.Id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
        LStopLoop := LStopLoop or ToolCall.StopAgenticLoop;
      end;

      if LStopLoop then
      begin
        // Un handler pidi? detener el loop agentico (tool passthrough: lo
        // ejecuta el cliente final). Se finaliza el turno con los tool_calls
        // en ResMsg en vez de reenviar el resultado sint?tico al modelo.
        ResMsg.Tool_calls := sToolCalls;
        DoProcessResponse(AskMsg, ResMsg, Respuesta);
        FBusy := False;
        DoStateChange(acsFinished, 'Done');
        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, jObj, Role, Respuesta);
      end
      else
      begin
        // ISSUE #100: en async este ParseChat corre DENTRO del callback de recepción
        // (OnInternalReceiveData -> message_stop). Reentrar con Self.Run inicia un POST
        // nuevo cuyo finally libera el FCurrentPostStream de la petición aún en vuelo -> AV
        // en THTTPClient.ExecuteHTTPInternal. Se difiere la continuación a
        // OnRequestCompletedEvent (base), que corre cuando la petición ya liberó su stream.
        if FClient.Asynchronous then
          FPendingToolRun := True
        else
          Self.Run(Nil, ResMsg);
      end;
    end
    else
    begin
      if cap_ExtractCode in ModelConfig.SessionCaps then
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
  line, Chunk: string;
begin

  if not FClient.Asynchronous then
    Exit;

  LogDebug('-- OnInternalReceiveData--');

  // ISSUE #124 (extiende #108): FResponse acumula bytes TCP crudos en UTF-8; si un
  // chunk parte un caracter multibyte, DataString lanza EEncodingError. Se decodifica
  // una sola vez ANTES de limpiar: si falla, los bytes parciales quedan en FResponse
  // (sin Clear) y el proximo chunk completa el caracter.
  try
    Chunk := FResponse.DataString;
  except
    on EEncodingError do
    begin
      LogDebug('[chunk UTF-8 parcial - reintento con el proximo chunk]');
      Chunk := '';
    end;
  end;

  LogDebug(Chunk);

  AAbort := FAbort;
  if FAbort then
  begin
    FBusy := False;
    FPendingToolRun := False;
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    ClearStreamState;
    Exit;
  end;

  if Chunk <> '' then
  begin
    FStreamBuffer.Append(Chunk);
    FResponse.Clear;
  end;

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
  // 1. Procesar l?nea de evento (event: ...)
  if AChunk.StartsWith('event:') then
  begin
    FStreamLastEventType := Trim(Copy(AChunk, 7, Length(AChunk)));
    Exit;
  end;

  // 2. Procesar l?nea de datos (data: ...)
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
          FStreamResponseMsg.ToolCallId := jMessage.GetValue<string>('id'); // ID del mensaje de Claude
          FStreamResponseMsg.Model := jMessage.GetValue<string>('model');
          FStreamResponseMsg.Role := jMessage.GetValue<string>('role');

          // Leer input_tokens desde message_start (Claude streaming no incluye usage en message_stop)
          var jStartUsage: TJSONObject;
          if jMessage.TryGetValue<TJSONObject>('usage', jStartUsage) then
            Self.Prompt_tokens := jStartUsage.GetValue<Integer>('input_tokens', 0);

          // Notificar inicio de recepci?n
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
          streamBlock.ToolFunction.&Type := 'function';
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
            var jCitation: TJSONObject;
            if jDelta.TryGetValue<TJSONObject>('citation', jCitation) then
              streamBlock.CitationsBuffer.Add(jCitation.Clone as TJSONObject);
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
      // Fin de un bloque espec?fico
      // =======================================================================
      Else If AnsiLowerCase(eventType) = 'content_block_stop' then
      begin
        blockIndex := jData.GetValue<Integer>('index');
        if FStreamContentBlocks.TryGetValue(blockIndex, streamBlock) then
        begin
          // Si termin? un bloque de herramienta, parseamos los argumentos JSON acumulados
          if streamBlock.BlockType = 'tool_use' then
          begin
            try
              var
              jInput := TJSONObject.ParseJSONValue(streamBlock.JsonContent.ToString) as TJSONObject;
              if Assigned(jInput) then
              begin
                streamBlock.ToolFunction.Arguments := jInput.Format;
                jInput.Free;
              end;
            except
              // Ignorar error de parseo JSON en este punto, se manejar? globalmente si falla
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
      // Fin del mensaje completo. Reconstrucci?n y ejecuci?n.
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
            // y 'Run' crear? un NUEVO FStreamResponseMsg. Si hacemos el nil despu?s,
            // borrar?amos ese nuevo objeto creado por la recursividad.
          FStreamResponseMsg := nil;

          var
          jSyntheticResponse := TJSONObject.Create;
          try
            // Usamos la variable local MsgToProcess en lugar de FStreamResponseMsg
            jSyntheticResponse.AddPair('id', MsgToProcess.ToolCallId);
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

                // --- CASO 4: BLOQUES GEN?RICOS (Code Execution Results, etc.) ---
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
                      // Fallback si no es JSON v?lido
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
            // Si esto dispara un nuevo Run, FStreamResponseMsg (Global) ya estar? libre.
            ParseChat(jSyntheticResponse, MsgToProcess);

          finally
            jSyntheticResponse.Free;

            // ISSUE #105/#118: en async, TAiChat.RunNew LIBERA su ResMsg justo despues de
            // disparar el POST (ver rama "if FClient.Asynchronous then ResMsg.Free" en
            // uMakerAi.Chat.pas) y NO lo deja en FMessages. Por eso el intento previo de
            // reconciliar con FAsyncResMsg nunca funcionaba: apuntaba a un objeto ya
            // liberado. El mensaje del assistant se construye AQUI en MsgToProcess, asi
            // que lo archivamos directamente cuando es la respuesta FINAL (sin tool-loop
            // en curso: FBusy=False y sin tool_calls). Mismo patron que TAiChat/TAiOpenChat.
            // Si hubo tool_use, ParseChat ya agrego su propio mensaje (assistant + tool),
            // por lo que aqui MsgToProcess es descartable. Fix propuesto por @martijntonies.
            if Assigned(MsgToProcess) and (FMessages.IndexOf(MsgToProcess) = -1) then
            begin
              if (not FBusy) and (MsgToProcess.Tool_calls = '') then
              begin
                MsgToProcess.Id := FMessages.Count + 1;
                FMessages.Add(MsgToProcess);
                MsgToProcess := nil; // propiedad transferida a FMessages; no liberar abajo
              end;
              if Assigned(MsgToProcess) then
                MsgToProcess.Free;
            end;

            // Si ParseChat disparó un Run recursivo por tool calls, FBusy quedó True
            // (puesto por el InternalRunCompletions del nuevo round) y ya llamó
            // ClearStreamState antes de iniciar la nueva petición HTTP.
            // Limpiar aquí en ese caso crea una race con los callbacks del nuevo round.
            // Solo limpiamos cuando NO hay un run recursivo en curso (FBusy=False,
            // que es lo que hace el path sin tools en ParseChat antes de retornar).
            if not FBusy then
            begin
              FStreamBuffer.Clear;
              FStreamContentBlocks.Clear;
              FStreamLastEventType := '';
            end;
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
        FPendingToolRun := False;
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
  FContextConfig.Clear;

  // Agregar la regla de limpieza de herramientas
  // TriggerTokens: Cuando el prompt supere este tama?o
  // KeepLast: Mantener los ?ltimos N usos de herramientas (para no perder contexto inmediato)
  // ClearAtLeast: 0 (Default, deja que Claude decida cu?nto borrar)
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
    // Si hay archivos que requieren pre-procesamiento (ej: OCR local, conversi?n), se hace aqu?.
    For MF in aMsg.MediaFiles do
    Begin
      Procesado := False;
      DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado); // Env?a el archivo por si lo quiere procesar otra AI especializada
      MF.Procesado := Procesado;
      MF.Transcription := Respuesta;
      // Guarda las transcripciones en los MediaFile
    End;

    FLastPrompt := aMsg.Prompt; // Actualiza el ?ltimo prompt registrado

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
  LLastContent: TJSonArray;
  LSupportsMidSystem: Boolean;
begin
  Result := TJSonArray.Create;
  LLastContent := nil; // contenido del ultimo mensaje emitido (auto-cache del ultimo turno)

  // Mensajes {role:"system"} dentro del historial (mid-conversation): solo
  // opus-5/4.8/fable/mythos. En modelos sin soporte se degradan a un turno
  // user envuelto en <system-reminder> (mismo perfil de cache, sin 400)
  LSupportsMidSystem := IsClaudeMidSystemCapable(
    TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model));

  // Verificamos si el Code Interpreter est? activo
  IsCodeExecutionEnabled := cap_CodeInterpreter in ModelConfig.ModelCaps;

  // --- L?GICA DE FILTRADO DIN?MICO ---
  if IsCodeExecutionEnabled then
    // Si hay Code Execution, permitimos TODO (Excel, Zip, Code, etc.)
    // Construimos un set con todos los valores posibles del enum
    TargetCategories := [Low(TAiFileCategory) .. High(TAiFileCategory)]
  else
    // Si no, somos estrictos y solo permitimos lo que el componente diga (Vision, PDF)
    TargetCategories := Self.GetModelInputFileTypes;

  for LMessage in Self.Messages do
  begin
    LMessageObj := TJSONObject.Create;
    // Anthropic no acepta role 'tool' (convenci?n OpenAI usada por historiales
    // externos): los tool_result van dentro de un mensaje 'user'.
    if SameText(LMessage.Role, 'tool') then
      LMessageObj.AddPair('role', 'user')
    else if SameText(LMessage.Role, 'system') and (not LSupportsMidSystem) then
      LMessageObj.AddPair('role', 'user') // fallback <system-reminder> (caso 3)
    else
      LMessageObj.AddPair('role', LMessage.Role);
    LContentArray := TJSonArray.Create;

    // -------------------------------------------------------------------------
    // CASO 1: Resultado de Herramienta (Role: User o 'tool' estilo OpenAI)
    // -------------------------------------------------------------------------
    if ((LMessage.Role = 'user') or SameText(LMessage.Role, 'tool')) and (not LMessage.ToolCallId.IsEmpty) then
    begin
      LPartObj := TJSONObject.Create;
      LPartObj.AddPair('type', 'tool_result');
      LPartObj.AddPair('tool_use_id', LMessage.ToolCallId);

      MediaArr := LMessage.MediaFiles.GetMediaList([Tfc_Image, Tfc_pdf], False);
      if Length(MediaArr) = 0 then
      begin
        // Sin archivos adjuntos: content como string simple
        LPartObj.AddPair('content', LMessage.Prompt);
      end
      else
      begin
        // Con archivos adjuntos: content como array [text, image/document, ...]
        var LToolContent := TJSonArray.Create;
        var LTextBlock := TJSONObject.Create;
        LTextBlock.AddPair('type', 'text');
        LTextBlock.AddPair('text', LMessage.Prompt);
        LToolContent.Add(LTextBlock);
        for LMediaFile in MediaArr do
        begin
          var LFileBlock := TJSONObject.Create;
          if LMediaFile.FileCategory = Tfc_pdf then
          begin
            // Documento PDF
            LFileBlock.AddPair('type', 'document');
            var LDocSource := TJSONObject.Create;
            LDocSource.AddPair('type', 'base64');
            LDocSource.AddPair('media_type', LMediaFile.MimeType);
            LDocSource.AddPair('data', LMediaFile.Base64);
            LFileBlock.AddPair('source', LDocSource);
          end
          else
          begin
            // Imagen
            LFileBlock.AddPair('type', 'image');
            LSourceObj := TJSONObject.Create;
            LSourceObj.AddPair('type', 'base64');
            LSourceObj.AddPair('media_type', LMediaFile.MimeType);
            LSourceObj.AddPair('data', LMediaFile.Base64);
            LFileBlock.AddPair('source', LSourceObj);
          end;
          LToolContent.Add(LFileBlock);
        end;
        LPartObj.AddPair('content', LToolContent);
      end;

      LContentArray.Add(LPartObj);
    end

    // -------------------------------------------------------------------------
    // CASO 2: Mensaje del Asistente (Texto / Thinking / Tool Request)
    // -------------------------------------------------------------------------
    else if (LMessage.Role = 'assistant') then
    begin
      // 0. Bloque de compaction preservado (si este turno lo trajo): debe ir
      // primero — el API lo usa para reemplazar el historial compactado
      var LCompactRaw: string;
      if FCompactionBlocks.TryGetValue(LMessage, LCompactRaw) then
      begin
        var LCompactVal := TJSONObject.ParseJSONValue(LCompactRaw);
        if LCompactVal is TJSONObject then
          LContentArray.Add(TJSONObject(LCompactVal))
        else
          LCompactVal.Free;
      end;

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

        if (LMessage.CacheControl) and (FCacheCount < 4) then
        begin
          var
          jCache := TJSONObject.Create;
          jCache.AddPair('type', 'ephemeral');
          if FCacheTTL <> '' then
            jCache.AddPair('ttl', FCacheTTL);
          LPartObj.AddPair('cache_control', jCache);
          Inc(FCacheCount);
        end;

        LContentArray.Add(LPartObj);
      end;

      // C. Tool Use Blocks
      if LMessage.Tool_calls <> '' then
      begin
        try

{$IF CompilerVersion < 35}
          var
          LToolUseArray := TJSONUtils.ParseAsArray(LMessage.Tool_calls) as TJSonArray;
{$ELSE}
          var
          LToolUseArray := TJSonArray.ParseJSONValue(LMessage.Tool_calls) as TJSonArray;
{$ENDIF}
          if Assigned(LToolUseArray) then
          begin
            for var Val in LToolUseArray do
            begin
              var JTC := Val as TJSONObject;
              if SameText(JTC.GetValue<string>('type', ''), 'function') then
              begin
                // Formato OpenAI {"type":"function","function":{name,arguments}}
                // (historiales multi-turn externos) → bloque tool_use Anthropic.
                var JUse := TJSONObject.Create;
                JUse.AddPair('type', 'tool_use');
                JUse.AddPair('id', JTC.GetValue<string>('id', ''));
                var JFn := JTC.GetValue<TJSONObject>('function', nil);
                if Assigned(JFn) then
                begin
                  JUse.AddPair('name', JFn.GetValue<string>('name', ''));
                  var JArgs := TJSONObject.ParseJSONValue(JFn.GetValue<string>('arguments', '{}'));
                  if JArgs is TJSONObject then
                    JUse.AddPair('input', JArgs)
                  else
                  begin
                    JArgs.Free;
                    JUse.AddPair('input', TJSONObject.Create);
                  end;
                end;
                LContentArray.Add(JUse);
              end
              else
                LContentArray.Add(Val.Clone as TJSONObject);
            end;
            LToolUseArray.Free;
          end;
        except
          on E: Exception do LogDebug('Tool calls parse error: ' + E.Message);
        end;
      end;
    end

    // -------------------------------------------------------------------------
    // CASO 3: Mensaje Est?ndar de Usuario (Texto + Archivos)
    // -------------------------------------------------------------------------
    else
    begin
      bHasContent := False;

      // A. Texto
      if not LMessage.Prompt.IsEmpty then
      begin
        LPartObj := TJSONObject.Create;
        LPartObj.AddPair('type', 'text');
        // Mensaje system en modelo sin soporte mid-conversation: degradar a
        // turno user con envoltura <system-reminder> (fallback documentado)
        if SameText(LMessage.Role, 'system') and (not LSupportsMidSystem) then
          LPartObj.AddPair('text', '<system-reminder>' + sLineBreak +
            LMessage.Prompt + sLineBreak + '</system-reminder>')
        else
          LPartObj.AddPair('text', LMessage.Prompt);

        if (LMessage.CacheControl) and (FCacheCount < 4) then
        begin
          var
          jCache := TJSONObject.Create;
          jCache.AddPair('type', 'ephemeral');
          if FCacheTTL <> '' then
            jCache.AddPair('ttl', FCacheTTL);
          LPartObj.AddPair('cache_control', jCache);
          Inc(FCacheCount);
        end;

        LContentArray.Add(LPartObj);
        bHasContent := True;
      end;

      // B. Archivos (MediaFiles) - USANDO EL FILTRO DIN?MICO
      MediaArr := LMessage.MediaFiles.GetMediaList(TargetCategories, False);

      for LMediaFile in MediaArr do
      begin
        LPartObj := TJSONObject.Create;

        // --- SUB-CASO 3.1: IM?GENES ---
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
            // Detecci?n autom?tica de MimeType (Como corregimos antes)
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
        // Aqu? entrar? el Excel, CSV, etc.
        else if IsCodeExecutionEnabled and (not LMediaFile.IdFile.IsEmpty) then
        begin
          LPartObj.AddPair('type', 'container_upload');
          LPartObj.AddPair('file_id', LMediaFile.IdFile);
        end

        // --- SUB-CASO 3.3: DOCUMENTOS (PDFs para visi?n) ---
        else
        begin
          LPartObj.AddPair('type', 'document');
          LSourceObj := TJSONObject.Create;

          if not LMediaFile.IdFile.IsEmpty then
          begin
            LSourceObj.AddPair('type', 'file');
            LSourceObj.AddPair('file_id', LMediaFile.IdFile);
          end
          else if not LMediaFile.UrlMedia.IsEmpty then
          begin
            // URL directa — Claude descarga el documento sin necesidad de subida previa
            LSourceObj.AddPair('type', 'url');
            LSourceObj.AddPair('url', LMediaFile.UrlMedia);
          end
          else if LMediaFile.FileCategory = Tfc_Text then
          begin
            // Texto plano — enviar decodificado, no como base64
            var LBytes := TNetEncoding.Base64.DecodeStringToBytes(LMediaFile.Base64);
            LSourceObj.AddPair('type', 'text');
            LSourceObj.AddPair('data', TEncoding.UTF8.GetString(LBytes));
          end
          else
          begin
            LSourceObj.AddPair('type', 'base64');
            LSourceObj.AddPair('media_type', LMediaFile.MimeType);
            LSourceObj.AddPair('data', LMediaFile.Base64);
          end;

          LPartObj.AddPair('source', LSourceObj);

          // Title: priorizar LMediaFile.Title, fallback a FileName
          if not LMediaFile.Title.IsEmpty then
            LPartObj.AddPair('title', LMediaFile.Title)
          else if not LMediaFile.FileName.IsEmpty then
            LPartObj.AddPair('title', LMediaFile.FileName);

          // Context
          if not LMediaFile.Context.IsEmpty then
            LPartObj.AddPair('context', LMediaFile.Context);

          // Citations (RAG Nativo)
          if LMediaFile.EnableCitations then
          begin
            var jCit := TJSONObject.Create;
            jCit.AddPair('enabled', TJSONBool.Create(True));
            LPartObj.AddPair('citations', jCit);
          end;
        end;

        if (LMediaFile.CacheControl) and (FCacheCount < 4) then
        begin
          var
          jCache := TJSONObject.Create;
          jCache.AddPair('type', 'ephemeral');
          if FCacheTTL <> '' then
            jCache.AddPair('ttl', FCacheTTL);
          LPartObj.AddPair('cache_control', jCache);
          Inc(FCacheCount);
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
    LLastContent := LContentArray; // referencia al contenido del ultimo mensaje emitido
  end;

  // Conveniencia multi-turn: cachea el ultimo turno (su ultimo bloque) para que el
  // siguiente request lea TODO el historial previo desde cache. Solo si el cacheo de
  // contexto esta activo, queda presupuesto (<4 breakpoints) y el bloque no fue marcado
  // manualmente (CacheControl) para no duplicar el breakpoint.
  if FCacheCtxActive and (FCacheCount < 4) and Assigned(LLastContent) and (LLastContent.Count > 0) then
  begin
    var
    LLastBlock := LLastContent.Items[LLastContent.Count - 1] as TJSONObject;
    if LLastBlock.GetValue('cache_control') = nil then
    begin
      var
      jCacheLT := TJSONObject.Create;
      jCacheLT.AddPair('type', 'ephemeral');
      if FCacheTTL <> '' then
        jCacheLT.AddPair('ttl', FCacheTTL);
      LLastBlock.AddPair('cache_control', jCacheLT);
      Inc(FCacheCount);
    end;
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
  try // ISSUE #114: si el cuerpo lanza, liberar Result para no fugarlo

  // 1. Determinar la URL base
  if aUrl <> '' then
    EndPointUrl := aUrl
  else
    EndPointUrl := GlAIUrl;

  sUrl := EndPointUrl + 'models';

  Client := TNetHTTPClient.Create(Nil);
  try
    // 2. Configurar Headers espec?ficos de Claude
    // Nota: Claude requiere 'x-api-key' en lugar de Bearer token, y la versi?n de la API
    Headers := [TNetHeader.Create('x-api-key', aApiKey), TNetHeader.Create('anthropic-version', CLAUDE_API_VERSION), TNetHeader.Create('content-type', 'application/json')];

    // 3. Ejecutar Petici?n GET
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
                // Extraer el ID del modelo (ej: "claude-sonnet-4-5-20250514")
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
      // Esto es ?til si la API no devuelve modelos finetuned o nuevos que a?n no lista
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
  except // ISSUE #114: el camino de error no debe dejar huerfano el Result
    Result.Free;
    raise;
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
begin
  Result := TAiToolsFunctions.Create;
  For JVal1 in jChoices do
  Begin
    if JVal1.GetValue<String>('type') = 'tool_use' then
    begin
      Fun := TAiToolsFunction.Create;
      Fun.Id := JVal1.GetValue<String>('id');
      Fun.&Type := 'function';
      Fun.Name := JVal1.GetValue<String>('name');
      if JVal1.TryGetValue<TJSONObject>('input', Arg) then
      begin
        Fun.Arguments := Arg.Format;
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

{$IF CompilerVersion >= 36}
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
var
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  jRes: TJSONObject;
  jArr: TJSonArray;
  JVal: TJSONValue;
  MF: TAiMediaFile;
begin
  Result := TAiMediaFiles.Create;
  Client := TNetHTTPClient.Create(Nil);
  try
    Res := Client.Get(Url + 'files', nil, GetFileHeaders);
    if Res.StatusCode = 200 then
    begin
      jRes := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      if Assigned(jRes) then
      try
        if jRes.TryGetValue<TJSonArray>('data', jArr) then
          for JVal in jArr do
            if JVal is TJSONObject then
            begin
              MF := TAiMediaFile.Create;
              MF.IdFile := (JVal as TJSONObject).GetValue<string>('id', '');
              MF.FileName := (JVal as TJSONObject).GetValue<string>('filename', '');
              Result.Add(MF);
            end;
      finally
        jRes.Free;
      end;
    end
    else
      raise Exception.CreateFmt('RetrieveFileList Error: %d - %s',
        [Res.StatusCode, Res.ContentAsString]);
  finally
    Client.Free;
  end;
end;

procedure TAiClaudeChat.TranslateClaudeComputerArgs(ToolCall: TAiToolsFunction);
// Convierte el formato nativo de Claude Computer Use al formato TAiComputerUseTool.
// Claude envía: {"action":"left_click","coordinate":[x_px, y_px], ...}
// TAiComputerUseTool espera: {"x":norm, "y":norm, "text":"...", ...} + ToolCall.Name = acción mapeada
var
  JArgs, JNew: TJSONObject;
  JCoord, JStartCoord, JRegion: TJSONArray;
  Action, MappedName, SText, SDir: string;
  ScrW, ScrH, PxX, PxY, NormX, NormY, Amount: Integer;
  DDur: Double;
begin
  JArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if not Assigned(JArgs) then
    Exit;
  try
    if not JArgs.TryGetValue<string>('action', Action) then
      Exit;

    ScrW := ChatTools.ComputerUseTool.ScreenWidth;
    ScrH := ChatTools.ComputerUseTool.ScreenHeight;
    if ScrW <= 0 then ScrW := 1920;
    if ScrH <= 0 then ScrH := 1080;

    // Mapeo de nombres de acción Claude → TAiComputerUseTool
    if      Action = 'left_click'       then MappedName := 'click_at'
    else if Action = 'right_click'      then MappedName := 'right_click'
    else if Action = 'middle_click'     then MappedName := 'middle_click'
    else if Action = 'double_click'     then MappedName := 'double_click'
    else if Action = 'left_click_drag'  then MappedName := 'drag_and_drop'
    else if Action = 'mouse_move'       then MappedName := 'hover_at'
    else if Action = 'type'             then MappedName := 'type_text_at'
    else if Action = 'key'              then MappedName := 'key_combination'
    else if Action = 'scroll'           then MappedName := 'scroll_at'
    else if Action = 'wait'             then MappedName := 'wait_5_seconds'
    else MappedName := Action; // screenshot, go_back, go_forward pass through

    ToolCall.Name := MappedName;

    JNew := TJSONObject.Create;
    try
      // Drag: start_coordinate = origen (→ x,y); coordinate = destino (→ destination_x,y)
      if (Action = 'left_click_drag') and
         JArgs.TryGetValue<TJSONArray>('start_coordinate', JStartCoord) and
         (JStartCoord.Count >= 2) then
      begin
        PxX  := (JStartCoord.Items[0] as TJSONNumber).AsInt;
        PxY  := (JStartCoord.Items[1] as TJSONNumber).AsInt;
        NormX := Round(PxX / ScrW * 1000); if NormX > 999 then NormX := 999;
        NormY := Round(PxY / ScrH * 1000); if NormY > 999 then NormY := 999;
        JNew.AddPair('x', TJSONNumber.Create(NormX));
        JNew.AddPair('y', TJSONNumber.Create(NormY));

        if JArgs.TryGetValue<TJSONArray>('coordinate', JCoord) and (JCoord.Count >= 2) then
        begin
          NormX := Round((JCoord.Items[0] as TJSONNumber).AsInt / ScrW * 1000);
          NormY := Round((JCoord.Items[1] as TJSONNumber).AsInt / ScrH * 1000);
          if NormX > 999 then NormX := 999;
          if NormY > 999 then NormY := 999;
          JNew.AddPair('destination_x', TJSONNumber.Create(NormX));
          JNew.AddPair('destination_y', TJSONNumber.Create(NormY));
        end;
      end
      else if JArgs.TryGetValue<TJSONArray>('coordinate', JCoord) and (JCoord.Count >= 2) then
      begin
        PxX  := (JCoord.Items[0] as TJSONNumber).AsInt;
        PxY  := (JCoord.Items[1] as TJSONNumber).AsInt;
        NormX := Round(PxX / ScrW * 1000); if NormX > 999 then NormX := 999;
        NormY := Round(PxY / ScrH * 1000); if NormY > 999 then NormY := 999;
        JNew.AddPair('x', TJSONNumber.Create(NormX));
        JNew.AddPair('y', TJSONNumber.Create(NormY));
      end;

      // Texto / teclas / modificadores: el campo 'text' de Claude cambia de
      // significado según la acción.
      if JArgs.TryGetValue<string>('text', SText) then
      begin
        if (Action = 'key') or (Action = 'hold_key') then
          JNew.AddPair('keys', SText)
        else if Action = 'type' then
        begin
          JNew.AddPair('text', SText);
          // La acci?n 'type' de Claude NO implica Enter ni posici?n: escribe en el
          // control con foco. Evita el Enter autom?tico (default True en ParseAction).
          JNew.AddPair('press_enter', TJSONBool.Create(False));
        end
        else
          // En click/scroll/triple_click el 'text' contiene los modificadores
          JNew.AddPair('modifiers', SText);
      end;

      // Duración de hold_key (segundos)
      if (Action = 'hold_key') and JArgs.TryGetValue<Double>('duration', DDur) then
        JNew.AddPair('duration', TJSONNumber.Create(DDur));

      // Zoom: region [x1,y1,x2,y2] (px) → x,y + destination_x,destination_y (norm 0-999)
      if (Action = 'zoom') and JArgs.TryGetValue<TJSONArray>('region', JRegion) and (JRegion.Count >= 4) then
      begin
        NormX := Round((JRegion.Items[0] as TJSONNumber).AsInt / ScrW * 1000); if NormX > 999 then NormX := 999;
        NormY := Round((JRegion.Items[1] as TJSONNumber).AsInt / ScrH * 1000); if NormY > 999 then NormY := 999;
        JNew.AddPair('x', TJSONNumber.Create(NormX));
        JNew.AddPair('y', TJSONNumber.Create(NormY));
        NormX := Round((JRegion.Items[2] as TJSONNumber).AsInt / ScrW * 1000); if NormX > 999 then NormX := 999;
        NormY := Round((JRegion.Items[3] as TJSONNumber).AsInt / ScrH * 1000); if NormY > 999 then NormY := 999;
        JNew.AddPair('destination_x', TJSONNumber.Create(NormX));
        JNew.AddPair('destination_y', TJSONNumber.Create(NormY));
      end;

      // Scroll: Claude usa 'scroll_direction'/'scroll_amount' (computer_2025xxxx).
      // Se aceptan tambi?n 'direction'/'amount' por compatibilidad.
      if JArgs.TryGetValue<string>('scroll_direction', SDir) or
         JArgs.TryGetValue<string>('direction', SDir) then
        JNew.AddPair('direction', SDir);
      if JArgs.TryGetValue<Integer>('scroll_amount', Amount) or
         JArgs.TryGetValue<Integer>('amount', Amount) then
        JNew.AddPair('magnitude', TJSONNumber.Create(Amount * 120))
      else if Action = 'scroll' then
        JNew.AddPair('magnitude', TJSONNumber.Create(800));

      ToolCall.Arguments := JNew.ToJSON;
    finally
      JNew.Free;
    end;
  finally
    JArgs.Free;
  end;
end;

procedure TAiClaudeChat.DoCallFunction(ToolCall: TAiToolsFunction);
var
  LScreenshot: TAiMediaFile;
begin
  // 0. Computer Use nativo de Claude (tool name = 'computer')
  if (ToolCall.Name = 'computer') and Assigned(ChatTools.ComputerUseTool) then
  begin
    if Assigned(FOnCallToolFunction) then
      FOnCallToolFunction(Self, ToolCall);

    if ToolCall.Response = '' then
    begin
      LScreenshot := nil;
      try
        ChatTools.ComputerUseTool.TranslateClaudeToolCall(ToolCall);
        ToolCall.Response := ChatTools.ComputerUseTool.ProcessToolCall(ToolCall, LScreenshot);
        if Assigned(LScreenshot) then
          // El screenshot debe ir en ToolCall.MediaFiles: ParseChat lo copia al
          // ToolMsg ('user' + tool_use_id) que se serializa como tool_result con
          // bloque image. (Antes iba a ResMsg —mensaje del assistant— y nunca
          // llegaba al modelo, dejando a Claude "ciego".)
          ToolCall.MediaFiles.Add(LScreenshot);
      except
        on E: Exception do
        begin
          FreeAndNil(LScreenshot);
          ToolCall.Response := Format('{"output":"error: %s","url":"%s"}',
            [E.Message, ChatTools.ComputerUseTool.CurrentUrl]);
        end;
      end;
    end;
    Exit;
  end;

  // ---------------------------------------------------------------------------
  // 1. Interceptar Herramienta BASH / SHELL
  // ---------------------------------------------------------------------------
  if (ToolCall.Name = 'bash') then
  begin
    // A. Prioridad: Evento de Usuario (OnCallToolFunction)
    // Permite al programador interceptar, modificar o bloquear el comando antes de ejecutarlo.
    if Assigned(FOnCallToolFunction) then
      FOnCallToolFunction(Self, ToolCall);

    // B. Ejecuci?n Autom?tica (Componente TAiShell)
    // Si el usuario no llen? la respuesta en el evento anterior y tenemos el componente:
    if (ToolCall.Response = '') and Assigned(ChatTools.ShellTool) then
    begin
      // Asegurar que est? activo
      if not ChatTools.ShellTool.Active then
        ChatTools.ShellTool.Active := True;

      // Ejecutar el comando en la sesi?n persistente
      ToolCall.Response := ChatTools.ShellTool.Execute(ToolCall.Id, ToolCall.Arguments);
    end;

    // Si no hay componente ni evento, Claude recibir? una respuesta vac?a o error,
    // lo cual est? bien, pero idealmente ChatTools.ShellTool deber?a estar asignado.
    Exit;
  end;

  // 2. Interceptar Herramienta de Edici?n Nativa
  if ((ToolCall.Name = 'str_replace_based_edit_tool') or (ToolCall.Name = 'str_replace_editor')) and Assigned(ChatTools.TextEditorTool) then
  begin
    ToolCall.Response := ChatTools.TextEditorTool.Execute(ToolCall.Arguments);
    Exit;
  end;

  // 3. Verificar Componente Externo (AiFunctions)
  if Assigned(AiFunctions) and AiFunctions.DoCallFunction(ToolCall) then
  begin
    Exit;
  end;

  // 4. Evento de Usuario
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

      // Use HTTP Content-Type header to fix extension when filename is a .bin fallback
      var ContentType := Res.GetHeaderValue('Content-Type');
      // Strip charset/boundary suffix: "audio/wav; charset=utf-8" → "audio/wav"
      var SemiPos := Pos(';', ContentType);
      if SemiPos > 0 then
        ContentType := Trim(Copy(ContentType, 1, SemiPos - 1));
      if (ContentType <> '') and
         (not SameText(ContentType, 'application/octet-stream')) and
         (aMediaFile.FileName.EndsWith('.bin') or aMediaFile.FileName.IsEmpty) then
      begin
        var Ext := GetFileExtensionFromMimeType(ContentType);
        if Ext <> '' then
        begin
          if aMediaFile.FileName.IsEmpty then
            aMediaFile.FileName := aMediaFile.IdFile + '.' + Ext
          else
            aMediaFile.FileName := ChangeFileExt(aMediaFile.FileName, '.' + Ext);
        end;
      end;

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
