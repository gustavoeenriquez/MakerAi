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
// Driver para Anthropic Claude (https://api.anthropic.com/v1/)
//
// API propia con cabeceras x-api-key + anthropic-version + anthropic-beta.
// Formato SSE: event:/data: en pares (dos lineas por evento SSE).
// ProcessSSELine sobreescrito para manejar el par event/data.
//
// Diferencias respecto al Delphi original:
//   - TNetHTTPClient/TNetHeaders   → TFPHTTPClient + TStringList de pares
//   - TDictionary<Integer,Block>   → specialize TObjectDictionary<Integer,Block>
//   - TMultipartFormData           → multipart manual con TMemoryStream
//   - Inline var (Delphi 10.4+)   → declaracion al inicio del metodo
//   - string.IsEmpty / StartsWith  → comparacion manual con Copy/=
//   - string.Join(...)             → loop manual
//   - TTask.WaitForAll(...)        → loop secuencial (SaveAsync pattern)
//   - FOnReceiveData/FOnReceiveDataEnd → DoData / DoDataEnd (protegidos)
//   - Client.RequestTimeout        → Client.IOTimeout
//   - TCodeFileEntry               → TCodeFile (nombre correcto en FPC)

unit uMakerAi.Chat.Claude;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs,
  fpjson, jsonparser,
  fphttpclient, opensslsockets,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Tools.Functions,
  uMakerAi.Utils.CodeExtractor;

// ---------------------------------------------------------------------------
//  Clases de Gestion de Contexto (Context Editing)
// ---------------------------------------------------------------------------
type

  TClaudeContextTrigger = class
  public
    TriggerType: string;   // 'input_tokens' por defecto
    Value      : Integer;
    constructor Create(aValue: Integer; aType: string = 'input_tokens');
  end;

  TClaudeContextEdit = class
  public
    EditType                : string;   // 'clear_tool_uses_20250919'
    Trigger                 : TClaudeContextTrigger;
    Keep_ToolUses           : Integer;
    ClearAtLeast_InputTokens: Integer;
    constructor Create;
    destructor  Destroy; override;
    function    ToJSONObject: TJSONObject;
  end;

  TClaudeContextConfig = class
  private
    FEdits: specialize TObjectList<TClaudeContextEdit>;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure AddRule_ClearTools(TriggerTokens: Integer;
        KeepCount: Integer = 0; ClearAtLeast: Integer = 0);
    function  ToJSONObject: TJSONObject;
    function  IsEmpty: Boolean;
    procedure Clear;
  end;

  // ---------------------------------------------------------------------------
  //  Clase auxiliar para acumulacion de bloques en Streaming SSE
  // ---------------------------------------------------------------------------
  TClaudeStreamContentBlock = class
  public
    BlockType       : string;
    TextContent     : TStringBuilder;
    JsonContent     : TStringBuilder;
    Signature       : TStringBuilder;
    CitationsBuffer : TJSONArray;
    ExtraData       : TJSONObject;
    ToolFunction    : TAiToolsFunction;
    constructor Create;
    destructor  Destroy; override;
  end;

  TClaudeStreamBlocks = specialize TObjectDictionary<Integer, TClaudeStreamContentBlock>;

  // ---------------------------------------------------------------------------
  //  TAiClaudeChat — driver principal
  // ---------------------------------------------------------------------------
  TAiClaudeChat = class(TAiChat)
  private
    FStreamResponseMsg   : TAiChatMessage;
    FStreamContentBlocks : TClaudeStreamBlocks;
    FStreamBuffer        : TStringBuilder;
    FStreamLastEventType : string;

    FEnableMemory     : Boolean;
    FEnableThinking   : Boolean;
    FThinkingBudget   : Integer;
    FContextConfig    : TClaudeContextConfig;
    FCacheSystemPrompt: Boolean;
    FServiceTier      : string;

    // Cabeceras dinamicas como lista de pares name/value
    function  GetDynamicHeadersList: TStringList;
    function  GetFileHeadersList: TStringList;

    // Multipart manual para subida de archivos
    function  BuildMultipart(const ABoundary: string;
        aMediaFile: TAiMediaFile): TMemoryStream;

    procedure ClearStreamState;

    procedure SetEnableMemory(const Value: Boolean);
    procedure SetEnableThinking(const Value: Boolean);
    procedure SetThinkingBudget(const Value: Integer);

  protected
    // Sobreescritura del dispatcher SSE — Claude usa event:/data: en pares
    procedure ProcessSSELine(const ALine: string); override;

    // Manejo de archivos adjuntos antes de enviar
    function  InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
        overload; override;

    function  InitChatCompletions: string; override;
    procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); override;
    function  InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string; override;
    function  ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions; override;
    procedure DoCallFunction(ToolCall: TAiToolsFunction); override;

    function  ExtractToolCallJson(jChoices: TJSONArray): TJSONArray;

  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
    class function  GetModels(aApiKey: string; aUrl: string = ''): TStringList; override;

    function  GetMessages: TJSONArray; override;

    // Gestion de Archivos (Files API)
    function  UploadFile(aMediaFile: TAiMediaFile): string; override;
    function  DownloadFile(aMediaFile: TAiMediaFile): string; override;
    function  CheckFileState(aMediaFile: TAiMediaFile): string; override;
    function  DeleteFile(aMediaFile: TAiMediaFile): string; override;
    function  RetrieveFile(aFileId: string): TAiMediaFile; override;
    function  RetrieveFileList: TAiMediaFiles; override;
    function  UploadFileToCache(aMediaFile: TAiMediaFile;
        aTTL_Seconds: Integer = 3600): string; override;

    // Configuracion automatica de limpieza de contexto
    procedure ConfigureAutoContextClearing(TriggerTokens: Integer;
        KeepLast: Integer = 3);

    property  ContextConfig    : TClaudeContextConfig read FContextConfig;
    property  CacheSystemPrompt: Boolean              read FCacheSystemPrompt
        write FCacheSystemPrompt;

  published
    property EnableMemory   : Boolean read FEnableMemory    write SetEnableMemory    default False;
    property EnableThinking : Boolean read FEnableThinking  write SetEnableThinking  default False;
    property ThinkingBudget : Integer read FThinkingBudget  write SetThinkingBudget  default 1024;
    property ServiceTier    : string  read FServiceTier     write FServiceTier;
  end;

procedure Register;

implementation

uses
  UMakerAi.ParamsRegistry;

const
  GlAIUrl            = 'https://api.anthropic.com/v1/';
  CLAUDE_API_VERSION  = '2023-06-01';

  BETA_HDR_TOOLS              = 'tools-2024-05-16';
  BETA_HDR_FILES              = 'files-api-2025-04-14';
  BETA_HDR_MEMORY             = 'context-management-2025-06-27';
  BETA_HDR_CODE               = 'code-execution-2025-05-22';
  BETA_HDR_COMPUTER           = 'computer-use-2025-01-24';
  BETA_HDR_THINKING           = 'interleaved-thinking-2025-05-14';
  BETA_HDR_PDFS               = 'pdfs-2024-09-25';
  BETA_HDR_PROMPT_CACHING     = 'prompt-caching-2024-07-31';
  BETA_HDR_STRUCTURED_OUTPUTS = 'structured-outputs-2025-11-13';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiClaudeChat]);
end;

// ===========================================================================
//  TClaudeContextTrigger
// ===========================================================================

constructor TClaudeContextTrigger.Create(aValue: Integer; aType: string);
begin
  Value       := aValue;
  TriggerType := aType;
end;

// ===========================================================================
//  TClaudeContextEdit
// ===========================================================================

constructor TClaudeContextEdit.Create;
begin
  EditType                 := 'clear_tool_uses_20250919';
  Trigger                  := nil;
  Keep_ToolUses            := 0;
  ClearAtLeast_InputTokens := 0;
end;

destructor TClaudeContextEdit.Destroy;
begin
  Trigger.Free;
  inherited;
end;

function TClaudeContextEdit.ToJSONObject: TJSONObject;
var
  jTrigger, jKeep, jClear: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('type', EditType);

  if Assigned(Trigger) then
  begin
    jTrigger := TJSONObject.Create;
    jTrigger.Add('type',  Trigger.TriggerType);
    jTrigger.Add('value', TJSONIntegerNumber.Create(Trigger.Value));
    Result.Add('trigger', jTrigger);
  end;

  if Keep_ToolUses > 0 then
  begin
    jKeep := TJSONObject.Create;
    jKeep.Add('type',  'tool_uses');
    jKeep.Add('value', TJSONIntegerNumber.Create(Keep_ToolUses));
    Result.Add('keep', jKeep);
  end;

  if ClearAtLeast_InputTokens > 0 then
  begin
    jClear := TJSONObject.Create;
    jClear.Add('type',  'input_tokens');
    jClear.Add('value', TJSONIntegerNumber.Create(ClearAtLeast_InputTokens));
    Result.Add('clear_at_least', jClear);
  end;
end;

// ===========================================================================
//  TClaudeContextConfig
// ===========================================================================

constructor TClaudeContextConfig.Create;
begin
  FEdits := specialize TObjectList<TClaudeContextEdit>.Create(True);
end;

destructor TClaudeContextConfig.Destroy;
begin
  FEdits.Free;
  inherited;
end;

procedure TClaudeContextConfig.Clear;
begin
  FEdits.Clear;
end;

procedure TClaudeContextConfig.AddRule_ClearTools(TriggerTokens: Integer;
    KeepCount: Integer; ClearAtLeast: Integer);
var
  Rule: TClaudeContextEdit;
begin
  Rule                          := TClaudeContextEdit.Create;
  Rule.Trigger                  := TClaudeContextTrigger.Create(TriggerTokens);
  Rule.Keep_ToolUses            := KeepCount;
  Rule.ClearAtLeast_InputTokens := ClearAtLeast;
  FEdits.Add(Rule);
end;

function TClaudeContextConfig.IsEmpty: Boolean;
begin
  Result := FEdits.Count = 0;
end;

function TClaudeContextConfig.ToJSONObject: TJSONObject;
var
  jArr: TJSONArray;
  I   : Integer;
begin
  if IsEmpty then
    Exit(nil);

  Result := TJSONObject.Create;
  jArr   := TJSONArray.Create;
  for I := 0 to FEdits.Count - 1 do
    jArr.Add(FEdits[I].ToJSONObject);
  Result.Add('edits', jArr);
end;

// ===========================================================================
//  TClaudeStreamContentBlock
// ===========================================================================

constructor TClaudeStreamContentBlock.Create;
begin
  TextContent     := TStringBuilder.Create;
  JsonContent     := TStringBuilder.Create;
  Signature       := TStringBuilder.Create;
  CitationsBuffer := TJSONArray.Create;
  ExtraData       := TJSONObject.Create;
  ToolFunction    := nil;
end;

destructor TClaudeStreamContentBlock.Destroy;
begin
  TextContent.Free;
  JsonContent.Free;
  Signature.Free;
  CitationsBuffer.Free;
  ExtraData.Free;
  ToolFunction.Free;
  inherited;
end;

// ===========================================================================
//  TAiClaudeChat — helpers privados
// ===========================================================================

function TAiClaudeChat.GetDynamicHeadersList: TStringList;
var
  BetaList: TStringList;
  BetaStr : string;
  I       : Integer;
begin
  Result := TStringList.Create;

  // Cabeceras base siempre requeridas
  Result.Add('x-api-key');
  Result.Add(ApiKey);
  Result.Add('anthropic-version');
  Result.Add(CLAUDE_API_VERSION);
  Result.Add('content-type');
  Result.Add('application/json');

  // Construir beta header acumulado
  BetaList := TStringList.Create;
  BetaList.Duplicates := dupIgnore;
  BetaList.Sorted     := True;
  try
    if Tool_Active then
      BetaList.Add(BETA_HDR_TOOLS);
    if FEnableMemory or (not FContextConfig.IsEmpty) then
      BetaList.Add(BETA_HDR_MEMORY);
    if Tcm_CodeInterpreter in ChatMediaSupports then
      BetaList.Add(BETA_HDR_CODE);
    if tcm_ComputerUse in ChatMediaSupports then
      BetaList.Add(BETA_HDR_COMPUTER);
    if FEnableThinking or (ThinkingLevel <> tlDefault) then
      BetaList.Add(BETA_HDR_THINKING);
    BetaList.Add(BETA_HDR_PROMPT_CACHING);
    if tcm_pdf in ChatMediaSupports then
      BetaList.Add(BETA_HDR_PDFS);
    BetaList.Add(BETA_HDR_FILES);
    if (Response_format = tiaChatRfJsonSchema) or Tool_Active then
      BetaList.Add(BETA_HDR_STRUCTURED_OUTPUTS);

    if BetaList.Count > 0 then
    begin
      BetaStr := '';
      for I := 0 to BetaList.Count - 1 do
      begin
        if I > 0 then BetaStr := BetaStr + ',';
        BetaStr := BetaStr + BetaList[I];
      end;
      Result.Add('anthropic-beta');
      Result.Add(BetaStr);
    end;
  finally
    BetaList.Free;
  end;
end;

function TAiClaudeChat.GetFileHeadersList: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('x-api-key');
  Result.Add(ApiKey);
  Result.Add('anthropic-version');
  Result.Add(CLAUDE_API_VERSION);
  Result.Add('anthropic-beta');
  Result.Add(BETA_HDR_FILES);
end;

function TAiClaudeChat.BuildMultipart(const ABoundary: string;
    aMediaFile: TAiMediaFile): TMemoryStream;
var
  Part1, Footer: string;
  PartStream   : TStringStream;
begin
  Result := TMemoryStream.Create;

  Part1 :=
    '--' + ABoundary + #13#10 +
    'Content-Disposition: form-data; name="file"; filename="' +
        aMediaFile.FileName + '"' + #13#10 +
    'Content-Type: ' + aMediaFile.MimeType + #13#10 +
    #13#10;

  PartStream := TStringStream.Create(Part1);
  try
    Result.CopyFrom(PartStream, 0);
  finally
    PartStream.Free;
  end;

  aMediaFile.Content.Position := 0;
  Result.CopyFrom(aMediaFile.Content, 0);

  Footer :=
    #13#10 +
    '--' + ABoundary + #13#10 +
    'Content-Disposition: form-data; name="purpose"' + #13#10 +
    #13#10 +
    'assistants' + #13#10 +
    '--' + ABoundary + '--' + #13#10;

  PartStream := TStringStream.Create(Footer);
  try
    Result.CopyFrom(PartStream, 0);
  finally
    PartStream.Free;
  end;

  Result.Position := 0;
end;

procedure TAiClaudeChat.ClearStreamState;
begin
  FStreamBuffer.Clear;
  FStreamContentBlocks.Clear;
  FreeAndNil(FStreamResponseMsg);
  FStreamLastEventType := '';
end;

// ===========================================================================
//  TAiClaudeChat — implementacion principal
// ===========================================================================

class function TAiClaudeChat.GetDriverName: string;
begin
  Result := 'Claude';
end;

class procedure TAiClaudeChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@CLAUDE_API_KEY');
  Params.Add('Model=claude-sonnet-4-5-20250514');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=https://api.anthropic.com/v1/');
end;

class function TAiClaudeChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiClaudeChat.Create(Sender);
end;

constructor TAiClaudeChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey          := '@CLAUDE_API_KEY';
  Model           := 'claude-sonnet-4-5-20250514';
  Max_tokens      := 4096;
  Url             := GlAIUrl;
  ResponseTimeOut := 60000;

  FStreamContentBlocks := TClaudeStreamBlocks.Create([doOwnsValues]);
  FStreamBuffer        := TStringBuilder.Create;
  FStreamResponseMsg   := nil;
  FContextConfig       := TClaudeContextConfig.Create;

  FEnableMemory      := False;
  FEnableThinking    := False;
  FThinkingBudget    := 1024;
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

// ===========================================================================
//  InitChatCompletions — construye el cuerpo JSON del request
// ===========================================================================
function TAiClaudeChat.InitChatCompletions: string;
var
  AJSONObject       : TJSONObject;
  jThink            : TJSONObject;
  jMetaData         : TJSONObject;
  jContext          : TJSONObject;
  jArrTools         : TJSONArray;
  jArrStop          : TJSONArray;
  JTools            : TJSONObject;
  jSysArr           : TJSONArray;
  jSysBlock         : TJSONObject;
  jCache            : TJSONObject;
  jToolChoice       : TJSONObject;
  jOutputFormat     : TJSONObject;
  jSchemaParsed     : TJSONObject;
  jUserTools        : TJSONArray;
  Lista             : TStringList;
  LModel            : string;
  LAsincronico      : Boolean;
  LSystemPrompt     : string;
  JsonSchemaStr     : string;
  LToolsJson        : string;
  Res               : string;
  LActualBudget     : Integer;
  LMsg              : TAiChatMessage;
  LMedia            : TAiMediaFile;
  I, J              : Integer;
begin
  if User = '' then
    User := 'user';

  // 1. AUTO-UPLOAD de archivos no subidos aun
  for I := 0 to FMessages.Count - 1 do
  begin
    LMsg := FMessages[I];
    if (LMsg.Role = 'user') and (LMsg.MediaFiles.Count > 0) then
      for J := 0 to LMsg.MediaFiles.Count - 1 do
      begin
        LMedia := LMsg.MediaFiles[J];
        if LMedia.IdFile = '' then
          if (Tcm_CodeInterpreter in ChatMediaSupports) or
             (not (LMedia.FileCategory in [Tfc_Image, Tfc_pdf])) then
            try
              UploadFile(LMedia);
            except
              on E: Exception do
                LogDebug('Auto-Upload error: ' + E.Message);
            end;
      end;
  end;

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'claude-sonnet-4-5-20250514';

  // 2. Calculo de thinking budget
  LActualBudget := FThinkingBudget;
  if ThinkingLevel <> tlDefault then
    FEnableThinking := True;

  if FEnableThinking then
  begin
    case ThinkingLevel of
      tlLow    : LActualBudget := 2048;
      tlMedium  : LActualBudget := 8192;
      tlHigh    : LActualBudget := 16384;
    else
      LActualBudget := FThinkingBudget;
    end;
    if LActualBudget < 1024 then
      LActualBudget := 1024;
    if Max_tokens <= LActualBudget then
      Max_tokens := LActualBudget + 4096;
  end;

  LAsincronico := Asynchronous;
  AJSONObject  := TJSONObject.Create;
  Lista        := TStringList.Create;
  try
    AJSONObject.Add('model', LModel);

    // 3. System Prompt
    LSystemPrompt := PrepareSystemMsg;
    if LSystemPrompt <> '' then
    begin
      if FCacheSystemPrompt then
      begin
        jSysArr   := TJSONArray.Create;
        jSysBlock := TJSONObject.Create;
        jSysBlock.Add('type', 'text');
        jSysBlock.Add('text', LSystemPrompt);
        jCache := TJSONObject.Create;
        jCache.Add('type', 'ephemeral');
        jSysBlock.Add('cache_control', jCache);
        jSysArr.Add(jSysBlock);
        AJSONObject.Add('system', jSysArr);
      end
      else
        AJSONObject.Add('system', LSystemPrompt);
    end;

    AJSONObject.Add('max_tokens', TJSONIntegerNumber.Create(Max_tokens));
    AJSONObject.Add('messages',   GetMessages);

    // 4. Structured Outputs (JSON Schema)
    if Response_format = tiaChatRfJsonSchema then
    begin
      JsonSchemaStr := Trim(JsonSchema.Text);
      if JsonSchemaStr <> '' then
      begin
        try
          jSchemaParsed := TJSONObject(GetJSON(
              StringReplace(JsonSchemaStr, '\n', ' ', [rfReplaceAll])));
          if Assigned(jSchemaParsed) then
          begin
            if (JGetStr(jSchemaParsed, 'type') = 'object') and
               (jSchemaParsed.IndexOfName('additionalProperties') < 0) then
              jSchemaParsed.Add('additionalProperties',
                  TJSONBoolean.Create(False));
            jOutputFormat := TJSONObject.Create;
            jOutputFormat.Add('type', 'json_schema');
            jOutputFormat.Add('schema', jSchemaParsed);
            AJSONObject.Add('output_format', jOutputFormat);
          end;
        except
          on E: Exception do
            LogDebug('JSON Schema parse error: ' + E.Message);
        end;
      end;
    end;

    // 5. Thinking parameters
    if FEnableThinking then
    begin
      jThink := TJSONObject.Create;
      jThink.Add('type', 'enabled');
      jThink.Add('budget_tokens', TJSONIntegerNumber.Create(LActualBudget));
      AJSONObject.Add('thinking', jThink);
      AJSONObject.Add('temperature', TJSONFloatNumber.Create(1.0));
    end
    else
    begin
      if Temperature > 0 then
        AJSONObject.Add('temperature', TJSONFloatNumber.Create(Temperature))
      else if Top_p > 0 then
        AJSONObject.Add('top_p', TJSONFloatNumber.Create(Top_p));
      if K > 0 then
        AJSONObject.Add('top_k', TJSONIntegerNumber.Create(K));
    end;

    // 6. Metadata y Service Tier
    if (User <> '') and (User <> 'user') then
    begin
      jMetaData := TJSONObject.Create;
      jMetaData.Add('user_id', User);
      AJSONObject.Add('metadata', jMetaData);
    end;
    if FServiceTier <> '' then
      AJSONObject.Add('service_tier', FServiceTier);
    if not FContextConfig.IsEmpty then
    begin
      jContext := FContextConfig.ToJSONObject;
      if Assigned(jContext) then
        AJSONObject.Add('context_management', jContext);
    end;

    // 7. Tools
    jArrTools := TJSONArray.Create;

    if Tool_Active then
    begin
      LToolsJson := Trim(GetToolsStr(tfClaude));
      if LToolsJson <> '' then
      begin
        jUserTools := TJSONArray(GetJSON(LToolsJson));
        if Assigned(jUserTools) then
        begin
          for I := 0 to jUserTools.Count - 1 do
            jArrTools.Add(jUserTools.Items[I].Clone as TJSONObject);
          jUserTools.Free;
        end;
      end;
    end;

    if tcm_WebSearch in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.Add('type', 'web_search_20250305');
      JTools.Add('name', 'web_search');
      jArrTools.Add(JTools);
    end;

    if Tcm_CodeInterpreter in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.Add('type', 'code_execution_20250522');
      JTools.Add('name', 'code_execution');
      jArrTools.Add(JTools);
    end;

    if FEnableMemory then
    begin
      JTools := TJSONObject.Create;
      JTools.Add('type', 'memory_20250818');
      JTools.Add('name', 'memory');
      jArrTools.Add(JTools);
    end;

    if tcm_TextEditor in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.Add('type', 'text_editor_20250728');
      JTools.Add('name', 'str_replace_based_edit_tool');
      jArrTools.Add(JTools);
    end;

    if tcm_ComputerUse in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.Add('type', 'computer_20250124');
      JTools.Add('name', 'computer');
      JTools.Add('display_width_px',  TJSONIntegerNumber.Create(1024));
      JTools.Add('display_height_px', TJSONIntegerNumber.Create(768));
      jArrTools.Add(JTools);
    end;

    if tcm_Shell in ChatMediaSupports then
    begin
      JTools := TJSONObject.Create;
      JTools.Add('type', 'bash_20250124');
      JTools.Add('name', 'bash');
      jArrTools.Add(JTools);
    end;

    if jArrTools.Count > 0 then
    begin
      AJSONObject.Add('tools', jArrTools);
      if Trim(Tool_choice) <> '' then
      begin
        try
          jToolChoice := TJSONObject(GetJSON(Tool_choice));
          if Assigned(jToolChoice) then
          begin
            AJSONObject.Add('tool_choice', TJSONObject(jToolChoice.Clone));
            jToolChoice.Free;
          end;
        except
          AJSONObject.Add('tool_choice', Tool_choice);
        end;
      end;
    end
    else
      jArrTools.Free;

    // 8. Stop sequences
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      jArrStop := TJSONArray.Create;
      for I := 0 to Lista.Count - 1 do
        jArrStop.Add(Lista[I]);
      AJSONObject.Add('stop_sequences', jArrStop);
    end;

    AJSONObject.Add('stream', TJSONBoolean.Create(LAsincronico));

    Res    := AJSONObject.AsJSON;
    Res    := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);

  finally
    AJSONObject.Free;
    Lista.Free;
  end;
end;

// ===========================================================================
//  InternalRunCompletions — POST HTTP
// ===========================================================================
function TAiClaudeChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
var
  ABody      : string;
  sUrl       : string;
  jObj       : TJSONObject;
  BodyStream : TStringStream;
  RespStream : TStringStream;
  Client     : TFPHTTPClient;
  HdrList    : TStringList;
  HdrArr     : array of string;
  HdrIdx     : Integer;
begin
  FBusy         := True;
  FAbort        := False;
  FLastError    := '';
  FLastContent  := '';
  FLastPrompt   := '';
  ClearStreamState;

  sUrl := Url + 'messages';

  try
    DoStateChange(acsConnecting, 'Sending request...');

    if Asynchronous then
      FStreamResponseMsg := TAiChatMessage.Create('', 'assistant');

    ABody := InitChatCompletions;
    LogDebug('-- Request body --');
    LogDebug(ABody);

    // Construir lista de cabeceras
    HdrList := GetDynamicHeadersList;
    try
      if Asynchronous then
      begin
        // Convertir TStringList a array of string para StartHttpThread
        SetLength(HdrArr, HdrList.Count);
        for HdrIdx := 0 to HdrList.Count - 1 do
          HdrArr[HdrIdx] := HdrList[HdrIdx];
        StartHttpThread(sUrl, ABody, HdrArr);
        Result := '';
        // Resultado llega por ProcessSSELine
      end
      else
      begin
        // Modo sync: TFPHTTPClient
        Client     := TFPHTTPClient.Create(nil);
        BodyStream := TStringStream.Create(ABody);
        RespStream := TStringStream.Create('');
        try
          for HdrIdx := 0 to (HdrList.Count div 2) - 1 do
            Client.AddHeader(HdrList[HdrIdx * 2], HdrList[HdrIdx * 2 + 1]);
          if ResponseTimeOut > 0 then
            Client.IOTimeout := ResponseTimeOut;
          Client.RequestBody := BodyStream;
          try
            Client.HTTPMethod('POST', sUrl, RespStream, [200]);
          except
            on E: Exception do
            begin
              DoError('Claude HTTP error: ' + E.Message, E);
              FBusy := False;
              Exit('');
            end;
          end;

          LogDebug('-- Response sync --');
          LogDebug(RespStream.DataString);

          if Client.ResponseStatusCode = 200 then
          begin
            jObj := TJSONObject(GetJSON(RespStream.DataString));
            if Assigned(jObj) then
            try
              ParseChat(jObj, ResMsg);
              Result := FLastContent;
            finally
              jObj.Free;
            end
            else
              DoError('Invalid JSON response from Claude', nil);
          end
          else
            raise Exception.CreateFmt('Error Received: %d, %s',
                [Client.ResponseStatusCode, RespStream.DataString]);

        finally
          Client.RequestBody := nil;
          RespStream.Free;
          BodyStream.Free;
          Client.Free;
          FBusy := False;
        end;
      end;
    finally
      HdrList.Free;
    end;
  except
    on E: Exception do
    begin
      FBusy := False;
      DoError(E.Message, E);
      Result := '';
    end;
  end;
end;

// ===========================================================================
//  ProcessSSELine — sobreescritura del dispatcher SSE de Claude
//  Claude usa pares event:/data: (dos lineas por evento SSE)
// ===========================================================================
procedure TAiClaudeChat.ProcessSSELine(const ALine: string);
var
  jData         : TJSONObject;
  jDelta        : TJSONObject;
  jBlock        : TJSONObject;
  jMessage      : TJSONObject;
  jUsage        : TJSONObject;
  jInput        : TJSONObject;
  jCitation     : TJSONObject;
  jCont         : TJSONObject;
  jError        : TJSONObject;
  jSynth        : TJSONObject;
  jSynthUsage   : TJSONObject;
  jSynthContent : TJSONArray;
  jBlockObj     : TJSONObject;
  jInputObj     : TJSONObject;
  jCitClone     : TJSONObject;
  streamBlock   : TClaudeStreamContentBlock;
  MsgToProcess  : TAiChatMessage;
  jsonDataStr   : string;
  eventType     : string;
  deltaType     : string;
  textDelta     : string;
  jsonDelta     : string;
  StopReason    : string;
  BlockTypeLoc  : string;
  sArgs         : string;
  ErrMsg        : string;
  tmpSig        : string;
  sId           : string;
  blockIndex    : Integer;
  SortedKeys    : specialize TList<Integer>;
  SortedIdx     : Integer;
  BlockKey      : Integer;
  PairIdx       : Integer;
begin
  // Linea de evento (event: xxx) — guardar tipo y salir
  if Copy(ALine, 1, 6) = 'event:' then
  begin
    FStreamLastEventType := Trim(Copy(ALine, 7, Length(ALine)));
    Exit;
  end;

  // Solo procesar lineas data: si hay tipo de evento registrado
  if Copy(ALine, 1, 5) <> 'data:' then
    Exit;
  if FStreamLastEventType = '' then
    Exit;

  jsonDataStr := Trim(Copy(ALine, 6, Length(ALine)));
  if jsonDataStr = '' then
    Exit;

  jData := TJSONObject(GetJSON(jsonDataStr));
  if not Assigned(jData) then
    Exit;

  try
    eventType := JGetStr(jData, 'type');

    // ----- message_start -----
    if AnsiLowerCase(eventType) = 'message_start' then
    begin
      if Assigned(FStreamResponseMsg) then
      begin
        jMessage := JGetObj(jData, 'message');
        if Assigned(jMessage) then
        begin
          FStreamResponseMsg.ToolCallId := JGetStr(jMessage, 'id');
          FStreamResponseMsg.Model      := JGetStr(jMessage, 'model');
          FStreamResponseMsg.Role       := JGetStr(jMessage, 'role');
          // Capturar input_tokens del message_start (disponible en streaming)
          jUsage := JGetObj(jMessage, 'usage');
          if Assigned(jUsage) then
          begin
            FStreamResponseMsg.Prompt_tokens := JGetInt(jUsage, 'input_tokens');
            FStreamResponseMsg.Cached_tokens := JGetInt(jUsage, 'cache_read_input_tokens');
          end;
        end;
        // Notificar inicio de recepcion (sin datos de texto aun)
        DoData(FStreamResponseMsg, 'assistant', '', nil);
      end;
    end

    // ----- content_block_start -----
    else if AnsiLowerCase(eventType) = 'content_block_start' then
    begin
      blockIndex := JGetInt(jData, 'index');
      jBlock     := JGetObj(jData, 'content_block');
      if Assigned(jBlock) then
      begin
        streamBlock           := TClaudeStreamContentBlock.Create;
        streamBlock.BlockType := JGetStr(jBlock, 'type');

        // Copiar metadatos extra (todo excepto 'type')
        for PairIdx := 0 to jBlock.Count - 1 do
          if jBlock.Names[PairIdx] <> 'type' then
            streamBlock.ExtraData.Add(
                jBlock.Names[PairIdx],
                jBlock.Items[PairIdx].Clone as TJSONData);

        if streamBlock.BlockType = 'tool_use' then
        begin
          streamBlock.ToolFunction      := TAiToolsFunction.Create;
          streamBlock.ToolFunction.Id   := JGetStr(jBlock, 'id');
          streamBlock.ToolFunction.Name := JGetStr(jBlock, 'name');
          streamBlock.ToolFunction.Tipo := 'function';
        end;

        FStreamContentBlocks.Add(blockIndex, streamBlock);
      end;
    end

    // ----- content_block_delta -----
    else if AnsiLowerCase(eventType) = 'content_block_delta' then
    begin
      blockIndex := JGetInt(jData, 'index');
      if FStreamContentBlocks.TryGetValue(blockIndex, streamBlock) then
      begin
        jDelta := JGetObj(jData, 'delta');
        if Assigned(jDelta) then
        begin
          deltaType := JGetStr(jDelta, 'type');

          if deltaType = 'text_delta' then
          begin
            textDelta := JGetStr(jDelta, 'text');
            streamBlock.TextContent.Append(textDelta);
            if streamBlock.BlockType = 'text' then
            begin
              FLastContent := FLastContent + textDelta;
              // Pasar nil para evitar dangling pointer al despachar al main thread
              DoData(FStreamResponseMsg, 'assistant', textDelta, nil);
            end;
          end

          else if deltaType = 'citations_delta' then
          begin
            jCitation := JGetObj(jDelta, 'citation');
            if Assigned(jCitation) then
            begin
              jCitClone := TJSONObject(jCitation.Clone);
              streamBlock.CitationsBuffer.Add(jCitClone);
            end;
          end

          else if deltaType = 'input_json_delta' then
          begin
            jsonDelta := JGetStr(jDelta, 'partial_json');
            streamBlock.JsonContent.Append(jsonDelta);
          end

          else if deltaType = 'thinking_delta' then
          begin
            textDelta := JGetStr(jDelta, 'thinking');
            streamBlock.TextContent.Append(textDelta);
            DoThinking(FStreamResponseMsg, 'assistant', textDelta, nil);
          end

          else if deltaType = 'signature_delta' then
          begin
            tmpSig := JGetStr(jDelta, 'signature');
            streamBlock.Signature.Append(tmpSig);
          end;
        end;
      end;
    end

    // ----- content_block_stop -----
    else if AnsiLowerCase(eventType) = 'content_block_stop' then
    begin
      blockIndex := JGetInt(jData, 'index');
      if FStreamContentBlocks.TryGetValue(blockIndex, streamBlock) then
      begin
        if streamBlock.BlockType = 'tool_use' then
        begin
          try
            sArgs  := streamBlock.JsonContent.ToString;
            jInput := TJSONObject(GetJSON(sArgs));
            if Assigned(jInput) then
            begin
              streamBlock.ToolFunction.Arguments := jInput.FormatJSON;
              for PairIdx := 0 to jInput.Count - 1 do
                streamBlock.ToolFunction.Params.Values[jInput.Names[PairIdx]] :=
                    jInput.Items[PairIdx].AsString;
              jInput.Free;
            end;
          except
            // ignorar error de parseo parcial
          end;
        end;
      end;
    end

    // ----- message_delta -----
    else if AnsiLowerCase(eventType) = 'message_delta' then
    begin
      jDelta := JGetObj(jData, 'delta');
      if Assigned(jDelta) and Assigned(FStreamResponseMsg) then
      begin
        StopReason := JGetStr(jDelta, 'stop_reason');
        if StopReason <> '' then
          FStreamResponseMsg.StopReason := StopReason;
      end;
      jUsage := JGetObj(jData, 'usage');
      if Assigned(jUsage) and Assigned(FStreamResponseMsg) then
        FStreamResponseMsg.Completion_tokens := JGetInt(jUsage, 'output_tokens');
    end

    // ----- message_stop -----
    else if AnsiLowerCase(eventType) = 'message_stop' then
    begin
      if Assigned(FStreamResponseMsg) then
      begin
        // Desvinculamos ANTES de ParseChat (puede ser recursivo via tool calls)
        MsgToProcess       := FStreamResponseMsg;
        FStreamResponseMsg := nil;

        jSynth := TJSONObject.Create;
        try
          jSynth.Add('id',         MsgToProcess.ToolCallId);
          jSynth.Add('type',       'message');
          jSynth.Add('role',       'assistant');
          jSynth.Add('model',      MsgToProcess.Model);
          if MsgToProcess.StopReason <> '' then
            jSynth.Add('stop_reason', MsgToProcess.StopReason)
          else
            jSynth.Add('stop_reason', TJSONNull.Create);

          jSynthUsage := TJSONObject.Create;
          jSynthUsage.Add('input_tokens',  TJSONIntegerNumber.Create(MsgToProcess.Prompt_tokens));
          jSynthUsage.Add('output_tokens', TJSONIntegerNumber.Create(MsgToProcess.Completion_tokens));
          jSynth.Add('usage', jSynthUsage);

          // Construir content array ordenado
          jSynthContent := TJSONArray.Create;
          SortedKeys    := specialize TList<Integer>.Create;
          try
            for BlockKey in FStreamContentBlocks.Keys do
              SortedKeys.Add(BlockKey);
            SortedKeys.Sort;

            for SortedIdx := 0 to SortedKeys.Count - 1 do
            begin
              BlockKey     := SortedKeys[SortedIdx];
              streamBlock  := FStreamContentBlocks[BlockKey];
              jBlockObj    := TJSONObject.Create;
              BlockTypeLoc := streamBlock.BlockType;

              if BlockTypeLoc = 'text' then
              begin
                jBlockObj.Add('type', 'text');
                jBlockObj.Add('text', streamBlock.TextContent.ToString);
                if streamBlock.CitationsBuffer.Count > 0 then
                  jBlockObj.Add('citations',
                      TJSONArray(streamBlock.CitationsBuffer.Clone));
                jSynthContent.Add(jBlockObj);
              end

              else if (BlockTypeLoc = 'tool_use') and
                      Assigned(streamBlock.ToolFunction) then
              begin
                jBlockObj.Add('type', 'tool_use');
                jBlockObj.Add('id',   streamBlock.ToolFunction.Id);
                jBlockObj.Add('name', streamBlock.ToolFunction.Name);
                sArgs := streamBlock.JsonContent.ToString;
                if sArgs = '' then sArgs := '{}';
                try
                  jInputObj := TJSONObject(GetJSON(sArgs));
                  if Assigned(jInputObj) then
                    jBlockObj.Add('input', jInputObj)
                  else
                    jBlockObj.Add('input', TJSONObject.Create);
                except
                  jBlockObj.Add('input', TJSONObject.Create);
                end;
                jSynthContent.Add(jBlockObj);
              end

              else if BlockTypeLoc = 'thinking' then
              begin
                jBlockObj.Add('type',     'thinking');
                jBlockObj.Add('thinking',  streamBlock.TextContent.ToString);
                if streamBlock.Signature.Length > 0 then
                  jBlockObj.Add('signature', streamBlock.Signature.ToString);
                jSynthContent.Add(jBlockObj);
              end

              else
              begin
                // Bloques genericos (code execution, etc.)
                jBlockObj.Add('type', BlockTypeLoc);
                for PairIdx := 0 to streamBlock.ExtraData.Count - 1 do
                  jBlockObj.Add(streamBlock.ExtraData.Names[PairIdx],
                      streamBlock.ExtraData.Items[PairIdx].Clone as TJSONData);
                if streamBlock.JsonContent.Length > 0 then
                begin
                  try
                    jCont := TJSONObject(GetJSON(streamBlock.JsonContent.ToString));
                    if Assigned(jCont) then
                      jBlockObj.Add('content', jCont);
                  except
                    // fallback
                  end;
                end;
                jSynthContent.Add(jBlockObj);
              end;
            end;

            jSynth.Add('content', jSynthContent);
          finally
            SortedKeys.Free;
          end;

          // Llamar ParseChat con la respuesta sintetica reconstruida
          ParseChat(jSynth, MsgToProcess);

        finally
          jSynth.Free;
          FStreamBuffer.Clear;
          FStreamContentBlocks.Clear;
          FStreamLastEventType := '';
          FBusy := False;
        end;
      end;
    end

    // ----- error -----
    else if AnsiLowerCase(eventType) = 'error' then
    begin
      jError := JGetObj(jData, 'error');
      ErrMsg := 'Unknown Error';
      if Assigned(jError) then
        ErrMsg := JGetStr(jError, 'message', 'Unknown');
      DoError('Claude Stream Error: ' + ErrMsg, nil);
      ClearStreamState;
      FBusy := False;
    end;

  finally
    jData.Free;
    FStreamLastEventType := '';
  end;
end;

// ===========================================================================
//  ParseChat — parsea la respuesta completa
// ===========================================================================
procedure TAiClaudeChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
var
  choices        : TJSONArray;
  jContentItem   : TJSONObject;
  jval           : TJSONData;
  uso            : TJSONObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens, aCached_tokens: Integer;
  Role, Respuesta, StopR, LModel, cType: string;
  Clave          : string;
  sToolCalls     : string;
  ThinkingContent: string;
  SigVal         : string;
  LFunciones     : TAiToolsFunctions;
  ToolCall       : TAiToolsFunction;
  ToolMsg        : TAiChatMessage;
  AskMsg         : TAiChatMessage;
  jCitationsArr  : TJSONArray;
  jCitObj        : TJSONObject;
  jCitData       : TJSONData;
  citType        : string;
  LCitation      : TAiMsgCitation;
  LSource        : TAiCitationSource;
  SearchItem     : TAiWebSearchItem;
  jInnerContent  : TJSONObject;
  jResultArray   : TJSONArray;
  jResultContent : TJSONObject;
  ToolUseID      : string;
  FoundFileName  : string;
  ScanObj        : TJSONObject;
  InputObj       : TJSONObject;
  CodeStr        : string;
  NewFile        : TAiMediaFile;
  jToolCallArray : TJSONArray;
  code           : TMarkdownCodeExtractor;
  CodeFiles      : TCodeFileList;
  CodeFile       : TCodeFile;
  MF             : TAiMediaFile;
  St             : TStringStream;
  SaveAsync      : Boolean;
  ExtPos         : Integer;
  sId            : string;
  I, J, ScanIdx  : Integer;
begin
  AskMsg := GetLastMessage;

  // 1. Metadatos del mensaje
  LModel  := JGetStr(jObj, 'model');
  Role    := JGetStr(jObj, 'role', 'assistant');
  StopR   := JGetStr(jObj, 'stop_reason');
  ResMsg.StopReason := StopR;
  if StopR = 'refusal' then
    ResMsg.IsRefusal := True;

  // 2. Usage tokens
  aPrompt_tokens     := 0;
  aCompletion_tokens := 0;
  aTotal_tokens      := 0;
  aCached_tokens     := 0;

  uso := JGetObj(jObj, 'usage');
  if Assigned(uso) then
  begin
    aPrompt_tokens     := JGetInt(uso, 'input_tokens');
    aCompletion_tokens := JGetInt(uso, 'output_tokens');
    aCached_tokens     := JGetInt(uso, 'cache_read_input_tokens');
    aTotal_tokens      := aPrompt_tokens + aCompletion_tokens;
  end;

  // 3. Content blocks
  Respuesta := '';
  choices   := JGetArr(jObj, 'content');
  if Assigned(choices) then
  begin
    for I := 0 to choices.Count - 1 do
    begin
      jval := choices.Items[I];
      if not (jval is TJSONObject) then Continue;
      jContentItem := TJSONObject(jval);
      cType        := JGetStr(jContentItem, 'type');

      // A. Texto
      if cType = 'text' then
        Respuesta := Respuesta + JGetStr(jContentItem, 'text') + LineEnding;

      // B. Thinking
      if cType = 'thinking' then
      begin
        ThinkingContent := JGetStr(jContentItem, 'thinking');
        ResMsg.ReasoningContent := ResMsg.ReasoningContent + ThinkingContent;
        if JTryGetStr(jContentItem, 'signature', SigVal) then
          ResMsg.ThinkingSignature := SigVal;
        DoThinking(ResMsg, 'assistant', ThinkingContent, nil);
      end;

      // C. Citations
      if JTryGetArr(jContentItem, 'citations', jCitationsArr) then
      begin
        for J := 0 to jCitationsArr.Count - 1 do
        begin
          jCitData := jCitationsArr.Items[J];
          if not (jCitData is TJSONObject) then Continue;
          jCitObj := TJSONObject(jCitData);
          citType := JGetStr(jCitObj, 'type');

          if (citType = 'char_location') or (citType = 'page_location') or
             (citType = 'content_block_location') then
          begin
            LCitation      := TAiMsgCitation.Create;
            LCitation.Text := JGetStr(jCitObj, 'cited_text');
            LSource := TAiCitationSource.Create;
            LSource.SourceType       := cstDocument;
            LSource.DataSource.Title := JGetStr(jCitObj, 'document_title');
            LSource.DataSource.id    := IntToStr(JGetInt(jCitObj, 'document_index'));
            if citType = 'char_location' then
            begin
              LCitation.StartIndex := JGetInt(jCitObj, 'start_char_index');
              LCitation.EndIndex   := JGetInt(jCitObj, 'end_char_index');
            end
            else if citType = 'page_location' then
            begin
              LCitation.StartIndex := JGetInt(jCitObj, 'start_page_number');
              LCitation.EndIndex   := JGetInt(jCitObj, 'end_page_number');
            end
            else
            begin
              LCitation.StartIndex := JGetInt(jCitObj, 'start_block_index');
              LCitation.EndIndex   := JGetInt(jCitObj, 'end_block_index');
            end;
            LCitation.Sources.Add(LSource);
            ResMsg.Citations.Add(LCitation);
          end
          else if citType = 'web_search_result_location' then
          begin
            SearchItem       := TAiWebSearchItem.Create;
            SearchItem.&type := 'web_search_result_location';
            SearchItem.Title := JGetStr(jCitObj, 'title');
            SearchItem.Url   := JGetStr(jCitObj, 'url');
            ResMsg.WebSearchResponse.annotations.Add(SearchItem);

            LCitation      := TAiMsgCitation.Create;
            LCitation.Text := JGetStr(jCitObj, 'cited_text');
            LSource := TAiCitationSource.Create;
            LSource.SourceType       := cstWeb;
            LSource.DataSource.Title := JGetStr(jCitObj, 'title');
            LSource.DataSource.Url   := JGetStr(jCitObj, 'url');
            LCitation.Sources.Add(LSource);
            ResMsg.Citations.Add(LCitation);
          end;
        end;
      end;

      // D. Code Execution Output
      if (cType = 'tool_result') or (cType = 'code_execution_tool_result') then
      begin
        if JTryGetObj(jContentItem, 'content', jInnerContent) then
        begin
          if JGetStr(jInnerContent, 'type') = 'code_execution_result' then
          begin
            if JTryGetArr(jInnerContent, 'content', jResultArray) then
            begin
              for J := 0 to jResultArray.Count - 1 do
              begin
                if not (jResultArray.Items[J] is TJSONObject) then Continue;
                jResultContent := TJSONObject(jResultArray.Items[J]);
                if not JTryGetStr(jResultContent, 'file_id', Clave) then
                  Continue;

                FoundFileName := 'generated_file_' + Copy(Clave, 1, 8) + '.bin';
                ToolUseID := JGetStr(jContentItem, 'tool_use_id');

                if ToolUseID <> '' then
                begin
                  for ScanIdx := 0 to choices.Count - 1 do
                  begin
                    if not (choices.Items[ScanIdx] is TJSONObject) then Continue;
                    ScanObj := TJSONObject(choices.Items[ScanIdx]);
                    if not JTryGetStr(ScanObj, 'id', sId) then Continue;
                    if sId <> ToolUseID then Continue;
                    if JTryGetObj(ScanObj, 'input', InputObj) then
                    begin
                      CodeStr := JGetStr(InputObj, 'code');
                      // Heuristica: buscar extension para deducir nombre del archivo
                      ExtPos := 0;
                      if ExtPos = 0 then ExtPos := Pos('.wav',  CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.csv',  CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.png',  CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.pdf',  CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.mp3',  CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.json', CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.xlsx', CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.zip',  CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.txt',  CodeStr);
                      if ExtPos = 0 then ExtPos := Pos('.py',   CodeStr);
                      if ExtPos > 0 then
                        FoundFileName := 'output' + Copy(CodeStr, ExtPos, 5);
                    end;
                    Break;
                  end;
                end;

                NewFile := TAiMediaFile.Create;
                NewFile.IdFile   := Clave;
                NewFile.FileName := FoundFileName;
                try
                  DownloadFile(NewFile);
                  ResMsg.MediaFiles.Add(NewFile);
                except
                  NewFile.Free;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  Respuesta := Trim(Respuesta);

  // 4. Tool calls
  sToolCalls := '';
  if Assigned(choices) then
  begin
    jToolCallArray := ExtractToolCallJson(choices);
    if jToolCallArray.Count > 0 then
      sToolCalls := jToolCallArray.AsJSON;
    jToolCallArray.Free;
  end;

  if Assigned(choices) then
    LFunciones := ExtractToolCallFromJson(choices)
  else
    LFunciones := TAiToolsFunctions.Create;

  // 5. Actualizar estado y tokens
  FLastContent      := Respuesta;
  Prompt_tokens     := Prompt_tokens     + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens      := Total_tokens      + aTotal_tokens;

  ResMsg.Prompt_tokens     := aPrompt_tokens;
  ResMsg.Completion_tokens := aCompletion_tokens;
  ResMsg.Total_tokens      := aTotal_tokens;
  ResMsg.Cached_tokens     := aCached_tokens;

  if sToolCalls = '' then
  begin
    ResMsg.Role       := Role;
    ResMsg.Model      := LModel;
    ResMsg.Tool_calls := sToolCalls;
    ResMsg.Prompt     := ResMsg.Prompt + Respuesta;
    DoProcessResponse(AskMsg, ResMsg, Respuesta);
  end
  else
  begin
    // Mensaje temporal con tool use blocks para el historial
    ToolMsg                   := TAiChatMessage.Create(Respuesta, Role);
    ToolMsg.Tool_calls        := sToolCalls;
    ToolMsg.ReasoningContent  := ResMsg.ReasoningContent;
    ToolMsg.ThinkingSignature := ResMsg.ThinkingSignature;
    ToolMsg.Id                := FMessages.Count + 1;
    FMessages.Add(ToolMsg);
  end;

  // 6. Ejecucion de herramientas (secuencial en FPC)
  try
    if LFunciones.Count > 0 then
    begin
      for Clave in LFunciones.Keys do
      begin
        ToolCall        := LFunciones[Clave];
        ToolCall.ResMsg := ResMsg;
        ToolCall.AskMsg := AskMsg;
        DoCallFunction(ToolCall);

        ToolMsg    := TAiChatMessage.Create(ToolCall.Response, 'user',
            ToolCall.Id, ToolCall.Name);
        ToolMsg.Id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
      end;

      // Re-llamar al modelo con los resultados de herramientas
      SaveAsync    := Asynchronous;
      Asynchronous := False;
      Self.Run(nil, ResMsg);
      Asynchronous := SaveAsync;
    end
    else
    begin
      // Extraccion de archivos de codigo embebidos (si habilitado)
      if tfc_ExtracttextFile in NativeOutputFiles then
      begin
        code := TMarkdownCodeExtractor.Create;
        try
          CodeFiles := code.ExtractCodeFiles(Respuesta);
          for CodeFile in CodeFiles do
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
      // Pasar nil como jObj para evitar dangling pointer en modo async
      DoDataEnd(ResMsg, Role, Respuesta, nil);
    end;
  finally
    LFunciones.Free;
  end;
end;

// ===========================================================================
//  InternalAddMessage — manejo de archivos adjuntos
// ===========================================================================
function TAiClaudeChat.InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
var
  Respuesta : string;
  MF        : TAiMediaFile;
  Procesado : Boolean;
  I         : Integer;
begin
  try
    aMsg.Id := FMessages.Count + 1;
    FMessages.Add(aMsg);

    // Notificar via propiedad publica (FOnAddMessage es privado en la base)
    if Assigned(OnAddMessage) then
      OnAddMessage(Self, aMsg, nil, aMsg.Role, aMsg.Prompt);

    // Preprocesar archivos adjuntos (ej: transcripcion local, OCR)
    for I := 0 to aMsg.MediaFiles.Count - 1 do
    begin
      MF        := aMsg.MediaFiles[I];
      Respuesta := '';
      Procesado := False;
      DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado);
      MF.Procesado    := Procesado;
      MF.Transcription := Respuesta;
    end;

    FLastPrompt := aMsg.Prompt;

    if Assigned(OnBeforeSendMessage) then
      OnBeforeSendMessage(Self, aMsg);

    Result := aMsg;
  except
    on E: Exception do
      raise Exception.Create('Error en InternalAddMessage: ' + E.Message);
  end;
end;

// ===========================================================================
//  GetMessages — formato de mensajes Claude
// ===========================================================================
function TAiClaudeChat.GetMessages: TJSONArray;
var
  LMessage          : TAiChatMessage;
  LMessageObj       : TJSONObject;
  LPartObj          : TJSONObject;
  LSourceObj        : TJSONObject;
  LThinkingObj      : TJSONObject;
  LContentArray     : TJSONArray;
  LToolUseArray     : TJSONArray;
  jCache            : TJSONObject;
  jCit              : TJSONObject;
  LMediaFile        : TAiMediaFile;
  MediaArr          : TAiMediaFilesArray;
  IsCodeExecEnabled : Boolean;
  TargetCategories  : TAiFileCategories;
  bHasContent       : Boolean;
  B64Head           : string;
  RealMime          : string;
  LToolCallsJson    : string;
  I, J              : Integer;
begin
  Result := TJSONArray.Create;

  IsCodeExecEnabled := Tcm_CodeInterpreter in ChatMediaSupports;
  if IsCodeExecEnabled then
    TargetCategories := [Low(TAiFileCategory) .. High(TAiFileCategory)]
  else
    TargetCategories := NativeInputFiles;

  for I := 0 to FMessages.Count - 1 do
  begin
    LMessage    := FMessages[I];
    LMessageObj := TJSONObject.Create;
    LMessageObj.Add('role', LMessage.Role);
    LContentArray := TJSONArray.Create;

    // ---- CASO 1: Resultado de herramienta (user con ToolCallId) ----
    if (LMessage.Role = 'user') and (LMessage.ToolCallId <> '') then
    begin
      LPartObj := TJSONObject.Create;
      LPartObj.Add('type',        'tool_result');
      LPartObj.Add('tool_use_id', LMessage.ToolCallId);
      LPartObj.Add('content',     LMessage.Prompt);
      LContentArray.Add(LPartObj);
    end

    // ---- CASO 2: Mensaje del asistente ----
    else if LMessage.Role = 'assistant' then
    begin
      // A. Thinking block (si thinking habilitado y tiene signature)
      if FEnableThinking and (LMessage.ReasoningContent <> '') and
         (LMessage.ThinkingSignature <> '') then
      begin
        LThinkingObj := TJSONObject.Create;
        LThinkingObj.Add('type',      'thinking');
        LThinkingObj.Add('thinking',   LMessage.ReasoningContent);
        LThinkingObj.Add('signature',  LMessage.ThinkingSignature);
        LContentArray.Add(LThinkingObj);
      end;

      // B. Texto
      if LMessage.Prompt <> '' then
      begin
        LPartObj := TJSONObject.Create;
        LPartObj.Add('type', 'text');
        LPartObj.Add('text', LMessage.Prompt);
        if LMessage.CacheControl then
        begin
          jCache := TJSONObject.Create;
          jCache.Add('type', 'ephemeral');
          LPartObj.Add('cache_control', jCache);
        end;
        LContentArray.Add(LPartObj);
      end;

      // C. Tool use blocks (guardados como JSON en Tool_calls)
      if LMessage.Tool_calls <> '' then
      begin
        try
          LToolCallsJson := LMessage.Tool_calls;
          LToolUseArray  := TJSONArray(GetJSON(LToolCallsJson));
          if Assigned(LToolUseArray) then
          begin
            for J := 0 to LToolUseArray.Count - 1 do
              LContentArray.Add(LToolUseArray.Items[J].Clone as TJSONObject);
            LToolUseArray.Free;
          end;
        except
          on E: Exception do
            LogDebug('Tool calls parse error: ' + E.Message);
        end;
      end;
    end

    // ---- CASO 3: Mensaje de usuario (texto + archivos) ----
    else
    begin
      bHasContent := False;

      // A. Texto del mensaje
      if LMessage.Prompt <> '' then
      begin
        LPartObj := TJSONObject.Create;
        LPartObj.Add('type', 'text');
        LPartObj.Add('text', LMessage.Prompt);
        if LMessage.CacheControl then
        begin
          jCache := TJSONObject.Create;
          jCache.Add('type', 'ephemeral');
          LPartObj.Add('cache_control', jCache);
        end;
        LContentArray.Add(LPartObj);
        bHasContent := True;
      end;

      // B. Archivos adjuntos
      MediaArr := LMessage.MediaFiles.GetMediaList(TargetCategories, False);
      for J := 0 to Length(MediaArr) - 1 do
      begin
        LMediaFile := MediaArr[J];
        LPartObj   := TJSONObject.Create;

        // Imagen
        if LMediaFile.FileCategory = Tfc_Image then
        begin
          LPartObj.Add('type', 'image');
          LSourceObj := TJSONObject.Create;
          if LMediaFile.IdFile <> '' then
          begin
            LSourceObj.Add('type',    'file');
            LSourceObj.Add('file_id', LMediaFile.IdFile);
          end
          else
          begin
            LSourceObj.Add('type', 'base64');
            RealMime := LMediaFile.MimeType;
            B64Head  := Copy(LMediaFile.Base64, 1, 15);
            if Copy(B64Head, 1, 7) = 'iVBORw0' then
              RealMime := 'image/png'
            else if Copy(B64Head, 1, 4) = '/9j/' then
              RealMime := 'image/jpeg'
            else if Copy(B64Head, 1, 6) = 'R0lGOD' then
              RealMime := 'image/gif'
            else if Copy(B64Head, 1, 5) = 'UklGR' then
              RealMime := 'image/webp';
            LSourceObj.Add('media_type', RealMime);
            LSourceObj.Add('data',       LMediaFile.Base64);
          end;
          LPartObj.Add('source', LSourceObj);
        end

        // Container upload (Code Interpreter)
        else if IsCodeExecEnabled and (LMediaFile.IdFile <> '') then
        begin
          LPartObj.Add('type',    'container_upload');
          LPartObj.Add('file_id', LMediaFile.IdFile);
        end

        // Documento (PDF u otros)
        else
        begin
          LPartObj.Add('type', 'document');
          LSourceObj := TJSONObject.Create;
          if LMediaFile.IdFile <> '' then
          begin
            LSourceObj.Add('type',    'file');
            LSourceObj.Add('file_id', LMediaFile.IdFile);
          end
          else
          begin
            LSourceObj.Add('type',       'base64');
            LSourceObj.Add('media_type', LMediaFile.MimeType);
            LSourceObj.Add('data',       LMediaFile.Base64);
          end;
          LPartObj.Add('source', LSourceObj);

          if LMediaFile.Title <> '' then
            LPartObj.Add('title', LMediaFile.Title)
          else if LMediaFile.FileName <> '' then
            LPartObj.Add('title', LMediaFile.FileName);

          if LMediaFile.Context <> '' then
            LPartObj.Add('context', LMediaFile.Context);

          if LMediaFile.EnableCitations then
          begin
            jCit := TJSONObject.Create;
            jCit.Add('enabled', TJSONBoolean.Create(True));
            LPartObj.Add('citations', jCit);
          end;
        end;

        if LMediaFile.CacheControl then
        begin
          jCache := TJSONObject.Create;
          jCache.Add('type', 'ephemeral');
          LPartObj.Add('cache_control', jCache);
        end;

        LContentArray.Add(LPartObj);
        bHasContent := True;
      end;

      if not bHasContent then
      begin
        LPartObj := TJSONObject.Create;
        LPartObj.Add('type', 'text');
        LPartObj.Add('text', ' ');
        LContentArray.Add(LPartObj);
      end;
    end;

    LMessageObj.Add('content', LContentArray);
    Result.Add(LMessageObj);
  end;
end;

// ===========================================================================
//  ExtractToolCallFromJson — extrae tool_use blocks como TAiToolsFunctions
// ===========================================================================
function TAiClaudeChat.ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions;
var
  jItem   : TJSONObject;
  Fun     : TAiToolsFunction;
  Arg     : TJSONObject;
  I       : Integer;
  ArgIdx  : Integer;
begin
  Result := TAiToolsFunctions.Create;
  if not Assigned(jChoices) then Exit;
  for I := 0 to jChoices.Count - 1 do
  begin
    if not (jChoices.Items[I] is TJSONObject) then Continue;
    jItem := TJSONObject(jChoices.Items[I]);
    if JGetStr(jItem, 'type') <> 'tool_use' then Continue;

    Fun      := TAiToolsFunction.Create;
    Fun.Id   := JGetStr(jItem, 'id');
    Fun.Tipo := 'function';
    Fun.Name := JGetStr(jItem, 'name');
    if JTryGetObj(jItem, 'input', Arg) then
    begin
      Fun.Arguments := Arg.FormatJSON;
      for ArgIdx := 0 to Arg.Count - 1 do
        Fun.Params.Values[Arg.Names[ArgIdx]] := Arg.Items[ArgIdx].AsString;
    end;
    Result.Add(Fun.Id, Fun);
  end;
end;

// ===========================================================================
//  ExtractToolCallJson — extrae tool_use blocks como TJSONArray (para historial)
// ===========================================================================
function TAiClaudeChat.ExtractToolCallJson(jChoices: TJSONArray): TJSONArray;
var
  jItem : TJSONObject;
  jObj  : TJSONObject;
  Arg   : TJSONObject;
  I     : Integer;
begin
  Result := TJSONArray.Create;
  if not Assigned(jChoices) then Exit;
  for I := 0 to jChoices.Count - 1 do
  begin
    if not (jChoices.Items[I] is TJSONObject) then Continue;
    jItem := TJSONObject(jChoices.Items[I]);
    if JGetStr(jItem, 'type') <> 'tool_use' then Continue;

    jObj := TJSONObject.Create;
    jObj.Add('type', 'tool_use');
    jObj.Add('id',   JGetStr(jItem, 'id'));
    jObj.Add('name', JGetStr(jItem, 'name'));
    if JTryGetObj(jItem, 'input', Arg) then
      jObj.Add('input', TJSONObject(Arg.Clone));
    Result.Add(jObj);
  end;
end;

// ===========================================================================
//  DoCallFunction — despacha llamadas de herramientas
// ===========================================================================
procedure TAiClaudeChat.DoCallFunction(ToolCall: TAiToolsFunction);
begin
  // 1. Bash / Shell nativo
  if ToolCall.Name = 'bash' then
  begin
    if Assigned(OnCallToolFunction) then
      OnCallToolFunction(Self, ToolCall);
    // ShellTool.Active / .Execute disponibles cuando el stub sea portado a Fase 2
    {if (ToolCall.Response = '') and Assigned(ShellTool) then
    begin
      if not ShellTool.Active then
        ShellTool.Active := True;
      ToolCall.Response := ShellTool.Execute(ToolCall.Id, ToolCall.Arguments);
    end;}
    Exit;
  end;

  // 2. Text Editor nativo
  // TextEditorTool.Execute disponible cuando el stub sea portado a Fase 2
  {if ((ToolCall.Name = 'str_replace_based_edit_tool') or
      (ToolCall.Name = 'str_replace_editor')) and Assigned(TextEditorTool) then
  begin
    ToolCall.Response := TextEditorTool.Execute(ToolCall.Arguments);
    Exit;
  end;}

  // 3. AiFunctions externas
  if Assigned(AiFunctions) and AiFunctions.DoCallFunction(ToolCall) then
    Exit;

  // 4. Evento de usuario
  if Assigned(OnCallToolFunction) then
    OnCallToolFunction(Self, ToolCall);
end;

// ===========================================================================
//  GetModels — lista modelos via GET /v1/models
// ===========================================================================
class function TAiClaudeChat.GetModels(aApiKey: string; aUrl: string): TStringList;
var
  Client    : TFPHTTPClient;
  RespStream: TStringStream;
  jRes      : TJSONObject;
  jArr      : TJSONArray;
  sModel    : string;
  sUrl      : string;
  I         : Integer;
begin
  Result := TStringList.Create;
  if aUrl <> '' then
    sUrl := aUrl + 'models'
  else
    sUrl := GlAIUrl + 'models';

  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('x-api-key',         aApiKey);
    Client.AddHeader('anthropic-version', CLAUDE_API_VERSION);
    Client.AddHeader('content-type',      'application/json');
    try
      Client.Get(sUrl, RespStream);
    except
      on E: Exception do
        raise Exception.CreateFmt('Error al obtener modelos de Claude: %s', [E.Message]);
    end;

    if Client.ResponseStatusCode = 200 then
    begin
      jRes := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jRes) then
      try
        jArr := JGetArr(jRes, 'data');
        if Assigned(jArr) then
          for I := 0 to jArr.Count - 1 do
            if jArr.Items[I] is TJSONObject then
            begin
              sModel := JGetStr(TJSONObject(jArr.Items[I]), 'id');
              if sModel <> '' then
                Result.Add(sModel);
            end;
      finally
        jRes.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Error modelos Claude: %d - %s',
          [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    RespStream.Free;
    Client.Free;
  end;
end;

// ===========================================================================
//  Gestion de Archivos (Files API)
// ===========================================================================

function TAiClaudeChat.UploadFile(aMediaFile: TAiMediaFile): string;
var
  Client     : TFPHTTPClient;
  Body       : TMemoryStream;
  RespStream : TStringStream;
  jObj       : TJSONObject;
  Boundary   : string;
  HdrList    : TStringList;
  HIdx       : Integer;
begin
  Result := '';
  if (not Assigned(aMediaFile)) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Empty file content');

  Boundary   := 'ClaudeBoundary' + IntToStr(Random(999999));
  Body       := BuildMultipart(Boundary, aMediaFile);
  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  HdrList    := GetFileHeadersList;
  try
    for HIdx := 0 to (HdrList.Count div 2) - 1 do
      Client.AddHeader(HdrList[HIdx * 2], HdrList[HIdx * 2 + 1]);
    Client.AddHeader('Content-Type',
        'multipart/form-data; boundary=' + Boundary);
    Client.RequestBody := Body;
    try
      Client.HTTPMethod('POST', Url + 'files', RespStream, [200, 201]);
    finally
      Client.RequestBody := nil;
    end;

    if (Client.ResponseStatusCode = 200) or
       (Client.ResponseStatusCode = 201) then
    begin
      jObj := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jObj) then
      try
        Result            := JGetStr(jObj, 'id');
        aMediaFile.IdFile := Result;
      finally
        jObj.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Upload Error: %d - %s',
          [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    Body.Free;
    RespStream.Free;
    Client.Free;
    HdrList.Free;
  end;
end;

function TAiClaudeChat.DownloadFile(aMediaFile: TAiMediaFile): string;
var
  Client    : TFPHTTPClient;
  MemStream : TMemoryStream;
  HdrList   : TStringList;
  HIdx      : Integer;
begin
  Result := '';
  if (not Assigned(aMediaFile)) or (aMediaFile.IdFile = '') then
    raise Exception.Create('File ID is missing');

  Client    := TFPHTTPClient.Create(nil);
  MemStream := TMemoryStream.Create;
  HdrList   := GetFileHeadersList;
  try
    for HIdx := 0 to (HdrList.Count div 2) - 1 do
      Client.AddHeader(HdrList[HIdx * 2], HdrList[HIdx * 2 + 1]);
    try
      Client.Get(Url + 'files/' + aMediaFile.IdFile + '/content', MemStream);
    except
      on E: Exception do
        raise Exception.Create('Download Error: ' + E.Message);
    end;

    if Client.ResponseStatusCode = 200 then
    begin
      MemStream.Position := 0;
      aMediaFile.Content.CopyFrom(MemStream, 0);
      aMediaFile.Content.Position := 0;
      if aMediaFile.FileName = '' then
        aMediaFile.FileName := aMediaFile.IdFile + '.bin';
      Result := aMediaFile.IdFile;
    end
    else
      raise Exception.CreateFmt('Download Error: %d',
          [Client.ResponseStatusCode]);
  finally
    MemStream.Free;
    Client.Free;
    HdrList.Free;
  end;
end;

function TAiClaudeChat.CheckFileState(aMediaFile: TAiMediaFile): string;
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

function TAiClaudeChat.DeleteFile(aMediaFile: TAiMediaFile): string;
var
  Client     : TFPHTTPClient;
  RespStream : TStringStream;
  jObj       : TJSONObject;
  HdrList    : TStringList;
  HIdx       : Integer;
begin
  Result     := '';
  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  HdrList    := GetFileHeadersList;
  try
    for HIdx := 0 to (HdrList.Count div 2) - 1 do
      Client.AddHeader(HdrList[HIdx * 2], HdrList[HIdx * 2 + 1]);
    try
      Client.HTTPMethod('DELETE', Url + 'files/' + aMediaFile.IdFile,
          RespStream, [200]);
    except
      on E: Exception do
        raise Exception.Create('Delete Error: ' + E.Message);
    end;
    if Client.ResponseStatusCode = 200 then
    begin
      jObj := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jObj) then
      begin
        Result := JGetStr(jObj, 'id');
        jObj.Free;
      end;
    end;
  finally
    RespStream.Free;
    Client.Free;
    HdrList.Free;
  end;
end;

function TAiClaudeChat.RetrieveFile(aFileId: string): TAiMediaFile;
var
  Client     : TFPHTTPClient;
  RespStream : TStringStream;
  jObj       : TJSONObject;
  HdrList    : TStringList;
  HIdx       : Integer;
begin
  Result := nil;
  if aFileId = '' then Exit;

  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  HdrList    := GetFileHeadersList;
  try
    for HIdx := 0 to (HdrList.Count div 2) - 1 do
      Client.AddHeader(HdrList[HIdx * 2], HdrList[HIdx * 2 + 1]);
    try
      Client.Get(Url + 'files/' + aFileId, RespStream);
    except
      on E: Exception do
        raise Exception.Create('RetrieveFile Error: ' + E.Message);
    end;
    if Client.ResponseStatusCode = 200 then
    begin
      jObj := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jObj) then
      try
        Result          := TAiMediaFile.Create;
        Result.IdFile   := JGetStr(jObj, 'id');
        Result.FileName := JGetStr(jObj, 'filename');
      finally
        jObj.Free;
      end;
    end
    else
      raise Exception.Create('Retrieve File Error');
  finally
    RespStream.Free;
    Client.Free;
    HdrList.Free;
  end;
end;

function TAiClaudeChat.RetrieveFileList: TAiMediaFiles;
var
  Client     : TFPHTTPClient;
  RespStream : TStringStream;
  jRes       : TJSONObject;
  jArr       : TJSONArray;
  MF         : TAiMediaFile;
  HdrList    : TStringList;
  HIdx, I    : Integer;
begin
  Result     := TAiMediaFiles.Create;
  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  HdrList    := GetFileHeadersList;
  try
    for HIdx := 0 to (HdrList.Count div 2) - 1 do
      Client.AddHeader(HdrList[HIdx * 2], HdrList[HIdx * 2 + 1]);
    try
      Client.Get(Url + 'files', RespStream);
    except
      on E: Exception do
        raise Exception.Create('RetrieveFileList Error: ' + E.Message);
    end;
    if Client.ResponseStatusCode = 200 then
    begin
      jRes := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jRes) then
      try
        jArr := JGetArr(jRes, 'data');
        if Assigned(jArr) then
          for I := 0 to jArr.Count - 1 do
            if jArr.Items[I] is TJSONObject then
            begin
              MF          := TAiMediaFile.Create;
              MF.IdFile   := JGetStr(TJSONObject(jArr.Items[I]), 'id');
              MF.FileName := JGetStr(TJSONObject(jArr.Items[I]), 'filename');
              Result.Add(MF);
            end;
      finally
        jRes.Free;
      end;
    end
    else
      raise Exception.CreateFmt('RetrieveFileList Error: %d - %s',
          [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    RespStream.Free;
    Client.Free;
    HdrList.Free;
  end;
end;

function TAiClaudeChat.UploadFileToCache(aMediaFile: TAiMediaFile;
    aTTL_Seconds: Integer): string;
begin
  Result := UploadFile(aMediaFile);
end;

procedure TAiClaudeChat.ConfigureAutoContextClearing(TriggerTokens: Integer;
    KeepLast: Integer);
begin
  FContextConfig.Clear;
  FContextConfig.AddRule_ClearTools(TriggerTokens, KeepLast, 0);
end;

// ===========================================================================
//  Initialization
// ===========================================================================
initialization
  TAiChatFactory.Instance.RegisterDriver(TAiClaudeChat);

end.
