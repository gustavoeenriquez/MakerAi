unit uDmTelegramBot;

// =============================================================================
// MakerAI - Demo 058: FMX Telegram Bot  (Fase 1 + 2 + 3 + 4)
//
// Flujo texto:
//   Telegram long poll -> ProcessUpdate
//       -> IsAuthorized / HandleCommand / GetOrCreateChat
//       -> TAiChatConnection.AddMessageAndRun  [LLM]
//       -> SendTgMessage
//
// Flujo voz (Fase 4):
//   Telegram voice message -> DownloadTgFile (OGG)
//       -> TranscribeAudio  (MCPService mcp-transcribe  ||  TAiOpenAiAudio)
//       -> TAiChatConnection.AddMessageAndRun  [LLM]
//       -> SynthesizeVoice  (MCPService mcp-tts  ||  TAiOpenAiAudio)
//       -> SendTgVoice      (multipart MP3)
//
// Skills Fase 1: shell_exec, file_read, file_write, file_list
// Skills Fase 2: memory_save/get/delete/list, agent_run
// Fase 3: permisos whitelist, historial persistente, stats
// Fase 4: voz entrada+salida via MCPService o TAiOpenAiAudio directo
//         CheckVoiceCapabilities consulta PPM registry al arrancar
//
// Comandos: /start /help /clear /status /voice /voiceon /voiceoff
// Admin:    /allow /deny /users /whitelist /openmode
//
// PPM registry: https://registry.pascalai.org  (mcp-transcribe, mcp-tts)
// MCPService:   FMCPToolsURL (default vacio = modo directo)
// Datos:        %APPDATA%\MakerAI\
// =============================================================================

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Net.Mime,
  System.Generics.Collections,
  System.SyncObjs,
  System.IOUtils,
  System.DateUtils,
  System.StrUtils,
  Winapi.Windows,
  uMakerAi.Core,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.OpenAI.Audio,
  uMakerAi.Gemini.Speech,
  uMakerAi.Agents.IAiTool,
  uMakerAi.Agents.ToolRegistry,
  uBotMemory, uMakerAi.Prompts;

type
  TTelegramLogEvent = procedure(const ADirection, AText: string) of object;

  // Configuracion de un proveedor en una categoria
  TProviderConfig = record
    Driver : string;  // 'Claude', 'OpenAI', 'Gemini', 'Ollama', 'ElevenLabs', etc.
    Model  : string;  // modelo especifico del proveedor
    ApiKey : string;  // '@ENV_VAR' o key directa
    Extra  : string;  // voz, base_url, idioma, etc. (depende de categoria)
    constructor Create(const ADriver, AModel, AApiKey, AExtra: string);
    function IsEmpty: Boolean;
    procedure Clear;
  end;

  // Par primario + fallback para cada categoria
  TCategoryConfig = record
    Primary  : TProviderConfig;
    Fallback : TProviderConfig;
  end;

  // Config completa de proveedores por categoria
  TBotProviders = record
    LLM       : TCategoryConfig;
    TTS       : TCategoryConfig;
    STT       : TCategoryConfig;
    Image     : TCategoryConfig;
    Video     : TCategoryConfig;
    Embeddings: TCategoryConfig;
  end;

  TDmTelegramBot = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    // ── Fase 1: Multi-usuario ──────────────────────────────────────────────
    FChats    : TObjectDictionary<Int64, TAiChatConnection>;
    FFunctions: TAiFunctions;
    FCSChats  : TCriticalSection;

    // ── Fase 1: Polling ───────────────────────────────────────────────────
    FToken   : string;
    FOffset  : Int64;
    FPolling : Boolean;
    FThread  : TThread;
    FOnLog   : TTelegramLogEvent;

    // ── Fase 2: Memory ────────────────────────────────────────────────────
    FMemory : TBotMemory;

    // ── Fase 2: chat_id activo (para progress msgs del pipeline) ──────────
    FCurrentChatId: Int64;

    // ── Fase 3: Permisos ──────────────────────────────────────────────────
    FOwnerChatId  : Int64;
    FWhitelistMode: Boolean;
    FAllowedIds   : TDictionary<Int64, string>;
    FConfigFile   : string;
    FConfigLock   : TCriticalSection;
    FLastToken    : string;

    // ── Fase 3: Stats ─────────────────────────────────────────────────────
    FTotalMessages : Integer;
    FTotalToolCalls: Integer;
    FBotStartTime  : TDateTime;

    // ── Fase 4: Voz ───────────────────────────────────────────────────────
    FVoiceEnabled : Boolean;  // responde con voz cuando el input es voz
    FMCPToolsURL  : string;   // URL MCPService (vacio = TAiOpenAiAudio directo)

    // ── Proveedores configurables por categoria ───────────────────────────
    FProviders  : TBotProviders;

    // ── PPM auto-install ──────────────────────────────────────────────────
    FInstalledClients : TObjectList<TObject>;  // lifetime de clientes MCP instalados
    FInstalledPkgs    : TStringList;           // paquetes con proceso MCP activo en sesión
    FPPMStateFile     : string;                // JSON con paquetes a restaurar al reiniciar

    // ── Base dir ──────────────────────────────────────────────────────────
    FDataDir: string;

    // Chat management
    function  GetOrCreateChat(AChatId: Int64): TAiChatConnection;
    function  CreateNewChat: TAiChatConnection;

    // Tool registration & dispatch
    procedure RegisterTools;
    procedure HandleToolCall(Sender: TObject; FunctionAction: TFunctionActionItem;
                FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure OnChatError(Sender: TObject; const ErrorMsg: string; E: Exception; const AResponse: IHTTPResponse);

    // ── Fase 1: System tools ──────────────────────────────────────────────
    function  ToolShellExec(const Args: TJSONObject): string;
    function  ToolFileRead (const Args: TJSONObject): string;
    function  ToolFileWrite(const Args: TJSONObject): string;
    function  ToolFileList (const Args: TJSONObject): string;
    function  RunShell(const ACmd: string; var AOutput: string): Integer;

    // ── Fase 2: Memory tools ──────────────────────────────────────────────
    function  ToolMemorySave  (const Args: TJSONObject): string;
    function  ToolMemoryGet   (const Args: TJSONObject): string;
    function  ToolMemoryDelete(const Args: TJSONObject): string;
    function  ToolMemoryList  (const Args: TJSONObject): string;
    function  ToolMemorySearch(const Args: TJSONObject): string;

    // ── Fase 2: Agent tool ────────────────────────────────────────────────
    function  ToolAgentRun      (const Args: TJSONObject): string;
    function  RunStep(const ASysPrompt, AInput: string): string;
    function  PipelineAnalyze   (const Task: string): string;
    function  PipelineCodeReview(const Task: string): string;

    // ── Fase 3: Permisos ──────────────────────────────────────────────────
    procedure LoadConfig;
    procedure SaveConfig;
    procedure LoadProviders(AObj: TJSONObject);
    procedure SaveProvidersTo(AObj: TJSONObject);
    function  IsAuthorized(AChatId: Int64): Boolean;
    function  HandleCommand(AChatId: Int64; const AText, AUsername: string): Boolean;

    // ── Fase 3: Historial ─────────────────────────────────────────────────
    function  HistoryPath(AChatId: Int64): string;
    procedure SaveHistory(AChatId: Int64; AChat: TAiChatConnection);
    procedure LoadHistory(AChatId: Int64; AChat: TAiChatConnection);

    // ── Fase 4: Voz ───────────────────────────────────────────────────────
    function  DownloadTgFile(const AFileId: string): string;
    function  TranscribeAudio(const AFilePath: string): string;
    function  SynthesizeVoice(const AText: string): string;
    procedure SendTgVoice(const AChatId: Int64; const AFilePath: string);
    function  ToolTgVoice(const Args: TJSONObject): string;
    function  CallMCPTool(const AToolName: string; const AArgs: TJSONObject): string;
    procedure CheckVoiceCapabilities;

    // ── PPM helpers ───────────────────────────────────────────────────────
    // Registra un IAiTool del registry en TAiFunctions para que el LLM pueda llamarlo.
    procedure RegisterIAiToolInFunctions(const ATool: IAiTool);
    // Devuelve el primer tool disponible cuyo SourceId coincide con ASourceId.
    function  FindMCPToolBySource(const ASourceId: string): IAiTool;
    // Extrae texto de una respuesta MCP (content array o campo text directo).
    function  ExtractMCPText(const AResult: TJSONObject): string;
    // Busca AQuery en PPM, descarga, instala y registra en TAiFunctions.
    // Devuelve True si instaló exitosamente. APkgName y AToolsList son informativos.
    function  InstallMCPPackage(const AQuery: string;
                                out APkgName, AToolsList: string): Boolean;
    // Implementación del tool ppm_install que el LLM puede llamar.
    function  ToolPPMInstall(const Args: TJSONObject): string;
    // Lista los paquetes PPM instalados en esta sesión.
    function  ToolPPMList(const Args: TJSONObject): string;
    // Limpia todos los paquetes PPM instalados.
    function  ToolPPMClear(const Args: TJSONObject): string;
    // Persiste FInstalledPkgs en FPPMStateFile.
    procedure SavePPMState;
    // Lee FPPMStateFile y re-instala los paquetes guardados.
    procedure LoadAndRestorePPMState;
    // Bridge nativo para mcp-telegram (schema-only en PPM).
    // Soporta operation: send_message, get_updates.
    function  ToolNativeTelegram(const Args: TJSONObject): string;

    // Telegram helpers
    function  ResolveToken(const AToken: string): string;
    procedure SendTgMessage(const AChatId: Int64; const AText: string);
    procedure ProcessUpdate(const AUpdate: TJSONObject);
    procedure PollLoop;
    procedure DoLog(const ADir, AText: string);
  public
    procedure Start(const AToken: string);
    procedure Stop;
    function  UserCount: Integer;
    function  Uptime: string;
    property  OnLog           : TTelegramLogEvent read FOnLog write FOnLog;
    property  MessageCount    : Integer           read FTotalMessages;
    property  ToolCallCount   : Integer           read FTotalToolCalls;
    property  WhitelistMode   : Boolean           read FWhitelistMode;
    property  OwnerChatId     : Int64             read FOwnerChatId;
    property  LastToken       : string            read FLastToken;
    property  VoiceEnabled    : Boolean           read FVoiceEnabled;
    property  MCPToolsURL     : string            read FMCPToolsURL;
    property  Providers       : TBotProviders     read FProviders write FProviders;
    procedure SaveProviders;
  end;

var
  DmTelegramBot: TDmTelegramBot;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

// =============================================================================
// TProviderConfig
// =============================================================================

constructor TProviderConfig.Create(const ADriver, AModel, AApiKey, AExtra: string);
begin
  Driver := ADriver; Model := AModel; ApiKey := AApiKey; Extra := AExtra;
end;

function TProviderConfig.IsEmpty: Boolean;
begin
  Result := (Driver = '') and (Model = '');
end;

procedure TProviderConfig.Clear;
begin
  Driver := ''; Model := ''; ApiKey := ''; Extra := '';
end;

const
  TG_API          = 'https://api.telegram.org/bot';
  TG_FILE_API     = 'https://api.telegram.org/file/bot';
  POLL_SECS       = 25;
  HTTP_TO         = (POLL_SECS + 10) * 1000;
  MAX_RESP        = 3800;
  MAX_HISTORY_MSG = 40;

  SYS_PROMPT =
    'Eres un potente asistente de IA accesible v'#237'a Telegram. ' +
    'Herramientas disponibles:' + #10 +
    'SISTEMA : shell_exec, file_read, file_write, file_list' + #10 +
    'MEMORIA : memory_save, memory_get, memory_delete, memory_list, memory_search' + #10 +
    'AGENTES : agent_run (tipo: "analyze" o "code_review")' + #10 +
    'VOZ     : tg_voice (s'#237'ntesis de voz — env'#237'a un audio al chat de Telegram)' + #10 +
    'PPM     : ppm_install (instala cualquier capacidad externa desde el registro PPM)' + #10#10 +
    'Regla clave: si necesitas una herramienta que no est'#225' en la lista (clima, base de datos, ' +
    'GitHub, correo, traducci'#243'n, etc.), llama primero a ppm_install{"capability":"keyword"}. ' +
    'Tras la instalaci'#243'n, las nuevas herramientas quedan disponibles de inmediato.' + #10 +
    'Otras reglas: s'#233' conciso, usa texto plano (sin markdown), ' +
    'usa las herramientas de forma proactiva cuando el usuario pida hacer algo en el sistema ' +
    'o quiera recordar informaci'#243'n. ' +
    'Pide confirmaci'#243'n antes de operaciones destructivas. ' +
    'Responde siempre en espa'#241'ol.';

  PPM_REGISTRY = 'https://registry.pascalai.org';

// =============================================================================
// Lifecycle
// =============================================================================

procedure TDmTelegramBot.DataModuleCreate(Sender: TObject);
begin
  FDataDir := TPath.Combine(GetEnvironmentVariable('APPDATA'), 'MakerAI');

  // Fase 1
  FCSChats   := TCriticalSection.Create;
  FChats     := TObjectDictionary<Int64, TAiChatConnection>.Create([doOwnsValues]);
  FFunctions := TAiFunctions.Create(nil);
  FPolling   := False;
  FThread    := nil;
  FOffset    := 0;

  // Fase 2 — defaults de proveedores (se sobreescriben con LoadConfig)
  FProviders.LLM.Primary   := TProviderConfig.Create('Claude',  'claude-haiku-4-5-20251001',     '@CLAUDE_API_KEY',  '');
  FProviders.LLM.Fallback  := TProviderConfig.Create('OpenAI',  'gpt-4o-mini',                   '@OPENAI_API_KEY',  '');
  FProviders.TTS.Primary   := TProviderConfig.Create('Gemini',  'gemini-2.5-flash-preview-tts',  '@GEMINI_API_KEY',  'Aoede');
  FProviders.TTS.Fallback  := TProviderConfig.Create('OpenAI',  'tts-1',                         '@OPENAI_API_KEY',  'shimmer');
  FProviders.STT.Primary   := TProviderConfig.Create('OpenAI',  'whisper-1',                     '@OPENAI_API_KEY',  'es');
  FProviders.STT.Fallback  := TProviderConfig.Create('Gemini',  'gemini-3-flash-preview',        '@GEMINI_API_KEY',  '');
  FProviders.Image.Primary := TProviderConfig.Create('OpenAI',  'dall-e-3',                      '@OPENAI_API_KEY',  '1024x1024');
  FProviders.Image.Fallback:= TProviderConfig.Create('Stability','stable-diffusion-3',            '@STABILITY_API_KEY','');
  FProviders.Video.Primary := TProviderConfig.Create('Gemini',  'veo-2',                         '@GEMINI_API_KEY',  '');
  FProviders.Video.Fallback.Clear;
  FProviders.Embeddings.Primary  := TProviderConfig.Create('OpenAI', 'text-embedding-3-small',   '@OPENAI_API_KEY',  '');
  FProviders.Embeddings.Fallback := TProviderConfig.Create('Gemini', 'text-embedding-004',        '@GEMINI_API_KEY',  '');

  // Fase 3 — config (overrides defaults above)
  FConfigLock    := TCriticalSection.Create;
  FAllowedIds    := TDictionary<Int64, string>.Create;
  FConfigFile    := TPath.Combine(FDataDir, 'telegram_bot_config.json');
  FOwnerChatId   := 0;
  FWhitelistMode := False;
  FConfigLock.Enter;
  try LoadConfig;
  finally FConfigLock.Leave; end;

  // Fase 4 — memoria semántica (KV + RAG) — usa FProviders.Embeddings cargado
  FMemory := TBotMemory.Create(FDataDir,
    FProviders.Embeddings.Primary.ApiKey,
    FProviders.Embeddings.Primary.Driver,
    FProviders.Embeddings.Primary.Model);

  // Fase 5 — stats
  FTotalMessages  := 0;
  FTotalToolCalls := 0;
  FBotStartTime   := 0;

  // Fase 6 — voz
  FVoiceEnabled := True;
  FMCPToolsURL  := '';

  // PPM auto-install
  FInstalledClients := TObjectList<TObject>.Create(True);
  FInstalledPkgs    := TStringList.Create;
  FPPMStateFile     := TPath.Combine(FDataDir, 'ppm_installed.json');
  TAiToolRegistry.Instance.PPMBaseUrl := PPM_REGISTRY;
  TAiToolRegistry.Instance.ToolsDir   := TPath.Combine(FDataDir, 'tools');

  RegisterTools;
end;

procedure TDmTelegramBot.DataModuleDestroy(Sender: TObject);
var Chat: TAiChatConnection;
begin
  FPolling := False;
  if Assigned(FThread) then
  begin
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;

  FCSChats.Enter;
  try
    for Chat in FChats.Values do Chat.AiFunctions := nil;
  finally FCSChats.Leave; end;
  FChats.Free;
  FFunctions.Free;
  FCSChats.Free;

  FreeAndNil(FMemory);

  FAllowedIds.Free;
  FConfigLock.Free;

  // PPM: liberar clientes MCP instalados dinámicamente
  TAiToolRegistry.Instance.Clear;
  FreeAndNil(FInstalledClients);
  FreeAndNil(FInstalledPkgs);
end;

// =============================================================================
// Multi-usuario
// =============================================================================

function TDmTelegramBot.CreateNewChat: TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName                    := FProviders.LLM.Primary.Driver;
  Result.Model                         := FProviders.LLM.Primary.Model;
  Result.Params.Values['ApiKey']       := FProviders.LLM.Primary.ApiKey;
  if FProviders.LLM.Primary.Extra <> '' then
    Result.Params.Values['BaseUrl']    := FProviders.LLM.Primary.Extra;
  Result.Params.Values['Asynchronous'] := 'False';
  Result.Params.Values['Max_tokens']   := '2048';
  Result.Params.Values['Tool_Active']  := 'True';
  Result.SystemPrompt.Text             := SYS_PROMPT;
  Result.AiFunctions                   := FFunctions;
  // Log API errors que RunNew captura internamente (sin re-raise)
  Result.OnError := OnChatError;
end;

function TDmTelegramBot.GetOrCreateChat(AChatId: Int64): TAiChatConnection;
begin
  FCSChats.Enter;
  try
    if not FChats.TryGetValue(AChatId, Result) then
    begin
      Result := CreateNewChat;
      LoadHistory(AChatId, Result);
      FChats.Add(AChatId, Result);
      if Result.Messages.Count > 0 then
        DoLog('SYS', Format('Usuario reconectado: chat_id=%d (historial: %d msgs, sesiones: %d)',
                            [AChatId, Result.Messages.Count, FChats.Count]))
      else
        DoLog('SYS', Format('Nuevo usuario: chat_id=%d (total: %d)', [AChatId, FChats.Count]));
    end;
  finally FCSChats.Leave; end;
end;

function TDmTelegramBot.UserCount: Integer;
begin
  FCSChats.Enter;
  try Result := FChats.Count;
  finally FCSChats.Leave; end;
end;

// =============================================================================
// Tool registration
// =============================================================================

procedure TDmTelegramBot.RegisterTools;

  procedure AddTool(const AName, ADesc: string;
    const APropNames, APropDescs, ARequired: array of string);
  var Item: TFunctionActionItem; JWrapper, JFunc, JParams, JProps, JProp: TJSONObject;
      JReq: TJSONArray; I: Integer;
  begin
    JProps := TJSONObject.Create;
    for I := 0 to High(APropNames) do
    begin
      JProp := TJSONObject.Create;
      JProp.AddPair('type', 'string');
      JProp.AddPair('description', APropDescs[I]);
      JProps.AddPair(APropNames[I], JProp);
    end;
    JReq := TJSONArray.Create;
    for I := 0 to High(ARequired) do JReq.Add(ARequired[I]);
    JParams := TJSONObject.Create;
    JParams.AddPair('type', 'object');
    JParams.AddPair('properties', JProps);
    JParams.AddPair('required', JReq);
    JFunc := TJSONObject.Create;
    JFunc.AddPair('name', AName);
    JFunc.AddPair('description', ADesc);
    JFunc.AddPair('parameters', JParams);
    JWrapper := TJSONObject.Create;
    JWrapper.AddPair('type', 'function');
    JWrapper.AddPair('function', JFunc);
    Item := FFunctions.Functions.AddFunction(AName, True, HandleToolCall);
    try Item.SetJSon(JWrapper);
    finally JWrapper.Free; end;
  end;

begin
  AddTool('shell_exec',
    'Execute a Windows shell command (cmd.exe /c) and return stdout + stderr.',
    ['command'], ['The shell command to run'], ['command']);
  AddTool('file_read', 'Read and return the text content of a file.',
    ['path'], ['Absolute or relative file path'], ['path']);
  AddTool('file_write', 'Write text content to a file. Creates or overwrites.',
    ['path', 'content'], ['File path', 'Text content to write'], ['path', 'content']);
  AddTool('file_list', 'List files and subdirectories in a directory.',
    ['path'], ['Directory path (use "." for current)'], ['path']);
  AddTool('memory_save',
    'Persist a key/value pair in permanent memory with optional category.',
    ['key', 'value', 'category'],
    ['Unique identifier', 'Value to store', 'Category label (e.g. "user", "task", "fact")'],
    ['key', 'value']);
  AddTool('memory_get', 'Retrieve a previously saved value by exact key.',
    ['key'], ['The key to look up'], ['key']);
  AddTool('memory_delete', 'Delete a saved memory entry by key.',
    ['key'], ['The key to delete'], ['key']);
  AddTool('memory_list', 'List all saved memory entries with key, value, category and timestamp.',
    [], [], []);
  AddTool('memory_search',
    'Semantic search across all saved memories using hybrid BM25 + vector similarity. ' +
    'Use this to find relevant memories when you don''t know the exact key.',
    ['query', 'limit'],
    ['Natural language query', 'Max results (default 5)'],
    ['query']);
  AddTool('agent_run',
    'Run a multi-step AI pipeline. ' +
    'type="analyze": deep analysis + insights + summary. ' +
    'type="code_review": generate + review + refactor.',
    ['task', 'type'],
    ['The task or requirement', '"analyze" (default) or "code_review"'],
    ['task']);
  AddTool('ppm_install',
    'Search and auto-install an external capability from the PPM registry ' +
    '(118+ MCP tools: weather, GitHub, databases, email, translation, finance, ' +
    'Kubernetes, Kafka, S3, Telegram, Discord, Shopify, and more). ' +
    'Call this when you need a tool that is not currently available. ' +
    'After installation the new tools are immediately callable.',
    ['capability'],
    ['Keyword or package name describing the needed capability ' +
     '(e.g. "weather", "github", "postgres", "email", "translate", "coingecko")'],
    ['capability']);
  AddTool('tg_voice',
    'Synthesize text to speech and send it as a Telegram voice note to the current user. ' +
    'Use this when the user asks you to speak, send a voice message, or respond with audio.',
    ['text'],
    ['The text to convert to speech and send as voice note'],
    ['text']);
  AddTool('ppm_list',
    'List all PPM packages currently installed in this bot session (persisted across restarts).',
    [], [], []);
  AddTool('ppm_clear',
    'Remove all dynamically installed PPM packages from this session and clear the persisted list. ' +
    'Use when a package is broken or you want a clean slate.',
    [], [], []);
end;

procedure TDmTelegramBot.OnChatError(Sender: TObject; const ErrorMsg: string;
  E: Exception; const AResponse: IHTTPResponse);
begin
  DoLog('ERR', 'LLM error: ' + ErrorMsg);
end;

procedure TDmTelegramBot.HandleToolCall(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: String;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var Args: TJSONObject; JErr: TJSONObject;
begin
  Handled := False;
  TInterlocked.Increment(FTotalToolCalls);
  DoLog('TOOL', FunctionName + ' ' + Copy(ToolCall.Arguments, 1, 80));
  Args := nil;
  if ToolCall.Arguments <> '' then
    try Args := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
    except Args := nil; end;
  try
    if      FunctionName = 'shell_exec'    then ToolCall.Response := ToolShellExec(Args)
    else if FunctionName = 'file_read'     then ToolCall.Response := ToolFileRead(Args)
    else if FunctionName = 'file_write'    then ToolCall.Response := ToolFileWrite(Args)
    else if FunctionName = 'file_list'     then ToolCall.Response := ToolFileList(Args)
    else if FunctionName = 'memory_save'   then ToolCall.Response := ToolMemorySave(Args)
    else if FunctionName = 'memory_get'    then ToolCall.Response := ToolMemoryGet(Args)
    else if FunctionName = 'memory_delete' then ToolCall.Response := ToolMemoryDelete(Args)
    else if FunctionName = 'memory_list'   then ToolCall.Response := ToolMemoryList(Args)
    else if FunctionName = 'memory_search' then ToolCall.Response := ToolMemorySearch(Args)
    else if FunctionName = 'agent_run'     then ToolCall.Response := ToolAgentRun(Args)
    else if FunctionName = 'ppm_install'   then ToolCall.Response := ToolPPMInstall(Args)
    else if FunctionName = 'ppm_list'      then ToolCall.Response := ToolPPMList(Args)
    else if FunctionName = 'ppm_clear'     then ToolCall.Response := ToolPPMClear(Args)
    else if FunctionName = 'tg_voice'      then ToolCall.Response := ToolTgVoice(Args)
    else if FunctionName = 'telegram'      then ToolCall.Response := ToolNativeTelegram(Args)
    else
    begin
      // Fallback dinámico: buscar en TAiToolRegistry (tools auto-instalados vía PPM)
      var DynTool: IAiTool;
      if TAiToolRegistry.Instance.TryFind(FunctionName, DynTool) then
      begin
        DoLog('TOOL', '[PPM] ' + FunctionName + ' -> ejecutando via registry');
        var JResult: TJSONObject := nil;
        try
          JResult := DynTool.Execute(Args);
        except
          on E: Exception do
          begin
            DoLog('ERR', '[PPM] ' + FunctionName + ' excepcion: ' + E.Message);
            ToolCall.Response := Format('{"error":"%s"}',
              [StringReplace(E.Message, '"', '\"', [rfReplaceAll])]);
            Handled := True;
            Exit;
          end;
        end;
        if Assigned(JResult) then
        begin
          ToolCall.Response := JResult.ToJSON;
          JResult.Free;
        end
        else
        begin
          DoLog('ERR', '[PPM] ' + FunctionName + ' retorno nil (binario fallo o sin output)');
          ToolCall.Response := Format(
            '{"error":"Tool ''%s'' returned no output. The binary may have crashed, ' +
            'require missing dependencies, or need environment variables (e.g. GITHUB_TOKEN)."}',
            [FunctionName]);
        end;
      end
      else
      begin
        JErr := TJSONObject.Create;
        try
          JErr.AddPair('error', 'Unknown tool: ' + FunctionName +
            '. Tip: call ppm_install{"capability":"..."} to add new capabilities.');
          ToolCall.Response := JErr.ToJSON;
        finally JErr.Free; end;
      end;
    end;
    Handled := True;
  finally Args.Free; end;
end;

// =============================================================================
// Fase 1: Shell tools
// =============================================================================

function TDmTelegramBot.RunShell(const ACmd: string; var AOutput: string): Integer;
const
  MAX_IDLE_MS  = 12000; // abortar si no hay output nuevo en 12s (ej. psql esperando password)
  SHELL_TIMEOUT_MS = 30000; // timeout total del proceso
var SA: TSecurityAttributes; SI: TStartupInfo; PI: TProcessInformation;
    hR, hW, hNulIn: THandle; Buf: array[0..4095] of Byte; nRead, BytesAvail: DWORD;
    ExitCode: DWORD; SB: TStringBuilder; CmdLine: string;
    LEnc: TEncoding; LastActivity: Cardinal; TimedOut: Boolean;
begin
  Result := -1; AOutput := '';
  FillChar(SA, SizeOf(SA), 0);
  SA.nLength := SizeOf(SA); SA.bInheritHandle := True;

  // Pipe para capturar stdout+stderr
  if not CreatePipe(hR, hW, @SA, 0) then begin AOutput := 'CreatePipe failed'; Exit; end;
  SetHandleInformation(hR, HANDLE_FLAG_INHERIT, 0); // el extremo lector no se hereda

  // stdin → NUL device (no INVALID_HANDLE_VALUE que confunde a cmd.exe)
  hNulIn := CreateFile('nul', GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    @SA, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hNulIn = INVALID_HANDLE_VALUE then hNulIn := 0;

  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags   := STARTF_USESTDHANDLES; // sin STARTF_USESHOWWINDOW (ya usamos CREATE_NO_WINDOW)
  SI.hStdInput  := hNulIn;
  SI.hStdOutput := hW;
  SI.hStdError  := hW;

  FillChar(PI, SizeOf(PI), 0);
  CmdLine := 'cmd.exe /c chcp 65001 > nul && ' + ACmd; UniqueString(CmdLine);
  if not CreateProcess(nil, PChar(CmdLine), nil, nil, True, CREATE_NO_WINDOW, nil, nil, SI, PI) then
  begin
    CloseHandle(hR); CloseHandle(hW);
    if hNulIn <> 0 then CloseHandle(hNulIn);
    AOutput := 'CreateProcess failed: ' + SysErrorMessage(GetLastError);
    Exit;
  end;

  // Cerrar los extremos escritores en el proceso padre (crítico para que ReadFile termine)
  CloseHandle(hW);
  if hNulIn <> 0 then CloseHandle(hNulIn);
  CloseHandle(PI.hThread); // no necesitamos el handle del thread

  // Leer output con loop no-bloqueante (PeekNamedPipe + timeout de inactividad).
  // ReadFile bloqueante se quedaría colgado si el proceso espera input interactivo
  // (ej. psql pidiendo contraseña via consola en lugar de stdin).
  LEnc := TEncoding.UTF8;
  SB := TStringBuilder.Create;
  TimedOut := False;
  LastActivity := GetTickCount;
  try
    repeat
      BytesAvail := 0;
      if PeekNamedPipe(hR, nil, 0, nil, @BytesAvail, nil) then
      begin
        if BytesAvail > 0 then
        begin
          nRead := 0;
          if ReadFile(hR, Buf[0], SizeOf(Buf), nRead, nil) and (nRead > 0) then
          begin
            SB.Append(LEnc.GetString(TBytes(@Buf[0]), 0, nRead));
            LastActivity := GetTickCount;
          end;
        end
        else
        begin
          // Sin datos: verificar si el proceso ya terminó
          if WaitForSingleObject(PI.hProcess, 0) = WAIT_OBJECT_0 then
          begin
            // Proceso terminado; leer los bytes finales que queden
            nRead := 0;
            while PeekNamedPipe(hR, nil, 0, nil, @BytesAvail, nil) and (BytesAvail > 0) and
                  ReadFile(hR, Buf[0], SizeOf(Buf), nRead, nil) and (nRead > 0) do
            begin
              SB.Append(LEnc.GetString(TBytes(@Buf[0]), 0, nRead));
              nRead := 0;
            end;
            Break;
          end;
          // Timeout de inactividad: proceso colgado esperando input interactivo
          if (GetTickCount - LastActivity) > MAX_IDLE_MS then
          begin
            TerminateProcess(PI.hProcess, 1);
            SB.Append(#10 + '[Timeout: proceso sin output durante ' +
              IntToStr(MAX_IDLE_MS div 1000) + 's — posiblemente requiere input interactivo. ' +
              'Tip: usa variables de entorno (ej. set PGPASSWORD=xxx) en lugar de prompts.]');
            TimedOut := True;
            Break;
          end;
          Sleep(50);
        end;
      end
      else
        Break; // PeekNamedPipe falló: pipe cerrado
    until False;
    AOutput := Trim(SB.ToString);
    if Length(AOutput) > MAX_RESP then
      AOutput := Copy(AOutput, 1, MAX_RESP) + #10 + '[...truncado]';
  finally SB.Free; end;

  if not TimedOut then
    WaitForSingleObject(PI.hProcess, SHELL_TIMEOUT_MS);
  ExitCode := 0; GetExitCodeProcess(PI.hProcess, ExitCode); Result := Integer(ExitCode);
  CloseHandle(PI.hProcess);
  CloseHandle(hR);
end;

function TDmTelegramBot.ToolShellExec(const Args: TJSONObject): string;
var Cmd, Output: string; ExitCode: Integer; JR: TJSONObject;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('command', Cmd) then
  begin Result := '{"error":"Missing: command"}'; Exit; end;
  DoLog('SYS', '[shell] ' + Cmd);
  ExitCode := RunShell(Cmd, Output);
  JR := TJSONObject.Create;
  try JR.AddPair('output', Output); JR.AddPair('exit_code', TJSONNumber.Create(ExitCode)); Result := JR.ToJSON;
  finally JR.Free; end;
end;

function TDmTelegramBot.ToolFileRead(const Args: TJSONObject): string;
var Path, Content: string; JR: TJSONObject;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('path', Path) then
  begin Result := '{"error":"Missing: path"}'; Exit; end;
  JR := TJSONObject.Create;
  try
    try
      Content := TFile.ReadAllText(Path, TEncoding.UTF8);
      if Length(Content) > MAX_RESP then Content := Copy(Content, 1, MAX_RESP) + #10 + '[...truncado]';
      JR.AddPair('content', Content); JR.AddPair('chars', TJSONNumber.Create(Length(Content)));
    except on E: Exception do JR.AddPair('error', E.Message); end;
    Result := JR.ToJSON;
  finally JR.Free; end;
end;

function TDmTelegramBot.ToolFileWrite(const Args: TJSONObject): string;
var Path, Content: string; JR: TJSONObject;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('path', Path)
     or not Args.TryGetValue<string>('content', Content) then
  begin Result := '{"error":"Missing: path, content"}'; Exit; end;
  JR := TJSONObject.Create;
  try
    try
      TFile.WriteAllText(Path, Content, TEncoding.UTF8);
      JR.AddPair('ok', TJSONBool.Create(True)); JR.AddPair('path', Path);
      JR.AddPair('bytes', TJSONNumber.Create(TEncoding.UTF8.GetByteCount(Content)));
      DoLog('SYS', '[file_write] ' + Path);
    except on E: Exception do JR.AddPair('error', E.Message); end;
    Result := JR.ToJSON;
  finally JR.Free; end;
end;

function TDmTelegramBot.ToolFileList(const Args: TJSONObject): string;
var Path, F: string; JR: TJSONObject; JE: TJSONObject; JA: TJSONArray;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('path', Path) then Path := '.';
  JR := TJSONObject.Create;
  try
    try
      JR.AddPair('path', TPath.GetFullPath(Path));
      JA := TJSONArray.Create; JR.AddPair('entries', JA);
      for F in TDirectory.GetDirectories(Path) do
      begin JE := TJSONObject.Create; JE.AddPair('name', TPath.GetFileName(F)); JE.AddPair('type', 'dir'); JA.Add(JE); end;
      for F in TDirectory.GetFiles(Path) do
      begin JE := TJSONObject.Create; JE.AddPair('name', TPath.GetFileName(F)); JE.AddPair('type', 'file'); JA.Add(JE); end;
    except on E: Exception do JR.AddPair('error', E.Message); end;
    Result := JR.ToJSON;
  finally JR.Free; end;
end;

// =============================================================================
// Fase 2: Memory (TBotMemory — KV store + RAG semántico)
// =============================================================================

function TDmTelegramBot.ToolMemorySave(const Args: TJSONObject): string;
var Key, Value, Category: string;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('key', Key)
     or not Args.TryGetValue<string>('value', Value) then
  begin Result := '{"error":"Missing: key, value"}'; Exit; end;
  if not Args.TryGetValue<string>('category', Category) then
    Category := 'general';
  Result := FMemory.Save(Key, Value, Category);
end;

function TDmTelegramBot.ToolMemoryGet(const Args: TJSONObject): string;
var Key: string;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('key', Key) then
  begin Result := '{"error":"Missing: key"}'; Exit; end;
  Result := FMemory.Get(Key);
end;

function TDmTelegramBot.ToolMemoryDelete(const Args: TJSONObject): string;
var Key: string; Removed: Boolean; JR: TJSONObject;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('key', Key) then
  begin Result := '{"error":"Missing: key"}'; Exit; end;
  Removed := FMemory.Delete(Key);
  JR := TJSONObject.Create;
  try
    JR.AddPair('deleted', TJSONBool.Create(Removed));
    JR.AddPair('key', Key);
    Result := JR.ToJSON;
  finally JR.Free; end;
end;

function TDmTelegramBot.ToolMemoryList(const Args: TJSONObject): string;
var JA: TJSONArray; JR: TJSONObject;
begin
  JA := FMemory.List;
  try
    JR := TJSONObject.Create;
    try
      JR.AddPair('count', TJSONNumber.Create(JA.Count));
      JR.AddPair('memories', JA.Clone as TJSONArray);
      Result := JR.ToJSON;
    finally JR.Free; end;
  finally JA.Free; end;
end;

function TDmTelegramBot.ToolMemorySearch(const Args: TJSONObject): string;
var Query: string; Limit: Integer; JR: TJSONObject;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('query', Query) then
  begin Result := '{"error":"Missing: query"}'; Exit; end;
  if not Args.TryGetValue<Integer>('limit', Limit) or (Limit <= 0) then
    Limit := 5;
  JR := TJSONObject.Create;
  try
    JR.AddPair('results', FMemory.Search(Query, Limit));
    JR.AddPair('semantic_enabled', TJSONBool.Create(FMemory.RagEnabled));
    Result := JR.ToJSON;
  finally JR.Free; end;
end;

// =============================================================================
// Fase 2: Agent pipeline
// =============================================================================

function TDmTelegramBot.RunStep(const ASysPrompt, AInput: string): string;
var Chat: TAiChatConnection;
begin
  Chat := TAiChatConnection.Create(nil);
  try
    Chat.DriverName := 'Claude'; Chat.Model := 'claude-haiku-4-5-20251001';
    Chat.Params.Values['ApiKey']       := '@CLAUDE_API_KEY';
    Chat.Params.Values['Asynchronous'] := 'False';
    Chat.Params.Values['Max_tokens']   := '1024';
    Chat.SystemPrompt.Text := ASysPrompt;
    Result := Chat.AddMessageAndRun(AInput, 'user', []);
    if Result = '' then Result := '(sin respuesta)';
  finally Chat.Free; end;
end;

function TDmTelegramBot.PipelineAnalyze(const Task: string): string;
var Step1, Step2, Step3, Ctx: string;
begin
  SendTgMessage(FCurrentChatId, '[1/3] Analyzing...');
  Step1 := RunStep('You are a deep analysis expert. Analyze thoroughly. 3-4 paragraphs. Plain text.', Task);
  SendTgMessage(FCurrentChatId, '[2/3] Extracting insights...');
  Ctx   := 'TOPIC: ' + Task + #10#10 + 'ANALYSIS:' + #10 + Step1;
  Step2 := RunStep('Extract 5 key insights. Format: "Insight N: [title] - [sentence]".', Ctx);
  SendTgMessage(FCurrentChatId, '[3/3] Summarizing...');
  Ctx   := Ctx + #10#10 + 'INSIGHTS:' + #10 + Step2;
  Step3 := RunStep('Write executive summary (max 100 words). Start with "SUMMARY:".', Ctx);
  Result := '--- Key Insights ---' + #10 + Step2 + #10#10 + '--- Summary ---' + #10 + Step3;
end;

function TDmTelegramBot.PipelineCodeReview(const Task: string): string;
var Step1, Step2, Step3, Ctx: string;
begin
  SendTgMessage(FCurrentChatId, '[1/3] Generating code...');
  Step1 := RunStep('Expert engineer. Generate clean code. Prefer Python. Show code + inline comments.', Task);
  SendTgMessage(FCurrentChatId, '[2/3] Reviewing...');
  Ctx   := 'REQUIREMENT: ' + Task + #10#10 + 'CODE:' + #10 + Step1;
  Step2 := RunStep('Senior reviewer. Find BUGS, SECURITY, PERFORMANCE, STYLE issues. Label: "Issue N: [CAT] desc".', Ctx);
  SendTgMessage(FCurrentChatId, '[3/3] Refactoring...');
  Ctx   := Ctx + #10#10 + 'REVIEW:' + #10 + Step2;
  Step3 := RunStep('Produce improved version. Show ONLY improved code + brief "Changes:" section.', Ctx);
  Result := '--- Code Review ---' + #10 + Step2 + #10#10 + '--- Improved Code ---' + #10 + Step3;
end;

function TDmTelegramBot.ToolAgentRun(const Args: TJSONObject): string;
var Task, AType, PipelineResult: string; JR: TJSONObject;
begin
  if not Assigned(Args) or not Args.TryGetValue<string>('task', Task) then
  begin Result := '{"error":"Missing: task"}'; Exit; end;
  if not Args.TryGetValue<string>('type', AType) then AType := 'analyze';
  DoLog('SYS', '[agent] type=' + AType + ' task=' + Copy(Task, 1, 60));
  try
    if AType = 'code_review' then PipelineResult := PipelineCodeReview(Task)
    else PipelineResult := PipelineAnalyze(Task);
    if Length(PipelineResult) > MAX_RESP then
      PipelineResult := Copy(PipelineResult, 1, MAX_RESP) + #10 + '[...truncado]';
    JR := TJSONObject.Create;
    try JR.AddPair('result', PipelineResult); JR.AddPair('pipeline', AType); Result := JR.ToJSON;
    finally JR.Free; end;
  except on E: Exception do Result := '{"error":"' + E.Message + '"}'; end;
end;

// =============================================================================
// Fase 3: Permisos
// =============================================================================

procedure LoadSlot(AObj: TJSONObject; const AKey: string; var ASlot: TProviderConfig);
var JS: TJSONObject;
begin
  if Assigned(AObj) and AObj.TryGetValue<TJSONObject>(AKey, JS) then
  begin
    JS.TryGetValue<string>('driver',  ASlot.Driver);
    JS.TryGetValue<string>('model',   ASlot.Model);
    JS.TryGetValue<string>('api_key', ASlot.ApiKey);
    JS.TryGetValue<string>('extra',   ASlot.Extra);
  end;
end;

function SlotToJSON(const S: TProviderConfig): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('driver',  S.Driver);
  Result.AddPair('model',   S.Model);
  Result.AddPair('api_key', S.ApiKey);
  Result.AddPair('extra',   S.Extra);
end;

procedure TDmTelegramBot.LoadProviders(AObj: TJSONObject);
var JCat: TJSONObject;
begin
  if not Assigned(AObj) then Exit;
  if AObj.TryGetValue<TJSONObject>('prov_llm', JCat) then
  begin LoadSlot(JCat, 'p', FProviders.LLM.Primary); LoadSlot(JCat, 'f', FProviders.LLM.Fallback); end;
  if AObj.TryGetValue<TJSONObject>('prov_tts', JCat) then
  begin LoadSlot(JCat, 'p', FProviders.TTS.Primary); LoadSlot(JCat, 'f', FProviders.TTS.Fallback); end;
  if AObj.TryGetValue<TJSONObject>('prov_stt', JCat) then
  begin LoadSlot(JCat, 'p', FProviders.STT.Primary); LoadSlot(JCat, 'f', FProviders.STT.Fallback); end;
  if AObj.TryGetValue<TJSONObject>('prov_img', JCat) then
  begin LoadSlot(JCat, 'p', FProviders.Image.Primary); LoadSlot(JCat, 'f', FProviders.Image.Fallback); end;
  if AObj.TryGetValue<TJSONObject>('prov_vid', JCat) then
  begin LoadSlot(JCat, 'p', FProviders.Video.Primary); LoadSlot(JCat, 'f', FProviders.Video.Fallback); end;
  if AObj.TryGetValue<TJSONObject>('prov_emb', JCat) then
  begin LoadSlot(JCat, 'p', FProviders.Embeddings.Primary); LoadSlot(JCat, 'f', FProviders.Embeddings.Fallback); end;
end;

procedure TDmTelegramBot.SaveProvidersTo(AObj: TJSONObject);
  function CatJSON(const P, F: TProviderConfig): TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.AddPair('p', SlotToJSON(P));
    Result.AddPair('f', SlotToJSON(F));
  end;
begin
  AObj.AddPair('prov_llm', CatJSON(FProviders.LLM.Primary,        FProviders.LLM.Fallback));
  AObj.AddPair('prov_tts', CatJSON(FProviders.TTS.Primary,        FProviders.TTS.Fallback));
  AObj.AddPair('prov_stt', CatJSON(FProviders.STT.Primary,        FProviders.STT.Fallback));
  AObj.AddPair('prov_img', CatJSON(FProviders.Image.Primary,      FProviders.Image.Fallback));
  AObj.AddPair('prov_vid', CatJSON(FProviders.Video.Primary,      FProviders.Video.Fallback));
  AObj.AddPair('prov_emb', CatJSON(FProviders.Embeddings.Primary, FProviders.Embeddings.Fallback));
end;

procedure TDmTelegramBot.SaveProviders;
var NewMem: TBotMemory;
begin
  FConfigLock.Enter;
  try SaveConfig;
  finally FConfigLock.Leave; end;

  // Reinicializar memoria si cambi'#243' el provider de embeddings
  NewMem := TBotMemory.Create(FDataDir,
    FProviders.Embeddings.Primary.ApiKey,
    FProviders.Embeddings.Primary.Driver,
    FProviders.Embeddings.Primary.Model);
  FreeAndNil(FMemory);
  FMemory := NewMem;
end;

procedure TDmTelegramBot.LoadConfig;
var JVal: TJSONValue; JObj: TJSONObject; JArr: TJSONArray; JItem: TJSONValue; Id: Int64;
begin
  FOwnerChatId := 0; FWhitelistMode := False; FLastToken := ''; FAllowedIds.Clear;
  if not TFile.Exists(FConfigFile) then Exit;
  try
    JVal := TJSONObject.ParseJSONValue(TFile.ReadAllText(FConfigFile, TEncoding.UTF8));
    if not (JVal is TJSONObject) then begin JVal.Free; Exit; end;
    JObj := TJSONObject(JVal);
    try
      JObj.TryGetValue<Int64>('owner_id', FOwnerChatId);
      JObj.TryGetValue<Boolean>('whitelist_mode', FWhitelistMode);
      JObj.TryGetValue<string>('last_token', FLastToken);
      JObj.TryGetValue<string>('mcp_tools_url', FMCPToolsURL);
      LoadProviders(JObj);
      if JObj.TryGetValue<TJSONArray>('allowed_ids', JArr) then
        for JItem in JArr do
          if JItem is TJSONNumber then
          begin
            Id := Trunc(TJSONNumber(JItem).AsDouble);
            FAllowedIds.AddOrSetValue(Id, '');
          end;
    finally JObj.Free; end;
  except end;
end;

procedure TDmTelegramBot.SaveConfig;
var JObj: TJSONObject; JArr: TJSONArray; Id: Int64;
begin
  if not TDirectory.Exists(FDataDir) then TDirectory.CreateDirectory(FDataDir);
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('owner_id', TJSONNumber.Create(FOwnerChatId));
    JObj.AddPair('whitelist_mode', TJSONBool.Create(FWhitelistMode));
    JObj.AddPair('last_token', FLastToken);
    JObj.AddPair('mcp_tools_url', FMCPToolsURL);
    SaveProvidersTo(JObj);
    JArr := TJSONArray.Create;
    for Id in FAllowedIds.Keys do JArr.Add(Double(Id));
    JObj.AddPair('allowed_ids', JArr);
    TFile.WriteAllText(FConfigFile, JObj.ToJSON, TEncoding.UTF8);
  finally JObj.Free; end;
end;

function TDmTelegramBot.IsAuthorized(AChatId: Int64): Boolean;
begin
  if FOwnerChatId = 0    then Exit(True);
  if AChatId = FOwnerChatId then Exit(True);
  if not FWhitelistMode  then Exit(True);
  Result := FAllowedIds.ContainsKey(AChatId);
end;

function TDmTelegramBot.HandleCommand(AChatId: Int64; const AText, AUsername: string): Boolean;
var Cmd, Arg: string; SpacePos: Integer; Id: Int64; SB: TStringBuilder; Chat_: TAiChatConnection;
    VoiceStatus, VoiceEngine: string;
begin
  Result := False;
  if (AText = '') or (AText[1] <> '/') then Exit;
  SpacePos := Pos(' ', AText);
  if SpacePos > 0 then begin Cmd := LowerCase(Trim(Copy(AText, 1, SpacePos-1))); Arg := Trim(Copy(AText, SpacePos+1, MaxInt)); end
  else begin Cmd := LowerCase(Trim(AText)); Arg := ''; end;

  if Cmd = '/start' then
  begin
    Result := True;
    SendTgMessage(AChatId,
      'MakerAI Bot ready!' + #10 +
      'Skills: shell, files, memory, AI agents, voice' + #10 +
      'Commands: /help /clear /status /voice /voiceon /voiceoff');
  end
  else if Cmd = '/help' then
  begin
    Result := True;
    SendTgMessage(AChatId,
      'Commands:' + #10 +
      '/start      - greeting' + #10 +
      '/clear      - clear chat history' + #10 +
      '/status     - bot statistics' + #10 +
      '/voice      - voice mode status' + #10 +
      '/voiceon    - enable voice responses' + #10 +
      '/voiceoff   - disable voice responses' + #10 +
      '/help       - this message');
    if AChatId = FOwnerChatId then
      SendTgMessage(AChatId,
        'Admin commands:' + #10 +
        '/allow <id>              - add to whitelist' + #10 +
        '/deny <id>               - remove from whitelist' + #10 +
        '/users                   - list allowed users' + #10 +
        '/whitelist               - enable whitelist mode' + #10 +
        '/openmode                - allow all users' + #10 +
        '/setmcp <url>            - set MCPService URL (empty = direct)' + #10 +
        '/llm [Driver Model Key]  - show or change LLM provider');
  end
  else if Cmd = '/clear' then
  begin
    Result := True;
    FCSChats.Enter;
    try
      if FChats.TryGetValue(AChatId, Chat_) then Chat_.AiFunctions := nil;
      FChats.Remove(AChatId);
    finally FCSChats.Leave; end;
    if TFile.Exists(HistoryPath(AChatId)) then TFile.Delete(HistoryPath(AChatId));
    SendTgMessage(AChatId, 'Chat history cleared.');
    DoLog('SYS', Format('Historial limpiado: chat_id=%d', [AChatId]));
  end
  else if Cmd = '/status' then
  begin
    Result := True;
    if FMCPToolsURL <> '' then VoiceEngine := 'MCPService (' + FMCPToolsURL + ')'
    else VoiceEngine := 'Direct (OpenAI Whisper/TTS)';
    SendTgMessage(AChatId, Format(
      'Bot status:' + #10 +
      'Uptime:    %s' + #10 +
      'Sessions:  %d active' + #10 +
      'Messages:  %d processed' + #10 +
      'Tools:     %d calls' + #10 +
      'Mode:      %s' + #10 +
      'LLM:       %s / %s' + #10 +
      'Voice:     %s' + #10 +
      'Engine:    %s',
      [Uptime, UserCount, FTotalMessages, FTotalToolCalls,
       IfThen(FWhitelistMode, 'whitelist', 'open'),
       FProviders.LLM.Primary.Driver, FProviders.LLM.Primary.Model,
       IfThen(FVoiceEnabled, 'ON', 'OFF'),
       VoiceEngine]));
  end
  else if Cmd = '/voice' then
  begin
    Result := True;
    if FVoiceEnabled then VoiceStatus := 'ON (send a voice message to try)'
    else VoiceStatus := 'OFF (use /voiceon to enable)';
    if FMCPToolsURL <> '' then VoiceEngine := 'MCPService: ' + FMCPToolsURL
    else VoiceEngine := 'Direct: OpenAI Whisper + TTS';
    SendTgMessage(AChatId, 'Voice mode: ' + VoiceStatus + #10 + 'Engine: ' + VoiceEngine);
  end
  else if Cmd = '/voiceon' then
  begin
    Result := True;
    FVoiceEnabled := True;
    SendTgMessage(AChatId, 'Voice responses enabled. Send a voice message!');
    DoLog('SYS', 'Voice mode ON');
  end
  else if Cmd = '/voiceoff' then
  begin
    Result := True;
    FVoiceEnabled := False;
    SendTgMessage(AChatId, 'Voice responses disabled. Text only.');
    DoLog('SYS', 'Voice mode OFF');
  end
  // Admin commands
  else if AChatId = FOwnerChatId then
  begin
    if Cmd = '/allow' then
    begin
      Result := True;
      if TryStrToInt64(Arg, Id) then
      begin
        FConfigLock.Enter; try FAllowedIds.AddOrSetValue(Id, ''); SaveConfig; finally FConfigLock.Leave; end;
        SendTgMessage(AChatId, Format('User %d added.', [Id]));
      end else SendTgMessage(AChatId, 'Usage: /allow <chat_id>');
    end
    else if Cmd = '/deny' then
    begin
      Result := True;
      if TryStrToInt64(Arg, Id) and (Id <> FOwnerChatId) then
      begin
        FConfigLock.Enter; try FAllowedIds.Remove(Id); SaveConfig; finally FConfigLock.Leave; end;
        SendTgMessage(AChatId, Format('User %d removed.', [Id]));
      end else SendTgMessage(AChatId, 'Usage: /deny <chat_id>');
    end
    else if Cmd = '/users' then
    begin
      Result := True;
      SB := TStringBuilder.Create;
      try
        SB.AppendLine(Format('Active sessions: %d', [UserCount]));
        FConfigLock.Enter;
        try
          SB.AppendLine(Format('Allowed users: %d', [FAllowedIds.Count]));
          for var Pair in FAllowedIds do
            SB.AppendLine(Format('  %d%s', [Pair.Key, IfThen(Pair.Key = FOwnerChatId, ' (owner)', '')]));
        finally FConfigLock.Leave; end;
        SendTgMessage(AChatId, Trim(SB.ToString));
      finally SB.Free; end;
    end
    else if Cmd = '/whitelist' then
    begin
      Result := True;
      FConfigLock.Enter;
      try FWhitelistMode := True; FAllowedIds.AddOrSetValue(FOwnerChatId, AUsername); SaveConfig;
      finally FConfigLock.Leave; end;
      SendTgMessage(AChatId, 'Whitelist mode ON.');
    end
    else if Cmd = '/openmode' then
    begin
      Result := True;
      FConfigLock.Enter; try FWhitelistMode := False; SaveConfig; finally FConfigLock.Leave; end;
      SendTgMessage(AChatId, 'Open mode ON.');
    end
    else if Cmd = '/setmcp' then
    begin
      Result := True;
      FMCPToolsURL := Arg;
      FConfigLock.Enter; try SaveConfig; finally FConfigLock.Leave; end;
      if Arg = '' then SendTgMessage(AChatId, 'MCPService cleared. Using direct mode.')
      else SendTgMessage(AChatId, 'MCPService set to: ' + Arg);
      CheckVoiceCapabilities;
    end
    else if Cmd = '/llm' then
    begin
      Result := True;
      // Uso: /llm   (sin arg = mostrar config actual)
      //      /llm Claude claude-opus-4-6 @CLAUDE_API_KEY
      //      /llm OpenAI gpt-4o @OPENAI_API_KEY
      //      /llm Gemini gemini-3-flash-preview @GEMINI_API_KEY
      //      /llm Ollama llama3.2 http://localhost:11434
      if Arg = '' then
      begin
        SendTgMessage(AChatId,
          'LLM actual:' + #10 +
          'Driver: ' + FProviders.LLM.Primary.Driver + #10 +
          'Model:  ' + FProviders.LLM.Primary.Model  + #10 +
          'Key:    ' + FProviders.LLM.Primary.ApiKey + #10 +
          'Fallback: ' + FProviders.LLM.Fallback.Driver + ' / ' + FProviders.LLM.Fallback.Model + #10#10 +
          'Para cambiar: /llm <Driver> <Model> <ApiKey>' + #10 +
          'Drivers: Claude, OpenAI, Gemini, Ollama');
      end
      else
      begin
        var Parts := Arg.Split([' '], 3);
        if Length(Parts) >= 2 then
        begin
          FProviders.LLM.Primary.Driver := Parts[0];
          FProviders.LLM.Primary.Model  := Parts[1];
          if Length(Parts) >= 3 then FProviders.LLM.Primary.ApiKey := Parts[2];
          SaveProviders;
          FCSChats.Enter;
          try
            for var C in FChats.Values do C.AiFunctions := nil;
            FChats.Clear;
          finally FCSChats.Leave; end;
          SendTgMessage(AChatId, Format(
            'LLM actualizado:' + #10 +
            'Driver: %s' + #10 +
            'Model:  %s' + #10 +
            'Key:    %s' + #10 +
            'Sesiones reiniciadas.',
            [FProviders.LLM.Primary.Driver, FProviders.LLM.Primary.Model, FProviders.LLM.Primary.ApiKey]));
          DoLog('SYS', Format('[LLM] -> %s / %s', [FProviders.LLM.Primary.Driver, FProviders.LLM.Primary.Model]));
        end
        else
          SendTgMessage(AChatId, 'Uso: /llm <Driver> <Model> <ApiKey>');
      end;
    end;
  end;
end;

// =============================================================================
// Fase 3: Historial
// =============================================================================

function TDmTelegramBot.HistoryPath(AChatId: Int64): string;
begin Result := TPath.Combine(FDataDir, Format('history_%d.json', [AChatId])); end;

procedure TDmTelegramBot.SaveHistory(AChatId: Int64; AChat: TAiChatConnection);
var JArr: TJSONArray; JMsg: TJSONObject; Msg: TAiChatMessage; I, StartIdx: Integer;
begin
  JArr := TJSONArray.Create;
  try
    StartIdx := 0;
    if AChat.Messages.Count > MAX_HISTORY_MSG then StartIdx := AChat.Messages.Count - MAX_HISTORY_MSG;
    for I := StartIdx to AChat.Messages.Count - 1 do
    begin
      Msg := AChat.Messages[I];
      if (Msg.Role <> 'user') and (Msg.Role <> 'assistant') then Continue;
      if Msg.Prompt = '' then Continue;
      JMsg := TJSONObject.Create; JMsg.AddPair('role', Msg.Role); JMsg.AddPair('content', Msg.Prompt); JArr.Add(JMsg);
    end;
    if not TDirectory.Exists(FDataDir) then TDirectory.CreateDirectory(FDataDir);
    TFile.WriteAllText(HistoryPath(AChatId), JArr.ToJSON, TEncoding.UTF8);
  finally JArr.Free; end;
end;

procedure TDmTelegramBot.LoadHistory(AChatId: Int64; AChat: TAiChatConnection);
var JVal: TJSONValue; JArr: TJSONArray; JMsg: TJSONObject; Msg: TAiChatMessage;
    Role, Content: string;
begin
  if not TFile.Exists(HistoryPath(AChatId)) then Exit;
  try
    JVal := TJSONObject.ParseJSONValue(TFile.ReadAllText(HistoryPath(AChatId), TEncoding.UTF8));
    if not (JVal is TJSONArray) then begin JVal.Free; Exit; end;
    JArr := TJSONArray(JVal);
    try
      for JVal in JArr do
      begin
        if not (JVal is TJSONObject) then Continue;
        JMsg := TJSONObject(JVal);
        if not JMsg.TryGetValue<string>('role', Role) then Continue;
        if not JMsg.TryGetValue<string>('content', Content) then Continue;
        Msg := AChat.NewMessage(Content, Role); Msg.Id := AChat.Messages.Count + 1; AChat.Messages.Add(Msg);
      end;
    finally JArr.Free; end;
  except end;
end;

// =============================================================================
// Fase 3: Stats
// =============================================================================

function TDmTelegramBot.Uptime: string;
var Secs: Int64;
begin
  if FBotStartTime = 0 then begin Result := '-'; Exit; end;
  Secs   := SecondsBetween(Now, FBotStartTime);
  Result := Format('%dh %dm %ds', [Secs div 3600, (Secs mod 3600) div 60, Secs mod 60]);
end;

// =============================================================================
// Fase 4: MCP / PPM
// =============================================================================

// Llama a una herramienta en MCPService via JSON-RPC HTTP.
// Devuelve el texto del primer content item, o '' si falla.
function TDmTelegramBot.CallMCPTool(const AToolName: string; const AArgs: TJSONObject): string;
var
  Http     : THTTPClient;
  JBody    : TJSONObject;
  JParams  : TJSONObject;
  Body     : TStringStream;
  Resp     : IHTTPResponse;
  JRoot    : TJSONValue;
  JResult  : TJSONObject;
  JContent : TJSONArray;
  JItem    : TJSONValue;
  ItemText : string;
  JInner   : TJSONValue;
begin
  Result := '';
  if FMCPToolsURL = '' then Exit;
  Http := THTTPClient.Create;
  try
    JBody := TJSONObject.Create;
    try
      JBody.AddPair('jsonrpc', '2.0');
      JBody.AddPair('method', 'tools/call');
      JBody.AddPair('id', TJSONNumber.Create(1));
      JParams := TJSONObject.Create;
      JParams.AddPair('name', AToolName);
      JParams.AddPair('arguments', AArgs.Clone as TJSONValue);
      JBody.AddPair('params', JParams);
      Body := TStringStream.Create(JBody.ToJSON, TEncoding.UTF8);
      try
        try
          Resp := Http.Post(FMCPToolsURL + '/mcp', Body, nil,
            [TNameValuePair.Create('Content-Type', 'application/json')]);
          if (Resp <> nil) and (Resp.StatusCode = 200) then
          begin
            JRoot := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8));
            if Assigned(JRoot) then
            try
              // result.content[0].text contiene un JSON string con el resultado real
              if JRoot.TryGetValue<TJSONObject>('result', JResult) then
                if JResult.TryGetValue<TJSONArray>('content', JContent) then
                  if JContent.Count > 0 then
                  begin
                    JItem := JContent.Items[0];
                    if (JItem is TJSONObject) and
                       TJSONObject(JItem).TryGetValue<string>('text', ItemText) then
                    begin
                      // El text es en si mismo un JSON string con el resultado
                      JInner := TJSONObject.ParseJSONValue(ItemText);
                      if Assigned(JInner) then
                      begin Result := ItemText; JInner.Free; end  // devolver el JSON raw
                      else Result := ItemText;
                    end;
                  end;
            finally JRoot.Free; end;
          end;
        except end;
      finally Body.Free; end;
    finally JBody.Free; end;
  finally Http.Free; end;
end;

// Verifica e instala automáticamente capacidades de voz (mcp-transcribe, mcp-tts).
procedure TDmTelegramBot.CheckVoiceCapabilities;
const
  VOICE_CAPS: array[0..1] of string = ('mcp-stt-openai', 'mcp-tts-openai');
var
  PkgName, ToolsList: string;
  Http: THTTPClient;
  Resp: IHTTPResponse;
begin
  // Restaurar paquetes PPM instalados en sesiones anteriores
  LoadAndRestorePPMState;

  // Auto-instalar herramientas de voz vía PPM
  for var Cap in VOICE_CAPS do
  begin
    if InstallMCPPackage(Cap, PkgName, ToolsList) then
      DoLog('SYS', Format('[PPM] %s listo - tools: %s', [PkgName, ToolsList]))
    else
      DoLog('SYS', Format('[PPM] %s no disponible - usando modo directo', [Cap]));
  end;

  // Verificar MCPService legacy si esta configurado
  if FMCPToolsURL <> '' then
  begin
    Http := THTTPClient.Create;
    try
      Http.ResponseTimeout := 3000;
      try
        Resp := Http.Get(FMCPToolsURL + '/health');
        if (Resp <> nil) and (Resp.StatusCode = 200) then
          DoLog('SYS', '[MCP] MCPService activo: ' + FMCPToolsURL)
        else
        begin
          DoLog('SYS', '[MCP] MCPService no responde - cambiando a modo directo');
          FMCPToolsURL := '';
        end;
      except
        DoLog('SYS', '[MCP] MCPService no disponible - usando modo directo');
        FMCPToolsURL := '';
      end;
    finally Http.Free; end;
  end;
end;

// =============================================================================
// Fase 4: Voz
// =============================================================================

// Descarga un archivo de Telegram por file_id, devuelve ruta local (OGG).
function TDmTelegramBot.DownloadTgFile(const AFileId: string): string;
var
  Http       : THTTPClient;
  Resp       : IHTTPResponse;
  JVal       : TJSONValue;
  JObj       : TJSONObject;
  JFile      : TJSONObject;
  FilePath   : string;
  DownURL    : string;
  OutPath    : string;
  FileStream : TFileStream;
begin
  Result := '';
  Http := THTTPClient.Create;
  try
    // Paso 1: obtener file_path de Telegram
    Resp := Http.Get(TG_API + FToken + '/getFile?file_id=' + AFileId);
    if (Resp = nil) or (Resp.StatusCode <> 200) then Exit;
    JVal := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8));
    if not Assigned(JVal) then Exit;
    FilePath := '';
    try
      if (JVal is TJSONObject) then
      begin
        JObj := TJSONObject(JVal);
        if JObj.TryGetValue<TJSONObject>('result', JFile) then
          JFile.TryGetValue<string>('file_path', FilePath);
      end;
    finally JVal.Free; end;
    if FilePath = '' then Exit;

    // Paso 2: descargar
    DownURL := TG_FILE_API + FToken + '/' + FilePath;
    OutPath := TPath.Combine(GetEnvironmentVariable('TEMP'),
      Format('tg_voice_%d_%d.ogg', [FCurrentChatId, GetTickCount64 mod 999983]));
    try
      FileStream := TFileStream.Create(OutPath, fmCreate);
      try Resp := Http.Get(DownURL, FileStream);
      finally FileStream.Free; end;
      if (Resp <> nil) and (Resp.StatusCode = 200) then
        Result := OutPath
      else
      begin if TFile.Exists(OutPath) then TFile.Delete(OutPath); end;
    except
      if TFile.Exists(OutPath) then TFile.Delete(OutPath);
    end;
  finally Http.Free; end;
end;

// Transcribe audio OGG a texto. Intenta MCPService primero, luego TAiOpenAiAudio.
function TDmTelegramBot.TranscribeAudio(const AFilePath: string): string;
var
  JArgs   : TJSONObject;
  RawResp : string;
  JInner  : TJSONValue;
  Audio   : TAiOpenAiAudio;
  MF      : TAiMediaFile;
  Trans   : TTranscriptionResult;
begin
  Result := '';

  // Opcion A: TAiToolRegistry (mcp-transcribe auto-instalado vía PPM)
  // Buscar por nombre de tool (no por SourceId: mcp-audio registra sus tools con SourceId='mcp-audio')
  var McpTranscribe: IAiTool := nil;
  TAiToolRegistry.Instance.TryFind('stt', McpTranscribe);
  if Assigned(McpTranscribe) then
  begin
    JArgs := TJSONObject.Create;
    try
      JArgs.AddPair('filepath', AFilePath);
      JArgs.AddPair('language', 'es');
      var JRes := McpTranscribe.Execute(JArgs);
      if Assigned(JRes) then
      try
        // MCP response: { "content": [{"type":"text","text":"{\"text\":\"...\",\"language\":\"...\"}"}] }
        var InnerJson := ExtractMCPText(JRes);
        if InnerJson <> '' then
        begin
          var JInnerObj := TJSONObject.ParseJSONValue(InnerJson);
          if Assigned(JInnerObj) then
          try JInnerObj.TryGetValue<string>('text', Result);
          finally JInnerObj.Free; end
          else
            Result := InnerJson;
        end;
      finally JRes.Free; end;
    finally JArgs.Free; end;
    if Result <> '' then begin DoLog('SYS', '[STT] via PPM/' + McpTranscribe.GetName); Exit; end;
  end;

  // Opcion B: MCPService legacy
  if FMCPToolsURL <> '' then
  begin
    JArgs := TJSONObject.Create;
    try
      JArgs.AddPair('filepath', AFilePath);
      JArgs.AddPair('language', 'es');
      RawResp := CallMCPTool('mcp-transcribe', JArgs);
    finally JArgs.Free; end;
    if RawResp <> '' then
    begin
      JInner := TJSONObject.ParseJSONValue(RawResp);
      if Assigned(JInner) then
      try JInner.TryGetValue<string>('text', Result);
      finally JInner.Free; end;
      if Result <> '' then begin DoLog('SYS', '[STT] via MCPService'); Exit; end;
    end;
  end;

  // Opcion B: TAiOpenAiAudio directo (Whisper)
  Audio := TAiOpenAiAudio.Create(nil);
  try
    Audio.ApiKey             := '@OPENAI_API_KEY';
    Audio.TranscriptionModel := tmWhisper1;
    MF := TAiMediaFile.Create;
    try
      MF.LoadFromFile(AFilePath);
      Trans := Audio.Transcribe(MF, '');
      try
        Result := Trans.Text;
        DoLog('SYS', '[STT] via Whisper | lang=' + Trans.Language);
      finally Trans.Free; end;
    finally MF.Free; end;
  except on E: Exception do DoLog('SYS', '[STT] error: ' + E.Message); end;
  Audio.Free;
end;

// Sintetiza texto a MP3. Intenta MCPService primero, luego TAiOpenAiAudio.
// Devuelve ruta al MP3 temporal o '' si falla.
function TDmTelegramBot.SynthesizeVoice(const AText: string): string;
var
  OutPath : string;
  JArgs   : TJSONObject;
  RawResp : string;
  JInner  : TJSONValue;
  Ok      : Boolean;
  Audio   : TAiOpenAiAudio;
  Stream  : TMemoryStream;
  TxtIn   : string;
begin
  Result  := '';
  TxtIn   := Copy(AText, 1, 4000);
  OutPath := TPath.Combine(GetEnvironmentVariable('TEMP'),
    Format('tts_%d_%d.mp3', [FCurrentChatId, GetTickCount64 mod 999983]));

  // Opcion A: TAiToolRegistry (mcp-tts auto-instalado vía PPM)
  // Buscar por nombre de tool (no por SourceId: mcp-audio registra sus tools con SourceId='mcp-audio')
  var McpTts: IAiTool := nil;
  TAiToolRegistry.Instance.TryFind('tts', McpTts);
  if Assigned(McpTts) then
  begin
    JArgs := TJSONObject.Create;
    try
      JArgs.AddPair('text', TxtIn);
      JArgs.AddPair('voice', 'shimmer');
      JArgs.AddPair('output_path', OutPath);
      var JRes := McpTts.Execute(JArgs);
      if Assigned(JRes) then
      try
        Ok := False;
        // MCP response: { "content": [{"type":"text","text":"{\"ok\":true,\"output_path\":\"...\"}"}] }
        var InnerJson := ExtractMCPText(JRes);
        if InnerJson <> '' then
        begin
          var JInnerObj := TJSONObject.ParseJSONValue(InnerJson);
          if Assigned(JInnerObj) then
          try JInnerObj.TryGetValue<Boolean>('ok', Ok);
          finally JInnerObj.Free; end;
        end;
        if Ok and TFile.Exists(OutPath) then
        begin Result := OutPath; DoLog('SYS', '[TTS] via PPM/' + McpTts.GetName); end;
      finally JRes.Free; end;
    finally JArgs.Free; end;
    if Result <> '' then Exit;
  end;

  // Opcion B: MCPService legacy
  if FMCPToolsURL <> '' then
  begin
    JArgs := TJSONObject.Create;
    try
      JArgs.AddPair('text', TxtIn);
      JArgs.AddPair('voice', 'shimmer');
      JArgs.AddPair('output_path', OutPath);
      RawResp := CallMCPTool('mcp-tts', JArgs);
    finally JArgs.Free; end;
    if RawResp <> '' then
    begin
      Ok := False;
      JInner := TJSONObject.ParseJSONValue(RawResp);
      if Assigned(JInner) then
      try JInner.TryGetValue<Boolean>('ok', Ok);
      finally JInner.Free; end;
      if Ok and TFile.Exists(OutPath) then
      begin Result := OutPath; DoLog('SYS', '[TTS] via MCPService'); Exit; end;
    end;
  end;

  // Opcion C: proveedor primario configurado
  var TtsSlots: array[0..1] of TProviderConfig;
  TtsSlots[0] := FProviders.TTS.Primary;
  TtsSlots[1] := FProviders.TTS.Fallback;
  for var SI := 0 to 1 do
  begin
    var Slot := TtsSlots[SI];
    if Slot.IsEmpty then Continue;
    try
      if SameText(Slot.Driver, 'Gemini') then
      begin
        var WavPath := TPath.Combine(GetEnvironmentVariable('TEMP'),
          Format('tts_%d_%d.wav', [FCurrentChatId, GetTickCount64 mod 999983]));
        var MF := TAiGeminiSpeechTool.GenerateSpeech(Slot.ApiKey, TxtIn, Slot.Extra, nil, nil, nil);
        if Assigned(MF) then
        try
          MF.SaveToFile(WavPath);
          if TFile.Exists(WavPath) then
          begin Result := WavPath; DoLog('SYS', '[TTS] via Gemini/' + Slot.Model); end;
        finally MF.Free; end;
      end
      else if SameText(Slot.Driver, 'OpenAI') then
      begin
        Audio := TAiOpenAiAudio.Create(nil);
        try
          Audio.ApiKey   := Slot.ApiKey;
          Audio.TTSModel := tts_1;
          Audio.TTSVoice := tvShimmer;
          Stream := Audio.Speech(TxtIn);
          if Assigned(Stream) then
          try
            Stream.SaveToFile(OutPath);
            Result := OutPath;
            DoLog('SYS', '[TTS] via OpenAI/' + Slot.Model);
          finally Stream.Free; end;
        finally Audio.Free; end;
      end;
    except on E: Exception do DoLog('SYS', Format('[TTS] %s error: %s', [Slot.Driver, E.Message])); end;
    if Result <> '' then Exit;
  end;
end;

// Envia un archivo MP3/OGG como mensaje de voz a Telegram.
procedure TDmTelegramBot.SendTgVoice(const AChatId: Int64; const AFilePath: string);
var Http: THTTPClient; MP: TMultipartFormData; URL: string;
begin
  if not TFile.Exists(AFilePath) then Exit;
  URL  := TG_API + FToken + '/sendVoice';
  Http := THTTPClient.Create;
  MP   := TMultipartFormData.Create;
  try
    MP.AddField('chat_id', IntToStr(AChatId));
    MP.AddFile('voice', AFilePath);
    try Http.Post(URL, MP);
    except on E: Exception do DoLog('SYS', 'sendVoice error: ' + E.Message); end;
  finally MP.Free; Http.Free; end;
end;

function TDmTelegramBot.ToolTgVoice(const Args: TJSONObject): string;
var
  Txt, FilePath: string;
  JRes: TJSONObject;
begin
  Txt := '';
  if Assigned(Args) then Args.TryGetValue<string>('text', Txt);
  if Txt = '' then begin Result := '{"error":"text param required"}'; Exit; end;

  FilePath := SynthesizeVoice(Txt);
  JRes := TJSONObject.Create;
  try
    if FilePath <> '' then
    begin
      SendTgVoice(FCurrentChatId, FilePath);
      TFile.Delete(FilePath);
      JRes.AddPair('ok', TJSONBool.Create(True));
      JRes.AddPair('sent', 'voice note sent to Telegram');
    end
    else
    begin
      JRes.AddPair('ok', TJSONBool.Create(False));
      JRes.AddPair('error', 'TTS synthesis failed');
    end;
    Result := JRes.ToJSON;
  finally JRes.Free; end;
end;

// =============================================================================
// Logging, Telegram, Polling
// =============================================================================

procedure TDmTelegramBot.DoLog(const ADir, AText: string);
var Dir, Txt: string;
begin
  Dir := ADir; Txt := AText;
  TThread.Queue(nil, procedure begin if Assigned(FOnLog) then FOnLog(Dir, Txt); end);
end;

function TDmTelegramBot.ResolveToken(const AToken: string): string;
var VarName: string;
begin
  if (Length(AToken) > 1) and (AToken[1] = '@') then
  begin
    VarName := Copy(AToken, 2, MaxInt);
    Result  := GetEnvironmentVariable(VarName);
    if Result = '' then
      DoLog('SYS', 'AVISO: variable "' + VarName + '" no encontrada - ingresa el token literal');
  end
  else
    Result := AToken;
end;

procedure TDmTelegramBot.SendTgMessage(const AChatId: Int64; const AText: string);
var Http: THTTPClient; URL: string; JBody: TJSONObject; Body: TStringStream; Txt: string;
begin
  Txt := AText;
  if Length(Txt) > 4000 then Txt := Copy(Txt, 1, 4000) + #10 + '[...truncado]';
  Http := THTTPClient.Create; JBody := TJSONObject.Create;
  try
    URL := TG_API + FToken + '/sendMessage';
    JBody.AddPair('chat_id', TJSONNumber.Create(AChatId));
    JBody.AddPair('text', Txt);
    Body := TStringStream.Create(JBody.ToJSON, TEncoding.UTF8);
    try Http.Post(URL, Body, nil, [TNameValuePair.Create('Content-Type', 'application/json')]);
    finally Body.Free; end;
  except on E: Exception do DoLog('SYS', 'sendMessage error: ' + E.Message); end;
  JBody.Free; Http.Free;
end;

procedure TDmTelegramBot.ProcessUpdate(const AUpdate: TJSONObject);
var
  JMsg, JChat, JFrom, JVoice : TJSONObject;
  ChatId                     : Int64;
  Text, Username             : string;
  Response                   : string;
  Chat                       : TAiChatConnection;
  IsVoice                    : Boolean;
  VoiceFileId                : string;
  TempAudio                  : string;
  Mp3Path                    : string;
begin
  if not AUpdate.TryGetValue<TJSONObject>('message', JMsg) then Exit;
  if not JMsg.TryGetValue<TJSONObject>('chat', JChat) then Exit;

  ChatId   := JChat.GetValue<Int64>('id');
  Username := '';
  if JMsg.TryGetValue<TJSONObject>('from', JFrom) then
    JFrom.TryGetValue<string>('username', Username);

  // Autorizar
  if not IsAuthorized(ChatId) then
  begin
    SendTgMessage(ChatId, 'Sorry, this bot is restricted.');
    DoLog('SYS', Format('Acceso denegado: chat_id=%d @%s', [ChatId, Username]));
    Exit;
  end;

  // Registrar owner (primer usuario)
  if FOwnerChatId = 0 then
  begin
    FOwnerChatId := ChatId;
    FConfigLock.Enter;
    try FAllowedIds.AddOrSetValue(ChatId, Username); SaveConfig;
    finally FConfigLock.Leave; end;
    DoLog('SYS', Format('Owner establecido: chat_id=%d @%s', [ChatId, Username]));
  end;

  FCurrentChatId := ChatId;

  // Detectar mensaje de voz
  IsVoice     := False;
  VoiceFileId := '';
  Text        := '';

  if JMsg.TryGetValue<TJSONObject>('voice', JVoice) then
  begin
    IsVoice     := True;
    VoiceFileId := JVoice.GetValue<string>('file_id');
    DoLog('IN', '[voz recibida] descargando...');

    TempAudio := DownloadTgFile(VoiceFileId);
    if TempAudio = '' then
    begin SendTgMessage(ChatId, 'No pude descargar el audio.'); Exit; end;

    try
      Text := TranscribeAudio(TempAudio);
    finally
      TFile.Delete(TempAudio);
    end;

    if Text = '' then
    begin SendTgMessage(ChatId, 'No pude transcribir el audio.'); Exit; end;

    // Confirmar lo que se entendio
    SendTgMessage(ChatId, '[Escuche]: ' + Text);
    DoLog('IN', '[voz] ' + Text);
  end
  else if not JMsg.TryGetValue<string>('text', Text) then
    Exit;

  if Text = '' then Exit;
  if not IsVoice then DoLog('IN', Text);

  // Comandos
  if HandleCommand(ChatId, Text, Username) then Exit;

  // LLM
  TInterlocked.Increment(FTotalMessages);
  Chat := GetOrCreateChat(ChatId);
  try
    Response := Chat.AddMessageAndRun(Text, 'user', []);
  except on E: Exception do Response := 'Error: ' + E.Message; end;

  if Response = '' then Response := '(Sin respuesta del modelo)';

  // Responder: voz si el input fue voz y voice mode activo, sino texto
  if IsVoice and FVoiceEnabled then
  begin
    Mp3Path := SynthesizeVoice(Response);
    if Mp3Path <> '' then
    begin
      SendTgVoice(ChatId, Mp3Path);
      TFile.Delete(Mp3Path);
      DoLog('OUT', '[voz] ' + Copy(Response, 1, 100));
    end
    else
    begin
      SendTgMessage(ChatId, Response);
      DoLog('OUT', Response);
    end;
  end
  else
  begin
    SendTgMessage(ChatId, Response);
    DoLog('OUT', Response);
  end;

  SaveHistory(ChatId, Chat);
end;

procedure TDmTelegramBot.PollLoop;
var Http: THTTPClient; Resp: IHTTPResponse; URL: string;
    JRoot: TJSONValue; JObj: TJSONObject; JResult: TJSONArray;
    JUpd: TJSONValue; UpdId: Int64;
begin
  Http := THTTPClient.Create;
  try
    Http.ResponseTimeout := HTTP_TO;
    while FPolling do
    begin
      URL := Format('%s%s/getUpdates?offset=%d&timeout=%d', [TG_API, FToken, FOffset, POLL_SECS]);
      try
        Resp := Http.Get(URL);
        if Resp = nil then Continue;
        if Resp.StatusCode = 401 then begin DoLog('SYS', 'Error 401: token invalido.'); FPolling := False; Break; end;
        if Resp.StatusCode <> 200 then begin DoLog('SYS', Format('Telegram HTTP %d', [Resp.StatusCode])); if FPolling then Sleep(3000); Continue; end;
        JRoot := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8));
        if Assigned(JRoot) then
        try
          JObj := JRoot as TJSONObject;
          if JObj.TryGetValue<TJSONArray>('result', JResult) then
            for JUpd in JResult do
            begin
              UpdId := (JUpd as TJSONObject).GetValue<Int64>('update_id');
              if UpdId >= FOffset then FOffset := UpdId + 1;
              ProcessUpdate(JUpd as TJSONObject);
            end;
        finally JRoot.Free; end;
      except
        on E: Exception do
          if FPolling then begin DoLog('SYS', 'Red: ' + E.Message); Sleep(3000); end;
      end;
    end;
  finally Http.Free; end;
end;

procedure TDmTelegramBot.Start(const AToken: string);
begin
  FToken          := ResolveToken(AToken);
  FOffset         := 0;
  FPolling        := True;
  FTotalMessages  := 0;
  FTotalToolCalls := 0;
  FBotStartTime   := Now;

  // Guardar token
  if FToken <> '' then
  begin
    FLastToken := AToken;
    FConfigLock.Enter; try SaveConfig; finally FConfigLock.Leave; end;
  end;

  FThread := TThread.CreateAnonymousThread(procedure
  begin
    CheckVoiceCapabilities;  // consulta PPM registry al arrancar
    PollLoop;
  end);
  FThread.FreeOnTerminate := False;
  FThread.Start;
  DoLog('SYS', Format('Bot iniciado - %d skills | modo: %s | owner: %d | voz: %s',
        [FFunctions.Functions.Count,
         IfThen(FWhitelistMode, 'whitelist', 'open'),
         FOwnerChatId,
         IfThen(FVoiceEnabled, 'ON', 'OFF')]));
end;

procedure TDmTelegramBot.Stop;
begin
  if FPolling then begin FPolling := False; DoLog('SYS', 'Bot detenido.'); end;
end;

// =============================================================================
// PPM auto-install helpers
// =============================================================================

procedure TDmTelegramBot.RegisterIAiToolInFunctions(const ATool: IAiTool);
var
  Item    : TFunctionActionItem;
  JWrapper: TJSONObject;
  JFunc   : TJSONObject;
  JSchema : TJSONObject;
  JEmpty  : TJSONObject;
begin
  if FFunctions.Functions.IndexOf(ATool.GetName) >= 0 then Exit; // ya registrado

  JWrapper := TJSONObject.Create;
  JWrapper.AddPair('type', 'function');
  JFunc := TJSONObject.Create;
  JFunc.AddPair('name', ATool.GetName);
  JFunc.AddPair('description', ATool.GetDescription);

  JSchema := ATool.GetSchema;
  if Assigned(JSchema) then
    JFunc.AddPair('parameters', JSchema.Clone as TJSONObject)
  else
  begin
    JEmpty := TJSONObject.Create;
    JEmpty.AddPair('type', 'object');
    JEmpty.AddPair('properties', TJSONObject.Create);
    JFunc.AddPair('parameters', JEmpty);
  end;

  JWrapper.AddPair('function', JFunc);
  Item := FFunctions.Functions.AddFunction(ATool.GetName, True, HandleToolCall);
  try
    Item.SetJSon(JWrapper);
  finally
    JWrapper.Free;
  end;
end;

function TDmTelegramBot.FindMCPToolBySource(const ASourceId: string): IAiTool;
var
  Entries: TArray<TAiRegistryEntry>;
  E      : TAiRegistryEntry;
begin
  Result := nil;
  Entries := TAiToolRegistry.Instance.GetEntries;
  for E in Entries do
    if SameText(E.SourceId, ASourceId) and E.Tool.IsAvailable then
    begin
      Result := E.Tool;
      Exit;
    end;
end;

function TDmTelegramBot.ExtractMCPText(const AResult: TJSONObject): string;
var
  JContent : TJSONArray;
  JItem    : TJSONValue;
  ItemText : string;
begin
  Result := '';
  if not Assigned(AResult) then Exit;

  // Campo text directo
  if AResult.TryGetValue<string>('text', Result) and (Result <> '') then Exit;

  // Formato MCP: { "content": [{"type":"text","text":"..."}] }
  if AResult.TryGetValue<TJSONArray>('content', JContent) then
    for JItem in JContent do
      if (JItem is TJSONObject) and
         TJSONObject(JItem).TryGetValue<string>('text', ItemText) and
         (ItemText <> '') then
      begin
        Result := ItemText;
        Exit;
      end;
end;

function TDmTelegramBot.InstallMCPPackage(const AQuery: string;
  out APkgName, AToolsList: string): Boolean;
var
  Pkgs    : TArray<TAiPPMPackageInfo>;
  Pkg     : TAiPPMPackageInfo;
  Client  : TObject;
  Entries : TArray<TAiRegistryEntry>;
  E       : TAiRegistryEntry;
  Tools   : TStringList;
begin
  Result   := False;
  APkgName := '';
  AToolsList := '';

  Pkgs := TAiToolRegistry.Instance.SearchPPM(AQuery);
  if Length(Pkgs) = 0 then Exit;

  Pkg      := Pkgs[0];
  APkgName := Pkg.Name;

  // Verificar si ya está instalado CON PROCESO ACTIVO en esta sesión
  // (FInstalledPkgs solo contiene paquetes con cliente MCP vivo, no schema-only)
  if FInstalledPkgs.IndexOf(Pkg.Name) >= 0 then
  begin
    Entries := TAiToolRegistry.Instance.GetEntries;
    for E in Entries do
      if SameText(E.SourceId, Pkg.Name) then
        RegisterIAiToolInFunctions(E.Tool);
    Exit(True);
  end;

  // Instalar
  DoLog('SYS', Format('[PPM] Instalando %s v%s...', [Pkg.Name, Pkg.Version]));
  Client := TAiToolRegistry.Instance.InstallFromPPM(Pkg, nil);
  if Assigned(Client) then
  begin
    FInstalledClients.Add(Client);
    FInstalledPkgs.Add(Pkg.Name);   // marcar como instalado con proceso activo
    SavePPMState;                   // persistir para restaurar al reiniciar
  end
  else
  begin
    // Paquete schema-only (sin binario): registrar solo la definicion .tool
    if TAiToolRegistry.Instance.InstallSchemaFromPPM(Pkg) = 0 then Exit;
    if FInstalledPkgs.IndexOf(Pkg.Name) < 0 then
    begin
      FInstalledPkgs.Add(Pkg.Name);
      SavePPMState;
    end;
  end;

  // Registrar tools nuevas en TAiFunctions
  Tools := TStringList.Create;
  try
    Entries := TAiToolRegistry.Instance.GetEntries;
    for E in Entries do
      if SameText(E.SourceId, Pkg.Name) then
      begin
        RegisterIAiToolInFunctions(E.Tool);
        Tools.Add(E.Tool.GetName);
      end;
    AToolsList := Tools.CommaText;
  finally
    Tools.Free;
  end;

  Result := True;
end;

function TDmTelegramBot.ToolNativeTelegram(const Args: TJSONObject): string;
// Bridge nativo para el tool 'telegram' (mcp-telegram schema-only).
// Usa el Telegram Bot API directamente con FToken o bot_token del Args.
var
  Op, ChatId, Text, BotToken, ParseMode, URL: string;
  Http  : THTTPClient;
  JBody : TJSONObject;
  Body  : TStringStream;
  Resp  : IHTTPResponse;
begin
  Result := '{"ok":false,"error":"invalid args"}';
  if not Assigned(Args) then Exit;

  Op       := ''; Args.TryGetValue<string>('operation',   Op);
  ChatId   := ''; Args.TryGetValue<string>('chat_id',     ChatId);
  Text     := ''; Args.TryGetValue<string>('text',        Text);
  BotToken := ''; Args.TryGetValue<string>('bot_token',   BotToken);
  ParseMode:= ''; Args.TryGetValue<string>('parse_mode',  ParseMode);

  if BotToken = '' then BotToken := FToken;   // usar token propio del bot
  if BotToken = '' then
  begin Result := '{"ok":false,"error":"bot_token required"}'; Exit; end;

  Http  := THTTPClient.Create;
  JBody := TJSONObject.Create;
  try
    if Op = 'send_message' then
    begin
      if (ChatId = '') or (Text = '') then
      begin Result := '{"ok":false,"error":"chat_id and text required"}'; Exit; end;

      URL := TG_API + BotToken + '/sendMessage';
      JBody.AddPair('chat_id', ChatId);
      JBody.AddPair('text', Copy(Text, 1, 4000));
      if ParseMode <> '' then JBody.AddPair('parse_mode', ParseMode);

      Body := TStringStream.Create(JBody.ToJSON, TEncoding.UTF8);
      try
        Resp := Http.Post(URL, Body, nil,
          [TNameValuePair.Create('Content-Type', 'application/json')]);
        Result := Resp.ContentAsString;
        DoLog('TOOL', '[telegram] sendMessage chat=' + ChatId + ' -> ' +
          Copy(Result, 1, 80));
      finally Body.Free; end;
    end
    else if Op = 'get_updates' then
    begin
      URL    := TG_API + BotToken + '/getUpdates';
      Resp   := Http.Get(URL);
      Result := Resp.ContentAsString;
    end
    else
      Result := '{"ok":false,"error":"operation not supported: ' + Op + '"}';
  except
    on E: Exception do
      Result := '{"ok":false,"error":"' + E.Message + '"}';
  end;
  JBody.Free;
  Http.Free;
end;

procedure TDmTelegramBot.SavePPMState;
var JArr: TJSONArray; I: Integer;
begin
  if FPPMStateFile = '' then Exit;
  if not TDirectory.Exists(FDataDir) then TDirectory.CreateDirectory(FDataDir);
  JArr := TJSONArray.Create;
  try
    for I := 0 to FInstalledPkgs.Count - 1 do
      JArr.Add(FInstalledPkgs[I]);
    TFile.WriteAllText(FPPMStateFile, JArr.ToJSON, TEncoding.UTF8);
  finally JArr.Free; end;
end;

procedure TDmTelegramBot.LoadAndRestorePPMState;
var JVal: TJSONValue; JArr: TJSONArray; JItem: TJSONValue;
    PkgName, PkgQuery, ToolsList: string;
begin
  if (FPPMStateFile = '') or not TFile.Exists(FPPMStateFile) then Exit;
  try
    JVal := TJSONObject.ParseJSONValue(
      TFile.ReadAllText(FPPMStateFile, TEncoding.UTF8));
    if not (JVal is TJSONArray) then begin JVal.Free; Exit; end;
    JArr := TJSONArray(JVal);
    try
      DoLog('SYS', Format('[PPM] Restaurando %d paquete(s) guardados...', [JArr.Count]));
      for JItem in JArr do
      begin
        PkgQuery := JItem.Value;
        if PkgQuery = '' then Continue;
        if FInstalledPkgs.IndexOf(PkgQuery) >= 0 then Continue; // ya instalado (voz)
        if InstallMCPPackage(PkgQuery, PkgName, ToolsList) then
          DoLog('SYS', '[PPM] Restaurado: ' + PkgName + ' (' + ToolsList + ')')
        else
          DoLog('SYS', '[PPM] No se pudo restaurar: ' + PkgQuery);
      end;
    finally JArr.Free; end;
  except
    on E: Exception do
      DoLog('SYS', '[PPM] Error al restaurar estado: ' + E.Message);
  end;
end;

function TDmTelegramBot.ToolPPMList(const Args: TJSONObject): string;
var JObj: TJSONObject; JArr: TJSONArray; I: Integer;
begin
  JObj := TJSONObject.Create;
  JArr := TJSONArray.Create;
  try
    JObj.AddPair('installed_packages', JArr);
    JObj.AddPair('count', TJSONNumber.Create(FInstalledPkgs.Count));
    for I := 0 to FInstalledPkgs.Count - 1 do
      JArr.Add(FInstalledPkgs[I]);
    JArr := nil; // owned by JObj
    Result := JObj.ToJSON;
  finally JObj.Free; end;
end;

function TDmTelegramBot.ToolPPMClear(const Args: TJSONObject): string;
var Count: Integer;
begin
  Count := FInstalledPkgs.Count;
  // Liberar clientes MCP activos (detiene los procesos)
  FInstalledClients.Clear;
  FInstalledPkgs.Clear;
  // Nota: los tools siguen registrados en TAiFunctions pero sus clientes
  // ya no corren; llamadas fallaran con error hasta reiniciar o reinstalar.
  // Borrar archivo de estado
  if TFile.Exists(FPPMStateFile) then
    TFile.Delete(FPPMStateFile);
  DoLog('SYS', Format('[PPM] Limpiado: %d paquete(s) eliminados', [Count]));
  Result := Format('{"ok":true,"removed":%d,"message":"All PPM packages cleared. ' +
    'Use ppm_install to reinstall as needed."}', [Count]);
end;

function TDmTelegramBot.ToolPPMInstall(const Args: TJSONObject): string;
var
  Capability: string;
  PkgName   : string;
  ToolsList : string;
  JResult   : TJSONObject;
  JArr      : TJSONArray;
  ToolName  : string;
begin
  Capability := '';
  if Assigned(Args) then Args.TryGetValue<string>('capability', Capability);

  if Capability = '' then
  begin
    Result := '{"error": "capability parameter required"}';
    Exit;
  end;

  DoLog('TOOL', '[PPM] Buscando: ' + Capability);

  if InstallMCPPackage(Capability, PkgName, ToolsList) then
  begin
    DoLog('TOOL', Format('[PPM] %s instalado - tools: %s', [PkgName, ToolsList]));
    JResult := TJSONObject.Create;
    JArr    := TJSONArray.Create;
    try
      JResult.AddPair('installed', PkgName);
      JResult.AddPair('tools', JArr);
      for ToolName in ToolsList.Split([',']) do
        if Trim(ToolName) <> '' then JArr.Add(Trim(ToolName));
      JResult.AddPair('message',
        'Tools installed and available. Call them directly in the next step.');
      Result := JResult.ToJSON;
      JArr := nil;  // owned by JResult
    finally
      JResult.Free;
    end;
  end
  else
  begin
    Result := Format('{"error": "Could not install capability: %s. ' +
      'Try a more specific keyword."}', [Capability]);
    DoLog('TOOL', '[PPM] Instalación fallida para: ' + Capability);
  end;
end;

end.




