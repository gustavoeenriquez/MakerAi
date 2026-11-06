unit uDMAgent;

// =============================================================================
// MakerAI - Demo 059: uDMAgent
//
// Core AI agent DataModule - independent of Telegram (or any frontend).
// Uses design-time components: TAiChatConnection, TAiFunctions, TAiPrompts.
//
// Bootstrap tool: ppm_install (defined at design-time in DFM).
// Dynamic tools: downloaded from PPM registry on demand and registered at runtime.
//
// Usage (single agent):
//   DmAgent := TDmAgent.Create(nil);
//   DmAgent.Initialize(TPath.Combine(GetEnvVar('APPDATA'), 'MakerAI'));
//   DmAgent.OnLog := MyLogHandler;
//   Response := DmAgent.ProcessText('Hello');
//
// Usage (multi-user via TAgentManager):
//   FManager := TAgentManager.Create(DataDir);
//   Agent    := FManager.GetOrCreate('user_12345');
//   Agent.OnLog := MyLogHandler;
//   Response := Agent.ProcessText(Text);
// =============================================================================

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils,
  System.Generics.Collections, System.IOUtils, System.NetEncoding,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Agents.IAiTool,
  uMakerAi.Agents.ToolRegistry,
  uMakerAi.Prompts;

type
  TAgentLogEvent = procedure(const ADir, AText: string) of object;

  // ---------------------------------------------------------------------------
  // TDmAgent - single AI agent instance
  // Design-time components: AiConn, AgentFuncs, AgentPrompts
  // ---------------------------------------------------------------------------
  TDmAgent = class(TDataModule)
    AiConn      : TAiChatConnection;
    AgentFuncs  : TAiFunctions;
    AgentPrompts: TAiPrompts;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    // ppm_install action wired in DFM
    procedure AgentFuncsPPMInstall(Sender: TObject;
      Action: TFunctionActionItem; FunctionName: string;
      ToolCall: TAiToolsFunction; var Handled: Boolean);
  private
    FAgentId     : string;
    FDataDir     : string;
    FClients     : TObjectList<TObject>;   // lifetime of installed MCP clients
    FOnLog       : TAgentLogEvent;
    FAudioB64    : string;   // base64 audio from last TTS tool call
    FAudioFormat : string;   // format of FAudioB64 (wav, mp3, ogg…)

    procedure HandleDynTool(Sender: TObject;
      Action: TFunctionActionItem; FunctionName: string;
      ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure RegisterDynTool(const ATool: IAiTool);
    procedure OnAiError(Sender: TObject; const ErrorMsg: string;
      E: Exception; const AResponse: IHTTPResponse);
    procedure DoLog(const ADir, AText: string);
  public
    // Call once after Create. Sets up LLM params and PPM registry.
    procedure Initialize(const ADataDir: string; const AAgentId: string = '');
    // Run one user turn. Handles tool calls internally. Returns LLM response.
    function  ProcessText(const AText: string): string;
    // Clear conversation history (keep system prompt).
    procedure ClearHistory;

    property AgentId     : string         read FAgentId     write FAgentId;
    property OnLog       : TAgentLogEvent read FOnLog       write FOnLog;
    property AudioB64    : string         read FAudioB64;
    property AudioFormat : string         read FAudioFormat;
  end;

  // ---------------------------------------------------------------------------
  // TAgentManager - one TDmAgent per string ID (e.g. Telegram chat_id)
  // ---------------------------------------------------------------------------
  TAgentManager = class
  private
    FAgents  : TObjectDictionary<string, TDmAgent>;
    FDataDir : string;
  public
    constructor Create(const ADataDir: string);
    destructor  Destroy; override;

    // Returns existing agent or creates a new initialized one.
    function  GetOrCreate(const AId: string): TDmAgent;
    function  TryGet(const AId: string; out AAgent: TDmAgent): Boolean;
    procedure Remove(const AId: string);
    procedure Clear;
    function  Count: Integer;
  end;

var
  DmAgent: TDmAgent;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}


// =============================================================================
// TDmAgent — DataModule lifecycle
// =============================================================================

procedure TDmAgent.DataModuleCreate(Sender: TObject);
begin
  FClients := TObjectList<TObject>.Create(True);
  FAgentId := '';
  FDataDir := '';
end;

procedure TDmAgent.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FClients);
end;

// =============================================================================
// TDmAgent — public interface
// =============================================================================

procedure TDmAgent.Initialize(const ADataDir: string; const AAgentId: string);
begin
  FDataDir := ADataDir;
  FAgentId := AAgentId;

  // Wire error handler so LLM errors appear in the log
  AiConn.OnError := OnAiError;

  // Apply system prompt from design-time TAiPrompts collection
  if AgentPrompts.Items.Count > 0 then
    AiConn.SystemPrompt.Text := AgentPrompts.GetString('system');

  // Resolve API key from environment if it starts with '@'
  if (AiConn.Params.IndexOfName('ApiKey') >= 0) then
  begin
    var AK := AiConn.Params.Values['ApiKey'];
    if (Length(AK) > 1) and (AK[1] = '@') then
      AiConn.Params.Values['ApiKey'] := GetEnvironmentVariable(Copy(AK, 2, MaxInt));
  end;

  // PPM registry setup
  TAiToolRegistry.Instance.PPMBaseUrl := AgentFuncs.AutoMCPConfig.RegistryUrl;
  if ADataDir <> '' then
    TAiToolRegistry.Instance.ToolsDir := TPath.Combine(ADataDir, 'tools');

  DoLog('SYS', 'Agent initialized: id=' + IfThen(FAgentId <> '', FAgentId, '(default)'));
end;

function TDmAgent.ProcessText(const AText: string): string;
begin
  FAudioB64    := '';
  FAudioFormat := '';
  DoLog('IN', AText);
  try
    Result := AiConn.AddMessageAndRun(AText, 'user', []);
  except
    on E: Exception do
    begin
      DoLog('ERR', 'LLM: ' + E.Message);
      Result := 'Error: ' + E.Message;
    end;
  end;
  if Result = '' then Result := '(no response)';
  DoLog('OUT', Copy(Result, 1, 120));
end;

procedure TDmAgent.ClearHistory;
begin
  AiConn.Messages.Clear;
  DoLog('SYS', 'History cleared');
end;

// =============================================================================
// TDmAgent — ppm_install handler (wired in DFM)
// =============================================================================

procedure TDmAgent.AgentFuncsPPMInstall(Sender: TObject;
  Action: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Args      : TJSONObject;
  Capability: string;
  Pkgs      : TArray<TAiPPMPackageInfo>;
  Pkg       : TAiPPMPackageInfo;
  Client    : TObject;
  Entry     : TAiRegistryEntry;
  Count     : Integer;
begin
  Handled := True;

  // Parse arguments
  Args := nil;
  if ToolCall.Arguments <> '' then
    try Args := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
    except Args := nil; end;
  try
    Capability := '';
    if Assigned(Args) then Args.TryGetValue<string>('capability', Capability);
  finally
    FreeAndNil(Args);
  end;

  if Capability = '' then
  begin
    ToolCall.Response := '{"error":"capability parameter required"}';
    Exit;
  end;

  DoLog('TOOL', '[PPM] searching: ' + Capability);

  Pkgs := TAiToolRegistry.Instance.SearchPPM(Capability);
  if Length(Pkgs) = 0 then
  begin
    ToolCall.Response := '{"error":"no packages found for: ' + Capability + '"}';
    Exit;
  end;

  Pkg   := Pkgs[0];
  Count := 0;
  DoLog('SYS', '[PPM] installing: ' + Pkg.Name + ' v' + Pkg.Version);

  Client := TAiToolRegistry.Instance.InstallFromPPM(Pkg, nil);
  if Assigned(Client) then
  begin
    // Binary MCP package — process stays alive for subsequent calls
    FClients.Add(Client);
    for Entry in TAiToolRegistry.Instance.GetEntries do
      if SameText(Entry.SourceId, Pkg.Name) then
      begin
        RegisterDynTool(Entry.Tool);
        Inc(Count);
      end;
  end
  else
  begin
    // Schema-only package — no subprocess
    Count := TAiToolRegistry.Instance.InstallSchemaFromPPM(Pkg);
    for Entry in TAiToolRegistry.Instance.GetEntries do
      if SameText(Entry.SourceId, Pkg.Name) then
        RegisterDynTool(Entry.Tool);
  end;

  ToolCall.Response := Format(
    '{"installed":"%s","version":"%s","tools_added":%d,' +
    '"message":"Tools are ready to use immediately."}',
    [Pkg.Name, Pkg.Version, Count]);

  DoLog('SYS', Format('[PPM] %s installed — %d tool(s)', [Pkg.Name, Count]));
end;

// =============================================================================
// TDmAgent — dynamic tool dispatcher
// =============================================================================

// Called for every tool that was registered via RegisterDynTool.
procedure TDmAgent.HandleDynTool(Sender: TObject;
  Action: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  DynTool: IAiTool;
  Args   : TJSONObject;
  JRes   : TJSONObject;
begin
  Handled := True;
  DoLog('TOOL', '[DYN] calling: ' + FunctionName + ' | args: ' + Copy(ToolCall.Arguments, 1, 200));
  if not TAiToolRegistry.Instance.TryFind(FunctionName, DynTool) then
  begin
    ToolCall.Response := '{"error":"tool not found: ' + FunctionName + '"}';
    Exit;
  end;

  Args := nil;
  if ToolCall.Arguments <> '' then
    try Args := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
    except Args := nil; end;
  try
    JRes := nil;
    try
      JRes := DynTool.Execute(Args);  // caller owns result
    except
      on E: Exception do
      begin
        ToolCall.Response := Format('{"error":"%s"}',
          [StringReplace(E.Message, '"', '\"', [rfReplaceAll])]);
        DoLog('ERR', '[DYN] ' + FunctionName + ' exception: ' + E.Message);
        Exit;
      end;
    end;

    if Assigned(JRes) then
    begin
      ToolCall.Response := JRes.ToJSON;
      DoLog('TOOL', '[DYN] response: ' + Copy(ToolCall.Response, 1, 200));
      // Extract audio_b64 — handles MCP content[] format and direct format
      var JAudio: string := '';
      var JFmt  : string := 'wav';
      var JContent: TJSONArray;
      if JRes.TryGetValue<TJSONArray>('content', JContent) and (JContent.Count > 0) then
      begin
        var JItem := JContent.Items[0] as TJSONObject;
        if Assigned(JItem) then
        begin
          var SText: string;
          if JItem.TryGetValue<string>('text', SText) and (SText <> '') then
          begin
            var JInner := TJSONObject.ParseJSONValue(SText) as TJSONObject;
            if Assigned(JInner) then
            try
              JInner.TryGetValue<string>('audio_b64', JAudio);
              JInner.TryGetValue<string>('format', JFmt);
            finally
              JInner.Free;
            end;
          end;
        end;
      end
      else
      begin
        JRes.TryGetValue<string>('audio_b64', JAudio);
        JRes.TryGetValue<string>('format', JFmt);
      end;
      // Fallback: read from output_path if audio_b64 not present
      if JAudio = '' then
      begin
        var OutPath: string := '';
        var JContentArr2: TJSONArray;
        if JRes.TryGetValue<TJSONArray>('content', JContentArr2) and (JContentArr2.Count > 0) then
        begin
          var JItem2 := JContentArr2.Items[0] as TJSONObject;
          if Assigned(JItem2) then
          begin
            var SText2: string;
            if JItem2.TryGetValue<string>('text', SText2) and (SText2 <> '') then
            begin
              var JInner2 := TJSONObject.ParseJSONValue(SText2) as TJSONObject;
              if Assigned(JInner2) then
              try
                JInner2.TryGetValue<string>('output_path', OutPath);
                JInner2.TryGetValue<string>('format', JFmt);
              finally
                JInner2.Free;
              end;
            end;
          end;
        end
        else
          JRes.TryGetValue<string>('output_path', OutPath);

        if (OutPath <> '') and TFile.Exists(OutPath) then
        begin
          try
            var FileBytes := TFile.ReadAllBytes(OutPath);
            JAudio := TNetEncoding.Base64.EncodeBytesToString(FileBytes);
            DoLog('TOOL', '[DYN] audio read from file: ' + (Length(FileBytes) div 1024).ToString + ' KB, fmt=' + JFmt);
            try TFile.Delete(OutPath); except end;
          except
            on E: Exception do
              DoLog('ERR', '[DYN] audio file read failed: ' + E.Message);
          end;
        end;
      end;

      if JAudio <> '' then
      begin
        FAudioB64    := JAudio;
        FAudioFormat := JFmt;
        DoLog('TOOL', '[DYN] audio captured: ' + (Length(FAudioB64) div 1024).ToString + ' KB, fmt=' + FAudioFormat);
      end;
      JRes.Free;
    end
    else
    begin
      ToolCall.Response := Format(
        '{"error":"Tool ''%s'' returned no output. ' +
        'Check that required env vars are set (e.g. GITHUB_TOKEN, API keys)."}',
        [FunctionName]);
      DoLog('ERR', '[DYN] ' + FunctionName + ' returned nil (no output)');
    end;
  finally
    FreeAndNil(Args);
  end;
end;

// Register a PPM tool into AgentFuncs so the LLM can call it.
procedure TDmAgent.RegisterDynTool(const ATool: IAiTool);
var
  Item            : TFunctionActionItem;
  JWrapper, JFunc : TJSONObject;
  JSchema, JEmpty : TJSONObject;
begin
  // Skip if already registered (handles re-installs gracefully)
  if AgentFuncs.Functions.IndexOf(ATool.GetName) >= 0 then Exit;

  JWrapper := TJSONObject.Create;
  JWrapper.AddPair('type', 'function');
  JFunc := TJSONObject.Create;
  JFunc.AddPair('name',        ATool.GetName);
  JFunc.AddPair('description', ATool.GetDescription);

  // Clone schema — GetSchema returns a read-only ref owned by the tool
  JSchema := ATool.GetSchema;
  if Assigned(JSchema) then
    JFunc.AddPair('parameters', JSchema.Clone as TJSONObject)
  else
  begin
    JEmpty := TJSONObject.Create;
    JEmpty.AddPair('type',       'object');
    JEmpty.AddPair('properties', TJSONObject.Create);
    JFunc.AddPair('parameters', JEmpty);
  end;
  JWrapper.AddPair('function', JFunc);

  Item := AgentFuncs.Functions.AddFunction(ATool.GetName, True, HandleDynTool);
  try   Item.SetJSon(JWrapper);
  finally JWrapper.Free; end;

  DoLog('SYS', '[DYN] registered: ' + ATool.GetName);
end;

// =============================================================================
// TDmAgent — private helpers
// =============================================================================

procedure TDmAgent.OnAiError(Sender: TObject; const ErrorMsg: string;
  E: Exception; const AResponse: IHTTPResponse);
begin
  DoLog('ERR', 'LLM: ' + ErrorMsg);
end;

procedure TDmAgent.DoLog(const ADir, AText: string);
begin
  if Assigned(FOnLog) then FOnLog(ADir, AText);
end;

// =============================================================================
// TAgentManager
// =============================================================================

constructor TAgentManager.Create(const ADataDir: string);
begin
  inherited Create;
  FDataDir := ADataDir;
  FAgents  := TObjectDictionary<string, TDmAgent>.Create([doOwnsValues]);
end;

destructor TAgentManager.Destroy;
begin
  FreeAndNil(FAgents);
  inherited;
end;

function TAgentManager.GetOrCreate(const AId: string): TDmAgent;
begin
  if not FAgents.TryGetValue(AId, Result) then
  begin
    Result := TDmAgent.Create(nil);
    Result.Initialize(FDataDir, AId);
    FAgents.Add(AId, Result);
  end;
end;

function TAgentManager.TryGet(const AId: string; out AAgent: TDmAgent): Boolean;
begin
  Result := FAgents.TryGetValue(AId, AAgent);
end;

procedure TAgentManager.Remove(const AId: string);
begin
  FAgents.Remove(AId);
end;

procedure TAgentManager.Clear;
begin
  FAgents.Clear;
end;

function TAgentManager.Count: Integer;
begin
  Result := FAgents.Count;
end;

end.
