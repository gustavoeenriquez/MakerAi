unit uApp.ChatBridge;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.DateUtils, System.Math,
  System.Net.HttpClient,
  AIChat.Types, AIChat.Control.FMX, AIChat.Input,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor,
  uMakerAi.Tools.ComputerUse,
  uMakerAi.Whisper,
  uMakerAi.Prompts;

type
  TChatBridge = class
  private
    FConn           : TAiChatConnection;
    FFunctions      : TAiFunctions;
    FPrompts        : TAiPrompts;
    FChatView       : TAIChatView;
    FChatInput      : TAIChatInput;
    FCurrentMsgIndex: Integer;
    FCurrentConvId  : string;

    // Callbacks to form
    FOnThinking   : TProc<string>;
    FOnStatsUpdate: TProc<Integer, Integer, Integer, Integer>;
    FOnLog        : TProc<string>;

    // Extended tools
    FShell         : TAiShell;
    FEditor        : TAiTextEditorTool;
    FComputerUse   : TAiComputerUseTool;
    FWhisper       : TAIWhisper;

    // Stats
    FPromptTokens    : Integer;
    FCompletionTokens: Integer;
    FThinkingTokens  : Integer;
    FTotalTokens     : Integer;

    // Event handlers for FConn (must be of object)
    procedure HandleReceiveData(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string);
    procedure HandleReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string);
    procedure HandleReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string);
    procedure HandleError(Sender: TObject; const ErrorMsg: string;
      AException: Exception; const AResponse: IHTTPResponse);

    // Function action handlers (of object)
    procedure OnDateTimeAction(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure OnCalculateAction(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure OnEditorAction(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);

    // Shell log callback
    procedure HandleShellLog(Sender: TObject; const Command, StdOut, StdErr: string;
      ExitCode: Integer);

    procedure RegisterFunctions;
    procedure RegisterTextEditorFunction;
    procedure UpdateStats(AResponse: TJSONObject);
    procedure FireStatsUpdate;
    procedure FireLog(const AText: string);

    function  GetShellEnabled: Boolean;
    procedure SetShellEnabled(AValue: Boolean);
    function  GetEditorEnabled: Boolean;
    procedure SetEditorEnabled(AValue: Boolean);
    function  GetComputerUseEnabled: Boolean;
    procedure SetComputerUseEnabled(AValue: Boolean);

  public
    constructor Create(AChatView: TAIChatView; AChatInput: TAIChatInput);
    destructor  Destroy; override;

    procedure ApplySettings;
    procedure SendMessage(const AText: string; AAttachments: TAIChatAttachments;
      AAudio: TMemoryStream);
    procedure AbortGeneration;
    procedure NewChat;
    procedure LoadMCPConfig(const AFilePath: string);
    procedure ResetStats;

    property Connection    : TAiChatConnection read FConn;
    property CurrentConvId : string            read FCurrentConvId write FCurrentConvId;

    property OnThinking   : TProc<string>                        read FOnThinking    write FOnThinking;
    property OnStatsUpdate: TProc<Integer, Integer, Integer, Integer> read FOnStatsUpdate write FOnStatsUpdate;
    property OnLog        : TProc<string>                        read FOnLog         write FOnLog;

    property ShellEnabled      : Boolean read GetShellEnabled       write SetShellEnabled;
    property EditorEnabled     : Boolean read GetEditorEnabled      write SetEditorEnabled;
    property ComputerUseEnabled: Boolean read GetComputerUseEnabled write SetComputerUseEnabled;

    property Shell      : TAiShell          read FShell;
    property Editor     : TAiTextEditorTool read FEditor;
    property ComputerUse: TAiComputerUseTool read FComputerUse;
    property Whisper    : TAIWhisper        read FWhisper;
  end;

implementation

uses
  uApp.Settings;

{ TChatBridge }

constructor TChatBridge.Create(AChatView: TAIChatView; AChatInput: TAIChatInput);
begin
  inherited Create;
  FChatView        := AChatView;
  FChatInput       := AChatInput;
  FCurrentMsgIndex := -1;

  // Connection
  FConn := TAiChatConnection.Create(nil);
  FConn.OnReceiveData     := HandleReceiveData;
  FConn.OnReceiveDataEnd  := HandleReceiveDataEnd;
  FConn.OnReceiveThinking := HandleReceiveThinking;
  FConn.OnError           := HandleError;
  FConn.AiChat.Asynchronous := True;

  // Functions
  FFunctions := TAiFunctions.Create(nil);
  FPrompts   := TAiPrompts.Create(nil);

  // Tools
  FShell       := TAiShell.Create(nil);
  FEditor      := TAiTextEditorTool.Create(nil);
  FComputerUse := TAiComputerUseTool.Create(nil);
  FWhisper     := TAIWhisper.Create(nil);

  FShell.OnConsoleLog := HandleShellLog;

  RegisterFunctions;

  FConn.AiFunctions := FFunctions;

  ApplySettings;
end;

destructor TChatBridge.Destroy;
begin
  FConn.Free;
  FFunctions.Free;
  FPrompts.Free;
  FShell.Free;
  FEditor.Free;
  FComputerUse.Free;
  FWhisper.Free;
  inherited;
end;

procedure TChatBridge.RegisterFunctions;
var
  LFunc : TFunctionActionItem;
  LParam: TFunctionParamsItem;
begin
  // get_datetime
  LFunc := FFunctions.Functions.AddFunction('get_datetime', True, OnDateTimeAction);
  LFunc.Description.Text := 'Returns the current date and time.';

  // calculate
  LFunc := FFunctions.Functions.AddFunction('calculate', True, OnCalculateAction);
  LFunc.Description.Text := 'Evaluates a simple arithmetic expression and returns the result.';
  LParam := LFunc.Parameters.Add;
  LParam.Name := 'expression';
  LParam.ParamType := ptString;
  LParam.Required := True;
  LParam.Description.Text := 'Arithmetic expression to evaluate (e.g. "2 + 3 * 4")';

  // Shell registered separately when enabled
  // Editor registered separately when enabled
end;

procedure TChatBridge.RegisterTextEditorFunction;
var
  LFunc : TFunctionActionItem;
  LParam: TFunctionParamsItem;
begin
  if FFunctions.Functions.IndexOf('str_replace_editor') >= 0 then Exit;

  LFunc := FFunctions.Functions.AddFunction('str_replace_editor', True, OnEditorAction);
  LFunc.Description.Text :=
    'View and edit files using commands: view, create, str_replace, insert, undo_edit. ' +
    'Use this tool to read, create, or modify files on the filesystem.';

  LParam := LFunc.Parameters.Add;
  LParam.Name := 'command';
  LParam.ParamType := ptString;
  LParam.Required := True;
  LParam.Description.Text := 'Command: view, create, str_replace, insert, undo_edit';

  LParam := LFunc.Parameters.Add;
  LParam.Name := 'path';
  LParam.ParamType := ptString;
  LParam.Required := True;
  LParam.Description.Text := 'File path to operate on';

  LParam := LFunc.Parameters.Add;
  LParam.Name := 'file_text';
  LParam.ParamType := ptString;
  LParam.Required := False;
  LParam.Description.Text := 'Content for create command';

  LParam := LFunc.Parameters.Add;
  LParam.Name := 'old_str';
  LParam.ParamType := ptString;
  LParam.Required := False;
  LParam.Description.Text := 'Text to find for str_replace';

  LParam := LFunc.Parameters.Add;
  LParam.Name := 'new_str';
  LParam.ParamType := ptString;
  LParam.Required := False;
  LParam.Description.Text := 'Replacement text for str_replace';

  LParam := LFunc.Parameters.Add;
  LParam.Name := 'insert_line';
  LParam.ParamType := ptInteger;
  LParam.Required := False;
  LParam.Description.Text := 'Line number for insert command';

  LParam := LFunc.Parameters.Add;
  LParam.Name := 'new_str';
  LParam.ParamType := ptString;
  LParam.Required := False;
  LParam.Description.Text := 'Text to insert';
end;

{ ── Event handlers ── }

procedure TChatBridge.HandleReceiveData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
begin
  TThread.Synchronize(nil, procedure
  begin
    if FCurrentMsgIndex >= 0 then
      FChatView.AppendToken(FCurrentMsgIndex, aText);
  end);
end;

procedure TChatBridge.HandleReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
begin
  TThread.Synchronize(nil, procedure
  begin
    if FCurrentMsgIndex >= 0 then
      FChatView.FinishMessage(FCurrentMsgIndex);
    FChatInput.Busy := False;
    UpdateStats(aResponse);
    FireStatsUpdate;
  end);
end;

procedure TChatBridge.HandleReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
begin
  TThread.Synchronize(nil, procedure
  begin
    if Assigned(FOnThinking) then
      FOnThinking(aText);
  end);
end;

procedure TChatBridge.HandleError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  TThread.Synchronize(nil, procedure
  begin
    if FCurrentMsgIndex >= 0 then
    begin
      FChatView.AppendToken(FCurrentMsgIndex, #13#10 + '[Error: ' + ErrorMsg + ']');
      FChatView.FinishMessage(FCurrentMsgIndex);
    end;
    FChatInput.Busy := False;
  end);
end;

{ ── Function handlers ── }

procedure TChatBridge.OnDateTimeAction(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
  Handled := True;
end;

procedure TChatBridge.OnCalculateAction(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  // Simple placeholder — returns the expression unchanged
  ToolCall.Response := 'Expression: ' + ToolCall.Params.Values['expression'] +
    ' = (calculation not implemented in demo)';
  Handled := True;
end;

procedure TChatBridge.OnEditorAction(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  LResult: string;
begin
  try
    LResult := FEditor.Execute(ToolCall.Arguments);
    ToolCall.Response := LResult;
    FireLog('[Editor] ' + ToolCall.Params.Values['command'] + ': ' +
      ToolCall.Params.Values['path']);
  except
    on E: Exception do
    begin
      ToolCall.Response := 'Editor error: ' + E.Message;
      FireLog('[Editor error] ' + E.Message);
    end;
  end;
  Handled := True;
end;

procedure TChatBridge.HandleShellLog(Sender: TObject;
  const Command, StdOut, StdErr: string; ExitCode: Integer);
begin
  var LText := '$ ' + Command;
  if StdOut <> '' then LText := LText + #13#10 + StdOut;
  if StdErr <> '' then LText := LText + #13#10 + '[stderr] ' + StdErr;
  LText := LText + #13#10 + '[exit: ' + IntToStr(ExitCode) + ']';
  TThread.Synchronize(nil, procedure
  begin
    FireLog(LText);
  end);
end;

{ ── Stats ── }

procedure TChatBridge.UpdateStats(AResponse: TJSONObject);
var
  JUsage : TJSONObject;
begin
  if not Assigned(AResponse) then Exit;
  JUsage := AResponse.GetValue<TJSONObject>('usage');
  if not Assigned(JUsage) then Exit;
  JUsage.TryGetValue<Integer>('prompt_tokens',     FPromptTokens);
  JUsage.TryGetValue<Integer>('completion_tokens', FCompletionTokens);
  JUsage.TryGetValue<Integer>('total_tokens',      FTotalTokens);
  // Claude / thinking tokens
  var JThinking := AResponse.GetValue<TJSONObject>('thinking');
  if Assigned(JThinking) then
    JThinking.TryGetValue<Integer>('budget_tokens', FThinkingTokens);
  // Accumulate connection-level tokens
  if FTotalTokens = 0 then
    FTotalTokens := FPromptTokens + FCompletionTokens;
end;

procedure TChatBridge.FireStatsUpdate;
begin
  if Assigned(FOnStatsUpdate) then
    FOnStatsUpdate(FTotalTokens, FPromptTokens, FCompletionTokens, FThinkingTokens);
end;

procedure TChatBridge.FireLog(const AText: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AText);
end;

procedure TChatBridge.ResetStats;
begin
  FPromptTokens     := 0;
  FCompletionTokens := 0;
  FThinkingTokens   := 0;
  FTotalTokens      := 0;
  FireStatsUpdate;
end;

{ ── Properties ── }

function TChatBridge.GetShellEnabled: Boolean;
begin
  Result := FShell.Active;
end;

procedure TChatBridge.SetShellEnabled(AValue: Boolean);
begin
  if AValue = FShell.Active then Exit;
  if AValue then
  begin
    FShell.RegisterInFunctions(FFunctions);
    FShell.Active := True;
    FireLog('[Shell tool enabled]');
  end
  else
  begin
    FShell.Active := False;
    FireLog('[Shell tool disabled]');
  end;
end;

function TChatBridge.GetEditorEnabled: Boolean;
begin
  Result := FFunctions.Functions.IndexOf('str_replace_editor') >= 0;
end;

procedure TChatBridge.SetEditorEnabled(AValue: Boolean);
var
  LFunc: TFunctionActionItem;
begin
  if AValue = GetEditorEnabled then Exit;
  if AValue then
  begin
    RegisterTextEditorFunction;
    FireLog('[Text Editor tool enabled]');
  end
  else
  begin
    LFunc := FFunctions.Functions.GetFunction('str_replace_editor');
    if Assigned(LFunc) then
      FFunctions.Functions.Delete(LFunc.Index);
    FireLog('[Text Editor tool disabled]');
  end;
end;

function TChatBridge.GetComputerUseEnabled: Boolean;
begin
  Result := Assigned(FConn.ComputerUseTool);
end;

procedure TChatBridge.SetComputerUseEnabled(AValue: Boolean);
begin
  if AValue then
  begin
    FConn.ComputerUseTool := FComputerUse;
    FireLog('[Computer Use tool enabled]');
  end
  else
  begin
    FConn.ComputerUseTool := nil;
    FireLog('[Computer Use tool disabled]');
  end;
end;

{ ── Public methods ── }

procedure TChatBridge.ApplySettings;
begin
  FConn.DriverName := AppSettings.ProviderName;
  FConn.Model      := AppSettings.ModelName;
  var LKey := AppSettings.ApiKey[AppSettings.ProviderName];
  if LKey <> '' then
    FConn.Params.Values['ApiKey'] := LKey;
  FConn.SystemPrompt.Text := AppSettings.SystemPrompt;
  FConn.AiChat.Asynchronous := True;
  // Update Whisper key (uses OpenAI endpoint)
  var LWhisperKey := AppSettings.ApiKey['OpenAI'];
  if LWhisperKey <> '' then
    FWhisper.ApiKey := LWhisperKey;
end;

procedure TChatBridge.SendMessage(const AText: string;
  AAttachments: TAIChatAttachments; AAudio: TMemoryStream);
var
  LText : string;
  LMsg  : TAiChatMessage;
  I     : Integer;
begin
  LText := AText;

  // Transcribe audio if provided
  if Assigned(AAudio) and (AAudio.Size > 0) then
  begin
    try
      AAudio.Position := 0;
      var LTranscript := FWhisper.Transcription(AAudio, 'audio.wav', '');
      if LTranscript <> '' then
        LText := LTranscript
      else if LText = '' then
        LText := '[Audio message - transcription failed]';
    except
      on E: Exception do
        if LText = '' then
          LText := '[Audio transcription error: ' + E.Message + ']';
    end;
  end;

  if LText = '' then Exit;

  // Add to chat view
  if Assigned(AAttachments) and (AAttachments.Count > 0) then
    FChatView.AddUserMessage(LText, AAttachments)
  else
    FChatView.AddUserMessage(LText);
  FChatView.BeginAssistantMessage;
  FCurrentMsgIndex := FChatView.MessageCount - 1;
  FChatInput.Busy := True;

  // Thinking memo
  if Assigned(FOnThinking) then FOnThinking('');

  // Create AI message and attach media files
  LMsg := FConn.AddMessage(LText, 'user');
  if Assigned(AAttachments) then
    for I := 0 to AAttachments.Count - 1 do
    begin
      var LAttach := AAttachments[I];
      if Length(LAttach.Data) > 0 then
      begin
        var LStream := TMemoryStream.Create;
        try
          LStream.WriteBuffer(LAttach.Data[0], Length(LAttach.Data));
          LStream.Position := 0;
          LMsg.LoadMediaFromStream(LAttach.FileName, LStream);
        finally
          LStream.Free;
        end;
      end;
    end;

  // Run async
  FConn.Run;
end;

procedure TChatBridge.AbortGeneration;
begin
  FConn.Abort;
  TThread.Synchronize(nil, procedure
  begin
    if FCurrentMsgIndex >= 0 then
    begin
      FChatView.FinishMessage(FCurrentMsgIndex);
      FCurrentMsgIndex := -1;
    end;
    FChatInput.Busy := False;
  end);
end;

procedure TChatBridge.NewChat;
begin
  FConn.NewChat;
  FChatView.ClearMessages;
  FCurrentMsgIndex := -1;
  ResetStats;
end;

procedure TChatBridge.LoadMCPConfig(const AFilePath: string);
var
  LCount: Integer;
begin
  try
    LCount := FFunctions.ImportClaudeMCPConfiguration(AFilePath);
    FireLog(Format('[MCP] Loaded %d server(s) from: %s', [LCount, AFilePath]));
  except
    on E: Exception do
      FireLog('[MCP error] ' + E.Message);
  end;
end;

end.
