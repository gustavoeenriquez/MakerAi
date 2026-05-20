unit uMainChatFull;

// =============================================================================
// MakerAI Chat — All Functions Demo (v3.4)
//
// Features vs MakerAIChat base:
//   - Tool calling: Shell (bash), Text Editor, Computer Use
//   - MCP (Model Context Protocol) configuration
//   - Live token / cost statistics
//   - Whisper STT for audio transcription
// =============================================================================

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Edit, FMX.Memo,
  FMX.TabControl, FMX.Controls.Presentation,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellAPI,
  {$ENDIF}
  AIChat.Types,
  AIChat.Control.FMX,
  AIChat.Input,
  System.Skia, FMX.Skia,
  uAudio.Player;

type
  TFrmChatFull = class(TForm)
    ToolBar1          : TToolBar;
    BtnSidebar        : TButton;
    BtnNewChat        : TButton;
    LblProvider       : TLabel;
    CmbProvider       : TComboBox;
    CmbModel          : TComboBox;
    BtnTheme          : TButton;
    BtnSettings       : TButton;
    LayoutMain        : TLayout;
    RectSidebar       : TRectangle;
    TabSidebar        : TTabControl;
    TabChats          : TTabItem;
    TabParams         : TTabItem;
    TabTools          : TTabItem;
    TabStats          : TTabItem;
    ScrParams         : TVertScrollBox;
    ScrTools          : TVertScrollBox;
    ScrStats          : TVertScrollBox;
    LayoutSidebarInner: TLayout;
    LblConversations  : TLabel;
    BtnNewConv        : TButton;
    LstConversations  : TListBox;
    BtnDeleteConv     : TButton;
    LayoutChat        : TLayout;
    ChatView          : TAIChatView;
    ChatInput         : TAIChatInput;
    MemoThinking      : TMemo;
    SplitterSidebar   : TSplitter;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSidebar_Click(Sender: TObject);
    procedure BtnNewChat_Click(Sender: TObject);
    procedure BtnNewConv_Click(Sender: TObject);
    procedure BtnTheme_Click(Sender: TObject);
    procedure BtnSettings_Click(Sender: TObject);
    procedure CmbProvider_Change(Sender: TObject);
    procedure CmbModel_Change(Sender: TObject);
    procedure LstConversations_ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure BtnDeleteConv_Click(Sender: TObject);
    procedure ChatInput_Send(Sender: TObject; const APrompt: string;
      AAttachments: TAIChatAttachments; AAudio: TMemoryStream);
    procedure ChatInput_Cancel(Sender: TObject);
    procedure ChatInput_MicToggle(Sender: TObject; AActivate: Boolean);
    procedure ChatView_LinkClick(Sender: TObject; AMsg: TAIChatMessage;
      const AURL: string);
    procedure ChatView_AttachOpen(Sender: TObject; AMsg: TAIChatMessage;
      AAttach: TAIChatAttachment);
  private
    FCurrentConvIndex : Integer;
    FSidebarVisible   : Boolean;
    FIgnoreModelChange: Boolean;
    FAudioPlayer      : TAudioPlayer;

    // ── Params tab controls (created dynamically) ──────────────────────────
    FIgnoreParams     : Boolean;
    FCmbChatMode      : TComboBox;
    FCmbThinkingLevel : TComboBox;
    FChkToolActive    : TCheckBox;
    FCmbRespFormat    : TComboBox;
    FPnlJsonSchema    : TLayout;
    FMemoJsonSchema   : TMemo;
    FTrkTemperature   : TTrackBar;
    FLblTempValue     : TLabel;
    FEdtMaxTokens     : TEdit;
    FTrkTopP          : TTrackBar;
    FLblTopPValue     : TLabel;
    FChkModelCaps     : array[0..15] of TCheckBox;
    FChkSessionCaps   : array[0..15] of TCheckBox;

    // ── Tools tab controls (created dynamically) ───────────────────────────
    FChkShell        : TCheckBox;
    FChkEditor       : TCheckBox;
    FChkComputerUse  : TCheckBox;
    FBtnLoadMCP      : TButton;
    FMemoToolLog     : TMemo;

    // ── Stats tab controls (created dynamically) ───────────────────────────
    FEdtTotalTokens     : TEdit;
    FEdtPromptTokens    : TEdit;
    FEdtCompletionTokens: TEdit;
    FEdtThinkingTokens  : TEdit;

    // ── Sidebar / conversation management ──────────────────────────────────
    procedure InitProviderCombos;
    procedure LoadConversationList;
    procedure PopulateModels(const AProviderName: string);
    procedure SelectConversation(AIndex: Integer);
    procedure SaveCurrentConversation;
    procedure StartNewConversation(const ATitle: string = '');
    procedure UpdateThemeButton;
    procedure SyncBridgeSettings;
    function  NextConvTitle: string;

    // ── Params panel ───────────────────────────────────────────────────────
    procedure BuildParamsPanel;
    procedure LoadParamsToUI;
    procedure SyncModelConfig;
    procedure OnParamChatModeChange(Sender: TObject);
    procedure OnParamThinkingLevelChange(Sender: TObject);
    procedure OnParamToolActiveChange(Sender: TObject);
    procedure OnParamRespFormatChange(Sender: TObject);
    procedure OnParamJsonSchemaChange(Sender: TObject);
    procedure OnParamTemperatureChange(Sender: TObject);
    procedure OnParamMaxTokensChange(Sender: TObject);
    procedure OnParamTopPChange(Sender: TObject);
    procedure OnParamModelCapChange(Sender: TObject);
    procedure OnParamSessionCapChange(Sender: TObject);

    // ── Tools panel ────────────────────────────────────────────────────────
    procedure BuildToolsPanel;
    procedure OnChkShellChange(Sender: TObject);
    procedure OnChkEditorChange(Sender: TObject);
    procedure OnChkComputerUseChange(Sender: TObject);
    procedure OnBtnLoadMCPClick(Sender: TObject);
    procedure OnBridgeToolLog(const AText: string);

    // ── Stats panel ────────────────────────────────────────────────────────
    procedure BuildStatsPanel;
    procedure OnBridgeStatsUpdate(ATotal, APrompt, ACompletion, AThinking: Integer);
  end;

var
  FrmChatFull: TFrmChatFull;

implementation

{$R *.fmx}

uses
  System.JSON, System.Math,
  uApp.Settings,
  uApp.ProviderList,
  uApp.Conversation,
  uApp.ChatBridge,
  uFrm.Settings,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Core,
  uMakerAi.Chat;

var
  ChatBridge: TChatBridge;

{ ── Helpers ──────────────────────────────────────────────────────────────── }

procedure TFrmChatFull.FormCreate(Sender: TObject);
begin
  FCurrentConvIndex := -1;
  FSidebarVisible   := True;

  FAudioPlayer := TAudioPlayer.Create;
  ChatBridge   := TChatBridge.Create(ChatView, ChatInput);

  ChatBridge.OnThinking := procedure(AChunk: string)
  begin
    if AChunk = '' then
    begin
      MemoThinking.Text    := '';
      MemoThinking.Visible := False;
      Exit;
    end;
    if not MemoThinking.Visible then MemoThinking.Visible := True;
    MemoThinking.Text := MemoThinking.Text + AChunk;
    MemoThinking.GoToTextEnd;
  end;

  ChatBridge.OnStatsUpdate := OnBridgeStatsUpdate;
  ChatBridge.OnLog         := OnBridgeToolLog;

  InitProviderCombos;
  LoadConversationList;
  UpdateThemeButton;
  BuildParamsPanel;
  BuildToolsPanel;
  BuildStatsPanel;
  LoadParamsToUI;

  if ConversationStore.Count = 0 then
    StartNewConversation('New Chat')
  else
    SelectConversation(0);
end;

procedure TFrmChatFull.FormDestroy(Sender: TObject);
begin
  if Assigned(FAudioPlayer) then FAudioPlayer.Stop;
  SaveCurrentConversation;
  ConversationStore.Save;
  AppSettings.Save;
  FreeAndNil(ChatBridge);
  FreeAndNil(FAudioPlayer);
end;

{ ── Initialization ───────────────────────────────────────────────────────── }

procedure TFrmChatFull.InitProviderCombos;
begin
  FIgnoreModelChange := True;
  try
    CmbProvider.Items.Clear;
    for var I := 0 to ProviderList.Count - 1 do
      CmbProvider.Items.Add(ProviderList.Item(I).DisplayName);

    var Idx := ProviderList.IndexOf(AppSettings.ProviderName);
    if Idx >= 0 then CmbProvider.ItemIndex := Idx else CmbProvider.ItemIndex := 0;

    PopulateModels(AppSettings.ProviderName);

    var MIdx := CmbModel.Items.IndexOf(AppSettings.ModelName);
    if MIdx >= 0 then
      CmbModel.ItemIndex := MIdx
    else if CmbModel.Items.Count = 0 then
      CmbModel.Items.Add(AppSettings.ModelName);
  finally
    FIgnoreModelChange := False;
  end;
end;

procedure TFrmChatFull.PopulateModels(const AProviderName: string);
var
  LModels: TStringList;
begin
  FIgnoreModelChange := True;
  try
    CmbModel.Items.Clear;
    LModels := ProviderList.FetchModels(AProviderName, AppSettings.ApiKey[AProviderName]);
    try
      LModels.Sort;
      CmbModel.Items.Assign(LModels);
    finally
      LModels.Free;
    end;
    if CmbModel.Items.Count > 0 then CmbModel.ItemIndex := 0;
  finally
    FIgnoreModelChange := False;
  end;
end;

procedure TFrmChatFull.LoadConversationList;
begin
  LstConversations.Items.Clear;
  for var I := 0 to ConversationStore.Count - 1 do
    LstConversations.Items.Add(ConversationStore[I].Title);
end;

{ ── Toolbar ──────────────────────────────────────────────────────────────── }

procedure TFrmChatFull.BtnSidebar_Click(Sender: TObject);
begin
  FSidebarVisible := not FSidebarVisible;
  RectSidebar.Visible := FSidebarVisible;
end;

procedure TFrmChatFull.BtnNewChat_Click(Sender: TObject);
begin
  StartNewConversation;
end;

procedure TFrmChatFull.BtnTheme_Click(Sender: TObject);
begin
  AppSettings.ThemeDark := not AppSettings.ThemeDark;
  ChatView.ThemeDark    := AppSettings.ThemeDark;
  ChatInput.ThemeDark   := AppSettings.ThemeDark;
  UpdateThemeButton;
end;

procedure TFrmChatFull.UpdateThemeButton;
begin
  if AppSettings.ThemeDark then BtnTheme.Text := 'Light'
  else BtnTheme.Text := 'Dark';
end;

procedure TFrmChatFull.BtnSettings_Click(Sender: TObject);
begin
  var Frm := TFrmSettings.Create(Self);
  try
    if Frm.ShowModal = mrOk then
    begin
      InitProviderCombos;
      UpdateThemeButton;
      SyncBridgeSettings;
    end;
  finally
    Frm.Free;
  end;
end;

procedure TFrmChatFull.CmbProvider_Change(Sender: TObject);
begin
  if FIgnoreModelChange then Exit;
  var Idx := CmbProvider.ItemIndex;
  if Idx < 0 then Exit;
  var Prov := ProviderList.Item(Idx);
  AppSettings.ProviderName := Prov.Name;
  PopulateModels(Prov.Name);
  if CmbModel.Items.Count > 0 then
    AppSettings.ModelName := CmbModel.Items[CmbModel.ItemIndex];
  SyncBridgeSettings;
end;

procedure TFrmChatFull.CmbModel_Change(Sender: TObject);
begin
  if FIgnoreModelChange then Exit;
  if CmbModel.ItemIndex >= 0 then
  begin
    AppSettings.ModelName := CmbModel.Items[CmbModel.ItemIndex];
    SyncBridgeSettings;
  end;
end;

procedure TFrmChatFull.SyncBridgeSettings;
begin
  ChatBridge.ApplySettings;
  if Assigned(FCmbChatMode) then LoadParamsToUI;
end;

{ ── Conversation ─────────────────────────────────────────────────────────── }

procedure TFrmChatFull.BtnNewConv_Click(Sender: TObject);
begin
  StartNewConversation;
end;

function TFrmChatFull.NextConvTitle: string;
begin
  Result := 'Chat ' + FormatDateTime('dd/mm hh:nn', Now);
end;

procedure TFrmChatFull.StartNewConversation(const ATitle: string = '');
begin
  SaveCurrentConversation;
  var Title := ATitle;
  if Title = '' then Title := NextConvTitle;
  var Conv := ConversationStore.AddNew(Title);
  LoadConversationList;
  LstConversations.ItemIndex := 0;
  FCurrentConvIndex := 0;
  ChatBridge.CurrentConvId := Conv.Id;
  ChatBridge.NewChat;
  Caption := 'MakerAI Chat — All Functions — ' + Conv.Title;
end;

procedure TFrmChatFull.SelectConversation(AIndex: Integer);
var
  Conv: TConversation;
  Arr : TJSONValue;
  I   : Integer;
  Msg : TAIChatMessage;
begin
  if (AIndex < 0) or (AIndex >= ConversationStore.Count) then Exit;
  SaveCurrentConversation;
  FCurrentConvIndex          := AIndex;
  LstConversations.ItemIndex := AIndex;
  Conv := ConversationStore[AIndex];
  ChatBridge.CurrentConvId := Conv.Id;
  Caption := 'MakerAI Chat — All Functions — ' + Conv.Title;
  ChatBridge.NewChat;

  if Conv.MessagesJSON <> '' then
  begin
    Arr := TJSONValue.ParseJSONValue(Conv.MessagesJSON);
    if Arr is TJSONArray then
    try
      ChatView.LoadFromJSON(TJSONArray(Arr));
    finally
      Arr.Free;
    end
    else
      FreeAndNil(Arr);
  end
  else
  begin
    for I := 0 to Conv.Messages.Count - 1 do
    begin
      var M := Conv.Messages[I];
      if SameText(M.Role, 'user') then
        ChatView.AddUserMessage(M.Content)
      else if SameText(M.Role, 'assistant') then
        ChatView.AddMessage(TChatRole.crAssistant, M.Content);
    end;
  end;

  for I := 0 to ChatView.VirtualList.Messages.Count - 1 do
  begin
    Msg := ChatView.VirtualList.Messages[I];
    case Msg.Role of
      TChatRole.crUser     : ChatBridge.Connection.AddMessage(Msg.Text, 'user');
      TChatRole.crAssistant: ChatBridge.Connection.AddMessage(Msg.Text, 'assistant');
    end;
  end;

  ChatView.RefreshLayout;
  ChatView.ScrollToBottom;
end;

procedure TFrmChatFull.SaveCurrentConversation;
var
  Conv: TConversation;
  Arr : TJSONArray;
  Msgs: TAIChatMessages;
  I   : Integer;
  Msg : TAIChatMessage;
  T   : string;
begin
  if FCurrentConvIndex < 0 then Exit;
  if FCurrentConvIndex >= ConversationStore.Count then Exit;
  Conv := ConversationStore[FCurrentConvIndex];
  Arr := ChatView.SaveToJSON;
  try
    Conv.MessagesJSON := Arr.ToJSON;
  finally
    Arr.Free;
  end;
  Conv.Clear;
  Msgs := ChatView.VirtualList.Messages;
  for I := 0 to Msgs.Count - 1 do
  begin
    Msg := Msgs[I];
    case Msg.Role of
      TChatRole.crUser     : Conv.AddMessage('user',      Msg.Text);
      TChatRole.crAssistant: Conv.AddMessage('assistant', Msg.Text);
    end;
  end;
  if Conv.Title.StartsWith('Chat ') or Conv.Title.StartsWith('New Chat') then
    for I := 0 to Conv.Messages.Count - 1 do
      if SameText(Conv.Messages[I].Role, 'user') and (Conv.Messages[I].Content <> '') then
      begin
        T := Conv.Messages[I].Content;
        if Length(T) > 40 then T := Copy(T, 1, 40) + '…';
        Conv.Title := T;
        LoadConversationList;
        LstConversations.ItemIndex := FCurrentConvIndex;
        Break;
      end;
end;

procedure TFrmChatFull.LstConversations_ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  SelectConversation(LstConversations.ItemIndex);
end;

procedure TFrmChatFull.BtnDeleteConv_Click(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := LstConversations.ItemIndex;
  if AIndex < 0 then Exit;
  ConversationStore.Remove(AIndex);
  if FCurrentConvIndex = AIndex then
  begin
    FCurrentConvIndex := -1;
    ChatBridge.NewChat;
  end
  else if FCurrentConvIndex > AIndex then
    Dec(FCurrentConvIndex);
  LoadConversationList;
  if (FCurrentConvIndex < 0) and (ConversationStore.Count > 0) then
    SelectConversation(0);
end;

{ ── Chat events ──────────────────────────────────────────────────────────── }

procedure TFrmChatFull.ChatInput_Send(Sender: TObject; const APrompt: string;
  AAttachments: TAIChatAttachments; AAudio: TMemoryStream);
begin
  MemoThinking.Text    := '';
  MemoThinking.Visible := False;
  ChatBridge.SendMessage(APrompt, AAttachments, AAudio);
end;

procedure TFrmChatFull.ChatInput_Cancel(Sender: TObject);
begin
  ChatBridge.AbortGeneration;
end;

procedure TFrmChatFull.ChatInput_MicToggle(Sender: TObject; AActivate: Boolean);
begin
  if AActivate then ChatInput.NotifyVoiceCalibrated
  else ChatInput.NotifyVoiceError;
end;

procedure TFrmChatFull.ChatView_LinkClick(Sender: TObject; AMsg: TAIChatMessage;
  const AURL: string);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
  {$ELSE}
  ShowMessage('Open: ' + AURL);
  {$ENDIF}
end;

procedure TFrmChatFull.ChatView_AttachOpen(Sender: TObject;
  AMsg: TAIChatMessage; AAttach: TAIChatAttachment);
var
  LPath: string;
begin
  LPath := AAttach.LocalPath;
  if ((LPath = '') or not TFile.Exists(LPath)) and (Length(AAttach.Data) > 0) then
  begin
    LPath := TPath.Combine(TPath.GetTempPath,
      Format('aich_%s', [AAttach.FileName]));
    TFile.WriteAllBytes(LPath, AAttach.Data);
  end;
  if AAttach.FileKind = TChatFileKind.fkAudio then
  begin
    if FAudioPlayer.IsPlaying then begin FAudioPlayer.Stop; Exit; end;
    if TFile.Exists(LPath) and FAudioPlayer.Play(LPath, AAttach.MimeType) then Exit;
  end;
  {$IFDEF MSWINDOWS}
  if TFile.Exists(LPath) then
    ShellExecute(0, 'open', PChar(LPath), nil, nil, SW_SHOWNORMAL)
  else
    ShowMessage('Attachment not found: ' + AAttach.FileName);
  {$ELSE}
  ShowMessage('Attachment: ' + AAttach.FileName);
  {$ENDIF}
end;

{ ── Params panel ─────────────────────────────────────────────────────────── }

procedure TFrmChatFull.BuildParamsPanel;
const
  CapNames: array[0..15] of string = (
    'Image Input', 'Audio Input', 'Video Input', 'PDF Input',
    'Web Search', 'Reasoning (CoT)', 'Code Interpreter', 'Memory',
    'Text Editor', 'Computer Use', 'Shell',
    'Gen Image', 'Gen Audio (TTS)', 'Gen Video', 'Gen Report', 'Extract Code'
  );
var
  I  : Integer;
  Chk: TCheckBox;

  procedure Hdr(const AText: string);
  var Lbl: TLabel;
  begin
    Lbl := TLabel.Create(Self);
    Lbl.Parent := ScrParams;
    Lbl.Align  := TAlignLayout.Top;
    Lbl.Margins.Rect := RectF(0, 10, 0, 2);
    Lbl.Height := 20;
    Lbl.Text   := AText;
    Lbl.TextSettings.Font.Style := [TFontStyle.fsBold];
    Lbl.TextSettings.Font.Size  := 11;
    Lbl.WordWrap := False;
  end;

begin
  Hdr('Chat Mode');
  FCmbChatMode := TComboBox.Create(Self);
  FCmbChatMode.Parent := ScrParams; FCmbChatMode.Align := TAlignLayout.Top;
  FCmbChatMode.Height := 28; FCmbChatMode.Margins.Rect := RectF(0, 2, 0, 0);
  FCmbChatMode.Items.Add('Conversation');   FCmbChatMode.Items.Add('Image Generation');
  FCmbChatMode.Items.Add('Video Generation'); FCmbChatMode.Items.Add('Speech Generation');
  FCmbChatMode.Items.Add('Transcription');  FCmbChatMode.Items.Add('Web Search');
  FCmbChatMode.Items.Add('Report Generation'); FCmbChatMode.Items.Add('Smart Dispatch');
  FCmbChatMode.OnChange := OnParamChatModeChange;

  Hdr('Thinking Level');
  FCmbThinkingLevel := TComboBox.Create(Self);
  FCmbThinkingLevel.Parent := ScrParams; FCmbThinkingLevel.Align := TAlignLayout.Top;
  FCmbThinkingLevel.Height := 28; FCmbThinkingLevel.Margins.Rect := RectF(0, 2, 0, 0);
  FCmbThinkingLevel.Items.Add('Default'); FCmbThinkingLevel.Items.Add('Low');
  FCmbThinkingLevel.Items.Add('Medium');  FCmbThinkingLevel.Items.Add('High');
  FCmbThinkingLevel.OnChange := OnParamThinkingLevelChange;

  FChkToolActive := TCheckBox.Create(Self);
  FChkToolActive.Parent := ScrParams; FChkToolActive.Align := TAlignLayout.Top;
  FChkToolActive.Height := 26; FChkToolActive.Margins.Rect := RectF(0, 10, 0, 0);
  FChkToolActive.Text := 'Enable Tools';
  FChkToolActive.OnChange := OnParamToolActiveChange;

  Hdr('Response Format');
  FCmbRespFormat := TComboBox.Create(Self);
  FCmbRespFormat.Parent := ScrParams; FCmbRespFormat.Align := TAlignLayout.Top;
  FCmbRespFormat.Height := 28; FCmbRespFormat.Margins.Rect := RectF(0, 2, 0, 0);
  FCmbRespFormat.Items.Add('Text'); FCmbRespFormat.Items.Add('JSON');
  FCmbRespFormat.Items.Add('JSON Schema');
  FCmbRespFormat.OnChange := OnParamRespFormatChange;

  FPnlJsonSchema := TLayout.Create(Self);
  FPnlJsonSchema.Parent := ScrParams; FPnlJsonSchema.Align := TAlignLayout.Top;
  FPnlJsonSchema.Height := 80; FPnlJsonSchema.Margins.Rect := RectF(0, 2, 0, 0);
  FPnlJsonSchema.Visible := False;
  FMemoJsonSchema := TMemo.Create(Self);
  FMemoJsonSchema.Parent := FPnlJsonSchema; FMemoJsonSchema.Align := TAlignLayout.Client;
  FMemoJsonSchema.OnChangeTracking := OnParamJsonSchemaChange;

  Hdr('Temperature');
  FLblTempValue := TLabel.Create(Self);
  FLblTempValue.Parent := ScrParams; FLblTempValue.Align := TAlignLayout.Top;
  FLblTempValue.Height := 16; FLblTempValue.Text := '1.00';
  FLblTempValue.TextSettings.HorzAlign := TTextAlign.Trailing;
  FLblTempValue.TextSettings.Font.Size := 10;
  FTrkTemperature := TTrackBar.Create(Self);
  FTrkTemperature.Parent := ScrParams; FTrkTemperature.Align := TAlignLayout.Top;
  FTrkTemperature.Height := 28; FTrkTemperature.Min := 0; FTrkTemperature.Max := 2;
  FTrkTemperature.Value := 1;
  FTrkTemperature.OnChange := OnParamTemperatureChange;

  Hdr('Max Tokens');
  FEdtMaxTokens := TEdit.Create(Self);
  FEdtMaxTokens.Parent := ScrParams; FEdtMaxTokens.Align := TAlignLayout.Top;
  FEdtMaxTokens.Height := 28; FEdtMaxTokens.Margins.Rect := RectF(0, 2, 0, 0);
  FEdtMaxTokens.OnChangeTracking := OnParamMaxTokensChange;

  Hdr('Top P');
  FLblTopPValue := TLabel.Create(Self);
  FLblTopPValue.Parent := ScrParams; FLblTopPValue.Align := TAlignLayout.Top;
  FLblTopPValue.Height := 16; FLblTopPValue.Text := '1.00';
  FLblTopPValue.TextSettings.HorzAlign := TTextAlign.Trailing;
  FLblTopPValue.TextSettings.Font.Size := 10;
  FTrkTopP := TTrackBar.Create(Self);
  FTrkTopP.Parent := ScrParams; FTrkTopP.Align := TAlignLayout.Top;
  FTrkTopP.Height := 28; FTrkTopP.Min := 0; FTrkTopP.Max := 1; FTrkTopP.Value := 1;
  FTrkTopP.OnChange := OnParamTopPChange;

  Hdr('Model Capabilities');
  for I := 0 to 15 do
  begin
    Chk := TCheckBox.Create(Self);
    Chk.Parent := ScrParams; Chk.Align := TAlignLayout.Top;
    Chk.Height := 22; Chk.Text := CapNames[I]; Chk.Tag := I;
    Chk.OnChange := OnParamModelCapChange;
    FChkModelCaps[I] := Chk;
  end;

  Hdr('Session Capabilities');
  for I := 0 to 15 do
  begin
    Chk := TCheckBox.Create(Self);
    Chk.Parent := ScrParams; Chk.Align := TAlignLayout.Top;
    Chk.Height := 22; Chk.Text := CapNames[I]; Chk.Tag := I;
    Chk.OnChange := OnParamSessionCapChange;
    FChkSessionCaps[I] := Chk;
  end;
end;

procedure TFrmChatFull.LoadParamsToUI;
var
  I: Integer;
begin
  FIgnoreParams := True;
  try
    FCmbChatMode.ItemIndex      := Ord(ChatBridge.Connection.ChatMode);
    FCmbThinkingLevel.ItemIndex := Ord(ChatBridge.Connection.ModelConfig.ThinkingLevel);
    FChkToolActive.IsChecked    := ChatBridge.Connection.ModelConfig.Tool_Active;
    FCmbRespFormat.ItemIndex    := Ord(ChatBridge.Connection.AiChat.Response_format);
    FPnlJsonSchema.Visible      := ChatBridge.Connection.AiChat.Response_format =
      TAiChatResponseFormat.tiaChatRfJsonSchema;
    FMemoJsonSchema.Text        := ChatBridge.Connection.AiChat.JsonSchema.Text;
    FTrkTemperature.Value       := ChatBridge.Connection.AiChat.Temperature;
    FLblTempValue.Text          := Format('%.2f', [ChatBridge.Connection.AiChat.Temperature]);
    FEdtMaxTokens.Text          := IntToStr(ChatBridge.Connection.AiChat.Max_tokens);
    FTrkTopP.Value              := ChatBridge.Connection.AiChat.Top_p;
    FLblTopPValue.Text          := Format('%.2f', [ChatBridge.Connection.AiChat.Top_p]);
    for I := 0 to 15 do
    begin
      FChkModelCaps[I].IsChecked   := TAiCapability(I) in ChatBridge.Connection.ModelConfig.ModelCaps;
      FChkSessionCaps[I].IsChecked := TAiCapability(I) in ChatBridge.Connection.ModelConfig.SessionCaps;
    end;
  finally
    FIgnoreParams := False;
  end;
end;

procedure TFrmChatFull.SyncModelConfig;
begin
  ChatBridge.Connection.AiChat.ModelConfig.Assign(ChatBridge.Connection.ModelConfig);
end;

procedure TFrmChatFull.OnParamChatModeChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.ChatMode := TAiChatMode(FCmbChatMode.ItemIndex);
end;

procedure TFrmChatFull.OnParamThinkingLevelChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.ModelConfig.ThinkingLevel := TAiThinkingLevel(FCmbThinkingLevel.ItemIndex);
  SyncModelConfig;
end;

procedure TFrmChatFull.OnParamToolActiveChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.ModelConfig.Tool_Active := FChkToolActive.IsChecked;
  SyncModelConfig;
end;

procedure TFrmChatFull.OnParamRespFormatChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.AiChat.Response_format := TAiChatResponseFormat(FCmbRespFormat.ItemIndex);
  FPnlJsonSchema.Visible := FCmbRespFormat.ItemIndex = 2;
end;

procedure TFrmChatFull.OnParamJsonSchemaChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.AiChat.JsonSchema.Text := FMemoJsonSchema.Text;
end;

procedure TFrmChatFull.OnParamTemperatureChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.AiChat.Temperature := SimpleRoundTo(FTrkTemperature.Value, -2);
  FLblTempValue.Text := Format('%.2f', [FTrkTemperature.Value]);
end;

procedure TFrmChatFull.OnParamMaxTokensChange(Sender: TObject);
var V: Integer;
begin
  if FIgnoreParams then Exit;
  if TryStrToInt(FEdtMaxTokens.Text, V) and (V > 0) then
    ChatBridge.Connection.AiChat.Max_tokens := V;
end;

procedure TFrmChatFull.OnParamTopPChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.AiChat.Top_p := SimpleRoundTo(FTrkTopP.Value, -2);
  FLblTopPValue.Text := Format('%.2f', [FTrkTopP.Value]);
end;

procedure TFrmChatFull.OnParamModelCapChange(Sender: TObject);
var Caps: TAiCapabilities; I: Integer;
begin
  if FIgnoreParams then Exit;
  Caps := [];
  for I := 0 to 15 do
    if FChkModelCaps[I].IsChecked then Include(Caps, TAiCapability(I));
  ChatBridge.Connection.ModelConfig.ModelCaps := Caps;
  SyncModelConfig;
end;

procedure TFrmChatFull.OnParamSessionCapChange(Sender: TObject);
var Caps: TAiCapabilities; I: Integer;
begin
  if FIgnoreParams then Exit;
  Caps := [];
  for I := 0 to 15 do
    if FChkSessionCaps[I].IsChecked then Include(Caps, TAiCapability(I));
  ChatBridge.Connection.ModelConfig.SessionCaps := Caps;
  SyncModelConfig;
end;

{ ── Tools panel ──────────────────────────────────────────────────────────── }

procedure TFrmChatFull.BuildToolsPanel;

  procedure Hdr(const AText: string);
  var Lbl: TLabel;
  begin
    Lbl := TLabel.Create(Self);
    Lbl.Parent := ScrTools; Lbl.Align := TAlignLayout.Top;
    Lbl.Margins.Rect := RectF(0, 10, 0, 2);
    Lbl.Height := 20; Lbl.Text := AText;
    Lbl.TextSettings.Font.Style := [TFontStyle.fsBold];
    Lbl.TextSettings.Font.Size  := 11;
    Lbl.WordWrap := False;
  end;

begin
  Hdr('Agent Tools');

  FChkShell := TCheckBox.Create(Self);
  FChkShell.Parent := ScrTools; FChkShell.Align := TAlignLayout.Top;
  FChkShell.Height := 26; FChkShell.Text := 'Shell (bash)';
  FChkShell.OnChange := OnChkShellChange;

  FChkEditor := TCheckBox.Create(Self);
  FChkEditor.Parent := ScrTools; FChkEditor.Align := TAlignLayout.Top;
  FChkEditor.Height := 26; FChkEditor.Text := 'Text Editor (str_replace_editor)';
  FChkEditor.OnChange := OnChkEditorChange;

  FChkComputerUse := TCheckBox.Create(Self);
  FChkComputerUse.Parent := ScrTools; FChkComputerUse.Align := TAlignLayout.Top;
  FChkComputerUse.Height := 26; FChkComputerUse.Text := 'Computer Use';
  FChkComputerUse.OnChange := OnChkComputerUseChange;

  Hdr('MCP Servers');

  FBtnLoadMCP := TButton.Create(Self);
  FBtnLoadMCP.Parent := ScrTools; FBtnLoadMCP.Align := TAlignLayout.Top;
  FBtnLoadMCP.Height := 32; FBtnLoadMCP.Margins.Rect := RectF(0, 4, 0, 8);
  FBtnLoadMCP.Text := 'Load MCP Config…';
  FBtnLoadMCP.OnClick := OnBtnLoadMCPClick;

  Hdr('Tool Log');

  FMemoToolLog := TMemo.Create(Self);
  FMemoToolLog.Parent := ScrTools; FMemoToolLog.Align := TAlignLayout.Top;
  FMemoToolLog.Height := 200;
  FMemoToolLog.Margins.Rect := RectF(0, 2, 0, 0);
  FMemoToolLog.ReadOnly := True;
  FMemoToolLog.TextSettings.Font.Size := 10;
  FMemoToolLog.WordWrap := True;
end;

procedure TFrmChatFull.OnChkShellChange(Sender: TObject);
begin
  ChatBridge.ShellEnabled := FChkShell.IsChecked;
end;

procedure TFrmChatFull.OnChkEditorChange(Sender: TObject);
begin
  ChatBridge.EditorEnabled := FChkEditor.IsChecked;
end;

procedure TFrmChatFull.OnChkComputerUseChange(Sender: TObject);
begin
  ChatBridge.ComputerUseEnabled := FChkComputerUse.IsChecked;
end;

procedure TFrmChatFull.OnBtnLoadMCPClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Title := 'Select MCP Configuration File';
    Dlg.Filter := 'MCP Config (*.mcpconf;*.json)|*.mcpconf;*.json|All Files (*.*)|*.*';
    if Dlg.Execute then
      ChatBridge.LoadMCPConfig(Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

procedure TFrmChatFull.OnBridgeToolLog(const AText: string);
begin
  if not Assigned(FMemoToolLog) then Exit;
  FMemoToolLog.Lines.Add(AText);
  FMemoToolLog.GoToTextEnd;
  // Auto-switch to Tools tab so user sees the log
  TabSidebar.TabIndex := 2;
end;

{ ── Stats panel ──────────────────────────────────────────────────────────── }

procedure TFrmChatFull.BuildStatsPanel;

  procedure Hdr(const AText: string);
  var Lbl: TLabel;
  begin
    Lbl := TLabel.Create(Self);
    Lbl.Parent := ScrStats; Lbl.Align := TAlignLayout.Top;
    Lbl.Margins.Rect := RectF(0, 10, 0, 2);
    Lbl.Height := 20; Lbl.Text := AText;
    Lbl.TextSettings.Font.Style := [TFontStyle.fsBold];
    Lbl.TextSettings.Font.Size  := 11;
    Lbl.WordWrap := False;
  end;

  function AddStatRow(const ALabel: string): TEdit;
  var
    Lbl: TLabel;
    Edt: TEdit;
  begin
    Lbl := TLabel.Create(Self);
    Lbl.Parent := ScrStats; Lbl.Align := TAlignLayout.Top;
    Lbl.Height := 18; Lbl.Text := ALabel;
    Lbl.TextSettings.Font.Size := 10;
    Edt := TEdit.Create(Self);
    Edt.Parent := ScrStats; Edt.Align := TAlignLayout.Top;
    Edt.Height := 28; Edt.Margins.Rect := RectF(0, 2, 0, 6);
    Edt.ReadOnly := True;
    Edt.Text := '0';
    Result := Edt;
  end;

begin
  Hdr('Token Usage (last message)');
  FEdtTotalTokens      := AddStatRow('Total tokens');
  FEdtPromptTokens     := AddStatRow('Prompt tokens');
  FEdtCompletionTokens := AddStatRow('Completion tokens');
  FEdtThinkingTokens   := AddStatRow('Thinking tokens');
end;

procedure TFrmChatFull.OnBridgeStatsUpdate(ATotal, APrompt, ACompletion, AThinking: Integer);
begin
  if not Assigned(FEdtTotalTokens) then Exit;
  FEdtTotalTokens.Text      := IntToStr(ATotal);
  FEdtPromptTokens.Text     := IntToStr(APrompt);
  FEdtCompletionTokens.Text := IntToStr(ACompletion);
  FEdtThinkingTokens.Text   := IntToStr(AThinking);
end;

end.
