unit uFrm.Main;

// =============================================================================
// MakerAI Chat — Main Form
//
// Phase 1 features:
//   - Multi-provider selection (Claude, OpenAI, Gemini, Groq, Ollama)
//   - Real streaming via TAiChatConnection (Asynchronous=True)
//   - Conversation list with persistence (TConversationStore)
//   - Settings form (API keys, system prompt, dark/light theme)
//   - TAiFunctions: get_datetime, calculate, get_weather (simulated)
//   - TAiPrompts: system prompt templates
// =============================================================================

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Edit, FMX.Memo,
  FMX.TabControl, FMX.Controls.Presentation,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellAPI,
  {$ENDIF}
  AIChat.Types,
  AIChat.Control.FMX,
  AIChat.Input, System.Skia, FMX.Skia,
  uAudio.Player, FMX.Memo.Types, FMX.ScrollBox;

type
  TFrmMain = class(TForm)
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
    ScrParams         : TVertScrollBox;
    LayoutSidebarInner: TLayout;
    LblConversations  : TLabel;
    BtnNewConv        : TButton;
    LstConversations  : TListBox;
    BtnDeleteConv     : TButton;
    LayoutChat        : TLayout;
    ChatView          : TAIChatView;
    ChatInput         : TAIChatInput;
    MemoThinking      : TMemo;
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

    // parameter panel controls
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

    procedure InitProviderCombos;
    procedure LoadConversationList;
    procedure PopulateModels(const AProviderName: string);
    procedure SelectConversation(AIndex: Integer);
    procedure SaveCurrentConversation;
    procedure StartNewConversation(const ATitle: string = '');
    procedure UpdateThemeButton;
    procedure SyncBridgeSettings;
    function  NextConvTitle: string;

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
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.fmx}

uses
  System.JSON, System.IOUtils, System.Math, System.TypInfo,
  uApp.Settings,
  uApp.ProviderList,
  uApp.Conversation,
  uApp.ChatBridge,
  uFrm.Settings,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Core,
  UMakerAi.Chat;

var
  ChatBridge: TChatBridge;

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FCurrentConvIndex := -1;
  FSidebarVisible   := True;

  FAudioPlayer := TAudioPlayer.Create;
  ProviderList := TProviderList.Create;
  ChatBridge   := TChatBridge.Create(ChatView, ChatInput);

  ChatBridge.OnThinking := procedure(AChunk: string)
  begin
    if not MemoThinking.Visible then
      MemoThinking.Visible := True;
    MemoThinking.Text := MemoThinking.Text + AChunk;
    MemoThinking.GoToTextEnd;
  end;

  InitProviderCombos;
  LoadConversationList;
  UpdateThemeButton;
  BuildParamsPanel;
  LoadParamsToUI;

  if ConversationStore.Count = 0 then
    StartNewConversation('New Chat')
  else
    SelectConversation(0);
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FAudioPlayer) then FAudioPlayer.Stop; // stop before any teardown
  SaveCurrentConversation;
  ConversationStore.Save;
  AppSettings.Save;
  FreeAndNil(ChatBridge);
  FreeAndNil(ProviderList);
  FreeAndNil(FAudioPlayer);
end;

{ ── Initialization ─────────────────────────────────────────────────────────── }

procedure TFrmMain.InitProviderCombos;
begin
  FIgnoreModelChange := True;
  try
    CmbProvider.Items.Clear;
    for var I := 0 to ProviderList.Count - 1 do
      CmbProvider.Items.Add(ProviderList.Item(I).DisplayName);

    var Idx := ProviderList.IndexOf(AppSettings.ProviderName);
    if Idx >= 0 then CmbProvider.ItemIndex := Idx else CmbProvider.ItemIndex := 0;

    PopulateModels(AppSettings.ProviderName);

    // Restore saved model; if GetModels returned nothing (no key yet) keep saved name visible
    var MIdx := CmbModel.Items.IndexOf(AppSettings.ModelName);
    if MIdx >= 0 then
      CmbModel.ItemIndex := MIdx
    else if CmbModel.Items.Count = 0 then
      CmbModel.Items.Add(AppSettings.ModelName);
  finally
    FIgnoreModelChange := False;
  end;
end;

procedure TFrmMain.PopulateModels(const AProviderName: string);
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
    if CmbModel.Items.Count > 0 then
      CmbModel.ItemIndex := 0;
  finally
    FIgnoreModelChange := False;
  end;
end;

procedure TFrmMain.LoadConversationList;
begin
  LstConversations.Items.Clear;
  for var I := 0 to ConversationStore.Count - 1 do
    LstConversations.Items.Add(ConversationStore[I].Title);
end;

{ ── Toolbar ─────────────────────────────────────────────────────────────────── }

procedure TFrmMain.BtnSidebar_Click(Sender: TObject);
begin
  FSidebarVisible := not FSidebarVisible;
  RectSidebar.Visible := FSidebarVisible;
end;

procedure TFrmMain.BtnNewChat_Click(Sender: TObject);
begin
  StartNewConversation;
end;

procedure TFrmMain.BtnTheme_Click(Sender: TObject);
begin
  AppSettings.ThemeDark := not AppSettings.ThemeDark;
  ChatView.ThemeDark    := AppSettings.ThemeDark;
  ChatInput.ThemeDark   := AppSettings.ThemeDark;
  UpdateThemeButton;
end;

procedure TFrmMain.UpdateThemeButton;
begin
  if AppSettings.ThemeDark then BtnTheme.Text := 'Light'
  else BtnTheme.Text := 'Dark';
end;

procedure TFrmMain.BtnSettings_Click(Sender: TObject);
begin
  var Frm := TFrmSettings.Create(Self);
  try
    if Frm.ShowModal = mrOk then
    begin
      // Re-init combos in case provider/model changed
      InitProviderCombos;
      UpdateThemeButton;
      SyncBridgeSettings;
    end;
  finally
    Frm.Free;
  end;
end;

procedure TFrmMain.CmbProvider_Change(Sender: TObject);
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

procedure TFrmMain.CmbModel_Change(Sender: TObject);
begin
  if FIgnoreModelChange then Exit;
  if CmbModel.ItemIndex >= 0 then
  begin
    AppSettings.ModelName := CmbModel.Items[CmbModel.ItemIndex];
    SyncBridgeSettings;
  end;
end;

procedure TFrmMain.SyncBridgeSettings;
begin
  ChatBridge.ApplySettings;
  // Refresh param panel to reflect defaults loaded by the new driver/model
  if Assigned(FCmbChatMode) then
    LoadParamsToUI;
end;

{ ── Conversation sidebar ─────────────────────────────────────────────────────── }

procedure TFrmMain.BtnNewConv_Click(Sender: TObject);
begin
  StartNewConversation;
end;

function TFrmMain.NextConvTitle: string;
begin
  Result := 'Chat ' + FormatDateTime('dd/mm hh:nn', Now);
end;

procedure TFrmMain.StartNewConversation(const ATitle: string = '');
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
  Caption := 'MakerAI Chat — ' + Conv.Title;
end;

procedure TFrmMain.SelectConversation(AIndex: Integer);
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
  Caption := 'MakerAI Chat — ' + Conv.Title;

  ChatBridge.NewChat;

  if Conv.MessagesJSON <> '' then
  begin
    // New format: restore visual state including attachments via component API
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
    // Fallback for old-format conversations (text only, no messagesJson key)
    for I := 0 to Conv.Messages.Count - 1 do
    begin
      var M := Conv.Messages[I];
      if SameText(M.Role, 'user') then
        ChatView.AddUserMessage(M.Content)
      else if SameText(M.Role, 'assistant') then
        ChatView.AddMessage(TChatRole.crAssistant, M.Content);
    end;
  end;

  // Rebuild AI connection history from the restored visual messages
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

procedure TFrmMain.SaveCurrentConversation;
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

  // Save full visual state — text + base64 images (if ChatView.PersistAttachments=True)
  Arr := ChatView.SaveToJSON;
  try
    Conv.MessagesJSON := Arr.ToJSON;
  finally
    Arr.Free;
  end;

  // Also save text-only messages for AI connection-history rebuild on load
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

  // Auto-title from first non-empty user message
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

procedure TFrmMain.LstConversations_ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  SelectConversation(LstConversations.ItemIndex);
end;

procedure TFrmMain.BtnDeleteConv_Click(Sender: TObject);
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

{ ── Chat events ─────────────────────────────────────────────────────────────── }

procedure TFrmMain.ChatInput_Send(Sender: TObject; const APrompt: string;
  AAttachments: TAIChatAttachments; AAudio: TMemoryStream);
begin
  MemoThinking.Text    := '';
  MemoThinking.Visible := False;
  ChatBridge.SendMessage(APrompt, AAttachments, AAudio);
end;

procedure TFrmMain.ChatInput_Cancel(Sender: TObject);
begin
  ChatBridge.AbortGeneration;
end;

procedure TFrmMain.ChatInput_MicToggle(Sender: TObject; AActivate: Boolean);
begin
  if AActivate then ChatInput.NotifyVoiceCalibrated
  else ChatInput.NotifyVoiceError;
end;

procedure TFrmMain.ChatView_LinkClick(Sender: TObject; AMsg: TAIChatMessage;
  const AURL: string);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
  {$ELSE}
  ShowMessage('Open: ' + AURL);
  {$ENDIF}
end;

procedure TFrmMain.ChatView_AttachOpen(Sender: TObject; AMsg: TAIChatMessage;
  AAttach: TAIChatAttachment);
var
  LPath: string;
begin
  LPath := AAttach.LocalPath;

  // Original temp file may have been deleted after send; restore from stored bytes
  if ((LPath = '') or not TFile.Exists(LPath)) and (Length(AAttach.Data) > 0) then
  begin
    LPath := TPath.Combine(TPath.GetTempPath,
      Format('aich_play_%s', [AAttach.FileName]));
    TFile.WriteAllBytes(LPath, AAttach.Data);
  end;

  if AAttach.FileKind = TChatFileKind.fkAudio then
  begin
    if FAudioPlayer.IsPlaying then
    begin
      FAudioPlayer.Stop;
      Exit;
    end;
    FAudioPlayer.Stop;
    if TFile.Exists(LPath) and FAudioPlayer.Play(LPath, AAttach.MimeType) then
      Exit;
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

{ ── Parameters panel ─────────────────────────────────────────────────────────── }

function ParseCapSet(const AStr: string): TAiCapabilities;
var
  S    : string;
  Parts: TArray<string>;
  Part : string;
  V    : Integer;
begin
  Result := [];
  S := AStr.Replace('[', '').Replace(']', '').Trim;
  if S = '' then Exit;
  Parts := S.Split([',']);
  for Part in Parts do
  begin
    V := GetEnumValue(TypeInfo(TAiCapability), Part.Trim);
    if (V >= 0) and (V <= 15) then
      Include(Result, TAiCapability(V));
  end;
end;

procedure TFrmMain.BuildParamsPanel;
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
  // Chat Mode
  Hdr('Chat Mode');
  FCmbChatMode := TComboBox.Create(Self);
  FCmbChatMode.Parent := ScrParams;
  FCmbChatMode.Align  := TAlignLayout.Top;
  FCmbChatMode.Height := 28;
  FCmbChatMode.Margins.Rect := RectF(0, 2, 0, 0);
  FCmbChatMode.Items.Add('Conversation');
  FCmbChatMode.Items.Add('Image Generation');
  FCmbChatMode.Items.Add('Video Generation');
  FCmbChatMode.Items.Add('Speech Generation');
  FCmbChatMode.Items.Add('Transcription');
  FCmbChatMode.Items.Add('Web Search');
  FCmbChatMode.Items.Add('Report Generation');
  FCmbChatMode.Items.Add('Smart Dispatch');
  FCmbChatMode.OnChange := OnParamChatModeChange;

  // Thinking Level
  Hdr('Thinking Level');
  FCmbThinkingLevel := TComboBox.Create(Self);
  FCmbThinkingLevel.Parent := ScrParams;
  FCmbThinkingLevel.Align  := TAlignLayout.Top;
  FCmbThinkingLevel.Height := 28;
  FCmbThinkingLevel.Margins.Rect := RectF(0, 2, 0, 0);
  FCmbThinkingLevel.Items.Add('Default');
  FCmbThinkingLevel.Items.Add('Low');
  FCmbThinkingLevel.Items.Add('Medium');
  FCmbThinkingLevel.Items.Add('High');
  FCmbThinkingLevel.OnChange := OnParamThinkingLevelChange;

  // Tools
  FChkToolActive := TCheckBox.Create(Self);
  FChkToolActive.Parent := ScrParams;
  FChkToolActive.Align  := TAlignLayout.Top;
  FChkToolActive.Height := 26;
  FChkToolActive.Margins.Rect := RectF(0, 10, 0, 0);
  FChkToolActive.Text := 'Enable Tools';
  FChkToolActive.OnChange := OnParamToolActiveChange;

  // Response Format
  Hdr('Response Format');
  FCmbRespFormat := TComboBox.Create(Self);
  FCmbRespFormat.Parent := ScrParams;
  FCmbRespFormat.Align  := TAlignLayout.Top;
  FCmbRespFormat.Height := 28;
  FCmbRespFormat.Margins.Rect := RectF(0, 2, 0, 0);
  FCmbRespFormat.Items.Add('Text');
  FCmbRespFormat.Items.Add('JSON');
  FCmbRespFormat.Items.Add('JSON Schema');
  FCmbRespFormat.OnChange := OnParamRespFormatChange;

  // JSON Schema memo (hidden until JSON Schema format selected)
  FPnlJsonSchema := TLayout.Create(Self);
  FPnlJsonSchema.Parent  := ScrParams;
  FPnlJsonSchema.Align   := TAlignLayout.Top;
  FPnlJsonSchema.Height  := 80;
  FPnlJsonSchema.Margins.Rect := RectF(0, 2, 0, 0);
  FPnlJsonSchema.Visible := False;
  FMemoJsonSchema := TMemo.Create(Self);
  FMemoJsonSchema.Parent := FPnlJsonSchema;
  FMemoJsonSchema.Align  := TAlignLayout.Client;
  FMemoJsonSchema.OnChangeTracking := OnParamJsonSchemaChange;

  // Temperature
  Hdr('Temperature');
  FLblTempValue := TLabel.Create(Self);
  FLblTempValue.Parent := ScrParams;
  FLblTempValue.Align  := TAlignLayout.Top;
  FLblTempValue.Height := 16;
  FLblTempValue.Text   := '1.00';
  FLblTempValue.TextSettings.HorzAlign := TTextAlign.Trailing;
  FLblTempValue.TextSettings.Font.Size := 10;
  FTrkTemperature := TTrackBar.Create(Self);
  FTrkTemperature.Parent := ScrParams;
  FTrkTemperature.Align  := TAlignLayout.Top;
  FTrkTemperature.Height := 28;
  FTrkTemperature.Min    := 0;
  FTrkTemperature.Max    := 2;
  FTrkTemperature.Value  := 1;
  FTrkTemperature.OnChange := OnParamTemperatureChange;

  // Max Tokens
  Hdr('Max Tokens');
  FEdtMaxTokens := TEdit.Create(Self);
  FEdtMaxTokens.Parent := ScrParams;
  FEdtMaxTokens.Align  := TAlignLayout.Top;
  FEdtMaxTokens.Height := 28;
  FEdtMaxTokens.Margins.Rect := RectF(0, 2, 0, 0);
  FEdtMaxTokens.OnChangeTracking := OnParamMaxTokensChange;

  // Top P
  Hdr('Top P');
  FLblTopPValue := TLabel.Create(Self);
  FLblTopPValue.Parent := ScrParams;
  FLblTopPValue.Align  := TAlignLayout.Top;
  FLblTopPValue.Height := 16;
  FLblTopPValue.Text   := '1.00';
  FLblTopPValue.TextSettings.HorzAlign := TTextAlign.Trailing;
  FLblTopPValue.TextSettings.Font.Size := 10;
  FTrkTopP := TTrackBar.Create(Self);
  FTrkTopP.Parent := ScrParams;
  FTrkTopP.Align  := TAlignLayout.Top;
  FTrkTopP.Height := 28;
  FTrkTopP.Min    := 0;
  FTrkTopP.Max    := 1;
  FTrkTopP.Value  := 1;
  FTrkTopP.OnChange := OnParamTopPChange;

  // Model Capabilities
  Hdr('Model Capabilities');
  for I := 0 to 15 do
  begin
    Chk := TCheckBox.Create(Self);
    Chk.Parent   := ScrParams;
    Chk.Align    := TAlignLayout.Top;
    Chk.Height   := 22;
    Chk.Text     := CapNames[I];
    Chk.Tag      := I;
    Chk.OnChange := OnParamModelCapChange;
    FChkModelCaps[I] := Chk;
  end;

  // Session Capabilities
  Hdr('Session Capabilities');
  for I := 0 to 15 do
  begin
    Chk := TCheckBox.Create(Self);
    Chk.Parent   := ScrParams;
    Chk.Align    := TAlignLayout.Top;
    Chk.Height   := 22;
    Chk.Text     := CapNames[I];
    Chk.Tag      := I;
    Chk.OnChange := OnParamSessionCapChange;
    FChkSessionCaps[I] := Chk;
  end;
end;

procedure TFrmMain.LoadParamsToUI;
var
  P   : TStrings;
  S   : string;
  V   : Integer;
  D   : Double;
  I   : Integer;
  Caps: TAiCapabilities;
  FS  : TFormatSettings;
begin
  P  := ChatBridge.Connection.Params;
  FS := TFormatSettings.Invariant;
  FIgnoreParams := True;
  try
    // Temperature
    S := P.Values['Temperature'];
    if (S <> '') and TryStrToFloat(S, D, FS) then
    begin
      D := EnsureRange(D, 0, 2);
      FTrkTemperature.Value := D;
      FLblTempValue.Text    := Format('%.2f', [D]);
    end;

    // Max Tokens (drivers differ: Max_Tokens vs MaxTokens)
    S := P.Values['Max_Tokens'];
    if S = '' then S := P.Values['MaxTokens'];
    if (S <> '') and TryStrToInt(S, V) and (V > 0) then
      FEdtMaxTokens.Text := IntToStr(V);

    // Top P
    S := P.Values['Top_p'];
    if S = '' then S := P.Values['Top_P'];
    if (S <> '') and TryStrToFloat(S, D, FS) then
    begin
      D := EnsureRange(D, 0, 1);
      FTrkTopP.Value     := D;
      FLblTopPValue.Text := Format('%.2f', [D]);
    end;

    // Tool_Active
    S := P.Values['Tool_Active'];
    if S <> '' then
    begin
      var B := SameText(S, 'True') or SameText(S, '1') or SameText(S, 'Yes');
      FChkToolActive.IsChecked := B;
      ChatBridge.Connection.ModelConfig.Tool_Active := B;
    end;

    // ThinkingLevel — enum name stored in Params (e.g. "tlDefault", "tlMedium")
    S := P.Values['ThinkingLevel'];
    if S <> '' then
    begin
      V := GetEnumValue(TypeInfo(TAiThinkingLevel), S);
      if V >= 0 then
      begin
        FCmbThinkingLevel.ItemIndex := V;
        ChatBridge.Connection.ModelConfig.ThinkingLevel := TAiThinkingLevel(V);
      end;
    end;

    // ChatMode — enum name stored in Params
    S := P.Values['ChatMode'];
    if S <> '' then
    begin
      V := GetEnumValue(TypeInfo(TAiChatMode), S);
      if V >= 0 then
      begin
        FCmbChatMode.ItemIndex := V;
        ChatBridge.Connection.ChatMode := TAiChatMode(V);
      end;
    end;

    // ModelCaps — format: "[cap_Image, cap_Audio, ...]"
    S := P.Values['ModelCaps'];
    if S <> '' then
    begin
      Caps := ParseCapSet(S);
      for I := 0 to 15 do
        FChkModelCaps[I].IsChecked := TAiCapability(I) in Caps;
      ChatBridge.Connection.ModelConfig.ModelCaps := Caps;
    end;

    // SessionCaps
    S := P.Values['SessionCaps'];
    if S <> '' then
    begin
      Caps := ParseCapSet(S);
      for I := 0 to 15 do
        FChkSessionCaps[I].IsChecked := TAiCapability(I) in Caps;
      ChatBridge.Connection.ModelConfig.SessionCaps := Caps;
    end;

    // ResponseFormat / JsonSchema — not stored in Params, read from AiChat
    FCmbRespFormat.ItemIndex := Ord(ChatBridge.Connection.AiChat.Response_format);
    FPnlJsonSchema.Visible   := ChatBridge.Connection.AiChat.Response_format = TAiChatResponseFormat.tiaChatRfJsonSchema;
    FMemoJsonSchema.Text     := ChatBridge.Connection.AiChat.JsonSchema.Text;

  finally
    FIgnoreParams := False;
  end;
  // Keep Connection.ModelConfig and AiChat.ModelConfig in sync
  SyncModelConfig;
end;

procedure TFrmMain.SyncModelConfig;
begin
  ChatBridge.Connection.AiChat.ModelConfig.Assign(ChatBridge.Connection.ModelConfig);
end;

procedure TFrmMain.OnParamChatModeChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.ChatMode := TAiChatMode(FCmbChatMode.ItemIndex);
end;

procedure TFrmMain.OnParamThinkingLevelChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.ModelConfig.ThinkingLevel := TAiThinkingLevel(FCmbThinkingLevel.ItemIndex);
  SyncModelConfig;
end;

procedure TFrmMain.OnParamToolActiveChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.ModelConfig.Tool_Active := FChkToolActive.IsChecked;
  SyncModelConfig;
end;

procedure TFrmMain.OnParamRespFormatChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.AiChat.Response_format := TAiChatResponseFormat(FCmbRespFormat.ItemIndex);
  FPnlJsonSchema.Visible := FCmbRespFormat.ItemIndex = 2;
end;

procedure TFrmMain.OnParamJsonSchemaChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.AiChat.JsonSchema.Text := FMemoJsonSchema.Text;
end;

procedure TFrmMain.OnParamTemperatureChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.AiChat.Temperature := SimpleRoundTo(FTrkTemperature.Value, -2);
  FLblTempValue.Text := Format('%.2f', [FTrkTemperature.Value]);
end;

procedure TFrmMain.OnParamMaxTokensChange(Sender: TObject);
var
  V: Integer;
begin
  if FIgnoreParams then Exit;
  if TryStrToInt(FEdtMaxTokens.Text, V) and (V > 0) then
    ChatBridge.Connection.AiChat.Max_tokens := V;
end;

procedure TFrmMain.OnParamTopPChange(Sender: TObject);
begin
  if FIgnoreParams then Exit;
  ChatBridge.Connection.AiChat.Top_p := SimpleRoundTo(FTrkTopP.Value, -2);
  FLblTopPValue.Text := Format('%.2f', [FTrkTopP.Value]);
end;

procedure TFrmMain.OnParamModelCapChange(Sender: TObject);
var
  Caps: TAiCapabilities;
  I   : Integer;
begin
  if FIgnoreParams then Exit;
  Caps := [];
  for I := 0 to 15 do
    if FChkModelCaps[I].IsChecked then
      Include(Caps, TAiCapability(I));
  ChatBridge.Connection.ModelConfig.ModelCaps := Caps;
  SyncModelConfig;
end;

procedure TFrmMain.OnParamSessionCapChange(Sender: TObject);
var
  Caps: TAiCapabilities;
  I   : Integer;
begin
  if FIgnoreParams then Exit;
  Caps := [];
  for I := 0 to 15 do
    if FChkSessionCaps[I].IsChecked then
      Include(Caps, TAiCapability(I));
  ChatBridge.Connection.ModelConfig.SessionCaps := Caps;
  SyncModelConfig;
end;

end.
