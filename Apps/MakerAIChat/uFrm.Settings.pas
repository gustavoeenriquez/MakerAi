unit uFrm.Settings;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.TabControl, FMX.Edit, FMX.Memo,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox;

type
  TFrmSettings = class(TForm)
    ToolBar1       : TToolBar;
    LblTitle       : TLabel;
    BtnClose       : TButton;
    TabControl1    : TTabControl;
    TabGeneral     : TTabItem;
    LayoutGeneral  : TLayout;
    LblProvider    : TLabel;
    CmbProvider    : TComboBox;
    LblModel       : TLabel;
    CmbModel       : TComboBox;
    LblTheme       : TLabel;
    SwtDark        : TSwitch;
    LblPromptLabel : TLabel;
    MemoPrompt     : TMemo;
    TabApiKeys     : TTabItem;
    ScrollApiKeys  : TVertScrollBox;
    LayoutApiKeys  : TLayout;
    LblKeyHint     : TLabel;
    LblKeyclaude   : TLabel;
    EdtKeyClaude   : TEdit;
    LblKeyOpenAI   : TLabel;
    EdtKeyOpenAI   : TEdit;
    LblKeyGemini   : TLabel;
    EdtKeyGemini   : TEdit;
    LblKeyGroq     : TLabel;
    EdtKeyGroq     : TEdit;
    procedure FormCreate(Sender: TObject);
    procedure BtnClose_Click(Sender: TObject);
    procedure CmbProvider_Change(Sender: TObject);
  private
    procedure LoadFromSettings;
    procedure SaveToSettings;
    procedure PopulateModels(const AProvider: string);
  end;

implementation

{$R *.fmx}

uses
  uApp.Settings,
  uApp.ProviderList;

procedure TFrmSettings.FormCreate(Sender: TObject);
begin
  for var I := 0 to ProviderList.Count - 1 do
    CmbProvider.Items.Add(ProviderList.Item(I).DisplayName);
  LoadFromSettings;
end;

procedure TFrmSettings.LoadFromSettings;
begin
  // Provider
  var Idx := ProviderList.IndexOf(AppSettings.ProviderName);
  if Idx >= 0 then CmbProvider.ItemIndex := Idx
  else CmbProvider.ItemIndex := 0;

  PopulateModels(AppSettings.ProviderName);
  var MIdx := CmbModel.Items.IndexOf(AppSettings.ModelName);
  if MIdx >= 0 then CmbModel.ItemIndex := MIdx
  else CmbModel.ItemIndex := 0;

  // Theme
  SwtDark.IsChecked := AppSettings.ThemeDark;

  // System prompt
  MemoPrompt.Text := AppSettings.SystemPrompt;

  // API keys
  EdtKeyClaude.Text  := AppSettings.ApiKey['Claude'];
  EdtKeyOpenAI.Text  := AppSettings.ApiKey['OpenAI'];
  EdtKeyGemini.Text  := AppSettings.ApiKey['Gemini'];
  EdtKeyGroq.Text    := AppSettings.ApiKey['Groq'];
end;

procedure TFrmSettings.SaveToSettings;
begin
  var Idx := CmbProvider.ItemIndex;
  if Idx >= 0 then
  begin
    AppSettings.ProviderName := ProviderList.Item(Idx).Name;
    AppSettings.ModelName    := CmbModel.Text;
  end;

  AppSettings.ThemeDark    := SwtDark.IsChecked;
  AppSettings.SystemPrompt := MemoPrompt.Text;

  AppSettings.ApiKey['Claude'] := EdtKeyClaude.Text;
  AppSettings.ApiKey['OpenAI'] := EdtKeyOpenAI.Text;
  AppSettings.ApiKey['Gemini'] := EdtKeyGemini.Text;
  AppSettings.ApiKey['Groq']   := EdtKeyGroq.Text;

  AppSettings.Save;
end;

procedure TFrmSettings.PopulateModels(const AProvider: string);
var
  ProvName: string;
  LModels : TStringList;
begin
  ProvName := AProvider;
  if ProvName = '' then
  begin
    var Idx := CmbProvider.ItemIndex;
    if Idx >= 0 then ProvName := ProviderList.Item(Idx).Name;
  end;
  CmbModel.Items.Clear;
  LModels := ProviderList.FetchModels(ProvName, AppSettings.ApiKey[ProvName]);
  try
    CmbModel.Items.Assign(LModels);
  finally
    LModels.Free;
  end;
  if CmbModel.Items.Count > 0 then
    CmbModel.ItemIndex := 0;
end;

procedure TFrmSettings.CmbProvider_Change(Sender: TObject);
begin
  if CmbProvider.ItemIndex >= 0 then
    PopulateModels(ProviderList.Item(CmbProvider.ItemIndex).Name);
end;

procedure TFrmSettings.BtnClose_Click(Sender: TObject);
begin
  SaveToSettings;
  ModalResult := mrOk;
end;

end.
