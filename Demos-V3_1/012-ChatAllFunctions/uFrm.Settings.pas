unit uFrm.Settings;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.Edit, FMX.Memo,
  FMX.Controls.Presentation, FMX.ScrollBox,
  uApp.ProviderList;

type
  TFrmSettings = class(TForm)
    PanelBottom: TPanel;
    BtnOK      : TButton;
    BtnCancel  : TButton;
    ScrMain    : TVertScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnOK_Click(Sender: TObject);
  private
    FKeyEdits: TArray<TEdit>;
    FMemoSystemPrompt: TMemo;
    procedure BuildControls;
    procedure LoadValues;
    procedure SaveValues;
    procedure Hdr(const AText: string);
    function  AddKeyRow(const AProvider: string): TEdit;
  end;

implementation

{$R *.fmx}

uses
  System.Math,
  uApp.Settings;

procedure TFrmSettings.Hdr(const AText: string);
var
  Lbl: TLabel;
begin
  Lbl := TLabel.Create(Self);
  Lbl.Parent  := ScrMain;
  Lbl.Align   := TAlignLayout.Top;
  Lbl.Margins.Rect := RectF(0, 10, 0, 2);
  Lbl.Height  := 20;
  Lbl.Text    := AText;
  Lbl.TextSettings.Font.Style := [TFontStyle.fsBold];
  Lbl.TextSettings.Font.Size  := 11;
end;

function TFrmSettings.AddKeyRow(const AProvider: string): TEdit;
var
  Lbl: TLabel;
  Edt: TEdit;
begin
  Lbl := TLabel.Create(Self);
  Lbl.Parent := ScrMain;
  Lbl.Align  := TAlignLayout.Top;
  Lbl.Height := 18;
  Lbl.Text   := AProvider;
  Lbl.TextSettings.Font.Size := 10;

  Edt := TEdit.Create(Self);
  Edt.Parent := ScrMain;
  Edt.Align  := TAlignLayout.Top;
  Edt.Height := 28;
  Edt.Margins.Rect := RectF(0, 2, 0, 4);
  Edt.Password := False;
  Edt.Name   := 'EdtKey_' + AProvider;
  Result := Edt;
end;

procedure TFrmSettings.BuildControls;
var
  I    : Integer;
  LItem: TProviderItem;
begin
  Hdr('API Keys');

  SetLength(FKeyEdits, ProviderList.Count);
  for I := 0 to ProviderList.Count - 1 do
  begin
    LItem := ProviderList.Item(I);
    if LItem.NeedsKey then
      FKeyEdits[I] := AddKeyRow(LItem.DisplayName + ' (' + LItem.Name + ')')
    else
      FKeyEdits[I] := nil;
  end;

  Hdr('System Prompt');
  FMemoSystemPrompt := TMemo.Create(Self);
  FMemoSystemPrompt.Parent  := ScrMain;
  FMemoSystemPrompt.Align   := TAlignLayout.Top;
  FMemoSystemPrompt.Height  := 120;
  FMemoSystemPrompt.Margins.Rect := RectF(0, 2, 0, 0);
  FMemoSystemPrompt.WordWrap := True;
end;

procedure TFrmSettings.LoadValues;
var
  I    : Integer;
  LItem: TProviderItem;
begin
  for I := 0 to ProviderList.Count - 1 do
    if Assigned(FKeyEdits[I]) then
    begin
      LItem := ProviderList.Item(I);
      FKeyEdits[I].Text := AppSettings.ApiKey[LItem.Name];
    end;
  FMemoSystemPrompt.Text := AppSettings.SystemPrompt;
end;

procedure TFrmSettings.SaveValues;
var
  I    : Integer;
  LItem: TProviderItem;
begin
  for I := 0 to ProviderList.Count - 1 do
    if Assigned(FKeyEdits[I]) then
    begin
      LItem := ProviderList.Item(I);
      AppSettings.ApiKey[LItem.Name] := Trim(FKeyEdits[I].Text);
    end;
  AppSettings.SystemPrompt := FMemoSystemPrompt.Text;
end;

procedure TFrmSettings.FormCreate(Sender: TObject);
begin
  BuildControls;
  LoadValues;
end;

procedure TFrmSettings.BtnOK_Click(Sender: TObject);
begin
  SaveValues;
  AppSettings.Save;
  ModalResult := mrOk;
end;

end.
