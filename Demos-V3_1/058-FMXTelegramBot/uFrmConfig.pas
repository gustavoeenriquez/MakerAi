unit uFrmConfig;

// =============================================================================
// MakerAI Bot - Formulario de configuracion de proveedores
// Construido 100% en codigo (sin .fmx) para evitar problemas de encoding.
// Categorias: LLM, TTS, STT, Imagen, Video, Embeddings
// Cada categoria tiene proveedor primario + fallback.
// =============================================================================

interface

uses
  System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.Layouts, FMX.TabControl,
  FMX.ListBox, FMX.Controls.Presentation,
  uDmTelegramBot;

type
  // Controles de un slot (primario o fallback)
  TProviderRow = record
    CboDriver: TComboBox;
    EdModel  : TEdit;
    EdApiKey : TEdit;
    EdExtra  : TEdit;
  end;

  // Par de rows por categoria
  TCategoryUI = record
    Row: array[0..1] of TProviderRow;  // [0]=primario [1]=fallback
  end;

  TFrmConfig = class(TForm)
  private
    FTabs    : TTabControl;
    FCats    : array[0..5] of TCategoryUI;
    FBtnSave : TButton;
    FBtnCancel: TButton;
    FDm      : TDmTelegramBot;

    // Drivers disponibles por categoria
    class function DriversFor(ACat: Integer): TArray<string>;
    // Extra label por categoria
    class function ExtraLabel(ACat: Integer): string;

    procedure BuildUI;
    procedure BuildCategoryTab(ATabIdx: Integer; const ATitle: string;
      const ADrivers: TArray<string>; const AExtraLbl: string);
    procedure BuildSlotGroup(AParent: TControl; ASlotIdx: Integer;
      ACatIdx: Integer; const ADrivers: TArray<string>;
      const AExtraLbl: string);

    procedure BtnSaveClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);

    procedure LoadFromDm;
    procedure SaveToDm;

    procedure FillSlot(ACat, ASlot: Integer; const AConfig: TProviderConfig);
    function  ReadSlot(ACat, ASlot: Integer): TProviderConfig;
  public
    constructor CreateConfig(AOwner: TComponent; ADm: TDmTelegramBot);
  end;

implementation

{$R *.fmx}

const
  CAT_LLM  = 0;
  CAT_TTS  = 1;
  CAT_STT  = 2;
  CAT_IMG  = 3;
  CAT_VID  = 4;
  CAT_EMB  = 5;

  CAT_TITLES: array[0..5] of string = ('LLM', 'TTS', 'STT', 'Imagen', 'Video', 'Embeddings');

class function TFrmConfig.DriversFor(ACat: Integer): TArray<string>;
begin
  case ACat of
    CAT_LLM: Result := ['Claude', 'OpenAI', 'Gemini', 'Ollama'];
    CAT_TTS: Result := ['Gemini', 'OpenAI', 'ElevenLabs'];
    CAT_STT: Result := ['OpenAI', 'Gemini'];
    CAT_IMG: Result := ['OpenAI', 'Gemini', 'Stability AI'];
    CAT_VID: Result := ['Gemini'];
    CAT_EMB: Result := ['OpenAI', 'Gemini'];
  else
    Result := [];
  end;
end;

class function TFrmConfig.ExtraLabel(ACat: Integer): string;
begin
  case ACat of
    CAT_LLM: Result := 'Base URL (vacio = default)';
    CAT_TTS: Result := 'Voz';
    CAT_STT: Result := 'Idioma (ej: es, en)';
    CAT_IMG: Result := 'Tamano (ej: 1024x1024)';
    CAT_VID: Result := 'Duracion (seg)';
    CAT_EMB: Result := 'Dimensiones';
  else
    Result := 'Extra';
  end;
end;

constructor TFrmConfig.CreateConfig(AOwner: TComponent; ADm: TDmTelegramBot);
begin
  inherited CreateNew(AOwner);
  FDm := ADm;
  Caption      := 'Configuracion de Proveedores';
  Width        := 720;
  Height       := 520;
  Position     := TFormPosition.ScreenCenter;
  BuildUI;
  LoadFromDm;
end;

procedure TFrmConfig.BuildUI;
var
  LayBottom: TLayout;
  i: Integer;
begin
  // Tab control (ocupa todo menos los botones de abajo)
  FTabs := TTabControl.Create(Self);
  FTabs.Parent := Self;
  FTabs.Align  := TAlignLayout.Client;
  FTabs.TabPosition := TTabPosition.Top;

  for i := 0 to 5 do
    BuildCategoryTab(i, CAT_TITLES[i], DriversFor(i), ExtraLabel(i));

  // Barra de botones abajo
  LayBottom := TLayout.Create(Self);
  LayBottom.Parent := Self;
  LayBottom.Align  := TAlignLayout.Bottom;
  LayBottom.Height := 48;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent  := LayBottom;
  FBtnCancel.Align   := TAlignLayout.Right;
  FBtnCancel.Width   := 110;
  FBtnCancel.Margins.Right  := 8;
  FBtnCancel.Margins.Top    := 8;
  FBtnCancel.Margins.Bottom := 8;
  FBtnCancel.Text    := 'Cancelar';
  FBtnCancel.OnClick := BtnCancelClick;

  FBtnSave := TButton.Create(Self);
  FBtnSave.Parent  := LayBottom;
  FBtnSave.Align   := TAlignLayout.Right;
  FBtnSave.Width   := 110;
  FBtnSave.Margins.Right  := 4;
  FBtnSave.Margins.Top    := 8;
  FBtnSave.Margins.Bottom := 8;
  FBtnSave.Text    := 'Guardar';
  FBtnSave.OnClick := BtnSaveClick;
end;

procedure TFrmConfig.BuildCategoryTab(ATabIdx: Integer; const ATitle: string;
  const ADrivers: TArray<string>; const AExtraLbl: string);
var
  Tab    : TTabItem;
  Scroll : TScrollBox;
  LblPri, LblFal: TLabel;
  Sep   : TLayout;
begin
  Tab := FTabs.Add;
  Tab.Text := ATitle;

  Scroll := TScrollBox.Create(Self);
  Scroll.Parent := Tab;
  Scroll.Align  := TAlignLayout.Client;
  Scroll.ShowScrollBars := True;

  // --- Seccion Primario ---
  LblPri := TLabel.Create(Self);
  LblPri.Parent := Scroll;
  LblPri.Align  := TAlignLayout.Top;
  LblPri.Text   := 'Proveedor Principal';
  LblPri.Font.Style := [TFontStyle.fsBold];
  LblPri.Height := 28;
  LblPri.Margins.Left := 8;
  LblPri.Margins.Top  := 8;

  BuildSlotGroup(Scroll, 0, ATabIdx, ADrivers, AExtraLbl);

  // Separador visual
  Sep := TLayout.Create(Self);
  Sep.Parent := Scroll;
  Sep.Align  := TAlignLayout.Top;
  Sep.Height := 16;

  // --- Seccion Fallback ---
  LblFal := TLabel.Create(Self);
  LblFal.Parent := Scroll;
  LblFal.Align  := TAlignLayout.Top;
  LblFal.Text   := 'Proveedor Alternativo (Fallback)';
  LblFal.Font.Style := [TFontStyle.fsBold];
  LblFal.Height := 28;
  LblFal.Margins.Left := 8;

  BuildSlotGroup(Scroll, 1, ATabIdx, ADrivers, AExtraLbl);
end;

procedure TFrmConfig.BuildSlotGroup(AParent: TControl; ASlotIdx: Integer;
  ACatIdx: Integer; const ADrivers: TArray<string>; const AExtraLbl: string);

  function MakeRow(const ALabel: string; AHeight: Single): TLayout;
  var Lbl: TLabel;
  begin
    Result := TLayout.Create(Self);
    Result.Parent := AParent;
    Result.Align  := TAlignLayout.Top;
    Result.Height := AHeight;
    Result.Margins.Left  := 16;
    Result.Margins.Right := 8;
    Result.Margins.Top   := 4;
    Lbl := TLabel.Create(Self);
    Lbl.Parent := Result;
    Lbl.Align  := TAlignLayout.Left;
    Lbl.Width  := 100;
    Lbl.Text   := ALabel;
    Lbl.VertTextAlign := TTextAlign.Center;
  end;

var
  Row: TLayout;
  Cbo: TComboBox;
  Ed : TEdit;
  D  : string;
begin
  // Driver
  Row := MakeRow('Driver:', 36);
  Cbo := TComboBox.Create(Self);
  Cbo.Parent := Row;
  Cbo.Align  := TAlignLayout.Client;
  for D in ADrivers do Cbo.Items.Add(D);
  if Cbo.Items.Count > 0 then Cbo.ItemIndex := 0;
  FCats[ACatIdx].Row[ASlotIdx].CboDriver := Cbo;

  // Model
  Row := MakeRow('Modelo:', 36);
  Ed  := TEdit.Create(Self);
  Ed.Parent := Row;
  Ed.Align  := TAlignLayout.Client;
  FCats[ACatIdx].Row[ASlotIdx].EdModel := Ed;

  // API Key
  Row := MakeRow('API Key:', 36);
  Ed  := TEdit.Create(Self);
  Ed.Parent   := Row;
  Ed.Align    := TAlignLayout.Client;
  Ed.Password := True;
  FCats[ACatIdx].Row[ASlotIdx].EdApiKey := Ed;

  // Extra
  Row := MakeRow(AExtraLbl + ':', 36);
  Ed  := TEdit.Create(Self);
  Ed.Parent := Row;
  Ed.Align  := TAlignLayout.Client;
  FCats[ACatIdx].Row[ASlotIdx].EdExtra := Ed;
end;

// --- Data binding ---

procedure TFrmConfig.FillSlot(ACat, ASlot: Integer; const AConfig: TProviderConfig);
var
  R  : TProviderRow;
  Idx: Integer;
begin
  R := FCats[ACat].Row[ASlot];
  // Driver
  Idx := R.CboDriver.Items.IndexOf(AConfig.Driver);
  if Idx >= 0 then R.CboDriver.ItemIndex := Idx
  else
  begin
    R.CboDriver.Items.Add(AConfig.Driver);
    R.CboDriver.ItemIndex := R.CboDriver.Items.Count - 1;
  end;
  R.EdModel.Text  := AConfig.Model;
  R.EdApiKey.Text := AConfig.ApiKey;
  R.EdExtra.Text  := AConfig.Extra;
end;

function TFrmConfig.ReadSlot(ACat, ASlot: Integer): TProviderConfig;
var R: TProviderRow;
begin
  R := FCats[ACat].Row[ASlot];
  Result.Driver := R.CboDriver.Items[R.CboDriver.ItemIndex];
  Result.Model  := R.EdModel.Text;
  Result.ApiKey := R.EdApiKey.Text;
  Result.Extra  := R.EdExtra.Text;
end;

procedure TFrmConfig.LoadFromDm;
var P: TBotProviders;
begin
  P := FDm.Providers;
  FillSlot(CAT_LLM, 0, P.LLM.Primary);        FillSlot(CAT_LLM, 1, P.LLM.Fallback);
  FillSlot(CAT_TTS, 0, P.TTS.Primary);        FillSlot(CAT_TTS, 1, P.TTS.Fallback);
  FillSlot(CAT_STT, 0, P.STT.Primary);        FillSlot(CAT_STT, 1, P.STT.Fallback);
  FillSlot(CAT_IMG, 0, P.Image.Primary);      FillSlot(CAT_IMG, 1, P.Image.Fallback);
  FillSlot(CAT_VID, 0, P.Video.Primary);      FillSlot(CAT_VID, 1, P.Video.Fallback);
  FillSlot(CAT_EMB, 0, P.Embeddings.Primary); FillSlot(CAT_EMB, 1, P.Embeddings.Fallback);
end;

procedure TFrmConfig.SaveToDm;
var P: TBotProviders;
begin
  P.LLM.Primary        := ReadSlot(CAT_LLM, 0); P.LLM.Fallback        := ReadSlot(CAT_LLM, 1);
  P.TTS.Primary        := ReadSlot(CAT_TTS, 0); P.TTS.Fallback        := ReadSlot(CAT_TTS, 1);
  P.STT.Primary        := ReadSlot(CAT_STT, 0); P.STT.Fallback        := ReadSlot(CAT_STT, 1);
  P.Image.Primary      := ReadSlot(CAT_IMG, 0); P.Image.Fallback      := ReadSlot(CAT_IMG, 1);
  P.Video.Primary      := ReadSlot(CAT_VID, 0); P.Video.Fallback      := ReadSlot(CAT_VID, 1);
  P.Embeddings.Primary := ReadSlot(CAT_EMB, 0); P.Embeddings.Fallback := ReadSlot(CAT_EMB, 1);
  FDm.Providers := P;
  FDm.SaveProviders;
end;

procedure TFrmConfig.BtnSaveClick(Sender: TObject);
begin
  SaveToDm;
  ModalResult := mrOk;
end;

procedure TFrmConfig.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
