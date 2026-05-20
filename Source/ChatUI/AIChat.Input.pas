unit AIChat.Input;

// MIT License — Gustavo Enríquez <gustavoeenriquez@gmail.com>
// TAIChatInput — Skia-painted chat input bar (no child FMX buttons).
// Custom Skia dropdown menu overlay — no TPopupMenu needed.
// Voice mode: wire TAIVoiceMonitor events to the Notify* methods below.

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math,
  System.UITypes, System.IOUtils, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Memo,
  FMX.Dialogs, FMX.Graphics, FMX.Surfaces,
  FMX.Platform, FMX.Forms,
  Skia, FMX.Skia,
  AIChat.Types, AIChat.TempStore, AIChat.ScreenCapture;

procedure Register;

const
  // Layout
  ACI_PAD         = 12;
  ACI_BTN_SZ      = 30;
  ACI_CHIP_W      = 72;
  ACI_CHIP_H      = 52;
  ACI_CHIP_GAP    = 6;
  ACI_CHIP_PAD    = 8;
  ACI_BOTTOM_H    = 44;
  ACI_MIN_MEMO    = 44;
  ACI_MAX_MEMO    = 200;
  ACI_RADIUS      = 16;
  ACI_MIC_GAP     = 8;     // horizontal gap between mic and send buttons
  // Dropdown menu
  ACI_MENU_W      = 172;
  ACI_MENU_ITEM_H = 36;
  ACI_MENU_ITEMS  = 4;
  ACI_MENU_VPAD   = 6;
  ACI_MENU_HPAD   = 14;
  ACI_MENU_RADIUS = 8;
  ACI_MENU_H      = ACI_MENU_ITEMS * ACI_MENU_ITEM_H + 2 * ACI_MENU_VPAD;

type
  // Color palette — all colors that change between light and dark theme
  TACIPalette = record
    Bg     : TAlphaColor;
    Border : TAlphaColor;
    Sep    : TAlphaColor;
    Btn    : TAlphaColor;
    BtnHov : TAlphaColor;
    BtnPre : TAlphaColor;
    SendDis: TAlphaColor;
    Gray   : TAlphaColor;
    ChipBg : TAlphaColor;
    ChipBd : TAlphaColor;
    CloseN : TAlphaColor;
    CloseH : TAlphaColor;
    MenuBg : TAlphaColor;
    MenuBd : TAlphaColor;
    MenuHov: TAlphaColor;
    MenuTxt: TAlphaColor;
    MenuDis: TAlphaColor;
    BarBg  : TAlphaColor;
  end;

  // State of the microphone button (mirrors TAIVoiceMonitor states)
  TAIChatVoiceState = (vsOff, vsPreparing, vsListening, vsTalking, vsAITalking);

  // AAudio = nil for text sends; WAV TMemoryStream for voice sends.
  // Receiver must copy AAudio if it needs it after the handler returns.
  TAIChatInputSendEvent = procedure(Sender: TObject; const APrompt: string;
                                    AAttachments: TAIChatAttachments;
                                    AAudio: TMemoryStream) of object;

  // Fired when the user toggles the mic button.
  // Wire AActivate to TAIVoiceMonitor.Active in the host form.
  TAIChatMicToggleEvent = procedure(Sender: TObject; AActivate: Boolean) of object;

  TAIChatBtnKind = (bkNone, bkMenu, bkSend, bkMic);

  TAIChatChipState = record
    Attachment     : TAIChatAttachment;  // owned
    ChipRect       : TRectF;
    CloseRect      : TRectF;
    ThumbnailImage : ISkImage;           // lazy-loaded for fkImage; nil = not yet tried
    ThumbFailed    : Boolean;
  end;

  // ── Forward declaration so TAIChatMenuPanel can reference TAIChatInput ──────
  TAIChatInput = class;

  // ── Skia-drawn dropdown menu overlay ─────────────────────────────────────────
  TAIChatMenuPanel = class(TSkCustomControl)
  private
    FInput    : TAIChatInput;
    FHovItem  : Integer;
    FMenuRect : TRectF;
    FItemRects: array[0..ACI_MENU_ITEMS - 1] of TRectF;
    function  ItemEnabled(AIdx: Integer): Boolean;
    procedure ComputeItemRects;
    procedure HandleMouseLeave(Sender: TObject);
  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
                   const AOpacity: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent; AInput: TAIChatInput;
                       AMenuLeft, AMenuBottom: Single); reintroduce;
  end;

  // ── TAIChatInput ─────────────────────────────────────────────────────────────
  TAIChatInput = class(TSkCustomControl)
  private
    // ── Child controls ─────────────────────────────────────────────────────
    FMemo       : TMemo;
    FTimer      : TTimer;
    FAudioTimer : TTimer;    // 50 ms — animates sound bar while mic active
    FMenuPanel  : TAIChatMenuPanel;
    FTempStore  : TAIChatTempStore;

    // ── Chip data (owned) ──────────────────────────────────────────────────
    FChips : array of TAIChatChipState;

    // ── Hit areas (computed each frame) ───────────────────────────────────
    FBtnMenuRect : TRectF;
    FBtnSendRect : TRectF;
    FBtnMicRect  : TRectF;   // Empty when MicVisible=False

    // ── Interaction state ──────────────────────────────────────────────────
    FHovBtn      : TAIChatBtnKind;
    FPressBtn    : TAIChatBtnKind;
    FHovClose    : Integer;   // chip index, -1 = none

    // ── Layout cache ───────────────────────────────────────────────────────
    FChipStripH : Single;

    // ── Voice state ────────────────────────────────────────────────────────
    FMicVisible      : Boolean;
    FUseSoundMonitor : Boolean;
    FMicState        : TAIChatVoiceState;
    FSoundLevel      : Int64;
    FMaxSoundSeen    : Int64;

    // ── Properties backing ─────────────────────────────────────────────────
    FBusy            : Boolean;
    FEnterAsSend     : Boolean;
    FValidExtensions : string;
    FThemeDark       : Boolean;
    FPalette         : TACIPalette;
    FOnSend          : TAIChatInputSendEvent;
    FOnCancel        : TNotifyEvent;
    FOnMicToggle     : TAIChatMicToggleEvent;

    // ── Setup ──────────────────────────────────────────────────────────────
    procedure CreateMemo;
    procedure MemoStyleApplied(Sender: TObject);

    // ── Skia drawing ───────────────────────────────────────────────────────
    procedure DrawChips(const ACanvas: ISkCanvas);
    procedure DrawBottomBar(const ACanvas: ISkCanvas);
    procedure PaintCircleBtn(const ACanvas: ISkCanvas; const ARect: TRectF;
                             const AGlyph: string; ABg, AFg: TAlphaColor;
                             AFontSz: Single);
    procedure DrawMicIcon(const ACanvas: ISkCanvas; const ARect: TRectF);
    procedure DrawSoundBar(const ACanvas: ISkCanvas);

    // ── Layout ─────────────────────────────────────────────────────────────
    procedure ComputeLayout;
    procedure UpdateLayout;
    procedure UpdateMemoPos;
    function  ChipStripHeight: Single;
    function  SendEnabled: Boolean;

    // ── Timers + Memo ──────────────────────────────────────────────────────
    procedure TimerTick(Sender: TObject);
    procedure AudioTimerTick(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
                          Shift: TShiftState);
    procedure MemoChange(Sender: TObject);

    // ── Dropdown menu ──────────────────────────────────────────────────────
    procedure ShowMenu;
    procedure HideMenu;
    procedure ExecuteMenuItem(AIdx: Integer);

    // ── Operations ─────────────────────────────────────────────────────────
    procedure DoSend;
    procedure DoSendWithAudio(AAudio: TMemoryStream);
    procedure AddChip(AAttachment: TAIChatAttachment);
    procedure DeleteChip(AIndex: Integer);
    procedure AddBitmapChip(ABitmap: FMX.Graphics.TBitmap; const AName: string);
    procedure PasteFromClipboard;
    procedure UploadFile;
    procedure CaptureScreen;

    // ── Voice helpers ──────────────────────────────────────────────────────
    procedure SetThemeDark(AValue: Boolean);
    procedure SetMicVisible(AValue: Boolean);
    procedure SetUseSoundMonitor(AValue: Boolean);
    function  MicStateColor: TAlphaColor;

    // ── Helpers ────────────────────────────────────────────────────────────
    function  HasAttachments: Boolean;
    function  IsExtensionValid(const AExt: string): Boolean;
    function  LoadThumbnail(const APath: string): ISkImage;
    function  GetPromptText: string;
    procedure SetPromptText(const V: string);
    procedure SetBusy(const V: Boolean);
    procedure HandleMouseLeave(Sender: TObject);

  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
                   const AOpacity: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                      X, Y: Single); override;
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure AddAttachment(const APath: string);
    procedure ClearAttachments;
    procedure ClearAll;
    procedure SetFocus; reintroduce;

    // ── Voice notifications ─────────────────────────────────────────────────
    // Call from your TAIVoiceMonitor event handlers (can be called from any
    // thread — use TThread.Queue if calling from a background thread).
    procedure NotifyVoiceCalibrated;
    procedure NotifyVoiceChangeState(AUserSpeaking: Boolean);
    procedure NotifyVoiceSpeechEnd(AIsValid: Boolean; AAudioStream: TMemoryStream);
    procedure NotifyVoiceUpdate(ASoundLevel: Int64);
    procedure NotifyVoiceError;
    procedure NotifyVoiceAITalking(AIsAITalking: Boolean);

    property MicState : TAIChatVoiceState read FMicState;

  published
    property Busy            : Boolean read FBusy       write SetBusy       default False;
    property EnterAsSend     : Boolean read FEnterAsSend write FEnterAsSend  default False;
    property ThemeDark       : Boolean read FThemeDark  write SetThemeDark  default False;
    property ValidExtensions : string  read FValidExtensions write FValidExtensions;
    property PromptText      : string  read GetPromptText write SetPromptText;
    property MicVisible      : Boolean read FMicVisible write SetMicVisible default False;
    property UseSoundMonitor : Boolean read FUseSoundMonitor write SetUseSoundMonitor default False;
    property OnSend          : TAIChatInputSendEvent read FOnSend   write FOnSend;
    property OnCancel        : TNotifyEvent          read FOnCancel write FOnCancel;
    property OnMicToggle     : TAIChatMicToggleEvent read FOnMicToggle write FOnMicToggle;
  end;

implementation

// ── Theme-invariant colors ────────────────────────────────────────────────────
const
  CLR_SEND      = $FF007AFF;
  CLR_SEND_HOV  = $FF1A8FFF;
  CLR_SEND_PRE  = $FF005FCC;
  CLR_STOP      = $FFFF3B30;
  CLR_STOP_PRE  = $FFCC2A22;
  CLR_WHITE     = $FFFFFFFF;
  // Mic states (same in both themes)
  CLR_MIC_OFF   = $FFA0A0A0;
  CLR_MIC_PREP  = $FFFF9500;
  CLR_MIC_LIST  = $FF34C759;
  CLR_MIC_TALK  = $FFFF3B30;
  CLR_MIC_AI    = $FF007AFF;

// ── Palette factory functions ─────────────────────────────────────────────────

function LightPalette: TACIPalette;
begin
  Result.Bg      := $FFF8F8F8;
  Result.Border  := $FFD0D0D0;
  Result.Sep     := $30000000;
  Result.Btn     := $FFE8E8E8;
  Result.BtnHov  := $FFD0D0D0;
  Result.BtnPre  := $FFB0B0B0;
  Result.SendDis := $FFD0D0D0;
  Result.Gray    := $FF909090;
  Result.ChipBg  := $FFE8E8E8;
  Result.ChipBd  := $FFCCCCCC;
  Result.CloseN  := $FFC0C0C0;
  Result.CloseH  := $FF909090;
  Result.MenuBg  := $FFFFFFFF;
  Result.MenuBd  := $FFD0D0D0;
  Result.MenuHov := $FFE8F0FE;
  Result.MenuTxt := $FF202020;
  Result.MenuDis := $FFA0A0A0;
  Result.BarBg   := $FFD8D8D8;
end;

function DarkPalette: TACIPalette;
begin
  Result.Bg      := $FF2A2A2E;
  Result.Border  := $FF444455;
  Result.Sep     := $30FFFFFF;
  Result.Btn     := $FF3A3A4A;
  Result.BtnHov  := $FF505060;
  Result.BtnPre  := $FF606070;
  Result.SendDis := $FF505060;
  Result.Gray    := $FFA0A0B0;
  Result.ChipBg  := $FF333344;
  Result.ChipBd  := $FF555566;
  Result.CloseN  := $FF707080;
  Result.CloseH  := $FFA0A0B0;
  Result.MenuBg  := $FF2A2A2E;
  Result.MenuBd  := $FF444455;
  Result.MenuHov := $FF3A3A5A;
  Result.MenuTxt := $FFE0E0E8;
  Result.MenuDis := $FF666677;
  Result.BarBg   := $FF404055;
end;

const
  MENU_LABELS: array[0..ACI_MENU_ITEMS - 1] of string = (
    'Paste',
    'Upload file...',
    'Screenshot...',
    'Clear attachments');

{ ── TAIChatMenuPanel ─────────────────────────────────────────────────────────── }

constructor TAIChatMenuPanel.Create(AOwner: TComponent; AInput: TAIChatInput;
  AMenuLeft, AMenuBottom: Single);
var
  Form  : TCommonCustomForm;
  MLeft : Single;
  MTop  : Single;
begin
  inherited Create(AOwner);
  FInput   := AInput;
  FHovItem := -1;
  HitTest  := True;

  Form := AOwner as TCommonCustomForm;
  SetBounds(0, 0, Round(Form.ClientWidth), Round(Form.ClientHeight));

  MLeft := AMenuLeft;
  MTop  := AMenuBottom - ACI_MENU_H;
  if MLeft + ACI_MENU_W > Form.ClientWidth then
    MLeft := Form.ClientWidth - ACI_MENU_W;
  if MLeft < 2 then MLeft := 2;
  if MTop < 2 then
    MTop := AMenuBottom + 4;

  FMenuRect := TRectF.Create(MLeft, MTop,
                              MLeft + ACI_MENU_W, MTop + ACI_MENU_H);
  OnMouseLeave := HandleMouseLeave;
end;

function TAIChatMenuPanel.ItemEnabled(AIdx: Integer): Boolean;
begin
  case AIdx of
    0: Result := True;
    1: Result := True;
    2: Result := {$IFDEF MSWINDOWS} True {$ELSE} False {$ENDIF};
    3: Result := (FInput <> nil) and FInput.HasAttachments;
  else
    Result := True;
  end;
end;

procedure TAIChatMenuPanel.ComputeItemRects;
var
  I    : Integer;
  ItemY: Single;
begin
  ItemY := FMenuRect.Top + ACI_MENU_VPAD;
  for I := 0 to ACI_MENU_ITEMS - 1 do
  begin
    FItemRects[I] := TRectF.Create(
      FMenuRect.Left,  ItemY,
      FMenuRect.Right, ItemY + ACI_MENU_ITEM_H);
    ItemY := ItemY + ACI_MENU_ITEM_H;
  end;
end;

procedure TAIChatMenuPanel.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
var
  Paint     : ISkPaint;
  Font      : ISkFont;
  ShadR     : TRectF;
  IR        : TRectF;
  I         : Integer;
  TxtColor  : TAlphaColor;
  TxtBounds : TRectF;
  TX, TY    : Single;
begin
  ComputeItemRects;

  Paint           := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style     := TSkPaintStyle.Fill;

  ShadR := FMenuRect;
  ShadR.Offset(2, 3);
  Paint.Color := $28000000;
  ACanvas.DrawRoundRect(ShadR, ACI_MENU_RADIUS, ACI_MENU_RADIUS, Paint);

  Paint.Color := FInput.FPalette.MenuBg;
  ACanvas.DrawRoundRect(FMenuRect, ACI_MENU_RADIUS, ACI_MENU_RADIUS, Paint);

  ACanvas.Save;
  ACanvas.ClipRect(FMenuRect);

  Font := TSkFont.Create(TSkTypeface.MakeDefault, 14);
  for I := 0 to ACI_MENU_ITEMS - 1 do
  begin
    IR := FItemRects[I];
    if (I = FHovItem) and ItemEnabled(I) then
    begin
      Paint.Color := FInput.FPalette.MenuHov;
      ACanvas.DrawRect(IR, Paint);
    end;
    if ItemEnabled(I) then TxtColor := FInput.FPalette.MenuTxt
    else                   TxtColor := FInput.FPalette.MenuDis;
    Paint.Color := TxtColor;
    Font.MeasureText(MENU_LABELS[I], TxtBounds, Paint);
    TX := IR.Left + ACI_MENU_HPAD;
    TY := IR.CenterPoint.Y - (TxtBounds.Top + TxtBounds.Bottom) / 2;
    ACanvas.DrawSimpleText(MENU_LABELS[I], TX, TY, Font, Paint);
  end;

  ACanvas.Restore;

  Paint.Style       := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  Paint.Color       := FInput.FPalette.MenuBd;
  ACanvas.DrawRoundRect(FMenuRect, ACI_MENU_RADIUS, ACI_MENU_RADIUS, Paint);
end;

procedure TAIChatMenuPanel.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Pt     : TPointF;
  NewHov : Integer;
  I      : Integer;
begin
  Pt     := TPointF.Create(X, Y);
  NewHov := -1;
  if FMenuRect.Contains(Pt) then
  begin
    ComputeItemRects;
    for I := 0 to ACI_MENU_ITEMS - 1 do
      if FItemRects[I].Contains(Pt) and ItemEnabled(I) then
      begin
        NewHov := I;
        Break;
      end;
  end;
  Cursor := IfThen(NewHov >= 0, crHandPoint, crDefault);
  if NewHov <> FHovItem then
  begin
    FHovItem := NewHov;
    Redraw;
  end;
end;

procedure TAIChatMenuPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  Pt    : TPointF;
  I     : Integer;
  SelIdx: Integer;
  InpRef: TAIChatInput;
begin
  if Button <> TMouseButton.mbLeft then Exit;
  Pt     := TPointF.Create(X, Y);
  InpRef := FInput;
  if FMenuRect.Contains(Pt) then
  begin
    ComputeItemRects;
    for I := 0 to ACI_MENU_ITEMS - 1 do
      if FItemRects[I].Contains(Pt) and ItemEnabled(I) then
      begin
        SelIdx := I;
        TThread.ForceQueue(nil, procedure begin InpRef.ExecuteMenuItem(SelIdx); end);
        Exit;
      end;
  end
  else
    TThread.ForceQueue(nil, procedure begin InpRef.HideMenu; end);
end;

procedure TAIChatMenuPanel.HandleMouseLeave(Sender: TObject);
begin
  if FHovItem <> -1 then
  begin
    FHovItem := -1;
    Redraw;
  end;
end;

{ ── TAIChatInput ─────────────────────────────────────────────────────────────── }

constructor TAIChatInput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBusy            := False;
  FEnterAsSend     := False;
  FValidExtensions := '';
  FThemeDark       := False;
  FPalette         := LightPalette;
  FHovBtn          := TAIChatBtnKind.bkNone;
  FPressBtn        := TAIChatBtnKind.bkNone;
  FHovClose        := -1;
  FChipStripH      := 0;
  FMenuPanel       := nil;
  FMicVisible      := False;
  FUseSoundMonitor := False;
  FMicState        := TAIChatVoiceState.vsOff;
  FSoundLevel      := 0;
  FMaxSoundSeen    := 0;
  FTempStore       := TAIChatTempStore.Create;
  Height           := ACI_MIN_MEMO + ACI_BOTTOM_H;
  HitTest          := True;
  OnMouseLeave     := HandleMouseLeave;

  FAudioTimer          := TTimer.Create(Self);
  FAudioTimer.Interval := 50;
  FAudioTimer.Enabled  := False;
  FAudioTimer.OnTimer  := AudioTimerTick;

  CreateMemo;
end;

destructor TAIChatInput.Destroy;
var
  I: Integer;
begin
  HideMenu;
  if FMemo       <> nil then FMemo.OnChange      := nil;
  if FTimer      <> nil then FTimer.OnTimer      := nil;
  if FAudioTimer <> nil then FAudioTimer.OnTimer := nil;
  for I := 0 to High(FChips) do
    FChips[I].Attachment.Free;
  FTempStore.Free;
  inherited;
end;

// ── Setup ──────────────────────────────────────────────────────────────────────

procedure TAIChatInput.MemoStyleApplied(Sender: TObject);
begin
  // Remove the TMemo's rectangular background so the Skia-drawn
  // rounded container shows through without the memo overlapping the corners.
  FMemo.StylesData['background.Visible'] := False;
  // Opt out of style-driven font color so our programmatic color always wins.
  FMemo.StyledSettings := FMemo.StyledSettings - [TStyledSetting.FontColor];
  if FThemeDark then
    FMemo.TextSettings.FontColor := $FFE0E0E8
  else
    FMemo.TextSettings.FontColor := $FF202020;
end;

procedure TAIChatInput.CreateMemo;
begin
  FMemo                        := TMemo.Create(Self);
  FMemo.Parent                 := Self;
  FMemo.Stored                 := False;
  FMemo.DisableFocusEffect     := True;
  FMemo.OnChange               := MemoChange;
  FMemo.OnKeyDown              := MemoKeyDown;
  FMemo.OnApplyStyleLookup     := MemoStyleApplied;
  FMemo.Padding.Left           := 4;
  FMemo.Padding.Right          := 4;
  FMemo.Padding.Top            := 4;
  FMemo.Padding.Bottom         := 4;

  FTimer          := TTimer.Create(Self);
  FTimer.Enabled  := False;
  FTimer.Interval := 60;
  FTimer.OnTimer  := TimerTick;
end;

// ── Layout ─────────────────────────────────────────────────────────────────────

function TAIChatInput.ChipStripHeight: Single;
begin
  if Length(FChips) > 0 then
    Result := ACI_CHIP_H + ACI_CHIP_PAD * 2
  else
    Result := 0;
end;

function TAIChatInput.SendEnabled: Boolean;
begin
  Result := FBusy or (Trim(FMemo.Text) <> '') or HasAttachments;
end;

procedure TAIChatInput.ComputeLayout;
var
  I, N  : Integer;
  X, CY : Single;
begin
  N := Length(FChips);
  X := ACI_PAD;
  for I := 0 to N - 1 do
  begin
    FChips[I].ChipRect := TRectF.Create(X, ACI_CHIP_PAD,
                                         X + ACI_CHIP_W, ACI_CHIP_PAD + ACI_CHIP_H);
    FChips[I].CloseRect := TRectF.Create(
      FChips[I].ChipRect.Right - 16, FChips[I].ChipRect.Top,
      FChips[I].ChipRect.Right,      FChips[I].ChipRect.Top + 16);
    X := X + ACI_CHIP_W + ACI_CHIP_GAP;
  end;

  CY := Height - ACI_BOTTOM_H / 2;

  // Send button (rightmost, always)
  FBtnSendRect := TRectF.Create(
    Width - ACI_PAD - ACI_BTN_SZ, CY - ACI_BTN_SZ / 2,
    Width - ACI_PAD,              CY + ACI_BTN_SZ / 2);

  // Mic button (left of send, only when MicVisible)
  if FMicVisible then
    FBtnMicRect := TRectF.Create(
      FBtnSendRect.Left - ACI_MIC_GAP - ACI_BTN_SZ, CY - ACI_BTN_SZ / 2,
      FBtnSendRect.Left - ACI_MIC_GAP,              CY + ACI_BTN_SZ / 2)
  else
    FBtnMicRect := TRectF.Empty;

  // + button (leftmost, always)
  FBtnMenuRect := TRectF.Create(
    ACI_PAD,               CY - ACI_BTN_SZ / 2,
    ACI_PAD + ACI_BTN_SZ, CY + ACI_BTN_SZ / 2);
end;

procedure TAIChatInput.UpdateMemoPos;
var
  MH: Single;
begin
  if FMemo = nil then Exit;
  MH := Max(ACI_MIN_MEMO, Height - FChipStripH - ACI_BOTTOM_H);
  // ACI_PAD horizontal margin keeps the memo inside the rounded corners (r=16).
  // 4px top offset adds breathing room between the top border and the text.
  FMemo.SetBounds(ACI_PAD, FChipStripH + 4, Width - ACI_PAD * 2, MH - 4);
end;

procedure TAIChatInput.UpdateLayout;
var
  StripH, ContentH, MemoH, TotalH: Single;
begin
  if FMemo = nil then Exit;
  StripH   := ChipStripHeight;
  ContentH := FMemo.ContentBounds.Height;
  if ContentH < 1 then ContentH := ACI_MIN_MEMO;
  MemoH    := Min(ACI_MAX_MEMO, Max(ACI_MIN_MEMO, ContentH));
  TotalH   := StripH + MemoH + ACI_BOTTOM_H;
  FChipStripH := StripH;
  if Abs(Self.Height - TotalH) > 0.5 then
    Self.Height := TotalH;
  ComputeLayout;
  UpdateMemoPos;
  Redraw;
end;

procedure TAIChatInput.Resize;
begin
  inherited;
  FChipStripH := ChipStripHeight;
  ComputeLayout;
  UpdateMemoPos;
end;

// ── Skia drawing ───────────────────────────────────────────────────────────────

procedure TAIChatInput.PaintCircleBtn(const ACanvas: ISkCanvas;
  const ARect: TRectF; const AGlyph: string;
  ABg, AFg: TAlphaColor; AFontSz: Single);
var
  Paint     : ISkPaint;
  Font      : ISkFont;
  TxtBounds : TRectF;
  TX, TY    : Single;
begin
  Paint           := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color     := ABg;
  ACanvas.DrawOval(ARect, Paint);

  if (AFontSz > 0) and (AGlyph <> '') then
  begin
    Font := TSkFont.Create(TSkTypeface.MakeDefault, AFontSz);
    Paint.Color := AFg;
    Font.MeasureText(AGlyph, TxtBounds, Paint);
    TX := ARect.CenterPoint.X - TxtBounds.Width / 2 - TxtBounds.Left;
    TY := ARect.CenterPoint.Y - (TxtBounds.Top + TxtBounds.Bottom) / 2;
    ACanvas.DrawSimpleText(AGlyph, TX, TY, Font, Paint);
  end;
end;

procedure TAIChatInput.DrawMicIcon(const ACanvas: ISkCanvas; const ARect: TRectF);
var
  Paint        : ISkPaint;
  CX, CY, W   : Single;
  BodyR, ArcR  : TRectF;
begin
  Paint           := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color     := CLR_WHITE;
  CX := ARect.CenterPoint.X;
  CY := ARect.CenterPoint.Y;
  W  := ARect.Width;

  // Mic capsule (filled white rounded rect, upper half)
  BodyR := TRectF.Create(CX - W*0.14, CY - W*0.30, CX + W*0.14, CY + W*0.10);
  Paint.Style := TSkPaintStyle.Fill;
  ACanvas.DrawRoundRect(BodyR, W*0.14, W*0.14, Paint);

  // Mic stand: arc + vertical stem + base line
  Paint.Style       := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := Max(1.5, W * 0.06);
  ArcR := TRectF.Create(CX - W*0.22, CY - W*0.06, CX + W*0.22, CY + W*0.20);
  ACanvas.DrawArc(ArcR, 0, 180, False, Paint);
  ACanvas.DrawLine(TPointF.Create(CX, CY + W*0.20), TPointF.Create(CX, CY + W*0.30), Paint);
  ACanvas.DrawLine(TPointF.Create(CX - W*0.15, CY + W*0.30),
                   TPointF.Create(CX + W*0.15, CY + W*0.30), Paint);
end;

procedure TAIChatInput.DrawSoundBar(const ACanvas: ISkCanvas);
var
  Paint     : ISkPaint;
  TrackRect : TRectF;
  FillRect  : TRectF;
  Level     : Single;
  BarL, BarR, BarCY: Single;
begin
  // Bar spans the horizontal space between + button and mic button
  BarL  := FBtnMenuRect.Right + 8;
  BarR  := FBtnMicRect.Left  - 8;
  if BarR <= BarL + 10 then Exit;

  BarCY := Height - ACI_BOTTOM_H / 2;

  Paint           := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style     := TSkPaintStyle.Fill;

  // Track background
  TrackRect := TRectF.Create(BarL, BarCY - 3, BarR, BarCY + 3);
  Paint.Color := FPalette.BarBg;
  ACanvas.DrawRoundRect(TrackRect, 3, 3, Paint);

  // Level fill
  if FMaxSoundSeen > 0 then
    Level := Min(1.0, FSoundLevel / FMaxSoundSeen)
  else
    Level := 0;

  if Level > 0.02 then
  begin
    FillRect := TRectF.Create(TrackRect.Left, TrackRect.Top,
                               TrackRect.Left + TrackRect.Width * Level,
                               TrackRect.Bottom);
    Paint.Color := MicStateColor;
    ACanvas.DrawRoundRect(FillRect, 3, 3, Paint);
  end;
end;

procedure TAIChatInput.DrawChips(const ACanvas: ISkCanvas);
var
  I          : Integer;
  Paint      : ISkPaint;
  Font       : ISkFont;
  CR, XR     : TRectF;
  Badge      : string;
  BadgeBg    : TAlphaColor;
  CloseBg    : TAlphaColor;
  TxtBounds  : TRectF;
  TX, TY     : Single;
  BadgeRect  : TRectF;
  ThumbRect  : TRectF;
  FitRect    : TRectF;
  Img        : ISkImage;
  ImgW, ImgH : Single;
  Scale      : Single;
  FitW, FitH : Single;
  UseBadge   : Boolean;
begin
  Paint           := TSkPaint.Create;
  Paint.AntiAlias := True;

  for I := 0 to High(FChips) do
  begin
    CR := FChips[I].ChipRect;
    XR := FChips[I].CloseRect;

    // Chip background + border
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := FPalette.ChipBg;
    ACanvas.DrawRoundRect(CR, 8, 8, Paint);
    Paint.Style       := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 1;
    Paint.Color       := FPalette.ChipBd;
    ACanvas.DrawRoundRect(CR, 8, 8, Paint);

    // ── Thumbnail or badge ───────────────────────────────────────────────
    UseBadge := True;
    if FChips[I].Attachment.FileKind = TChatFileKind.fkImage then
    begin
      if (FChips[I].ThumbnailImage = nil) and not FChips[I].ThumbFailed then
      begin
        FChips[I].ThumbnailImage := LoadThumbnail(FChips[I].Attachment.LocalPath);
        FChips[I].ThumbFailed    := FChips[I].ThumbnailImage = nil;
      end;

      Img := FChips[I].ThumbnailImage;
      if Img <> nil then
      begin
        UseBadge  := False;
        ThumbRect := TRectF.Create(CR.Left + 3, CR.Top + 3, CR.Right - 3, CR.Top + 35);
        ImgW  := Img.Width;
        ImgH  := Img.Height;
        Scale := Min(ThumbRect.Width / ImgW, ThumbRect.Height / ImgH);
        FitW  := ImgW * Scale;
        FitH  := ImgH * Scale;
        FitRect := TRectF.Create(
          ThumbRect.CenterPoint.X - FitW / 2,
          ThumbRect.CenterPoint.Y - FitH / 2,
          ThumbRect.CenterPoint.X + FitW / 2,
          ThumbRect.CenterPoint.Y + FitH / 2);
        ACanvas.Save;
        ACanvas.ClipRect(ThumbRect);
        ACanvas.DrawImageRect(Img, FitRect,
          TSkSamplingOptions.Create(TSkFilterMode.Linear, TSkMipmapMode.None), nil);
        ACanvas.Restore;
        Paint.Style       := TSkPaintStyle.Stroke;
        Paint.StrokeWidth := 1;
        Paint.Color       := FPalette.ChipBd;
        ACanvas.DrawRoundRect(ThumbRect, 3, 3, Paint);
        Paint.Style := TSkPaintStyle.Fill;
      end;
    end;

    if UseBadge then
    begin
      case FChips[I].Attachment.FileKind of
        TChatFileKind.fkImage    : begin Badge := 'IMG'; BadgeBg := $FF2E86C1; end;
        TChatFileKind.fkAudio    : begin Badge := 'AUD'; BadgeBg := $FF1E8449; end;
        TChatFileKind.fkVideo    : begin Badge := 'VID'; BadgeBg := $FFD35400; end;
        TChatFileKind.fkPDF      : begin Badge := 'PDF'; BadgeBg := $FFC0392B; end;
        TChatFileKind.fkDocument : begin Badge := 'DOC'; BadgeBg := $FF7D3C98; end;
      else                         begin Badge := 'ATT'; BadgeBg := $FF707070; end;
      end;
      Font          := TSkFont.Create(TSkTypeface.MakeDefault, 10);
      Font.Embolden := True;
      BadgeRect     := TRectF.Create(CR.Left + 10, CR.Top + 8, CR.Right - 10, CR.Top + 26);
      Paint.Style   := TSkPaintStyle.Fill;
      Paint.Color   := BadgeBg;
      ACanvas.DrawRoundRect(BadgeRect, 4, 4, Paint);
      Paint.Color := CLR_WHITE;
      Font.MeasureText(Badge, TxtBounds, Paint);
      TX := BadgeRect.CenterPoint.X - TxtBounds.Width / 2 - TxtBounds.Left;
      TY := BadgeRect.CenterPoint.Y - (TxtBounds.Top + TxtBounds.Bottom) / 2;
      ACanvas.DrawSimpleText(Badge, TX, TY, Font, Paint);
    end;

    // File name (clipped to bottom strip of chip)
    Font          := TSkFont.Create(TSkTypeface.MakeDefault, 10);
    Font.Embolden := False;
    Paint.Color   := FPalette.Gray;
    ACanvas.Save;
    ACanvas.ClipRect(TRectF.Create(CR.Left + 3, CR.Top + 36, CR.Right - 3, CR.Bottom - 2));
    TX := CR.Left + 4;
    TY := CR.Bottom - 7;
    ACanvas.DrawSimpleText(FChips[I].Attachment.FileName, TX, TY, Font, Paint);
    ACanvas.Restore;

    // Close ×
    if FHovClose = I then CloseBg := FPalette.CloseH
    else CloseBg := FPalette.CloseN;
    PaintCircleBtn(ACanvas, XR, #$00D7, CloseBg, CLR_WHITE, 8);
  end;
end;

function TAIChatInput.MicStateColor: TAlphaColor;
begin
  case FMicState of
    TAIChatVoiceState.vsPreparing : Result := CLR_MIC_PREP;
    TAIChatVoiceState.vsListening : Result := CLR_MIC_LIST;
    TAIChatVoiceState.vsTalking   : Result := CLR_MIC_TALK;
    TAIChatVoiceState.vsAITalking : Result := CLR_MIC_AI;
  else
    Result := CLR_MIC_OFF;
  end;
end;

procedure TAIChatInput.DrawBottomBar(const ACanvas: ISkCanvas);
var
  MenuBg, SendBg, SendFg, MicBg: TAlphaColor;
  SendGlyph    : string;
  OverlayPaint : ISkPaint;
begin
  // + button
  if FPressBtn = TAIChatBtnKind.bkMenu then
    MenuBg := FPalette.BtnPre
  else if FHovBtn = TAIChatBtnKind.bkMenu then
    MenuBg := FPalette.BtnHov
  else
    MenuBg := FPalette.Btn;
  PaintCircleBtn(ACanvas, FBtnMenuRect, '+', MenuBg, FPalette.Gray, 18);

  // Send / Stop button
  if FBusy then
  begin
    SendGlyph := #$25A0;
    SendBg    := IfThen(FPressBtn = TAIChatBtnKind.bkSend, CLR_STOP_PRE, CLR_STOP);
    SendFg    := CLR_WHITE;
  end
  else if SendEnabled then
  begin
    SendGlyph := #$2191;
    if FPressBtn = TAIChatBtnKind.bkSend then SendBg := CLR_SEND_PRE
    else if FHovBtn = TAIChatBtnKind.bkSend then SendBg := CLR_SEND_HOV
    else SendBg := CLR_SEND;
    SendFg := CLR_WHITE;
  end
  else
  begin
    SendGlyph := #$2191;
    SendBg    := FPalette.SendDis;
    SendFg    := FPalette.Gray;
  end;
  PaintCircleBtn(ACanvas, FBtnSendRect, SendGlyph, SendBg, SendFg, 16);

  // Mic button (when MicVisible)
  if FMicVisible and not FBtnMicRect.IsEmpty then
  begin
    MicBg := MicStateColor;

    OverlayPaint           := TSkPaint.Create;
    OverlayPaint.AntiAlias := True;
    OverlayPaint.Style     := TSkPaintStyle.Fill;

    // Base circle (state color)
    OverlayPaint.Color := MicBg;
    ACanvas.DrawOval(FBtnMicRect, OverlayPaint);

    // Hover / press overlay
    if FPressBtn = TAIChatBtnKind.bkMic then
      OverlayPaint.Color := $40000000
    else if FHovBtn = TAIChatBtnKind.bkMic then
      OverlayPaint.Color := $20000000
    else
      OverlayPaint.Color := 0;
    if OverlayPaint.Color <> 0 then
      ACanvas.DrawOval(FBtnMicRect, OverlayPaint);

    // White mic icon on top
    DrawMicIcon(ACanvas, FBtnMicRect);

    // Animated sound bar between + and mic when monitoring
    if FUseSoundMonitor then
      DrawSoundBar(ACanvas);
  end;
end;

procedure TAIChatInput.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
var
  Paint : ISkPaint;
  R     : TRectF;
begin
  ComputeLayout;

  R := ADest;
  R.Inflate(-0.5, -0.5);

  Paint           := TSkPaint.Create;
  Paint.AntiAlias := True;

  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := FPalette.Bg;
  ACanvas.DrawRoundRect(R, ACI_RADIUS, ACI_RADIUS, Paint);

  Paint.Style       := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  Paint.Color       := FPalette.Border;
  ACanvas.DrawRoundRect(R, ACI_RADIUS, ACI_RADIUS, Paint);

  Paint.Color       := FPalette.Sep;
  Paint.StrokeWidth := 1;
  ACanvas.DrawLine(
    TPointF.Create(ADest.Left  + ACI_RADIUS * 0.5, ADest.Bottom - ACI_BOTTOM_H),
    TPointF.Create(ADest.Right - ACI_RADIUS * 0.5, ADest.Bottom - ACI_BOTTOM_H),
    Paint);

  if Length(FChips) > 0 then DrawChips(ACanvas);
  DrawBottomBar(ACanvas);
end;

// ── Mouse ──────────────────────────────────────────────────────────────────────

procedure TAIChatInput.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  Pt: TPointF;
begin
  if Button <> TMouseButton.mbLeft then Exit;
  Pt        := TPointF.Create(X, Y);
  FPressBtn := TAIChatBtnKind.bkNone;
  if FBtnMenuRect.Contains(Pt) and not FBusy then
    FPressBtn := TAIChatBtnKind.bkMenu
  else if FBtnSendRect.Contains(Pt) then
    FPressBtn := TAIChatBtnKind.bkSend
  else if FMicVisible and (not FBtnMicRect.IsEmpty) and FBtnMicRect.Contains(Pt) then
    FPressBtn := TAIChatBtnKind.bkMic;
  if FPressBtn <> TAIChatBtnKind.bkNone then Redraw;
end;

procedure TAIChatInput.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Pt          : TPointF;
  NewHov      : TAIChatBtnKind;
  NewHovClose : Integer;
  I           : Integer;
  Changed     : Boolean;
begin
  Pt          := TPointF.Create(X, Y);
  NewHov      := TAIChatBtnKind.bkNone;
  NewHovClose := -1;

  if FBtnMenuRect.Contains(Pt) and not FBusy then
    NewHov := TAIChatBtnKind.bkMenu
  else if FBtnSendRect.Contains(Pt) then
    NewHov := TAIChatBtnKind.bkSend
  else if FMicVisible and (not FBtnMicRect.IsEmpty) and FBtnMicRect.Contains(Pt) then
    NewHov := TAIChatBtnKind.bkMic;

  if NewHov = TAIChatBtnKind.bkNone then
    for I := 0 to High(FChips) do
      if FChips[I].CloseRect.Contains(Pt) then
      begin
        NewHovClose := I;
        Break;
      end;

  Changed   := (NewHov <> FHovBtn) or (NewHovClose <> FHovClose);
  FHovBtn   := NewHov;
  FHovClose := NewHovClose;
  Cursor    := IfThen((FHovBtn <> TAIChatBtnKind.bkNone) or (FHovClose >= 0),
                      crHandPoint, crDefault);
  if Changed then Redraw;
end;

procedure TAIChatInput.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  Pt        : TPointF;
  Prev      : TAIChatBtnKind;
  I         : Integer;
  NewActive : Boolean;
begin
  if Button <> TMouseButton.mbLeft then Exit;
  Pt        := TPointF.Create(X, Y);
  Prev      := FPressBtn;
  FPressBtn := TAIChatBtnKind.bkNone;

  if (Prev = TAIChatBtnKind.bkMenu) and FBtnMenuRect.Contains(Pt) and not FBusy then
  begin
    if FMenuPanel = nil then ShowMenu else HideMenu;
  end
  else if (Prev = TAIChatBtnKind.bkSend) and FBtnSendRect.Contains(Pt) then
  begin
    if FBusy then
    begin
      if Assigned(FOnCancel) then FOnCancel(Self);
    end
    else
      DoSend;
  end
  else if (Prev = TAIChatBtnKind.bkMic) and FMicVisible and
          (not FBtnMicRect.IsEmpty) and FBtnMicRect.Contains(Pt) then
  begin
    NewActive := not FUseSoundMonitor;
    SetUseSoundMonitor(NewActive);
    if Assigned(FOnMicToggle) then
      FOnMicToggle(Self, NewActive);
  end
  else
  begin
    for I := 0 to High(FChips) do
      if FChips[I].CloseRect.Contains(Pt) then
      begin
        DeleteChip(I);
        Break;
      end;
  end;

  Redraw;
end;

procedure TAIChatInput.HandleMouseLeave(Sender: TObject);
begin
  if (FHovBtn <> TAIChatBtnKind.bkNone) or (FHovClose >= 0) then
  begin
    FHovBtn   := TAIChatBtnKind.bkNone;
    FHovClose := -1;
    Cursor    := crDefault;
    Redraw;
  end;
end;

// ── Timers + Memo ──────────────────────────────────────────────────────────────

procedure TAIChatInput.TimerTick(Sender: TObject);
begin
  FTimer.Enabled := False;
  UpdateLayout;
end;

procedure TAIChatInput.AudioTimerTick(Sender: TObject);
begin
  if FMicVisible and FUseSoundMonitor then
    Redraw;
end;

procedure TAIChatInput.MemoChange(Sender: TObject);
begin
  FTimer.Enabled := False;
  FTimer.Enabled := True;
  Redraw;
end;

procedure TAIChatInput.MemoKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if FEnterAsSend and (Key = vkReturn) and (Shift = []) then
  begin
    Key := 0;
    DoSend;
  end;
end;

// ── Dropdown menu ──────────────────────────────────────────────────────────────

procedure TAIChatInput.ShowMenu;
var
  Form : TCommonCustomForm;
  Pt   : TPointF;
begin
  Form := nil;
  if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
    Form := TCommonCustomForm(Root.GetObject);
  if Form = nil then Exit;
  Pt         := LocalToAbsolute(TPointF.Create(FBtnMenuRect.Left, FBtnMenuRect.Top));
  FMenuPanel := TAIChatMenuPanel.Create(Form, Self, Pt.X, Pt.Y);
  FMenuPanel.Parent := Form;
  FMenuPanel.BringToFront;
end;

procedure TAIChatInput.HideMenu;
var
  P: TAIChatMenuPanel;
begin
  if FMenuPanel = nil then Exit;
  P          := FMenuPanel;
  FMenuPanel := nil;
  P.Parent   := nil;
  P.Free;
  Redraw;
end;

procedure TAIChatInput.ExecuteMenuItem(AIdx: Integer);
begin
  HideMenu;
  case AIdx of
    0: PasteFromClipboard;
    1: UploadFile;
    2: CaptureScreen;
    3: ClearAttachments;
  end;
end;

// ── Send ───────────────────────────────────────────────────────────────────────

procedure TAIChatInput.DoSend;
begin
  if (Trim(FMemo.Text) = '') and (Length(FChips) = 0) then Exit;
  DoSendWithAudio(nil);
end;

procedure TAIChatInput.DoSendWithAudio(AAudio: TMemoryStream);
var
  Prompt : string;
  Atts   : TAIChatAttachments;
  I      : Integer;
begin
  Prompt := Trim(FMemo.Text);
  if (Prompt = '') and (Length(FChips) = 0) and (AAudio = nil) then Exit;

  Atts := TAIChatAttachments.Create(False);
  try
    for I := 0 to High(FChips) do
      Atts.Add(FChips[I].Attachment);
    SetBusy(True);
    if Assigned(FOnSend) then
      FOnSend(Self, Prompt, Atts, AAudio);
  finally
    Atts.Free;
    AAudio.Free;  // safe when nil; receiver must copy if doing async work
  end;

  FMemo.Text := '';
  ClearAttachments;
end;

// ── Chip management ────────────────────────────────────────────────────────────

procedure TAIChatInput.AddChip(AAttachment: TAIChatAttachment);
var
  N: Integer;
begin
  N := Length(FChips);
  SetLength(FChips, N + 1);
  FChips[N].Attachment := AAttachment;
  FChips[N].ChipRect   := TRectF.Empty;
  FChips[N].CloseRect  := TRectF.Empty;
  UpdateLayout;
end;

procedure TAIChatInput.DeleteChip(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= Length(FChips)) then Exit;
  FTempStore.RemoveFile(FChips[AIndex].Attachment.LocalPath);
  FChips[AIndex].Attachment.Free;
  for I := AIndex to Length(FChips) - 2 do
    FChips[I] := FChips[I + 1];
  SetLength(FChips, Length(FChips) - 1);
  if FHovClose >= Length(FChips) then FHovClose := -1;
  UpdateLayout;
end;

// ── Clipboard paste ────────────────────────────────────────────────────────────

procedure TAIChatInput.PasteFromClipboard;
var
  Svc  : IFMXClipboardService;
  Data : TValue;
  Surf : TBitmapSurface;
  Bmp  : FMX.Graphics.TBitmap;
  Txt  : string;
begin
  if not TPlatformServices.Current.SupportsPlatformService(
           IFMXClipboardService, Svc) then Exit;
  Data := Svc.GetClipboard;
  if Data.IsEmpty then Exit;

  if Data.IsType<TBitmapSurface> then
  begin
    Surf := Data.AsType<TBitmapSurface>;
    Bmp  := FMX.Graphics.TBitmap.Create;
    try
      Bmp.Assign(Surf);
      AddBitmapChip(Bmp, 'pasted-image.png');
    finally
      Bmp.Free;
    end;
  end
  else if Data.IsType<string> then
  begin
    Txt := Data.AsType<string>;
    FMemo.Text := FMemo.Text + Txt;
  end;
end;

// ── File upload ────────────────────────────────────────────────────────────────

procedure TAIChatInput.UploadFile;
var
  Dlg: TOpenDialog;
  I  : Integer;
begin
  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.Filter :=
      'All supported|*.jpg;*.jpeg;*.png;*.gif;*.bmp;*.webp;*.pdf;*.doc;*.docx;*.txt' +
      '|Images|*.jpg;*.jpeg;*.png;*.gif;*.bmp;*.webp' +
      '|PDF|*.pdf|Documents|*.doc;*.docx;*.txt|All files|*.*';
    Dlg.Options := Dlg.Options + [TOpenOption.ofAllowMultiSelect];
    if Dlg.Execute then
    begin
      if Dlg.Files.Count > 0 then
        for I := 0 to Dlg.Files.Count - 1 do
          AddAttachment(Dlg.Files[I])
      else if Dlg.FileName <> '' then
        AddAttachment(Dlg.FileName);
    end;
  finally
    Dlg.Free;
  end;
end;

// ── Screen capture ─────────────────────────────────────────────────────────────

procedure TAIChatInput.CaptureScreen;
{$IFDEF MSWINDOWS}
var
  SelRect: TRect;
  Bmp    : FMX.Graphics.TBitmap;
  Form   : TCommonCustomForm;
begin
  Form := nil;
  if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
    Form := TCommonCustomForm(Root.GetObject);
  if Form <> nil then Form.Visible := False;
  try
    Application.ProcessMessages;
    SelRect := TRect.Empty;
    if TScreenCapture.SelectArea(SelRect) then
    begin
      Bmp := TScreenCapture.CaptureArea(SelRect);
      if Bmp <> nil then
      try
        AddBitmapChip(Bmp, 'screenshot.png');
      finally
        Bmp.Free;
      end;
    end;
  finally
    if Form <> nil then Form.Visible := True;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

// ── AddBitmapChip ──────────────────────────────────────────────────────────────

procedure TAIChatInput.AddBitmapChip(ABitmap: FMX.Graphics.TBitmap;
  const AName: string);
var
  G         : TGUID;
  TmpPath   : string;
  StorePath : string;
  Att       : TAIChatAttachment;
begin
  if ABitmap = nil then Exit;
  CreateGUID(G);
  TmpPath := System.IOUtils.TPath.Combine(
    System.IOUtils.TPath.GetTempPath,
    GUIDToString(G).Replace('{','').Replace('}','').Replace('-','') + '.png');
  ABitmap.SaveToFile(TmpPath);
  try
    StorePath := FTempStore.AddFile(TmpPath);
  finally
    if TFile.Exists(TmpPath) then TFile.Delete(TmpPath);
  end;
  if StorePath = '' then Exit;
  Att          := TAIChatAttachment.Create(StorePath);
  Att.FileName := AName;
  Att.FileKind := TChatFileKind.fkImage;
  Att.MimeType := 'image/png';
  if TFile.Exists(StorePath) then
    Att.FileSize := TFile.GetSize(StorePath);
  AddChip(Att);
end;

// ── Voice helpers ──────────────────────────────────────────────────────────────

procedure TAIChatInput.SetThemeDark(AValue: Boolean);
begin
  if FThemeDark = AValue then Exit;
  FThemeDark := AValue;
  if AValue then FPalette := DarkPalette
  else           FPalette := LightPalette;
  if FMemo <> nil then
  begin
    FMemo.StyledSettings := FMemo.StyledSettings - [TStyledSetting.FontColor];
    if AValue then FMemo.TextSettings.FontColor := $FFE0E0E8
    else           FMemo.TextSettings.FontColor := $FF202020;
  end;
  Redraw;
end;

procedure TAIChatInput.SetMicVisible(AValue: Boolean);
begin
  if FMicVisible = AValue then Exit;
  FMicVisible := AValue;
  if not AValue then
  begin
    SetUseSoundMonitor(False);
    FBtnMicRect := TRectF.Empty;
  end;
  ComputeLayout;
  Redraw;
end;

procedure TAIChatInput.SetUseSoundMonitor(AValue: Boolean);
begin
  if FUseSoundMonitor = AValue then Exit;
  FUseSoundMonitor := AValue;
  if not AValue then
  begin
    FMicState     := TAIChatVoiceState.vsOff;
    FSoundLevel   := 0;
    FMaxSoundSeen := 0;
  end
  else
    FMicState := TAIChatVoiceState.vsPreparing;
  FAudioTimer.Enabled := AValue and FMicVisible;
  Redraw;
end;

// ── Voice notifications ────────────────────────────────────────────────────────

procedure TAIChatInput.NotifyVoiceCalibrated;
begin
  FMicState := TAIChatVoiceState.vsListening;
  Redraw;
end;

procedure TAIChatInput.NotifyVoiceChangeState(AUserSpeaking: Boolean);
begin
  if AUserSpeaking then
    FMicState := TAIChatVoiceState.vsTalking
  else
    FMicState := TAIChatVoiceState.vsListening;
  Redraw;
end;

procedure TAIChatInput.NotifyVoiceSpeechEnd(AIsValid: Boolean;
  AAudioStream: TMemoryStream);
var
  AudioCopy: TMemoryStream;
begin
  if AIsValid and (AAudioStream <> nil) and (AAudioStream.Size > 0) then
  begin
    // Stream is valid only during this call — make a copy for the async send
    AudioCopy := TMemoryStream.Create;
    AAudioStream.Position := 0;
    AudioCopy.CopyFrom(AAudioStream, AAudioStream.Size);
    AudioCopy.Position := 0;
    TThread.ForceQueue(nil, procedure begin DoSendWithAudio(AudioCopy); end);
  end;
  FMicState := TAIChatVoiceState.vsListening;
  Redraw;
end;

procedure TAIChatInput.NotifyVoiceUpdate(ASoundLevel: Int64);
begin
  FSoundLevel := ASoundLevel;
  if ASoundLevel > FMaxSoundSeen then
    FMaxSoundSeen := ASoundLevel;
  // AudioTimer redraws at 50 ms — no Redraw call here
end;

procedure TAIChatInput.NotifyVoiceError;
begin
  FMicState           := TAIChatVoiceState.vsOff;
  FUseSoundMonitor    := False;
  FAudioTimer.Enabled := False;
  Redraw;
end;

procedure TAIChatInput.NotifyVoiceAITalking(AIsAITalking: Boolean);
begin
  if AIsAITalking then
    FMicState := TAIChatVoiceState.vsAITalking
  else
    FMicState := TAIChatVoiceState.vsListening;
  Redraw;
end;

// ── Public API ─────────────────────────────────────────────────────────────────

procedure TAIChatInput.AddAttachment(const APath: string);
var
  StorePath, Ext: string;
  Att: TAIChatAttachment;
begin
  if not TFile.Exists(APath) then Exit;
  Ext := System.IOUtils.TPath.GetExtension(APath).ToLower;
  if (FValidExtensions <> '') and not IsExtensionValid(Ext) then Exit;
  StorePath := FTempStore.AddFile(APath);
  if StorePath = '' then Exit;
  Att          := TAIChatAttachment.Create(StorePath);
  Att.FileName := System.IOUtils.TPath.GetFileName(APath);
  Att.FileKind := ExtToFileKind(Ext);
  Att.MimeType := ExtToMime(Ext);
  Att.FileSize := TFile.GetSize(APath);
  AddChip(Att);
end;

procedure TAIChatInput.ClearAttachments;
var
  I: Integer;
begin
  for I := 0 to High(FChips) do
  begin
    FTempStore.RemoveFile(FChips[I].Attachment.LocalPath);
    FChips[I].Attachment.Free;
  end;
  SetLength(FChips, 0);
  FHovClose := -1;
  UpdateLayout;
end;

procedure TAIChatInput.ClearAll;
begin
  FMemo.Text := '';
  ClearAttachments;
end;

procedure TAIChatInput.SetFocus;
begin
  FMemo.SetFocus;
end;

// ── Properties ─────────────────────────────────────────────────────────────────

function TAIChatInput.GetPromptText: string;
begin
  Result := FMemo.Text;
end;

procedure TAIChatInput.SetPromptText(const V: string);
begin
  FMemo.Text := V;
  UpdateLayout;
end;

procedure TAIChatInput.SetBusy(const V: Boolean);
begin
  if FBusy = V then Exit;
  FBusy          := V;
  FMemo.ReadOnly := FBusy;
  Redraw;
end;

function TAIChatInput.LoadThumbnail(const APath: string): ISkImage;
var
  Bytes: TBytes;
begin
  Result := nil;
  if not TFile.Exists(APath) then Exit;
  try
    Bytes  := TFile.ReadAllBytes(APath);
    Result := TSkImage.MakeFromEncoded(Bytes);
  except
    Result := nil;
  end;
end;

function TAIChatInput.HasAttachments: Boolean;
begin
  Result := Length(FChips) > 0;
end;

function TAIChatInput.IsExtensionValid(const AExt: string): Boolean;
var
  Exts: TArray<string>;
  E   : string;
begin
  if FValidExtensions = '' then Exit(True);
  Exts := FValidExtensions.ToLower.Split([';', ',', ' ']);
  for E in Exts do
    if SameText(AExt, E) or SameText(AExt, '.' + E) then
      Exit(True);
  Result := False;
end;

procedure Register;
begin
  RegisterComponents('MakerAI Chat', [TAIChatInput]);
end;

initialization
  RegisterFMXClasses([TAIChatInput]);

end.
