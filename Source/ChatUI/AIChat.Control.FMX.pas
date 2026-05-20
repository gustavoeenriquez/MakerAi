unit AIChat.Control.FMX;

// MIT License — Gustavo Enríquez <gustavoeenriquez@gmail.com>
// TAIChatView : TSkCustomControl
// Single-canvas, virtualized AI chat conversation renderer.
// Drop it on a form, call AddUserMessage / BeginAssistantMessage + AppendToken
// + FinishMessage and it handles everything else.

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.Math,
  System.JSON, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Platform, FMX.Menus,
  System.Skia, FMX.Skia,
  uMakerAi.MD.Types,
  uMakerAi.Core,
  AIChat.Types,
  AIChat.Bubble,
  AIChat.VirtualList;

procedure Register;

type
  TAIChatLinkEvent   = procedure(Sender: TObject; AMsg: TAIChatMessage;
                         const AURL: string) of object;
  TAIChatAttachEvent = procedure(Sender: TObject; AMsg: TAIChatMessage;
                         AAttach: TAIChatAttachment) of object;
  TAIChatMsgEvent    = procedure(Sender: TObject; AMsg: TAIChatMessage) of object;

  // Forward
  TAIChatView = class;

  // Lightweight proxy returned by AddBubble — provides AppendText / AddContent
  // compatible with the old TChatBubble streaming pattern.
  TAIChatBubble = class
  private
    FOwner   : TAIChatView;
    FMsgIndex: Integer;
  public
    constructor Create(AOwner: TAIChatView; AMsgIndex: Integer);
    procedure AppendText(const AFragment: string);
    procedure AddContent(const AText: string; AMediaFiles: TAiMediaFiles = nil);
    property MsgIndex: Integer read FMsgIndex;
  end;

  // Backward-compatible event types (parameter names match old TChatList events)
  TAIChatBubbleEvent    = procedure(Sender: TObject; ABubble: TAIChatBubble) of object;
  TAIChatMediaFileEvent = procedure(Sender: TObject; ABubble: TAIChatBubble;
                            AAttach: TAIChatAttachment) of object;

  TAIChatView = class(TSkCustomControl)
  private
    FList         : TAIChatVirtualList;
    FScrollOffset : Single;
    FAutoScroll   : Boolean;
    FThemeDark    : Boolean;

    // Hover state
    FHoveredMsg    : Integer;
    FHoveredAttach : Integer;

    // Vertical scrollbar drag
    FSBDragging   : Boolean;
    FSBDragStartY : Single;
    FSBDragOffset : Single;

    // Long-press timer for mobile context menu
    FLongPressTimer  : TTimer;
    FLongPressPos    : TPointF;
    FLongPressMsgIdx : Integer;

    // Copy-button feedback
    FCopyFeedbackTimer   : TTimer;
    FCopiedCodeMsgIndex  : Integer;   // for code-block copy (uses renderer feedback)

    // Text selection (spans 0..N assistant messages)
    FSelAnchorMsgIdx : Integer;   // message where drag started (-1 = none)
    FSelStartMsgIdx  : Integer;   // normalized range start  (-1 = none)
    FSelEndMsgIdx    : Integer;   // normalized range end
    FSelDragging     : Boolean;
    FSelAnchorRX     : Single;
    FSelAnchorRY     : Single;
    FSelAnchorEntry  : Integer;
    FSelAnchorChar   : Integer;

    // Context menu
    FContextMenu     : TPopupMenu;
    FCtxCopyMsg      : TMenuItem;
    FCtxCopyAll      : TMenuItem;
    FCtxOpenAttach   : TMenuItem;
    FCtxTargetMsg     : Integer;
    FCtxTargetAttach  : Integer;
    FCtxCopySelection : TMenuItem;

    // Events
    FOnLinkClick        : TAIChatLinkEvent;
    FOnAttachOpen       : TAIChatAttachEvent;
    FOnMessageCopy      : TAIChatMsgEvent;
    FOnConversationCopy : TNotifyEvent;

    // Backward-compat events
    FOnBubbleAvatarClick     : TAIChatBubbleEvent;
    FOnBubbleMoreOptionsClick: TAIChatBubbleEvent;
    FOnMediaFileDblClick     : TAIChatMediaFileEvent;

    // Backward-compat appearance
    FInboundColor       : TAlphaColor;
    FOutboundColor      : TAlphaColor;
    FAttachmentPopupMenu: TPopupMenu;

    // Proxy lifetime management
    FBubbleProxies: TObjectList<TAIChatBubble>;

    // Internal helpers
    procedure ClampScroll;
    function  DocYFromMouseY(AMouseY: Single): Single; inline;
    function  MaxScrollVal: Single; inline;
    procedure GetThumbRect(out ARect: TRectF);
    procedure DrawScrollbar(ACanvas: ISkCanvas);

    procedure OnListChanged(Sender: TObject);
    procedure ContextMenuClick(Sender: TObject);
    procedure LongPressTimerFired(Sender: TObject);
    procedure ShowContextAt(AMsgIdx, AAttachIdx: Integer; X, Y: Single);
    procedure CopyFeedbackTimerFired(Sender: TObject);

    function  RendererY(AMsgIdx: Integer; ADocY: Single): Single;
    function  RendererX(AMsgIdx: Integer; ADocX: Single): Single;
    procedure ClearSelectionOf(AMsgIdx: Integer);
    procedure ClearAllSelections;
    function  GetMultiSelectionText: string;
    function  HasActiveSelection: Boolean;

    procedure SetThemeDark(AValue: Boolean);
    procedure SetPersistAttachments(AValue: Boolean);
    function  GetMessageCount: Integer;
    function  GetPersistAttachments: Boolean;
    procedure SetInboundColor(AValue: TAlphaColor);
    procedure SetOutboundColor(AValue: TAlphaColor);

    procedure DoCopyMessage(AMsg: TAIChatMessage);
    procedure DoCopyConversation;
    procedure DoClipboardSet(const AText: string);

  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
                   const AOpacity: Single); override;
    procedure Resize; override;
    procedure MouseWheel(AShift: TShiftState; AWheelDelta: Integer;
                         var AHandled: Boolean); override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState;
                        X, Y: Single); override;
    procedure MouseMove(AShift: TShiftState; X, Y: Single); override;
    procedure MouseUp  (AButton: TMouseButton; AShift: TShiftState;
                        X, Y: Single); override;
    procedure KeyDown  (var AKey: Word; var AKeyChar: WideChar;
                        AShift: TShiftState); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    // ── Message API ──────────────────────────────────────────────────────────
    function  AddMessage(ARole: TChatRole; const AText: string): TAIChatMessage;
    function  AddUserMessage(const AText: string): TAIChatMessage; overload;
    function  AddUserMessage(const AText: string;
                AMediaFiles: TAiMediaFiles): TAIChatMessage; overload;
    function  BeginAssistantMessage: TAIChatMessage;  // status = msStreaming
    procedure AppendToken(AMsgIndex: Integer; const AToken: string);
    procedure FinishMessage(AMsgIndex: Integer);
    procedure ClearMessages;

    // ── Navigation ───────────────────────────────────────────────────────────
    procedure ScrollBy(ADelta: Single);
    procedure ScrollToTop;
    procedure ScrollToBottom;
    procedure RefreshLayout;

    // ── Clipboard ────────────────────────────────────────────────────────────
    procedure CopyMessage(AMsgIndex: Integer);
    procedure CopyConversation;

    // ── Serialization ────────────────────────────────────────────────────────
    function  SaveToJSON: TJSONArray;
    procedure LoadFromJSON(AArr: TJSONArray);
    // Backward-compatible stream serialization (wraps JSON internally)
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);

    // ── Backward-compatible message API ──────────────────────────────────────
    // Returns a TAIChatBubble proxy — use AppendText on it for streaming.
    // For streaming assistant turns, call FinishMessage(Bubble.MsgIndex) when done.
    function  AddBubble(const AText: string; const AUserName: string;
                AMediaFiles: TAiMediaFiles; AInbound: Boolean = True): TAIChatBubble;
    procedure Clear;  // alias for ClearMessages

    property VirtualList  : TAIChatVirtualList read FList;
    property MessageCount : Integer            read GetMessageCount;
    property ScrollOffset : Single             read FScrollOffset;

  published
    property ThemeDark          : Boolean      read FThemeDark           write SetThemeDark          default False;
    property PersistAttachments : Boolean      read GetPersistAttachments write SetPersistAttachments default False;
    property AutoScroll         : Boolean      read FAutoScroll           write FAutoScroll           default True;
    property InboundColor       : TAlphaColor  read FInboundColor         write SetInboundColor       default TAlphaColors.LightGray;
    property OutboundColor      : TAlphaColor  read FOutboundColor        write SetOutboundColor      default TAlphaColors.LightGreen;
    property AttachmentPopupMenu: TPopupMenu   read FAttachmentPopupMenu  write FAttachmentPopupMenu;

    property OnLinkClick             : TAIChatLinkEvent     read FOnLinkClick             write FOnLinkClick;
    property OnAttachmentOpen        : TAIChatAttachEvent   read FOnAttachOpen            write FOnAttachOpen;
    property OnMessageCopy           : TAIChatMsgEvent      read FOnMessageCopy           write FOnMessageCopy;
    property OnConversationCopy      : TNotifyEvent         read FOnConversationCopy      write FOnConversationCopy;
    // Backward-compat events
    property OnMediaFileDblClick     : TAIChatMediaFileEvent read FOnMediaFileDblClick     write FOnMediaFileDblClick;
    property OnBubbleAvatarClick     : TAIChatBubbleEvent   read FOnBubbleAvatarClick     write FOnBubbleAvatarClick;
    property OnBubbleMoreOptionsClick: TAIChatBubbleEvent   read FOnBubbleMoreOptionsClick write FOnBubbleMoreOptionsClick;

    property Align;
    property Anchors;
    property Padding;
    property Margins;
    property Visible;
    property Opacity;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnResize;
  end;

implementation

uses
  System.IOUtils,
  FMX.Forms,
  uMakerAi.MD.Renderer.Skia
  {$IFDEF MSWINDOWS}
  , Winapi.Windows
  {$ENDIF}
  ;

const
  SB_W          = 6;
  SB_PAD        = 2;
  SB_R          = 3;
  CHAT_RADIUS   = 16;   // rounded-corner radius, matches TAIChatInput

procedure Register;
begin
  RegisterComponents('MakerAI Chat', [TAIChatView]);
end;

{ ── Constructor / Destructor ──────────────────────────────────────────────── }

constructor TAIChatView.Create(AOwner: TComponent);
var
  Theme: TAIChatTheme;
begin
  inherited;
  FThemeDark    := False;
  FScrollOffset := 0;
  FAutoScroll   := True;
  FHoveredMsg   := -1;
  FHoveredAttach:= -1;
  FSBDragging   := False;
  FCtxTargetMsg := -1;
  FCtxTargetAttach := -1;

  Theme := TAIChatTheme.Default;
  FList := TAIChatVirtualList.Create(Theme);
  FList.OnLayoutChanged := OnListChanged;

  DrawCacheKind := TSkDrawCacheKind.Never;
  HitTest       := True;
  CanFocus      := True;
  AutoCapture   := True;

  FCopiedCodeMsgIndex := -1;
  FSelAnchorMsgIdx    := -1;
  FSelStartMsgIdx     := -1;
  FSelEndMsgIdx       := -1;
  FSelDragging        := False;

  // Copy-button feedback timer (1 second)
  FCopyFeedbackTimer          := TTimer.Create(Self);
  FCopyFeedbackTimer.Interval := 1000;
  FCopyFeedbackTimer.Enabled  := False;
  FCopyFeedbackTimer.OnTimer  := CopyFeedbackTimerFired;

  // Long-press timer for mobile context-menu trigger
  FLongPressTimer          := TTimer.Create(Self);
  FLongPressTimer.Interval := 500;
  FLongPressTimer.Enabled  := False;
  FLongPressTimer.OnTimer  := LongPressTimerFired;

  // Context menu
  FContextMenu := TPopupMenu.Create(Self);

  FCtxCopyMsg         := TMenuItem.Create(FContextMenu);
  FCtxCopyMsg.Text    := 'Copy Message';
  FCtxCopyMsg.OnClick := ContextMenuClick;
  FContextMenu.AddObject(FCtxCopyMsg);

  FCtxCopyAll         := TMenuItem.Create(FContextMenu);
  FCtxCopyAll.Text    := 'Copy Conversation';
  FCtxCopyAll.OnClick := ContextMenuClick;
  FContextMenu.AddObject(FCtxCopyAll);

  FCtxOpenAttach         := TMenuItem.Create(FContextMenu);
  FCtxOpenAttach.Text    := 'Open Attachment';
  FCtxOpenAttach.OnClick := ContextMenuClick;
  FContextMenu.AddObject(FCtxOpenAttach);

  FCtxCopySelection         := TMenuItem.Create(FContextMenu);
  FCtxCopySelection.Text    := 'Copy Selection';
  FCtxCopySelection.OnClick := ContextMenuClick;
  FContextMenu.AddObject(FCtxCopySelection);

  FBubbleProxies := TObjectList<TAIChatBubble>.Create(True);
  FInboundColor  := TAlphaColors.LightGray;
  FOutboundColor := TAlphaColors.LightGreen;
end;

destructor TAIChatView.Destroy;
begin
  FBubbleProxies.Free;
  FLongPressTimer.Free;
  FList.Free;
  inherited;
end;

{ ── Scroll helpers ────────────────────────────────────────────────────────── }

function TAIChatView.DocYFromMouseY(AMouseY: Single): Single;
begin
  Result := AMouseY + FScrollOffset;
end;

function TAIChatView.MaxScrollVal: Single;
begin
  Result := Max(0, FList.TotalHeight - Height);
end;

procedure TAIChatView.ClampScroll;
begin
  FScrollOffset := Max(0, Min(FScrollOffset, MaxScrollVal));
end;

procedure TAIChatView.GetThumbRect(out ARect: TRectF);
var
  TrackH, ThumbH, ScrollRange, ThumbTop: Single;
begin
  ARect := TRectF.Empty;
  if (FList.TotalHeight <= Height) or (Height <= 0) then Exit;
  TrackH      := Height - SB_PAD * 2;
  ThumbH      := Max(20, TrackH * TrackH / FList.TotalHeight);
  ScrollRange := FList.TotalHeight - Height;
  ThumbTop    := FScrollOffset / Max(1, ScrollRange) * (TrackH - ThumbH) + SB_PAD;
  ARect := TRectF.Create(
    Width - SB_W - SB_PAD, ThumbTop,
    Width - SB_PAD,         ThumbTop + ThumbH);
end;

procedure TAIChatView.DrawScrollbar(ACanvas: ISkCanvas);
var
  ThumbRect : TRectF;
  SbPaint   : ISkPaint;
begin
  GetThumbRect(ThumbRect);
  if ThumbRect.IsEmpty then Exit;
  SbPaint           := TSkPaint.Create;
  SbPaint.AntiAlias := True;
  SbPaint.Style     := TSkPaintStyle.Fill;
  SbPaint.Color     := $60808090;
  ACanvas.DrawRoundRect(ThumbRect, SB_R, SB_R, SbPaint);
end;

{ ── Layout change callback ───────────────────────────────────────────────── }

procedure TAIChatView.OnListChanged(Sender: TObject);
begin
  ClampScroll;
  if FAutoScroll then
    FScrollOffset := MaxScrollVal;
  Redraw;
end;

{ ── Context menu ─────────────────────────────────────────────────────────── }

procedure TAIChatView.LongPressTimerFired(Sender: TObject);
var
  HMsg     : TAIChatMessage;
  HHit     : TAIChatHitArea;
  AttachIdx: Integer;
begin
  FLongPressTimer.Enabled := False;
  AttachIdx := -1;
  if FList.HitTest(FLongPressPos.X, DocYFromMouseY(FLongPressPos.Y), HMsg, HHit) and
     (HHit.Kind = TChatHitKind.hkAttachment) then
    AttachIdx := HHit.AttachIndex;
  ShowContextAt(FLongPressMsgIdx, AttachIdx, FLongPressPos.X, FLongPressPos.Y);
end;

procedure TAIChatView.CopyFeedbackTimerFired(Sender: TObject);
var
  Rdr: TAITextMDRenderer;
begin
  FCopyFeedbackTimer.Enabled := False;
  // Clear message-level "copied" badge
  FList.SetCopied(-1);
  // Clear code-block copy feedback in the renderer
  if FCopiedCodeMsgIndex >= 0 then
  begin
    Rdr := FList.GetRenderer(FCopiedCodeMsgIndex);
    if Rdr <> nil then Rdr.ClearCopyFeedback;
    FCopiedCodeMsgIndex := -1;
  end;
  Redraw;
end;

{ ── Selection helpers ────────────────────────────────────────────────────── }

function TAIChatView.RendererY(AMsgIdx: Integer; ADocY: Single): Single;
begin
  Result := (ADocY - FList.GetMsgDocY(AMsgIdx))
            - FList.Theme.BubblePadding
            + MD_OUTER_PADDING;
end;

function TAIChatView.RendererX(AMsgIdx: Integer; ADocX: Single): Single;
begin
  Result := ADocX
            - FList.Painter.ContentLeftFor(FList.Messages[AMsgIdx], Width)
            + MD_OUTER_PADDING;
end;

procedure TAIChatView.ClearSelectionOf(AMsgIdx: Integer);
var
  Rdr: TAITextMDRenderer;
begin
  if AMsgIdx < 0 then Exit;
  Rdr := FList.GetRenderer(AMsgIdx);
  if Rdr <> nil then Rdr.ClearSelection;
end;

procedure TAIChatView.ClearAllSelections;
var i: Integer;
begin
  if FSelStartMsgIdx >= 0 then
    for i := FSelStartMsgIdx to FSelEndMsgIdx do
      ClearSelectionOf(i);
  FSelAnchorMsgIdx := -1;
  FSelStartMsgIdx  := -1;
  FSelEndMsgIdx    := -1;
end;

function TAIChatView.GetMultiSelectionText: string;
var
  SB  : TStringBuilder;
  i   : Integer;
  Rdr : TAITextMDRenderer;
  Txt : string;
begin
  Result := '';
  if FSelStartMsgIdx < 0 then Exit;
  SB := TStringBuilder.Create;
  try
    for i := FSelStartMsgIdx to FSelEndMsgIdx do
    begin
      if i >= FList.Messages.Count then Break;
      var Msg := FList.Messages[i];
      if Msg.Role = TChatRole.crAssistant then
      begin
        Rdr := FList.GetRenderer(i);
        if (Rdr <> nil) and Rdr.HasSelection then
        begin
          Txt := Rdr.GetSelectedText;
          if Txt <> '' then
          begin
            if SB.Length > 0 then SB.AppendLine;
            SB.Append(Txt);
          end;
        end;
      end
      else
      begin
        if Msg.Text <> '' then
        begin
          if SB.Length > 0 then SB.AppendLine;
          SB.Append(Msg.Text);
        end;
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TAIChatView.HasActiveSelection: Boolean;
begin
  Result := FSelStartMsgIdx >= 0;
end;

procedure TAIChatView.ShowContextAt(AMsgIdx, AAttachIdx: Integer; X, Y: Single);
var
  AbsPt  : TPointF;
  Obj    : TFmxObject;
  F      : TCommonCustomForm;
begin
  FCtxTargetMsg    := AMsgIdx;
  FCtxTargetAttach := AAttachIdx;
  FCtxCopyMsg.Enabled       := AMsgIdx >= 0;
  FCtxOpenAttach.Enabled    := (AMsgIdx >= 0) and (AAttachIdx >= 0) and
                               Assigned(FOnAttachOpen);
  FCtxCopySelection.Enabled := HasActiveSelection;
  FContextMenu.PopupComponent := Self;
  AbsPt := LocalToAbsolute(TPointF.Create(X, Y));
  // Walk the parent chain to find the owning form for screen-coord conversion
  F   := nil;
  Obj := Parent;
  while Obj <> nil do
  begin
    if Obj is TCommonCustomForm then
    begin
      F := TCommonCustomForm(Obj);
      Break;
    end;
    Obj := Obj.Parent;
  end;
  if F <> nil then
    FContextMenu.Popup(Round(AbsPt.X + F.Left), Round(AbsPt.Y + F.Top))
  else
    FContextMenu.Popup(Round(AbsPt.X), Round(AbsPt.Y));
end;

procedure TAIChatView.ContextMenuClick(Sender: TObject);
var
  Msg: TAIChatMessage;
begin
  if Sender = FCtxCopyMsg then
  begin
    if (FCtxTargetMsg >= 0) and (FCtxTargetMsg < FList.Messages.Count) then
      DoCopyMessage(FList.Messages[FCtxTargetMsg]);
  end
  else if Sender = FCtxCopyAll then
    DoCopyConversation
  else if Sender = FCtxCopySelection then
  begin
    var SelTxt := GetMultiSelectionText;
    if SelTxt <> '' then DoClipboardSet(SelTxt);
  end
  else if Sender = FCtxOpenAttach then
  begin
    if (FCtxTargetMsg >= 0) and (FCtxTargetMsg < FList.Messages.Count) and
       Assigned(FOnAttachOpen) then
    begin
      Msg := FList.Messages[FCtxTargetMsg];
      if (FCtxTargetAttach >= 0) and (FCtxTargetAttach < Msg.Attachments.Count) then
        FOnAttachOpen(Self, Msg, Msg.Attachments[FCtxTargetAttach]);
    end;
  end;
end;

{ ── Clipboard ────────────────────────────────────────────────────────────── }

procedure TAIChatView.DoClipboardSet(const AText: string);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
       IFMXClipboardService, IInterface(Svc)) then
    Svc.SetClipboard(AText);
end;

procedure TAIChatView.DoCopyMessage(AMsg: TAIChatMessage);
begin
  DoClipboardSet(AMsg.Text);
  if Assigned(FOnMessageCopy) then FOnMessageCopy(Self, AMsg);
end;

procedure TAIChatView.DoCopyConversation;
var
  SB  : TStringBuilder;
  Msg : TAIChatMessage;
  Role: string;
begin
  SB := TStringBuilder.Create;
  try
    for Msg in FList.Messages do
    begin
      case Msg.Role of
        TChatRole.crUser     : Role := 'User';
        TChatRole.crAssistant: Role := 'Assistant';
      else                     Role := 'System';
      end;
      SB.Append(Role).Append(': ').AppendLine(Msg.Text).AppendLine;
    end;
    DoClipboardSet(SB.ToString);
  finally
    SB.Free;
  end;
  if Assigned(FOnConversationCopy) then FOnConversationCopy(Self);
end;

{ ── Theme ────────────────────────────────────────────────────────────────── }

procedure TAIChatView.SetThemeDark(AValue: Boolean);
begin
  if FThemeDark = AValue then Exit;
  FThemeDark := AValue;
  if AValue then
    FList.SetTheme(TAIChatTheme.Dark)
  else
    FList.SetTheme(TAIChatTheme.Default);
  Redraw;
end;

function TAIChatView.GetMessageCount: Integer;
begin
  Result := FList.Messages.Count;
end;

function TAIChatView.GetPersistAttachments: Boolean;
begin
  Result := FList.PersistAttachments;
end;

procedure TAIChatView.SetPersistAttachments(AValue: Boolean);
begin
  FList.PersistAttachments := AValue;
end;

{ ── Draw ─────────────────────────────────────────────────────────────────── }

procedure TAIChatView.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
var
  BgPaint: ISkPaint;
  LocalR : TRectF;
begin
  ACanvas.Save;
  ACanvas.ClipRect(ADest);
  ACanvas.Translate(ADest.Left, ADest.Top);

  LocalR := TRectF.Create(0, 0, ADest.Width, ADest.Height);
  LocalR.Inflate(-0.5, -0.5);

  BgPaint           := TSkPaint.Create;
  BgPaint.AntiAlias := True;
  BgPaint.Style     := TSkPaintStyle.Fill;
  BgPaint.Color     := FList.Theme.MD.BgColor;
  if TAlphaColorRec(BgPaint.Color).A > 0 then
    ACanvas.DrawRoundRect(LocalR, CHAT_RADIUS, CHAT_RADIUS, BgPaint);

  BgPaint.Style       := TSkPaintStyle.Stroke;
  BgPaint.StrokeWidth := 1;
  BgPaint.Color       := FList.Theme.ViewBorderColor;
  ACanvas.DrawRoundRect(LocalR, CHAT_RADIUS, CHAT_RADIUS, BgPaint);

  // Messages via virtual list (all coords relative to component top-left)
  FList.Paint(ACanvas, FScrollOffset, ADest.Height, FSelStartMsgIdx, FSelEndMsgIdx);

  // Vertical scrollbar thumb
  DrawScrollbar(ACanvas);

  ACanvas.Restore;
end;

{ ── Resize ───────────────────────────────────────────────────────────────── }

procedure TAIChatView.Resize;
begin
  inherited;
  if Width > 0 then
    FList.SetViewWidth(Width);
  ClampScroll;
end;

{ ── Mouse / keyboard ─────────────────────────────────────────────────────── }

procedure TAIChatView.MouseWheel(AShift: TShiftState; AWheelDelta: Integer;
  var AHandled: Boolean);
begin
  FScrollOffset := FScrollOffset - AWheelDelta * 0.5;
  ClampScroll;
  FAutoScroll := FScrollOffset >= MaxScrollVal - 2;
  AHandled    := True;
  Redraw;
end;

procedure TAIChatView.MouseDown(AButton: TMouseButton; AShift: TShiftState;
  X, Y: Single);
var
  DocX, DocY: Single;
  Msg       : TAIChatMessage;
  Hit       : TAIChatHitArea;
  ThumbRect : TRectF;
  Idx       : Integer;
begin
  inherited;

  // Scrollbar thumb drag takes priority
  GetThumbRect(ThumbRect);
  if (not ThumbRect.IsEmpty) and ThumbRect.Contains(TPointF.Create(X, Y)) then
  begin
    FSBDragging   := True;
    FSBDragStartY := Y;
    FSBDragOffset := FScrollOffset;
    Exit;
  end;

  DocX := X;
  DocY := DocYFromMouseY(Y);

  if AButton = TMouseButton.mbLeft then
  begin
    if FList.HitTest(DocX, DocY, Msg, Hit) then
    begin
      case Hit.Kind of
        TChatHitKind.hkCopyButton:
        begin
          DoCopyMessage(Msg);
          // Show "copied" feedback on the message-level copy button
          FList.SetCopied(FList.HitTestIndex(DocX, DocY));
          FCopyFeedbackTimer.Enabled := False;
          FCopyFeedbackTimer.Enabled := True;
          Redraw;
        end;
        TChatHitKind.hkAttachment:
          if Assigned(FOnAttachOpen) and
             (Hit.AttachIndex < Msg.Attachments.Count) then
            FOnAttachOpen(Self, Msg, Msg.Attachments[Hit.AttachIndex]);
        TChatHitKind.hkLink:
          if Assigned(FOnLinkClick) then
            FOnLinkClick(Self, Msg, Hit.URL);
      else
        // hkText — check code-block copy first, then start text selection drag
        begin
          var MsgIdx  := FList.HitTestIndex(DocX, DocY);
          var CodeHit := False;
          if MsgIdx >= 0 then
          begin
            var Rdr := FList.GetRenderer(MsgIdx);
            if Rdr <> nil then
            begin
              var CodeStr: string;
              if FList.Painter.HitCodeCopyAt(DocX, DocY,
                   FList.GetMsgDocY(MsgIdx), Width,
                   FList.Messages[MsgIdx], Rdr, CodeStr) then
              begin
                DoClipboardSet(CodeStr);
                Rdr.StartCopyFeedback(CodeStr);
                FCopiedCodeMsgIndex        := MsgIdx;
                FCopyFeedbackTimer.Enabled := False;
                FCopyFeedbackTimer.Enabled := True;
                Redraw;
                CodeHit := True;
              end;
            end;
          end;

          if not CodeHit then
          begin
            // Clear previous selection
            ClearAllSelections;

            // Begin selection drag on any message
            if MsgIdx >= 0 then
            begin
              FSelAnchorMsgIdx := MsgIdx;
              FSelStartMsgIdx  := MsgIdx;
              FSelEndMsgIdx    := MsgIdx;
              var SelRdr := FList.GetRenderer(MsgIdx);
              if SelRdr <> nil then
              begin
                FSelAnchorRX    := RendererX(MsgIdx, DocX);
                FSelAnchorRY    := RendererY(MsgIdx, DocY);
                FSelAnchorEntry := SelRdr.EntryAtDocY(FSelAnchorRY);
                FSelAnchorChar  := SelRdr.GetCharAtPoint(FSelAnchorEntry,
                                     FSelAnchorRX, FSelAnchorRY);
              end
              else
              begin
                FSelAnchorRX    := 0;
                FSelAnchorRY    := 0;
                FSelAnchorEntry := -1;
                FSelAnchorChar  := -1;
              end;
              FSelDragging := True;
            end;
            Redraw;
          end;
        end;
      end;
    end;

    // Start long-press countdown (fires context menu on mobile)
    Idx := FList.HitTestIndex(DocX, DocY);
    if Idx >= 0 then
    begin
      FLongPressMsgIdx        := Idx;
      FLongPressPos           := TPointF.Create(X, Y);
      FLongPressTimer.Enabled := True;
    end;
  end
  else if AButton = TMouseButton.mbRight then
  begin
    Idx := FList.HitTestIndex(DocX, DocY);
    var AttachIdx: Integer := -1;
    var HMsg: TAIChatMessage;
    var HHit: TAIChatHitArea;
    if FList.HitTest(DocX, DocY, HMsg, HHit) and
       (HHit.Kind = TChatHitKind.hkAttachment) then
      AttachIdx := HHit.AttachIndex;
    ShowContextAt(Idx, AttachIdx, X, Y);
  end;
end;

procedure TAIChatView.MouseMove(AShift: TShiftState; X, Y: Single);
var
  DocX, DocY  : Single;
  Idx, OldHov : Integer;
  Msg         : TAIChatMessage;
  Hit         : TAIChatHitArea;
  Dist        : Single;
  Delta       : Single;
  TrackH      : Single;
  ThumbH      : Single;
  Scale       : Single;
begin
  inherited;

  // Cancel long-press if finger/mouse drifted
  if FLongPressTimer.Enabled then
  begin
    Dist := Sqrt(Sqr(X - FLongPressPos.X) + Sqr(Y - FLongPressPos.Y));
    if Dist > 8 then
      FLongPressTimer.Enabled := False;
  end;

  // Scrollbar drag
  if FSBDragging then
  begin
    Delta  := Y - FSBDragStartY;
    TrackH := Height - SB_PAD * 2;
    ThumbH := Max(20, TrackH * TrackH / Max(1, FList.TotalHeight));
    Scale  := (FList.TotalHeight - Height) / Max(1, TrackH - ThumbH);
    FScrollOffset := FSBDragOffset + Delta * Scale;
    ClampScroll;
    FAutoScroll := FScrollOffset >= MaxScrollVal - 2;
    Redraw;
    Exit;
  end;

  // Update hover state
  DocX   := X;
  DocY   := DocYFromMouseY(Y);
  OldHov := FHoveredMsg;

  // Selection drag — update char-level (possibly multi-bubble) selection
  if FSelDragging and (FSelAnchorMsgIdx >= 0) then
  begin
    var CurMsgIdx := FList.HitTestIndex(DocX, DocY);
    if CurMsgIdx < 0 then
      CurMsgIdx := FSelAnchorMsgIdx;

    // Normalize range
    var NStart, NEnd: Integer;
    var AnchorIsStart: Boolean;
    if CurMsgIdx >= FSelAnchorMsgIdx then
    begin
      NStart        := FSelAnchorMsgIdx;
      NEnd          := CurMsgIdx;
      AnchorIsStart := True;
    end
    else
    begin
      NStart        := CurMsgIdx;
      NEnd          := FSelAnchorMsgIdx;
      AnchorIsStart := False;
    end;

    // Clear renderers that fell outside the new range
    if FSelStartMsgIdx >= 0 then
      for var OI := FSelStartMsgIdx to FSelEndMsgIdx do
        if (OI < NStart) or (OI > NEnd) then
          ClearSelectionOf(OI);
    FSelStartMsgIdx := NStart;
    FSelEndMsgIdx   := NEnd;

    // Compute focus entry/char in CurMsgIdx (if assistant)
    var FocusEntry := 0;
    var FocusChar  := 0;
    if CurMsgIdx >= 0 then
    begin
      var FocusRdr := FList.GetRenderer(CurMsgIdx);
      if FocusRdr <> nil then
      begin
        var FocusRX := RendererX(CurMsgIdx, DocX);
        var FocusRY := RendererY(CurMsgIdx, DocY);
        FocusEntry  := FocusRdr.EntryAtDocY(FocusRY);
        FocusChar   := FocusRdr.GetCharAtPoint(FocusEntry, FocusRX, FocusRY);
      end;
    end;

    // Apply selection to each message in range
    for var SI := NStart to NEnd do
    begin
      if SI >= FList.Messages.Count then Break;
      if FList.Messages[SI].Role <> TChatRole.crAssistant then Continue;
      var Rdr := FList.GetRenderer(SI);
      if Rdr = nil then Continue;
      var EC := Rdr.LayoutEntryCount;
      if EC = 0 then Continue;

      if NStart = NEnd then
      begin
        // Single bubble
        if FSelAnchorEntry >= 0 then
          Rdr.SetCharSelection(FSelAnchorEntry, FSelAnchorChar,
                               FocusEntry, FocusChar)
        else
          Rdr.ClearSelection;
      end
      else if SI = NStart then
      begin
        // First message in range: select from start point to last entry
        var SE := IfThen(AnchorIsStart, FSelAnchorEntry, FocusEntry);
        var SC := IfThen(AnchorIsStart, FSelAnchorChar,  FocusChar);
        Rdr.SetCharSelection(SE, SC, EC - 1, MaxInt);
      end
      else if SI = NEnd then
      begin
        // Last message in range: select from first entry to end point
        var EE := IfThen(AnchorIsStart, FocusEntry, FSelAnchorEntry);
        var ECh := IfThen(AnchorIsStart, FocusChar,  FSelAnchorChar);
        Rdr.SetCharSelection(0, 0, EE, ECh);
      end
      else
        // Middle message: select everything
        Rdr.SetCharSelection(0, 0, EC - 1, MaxInt);
    end;

    Cursor := crIBeam;
    Redraw;
    Exit;
  end;

  Idx            := FList.HitTestIndex(DocX, DocY);
  FHoveredMsg    := Idx;
  FHoveredAttach := -1;

  if Idx >= 0 then
  begin
    if FList.HitTest(DocX, DocY, Msg, Hit) then
    begin
      if Hit.Kind = TChatHitKind.hkAttachment then
        FHoveredAttach := Hit.AttachIndex;
      if Hit.Kind in [TChatHitKind.hkCopyButton, TChatHitKind.hkAttachment,
                      TChatHitKind.hkLink] then
        Cursor := crHandPoint
      else if Hit.Kind = TChatHitKind.hkText then
        Cursor := crIBeam
      else
        Cursor := crDefault;
    end
    else
      Cursor := crDefault;
  end
  else
    Cursor := crDefault;

  FList.SetHovered(FHoveredMsg, FHoveredAttach);

  // Track hover over the message-level copy button for hover animation
  var NewHovCopy: Integer := -1;
  if (Idx >= 0) and (Idx = FHoveredMsg) then
  begin
    var Areas := FList.Messages[Idx].HitAreas;
    var HI: Integer;
    for HI := 0 to High(Areas) do
      if (Areas[HI].Kind = TChatHitKind.hkCopyButton) and
         Areas[HI].DocRect.Contains(TPointF.Create(DocX, DocY)) then
      begin
        NewHovCopy := Idx;
        Break;
      end;
  end;
  if NewHovCopy <> FList.HoveredCopyBtn then
  begin
    FList.SetHoverCopyBtn(NewHovCopy);
    Redraw;
  end;

  if FHoveredMsg <> OldHov then
    Redraw;
end;

procedure TAIChatView.MouseUp(AButton: TMouseButton; AShift: TShiftState;
  X, Y: Single);
begin
  inherited;
  FSBDragging             := False;
  FLongPressTimer.Enabled := False;

  // End selection drag; clear if it was a zero-length click
  if FSelDragging then
  begin
    FSelDragging := False;
    if (FSelAnchorMsgIdx >= 0) and (FSelStartMsgIdx = FSelEndMsgIdx) then
    begin
      var Rdr := FList.GetRenderer(FSelStartMsgIdx);
      if Rdr <> nil then
      begin
        var FocusRX    := RendererX(FSelStartMsgIdx, X);
        var FocusRY    := RendererY(FSelStartMsgIdx, DocYFromMouseY(Y));
        var FocusEntry := Rdr.EntryAtDocY(FocusRY);
        var FocusChar  := Rdr.GetCharAtPoint(FocusEntry, FocusRX, FocusRY);
        if (FocusEntry = FSelAnchorEntry) and (FocusChar = FSelAnchorChar) then
        begin
          ClearAllSelections;
          Redraw;
        end;
      end;
    end;
  end;
end;

procedure TAIChatView.KeyDown(var AKey: Word; var AKeyChar: WideChar;
  AShift: TShiftState);
begin
  // Ctrl+C — copy selected text (if selection active) or fall through
  if (ssCtrl in AShift) and (AKey = Ord('C')) then
  begin
    if FSelStartMsgIdx >= 0 then
    begin
      var SelTxt := GetMultiSelectionText;
      if SelTxt <> '' then
      begin
        DoClipboardSet(SelTxt);
        AKey := 0;
        Exit;
      end;
    end;
  end;

  // Ctrl+A — select all text in the hovered assistant message
  if (ssCtrl in AShift) and (AKey = Ord('A')) then
  begin
    if (FHoveredMsg >= 0) and
       (FList.Messages[FHoveredMsg].Role = TChatRole.crAssistant) then
    begin
      var SelRdr := FList.GetRenderer(FHoveredMsg);
      if SelRdr <> nil then
      begin
        ClearAllSelections;
        FSelAnchorMsgIdx := FHoveredMsg;
        FSelStartMsgIdx  := FHoveredMsg;
        FSelEndMsgIdx    := FHoveredMsg;
        SelRdr.SetSelection(0, SelRdr.LayoutEntryCount - 1);
        Redraw;
        AKey := 0;
        Exit;
      end;
    end;
  end;

  // Escape — clear active selection
  if AKey = vkEscape then
  begin
    if FSelStartMsgIdx >= 0 then
    begin
      ClearAllSelections;
      FSelDragging := False;
      Redraw;
      AKey := 0;
      Exit;
    end;
  end;

  case AKey of
    vkUp   : ScrollBy(-30);
    vkDown : ScrollBy(30);
    vkPrior: ScrollBy(-(Height * 0.9));
    vkNext : ScrollBy(Height * 0.9);
    vkHome : ScrollToTop;
    vkEnd  : ScrollToBottom;
  end;
  inherited;
end;

{ ── Public message API ───────────────────────────────────────────────────── }

function TAIChatView.AddMessage(ARole: TChatRole;
  const AText: string): TAIChatMessage;
begin
  Result := FList.AddMessage(ARole, AText);
end;

function TAIChatView.AddUserMessage(const AText: string): TAIChatMessage;
begin
  Result := FList.AddMessage(TChatRole.crUser, AText);
end;

function TAIChatView.AddUserMessage(const AText: string;
  AMediaFiles: TAiMediaFiles): TAIChatMessage;
var
  I  : Integer;
  F  : TAiMediaFile;
  Att: TAIChatAttachment;
begin
  Result := FList.AddMessage(TChatRole.crUser, AText);
  if Assigned(AMediaFiles) and (AMediaFiles.Count > 0) then
  begin
    for I := 0 to AMediaFiles.Count - 1 do
    begin
      F   := AMediaFiles[I];
      Att := TAIChatAttachment.Create(F.FullFileName);
      Att.MimeType := F.MimeType;
      Result.Attachments.Add(Att);
      // Preload into painter's image cache NOW while the temp file still exists.
      // Also read raw bytes so the attachment can be serialized inline (base64)
      // when PersistAttachments=True — the temp file will be deleted after OnSend.
      if Att.FileKind = TChatFileKind.fkImage then
      begin
        if TFile.Exists(Att.LocalPath) then
          Att.Data := TFile.ReadAllBytes(Att.LocalPath);
        FList.Painter.PreloadImage(Att.LocalPath);
      end
      else if Att.FileKind = TChatFileKind.fkAudio then
      begin
        if TFile.Exists(Att.LocalPath) then
        begin
          Att.Data := TFile.ReadAllBytes(Att.LocalPath);
          // Copy to a path outside TempStore so it survives ClearAttachments
          var PersistPath := TPath.Combine(TPath.GetTempPath,
            Format('aichaudio_%d_%s', [FList.Messages.Count, Att.FileName]));
          TFile.Copy(Att.LocalPath, PersistPath, True);
          Att.LocalPath := PersistPath;
        end;
      end;
    end;
    FList.InvalidateMessage(FList.Messages.Count - 1);
  end;
end;

function TAIChatView.BeginAssistantMessage: TAIChatMessage;
begin
  FAutoScroll := True;
  Result := FList.AddMessage(TChatRole.crAssistant, '',
                              TChatMessageStatus.msStreaming);
end;

procedure TAIChatView.AppendToken(AMsgIndex: Integer; const AToken: string);
begin
  FList.AppendToken(AMsgIndex, AToken);
end;

procedure TAIChatView.FinishMessage(AMsgIndex: Integer);
begin
  FList.FinishMessage(AMsgIndex);
end;

procedure TAIChatView.ClearMessages;
begin
  ClearAllSelections;
  FSelDragging := False;
  FList.Clear;
  FScrollOffset := 0;
  FAutoScroll   := True;
  Redraw;
end;

{ ── Navigation ───────────────────────────────────────────────────────────── }

procedure TAIChatView.ScrollBy(ADelta: Single);
begin
  FScrollOffset := FScrollOffset + ADelta;
  ClampScroll;
  FAutoScroll := FScrollOffset >= MaxScrollVal - 2;
  Redraw;
end;

procedure TAIChatView.ScrollToTop;
begin
  FScrollOffset := 0;
  FAutoScroll   := False;
  Redraw;
end;

procedure TAIChatView.ScrollToBottom;
begin
  FScrollOffset := MaxScrollVal;
  FAutoScroll   := True;
  Redraw;
end;

procedure TAIChatView.RefreshLayout;
begin
  FList.ForceRelayout(Width);
  ClampScroll;
  Redraw;
end;

{ ── Clipboard (public) ───────────────────────────────────────────────────── }

procedure TAIChatView.CopyMessage(AMsgIndex: Integer);
begin
  if (AMsgIndex >= 0) and (AMsgIndex < FList.Messages.Count) then
    DoCopyMessage(FList.Messages[AMsgIndex]);
end;

procedure TAIChatView.CopyConversation;
begin
  DoCopyConversation;
end;

{ ── Serialization ────────────────────────────────────────────────────────── }

function TAIChatView.SaveToJSON: TJSONArray;
begin
  Result := FList.SaveToJSON;
end;

procedure TAIChatView.LoadFromJSON(AArr: TJSONArray);
begin
  FList.LoadFromJSON(AArr);
  FScrollOffset := 0;
  FAutoScroll   := True;
  ClampScroll;
  Redraw;
end;

// ── TAIChatBubble — streaming proxy ───────────────────────────────────────────

constructor TAIChatBubble.Create(AOwner: TAIChatView; AMsgIndex: Integer);
begin
  inherited Create;
  FOwner    := AOwner;
  FMsgIndex := AMsgIndex;
end;

procedure TAIChatBubble.AppendText(const AFragment: string);
begin
  FOwner.AppendToken(FMsgIndex, AFragment);
end;

procedure TAIChatBubble.AddContent(const AText: string; AMediaFiles: TAiMediaFiles);
begin
  // Replace full text of the message (non-streaming use)
  FOwner.FList.AppendToken(FMsgIndex, AText);
  FOwner.FinishMessage(FMsgIndex);
end;

// ── Backward-compat color setters ─────────────────────────────────────────────

procedure TAIChatView.SetInboundColor(AValue: TAlphaColor);
var
  Theme: TAIChatTheme;
begin
  if FInboundColor = AValue then Exit;
  FInboundColor := AValue;
  Theme := FList.Theme;
  Theme.UserBubbleBg := AValue;
  FList.SetTheme(Theme);
  Redraw;
end;

procedure TAIChatView.SetOutboundColor(AValue: TAlphaColor);
var
  Theme: TAIChatTheme;
begin
  if FOutboundColor = AValue then Exit;
  FOutboundColor := AValue;
  Theme := FList.Theme;
  Theme.AssistantBubbleBg := AValue;
  FList.SetTheme(Theme);
  Redraw;
end;

// ── Backward-compat message API ───────────────────────────────────────────────

function TAIChatView.AddBubble(const AText: string; const AUserName: string;
  AMediaFiles: TAiMediaFiles; AInbound: Boolean): TAIChatBubble;
var
  MsgIdx: Integer;
  Proxy : TAIChatBubble;
begin
  if AInbound then
    AddUserMessage(AText, AMediaFiles)
  else
  begin
    // Empty text → streaming assistant message
    if AText = '' then
      BeginAssistantMessage
    else
      AddMessage(TChatRole.crAssistant, AText);
  end;

  MsgIdx := MessageCount - 1;
  Proxy  := TAIChatBubble.Create(Self, MsgIdx);
  FBubbleProxies.Add(Proxy);
  Result := Proxy;
end;

procedure TAIChatView.Clear;
begin
  FBubbleProxies.Clear;
  ClearMessages;
end;

// ── Stream serialization (wraps JSON) ─────────────────────────────────────────

procedure TAIChatView.SaveToStream(AStream: TStream);
var
  Arr: TJSONArray;
  S  : TBytes;
begin
  Arr := SaveToJSON;
  try
    S := TEncoding.UTF8.GetBytes(Arr.ToString);
    if Length(S) > 0 then
      AStream.WriteBuffer(S[0], Length(S));
  finally
    Arr.Free;
  end;
end;

procedure TAIChatView.LoadFromStream(AStream: TStream);
var
  S  : TBytes;
  V  : TJSONValue;
  Arr: TJSONArray;
begin
  SetLength(S, AStream.Size - AStream.Position);
  if Length(S) = 0 then Exit;
  AStream.ReadBuffer(S[0], Length(S));
  V := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetString(S));
  if V is TJSONArray then
  begin
    Arr := TJSONArray(V);
    try
      LoadFromJSON(Arr);
    finally
      Arr.Free;
    end;
  end
  else
    V.Free;
end;

initialization
  RegisterFMXClasses([TAIChatView]);

end.
