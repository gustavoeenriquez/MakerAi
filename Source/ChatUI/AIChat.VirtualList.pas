unit AIChat.VirtualList;

// MIT License — Gustavo Enríquez <gustavoeenriquez@gmail.com>
// Virtual scroll engine for TAIChatView:
//   • per-message measured-height cache (invalidated on text/width change)
//   • per-assistant-message renderer + parsed document (lazy, owned)
//   • O(log n) binary-search visible range + O(1) streaming append
//   • stateless paint loop delegated to TAIChatBubblePainter

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math, System.Classes,
  System.Generics.Collections, System.JSON, System.IOUtils,
  System.Skia, FMX.Skia,
  uMakerAi.MD.Types,
  uMakerAi.MD.Renderer.Skia,
  AIChat.Types,
  AIChat.Bubble;

type
  TAIChatVirtualList = class
  private
    FMessages       : TAIChatMessages;   // owned; frees messages on destroy/clear
    FPainter        : TAIChatBubblePainter;
    FTheme          : TAIChatTheme;
    FViewWidth      : Single;

    FDocYPositions  : TList<Single>;     // [i] = top DocY of message i
    FTotalHeight    : Single;

    // Per-assistant-message caches (keyed by message list index, both owned)
    FRenderers      : TObjectDictionary<Integer, TAITextMDRenderer>;
    FDocuments      : TObjectDictionary<Integer, TMDDocument>;

    // Hover state — updated by owning control, consumed in Paint
    FHoveredMessage : Integer;
    FHoveredAttach  : Integer;
    FHoveredCopyBtn : Integer;   // message index whose copy-button is hovered (-1 = none)
    FCopiedMsg      : Integer;   // message index showing "copied" feedback (-1 = none)

    FOnLayoutChanged   : TNotifyEvent;
    FPersistAttachments: Boolean;

    procedure EnsureLayout(AIndex: Integer);
    procedure RebuildPositionsFrom(AFromIndex: Integer);

  public
    constructor Create(const ATheme: TAIChatTheme);
    destructor  Destroy; override;

    // ── Message management ───────────────────────────────────────────────────
    function  AddMessage(ARole: TChatRole; const AText: string;
                AStatus: TChatMessageStatus = TChatMessageStatus.msComplete
              ): TAIChatMessage;
    procedure AppendToken(AIndex: Integer; const AToken: string);
    procedure FinishMessage(AIndex: Integer);
    procedure DeleteMessage(AIndex: Integer);
    procedure Clear;

    // ── Layout ───────────────────────────────────────────────────────────────
    procedure SetViewWidth(AWidth: Single);
    procedure SetTheme(const ATheme: TAIChatTheme);
    function  TotalHeight: Single;
    function  MaxScroll(AViewHeight: Single): Single;
    function  MessageAtDocY(ADocY: Single): Integer;
    procedure GetVisibleRange(AScrollOffset, AViewHeight: Single;
                out AFirst, ALast: Integer);

    // ── Painting ─────────────────────────────────────────────────────────────
    procedure Paint(ACanvas: ISkCanvas; AScrollOffset, AViewHeight: Single;
                ASelStart: Integer = -1; ASelEnd: Integer = -1);

    // ── Hit testing (DocX/DocY = mouse in document coordinates) ─────────────
    function  HitTest(ADocX, ADocY: Single; out AMsg: TAIChatMessage;
                out AHit: TAIChatHitArea): Boolean;
    function  HitTestIndex(ADocX, ADocY: Single): Integer;

    // ── Hover / copy-button state ─────────────────────────────────────────────
    procedure SetHovered(AMsgIndex, AAttachIndex: Integer);
    procedure SetHoverCopyBtn(AMsgIndex: Integer);
    procedure SetCopied(AMsgIndex: Integer);

    // ── Accessors for code-copy hit detection ─────────────────────────────────
    function GetRenderer(AIndex: Integer): TAITextMDRenderer;
    function GetMsgDocY(AIndex: Integer): Single;

    // ── Layout invalidation ───────────────────────────────────────────────────
    procedure InvalidateMessage(AIndex: Integer);
    procedure ForceRelayout(AWidth: Single = 0);

    // ── Serialization ─────────────────────────────────────────────────────────
    function  SaveToJSON: TJSONArray;
    procedure LoadFromJSON(AArr: TJSONArray);

    property Messages        : TAIChatMessages     read FMessages;
    property ViewWidth       : Single              read FViewWidth;
    property Theme           : TAIChatTheme        read FTheme;
    property HoveredMessage  : Integer             read FHoveredMessage;
    property HoveredCopyBtn  : Integer             read FHoveredCopyBtn;
    property Painter         : TAIChatBubblePainter read FPainter;
    property OnLayoutChanged    : TNotifyEvent read FOnLayoutChanged
                                              write FOnLayoutChanged;
    property PersistAttachments : Boolean      read FPersistAttachments
                                              write FPersistAttachments;
  end;

implementation

{ TAIChatVirtualList }

constructor TAIChatVirtualList.Create(const ATheme: TAIChatTheme);
begin
  inherited Create;
  FTheme          := ATheme;
  FMessages       := TAIChatMessages.Create(True);
  FPainter        := TAIChatBubblePainter.Create(ATheme);
  FDocYPositions  := TList<Single>.Create;
  FRenderers      := TObjectDictionary<Integer, TAITextMDRenderer>.Create([doOwnsValues]);
  FDocuments      := TObjectDictionary<Integer, TMDDocument>.Create([doOwnsValues]);
  FTotalHeight    := 0;
  FViewWidth      := 0;
  FHoveredMessage := -1;
  FHoveredAttach  := -1;
  FHoveredCopyBtn := -1;
  FCopiedMsg      := -1;
end;

destructor TAIChatVirtualList.Destroy;
begin
  FDocuments.Free;    // frees owned TMDDocument objects
  FRenderers.Free;    // frees owned TAITextMDRenderer objects
  FDocYPositions.Free;
  FPainter.Free;
  FMessages.Free;     // frees owned TAIChatMessage objects (last)
  inherited;
end;

{ ── EnsureLayout ─────────────────────────────────────────────────────────── }
// Measures and caches the height of message[AIndex] at FViewWidth.
// Two-level invalidation:
//   MeasuredHeight = -1  → text changed → re-parse + re-measure
//   LayoutWidth ≠ FViewWidth → width changed → re-measure only (reuse doc)

procedure TAIChatVirtualList.EnsureLayout(AIndex: Integer);
var
  Msg   : TAIChatMessage;
  Rdr   : TAITextMDRenderer;
  Doc   : TMDDocument;
  NewDoc: TMDDocument;
  H     : Single;
begin
  if (AIndex < 0) or (AIndex >= FMessages.Count) then Exit;
  if FViewWidth < 1 then Exit;

  Msg := FMessages[AIndex];

  if (Msg.MeasuredHeight > 0) and (Abs(Msg.LayoutWidth - FViewWidth) < 0.5) then Exit;

  Rdr := nil;
  Doc := nil;

  if Msg.Role = TChatRole.crAssistant then
  begin
    // Get or create renderer — stable across text changes
    if not FRenderers.TryGetValue(AIndex, Rdr) then
    begin
      Rdr := TAITextMDRenderer.Create;
      Rdr.Theme := FTheme.MD;
      FRenderers.Add(AIndex, Rdr);
    end;

    // Re-parse if text was invalidated (MeasuredHeight=-1) or no cached doc
    if (Msg.MeasuredHeight < 0) or not FDocuments.TryGetValue(AIndex, Doc) then
    begin
      NewDoc := FPainter.ParseMarkdown(Msg.Text);
      FDocuments.AddOrSetValue(AIndex, NewDoc);  // dict frees any previous doc
      Doc := NewDoc;
    end;
  end;

  H := FPainter.MeasureHeight(Msg, FViewWidth, Rdr, Doc);
  Msg.MeasuredHeight := Max(10, H);
  Msg.LayoutWidth    := FViewWidth;
end;

{ ── RebuildPositionsFrom ─────────────────────────────────────────────────── }
// Recomputes FDocYPositions[AFromIndex..Count-1] and FTotalHeight.
// Calls EnsureLayout for any unmeasured message encountered.

procedure TAIChatVirtualList.RebuildPositionsFrom(AFromIndex: Integer);
var
  I   : Integer;
  DocY: Single;
  H   : Single;
begin
  if FMessages.Count = 0 then
  begin
    FTotalHeight := 0;
    FDocYPositions.Clear;
    Exit;
  end;

  while FDocYPositions.Count < FMessages.Count do
    FDocYPositions.Add(0);

  if AFromIndex <= 0 then
    DocY := 0
  else
  begin
    DocY := FDocYPositions[AFromIndex - 1];
    H    := FMessages[AFromIndex - 1].MeasuredHeight;
    DocY := DocY + Max(10, H);
  end;

  for I := AFromIndex to FMessages.Count - 1 do
  begin
    FDocYPositions[I] := DocY;
    EnsureLayout(I);
    H    := Max(10, FMessages[I].MeasuredHeight);
    DocY := DocY + H;
  end;

  FTotalHeight := DocY;
end;

{ ── Message management ───────────────────────────────────────────────────── }

function TAIChatVirtualList.AddMessage(ARole: TChatRole; const AText: string;
  AStatus: TChatMessageStatus): TAIChatMessage;
var
  Msg: TAIChatMessage;
  Idx: Integer;
begin
  Msg        := TAIChatMessage.Create(ARole, AText);
  Msg.Status := AStatus;
  FMessages.Add(Msg);
  Idx := FMessages.Count - 1;

  while FDocYPositions.Count < FMessages.Count do
    FDocYPositions.Add(0);

  EnsureLayout(Idx);
  RebuildPositionsFrom(Idx);

  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
  Result := Msg;
end;

procedure TAIChatVirtualList.AppendToken(AIndex: Integer; const AToken: string);
var
  Msg: TAIChatMessage;
begin
  if (AIndex < 0) or (AIndex >= FMessages.Count) then Exit;
  Msg := FMessages[AIndex];
  Msg.AppendText(AToken);           // sets MeasuredHeight=-1 → EnsureLayout re-parses
  RebuildPositionsFrom(AIndex);     // O(1) when AIndex = last message
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;

procedure TAIChatVirtualList.FinishMessage(AIndex: Integer);
var
  Msg: TAIChatMessage;
begin
  if (AIndex < 0) or (AIndex >= FMessages.Count) then Exit;
  Msg        := FMessages[AIndex];
  Msg.Status := TChatMessageStatus.msComplete;
  Msg.InvalidateLayout;
  RebuildPositionsFrom(AIndex);
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;

procedure TAIChatVirtualList.InvalidateMessage(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FMessages.Count) then Exit;
  FMessages[AIndex].InvalidateLayout;
  RebuildPositionsFrom(AIndex);
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;

procedure TAIChatVirtualList.ForceRelayout(AWidth: Single = 0);
var
  I: Integer;
begin
  if AWidth > 0.5 then FViewWidth := AWidth;
  if FViewWidth < 1 then Exit;
  for I := 0 to FMessages.Count - 1 do
    FMessages[I].LayoutWidth := -1;
  RebuildPositionsFrom(0);
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;

procedure TAIChatVirtualList.DeleteMessage(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FMessages.Count) then Exit;

  // Clear all renderer/doc caches (simpler than re-keying the dicts;
  // delete is infrequent — caches rebuild lazily on next paint)
  FRenderers.Clear;
  FDocuments.Clear;
  FMessages.Delete(AIndex);

  if FDocYPositions.Count > FMessages.Count then
    FDocYPositions.Delete(FDocYPositions.Count - 1);

  // Setting LayoutWidth=-1 forces EnsureLayout to recreate renderers/docs
  for I := 0 to FMessages.Count - 1 do
    FMessages[I].LayoutWidth := -1;

  RebuildPositionsFrom(0);
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;

procedure TAIChatVirtualList.Clear;
begin
  FMessages.Clear;
  FRenderers.Clear;
  FDocuments.Clear;
  FDocYPositions.Clear;
  FTotalHeight    := 0;
  FHoveredMessage := -1;
  FHoveredAttach  := -1;
  FHoveredCopyBtn := -1;
  FCopiedMsg      := -1;
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;

{ ── Layout ───────────────────────────────────────────────────────────────── }

procedure TAIChatVirtualList.SetViewWidth(AWidth: Single);
var
  I: Integer;
begin
  if Abs(AWidth - FViewWidth) < 0.5 then Exit;
  FViewWidth := AWidth;
  // Invalidate measured width; MeasuredHeight stays as estimate for doc caching
  for I := 0 to FMessages.Count - 1 do
    FMessages[I].LayoutWidth := -1;
  RebuildPositionsFrom(0);
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;

procedure TAIChatVirtualList.SetTheme(const ATheme: TAIChatTheme);
var
  I  : Integer;
  Rdr: TAITextMDRenderer;
begin
  FTheme         := ATheme;
  FPainter.Theme := ATheme;
  for I := 0 to FMessages.Count - 1 do
    if FRenderers.TryGetValue(I, Rdr) then
      Rdr.Theme := ATheme.MD;
  for I := 0 to FMessages.Count - 1 do
    FMessages[I].LayoutWidth := -1;
  RebuildPositionsFrom(0);
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;

function TAIChatVirtualList.TotalHeight: Single;
begin
  Result := FTotalHeight;
end;

function TAIChatVirtualList.MaxScroll(AViewHeight: Single): Single;
begin
  Result := Max(0, FTotalHeight - AViewHeight);
end;

// Binary search: returns the index of the message whose slot contains ADocY,
// or -1 if ADocY is outside the document.

function TAIChatVirtualList.MessageAtDocY(ADocY: Single): Integer;
var
  Lo, Hi, Mid: Integer;
  SlotBottom : Single;
begin
  Result := -1;
  if FMessages.Count = 0 then Exit;
  if FDocYPositions.Count < FMessages.Count then Exit;
  if ADocY < 0 then Exit;
  if ADocY >= FTotalHeight then Exit;

  Lo := 0;
  Hi := FMessages.Count - 1;
  while Lo < Hi do
  begin
    Mid := (Lo + Hi + 1) div 2;
    if FDocYPositions[Mid] <= ADocY then Lo := Mid
    else Hi := Mid - 1;
  end;

  // Verify ADocY is within this slot (not in a gap beyond the last message)
  if Lo < FMessages.Count - 1 then
    SlotBottom := FDocYPositions[Lo + 1]
  else
    SlotBottom := FTotalHeight;

  if ADocY < SlotBottom then
    Result := Lo;
end;

// Binary search for first visible; linear walk for last visible.
// AFirst = last message whose top ≤ AScrollOffset (may start above viewport).
// ALast  = last message whose top < AScrollOffset + AViewHeight.

procedure TAIChatVirtualList.GetVisibleRange(AScrollOffset, AViewHeight: Single;
  out AFirst, ALast: Integer);
var
  ViewTop, ViewBottom: Single;
  Lo, Hi, Mid       : Integer;
begin
  AFirst := -1;
  ALast  := -1;

  if (FMessages.Count = 0) or (FDocYPositions.Count < FMessages.Count) then Exit;

  ViewTop    := AScrollOffset;
  ViewBottom := AScrollOffset + AViewHeight;

  if (ViewBottom <= 0) or (ViewTop >= FTotalHeight) then Exit;

  Lo := 0;
  Hi := FMessages.Count - 1;
  while Lo < Hi do
  begin
    Mid := (Lo + Hi + 1) div 2;
    if FDocYPositions[Mid] <= ViewTop then Lo := Mid
    else Hi := Mid - 1;
  end;
  AFirst := Lo;

  ALast := AFirst;
  while (ALast < FMessages.Count - 1) and
        (FDocYPositions[ALast + 1] < ViewBottom) do
    Inc(ALast);
end;

{ ── Paint ────────────────────────────────────────────────────────────────── }

procedure TAIChatVirtualList.Paint(ACanvas: ISkCanvas;
  AScrollOffset, AViewHeight: Single;
  ASelStart, ASelEnd: Integer);
var
  First, Last, I: Integer;
  Msg           : TAIChatMessage;
  Rdr           : TAITextMDRenderer;
  Doc           : TMDDocument;
  DocY, ScreenY : Single;
  ShowCopy      : Boolean;
  HovAttach     : Integer;
  IsSelected    : Boolean;
begin
  if FMessages.Count = 0 then Exit;
  if FViewWidth < 1 then Exit;

  GetVisibleRange(AScrollOffset, AViewHeight, First, Last);
  if First < 0 then Exit;

  for I := First to Last do
  begin
    EnsureLayout(I);
    Msg     := FMessages[I];
    DocY    := FDocYPositions[I];
    ScreenY := DocY - AScrollOffset;

    // Safety cull (GetVisibleRange is approximate for partially-off-screen msgs)
    if ScreenY + Msg.MeasuredHeight < 0 then Continue;
    if ScreenY > AViewHeight then Break;

    Rdr := nil;
    Doc := nil;
    if Msg.Role = TChatRole.crAssistant then
    begin
      FRenderers.TryGetValue(I, Rdr);
      FDocuments.TryGetValue(I, Doc);
    end;

    ShowCopy   := (I = FHoveredMessage);
    HovAttach  := -1;
    if I = FHoveredMessage then HovAttach := FHoveredAttach;
    IsSelected := (ASelStart >= 0) and (I >= ASelStart) and (I <= ASelEnd);

    FPainter.Paint(ACanvas, Msg, DocY, ScreenY, FViewWidth,
                   Rdr, Doc, ShowCopy, HovAttach,
                   {AHovCopyBtn=}  (I = FHoveredCopyBtn),
                   {ACopied=}      (I = FCopiedMsg),
                   {AIsSelected=}  IsSelected);
  end;
end;

{ ── Hit testing ──────────────────────────────────────────────────────────── }

function TAIChatVirtualList.HitTest(ADocX, ADocY: Single;
  out AMsg: TAIChatMessage; out AHit: TAIChatHitArea): Boolean;
var
  Idx: Integer;
  Msg: TAIChatMessage;
  I  : Integer;
  H  : TAIChatHitArea;
begin
  Result := False;
  AMsg   := nil;
  FillChar(AHit, SizeOf(AHit), 0);

  Idx := MessageAtDocY(ADocY);
  if Idx < 0 then Exit;

  Msg  := FMessages[Idx];
  AMsg := Msg;

  // Check registered hit areas (stored in doc coordinates by the painter)
  for I := 0 to High(Msg.HitAreas) do
  begin
    H := Msg.HitAreas[I];
    if H.DocRect.Contains(TPointF.Create(ADocX, ADocY)) then
    begin
      AHit   := H;
      Result := True;
      Exit;
    end;
  end;

  // Fallback: hkText covering the full slot
  AHit.Kind    := TChatHitKind.hkText;
  AHit.DocRect := TRectF.Create(0, FDocYPositions[Idx], FViewWidth,
                                 FDocYPositions[Idx] + Msg.MeasuredHeight);
  Result := True;
end;

function TAIChatVirtualList.HitTestIndex(ADocX, ADocY: Single): Integer;
begin
  Result := MessageAtDocY(ADocY);
end;

{ ── Hover state ──────────────────────────────────────────────────────────── }

procedure TAIChatVirtualList.SetHovered(AMsgIndex, AAttachIndex: Integer);
begin
  FHoveredMessage := AMsgIndex;
  FHoveredAttach  := AAttachIndex;
end;

procedure TAIChatVirtualList.SetHoverCopyBtn(AMsgIndex: Integer);
begin
  FHoveredCopyBtn := AMsgIndex;
end;

procedure TAIChatVirtualList.SetCopied(AMsgIndex: Integer);
begin
  FCopiedMsg := AMsgIndex;
end;

function TAIChatVirtualList.GetRenderer(AIndex: Integer): TAITextMDRenderer;
begin
  if not FRenderers.TryGetValue(AIndex, Result) then
    Result := nil;
end;

function TAIChatVirtualList.GetMsgDocY(AIndex: Integer): Single;
begin
  if (AIndex >= 0) and (AIndex < FDocYPositions.Count) then
    Result := FDocYPositions[AIndex]
  else
    Result := 0;
end;

{ ── Serialization ────────────────────────────────────────────────────────── }

function TAIChatVirtualList.SaveToJSON: TJSONArray;
var
  Msg: TAIChatMessage;
begin
  Result := TJSONArray.Create;
  for Msg in FMessages do
    Result.AddElement(Msg.ToJSON(FPersistAttachments));
end;

procedure TAIChatVirtualList.LoadFromJSON(AArr: TJSONArray);
var
  I      : Integer;
  J      : Integer;
  Msg    : TAIChatMessage;
  Att    : TAIChatAttachment;
  TmpPath: string;
begin
  Clear;
  if AArr = nil then Exit;
  for I := 0 to AArr.Count - 1 do
  begin
    Msg := TAIChatMessage.Create(TChatRole.crUser);
    Msg.FromJSON(AArr.Items[I] as TJSONObject);
    FMessages.Add(Msg);
    // Restore attachments with inline data (images + audio) → temp file
    for J := 0 to Msg.Attachments.Count - 1 do
    begin
      Att := Msg.Attachments[J];
      if not (Att.FileKind in [TChatFileKind.fkImage, TChatFileKind.fkAudio]) then Continue;
      if Length(Att.Data) > 0 then
      begin
        TmpPath := TPath.Combine(TPath.GetTempPath,
          Format('aich_%d_%d_%s', [I, J, Att.FileName]));
        TFile.WriteAllBytes(TmpPath, Att.Data);
        Att.LocalPath := TmpPath;
      end;
      if (Att.FileKind = TChatFileKind.fkImage) and TFile.Exists(Att.LocalPath) then
        FPainter.PreloadImage(Att.LocalPath);
    end;
    while FDocYPositions.Count < FMessages.Count do
      FDocYPositions.Add(0);
  end;
  RebuildPositionsFrom(0);
end;

end.
