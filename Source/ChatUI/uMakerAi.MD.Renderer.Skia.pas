unit uMakerAi.MD.Renderer.Skia;

{
  AITextMD.Renderer.Skia
  ----------------------
  Skia-based layout and paint engine for TMDDocument.
  Renders each block top-to-bottom; returns heights so the FMX control
  can manage scrolling.  Pass ACanvas=nil to measure without painting.
}

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math,
  System.Generics.Collections,
  System.Skia,
  FMX.Skia,
  uMakerAi.MD.Types,
  uMakerAi.MD.Highlighter;

type
  TAITextMDRenderer = class
  private
    FTheme          : TMDTheme;
    FBgPaint        : ISkPaint;
    FFillPaint      : ISkPaint;
    FLinePaint      : ISkPaint;
    FFontProvider   : ISkTypefaceFontProvider;

    { Layout cache (rebuilt in RebuildLayoutFor / MeasureHeight) }
    FLayoutEntries      : TList<TMDLayoutEntry>;
    FBlockHeights       : TList<Single>;   // one height per top-level block
    FBuildingLayout     : Boolean;
    FContentHeightCache : Single;

    { Selection state }
    FHasSelection  : Boolean;
    FSelFirstEntry : Integer;
    FSelLastEntry  : Integer;
    FSelFirstChar  : Integer;  // char offset in FSelFirstEntry (-1 = whole entry)
    FSelLastChar   : Integer;  // char offset in FSelLastEntry  (-1 = whole entry)

    { Per-entry cached paragraphs (parallel to FLayoutEntries) — nil = non-text }
    FEntryParas  : TList<ISkParagraph>;
    FEntryParasX : TList<Single>;

    { Media cache — persists across layout rebuilds; cleared by ClearImageCache }
    FImageCache    : TDictionary<string, ISkImage>;  // URL → decoded image
    FMimeCache     : TDictionary<string, string>;    // URL → resolved MIME type
    FButtonAreas   : TList<TMDButtonArea>;           // rebuilt each layout pass

    { Link areas — rebuilt each layout pass }
    FLinkAreas     : TList<TMDLinkArea>;

    { Footnote def positions — rebuilt each layout pass (label → DocY) }
    FFootnoteDefPositions : TDictionary<string, Single>;
    { Footnote ref positions — rebuilt each layout pass (label → DocY of para) }
    FFootnoteRefPositions : TDictionary<string, Single>;

    { Code-block copy buttons — rebuilt each layout pass }
    FCodeCopyAreas        : TList<TCodeCopyArea>;

    { Code of the block whose copy-button shows "✓ Copied" feedback.
      Empty string = no feedback active. Compared against ABlock.RawText so
      coordinate-system mismatches between layout and draw are irrelevant. }
    FCopiedFeedbackCode   : string;

    { Details toggle areas — rebuilt each layout pass }
    FDetailsToggleAreas   : TList<TDetailsToggleArea>;

    { Zoomable image areas — rebuilt each layout pass }
    FImageAreas           : TList<TMDImageArea>;

    { Code-block word-wrap: True = wrap at available width, no h-scroll }
    FCodeWrap             : Boolean;

    { Media event callback (assigned by TAITextMD) }
    FOnLoadMedia   : TAITextMDLoadMediaEvent;

    procedure EnsureFontProvider;
    procedure EnsurePaints;
    function  GetFontProvider: ISkTypefaceFontProvider;

    { Plain-text extraction for layout entries }
    function SpansToText(ASpans: TObjectList<TMDSpan>): string;

    { Text style factory }
    function MakeTextStyle(ASize: Single; AColor: TAlphaColor;
                           ABold, AItalic, AUnderline, ACode: Boolean;
                           AStrikethrough: Boolean = False): ISkTextStyle;

    { Recursive span pusher — appends styled runs to an open builder }
    procedure PushSpan(ABuilder: ISkParagraphBuilder; ASpan: TMDSpan;
                       ASize: Single; AColor: TAlphaColor;
                       ABold, AItalic, AUnderline: Boolean;
                       AStrikethrough: Boolean = False);

    { Build + layout a Skia paragraph from a span list }
    function BuildParagraph(ASpans: TObjectList<TMDSpan>;
                            ASize: Single; AColor: TAlphaColor;
                            ABold: Boolean; AWidth: Single): ISkParagraph;

    { Block painters — return height consumed.  ACanvas may be nil (dry-run). }
    function DrawBlock     (ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth: Single): Single;
    function DrawSpanBlock (ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth, ASize: Single;
                            ABold: Boolean; AColor: TAlphaColor): Single;
    function DrawCodeBlock (ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth: Single): Single;
    function DrawBlockQuote(ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth: Single): Single;
    function DrawList      (ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth: Single): Single;
    function DrawHRule     (ACanvas: ISkCanvas;
                            AX, AY, AWidth: Single): Single;
    function DrawMediaBlock(ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth: Single): Single;
    function DrawTable     (ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth: Single): Single;
    function DrawFootnotes (ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth: Single): Single;
    function DrawDetails   (ACanvas: ISkCanvas; ABlock: TMDBlock;
                            AX, AY, AWidth: Single): Single;

    { Extract link bounding rects from a laid-out paragraph into FLinkAreas }
    procedure ExtractLinkAreas(ASpans: TObjectList<TMDSpan>;
                               APara: ISkParagraph; AX, ADocY: Single);
  public
    constructor Create;
    destructor  Destroy; override;

    { Build the layout cache for the given document and width.
      Must be called after any document / font / width change.
      Also updates ContentHeight. }
    procedure RebuildLayoutFor(ADoc: TMDDocument; AWidth: Single);

    { Paint ADocument onto ACanvas clipped to ADest. }
    procedure Draw(ACanvas: ISkCanvas; ADocument: TMDDocument;
                   const ADest: TRectF; AScrollOffset: Single = 0);

    { Return the total height needed to render ADocument at AWidth.
      Calls RebuildLayoutFor internally. }
    function MeasureHeight(ADocument: TMDDocument; AWidth: Single): Single;

    { Selection — indices into the layout cache }
    function  EntryAtDocY(ADocY: Single): Integer;
    procedure SetSelection(AFirst, ALast: Integer);
    procedure SetCharSelection(AFirstEntry, AFirstChar, ALastEntry, ALastChar: Integer);
    procedure ClearSelection;
    function  GetCharAtPoint(AEntryIdx: Integer; ARX, ARY: Single): Integer;
    function  GetSelectedText: string;
    function  GetAllText: string;

    { Code copy }
    function  CodeCopyAtPoint(ADocY, AX: Single; out ACode: string;
                              out ABtnDocRect: TRectF): Boolean;
    procedure StartCopyFeedback(const ACode: string);
    procedure ClearCopyFeedback;

    { Code block horizontal scroll }
    function  CodeBlockAtDocY(ADocY: Single; out AAreaIdx: Integer): Boolean;
    function  CodeHScrollTrackAtPoint(ADocY, AX: Single;
                out AAreaIdx: Integer): Boolean;
    procedure GetCodeHScrollInfo(AAreaIdx: Integer;
                out AContentW, AVisibleW, AHScroll: Single;
                out ATrackDocRect: TRectF);
    procedure SetCodeHScroll(AAreaIdx: Integer; AHScroll: Single);

    { Details toggle }
    function  DetailsToggleAtPoint(ADocY, AX: Single; out ABlock: TMDBlock): Boolean;

    { Links }
    function  LinkAreaAtPoint(ADocY, AX: Single; out AURL: string): Boolean;
    function  FootnoteDefDocY(const ALabel: string; out ADocY: Single): Boolean;
    function  FootnoteRefDocY(const ALabel: string; out ADocY: Single): Boolean;
    function  GetLayoutEntryCount: Integer;
    function  LayoutEntryAt(AIndex: Integer): TMDLayoutEntry;

    property  LayoutEntryCount: Integer read GetLayoutEntryCount;

    { Media }
    function  ButtonAreaAtPoint(ADocY, AX: Single; out AArea: TMDButtonArea): Boolean;
    procedure ClearImageCache;

    { Image zoom }
    function  ImageAtPoint(ADocY, AX: Single; out AURL: string): Boolean;
    function  GetCachedImage(const AURL: string): ISkImage;

    property Theme        : TMDTheme               read FTheme        write FTheme;
    property CodeWrap     : Boolean                read FCodeWrap     write FCodeWrap;
    property ContentHeight: Single                 read FContentHeightCache;
    property HasSelection : Boolean                read FHasSelection;
    property OnLoadMedia  : TAITextMDLoadMediaEvent read FOnLoadMedia  write FOnLoadMedia;
    property FontProvider : ISkTypefaceFontProvider read GetFontProvider;
  end;

implementation

const
  OUTER_PADDING = 16;   // px — horizontal + top/bottom margin
  HRULE_HEIGHT  = 17;   // px — space consumed by <hr>

{ =========================================================================
  Helpers
  ========================================================================= }

procedure TAITextMDRenderer.EnsureFontProvider;
begin
  { Shared font provider — registers the theme fonts so Skia finds them,
    and enables system-font fallback (AEnableFontFallback=True) so glyphs
    for accented characters, CJK, emoji, etc. are resolved automatically. }
  if FFontProvider <> nil then Exit;

  FFontProvider := TSkTypefaceFontProvider.Create;
  // Register all four weights/styles for the text font
  FFontProvider.RegisterTypeface(TSkTypeface.MakeFromName(FTheme.FontFamily, TSkFontStyle.Normal));
  FFontProvider.RegisterTypeface(TSkTypeface.MakeFromName(FTheme.FontFamily, TSkFontStyle.Bold));
  FFontProvider.RegisterTypeface(TSkTypeface.MakeFromName(FTheme.FontFamily, TSkFontStyle.Italic));
  FFontProvider.RegisterTypeface(TSkTypeface.MakeFromName(FTheme.FontFamily, TSkFontStyle.BoldItalic));
  // Register monospace font for code blocks
  FFontProvider.RegisterTypeface(TSkTypeface.MakeFromName(FTheme.MonoFamily, TSkFontStyle.Normal));
  FFontProvider.RegisterTypeface(TSkTypeface.MakeFromName(FTheme.MonoFamily, TSkFontStyle.Bold));
end;

procedure TAITextMDRenderer.EnsurePaints;
begin
  EnsureFontProvider;

  if FBgPaint = nil then
  begin
    FBgPaint           := TSkPaint.Create;
    FBgPaint.AntiAlias := False;
    FBgPaint.Style     := TSkPaintStyle.Fill;
  end;

  if FFillPaint = nil then
  begin
    FFillPaint           := TSkPaint.Create;
    FFillPaint.AntiAlias := True;
    FFillPaint.Style     := TSkPaintStyle.Fill;
  end;

  if FLinePaint = nil then
  begin
    FLinePaint             := TSkPaint.Create;
    FLinePaint.AntiAlias   := False;
    FLinePaint.Style       := TSkPaintStyle.Stroke;
    FLinePaint.StrokeWidth := 1;
  end;
end;

{ =========================================================================
  Text style factory
  ========================================================================= }

function TAITextMDRenderer.MakeTextStyle(ASize: Single; AColor: TAlphaColor;
  ABold, AItalic, AUnderline, ACode: Boolean;
  AStrikethrough: Boolean): ISkTextStyle;
var
  Style  : ISkTextStyle;
  Weight : TSkFontWeight;
  Slant  : TSkFontSlant;
begin
  Style := TSkTextStyle.Create;

  if ACode then
  begin
    Style.FontFamilies := [FTheme.MonoFamily, 'Courier New', 'Consolas'];
    Style.Color        := FTheme.CodeColor;
    { Note: ISkTextStyle.Background not available in Delphi 12 Skia.
      Code background is rendered as a rect in DrawCodeBlock instead. }
  end
  else
  begin
    Style.FontFamilies := [FTheme.FontFamily, 'Segoe UI', 'Arial'];
    Style.Color        := AColor;
  end;

  Style.FontSize := ASize;

  if ABold   then Weight := TSkFontWeight.Bold   else Weight := TSkFontWeight.Normal;
  if AItalic then Slant  := TSkFontSlant.Italic  else Slant  := TSkFontSlant.Upright;
  Style.FontStyle := TSkFontStyle.Create(Weight, TSkFontWidth.Normal, Slant);

  if AUnderline or AStrikethrough then
  begin
    var Decs: TSkTextDecorations := [];
    if AUnderline     then Include(Decs, TSkTextDecoration.Underline);
    if AStrikethrough then Include(Decs, TSkTextDecoration.LineThrough);
    Style.Decorations         := Decs;
    Style.DecorationColor     := AColor;
    Style.DecorationStyle     := TSkTextDecorationStyle.Solid;
    Style.DecorationThickness := 1;
  end;

  Result := Style;
end;

{ =========================================================================
  Span builder
  ========================================================================= }

procedure TAITextMDRenderer.PushSpan(ABuilder: ISkParagraphBuilder;
  ASpan: TMDSpan; ASize: Single; AColor: TAlphaColor;
  ABold, AItalic, AUnderline: Boolean;
  AStrikethrough: Boolean);
var
  Style : ISkTextStyle;
  Child : TMDSpan;
begin
  case ASpan.Kind of

    TMDSpanKind.skText:
    begin
      Style := MakeTextStyle(ASize, AColor, ABold, AItalic, AUnderline, False,
                             AStrikethrough);
      ABuilder.PushStyle(Style);
      ABuilder.AddText(ASpan.Text);
      ABuilder.Pop;
    end;

    TMDSpanKind.skBold:
      for Child in ASpan.Children do
        PushSpan(ABuilder, Child, ASize, AColor, True, AItalic, AUnderline,
                 AStrikethrough);

    TMDSpanKind.skItalic:
      for Child in ASpan.Children do
        PushSpan(ABuilder, Child, ASize, AColor, ABold, True, AUnderline,
                 AStrikethrough);

    TMDSpanKind.skBoldItalic:
      for Child in ASpan.Children do
        PushSpan(ABuilder, Child, ASize, AColor, True, True, AUnderline,
                 AStrikethrough);

    TMDSpanKind.skCode:
    begin
      Style := MakeTextStyle(ASize * 0.88, FTheme.CodeColor, False, False, False, True);
      ABuilder.PushStyle(Style);
      ABuilder.AddText(ASpan.Text);
      ABuilder.Pop;
    end;

    TMDSpanKind.skLink:
      for Child in ASpan.Children do
        PushSpan(ABuilder, Child, ASize, FTheme.LinkColor, ABold, AItalic, True,
                 AStrikethrough);

    TMDSpanKind.skImage:
    begin
      Style := MakeTextStyle(ASize, FTheme.LinkColor, False, True, False, False);
      ABuilder.PushStyle(Style);
      ABuilder.AddText('[' + ASpan.Alt + ']');
      ABuilder.Pop;
    end;

    TMDSpanKind.skLineBreak:
      ABuilder.AddText(#10);

    TMDSpanKind.skStrikethrough:
      for Child in ASpan.Children do
        PushSpan(ABuilder, Child, ASize, AColor, ABold, AItalic, AUnderline,
                 True);

    TMDSpanKind.skFootnoteRef:
    begin
      Style := MakeTextStyle(ASize * 0.72, FTheme.LinkColor, ABold, AItalic, False, False);
      ABuilder.PushStyle(Style);
      ABuilder.AddText('[' + ASpan.Text + ']');
      ABuilder.Pop;
    end;

  end;
end;

function TAITextMDRenderer.BuildParagraph(ASpans: TObjectList<TMDSpan>;
  ASize: Single; AColor: TAlphaColor; ABold: Boolean; AWidth: Single): ISkParagraph;
var
  ParaStyle : ISkParagraphStyle;
  DefStyle  : ISkTextStyle;
  Builder   : ISkParagraphBuilder;
  Span      : TMDSpan;
  Para      : ISkParagraph;
begin
  EnsureFontProvider;

  ParaStyle           := TSkParagraphStyle.Create;
  DefStyle            := MakeTextStyle(ASize, AColor, ABold, False, False, False);
  ParaStyle.TextStyle := DefStyle;

  Builder := TSkParagraphBuilder.Create(ParaStyle, FFontProvider, True);

  for Span in ASpans do
    PushSpan(Builder, Span, ASize, AColor, ABold, False, False);

  Para := Builder.Build;
  Para.Layout(Max(1, AWidth));
  Result := Para;
end;

{ =========================================================================
  Block painters
  ========================================================================= }

function TAITextMDRenderer.DrawSpanBlock(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth, ASize: Single; ABold: Boolean; AColor: TAlphaColor): Single;
var
  Para : ISkParagraph;
  Entry: TMDLayoutEntry;
begin
  Para := BuildParagraph(ABlock.Spans, ASize, AColor, ABold, AWidth);
  if ACanvas <> nil then
    Para.Paint(ACanvas, AX, AY);
  Result := Para.Height;

  { Record selectable unit + link areas during layout-building pass }
  if FBuildingLayout and (ACanvas = nil) then
  begin
    Entry.DocY      := AY;
    Entry.Height    := Para.Height;
    Entry.PlainText := SpansToText(ABlock.Spans);
    FLayoutEntries.Add(Entry);
    FEntryParas.Add(Para);
    FEntryParasX.Add(AX);
    ExtractLinkAreas(ABlock.Spans, Para, AX, AY);
  end;
end;

function TAITextMDRenderer.DrawCodeBlock(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth: Single): Single;
const
  CPY_W      = 52;   // Copy button width
  CPY_H      = 20;   // Copy button height
  CPY_PAD    = 6;    // margin from block edge
  HSB_H      = 4;    // horizontal scrollbar thumb height
  HSB_PAD    = 5;    // gap above and below scrollbar (inside block)
  GLPAD      = 4;    // gutter left padding
  GRPAD      = 8;    // gutter right padding (gap between number and code)
var
  Pad        : Single;
  ParaStyle  : ISkParagraphStyle;
  TextStyle  : ISkTextStyle;
  Builder    : ISkParagraphBuilder;
  Para       : ISkParagraph;
  TotalH     : Single;
  BoxRect    : TRectF;
  BtnRect    : TRectF;
  BtnTS      : ISkTextStyle;
  BtnPS      : ISkParagraphStyle;
  BtnB       : ISkParagraphBuilder;
  BtnP       : ISkParagraph;
  ContentW   : Single;
  VisibleW   : Single;
  NeedsHScroll      : Boolean;
  HScrollTrackDocRect: TRectF;
  LineCount  : Integer;
  MaxDigits  : Integer;
  GutterW    : Single;
  UseGutter  : Boolean;
  GTS        : ISkTextStyle;
  GPS        : ISkParagraphStyle;
  GTB        : ISkParagraphBuilder;
  GTP        : ISkParagraph;
  NTS        : ISkTextStyle;
  NPS        : ISkParagraphStyle;
  NB         : ISkParagraphBuilder;
  NumPara    : ISkParagraph;
  LineIdx    : Integer;
begin
  EnsureFontProvider;
  Pad := FTheme.CodeBlockPadding;

  ParaStyle           := TSkParagraphStyle.Create;
  TextStyle           := MakeTextStyle(FTheme.BaseFontSize * 0.88,
                                       FTheme.BlockCodeColor,
                                       False, False, False, True);
  ParaStyle.TextStyle := TextStyle;

  Builder := TSkParagraphBuilder.Create(ParaStyle, FFontProvider, True);

  if (ABlock.FenceInfo <> '') and TCodeHighlighter.SupportsLang(ABlock.FenceInfo) and
     (ABlock.RawText <> '') then
  begin
    { Highlighted run per token }
    for var Tok in TCodeHighlighter.Tokenize(ABlock.RawText, ABlock.FenceInfo) do
    begin
      var TokColor: TAlphaColor;
      case Tok.Kind of
        TSHTokenKind.htKeyword     : TokColor := FTheme.SHKeyword;
        TSHTokenKind.htString      : TokColor := FTheme.SHString;
        TSHTokenKind.htComment     : TokColor := FTheme.SHComment;
        TSHTokenKind.htNumber      : TokColor := FTheme.SHNumber;
        TSHTokenKind.htPreprocessor: TokColor := FTheme.SHPreprocessor;
        TSHTokenKind.htDiffAdd     : TokColor := FTheme.SHDiffAdd;
        TSHTokenKind.htDiffRemove  : TokColor := FTheme.SHDiffRemove;
      else
        TokColor := FTheme.BlockCodeColor;   // htPlain
      end;
      var TokStyle := MakeTextStyle(FTheme.BaseFontSize * 0.88,
                                    TokColor,
                                    False, False, False, True);
      Builder.PushStyle(TokStyle);
      Builder.AddText(Tok.Text);
      Builder.Pop;
    end;
  end
  else if ABlock.RawText <> '' then
    Builder.AddText(ABlock.RawText)
  else
    Builder.AddText(' ');

  Para    := Builder.Build;
  if FCodeWrap then
    Para.Layout(Max(1, AWidth - Pad * 2))  // gutter not yet known; approx
  else
    { Layout at huge width to prevent wrapping — we clip and h-scroll instead }
    Para.Layout(100000);

  { --- Line-number gutter -------------------------------------------------- }
  LineCount := 1;
  for var Ch in ABlock.RawText do
    if Ch = #10 then Inc(LineCount);
  if (ABlock.RawText <> '') and (ABlock.RawText[High(ABlock.RawText)] = #10) then
    Dec(LineCount);
  if LineCount < 1 then LineCount := 1;

  UseGutter := LineCount > 1;
  GutterW   := 0;
  MaxDigits := 0;
  if UseGutter then
  begin
    MaxDigits := Length(IntToStr(LineCount));
    { Measure exact gutter width from Skia using the widest number string }
    GTS := MakeTextStyle(FTheme.BaseFontSize * 0.88, FTheme.BlockCodeColor,
                         False, False, False, True);
    GPS := TSkParagraphStyle.Create;
    GPS.TextStyle := GTS;
    GTB := TSkParagraphBuilder.Create(GPS, FFontProvider, True);
    GTB.AddText(IntToStr(LineCount));   // widest digit count
    GTP := GTB.Build;
    GTP.Layout(1000);
    GutterW := Ceil(GTP.LongestLine) + GLPAD + GRPAD;
  end;
  { ------------------------------------------------------------------------ }

  { In wrap mode, re-layout now that GutterW is known }
  if FCodeWrap then
    Para.Layout(Max(1, AWidth - Pad * 2 - GutterW));

  ContentW     := Para.LongestLine;
  VisibleW     := AWidth - Pad * 2 - GutterW;
  NeedsHScroll := (not FCodeWrap) and (ContentW > VisibleW + 1);

  TotalH := Para.Height + Pad * 2;
  if NeedsHScroll then
    TotalH := TotalH + HSB_PAD + HSB_H + HSB_PAD;

  { Horizontal scrollbar track rect — starts after gutter }
  if NeedsHScroll then
    HScrollTrackDocRect := TRectF.Create(
      AX + Pad + GutterW, AY + TotalH - HSB_PAD - HSB_H,
      AX + AWidth - Pad,  AY + TotalH - HSB_PAD);

  { Copy button rect (top-right corner, doc coordinates) }
  BtnRect := TRectF.Create(
    AX + AWidth - CPY_PAD - CPY_W, AY + CPY_PAD,
    AX + AWidth - CPY_PAD,         AY + CPY_PAD + CPY_H);

  if ACanvas <> nil then
  begin
    { Block background }
    BoxRect := TRectF.Create(AX, AY, AX + AWidth, AY + TotalH);
    FFillPaint.Color := FTheme.BlockCodeBg;
    ACanvas.DrawRoundRect(BoxRect, 4, 4, FFillPaint);

    { --- Gutter ------------------------------------------------------------ }
    if UseGutter then
    begin
      { Subtle gutter background overlay }
      FFillPaint.Color := $0C000000;   // 5% black tint
      ACanvas.DrawRect(TRectF.Create(AX + Pad, AY + Pad,
                                     AX + Pad + GutterW,
                                     AY + Pad + Para.Height), FFillPaint);

      { Line numbers — right-aligned via space-padding (monospace) }
      var GutterBaseColor := FTheme.BlockCodeColor and $00FFFFFF;
      var GutterNumColor  := GutterBaseColor or $80000000;  // 50 % alpha
      NTS := MakeTextStyle(FTheme.BaseFontSize * 0.88, GutterNumColor,
                           False, False, False, True);
      NPS := TSkParagraphStyle.Create;
      NPS.TextStyle := NTS;
      NB := TSkParagraphBuilder.Create(NPS, FFontProvider, True);
      for LineIdx := 1 to LineCount do
      begin
        NB.AddText(IntToStr(LineIdx).PadLeft(MaxDigits));
        if LineIdx < LineCount then NB.AddText(#10);
      end;
      NumPara := NB.Build;
      NumPara.Layout(Max(1, GutterW - GLPAD));
      NumPara.Paint(ACanvas, AX + Pad + GLPAD, AY + Pad);

      { Separator line }
      FLinePaint.Color := (FTheme.BlockCodeColor and $00FFFFFF) or $20000000;
      ACanvas.DrawLine(TPointF.Create(AX + Pad + GutterW, AY + Pad),
                       TPointF.Create(AX + Pad + GutterW, AY + Pad + Para.Height),
                       FLinePaint);
    end;
    { ---------------------------------------------------------------------- }

    { Retrieve current h-scroll offset for this block from layout cache }
    var HScroll: Single := 0;
    for var AreaIdx := 0 to FCodeCopyAreas.Count - 1 do
      if FCodeCopyAreas[AreaIdx].Block = ABlock then
      begin
        HScroll := FCodeCopyAreas[AreaIdx].HScroll;
        Break;
      end;

    { Paint code — clipped after gutter, offset by HScroll }
    var CodeLeft := AX + Pad + GutterW;
    if NeedsHScroll then
    begin
      var ClipR := TRectF.Create(CodeLeft, AY + Pad,
                                  AX + AWidth - Pad,
                                  AY + Pad + Para.Height);
      ACanvas.Save;
      ACanvas.ClipRect(ClipR);
      Para.Paint(ACanvas, CodeLeft - HScroll, AY + Pad);
      ACanvas.Restore;
    end
    else
      Para.Paint(ACanvas, CodeLeft, AY + Pad);

    { Horizontal scrollbar }
    if NeedsHScroll then
    begin
      var TrackW := HScrollTrackDocRect.Width;
      var ThumbW := Max(20, VisibleW / ContentW * TrackW);
      var MaxHS  := Max(0, ContentW - VisibleW);
      var ThumbX := HScrollTrackDocRect.Left;
      if MaxHS > 0 then
        ThumbX := HScrollTrackDocRect.Left +
                  Min(HScroll, MaxHS) / MaxHS * (TrackW - ThumbW);
      var ThumbRect := TRectF.Create(ThumbX, HScrollTrackDocRect.Top,
                                     ThumbX + ThumbW, HScrollTrackDocRect.Bottom);
      FFillPaint.Color := $18808080;
      ACanvas.DrawRoundRect(HScrollTrackDocRect, 2, 2, FFillPaint);
      FFillPaint.Color := $70808080;
      ACanvas.DrawRoundRect(ThumbRect, 2, 2, FFillPaint);
    end;

    { Copy button }
    var IsFeedback := (FCopiedFeedbackCode <> '') and
                      (FCopiedFeedbackCode = ABlock.RawText);
    if IsFeedback then
    begin
      if (FTheme.BgColor and $00FFFFFF) = $00FFFFFF then FFillPaint.Color := $FF4CAF50
      else                                               FFillPaint.Color := $FF2D7A2D;
    end
    else
    begin
      if (FTheme.BgColor and $00FFFFFF) = $00FFFFFF then FFillPaint.Color := $FFE0E0E0
      else                                               FFillPaint.Color := $FF3C3C3C;
    end;
    ACanvas.DrawRoundRect(BtnRect, 3, 3, FFillPaint);

    var BtnTextColor: TAlphaColor;
    if IsFeedback then BtnTextColor := TAlphaColorRec.White
    else               BtnTextColor := FTheme.BlockCodeColor;
    BtnTS := MakeTextStyle(FTheme.BaseFontSize * 0.75,
                           BtnTextColor, False, False, False, False);
    BtnPS := TSkParagraphStyle.Create;
    BtnPS.TextStyle := BtnTS;
    BtnB := TSkParagraphBuilder.Create(BtnPS, FFontProvider, True);
    if IsFeedback then BtnB.AddText(#$2713 + ' Copied')
    else               BtnB.AddText('Copy');
    BtnP := BtnB.Build;
    BtnP.Layout(Max(1, CPY_W - 8));
    BtnP.Paint(ACanvas, BtnRect.Left + 4,
               BtnRect.Top + (CPY_H - BtnP.Height) * 0.5);
  end;

  Result := TotalH;

  { Record layout entry + copy area during layout pass }
  if FBuildingLayout and (ACanvas = nil) then
  begin
    var Entry: TMDLayoutEntry;
    Entry.DocY      := AY;
    Entry.Height    := TotalH;
    Entry.PlainText := ABlock.RawText;
    FLayoutEntries.Add(Entry);
    FEntryParas.Add(nil);
    FEntryParasX.Add(0);

    var CopyArea: TCodeCopyArea;
    CopyArea.DocRect             := BtnRect;
    CopyArea.Code                := ABlock.RawText;
    CopyArea.Block               := ABlock;
    CopyArea.ContentW            := ContentW;
    CopyArea.VisibleW            := VisibleW;
    CopyArea.HScroll             := 0;
    CopyArea.HScrollTrackDocRect := HScrollTrackDocRect;
    CopyArea.BlockDocRect        := TRectF.Create(AX, AY, AX + AWidth, AY + TotalH);
    CopyArea.GutterW             := GutterW;
    FCodeCopyAreas.Add(CopyArea);
  end;
end;

function TAITextMDRenderer.DrawBlockQuote(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth: Single): Single;
const
  BAR_W = 4;
var
  Indent  : Single;
  ChildY  : Single;
  Child   : TMDBlock;
  ChildH  : Single;
  BarRect : TRectF;
begin
  Indent := FTheme.BlockQuoteIndent;
  ChildY := AY;

  for Child in ABlock.Children do
  begin
    ChildH := DrawBlock(ACanvas, Child, AX + Indent, ChildY, AWidth - Indent);
    ChildY := ChildY + ChildH + FTheme.ParagraphSpacing * 0.5;
  end;

  { Left accent bar — drawn after so we know the full height }
  if (ACanvas <> nil) and (ChildY > AY) then
  begin
    BarRect := TRectF.Create(AX, AY, AX + BAR_W, ChildY - FTheme.ParagraphSpacing * 0.5);
    FFillPaint.Color := FTheme.QuoteBorderColor;
    ACanvas.DrawRoundRect(BarRect, 2, 2, FFillPaint);
  end;

  Result := ChildY - AY;
end;

function TAITextMDRenderer.DrawList(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth: Single): Single;
var
  Indent       : Single;
  ItemY        : Single;
  ItemIdx      : Integer;
  Item         : TMDBlock;
  ItemH        : Single;
  BulletText   : string;
  BulletStyle  : ISkParagraphStyle;
  BulletTS     : ISkTextStyle;
  BulletBuilder: ISkParagraphBuilder;
  BulletPara   : ISkParagraph;
  Child        : TMDBlock;
  { Task list checkbox }
  BorderPaint  : ISkPaint;
  BoxSz, LineH, BoxX, BoxY: Single;
  BoxR         : TRectF;
  ChkTS        : ISkTextStyle;
  ChkPS        : ISkParagraphStyle;
  ChkB         : ISkParagraphBuilder;
  ChkP         : ISkParagraph;
begin
  Indent := FTheme.ListIndent;
  ItemY  := AY;

  for ItemIdx := 0 to ABlock.Children.Count - 1 do
  begin
    Item := ABlock.Children[ItemIdx];

    if Item.IsTaskItem then
    begin
      { GFM task list checkbox }
      if ACanvas <> nil then
      begin
        BoxSz  := FTheme.BaseFontSize * 0.72;
        LineH  := FTheme.BaseFontSize * FTheme.LineSpacing;
        BoxX   := AX + (Indent - BoxSz) / 2;
        BoxY   := ItemY + (LineH - BoxSz) / 2;
        BoxR   := TRectF.Create(BoxX, BoxY, BoxX + BoxSz, BoxY + BoxSz);

        BorderPaint             := TSkPaint.Create;
        BorderPaint.Style       := TSkPaintStyle.Stroke;
        BorderPaint.StrokeWidth := 1.5;

        if Item.TaskChecked then
        begin
          { Filled box (accent color) + white checkmark }
          FFillPaint.Color  := FTheme.LinkColor;
          BorderPaint.Color := FTheme.LinkColor;
          ACanvas.DrawRoundRect(BoxR, 2, 2, FFillPaint);

          ChkTS             := MakeTextStyle(BoxSz * 0.82, TAlphaColorRec.White,
                                             True, False, False, False);
          ChkPS             := TSkParagraphStyle.Create;
          ChkPS.TextStyle   := ChkTS;
          ChkB              := TSkParagraphBuilder.Create(ChkPS, FFontProvider, True);
          ChkB.AddText(#$2713);   // ✓
          ChkP              := ChkB.Build;
          ChkP.Layout(BoxSz + 4);
          ChkP.Paint(ACanvas,
                     BoxX + (BoxSz - ChkP.LongestLine) / 2,
                     BoxY + (BoxSz - ChkP.Height) / 2);
        end
        else
        begin
          { Outlined box }
          FFillPaint.Color  := FTheme.BgColor;
          BorderPaint.Color := FTheme.QuoteBorderColor;
          ACanvas.DrawRoundRect(BoxR, 2, 2, FFillPaint);
          ACanvas.DrawRoundRect(BoxR, 2, 2, BorderPaint);
        end;
      end;
    end
    else
    begin
      if ABlock.Ordered then
        BulletText := IntToStr(ABlock.StartNum + ItemIdx) + '.  '
      else
        BulletText := #$2022 + '  ';    // •

      { Draw bullet / number }
      if ACanvas <> nil then
      begin
        BulletStyle            := TSkParagraphStyle.Create;
        BulletTS               := MakeTextStyle(FTheme.BaseFontSize,
                                                FTheme.TextColor,
                                                False, False, False, False);
        BulletStyle.TextStyle  := BulletTS;
        BulletBuilder          := TSkParagraphBuilder.Create(BulletStyle, FFontProvider, True);
        BulletBuilder.AddText(BulletText);
        BulletPara := BulletBuilder.Build;
        BulletPara.Layout(Indent);
        BulletPara.Paint(ACanvas, AX, ItemY);
      end;
    end;

    { Draw item content }
    if Item.Spans.Count > 0 then
      ItemH := DrawSpanBlock(ACanvas, Item,
                             AX + Indent, ItemY, AWidth - Indent,
                             FTheme.BaseFontSize, False, FTheme.TextColor)
    else
    begin
      ItemH := 0;
      for Child in Item.Children do
      begin
        var Ch := DrawBlock(ACanvas, Child, AX + Indent, ItemY + ItemH, AWidth - Indent);
        ItemH := ItemH + Ch + FTheme.ParagraphSpacing * 0.3;
      end;
    end;

    ItemY := ItemY + Max(FTheme.BaseFontSize * FTheme.LineSpacing, ItemH)
                   + FTheme.ParagraphSpacing * 0.3;
  end;

  Result := ItemY - AY;
end;

function TAITextMDRenderer.DrawHRule(ACanvas: ISkCanvas;
  AX, AY, AWidth: Single): Single;
var
  MidY: Single;
  P1, P2: TPointF;
begin
  MidY := AY + HRULE_HEIGHT / 2;
  if ACanvas <> nil then
  begin
    FLinePaint.Color       := FTheme.QuoteBorderColor;
    FLinePaint.StrokeWidth := 1;
    P1 := TPointF.Create(AX,          MidY);
    P2 := TPointF.Create(AX + AWidth, MidY);
    ACanvas.DrawLine(P1, P2, FLinePaint);
  end;
  Result := HRULE_HEIGHT;
end;

function TAITextMDRenderer.DrawBlock(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth: Single): Single;
var
  FontSize: Single;
begin
  case ABlock.Kind of

    TMDBlockKind.bkParagraph:
      Result := DrawSpanBlock(ACanvas, ABlock, AX, AY, AWidth,
                              FTheme.BaseFontSize, False, FTheme.TextColor);

    TMDBlockKind.bkHeading:
    begin
      FontSize := FTheme.BaseFontSize * FTheme.HeadingScale[ABlock.Level];
      Result   := DrawSpanBlock(ACanvas, ABlock, AX, AY, AWidth,
                                FontSize, True, FTheme.TextColor);
    end;

    TMDBlockKind.bkCodeBlock:
      Result := DrawCodeBlock(ACanvas, ABlock, AX, AY, AWidth);

    TMDBlockKind.bkBlockQuote:
      Result := DrawBlockQuote(ACanvas, ABlock, AX, AY, AWidth);

    TMDBlockKind.bkList:
      Result := DrawList(ACanvas, ABlock, AX, AY, AWidth);

    TMDBlockKind.bkHRule:
      Result := DrawHRule(ACanvas, AX, AY, AWidth);

    TMDBlockKind.bkMedia:
      Result := DrawMediaBlock(ACanvas, ABlock, AX, AY, AWidth);

    TMDBlockKind.bkTable:
      Result := DrawTable(ACanvas, ABlock, AX, AY, AWidth);

    TMDBlockKind.bkFootnotes:
      Result := DrawFootnotes(ACanvas, ABlock, AX, AY, AWidth);

    TMDBlockKind.bkDetails:
      Result := DrawDetails(ACanvas, ABlock, AX, AY, AWidth);

    else
      Result := 0;
  end;
end;

{ =========================================================================
  Public API
  ========================================================================= }

constructor TAITextMDRenderer.Create;
begin
  inherited;
  FTheme         := TMDTheme.Default;
  FLayoutEntries := TList<TMDLayoutEntry>.Create;
  FBlockHeights  := TList<Single>.Create;
  FSelFirstEntry := -1;
  FSelLastEntry  := -1;
  FSelFirstChar  := -1;
  FSelLastChar   := -1;
  FEntryParas    := TList<ISkParagraph>.Create;
  FEntryParasX   := TList<Single>.Create;
  FImageCache           := TDictionary<string, ISkImage>.Create;
  FMimeCache            := TDictionary<string, string>.Create;
  FButtonAreas          := TList<TMDButtonArea>.Create;
  FLinkAreas            := TList<TMDLinkArea>.Create;
  FFootnoteDefPositions := TDictionary<string, Single>.Create;
  FFootnoteRefPositions := TDictionary<string, Single>.Create;
  FCodeCopyAreas        := TList<TCodeCopyArea>.Create;
  FDetailsToggleAreas   := TList<TDetailsToggleArea>.Create;
  FImageAreas           := TList<TMDImageArea>.Create;
  FCodeWrap             := False;
end;

destructor TAITextMDRenderer.Destroy;
begin
  FImageAreas.Free;
  FDetailsToggleAreas.Free;
  FCodeCopyAreas.Free;
  FFootnoteRefPositions.Free;
  FFootnoteDefPositions.Free;
  FLinkAreas.Free;
  FButtonAreas.Free;
  FMimeCache.Free;
  FImageCache.Free;
  FEntryParasX.Free;
  FEntryParas.Free;
  FBlockHeights.Free;
  FLayoutEntries.Free;
  inherited;
end;

{ =========================================================================
  Plain-text helper
  ========================================================================= }

function TAITextMDRenderer.SpansToText(ASpans: TObjectList<TMDSpan>): string;
var
  Span: TMDSpan;
begin
  Result := '';
  for Span in ASpans do
    case Span.Kind of
      TMDSpanKind.skText,
      TMDSpanKind.skCode      : Result := Result + Span.Text;
      TMDSpanKind.skLineBreak : Result := Result + #10;
      TMDSpanKind.skImage     : Result := Result + '[' + Span.Alt + ']';
      TMDSpanKind.skFootnoteRef: Result := Result + '[' + Span.Text + ']';
    else
      Result := Result + SpansToText(Span.Children);
    end;
end;

{ =========================================================================
  Table
  ========================================================================= }

function TAITextMDRenderer.DrawTable(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth: Single): Single;
const
  CELL_PAD_H = 10;
  CELL_PAD_V = 6;
  MIN_COL_W  = 40;
var
  ColCount    : Integer;
  ColWidths   : array of Single;
  RowHeights  : array of Single;
  Row, Cell   : TMDBlock;
  RowIdx, ColIdx: Integer;
  Para        : ISkParagraph;
  CellInnerW  : Single;
  TotalW      : Single;
  X, Y        : Single;
  RowH, CellH : Single;
  BorderColor : TAlphaColor;
  HeaderBg    : TAlphaColor;
  AlignVal    : TMDTableAlign;
  TextX       : Single;
  PlainSB     : TStringBuilder;
  UsedW       : Single;

  function BuildCellPara(ACell: TMDBlock; AInnerW: Single;
                         ABold: Boolean): ISkParagraph;
  var
    PS: ISkParagraphStyle;
    TS: ISkTextStyle;
    B : ISkParagraphBuilder;
    S : TMDSpan;
  begin
    PS           := TSkParagraphStyle.Create;
    TS           := MakeTextStyle(FTheme.BaseFontSize, FTheme.TextColor,
                                  ABold, False, False, False);
    PS.TextStyle := TS;
    B := TSkParagraphBuilder.Create(PS, FFontProvider, True);
    for S in ACell.Spans do
      PushSpan(B, S, FTheme.BaseFontSize, FTheme.TextColor, ABold, False, False);
    Result := B.Build;
    Result.Layout(Max(1, AInnerW));
  end;

begin
  EnsurePaints;
  ColCount := ABlock.ColCount;
  if (ColCount = 0) or (ABlock.Children.Count = 0) then
    Exit(0);

  TotalW := AWidth;

  { Equal column widths }
  SetLength(ColWidths, ColCount);
  for ColIdx := 0 to ColCount - 1 do
    ColWidths[ColIdx] := Max(MIN_COL_W, TotalW / ColCount);
  UsedW := 0;
  for ColIdx := 0 to ColCount - 2 do
    UsedW := UsedW + ColWidths[ColIdx];
  ColWidths[ColCount - 1] := Max(MIN_COL_W, TotalW - UsedW);

  { Measure row heights }
  SetLength(RowHeights, ABlock.Children.Count);
  for RowIdx := 0 to ABlock.Children.Count - 1 do
  begin
    Row  := ABlock.Children[RowIdx];
    RowH := 0;
    for ColIdx := 0 to Row.Children.Count - 1 do
    begin
      if ColIdx >= ColCount then Break;
      CellInnerW := ColWidths[ColIdx] - CELL_PAD_H * 2;
      Para  := BuildCellPara(Row.Children[ColIdx], CellInnerW, Row.Level = 0);
      CellH := Para.Height + CELL_PAD_V * 2;
      if CellH > RowH then RowH := CellH;
    end;
    if RowH < FTheme.BaseFontSize * FTheme.LineSpacing + CELL_PAD_V * 2 then
      RowH := FTheme.BaseFontSize * FTheme.LineSpacing + CELL_PAD_V * 2;
    RowHeights[RowIdx] := RowH;
  end;

  { Theme colors }
  if (FTheme.BgColor and $00FFFFFF) = $00FFFFFF then
  begin
    BorderColor := $FFCCCCCC;
    HeaderBg    := $FFF0F0F0;
  end
  else
  begin
    BorderColor := $FF444444;
    HeaderBg    := $FF2D2D2D;
  end;

  Y := AY;
  for RowIdx := 0 to ABlock.Children.Count - 1 do
  begin
    Row  := ABlock.Children[RowIdx];
    RowH := RowHeights[RowIdx];
    X    := AX;

    if (ACanvas <> nil) and (Row.Level = 0) then
    begin
      FFillPaint.Color := HeaderBg;
      ACanvas.DrawRect(TRectF.Create(AX, Y, AX + TotalW, Y + RowH), FFillPaint);
    end;

    for ColIdx := 0 to ColCount - 1 do
    begin
      CellInnerW := ColWidths[ColIdx] - CELL_PAD_H * 2;
      if ColIdx < Row.Children.Count then
        Cell := Row.Children[ColIdx]
      else
        Cell := nil;

      if ACanvas <> nil then
      begin
        FLinePaint.Color       := BorderColor;
        FLinePaint.StrokeWidth := 1;
        ACanvas.DrawLine(TPointF.Create(X + ColWidths[ColIdx], Y),
                         TPointF.Create(X + ColWidths[ColIdx], Y + RowH),
                         FLinePaint);
        ACanvas.DrawLine(TPointF.Create(X, Y + RowH),
                         TPointF.Create(X + ColWidths[ColIdx], Y + RowH),
                         FLinePaint);

        if Cell <> nil then
        begin
          Para     := BuildCellPara(Cell, CellInnerW, Row.Level = 0);
          AlignVal := ABlock.ColAlign[ColIdx];
          case AlignVal of
            TMDTableAlign.taRight :
              TextX := X + ColWidths[ColIdx] - CELL_PAD_H - Para.LongestLine;
            TMDTableAlign.taCenter:
              TextX := X + (ColWidths[ColIdx] - Para.LongestLine) * 0.5;
          else
            TextX := X + CELL_PAD_H;
          end;
          Para.Paint(ACanvas, TextX, Y + CELL_PAD_V);
          ExtractLinkAreas(Cell.Spans, Para, TextX, Y + CELL_PAD_V);
        end;
      end
      else if FBuildingLayout and (Cell <> nil) then
      begin
        { Layout pass: register link areas for hit-testing }
        Para     := BuildCellPara(Cell, CellInnerW, Row.Level = 0);
        AlignVal := ABlock.ColAlign[ColIdx];
        case AlignVal of
          TMDTableAlign.taRight :
            TextX := X + ColWidths[ColIdx] - CELL_PAD_H - Para.LongestLine;
          TMDTableAlign.taCenter:
            TextX := X + (ColWidths[ColIdx] - Para.LongestLine) * 0.5;
        else
          TextX := X + CELL_PAD_H;
        end;
        ExtractLinkAreas(Cell.Spans, Para, TextX, Y + CELL_PAD_V);
      end;
      X := X + ColWidths[ColIdx];
    end;

    if ACanvas <> nil then
    begin
      FLinePaint.Color := BorderColor;
      ACanvas.DrawLine(TPointF.Create(AX, Y),
                       TPointF.Create(AX, Y + RowH), FLinePaint);
    end;

    Y := Y + RowH;
  end;

  if ACanvas <> nil then
  begin
    FLinePaint.Color := BorderColor;
    ACanvas.DrawLine(TPointF.Create(AX, AY),
                     TPointF.Create(AX + TotalW, AY), FLinePaint);
  end;

  Result := Y - AY;

  if FBuildingLayout and (ACanvas = nil) then
  begin
    PlainSB := TStringBuilder.Create;
    try
      for RowIdx := 0 to ABlock.Children.Count - 1 do
      begin
        Row := ABlock.Children[RowIdx];
        for ColIdx := 0 to Row.Children.Count - 1 do
        begin
          if ColIdx > 0 then PlainSB.Append(#9);
          PlainSB.Append(SpansToText(Row.Children[ColIdx].Spans));
        end;
        PlainSB.AppendLine;
      end;
      var E: TMDLayoutEntry;
      E.DocY      := AY;
      E.Height    := Y - AY;
      E.PlainText := PlainSB.ToString;
      FLayoutEntries.Add(E);
      FEntryParas.Add(nil);
      FEntryParasX.Add(0);
    finally
      PlainSB.Free;
    end;
  end;
end;

{ =========================================================================
  Footnotes
  ========================================================================= }

function TAITextMDRenderer.DrawFootnotes(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth: Single): Single;
const
  MARKER_W = 32;   // px — width reserved for "[N]" marker
  ROW_GAP  = 3;    // px — vertical gap between definitions
  HR_H     = 10;   // px — height of the section divider
var
  Y          : Single;
  SmallSize  : Single;
  Child      : TMDBlock;
  MarkerSpans: TObjectList<TMDSpan>;
  MarkerPara : ISkParagraph;
  DefPara    : ISkParagraph;
  RowH       : Single;
  Entry      : TMDLayoutEntry;
begin
  EnsureFontProvider;
  Y         := AY;
  SmallSize := FTheme.BaseFontSize * 0.85;

  // Thin horizontal rule to separate footnotes from body
  if ACanvas <> nil then
  begin
    FLinePaint.Color       := FTheme.QuoteBorderColor;
    FLinePaint.StrokeWidth := 1;
    ACanvas.DrawLine(TPointF.Create(AX, Y + HR_H * 0.5),
                     TPointF.Create(AX + AWidth * 0.5, Y + HR_H * 0.5),
                     FLinePaint);
  end;
  Y := Y + HR_H;

  for Child in ABlock.Children do
  begin
    if Child.Kind <> TMDBlockKind.bkFootnoteDef then Continue;

    // Marker: "[N]" in link color
    MarkerSpans := TObjectList<TMDSpan>.Create(True);
    try
      MarkerSpans.Add(TMDSpan.Create(TMDSpanKind.skText,
                      '[' + IntToStr(Child.Level) + ']'));
      MarkerPara := BuildParagraph(MarkerSpans, SmallSize,
                                   FTheme.LinkColor, False, MARKER_W);
    finally
      MarkerSpans.Free;
    end;

    // Definition text + ↩ back-link (appended inline)
    var BackSpan := TMDSpan.Create(TMDSpanKind.skLink);
    BackSpan.URL := '#fnref-' + Child.RawText;
    BackSpan.Children.Add(TMDSpan.Create(TMDSpanKind.skText, ' ' + #$21A9));
    var AllDef := TObjectList<TMDSpan>.Create(False);  // non-owning view
    try
      for var Sp in Child.Spans do AllDef.Add(Sp);
      AllDef.Add(BackSpan);
      DefPara := BuildParagraph(AllDef, SmallSize,
                                 FTheme.QuoteTextColor, False,
                                 AWidth - MARKER_W);

      RowH := Max(MarkerPara.Height, DefPara.Height);

      if ACanvas <> nil then
      begin
        MarkerPara.Paint(ACanvas, AX, Y);
        DefPara.Paint(ACanvas, AX + MARKER_W, Y);
      end;

      if FBuildingLayout and (ACanvas = nil) then
      begin
        Entry.DocY      := Y;
        Entry.Height    := RowH;
        Entry.PlainText := '[' + IntToStr(Child.Level) + '] ' +
                           SpansToText(Child.Spans);
        FLayoutEntries.Add(Entry);
        FEntryParas.Add(DefPara);
        FEntryParasX.Add(AX + MARKER_W);
        FFootnoteDefPositions.AddOrSetValue(Child.RawText, Y);
        ExtractLinkAreas(AllDef, DefPara, AX + MARKER_W, Y);
      end;
    finally
      AllDef.Free;
      BackSpan.Free;
    end;

    Y := Y + RowH + ROW_GAP;
  end;

  Result := Y - AY;
end;

{ =========================================================================
  Details / Summary
  ========================================================================= }

function TAITextMDRenderer.DrawDetails(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth: Single): Single;
const
  ARROW_W = 20;   // px — width for the ▶/▼ indicator
  INDENT  = 16;   // px — extra left indent for body content
var
  Y          : Single;
  ArrowText  : string;
  ArrowSpans : TObjectList<TMDSpan>;
  ArrowPara  : ISkParagraph;
  SumBlock   : TMDBlock;
  SumPara    : ISkParagraph;
  EmptySpans : TObjectList<TMDSpan>;
  RowH       : Single;
  ToggleArea : TDetailsToggleArea;
  Entry      : TMDLayoutEntry;
  i          : Integer;
  Child      : TMDBlock;
  ChildH     : Single;
begin
  EnsureFontProvider;
  Y := AY;

  if ABlock.IsOpen then ArrowText := #$25BC   // ▼
  else                  ArrowText := #$25B6;  // ▶

  { Arrow paragraph }
  ArrowSpans := TObjectList<TMDSpan>.Create(True);
  try
    ArrowSpans.Add(TMDSpan.Create(TMDSpanKind.skText, ArrowText));
    ArrowPara := BuildParagraph(ArrowSpans, FTheme.BaseFontSize,
                                FTheme.LinkColor, False, ARROW_W);
  finally
    ArrowSpans.Free;
  end;

  { Summary paragraph }
  SumBlock := nil;
  if ABlock.Children.Count > 0 then
    SumBlock := ABlock.Children[0];

  if SumBlock <> nil then
    SumPara := BuildParagraph(SumBlock.Spans, FTheme.BaseFontSize,
                              FTheme.TextColor, True, AWidth - ARROW_W)
  else
  begin
    EmptySpans := TObjectList<TMDSpan>.Create(True);
    try
      SumPara := BuildParagraph(EmptySpans, FTheme.BaseFontSize,
                                FTheme.TextColor, False, AWidth - ARROW_W);
    finally
      EmptySpans.Free;
    end;
  end;

  RowH := Max(ArrowPara.Height, SumPara.Height);

  if ACanvas <> nil then
  begin
    ArrowPara.Paint(ACanvas, AX, Y);
    SumPara.Paint(ACanvas, AX + ARROW_W, Y);
  end;

  if FBuildingLayout and (ACanvas = nil) then
  begin
    ToggleArea.DocRect := TRectF.Create(AX, Y, AX + AWidth, Y + RowH);
    ToggleArea.Block   := ABlock;
    FDetailsToggleAreas.Add(ToggleArea);

    Entry.DocY      := Y;
    Entry.Height    := RowH;
    if SumBlock <> nil then
      Entry.PlainText := SpansToText(SumBlock.Spans)
    else
      Entry.PlainText := '';
    FLayoutEntries.Add(Entry);
    FEntryParas.Add(SumPara);
    FEntryParasX.Add(AX + ARROW_W);
  end;

  Y := Y + RowH + FTheme.ParagraphSpacing * 0.3;

  { If open, draw child blocks (index 1 onwards — index 0 is the bkSummary) }
  if ABlock.IsOpen then
  begin
    for i := 1 to ABlock.Children.Count - 1 do
    begin
      Child  := ABlock.Children[i];
      ChildH := DrawBlock(ACanvas, Child, AX + INDENT, Y, AWidth - INDENT);
      Y      := Y + ChildH + FTheme.ParagraphSpacing * 0.5;
    end;
  end;

  Result := Y - AY;
end;

{ =========================================================================
  Layout cache
  ========================================================================= }

procedure TAITextMDRenderer.RebuildLayoutFor(ADoc: TMDDocument; AWidth: Single);
var
  W, X, Y : Single;
  Block   : TMDBlock;
  H       : Single;
begin
  FLayoutEntries.Clear;
  FEntryParas.Clear;
  FEntryParasX.Clear;
  FBlockHeights.Clear;
  FButtonAreas.Clear;
  FLinkAreas.Clear;
  FFootnoteDefPositions.Clear;
  FFootnoteRefPositions.Clear;
  FCodeCopyAreas.Clear;
  FDetailsToggleAreas.Clear;
  FImageAreas.Clear;
  FContentHeightCache := 0;
  if ADoc = nil then Exit;

  EnsurePaints;

  W := AWidth - OUTER_PADDING * 2;
  X := OUTER_PADDING;
  Y := OUTER_PADDING;

  FBuildingLayout := True;
  try
    for Block in ADoc.Blocks do
    begin
      H := DrawBlock(nil, Block, X, Y, W);
      FBlockHeights.Add(H);
      Y := Y + H + FTheme.ParagraphSpacing;
    end;
  finally
    FBuildingLayout := False;
  end;

  FContentHeightCache := Y + OUTER_PADDING;
end;

{ =========================================================================
  Selection
  ========================================================================= }

function TAITextMDRenderer.EntryAtDocY(ADocY: Single): Integer;
var
  i        : Integer;
  E        : TMDLayoutEntry;
  BestIdx  : Integer;
  BestDist : Single;
  Dist     : Single;
begin
  Result := -1;
  if FLayoutEntries.Count = 0 then Exit;

  { Exact hit }
  for i := 0 to FLayoutEntries.Count - 1 do
  begin
    E := FLayoutEntries[i];
    if (ADocY >= E.DocY) and (ADocY < E.DocY + E.Height) then
      Exit(i);
  end;

  { Mouse is in a gap between blocks — return nearest entry by center distance }
  BestIdx  := 0;
  BestDist := MaxSingle;
  for i := 0 to FLayoutEntries.Count - 1 do
  begin
    E    := FLayoutEntries[i];
    Dist := Abs(ADocY - (E.DocY + E.Height * 0.5));
    if Dist < BestDist then
    begin
      BestDist := Dist;
      BestIdx  := i;
    end;
  end;
  Result := BestIdx;
end;

procedure TAITextMDRenderer.SetSelection(AFirst, ALast: Integer);
begin
  FSelFirstEntry := AFirst;
  FSelLastEntry  := ALast;
  FSelFirstChar  := -1;
  FSelLastChar   := -1;
  FHasSelection  := True;
end;

procedure TAITextMDRenderer.ClearSelection;
begin
  FHasSelection  := False;
  FSelFirstEntry := -1;
  FSelLastEntry  := -1;
  FSelFirstChar  := -1;
  FSelLastChar   := -1;
end;

function TAITextMDRenderer.GetAllText: string;
var
  SB: TStringBuilder;
  i : Integer;
  E : TMDLayoutEntry;
begin
  SB := TStringBuilder.Create;
  try
    for i := 0 to FLayoutEntries.Count - 1 do
    begin
      E := FLayoutEntries[i];
      if SB.Length > 0 then SB.AppendLine;
      SB.Append(E.PlainText);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TAITextMDRenderer.SetCharSelection(AFirstEntry, AFirstChar,
  ALastEntry, ALastChar: Integer);
begin
  FSelFirstEntry := AFirstEntry;
  FSelLastEntry  := ALastEntry;
  FSelFirstChar  := AFirstChar;
  FSelLastChar   := ALastChar;
  FHasSelection  := True;
end;

function TAITextMDRenderer.GetCharAtPoint(AEntryIdx: Integer;
  ARX, ARY: Single): Integer;
var
  Para : ISkParagraph;
  ParaX: Single;
  E    : TMDLayoutEntry;
  Pos  : TSkPositionAffinity;
begin
  Result := -1;
  if (AEntryIdx < 0) or (AEntryIdx >= FEntryParas.Count) then Exit;
  Para := FEntryParas[AEntryIdx];
  if Para = nil then Exit;
  ParaX := FEntryParasX[AEntryIdx];
  E     := FLayoutEntries[AEntryIdx];
  Pos   := Para.GetGlyphPositionAtCoordinate(ARX - ParaX, ARY - E.DocY);
  Result := Pos.Position;
end;

function TAITextMDRenderer.GetSelectedText: string;
var
  SB : TStringBuilder;
  i  : Integer;
  E  : TMDLayoutEntry;
begin
  Result := '';
  if not FHasSelection then Exit;
  if FLayoutEntries.Count = 0 then Exit;

  var NormFirst, NormLast: Integer;
  var NormFirstChar, NormLastChar: Integer;
  if FSelFirstEntry < FSelLastEntry then
  begin
    NormFirst     := FSelFirstEntry; NormLast     := FSelLastEntry;
    NormFirstChar := FSelFirstChar;  NormLastChar := FSelLastChar;
  end
  else if FSelFirstEntry > FSelLastEntry then
  begin
    NormFirst     := FSelLastEntry;  NormLast     := FSelFirstEntry;
    NormFirstChar := FSelLastChar;   NormLastChar := FSelFirstChar;
  end
  else
  begin
    NormFirst     := FSelFirstEntry;
    NormLast      := FSelFirstEntry;
    NormFirstChar := Min(FSelFirstChar, FSelLastChar);
    NormLastChar  := Max(FSelFirstChar, FSelLastChar);
  end;
  NormFirst := Max(0, Min(NormFirst, FLayoutEntries.Count - 1));
  NormLast  := Max(0, Min(NormLast,  FLayoutEntries.Count - 1));

  SB := TStringBuilder.Create;
  try
    for i := NormFirst to NormLast do
    begin
      E := FLayoutEntries[i];
      var TextLen := Length(E.PlainText);
      var StartChar := 0;
      var EndChar   := TextLen;
      if (i = NormFirst) and (NormFirstChar >= 0) then
        StartChar := Min(NormFirstChar, TextLen);
      if (i = NormLast) and (NormLastChar >= 0) then
        EndChar := Min(Max(NormLastChar, StartChar), TextLen);
      var Fragment := Copy(E.PlainText, StartChar + 1, EndChar - StartChar);
      if SB.Length > 0 then SB.AppendLine;
      SB.Append(Fragment);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ =========================================================================
  Public API
  ========================================================================= }

procedure TAITextMDRenderer.Draw(ACanvas: ISkCanvas; ADocument: TMDDocument;
  const ADest: TRectF; AScrollOffset: Single);
var
  X, Y, W    : Single;
  Block      : TMDBlock;
  BlockH     : Single;
  OffsetY    : Single;
  SelFirst,
  SelLast, i : Integer;
  E          : TMDLayoutEntry;
  HighY1,
  HighY2     : Single;
  NormFirstChar,
  NormLastChar   : Integer;
  EntryPara      : ISkParagraph;
  EntryParaX     : Single;
  StartChar,
  EndChar,
  TextLen        : Integer;
  SelRects       : TArray<TSkTextBox>;
begin
  if ADocument = nil then Exit;
  EnsurePaints;

  { Background }
  FBgPaint.Color := FTheme.BgColor;
  ACanvas.DrawRect(ADest, FBgPaint);

  X := ADest.Left + OUTER_PADDING;
  W := ADest.Width - OUTER_PADDING * 2;
  Y := ADest.Top  + OUTER_PADDING - AScrollOffset;

  ACanvas.Save;
  ACanvas.ClipRect(ADest);

  { ---- Selection highlight — drawn before text so text appears on top ---- }
  if FHasSelection and (FLayoutEntries.Count > 0) then
  begin
    OffsetY  := ADest.Top - AScrollOffset;

    { Normalize direction: NormFirst ≤ NormLast; swap char offsets accordingly }
    if FSelFirstEntry < FSelLastEntry then
    begin
      SelFirst := FSelFirstEntry; SelLast := FSelLastEntry;
      NormFirstChar := FSelFirstChar; NormLastChar := FSelLastChar;
    end
    else if FSelFirstEntry > FSelLastEntry then
    begin
      SelFirst := FSelLastEntry;  SelLast := FSelFirstEntry;
      NormFirstChar := FSelLastChar; NormLastChar := FSelFirstChar;
    end
    else
    begin
      SelFirst := FSelFirstEntry; SelLast := FSelFirstEntry;
      NormFirstChar := Min(FSelFirstChar, FSelLastChar);
      NormLastChar  := Max(FSelFirstChar, FSelLastChar);
    end;
    SelFirst := Max(0, Min(SelFirst, FLayoutEntries.Count - 1));
    SelLast  := Max(0, Min(SelLast,  FLayoutEntries.Count - 1));

    FFillPaint.Color := FTheme.SelColor;
    for i := SelFirst to SelLast do
    begin
      if i >= FLayoutEntries.Count then Break;
      E      := FLayoutEntries[i];
      HighY1 := E.DocY + OffsetY;
      HighY2 := E.DocY + E.Height + OffsetY;
      if HighY2 < ADest.Top    then Continue;
      if HighY1 > ADest.Bottom then Break;

      { Try character-level highlight for text entries at the selection endpoints }
      EntryPara  := nil;
      EntryParaX := 0;
      if i < FEntryParas.Count  then EntryPara  := FEntryParas[i];
      if i < FEntryParasX.Count then EntryParaX := FEntryParasX[i];

      StartChar := -1;
      EndChar   := -1;
      if EntryPara <> nil then
      begin
        if (i = SelFirst) and (NormFirstChar >= 0) then StartChar := NormFirstChar;
        if (i = SelLast)  and (NormLastChar  >= 0) then EndChar   := NormLastChar;
      end;

      if (EntryPara <> nil) and ((StartChar >= 0) or (EndChar >= 0)) then
      begin
        TextLen := Length(E.PlainText);
        var SC: Cardinal := 0;
        var EC: Cardinal := Cardinal(TextLen);
        if StartChar >= 0 then SC := Cardinal(Min(StartChar, TextLen));
        if EndChar   >= 0 then EC := Cardinal(Min(EndChar,   TextLen));
        if SC < EC then
        begin
          SelRects := EntryPara.GetRectsForRange(SC, EC,
                        TSkRectHeightStyle.Max, TSkRectWidthStyle.Tight);
          for var R in SelRects do
          begin
            var ScrX1 := ADest.Left + EntryParaX + R.Rect.Left;
            var ScrX2 := ADest.Left + EntryParaX + R.Rect.Right;
            var ScrY1 := E.DocY + R.Rect.Top    + OffsetY;
            var ScrY2 := E.DocY + R.Rect.Bottom + OffsetY;
            ScrX1 := Max(ScrX1, ADest.Left);
            ScrX2 := Min(ScrX2, ADest.Right);
            if ScrX2 > ScrX1 then
              ACanvas.DrawRect(TRectF.Create(ScrX1, ScrY1, ScrX2, ScrY2), FFillPaint);
          end;
        end;
      end
      else
        ACanvas.DrawRect(TRectF.Create(ADest.Left, HighY1, ADest.Right, HighY2),
                         FFillPaint);
    end;
  end;

  { ---- Blocks — use cached heights; no nil-canvas rebuild per frame ---- }
  for var BlockIdx := 0 to ADocument.Blocks.Count - 1 do
  begin
    Block := ADocument.Blocks[BlockIdx];

    { Use cached height when available; fall back to live measure on mismatch }
    if BlockIdx < FBlockHeights.Count then
      BlockH := FBlockHeights[BlockIdx]
    else
      BlockH := DrawBlock(nil, Block, X, Y, W);

    if (Y + BlockH >= ADest.Top) and (Y <= ADest.Bottom) then
      DrawBlock(ACanvas, Block, X, Y, W);

    Y := Y + BlockH + FTheme.ParagraphSpacing;
  end;

  ACanvas.Restore;
end;

function TAITextMDRenderer.CodeCopyAtPoint(ADocY, AX: Single;
  out ACode: string; out ABtnDocRect: TRectF): Boolean;
var
  i: Integer;
  C: TCodeCopyArea;
begin
  Result      := False;
  ABtnDocRect := TRectF.Empty;
  for i := 0 to FCodeCopyAreas.Count - 1 do
  begin
    C := FCodeCopyAreas[i];
    if C.DocRect.Contains(TPointF.Create(AX, ADocY)) then
    begin
      ACode       := C.Code;
      ABtnDocRect := C.DocRect;
      Result      := True;
      Exit;
    end;
  end;
end;

procedure TAITextMDRenderer.StartCopyFeedback(const ACode: string);
begin
  FCopiedFeedbackCode := ACode;
end;

procedure TAITextMDRenderer.ClearCopyFeedback;
begin
  FCopiedFeedbackCode := '';
end;

function TAITextMDRenderer.CodeBlockAtDocY(ADocY: Single;
  out AAreaIdx: Integer): Boolean;
var I: Integer;
begin
  for I := 0 to FCodeCopyAreas.Count - 1 do
  begin
    var R := FCodeCopyAreas[I].BlockDocRect;
    if (ADocY >= R.Top) and (ADocY <= R.Bottom) then
    begin
      AAreaIdx := I;
      Exit(True);
    end;
  end;
  AAreaIdx := -1;
  Result := False;
end;

function TAITextMDRenderer.CodeHScrollTrackAtPoint(ADocY, AX: Single;
  out AAreaIdx: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCodeCopyAreas.Count - 1 do
  begin
    var A := FCodeCopyAreas[I];
    if (A.ContentW > A.VisibleW + 1) and
       (ADocY >= A.HScrollTrackDocRect.Top) and
       (ADocY <= A.HScrollTrackDocRect.Bottom) and
       (AX >= A.HScrollTrackDocRect.Left) and
       (AX <= A.HScrollTrackDocRect.Right) then
    begin
      AAreaIdx := I;
      Exit(True);
    end;
  end;
  AAreaIdx := -1;
  Result := False;
end;

procedure TAITextMDRenderer.GetCodeHScrollInfo(AAreaIdx: Integer;
  out AContentW, AVisibleW, AHScroll: Single; out ATrackDocRect: TRectF);
begin
  if (AAreaIdx >= 0) and (AAreaIdx < FCodeCopyAreas.Count) then
  begin
    var A := FCodeCopyAreas[AAreaIdx];
    AContentW     := A.ContentW;
    AVisibleW     := A.VisibleW;
    AHScroll      := A.HScroll;
    ATrackDocRect := A.HScrollTrackDocRect;
  end
  else
  begin
    AContentW := 0; AVisibleW := 0; AHScroll := 0;
    ATrackDocRect := TRectF.Empty;
  end;
end;

procedure TAITextMDRenderer.SetCodeHScroll(AAreaIdx: Integer; AHScroll: Single);
begin
  if (AAreaIdx >= 0) and (AAreaIdx < FCodeCopyAreas.Count) then
  begin
    var A := FCodeCopyAreas[AAreaIdx];
    A.HScroll := Max(0, Min(AHScroll, A.ContentW - A.VisibleW));
    FCodeCopyAreas[AAreaIdx] := A;
  end;
end;

function TAITextMDRenderer.DetailsToggleAtPoint(ADocY, AX: Single;
  out ABlock: TMDBlock): Boolean;
var
  i: Integer;
  D: TDetailsToggleArea;
begin
  Result := False;
  ABlock := nil;
  for i := 0 to FDetailsToggleAreas.Count - 1 do
  begin
    D := FDetailsToggleAreas[i];
    if D.DocRect.Contains(TPointF.Create(AX, ADocY)) then
    begin
      ABlock := D.Block;
      Result := True;
      Exit;
    end;
  end;
end;

procedure TAITextMDRenderer.ExtractLinkAreas(ASpans: TObjectList<TMDSpan>;
  APara: ISkParagraph; AX, ADocY: Single);

  { Count UTF-16 code units contributed to the paragraph by a span tree }
  function SpanCharCount(ASpan: TMDSpan): Integer;
  var
    Child: TMDSpan;
  begin
    case ASpan.Kind of
      TMDSpanKind.skText:        Result := Length(ASpan.Text);
      TMDSpanKind.skCode:        Result := Length(ASpan.Text);
      TMDSpanKind.skLineBreak:   Result := 1;
      TMDSpanKind.skImage:       Result := 1 + Length(ASpan.Alt) + 1; // '[alt]'
      TMDSpanKind.skFootnoteRef: Result := 1 + Length(ASpan.Text) + 1; // '[N]'
    else
      begin
        Result := 0;
        for Child in ASpan.Children do
          Inc(Result, SpanCharCount(Child));
      end;
    end;
  end;

  { Walk span tree, updating ACharOff; record link rects into FLinkAreas }
  procedure ProcessSpan(ASpan: TMDSpan; var ACharOff: Integer);
  var
    StartChar, EndChar : Integer;
    Boxes              : TArray<TSkTextBox>;
    Box                : TSkTextBox;
    Area               : TMDLinkArea;
    Child              : TMDSpan;
  begin
    if ASpan.Kind in [TMDSpanKind.skLink, TMDSpanKind.skFootnoteRef] then
    begin
      StartChar := ACharOff;
      EndChar   := ACharOff + SpanCharCount(ASpan);
      Boxes     := APara.GetRectsForRange(StartChar, EndChar,
                     TSkRectHeightStyle.Max, TSkRectWidthStyle.Tight);
      for Box in Boxes do
      begin
        Area.DocRect := TRectF.Create(
          AX + Box.Rect.Left,  ADocY + Box.Rect.Top,
          AX + Box.Rect.Right, ADocY + Box.Rect.Bottom);
        if ASpan.Kind = TMDSpanKind.skFootnoteRef then
        begin
          Area.URL := '#fn-' + ASpan.URL;  // internal anchor: #fn-label
          FFootnoteRefPositions.AddOrSetValue(ASpan.URL, ADocY);
        end
        else
          Area.URL := ASpan.URL;
        FLinkAreas.Add(Area);
      end;
      ACharOff := EndChar;
    end
    else if ASpan.Children.Count > 0 then
    begin
      { Container span (bold / italic / bolditalic) — recurse }
      for Child in ASpan.Children do
        ProcessSpan(Child, ACharOff);
    end
    else
      Inc(ACharOff, SpanCharCount(ASpan));
  end;

var
  CharOff : Integer;
  Span    : TMDSpan;
begin
  CharOff := 0;
  for Span in ASpans do
    ProcessSpan(Span, CharOff);
end;

function TAITextMDRenderer.LinkAreaAtPoint(ADocY, AX: Single;
  out AURL: string): Boolean;
var
  i: Integer;
  L: TMDLinkArea;
begin
  Result := False;
  for i := 0 to FLinkAreas.Count - 1 do
  begin
    L := FLinkAreas[i];
    if L.DocRect.Contains(TPointF.Create(AX, ADocY)) then
    begin
      AURL   := L.URL;
      Result := True;
      Exit;
    end;
  end;
end;

function TAITextMDRenderer.FootnoteDefDocY(const ALabel: string;
  out ADocY: Single): Boolean;
begin
  Result := FFootnoteDefPositions.TryGetValue(ALabel, ADocY);
end;

function TAITextMDRenderer.FootnoteRefDocY(const ALabel: string;
  out ADocY: Single): Boolean;
begin
  Result := FFootnoteRefPositions.TryGetValue(ALabel, ADocY);
end;

function TAITextMDRenderer.GetFontProvider: ISkTypefaceFontProvider;
begin
  EnsureFontProvider;
  Result := FFontProvider;
end;

function TAITextMDRenderer.GetLayoutEntryCount: Integer;
begin
  Result := FLayoutEntries.Count;
end;

function TAITextMDRenderer.LayoutEntryAt(AIndex: Integer): TMDLayoutEntry;
begin
  Result := FLayoutEntries[AIndex];
end;

function TAITextMDRenderer.DrawMediaBlock(ACanvas: ISkCanvas; ABlock: TMDBlock;
  AX, AY, AWidth: Single): Single;
const
  CARD_PAD    = 10;
  BTN_W       = 72;
  BTN_H       = 26;
  BTN_GAP     = 6;
  BTN_RADIUS  = 4;
  CARD_RADIUS = 6;
  MAX_IMG_H   = 400;
var
  URL, MimeType    : string;
  CachedMime       : string;
  ImgData          : TBytes;
  Handled          : Boolean;
  Img              : ISkImage;
  Scale, DrawW, DrawH: Single;
  ImgRect          : TRectF;
  AltText, FileName: string;
  P                : Integer;
  CardH, TextH     : Single;
  CardRect, BtnRect: TRectF;
  IconText         : string;
  LabelText        : string;
  BtnY, BtnX       : Single;
  A                : Integer;
  BtnLabel         : string;
  BtnAction        : TMediaAction;
  Entry            : TMDLayoutEntry;
  BtnArea          : TMDButtonArea;
  ParaStyle        : ISkParagraphStyle;
  TextStyle        : ISkTextStyle;
  Builder          : ISkParagraphBuilder;
  Para             : ISkParagraph;
  IArea            : TMDImageArea;
begin
  URL      := ABlock.RawText;
  MimeType := ABlock.MimeType;
  Img      := nil;
  Handled  := False;
  ImgData  := nil;

  EnsureFontProvider;

  { Resolve MIME type and load bytes — once per URL; results cached }
  if not FMimeCache.TryGetValue(URL, CachedMime) then
  begin
    if Assigned(FOnLoadMedia) then
      FOnLoadMedia(nil, URL, ImgData, MimeType, Handled);
    if MimeType <> '' then
      ABlock.MimeType := MimeType;
    FMimeCache.AddOrSetValue(URL, MimeType);

    { Decode image bytes if applicable }
    if Handled and (Length(ImgData) > 0) then
    begin
      var IsImg := (MimeType = '') or MimeType.StartsWith('image/');
      if IsImg then
      try
        var DecodedImg := TSkImage.MakeFromEncoded(ImgData);
        if DecodedImg <> nil then
        begin
          FImageCache.AddOrSetValue(URL, DecodedImg);
          Img := DecodedImg;
        end;
      except
        // ignore decode errors; fall through to card view
      end;
    end;
  end
  else
  begin
    { MIME already resolved in a previous layout pass }
    MimeType := CachedMime;
    if MimeType <> '' then
      ABlock.MimeType := MimeType;
    FImageCache.TryGetValue(URL, Img);
  end;

  { ---- Render as IMAGE ---- }
  if Img <> nil then
  begin
    Scale := AWidth / Img.Width;
    if Img.Height * Scale > MAX_IMG_H then
      Scale := MAX_IMG_H / Img.Height;
    DrawW   := Img.Width  * Scale;
    DrawH   := Img.Height * Scale;
    ImgRect := TRectF.Create(
      AX + (AWidth - DrawW) * 0.5, AY,
      AX + (AWidth - DrawW) * 0.5 + DrawW, AY + DrawH);

    if ACanvas <> nil then
      ACanvas.DrawImageRect(Img, ImgRect,
        TSkSamplingOptions.Create(TSkFilterMode.Linear, TSkMipmapMode.None), nil);

    Result := DrawH;

    if FBuildingLayout then
    begin
      AltText := '';
      if ABlock.Spans.Count > 0 then AltText := ABlock.Spans[0].Alt;
      Entry.DocY      := AY;
      Entry.Height    := DrawH;
      Entry.PlainText := '![' + AltText + '](' + URL + ')';
      FLayoutEntries.Add(Entry);
      FEntryParas.Add(nil);
      FEntryParasX.Add(0);
      IArea.DocRect := ImgRect;
      IArea.URL     := URL;
      FImageAreas.Add(IArea);
    end;
    Exit;
  end;

  { ---- Render as ATTACHMENT CARD ---- }
  AltText := '';
  if ABlock.Spans.Count > 0 then AltText := ABlock.Spans[0].Alt;

  { Extract filename from URL }
  FileName := URL;
  P := FileName.LastIndexOf('/');
  if P >= 0 then FileName := Copy(FileName, P + 2, MaxInt);
  P := FileName.LastIndexOf('\');
  if P >= 0 then FileName := Copy(FileName, P + 2, MaxInt);
  if FileName = '' then FileName := URL;

  { Icon character based on MIME type }
  if MimeType.StartsWith('audio/') then
    IconText := #$266A + '  '                    // ♪
  else if MimeType.StartsWith('video/') then
    IconText := #$25B6 + '  '                    // ▶
  else if MimeType.StartsWith('image/') or (MimeType = '') then
    IconText := #$25FB + '  '                    // ◻ broken/unloaded image
  else
    IconText := #$25A0 + '  ';                   // ■ generic file

  { Build label text and measure its height }
  LabelText := IconText;
  if AltText <> '' then LabelText := LabelText + AltText + '  ' + #$2014 + '  ';
  LabelText := LabelText + FileName;

  TextStyle := MakeTextStyle(FTheme.BaseFontSize, FTheme.TextColor,
                             False, False, False, False);
  ParaStyle := TSkParagraphStyle.Create;
  ParaStyle.TextStyle := TextStyle;
  Builder := TSkParagraphBuilder.Create(ParaStyle, FFontProvider, True);
  Builder.AddText(LabelText);
  Para := Builder.Build;
  Para.Layout(Max(1, AWidth - CARD_PAD * 2));
  TextH := Para.Height;

  CardH    := CARD_PAD + TextH + CARD_PAD + BTN_H + CARD_PAD;
  CardRect := TRectF.Create(AX, AY, AX + AWidth, AY + CardH);

  if ACanvas <> nil then
  begin
    { Card background }
    if (FTheme.BgColor and $00FFFFFF) = $00FFFFFF then
      FFillPaint.Color := $FFF5F5F5
    else
      FFillPaint.Color := $FF2A2A2A;
    ACanvas.DrawRoundRect(CardRect, CARD_RADIUS, CARD_RADIUS, FFillPaint);
    { Card border }
    FLinePaint.Color       := FTheme.QuoteBorderColor;
    FLinePaint.StrokeWidth := 1;
    ACanvas.DrawRoundRect(CardRect, CARD_RADIUS, CARD_RADIUS, FLinePaint);
    { Label row }
    Para.Paint(ACanvas, AX + CARD_PAD, AY + CARD_PAD);
  end;

  { Button row }
  BtnY := AY + CARD_PAD + TextH + CARD_PAD;
  BtnX := AX + CARD_PAD;

  for A := 0 to 2 do
  begin
    case A of
      0: begin BtnLabel := 'Open';  BtnAction := TMediaAction.maOpen; end;
      1: begin BtnLabel := 'Copy';  BtnAction := TMediaAction.maCopy; end;
      2: begin BtnLabel := 'Save';  BtnAction := TMediaAction.maSave; end;
    else
      BtnLabel := ''; BtnAction := TMediaAction.maOpen;
    end;

    BtnRect := TRectF.Create(BtnX, BtnY, BtnX + BTN_W, BtnY + BTN_H);

    if ACanvas <> nil then
    begin
      FFillPaint.Color := FTheme.LinkColor;
      ACanvas.DrawRoundRect(BtnRect, BTN_RADIUS, BTN_RADIUS, FFillPaint);

      TextStyle := MakeTextStyle(FTheme.BaseFontSize * 0.85,
                                 TAlphaColorRec.White,
                                 False, False, False, False);
      ParaStyle := TSkParagraphStyle.Create;
      ParaStyle.TextStyle := TextStyle;
      Builder := TSkParagraphBuilder.Create(ParaStyle, FFontProvider, True);
      Builder.AddText(BtnLabel);
      Para := Builder.Build;
      Para.Layout(Max(1, BTN_W - 8));
      Para.Paint(ACanvas, BtnRect.Left + 4,
                 BtnRect.Top + (BTN_H - Para.Height) * 0.5);
    end;

    if FBuildingLayout then
    begin
      BtnArea.DocY     := BtnY;
      BtnArea.Height   := BTN_H;
      BtnArea.Left     := BtnX;
      BtnArea.Width    := BTN_W;
      BtnArea.URL      := URL;
      BtnArea.MimeType := MimeType;
      BtnArea.Action   := BtnAction;
      FButtonAreas.Add(BtnArea);
    end;

    BtnX := BtnX + BTN_W + BTN_GAP;
  end;

  Result := CardH;

  if FBuildingLayout then
  begin
    Entry.DocY      := AY;
    Entry.Height    := CardH;
    Entry.PlainText := AltText + ' (' + FileName + ')';
    FLayoutEntries.Add(Entry);
    FEntryParas.Add(nil);
    FEntryParasX.Add(0);
  end;
end;

function TAITextMDRenderer.ButtonAreaAtPoint(ADocY, AX: Single;
  out AArea: TMDButtonArea): Boolean;
var
  i: Integer;
  B: TMDButtonArea;
begin
  Result := False;
  for i := 0 to FButtonAreas.Count - 1 do
  begin
    B := FButtonAreas[i];
    if (ADocY >= B.DocY) and (ADocY < B.DocY + B.Height) and
       (AX    >= B.Left) and (AX    < B.Left + B.Width) then
    begin
      AArea  := B;
      Result := True;
      Exit;
    end;
  end;
end;

procedure TAITextMDRenderer.ClearImageCache;
begin
  FImageCache.Clear;
  FMimeCache.Clear;
end;

function TAITextMDRenderer.MeasureHeight(ADocument: TMDDocument; AWidth: Single): Single;
begin
  RebuildLayoutFor(ADocument, AWidth);
  Result := FContentHeightCache;
end;

{ ---------------------------------------------------------------------------
  Image zoom helpers
  --------------------------------------------------------------------------- }

function TAITextMDRenderer.ImageAtPoint(ADocY, AX: Single; out AURL: string): Boolean;
var
  I: Integer;
  R: TRectF;
begin
  for I := 0 to FImageAreas.Count - 1 do
  begin
    R := FImageAreas[I].DocRect;
    if (ADocY >= R.Top) and (ADocY <= R.Bottom) and
       (AX    >= R.Left) and (AX    <= R.Right) then
    begin
      AURL := FImageAreas[I].URL;
      Exit(True);
    end;
  end;
  AURL   := '';
  Result := False;
end;

function TAITextMDRenderer.GetCachedImage(const AURL: string): ISkImage;
begin
  if not FImageCache.TryGetValue(AURL, Result) then
    Result := nil;
end;

end.
