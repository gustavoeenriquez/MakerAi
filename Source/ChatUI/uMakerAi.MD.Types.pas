unit uMakerAi.MD.Types;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Types, System.UITypes;

type

  { -----------------------------------------------------------------------
    Span-level node kinds (inline content inside a block)
    ----------------------------------------------------------------------- }
  TMDSpanKind = (
    skText,          // plain text fragment
    skBold,          // **text** or __text__
    skItalic,        // *text* or _text_
    skBoldItalic,    // ***text***
    skCode,          // `code`
    skLink,          // [label](url)
    skImage,         // ![alt](url)
    skLineBreak,     // trailing two-spaces or <br>
    skStrikethrough, // ~~text~~
    skFootnoteRef    // [^label] → Text=number, URL=label
  );

  { -----------------------------------------------------------------------
    Block-level node kinds
    ----------------------------------------------------------------------- }
  TMDBlockKind = (
    bkParagraph,   // plain paragraph
    bkHeading,     // # ## ### (level 1-6)
    bkCodeBlock,   // fenced ``` or indented
    bkBlockQuote,  // > ...
    bkListItem,    // bullet or ordered item
    bkList,        // container: ordered or unordered
    bkHRule,       // --- / *** / ___
    bkBlank,       // empty line separator
    bkMedia,       // standalone image / attachment card
    bkTable,       // GFM table container; Children = bkTableRow blocks
    bkTableRow,    // one row;  Level=0 → header, Level>0 → data; Children = bkTableCell
    bkTableCell,   // one cell; Spans = inline content
    bkFootnotes,   // footnote section; Children = bkFootnoteDef blocks
    bkFootnoteDef, // one definition; Level=number, RawText=label, Spans=text
    bkDetails,     // <details>...</details> collapsible block
    bkSummary      // <summary>...</summary> header inside bkDetails
  );

  { -----------------------------------------------------------------------
    Table column alignment  (GFM alignment row: |:---|:---:|---:|)
    ----------------------------------------------------------------------- }
  TMDTableAlign = (taLeft, taCenter, taRight);

  { -----------------------------------------------------------------------
    Span node  (inline content)
    ----------------------------------------------------------------------- }
  TMDSpan = class
  public
    Kind    : TMDSpanKind;
    Text    : string;   // raw text (skText, skCode, skLineBreak)
    URL     : string;   // skLink / skImage
    Alt     : string;   // skImage alt text
    Children: TObjectList<TMDSpan>;  // skBold / skItalic / skBoldItalic / skLink label

    constructor Create(AKind: TMDSpanKind; const AText: string = '');
    destructor  Destroy; override;
  end;

  { -----------------------------------------------------------------------
    Block node
    ----------------------------------------------------------------------- }
  TMDBlock = class
  public
    Kind      : TMDBlockKind;
    Level     : Integer;           // bkHeading: 1-6; bkTableRow: 0=header, >0=data
    Ordered   : Boolean;           // bkList: True = ordered (1. 2. 3.)
    StartNum  : Integer;           // bkList: first number (usually 1)
    FenceInfo   : string;            // bkCodeBlock: language hint after ```
    RawText     : string;           // bkCodeBlock / bkHRule / bkMedia: raw content or URL
    MimeType    : string;           // bkMedia: MIME type (filled by OnLoadMedia callback)
    IsTaskItem  : Boolean;          // bkListItem: True = GFM task list item (- [ ] / - [x])
    TaskChecked : Boolean;          // bkListItem: True = [x] checked
    IsOpen      : Boolean;          // bkDetails: True = expanded
    ColCount  : Integer;           // bkTable: number of columns
    ColAlign  : array of TMDTableAlign; // bkTable: per-column alignment [0..ColCount-1]
    Spans     : TObjectList<TMDSpan>;   // inline content for paragraph/heading/item/cell
    Children  : TObjectList<TMDBlock>;  // bkList/bkTable → rows/items; bkBlockQuote → blocks

    constructor Create(AKind: TMDBlockKind);
    destructor  Destroy; override;

    { Convenience: add a plain-text span }
    procedure AddText(const AText: string);
  end;

  { -----------------------------------------------------------------------
    Document  (root of the AST)
    ----------------------------------------------------------------------- }
  TMDDocument = class
  public
    Blocks: TObjectList<TMDBlock>;

    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
  end;

  { -----------------------------------------------------------------------
    Theme  — color + size tokens used by the renderer
    ----------------------------------------------------------------------- }
  TMDTheme = record
    { Background }
    BgColor        : TAlphaColor;

    { Text }
    TextColor      : TAlphaColor;
    CodeColor      : TAlphaColor;      // inline code foreground
    CodeBgColor    : TAlphaColor;      // inline code background
    BlockCodeColor : TAlphaColor;      // fenced block foreground
    BlockCodeBg    : TAlphaColor;      // fenced block background
    LinkColor      : TAlphaColor;
    QuoteBorderColor: TAlphaColor;
    QuoteTextColor : TAlphaColor;

    { Syntax highlighting (fenced code blocks) }
    SHKeyword      : TAlphaColor;
    SHString       : TAlphaColor;
    SHComment      : TAlphaColor;
    SHNumber       : TAlphaColor;
    SHPreprocessor : TAlphaColor;
    SHDiffAdd      : TAlphaColor;   // diff: added line (+ prefix)
    SHDiffRemove   : TAlphaColor;   // diff: removed line (- prefix)

    { Fonts }
    FontFamily     : string;
    MonoFamily     : string;
    BaseFontSize   : Single;           // points; headings scale from this

    { Heading scale factors (H1..H6) }
    HeadingScale   : array[1..6] of Single;

    { Spacing (pixels) }
    ParagraphSpacing : Single;
    LineSpacing      : Single;         // multiplier (1.5 = 150%)
    BlockQuoteIndent : Single;
    ListIndent       : Single;
    CodeBlockPadding : Single;

    { Selection highlight color (ARGB, semi-transparent) }
    SelColor         : TAlphaColor;

    class function Default: TMDTheme; static;
    class function Dark: TMDTheme; static;
  end;

  { -----------------------------------------------------------------------
    Layout entry — one selectable unit produced by the renderer.
    Stored in a list; used for hit-testing and selection highlighting.
    ----------------------------------------------------------------------- }
  TMDLayoutEntry = record
    DocY     : Single;   // top Y in document coordinates (scroll-independent)
    Height   : Single;   // height of this unit in pixels
    PlainText: string;   // plain text for clipboard
  end;

  { -----------------------------------------------------------------------
    Media action — fired when user clicks an attachment button
    ----------------------------------------------------------------------- }
  TMediaAction = (maOpen, maCopy, maSave);

  { -----------------------------------------------------------------------
    Code copy area — "Copy" button overlaid on a fenced code block.
    ----------------------------------------------------------------------- }
  TCodeCopyArea = record
    DocRect              : TRectF;   // bounding box of the Copy button in document coordinates
    Code                 : string;   // raw code text to put on the clipboard
    Block                : TMDBlock; // reference for draw-pass HScroll lookup
    ContentW             : Single;   // Para.LongestLine — full code paragraph width
    VisibleW             : Single;   // visible content area width (AWidth - Pad*2 - GutterW)
    HScroll              : Single;   // current horizontal scroll offset (mutable)
    HScrollTrackDocRect  : TRectF;   // h-scrollbar track rect in document coordinates
    BlockDocRect         : TRectF;   // full block bounds in document coordinates
    GutterW              : Single;   // line-number gutter width (0 = no gutter)
  end;

  { -----------------------------------------------------------------------
    Details toggle area — the summary row of a <details> block.
    ----------------------------------------------------------------------- }
  TDetailsToggleArea = record
    DocRect: TRectF;
    Block  : TMDBlock;
  end;

  { -----------------------------------------------------------------------
    Image area — one zoomable standalone image block.
    ----------------------------------------------------------------------- }
  TMDImageArea = record
    DocRect : TRectF;   // bounding box in document coordinates
    URL     : string;
  end;

  { -----------------------------------------------------------------------
    Link area — one clickable inline hyperlink.
    Stored per text-box returned by ISkParagraph.GetRectsForRange.
    ----------------------------------------------------------------------- }
  TMDLinkArea = record
    DocRect : TRectF;   // bounding box in document coordinates (scroll-independent)
    URL     : string;
  end;

  { -----------------------------------------------------------------------
    Button area — one clickable button on an attachment card.
    Stored in a list by the renderer; used for mouse hit-testing.
    ----------------------------------------------------------------------- }
  TMDButtonArea = record
    DocY    : Single;   // top Y in document coordinates
    Height  : Single;
    Left    : Single;   // X in control-local coordinates
    Width   : Single;
    URL     : string;
    MimeType: string;
    Action  : TMediaAction;
  end;

  { -----------------------------------------------------------------------
    Event types
    ----------------------------------------------------------------------- }
  TAITextMDLinkEvent = procedure(Sender: TObject; const AURL: string) of object;
  TAITextMDEvent     = procedure(Sender: TObject) of object;

  { Called when the renderer needs the raw bytes for a media URL.
    Set AHandled := True and fill AData/AMimeType.
    If only AMimeType is known (no stream), set AHandled := True and leave AData empty
    — the renderer will show an attachment card with the correct icon.
    The renderer owns no stream; provide bytes via AData (TBytes). }
  TAITextMDLoadMediaEvent = procedure(Sender: TObject; const AURL: string;
    out AData: TBytes; out AMimeType: string; out AHandled: Boolean) of object;

  { Fired when the user clicks Open / Copy / Save on an attachment card. }
  TAITextMDMediaActionEvent = procedure(Sender: TObject; const AURL: string;
    const AMimeType: string; AAction: TMediaAction) of object;

implementation

{ TMDSpan }

constructor TMDSpan.Create(AKind: TMDSpanKind; const AText: string);
begin
  inherited Create;
  Kind     := AKind;
  Text     := AText;
  Children := TObjectList<TMDSpan>.Create(True);
end;

destructor TMDSpan.Destroy;
begin
  Children.Free;
  inherited;
end;

{ TMDBlock }

constructor TMDBlock.Create(AKind: TMDBlockKind);
begin
  inherited Create;
  Kind     := AKind;
  Level    := 1;
  Ordered  := False;
  StartNum := 1;
  Spans    := TObjectList<TMDSpan>.Create(True);
  Children := TObjectList<TMDBlock>.Create(True);
end;

destructor TMDBlock.Destroy;
begin
  Spans.Free;
  Children.Free;
  inherited;
end;

procedure TMDBlock.AddText(const AText: string);
begin
  Spans.Add(TMDSpan.Create(TMDSpanKind.skText, AText));
end;

{ TMDDocument }

constructor TMDDocument.Create;
begin
  inherited Create;
  Blocks := TObjectList<TMDBlock>.Create(True);
end;

destructor TMDDocument.Destroy;
begin
  Blocks.Free;
  inherited;
end;

procedure TMDDocument.Clear;
begin
  Blocks.Clear;
end;

{ TMDTheme }

class function TMDTheme.Default: TMDTheme;
begin
  Result.BgColor          := TAlphaColorRec.White;
  Result.TextColor        := $FF1A1A1A;
  Result.CodeColor        := $FFD63384;
  Result.CodeBgColor      := $FFF3F3F3;
  Result.BlockCodeColor   := $FF2B2B2B;
  Result.BlockCodeBg      := $FFF5F5F5;
  Result.LinkColor        := $FF0066CC;
  Result.QuoteBorderColor := $FFCCCCCC;
  Result.QuoteTextColor   := $FF666666;

  Result.SHKeyword        := $FF0000FF;  // blue
  Result.SHString         := $FFA31515;  // dark red
  Result.SHComment        := $FF008000;  // green
  Result.SHNumber         := $FF098658;  // teal
  Result.SHPreprocessor   := $FFAF00DB;  // purple
  Result.SHDiffAdd        := $FF0A6B0A;  // diff added: dark green
  Result.SHDiffRemove     := $FF9B1C1C;  // diff removed: dark red

  Result.FontFamily       := 'Segoe UI';
  Result.MonoFamily       := 'Consolas';
  Result.BaseFontSize     := 14;

  Result.HeadingScale[1]  := 2.00;
  Result.HeadingScale[2]  := 1.60;
  Result.HeadingScale[3]  := 1.35;
  Result.HeadingScale[4]  := 1.15;
  Result.HeadingScale[5]  := 1.00;
  Result.HeadingScale[6]  := 0.85;

  Result.ParagraphSpacing := 10;
  Result.LineSpacing      := 1.5;
  Result.BlockQuoteIndent := 16;
  Result.ListIndent       := 20;
  Result.CodeBlockPadding := 10;

  Result.SelColor         := $600078D4;  // Windows-blue, ~38 % opacity
end;

class function TMDTheme.Dark: TMDTheme;
begin
  Result := TMDTheme.Default;
  Result.BgColor          := $FF1E1E1E;
  Result.TextColor        := $FFD4D4D4;
  Result.CodeColor        := $FF9CDCFE;
  Result.CodeBgColor      := $FF2D2D2D;
  Result.BlockCodeColor   := $FFCE9178;
  Result.BlockCodeBg      := $FF252526;
  Result.LinkColor        := $FF4FC1FF;
  Result.QuoteBorderColor := $FF555555;
  Result.QuoteTextColor   := $FF9E9E9E;
  Result.SHKeyword        := $FF569CD6;  // VS Code blue
  Result.SHString         := $FFCE9178;  // VS Code orange-brown
  Result.SHComment        := $FF6A9955;  // VS Code green
  Result.SHNumber         := $FFB5CEA8;  // VS Code light green
  Result.SHPreprocessor   := $FFC586C0;  // VS Code pink
  Result.SHDiffAdd        := $FF4EC94E;  // diff added: bright green
  Result.SHDiffRemove     := $FFF47474;  // diff removed: bright red/salmon
  Result.SelColor         := $605599EE;  // muted blue, ~38 % opacity
end;

end.
