unit AIChat.Bubble;

// MIT License — Gustavo Enríquez <gustavoeenriquez@gmail.com>
// Stateless Skia painter for a single TAIChatMessage bubble.

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math, System.Classes,
  System.DateUtils, System.Generics.Collections,
  System.Skia, FMX.Skia,
  uMakerAi.MD.Types,
  uMakerAi.MD.Parser,
  uMakerAi.MD.Renderer.Skia,
  AIChat.Types;

const
  MD_OUTER_PADDING = 16;  // mirrors OUTER_PADDING in AITextMD.Renderer.Skia

type
  TAIChatBubbleLayout = record
    SlotRect    : TRectF;   // full vertical slot in doc coords
    BubbleRect  : TRectF;   // rounded background rect in doc coords
    ContentRect : TRectF;   // dest rect for MD renderer (includes MD_OUTER_PADDING)
    ContentWidth: Single;   // effective text width inside bubble
    AttachTop   : Single;   // Y of first attachment row in doc coords (0 = none)
    ChipW       : Single;
    ChipH       : Single;
    ChipsPerRow : Integer;
    CopyBtnRect : TRectF;   // doc coords
  end;

  TAIChatBubblePainter = class
  private
    FTheme      : TAIChatTheme;
    FPaint      : ISkPaint;
    FParser     : TMDParser;
    FImageCache : TDictionary<string, ISkImage>;

    procedure EnsurePaint;
    function  FileKindColor(AKind: TChatFileKind): TAlphaColor;
    function  FileKindLabel(AKind: TChatFileKind): string;
    function  BubbleBgColor(ARole: TChatRole): TAlphaColor;
    function  BubbleTextColor(ARole: TChatRole): TAlphaColor;
    function  BubbleIsRight(ARole: TChatRole): Boolean; inline;
    function  ChipRows(ACount: Integer; AChipW, AGap, AAvailW: Single): Integer;
    function  AttachRowsHeight(ACount: Integer; AChipW, AChipH, AGap,
                AAvailW: Single): Single;
    function  ChipHeightFor(AMsg: TAIChatMessage): Single;
    function  GetCachedImage(const APath: string): ISkImage;
    // Paragraph helpers — all Skia vars in outer var section
    function  MakePlainParagraph(const AText, AFamily: string;
                ASize: Single; AColor: TAlphaColor;
                AWidth: Single; AAlign: TSkTextAlign): ISkParagraph;
    function  MeasurePlainH(const AText: string; AWidth, ASize: Single): Single;
  public
    constructor Create(const ATheme: TAIChatTheme);
    destructor  Destroy; override;

    function  ComputeLayout(AMsg: TAIChatMessage; ADocY, ATotalWidth: Single;
                ARenderer: TAITextMDRenderer; ADoc: TMDDocument): TAIChatBubbleLayout;
    function  MeasureHeight(AMsg: TAIChatMessage; ATotalWidth: Single;
                ARenderer: TAITextMDRenderer; ADoc: TMDDocument): Single;

    function  ParseMarkdown(const AText: string): TMDDocument;
    procedure PrepareRenderer(ARenderer: TAITextMDRenderer;
                ADoc: TMDDocument; AContentWidth: Single);
    // Call this while the file still exists to guarantee the thumbnail is in cache
    procedure PreloadImage(const APath: string);

    procedure Paint(ACanvas: ISkCanvas; AMsg: TAIChatMessage;
                ADocY, AScreenY, ATotalWidth: Single;
                ARenderer: TAITextMDRenderer; ADoc: TMDDocument;
                AShowCopy: Boolean; AHovAttach: Integer;
                AHovCopyBtn: Boolean = False; ACopied: Boolean = False;
                AIsSelected: Boolean = False);

    // Returns True if (ADocX, ADocY) is inside a code-block copy button of AMsg.
    // AMsgDocY = the message's top DocY (from FDocYPositions).
    function HitCodeCopyAt(ADocX, ADocY, AMsgDocY, ATotalWidth: Single;
                AMsg: TAIChatMessage; ARenderer: TAITextMDRenderer;
                out ACode: string): Boolean;

    { Returns the X origin of the MD renderer content area for AMsg (renderer coords). }
    function ContentLeftFor(AMsg: TAIChatMessage; ATotalWidth: Single): Single;

    property Theme: TAIChatTheme read FTheme write FTheme;
  end;

implementation

uses
  System.IOUtils;

const
  CHIP_W        = 180;
  CHIP_GAP      = 6;
  CHIP_H_IMAGE  = 80;    // taller chip for image thumbnails
  ATTACH_T_GAP  = 8;
  COPY_SZ       = 22;
  COPY_PAD      = 5;
  TIMESTAMP_H   = 16;
  TIMESTAMP_GAP = 3;
  AVATAR_R      = 13;
  AVATAR_GAP    = 6;

{ ── Internal helpers ─────────────────────────────────────────────────────── }

procedure TAIChatBubblePainter.EnsurePaint;
begin
  if FPaint <> nil then Exit;
  FPaint           := TSkPaint.Create;
  FPaint.AntiAlias := True;
  FPaint.Style     := TSkPaintStyle.Fill;
end;

function TAIChatBubblePainter.FileKindColor(AKind: TChatFileKind): TAlphaColor;
begin
  case AKind of
    TChatFileKind.fkImage   : Result := $FF4A90E2;
    TChatFileKind.fkAudio   : Result := $FF7B68EE;
    TChatFileKind.fkVideo   : Result := $FFE25050;
    TChatFileKind.fkPDF     : Result := $FFDD4B39;
    TChatFileKind.fkDocument: Result := $FF50A050;
  else                        Result := $FF888888;
  end;
end;

function TAIChatBubblePainter.FileKindLabel(AKind: TChatFileKind): string;
begin
  case AKind of
    TChatFileKind.fkImage   : Result := 'IMG';
    TChatFileKind.fkAudio   : Result := 'AUD';
    TChatFileKind.fkVideo   : Result := 'VID';
    TChatFileKind.fkPDF     : Result := 'PDF';
    TChatFileKind.fkDocument: Result := 'DOC';
  else                        Result := 'FIL';
  end;
end;

function TAIChatBubblePainter.BubbleBgColor(ARole: TChatRole): TAlphaColor;
begin
  case ARole of
    TChatRole.crUser     : Result := FTheme.UserBubbleBg;
    TChatRole.crAssistant: Result := FTheme.AssistantBubbleBg;
  else                     Result := FTheme.SystemBubbleBg;
  end;
end;

function TAIChatBubblePainter.BubbleTextColor(ARole: TChatRole): TAlphaColor;
begin
  case ARole of
    TChatRole.crUser     : Result := FTheme.UserTextColor;
    TChatRole.crAssistant: Result := FTheme.AssistantTextColor;
  else                     Result := FTheme.SystemTextColor;
  end;
end;

function TAIChatBubblePainter.BubbleIsRight(ARole: TChatRole): Boolean;
begin
  Result := ARole = TChatRole.crUser;
end;

function TAIChatBubblePainter.ChipRows(ACount: Integer;
  AChipW, AGap, AAvailW: Single): Integer;
var
  CpR: Integer;
begin
  CpR    := Max(1, Trunc((AAvailW + AGap) / (AChipW + AGap)));
  Result := (ACount + CpR - 1) div Max(1, CpR);
end;

function TAIChatBubblePainter.AttachRowsHeight(ACount: Integer;
  AChipW, AChipH, AGap, AAvailW: Single): Single;
var
  Rows: Integer;
begin
  if ACount = 0 then Exit(0);
  Rows   := ChipRows(ACount, AChipW, AGap, AAvailW);
  Result := Rows * AChipH + (Rows - 1) * AGap;
end;


{ ── Paragraph helpers ─────────────────────────────────────────────────────── }

function TAIChatBubblePainter.MakePlainParagraph(const AText, AFamily: string;
  ASize: Single; AColor: TAlphaColor; AWidth: Single;
  AAlign: TSkTextAlign): ISkParagraph;
var
  PStyle  : ISkParagraphStyle;
  TStyle  : ISkTextStyle;
  Builder : ISkParagraphBuilder;
  Para    : ISkParagraph;
begin
  PStyle := TSkParagraphStyle.Create;
  PStyle.TextAlign := AAlign;

  TStyle := TSkTextStyle.Create;
  TStyle.Color        := AColor;
  TStyle.FontSize     := ASize;
  TStyle.FontFamilies := [AFamily, 'Segoe UI', 'Arial'];
  TStyle.FontStyle    := TSkFontStyle.Normal;
  PStyle.TextStyle    := TStyle;

  Builder := TSkParagraphBuilder.Create(PStyle);
  Builder.PushStyle(TStyle);
  Builder.AddText(AText);
  Builder.Pop;

  Para := Builder.Build;
  Para.Layout(Max(1, AWidth));
  Result := Para;
end;

function TAIChatBubblePainter.MeasurePlainH(const AText: string;
  AWidth, ASize: Single): Single;
var
  Para: ISkParagraph;
begin
  Para   := MakePlainParagraph(AText, FTheme.MD.FontFamily, ASize,
                               TAlphaColors.Black, AWidth, TSkTextAlign.Left);
  Result := Para.Height;
end;

{ ── Constructor / Destructor ─────────────────────────────────────────────── }

constructor TAIChatBubblePainter.Create(const ATheme: TAIChatTheme);
begin
  inherited Create;
  FTheme      := ATheme;
  FParser     := TMDParser.Create;
  FImageCache := TDictionary<string, ISkImage>.Create;
end;

destructor TAIChatBubblePainter.Destroy;
begin
  FImageCache.Free;
  FParser.Free;
  inherited;
end;

function TAIChatBubblePainter.ChipHeightFor(AMsg: TAIChatMessage): Single;
var I: Integer;
begin
  Result := FTheme.AttachChipHeight;
  for I := 0 to AMsg.Attachments.Count - 1 do
    if AMsg.Attachments[I].FileKind = TChatFileKind.fkImage then
      Exit(CHIP_H_IMAGE);
end;

procedure TAIChatBubblePainter.PreloadImage(const APath: string);
begin
  GetCachedImage(APath);  // loads into FImageCache; caller discards result
end;

function TAIChatBubblePainter.GetCachedImage(const APath: string): ISkImage;
var
  Bytes: TBytes;
begin
  if FImageCache.TryGetValue(APath, Result) then Exit;
  Result := nil;
  try
    if (APath <> '') and TFile.Exists(APath) then
    begin
      Bytes  := TFile.ReadAllBytes(APath);
      Result := TSkImage.MakeFromEncoded(Bytes);
    end;
  except
    Result := nil;
  end;
  FImageCache.AddOrSetValue(APath, Result);
end;

{ ── ParseMarkdown / PrepareRenderer ──────────────────────────────────────── }

function TAIChatBubblePainter.ParseMarkdown(const AText: string): TMDDocument;
begin
  Result := FParser.Parse(AText);
end;

procedure TAIChatBubblePainter.PrepareRenderer(ARenderer: TAITextMDRenderer;
  ADoc: TMDDocument; AContentWidth: Single);
var
  T: TMDTheme;
begin
  T         := FTheme.MD;
  // Preserve RGB (for light/dark mode detection in renderer) but zero alpha so
  // the renderer skips its own background fill — the bubble background is behind it.
  T.BgColor := FTheme.MD.BgColor and $00FFFFFF;
  ARenderer.Theme := T;
  ARenderer.MeasureHeight(ADoc, AContentWidth + 2 * MD_OUTER_PADDING);
end;

{ ── ComputeLayout ────────────────────────────────────────────────────────── }

function TAIChatBubblePainter.ComputeLayout(AMsg: TAIChatMessage;
  ADocY, ATotalWidth: Single;
  ARenderer: TAITextMDRenderer; ADoc: TMDDocument): TAIChatBubbleLayout;
var
  R                        : TAIChatBubbleLayout;
  MaxBubbleW, BubbleW      : Single;
  BubbleLeft, BubbleRight  : Single;
  ContentLeft, ContentRight, ContentW: Single;
  ContentH, AttH, BubbleH  : Single;
  SlotH, Pad, ChipH, CW    : Single;
  ACount, CpR              : Integer;
begin
  Pad   := FTheme.BubblePadding;
  ChipH := ChipHeightFor(AMsg);
  MaxBubbleW := ATotalWidth * FTheme.MaxBubbleWidthPct;

  if BubbleIsRight(AMsg.Role) then
  begin
    BubbleRight := ATotalWidth - FTheme.OuterPadding;
    BubbleLeft  := BubbleRight - MaxBubbleW;
  end
  else if AMsg.Role = TChatRole.crSystem then
  begin
    CW         := ATotalWidth * 0.6;
    BubbleLeft  := (ATotalWidth - CW) * 0.5;
    BubbleRight := BubbleLeft + CW;
  end
  else
  begin
    BubbleLeft  := FTheme.OuterPadding + AVATAR_R * 2 + AVATAR_GAP;
    BubbleRight := BubbleLeft + MaxBubbleW;
    if BubbleRight > ATotalWidth - FTheme.OuterPadding then
      BubbleRight := ATotalWidth - FTheme.OuterPadding;
  end;
  BubbleW := BubbleRight - BubbleLeft;

  ContentLeft  := BubbleLeft  + Pad;
  ContentRight := BubbleRight - Pad;
  ContentW     := ContentRight - ContentLeft;

  // Content height
  if (AMsg.Role = TChatRole.crAssistant) and (ARenderer <> nil) and
     (ADoc <> nil) then
  begin
    PrepareRenderer(ARenderer, ADoc, ContentW);
    ContentH := ARenderer.ContentHeight - 2 * MD_OUTER_PADDING;
    if ContentH < FTheme.MD.BaseFontSize * 1.4 then
      ContentH := FTheme.MD.BaseFontSize * 1.4;
  end
  else
  begin
    if AMsg.Text = '' then
      ContentH := FTheme.MD.BaseFontSize * 1.4
    else
      ContentH := MeasurePlainH(AMsg.Text, ContentW, FTheme.MD.BaseFontSize);
  end;

  // Attachments
  ACount := AMsg.Attachments.Count;
  AttH   := 0;
  if ACount > 0 then
    AttH := ATTACH_T_GAP +
            AttachRowsHeight(ACount, CHIP_W, ChipH, CHIP_GAP,
                             BubbleW - 2 * Pad);

  BubbleH := Pad + ContentH + Pad;
  SlotH   := BubbleH + AttH + TIMESTAMP_GAP + TIMESTAMP_H + FTheme.BubbleMarginV;

  R.SlotRect   := TRectF.Create(0, ADocY, ATotalWidth, ADocY + SlotH);
  R.BubbleRect := TRectF.Create(BubbleLeft, ADocY,
                                BubbleRight, ADocY + BubbleH);
  R.ContentRect := TRectF.Create(ContentLeft  - MD_OUTER_PADDING,
                                 ADocY + Pad  - MD_OUTER_PADDING,
                                 ContentRight + MD_OUTER_PADDING,
                                 ADocY + Pad  + ContentH + MD_OUTER_PADDING);
  R.ContentWidth := ContentW;

  if ACount > 0 then
  begin
    R.AttachTop := ADocY + BubbleH + ATTACH_T_GAP;
    CpR := Max(1, Trunc((BubbleW - 2 * Pad + CHIP_GAP) / (CHIP_W + CHIP_GAP)));
    R.ChipsPerRow := CpR;
  end
  else
  begin
    R.AttachTop   := 0;
    R.ChipsPerRow := 0;
  end;
  R.ChipW := CHIP_W;
  R.ChipH := ChipH;

  R.CopyBtnRect := TRectF.Create(
    BubbleRight - COPY_PAD - COPY_SZ,
    ADocY + COPY_PAD,
    BubbleRight - COPY_PAD,
    ADocY + COPY_PAD + COPY_SZ);

  Result := R;
end;

{ ── MeasureHeight ────────────────────────────────────────────────────────── }

function TAIChatBubblePainter.MeasureHeight(AMsg: TAIChatMessage;
  ATotalWidth: Single; ARenderer: TAITextMDRenderer;
  ADoc: TMDDocument): Single;
var
  L: TAIChatBubbleLayout;
begin
  L      := ComputeLayout(AMsg, 0, ATotalWidth, ARenderer, ADoc);
  Result := L.SlotRect.Height;
end;

{ ── Paint ────────────────────────────────────────────────────────────────── }

procedure TAIChatBubblePainter.Paint(ACanvas: ISkCanvas; AMsg: TAIChatMessage;
  ADocY, AScreenY, ATotalWidth: Single;
  ARenderer: TAITextMDRenderer; ADoc: TMDDocument;
  AShowCopy: Boolean; AHovAttach: Integer;
  AHovCopyBtn: Boolean; ACopied: Boolean; AIsSelected: Boolean);
var
  Layout       : TAIChatBubbleLayout;
  ScrollOffset : Single;
  BubbleScrR,
  ContentScrR,
  ChipScrR,
  CopyScrR,
  AccentR,
  AvatarBounds : TRectF;
  // ── All Skia interface vars must be in the outer var section ──
  BgPaint      : ISkPaint;
  BorderPaint  : ISkPaint;
  ChipPaint    : ISkPaint;
  AccentPaint  : ISkPaint;
  CopyPaint    : ISkPaint;
  AvPaint      : ISkPaint;
  Para         : ISkParagraph;
  ChipImg      : ISkImage;
  // ──
  ACount       : Integer;
  I            : Integer;
  Att          : TAIChatAttachment;
  UseBadge     : Boolean;
  ImgThumbR,
  ImgFitR      : TRectF;
  ImgScale,
  ImgFW, ImgFH : Single;
  FlowLeft,
  FlowRight,
  FirstRowSY   : Single;
  ChipBgColor  : TAlphaColor;
  HitAreas     : TArray<TAIChatHitArea>;
  HitCount     : Integer;
  Pad          : Single;
  TSStr        : string;
  TSAlign      : TSkTextAlign;
  PlainAlign   : TSkTextAlign;
  AvatarCX,
  AvatarCY     : Single;
  AX, AY       : Single;

  procedure AddHit(AKind: TChatHitKind; const ADocRect: TRectF;
    AAttIdx: Integer = -1; const AURL: string = '');
  var
    H: TAIChatHitArea;
  begin
    H.Kind        := AKind;
    H.DocRect     := ADocRect;
    H.AttachIndex := AAttIdx;
    H.URL         := AURL;
    if HitCount >= Length(HitAreas) then
      SetLength(HitAreas, Max(8, Length(HitAreas) * 2));
    HitAreas[HitCount] := H;
    Inc(HitCount);
  end;

begin
  EnsurePaint;
  HitCount     := 0;
  ScrollOffset := ADocY - AScreenY;
  Pad          := FTheme.BubblePadding;

  Layout := ComputeLayout(AMsg, ADocY, ATotalWidth, ARenderer, ADoc);

  // Convert bubble rect to screen space
  BubbleScrR := TRectF.Create(
    Layout.BubbleRect.Left,
    Layout.BubbleRect.Top    - ScrollOffset,
    Layout.BubbleRect.Right,
    Layout.BubbleRect.Bottom - ScrollOffset);

  // ── 1. Bubble background ──────────────────────────────────────────────────
  BgPaint           := TSkPaint.Create;
  BgPaint.AntiAlias := True;
  BgPaint.Style     := TSkPaintStyle.Fill;
  BgPaint.Color     := BubbleBgColor(AMsg.Role);
  ACanvas.DrawRoundRect(BubbleScrR, FTheme.BubbleRadius, FTheme.BubbleRadius,
                        BgPaint);

  // Selection overlay for non-assistant messages (assistant uses its own renderer)
  if AIsSelected and (AMsg.Role <> TChatRole.crAssistant) then
  begin
    BgPaint.Color     := (FTheme.MD.SelColor and $00FFFFFF) or $60000000;
    BgPaint.AntiAlias := False;
    ACanvas.DrawRoundRect(BubbleScrR, FTheme.BubbleRadius, FTheme.BubbleRadius,
                          BgPaint);
  end;

  if TAlphaColorRec(FTheme.BubbleBorderColor).A > 0 then
  begin
    BorderPaint             := TSkPaint.Create;
    BorderPaint.AntiAlias   := True;
    BorderPaint.Style       := TSkPaintStyle.Stroke;
    BorderPaint.StrokeWidth := 1;
    BorderPaint.Color       := FTheme.BubbleBorderColor;
    ACanvas.DrawRoundRect(BubbleScrR, FTheme.BubbleRadius, FTheme.BubbleRadius,
                          BorderPaint);
  end;

  // ── 2. Content ────────────────────────────────────────────────────────────
  if (AMsg.Role = TChatRole.crAssistant) and (ARenderer <> nil) and
     (ADoc <> nil) then
  begin
    ContentScrR := TRectF.Create(
      Layout.ContentRect.Left,
      Layout.ContentRect.Top    - ScrollOffset,
      Layout.ContentRect.Right,
      Layout.ContentRect.Bottom - ScrollOffset);

    ACanvas.Save;
    ACanvas.ClipRect(BubbleScrR);
    ARenderer.Draw(ACanvas, ADoc, ContentScrR, 0);
    ACanvas.Restore;
  end
  else
  begin
    if AMsg.Role = TChatRole.crSystem then
      PlainAlign := TSkTextAlign.Center
    else
      PlainAlign := TSkTextAlign.Left;
    Para := MakePlainParagraph(
      AMsg.Text, FTheme.MD.FontFamily, FTheme.MD.BaseFontSize,
      BubbleTextColor(AMsg.Role), Layout.ContentWidth, PlainAlign);
    AX := Layout.ContentRect.Left + MD_OUTER_PADDING;
    AY := Layout.BubbleRect.Top   + Pad - ScrollOffset;
    Para.Paint(ACanvas, AX, AY);
  end;

  // ── 3. Attachment chips ───────────────────────────────────────────────────
  ACount := AMsg.Attachments.Count;
  if (Layout.AttachTop > 0) and (ACount > 0) then
  begin
    FlowLeft  := Layout.BubbleRect.Left  + Pad;
    FlowRight := Layout.BubbleRect.Right - Pad;
    FirstRowSY := Layout.AttachTop - ScrollOffset;

    for I := 0 to ACount - 1 do
    begin
      Att := AMsg.Attachments[I];

      // Chip position using layout's ChipsPerRow
      var Row := I div Max(1, Layout.ChipsPerRow);
      var Col := I mod Max(1, Layout.ChipsPerRow);
      ChipScrR := TRectF.Create(
        FlowLeft + Col * (CHIP_W + CHIP_GAP),
        FirstRowSY + Row * (Layout.ChipH + CHIP_GAP),
        FlowLeft + Col * (CHIP_W + CHIP_GAP) + CHIP_W,
        FirstRowSY + Row * (Layout.ChipH + CHIP_GAP) + Layout.ChipH);

      // Background
      ChipBgColor := FTheme.AttachChipBg;
      if I = AHovAttach then
        ChipBgColor := TAlphaColor(ChipBgColor - $18000000);
      ChipPaint           := TSkPaint.Create;
      ChipPaint.AntiAlias := True;
      ChipPaint.Style     := TSkPaintStyle.Fill;
      ChipPaint.Color     := ChipBgColor;
      ACanvas.DrawRoundRect(ChipScrR, FTheme.AttachChipRadius,
                            FTheme.AttachChipRadius, ChipPaint);

      // ── Image thumbnail or badge ──────────────────────────────────────────
      UseBadge := True;
      if Att.FileKind = TChatFileKind.fkImage then
      begin
        ChipImg := GetCachedImage(Att.LocalPath);
        if ChipImg <> nil then
        begin
          UseBadge  := False;
          ImgThumbR := TRectF.Create(ChipScrR.Left + 2, ChipScrR.Top + 2,
                                     ChipScrR.Right - 2,
                                     ChipScrR.Bottom - 20);
          ImgScale  := Min(ImgThumbR.Width  / ChipImg.Width,
                           ImgThumbR.Height / ChipImg.Height);
          ImgFW     := ChipImg.Width  * ImgScale;
          ImgFH     := ChipImg.Height * ImgScale;
          ImgFitR   := TRectF.Create(
            ImgThumbR.CenterPoint.X - ImgFW / 2,
            ImgThumbR.CenterPoint.Y - ImgFH / 2,
            ImgThumbR.CenterPoint.X + ImgFW / 2,
            ImgThumbR.CenterPoint.Y + ImgFH / 2);
          ACanvas.Save;
          ACanvas.ClipRect(ImgThumbR);
          ACanvas.DrawImageRect(ChipImg, ImgFitR,
            TSkSamplingOptions.Create(TSkFilterMode.Linear, TSkMipmapMode.None),
            nil);
          ACanvas.Restore;
          // Filename below thumbnail
          Para := MakePlainParagraph(Att.DisplayName,
            FTheme.MD.FontFamily, FTheme.MD.BaseFontSize * 0.75,
            FTheme.AttachChipText, ChipScrR.Width - 6, TSkTextAlign.Center);
          Para.Paint(ACanvas, ChipScrR.Left + 3, ChipScrR.Bottom - 18);
        end;
      end;

      if UseBadge then
      begin
        // Color accent bar (left edge)
        AccentR := TRectF.Create(ChipScrR.Left, ChipScrR.Top,
                                 ChipScrR.Left + 4, ChipScrR.Bottom);
        AccentPaint           := TSkPaint.Create;
        AccentPaint.AntiAlias := False;
        AccentPaint.Style     := TSkPaintStyle.Fill;
        AccentPaint.Color     := FileKindColor(Att.FileKind);
        ACanvas.Save;
        ACanvas.ClipRect(ChipScrR);
        ACanvas.DrawRect(AccentR, AccentPaint);
        ACanvas.Restore;

        // Kind label
        Para := MakePlainParagraph(FileKindLabel(Att.FileKind),
          FTheme.MD.MonoFamily, 7, FileKindColor(Att.FileKind), 30,
          TSkTextAlign.Center);
        Para.Paint(ACanvas, ChipScrR.Left + 4,
          ChipScrR.Top + (Layout.ChipH - Para.Height) * 0.5);

        // File name + size
        Para := MakePlainParagraph(
          Att.DisplayName + '  ' + FileSizeLabel(Att.FileSize),
          FTheme.MD.FontFamily,
          FTheme.MD.BaseFontSize * 0.78, FTheme.AttachChipText,
          ChipScrR.Right - ChipScrR.Left - 40, TSkTextAlign.Left);
        Para.Paint(ACanvas, ChipScrR.Left + 38,
          ChipScrR.Top + (Layout.ChipH - Para.Height) * 0.5);
      end;

      // Hit area in doc coords
      AddHit(TChatHitKind.hkAttachment,
        TRectF.Create(ChipScrR.Left,  ChipScrR.Top    + ScrollOffset,
                      ChipScrR.Right, ChipScrR.Bottom + ScrollOffset),
        I);
    end;
  end;

  // ── 4. Timestamp ─────────────────────────────────────────────────────────
  TSStr  := FormatDateTime('hh:nn', AMsg.Timestamp);
  if AMsg.Status = TChatMessageStatus.msStreaming then
    TSStr := TSStr + '  •••';
  if AMsg.Status = TChatMessageStatus.msError then
    TSStr := TSStr + '  !';
  TSAlign := TSkTextAlign.Left;
  if BubbleIsRight(AMsg.Role) then TSAlign := TSkTextAlign.Right;

  Para := MakePlainParagraph(TSStr, FTheme.MD.FontFamily,
    FTheme.TimestampFontSize, FTheme.TimestampColor,
    Layout.BubbleRect.Width, TSAlign);
  Para.Paint(ACanvas, Layout.BubbleRect.Left,
    Layout.BubbleRect.Bottom - ScrollOffset + TIMESTAMP_GAP);

  // ── 5. Copy button ────────────────────────────────────────────────────────
  if AShowCopy then
  begin
    CopyScrR := TRectF.Create(
      Layout.CopyBtnRect.Left,
      Layout.CopyBtnRect.Top    - ScrollOffset,
      Layout.CopyBtnRect.Right,
      Layout.CopyBtnRect.Bottom - ScrollOffset);

    // Theme-aware colors: detect light mode by RGB of BgColor (alpha may be 0)
    var BgIsLight: Boolean := (FTheme.MD.BgColor and $00FFFFFF) = $00FFFFFF;
    var CopyBg  : TAlphaColor;
    var CopyText: TAlphaColor;
    var CopyLabel: string;
    if ACopied then
    begin
      CopyBg    := TAlphaColor($FF4CAF50);   // green — same light/dark
      CopyText  := TAlphaColors.White;
      CopyLabel := #$2713;                   // ✓
    end
    else if AHovCopyBtn then
    begin
      if BgIsLight then CopyBg := TAlphaColor($FFBDBDBD)
      else              CopyBg := TAlphaColor($FF505070);
      if BgIsLight then CopyText := TAlphaColor($FF212121)
      else              CopyText := TAlphaColors.White;
      CopyLabel := #$2398;
    end
    else
    begin
      if BgIsLight then CopyBg := TAlphaColor($FFE0E0E0)
      else              CopyBg := TAlphaColor($CC3A3A5A);
      if BgIsLight then CopyText := TAlphaColor($FF212121)
      else              CopyText := TAlphaColors.White;
      CopyLabel := #$2398;
    end;

    CopyPaint           := TSkPaint.Create;
    CopyPaint.AntiAlias := True;
    CopyPaint.Style     := TSkPaintStyle.Fill;
    CopyPaint.Color     := CopyBg;
    ACanvas.DrawRoundRect(CopyScrR, 4, 4, CopyPaint);

    Para := MakePlainParagraph(CopyLabel, FTheme.MD.FontFamily, 11,
      CopyText, COPY_SZ, TSkTextAlign.Center);
    Para.Paint(ACanvas, CopyScrR.Left,
      CopyScrR.Top + (COPY_SZ - Para.Height) * 0.5);

    AddHit(TChatHitKind.hkCopyButton, Layout.CopyBtnRect, -1, AMsg.Text);
  end;

  // ── 6. Avatar (assistant only) ────────────────────────────────────────────
  if AMsg.Role = TChatRole.crAssistant then
  begin
    AvatarCX := Layout.BubbleRect.Left - AVATAR_GAP - AVATAR_R;
    AvatarCY := Layout.BubbleRect.Top  - ScrollOffset + AVATAR_R + 4;

    AvPaint           := TSkPaint.Create;
    AvPaint.AntiAlias := True;
    AvPaint.Style     := TSkPaintStyle.Fill;
    AvPaint.Color     := $FF6666AA;
    ACanvas.DrawCircle(TPointF.Create(AvatarCX, AvatarCY), AVATAR_R, AvPaint);

    AvatarBounds := TRectF.Create(AvatarCX - AVATAR_R, AvatarCY - AVATAR_R,
                                  AvatarCX + AVATAR_R, AvatarCY + AVATAR_R);
    Para := MakePlainParagraph('AI', FTheme.MD.FontFamily, 9,
      TAlphaColors.White, AVATAR_R * 2, TSkTextAlign.Center);
    Para.Paint(ACanvas, AvatarBounds.Left,
      AvatarCY - Para.Height * 0.5);
  end;

  // ── 7. Commit hit areas ───────────────────────────────────────────────────
  SetLength(HitAreas, HitCount);
  AMsg.HitAreas := HitAreas;
end;

{ ── HitCodeCopyAt ────────────────────────────────────────────────────────── }
// Converts chat document coordinates (ADocX, ADocY) to the renderer's internal
// layout coordinates, then asks the renderer whether a code-block copy button
// is at that position.  AMsgDocY is the message's FDocYPositions entry.
// The renderer was laid out with ADest=(0,0,ContentWidth+2*OPad,0),
// so renderer_X = DocX - ContentLeft and renderer_Y = DocY - AMsgDocY - Pad + OPad.

function TAIChatBubblePainter.HitCodeCopyAt(ADocX, ADocY, AMsgDocY,
  ATotalWidth: Single; AMsg: TAIChatMessage; ARenderer: TAITextMDRenderer;
  out ACode: string): Boolean;
var
  BubbleLeft, ContentLeft: Single;
  Pad, RX, RY              : Single;
  BtnRect                  : TRectF;
begin
  Result := False;
  ACode  := '';
  if (ARenderer = nil) or (AMsg.Role <> TChatRole.crAssistant) then Exit;

  Pad      := FTheme.BubblePadding;
  BubbleLeft  := FTheme.OuterPadding + AVATAR_R * 2 + AVATAR_GAP;
  ContentLeft := BubbleLeft + Pad;

  // Layout was done with ADest.Left=0 → renderer draws blocks at AX = MD_OUTER_PADDING.
  // BtnRect.Left (layout) = MD_OUTER_PADDING + ContentWidth - CPY_PAD - CPY_W
  // At chat DocX: ContentLeft + ContentWidth - CPY_PAD - CPY_W
  // So renderer_x = DocX - ContentLeft + MD_OUTER_PADDING
  RX := ADocX - ContentLeft + MD_OUTER_PADDING;
  // Layout was done with ADest.Top=0 → blocks start at AY = MD_OUTER_PADDING.
  // renderer_y = (DocY - AMsgDocY) - Pad + MD_OUTER_PADDING
  RY := (ADocY - AMsgDocY) - Pad + MD_OUTER_PADDING;

  Result := ARenderer.CodeCopyAtPoint(RY, RX, ACode, BtnRect);
end;

function TAIChatBubblePainter.ContentLeftFor(AMsg: TAIChatMessage;
  ATotalWidth: Single): Single;
var
  MaxBW, BubbleLeft: Single;
begin
  if AMsg.Role = TChatRole.crAssistant then
  begin
    BubbleLeft := FTheme.OuterPadding + AVATAR_R * 2 + AVATAR_GAP;
  end
  else
  begin
    MaxBW      := ATotalWidth * FTheme.MaxBubbleWidthPct;
    BubbleLeft := ATotalWidth - FTheme.OuterPadding - MaxBW;
  end;
  Result := BubbleLeft + FTheme.BubblePadding;
end;

end.
