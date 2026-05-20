unit uMakerAi.MD.Parser;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Math,
  uMakerAi.MD.Types;

type
  { -----------------------------------------------------------------------
    TMDParser
    Converts a Markdown string into a TMDDocument (AST).
    Supports:
      Blocks  : headings (#-######), paragraphs, fenced code blocks (```),
                blockquotes (>), unordered lists (- * +),
                ordered lists (1.), horizontal rules (--- *** ___), blank lines
      Spans   : plain text, **bold**, *italic*, ***bold-italic***,
                `inline code`, [link](url), ![image](url), line-break (two spaces)
    ----------------------------------------------------------------------- }
  TMDParser = class
  private
    FLines         : TStringList;
    FDoc           : TMDDocument;
    FCurrent       : Integer;   // current line index
    FFootnoteDefs  : TDictionary<string, string>;  // label → raw definition text
    FFootnoteRefs  : TList<string>;                // ordered list of referenced labels

    { ---- line helpers ---- }
    function  AtEnd: Boolean; inline;
    function  CurrentLine: string; inline;
    procedure Advance; inline;
    function  PeekLine(AOffset: Integer = 1): string;

    { ---- block-level parsers ---- }
    procedure ParseBlocks(ATarget: TObjectList<TMDBlock>);
    function  TryParseSetextHeading(out ABlock: TMDBlock): Boolean;
    function  TryParseHeading   (out ABlock: TMDBlock): Boolean;
    function  TryParseHRule     (out ABlock: TMDBlock): Boolean;
    function  TryParseFencedCode(out ABlock: TMDBlock): Boolean;
    function  TryParseBlockQuote(out ABlock: TMDBlock): Boolean;
    function  TryParseDetails   (out ABlock: TMDBlock): Boolean;
    function  TryParseList      (out ABlock: TMDBlock): Boolean;
    function  TryParseTable     (out ABlock: TMDBlock): Boolean;
    function  ParseParagraph: TMDBlock;

    { ---- footnote helpers ---- }
    procedure CollectFootnoteDefs;
    function  FootnoteNumber(const ALabel: string): Integer;
    function  TrySkipFootnoteDef: Boolean;
    procedure AppendFootnotesBlock(ATarget: TObjectList<TMDBlock>);

    { ---- inline-span parser ---- }
    procedure ParseSpans(const AText: string; ATarget: TObjectList<TMDSpan>);

    { ---- low-level helpers ---- }
    class function IsBlankLine(const S: string): Boolean; static;
    class function IsHRule(const S: string): Boolean; static;
    class function IsTableRow(const S: string): Boolean; static;
    class function StripBlockQuotePrefix(const S: string): string; static;
    class function ListBulletOffset(const S: string; out IsOrdered: Boolean;
                                    out Num: Integer): Integer; static;
    class function CountLeadingChar(const S: string; C: Char): Integer; static;
    class function TrimRight(const S: string): string; static;
    class function ParseTableAlignment(const ACell: string): TMDTableAlign; static;
    class function SplitTableCells(const S: string): TArray<string>; static;
  public
    { Parse AMarkdown and return a new TMDDocument (caller owns it) }
    class function Parse(const AMarkdown: string): TMDDocument; static;
  end;

implementation

{ =========================================================================
  Small helpers
  ========================================================================= }

class function TMDParser.IsBlankLine(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if not CharInSet(S[I], [' ', #9]) then
      Exit(False);
  Result := True;
end;

class function TMDParser.IsHRule(const S: string): Boolean;
var
  Trimmed: string;
  C: Char;
  Count, I: Integer;
begin
  Trimmed := S.Trim;
  if Length(Trimmed) < 3 then Exit(False);
  C := Trimmed[1];
  if not CharInSet(C, ['-', '*', '_']) then Exit(False);
  Count := 0;
  for I := 1 to Length(Trimmed) do
  begin
    if Trimmed[I] = C then Inc(Count)
    else if Trimmed[I] <> ' ' then Exit(False);
  end;
  Result := Count >= 3;
end;

class function TMDParser.StripBlockQuotePrefix(const S: string): string;
var
  I: Integer;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  if (I <= Length(S)) and (S[I] = '>') then
  begin
    Inc(I);
    if (I <= Length(S)) and (S[I] = ' ') then Inc(I);
    Result := Copy(S, I, MaxInt);
  end
  else
    Result := S;
end;

{ Returns the column offset where list content starts (after marker).
  Returns -1 if not a list line. }
class function TMDParser.ListBulletOffset(const S: string; out IsOrdered: Boolean;
  out Num: Integer): Integer;
var
  I, J: Integer;
  NumStr: string;
begin
  Result    := -1;
  IsOrdered := False;
  Num       := 1;
  I := 1;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  if I > Length(S) then Exit;

  // Unordered: - * +
  if CharInSet(S[I], ['-', '*', '+']) then
  begin
    if (I + 1 <= Length(S)) and (S[I + 1] = ' ') then
      Result := I + 2
    else
      Exit;
  end
  else if CharInSet(S[I], ['0'..'9']) then
  begin
    J := I;
    while (J <= Length(S)) and CharInSet(S[J], ['0'..'9']) do Inc(J);
    if (J <= Length(S)) and (S[J] = '.') and (J + 1 <= Length(S)) and (S[J + 1] = ' ') then
    begin
      NumStr    := Copy(S, I, J - I);
      Num       := StrToIntDef(NumStr, 1);
      IsOrdered := True;
      Result    := J + 2;
    end;
  end;
end;

class function TMDParser.CountLeadingChar(const S: string; C: Char): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then Inc(Result)
    else Break;
end;

class function TMDParser.TrimRight(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I >= 1) and CharInSet(S[I], [' ', #9]) do Dec(I);
  Result := Copy(S, 1, I);
end;

{ =========================================================================
  Instance helpers
  ========================================================================= }

function TMDParser.AtEnd: Boolean;
begin
  Result := FCurrent >= FLines.Count;
end;

function TMDParser.CurrentLine: string;
begin
  if FCurrent < FLines.Count then
    Result := FLines[FCurrent]
  else
    Result := '';
end;

procedure TMDParser.Advance;
begin
  Inc(FCurrent);
end;

function TMDParser.PeekLine(AOffset: Integer): string;
var
  Idx: Integer;
begin
  Idx := FCurrent + AOffset;
  if (Idx >= 0) and (Idx < FLines.Count) then
    Result := FLines[Idx]
  else
    Result := '';
end;

{ =========================================================================
  Footnote helpers
  ========================================================================= }

procedure TMDParser.CollectFootnoteDefs;
var
  I          : Integer;
  Line, Lbl  : string;
  BracketEnd : Integer;
  DefText    : string;
begin
  for I := 0 to FLines.Count - 1 do
  begin
    Line := FLines[I];
    if (Length(Line) < 5) or (Line[1] <> '[') or (Line[2] <> '^') then Continue;
    BracketEnd := Pos(']', Line, 3);
    if BracketEnd <= 0 then Continue;
    if (BracketEnd + 1 > Length(Line)) or (Line[BracketEnd + 1] <> ':') then Continue;
    Lbl     := Copy(Line, 3, BracketEnd - 3);
    DefText := Trim(Copy(Line, BracketEnd + 2, MaxInt));
    if not FFootnoteDefs.ContainsKey(Lbl) then
      FFootnoteDefs.Add(Lbl, DefText);
  end;
end;

function TMDParser.FootnoteNumber(const ALabel: string): Integer;
var
  Idx: Integer;
begin
  Idx := FFootnoteRefs.IndexOf(ALabel);
  if Idx < 0 then
  begin
    FFootnoteRefs.Add(ALabel);
    Idx := FFootnoteRefs.Count - 1;
  end;
  Result := Idx + 1;
end;

function TMDParser.TrySkipFootnoteDef: Boolean;
var
  Line      : string;
  BracketEnd: Integer;
begin
  Result := False;
  if FFootnoteDefs = nil then Exit;
  Line := CurrentLine;
  if (Length(Line) < 5) or (Line[1] <> '[') or (Line[2] <> '^') then Exit;
  BracketEnd := Pos(']', Line, 3);
  if BracketEnd <= 0 then Exit;
  if (BracketEnd + 1 > Length(Line)) or (Line[BracketEnd + 1] <> ':') then Exit;
  Advance;
  Result := True;
end;

procedure TMDParser.AppendFootnotesBlock(ATarget: TObjectList<TMDBlock>);
var
  FnBlock  : TMDBlock;
  DefBlock : TMDBlock;
  I        : Integer;
  Lbl      : string;
  DefText  : string;
begin
  if (FFootnoteRefs = nil) or (FFootnoteRefs.Count = 0) then Exit;

  FnBlock := TMDBlock.Create(TMDBlockKind.bkFootnotes);
  for I := 0 to FFootnoteRefs.Count - 1 do
  begin
    Lbl     := FFootnoteRefs[I];
    DefText := '';
    FFootnoteDefs.TryGetValue(Lbl, DefText);
    DefBlock         := TMDBlock.Create(TMDBlockKind.bkFootnoteDef);
    DefBlock.Level   := I + 1;     // footnote number (1-based)
    DefBlock.RawText := Lbl;       // label string
    ParseSpans(DefText, DefBlock.Spans);
    FnBlock.Children.Add(DefBlock);
  end;
  ATarget.Add(FnBlock);
end;

{ =========================================================================
  Inline span parser
  ========================================================================= }

procedure TMDParser.ParseSpans(const AText: string; ATarget: TObjectList<TMDSpan>);
var
  I, Len: Integer;
  Buf: string;

  procedure FlushBuf;
  begin
    if Buf <> '' then
    begin
      ATarget.Add(TMDSpan.Create(TMDSpanKind.skText, Buf));
      Buf := '';
    end;
  end;

  function Remaining: string;
  begin
    Result := Copy(AText, I, MaxInt);
  end;

  function StartsWith(const Sub: string): Boolean;
  begin
    Result := Copy(AText, I, Length(Sub)) = Sub;
  end;

  function FindClose(const Marker: string; StartAfter: Integer): Integer;
  var
    J: Integer;
  begin
    J := StartAfter;
    while J <= Len - Length(Marker) + 1 do
    begin
      if Copy(AText, J, Length(Marker)) = Marker then
        Exit(J);
      Inc(J);
    end;
    Result := -1;
  end;

  procedure ParseNested(const InnerText: string; AKind: TMDSpanKind);
  var
    Span: TMDSpan;
  begin
    Span := TMDSpan.Create(AKind);
    ParseSpans(InnerText, Span.Children);
    ATarget.Add(Span);
  end;

var
  ClosePos: Integer;
  InnerText, URL: string;
  BracketEnd, ParenEnd: Integer;
begin
  Buf := '';
  I   := 1;
  Len := Length(AText);

  while I <= Len do
  begin
    // ---- image: ![alt](url) ----
    if (AText[I] = '!') and (I + 1 <= Len) and (AText[I + 1] = '[') then
    begin
      BracketEnd := Pos(']', AText, I + 2);
      if (BracketEnd > 0) and (BracketEnd + 1 <= Len) and (AText[BracketEnd + 1] = '(') then
      begin
        ParenEnd := Pos(')', AText, BracketEnd + 2);
        if ParenEnd > 0 then
        begin
          FlushBuf;
          InnerText := Copy(AText, I + 2, BracketEnd - I - 2);
          URL       := Copy(AText, BracketEnd + 2, ParenEnd - BracketEnd - 2);
          var Span := TMDSpan.Create(TMDSpanKind.skImage, InnerText);
          Span.URL := URL;
          Span.Alt := InnerText;
          ATarget.Add(Span);
          I := ParenEnd + 1;
          Continue;
        end;
      end;
    end;

    // ---- footnote reference: [^label] ----
    if (AText[I] = '[') and (I + 1 <= Len) and (AText[I + 1] = '^') and
       (FFootnoteDefs <> nil) then
    begin
      var FnEnd := Pos(']', AText, I + 2);
      if FnEnd > 0 then
      begin
        var FnLabel := Copy(AText, I + 2, FnEnd - I - 2);
        if FFootnoteDefs.ContainsKey(FnLabel) then
        begin
          FlushBuf;
          var FnNum := FootnoteNumber(FnLabel);
          var FnSpan := TMDSpan.Create(TMDSpanKind.skFootnoteRef, IntToStr(FnNum));
          FnSpan.URL := FnLabel;
          ATarget.Add(FnSpan);
          I := FnEnd + 1;
          Continue;
        end;
      end;
    end;

    // ---- link: [label](url) ----
    if AText[I] = '[' then
    begin
      BracketEnd := Pos(']', AText, I + 1);
      if (BracketEnd > 0) and (BracketEnd + 1 <= Len) and (AText[BracketEnd + 1] = '(') then
      begin
        ParenEnd := Pos(')', AText, BracketEnd + 2);
        if ParenEnd > 0 then
        begin
          FlushBuf;
          InnerText := Copy(AText, I + 1, BracketEnd - I - 1);
          URL       := Copy(AText, BracketEnd + 2, ParenEnd - BracketEnd - 2);
          var Span := TMDSpan.Create(TMDSpanKind.skLink);
          Span.URL := URL;
          ParseSpans(InnerText, Span.Children);
          ATarget.Add(Span);
          I := ParenEnd + 1;
          Continue;
        end;
      end;
    end;

    // ---- bold+italic: ***text*** ----
    if StartsWith('***') then
    begin
      ClosePos := FindClose('***', I + 3);
      if ClosePos > 0 then
      begin
        FlushBuf;
        InnerText := Copy(AText, I + 3, ClosePos - I - 3);
        ParseNested(InnerText, TMDSpanKind.skBoldItalic);
        I := ClosePos + 3;
        Continue;
      end;
    end;

    // ---- bold: **text** or __text__ ----
    if StartsWith('**') or StartsWith('__') then
    begin
      var Marker := Copy(AText, I, 2);
      ClosePos := FindClose(Marker, I + 2);
      if ClosePos > 0 then
      begin
        FlushBuf;
        InnerText := Copy(AText, I + 2, ClosePos - I - 2);
        ParseNested(InnerText, TMDSpanKind.skBold);
        I := ClosePos + 2;
        Continue;
      end;
    end;

    // ---- italic: *text* or _text_ ----
    if CharInSet(AText[I], ['*', '_']) then
    begin
      var Marker := AText[I];
      ClosePos := FindClose(Marker, I + 1);
      if ClosePos > 0 then
      begin
        FlushBuf;
        InnerText := Copy(AText, I + 1, ClosePos - I - 1);
        ParseNested(InnerText, TMDSpanKind.skItalic);
        I := ClosePos + 1;
        Continue;
      end;
    end;

    // ---- strikethrough: ~~text~~ ----
    if StartsWith('~~') then
    begin
      ClosePos := FindClose('~~', I + 2);
      if ClosePos > 0 then
      begin
        FlushBuf;
        InnerText := Copy(AText, I + 2, ClosePos - I - 2);
        ParseNested(InnerText, TMDSpanKind.skStrikethrough);
        I := ClosePos + 2;
        Continue;
      end;
    end;

    // ---- inline code: `code` ----
    if AText[I] = '`' then
    begin
      ClosePos := FindClose('`', I + 1);
      if ClosePos > 0 then
      begin
        FlushBuf;
        InnerText := Copy(AText, I + 1, ClosePos - I - 1);
        ATarget.Add(TMDSpan.Create(TMDSpanKind.skCode, InnerText));
        I := ClosePos + 1;
        Continue;
      end;
    end;

    // ---- HTML inline tags ----
    if AText[I] = '<' then
    begin
      // Self-closing: <br>  <br/>  <br />
      if StartsWith('<br>') or StartsWith('<br/>') or StartsWith('<br />') then
      begin
        FlushBuf;
        ATarget.Add(TMDSpan.Create(TMDSpanKind.skLineBreak));
        if StartsWith('<br />') then Inc(I, 6)
        else if StartsWith('<br/>') then Inc(I, 5)
        else Inc(I, 4);
        Continue;
      end;
      // Paired tags: determine kind + closing tag
      var HTMLKind : TMDSpanKind := TMDSpanKind.skText;  // sentinel = unmatched
      var HTMLOpen : Integer     := 0;
      var HTMLClose: string      := '';
      if      StartsWith('<strong>') then begin HTMLKind := TMDSpanKind.skBold;          HTMLOpen := 8; HTMLClose := '</strong>'; end
      else if StartsWith('<b>')      then begin HTMLKind := TMDSpanKind.skBold;          HTMLOpen := 3; HTMLClose := '</b>';      end
      else if StartsWith('<em>')     then begin HTMLKind := TMDSpanKind.skItalic;        HTMLOpen := 4; HTMLClose := '</em>';     end
      else if StartsWith('<i>')      then begin HTMLKind := TMDSpanKind.skItalic;        HTMLOpen := 3; HTMLClose := '</i>';      end
      else if StartsWith('<code>')   then begin HTMLKind := TMDSpanKind.skCode;          HTMLOpen := 6; HTMLClose := '</code>';   end
      else if StartsWith('<del>')    then begin HTMLKind := TMDSpanKind.skStrikethrough; HTMLOpen := 5; HTMLClose := '</del>';    end
      else if StartsWith('<s>')      then begin HTMLKind := TMDSpanKind.skStrikethrough; HTMLOpen := 3; HTMLClose := '</s>';      end
      else if StartsWith('<strike>') then begin HTMLKind := TMDSpanKind.skStrikethrough; HTMLOpen := 8; HTMLClose := '</strike>'; end;

      if HTMLOpen > 0 then
      begin
        ClosePos := Pos(HTMLClose, AText, I + HTMLOpen);
        if ClosePos > 0 then
        begin
          FlushBuf;
          InnerText := Copy(AText, I + HTMLOpen, ClosePos - I - HTMLOpen);
          if HTMLKind = TMDSpanKind.skCode then
            ATarget.Add(TMDSpan.Create(TMDSpanKind.skCode, InnerText))
          else
            ParseNested(InnerText, HTMLKind);
          I := ClosePos + Length(HTMLClose);
          Continue;
        end;
      end;
    end;

    // ---- line break: trailing two spaces before newline marker ----
    // (In single-line context we detect "\  " at end of string)
    if (AText[I] = ' ') and (I + 1 <= Len) and (AText[I + 1] = ' ') and (I + 2 > Len) then
    begin
      FlushBuf;
      ATarget.Add(TMDSpan.Create(TMDSpanKind.skLineBreak));
      Break;
    end;

    Buf := Buf + AText[I];
    Inc(I);
  end;

  FlushBuf;
end;

{ =========================================================================
  Block parsers
  ========================================================================= }

function TMDParser.TryParseSetextHeading(out ABlock: TMDBlock): Boolean;
var
  ContentLine, UnderLine: string;
  Ch: Char;
  I, Level: Integer;
begin
  Result := False;
  if AtEnd then Exit;
  ContentLine := CurrentLine;

  // Content line must be non-blank
  if IsBlankLine(ContentLine) then Exit;

  // Don't consume ATX headings, blockquote markers, fence markers
  var Trimmed := ContentLine.TrimLeft;
  var HLevel  := CountLeadingChar(ContentLine, '#');
  if (HLevel >= 1) and (HLevel <= 6) and
     (Length(ContentLine) > HLevel) and (ContentLine[HLevel + 1] = ' ') then Exit;
  if (Trimmed <> '') and (Trimmed[1] = '>') then Exit;
  if (Length(Trimmed) >= 3) and CharInSet(Trimmed[1], ['`', '~']) and
     (CountLeadingChar(Trimmed, Trimmed[1]) >= 3) then Exit;

  // Peek next line: must be all '=' or all '-' (min 1 char)
  UnderLine := Trim(PeekLine(1));
  if Length(UnderLine) < 1 then Exit;
  Ch := UnderLine[1];
  if Ch = '=' then Level := 1
  else if Ch = '-' then Level := 2
  else Exit;

  // All characters in underline must be the same char
  for I := 1 to Length(UnderLine) do
    if UnderLine[I] <> Ch then Exit;

  ABlock       := TMDBlock.Create(TMDBlockKind.bkHeading);
  ABlock.Level := Level;
  ParseSpans(ContentLine.Trim, ABlock.Spans);
  Advance;  // consume content line
  Advance;  // consume underline
  Result := True;
end;

function TMDParser.TryParseHeading(out ABlock: TMDBlock): Boolean;
var
  Line, Content: string;
  Level: Integer;
begin
  Line  := CurrentLine;
  Level := CountLeadingChar(Line, '#');
  if (Level < 1) or (Level > 6) then Exit(False);
  if (Length(Line) <= Level) or (Line[Level + 1] <> ' ') then Exit(False);

  ABlock         := TMDBlock.Create(TMDBlockKind.bkHeading);
  ABlock.Level   := Level;
  Content        := TrimRight(Copy(Line, Level + 2, MaxInt));
  // Strip optional trailing #
  while (Content <> '') and (Content[Length(Content)] = '#') do
    Content := TrimRight(Copy(Content, 1, Length(Content) - 1));
  ParseSpans(Content, ABlock.Spans);
  Advance;
  Result := True;
end;

function TMDParser.TryParseHRule(out ABlock: TMDBlock): Boolean;
begin
  if not IsHRule(CurrentLine) then Exit(False);
  ABlock := TMDBlock.Create(TMDBlockKind.bkHRule);
  Advance;
  Result := True;
end;

function TMDParser.TryParseFencedCode(out ABlock: TMDBlock): Boolean;
var
  Line, Fence, Info: string;
  FenceChar: Char;
  FenceLen: Integer;
  Body: TStringList;
begin
  Line := CurrentLine;
  if Length(Line) < 3 then Exit(False);
  FenceChar := Line[1];
  if not CharInSet(FenceChar, ['`', '~']) then Exit(False);
  FenceLen := CountLeadingChar(Line, FenceChar);
  if FenceLen < 3 then Exit(False);

  Fence := StringOfChar(FenceChar, FenceLen);
  Info  := Trim(Copy(Line, FenceLen + 1, MaxInt));
  Advance;

  Body := TStringList.Create;
  try
    while not AtEnd do
    begin
      Line := CurrentLine;
      if CountLeadingChar(Line, FenceChar) >= FenceLen then
      begin
        Advance;
        Break;
      end;
      Body.Add(Line);
      Advance;
    end;
    ABlock          := TMDBlock.Create(TMDBlockKind.bkCodeBlock);
    ABlock.FenceInfo:= Info;
    ABlock.RawText  := Body.Text;
    // Remove trailing newline added by TStringList
    if (ABlock.RawText <> '') and (ABlock.RawText[Length(ABlock.RawText)] = #10) then
      ABlock.RawText := Copy(ABlock.RawText, 1, Length(ABlock.RawText) - 1);
    if (ABlock.RawText <> '') and (ABlock.RawText[Length(ABlock.RawText)] = #13) then
      ABlock.RawText := Copy(ABlock.RawText, 1, Length(ABlock.RawText) - 1);
  finally
    Body.Free;
  end;
  Result := True;
end;

function TMDParser.TryParseBlockQuote(out ABlock: TMDBlock): Boolean;
var
  Line: string;
  Lines: TStringList;
  Inner: string;
  InnerParser: TMDParser;
begin
  Line := CurrentLine.TrimLeft;
  if (Line = '') or (Line[1] <> '>') then Exit(False);

  Lines := TStringList.Create;
  try
    while not AtEnd do
    begin
      Line := CurrentLine;
      var Stripped := Line.TrimLeft;
      if (Stripped <> '') and (Stripped[1] = '>') then
      begin
        Lines.Add(StripBlockQuotePrefix(Line));
        Advance;
      end
      else if IsBlankLine(Line) then
        Break
      else
      begin
        // Lazy continuation: plain line still part of blockquote
        Lines.Add(Line);
        Advance;
      end;
    end;

    ABlock := TMDBlock.Create(TMDBlockKind.bkBlockQuote);

    // Re-parse inner content recursively
    Inner       := Lines.Text;
    InnerParser := TMDParser.Create;
    try
      InnerParser.FLines   := TStringList.Create;
      InnerParser.FLines.Text := Inner;
      InnerParser.FCurrent := 0;
      InnerParser.FDoc     := TMDDocument.Create;
      InnerParser.ParseBlocks(ABlock.Children);
    finally
      InnerParser.FLines.Free;
      InnerParser.FDoc.Free;
      InnerParser.Free;
    end;
  finally
    Lines.Free;
  end;
  Result := True;
end;

function TMDParser.TryParseDetails(out ABlock: TMDBlock): Boolean;
var
  FirstLine   : string;
  TagEnd      : Integer;
  TagPart     : string;
  IsOpen      : Boolean;
  Lines       : TStringList;
  Line        : string;
  Inner       : string;
  InnerLower  : string;
  SumStart    : Integer;
  SumEnd      : Integer;
  SummaryText : string;
  ContentStr  : string;
  SummaryBlock: TMDBlock;
  InnerParser : TMDParser;
begin
  Result := False;
  FirstLine := CurrentLine;
  if not FirstLine.TrimLeft.ToLower.StartsWith('<details') then Exit;

  { Determine whether 'open' attribute is present in the opening tag }
  TagEnd := Pos('>', FirstLine);
  IsOpen := False;
  if TagEnd > 0 then
  begin
    TagPart := Copy(FirstLine, 1, TagEnd).ToLower;
    IsOpen  := TagPart.Contains(' open') or TagPart.Contains(#9 + 'open');
  end;

  Advance;

  { Collect all lines until </details> or end of input }
  Lines := TStringList.Create;
  try
    while not AtEnd do
    begin
      Line := CurrentLine;
      if Line.TrimLeft.ToLower.StartsWith('</details>') then
      begin
        Advance;
        Break;
      end;
      Lines.Add(Line);
      Advance;
    end;

    Inner      := Lines.Text;
    InnerLower := Inner.ToLower;

    { Extract <summary>...</summary> text }
    SumStart := Pos('<summary>', InnerLower);
    SumEnd   := Pos('</summary>', InnerLower);

    SummaryText  := '';
    ContentStr   := Inner;

    if (SumStart > 0) and (SumEnd > SumStart) then
    begin
      SummaryText := Copy(Inner, SumStart + Length('<summary>'),
                          SumEnd - SumStart - Length('<summary>'));
      ContentStr  := Trim(Copy(Inner, SumEnd + Length('</summary>'), MaxInt));
    end;

    ABlock        := TMDBlock.Create(TMDBlockKind.bkDetails);
    ABlock.IsOpen := IsOpen;

    { bkSummary child — always first (index 0) }
    SummaryBlock := TMDBlock.Create(TMDBlockKind.bkSummary);
    ParseSpans(Trim(SummaryText), SummaryBlock.Spans);
    ABlock.Children.Add(SummaryBlock);

    { Parse body content into remaining children }
    if ContentStr <> '' then
    begin
      InnerParser := TMDParser.Create;
      try
        InnerParser.FLines        := TStringList.Create;
        InnerParser.FLines.Text   := ContentStr;
        InnerParser.FCurrent      := 0;
        InnerParser.FDoc          := TMDDocument.Create;
        InnerParser.ParseBlocks(ABlock.Children);
      finally
        InnerParser.FLines.Free;
        InnerParser.FDoc.Free;
        InnerParser.Free;
      end;
    end;
  finally
    Lines.Free;
  end;

  Result := True;
end;

function TMDParser.TryParseList(out ABlock: TMDBlock): Boolean;
var
  Line: string;
  IsOrdered: Boolean;
  Num, ContentOffset: Integer;
  Item: TMDBlock;
  ItemLines: TStringList;
  ItemContent: string;
  InnerParser: TMDParser;
begin
  Line          := CurrentLine;
  ContentOffset := ListBulletOffset(Line, IsOrdered, Num);
  if ContentOffset < 0 then Exit(False);

  ABlock          := TMDBlock.Create(TMDBlockKind.bkList);
  ABlock.Ordered  := IsOrdered;
  ABlock.StartNum := Num;

  while not AtEnd do
  begin
    Line          := CurrentLine;
    ContentOffset := ListBulletOffset(Line, IsOrdered, Num);
    if ContentOffset < 0 then Break;

    // Gather item lines (first line + indented continuations)
    ItemLines := TStringList.Create;
    try
      ItemLines.Add(Copy(Line, ContentOffset, MaxInt));
      Advance;

      while not AtEnd do
      begin
        var NextLine := CurrentLine;
        if IsBlankLine(NextLine) then Break;
        // Indented continuation (at least 2 spaces or tab)
        if (Length(NextLine) > 1) and CharInSet(NextLine[1], [' ', #9]) then
        begin
          // Strip exactly ContentOffset-1 leading spaces/tabs to preserve
          // relative indentation for sub-sub-lists (TrimLeft would lose it)
          var StripN  := ContentOffset - 1;
          var SPos    := 1;
          var Stripped := 0;
          while (Stripped < StripN) and (SPos <= Length(NextLine)) do
          begin
            if NextLine[SPos] = ' ' then
            begin
              Inc(SPos);
              Inc(Stripped);
            end
            else if NextLine[SPos] = #9 then
            begin
              Inc(SPos);
              Inc(Stripped, 4);  // tab counts as 4 spaces
            end
            else
              Break;
          end;
          ItemLines.Add(Copy(NextLine, SPos, MaxInt));
          Advance;
        end
        else
          Break;
      end;

      Item := TMDBlock.Create(TMDBlockKind.bkListItem);

      { GFM task list: - [ ] text  or  - [x] text }
      var FirstLine := ItemLines[0];
      if (Length(FirstLine) >= 4) and
         (FirstLine[1] = '[') and (FirstLine[3] = ']') and (FirstLine[4] = ' ') then
      begin
        if FirstLine[2] = ' ' then
        begin
          Item.IsTaskItem  := True;
          Item.TaskChecked := False;
          ItemLines[0]     := Copy(FirstLine, 5, MaxInt);
        end
        else if (FirstLine[2] = 'x') or (FirstLine[2] = 'X') then
        begin
          Item.IsTaskItem  := True;
          Item.TaskChecked := True;
          ItemLines[0]     := Copy(FirstLine, 5, MaxInt);
        end;
      end;

      if ItemLines.Count = 1 then
        ParseSpans(ItemLines[0], Item.Spans)
      else
      begin
        ItemContent := ItemLines.Text;
        InnerParser := TMDParser.Create;
        try
          InnerParser.FLines   := TStringList.Create;
          InnerParser.FLines.Text := ItemContent;
          InnerParser.FCurrent := 0;
          InnerParser.FDoc     := TMDDocument.Create;
          InnerParser.ParseBlocks(Item.Children);
        finally
          InnerParser.FLines.Free;
          InnerParser.FDoc.Free;
          InnerParser.Free;
        end;
      end;
      ABlock.Children.Add(Item);
    finally
      ItemLines.Free;
    end;

    // Skip blank line between items
    if not AtEnd and IsBlankLine(CurrentLine) then
      Advance;
  end;

  Result := True;
end;

function TMDParser.ParseParagraph: TMDBlock;
var
  Lines: TStringList;
  Line : string;
  _B   : Boolean;
  _N   : Integer;
begin
  Lines := TStringList.Create;
  try
    while not AtEnd do
    begin
      Line := CurrentLine;
      if IsBlankLine(Line) then Break;
      // Stop at block-level starters
      if IsHRule(Line) then Break;
      { Only break for a valid ATX heading (level 1-6 followed by a space).
        A bare "##" or "###text" without the space is NOT a heading — treat it
        as paragraph text; otherwise ParseBlocks would loop forever when the
        stream delivers a partial heading with no trailing space yet. }
      var HLevel := CountLeadingChar(Line, '#');
      if (HLevel >= 1) and (HLevel <= 6) and
         (Length(Line) > HLevel) and (Line[HLevel + 1] = ' ') then Break;
      if (Line <> '') and CharInSet(Line.TrimLeft[1], ['>']) then Break;
      if (Line.Length >= 3) and
         CharInSet(Line[1], ['`','~']) and
         (CountLeadingChar(Line, Line[1]) >= 3)  then Break;
      if ListBulletOffset(Line, _B, _N) >= 0 then Break;
      if IsTableRow(Line) then Break;

      // Trailing two-space line-break: keep the spaces (ParseSpans will detect)
      Lines.Add(Line);
      Advance;
    end;

    Result := TMDBlock.Create(TMDBlockKind.bkParagraph);
    // Join lines with a single space, preserving trailing two-space breaks
    var Combined := '';
    for var K := 0 to Lines.Count - 1 do
    begin
      var L := Lines[K];
      if Combined <> '' then
      begin
        if TrimRight(Lines[K - 1]).EndsWith('  ') then
          Combined := Combined + #10    // hard line break
        else
          Combined := Combined + ' ';
      end;
      Combined := Combined + L.Trim;
    end;
    ParseSpans(Combined, Result.Spans);
  finally
    Lines.Free;
  end;
end;

{ =========================================================================
  Main block dispatcher
  ========================================================================= }

procedure TMDParser.ParseBlocks(ATarget: TObjectList<TMDBlock>);
var
  Block: TMDBlock;
begin
  while not AtEnd do
  begin
    // Blank line → skip
    if IsBlankLine(CurrentLine) then
    begin
      Advance;
      Continue;
    end;

    // Skip footnote definition lines (already collected in pre-scan)
    if TrySkipFootnoteDef then Continue;

    if TryParseSetextHeading(Block)   then ATarget.Add(Block)
    else if TryParseHeading(Block)    then ATarget.Add(Block)
    else if TryParseHRule(Block)      then ATarget.Add(Block)
    else if TryParseFencedCode(Block) then ATarget.Add(Block)
    else if TryParseBlockQuote(Block) then ATarget.Add(Block)
    else if TryParseDetails(Block)    then ATarget.Add(Block)
    else if TryParseList(Block)       then ATarget.Add(Block)
    else if TryParseTable(Block)      then ATarget.Add(Block)
    else
    begin
      var PosBefore := FCurrent;
      var Para := ParseParagraph;
      ATarget.Add(Para);
      { Promote a paragraph that contains only a single image span → bkMedia }
      if (Para.Kind = TMDBlockKind.bkParagraph) and
         (Para.Spans.Count = 1) and
         (Para.Spans[0].Kind = TMDSpanKind.skImage) then
      begin
        Para.Kind    := TMDBlockKind.bkMedia;
        Para.RawText := Para.Spans[0].URL;
      end;
      { Safety guard: if ParseParagraph consumed nothing (all break conditions
        fired on the first line without advancing), force-skip that line to
        prevent an infinite loop. This can happen with unrecognised partial
        block markers delivered by streaming. }
      if FCurrent = PosBefore then
        Advance;
    end;
  end;
end;

{ =========================================================================
  Table helpers
  ========================================================================= }

class function TMDParser.IsTableRow(const S: string): Boolean;
var
  T: string;
begin
  T := Trim(S);
  Result := (Length(T) >= 1) and T.Contains('|');
end;

class function TMDParser.SplitTableCells(const S: string): TArray<string>;
var
  T    : string;
  Parts: TArray<string>;
  I    : Integer;
begin
  T := Trim(S);
  if (T <> '') and (T[1] = '|') then
    T := Copy(T, 2, MaxInt);
  if (T <> '') and (T[Length(T)] = '|') then
    T := Copy(T, 1, Length(T) - 1);
  Parts := T.Split(['|']);
  for I := 0 to High(Parts) do
    Parts[I] := Trim(Parts[I]);
  Result := Parts;
end;

class function TMDParser.ParseTableAlignment(const ACell: string): TMDTableAlign;
var
  T       : string;
  HasLeft : Boolean;
  HasRight: Boolean;
begin
  T        := Trim(ACell);
  HasLeft  := (T <> '') and (T[1] = ':');
  HasRight := (T <> '') and (T[Length(T)] = ':');
  if HasLeft and HasRight then
    Result := TMDTableAlign.taCenter
  else if HasRight then
    Result := TMDTableAlign.taRight
  else
    Result := TMDTableAlign.taLeft;
end;

function TMDParser.TryParseTable(out ABlock: TMDBlock): Boolean;
var
  HeaderLine, AlignLine: string;
  Headers, AlignCells : TArray<string>;
  ColCount, C         : Integer;
  Row, Cell           : TMDBlock;
  CellTexts           : TArray<string>;

  function IsAlignCell(const S: string): Boolean;
  var
    T : string;
    Ch: Char;
  begin
    T := Trim(S);
    if T = '' then Exit(False);
    for Ch in T do
      if not CharInSet(Ch, ['-', ':', ' ']) then Exit(False);
    Result := T.Contains('-');
  end;

begin
  Result := False;
  ABlock := nil;

  if AtEnd then Exit;
  HeaderLine := CurrentLine;
  if not IsTableRow(HeaderLine) then Exit;

  AlignLine := PeekLine(1);
  if not IsTableRow(AlignLine) then Exit;

  AlignCells := SplitTableCells(AlignLine);
  for var I := 0 to High(AlignCells) do
    if not IsAlignCell(AlignCells[I]) then Exit;
  if Length(AlignCells) = 0 then Exit;

  // Commit: valid GFM table
  Headers  := SplitTableCells(HeaderLine);
  ColCount := Max(Length(Headers), Length(AlignCells));

  ABlock          := TMDBlock.Create(TMDBlockKind.bkTable);
  ABlock.ColCount := ColCount;
  SetLength(ABlock.ColAlign, ColCount);
  for C := 0 to ColCount - 1 do
  begin
    if C <= High(AlignCells) then
      ABlock.ColAlign[C] := ParseTableAlignment(AlignCells[C])
    else
      ABlock.ColAlign[C] := TMDTableAlign.taLeft;
  end;

  // Header row (Level = 0)
  Row       := TMDBlock.Create(TMDBlockKind.bkTableRow);
  Row.Level := 0;
  for C := 0 to ColCount - 1 do
  begin
    Cell := TMDBlock.Create(TMDBlockKind.bkTableCell);
    if C <= High(Headers) then
      ParseSpans(Headers[C], Cell.Spans);
    Row.Children.Add(Cell);
  end;
  ABlock.Children.Add(Row);
  Advance;  // consume header line
  Advance;  // consume alignment line

  // Data rows
  var DataIdx := 1;
  while not AtEnd do
  begin
    var DataLine := CurrentLine;
    if not IsTableRow(DataLine) then Break;
    CellTexts := SplitTableCells(DataLine);

    Row       := TMDBlock.Create(TMDBlockKind.bkTableRow);
    Row.Level := DataIdx;
    Inc(DataIdx);
    for C := 0 to ColCount - 1 do
    begin
      Cell := TMDBlock.Create(TMDBlockKind.bkTableCell);
      if C <= High(CellTexts) then
        ParseSpans(CellTexts[C], Cell.Spans);
      Row.Children.Add(Cell);
    end;
    ABlock.Children.Add(Row);
    Advance;
  end;

  Result := True;
end;

{ =========================================================================
  Public entry point
  ========================================================================= }

class function TMDParser.Parse(const AMarkdown: string): TMDDocument;
var
  Parser: TMDParser;
begin
  Parser := TMDParser.Create;
  try
    Parser.FLines        := TStringList.Create;
    Parser.FDoc          := TMDDocument.Create;
    Parser.FCurrent      := 0;
    Parser.FFootnoteDefs := TDictionary<string, string>.Create;
    Parser.FFootnoteRefs := TList<string>.Create;
    try
      Parser.FLines.Text := AMarkdown;
      Parser.CollectFootnoteDefs;
      Parser.ParseBlocks(Parser.FDoc.Blocks);
      Parser.AppendFootnotesBlock(Parser.FDoc.Blocks);
      Result      := Parser.FDoc;
      Parser.FDoc := nil;  // transfer ownership
    finally
      Parser.FFootnoteRefs.Free;
      Parser.FFootnoteDefs.Free;
      Parser.FLines.Free;
      Parser.FDoc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

end.
