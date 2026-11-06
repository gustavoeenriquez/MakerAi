unit uRegexTool;

interface

uses
  System.SysUtils, System.JSON,
  System.RegularExpressions,
  uMakerAi.MCPServer.Core;

type
  // --- Tool 1: regex_match ---
  TRegexMatchParams = class
  private
    FText: string;
    FPattern: string;
    FOptions: string;
    FMultiLine: Boolean;
  public
    [AiMCPSchemaDescription('Text to search in')]
    property Text: string read FText write FText;
    [AiMCPSchemaDescription('Regular expression pattern')]
    property Pattern: string read FPattern write FPattern;
    [AiMCPSchemaDescription('Options: comma-separated list of: "i" (case insensitive), ' +
      '"m" (multiline), "s" (single-line / dotall), "x" (extended). Default: none')]
    [AiMCPOptional]
    property Options: string read FOptions write FOptions;
    [AiMCPSchemaDescription('If true, return all matches. If false (default), return only the first match')]
    [AiMCPOptional]
    property MultiLine: Boolean read FMultiLine write FMultiLine;
  end;

  TRegexMatchTool = class(TAiMCPToolBase<TRegexMatchParams>)
  protected
    function ExecuteWithParams(const AParams: TRegexMatchParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 2: regex_replace ---
  TRegexReplaceParams = class
  private
    FText: string;
    FPattern: string;
    FReplacement: string;
    FOptions: string;
  public
    [AiMCPSchemaDescription('Text to perform replacements in')]
    property Text: string read FText write FText;
    [AiMCPSchemaDescription('Regular expression pattern to search for')]
    property Pattern: string read FPattern write FPattern;
    [AiMCPSchemaDescription('Replacement string. Use $1, $2 etc. for capture group back-references')]
    property Replacement: string read FReplacement write FReplacement;
    [AiMCPSchemaDescription('Options: comma-separated list of: "i" (case insensitive), ' +
      '"m" (multiline), "s" (single-line), "x" (extended). Default: none')]
    [AiMCPOptional]
    property Options: string read FOptions write FOptions;
  end;

  TRegexReplaceTool = class(TAiMCPToolBase<TRegexReplaceParams>)
  protected
    function ExecuteWithParams(const AParams: TRegexReplaceParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

uses
  System.Classes;

function ParseRegexOptions(const AOpts: string): TRegExOptions;
var
  Parts: TArray<string>;
  P: string;
begin
  Result := [];
  if Trim(AOpts) = '' then Exit;
  Parts := AOpts.Split([',', ' ']);
  for P in Parts do
  begin
    var Trimmed := Trim(LowerCase(P));
    if      Trimmed = 'i' then Include(Result, roIgnoreCase)
    else if Trimmed = 'm' then Include(Result, roMultiLine)
    else if Trimmed = 's' then Include(Result, roSingleLine)
    else if Trimmed = 'x' then Include(Result, roIgnorePatternSpace);
  end;
end;

// -------------------------------------------------------------------------
// TRegexMatchTool
// -------------------------------------------------------------------------

constructor TRegexMatchTool.Create;
begin
  inherited;
  FName        := 'regex_match';
  FDescription := 'Test a regular expression against text. Returns matched groups or ' +
                  'all matches if MultiLine=true. Returns empty result if no match found.';
end;

function TRegexMatchTool.ExecuteWithParams(const AParams: TRegexMatchParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  RE:     TRegEx;
  Match:  TMatch;
  Opts:   TRegExOptions;
  Resp:   TStringBuilder;
  I:      Integer;
begin
  Opts := ParseRegexOptions(AParams.Options);
  RE   := TRegEx.Create(AParams.Pattern, Opts);

  Resp := TStringBuilder.Create;
  try
    Match := RE.Match(AParams.Text);

    if not Match.Success then
    begin
      Resp.Append('No match found.');
    end
    else if AParams.MultiLine then
    begin
      // All matches
      var MatchNum := 0;
      while Match.Success do
      begin
        Inc(MatchNum);
        Resp.AppendFormat('Match %d: "%s"', [MatchNum, Match.Value]);
        if Match.Groups.Count > 1 then
        begin
          for I := 1 to Match.Groups.Count - 1 do
            Resp.AppendFormat(#10'  Group %d: "%s"', [I, Match.Groups[I].Value]);
        end;
        Resp.AppendLine;
        Match := Match.NextMatch;
      end;
      Resp.AppendFormat('Total: %d match(es)', [MatchNum]);
    end
    else
    begin
      // First match only
      Resp.AppendFormat('Match: "%s"', [Match.Value]);
      if Match.Groups.Count > 1 then
      begin
        Resp.AppendLine;
        for I := 1 to Match.Groups.Count - 1 do
          Resp.AppendFormat('Group %d: "%s"'#10, [I, Match.Groups[I].Value]);
      end;
    end;

    Result := TAiMCPResponseBuilder.New.AddText(Resp.ToString).Build;
  finally
    Resp.Free;
  end;
end;

// -------------------------------------------------------------------------
// TRegexReplaceTool
// -------------------------------------------------------------------------

constructor TRegexReplaceTool.Create;
begin
  inherited;
  FName        := 'regex_replace';
  FDescription := 'Replace all occurrences of a pattern in text with a replacement string. ' +
                  'Supports capture group back-references ($1, $2, ...) in the replacement.';
end;

function TRegexReplaceTool.ExecuteWithParams(const AParams: TRegexReplaceParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  RE:   TRegEx;
  Opts: TRegExOptions;
begin
  Opts   := ParseRegexOptions(AParams.Options);
  RE     := TRegEx.Create(AParams.Pattern, Opts);
  Result := TAiMCPResponseBuilder.New
    .AddText(RE.Replace(AParams.Text, AParams.Replacement))
    .Build;
end;

end.
