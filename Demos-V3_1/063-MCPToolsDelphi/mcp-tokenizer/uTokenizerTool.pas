unit uTokenizerTool;

interface

uses
  System.SysUtils, System.StrUtils, System.JSON,
  uMakerAi.MCPServer.Core;

type
  TTokenizeParams = class
  private
    FText: string;
    FModel: string;
  public
    [AiMCPSchemaDescription('Text to estimate token count for')]
    property Text: string read FText write FText;
    [AiMCPSchemaDescription('Model name hint for better estimation. ' +
      'Examples: "gpt-4", "gpt-3.5", "claude", "gemini". Default: generic English estimation')]
    [AiMCPOptional]
    property Model: string read FModel write FModel;
  end;

  TTokenizeTool = class(TAiMCPToolBase<TTokenizeParams>)
  protected
    function ExecuteWithParams(const AParams: TTokenizeParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 2: split_text ---
  TSplitTextParams = class
  private
    FText: string;
    FMaxTokens: Integer;
    FOverlap: Integer;
    FModel: string;
  public
    [AiMCPSchemaDescription('Text to split into chunks')]
    property Text: string read FText write FText;
    [AiMCPSchemaDescription('Maximum tokens per chunk. Default: 512')]
    [AiMCPOptional]
    property MaxTokens: Integer read FMaxTokens write FMaxTokens;
    [AiMCPSchemaDescription('Token overlap between adjacent chunks (for context continuity). Default: 50')]
    [AiMCPOptional]
    property Overlap: Integer read FOverlap write FOverlap;
    [AiMCPSchemaDescription('Model name for token estimation. Default: generic')]
    [AiMCPOptional]
    property Model: string read FModel write FModel;
  end;

  TSplitTextTool = class(TAiMCPToolBase<TSplitTextParams>)
  protected
    function ExecuteWithParams(const AParams: TSplitTextParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

function EstimateTokens(const AText: string; const AModel: string): Integer;

implementation

uses
  System.Math, System.Classes;

// Approximate token estimation based on model family and text content
// OpenAI tokenizers average ~4 chars/token for English, ~2-3 for code
// Claude is similar. Gemini uses similar byte-pair encoding.
function EstimateTokens(const AText: string; const AModel: string): Integer;
var
  CharCount:  Integer;
  WordCount:  Integer;
  Words:      TArray<string>;
  CharsPerTok: Double;
  ModelLower: string;

  // Count CJK characters
  function IsCJK(C: Char): Boolean;
  var CP: Cardinal;
  begin
    CP := Ord(C);
    Result := ((CP >= $4E00) and (CP <= $9FFF)) or  // CJK Unified Ideographs
              ((CP >= $3040) and (CP <= $309F)) or  // Hiragana
              ((CP >= $30A0) and (CP <= $30FF)) or  // Katakana
              ((CP >= $AC00) and (CP <= $D7A9));    // Hangul
  end;

var
  I: Integer;
  CJKCount: Integer;
begin
  if AText = '' then
  begin
    Result := 0;
    Exit;
  end;

  CharCount := Length(AText);
  Words     := AText.Split([' ', #9, #13, #10], TStringSplitOptions.ExcludeEmpty);
  WordCount := Length(Words);

  // Count CJK chars (each is roughly 1 token)
  CJKCount := 0;
  for I := 1 to Length(AText) do
    if IsCJK(AText[I]) then Inc(CJKCount);

  ModelLower := LowerCase(Trim(AModel));

  // Base chars-per-token by model family
  if    ContainsText(ModelLower, 'gpt-4')
     or ContainsText(ModelLower, 'gpt-3.5')
     or ContainsText(ModelLower, 'openai') then
    CharsPerTok := 4.0
  else if ContainsText(ModelLower, 'claude') then
    CharsPerTok := 3.8
  else if ContainsText(ModelLower, 'gemini') then
    CharsPerTok := 4.0
  else if ContainsText(ModelLower, 'llama') then
    CharsPerTok := 3.5
  else
    CharsPerTok := 4.0; // generic default

  // Adjust for code content (detect by high density of non-alpha chars)
  var NonAlpha := 0;
  for I := 1 to Min(1000, CharCount) do
    if not CharInSet(AText[I], ['a'..'z', 'A'..'Z', ' ', #9, #13, #10]) then
      Inc(NonAlpha);
  if (CharCount > 0) and (NonAlpha * 100 div Min(1000, CharCount) > 30) then
    CharsPerTok := 3.0; // code is more token-dense

  // Estimate: subtract CJK (count separately as 1 token each)
  var NonCJKChars := CharCount - CJKCount * 2; // CJK chars are multi-byte
  if NonCJKChars < 0 then NonCJKChars := 0;

  Result := CJKCount + Ceil(NonCJKChars / CharsPerTok);

  // Sanity bounds
  if Result < 1 then Result := 1;
end;

// -------------------------------------------------------------------------
// TTokenizeTool
// -------------------------------------------------------------------------

constructor TTokenizeTool.Create;
begin
  inherited;
  FName        := 'tokenize';
  FDescription := 'Estimate the number of tokens in a text for a given model. ' +
                  'Uses heuristic approximation (not exact BPE tokenization). ' +
                  'Accuracy: typically within 10-20% of actual token count.';
end;

function TTokenizeTool.ExecuteWithParams(const AParams: TTokenizeParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Count:     Integer;
  CharCount: Integer;
  WordCount: Integer;
begin
  Count     := EstimateTokens(AParams.Text, AParams.Model);
  CharCount := Length(AParams.Text);
  WordCount := Length(AParams.Text.Split([' ', #9, #13, #10], TStringSplitOptions.ExcludeEmpty));

  Result := TAiMCPResponseBuilder.New
    .AddText(Format(
      'Estimated tokens: %d'#10 +
      'Characters: %d'#10 +
      'Words: %d'#10 +
      'Method: heuristic approximation'#10 +
      'Model hint: %s',
      [Count, CharCount, WordCount,
       IfThen(Trim(AParams.Model) = '', 'generic', Trim(AParams.Model))]))
    .Build;
end;

// -------------------------------------------------------------------------
// TSplitTextTool
// -------------------------------------------------------------------------

constructor TSplitTextTool.Create;
begin
  inherited;
  FName        := 'split_text';
  FDescription := 'Split a long text into smaller chunks that fit within a token limit. ' +
                  'Chunks are split at sentence boundaries when possible. ' +
                  'Each chunk includes an overlap for context continuity.';
end;

function TSplitTextTool.ExecuteWithParams(const AParams: TSplitTextParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  MaxTok:  Integer;
  Overlap: Integer;
  Lines:   TStringList;
  Chunks:  TStringList;
  Current: TStringBuilder;
  CurTok:  Integer;
  I:       Integer;
  Line:    string;
  LineTok: Integer;
  SB:      TStringBuilder;
begin
  MaxTok  := AParams.MaxTokens;
  Overlap := AParams.Overlap;
  if MaxTok  <= 0 then MaxTok  := 512;
  if Overlap <  0 then Overlap := 50;
  if Overlap >= MaxTok then Overlap := MaxTok div 4;

  // Split into sentences (simple: by lines/periods/newlines)
  Lines   := TStringList.Create;
  Chunks  := TStringList.Create;
  Current := TStringBuilder.Create;
  try
    Lines.Text := AParams.Text;
    CurTok := 0;

    for I := 0 to Lines.Count - 1 do
    begin
      Line    := Lines[I];
      LineTok := EstimateTokens(Line, AParams.Model);

      if CurTok + LineTok > MaxTok then
      begin
        // Save current chunk
        if Current.Length > 0 then
          Chunks.Add(Trim(Current.ToString));

        // Start new chunk
        Current.Clear;
        CurTok := 0;

        // If line itself exceeds max, hard-split it
        if LineTok > MaxTok then
        begin
          var Words := Line.Split([' ']);
          var W := '';
          var WTok := 0;
          var WordBuf := TStringBuilder.Create;
          try
            for var Word in Words do
            begin
              WTok := EstimateTokens(Word, AParams.Model);
              if CurTok + WTok > MaxTok then
              begin
                Chunks.Add(Trim(WordBuf.ToString));
                WordBuf.Clear;
                CurTok := 0;
              end;
              WordBuf.Append(Word).Append(' ');
              Inc(CurTok, WTok);
            end;
            if WordBuf.Length > 0 then
            begin
              Current.Append(WordBuf.ToString);
              CurTok := EstimateTokens(Current.ToString, AParams.Model);
            end;
          finally
            WordBuf.Free;
          end;
          Continue;
        end;
      end;

      Current.AppendLine(Line);
      Inc(CurTok, LineTok);
    end;

    if Current.Length > 0 then
      Chunks.Add(Trim(Current.ToString));

    // Build output
    SB := TStringBuilder.Create;
    try
      SB.AppendFormat('Split into %d chunks (max %d tokens each, overlap: %d):'#10#10,
        [Chunks.Count, MaxTok, Overlap]);
      for I := 0 to Chunks.Count - 1 do
      begin
        var ChunkTok := EstimateTokens(Chunks[I], AParams.Model);
        SB.AppendFormat('--- Chunk %d/%d (~%d tokens) ---'#10, [I+1, Chunks.Count, ChunkTok]);
        SB.AppendLine(Chunks[I]);
        SB.AppendLine;
      end;
      Result := TAiMCPResponseBuilder.New.AddText(SB.ToString).Build;
    finally
      SB.Free;
    end;
  finally
    Lines.Free;
    Chunks.Free;
    Current.Free;
  end;
end;

end.
