unit uTextTransformTool;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Hash, System.NetEncoding,
  uMakerAi.MCPServer.Core;

type
  // --- Tool 1: base64_encode ---
  TBase64EncodeParams = class
  private
    FText: string;
    FEncoding: string;
  public
    [AiMCPSchemaDescription('Text to encode as Base64')]
    property Text: string read FText write FText;
    [AiMCPSchemaDescription('Input text encoding: utf8 (default) or ascii')]
    [AiMCPOptional]
    property Encoding: string read FEncoding write FEncoding;
  end;

  TBase64EncodeTool = class(TAiMCPToolBase<TBase64EncodeParams>)
  protected
    function ExecuteWithParams(const AParams: TBase64EncodeParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 2: base64_decode ---
  TBase64DecodeParams = class
  private
    FBase64: string;
  public
    [AiMCPSchemaDescription('Base64-encoded string to decode')]
    property Base64: string read FBase64 write FBase64;
  end;

  TBase64DecodeTool = class(TAiMCPToolBase<TBase64DecodeParams>)
  protected
    function ExecuteWithParams(const AParams: TBase64DecodeParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 3: compute_hash ---
  TComputeHashParams = class
  private
    FText: string;
    FAlgorithm: string;
  public
    [AiMCPSchemaDescription('Text to hash')]
    property Text: string read FText write FText;
    [AiMCPSchemaDescription('Hash algorithm: md5, sha1, sha256 (default), sha512')]
    [AiMCPOptional]
    property Algorithm: string read FAlgorithm write FAlgorithm;
  end;

  TComputeHashTool = class(TAiMCPToolBase<TComputeHashParams>)
  protected
    function ExecuteWithParams(const AParams: TComputeHashParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 4: string_transform ---
  TStringTransformParams = class
  private
    FText: string;
    FOperation: string;
  public
    [AiMCPSchemaDescription('Text to transform')]
    property Text: string read FText write FText;
    [AiMCPSchemaDescription('Transformation: upper, lower, trim, reverse, title_case, sentence_case, camel_case, snake_case, count_words, count_chars, count_lines, sort_lines, sort_lines_desc, remove_duplicates_lines')]
    property Operation: string read FOperation write FOperation;
  end;

  TStringTransformTool = class(TAiMCPToolBase<TStringTransformParams>)
  protected
    function ExecuteWithParams(const AParams: TStringTransformParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 5: url_encode ---
  TUrlEncodeParams = class
  private
    FText: string;
  public
    [AiMCPSchemaDescription('Text to URL-encode (percent encoding)')]
    property Text: string read FText write FText;
  end;

  TUrlEncodeTool = class(TAiMCPToolBase<TUrlEncodeParams>)
  protected
    function ExecuteWithParams(const AParams: TUrlEncodeParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 6: url_decode ---
  TUrlDecodeParams = class
  private
    FText: string;
  public
    [AiMCPSchemaDescription('URL-encoded text to decode')]
    property Text: string read FText write FText;
  end;

  TUrlDecodeTool = class(TAiMCPToolBase<TUrlDecodeParams>)
  protected
    function ExecuteWithParams(const AParams: TUrlDecodeParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

uses
  System.StrUtils;

// -------------------------------------------------------------------------
// TBase64EncodeTool
// -------------------------------------------------------------------------

constructor TBase64EncodeTool.Create;
begin
  inherited;
  FName        := 'base64_encode';
  FDescription := 'Encode a string to Base64';
end;

function TBase64EncodeTool.ExecuteWithParams(const AParams: TBase64EncodeParams;
  const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New
    .AddText(TNetEncoding.Base64.Encode(AParams.Text))
    .Build;
end;

// -------------------------------------------------------------------------
// TBase64DecodeTool
// -------------------------------------------------------------------------

constructor TBase64DecodeTool.Create;
begin
  inherited;
  FName        := 'base64_decode';
  FDescription := 'Decode a Base64-encoded string back to plain text';
end;

function TBase64DecodeTool.ExecuteWithParams(const AParams: TBase64DecodeParams;
  const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New
    .AddText(TNetEncoding.Base64.Decode(AParams.Base64))
    .Build;
end;

// -------------------------------------------------------------------------
// TComputeHashTool
// -------------------------------------------------------------------------

constructor TComputeHashTool.Create;
begin
  inherited;
  FName        := 'compute_hash';
  FDescription := 'Compute a cryptographic hash of a string (MD5, SHA-1, SHA-256, SHA-512). ' +
                  'Returns lowercase hex digest.';
end;

function TComputeHashTool.ExecuteWithParams(const AParams: TComputeHashParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Algo, Hash: string;
begin
  Algo := LowerCase(Trim(AParams.Algorithm));
  if Algo = '' then Algo := 'sha256';

  if      Algo = 'md5'    then Hash := THashMD5.GetHashString(AParams.Text)
  else if Algo = 'sha1'   then Hash := THashSHA1.GetHashString(AParams.Text)
  else if Algo = 'sha256' then Hash := THashSHA2.GetHashString(AParams.Text)
  else if Algo = 'sha512' then Hash := THashSHA2.GetHashString(AParams.Text, SHA512)
  else
    raise Exception.CreateFmt('Unknown algorithm: "%s". Use: md5, sha1, sha256, sha512', [AParams.Algorithm]);

  Result := TAiMCPResponseBuilder.New
    .AddText(LowerCase(Hash))
    .Build;
end;

// -------------------------------------------------------------------------
// TStringTransformTool
// -------------------------------------------------------------------------

constructor TStringTransformTool.Create;
begin
  inherited;
  FName        := 'string_transform';
  FDescription := 'Apply a text transformation: upper, lower, trim, reverse, ' +
                  'title_case, camel_case, snake_case, count_words, count_chars, ' +
                  'count_lines, sort_lines, remove_duplicates_lines, etc.';
end;

function TitleCase(const S: string): string;
var
  I: Integer;
  PrevSpace: Boolean;
begin
  Result    := LowerCase(S);
  PrevSpace := True;
  for I := 1 to Length(Result) do
  begin
    if PrevSpace and (Result[I] <> ' ') then
      Result[I] := UpCase(Result[I]);
    PrevSpace := Result[I] = ' ';
  end;
end;

function CamelCase(const S: string): string;
var
  Words: TArray<string>;
  I: Integer;
begin
  Words  := S.Split([' ', '_', '-']);
  Result := '';
  for I := 0 to High(Words) do
    if Words[I] <> '' then
    begin
      if I = 0 then
        Result := Result + LowerCase(Words[I])
      else
        Result := Result + UpperCase(Copy(Words[I], 1, 1)) + LowerCase(Copy(Words[I], 2, MaxInt));
    end;
end;

function SnakeCase(const S: string): string;
var
  I: Integer;
begin
  Result := LowerCase(S);
  for I := 1 to Length(Result) do
    if Result[I] = ' ' then Result[I] := '_';
end;

function TStringTransformTool.ExecuteWithParams(const AParams: TStringTransformParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Op:    string;
  Lines: TStringList;
  Text:  string;
begin
  Op   := LowerCase(Trim(AParams.Operation));
  Text := AParams.Text;

  if      Op = 'upper'           then Text := UpperCase(Text)
  else if Op = 'lower'           then Text := LowerCase(Text)
  else if Op = 'trim'            then Text := Trim(Text)
  else if Op = 'trim_left'       then Text := TrimLeft(Text)
  else if Op = 'trim_right'      then Text := TrimRight(Text)
  else if Op = 'reverse'         then Text := ReverseString(Text)
  else if Op = 'title_case'      then Text := TitleCase(Text)
  else if Op = 'sentence_case'   then
  begin
    Text := LowerCase(Text);
    if Length(Text) > 0 then Text[1] := UpCase(Text[1]);
  end
  else if Op = 'camel_case'      then Text := CamelCase(Text)
  else if Op = 'snake_case'      then Text := SnakeCase(Text)
  else if Op = 'count_words'     then Text := IntToStr(Length(Text.Split([' ', #9, #13, #10])))
  else if Op = 'count_chars'     then Text := IntToStr(Length(Text))
  else if Op = 'count_lines'     then
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := Text;
      Text := IntToStr(Lines.Count);
    finally
      Lines.Free;
    end;
  end
  else if Op = 'sort_lines' then
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := Text;
      Lines.Sort;
      Text := Lines.Text;
    finally
      Lines.Free;
    end;
  end
  else if Op = 'sort_lines_desc' then
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := Text;
      Lines.Sort;
      // Reverse
      var I: Integer;
      for I := 0 to (Lines.Count div 2) - 1 do
        Lines.Exchange(I, Lines.Count - 1 - I);
      Text := Lines.Text;
    finally
      Lines.Free;
    end;
  end
  else if Op = 'remove_duplicates_lines' then
  begin
    Lines := TStringList.Create;
    try
      Lines.Duplicates := dupIgnore;
      Lines.Sorted := False;
      var Src := TStringList.Create;
      try
        Src.Text := Text;
        var Seen := TStringList.Create;
        try
          Seen.Sorted := True;
          Seen.Duplicates := dupIgnore;
          var I: Integer;
          for I := 0 to Src.Count - 1 do
          begin
            if Seen.IndexOf(Src[I]) < 0 then
            begin
              Lines.Add(Src[I]);
              Seen.Add(Src[I]);
            end;
          end;
        finally
          Seen.Free;
        end;
      finally
        Src.Free;
      end;
      Text := Lines.Text;
    finally
      Lines.Free;
    end;
  end
  else
    raise Exception.CreateFmt('Unknown operation: "%s"', [AParams.Operation]);

  Result := TAiMCPResponseBuilder.New.AddText(Text).Build;
end;

// -------------------------------------------------------------------------
// TUrlEncodeTool
// -------------------------------------------------------------------------

constructor TUrlEncodeTool.Create;
begin
  inherited;
  FName        := 'url_encode';
  FDescription := 'Percent-encode a string for use in URLs (encodes all non-unreserved characters)';
end;

function TUrlEncodeTool.ExecuteWithParams(const AParams: TUrlEncodeParams;
  const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New
    .AddText(TNetEncoding.URL.Encode(AParams.Text))
    .Build;
end;

// -------------------------------------------------------------------------
// TUrlDecodeTool
// -------------------------------------------------------------------------

constructor TUrlDecodeTool.Create;
begin
  inherited;
  FName        := 'url_decode';
  FDescription := 'Decode a percent-encoded URL string';
end;

function TUrlDecodeTool.ExecuteWithParams(const AParams: TUrlDecodeParams;
  const AuthContext: TAiAuthContext): TJSONObject;
begin
  Result := TAiMCPResponseBuilder.New
    .AddText(TNetEncoding.URL.Decode(AParams.Text))
    .Build;
end;

end.
