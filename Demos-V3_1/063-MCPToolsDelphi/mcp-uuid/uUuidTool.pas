unit uUuidTool;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, System.JSON,
  uMakerAi.MCPServer.Core;

type
  // --- Tool 1: uuid_generate ---
  TUuidGenerateParams = class
  private
    FCount: Integer;
    FFormat: string;
  public
    [AiMCPSchemaDescription('Number of UUIDs to generate (1-100). Default: 1')]
    [AiMCPOptional]
    property Count: Integer read FCount write FCount;
    [AiMCPSchemaDescription('Output format: "default" (lowercase, no braces), "upper" (uppercase), ' +
      '"braces" ({XXXXXXXX-...}), "urn" (urn:uuid:...). Default: "default"')]
    [AiMCPOptional]
    property Format: string read FFormat write FFormat;
  end;

  TUuidGenerateTool = class(TAiMCPToolBase<TUuidGenerateParams>)
  protected
    function ExecuteWithParams(const AParams: TUuidGenerateParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 2: uuid_validate ---
  TUuidValidateParams = class
  private
    FUuid: string;
  public
    [AiMCPSchemaDescription('UUID string to validate')]
    property Uuid: string read FUuid write FUuid;
  end;

  TUuidValidateTool = class(TAiMCPToolBase<TUuidValidateParams>)
  protected
    function ExecuteWithParams(const AParams: TUuidValidateParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 3: uuid_parse ---
  TUuidParseParams = class
  private
    FUuid: string;
  public
    [AiMCPSchemaDescription('UUID to parse into its component fields and representations')]
    property Uuid: string read FUuid write FUuid;
  end;

  TUuidParseTool = class(TAiMCPToolBase<TUuidParseParams>)
  protected
    function ExecuteWithParams(const AParams: TUuidParseParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

// -------------------------------------------------------------------------
// Helper: format a TGUID according to a format string
// -------------------------------------------------------------------------
function FormatGuid(const G: TGUID; const AFormat: string): string;
var
  Raw: string;
  Fmt: string;
begin
  // GUIDToString returns {XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}
  Raw := GUIDToString(G);
  Fmt := LowerCase(Trim(AFormat));
  if Fmt = '' then
    Fmt := 'default';

  if Fmt = 'braces' then
    Result := Raw
  else if Fmt = 'upper' then
    Result := UpperCase(Copy(Raw, 2, 36))
  else if Fmt = 'urn' then
    Result := 'urn:uuid:' + LowerCase(Copy(Raw, 2, 36))
  else
    // default: lowercase without braces
    Result := LowerCase(Copy(Raw, 2, 36));
end;

// -------------------------------------------------------------------------
// Helper: try to parse a UUID string, normalizing missing braces
// -------------------------------------------------------------------------
function TryParseUUID(const S: string; out G: TGUID): Boolean;
var
  Normalized: string;
begin
  Normalized := Trim(S);
  // If it starts with 'urn:uuid:' strip the prefix
  if StartsText('urn:uuid:', Normalized) then
    Normalized := Copy(Normalized, 10, MaxInt);
  // If no braces, add them
  if (Length(Normalized) > 0) and (Normalized[1] <> '{') then
    Normalized := '{' + Normalized + '}';
  try
    G := StringToGUID(Normalized);
    Result := True;
  except
    Result := False;
  end;
end;

// -------------------------------------------------------------------------
// Helper: extract UUID version from the canonical hex string (no braces)
// Position 12 (0-based) in 'xxxxxxxx-xxxx-Vxxx-xxxx-xxxxxxxxxxxx' is the version nibble
// Canonical string without braces has '-' at positions 8,13,18,23
// Version nibble is at index 14 (0-based) in the 36-char string
// -------------------------------------------------------------------------
function ExtractUUIDVersion(const HexNoBraces: string): Integer;
var
  VersionChar: Char;
  N: Integer;
begin
  // 'xxxxxxxx-xxxx-Vxxx-Xxxx-xxxxxxxxxxxx'
  //  0         9   13   18
  // Position 14 (1-based) holds the version digit
  if Length(HexNoBraces) <> 36 then
  begin
    Result := -1;
    Exit;
  end;
  VersionChar := HexNoBraces[15]; // 1-based; index 15 = offset 14
  if not TryStrToInt('$' + VersionChar, N) then
    Result := -1
  else
    Result := N;
end;

// -------------------------------------------------------------------------
// Helper: extract UUID variant from the 17th char (index 19, 1-based)
// 'xxxxxxxx-xxxx-xxxx-Xxxx-xxxxxxxxxxxx'
// The variant nibble is at position 19 (1-based) in the 36-char string
// Variant bits: top 2 bits of this nibble
//   0x  (0b0xxx) = NCS backward compat
//   10x (0b10xx) = RFC 4122
//   110 (0b110x) = Microsoft
//   111 (0b111x) = reserved
// -------------------------------------------------------------------------
function ExtractUUIDVariant(const HexNoBraces: string): string;
var
  VariantChar: Char;
  N: Integer;
begin
  if Length(HexNoBraces) <> 36 then
  begin
    Result := 'unknown';
    Exit;
  end;
  VariantChar := HexNoBraces[20]; // 1-based position 20
  if not TryStrToInt('$' + VariantChar, N) then
  begin
    Result := 'unknown';
    Exit;
  end;
  if (N shr 3) = 0 then        // 0b0xxx
    Result := 'NCS (0b0xxx)'
  else if (N shr 2) = 2 then   // 0b10xx
    Result := 'RFC 4122 (0b10xx)'
  else if (N shr 1) = 6 then   // 0b110x
    Result := 'Microsoft (0b110x)'
  else
    Result := 'Reserved (0b111x)';
end;

// -------------------------------------------------------------------------
// TUuidGenerateTool
// -------------------------------------------------------------------------

constructor TUuidGenerateTool.Create;
begin
  inherited;
  FName        := 'uuid_generate';
  FDescription := 'Generate one or more UUID v4 (random) values. ' +
                  'Supports multiple output formats: default (lowercase), upper, braces, urn.';
end;

function TUuidGenerateTool.ExecuteWithParams(const AParams: TUuidGenerateParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Count: Integer;
  I: Integer;
  G: TGUID;
  Lines: TStringList;
  Formatted: string;
begin
  Count := AParams.Count;
  if Count <= 0 then
    Count := 1;
  if Count > 100 then
    Count := 100;

  Lines := TStringList.Create;
  try
    for I := 1 to Count do
    begin
      CreateGUID(G);
      Formatted := FormatGuid(G, AParams.Format);
      Lines.Add(Formatted);
    end;
    Result := TAiMCPResponseBuilder.New
      .AddText('Generated ' + IntToStr(Count) + ' UUID(s):' + sLineBreak +
               Lines.Text)
      .Build;
  finally
    Lines.Free;
  end;
end;

// -------------------------------------------------------------------------
// TUuidValidateTool
// -------------------------------------------------------------------------

constructor TUuidValidateTool.Create;
begin
  inherited;
  FName        := 'uuid_validate';
  FDescription := 'Check if a string is a valid UUID. Returns valid/invalid status and version info.';
end;

function TUuidValidateTool.ExecuteWithParams(const AParams: TUuidValidateParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  G: TGUID;
  HexNoBraces: string;
  Version: Integer;
  VersionStr: string;
  Output: string;
begin
  if Trim(AParams.Uuid) = '' then
    raise Exception.Create('UUID input cannot be empty');

  if TryParseUUID(AParams.Uuid, G) then
  begin
    HexNoBraces := LowerCase(Copy(GUIDToString(G), 2, 36));
    Version := ExtractUUIDVersion(HexNoBraces);
    if Version >= 1 then
      VersionStr := 'v' + IntToStr(Version)
    else
      VersionStr := 'unknown';
    Output := 'Valid UUID' + sLineBreak +
              'Input:   ' + AParams.Uuid + sLineBreak +
              'Version: ' + VersionStr + sLineBreak +
              'Variant: ' + ExtractUUIDVariant(HexNoBraces);
  end
  else
    Output := 'Invalid UUID: "' + AParams.Uuid + '"';

  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

// -------------------------------------------------------------------------
// TUuidParseTool
// -------------------------------------------------------------------------

constructor TUuidParseTool.Create;
begin
  inherited;
  FName        := 'uuid_parse';
  FDescription := 'Parse a UUID and return its version, variant, byte count, ' +
                  'and all standard formatted representations.';
end;

function TUuidParseTool.ExecuteWithParams(const AParams: TUuidParseParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  G: TGUID;
  HexNoBraces: string;
  Version: Integer;
  VersionStr: string;
  Output: string;
begin
  if Trim(AParams.Uuid) = '' then
    raise Exception.Create('UUID input cannot be empty');

  if not TryParseUUID(AParams.Uuid, G) then
    raise Exception.CreateFmt('Invalid UUID: "%s"', [AParams.Uuid]);

  HexNoBraces := LowerCase(Copy(GUIDToString(G), 2, 36));
  Version := ExtractUUIDVersion(HexNoBraces);
  if Version >= 1 then
    VersionStr := 'v' + IntToStr(Version)
  else
    VersionStr := 'unknown';

  Output :=
    'Input:     ' + AParams.Uuid + sLineBreak +
    'Version:   ' + VersionStr + sLineBreak +
    'Variant:   ' + ExtractUUIDVariant(HexNoBraces) + sLineBreak +
    'Bytes:     16' + sLineBreak +
    sLineBreak +
    'Representations:' + sLineBreak +
    '  default: ' + HexNoBraces + sLineBreak +
    '  upper:   ' + UpperCase(HexNoBraces) + sLineBreak +
    '  braces:  ' + GUIDToString(G) + sLineBreak +
    '  urn:     urn:uuid:' + HexNoBraces;

  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

end.
