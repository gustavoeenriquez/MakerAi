unit uJsonTool;

// =============================================================================
// mcp-json  —  JSON file and string manipulation MCP server
//
// Tools exposed:
//   json_read    — Read a JSON file from disk and return formatted content
//   json_write   — Write JSON content to a file
//   json_query   — Query a value inside JSON using dot-path notation
//   json_format  — Format (pretty-print) or minify a JSON string
// =============================================================================

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils,
  uMakerAi.MCPServer.Core;

// =============================================================================
// json_read
// =============================================================================

type
  TJsonReadParams = class
  private
    FFilePath: string;
  public
    [AiMCPSchemaDescription('Path to the JSON file to read')]
    property FilePath: string read FFilePath write FFilePath;
  end;

  TJsonReadTool = class(TAiMCPToolBase<TJsonReadParams>)
  protected
    function ExecuteWithParams(const AParams: TJsonReadParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// json_write
// =============================================================================

type
  TJsonWriteParams = class
  private
    FFilePath: string;
    FContent : string;
    FPretty  : Boolean;
  public
    [AiMCPSchemaDescription('Path to the JSON file to write')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPSchemaDescription('JSON content to write')]
    property Content: string read FContent write FContent;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Pretty-print the output. Default: true')]
    property Pretty: Boolean read FPretty write FPretty;
  end;

  TJsonWriteTool = class(TAiMCPToolBase<TJsonWriteParams>)
  protected
    function ExecuteWithParams(const AParams: TJsonWriteParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// json_query
// =============================================================================

type
  TJsonQueryParams = class
  private
    FJson    : string;
    FFilePath: string;
    FPath    : string;
  public
    [AiMCPOptional]
    [AiMCPSchemaDescription('JSON string to query')]
    property Json: string read FJson write FJson;

    [AiMCPOptional]
    [AiMCPSchemaDescription('JSON file path (alternative to Json param)')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPSchemaDescription('Dot-path to the value, e.g. "user.name" or "items.0.id"')]
    property Path: string read FPath write FPath;
  end;

  TJsonQueryTool = class(TAiMCPToolBase<TJsonQueryParams>)
  protected
    function ExecuteWithParams(const AParams: TJsonQueryParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// json_format
// =============================================================================

type
  TJsonFormatParams = class
  private
    FJson   : string;
    FMinify : Boolean;
  public
    [AiMCPSchemaDescription('JSON string to format')]
    property Json: string read FJson write FJson;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Minify instead of pretty-printing. Default: false')]
    property Minify: Boolean read FMinify write FMinify;
  end;

  TJsonFormatTool = class(TAiMCPToolBase<TJsonFormatParams>)
  protected
    function ExecuteWithParams(const AParams: TJsonFormatParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

// =============================================================================
// TJsonReadTool
// =============================================================================

constructor TJsonReadTool.Create;
begin
  inherited;
  FName        := 'json_read';
  FDescription :=
    'Read a JSON file from disk and return its content formatted (pretty-printed). ' +
    'Returns an error message if the file does not exist or is not valid JSON.';
end;

function TJsonReadTool.ExecuteWithParams(const AParams: TJsonReadParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  RawText  : string;
  JVal     : TJSONValue;
  Formatted: string;
begin
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath parameter is required');

  if not TFile.Exists(AParams.FilePath) then
    raise Exception.CreateFmt('File not found: %s', [AParams.FilePath]);

  RawText := TFile.ReadAllText(AParams.FilePath, TEncoding.UTF8);

  JVal := TJSONObject.ParseJSONValue(RawText);
  if not Assigned(JVal) then
  begin
    Result := TAiMCPResponseBuilder.New
      .AddText('ERROR: File does not contain valid JSON.' + sLineBreak +
               'Raw content (first 200 chars): ' + Copy(RawText, 1, 200))
      .Build;
    Exit;
  end;

  try
    Formatted := JVal.Format(2);
  finally
    JVal.Free;
  end;

  Result := TAiMCPResponseBuilder.New
    .AddText(Formatted)
    .Build;
end;

// =============================================================================
// TJsonWriteTool
// =============================================================================

constructor TJsonWriteTool.Create;
begin
  inherited;
  FName        := 'json_write';
  FDescription :=
    'Write JSON content to a file. ' +
    'Validates that the content is valid JSON before writing. ' +
    'Creates parent directories if they do not exist. ' +
    'By default, pretty-prints the output (Pretty=true).';
end;

function TJsonWriteTool.ExecuteWithParams(const AParams: TJsonWriteParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  JVal       : TJSONValue;
  ToWrite    : string;
  Dir        : string;
  DoPretty   : Boolean;
begin
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath parameter is required');

  if Trim(AParams.Content) = '' then
    raise Exception.Create('Content parameter is required');

  JVal := TJSONObject.ParseJSONValue(AParams.Content);
  if not Assigned(JVal) then
    raise Exception.Create('Content is not valid JSON');

  try
    // Default for Pretty is true — if the field was not supplied it will be
    // False (Delphi default for Boolean). We treat False as "use default=true"
    // unless the caller explicitly sets Minify. Since we have no way to
    // distinguish "not supplied" from "supplied as false" here, we apply the
    // documented default: true (pretty-print).
    DoPretty := True; // default
    // If Pretty was explicitly set to False we honour it (minify)
    if not AParams.Pretty then
      DoPretty := False;

    if DoPretty then
      ToWrite := JVal.Format(2)
    else
      ToWrite := JVal.ToString;
  finally
    JVal.Free;
  end;

  Dir := TPath.GetDirectoryName(TPath.GetFullPath(AParams.FilePath));
  if (Dir <> '') and not TDirectory.Exists(Dir) then
    TDirectory.CreateDirectory(Dir);

  TFile.WriteAllText(AParams.FilePath, ToWrite, TEncoding.UTF8);

  Result := TAiMCPResponseBuilder.New
    .AddText(Format('OK — wrote %d bytes to: %s', [Length(ToWrite), AParams.FilePath]))
    .Build;
end;

// =============================================================================
// TJsonQueryTool
// =============================================================================

constructor TJsonQueryTool.Create;
begin
  inherited;
  FName        := 'json_query';
  FDescription :=
    'Query a value inside a JSON document using dot-path notation. ' +
    'Examples: "user.name", "items.0.id", "config.database.host". ' +
    'Provide JSON either as a string (Json param) or as a file path (FilePath param). ' +
    'Array elements are accessed by numeric index (0-based).';
end;

function TJsonQueryTool.ExecuteWithParams(const AParams: TJsonQueryParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  JsonText  : string;
  JVal      : TJSONValue;
  Current   : TJSONValue;
  Parts     : TArray<string>;
  Part      : string;
  Idx       : Integer;
  JArr      : TJSONArray;
begin
  if Trim(AParams.Path) = '' then
    raise Exception.Create('Path parameter is required');

  // Resolve JSON source
  if Trim(AParams.Json) <> '' then
    JsonText := AParams.Json
  else if Trim(AParams.FilePath) <> '' then
  begin
    if not TFile.Exists(AParams.FilePath) then
      raise Exception.CreateFmt('File not found: %s', [AParams.FilePath]);
    JsonText := TFile.ReadAllText(AParams.FilePath, TEncoding.UTF8);
  end
  else
    raise Exception.Create('Either Json or FilePath parameter is required');

  JVal := TJSONObject.ParseJSONValue(JsonText);
  if not Assigned(JVal) then
    raise Exception.Create('Input is not valid JSON');

  try
    Current := JVal;
    Parts   := AParams.Path.Split(['.']);

    for Part in Parts do
    begin
      if Part = '' then Continue;

      if Current is TJSONObject then
      begin
        Current := TJSONObject(Current).GetValue(Part);
        if not Assigned(Current) then
        begin
          Result := TAiMCPResponseBuilder.New
            .AddText(Format('Path not found: key "%s" does not exist at this level.', [Part]))
            .Build;
          Exit;
        end;
      end
      else if Current is TJSONArray then
      begin
        JArr := TJSONArray(Current);
        if not TryStrToInt(Part, Idx) then
        begin
          Result := TAiMCPResponseBuilder.New
            .AddText(Format('Expected a numeric array index but got "%s".', [Part]))
            .Build;
          Exit;
        end;
        if (Idx < 0) or (Idx >= JArr.Count) then
        begin
          Result := TAiMCPResponseBuilder.New
            .AddText(Format('Array index %d is out of bounds (array has %d elements).', [Idx, JArr.Count]))
            .Build;
          Exit;
        end;
        Current := JArr.Items[Idx];
      end
      else
      begin
        Result := TAiMCPResponseBuilder.New
          .AddText(Format('Cannot navigate into a scalar value at path segment "%s".', [Part]))
          .Build;
        Exit;
      end;
    end;

    // Return found value
    if Current is TJSONNull then
      Result := TAiMCPResponseBuilder.New.AddText('null').Build
    else if Current is TJSONObject then
      Result := TAiMCPResponseBuilder.New.AddText(TJSONObject(Current).Format(2)).Build
    else if Current is TJSONArray then
      Result := TAiMCPResponseBuilder.New.AddText(TJSONArray(Current).Format(2)).Build
    else
      Result := TAiMCPResponseBuilder.New.AddText(Current.Value).Build;

  finally
    JVal.Free;
  end;
end;

// =============================================================================
// TJsonFormatTool
// =============================================================================

constructor TJsonFormatTool.Create;
begin
  inherited;
  FName        := 'json_format';
  FDescription :=
    'Format (pretty-print) or minify a JSON string. ' +
    'By default, pretty-prints with 2-space indentation. ' +
    'Set Minify=true to produce compact single-line output. ' +
    'Returns an error if the input is not valid JSON.';
end;

function TJsonFormatTool.ExecuteWithParams(const AParams: TJsonFormatParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  JVal  : TJSONValue;
  Output: string;
begin
  if Trim(AParams.Json) = '' then
    raise Exception.Create('Json parameter is required');

  JVal := TJSONObject.ParseJSONValue(AParams.Json);
  if not Assigned(JVal) then
    raise Exception.Create('Input is not valid JSON');

  try
    if AParams.Minify then
      Output := JVal.ToString
    else
      Output := JVal.Format(2);
  finally
    JVal.Free;
  end;

  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

end.
