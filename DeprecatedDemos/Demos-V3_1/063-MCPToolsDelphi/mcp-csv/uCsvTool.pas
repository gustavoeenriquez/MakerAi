unit uCsvTool;

// =============================================================================
// mcp-csv  —  CSV file and string manipulation MCP server
//
// Tools exposed:
//   csv_parse   — Parse CSV and return as ASCII table or JSON
//   csv_to_json — Convert CSV to JSON array of objects
//   csv_filter  — Filter CSV rows where a column matches a value
//   csv_info    — Get metadata about a CSV file or string
// =============================================================================

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils,
  System.StrUtils, System.Math,
  uMakerAi.MCPServer.Core;

// =============================================================================
// csv_parse
// =============================================================================

type
  TCsvParseParams = class
  private
    FCsv      : string;
    FFilePath : string;
    FDelimiter: string;
    FFormat   : string;
  public
    [AiMCPOptional]
    [AiMCPSchemaDescription('CSV text to parse')]
    property Csv: string read FCsv write FCsv;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Path to CSV file (alternative to Csv param)')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Column delimiter. Default: ","')]
    property Delimiter: string read FDelimiter write FDelimiter;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Output format: "table" (default) or "json"')]
    property Format: string read FFormat write FFormat;
  end;

  TCsvParseTool = class(TAiMCPToolBase<TCsvParseParams>)
  protected
    function ExecuteWithParams(const AParams: TCsvParseParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// csv_to_json
// =============================================================================

type
  TCsvToJsonParams = class
  private
    FCsv      : string;
    FFilePath : string;
    FDelimiter: string;
  public
    [AiMCPOptional]
    [AiMCPSchemaDescription('CSV text to convert')]
    property Csv: string read FCsv write FCsv;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Path to CSV file (alternative to Csv param)')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Column delimiter. Default: ","')]
    property Delimiter: string read FDelimiter write FDelimiter;
  end;

  TCsvToJsonTool = class(TAiMCPToolBase<TCsvToJsonParams>)
  protected
    function ExecuteWithParams(const AParams: TCsvToJsonParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// csv_filter
// =============================================================================

type
  TCsvFilterParams = class
  private
    FCsv      : string;
    FFilePath : string;
    FDelimiter: string;
    FColumn   : string;
    FValue    : string;
    FOperator : string;
  public
    [AiMCPOptional]
    [AiMCPSchemaDescription('CSV text to filter')]
    property Csv: string read FCsv write FCsv;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Path to CSV file (alternative to Csv param)')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Column delimiter. Default: ","')]
    property Delimiter: string read FDelimiter write FDelimiter;

    [AiMCPSchemaDescription('Column name to filter on')]
    property Column: string read FColumn write FColumn;

    [AiMCPSchemaDescription('Value to match')]
    property Value: string read FValue write FValue;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Comparison operator: "eq" (default), "contains", "starts", "gt", "lt"')]
    property Operator: string read FOperator write FOperator;
  end;

  TCsvFilterTool = class(TAiMCPToolBase<TCsvFilterParams>)
  protected
    function ExecuteWithParams(const AParams: TCsvFilterParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// csv_info
// =============================================================================

type
  TCsvInfoParams = class
  private
    FCsv      : string;
    FFilePath : string;
    FDelimiter: string;
  public
    [AiMCPOptional]
    [AiMCPSchemaDescription('CSV text to inspect')]
    property Csv: string read FCsv write FCsv;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Path to CSV file (alternative to Csv param)')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Column delimiter. Default: ","')]
    property Delimiter: string read FDelimiter write FDelimiter;
  end;

  TCsvInfoTool = class(TAiMCPToolBase<TCsvInfoParams>)
  protected
    function ExecuteWithParams(const AParams: TCsvInfoParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

// =============================================================================
// CSV parser helpers
// =============================================================================

// Parse a single CSV line respecting quoted fields.
function ParseCsvLine(const Line: string; Delimiter: Char): TArray<string>;
var
  Fields  : TStringList;
  I       : Integer;
  InQuote : Boolean;
  Field   : string;
  Ch      : Char;
begin
  Fields  := TStringList.Create;
  try
    Field   := '';
    InQuote := False;
    I       := 1;
    while I <= Length(Line) do
    begin
      Ch := Line[I];
      if InQuote then
      begin
        if Ch = '"' then
        begin
          // Check for escaped quote ("")
          if (I < Length(Line)) and (Line[I + 1] = '"') then
          begin
            Field := Field + '"';
            Inc(I); // skip second quote
          end
          else
            InQuote := False; // closing quote
        end
        else
          Field := Field + Ch;
      end
      else
      begin
        if Ch = '"' then
          InQuote := True
        else if Ch = Delimiter then
        begin
          Fields.Add(Field);
          Field := '';
        end
        else
          Field := Field + Ch;
      end;
      Inc(I);
    end;
    Fields.Add(Field); // last field

    SetLength(Result, Fields.Count);
    for I := 0 to Fields.Count - 1 do
      Result[I] := Fields[I];
  finally
    Fields.Free;
  end;
end;

// Parse all CSV text into rows. First row becomes headers.
procedure ParseCsv(const Text: string; Delimiter: Char;
  out Headers: TArray<string>; out Rows: TArray<TArray<string>>);
var
  Lines    : TStringList;
  I        : Integer;
  RowCount : Integer;
  Line     : string;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Text;

    // Remove trailing empty lines
    while (Lines.Count > 0) and (Trim(Lines[Lines.Count - 1]) = '') do
      Lines.Delete(Lines.Count - 1);

    if Lines.Count = 0 then
    begin
      SetLength(Headers, 0);
      SetLength(Rows, 0);
      Exit;
    end;

    // First row = headers
    Headers := ParseCsvLine(Lines[0], Delimiter);

    RowCount := Lines.Count - 1;
    SetLength(Rows, RowCount);
    for I := 0 to RowCount - 1 do
    begin
      Line := Lines[I + 1];
      if Trim(Line) = '' then
      begin
        // Empty row — fill with empty strings
        SetLength(Rows[I], Length(Headers));
      end
      else
        Rows[I] := ParseCsvLine(Line, Delimiter);
    end;
  finally
    Lines.Free;
  end;
end;

// Load CSV text from either the text param or from a file.
// Returns True on success; raises on error.
function LoadCsvContent(const TextParam, FileParam: string; out Content: string): Boolean;
begin
  if Trim(TextParam) <> '' then
  begin
    Content := TextParam;
    Result  := True;
  end
  else if Trim(FileParam) <> '' then
  begin
    if not TFile.Exists(FileParam) then
      raise Exception.CreateFmt('File not found: %s', [FileParam]);
    Content := TFile.ReadAllText(FileParam, TEncoding.UTF8);
    Result  := True;
  end
  else
  begin
    Content := '';
    Result  := False;
  end;
end;

// Resolve delimiter from param (default ',')
function ResolveDelimiter(const DelimParam: string): Char;
begin
  if Length(DelimParam) >= 1 then
    Result := DelimParam[1]
  else
    Result := ',';
end;

// Build an ASCII table from headers + rows
function ArraysToTable(const Headers: TArray<string>;
  const Rows: TArray<TArray<string>>): string;
var
  ColCount  : Integer;
  ColWidths : array of Integer;
  I, R      : Integer;
  Sep       : string;
  HeaderLine: string;
  RowLine   : string;
  CellVal   : string;
  SB        : TStringBuilder;
begin
  ColCount := Length(Headers);
  if ColCount = 0 then
  begin
    Result := '(no columns)';
    Exit;
  end;

  SetLength(ColWidths, ColCount);

  // Initialize widths from header names
  for I := 0 to ColCount - 1 do
    ColWidths[I] := Length(Headers[I]);

  // Expand for cell values (capped at 50)
  for R := 0 to Length(Rows) - 1 do
    for I := 0 to Min(ColCount, Length(Rows[R])) - 1 do
      ColWidths[I] := Min(50, Max(ColWidths[I], Length(Rows[R][I])));

  // Build separator
  Sep := '+';
  for I := 0 to ColCount - 1 do
    Sep := Sep + StringOfChar('-', ColWidths[I] + 2) + '+';

  SB := TStringBuilder.Create;
  try
    // Header
    SB.AppendLine(Sep);
    HeaderLine := '|';
    for I := 0 to ColCount - 1 do
      HeaderLine := HeaderLine + ' ' + Headers[I].PadRight(ColWidths[I]) + ' |';
    SB.AppendLine(HeaderLine);
    SB.AppendLine(Sep);

    // Rows
    for R := 0 to Length(Rows) - 1 do
    begin
      RowLine := '|';
      for I := 0 to ColCount - 1 do
      begin
        if I < Length(Rows[R]) then
          CellVal := Rows[R][I]
        else
          CellVal := '';
        if Length(CellVal) > ColWidths[I] then
          CellVal := Copy(CellVal, 1, ColWidths[I] - 3) + '...';
        RowLine := RowLine + ' ' + CellVal.PadRight(ColWidths[I]) + ' |';
      end;
      SB.AppendLine(RowLine);
    end;
    SB.AppendLine(Sep);
    SB.AppendFormat('(%d row%s)', [Length(Rows), IfThen(Length(Rows) = 1, '', 's')]);

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

// Build a JSON array of objects from headers + rows
function ArraysToJson(const Headers: TArray<string>;
  const Rows: TArray<TArray<string>>): string;
var
  JArr : TJSONArray;
  JObj : TJSONObject;
  R, I : Integer;
begin
  JArr := TJSONArray.Create;
  try
    for R := 0 to Length(Rows) - 1 do
    begin
      JObj := TJSONObject.Create;
      for I := 0 to Length(Headers) - 1 do
      begin
        if I < Length(Rows[R]) then
          JObj.AddPair(Headers[I], Rows[R][I])
        else
          JObj.AddPair(Headers[I], '');
      end;
      JArr.Add(JObj);
    end;
    Result := JArr.Format(2);
  finally
    JArr.Free;
  end;
end;

// =============================================================================
// TCsvParseTool
// =============================================================================

constructor TCsvParseTool.Create;
begin
  inherited;
  FName        := 'csv_parse';
  FDescription :=
    'Parse CSV text or a CSV file and return the data as an ASCII table or JSON. ' +
    'Provide the CSV data via the Csv param (string) or FilePath param (file). ' +
    'Use Format="json" to get a JSON array of objects instead of the default ASCII table. ' +
    'Supports custom delimiters (e.g. ";" for European CSVs).';
end;

function TCsvParseTool.ExecuteWithParams(const AParams: TCsvParseParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Content  : string;
  Headers  : TArray<string>;
  Rows     : TArray<TArray<string>>;
  Delim    : Char;
  OutFmt   : string;
  Output   : string;
begin
  if not LoadCsvContent(AParams.Csv, AParams.FilePath, Content) then
    raise Exception.Create('Either Csv or FilePath parameter is required');

  Delim  := ResolveDelimiter(AParams.Delimiter);
  OutFmt := LowerCase(Trim(AParams.Format));
  if OutFmt = '' then OutFmt := 'table';

  ParseCsv(Content, Delim, Headers, Rows);

  if Length(Headers) = 0 then
  begin
    Result := TAiMCPResponseBuilder.New.AddText('(empty CSV — no data found)').Build;
    Exit;
  end;

  if OutFmt = 'json' then
    Output := ArraysToJson(Headers, Rows)
  else
    Output := ArraysToTable(Headers, Rows);

  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

// =============================================================================
// TCsvToJsonTool
// =============================================================================

constructor TCsvToJsonTool.Create;
begin
  inherited;
  FName        := 'csv_to_json';
  FDescription :=
    'Convert CSV data to a JSON array of objects, using the first row as property names. ' +
    'Returns a formatted JSON array where each element represents one CSV row. ' +
    'Provide data via Csv (string) or FilePath (file path) parameter.';
end;

function TCsvToJsonTool.ExecuteWithParams(const AParams: TCsvToJsonParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Content: string;
  Headers: TArray<string>;
  Rows   : TArray<TArray<string>>;
  Delim  : Char;
  Output : string;
begin
  if not LoadCsvContent(AParams.Csv, AParams.FilePath, Content) then
    raise Exception.Create('Either Csv or FilePath parameter is required');

  Delim := ResolveDelimiter(AParams.Delimiter);
  ParseCsv(Content, Delim, Headers, Rows);

  if Length(Headers) = 0 then
  begin
    Result := TAiMCPResponseBuilder.New.AddText('[]').Build;
    Exit;
  end;

  Output := ArraysToJson(Headers, Rows);
  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

// =============================================================================
// TCsvFilterTool
// =============================================================================

constructor TCsvFilterTool.Create;
begin
  inherited;
  FName        := 'csv_filter';
  FDescription :=
    'Filter CSV rows where a specified column matches a given value. ' +
    'Returns the header row plus all matching rows as an ASCII table, ' +
    'along with the total matching row count. ' +
    'Operators: "eq" (exact match, default), "contains", "starts", "gt", "lt".';
end;

function TCsvFilterTool.ExecuteWithParams(const AParams: TCsvFilterParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Content     : string;
  Headers     : TArray<string>;
  Rows        : TArray<TArray<string>>;
  Delim       : Char;
  ColIdx      : Integer;
  Op          : string;
  I           : Integer;
  CellVal     : string;
  Matches     : Boolean;
  FilteredRows: TArray<TArray<string>>;
  MatchCount  : Integer;
  Output      : string;
  SB          : TStringBuilder;
begin
  if not LoadCsvContent(AParams.Csv, AParams.FilePath, Content) then
    raise Exception.Create('Either Csv or FilePath parameter is required');

  if Trim(AParams.Column) = '' then
    raise Exception.Create('Column parameter is required');

  if Trim(AParams.Value) = '' then
    raise Exception.Create('Value parameter is required');

  Delim := ResolveDelimiter(AParams.Delimiter);
  ParseCsv(Content, Delim, Headers, Rows);

  if Length(Headers) = 0 then
  begin
    Result := TAiMCPResponseBuilder.New.AddText('(empty CSV — no data found)').Build;
    Exit;
  end;

  // Find column index
  ColIdx := -1;
  for I := 0 to Length(Headers) - 1 do
    if SameText(Headers[I], AParams.Column) then
    begin
      ColIdx := I;
      Break;
    end;

  if ColIdx = -1 then
    raise Exception.CreateFmt('Column "%s" not found. Available columns: %s',
      [AParams.Column, String.Join(', ', Headers)]);

  Op := LowerCase(Trim(AParams.Operator));
  if Op = '' then Op := 'eq';

  // Filter rows
  MatchCount := 0;
  SetLength(FilteredRows, Length(Rows));

  for I := 0 to Length(Rows) - 1 do
  begin
    if ColIdx < Length(Rows[I]) then
      CellVal := Rows[I][ColIdx]
    else
      CellVal := '';

    case AnsiIndexStr(Op, ['eq', 'contains', 'starts', 'gt', 'lt']) of
      0: Matches := SameText(CellVal, AParams.Value);
      1: Matches := ContainsText(CellVal, AParams.Value);
      2: Matches := StartsText(AParams.Value, CellVal);
      3: Matches := StrToFloatDef(CellVal, 0) > StrToFloatDef(AParams.Value, 0);
      4: Matches := StrToFloatDef(CellVal, 0) < StrToFloatDef(AParams.Value, 0);
    else
      Matches := SameText(CellVal, AParams.Value); // fallback to eq
    end;

    if Matches then
    begin
      FilteredRows[MatchCount] := Rows[I];
      Inc(MatchCount);
    end;
  end;

  SetLength(FilteredRows, MatchCount);

  SB := TStringBuilder.Create;
  try
    SB.AppendFormat('Filter: %s %s "%s" — %d match%s found',
      [AParams.Column, Op, AParams.Value, MatchCount,
       IfThen(MatchCount = 1, '', 'es')]).AppendLine;
    SB.AppendLine;

    if MatchCount > 0 then
      Output := ArraysToTable(Headers, FilteredRows)
    else
      Output := '(no rows matched the filter criteria)';

    SB.Append(Output);
    Result := TAiMCPResponseBuilder.New.AddText(SB.ToString).Build;
  finally
    SB.Free;
  end;
end;

// =============================================================================
// TCsvInfoTool
// =============================================================================

constructor TCsvInfoTool.Create;
begin
  inherited;
  FName        := 'csv_info';
  FDescription :=
    'Get metadata about a CSV file or string: column names, column count, ' +
    'row count, and a sample of the first 3 data rows. ' +
    'Useful for understanding the structure of a CSV before processing it.';
end;

function TCsvInfoTool.ExecuteWithParams(const AParams: TCsvInfoParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Content   : string;
  Headers   : TArray<string>;
  Rows      : TArray<TArray<string>>;
  Delim     : Char;
  SB        : TStringBuilder;
  I         : Integer;
  SampleRows: TArray<TArray<string>>;
  SampleCount: Integer;
begin
  if not LoadCsvContent(AParams.Csv, AParams.FilePath, Content) then
    raise Exception.Create('Either Csv or FilePath parameter is required');

  Delim := ResolveDelimiter(AParams.Delimiter);
  ParseCsv(Content, Delim, Headers, Rows);

  SB := TStringBuilder.Create;
  try
    SB.AppendFormat('Columns   : %d', [Length(Headers)]).AppendLine;
    SB.AppendFormat('Rows      : %d', [Length(Rows)]).AppendLine;
    SB.AppendLine;

    if Length(Headers) > 0 then
    begin
      SB.AppendLine('Column names:');
      for I := 0 to Length(Headers) - 1 do
        SB.AppendFormat('  [%d] %s', [I, Headers[I]]).AppendLine;
      SB.AppendLine;

      // Sample: first 3 rows
      SampleCount := Min(3, Length(Rows));
      if SampleCount > 0 then
      begin
        SetLength(SampleRows, SampleCount);
        for I := 0 to SampleCount - 1 do
          SampleRows[I] := Rows[I];

        SB.AppendFormat('Sample (first %d row%s):', [SampleCount,
          IfThen(SampleCount = 1, '', 's')]).AppendLine;
        SB.Append(ArraysToTable(Headers, SampleRows));
      end
      else
        SB.AppendLine('(no data rows)');
    end
    else
      SB.AppendLine('(empty CSV — no headers found)');

    Result := TAiMCPResponseBuilder.New.AddText(SB.ToString).Build;
  finally
    SB.Free;
  end;
end;

end.
