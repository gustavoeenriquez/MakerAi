unit uFileReaderTool;

interface

uses
  System.SysUtils, System.JSON, System.IOUtils, System.Classes,
  uMakerAi.MCPServer.Core;

type
  // --- Tool 1: file_read ---
  TFileReadParams = class
  private
    FPath: string;
    FStartLine: Integer;
    FEndLine: Integer;
    FEncoding: string;
  public
    [AiMCPSchemaDescription('Absolute or relative path to the file to read')]
    property Path: string read FPath write FPath;
    [AiMCPSchemaDescription('First line to read (1-based). Default: 1 (start of file)')]
    [AiMCPOptional]
    property StartLine: Integer read FStartLine write FStartLine;
    [AiMCPSchemaDescription('Last line to read (1-based, inclusive). Default: 0 = read all lines from StartLine')]
    [AiMCPOptional]
    property EndLine: Integer read FEndLine write FEndLine;
    [AiMCPSchemaDescription('File encoding hint: "utf8" (default), "utf16", "ansi"')]
    [AiMCPOptional]
    property Encoding: string read FEncoding write FEncoding;
  end;

  TFileReadTool = class(TAiMCPToolBase<TFileReadParams>)
  protected
    function ExecuteWithParams(const AParams: TFileReadParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 2: file_list ---
  TFileListParams = class
  private
    FPath: string;
    FPattern: string;
    FIncludeHidden: Boolean;
    FRecursive: Boolean;
  public
    [AiMCPSchemaDescription('Directory path to list. Use "." for current directory.')]
    property Path: string read FPath write FPath;
    [AiMCPSchemaDescription('File name filter pattern (wildcard). Examples: "*.pas", "*.txt". Default: "*" (all files)')]
    [AiMCPOptional]
    property Pattern: string read FPattern write FPattern;
    [AiMCPSchemaDescription('Include hidden files (starting with dot). Default: false')]
    [AiMCPOptional]
    property IncludeHidden: Boolean read FIncludeHidden write FIncludeHidden;
    [AiMCPSchemaDescription('Recursively list subdirectories. Default: false')]
    [AiMCPOptional]
    property Recursive: Boolean read FRecursive write FRecursive;
  end;

  TFileListTool = class(TAiMCPToolBase<TFileListParams>)
  protected
    function ExecuteWithParams(const AParams: TFileListParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 3: file_info ---
  TFileInfoParams = class
  private
    FPath: string;
  public
    [AiMCPSchemaDescription('Path to the file or directory to inspect')]
    property Path: string read FPath write FPath;
  end;

  TFileInfoTool = class(TAiMCPToolBase<TFileInfoParams>)
  protected
    function ExecuteWithParams(const AParams: TFileInfoParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

// -------------------------------------------------------------------------
// TFileReadTool
// -------------------------------------------------------------------------

constructor TFileReadTool.Create;
begin
  inherited;
  FName        := 'file_read';
  FDescription := 'Read the contents of a text file. Optionally specify a line range ' +
                  '(StartLine and EndLine, 1-based) to read only part of the file. ' +
                  'Returns the file content as text.';
end;

function TFileReadTool.ExecuteWithParams(const AParams: TFileReadParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Lines:     TStringList;
  StartLine, EndLine: Integer;
  I:         Integer;
  SB:        TStringBuilder;
  Enc:       TEncoding;
  EncodingStr: string;
begin
  if not TFile.Exists(AParams.Path) then
    raise Exception.CreateFmt('File not found: "%s"', [AParams.Path]);

  // Choose encoding
  EncodingStr := LowerCase(Trim(AParams.Encoding));
  if      EncodingStr = 'utf16' then Enc := TEncoding.Unicode
  else if EncodingStr = 'ansi'  then Enc := TEncoding.ANSI
  else                               Enc := TEncoding.UTF8;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AParams.Path, Enc);

    StartLine := AParams.StartLine;
    EndLine   := AParams.EndLine;

    if StartLine <= 0 then StartLine := 1;
    if EndLine   <= 0 then EndLine   := Lines.Count;

    // Clamp
    if StartLine > Lines.Count then StartLine := Lines.Count;
    if EndLine   > Lines.Count then EndLine   := Lines.Count;
    if StartLine > EndLine     then StartLine := EndLine;

    SB := TStringBuilder.Create;
    try
      for I := StartLine - 1 to EndLine - 1 do
      begin
        SB.AppendLine(Lines[I]);
      end;

      // Add metadata header
      var Header := Format('[File: %s | Lines %d-%d of %d | Size: %d bytes]'#10,
        [AParams.Path, StartLine, EndLine, Lines.Count,
         TFile.GetSize(AParams.Path)]);

      Result := TAiMCPResponseBuilder.New
        .AddText(Header + SB.ToString)
        .Build;
    finally
      SB.Free;
    end;
  finally
    Lines.Free;
  end;
end;

// -------------------------------------------------------------------------
// TFileListTool
// -------------------------------------------------------------------------

constructor TFileListTool.Create;
begin
  inherited;
  FName        := 'file_list';
  FDescription := 'List files and directories in a path. Supports wildcard patterns ' +
                  'and optional recursive listing. Returns name, size, and modification date.';
end;

function TFileListTool.ExecuteWithParams(const AParams: TFileListParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Dir:      string;
  Pattern:  string;
  Files:    TArray<string>;
  Dirs:     TArray<string>;
  F:        string;
  SB:       TStringBuilder;
  Option:   TSearchOption;
begin
  Dir     := AParams.Path;
  Pattern := Trim(AParams.Pattern);
  if Pattern = '' then Pattern := '*';
  if Dir = '' then Dir := '.';

  if not TDirectory.Exists(Dir) then
    raise Exception.CreateFmt('Directory not found: "%s"', [Dir]);

  if AParams.Recursive then
    Option := TSearchOption.soAllDirectories
  else
    Option := TSearchOption.soTopDirectoryOnly;

  SB := TStringBuilder.Create;
  try
    // List subdirectories first
    Dirs := TDirectory.GetDirectories(Dir, '*', Option);
    for F in Dirs do
    begin
      var Name := TPath.GetFileName(F);
      if (not AParams.IncludeHidden) and Name.StartsWith('.') then Continue;
      SB.AppendFormat('[DIR]  %s', [F]).AppendLine;
    end;

    // List files
    Files := TDirectory.GetFiles(Dir, Pattern, Option);
    for F in Files do
    begin
      var Name := TPath.GetFileName(F);
      if (not AParams.IncludeHidden) and Name.StartsWith('.') then Continue;
      var Size := TFile.GetSize(F);
      var ModTime := TFile.GetLastWriteTime(F);
      SB.AppendFormat('[FILE] %-50s  %8d bytes  %s',
        [F, Size, FormatDateTime('yyyy-mm-dd hh:nn:ss', ModTime)]).AppendLine;
    end;

    if SB.Length = 0 then
      SB.Append('(no files found matching the criteria)');

    Result := TAiMCPResponseBuilder.New.AddText(SB.ToString).Build;
  finally
    SB.Free;
  end;
end;

// -------------------------------------------------------------------------
// TFileInfoTool
// -------------------------------------------------------------------------

constructor TFileInfoTool.Create;
begin
  inherited;
  FName        := 'file_info';
  FDescription := 'Get metadata about a file or directory: existence, size, ' +
                  'creation time, modification time, attributes.';
end;

function TFileInfoTool.ExecuteWithParams(const AParams: TFileInfoParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  P:  string;
  SB: TStringBuilder;
begin
  P  := Trim(AParams.Path);
  SB := TStringBuilder.Create;
  try
    SB.AppendFormat('Path: %s', [P]).AppendLine;

    if TFile.Exists(P) then
    begin
      SB.Append('Type: File').AppendLine;
      SB.AppendFormat('Size: %d bytes', [TFile.GetSize(P)]).AppendLine;
      SB.AppendFormat('Created:  %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', TFile.GetCreationTime(P))]).AppendLine;
      SB.AppendFormat('Modified: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', TFile.GetLastWriteTime(P))]).AppendLine;
      SB.AppendFormat('Accessed: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', TFile.GetLastAccessTime(P))]).AppendLine;
      var Attr := TFile.GetAttributes(P);
      SB.AppendFormat('Hidden: %s', [BoolToStr(TFileAttribute.faHidden in Attr, True)]).AppendLine;
      SB.AppendFormat('ReadOnly: %s', [BoolToStr(TFileAttribute.faReadOnly in Attr, True)]).AppendLine;
    end
    else if TDirectory.Exists(P) then
    begin
      SB.Append('Type: Directory').AppendLine;
      var FC := Length(TDirectory.GetFiles(P));
      var DC := Length(TDirectory.GetDirectories(P));
      SB.AppendFormat('Files: %d | Subdirectories: %d', [FC, DC]).AppendLine;
      SB.AppendFormat('Created:  %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', TDirectory.GetCreationTime(P))]).AppendLine;
      SB.AppendFormat('Modified: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', TDirectory.GetLastWriteTime(P))]).AppendLine;
    end
    else
      SB.Append('Status: NOT FOUND').AppendLine;

    Result := TAiMCPResponseBuilder.New.AddText(SB.ToString).Build;
  finally
    SB.Free;
  end;
end;

end.
