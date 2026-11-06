unit uZipTool;

// =============================================================================
// mcp-zip  --  ZIP file list/extract/create/add MCP server
//
// Tools exposed:
//   zip_list    -- List contents of a ZIP file
//   zip_extract -- Extract contents of a ZIP file
//   zip_create  -- Create a new ZIP file from a list of files
//   zip_add     -- Add a file to an existing ZIP (or create if not exists)
// =============================================================================

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Zip, System.IOUtils,
  System.StrUtils, uMakerAi.MCPServer.Core;

// =============================================================================
// zip_list
// =============================================================================

type
  TZipListParams = class
  private
    FZipPath: string;
  public
    [AiMCPSchemaDescription('Path to the ZIP file')]
    property ZipPath: string read FZipPath write FZipPath;
  end;

  TZipListTool = class(TAiMCPToolBase<TZipListParams>)
  protected
    function ExecuteWithParams(const AParams: TZipListParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// zip_extract
// =============================================================================

type
  TZipExtractParams = class
  private
    FZipPath  : string;
    FOutputDir: string;
    FEntry    : string;
  public
    [AiMCPSchemaDescription('Path to the ZIP file')]
    property ZipPath: string read FZipPath write FZipPath;

    [AiMCPSchemaDescription('Directory to extract into (created if not exists)')]
    property OutputDir: string read FOutputDir write FOutputDir;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Specific entry to extract. If empty, extracts all.')]
    property Entry: string read FEntry write FEntry;
  end;

  TZipExtractTool = class(TAiMCPToolBase<TZipExtractParams>)
  protected
    function ExecuteWithParams(const AParams: TZipExtractParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// zip_create
// =============================================================================

type
  TZipCreateParams = class
  private
    FZipPath  : string;
    FFiles    : string;
    FOverwrite: Boolean;
  public
    [AiMCPSchemaDescription('Path of the ZIP file to create')]
    property ZipPath: string read FZipPath write FZipPath;

    [AiMCPSchemaDescription('Comma-separated list of file paths to add')]
    property Files: string read FFiles write FFiles;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Overwrite existing ZIP. Default: false')]
    property Overwrite: Boolean read FOverwrite write FOverwrite;
  end;

  TZipCreateTool = class(TAiMCPToolBase<TZipCreateParams>)
  protected
    function ExecuteWithParams(const AParams: TZipCreateParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// zip_add
// =============================================================================

type
  TZipAddParams = class
  private
    FZipPath  : string;
    FFilePath : string;
    FEntryName: string;
  public
    [AiMCPSchemaDescription('Path to the ZIP file')]
    property ZipPath: string read FZipPath write FZipPath;

    [AiMCPSchemaDescription('File to add to the ZIP')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Name/path inside the ZIP. Default: filename only.')]
    property EntryName: string read FEntryName write FEntryName;
  end;

  TZipAddTool = class(TAiMCPToolBase<TZipAddParams>)
  protected
    function ExecuteWithParams(const AParams: TZipAddParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

// =============================================================================
// TZipListTool
// =============================================================================

constructor TZipListTool.Create;
begin
  inherited;
  FName        := 'zip_list';
  FDescription :=
    'List the contents of a ZIP file. ' +
    'Returns a numbered list of all entries (files and directories) inside the archive.';
end;

function TZipListTool.ExecuteWithParams(const AParams: TZipListParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Zip   : TZipFile;
  Output: string;
  i     : Integer;
begin
  if Trim(AParams.ZipPath) = '' then
    raise Exception.Create('ZipPath parameter is required');

  if not TFile.Exists(AParams.ZipPath) then
    raise Exception.CreateFmt('File not found: %s', [AParams.ZipPath]);

  Zip := TZipFile.Create;
  try
    Zip.Open(AParams.ZipPath, zmRead);
    try
      Output := Format('Contents of %s (%d entries):', [AParams.ZipPath, Zip.FileCount]) + sLineBreak;
      for i := 0 to Zip.FileCount - 1 do
        Output := Output + Format('  %d. %s', [i + 1, Zip.FileNames[i]]) + sLineBreak;
    finally
      Zip.Close;
    end;
  finally
    Zip.Free;
  end;

  Result := TAiMCPResponseBuilder.New.AddText(Trim(Output)).Build;
end;

// =============================================================================
// TZipExtractTool
// =============================================================================

constructor TZipExtractTool.Create;
begin
  inherited;
  FName        := 'zip_extract';
  FDescription :=
    'Extract the contents of a ZIP file to a directory. ' +
    'If Entry is specified, only that entry is extracted. ' +
    'If Entry is empty, all entries are extracted. ' +
    'The output directory is created if it does not exist.';
end;

function TZipExtractTool.ExecuteWithParams(const AParams: TZipExtractParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Zip    : TZipFile;
  Output : string;
  i      : Integer;
  Entry  : string;
begin
  if Trim(AParams.ZipPath) = '' then
    raise Exception.Create('ZipPath parameter is required');
  if Trim(AParams.OutputDir) = '' then
    raise Exception.Create('OutputDir parameter is required');

  if not TFile.Exists(AParams.ZipPath) then
    raise Exception.CreateFmt('File not found: %s', [AParams.ZipPath]);

  ForceDirectories(AParams.OutputDir);

  Entry := Trim(AParams.Entry);

  if Entry = '' then
  begin
    // Extract all entries
    TZipFile.ExtractZipFile(AParams.ZipPath, AParams.OutputDir);

    // Report what was extracted
    Zip := TZipFile.Create;
    try
      Zip.Open(AParams.ZipPath, zmRead);
      try
        Output := Format('Extracted %d file(s) to: %s', [Zip.FileCount, AParams.OutputDir]);
      finally
        Zip.Close;
      end;
    finally
      Zip.Free;
    end;
  end
  else
  begin
    // Extract a specific entry
    Zip := TZipFile.Create;
    try
      Zip.Open(AParams.ZipPath, zmRead);
      try
        // Find the entry (case-insensitive search)
        i := -1;
        for i := 0 to Zip.FileCount - 1 do
          if SameText(Zip.FileNames[i], Entry) then
            Break;

        if (i < 0) or (i >= Zip.FileCount) or not SameText(Zip.FileNames[i], Entry) then
          raise Exception.CreateFmt('Entry not found in ZIP: %s', [Entry]);

        Zip.Extract(i, AParams.OutputDir);
        Output := Format('Extracted "%s" to: %s', [Entry, AParams.OutputDir]);
      finally
        Zip.Close;
      end;
    finally
      Zip.Free;
    end;
  end;

  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

// =============================================================================
// TZipCreateTool
// =============================================================================

constructor TZipCreateTool.Create;
begin
  inherited;
  FName        := 'zip_create';
  FDescription :=
    'Create a new ZIP file from a comma-separated list of file paths. ' +
    'Set Overwrite=true to replace an existing ZIP file. ' +
    'Returns the number of files added and the ZIP path.';
end;

function TZipCreateTool.ExecuteWithParams(const AParams: TZipCreateParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Zip       : TZipFile;
  FileParts : TArray<string>;
  FilePart  : string;
  FileTrim  : string;
  Added     : Integer;
  Dir       : string;
begin
  if Trim(AParams.ZipPath) = '' then
    raise Exception.Create('ZipPath parameter is required');
  if Trim(AParams.Files) = '' then
    raise Exception.Create('Files parameter is required');

  if TFile.Exists(AParams.ZipPath) then
  begin
    if not AParams.Overwrite then
      raise Exception.CreateFmt(
        'ZIP file already exists: %s. Set Overwrite=true to replace it.', [AParams.ZipPath]);
    TFile.Delete(AParams.ZipPath);
  end;

  Dir := TPath.GetDirectoryName(TPath.GetFullPath(AParams.ZipPath));
  if (Dir <> '') and not TDirectory.Exists(Dir) then
    TDirectory.CreateDirectory(Dir);

  FileParts := AParams.Files.Split([',']);
  Added := 0;

  Zip := TZipFile.Create;
  try
    Zip.Open(AParams.ZipPath, zmWrite);
    try
      for FilePart in FileParts do
      begin
        FileTrim := Trim(FilePart);
        if FileTrim = '' then
          Continue;
        if not TFile.Exists(FileTrim) then
          raise Exception.CreateFmt('File not found: %s', [FileTrim]);
        Zip.Add(FileTrim);
        Inc(Added);
      end;
    finally
      Zip.Close;
    end;
  finally
    Zip.Free;
  end;

  Result := TAiMCPResponseBuilder.New
    .AddText(Format('Created ZIP: %s with %d file(s).', [AParams.ZipPath, Added]))
    .Build;
end;

// =============================================================================
// TZipAddTool
// =============================================================================

constructor TZipAddTool.Create;
begin
  inherited;
  FName        := 'zip_add';
  FDescription :=
    'Add a file to an existing ZIP archive, or create the archive if it does not exist. ' +
    'EntryName controls the name of the file inside the ZIP; defaults to the filename only. ' +
    'Returns a confirmation message with the entry name used.';
end;

function TZipAddTool.ExecuteWithParams(const AParams: TZipAddParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Zip      : TZipFile;
  Mode     : TZipMode;
  UsedEntry: string;
  Dir      : string;
begin
  if Trim(AParams.ZipPath) = '' then
    raise Exception.Create('ZipPath parameter is required');
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath parameter is required');

  if not TFile.Exists(AParams.FilePath) then
    raise Exception.CreateFmt('File not found: %s', [AParams.FilePath]);

  UsedEntry := Trim(AParams.EntryName);
  if UsedEntry = '' then
    UsedEntry := TPath.GetFileName(AParams.FilePath);

  if TFile.Exists(AParams.ZipPath) then
    Mode := zmReadWrite
  else
  begin
    Mode := zmWrite;
    Dir  := TPath.GetDirectoryName(TPath.GetFullPath(AParams.ZipPath));
    if (Dir <> '') and not TDirectory.Exists(Dir) then
      TDirectory.CreateDirectory(Dir);
  end;

  Zip := TZipFile.Create;
  try
    Zip.Open(AParams.ZipPath, Mode);
    try
      Zip.Add(AParams.FilePath, UsedEntry);
    finally
      Zip.Close;
    end;
  finally
    Zip.Free;
  end;

  Result := TAiMCPResponseBuilder.New
    .AddText(Format('Added "%s" to ZIP as "%s".', [AParams.FilePath, UsedEntry]))
    .Build;
end;

end.
