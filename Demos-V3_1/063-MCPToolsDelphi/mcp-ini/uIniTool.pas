unit uIniTool;

// =============================================================================
// mcp-ini  --  INI file read/write/delete/list/read-section MCP server
//
// Tools exposed:
//   ini_read         -- Read a value from an INI file
//   ini_write        -- Write a value to an INI file
//   ini_delete       -- Delete a key or entire section from an INI file
//   ini_list         -- List all sections, or all keys in a section
//   ini_read_section -- Read all key=value pairs in a section
// =============================================================================

interface

uses
  System.SysUtils, System.Classes, System.JSON, IniFiles, System.IOUtils,
  uMakerAi.MCPServer.Core;

// =============================================================================
// ini_read
// =============================================================================

type
  TIniReadParams = class
  private
    FFilePath: string;
    FSection : string;
    FKey     : string;
    FDefault : string;
  public
    [AiMCPSchemaDescription('Path to the INI file')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPSchemaDescription('Section name (without brackets)')]
    property Section: string read FSection write FSection;

    [AiMCPSchemaDescription('Key name')]
    property Key: string read FKey write FKey;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Default value if key not found. Default: ""')]
    property Default: string read FDefault write FDefault;
  end;

  TIniReadTool = class(TAiMCPToolBase<TIniReadParams>)
  protected
    function ExecuteWithParams(const AParams: TIniReadParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// ini_write
// =============================================================================

type
  TIniWriteParams = class
  private
    FFilePath: string;
    FSection : string;
    FKey     : string;
    FValue   : string;
  public
    [AiMCPSchemaDescription('Path to the INI file (created if not exists)')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPSchemaDescription('Section name')]
    property Section: string read FSection write FSection;

    [AiMCPSchemaDescription('Key name')]
    property Key: string read FKey write FKey;

    [AiMCPSchemaDescription('Value to write')]
    property Value: string read FValue write FValue;
  end;

  TIniWriteTool = class(TAiMCPToolBase<TIniWriteParams>)
  protected
    function ExecuteWithParams(const AParams: TIniWriteParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// ini_delete
// =============================================================================

type
  TIniDeleteParams = class
  private
    FFilePath: string;
    FSection : string;
    FKey     : string;
  public
    [AiMCPSchemaDescription('Path to the INI file')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPSchemaDescription('Section name')]
    property Section: string read FSection write FSection;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Key to delete. If empty, deletes the entire section.')]
    property Key: string read FKey write FKey;
  end;

  TIniDeleteTool = class(TAiMCPToolBase<TIniDeleteParams>)
  protected
    function ExecuteWithParams(const AParams: TIniDeleteParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// ini_list
// =============================================================================

type
  TIniListParams = class
  private
    FFilePath: string;
    FSection : string;
  public
    [AiMCPSchemaDescription('Path to the INI file')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Section to list keys. If empty, lists all sections.')]
    property Section: string read FSection write FSection;
  end;

  TIniListTool = class(TAiMCPToolBase<TIniListParams>)
  protected
    function ExecuteWithParams(const AParams: TIniListParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// =============================================================================
// ini_read_section
// =============================================================================

type
  TIniReadSectionParams = class
  private
    FFilePath: string;
    FSection : string;
    FFormat  : string;
  public
    [AiMCPSchemaDescription('Path to the INI file')]
    property FilePath: string read FFilePath write FFilePath;

    [AiMCPSchemaDescription('Section name to read')]
    property Section: string read FSection write FSection;

    [AiMCPOptional]
    [AiMCPSchemaDescription('"table" (default) or "json"')]
    property Format: string read FFormat write FFormat;
  end;

  TIniReadSectionTool = class(TAiMCPToolBase<TIniReadSectionParams>)
  protected
    function ExecuteWithParams(const AParams: TIniReadSectionParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

// =============================================================================
// TIniReadTool
// =============================================================================

constructor TIniReadTool.Create;
begin
  inherited;
  FName        := 'ini_read';
  FDescription :=
    'Read a value from an INI file. ' +
    'Returns the value for the specified section and key, or the default if not found.';
end;

function TIniReadTool.ExecuteWithParams(const AParams: TIniReadParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Ini  : TIniFile;
  Res  : string;
begin
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath parameter is required');
  if Trim(AParams.Section) = '' then
    raise Exception.Create('Section parameter is required');
  if Trim(AParams.Key) = '' then
    raise Exception.Create('Key parameter is required');

  if not TFile.Exists(AParams.FilePath) then
    raise Exception.CreateFmt('File not found: %s', [AParams.FilePath]);

  Ini := TIniFile.Create(AParams.FilePath);
  try
    Res := Ini.ReadString(AParams.Section, AParams.Key, AParams.Default);
  finally
    Ini.Free;
  end;

  Result := TAiMCPResponseBuilder.New
    .AddText(Format('[%s] %s = %s', [AParams.Section, AParams.Key, Res]))
    .Build;
end;

// =============================================================================
// TIniWriteTool
// =============================================================================

constructor TIniWriteTool.Create;
begin
  inherited;
  FName        := 'ini_write';
  FDescription :=
    'Write a value to an INI file. ' +
    'Creates the file if it does not exist. ' +
    'Creates the section and key if they do not exist.';
end;

function TIniWriteTool.ExecuteWithParams(const AParams: TIniWriteParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Ini: TIniFile;
  Dir: string;
begin
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath parameter is required');
  if Trim(AParams.Section) = '' then
    raise Exception.Create('Section parameter is required');
  if Trim(AParams.Key) = '' then
    raise Exception.Create('Key parameter is required');

  Dir := TPath.GetDirectoryName(TPath.GetFullPath(AParams.FilePath));
  if (Dir <> '') and not TDirectory.Exists(Dir) then
    TDirectory.CreateDirectory(Dir);

  Ini := TIniFile.Create(AParams.FilePath);
  try
    Ini.WriteString(AParams.Section, AParams.Key, AParams.Value);
  finally
    Ini.Free;
  end;

  Result := TAiMCPResponseBuilder.New
    .AddText(Format('OK -- [%s] %s = %s', [AParams.Section, AParams.Key, AParams.Value]))
    .Build;
end;

// =============================================================================
// TIniDeleteTool
// =============================================================================

constructor TIniDeleteTool.Create;
begin
  inherited;
  FName        := 'ini_delete';
  FDescription :=
    'Delete a key or an entire section from an INI file. ' +
    'If Key is empty, the entire section is deleted. ' +
    'If Key is specified, only that key is deleted.';
end;

function TIniDeleteTool.ExecuteWithParams(const AParams: TIniDeleteParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Ini: TIniFile;
  Msg: string;
begin
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath parameter is required');
  if Trim(AParams.Section) = '' then
    raise Exception.Create('Section parameter is required');

  if not TFile.Exists(AParams.FilePath) then
    raise Exception.CreateFmt('File not found: %s', [AParams.FilePath]);

  Ini := TIniFile.Create(AParams.FilePath);
  try
    if Trim(AParams.Key) = '' then
    begin
      Ini.EraseSection(AParams.Section);
      Msg := Format('Deleted section [%s]', [AParams.Section]);
    end
    else
    begin
      Ini.DeleteKey(AParams.Section, AParams.Key);
      Msg := Format('Deleted key "%s" from section [%s]', [AParams.Key, AParams.Section]);
    end;
  finally
    Ini.Free;
  end;

  Result := TAiMCPResponseBuilder.New.AddText(Msg).Build;
end;

// =============================================================================
// TIniListTool
// =============================================================================

constructor TIniListTool.Create;
begin
  inherited;
  FName        := 'ini_list';
  FDescription :=
    'List all sections in an INI file, or all keys in a specific section. ' +
    'If Section is empty, lists all section names. ' +
    'If Section is provided, lists all key names within that section.';
end;

function TIniListTool.ExecuteWithParams(const AParams: TIniListParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Ini    : TIniFile;
  Items  : TStringList;
  Output : string;
  i      : Integer;
begin
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath parameter is required');

  if not TFile.Exists(AParams.FilePath) then
    raise Exception.CreateFmt('File not found: %s', [AParams.FilePath]);

  Items := TStringList.Create;
  Ini   := TIniFile.Create(AParams.FilePath);
  try
    if Trim(AParams.Section) = '' then
    begin
      Ini.ReadSections(Items);
      Output := Format('Sections in %s (%d total):', [AParams.FilePath, Items.Count]) + sLineBreak;
      for i := 0 to Items.Count - 1 do
        Output := Output + Format('  [%s]', [Items[i]]) + sLineBreak;
    end
    else
    begin
      Ini.ReadSection(AParams.Section, Items);
      Output := Format('Keys in [%s] (%d total):', [AParams.Section, Items.Count]) + sLineBreak;
      for i := 0 to Items.Count - 1 do
        Output := Output + Format('  %s', [Items[i]]) + sLineBreak;
    end;
  finally
    Ini.Free;
    Items.Free;
  end;

  Result := TAiMCPResponseBuilder.New.AddText(Trim(Output)).Build;
end;

// =============================================================================
// TIniReadSectionTool
// =============================================================================

constructor TIniReadSectionTool.Create;
begin
  inherited;
  FName        := 'ini_read_section';
  FDescription :=
    'Read all key=value pairs in a section of an INI file. ' +
    'Returns the data formatted as a table (default) or as a JSON object. ' +
    'Set Format to "json" to receive a JSON object with all key/value pairs.';
end;

function TIniReadSectionTool.ExecuteWithParams(const AParams: TIniReadSectionParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Ini    : TIniFile;
  Items  : TStringList;
  Output : string;
  i      : Integer;
  JObj   : TJSONObject;
  EqPos  : Integer;
  K, V   : string;
  FmtLow : string;
begin
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath parameter is required');
  if Trim(AParams.Section) = '' then
    raise Exception.Create('Section parameter is required');

  if not TFile.Exists(AParams.FilePath) then
    raise Exception.CreateFmt('File not found: %s', [AParams.FilePath]);

  FmtLow := LowerCase(Trim(AParams.Format));
  if FmtLow = '' then
    FmtLow := 'table';

  Items := TStringList.Create;
  Ini   := TIniFile.Create(AParams.FilePath);
  try
    Ini.ReadSectionValues(AParams.Section, Items);

    if FmtLow = 'json' then
    begin
      JObj := TJSONObject.Create;
      try
        for i := 0 to Items.Count - 1 do
        begin
          EqPos := Pos('=', Items[i]);
          if EqPos > 0 then
          begin
            K := Copy(Items[i], 1, EqPos - 1);
            V := Copy(Items[i], EqPos + 1, MaxInt);
          end
          else
          begin
            K := Items[i];
            V := '';
          end;
          JObj.AddPair(K, V);
        end;
        Output := JObj.Format(2);
      finally
        JObj.Free;
      end;
    end
    else
    begin
      // table format
      Output := Format('[%s] -- %d key(s):', [AParams.Section, Items.Count]) + sLineBreak;
      for i := 0 to Items.Count - 1 do
        Output := Output + '  ' + Items[i] + sLineBreak;
      Output := Trim(Output);
    end;
  finally
    Ini.Free;
    Items.Free;
  end;

  Result := TAiMCPResponseBuilder.New.AddText(Output).Build;
end;

end.
