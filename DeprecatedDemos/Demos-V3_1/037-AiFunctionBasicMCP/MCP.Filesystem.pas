unit MCP.Filesystem;

  interface

  uses
    System.SysUtils, System.Classes, System.IOUtils, System.JSON,
    System.Generics.Collections, System.RegularExpressions;

  type
    TFileInfo = record
      Name: string;
      FullPath: string;
      Size: Int64;
      CreatedAt: TDateTime;
      ModifiedAt: TDateTime;
      IsDirectory: Boolean;
      IsReadOnly: Boolean;
    end;

    TDirectoryEntry = record
      Name: string;
      EntryType: string; // 'file' | 'directory'
    end;

    TMCPFilesystem = class
    private
      FAllowedDirectories: TList<string>;
      function NormalizePath(const APath: string): string;
      function IsPathAllowed(const APath: string): Boolean;
      procedure CheckPathAllowed(const APath: string);
      function BuildTreeNode(const APath: string; ADepth: Integer): TJSONObject;
      function ApplyEdit(const AContent, AOldText, ANewText: string): string;
    public
      constructor Create(const AAllowedDirectories: array of string);
      destructor Destroy; override;

      // Lectura
      function ReadFile(const APath: string): string;
      function ReadMultipleFiles(const APaths: array of string): TDictionary<string, string>;
      function GetFileInfo(const APath: string): TFileInfo;

      // Escritura
      procedure WriteFile(const APath, AContent: string);
      function EditFile(const APath, AOldText, ANewText: string): string;
      procedure MoveFile(const ASourcePath, ADestPath: string);

      // Directorios
      procedure CreateDirectory(const APath: string);
      function ListDirectory(const APath: string): TArray<TDirectoryEntry>;
      function DirectoryTree(const APath: string): TJSONObject;

      // B�squeda
      function SearchFiles(const ARootPath, APattern: string;
        ARecursive: Boolean = True): TArray<string>;

      // Scope
      function ListAllowedDirectories: TArray<string>;
    end;

  implementation

  { TMCPFilesystem }

  constructor TMCPFilesystem.Create(const AAllowedDirectories: array of string);
  var
    Dir: string;
  begin
    inherited Create;
    FAllowedDirectories := TList<string>.Create;
    for Dir in AAllowedDirectories do
      FAllowedDirectories.Add(NormalizePath(Dir));
  end;

  destructor TMCPFilesystem.Destroy;
  begin
    FAllowedDirectories.Free;
    inherited;
  end;

  function TMCPFilesystem.NormalizePath(const APath: string): string;
  begin
    Result := TPath.GetFullPath(APath);
    Result := ExcludeTrailingPathDelimiter(Result);
  end;

  function TMCPFilesystem.IsPathAllowed(const APath: string): Boolean;
  var
    Normalized: string;
    AllowedDir: string;
  begin
    Result := False;
    Normalized := NormalizePath(APath);
    for AllowedDir in FAllowedDirectories do
    begin
      if Normalized.StartsWith(AllowedDir, True) then
        Exit(True);
    end;
  end;

  procedure TMCPFilesystem.CheckPathAllowed(const APath: string);
  begin
    if not IsPathAllowed(APath) then
      raise EAccessViolation.CreateFmt(
        'Access denied: path "%s" is outside allowed directories', [APath]);
  end;

  // ---------------------------------------------------------------------------
  // Lectura
  // ---------------------------------------------------------------------------

  function TMCPFilesystem.ReadFile(const APath: string): string;
  begin
    CheckPathAllowed(APath);
    if not TFile.Exists(APath) then
      raise EFileNotFoundException.CreateFmt('File not found: %s', [APath]);
    Result := TFile.ReadAllText(APath, TEncoding.UTF8);
  end;

  function TMCPFilesystem.ReadMultipleFiles(
    const APaths: array of string): TDictionary<string, string>;
  var
    Path: string;
  begin
    Result := TDictionary<string, string>.Create;
    for Path in APaths do
    begin
      try
        Result.Add(Path, ReadFile(Path));
      except
        on E: Exception do
          Result.Add(Path, 'ERROR: ' + E.Message);
      end;
    end;
  end;

  function TMCPFilesystem.GetFileInfo(const APath: string): TFileInfo;
  var
    Attrs: TSearchRec;
  begin
    CheckPathAllowed(APath);
    if not (TFile.Exists(APath) or TDirectory.Exists(APath)) then
      raise EFileNotFoundException.CreateFmt('Path not found: %s', [APath]);

    Result.FullPath  := NormalizePath(APath);
    Result.Name      := TPath.GetFileName(APath);
    Result.IsDirectory := TDirectory.Exists(APath);
    Result.ModifiedAt  := TFile.GetLastWriteTime(APath);
    Result.CreatedAt   := TFile.GetCreationTime(APath);
    Result.IsReadOnly  := TFile.GetAttributes(APath) * [TFileAttribute.faReadOnly] <> [];

    if not Result.IsDirectory then
    begin
      if FindFirst(APath, faAnyFile, Attrs) = 0 then
      begin
        Result.Size := Attrs.Size;
        FindClose(Attrs);
      end;
    end
    else
      Result.Size := 0;
  end;

  // ---------------------------------------------------------------------------
  // Escritura
  // ---------------------------------------------------------------------------

  procedure TMCPFilesystem.WriteFile(const APath, AContent: string);
  var
    Dir: string;
  begin
    CheckPathAllowed(APath);
    Dir := TPath.GetDirectoryName(APath);
    if (Dir <> '') and not TDirectory.Exists(Dir) then
      TDirectory.CreateDirectory(Dir);
    TFile.WriteAllText(APath, AContent, TEncoding.UTF8);
  end;

  function TMCPFilesystem.ApplyEdit(const AContent, AOldText, ANewText: string): string;
  begin
    if not AContent.Contains(AOldText) then
      raise EArgumentException.Create(
        'edit_file: old_text block not found in file content');
    Result := AContent.Replace(AOldText, ANewText, []);
  end;

  function TMCPFilesystem.EditFile(const APath, AOldText, ANewText: string): string;
  var
    Content, Updated: string;
  begin
    CheckPathAllowed(APath);
    Content := ReadFile(APath);
    Updated := ApplyEdit(Content, AOldText, ANewText);
    TFile.WriteAllText(APath, Updated, TEncoding.UTF8);
    Result  := Updated;
  end;

  procedure TMCPFilesystem.MoveFile(const ASourcePath, ADestPath: string);
  var
    DestDir: string;
  begin
    CheckPathAllowed(ASourcePath);
    CheckPathAllowed(ADestPath);

    if not (TFile.Exists(ASourcePath) or TDirectory.Exists(ASourcePath)) then
      raise EFileNotFoundException.CreateFmt('Source not found: %s', [ASourcePath]);

    DestDir := TPath.GetDirectoryName(ADestPath);
    if (DestDir <> '') and not TDirectory.Exists(DestDir) then
      TDirectory.CreateDirectory(DestDir);

    if TFile.Exists(ASourcePath) then
      TFile.Move(ASourcePath, ADestPath)
    else
      TDirectory.Move(ASourcePath, ADestPath);
  end;

  // ---------------------------------------------------------------------------
  // Directorios
  // ---------------------------------------------------------------------------

  procedure TMCPFilesystem.CreateDirectory(const APath: string);
  begin
    CheckPathAllowed(APath);
    TDirectory.CreateDirectory(APath);
  end;

  function TMCPFilesystem.ListDirectory(const APath: string): TArray<TDirectoryEntry>;
  var
    Entries: TList<TDirectoryEntry>;
    Entry: TDirectoryEntry;
    SubDirs, Files: TArray<string>;
    Item: string;
  begin
    //CheckPathAllowed(APath);
    //if not TDirectory.Exists(APath) then
    //  raise EDirectoryNotFoundException.CreateFmt('Directory not found: %s', [APath]);

    Entries := TList<TDirectoryEntry>.Create;
    try
      SubDirs := TDirectory.GetDirectories(APath);
      for Item in SubDirs do
      begin
        Entry.Name      := TPath.GetFileName(Item);
        Entry.EntryType := 'directory';
        Entries.Add(Entry);
      end;

      Files := TDirectory.GetFiles(APath);
      for Item in Files do
      begin
        Entry.Name      := TPath.GetFileName(Item);
        Entry.EntryType := 'file';
        Entries.Add(Entry);
      end;

      Result := Entries.ToArray;
    finally
      Entries.Free;
    end;
  end;

  function TMCPFilesystem.BuildTreeNode(const APath: string;
    ADepth: Integer): TJSONObject;
  var
    Node: TJSONObject;
    Children: TJSONArray;
    SubDirs, Files: TArray<string>;
    Item: string;
    Child: TJSONObject;
  const
    MAX_DEPTH = 10;
  begin
    Node := TJSONObject.Create;
    Node.AddPair('name', TPath.GetFileName(APath));

    if TDirectory.Exists(APath) then
    begin
      Node.AddPair('type', 'directory');
      Children := TJSONArray.Create;
      if ADepth < MAX_DEPTH then
      begin
        SubDirs := TDirectory.GetDirectories(APath);
        for Item in SubDirs do
        begin
          Child := BuildTreeNode(Item, ADepth + 1);
          Children.AddElement(Child);
        end;
        Files := TDirectory.GetFiles(APath);
        for Item in Files do
        begin
          Child := TJSONObject.Create;
          Child.AddPair('name', TPath.GetFileName(Item));
          Child.AddPair('type', 'file');
          Children.AddElement(Child);
        end;
      end;
      Node.AddPair('children', Children);
    end
    else
    begin
      Node.AddPair('type', 'file');
    end;

    Result := Node;
  end;

  function TMCPFilesystem.DirectoryTree(const APath: string): TJSONObject;
  begin
    CheckPathAllowed(APath);
    if not TDirectory.Exists(APath) then
      raise EDirectoryNotFoundException.CreateFmt('Directory not found: %s', [APath]);
    Result := BuildTreeNode(APath, 0);
  end;

  // ---------------------------------------------------------------------------
  // B�squeda
  // ---------------------------------------------------------------------------

  function TMCPFilesystem.SearchFiles(const ARootPath, APattern: string;
    ARecursive: Boolean): TArray<string>;
  var
    Results: TList<string>;
    SearchOption: TSearchOption;
    Files: TArray<string>;
    Item: string;
  begin
    CheckPathAllowed(ARootPath);
    if not TDirectory.Exists(ARootPath) then
      raise EDirectoryNotFoundException.CreateFmt('Directory not found: %s', [ARootPath]);

    Results := TList<string>.Create;
    try
      if ARecursive then
        SearchOption := TSearchOption.soAllDirectories
      else
        SearchOption := TSearchOption.soTopDirectoryOnly;

      Files := TDirectory.GetFiles(ARootPath, APattern, SearchOption);
      for Item in Files do
        Results.Add(Item);

      Result := Results.ToArray;
    finally
      Results.Free;
    end;
  end;

  // ---------------------------------------------------------------------------
  // Scope
  // ---------------------------------------------------------------------------

  function TMCPFilesystem.ListAllowedDirectories: TArray<string>;
  begin
    Result := FAllowedDirectories.ToArray;
  end;

  end.
