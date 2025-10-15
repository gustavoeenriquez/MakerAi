unit uTool.FileAccess;

interface

uses
  uMakerAi.MCPServer.Core,
  System.SysUtils,
  System.Types,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.JSON;

type
  // =============================================================================
  // HERRAMIENTA PARA LISTAR ARCHIVOS
  // =============================================================================
  TListFilesParams = class
  private
    FPath: string;
    FPattern: string;
    FIncludeSubdirs: Boolean;
    FShowDetails: Boolean;
  public
    [AiMCPSchemaDescription('Ruta de la carpeta a listar')]
    property Path: string read FPath write FPath;
    [AiMCPOptional]
    [AiMCPSchemaDescription('Patrón de búsqueda (ej: "*.pas", "*.txt") - default: "*.*"')]
    property Pattern: string read FPattern write FPattern;
    [AiMCPOptional]
    [AiMCPSchemaDescription('Incluir subdirectorios (default: false)')]
    property IncludeSubdirs: Boolean read FIncludeSubdirs write FIncludeSubdirs;
    [AiMCPOptional]
    [AiMCPSchemaDescription('Mostrar detalles (tamaño, fecha) - default: false)')]
    property ShowDetails: Boolean read FShowDetails write FShowDetails;
  end;

  TListFilesTool = class(TAiMCPToolBase<TListFilesParams>)
  private
    class var FAllowedPaths: TList<string>;
    class var FAllowedExtensions: TList<string>;
    class function IsPathAllowed(const APath: string): Boolean;
    class function IsExtensionAllowed(const AFilePath: string): Boolean;
    class function NormalizePath(const APath: string): string;
  protected
    // CAMBIO: La función ahora devuelve TJSONObject
    function ExecuteWithParams(const AParams: TListFilesParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; Override;
    class constructor Create;
    class destructor Destroy;
    class procedure AddAllowedPath(const APath: string);
    class procedure AddAllowedExtension(const AExtension: string);
    class procedure ClearAllowedPathsAndExtensions;
    class procedure SetDefaultConfiguration;
  end;

  // =============================================================================
  // HERRAMIENTA PARA LEER ARCHIVOS
  // =============================================================================
  TReadFileParams = class
    // ... (sin cambios)
  private
    FFilePath: string;
  public
    [AiMCPSchemaDescription('Ruta completa del archivo a leer')]
    property FilePath: string read FFilePath write FFilePath;
  end;

  TReadFileTool = class(TAiMCPToolBase<TReadFileParams>)
  private const
    MAX_FILE_SIZE = 10 * 1024 * 1024; // 10 MB
  protected
    // CAMBIO: La función ahora devuelve TJSONObject
    function ExecuteWithParams(const AParams: TReadFileParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; Override;
  end;

  // =============================================================================
  // HERRAMIENTA PARA ESCRIBIR ARCHIVOS
  // =============================================================================
  TWriteFileParams = class
    // ... (sin cambios)
  private
    FFilePath: string;
    FContent: string;
    FAppend: Boolean;
    FCreateDirs: Boolean;
  public
    [AiMCPSchemaDescription('Ruta completa del archivo a escribir')]
    property FilePath: string read FFilePath write FFilePath;
    [AiMCPSchemaDescription('Contenido a escribir en el archivo')]
    property Content: string read FContent write FContent;
    [AiMCPOptional]
    [AiMCPSchemaDescription('Agregar al final del archivo (default: false = sobrescribir)')]
    property Append: Boolean read FAppend write FAppend;
    [AiMCPOptional]
    [AiMCPSchemaDescription('Crear directorios si no existen (default: true)')]
    property CreateDirs: Boolean read FCreateDirs write FCreateDirs;
  end;

  TWriteFileTool = class(TAiMCPToolBase<TWriteFileParams>)
  protected
    // CAMBIO: La función ahora devuelve TJSONObject
    function ExecuteWithParams(const AParams: TWriteFileParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; Override;
  end;

procedure RegisterTools(ALogicServer: TAiMCPServer);

implementation

uses
  System.StrUtils;

{ TListFilesTool }

// ... (RegisterTools y los métodos de configuración de TListFilesTool no cambian)
procedure RegisterTools(ALogicServer: TAiMCPServer);
begin
  if not Assigned(ALogicServer) then
    raise Exception.Create('LogicServer no puede ser nulo para registrar herramientas.');
  ALogicServer.RegisterTool('list_files', function: IAiMCPTool begin Result := TListFilesTool.Create; end);
  ALogicServer.RegisterTool('read_file', function: IAiMCPTool begin Result := TReadFileTool.Create; end);
  ALogicServer.RegisterTool('write_file', function: IAiMCPTool begin Result := TWriteFileTool.Create; end);
end;

class constructor TListFilesTool.Create;
begin
  FAllowedPaths := TList<string>.Create;
  FAllowedExtensions := TList<string>.Create;
  SetDefaultConfiguration;
end;

class destructor TListFilesTool.Destroy;
begin
  FAllowedPaths.Free;
  FAllowedExtensions.Free;
end;

constructor TListFilesTool.Create;
begin
  inherited;
  FName := 'list_files';
  FDescription := 'Lista archivos en carpetas permitidas con filtros de seguridad';
end;

class procedure TListFilesTool.SetDefaultConfiguration;
begin
  ClearAllowedPathsAndExtensions;
  AddAllowedPath(TPath.GetTempPath);
  AddAllowedPath(TPath.GetDocumentsPath);
  AddAllowedPath(ExtractFilePath(ParamStr(0)));
  AddAllowedExtension('.txt');
  AddAllowedExtension('.json');
  AddAllowedExtension('.xml');
  AddAllowedExtension('.csv');
  AddAllowedExtension('.log');
  AddAllowedExtension('.md');
  // Por seguridad, no añadimos extensiones ejecutables o de sistema.
  // También añadimos las comunes de imágenes, documentos y audio para que `read_file` pueda devolverlas.
  AddAllowedExtension('.png');
  AddAllowedExtension('.jpg');
  AddAllowedExtension('.jpeg');
  AddAllowedExtension('.gif');
  AddAllowedExtension('.bmp');
  AddAllowedExtension('.pdf');
  AddAllowedExtension('.docx');
  AddAllowedExtension('.xlsx');
  AddAllowedExtension('.mp3');
  AddAllowedExtension('.wav');
end;

class procedure TListFilesTool.AddAllowedPath(const APath: string);
var
  NormalizedPath: string;
begin
  if not TDirectory.Exists(APath) then
    Exit;
  NormalizedPath := NormalizePath(APath);
  if FAllowedPaths.IndexOf(NormalizedPath) = -1 then
    FAllowedPaths.Add(NormalizedPath);
end;

class procedure TListFilesTool.AddAllowedExtension(const AExtension: string);
var
  Ext: string;
begin
  Ext := Trim(LowerCase(AExtension));
  if not Ext.StartsWith('.') then
    Ext := '.' + Ext;
  if FAllowedExtensions.IndexOf(Ext) = -1 then
    FAllowedExtensions.Add(Ext);
end;

class procedure TListFilesTool.ClearAllowedPathsAndExtensions;
begin
  FAllowedPaths.Clear;
  FAllowedExtensions.Clear;
end;

class function TListFilesTool.NormalizePath(const APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetFullPath(APath));
end;

class function TListFilesTool.IsPathAllowed(const APath: string): Boolean;
var
  NormalizedRequestPath, AllowedPath: string;
begin
  Result := False;
  NormalizedRequestPath := NormalizePath(APath);
  for AllowedPath in FAllowedPaths do
  begin
    if SameText(NormalizedRequestPath, AllowedPath) or NormalizedRequestPath.StartsWith(AllowedPath) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

class function TListFilesTool.IsExtensionAllowed(const AFilePath: string): Boolean;
var
  Ext: string;
begin
  if FAllowedExtensions.Count = 0 then
    Exit(True);
  Ext := LowerCase(TPath.GetExtension(AFilePath));
  Result := FAllowedExtensions.IndexOf(Ext) > -1;
end;


// La función devuelve un TJSONObject estructurado.
function TListFilesTool.ExecuteWithParams(const AParams: TListFilesParams; const AuthContext: TAiAuthContext): TJSONObject;
var
  SearchPattern: string;
  Files: TStringDynArray;
  FilePath: string;
  ResultObject: TJSONObject;
  FilesArray: TJSONArray;
  FileObject: TJSONObject;
  FileInfo: TSearchRec;
begin
  try
    if not TDirectory.Exists(AParams.Path) then
      raise Exception.Create('La carpeta no existe: ' + AParams.Path);

    if not IsPathAllowed(AParams.Path) then
      raise Exception.Create('Acceso denegado a la carpeta: ' + AParams.Path);

    SearchPattern := AParams.Pattern;
    if SearchPattern.IsEmpty then
      SearchPattern := '*.*';

    if AParams.IncludeSubdirs then
      Files := TDirectory.GetFiles(AParams.Path, SearchPattern, TSearchOption.soAllDirectories)
    else
      Files := TDirectory.GetFiles(AParams.Path, SearchPattern, TSearchOption.soTopDirectoryOnly);

    ResultObject := TJSONObject.Create;
    FilesArray := TJSONArray.Create;
    ResultObject.AddPair('files', FilesArray);

    for FilePath in Files do
    begin
      if IsExtensionAllowed(FilePath) then
      begin
        FileObject := TJSONObject.Create;
        FileObject.AddPair('path', FilePath);
        FileObject.AddPair('name', TPath.GetFileName(FilePath));

        if AParams.ShowDetails then
        begin
          if FindFirst(FilePath, faAnyFile, FileInfo) = 0 then
          try
            FileObject.AddPair('size', FileInfo.Size);
            FileObject.AddPair('modified', TJSONString.Create(FormatDateTime('c', FileDateToDateTime(FileInfo.Time))));
          finally
            FindClose(FileInfo);
          end;
        end;
        FilesArray.AddElement(FileObject);
      end;
    end;

    ResultObject.AddPair('count', FilesArray.Count);
    ResultObject.AddPair('message', Format('%d archivos encontrados en "%s".', [FilesArray.Count, AParams.Path]));

    // Devolvemos el JSON de la lista de archivos dentro del formato de respuesta "content"
    Result := TAiMCPResponseBuilder.New.AddText(ResultObject.ToJSON).Build;
    ResultObject.Free;

  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('❌ Error listando archivos: ' + E.Message).Build;
  end;
end;

{ TReadFileTool }

constructor TReadFileTool.Create;
begin
  inherited;
  FName := 'read_file';
  FDescription := 'Lee y devuelve el contenido de un archivo (texto o binario) con restricciones de seguridad.';
end;

// CAMBIO: La función ahora devuelve el archivo usando el builder.
function TReadFileTool.ExecuteWithParams(const AParams: TReadFileParams; const AuthContext: TAiAuthContext): TJSONObject;
var
  FileSize: Int64;
begin
  try
    if not TFile.Exists(AParams.FilePath) then
      raise Exception.Create('El archivo no existe: ' + AParams.FilePath);

    if not TListFilesTool.IsPathAllowed(ExtractFileDir(AParams.FilePath)) then
      raise Exception.Create('Acceso denegado a la ruta del archivo.');

    if not TListFilesTool.IsExtensionAllowed(AParams.FilePath) then
      raise Exception.Create('Tipo de archivo no permitido para lectura: ' + TPath.GetExtension(AParams.FilePath));

    FileSize := TFile.GetSize(AParams.FilePath);
    if FileSize > MAX_FILE_SIZE then
      raise Exception.CreateFmt('Archivo demasiado grande: %d bytes (máximo permitido: %d bytes)', [FileSize, MAX_FILE_SIZE]);

    // Usamos el builder para devolver el archivo. Él se encarga de todo.
    Result := TAiMCPResponseBuilder.New
      .AddFile(AParams.FilePath)
      .Build;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('❌ Error leyendo archivo: ' + E.Message).Build;
  end;
end;

{ TWriteFileTool }

constructor TWriteFileTool.Create;
begin
  inherited;
  FName := 'write_file';
  FDescription := 'Escribe o agrega contenido a archivos de texto con restricciones de seguridad.';
end;

// CAMBIO: La función ahora devuelve un JSON simple de confirmación.
function TWriteFileTool.ExecuteWithParams(const AParams: TWriteFileParams; const AuthContext: TAiAuthContext): TJSONObject;
var
  FileDir: string;
  CreateDirs: Boolean;
begin
  try
    FileDir := ExtractFileDir(AParams.FilePath);

    if not TListFilesTool.IsPathAllowed(FileDir) then
      raise Exception.Create('Acceso denegado para escribir en: ' + FileDir);

    if not TListFilesTool.IsExtensionAllowed(AParams.FilePath) then
      raise Exception.Create('Tipo de archivo no permitido para escritura: ' + TPath.GetExtension(AParams.FilePath));

    CreateDirs := AParams.CreateDirs;
    if CreateDirs and not TDirectory.Exists(FileDir) then
      TDirectory.CreateDirectory(FileDir);

    if AParams.Append then
      TFile.AppendAllText(AParams.FilePath, AParams.Content, TEncoding.UTF8)
    else
      TFile.WriteAllText(AParams.FilePath, AParams.Content, TEncoding.UTF8);

    Result := TAiMCPResponseBuilder.New
      .AddText(Format('✅ Archivo guardado correctamente en: %s', [AParams.FilePath]))
      .Build;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('❌ Error escribiendo archivo: ' + E.Message).Build;
  end;
end;

end.
