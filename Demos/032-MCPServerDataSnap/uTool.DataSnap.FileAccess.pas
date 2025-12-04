// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


unit uTool.DataSnap.FileAccess;

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
  // CLASE BASE CON LÓGICA DE SEGURIDAD (Permisos por Instancia)
  // =============================================================================
  TFileAccessConfig = class
  private
    FAllowedPaths: TList<string>;
    FAllowedExtensions: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAllowedPath(const APath: string);
    procedure AddAllowedExtension(const AExtension: string);
    procedure ClearPermissions;
    procedure SetDefaultConfiguration;

    function IsPathAllowed(const APath: string): Boolean;
    function IsExtensionAllowed(const AFilePath: string): Boolean;
    function NormalizePath(const APath: string): string;
  end;

  // Clase base genérica para herramientas de archivo que comparten configuración
  TAiFileToolBase<T: class, constructor> = class(TAiMCPToolBase<T>)
  protected
    FConfig: TFileAccessConfig;
    FOwnsConfig: Boolean; // Si es true, libera la config al destruirse
  public
    constructor Create(AConfig: TFileAccessConfig = nil); reintroduce;
    destructor Destroy; override;

    property Config: TFileAccessConfig read FConfig;
  end;

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

  TListFilesTool = class(TAiFileToolBase<TListFilesParams>)
  protected
    function ExecuteWithParams(const AParams: TListFilesParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AConfig: TFileAccessConfig = nil);
  end;

  // =============================================================================
  // HERRAMIENTA PARA LEER ARCHIVOS
  // =============================================================================
  TReadFileParams = class
  private
    FFilePath: string;
  public
    [AiMCPSchemaDescription('Ruta completa del archivo a leer')]
    property FilePath: string read FFilePath write FFilePath;
  end;

  TReadFileTool = class(TAiFileToolBase<TReadFileParams>)
  private const
    MAX_FILE_SIZE = 10 * 1024 * 1024; // 10 MB
  protected
    function ExecuteWithParams(const AParams: TReadFileParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AConfig: TFileAccessConfig = nil);
  end;

  // =============================================================================
  // HERRAMIENTA PARA ESCRIBIR ARCHIVOS
  // =============================================================================
  TWriteFileParams = class
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

  TWriteFileTool = class(TAiFileToolBase<TWriteFileParams>)
  protected
    function ExecuteWithParams(const AParams: TWriteFileParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AConfig: TFileAccessConfig = nil);
  end;

// Helper para registrar con configuración compartida o por defecto
procedure RegisterTools(ALogicServer: TAiMCPServer; AConfig: TFileAccessConfig = nil);

implementation

uses
  System.StrUtils;

// =============================================================================
// Helper de Registro
// =============================================================================
procedure RegisterTools(ALogicServer: TAiMCPServer; AConfig: TFileAccessConfig = nil);
var
  // Si no se pasa config, creamos una compartida para este grupo de herramientas
  SharedConfig: TFileAccessConfig;
begin
  if not Assigned(ALogicServer) then
    raise Exception.Create('LogicServer no puede ser nulo.');

  // Nota: Si AConfig es nil, cada herramienta creará su propia config por defecto internamente.
  // Pero para consistencia, es mejor que compartan la misma si se van a registrar juntas.

  // Opción 1: Si el usuario pasa una config, la usamos. EL USUARIO ES RESPONSABLE DE LIBERARLA (o el último tool).
  // Para simplificar en DataSnap, haremos que cada tool tenga su referencia.

  ALogicServer.RegisterTool('list_files',
    function: IAiMCPTool
    begin
      // Si AConfig es nil, el tool crea una nueva y se hace dueño.
      // Si AConfig NO es nil, el tool la usa pero NO se hace dueño (referencia compartida).
      Result := TListFilesTool.Create(AConfig);
    end);

  ALogicServer.RegisterTool('read_file',
    function: IAiMCPTool
    begin
      Result := TReadFileTool.Create(AConfig);
    end);

  ALogicServer.RegisterTool('write_file',
    function: IAiMCPTool
    begin
      Result := TWriteFileTool.Create(AConfig);
    end);
end;

// =============================================================================
// TFileAccessConfig (Lógica de Seguridad)
// =============================================================================

constructor TFileAccessConfig.Create;
begin
  inherited;
  FAllowedPaths := TList<string>.Create;
  FAllowedExtensions := TList<string>.Create;
  SetDefaultConfiguration;
end;

destructor TFileAccessConfig.Destroy;
begin
  FAllowedPaths.Free;
  FAllowedExtensions.Free;
  inherited;
end;

procedure TFileAccessConfig.ClearPermissions;
begin
  FAllowedPaths.Clear;
  FAllowedExtensions.Clear;
end;

procedure TFileAccessConfig.SetDefaultConfiguration;
begin
  ClearPermissions;
  // Permisos por defecto seguros
  AddAllowedPath(TPath.GetTempPath);
  AddAllowedPath(TPath.GetDocumentsPath);

  // Extensiones seguras por defecto
  AddAllowedExtension('.txt'); AddAllowedExtension('.json'); AddAllowedExtension('.xml');
  AddAllowedExtension('.csv'); AddAllowedExtension('.log'); AddAllowedExtension('.md');
  AddAllowedExtension('.png'); AddAllowedExtension('.jpg'); AddAllowedExtension('.jpeg');
  AddAllowedExtension('.pdf');
end;

procedure TFileAccessConfig.AddAllowedPath(const APath: string);
var
  Normalized: string;
begin
  if not TDirectory.Exists(APath) then Exit;
  Normalized := NormalizePath(APath);
  if FAllowedPaths.IndexOf(Normalized) = -1 then
    FAllowedPaths.Add(Normalized);
end;

procedure TFileAccessConfig.AddAllowedExtension(const AExtension: string);
var
  Ext: string;
begin
  Ext := Trim(LowerCase(AExtension));
  if not Ext.StartsWith('.') then Ext := '.' + Ext;
  if FAllowedExtensions.IndexOf(Ext) = -1 then
    FAllowedExtensions.Add(Ext);
end;

function TFileAccessConfig.NormalizePath(const APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetFullPath(APath));
end;

function TFileAccessConfig.IsPathAllowed(const APath: string): Boolean;
var
  NormalizedRequest, Allowed: string;
begin
  Result := False;
  NormalizedRequest := NormalizePath(APath);
  for Allowed in FAllowedPaths do
  begin
    if SameText(NormalizedRequest, Allowed) or NormalizedRequest.StartsWith(Allowed) then
      Exit(True);
  end;
end;

function TFileAccessConfig.IsExtensionAllowed(const AFilePath: string): Boolean;
var
  Ext: string;
begin
  if FAllowedExtensions.Count = 0 then Exit(True);
  Ext := LowerCase(TPath.GetExtension(AFilePath));
  Result := FAllowedExtensions.IndexOf(Ext) > -1;
end;

// =============================================================================
// TAiFileToolBase (Clase Base para Herramientas)
// =============================================================================

constructor TAiFileToolBase<T>.Create(AConfig: TFileAccessConfig);
begin
  inherited Create;
  if Assigned(AConfig) then
  begin
    FConfig := AConfig;
    FOwnsConfig := False; // Usamos configuración compartida externa
  end
  else
  begin
    FConfig := TFileAccessConfig.Create;
    FOwnsConfig := True; // Creamos nuestra propia configuración
  end;
end;

destructor TAiFileToolBase<T>.Destroy;
begin
  if FOwnsConfig and Assigned(FConfig) then
    FConfig.Free;
  inherited;
end;

// =============================================================================
// TListFilesTool
// =============================================================================

constructor TListFilesTool.Create(AConfig: TFileAccessConfig);
begin
  inherited Create(AConfig);
  FName := 'list_files';
  FDescription := 'Lista archivos en carpetas permitidas con filtros de seguridad';
end;

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

    if not FConfig.IsPathAllowed(AParams.Path) then
      raise Exception.Create('Acceso denegado a la carpeta: ' + AParams.Path);

    SearchPattern := AParams.Pattern;
    if SearchPattern.IsEmpty then SearchPattern := '*.*';

    if AParams.IncludeSubdirs then
      Files := TDirectory.GetFiles(AParams.Path, SearchPattern, TSearchOption.soAllDirectories)
    else
      Files := TDirectory.GetFiles(AParams.Path, SearchPattern, TSearchOption.soTopDirectoryOnly);

    ResultObject := TJSONObject.Create;
    FilesArray := TJSONArray.Create;
    ResultObject.AddPair('files', FilesArray);

    for FilePath in Files do
    begin
      if FConfig.IsExtensionAllowed(FilePath) then
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

    Result := TAiMCPResponseBuilder.New.AddText(ResultObject.ToJSON).Build;
    ResultObject.Free;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('Error: ' + E.Message).Build;
  end;
end;

// =============================================================================
// TReadFileTool
// =============================================================================

constructor TReadFileTool.Create(AConfig: TFileAccessConfig);
begin
  inherited Create(AConfig);
  FName := 'read_file';
  FDescription := 'Lee y devuelve el contenido de un archivo con restricciones de seguridad.';
end;

function TReadFileTool.ExecuteWithParams(const AParams: TReadFileParams; const AuthContext: TAiAuthContext): TJSONObject;
begin
  try
    if not TFile.Exists(AParams.FilePath) then
      raise Exception.Create('El archivo no existe.');

    if not FConfig.IsPathAllowed(ExtractFileDir(AParams.FilePath)) then
      raise Exception.Create('Acceso denegado a la ruta.');

    if not FConfig.IsExtensionAllowed(AParams.FilePath) then
      raise Exception.Create('Extensión no permitida.');

    if TFile.GetSize(AParams.FilePath) > MAX_FILE_SIZE then
      raise Exception.Create('Archivo demasiado grande (>10MB).');

    Result := TAiMCPResponseBuilder.New
      .AddFile(AParams.FilePath)
      .Build;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('Error: ' + E.Message).Build;
  end;
end;

// =============================================================================
// TWriteFileTool
// =============================================================================

constructor TWriteFileTool.Create(AConfig: TFileAccessConfig);
begin
  inherited Create(AConfig);
  FName := 'write_file';
  FDescription := 'Escribe contenido en un archivo con restricciones de seguridad.';
end;

function TWriteFileTool.ExecuteWithParams(const AParams: TWriteFileParams; const AuthContext: TAiAuthContext): TJSONObject;
var
  FileDir: string;
begin
  try
    FileDir := ExtractFileDir(AParams.FilePath);

    if not FConfig.IsPathAllowed(FileDir) then
      raise Exception.Create('Acceso denegado a la ruta de escritura.');

    if not FConfig.IsExtensionAllowed(AParams.FilePath) then
      raise Exception.Create('Extensión no permitida para escritura.');

    if AParams.CreateDirs and not TDirectory.Exists(FileDir) then
      TDirectory.CreateDirectory(FileDir);

    if AParams.Append then
      TFile.AppendAllText(AParams.FilePath, AParams.Content, TEncoding.UTF8)
    else
      TFile.WriteAllText(AParams.FilePath, AParams.Content, TEncoding.UTF8);

    Result := TAiMCPResponseBuilder.New
      .AddText('Archivo guardado correctamente.')
      .Build;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('Error: ' + E.Message).Build;
  end;
end;

end.
