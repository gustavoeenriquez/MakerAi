unit uTool.Sandbox;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON, System.Types,
  uMakerAi.MCPServer.Core; // Tu núcleo MCP

const
  // Carpeta raíz que controlará la IA.
  SANDBOX_ROOT = 'd:\taller\mcpdir';

type
  // ===========================================================================
  // Argumentos (DTOs) para las herramientas
  // ===========================================================================

  // Argumentos para Listar y Leer (Solo piden un path)
  TPathArgs = class
  private
    FPath: string;
  public
    [AiMCPSchemaDescription('Ruta relativa del archivo o carpeta (ej: "notas/lista.txt" o "." para la raíz)')]
    property Path: string read FPath write FPath;
  end;

  // Argumentos para Escribir (Path + Contenido)
  TWriteArgs = class
  private
    FPath: string;
    FContent: string;
  public
    [AiMCPSchemaDescription('Ruta relativa donde guardar el archivo')]
    property Path: string read FPath write FPath;
    [AiMCPSchemaDescription('Contenido de texto a guardar en el archivo')]
    property Content: string read FContent write FContent;
  end;

  // Argumentos para Copiar (Origen + Destino)
  TCopyArgs = class
  private
    FSource: string;
    FDestination: string;
  public
    [AiMCPSchemaDescription('Ruta relativa del archivo origen')]
    property Source: string read FSource write FSource;
    [AiMCPSchemaDescription('Ruta relativa del nuevo destino')]
    property Destination: string read FDestination write FDestination;
  end;

  // ===========================================================================
  // Clases de Herramientas
  // ===========================================================================

  TListFilesTool = class(TAiMCPToolBase<TPathArgs>)
  protected
    function ExecuteWithParams(const AParams: TPathArgs; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  TReadTool = class(TAiMCPToolBase<TPathArgs>)
  protected
    function ExecuteWithParams(const AParams: TPathArgs; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  TWriteTool = class(TAiMCPToolBase<TWriteArgs>)
  protected
    function ExecuteWithParams(const AParams: TWriteArgs; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  TDeleteTool = class(TAiMCPToolBase<TPathArgs>)
  protected
    function ExecuteWithParams(const AParams: TPathArgs; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  TCopyTool = class(TAiMCPToolBase<TCopyArgs>)
  protected
    function ExecuteWithParams(const AParams: TCopyArgs; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

// Procedimiento para registrar todo de una vez
procedure RegisterSandboxTools(ALogicServer: TAiMCPServer);

implementation

// =============================================================================
// Helpers de Seguridad (Sandboxing)
// =============================================================================

procedure EnsureSandboxExists;
begin
  if not TDirectory.Exists(SANDBOX_ROOT) then
    TDirectory.CreateDirectory(SANDBOX_ROOT);
end;

// Convierte una ruta relativa en absoluta y verifica que esté dentro del ROOT
function ResolvePath(const ARelativePath: string): string;
var
  LFullPath: string;
  LCleanRel: string;
begin
  // Normalizar entrada
  if (ARelativePath = '') or (ARelativePath = '.') then
    LCleanRel := ''
  else
    LCleanRel := ARelativePath;

  // Construir ruta absoluta
  LFullPath := TPath.GetFullPath(TPath.Combine(SANDBOX_ROOT, LCleanRel));

  // Verificar seguridad (Evitar Path Traversal: "..\..\windows")
  if not LFullPath.StartsWith(SANDBOX_ROOT, True) then
    raise Exception.CreateFmt('SEGURIDAD: Acceso denegado. La ruta "%s" está fuera del sandbox.', [ARelativePath]);

  Result := LFullPath;
end;

// =============================================================================
// Registro de Herramientas
// =============================================================================

procedure RegisterSandboxTools(ALogicServer: TAiMCPServer);
begin
  EnsureSandboxExists;

  ALogicServer.RegisterTool('fs_list', function: IAiMCPTool begin Result := TListFilesTool.Create; end);
  ALogicServer.RegisterTool('fs_read', function: IAiMCPTool begin Result := TReadTool.Create; end);
  ALogicServer.RegisterTool('fs_write', function: IAiMCPTool begin Result := TWriteTool.Create; end);
  ALogicServer.RegisterTool('fs_delete', function: IAiMCPTool begin Result := TDeleteTool.Create; end);
  ALogicServer.RegisterTool('fs_copy', function: IAiMCPTool begin Result := TCopyTool.Create; end);
end;

// =============================================================================
// Implementación: Listar Archivos
// =============================================================================

constructor TListFilesTool.Create;
begin
  inherited;
  FName := 'fs_list';
  FDescription := 'Lista los archivos y carpetas dentro del directorio de trabajo.';
end;

function TListFilesTool.ExecuteWithParams(const AParams: TPathArgs; const AuthContext: TAiAuthContext): TJSONObject;
var
  LPath: string;
  LFiles, LDirs: TStringDynArray;
  LItem: string;
  LList: TStringBuilder;
begin
  try
    LPath := ResolvePath(AParams.Path);

    if not TDirectory.Exists(LPath) then
      Exit(TAiMCPResponseBuilder.New.AddText('Error: El directorio no existe.').Build);

    LList := TStringBuilder.Create;
    try
      LList.AppendLine('Contenido de: ' + AParams.Path);
      LList.AppendLine('-----------------------------------');

      LDirs := TDirectory.GetDirectories(LPath);
      for LItem in LDirs do
        LList.AppendLine('[DIR]  ' + TPath.GetFileName(LItem));

      LFiles := TDirectory.GetFiles(LPath);
      for LItem in LFiles do
        LList.AppendLine('[FILE] ' + TPath.GetFileName(LItem));

      Result := TAiMCPResponseBuilder.New.AddText(LList.ToString).Build;
    finally
      LList.Free;
    end;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('Error listando: ' + E.Message).Build;
  end;
end;

// =============================================================================
// Implementación: Leer Archivo
// =============================================================================

constructor TReadTool.Create;
begin
  inherited;
  FName := 'fs_read';
  FDescription := 'Lee el contenido de un archivo de texto.';
end;

function TReadTool.ExecuteWithParams(const AParams: TPathArgs; const AuthContext: TAiAuthContext): TJSONObject;
var
  LPath: string;
begin
  try
    LPath := ResolvePath(AParams.Path);
    if not TFile.Exists(LPath) then
      raise Exception.Create('Archivo no encontrado.');

    // Usamos el Helper de Core que ya maneja streams o texto
    Result := TAiMCPResponseBuilder.New.AddFile(LPath).Build;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('Error leyendo archivo: ' + E.Message).Build;
  end;
end;

// =============================================================================
// Implementación: Escribir Archivo
// =============================================================================

constructor TWriteTool.Create;
begin
  inherited;
  FName := 'fs_write';
  FDescription := 'Crea un nuevo archivo o sobrescribe uno existente con el contenido proporcionado.';
end;

function TWriteTool.ExecuteWithParams(const AParams: TWriteArgs; const AuthContext: TAiAuthContext): TJSONObject;
var
  LPath: string;
begin
  try
    LPath := ResolvePath(AParams.Path);
    // Asegurar que la carpeta padre existe
    TDirectory.CreateDirectory(TPath.GetDirectoryName(LPath));

    TFile.WriteAllText(LPath, AParams.Content, TEncoding.UTF8);

    Result := TAiMCPResponseBuilder.New.AddText('✅ Archivo guardado correctamente: ' + AParams.Path).Build;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('Error escribiendo archivo: ' + E.Message).Build;
  end;
end;

// =============================================================================
// Implementación: Borrar Archivo
// =============================================================================

constructor TDeleteTool.Create;
begin
  inherited;
  FName := 'fs_delete';
  FDescription := 'Elimina un archivo permanentemente.';
end;

function TDeleteTool.ExecuteWithParams(const AParams: TPathArgs; const AuthContext: TAiAuthContext): TJSONObject;
var
  LPath: string;
begin
  try
    LPath := ResolvePath(AParams.Path);
    if TFile.Exists(LPath) then
    begin
      TFile.Delete(LPath);
      Result := TAiMCPResponseBuilder.New.AddText('🗑️ Archivo eliminado: ' + AParams.Path).Build;
    end
    else if TDirectory.Exists(LPath) then
    begin
       // Opcional: Permitir borrar carpetas
       TDirectory.Delete(LPath, True);
       Result := TAiMCPResponseBuilder.New.AddText('🗑️ Carpeta eliminada: ' + AParams.Path).Build;
    end
    else
      Result := TAiMCPResponseBuilder.New.AddText('El archivo no existe.').Build;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('Error eliminando: ' + E.Message).Build;
  end;
end;

// =============================================================================
// Implementación: Copiar Archivo
// =============================================================================

constructor TCopyTool.Create;
begin
  inherited;
  FName := 'fs_copy';
  FDescription := 'Copia un archivo de una ubicación a otra.';
end;

function TCopyTool.ExecuteWithParams(const AParams: TCopyArgs; const AuthContext: TAiAuthContext): TJSONObject;
var
  LSrc, LDst: string;
begin
  try
    LSrc := ResolvePath(AParams.Source);
    LDst := ResolvePath(AParams.Destination);

    if not TFile.Exists(LSrc) then
      raise Exception.Create('Archivo origen no encontrado.');

    // Asegurar directorio destino
    TDirectory.CreateDirectory(TPath.GetDirectoryName(LDst));

    TFile.Copy(LSrc, LDst, True); // True = Sobrescribir
    Result := TAiMCPResponseBuilder.New.AddText(Format('✅ Copiado de "%s" a "%s"', [AParams.Source, AParams.Destination])).Build;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('Error copiando: ' + E.Message).Build;
  end;
end;

end.
