// MIT License
// MakerAI - Sistema de Agentes v3.4
// Registro unificado de herramientas IAiTool con integración PPM.
//
// TAiToolRegistry centraliza todas las herramientas disponibles para
// el sistema de agentes, independientemente de su origen (legacy TAiToolBase,
// función LLM, servidor MCP, o herramienta PPM descargada).
//
// Autor: Gustavo Enríquez
// GitHub: https://github.com/gustavoeenriquez/MakerAi

unit uMakerAi.Agents.ToolRegistry;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  uMakerAi.Agents.IAiTool,
  uMakerAi.MCPClient.Core,
  uMakerAi.Tools.Functions;

type

  { TAiRegistryEntry ------------------------------------------------------------
    Entrada del registry: herramienta + metadatos de origen.
  }
  TAiRegistryEntry = record
    Tool     : IAiTool;
    Origin   : String;  // 'local', 'mcp', 'ppm', 'legacy'
    SourceId : String;  // nombre del servidor MCP, paquete PPM, etc.
  end;

  { EAiToolNotFound }
  EAiToolNotFound = class(Exception);

  { TAiPPMPackageInfo }
  TAiPPMPackageInfo = record
    Name        : String;
    Version     : String;
    Description : String;
    DownloadUrl : String;
    Command     : String;
    Args        : String;
  end;

  { TAiToolRegistry -------------------------------------------------------------
    Registro central de herramientas IAiTool.

    Singleton global: TAiToolRegistry.Instance
    Limpiar singleton: TAiToolRegistry.DropInstance
  }
  TAiToolRegistry = class
  private
    class var FInstance: TAiToolRegistry;
    var
    FEntries  : TList<TAiRegistryEntry>;
    FPPMBase  : String;
    FToolsDir : String;
  public
    constructor Create;
    destructor Destroy; override;

    class function  Instance: TAiToolRegistry;
    // Libera el singleton (llamar en finalization o TearDown de tests)
    class procedure DropInstance;

    // --- Registro ---
    procedure Register(const ATool: IAiTool;
                       const AOrigin: String = 'local';
                       const ASourceId: String = '');
    function RegisterFromMCP(AClient: TMCPClientCustom): Integer;
    // Registra todas las funciones locales y clientes MCP de un TAiFunctions.
    // Devuelve el número total de herramientas registradas.
    function RegisterFromTAiFunctions(AFunctions: TAiFunctions): Integer;

    // --- Búsqueda ---
    function Find(const AName: String): IAiTool;
    function TryFind(const AName: String; out ATool: IAiTool): Boolean;
    function GetAll: TArray<IAiTool>;
    function GetEntries: TArray<TAiRegistryEntry>;
    procedure Unregister(const AName: String);
    procedure Clear;
    function Count: Integer;

    // --- PPM ---
    function SearchPPM(const AQuery: String;
                       APage: Integer = 1;
                       APerPage: Integer = 20): TArray<TAiPPMPackageInfo>;
    function GetPPMPackage(const AName, AVersion: String): TAiPPMPackageInfo;
    // Descarga el ejecutable del paquete a ToolsDir (omite si ya existe).
    // Devuelve la ruta completa del ejecutable, o '' si falla.
    function DownloadPackage(const APkg: TAiPPMPackageInfo): String;
    function InstallFromPPM(const APkg: TAiPPMPackageInfo;
                            AOwner: TComponent = nil): TMCPClientCustom;
    // Para paquetes schema-only (sin binario ejecutable):
    // descarga el .paipkg, extrae los .tool JSON y registra TAiSchemaTool.
    // Devuelve el número de herramientas registradas (0 = fallo).
    function InstallSchemaFromPPM(const APkg: TAiPPMPackageInfo): Integer;

    property PPMBaseUrl: String read FPPMBase write FPPMBase;
    // Directorio donde se guardan los ejecutables descargados.
    // Si está vacío usa %APPDATA%\MakerAI\tools\
    property ToolsDir: String read FToolsDir write FToolsDir;
  end;

implementation

uses
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Net.HttpClientComponent,
  System.NetEncoding,
  System.IOUtils,
  System.Zip,
  IniFiles,
  uMakerAi.Chat.Messages,
  uMakerAi.Agents.Tools.MCP;

{ TAiFunctionItem_IAiTool -----------------------------------------------------
  Adaptador que expone un TFunctionActionItem (función local de TAiFunctions)
  como IAiTool para el TAiToolRegistry.

  Memoria: no-owning sobre FItem (el TFunctionActionItem vive en TAiFunctions).
  FSchema es propiedad de este objeto (liberado en destructor).
}
type
  TAiFunctionItem_IAiTool = class(TInterfacedObject, IAiTool)
  private
    FItem   : TFunctionActionItem;  // ref débil — no owning
    FSchema : TJSONObject;          // propiedad de este objeto
  public
    constructor Create(AItem: TFunctionActionItem);
    destructor  Destroy; override;
    function GetName:        String;
    function GetDescription: String;
    function GetCategory:    String;
    function GetSchema:      TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable:    Boolean;
  end;

constructor TAiFunctionItem_IAiTool.Create(AItem: TFunctionActionItem);
begin
  inherited Create;
  FItem   := AItem;
  FSchema := nil;
end;

destructor TAiFunctionItem_IAiTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TAiFunctionItem_IAiTool.GetName: String;
begin
  if Assigned(FItem) then Result := FItem.FunctionName else Result := '';
end;

function TAiFunctionItem_IAiTool.GetDescription: String;
begin
  if Assigned(FItem) then Result := Trim(FItem.Description.Text) else Result := '';
end;

function TAiFunctionItem_IAiTool.GetCategory: String;
begin
  Result := 'local';
end;

function TAiFunctionItem_IAiTool.GetSchema: TJSONObject;
begin
  if not Assigned(FSchema) then
  begin
    if Assigned(FItem) then
      FSchema := FItem.Parameters.ToJSon
    else
      FSchema := nil;

    if not Assigned(FSchema) then
    begin
      FSchema := TJSONObject.Create;
      FSchema.AddPair('type', 'object');
      FSchema.AddPair('properties', TJSONObject.Create);
    end;
  end;
  Result := FSchema;
end;

function TAiFunctionItem_IAiTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  ToolCall : TAiToolsFunction;
  Handled  : Boolean;
  JResp    : TJSONValue;
begin
  Result := nil;
  if not IsAvailable then Exit;

  ToolCall := TAiToolsFunction.Create;
  try
    ToolCall.Name := FItem.FunctionName;
    if Assigned(AArgs) then
      ToolCall.Arguments := AArgs.ToJSON
    else
      ToolCall.Arguments := '{}';

    Handled := False;
    FItem.OnAction(nil, FItem, FItem.FunctionName, ToolCall, Handled);

    if ToolCall.Response <> '' then
    begin
      JResp := TJSONObject.ParseJSONValue(ToolCall.Response);
      if JResp is TJSONObject then
        Result := TJSONObject(JResp)
      else
      begin
        JResp.Free;
        Result := TJSONObject.Create;
        Result.AddPair('result', ToolCall.Response);
      end;
    end;
  finally
    ToolCall.Free;
  end;
end;

function TAiFunctionItem_IAiTool.IsAvailable: Boolean;
begin
  Result := Assigned(FItem) and FItem.Enabled and Assigned(FItem.OnAction);
end;

{ TAiSchemaTool ---------------------------------------------------------------
  Herramienta schema-only (sin binario): lee un .tool JSON del PPM y expone
  su interfaz como IAiTool. Execute devuelve nil (el llamador usa fallback
  nativo). IsAvailable = True para que FindMCPToolBySource lo encuentre.
}
type
  TAiSchemaTool = class(TInterfacedObject, IAiTool)
  private
    FName        : String;
    FDescription : String;
    FCategory    : String;
    FSchema      : TJSONObject;
  public
    constructor CreateFromJSON(const AJson: TJSONObject);
    destructor  Destroy; override;
    function GetName       : String;
    function GetDescription: String;
    function GetCategory   : String;
    function GetSchema     : TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable   : Boolean;
  end;

constructor TAiSchemaTool.CreateFromJSON(const AJson: TJSONObject);
var
  JTmp: TJSONValue;
begin
  inherited Create;
  FName := ''; FDescription := ''; FCategory := 'ppm';
  if Assigned(AJson) then
  begin
    AJson.TryGetValue<String>('name',        FName);
    AJson.TryGetValue<String>('description', FDescription);
    AJson.TryGetValue<String>('category',    FCategory);
    if AJson.TryGetValue('inputSchema', JTmp) and (JTmp is TJSONObject) then
      FSchema := TJSONObject(JTmp.Clone)
    else
    begin
      FSchema := TJSONObject.Create;
      FSchema.AddPair('type', 'object');
      FSchema.AddPair('properties', TJSONObject.Create);
    end;
  end
  else
  begin
    FSchema := TJSONObject.Create;
    FSchema.AddPair('type', 'object');
    FSchema.AddPair('properties', TJSONObject.Create);
  end;
end;

destructor TAiSchemaTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TAiSchemaTool.GetName:        String;       begin Result := FName;        end;
function TAiSchemaTool.GetDescription: String;       begin Result := FDescription; end;
function TAiSchemaTool.GetCategory:    String;       begin Result := FCategory;    end;
function TAiSchemaTool.GetSchema:      TJSONObject;  begin Result := FSchema;      end;
function TAiSchemaTool.IsAvailable:    Boolean;      begin Result := True;         end;
function TAiSchemaTool.Execute(const AArgs: TJSONObject): TJSONObject;
begin
  Result := nil;  // sin implementacion nativa; el llamador usa fallback
end;

{ TAiToolRegistry }

constructor TAiToolRegistry.Create;
begin
  inherited Create;
  FEntries := TList<TAiRegistryEntry>.Create;
  FPPMBase := 'https://registry.pascalai.org';
end;

destructor TAiToolRegistry.Destroy;
begin
  FEntries.Free;
  inherited;
end;

class function TAiToolRegistry.Instance: TAiToolRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TAiToolRegistry.Create;
  Result := FInstance;
end;

class procedure TAiToolRegistry.DropInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TAiToolRegistry.Register(const ATool: IAiTool;
  const AOrigin, ASourceId: String);
var
  Entry    : TAiRegistryEntry;
  Existing : IAiTool;
begin
  if not Assigned(ATool) then Exit;
  if TryFind(ATool.Name, Existing) then Exit;  // evitar duplicados
  Entry.Tool     := ATool;
  Entry.Origin   := AOrigin;
  Entry.SourceId := ASourceId;
  FEntries.Add(Entry);
end;

function TAiToolRegistry.RegisterFromMCP(AClient: TMCPClientCustom): Integer;
var
  Tools : TArray<IAiTool>;
  T     : IAiTool;
begin
  Result := 0;
  if not Assigned(AClient) then Exit;
  Tools := TAiMCPToolFactory.CreateFromClient(AClient);
  for T in Tools do
  begin
    Register(T, 'mcp', AClient.Name);
    Inc(Result);
  end;
end;

function TAiToolRegistry.RegisterFromTAiFunctions(AFunctions: TAiFunctions): Integer;
var
  I          : Integer;
  ClientItem : TMCPClientItem;
  FuncItem   : TFunctionActionItem;
  Tool       : IAiTool;
begin
  Result := 0;
  if not Assigned(AFunctions) then Exit;

  // Registrar clientes MCP del componente
  for I := 0 to AFunctions.MCPClients.Count - 1 do
  begin
    ClientItem := AFunctions.MCPClients[I];
    if ClientItem.Enabled and Assigned(ClientItem.MCPClient) then
      Inc(Result, RegisterFromMCP(ClientItem.MCPClient));
  end;

  // Registrar funciones locales habilitadas
  for I := 0 to AFunctions.Functions.Count - 1 do
  begin
    FuncItem := AFunctions.Functions[I];
    if FuncItem.Enabled and Assigned(FuncItem.OnAction) then
    begin
      Tool := TAiFunctionItem_IAiTool.Create(FuncItem);
      Register(Tool, 'local', AFunctions.Name);
      Inc(Result);
    end;
  end;
end;

function TAiToolRegistry.Find(const AName: String): IAiTool;
begin
  if not TryFind(AName, Result) then
    raise EAiToolNotFound.CreateFmt('Herramienta "%s" no encontrada en el registry.', [AName]);
end;

function TAiToolRegistry.TryFind(const AName: String; out ATool: IAiTool): Boolean;
var
  i: Integer;
begin
  for i := 0 to FEntries.Count - 1 do
    if SameText(FEntries[i].Tool.Name, AName) then
    begin
      ATool  := FEntries[i].Tool;
      Result := True;
      Exit;
    end;
  ATool  := nil;
  Result := False;
end;

function TAiToolRegistry.GetAll: TArray<IAiTool>;
var
  List : TList<IAiTool>;
  i    : Integer;
begin
  List := TList<IAiTool>.Create;
  try
    for i := 0 to FEntries.Count - 1 do
      if FEntries[i].Tool.IsAvailable then
        List.Add(FEntries[i].Tool);
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TAiToolRegistry.GetEntries: TArray<TAiRegistryEntry>;
begin
  Result := FEntries.ToArray;
end;

procedure TAiToolRegistry.Unregister(const AName: String);
var
  i: Integer;
begin
  for i := FEntries.Count - 1 downto 0 do
    if SameText(FEntries[i].Tool.Name, AName) then
    begin
      FEntries.Delete(i);
      Break;
    end;
end;

procedure TAiToolRegistry.Clear;
begin
  FEntries.Clear;
end;

function TAiToolRegistry.Count: Integer;
begin
  Result := FEntries.Count;
end;

function TAiToolRegistry.SearchPPM(const AQuery: String;
  APage, APerPage: Integer): TArray<TAiPPMPackageInfo>;
var
  Http   : TNetHTTPClient;
  Resp   : IHTTPResponse;
  JResp  : TJSONObject;
  JData  : TJSONArray;
  JItem  : TJSONValue;
  JObj   : TJSONObject;
  List   : TList<TAiPPMPackageInfo>;
  Pkg    : TAiPPMPackageInfo;
  Url    : String;
  i      : Integer;
begin
  Result := nil;
  List   := TList<TAiPPMPackageInfo>.Create;
  Http   := TNetHTTPClient.Create(nil);
  try
    Http.Accept := 'application/json';
    Url := Format('%s/v1/search?q=%s&type=mcp&page=%d&per_page=%d',
                  [FPPMBase, TNetEncoding.URL.Encode(AQuery), APage, APerPage]);
    try
      Resp := Http.Get(Url);
      if Resp.StatusCode <> 200 then Exit;

      JResp := TJSONObject(TJSONObject.ParseJSONValue(Resp.ContentAsString));
      if not Assigned(JResp) then Exit;
      try
        // El registry devuelve { "packages": [...], "total", "page", "per_page" }
        if not JResp.TryGetValue<TJSONArray>('packages', JData) then Exit;
        for i := 0 to JData.Count - 1 do
        begin
          JItem := JData.Items[i];
          if not (JItem is TJSONObject) then Continue;
          JObj := TJSONObject(JItem);

          Pkg := Default(TAiPPMPackageInfo);
          JObj.TryGetValue<String>('name',        Pkg.Name);
          JObj.TryGetValue<String>('version',     Pkg.Version);
          JObj.TryGetValue<String>('description', Pkg.Description);
          // download_url no viene en el search; se obtiene con GetPPMPackage

          if Pkg.Name <> '' then
            List.Add(Pkg);
        end;
      finally
        JResp.Free;
      end;
    except
      // Error de red: devolver lista vacía
    end;
    Result := List.ToArray;
  finally
    Http.Free;
    List.Free;
  end;
end;

function TAiToolRegistry.GetPPMPackage(const AName, AVersion: String): TAiPPMPackageInfo;
// API Registry:
//   GET /v1/packages/{name}           → { "package": { "name","description","versions":[{"version","yanked"...}] } }
//   GET /v1/packages/{name}/{version} → { "version": { "package","version","description","download_url" } }
// Si no hay versión, primero obtenemos la última versión no-yanked del listado,
// luego consultamos el endpoint versionado para obtener el download_url.
var
  Http      : TNetHTTPClient;
  Resp      : IHTTPResponse;
  JResp     : TJSONObject;
  Url       : String;
  LatestVer : String;
  STmp      : String;
  JData     : TJSONObject;
begin
  Result := Default(TAiPPMPackageInfo);
  Result.Name := AName;
  Http := TNetHTTPClient.Create(nil);
  try
    Http.Accept := 'application/json';
    LatestVer := AVersion;

    // Si no viene versión, resolver la última desde GET /v1/packages/{name}
    if LatestVer = '' then
    begin
      Url := Format('%s/v1/packages/%s', [FPPMBase, AName]);
      try
        Resp := Http.Get(Url);
        if Resp.StatusCode = 200 then
        begin
          JResp := TJSONObject(TJSONObject.ParseJSONValue(Resp.ContentAsString));
          if Assigned(JResp) then
          try
            var JPkg: TJSONObject := nil;
            if JResp.TryGetValue<TJSONObject>('package', JPkg) then
            begin
              JPkg.TryGetValue<String>('name',        Result.Name);
              JPkg.TryGetValue<String>('description', Result.Description);
              // Tomar la primera versión no-yanked (el registry las devuelve en orden descendente)
              var JVersions: TJSONArray := nil;
              if JPkg.TryGetValue<TJSONArray>('versions', JVersions) then
              begin
                var i: Integer;
                for i := 0 to JVersions.Count - 1 do
                begin
                  var JV := JVersions.Items[i];
                  if not (JV is TJSONObject) then Continue;
                  var Yanked: Boolean := False;
                  TJSONObject(JV).TryGetValue<Boolean>('yanked', Yanked);
                  if not Yanked then
                  begin
                    TJSONObject(JV).TryGetValue<String>('version', LatestVer);
                    Break;
                  end;
                end;
              end;
            end;
          finally
            JResp.Free;
          end;
        end;
      except
      end;
    end;

    if LatestVer = '' then Exit;
    Result.Version := LatestVer;

    // Obtener detalles con download_url desde GET /v1/packages/{name}/{version}
    Url := Format('%s/v1/packages/%s/%s', [FPPMBase, Result.Name, LatestVer]);
    try
      Resp := Http.Get(Url);
      if Resp.StatusCode <> 200 then Exit;

      JResp := TJSONObject(TJSONObject.ParseJSONValue(Resp.ContentAsString));
      if not Assigned(JResp) then Exit;
      try
        // El registry devuelve { "version": { "package","version","description","download_url" } }
        JData := nil;
        if not JResp.TryGetValue<TJSONObject>('version', JData) then
          JData := JResp;

        if JData.TryGetValue<String>('package', STmp) and (STmp <> '') then
          Result.Name := STmp
        else if JData.TryGetValue<String>('name', STmp) and (STmp <> '') then
          Result.Name := STmp;
        if JData.TryGetValue<String>('version', STmp) and (STmp <> '') then
          Result.Version := STmp;
        if JData.TryGetValue<String>('description', STmp) and (STmp <> '') then
          Result.Description := STmp;
        if JData.TryGetValue<String>('download_url', STmp) and (STmp <> '') then
        begin
          // Convertir URL relativa a absoluta
          if STmp[1] = '/' then
            Result.DownloadUrl := FPPMBase + STmp
          else
            Result.DownloadUrl := STmp;
        end;
      finally
        JResp.Free;
      end;
    except
    end;
  finally
    Http.Free;
  end;
end;

function TAiToolRegistry.DownloadPackage(const APkg: TAiPPMPackageInfo): String;
// El .paipkg es un ZIP que contiene pai.package (manifiesto INI) + binarios.
// Descarga, extrae y devuelve el path del ejecutable según [mcp] entrypoint=...
var
  Http        : TNetHTTPClient;
  Stream      : TFileStream;
  Resp        : IHTTPResponse;
  DestDir     : String;
  ExtractDir  : String;
  PkgPath     : String;
  ManifestPath: String;
  Entrypoint  : String;
  Ini         : TMemIniFile;
  Zip         : TZipFile;
  Ok          : Boolean;
begin
  Result := '';
  if APkg.DownloadUrl = '' then Exit;

  if FToolsDir <> '' then
    DestDir := FToolsDir
  else
    DestDir := TPath.Combine(GetEnvironmentVariable('APPDATA'),
                             'MakerAI' + PathDelim + 'tools');

  if not ForceDirectories(DestDir) then Exit;

  // Directorio de extracción: tools\mcp-nombre\
  ExtractDir   := TPath.Combine(DestDir, APkg.Name);
  ManifestPath := TPath.Combine(ExtractDir, 'pai.package');

  // Si ya está extraído, leer entrypoint del manifiesto y devolver
  if TFile.Exists(ManifestPath) then
  begin
    Ini := TMemIniFile.Create(ManifestPath);
    try
      Entrypoint := Ini.ReadString('mcp', 'entrypoint', '');
    finally
      Ini.Free;
    end;
    if Entrypoint <> '' then
    begin
      Result := TPath.Combine(ExtractDir, Entrypoint);
      if TFile.Exists(Result) then Exit;
    end;
  end;

  // Descargar el .paipkg (ZIP) — la URL puede redirigir (302)
  PkgPath := TPath.Combine(DestDir, APkg.Name + '.paipkg');
  Http    := TNetHTTPClient.Create(nil);
  try
    Http.HandleRedirects := True;
    try
      Stream := TFileStream.Create(PkgPath, fmCreate);
      try
        Resp := Http.Get(APkg.DownloadUrl, Stream);
        Ok   := (Resp <> nil) and (Resp.StatusCode = 200);
      finally
        Stream.Free;
      end;
    except
      Ok := False;
    end;
  finally
    Http.Free;
  end;

  if not Ok then
  begin
    if TFile.Exists(PkgPath) then TFile.Delete(PkgPath);
    Exit;
  end;

  // Extraer el ZIP al directorio del paquete
  ForceDirectories(ExtractDir);
  try
    Zip := TZipFile.Create;
    try
      Zip.Open(PkgPath, zmRead);
      Zip.ExtractAll(ExtractDir);
      Zip.Close;
    finally
      Zip.Free;
    end;
    TFile.Delete(PkgPath); // limpiar .paipkg temporal
  except
    Exit; // extracción fallida
  end;

  // Leer entrypoint del manifiesto extraído
  if not TFile.Exists(ManifestPath) then Exit;

  Ini := TMemIniFile.Create(ManifestPath);
  try
    Entrypoint := Ini.ReadString('mcp', 'entrypoint', '');
  finally
    Ini.Free;
  end;

  if Entrypoint = '' then Exit;

  Result := TPath.Combine(ExtractDir, Entrypoint);
  if not TFile.Exists(Result) then Result := '';
end;

function TAiToolRegistry.InstallFromPPM(const APkg: TAiPPMPackageInfo;
  AOwner: TComponent): TMCPClientCustom;
var
  Client : TMCPClientStdIo;
  ExePath: String;
begin
  Result := nil;
  if APkg.Name = '' then Exit;

  // Si no tenemos DownloadUrl, obtener info completa del paquete vía registry
  var FullPkg: TAiPPMPackageInfo := APkg;
  if FullPkg.DownloadUrl = '' then
    FullPkg := GetPPMPackage(APkg.Name, APkg.Version);

  Client := TMCPClientStdIo.Create(AOwner);
  try
    Client.Name    := FullPkg.Name;
    Client.Enabled := True;

    // Preferir descargar el ejecutable si hay DownloadUrl
    if FullPkg.DownloadUrl <> '' then
    begin
      ExePath := DownloadPackage(FullPkg);
      if ExePath <> '' then
      begin
        Client.Params.Values['Command']   := ExePath;
        Client.Params.Values['Arguments'] := '--protocol stdio';
      end;
    end;

    // Fallback: Command/Args del paquete (ej. paquetes npm via npx)
    if Client.Params.Values['Command'] = '' then
    begin
      if FullPkg.Command <> '' then
        Client.Params.Values['Command'] := FullPkg.Command
      else
        Client.Params.Values['Command'] := 'npx';

      if FullPkg.Args <> '' then
        Client.Params.Values['Arguments'] := FullPkg.Args
      else
        Client.Params.Values['Arguments'] := '-y ' + FullPkg.Name;
    end;

    if Client.Initialize then
    begin
      RegisterFromMCP(Client);
      Result := Client;
    end
    else
    begin
      if AOwner = nil then
        Client.Free;
    end;
  except
    if AOwner = nil then
      Client.Free;
    raise;
  end;
end;

function TAiToolRegistry.InstallSchemaFromPPM(const APkg: TAiPPMPackageInfo): Integer;
// Descarga el .paipkg, extrae los .tool JSON y registra TAiSchemaTool.
// Si ya fue extraido anteriormente, omite la descarga y reutiliza el dir.
var
  FullPkg     : TAiPPMPackageInfo;
  DestDir     : String;
  ExtractDir  : String;
  ManifestPath: String;
  PkgPath     : String;
  Http        : TNetHTTPClient;
  Stream      : TFileStream;
  Resp        : IHTTPResponse;
  Zip         : TZipFile;
  Ok          : Boolean;
  Sr          : TSearchRec;
  ToolJson    : String;
  ToolObj     : TJSONObject;
  Tool        : IAiTool;
begin
  Result  := 0;
  FullPkg := APkg;
  if FullPkg.Name = '' then Exit;

  if FullPkg.DownloadUrl = '' then
    FullPkg := GetPPMPackage(APkg.Name, APkg.Version);
  if FullPkg.DownloadUrl = '' then Exit;

  if FToolsDir <> '' then
    DestDir := FToolsDir
  else
    DestDir := TPath.Combine(GetEnvironmentVariable('APPDATA'),
                             'MakerAI' + PathDelim + 'tools');

  if not ForceDirectories(DestDir) then Exit;

  ExtractDir   := TPath.Combine(DestDir, FullPkg.Name);
  ManifestPath := TPath.Combine(ExtractDir, 'pai.package');

  // Solo descargar si aun no fue extraido
  if not TFile.Exists(ManifestPath) then
  begin
    PkgPath := TPath.Combine(DestDir, FullPkg.Name + '.paipkg');
    Http    := TNetHTTPClient.Create(nil);
    try
      Http.HandleRedirects := True;
      try
        Stream := TFileStream.Create(PkgPath, fmCreate);
        try
          Resp := Http.Get(FullPkg.DownloadUrl, Stream);
          Ok   := (Resp <> nil) and (Resp.StatusCode = 200);
        finally
          Stream.Free;
        end;
      except
        Ok := False;
      end;
    finally
      Http.Free;
    end;

    if not Ok then
    begin
      if TFile.Exists(PkgPath) then TFile.Delete(PkgPath);
      Exit;
    end;

    ForceDirectories(ExtractDir);
    try
      Zip := TZipFile.Create;
      try
        Zip.Open(PkgPath, zmRead);
        Zip.ExtractAll(ExtractDir);
        Zip.Close;
      finally
        Zip.Free;
      end;
      TFile.Delete(PkgPath);
    except
      Exit;
    end;
  end;

  // Buscar archivos .tool y registrar
  if FindFirst(TPath.Combine(ExtractDir, '*.tool'), faAnyFile, Sr) = 0 then
  try
    repeat
      ToolJson := '';
      try
        ToolJson := TFile.ReadAllText(TPath.Combine(ExtractDir, Sr.Name),
                                      TEncoding.UTF8);
      except
      end;
      if ToolJson = '' then Continue;

      ToolObj := TJSONObject.ParseJSONValue(ToolJson) as TJSONObject;
      if Assigned(ToolObj) then
      try
        Tool := TAiSchemaTool.CreateFromJSON(ToolObj);
        if Tool.GetName <> '' then
        begin
          Register(Tool, 'ppm', FullPkg.Name);
          Inc(Result);
        end;
      finally
        ToolObj.Free;
      end;
    until FindNext(Sr) <> 0;
  finally
    FindClose(Sr);
  end;
end;

initialization

finalization
  TAiToolRegistry.DropInstance;

end.
