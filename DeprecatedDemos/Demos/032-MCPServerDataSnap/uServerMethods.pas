unit uServerMethods;

interface

uses System.SysUtils, System.Classes, System.Json, System.IOUtils,
  Datasnap.DSServer, Datasnap.DSAuth, uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Direct, Datasnap.DSSession;

type
{$METHODINFO ON}
  TSSE = class(TDataModule)
    MCPDirect: TAiMCPDirectConnection;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    Procedure InitMCP;
  public
    { Public declarations }
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function ListTools: TJSONObject;
    function CallTool(ToolName: string; Args: TJSONObject): TJSONObject;
  end;
{$METHODINFO OFF}

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

uses System.StrUtils, uTool.Datasnap.FileAccess;

procedure TSSE.DataModuleCreate(Sender: TObject);
begin
  InitMCP;
end;

procedure TSSE.DataModuleDestroy(Sender: TObject);
begin
  MCPDirect.Stop;
end;

function TSSE.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TSSE.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

procedure TSSE.InitMCP;
var
  UserConfig: TFileAccessConfig;
begin

  // 1. Configurar Usuario (Contexto de Seguridad)
  // DataSnap nos da la info de sesión actual
  if TDSSessionManager.GetThreadSession <> nil then
    MCPDirect.User := TDSSessionManager.GetThreadSession.UserName;

  // 1. Crear configuración personalizada para este usuario
  UserConfig := TFileAccessConfig.Create;

  // Limpiar defaults si quieres ser estricto
  UserConfig.ClearPermissions;

  // Agregar carpeta privada del usuario
  UserConfig.AddAllowedPath('C:\Users\' + TDSSessionManager.GetThreadSession.UserName);
  UserConfig.AddAllowedPath('C:\Empresa\DocumentosPublicos');
  UserConfig.AddAllowedPath(TPath.GetTempPath);

  // Agregar extensiones permitidas
  UserConfig.AddAllowedExtension('.txt');
  UserConfig.AddAllowedExtension('.pdf');

  // 2. Registrar herramientas pasándoles ESTA configuración
  // Nota: Las herramientas usarán esta config pero NO la liberarán (FOwnsConfig=False)
  // Debemos asegurarnos de que la configuración viva tanto como las herramientas.
  // Como las herramientas y el DataModule mueren juntos al final de la sesión,
  // podemos dejar que UserConfig se libere en el DataModuleDestroy o usar una interfaz para conteo de referencias.

  // TRUCO: Para simplificar la gestión de memoria sin interfaces, podemos hacer que
  // la PRIMERA herramienta sea dueña de la config, o registrarlas manualmente.

  MCPDirect.RegisterTool('list_files',
    function: IAiMCPTool
    begin
      Result := TListFilesTool.Create(UserConfig);
    end);

  // 3. Arrancar
  MCPDirect.Start;
end;

function TSSE.CallTool(ToolName: string; Args: TJSONObject): TJSONObject;
var
  ArgsClone: TJSONObject;
begin
  // Recordando el tema de ownership: clonamos para que McpDirect sea dueño de su copia
  if Assigned(Args) then
    ArgsClone := Args.Clone as TJSONObject
  else
    ArgsClone := TJSONObject.Create;

  try
    Result := MCPDirect.CallTool(ToolName, ArgsClone);
  except
    ArgsClone.Free;
    raise;
  end;
end;

function TSSE.ListTools: TJSONObject;
begin
  Result := MCPDirect.ListTools;
end;

end.
