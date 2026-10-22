unit uMCPServerFileSystem_Tool;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, uMakerAi.Tools.Functions, uMakerAi.MCPServer.Core,
  UMakerAi.MCPServer.Stdio, uMakerAi.Core, uMakerAi.Chat.Messages;

type
  TFMCPServerFileSystem_Tool = class(TDataModule)
    AiMCPStdioServer1: TAiMCPStdioServer;
    AiFunctions1: TAiFunctions;
    procedure AiFunctions1Functions0fs_listarAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  private
    FSandboxDir: string;
  public
    // Crea la carpeta sandbox (y un archivo de ejemplo si esta vacia)
    procedure PrepareSandbox;
    // Unica carpeta que la tool puede listar
    property SandboxDir: string read FSandboxDir write FSandboxDir;
  end;

var
  FMCPServerFileSystem_Tool: TFMCPServerFileSystem_Tool;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TFMCPServerFileSystem_Tool.PrepareSandbox;
begin
  if FSandboxDir = '' then
    FSandboxDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'sandbox');
  FSandboxDir := IncludeTrailingPathDelimiter(TPath.GetFullPath(FSandboxDir));
  ForceDirectories(FSandboxDir);
  if Length(TDirectory.GetFiles(FSandboxDir)) = 0 then
    TFile.WriteAllText(FSandboxDir + 'leeme.txt', 'Archivo de ejemplo del sandbox MCP.', TEncoding.UTF8);
end;

procedure TFMCPServerFileSystem_Tool.AiFunctions1Functions0fs_listarAction(
  Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Filtro: string;
  Archivos: TStringList;
  Archivo: string;
begin
  Handled := True;
  Filtro := Trim(ToolCall.Params.Values['filtro']);
  if Filtro = '' then
    Filtro := '*.*';

  // El filtro es solo una mascara de nombre: nada de rutas ni subir de
  // carpeta. Es lo unico que impide salir del sandbox.
  if (Pos('..', Filtro) > 0) or (Filtro.IndexOfAny(['\', '/', ':']) >= 0) then
  begin
    ToolCall.Response := 'Error: el filtro debe ser una mascara de nombre (p.ej. *.txt), sin rutas.';
    Exit;
  end;

  Archivos := TStringList.Create;
  try
    for Archivo in TDirectory.GetFiles(FSandboxDir, Filtro) do
      Archivos.Add(ExtractFileName(Archivo));
    if Archivos.Count = 0 then
      ToolCall.Response := 'No hay archivos que coincidan con ' + Filtro
    else
      ToolCall.Response := Archivos.Text;
  finally
    Archivos.Free;
  end;
end;

end.
