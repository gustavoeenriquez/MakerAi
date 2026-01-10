unit uMCPServerAiFunctionsDm;

interface

uses
  System.SysUtils, System.Classes,

  uMakerAi.Core,
  uMakerAi.Tools.Functions,
  uMakerAi.MCPServer.Core,
  uMakerAi.MCPServer.Stdio,
  uMakerAi.Chat.Messages,
  uMakerAi.MCPServer.Bridge, UMakerAi.MCPServer.Http;

type
  TMCPServerAiFunctionsDm = class(TDataModule)
    AiFunctions1: TAiFunctions;
    AiMCPServer1: TAiMCPStdioServer;
    AiMCPHttpServer1: TAiMCPHttpServer;
    procedure AiFunctions1Functions0GetFechaHoraAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure SetupAiFunctions;
  public
    procedure StartServer;
  end;

var
  MCPServerAiFunctionsDm: TMCPServerAiFunctionsDm;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

procedure TMCPServerAiFunctionsDm.AiFunctions1Functions0GetFechaHoraAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := FormatDateTime('YYYY_MM_DD hh:nn:ss', Now);
  Handled := True;
end;

procedure TMCPServerAiFunctionsDm.DataModuleCreate(Sender: TObject);
begin
  SetupAiFunctions;
end;

procedure TMCPServerAiFunctionsDm.SetupAiFunctions;
begin
{

// Esta es otra forma de anexar las funciones manualmente y no por el IDE de Delphi
var
  Func: TFunctionActionItem;
  Param: TFunctionParamsItem;
begin
  AiFunctions1.Functions.Clear;

  // --- FUNCIÓN 1: Información del Sistema ---
  Func := AiFunctions1.Functions.Add;
  Func.FunctionName := 'get_system_info';
  Func.Description.Text := 'Retorna información básica del servidor Delphi.';
  Func.OnAction := OnGetSystemInfo;

  // --- FUNCIÓN 2: Escribir Nota ---
  Func := AiFunctions1.Functions.Add;
  Func.FunctionName := 'write_note';
  Func.Description.Text := 'Guarda una nota de texto en el servidor.';
  Func.OnAction := OnWriteNote;

  // Parámetro: Título
  Param := Func.Parameters.Add;
  Param.Name := 'title';
  Param.ParamType := ptString;
  Param.Description.Text := 'Título del archivo';
  Param.Required := True;

  // Parámetro: Contenido
  Param := Func.Parameters.Add;
  Param.Name := 'content';
  Param.ParamType := ptString;
  Param.Description.Text := 'Contenido de la nota';
  Param.Required := True;

}

end;

procedure TMCPServerAiFunctionsDm.StartServer;
begin
  AiMCPServer1.Start;
  WriteLn(ErrOutput, '✅ Servidor MCP iniciado desde DataModule.');
end;

end.
