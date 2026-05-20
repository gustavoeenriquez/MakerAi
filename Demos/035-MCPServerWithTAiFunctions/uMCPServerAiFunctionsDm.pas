unit uMCPServerAiFunctionsDm;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils,

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


procedure TMCPServerAiFunctionsDm.StartServer;
var
  I: Integer;
  Func: TFunctionActionItem;
begin
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, '=== MCP Server con TAiFunctions ===');

  // Iniciar el servidor deseado (cambiar aquí para usar StdIO en su lugar)
  //AiMCPServer1.Start;
  AiMCPHttpServer1.Start;

  // Mostrar información según el protocolo que quedó activo
  if AiMCPHttpServer1.IsActive then
  begin
    WriteLn(ErrOutput, 'Protocolo : HTTP');
    WriteLn(ErrOutput, 'Servidor  : ' + AiMCPHttpServer1.ServerName);
    WriteLn(ErrOutput, 'Puerto    : ' + IntToStr(AiMCPHttpServer1.Port));
    WriteLn(ErrOutput, 'Endpoint  : ' + AiMCPHttpServer1.Endpoint);
    WriteLn(ErrOutput, 'URL       : http://localhost:' + IntToStr(AiMCPHttpServer1.Port) + AiMCPHttpServer1.Endpoint);
    WriteLn(ErrOutput, 'CORS      : ' + IfThen(AiMCPHttpServer1.CorsEnabled,
      'Habilitado (' + AiMCPHttpServer1.CorsAllowedOrigins + ')', 'Deshabilitado'));
    WriteLn(ErrOutput, '');
    WriteLn(ErrOutput, 'Ejemplos de uso con curl:');
    WriteLn(ErrOutput, '  Listar herramientas:');
    WriteLn(ErrOutput, '    curl -X POST http://localhost:' + IntToStr(AiMCPHttpServer1.Port) +
      AiMCPHttpServer1.Endpoint + ' -H "Content-Type: application/json"' +
      ' -d "{""jsonrpc"":""2.0"",""method"":""tools/list"",""id"":1}"');
    WriteLn(ErrOutput, '  Llamar herramienta:');
    WriteLn(ErrOutput, '    curl -X POST http://localhost:' + IntToStr(AiMCPHttpServer1.Port) +
      AiMCPHttpServer1.Endpoint + ' -H "Content-Type: application/json"' +
      ' -d "{""jsonrpc"":""2.0"",""method"":""tools/call"",""params"":{""name"":""<herramienta>"",""arguments"":{}},""id"":2}"');
  end
  else if AiMCPServer1.IsActive then
  begin
    WriteLn(ErrOutput, 'Protocolo : StdIO');
    WriteLn(ErrOutput, 'Servidor  : ' + AiMCPServer1.ServerName);
    WriteLn(ErrOutput, 'Uso       : Registrar este ejecutable como subproceso en el cliente MCP.');
    WriteLn(ErrOutput, '            El cliente escribe en stdin y lee las respuestas desde stdout.');
    WriteLn(ErrOutput, '            Los mensajes de diagnóstico (como éste) van a stderr.');
  end;

  // Listar las herramientas registradas
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, 'Herramientas registradas (' + IntToStr(AiFunctions1.Functions.Count) + '):');
  for I := 0 to AiFunctions1.Functions.Count - 1 do
  begin
    Func := AiFunctions1.Functions[I];
    WriteLn(ErrOutput, '  [' + IntToStr(I + 1) + '] ' + Func.FunctionName +
      ' - ' + Func.Description.Text.Trim);
  end;

  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, '✅ Servidor iniciado. Esperando peticiones JSON-RPC...');
  WriteLn(ErrOutput, '');
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


end.
