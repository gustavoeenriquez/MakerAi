unit uMCPServerFileSystem_Tool;

interface

uses
  System.SysUtils, System.Classes, uMakerAi.Tools.Functions, uMakerAi.MCPServer.Core, UMakerAi.MCPServer.Stdio, uMakerAi.Core, uMakerAi.Chat.Messages;

type
  TFMCPServerFileSystem_Tool = class(TDataModule)
    AiMCPStdioServer1: TAiMCPStdioServer;
    AiFunctions1: TAiFunctions;
    procedure AiFunctions1Functions0fs_listarAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMCPServerFileSystem_Tool: TFMCPServerFileSystem_Tool;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TFMCPServerFileSystem_Tool.AiFunctions1Functions0fs_listarAction(
  Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Filtro, ArchivosListados: String;
  SearchRec: TSearchRec;
  Ruta: String;
begin
  Filtro := ToolCall.Params.Values['filtro'];

{
  Ruta := 'D:\Taller\mcpdir\';
  ArchivosListados := '';

  // Si no hay filtro, usar *.*
  if Filtro = '' then
    Filtro := '*.*';

  // Buscar archivos
  if FindFirst(Ruta + Filtro, faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        // Excluir directorios '.' y '..'
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          // Agregar a la lista
          if ArchivosListados <> '' then
            ArchivosListados := ArchivosListados + #13#10;

          ArchivosListados := ArchivosListados + SearchRec.Name;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  }
  ArchivosListados := 'miarchivo.txt, holamundo.pas';

  ToolCall.Response := ArchivosListados;
  Handled := True;
end;

end.
