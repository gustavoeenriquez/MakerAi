unit uDMFunction;

interface

uses
  System.SysUtils, System.Classes, uMakerAi.MCPServer.Core, System.JSON, uMakerAi.Core, uMakerAi.Chat.Messages, uMakerAi.Tools.Functions, uMakerAi.MCPServer.Http, uMakerAi.MCPServer.Stdio, MCP.Filesystem, UMakerAi.MCPServer.SSE;

type
  TDmFunction = class(TDataModule)
    AiMCPStdioServer1: TAiMCPStdioServer;
    AiMCPHttpServer1: TAiMCPHttpServer;
    AiFunctions1: TAiFunctions;
    AiMCPSSEHttpServer1: TAiMCPSSEHttpServer;
    procedure AiFunctions1Functions0ReadDirectoryAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure AiMCPHttpServer1ValidateRequest(Sender: TObject; const AAuthHeader, ARemoteIP: string; out AAuthContext: TAiAuthContext; out AIsValid: Boolean);
    procedure AiFunctions1Functions1CreateDirectoryAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DmFunction: TDmFunction;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

procedure TDmFunction.AiFunctions1Functions0ReadDirectoryAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
Var
  Path: String;
  Dir: TMCPFilesystem;
  FileArr: TArray<TDirectoryEntry>;
  Fil: TDirectoryEntry;
  Res: String;
  I: Integer;
begin
  var LArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(LArgs) then
  try
    LArgs.TryGetValue<string>('DirectoryPath', Path);
  finally
    LArgs.Free;
  end;
  // Path := 'c:\temp\';
  Try

    Dir := TMCPFilesystem.Create([]);
    Try
      FileArr := Dir.ListDirectory(Path);

      I := 0;
      For Fil in FileArr do
      Begin
        Inc(I);

        If I > 100 then
          break;
        Res := Res + Fil.Name + sLineBreak;
      End;

    Finally
      Dir.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Res := E.Message;
    End;
  End;

  ToolCall.Response := Res;

  Handled := True;
end;

procedure TDmFunction.AiFunctions1Functions1CreateDirectoryAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
Var
  Path: String;
  Dir: TMCPFilesystem;
  Res: String;
  I: Integer;
begin
  var LArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(LArgs) then
  try
    LArgs.TryGetValue<string>('DirectoryPath', Path);
  finally
    LArgs.Free;
  end;
  // Path := 'c:\temp\';
  Try

    Dir := TMCPFilesystem.Create([]);
    Try
      Dir.CreateDirectory(Path);
      Res := 'the directory was created';

    Finally
      Dir.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Res := E.Message;
    End;
  End;

  ToolCall.Response := Res;

  Handled := True;
end;

procedure TDmFunction.AiMCPHttpServer1ValidateRequest(Sender: TObject; const AAuthHeader, ARemoteIP: string; out AAuthContext: TAiAuthContext; out AIsValid: Boolean);
begin
  AIsValid := True;
end;

end.
