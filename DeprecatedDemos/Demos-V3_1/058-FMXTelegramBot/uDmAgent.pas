unit uDmAgent;

interface

uses
  System.SysUtils, System.Classes, uMakerAi.Core, uMakerAi.Chat.Messages, System.JSON, uMakerAi.Tools.Functions, uMakerAi.Chat.AiConnection, uMakerAi.Prompts;

type
  TDmAgent = class(TDataModule)
    AiPrompts1: TAiPrompts;
    AiConn: TAiChatConnection;
    AiFunctions1: TAiFunctions;
    procedure AiFunctions1Functions0ppm_installAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  private
    { Private declarations }
  public
    procedure DoLog(const ADir, AText: string);
  end;

  Function NewChat(parametros : String {aquí van los parametros que necesitemos}) : TDmAgent;

var
  DmAgent: TDmAgent;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

function InstallMCPPackage(const ACapability: string; out APkgName, AToolsList: string): Boolean;
begin
  Result    := False;
  APkgName  := '';
  AToolsList := '';
end;

Function NewChat(parametros : String {aquí van los parametros que necesitemos}) : TDmAgent;
Begin

End;


procedure TDmAgent.AiFunctions1Functions0ppm_installAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Capability: string;
  PkgName   : string;
  ToolsList : string;
  JResult   : TJSONObject;
  JArr      : TJSONArray;
  ToolName  : string;
  Args      : TJSONObject;
begin
  Capability := '';
  Args := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(Args) then
  try
    Args.TryGetValue<string>('capability', Capability);
  finally
    Args.Free;
  end;

  if Capability = '' then
  begin
    ToolCall.Response := '{"error": "capability parameter required"}';
    Handled := True;
    Exit;
  end;

  DoLog('TOOL', '[PPM] Buscando: ' + Capability);

  if InstallMCPPackage(Capability, PkgName, ToolsList) then
  begin
    DoLog('TOOL', Format('[PPM] %s instalado - tools: %s', [PkgName, ToolsList]));
    JResult := TJSONObject.Create;
    JArr    := TJSONArray.Create;
    try
      JResult.AddPair('installed', PkgName);
      JResult.AddPair('tools', JArr);
      for ToolName in ToolsList.Split([',']) do
        if Trim(ToolName) <> '' then JArr.Add(Trim(ToolName));
      JResult.AddPair('message',
        'Tools installed and available. Call them directly in the next step.');
      ToolCall.Response := JResult.ToJSON;
      JArr := nil;  // owned by JResult
    finally
      JResult.Free;
    end;
  end
  else
  begin
    ToolCall.Response := Format('{"error": "Could not install capability: %s. ' +
      'Try a more specific keyword."}', [Capability]);
    DoLog('TOOL', '[PPM] Instalación fallida para: ' + Capability);
  end;
  Handled := True;
end;

procedure TDmAgent.DoLog(const ADir, AText: string);
begin
  // stub — override or wire up a callback when needed
end;

end.
