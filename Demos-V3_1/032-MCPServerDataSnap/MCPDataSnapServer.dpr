program MCPDataSnapServer;
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Types,
  IPPeerServer,
  IPPeerAPI,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  Datasnap.DSSession,
  uServerMethods in 'uServerMethods.pas' {SSE: TDataModule},
  uServerContainer in 'uServerContainer.pas' {ServerContainer2: TDataModule},
  uWebModule in 'uWebModule.pas' {WebModule2: TWebModule},
  uServerConst in 'uServerConst.pas',
  uTool.DataSnap.FileAccess in 'uTool.DataSnap.FileAccess.pas';

{$R *.res}


Const
  DEFAULT_PORT = 8000;

procedure TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
    TDSSessionManager.Instance.TerminateAllSessions;
end;

function BindPort(APort: Integer): Boolean;
var
  LTestServer: IIPTestServer;
begin
  Result := True;
  try
    LTestServer := PeerFactory.CreatePeer('', IIPTestServer) as IIPTestServer;
    LTestServer.TestOpenPort(APort, nil);
  except
    Result := False;
  end;
end;

function CheckPort(APort: Integer): Integer;
begin
  if BindPort(APort) then
    Result := APort
  else
    Result := 0;
end;

procedure SetPort(const AServer: TIdHTTPWebBrokerBridge; APort: String);
begin
  if not AServer.Active then
  begin
    APort := APort.Replace(cCommandSetPort, '').Trim;
    if CheckPort(APort.ToInteger) > 0 then
    begin
      AServer.DefaultPort := APort.ToInteger;
      Writeln(Format(sPortSet, [APort]));
    end
    else
      Writeln(Format(sPortInUse, [APort]));
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StartServer(const AServer: TIdHTTPWebBrokerBridge);
begin
  if not AServer.Active then
  begin
    if CheckPort(AServer.DefaultPort) > 0 then
    begin
      Writeln(Format(sStartingServer, [AServer.DefaultPort]));
      AServer.Bindings.Clear;
      AServer.Active := True;
    end
    else
      Writeln(Format(sPortInUse, [AServer.DefaultPort.ToString]));
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StopServer(const AServer: TIdHTTPWebBrokerBridge);
begin
  if AServer.Active then
  begin
    Writeln(sStoppingServer);
    TerminateThreads;
    AServer.Active := False;
    AServer.Bindings.Clear;
    Writeln(sServerStopped);
  end
  else
    Writeln(sServerNotRunning);
  Write(cArrow);
end;

procedure WriteCommands;
begin
  Writeln(sCommands);
  Write(cArrow);
end;

procedure WriteStatus(const AServer: TIdHTTPWebBrokerBridge);
begin
  Writeln(sIndyVersion + AServer.SessionList.Version);
  Writeln(sActive + AServer.Active.ToString(TUseBoolStrs.True));
  Writeln(sPort + AServer.DefaultPort.ToString);
  Writeln(sSessionID + AServer.SessionIDCookieName);
  Write(cArrow);
end;

procedure ShowServerBanner(APort: Integer);
const
  C_ClassName = 'TSSE'; // El nombre exacto de tu clase DataSnap
begin
  Writeln('');
  // ASCII Art for MakerAi
  Writeln('  __  __       _               _    _ ');
  Writeln(' |  \/  | __ _| | _____ _ __  / \  (_)');
  Writeln(' | |\/| |/ _` | |/ / _ \ ''__|/ _ \ | |');
  Writeln(' | |  | | (_| |   <  __/ |  / ___ \| |');
  Writeln(' |_|  |_|\__,_|_|\_\___|_| /_/   \_\_|   M C P   S E R V E R');
  Writeln('');
  Writeln('==============================================================================');
  Writeln('   MAKER AI DELPHI SUITE - DATASNAP (TSSE) IMPLEMENTATION');
  Writeln('==============================================================================');
  Writeln('');
  Writeln('This server exposes MCP tools via DataSnap REST methods.');
  Writeln('It uses TAiMCPDirectConnection to map REST calls to internal MCP logic.');
  Writeln('');
  Writeln('Suite Information:');
  Writeln('  * Edition:        MakerAi Delphi Suite');
  Writeln('  * Protocol:       tpMakerAI (DataSnap REST)');
  Writeln('  * Handler Class:  ' + C_ClassName);
  Writeln('');
  Writeln('------------------------------------------------------------------------------');
  Writeln('  CLIENT CONFIGURATION (TMCPClientMakerAi)');
  Writeln('------------------------------------------------------------------------------');
  Writeln('  Set the following properties in your Client:');
  Writeln('');
  Writeln(Format('  > URL:  http://localhost:%d/datasnap/rest/%s', [APort, C_ClassName]));
  Writeln('  > Name: MyDataSnapClient');
  Writeln('');
  Writeln('------------------------------------------------------------------------------');
  Writeln('  AVAILABLE ENDPOINTS (REST MAPPING)');
  Writeln('------------------------------------------------------------------------------');
  Writeln('');
  Writeln('  1. List Tools (GET):');
  Writeln(Format('     /datasnap/rest/%s/ListTools', [C_ClassName]));
  Writeln('');
  Writeln('  2. Call Tool (POST/GET):');
  Writeln(Format('     /datasnap/rest/%s/CallTool', [C_ClassName]));
  Writeln('     Params: "ToolName" (String), "Args" (JSON Object)');
  Writeln('');
  Writeln('  3. List Resources (GET):');
  Writeln(Format('     /datasnap/rest/%s/ListResources', [C_ClassName]));
  Writeln('');
  Writeln('  4. Read Resource (POST/GET):');
  Writeln(Format('     /datasnap/rest/%s/ReadResource', [C_ClassName]));
  Writeln('     Params: "AURI" (String)');
  Writeln('');
  Writeln('==============================================================================');
end;

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
  LResponse: string;
begin
  WriteCommands;
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    while True do
    begin
      Readln(LResponse);
      LResponse := LowerCase(LResponse);
      if LResponse.StartsWith(cCommandSetPort) then
        SetPort(LServer, LResponse)
      else if sametext(LResponse, cCommandStart) then
        StartServer(LServer)
      else if sametext(LResponse, cCommandStatus) then
        WriteStatus(LServer)
      else if sametext(LResponse, cCommandStop) then
        StopServer(LServer)
      else if sametext(LResponse, cCommandHelp) then
        WriteCommands
      else if sametext(LResponse, cCommandExit) then
        if LServer.Active then
        begin
          StopServer(LServer);
          break
        end
        else
          break
      else
      begin
        Writeln(sInvalidCommand);
        Write(cArrow);
      end;
    end;
    TerminateThreads();
  finally
    LServer.Free;
  end;
end;

begin

 ShowServerBanner(DEFAULT_PORT);

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(DEFAULT_PORT);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
