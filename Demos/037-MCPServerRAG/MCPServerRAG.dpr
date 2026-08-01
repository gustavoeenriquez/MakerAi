program MCPServerRAG;

(*
  Demo 037-MCPServerRAG

  Servidor MCP (protocolo SSE por defecto) que expone un sistema RAG real
  respaldado por SQL Server 2025 (TAiRAGVector + TAiRAGVectorMSSQLDriver +
  TAiOpenAiEmbeddings).

  Configuracion: MCPServerRAG.ini junto al ejecutable (se crea con defaults
  en la primera ejecucion). Ver secciones [Server], [Database], [Embeddings],
  [Search].

  Autenticacion: login/password quemados en uTool.RAG (RAG_LOGIN/RAG_PASSWORD).
  El cliente debe enviar uno de:
    Authorization: Basic base64(usuario:password)
    Authorization: Bearer usuario:password
    X-API-Key: usuario:password

  Uso:
    MCPServerRAG.exe [--config <ruta.ini>] [--protocol sse|http|stdio] [--port N]
  (los parametros de linea de comandos tienen prioridad sobre el .ini)
*)

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  uMakerAi.MCPServer.Core,
  UMakerAi.MCPServer.Stdio,
  UMakerAi.MCPServer.Http,
  UMakerAi.MCPServer.SSE,
  uTool.RAG in 'uTool.RAG.pas';

var
  MCPServer: TAiMCPServer;
  Cfg: TRagServerConfig;
  IniPath, CliProtocol: string;
  CliPort, i: Integer;

begin
  IniPath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'MCPServerRAG.ini');
  CliProtocol := '';
  CliPort := 0;

  i := 1;
  while i <= ParamCount do
  begin
    if SameText(ParamStr(i), '--config') and (i < ParamCount) then
    begin
      Inc(i);
      IniPath := ParamStr(i);
    end
    else if SameText(ParamStr(i), '--protocol') and (i < ParamCount) then
    begin
      Inc(i);
      CliProtocol := LowerCase(ParamStr(i));
    end
    else if SameText(ParamStr(i), '--port') and (i < ParamCount) then
    begin
      Inc(i);
      CliPort := StrToIntDef(ParamStr(i), 0);
    end;
    Inc(i);
  end;

  try
    Cfg := LoadServerConfig(IniPath);
    if CliProtocol <> '' then
      Cfg.Protocol := CliProtocol;
    if CliPort > 0 then
      Cfg.Port := CliPort;

    InitRagEngine(Cfg);
    try
      if SameText(Cfg.Protocol, 'sse') then
        MCPServer := TAiMCPSSEHttpServer.Create(nil)
      else if SameText(Cfg.Protocol, 'http') then
        MCPServer := TAiMCPHttpServer.Create(nil)
      else
        MCPServer := TAiMCPStdioServer.Create(nil);

      try
        MCPServer.ServerName := 'mcp-rag-vector';
        MCPServer.Port := Cfg.Port;
        MCPServer.CorsEnabled := True;
        MCPServer.CorsAllowedOrigins := '*';

        uTool.RAG.RegisterTools(MCPServer); // registra el tool y engancha la autenticacion

        MCPServer.Start;
        WriteLn(ErrOutput, Format('[mcp-rag-vector] listo. protocolo=%s puerto=%d config=%s',
          [Cfg.Protocol, Cfg.Port, IniPath]));

        while True do
          Sleep(1000);
      finally
        MCPServer.Free;
      end;
    finally
      DoneRagEngine;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, '[mcp-rag-vector] Fatal: ' + E.Message);
      Halt(1);
    end;
  end;
end.
