// MIT License
// MakerAI — Demo 066: AutoMCP Console Test (v2 — API actualizada)
//
// Prueba end-to-end del sistema AutoMCP:
//   1. Instala mcp-postgres via PPM
//   2. Lista sus herramientas
//   3. Verifica conectividad con PostgreSQL local
//   4. Chat interactivo con LLM + AutoMCP activo
//
// Variables de entorno:
//   DEMO_DRIVER  = Ollama (default)
//   DEMO_MODEL   = gpt-oss:20b (default)
//   DEMO_API_KEY = @OLLAMA_API_KEY (default)
//   DEMO_URL     = http://192.168.3.121:11434/ (default)
//   PG_CONN      = postgresql://postgres:masterkey@localhost:5432/postgres (default)

program AutoMCPConsoleTest;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils,
  System.IOUtils, System.Net.HttpClient, System.Generics.Collections,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Tools.Functions,
  uMakerAi.MCPClient.Core,
  uMakerAi.Chat.Initializations;

// ============================================================================
// Helpers
// ============================================================================
procedure Sep; begin Writeln(StringOfChar('-', 70)); end;
procedure OK(const S: string);   begin Writeln('  [OK]  ' + S); end;
procedure ERR(const S: string);  begin Writeln('  [ERR] ' + S); end;
procedure INFO(const S: string); begin Writeln('  [..]  ' + S); end;
procedure HDR(const S: string);
begin
  Writeln;
  Writeln('>>> ' + S);
  Sep;
end;

// ============================================================================
// Test principal
// ============================================================================
type
  TPostgresAutoMCPTest = class
  private
    FFunctions: TAiFunctions;
    FAiConn   : TAiChatConnection;
    FLastError: string;
    FPGConn   : string;

    procedure OnLog(Sender: TObject; const Msg: string);
    procedure OnStatus(Sender: TObject; const Msg: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string;
      AException: Exception; const AResponse: IHTTPResponse);
    procedure OnState(Sender: TObject; State: TAiChatState; const Desc: string);

    procedure TestInstallPostgres;
    procedure TestListTools;
    procedure TestDirectQuery;
    procedure TestChatPostgres;
    procedure TestTTSGeminiSetup;
    procedure TestTTSGeminiDirect;
    procedure TestTTSGeminiChat;
    procedure InteractiveLoop;
  public
    constructor Create(const ADriver, AModel, AApiKey, AUrl, APGConn: string);
    destructor  Destroy; override;
    procedure   Run;
  end;

// ----------------------------------------------------------------------------
constructor TPostgresAutoMCPTest.Create(const ADriver, AModel, AApiKey, AUrl, APGConn: string);
begin
  inherited Create;
  FPGConn := APGConn;

  // TAiFunctions — AutoMCP activo
  FFunctions := TAiFunctions.Create(nil);
  FFunctions.OnLog          := OnLog;
  FFunctions.OnStatusUpdate := OnStatus;
  FFunctions.AutoMCPConfig.Active      := True;
  FFunctions.AutoMCPConfig.RegistryUrl := 'https://registry.pascalai.org';

  // TAiChatConnection
  FAiConn := TAiChatConnection.Create(nil);
  FAiConn.AiFunctions   := FFunctions;
  FAiConn.OnError       := OnError;
  FAiConn.OnStateChange := OnState;

  FAiConn.RegisterUserParam(ADriver, 'Tool_Active',   'True');
  FAiConn.RegisterUserParam(ADriver, 'Asynchronous',  'False');
  FAiConn.RegisterUserParam(ADriver, 'Max_Tokens',    '4096');
  FAiConn.RegisterUserParam(ADriver, 'ResponseTimeOut', '120000');

  FAiConn.DriverName              := ADriver;
  FAiConn.Model                   := AModel;
  FAiConn.Params.Values['ApiKey'] := AApiKey;
  if AUrl <> '' then
    FAiConn.Params.Values['Url']  := AUrl;

  FAiConn.SystemPrompt.Text := FFunctions.GetAutoMCPSystemPrompt;
end;

destructor TPostgresAutoMCPTest.Destroy;
begin
  FAiConn.Free;
  FFunctions.Free;
  inherited;
end;

// --- Eventos ----------------------------------------------------------------
procedure TPostgresAutoMCPTest.OnLog(Sender: TObject; const Msg: string);
begin Writeln('  [MCP]  ' + Msg); end;

procedure TPostgresAutoMCPTest.OnStatus(Sender: TObject; const Msg: string);
begin Writeln('  [STS]  ' + Msg); end;

procedure TPostgresAutoMCPTest.OnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  FLastError := ErrorMsg;
  Writeln('  [ERR]  ' + ErrorMsg);
end;

procedure TPostgresAutoMCPTest.OnState(Sender: TObject; State: TAiChatState;
  const Desc: string);
begin
  case State of
    acsToolCalling   : INFO('LLM → tool call...');
    acsToolExecuting : INFO('Ejecutando tool...');
  end;
end;

// ============================================================================
// TEST 1 — Instalar mcp-postgres via PPM
// ============================================================================
procedure TPostgresAutoMCPTest.TestInstallPostgres;
var
  LItem: TMCPClientItem;
begin
  HDR('TEST 1 — Instalar mcp-postgres desde PPM');

  // Verificar si ya está
  LItem := FFunctions.MCPClients.GetClientByName('mcp-postgres');
  if Assigned(LItem) then
  begin
    INFO('mcp-postgres ya registrado: ' + LItem.Params.Values['Command']);
    INFO('Available: ' + BoolToStr(LItem.MCPClient.Available, True));
    Exit;
  end;

  // Instalar
  INFO('Descargando e instalando mcp-postgres...');
  LItem := FFunctions.InstallMCPFromPPM('mcp-postgres', '', '',
    'https://registry.pascalai.org');

  if not Assigned(LItem) then
  begin
    ERR('InstallMCPFromPPM retornó nil. Verificar conectividad con PPM.');
    Exit;
  end;

  OK('Instalado en: ' + LItem.Params.Values['Command']);

  // Inicializar el proceso MCP
  INFO('Inicializando proceso MCP...');
  if not LItem.MCPClient.Initialized then
    LItem.MCPClient.Initialize;

  if LItem.MCPClient.Available then
    OK('Proceso MCP activo y disponible.')
  else
    ERR('El proceso MCP NO quedó disponible tras Initialize.');
end;

// ============================================================================
// TEST 2 — Listar herramientas del servidor mcp-postgres
// ============================================================================
procedure TPostgresAutoMCPTest.TestListTools;
var
  LItem: TMCPClientItem;
  LToolsJson: string;
  LRoot, LTool: TJSONObject;
  LArr: TJSONArray;
  LName, LDesc: string;
begin
  HDR('TEST 2 — Listar herramientas de mcp-postgres');

  LItem := FFunctions.MCPClients.GetClientByName('mcp-postgres');
  if not Assigned(LItem) then
  begin
    ERR('mcp-postgres no está registrado. Ejecuta TEST 1 primero.');
    Exit;
  end;

  if not LItem.MCPClient.Available then
  begin
    ERR('Proceso MCP no disponible.');
    Exit;
  end;

  LToolsJson := LItem.MCPClient.Tools.Text;
  INFO('JSON tools raw (primeros 400 chars):');
  Writeln('  ' + Copy(LToolsJson, 1, 400));
  Writeln;

  LRoot := TJSONObject.ParseJSONValue(LToolsJson) as TJSONObject;
  if not Assigned(LRoot) then
  begin
    ERR('Tools JSON no es un objeto válido.');
    Exit;
  end;
  try
    if LRoot.TryGetValue<TJSONArray>('tools', LArr) then
    begin
      INFO(Format('%d herramienta(s) disponibles:', [LArr.Count]));
      for var I := 0 to LArr.Count - 1 do
      begin
        LTool := LArr.Items[I] as TJSONObject;
        LName := '';
        LDesc := '';
        LTool.TryGetValue<string>('name', LName);
        LTool.TryGetValue<string>('description', LDesc);
        Writeln(Format('  [%d] %s — %s', [I, LName, Copy(LDesc, 1, 80)]));
      end;
    end
    else
      ERR('Sin campo "tools" en la respuesta del servidor.');
  finally
    LRoot.Free;
  end;
end;

// ============================================================================
// TEST 3 — Llamada directa a la función query (sin LLM)
// ============================================================================
procedure TPostgresAutoMCPTest.TestDirectQuery;
var
  LItem: TMCPClientItem;
  LArgs, LResult: TJSONObject;
  LMedia: TObjectList<TAiMediaFile>;
begin
  HDR('TEST 3 — Llamada directa: mcp-postgres → query "SELECT version()"');

  LItem := FFunctions.MCPClients.GetClientByName('mcp-postgres');
  if not Assigned(LItem) or not LItem.MCPClient.Available then
  begin
    ERR('mcp-postgres no disponible. Salta TEST 3.');
    Exit;
  end;

  // mcp-postgres es un dispatcher con parámetro "operation"
  // operations: query, execute, tables, schema, describe
  LArgs := TJSONObject.Create;
  LMedia := TObjectList<TAiMediaFile>.Create(True);
  try
    // mcp-postgres espera parámetros individuales, no connectionString
    LArgs.AddPair('operation', 'query');
    LArgs.AddPair('host',     'localhost');
    LArgs.AddPair('port',     TJSONNumber.Create(5432));
    LArgs.AddPair('database', 'postgres');
    LArgs.AddPair('user',     'postgres');
    LArgs.AddPair('password', 'masterkey');
    LArgs.AddPair('sql', 'SELECT version()');

    INFO('Llamando mcp-postgres (op=query) con: ' + LArgs.ToJSON);
    LResult := LItem.MCPClient.CallTool('mcp-postgres', LArgs, LMedia);
    if Assigned(LResult) then
    try
      OK('Respuesta: ' + Copy(LResult.ToJSON, 1, 400));
    finally
      LResult.Free;
    end
    else
      ERR('CallTool retornó nil.');
  finally
    LArgs.Free;
    LMedia.Free;
  end;
end;

// ============================================================================
// TEST 4 — Chat con LLM + AutoMCP (mismo escenario que el usuario)
// ============================================================================
procedure TPostgresAutoMCPTest.TestChatPostgres;
var
  LResp: string;
begin
  HDR('TEST 4 — Chat LLM + AutoMCP: verificar postgres local');
  FAiConn.NewChat;
  // Actualizar system prompt para incluir mcp-postgres si ya está instalado
  FAiConn.SystemPrompt.Text := FFunctions.GetAutoMCPSystemPrompt;

  INFO('System prompt:');
  Writeln(Copy(FAiConn.SystemPrompt.Text, 1, 600));
  Writeln;
  INFO('Enviando prompt...');

  FLastError := '';
  try
    LResp := FAiConn.AddMessageAndRun(
      'Puedes verificar si mi base de datos postgres local está activa? ' +
      'Usa esta cadena de conexión: ' + FPGConn +
      '. Ejecuta un SELECT 1 para confirmar que está activa.',
      'user', []);
  except
    on E: Exception do LResp := '(excepción: ' + E.Message + ')';
  end;

  if LResp <> '' then
  begin
    Writeln;
    OK('Respuesta del LLM:');
    Writeln(LResp);
    Writeln;
  end
  else if FLastError <> '' then
    ERR('Sin respuesta, error: ' + FLastError)
  else
    ERR('Sin respuesta y sin error registrado.');
end;

// ============================================================================
// TEST 5 — Instalar mcp-tts-gemini desde PPM (v1.1.0 — formato estándar)
// ============================================================================
procedure TPostgresAutoMCPTest.TestTTSGeminiSetup;
var
  LItem: TMCPClientItem;
begin
  HDR('TEST 5 — Instalar mcp-tts-gemini v1.1.0 desde PPM');

  LItem := FFunctions.MCPClients.GetClientByName('mcp-tts-gemini');
  if Assigned(LItem) then
    INFO('mcp-tts-gemini ya registrado: ' + LItem.Params.Values['Command'])
  else
  begin
    INFO('Descargando mcp-tts-gemini desde PPM...');
    LItem := FFunctions.InstallMCPFromPPM('mcp-tts-gemini', '', '',
      'https://registry.pascalai.org');
    if not Assigned(LItem) then
    begin
      ERR('InstallMCPFromPPM retornó nil.');
      Exit;
    end;
    OK('Instalado en: ' + LItem.Params.Values['Command']);
  end;

  INFO('Inicializando proceso MCP...');
  if not LItem.MCPClient.Initialized then
    LItem.MCPClient.Initialize;

  if LItem.MCPClient.Available then
  begin
    OK('Proceso activo.');
    var LRoot := TJSONObject.ParseJSONValue(LItem.MCPClient.Tools.Text) as TJSONObject;
    if Assigned(LRoot) then
    try
      var LArr: TJSONArray;
      if LRoot.TryGetValue<TJSONArray>('tools', LArr) then
      begin
        INFO(Format('%d herramienta(s):', [LArr.Count]));
        for var I := 0 to LArr.Count - 1 do
        begin
          var LTool := LArr.Items[I] as TJSONObject;
          var LName, LDesc: string;
          LTool.TryGetValue<string>('name', LName);
          LTool.TryGetValue<string>('description', LDesc);
          Writeln(Format('  [%d] %s — %s', [I, LName, Copy(LDesc, 1, 120)]));
        end;
      end;
    finally
      LRoot.Free;
    end;
  end
  else
    ERR('Proceso MCP no disponible. Verificar GEMINI_API_KEY.');
end;

// ============================================================================
// TEST 6 — Llamada directa a tts-gemini (sin LLM): ver JSON raw de retorno
// ============================================================================
procedure TPostgresAutoMCPTest.TestTTSGeminiDirect;
var
  LItem: TMCPClientItem;
  LArgs, LResult: TJSONObject;
  LMedia: TObjectList<TAiMediaFile>;
  LToolName, LRawJson: string;
begin
  HDR('TEST 6 — Llamada directa a tts-gemini: "hola"');

  LItem := FFunctions.MCPClients.GetClientByName('mcp-tts-gemini');
  if not Assigned(LItem) or not LItem.MCPClient.Available then
  begin
    ERR('mcp-tts-gemini no disponible. Ejecuta TEST 5 primero.');
    Exit;
  end;

  // Determinar nombre real del tool TTS en el servidor
  LToolName := '';
  var LToolsRoot := TJSONObject.ParseJSONValue(LItem.MCPClient.Tools.Text) as TJSONObject;
  if Assigned(LToolsRoot) then
  try
    var LArr: TJSONArray;
    if LToolsRoot.TryGetValue<TJSONArray>('tools', LArr) and (LArr.Count > 0) then
      (LArr.Items[0] as TJSONObject).TryGetValue<string>('name', LToolName);
  finally
    LToolsRoot.Free;
  end;

  if LToolName = '' then
  begin
    ERR('No se encontró ningún tool en tts-gemini.');
    Exit;
  end;

  INFO('Llamando tool: "' + LToolName + '" con texto "hola"');

  LArgs  := TJSONObject.Create;
  LMedia := TObjectList<TAiMediaFile>.Create(True);
  try
    LArgs.AddPair('text', 'hola');

    LResult := LItem.MCPClient.CallTool(LToolName, LArgs, LMedia);
    if Assigned(LResult) then
    try
      LRawJson := LResult.ToJSON;
      Writeln;
      Writeln('  JSON raw completo:');
      Writeln('  ' + Copy(LRawJson, 1, 1000));
      if Length(LRawJson) > 1000 then
        Writeln(Format('  ... [total %d chars]', [Length(LRawJson)]));
      Writeln;

      // ¿Hay audio en el contenido?
      var LContent: TJSONArray;
      if LResult.TryGetValue<TJSONArray>('content', LContent) then
      begin
        INFO(Format('%d elemento(s) en "content":', [LContent.Count]));
        for var I := 0 to LContent.Count - 1 do
        begin
          var LObj := LContent.Items[I] as TJSONObject;
          var LType, LMimeType, LData: string;
          LObj.TryGetValue<string>('type', LType);
          LObj.TryGetValue<string>('mimeType', LMimeType);
          LObj.TryGetValue<string>('data', LData);
          Writeln(Format('  [%d] type=%s  mimeType=%s  data_len=%d',
            [I, LType, LMimeType, Length(LData)]));
        end;
      end;

      // ¿Llegaron archivos media extraídos?
      if LMedia.Count > 0 then
      begin
        INFO(Format('%d archivo(s) media extraído(s):', [LMedia.Count]));
        for var MF in LMedia do
        begin
          var LStream := MF.Content; // GetContent ahora resetea Position := 0
          Writeln(Format('  • %s  cat=%d  stream.Size=%d  stream.Position=%d',
            [MF.FileName, Ord(MF.FileCategory), LStream.Size, LStream.Position]));

          // Guardar a disco para poder reproducirlo manualmente
          if LStream.Size > 0 then
          begin
            var LSavePath := TPath.Combine(TPath.GetTempPath, MF.FileName);
            LStream.SaveToFile(LSavePath);
            OK('Audio guardado en: ' + LSavePath);
          end;
        end;
      end
      else
        INFO('(sin archivos media extraídos — el audio puede estar inline en JSON)');
    finally
      LResult.Free;
    end
    else
      ERR('CallTool retornó nil.');
  finally
    LArgs.Free;
    LMedia.Free;
  end;
end;

// ============================================================================
// TEST 7 — Chat LLM: pedir a la IA que use tts-gemini para decir "hola"
// ============================================================================
procedure TPostgresAutoMCPTest.TestTTSGeminiChat;
var
  LResp: string;
begin
  HDR('TEST 7 — Chat LLM + AutoMCP: usa tts-gemini para decir "hola"');
  FAiConn.NewChat;
  // Actualizar system prompt — tts-gemini debería aparecer como ya instalado
  FAiConn.SystemPrompt.Text := FFunctions.GetAutoMCPSystemPrompt;

  INFO('System prompt (primeros 400):');
  Writeln(Copy(FAiConn.SystemPrompt.Text, 1, 400));
  Writeln;

  INFO('Prompt: usar tts-gemini para decir hola...');
  FLastError := '';
  try
    LResp := FAiConn.AddMessageAndRun(
      'Usa el servidor MCP mcp-tts-gemini para generar audio con el texto "hola". ' +
      'Llama a call_mcp_tool con tool_name="mcp-tts-gemini" y el texto. ' +
      'Muéstrame la respuesta JSON cruda que devuelve el servidor.',
      'user', []);
  except
    on E: Exception do LResp := '(excepción: ' + E.Message + ')';
  end;

  if LResp <> '' then
  begin
    Writeln;
    OK('Respuesta del LLM:');
    Writeln(LResp);
  end
  else
    ERR('Sin respuesta. Error: ' + FLastError);
end;

// ============================================================================
// Modo interactivo
// ============================================================================
procedure TPostgresAutoMCPTest.InteractiveLoop;
var
  LInput, LResp: string;
begin
  HDR('MODO INTERACTIVO (AutoMCP activo — Ollama + gpt-oss)');
  Writeln('  /new    = nueva conversación');
  Writeln('  /tools  = listar tools disponibles');
  Writeln('  /exit   = salir');
  Writeln;
  FAiConn.NewChat;
  FAiConn.SystemPrompt.Text := FFunctions.GetAutoMCPSystemPrompt;

  repeat
    Write('Tú> ');
    Readln(LInput);
    LInput := Trim(LInput);
    if LInput = '' then Continue;
    if SameText(LInput, '/exit') then Break;
    if SameText(LInput, '/new')  then begin FAiConn.NewChat; OK('Nueva conversación.'); Continue; end;
    if SameText(LInput, '/tools') then
    begin
      var LToolsJson := FFunctions.GetTools(tfOpenAI);
      var LRoot := TJSONObject.ParseJSONValue(LToolsJson) as TJSONObject;
      if Assigned(LRoot) then
      try
        var LArr: TJSONArray;
        if LRoot.TryGetValue<TJSONArray>('tools', LArr) then
          for var I := 0 to LArr.Count - 1 do
          begin
            var LTool := LArr.Items[I] as TJSONObject;
            var LFunc: TJSONObject;
            var LName: string;
            if LTool.TryGetValue<TJSONObject>('function', LFunc) then
              LFunc.TryGetValue<string>('name', LName)
            else
              LTool.TryGetValue<string>('name', LName);
            Writeln('  • ' + LName);
          end;
      finally
        LRoot.Free;
      end;
      Continue;
    end;

    FLastError := '';
    try
      LResp := FAiConn.AddMessageAndRun(LInput, 'user', []);
    except
      on E: Exception do LResp := '(excepción: ' + E.Message + ')';
    end;
    if LResp <> '' then
    begin
      Writeln;
      Writeln('AI> ' + LResp);
      Writeln;
    end
    else if FLastError <> '' then
      ERR('Error: ' + FLastError);
  until False;
end;

// ============================================================================
// Run
// ============================================================================
procedure TPostgresAutoMCPTest.Run;
var
  LYN: string;
begin
  TestInstallPostgres;
  TestListTools;
  TestDirectQuery;
  TestChatPostgres;
  TestTTSGeminiSetup;
  TestTTSGeminiDirect;
  TestTTSGeminiChat;

  Sep;
  Write('¿Iniciar modo interactivo? (s/n): ');
  Readln(LYN);
  if SameText(Trim(LYN), 's') or SameText(Trim(LYN), 'y') then
    InteractiveLoop;
end;

// ============================================================================
// Main
// ============================================================================
var
  GTest: TPostgresAutoMCPTest;
  LDriver, LModel, LApiKey, LUrl, LPGConn: string;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Writeln('╔══════════════════════════════════════════════════════════════════╗');
    Writeln('║   MakerAI — Demo 066: AutoMCP + PostgreSQL Console Test          ║');
    Writeln('╚══════════════════════════════════════════════════════════════════╝');
    Writeln;

    LDriver := GetEnvironmentVariable('DEMO_DRIVER');
    if LDriver = '' then LDriver := 'Ollama';

    LModel := GetEnvironmentVariable('DEMO_MODEL');
    if LModel = '' then LModel := 'gpt-oss:20b';

    LApiKey := GetEnvironmentVariable('DEMO_API_KEY');
    if LApiKey = '' then LApiKey := '@OLLAMA_API_KEY';

    LUrl := GetEnvironmentVariable('DEMO_URL');
    if LUrl = '' then LUrl := 'http://192.168.3.121:11434/';

    LPGConn := GetEnvironmentVariable('PG_CONN');
    if LPGConn = '' then LPGConn := 'postgresql://postgres:masterkey@localhost:5432/postgres';

    Writeln('Driver : ' + LDriver);
    Writeln('Model  : ' + LModel);
    Writeln('URL    : ' + LUrl);
    Writeln('PG     : ' + LPGConn);
    Sep;
    Writeln;

    GTest := TPostgresAutoMCPTest.Create(LDriver, LModel, LApiKey, LUrl, LPGConn);
    try
      GTest.Run;
    finally
      GTest.Free;
    end;
  except
    on E: Exception do
      Writeln('[FATAL] ' + E.ClassName + ': ' + E.Message);
  end;

  Writeln;
  Write('Presiona Enter para salir...');
  Readln;
end.
