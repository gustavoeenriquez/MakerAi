// MIT License
//
// MakerAI - Demo 064: PPM Auto Tools
// Demuestra cómo el LLM puede descubrir, instalar y ejecutar herramientas MCP
// del registry PPM de forma totalmente autónoma.
//
// Flujo autónomo:
//   1. Usuario pide algo que requiere acceso externo (ej: "busca repos de Pascal en GitHub")
//   2. LLM llama a ppm_search("github") → ve mcp-github disponible
//   3. LLM llama a ppm_install("mcp-github") → binario descargado y configurado
//   4. LLM llama a call_mcp_tool("mcp-github", "search_repos", '{"query":"pascal"}')
//   5. Resultado devuelto al LLM → respuesta al usuario

unit uMainPPMAutoTools;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.JSON, System.Generics.Collections, System.Threading, System.StrUtils,
  System.Net.HttpClient, System.SyncObjs,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.StdCtrls, FMX.Edit, FMX.Memo, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.ListBox,
  FMX.Objects,

  uMakerAi.Core, uMakerAi.Chat, uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection, uMakerAi.Chat.Initializations,
  uMakerAi.Chat.OpenAi, uMakerAi.Chat.Claude, uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Ollama, uMakerAi.Chat.Groq, uMakerAi.Chat.Grok,
  uMakerAi.Chat.DeepSeek, uMakerAi.Chat.Mistral, uMakerAi.Chat.Cohere,
  uMakerAi.Chat.Kimi, uMakerAi.Chat.LMStudio, uMakerAi.Chat.GenericLLM,
  uMakerAi.Tools.Functions, uMakerAi.MCPClient.Core,
  uMakerAi.UI.ChatList, uMakerAi.UI.ChatInput, uMakerAi.UI.ChatBubble;

type
  TfrmPPMAutoTools = class(TForm)
    // -- Componentes no visuales --
    AiConn: TAiChatConnection;
    AiFunctions1: TAiFunctions;

    // -- Layout principal --
    MainLayout: TLayout;

    // -- Panel izquierdo (configuración) --
    LeftPanel: TLayout;
    RectLeft: TRectangle;
    LblTitulo: TLabel;
    LblDriver: TLabel;
    ComboDriver: TComboBox;
    LblModel: TLabel;
    ComboModel: TComboBox;
    LblApiKey: TLabel;
    EditApiKey: TEdit;
    ChAsync: TCheckBox;
    LineSep1: TLine;
    LblInstalados: TLabel;
    MemoInstalados: TMemo;
    LineSep2: TLine;
    LblLog: TLabel;
    MemoLog: TMemo;

    // -- Panel derecho (chat) --
    ChatPanel: TLayout;
    ChatList1: TChatList;
    AniIndicator1: TAniIndicator;
    RectStatus: TRectangle;
    LblStatus: TLabel;
    ChatInput1: TChatInput;

    procedure FormCreate(Sender: TObject);
    procedure ComboDriverChange(Sender: TObject);
    procedure ComboModelChange(Sender: TObject);
    procedure EditApiKeyChange(Sender: TObject);
    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string;
      aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
    procedure ChatInput1Cancel(Sender: TObject);
    procedure AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string);
    procedure AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string);
    procedure AiConnError(Sender: TObject; const ErrorMsg: string;
      Exception: Exception; const aResponse: IHTTPResponse);
    procedure AiConnStateChange(Sender: TObject; State: TAiChatState;
      const Description: string);
    procedure AiFunctions1Log(Sender: TObject; const Msg: string);
    procedure AiFunctions1StatusUpdate(Sender: TObject; const StatusMsg: string);
    procedure FormDestroy(Sender: TObject);

    // -- Handlers de las 3 funciones PPM --
    procedure OnPpmSearch(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure OnPpmInstall(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure OnCallMcpTool(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);

  private
    FLastBubble: TChatBubble;
    FMCPLock: TCriticalSection; // Serializa llamadas concurrentes a CallTool

    procedure AddLog(const AMsg: string);
    procedure SetStatus(const AMsg: string);
    procedure AddInstalledTool(const AName, AExePath: string);
    procedure InitFunctions;
    procedure InitSystemPrompt;
    procedure InitDrivers;
    function  GetInstalledToolsFile: string;
    procedure SaveInstalledTools;
    procedure LoadInstalledTools;
    procedure UpdateSystemPromptWithInstalledTools;
  end;

var
  frmPPMAutoTools: TfrmPPMAutoTools;

implementation

{$R *.fmx}

const
  // Prompt que explica al LLM cómo usar las herramientas PPM autónomamente
  SYSTEM_PROMPT =
    'Eres un asistente inteligente con acceso al registry PPM (PascalAI Package Manager), ' +
    'un catálogo de más de 158 herramientas MCP que cubren GitHub, PostgreSQL, Slack, Jira, ' +
    'Stripe, DuckDB, Kubernetes, AWS S3, Azure, Kafka, GraphQL y muchas más.' + sLineBreak +
    sLineBreak +
    'Cuando el usuario te pida algo que requiere acceso a servicios externos, actúa de forma ' +
    'AUTÓNOMA siguiendo este flujo sin pedir permiso:' + sLineBreak +
    '  1. Llama a ppm_search con palabras clave para encontrar la herramienta adecuada.' + sLineBreak +
    '  2. Llama a ppm_install con el nombre exacto del paquete para instalarlo.' + sLineBreak +
    '  3. Llama a call_mcp_tool con el nombre del tool, la función y los argumentos JSON.' + sLineBreak +
    sLineBreak +
    'Convenciones importantes:' + sLineBreak +
    '- Los nombres de paquetes siguen el patrón mcp-<servicio> (ej: mcp-github, mcp-postgres).' + sLineBreak +
    '- Los argumentos de call_mcp_tool deben ser un JSON válido como string.' + sLineBreak +
    '- Si una instalación falla, informa al usuario con claridad.' + sLineBreak +
    '- Responde en el mismo idioma que el usuario.';

{ TfrmPPMAutoTools }

// ---------------------------------------------------------------------------
// Persistencia de herramientas instaladas
// ---------------------------------------------------------------------------

function TfrmPPMAutoTools.GetInstalledToolsFile: string;
begin
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) +
            '.ppm\ppm_installed.json';
end;

procedure TfrmPPMAutoTools.SaveInstalledTools;
var
  LArr: TJSONArray;
  LObj: TJSONObject;
  LItem: TMCPClientItem;
  I: Integer;
  LSL: TStringList;
begin
  LArr := TJSONArray.Create;
  try
    for I := 0 to AiFunctions1.MCPClients.Count - 1 do
    begin
      LItem := AiFunctions1.MCPClients[I];
      LObj := TJSONObject.Create;
      LObj.AddPair('name',      LItem.Name);
      LObj.AddPair('command',   LItem.Params.Values['Command']);
      LObj.AddPair('arguments', LItem.Params.Values['Arguments']);
      LObj.AddPair('rootdir',   LItem.Params.Values['RootDir']);
      LArr.Add(LObj);
    end;
    var LDir := ExtractFileDir(GetInstalledToolsFile);
    if not DirectoryExists(LDir) then
      ForceDirectories(LDir);
    LSL := TStringList.Create;
    try
      LSL.Text := LArr.Format;
      LSL.SaveToFile(GetInstalledToolsFile, TEncoding.UTF8);
    finally
      LSL.Free;
    end;
  finally
    LArr.Free;
  end;
end;

procedure TfrmPPMAutoTools.LoadInstalledTools;
var
  LJson: string;
  LArr: TJSONArray;
  LItem: TMCPClientItem;
  LSL: TStringList;
begin
  if not FileExists(GetInstalledToolsFile) then Exit;
  try
    LSL := TStringList.Create;
    try
      LSL.LoadFromFile(GetInstalledToolsFile, TEncoding.UTF8);
      LJson := LSL.Text;
    finally
      LSL.Free;
    end;
    LArr := TJSONObject.ParseJSONValue(LJson) as TJSONArray;
    if not Assigned(LArr) then Exit;
    try
      for var LVal in LArr do
      begin
        if not (LVal is TJSONObject) then Continue;
        var LO   := TJSONObject(LVal);
        var LName := LO.GetValue<string>('name', '');
        var LCmd  := LO.GetValue<string>('command', '');
        if (LName = '') or (LCmd = '') then Continue;
        if not FileExists(LCmd) then
        begin
          AddLog('LoadTools: ejecutable no encontrado para "' + LName + '", ignorando.');
          Continue;
        end;
        if Assigned(AiFunctions1.MCPClients.GetClientByName(LName)) then Continue;

        LItem := AiFunctions1.MCPClients.Add;
        LItem.Name          := LName;
        LItem.TransportType := tpStdIo;
        LItem.Params.Values['Command']   := LCmd;
        LItem.Params.Values['Arguments'] := LO.GetValue<string>('arguments', '--protocol stdio');
        LItem.Params.Values['RootDir']   := LO.GetValue<string>('rootdir', '');
        LItem.Enabled := False;
        LItem.UpdateClientProperties;

        AddInstalledTool(LName, LCmd);
        AddLog('LoadTools: "' + LName + '" restaurado desde disco.');
      end;
    finally
      LArr.Free;
    end;
  except
    on E: Exception do
      AddLog('LoadTools: error cargando herramientas — ' + E.Message);
  end;
  UpdateSystemPromptWithInstalledTools;
end;

procedure TfrmPPMAutoTools.UpdateSystemPromptWithInstalledTools;
var
  LSb: TStringBuilder;
  I: Integer;
begin
  if AiFunctions1.MCPClients.Count = 0 then
  begin
    AiConn.SystemPrompt.Text := SYSTEM_PROMPT;
    Exit;
  end;
  LSb := TStringBuilder.Create;
  try
    LSb.Append(SYSTEM_PROMPT);
    LSb.AppendLine;
    LSb.AppendLine;
    LSb.AppendLine('HERRAMIENTAS MCP YA INSTALADAS (no uses ppm_install para estas):');
    for I := 0 to AiFunctions1.MCPClients.Count - 1 do
      LSb.AppendLine('- ' + AiFunctions1.MCPClients[I].Name +
        ' (usa call_mcp_tool directamente, sin ppm_install)');
    LSb.AppendLine('Para estas herramientas: omite ppm_search y ppm_install, ve directo al paso 3.');
    AiConn.SystemPrompt.Text := LSb.ToString;
  finally
    LSb.Free;
  end;
end;

procedure TfrmPPMAutoTools.FormDestroy(Sender: TObject);
begin
  FMCPLock.Free;
end;

procedure TfrmPPMAutoTools.FormCreate(Sender: TObject);
var
  LDrivers, LModels: TStringList;
begin
  FLastBubble := nil;
  FMCPLock := TCriticalSection.Create;

  // Registrar drivers disponibles
  InitDrivers;

  // Configurar las 3 funciones PPM en AiFunctions1
  InitFunctions;

  // Asignar el system prompt base
  InitSystemPrompt;

  // Conectar AiFunctions1 con AiConn
  AiConn.AiFunctions := AiFunctions1;

  // Conectar eventos de estado y log
  AiConn.OnStateChange        := AiConnStateChange;
  AiFunctions1.OnLog          := AiFunctions1Log;
  AiFunctions1.OnStatusUpdate := AiFunctions1StatusUpdate;

  // Restaurar herramientas instaladas en sesiones anteriores
  LoadInstalledTools;

  // Poblar combo de drivers
  LDrivers := AiConn.GetDriversNames;
  try
    LDrivers.Sort;
    ComboDriver.Items.Assign(LDrivers);
    if ComboDriver.Items.Count > 0 then
    begin
      // Intentar preseleccionar OpenAI
      var Idx := ComboDriver.Items.IndexOf('OpenAI');
      if Idx >= 0 then
        ComboDriver.ItemIndex := Idx
      else
        ComboDriver.ItemIndex := 0;
      ComboDriverChange(nil);
    end;
  finally
    LDrivers.Free;
  end;
end;

procedure TfrmPPMAutoTools.InitDrivers;
begin
  // Configuraciones base por driver
  AiConn.RegisterUserParam('OpenAI',    'Asynchronous', 'True');
  AiConn.RegisterUserParam('OpenAI',    'Tool_Active',  'True');
  AiConn.RegisterUserParam('Claude',    'Asynchronous', 'True');
  AiConn.RegisterUserParam('Claude',    'Tool_Active',  'True');
  AiConn.RegisterUserParam('Gemini',    'Asynchronous', 'True');
  AiConn.RegisterUserParam('Gemini',    'Tool_Active',  'True');
  AiConn.RegisterUserParam('Groq',      'Asynchronous', 'True');
  AiConn.RegisterUserParam('Groq',      'Tool_Active',  'True');
  AiConn.RegisterUserParam('Ollama',    'Asynchronous', 'True');
  AiConn.RegisterUserParam('Ollama',    'Tool_Active',  'True');
  AiConn.RegisterUserParam('Ollama',    'Url',          'http://localhost:11434/');
end;

procedure TfrmPPMAutoTools.InitFunctions;
var
  LFn: TFunctionActionItem;
  LParam: TFunctionParamsItem;
begin
  AiFunctions1.Functions.Clear;

  // --- Función 1: ppm_search ---
  LFn := TFunctionActionItem(AiFunctions1.Functions.Add);
  LFn.FunctionName := 'ppm_search';
  LFn.Description.Text :=
    'Search for MCP tools in the PPM registry. Returns a list of available tools ' +
    'matching the query. Use this before calling ppm_install.';
  LFn.OnAction := OnPpmSearch;

  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name            := 'query';
  LParam.Description.Text := 'Search keywords (e.g.: github, postgres, slack, kubernetes)';
  LParam.ParamType        := ptString;
  LParam.Required         := True;

  // --- Función 2: ppm_install ---
  LFn := TFunctionActionItem(AiFunctions1.Functions.Add);
  LFn.FunctionName := 'ppm_install';
  LFn.Description.Text :=
    'Download and install an MCP tool from the PPM registry. ' +
    'After installation the tool is ready to use with call_mcp_tool. ' +
    'Do NOT ask for user confirmation — install immediately when needed.';
  LFn.OnAction := OnPpmInstall;

  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name            := 'tool_name';
  LParam.Description.Text := 'Exact PPM package name (e.g.: mcp-github, mcp-postgres, mcp-slack)';
  LParam.ParamType        := ptString;
  LParam.Required         := True;

  // --- Función 3: call_mcp_tool ---
  LFn := TFunctionActionItem(AiFunctions1.Functions.Add);
  LFn.FunctionName := 'call_mcp_tool';
  LFn.Description.Text :=
    'Call a specific function on an installed MCP tool. ' +
    'Use ppm_search and ppm_install first if the tool is not yet installed. ' +
    'You can get the available function names and their schemas from ppm_search results.';
  LFn.OnAction := OnCallMcpTool;

  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name            := 'tool_name';
  LParam.Description.Text := 'Name of the installed MCP tool (e.g.: mcp-github)';
  LParam.ParamType        := ptString;
  LParam.Required         := True;

  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name            := 'function_name';
  LParam.Description.Text := 'Name of the function to call on the tool (from its schema)';
  LParam.ParamType        := ptString;
  LParam.Required         := True;

  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name            := 'arguments';
  LParam.Description.Text := 'JSON string with the function arguments (e.g.: {"query":"pascal","limit":10})';
  LParam.ParamType        := ptString;
  LParam.Required         := True;
end;

procedure TfrmPPMAutoTools.InitSystemPrompt;
begin
  AiConn.SystemPrompt.Text := SYSTEM_PROMPT;
end;

// ---------------------------------------------------------------------------
// Handlers de funciones PPM
// ---------------------------------------------------------------------------

procedure TfrmPPMAutoTools.OnPpmSearch(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  LQuery: string;
  LResult: TJSONObject;
  LTools: TJSONArray;
  LTool: TJSONObject;
  LSb: TStringBuilder;
  I: Integer;
begin
  var LArgsS := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(LArgsS) then
  try
    LArgsS.TryGetValue<string>('query', LQuery);
    LQuery := Trim(LQuery);
  finally
    LArgsS.Free;
  end;
  AddLog('ppm_search: "' + LQuery + '"...');
  SetStatus('Buscando "' + LQuery + '" en el registry PPM...');

  LResult := AiFunctions1.SearchPPMMCP(LQuery);
  if not Assigned(LResult) then
  begin
    ToolCall.Response := 'No se pudo conectar con el registry PPM. Verifica la conexión.';
    Handled := True;
    Exit;
  end;

  try
    // Formatear resultados de forma compacta para el LLM
    LSb := TStringBuilder.Create;
    try
      if LResult.TryGetValue<TJSONArray>('tools', LTools) and (LTools.Count > 0) then
      begin
        LSb.AppendLine(Format('Se encontraron %d herramientas para "%s":', [LTools.Count, LQuery]));
        LSb.AppendLine;
        for I := 0 to LTools.Count - 1 do
        begin
          LTool := LTools.Items[I] as TJSONObject;
          var LName, LDesc, LVer: string;
          LTool.TryGetValue<string>('name',        LName);
          LTool.TryGetValue<string>('description', LDesc);
          LTool.TryGetValue<string>('version',     LVer);

          LSb.AppendLine(Format('• %s (v%s)', [LName, LVer]));
          LSb.AppendLine('  ' + LDesc);

          // Incluir las operaciones disponibles desde el schema si están presentes
          var LSchema: TJSONObject;
          if LTool.TryGetValue<TJSONObject>('schema', LSchema) then
          begin
            var LInput: TJSONObject;
            if LSchema.TryGetValue<TJSONObject>('inputSchema', LInput) then
            begin
              var LProps: TJSONObject;
              if LInput.TryGetValue<TJSONObject>('properties', LProps) then
              begin
                // Extraer operaciones del enum si existe
                var LOpEnum: TJSONArray;
                var LOp: TJSONObject;
                if LProps.TryGetValue<TJSONObject>('operation', LOp) and
                   LOp.TryGetValue<TJSONArray>('enum', LOpEnum) then
                begin
                  var LOps := TStringList.Create;
                  try
                    for var J := 0 to LOpEnum.Count - 1 do
                      LOps.Add(LOpEnum.Items[J].Value);
                    LSb.AppendLine('  Operaciones: ' + LOps.CommaText);
                  finally
                    LOps.Free;
                  end;
                end;
              end;
            end;
          end;
          LSb.AppendLine;
        end;
        LSb.AppendLine('Para usar una herramienta: primero llama ppm_install("<nombre>"), luego call_mcp_tool.');
      end
      else
        LSb.AppendLine(Format('No se encontraron herramientas para "%s" en el registry PPM.', [LQuery]));

      ToolCall.Response := LSb.ToString;
    finally
      LSb.Free;
    end;
  finally
    LResult.Free;
  end;

  AddLog('ppm_search: OK (' + LQuery + ')');
  Handled := True;
end;

procedure TfrmPPMAutoTools.OnPpmInstall(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  LToolName: string;
  LItem: TMCPClientItem;
begin
  var LArgsI := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(LArgsI) then
  try
    LArgsI.TryGetValue<string>('tool_name', LToolName);
    LToolName := Trim(LToolName);
  finally
    LArgsI.Free;
  end;
  AddLog('ppm_install: "' + LToolName + '"...');

  // Si el cliente ya está registrado, reutilizarlo sin volver a descargar
  LItem := AiFunctions1.MCPClients.GetClientByName(LToolName);
  if Assigned(LItem) then
  begin
    AddLog('ppm_install: "' + LToolName + '" ya instalado, reutilizando.');
    SetStatus('"' + LToolName + '" ya disponible.');
  end
  else
  begin
    SetStatus('Instalando paquete "' + LToolName + '" desde PPM...');
    LItem := AiFunctions1.InstallMCPFromPPM(LToolName);
  end;

  if Assigned(LItem) then
  begin
    // Inicializar el cliente MCP (arranca el proceso e interroga tools/list)
    AddLog('ppm_install: inicializando proceso "' + LToolName + '"...');
    if LItem.MCPClient.Initialize then
    begin
      // Desactivar el cliente para que GetTools no exponga sus herramientas
      // directamente al LLM. Así el LLM solo puede acceder a ellas a través
      // de call_mcp_tool (nuestro handler), evitando llamadas paralelas
      // no controladas que causarían race conditions en TMCPClientStdIo.
      LItem.Enabled := False;

      // Extraer los nombres reales de funciones del tools/list para informar al LLM
      var LFuncNames := TStringList.Create;
      try
        var LToolsJson := TJSONObject.ParseJSONValue(LItem.MCPClient.Tools.Text);
        if Assigned(LToolsJson) then
        try
          var LToolsArr: TJSONArray;
          if LToolsJson.TryGetValue<TJSONArray>('tools', LToolsArr) then
            for var LT in LToolsArr do
            begin
              var LFName: string;
              if (LT is TJSONObject) and TJSONObject(LT).TryGetValue<string>('name', LFName) then
                LFuncNames.Add(LFName);
            end;
        finally
          LToolsJson.Free;
        end;
      except
        // si falla el parse, continuamos sin lista de funciones
      end;

      var LFuncList := '';
      if LFuncNames.Count > 0 then
        LFuncList := sLineBreak + 'Funciones disponibles: ' + LFuncNames.CommaText
      else
        LFuncList := '';

      LFuncNames.Free;

      ToolCall.Response := Format(
        'Herramienta "%s" instalada e inicializada correctamente.' + sLineBreak +
        'Ejecutable: %s' + sLineBreak +
        'Ya puedes llamarla con call_mcp_tool.%s',
        [LToolName, LItem.Params.Values['Command'], LFuncList]);

      AddInstalledTool(LToolName, LItem.Params.Values['Command']);
      AddLog('ppm_install: OK → ' + LItem.Params.Values['Command']);
      SaveInstalledTools;
      UpdateSystemPromptWithInstalledTools;
    end
    else
    begin
      ToolCall.Response := Format(
        'Herramienta "%s" instalada pero no se pudo inicializar el proceso. ' +
        'Verifica que el ejecutable exista y sea compatible.', [LToolName]);
      AddLog('ppm_install: WARN - proceso no iniciado para ' + LToolName);
    end;
  end
  else
  begin
    ToolCall.Response := Format(
      'No se pudo instalar "%s". Verifica el nombre del paquete y la conexión.', [LToolName]);
    AddLog('ppm_install: ERROR - ' + LToolName);
  end;

  Handled := True;
end;

procedure TfrmPPMAutoTools.OnCallMcpTool(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  LToolName, LFuncName, LArgStr: string;
  LClientItem: TMCPClientItem;
  LArgsJson: TJSONObject;
  LResult: TJSONObject;
  LMedia: TObjectList<TAiMediaFile>;
begin
  // Sección crítica global: serializa llamadas concurrentes a cualquier MCP client
  // (TMCPClientStdIo no es thread-safe — dos llamadas paralelas al mismo cliente
  //  producen race conditions → EInvalidPointer)
  FMCPLock.Enter;
  try

  var LArgsC := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(LArgsC) then
  try
    LArgsC.TryGetValue<string>('tool_name',     LToolName);
    LArgsC.TryGetValue<string>('function_name', LFuncName);
    LArgsC.TryGetValue<string>('arguments',     LArgStr);
    LToolName := Trim(LToolName);
    LFuncName := Trim(LFuncName);
    LArgStr   := Trim(LArgStr);
  finally
    LArgsC.Free;
  end;

  AddLog(Format('call_mcp_tool: %s → %s', [LToolName, LFuncName]));
  SetStatus(Format('Ejecutando %s.%s...', [LToolName, LFuncName]));

  // Buscar el cliente instalado
  LClientItem := AiFunctions1.MCPClients.GetClientByName(LToolName);
  if not Assigned(LClientItem) then
  begin
    ToolCall.Response := Format(
      'La herramienta "%s" no está instalada. Usa ppm_install primero.', [LToolName]);
    Handled := True;
    Exit;
  end;

  // Conectar log del cliente MCP al log de la demo para ver errores del servidor
  LClientItem.MCPClient.OnLog := AiFunctions1Log;
  LClientItem.MCPClient.OnStatusUpdate := AiFunctions1StatusUpdate;

  // Parsear argumentos antes de verificar connection_string
  LArgsJson := nil;
  if (LArgStr <> '') and (LArgStr <> '{}') then
  begin
    LArgsJson := TJSONObject.ParseJSONValue(LArgStr) as TJSONObject;
    if not Assigned(LArgsJson) then
    begin
      ToolCall.Response := Format('Argumentos JSON inválidos para "%s": %s', [LFuncName, LArgStr]);
      Handled := True;
      Exit;
    end;
  end
  else
    LArgsJson := TJSONObject.Create;

  // Si el LLM pasa connection_string, usarlo como argumento de startup del proceso.
  // mcp-postgres (y muchos otros MCP tools) necesitan la conexión como argumento
  // posicional al iniciar el proceso (ej: mcp-postgres.exe postgresql://user:pass@host/db).
  var LConnStr: string;
  var LConnStrUpdated := False;
  if LArgsJson.TryGetValue<string>('connection_string', LConnStr) and (LConnStr <> '') then
  begin
    LClientItem.MCPClient.Params.Values['Arguments'] := LConnStr;
    LArgsJson.RemovePair('connection_string');
    LConnStrUpdated := True;
    AddLog('call_mcp_tool: connection_string → startup arg: ' + LConnStr);
  end;

  // Inicializar si aún no fue hecho, O re-inicializar si se actualizó la connection_string.
  // Re-inicializar es necesario porque ppm_install arrancó el servidor sin connection_string,
  // por lo que el tools/list previo puede estar vacío o incompleto.
  if not LClientItem.MCPClient.Initialized or LConnStrUpdated then
  begin
    AddLog('call_mcp_tool: inicializando ' + LToolName + '...');
    LClientItem.MCPClient.Initialized := False;
    if not LClientItem.MCPClient.Initialize then
    begin
      ToolCall.Response := Format(
        'No se pudo inicializar "%s". ' +
        'Verifica que el ejecutable exista y soporte --protocol stdio.', [LToolName]);
      Handled := True;
      Exit;
    end;
  end;

  LMedia := TObjectList<TAiMediaFile>.Create(True);
  try
    try
      AddLog('call_mcp_tool: iniciando CallTool...');
      // NOTA: CallTool toma ownership de LArgsJson y lo libera internamente.
      // NO llamar LArgsJson.Free después de esta llamada.
      LResult := LClientItem.MCPClient.CallTool(LFuncName, LArgsJson, LMedia);
      LArgsJson := nil; // Ya fue liberado por CallTool
      AddLog('call_mcp_tool: CallTool completado. Result=' + BoolToStr(Assigned(LResult), True));
      try
        if Assigned(LResult) then
          ToolCall.Response := LResult.Format
        else
        begin
          // Incluir funciones reales disponibles para ayudar al LLM a corregir el nombre
          var LAvailFuncs := '';
          var LToolsJson := TJSONObject.ParseJSONValue(LClientItem.MCPClient.Tools.Text);
          if Assigned(LToolsJson) then
          try
            var LToolsArr: TJSONArray;
            if LToolsJson.TryGetValue<TJSONArray>('tools', LToolsArr) then
            begin
              var LSb := TStringList.Create;
              try
                for var LT in LToolsArr do
                begin
                  var LFN: string;
                  if (LT is TJSONObject) and TJSONObject(LT).TryGetValue<string>('name', LFN) then
                    LSb.Add(LFN);
                end;
                if LSb.Count > 0 then
                  LAvailFuncs := ' Funciones disponibles en ' + LToolName + ': ' + LSb.CommaText + '.';
              finally
                LSb.Free;
              end;
            end;
          finally
            LToolsJson.Free;
          end;
          ToolCall.Response := Format(
            'Función "%s" no encontrada en "%s".%s',
            [LFuncName, LToolName, LAvailFuncs]);
        end;
      finally
        LResult.Free;
      end;
    except
      on E: Exception do
      begin
        AddLog('EXCEPCION en CallTool [' + E.ClassName + ']: ' + E.Message);
        ToolCall.Response := Format('Error ejecutando "%s": %s — %s', [LFuncName, E.ClassName, E.Message]);
        LArgsJson := nil; // ya liberado o no — evitar doble-free
      end;
    end;
  finally
    LMedia.Free;
  end;

  AddLog('call_mcp_tool: OK → ' + LToolName + '.' + LFuncName);
  Handled := True;

  finally
    FMCPLock.Leave;
  end;
end;

// ---------------------------------------------------------------------------
// Chat handlers
// ---------------------------------------------------------------------------

procedure TfrmPPMAutoTools.ChatInput1SendEvent(Sender: TObject; APrompt: string;
  aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
var
  LFiles: TAiMediaFilesArray;
begin
  // Configurar parámetros del chat antes de cada llamada
  AiConn.Params.Values['Asynchronous'] := BoolToStr(ChAsync.IsChecked, True);
  AiConn.Params.Values['Tool_Active']  := 'True';

  FLastBubble := nil;
  ChatList1.AddBubble(APrompt, 'Usuario', aMediaFiles, False);

  // Clonar MediaFiles en el hilo principal ANTES de TTask.Run.
  // DoSendEvent (TChatInput) libera aMediaFiles al retornar este handler;
  // si lo capturamos en el closure, el hilo en background accedería a un
  // objeto ya liberado → EInvalidPointer.
  LFiles := aMediaFiles.ToMediaFileArray;

  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;

  TTask.Run(
    procedure
    begin
      try
        AiConn.AddMessageAndRun(APrompt, 'user', LFiles);
      except
        on E: Exception do
          AddLog('ERROR chat: ' + E.Message);
      end;
    end);
end;

procedure TfrmPPMAutoTools.ChatInput1Cancel(Sender: TObject);
begin
  AiConn.Abort;
  TThread.Queue(nil,
    procedure
    begin
      ChatInput1.Busy := False;
      AniIndicator1.Visible := False;
      AniIndicator1.Enabled := False;
    end);
end;

procedure TfrmPPMAutoTools.AiConnReceiveData(const Sender: TObject;
  aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  if not ChAsync.IsChecked then Exit;
  if aText = '' then Exit;

  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FLastBubble) then
        FLastBubble.AppendText(aText)
      else
        FLastBubble := ChatList1.AddBubble(aText, aRole, nil);
      ChatList1.RecalcSize;
    end);
end;

procedure TfrmPPMAutoTools.AiConnReceiveDataEnd(const Sender: TObject;
  aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      ChatInput1.Busy := False;
      AniIndicator1.Visible := False;
      AniIndicator1.Enabled := False;

      if not ChAsync.IsChecked then
        FLastBubble := ChatList1.AddBubble(aText, aRole, aMsg.MediaFiles);

      ChatList1.RecalcSize;
      FLastBubble := nil;
      LblStatus.Text := '';
    end);
end;

procedure TfrmPPMAutoTools.AiConnError(Sender: TObject; const ErrorMsg: string;
  Exception: Exception; const aResponse: IHTTPResponse);
begin
  TThread.Queue(nil,
    procedure
    begin
      ChatInput1.Busy := False;
      AniIndicator1.Visible := False;
      AniIndicator1.Enabled := False;
      LblStatus.Text := '';
      var LExClass := '';
      if Assigned(Exception) then LExClass := '[' + Exception.ClassName + '] ';
      AddLog('ERROR ' + LExClass + ': ' + ErrorMsg);
      ChatList1.AddBubble('⚠ ' + LExClass + ErrorMsg, 'Sistema', nil);
    end);
end;

// ---------------------------------------------------------------------------
// Driver/Model combo
// ---------------------------------------------------------------------------

procedure TfrmPPMAutoTools.ComboDriverChange(Sender: TObject);
const
  // Drivers que no necesitan API key
  NO_KEY_DRIVERS: array[0..2] of string = ('Ollama', 'LMStudio', 'GenericLLM');
var
  LDriverName, LEnvKey, LAutoKey: string;
  LModels: TStringList;
  I: Integer;
  LNeedsKey: Boolean;
begin
  if not Assigned(ComboDriver.Selected) then Exit;

  LDriverName := Trim(ComboDriver.Selected.Text);
  AiConn.DriverName := LDriverName;

  // Auto-detectar variable de entorno del API key
  LNeedsKey := True;
  for I := Low(NO_KEY_DRIVERS) to High(NO_KEY_DRIVERS) do
    if SameText(LDriverName, NO_KEY_DRIVERS[I]) then
    begin
      LNeedsKey := False;
      Break;
    end;

  if LNeedsKey then
  begin
    LEnvKey := UpperCase(LDriverName) + '_API_KEY';
    LAutoKey := GetEnvironmentVariable(LEnvKey);
    if LAutoKey <> '' then
    begin
      AiConn.Params.Values['ApiKey'] := LAutoKey;
      EditApiKey.Text := '(auto: ' + LEnvKey + ')';
    end
    else if Copy(EditApiKey.Text, 1, 5) = '(auto' then
      EditApiKey.Text := '';  // limpiar placeholder de otro driver
  end
  else
  begin
    AiConn.Params.Values['ApiKey'] := '';
    EditApiKey.Text := '(no requerido)';
  end;

  LModels := AiConn.GetModels;
  try
    LModels.Sort;
    ComboModel.Items.Assign(LModels);
    if ComboModel.Items.Count > 0 then
      ComboModel.ItemIndex := 0;
  finally
    LModels.Free;
  end;

  // Sincronizar checkbox async con parámetro del driver
  ChAsync.IsChecked := StrToBoolDef(AiConn.Params.Values['Asynchronous'], True);
end;

procedure TfrmPPMAutoTools.EditApiKeyChange(Sender: TObject);
var
  LKey: string;
begin
  LKey := EditApiKey.Text;
  // Ignorar los placeholders informativos
  if Copy(LKey, 1, 1) = '(' then Exit;
  AiConn.Params.Values['ApiKey'] := LKey;
end;

procedure TfrmPPMAutoTools.ComboModelChange(Sender: TObject);
begin
  if Assigned(ComboModel.Selected) then
    AiConn.Model := Trim(ComboModel.Selected.Text);
end;

// ---------------------------------------------------------------------------
// Helpers de log e UI
// ---------------------------------------------------------------------------

procedure TfrmPPMAutoTools.SetStatus(const AMsg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      LblStatus.Text := AMsg;
    end);
end;

procedure TfrmPPMAutoTools.AiConnStateChange(Sender: TObject; State: TAiChatState;
  const Description: string);
const
  STATE_ICONS: array[TAiChatState] of string = (
    '',                          // acsIdle
    'Conectando...',             // acsConnecting
    '',                          // acsCreated
    'Razonando...',              // acsReasoning
    'Generando respuesta...',    // acsWriting
    'Llamando herramienta...',   // acsToolCalling
    'Ejecutando herramienta...', // acsToolExecuting
    '',                          // acsFinished
    '',                          // acsAborted
    'Cargando...',               // acsLoading
    'Procesando...',             // acsProcessing
    ''                           // acsError
  );
begin
  SetStatus(STATE_ICONS[State]);
end;

procedure TfrmPPMAutoTools.AiFunctions1Log(Sender: TObject; const Msg: string);
begin
  // Filtrar mensajes de bajo nivel (muy verbosos) para no saturar el status
  if (Pos('DEBUG', Msg) > 0) or (Pos('Waiting for', Msg) > 0) or
     (Pos('TIMEOUT', Msg) > 0) then
    Exit;
  SetStatus(Msg);
  AddLog(Msg);
end;

procedure TfrmPPMAutoTools.AiFunctions1StatusUpdate(Sender: TObject; const StatusMsg: string);
begin
  SetStatus(StatusMsg);
end;

procedure TfrmPPMAutoTools.AddLog(const AMsg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      MemoLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + AMsg);
      MemoLog.GoToTextEnd;
    end);
end;

procedure TfrmPPMAutoTools.AddInstalledTool(const AName, AExePath: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      MemoInstalados.Lines.Add('✓ ' + AName);
    end);
end;

end.
