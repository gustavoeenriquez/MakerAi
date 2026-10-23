program A2AChatAgentTool;

// =============================================================================
// DEMO 093 - Un chat que consulta a un agente A2A como herramienta
// =============================================================================
// Un LLM no tiene por que saberlo todo: puede preguntarle a otro agente.
//
//   1. AGENTE  : un grafo TAIAgentManager con el catalogo de Cafe La Ladera
//                (precios, notas de cata, existencias), publicado como agente
//                A2A 1.0 con TAiA2AServer. Es determinista: no usa LLM.
//   2. CHAT    : un TAiChatConnection cuyo TAiFunctions incluye un
//                TAiA2AAgentTool. El modelo ve la herramienta 'catalogo_cafe'
//                (con la descripcion que el agente publica en su Agent Card) y
//                decide cuando llamarla.
//
// Es el hermano de TAiA2ARemoteAgentTool (demo 072/074): aquel federa un NODO
// de grafo; este, una CONVERSACION.
//
// Uso:
//   A2AChatAgentTool.exe                               OpenAI (@OPENAI_API_KEY)
//   A2AChatAgentTool.exe --driver Claude               cualquier driver registrado
//   A2AChatAgentTool.exe --driver Ollama --model gemma4
//   A2AChatAgentTool.exe --sin-llm                     llama la herramienta directo
//   A2AChatAgentTool.exe "pregunta libre"              en lugar de las de ejemplo
//   --port 8293                                        puerto del agente A2A
//
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.SyncObjs,
  uMakerAi.Agents,
  uMakerAi.A2A.Server,
  uMakerAi.A2A.Client,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;   // registra todos los drivers

type
  TProducto = record
    Nombre, Claves, Notas: string;
    Precio, Existencias: Integer;
  end;

const
  CATALOGO: array [0 .. 3] of TProducto = (
    (Nombre: 'Huila Reserva'; Claves: 'huila,reserva,frutal,panela,rojos';
     Notas: 'panela, frutos rojos, acidez brillante'; Precio: 38000; Existencias: 24),
    (Nombre: 'Nariño Altura'; Claves: 'nariño,narino,altura,citrico,caramelo';
     Notas: 'citricos, caramelo, cuerpo medio'; Precio: 42000; Existencias: 10),
    (Nombre: 'Sierra Nevada Orgánico'; Claves: 'sierra,nevada,organico,orgánico,chocolate,nuez';
     Notas: 'chocolate amargo, nuez, cuerpo alto'; Precio: 36000; Existencias: 0),
    (Nombre: 'Descafeinado Suave'; Claves: 'descafeinado,decaf,suave,noche,cacao';
     Notas: 'cacao suave, dulce, proceso de agua de montaña'; Precio: 34000; Existencias: 15));

  PREGUNTAS_EJEMPLO: array [0 .. 1] of string = (
    'Me gustan los cafés con notas a chocolate. ¿Cuál me recomiendas y cuánto cuesta?',
    '¿Tienen el Nariño Altura? Quiero 2 bolsas, ¿cuánto pagaría?');

type
  // Nodos del agente de catalogo
  TCatalogoHandlers = class
  public
    procedure BuscarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure FirmarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
  end;

var
  ConsoleLock: TCriticalSection;

procedure Log(const S: string);
begin
  ConsoleLock.Enter;
  try
    Writeln(S);
  finally
    ConsoleLock.Leave;
  end;
end;

function FormatoProducto(const P: TProducto): string;
begin
  Result := Format('%s: notas de %s; bolsa de 340 g a %s COP; ', [P.Nombre, P.Notas,
    FormatFloat('#,##0', P.Precio)]);
  if P.Existencias > 0 then
    Result := Result + Format('%d bolsas disponibles.', [P.Existencias])
  else
    Result := Result + 'AGOTADO.';
end;

procedure TCatalogoHandlers.BuscarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string;
  var Output: string);
var
  Consulta, Clave: string;
  Encontrados: TStringList;
  P: TProducto;
begin
  // Corre en un hilo del servidor A2A: la consola va con candado
  Log('    [agente A2A] consulta recibida: ' + Input);
  Consulta := LowerCase(Input);
  Encontrados := TStringList.Create;
  try
    for P in CATALOGO do
      for Clave in P.Claves.Split([',']) do
        if Consulta.Contains(Clave) then
        begin
          Encontrados.Add(FormatoProducto(P));
          Break;
        end;
    // Sin coincidencias: el catalogo completo, para que el modelo elija
    if Encontrados.Count = 0 then
      for P in CATALOGO do
        Encontrados.Add(FormatoProducto(P));
    Output := String.Join(' ', Encontrados.ToStringArray);
  finally
    Encontrados.Free;
  end;
end;

procedure TCatalogoHandlers.FirmarExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string;
  var Output: string);
begin
  Output := Input + ' (Fuente: catálogo de Café La Ladera; precios con IVA, envío no incluido.)';
end;

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

function HasFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
      Exit(True);
end;

// Pregunta libre: el primer argumento que no es opcion ni valor de opcion
function PreguntaLibre: string;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= ParamCount do
  begin
    if ParamStr(I).StartsWith('--') then
    begin
      if not SameText(ParamStr(I), '--sin-llm') then
        Inc(I); // salta el valor de la opcion
    end
    else
      Exit(ParamStr(I));
    Inc(I);
  end;
end;

function ApiKeyFor(const ADriver: string): string;
begin
  if SameText(ADriver, 'Ollama') or SameText(ADriver, 'LMStudio') or SameText(ADriver, 'GenericLLM') then
    Result := ''
  else if SameText(ADriver, 'OpenAi') then
    Result := '@OPENAI_API_KEY'
  else
    Result := '@' + UpperCase(ADriver) + '_API_KEY';
end;

procedure PreguntarSinLLM(AFuncs: TAiFunctions; const APregunta: string);
var
  ToolCall: TAiToolsFunction;
begin
  ToolCall := TAiToolsFunction.Create;
  try
    ToolCall.Name := 'catalogo_cafe';
    ToolCall.Arguments := '{"query":"' + APregunta.Replace('"', '''') + '"}';
    AFuncs.DoCallFunction(ToolCall);
    Log('  Respuesta del agente: ' + ToolCall.Response);
  finally
    ToolCall.Free;
  end;
end;

var
  Handlers: TCatalogoHandlers;
  Agente: TAIAgentManager;
  Server: TAiA2AServer;
  Funcs: TAiFunctions;
  AgentTool: TAiA2AAgentTool;
  Conn: TAiChatConnection;
  Port: Integer;
  Driver, Model, Key, Url, Respuesta: string;
  SinLLM: Boolean;
  Preguntas: TArray<string>;
  Pregunta: string;
var
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  // Tildes y eñes en la consola
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes y
  // el streaming se veria de golpe. Con un buffer de 1 byte cada caracter sale al
  // escribirlo (en una consola real ya se vaciaba en cada Write).
  SetTextBuf(Output, BufSalida, 1);
  ConsoleLock := TCriticalSection.Create;
  Handlers := TCatalogoHandlers.Create;
  Agente := TAIAgentManager.Create(nil);
  Server := TAiA2AServer.Create(nil);
  Funcs := TAiFunctions.Create(nil);
  AgentTool := TAiA2AAgentTool.Create(nil);
  Conn := TAiChatConnection.Create(nil);
  try
    try
      Port := StrToIntDef(ArgValue('--port', '8293'), 8293);
      Url := Format('http://localhost:%d', [Port]);

      // --- 1. El agente de catalogo, publicado por A2A ---------------------
      Agente.Name := 'AgenteCatalogo';
      Agente.AddNode('Buscar', Handlers.BuscarExec).AddNode('Firmar', Handlers.FirmarExec)
        .AddEdge('Buscar', 'Firmar')
        .SetEntryPoint('Buscar').SetFinishPoint('Firmar');

      Server.AgentManager := Agente;
      Server.AgentName := 'Agente de Catálogo de Café La Ladera';
      Server.AgentDescription := 'Precios, notas de cata y existencias de los cafés de Café La Ladera';
      Server.Skills.AddSkill('catalogo', 'Catálogo de productos',
        'Responde precios, notas de cata y existencias de cada café', 'cafe, precios, inventario');
      Server.Port := Port;
      Server.Active := True;
      Log('Agente A2A en ' + Url + '  (Agent Card: ' + Url + '/.well-known/agent-card.json)');

      // --- 2. La herramienta: el agente remoto visto desde el chat ---------
      AgentTool.AgentUrl := Url;
      AgentTool.ToolName := 'catalogo_cafe';
      AgentTool.Description := 'Consulta el catálogo de Café La Ladera. Úsala para precios, existencias o recomendaciones';
      AgentTool.Functions := Funcs;  // al asignarlo registra la funcion
      AgentTool.DiscoverFromCard;    // suma la descripcion que publica el agente
      Log('Herramienta para el modelo: ' + AgentTool.ToolName + ' - ' + AgentTool.Description);
      Log('');

      Pregunta := PreguntaLibre;
      if Pregunta <> '' then
        Preguntas := [Pregunta]
      else
        Preguntas := [PREGUNTAS_EJEMPLO[0], PREGUNTAS_EJEMPLO[1]];

      // --- 3. El chat --------------------------------------------------------
      Driver := ArgValue('--driver', 'OpenAi');
      Model := ArgValue('--model', '');
      Key := ApiKeyFor(Driver);
      SinLLM := HasFlag('--sin-llm');
      if (not SinLLM) and Key.StartsWith('@') and (GetEnvironmentVariable(Key.Substring(1)) = '') then
      begin
        Log('Falta la variable ' + Key.Substring(1) + ': se llama la herramienta directo, sin LLM.');
        Log('');
        SinLLM := True;
      end;

      if SinLLM then
      begin
        for Pregunta in Preguntas do
        begin
          Log('> ' + Pregunta);
          PreguntarSinLLM(Funcs, Pregunta);
          Log('');
        end;
        Exit;
      end;

      Conn.DriverName := Driver;
      if Model <> '' then
        Conn.Model := Model;
      if Key <> '' then
        Conn.Params.Values['ApiKey'] := Key;
      Conn.Params.Values['Asynchronous'] := 'False';
      Conn.SystemPrompt.Text :=
        'Eres Lucía, la asistente de atención al cliente de Café La Ladera. ' +
        'Para cualquier dato de productos, precios o existencias consulta la herramienta catalogo_cafe; ' +
        'no inventes precios. Responde en español, breve y amable, en texto plano sin Markdown: ' +
        'la respuesta sale en una consola.';
      Conn.AiFunctions := Funcs;
      Conn.Tool_Active := True;   // canal tipado: el driver envia las tools
      Log(Format('Chat: %s / %s', [Conn.DriverName, Conn.AiChat.Model]));
      Log('');

      for Pregunta in Preguntas do
      begin
        Log('> ' + Pregunta);
        Respuesta := Conn.AddMessageAndRun(Pregunta, 'user', []);
        if Respuesta = '' then
          Log('  (sin respuesta) ' + Conn.AiChat.LastError)
        else
          Log('  ' + Respuesta.Trim);
        Log('');
      end;
    except
      on E: Exception do
        Log(E.ClassName + ': ' + E.Message);
    end;
  finally
    Server.Active := False;
    Conn.Free;
    AgentTool.Free;
    Funcs.Free;
    Server.Free;
    Agente.Free;
    Handlers.Free;
    ConsoleLock.Free;
  end;
end.
