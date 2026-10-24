unit uLaderaApp;

// =============================================================================
// LIBRO "IA con Delphi" - La app de Cafe La Ladera, etapa del capitulo 23
// =============================================================================
// Observabilidad: un TAiTelemetry en el formulario (Enabled, ServiceName, Endpoint) y todo
// lo que ya hacia la app deja trazas OpenTelemetry: cada chat (modelo, tokens), cada
// herramienta, cada llamada MCP, cada grafo y cada nodo, cada busqueda RAG; anidadas en
// una sola traza por turno. Se exportan por OTLP/HTTP a localhost:4318 (Jaeger, Grafana
// Tempo, Langfuse, Phoenix...). OnLog muestra cada lote exportado en la barra de estado.
//
// Reglas: un TAiGuardrails en el formulario revisa CADA llamada a herramienta antes de
// ejecutarla, tanto las de Lucia (AiFunctions1.Guardrails) como las del agente de reclamos
// (Atencion.Guardrails). BlockedTools = anular_* (anular es cosa de personas) y
// OnCheckToolCall veta un descuento mayor al 15 % leyendo los argumentos. Si bloquea, la
// herramienta no corre y el modelo recibe el motivo; OnBlocked lo muestra como paso.
// Los evals de las respuestas de Lucia estan en el demo de consola 104.
//
// Aprobacion: un pedido por encima del tope no se despacha sin que lo apruebe el encargado.
// El flujo pasa por Revisar -> Aprobar (TAiWaitApprovalTool: suspende el grafo) -> Decidir
// -> Despachar o Rechazar. La pregunta aparece en el chat (OnSuspend), el estado se guarda en
// disco (TAiFileCheckpointer) y sobrevive al cierre de la app; la siguiente respuesta en el
// chat reanuda el grafo donde quedo (ResumeThread), incluso en otra sesion.
//
// El flujo: el pedido recorre un grafo de diez nodos armado ENTERO en el formulario
// (TAIAgentsNode con su OnExecute, TAIAgentsLink con NextA..C, jmAll, lmExpression con
// NextNo como camino por defecto, y un TLLMNode que redacta al final). Lucia lo dispara con
// la herramienta procesar_pedido(items, ciudad); cada nodo se ve como un paso.
//
// El primer agente: un reclamo no lo contesta el chat, lo delega en un grafo de dos nodos
// que esta en el formulario: Atencion (TLLMNode con el skill respuesta-reclamo y las
// herramientas consultar_pedido / programar_cambio) y Revisor (TLLMNode sin herramientas,
// que aprueba o corrige). Lucia lo llama con la herramienta atender_reclamo y entrega la
// respuesta aprobada. Cada nodo y cada herramienta del agente se ve como un paso.
//
// Habilidades: la carpeta skills\ trae tres SKILL.md de la tienda (cotizacion mayorista con
// su plantilla, respuesta a reclamos, ficha de cata). TAiSkills los ofrece BAJO DEMANDA: el
// modelo ve solo el catalogo (una linea por skill) y carga el que necesita con use_skill; los
// archivos de apoyo los lee con read_skill_file. Una pregunta que no es de ningun skill no
// carga nada. Cada carga se muestra como un paso en la burbuja.
//
// Conectores MCP: la bodega es OTRO programa (BodegaLadera.exe --servidor, un servidor MCP
// por stdio con inventario en kilos y precio mayorista). El item de AiFunctions1.MCPClients
// lo lanza como proceso hijo y sus herramientas llegan al modelo junto con las locales, con
// el prefijo del servidor (bodega_99_inventario). OnLog muestra cada tools/call en la burbuja.
//
// Memoria: el historial dura lo que dura la conversacion; TAiMemory sobrevive. Cada cliente
// tiene su Namespace (el cuadro "Cliente"), la memoria vive en SQLite (memoria.db) y en cada
// turno el chat (PersistentMemory + MemoryTokenBudget) busca lo relevante y lo inyecta antes
// del prompt. Al cerrar la sesion (boton Nueva), un modelo barato (Analizador, gpt-6-luna)
// lee la conversacion y decide que vale la pena recordar (AnalyzeNow).
//
// La otra cara del conocimiento: un grafo (TAiRagGraph) con las tripletas que el modelo
// extrajo de la carta y de lo que conto una clienta (grafo\tripletas.json, armado con
// TAiRagGraphBuilder). Dos herramientas lo consultan: que_sabemos_de (busqueda semantica
// + vecinos a dos saltos) y cafes_por_acidez (una consulta MakerGQL con parametro).
//
// Conocimiento: el manual de la tienda (envios, molienda, club, devoluciones, tostion,
// atencion) vive en una base vectorial sobre SQLite (TAiRAGVector + TAiRAGVectorSQLiteDriver,
// embeddings de OpenAI). Se indexa una sola vez, por secciones y con metadatos. El modelo lo
// consulta con la herramienta buscar_en_el_manual: recuperar (coseno) + reordenar con Jev
// (TAiJevRAGReranker), que ademas descarta un pasaje con inyeccion de prompt.
// Sigue todo lo anterior: funciones de la tienda, web, editor y consola con sus limites,
// sentidos y filtro.
//
//   ModelCaps   : lo que el modelo entiende por si mismo (gpt-6-sol: imagen y PDF).
//   SessionCaps : lo que la sesion pide (ademas, audio).
//   El hueco (SessionCaps - ModelCaps) lo cubre el framework con un puente
//   ANTES de llamar al modelo: aqui, ChatTools.SpeechTool transcribe la nota
//   con gpt-transcribe y el modelo recibe el texto.
//
// El filtro del capitulo 8 sigue activo. La llamada corre en un TTask porque
// el puente es sincrono y sin el hilo la ventana se congelaria al transcribir.
//
// Prueba automatica (para el libro, sin tocar el teclado):
//   LaderaApp.exe --prueba "pregunta" "@media\bolsa.png|que cafe es" "@media\nota.wav|" [--captura x.png] [--registro t.txt]
// Empresa, productos y precios ficticios.
// =============================================================================

interface

uses
  System.SysUtils, System.StrUtils, System.Variants, System.Types, System.UITypes, System.Classes, System.IOUtils,
  System.JSON, System.Threading, System.Diagnostics, System.Net.HttpClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.Controls.Presentation,
  // Primero la UI: AIChat.Types tambien declara un TAIChatMessage y Delphi no
  // distingue mayusculas; el de uMakerAi.Chat.Messages, que va despues, gana.
  AIChat.Types,
  AIChat.Control.FMX,
  AIChat.Input,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Sanitizer,
  uMakerAi.Chat.Tools,
  uMakerAi.Jev.PromptGuard,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,   // registra todos los drivers
  uMakerAi.OpenAI.Audio,
  uMakerAi.OpenAI.Audio.Tool,
  uMakerAi.Utils.VoiceMonitor,
  // El conocimiento (capitulo 13): embeddings, base vectorial y su driver SQLite (FireDAC embebido)
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.DApt, Data.DB, FireDAC.Comp.Client,
  uMakerAi.Embeddings.Core, uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Vectors, uMakerAi.RAG.Vectors.Index, uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vector.Driver.SQLite, uMakerAi.Jev.RAG,
  uMakerAi.RAG.Graph.Core, uMakerAi.RAG.Graph.Builder,
  uMakerAi.Memory.Types, uMakerAi.Memory,
  uMakerAi.Tools.Skills,
  uMakerAi.Agents, uMakerAi.Agents.Skill, uMakerAi.Agents.ToolRegistry, uMakerAi.Agents.Node.LLM,
  uMakerAi.Agents.Checkpoint, uMakerAi.Agents.Tools.Approval,
  uMakerAi.Guardrails,
  uMakerAi.Telemetry;

type
  TFormLadera = class(TForm)
    AiConn: TAiChatConnection;
    JevGuard: TAiJevPromptGuard;
    SpeechTool: TAiOpenAiSpeechTool;
    AIVoiceMonitor1: TAIVoiceMonitor;
    AiFunctions1: TAiFunctions;
    Shell: TAiShell;
    Editor: TAiTextEditorTool;
    Embeddings: TAiOpenAiEmbeddings;
    FDConn: TFDConnection;
    FDWait: TFDGUIxWaitCursor;
    RAGDriver: TAiRAGVectorSQLiteDriver;
    RAG: TAiRAGVector;
    Reranker: TAiJevRAGReranker;
    Grafo: TAiRagGraph;
    Builder: TAiRagGraphBuilder;
    Memoria: TAiMemory;
    Analizador: TAiChatConnection;
    Skills: TAiSkills;
    Reclamos: TAIAgentManager;
    Atencion: TLLMNode;
    LinkAtencion: TAIAgentsLink;
    Revisor: TLLMNode;
    FuncionesReclamo: TAiFunctions;
    Pedidos: TAIAgentManager;
    Recibir: TAIAgentsNode;
    LinkRecibir: TAIAgentsLink;
    Inventario: TAIAgentsNode;
    LinkInventario: TAIAgentsLink;
    Cotizar: TAIAgentsNode;
    LinkCotizar: TAIAgentsLink;
    Envio: TAIAgentsNode;
    LinkEnvio: TAIAgentsLink;
    Consolidar: TAIAgentsNode;
    LinkConsolidar: TAIAgentsLink;
    Avisar: TAIAgentsNode;
    LinkAvisar: TAIAgentsLink;
    Despachar: TAIAgentsNode;
    LinkDespachar: TAIAgentsLink;
    Mensajero: TAIAgentsNode;
    LinkMensajero: TAIAgentsLink;
    Transportadora: TAIAgentsNode;
    LinkTransportadora: TAIAgentsLink;
    Redactor: TLLMNode;
    Revisar: TAIAgentsNode;
    LinkRevisar: TAIAgentsLink;
    Aprobar: TAIAgentsNode;
    Aprobacion: TAiWaitApprovalTool;
    LinkAprobar: TAIAgentsLink;
    Decidir: TAIAgentsNode;
    LinkDecidir: TAIAgentsLink;
    Rechazar: TAIAgentsNode;
    LinkRechazar: TAIAgentsLink;
    Guardia: TAiGuardrails;
    Telemetria: TAiTelemetry;
    lblCliente: TLabel;
    edCliente: TEdit;
    LayoutTop: TLayout;
    ComboDriver: TComboBox;
    btnNueva: TButton;
    btnGuardar: TButton;
    btnAbrir: TButton;
    lblEstado: TLabel;
    ChatView1: TAIChatView;
    ChatInput1: TAIChatInput;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles;
      aAudioStream: TMemoryStream);
    procedure ChatInput1Cancel(Sender: TObject);
    procedure ChatInput1MicToggle(Sender: TObject; AActivate: Boolean);
    procedure ComboDriverChange(Sender: TObject);
    procedure btnNuevaClick(Sender: TObject);
    procedure btnGuardarClick(Sender: TObject);
    procedure btnAbrirClick(Sender: TObject);
    procedure AiConnReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception;
      const AResponse: IHTTPResponse);
    procedure AiConnSanitize(Sender: TObject; const AResult: TSanitizeResult; var AAction: TAiSanitizeAction);
    procedure AiConnPromptGuard(Sender: TObject; const AVerdict: TAiPromptVerdict; var AAction: TAiSanitizeAction);
    procedure FnConsultarInventario(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnCalcularTotal(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnBuscarManual(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnQueSabemosDe(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnCafesPorAcidez(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure AiFunctions1Log(Sender: TObject; const Msg: string);
    procedure SkillsSkillLoaded(Sender: TObject; ASkill: TAiSkillItem);
    procedure FnAtenderReclamo(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnConsultarPedido(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnProgramarCambio(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure AgenteEnterNode(Sender: TObject; Node: TAIAgentsNode);
    procedure FnProcesarPedido(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure RecibirExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure InventarioExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure CotizarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure EnvioExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure ConsolidarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure AvisarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure DespacharExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure MensajeroExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure TransportadoraExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure RevisarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure DecidirExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure RechazarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure PedidosSuspend(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
    procedure FnAplicarDescuento(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnAnularPedido(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure GuardiaCheckToolCall(Sender: TObject; const AToolName, AArguments: string; var AAllow: Boolean;
      var AReason: string);
    procedure GuardiaBlocked(Sender: TObject; const AToolName, AArguments, AReason: string);
    procedure TelemetriaLog(Sender: TObject; const AMsg: string);
    procedure PedidosFinish(Sender: TObject; const Input, Output: string; Status: TAgentExecutionStatus; E: Exception);
    procedure AgenteExitNode(Sender: TObject; Node: TAIAgentsNode);
    procedure SkillLeerArchivo(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure EditorBeforeCommand(Sender: TObject; const Command, Path: string; Args: TJSONObject;
      var Result: string; var Handled: Boolean);
    procedure ShellConsoleLog(Sender: TObject; const Command: string; const StdOut, StdErr: string; ExitCode: Integer);
    procedure ShellSecurityViolation(Sender: TObject; const Command: string; const CallId: string; var AErrorMessage: string);
  private
    FBurbuja: Integer;        // burbuja del asistente que se esta escribiendo
    FBurbujaUsuario: Integer; // burbuja del ultimo mensaje del usuario (para la transcripcion)
    FPensando: Boolean;
    FReloj: TStopwatch;
    FTarea: ITask;            // el turno en curso (corre fuera del hilo principal)
    FCerrando: Boolean;
    FBloqueo: string;
    FTextoRecibido: Boolean;  // llego algun pedazo por OnReceiveData en este turno
    FCarpetaTrabajo: string;
    FCliente: string;         // el cliente de este turno (lo lee el hilo de la peticion)
    FLlamadasMCP: TStringList; // id JSON-RPC -> llamada, para unir cada tools/call con su respuesta
    FHerramientasAgente: TAiToolRegistry; // las herramientas del agente Atencion
    FHiloPendiente: string;    // <> '' cuando el flujo de pedidos espera la respuesta del encargado
    FReanudando: Boolean;      // el turno en curso es una reanudacion, no una pregunta a Lucia
    procedure ConfigurarConexion(const ADriver: string);
    procedure TerminarPensando;
    procedure MostrarTranscripcion;
    procedure SetEstado(const AText: string);
    function CarpetaDatos: string;
    procedure IniciarPrueba;
    procedure PasoPrueba(Sender: TObject);
    procedure Registrar(const ALinea: string);
    procedure PasoHerramienta(const ATitulo, ADetalle: string);
    procedure PrepararConocimiento;
    procedure PrepararMemoria;
    procedure ConectarBodega;
    procedure CargarSkills;
    procedure PrepararAgente;
    procedure PrepararAprobacion;
    function IndexarManual: Integer;
  private
    FPrueba: TStringList;
    FCaptura, FRegistro: string;
    FTimerPrueba: TTimer;
    FFinTurno: Cardinal;
  end;

var
  FormLadera: TFormLadera;

implementation

{$R *.fmx}

function ApiKeyFor(const ADriver: string): string;
begin
  if MatchText(ADriver, ['Ollama', 'LMStudio', 'GenericLLM']) then
    Result := ''
  else if SameText(ADriver, 'OpenAi') then
    Result := '@OPENAI_API_KEY'
  else
    Result := '@' + UpperCase(ADriver) + '_API_KEY';
end;

{ Arranque }

procedure TFormLadera.FormCreate(Sender: TObject);
begin
  FBurbuja := -1;
  FBurbujaUsuario := -1;
  // La carpeta donde el editor y la consola pueden trabajar
  FCarpetaTrabajo := IncludeTrailingPathDelimiter(TPath.Combine(CarpetaDatos, 'trabajo'));
  ForceDirectories(FCarpetaTrabajo);
  SetCurrentDir(FCarpetaTrabajo);
  PrepararConocimiento;
  PrepararMemoria;
  FLlamadasMCP := TStringList.Create;
  ComboDriver.Items.CommaText := 'OpenAi,Claude,Groq,DeepSeek,Mistral,Ollama';
  ComboDriver.ItemIndex := 0;   // dispara ComboDriverChange -> ConfigurarConexion
  ChatView1.AddMessage(TChatRole.crAssistant,
    '¡Hola! Soy **Lucía**, de Café La Ladera ☕' + sLineBreak + sLineBreak +
    'Pregúntame por nuestros cafés, mándame una foto o un PDF, o háblame con el micrófono.');
  ConectarBodega;
  CargarSkills;
  PrepararAgente;
  PrepararAprobacion;
  IniciarPrueba;
end;

procedure TFormLadera.FormDestroy(Sender: TObject);
begin
  FPrueba.Free;
  FLlamadasMCP.Free;
  FHerramientasAgente.Free;
end;

procedure TFormLadera.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Cerrar a mitad de un turno: la tarea todavia usa AiConn (por ejemplo,
  // transcribiendo la nota de voz). Se desconectan los eventos, se aborta y se
  // espera a que termine; mientras, CheckSynchronize atiende lo ya encolado.
  FCerrando := True;
  AIVoiceMonitor1.Active := False;
  AiConn.OnReceiveThinking := nil;
  AiConn.OnReceiveData := nil;
  AiConn.OnReceiveDataEnd := nil;
  AiConn.OnError := nil;
  AiConn.Abort;
  if Assigned(FTarea) then
    while not FTarea.Wait(50) do
      CheckSynchronize;
  CheckSynchronize;
  CanClose := True;
end;

procedure TFormLadera.ConfigurarConexion(const ADriver: string);
var
  Key: string;
begin
  // Cambiar DriverName recrea el chat interno: el historial empieza de cero.
  AiConn.DriverName := ADriver;
  Key := ApiKeyFor(ADriver);
  if Key <> '' then
    AiConn.Params.Values['ApiKey'] := Key;

  // Lo que el modelo entiende por si mismo (ModelCaps) y lo que la sesion pide
  // (SessionCaps). Lo que falta lo resuelve el framework antes de llamar al
  // modelo: aqui, cap_Audio -> transcribir con ChatTools.SpeechTool.
  // Al fijar ModelCaps se fija el conjunto completo: vacio = el catalogo del modelo.
  if MatchText(ADriver, ['OpenAi', 'Claude']) then
    // leen imagenes y PDF, y traen busqueda web, editor y consola nativos
    AiConn.ModelCaps := [cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch, cap_TextEditor, cap_Shell]
  else
    AiConn.ModelCaps := [];                                    // los demas, aqui, solo texto
  AiConn.SessionCaps := AiConn.ModelCaps + [cap_Audio];        // la sesion ademas escucha

  if Key.StartsWith('@') and (GetEnvironmentVariable(Key.Substring(1)) = '') then
    SetEstado('Falta la variable ' + Key.Substring(1))
  else
    SetEstado(Format('%s / %s', [AiConn.DriverName, AiConn.AiChat.Model]));
end;

procedure TFormLadera.ComboDriverChange(Sender: TObject);
begin
  if ComboDriver.ItemIndex < 0 then
    Exit;
  ConfigurarConexion(ComboDriver.Items[ComboDriver.ItemIndex]);
  ChatView1.ClearMessages;
end;

{ Un turno: enviar (con adjuntos), recibir, terminar }

procedure TFormLadera.ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles;
  aAudioStream: TMemoryStream);
var
  Archivos: TAiMediaFilesArray;
  Nota: TAiMediaFile;
begin
  // aMediaFiles y aAudioStream se liberan al volver de este evento:
  // ToMediaFileArray devuelve copias, y el audio se copia en un TAiMediaFile.
  // Hay un pedido suspendido: este mensaje es la respuesta del encargado y reanuda el
  // grafo donde quedo (en esta sesion o en otra: el estado esta en el checkpoint)
  if FHiloPendiente <> '' then
  begin
    ChatView1.AddUserMessage(APrompt, aMediaFiles);
    ChatView1.BeginAssistantMessage;
    FBurbuja := ChatView1.MessageCount - 1;
    FReloj := TStopwatch.StartNew;
    FReanudando := True;
    Registrar('reanudar'#9 + FHiloPendiente + ' <- ' + APrompt);
    SetEstado('Reanudando el pedido...');
    if not Pedidos.ResumeThread(FHiloPendiente, 'Aprobar', APrompt) then
    begin
      ChatView1.SetMessageText(FBurbuja, '⚠️ No pude reanudar el pedido suspendido.');
      ChatView1.FinishMessage(FBurbuja);
      FBurbuja := -1;
      FReanudando := False;
      ChatInput1.Busy := False;
    end;
    FHiloPendiente := '';
    Exit;
  end;
  // La memoria es por cliente: el cuadro "Cliente" elige el espacio (Namespace)
  FCliente := edCliente.Text.Trim;
  Memoria.Namespace := 'cliente-' + FCliente.ToLower;
  Archivos := aMediaFiles.ToMediaFileArray;
  if Assigned(aAudioStream) and (aAudioStream.Size > 100) then
  begin
    Nota := TAiMediaFile.Create;
    aAudioStream.Position := 0;
    Nota.LoadFromStream('nota_de_voz.wav', aAudioStream);
    Archivos := Archivos + [Nota];
    if APrompt = '' then
      APrompt := '🎤 (nota de voz)';
  end;

  ChatView1.AddUserMessage(APrompt, aMediaFiles);   // la burbuja muestra los adjuntos
  FBurbujaUsuario := ChatView1.MessageCount - 1;
  ChatView1.BeginAssistantMessage;
  FBurbuja := ChatView1.MessageCount - 1;
  FPensando := False;
  FBloqueo := '';
  FTextoRecibido := False;
  FReloj := TStopwatch.StartNew;
  SetEstado('Lucía está escribiendo...');

  // El puente de audio (transcribir) corre antes de la llamada al modelo y es
  // sincrono: en un hilo aparte la ventana no se congela.
  FTarea := TTask.Run(
    procedure
    var
      Recuerdos: string;
    begin
      try
        // Lo que la memoria va a inyectar en este turno (el chat lo busca igual por su
        // cuenta; aqui se muestra en la burbuja para verlo)
        Recuerdos := Memoria.BuildContext(APrompt, AiConn.MemoryTokenBudget).Trim;
        if Recuerdos <> '' then
          PasoHerramienta('memoria de ' + FCliente, Recuerdos.Replace(sLineBreak, ' '));
        AiConn.AddMessageAndRun(IfThen(APrompt.StartsWith('🎤'), '', APrompt), 'user', Archivos);
      except
        on E: Exception do
        begin
          var Msg := E.Message;
          TThread.Queue(nil,
            procedure
            begin
              AiConnError(AiConn, Msg, nil, nil);
            end);
        end;
      end;
    end);
end;

procedure TFormLadera.AiConnReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando then
        Exit;
      if (FBurbuja >= 0) and not FPensando then
      begin
        FPensando := True;
        ChatView1.AddStep(FBurbuja, 'pensar', 'Pensando');
      end;
    end);
end;

procedure TFormLadera.TerminarPensando;
begin
  if FPensando then
  begin
    FPensando := False;
    ChatView1.FinishStep(FBurbuja, 'pensar', True, FReloj.ElapsedMilliseconds, '');
  end;
end;

procedure TFormLadera.AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
begin
  if aText = '' then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando or (FBurbuja < 0) then
        Exit;
      TerminarPensando;
      FTextoRecibido := True;
      ChatView1.AppendToken(FBurbuja, aText);
    end);
end;

procedure TFormLadera.AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
var
  Pie: string;
  Entrada: Integer;
begin
  if Assigned(aMsg) then
  begin
    Entrada := aMsg.Prompt_tokens + aMsg.Cached_tokens + aMsg.Cache_write_tokens;
    Pie := Format('%d tokens de entrada', [Entrada]);
    if aMsg.Cached_tokens > 0 then
      Pie := Pie + Format(' (%d en caché)', [aMsg.Cached_tokens]);
    Pie := Pie + Format(' · %d de salida · %.1f s', [aMsg.Completion_tokens, FReloj.ElapsedMilliseconds / 1000]);
  end
  else
    Pie := Format('%.1f s', [FReloj.ElapsedMilliseconds / 1000]);
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando or (FBurbuja < 0) then
        Exit;
      TerminarPensando;
      MostrarTranscripcion;
      // Tras llamar herramientas, la respuesta final puede llegar completa en aText
      // sin pasar por OnReceiveData: se pone entera si la burbuja sigue vacia.
      if (not FTextoRecibido) and (aText <> '') then
        ChatView1.SetMessageText(FBurbuja, aText);
      ChatView1.FinishMessage(FBurbuja);
      ChatView1.SetMessageFooter(FBurbuja, Pie);
      Registrar('turno'#9 + Pie);
      FBurbuja := -1;
      ChatInput1.Busy := False;
      SetEstado(Format('%s / %s', [AiConn.DriverName, AiConn.AiChat.Model]));
    end);
end;

procedure TFormLadera.MostrarTranscripcion;
var
  I: Integer;
  Msg: TAiChatMessage;
  Texto: string;
begin
  // Si el usuario mando una nota de voz, su burbuja muestra lo que se entendio
  for I := AiConn.Messages.Count - 1 downto 0 do
  begin
    Msg := AiConn.Messages[I];
    if Msg.Role <> 'user' then
      Continue;
    Texto := Msg.GetMediaTranscription;
    if (Texto <> '') and (FBurbujaUsuario >= 0) then
    begin
      ChatView1.SetMessageText(FBurbujaUsuario, '🎤 ' + Texto.Trim);
      Registrar('transcripcion'#9 + Texto.Trim);
    end;
    Break;
  end;
end;

procedure TFormLadera.AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception;
  const AResponse: IHTTPResponse);
var
  Texto: string;
begin
  Texto := IfThen(FBloqueo <> '', '🛡️ ' + FBloqueo, '⚠️ ' + ErrorMsg);
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando then
        Exit;
      if FBurbuja >= 0 then
      begin
        TerminarPensando;
        ChatView1.SetMessageText(FBurbuja, Texto);
        ChatView1.FinishMessage(FBurbuja);
        FBurbuja := -1;
      end;
      Registrar('error'#9 + Texto);
      ChatInput1.Busy := False;
      SetEstado(IfThen(FBloqueo <> '', 'Mensaje bloqueado', 'Error: ' + ErrorMsg));
    end);
end;

procedure TFormLadera.ChatInput1Cancel(Sender: TObject);
begin
  AiConn.Abort;
  if FBurbuja >= 0 then
  begin
    TerminarPensando;
    ChatView1.SetMessageFooter(FBurbuja, 'cancelado');
    ChatView1.FinishMessage(FBurbuja);
    FBurbuja := -1;
  end;
  ChatInput1.Busy := False;
  SetEstado('Cancelado');
end;

procedure TFormLadera.ChatInput1MicToggle(Sender: TObject; AActivate: Boolean);
begin
  // El boton del microfono solo cambia su dibujo: el que graba es el monitor.
  // Cuando el usuario calla, TAIChatInput manda el WAV por OnSendEvent.
  AIVoiceMonitor1.Active := AActivate;
end;

{ El filtro (capitulo 8) }

procedure TFormLadera.AiConnSanitize(Sender: TObject; const AResult: TSanitizeResult; var AAction: TAiSanitizeAction);
var
  M: TSuspiciousMatch;
  Nombres: string;
begin
  for M in AResult.SuspiciousMatches do
    Nombres := Nombres + M.PatternName + ' ';
  FBloqueo := 'Mensaje bloqueado por el sanitizador: fórmula conocida (' + Nombres.Trim + ').';
end;

procedure TFormLadera.AiConnPromptGuard(Sender: TObject; const AVerdict: TAiPromptVerdict; var AAction: TAiSanitizeAction);
begin
  FBloqueo := Format('Mensaje bloqueado por el guardián: `%s` (%.2f).', [AVerdict.Category, AVerdict.Score]);   // en backticks: la burbuja es Markdown
end;


{ Las herramientas de la tienda (capitulo 11). El modelo decide cuando llamarlas;
  cada OnAction lee los parametros, hace el trabajo y devuelve el resultado en
  ToolCall.Response. Llegan desde el hilo de la peticion: la UI se toca con Queue. }

function BolsasDe(const ACafe: string): Integer;
begin
  // El inventario de hoy (ficticio). En una app real: la base de datos.
  if ACafe.Contains('Huila') then Exit(24);
  if ACafe.Contains('Nari') then Exit(10);
  if ACafe.Contains('Sierra') then Exit(0);
  if ACafe.Contains('Descaf') then Exit(15);
  Result := -1;
end;

function PrecioDe(const ACafe: string): Integer;
begin
  if ACafe.Contains('Huila') then Exit(38000);
  if ACafe.Contains('Nari') then Exit(42000);
  if ACafe.Contains('Sierra') then Exit(36000);
  if ACafe.Contains('Descaf') then Exit(34000);
  Result := 0;
end;

procedure TFormLadera.PasoHerramienta(const ATitulo, ADetalle: string);
var
  Id: string;
begin
  // Un paso en la burbuja de Lucia: lo que hizo mientras preparaba la respuesta
  Id := 'tool' + IntToStr(Random(100000));
  Registrar('herramienta'#9 + ATitulo + ' -> ' + ADetalle);
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando or (FBurbuja < 0) then
        Exit;
      ChatView1.AddStep(FBurbuja, Id, ATitulo);
      ChatView1.FinishStep(FBurbuja, Id, True, 0, ADetalle);
    end);
end;

procedure TFormLadera.FnConsultarInventario(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Cafe: string;
  Bolsas: Integer;
begin
  Handled := True;
  Cafe := ToolCall.Params.Values['cafe'];
  Bolsas := BolsasDe(Cafe);
  if Bolsas < 0 then
    ToolCall.Response := 'No existe ese café en la carta.'
  else
    ToolCall.Response := Format('%s: %d bolsas de 340 g disponibles.', [Cafe, Bolsas]);
  PasoHerramienta('consultar_inventario(' + Cafe + ')', ToolCall.Response);
end;

procedure TFormLadera.FnCalcularTotal(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Cafe, Ciudad: string;
  Bolsas, Subtotal, Envio: Integer;
begin
  Handled := True;
  Cafe := ToolCall.Params.Values['cafe'];
  Bolsas := StrToIntDef(ToolCall.Params.Values['bolsas'], 0);
  Ciudad := ToolCall.Params.Values['ciudad'];
  Subtotal := PrecioDe(Cafe) * Bolsas;
  if SameText(Ciudad, 'Pitalito') then
    Envio := 5000
  else
    Envio := 12000;
  ToolCall.Response := Format('%d x %s = %d COP; envío a %s: %d COP; total: %d COP.',
    [Bolsas, Cafe, Subtotal, Ciudad, Envio, Subtotal + Envio]);
  PasoHerramienta(Format('calcular_total(%s, %d, %s)', [Cafe, Bolsas, Ciudad]), ToolCall.Response);
end;


{ Manos en el sistema (capitulo 12): cada herramienta con su limite }

procedure TFormLadera.EditorBeforeCommand(Sender: TObject; const Command, Path: string; Args: TJSONObject;
  var Result: string; var Handled: Boolean);
var
  Completa: string;
begin
  // Rutas relativas se resuelven contra la carpeta de trabajo; lo que termine fuera
  // (C:\Windows\..., ..\..\) se rechaza sin tocarlo. Y el editor no borra.
  if TPath.IsPathRooted(Path) then
    Completa := TPath.GetFullPath(Path)
  else
    Completa := TPath.GetFullPath(TPath.Combine(FCarpetaTrabajo, Path));
  if not Completa.StartsWith(FCarpetaTrabajo, True) then
  begin
    Result := 'Error: solo puedes trabajar dentro de la carpeta de trabajo.';
    Handled := True;
    PasoHerramienta('editor ' + Command + ' ' + Path, 'BLOQUEADO: fuera de la carpeta de trabajo');
  end
  else if SameText(Command, 'delete') then
  begin
    Result := 'Error: el editor no tiene permiso para borrar archivos.';
    Handled := True;
    PasoHerramienta('editor ' + Command + ' ' + Path, 'BLOQUEADO: el editor no borra');
  end
  else
    PasoHerramienta('editor ' + Command + ' ' + Path, 'ok');
end;

procedure TFormLadera.ShellConsoleLog(Sender: TObject; const Command: string; const StdOut, StdErr: string;
  ExitCode: Integer);
begin
  PasoHerramienta('consola: ' + Command, Format('salida de %d caracteres', [Length(StdOut)]));
end;

procedure TFormLadera.ShellSecurityViolation(Sender: TObject; const Command: string; const CallId: string;
  var AErrorMessage: string);
begin
  PasoHerramienta('consola: ' + Command, 'BLOQUEADO: no está en la lista permitida');
end;

{ El conocimiento (capitulo 13): el manual de la tienda en una base vectorial sobre
  SQLite. Se indexa una sola vez, por secciones y con metadatos; el modelo lo consulta
  con la herramienta buscar_en_el_manual y el reranker descarta lo que no responde
  (o trae una inyeccion). }

procedure TFormLadera.PrepararConocimiento;
var
  N: Integer;
begin
  // Los componentes estan en el formulario; aqui se conectan entre si.
  Embeddings.ApiKey := '@OPENAI_API_KEY';
  RAG.Embeddings := Embeddings;       // quien convierte texto en vectores
  RAG.Driver := RAGDriver;            // donde se guardan y se buscan (SQLite)
  FDConn.Params.Database := TPath.Combine(CarpetaDatos, 'conocimiento.db');
  FDConn.Connected := True;
  RAGDriver.CreateSchema('manual');   // CREATE TABLE IF NOT EXISTS: la segunda vez no hace nada
  N := FDConn.ExecSQLScalar('SELECT COUNT(*) FROM manual');
  if N = 0 then
    N := IndexarManual;               // solo la primera vez: cada pasaje cuesta una llamada de embeddings
  Registrar('conocimiento'#9 + Format('%d pasajes en %s', [N, FDConn.Params.Database]));

  // El grafo (capitulo 14): se arma en memoria con las tripletas del archivo. Builder.Graph
  // esta conectado en el formulario; cada nodo recibe un embedding para la busqueda semantica.
  Grafo.Embeddings := Embeddings;
  N := Builder.Process(TFile.ReadAllText(TPath.Combine(ExtractFilePath(ParamStr(0)), 'grafo\tripletas.json'), TEncoding.UTF8));
  Registrar('grafo'#9 + Format('%d tripletas, %d nodos, %d relaciones', [N, Grafo.NodeCount, Grafo.EdgeCount]));
end;

function TFormLadera.IndexarManual: Integer;
var
  Archivo, L, Titulo, Texto: string;
  Meta: TAiEmbeddingMetaData;

  procedure Guardar;
  begin
    if (Texto.Trim = '') or (Titulo = '') then
      Exit;
    Meta['fuente'] := IfThen(Archivo.Contains('foro'), 'foro', 'manual');
    Meta['seccion'] := Titulo;
    RAG.AddItem(Titulo + '. ' + Texto.Trim, Meta);   // embedding + fila en SQLite
    Inc(Result);
    Texto := '';
  end;

begin
  // Cada archivo de manual\ se parte por secciones: un titulo (linea corta sin punto
  // final) seguido de sus parrafos. Un pasaje por seccion, con su fuente en los metadatos.
  Result := 0;
  Meta := TAiEmbeddingMetaData.Create;
  try
    for Archivo in TDirectory.GetFiles(TPath.Combine(ExtractFilePath(ParamStr(0)), 'manual'), '*.txt') do
    begin
      Titulo := '';
      Texto := '';
      for L in TFile.ReadAllLines(Archivo, TEncoding.UTF8) do
      begin
        if L.Trim = '' then
          Continue;
        if (L.Length < 60) and not L.Trim.EndsWith('.') and not L.Contains(':') then
        begin
          Guardar;
          Titulo := L.Trim;
        end
        else if Titulo <> '' then
          Texto := Texto + L.Trim + ' ';
      end;
      Guardar;
    end;
  finally
    Meta.Free;
  end;
end;

procedure TFormLadera.FnBuscarManual(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Pregunta, Detalle, Respuesta: string;
  Encontrados: TAiRAGVector;
  I: Integer;
  Nodo: TAiEmbeddingNode;
begin
  Handled := True;
  Pregunta := ToolCall.Params.Values['pregunta'];
  // 1. Recuperar: los pasajes mas parecidos a la pregunta (coseno, en SQLite)
  Encontrados := RAG.Search(Pregunta, 4, 0.3, nil);
  try
    for I := 0 to Encontrados.Count - 1 do
      Detalle := Detalle + Format('%s %.2f · ', [VarToStr(Encontrados.Items[I].MetaData['seccion']), Encontrados.Items[I].Idx]);
    // 2. Reordenar con Jev: evidencia por pasaje; una inyeccion de prompt queda en -1
    try
      Encontrados.RerankWith(Pregunta, Reranker);
      Detalle := Detalle + '→ rerank: ';
    except
      on E: Exception do
        Detalle := Detalle + '→ sin reranker (' + E.Message + '): ';
    end;
    for I := 0 to Encontrados.Count - 1 do
    begin
      Nodo := Encontrados.Items[I];
      if Nodo.Idx < 0 then
        Detalle := Detalle + Format('%s descartado · ', [VarToStr(Nodo.MetaData['seccion'])])
      else
      begin
        Detalle := Detalle + Format('%s %.2f · ', [VarToStr(Nodo.MetaData['seccion']), Nodo.Idx]);
        if Nodo.Idx >= 0.2 then
          Respuesta := Respuesta + Nodo.Text + sLineBreak;
      end;
    end;
  finally
    Encontrados.Free;
  end;
  if Respuesta = '' then
    Respuesta := 'El manual no dice nada sobre eso.';
  ToolCall.Response := Respuesta;
  PasoHerramienta('buscar_en_el_manual(' + Pregunta + ')', Detalle.TrimRight([' ', '·']));
end;

{ El grafo (capitulo 14): lo que la tienda sabe de sus cafes y de sus clientes como
  entidades y relaciones. Una herramienta busca por significado y trae los vecinos;
  la otra hace una consulta MakerGQL con el parametro que mando el modelo. }

procedure TFormLadera.FnQueSabemosDe(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Nombre, Contexto: string;
begin
  Handled := True;
  Nombre := ToolCall.Params.Values['nombre'];
  // El nodo mas parecido al nombre y todo lo que lo rodea a dos saltos, como texto para el modelo
  Contexto := Grafo.SearchText(Nombre, 2, False, 1, 0.5);
  if Contexto.Trim = '' then
    Contexto := 'El grafo no tiene nada sobre ' + Nombre + '.';
  ToolCall.Response := Contexto;
  PasoHerramienta('que_sabemos_de(' + Nombre + ')', Format('%d caracteres de contexto del grafo', [Length(Contexto)]));
end;

procedure TFormLadera.FnCafesPorAcidez(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Nivel, GQL, R: string;
begin
  Handled := True;
  Nivel := ToolCall.Params.Values['nivel'].Trim;
  if Nivel <> '' then
    Nivel := UpperCase(Nivel.Substring(0, 1)) + LowerCase(Nivel.Substring(1));   // 'baja' -> 'Baja'
  // Una consulta de grafo con el parametro del modelo: productos unidos a ese nivel de acidez
  GQL := Format('MATCH (c:Producto)-[t:TIENE_ACIDEZ]->(a:Acidez) WHERE a.name = ''%s'' RETURN c', [Nivel.Replace('''', '')]);
  R := Grafo.ExecuteMakerGQL(GQL);
  ToolCall.Response := R;
  PasoHerramienta('cafes_por_acidez(' + Nivel + ')', GQL);
end;

{ La memoria (capitulo 15): lo que cada cliente conto, mas alla de esta conversacion }

procedure TFormLadera.PrepararMemoria;
begin
  // Los componentes y sus enlaces (Analyzer, Embedder, PersistentMemory) estan en el
  // formulario; aqui solo lo que depende de la maquina.
  Memoria.DbPath := TPath.Combine(CarpetaDatos, 'memoria.db');
  Analizador.Tool_Active := False;       // el analizador solo lee y resume
  FCliente := edCliente.Text.Trim;
end;

{ Conectores (capitulo 16): la bodega como servidor MCP, en otro proceso }

procedure TFormLadera.ConectarBodega;
var
  Item: TMCPClientItem;
  Lista: TJSONObject;
  Tools: TJSONArray;
  T: TJSONValue;
  Nombres: string;
begin
  // El item (nombre, transporte stdio, comando) esta en el formulario; aqui solo se
  // completa la ruta del .exe, que vive junto a la app. Initialize lanza el proceso hijo,
  // negocia la version del protocolo y pide tools/list.
  Item := AiFunctions1.MCPClients.Items[0] as TMCPClientItem;
  Item.Params.Values['Command'] := TPath.Combine(ExtractFilePath(ParamStr(0)), 'BodegaLadera.exe');
  if Item.MCPClient.Initialize and Item.MCPClient.Available then
  begin
    Lista := Item.MCPClient.ListTools;
    try
      if Assigned(Lista) and Lista.TryGetValue<TJSONArray>('tools', Tools) then
        for T in Tools do
          Nombres := Nombres + IfThen(Nombres = '', '', ', ') + '`' + T.GetValue<string>('name') + '`';
    finally
      Lista.Free;
    end;
    ChatView1.AddMessage(TChatRole.crAssistant, Format('🔌 Bodega conectada por MCP (`%s`, stdio, protocolo %s): %s.',
      [Item.Name, Item.MCPClient.NegotiatedProtocol, Nombres]));
    Registrar('mcp'#9 + Item.Name + ' ' + Item.MCPClient.NegotiatedProtocol + ': ' + Nombres);
  end
  else
    SetEstado('No se pudo arrancar el servidor MCP de la bodega');
end;

procedure TFormLadera.AiFunctions1Log(Sender: TObject; const Msg: string);
const
  ENVIO = 'CLIENT -> SERVER: ';
  RESPUESTA = 'SERVER -> CLIENT: ';
var
  J: TJSONValue;
  Params, Res: TJSONObject;
  Contenido: TJSONArray;
  Id, Texto, Llamada: string;
  I: Integer;
begin
  // El trafico JSON-RPC completo pasa por aqui; se muestran solo los tools/call y su resultado
  if Msg.StartsWith(ENVIO) then
  begin
    J := TJSONObject.ParseJSONValue(Msg.Substring(Length(ENVIO)));
    try
      if (J is TJSONObject) and (TJSONObject(J).GetValue<string>('method', '') = 'tools/call') then
      begin
        Params := TJSONObject(J).GetValue<TJSONObject>('params');
        TMonitor.Enter(FLlamadasMCP);
        try
          FLlamadasMCP.Values[TJSONObject(J).GetValue<string>('id', '')] :=
            Params.GetValue<string>('name', '') + ' ' + Params.GetValue<TJSONObject>('arguments').ToString;
        finally
          TMonitor.Exit(FLlamadasMCP);
        end;
      end;
    finally
      J.Free;
    end;
  end
  else if Msg.StartsWith(RESPUESTA) then
  begin
    J := TJSONObject.ParseJSONValue(Msg.Substring(Length(RESPUESTA)));
    try
      if J is TJSONObject then
      begin
        Id := TJSONObject(J).GetValue<string>('id', '');
        TMonitor.Enter(FLlamadasMCP);
        try
          I := FLlamadasMCP.IndexOfName(Id);
          if I >= 0 then
          begin
            Llamada := FLlamadasMCP.ValueFromIndex[I];
            FLlamadasMCP.Delete(I);
          end;
        finally
          TMonitor.Exit(FLlamadasMCP);
        end;
        if Llamada <> '' then
        begin
          if TJSONObject(J).TryGetValue<TJSONObject>('result', Res) and
            Res.TryGetValue<TJSONArray>('content', Contenido) and (Contenido.Count > 0) then
            Texto := TJSONObject(Contenido.Items[0]).GetValue<string>('text', '');
          PasoHerramienta('MCP bodega: tools/call ' + Llamada, Texto);
        end;
      end;
    finally
      J.Free;
    end;
  end;
end;

{ Habilidades (capitulo 17): procedimientos de la tienda, cargados solo cuando hacen falta }

procedure TFormLadera.CargarSkills;
var
  Nombres: string;
  I: Integer;
begin
  // Skills.Functions = AiFunctions1 esta en el formulario: al cargar la carpeta se registran
  // dos funciones (use_skill y read_skill_file). La carpeta vive junto al .exe.
  Skills.Folder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'skills');
  for I := 0 to Skills.Skills.Count - 1 do
    Nombres := Nombres + IfThen(Nombres = '', '', ', ') + '`' + Skills.Skills[I].Name + '`';
  // Para ver cada lectura de archivo, la app envuelve read_skill_file y delega en TAiSkills
  AiFunctions1.Functions.GetFunction(Skills.ReadFunctionName).OnAction := SkillLeerArchivo;
  ChatView1.AddMessage(TChatRole.crAssistant, Format('📚 %d skills disponibles (catálogo de %d caracteres): %s.',
    [Skills.Skills.Count, Skills.Catalog.Length, Nombres]));
  Registrar('skills'#9 + Format('%d skills, catalogo %d caracteres', [Skills.Skills.Count, Skills.Catalog.Length]));
end;

procedure TFormLadera.SkillsSkillLoaded(Sender: TObject; ASkill: TAiSkillItem);
begin
  // El modelo pidio use_skill: las instrucciones completas entran a la conversacion ahora
  PasoHerramienta('use_skill ' + ASkill.Name, Format('%d caracteres de instrucciones', [ASkill.Instructions.Text.Length]));
end;

procedure TFormLadera.SkillLeerArchivo(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  Handled := True;
  try
    ToolCall.Response := Skills.ExecuteReadFile(ToolCall.Params.Values['name'], ToolCall.Params.Values['path']);
    PasoHerramienta('read_skill_file ' + ToolCall.Params.Values['name'] + '/' + ToolCall.Params.Values['path'],
      Format('%d caracteres', [ToolCall.Response.Length]));
  except
    on E: Exception do
    begin
      ToolCall.Response := 'Error: ' + E.Message;
      PasoHerramienta('read_skill_file ' + ToolCall.Params.Values['path'], E.Message);
    end;
  end;
end;

{ El primer agente (capitulo 19): dos TLLMNode en el formulario, Atencion -> Revisor }

procedure TFormLadera.PrepararAgente;
begin
  // El grafo (Reclamos, Atencion, LinkAtencion, Revisor) esta armado en el formulario:
  // StartNode/EndNode, Next y NextA en el Object Inspector. Aqui va lo que no se puede
  // enlazar en diseno: las herramientas (un TAiFunctions normal, via el registro) y el
  // skill (un SKILL.md leido de la carpeta; el nodo lo libera).
  FHerramientasAgente := TAiToolRegistry.Create;
  FHerramientasAgente.RegisterFromTAiFunctions(FuncionesReclamo);
  Atencion.Registry := FHerramientasAgente;
  Atencion.Skill := TAiSkill.FromFolder(TPath.Combine(ExtractFilePath(ParamStr(0)), 'skills\respuesta-reclamo'));
end;

procedure TFormLadera.FnAtenderReclamo(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  R: string;
begin
  // Lucia delega: el grafo corre en su pool de hilos y este hilo (el de la peticion) espera.
  // Los eventos del grafo vuelven al hilo principal por su cuenta.
  Handled := True;
  PasoHerramienta('atender_reclamo → agente Reclamos', 'Atencion → Revisor');
  Reclamos.Run(ToolCall.Params.Values['mensaje'], nil);
  while Reclamos.Busy do
    Sleep(50);
  R := Reclamos.FindNode('Revisor').Output.Trim;
  if R.StartsWith('APROBADA') or R.StartsWith('CORREGIDA') then
    R := R.Substring(R.IndexOf(#10) + 1).Trim;       // la primera linea es el veredicto
  ToolCall.Response := R;
end;

procedure TFormLadera.FnConsultarPedido(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Numero: string;
begin
  Handled := True;
  Numero := ToolCall.Params.Values['numero'].Trim;
  if Numero = '1047' then
    ToolCall.Response := 'Pedido 1047 de Marta Gómez, Medellín: 2 bolsas de Huila Reserva (340 g). ' +
      'Molienda pedida: en grano. Despachado: molido para prensa francesa (error de empaque). Entregado el 20 de octubre.'
  else
    ToolCall.Response := 'No existe el pedido ' + Numero + '.';
  PasoHerramienta('   consultar_pedido(' + Numero + ')', ToolCall.Response);
end;

procedure TFormLadera.FnProgramarCambio(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Numero: string;
begin
  Handled := True;
  Numero := ToolCall.Params.Values['numero'].Trim;
  if Numero = '1047' then
    ToolCall.Response := 'Cambio programado sin costo: 2 bolsas de Huila Reserva en grano. ' +
      'Guía LAD-55821, sale mañana jueves y llega a Medellín el viernes. La clienta entrega las bolsas molidas al mensajero.'
  else
    ToolCall.Response := 'No existe el pedido ' + Numero + '.';
  PasoHerramienta('   programar_cambio(' + Numero + ', ' + ToolCall.Params.Values['motivo'].Trim + ')', ToolCall.Response);
end;

procedure TFormLadera.AgenteEnterNode(Sender: TObject; Node: TAIAgentsNode);
begin
  // Ya viene sincronizado con el hilo principal
  PasoHerramienta('→ nodo ' + Node.Name, 'entrada: ' + Node.Input.Replace(sLineBreak, ' '));
end;

procedure TFormLadera.AgenteExitNode(Sender: TObject; Node: TAIAgentsNode);
begin
  PasoHerramienta('   ' + Node.Name + ' terminó', Node.Output.Replace(sLineBreak, ' '));
end;

{ El flujo (capitulo 20): el pedido por un grafo armado en el formulario }

function Pesos(AValor: Integer): string;
begin
  Result := FormatFloat('#,##0', AValor).Replace(',', '.') + ' COP';
end;

function NotaEncargado(Node: TAIAgentsNode): string;
begin
  Result := '';
  if Node.Graph.Blackboard.GetString('respuesta_encargado') <> '' then
    Result := ' Aprobado por el encargado: "' + Node.Graph.Blackboard.GetString('respuesta_encargado') + '".';
end;

procedure TFormLadera.FnProcesarPedido(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  Handled := True;
  PasoHerramienta('procesar_pedido → flujo Pedidos', ToolCall.Params.Values['items'] + ' | ' + ToolCall.Params.Values['ciudad']);
  // Run(texto, nil) arranca con blackboard y estado limpios; las ramas corren en el pool
  Pedidos.Run(ToolCall.Params.Values['items'] + ' | ' + ToolCall.Params.Values['ciudad'], nil);
  while Pedidos.Busy do
    Sleep(50);
  // Suspendido: el grafo guardo su estado y la pregunta ya esta en el chat (OnSuspend)
  if Pedidos.Blackboard.GetStatus = esSuspended then
    ToolCall.Response := 'El pedido supera el tope que puedo confirmar sola y quedó EN ESPERA de la aprobación ' +
      'del encargado; la pregunta ya le llegó. Dile al cliente que confirmamos en cuanto el encargado responda, sin prometer fecha.'
  else
    ToolCall.Response := Pedidos.FindNode('Redactor').Output.Trim;
end;

procedure TFormLadera.RecibirExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Partes: TArray<string>;
begin
  // "cantidad producto, cantidad producto | ciudad" -> al blackboard, para todas las ramas
  Partes := Input.Split(['|']);
  Node.Graph.Blackboard.SetString('items', Partes[0].Trim);
  Node.Graph.Blackboard.SetString('ciudad', Partes[1].Trim);
  Output := Input;
end;

procedure TFormLadera.InventarioExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Item, Nombre, Faltan: string;
  Cant, Faltantes: Integer;
begin
  Faltantes := 0;
  Faltan := '';
  for Item in Node.Graph.Blackboard.GetString('items').Split([',']) do
  begin
    Cant := StrToIntDef(Item.Trim.Split([' '])[0], 0);
    Nombre := Item.Trim.Substring(Item.Trim.IndexOf(' ') + 1);
    if BolsasDe(Nombre) < Cant then
    begin
      Inc(Faltantes);
      Faltan := Faltan + IfThen(Faltan <> '', ', ') + Nombre;
    end;
  end;
  Node.Graph.Blackboard.SetInteger('faltantes', Faltantes);   // lo lee la expresion del link
  Node.Graph.Blackboard.SetString('agotados', Faltan);
  Output := Format('faltantes: %d %s', [Faltantes, Faltan]);
end;

procedure TFormLadera.CotizarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Item, Nombre: string;
  Subtotal: Integer;
begin
  Subtotal := 0;
  for Item in Node.Graph.Blackboard.GetString('items').Split([',']) do
  begin
    Nombre := Item.Trim.Substring(Item.Trim.IndexOf(' ') + 1);
    Inc(Subtotal, StrToIntDef(Item.Trim.Split([' '])[0], 0) * PrecioDe(Nombre));
  end;
  Node.Graph.Blackboard.SetInteger('subtotal', Subtotal);
  Output := 'subtotal: ' + Pesos(Subtotal);
end;

procedure TFormLadera.EnvioExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Costo: Integer;
begin
  if SameText(Node.Graph.Blackboard.GetString('ciudad'), 'Pitalito') then
    Costo := 5000
  else
    Costo := 12000;
  Node.Graph.Blackboard.SetInteger('envio', Costo);
  Output := 'envío: ' + Pesos(Costo);
end;

procedure TFormLadera.ConsolidarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  BB: TAIBlackboard;
begin
  // jmAll: corre una sola vez, cuando las tres ramas terminaron
  BB := Node.Graph.Blackboard;
  BB.SetInteger('total', BB.GetInteger('subtotal') + BB.GetInteger('envio'));
  Output := Format('total %s, faltantes %d', [Pesos(BB.GetInteger('total')), BB.GetInteger('faltantes')]);
end;

procedure TFormLadera.AvisarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Format('Pedido en espera. Productos agotados: %s. Pedido: %s. Ciudad: %s.',
    [Node.Graph.Blackboard.GetString('agotados'), Node.Graph.Blackboard.GetString('items'),
     Node.Graph.Blackboard.GetString('ciudad')]);
end;

procedure TFormLadera.DespacharExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  // El link de este nodo es una expresion: es_local = 1 -> Mensajero; si no (NextNo) -> Transportadora
  Node.Graph.Blackboard.SetInteger('es_local', Ord(SameText(Node.Graph.Blackboard.GetString('ciudad'), 'Pitalito')));
  Output := 'zona: ' + IfThen(Node.Graph.Blackboard.GetInteger('es_local') = 1, 'local', 'nacional');
end;

procedure TFormLadera.MensajeroExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Format('Pedido confirmado: %s. Total %s con envío. Entrega hoy con mensajero en %s.',
    [Node.Graph.Blackboard.GetString('items'), Pesos(Node.Graph.Blackboard.GetInteger('total')),
     Node.Graph.Blackboard.GetString('ciudad')]) + NotaEncargado(Node);
end;

procedure TFormLadera.TransportadoraExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Format('Pedido confirmado: %s. Total %s con envío. Sale por transportadora a %s, llega en 2 a 3 días.',
    [Node.Graph.Blackboard.GetString('items'), Pesos(Node.Graph.Blackboard.GetInteger('total')),
     Node.Graph.Blackboard.GetString('ciudad')]) + NotaEncargado(Node);
end;

{ Aprobacion (capitulo 21): el humano en el ciclo, con el estado en disco }

const
  TOPE_SIN_APROBACION = 200000;   // COP: por encima, decide el encargado

procedure TFormLadera.PrepararAprobacion;
var
  Carpeta: string;
  Pendientes: TArray<string>;
begin
  // La suspension va a disco: sobrevive al cierre de la app. Al arrancar, si quedo un
  // pedido esperando, la proxima respuesta del chat lo reanuda.
  Carpeta := TPath.Combine(CarpetaDatos, 'checkpoints');
  ForceDirectories(Carpeta);
  Pedidos.Checkpointer := TAiFileCheckpointer.Create(Carpeta);
  Pendientes := Pedidos.GetActiveThreads;
  if Length(Pendientes) > 0 then
  begin
    FHiloPendiente := Pendientes[0];
    ChatView1.AddMessage(TChatRole.crAssistant, '⏸️ Hay un pedido esperando la aprobación del encargado desde la sesión ' +
      'anterior (hilo `' + FHiloPendiente + '`). Tu próxima respuesta lo reanuda: **sí** aprueba, cualquier otra cosa lo rechaza.');
    Registrar('aprobacion'#9'hilo pendiente del checkpoint: ' + FHiloPendiente);
  end;
end;

procedure TFormLadera.RevisarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  BB: TAIBlackboard;
begin
  // El link de este nodo lee 'grande': 1 -> Aprobar (suspende); si no (NextNo) -> Despachar
  BB := Node.Graph.Blackboard;
  BB.SetInteger('grande', Ord(BB.GetInteger('total') > TOPE_SIN_APROBACION));
  BB.SetString('resumen', Format('%s para %s: total %s con envío.', [BB.GetString('items'), BB.GetString('ciudad'), Pesos(BB.GetInteger('total'))]));
  Output := IfThen(BB.GetInteger('grande') = 1, 'supera ' + Pesos(TOPE_SIN_APROBACION) + ': necesita aprobación', 'dentro del tope');
end;

procedure TFormLadera.DecidirExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  R: string;
  Aprobado: Boolean;
begin
  // Input es la respuesta del encargado, tal como la escribio en el chat
  R := Input.Trim.ToLower;
  Aprobado := (not R.StartsWith('no')) and (R.StartsWith('sí') or R.StartsWith('si') or R.StartsWith('ok') or
    R.StartsWith('dale') or (Pos('aprob', R) > 0));
  Node.Graph.Blackboard.SetInteger('aprobado', Ord(Aprobado));
  Node.Graph.Blackboard.SetString('respuesta_encargado', Input.Trim);
  Output := IfThen(Aprobado, 'aprobado', 'no aprobado') + ': "' + Input.Trim + '"';
end;

procedure TFormLadera.RechazarExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Format('Pedido NO aprobado por el encargado ("%s"). Pedido: %s. Ciudad: %s. Total %s. No se despacha.',
    [Node.Graph.Blackboard.GetString('respuesta_encargado'), Node.Graph.Blackboard.GetString('items'),
     Node.Graph.Blackboard.GetString('ciudad'), Pesos(Node.Graph.Blackboard.GetInteger('total'))]);
end;

procedure TFormLadera.PedidosSuspend(Sender: TObject; const AThreadID, ANodeName, AReason, AContext: string);
begin
  // Llega en un hilo del pool. La pregunta se vuelve un mensaje del chat; nada queda esperando.
  TThread.Queue(nil,
    procedure
    begin
      FHiloPendiente := AThreadID;
      ChatView1.AddMessage(TChatRole.crAssistant, '⏸️ **' + AReason + '**' + sLineBreak + AContext + sLineBreak + sLineBreak +
        'Responde **sí** para aprobar; cualquier otra respuesta lo rechaza. (El estado quedó guardado: puedes cerrar la app y responder después.)');
      Registrar('suspendido'#9 + ANodeName + ' ' + AThreadID + ': ' + AReason + ' | ' + AContext);
    end);
end;

procedure TFormLadera.PedidosFinish(Sender: TObject; const Input, Output: string; Status: TAgentExecutionStatus; E: Exception);
var
  Pie: string;
begin
  // Solo interesa cuando el turno es una reanudacion: la respuesta final va a la burbuja abierta.
  // (Si el grafo lo disparo la herramienta procesar_pedido, esta ya espera con Busy.)
  if Status = esSuspended then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if not FReanudando or (FBurbuja < 0) then
        Exit;
      FReanudando := False;
      Pie := Format('reanudado desde el checkpoint · %.1f s', [FReloj.ElapsedMilliseconds / 1000]);
      ChatView1.SetMessageText(FBurbuja, Pedidos.FindNode('Redactor').Output.Trim);
      ChatView1.FinishMessage(FBurbuja);
      ChatView1.SetMessageFooter(FBurbuja, Pie);
      Registrar('turno'#9 + Pie);
      FBurbuja := -1;
      ChatInput1.Busy := False;
      SetEstado(Format('%s / %s', [AiConn.DriverName, AiConn.AiChat.Model]));
    end);
end;

{ Reglas (capitulo 22): lo que Lucia no puede hacer sola, decidido ANTES de ejecutar }

const
  DESCUENTO_MAXIMO = 15;

procedure TFormLadera.FnAplicarDescuento(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Porcentaje: Integer;
begin
  // Si el guardrail la dejo pasar, el porcentaje ya es <= 15
  Handled := True;
  Porcentaje := StrToIntDef(ToolCall.Params.Values['porcentaje'], 0);
  ToolCall.Response := Format('Descuento del pedido %s fijado en %d %%: nuevo total %s COP.',
    [ToolCall.Params.Values['numero'], Porcentaje, FormatFloat('#,##0', 76000 * (100 - Porcentaje) / 100).Replace(',', '.')]);
  PasoHerramienta(Format('aplicar_descuento(%s, %d %%)', [ToolCall.Params.Values['numero'], Porcentaje]), ToolCall.Response);
end;

procedure TFormLadera.FnAnularPedido(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  // Si esto se ejecuta, el guardrail no hizo su trabajo
  Handled := True;
  ToolCall.Response := 'Pedido anulado.';
  PasoHerramienta('anular_pedido(' + ToolCall.Params.Values['numero'] + ')', '¡SE EJECUTÓ! el guardrail no bloqueó');
end;

procedure TFormLadera.GuardiaCheckToolCall(Sender: TObject; const AToolName, AArguments: string; var AAllow: Boolean;
  var AReason: string);
var
  J: TJSONValue;
  Porcentaje: Integer;
begin
  // Veto en codigo: AArguments es el JSON crudo que mando el modelo (se parsea, no se compara como texto)
  if AToolName <> 'aplicar_descuento' then
    Exit;
  Porcentaje := 0;
  J := TJSONObject.ParseJSONValue(AArguments);
  try
    if J is TJSONObject then
      Porcentaje := TJSONObject(J).GetValue<Integer>('porcentaje', 0);
  finally
    J.Free;
  end;
  if Porcentaje > DESCUENTO_MAXIMO then
  begin
    AAllow := False;
    AReason := Format('el descuento máximo que puede aplicar Lucía es %d %%', [DESCUENTO_MAXIMO]);
  end;
end;

procedure TFormLadera.GuardiaBlocked(Sender: TObject; const AToolName, AArguments, AReason: string);
begin
  PasoHerramienta('🛑 guardrail: ' + AToolName + ' ' + AArguments, 'BLOQUEADO: ' + AReason);
end;

{ Observabilidad (capitulo 23): el componente esta en el formulario; aqui solo se ve que exporta }

procedure TFormLadera.TelemetriaLog(Sender: TObject; const AMsg: string);
begin
  // Llega desde el hilo exportador (lotes cada FlushIntervalMs)
  Registrar('otel'#9 + AMsg);
  TThread.Queue(nil,
    procedure
    begin
      if not FCerrando then
        SetEstado('OTel: ' + AMsg);
    end);
end;

{ Nueva, Guardar, Abrir }

procedure TFormLadera.btnNuevaClick(Sender: TObject);
var
  N: Integer;
  Lista: TMemoryEntryList;
  E: TMemoryEntry;
  Texto: string;
begin
  // Cerrar la sesion: el analizador lee la conversacion acumulada y guarda lo que
  // vale la pena recordar. Es sincrono (un modelo barato, dos o tres segundos).
  SetEstado('Analizando la conversación...');
  N := Memoria.AnalyzeNow;
  AiConn.NewChat;
  ChatView1.ClearMessages;
  Texto := 'Conversación nueva.';
  if N > 0 then
  begin
    Texto := Texto + Format(' 🧠 De la charla anterior guardé **%d recuerdos**. Lo que recuerdo de %s:', [N, FCliente]);
    Lista := Memoria.Recall(1, 20);     // todo lo guardado de este cliente, por importancia
    try
      for E in Lista do
        Texto := Texto + sLineBreak + Format('- (%d) %s', [E.Importance, E.Content]);
    finally
      Lista.Free;
    end;
  end;
  ChatView1.AddMessage(TChatRole.crAssistant, Texto);
  Registrar('memoria'#9 + Format('%d recuerdos guardados de %s', [N, FCliente]));
  SetEstado('Conversación nueva');
end;

function TFormLadera.CarpetaDatos: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'CafeLaLadera');
  ForceDirectories(Result);
end;

procedure TFormLadera.btnGuardarClick(Sender: TObject);
var
  St: TFileStream;
begin
  St := TFileStream.Create(TPath.Combine(CarpetaDatos, 'pantalla.json'), fmCreate);
  try
    ChatView1.SaveToStream(St);
  finally
    St.Free;
  end;
  AiConn.Messages.SaveToFile(TPath.Combine(CarpetaDatos, 'historial.json'));
  SetEstado('Guardado en ' + CarpetaDatos);
end;

procedure TFormLadera.btnAbrirClick(Sender: TObject);
var
  St: TFileStream;
begin
  if not FileExists(TPath.Combine(CarpetaDatos, 'historial.json')) then
  begin
    SetEstado('No hay conversación guardada');
    Exit;
  end;
  St := TFileStream.Create(TPath.Combine(CarpetaDatos, 'pantalla.json'), fmOpenRead);
  try
    ChatView1.ClearMessages;
    ChatView1.LoadFromStream(St);
  finally
    St.Free;
  end;
  AiConn.NewChat;
  AiConn.Messages.LoadFromFile(TPath.Combine(CarpetaDatos, 'historial.json'));
  SetEstado(Format('Conversación recuperada: %d mensajes', [AiConn.Messages.Count]));
end;

procedure TFormLadera.SetEstado(const AText: string);
begin
  lblEstado.Text := AText;
end;

{ Prueba automatica }

procedure TFormLadera.Registrar(const ALinea: string);
begin
  if FRegistro <> '' then
    TFile.AppendAllText(FRegistro, ALinea + sLineBreak, TEncoding.UTF8);
end;

procedure TFormLadera.IniciarPrueba;
var
  I: Integer;
begin
  I := 1;
  while I <= ParamCount do
  begin
    if SameText(ParamStr(I), '--captura') and (I < ParamCount) then
    begin
      FCaptura := ParamStr(I + 1);
      Inc(I);
    end
    else if SameText(ParamStr(I), '--registro') and (I < ParamCount) then
    begin
      FRegistro := ParamStr(I + 1);
      Inc(I);
    end
    else if not SameText(ParamStr(I), '--prueba') then
    begin
      if not Assigned(FPrueba) then
        FPrueba := TStringList.Create;
      FPrueba.Add(ParamStr(I));
    end;
    Inc(I);
  end;
  if not Assigned(FPrueba) then
    Exit;
  FTimerPrueba := TTimer.Create(Self);
  FTimerPrueba.Interval := 100;
  FFinTurno := TThread.GetTickCount + 1500;
  FTimerPrueba.OnTimer := PasoPrueba;
end;

procedure TFormLadera.PasoPrueba(Sender: TObject);
var
  Linea, Pregunta, Archivo: string;
  Archivos: TAiMediaFiles;
  MF: TAiMediaFile;
begin
  // Envia cada pregunta como si la escribiera el usuario. "@ruta|pregunta" adjunta un archivo.
  if ChatInput1.Busy or (TThread.GetTickCount < FFinTurno) then
    Exit;
  if FPrueba.Count = 0 then
  begin
    FTimerPrueba.Enabled := False;
    if FCaptura <> '' then
      with ChatView1.MakeScreenshot do
      try
        SaveToFile(FCaptura);
      finally
        Free;
      end;
    Close;
    Exit;
  end;
  Linea := FPrueba[0];
  FPrueba.Delete(0);
  Registrar('accion'#9 + Linea);
  if SameText(Linea, '/nueva') then
    btnNuevaClick(nil)
  else if Linea.StartsWith('/cliente ') then
    edCliente.Text := Linea.Substring(9).Trim
  else
  begin
    Pregunta := Linea;
    Archivo := '';
    if Linea.StartsWith('@') then
    begin
      Pregunta := Linea.Substring(Linea.IndexOf('|') + 1);
      Archivo := Linea.Substring(1, Linea.IndexOf('|') - 1);
    end;
    Archivos := TAiMediaFiles.Create;
    try
      if Archivo <> '' then
      begin
        MF := TAiMediaFile.Create;
        MF.LoadFromFile(Archivo);
        Archivos.Add(MF);
      end;
      ChatInput1.PromptText := '';
      ChatInput1.Busy := True;
      ChatInput1SendEvent(ChatInput1, Pregunta, Archivos, nil);
    finally
      Archivos.Free;
    end;
  end;
  FFinTurno := TThread.GetTickCount + 700;
end;

end.
