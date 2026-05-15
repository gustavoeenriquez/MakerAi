// MIT License
// MakerAI - Sistema de Agentes v3.4
// TLLMNode: nodo de agente con loop ReAct integrado.
//
// TLLMNode extiende TAIAgentsNode añadiendo un modelo LLM con capacidad de
// llamar herramientas del TAiToolRegistry automáticamente. El loop LLM →
// Tool → Observación es manejado internamente por TAiChatConnection
// (function calling nativo de cada proveedor).
//
// Uso básico:
//   Node := TLLMNode.Create(Manager);
//   Node.DriverName   := 'Claude';
//   Node.Model        := 'claude-sonnet-4-5';
//   Node.ApiKey       := '@CLAUDE_API_KEY';
//   Node.SystemPrompt := 'Eres un asistente experto en...';
//   Node.UseAllTools  := True;   // inyecta todo el TAiToolRegistry.Instance
//
// Autor: Gustavo Enríquez
// GitHub: https://github.com/gustavoeenriquez/MakerAi

unit uMakerAi.Agents.Node.LLM;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,     // IHTTPResponse (para TAiErrorEvent)
  uMakerAi.Agents,
  uMakerAi.Agents.IAiTool,
  uMakerAi.Agents.ToolRegistry,
  uMakerAi.Agents.Skill,     // TAiSkill
  uMakerAi.Chat.Messages,    // TAiToolsFunction
  uMakerAi.Tools.Functions;  // TAiFunctions, TFunctionActionItem, TFunctionEvent

type

  { TLLMNode -------------------------------------------------------------------
    Nodo de agente con LLM y herramientas del TAiToolRegistry integradas.

    El loop ReAct (Think → Call Tool → Observe → repeat) es transparente:
    TAiChatConnection lo maneja internamente vía function calling. TLLMNode
    sólo conecta el resultado de cada tool call al TAiToolRegistry.

    Propiedad Registry:
      - nil (default) → usa TAiToolRegistry.Instance (singleton global)
      - Asignar uno propio para aislar las herramientas de este nodo

    Ciclo de vida de los objetos internos:
      TAiChatConnection y TAiFunctions se crean y liberan en cada llamada a
      DoExecute para que no persista estado entre ejecuciones del nodo.
  }
  TLLMNode = class(TAIAgentsNode)
  private
    FDriverName    : String;
    FModel         : String;
    FApiKey        : String;
    FSystemPrompt  : String;
    FMaxTokens     : Integer;
    FUseAllTools   : Boolean;
    FRegistry      : TAiToolRegistry;
    FSkill         : TAiSkill;

    // Estado temporal válido sólo durante DoExecute (un hilo a la vez por nodo)
    FActiveRegistry: TAiToolRegistry;
    // Captura el último error del LLM para re-lanzarlo como excepción
    FLastError     : String;

    procedure SetSkill(const Value: TAiSkill);
    procedure LoadRegistryTools(AFunctions: TAiFunctions);
    // Carga en AFunctions sólo las herramientas de AToolNames presentes en el registry
    procedure LoadSkillTools(AFunctions: TAiFunctions; AToolNames: TStrings);
    procedure HandleToolCall(Sender: TObject;
                             FunctionAction: TFunctionActionItem;
                             FunctionName: String;
                             ToolCall: TAiToolsFunction;
                             var Handled: Boolean);
    procedure InternalOnError(Sender: TObject; const ErrorMsg: string;
                              AException: Exception; const AResponse: IHTTPResponse);
  protected
    procedure DoExecute(aBeforeNode: TAIAgentsNode;
                        aLink: TAIAgentsLink); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Referencia al registry a usar. nil = TAiToolRegistry.Instance.
    property Registry: TAiToolRegistry read FRegistry write FRegistry;

    // Skill de configuración reutilizable.
    // El nodo toma ownership: libera el skill anterior al asignar uno nuevo,
    // y libera el skill activo en el destructor.
    // Si se asigna un skill:
    //   - Sus campos se aplican como BASE antes de las props del nodo.
    //   - Las props explícitas del nodo (no vacías) sobrescriben las del skill.
    //   - ExtraTools se carga si UseAllTools=False y el skill tiene herramientas.
    property Skill: TAiSkill read FSkill write SetSkill;
  published
    // Nombre del driver LLM: 'OpenAI', 'Claude', 'Gemini', 'Ollama', etc.
    // Vacío = usa el DriverName del skill (si está asignado).
    property DriverName   : String  read FDriverName   write FDriverName;
    // Modelo específico del proveedor (vacío = usa el del skill o el default del driver)
    property Model        : String  read FModel        write FModel;
    // API key. Soporta sintaxis @ENV_VAR_NAME para resolución en runtime.
    // Vacío = usa el ApiKey del skill (si está asignado).
    property ApiKey       : String  read FApiKey       write FApiKey;
    // Instrucción de sistema para el LLM.
    // Vacío = usa el SystemPrompt del skill (si está asignado).
    property SystemPrompt : String  read FSystemPrompt write FSystemPrompt;
    // Máximo de tokens en la respuesta (0 = usa el default del driver)
    property MaxTokens    : Integer read FMaxTokens    write FMaxTokens default 0;
    // Si True, inyecta automáticamente todas las herramientas disponibles del registry.
    // Si False y hay un skill con ExtraTools, carga sólo esas herramientas.
    property UseAllTools  : Boolean read FUseAllTools  write FUseAllTools default True;
  end;


procedure Register;

implementation

uses
  uMakerAi.Chat.AiConnection;   // TAiChatConnection

procedure Register;
begin
  RegisterComponents('MakerAI', [TLLMNode]);
end;



{ TLLMNode }

constructor TLLMNode.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDriverName     := 'Claude';
  FModel          := '';
  FApiKey         := '';
  FSystemPrompt   := '';
  FMaxTokens      := 0;
  FUseAllTools    := True;
  FRegistry       := nil;
  FActiveRegistry := nil;
  FSkill          := nil;
end;

destructor TLLMNode.Destroy;
begin
  FSkill.Free;
  inherited;
end;

// ---------------------------------------------------------------------------
// Setter de Skill: el nodo toma ownership — libera el skill anterior si existe.
// ---------------------------------------------------------------------------
procedure TLLMNode.SetSkill(const Value: TAiSkill);
begin
  if FSkill = Value then Exit;
  FSkill.Free;
  FSkill := Value;
end;

// ---------------------------------------------------------------------------
// Carga todas las herramientas del registry en el componente TAiFunctions.
// Cada IAiTool se convierte en un TFunctionActionItem usando SetJSon para
// transferir el nombre, descripción e inputSchema completo.
// ---------------------------------------------------------------------------
procedure TLLMNode.LoadRegistryTools(AFunctions: TAiFunctions);
var
  Tools  : TArray<IAiTool>;
  T      : IAiTool;
  Item   : TFunctionActionItem;
  Schema : TJSONObject;
begin
  if not Assigned(AFunctions) then Exit;
  if not Assigned(FActiveRegistry) then Exit;

  Tools := FActiveRegistry.GetAll;
  for T in Tools do
  begin
    // Registrar en TAiFunctions con el handler unificado
    Item := AFunctions.Functions.AddFunction(T.Name, True, HandleToolCall);
    Item.Description.Text := T.Description;

    // Preservar el schema completo sin descomponerlo en TFunctionParamsItems.
    // Esto garantiza que schemas complejos (anyOf, nested objects, arrays, etc.)
    // lleguen intactos a GetTools() → NormalizeToolsFromSource → FormatToolList.
    Schema := T.GetSchema;  // NO liberar — propiedad de la herramienta
    if Assigned(Schema) then
      Item.RawSchemaJson := Schema.ToJSON
    else
      Item.RawSchemaJson := '{"type":"object","properties":{}}';
  end;
end;

// ---------------------------------------------------------------------------
// Carga en AFunctions sólo las herramientas de AToolNames presentes en el
// registry activo. Usado cuando UseAllTools=False y el skill define ExtraTools.
// ---------------------------------------------------------------------------
procedure TLLMNode.LoadSkillTools(AFunctions: TAiFunctions; AToolNames: TStrings);
var
  ToolName: String;
  Tool    : IAiTool;
  Item    : TFunctionActionItem;
  Schema  : TJSONObject;
begin
  if not Assigned(AFunctions) then Exit;
  if not Assigned(FActiveRegistry) then Exit;

  for ToolName in AToolNames do
  begin
    if not FActiveRegistry.TryFind(ToolName, Tool) then
      Continue;

    Item := AFunctions.Functions.AddFunction(Tool.Name, True, HandleToolCall);
    Item.Description.Text := Tool.Description;
    Schema := Tool.GetSchema;  // NO liberar — propiedad de la herramienta
    if Assigned(Schema) then
      Item.RawSchemaJson := Schema.ToJSON
    else
      Item.RawSchemaJson := '{"type":"object","properties":{}}';
  end;
end;

// ---------------------------------------------------------------------------
// Captura errores del LLM para re-lanzarlos como excepción en DoExecute.
// ---------------------------------------------------------------------------
procedure TLLMNode.InternalOnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  FLastError := ErrorMsg;
end;

// ---------------------------------------------------------------------------
// Handler unificado para todas las tool calls del LLM.
// TAiChatConnection invoca este método cuando el modelo pide ejecutar
// una función. Buscamos la herramienta en el registry y ejecutamos.
// ---------------------------------------------------------------------------
procedure TLLMNode.HandleToolCall(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: String;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Tool      : IAiTool;
  ArgsJSON  : TJSONObject;
  ResultJSON: TJSONObject;
begin
  Handled := False;
  if not Assigned(FActiveRegistry) then Exit;

  if not FActiveRegistry.TryFind(FunctionName, Tool) then
  begin
    ToolCall.Response := Format('{"error":"Tool ''%s'' not found in registry"}', [FunctionName]);
    Handled := True;
    Exit;
  end;

  // Parsear argumentos
  ArgsJSON := nil;
  if ToolCall.Arguments <> '' then
  begin
    try
      ArgsJSON := TJSONObject(TJSONObject.ParseJSONValue(ToolCall.Arguments));
    except
      ArgsJSON := nil;
    end;
  end;

  ResultJSON := nil;
  try
    try
      ResultJSON := Tool.Execute(ArgsJSON);   // caller libera el resultado
      if Assigned(ResultJSON) then
        ToolCall.Response := ResultJSON.ToJSON
      else
        ToolCall.Response := '{"result":"ok"}';
    except
      on E: Exception do
        ToolCall.Response := Format('{"error":"%s"}', [E.Message]);
    end;
  finally
    ArgsJSON.Free;
    ResultJSON.Free;
  end;

  Handled := True;
end;

// ---------------------------------------------------------------------------
// Punto de entrada principal del nodo. Reemplaza el DoExecute genérico.
// Crea el chat, carga las herramientas y ejecuta el input del nodo.
// El loop ReAct es gestionado internamente por TAiChatConnection.
// ---------------------------------------------------------------------------
procedure TLLMNode.DoExecute(aBeforeNode: TAIAgentsNode;
  aLink: TAIAgentsLink);
var
  Chat      : TAiChatConnection;
  Functions : TAiFunctions;
  Response  : String;
begin
  // Evaluar logica de join (jmAny / jmAll) y actualizar Self.Input.
  // Si el nodo no esta listo aun (jmAll esperando mas inputs), salir sin ejecutar.
  if not CheckJoinAndPrepareInput(aBeforeNode, aLink) then Exit;

  // Disparar OnEnterNode ANTES de la llamada al LLM para que el handler pueda
  // modificar Self.Input (p.ej. inyectar historial del debate).
  // Se llama directo (sin Synchronize) porque Self.Input solo lo toca este
  // worker thread en este momento, y Blackboard ya es thread-safe.
  // Nota: en apps con UI, el handler debe ser thread-safe (no tocar controles).
  if Assigned(Self.Graph) and Assigned(Self.Graph.OnEnterNode) then
    Self.Graph.OnEnterNode(Self.Graph, Self);

  // Determinar el registry activo para esta ejecución
  if Assigned(FRegistry) then
    FActiveRegistry := FRegistry
  else
    FActiveRegistry := TAiToolRegistry.Instance;

  FLastError := '';
  Chat      := TAiChatConnection.Create(nil);
  Functions := TAiFunctions.Create(nil);
  try
    // 1. Aplicar el skill como BASE (si está asignado).
    //    Sus valores se establecen primero; las props del nodo los sobrescriben
    //    a continuación si son no-vacías.
    if Assigned(FSkill) then
    begin
      if FSkill.DriverName <> '' then
        Chat.DriverName := FSkill.DriverName;
      if FSkill.Model <> '' then
        Chat.Model := FSkill.Model;
      if FSkill.ApiKey <> '' then
        Chat.Params.Values['ApiKey'] := FSkill.ApiKey;
      if FSkill.SystemPrompt <> '' then
        Chat.SystemPrompt.Text := FSkill.SystemPrompt;
    end;

    // 2. Aplicar props explícitas del nodo (tienen prioridad sobre el skill).
    //    DriverName: override si el nodo tiene valor; si no, conserva el del skill.
    if FDriverName <> '' then
      Chat.DriverName := FDriverName;
    if FModel <> '' then
      Chat.Model := FModel;

    // Conectar handler de errores para capturar fallos del LLM
    Chat.OnError := InternalOnError;

    // Parámetros vía TStrings (Asynchronous DEBE ser False en nodos de agente)
    Chat.Params.Values['Asynchronous'] := 'False';
    if FApiKey <> '' then
      Chat.Params.Values['ApiKey'] := FApiKey;
    if FMaxTokens > 0 then
      Chat.Params.Values['Max_tokens'] := IntToStr(FMaxTokens);
    if FSystemPrompt <> '' then
      Chat.SystemPrompt.Text := FSystemPrompt;

    // 3. Cargar herramientas.
    //    - UseAllTools=True  → carga todo el registry (comportamiento original).
    //    - UseAllTools=False + Skill.ExtraTools → carga sólo las tools del skill.
    if FUseAllTools and (FActiveRegistry.Count > 0) then
    begin
      LoadRegistryTools(Functions);
      Chat.AiFunctions := Functions;

      // Limpiar ModelCaps/SessionCaps: LLMNode usa sólo TAiFunctions.
      // Sin esto, el driver puede añadir tools built-in (web_search,
      // code_execution, etc.) que nuestro código no sabe manejar.
      Chat.Params.Values['ModelCaps']   := '[]';
      Chat.Params.Values['SessionCaps'] := '[]';

      // Tool_Active debe ser True para que el driver envíe las tools al LLM.
      // Algunos drivers (Claude, Gemini) lo tienen en False por defecto.
      Chat.Params.Values['Tool_Active'] := 'True';
    end
    else if Assigned(FSkill) and (FSkill.ExtraTools.Count > 0) then
    begin
      // UseAllTools=False pero el skill declara herramientas específicas
      LoadSkillTools(Functions, FSkill.ExtraTools);
      if Functions.Functions.Count > 0 then
      begin
        Chat.AiFunctions := Functions;
        Chat.Params.Values['ModelCaps']   := '[]';
        Chat.Params.Values['SessionCaps'] := '[]';
        Chat.Params.Values['Tool_Active'] := 'True';
      end;
    end;

    // Ejecutar el input del nodo (el loop de tools es automático)
    Response := Chat.AddMessageAndRun(Self.Input, 'user', []);

    // Si el LLM reportó un error y la respuesta está vacía, propagarlo
    if (Response = '') and (FLastError <> '') then
      raise Exception.Create('[TLLMNode] LLM error: ' + FLastError);

    Self.Output := Response;

    // Publicar en el Blackboard para que otros nodos puedan leerlo
    if Assigned(Self.Graph) and Assigned(Self.Graph.Blackboard) then
      Self.Graph.Blackboard.SetString(Self.Name + '.output', Response);

  finally
    // Desconectar el functions ANTES de liberar para evitar referencias colgantes
    Chat.AiFunctions := nil;
    Functions.Free;
    Chat.Free;
    FActiveRegistry := nil;
  end;

  // Enrutar al siguiente link (equivalente al bloque de DoExecute del base class
  // que no se ejecuta porque TLLMNode sobreescribe DoExecute sin llamar inherited).
  DoTraverseLinks(aBeforeNode, aLink);
end;

initialization
  // Necesario para que el DFM streaming encuentre TLLMNode en runtime
  // (RegisterComponents solo aplica en el IDE; RegisterClass aplica siempre)
  RegisterClass(TLLMNode);

end.
