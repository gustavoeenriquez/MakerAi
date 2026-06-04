program CadenaSupministro;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — Capítulo 23: RAG de Grafos
// Demo: Cadena de suministro — grafo de conocimiento + integración LLM
// =============================================================================
//
//   A) Construcción del grafo y consultas GQL (sin API key)
//      TAiRagGraph + GQL estructural vía ExecuteMakerGQL
//
//   B) Lenguaje natural → GQL → respuesta via LLM (requiere OPENAI_API_KEY)
//      El modelo genera la consulta GQL; el grafo la ejecuta; el modelo responde
//
// Prerequisito: paquete MakerAI instalado.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  uMakerAi.RAG.Graph.Core,
  uMakerAi.RAG.Graph.GQL,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.OpenAi;  // auto-registra driver 'OpenAI' en el factory

const
  GRAFO_FILE    = 'cadena_suministro.makerAi';
  MODEL_OPENAI  = 'gpt-4o-mini';

// =============================================================================

function KeyPresente(const EnvVar: string): Boolean;
begin
  Result := GetEnvironmentVariable(EnvVar) <> '';
end;

procedure Separador;
begin
  WriteLn(StringOfChar('─', 64));
end;

// =============================================================================
//  Construcción del grafo de cadena de suministro
// =============================================================================

function BuildSupplyChainGraph: TAiRagGraph;
var
  Grafo                     : TAiRagGraph;
  NodoMateria, NodoTech     : TAiRagGraphNode;
  NodoFabrica               : TAiRagGraphNode;
  NodoWidgetPro, NodoCompA  : TAiRagGraphNode;
  NodoDN, NodoDS            : TAiRagGraphNode;
  NodoEmpABC, NodoEmpXYZ    : TAiRagGraphNode;
begin
  Grafo := TAiRagGraph.Create(nil);

  // ── Proveedores ──────────────────────────────────────────────────────────
  NodoMateria := Grafo.AddNode('prov:materiasas',  'proveedor',    'MateriaSAS');
  NodoTech    := Grafo.AddNode('prov:techparts',   'proveedor',    'TechParts Ltd');

  // ── Fabricante ────────────────────────────────────────────────────────────
  NodoFabrica := Grafo.AddNode('fab:fabrica_norte', 'fabricante',  'Fábrica Norte');

  // ── Productos ─────────────────────────────────────────────────────────────
  NodoWidgetPro := Grafo.AddNode('prod:widget_pro', 'producto',    'Widget Pro');
  NodoCompA     := Grafo.AddNode('prod:comp_a',     'producto',    'Componente A');

  // ── Distribuidores ────────────────────────────────────────────────────────
  NodoDN := Grafo.AddNode('dist:dist_norte', 'distribuidor', 'Distribuidor Norte');
  NodoDS := Grafo.AddNode('dist:dist_sur',   'distribuidor', 'Distribuidor Sur');

  // ── Clientes ──────────────────────────────────────────────────────────────
  NodoEmpABC := Grafo.AddNode('cli:empresa_abc', 'cliente', 'Empresa ABC');
  NodoEmpXYZ := Grafo.AddNode('cli:empresa_xyz', 'cliente', 'Empresa XYZ');

  // ── Aristas de suministro ─────────────────────────────────────────────────
  Grafo.AddEdge(NodoMateria, NodoFabrica, 'sum:1', 'PROVEE', 'MateriaSAS provee Fábrica Norte');
  Grafo.AddEdge(NodoTech,    NodoFabrica, 'sum:2', 'PROVEE', 'TechParts provee Fábrica Norte');

  // ── Aristas de fabricación ────────────────────────────────────────────────
  Grafo.AddEdge(NodoFabrica, NodoWidgetPro, 'fab:1', 'FABRICA', 'Fábrica Norte fabrica Widget Pro');
  Grafo.AddEdge(NodoFabrica, NodoCompA,     'fab:2', 'FABRICA', 'Fábrica Norte fabrica Componente A');

  // ── Aristas de distribución ───────────────────────────────────────────────
  Grafo.AddEdge(NodoDN, NodoWidgetPro, 'dis:1', 'DISTRIBUYE', 'Dist Norte distribuye Widget Pro');
  Grafo.AddEdge(NodoDS, NodoWidgetPro, 'dis:2', 'DISTRIBUYE', 'Dist Sur distribuye Widget Pro');

  // ── Aristas de compra ─────────────────────────────────────────────────────
  Grafo.AddEdge(NodoEmpABC, NodoDN, 'com:1', 'COMPRA_DE', 'Empresa ABC compra de Dist Norte');
  Grafo.AddEdge(NodoEmpXYZ, NodoDS, 'com:2', 'COMPRA_DE', 'Empresa XYZ compra de Dist Sur');

  Result := Grafo;
end;

// =============================================================================
//  A) Demo de consultas GQL
// =============================================================================

procedure DemoGQL;
var
  Grafo    : TAiRagGraph;
  Resultado: string;
  Nodo     : TAiRagGraphNode;
  Vecinos  : TArray<TAiRagGraphNode>;
  N        : TAiRagGraphNode;
begin
  WriteLn('A) Consultas GQL sobre el grafo de cadena de suministro');
  WriteLn('   (Sin API key — operaciones estructurales en memoria)');
  Separador;

  Grafo := BuildSupplyChainGraph;
  try
    WriteLn;
    WriteLn('▶ Esquema del grafo:');
    Resultado := Grafo.ExecuteMakerGQL('SHOW LABELS');
    WriteLn(Resultado);
    Resultado := Grafo.ExecuteMakerGQL('SHOW EDGES');
    WriteLn(Resultado);

    WriteLn;
    WriteLn('▶ Todos los proveedores que abastecen fábricas:');
    Resultado := Grafo.ExecuteMakerGQL(
      'MATCH (p:proveedor)-[PROVEE]->(f:fabricante) RETURN p, f');
    WriteLn(Resultado);

    WriteLn;
    WriteLn('▶ Qué productos fabrica Fábrica Norte:');
    Resultado := Grafo.ExecuteMakerGQL(
      'MATCH (f:fabricante)-[FABRICA]->(p:producto) RETURN p');
    WriteLn(Resultado);

    WriteLn;
    WriteLn('▶ Ruta más corta: proveedor → cliente:');
    Resultado := Grafo.ExecuteMakerGQL(
      'SHORTEST PATH (s:proveedor) TO (c:cliente)');
    WriteLn(Resultado);

    WriteLn;
    WriteLn('▶ Vecinos directos de Empresa ABC (método GetNeighbors):');
    Nodo    := Grafo.FindNodeByID('cli:empresa_abc');
    Vecinos := Grafo.GetNeighbors(Nodo, gdBoth);
    for N in Vecinos do
      WriteLn('   → [', N.NodeLabel, '] ', N.Name);

    WriteLn;
    WriteLn(Format('Grafo: %d nodos, %d aristas',
      [Grafo.NodeCount, Grafo.EdgeCount]));

    // Guardar para uso en sección B
    Grafo.SaveToMakerAi(GRAFO_FILE);
    WriteLn('Grafo guardado en: ', GRAFO_FILE);
  finally
    Grafo.Free;
  end;
end;

// =============================================================================
//  B) Integración LLM — lenguaje natural → GQL → respuesta
// =============================================================================

procedure DemoLLM;
const
  PREGUNTA = '¿Qué proveedores abastecen a Fábrica Norte y qué productos fabrica?';
  ESQUEMA  =
    'Labels (nodos): proveedor, fabricante, producto, distribuidor, cliente' + sLineBreak +
    'Edges (aristas): PROVEE, FABRICA, DISTRIBUYE, COMPRA_DE' + sLineBreak +
    'Propiedad de nodo: name (string), ej: name = ''Fábrica Norte''' + sLineBreak +
    'Sintaxis: MATCH (a:label)-[r:EDGE]->(b:label) WHERE a.name = ''valor'' RETURN a, r, b';
var
  Grafo   : TAiRagGraph;
  Conn    : TAiChatConnection;
  GQLQuery: string;
  Contexto: string;
  Resp    : string;
begin
  WriteLn('B) Integración LLM — lenguaje natural → GQL → respuesta (OpenAI)');
  Separador;

  Grafo := BuildSupplyChainGraph;
  Conn  := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := 'OpenAI';
    Conn.Model      := MODEL_OPENAI;
    Conn.Params.Values['ApiKey']       := '@OPENAI_API_KEY';
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '256';

    WriteLn;
    WriteLn('Pregunta: ', PREGUNTA);
    WriteLn;

    // Paso 1 — El LLM genera la consulta GQL
    Conn.SystemPrompt.Text :=
      'Eres un experto en GQL (Graph Query Language) de MakerAI.' + sLineBreak +
      'Esquema del grafo:' + sLineBreak + ESQUEMA + sLineBreak +
      'REGLAS ESTRICTAS:' + sLineBreak +
      '- Genera UNA SOLA consulta MATCH en una sola línea.' + sLineBreak +
      '- Usa WHERE para filtrar por name: WHERE b.name = ''Fábrica Norte''' + sLineBreak +
      '- Devuelve solo la consulta GQL, sin explicaciones.';

    GQLQuery := Conn.AddMessageAndRun(PREGUNTA, 'user', []).Trim;
    // Quitar markdown code fences si el modelo las incluyó (ej: ```gql...```)
    if GQLQuery.StartsWith('`') then
    begin
      var LF := GQLQuery.IndexOf(#10);
      if LF > 0 then
        GQLQuery := GQLQuery.Substring(LF + 1).Trim;
      if GQLQuery.EndsWith('```') then
        GQLQuery := GQLQuery.Substring(0, GQLQuery.Length - 3).Trim;
    end;
    WriteLn('[GQL generado por OpenAI]: ', GQLQuery);
    WriteLn;

    // Paso 2 — Ejecutar la consulta en el grafo local
    Contexto := Grafo.ExecuteMakerGQL(GQLQuery);
    WriteLn('[Contexto del grafo]: ', Contexto);
    WriteLn;

    // Paso 3 — El LLM sintetiza la respuesta final
    Conn.NewChat;
    Conn.SystemPrompt.Text :=
      'Eres un asistente de análisis de cadena de suministro. ' +
      'Responde en español de forma clara y concisa, usando solo los datos del grafo proporcionados.';

    Resp := Conn.AddMessageAndRun(
      PREGUNTA + #13#10 +
      'Datos del grafo de conocimiento:' + #13#10 + Contexto,
      'user', []);

    WriteLn('[Respuesta final]:');
    WriteLn(Resp);
  finally
    Conn.Free;
    Grafo.Free;
  end;
end;

// =============================================================================
begin
  try
    WriteLn;
    WriteLn('════════════════════════════════════════════════════════════════');
    WriteLn('  Cap. 23 — RAG de Grafos: Cadena de suministro');
    WriteLn('  Motor   : TAiRagGraph (MakerAI in-memory)');
    WriteLn('  Lenguaje: GQL — Graph Query Language (ExecuteMakerGQL)');
    WriteLn('════════════════════════════════════════════════════════════════');
    WriteLn;

    DemoGQL;
    WriteLn;

    if KeyPresente('OPENAI_API_KEY') then
      DemoLLM
    else
      WriteLn('SKIP B: OPENAI_API_KEY no configurada.');

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('ERROR: ', E.ClassName, ' — ', E.Message);
    end;
  end;

  WriteLn;
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
