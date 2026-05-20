program GraphBuilder;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 05-RAGGraph / 02-GraphBuilder
// =============================================================================
// Construye un grafo más complejo y navega relaciones entre nodos.
// Muestra cómo recorrer vecinos, encontrar caminos y analizar el grafo.
//
// Conceptos que cubre:
//   - Grafo de dominio científico (disciplinas, conceptos, autores)
//   - FindNodesByLabel: buscar nodos por etiqueta
//   - FindNodeByName: buscar nodo por nombre
//   - GetNeighbors: vecinos de un nodo
//   - GetShortestPath: camino mínimo entre nodos
//   - GetNodesByDegree: nodos más conectados
//   - Búsqueda semántica con profundidad (depth > 0)
// =============================================================================

uses
  System.SysUtils,
  System.Generics.Collections,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Graph.Core,
  uMakerAi.RAG.Vectors;

const
  EMB_DRIVER  = 'OpenAI';
  EMB_MODEL   = 'text-embedding-3-small';
  EMB_API_KEY = '@OPENAI_API_KEY';

// =============================================================================
//  Grafo de ciencias de la computación
// =============================================================================
procedure ConstruirGrafoCiencias(RAG: TAiRagGraph);
var
  // Disciplinas
  IA, ML, DL, NLP, CV: TAiRagGraphNode;
  // Técnicas
  NN, CNN, RNN, Transformer, RAGTech: TAiRagGraphNode;
  // Frameworks
  TensorFlow, PyTorch, Keras: TAiRagGraphNode;
begin
  Writeln('Construyendo grafo de Ciencias de la Computacion...');

  // --- Disciplinas ---
  IA := RAG.AddNode('d001', 'DISCIPLINA', 'Inteligencia Artificial');
  IA.Text := 'Inteligencia Artificial: rama de la informatica que simula capacidades cognitivas.';

  ML := RAG.AddNode('d002', 'DISCIPLINA', 'Machine Learning');
  ML.Text := 'Machine Learning: subcampo de IA que aprende patrones de datos.';

  DL := RAG.AddNode('d003', 'DISCIPLINA', 'Deep Learning');
  DL.Text := 'Deep Learning: ML con redes neuronales profundas de múltiples capas.';

  NLP := RAG.AddNode('d004', 'DISCIPLINA', 'Procesamiento de Lenguaje Natural');
  NLP.Text := 'NLP: rama de IA para entender y generar texto en lenguaje humano.';

  CV := RAG.AddNode('d005', 'DISCIPLINA', 'Vision por Computadora');
  CV.Text := 'Vision por Computadora: IA para interpretar y analizar imágenes y video.';

  // --- Técnicas ---
  NN := RAG.AddNode('t001', 'TECNICA', 'Redes Neuronales');
  NN.Text := 'Redes neuronales artificiales inspiradas en el cerebro humano.';

  CNN := RAG.AddNode('t002', 'TECNICA', 'CNN');
  CNN.Text := 'Convolutional Neural Networks, especializadas en análisis de imágenes.';

  RNN := RAG.AddNode('t003', 'TECNICA', 'RNN');
  RNN.Text := 'Recurrent Neural Networks, para secuencias y datos temporales.';

  Transformer := RAG.AddNode('t004', 'TECNICA', 'Transformer');
  Transformer.Text := 'Arquitectura Transformer con atención, base de GPT y BERT.';

  RAGTech := RAG.AddNode('t005', 'TECNICA', 'RAG');
  RAGTech.Text := 'Retrieval Augmented Generation: combina búsqueda vectorial con LLMs.';

  // --- Frameworks ---
  TensorFlow := RAG.AddNode('f001', 'FRAMEWORK', 'TensorFlow');
  TensorFlow.Text := 'TensorFlow: framework de ML de Google para producción.';

  PyTorch := RAG.AddNode('f002', 'FRAMEWORK', 'PyTorch');
  PyTorch.Text := 'PyTorch: framework de ML de Meta, popular en investigación.';

  Keras := RAG.AddNode('f003', 'FRAMEWORK', 'Keras');
  Keras.Text := 'Keras: API de alto nivel para construir redes neuronales.';

  // --- Relaciones jerárquicas ---
  RAG.AddEdge(IA, ML,  'h001', 'INCLUYE', 'incluye');
  RAG.AddEdge(ML, DL,  'h002', 'INCLUYE', 'incluye');
  RAG.AddEdge(DL, NLP, 'h003', 'HABILITA', 'habilita');
  RAG.AddEdge(DL, CV,  'h004', 'HABILITA', 'habilita');

  // --- Relaciones técnica → disciplina ---
  RAG.AddEdge(NN,          ML,  'u001', 'USADA_EN', 'usada en');
  RAG.AddEdge(CNN,         CV,  'u002', 'USADA_EN', 'usada en');
  RAG.AddEdge(RNN,         NLP, 'u003', 'USADA_EN', 'usada en');
  RAG.AddEdge(Transformer, NLP, 'u004', 'USADA_EN', 'usada en');
  RAG.AddEdge(RAGTech,     NLP, 'u005', 'USADA_EN', 'usada en');

  // --- Relaciones framework → técnica ---
  RAG.AddEdge(TensorFlow, NN,          'i001', 'IMPLEMENTA', 'implementa');
  RAG.AddEdge(PyTorch,    Transformer, 'i002', 'IMPLEMENTA', 'implementa');
  RAG.AddEdge(Keras,      CNN,         'i003', 'IMPLEMENTA', 'implementa');

  Writeln(Format('Grafo listo: %d nodos, %d aristas.', [RAG.NodeCount, RAG.EdgeCount]));
  Writeln;
end;

// =============================================================================
//  Muestra vecinos de un nodo
// =============================================================================
procedure MostrarVecinos(RAG: TAiRagGraph; const NodeName, NodeLabel: String);
var
  Node     : TAiRagGraphNode;
  Vecinos  : TArray<TAiRagGraphNode>;
  Vecino   : TAiRagGraphNode;
begin
  Node := RAG.FindNodeByName(NodeName, NodeLabel);
  if not Assigned(Node) then
  begin
    Writeln('Nodo no encontrado: ', NodeName);
    Exit;
  end;

  Writeln(StringOfChar('-', 60));
  Writeln('Vecinos de "', Node.Name, '" (', Node.NodeLabel, '):');

  Vecinos := RAG.GetNeighbors(Node, gdOutgoing);
  if Length(Vecinos) = 0 then
    Writeln('  (sin vecinos salientes)')
  else
    for Vecino in Vecinos do
      Writeln(Format('  --> %s (%s)', [Vecino.Name, Vecino.NodeLabel]));
end;

// =============================================================================
//  Camino mínimo entre dos nodos
// =============================================================================
procedure MostrarCaminoMinimo(RAG: TAiRagGraph;
  const NombreA, LabelA, NombreB, LabelB: String);
var
  NodeA, NodeB: TAiRagGraphNode;
  Camino      : TArray<TObject>;
  I           : Integer;
  Obj         : TObject;
begin
  NodeA := RAG.FindNodeByName(NombreA, LabelA);
  NodeB := RAG.FindNodeByName(NombreB, LabelB);

  if not Assigned(NodeA) or not Assigned(NodeB) then
  begin
    Writeln('Nodos no encontrados para camino minimo.');
    Exit;
  end;

  Writeln(StringOfChar('-', 60));
  Writeln(Format('Camino minimo: "%s" --> "%s"', [NombreA, NombreB]));

  Camino := RAG.GetShortestPath(NodeA, NodeB);
  if Length(Camino) = 0 then
  begin
    Writeln('  (no hay camino)');
    Exit;
  end;

  for I := 0 to High(Camino) do
  begin
    Obj := Camino[I];
    if Obj is TAiRagGraphNode then
      Write(Format('[%s]', [(Obj as TAiRagGraphNode).Name]))
    else if Obj is TAiRagGraphEdge then
      Write(Format('--%s-->', [(Obj as TAiRagGraphEdge).EdgeLabel]));
  end;
  Writeln;
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn  : TAiEmbeddingConnection;
  RAG      : TAiRagGraph;
  TopNodes : TArray<TAiRagGraphNode>;
  Node     : TAiRagGraphNode;
begin
  Writeln('=== GraphBuilder ===');
  Writeln('Driver: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln;

  EmbConn := TAiEmbeddingConnection.Create(nil);
  RAG     := TAiRagGraph.Create(nil);
  try
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    RAG.Embeddings := EmbConn.AiEmbeddings;

    ConstruirGrafoCiencias(RAG);

    Writeln('Generando embeddings...');
    RAG.RebuildIndexes;
    Writeln('Listo.');
    Writeln;

    // Vecinos
    MostrarVecinos(RAG, 'Deep Learning', 'DISCIPLINA');
    MostrarVecinos(RAG, 'PyTorch', 'FRAMEWORK');
    Writeln;

    // Camino mínimo
    MostrarCaminoMinimo(RAG,
      'PyTorch', 'FRAMEWORK',
      'Procesamiento de Lenguaje Natural', 'DISCIPLINA');
    Writeln;

    // Nodos más conectados
    Writeln(StringOfChar('-', 60));
    Writeln('Top 3 nodos mas conectados:');
    TopNodes := RAG.GetNodesByDegree(3);
    for Node in TopNodes do
      Writeln(Format('  %s (%s)', [Node.Name, Node.NodeLabel]));
    Writeln;

    // Búsqueda semántica con depth=1 (expande vecinos)
    Writeln(StringOfChar('-', 60));
    Writeln('Busqueda semantica con expansion (depth=1):');
    Writeln('"generacion de texto con transformers"');
    Writeln(RAG.SearchText('generacion de texto con transformers', 1, False, 3, 0.2));

  finally
    RAG.Free;
    EmbConn.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
