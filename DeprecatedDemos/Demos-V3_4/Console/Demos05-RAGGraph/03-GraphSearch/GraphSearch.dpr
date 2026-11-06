program GraphSearch;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 05-RAGGraph / 03-GraphSearch
// =============================================================================
// Demuestra las diferentes estrategias de búsqueda en un Knowledge Graph.
//
// Conceptos que cubre:
//   - Search() → TArray<TAiRagGraphNode>: resultado como objetos
//   - SearchText() → String: resultado formateado como texto
//   - depth=0: solo nodos ancla; depth=1: nodos + vecinos directos
//   - Diferencia entre búsqueda superficial y profunda
//   - SaveToMakerAi / SaveToFile: persistencia del grafo
//   - FindNodesByLabel: filtro por tipo de entidad
//   - CountNodesByLabel / CountEdgesByLabel
// =============================================================================

uses
  System.SysUtils,
  System.IOUtils,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Graph.Core,
  uMakerAi.RAG.Vectors;

const
  EMB_DRIVER  = 'OpenAI';
  EMB_MODEL   = 'text-embedding-3-small';
  EMB_API_KEY = '@OPENAI_API_KEY';

  GRAPH_FILE  = 'historia_tech.mkai';

// =============================================================================
//  Grafo de historia de la tecnología
// =============================================================================
procedure ConstruirGrafoHistoria(RAG: TAiRagGraph);
var
  N: TAiRagGraphNode;
  // Décadas
  D70, D80, D90, D00, D10, D20: TAiRagGraphNode;
begin
  // Eventos históricos de tecnología
  N := RAG.AddNode('ev001', 'EVENTO', 'Creacion de UNIX');
  N.Text := 'UNIX fue creado en Bell Labs en 1969, base de Linux y macOS.';
  N.Properties['año'] := 1969;

  N := RAG.AddNode('ev002', 'EVENTO', 'Lanzamiento de Apple II');
  N.Text := 'Apple II en 1977, primer computador personal de éxito masivo.';
  N.Properties['año'] := 1977;

  N := RAG.AddNode('ev003', 'EVENTO', 'Creacion de Internet TCP/IP');
  N.Text := 'TCP/IP en 1983 estandarizó las comunicaciones de Internet.';
  N.Properties['año'] := 1983;

  N := RAG.AddNode('ev004', 'EVENTO', 'Lanzamiento de Windows');
  N.Text := 'Windows 1.0 en 1985, interfaz gráfica de Microsoft.';
  N.Properties['año'] := 1985;

  N := RAG.AddNode('ev005', 'EVENTO', 'Creacion de World Wide Web');
  N.Text := 'Tim Berners-Lee creó la WWW en 1991 en el CERN.';
  N.Properties['año'] := 1991;

  N := RAG.AddNode('ev006', 'EVENTO', 'Lanzamiento de Linux');
  N.Text := 'Linux kernel creado por Linus Torvalds en 1991, OS de codigo abierto.';
  N.Properties['año'] := 1991;

  N := RAG.AddNode('ev007', 'EVENTO', 'Fundacion de Google');
  N.Text := 'Google fundado en 1998 por Page y Brin, revolucionó la búsqueda web.';
  N.Properties['año'] := 1998;

  N := RAG.AddNode('ev008', 'EVENTO', 'Creacion de Wikipedia');
  N.Text := 'Wikipedia fundada en 2001, enciclopedia colaborativa en internet.';
  N.Properties['año'] := 2001;

  N := RAG.AddNode('ev009', 'EVENTO', 'Lanzamiento de iPhone');
  N.Text := 'iPhone de Apple en 2007 revolucionó los smartphones y la movilidad.';
  N.Properties['año'] := 2007;

  N := RAG.AddNode('ev010', 'EVENTO', 'Lanzamiento de ChatGPT');
  N.Text := 'ChatGPT lanzado en noviembre 2022, popularizó los LLMs masivamente.';
  N.Properties['año'] := 2022;

  // Décadas como nodos temporales
  D70 := RAG.AddNode('dec70', 'DECADA', '1970s');
  D70.Text := 'Década de 1970: inicio de la computación personal y UNIX.';
  D80 := RAG.AddNode('dec80', 'DECADA', '1980s');
  D80.Text := 'Década de 1980: PC, GUIs y el inicio de la red global.';
  D90 := RAG.AddNode('dec90', 'DECADA', '1990s');
  D90.Text := 'Década de 1990: Internet, web, Linux y software open source.';
  D00 := RAG.AddNode('dec00', 'DECADA', '2000s');
  D00.Text := 'Década de 2000: web 2.0, redes sociales y cloud computing.';
  D10 := RAG.AddNode('dec10', 'DECADA', '2010s');
  D10.Text := 'Década de 2010: móviles, big data, deep learning y smartphones.';
  D20 := RAG.AddNode('dec20', 'DECADA', '2020s');
  D20.Text := 'Década de 2020: IA generativa, LLMs y transformers.';

  // Relaciones evento → décadas
  var Ev001 := RAG.FindNodeByID('ev001');
  var Ev002 := RAG.FindNodeByID('ev002');
  var Ev003 := RAG.FindNodeByID('ev003');
  var Ev004 := RAG.FindNodeByID('ev004');
  var Ev005 := RAG.FindNodeByID('ev005');
  var Ev006 := RAG.FindNodeByID('ev006');
  var Ev007 := RAG.FindNodeByID('ev007');
  var Ev008 := RAG.FindNodeByID('ev008');
  var Ev009 := RAG.FindNodeByID('ev009');
  var Ev010 := RAG.FindNodeByID('ev010');

  RAG.AddEdge(Ev001, D70, 're001', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev002, D70, 're002', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev003, D80, 're003', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev004, D80, 're004', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev005, D90, 're005', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev006, D90, 're006', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev007, D90, 're007', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev008, D00, 're008', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev009, D00, 're009', 'OCURRIO_EN', 'ocurrió en');
  RAG.AddEdge(Ev010, D20, 're010', 'OCURRIO_EN', 'ocurrió en');

  Writeln(Format('Grafo: %d nodos, %d aristas.', [RAG.NodeCount, RAG.EdgeCount]));
end;

// =============================================================================
//  Compara búsqueda superficial vs profunda
// =============================================================================
procedure CompararProfundidad(RAG: TAiRagGraph; const Query: String);
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Consulta: "', Query, '"');
  Writeln;

  Writeln('  depth=0 (solo nodos ancla):');
  var R0 := RAG.SearchText(Query, 0, False, 2, 0.2);
  Writeln(R0);

  Writeln('  depth=1 (nodos + vecinos):');
  var R1 := RAG.SearchText(Query, 1, False, 2, 0.2);
  Writeln(R1);
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn: TAiEmbeddingConnection;
  RAG    : TAiRagGraph;
  Nodes  : TArray<TAiRagGraphNode>;
  Node   : TAiRagGraphNode;
begin
  Writeln('=== GraphSearch ===');
  Writeln('Driver: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln;

  EmbConn := TAiEmbeddingConnection.Create(nil);
  RAG     := TAiRagGraph.Create(nil);
  try
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    RAG.Embeddings := EmbConn.AiEmbeddings;

    ConstruirGrafoHistoria(RAG);

    Writeln('Generando embeddings...');
    RAG.RebuildIndexes;
    Writeln;

    // Estadísticas por tipo
    Writeln(StringOfChar('=', 60));
    Writeln('Estadisticas:');
    Writeln('  Eventos: ',   RAG.CountNodesByLabel('EVENTO'));
    Writeln('  Decadas: ',   RAG.CountNodesByLabel('DECADA'));
    Writeln('  Relaciones: ', RAG.EdgeCount);
    Writeln;

    // Listar nodos por etiqueta
    Writeln(StringOfChar('-', 60));
    Writeln('Todos los nodos DECADA:');
    Nodes := RAG.FindNodesByLabel('DECADA');
    for Node in Nodes do
      Writeln('  - ', Node.Name);
    Writeln;

    // Búsqueda a diferentes profundidades
    CompararProfundidad(RAG, 'sistemas operativos de codigo abierto');
    CompararProfundidad(RAG, 'inteligencia artificial y chatbots');

    // Búsqueda como array de nodos
    Writeln(StringOfChar('-', 60));
    Writeln('Resultado como array de nodos:');
    var ResultNodes := RAG.Search('busqueda en internet', 0, 3, 0.2);
    for var N in ResultNodes do
      Writeln(Format('  [%s] %s', [N.NodeLabel, N.Name]));
    Writeln;

    // Guardar grafo
    Writeln(StringOfChar('-', 60));
    Writeln('Guardando grafo en: ', GRAPH_FILE);
    RAG.SaveToMakerAi(GRAPH_FILE, True);
    Writeln('Guardado OK.');
    TFile.Delete(GRAPH_FILE);

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
