program BasicGraph;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 05-RAGGraph / 01-BasicGraph
// =============================================================================
// Grafo de conocimiento básico: nodos, aristas y búsqueda semántica.
// Un Knowledge Graph combina vectores semánticos con relaciones estructuradas.
//
// Conceptos que cubre:
//   - TAiRagGraph: componente principal de grafo vectorial
//   - AddNode(id, label, name): agrega entidades al grafo
//   - AddEdge(from, to, id, label, name): agrega relaciones
//   - Node.Text := '...': texto para búsqueda semántica
//   - Node.Properties['key'] := value: metadatos del nodo
//   - RebuildIndexes: construye índices vectoriales y lexicales
//   - Search(prompt, depth, limit, precision): búsqueda semántica
//   - SearchText(prompt, depth, showProps, limit, precision): resultado texto
// =============================================================================

uses
  System.SysUtils,
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
//  Construye un grafo de empresas tecnológicas
// =============================================================================
procedure ConstruirGrafo(RAG: TAiRagGraph);
var
  Apple, Microsoft, Google, OpenAI: TAiRagGraphNode;
  Jobs, Gates, Page, Altman  : TAiRagGraphNode;
  IPhone, Windows, Search, GPT: TAiRagGraphNode;
begin
  Writeln('Creando nodos de personas...');

  // --- PERSONAS ---
  Jobs := RAG.AddNode('p001', 'PERSONA', 'Steve Jobs');
  Jobs.Text := 'Steve Jobs, visionario y cofundador de Apple, creó el iPhone y la Mac.';
  Jobs.Properties['nacimiento'] := 1955;
  Jobs.Properties['pais']       := 'USA';

  Gates := RAG.AddNode('p002', 'PERSONA', 'Bill Gates');
  Gates.Text := 'Bill Gates, cofundador de Microsoft y pionero del software personal.';
  Gates.Properties['nacimiento'] := 1955;
  Gates.Properties['pais']       := 'USA';

  Page := RAG.AddNode('p003', 'PERSONA', 'Larry Page');
  Page.Text := 'Larry Page, cofundador de Google y creador del algoritmo PageRank.';
  Page.Properties['nacimiento'] := 1973;
  Page.Properties['pais']       := 'USA';

  Altman := RAG.AddNode('p004', 'PERSONA', 'Sam Altman');
  Altman.Text := 'Sam Altman, CEO de OpenAI, impulsor de ChatGPT y los LLMs modernos.';
  Altman.Properties['nacimiento'] := 1985;
  Altman.Properties['pais']       := 'USA';

  Writeln('Creando nodos de empresas...');

  // --- EMPRESAS ---
  Apple := RAG.AddNode('e001', 'EMPRESA', 'Apple Inc.');
  Apple.Text := 'Apple Inc., empresa creadora del iPhone, Mac y el ecosistema iOS.';
  Apple.Properties['fundacion'] := 1976;
  Apple.Properties['sector']    := 'Tecnologia';

  Microsoft := RAG.AddNode('e002', 'EMPRESA', 'Microsoft');
  Microsoft.Text := 'Microsoft, empresa creadora de Windows, Office y Azure cloud.';
  Microsoft.Properties['fundacion'] := 1975;
  Microsoft.Properties['sector']    := 'Tecnologia';

  Google := RAG.AddNode('e003', 'EMPRESA', 'Google');
  Google.Text := 'Google, empresa de búsqueda en internet, Android y computación en la nube.';
  Google.Properties['fundacion'] := 1998;
  Google.Properties['sector']    := 'Tecnologia';

  OpenAI := RAG.AddNode('e004', 'EMPRESA', 'OpenAI');
  OpenAI.Text := 'OpenAI, empresa de inteligencia artificial creadora de GPT y ChatGPT.';
  OpenAI.Properties['fundacion'] := 2015;
  OpenAI.Properties['sector']    := 'IA';

  Writeln('Creando nodos de productos...');

  // --- PRODUCTOS ---
  IPhone := RAG.AddNode('pr001', 'PRODUCTO', 'iPhone');
  IPhone.Text := 'iPhone, smartphone revolucionario que transformó la industria móvil.';
  IPhone.Properties['lanzamiento'] := 2007;

  Windows := RAG.AddNode('pr002', 'PRODUCTO', 'Windows');
  Windows.Text := 'Windows, sistema operativo más usado en computadoras personales.';
  Windows.Properties['lanzamiento'] := 1985;

  Search := RAG.AddNode('pr003', 'PRODUCTO', 'Google Search');
  Search.Text := 'Google Search, motor de búsqueda más utilizado del mundo.';
  Search.Properties['lanzamiento'] := 1998;

  GPT := RAG.AddNode('pr004', 'PRODUCTO', 'ChatGPT');
  GPT.Text := 'ChatGPT, asistente de inteligencia artificial basado en GPT-4.';
  GPT.Properties['lanzamiento'] := 2022;

  Writeln('Creando relaciones...');

  // --- RELACIONES persona → empresa ---
  RAG.AddEdge(Jobs,   Apple,     'r001', 'COFUNDO', 'cofundó', 0.9);
  RAG.AddEdge(Gates,  Microsoft, 'r002', 'COFUNDO', 'cofundó', 0.9);
  RAG.AddEdge(Page,   Google,    'r003', 'COFUNDO', 'cofundó', 0.9);
  RAG.AddEdge(Altman, OpenAI,    'r004', 'DIRIGE',  'dirige',  0.8);

  // --- RELACIONES empresa → producto ---
  RAG.AddEdge(Apple,     IPhone,  'r005', 'CREO', 'creó', 0.9);
  RAG.AddEdge(Microsoft, Windows, 'r006', 'CREO', 'creó', 0.9);
  RAG.AddEdge(Google,    Search,  'r007', 'CREO', 'creó', 0.9);
  RAG.AddEdge(OpenAI,    GPT,     'r008', 'CREO', 'creó', 0.9);

  // --- RELACIONES entre empresas ---
  RAG.AddEdge(Microsoft, OpenAI, 'r009', 'INVIRTIO_EN', 'invirtió en', 0.7);

  Writeln(Format('Grafo creado: %d nodos, %d aristas.',
    [RAG.NodeCount, RAG.EdgeCount]));
  Writeln;
end;

// =============================================================================
//  Búsqueda semántica en el grafo
// =============================================================================
procedure BuscarEnGrafo(RAG: TAiRagGraph; const Query: String);
var
  Resultado: String;
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Busqueda: "', Query, '"');
  Resultado := RAG.SearchText(Query, 0, True, 3, 0.2);
  Writeln(Resultado);
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn: TAiEmbeddingConnection;
  RAG    : TAiRagGraph;
begin
  Writeln('=== BasicGraph ===');
  Writeln('Driver: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln;

  EmbConn := TAiEmbeddingConnection.Create(nil);
  RAG     := TAiRagGraph.Create(nil);
  try
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    RAG.Embeddings := EmbConn.AiEmbeddings;

    // Construir el grafo
    ConstruirGrafo(RAG);

    // Generar embeddings e índices
    Writeln('Generando embeddings y construyendo indices...');
    RAG.RebuildIndexes;
    Writeln('Indices listos.');
    Writeln;

    // Búsquedas semánticas
    BuscarEnGrafo(RAG, 'fundadores de empresas tecnologicas');
    BuscarEnGrafo(RAG, 'inteligencia artificial y LLMs');
    BuscarEnGrafo(RAG, 'sistemas operativos y software');
    BuscarEnGrafo(RAG, 'busqueda en internet y publicidad');

    // Estadísticas
    Writeln(StringOfChar('=', 60));
    Writeln('Estadisticas del grafo:');
    Writeln('  Total de nodos: ', RAG.NodeCount);
    Writeln('  Total de aristas: ', RAG.EdgeCount);

    // Etiuetas únicas de nodos
    var Labels := RAG.GetUniqueNodeLabels;
    Write('  Tipos de nodos: ');
    for var L in Labels do Write(L, ' ');
    Writeln;

    var EdgeLabels := RAG.GetUniqueEdgeLabels;
    Write('  Tipos de aristas: ');
    for var EL in EdgeLabels do Write(EL, ' ');
    Writeln;

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
