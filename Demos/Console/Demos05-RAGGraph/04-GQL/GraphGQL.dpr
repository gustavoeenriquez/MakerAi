program GraphGQL;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 05-RAGGraph / 04-GQL
// =============================================================================
// MakerGQL: lenguaje de consulta para grafos basado en GQL (ISO/IEC 39075:2024).
// Permite consultar el grafo con sintaxis declarativa MATCH/WHERE/RETURN.
//
// Conceptos que cubre:
//   - ExecuteMakerGQL(code): ejecuta una consulta GQL
//   - SHOW LABELS: lista etiquetas de nodos presentes
//   - SHOW EDGES: lista tipos de aristas
//   - MATCH (n:Label) RETURN n: buscar por etiqueta
//   - MATCH (a)-[r:LABEL]->(b) RETURN a, r, b: relaciones
//   - WHERE: filtros sobre propiedades
//   - SHORTEST PATH FROM (...) TO (...): camino mínimo
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
//  Grafo de películas y directores
// =============================================================================
procedure ConstruirGrafoPeliculas(RAG: TAiRagGraph);
var
  N: TAiRagGraphNode;
  // Directores
  Nolan, Spielberg, Kubrick, Cameron: TAiRagGraphNode;
  // Películas
  Inception, Interstellar, Schindler, ET, Odyssey,
  Shining, Titanic, Avatar: TAiRagGraphNode;
  // Géneros
  SciFi, Drama, Terror, Aventura: TAiRagGraphNode;
begin
  // Directores
  Nolan := RAG.AddNode('d001', 'DIRECTOR', 'Christopher Nolan');
  Nolan.Text := 'Christopher Nolan, director britanico de thrillers cientificos complejos.';
  Nolan.Properties['nacionalidad'] := 'Britanico';

  Spielberg := RAG.AddNode('d002', 'DIRECTOR', 'Steven Spielberg');
  Spielberg.Text := 'Steven Spielberg, director americano de aventura, drama y ciencia ficcion.';
  Spielberg.Properties['nacionalidad'] := 'Americano';

  Kubrick := RAG.AddNode('d003', 'DIRECTOR', 'Stanley Kubrick');
  Kubrick.Text := 'Stanley Kubrick, director visionario de ciencia ficcion y terror.';
  Kubrick.Properties['nacionalidad'] := 'Americano';

  Cameron := RAG.AddNode('d004', 'DIRECTOR', 'James Cameron');
  Cameron.Text := 'James Cameron, director de thrillers de accion y ciencia ficcion visual.';
  Cameron.Properties['nacionalidad'] := 'Canadiense';

  // Películas
  Inception := RAG.AddNode('p001', 'PELICULA', 'Inception');
  Inception.Text := 'Inception 2010: sueños dentro de sueños, thriller de ciencia ficcion.';
  Inception.Properties['año']  := 2010;
  Inception.Properties['nota'] := 8.8;

  Interstellar := RAG.AddNode('p002', 'PELICULA', 'Interstellar');
  Interstellar.Text := 'Interstellar 2014: viaje espacial y agujeros de gusano, drama epico.';
  Interstellar.Properties['año']  := 2014;
  Interstellar.Properties['nota'] := 8.6;

  Schindler := RAG.AddNode('p003', 'PELICULA', 'La Lista de Schindler');
  Schindler.Text := 'La Lista de Schindler 1993: drama historico del Holocausto.';
  Schindler.Properties['año']  := 1993;
  Schindler.Properties['nota'] := 8.9;

  ET := RAG.AddNode('p004', 'PELICULA', 'E.T.');
  ET.Text := 'E.T. el extraterrestre 1982: aventura de un niño con un alien amigable.';
  ET.Properties['año']  := 1982;
  ET.Properties['nota'] := 7.9;

  Odyssey := RAG.AddNode('p005', 'PELICULA', '2001: A Space Odyssey');
  Odyssey.Text := '2001: A Space Odyssey 1968, viaje espacial filosofico y monolito misterioso.';
  Odyssey.Properties['año']  := 1968;
  Odyssey.Properties['nota'] := 8.3;

  Shining := RAG.AddNode('p006', 'PELICULA', 'The Shining');
  Shining.Text := 'The Shining 1980: hotel aislado y locura, terror psicologico.';
  Shining.Properties['año']  := 1980;
  Shining.Properties['nota'] := 8.4;

  Titanic := RAG.AddNode('p007', 'PELICULA', 'Titanic');
  Titanic.Text := 'Titanic 1997: romance epico en el barco que se hundio.';
  Titanic.Properties['año']  := 1997;
  Titanic.Properties['nota'] := 7.9;

  Avatar := RAG.AddNode('p008', 'PELICULA', 'Avatar');
  Avatar.Text := 'Avatar 2009: planeta Pandora y lucha por los recursos naturales.';
  Avatar.Properties['año']  := 2009;
  Avatar.Properties['nota'] := 7.9;

  // Géneros
  SciFi    := RAG.AddNode('g001', 'GENERO', 'Ciencia Ficcion');
  Drama    := RAG.AddNode('g002', 'GENERO', 'Drama');
  Terror   := RAG.AddNode('g003', 'GENERO', 'Terror');
  Aventura := RAG.AddNode('g004', 'GENERO', 'Aventura');

  SciFi.Text    := 'Ciencia ficcion: genero de tecnologia futurista y espacios alternativos.';
  Drama.Text    := 'Drama: genero de emociones profundas y conflictos humanos.';
  Terror.Text   := 'Terror: genero de miedo, suspenso y ambientes oscuros.';
  Aventura.Text := 'Aventura: genero de viajes, accion y descubrimientos.';

  // Director → Película
  RAG.AddEdge(Nolan,     Inception,     'r001', 'DIRIGIO', 'dirigió', 0.9);
  RAG.AddEdge(Nolan,     Interstellar,  'r002', 'DIRIGIO', 'dirigió', 0.9);
  RAG.AddEdge(Spielberg, Schindler,     'r003', 'DIRIGIO', 'dirigió', 0.9);
  RAG.AddEdge(Spielberg, ET,            'r004', 'DIRIGIO', 'dirigió', 0.9);
  RAG.AddEdge(Kubrick,   Odyssey,       'r005', 'DIRIGIO', 'dirigió', 0.9);
  RAG.AddEdge(Kubrick,   Shining,       'r006', 'DIRIGIO', 'dirigió', 0.9);
  RAG.AddEdge(Cameron,   Titanic,       'r007', 'DIRIGIO', 'dirigió', 0.9);
  RAG.AddEdge(Cameron,   Avatar,        'r008', 'DIRIGIO', 'dirigió', 0.9);

  // Película → Género
  RAG.AddEdge(Inception,    SciFi,    'g001', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(Interstellar, SciFi,    'g002', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(Interstellar, Drama,    'g003', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(Schindler,    Drama,    'g004', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(ET,           Aventura, 'g005', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(Odyssey,      SciFi,    'g006', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(Shining,      Terror,   'g007', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(Titanic,      Drama,    'g008', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(Avatar,       SciFi,    'g009', 'ES_DE_GENERO', 'es de género');
  RAG.AddEdge(Avatar,       Aventura, 'g010', 'ES_DE_GENERO', 'es de género');

  Writeln(Format('Grafo listo: %d nodos, %d aristas.', [RAG.NodeCount, RAG.EdgeCount]));
  Writeln;
end;

// =============================================================================
//  Ejecuta una consulta GQL y muestra el resultado
// =============================================================================
procedure EjecutarGQL(RAG: TAiRagGraph; const Descripcion, Query: String);
var
  Resultado: String;
begin
  Writeln(StringOfChar('-', 60));
  Writeln('Consulta: ', Descripcion);
  Writeln('GQL: ', Query);
  Writeln;
  try
    Resultado := RAG.ExecuteMakerGQL(Query);
    Writeln(Resultado);
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
  Writeln;
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  EmbConn: TAiEmbeddingConnection;
  RAG    : TAiRagGraph;
begin
  Writeln('=== GraphGQL ===');
  Writeln('Driver: ', EMB_DRIVER, ' / ', EMB_MODEL);
  Writeln;

  EmbConn := TAiEmbeddingConnection.Create(nil);
  RAG     := TAiRagGraph.Create(nil);
  try
    EmbConn.DriverName := EMB_DRIVER;
    EmbConn.Model      := EMB_MODEL;
    EmbConn.ApiKey     := EMB_API_KEY;

    RAG.Embeddings := EmbConn.AiEmbeddings;

    ConstruirGrafoPeliculas(RAG);

    Writeln('Generando embeddings...');
    RAG.RebuildIndexes;
    Writeln;

    // ── Comandos de exploración ───────────────────────────────────────────
    EjecutarGQL(RAG,
      'Listar etiquetas de nodos',
      'SHOW LABELS');

    EjecutarGQL(RAG,
      'Listar tipos de aristas',
      'SHOW EDGES');

    // ── Consultas MATCH básicas ───────────────────────────────────────────
    EjecutarGQL(RAG,
      'Todos los directores',
      'MATCH (d:DIRECTOR) RETURN d');

    EjecutarGQL(RAG,
      'Peliculas de Christopher Nolan',
      'MATCH (d:DIRECTOR)-[r:DIRIGIO]->(p:PELICULA) WHERE d.nombre = "Christopher Nolan" RETURN p');

    // ── Consultas con WHERE ───────────────────────────────────────────────
    EjecutarGQL(RAG,
      'Peliculas con nota > 8.5',
      'MATCH (p:PELICULA) WHERE p.nota > 8.5 RETURN p');

    EjecutarGQL(RAG,
      'Peliculas anteriores a 1990',
      'MATCH (p:PELICULA) WHERE p.año < 1990 RETURN p');

    // ── Relaciones completas ──────────────────────────────────────────────
    EjecutarGQL(RAG,
      'Peliculas de ciencia ficcion con su director',
      'MATCH (d:DIRECTOR)-[r:DIRIGIO]->(p:PELICULA)-[g:ES_DE_GENERO]->(gen:GENERO) ' +
      'WHERE gen.nombre = "Ciencia Ficcion" RETURN d, p');

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
