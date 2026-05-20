program Similarity;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 03-Embeddings / 02-Similarity
// =============================================================================
// Exploración profunda de la similitud semántica entre textos.
// Construye una mini-matriz de similitud y encuentra el más similar
// a una consulta usando búsqueda lineal.
//
// Conceptos que cubre:
//   - Similitud coseno, distancia euclidiana, producto punto
//   - Matriz NxN de similitudes entre un corpus de textos
//   - Búsqueda del top-K más similares a una consulta
//   - Caso de uso: recomendación, deduplicación, clustering básico
// =============================================================================

uses
  System.SysUtils,
  System.Math,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.Embeddings.Gemini,
  uMakerAi.Embeddings.Ollama;

const
  DRIVER  = 'OpenAI';
  MODEL   = 'text-embedding-3-small';
  API_KEY = '@OPENAI_API_KEY';

// Corpus de ejemplo — frases sobre tecnología y otros temas
const
  CORPUS: array[0..7] of String = (
    'Machine learning models learn patterns from data',
    'Deep neural networks have many hidden layers',
    'Python is the most popular language for AI development',
    'The Eiffel Tower is located in Paris, France',
    'Photosynthesis converts sunlight into chemical energy',
    'Gradient descent optimizes neural network weights',
    'The Amazon rainforest produces 20% of the world oxygen',
    'Transformers use attention mechanisms for NLP tasks'
  );

// =============================================================================
//  Imprime los resultados de similitud ordenados
// =============================================================================
procedure PrintTopK(const Query: String; const Corpus: array of String;
  const Embeddings: array of TAiEmbeddingData; const QEmb: TAiEmbeddingData; K: Integer);
var
  Scores: array[0..7] of Double;
  I, J  : Integer;
  Temp  : Double;
  TempI : Integer;
  Idx   : array[0..7] of Integer;
begin
  for I := 0 to High(Corpus) do
  begin
    Scores[I] := TAiEmbeddingsCore.CosineSimilarity(QEmb, Embeddings[I]);
    Idx[I]    := I;
  end;

  // Bubble sort descendente
  for I := 0 to High(Corpus) - 1 do
    for J := 0 to High(Corpus) - I - 1 do
      if Scores[Idx[J]] < Scores[Idx[J+1]] then
      begin
        TempI := Idx[J]; Idx[J] := Idx[J+1]; Idx[J+1] := TempI;
      end;

  Writeln(Format('  Consulta: "%s"', [Query]));
  Writeln(Format('  Top %d mas similares:', [K]));
  for I := 0 to Min(K - 1, High(Corpus)) do
    Writeln(Format('    [%d] %.4f — %s', [I+1, Scores[Idx[I]], Corpus[Idx[I]]]));
  Writeln;
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  Conn          : TAiEmbeddingConnection;
  Embeddings    : array[0..7] of TAiEmbeddingData;
  QueryEmb      : TAiEmbeddingData;
  I, J          : Integer;
begin
  Writeln('=== Similarity ===');
  Writeln('Driver: ', DRIVER, ' / Model: ', MODEL);
  Writeln;

  Conn := TAiEmbeddingConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;
    Conn.ApiKey     := API_KEY;

    // ── Paso 1: generar embeddings del corpus ─────────────────────────────
    Writeln('Generando embeddings del corpus (', Length(CORPUS), ' textos)...');
    for I := 0 to High(CORPUS) do
    begin
      Write(Format('  [%d/%d] ', [I+1, Length(CORPUS)]));
      Embeddings[I] := Conn.CreateEmbedding(CORPUS[I], 'user');
      Writeln(Format('dims=%d', [Length(Embeddings[I])]));
    end;
    Writeln;

    // ── Paso 2: búsqueda por similitud ────────────────────────────────────
    Writeln('--- Búsqueda por similitud ---');

    // Consulta 1: tema de IA/ML → debe encontrar frases de ML
    QueryEmb := Conn.CreateEmbedding('How do neural networks learn?', 'user');
    PrintTopK('How do neural networks learn?', CORPUS, Embeddings, QueryEmb, 3);

    // Consulta 2: tema de naturaleza → debe encontrar frases de biología/naturaleza
    QueryEmb := Conn.CreateEmbedding('Plants and nature', 'user');
    PrintTopK('Plants and nature', CORPUS, Embeddings, QueryEmb, 3);

    // ── Paso 3: matriz de similitud parcial ──────────────────────────────
    Writeln('--- Matriz de similitud (primeros 4 textos) ---');
    Write(Format('%-6s', ['']));
    for I := 0 to 3 do
      Write(Format('  T%-3d', [I+1]));
    Writeln;
    for I := 0 to 3 do
    begin
      Write(Format('T%-4d  ', [I+1]));
      for J := 0 to 3 do
        Write(Format('%5.3f ', [TAiEmbeddingsCore.CosineSimilarity(Embeddings[I], Embeddings[J])]));
      Writeln;
    end;
    Writeln;
    Writeln('  (1.000 = identico, 0.000 = ortogonal, ~0.8+ = muy similar)');

  finally
    Conn.Free;
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
