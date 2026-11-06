program BasicEmbeddings;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 03-Embeddings / 01-BasicEmbeddings
// =============================================================================
// Genera embeddings de texto usando un proveedor LLM y explora el vector
// resultante. Primer acercamiento al concepto de representación vectorial.
//
// Conceptos que cubre:
//   - Crear y configurar TAiEmbeddingConnection
//   - Llamar CreateEmbedding() para obtener un vector de floats
//   - Inspeccionar dimensiones del vector
//   - Comparar dos textos con CosineSimilarity
//   - Diferencia entre textos similares y distintos
//
// Proveedores soportados: OpenAI, Gemini, Ollama, Cohere, Mistral
// Para OpenAI: model = 'text-embedding-3-small' (1536 dims)
// Para Gemini: model = 'models/text-embedding-004'
// Para Ollama: model = 'nomic-embed-text' (768 dims), URL local
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

// =============================================================================
//  Imprime un resumen del vector (primeros 8 valores)
// =============================================================================
procedure PrintVectorSummary(const V: TAiEmbeddingData; const Name: String);
var
  I    : Integer;
  Parts: TArray<String>;
begin
  Writeln(Format('  %s: dims=%d', [Name, Length(V)]));
  if Length(V) = 0 then Exit;
  SetLength(Parts, Min(8, Length(V)));
  for I := 0 to High(Parts) do
    Parts[I] := Format('%.4f', [V[I]]);
  Writeln(Format('  Primeros 8 valores: [%s...]', [String.Join(', ', Parts)]));
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  Conn : TAiEmbeddingConnection;
  V1, V2, V3: TAiEmbeddingData;
  Sim12, Sim13: Double;
begin
  Writeln('=== BasicEmbeddings ===');
  Writeln('Driver: ', DRIVER, ' / Model: ', MODEL);
  Writeln;

  Conn := TAiEmbeddingConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;
    Conn.ApiKey     := API_KEY;

    // ── Demo 1: generar embedding de un texto ─────────────────────────────
    Writeln('--- Demo 1: Generar un embedding ---');
    Writeln('Texto: "La inteligencia artificial transforma la industria"');
    V1 := Conn.CreateEmbedding(
      'La inteligencia artificial transforma la industria', 'user');
    PrintVectorSummary(V1, 'V1');
    Writeln(Format('  Tokens usados: prompt=%d, total=%d',
      [Conn.prompt_tokens, Conn.total_tokens]));
    Writeln;

    // ── Demo 2: textos similares vs. distintos ────────────────────────────
    Writeln('--- Demo 2: Similitud coseno ---');

    V2 := Conn.CreateEmbedding(
      'La IA y el machine learning cambian la industria', 'user');
    V3 := Conn.CreateEmbedding(
      'Las recetas de cocina mexicana son muy sabrosas', 'user');

    Sim12 := TAiEmbeddingsCore.CosineSimilarity(V1, V2);
    Sim13 := TAiEmbeddingsCore.CosineSimilarity(V1, V3);

    Writeln('  Texto A: "La inteligencia artificial transforma la industria"');
    Writeln('  Texto B: "La IA y el machine learning cambian la industria"  (similar)');
    Writeln('  Texto C: "Las recetas de cocina mexicana son muy sabrosas"  (distinto)');
    Writeln;
    Writeln(Format('  Similitud A↔B: %.4f  (esperado: alto ~0.85+)', [Sim12]));
    Writeln(Format('  Similitud A↔C: %.4f  (esperado: bajo ~0.10-0.30)', [Sim13]));
    Writeln;

    if Sim12 > Sim13 then
      Writeln('  Correcto: A es mas similar a B que a C.')
    else
      Writeln('  Nota: resultado inesperado — verifica el modelo/driver.');

    // ── Demo 3: magnitud del vector ───────────────────────────────────────
    Writeln;
    Writeln('--- Demo 3: Propiedades del vector ---');
    Writeln(Format('  Dimensiones      : %d', [Length(V1)]));
    Writeln(Format('  Magnitud (norma) : %.6f', [TAiEmbeddingsCore.Magnitude(V1)]));
    Writeln(Format('  Distancia Eucl.  : %.6f', [TAiEmbeddingsCore.EuclideanDistance(V1, V2)]));
    Writeln;
    Writeln('Nota: Un embedding con magnitud ~1.0 esta normalizado (L2 norm).');

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
