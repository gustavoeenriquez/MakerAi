program demo_embeddings;
{$mode objfpc}{$H+}

// Demo: TAiEmbeddings + TAiEmbeddingsCore — generacion y operaciones vectoriales
//
// Escenarios:
//   1. Generar embeddings para 5 textos (modelo text-embedding-3-small)
//   2. Similitud coseno entre pares de textos
//   3. FindNearest — encontrar el texto mas cercano a una query
//   4. FindTopK   — top-3 candidatos mas similares
//   5. Operaciones vectoriales: suma, resta, promedio
//
// Requisitos: OPENAI_API_KEY
//
// Compilar:
//   fpc demo_embeddings.pas
//     -Fu../Source/Core -Fu../Source/Chat -Fu../Source/Utils

uses
  SysUtils, Classes,
  uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings;

// ---------------------------------------------------------------------------
// Muestra los primeros N componentes de un vector
// ---------------------------------------------------------------------------
procedure PrintVector(const V: TAiEmbeddingData; N: Integer = 6);
var
  I: Integer;
begin
  Write('  [');
  for I := 0 to Min(N - 1, High(V)) do
  begin
    if I > 0 then Write(', ');
    Write(FormatFloat('0.0000', V[I]));
  end;
  if Length(V) > N then Write(', ...');
  WriteLn(']  (dim=', Length(V), ')');
end;

// ---------------------------------------------------------------------------
var
  Emb   : TAiEmbeddings;
  V     : array[0..4] of TAiEmbeddingData;
  Cands : TAiEmbeddingList;
  Query : TAiEmbeddingData;
  Nearest: TAiSimilarityResult;
  TopK   : TAiSimilarityList;
  Avg    : TAiEmbeddingData;
  I      : Integer;

const
  TEXTS: array[0..4] of string = (
    'Artificial intelligence is transforming technology.',
    'Machine learning models require large datasets.',
    'The cat sat on the warm mat by the window.',
    'Deep learning is a subset of machine learning.',
    'A dog chased a ball in the sunny park.'
  );

begin
  WriteLn('=== MakerAI FPC — Demo Embeddings ===');
  WriteLn;

  Emb := TAiEmbeddings.Create(nil);
  try
    Emb.ApiKey := '@OPENAI_API_KEY';
    Emb.Model  := 'text-embedding-3-small';

    // -----------------------------------------------------------------------
    // Escenario 1: Generar embeddings para 5 textos
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 1: Generar embeddings para 5 textos ---');
    for I := 0 to 4 do
    begin
      WriteLn('  Texto ', I, ': "', TEXTS[I], '"');
      V[I] := Emb.CreateEmbedding(TEXTS[I], '');
      PrintVector(V[I]);
    end;
    WriteLn('  Tokens usados: prompt=', Emb.prompt_tokens,
            '  total=', Emb.total_tokens);
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 2: Similitud coseno entre pares
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 2: Similitud coseno entre pares ---');
    WriteLn('  Texto 0 vs Texto 1 (ambos sobre AI/ML):');
    WriteLn('    similitud = ',
        FormatFloat('0.0000', TAiEmbeddingsCore.CosineSimilarity(V[0], V[1])));

    WriteLn('  Texto 0 vs Texto 2 (AI vs gato):');
    WriteLn('    similitud = ',
        FormatFloat('0.0000', TAiEmbeddingsCore.CosineSimilarity(V[0], V[2])));

    WriteLn('  Texto 1 vs Texto 3 (ML vs deep learning — muy similar):');
    WriteLn('    similitud = ',
        FormatFloat('0.0000', TAiEmbeddingsCore.CosineSimilarity(V[1], V[3])));

    WriteLn('  Texto 2 vs Texto 4 (gato vs perro — tema animales):');
    WriteLn('    similitud = ',
        FormatFloat('0.0000', TAiEmbeddingsCore.CosineSimilarity(V[2], V[4])));
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 3: FindNearest — query busca el texto mas cercano
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 3: FindNearest ---');
    Query := Emb.CreateEmbedding('neural networks and AI models', '');
    WriteLn('  Query: "neural networks and AI models"');

    SetLength(Cands, 5);
    for I := 0 to 4 do Cands[I] := V[I];

    Nearest := TAiEmbeddingsCore.FindNearest(Query, Cands);
    WriteLn('  Texto mas cercano: indice=', Nearest.Index,
            '  score=', FormatFloat('0.0000', Nearest.Score));
    WriteLn('  Texto: "', TEXTS[Nearest.Index], '"');
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 4: FindTopK — top-3 candidatos
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 4: FindTopK (top-3) ---');
    WriteLn('  Query: "neural networks and AI models"');
    TopK := TAiEmbeddingsCore.FindTopK(Query, Cands, 3);
    for I := 0 to High(TopK) do
      WriteLn('  #', I + 1, '  idx=', TopK[I].Index,
              '  score=', FormatFloat('0.0000', TopK[I].Score),
              '  "', TEXTS[TopK[I].Index], '"');
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 5: Operaciones vectoriales
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 5: Operaciones vectoriales ---');

    WriteLn('  DotProduct(V[0], V[1]) = ',
        FormatFloat('0.0000', TAiEmbeddingsCore.DotProduct(V[0], V[1])));
    WriteLn('  EuclideanDistance(V[0], V[2]) = ',
        FormatFloat('0.0000', TAiEmbeddingsCore.EuclideanDistance(V[0], V[2])));

    SetLength(Cands, 3);
    Cands[0] := V[0]; Cands[1] := V[1]; Cands[2] := V[3];
    Avg := TAiEmbeddingsCore.AverageEmbedding(Cands);
    Write('  Promedio(V[0],V[1],V[3]): ');
    PrintVector(Avg);

    WriteLn('  Similitud query vs promedio AI/ML = ',
        FormatFloat('0.0000', TAiEmbeddingsCore.CosineSimilarity(Query, Avg)));

  finally
    Emb.Free;
  end;

  WriteLn;
  WriteLn('Demo Embeddings finalizado.');
end.
