{ ===============================================================================
  MakerAI — TAiLlamacppEmbeddings Demo

  Demonstrates local GGUF text embeddings using TAiLlamacppEmbeddings
  (uMakerAi.Embeddings.Llamacpp).  Runs without any HTTP server.

  Scenarios:
    1. Basic embedding         — embed a sentence, show dimensions
    2. Cosine similarity       — compare semantically similar / dissimilar pairs
    3. Nearest-neighbour search — find closest sentence in a corpus
    4. Top-K ranking           — rank a corpus by relevance to a query

  Model: mxbai-embed-large-v1 (1024-dim)
    Via `ollama pull mxbai-embed-large`
    Requires makerai.embedder.dll and llama.cpp DLLs in DLL_PATH.
=============================================================================== }
program llamacpp_embed_demo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Math,
  Winapi.Windows,
  uMakerAi.Embeddings.Llamacpp,
  uMakerAi.Embeddings.Core;

const
  // Path to makerai.embedder.dll (and llama.cpp DLLs alongside it)
  DLL_PATH = 'E:\copilot\LlamaKit\Win64\Release\makerai.embedder.dll';

  // mxbai-embed-large-v1 Q4_K_M blob from Ollama (`ollama pull mxbai-embed-large`)
  FALLBACK_MODEL =
    'E:\IAs\Ollama\models\blobs\' +
    'sha256-819c2adf5ce6df2b6bd2ae4ca90d2a69f060afeb438d0c171db57daa02e39c3d';

// =============================================================================
// Corpus — 9 sentences in 3 semantic clusters.
// =============================================================================
const
  Corpus: array[0..8] of string = (
    // Astronomy (0-2)
    'The Milky Way galaxy contains over 200 billion stars.',
    'Black holes are regions of spacetime with extreme gravitational pull.',
    'The James Webb Space Telescope images galaxies billions of light-years away.',
    // Programming (3-5)
    'Delphi is an object-oriented language for building Windows and mobile apps.',
    'Object Pascal provides strong typing and component-based design.',
    'FireMonkey (FMX) enables cross-platform GUI development in Delphi.',
    // Food (6-8)
    'Pizza is an Italian dish topped with tomato sauce, cheese, and toppings.',
    'Pasta comes in dozens of shapes and is cooked in salted boiling water.',
    'Risotto is a creamy Italian rice dish prepared by slowly adding broth.'
  );

// =============================================================================

procedure PrintSection(const Title: string);
var W: Integer;
begin
  Writeln;
  W := 58 - Length(Title);
  if W < 0 then W := 0;
  Writeln('  -- ', Title, ' ', StringOfChar('-', W));
end;

procedure PrintBar(Score: Double; Width: Integer = 30);
var
  Filled: Integer;
begin
  Filled := Round(Score * Width);
  Write('[', StringOfChar('#', Filled), StringOfChar('.', Width - Filled), ']');
end;

// =============================================================================
var
  Emb:     TAiLlamacppEmbeddings;
  Vecs:    TAiEmbeddingList;
  V:       TAiEmbeddingData;
  TopK:    TAiSimilarityList;
  Sim:     Double;
  I, J:    Integer;

  // Similarity pairs to demonstrate
  Pairs: array[0..5, 0..1] of Integer = (
    (0, 1),   // astronomy <-> astronomy  (should be HIGH)
    (0, 3),   // astronomy <-> programming (should be LOW)
    (3, 4),   // programming <-> programming (should be HIGH)
    (6, 7),   // food <-> food (should be HIGH)
    (1, 6),   // astronomy <-> food (should be LOW)
    (0, 2)    // astronomy <-> astronomy (should be HIGH)
  );

  Query: string;
begin
  Writeln('================================================================');
  Writeln('  MakerAI — TAiLlamacppEmbeddings Demo');
  Writeln('  Driver: TAiLlamacppEmbeddings');
  Writeln('  Model : mxbai-embed-large-v1 (1024-dim)');
  Writeln('================================================================');
  Writeln;
  Writeln('  DLL  : ', DLL_PATH);
  Writeln('  Model: ...', Copy(FALLBACK_MODEL, Length(FALLBACK_MODEL) - 11, 12));

  // Add the DLL directory to the search path so makerai.embedder.dll can find
  // llama.dll and ggml.dll when loaded from a different working directory.
  SetCurrentDir(ExtractFilePath(DLL_PATH));
  SetDllDirectory(PChar(ExtractFilePath(DLL_PATH)));

  Emb := TAiLlamacppEmbeddings.Create(nil);
  try
    Emb.DLLPath     := DLL_PATH;
    Emb.ModelPath   := FALLBACK_MODEL;
    Emb.NGPULayers  := 99;    // all layers on GPU; set 0 for CPU-only

    // ── Scenario 1: Basic embedding ─────────────────────────────────────────
    PrintSection('Scenario 1 — Basic embedding');

    Writeln;
    Write  ('  Embedding: "', Corpus[0], '"  ... ');
    V := Emb.CreateEmbedding(Corpus[0], '');
    Writeln('done');
    Writeln(Format('  Dimensions : %d', [Emb.Dimensions]));
    Writeln(Format('  First 5 dims: [%.4f, %.4f, %.4f, %.4f, %.4f]',
      [V[0], V[1], V[2], V[3], V[4]]));
    Writeln(Format('  Magnitude  : %.6f',
      [TAiEmbeddingsCore.Magnitude(V)]));

    // ── Scenario 2: Cosine similarity — known pairs ──────────────────────────
    PrintSection('Scenario 2 — Cosine similarity (selected pairs)');
    Writeln;
    Writeln('  Computing embeddings for all 9 corpus sentences ...');

    SetLength(Vecs, Length(Corpus));
    for I := 0 to High(Corpus) do
      Vecs[I] := Emb.CreateEmbedding(Corpus[I], '');

    Writeln('  done.');
    Writeln;

    for I := 0 to High(Pairs) do
    begin
      J   := Pairs[I][0];
      var K := Pairs[I][1];
      Sim := TAiEmbeddingsCore.CosineSimilarity(Vecs[J], Vecs[K]);
      Write (Format('  [%d vs %d]  %.4f  ', [J, K, Sim]));
      PrintBar(Sim);
      Writeln;
      Writeln('    A: "', Copy(Corpus[J], 1, 55), '"');
      Writeln('    B: "', Copy(Corpus[K], 1, 55), '"');
    end;

    // ── Scenario 3: Nearest-neighbour search ────────────────────────────────
    PrintSection('Scenario 3 — Nearest-neighbour search');
    Writeln;
    Query := 'telescopes used for observing distant objects in space';
    Write  ('  Query: "', Query, '"  ... ');
    V := Emb.CreateEmbedding(Query, '');
    Writeln('done');
    Writeln;

    var Best := TAiEmbeddingsCore.FindNearest(V, Vecs);
    Writeln(Format('  Nearest  idx=%d  score=%.4f', [Best.Index, Best.Score]));
    Writeln('    "', Corpus[Best.Index], '"');

    // ── Scenario 4: Top-K ranking ────────────────────────────────────────────
    PrintSection('Scenario 4 — Top-K ranking (K=3)');
    Writeln;
    Writeln('  Same query: "', Query, '"');
    Writeln;
    TopK := TAiEmbeddingsCore.FindTopK(V, Vecs, 3);
    for I := 0 to High(TopK) do
    begin
      Write  (Format('  #%d  idx=%d  %.4f  ', [I + 1, TopK[I].Index, TopK[I].Score]));
      PrintBar(TopK[I].Score);
      Writeln;
      Writeln('      "', Copy(Corpus[TopK[I].Index], 1, 65), '"');
    end;

  finally
    Emb.Free;
  end;

  Writeln;
  Writeln('Press Enter to exit.');
  Readln;
end.
