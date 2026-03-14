program demo_rag_vector;
{$mode objfpc}{$H+}

// Demo: TAiRAGVector — base de conocimiento vectorial con busqueda hibrida
//
// Escenarios:
//   1. Construir una base de conocimiento con 8 fragmentos (AddItem)
//   2. BuildIndex (HNSW) + BuildLexicalIndex (BM25)
//   3. SearchText — busqueda semantica con 3 queries distintas
//   4. SearchText con filtro de metadatos (categoria = "science")
//   5. Busqueda hibrida BM25 + embedding (SearchOptions)
//
// Requisitos: OPENAI_API_KEY
//
// Compilar:
//   fpc demo_rag_vector.pas
//     -Fu../Source/Core -Fu../Source/Chat -Fu../Source/RAG -Fu../Source/Utils

uses
  SysUtils, Classes,
  uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vectors;

// ---------------------------------------------------------------------------
// Datos de prueba: texto + categoria + tema
// ---------------------------------------------------------------------------
type
  TKnowledgeItem = record
    Text    : string;
    Category: string;
    Topic   : string;
  end;

const
  KB_SIZE = 8;
  KB: array[0..KB_SIZE - 1] of TKnowledgeItem = (
    (Text: 'Machine learning algorithms learn patterns from large datasets without explicit programming.';
     Category: 'science'; Topic: 'ML'),
    (Text: 'Deep neural networks consist of multiple layers that transform input data into predictions.';
     Category: 'science'; Topic: 'DeepLearning'),
    (Text: 'Python is the most popular programming language for data science and AI development.';
     Category: 'technology'; Topic: 'Programming'),
    (Text: 'Embeddings are dense vector representations that capture semantic meaning of text.';
     Category: 'science'; Topic: 'NLP'),
    (Text: 'The solar system contains eight planets orbiting the Sun, with Earth being the third.';
     Category: 'astronomy'; Topic: 'Planets'),
    (Text: 'Photosynthesis is the process by which plants convert sunlight into chemical energy.';
     Category: 'biology'; Topic: 'Plants'),
    (Text: 'Large language models are trained on internet-scale text to understand and generate language.';
     Category: 'science'; Topic: 'LLM'),
    (Text: 'The speed of light in vacuum is approximately 299,792 kilometres per second.';
     Category: 'physics'; Topic: 'Constants')
  );

// ---------------------------------------------------------------------------
var
  Emb    : TAiEmbeddings;
  RAG    : TAiRAGVector;
  Meta   : TAiEmbeddingMetaData;
  Node   : TAiEmbeddingNode;
  Filter : TAiFilterCriteria;
  Result : string;
  I      : Integer;

begin
  WriteLn('=== MakerAI FPC — Demo RAG Vector ===');
  WriteLn;

  Emb := TAiEmbeddings.Create(nil);
  RAG := TAiRAGVector.Create(nil);
  try
    Emb.ApiKey := '@OPENAI_API_KEY';
    Emb.Model  := 'text-embedding-3-small';

    RAG.Embeddings := Emb;
    RAG.NameVec    := 'KnowledgeBase';
    RAG.Description:= 'Demo knowledge base with science and technology facts';

    // -----------------------------------------------------------------------
    // Escenario 1: Cargar 8 fragmentos con metadatos
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 1: Cargar base de conocimiento (', KB_SIZE, ' fragmentos) ---');
    for I := 0 to KB_SIZE - 1 do
    begin
      Meta := TAiEmbeddingMetaData.Create;
      try
        Meta['category'] := KB[I].Category;
        Meta['topic']    := KB[I].Topic;
        Meta['index']    := I;
        Write('  [', I, '] "', Copy(KB[I].Text, 1, 60), '..."  ');
        Node := RAG.AddItem(KB[I].Text, Meta);
        if Assigned(Node) then
          WriteLn('OK (dim=', Node.Dim, ')')
        else
          WriteLn('ERROR');
      finally
        Meta.Free;
      end;
    end;
    WriteLn('  Total en base: ', RAG.Count, ' items');
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 2: Construir indices
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 2: Construir indices HNSW + BM25 ---');
    Write('  BuildIndex (HNSW)...');
    RAG.BuildIndex;
    WriteLn(' OK');
    Write('  BuildLexicalIndex (BM25)...');
    RAG.BuildLexicalIndex;
    WriteLn(' OK');
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 3: Busqueda semantica — 3 queries
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 3: SearchText — busqueda semantica ---');

    WriteLn('  Query: "How do neural networks work?"');
    Result := RAG.SearchText('How do neural networks work?', 3, 0.3, nil,
                             True, True);
    WriteLn(Result);

    WriteLn('  Query: "What is a planet?"');
    Result := RAG.SearchText('What is a planet?', 3, 0.3, nil, True, True);
    WriteLn(Result);

    WriteLn('  Query: "vector representations of text"');
    Result := RAG.SearchText('vector representations of text', 3, 0.3, nil,
                             True, True);
    WriteLn(Result);

    // -----------------------------------------------------------------------
    // Escenario 4: Busqueda con filtro de metadatos (category = "science")
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 4: SearchText con filtro category="science" ---');
    Filter := TAiFilterCriteria.Create(loAnd);
    try
      Filter.AddEqual('category', 'science');
      WriteLn('  Query: "artificial intelligence models"');
      Result := RAG.SearchText('artificial intelligence models', 5, 0.2,
                               Filter, True, True);
      WriteLn(Result);
    finally
      Filter.Free;
    end;

    // -----------------------------------------------------------------------
    // Escenario 5: Busqueda hibrida — habilitar BM25 + embedding
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 5: Busqueda hibrida (BM25 + embedding, RRF) ---');
    RAG.SearchOptions.UseEmbeddings := True;
    RAG.SearchOptions.UseBM25       := True;
    RAG.SearchOptions.UseRRF        := True;

    WriteLn('  Query: "programming language for machine learning"');
    Result := RAG.SearchText('programming language for machine learning', 3,
                             0.2, nil, True, True);
    WriteLn(Result);

  finally
    RAG.Free;
    Emb.Free;
  end;

  WriteLn('Demo RAG Vector finalizado.');
end.
