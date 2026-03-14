program demo_rag_graph;
{$mode objfpc}{$H+}

// Demo: TAiRagGraph — grafo de conocimiento vectorial + estructural
//
// Dominio: empresas de IA, modelos y tecnologias
//
// Escenarios:
//   1. Construir grafo: 5 empresas + 5 modelos + 3 tecnologias, 12 aristas
//   2. Generar embeddings por nodo y reconstruir indices
//   3. SearchText — busqueda semantica en el grafo
//   4. ExecuteMakerGQL — MATCH patron empresa-CREATED-modelo
//   5. ExecuteMakerGQL — GET DEGREES TOP 5 (nodos mas conectados)
//   6. ExecuteMakerGQL — GET SHORTEST PATH entre dos nodos
//   7. Match structural — patron programatico con TGraphMatchQuery
//   8. SaveToFile — exportar en formato MakerAI
//
// Requisitos: OPENAI_API_KEY
//
// Compilar:
//   fpc demo_rag_graph.pas
//     -Fu../Source/Core -Fu../Source/Chat -Fu../Source/RAG -Fu../Source/Utils

uses
  SysUtils, Classes,
  uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Graph.Core;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
procedure AddNodeAndEmbed(Graph: TAiRagGraph; Emb: TAiEmbeddings;
    const AID, ALabel, AName, AText: string;
    const AProp1Key: string = ''; AProp1Val: Variant = Unassigned);
var
  Node: TAiRagGraphNode;
begin
  Node := Graph.AddNode(AID, ALabel, AName);
  Node.Text := AText;
  if AProp1Key <> '' then
    Node.Properties[AProp1Key] := AProp1Val;
  if Assigned(Emb) and (AText <> '') then
  begin
    Node.Data := Emb.CreateEmbedding(AText, 'user');
    Write('.');
  end;
end;

// ---------------------------------------------------------------------------
var
  Emb   : TAiEmbeddings;
  Graph : TAiRagGraph;

  // Nodos — empresas
  NOpenAI, NAnthropic, NGoogle, NMeta, NMistral: TAiRagGraphNode;
  // Nodos — modelos
  NGPT4, NClaude, NGemini, NLlama, NMixtral: TAiRagGraphNode;
  // Nodos — tecnologias
  NTransformer, NRLHF, NSFT: TAiRagGraphNode;

  Nodes   : TNodeArray;
  I       : Integer;
  GqlRes  : string;
  Path    : TObjectArray;
  PathNode: TAiRagGraphNode;
  PathEdge: TAiRagGraphEdge;

  // Para Match programatico
  Query     : TGraphMatchQuery;
  P_Company : TMatchNodePattern;
  P_Edge    : TMatchEdgePattern;
  P_Model   : TMatchNodePattern;
  Results   : TGQLResultArray;
  Res       : TGQLResult;
  Obj       : TObject;

begin
  WriteLn('=== MakerAI FPC — Demo RAG Graph ===');
  WriteLn;

  Emb   := TAiEmbeddings.Create(nil);
  Graph := TAiRagGraph.Create(nil);
  try
    Emb.ApiKey := '@OPENAI_API_KEY';
    Emb.Model  := 'text-embedding-3-small';
    Graph.Embeddings := Emb;

    // -----------------------------------------------------------------------
    // Escenario 1: Construir el grafo de conocimiento
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 1: Construir grafo (nodos + aristas) ---');
    Write('  Generando embeddings');

    // Empresas
    NOpenAI    := Graph.AddNode('openai',    'COMPANY', 'OpenAI');
    NOpenAI.Text := 'OpenAI is an AI research company that created GPT-4 and ChatGPT, a popular conversational AI.';
    NOpenAI.Data := Emb.CreateEmbedding(NOpenAI.Text, 'user'); Write('.');
    NOpenAI.Properties['founded'] := 2015;

    NAnthropic := Graph.AddNode('anthropic', 'COMPANY', 'Anthropic');
    NAnthropic.Text := 'Anthropic is an AI safety company founded by former OpenAI researchers, creators of Claude.';
    NAnthropic.Data := Emb.CreateEmbedding(NAnthropic.Text, 'user'); Write('.');
    NAnthropic.Properties['founded'] := 2021;

    NGoogle    := Graph.AddNode('google',    'COMPANY', 'Google DeepMind');
    NGoogle.Text := 'Google DeepMind is the AI division of Google, responsible for the Gemini model family.';
    NGoogle.Data := Emb.CreateEmbedding(NGoogle.Text, 'user'); Write('.');
    NGoogle.Properties['founded'] := 1998;

    NMeta      := Graph.AddNode('meta',      'COMPANY', 'Meta AI');
    NMeta.Text := 'Meta AI is the research division of Meta, creator of the open-source LLaMA language models.';
    NMeta.Data := Emb.CreateEmbedding(NMeta.Text, 'user'); Write('.');

    NMistral   := Graph.AddNode('mistral',   'COMPANY', 'Mistral AI');
    NMistral.Text := 'Mistral AI is a French AI startup known for efficient open-weight models like Mixtral using MoE.';
    NMistral.Data := Emb.CreateEmbedding(NMistral.Text, 'user'); Write('.');

    // Modelos
    NGPT4    := Graph.AddNode('gpt4',    'MODEL', 'GPT-4');
    NGPT4.Text := 'GPT-4 is a large multimodal language model from OpenAI with strong reasoning and coding capabilities.';
    NGPT4.Data := Emb.CreateEmbedding(NGPT4.Text, 'user'); Write('.');

    NClaude  := Graph.AddNode('claude',  'MODEL', 'Claude');
    NClaude.Text := 'Claude is a family of AI models from Anthropic designed to be helpful, harmless and honest.';
    NClaude.Data := Emb.CreateEmbedding(NClaude.Text, 'user'); Write('.');

    NGemini  := Graph.AddNode('gemini',  'MODEL', 'Gemini');
    NGemini.Text := 'Gemini is a multimodal AI model by Google DeepMind, successor to PaLM with strong reasoning.';
    NGemini.Data := Emb.CreateEmbedding(NGemini.Text, 'user'); Write('.');

    NLlama   := Graph.AddNode('llama',   'MODEL', 'LLaMA');
    NLlama.Text := 'LLaMA is an open-source large language model by Meta AI, widely used for research and fine-tuning.';
    NLlama.Data := Emb.CreateEmbedding(NLlama.Text, 'user'); Write('.');

    NMixtral := Graph.AddNode('mixtral', 'MODEL', 'Mixtral');
    NMixtral.Text := 'Mixtral is a sparse mixture-of-experts model by Mistral AI offering high performance at lower cost.';
    NMixtral.Data := Emb.CreateEmbedding(NMixtral.Text, 'user'); Write('.');

    // Tecnologias
    NTransformer := Graph.AddNode('transformer', 'TECH', 'Transformer');
    NTransformer.Text := 'The Transformer is the foundational neural network architecture based on self-attention mechanisms.';
    NTransformer.Data := Emb.CreateEmbedding(NTransformer.Text, 'user'); Write('.');

    NRLHF := Graph.AddNode('rlhf', 'TECH', 'RLHF');
    NRLHF.Text := 'RLHF (Reinforcement Learning from Human Feedback) aligns language models with human preferences.';
    NRLHF.Data := Emb.CreateEmbedding(NRLHF.Text, 'user'); Write('.');

    NSFT := Graph.AddNode('sft', 'TECH', 'SFT');
    NSFT.Text := 'Supervised Fine-Tuning (SFT) trains language models on curated instruction-following datasets.';
    NSFT.Data := Emb.CreateEmbedding(NSFT.Text, 'user'); Write('.');

    WriteLn(' OK');
    WriteLn('  Nodos: ', Graph.NodeCount);

    // Aristas
    Graph.AddEdge(NOpenAI,    NGPT4,    'e01', 'CREATED',   'created',    1.0);
    Graph.AddEdge(NAnthropic, NClaude,  'e02', 'CREATED',   'created',    1.0);
    Graph.AddEdge(NGoogle,    NGemini,  'e03', 'CREATED',   'created',    1.0);
    Graph.AddEdge(NMeta,      NLlama,   'e04', 'CREATED',   'created',    1.0);
    Graph.AddEdge(NMistral,   NMixtral, 'e05', 'CREATED',   'created',    1.0);

    Graph.AddEdge(NGPT4,    NTransformer, 'e06', 'USES', 'based on', 0.9);
    Graph.AddEdge(NClaude,  NTransformer, 'e07', 'USES', 'based on', 0.9);
    Graph.AddEdge(NGemini,  NTransformer, 'e08', 'USES', 'based on', 0.9);
    Graph.AddEdge(NLlama,   NTransformer, 'e09', 'USES', 'based on', 0.9);
    Graph.AddEdge(NMixtral, NTransformer, 'e10', 'USES', 'based on', 0.9);

    Graph.AddEdge(NGPT4,   NRLHF, 'e11', 'TRAINED_WITH', 'trained with', 0.8);
    Graph.AddEdge(NClaude, NRLHF, 'e12', 'TRAINED_WITH', 'trained with', 0.8);
    Graph.AddEdge(NGPT4,   NSFT,  'e13', 'TRAINED_WITH', 'trained with', 0.8);
    Graph.AddEdge(NClaude, NSFT,  'e14', 'TRAINED_WITH', 'trained with', 0.8);

    WriteLn('  Aristas: ', Graph.EdgeCount);

    // Reconstruir indices
    Graph.RebuildIndexes;
    Graph.Nodes.BuildIndex;
    Graph.Nodes.BuildLexicalIndex;
    WriteLn('  Indices: OK');
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 2: Busqueda semantica
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 2: SearchText — busqueda semantica ---');

    WriteLn('  Query: "open source language model for research"');
    GqlRes := Graph.SearchText('open source language model for research',
                               0, True, 3, 0.3);
    WriteLn(GqlRes);

    WriteLn('  Query: "AI safety and alignment"');
    GqlRes := Graph.SearchText('AI safety and alignment', 0, False, 3, 0.3);
    WriteLn(GqlRes);
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 3: GQL — MATCH empresa CREATED modelo
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 3: GQL — MATCH (c:COMPANY) -[r:CREATED]-> (m:MODEL) ---');
    GqlRes := Graph.ExecuteMakerGQL(
      'MATCH (c:COMPANY) -[r:CREATED]-> (m:MODEL) RETURN c, r, m');
    WriteLn(GqlRes);
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 4: GQL — GET DEGREES TOP 5
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 4: GQL — GET DEGREES TOP 5 ---');
    GqlRes := Graph.ExecuteMakerGQL('GET DEGREES TOP 5');
    WriteLn(GqlRes);
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 5: GQL — GET SHORTEST PATH
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 5: GQL — GET SHORTEST PATH (Anthropic) TO (Transformer) ---');
    GqlRes := Graph.ExecuteMakerGQL(
      'GET SHORTEST PATH (s:COMPANY) TO (t:TECH)');
    WriteLn(GqlRes);
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 6: GetShortestPath programatico
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 6: GetShortestPath (Mistral -> Transformer) ---');
    Path := Graph.GetShortestPath(NMistral, NTransformer);
    if Length(Path) = 0 then
      WriteLn('  Sin ruta encontrada.')
    else
    begin
      Write('  Ruta: ');
      for I := 0 to High(Path) do
      begin
        if I > 0 then Write(' -> ');
        if Path[I] is TAiRagGraphNode then
          Write(TAiRagGraphNode(Path[I]).Name)
        else if Path[I] is TAiRagGraphEdge then
        begin
          PathEdge := TAiRagGraphEdge(Path[I]);
          Write('[', PathEdge.EdgeLabel, ']');
        end;
      end;
      WriteLn;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 7: Match programatico — (COMPANY)-[TRAINED_WITH]->(TECH)
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 7: Match programatico — modelos entrenados con RLHF ---');
    Query := TGraphMatchQuery.Create;
    try
      P_Company := TMatchNodePattern.Create;
      P_Company.Variable  := 'm';
      P_Company.NodeLabel := 'MODEL';

      P_Edge := TMatchEdgePattern.Create;
      P_Edge.EdgeLabel  := 'TRAINED_WITH';
      P_Edge.Direction  := gdOutgoing;

      P_Model := TMatchNodePattern.Create;
      P_Model.Variable  := 't';
      P_Model.NodeLabel := 'TECH';

      Query.AddNodePattern(P_Company);
      Query.AddNodePattern(P_Model);
      Query.AddMatchClause(TMatchClause.Create('m', P_Edge, 't'));

      Results := Graph.Match(Query);
      try
        WriteLn('  Resultados: ', Length(Results));
        for I := 0 to High(Results) do
        begin
          Res := Results[I];
          if Res.TryGet('m', Obj) and (Obj is TAiRagGraphNode) then
            Write('  Modelo: ', TAiRagGraphNode(Obj).Name);
          if Res.TryGet('t', Obj) and (Obj is TAiRagGraphNode) then
            WriteLn('  -> ', TAiRagGraphNode(Obj).Name)
          else
            WriteLn;
        end;
      finally
        for I := 0 to High(Results) do Results[I].Free;
      end;
    finally
      Query.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 8: SHOW LABELS + SHOW EDGES
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 8: GQL — SHOW LABELS / SHOW EDGES ---');
    WriteLn('  SHOW LABELS: ', Graph.ExecuteMakerGQL('SHOW LABELS'));
    WriteLn('  SHOW EDGES:  ', Graph.ExecuteMakerGQL('SHOW EDGES'));
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 9: Guardar en formato MakerAI
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 9: SaveToFile (MakerAI format) ---');
    Graph.SaveToFile('rag_graph_ai.mkai', gefGraphMkai, True);
    WriteLn('  Guardado: rag_graph_ai.mkai');

  finally
    Graph.Free;
    Emb.Free;
  end;

  WriteLn;
  WriteLn('Demo RAG Graph finalizado.');
end.
