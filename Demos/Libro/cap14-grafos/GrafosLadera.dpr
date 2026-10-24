program GrafosLadera;

// =============================================================================
// LIBRO "IA con Delphi", capitulo 14 - La otra cara del conocimiento: grafos y MakerGQL
// =============================================================================
//   GrafosLadera --extraer   el modelo (gpt-6-sol) extrae tripletas JSON de la carta y de lo
//                            que conto una clienta -> grafo\tripletas.json (ya viene en el repo)
//   GrafosLadera             arma el grafo con TAiRagGraphBuilder (un embedding por nodo), lista
//                            las tripletas, corre consultas MakerGQL, una busqueda semantica, una
//                            hibrida (semantica + vecinos) y la respuesta de Lucia con el grafo.
//                            Exporta el grafo a DOT (GraphViz) y GraphML (Gephi).
// Uso: OPENAI_API_KEY. Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, Winapi.ActiveX, Xml.Win.msxmldom, {$ENDIF}   // GraphML usa TXMLDocument: en consola hay que traer el DOM
  System.SysUtils, System.Classes, System.IOUtils,
  uMakerAi.Chat.AiConnection, uMakerAi.Chat.Initializations,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Graph.Core, uMakerAi.RAG.Graph.Builder;

const
  TEXTO =
    'Carta de Café La Ladera (bolsa de 340 g): Huila Reserva cuesta 38.000 COP, tiene notas de panela y frutos rojos y acidez brillante. ' +
    'Nariño Altura cuesta 42.000 COP, tiene notas de cítricos y caramelo y acidez alta. ' +
    'Sierra Nevada Orgánico cuesta 36.000 COP, tiene notas de chocolate amargo y nuez y acidez baja. ' +
    'Descafeinado Suave cuesta 34.000 COP y tiene notas de cacao suave. ' +
    'Marta es clienta de la tienda: prepara el café en prensa francesa, lo toma sin azúcar y le gustan los cafés frutales. ' +
    'Marta compró el Nariño Altura y le pareció demasiado ácido.';
  EXTRAER =
    'Extrae las entidades y relaciones del texto como tripletas para un grafo de conocimiento. ' +
    'Categorías (nodeLabel): Persona, Producto, Nota (notas de sabor), Acidez, Metodo (forma de preparar). ' +
    'Relaciones (edgeLabel) en MAYÚSCULAS_CON_GUIONES, por ejemplo TIENE_NOTA, TIENE_ACIDEZ, COMPRO, LE_GUSTA, ' +
    'NO_LE_GUSTO, PREPARA_CON. Nombres canónicos, sin artículos. Pon el precio como propiedad numérica del producto. ' +
    'Devuelve ÚNICAMENTE un arreglo JSON, sin Markdown, con objetos {"subject":{"name","nodeLabel","properties"},' +
    '"predicate":{"edgeLabel"},"object":{"name","nodeLabel","properties"}}.' + sLineBreak + 'TEXTO: ';
  CONSULTAS: array [0 .. 3] of string = (
    'SHOW LABELS',
    'MATCH (c:Producto)-[t:TIENE_ACIDEZ]->(a:Acidez) WHERE a.name = ''Baja'' RETURN c',
    'MATCH (p:Persona)-[r:COMPRO]->(c:Producto)-[t:TIENE_ACIDEZ]->(a:Acidez) RETURN p, c, a',
    'MATCH (p:Persona)-[r:NO_LE_GUSTO]->(a:Acidez)<-[t:TIENE_ACIDEZ]-(c:Producto) RETURN c');

function Chat: TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := 'OpenAi';
  Result.Params.Values['Asynchronous'] := 'False';
end;

procedure Titulo(const S: string);
begin
  Writeln('==== ', S, ' ', StringOfChar('=', 62 - Length(S)));
  Writeln;
end;

procedure ExtraerTripletas(const ARuta: string);
var
  Conn: TAiChatConnection;
  Json: string;
begin
  // El modelo lee el texto y devuelve las tripletas; se guardan tal cual para armar el grafo
  Conn := Chat;
  try
    Json := Conn.AddMessageAndRun(EXTRAER + TEXTO, 'user', []).Trim;
    if Json.StartsWith('```') then
      Json := Json.Substring(Json.IndexOf(#10) + 1).Replace('```', '').Trim;
    TFile.WriteAllText(ARuta, Json, TEncoding.UTF8);
    Writeln('tripletas.json: ', Length(Json), ' caracteres, ', Conn.Total_tokens, ' tokens');
  finally
    Conn.Free;
  end;
end;

var
  Dir, Ruta, L, Contexto: string;
  Grafo: TAiRagGraph;
  Builder: TAiRagGraphBuilder;
  Emb: TAiOpenAiEmbeddings;
  Conn: TAiChatConnection;
  E: TAiRagGraphEdge;
  Lista: TStringList;
  N: TObject;
  Nodo: TAiRagGraphNode;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  CoInitialize(nil);   // MSXML es COM
  {$ENDIF}
  Dir := ExtractFilePath(ParamStr(0));
  Ruta := TPath.Combine(Dir, 'grafo\tripletas.json');
  if FindCmdLineSwitch('extraer') then
  begin
    ExtraerTripletas(Ruta);
    Exit;
  end;

  Grafo := TAiRagGraph.Create(nil);
  Builder := TAiRagGraphBuilder.Create(nil);
  Emb := TAiOpenAiEmbeddings.Create(nil);
  Lista := TStringList.Create;
  try
    Emb.ApiKey := '@OPENAI_API_KEY';
    Grafo.Embeddings := Emb;      // un vector por nodo: la busqueda semantica entra por aqui
    Builder.Graph := Grafo;

    Titulo('A. De las tripletas al grafo');
    Writeln('Insertadas: ', Builder.Process(TFile.ReadAllText(Ruta, TEncoding.UTF8)), ' tripletas; ',
      Grafo.NodeCount, ' nodos, ', Grafo.EdgeCount, ' relaciones');
    for N in Grafo.Edges.Items do
      if N is TAiRagGraphEdge then
      begin
        E := TAiRagGraphEdge(N);
        Lista.Add(Format('%s (%s) --[%s]--> %s (%s)', [E.FromNode.Name, E.FromNode.NodeLabel, E.EdgeLabel, E.ToNode.Name, E.ToNode.NodeLabel]));
      end;
    Lista.Sort;
    for L in Lista do
      Writeln('   ', L);
    Grafo.SaveToFile(TPath.Combine(Dir, 'grafo\ladera.dot'), gefDOT);
    Grafo.SaveToFile(TPath.Combine(Dir, 'grafo\ladera.graphml'), gefGraphML);
    Writeln('   exportado: grafo\ladera.dot (GraphViz) y grafo\ladera.graphml (Gephi)');
    Writeln;

    Titulo('B. Consultas MakerGQL: preguntas por la estructura');
    for L in CONSULTAS do
    begin
      Writeln('GQL> ', L);
      try
        Writeln(Grafo.ExecuteMakerGQL(L));
      except
        on Ex: Exception do
          Writeln('   ERROR ', Ex.Message);
      end;
    end;

    Titulo('C. Busqueda semantica sobre los nodos, e hibrida (nodo + vecinos)');
    Writeln('Search> algo suave para tomar en la noche');
    for Nodo in Grafo.Search('algo suave para tomar en la noche', 0, 3, 0.0) do
      Writeln(Format('   %.3f  %s (%s)', [Nodo.Idx, Nodo.Name, Nodo.NodeLabel]));
    Writeln;
    Writeln('SearchText> algo suave para tomar en la noche (profundidad 1)');
    Writeln(Grafo.SearchText('algo suave para tomar en la noche', 1, False, 2, 0.5));
    Writeln;

    Titulo('D. Lucia responde con el grafo');
    Contexto := Grafo.SearchText('Marta', 2, False, 1, 0.5);   // Marta y todo lo que la rodea a dos saltos
    Writeln(Contexto);
    Conn := Chat;
    try
      Writeln('Tú: Marta volvió, ¿qué café le recomiendo?');
      Writeln('Lucía: ', Conn.AddMessageAndRun('Eres Lucía, la barista de Café La Ladera. Con este contexto del grafo de ' +
        'conocimiento, responde en 2 frases, en texto plano: Marta volvió, ¿qué café le recomiendo?' + sLineBreak +
        'CONTEXTO:' + sLineBreak + Contexto, 'user', []).Trim);
      Writeln(Format('   (%d tokens de entrada, %d de salida)', [Conn.Prompt_tokens, Conn.Completion_tokens]));
    finally
      Conn.Free;
    end;
  finally
    Lista.Free;
    Builder.Free;
    Grafo.Free;
    Emb.Free;
  end;
end.
