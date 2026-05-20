unit uMainBasicRagGraph;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.JSON,

  uMakerAi.RAG.Graph.Core, uMakerAi.Embeddings.Core, uMakerAi.Embeddings, uMakerAi.Chat.Ollama, uMakerAi.RAG.Vectors,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uMakerAi.RAG.Graph.Builder;

type
  TForm20 = class(TForm)
    RAG: TAiRagGraph;
    AiOllamaEmbeddings1: TAiOllamaEmbeddings;
    BtnAddNodoManual: TButton;
    BtnBusquedaSemantica: TButton;
    BtnBusquedaEstructural: TButton;
    BtnBusquedaHibrida: TButton;
    GraphBuilder: TAiRagGraphBuilder;
    MemoJson: TMemo;
    BtnAddNodoFromJson: TButton;
    MemoContexto: TMemo;
    Button1: TButton;
    procedure BtnAddNodoManualClick(Sender: TObject);
    procedure BtnBusquedaSemanticaClick(Sender: TObject);
    procedure BtnBusquedaEstructuralClick(Sender: TObject);
    procedure BtnBusquedaHibridaClick(Sender: TObject);
    procedure BtnAddNodoFromJsonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    Procedure ListarContenidoGrafo;
  public
    { Public declarations }
  end;

var
  Form20: TForm20;

implementation

{$R *.dfm}

procedure TForm20.BtnAddNodoFromJsonClick(Sender: TObject);
var
  JsonString: string;
begin
  // Asumimos que el JSON de arriba está en esta variable
  JsonString := MemoJson.Lines.text;

  // La magia ocurre aquí:
  // 1. Parsea el JSON.
  // 2. Busca si los nodos "Steve Jobs" o "Apple" ya existen (Identity Map).
  // 3. Si no existen, los crea.
  // 4. Si tienen el campo "text", genera sus vectores automáticamente (usando Embeddings).
  // 5. Crea la relación "FUNDÓ" entre ellos.

  GraphBuilder.Process(JsonString, msAddNewOnly);
  RAG.RebuildIndexes;

  ListarContenidoGrafo;
end;

procedure TForm20.BtnAddNodoManualClick(Sender: TObject);
var
  Steve, Apple: TAiRagGraphNode;
  Relacion: TAiRagGraphEdge;
begin
  // 1. Crear los Nodos (Entidades)
  // Sintaxis: AddNode(ID_Unico, Etiqueta_Tipo, Nombre_Legible)
  Steve := RAG.AddNode('id_001', 'PERSONA', 'Steve Jobs');

  // Opcional: Añadir propiedades de negocio (Metadatos)
  Steve.Properties.Add('nacimiento', 1955);
  Steve.Properties.Add('text', 'Steve Jobs, visionario tecnológico y cofundador de Apple.');
  Steve.text := 'Steve Jobs, visionario tecnológico y cofundador de Apple.'; // Información para el RAG


  Apple := RAG.AddNode('id_002', 'EMPRESA', 'Apple Inc.');
  Apple.Properties.Add('industria', 'Tecnología');
  Apple.text := 'Apple Empresa de tecnología innovadora y creadora del IPhone';

  // 2. Crear la SEMÁNTICA  - el vector para búsquedas RAG
  // Opcional solo si se requiere buscar por RAG
  if Assigned(RAG.Embeddings) then
  Begin
    Steve.Data := RAG.Embeddings.CreateEmbedding(Steve.text, 'user');
    Apple.Data := RAG.Embeddings.CreateEmbedding(Apple.text, 'user');
  End;

  // 3. Crear la Arista (Relación)
  // Sintaxis: AddEdge(Origen, Destino, ID_Unico, Etiqueta_Verbo, Descripcion)
  Relacion := RAG.AddEdge(Steve, Apple, 'edge_001', 'FUNDÓ', 'fue el fundador de');

  // Opcional: Cualificar la relación
  Relacion.Properties.Add('año', 1976);

  Relacion.Weight := 0.9; // Alta relevancia

  RAG.RebuildIndexes;

  ListarContenidoGrafo;
end;

procedure TForm20.BtnBusquedaEstructuralClick(Sender: TObject);
var
  Query: TGraphMatchQuery;
  // Patrones para: (Persona) -[FUNDÓ]-> (Empresa)
  P_Persona, P_Empresa: TMatchNodePattern;
  P_Relacion: TMatchEdgePattern;
begin
  Query := TGraphMatchQuery.Create;

  // Definimos el "Molde": Buscamos cualquier PERSONA...
  P_Persona := TMatchNodePattern.Create;
  P_Persona.Variable := 'p';
  P_Persona.NodeLabel := 'PERSONA';

  // ...que haya FUNDADO...
  P_Relacion := TMatchEdgePattern.Create;
  P_Relacion.EdgeLabel := 'FUNDÓ';

  // ...cualquier EMPRESA.
  P_Empresa := TMatchNodePattern.Create;
  P_Empresa.Variable := 'c';
  P_Empresa.NodeLabel := 'EMPRESA';

  // Ejecutamos el patrón (p)-[r]->(c)
  Query.AddMatchClause(TMatchClause.Create('p', P_Relacion, 'c'));
  var
  Res := RAG.Match(Query);
end;

procedure TForm20.BtnBusquedaHibridaClick(Sender: TObject);
var
  Plan: TQueryPlan; // Este plan suele generarlo el LLM automáticamente
begin
  // 1. ANCLA (Vector): "Busca a alguien parecido a 'El genio de Apple'"
  Plan.AnchorPrompt := 'El genio de Apple';
  Plan.AnchorVariable := 'fundador';

  // 2. NAVEGACIÓN (Grafo): "Desde ahí, busca qué empresas fundó"
  SetLength(Plan.Steps, 1);
  Plan.Steps[0].SourceVariable := 'fundador';
  Plan.Steps[0].EdgeLabel := 'FUNDÓ';
  Plan.Steps[0].TargetVariable := 'empresa';

  Plan.ResultVariable := 'empresa';

  // Ejecución: Vector encuentra a Steve -> Grafo encuentra a Pixar y Next.
  var
  Resultados := RAG.Query(Plan);

end;

procedure TForm20.BtnBusquedaSemanticaClick(Sender: TObject);
var
  Resultados: TArray<TAiRagGraphNode>;
  Prompt, Res: String;
  Depth, Limit: Integer;
  ShowProperties: Boolean;
  Precision: Double;
begin
  // El usuario busca un concepto, NO un nombre exacto.
  // En la BD no existe un nodo llamado "Creador del iPhone",
  // pero el vector encontrará a "Steve Jobs" por su descripción.

  Prompt := 'Fundador';

  Depth := 0;
  Limit := 5;
  Precision := 0.1;
  ShowProperties := True;

  Resultados := RAG.Search(Prompt, Depth, Limit, Precision);

  // Forma 1 :  recorrer el resultado y armar la respuesta personalizada
  For var Node in Resultados do
  Begin
    Res := Res + sLineBreak + Node.text;
  End;

  // Forma 2:  obtener la respuesta organizada por el componente
  Res := RAG.SearchText(Prompt, Depth, ShowProperties, Limit, Precision);

  MemoContexto.Lines.text := Res;
end;

procedure TForm20.Button1Click(Sender: TObject);
begin
  RAG.SaveToFile('c:\temp\grafo.txt', TGraphExportFormat.gefGraphMkai, True);
end;

procedure TForm20.ListarContenidoGrafo;
var
  Graph: TAiRagGraph;
  EdgeNode: TAiEmbeddingNode;
  Edge: TAiRagGraphEdge;
  TripletList: TStringList;
  Subject, Predicate, Obj: string;
begin
  // 1. Asegurarse de que el grafo está asignado
  Graph := RAG;
  if not Assigned(Graph) then
  begin
    ShowMessage('El componente del grafo (RAG) no está asignado.');
    Exit;
  end;

  // 2. Usar un TStringList para recolectar y ordenar las tripletas
  TripletList := TStringList.Create;
  try
    // 3. Iterar sobre todas las aristas del grafo
    // Graph.Edges devuelve un TAiRAGVector, y su propiedad Items es una lista de los nodos de embedding (en este caso, las aristas)
    for EdgeNode in Graph.Edges.Items do
    begin
      // Asegurarse de que el elemento es realmente una arista
      if EdgeNode is TAiRagGraphEdge then
      begin
        Edge := EdgeNode as TAiRagGraphEdge;

        // 4. Formatear la tripleta en un formato legible
        Subject := Format('%s (%s)', [Edge.FromNode.Name, Edge.FromNode.NodeLabel]);
        Predicate := Edge.EdgeLabel;
        Obj := Format('%s (%s)', [Edge.ToNode.Name, Edge.ToNode.NodeLabel]);

        // Añadir la tripleta formateada a la lista
        TripletList.Add(Format('%-35s --[%s]--> %s', [Subject, Predicate, Obj]));
      end;
    end;

    // 5. Ordenar la lista para que sea más fácil de leer
    TripletList.Sort;

    // 6. Mostrar los resultados en el TMemo
    MemoContexto.Lines.Clear;
    MemoContexto.Lines.Add(Format('--- Se encontraron %d tripletas en el grafo ---', [TripletList.Count]));
    MemoContexto.Lines.Add('');
    MemoContexto.Lines.AddStrings(TripletList);

  finally
    TripletList.Free;
  end;

end;

end.
