unit uMainRagGraphDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON,
  System.Generics.Collections, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  uMakerAi.RAG.Graph.Core, uMakerAi.RAG.Graph.Builder, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings, uMakerAi.Chat.OpenAi, uMakerAi.RAG.Vectors, FMX.Edit, uMakerAi.Prompts,
  uMakerAi.Chat.AiConnection, uMakerAi.Chat, uMakerAi.Core, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  uMakerAi.Chat.Gemini, uMakerAi.Chat.Initializations,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.PGDef, FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.PG, Data.DB, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FMX.Objects, FMX.Layouts, FMX.TabControl,
  FMX.ListBox, FMX.EditBox, FMX.SpinBox, FMX.SearchBox, uMakerAi.RAG.Graph.Driver.Postgres;

type

  TAiRagGraphNodeHacker = class helper for TAiRagGraphNode
  public
    procedure InternalAddOutgoingEdge(AEdge: TAiRagGraphEdge);
    procedure InternalAddIncomingEdge(AEdge: TAiRagGraphEdge);
  end;

  TMainRagGraphDemo = class(TForm)
    RAG: TAiRagGraph;
    AiOpenAiEmbeddings1: TAiOpenAiEmbeddings;
    AiConn: TAiChatConnection;
    AiPrompts1: TAiPrompts;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    DbConn: TFDConnection;
    MainLayout: TLayout;
    RAGBuilder1: TAiRagGraphBuilder;
    Layout1: TLayout;
    Button1: TButton;
    Image1: TImage;
    BtnSave: TButton;
    Image2: TImage;
    BtnLoad: TButton;
    Image3: TImage;
    TabControl1: TTabControl;
    TabSearch: TTabItem;
    TabAddItems: TTabItem;
    Layout2: TLayout;
    ChDetailed: TCheckBox;
    EditDepth: TEdit;
    EditLimit: TEdit;
    EditPrecision: TEdit;
    Label1: TLabel;
    label12: TLabel;
    Label2: TLabel;
    Layout3: TLayout;
    Label3: TLabel;
    MemoPrompt: TMemo;
    Layout4: TLayout;
    Label4: TLabel;
    MemoResultados: TMemo;
    Splitter1: TSplitter;
    Layout5: TLayout;
    Label5: TLabel;
    MemoToProcess: TMemo;
    Layout6: TLayout;
    Label6: TLabel;
    MemoProcessOut: TMemo;
    Splitter2: TSplitter;
    Layout7: TLayout;
    BtnAddNodos: TButton;
    Image5: TImage;
    BtnMatch: TButton;
    Image6: TImage;
    BtnConsultaQueryLLM: TButton;
    Image7: TImage;
    BtnBusquedaRAG: TButton;
    Image4: TImage;
    TabMatch: TTabItem;
    Layout8: TLayout;
    Layout9: TLayout;
    cbDirection: TComboBox;
    Label7: TLabel;
    cbStartNodeLabel: TComboBox;
    cbTargetNodeLabel: TComboBox;
    cbEdgeLabel: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edtStartNodeName: TEdit;
    Label11: TLabel;
    BtnInitMatch: TButton;
    BtnListTriplets: TButton;
    popSuggestions: TPopup;
    lbSuggestions: TListBox;
    numDepth: TSpinBox;
    chMatchShowProperties: TCheckBox;
    RAGPgDriver: TAiRagGraphPostgresDriver;
    Label13: TLabel;
    Layout10: TLayout;
    Layout11: TLayout;
    Splitter4: TSplitter;
    Label14: TLabel;
    lbQuerySteps: TListBox;
    Layout12: TLayout;
    Label15: TLabel;
    MemoResults: TMemo;
    Layout13: TLayout;
    BtnClear: TButton;
    btnAddStep: TButton;
    btnSearch: TButton;
    GroupBox1: TGroupBox;
    BtnUseInMemoryData: TSpeedButton;
    BtnUseDataBase: TSpeedButton;
    Image9: TImage;
    Image8: TImage;
    BtnLoadText: TButton;
    Image10: TImage;
    Label16: TLabel;
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnBusquedaRAGClick(Sender: TObject);
    procedure BtnAddNodosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RagGraphAddEdge(Sender: TObject; AEdge: TAiRagGraphEdge; var Handled: Boolean);
    procedure RagGraphAddNode(Sender: TObject; ANode: TAiRagGraphNode; var Handled: Boolean);
    procedure RagGraphDeleteEdge(Sender: TObject; const AEdgeID: string; var Handled: Boolean);
    procedure RagGraphDeleteNode(Sender: TObject; const ANodeID: string; var Handled: Boolean);
    procedure RagGraphFindNodeByID(Sender: TObject; const ANodeID: string; out ANodeData: TNodeDataRecord; var Found: Boolean);
    procedure RagGraphLoad(Sender: TObject; Graph: TAiRagGraph; var Handled: Boolean);
    procedure RagGraphQuery(Sender: TObject; const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double; ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
    procedure RagGraphSearchNodes(Sender: TObject; const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; out ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
    procedure BtnConsultaQueryLLMClick(Sender: TObject);
    procedure BtnMatchClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnAddStepClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure BtnInitMatchClick(Sender: TObject);
    procedure BtnListTripletsClick(Sender: TObject);
    procedure edtStartNodeNameChangeTracking(Sender: TObject);
    procedure lbSuggestionsItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure cbStartNodeLabelChange(Sender: TObject);
    procedure edtStartNodeNameExit(Sender: TObject);
    procedure RagGetUniqueNodeLabels(Sender: TObject; Graph: TAiRagGraph; var ResultLabels: TArray<System.string>; var Handled: Boolean);
    procedure RagGetUniqueEdgeLabels(Sender: TObject; Graph: TAiRagGraph; var ResultLabels: TArray<System.string>; var Handled: Boolean);
    procedure RagFindNodeByName(Sender: TObject; Graph: TAiRagGraph; const AName, ANodeLabel: string; var ResultNode: TAiRagGraphNode; var Handled: Boolean);
    procedure RagFindNodesByProperty(Sender: TObject; Graph: TAiRagGraph; const AKey: string; const AValue: Variant; var ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
    procedure RagFindNodesByLabel(Sender: TObject; Graph: TAiRagGraph; const ALabel: string; var ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
    procedure RagGraphFindEdgeByID(Sender: TObject; const AEdgeID: string; out AEdgeData: TEdgeDataRecord; var Found: Boolean);
    procedure RagGetNodeEdges(Sender: TObject; ANode: TAiRagGraphNode; var Handled: Boolean);
    procedure RagFindNodeNamesByLabel(Sender: TObject; const ANodeLabel, ASearchText: string; ALimit: Integer; out ResultNames: TArray<System.string>; var Handled: Boolean);
    procedure BtnUseDataBaseClick(Sender: TObject);
    procedure BtnUseInMemoryDataClick(Sender: TObject);
    procedure BtnLoadTextClick(Sender: TObject);
  private
    FCurrentEntidad: String;

    // Variables para búsquedas por match
    FCurrentQuery: TGraphMatchQuery;
    FNodeVarCounter: Integer;

    Function NewQuery: TFDQuery;
    function EmbeddingToString(const AData: TAiEmbeddingData): string;
    function PropertiesToJSONString(const AProperties: TDictionary<string, Variant>): string;
    function HydrateNodeFromQuery(AQuery: TFDQuery; AGraph: TAiRagGraph): TAiRagGraphNode;

    procedure MatchBuilder_PopulateComboBoxes;
    function MatchBuilder_GetNextNodeVar: string;
    procedure MatchBuilder_UpdateQueryVisualizer;
    procedure MatchBuilder_PopulateSuggestions(const SearchText: string);
    procedure MatchBuilder_DisplayMatchResults(const AResults: TArray<TDictionary<string, TObject>>; AShowProperties: Boolean);
    procedure MatchBuilder_DisplaySubgraphResults(const AResults: TArray<TDictionary<string, TObject>>; AShowProperties: Boolean);
    procedure MatchBuilder_DisplayProperties(AProperties: TDictionary<string, Variant>; AOutput: TStrings; const AIndentation: string);

  public
    function ParseJsonToQueryPlan(const AJSONString: string): TQueryPlan;
  end;

var
  MainRagGraphDemo: TMainRagGraphDemo;


implementation

{$R *.fmx}

procedure TMainRagGraphDemo.BtnAddNodosClick(Sender: TObject);
Var
  Ent, Prompt, Res: String;
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
begin
  Ent := Trim(MemoToProcess.Lines.Text);

  If Ent = '' then
    Raise Exception.Create('Primero adicione el texto a procesar en el memo y luego presione el botón para crear el grafo');



  Prompt := AiPrompts1.GetTemplate('CreaJSonFromTexto', ['texto=' + Ent]);
  // Prompt := AiPrompts1.GetTemplate('CreaJSonFromTextoMejorado', ['texto=' + Ent]);

  // Memo1.Lines.Text := Prompt;

  AiConn.Params.Values['NativeOutputFiles'] := '[tfc_textFile]';
  AiConn.Params.Values['Max_Tokens'] := '16000';

  Res := AiConn.AddMessageAndRun(Prompt, 'user', []);

  Msg := AiConn.GetLastMessage;

  MemoProcessOut.Lines.Add('');
  MemoProcessOut.Lines.Add('');
  // Memo1.Lines.Add(Res);

  If Msg.MediaFiles.Count > 0 then
  Begin
    For MF in Msg.MediaFiles do
    Begin
      MemoProcessOut.Lines.Add('');
      MemoProcessOut.Lines.Add('');
      MemoProcessOut.Lines.Add(MF.ToString);
      RAGBuilder1.Process(MF.ToString);
    End;
    ShowMessage('Se almacenó correctamente el json- Nodos:' + RAG.NodeCount.ToString);
  End
  Else
    ShowMessage('Error al almacenar el Json');
end;

procedure TMainRagGraphDemo.btnAddStepClick(Sender: TObject);
var
  SourceVar, TargetVar: string;
  SourcePattern, TargetPattern: TMatchNodePattern;
  EdgePattern: TMatchEdgePattern;
begin
  // Si es el primer paso, usamos el ancla
  if FCurrentQuery.MatchClauses.Count = 0 then
  begin
    // Crear el nodo de origen (ancla)
    SourceVar := MatchBuilder_GetNextNodeVar; // n0
    SourcePattern := TMatchNodePattern.Create;
    SourcePattern.Variable := SourceVar;
    SourcePattern.NodeLabel := cbStartNodeLabel.Text;
    if Trim(edtStartNodeName.Text) <> '' then
      SourcePattern.Properties.Add('name', edtStartNodeName.Text);
    FCurrentQuery.AddNodePattern(SourcePattern);
  end
  else
  begin
    // Si no, el origen es el destino del paso anterior
    SourceVar := FCurrentQuery.MatchClauses.Last.TargetNodeVar;
  end;

  // Crear el nodo de destino para este paso
  TargetVar := MatchBuilder_GetNextNodeVar; // n1, n2, ...
  TargetPattern := TMatchNodePattern.Create;
  TargetPattern.Variable := TargetVar;
  TargetPattern.NodeLabel := cbTargetNodeLabel.Text;
  FCurrentQuery.AddNodePattern(TargetPattern);

  // Crear el patrón de la arista
  EdgePattern := TMatchEdgePattern.Create;
  EdgePattern.Variable := 'e' + IntToStr(FCurrentQuery.MatchClauses.Count);
  EdgePattern.EdgeLabel := cbEdgeLabel.Text;
  case cbDirection.ItemIndex of
    0:
      EdgePattern.Direction := gdOutgoing;
    1:
      EdgePattern.Direction := gdIncoming;
    2:
      EdgePattern.Direction := gdBoth;
  end;

  // Añadir la cláusula a la consulta
  FCurrentQuery.AddMatchClause(TMatchClause.Create(SourceVar, EdgePattern, TargetVar));

  // Actualizar la visualización
  MatchBuilder_UpdateQueryVisualizer;
end;

procedure TMainRagGraphDemo.BtnBusquedaRAGClick(Sender: TObject);
Var
  Prompt, Res: String;
  Depth, Limit: Integer;
  Precision: Single;
  Detailed: Boolean;
begin
  Prompt := MemoPrompt.Lines.Text;
  Depth := StrToIntDef(EditDepth.Text, 0);
  Detailed := ChDetailed.IsChecked;
  Limit := StrToIntDef(EditLimit.Text, 5);
  Precision := StrToFloatDef(EditPrecision.Text, 0.5);

  Res := RAG.SearchText(Prompt, Depth, Detailed, Limit, Precision);
  MemoResultados.Lines.Add(Res);
end;

procedure TMainRagGraphDemo.BtnClearClick(Sender: TObject);
Begin
  // Liberar la consulta actual y crear una nueva
  FCurrentQuery.Free;
  FCurrentQuery := TGraphMatchQuery.Create;
  FNodeVarCounter := 0;

  // Limpiar la UI
  lbQuerySteps.Clear;
  MemoResults.Lines.Clear;
end;

procedure TMainRagGraphDemo.BtnConsultaQueryLLMClick(Sender: TObject);
var
  Ent, Prompt, Res, JSonPlanStr: String;
  Msg: TAiChatMessage;
  Plan: TQueryPlan;
  Results: TArray<TAiRagGraphNode>;
  Node: TAiRagGraphNode;
  Log: TStringList;

  Depth, Limit: Integer;
  Precision: Single;
  Detailed: Boolean;
  PropPair: TPair<string, Variant>;
begin
  Depth := StrToIntDef(EditDepth.Text, 0);
  Detailed := ChDetailed.IsChecked;
  Limit := StrToIntDef(EditLimit.Text, 5);
  Precision := StrToFloatDef(EditPrecision.Text, 0.5);

  Log := TStringList.Create;
  try
    Try
      Self.Cursor := crHourGlass; // Cambiar el cursor para indicar que se está trabajando

      // --- Paso 1: Obtener la consulta del usuario y construir el prompt para el LLM ---
      Ent := MemoPrompt.Lines.Text;
      if Trim(Ent) = '' then
      begin
        ShowMessage('Por favor, ingrese una consulta.');
        Exit;
      end;

      Log.Add('Consulta del usuario: "' + Ent + '"');
      Log.Add('');

      Prompt := AiPrompts1.GetTemplate('CreaJSonQueryPlan', ['texto=' + Ent]);

      // --- Paso 2: Llamar al LLM para generar el plan de consulta JSON ---
      // Tu configuración para forzar la salida de un archivo de texto es excelente.
      AiConn.Params.Values['NativeOutputFiles'] := '[tfc_textFile]';
      Res := AiConn.AddMessageAndRun(Prompt, 'user', []);
      Msg := AiConn.GetLastMessage;

      // --- Paso 3: Parsear el JSON y ejecutar la consulta ---
      if (Msg.MediaFiles = nil) or (Msg.MediaFiles.Count = 0) then
      begin
        ShowMessage('El LLM no devolvió un plan de consulta estructurado.');
        Exit;
      end;

      // Asumimos que el LLM devuelve un solo archivo con el plan
      JSonPlanStr := Msg.MediaFiles[0].ToString;
      Log.Add('Plan JSON recibido del LLM:');
      Log.Add(JSonPlanStr);
      Log.Add('');

      Plan := ParseJsonToQueryPlan(JSonPlanStr);

      // Verificación básica de que el plan es válido
      if (Plan.AnchorPrompt <> '') and (Plan.AnchorVariable <> '') then
      begin
        // ---- INICIO DEL BLOQUE COMPLETADO ----

        Log.Add('Ejecutando el plan de consulta...');

        // Ejecutar la consulta en el componente RAG usando el plan
        // Usamos valores por defecto para profundidad, límite y precisión por ahora.
        Results := RAG.Query(Plan, Depth, Limit, Precision);

        // Formatear y mostrar los resultados
        Log.Add('');
        Log.Add('--- Resultados de la Consulta ---');
        if Length(Results) > 0 then
        begin
          for Node in Results do
          begin
            Log.Add('  - ' + Node.Name + ' (Tipo: ' + Node.NodeLabel + ')');

            if Detailed and (Node.Properties <> nil) and (Node.Properties.Count > 0) then
            begin
              Log.Add('  Propiedades de la entidad:');
              for PropPair in Node.Properties do
              begin
                Log.Add('    • ');
                Log.Add(PropPair.Key);
                Log.Add(': ');
                try
                  Log.Add(VarToStr(PropPair.Value));
                except
                  Log.Add('<valor no legible>');
                end;
              end;
            end;
          end;
        end
        else
        begin
          Log.Add('(No se encontraron resultados que coincidan con el plan de consulta)');
        end;
      end
      else
      begin
        Log.Add('El plan de consulta recibido del LLM es inválido o está incompleto.');
      end;

      MemoResultados.Lines.Assign(Log);

    except
      on E: Exception do
        ShowMessage('Ocurrió un error durante el proceso: ' + E.Message);
    end;
  finally
    Log.Free;
    Self.Cursor := crDefault; // Restaurar el cursor
  end;
End;

procedure TMainRagGraphDemo.BtnInitMatchClick(Sender: TObject);
begin
  FCurrentQuery := TGraphMatchQuery.Create;
  FNodeVarCounter := 0;

  // Poblar los ComboBox con datos del grafo
  MatchBuilder_PopulateComboBoxes;
end;

procedure TMainRagGraphDemo.BtnListTripletsClick(Sender: TObject);
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
    MemoResults.Lines.Clear;
    MemoResults.Lines.Add(Format('--- Se encontraron %d tripletas en el grafo ---', [TripletList.Count]));
    MemoResults.Lines.Add('');
    MemoResults.Lines.AddStrings(TripletList);

  finally
    TripletList.Free;
  end;
end;

procedure TMainRagGraphDemo.BtnLoadClick(Sender: TObject);
var
  St: TMemoryStream;
begin
  OpenDialog1.Filter := 'MakerAi Graph (*.mkai)|*.mkai';
  OpenDialog1.DefaultExt := '.mkai';
  OpenDialog1.FilterIndex := 1;
  OpenDialog1.InitialDir := 'c:\temp\';

  if OpenDialog1.Execute then
  begin
    St := TMemoryStream.Create;
    try
      St.LoadFromFile(OpenDialog1.FileName);
      St.Position := 0;
      RAG.LoadFromStream(St);
      SaveDialog1.FileName := OpenDialog1.FileName;
    finally
      St.Free;
    end;
  end;
end;

procedure TMainRagGraphDemo.BtnMatchClick(Sender: TObject);
begin
  TabControl1.ActiveTab := TabMatch;
end;

procedure TMainRagGraphDemo.BtnSaveClick(Sender: TObject);
begin
  // --- Configurar el diálogo de guardado ---
  SaveDialog1.Title := 'Guardar Grafo de Conocimiento';
  SaveDialog1.InitialDir := 'c:\temp\';

  // Configurar los filtros. La primera línea es la que se selecciona por defecto.
  SaveDialog1.Filter := 'MakerAI Graph (*.mkai)|*.mkai|' + // Formato nativo (completo o ligero)
    'GraphML File (*.graphml)|*.graphml|' + // Para Gephi, yEd
    'Graphviz DOT File (*.dot)|*.dot|' + // Para Graphviz
    'JSON Graph (Legacy) (*.json)|*.json|' + // Si quieres mantener compatibilidad
    'Todos los archivos (*.*)|*.*';

  // Establecer la extensión por defecto que se añadirá si el usuario no escribe una.
  SaveDialog1.DefaultExt := '.mkai';
  SaveDialog1.FileName := 'mi_grafo_de_conocimiento'; // Nombre de archivo sugerido

  // --- Ejecutar el diálogo y guardar ---
  if SaveDialog1.Execute then
  begin
    try
      // Opción A: Siempre guardado completo.
      // RAG.SaveToFile(SaveDialog1.FileName, True);

      var
      SaveWithEmbeddings := MessageDlg('¿Desea incluir los datos de embeddings en el archivo?' + sLineBreak + '(El archivo será mucho más grande)', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes;
      RAG.SaveToFile(SaveDialog1.FileName, SaveWithEmbeddings);

      // Por ahora, usemos la Opción A para simplicidad.
      // RAG.SaveToFile(SaveDialog1.FileName, True);

      // Informar al usuario (opcional pero recomendado)
      ShowMessage('Grafo guardado exitosamente en: ' + SaveDialog1.FileName);

      // Actualizar el diálogo de apertura por conveniencia
      OpenDialog1.FileName := SaveDialog1.FileName;
    except
      on E: Exception do
      begin
        ShowMessage('Error al guardar el grafo: ' + E.Message);
      end;
    end;
  end;
end;

procedure TMainRagGraphDemo.btnSearchClick(Sender: TObject);
var
  Results: TArray<TDictionary<string, TObject>>;
  Depth: Integer;
begin
  if FCurrentQuery.MatchClauses.Count = 0 then
  begin
    ShowMessage('Por favor, añada al menos un paso a la consulta.');
    Exit;
  end;

  Depth := Round(numDepth.Value); // Obtener el valor del control de profundidad

  MemoResults.Lines.Clear;
  MemoResults.Lines.Add('Ejecutando consulta...');
  Application.ProcessMessages;

  Results := RAG.Match(FCurrentQuery, Depth);

  MemoResults.Lines.Clear;
  MemoResults.Lines.Add(Format('Se encontraron %d coincidencias.', [Length(Results)]));
  MemoResults.Lines.Add('----------------------------------');

  if Length(Results) > 0 then
  begin
    // El código para mostrar ahora debe diferenciar entre el resultado del Match y el del subgrafo
    if Depth = 0 then
      MatchBuilder_DisplayMatchResults(Results, chMatchShowProperties.IsChecked) // Un nuevo método para mostrar resultados de Match
    else
      MatchBuilder_DisplaySubgraphResults(Results, chMatchShowProperties.IsChecked); // Un nuevo método para mostrar el subgrafo
  end;
end;

procedure TMainRagGraphDemo.BtnUseDataBaseClick(Sender: TObject);
begin
   Rag.Driver := RAGPgDriver;
   Rag.Clear;
   BtnSave.Enabled := False;
   BtnLoad.Enabled := False;
   ShowMessage('Now the Graph work with DataBase data');
end;

procedure TMainRagGraphDemo.BtnUseInMemoryDataClick(Sender: TObject);
begin
   Rag.Driver := Nil;
   Rag.Clear;
   BtnSave.Enabled := True;
   BtnLoad.Enabled := True;
   ShowMessage('Now the Graph work with memory data');
end;

procedure TMainRagGraphDemo.Button1Click(Sender: TObject);
begin
  RAG.Clear;
  MemoToProcess.Lines.Clear;
  MemoProcessOut.Lines.Clear;
  MemoResultados.Lines.Clear;
  MemoPrompt.Lines.Clear;
end;

procedure TMainRagGraphDemo.BtnLoadTextClick(Sender: TObject);
var
  TextStream: TStringStream;
begin
  OpenDialog1.Filter := 'Archivos de Texto (*.txt)|*.txt|' +
                        'Archivos Text (*.text)|*.text|' +
                        'Archivos JSON (*.json)|*.json|' +
                        'Archivos CSV (*.csv)|*.csv|' +
                        'Todos los archivos de texto (*.txt;*.text;*.json;*.csv)|*.txt;*.text;*.json;*.csv|' +
                        'Todos los archivos (*.*)|*.*';
  OpenDialog1.DefaultExt := '.txt';
  OpenDialog1.FilterIndex := 5; // Selecciona "Todos los archivos de texto" por defecto
  OpenDialog1.InitialDir := 'c:\temp\';

  if OpenDialog1.Execute then
  begin
    TextStream := TStringStream.Create('', TEncoding.UTF8);
    try
      TextStream.LoadFromFile(OpenDialog1.FileName);
      MemoToProcess.Lines.Text := TextStream.DataString;
    finally
      TextStream.Free;
    end;
  end;
end;


procedure TMainRagGraphDemo.cbStartNodeLabelChange(Sender: TObject);
begin
  edtStartNodeName.Text := '';
  popSuggestions.IsOpen := False;
end;

procedure TMainRagGraphDemo.edtStartNodeNameChangeTracking(Sender: TObject);
begin
  MatchBuilder_PopulateSuggestions(edtStartNodeName.Text);
end;

procedure TMainRagGraphDemo.edtStartNodeNameExit(Sender: TObject);
begin
  // Pequeño retraso para permitir que el clic en el ListBox se procese primero
  TThread.ForceQueue(nil,
    procedure
    begin
      if not lbSuggestions.IsFocused then
        popSuggestions.IsOpen := False;
    end);
end;

function TMainRagGraphDemo.NewQuery: TFDQuery;
begin
  Result := TFDQuery.Create(Self);
  Result.Connection := DbConn;
end;

function TMainRagGraphDemo.EmbeddingToString(const AData: TAiEmbeddingData): string;
var
  i: Integer;
  SB: TStringBuilder;
begin
  if Length(AData) = 0 then
    Exit('[]');

  SB := TStringBuilder.Create;
  try
    SB.Append('[');
    for i := 0 to High(AData) do
    begin
      SB.Append(FloatToStr(AData[i])); // Usa FormatFloat si necesitas control de formato/región
      if i < High(AData) then
        SB.Append(',');
    end;
    SB.Append(']');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TMainRagGraphDemo.FormCreate(Sender: TObject);
begin
  FCurrentEntidad := 'CLIENTE01';
end;

function TMainRagGraphDemo.HydrateNodeFromQuery(AQuery: TFDQuery; AGraph: TAiRagGraph): TAiRagGraphNode;
var
  Dim: Integer;
begin
  if AGraph.Nodes.Dim > 0 then
    Dim := AGraph.Nodes.Dim
  else
    Dim := 1536; // Reemplaza con tu dimensión por defecto

  Result := TAiRagGraphNode.Create(AGraph, Dim);
  Result.ID := AQuery.FieldByName('id').AsString;
  Result.NodeLabel := AQuery.FieldByName('node_label').AsString;
  Result.Name := AQuery.FieldByName('name').AsString;

  if not AQuery.FieldByName('properties').IsNull then
    JSONStringToProperties(AQuery.FieldByName('properties').AsString, Result.Properties);

  if not AQuery.FieldByName('embedding').IsNull then
    Result.Data := StringToEmbedding(AQuery.FieldByName('embedding').AsString);
end;



procedure TMainRagGraphDemo.lbSuggestionsItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  if Item <> nil then
  begin
    // 1. Poner el texto seleccionado en el TEdit
    edtStartNodeName.Text := Item.Text;
    // 2. Mover el cursor al final del texto
    edtStartNodeName.SetFocus;
    edtStartNodeName.GoToTextEnd;
    // 3. Ocultar el popup
    popSuggestions.IsOpen := False;
  end;
end;

function TMainRagGraphDemo.ParseJsonToQueryPlan(const AJSONString: string): TQueryPlan;
var
  JsonValue: TJSONValue;
  RootObject: TJSONObject;
  StepsArray: TJSONArray;
  StepValue: TJSONValue;
  StepObject: TJSONObject;
  i: Integer;
begin
  // Inicializar el resultado con valores por defecto
  FillChar(Result, SizeOf(TQueryPlan), 0);
  Result.AnchorPrompt := '';
  Result.AnchorVariable := '';
  Result.ResultVariable := '';
  SetLength(Result.Steps, 0);

  JsonValue := TJSONObject.ParseJSONValue(AJSONString);
  if not(JsonValue is TJSONObject) then
  begin
    JsonValue.Free;
    raise Exception.Create('El JSON proporcionado no es un objeto válido.');
  end;

  RootObject := JsonValue as TJSONObject;
  try
    // Extraer los campos raíz
    Result.AnchorPrompt := RootObject.GetValue<string>('anchorPrompt', '');
    Result.AnchorVariable := RootObject.GetValue<string>('anchorVariable', '');
    Result.ResultVariable := RootObject.GetValue<string>('resultVariable', '');

    // Procesar el array de 'steps'
    if RootObject.TryGetValue<TJSONArray>('steps', StepsArray) then
    begin
      SetLength(Result.Steps, StepsArray.Count);
      for i := 0 to StepsArray.Count - 1 do
      begin
        StepValue := StepsArray.Items[i];
        if StepValue is TJSONObject then
        begin
          StepObject := StepValue as TJSONObject;
          Result.Steps[i].SourceVariable := StepObject.GetValue<string>('sourceVariable', '');
          Result.Steps[i].EdgeLabel := StepObject.GetValue<string>('edgeLabel', '');
          Result.Steps[i].TargetVariable := StepObject.GetValue<string>('targetVariable', '');
          Result.Steps[i].TargetNodeLabel := StepObject.GetValue<string>('targetNodeLabel', '');
          Result.Steps[i].IsReversed := StepObject.GetValue<Boolean>('isReversed', False);
        end;
      end;
    end;
  finally
    JsonValue.Free;
  end;
end;

function TMainRagGraphDemo.PropertiesToJSONString(const AProperties: TDictionary<string, Variant>): string;
var
  JsonObj: TJSONObject;
  Pair: TPair<string, Variant>;
begin
  if (AProperties = nil) or (AProperties.Count = 0) then
    Exit('{}');

  // La lógica es muy similar a la que acabamos de poner en el Core.
  // Podríamos incluso reutilizarla si tuviéramos una unidad de helpers común.
  JsonObj := TJSONObject.Create;
  try
    for Pair in AProperties do
    begin
      // Para no depender de VariantToJSONValue (que está en el Core),
      // podemos replicar su lógica simple aquí.
      case VarType(Pair.Value) of
        varBoolean: JsonObj.AddPair(Pair.Key, TJSONBool.Create(Boolean(Pair.Value)));
        varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord, varInt64, varUInt64, varSingle, varDouble, varCurrency:
            JsonObj.AddPair(Pair.Key, TJSONNumber.Create(Extended(Pair.Value)));
      else // varString y otros
        JsonObj.AddPair(Pair.Key, TJSONString.Create(VarToStr(Pair.Value)));
      end;
    end;
    Result := JsonObj.ToString;
  finally
    JsonObj.Free;
  end;
end;

procedure TMainRagGraphDemo.RagFindNodeByName(Sender: TObject; Graph: TAiRagGraph; const AName, ANodeLabel: string; var ResultNode: TAiRagGraphNode; var Handled: Boolean);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Text := 'SELECT * FROM graph_nodes WHERE entidad = :entidad AND node_label = :node_label AND name = :name LIMIT 1';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_label').AsString := ANodeLabel;
    Query.ParamByName('name').AsString := AName;
    Query.Open;

    if not Query.Eof then
    begin
      // Función auxiliar para "hidratar" un nodo desde un TFDQuery
      ResultNode := HydrateNodeFromQuery(Query, Graph);
      Handled := True;
    end
    else
    begin
      ResultNode := nil;
      Handled := True;
    end;
  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagFindNodeNamesByLabel(Sender: TObject; const ANodeLabel, ASearchText: string; ALimit: Integer; out ResultNames: TArray<System.string>; var Handled: Boolean);
var
  Query: TFDQuery;
  NameList: TStringList;
begin
  Query := NewQuery;
  NameList := TStringList.Create;
  try
    // La consulta es perfecta. Usa LIKE y es case-insensitive gracias a UPPER.
    Query.SQL.Text := 'SELECT name FROM graph_nodes ' +
                      'WHERE entidad = :entidad AND node_label = :node_label AND UPPER(name) LIKE :search_text ' +
                      'ORDER BY name LIMIT :limit';

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_label').AsString := ANodeLabel;
    Query.ParamByName('search_text').AsString := '%' + ASearchText.ToUpper + '%';
    Query.ParamByName('limit').AsInteger := ALimit;

    Query.Open;

    while not Query.Eof do
    begin
      NameList.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    ResultNames := NameList.ToStringArray;
    Handled := True;
  finally
    Query.Free;
    NameList.Free;
  end;
end;

procedure TMainRagGraphDemo.RagFindNodesByLabel(Sender: TObject; Graph: TAiRagGraph; const ALabel: string; var ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
var
  Query: TFDQuery;
  NodeIDs: TStringList;
  i: Integer;
begin
  if not (Sender is TAiRagGraph) then Exit;

  Query := NewQuery;
  NodeIDs := TStringList.Create;
  try
    // 1. Obtener solo los IDs de los nodos que coinciden.
    Query.SQL.Text := 'SELECT id FROM graph_nodes WHERE entidad = :entidad AND node_label = :node_label';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_label').AsString := ALabel;
    Query.Open;

    while not Query.Eof do
    begin
      NodeIDs.Add(Query.FieldByName('id').AsString);
      Query.Next;
    end;

    // 2. Pedirle al grafo que "hidrate" o devuelva cada nodo desde su caché.
    SetLength(ResultNodes, NodeIDs.Count);
    for i := 0 to NodeIDs.Count - 1 do
    begin
      // FindNodeByID es ahora nuestro único punto de entrada para obtener nodos.
      // Se encargará de crear el objeto si no existe, o devolver el
      // existente si ya fue cargado.
      ResultNodes[i] := Graph.FindNodeByID(NodeIDs[i]);
    end;

    Handled := True;
  finally
    Query.Free;
    NodeIDs.Free;
  end;
end;

procedure TMainRagGraphDemo.RagFindNodesByProperty(Sender: TObject; Graph: TAiRagGraph; const AKey: string; const AValue: Variant; var ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
var
  Query: TFDQuery;
  NodeList: TObjectList<TAiRagGraphNode>;
begin
  Query := NewQuery;
  NodeList := TObjectList<TAiRagGraphNode>.Create(True); // <-- LÍNEA AÑADIDA
  try
    if SameText(AKey, 'name') then
    begin
      Query.SQL.Text := 'SELECT * FROM graph_nodes WHERE entidad = :entidad AND name = :value';
      Query.ParamByName('value').AsString := VarToStr(AValue);
    end
    else
    begin
      Query.SQL.Text := 'SELECT * FROM graph_nodes WHERE entidad = :entidad AND properties ->> :key = :value';
      Query.ParamByName('key').AsString := AKey;
      Query.ParamByName('value').AsString := VarToStr(AValue);
    end;

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;

    while not Query.Eof do
    begin
      NodeList.Add(HydrateNodeFromQuery(Query, Graph)); // Pasamos 'Graph' en lugar de 'RAG'
      Query.Next;
    end;

    ResultNodes := NodeList.ToArray;
    NodeList.OwnsObjects := False;
    Handled := True;
  finally
    Query.Free;
    NodeList.Free;
  end;
end;

{
procedure TMainRagGraphDemo.RagGetNodeEdges(Sender: TObject; ANode: TAiRagGraphNode; var Handled: Boolean);
var
  Query: TFDQuery;
  Graph: TAiRagGraph;
  EdgeID: string;
begin
  if not (Sender is TAiRagGraph) or (ANode = nil) then
    Exit;

  Graph := Sender as TAiRagGraph;
  Query := NewQuery;
  try
    // 1. Buscamos los IDs de todas las aristas conectadas a este nodo.
    // Solo necesitamos los IDs, el resto del trabajo lo hará el grafo.
    Query.SQL.Text := 'SELECT id FROM graph_edges ' +
                      'WHERE entidad = :entidad AND ' +
                      '(source_node_id = :node_id OR target_node_id = :node_id)';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_id').AsString := ANode.ID;
    Query.Open;

    // 2. Iteramos sobre los IDs y le pedimos al grafo que cargue cada arista.
    while not Query.Eof do
    begin
      EdgeID := Query.FieldByName('id').AsString;
      // Esta es la llamada clave.
      // FindEdgeByID es ahora un "get-or-create-from-db".
      // Se encargará de todo el proceso de carga y conexión.
      Graph.FindEdgeByID(EdgeID);
      Query.Next;
    end;

    Handled := True;
  finally
    Query.Free;
  end;
end;
}

procedure TMainRagGraphDemo.RagGetNodeEdges(Sender: TObject; ANode: TAiRagGraphNode; var Handled: Boolean);
var
  Query: TFDQuery;
  Graph: TAiRagGraph;
  EdgeData: TEdgeDataRecord;
  EdgeID: string;
begin
  if not (Sender is TAiRagGraph) or (ANode = nil) then Exit;

  Graph := Sender as TAiRagGraph;
  Query := NewQuery;
  try
    // 1. Traemos TODOS los datos de las aristas en UNA SOLA consulta.
    Query.SQL.Text := 'SELECT id, edge_label, name, source_node_id, target_node_id, ' +
                      'weight, properties, embedding ' +
                      'FROM graph_edges ' +
                      'WHERE entidad = :entidad AND ' +
                      '(source_node_id = :node_id OR target_node_id = :node_id)';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('node_id').AsString := ANode.ID;
    Query.Open;

    // 2. Iteramos sobre los resultados y le pedimos al grafo que hidrate cada arista.
    while not Query.Eof do
    begin
      EdgeID := Query.FieldByName('id').AsString;

      // --- ¡LÓGICA CORREGIDA Y LIMPIA! ---
      // Usamos la nueva función pública para comprobar si la arista ya existe en memoria.
      if not Graph.EdgeExistsInMemory(EdgeID) then
      begin
        // La arista no existe en memoria, así que la creamos e hidratamos.
        EdgeData.ID := EdgeID;
        EdgeData.EdgeLabel := Query.FieldByName('edge_label').AsString;
        EdgeData.Name := Query.FieldByName('name').AsString;
        EdgeData.SourceNodeID := Query.FieldByName('source_node_id').AsString;
        EdgeData.TargetNodeID := Query.FieldByName('target_node_id').AsString;
        EdgeData.Weight := Query.FieldByName('weight').AsFloat;

        if not Query.FieldByName('properties').IsNull then
          EdgeData.PropertiesJSON := Query.FieldByName('properties').AsString
        else
          EdgeData.PropertiesJSON := '{}';

        if not Query.FieldByName('embedding').IsNull then
          EdgeData.EmbeddingStr := Query.FieldByName('embedding').AsString
        else
          EdgeData.EmbeddingStr := '[]';

        // Usamos el método protegido del grafo para crear el objeto desde los datos.
        // Para llamarlo, SÍ necesitamos el class helper aquí.
        Graph.InternalHydrateEdge(EdgeData);
      end;
      // --- FIN DE LA LÓGICA ---

      Query.Next;
    end;
    Handled := True;
  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGetUniqueEdgeLabels(Sender: TObject; Graph: TAiRagGraph; var ResultLabels: TArray<System.string>; var Handled: Boolean);
var
  Query: TFDQuery;
  LabelList: TStringList;
begin
  Query := NewQuery;
  LabelList := TStringList.Create;
  try
    // 1. Definir la consulta SQL
    Query.SQL.Text := 'SELECT DISTINCT edge_label FROM graph_edges WHERE entidad = :entidad ORDER BY edge_label';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;

    // 2. Ejecutar la consulta
    Query.Open;

    // 3. Recorrer los resultados
    while not Query.Eof do
    begin
      LabelList.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    // 4. Poblar el array de resultado
    ResultLabels := LabelList.ToStringArray;

    // 5. Marcar como manejado
    Handled := True;

  finally
    Query.Free;
    LabelList.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGetUniqueNodeLabels(Sender: TObject; Graph: TAiRagGraph; var ResultLabels: TArray<string>; var Handled: Boolean);
var
  Query: TFDQuery;
  LabelList: TStringList;
begin
  // Asumimos que la conexión a la base de datos ya está activa.
  // Si no es así, puedes añadir una comprobación aquí.

  Query := NewQuery; // Usando tu función helper
  LabelList := TStringList.Create;
  try
    // 1. Definir la consulta SQL
    Query.SQL.Text := 'SELECT DISTINCT node_label FROM graph_nodes WHERE entidad = :entidad ORDER BY node_label';
    Query.ParamByName('entidad').AsString := FCurrentEntidad;

    // 2. Ejecutar la consulta
    Query.Open;

    // 3. Recorrer los resultados y añadirlos a la lista
    while not Query.Eof do
    begin
      LabelList.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    // 4. Convertir la TStringList a un TArray<string> para el resultado
    ResultLabels := LabelList.ToStringArray;

    // 5. Indicar que la solicitud ha sido manejada por la base de datos
    Handled := True;

  finally
    Query.Free;
    LabelList.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphAddEdge(Sender: TObject; AEdge: TAiRagGraphEdge; var Handled: Boolean);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Clear;

    if Length(AEdge.Data) > 0 then
    Begin
      Query.SQL.Add('INSERT INTO graph_edges (entidad, id, edge_label, name, source_node_id, target_node_id, weight, properties, embedding) ');
      Query.SQL.Add('VALUES (:entidad, :id, :edge_label, :name, :source_node_id, :target_node_id, :weight, :properties::JsonB, ''' + EmbeddingToString(AEdge.Data) + ''')');
    End
    Else
    Begin
      Query.SQL.Add('INSERT INTO graph_edges (entidad, id, edge_label, name, source_node_id, target_node_id, weight, properties) ');
      Query.SQL.Add('VALUES (:entidad, :id, :edge_label, :name, :source_node_id, :target_node_id, :weight, :properties::JsonB)');
    End;

    // Asignar valores a los parámetros
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('id').AsString := AEdge.ID;
    Query.ParamByName('edge_label').AsString := AEdge.EdgeLabel;
    Query.ParamByName('name').AsString := AEdge.Name;
    Query.ParamByName('source_node_id').AsString := AEdge.FromNode.ID;
    Query.ParamByName('target_node_id').AsString := AEdge.ToNode.ID;
    Query.ParamByName('weight').AsFloat := AEdge.Weight;
    Query.ParamByName('properties').AsString := PropertiesToJSONString(AEdge.Properties);

    Query.ExecSQL;
    Handled := True;

  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphAddNode(Sender: TObject; ANode: TAiRagGraphNode; var Handled: Boolean);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    if Length(ANode.Data) > 0 then
    Begin
      Query.SQL.Add('INSERT INTO graph_nodes (entidad, id, node_label, name, properties, embedding) ');
      Query.SQL.Add('VALUES (:entidad, :id, :node_label, :name, :properties::jsonb, ''' + EmbeddingToString(ANode.Data) + ''')');
    End
    Else
    Begin
      Query.SQL.Add('INSERT INTO graph_nodes (entidad, id, node_label, name, properties) ');
      Query.SQL.Add('VALUES (:entidad, :id, :node_label, :name, :properties::jsonb)');
    End;

    // Asignar valores a los parámetros
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('id').AsString := ANode.ID;
    Query.ParamByName('node_label').AsString := ANode.NodeLabel;
    Query.ParamByName('name').AsString := ANode.Name;
    Query.ParamByName('properties').AsString := PropertiesToJSONString(ANode.Properties);

    Query.ExecSQL;
    Handled := True;
  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphDeleteEdge(Sender: TObject; const AEdgeID: string; var Handled: Boolean);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Text := 'DELETE FROM graph_edges WHERE id = :id AND entidad = :entidad';
    Query.ParamByName('id').AsString := AEdgeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;

    Query.ExecSQL;
    Handled := True;
  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphDeleteNode(Sender: TObject; const ANodeID: string; var Handled: Boolean);
var
  Query: TFDQuery;
begin
  Query := NewQuery;
  try
    Query.SQL.Text := 'DELETE FROM graph_nodes WHERE id = :id AND entidad = :entidad';

    Query.ParamByName('id').AsString := ANodeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;

    Query.ExecSQL;
    Handled := True;
  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphFindEdgeByID(Sender: TObject; const AEdgeID: string; out AEdgeData: TEdgeDataRecord; var Found: Boolean);
var
  Query: TFDQuery;
begin
  Found := False;
  Query := NewQuery;
  try
    Query.SQL.Text := 'SELECT id, edge_label, name, source_node_id, target_node_id, ' +
                      'weight, properties, embedding ' +
                      'FROM graph_edges ' +
                      'WHERE id = :id AND entidad = :entidad';
    Query.ParamByName('id').AsString := AEdgeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;

    if not Query.IsEmpty then
    begin
      // Llenamos el record con los datos crudos de la BD.
      AEdgeData.ID := Query.FieldByName('id').AsString;
      AEdgeData.EdgeLabel := Query.FieldByName('edge_label').AsString;
      AEdgeData.Name := Query.FieldByName('name').AsString;
      AEdgeData.SourceNodeID := Query.FieldByName('source_node_id').AsString;
      AEdgeData.TargetNodeID := Query.FieldByName('target_node_id').AsString;
      AEdgeData.Weight := Query.FieldByName('weight').AsFloat;

      if not Query.FieldByName('properties').IsNull then
        AEdgeData.PropertiesJSON := Query.FieldByName('properties').AsString
      else
        AEdgeData.PropertiesJSON := '{}';

      if not Query.FieldByName('embedding').IsNull then
        AEdgeData.EmbeddingStr := Query.FieldByName('embedding').AsString
      else
        AEdgeData.EmbeddingStr := '[]';

      Found := True;
    end;
  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphFindNodeByID(Sender: TObject; const ANodeID: string; out ANodeData: TNodeDataRecord; var Found: Boolean);
var
  Query: TFDQuery;
begin
  Found := False;
  Query := NewQuery;
  try
    Query.SQL.Text := 'SELECT id, node_label, name, properties, embedding ' +
                      'FROM graph_nodes ' +
                      'WHERE id = :id AND entidad = :entidad';
    Query.ParamByName('id').AsString := ANodeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;

    if not Query.IsEmpty then
    begin
      // En lugar de crear un objeto, llenamos el record con los datos crudos.
      ANodeData.ID := Query.FieldByName('id').AsString;
      ANodeData.NodeLabel := Query.FieldByName('node_label').AsString;
      ANodeData.Name := Query.FieldByName('name').AsString;

      if not Query.FieldByName('properties').IsNull then
        ANodeData.PropertiesJSON := Query.FieldByName('properties').AsString
      else
        ANodeData.PropertiesJSON := '{}';

      if not Query.FieldByName('embedding').IsNull then
        ANodeData.EmbeddingStr := Query.FieldByName('embedding').AsString
      else
        ANodeData.EmbeddingStr := '[]';

      Found := True; // Indicamos que encontramos los datos.
    end;

    // Ya no hay un `Handled` en la nueva firma, `Found` cumple ese rol.
    // Ya no creamos ningún TAiRagGraphNode aquí.

  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphLoad(Sender: TObject; Graph: TAiRagGraph; var Handled: Boolean);
begin
  //
end;

procedure TMainRagGraphDemo.RagGraphQuery(Sender: TObject; const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double; ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
var
  Graph: TAiRagGraph;
  Query: TFDQuery;
  Step: TQueryStep;
  QueryEmbedding: TAiEmbeddingData;
  EmbeddingStr: String;
  NodeIDs: TStringList;
begin
  SetLength(ResultNodes, 0);
  if not (Sender is TAiRagGraph) then Exit;
  Graph := Sender as TAiRagGraph;

  // Solo manejaremos planes con un ancla y un solo paso, como antes.
  // Podrías extender esto con un generador de SQL dinámico en el futuro.
  if Length(APlan.Steps) <> 1 then
  begin
    Handled := False;
    Exit;
  end;

  Query := NewQuery;
  NodeIDs := TStringList.Create;
  Step := APlan.Steps[0];
  try
    // Paso 1: Obtener embedding (sin cambios)
    QueryEmbedding := Graph.Embeddings.CreateEmbedding(APlan.AnchorPrompt, 'user');
    EmbeddingStr := EmbeddingToString(QueryEmbedding);

    // Paso 2: Construir y ejecutar la consulta SQL. Seleccionamos solo los IDs del resultado final.
    Query.SQL.Text :=
      'WITH AnchorNodes AS (' +
      '  SELECT id' +
      '  FROM graph_nodes' +
      '  WHERE entidad = :entidad AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold' +
      '  ORDER BY embedding <-> ''' + EmbeddingStr + ''' LIMIT :limit' +
      ')' +
      // Consulta final: Obtener los IDs de los nodos de destino.
      'SELECT final_node.id ' +
      'FROM AnchorNodes an ' +
      'JOIN graph_edges e ON an.id = e.source_node_id AND e.entidad = :entidad AND e.edge_label = :edge_label ' +
      'JOIN graph_nodes final_node ON e.target_node_id = final_node.id AND final_node.entidad = :entidad ' +
      'WHERE final_node.node_label = :target_label';

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('distance_threshold').AsFloat := 1 - APrecision;
    Query.ParamByName('limit').AsInteger := ALimit;
    Query.ParamByName('edge_label').AsString := Step.EdgeLabel;
    Query.ParamByName('target_label').AsString := Step.TargetNodeLabel;
    Query.Open;

    while not Query.Eof do
    begin
      NodeIDs.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    // Paso 3: Hidratación delegada y construcción del resultado
    SetLength(ResultNodes, NodeIDs.Count);
    for var i := 0 to NodeIDs.Count - 1 do
    begin
      // FindNodeByID se asegura de que el nodo exista en memoria,
      // cargándolo si es necesario.
      ResultNodes[i] := Graph.FindNodeByID(NodeIDs[i]);
    end;

    Handled := True;

  finally
    Query.Free;
    NodeIDs.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphSearchNodes(Sender: TObject; const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; out ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
var
  Query: TFDQuery;
  Graph: TAiRagGraph;
  QueryEmbedding: TAiEmbeddingData;
  EmbeddingStr: string;
  NodeIDs: TStringList;
  InitialNodeIDs: TStringList; // Para guardar los nodos semilla
  Node: TAiRagGraphNode;
begin
  SetLength(ResultNodes, 0);
  if not (Sender is TAiRagGraph) then Exit;
  Graph := Sender as TAiRagGraph;

  // Paso 1: Obtener el embedding (sin cambios)
  if not Assigned(Graph.Embeddings) then
    raise Exception.Create('El motor de Embeddings no está asignado al grafo.');
  QueryEmbedding := Graph.Embeddings.CreateEmbedding(APrompt, 'user');
  EmbeddingStr := EmbeddingToString(QueryEmbedding);

  Query := NewQuery;
  NodeIDs := TStringList.Create;
  InitialNodeIDs := TStringList.Create;
  try
    // Paso 2: Ejecutar la consulta SQL (la consulta en sí casi no cambia)
    if ADepth = 0 then
    begin
      // CASO SIMPLE: Búsqueda semántica. Solo necesitamos los IDs.
      Query.SQL.Text := 'SELECT id FROM graph_nodes ' +
                        'WHERE entidad = :entidad AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold ' +
                        'ORDER BY embedding <-> ''' + EmbeddingStr + ''' LIMIT :limit';
      Query.ParamByName('distance_threshold').AsFloat := 1 - APrecision;
    end
    else
    begin
      // CASO COMPLEJO: Expansión. La CTE es perfecta. Solo seleccionamos los IDs al final.
      Query.SQL.Text :=
        'WITH RECURSIVE traversal AS (' +
        '  SELECT id, 1 AS depth, ARRAY[id] AS path' +
        '  FROM (' +
        '    SELECT id' +
        '    FROM graph_nodes' +
        '    WHERE entidad = :entidad AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold' +
        '    ORDER BY embedding <-> ''' + EmbeddingStr + '''' +
        '    LIMIT :limit' +
        '  ) AS initial' +
        '  UNION ALL' +
        '  SELECT' +
        '    CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END AS id,' +
        '    t.depth + 1,' +
        '    t.path || CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END' +
        '  FROM traversal t' +
        '  JOIN graph_edges e ON (e.source_node_id = t.id OR e.target_node_id = t.id) AND e.entidad = :entidad' +
        '  WHERE t.depth <= :depth' + // <= para incluir el nivel de profundidad final
        '    AND NOT (CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END = ANY(t.path))' +
        ')' +
        // Consulta final: solo queremos los IDs únicos de todos los nodos del subgrafo
        'SELECT DISTINCT id FROM traversal';

      Query.ParamByName('distance_threshold').AsFloat := 1 - APrecision;
      Query.ParamByName('depth').AsInteger := ADepth;
    end;

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('limit').AsInteger := ALimit;
    Query.Open;

    // Guardamos los IDs de los nodos semilla (los primeros resultados)
    // para saber qué devolver al final.
    while not Query.Eof do
    begin
      NodeIDs.Add(Query.Fields[0].AsString);
      if ADepth = 0 then
         InitialNodeIDs.Add(Query.Fields[0].AsString);
      Query.Next;
    end;

    // Si la búsqueda fue con expansión, necesitamos determinar los nodos semilla por separado.
    if ADepth > 0 then
    begin
        Query.SQL.Text := 'SELECT id FROM graph_nodes ' +
                          'WHERE entidad = :entidad AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold ' +
                          'ORDER BY embedding <-> ''' + EmbeddingStr + ''' LIMIT :limit';
        Query.Open;
        while not Query.Eof do
        begin
           InitialNodeIDs.Add(Query.Fields[0].AsString);
           Query.Next;
        end;
    end;


    // Paso 3: "Hidratación" delegada. Le pedimos al grafo que se asegure
    // de que todos los nodos del subgrafo existan en memoria.
    for var NodeID in NodeIDs do
    begin
      // FindNodeByID se encargará de cargar el nodo desde la BD si no está
      // ya en memoria. ¡Esto es mucho más limpio!
      Graph.FindNodeByID(NodeID);
    end;

    // Si hubo expansión, también debemos asegurarnos de que todas las aristas
    // entre los nodos del subgrafo estén cargadas.
    if ADepth > 0 then
    begin
        for var NodeID in NodeIDs do
        begin
            Node := Graph.FindNodeByID(NodeID);
            if Node <> nil then
                Node.EnsureEdgesAreLoaded; // Esto disparará OnGetNodeEdges
        end;
    end;

    // Paso 4: Construir el resultado final a partir de los nodos semilla.
    // Los objetos ya están en la caché del grafo.
    SetLength(ResultNodes, InitialNodeIDs.Count);
    for var i := 0 to InitialNodeIDs.Count - 1 do
    begin
      ResultNodes[i] := Graph.FindNodeByID(InitialNodeIDs[i]);
    end;

    Handled := True;

  finally
    Query.Free;
    NodeIDs.Free;
    InitialNodeIDs.Free;
  end;
end;


{ TAiRagGraphNodeHacker }

procedure TAiRagGraphNodeHacker.InternalAddIncomingEdge(AEdge: TAiRagGraphEdge);
begin
  Self.AddIncomingEdge(AEdge); // Llama al método 'protected' original
end;

procedure TAiRagGraphNodeHacker.InternalAddOutgoingEdge(AEdge: TAiRagGraphEdge);
begin
  Self.AddOutgoingEdge(AEdge); // Llama al método 'protected' original
end;

procedure TMainRagGraphDemo.MatchBuilder_DisplayMatchResults(const AResults: TArray<TDictionary<string, TObject>>; AShowProperties: Boolean);
var
  Match: TDictionary<string, TObject>;
  VarName: TPair<string, TObject>;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  MatchIndex: Integer;
begin
  MatchIndex := 0;
  for Match in AResults do
    try
      Inc(MatchIndex);
      MemoResults.Lines.Add(Format('*** Coincidencia #%d ***', [MatchIndex]));
      for VarName in Match do
      begin
        if VarName.Value is TAiRagGraphNode then
        begin
          Node := VarName.Value as TAiRagGraphNode;
          MemoResults.Lines.Add(Format('  %s (%s): %s', [VarName.Key, Node.NodeLabel, Node.Name]));

          if AShowProperties then
            MatchBuilder_DisplayProperties(Node.Properties, MemoResults.Lines, '    ');
        end
        else if VarName.Value is TAiRagGraphEdge then
        begin
          Edge := VarName.Value as TAiRagGraphEdge;
          MemoResults.Lines.Add(Format('  %s (%s): %s --%s--> %s', [VarName.Key, Edge.EdgeLabel, Edge.FromNode.Name, Edge.EdgeLabel, Edge.ToNode.Name]));

          if AShowProperties then
            MatchBuilder_DisplayProperties(Edge.Properties, MemoResults.Lines, '    ');
        end;
      end;
      MemoResults.Lines.Add('');
    finally
      Match.Free;
    end;
end;

procedure TMainRagGraphDemo.MatchBuilder_DisplayProperties(AProperties: TDictionary<string, Variant>; AOutput: TStrings; const AIndentation: string);
var
  Pair: TPair<string, Variant>;
begin
  if (AProperties = nil) or (AProperties.Count = 0) then
    Exit;

  for Pair in AProperties do
  begin
    AOutput.Add(Format('%s• %s: %s', [AIndentation, Pair.Key, VarToStr(Pair.Value)]));
  end;
end;

procedure TMainRagGraphDemo.MatchBuilder_DisplaySubgraphResults(const AResults: TArray<TDictionary<string, TObject>>; AShowProperties: Boolean);
var
  Item: TDictionary<string, TObject>;
  ElementTypeObj: TObject;
  ElementTypeStr: string;
  ElementObj: TObject;
  Node: TAiRagGraphNode;
  Edge: TAiRagGraphEdge;
  NodeList: TStringList;
  EdgeList: TStringList;
begin
  NodeList := TStringList.Create;
  EdgeList := TStringList.Create;
  try
    for Item in AResults do
      try
        ElementTypeStr := '';
        if Item.TryGetValue('type', ElementTypeObj) and (ElementTypeObj is TStringWrapper) then
          ElementTypeStr := (ElementTypeObj as TStringWrapper).Value;

        if Item.TryGetValue('element', ElementObj) then
        begin
          if (ElementTypeStr = 'node') and (ElementObj is TAiRagGraphNode) then
          begin
            Node := ElementObj as TAiRagGraphNode;
            NodeList.Add(Format('- %s (%s)', [Node.Name, Node.NodeLabel]));
            // --- INICIO DE LA MEJORA ---
            if AShowProperties then
              MatchBuilder_DisplayProperties(Node.Properties, NodeList, '    ');
            // --- FIN DE LA MEJORA ---
          end
          else if (ElementTypeStr = 'edge') and (ElementObj is TAiRagGraphEdge) then
          begin
            Edge := ElementObj as TAiRagGraphEdge;
            EdgeList.Add(Format('- %s --[%s]--> %s', [Edge.FromNode.Name, Edge.EdgeLabel, Edge.ToNode.Name]));
            // --- INICIO DE LA MEJORA ---
            if AShowProperties then
              MatchBuilder_DisplayProperties(Edge.Properties, EdgeList, '    ');
            // --- FIN DE LA MEJORA ---
          end;
        end;
      finally
        if Item.TryGetValue('type', ElementTypeObj) then
          ElementTypeObj.Free;
        Item.Free;
      end;

    // ... (El código para mostrar los TStringList en el TMemo no cambia)
    MemoResults.Lines.Clear;
    MemoResults.Lines.Add(Format('--- Subgrafo Expandido: %d Nodos y %d Aristas ---', [NodeList.Count, EdgeList.Count]));
    MemoResults.Lines.Add('');
    MemoResults.Lines.Add('--- Nodos ---');
    if NodeList.Count > 0 then
    begin
      NodeList.Sort;
      MemoResults.Lines.AddStrings(NodeList);
    end
    else
      MemoResults.Lines.Add('(No se encontraron nodos)');
    MemoResults.Lines.Add('');
    MemoResults.Lines.Add('--- Aristas ---');
    if EdgeList.Count > 0 then
    begin
      EdgeList.Sort;
      MemoResults.Lines.AddStrings(EdgeList);
    end
    else
      MemoResults.Lines.Add('(No se encontraron aristas internas)');
  finally
    NodeList.Free;
    EdgeList.Free;
  end;
end;

// ----- Funciones de búsqueda por match ------

function TMainRagGraphDemo.MatchBuilder_GetNextNodeVar: string;
begin
  Result := 'n' + IntToStr(FNodeVarCounter);
  Inc(FNodeVarCounter);
end;

procedure TMainRagGraphDemo.MatchBuilder_PopulateComboBoxes;
var
  ALabel: string;
  List: TStringList;
begin

  List := TStringList.Create;
  Try

    // Limpiar y llenar ComboBoxes de etiquetas de nodos
    cbStartNodeLabel.Items.Clear;
    cbTargetNodeLabel.Items.Clear;
    for ALabel in RAG.GetUniqueNodeLabels do
    begin
      List.Add(ALabel);
    end;

    List.Sort;
    List.Insert(0, '');

    cbStartNodeLabel.Items.Text := List.Text;
    cbTargetNodeLabel.Items.Text := List.Text;

    if cbStartNodeLabel.Items.Count > 0 then
      cbStartNodeLabel.ItemIndex := 0;

    if cbTargetNodeLabel.Items.Count > 0 then
      cbTargetNodeLabel.ItemIndex := 0;

    // Limpiar y llenar ComboBox de etiquetas de aristas
    cbEdgeLabel.Items.Clear;
    List.Clear;
    for ALabel in RAG.GetUniqueEdgeLabels do
    begin
      List.Add(ALabel);
    end;
    List.Sort;
    List.Insert(0, '');

    cbEdgeLabel.Items.Text := List.Text;

    if cbEdgeLabel.Items.Count > 0 then
      cbEdgeLabel.ItemIndex := 0;
  Finally
    List.Free;
  End;
end;

procedure TMainRagGraphDemo.MatchBuilder_PopulateSuggestions(const SearchText: string);
var
  SelectedLabel: string;
  SuggestedNames: TArray<string>;
  AName: string;
begin
  // Si no hay texto, ocultamos el popup y salimos
  if SearchText.Trim.IsEmpty then
  begin
    popSuggestions.IsOpen := False;
    Exit;
  end;

  // Obtenemos la etiqueta seleccionada.
  if (cbStartNodeLabel.ItemIndex <= 0) or (cbStartNodeLabel.Selected = nil) then // <= 0 para ignorar el item vacío
  begin
      popSuggestions.IsOpen := False;
      Exit;
  end;
  SelectedLabel := cbStartNodeLabel.Selected.Text;
  if SelectedLabel.IsEmpty then
  begin
      popSuggestions.IsOpen := False;
      Exit;
  end;


  // Limpiamos la lista de sugerencias anterior
  lbSuggestions.Clear;

  // --- LÍNEA CLAVE CORREGIDA ---
  // Llamamos a la nueva función del grafo, que delegará a nuestra consulta SQL.
  SuggestedNames := RAG.FindNodeNamesByLabel(SelectedLabel, SearchText, 15); // Límite de 15 sugerencias

  // Poblamos el ListBox con los resultados
  for AName in SuggestedNames do
  begin
    lbSuggestions.Items.Add(AName);
  end;
  // --- FIN DE LA CORRECCIÓN ---

  // Mostramos u ocultamos el popup basado en si encontramos resultados
  if lbSuggestions.Items.Count > 0 then
  begin
    // Posicionar el popup debajo del TEdit
    {
    var LPoint := TPointF.Create(0, edtStartNodeName.Height);
    LPoint := edtStartNodeName.LocalToAbsolute(LPoint);
    LPoint := Self.AbsoluteToLocal(LPoint);
    popSuggestions.Position.Point := LPoint;
    popSuggestions.Width := edtStartNodeName.Width;
    }
    popSuggestions.IsOpen := True;
  end
  else
  begin
    popSuggestions.IsOpen := False;
  end;
end;

procedure TMainRagGraphDemo.MatchBuilder_UpdateQueryVisualizer;
var
  i: Integer;
  Clause: TMatchClause;
  SourcePattern, TargetPattern: TMatchNodePattern;
  DirectionStr: string;
begin
  lbQuerySteps.Clear;
  if FCurrentQuery.MatchClauses.Count = 0 then
    Exit;

  // Visualizar la primera cláusula de forma completa
  Clause := FCurrentQuery.MatchClauses[0];
  SourcePattern := FCurrentQuery.NodePatternByVariable[Clause.SourceNodeVar];
  TargetPattern := FCurrentQuery.NodePatternByVariable[Clause.TargetNodeVar];
  case Clause.EdgePattern.Direction of
    gdOutgoing:
      DirectionStr := '-[' + Clause.EdgePattern.Variable + ':' + Clause.EdgePattern.EdgeLabel + ']->';
    gdIncoming:
      DirectionStr := '<-[' + Clause.EdgePattern.Variable + ':' + Clause.EdgePattern.EdgeLabel + ']-';
  else
    DirectionStr := '-[' + Clause.EdgePattern.Variable + ':' + Clause.EdgePattern.EdgeLabel + ']-';
  end;
  lbQuerySteps.Items.Add(Format('(%s:%s) %s (%s:%s)', [SourcePattern.Variable, SourcePattern.NodeLabel, DirectionStr, TargetPattern.Variable, TargetPattern.NodeLabel]));

  // Visualizar las cláusulas siguientes
  for i := 1 to FCurrentQuery.MatchClauses.Count - 1 do
  begin
    Clause := FCurrentQuery.MatchClauses[i];
    TargetPattern := FCurrentQuery.NodePatternByVariable[Clause.TargetNodeVar];
    case Clause.EdgePattern.Direction of
      gdOutgoing:
        DirectionStr := '-[' + Clause.EdgePattern.Variable + ':' + Clause.EdgePattern.EdgeLabel + ']->';
      gdIncoming:
        DirectionStr := '<-[' + Clause.EdgePattern.Variable + ':' + Clause.EdgePattern.EdgeLabel + ']-';
    else
      DirectionStr := '-[' + Clause.EdgePattern.Variable + ':' + Clause.EdgePattern.EdgeLabel + ']-';
    end;
    lbQuerySteps.Items.Add(Format('%s (%s:%s)', [DirectionStr, TargetPattern.Variable, TargetPattern.NodeLabel]));
  end;
end;

end.
