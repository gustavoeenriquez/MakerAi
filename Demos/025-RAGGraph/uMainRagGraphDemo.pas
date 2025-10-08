unit uMainRagGraphDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  uMakerAi.RAG.Graph.Core, uMakerAi.RAG.Graph.Builder, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings, uMakerAi.Chat.OpenAi, uMakerAi.RAG.Vectors, FMX.Edit, uMakerAi.Prompts,
  uMakerAi.Chat.AiConnection, uMakerAi.Chat, uMakerAi.Core, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  uMakerAi.Chat.Gemini,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.PGDef, FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.PG, Data.DB, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FMX.Objects, FMX.Layouts, FMX.TabControl;

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
    Builder1: TAiRagGraphBuilder;
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
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnBusquedaRAGClick(Sender: TObject);
    procedure BtnAddNodosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RagGraphAddEdge(Sender: TObject; AEdge: TAiRagGraphEdge; var Handled: Boolean);
    procedure RagGraphAddNode(Sender: TObject; ANode: TAiRagGraphNode; var Handled: Boolean);
    procedure RagGraphDeleteEdge(Sender: TObject; const AEdgeID: string; var Handled: Boolean);
    procedure RagGraphDeleteNode(Sender: TObject; const ANodeID: string; var Handled: Boolean);
    procedure RagGraphFindNodeByID(Sender: TObject; const ANodeID: string; var ResultNode: TAiRagGraphNode; var Handled: Boolean);
    procedure RagGraphLoad(Sender: TObject; Graph: TAiRagGraph; var Handled: Boolean);
    procedure RagGraphQuery(Sender: TObject; const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double; ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
    procedure RagGraphSearchNodes(Sender: TObject; const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; out ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
    procedure BtnConsultaQueryLLMClick(Sender: TObject);
    procedure BtnMatchClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FCurrentEntidad: String;
    Function NewQuery: TFDQuery;
    function EmbeddingToString(const AData: TAiEmbeddingData): string;
    function PropertiesToJSONString(const AProperties: TDictionary<string, Variant>): string;
    function JSONValueToVariant(AJsonValue: TJSONValue): Variant;
    procedure JSONStringToProperties(const AJSONString: string; const AProperties: TDictionary<string, Variant>);
    function StringToEmbedding(const AVectorString: string): TAiEmbeddingData;
    function HydrateNodeFromQuery(AQuery: TFDQuery; AGraph: TAiRagGraph): TAiRagGraphNode;

  public
    function ParseJsonToQueryPlan(const AJSONString: string): TQueryPlan;
  end;

var
  MainRagGraphDemo: TMainRagGraphDemo;

const
  // JSON de prueba con una entidad duplicada ("Apple Inc.") para probar la reconciliación
  JSON_TRIPLETAS_1 = '[' + '  {' + '    "subject": {"name": "Steve Jobs", "nodeLabel": "Persona"},' + '    "predicate": {"edgeLabel": "FUNDÓ", "name": "Cofundador en 1976", "properties": {"rol": "Cofundador"}},' +
    '    "object": {"name": "Apple Inc.", "nodeLabel": "Empresa", "properties": {"industria": "Tecnología"}}' + '  },' + '  {' + '    "subject": {"name": "Steve Wozniak", "nodeLabel": "Persona"},' +
    '    "predicate": {"edgeLabel": "FUNDÓ", "name": "Cofundador en 1976", "properties": {"rol": "Cofundador"}},' + '    "object": {"name": "Apple Inc.", "nodeLabel": "Empresa"}' + '  },' + '  {' +
    '    "subject": {"name": "Mike Markkula", "nodeLabel": "Persona"},' + '    "predicate": {"edgeLabel": "INVIRTIÓ_EN", "name": "Inversión Ángel", "properties": {"cantidad_usd": 250000}},' +
    '    "object": {"name": "Apple Inc.", "nodeLabel": "Empresa"}' + '  }' + ']';

  // Segundo JSON para probar la fusión de propiedades en nodos existentes
  JSON_TRIPLETAS_2 = '[' + '  {' + '    "subject": {"name": "Apple Inc.", "nodeLabel": "Empresa", "properties": {"sede": "Cupertino, CA"}},' + '    "predicate": {"edgeLabel": "TIENE_SEDE_EN"},' +
    '    "object": {"name": "Cupertino", "nodeLabel": "Ciudad"}' + '  }' + ']';

  JSON_SOBRESCRITURA = '[' + '  {' + '    "subject": {"name": "Apple Inc.", "nodeLabel": "Empresa", "properties": {"industria": "Electrónica de Consumo"}},' + '    "predicate": {"edgeLabel": "ACTUALIZAR_PROPIEDAD"},' +
    '    "object": {"name": "Apple Inc.", "nodeLabel": "Empresa"}' + // El objeto puede ser el mismo sujeto
    '  }' + ']';

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

  Prompt := AiPrompts1.GetTemplate('CreaJSonFromTextoMejorado', ['texto=' + Ent]);

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
      Builder1.Process(MF.ToString);
    End;
    ShowMessage('Se almacenó correctamente el json- Nodos:'+RAG.NodeCount.ToString);
  End
  Else
    ShowMessage('Error al almacenar el Json');
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

procedure TMainRagGraphDemo.BtnConsultaQueryLLMClick(Sender: TObject);
var
  Ent, Prompt, Res, JSonPlanStr: String;
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
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

procedure TMainRagGraphDemo.BtnLoadClick(Sender: TObject);
Var
  St: TMemoryStream;
begin
  OpenDialog1.DefaultExt := '.json';
  OpenDialog1.InitialDir := 'c:\temp\'; // raggraph.data.json

  If OpenDialog1.Execute then
  Begin
    St := TMemoryStream.Create;
    St.LoadFromFile(OpenDialog1.FileName);
    St.Position := 0;
    RAG.LoadFromStream(St);
    SaveDialog1.FileName := OpenDialog1.FileName;
    St.Free;
  End;
end;

procedure TMainRagGraphDemo.BtnMatchClick(Sender: TObject);
begin
  Raise Exception.Create('Falta por Implementar en el grafo');
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

procedure TMainRagGraphDemo.Button1Click(Sender: TObject);
begin
  Rag.Clear;
  MemoToProcess.Lines.Clear;
  MemoProcessOut.Lines.Clear;

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

procedure TMainRagGraphDemo.JSONStringToProperties(const AJSONString: string; const AProperties: TDictionary<string, Variant>);
var
  JsonValue: TJSONValue;
  JsonObj: TJSONObject;
  Pair: TJSonPair;
begin
  if (AProperties = nil) or AJSONString.IsEmpty or (AJSONString = '{}') then
    Exit;

  AProperties.Clear; // Limpiamos el diccionario antes de poblarlo.
  JsonValue := TJSONObject.ParseJSONValue(AJSONString);
  if JsonValue = nil then
    Exit; // JSON inválido

  try
    if JsonValue is TJSONObject then
    begin
      JsonObj := JsonValue as TJSONObject;
      for Pair in JsonObj do
      begin
        AProperties.Add(Pair.JsonString.Value, JSONValueToVariant(Pair.JsonValue));
      end;
    end;
  finally
    JsonValue.Free;
  end;
end;

function TMainRagGraphDemo.JSONValueToVariant(AJsonValue: TJSONValue): Variant;
begin
  if AJsonValue = nil then
    Result := Null
  else if AJsonValue is TJSONString then
    Result := AJsonValue.Value
  else if AJsonValue is TJSONNumber then
    Result := StrToFloat(AJsonValue.Value) // Considerar StrToFloat con TFormatSettings.Invariant si hay problemas de localización
  else if AJsonValue is TJSONTrue then
    Result := True
  else if AJsonValue is TJSONFalse then
    Result := False
  else if AJsonValue is TJSONNull then
    Result := Null
  else
    // Para objetos y arrays anidados, por simplicidad los devolvemos como string.
    // Una versión más avanzada podría convertirlos a TObjectDictionary o TArray<Variant>.
    Result := AJsonValue.ToString;
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

  JsonObj := TJSONObject.Create;
  try
    for Pair in AProperties do
    begin
      // Una implementación más robusta usaría una función VariantToJSONValue
      JsonObj.AddPair(Pair.Key, TJSONString.Create(VarToStr(Pair.Value)));
    end;
    Result := JsonObj.ToString;
  finally
    JsonObj.Free;
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

procedure TMainRagGraphDemo.RagGraphFindNodeByID(Sender: TObject; const ANodeID: string; var ResultNode: TAiRagGraphNode; var Handled: Boolean);
var
  Query: TFDQuery;
  Graph: TAiRagGraph;
  Dim: Integer;
begin
  ResultNode := nil;
  Query := NewQuery;
  try
    Query.SQL.Text := 'SELECT id, node_label, name, properties, embedding ' + 'FROM graph_nodes ' + 'WHERE id = :id AND entidad = :entidad';

    Query.ParamByName('id').AsString := ANodeID;
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.Open;

    if not Query.IsEmpty then
    begin
      // Nodo encontrado, ahora lo reconstruimos en un objeto Delphi.
      Graph := Sender as TAiRagGraph;
      // Determinar la dimensión. Si no está establecida, podríamos intentar inferirla
      // del primer embedding que carguemos, pero es mejor tenerla pre-configurada.
      if Graph.Nodes.Dim > 0 then
        Dim := Graph.Nodes.Dim
      else
        Dim := 1536;

      ResultNode := TAiRagGraphNode.Create(Graph, Dim);
      ResultNode.ID := Query.FieldByName('id').AsString;
      ResultNode.NodeLabel := Query.FieldByName('node_label').AsString;
      ResultNode.Name := Query.FieldByName('name').AsString;

      if not Query.FieldByName('properties').IsNull then
        JSONStringToProperties(Query.FieldByName('properties').AsString, ResultNode.Properties);

      if not Query.FieldByName('embedding').IsNull then
        ResultNode.Data := StringToEmbedding(Query.FieldByName('embedding').AsString);
    end;

    Handled := True;

  finally
    Query.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphLoad(Sender: TObject; Graph: TAiRagGraph; var Handled: Boolean);
begin
  //
end;

procedure TMainRagGraphDemo.RagGraphQuery(Sender: TObject; const APlan: TQueryPlan; ADepth, ALimit: Integer; APrecision: Double; ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
Var
  Graph: TAiRagGraph;
  Query: TFDQuery;
  Step: TQueryStep;
  QueryEmbedding: TAiEmbeddingData;
  EmbeddingStr: String;
begin
  // NOTA: Esta es una implementación simplificada para demostrar el concepto.
  // Un manejador completo necesitaría un generador de SQL dinámico que parsee todos los pasos.

  // Solo manejaremos planes con un ancla y un solo paso hacia adelante.
  if Length(APlan.Steps) <> 1 then
  begin
    Handled := False; // Dejamos que la implementación en memoria se encargue
    Exit;
  end;

  Graph := Sender as TAiRagGraph;
  Query := NewQuery;
  Step := APlan.Steps[0];
  QueryEmbedding := Graph.Embeddings.CreateEmbedding(APlan.AnchorPrompt, 'user');
  EmbeddingStr := EmbeddingToString(QueryEmbedding);
  var
  NodeDict := TDictionary<string, TAiRagGraphNode>.Create;
  var
  NodeList := TObjectList<TAiRagGraphNode>.Create;

  try
    // La consulta combina la búsqueda semántica con un JOIN estructural
    Query.FetchOptions.Mode := fmAll;
    Query.ResourceOptions.CmdExecMode := amBlocking;
    Query.SQL.Clear;
    Query.SQL.Add('WITH AnchorNodes AS (');
    Query.SQL.Add('  SELECT id');
    Query.SQL.Add('  FROM graph_nodes');
    Query.SQL.Add('  WHERE entidad = :entidad AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold');
    Query.SQL.Add('  ORDER BY embedding <-> ''' + EmbeddingStr + ''' LIMIT :limit');
    Query.SQL.Add(')');
    Query.SQL.Add('SELECT final_node.*');
    Query.SQL.Add('FROM AnchorNodes an');
    Query.SQL.Add('JOIN graph_edges e ON an.id = e.source_node_id AND e.entidad = :entidad AND e.edge_label = :edge_label');
    Query.SQL.Add('JOIN graph_nodes final_node ON e.target_node_id = final_node.id AND final_node.entidad = :entidad');
    Query.SQL.Add('WHERE final_node.node_label = :target_label');

    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    // Query.ParamByName('embedding').AsString := EmbeddingStr;
    Query.ParamByName('distance_threshold').AsFloat := 1 - APrecision;
    Query.ParamByName('limit').AsInteger := ALimit;
    Query.ParamByName('edge_label').AsString := Step.EdgeLabel;
    Query.ParamByName('target_label').AsString := Step.TargetNodeLabel;
    Query.Open;

    // Hidratar los nodos resultantes
    while not Query.Eof do
    begin
      NodeList.Add(HydrateNodeFromQuery(Query, Graph));
      Query.Next;
    end;

    ResultNodes := NodeList.ToArray;
    NodeList.Clear;
    Handled := True;

  finally
    Query.Free;
    NodeDict.Free;
    NodeList.Free;
  end;
end;

procedure TMainRagGraphDemo.RagGraphSearchNodes(Sender: TObject; const APrompt: string; ADepth, ALimit: Integer; APrecision: Double; out ResultNodes: TArray<uMakerAi.RAG.Graph.Core.TAiRagGraphNode>; var Handled: Boolean);
var
  Query: TFDQuery;
  Graph: TAiRagGraph;
  QueryEmbedding: TAiEmbeddingData;
  EmbeddingStr: string;
  NodeDict: TDictionary<string, TAiRagGraphNode>;
  NodeList: TObjectList<TAiRagGraphNode>;
  SourceNode, TargetNode: TAiRagGraphNode;
begin
  SetLength(ResultNodes, 0);
  Graph := Sender as TAiRagGraph;

  // Paso 1: Obtener el embedding para el prompt de búsqueda
  if not Assigned(Graph.Embeddings) then
    raise Exception.Create('El motor de Embeddings no está asignado al grafo.');
  QueryEmbedding := Graph.Embeddings.CreateEmbedding(APrompt, 'user');
  EmbeddingStr := EmbeddingToString(QueryEmbedding);

  Query := NewQuery;
  NodeDict := TDictionary<string, TAiRagGraphNode>.Create;
  NodeList := TObjectList<TAiRagGraphNode>.Create; // TObjectList para gestionar la memoria
  try
    // Paso 2: Construir y ejecutar la consulta SQL
    if ADepth = 0 then
    begin
      // --- CASO SIMPLE: Solo búsqueda semántica ---
      Query.SQL.Add('SELECT id, node_label, name, properties, embedding, embedding <-> ''' + EmbeddingStr + ''' AS distance ');
      Query.SQL.Add('FROM graph_nodes ');
      Query.SQL.Add('WHERE entidad = :entidad');
      Query.SQL.Add('  AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold ');
      Query.SQL.Add('ORDER BY distance LIMIT :limit');

      Query.ParamByName('distance_threshold').AsFloat := 1 - APrecision;
    end
    else
    begin
      // --- CASO COMPLEJO: Búsqueda semántica + Expansión Estructural con CTE Recursiva ---
      // Esta consulta es el corazón del sistema híbrido en la base de datos.
      Query.FetchOptions.Mode := fmAll;
      Query.ResourceOptions.CmdExecMode := amBlocking;
      Query.SQL.Clear;
      Query.SQL.Add('WITH RECURSIVE traversal AS (');
      Query.SQL.Add('  -- Ancla: Nodos iniciales de la búsqueda semántica');
      Query.SQL.Add('  SELECT id, 1 AS depth, ARRAY[id] AS path');
      Query.SQL.Add('  FROM (');
      Query.SQL.Add('    SELECT id');
      Query.SQL.Add('    FROM graph_nodes');
      Query.SQL.Add('    WHERE entidad = :entidad AND (embedding <-> ''' + EmbeddingStr + ''') < :distance_threshold');
      Query.SQL.Add('    ORDER BY embedding <-> ''' + EmbeddingStr + '''');
      Query.SQL.Add('    LIMIT :limit');
      Query.SQL.Add('  ) AS initial');
      Query.SQL.Add(' ');
      Query.SQL.Add('  UNION ALL');
      Query.SQL.Add(' ');
      Query.SQL.Add('  -- Paso recursivo: expandir a los vecinos');
      Query.SQL.Add('  SELECT');
      Query.SQL.Add('    CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END AS id,');
      Query.SQL.Add('    t.depth + 1,');
      Query.SQL.Add('    t.path || CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END');
      Query.SQL.Add('  FROM traversal t');
      Query.SQL.Add('  JOIN graph_edges e ON (e.source_node_id = t.id OR e.target_node_id = t.id) AND e.entidad = :entidad');
      Query.SQL.Add('  WHERE t.depth < :depth');
      Query.SQL.Add('    AND NOT (CASE WHEN e.source_node_id = t.id THEN e.target_node_id ELSE e.source_node_id END = ANY(t.path))');
      Query.SQL.Add(')');
      Query.SQL.Add(' ');
      Query.SQL.Add('-- Consulta final: obtener todos los nodos y aristas del subgrafo');
      Query.SQL.Add('SELECT');
      Query.SQL.Add('  n.id, n.node_label, n.name, n.properties, n.embedding,');
      Query.SQL.Add('  e.id AS edge_id, e.edge_label, e.source_node_id, e.target_node_id, e.weight, e.properties AS edge_properties');
      Query.SQL.Add('FROM traversal');
      Query.SQL.Add('JOIN graph_nodes n ON n.id = traversal.id AND n.entidad = :entidad');
      Query.SQL.Add('LEFT JOIN graph_edges e ON (e.source_node_id = n.id OR e.target_node_id = n.id) AND e.entidad = :entidad');

      Query.ParamByName('distance_threshold').AsFloat := 1 - APrecision;
      Query.ParamByName('depth').AsInteger := ADepth;
    end;

    // Parámetros comunes a ambas consultas
    Query.ParamByName('entidad').AsString := FCurrentEntidad;
    Query.ParamByName('limit').AsInteger := ALimit;
    Query.Open;

    // Paso 3: Hidratar los objetos desde el resultado de la consulta
    // Usamos el patrón de 2 pasadas para reconstruir las relaciones

    // Pasada 1: Crear todos los nodos y guardarlos en un diccionario para acceso rápido
    Query.First;
    while not Query.Eof do
    begin
      if not NodeDict.ContainsKey(Query.FieldByName('id').AsString) then
      begin
        var
        Node := HydrateNodeFromQuery(Query, Graph);
        NodeDict.Add(Node.ID, Node);
        NodeList.Add(Node); // TObjectList se encargará de liberar la memoria
      end;
      Query.Next;
    end;

    // Pasada 2 (solo para ADepth > 0): Conectar los nodos con las aristas
    if ADepth > 0 then
    begin
      Query.First;
      while not Query.Eof do
      begin
        if not Query.FieldByName('edge_id').IsNull then
        begin
          if NodeDict.TryGetValue(Query.FieldByName('source_node_id').AsString, SourceNode) and NodeDict.TryGetValue(Query.FieldByName('target_node_id').AsString, TargetNode) then
          begin
            // Comprobar si la arista ya fue creada para este par de nodos
            if Graph.FindEdge(SourceNode, TargetNode, Query.FieldByName('edge_label').AsString) = nil then
            begin
              var
              Edge := TAiRagGraphEdge.Create(Graph, Graph.Edges.Dim);
              Edge.ID := Query.FieldByName('edge_id').AsString;
              Edge.EdgeLabel := Query.FieldByName('edge_label').AsString;
              Edge.Weight := Query.FieldByName('weight').AsFloat;
              Edge.FromNode := SourceNode;
              Edge.ToNode := TargetNode;
              if not Query.FieldByName('edge_properties').IsNull then
                JSONStringToProperties(Query.FieldByName('edge_properties').AsString, Edge.Properties);

              // Conectar la arista a los nodos en memoria
              SourceNode.InternalAddOutgoingEdge(Edge);
              TargetNode.InternalAddIncomingEdge(Edge);
            end;
          end;
        end;
        Query.Next;
      end;
    end;

    ResultNodes := NodeList.ToArray;
    NodeList.OwnsObjects := False;
    NodeList.Clear; // Limpiamos la lista para no liberar los nodos que vamos a devolver
    Handled := True;

  finally
    Query.Free;
    NodeDict.Free;
    NodeList.Free; // Libera la memoria de los nodos si hubo una excepción
  end;
end;

function TMainRagGraphDemo.StringToEmbedding(const AVectorString: string): TAiEmbeddingData;
var
  CleanedString: string;
  ValueStrings: TArray<string>;
  i: Integer;
  FormatSettings: TFormatSettings;
begin
  Result := []; // Devuelve un array vacío por defecto
  if AVectorString.IsEmpty or (AVectorString = '[]') then
    Exit;

  // 1. Limpiar el string, quitando los corchetes
  CleanedString := AVectorString.Trim(['[', ']']);
  if CleanedString.IsEmpty then
    Exit;

  // 2. Separar los valores por la coma
  ValueStrings := CleanedString.Split([',']);

  // 3. Preparar para la conversión de float insensible a la localización
  // Esto asegura que el '.' siempre se interprete como el separador decimal.
  FormatSettings := TFormatSettings.Invariant;

  // 4. Convertir cada valor y añadirlo al resultado
  SetLength(Result, Length(ValueStrings));
  for i := 0 to High(ValueStrings) do
  begin
    // Usamos TryStrToFloat para más seguridad contra datos mal formados
    if TryStrToFloat(ValueStrings[i], Result[i], FormatSettings) then
    begin
      // La conversión fue exitosa, continuar.
    end
    else
    begin
      Result[i] := 0.0; // O manejar el error como se prefiera
    end;
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

end.
