unit uMainRAGVQL;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Threading,   System.Generics.Collections, System.StrUtils, system.Math,

   uMakerAi.Embeddings.core, uMakerAi.Embeddings, uMakerAi.Chat.Ollama, uMakerAi.rag.Vector.Driver.Postgres,
  uMakerAi.rag.Vectors, uMakerAi.rag.Vectors.Index, uMakerAi.RAG.Vectors.VQL,  uMakerAi.RAG.MetaData,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.PG, FireDAC.Phys.PGDef, FireDAC.FMXUI.Wait, Data.DB, FireDAC.Comp.Client, FMX.Layouts;

type
  TForm21 = class(TForm)
    BtnUploadFile: TButton;
    AiOllamaEmbeddings1: TAiOllamaEmbeddings;
    rag: TAiRAGVector;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    BtnCancelar: TButton;
    ChShowProperties: TCheckBox;
    EditPrecision: TEdit;
    EditLimit: TEdit;
    Label10: TLabel;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    BtnSaveToFile: TButton;
    BtnLoadFromfile: TButton;
    ChLexicalSearch: TCheckBox;
    ChEnableRRF: TCheckBox;
    ChReorderABC: TCheckBox;
    ChSemanticSearch: TCheckBox;
    DbConn: TFDConnection;
    PgDriver: TAiRAGVectorPostgresDriver;
    BtnCreateTable: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    BtnConsultar: TButton;
    BtnBuscarVGQL: TButton;
    BtnVGqlToText: TButton;
    Label1: TLabel;
    Label3: TLabel;
    MemoResponse: TMemo;
    MemoPrompt: TMemo;
    Splitter1: TSplitter;
    BtnConsultaToText: TButton;
    procedure BtnUploadFileClick(Sender: TObject);
    procedure RAGImportProgress(Sender: TObject; aPosition, Total: Integer; var Cancel: Boolean);
    procedure BtnConsultarClick(Sender: TObject);
    procedure BtnSaveToFileClick(Sender: TObject);
    procedure BtnLoadFromfileClick(Sender: TObject);
    procedure BtnBuscarVGQLClick(Sender: TObject);
    procedure ChLexicalSearchChange(Sender: TObject);
    procedure ChEnableRRFChange(Sender: TObject);
    procedure ChSemanticSearchChange(Sender: TObject);
    procedure BtnCreateTableClick(Sender: TObject);
    procedure BtnVGqlToTextClick(Sender: TObject);
    procedure BtnConsultaToTextClick(Sender: TObject);
  private
    FUserCancelled: Boolean; // Variable para comunicar la cancelación
  public
    Procedure ShowAllRAG;
  end;

var
  Form21: TForm21;

implementation

{$R *.fmx}

procedure TForm21.BtnConsultarClick(Sender: TObject);
Var
  // Variables locales para capturar el estado de la UI (Thread-Safety)
  LPrompt: String;
  LShowProperties: Boolean;
  LLimit: Integer;
  LPrecision: Double;
  // Variables para las nuevas opciones de búsqueda
  LUseBM25, LUseRRF, LUseABC: Boolean;
begin
  // =========================================================================
  // 1. CAPTURA DE DATOS (HILO PRINCIPAL)
  // =========================================================================
  // Es vital leer todas las propiedades VCL/FMX aquí, no dentro del Task.

  LPrompt := MemoPrompt.Lines.Text.Trim;

  if LPrompt.IsEmpty then
  begin
    ShowMessage('Por favor ingrese una consulta.');
    Exit;
  end;

  // Configuraciones básicas
  LShowProperties := ChShowProperties.IsChecked;
  LLimit := StrToIntDef(EditLimit.Text, 5);
  LPrecision := StrToFloatDef(EditPrecision.Text, 0.7);

  // Configuraciones del Motor RAG (Nuevos Checkboxes)
  // Asegúrate de que estos componentes existan en tu formulario con estos nombres
  // o cámbialos por los que estés usando.
  LUseBM25 := ChLexicalSearch.IsChecked;
  LUseRRF  := ChEnableRRF.IsChecked;
  LUseABC  := ChReorderABC.IsChecked;

  // =========================================================================
  // 2. PREPARAR UI VISUAL
  // =========================================================================
  BtnConsultar.Enabled := False;
  MemoResponse.Lines.Text := 'Pensando... (Generando embedding y buscando)';
  Cursor := crHourGlass;

  // =========================================================================
  // 3. EJECUTAR EN SEGUNDO PLANO (TTask)
  // =========================================================================
  TTask.Run(
    procedure
    Var
      LResponse: String;
    begin
      try
        // --- A. CONFIGURAR EL MOTOR ---
        // Aplicamos las opciones capturadas al componente.
        // Como el botón está deshabilitado, no hay riesgo de concurrencia aquí.

        rag.SearchOptions.UseEmbeddings := True; // Siempre activo para este demo
        rag.SearchOptions.UseBM25       := LUseBM25;
        rag.SearchOptions.UseRRF        := LUseRRF;
        rag.SearchOptions.UseReorderABC := LUseABC;

        // Opcional: Si tuvieras edits para los pesos
        // rag.SearchOptions.BM25Weight := 0.7;

        // --- B. PROCESO PESADO (BÚSQUEDA) ---
        // SearchText internamente llamará a Search(), que ahora usa SearchOptions
        LResponse := rag.SearchText(
          LPrompt,
          LLimit,
          LPrecision,
          nil,            // Filtro (nil)
          LShowProperties // Formato de salida
        );

        // --- C. ACTUALIZAR UI (HILO PRINCIPAL) ---
        TThread.Queue(nil,
          procedure
          begin
            // Verificación defensiva por si cerraron el form
            if not Assigned(MemoResponse) then Exit;

            MemoResponse.Lines.Text := LResponse;

            if LResponse.IsEmpty then
              MemoResponse.Lines.Add('(No se encontraron coincidencias con esa precisión)');

            // Restaurar controles
            BtnConsultar.Enabled := True;
            Cursor := crDefault;
          end);

      except
        on E: Exception do
        begin
          // Manejo de errores seguro
          TThread.Queue(nil,
            procedure
            begin
              MemoResponse.Lines.Text := 'Error en la búsqueda: ' + E.Message;
              BtnConsultar.Enabled := True;
              Cursor := crDefault;
            end);
        end;
      end;
    end);
end;

procedure TForm21.BtnConsultaToTextClick(Sender: TObject);
var
  SB: TStringBuilder;
  LPrompt: string;
  LUseEmbeddings, LUseBM25, LUseRRF: Boolean;
  LWeightSem, LWeightLex: Double;
begin
  // 1. Capturar valores de la UI
  LPrompt := MemoPrompt.Lines.Text.Trim;

  // Basado en la lógica de tu BtnConsultarClick:
  LUseEmbeddings := True; // Tu código fuerza esto a True
  // Si tienes un Checkbox para esto, usa: LUseEmbeddings := ChSemanticSearch.IsChecked;

  LUseBM25 := ChLexicalSearch.IsChecked;
  LUseRRF  := ChEnableRRF.IsChecked;

  // Pesos actuales (si no tienes Edits para esto, tomamos los del objeto RAG)
  LWeightSem := rag.SearchOptions.EmbeddingWeight;
  LWeightLex := rag.SearchOptions.BM25Weight;

  SB := TStringBuilder.Create;
  try
    SB.AppendLine('=== MANUAL CONFIG DEBUG REPORT ===');
    SB.AppendLine;

    // --- MATCH ---
    // En búsqueda manual directa no suele haber filtro de entidad a menos que se codifique
    SB.AppendLine('ENTITY (Target): [Global / Default]');
    SB.AppendLine;

    // --- SEARCH ---
    if LPrompt.IsEmpty then
      SB.AppendLine('QUERY: (Empty)')
    else
      SB.AppendLine('QUERY: ' + LPrompt);
    SB.AppendLine;

    // --- USING (Mode & Weights) ---
    SB.Append('MODE: ');
    if LUseEmbeddings and LUseBM25 then
      SB.Append('Hybrid (Semántico + Léxico)')
    else if LUseEmbeddings then
      SB.Append('Embeddings Only (Semántico)')
    else if LUseBM25 then
      SB.Append('BM25 Only (Léxico)')
    else
      SB.Append('NONE (Configuración inválida)');
    SB.AppendLine;

    if LUseEmbeddings and LUseBM25 then
    begin
      SB.AppendFormat('  - Weight Semantic: %.2f', [LWeightSem]).AppendLine;
      SB.AppendFormat('  - Weight Lexical:  %.2f', [LWeightLex]).AppendLine;
      SB.Append('  - Fusion Strategy: ');
      if LUseRRF then
        SB.Append('RRF (Reciprocal Rank Fusion)')
      else
        SB.Append('Weighted Sum');
      SB.AppendLine;
    end;

    // Idioma por defecto del componente
    SB.AppendFormat('  - Language: %s (System Default)', ['Spanish']).AppendLine;
    SB.AppendLine;

    // --- WHERE (Filters) ---
    SB.AppendLine('FILTERS (MetaData):');
    SB.AppendLine('  (La interfaz manual actual no tiene controles de filtro)');
    SB.AppendLine;

    // --- THRESHOLD ---
    SB.AppendLine('THRESHOLDS (Precision):');
    SB.AppendFormat('  - Min Global:   %s', [EditPrecision.Text]).AppendLine;
    // En modo manual simple, el MinSemantic suele ser igual al Global o ignorado según la lógica
    SB.AppendFormat('  - Min Semantic: %s (Inherited)', [EditPrecision.Text]).AppendLine;
    SB.AppendLine;

    // --- RERANK ---
    // Tu BtnConsultarClick llama a SearchText, no a ExecuteVGQL, por lo que
    // la lógica de Rerank (segunda pasada) no se ejecuta automáticamente ahí.
    SB.AppendLine('RERANKING: Inactive');
    SB.AppendLine('  (La búsqueda manual estándar es de una sola etapa)');
    SB.AppendLine;

    // --- OPTIMIZE ---
    SB.AppendLine('OPTIMIZATIONS:');
    SB.AppendFormat('  - Context Reordering (Lost-in-Middle): %s',
      [BoolToStr(ChReorderABC.IsChecked, True)]).AppendLine;
    SB.AppendLine;

    // --- RETURN ---
    SB.AppendLine('OUTPUT CONFIG:');
    SB.AppendFormat('  - Limit: %s', [EditLimit.Text]).AppendLine;
    SB.AppendFormat('  - Include Metadata: %s',
      [BoolToStr(ChShowProperties.IsChecked, True)]).AppendLine;
    SB.AppendFormat('  - Include Score:    %s',
      [BoolToStr(ChShowProperties.IsChecked, True)]).AppendLine;

    // 3. Mostrar resultado
    MemoResponse.Lines.Text := SB.ToString;

  finally
    SB.Free;
  end;
end;

procedure TForm21.BtnCreateTableClick(Sender: TObject);
begin
  PgDriver.CreateSchema(PgDriver.TableName, 1024);
end;

procedure TForm21.BtnLoadFromfileClick(Sender: TObject);
var
  ItemsCargados: Integer;
begin
  OpenDialog1.DefaultExt := 'mkVRag';
  OpenDialog1.Filter := 'MakerAI Vector Files (*.mkVRag)|*.mkVRag|Todos los archivos (*.*)|*.*';

  if OpenDialog1.Execute then
  begin
    Cursor := crHourGlass;
    try
      // Capturamos el resultado
      ItemsCargados := rag.LoadFromFile(OpenDialog1.FileName);
      Rag.BuildIndex;

      SaveDialog1.FileName := OpenDialog1.FileName;

      ShowAllRAG;

      ShowMessage('Archivo cargado correctamente.' + sLineBreak + 'Nodos recuperados: ' + IntToStr(ItemsCargados));
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TForm21.BtnSaveToFileClick(Sender: TObject);
begin
  // 1. Configurar propiedades del Diálogo
  SaveDialog1.DefaultExt := 'mkVRag';
  SaveDialog1.Filter := 'MakerAI Vector Files (*.mkVRag)|*.mkVRag|Todos los archivos (*.*)|*.*';
  SaveDialog1.Title := 'Guardar Base de Vectores';

  // 2. Ejecutar
  if SaveDialog1.Execute then
  begin
    Cursor := crHourGlass; // Feedback visual de guardado
    try
      // 3. Lógica de Guardado
      rag.SaveToFile(SaveDialog1.FileName);

      // 4. Sincronización
      OpenDialog1.FileName := SaveDialog1.FileName;

      ShowMessage('Base de conocimientos guardada exitosamente.');
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TForm21.BtnUploadFileClick(Sender: TObject);
Var
  FileName: String;
begin
  // 1. Interacción UI (Hilo Principal)
  // Abrimos el diálogo antes de iniciar la tarea
  If not OpenDialog1.Execute then
    Exit;

  // Capturamos el nombre del archivo para pasarlo a la tarea
  FileName := OpenDialog1.FileName;

  // 2. Preparar UI
  FUserCancelled := False; // Resetear bandera de cancelación
  ProgressBar1.Value := 0; // Resetear barra
  BtnUploadFile.Enabled := False; // Evitar doble click
  rag.Clear; // Limpiamos (thread-safe gracias a tu FLock)

  // 3. Iniciar la Tarea en segundo plano
  TTask.Run(
    procedure
    Var
      St: TStringStream;
      LocalText: String;
      NodosCreados: Integer;
      MetaData : TAiEmbeddingMetaData;
    begin
      try
        // --- INICIO PROCESO BACKGROUND ---

        // A. Carga del archivo (Pesado para disco)
        St := TStringStream.Create('', TEncoding.UTF8);
        Try
          St.LoadFromFile(FileName);
          LocalText := St.DataString;
        Finally
          St.Free;
        End;

        // B. Procesamiento del RAG
        // Como ya configuraste el evento OnImportProgress en el form,
        // este método llamará automáticamente para actualizar la barra.
        if not FUserCancelled then
        begin
          MetaData := TAiEmbeddingMetaData.Create;
          MetaData.Properties['tipo'] := 'cuento';
          MetaData.Properties['fecha'] := EncodeDate(1990,10,1);
          MetaData.Properties['autor'] := 'Gustavo';
          MetaData.Properties['path'] := '10.102.36.2';
          Metadata.Properties['Posicion'] := 0;

          NodosCreados := rag.AddItemsFromPlainText(LocalText, MetaData, 200, 15);
        end;

        // --- FIN PROCESO BACKGROUND ---

        // 4. Actualizar UI al finalizar (Volver al Hilo Principal)
        TThread.Queue(nil,
          procedure
          begin
            if FUserCancelled then
              ShowMessage('Proceso cancelado por el usuario.')
            else
              ShowMessage('Terminó de procesar el archivo. Nodos: ' + NodosCreados.ToString);

            BtnUploadFile.Enabled := True;
            ProgressBar1.Value := ProgressBar1.Max;

            //No olvidar recrear los indices cuando se cargan a memoria
            RAG.BuildIndex;

            ShowAllRAG;
          end);

      except
        on E: Exception do
        begin
          // Manejo de errores seguro para hilos
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Error al procesar: ' + E.Message);
              BtnUploadFile.Enabled := True;
            end);
        end;
      end;
    end);
end;

procedure TForm21.BtnVGqlToTextClick(Sender: TObject);
var
  sVGQL: string;
  Parser: TVGQLParser;
  AST: TVGQLQuery;          // Nueva variable para el AST
  Compiler: TVGQLCompiler;  // Nueva variable para el Compilador
  Req: TVGQLRequest;
  SB: TStringBuilder;

  // Procedimiento local para mostrar filtros de forma recursiva
  procedure RenderCriteria(ACriteria: TAiFilterCriteria; Indent: string);
  var
    j: Integer;
    Criterion: TFilterCriterion;
    OpStr: string;
  begin
    if (ACriteria = nil) or (ACriteria.Count = 0) then Exit;

    for j := 0 to ACriteria.Count - 1 do
    begin
      Criterion := ACriteria.Items[j];

      if Criterion.IsGroup then
      begin
        SB.Append(Indent + '  [GRUPO ' + IfThen(Criterion.SubCriteria.LogicalOp = loAnd, 'AND', 'OR') + ']').AppendLine;
        RenderCriteria(Criterion.SubCriteria, Indent + '    ');
      end
      else
      begin
        case Criterion.Op of
          foEqual:          OpStr := '=';
          foNotEqual:       OpStr := '<>';
          foGreater:        OpStr := '>';
          foGreaterOrEqual: OpStr := '>=';
          foLess:           OpStr := '<';
          foLessOrEqual:    OpStr := '<=';
          foContains:       OpStr := 'CONTAINS';
          foStartsWith:     OpStr := 'STARTS_WITH';
          foEndsWith:       OpStr := 'ENDS_WITH';
          foLike:           OpStr := 'LIKE';
          foILike:          OpStr := 'ILIKE';
          foIn:             OpStr := 'IN';
          foNotIn:          OpStr := 'NOT IN';
          foBetween:        OpStr := 'BETWEEN';
          foIsNull:         OpStr := 'IS NULL';
          foIsNotNull:      OpStr := 'IS NOT NULL';
          foExists:         OpStr := 'EXISTS';
        else
          OpStr := '???';
        end;

        SB.Append(Indent + '  [+] ' + Criterion.Key + ' ' + OpStr);

        if not (Criterion.Op in [foIsNull, foIsNotNull, foExists]) then
        begin
          SB.Append(' ' + VarToStr(Criterion.Value));
          if Criterion.Op = foBetween then
            SB.Append(' AND ' + VarToStr(Criterion.Value2));
        end;
        SB.AppendLine;
      end;
    end;
  end;

begin
  sVGQL := MemoPrompt.Lines.Text.Trim;
  if sVGQL.IsEmpty then Exit;

  SB := TStringBuilder.Create;
  Parser := TVGQLParser.Create(sVGQL);
  AST := nil;
  Req := nil;

  try
    try
      // 1. Interpretamos el texto -> Obtenemos el Árbol (AST)
      AST := Parser.Parse;

      // 2. Compilamos el Árbol -> Obtenemos el Request de ejecución
      Compiler := TVGQLCompiler.Create;
      try
        Req := Compiler.Translate(AST);
      finally
        Compiler.Free;
      end;

      // 3. Construimos el reporte debug
      SB.AppendLine('=== VGQL DEBUG REPORT (Compiled) ===').AppendLine;

      // --- MATCH ---
      SB.AppendFormat('ENTITY: %s', [Req.Entity]);
      if Req.ALabel <> '' then SB.AppendFormat(' [Alias: %s]', [Req.ALabel]);
      SB.AppendLine.AppendLine;

      // --- SEARCH ---
      SB.AppendLine('QUERY: ' + Req.Query).AppendLine;

      // --- USING ---
      SB.Append('MODE: ');
      // Calificación completa para evitar error E2010
      case Req.Mode of
        smEmbeddings: SB.Append('Semántico (Embeddings)');
        smBM25:       SB.Append('Léxico (BM25)');
        smHybrid:     SB.Append('Híbrido');
      end;
      SB.AppendLine;

      SB.AppendFormat('  - Pesos: Sem[%.2f] Lex[%.2f]', [Req.WeightSemantic, Req.WeightLexical]).AppendLine;
      SB.Append('  - Fusión: ');
      if Req.Fusion = fmRRF then SB.Append('RRF') else SB.Append('Weighted');
      SB.AppendLine.AppendLine;

      // --- WHERE (Recursivo) ---
      SB.AppendLine('FILTERS (Tree):');
      if (Req.Filter <> nil) and (Req.Filter.Count > 0) then
        RenderCriteria(Req.Filter, '')
      else
        SB.AppendLine('  (No filters defined)');
      SB.AppendLine;

      // --- RERANK & OTROS ---
      if Req.RerankQuery <> '' then
        SB.AppendFormat('RERANK: "%s"', [Req.RerankQuery]).AppendLine;

      SB.AppendFormat('LIMIT: %d', [Req.Limit]).AppendLine;
      SB.AppendFormat('SCORES: Metadata[%s] Score[%s]',
        [BoolToStr(Req.IncludeMetadata, True), BoolToStr(Req.IncludeScore, True)]).AppendLine;

    except
      on E: Exception do
      begin
        SB.AppendLine.AppendLine('!!! ERROR !!!');
        SB.AppendLine(E.Message);
      end;
    end;

    MemoResponse.Lines.Text := SB.ToString;

  finally
    if Assigned(AST) then AST.Free;
    if Assigned(Req) then Req.Free;
    Parser.Free;
    SB.Free;
  end;
end;

procedure TForm21.BtnBuscarVGQLClick(Sender: TObject);
var
  LPrompt, LRes: string;
begin
  // 1. Capturamos la consulta VGQL del Memo
  LPrompt := MemoPrompt.Lines.Text.Trim;

  if LPrompt.IsEmpty then
  begin
    ShowMessage('Por favor, escribe una consulta MakerAIVGQL');
    Exit;
  end;

  // 2. Limpiamos la respuesta anterior
  MemoResponse.Lines.Clear;
  MemoResponse.Lines.Add('Buscando...');
  Application.ProcessMessages;

  try
    // 3. Ejecutamos la magia del lenguaje híbrido
    // ExecuteVGQL hará el Lexer -> Parser -> Search -> Rerank -> Optimize
    LRes := Rag.ExecuteVGQL(LPrompt);

    // 4. Mostramos el resultado (Contexto formateado para el LLM)
    if LRes.IsEmpty then
      MemoResponse.Lines.Text := 'No se encontraron resultados que coincidan con los criterios.'
    else
      MemoResponse.Lines.Text := LRes;

  except
    on E: Exception do
    begin
      // En caso de error de sintaxis en el VGQL o error de conexión
      MemoResponse.Lines.Text := 'ERROR EN CONSULTA VGQL: ' + sLineBreak + E.Message;
      // Opcional: poner el foco en el memo para corregir
      MemoPrompt.SetFocus;
    end;
  end;
end;

procedure TForm21.ChEnableRRFChange(Sender: TObject);
begin
  RAG.SearchOptions.UseRRF := ChEnableRRF.IsChecked;
end;

procedure TForm21.ChLexicalSearchChange(Sender: TObject);
begin
  RAG.SearchOptions.UseBM25 := ChLexicalSearch.IsChecked;

  If ChLexicalSearch.IsChecked  and (RAG.Items.Count > 0) then
     RAG.BuildLexicalIndex;
end;

procedure TForm21.ChSemanticSearchChange(Sender: TObject);
begin
   RAG.SearchOptions.UseEmbeddings := ChSemanticSearch.IsChecked;
end;

procedure TForm21.RAGImportProgress(Sender: TObject; aPosition, Total: Integer; var Cancel: Boolean);
begin
  // 1. Verificar si el usuario pidió cancelar (desde el hilo principal)
  if FUserCancelled then
  begin
    Cancel := True;
    Exit;
  end;

  // 3. ENCOLAR la actualización visual al Hilo Principal
  TThread.Queue(nil,
    procedure
    begin
      // Este código se ejecuta en el Hilo Principal de forma segura
      if not Assigned(ProgressBar1) then
        Exit; // Protección al cerrar form

      ProgressBar1.Max := Total;
      ProgressBar1.Value := aPosition;

      // Opcional: Mostrar porcentaje en un Label
      // LabelPorcentaje.Caption := Format('%.1f %%', [(Position / Total) * 100]);
    end);
end;

procedure TForm21.ShowAllRAG;
var
  i: Integer;
  Node: TAiEmbeddingNode;
  SB: TStringBuilder;
begin
  // Verificamos si hay datos
  if rag.Count = 0 then
  begin
    ShowMessage('La base de conocimientos está vacía.');
    Exit;
  end;

  MemoResponse.Lines.BeginUpdate; // Congela el repintado para mayor velocidad
  try
    MemoResponse.Lines.Clear;
    SB := TStringBuilder.Create;
    try
      for i := 0 to rag.Count - 1 do
      begin
        Node := rag.Items[i]; // Acceso directo al nodo

        SB.Append('--------------------------------------------------');
        SB.AppendLine;
        SB.AppendFormat('NODE #%d | ID: %s', [i, Node.Tag]);
        SB.AppendLine;

        // Mostrar Metadatos si existen
        if (Node.MetaData <> nil) and (Node.MetaData.InternalDictionary.Count > 0) then
        begin
          SB.Append('METADATA: ');
          SB.Append(Node.MetaData.ToJSON.ToString); // Serialización rápida
          SB.AppendLine;
        end;

        SB.Append('TEXT: ');
        SB.Append(Node.Text); // El contenido real
        SB.AppendLine;
        SB.AppendLine;
      end;

      MemoResponse.Text := SB.ToString;
    finally
      SB.Free;
    end;
  finally
    MemoResponse.Lines.EndUpdate;
  end;
end;

end.
