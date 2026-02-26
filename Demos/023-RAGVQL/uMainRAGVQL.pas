unit uMainRAGVQL;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Threading, System.Generics.Collections, System.StrUtils, System.Math,

  uMakerAi.Embeddings.core, uMakerAi.Embeddings, uMakerAi.Chat.Ollama,
  uMakerAi.rag.Vector.Driver.Postgres,
  uMakerAi.rag.Vectors, uMakerAi.rag.Vectors.Index,
  uMakerAi.RAG.Vectors.VQL, uMakerAi.RAG.MetaData,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, FMX.Layouts,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.PG, FireDAC.Phys.PGDef, FireDAC.FMXUI.Wait,
  Data.DB, FireDAC.Comp.Client;

type
  TForm21 = class(TForm)
    // ── Non-visual components ──────────────────────────────────────────────
    AiOllamaEmbeddings1: TAiOllamaEmbeddings;
    RAG: TAiRAGVector;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    DbConn: TFDConnection;
    PgDriver: TAiRAGVectorPostgresDriver;
    // ── Left panel ────────────────────────────────────────────────────────
    BtnUploadFile: TButton;
    BtnSaveToFile: TButton;
    BtnLoadFromfile: TButton;
    BtnCreateTable: TButton;
    BtnCancelar: TButton;
    BtnMostrarTodos: TButton;
    ProgressBar1: TProgressBar;
    ChSemanticSearch: TCheckBox;
    ChLexicalSearch: TCheckBox;
    ChEnableRRF: TCheckBox;
    ChReorderABC: TCheckBox;
    ChShowProperties: TCheckBox;
    EditPrecision: TEdit;
    EditLimit: TEdit;
    Label10: TLabel;
    Label2: TLabel;
    LblStatus: TLabel;
    // ── Right panel ───────────────────────────────────────────────────────
    Label3: TLabel;
    MemoResponse: TMemo;
    Splitter1: TSplitter;
    Label1: TLabel;
    MemoPrompt: TMemo;
    BtnConsultar: TButton;
    BtnBuscarVGQL: TButton;
    BtnConsultaToText: TButton;
    BtnVGqlToText: TButton;
    // ── Events ────────────────────────────────────────────────────────────
    procedure FormCreate(Sender: TObject);
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
    procedure BtnCancelarClick(Sender: TObject);
    procedure BtnMostrarTodosClick(Sender: TObject);
  private
    FUserCancelled: Boolean;
    procedure SetStatus(const AMsg: string);
  public
    procedure ShowAllRAG;
  end;

var
  Form21: TForm21;

implementation

{$R *.fmx}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

procedure TForm21.SetStatus(const AMsg: string);
begin
  LblStatus.Text := AMsg;
end;

// ---------------------------------------------------------------------------
//  Inicializacion
// ---------------------------------------------------------------------------

procedure TForm21.FormCreate(Sender: TObject);
begin
  // Ejemplo de consulta VQL para orientar al usuario
  MemoPrompt.Lines.Text :=
    'MATCH documentos'                                     + sLineBreak +
    'SEARCH ''inteligencia artificial'''                   + sLineBreak +
    'USING HYBRID'                                         + sLineBreak +
    '  WEIGHTS(semantic: 0.7, lexical: 0.3)'               + sLineBreak +
    '  FUSION RRF'                                         + sLineBreak +
    'WHERE autor = ''Gustavo'''                            + sLineBreak +
    'THRESHOLD GLOBAL 0.5'                                 + sLineBreak +
    'OPTIMIZE REORDER ABC'                                 + sLineBreak +
    'RETURN TEXT, METADATA, SCORE'                         + sLineBreak +
    'LIMIT 5';

  SetStatus('Listo. Cargue un archivo de texto para comenzar.');
end;

// ---------------------------------------------------------------------------
//  Base de Conocimientos
// ---------------------------------------------------------------------------

procedure TForm21.BtnUploadFileClick(Sender: TObject);
Var
  FileName: String;
begin
  If not OpenDialog1.Execute then Exit;

  FileName := OpenDialog1.FileName;

  FUserCancelled    := False;
  ProgressBar1.Value := 0;
  BtnUploadFile.Enabled := False;
  BtnCancelar.Enabled   := True;
  rag.Clear;

  SetStatus('Importando: ' + ExtractFileName(FileName));

  TTask.Run(
    procedure
    Var
      St: TStringStream;
      LocalText: String;
      NodosCreados: Integer;
      MetaData: TAiEmbeddingMetaData;
    begin
      try
        St := TStringStream.Create('', TEncoding.UTF8);
        try
          St.LoadFromFile(FileName);
          LocalText := St.DataString;
        finally
          St.Free;
        end;

        if not FUserCancelled then
        begin
          MetaData := TAiEmbeddingMetaData.Create;
          MetaData.Properties['tipo']     := 'cuento';
          MetaData.Properties['fecha']    := EncodeDate(1990, 10, 1);
          MetaData.Properties['autor']    := 'Gustavo';
          MetaData.Properties['path']     := '10.102.36.2';
          MetaData.Properties['Posicion'] := 0;

          NodosCreados := rag.AddItemsFromPlainText(LocalText, MetaData, 200, 15);
        end;

        TThread.Queue(nil,
          procedure
          begin
            BtnCancelar.Enabled   := False;
            BtnUploadFile.Enabled := True;
            ProgressBar1.Value    := ProgressBar1.Max;
            RAG.BuildIndex;
            ShowAllRAG;

            if FUserCancelled then
              SetStatus('Importacion cancelada por el usuario.')
            else
              SetStatus('Importacion completada. Nodos: ' + NodosCreados.ToString);
          end);

      except
        on E: Exception do
        begin
          TThread.Queue(nil,
            procedure
            begin
              BtnCancelar.Enabled   := False;
              BtnUploadFile.Enabled := True;
              SetStatus('Error al importar: ' + E.Message);
              ShowMessage('Error al procesar: ' + E.Message);
            end);
        end;
      end;
    end);
end;

procedure TForm21.BtnCancelarClick(Sender: TObject);
begin
  FUserCancelled      := True;
  BtnCancelar.Enabled := False;
  SetStatus('Cancelando...');
end;

procedure TForm21.RAGImportProgress(Sender: TObject; aPosition, Total: Integer; var Cancel: Boolean);
begin
  if FUserCancelled then
  begin
    Cancel := True;
    Exit;
  end;

  TThread.Queue(nil,
    procedure
    begin
      if not Assigned(ProgressBar1) then Exit;
      ProgressBar1.Max   := Total;
      ProgressBar1.Value := aPosition;
    end);
end;

procedure TForm21.BtnSaveToFileClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt := 'mkVRag';
  SaveDialog1.Filter     := 'MakerAI Vector Files (*.mkVRag)|*.mkVRag|Todos los archivos (*.*)|*.*';
  SaveDialog1.Title      := 'Guardar Base de Vectores';

  if SaveDialog1.Execute then
  begin
    Cursor := crHourGlass;
    try
      rag.SaveToFile(SaveDialog1.FileName);
      OpenDialog1.FileName := SaveDialog1.FileName;
      SetStatus('Base guardada: ' + ExtractFileName(SaveDialog1.FileName));
      ShowMessage('Base de conocimientos guardada exitosamente.');
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TForm21.BtnLoadFromfileClick(Sender: TObject);
var
  ItemsCargados: Integer;
begin
  OpenDialog1.DefaultExt := 'mkVRag';
  OpenDialog1.Filter     := 'MakerAI Vector Files (*.mkVRag)|*.mkVRag|Todos los archivos (*.*)|*.*';

  if OpenDialog1.Execute then
  begin
    Cursor := crHourGlass;
    try
      ItemsCargados := rag.LoadFromFile(OpenDialog1.FileName);
      Rag.BuildIndex;
      SaveDialog1.FileName := OpenDialog1.FileName;
      ShowAllRAG;
      SetStatus('Base cargada: ' + IntToStr(ItemsCargados) + ' nodos  [' + ExtractFileName(OpenDialog1.FileName) + ']');
      ShowMessage('Archivo cargado correctamente.' + sLineBreak + 'Nodos recuperados: ' + IntToStr(ItemsCargados));
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TForm21.BtnCreateTableClick(Sender: TObject);
begin
  PgDriver.CreateSchema(PgDriver.TableName, 1024);
  SetStatus('Tabla PostgreSQL creada: ' + PgDriver.TableName);
end;

procedure TForm21.BtnMostrarTodosClick(Sender: TObject);
begin
  ShowAllRAG;
end;

// ---------------------------------------------------------------------------
//  Busqueda simple
// ---------------------------------------------------------------------------

procedure TForm21.BtnConsultarClick(Sender: TObject);
Var
  LPrompt: String;
  LShowProperties: Boolean;
  LLimit: Integer;
  LPrecision: Double;
  LUseBM25, LUseRRF, LUseABC: Boolean;
begin
  LPrompt := MemoPrompt.Lines.Text.Trim;
  if LPrompt.IsEmpty then
  begin
    ShowMessage('Por favor ingrese una consulta.');
    Exit;
  end;

  LShowProperties := ChShowProperties.IsChecked;
  LLimit          := StrToIntDef(EditLimit.Text, 5);
  LPrecision      := StrToFloatDef(EditPrecision.Text, 0.7);
  LUseBM25        := ChLexicalSearch.IsChecked;
  LUseRRF         := ChEnableRRF.IsChecked;
  LUseABC         := ChReorderABC.IsChecked;

  BtnConsultar.Enabled := False;
  MemoResponse.Lines.Text := 'Buscando...';
  SetStatus('Buscando (modo simple)...');

  TTask.Run(
    procedure
    Var
      LResponse: String;
    begin
      try
        rag.SearchOptions.UseEmbeddings := True;
        rag.SearchOptions.UseBM25       := LUseBM25;
        rag.SearchOptions.UseRRF        := LUseRRF;
        rag.SearchOptions.UseReorderABC := LUseABC;

        LResponse := rag.SearchText(
          LPrompt,
          LLimit,
          LPrecision,
          nil,
          LShowProperties
        );

        TThread.Queue(nil,
          procedure
          begin
            if not Assigned(MemoResponse) then Exit;

            MemoResponse.Lines.Text := LResponse;

            if LResponse.IsEmpty then
              MemoResponse.Lines.Add('(No se encontraron coincidencias con esa precision)');

            BtnConsultar.Enabled := True;
            Cursor := crDefault;
            SetStatus('Busqueda completada.');
          end);

      except
        on E: Exception do
        begin
          TThread.Queue(nil,
            procedure
            begin
              MemoResponse.Lines.Text := 'Error en la busqueda: ' + E.Message;
              BtnConsultar.Enabled := True;
              Cursor := crDefault;
              SetStatus('Error: ' + E.Message);
            end);
        end;
      end;
    end);
end;

// ---------------------------------------------------------------------------
//  Busqueda VQL
// ---------------------------------------------------------------------------

procedure TForm21.BtnBuscarVGQLClick(Sender: TObject);
var
  LPrompt, LRes: string;
begin
  LPrompt := MemoPrompt.Lines.Text.Trim;
  if LPrompt.IsEmpty then
  begin
    ShowMessage('Por favor, escribe una consulta VQL.');
    Exit;
  end;

  MemoResponse.Lines.Clear;
  MemoResponse.Lines.Add('Ejecutando consulta VQL...');
  SetStatus('Ejecutando VQL...');
  Application.ProcessMessages;

  try
    LRes := Rag.ExecuteVGQL(LPrompt);

    if LRes.IsEmpty then
      MemoResponse.Lines.Text := 'No se encontraron resultados que coincidan con los criterios.'
    else
      MemoResponse.Lines.Text := LRes;

    SetStatus('VQL ejecutado correctamente.');

  except
    on E: Exception do
    begin
      MemoResponse.Lines.Text := 'ERROR EN CONSULTA VQL:' + sLineBreak + E.Message;
      SetStatus('Error VQL: ' + E.Message);
      MemoPrompt.SetFocus;
    end;
  end;
end;

// ---------------------------------------------------------------------------
//  Debug / Config reports
// ---------------------------------------------------------------------------

procedure TForm21.BtnVGqlToTextClick(Sender: TObject);
var
  sVGQL: string;
  Parser: TVGQLParser;
  AST: TVGQLQuery;
  Compiler: TVGQLCompiler;
  Req: TVGQLRequest;
  SB: TStringBuilder;

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

  SB       := TStringBuilder.Create;
  Parser   := TVGQLParser.Create(sVGQL);
  AST      := nil;
  Req      := nil;

  try
    try
      AST := Parser.Parse;

      Compiler := TVGQLCompiler.Create;
      try
        Req := Compiler.Translate(AST);
      finally
        Compiler.Free;
      end;

      SB.AppendLine('=== VQL DEBUG REPORT (Compilado) ===').AppendLine;

      SB.AppendFormat('ENTITY: %s', [Req.Entity]);
      if Req.ALabel <> '' then SB.AppendFormat(' [Alias: %s]', [Req.ALabel]);
      SB.AppendLine.AppendLine;

      SB.AppendLine('QUERY: ' + Req.Query).AppendLine;

      SB.Append('MODE: ');
      case Req.Mode of
        smEmbeddings: SB.Append('Semantico (Embeddings)');
        smBM25:       SB.Append('Lexico (BM25)');
        smHybrid:     SB.Append('Hibrido');
      end;
      SB.AppendLine;

      SB.AppendFormat('  - Pesos: Sem[%.2f] Lex[%.2f]', [Req.WeightSemantic, Req.WeightLexical]).AppendLine;
      SB.Append('  - Fusion: ');
      if Req.Fusion = fmRRF then SB.Append('RRF') else SB.Append('Weighted');
      SB.AppendLine.AppendLine;

      SB.AppendLine('FILTROS (arbol):');
      if (Req.Filter <> nil) and (Req.Filter.Count > 0) then
        RenderCriteria(Req.Filter, '')
      else
        SB.AppendLine('  (Sin filtros)');
      SB.AppendLine;

      if Req.RerankQuery <> '' then
        SB.AppendFormat('RERANK: "%s"', [Req.RerankQuery]).AppendLine;

      SB.AppendFormat('LIMIT: %d', [Req.Limit]).AppendLine;
      SB.AppendFormat('SCORES: Metadata[%s] Score[%s]',
        [BoolToStr(Req.IncludeMetadata, True), BoolToStr(Req.IncludeScore, True)]).AppendLine;

    except
      on E: Exception do
      begin
        SB.AppendLine.AppendLine('!!! ERROR DE SINTAXIS !!!');
        SB.AppendLine(E.Message);
      end;
    end;

    MemoResponse.Lines.Text := SB.ToString;
    SetStatus('Reporte VQL generado.');

  finally
    if Assigned(AST) then AST.Free;
    if Assigned(Req) then Req.Free;
    Parser.Free;
    SB.Free;
  end;
end;

procedure TForm21.BtnConsultaToTextClick(Sender: TObject);
var
  SB: TStringBuilder;
  LPrompt: string;
  LUseEmbeddings, LUseBM25, LUseRRF: Boolean;
  LWeightSem, LWeightLex: Double;
begin
  LPrompt        := MemoPrompt.Lines.Text.Trim;
  LUseEmbeddings := True;
  LUseBM25       := ChLexicalSearch.IsChecked;
  LUseRRF        := ChEnableRRF.IsChecked;
  LWeightSem     := rag.SearchOptions.EmbeddingWeight;
  LWeightLex     := rag.SearchOptions.BM25Weight;

  SB := TStringBuilder.Create;
  try
    SB.AppendLine('=== CONFIG DEBUG REPORT (Modo Simple) ===').AppendLine;

    SB.AppendLine('ENTITY (Target): [Global / Default]').AppendLine;

    if LPrompt.IsEmpty then
      SB.AppendLine('QUERY: (Vacio)')
    else
      SB.AppendLine('QUERY: ' + LPrompt);
    SB.AppendLine;

    SB.Append('MODE: ');
    if LUseEmbeddings and LUseBM25 then
      SB.Append('Hibrido (Semantico + Lexico)')
    else if LUseEmbeddings then
      SB.Append('Embeddings Only (Semantico)')
    else if LUseBM25 then
      SB.Append('BM25 Only (Lexico)')
    else
      SB.Append('NINGUNO (configuracion invalida)');
    SB.AppendLine;

    if LUseEmbeddings and LUseBM25 then
    begin
      SB.AppendFormat('  - Weight Semantic: %.2f', [LWeightSem]).AppendLine;
      SB.AppendFormat('  - Weight Lexical:  %.2f', [LWeightLex]).AppendLine;
      SB.Append('  - Fusion: ');
      if LUseRRF then
        SB.Append('RRF (Reciprocal Rank Fusion)')
      else
        SB.Append('Weighted Sum');
      SB.AppendLine;
    end;
    SB.AppendLine;

    SB.AppendLine('FILTROS (MetaData):');
    SB.AppendLine('  (La busqueda simple no soporta filtros de metadatos)').AppendLine;

    SB.AppendLine('THRESHOLDS:');
    SB.AppendFormat('  - Min Global: %s', [EditPrecision.Text]).AppendLine.AppendLine;

    SB.AppendLine('RERANKING: Inactivo');
    SB.AppendLine('  (Usar VQL para activar RERANK)').AppendLine;

    SB.AppendLine('OPTIMIZACIONES:');
    SB.AppendFormat('  - Reordenar Contexto (Lost-in-Middle): %s',
      [BoolToStr(ChReorderABC.IsChecked, True)]).AppendLine.AppendLine;

    SB.AppendLine('OUTPUT:');
    SB.AppendFormat('  - Limit: %s', [EditLimit.Text]).AppendLine;
    SB.AppendFormat('  - Include Metadata: %s',
      [BoolToStr(ChShowProperties.IsChecked, True)]).AppendLine;

    MemoResponse.Lines.Text := SB.ToString;
    SetStatus('Reporte de configuracion generado.');

  finally
    SB.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  Cambios en checkboxes de modo de busqueda
// ---------------------------------------------------------------------------

procedure TForm21.ChEnableRRFChange(Sender: TObject);
begin
  RAG.SearchOptions.UseRRF := ChEnableRRF.IsChecked;
end;

procedure TForm21.ChLexicalSearchChange(Sender: TObject);
begin
  RAG.SearchOptions.UseBM25 := ChLexicalSearch.IsChecked;

  if ChLexicalSearch.IsChecked and (RAG.Items.Count > 0) then
    RAG.BuildLexicalIndex;
end;

procedure TForm21.ChSemanticSearchChange(Sender: TObject);
begin
  RAG.SearchOptions.UseEmbeddings := ChSemanticSearch.IsChecked;
end;

// ---------------------------------------------------------------------------
//  Visualizacion de todos los nodos
// ---------------------------------------------------------------------------

procedure TForm21.ShowAllRAG;
var
  i: Integer;
  Node: TAiEmbeddingNode;
  SB: TStringBuilder;
begin
  if rag.Count = 0 then
  begin
    ShowMessage('La base de conocimientos esta vacia.');
    Exit;
  end;

  MemoResponse.Lines.BeginUpdate;
  try
    MemoResponse.Lines.Clear;
    SB := TStringBuilder.Create;
    try
      for i := 0 to rag.Count - 1 do
      begin
        Node := rag.Items[i];

        SB.Append('--------------------------------------------------');
        SB.AppendLine;
        SB.AppendFormat('NODE #%d | ID: %s', [i, Node.Tag]);
        SB.AppendLine;

        if (Node.MetaData <> nil) and (Node.MetaData.InternalDictionary.Count > 0) then
        begin
          SB.Append('METADATA: ');
          SB.Append(Node.MetaData.ToJSON.ToString);
          SB.AppendLine;
        end;

        SB.Append('TEXT: ');
        SB.Append(Node.Text);
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

  SetStatus('Mostrando ' + rag.Count.ToString + ' nodos en la base.');
end;

end.
