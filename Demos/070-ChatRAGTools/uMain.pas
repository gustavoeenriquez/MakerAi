// =============================================================================
//  Demo 070 - ChatRAGTools
//  Chat multi-proveedor (MakerAI) + RAG local con SQLite + TAiFunctions
//  (TDateTime y GraficarSerie) + TabControl con WebBrowser(Edge), TeeChart y Memo.
//
//  Construido "code-first": todos los componentes se crean y cablean en
//  FormCreate para que el demo sea autocontenido y didactico.
//  Basado en el patron de 012-ChatAllFunctions (ChatFull).
// =============================================================================
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.JSON, System.Threading, System.IOUtils, System.DateUtils, System.Net.HttpClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types,
  FMX.Edit, FMX.ListBox, FMX.TabControl, FMX.WebBrowser,
  // TeeChart (FMX)
  FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMXTee.Series,
  // FireDAC (SQLite embebido, sin DLL externa via SQLiteWrapper.Stat)
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.DApt, Data.DB, FireDAC.Comp.Client,
  // MakerAI
  uMakerAi.Core, uMakerAi.Chat, uMakerAi.Chat.Messages, uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations, uMakerAi.Tools.Functions,
  uMakerAi.RAG.Vectors, uMakerAi.RAG.Vector.Driver.SQLite,
  uMakerAi.Embeddings.Core, uMakerAi.Embeddings.OpenAi;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    // --- Componentes MakerAI (no visuales) ---
    AiConn: TAiChatConnection;
    AiFunctions1: TAiFunctions;
    Embeddings: TAiOpenAiEmbeddings;
    RAG: TAiRAGVector;
    RAGDriver: TAiRAGVectorSQLiteDriver;
    // --- SQLite ---
    FDConn: TFDConnection;
    FGUIWait: TFDGUIxWaitCursor;
    // --- UI: chat (izquierda) ---
    ComboDriver, ComboModel: TComboBox;
    ChkUseRAG: TCheckBox;
    MemoChat, MemoPrompt: TMemo;
    BtnSend, BtnLoadRAG: TButton;
    // --- UI: TabControl (derecha) ---
    Tab: TTabControl;
    tabWeb, tabChart, tabLog: TTabItem;
    WebBrowser1: TWebBrowser;
    Chart1: TChart;
    FSerie: TLineSeries;
    MemoLog: TMemo;
    procedure BuildUI;
    procedure BuildAi;
    procedure InitProviders;
    // eventos UI
    procedure ComboDriverChange(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure BtnLoadRAGClick(Sender: TObject);
    // eventos chat
    procedure AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception; const AResponse: IHTTPResponse);
    // funciones (tools)
    procedure FuncGetFechaHora(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FuncGraficarSerie(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    // helpers
    procedure AddLog(const S: string);
    procedure ChatAppend(const S: string);
    function ParseFecha(const S: string; aIndex: Integer): TDateTime;
    // Prueba automatica / grabacion de video, sin teclado ni mouse:
    //   --driver OpenAi --model gpt-6-sol --prueba [--tecleo] [--pausa ms] [--tamano 1600x900] [--marcas archivo] "pregunta" ...
    procedure IniciarPrueba;
    procedure PasoPrueba(Sender: TObject);
  private
    FPrueba: TStringList;
    FTimerPrueba: TTimer;
    FTecleo: Boolean;
    FPausaMs: Integer;
    FFinTurno: Cardinal;
    FEscribiendo: string;
    FTecleadas: Integer;      // letras de FEscribiendo ya escritas en el memo
    FMarcas: string;          // archivo de marcas para el montaje del video
    FTurno: Integer;
    FCargaRAG: Cardinal;      // "@rag archivo": el texto ya esta en el prompt; se indexa al vencer
    procedure Marcar(const ANombre: string);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

const
  CHART_DT_FMT = 'dd/mm';

// =============================================================================
//  Creacion del form
// =============================================================================
procedure TFormMain.FormCreate(Sender: TObject);
begin
  BuildUI;
  BuildAi;
  InitProviders;
  AddLog('Listo. Selecciona proveedor y modelo, escribe un prompt y pulsa Enviar.');
  AddLog('Tools disponibles para la IA: GetFechaHora, GraficarSerie.');
  AddLog('RAG local en SQLite: ' + FDConn.Params.Database);
  IniciarPrueba;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FPrueba.Free;
  try
    if Assigned(FDConn) then
      FDConn.Connected := False;
  except
  end;
end;

// =============================================================================
//  UI - construida por codigo
// =============================================================================
procedure TFormMain.BuildUI;
var
  LLeft, LTopBar, LBottom: TLayout;
  Spl: TSplitter;
begin
  // ---------- Panel izquierdo (chat) ----------
  LLeft := TLayout.Create(Self);
  LLeft.Parent := Self;
  LLeft.Align := TAlignLayout.Left;
  LLeft.Width := 560;

  // barra superior: selectores + RAG
  LTopBar := TLayout.Create(Self);
  LTopBar.Parent := LLeft;
  LTopBar.Align := TAlignLayout.Top;
  LTopBar.Height := 96;

  ComboDriver := TComboBox.Create(Self);
  ComboDriver.Parent := LTopBar;
  ComboDriver.Position.X := 10; ComboDriver.Position.Y := 10;
  ComboDriver.Width := 200;
  ComboDriver.OnChange := ComboDriverChange;

  ComboModel := TComboBox.Create(Self);
  ComboModel.Parent := LTopBar;
  ComboModel.Position.X := 220; ComboModel.Position.Y := 10;
  ComboModel.Width := 320;

  ChkUseRAG := TCheckBox.Create(Self);
  ChkUseRAG.Parent := LTopBar;
  ChkUseRAG.Position.X := 10; ChkUseRAG.Position.Y := 52;
  ChkUseRAG.Width := 220;
  ChkUseRAG.Text := 'Usar RAG (contexto local)';

  BtnLoadRAG := TButton.Create(Self);
  BtnLoadRAG.Parent := LTopBar;
  BtnLoadRAG.Position.X := 240; BtnLoadRAG.Position.Y := 48;
  BtnLoadRAG.Width := 300;
  BtnLoadRAG.Text := 'Cargar texto/archivo a RAG...';
  BtnLoadRAG.OnClick := BtnLoadRAGClick;

  // zona inferior: prompt + enviar
  LBottom := TLayout.Create(Self);
  LBottom.Parent := LLeft;
  LBottom.Align := TAlignLayout.Bottom;
  LBottom.Height := 130;

  BtnSend := TButton.Create(Self);
  BtnSend.Parent := LBottom;
  BtnSend.Align := TAlignLayout.Right;
  BtnSend.Width := 90;
  BtnSend.Text := 'Enviar';
  BtnSend.OnClick := BtnSendClick;

  MemoPrompt := TMemo.Create(Self);
  MemoPrompt.Parent := LBottom;
  MemoPrompt.Align := TAlignLayout.Client;
  MemoPrompt.TextSettings.WordWrap := True;
  MemoPrompt.Lines.Text := 'Hola, dime la fecha y hora actual.';

  // centro: memo del chat
  MemoChat := TMemo.Create(Self);
  MemoChat.Parent := LLeft;
  MemoChat.Align := TAlignLayout.Client;
  MemoChat.ReadOnly := True;
  MemoChat.TextSettings.WordWrap := True;

  // ---------- Splitter ----------
  Spl := TSplitter.Create(Self);
  Spl.Parent := Self;
  Spl.Align := TAlignLayout.Left;
  Spl.Width := 6;

  // ---------- TabControl derecho ----------
  Tab := TTabControl.Create(Self);
  Tab.Parent := Self;
  Tab.Align := TAlignLayout.Client;

  tabWeb := Tab.Add;
  tabWeb.Text := 'Web (Edge)';
  WebBrowser1 := TWebBrowser.Create(Self);
  WebBrowser1.Parent := tabWeb;
  WebBrowser1.Align := TAlignLayout.Client;

  tabChart := Tab.Add;
  tabChart.Text := 'Grafica (TeeChart)';
  Chart1 := TChart.Create(Self);
  Chart1.Parent := tabChart;
  Chart1.Align := TAlignLayout.Client;
  Chart1.View3D := False;
  Chart1.Title.Text.Text := 'Serie de tiempo';
  FSerie := TLineSeries.Create(Chart1);
  Chart1.AddSeries(FSerie);
  FSerie.XValues.DateTime := True;
  FSerie.Pointer.Visible := True;          // un punto por dato
  Chart1.Legend.Visible := False;          // una sola serie: el titulo basta
  Chart1.BottomAxis.DateTimeFormat := CHART_DT_FMT;
  Chart1.BottomAxis.Increment := 1;        // una marca por dia

  tabLog := Tab.Add;
  tabLog.Text := 'Log';
  MemoLog := TMemo.Create(Self);
  MemoLog.Parent := tabLog;
  MemoLog.Align := TAlignLayout.Client;
  MemoLog.ReadOnly := True;

  Tab.ActiveTab := tabLog;

  // pagina inicial del navegador
  try
    WebBrowser1.URL := 'https://makerai.cimamaker.com';
    WebBrowser1.Navigate;
  except
  end;
end;

// =============================================================================
//  Componentes MakerAI + RAG SQLite
// =============================================================================
procedure TFormMain.BuildAi;
var
  LFn: TFunctionActionItem;
  LParam: TFunctionParamsItem;
begin
  // ---- SQLite (FireDAC, embebido) ----
  FGUIWait := TFDGUIxWaitCursor.Create(Self);
  FDConn := TFDConnection.Create(Self);
  FDConn.LoginPrompt := False;
  FDConn.Params.DriverID := 'SQLite';
  FDConn.Params.Database := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'rag_demo.db');
  FDConn.Params.Add('LockingMode=Normal');
  FDConn.Connected := True;

  // ---- Embeddings (OpenAI) ----
  Embeddings := TAiOpenAiEmbeddings.Create(Self);
  Embeddings.ApiKey := '@OPENAI_API_KEY';

  // ---- Driver SQLite del RAG (busqueda vectorial en Delphi puro, sin vec0) ----
  RAGDriver := TAiRAGVectorSQLiteDriver.Create(Self);
  RAGDriver.Connection := FDConn;
  RAGDriver.TableName := 'vec_docs';
  try
    RAGDriver.CreateSchema('vec_docs');
  except
    on E: Exception do
      AddLog('Aviso al crear esquema RAG: ' + E.Message);
  end;

  // ---- Store vectorial ----
  RAG := TAiRAGVector.Create(Self, True);
  RAG.Embeddings := Embeddings;
  RAG.Driver := RAGDriver;

  // ---- Conexion de chat ----
  AiConn := TAiChatConnection.Create(Self);
  AiConn.OnReceiveData := AiConnReceiveData;
  AiConn.OnReceiveDataEnd := AiConnReceiveDataEnd;
  AiConn.OnError := AiConnError;

  // ---- TAiFunctions con dos funciones ----
  AiFunctions1 := TAiFunctions.Create(Self);

  // 1) GetFechaHora
  LFn := AiFunctions1.Functions.Add;
  LFn.FunctionName := 'GetFechaHora';
  LFn.Enabled := True;
  LFn.Description.Text := 'Devuelve la fecha y hora actual del sistema en formato ISO 8601.';
  LFn.OnAction := FuncGetFechaHora;
  LParam := LFn.Parameters.Add;
  LParam.Name := 'zona';
  LParam.ParamType := ptString;
  LParam.Description.Text := 'Zona horaria opcional (solo informativa).';
  LParam.Required := False;

  // 2) GraficarSerie (schema con array de puntos via RawSchemaJson)
  LFn := AiFunctions1.Functions.Add;
  LFn.FunctionName := 'GraficarSerie';
  LFn.Enabled := True;
  LFn.Description.Text := 'Grafica una serie de tiempo en el panel de graficos. ' +
    'Recibe un titulo y un arreglo de puntos {fecha, valor}.';
  LFn.RawSchemaJson :=
    '{"type":"object","properties":{' +
    '"titulo":{"type":"string","description":"Titulo de la grafica"},' +
    '"puntos":{"type":"array","description":"Puntos de la serie de tiempo",' +
    '"items":{"type":"object","properties":{' +
    '"fecha":{"type":"string","description":"Fecha u hora en ISO 8601"},' +
    '"valor":{"type":"number","description":"Valor numerico del punto"}},' +
    '"required":["fecha","valor"]}}},"required":["puntos"]}';
  LFn.OnAction := FuncGraficarSerie;

  // conectar funciones al chat
  AiConn.AiFunctions := AiFunctions1;
end;

// =============================================================================
//  Proveedores y modelos
// =============================================================================
procedure TFormMain.InitProviders;
var
  List: TStringList;
begin
  try
    List := AiConn.GetDriversNames;
    try
      List.Sort;
      ComboDriver.Items.Assign(List);
      if ComboDriver.Items.Count > 0 then
      begin
        ComboDriver.ItemIndex := 0;
        ComboDriverChange(ComboDriver);
      end;
    finally
      List.Free;
    end;
  except
    on E: Exception do
      AddLog('No se pudieron cargar los proveedores: ' + E.Message);
  end;
end;

procedure TFormMain.ComboDriverChange(Sender: TObject);
var
  List: TStringList;
begin
  if not Assigned(ComboDriver.Selected) then
    Exit;

  AiConn.DriverName := Trim(ComboDriver.Selected.Text);
  // herramientas por el canal tipado (no por Params) y streaming
  AiConn.Tool_Active := True;
  AiConn.Params.Values['Asynchronous'] := 'True';
  // el chat es un TMemo: sin Markdown, los ** se verian tal cual
  AiConn.SystemPrompt.Text := 'Responde en español, breve y en texto plano, sin Markdown.';

  try
    List := AiConn.GetModels;
    try
      List.Sort;
      ComboModel.Items.Assign(List);
      if ComboModel.Items.Count > 0 then
        ComboModel.ItemIndex := 0;
    finally
      List.Free;
    end;
  except
    on E: Exception do
      AddLog('No se pudieron cargar los modelos de ' + AiConn.DriverName + ': ' + E.Message);
  end;
end;

// =============================================================================
//  Envio del prompt (con RAG opcional)
// =============================================================================
procedure TFormMain.BtnSendClick(Sender: TObject);
var
  LPrompt, LContext, LFinal: string;
begin
  LPrompt := Trim(MemoPrompt.Lines.Text);
  if LPrompt = '' then
    Exit;

  if Assigned(ComboModel.Selected) then
    AiConn.Model := Trim(ComboModel.Selected.Text);

  // RAG: recuperar contexto local y anteponerlo al prompt
  LFinal := LPrompt;
  if ChkUseRAG.IsChecked then
  begin
    try
      LContext := RAG.SearchText(LPrompt, 5, 0.3);
      if Trim(LContext) <> '' then
      begin
        LFinal := 'Usa el siguiente CONTEXTO (recuperado de la base local) para responder.' + sLineBreak +
          '--- CONTEXTO ---' + sLineBreak + LContext + sLineBreak +
          '--- PREGUNTA ---' + sLineBreak + LPrompt;
        AddLog('RAG: contexto recuperado (' + Length(LContext).ToString + ' chars).');
      end
      else
        AddLog('RAG: sin coincidencias relevantes.');
    except
      on E: Exception do
        AddLog('RAG error: ' + E.Message);
    end;
  end;

  ChatAppend('Tu: ' + LPrompt + sLineBreak + 'IA: ');
  MemoPrompt.Lines.Clear;
  BtnSend.Enabled := False;

  TTask.Run(
    procedure
    begin
      try
        AiConn.AddMessageAndRun(LFinal, 'user', []);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              AddLog('Error al enviar: ' + E.Message);
              BtnSend.Enabled := True;
            end);
      end;
    end);
end;

// =============================================================================
//  Cargar documentos a RAG (SQLite)
// =============================================================================
procedure TFormMain.BtnLoadRAGClick(Sender: TObject);
var
  Dlg: TOpenDialog;
  LText: string;
  N: Integer;
begin
  // si el prompt tiene texto, indexarlo; si no, abrir un archivo .txt
  LText := Trim(MemoPrompt.Lines.Text);
  if LText = '' then
  begin
    Dlg := TOpenDialog.Create(Self);
    try
      Dlg.Filter := 'Texto (*.txt;*.md)|*.txt;*.md|Todos (*.*)|*.*';
      if not Dlg.Execute then
        Exit;
      LText := TFile.ReadAllText(Dlg.FileName, TEncoding.UTF8);
    finally
      Dlg.Free;
    end;
  end;

  if Trim(LText) = '' then
    Exit;

  try
    N := RAG.AddItemsFromPlainText(LText, nil, 512, 20);
    AddLog(Format('RAG: indexados %d fragmentos en SQLite.', [N]));
    MemoPrompt.Lines.Clear;
  except
    on E: Exception do
      AddLog('RAG: error al indexar: ' + E.Message + ' (revisa OPENAI_API_KEY para embeddings)');
  end;
end;

// =============================================================================
//  Eventos del chat
// =============================================================================
procedure TFormMain.AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  if aText = '' then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      MemoChat.Lines.Text := MemoChat.Lines.Text + aText;
      MemoChat.GoToTextEnd;
    end);
end;

procedure TFormMain.AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      // en sincrono aText trae la respuesta completa; en async ya se fue
      // pintando en OnReceiveData, asi que solo cerramos el turno.
      if (MemoChat.Lines.Text <> '') and (aText <> '') and
         (Pos(aText, MemoChat.Lines.Text) = 0) then
        MemoChat.Lines.Text := MemoChat.Lines.Text + aText;

      ChatAppend(sLineBreak + sLineBreak);
      // Prompt_tokens son solo los tokens nuevos; los del cache van aparte
      if Assigned(aMsg) then
        AddLog(Format('Tokens -> prompt:%d (cache:%d)  completion:%d  total:%d  (%s)',
          [aMsg.Prompt_tokens + aMsg.Cached_tokens + aMsg.Cache_write_tokens, aMsg.Cached_tokens,
           aMsg.Completion_tokens, aMsg.Total_tokens, aMsg.Model]));
      BtnSend.Enabled := True;
    end);
end;

procedure TFormMain.AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception; const AResponse: IHTTPResponse);
begin
  TThread.Queue(nil,
    procedure
    begin
      AddLog('ERROR: ' + ErrorMsg);
      BtnSend.Enabled := True;
    end);
end;

// =============================================================================
//  Funciones (tools) llamadas por la IA
// =============================================================================
procedure TFormMain.FuncGetFechaHora(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  LAhora: string;
begin
  LAhora := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
  ToolCall.Response := LAhora;
  // El log se escribe despues, en el hilo principal: con el valor copiado, no con
  // ToolCall (para entonces el chat ya pudo liberarlo y el log salia vacio)
  TThread.Queue(nil,
    procedure
    begin
      AddLog('Tool GetFechaHora -> ' + LAhora);
    end);
  Handled := True;
end;

procedure TFormMain.FuncGraficarSerie(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  jObj: TJSONObject;
  jPuntos: TJSONArray;
  LTitulo: string;
  LCount: Integer;
begin
  Handled := True;
  jObj := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if not Assigned(jObj) then
  begin
    ToolCall.Response := 'Error: argumentos JSON invalidos.';
    Exit;
  end;
  try
    LTitulo := '';
    jObj.TryGetValue<string>('titulo', LTitulo);
    if not jObj.TryGetValue<TJSONArray>('puntos', jPuntos) or (jPuntos.Count = 0) then
    begin
      ToolCall.Response := 'Error: no se recibieron puntos para graficar.';
      Exit;
    end;

    LCount := jPuntos.Count;
    // Graficar en el hilo principal (toca la UI)
    TThread.Synchronize(nil,
      procedure
      var
        k: Integer;
        po: TJSONObject;
        f: string;
        v: Double;
      begin
        FSerie.Clear;
        if LTitulo <> '' then
        begin
          FSerie.Title := LTitulo;
          Chart1.Title.Text.Text := LTitulo;
        end;
        for k := 0 to jPuntos.Count - 1 do
        begin
          if not(jPuntos.Items[k] is TJSONObject) then
            Continue;
          po := jPuntos.Items[k] as TJSONObject;
          f := '';
          po.TryGetValue<string>('fecha', f);
          v := 0;
          po.TryGetValue<Double>('valor', v);
          FSerie.AddXY(ParseFecha(f, k), v);
        end;
        Chart1.BottomAxis.DateTimeFormat := CHART_DT_FMT;
        Tab.ActiveTab := tabChart;
        AddLog(Format('Tool GraficarSerie -> %d puntos. "%s"', [LCount, LTitulo]));
      end);

    ToolCall.Response := Format('OK, grafica generada con %d puntos.', [LCount]);
  finally
    jObj.Free;
  end;
end;

// =============================================================================
//  Prueba automatica (la usa la grabacion de los videos de la serie)
// =============================================================================
procedure TFormMain.IniciarPrueba;
var
  I, K: Integer;
  Arg, Valor: string;
begin
  I := 1;
  while I <= ParamCount do
  begin
    Arg := ParamStr(I);
    if I < ParamCount then
      Valor := ParamStr(I + 1)
    else
      Valor := '';
    if SameText(Arg, '--driver') then
    begin
      K := ComboDriver.Items.IndexOf(Valor);
      if K >= 0 then
        ComboDriver.ItemIndex := K;   // OnChange carga los modelos del proveedor
      Inc(I);
    end
    else if SameText(Arg, '--model') then
    begin
      K := ComboModel.Items.IndexOf(Valor);
      if K < 0 then
        K := ComboModel.Items.Add(Valor);
      ComboModel.ItemIndex := K;
      Inc(I);
    end
    else if SameText(Arg, '--pausa') then
    begin
      FPausaMs := StrToIntDef(Valor, 0);
      Inc(I);
    end
    else if SameText(Arg, '--tamano') then
    begin
      ClientWidth := StrToIntDef(Valor.Split(['x'])[0], ClientWidth);
      ClientHeight := StrToIntDef(Valor.Split(['x'])[1], ClientHeight);
      Inc(I);
    end
    else if SameText(Arg, '--marcas') then
    begin
      FMarcas := Valor;
      Inc(I);
    end
    else if SameText(Arg, '--tecleo') then
      FTecleo := True
    else if not SameText(Arg, '--prueba') then
    begin
      if not Assigned(FPrueba) then
        FPrueba := TStringList.Create;
      FPrueba.Add(Arg);
    end;
    Inc(I);
  end;
  if not Assigned(FPrueba) then
    Exit;
  MemoPrompt.Lines.Clear;
  FFinTurno := TThread.GetTickCount + 1500;
  FTimerPrueba := TTimer.Create(Self);
  FTimerPrueba.Interval := 50;
  FTimerPrueba.OnTimer := PasoPrueba;
end;

procedure TFormMain.PasoPrueba(Sender: TObject);
var
  Pregunta: string;
begin
  // Envia cada pregunta como si la escribiera el usuario, una tras otra
  if not BtnSend.Enabled then
  begin
    FFinTurno := 0;
    Exit;
  end;
  if FFinTurno = 0 then
  begin
    FFinTurno := TThread.GetTickCount;
    if FTurno > 0 then
      Marcar('fin' + IntToStr(FTurno));
  end;

  // "@rag archivo": el texto se ve un momento en el prompt, se indexa como con el
  // boton y queda marcado "Usar RAG" para las preguntas siguientes
  if FCargaRAG <> 0 then
  begin
    if TThread.GetTickCount < FCargaRAG then
      Exit;
    FCargaRAG := 0;
    BtnLoadRAGClick(BtnLoadRAG);
    ChkUseRAG.IsChecked := True;
    Tab.ActiveTab := tabLog;
    Marcar('rag' + IntToStr(FTurno));
    FFinTurno := TThread.GetTickCount;
    Exit;
  end;

  if FEscribiendo <> '' then
  begin
    if FTecleadas < FEscribiendo.Length then
    begin
      Inc(FTecleadas);
      MemoPrompt.Lines.Text := FEscribiendo.Substring(0, FTecleadas);
    end
    else if Int64(TThread.GetTickCount) - Int64(FFinTurno) > 700 then
    begin
      FEscribiendo := '';
      Marcar('envio' + IntToStr(FTurno));
      BtnSendClick(BtnSend);
    end;
    Exit;
  end;

  if Int64(TThread.GetTickCount) - Int64(FFinTurno) < FPausaMs then
    Exit;
  if FPrueba.Count = 0 then
  begin
    FTimerPrueba.Enabled := False;
    Close;
    Exit;
  end;
  Pregunta := FPrueba[0];
  FPrueba.Delete(0);
  Inc(FTurno);
  Marcar('q' + IntToStr(FTurno));
  if Pregunta.StartsWith('@rag ') then
  begin
    MemoPrompt.Lines.Text := TFile.ReadAllText(Pregunta.Substring(5).Trim, TEncoding.UTF8);
    FCargaRAG := TThread.GetTickCount + 2500;
  end
  else if FTecleo then
  begin
    FEscribiendo := Pregunta;
    FTecleadas := 0;
    MemoPrompt.Lines.Clear;
    FFinTurno := TThread.GetTickCount + Cardinal(Pregunta.Length) * 50;
  end
  else
  begin
    MemoPrompt.Lines.Text := Pregunta;
    Marcar('envio' + IntToStr(FTurno));
    BtnSendClick(BtnSend);
  end;
end;

procedure TFormMain.Marcar(const ANombre: string);
begin
  // Hora UTC en ms: grabar_ventana.py la convierte a segundos del video (tiempos.json)
  if FMarcas <> '' then
    TFile.AppendAllText(FMarcas, Format('%s'#9'%d'#13#10,
      [ANombre, Round((TTimeZone.Local.ToUniversalTime(Now) - UnixDateDelta) * MSecsPerDay)]));
end;

// =============================================================================
//  Helpers
// =============================================================================
procedure TFormMain.AddLog(const S: string);
begin
  if not Assigned(MemoLog) then
    Exit;
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + S);
  MemoLog.GoToTextEnd;
end;

procedure TFormMain.ChatAppend(const S: string);
begin
  MemoChat.Lines.Text := MemoChat.Lines.Text + S;
  MemoChat.GoToTextEnd;
end;

function TFormMain.ParseFecha(const S: string; aIndex: Integer): TDateTime;
var
  d: TDateTime;
begin
  // ReturnUTC = True: la fecha tal como llega. Con False la convertia a hora local
  // y "2026-10-19" caia el 18 a las 19:00 (UTC-5)
  if (S <> '') and TryISO8601ToDate(S, d, True) then
    Exit(d);
  if (S <> '') and TryStrToDateTime(S, d) then
    Exit(d);
  if (S <> '') and TryStrToDate(S, d) then
    Exit(d);
  // fallback: dias consecutivos desde hoy
  Result := Trunc(Now) + aIndex;
end;

end.
