unit uRagChatMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, System.threading, System.SyncObjs,
  System.JSON, Rest.JSON, System.Generics.Collections, System.Generics.Defaults,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.ListBox,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Phys.PG, FireDAC.Phys.PGDef,
  FMX.TabControl, FMX.Edit,
  uMakerAi.ToolFunctions, uMakerAi.Embeddings, uMakerAi.Vectors, uMakerAi.Chat, uMakerAi.Core;

type

  TForm69 = class(TForm)
    Layout1: TLayout;
    BtnLoadData: TButton;
    OpenChat: TAiOpenChat;
    RagChat: TAiRagChat;
    DataVec1: TAiDataVec;
    DbConn: TFDConnection;
    AiEmbeddings1: TAiEmbeddings;
    TabControl1: TTabControl;
    TabItemPreparacion: TTabItem;
    TabItemRAG: TTabItem;
    Layout2: TLayout;
    Layout5: TLayout;
    LayChat: TLayout;
    Layout3: TLayout;
    MemoChat: TMemo;
    Layout6: TLayout;
    MemoPrompt: TMemo;
    BtnPlay: TSpeedButton;
    Splitter2: TSplitter;
    Layout7: TLayout;
    BtnParseJSon: TButton;
    BtnSaveData: TButton;
    Layout4: TLayout;
    Layout8: TLayout;
    Label1: TLabel;
    MemoForParse: TMemo;
    Layout9: TLayout;
    Label2: TLabel;
    MemoDb: TMemo;
    Splitter1: TSplitter;
    SaveDialogDb: TSaveDialog;
    OpenDialogDB: TOpenDialog;
    BtnPrepareTexto: TButton;
    BtnLoadFile: TButton;
    BtnSaveFile: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    ChBDPostgres: TRadioButton;
    ChBDMemoria: TRadioButton;
    BtnConectarPostgres: TButton;
    Layout10: TLayout;
    Label3: TLabel;
    EditLimite: TEdit;
    Label4: TLabel;
    EditPrecision: TEdit;
    AiFunctions1: TAiFunctions;
    procedure BtnPlayClick(Sender: TObject);
    procedure BtnLoadDataClick(Sender: TObject);
    procedure BtnSaveDataClick(Sender: TObject);
    procedure DataVec1DataVecSearch(Sender: TObject; Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; var aDataVec: TAiDataVec; var Handled: Boolean);
    procedure DataVec1DataVecAddItem(Sender: TObject; aItem: TAiEmbeddingNode; var Handled: Boolean);
    procedure BtnParseJSonClick(Sender: TObject);
    procedure BtnPrepareTextoClick(Sender: TObject);
    procedure BtnSaveFileClick(Sender: TObject);
    procedure BtnLoadFileClick(Sender: TObject);
    procedure BtnConectarPostgresClick(Sender: TObject);
    procedure OpenChatFunctions0get_fechaAction(Sender: TObject;
      FunctionAction: TFunctionActionItem; FunctionName: string;
      ToolCall: TAiToolsFunction; var Handled: Boolean);
     procedure Chat1ReceiveDataEnd(const Sender: TObject;  aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
     procedure Chat1ReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
  private
    // DataVec: TAiDataVec;
    GlChat: TAiOpenChat;

  public
    Procedure UpdateMemo(Text: String);
    Function BuscarRAG(Prompt: String; aPresicion: Double = 0.5; aLimit: Integer = 10): String;
  end;

var
  Form69: TForm69;

implementation

{$R *.fmx}
{ TAiEmbedding }

{ TForm69 }

Function NewConnection: TFDConnection;
Begin
  Result := TFDConnection.Create(Nil);
  Result.Params.Add('Database=prueba');
  Result.Params.Add('User_Name=usrcimadb');
  Result.Params.Add('Server=192.168.101.11');
  Result.Params.Add('Password=masterkey');
  Result.Params.Add('DriverID=PG');
  Result.Open;
End;

Function NewQuery(DbConn: TFDConnection; Sql: String): TFDQuery;
Begin
  Result := TFDQuery.Create(Nil);
  Result.Connection := DbConn;
  Result.Sql.Text := Sql;
End;

procedure TForm69.BtnConectarPostgresClick(Sender: TObject);
begin
  showMessage('Esta opción abre la conexión a la base de datos postgres');

  DbConn.Open;
  showMessage('Conectado a la base de datos');
end;

procedure TForm69.BtnLoadDataClick(Sender: TObject);
begin
  showMessage('Esta opción lee un archivo json que representa una base de datos en memoria, debe tener el formato adecuado');
  If OpenDialogDB.Execute then
  Begin
    DataVec1.Clear;
    DataVec1.LoadFromFile(OpenDialogDB.FileName);
    DataVec1.BuildIndex;
    showMessage('La base de datos se cargó correctamente');

    Var
      St: TStringStream := TStringStream.Create;
    Try
      DataVec1.SaveToStream(St);
      MemoDb.Lines.Text := St.DataString;
    Finally
      St.Free;
    End;

  End;
end;

procedure TForm69.BtnLoadFileClick(Sender: TObject);
begin
  showMessage('Esta opción abre un archivo de texto y lo deja en el memo, no lo procesa todavía');

  If OpenDialog1.Execute then
    MemoForParse.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm69.BtnParseJSonClick(Sender: TObject);
Var
  JArr: TJSonArray;

begin
  showMessage('Esta opción requiere que en el memo exista un TJSonVector, no importa el formato');
  // Crea un vector de búsquedas por medio de un json
  JArr := TJSonArray(TJSONObject.ParseJSONValue(MemoForParse.Lines.Text));
  DataVec1.AddItemsFromJSonArray(JArr);

  Var
    St: TStringStream := TStringStream.Create;
  Try
    DataVec1.SaveToStream(St);
    MemoDb.Lines.Text := St.DataString;
  Finally
    St.Free;
  End;
end;

procedure TForm69.BtnPrepareTextoClick(Sender: TObject);
Var
  JArr: TJSonArray;
begin
  showMessage('Estas opción recibe un texto cualquiera, lo procesa por segmentos, esto puede tardar algunos minutos');
  // Crea un vector de búsquedas por medio de un texto plano, no es tan exacta pero es la más básica
  Cursor := crHourGlass;
  Try
    DataVec1.AddItemsFromPlainText(MemoForParse.Lines.Text, 1024, 200);
  Finally
    Cursor := crDefault;
  End;
end;

procedure TForm69.BtnPlayClick(Sender: TObject);
Var
  Prompt, Res: String;
  Limite : Integer;
  Precision : Single;
Begin

  Limite := StrToIntDef(EditLimite.Text,5);
  Precision := StrToFloatDef(EditPrecision.Text,0.7);

  Prompt := MemoPrompt.Lines.Text;

  UpdateMemo('user : ' + Prompt);

  Res := RagChat.AskToAi(Prompt, Limite, Precision);

  UpdateMemo(Res);
  MemoChat.Lines.Add('');
  MemoChat.Lines.Add('');
end;

procedure TForm69.BtnSaveDataClick(Sender: TObject);
begin
  showMessage('Esta opción guarda la base de datos de la memoria en un json en el disco');
  If SaveDialogDb.Execute then
  Begin
    DataVec1.SaveToFile(SaveDialogDb.FileName);
  End;
end;

procedure TForm69.BtnSaveFileClick(Sender: TObject);
begin
  showMessage('Esta opción guarda en un archivo de texto el contenido del memo, sin hacerle ningún cambio');
  If SaveDialog1.Execute then
    MemoForParse.Lines.SaveToFile(SaveDialog1.FileName);
end;

function TForm69.BuscarRAG(Prompt: String; aPresicion: Double = 0.5; aLimit: Integer = 10): String;
Var
  I: Integer;
  Emb, EmbA: TAiEmbeddingNode;
  Idx: Double;
  Text: String;
  Res: String;
  TmpVec: TAiDataVec;
begin
  // EmbA := CreateEmbedding(Prompt);

  TmpVec := DataVec1.Search(EmbA, aLimit, aPresicion);
  Try

    Text := '';
    For I := 0 to TmpVec.Count - 1 do
    Begin
      Emb := TmpVec.Items[I];
      Result := Result + Emb.Text.trim + #$D#$A;
    End;
  Finally
    TmpVec.Free;
  End;
end;

procedure TForm69.Chat1ReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  UpdateMemo(aText);
end;

procedure TForm69.DataVec1DataVecAddItem(Sender: TObject; aItem: TAiEmbeddingNode; var Handled: Boolean);
Var
  Query: TFDQuery;
  sEmbedding, Texto: String;
  JArr: TJSonArray;
begin
  If ChBDMemoria.IsChecked then // Si handled = false utiliza la base de datos en memoria por defecto
  Begin
    Handled := False;
    Exit;
  End;

  Query := NewQuery(DbConn, '');
  Try
    JArr := aItem.ToJsonArray;
    Try
      sEmbedding := JArr.ToString;
    Finally
      JArr.Free;
    End;

    Query.Sql.Clear;
    Query.Sql.Add('Insert into RagData(categoria, texto, embedding)');
    Query.Sql.Add('VALUES (:categoria, :texto, ''' + sEmbedding + ''')');
    Query.Sql.Add('Returning Id');
    Query.Params.ParamByName('categoria').AsString := 'motofacil';
    Query.Params.ParamByName('texto').AsString := aItem.Text;
    Query.Open;
  Finally
    Query.Free;
  End;
end;

procedure TForm69.DataVec1DataVecSearch(Sender: TObject; Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double; var aDataVec: TAiDataVec; var Handled: Boolean);
Var
  Query: TFDQuery;
  sEmbedding, Texto: String;
  JArr: TJSonArray;
  Emb: TAiEmbeddingNode;
begin
  If ChBDMemoria.IsChecked then // Si handled = false utiliza la base de datos en memoria por defecto
  Begin
    Handled := False;
    Exit;
  End;

  aDataVec := TAiDataVec.Create(Nil); // Crea un vector de respuesta temporal

  Query := NewQuery(DbConn, '');
  Try
    JArr := Target.ToJsonArray;
    Try
      sEmbedding := JArr.ToString;
    Finally
      JArr.Free;
    End;

    Query.Sql.Clear;
    Query.Sql.Clear;
    Query.Sql.Add('SELECT id, texto, embedding <-> ''' + sEmbedding + ''' as distancia');
    Query.Sql.Add('FROM RagData');
    Query.Sql.Add('ORDER BY embedding <-> ''' + sEmbedding + '''');
    Query.Sql.Add('LIMIT 5');
    Query.Open;

    While not Query.Eof do
    Begin
      Emb := TAiEmbeddingNode.Create(Target.Dim);
      Emb.Text := Query.FieldByName('texto').AsString;
      aDataVec.AddItem(Emb);
      Query.Next;
    End;

    Handled := True;

  Finally
    Query.Free;
  End;
end;


procedure TForm69.OpenChatFunctions0get_fechaAction(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
Var
  Localizacion: String;
begin
  Localizacion := ToolCall.Params.Values['localizacion'];
  ToolCall.Response := FormatDateTime('DD/MM/YYYY hh:nn:ss', Now);
end;

procedure TForm69.Chat1ReceiveDataEnd(const Sender: TObject;  aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin

  // Si no es asincrono se puede obtener el resultado aquí
  // o directamente en el llamado de la consulta en el botón de BtnPlay  Res := AiOpenChat1.AddMessageAndRun(MemoPrompt.Text, 'user');
  // If AiOpenChat1.Asynchronous = False then
  // UpdateMemo(Text);

  // If GlChat.Asynchronous then
  UpdateMemo('');
  // Else
  // UpdateMemo(Role + ' : ' + Text);

  MemoChat.Lines.Add('');
  BtnPlay.StyleLookup := 'playtoolbutton';
end;


procedure TForm69.UpdateMemo(Text: String);
begin
  MemoChat.BeginUpdate;
  Try
    MemoChat.Lines.Text := MemoChat.Lines.Text + Text;
    MemoChat.SelStart := Length(MemoChat.Text);
  Finally
    MemoChat.EndUpdate;
  End;
end;

end.
