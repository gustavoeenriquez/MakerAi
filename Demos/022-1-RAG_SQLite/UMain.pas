unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Comp.UI, FireDAC.DApt, Data.DB,
  FireDAC.Comp.Client, Vcl.StdCtrls, uMakerAi.Embeddings.core,
  uMakerAi.Embeddings, uMakerAi.Chat.OpenAi, Vcl.Samples.Spin,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.DBCtrls;

type
  TFMain = class(TForm)
    butConect: TButton;
    FDConnection: TFDConnection;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    memoInfo: TMemo;
    butCreate: TButton;
    memData: TMemo;
    AiOpenAiEmbeddings: TAiOpenAiEmbeddings;
    seTipo: TSpinEdit;
    labTipo: TLabel;
    butAddEmbedding: TButton;
    DBGridQy: TDBGrid;
    FDQy: TFDQuery;
    DSQy: TDataSource;
    butSearch: TButton;
    edSearch: TEdit;
    rgDistance: TRadioGroup;
    seK: TSpinEdit;
    Label1: TLabel;
    DBMemContent: TDBMemo;
    procedure FDConnectionAfterConnect(Sender: TObject);
    procedure butConectClick(Sender: TObject);
    procedure butCreateClick(Sender: TObject);
    procedure butAddEmbeddingClick(Sender: TObject);
    procedure butSearchClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

const
 apiKey = 'sk-proj-1CKK...';

implementation

{$R *.dfm}

procedure InsertDoc(const Conn: TFDConnection; const AContent, EmbeddingJson: string; aTipo: smallint; out NewId: Int64);
var
  Q: TFDQuery;
begin
  Q := TFDQuery.Create(nil);
  try
    Q.Connection := Conn;
    Q.SQL.Text :=
      'INSERT INTO vec_docs (tipo, content, emb_cos, emb_l2) ' +
      'VALUES (:tipo, :content, :emb, :emb)';
    Q.ParamByName('tipo').AsInteger   := aTipo;
    Q.ParamByName('content').AsString := AContent;
    Q.ParamByName('emb').AsString     := EmbeddingJson;
    Q.ExecSQL;

    NewId := Conn.ExecSQLScalar('SELECT last_insert_rowid()');
  finally
    Q.Free;
  end;
end;

procedure TFMain.butAddEmbeddingClick(Sender: TObject);
var
 aN: Int64;
 JsonA: TJSONArray;
begin
 if (memData.Text = '') then
  begin
   memoInfo.Lines.Add('Rellene los datos del documento');
   Exit;
  end;

 try
  AiOpenAiEmbeddings.ApiKey:= apiKey;
  JsonA:= AiOpenAiEmbeddings.ToJsonArray(AiOpenAiEmbeddings.CreateEmbedding(memData.Text,'user'));
  InsertDoc(FDConnection,memData.Text,JsonA.ToJSON,seTipo.Value,aN);
 finally
  JsonA.Free;
 end;

end;

procedure TFMain.butConectClick(Sender: TObject);
begin
 FDConnection.Connected:= True;
end;

procedure TFMain.butCreateClick(Sender: TObject);
const
  SQL_CREATE =
  'CREATE VIRTUAL TABLE IF NOT EXISTS vec_docs USING vec0(' +
  'doc_id    INTEGER PRIMARY KEY,' +
  'tipo   INTEGER,' +
  'content   TEXT,' +
  'emb_cos   FLOAT[1536] distance_metric=cosine,' +
  'emb_l2    FLOAT[1536] distance_metric=l2' +
  ');';
begin
 FDConnection.ExecSQL(SQL_CREATE);
end;

procedure TFMain.butSearchClick(Sender: TObject);
var
 EmbJson: string;
 JsonA: TJSONArray;
begin

 try
  AiOpenAiEmbeddings.ApiKey:= apiKey;
  JsonA:= AiOpenAiEmbeddings.ToJsonArray(AiOpenAiEmbeddings.CreateEmbedding(EdSearch.Text,'user'));
  EmbJson:= JsonA.ToString;
 finally
  JsonA.Free;
 end;

 case rgDistance.ItemIndex of
   0: begin
       FDQy.Active:= false;
       FDQy.Params.Clear;
       FDQy.SQL.Text :=
          'SELECT doc_id, content, distance ' +
          'FROM vec_docs ' +
          'WHERE emb_cos MATCH :q AND k = :k ' +
          'ORDER BY distance ASC'; // 0 = m�s similar (cosine distance)
        FDQy.ParamByName('q').AsString := EmbJson;
        FDQy.ParamByName('k').AsInteger := seK.Value;
        FDQy.Open;
      end;
   1: begin
       FDQy.Active:= false;
       FDQy.Params.Clear;
       FDQy.SQL.Text :=
          'SELECT doc_id, content, distance ' +
          'FROM vec_docs ' +
          'WHERE emb_l2 MATCH :q AND k = :k ' +
          'ORDER BY distance ASC';
        FDQy.ParamByName('q').AsString := EmbJson;
        FDQy.ParamByName('k').AsInteger := seK.Value;
        FDQy.Open;
      end;
 end;

end;

procedure TFMain.FDConnectionAfterConnect(Sender: TObject);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    try
      butCreate.Enabled:= true;
      butAddEmbedding.Enabled:= true;
      Query.Connection := FDConnection;
      Query.SQL.Text := 'SELECT load_extension(''vec0.dll'');';
      Query.Open; // Usamos Open porque es un SELECT
      Query.Close; // Lo cerramos porque no necesitamos el resultado
      memoInfo.Lines.Add('Extensi�n vec.dll cargada correctamente.');
    except
      on E: Exception do
      begin
        butCreate.Enabled:= false;
        butAddEmbedding.Enabled:= false;
        memoInfo.Lines.Add('Error al cargar la extensi�n vec0.dll: ' + E.Message);
        ShowMessage('Aseg�rate de que vec.dll y sqlite3.dll est�n junto al ejecutable y tienen la arquitectura correcta (32/64 bits).');
      end;
    end;
  finally
   Query.Free;
  end;
end;

end.
