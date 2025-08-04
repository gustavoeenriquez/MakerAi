unit uRagUpdateDBMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.JSON, Rest.JSON, System.threading,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Layouts,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.PG, FireDAC.Phys.PGDef, FireDAC.FMXUI.Wait,
  Data.DB, FireDAC.Comp.Client,
  uMakerAi.RAG.Vectors, uMakerAi.Embeddings, uMakerAi.Chat.Ollama, uMakerAi.Core, uMakerAi.Chat,

  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Comp.UI, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt, FMX.Bind.DBEngExt, FMX.Bind.Grid, System.Bindings.Outputs,
  FMX.Bind.Editors, Data.Bind.Components, FMX.Edit,
  Data.Bind.Grid, Data.Bind.DBScope, FMX.Grid, uMakerAi.Chat.AiConnection, uMakerAi.Prompts;

type
  TForm8 = class(TForm)
    Layout1: TLayout;
    Text1: TText;
    DbConn: TFDConnection;
    RAGVectorAdicion: TAiRAGVector;
    BtnAddText: TButton;
    MemoTextToAdd: TMemo;
    AiOllamalEmbeddings1: TAiOllamalEmbeddings;
    BusQuery: TFDQuery;
    Grid1: TGrid;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    BusQueryid: TLargeintField;
    BusQueryfechadoc: TSQLTimeStampField;
    BusQuerycategoria: TWideStringField;
    BusQuerypathdoc: TWideStringField;
    BusQueryfilename: TWideStringField;
    BusQueryresumen: TWideMemoField;
    EditPrompt: TEdit;
    Label1: TLabel;
    BtnBuscar: TButton;
    Memo1: TMemo;
    LinkControlToField1: TLinkControlToField;
    RagChat: TAiRagChat;
    MemoRes: TMemo;
    RAGVectorConsulta: TAiRAGVector;
    AiChatConnection1: TAiChatConnection;
    BusQuerydistancia: TFloatField;
    procedure BtnAddTextClick(Sender: TObject);
    procedure RAGVectorAdicionDataVecAddItem(Sender: TObject; aItem: TAiEmbeddingNode; MetaData: TAiEmbeddingMetaData;
      var Handled: Boolean);
    procedure BtnBuscarClick(Sender: TObject);
    procedure RAGVectorConsultaDataVecSearch(Sender: TObject; Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double;
      var aDataVec: TAiRAGVector; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure AiChatConnection1ReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
  private
    GlLista: TStringList;
    Function NewQuery: TFDQuery;
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.fmx}

procedure TForm8.RAGVectorAdicionDataVecAddItem(Sender: TObject; aItem: TAiEmbeddingNode; MetaData: TAiEmbeddingMetaData;
  var Handled: Boolean);
Var
  Query: TFDQuery;
  sEmbedding, Texto: String;
  JArr: TJSonArray;

  fechadoc, categoria, pathdoc, filename: String;
  resumen, fileformat, sJson: String;

begin

  fechadoc := MetaData.Data.Values['fechadoc'];
  categoria := MetaData.Data.Values['categoria'];
  pathdoc := MetaData.Data.Values['pathdoc'];
  filename := MetaData.Data.Values['filename'];
  resumen := MetaData.Data.Values['resumen'];
  sJson := MetaData.Data.Values['json'];
  fileformat := MetaData.Data.Values['fileformat'];
  Texto := MetaData.TagString;

  Query := NewQuery;
  Try
    JArr := aItem.ToJsonArray;
    Try
      sEmbedding := JArr.ToString;
    Finally
      JArr.Free;
    End;

    Query.Sql.Clear;
    Query.Sql.Add('Insert into anexos(FechaDoc, Categoria, PathDoc, FileName, Resumen, FileFormat, embedding)');
    Query.Sql.Add('VALUES (:FechaDoc, :Categoria, :PathDoc, :FileName, :Resumen, :FileFormat,  ''' + sEmbedding + ''')');
    Query.Sql.Add('Returning Id');

    Query.Params.ParamByName('FechaDoc').AsDateTime := Now;
    Query.Params.ParamByName('PathDoc').AsString := pathdoc;
    Query.Params.ParamByName('Categoria').AsString := categoria;
    Query.Params.ParamByName('FileName').AsString := filename;
    Query.Params.ParamByName('Resumen').AsString := Texto;
    Query.Params.ParamByName('Resumen').Size := Length(Texto) + 100;
    // Query.Params.ParamByName('Json').AsString := '';
    Query.Params.ParamByName('FileFormat').AsString := fileformat;
    Query.Open;

    Handled := True;

  Finally
    Query.Free;
  End;
end;

procedure TForm8.RAGVectorConsultaDataVecSearch(Sender: TObject; Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double;
  var aDataVec: TAiRAGVector; var Handled: Boolean);
Var
  Query: TFDQuery;
  sEmbedding, Texto: String;
  JArr: TJSonArray;
  Emb: TAiEmbeddingNode;
begin

  aDataVec := TAiRAGVector.Create(Nil); // Crea un vector de respuesta temporal

  Query := NewQuery;
  Try
    JArr := Target.ToJsonArray;
    Try
      sEmbedding := JArr.ToString;
    Finally
      JArr.Free;
    End;


    Query.Sql.Clear;
    Query.Sql.Add('SELECT id, resumen, embedding <-> ''' + sEmbedding + ''' as distancia');
    Query.Sql.Add('FROM anexos');
    Query.Sql.Add('ORDER BY embedding <-> ''' + sEmbedding + '''');
    Query.Sql.Add('LIMIT ' + aLimit.ToString);
    Query.Open;

    GlLista.Clear;

    While not Query.Eof do
    Begin
      Emb := TAiEmbeddingNode.Create(Target.Dim);
      Emb.Text := Query.FieldByName('resumen').AsString;
      aDataVec.AddItem(Emb, Nil);
      GlLista.Add(Query.FieldByName('Id').AsString);
      Query.Next;
    End;

    TTask.Run(
      procedure
      begin
        TThRead.Queue(nil,
          procedure
          begin
            BusQuery.Close;
            BusQuery.Sql.Clear;
            BusQuery.Sql.Add('Select id, fechadoc, categoria, pathdoc, filename, resumen,');
            BusQuery.Sql.Add('  embedding <-> ''' + sEmbedding + ''' as distancia');
            BusQuery.Sql.Add('FROM anexos');
            BusQuery.Sql.Add('ORDER BY embedding <-> ''' + sEmbedding + '''');
            BusQuery.Sql.Add('LIMIT ' + aLimit.ToString);
            BusQuery.Open;
          end);
      end);

    Handled := True;
  Finally
    Query.Free;
  End;
end;

procedure TForm8.AiChatConnection1ReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  MemoRes.Text := MemoRes.Text + aText;
end;

procedure TForm8.BtnAddTextClick(Sender: TObject);
Var
  Texto: String;
  Md: TAiEmbeddingMetaData;
begin
  Texto := MemoTextToAdd.Lines.Text;

  Md := TAiEmbeddingMetaData.Create;
  Try

    Md.Data.Values['fechadoc'] := FormatDateTime('YYYY-MM-DD hh:nn:ss', Now);
    Md.Data.Values['categoria'] := 'Pruebas';
    Md.Data.Values['pathdoc'] := '';
    Md.Data.Values['filename'] := '';
    Md.Data.Values['resumen'] := '';
    // Md.Data.Values['json'] := ;
    Md.Data.Values['fileformat'] := 'txt';
    Md.TagString := Texto;

    RAGVectorConsulta.AddItem(Texto, Md);
  Finally
    Md.Free;
  End;
end;

procedure TForm8.BtnBuscarClick(Sender: TObject);
Var
  Prompt, Res: String;
  Limit: Integer;
  Presicion: Double;

begin
  Limit := 10;
  Presicion := 0.5;
  Prompt := EditPrompt.Text;
  Res := RagChat.AskToAi(Prompt, Limit, Presicion);
  MemoRes.Lines.Text := Res;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  GlLista := TStringList.Create;

end;

function TForm8.NewQuery: TFDQuery;
begin
  Result := TFDQuery.Create(Self);
  Result.Connection := DbConn;
end;

end.




procedure TForm8.RAGVectorAdicionDataVecAddItem(Sender: TObject; aItem: TAiEmbeddingNode; MetaData: TAiEmbeddingMetaData;
  var Handled: Boolean);
Var
  Query: TFDQuery;
  sEmbedding, Texto: String;
  JArr: TJSonArray;

  fechadoc, categoria, pathdoc, filename: String;
  resumen, fileformat, sJson: String;

begin

  fechadoc := MetaData.Data.Values['fechadoc'];
  categoria := MetaData.Data.Values['categoria'];
  pathdoc := MetaData.Data.Values['pathdoc'];
  filename := MetaData.Data.Values['filename'];
  resumen := MetaData.Data.Values['resumen'];
  sJson := MetaData.Data.Values['json'];
  fileformat := MetaData.Data.Values['fileformat'];
  Texto := MetaData.TagString;

  Query := NewQuery;
  Try
    JArr := aItem.ToJsonArray;
    Try
      sEmbedding := JArr.ToString;
    Finally
      JArr.Free;
    End;

    Query.Sql.Clear;
    Query.Sql.Add('Insert into anexos(FechaDoc, Categoria, PathDoc, FileName, Resumen, FileFormat, embedding)');
    Query.Sql.Add('VALUES (:FechaDoc, :Categoria, :PathDoc, :FileName, :Resumen, :FileFormat,  ''' + sEmbedding + ''')');
    Query.Sql.Add('Returning Id');

    Query.Params.ParamByName('FechaDoc').AsDateTime := Now;
    Query.Params.ParamByName('PathDoc').AsString := pathdoc;
    Query.Params.ParamByName('Categoria').AsString := categoria;
    Query.Params.ParamByName('FileName').AsString := filename;
    Query.Params.ParamByName('Resumen').AsString := Texto;
    Query.Params.ParamByName('Resumen').Size := Length(Texto) + 100;
    // Query.Params.ParamByName('Json').AsString := '';
    Query.Params.ParamByName('FileFormat').AsString := fileformat;
    Query.Open;

    Handled := True;

  Finally
    Query.Free;
  End;
end;




procedure TForm8.RAGVectorConsultaDataVecSearch(Sender: TObject; Target: TAiEmbeddingNode; aLimit: Integer; aPrecision: Double;
  var aDataVec: TAiRAGVector; var Handled: Boolean);
Var
  Query: TFDQuery;
  sEmbedding, Texto: String;
  JArr: TJSonArray;
  Emb: TAiEmbeddingNode;
begin

  aDataVec := TAiRAGVector.Create(Nil); // Crea un vector de respuesta temporal

  Query := NewQuery;

  Try
    //1. Obteneemos el embedding de la consulta para realizar la búsqueda
    JArr := Target.ToJsonArray;
    Try
      sEmbedding := JArr.ToString;
    Finally
      JArr.Free;
    End;


    //2. ejecutamos el query organizandolo por parecido con la consulta
    Query.Sql.Clear;
    Query.Sql.Add('SELECT id, resumen, embedding <-> ''' + sEmbedding + ''' as distancia');
    Query.Sql.Add('FROM anexos');
    Query.Sql.Add('ORDER BY embedding <-> ''' + sEmbedding + '''');
    Query.Sql.Add('LIMIT ' + aLimit.ToString);
    Query.Open;

    //3. creamos un dataset con los registros que más se parecen adicionando la información obtenida en cada registro.
    //   Si los registros hacen referencia a documentos almacenados en lugar del resumen se puede adicionar el texto del documento
    While not Query.Eof do
    Begin
      Emb := TAiEmbeddingNode.Create(Target.Dim);
      Emb.Text := Query.FieldByName('resumen').AsString;
      aDataVec.AddItem(Emb, Nil);
      Query.Next;
    End;

    //4. Se indica que se maneja el procedimiento aquí, es importante para que el sistema no intente almacenarlo en memoria.
    Handled := True;
  Finally
    Query.Free;
  End;
end;

