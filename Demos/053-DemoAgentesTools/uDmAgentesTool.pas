unit uDmAgentesTool;

interface

uses
  System.SysUtils, System.Classes, uMakerAi.Chat, uMakerAi.Chat.Gemini, uMakerAi.Chat.OpenAi,
  uMakerAi.Embeddings.core, uMakerAi.Embeddings, uMakerAi.Chat.Ollama, uMakerAi.RAG.Vectors,
  uMakerAi.RAG.MetaData, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  uMakerAi.Chat.Messages,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.FMXUI.Wait, Data.DB,
  FireDAC.Comp.Client, uMakerAi.RAG.Vector.Driver.Postgres, FireDAC.Phys.PGDef, FireDAC.Comp.UI,
  FireDAC.Phys.PG, uMakerAi.Chat.Tools, uMakerAI.Ollama.Pdf, FMX.Graphics;

type
  TFDataModule = class(TDataModule)
    AiGeminiChat1: TAiGeminiChat;
    AiOpenChat1: TAiOpenChat;
    AiRAGVector1: TAiRAGVector;
    AiOllamaEmbeddings1: TAiOllamaEmbeddings;
    VectorRAGDriver: TAiRAGVectorPostgresDriver;
    FDConnection1: TFDConnection;
    AiOllamaPdfTool1: TAiOllamaPdfTool;
    NanoBanana: TAiGeminiChat;
    OllamaHtml: TAiOllamaChat;
  private
    { Private declarations }
  public
    Function WebSearch(Pregunta: String): String;
    Function BuscarRAGV(Pregunta: String): String;
    Function LoadRAGV: Boolean;
    Function CrearTablaVectoRAG : Boolean;
    Function BuscarVQL(Pregunta : String) : String;
    Function PdfToText(FileName : String) : String;
    Function CreateImage(Pregunta : String) : TBitMap;
    Function CreateDashBoard(Pregunta : String) : String;
  end;

var
  FDataModule: TFDataModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

function TFDataModule.BuscarRAGV(Pregunta: String): String;
begin
  Result := AiRAGVector1.SearchText(Pregunta, 10, 0.7);
end;

function TFDataModule.BuscarVQL(Pregunta: String): String;
begin
  Result := AiRAGVector1.ExecuteVGQL(Pregunta);
end;

function TFDataModule.CrearTablaVectoRAG: Boolean;
begin
   VectorRAGDriver.CreateSchema(VectorRAGDriver.TableName, 1024);
   Result := True;
end;

function TFDataModule.CreateDashBoard(Pregunta: String): String;
Var
  MsgRes : TAiChatMessage;
begin

   Result := OllamaHtml.AddMessageAndRun(Pregunta,'user',[]);

   MsgRes := OllamaHtml.GetLastMessage;

   If MsgRes.MediaFiles.Count > 0 then
   Begin
      Result := MsgRes.MediaFiles[0].ToString;
   End;
end;

function TFDataModule.CreateImage(Pregunta: String): TBitMap;
Var
  Res : String;
  MsgRes : TAiChatMessage;
begin
   Result := Nil;

   Res := NanoBanana.AddMessageAndRun(Pregunta,'user',[]);

   MsgRes := NanoBanana.GetLastMessage;

   If MsgRes.MediaFiles.Count > 0 then
   Begin
      Result := TBitMap.Create;
      Result.LoadFromStream(MsgRes.MediaFiles[0].Content);
   End;
end;

function TFDataModule.LoadRAGV: Boolean;
Var
  St: TStringStream;
  Text: String;
  MetaData: taiEmbeddingMetaData;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  St.LoadFromFile('c:\temp\El Misterio de la Biblioteca.txt');

  MetaData := taiEmbeddingMetaData.Create;
  Try
    Text := St.DataString;
    MetaData.Properties['tipo'] := 'Cuento';
    MetaData.Properties['Autor'] := 'Claude';
    MetaData.Properties['Año'] := 2026;
    MetaData.Properties['Fecha'] := EncodeDate(2026,01,10);

    AiRAGVector1.AddItemsFromPlainText(Text, MetaData, 200, 15);
  Finally
    St.Free;
  End;
end;

function TFDataModule.PdfToText(FileName: String): String;
begin
   Result := AiOllamaPdfTool1.ExtractText(FileName);
end;

{ TDataModule23 }

function TFDataModule.WebSearch(Pregunta: String): String;
begin
  AiGeminiChat1.Asynchronous := False;
  // Result := AiGeminiChat1.AddMessageAndRun(Pregunta, 'user', []);
  Result := AiOpenChat1.AddMessageAndRun(Pregunta, 'user', []);
end;

end.
