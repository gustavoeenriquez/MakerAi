unit uAiModule;

interface

uses
  System.SysUtils, System.Classes, uOpenAi, uAiVectors, uAiOpenChat,
  System.JSON;

type
  TAiModule = class(TDataModule)
    RagChat: TAiRagChat;
    DataVec1: TAiDataVec;
    AiEmbeddings1: TAiEmbeddings;
    OpenChat: TAiOpenChat;
  private
    { Private declarations }
  public
    Function AskToIA(Prompt : String; Limite : Integer; Precision : Double) : String;
    Function LoadJSonFromFile(FileName : String) : Boolean;
    Function LoadFromJSonString(JSon : String) : Boolean;
  end;

var
  AiModule: TAiModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TAiModule }

function TAiModule.AskToIA(Prompt: String; Limite: Integer;
  Precision: Double): String;
begin
   Result := RagChat.AskToAi(Prompt, Limite, Precision);
end;

function TAiModule.LoadFromJSonString(JSon: String): Boolean;
Var
  JArr: TJSonArray;
begin
  JArr := TJSonArray(TJSONObject.ParseJSONValue(JSon));
  DataVec1.AddItemsFromJSonArray(JArr);

  {
  Var
    St: TStringStream := TStringStream.Create;
  Try
    DataVec1.SaveToStream(St);
    MemoDb.Lines.Text := St.DataString;
  Finally
    St.Free;
  End;
  }
end;

function TAiModule.LoadJSonFromFile(FileName: String): Boolean;
Var
  JArr: TJSonArray;
  Lista : TStringList;
begin
  Lista := TStringList.Create;
  Try
  Lista.LoadFromFile(FileName);
  Result := LoadFromJSonString(Lista.Text);
  Finally
    Lista.Free;
  End;
end;

end.
