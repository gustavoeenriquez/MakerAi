// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


unit uMainCompareEmbeddings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uMakerAi.Embeddings.core, uMakerAi.Embeddings, uMakerAi.Chat.Ollama;

type
  TForm14 = class(TForm)
    AiOllamaEmbeddings1: TAiOllamaEmbeddings;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button2Click(Sender: TObject);
  private
    Res1, Res2 : TAiEmbeddingData;
  public
    { Public declarations }
  end;

var
  Form14: TForm14;

implementation

{$R *.dfm}

procedure TForm14.Button2Click(Sender: TObject);
Var
  Input : String;
  Coseno, Euclidian : Double;
begin
   Input := Memo1.Lines.Text;
   Res1 := AiOllamaEmbeddings1.CreateEmbedding(Input, 'user');
   Memo2.Lines.Text := AiOllamaEmbeddings1.ToJsonArray(Res1).Format;

   Input := Memo3.Lines.Text;
   Res2 := AiOllamaEmbeddings1.CreateEmbedding(Input, 'user');
   Memo4.Lines.Text := AiOllamaEmbeddings1.ToJsonArray(Res2).Format;


   Coseno := AiOllamaEmbeddings1.CosineSimilarity(Res1, Res2);
   Euclidian := AiOllamaEmbeddings1.EuclideanDistance(Res1, Res2);

   ShowMessage('Coseno = '+Coseno.ToString + sLineBreak+'Euclidian = '+Euclidian.ToString);

end;

end.
