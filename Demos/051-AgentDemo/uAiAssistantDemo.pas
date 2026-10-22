// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
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

unit uAiAssistantDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Threading, System.JSON, Rest.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, uMakerAi.Chat, uMakerAi.Prompts, uMakerAi.Core, uMakerAi.Utils.System,
  uMakerAi.Chat.OpenAi, uMakerAi.Agents, uMakerAi.Chat.AiConnection, uMakerAi.Chat.Messages;

type
  TForm70 = class(TForm)
    Memo1: TMemo;
    AiPrompts1: TAiPrompts;
    MemoPrompt: TMemo;
    Memo2: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NodoInicio: TAIAgentsNode;
    TareaB: TAIAgentsNode;
    TareaA: TAIAgentsNode;
    NodoFinal: TAIAgentsNode;
    Button2: TButton;
    AIAgentsManager: TAIAgentManager;
    AIAgentsLink1: TAIAgentsLink;
    AiOpenChat1: TAiOpenChat;
    procedure AIChain1Print(Sender: TObject; Value: string);
    procedure NodoInicioExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure TareaAExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure TareaBExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure NodoFinalExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Button2Click(Sender: TObject);
    procedure AIAgentsLink1Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
    procedure AIAgentsLink2Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
    procedure AIAgentsLink3Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
  private
  public
    { Public declarations }
  end;

var
  Form70: TForm70;

implementation

{$R *.fmx}

procedure TForm70.AIAgentsLink1Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
begin
  Node.Print(Node.Name + '--> Link1');

end;

procedure TForm70.AIAgentsLink2Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
begin
  Node.Print(Node.Name + '--> LinkA');

end;

procedure TForm70.AIAgentsLink3Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
begin
  Node.Print(Node.Name + '--> LinkB');

end;

procedure TForm70.AIChain1Print(Sender: TObject; Value: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin

      If Sender is TAIAgentsNode then
      Begin
        Memo2.Lines.Add(TAIAgentsNode(Sender).Name + '--> ' + Value);
      End;

      If Sender is TAIAgentsLink then
      Begin
        Memo2.Lines.Add(TAIAgentsLink(Sender).Name + '--> ' + Value);
      End;
    end);
end;

procedure TForm70.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  AIAgentsManager.Run(MemoPrompt.Lines.Text);
end;

procedure TForm70.NodoFinalExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  // JoinMode = jmAll: llega una vez, con las salidas de las dos ramas juntas
  Node.Print(Node.Name + '--> Uniendo resultados');
  Output := Input;
  var Texto := Output;   // un parametro var no se puede capturar en el metodo anonimo
  TThread.Queue(nil,
    procedure
    begin
      Memo1.Lines.Text := Texto;
    end);
end;

procedure TForm70.NodoInicioExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> Inicicando proceso');
  Output := Input;
end;

procedure TForm70.TareaAExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  // Rama A: el modelo responde la pregunta. Corre en un hilo del pool, en
  // paralelo con la rama B; la llamada sincrona no bloquea la ventana.
  Node.Print(Node.Name + '--> TareaA - Consultando el modelo');
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
    Output := 'TareaA: falta la variable OPENAI_API_KEY'
  else
  begin
    AiOpenChat1.NewChat;
    Output := 'TareaA (modelo): ' + AiOpenChat1.AddMessageAndRun(Input, 'user', []);
  end;
  Node.Print(Node.Name + '--> TareaA - Respuesta recibida');
end;

procedure TForm70.TareaBExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  // Rama B: un analisis local, sin modelo, mientras la rama A espera la respuesta
  Node.Print(Node.Name + '--> TareaB - Analisis local');
  Output := Format('TareaB (local): la pregunta tiene %d palabras y %d caracteres.',
    [Length(Input.Trim.Split([' ', #13, #10], TStringSplitOptions.ExcludeEmpty)), Length(Input.Trim)]);
end;

end.
