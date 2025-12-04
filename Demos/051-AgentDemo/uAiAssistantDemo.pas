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

unit uAiAssistantDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Threading, System.JSON, Rest.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, uMakerAi.Chat, uMakerAi.Prompts, uMakerAi.Core, uMakerAi.Utils.System,
  uMakerAi.Chat.OpenAi, uMakerAi.Agents;

type
  TForm70 = class(TForm)
    Link2: TAIAgentsLink;
    StartNode: TAIAgentsNode;
    AIGraph1: TAIAgents;
    AiNode: TAIAgentsNode;
    ExcecuteNode: TAIAgentsNode;
    EndNode: TAIAgentsNode;
    EvalNode: TAIAgentsNode;
    Link1: TAIAgentsLink;
    Link3: TAIAgentsLink;
    Link4: TAIAgentsLink;
    Memo1: TMemo;
    Button1: TButton;
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
    AIAgents1: TAIAgents;
    AIAgentsLink1: TAIAgentsLink;
    AIAgentsNode1: TAIAgentsNode;
    procedure AIChain1Print(Sender: TObject; Value: string);
    procedure Button1Click(Sender: TObject);
    procedure EndNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure AiNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure AIChain1End(Sender: TObject; Value: string);
    procedure ExcecuteNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure StartNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure EvalNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Link4Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
    procedure NodoInicioExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure TareaAExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure TareaBExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure NodoFinalExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Button2Click(Sender: TObject);
    procedure AIAgentsLink1Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
    procedure AIAgentsLink2Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
    procedure AIAgentsLink3Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
  private
    { Private declarations }
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

procedure TForm70.AIChain1End(Sender: TObject; Value: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Text := Value;
    end);
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

procedure TForm70.AiNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
Var
  Prompt: String;
begin
  Node.Print(Node.Name + '--> Generando código');
  Prompt := AiPrompts1.GetTemplate('AiPrompt', ['Prompt=' + Input]);
  // OJO Output := OpenChat.AddMessageAndRun(Prompt, 'user',[]);

  Output := 'Este es el codigo que retorn la IA';
end;

procedure TForm70.Button1Click(Sender: TObject);
begin
  // OJO OpenChat.Messages.Clear;
  // OJO ChatEval.Messages.Clear;
  AIGraph1.Run(MemoPrompt.Lines.Text);
end;

procedure TForm70.Button2Click(Sender: TObject);
begin
  AIAgents1.Run('Mensaje');
end;

procedure TForm70.EndNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
Var
  JObj: TJSonObject;
  FileName, Codigo, Res: String;
  Lista: TStringList;
begin
  Lista := TStringList.Create;
  JObj := TJSonObject(TJSonObject.ParseJSONValue(Input));
  Try

    If JObj.TryGetValue<String>('filename', FileName) and JObj.TryGetValue<String>('codigo', Codigo) then
    Begin
      FileName := 'c:\tmp\' + FileName;
      Output := Codigo;
      If Not FileExists(FileName) then
      Begin
        Node.Print(Node.Name + '--> No se generó el archivo con el código ' + FileName);
      End
      Else
      Begin
        FileName := ChangeFileExt(FileName, '.exe');

        If Not FileExists(FileName) then
        Begin
          Node.Print(Node.Name + '--> No se generó el Ejecutable ' + FileName);
        End
        Else
        Begin
          Try
            TUtilsSystem.ExecuteCommandLine(FileName);
          Except
            ON E: Exception do
            Begin
              Node.Print(Node.Name + '--> Error intentando ejecutar ' + FileName + ' : ' + E.Message);
              Output := E.Message;
            End;
          End;
        End;
      End;
    End
    Else
    Begin
      Output := 'No se encuentra el archivo ' + FileName;
    End;

    Node.Print(Node.Name + '--> ' + Output);

  Finally
    JObj.Free;
    Lista.Free;
  End;
end;

procedure TForm70.EvalNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> Evaluando....');
  // Realmente aquí no se hace nada

end;

procedure TForm70.ExcecuteNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
Var
  JObj, JRes: TJSonObject;
  FileName, Codigo, Res: String;
  Lista: TStringList;
begin
  Node.Print(Node.Name + '--> Compilando....');

  Lista := TStringList.Create;
  JObj := TJSonObject(TJSonObject.ParseJSONValue(Input));
  Try
    If JObj.TryGetValue<String>('filename', FileName) and JObj.TryGetValue<String>('codigo', Codigo) then
    Begin
      Node.Print(Node.Name + '--> Compilando ' + FileName + '...');
      FileName := 'c:\tmp\' + FileName;
      Lista.Text := Codigo;
      Lista.SaveToFile(FileName);

      Try
        Res := TUtilsSystem.RunCommandLine('DCC64 ' + FileName);
        Node.Print('Mensaje compilación :' + sLineBreak + Res);
      Except
        ON E: Exception do
        Begin
          Res := E.Message;
        End;
      End;

      JRes := TJSonObject.Create;
      Try
        JRes.AddPair('filename', FileName);
        JRes.AddPair('codigo', Codigo);
        JRes.AddPair('resultado_compilacion', Res);
        Output := JRes.Format;
      Finally
        JRes.Free;
      End;
    End
    Else
    Begin
      Output := 'No se encuentra la variable filename o codigo en el json';
    End;

  Finally
    JObj.Free;
    Lista.Free;
  End;
end;

procedure TForm70.Link4Execute(Node: TAIAgentsNode; Link: TAIAgentsLink; var IsOk, Handled: Boolean);
Var
  Prompt, Res: String;
  JObj: TJSonObject;
  FileName, Codigo, resultado_compilacion: String;
begin

  JObj := TJSonObject(TJSonObject.ParseJSONValue(Node.Output));
  Try
    If JObj.TryGetValue<String>('filename', FileName) and JObj.TryGetValue<String>('resultado_compilacion', resultado_compilacion) then
    Begin

      Prompt := AiPrompts1.GetTemplate('Eval', ['resultado=' + resultado_compilacion]);

      // OJO ChatEval.Messages.Clear; // Limpia la memoria, consume menos tokens
      // OJO Res := ChatEval.AddMessageAndRun(Prompt, 'user', []);

      If Pos('<ERROR', Res) > 0 then
      Begin
        IsOk := False;
        Link.Print(Node.Name + '--> Error: ' + Res);
      End
      Else
      Begin
        Link.Print(Node.Name + '--> Compiló correctamente, pasa al nodo final ');
        IsOk := True;
      End;
    End
    Else
      IsOk := False;
  Finally
    JObj.Free;
  End;
end;

procedure TForm70.NodoFinalExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> Finalizando proceso');

end;

procedure TForm70.NodoInicioExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> Inicicando proceso');
end;

procedure TForm70.StartNodeExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> Inicicando proceso');
end;

procedure TForm70.TareaAExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> TareaA');
  Sleep(1500);

end;

procedure TForm70.TareaBExecute(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> TareaB');
  Sleep(500);
end;

end.
