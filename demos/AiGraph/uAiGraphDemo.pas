unit uAiGraphDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Threading, System.JSON, Rest.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, uAiPrompts, uAiOpenChat, uUtils.System, uAGraph;

type
  TForm70 = class(TForm)
    Link2: TAIGraphLink;
    StartNode: TAIGraphNode;
    AIGraph1: TAIGraph;
    AiNode: TAIGraphNode;
    ExcecuteNode: TAIGraphNode;
    EndNode: TAIGraphNode;
    EvalNode: TAIGraphNode;
    Link1: TAIGraphLink;
    Link3: TAIGraphLink;
    Link4: TAIGraphLink;
    Memo1: TMemo;
    Button1: TButton;
    AiPrompts1: TAiPrompts;
    OpenChat: TAiOpenChat;
    MemoPrompt: TMemo;
    Memo2: TMemo;
    ChatEval: TAiOpenChat;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure AIChain1Print(Sender: TObject; Value: string);
    procedure Button1Click(Sender: TObject);
    procedure EndNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
    procedure AiNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
    procedure Link4Execute(Node: TAIGraphNode; Link: TAIGraphLink; var IsOk: Boolean);
    procedure AIChain1End(Sender: TObject; Value: string);
    procedure ExcecuteNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
    procedure StartNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
    procedure EvalNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form70: TForm70;

implementation

{$R *.fmx}

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

      If Sender is TAIGraphNode then
      Begin
        Memo2.Lines.Add(TAIGraphNode(Sender).Name + '--> ' + Value);
      End;

      If Sender is TAIGraphLink then
      Begin
        Memo2.Lines.Add(TAIGraphLink(Sender).Name + '--> ' + Value);
      End;
    end);
end;

procedure TForm70.AiNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
Var
  Prompt: String;
begin
  Node.Print(Node.Name + '--> Generando código');
  Prompt := AIGraph1.AiPrompts.GetTemplate('AiPrompt', ['Prompt=' + Input]);
  Output := OpenChat.AddMessageAndRun(Prompt, 'user');
end;

procedure TForm70.Button1Click(Sender: TObject);
begin
  OpenChat.Messages.Clear;
  ChatEval.Messages.Clear;
  AIGraph1.Run(MemoPrompt.Lines.Text);
end;

procedure TForm70.EndNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
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
            TUtilsSystem.ExcecuteCommandLine(FileName);
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

procedure TForm70.EvalNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> Evaluando....');
  // Realmente aquí no se hace nada

end;

procedure TForm70.ExcecuteNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
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

procedure TForm70.Link4Execute(Node: TAIGraphNode; Link: TAIGraphLink; var IsOk: Boolean);
Var
  Prompt, Res: String;
  JObj: TJSonObject;
  FileName, Codigo, resultado_compilacion: String;
begin

  JObj := TJSonObject(TJSonObject.ParseJSONValue(Node.Output));
  Try
    If JObj.TryGetValue<String>('filename', FileName) and JObj.TryGetValue<String>('resultado_compilacion', resultado_compilacion) then
    Begin

      Prompt := AIGraph1.AiPrompts.GetTemplate('Eval', ['resultado=' + resultado_compilacion]);

      ChatEval.Messages.Clear; // Limpia la memoria, consume menos tokens
      Res := ChatEval.AddMessageAndRun(Prompt, 'user');

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

procedure TForm70.StartNodeExecute(Node, BeforeNode: TAIGraphNode; Link: TAIGraphLink; Input: string; var Output: string);
begin
  Node.Print(Node.Name + '--> Inicicando proceso');
end;

end.
