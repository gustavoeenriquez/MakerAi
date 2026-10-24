unit uPrimerChatForm;

// Capitulo 3: el primer chat con Lucia, desde el IDE.
// DriverName, Model, Params y SystemPrompt del conector estan en el .fmx, puestos
// desde el Object Inspector. El codigo solo envia la pregunta y muestra la respuesta.
//
// Prueba automatica (para el libro, sin tocar el teclado):
//   PrimerChatForm.exe --prueba "pregunta" ["otra pregunta" ...] --captura salida.png

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.Controls.Presentation,
  uMakerAi.Chat.AiConnection, uMakerAi.Chat.Initializations;

type
  TFormPrimerChat = class(TForm)
    AiConn: TAiChatConnection;
    LayoutRaiz: TLayout;
    MemoChat: TMemo;
    LayoutAbajo: TLayout;
    EditPregunta: TEdit;
    btnEnviar: TButton;
    procedure btnEnviarClick(Sender: TObject);
    procedure EditPreguntaKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPrueba: TStringList;
    FCaptura: string;
    FTimerPrueba: TTimer;
    FEspera: Cardinal;
    procedure PasoPrueba(Sender: TObject);
  end;

var
  FormPrimerChat: TFormPrimerChat;

implementation

{$R *.fmx}

procedure TFormPrimerChat.btnEnviarClick(Sender: TObject);
var
  Pregunta, Respuesta: string;
begin
  Pregunta := EditPregunta.Text.Trim;
  if Pregunta = '' then
    Exit;
  MemoChat.Lines.Add('Tú: ' + Pregunta);
  EditPregunta.Text := '';
  // Sincrono: la ventana espera hasta que llega la respuesta completa
  Respuesta := AiConn.AddMessageAndRun(Pregunta, 'user', []);
  if Respuesta = '' then
    Respuesta := '[error] ' + AiConn.LastError;
  MemoChat.Lines.Add('Lucía: ' + Respuesta.Trim);
  MemoChat.Lines.Add('');
end;

procedure TFormPrimerChat.EditPreguntaKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkReturn then
    btnEnviarClick(nil);
end;

procedure TFormPrimerChat.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // --prueba: envia las preguntas de la linea de comandos, guarda la captura y cierra
  I := 1;
  while I <= ParamCount do
  begin
    if SameText(ParamStr(I), '--captura') and (I < ParamCount) then
    begin
      FCaptura := ParamStr(I + 1);
      Inc(I);
    end
    else if not SameText(ParamStr(I), '--prueba') then
    begin
      if not Assigned(FPrueba) then
        FPrueba := TStringList.Create;
      FPrueba.Add(ParamStr(I));
    end;
    Inc(I);
  end;
  if Assigned(FPrueba) then
  begin
    FTimerPrueba := TTimer.Create(Self);
    FTimerPrueba.Interval := 100;
    FEspera := TThread.GetTickCount + 800;
    FTimerPrueba.OnTimer := PasoPrueba;
  end;
end;

procedure TFormPrimerChat.FormDestroy(Sender: TObject);
begin
  FPrueba.Free;
end;

procedure TFormPrimerChat.PasoPrueba(Sender: TObject);
begin
  if TThread.GetTickCount < FEspera then
    Exit;
  if FPrueba.Count > 0 then
  begin
    EditPregunta.Text := FPrueba[0];
    FPrueba.Delete(0);
    btnEnviarClick(nil);
    FEspera := TThread.GetTickCount + 600;
    Exit;
  end;
  FTimerPrueba.Enabled := False;
  if FCaptura <> '' then
    with LayoutRaiz.MakeScreenshot do
    try
      SaveToFile(FCaptura);
    finally
      Free;
    end;
  Close;
end;

end.
