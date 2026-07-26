unit uMainMinimalChat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uMakerAi.Chat, uMakerAi.Chat.Ollama;

type

  { TFrmMinimalChat }

  TFrmMinimalChat = class(TForm)
    BtnSend: TButton;
    EdModel: TEdit;
    EdUrl: TEdit;
    LblModel: TLabel;
    LblStatus: TLabel;
    LblUrl: TLabel;
    MemoPrompt: TMemo;
    MemoResponse: TMemo;
    PnlTop: TPanel;
    procedure BtnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FChat: TAiOllamaChat;
    procedure SetBusy(ABusy: Boolean; const AMsg: string);
  end;

var
  FrmMinimalChat: TFrmMinimalChat;

implementation

{$R *.lfm}

{ TFrmMinimalChat }

procedure TFrmMinimalChat.FormCreate(Sender: TObject);
begin
  // El componente se crea una sola vez: TAiChat mantiene el historial de la
  // conversacion, asi que reutilizarlo permite varios turnos seguidos.
  FChat := TAiOllamaChat.Create(Self);
  FChat.Asynchronous := False;   // llamada sincrona: simple para un demo
  FChat.Max_tokens   := 1024;
  FChat.Temperature  := 0.7;

  MemoPrompt.Text := 'Hola, ¿como estas hoy?';
  LblStatus.Caption := 'Listo. Requiere Ollama corriendo en la URL indicada.';
end;

procedure TFrmMinimalChat.FormDestroy(Sender: TObject);
begin
  // FChat tiene Owner = Self, se libera solo. Nada que hacer aqui.
end;

procedure TFrmMinimalChat.SetBusy(ABusy: Boolean; const AMsg: string);
begin
  BtnSend.Enabled   := not ABusy;
  MemoPrompt.Enabled := not ABusy;
  LblStatus.Caption := AMsg;
  if ABusy then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

procedure TFrmMinimalChat.BtnSendClick(Sender: TObject);
var
  LPrompt, LRes: string;
begin
  LPrompt := Trim(MemoPrompt.Text);
  if LPrompt = '' then
  begin
    ShowMessage('Escribe una pregunta.');
    Exit;
  end;

  FChat.Url   := Trim(EdUrl.Text);
  FChat.Model := Trim(EdModel.Text);

  SetBusy(True, 'Consultando a ' + FChat.Model + ' ...');
  try
    try
      LRes := FChat.AddMessageAndRun(LPrompt, 'user', nil);

      MemoResponse.Lines.Add('> ' + LPrompt);
      MemoResponse.Lines.Add(LRes);
      MemoResponse.Lines.Add('');
      MemoPrompt.Clear;

      LblStatus.Caption := Format(
        'OK — tokens: prompt %d / respuesta %d / total %d',
        [FChat.Prompt_tokens, FChat.Completion_tokens, FChat.Total_tokens]);
    except
      on E: Exception do
      begin
        LblStatus.Caption := 'Error: ' + E.Message;
        MemoResponse.Lines.Add('[ERROR] ' + E.Message);
      end;
    end;
  finally
    SetBusy(False, LblStatus.Caption);
    MemoPrompt.SetFocus;
  end;
end;

end.
