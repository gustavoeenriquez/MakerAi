unit uMainLlamacppChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.Net.HttpClient, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Memo,
  FMX.Memo.Types, FMX.ScrollBox, FMX.ListBox,
  uMakerAi.Chat, uMakerAi.Chat.Messages, uMakerAi.Chat.Llamacpp,
  uMakerAi.Chat.Initializations, uMakerAi.Core;

type
  TfrmLlamacppChat = class(TForm)
    AiLlamacpp: TAiLlamacppChat;
    PnlTop: TPanel;
    LblDLL: TLabel;
    EdtDLL: TEdit;
    BtnDLL: TButton;
    LblModelo: TLabel;
    EdtModelo: TEdit;
    BtnModelo: TButton;
    LblSistema: TLabel;
    EdtSistema: TEdit;
    LblStyle: TLabel;
    CbxStyle: TComboBox;
    LblGPU: TLabel;
    EdtGPU: TEdit;
    LblCtx: TLabel;
    EdtCtx: TEdit;
    LblTok: TLabel;
    EdtTok: TEdit;
    LblTemp: TLabel;
    EdtTemp: TEdit;
    BtnNuevo: TButton;
    MemoChat: TMemo;
    PnlBottom: TPanel;
    LblStatus: TLabel;
    EdtInput: TEdit;
    BtnEnviar: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnDLLClick(Sender: TObject);
    procedure BtnModeloClick(Sender: TObject);
    procedure BtnNuevoClick(Sender: TObject);
    procedure BtnEnviarClick(Sender: TObject);
    procedure EdtInputKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure AiLlamacppReceiveData(const Sender: TObject;
      aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure AiLlamacppReceiveDataEnd(const Sender: TObject;
      aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure AiLlamacppError(Sender: TObject; const ErrorMsg: string;
      AException: Exception; const AResponse: IHTTPResponse);
    procedure CbxStyleChange(Sender: TObject);
  private
    FGenerando:           Boolean;
    FLineInicioRespuesta: Integer;
    FRespuestaActual:     string;
    procedure AppendMensaje(const ARol, ATexto: string);
    procedure FinalizarGeneracion;
    procedure SetControlesActivos(AValue: Boolean);
  end;

var
  frmLlamacppChat: TfrmLlamacppChat;

implementation

{$R *.fmx}

{ TfrmLlamacppChat }

procedure TfrmLlamacppChat.FormCreate(Sender: TObject);
begin
  FGenerando           := False;
  FLineInicioRespuesta := -1;

  CbxStyle.Items.Add('Llama 3');
  CbxStyle.Items.Add('ChatML');
  CbxStyle.Items.Add('Gemma');
  CbxStyle.Items.Add('Alpaca');
  CbxStyle.Items.Add('Custom');

  // Sincronizar edits con propiedades del componente
  EdtDLL.Text    := AiLlamacpp.DLLPath;
  EdtModelo.Text := AiLlamacpp.ModelPath;
  EdtGPU.Text    := IntToStr(AiLlamacpp.NGPULayers);
  EdtCtx.Text    := IntToStr(AiLlamacpp.NCtx);
  EdtTok.Text    := IntToStr(AiLlamacpp.Max_tokens);
  EdtTemp.Text   := FormatFloat('0.##', AiLlamacpp.Temperature);
  CbxStyle.ItemIndex := Ord(AiLlamacpp.PromptStyle);

  // Asignar eventos
  AiLlamacpp.OnReceiveData    := AiLlamacppReceiveData;
  AiLlamacpp.OnReceiveDataEnd := AiLlamacppReceiveDataEnd;
  AiLlamacpp.OnError          := AiLlamacppError;
end;

// --- Helpers ---

procedure TfrmLlamacppChat.AppendMensaje(const ARol, ATexto: string);
begin
  if MemoChat.Lines.Count > 0 then
    MemoChat.Lines.Add('');
  MemoChat.Lines.Add('[' + ARol + ']: ' + ATexto);
  MemoChat.GoToTextEnd;
end;

procedure TfrmLlamacppChat.SetControlesActivos(AValue: Boolean);
begin
  BtnEnviar.Enabled := AValue;
  BtnNuevo.Enabled  := AValue;
  EdtInput.Enabled  := AValue;
  BtnDLL.Enabled    := AValue;
  BtnModelo.Enabled := AValue;
end;

procedure TfrmLlamacppChat.FinalizarGeneracion;
begin
  if not FGenerando then Exit;
  FGenerando           := False;
  FLineInicioRespuesta := -1;
  SetControlesActivos(True);
  LblStatus.Text := 'Listo';
  EdtInput.SetFocus;
end;

// --- Eventos del componente AI ---

procedure TfrmLlamacppChat.AiLlamacppReceiveData(const Sender: TObject;
  aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  TThread.Queue(nil, procedure
  begin
    if (FLineInicioRespuesta >= 0) and
       (FLineInicioRespuesta < MemoChat.Lines.Count) then
    begin
      FRespuestaActual := FRespuestaActual + aText;
      MemoChat.Lines[FLineInicioRespuesta] := '[IA]: ' + FRespuestaActual;
      MemoChat.GoToTextEnd;
    end;
  end);
end;

procedure TfrmLlamacppChat.AiLlamacppReceiveDataEnd(const Sender: TObject;
  aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  TThread.Queue(nil, procedure
  begin
    FinalizarGeneracion;
  end);
end;

procedure TfrmLlamacppChat.AiLlamacppError(Sender: TObject;
  const ErrorMsg: string; AException: Exception;
  const AResponse: IHTTPResponse);
begin
  TThread.Queue(nil, procedure
  begin
    if (FLineInicioRespuesta >= 0) and
       (FLineInicioRespuesta < MemoChat.Lines.Count) then
      MemoChat.Lines[FLineInicioRespuesta] := '[IA]: ERROR - ' + ErrorMsg;
    LblStatus.Text := 'Error: ' + ErrorMsg;
    FinalizarGeneracion;
  end);
end;

// --- Configuracion ---

procedure TfrmLlamacppChat.BtnDLLClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Title  := 'Seleccionar makerai.gen.dll';
    Dlg.Filter := 'DLL (*.dll)|*.dll|Todos|*.*';
    if Dlg.Execute then
      EdtDLL.Text := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

procedure TfrmLlamacppChat.BtnModeloClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.Title  := 'Seleccionar modelo GGUF';
    Dlg.Filter := 'GGUF (*.gguf)|*.gguf|Todos|*.*';
    if Dlg.Execute then
      EdtModelo.Text := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

procedure TfrmLlamacppChat.CbxStyleChange(Sender: TObject);
begin
  AiLlamacpp.PromptStyle := TLlamacppPromptStyle(CbxStyle.ItemIndex);
end;

// --- Chat ---

procedure TfrmLlamacppChat.BtnNuevoClick(Sender: TObject);
begin
  AiLlamacpp.SystemPrompt.Text := EdtSistema.Text;
  AiLlamacpp.NewChat;
  MemoChat.Lines.Clear;
  LblStatus.Text := 'Nuevo chat iniciado.';
end;

procedure TfrmLlamacppChat.EdtInputKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) and not FGenerando then
  begin
    Key := 0;
    BtnEnviarClick(BtnEnviar);
  end;
end;

procedure TfrmLlamacppChat.BtnEnviarClick(Sender: TObject);
var
  sPrompt: string;
begin
  sPrompt := Trim(EdtInput.Text);
  if sPrompt = '' then Exit;
  if FGenerando then Exit;

  // Aplicar configuracion al componente
  AiLlamacpp.DLLPath     := Trim(EdtDLL.Text);
  AiLlamacpp.ModelPath   := Trim(EdtModelo.Text);
  AiLlamacpp.NGPULayers  := StrToIntDef(EdtGPU.Text, 99);
  AiLlamacpp.NCtx        := StrToIntDef(EdtCtx.Text, 4096);
  AiLlamacpp.Max_tokens  := StrToIntDef(EdtTok.Text, 512);
  AiLlamacpp.Temperature := StrToFloatDef(EdtTemp.Text, 0.8);
  AiLlamacpp.SystemPrompt.Text := Trim(EdtSistema.Text);

  EdtInput.Text    := '';
  FGenerando       := True;
  FRespuestaActual := '';
  SetControlesActivos(False);
  LblStatus.Text := 'Generando...';

  // Mostrar turno del usuario
  AppendMensaje('Voce', sPrompt);

  // Preparar linea de respuesta IA
  MemoChat.Lines.Add('');
  MemoChat.Lines.Add('[IA]: ');
  FLineInicioRespuesta := MemoChat.Lines.Count - 1;

  // Ejecutar en hilo de fondo
  var LPrompt := sPrompt;
  TTask.Run(procedure
  begin
    try
      AiLlamacpp.AddMessageAndRun(LPrompt, 'user', []);
    except
      on E: Exception do
      begin
        var sErr := E.Message;
        TThread.Queue(nil, procedure
        begin
          if (FLineInicioRespuesta >= 0) and
             (FLineInicioRespuesta < MemoChat.Lines.Count) then
            MemoChat.Lines[FLineInicioRespuesta] := '[IA]: ERROR - ' + sErr;
          LblStatus.Text := 'Error: ' + sErr;
          FinalizarGeneracion;
        end);
      end;
    end;
  end);
end;

end.