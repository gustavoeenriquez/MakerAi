unit uConversacionForm;

// Capitulo 4: la conversacion por dentro, desde el IDE.
// El conector esta en el .fmx con Asynchronous=True y los tres eventos (OnReceiveData,
// OnReceiveDataEnd, OnError) asignados en la pestana Events del Object Inspector.
// Los eventos llegan desde el hilo de la peticion HTTP: la ventana se toca con TThread.Queue.
//
// Prueba automatica (para el libro, sin tocar el teclado):
//   ConversacionForm.exe --prueba "pregunta" ["otra" ...] [--registro tiempos.txt] [--captura salida.png]
//   Una "pregunta" que sea /nueva, /guardar o /abrir pulsa el boton correspondiente.

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON,
  System.IOUtils, System.Diagnostics, System.Net.HttpClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.Controls.Presentation,
  uMakerAi.Chat.Messages, uMakerAi.Chat.AiConnection, uMakerAi.Chat.Initializations;

type
  TFormConversacion = class(TForm)
    AiConn: TAiChatConnection;
    LayoutRaiz: TLayout;
    LayoutArriba: TLayout;
    btnNueva: TButton;
    btnGuardar: TButton;
    btnAbrir: TButton;
    lblEstado: TLabel;
    MemoChat: TMemo;
    LayoutAbajo: TLayout;
    EditPregunta: TEdit;
    btnEnviar: TButton;
    procedure btnEnviarClick(Sender: TObject);
    procedure btnNuevaClick(Sender: TObject);
    procedure btnGuardarClick(Sender: TObject);
    procedure btnAbrirClick(Sender: TObject);
    procedure EditPreguntaKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception;
      const AResponse: IHTTPResponse);
  private
    FOcupado: Boolean;
    FReloj: TStopwatch;
    FMsPrimerPedazo: Int64;
    FTokensAntes: Integer;
    // prueba automatica
    FPrueba: TStringList;
    FCaptura, FRegistro: string;
    FTimerPrueba: TTimer;
    FEspera: Cardinal;
    function ArchivoHistorial: string;
    procedure SetOcupado(AValue: Boolean);
    procedure Estado(const ATexto: string);
    procedure Registrar(const ALinea: string);
    procedure PasoPrueba(Sender: TObject);
  end;

var
  FormConversacion: TFormConversacion;

implementation

{$R *.fmx}

{ Enviar y recibir }

procedure TFormConversacion.btnEnviarClick(Sender: TObject);
var
  Pregunta: string;
begin
  Pregunta := EditPregunta.Text.Trim;
  if (Pregunta = '') or FOcupado then
    Exit;
  MemoChat.Lines.Add('Tú: ' + Pregunta);
  MemoChat.Lines.Add('Lucía: ');          // los pedazos se van sumando a esta linea
  EditPregunta.Text := '';
  SetOcupado(True);
  FMsPrimerPedazo := 0;
  FTokensAntes := AiConn.Total_tokens;
  FReloj := TStopwatch.StartNew;
  AiConn.AddMessageAndRun(Pregunta, 'user', []);   // asincrono: vuelve enseguida
end;

procedure TFormConversacion.AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
begin
  // Llega desde el hilo HTTP, pedazo a pedazo. La ventana solo se toca en el hilo principal.
  if aText = '' then
    Exit;
  if FMsPrimerPedazo = 0 then
    FMsPrimerPedazo := FReloj.ElapsedMilliseconds;
  TThread.Queue(nil,
    procedure
    var
      Ultima: Integer;
    begin
      Ultima := MemoChat.Lines.Count - 1;
      MemoChat.Lines[Ultima] := MemoChat.Lines[Ultima] + aText;
      MemoChat.GoToTextEnd;
    end);
end;

procedure TFormConversacion.AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
var
  Ms: Int64;
begin
  Ms := FReloj.ElapsedMilliseconds;
  TThread.Queue(nil,
    procedure
    var
      Entrada, Salida: Integer;
    begin
      MemoChat.Lines.Add('');
      Entrada := AiConn.Prompt_tokens;
      Salida := AiConn.Completion_tokens;
      Estado(Format('Primer pedazo a los %d ms, completa a los %d ms · historial: %d mensajes · tokens acumulados: %d entrada, %d salida',
        [FMsPrimerPedazo, Ms, AiConn.Messages.Count, Entrada, Salida]));
      Registrar(Format('turno'#9'%d'#9'%d'#9'%d'#9'%d'#9'%d'#9'%d',
        [FMsPrimerPedazo, Ms, AiConn.Messages.Count, Entrada, Salida, AiConn.Total_tokens - FTokensAntes]));
      SetOcupado(False);
    end);
end;

procedure TFormConversacion.AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception;
  const AResponse: IHTTPResponse);
begin
  TThread.Queue(nil,
    procedure
    begin
      MemoChat.Lines.Add('[error] ' + ErrorMsg);
      SetOcupado(False);
    end);
end;

{ Nueva, Guardar, Abrir }

function TFormConversacion.ArchivoHistorial: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'historial_ladera.json');
end;

procedure TFormConversacion.btnNuevaClick(Sender: TObject);
begin
  AiConn.NewChat;                       // borra los mensajes; el SystemPrompt se queda
  MemoChat.Lines.Clear;
  Estado(Format('Conversación nueva: %d mensajes en el historial.', [AiConn.Messages.Count]));
end;

procedure TFormConversacion.btnGuardarClick(Sender: TObject);
begin
  AiConn.Messages.SaveToFile(ArchivoHistorial);
  Estado(Format('Historial guardado (%d mensajes) en %s', [AiConn.Messages.Count, ArchivoHistorial]));
end;

procedure TFormConversacion.btnAbrirClick(Sender: TObject);
var
  I: Integer;
  M: TAiChatMessage;
begin
  if not FileExists(ArchivoHistorial) then
  begin
    Estado('No hay historial guardado todavía.');
    Exit;
  end;
  AiConn.NewChat;
  AiConn.Messages.LoadFromFile(ArchivoHistorial);
  // Volver a pintar la conversacion desde el historial (sin el mensaje de sistema)
  MemoChat.Lines.Clear;
  for I := 0 to AiConn.Messages.Count - 1 do
  begin
    M := AiConn.Messages[I];
    if M.Role = 'user' then
      MemoChat.Lines.Add('Tú: ' + M.Prompt)
    else if M.Role = 'assistant' then
    begin
      MemoChat.Lines.Add('Lucía: ' + M.Prompt.Trim);
      MemoChat.Lines.Add('');
    end;
  end;
  Estado(Format('Historial recuperado: %d mensajes.', [AiConn.Messages.Count]));
end;

{ Utilidades }

procedure TFormConversacion.SetOcupado(AValue: Boolean);
begin
  FOcupado := AValue;
  btnEnviar.Enabled := not AValue;
  btnNueva.Enabled := not AValue;
  btnAbrir.Enabled := not AValue;
end;

procedure TFormConversacion.Estado(const ATexto: string);
begin
  lblEstado.Text := ATexto;
end;

procedure TFormConversacion.Registrar(const ALinea: string);
begin
  if FRegistro <> '' then
    TFile.AppendAllText(FRegistro, ALinea + sLineBreak, TEncoding.UTF8);
end;

procedure TFormConversacion.EditPreguntaKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkReturn then
    btnEnviarClick(nil);
end;

procedure TFormConversacion.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Estado('Chat: ' + AiConn.DriverName + ' / ' + AiConn.AiChat.Model);
  I := 1;
  while I <= ParamCount do
  begin
    if SameText(ParamStr(I), '--captura') and (I < ParamCount) then
    begin
      FCaptura := ParamStr(I + 1);
      Inc(I);
    end
    else if SameText(ParamStr(I), '--registro') and (I < ParamCount) then
    begin
      FRegistro := ParamStr(I + 1);
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

procedure TFormConversacion.FormDestroy(Sender: TObject);
begin
  FPrueba.Free;
end;

procedure TFormConversacion.PasoPrueba(Sender: TObject);
var
  Linea: string;
begin
  if FOcupado or (TThread.GetTickCount < FEspera) then
    Exit;
  if FPrueba.Count > 0 then
  begin
    Linea := FPrueba[0];
    FPrueba.Delete(0);
    if SameText(Linea, '/nueva') then
      btnNuevaClick(nil)
    else if SameText(Linea, '/guardar') then
      btnGuardarClick(nil)
    else if SameText(Linea, '/abrir') then
      btnAbrirClick(nil)
    else
    begin
      EditPregunta.Text := Linea;
      btnEnviarClick(nil);
    end;
    Registrar('accion'#9 + Linea);
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
