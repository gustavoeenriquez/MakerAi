unit uLaderaApp;

// =============================================================================
// LIBRO "IA con Delphi" - La app de Cafe La Ladera, etapa del capitulo 8
// =============================================================================
// Lucia con ventana: TAIChatView (burbujas con Markdown, streaming token a token,
// paso "Pensando", pie con tokens y tiempo) y TAIChatInput (la barra de entrada).
// El conector TAiChatConnection esta en el formulario, configurado en el Object
// Inspector, con sus eventos conectados desde la pestana Events.
// Ademas, el filtro: SanitizerActive y ChatTools.PromptGuard (TAiJevPromptGuard, tambien en el
// formulario) revisan cada mensaje ANTES de que llegue al modelo; un bloqueo llega por OnError.
//
// Prueba automatica (para el libro, sin tocar el teclado):
//   LaderaApp.exe --prueba "pregunta" ["otra" ...] [--captura salida.png] [--registro tiempos.txt]
//   /nueva, /guardar y /abrir pulsan los botones.
// Empresa, productos y precios ficticios.
// =============================================================================

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.IOUtils,
  System.JSON, System.Diagnostics, System.Net.HttpClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation,
  // Primero la UI: AIChat.Types tambien declara un TAIChatMessage y Delphi no
  // distingue mayusculas; el de uMakerAi.Chat.Messages, que va despues, gana.
  AIChat.Types,
  AIChat.Control.FMX,
  AIChat.Input,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Sanitizer,
  uMakerAi.Chat.Tools,
  uMakerAi.Jev.PromptGuard,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;   // registra todos los drivers

type
  TFormLadera = class(TForm)
    AiConn: TAiChatConnection;
    LayoutTop: TLayout;
    ComboDriver: TComboBox;
    btnNueva: TButton;
    btnGuardar: TButton;
    btnAbrir: TButton;
    lblEstado: TLabel;
    ChatView1: TAIChatView;
    ChatInput1: TAIChatInput;
    JevGuard: TAiJevPromptGuard;
    procedure AiConnSanitize(Sender: TObject; const AResult: TSanitizeResult; var AAction: TAiSanitizeAction);
    procedure AiConnPromptGuard(Sender: TObject; const AVerdict: TAiPromptVerdict; var AAction: TAiSanitizeAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles;
      aAudioStream: TMemoryStream);
    procedure ChatInput1Cancel(Sender: TObject);
    procedure ComboDriverChange(Sender: TObject);
    procedure btnNuevaClick(Sender: TObject);
    procedure btnGuardarClick(Sender: TObject);
    procedure btnAbrirClick(Sender: TObject);
    procedure AiConnReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception;
      const AResponse: IHTTPResponse);
  private
    FBurbuja: Integer;        // burbuja del asistente que se esta escribiendo
    FPensando: Boolean;
    FReloj: TStopwatch;
    FCerrando: Boolean;
    FBloqueo: string;         // motivo del ultimo bloqueo del filtro (capitulo 8)
    procedure ConfigurarConexion(const ADriver: string);
    procedure TerminarPensando;
    procedure SetEstado(const AText: string);
    function CarpetaDatos: string;
    // prueba automatica
    procedure IniciarPrueba;
    procedure PasoPrueba(Sender: TObject);
    procedure Registrar(const ALinea: string);
  private
    FPrueba: TStringList;
    FCaptura, FRegistro: string;
    FTimerPrueba: TTimer;
    FFinTurno: Cardinal;
  end;

var
  FormLadera: TFormLadera;

implementation

{$R *.fmx}

function ApiKeyFor(const ADriver: string): string;
begin
  if MatchText(ADriver, ['Ollama', 'LMStudio', 'GenericLLM']) then
    Result := ''
  else if SameText(ADriver, 'OpenAi') then
    Result := '@OPENAI_API_KEY'
  else
    Result := '@' + UpperCase(ADriver) + '_API_KEY';
end;

{ Arranque }

procedure TFormLadera.FormCreate(Sender: TObject);
begin
  FBurbuja := -1;
  ComboDriver.Items.CommaText := 'OpenAi,Claude,Groq,DeepSeek,Mistral,Ollama';
  ComboDriver.ItemIndex := 0;   // dispara ComboDriverChange -> ConfigurarConexion
  ChatView1.AddMessage(TChatRole.crAssistant,
    '¡Hola! Soy **Lucía**, de Café La Ladera ☕' + sLineBreak + sLineBreak +
    'Pregúntame por nuestros cafés.');
  IniciarPrueba;
end;

procedure TFormLadera.FormDestroy(Sender: TObject);
begin
  FPrueba.Free;
end;

procedure TFormLadera.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Cerrar a mitad de una respuesta: se desconectan los eventos, se aborta la
  // peticion y se atiende lo que ya estaba encolado.
  FCerrando := True;
  AiConn.OnReceiveThinking := nil;
  AiConn.OnReceiveData := nil;
  AiConn.OnReceiveDataEnd := nil;
  AiConn.OnError := nil;
  AiConn.Abort;
  CheckSynchronize;
  CanClose := True;
end;

procedure TFormLadera.ConfigurarConexion(const ADriver: string);
var
  Key: string;
begin
  // El prompt, Asynchronous=True y los eventos ya estan en el .fmx. Cambiar
  // DriverName recrea el chat interno: el historial empieza de cero.
  AiConn.DriverName := ADriver;
  Key := ApiKeyFor(ADriver);
  if Key <> '' then
    AiConn.Params.Values['ApiKey'] := Key;
  if Key.StartsWith('@') and (GetEnvironmentVariable(Key.Substring(1)) = '') then
    SetEstado('Falta la variable ' + Key.Substring(1))
  else
    SetEstado(Format('%s / %s', [AiConn.DriverName, AiConn.AiChat.Model]));
end;

procedure TFormLadera.ComboDriverChange(Sender: TObject);
begin
  if ComboDriver.ItemIndex < 0 then
    Exit;
  ConfigurarConexion(ComboDriver.Items[ComboDriver.ItemIndex]);
  ChatView1.ClearMessages;
end;

{ Un turno: enviar, recibir pedazo a pedazo, terminar }

procedure TFormLadera.ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles;
  aAudioStream: TMemoryStream);
begin
  // Los adjuntos y la voz llegan en el capitulo 9; aqui solo texto.
  ChatView1.AddUserMessage(APrompt, nil);
  ChatView1.BeginAssistantMessage;              // burbuja vacia que se ira llenando
  FBurbuja := ChatView1.MessageCount - 1;
  FPensando := False;
  FBloqueo := '';
  FReloj := TStopwatch.StartNew;
  SetEstado('Lucía está escribiendo...');
  AiConn.AddMessageAndRun(APrompt, 'user', []);  // asincrono: vuelve enseguida
end;

procedure TFormLadera.AiConnReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
begin
  // El razonamiento no se muestra: solo un paso "Pensando" mientras dura
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando then
        Exit;
      if (FBurbuja >= 0) and not FPensando then
      begin
        FPensando := True;
        ChatView1.AddStep(FBurbuja, 'pensar', 'Pensando');
      end;
    end);
end;

procedure TFormLadera.TerminarPensando;
begin
  if FPensando then
  begin
    FPensando := False;
    ChatView1.FinishStep(FBurbuja, 'pensar', True, FReloj.ElapsedMilliseconds, '');
  end;
end;

procedure TFormLadera.AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
begin
  // Llega desde el hilo HTTP: la UI se toca en el principal
  if aText = '' then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando or (FBurbuja < 0) then
        Exit;
      TerminarPensando;
      ChatView1.AppendToken(FBurbuja, aText);   // la burbuja crece y el Markdown se pinta
    end);
end;

procedure TFormLadera.AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
var
  Pie: string;
  Entrada: Integer;
begin
  if Assigned(aMsg) then
  begin
    // Prompt_tokens son los tokens nuevos; los del cache van aparte. La entrada es la suma.
    Entrada := aMsg.Prompt_tokens + aMsg.Cached_tokens + aMsg.Cache_write_tokens;
    Pie := Format('%d tokens de entrada', [Entrada]);
    if aMsg.Cached_tokens > 0 then
      Pie := Pie + Format(' (%d en caché)', [aMsg.Cached_tokens]);
    Pie := Pie + Format(' · %d de salida · %.1f s', [aMsg.Completion_tokens, FReloj.ElapsedMilliseconds / 1000]);
  end
  else
    Pie := Format('%.1f s', [FReloj.ElapsedMilliseconds / 1000]);
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando or (FBurbuja < 0) then
        Exit;
      TerminarPensando;
      ChatView1.FinishMessage(FBurbuja);
      ChatView1.SetMessageFooter(FBurbuja, Pie);
      Registrar('turno'#9 + Pie);
      FBurbuja := -1;
      ChatInput1.Busy := False;
      SetEstado(Format('%s / %s', [AiConn.DriverName, AiConn.AiChat.Model]));
    end);
end;

procedure TFormLadera.AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception;
  const AResponse: IHTTPResponse);
var
  Texto: string;
begin
  Texto := IfThen(FBloqueo <> '', '🛡️ ' + FBloqueo, '⚠️ ' + ErrorMsg);
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando then
        Exit;
      if FBurbuja >= 0 then
      begin
        TerminarPensando;
        ChatView1.SetMessageText(FBurbuja, Texto);
        ChatView1.FinishMessage(FBurbuja);
        FBurbuja := -1;
      end;
      Registrar('error'#9 + Texto);
      ChatInput1.Busy := False;
      SetEstado(IfThen(FBloqueo <> '', 'Mensaje bloqueado', 'Error: ' + ErrorMsg));
    end);
end;

procedure TFormLadera.ChatInput1Cancel(Sender: TObject);
begin
  AiConn.Abort;
  if FBurbuja >= 0 then
  begin
    TerminarPensando;
    ChatView1.SetMessageFooter(FBurbuja, 'cancelado');
    ChatView1.FinishMessage(FBurbuja);
    FBurbuja := -1;
  end;
  ChatInput1.Busy := False;
  SetEstado('Cancelado');
end;

{ El filtro: dos capas antes del modelo }

procedure TFormLadera.AiConnSanitize(Sender: TObject; const AResult: TSanitizeResult; var AAction: TAiSanitizeAction);
var
  M: TSuspiciousMatch;
  Nombres: string;
begin
  // Primera malla: el sanitizador por regex encontro una formula conocida de inyeccion.
  // AAction llega en saBlock; aqui se deja asi y se anota el motivo para la burbuja.
  for M in AResult.SuspiciousMatches do
    Nombres := Nombres + M.PatternName + ' ';
  FBloqueo := 'Mensaje bloqueado por el sanitizador: fórmula conocida (' + Nombres.Trim + ').';
end;

procedure TFormLadera.AiConnPromptGuard(Sender: TObject; const AVerdict: TAiPromptVerdict; var AAction: TAiSanitizeAction);
begin
  // Segunda malla: Jev juzgo la intencion. Categoria y probabilidad del veredicto.
  FBloqueo := Format('Mensaje bloqueado por el guardián: %s (%.2f).', [AVerdict.Category, AVerdict.Score]);
end;

{ Nueva, Guardar, Abrir }

procedure TFormLadera.btnNuevaClick(Sender: TObject);
begin
  AiConn.NewChat;
  ChatView1.ClearMessages;
  SetEstado('Conversación nueva');
end;

function TFormLadera.CarpetaDatos: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'CafeLaLadera');
  ForceDirectories(Result);
end;

procedure TFormLadera.btnGuardarClick(Sender: TObject);
var
  St: TFileStream;
begin
  // Dos cosas distintas: lo que se ve (ChatView) y lo que el modelo recuerda (Messages)
  St := TFileStream.Create(TPath.Combine(CarpetaDatos, 'pantalla.json'), fmCreate);
  try
    ChatView1.SaveToStream(St);
  finally
    St.Free;
  end;
  AiConn.Messages.SaveToFile(TPath.Combine(CarpetaDatos, 'historial.json'));
  SetEstado('Guardado en ' + CarpetaDatos);
end;

procedure TFormLadera.btnAbrirClick(Sender: TObject);
var
  St: TFileStream;
begin
  if not FileExists(TPath.Combine(CarpetaDatos, 'historial.json')) then
  begin
    SetEstado('No hay conversación guardada');
    Exit;
  end;
  St := TFileStream.Create(TPath.Combine(CarpetaDatos, 'pantalla.json'), fmOpenRead);
  try
    ChatView1.ClearMessages;
    ChatView1.LoadFromStream(St);
  finally
    St.Free;
  end;
  AiConn.NewChat;
  AiConn.Messages.LoadFromFile(TPath.Combine(CarpetaDatos, 'historial.json'));
  SetEstado(Format('Conversación recuperada: %d mensajes', [AiConn.Messages.Count]));
end;

procedure TFormLadera.SetEstado(const AText: string);
begin
  lblEstado.Text := AText;
end;

{ Prueba automatica }

procedure TFormLadera.Registrar(const ALinea: string);
begin
  if FRegistro <> '' then
    TFile.AppendAllText(FRegistro, ALinea + sLineBreak, TEncoding.UTF8);
end;

procedure TFormLadera.IniciarPrueba;
var
  I: Integer;
begin
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
  if not Assigned(FPrueba) then
    Exit;
  FTimerPrueba := TTimer.Create(Self);
  FTimerPrueba.Interval := 100;
  FFinTurno := TThread.GetTickCount + 1500;
  FTimerPrueba.OnTimer := PasoPrueba;
end;

procedure TFormLadera.PasoPrueba(Sender: TObject);
var
  Linea: string;
begin
  // Envia cada pregunta como si la escribiera el usuario, una tras otra
  if ChatInput1.Busy or (TThread.GetTickCount < FFinTurno) then
    Exit;
  if FPrueba.Count = 0 then
  begin
    FTimerPrueba.Enabled := False;
    if FCaptura <> '' then
      with ChatView1.MakeScreenshot do
      try
        SaveToFile(FCaptura);
      finally
        Free;
      end;
    Close;
    Exit;
  end;
  Linea := FPrueba[0];
  FPrueba.Delete(0);
  Registrar('accion'#9 + Linea);
  if SameText(Linea, '/nueva') then
    btnNuevaClick(nil)
  else if SameText(Linea, '/guardar') then
    btnGuardarClick(nil)
  else if SameText(Linea, '/abrir') then
    btnAbrirClick(nil)
  else
  begin
    ChatInput1.PromptText := '';
    ChatInput1.Busy := True;
    ChatInput1SendEvent(ChatInput1, Linea, nil, nil);
  end;
  FFinTurno := TThread.GetTickCount + 700;
end;

end.
