unit uMainChatLadera;

// =============================================================================
// DEMO 095 - Chat con interfaz: TAIChatView + TAIChatInput
// =============================================================================
// La barista virtual de Cafe La Ladera, ahora con ventana:
//
//   - TAIChatView    : burbujas con Markdown, streaming token a token
//                      (BeginAssistantMessage / AppendToken / FinishMessage),
//                      pasos (AddStep: "Pensando") y pie con tokens y tiempo.
//   - TAIChatInput   : texto, adjuntos (imagen y PDF) y microfono.
//   - TAIVoiceMonitor: graba la voz; cuando el usuario calla, TAIChatInput
//                      manda el WAV por OnSendEvent.
//   - Audio          : el modelo de chat no escucha audio, asi que la sesion
//                      pide cap_Audio y el framework lo transcribe antes
//                      (gap SessionCaps - ModelCaps) con el SpeechTool de
//                      ChatTools: gpt-transcribe.
//
// La conexion se arma en codigo (FormCreate) para que se lea completa.
// Empresa, productos y precios ficticios.
// =============================================================================

interface

uses
  System.SysUtils, System.StrUtils, System.DateUtils, System.Types, System.UITypes, System.Classes, System.IOUtils,
  System.JSON, System.Threading, System.Diagnostics, System.Net.HttpClient,
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
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,   // registra todos los drivers
  uMakerAi.OpenAI.Audio,
  uMakerAi.OpenAI.Audio.Tool,
  uMakerAi.Utils.VoiceMonitor;

type
  TFormChatLadera = class(TForm)
    LayoutTop: TLayout;
    ComboDriver: TComboBox;
    btnNueva: TButton;
    btnGuardar: TButton;
    btnAbrir: TButton;
    lblEstado: TLabel;
    ChatView1: TAIChatView;
    ChatInput1: TAIChatInput;
    AIVoiceMonitor1: TAIVoiceMonitor;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles;
      aAudioStream: TMemoryStream);
    procedure ChatInput1Cancel(Sender: TObject);
    procedure ChatInput1MicToggle(Sender: TObject; AActivate: Boolean);
    procedure ComboDriverChange(Sender: TObject);
    procedure btnNuevaClick(Sender: TObject);
    procedure btnGuardarClick(Sender: TObject);
    procedure btnAbrirClick(Sender: TObject);
  private
    FConn: TAiChatConnection;
    FSpeech: TAiOpenAiSpeechTool;
    FBurbuja: Integer;        // burbuja del asistente que se esta escribiendo
    FBurbujaUsuario: Integer; // burbuja del ultimo mensaje del usuario
    FPensando: Boolean;
    FReloj: TStopwatch;
    FTarea: ITask;            // el turno en curso (corre fuera del hilo principal)
    FCerrando: Boolean;
    procedure ConfigurarConexion(const ADriver: string);
    procedure AlPensar(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: string);
    procedure AlRecibir(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: string);
    procedure AlTerminar(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: string);
    procedure AlFallar(Sender: TObject; const ErrorMsg: string; AException: Exception; const AResponse: IHTTPResponse);
    procedure TerminarPensando;
    procedure MostrarTranscripcion;
    procedure SetEstado(const AText: string);
    function CarpetaDatos: string;
    // Prueba automatica: --prueba "pregunta" "@archivo|pregunta" ... [--captura x.png]
    // Para grabar video: [--tecleo] [--pausa ms] [--tamano 1440x1000] [--marcas archivo]
    procedure IniciarPrueba;
    procedure PasoPrueba(Sender: TObject);
  private
    FPrueba: TStringList;
    FCaptura: string;
    FTimerPrueba: TTimer;
    FTecleo: Boolean;          // escribe la pregunta letra a letra en el input
    FPausaMs: Integer;         // espera tras cada respuesta (y antes de cerrar)
    FFinTurno: Cardinal;       // cuando termino la ultima respuesta
    FEscribiendo: string;      // pregunta que se esta tecleando
    FArchivoPendiente: string; // adjunto de la pregunta que se esta tecleando
    FMarcas: string;           // archivo de marcas para el montaje del video
    FTurno: Integer;
    procedure Marcar(const ANombre: string);
  end;

var
  FormChatLadera: TFormChatLadera;

implementation

{$R *.fmx}

const
  PROMPT_SISTEMA =
    'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano.' + sLineBreak +
    'Carta (bolsa de 340 g): Huila Reserva 38.000 COP (panela, frutos rojos); ' +
    'Nariño Altura 42.000 COP (cítricos, caramelo); Sierra Nevada Orgánico 36.000 COP ' +
    '(chocolate amargo, nuez); Descafeinado Suave 34.000 COP (cacao suave).' + sLineBreak +
    'Si te mandan una foto, descríbela y relaciónala con el café. Si te mandan un PDF ' +
    '(una factura, una ficha técnica), resume lo importante. Usa Markdown: listas y negritas.' + sLineBreak +
    'Responde en español, cálido y breve. Solo hablas de café y de la tienda. ' +
    'No inventes productos ni precios fuera de la carta.';

function ApiKeyFor(const ADriver: string): string;
begin
  if SameText(ADriver, 'Ollama') or SameText(ADriver, 'LMStudio') or SameText(ADriver, 'GenericLLM') then
    Result := ''
  else if SameText(ADriver, 'OpenAi') then
    Result := '@OPENAI_API_KEY'
  else
    Result := '@' + UpperCase(ADriver) + '_API_KEY';
end;

{ TFormChatLadera }

procedure TFormChatLadera.FormCreate(Sender: TObject);
begin
  FBurbuja := -1;
  FBurbujaUsuario := -1;

  // Transcripcion de las notas de voz: gpt-transcribe de OpenAI
  FSpeech := TAiOpenAiSpeechTool.Create(Self);
  FSpeech.ApiKey := '@OPENAI_API_KEY';
  FSpeech.TranscriptionModel := tmGptTranscribe;
  FSpeech.TranscriptionLanguage := 'es';

  FConn := TAiChatConnection.Create(Self);
  FConn.OnReceiveThinking := AlPensar;
  FConn.OnReceiveData := AlRecibir;
  FConn.OnReceiveDataEnd := AlTerminar;
  FConn.OnError := AlFallar;

  ComboDriver.Items.CommaText := 'OpenAi,Claude,Gemini,Ollama';
  ComboDriver.ItemIndex := 0;   // dispara ComboDriverChange -> ConfigurarConexion

  ChatInput1.ValidExtensions := 'jpg,jpeg,png,webp,pdf,wav,mp3,m4a';
  ChatView1.AddMessage(TChatRole.crAssistant,
    '¡Hola! Soy **Lucía**, de Café La Ladera ☕' + sLineBreak + sLineBreak +
    'Pregúntame por nuestros cafés, mándame una foto o un PDF, o háblame con el micrófono.');
  IniciarPrueba;
end;

procedure TFormChatLadera.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Cerrar a mitad de un turno: la tarea todavia usa FConn (por ejemplo,
  // transcribiendo la nota de voz). Se desconectan los eventos, se aborta y se
  // espera a que termine; mientras, CheckSynchronize atiende lo ya encolado.
  FCerrando := True;
  AIVoiceMonitor1.Active := False;
  FConn.OnReceiveThinking := nil;
  FConn.OnReceiveData := nil;
  FConn.OnReceiveDataEnd := nil;
  FConn.OnError := nil;
  FConn.Abort;
  if Assigned(FTarea) then
    while not FTarea.Wait(50) do
      CheckSynchronize;
  CheckSynchronize;
  CanClose := True;
end;

procedure TFormChatLadera.FormDestroy(Sender: TObject);
begin
  FPrueba.Free;
end;

procedure TFormChatLadera.ConfigurarConexion(const ADriver: string);
var
  Key: string;
begin
  // Cambiar DriverName recrea el chat interno: el historial empieza de cero
  FConn.DriverName := ADriver;
  Key := ApiKeyFor(ADriver);
  if Key <> '' then
    FConn.Params.Values['ApiKey'] := Key;
  FConn.Params.Values['Asynchronous'] := 'True';   // streaming
  FConn.SystemPrompt.Text := PROMPT_SISTEMA;
  FConn.Tool_Active := False;

  // Lo que el modelo entiende directo (ModelCaps) y lo que la sesion pide
  // (SessionCaps). Lo que falta (SessionCaps - ModelCaps) lo resuelve el
  // framework antes de llamar al modelo: aqui, cap_Audio -> transcribir.
  if SameText(ADriver, 'OpenAi') then
    // gpt-6-sol lee imagenes y PDF (input_file de la API Responses)
    FConn.ModelCaps := [cap_Image, cap_Pdf, cap_Reasoning];
  FConn.SessionCaps := FConn.ModelCaps + [cap_Audio];
  FConn.ChatTools.SpeechTool := FSpeech;

  if Key.StartsWith('@') and (GetEnvironmentVariable(Key.Substring(1)) = '') then
    SetEstado('Falta la variable ' + Key.Substring(1))
  else
    SetEstado(Format('%s / %s', [FConn.DriverName, FConn.AiChat.Model]));
end;

procedure TFormChatLadera.ComboDriverChange(Sender: TObject);
begin
  if ComboDriver.ItemIndex < 0 then
    Exit;
  ConfigurarConexion(ComboDriver.Items[ComboDriver.ItemIndex]);
  ChatView1.ClearMessages;
end;

procedure TFormChatLadera.ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles;
  aAudioStream: TMemoryStream);
var
  Archivos: TAiMediaFilesArray;
  Nota: TAiMediaFile;
begin
  // aMediaFiles y aAudioStream se liberan al volver de este evento:
  // ToMediaFileArray devuelve copias, y el audio se copia en un TAiMediaFile.
  Archivos := aMediaFiles.ToMediaFileArray;
  if Assigned(aAudioStream) and (aAudioStream.Size > 100) then
  begin
    Nota := TAiMediaFile.Create;
    aAudioStream.Position := 0;
    Nota.LoadFromStream('nota_de_voz.wav', aAudioStream);
    Archivos := Archivos + [Nota];
    if APrompt = '' then
      APrompt := '🎤 (nota de voz)';
  end;

  ChatView1.AddUserMessage(APrompt, aMediaFiles);
  FBurbujaUsuario := ChatView1.MessageCount - 1;
  ChatView1.BeginAssistantMessage;
  FBurbuja := ChatView1.MessageCount - 1;
  FPensando := False;
  FReloj := TStopwatch.StartNew;
  SetEstado('Lucía está escribiendo...');

  // El puente de audio (transcribir) corre antes de la llamada al modelo y
  // es sincrono: en un hilo aparte la ventana no se congela.
  FTarea := TTask.Run(
    procedure
    begin
      try
        FConn.AddMessageAndRun(IfThen(APrompt.StartsWith('🎤'), '', APrompt), 'user', Archivos);
      except
        on E: Exception do
        begin
          var Msg := E.Message;
          TThread.Queue(nil,
            procedure
            begin
              AlFallar(FConn, Msg, nil, nil);
            end);
        end;
      end;
    end);
end;

procedure TFormChatLadera.AlPensar(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
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

procedure TFormChatLadera.TerminarPensando;
begin
  if FPensando then
  begin
    FPensando := False;
    ChatView1.FinishStep(FBurbuja, 'pensar', True, FReloj.ElapsedMilliseconds, '');
  end;
end;

procedure TFormChatLadera.AlRecibir(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
begin
  // Puede llegar desde el hilo de la peticion HTTP: la UI se toca en el principal
  if aText = '' then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando or (FBurbuja < 0) then
        Exit;
      TerminarPensando;
      ChatView1.AppendToken(FBurbuja, aText);
    end);
end;

procedure TFormChatLadera.AlTerminar(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
var
  Pie: string;
  Entrada: Integer;
begin
  if Assigned(aMsg) then
  begin
    // Los contadores del framework son disjuntos: Prompt_tokens son los nuevos y
    // Cached_tokens/Cache_write_tokens los del cache. La entrada es la suma; si
    // solo se mostrara Prompt_tokens, un turno con cache saldria con "3 tokens".
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
      MostrarTranscripcion;
      ChatView1.FinishMessage(FBurbuja);
      ChatView1.SetMessageFooter(FBurbuja, Pie);
      FBurbuja := -1;
      ChatInput1.Busy := False;
      SetEstado(Format('%s / %s', [FConn.DriverName, FConn.AiChat.Model]));
    end);
end;

procedure TFormChatLadera.MostrarTranscripcion;
var
  I: Integer;
  Msg: TAiChatMessage;
  Texto: string;
begin
  // Si el usuario mando una nota de voz, su burbuja muestra lo que se entendio
  for I := FConn.Messages.Count - 1 downto 0 do
  begin
    Msg := FConn.Messages[I];
    if Msg.Role <> 'user' then
      Continue;
    Texto := Msg.GetMediaTranscription;
    if (Texto <> '') and (FBurbujaUsuario >= 0) then
      ChatView1.SetMessageText(FBurbujaUsuario, '🎤 ' + Texto.Trim);
    Break;
  end;
end;

procedure TFormChatLadera.AlFallar(Sender: TObject; const ErrorMsg: string; AException: Exception;
  const AResponse: IHTTPResponse);
begin
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando then
        Exit;
      if FBurbuja >= 0 then
      begin
        TerminarPensando;
        ChatView1.SetMessageText(FBurbuja, '⚠️ ' + ErrorMsg);
        ChatView1.FinishMessage(FBurbuja);
        FBurbuja := -1;
      end;
      ChatInput1.Busy := False;
      SetEstado('Error: ' + ErrorMsg);
    end);
end;

procedure TFormChatLadera.ChatInput1Cancel(Sender: TObject);
begin
  FConn.Abort;
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

procedure TFormChatLadera.ChatInput1MicToggle(Sender: TObject; AActivate: Boolean);
begin
  // El boton del microfono solo cambia su dibujo: el que graba es el monitor
  AIVoiceMonitor1.Active := AActivate;
end;

procedure TFormChatLadera.btnNuevaClick(Sender: TObject);
begin
  FConn.NewChat;
  ChatView1.ClearMessages;
  SetEstado('Conversación nueva');
end;

function TFormChatLadera.CarpetaDatos: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'CafeLaLadera');
  ForceDirectories(Result);
end;

procedure TFormChatLadera.btnGuardarClick(Sender: TObject);
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
  FConn.Messages.SaveToFile(TPath.Combine(CarpetaDatos, 'historial.json'));
  SetEstado('Guardado en ' + CarpetaDatos);
end;

procedure TFormChatLadera.btnAbrirClick(Sender: TObject);
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
  FConn.NewChat;
  FConn.Messages.LoadFromFile(TPath.Combine(CarpetaDatos, 'historial.json'));
  SetEstado(Format('Conversación recuperada: %d mensajes', [FConn.Messages.Count]));
end;

procedure TFormChatLadera.IniciarPrueba;
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
    else if SameText(ParamStr(I), '--pausa') and (I < ParamCount) then
    begin
      FPausaMs := StrToIntDef(ParamStr(I + 1), 0);
      Inc(I);
    end
    else if SameText(ParamStr(I), '--tamano') and (I < ParamCount) then
    begin
      // "1440x1000": tamano del area cliente (para que el video no se agrande)
      ClientWidth := StrToIntDef(ParamStr(I + 1).Split(['x'])[0], ClientWidth);
      ClientHeight := StrToIntDef(ParamStr(I + 1).Split(['x'])[1], ClientHeight);
      Inc(I);
    end
    else if SameText(ParamStr(I), '--marcas') and (I < ParamCount) then
    begin
      FMarcas := ParamStr(I + 1);
      Inc(I);
    end
    else if SameText(ParamStr(I), '--tecleo') then
      FTecleo := True
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
  FTimerPrueba.Interval := 50;
  FFinTurno := TThread.GetTickCount + 1500;   // que se vea la ventana antes de la primera pregunta
  FTimerPrueba.OnTimer := PasoPrueba;
end;

procedure TFormChatLadera.PasoPrueba(Sender: TObject);

  procedure Enviar(const APregunta, AArchivo: string);
  var
    Archivos: TAiMediaFiles;
    MF: TAiMediaFile;
  begin
    Archivos := TAiMediaFiles.Create;
    try
      if AArchivo <> '' then
      begin
        MF := TAiMediaFile.Create;
        MF.LoadFromFile(AArchivo);
        Archivos.Add(MF);
      end;
      ChatInput1.PromptText := '';
      ChatInput1.Busy := True;
      Marcar('envio' + IntToStr(FTurno));
      ChatInput1SendEvent(ChatInput1, APregunta, Archivos, nil);
    finally
      Archivos.Free;
    end;
  end;

var
  Linea, Pregunta: string;
begin
  // Envia cada pregunta como si la escribiera el usuario, una tras otra
  if ChatInput1.Busy then
  begin
    FFinTurno := 0;
    Exit;
  end;
  if FFinTurno = 0 then
  begin
    FFinTurno := TThread.GetTickCount;
    if FTurno > 0 then
      Marcar('fin' + IntToStr(FTurno));
  end;

  // Tecleo en curso: una letra por tic (50 ms)
  if FEscribiendo <> '' then
  begin
    if ChatInput1.PromptText.Length < FEscribiendo.Length then
      ChatInput1.PromptText := FEscribiendo.Substring(0, ChatInput1.PromptText.Length + 1)
    else if Int64(TThread.GetTickCount) - Int64(FFinTurno) > 700 then
    begin
      Pregunta := FEscribiendo;
      FEscribiendo := '';
      Enviar(Pregunta, FArchivoPendiente);
    end;
    Exit;
  end;

  if Int64(TThread.GetTickCount) - Int64(FFinTurno) < FPausaMs then
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
  Inc(FTurno);
  Marcar('q' + IntToStr(FTurno));
  Pregunta := Linea;
  FArchivoPendiente := '';
  // "@ruta|pregunta": adjunta un archivo
  if Linea.StartsWith('@') then
  begin
    Pregunta := Linea.Substring(Linea.IndexOf('|') + 1);
    FArchivoPendiente := Linea.Substring(1, Linea.IndexOf('|') - 1);
  end;
  if FTecleo and (Pregunta <> '') then
  begin
    FEscribiendo := Pregunta;
    ChatInput1.PromptText := '';
    FFinTurno := TThread.GetTickCount + Cardinal(Pregunta.Length) * 50;  // pausa corta al terminar de teclear
  end
  else
    Enviar(Pregunta, FArchivoPendiente);
end;

procedure TFormChatLadera.Marcar(const ANombre: string);
begin
  // Hora UTC en ms: grabar_ventana.py la convierte a segundos del video (tiempos.json)
  if FMarcas <> '' then
    TFile.AppendAllText(FMarcas, Format('%s'#9'%d'#13#10,
      [ANombre, Round((TTimeZone.Local.ToUniversalTime(Now) - UnixDateDelta) * MSecsPerDay)]));
end;

procedure TFormChatLadera.SetEstado(const AText: string);
begin
  lblEstado.Text := AText;
end;

end.
