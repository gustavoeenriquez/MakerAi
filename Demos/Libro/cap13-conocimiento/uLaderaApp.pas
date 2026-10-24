unit uLaderaApp;

// =============================================================================
// LIBRO "IA con Delphi" - La app de Cafe La Ladera, etapa del capitulo 13
// =============================================================================
// Conocimiento: el manual de la tienda (envios, molienda, club, devoluciones, tostion,
// atencion) vive en una base vectorial sobre SQLite (TAiRAGVector + TAiRAGVectorSQLiteDriver,
// embeddings de OpenAI). Se indexa una sola vez, por secciones y con metadatos. El modelo lo
// consulta con la herramienta buscar_en_el_manual: recuperar (coseno) + reordenar con Jev
// (TAiJevRAGReranker), que ademas descarta un pasaje con inyeccion de prompt.
// Sigue todo lo anterior: funciones de la tienda, web, editor y consola con sus limites,
// sentidos y filtro.
//
//   ModelCaps   : lo que el modelo entiende por si mismo (gpt-6-sol: imagen y PDF).
//   SessionCaps : lo que la sesion pide (ademas, audio).
//   El hueco (SessionCaps - ModelCaps) lo cubre el framework con un puente
//   ANTES de llamar al modelo: aqui, ChatTools.SpeechTool transcribe la nota
//   con gpt-transcribe y el modelo recibe el texto.
//
// El filtro del capitulo 8 sigue activo. La llamada corre en un TTask porque
// el puente es sincrono y sin el hilo la ventana se congelaria al transcribir.
//
// Prueba automatica (para el libro, sin tocar el teclado):
//   LaderaApp.exe --prueba "pregunta" "@media\bolsa.png|que cafe es" "@media\nota.wav|" [--captura x.png] [--registro t.txt]
// Empresa, productos y precios ficticios.
// =============================================================================

interface

uses
  System.SysUtils, System.StrUtils, System.Variants, System.Types, System.UITypes, System.Classes, System.IOUtils,
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
  uMakerAi.Chat.Sanitizer,
  uMakerAi.Chat.Tools,
  uMakerAi.Jev.PromptGuard,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,   // registra todos los drivers
  uMakerAi.OpenAI.Audio,
  uMakerAi.OpenAI.Audio.Tool,
  uMakerAi.Utils.VoiceMonitor,
  // El conocimiento (capitulo 13): embeddings, base vectorial y su driver SQLite (FireDAC embebido)
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.DApt, Data.DB, FireDAC.Comp.Client,
  uMakerAi.Embeddings.Core, uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Vectors, uMakerAi.RAG.Vectors.Index, uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vector.Driver.SQLite, uMakerAi.Jev.RAG;

type
  TFormLadera = class(TForm)
    AiConn: TAiChatConnection;
    JevGuard: TAiJevPromptGuard;
    SpeechTool: TAiOpenAiSpeechTool;
    AIVoiceMonitor1: TAIVoiceMonitor;
    AiFunctions1: TAiFunctions;
    Shell: TAiShell;
    Editor: TAiTextEditorTool;
    Embeddings: TAiOpenAiEmbeddings;
    FDConn: TFDConnection;
    FDWait: TFDGUIxWaitCursor;
    RAGDriver: TAiRAGVectorSQLiteDriver;
    RAG: TAiRAGVector;
    Reranker: TAiJevRAGReranker;
    LayoutTop: TLayout;
    ComboDriver: TComboBox;
    btnNueva: TButton;
    btnGuardar: TButton;
    btnAbrir: TButton;
    lblEstado: TLabel;
    ChatView1: TAIChatView;
    ChatInput1: TAIChatInput;
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
    procedure AiConnReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
      aRole, aText: string);
    procedure AiConnError(Sender: TObject; const ErrorMsg: string; AException: Exception;
      const AResponse: IHTTPResponse);
    procedure AiConnSanitize(Sender: TObject; const AResult: TSanitizeResult; var AAction: TAiSanitizeAction);
    procedure AiConnPromptGuard(Sender: TObject; const AVerdict: TAiPromptVerdict; var AAction: TAiSanitizeAction);
    procedure FnConsultarInventario(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnCalcularTotal(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnBuscarManual(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure EditorBeforeCommand(Sender: TObject; const Command, Path: string; Args: TJSONObject;
      var Result: string; var Handled: Boolean);
    procedure ShellConsoleLog(Sender: TObject; const Command: string; const StdOut, StdErr: string; ExitCode: Integer);
    procedure ShellSecurityViolation(Sender: TObject; const Command: string; const CallId: string; var AErrorMessage: string);
  private
    FBurbuja: Integer;        // burbuja del asistente que se esta escribiendo
    FBurbujaUsuario: Integer; // burbuja del ultimo mensaje del usuario (para la transcripcion)
    FPensando: Boolean;
    FReloj: TStopwatch;
    FTarea: ITask;            // el turno en curso (corre fuera del hilo principal)
    FCerrando: Boolean;
    FBloqueo: string;
    FTextoRecibido: Boolean;  // llego algun pedazo por OnReceiveData en este turno
    FCarpetaTrabajo: string;
    procedure ConfigurarConexion(const ADriver: string);
    procedure TerminarPensando;
    procedure MostrarTranscripcion;
    procedure SetEstado(const AText: string);
    function CarpetaDatos: string;
    procedure IniciarPrueba;
    procedure PasoPrueba(Sender: TObject);
    procedure Registrar(const ALinea: string);
    procedure PasoHerramienta(const ATitulo, ADetalle: string);
    procedure PrepararConocimiento;
    function IndexarManual: Integer;
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
  FBurbujaUsuario := -1;
  // La carpeta donde el editor y la consola pueden trabajar
  FCarpetaTrabajo := IncludeTrailingPathDelimiter(TPath.Combine(CarpetaDatos, 'trabajo'));
  ForceDirectories(FCarpetaTrabajo);
  SetCurrentDir(FCarpetaTrabajo);
  PrepararConocimiento;
  ComboDriver.Items.CommaText := 'OpenAi,Claude,Groq,DeepSeek,Mistral,Ollama';
  ComboDriver.ItemIndex := 0;   // dispara ComboDriverChange -> ConfigurarConexion
  ChatView1.AddMessage(TChatRole.crAssistant,
    '¡Hola! Soy **Lucía**, de Café La Ladera ☕' + sLineBreak + sLineBreak +
    'Pregúntame por nuestros cafés, mándame una foto o un PDF, o háblame con el micrófono.');
  IniciarPrueba;
end;

procedure TFormLadera.FormDestroy(Sender: TObject);
begin
  FPrueba.Free;
end;

procedure TFormLadera.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Cerrar a mitad de un turno: la tarea todavia usa AiConn (por ejemplo,
  // transcribiendo la nota de voz). Se desconectan los eventos, se aborta y se
  // espera a que termine; mientras, CheckSynchronize atiende lo ya encolado.
  FCerrando := True;
  AIVoiceMonitor1.Active := False;
  AiConn.OnReceiveThinking := nil;
  AiConn.OnReceiveData := nil;
  AiConn.OnReceiveDataEnd := nil;
  AiConn.OnError := nil;
  AiConn.Abort;
  if Assigned(FTarea) then
    while not FTarea.Wait(50) do
      CheckSynchronize;
  CheckSynchronize;
  CanClose := True;
end;

procedure TFormLadera.ConfigurarConexion(const ADriver: string);
var
  Key: string;
begin
  // Cambiar DriverName recrea el chat interno: el historial empieza de cero.
  AiConn.DriverName := ADriver;
  Key := ApiKeyFor(ADriver);
  if Key <> '' then
    AiConn.Params.Values['ApiKey'] := Key;

  // Lo que el modelo entiende por si mismo (ModelCaps) y lo que la sesion pide
  // (SessionCaps). Lo que falta lo resuelve el framework antes de llamar al
  // modelo: aqui, cap_Audio -> transcribir con ChatTools.SpeechTool.
  // Al fijar ModelCaps se fija el conjunto completo: vacio = el catalogo del modelo.
  if MatchText(ADriver, ['OpenAi', 'Claude']) then
    // leen imagenes y PDF, y traen busqueda web, editor y consola nativos
    AiConn.ModelCaps := [cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch, cap_TextEditor, cap_Shell]
  else
    AiConn.ModelCaps := [];                                    // los demas, aqui, solo texto
  AiConn.SessionCaps := AiConn.ModelCaps + [cap_Audio];        // la sesion ademas escucha

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

{ Un turno: enviar (con adjuntos), recibir, terminar }

procedure TFormLadera.ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles;
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

  ChatView1.AddUserMessage(APrompt, aMediaFiles);   // la burbuja muestra los adjuntos
  FBurbujaUsuario := ChatView1.MessageCount - 1;
  ChatView1.BeginAssistantMessage;
  FBurbuja := ChatView1.MessageCount - 1;
  FPensando := False;
  FBloqueo := '';
  FTextoRecibido := False;
  FReloj := TStopwatch.StartNew;
  SetEstado('Lucía está escribiendo...');

  // El puente de audio (transcribir) corre antes de la llamada al modelo y es
  // sincrono: en un hilo aparte la ventana no se congela.
  FTarea := TTask.Run(
    procedure
    begin
      try
        AiConn.AddMessageAndRun(IfThen(APrompt.StartsWith('🎤'), '', APrompt), 'user', Archivos);
      except
        on E: Exception do
        begin
          var Msg := E.Message;
          TThread.Queue(nil,
            procedure
            begin
              AiConnError(AiConn, Msg, nil, nil);
            end);
        end;
      end;
    end);
end;

procedure TFormLadera.AiConnReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
begin
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
  if aText = '' then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando or (FBurbuja < 0) then
        Exit;
      TerminarPensando;
      FTextoRecibido := True;
      ChatView1.AppendToken(FBurbuja, aText);
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
      // Tras llamar herramientas, la respuesta final puede llegar completa en aText
      // sin pasar por OnReceiveData: se pone entera si la burbuja sigue vacia.
      if (not FTextoRecibido) and (aText <> '') then
        ChatView1.SetMessageText(FBurbuja, aText);
      ChatView1.FinishMessage(FBurbuja);
      ChatView1.SetMessageFooter(FBurbuja, Pie);
      Registrar('turno'#9 + Pie);
      FBurbuja := -1;
      ChatInput1.Busy := False;
      SetEstado(Format('%s / %s', [AiConn.DriverName, AiConn.AiChat.Model]));
    end);
end;

procedure TFormLadera.MostrarTranscripcion;
var
  I: Integer;
  Msg: TAiChatMessage;
  Texto: string;
begin
  // Si el usuario mando una nota de voz, su burbuja muestra lo que se entendio
  for I := AiConn.Messages.Count - 1 downto 0 do
  begin
    Msg := AiConn.Messages[I];
    if Msg.Role <> 'user' then
      Continue;
    Texto := Msg.GetMediaTranscription;
    if (Texto <> '') and (FBurbujaUsuario >= 0) then
    begin
      ChatView1.SetMessageText(FBurbujaUsuario, '🎤 ' + Texto.Trim);
      Registrar('transcripcion'#9 + Texto.Trim);
    end;
    Break;
  end;
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

procedure TFormLadera.ChatInput1MicToggle(Sender: TObject; AActivate: Boolean);
begin
  // El boton del microfono solo cambia su dibujo: el que graba es el monitor.
  // Cuando el usuario calla, TAIChatInput manda el WAV por OnSendEvent.
  AIVoiceMonitor1.Active := AActivate;
end;

{ El filtro (capitulo 8) }

procedure TFormLadera.AiConnSanitize(Sender: TObject; const AResult: TSanitizeResult; var AAction: TAiSanitizeAction);
var
  M: TSuspiciousMatch;
  Nombres: string;
begin
  for M in AResult.SuspiciousMatches do
    Nombres := Nombres + M.PatternName + ' ';
  FBloqueo := 'Mensaje bloqueado por el sanitizador: fórmula conocida (' + Nombres.Trim + ').';
end;

procedure TFormLadera.AiConnPromptGuard(Sender: TObject; const AVerdict: TAiPromptVerdict; var AAction: TAiSanitizeAction);
begin
  FBloqueo := Format('Mensaje bloqueado por el guardián: `%s` (%.2f).', [AVerdict.Category, AVerdict.Score]);   // en backticks: la burbuja es Markdown
end;


{ Las herramientas de la tienda (capitulo 11). El modelo decide cuando llamarlas;
  cada OnAction lee los parametros, hace el trabajo y devuelve el resultado en
  ToolCall.Response. Llegan desde el hilo de la peticion: la UI se toca con Queue. }

function BolsasDe(const ACafe: string): Integer;
begin
  // El inventario de hoy (ficticio). En una app real: la base de datos.
  if ACafe.Contains('Huila') then Exit(24);
  if ACafe.Contains('Nari') then Exit(10);
  if ACafe.Contains('Sierra') then Exit(0);
  if ACafe.Contains('Descaf') then Exit(15);
  Result := -1;
end;

function PrecioDe(const ACafe: string): Integer;
begin
  if ACafe.Contains('Huila') then Exit(38000);
  if ACafe.Contains('Nari') then Exit(42000);
  if ACafe.Contains('Sierra') then Exit(36000);
  if ACafe.Contains('Descaf') then Exit(34000);
  Result := 0;
end;

procedure TFormLadera.PasoHerramienta(const ATitulo, ADetalle: string);
var
  Id: string;
begin
  // Un paso en la burbuja de Lucia: lo que hizo mientras preparaba la respuesta
  Id := 'tool' + IntToStr(Random(100000));
  Registrar('herramienta'#9 + ATitulo + ' -> ' + ADetalle);
  TThread.Queue(nil,
    procedure
    begin
      if FCerrando or (FBurbuja < 0) then
        Exit;
      ChatView1.AddStep(FBurbuja, Id, ATitulo);
      ChatView1.FinishStep(FBurbuja, Id, True, 0, ADetalle);
    end);
end;

procedure TFormLadera.FnConsultarInventario(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Cafe: string;
  Bolsas: Integer;
begin
  Handled := True;
  Cafe := ToolCall.Params.Values['cafe'];
  Bolsas := BolsasDe(Cafe);
  if Bolsas < 0 then
    ToolCall.Response := 'No existe ese café en la carta.'
  else
    ToolCall.Response := Format('%s: %d bolsas de 340 g disponibles.', [Cafe, Bolsas]);
  PasoHerramienta('consultar_inventario(' + Cafe + ')', ToolCall.Response);
end;

procedure TFormLadera.FnCalcularTotal(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Cafe, Ciudad: string;
  Bolsas, Subtotal, Envio: Integer;
begin
  Handled := True;
  Cafe := ToolCall.Params.Values['cafe'];
  Bolsas := StrToIntDef(ToolCall.Params.Values['bolsas'], 0);
  Ciudad := ToolCall.Params.Values['ciudad'];
  Subtotal := PrecioDe(Cafe) * Bolsas;
  if SameText(Ciudad, 'Pitalito') then
    Envio := 5000
  else
    Envio := 12000;
  ToolCall.Response := Format('%d x %s = %d COP; envío a %s: %d COP; total: %d COP.',
    [Bolsas, Cafe, Subtotal, Ciudad, Envio, Subtotal + Envio]);
  PasoHerramienta(Format('calcular_total(%s, %d, %s)', [Cafe, Bolsas, Ciudad]), ToolCall.Response);
end;


{ Manos en el sistema (capitulo 12): cada herramienta con su limite }

procedure TFormLadera.EditorBeforeCommand(Sender: TObject; const Command, Path: string; Args: TJSONObject;
  var Result: string; var Handled: Boolean);
var
  Completa: string;
begin
  // Rutas relativas se resuelven contra la carpeta de trabajo; lo que termine fuera
  // (C:\Windows\..., ..\..\) se rechaza sin tocarlo. Y el editor no borra.
  if TPath.IsPathRooted(Path) then
    Completa := TPath.GetFullPath(Path)
  else
    Completa := TPath.GetFullPath(TPath.Combine(FCarpetaTrabajo, Path));
  if not Completa.StartsWith(FCarpetaTrabajo, True) then
  begin
    Result := 'Error: solo puedes trabajar dentro de la carpeta de trabajo.';
    Handled := True;
    PasoHerramienta('editor ' + Command + ' ' + Path, 'BLOQUEADO: fuera de la carpeta de trabajo');
  end
  else if SameText(Command, 'delete') then
  begin
    Result := 'Error: el editor no tiene permiso para borrar archivos.';
    Handled := True;
    PasoHerramienta('editor ' + Command + ' ' + Path, 'BLOQUEADO: el editor no borra');
  end
  else
    PasoHerramienta('editor ' + Command + ' ' + Path, 'ok');
end;

procedure TFormLadera.ShellConsoleLog(Sender: TObject; const Command: string; const StdOut, StdErr: string;
  ExitCode: Integer);
begin
  PasoHerramienta('consola: ' + Command, Format('salida de %d caracteres', [Length(StdOut)]));
end;

procedure TFormLadera.ShellSecurityViolation(Sender: TObject; const Command: string; const CallId: string;
  var AErrorMessage: string);
begin
  PasoHerramienta('consola: ' + Command, 'BLOQUEADO: no está en la lista permitida');
end;

{ El conocimiento (capitulo 13): el manual de la tienda en una base vectorial sobre
  SQLite. Se indexa una sola vez, por secciones y con metadatos; el modelo lo consulta
  con la herramienta buscar_en_el_manual y el reranker descarta lo que no responde
  (o trae una inyeccion). }

procedure TFormLadera.PrepararConocimiento;
var
  N: Integer;
begin
  // Los componentes estan en el formulario; aqui se conectan entre si.
  Embeddings.ApiKey := '@OPENAI_API_KEY';
  RAG.Embeddings := Embeddings;       // quien convierte texto en vectores
  RAG.Driver := RAGDriver;            // donde se guardan y se buscan (SQLite)
  FDConn.Params.Database := TPath.Combine(CarpetaDatos, 'conocimiento.db');
  FDConn.Connected := True;
  RAGDriver.CreateSchema('manual');   // CREATE TABLE IF NOT EXISTS: la segunda vez no hace nada
  N := FDConn.ExecSQLScalar('SELECT COUNT(*) FROM manual');
  if N = 0 then
    N := IndexarManual;               // solo la primera vez: cada pasaje cuesta una llamada de embeddings
  Registrar('conocimiento'#9 + Format('%d pasajes en %s', [N, FDConn.Params.Database]));
end;

function TFormLadera.IndexarManual: Integer;
var
  Archivo, L, Titulo, Texto: string;
  Meta: TAiEmbeddingMetaData;

  procedure Guardar;
  begin
    if (Texto.Trim = '') or (Titulo = '') then
      Exit;
    Meta['fuente'] := IfThen(Archivo.Contains('foro'), 'foro', 'manual');
    Meta['seccion'] := Titulo;
    RAG.AddItem(Titulo + '. ' + Texto.Trim, Meta);   // embedding + fila en SQLite
    Inc(Result);
    Texto := '';
  end;

begin
  // Cada archivo de manual\ se parte por secciones: un titulo (linea corta sin punto
  // final) seguido de sus parrafos. Un pasaje por seccion, con su fuente en los metadatos.
  Result := 0;
  Meta := TAiEmbeddingMetaData.Create;
  try
    for Archivo in TDirectory.GetFiles(TPath.Combine(ExtractFilePath(ParamStr(0)), 'manual'), '*.txt') do
    begin
      Titulo := '';
      Texto := '';
      for L in TFile.ReadAllLines(Archivo, TEncoding.UTF8) do
      begin
        if L.Trim = '' then
          Continue;
        if (L.Length < 60) and not L.Trim.EndsWith('.') and not L.Contains(':') then
        begin
          Guardar;
          Titulo := L.Trim;
        end
        else if Titulo <> '' then
          Texto := Texto + L.Trim + ' ';
      end;
      Guardar;
    end;
  finally
    Meta.Free;
  end;
end;

procedure TFormLadera.FnBuscarManual(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Pregunta, Detalle, Respuesta: string;
  Encontrados: TAiRAGVector;
  I: Integer;
  Nodo: TAiEmbeddingNode;
begin
  Handled := True;
  Pregunta := ToolCall.Params.Values['pregunta'];
  // 1. Recuperar: los pasajes mas parecidos a la pregunta (coseno, en SQLite)
  Encontrados := RAG.Search(Pregunta, 4, 0.3, nil);
  try
    for I := 0 to Encontrados.Count - 1 do
      Detalle := Detalle + Format('%s %.2f · ', [VarToStr(Encontrados.Items[I].MetaData['seccion']), Encontrados.Items[I].Idx]);
    // 2. Reordenar con Jev: evidencia por pasaje; una inyeccion de prompt queda en -1
    try
      Encontrados.RerankWith(Pregunta, Reranker);
      Detalle := Detalle + '→ rerank: ';
    except
      on E: Exception do
        Detalle := Detalle + '→ sin reranker (' + E.Message + '): ';
    end;
    for I := 0 to Encontrados.Count - 1 do
    begin
      Nodo := Encontrados.Items[I];
      if Nodo.Idx < 0 then
        Detalle := Detalle + Format('%s descartado · ', [VarToStr(Nodo.MetaData['seccion'])])
      else
      begin
        Detalle := Detalle + Format('%s %.2f · ', [VarToStr(Nodo.MetaData['seccion']), Nodo.Idx]);
        if Nodo.Idx >= 0.2 then
          Respuesta := Respuesta + Nodo.Text + sLineBreak;
      end;
    end;
  finally
    Encontrados.Free;
  end;
  if Respuesta = '' then
    Respuesta := 'El manual no dice nada sobre eso.';
  ToolCall.Response := Respuesta;
  PasoHerramienta('buscar_en_el_manual(' + Pregunta + ')', Detalle.TrimRight([' ', '·']));
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
  Linea, Pregunta, Archivo: string;
  Archivos: TAiMediaFiles;
  MF: TAiMediaFile;
begin
  // Envia cada pregunta como si la escribiera el usuario. "@ruta|pregunta" adjunta un archivo.
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
  else
  begin
    Pregunta := Linea;
    Archivo := '';
    if Linea.StartsWith('@') then
    begin
      Pregunta := Linea.Substring(Linea.IndexOf('|') + 1);
      Archivo := Linea.Substring(1, Linea.IndexOf('|') - 1);
    end;
    Archivos := TAiMediaFiles.Create;
    try
      if Archivo <> '' then
      begin
        MF := TAiMediaFile.Create;
        MF.LoadFromFile(Archivo);
        Archivos.Add(MF);
      end;
      ChatInput1.PromptText := '';
      ChatInput1.Busy := True;
      ChatInput1SendEvent(ChatInput1, Pregunta, Archivos, nil);
    finally
      Archivos.Free;
    end;
  end;
  FFinTurno := TThread.GetTickCount + 700;
end;

end.
