program ChatStreaming;

// =============================================================================
// DEMO 094 - Streaming, prompt de sistema e historial
// =============================================================================
// Tres ideas que todo chat necesita, con la barista virtual de Cafe La Ladera:
//
//   1. PROMPT DE SISTEMA : SystemPrompt fija quien es el asistente, que sabe
//                          (la carta de la tienda) y que reglas sigue.
//   2. STREAMING         : con Asynchronous=True la respuesta llega por pedazos
//                          en OnReceiveData; OnReceiveDataEnd avisa el final.
//   3. HISTORIAL         : Messages guarda la conversacion. Por eso el modelo
//                          entiende "y ese cuanto cuesta?". NewChat la olvida;
//                          Messages.SaveToFile / LoadFromFile la guardan y la
//                          recuperan.
//
// Uso:
//   ChatStreaming.exe                          OpenAI (@OPENAI_API_KEY)
//   ChatStreaming.exe --driver Claude          cualquier driver registrado
//   ChatStreaming.exe --driver Ollama --model gemma4
//   ChatStreaming.exe --sin-streaming          misma charla, respuesta completa
//   ChatStreaming.exe --interactivo            charla libre: /nuevo /guardar
//                                              /cargar /historial /salir
//
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  System.JSON,
  System.Net.HttpClient,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;   // registra todos los drivers

const
  ARCHIVO_HISTORIAL = 'historial_ladera.json';

  PROMPT_SISTEMA =
    'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano.' + sLineBreak +
    'Carta (bolsa de 340 g): Huila Reserva 38.000 COP (panela, frutos rojos); ' +
    'Nariño Altura 42.000 COP (cítricos, caramelo); Sierra Nevada Orgánico 36.000 COP ' +
    '(chocolate amargo, nuez); Descafeinado Suave 34.000 COP (cacao suave).' + sLineBreak +
    'Reglas: responde en español, cálido y breve (máximo 4 líneas). ' +
    'Solo hablas de café y de la tienda; si te preguntan otra cosa, vuelve con amabilidad al café. ' +
    'No inventes productos ni precios fuera de la carta.';

type
  // Recibe los eventos del chat. Los eventos de MakerAI son "of object":
  // necesitan un metodo de una clase, no un metodo anonimo.
  TOyente = class
  private
    FLock: TCriticalSection;
    FFin: TEvent;
    FError: string;
    FPrimerPedazo: Boolean;
    FReloj: TStopwatch;
    FMsPrimerPedazo: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AlRecibir(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: string);
    procedure AlTerminar(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: string);
    procedure AlFallar(Sender: TObject; const ErrorMsg: string; AException: Exception; const AResponse: IHTTPResponse);
    procedure Preparar;
    function Esperar: Boolean;
    property Error: string read FError;
    property MsPrimerPedazo: Int64 read FMsPrimerPedazo;
  end;

var
  ConsoleLock: TCriticalSection;

procedure Log(const S: string = '');
begin
  ConsoleLock.Enter;
  try
    Writeln(S);
  finally
    ConsoleLock.Leave;
  end;
end;

{ TOyente }

constructor TOyente.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FFin := TEvent.Create(nil, True, False, '');
end;

destructor TOyente.Destroy;
begin
  FFin.Free;
  FLock.Free;
  inherited;
end;

procedure TOyente.Preparar;
begin
  FError := '';
  FPrimerPedazo := True;
  FMsPrimerPedazo := 0;
  FFin.ResetEvent;
  FReloj := TStopwatch.StartNew;
end;

procedure TOyente.AlRecibir(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
begin
  // Cada pedazo se escribe apenas llega, sin salto de linea.
  // Puede llegar desde el hilo de la peticion HTTP: la consola va con candado.
  if aText = '' then
    Exit;
  ConsoleLock.Enter;
  try
    if FPrimerPedazo then
    begin
      FPrimerPedazo := False;
      FMsPrimerPedazo := FReloj.ElapsedMilliseconds;
    end;
    Write(aText);
  finally
    ConsoleLock.Leave;
  end;
end;

procedure TOyente.AlTerminar(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: string);
begin
  FFin.SetEvent;
end;

procedure TOyente.AlFallar(Sender: TObject; const ErrorMsg: string; AException: Exception;
  const AResponse: IHTTPResponse);
begin
  FError := ErrorMsg;
  FFin.SetEvent;
end;

function TOyente.Esperar: Boolean;
begin
  // Hay drivers que entregan los eventos con TThread.Queue: en una consola el
  // hilo principal tiene que atenderlos con CheckSynchronize mientras espera.
  while FFin.WaitFor(0) <> wrSignaled do
    CheckSynchronize(50);
  Result := FError = '';
end;

{ Utilidades de linea de comandos }

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

function HasFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
      Exit(True);
end;

function ApiKeyFor(const ADriver: string): string;
begin
  if SameText(ADriver, 'Ollama') or SameText(ADriver, 'LMStudio') or SameText(ADriver, 'GenericLLM') then
    Result := ''
  else if SameText(ADriver, 'OpenAi') then
    Result := '@OPENAI_API_KEY'
  else
    Result := '@' + UpperCase(ADriver) + '_API_KEY';
end;

{ La charla }

var
  Conn: TAiChatConnection;
  Oyente: TOyente;
  Streaming: Boolean;

procedure Preguntar(const APregunta: string);
var
  Reloj: TStopwatch;
  Respuesta: string;
begin
  Log('Tú: ' + APregunta);
  ConsoleLock.Enter;
  try
    Write('Lucía: ');
  finally
    ConsoleLock.Leave;
  end;

  if Streaming then
  begin
    Oyente.Preparar;
    Conn.AddMessageAndRun(APregunta, 'user', []);   // vuelve enseguida
    if not Oyente.Esperar then
      Log('[error] ' + Oyente.Error)
    else
    begin
      Log;
      Log(Format('  (primer pedazo a los %d ms)', [Oyente.MsPrimerPedazo]));
    end;
  end
  else
  begin
    Reloj := TStopwatch.StartNew;
    Respuesta := Conn.AddMessageAndRun(APregunta, 'user', []);   // bloquea hasta el final
    if Respuesta = '' then
      Log('[error] ' + Conn.LastError)
    else
    begin
      Log(Respuesta.Trim);
      Log(Format('  (respuesta completa a los %d ms)', [Reloj.ElapsedMilliseconds]));
    end;
  end;
  Log;
end;

procedure MostrarHistorial;
var
  I: Integer;
  M: TAiChatMessage;
  Texto: string;
begin
  Log(Format('--- Historial: %d mensajes ---', [Conn.Messages.Count]));
  for I := 0 to Conn.Messages.Count - 1 do
  begin
    M := Conn.Messages[I];
    Texto := M.Prompt.Replace(sLineBreak, ' ').Trim;
    if Texto.Length > 70 then
      Texto := Texto.Substring(0, 70) + '...';
    Log(Format('  %d. %-9s %s', [I + 1, M.Role, Texto]));
  end;
  Log(Format('  Tokens: %d de entrada, %d de salida', [Conn.Prompt_tokens, Conn.Completion_tokens]));
  Log;
end;

procedure Titulo(const S: string);
begin
  Log('==== ' + S + ' ' + StringOfChar('=', 60 - Length(S)));
  Log;
end;

procedure CharlaGuiada;
begin
  Titulo('1. Prompt de sistema');
  Log('SystemPrompt:');
  Log('  ' + Conn.SystemPrompt.Text.Trim.Replace(sLineBreak, sLineBreak + '  '));
  Log;
  Preguntar('Hola, ¿qué café me recomiendas si me gusta el chocolate?');

  Titulo('2. Las reglas se cumplen');
  Preguntar('¿Me ayudas con la tarea de matemáticas?');

  Titulo('3. Historial: el modelo recuerda');
  Preguntar('¿Y cuánto cuestan dos bolsas de ese?');
  MostrarHistorial;

  Titulo('4. Guardar, olvidar y recuperar');
  Conn.Messages.SaveToFile(ARCHIVO_HISTORIAL);
  Log('Historial guardado en ' + ARCHIVO_HISTORIAL);
  Conn.NewChat;
  Log(Format('NewChat: el historial queda con %d mensajes.', [Conn.Messages.Count]));
  Log;
  Preguntar('¿Qué café te pregunté hace un momento?');

  Conn.NewChat;
  Conn.Messages.LoadFromFile(ARCHIVO_HISTORIAL);
  Log(Format('Historial cargado de %s: %d mensajes.', [ARCHIVO_HISTORIAL, Conn.Messages.Count]));
  Log;
  Preguntar('¿Qué café te pregunté hace un momento?');
end;

procedure CharlaLibre;
var
  Linea: string;
begin
  Log('Escribe tu pregunta. Comandos: /nuevo /guardar /cargar /historial /salir');
  Log;
  while True do
  begin
    Write('> ');
    Readln(Linea);
    Linea := Linea.Trim;
    if Linea = '' then
      Continue;
    if SameText(Linea, '/salir') then
      Break
    else if SameText(Linea, '/nuevo') then
    begin
      Conn.NewChat;
      Log('Conversación nueva.');
      Log;
    end
    else if SameText(Linea, '/guardar') then
    begin
      Conn.Messages.SaveToFile(ARCHIVO_HISTORIAL);
      Log('Guardado en ' + ARCHIVO_HISTORIAL);
      Log;
    end
    else if SameText(Linea, '/cargar') then
    begin
      if FileExists(ARCHIVO_HISTORIAL) then
      begin
        Conn.NewChat;
        Conn.Messages.LoadFromFile(ARCHIVO_HISTORIAL);
        Log(Format('Cargados %d mensajes.', [Conn.Messages.Count]));
      end
      else
        Log('No hay historial guardado todavía.');
      Log;
    end
    else if SameText(Linea, '/historial') then
      MostrarHistorial
    else
      Preguntar(Linea);
  end;
end;

var
  Driver, Model, Key: string;
var
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  // Tildes y eñes en la consola
  SetConsoleOutputCP(CP_UTF8);
  SetConsoleCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  SetTextCodePage(Input, CP_UTF8);
  {$ENDIF}
  // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes y
  // el streaming se veria de golpe. Con un buffer de 1 byte cada caracter sale al
  // escribirlo (en una consola real ya se vaciaba en cada Write).
  SetTextBuf(Output, BufSalida, 1);
  ConsoleLock := TCriticalSection.Create;
  Oyente := TOyente.Create;
  Conn := TAiChatConnection.Create(nil);
  try
    try
      Driver := ArgValue('--driver', 'OpenAi');
      Model := ArgValue('--model', '');
      Key := ApiKeyFor(Driver);
      if Key.StartsWith('@') and (GetEnvironmentVariable(Key.Substring(1)) = '') then
      begin
        Log('Falta la variable ' + Key.Substring(1) + '. Defínela o usa --driver Ollama.');
        ExitCode := 1;
        Exit;
      end;
      Streaming := not HasFlag('--sin-streaming');

      Conn.DriverName := Driver;
      if Model <> '' then
        Conn.Model := Model;
      if Key <> '' then
        Conn.Params.Values['ApiKey'] := Key;
      Conn.Params.Values['Asynchronous'] := BoolToStr(Streaming, True);
      Conn.Tool_Active := False;   // esta charla no usa herramientas
      Conn.SystemPrompt.Text := PROMPT_SISTEMA;
      Conn.OnReceiveData := Oyente.AlRecibir;
      Conn.OnReceiveDataEnd := Oyente.AlTerminar;
      Conn.OnError := Oyente.AlFallar;

      Log(Format('Chat: %s / %s, %s', [Conn.DriverName, Conn.AiChat.Model,
        IfThen(Streaming, 'con streaming', 'sin streaming')]));
      Log;

      if HasFlag('--interactivo') then
        CharlaLibre
      else
        CharlaGuiada;
    except
      on E: Exception do
        Log(E.ClassName + ': ' + E.Message);
    end;
  finally
    Conn.Free;
    Oyente.Free;
    ConsoleLock.Free;
  end;
end.
