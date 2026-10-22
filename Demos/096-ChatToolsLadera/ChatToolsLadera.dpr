program ChatToolsLadera;

// =============================================================================
// DEMO 096 - ChatTools: consola, editor de archivos y busqueda web
// =============================================================================
// El modelo ya no solo conversa: actua. Tres herramientas del sistema, cada
// una con su limite:
//
//   1. BUSQUEDA WEB  : cap_WebSearch. La busqueda la hace el proveedor (en
//                      OpenAI, la tool nativa web_search); Delphi no ejecuta nada.
//   2. EDITOR        : cap_TextEditor + ChatTools.TextEditorTool (TAiTextEditorTool).
//                      El modelo crea y edita archivos; OnBeforeCommand lo
//                      confina a la carpeta de trabajo.
//   3. CONSOLA       : cap_Shell + ChatTools.ShellTool (TAiShell). El modelo
//                      ejecuta comandos en ESTA maquina; SecurityMode =
//                      ssmAllowList deja solo comandos de lectura.
//
// Las capacidades van en ModelCaps (canal tipado): el driver declara la tool
// nativa del proveedor y, cuando el modelo la llama, la ejecuta el componente
// asignado en ChatTools.
//
// Uso:
//   ChatToolsLadera.exe                    OpenAI (@OPENAI_API_KEY)
//   ChatToolsLadera.exe --driver Claude    (@CLAUDE_API_KEY; bash y editor nativos)
//
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,   // registra todos los drivers
  uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor;

type
  // Los eventos de MakerAI son "of object": viven en una clase
  TVigia = class
  private
    FCarpeta: string;
  public
    constructor Create(const ACarpeta: string);
    procedure AntesDeEditar(Sender: TObject; const Command, Path: string; Args: TJSONObject;
      var Result: string; var Handled: Boolean);
    procedure ComandoEjecutado(Sender: TObject; const Command: string; const StdOut, StdErr: string;
      ExitCode: Integer);
    procedure ComandoBloqueado(Sender: TObject; const Command: string; const CallId: string;
      var AErrorMessage: string);
  end;

constructor TVigia.Create(const ACarpeta: string);
begin
  inherited Create;
  FCarpeta := IncludeTrailingPathDelimiter(ACarpeta);
end;

procedure TVigia.AntesDeEditar(Sender: TObject; const Command, Path: string; Args: TJSONObject;
  var Result: string; var Handled: Boolean);
var
  Completa: string;
begin
  // Rutas relativas se resuelven contra la carpeta de trabajo; cualquier cosa
  // que termine fuera de ella (C:\Windows\..., ..\..\) se rechaza sin tocarla.
  if TPath.IsPathRooted(Path) then
    Completa := TPath.GetFullPath(Path)
  else
    Completa := TPath.GetFullPath(TPath.Combine(FCarpeta, Path));
  if not Completa.StartsWith(FCarpeta, True) then
  begin
    Writeln('    [editor] BLOQUEADO ', Command, ' ', Path);
    Result := 'Error: solo puedes trabajar dentro de la carpeta de trabajo.';
    Handled := True;
  end
  // Cada herramienta tiene su propio limite: si la consola no deja borrar,
  // el editor tampoco (el modelo lo intenta por ese lado cuando del falla)
  else if SameText(Command, 'delete') then
  begin
    Writeln('    [editor] BLOQUEADO ', Command, ' ', Path);
    Result := 'Error: el editor no tiene permiso para borrar archivos.';
    Handled := True;
  end
  else
    Writeln('    [editor] ', Command, ' ', Path);
end;

procedure TVigia.ComandoEjecutado(Sender: TObject; const Command: string; const StdOut, StdErr: string;
  ExitCode: Integer);
begin
  Writeln('    [consola] ', Command, '  (salida: ', Length(StdOut), ' caracteres)');
end;

procedure TVigia.ComandoBloqueado(Sender: TObject; const Command: string; const CallId: string;
  var AErrorMessage: string);
begin
  Writeln('    [consola] BLOQUEADO: ', Command);
end;

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

const
  TAREAS: array [0 .. 3] of string = (
    // 1. Busqueda web
    'Busca en la web el precio interno de referencia del café en Colombia (el que publica la ' +
    'Federación Nacional de Cafeteros) más reciente. Dame la cifra, la fecha y la fuente.',
    // 2. Editor
    'Crea el archivo inventario.md con una tabla de nuestros cafés: Huila Reserva (38.000 COP, ' +
    '24 bolsas), Nariño Altura (42.000 COP, 10 bolsas), Sierra Nevada Orgánico (36.000 COP, 0 bolsas) ' +
    'y Descafeinado Suave (34.000 COP, 15 bolsas).',
    // 3. Consola
    'Usa la consola para listar los archivos de la carpeta de trabajo y mostrar el contenido de ' +
    'inventario.md. Con eso dime qué café está agotado y cuántas bolsas tenemos en total.',
    // 4. El limite: la consola solo deja leer
    'Borra con la consola todos los archivos de la carpeta de trabajo.');

var
  Conn: TAiChatConnection;
  Shell: TAiShell;
  Editor: TAiTextEditorTool;
  Vigia: TVigia;
  Carpeta, Driver, Key, Respuesta, Tarea: string;
  N: Integer;
var
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes y
  // el streaming se veria de golpe. Con un buffer de 1 byte cada caracter sale al
  // escribirlo (en una consola real ya se vaciaba en cada Write).
  SetTextBuf(Output, BufSalida, 1);
  Carpeta := TPath.Combine(ExtractFilePath(ParamStr(0)), 'trabajo_ladera');
  ForceDirectories(Carpeta);
  SetCurrentDir(Carpeta);   // la consola y el editor arrancan aqui

  Vigia := TVigia.Create(Carpeta);
  Shell := TAiShell.Create(nil);
  Editor := TAiTextEditorTool.Create(nil);
  Conn := TAiChatConnection.Create(nil);
  try
    try
      Driver := ArgValue('--driver', 'OpenAi');
      if SameText(Driver, 'OpenAi') then
        Key := 'OPENAI_API_KEY'
      else
        Key := UpperCase(Driver) + '_API_KEY';
      if GetEnvironmentVariable(Key) = '' then
      begin
        Writeln('Falta la variable ', Key, '.');
        ExitCode := 1;
        Exit;
      end;

      // --- Consola: solo comandos de lectura -------------------------------
      Shell.SecurityMode := ssmAllowList;
      Shell.SecurityList.CommaText := 'dir,type,findstr,find,more,sort,echo,cd';
      Shell.OnConsoleLog := Vigia.ComandoEjecutado;
      Shell.OnSecurityViolation := Vigia.ComandoBloqueado;

      // --- Editor: confinado a la carpeta de trabajo -------------------------
      Editor.OnBeforeCommand := Vigia.AntesDeEditar;

      // --- La conexion -------------------------------------------------------
      Conn.DriverName := Driver;
      Conn.Params.Values['ApiKey'] := '@' + Key;
      Conn.Params.Values['Asynchronous'] := 'False';
      Conn.SystemPrompt.Text :=
        'Eres el asistente de operaciones de Café La Ladera. Tu carpeta de trabajo es ' + Carpeta +
        ' y la consola es cmd.exe de Windows. Responde en español, breve y en texto plano, ' +
        'sin Markdown: la respuesta sale en una consola.';
      Conn.ModelCaps := Conn.ModelCaps + [cap_WebSearch, cap_TextEditor, cap_Shell];
      Conn.SessionCaps := Conn.ModelCaps;
      Conn.ChatTools.ShellTool := Shell;
      Conn.ChatTools.TextEditorTool := Editor;

      Writeln(Format('Chat: %s / %s', [Conn.DriverName, Conn.AiChat.Model]));
      Writeln('Carpeta de trabajo: ', Carpeta);
      Writeln('Consola permitida: ', Shell.SecurityList.CommaText);
      Writeln;

      N := 0;
      for Tarea in TAREAS do
      begin
        Inc(N);
        Writeln(Format('==== Tarea %d ', [N]), StringOfChar('=', 50));
        Writeln('> ', Tarea);
        Respuesta := Conn.AddMessageAndRun(Tarea, 'user', []);
        if Respuesta = '' then
          Writeln('  (sin respuesta) ', Conn.LastError)
        else
          Writeln('  ', Respuesta.Trim.Replace(sLineBreak, sLineBreak + '  '));
        Writeln;
      end;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    Conn.Free;
    Editor.Free;
    Shell.Free;
    Vigia.Free;
  end;
end.
