program HerramientasSistema;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI -- Capitulo 18: Herramientas del Sistema
// =============================================================================
// TAiShell: el LLM ejecuta comandos en una sesion persistente de shell.
// TAiTextEditorTool: el LLM lee y edita archivos de forma controlada.
//
// Variables de entorno requeridas:
//   CLAUDE_API_KEY   -- Demos 1, 3, 4
//   (Ollama local)   -- Demo 2 (qwen3.5:2b debe estar corriendo en Ollama)
//
// Carpeta de trabajo: C:\temp\demoshell  (se crea automaticamente)
//
// Diferencia clave con ChatTools:
//   ChatTools    -> el framework los invoca de forma transparente al LLM
//   Estas tools  -> el LLM las DESCUBRE en su lista y decide cuando llamarlas
// =============================================================================

uses
  System.SysUtils, System.Classes, System.IOUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor,
  uMakerAi.Chat.Initializations;

const
  WORK_DIR   = 'C:\temp\demoshell';
  CLAUDE_KEY = '@CLAUDE_API_KEY';
  CLAUDE_DRV = 'Claude';
  CLAUDE_MDL = 'claude-sonnet-4-6';
  OLLAMA_DRV = 'Ollama';
  OLLAMA_MDL = 'qwen3.5:2b';  // 2.3B -- ligero y confiable en tool calling

// -----------------------------------------------------------------------------

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

// =============================================================================
//  TSistemaDemo
//
//  Clase contenedora de los 4 demos. Cada demo crea y libera sus propios
//  objetos; la clase solo existe para proveer handlers of object a los tools.
// =============================================================================
type
  TSistemaDemo = class
  private
    FTextEditor: TAiTextEditorTool;  // accedido desde OnTextEditorAction

    // Handlers de tool calls (deben ser metodos de clase -- TFunctionEvent of object)
    procedure OnTextEditorAction(Sender: TObject; FA: TFunctionActionItem;
      FN: String; TC: TAiToolsFunction; var Handled: Boolean);
    procedure OnShellLog(Sender: TObject;
      const Command, StdOut, StdErr: string; ExitCode: Integer);

    // Helpers
    procedure PrepararCarpeta;
    procedure RegistrarTextEditor(LFuncs: TAiFunctions);

    // Demos
    procedure Demo1_Shell_Claude;
    procedure Demo2_Shell_Ollama;
    procedure Demo3_TextEditor_Claude;
    procedure Demo4_Combinado;
  public
    procedure Run;
  end;

// =============================================================================
//  Handlers
// =============================================================================

procedure TSistemaDemo.OnTextEditorAction(Sender: TObject;
  FA: TFunctionActionItem; FN: String; TC: TAiToolsFunction;
  var Handled: Boolean);
begin
  WriteLn(Format('  [TextEditor] %s -> %s',
    [TC.Params.Values['command'], TC.Params.Values['path']]));
  TC.Response := FTextEditor.Execute(TC.Arguments);
  Handled := True;
end;

procedure TSistemaDemo.OnShellLog(Sender: TObject;
  const Command, StdOut, StdErr: string; ExitCode: Integer);
var
  LOut: string;
begin
  WriteLn('  [bash] $ ', Trim(Command));
  LOut := Trim(StdOut);
  if LOut <> '' then
  begin
    if Length(LOut) > 300 then
      LOut := Copy(LOut, 1, 300) + '...';
    WriteLn('  ',
      StringReplace(LOut, #10, #13#10 + '  ', [rfReplaceAll]));
  end;
end;

// =============================================================================
//  PrepararCarpeta: crea C:\temp\demoshell y tres archivos de prueba
// =============================================================================

procedure TSistemaDemo.PrepararCarpeta;
const
  // LF puro (#10): TAiTextEditorTool y los LLM trabajan con \n.
  // CRLF (#13#10) hace que str_replace falle porque el LLM reconstruye
  // old_str con \n y el archivo tiene \r\n — no coinciden.
  FIB_PAS =
    'function Fibonacci(N: Integer): Integer;'#10 +
    'begin'#10 +
    '  if N <= 1 then'#10 +
    '    Result := N'#10 +
    '  else'#10 +
    '    Result := Fibonacci(N - 1) + Fibonacci(N - 2);'#10 +
    '  // TODO: validar valores negativos'#10 +
    'end;'#10;

  CONFIG_TXT =
    'host=localhost'#10 +
    'port=5432'#10 +
    'database=produccion'#10 +
    'user=admin'#10 +
    'timeout=30'#10;

  NOTAS_TXT =
    'Reunion equipo - 17 mayo 2026'#10 +
    'Temas pendientes:'#10 +
    '- Revisar modulo facturacion'#10 +
    '- Actualizar dependencias'#10 +
    '- Documentar API REST'#10;
begin
  TDirectory.CreateDirectory(WORK_DIR);
  TFile.WriteAllText(WORK_DIR + '\fibonacci.pas', FIB_PAS,   TEncoding.UTF8);
  TFile.WriteAllText(WORK_DIR + '\config.txt',    CONFIG_TXT, TEncoding.UTF8);
  TFile.WriteAllText(WORK_DIR + '\notas.txt',     NOTAS_TXT,  TEncoding.UTF8);

  WriteLn('  Carpeta de trabajo: ', WORK_DIR);
  WriteLn('  Archivos creados  : fibonacci.pas, config.txt, notas.txt');
end;

// =============================================================================
//  RegistrarTextEditor: define la funcion text_editor en TAiFunctions
// =============================================================================

procedure TSistemaDemo.RegistrarTextEditor(LFuncs: TAiFunctions);
var
  LFn   : TFunctionActionItem;
  LParam: TFunctionParamsItem;
begin
  LFn := LFuncs.Functions.AddFunction('text_editor', True, OnTextEditorAction);
  LFn.Description.Text :=
    'Lee y edita archivos de texto. ' +
    'view: muestra el contenido. ' +
    'create: crea un archivo nuevo con file_text. ' +
    'str_replace: reemplaza old_str por new_str (old_str debe ser unico en el archivo). ' +
    'insert: inserta new_str despues de la linea insert_line. ' +
    'apply_diff: aplica un diff unificado.';

  LParam := LFn.Parameters.Add;
  LParam.Name := 'command';
  LParam.Description.Text := 'Accion: view | create | str_replace | insert | apply_diff';
  LParam.ParamType := ptString;
  LParam.Required  := True;

  LParam := LFn.Parameters.Add;
  LParam.Name := 'path';
  LParam.Description.Text := 'Ruta completa del archivo';
  LParam.ParamType := ptString;
  LParam.Required  := True;

  LParam := LFn.Parameters.Add;
  LParam.Name := 'file_text';
  LParam.Description.Text := 'Contenido completo del archivo (solo para create)';
  LParam.ParamType := ptString;
  LParam.Required  := False;

  LParam := LFn.Parameters.Add;
  LParam.Name := 'old_str';
  LParam.Description.Text :=
    'Fragmento EXACTO a reemplazar (para str_replace). Debe ser unico en el archivo.';
  LParam.ParamType := ptString;
  LParam.Required  := False;

  LParam := LFn.Parameters.Add;
  LParam.Name := 'new_str';
  LParam.Description.Text :=
    'Texto de reemplazo (para str_replace) o texto a insertar (para insert)';
  LParam.ParamType := ptString;
  LParam.Required  := False;

  LParam := LFn.Parameters.Add;
  LParam.Name := 'insert_line';
  LParam.Description.Text := 'Numero de linea despues de la cual insertar (para insert)';
  LParam.ParamType := ptInteger;
  LParam.Required  := False;
end;

// =============================================================================
//  Demo 1 -- TAiShell + Claude
//
//  El LLM usa bash para explorar el directorio. SecurityMode=ssmAllowList
//  restringe los comandos a un conjunto seguro definido en SecurityList.
//  La sesion de shell es persistente: cd seguido de dir funciona como esperado.
// =============================================================================
procedure TSistemaDemo.Demo1_Shell_Claude;
var
  LFuncs: TAiFunctions;
  LShell: TAiShell;
  Conn  : TAiChatConnection;
  Resp  : string;
begin
  Separador('Demo 1 -- TAiShell + Claude: explorar y leer archivos');

  LFuncs := TAiFunctions.Create(nil);
  LShell := TAiShell.Create(nil);
  Conn   := TAiChatConnection.Create(nil);
  try
    LShell.SecurityMode := ssmAllowList;
    LShell.SecurityList.CommaText := 'dir,type,tree,echo,cd';
    LShell.OnConsoleLog := OnShellLog;
    LShell.RegisterInFunctions(LFuncs, 'bash');

    Conn.DriverName := CLAUDE_DRV;
    Conn.Params.Values['ApiKey']       := CLAUDE_KEY;
    Conn.Model                         := CLAUDE_MDL;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Tool_Active']  := 'True';
    Conn.Params.Values['Max_Tokens']   := '1024';
    Conn.AiFunctions := LFuncs;

    Conn.SystemPrompt.Text :=
      'Eres un asistente de sistema. Ejecutas comandos Windows con bash. ' +
      'Trabaja SOLO en ' + WORK_DIR + '. ' +
      'Comandos permitidos: dir, type, tree, echo, cd. ' +
      'Responde siempre en espanol.';

    WriteLn('  Tarea: listar archivos y mostrar el contenido de fibonacci.pas');
    WriteLn('');

    Resp := Conn.AddMessageAndRun(
      'Usa dir para listar los archivos de ' + WORK_DIR +
      ' y despues usa type para mostrar el contenido de fibonacci.pas. ' +
      'Resume en dos o tres lineas lo que encontraste.',
      'user', []);

    WriteLn('');
    WriteLn('  Claude respondio:');
    WriteLn('  ', StringReplace(Trim(Resp), #10, #13#10 + '  ', [rfReplaceAll]));
  finally
    Conn.Free;
    LShell.Free;
    LFuncs.Free;
  end;

  WriteLn('');
  WriteLn('-> SecurityMode=ssmAllowList: solo comandos de la lista estan permitidos.');
  WriteLn('   La sesion es persistente: cd y dir en llamadas separadas funcionan.');
  WriteLn('   RegisterInFunctions registra bash con un solo metodo call.');
end;

// =============================================================================
//  Demo 2 -- TAiShell + Ollama/qwen3.5:2b  (100% local)
//
//  El mismo TAiShell funciona con Ollama sin cambios. La unica diferencia
//  es DriverName y Model. qwen3.5:2b es un modelo local de 2.3B parametros
//  con soporte confiable de tool calling via Ollama.
//
//  Nota: qwen3.5 no esta en Initializations -> Tool_Active debe activarse
//  explicitamente en el codigo. Modelos mas grandes (qwen3.5:7b, etc.)
//  son mas confiables pero requieren mas VRAM.
// =============================================================================
procedure TSistemaDemo.Demo2_Shell_Ollama;
var
  LFuncs: TAiFunctions;
  LShell: TAiShell;
  Conn  : TAiChatConnection;
  Resp  : string;
begin
  Separador('Demo 2 -- TAiShell + Ollama/qwen3.5:2b (sin internet, sin API key)');

  LFuncs := TAiFunctions.Create(nil);
  LShell := TAiShell.Create(nil);
  Conn   := TAiChatConnection.Create(nil);
  try
    LShell.SecurityMode := ssmAllowList;
    LShell.SecurityList.CommaText := 'dir,type,echo';
    LShell.OnConsoleLog := OnShellLog;
    LShell.RegisterInFunctions(LFuncs, 'bash');

    // La unica diferencia con Demo 1: DriverName y Model
    Conn.DriverName := OLLAMA_DRV;
    Conn.Model      := OLLAMA_MDL;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Tool_Active']  := 'True';  // qwen3.5 no esta en Initializations -> activar explicitamente
    Conn.Params.Values['Max_Tokens']   := '1024';
    Conn.AiFunctions := LFuncs;

    Conn.SystemPrompt.Text :=
      'Eres un asistente de sistema. Usa bash para ejecutar comandos Windows. ' +
      'Trabaja solo en ' + WORK_DIR + '. ' +
      'Comandos disponibles: dir, type. Responde en espanol.';

    WriteLn('  Driver : ', OLLAMA_DRV, '   Modelo : ', OLLAMA_MDL);
    WriteLn('  Tarea  : listar los archivos en ', WORK_DIR);
    WriteLn('  (El codigo de TAiShell es identico al Demo 1)');
    WriteLn('');

    Resp := Conn.AddMessageAndRun(
      'Usa dir para listar los archivos en ' + WORK_DIR +
      ' e indica cuantos archivos encontraste.',
      'user', []);

    WriteLn('');
    WriteLn('  qwen3.5:2b respondio:');
    WriteLn('  ', StringReplace(Trim(Resp), #10, #13#10 + '  ', [rfReplaceAll]));
  finally
    Conn.Free;
    LShell.Free;
    LFuncs.Free;
  end;

  WriteLn('');
  WriteLn('-> Mismo TAiShell, distinto driver: solo cambia DriverName y Model.');
  WriteLn('   qwen3.5:2b no esta en Initializations -> Tool_Active se activa en el codigo.');
  WriteLn('   100% local: sin internet, sin API key, sin costo por llamada.');
end;

// =============================================================================
//  Demo 3 -- TAiTextEditorTool + Claude
//
//  TAiTextEditorTool no tiene RegisterInFunctions propio: se registra
//  manualmente en TAiFunctions a traves de RegistrarTextEditor.
//  El handler OnTextEditorAction delega en FTextEditor.Execute(TC.Arguments).
//
//  Claude usa str_replace para editar con precision quirurgica: old_str
//  debe coincidir de forma EXACTA y UNICA en el archivo.
// =============================================================================
procedure TSistemaDemo.Demo3_TextEditor_Claude;
var
  LFuncs  : TAiFunctions;
  Conn    : TAiChatConnection;
  Resp    : string;
  Contenido: string;
begin
  Separador('Demo 3 -- TAiTextEditorTool + Claude: leer y editar codigo');

  LFuncs      := TAiFunctions.Create(nil);
  FTextEditor := TAiTextEditorTool.Create(nil);
  Conn        := TAiChatConnection.Create(nil);
  try
    RegistrarTextEditor(LFuncs);

    Conn.DriverName := CLAUDE_DRV;
    Conn.Params.Values['ApiKey']       := CLAUDE_KEY;
    Conn.Model                         := CLAUDE_MDL;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Tool_Active']  := 'True';
    Conn.Params.Values['Max_Tokens']   := '2048';
    Conn.AiFunctions := LFuncs;

    Conn.SystemPrompt.Text :=
      'Eres un asistente de desarrollo Delphi experto. ' +
      'Usas text_editor para leer y editar archivos. ' +
      'Trabaja solo en ' + WORK_DIR + '. ' +
      'Responde siempre en espanol.';

    WriteLn('  Tarea: leer fibonacci.pas y agregar validacion para N < 0');
    WriteLn('');

    Resp := Conn.AddMessageAndRun(
      'Lee el archivo ' + WORK_DIR + '\fibonacci.pas con view. ' +
      'Luego usa str_replace para agregar al inicio del cuerpo de la funcion ' +
      'un guard que retorne -1 cuando N < 0. ' +
      'Indica exactamente que cambio realizaste.',
      'user', []);

    WriteLn('');
    WriteLn('  Claude respondio:');
    WriteLn('  ', StringReplace(Trim(Resp), #10, #13#10 + '  ', [rfReplaceAll]));

    WriteLn('');
    WriteLn('  fibonacci.pas despues de la edicion:');
    WriteLn('  ' + StringOfChar('-', 50));
    Contenido := TFile.ReadAllText(WORK_DIR + '\fibonacci.pas', TEncoding.UTF8);
    WriteLn('  ', StringReplace(Trim(Contenido), #10, #13#10 + '  ', [rfReplaceAll]));
    WriteLn('  ' + StringOfChar('-', 50));
  finally
    Conn.Free;
    FTextEditor.Free;
    FTextEditor := nil;
    LFuncs.Free;
  end;

  WriteLn('');
  WriteLn('-> TAiTextEditorTool no tiene RegisterInFunctions: se registra manualmente.');
  WriteLn('   str_replace exige que old_str sea UNICO en el archivo.');
  WriteLn('   Si Claude no puede garantizarlo, usa insert o apply_diff.');
end;

// =============================================================================
//  Demo 4 -- TAiShell + TAiTextEditorTool combinados
//
//  Dos herramientas en un solo TAiFunctions. El LLM elige cual usar en cada
//  paso: bash para navegar y verificar; text_editor para leer y modificar.
//  Este patron es la base de los agentes de codigo del capitulo 26.
// =============================================================================
procedure TSistemaDemo.Demo4_Combinado;
var
  LFuncs: TAiFunctions;
  LShell: TAiShell;
  Conn  : TAiChatConnection;
  Resp  : string;
begin
  Separador('Demo 4 -- Shell + TextEditor: agente de codigo combinado');

  LFuncs      := TAiFunctions.Create(nil);
  LShell      := TAiShell.Create(nil);
  FTextEditor := TAiTextEditorTool.Create(nil);
  Conn        := TAiChatConnection.Create(nil);
  try
    // Registrar ambas herramientas en el mismo TAiFunctions
    LShell.SecurityMode := ssmAllowList;
    LShell.SecurityList.CommaText := 'dir,type,echo';
    LShell.OnConsoleLog := OnShellLog;
    LShell.RegisterInFunctions(LFuncs, 'bash');
    RegistrarTextEditor(LFuncs);

    Conn.DriverName := CLAUDE_DRV;
    Conn.Params.Values['ApiKey']       := CLAUDE_KEY;
    Conn.Model                         := CLAUDE_MDL;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Tool_Active']  := 'True';
    Conn.Params.Values['Max_Tokens']   := '2048';
    Conn.AiFunctions := LFuncs;

    Conn.SystemPrompt.Text :=
      'Eres un agente de desarrollo con dos herramientas: ' +
      'bash (ejecuta comandos Windows: dir, type, echo) y ' +
      'text_editor (lee y edita archivos). ' +
      'Trabaja solo en ' + WORK_DIR + '. Responde siempre en espanol.';

    WriteLn('  Tarea multi-paso:');
    WriteLn('    1. Listar archivos (bash)');
    WriteLn('    2. Leer config.txt (text_editor)');
    WriteLn('    3. Cambiar timeout=30 por timeout=60 (str_replace)');
    WriteLn('    4. Crear resumen.txt con lo encontrado (create)');
    WriteLn('');

    Resp := Conn.AddMessageAndRun(
      'Realiza estas tareas en orden:'#13#10 +
      '1. Usa bash (dir) para listar los archivos de ' + WORK_DIR + '.'#13#10 +
      '2. Usa text_editor (view) para leer config.txt.'#13#10 +
      '3. Cambia timeout=30 por timeout=60 con str_replace.'#13#10 +
      '4. Crea el archivo ' + WORK_DIR + '\resumen.txt con un breve resumen ' +
      'de los archivos que encontraste y el cambio que realizaste.',
      'user', []);

    WriteLn('');
    WriteLn('  Claude respondio:');
    WriteLn('  ', StringReplace(Trim(Resp), #10, #13#10 + '  ', [rfReplaceAll]));

    if TFile.Exists(WORK_DIR + '\resumen.txt') then
    begin
      WriteLn('');
      WriteLn('  resumen.txt generado:');
      WriteLn('  ', StringReplace(
        Trim(TFile.ReadAllText(WORK_DIR + '\resumen.txt', TEncoding.UTF8)),
        #10, #13#10 + '  ', [rfReplaceAll]));
    end;
  finally
    Conn.Free;
    FTextEditor.Free;
    FTextEditor := nil;
    LShell.Free;
    LFuncs.Free;
  end;

  WriteLn('');
  WriteLn('-> Dos herramientas en un TAiFunctions: el LLM elige cual usar en cada paso.');
  WriteLn('   Este patron (shell + editor) es la base de los agentes del cap. 26.');
end;

// =============================================================================
//  Run
// =============================================================================
procedure TSistemaDemo.Run;
var
  TieneClaude: Boolean;
begin
  WriteLn('=== MakerAI -- Capitulo 18: Herramientas del Sistema ===');
  WriteLn('');

  TieneClaude := GetEnvironmentVariable('CLAUDE_API_KEY') <> '';

  WriteLn('  Estado del entorno:');
  if TieneClaude then WriteLn('  [OK] CLAUDE_API_KEY detectado')
  else            WriteLn('  [--] CLAUDE_API_KEY no configurado -- demos 1, 3, 4 omitidos');
  WriteLn('  [??] Ollama: se detectara al ejecutar Demo 2 (qwen3.5:2b)');
  WriteLn('');

  PrepararCarpeta;

  if TieneClaude then
  begin
    try Demo1_Shell_Claude;
    except on E: Exception do WriteLn('  [Demo 1 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else WriteLn(#13#10'--- Demo 1 omitido (CLAUDE_API_KEY no configurado) ---');

  try Demo2_Shell_Ollama;
  except on E: Exception do WriteLn('  [Demo 2 ERROR] ', E.ClassName, ': ', E.Message); end;

  if TieneClaude then
  begin
    try Demo3_TextEditor_Claude;
    except on E: Exception do WriteLn('  [Demo 3 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else WriteLn(#13#10'--- Demo 3 omitido (CLAUDE_API_KEY no configurado) ---');

  if TieneClaude then
  begin
    try Demo4_Combinado;
    except on E: Exception do WriteLn('  [Demo 4 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else WriteLn(#13#10'--- Demo 4 omitido (CLAUDE_API_KEY no configurado) ---');

  WriteLn('');
  WriteLn(StringOfChar('=', 62));
  WriteLn('  Demos completados.');
  WriteLn('  Ver cap18-herramientas-sistema.md para la documentacion completa.');
  WriteLn(StringOfChar('=', 62));
end;

// =============================================================================
begin
  try
    with TSistemaDemo.Create do
    try
      Run;
    finally
      Free;
    end;
  except
    on E: Exception do
      WriteLn('ERROR FATAL: ', E.ClassName, ' -- ', E.Message);
  end;
  WriteLn('');
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
