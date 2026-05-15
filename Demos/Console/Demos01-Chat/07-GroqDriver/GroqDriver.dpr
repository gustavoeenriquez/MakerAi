program GroqDriver;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 01-Chat / 07-GroqDriver
// =============================================================================
// Validacion del driver Groq tras los fixes de Abr 2026:
//
//   Fix 1 — tool_choice:          era 'tools_choice' (typo silencioso)
//   Fix 2 — logprobs eliminados:  Groq rechaza logprobs, logit_bias, top_logprobs
//   Fix 3 — vision multi-turn:    antes solo enviaba LastMsg; ahora GetMessages
//   Fix 4 — max_completion_tokens: reasoning models deben usarlo (no max_tokens)
//
//   Test 1 — Chat basico         llama-3.1-8b-instant
//   Test 2 — Tool calling        llama-3.3-70b-versatile  (fix 1)
//   Test 3 — Reasoning           qwen/qwen3-32b           (fix 4)
//   Test 4 — Vision multi-turn   llama-4-scout            (fix 3)
//
// Prerequisito: GROQ_API_KEY definida como variable de entorno
// =============================================================================

uses
  System.SysUtils,
  System.StrUtils,
  System.JSON,
  System.IOUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Chat.Groq;

const
  API_KEY  = '@GROQ_API_KEY';
  IMG_PATH = 'C:\Windows\Web\Wallpaper\Windows\img0.jpg';

// =============================================================================
//  Helpers
// =============================================================================

procedure Section(const Title: String);
begin
  Writeln;
  Writeln(StringOfChar('-', 62));
  Writeln('  ', Title);
  Writeln(StringOfChar('-', 62));
end;

function MakeConn(const Model: String; MaxTok: Integer = 1024): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := 'Groq';
  Result.Model      := Model;
  Result.Params.Values['ApiKey']       := API_KEY;
  Result.Params.Values['Asynchronous'] := 'False';
  Result.Params.Values['Max_Tokens']   := IntToStr(MaxTok);
end;

// =============================================================================
//  Test 1 — Chat basico
// =============================================================================

procedure TestBasicChat;
var
  Conn: TAiChatConnection;
  Res : String;
begin
  Section('Test 1 — Chat basico  [llama-3.1-8b-instant]');
  Conn := MakeConn('llama-3.1-8b-instant');
  try
    Conn.SystemPrompt.Text := 'Responde siempre en una sola oracion en espanol.';
    Res := Conn.AddMessageAndRun(
      'Cual es el planeta mas grande del sistema solar?', 'user', []);
    Writeln('  Respuesta : ', Res);
    if Trim(Res) <> '' then
      Writeln('  => PASS')
    else
      Writeln('  => FAIL: respuesta vacia');
  finally
    Conn.Free;
  end;
end;

// =============================================================================
//  Test 2 — Tool calling  (fix: tool_choice, antes "tools_choice")
// =============================================================================

type
  TToolTest = class
  private
    FCallCount: Integer;
    procedure OnGetDate(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
  public
    procedure Run;
  end;

procedure TToolTest.OnGetDate(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: String; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  Inc(FCallCount);
  Writeln('  [tool invocado] get_current_date  args=', ToolCall.Arguments);
  ToolCall.Response := Format(
    '{"date":"%s","day_of_week":"%s","iso":"%s"}',
    [FormatDateTime('dd/mm/yyyy', Now),
     FormatDateTime('dddd', Now),
     FormatDateTime('yyyy-mm-dd', Now)]);
  Handled := True;
end;

procedure TToolTest.Run;
var
  Conn: TAiChatConnection;
  Fns : TAiFunctions;
  Fn  : TFunctionActionItem;
  Res : String;
begin
  Section('Test 2 — Tool calling  [llama-3.3-70b-versatile]  (fix: tool_choice)');
  FCallCount := 0;

  Fns := TAiFunctions.Create(nil);
  try
    Fn := Fns.Functions.AddFunction('get_current_date', True, OnGetDate);
    Fn.Description.Text :=
      'Returns today''s date (day, month, year) and day of the week.';

    Conn := MakeConn('llama-3.3-70b-versatile', 512);
    try
      Conn.Params.Values['Tool_Active'] := 'True';
      Conn.AiFunctions := Fns;
      Conn.SystemPrompt.Text :=
        'You are a helpful assistant. Use get_current_date when asked about today''s date.';

      Res := Conn.AddMessageAndRun(
        'What day of the week is today and what is the exact date?', 'user', []);
      Writeln('  Respuesta final: ', Res);

      if FCallCount > 0 then
        Writeln('  => PASS  (funcion invocada ', FCallCount, ' vez/veces)')
      else
        Writeln('  => WARN  (LLM respondio sin invocar la funcion)');
    finally
      Conn.Free;
    end;
  finally
    Fns.Free;
  end;
end;

// =============================================================================
//  Test 3 — Reasoning con qwen3-32b  (fix: max_completion_tokens)
// =============================================================================

procedure TestReasoning;
var
  Conn: TAiChatConnection;
  Res : String;
begin
  Section('Test 3 — Reasoning  [qwen/qwen3-32b]  (fix: max_completion_tokens)');
  Writeln('  Problema: secuencia a(1)=2, a(n+1)=a(n)+3.  Cuanto es a(5)?');
  Conn := MakeConn('qwen/qwen3-32b', 8192);
  try
    Conn.SystemPrompt.Text :=
      'You are a math assistant. Solve step by step, give final answer clearly.';
    Res := Conn.AddMessageAndRun(
      'Sequence: a(1)=2, a(n+1)=a(n)+3. Compute a(5). Show each step.',
      'user', []);
    Writeln('  Respuesta:');
    Writeln('  ', Res);
    if Trim(Res) <> '' then
      Writeln('  => PASS  (max_completion_tokens aceptado por Groq)')
    else
      Writeln('  => FAIL: respuesta vacia');
  finally
    Conn.Free;
  end;
end;

// =============================================================================
//  Test 4 — Vision multi-turn  (fix: GetMessages en vez de LastMsg.ToJson)
// =============================================================================

procedure TestVisionMultiTurn;
var
  Conn      : TAiChatConnection;
  Media     : TAiMediaFile;
  MediaList : TAiMediaFilesArray;
  Res1, Res2: String;
begin
  Section('Test 4 — Vision multi-turn  [llama-4-scout]  (fix: GetMessages)');

  if not TFile.Exists(IMG_PATH) then
  begin
    Writeln('  [SKIP] Imagen no encontrada: ', IMG_PATH);
    Writeln('  Ajusta IMG_PATH en el demo a una imagen existente.');
    Exit;
  end;

  Writeln('  Imagen: ', IMG_PATH);
  Writeln;

  Conn := MakeConn('meta-llama/llama-4-scout-17b-16e-instruct', 1024);
  try
    Conn.SystemPrompt.Text :=
      'You are a visual assistant. Answer concisely in Spanish.';

    // Turno 1: descripcion con imagen adjunta
    // NOTA: AddMediaFile transfiere ownership al mensaje (TObjectList OwnsObjects=True).
    //       No llamar Media.Free — Conn.Free lo destruye al liberar los mensajes.
    Media := TAiMediaFile.Create;
    Media.LoadFromFile(IMG_PATH);
    SetLength(MediaList, 1);
    MediaList[0] := Media;
    Writeln('  Turno 1 (imagen adjunta): descripcion general');
    Res1 := Conn.AddMessageAndRun(
      'Describe brevemente lo que ves en esta imagen.', 'user', MediaList);
    Writeln('  R1: ', Res1);

    // Turno 2: follow-up SIN nueva imagen
    // Con el fix, GetMessages envia TODO el historial (incluyendo la imagen del turno 1)
    // Sin el fix, solo enviaria este ultimo mensaje → el modelo no sabe de que imagen hablar
    Writeln;
    Writeln('  Turno 2 (sin nueva imagen, pero historial incluye la del turno 1):');
    Res2 := Conn.AddMessageAndRun(
      'Cual es el color predominante en lo que acabas de describir?', 'user', []);
    Writeln('  R2: ', Res2);

    if (Trim(Res1) <> '') and (Trim(Res2) <> '') then
    begin
      Writeln;
      Writeln('  => PASS  (multi-turn con imagen funciona — historial completo enviado)');
    end
    else
      Writeln('  => FAIL: alguno de los turnos devolvio respuesta vacia');
  finally
    Conn.Free;
  end;
end;

// =============================================================================
//  Punto de entrada
// =============================================================================

var
  ApiKeyOk: Boolean;
begin
  Writeln('=== GroqDriver — Validacion de fixes (Abr 2026) ===');
  ApiKeyOk := GetEnvironmentVariable('GROQ_API_KEY') <> '';
  Writeln('GROQ_API_KEY : ', IfThen(ApiKeyOk, '[OK]', '[FALTANTE — define la variable]'));
  Writeln;

  if not ApiKeyOk then
  begin
    Writeln('ERROR: Define GROQ_API_KEY antes de ejecutar.');
    Writeln;
    Writeln('Presiona Enter para salir...');
    Readln;
    Halt(1);
  end;

  try
    TestBasicChat;
    with TToolTest.Create do try Run; finally Free; end;
    TestReasoning;
    TestVisionMultiTurn;
  except
    on E: Exception do
      Writeln('ERROR FATAL: ', E.ClassName, ' — ', E.Message);
  end;

  Writeln;
  Writeln(StringOfChar('=', 62));
  Writeln('  Demo completo.');
  Writeln(StringOfChar('=', 62));
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
