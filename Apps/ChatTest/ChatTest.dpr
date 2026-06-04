program ChatTest;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils, System.Classes,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Initializations;

// ---------------------------------------------------------------------------
// Imprime la lista de mensajes en FConn indicando si tienen reasoning_content
// ---------------------------------------------------------------------------
procedure PrintMessages(Conn: TAiChatConnection; const Caption: string);
var
  I  : Integer;
  Msg: TAiChatMessage;
begin
  Writeln(Caption);
  for I := 0 to Conn.Messages.Count - 1 do
  begin
    Msg := Conn.Messages[I];
    Write('  [', I, '] role=', Msg.Role, '  text=',
          Copy(Msg.Prompt, 1, 60));
    if Length(Msg.Prompt) > 60 then Write('...');
    if Msg.ReasoningContent <> '' then
      Writeln('  *** HAS reasoning_content (', Length(Msg.ReasoningContent), ' chars) ***')
    else
      Writeln('  (clean)');
  end;
  Writeln;
end;

// ---------------------------------------------------------------------------
// Reconstruye el historial de FConn usando solo Role+Prompt (sin metadata)
// Replica exactamente lo que hace RebuildConnectionHistory en TChatBridge
// ---------------------------------------------------------------------------
procedure RebuildHistory(Conn: TAiChatConnection);
var
  I    : Integer;
  Roles: TStringList;
  Texts: TStringList;
begin
  Roles := TStringList.Create;
  Texts := TStringList.Create;
  try
    // 1. Snapshot antes de limpiar
    for I := 0 to Conn.Messages.Count - 1 do
    begin
      Roles.Add(Conn.Messages[I].Role);
      Texts.Add(Conn.Messages[I].Prompt);
    end;

    // 2. Limpiar historial del driver
    Conn.NewChat;

    // 3. Re-agregar con mensajes limpios (sin ReasoningContent)
    for I := 0 to Roles.Count - 1 do
      Conn.AddMessage(Texts[I], Roles[I]);
  finally
    Roles.Free;
    Texts.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Test principal
// ---------------------------------------------------------------------------
procedure RunTest(const AProvider, AModel, AApiKey: string);
var
  Conn: TAiChatConnection;
  Last: TAiChatMessage;
begin
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName                    := AProvider;
    Conn.Model                         := AModel;
    Conn.Params.Values['ApiKey']       := AApiKey;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Tool_Active']  := 'False';
    Conn.SystemPrompt.Text             := 'You are a helpful assistant. Be brief.';

    Writeln('--- Turn 1: "Hola, como te llamas?" ---');
    Conn.AddMessage('Hola, como te llamas?', 'user');
    Conn.Run;

    Last := Conn.GetLastMessage;
    if Assigned(Last) then
    begin
      Writeln('Response: ', Copy(Last.Prompt, 1, 120));
      if Last.ReasoningContent <> '' then
        Writeln('  [reasoning_content present: ', Length(Last.ReasoningContent), ' chars]');
    end;
    Writeln;

    PrintMessages(Conn, 'Messages after turn 1:');

    // Simular RebuildConnectionHistory
    Writeln('--- Rebuilding history (stripping reasoning_content) ---');
    RebuildHistory(Conn);
    PrintMessages(Conn, 'Messages after rebuild:');

    Writeln('--- Turn 2: "Mi nombre es Gustavo" ---');
    Conn.AddMessage('Mi nombre es Gustavo', 'user');
    try
      Conn.Run;
      Last := Conn.GetLastMessage;
      if Assigned(Last) then
        Writeln('Response: ', Copy(Last.Prompt, 1, 120));
      Writeln;
      Writeln('SUCCESS: Turn 2 completed without error.');
    except
      on E: Exception do
        Writeln('ERROR on turn 2: ', E.Message);
    end;

  finally
    Conn.Free;
  end;
end;

// ---------------------------------------------------------------------------
begin
  if ParamCount < 3 then
  begin
    Writeln('Usage: ChatTest <provider> <model> <apikey>');
    Writeln('  provider : driver name, e.g. "qwen", "deepseek", "openai"');
    Writeln('  model    : model name,  e.g. "qwen3-235b-a22b"');
    Writeln('  apikey   : your API key');
    Writeln;
    Writeln('Example (Qwen with thinking):');
    Writeln('  ChatTest qwen qwen3-235b-a22b sk-xxxxxxxx');
    Halt(1);
  end;

  try
    RunTest(ParamStr(1), ParamStr(2), ParamStr(3));
  except
    on E: Exception do
    begin
      Writeln('Fatal: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
