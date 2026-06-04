program OllamaLeakTest;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.Chat.Ollama,
  uMakerAi.Chat;

// ---------------------------------------------------------------------------
// Test de memory leak con TAiOllamaChat + gemma3:1b
// ReportMemoryLeaksOnShutdown es nativo en Delphi 10.3+ (FastMM integrado)
// ---------------------------------------------------------------------------

procedure RunTest;
var
  Chat: TAiOllamaChat;
  Respuesta: string;
begin
  Writeln('=== OllamaLeakTest ===');
  Writeln('Modelo : gemma3:1b');
  Writeln('URL    : http://localhost:11434/');
  Writeln;

  Chat := TAiOllamaChat.Create(nil);
  try
    Chat.Model        := 'gemma3:1b';
    Chat.Url          := 'http://localhost:11434/';
    Chat.Asynchronous := False;
    Chat.Max_tokens   := 256;

    Write('Enviando pregunta... ');
    Respuesta := Chat.AddMessageAndRun('¿Cuánto es 2 + 2? Responde en una sola frase.', 'user', []);
    Writeln('OK');
    Writeln;
    Writeln('Respuesta del modelo:');
    Writeln('  ', Respuesta);
    Writeln;
    Writeln('Tokens usados:');
    Writeln('  Prompt     : ', Chat.Prompt_tokens);
    Writeln('  Completion : ', Chat.Completion_tokens);
    Writeln('  Total      : ', Chat.Total_tokens);
  finally
    Chat.Free;
    Writeln;
    Writeln('TAiOllamaChat liberado.');
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  try
    RunTest;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' - ', E.Message);
  end;

end.
