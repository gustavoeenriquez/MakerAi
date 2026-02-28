program demo_ollama;
{$mode objfpc}{$H+}

uses
  SysUtils,
  uMakerAi.Chat,
  uMakerAi.Chat.GenericLLM,
  uMakerAi.Chat.Initializations;

var
  Chat : TAiGenericChat;
  Resp : string;

begin
  WriteLn('=== MakerAI FPC — Demo Ollama (GenericLLM) ===');
  WriteLn;

  Chat := TAiGenericChat.Create(nil);
  try
    // Configuracion para Ollama local
    Chat.ApiKey      := '1234';                          // Ollama no requiere clave
    Chat.Url         := 'http://127.0.0.1:11434/v1/';
    Chat.Model       := 'gemma3:1b';
    Chat.Max_tokens  := 512;
    Chat.Temperature := 0.7;
    Chat.Asynchronous:= False;

    WriteLn('URL   : ', Chat.Url);
    WriteLn('Modelo: ', Chat.Model);
    WriteLn;

    WriteLn('>>> Pregunta: Hola! En una sola frase, quien eres?');
    WriteLn;

    try
      Resp := Chat.AddMessageAndRun('Hola! En una sola frase, quien eres?', 'user');
    except
      on E: Exception do
      begin
        WriteLn('[ERROR] ', E.ClassName, ': ', E.Message);
        Halt(1);
      end;
    end;

    WriteLn('<<< Respuesta:');
    WriteLn(Resp);
    WriteLn;
    WriteLn('Tokens usados — Prompt: ', Chat.Prompt_tokens,
            '  Completion: ', Chat.Completion_tokens,
            '  Total: ', Chat.Total_tokens);
  finally
    Chat.Free;
  end;

  WriteLn;
  WriteLn('Demo finalizado OK.');
end.
