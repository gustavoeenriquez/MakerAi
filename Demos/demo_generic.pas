program demo_generic;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiGenericChat (GenericLLM)
// Driver OpenAI-compatible generico — sirve para cualquier endpoint local o
// remoto que implemente la API /chat/completions de OpenAI.
//
// En este demo se usa Ollama como backend local:
//   URL:   http://127.0.0.1:11434/v1/
//   Model: gemma3:1b  (o cualquier modelo instalado en Ollama)
//
// Requisito: Ollama corriendo con algun modelo cargado
//   ollama serve
//   ollama pull gemma3:1b

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.GenericLLM,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiGenericChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo GenericLLM (SYNC) ===');
  WriteLn;

  Chat := TAiGenericChat.Create(nil);
  try
    // Apuntar al endpoint OpenAI-compat de Ollama
    Chat.ApiKey       := '1234';
    Chat.Url          := 'http://127.0.0.1:11434/v1/';
    Chat.Model        := 'gemma3:1b';
    Chat.Max_tokens   := 256;
    Chat.Temperature  := 0.7;
    Chat.Asynchronous := False;

    WriteLn('Driver  : ', Chat.GetDriverName);
    WriteLn('Modelo  : ', Chat.Model);
    WriteLn('URL     : ', Chat.Url);
    WriteLn;

    WriteLn('>>> Pregunta: Hola! En una sola frase, quien eres?');
    WriteLn;

    try
      Resp := Chat.AddMessageAndRun('Hola! En una sola frase, quien eres?', 'user');
      WriteLn('<<< Respuesta: ', Resp);
      WriteLn;
      WriteLn('Tokens — Prompt: ', Chat.Prompt_tokens,
              '  Completion: ',    Chat.Completion_tokens,
              '  Total: ',         Chat.Total_tokens);
    except
      on E: Exception do
        WriteLn('[ERROR] ', E.Message);
    end;

  finally
    Chat.Free;
  end;

  WriteLn;
  WriteLn('Demo GenericLLM sync finalizado.');
end.
