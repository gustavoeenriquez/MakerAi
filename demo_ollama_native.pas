program demo_ollama_native;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiOllamaChat (driver nativo Ollama)
// Usa el endpoint /api/chat de Ollama directamente (no /v1/chat/completions)
//
// Requisito: Ollama corriendo en localhost:11434 con algun modelo disponible
// Ejemplo: ollama run gemma3:1b

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Ollama,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiOllamaChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo Ollama NATIVO (SYNC) ===');
  WriteLn;

  Chat := TAiOllamaChat.Create(nil);
  try
    Chat.ApiKey      := '1234';          // Ollama local no requiere key real
    Chat.Url         := 'http://127.0.0.1:11434/';
    Chat.Model       := 'gemma3:1b';
    Chat.Max_tokens  := 256;
    Chat.Temperature := 0.7;
    Chat.Asynchronous := False;
    Chat.KeepAlive   := '5m';

    WriteLn('Driver  : ', Chat.GetDriverName);
    WriteLn('Modelo  : ', Chat.Model);
    WriteLn('URL     : ', Chat.Url);
    WriteLn('Stream  : ', Chat.Asynchronous);
    WriteLn;

    WriteLn('>>> Pregunta: Hola! En una sola frase, quien eres?');

    Resp := Chat.AddMessageAndRun('Hola! En una sola frase, quien eres?', 'user');

    WriteLn('<<< Respuesta: ', Resp);
    WriteLn;
    WriteLn('Tokens — Prompt: ', Chat.Prompt_tokens,
            '  Completion: ',    Chat.Completion_tokens,
            '  Total: ',         Chat.Total_tokens);

    // Segunda pregunta — prueba historial de conversacion
    WriteLn;
    WriteLn('>>> Pregunta 2: Dame un ejemplo de codigo Pascal simple');

    Resp := Chat.AddMessageAndRun('Dame un ejemplo de codigo Pascal simple', 'user');

    WriteLn('<<< Respuesta 2:');
    WriteLn(Resp);
    WriteLn;
    WriteLn('Tokens acumulados — Prompt: ', Chat.Prompt_tokens,
            '  Completion: ',               Chat.Completion_tokens,
            '  Total: ',                    Chat.Total_tokens);
    WriteLn;
    WriteLn('Mensajes en historial: ', Chat.Messages.Count);

  finally
    Chat.Free;
  end;

  WriteLn;
  WriteLn('Demo Ollama nativo sync finalizado.');
end.
