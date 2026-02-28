program demo_cohere;
{$mode objfpc}{$H+}

// Demo SINCRONO con TCohereChat (Cohere v2)
//
// Cohere v2 Chat API: https://docs.cohere.com/reference/chat
//
// Diferencias clave respecto a OpenAI:
//   - Endpoint: /v2/chat
//   - top_p mapeado a 'p'; soporta 'k' para top-k sampling
//   - Tokens en usage.tokens.input_tokens / output_tokens
//   - Respuesta en message.content[] (array de {type, text})
//   - Citations en message.citations[]
//
// Modelos:
//   command-a-03-2025   — modelo principal (multiples idiomas)
//   command-r-plus      — version anterior, ampliamente disponible
//   command-r           — rapido y eficiente
//
// Requisito: variable de entorno COHERE_API_KEY configurada
//   Windows:  set COHERE_API_KEY=...
//   Linux:    export COHERE_API_KEY=...

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Cohere,
  uMakerAi.Chat.Initializations;

var
  Chat: TCohereChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo Cohere v2 (SYNC) ===');
  WriteLn;

  if GetEnvironmentVariable('COHERE_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno COHERE_API_KEY no configurada.');
    WriteLn('  Windows:  set COHERE_API_KEY=...');
    WriteLn('  Linux:    export COHERE_API_KEY=...');
    Halt(1);
  end;

  Chat := TCohereChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @COHERE_API_KEY en el constructor
    Chat.Model        := 'command-r';
    Chat.Max_tokens   := 256;
    Chat.Temperature  := 0.3;
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
  WriteLn('Demo Cohere sync finalizado.');
end.
