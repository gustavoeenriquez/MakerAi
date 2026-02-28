program demo_openai;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiOpenChat (Responses API)
// OpenAI usa la API Responses:
//   https://api.openai.com/v1/responses
//   Campos: input (no messages), instructions (no system),
//           previous_response_id para memoria de servidor.
//
// Requisito: variable de entorno OPENAI_API_KEY configurada
//   Windows:  set OPENAI_API_KEY=sk-xxxx
//   Linux:    export OPENAI_API_KEY=sk-xxxx
//
// Modelos disponibles: gpt-4o, gpt-4o-mini, gpt-4.5-preview,
//                      o1, o3, o4-mini, gpt-4.1, gpt-4.1-mini

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiOpenChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo OpenAI (SYNC) ===');
  WriteLn;

  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno OPENAI_API_KEY no configurada.');
    WriteLn('  Windows:  set OPENAI_API_KEY=sk-xxxx');
    WriteLn('  Linux:    export OPENAI_API_KEY=sk-xxxx');
    Halt(1);
  end;

  Chat := TAiOpenChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @OPENAI_API_KEY en el constructor
    Chat.Model        := 'gpt-4o-mini';
    Chat.Max_tokens   := 256;
    Chat.Temperature  := 1.0;
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
  WriteLn('Demo OpenAI sync finalizado.');
end.
