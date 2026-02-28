program demo_kimi;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiKimiChat
// Kimi (Moonshot AI) expone un endpoint OpenAI-compatible en
// https://api.moonshot.ai/v1/
//
// Requisito: variable de entorno KIMI_API_KEY configurada
//   Windows:  set KIMI_API_KEY=sk-xxxx
//   Linux/Mac: export KIMI_API_KEY=sk-xxxx
//
// Modelos disponibles: moonshot-v1-8k, moonshot-v1-32k, moonshot-v1-128k
//                      kimi-k2, kimi-thinking-preview

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Kimi,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiKimiChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo Kimi/Moonshot (SYNC) ===');
  WriteLn;

  if GetEnvironmentVariable('KIMI_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno KIMI_API_KEY no configurada.');
    WriteLn('  Windows:  set KIMI_API_KEY=sk-xxxx');
    WriteLn('  Linux:    export KIMI_API_KEY=sk-xxxx');
    Halt(1);
  end;

  Chat := TAiKimiChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @KIMI_API_KEY en el constructor
    Chat.Model        := 'moonshot-v1-8k';
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
  WriteLn('Demo Kimi sync finalizado.');
end.
