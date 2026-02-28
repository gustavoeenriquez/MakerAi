program demo_deepseek;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiDeepSeekChat
// DeepSeek usa la API OpenAI-compatible:
//   https://api.deepseek.com/v1/chat/completions
//
// Modelos principales:
//   deepseek-chat     — modelo conversacional
//   deepseek-reasoner — modelo con razonamiento (reasoning_content)
//
// Requisito: variable de entorno DEEPSEEK_API_KEY configurada
//   Windows:  set DEEPSEEK_API_KEY=sk-xxxx
//   Linux:    export DEEPSEEK_API_KEY=sk-xxxx

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.DeepSeek,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiDeepSeekChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo DeepSeek (SYNC) ===');
  WriteLn;

  if GetEnvironmentVariable('DEEPSEEK_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno DEEPSEEK_API_KEY no configurada.');
    WriteLn('  Windows:  set DEEPSEEK_API_KEY=sk-xxxx');
    WriteLn('  Linux:    export DEEPSEEK_API_KEY=sk-xxxx');
    Halt(1);
  end;

  Chat := TAiDeepSeekChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @DEEPSEEK_API_KEY en el constructor
    Chat.Model        := 'deepseek-chat';
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
  WriteLn('Demo DeepSeek sync finalizado.');
end.
