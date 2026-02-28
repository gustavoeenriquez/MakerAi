program demo_claude;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiClaudeChat
// Anthropic Claude usa su propia API:
//   https://api.anthropic.com/v1/messages
//   Cabeceras: x-api-key, anthropic-version, anthropic-beta
//
// Requisito: variable de entorno CLAUDE_API_KEY configurada
//   Windows:  set CLAUDE_API_KEY=sk-ant-xxxx
//   Linux:    export CLAUDE_API_KEY=sk-ant-xxxx
//
// Modelos disponibles: claude-opus-4-5, claude-sonnet-4-5,
//                      claude-haiku-4-5, claude-3-5-sonnet-20241022

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiClaudeChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo Claude (SYNC) ===');
  WriteLn;

  if GetEnvironmentVariable('CLAUDE_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno CLAUDE_API_KEY no configurada.');
    WriteLn('  Windows:  set CLAUDE_API_KEY=sk-ant-xxxx');
    WriteLn('  Linux:    export CLAUDE_API_KEY=sk-ant-xxxx');
    Halt(1);
  end;

  Chat := TAiClaudeChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @CLAUDE_API_KEY en el constructor
    Chat.Model        := 'claude-haiku-4-5-20251001';
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
  WriteLn('Demo Claude sync finalizado.');
end.
