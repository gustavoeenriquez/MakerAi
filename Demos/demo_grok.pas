program demo_grok;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiGrokChat (xAI Grok)
//
// Grok es compatible con OpenAI /chat/completions, con extensiones:
//   - reasoning_format: 'parsed', 'raw', 'hidden'
//   - reasoning_effort: 'low', 'medium', 'high' (campo raiz, no objeto anidado)
//   - Web search: POST /v1/responses con tools=[{type:"web_search"}]
//   - Generacion de imagenes: POST /v1/images/generations
//
// Modelos:
//   grok-3       — modelo principal
//   grok-3-mini  — variante economica
//   grok-2-image — generacion de imagenes (usar InternalRunImageGeneration)
//
// Requisito: variable de entorno GROK_API_KEY configurada
//   Windows:  set GROK_API_KEY=xai-xxxx
//   Linux:    export GROK_API_KEY=xai-xxxx

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Grok,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiGrokChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo xAI Grok (SYNC) ===');
  WriteLn;

  if GetEnvironmentVariable('GROK_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno GROK_API_KEY no configurada.');
    WriteLn('  Windows:  set GROK_API_KEY=xai-xxxx');
    WriteLn('  Linux:    export GROK_API_KEY=xai-xxxx');
    Halt(1);
  end;

  Chat := TAiGrokChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @GROK_API_KEY en el constructor
    Chat.Model        := 'grok-3';
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
  WriteLn('Demo Grok sync finalizado.');
end.
