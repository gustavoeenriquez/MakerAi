program demo_groq;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiGroqChat
// Groq expone un endpoint OpenAI-compatible en https://api.groq.com/openai/v1/
//
// Requisito: variable de entorno GROQ_API_KEY configurada
//   Windows:  set GROQ_API_KEY=gsk_xxxx
//   Linux/Mac: export GROQ_API_KEY=gsk_xxxx
//
// Modelo por defecto: llama-3.1-8b-instant (rapido, gratis en tier basico)
// Otros modelos populares: llama-3.3-70b-versatile, gemma2-9b-it,
//                          deepseek-r1-distill-llama-70b (con reasoning)

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Groq,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiGroqChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo Groq (SYNC) ===');
  WriteLn;

  // Verificar que la API key este configurada
  if GetEnvironmentVariable('GROQ_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno GROQ_API_KEY no configurada.');
    WriteLn('  Windows:  set GROQ_API_KEY=gsk_xxxx');
    WriteLn('  Linux:    export GROQ_API_KEY=gsk_xxxx');
    Halt(1);
  end;

  Chat := TAiGroqChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @GROQ_API_KEY en el constructor
    Chat.Model        := 'llama-3.1-8b-instant';
    Chat.Max_tokens   := 256;
    Chat.Temperature  := 0.7;
    Chat.Asynchronous := False;

    WriteLn('Driver  : ', Chat.GetDriverName);
    WriteLn('Modelo  : ', Chat.Model);
    WriteLn('URL     : ', Chat.Url);
    WriteLn;

    // Pregunta 1
    WriteLn('>>> Pregunta 1: Hola! En una sola frase, quien eres?');
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

    WriteLn;

    // Pregunta 2 — misma sesion (historial incluido)
    WriteLn('>>> Pregunta 2: Dame un dato curioso sobre IA');
    WriteLn;

    try
      Resp := Chat.AddMessageAndRun('Dame un dato curioso sobre IA', 'user');
      WriteLn('<<< Respuesta: ', Resp);
      WriteLn;
      WriteLn('Tokens — Prompt: ', Chat.Prompt_tokens,
              '  Completion: ',    Chat.Completion_tokens,
              '  Total: ',         Chat.Total_tokens);
    except
      on E: Exception do
        WriteLn('[ERROR] ', E.Message);
    end;

    WriteLn;
    WriteLn('Mensajes en historial: ', Chat.Messages.Count);

  finally
    Chat.Free;
  end;

  WriteLn;
  WriteLn('Demo Groq sync finalizado.');
end.
