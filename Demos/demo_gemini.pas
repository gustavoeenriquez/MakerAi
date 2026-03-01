program demo_gemini;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiGeminiChat
// Gemini (Google) usa su propia API:
//   https://generativelanguage.googleapis.com/v1beta/
//   Endpoint: models/{model}:generateContent?key={ApiKey}
//
// Requisito: variable de entorno GEMINI_API_KEY configurada
//   Windows:  set GEMINI_API_KEY=AIzaSy-xxxx
//   Linux:    export GEMINI_API_KEY=AIzaSy-xxxx
//
// Modelos disponibles: gemini-2.5-flash (default), gemini-2.0-flash,
//                      gemini-1.5-pro, gemini-1.5-flash

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiGeminiChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo Gemini (SYNC) ===');
  WriteLn;

  if GetEnvironmentVariable('GEMINI_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno GEMINI_API_KEY no configurada.');
    WriteLn('  Windows:  set GEMINI_API_KEY=AIzaSy-xxxx');
    WriteLn('  Linux:    export GEMINI_API_KEY=AIzaSy-xxxx');
    Halt(1);
  end;

  Chat := TAiGeminiChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @GEMINI_API_KEY en el constructor
    Chat.Model       := 'gemini-2.5-flash';
    // NOTA: gemini-2.5-flash tiene thinking activado por defecto.
    // Los thinking tokens comparten el mismo presupuesto que maxOutputTokens,
    // por lo que se necesita un valor suficientemente alto (>=4096).
    Chat.Max_tokens  := 4096;
    Chat.Temperature := 1.0;
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
  WriteLn('Demo Gemini sync finalizado.');
end.
