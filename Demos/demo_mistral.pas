program demo_mistral;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiMistralChat
// Mistral usa la API REST estandar de chat completions con algunas extensiones:
//   - random_seed en lugar de seed
//   - prompt_mode:"reasoning" para modelos Magistral
//   - Document QnA y OCR via endpoints propios
//
// Modelos principales:
//   mistral-small-latest   — rapido y economico
//   mistral-large-latest   — alta capacidad
//   magistral-medium-2506  — modelo reasoning
//   mistral-ocr-latest     — procesamiento OCR (no usa chat/completions)
//
// Requisito: variable de entorno MISTRAL_API_KEY configurada
//   Windows:  set MISTRAL_API_KEY=sk-xxxx
//   Linux:    export MISTRAL_API_KEY=sk-xxxx

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Mistral,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiMistralChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo Mistral AI (SYNC) ===');
  WriteLn;

  if GetEnvironmentVariable('MISTRAL_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno MISTRAL_API_KEY no configurada.');
    WriteLn('  Windows:  set MISTRAL_API_KEY=sk-xxxx');
    WriteLn('  Linux:    export MISTRAL_API_KEY=sk-xxxx');
    Halt(1);
  end;

  Chat := TAiMistralChat.Create(nil);
  try
    // ApiKey se lee automaticamente desde @MISTRAL_API_KEY en el constructor
    Chat.Model        := 'mistral-small-latest';
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
  WriteLn('Demo Mistral sync finalizado.');
end.
