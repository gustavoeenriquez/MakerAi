program demo_lmstudio;
{$mode objfpc}{$H+}

// Demo SINCRONO con TAiLMStudioChat
// LM Studio expone un endpoint OpenAI-compatible en http://127.0.0.1:1234/v1/
//
// Requisito: LM Studio corriendo con algun modelo cargado
// En LM Studio: abrir un modelo -> boton "Start Server" (puerto 1234 por defecto)
// El nombre del modelo no importa — LM Studio usa el modelo que tenga activo
//
// Si LM Studio no esta disponible, el demo mostrara un error de conexion.

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.LMStudio,
  uMakerAi.Chat.Initializations;

var
  Chat: TAiLMStudioChat;
  Resp: string;

begin
  WriteLn('=== MakerAI FPC — Demo LM Studio (SYNC) ===');
  WriteLn;

  Chat := TAiLMStudioChat.Create(nil);
  try
    // LM Studio local — no requiere ApiKey real, acepta cualquier valor
    // El modelo 'lmstudio-local' es el placeholder; LM Studio usa el modelo activo
    Chat.ApiKey      := '1234';
    Chat.Url         := 'http://127.0.0.1:1234/v1/';
    Chat.Model       := 'lmstudio-local';
    Chat.Max_tokens  := 256;
    Chat.Temperature := 0.7;
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
        WriteLn('[ERROR] No se pudo conectar a LM Studio: ', E.Message,
                LineEnding, 'Asegurate de tener LM Studio corriendo en ',
                Chat.Url);
    end;

  finally
    Chat.Free;
  end;

  WriteLn;
  WriteLn('Demo LM Studio sync finalizado.');
end.
