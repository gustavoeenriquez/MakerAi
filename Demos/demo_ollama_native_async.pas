program demo_ollama_native_async;
{$mode objfpc}{$H+}

// Demo ASINCRONO/STREAMING con TAiOllamaChat (driver nativo Ollama)
// Usa el endpoint /api/chat de Ollama con stream=true
//
// Requisito: Ollama corriendo en localhost:11434 con algun modelo disponible
// Ejemplo: ollama run gemma3:1b

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Ollama,
  uMakerAi.Chat.Initializations,
  fpjson;

// ---------------------------------------------------------------------------
// Manejador de eventos — en FPC los eventos "of object" requieren un objeto
// ---------------------------------------------------------------------------
type
  TDemoEvents = class
    Done    : Boolean;
    EndCount: Integer;
    procedure OnData(const Sender: TObject; aMsg: TAiChatMessage;
        aResponse: TJSONObject; aRole, aText: string);
    procedure OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
        aResponse: TJSONObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string;
        E: Exception; const Response: IAiHttpResponse);
  end;

procedure TDemoEvents.OnData(const Sender: TObject; aMsg: TAiChatMessage;
    aResponse: TJSONObject; aRole, aText: string);
begin
  Write(aText);
end;

procedure TDemoEvents.OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
    aResponse: TJSONObject; aRole, aText: string);
var
  Chat: TAiOllamaChat;
begin
  Inc(EndCount);
  WriteLn;
  WriteLn('[OnReceiveDataEnd #', EndCount, '] role=', aRole,
          '  chars=', Length(aText));

  // Mostrar tokens si estan disponibles
  if Sender is TAiOllamaChat then
  begin
    Chat := TAiOllamaChat(Sender);
    WriteLn('Tokens — Prompt: ', Chat.Prompt_tokens,
            '  Completion: ',    Chat.Completion_tokens,
            '  Total: ',         Chat.Total_tokens);
  end;

  Done := True;
end;

procedure TDemoEvents.OnError(Sender: TObject; const ErrorMsg: string;
    E: Exception; const Response: IAiHttpResponse);
begin
  WriteLn('[ERROR] ', ErrorMsg);
  Done := True;
end;

// ---------------------------------------------------------------------------
var
  Chat   : TAiOllamaChat;
  Events : TDemoEvents;
  Timeout: Integer;

begin
  WriteLn('=== MakerAI FPC — Demo Ollama NATIVO ASYNC/STREAMING ===');
  WriteLn;

  Events := TDemoEvents.Create;
  Chat   := TAiOllamaChat.Create(nil);
  try
    Chat.ApiKey      := '1234';
    Chat.Url         := 'http://127.0.0.1:11434/';
    Chat.Model       := 'gemma3:1b';
    Chat.Max_tokens  := 256;
    Chat.Temperature := 0.7;
    Chat.Asynchronous := True;
    Chat.KeepAlive   := '5m';

    Chat.OnReceiveData    := @Events.OnData;
    Chat.OnReceiveDataEnd := @Events.OnDataEnd;
    Chat.OnError          := @Events.OnError;

    WriteLn('Driver  : ', Chat.GetDriverName);
    WriteLn('Modelo  : ', Chat.Model);
    WriteLn('Stream  : ', Chat.Asynchronous);
    WriteLn;
    WriteLn('>>> Pregunta: Cuentame un chiste muy corto');
    WriteLn('<<< Respuesta (streaming):');
    WriteLn;

    Chat.AddMessageAndRun('Cuentame un chiste muy corto', 'user');

    // Esperar que el thread HTTP termine (maximo 30 segundos)
    Timeout := 0;
    while not Events.Done and (Timeout < 600) do
    begin
      Sleep(50);
      Inc(Timeout);
    end;

    if Timeout >= 600 then
      WriteLn('[TIMEOUT] No se recibio respuesta en 30 segundos');

    WriteLn;
    if Events.EndCount > 1 then
      WriteLn('[BUG] OnReceiveDataEnd se disparo ', Events.EndCount, ' veces!')
    else
      WriteLn('OnReceiveDataEnd disparado: ', Events.EndCount, ' vez. OK');

  finally
    Chat.Free;
    Events.Free;
  end;

  WriteLn;
  WriteLn('Demo Ollama nativo async finalizado.');
end.
