program demo_claude_async;
{$mode objfpc}{$H+}

// Demo ASINCRONO/STREAMING con TAiClaudeChat
// Anthropic Claude usa SSE con pares event:/data: (dos lineas por evento).
// El driver acumula bloques de contenido y reconstruye la respuesta al recibir
// el evento message_stop.
//
// Requisito: variable de entorno CLAUDE_API_KEY configurada
//   Windows:  set CLAUDE_API_KEY=sk-ant-xxxx
//   Linux:    export CLAUDE_API_KEY=sk-ant-xxxx

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.Initializations,
  fpjson;

// ---------------------------------------------------------------------------
// Manejador de eventos
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
  Chat: TAiClaudeChat;
begin
  Inc(EndCount);
  WriteLn;
  WriteLn('[OnReceiveDataEnd #', EndCount, '] role=', aRole,
          '  chars=', Length(aText));

  if Sender is TAiClaudeChat then
  begin
    Chat := TAiClaudeChat(Sender);
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
  WriteLn('Verifica que CLAUDE_API_KEY este configurado y sea valido.');
  Done := True;
end;

// ---------------------------------------------------------------------------
var
  Chat   : TAiClaudeChat;
  Events : TDemoEvents;
  Timeout: Integer;

begin
  WriteLn('=== MakerAI FPC — Demo Claude ASYNC/STREAMING ===');
  WriteLn;

  if GetEnvironmentVariable('CLAUDE_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno CLAUDE_API_KEY no configurada.');
    Halt(1);
  end;

  Events := TDemoEvents.Create;
  Chat   := TAiClaudeChat.Create(nil);
  try
    Chat.Model        := 'claude-haiku-4-5-20251001';
    Chat.Max_tokens   := 256;
    Chat.Temperature  := 1.0;
    Chat.Asynchronous := True;

    Chat.OnReceiveData    := @Events.OnData;
    Chat.OnReceiveDataEnd := @Events.OnDataEnd;
    Chat.OnError          := @Events.OnError;

    WriteLn('Driver  : ', Chat.GetDriverName);
    WriteLn('Modelo  : ', Chat.Model);
    WriteLn('URL     : ', Chat.Url);
    WriteLn('Stream  : ', Chat.Asynchronous);
    WriteLn;
    WriteLn('>>> Pregunta: Explica brevemente que es Anthropic Claude AI');
    WriteLn('<<< Respuesta (streaming):');
    WriteLn;

    Chat.AddMessageAndRun('Explica brevemente que es Anthropic Claude AI', 'user');

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
    else if Events.EndCount = 1 then
      WriteLn('OnReceiveDataEnd disparado: ', Events.EndCount, ' vez. OK');

  finally
    Chat.Free;
    Events.Free;
  end;

  WriteLn;
  WriteLn('Demo Claude async finalizado.');
end.
