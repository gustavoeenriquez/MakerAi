program demo_grok_async;
{$mode objfpc}{$H+}

// Demo ASINCRONO/STREAMING con TAiGrokChat (xAI Grok)
//
// Con reasoning_format='parsed' y ThinkingLevel=tlHigh, Grok emite
// bloques de razonamiento que llegan via OnReceiveThinking.
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
    procedure OnThinking(const Sender: TObject; aMsg: TAiChatMessage;
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

procedure TDemoEvents.OnThinking(const Sender: TObject; aMsg: TAiChatMessage;
    aResponse: TJSONObject; aRole, aText: string);
begin
  // Solo llega si ReasoningFormat='parsed' y ThinkingLevel != tlDefault
  Write('<think>', aText, '</think>');
end;

procedure TDemoEvents.OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
    aResponse: TJSONObject; aRole, aText: string);
var
  Chat: TAiGrokChat;
begin
  Inc(EndCount);
  WriteLn;
  WriteLn('[OnReceiveDataEnd #', EndCount, '] role=', aRole,
          '  chars=', Length(aText));

  if Sender is TAiGrokChat then
  begin
    Chat := TAiGrokChat(Sender);
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
  WriteLn('Verifica que GROK_API_KEY este configurado y sea valido.');
  Done := True;
end;

// ---------------------------------------------------------------------------
var
  Chat   : TAiGrokChat;
  Events : TDemoEvents;
  Timeout: Integer;

begin
  WriteLn('=== MakerAI FPC — Demo xAI Grok ASYNC/STREAMING ===');
  WriteLn;

  if GetEnvironmentVariable('GROK_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno GROK_API_KEY no configurada.');
    Halt(1);
  end;

  Events := TDemoEvents.Create;
  Chat   := TAiGrokChat.Create(nil);
  try
    Chat.Model        := 'grok-3';
    Chat.Max_tokens   := 256;
    Chat.Temperature  := 0.7;
    Chat.Asynchronous := True;

    Chat.OnReceiveData    := @Events.OnData;
    Chat.OnReceiveThinking := @Events.OnThinking;
    Chat.OnReceiveDataEnd := @Events.OnDataEnd;
    Chat.OnError          := @Events.OnError;

    WriteLn('Driver  : ', Chat.GetDriverName);
    WriteLn('Modelo  : ', Chat.Model);
    WriteLn('URL     : ', Chat.Url);
    WriteLn('Stream  : ', Chat.Asynchronous);
    WriteLn;
    WriteLn('>>> Pregunta: Explica brevemente que es xAI y Grok');
    WriteLn('<<< Respuesta (streaming):');
    WriteLn;

    Chat.AddMessageAndRun('Explica brevemente que es xAI y Grok', 'user');

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
  WriteLn('Demo Grok async finalizado.');
end.
