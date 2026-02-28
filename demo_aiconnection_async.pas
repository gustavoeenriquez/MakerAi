program demo_aiconnection_async;
{$mode objfpc}{$H+}

// Demo ASINCRONO/STREAMING con TAiChatConnection
//
// Demuestra que TAiChatConnection:
//   - Acepta OnReceiveData / OnReceiveDataEnd igual que un driver directo
//   - OnReceiveDataEnd acumula tokens de todos los turnos (acumulativo)
//   - El driver subyacente puede cambiarse en runtime (aqui: Ollama → GenericLLM)
//   - El patron Facade es transparente al codigo consumidor en modo async

uses
  SysUtils, Classes,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  fpjson;

// ---------------------------------------------------------------------------
// Manejador de eventos
// ---------------------------------------------------------------------------
type
  TDemoEvents = class
    Done        : Boolean;
    EndCount    : Integer;
    TotalPrompt : Integer;
    TotalComp   : Integer;
    TotalTok    : Integer;
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
  Conn: TAiChatConnection;
begin
  Inc(EndCount);
  WriteLn;
  WriteLn('[OnReceiveDataEnd #', EndCount, '] role=', aRole,
          '  chars=', Length(aText));

  // Los tokens ACUMULADOS estan en TAiChatConnection (suma de todos los turnos)
  if Sender is TAiChatConnection then
  begin
    Conn := TAiChatConnection(Sender);
    WriteLn('Tokens acumulados en Connection — Prompt: ', Conn.Prompt_tokens,
            '  Completion: ', Conn.Completion_tokens,
            '  Total: ', Conn.Total_tokens);
  end;

  // Los tokens de ESTE turno estan en aMsg
  WriteLn('Tokens este turno       — Prompt: ', aMsg.Prompt_tokens,
          '  Completion: ', aMsg.Completion_tokens,
          '  Total: ', aMsg.Total_tokens);

  Done := True;
end;

procedure TDemoEvents.OnError(Sender: TObject; const ErrorMsg: string;
    E: Exception; const Response: IAiHttpResponse);
begin
  WriteLn('[ERROR] ', ErrorMsg);
  Done := True;
end;

// ---------------------------------------------------------------------------
procedure WaitDone(Events: TDemoEvents; TimeoutMs: Integer = 30000);
var
  Elapsed: Integer;
begin
  Elapsed := 0;
  while not Events.Done and (Elapsed < TimeoutMs) do
  begin
    Sleep(50);
    Inc(Elapsed, 50);
  end;
  if Elapsed >= TimeoutMs then
    WriteLn('[TIMEOUT] No se recibio respuesta en ', TimeoutMs div 1000, 's');
end;

// ---------------------------------------------------------------------------
var
  Conn  : TAiChatConnection;
  Events: TDemoEvents;

begin
  WriteLn('=== MakerAI FPC — Demo TAiChatConnection ASYNC/STREAMING ===');
  WriteLn;

  Events := TDemoEvents.Create;
  Conn   := TAiChatConnection.Create(nil);
  try
    // --- Turno 1: Ollama nativo ---
    WriteLn('=== Turno 1: Driver=Ollama (API nativa) ===');
    Conn.DriverName := 'Ollama';
    Conn.Params.Values['Model']        := 'gemma3:1b';
    Conn.Params.Values['Max_tokens']   := '128';
    Conn.Params.Values['Temperature']  := '0.7';
    Conn.Params.Values['Asynchronous'] := 'True';

    Conn.OnReceiveData    := @Events.OnData;
    Conn.OnReceiveDataEnd := @Events.OnDataEnd;
    Conn.OnError          := @Events.OnError;

    WriteLn('Driver activo : ', Conn.AiChat.GetDriverName);
    WriteLn('Modelo        : ', Conn.AiChat.Model);
    WriteLn('Async         : ', Conn.AiChat.Asynchronous);
    WriteLn;
    WriteLn('>>> Cuéntame un chiste muy corto');
    WriteLn('<<< Respuesta (streaming):');
    WriteLn;

    Events.Done     := False;
    Events.EndCount := 0;
    Conn.AddMessageAndRun('Cuéntame un chiste muy corto', 'user');
    WaitDone(Events);

    WriteLn;
    if Events.EndCount = 1 then
      WriteLn('OnReceiveDataEnd x1. OK')
    else
      WriteLn('[BUG] OnReceiveDataEnd x', Events.EndCount);

    // --- Turno 2: cambiar a GenericLLM en runtime ---
    WriteLn;
    WriteLn('=== Turno 2: Cambiar a GenericLLM (OpenAI-compat) ===');
    Conn.NewChat;   // limpiar historial
    Conn.DriverName := 'GenericLLM';
    Conn.Params.Values['Url']          := 'http://127.0.0.1:11434/v1/';
    Conn.Params.Values['Model']        := 'gemma3:1b';
    Conn.Params.Values['Max_tokens']   := '64';
    Conn.Params.Values['Asynchronous'] := 'True';

    // Los eventos ya estan conectados al Conn, ApplyEventsToChat los propaga
    // al nuevo driver automaticamente

    WriteLn('Driver activo : ', Conn.AiChat.GetDriverName);
    WriteLn('Modelo        : ', Conn.AiChat.Model);
    WriteLn('URL           : ', Conn.AiChat.Url);
    WriteLn;
    WriteLn('>>> Di "streaming GenericLLM OK" en una sola linea.');
    WriteLn('<<< Respuesta (streaming):');
    WriteLn;

    Events.Done     := False;
    Events.EndCount := 0;
    Conn.AddMessageAndRun('Di "streaming GenericLLM OK" en una sola linea.', 'user');
    WaitDone(Events);

    WriteLn;
    if Events.EndCount = 1 then
      WriteLn('OnReceiveDataEnd x1. OK')
    else
      WriteLn('[BUG] OnReceiveDataEnd x', Events.EndCount);

    WriteLn;
    WriteLn('Tokens totales acumulados en Connection:');
    WriteLn('  Prompt: ', Conn.Prompt_tokens,
            '  Completion: ', Conn.Completion_tokens,
            '  Total: ', Conn.Total_tokens);

  finally
    Conn.Free;
    Events.Free;
  end;

  WriteLn;
  WriteLn('Demo AiConnection async finalizado.');
end.
