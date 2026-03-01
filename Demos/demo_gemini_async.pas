program demo_gemini_async;
{$mode objfpc}{$H+}

// Demo ASINCRONO/STREAMING con TAiGeminiChat
// Gemini (Google) usa streamGenerateContent con JSON array plano (sin SSE data:)
// El driver parsea con brace-counting interno.
//
// Requisito: variable de entorno GEMINI_API_KEY configurada
//   Windows:  set GEMINI_API_KEY=AIzaSy-xxxx
//   Linux:    export GEMINI_API_KEY=AIzaSy-xxxx

uses
  SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Gemini,
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
  Chat: TAiGeminiChat;
begin
  Inc(EndCount);
  WriteLn;
  WriteLn('[OnReceiveDataEnd #', EndCount, '] role=', aRole,
          '  chars=', Length(aText));

  if Sender is TAiGeminiChat then
  begin
    Chat := TAiGeminiChat(Sender);
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
  WriteLn('Verifica que GEMINI_API_KEY este configurado y sea valido.');
  Done := True;
end;

// ---------------------------------------------------------------------------
var
  Chat   : TAiGeminiChat;
  Events : TDemoEvents;
  Timeout: Integer;

begin
  WriteLn('=== MakerAI FPC — Demo Gemini ASYNC/STREAMING ===');
  WriteLn;

  if GetEnvironmentVariable('GEMINI_API_KEY') = '' then
  begin
    WriteLn('[ERROR] Variable de entorno GEMINI_API_KEY no configurada.');
    Halt(1);
  end;

  Events := TDemoEvents.Create;
  Chat   := TAiGeminiChat.Create(nil);
  try
    Chat.Model        := 'gemini-2.5-flash';
    // NOTA: gemini-2.5-flash tiene thinking activado por defecto.
    // Los thinking tokens comparten el mismo presupuesto que maxOutputTokens,
    // por lo que se necesita un valor suficientemente alto (>=4096).
    Chat.Max_tokens   := 4096;
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
    WriteLn('>>> Pregunta: Explica brevemente que es Google Gemini AI');
    WriteLn('<<< Respuesta (streaming):');
    WriteLn;

    Chat.AddMessageAndRun('Explica brevemente que es Google Gemini AI', 'user');

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
  WriteLn('Demo Gemini async finalizado.');
end.
