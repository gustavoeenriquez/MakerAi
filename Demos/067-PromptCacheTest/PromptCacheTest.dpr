program PromptCacheTest;

{$APPTYPE CONSOLE}
{$R *.res}

// Test runtime de prompt caching en Claude.
// Dispara 2 requests con el MISMO system prompt grande (>1024 tokens) y el flag
// portable CacheContext activado. Esperado:
//   Request 1 -> cache_write > 0 (escribe el system en cache)
//   Request 2 -> cache_read  > 0 (lee el system desde cache; mismo prefijo)
// Requiere CLAUDE_API_KEY en el entorno.

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.Initializations;

type
  TCap = class
  public
    Input, CacheWrite, CacheRead: Integer;
    procedure OnEnd(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSonObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string;
      AException: Exception; const AResponse: IHTTPResponse);
  end;

procedure TCap.OnEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
begin
  if Assigned(aMsg) then
  begin
    Input := aMsg.Prompt_tokens;
    CacheWrite := aMsg.Cache_write_tokens;
    CacheRead := aMsg.Cached_tokens;
  end;
end;

procedure TCap.OnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  if Assigned(AResponse) and (AResponse.StatusCode > 0) then
    WriteLn('  ERROR HTTP=' + IntToStr(AResponse.StatusCode) + ' | ' + ErrorMsg)
  else
    WriteLn('  ERROR: ' + ErrorMsg);
end;

function BigSystem: string;
var
  I: Integer;
begin
  Result := 'Eres un asistente experto en el framework MakerAI para Delphi. ' +
    'Responde siempre de forma muy breve. Contexto de referencia que debes considerar:' + sLineBreak;
  for I := 1 to 70 do
    Result := Result + Format(
      'Regla %d: MakerAI unifica multiples proveedores LLM (OpenAI, Claude, Gemini, Ollama, ' +
      'Groq, Mistral, DeepSeek, Kimi, Grok, Cohere) bajo el componente TAiChatConnection, ' +
      'con soporte de RAG vectorial y de grafos, protocolo MCP, agentes autonomos y ' +
      'herramientas nativas (vision, voz, busqueda web, generacion de imagen y video).', [I]) + sLineBreak;
end;

procedure RunOne(Conn: TAiChatConnection; Cap: TCap; const ALabel, APrompt: string);
var
  R: string;
begin
  WriteLn('=== ' + ALabel + ' ===');
  Cap.Input := 0; Cap.CacheWrite := 0; Cap.CacheRead := 0;
  R := Conn.AddMessageAndRun(APrompt, 'user', []);
  WriteLn('  Respuesta: ' + Trim(R));
  WriteLn(Format('  input=%d  cache_write=%d  cache_read=%d',
    [Cap.Input, Cap.CacheWrite, Cap.CacheRead]));
end;

var
  Conn: TAiChatConnection;
  Cap: TCap;

begin
  try
    Cap := TCap.Create;
    Conn := TAiChatConnection.Create(nil);
    try
      Conn.DriverName := 'Claude';
      Conn.Model := 'claude-opus-4-8';
      Conn.Params.Values['ApiKey'] := '@CLAUDE_API_KEY';
      Conn.Params.Values['Asynchronous'] := 'False';
      Conn.Params.Values['Max_Tokens'] := '300';
      Conn.Params.Values['Tool_Active'] := 'False';
      Conn.Params.Values['CacheContext'] := 'True'; // <-- flag portable de prompt caching
      Conn.SystemPrompt.Text := BigSystem;
      Conn.OnReceiveDataEnd := Cap.OnEnd;
      Conn.OnError := Cap.OnError;

      WriteLn('Modelo: claude-opus-4-8  | CacheContext=True');
      WriteLn(Format('System prompt: %d chars', [Length(Conn.SystemPrompt.Text)]));
      WriteLn('');

      RunOne(Conn, Cap, 'Request 1 (se espera cache_write>0)', 'Di "hola" en una sola palabra.');
      WriteLn('');
      Sleep(1500);
      RunOne(Conn, Cap, 'Request 2 (se espera cache_read>0)', 'Ahora di "adios" en una sola palabra.');

      WriteLn('');
      WriteLn('=== Veredicto ===');
      if Cap.CacheRead > 0 then
        WriteLn('OK: el system se leyo desde cache en Request 2 (cache_read=' + IntToStr(Cap.CacheRead) + ')')
      else
        WriteLn('ATENCION: cache_read=0 en Request 2 (caching no efectivo, revisar)');
    finally
      Conn.Free;
      Cap.Free;
    end;
    WriteLn('=== FIN ===');
  except
    on E: Exception do
      WriteLn('FATAL: ' + E.ClassName + ': ' + E.Message);
  end;
end.
