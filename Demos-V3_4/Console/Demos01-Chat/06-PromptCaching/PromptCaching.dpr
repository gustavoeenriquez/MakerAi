program PromptCaching;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 01-Chat / 06-PromptCaching
// =============================================================================
// Verifica el soporte de Prompt Caching en Claude, OpenAI y DeepSeek.
//
// Conceptos que cubre:
//   - Claude: CacheSystemPrompt + CacheTTL  → cache_read/write_input_tokens
//   - OpenAI: automático (≥1024 tokens)     → cached_tokens en usage
//   - DeepSeek: automático por prefijo      → prompt_cache_hit_tokens en usage
//
// Observar en la salida:
//   Llamada 1: cached_read=0, cache_write>0   (MISS: se escribe al caché)
//   Llamada 2: cached_read>0, cache_write=0   (HIT:  se lee del caché)
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.DeepSeek,
  uMakerAi.Chat.Initializations;

// -----------------------------------------------------------------------------
// System prompt suficientemente largo para superar el umbral de caché:
//   claude-haiku-4-5  : mínimo 4096 tokens (~16000 chars)
//   claude-sonnet-4-6 : mínimo 2048 tokens (~8000 chars)
//   OpenAI / DeepSeek : mínimo 1024 tokens (~4000 chars)
// -----------------------------------------------------------------------------
function BuildLongSystemPrompt: String;
const
  BLOQUE =
    'Eres un asistente experto en inteligencia artificial especializado en ayudar ' +
    'a desarrolladores Delphi a integrar LLMs en sus aplicaciones con MakerAI Suite. ' +
    'Conoces a fondo los modelos de OpenAI (GPT-4.1, o3, o4-mini, gpt-image-1), ' +
    'Anthropic Claude (Opus 4.7, Sonnet 4.6, Haiku 4.5), Google Gemini (2.5 Flash/Pro), ' +
    'DeepSeek (V3.2 chat, R1 reasoner), Groq (Llama-4, Kimi-K2, Qwen3), ' +
    'Mistral (Large, Magistral, devstral), Cohere (Command-A), xAI Grok (grok-3, grok-4). ' +
    'Cuando explicas conceptos usas ejemplos en código Delphi usando TAiChatConnection. ' +
    'Recomiendas el modelo adecuado según: velocidad, costo, razonamiento, multimodalidad, ' +
    'soporte de imágenes/audio/video/PDF, o agentes autónomos con function calling. ';
var
  I: Integer;
begin
  Result := '';
  for I := 1 to 30 do   // 30 * ~680 chars ≈ 20400 chars ≈ 5100 tokens
    Result := Result + BLOQUE;
end;

// -----------------------------------------------------------------------------
// Imprime las estadísticas de tokens de la última respuesta
// -----------------------------------------------------------------------------
procedure PrintTokenStats(const Prefijo: String; Msgs: TAiChatMessages);
var
  Msg: TAiChatMessage;
begin
  if Msgs.Count = 0 then Exit;
  Msg := Msgs.Items[Msgs.Count - 1];
  Write(Prefijo);
  Write(Format('  prompt=%d  compl=%d',
    [Msg.Prompt_tokens, Msg.Completion_tokens]));
  if Msg.Cached_tokens > 0 then
    Write(Format('  [HIT cached_read=%d]', [Msg.Cached_tokens]))
  else
    Write('  [MISS cached_read=0]');
  if Msg.Cache_write_tokens > 0 then
    Write(Format('  [cache_write=%d]', [Msg.Cache_write_tokens]));
  Writeln;
end;

// =============================================================================
//  TEST CLAUDE — CacheSystemPrompt=True, CacheTTL='1h'
// =============================================================================
procedure TestClaude;
const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';
  PREGUNTA = '¿Cuál es el mejor modelo para analizar documentos PDF extensos?';
var
  Conn : TAiChatConnection;
  Resp : String;
begin
  Writeln('━━━ CLAUDE — CacheSystemPrompt + CacheTTL ━━━');
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;
    Conn.Params.Values['ApiKey']       := API_KEY;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '256';
    Conn.SystemPrompt.Text := BuildLongSystemPrompt;

    // Activar caching del system prompt con TTL extendido
    if Conn.AiChat is TAiClaudeChat then
    begin
      (Conn.AiChat as TAiClaudeChat).CacheSystemPrompt := True;
      (Conn.AiChat as TAiClaudeChat).CacheTTL          := '1h';
    end;

    // Llamada 1 — esperar MISS: el system prompt se escribe al caché
    Writeln('  Llamada 1 (esperando MISS + cache_write):');
    Conn.Messages.Clear;
    Resp := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
    Writeln('  Respuesta: ', Copy(Resp, 1, 80), '...');
    PrintTokenStats('  Tokens:', Conn.Messages);

    Writeln;

    // Llamada 2 — esperar HIT: el system prompt ya está en caché
    Writeln('  Llamada 2 (esperando HIT):');
    Conn.Messages.Clear;
    Resp := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
    Writeln('  Respuesta: ', Copy(Resp, 1, 80), '...');
    PrintTokenStats('  Tokens:', Conn.Messages);

  finally
    Conn.Free;
  end;
  Writeln;
end;

// =============================================================================
//  TEST OPENAI — Caching automático (sin configuración adicional)
// =============================================================================
procedure TestOpenAI;
const
  DRIVER  = 'OpenAI';
  MODEL   = 'gpt-4.1-mini';
  API_KEY = '@OPENAI_API_KEY';
  PREGUNTA = '¿Cuál es el mejor modelo para analizar documentos PDF extensos?';
var
  Conn : TAiChatConnection;
  Resp : String;
begin
  Writeln('━━━ OPENAI — Caching automático ━━━');
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;
    Conn.Params.Values['ApiKey']       := API_KEY;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '256';
    Conn.SystemPrompt.Text := BuildLongSystemPrompt;

    // Sin configuración adicional: OpenAI cachea automáticamente prefijos ≥1024 tokens

    Writeln('  Llamada 1 (esperando MISS):');
    Conn.Messages.Clear;
    Resp := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
    Writeln('  Respuesta: ', Copy(Resp, 1, 80), '...');
    PrintTokenStats('  Tokens:', Conn.Messages);

    Writeln;

    Writeln('  Llamada 2 (esperando HIT):');
    Conn.Messages.Clear;
    Resp := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
    Writeln('  Respuesta: ', Copy(Resp, 1, 80), '...');
    PrintTokenStats('  Tokens:', Conn.Messages);

  finally
    Conn.Free;
  end;
  Writeln;
end;

// =============================================================================
//  TEST DEEPSEEK — Caching automático por prefijo
// =============================================================================
procedure TestDeepSeek;
const
  DRIVER  = 'DeepSeek';
  MODEL   = 'deepseek-chat';
  API_KEY = '@DEEPSEEK_API_KEY';
  PREGUNTA = '¿Cuál es el mejor modelo para analizar documentos PDF extensos?';
var
  Conn : TAiChatConnection;
  Resp : String;
begin
  Writeln('━━━ DEEPSEEK — Caching automático por prefijo ━━━');
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;
    Conn.Params.Values['ApiKey']       := API_KEY;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '256';
    Conn.SystemPrompt.Text := BuildLongSystemPrompt;

    // Sin configuración: DeepSeek cachea automáticamente prefijos repetidos

    Writeln('  Llamada 1 (esperando MISS):');
    Conn.Messages.Clear;
    Resp := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
    Writeln('  Respuesta: ', Copy(Resp, 1, 80), '...');
    PrintTokenStats('  Tokens:', Conn.Messages);

    Writeln;

    Writeln('  Llamada 2 (esperando HIT):');
    Conn.Messages.Clear;
    Resp := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
    Writeln('  Respuesta: ', Copy(Resp, 1, 80), '...');
    PrintTokenStats('  Tokens:', Conn.Messages);

  finally
    Conn.Free;
  end;
  Writeln;
end;

// =============================================================================
//  MAIN
// =============================================================================
begin
  Writeln('=== PromptCaching Demo — MakerAI Suite ===');
  Writeln('Verifica que cached_read > 0 en la Llamada 2 de cada provider.');
  Writeln('Nota: los HITs de caché pueden tardar algunos segundos en activarse.');
  Writeln;
  try
    TestClaude;
    TestOpenAI;
    TestDeepSeek;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
