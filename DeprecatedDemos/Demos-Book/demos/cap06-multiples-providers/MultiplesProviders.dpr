program MultiplesProviders;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — Capítulo 6: Trabajando con Múltiples Providers
// =============================================================================
// Demuestra el principio central de portabilidad de MakerAI: el código de
// aplicación NO cambia al cambiar de provider. Solo cambia DriverName.
//
// Demuestra:
//   - Demo 1: La misma pregunta a múltiples providers — código idéntico,
//             resultados comparables, tiempos y estilos distintos.
//   - Demo 2: Patrón Fallback — si el provider primario falla (timeout, API key
//             inválida, rate limit), el código intenta automáticamente con el
//             secundario. La lógica de aplicación no sabe qué provider respondió.
//
// Configurar providers:
//   GROQ_API_KEY   → gratuito, recomendado para empezar (Enabled: True)
//   CLAUDE_API_KEY → Anthropic Claude
//   OPENAI_API_KEY → OpenAI GPT
//   GEMINI_API_KEY → Google Gemini
//   Ollama         → local, sin API key (requiere Ollama instalado y corriendo)
//
//   Pon Enabled = False en los providers cuya API key no tengas configurada.
// =============================================================================

uses
  System.SysUtils,
  System.Diagnostics,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  // ── Cada import activa el auto-registro del driver ────────────────────────
  uMakerAi.Chat.Claude,    // DriverName = 'Claude'
  uMakerAi.Chat.OpenAi,   // DriverName = 'OpenAI'
  uMakerAi.Chat.Gemini,   // DriverName = 'Gemini'
  uMakerAi.Chat.Groq,     // DriverName = 'Groq'
  uMakerAi.Chat.Ollama;   // DriverName = 'Ollama' (local, sin API key)

// =============================================================================
//  Configuración de providers
// =============================================================================
type
  TProviderCfg = record
    Driver : string;
    Model  : string;
    ApiKey : string;
    Enabled: Boolean;
  end;

const
  // Habilita/deshabilita según las API keys que tengas disponibles.
  // Groq está habilitado por defecto — es gratuito.
  PROVIDERS: array[0..4] of TProviderCfg = (
    (Driver: 'Groq';   Model: 'llama-3.3-70b-versatile';   ApiKey: '@GROQ_API_KEY';   Enabled: True),
    (Driver: 'Claude'; Model: 'claude-haiku-4-5-20251001'; ApiKey: '@CLAUDE_API_KEY'; Enabled: False),
    (Driver: 'OpenAI'; Model: 'gpt-4.1-mini';              ApiKey: '@OPENAI_API_KEY'; Enabled: False),
    (Driver: 'Gemini'; Model: 'gemini-2.0-flash';          ApiKey: '@GEMINI_API_KEY'; Enabled: False),
    (Driver: 'Ollama'; Model: 'gemma3:4b';                 ApiKey: '';                Enabled: False)
  );

  PREGUNTA = 'En exactamente 2 oraciones: ¿Qué es la inteligencia artificial?';

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

// =============================================================================
//  Utilidad: crea y configura una conexión con los parámetros estándar.
//  El llamador es responsable de liberar el objeto con Free.
// =============================================================================
function CrearConexion(const Cfg: TProviderCfg): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := Cfg.Driver;
  Result.Model      := Cfg.Model;

  Result.Params.Values['ApiKey']       := Cfg.ApiKey;
  Result.Params.Values['Asynchronous'] := 'False';
  Result.Params.Values['Max_Tokens']   := '256';

  Result.SystemPrompt.Text := 'Responde siempre en español de forma concisa.';
end;

// =============================================================================
//  Demo 1 — La misma pregunta a múltiples providers
//
//  El código que envía la pregunta es idéntico para cada provider.
//  La única diferencia es DriverName — el conector universal se encarga del resto.
// =============================================================================
procedure Demo1_MultiProvider;
var
  Cfg : TProviderCfg;
  Conn: TAiChatConnection;
  SW  : TStopwatch;
  R   : string;
begin
  Separador('Demo 1 — Misma pregunta, múltiples providers');
  WriteLn('Pregunta: "', PREGUNTA, '"');

  for Cfg in PROVIDERS do
  begin
    WriteLn('');

    if not Cfg.Enabled then
    begin
      WriteLn('[', Cfg.Driver, '] — deshabilitado (pon Enabled = True en PROVIDERS)');
      Continue;
    end;

    Write('[', Cfg.Driver, ' / ', Cfg.Model, '] ...');
    Conn := CrearConexion(Cfg);
    try
      SW := TStopwatch.StartNew;
      try
        R := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
        SW.Stop;
        WriteLn(' ', SW.ElapsedMilliseconds, ' ms');
        WriteLn('  ', R);
      except
        on E: Exception do
        begin
          SW.Stop;
          WriteLn(' ERROR');
          WriteLn('  ', E.Message);
        end;
      end;
    finally
      Conn.Free;
    end;
  end;

  WriteLn('');
  WriteLn('→ El código de envío/recepción es literalmente el mismo para cada provider.');
  WriteLn('  Cambiar de provider = cambiar DriverName. Nada más.');
end;

// =============================================================================
//  Demo 2 — Patrón Fallback
//
//  Arquitectura de resiliencia: si el provider primario falla, usar el secundario.
//  El código de aplicación que llama a PreguntarConFallback() no sabe —ni le
//  importa— qué provider respondió al final.
// =============================================================================
function PreguntarConFallback(const Pregunta: string): string;
const
  // Provider primario: Groq (rápido, gratuito)
  PRIMARY: TProviderCfg = (
    Driver: 'Groq'; Model: 'llama-3.3-70b-versatile';
    ApiKey: '@GROQ_API_KEY'; Enabled: True);

  // Provider de respaldo: Claude (confiable, pago)
  FALLBACK: TProviderCfg = (
    Driver: 'Claude'; Model: 'claude-haiku-4-5-20251001';
    ApiKey: '@CLAUDE_API_KEY'; Enabled: True);
var
  Conn: TAiChatConnection;
begin
  Result := '';

  // Intentar con el provider primario
  Write('  Intentando con ', PRIMARY.Driver, ' (primario)...');
  Conn := CrearConexion(PRIMARY);
  try
    try
      Result := Conn.AddMessageAndRun(Pregunta, 'user', []);
      WriteLn(' OK (', PRIMARY.Driver, ' respondió)');
      Exit;
    except
      on E: Exception do
        WriteLn(' FALLÓ: ', E.Message);
    end;
  finally
    Conn.Free;
  end;

  // El primario falló — intentar con el fallback
  Write('  Intentando con ', FALLBACK.Driver, ' (fallback)...');
  Conn := CrearConexion(FALLBACK);
  try
    try
      Result := Conn.AddMessageAndRun(Pregunta, 'user', []);
      WriteLn(' OK (fallback ', FALLBACK.Driver, ' respondió)');
    except
      on E: Exception do
      begin
        WriteLn(' TAMBIÉN FALLÓ: ', E.Message);
        Result := '[Error: ningún provider disponible]';
      end;
    end;
  finally
    Conn.Free;
  end;
end;

procedure Demo2_Fallback;
var
  R: string;
begin
  Separador('Demo 2 — Patrón Fallback');
  WriteLn('Pregunta: "', PREGUNTA, '"');
  WriteLn('');

  R := PreguntarConFallback(PREGUNTA);

  WriteLn('');
  WriteLn('Respuesta final: ', R);
  WriteLn('');
  WriteLn('→ El código que llama a PreguntarConFallback() no sabe qué provider');
  WriteLn('  respondió. La lógica de resiliencia está encapsulada.');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
begin
  WriteLn('=== MakerAI — Capítulo 6: Múltiples Providers ===');
  WriteLn('Modifica el array PROVIDERS para habilitar más providers.');

  Demo1_MultiProvider;
  Demo2_Fallback;

  WriteLn('');
  WriteLn(StringOfChar('=', 62));
  WriteLn('  Demo completado.');
  WriteLn(StringOfChar('=', 62));
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  WriteLn('');
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
