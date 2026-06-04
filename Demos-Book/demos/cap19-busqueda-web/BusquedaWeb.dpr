program BusquedaWeb;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — Capítulo 19: Búsqueda Web y Herramientas Externas
// Demo: Tres modalidades de búsqueda web
// =============================================================================
//
//   A) Nativa  — Gemini con Google Search integrado
//               ModelCaps=[cap_WebSearch] → el modelo busca por sí mismo
//               Requiere: GEMINI_API_KEY
//
//   B) Bridge  — Tavily + Gemini via gap analysis
//               ModelCaps=[] + SessionCaps=[cap_WebSearch] + WebSearchTool=Tavily
//               MakerAI detecta el gap → Tavily busca → Gemini sintetiza
//               Requiere: GEMINI_API_KEY + TAVILY_API_KEY
//
//   C) Directa — Tavily sin LLM (llamada directa a la herramienta)
//               Sin TAiChatConnection — resultado inmediato, sin tokens LLM
//               Requiere: TAVILY_API_KEY
//
// Prerequisito: paquete MakerAI ChatTools instalado.
//   Opción A: instalar uMakerAi.ChatTools.bpl como paquete en el IDE.
//   Opción B: agregar AiMaker.ChatTools\Source\Web al Library Path del IDE.
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,       // IAiWebSearchTool
  uMakerAi.Chat.Gemini,      // auto-registra driver 'Gemini' en el factory
  uMakerAi.ChatTools.Tavily; // TAiTavilyWebSearchTool — paquete ChatTools

const
  MODEL_GEMINI   = 'gemini-2.0-flash';
  QUERY_DELPHI   = 'Delphi programming language 2026 Embarcadero news features';
  QUERY_PRECIOS  = 'precio Bitcoin BTC hoy USD valor actual';

// =============================================================================

function KeyPresente(const EnvVar: string): Boolean;
begin
  Result := GetEnvironmentVariable(EnvVar) <> '';
end;

procedure Separador;
begin
  WriteLn(StringOfChar('─', 64));
end;

// =============================================================================
//  A) Búsqueda NATIVA — Gemini con Google Search integrado
// =============================================================================
// ModelCaps=[cap_WebSearch] significa que el modelo tiene búsqueda nativa.
// SessionCaps=[cap_WebSearch] le indica a MakerAI que la necesitamos en esta
// sesión. Como no hay gap (el modelo ya puede), delega al modelo directamente.
// =============================================================================

procedure DemoNativa;
var
  Conn: TAiChatConnection;
  Resp: string;
begin
  WriteLn('A) Búsqueda NATIVA — Gemini con Google Search integrado');
  WriteLn('   ModelCaps=[cap_WebSearch] → el modelo busca por sí mismo');
  Separador;
  WriteLn;

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := 'Gemini';
    Conn.Model      := MODEL_GEMINI;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '1024';
    // gemini-2.0-flash tiene cap_WebSearch nativo registrado en el framework.
    // Sobreescribimos explícitamente para que el demo sea claro y autocontenido.
    Conn.Params.Values['ModelCaps']   := '[cap_WebSearch]';
    Conn.Params.Values['SessionCaps'] := '[cap_WebSearch]';
    Conn.SystemPrompt.Text :=
      'Eres un asistente informado. Usa búsqueda web para dar información ' +
      'actualizada y precisa. Responde en español y menciona las fuentes.';

    WriteLn('Pregunta: ', QUERY_DELPHI);
    WriteLn;

    Resp := Conn.AddMessageAndRun(QUERY_DELPHI, 'user', []);
    WriteLn(Resp);
  finally
    Conn.Free;
  end;
end;

// =============================================================================
//  B) Búsqueda via BRIDGE — Tavily + Gemini (gap analysis)
// =============================================================================
// ModelCaps=[] → el modelo no tiene búsqueda nativa en esta configuración.
// SessionCaps=[cap_WebSearch] → la necesitamos.
// Gap = SessionCaps − ModelCaps = [cap_WebSearch].
// MakerAI detecta el gap → llama Tavily → inyecta los resultados al LLM.
// =============================================================================

procedure DemoBridge;
var
  Conn  : TAiChatConnection;
  Tavily: TAiTavilyWebSearchTool;
  Resp  : string;
begin
  WriteLn('B) Búsqueda via BRIDGE — Tavily + Gemini (gap analysis)');
  WriteLn('   ModelCaps=[] + SessionCaps=[cap_WebSearch] → MakerAI usa Tavily');
  Separador;
  WriteLn;

  Tavily := TAiTavilyWebSearchTool.Create(nil);
  Conn   := TAiChatConnection.Create(nil);
  try
    // Configurar Tavily
    Tavily.ApiKey        := '@TAVILY_API_KEY';
    Tavily.MaxResults    := 5;
    Tavily.IncludeAnswer := True;   // Tavily genera una respuesta sintetizada propia

    // Configurar la conexión con gap analysis
    Conn.DriverName := 'Gemini';
    Conn.Model      := MODEL_GEMINI;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '1024';
    Conn.Params.Values['ModelCaps']    := '[]';              // sin búsqueda nativa
    Conn.Params.Values['SessionCaps']  := '[cap_WebSearch]'; // la necesitamos
    Conn.WebSearchTool := Tavily;  // MakerAI usará Tavily para cubrir el gap

    Conn.SystemPrompt.Text :=
      'Eres un asistente financiero. Analiza la información recibida de búsqueda ' +
      'y responde con datos concretos y actualizados. Responde en español.';

    WriteLn('Pregunta: ', QUERY_PRECIOS);
    WriteLn('[Tavily busca → resultados inyectados → Gemini analiza y responde]');
    WriteLn;

    Resp := Conn.AddMessageAndRun(QUERY_PRECIOS, 'user', []);
    WriteLn(Resp);
  finally
    Conn.WebSearchTool := nil;  // evitar que Conn libere Tavily (lo liberamos nosotros)
    Conn.Free;
    Tavily.Free;
  end;
end;

// =============================================================================
//  C) Búsqueda DIRECTA — Tavily sin LLM
// =============================================================================
// Llamada directa a la herramienta sin pasar por TAiChatConnection.
// ExecuteSearch es protected en la clase pero se expone vía IAiWebSearchTool.
// El resultado llega en ResMsg.Prompt: "## Respuesta" + "## Fuentes [1][2]..."
// Ideal para pipelines sin LLM, precarga de datos, o batch processing.
// =============================================================================

procedure DemoDirecta;
var
  Tavily: TAiTavilyWebSearchTool;
  ResMsg : TAiChatMessage;
  AskMsg : TAiChatMessage;
  Resultado: string;
begin
  WriteLn('C) Búsqueda DIRECTA — Tavily sin LLM');
  WriteLn('   (TAiTavilyWebSearchTool as IAiWebSearchTool).ExecuteSearch(...)');
  Separador;
  WriteLn;

  Tavily := TAiTavilyWebSearchTool.Create(nil);
  ResMsg := TAiChatMessage.Create;
  AskMsg := TAiChatMessage.Create;
  try
    Tavily.ApiKey        := '@TAVILY_API_KEY';
    Tavily.MaxResults    := 3;
    Tavily.IncludeAnswer := True;
    Tavily.SearchDepth   := tsdBasic;

    WriteLn('Buscando: "', QUERY_DELPHI, '"');
    WriteLn('(Sin LLM — resultado inmediato)');
    WriteLn;

    // ExecuteSearch es protected en la clase → se invoca via interfaz
    (Tavily as IAiWebSearchTool).ExecuteSearch(QUERY_DELPHI, ResMsg, AskMsg);

    Resultado := ResMsg.Prompt;

    WriteLn('─── ResMsg.Prompt ────────────────────────────────────────────');
    if Length(Resultado) > 1200 then
      WriteLn(Copy(Resultado, 1, 1200), #13#10'[...truncado para la demo]')
    else
      WriteLn(Resultado);
  finally
    ResMsg.Free;
    AskMsg.Free;
    Tavily.Free;
  end;
end;

// =============================================================================
begin
  try
    WriteLn;
    WriteLn('════════════════════════════════════════════════════════════════');
    WriteLn('  Cap. 19 — Búsqueda Web y Herramientas Externas');
    WriteLn('  Driver  : Gemini (gemini-2.0-flash)');
    WriteLn('  ChatTool: TAiTavilyWebSearchTool (MakerAI ChatTools)');
    WriteLn('════════════════════════════════════════════════════════════════');
    WriteLn;

    // A: Nativa — requiere GEMINI_API_KEY
    if KeyPresente('GEMINI_API_KEY') then
    begin
      DemoNativa;
      WriteLn;
    end
    else
      WriteLn('SKIP A: GEMINI_API_KEY no configurada.' + sLineBreak);

    // B: Bridge — requiere GEMINI_API_KEY + TAVILY_API_KEY
    if KeyPresente('GEMINI_API_KEY') and KeyPresente('TAVILY_API_KEY') then
    begin
      DemoBridge;
      WriteLn;
    end
    else
      WriteLn('SKIP B: GEMINI_API_KEY y/o TAVILY_API_KEY no configuradas.' + sLineBreak);

    // C: Directa — requiere solo TAVILY_API_KEY
    if KeyPresente('TAVILY_API_KEY') then
    begin
      DemoDirecta;
      WriteLn;
    end
    else
      WriteLn('SKIP C: TAVILY_API_KEY no configurada.' + sLineBreak);

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('ERROR: ', E.ClassName, ' — ', E.Message);
    end;
  end;

  WriteLn;
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
