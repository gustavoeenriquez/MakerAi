program ChatToolsDemo;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — Capítulo 20: ChatTools Comunitarios
// Demo: Cuatro modalidades de uso de ChatTools
// =============================================================================
//
//   Demo 4: Instanciar herramientas y mostrar propiedades (sin API key)
//   Demo 1: Búsqueda directa sin LLM (requiere TAVILY_API_KEY)
//   Demo 2: Visión directa sin LLM   (requiere OPENAI_API_KEY + imagen)
//   Demo 3: Integración completa con Ollama + ChatTools (requiere ambas keys)
//
// Patrón de llamada directa (sin LLM):
//   ResMsg := TAiChatMessage.Create;
//   AskMsg := TAiChatMessage.Create;
//   (Tool as IInterface).ExecuteXxx(input, ResMsg, AskMsg);
//   WriteLn(ResMsg.Prompt);
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Ollama,           // auto-registra driver 'Ollama'
  uMakerAi.Chat.Tools,            // IAiWebSearchTool, IAiVisionTool, IAiSpeechTool
  uMakerAi.ChatTools.Tavily,
  uMakerAi.ChatTools.OpenAIVision,
  uMakerAi.ChatTools.OpenAISpeech;

// ---------- Demo 1: Búsqueda directa sin LLM ----------
procedure DemoDirectSearch;
var
  Search: TAiTavilyWebSearchTool;
  ResMsg: TAiChatMessage;
  AskMsg: TAiChatMessage;
  Resultado: string;
begin
  WriteLn('=== DEMO 1: Búsqueda directa (sin LLM) ===');
  WriteLn('Herramienta: TAiTavilyWebSearchTool');
  WriteLn;

  Search := TAiTavilyWebSearchTool.Create(nil);
  ResMsg := TAiChatMessage.Create;
  AskMsg := TAiChatMessage.Create;
  try
    Search.ApiKey     := '@TAVILY_API_KEY';
    Search.MaxResults := 3;

    WriteLn('Buscando: "últimas novedades Delphi 2025"');
    // ExecuteSearch es protected → se invoca via interfaz
    (Search as IAiWebSearchTool).ExecuteSearch(
      'últimas novedades Delphi 2025', ResMsg, AskMsg);

    Resultado := ResMsg.Prompt;

    WriteLn('Resultados (primeros 600 chars):');
    if Length(Resultado) > 600 then
      WriteLn(Copy(Resultado, 1, 600), #13#10'...')
    else
      WriteLn(Resultado);
  finally
    ResMsg.Free;
    AskMsg.Free;
    Search.Free;
  end;
end;

// ---------- Demo 2: Visión directa sin LLM ----------
procedure DemoDirectVision(const ImagePath: string);
var
  Vision: TAiOpenAIVisionTool;
  Media : TAiMediaFile;
  ResMsg: TAiChatMessage;
  AskMsg: TAiChatMessage;
begin
  WriteLn('=== DEMO 2: Visión directa (sin LLM) ===');
  WriteLn('Herramienta: TAiOpenAIVisionTool');
  WriteLn;

  if not FileExists(ImagePath) then
  begin
    WriteLn('Imagen no encontrada: ', ImagePath);
    WriteLn('(Se requiere una imagen .jpg o .png para este demo)');
    Exit;
  end;

  Vision := TAiOpenAIVisionTool.Create(nil);
  Media  := TAiMediaFile.Create;
  ResMsg := TAiChatMessage.Create;
  AskMsg := TAiChatMessage.Create;
  try
    Vision.ApiKey    := '@OPENAI_API_KEY';
    Vision.Detail    := 'high';
    Vision.MaxTokens := 512;

    Media.LoadFromFile(ImagePath);
    WriteLn('Analizando imagen: ', ImagePath);

    // ExecuteImageDescription es protected → se invoca via interfaz
    (Vision as IAiVisionTool).ExecuteImageDescription(Media, ResMsg, AskMsg);

    WriteLn('Descripción:');
    WriteLn(ResMsg.Prompt);
  finally
    ResMsg.Free;
    AskMsg.Free;
    Vision.Free;
    Media.Free;
  end;
end;

// ---------- Demo 3: Integración completa con TAiChatConnection ----------
procedure DemoIntegrated;
var
  AiConn: TAiChatConnection;
  Search: TAiTavilyWebSearchTool;
  Speech: TAiOpenAISpeechTool;
  Resp  : string;
begin
  WriteLn('=== DEMO 3: Integración completa (Ollama + ChatTools) ===');
  WriteLn('Modelo base: Ollama llama3.2 (local, sin costo)');
  WriteLn('ChatTools: Tavily (búsqueda) + OpenAI Speech');
  WriteLn;

  AiConn := TAiChatConnection.Create(nil);
  Search := TAiTavilyWebSearchTool.Create(nil);
  Speech := TAiOpenAISpeechTool.Create(nil);
  try
    // Modelo base: Ollama local
    AiConn.DriverName := 'Ollama';
    AiConn.Model      := 'llama3.2';
    AiConn.Params.Values['Url']          := 'http://localhost:11434/';
    AiConn.Params.Values['Asynchronous'] := 'False';

    // ChatTool 1: búsqueda web — activa el gap cap_WebSearch automáticamente
    Search.ApiKey        := '@TAVILY_API_KEY';
    AiConn.WebSearchTool := Search;

    // ChatTool 2: voz
    Speech.ApiKey     := '@OPENAI_API_KEY';
    Speech.Language   := 'es';
    AiConn.SpeechTool := Speech;

    WriteLn('Pregunta: "¿Qué novedades trae Delphi 13 Florence?"');
    WriteLn('(Gap Analysis detectará cap_WebSearch y llamará Tavily)');
    WriteLn;

    Resp := AiConn.AddMessageAndRun(
      '¿Qué novedades trae Delphi 13 Florence? Busca información actualizada.',
      'user', []);

    WriteLn('Respuesta del modelo:');
    WriteLn(Resp);
  finally
    AiConn.WebSearchTool := nil;  // evitar doble liberación
    AiConn.SpeechTool    := nil;
    AiConn.Free;
    Search.Free;
    Speech.Free;
  end;
end;

// ---------- Demo 4: Instanciar y mostrar herramientas disponibles ----------
procedure DemoShowTools;
var
  Search: TAiTavilyWebSearchTool;
  Vision: TAiOpenAIVisionTool;
  Speech: TAiOpenAISpeechTool;
begin
  WriteLn('=== DEMO 4: Instanciar y mostrar herramientas disponibles ===');
  WriteLn;

  Search := TAiTavilyWebSearchTool.Create(nil);
  Vision := TAiOpenAIVisionTool.Create(nil);
  Speech := TAiOpenAISpeechTool.Create(nil);
  try
    Search.ApiKey := '@TAVILY_API_KEY';
    Vision.ApiKey := '@OPENAI_API_KEY';
    Speech.ApiKey := '@OPENAI_API_KEY';

    WriteLn('Herramientas instanciadas correctamente:');
    WriteLn('  [WebSearch] ', Search.ClassName, ' — MaxResults: ', Search.MaxResults);
    WriteLn('  [Vision]    ', Vision.ClassName, ' — Detail: ',     Vision.Detail);
    WriteLn('  [Speech]    ', Speech.ClassName, ' — Language: "',  Speech.Language, '" (vacío=autodetect)');
    WriteLn;
    WriteLn('Patrón de asignación a TAiChatConnection:');
    WriteLn('  AiConn.WebSearchTool := Search;  // activa gap cap_WebSearch');
    WriteLn('  AiConn.VisionTool    := Vision;  // activa gap cap_Image');
    WriteLn('  AiConn.SpeechTool    := Speech;  // activa gap cap_Speech');
    WriteLn;
    WriteLn('Patrón de llamada directa sin LLM:');
    WriteLn('  ResMsg := TAiChatMessage.Create;');
    WriteLn('  AskMsg := TAiChatMessage.Create;');
    WriteLn('  (Search as IAiWebSearchTool).ExecuteSearch(query, ResMsg, AskMsg);');
    WriteLn('  WriteLn(ResMsg.Prompt);');
  finally
    Search.Free;
    Vision.Free;
    Speech.Free;
  end;
end;

// =============================================================================
begin
  WriteLn('=================================================');
  WriteLn('  Cap. 20 — ChatTools Comunitarios: Demos');
  WriteLn('=================================================');
  WriteLn;

  try
    // Demo 4: sin API keys
    DemoShowTools;
    WriteLn;

    // Demo 1: requiere TAVILY_API_KEY
    if GetEnvironmentVariable('TAVILY_API_KEY') <> '' then
    begin
      DemoDirectSearch;
      WriteLn;
    end
    else
      WriteLn('SKIP Demo 1: TAVILY_API_KEY no configurada' + sLineBreak);

    // Demo 2: requiere OPENAI_API_KEY + imagen de prueba
    if GetEnvironmentVariable('OPENAI_API_KEY') <> '' then
    begin
      DemoDirectVision('test_image.jpg');
      WriteLn;
    end
    else
      WriteLn('SKIP Demo 2: OPENAI_API_KEY no configurada' + sLineBreak);

    // Demo 3: requiere ambas keys + Ollama corriendo
    if (GetEnvironmentVariable('TAVILY_API_KEY') <> '') and
       (GetEnvironmentVariable('OPENAI_API_KEY') <> '') then
    begin
      Write('Ejecutar Demo 3 (integración Ollama+ChatTools)? [S/N]: ');
      var LResp: string;
      ReadLn(LResp);
      if SameText(Trim(LResp), 'S') then
        DemoIntegrated;
    end
    else
      WriteLn('SKIP Demo 3: requiere TAVILY_API_KEY y OPENAI_API_KEY');

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('ERROR: ', E.ClassName, ' — ', E.Message);
    end;
  end;

  WriteLn;
  WriteLn('=================================================');
  WriteLn('  Presiona Enter para salir...');
  ReadLn;
end.
