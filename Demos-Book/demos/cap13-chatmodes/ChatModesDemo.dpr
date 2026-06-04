program ChatModesDemo;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI -- Capitulo 13: Modos de Chat -- El Orquestador en Detalle
// =============================================================================
//
// Demuestra los distintos TAiChatMode de TAiChatConnection:
//
//   Demo 1 -- cmConversation  (default)    Chat de texto con OpenAI
//   Demo 2 -- cmWebSearch     (forzado)    Busqueda web con citas (OpenAI Search)
//   Demo 3 -- cmSpeechGeneration (forzado) Texto a voz (OpenAI TTS)
//   Demo 4 -- cmTranscription  (forzado)   Transcripcion de audio (Whisper)
//   Demo 5 -- Cambio de modo en runtime    Una conexion, tres modos
//
// Variables de entorno requeridas:
//   OPENAI_API_KEY   -- Todos los demos
//   Demo 4:    requiere un archivo de audio en AUDIO_PATH.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  Winapi.Windows,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.OpenAI.Audio,
  uMakerAi.OpenAI.WebSearch,
  uMakerAi.OpenAI.Audio.Tool,
  uMakerAi.Chat.Initializations;

const
  AUDIO_PATH = 'C:\Users\Public\Documents\sample.mp3';

// =============================================================================

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

function OutFile(const AName: string): string;
begin
  Result := TPath.Combine(TPath.GetTempPath, AName);
end;

function EncontrarAudio: string;
var
  RutaLocal: string;
  Exts: array[0..4] of string;
  i: Integer;
  Archivos: TArray<string>;
begin
  if TFile.Exists(AUDIO_PATH) then
    Exit(AUDIO_PATH);

  Exts[0] := 'mp3'; Exts[1] := 'wav'; Exts[2] := 'm4a';
  Exts[3] := 'ogg'; Exts[4] := 'webm';
  for i := 0 to High(Exts) do
  begin
    RutaLocal := ExtractFilePath(ParamStr(0)) + 'sample.' + Exts[i];
    if TFile.Exists(RutaLocal) then
      Exit(RutaLocal);
  end;

  Archivos := TDirectory.GetFiles(
    ExtractFilePath(ParamStr(0)), '*.mp3', TSearchOption.soTopDirectoryOnly);
  if Length(Archivos) > 0 then Exit(Archivos[0]);

  Result := '';
end;

// =============================================================================
//  Demo 1 -- cmConversation (modo inteligente por defecto)
//
//  El ChatMode por defecto. El framework aplica gap analysis automaticamente
//  segun ModelCaps vs SessionCaps. Sin capacidades especiales configuradas,
//  usa InternalRunCompletions como salida (chat de texto normal).
//
//  Clave: AddMessageAndRun devuelve el texto directamente en modo sincrono.
// =============================================================================
procedure Demo1_Conversation;
var
  Conn: TAiChatConnection;
  Resp: string;
begin
  Separador('Demo 1 -- cmConversation (modo inteligente por defecto)');

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName                   := 'OpenAI';
    Conn.Params.Values['ApiKey']      := '@OPENAI_API_KEY';
    Conn.Model                        := 'gpt-4.1-mini';
    Conn.Params.Values['Asynchronous']:= 'False';
    // ChatMode por defecto es cmConversation -- no es necesario especificarlo
    // Conn.ChatMode := cmConversation;

    Conn.SystemPrompt.Text :=
      'Eres un asistente tecnico especializado en Delphi. ' +
      'Responde de forma concisa, en una o dos frases.';

    WriteLn('  Prompt: Cual es la principal ventaja de Delphi frente a otras IDEs?');
    WriteLn('');

    Write('  Esperando respuesta... ');
    Resp := Conn.AddMessageAndRun(
      'Cual es la principal ventaja de Delphi frente a otras IDEs?', 'user', []);
    WriteLn('OK');
    WriteLn('');
    WriteLn('  Respuesta:');
    WriteLn('  ', Resp);
  finally
    Conn.Free;
  end;

  WriteLn('');
  WriteLn('-> cmConversation aplica gap analysis: ModelCaps vs SessionCaps.');
  WriteLn('   Sin gaps configurados, siempre llama a InternalRunCompletions.');
  WriteLn('   Agrega SessionCaps=[cap_GenImage] + ImageTool para activar imagenes.');
end;

// =============================================================================
//  Demo 2 -- cmWebSearch (busqueda web forzada con citas)
//
//  Modo forzado: InternalRunWebSearch directo, sin gap analysis.
//  TAiOpenAiWebSearchTool usa gpt-4o-mini-search-preview, que tiene busqueda
//  web integrada. El resultado llega en Resp y las citas en
//  Conn.Messages.Last.Citations.
//
//  Diferencia con cmConversation + cap_WebSearch:
//    cmWebSearch     -> resultado crudo del tool (sin sintesis del LLM)
//    cmConversation  -> LLM sintetiza el resultado antes de responder
// =============================================================================
procedure Demo2_WebSearch;
var
  Conn:      TAiChatConnection;
  SearchTool:TAiOpenAiWebSearchTool;
  Resp:      string;
  Msg:       TAiChatMessage;
  Cit:       TAiMsgCitation;
  Src:       TAiCitationSource;
  nCitas:    Integer;
begin
  Separador('Demo 2 -- cmWebSearch (busqueda directa con citas)');

  SearchTool := TAiOpenAiWebSearchTool.Create(nil);
  Conn       := TAiChatConnection.Create(nil);
  try
    Conn.DriverName                   := 'OpenAI';
    Conn.Params.Values['ApiKey']      := '@OPENAI_API_KEY';
    Conn.ChatMode                     := cmWebSearch;
    Conn.Params.Values['Asynchronous']:= 'False';
    Conn.WebSearchTool                := SearchTool;

    SearchTool.ApiKey := '@OPENAI_API_KEY';

    WriteLn('  Consulta: "Delphi 13 Florence novedades 2026"');
    WriteLn('  Modelo de busqueda: gpt-4o-mini-search-preview');
    WriteLn('');

    Write('  Buscando... ');
    Resp := Conn.AddMessageAndRun('Delphi 13 Florence novedades 2026', 'user', []);
    WriteLn('OK');
    WriteLn('');

    if Resp = '' then
    begin
      WriteLn('  ADVERTENCIA: la busqueda retorno resultado vacio.');
      WriteLn('  Causas posibles:');
      WriteLn('    - OPENAI_API_KEY no configurado o sin acceso a los modelos search.');
      WriteLn('    - Cuota de API agotada o error de red.');
    end
    else
    begin
      WriteLn('  Resultado (primeros 300 chars):');
      WriteLn('  ', Copy(Resp, 1, 300));
    end;

    nCitas := 0;
    if Conn.Messages.Count > 0 then
    begin
      Msg := Conn.Messages.Last;
      if Msg.Citations.Count > 0 then
      begin
        WriteLn('');
        WriteLn('  Fuentes encontradas:');
        for Cit in Msg.Citations do
          for Src in Cit.Sources do
          begin
            Inc(nCitas);
            if Src.DataSource.Title <> '' then
              WriteLn(Format('    [%d] %s', [nCitas, Src.DataSource.Title]));
            if Src.DataSource.Url <> '' then
              WriteLn(Format('        %s', [Copy(Src.DataSource.Url, 1, 80)]));
          end;
      end;
    end;
    if nCitas = 0 then
      WriteLn('  (sin citas en la respuesta)');
  finally
    Conn.Free;
    SearchTool.Free;
  end;

  WriteLn('');
  WriteLn('-> TAiOpenAiWebSearchTool usa gpt-4o-mini-search-preview.');
  WriteLn('   Las citas estan en Conn.Messages.Last.Citations.');
  WriteLn('   Solo requiere OPENAI_API_KEY con acceso a modelos de busqueda.');
end;

// =============================================================================
//  Demo 3 -- cmSpeechGeneration (texto a voz directo)
//
//  Modo forzado: InternalRunSpeechGeneration directo, sin completions.
//  El texto del usuario se convierte a audio SIN pasar por el LLM.
//  El audio queda en Conn.Messages.Last.MediaFiles[0].
// =============================================================================
procedure Demo3_SpeechGeneration;
var
  Conn:      TAiChatConnection;
  SpeechTool:TAiOpenAiSpeechTool;
  Resp:      string;
  Msg:       TAiChatMessage;
  MF:        TAiMediaFile;
  AudioPath: string;
  i:         Integer;
begin
  Separador('Demo 3 -- cmSpeechGeneration (texto a voz)');

  SpeechTool := TAiOpenAiSpeechTool.Create(nil);
  Conn       := TAiChatConnection.Create(nil);
  try
    Conn.DriverName                   := 'OpenAI';
    Conn.Params.Values['ApiKey']      := '@OPENAI_API_KEY';
    Conn.ChatMode                     := cmSpeechGeneration;
    Conn.Params.Values['Asynchronous']:= 'False';
    Conn.SpeechTool                   := SpeechTool;

    SpeechTool.ApiKey   := '@OPENAI_API_KEY';
    SpeechTool.TTSVoice := tvAlloy;
    SpeechTool.TTSModel := tts_1;

    WriteLn('  Texto: "Bienvenido a MakerAI, la plataforma de IA para Delphi."');
    WriteLn('  Voz: Alloy | Modelo: TTS-1');
    WriteLn('');

    Write('  Generando audio... ');
    Resp := Conn.AddMessageAndRun(
      'Bienvenido a MakerAI, la plataforma de inteligencia artificial para Delphi.',
      'user', []);
    WriteLn('OK  [', Resp, ']');

    if Conn.Messages.Count > 0 then
    begin
      Msg := Conn.Messages.Last;
      for i := 0 to Msg.MediaFiles.Count - 1 do
      begin
        MF := Msg.MediaFiles[i];
        if MF.FileCategory = Tfc_Audio then
        begin
          AudioPath := OutFile('cap13_demo3_tts.mp3');
          MF.SaveToFile(AudioPath);
          WriteLn('  Audio guardado: ', AudioPath,
                  ' (', MF.Bytes div 1024, ' KB)');
          Break;
        end;
      end;
      if Msg.MediaFiles.Count = 0 then
        WriteLn('  (sin archivo de audio en respuesta)');
    end;
  finally
    Conn.Free;
    SpeechTool.Free;
  end;

  WriteLn('');
  WriteLn('-> cmSpeechGeneration: el texto del usuario se convierte directamente.');
  WriteLn('   Sin LLM intermedio. El audio queda en Conn.Messages.Last.MediaFiles.');
end;

// =============================================================================
//  Demo 4 -- cmTranscription (transcripcion de audio con Whisper)
//
//  Modo forzado: InternalRunTranscription sobre el primer archivo de audio
//  de los adjuntos. El texto transcrito llega directamente como retorno de
//  AddMessageAndRun (modo sincrono).
//
//  TAiOpenAiSpeechTool implementa TTS y STT en un solo componente.
// =============================================================================
procedure Demo4_Transcription(const AudioPath: string);
var
  Conn:      TAiChatConnection;
  SpeechTool:TAiOpenAiSpeechTool;
  AudioMF:   TAiMediaFile;
  Resp:      string;
begin
  Separador('Demo 4 -- cmTranscription (transcripcion de audio)');

  SpeechTool := TAiOpenAiSpeechTool.Create(nil);
  Conn       := TAiChatConnection.Create(nil);
  try
    Conn.DriverName                   := 'OpenAI';
    Conn.Params.Values['ApiKey']      := '@OPENAI_API_KEY';
    Conn.ChatMode                     := cmTranscription;
    Conn.Params.Values['Asynchronous']:= 'False';
    Conn.SpeechTool                   := SpeechTool;

    SpeechTool.ApiKey                := '@OPENAI_API_KEY';
    SpeechTool.TranscriptionModel    := tmWhisper1;
    SpeechTool.TranscriptionLanguage := 'es';

    // AudioMF se crea sin free explicito: AddMessageAndRun lo agrega a
    // AskMsg.MediaFiles (TObjectList con OwnsObjects=True) y Conn.Free
    // libera esa cadena completa. Free manual aqui causaria double-free.
    AudioMF := TAiMediaFile.Create;
    AudioMF.LoadFromFile(AudioPath);

    WriteLn('  Archivo : ', ExtractFileName(AudioPath));
    WriteLn('  Modelo  : whisper-1');
    WriteLn('');

    Write('  Transcribiendo... ');
    Resp := Conn.AddMessageAndRun('', 'user', [AudioMF]);
    WriteLn('OK');
    WriteLn('');
    WriteLn('  Transcripcion:');
    WriteLn('  ', Resp);
  finally
    Conn.Free;
    // AudioMF NO se libera aqui: la conexion es la duena
    SpeechTool.Free;
  end;

  WriteLn('');
  WriteLn('-> cmTranscription procesa el PRIMER archivo de audio de los adjuntos.');
  WriteLn('   El texto transcrito llega como retorno de AddMessageAndRun (modo sync).');
  WriteLn('   TAiOpenAiSpeechTool implementa TTS + STT en un solo componente.');
end;

// =============================================================================
//  Demo 5 -- Cambio de modo en tiempo de ejecucion
//
//  Demuestra que ChatMode es solo una propiedad: cambiarla entre llamadas
//  cambia el comportamiento sin recrear la conexion.
//
//    Ronda 1 -- cmConversation  -> texto del LLM (OpenAI)
//    Ronda 2 -- cmWebSearch     -> resultado de busqueda (OpenAI search tool)
//    Ronda 3 -- cmConversation  -> de vuelta a texto (OpenAI)
// =============================================================================
procedure Demo5_CambioModo;
var
  Conn:      TAiChatConnection;
  SearchTool:TAiOpenAiWebSearchTool;
  Resp:      string;
  nCitas:    Integer;
  Msg:       TAiChatMessage;
  Cit:       TAiMsgCitation;
  Src:       TAiCitationSource;
begin
  Separador('Demo 5 -- Cambio de ChatMode en runtime');

  SearchTool := TAiOpenAiWebSearchTool.Create(nil);
  SearchTool.ApiKey := '@OPENAI_API_KEY';
  Conn       := TAiChatConnection.Create(nil);
  try
    // --- Ronda 1: modo conversacion (texto OpenAI) ---
    // WebSearchTool NO asignado: cmConversation no lo usa;
    // asignarlo antes del cambio de driver contamina el driver OpenAI.
    Conn.DriverName                   := 'OpenAI';
    Conn.Params.Values['ApiKey']      := '@OPENAI_API_KEY';
    Conn.Model                        := 'gpt-4.1-mini';
    Conn.Params.Values['Asynchronous']:= 'False';
    Conn.ChatMode                     := cmConversation;

    WriteLn('  [1] ChatMode = cmConversation  (OpenAI)');
    Write('      Pregunta: "Explica en una frase que es TAiChatMode." ... ');
    Resp := Conn.AddMessageAndRun(
      'Explica en una frase que es TAiChatMode en MakerAI para Delphi.',
      'user', []);
    WriteLn('OK');
    WriteLn('      Resp: ', Copy(Resp, 1, 120));
    WriteLn('');

    // --- Ronda 2: cambio a busqueda web (OpenAI search tool) ---
    Conn.Messages.Clear;
    Conn.DriverName                   := 'OpenAI';
    Conn.Params.Values['ApiKey']      := '@OPENAI_API_KEY';
    Conn.Params.Values['Asynchronous']:= 'False';
    Conn.WebSearchTool                := SearchTool;  // asignar AQUI, solo para esta ronda
    Conn.ChatMode                     := cmWebSearch;

    WriteLn('  [2] ChatMode = cmWebSearch  (mismo objeto Conn, distinto modo)');
    Write('      Consulta: "RAD Studio Delphi novedades 2026" ... ');
    Resp := Conn.AddMessageAndRun('RAD Studio Delphi novedades 2026', 'user', []);
    WriteLn('OK');
    WriteLn('      Resp: ', Copy(Resp, 1, 120));

    nCitas := 0;
    if Conn.Messages.Count > 0 then
    begin
      Msg := Conn.Messages.Last;
      for Cit in Msg.Citations do
        for Src in Cit.Sources do
          Inc(nCitas);
    end;
    WriteLn(Format('      Citas: %d', [nCitas]));
    WriteLn('');

    // --- Ronda 3: de vuelta a conversacion (OpenAI) ---
    Conn.Messages.Clear;
    Conn.WebSearchTool                := nil;  // limpiar ANTES del cambio de driver
    Conn.DriverName                   := 'OpenAI';
    Conn.Params.Values['ApiKey']      := '@OPENAI_API_KEY';
    Conn.Params.Values['Asynchronous']:= 'False';
    Conn.ChatMode                     := cmConversation;

    WriteLn('  [3] ChatMode = cmConversation  (de vuelta al modo inicial)');
    Write('      Pregunta: "Cual es la diferencia entre OOP y procedural?" ... ');
    Resp := Conn.AddMessageAndRun(
      'En una frase, cual es la principal diferencia entre programacion orientada a objetos y programacion procedural?',
      'user', []);
    WriteLn('OK');
    WriteLn('      Resp: ', Copy(Resp, 1, 120));
  finally
    Conn.Free;
    SearchTool.Free;
  end;

  WriteLn('');
  WriteLn('-> ChatMode es solo una propiedad: cambiarla entre llamadas cambia');
  WriteLn('   el comportamiento sin recrear la conexion.');
  WriteLn('   Regla: asignar/limpiar ChatTools por ronda segun el modo.');
  WriteLn('   WebSearchTool solo debe estar asignado en la ronda cmWebSearch.');
  WriteLn('   Messages.Clear evita que contexto de distintos modos se mezcle.');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
var
  TieneOpenAI: Boolean;
  AudioPath:   string;
begin
  SetConsoleOutputCP(65001);
  WriteLn('=== MakerAI -- Capitulo 13: Modos de Chat ===');
  WriteLn('Demos: 5 modos de TAiChatConnection.ChatMode');
  WriteLn('');

  TieneOpenAI := GetEnvironmentVariable('OPENAI_API_KEY') <> '';
  AudioPath   := EncontrarAudio;

  WriteLn('  Estado del entorno:');
  if TieneOpenAI then WriteLn('  [OK] OPENAI_API_KEY detectado')
  else            WriteLn('  [--] OPENAI_API_KEY no configurado -- todos los demos omitidos');
  if AudioPath <> '' then
    WriteLn('  [OK] Audio: ', ExtractFileName(AudioPath))
  else
    WriteLn('  [--] Audio no encontrado en ', AUDIO_PATH, ' -- demo 4 omitido');

  if TieneOpenAI then
  begin
    try Demo1_Conversation;
    except on E: Exception do WriteLn('  [Demo 1 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else WriteLn(#13#10'--- Demo 1 omitido (OPENAI_API_KEY no configurado) ---');

  if TieneOpenAI then
  begin
    try Demo2_WebSearch;
    except on E: Exception do WriteLn('  [Demo 2 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else WriteLn(#13#10'--- Demo 2 omitido (OPENAI_API_KEY no configurado) ---');

  if TieneOpenAI then
  begin
    try Demo3_SpeechGeneration;
    except on E: Exception do WriteLn('  [Demo 3 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else WriteLn(#13#10'--- Demo 3 omitido (OPENAI_API_KEY no configurado) ---');

  if TieneOpenAI and (AudioPath <> '') then
  begin
    try Demo4_Transcription(AudioPath);
    except on E: Exception do WriteLn('  [Demo 4 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else
  begin
    WriteLn('');
    WriteLn('--- Demo 4 omitido ---');
    if not TieneOpenAI then WriteLn('    OPENAI_API_KEY no configurado');
    if AudioPath = '' then
    begin
      WriteLn('    Audio no encontrado. Copia un MP3/WAV a: ', AUDIO_PATH);
      WriteLn('    O ejecuta Demo 3 (genera un MP3) y copialo a esa ruta.');
    end;
  end;

  if TieneOpenAI then
  begin
    try Demo5_CambioModo;
    except on E: Exception do WriteLn('  [Demo 5 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else WriteLn(#13#10'--- Demo 5 omitido (OPENAI_API_KEY no configurado) ---');

  WriteLn('');
  WriteLn(StringOfChar('=', 62));
  WriteLn('  Demos completados.');
  WriteLn('  Ver cap13-chatmodes.md para la documentacion completa.');
  WriteLn(StringOfChar('=', 62));
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      WriteLn('ERROR FATAL: ', E.ClassName, ' -- ', E.Message);
  end;
  WriteLn('');
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
