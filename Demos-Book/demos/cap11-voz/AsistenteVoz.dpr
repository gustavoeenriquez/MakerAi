program AsistenteVoz;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — Capítulo 11: Voz — De audio a texto y de texto a voz
// =============================================================================
// Demuestra el flujo completo de voz con MakerAI: STT, TTS y pipeline
// integrado. Todos los demos usan únicamente la API de OpenAI.
//
//   Demo 1 — STT con TAIWhisper (standalone)
//     Transcribe un archivo de audio MP3/WAV a texto usando Whisper.
//     API directa: no requiere TAiChatConnection.
//
//   Demo 2 — TTS con TAiOpenAiAudio (standalone)
//     Convierte texto a voz y guarda el MP3 resultante en disco.
//     API directa: modelos tts-1 / tts-1-hd, 11 voces distintas.
//
//   Demo 3 — Asistente de voz completo (pipeline manual)
//     Audio MP3 → Whisper (STT) → gpt-4o-mini (LLM) → TTS → MP3
//     Muestra cómo encadenar las tres fases en una sola función.
//
//   Demo 4 — cmTranscription via TAiChatConnection
//     TAiChatConnection.ChatMode = cmTranscription + SpeechTool asignado.
//     Patrón limpio para integrar STT en aplicaciones con historial de chat.
//
// Nota sobre micrófono:
//   TAiVoiceMonitor (captura en tiempo real) y los modos Realtime se cubren
//   en el Capítulo 12. Aquí trabajamos exclusivamente con archivos de audio.
//
// Requisitos:
//   Todos los demos:  OPENAI_API_KEY en variables de entorno
//   Demos 1, 3, 4:   Un archivo de audio MP3 o WAV (ver AUDIO_PATH)
//
// Cómo probar sin archivo de audio propio:
//   Graba cualquier nota de voz desde tu teléfono y cópiala a AUDIO_PATH,
//   o bien ejecuta primero el Demo 2 (genera un MP3) y úsalo como entrada.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.OpenAi,    // DriverName = 'OpenAI' — se auto-registra al importar
  uMakerAi.Whisper,        // TAIWhisper — STT legacy, simple y directo
  uMakerAi.OpenAI.Audio;   // TAiOpenAiAudio — TTS moderno con enums tipados

const
  OPENAI_DRIVER = 'OpenAI';
  OPENAI_MODEL  = 'gpt-4o-mini';

  // Ruta del archivo de audio de entrada para demos de STT.
  // Formatos soportados por Whisper: mp3, mp4, mpeg, mpga, m4a, ogg, wav, webm.
  // Si el archivo no existe, los demos STT se omiten con instrucciones.
  AUDIO_PATH = 'C:\Users\Public\Documents\sample.mp3';

  // Archivos de salida para los demos TTS (se crean o sobreescriben).
  OUTPUT_TTS_DEMO2    = 'C:\Users\Public\Documents\tts_demo2.mp3';
  OUTPUT_TTS_PIPELINE = 'C:\Users\Public\Documents\tts_demo3.mp3';

// =============================================================================
procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

// =============================================================================
function OpenAiApiKeyDisponible: Boolean;
begin
  Result := GetEnvironmentVariable('OPENAI_API_KEY') <> '';
end;

// =============================================================================
//  Busca el archivo de audio en AUDIO_PATH y en el directorio del ejecutable.
// =============================================================================
function EncontrarAudio: string;
var
  RutaLocal: string;
  Exts     : array[0..5] of string;
  i        : Integer;
begin
  if TFile.Exists(AUDIO_PATH) then
    Exit(AUDIO_PATH);

  // Buscar en el directorio del ejecutable (formatos soportados por Whisper)
  Exts[0] := 'mp3';  Exts[1] := 'wav';  Exts[2] := 'ogg';
  Exts[3] := 'm4a';  Exts[4] := 'webm'; Exts[5] := 'mp4';
  for i := 0 to High(Exts) do
  begin
    RutaLocal := ExtractFilePath(ParamStr(0)) + 'sample.' + Exts[i];
    if TFile.Exists(RutaLocal) then
      Exit(RutaLocal);
  end;

  // Buscar cualquier MP3 o WAV en el directorio del ejecutable
  var Archivos := TDirectory.GetFiles(
    ExtractFilePath(ParamStr(0)), '*.mp3', TSearchOption.soTopDirectoryOnly);
  if Length(Archivos) > 0 then
    Exit(Archivos[0]);

  Archivos := TDirectory.GetFiles(
    ExtractFilePath(ParamStr(0)), '*.wav', TSearchOption.soTopDirectoryOnly);
  if Length(Archivos) > 0 then
    Exit(Archivos[0]);

  Result := '';
end;

// =============================================================================
//  Demo 1 — STT directo con TAIWhisper
//
//  TAIWhisper es la interfaz legacy para Whisper. Sencilla y directa:
//  carga el audio en un TMemoryStream y llama Transcription().
//  No requiere TAiChatConnection — es un componente independiente.
//
//  API:
//    Whisper.Transcription(Stream, NombreArchivo, Prompt): String
//    Whisper.Languaje := 'es';   // ISO-639-1; en blanco = auto-detectar
//    Whisper.Model   := 'whisper-1';  // único modelo público a Mayo 2026
// =============================================================================
procedure Demo1_STT_Whisper(const AudioPath: string);
var
  Whisper: TAIWhisper;
  Stream : TMemoryStream;
  Texto  : string;
begin
  Separador('Demo 1 — STT con TAIWhisper (transcripción directa)');

  Whisper := TAIWhisper.Create(nil);
  Stream  := TMemoryStream.Create;
  try
    Whisper.ApiKey  := '@OPENAI_API_KEY';
    Whisper.Model   := 'whisper-1';
    Whisper.Languaje := 'es';    // nota: propiedad con typo intencional en la fuente

    Stream.LoadFromFile(AudioPath);
    WriteLn('  Archivo  : ', ExtractFileName(AudioPath));
    WriteLn('  Tamaño   : ', Stream.Size div 1024, ' KB');
    WriteLn('');

    Write('  Transcribiendo... ');
    Texto := Whisper.Transcription(Stream, ExtractFileName(AudioPath), '');
    WriteLn('OK');
    WriteLn('');
    WriteLn('  Transcripción:');
    WriteLn('  ', Texto);
  finally
    Stream.Free;
    Whisper.Free;
  end;

  WriteLn('');
  WriteLn('→ Patrón: TAIWhisper.Transcription(Stream, NombreArchivo, HintPrompt).');
  WriteLn('  El parámetro "prompt" es un hint opcional para mejorar la precisión.');
  WriteLn('  Por ejemplo: ''Esta es una llamada médica'' mejora términos técnicos.');
end;

// =============================================================================
//  Demo 2 — TTS directo con TAiOpenAiAudio
//
//  TAiOpenAiAudio es la API moderna de OpenAI Audio. Usa enums tipados
//  en lugar de strings para evitar errores de escritura en modelo/voz.
//
//  API:
//    Audio.Speech(Texto): TMemoryStream   // devuelve el audio en memoria
//    Audio.TTSModel  := tts_1;            // o tts_1_hd, gpt_4o_mini_tts
//    Audio.TTSVoice  := tvAlloy;          // 11 voces: alloy, ash, ballad...
//    Audio.TTSResponseFormat := trfMp3;   // mp3, opus, aac, flac, wav, pcm
//    Audio.TTSSpeed  := 1.0;             // 0.25 a 4.0
// =============================================================================
procedure Demo2_TTS_OpenAiAudio;
const
  TEXTO_DEMO =
    'Hola. Soy un asistente de voz creado con MakerAI y Delphi. ' +
    'Esta es una demostración de síntesis de voz usando la API de OpenAI. ' +
    'El texto que escuchas fue generado en tiempo real desde una aplicación Delphi.';
var
  Audio : TAiOpenAiAudio;
  Stream: TMemoryStream;
begin
  Separador('Demo 2 — TTS con TAiOpenAiAudio (síntesis de voz)');

  WriteLn('  Texto   : ', Copy(TEXTO_DEMO, 1, 60), '...');
  WriteLn('  Longitud: ', Length(TEXTO_DEMO), ' caracteres');
  WriteLn('');

  Audio := TAiOpenAiAudio.Create(nil);
  try
    Audio.ApiKey            := '@OPENAI_API_KEY';
    Audio.TTSModel          := tts_1;          // estándar, baja latencia
    Audio.TTSVoice          := tvNova;          // voz femenina, clara y expresiva
    Audio.TTSResponseFormat := trfMp3;
    Audio.TTSSpeed          := 1.0;

    Write('  Generando audio (voz: Nova, modelo: tts-1)... ');
    Stream := Audio.Speech(TEXTO_DEMO);
    try
      if Stream.Size > 0 then
      begin
        Stream.SaveToFile(OUTPUT_TTS_DEMO2);
        WriteLn('OK');
        WriteLn('  Guardado: ', OUTPUT_TTS_DEMO2);
        WriteLn('  Tamaño  : ', Stream.Size div 1024, ' KB');
      end
      else
        WriteLn('ERROR — stream vacío');
    finally
      Stream.Free;
    end;
  finally
    Audio.Free;
  end;

  WriteLn('');
  WriteLn('→ Speech() devuelve TMemoryStream — guarda con SaveToFile().');
  WriteLn('  Voces disponibles: Alloy, Ash, Ballad, Coral, Echo, Fable,');
  WriteLn('                     Onyx, Nova, Sage, Shimmer, Verse.');
  WriteLn('  TTSInstructions   := ''Habla despacio y claro'' — tonalidad libre.');
end;

// =============================================================================
//  Demo 3 — Asistente de voz completo (pipeline manual)
//
//  Encadena tres fases sin magia:
//    1. STT: archivo de audio → texto (TAIWhisper)
//    2. LLM: texto → respuesta IA (TAiChatConnection con OpenAI)
//    3. TTS: respuesta → audio MP3 (TAiOpenAiAudio)
//
//  Este patrón "manual" da control total sobre cada fase.
//  El Capítulo 12 muestra cómo hacerlo en tiempo real con TAiVoiceMonitor.
// =============================================================================
procedure Demo3_PipelineCompleto(const AudioPath: string);
var
  Whisper : TAIWhisper;
  Audio   : TAiOpenAiAudio;
  Conn    : TAiChatConnection;
  Stream  : TMemoryStream;
  Pregunta: string;
  Respuesta: string;
begin
  Separador('Demo 3 — Asistente de voz: Audio → STT → LLM → TTS → Audio');

  WriteLn('  Fase 1 de 3: Transcribir audio de entrada...');

  // ── Fase 1: STT ─────────────────────────────────────────────────────────
  Whisper := TAIWhisper.Create(nil);
  Stream  := TMemoryStream.Create;
  try
    Whisper.ApiKey   := '@OPENAI_API_KEY';
    Whisper.Languaje := 'es';

    Stream.LoadFromFile(AudioPath);
    Write('    Transcribiendo ', ExtractFileName(AudioPath), '... ');
    Pregunta := Whisper.Transcription(Stream, ExtractFileName(AudioPath), '');
    WriteLn('OK');
    WriteLn('    Texto: ', Pregunta);
  finally
    Stream.Free;
    Whisper.Free;
  end;

  WriteLn('');
  WriteLn('  Fase 2 de 3: Enviar al LLM (gpt-4o-mini)...');

  // ── Fase 2: LLM ─────────────────────────────────────────────────────────
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := OPENAI_DRIVER;
    Conn.Model      := OPENAI_MODEL;
    Conn.Params.Values['ApiKey']       := '@OPENAI_API_KEY';
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '256';
    Conn.SystemPrompt.Text :=
      'Eres un asistente de voz amigable. Responde siempre en español, ' +
      'de forma concisa (máximo 3 oraciones) y en tono conversacional. ' +
      'Tus respuestas serán convertidas a voz, así que evita listas o formatos.';

    Conn.NewChat;
    Write('    Llamando a ', OPENAI_MODEL, '... ');
    Respuesta := Conn.AddMessageAndRun(Pregunta, 'user', []);
    WriteLn('OK');
    WriteLn('    Respuesta: ', Respuesta);
  finally
    Conn.Free;
  end;

  WriteLn('');
  WriteLn('  Fase 3 de 3: Sintetizar respuesta a voz...');

  // ── Fase 3: TTS ─────────────────────────────────────────────────────────
  Audio := TAiOpenAiAudio.Create(nil);
  try
    Audio.ApiKey   := '@OPENAI_API_KEY';
    Audio.TTSModel := tts_1;
    Audio.TTSVoice := tvAlloy;

    Write('    Generando audio... ');
    Stream := Audio.Speech(Respuesta);
    try
      if Stream.Size > 0 then
      begin
        Stream.SaveToFile(OUTPUT_TTS_PIPELINE);
        WriteLn('OK');
        WriteLn('    Guardado: ', OUTPUT_TTS_PIPELINE);
      end
      else
        WriteLn('ERROR — stream vacío');
    finally
      Stream.Free;
    end;
  finally
    Audio.Free;
  end;

  WriteLn('');
  WriteLn('→ Pipeline completo en 3 fases independientes.');
  WriteLn('  Cada fase puede sustituirse: Gemini STT, Groq LLM, ElevenLabs TTS...');
  WriteLn('  Cap.12 muestra el mismo flujo con entrada de micrófono en tiempo real.');
end;

// =============================================================================
//  Demo 4 — cmTranscription via TAiChatConnection
//
//  TAiChatConnection.ChatMode := cmTranscription delega la transcripción
//  al SpeechTool asignado, integrando STT en el historial de mensajes.
//
//  Ventaja vs. Demo 1: el resultado queda en Messages — útil cuando quieres
//  encadenar la transcripción con un turno de chat posterior.
//
//  Patrón:
//    Conn.SpeechTool  := Whisper;       // herramienta de audio
//    Conn.ChatMode    := cmTranscription;
//    MF.LoadFromFile('audio.mp3');       // FileCategory → Tfc_Audio
//    R := Conn.AddMessageAndRun('', 'user', [MF]);   // R = texto transcripto
// =============================================================================
procedure Demo4_cmTranscription(const AudioPath: string);
var
  Whisper: TAIWhisper;
  Conn   : TAiChatConnection;
  MF     : TAiMediaFile;
  Texto  : string;
begin
  Separador('Demo 4 — cmTranscription via TAiChatConnection');

  Whisper := TAIWhisper.Create(nil);
  Conn    := TAiChatConnection.Create(nil);
  MF      := TAiMediaFile.Create;
  try
    // Configurar el SpeechTool
    Whisper.ApiKey   := '@OPENAI_API_KEY';
    Whisper.Languaje := 'es';

    // Configurar la conexión con el modo de transcripción forzado
    Conn.DriverName              := OPENAI_DRIVER;
    Conn.Model                   := OPENAI_MODEL;
    Conn.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.SpeechTool  := Whisper;          // ← asignar herramienta STT
    Conn.ChatMode    := cmTranscription;  // ← forzar modo STT

    // Cargar el audio en un TAiMediaFile
    // LoadFromFile detecta la extensión → FileCategory = Tfc_Audio (ord=2)
    MF.LoadFromFile(AudioPath);
    WriteLn('  Archivo : ', ExtractFileName(AudioPath));
    WriteLn('  Categoría: Tfc_Audio (ord=', Ord(MF.FileCategory), ')');
    WriteLn('');

    Conn.NewChat;
    Write('  Transcribiendo via ChatMode... ');

    // El array [MF] lleva el audio; el texto del prompt queda vacío.
    // IMPORTANTE: AddMessageAndRun transfiere propiedad de MF a la conexión
    // (FMediaFiles es TObjectList — libera MF en Conn.Free → NewChat).
    // Poner MF := nil evita el double-free en el bloque finally.
    Texto := Conn.AddMessageAndRun('', 'user', [MF]);
    MF := nil;   // ownership transferida — Conn.Free libera MF vía NewChat
    WriteLn('OK');
    WriteLn('');
    WriteLn('  Transcripción:');
    WriteLn('  ', Texto);
    WriteLn('');
    WriteLn('  Historial de mensajes: ', Conn.Messages.Count,
            ' (user + assistant)');
  finally
    MF.Free;     // nil-safe: solo libera si AddMessageAndRun no llegó a ejecutarse
    Conn.Free;   // → NewChat → libera mensajes (y MF si MF <> nil)
    Whisper.Free;
  end;

  WriteLn('');
  WriteLn('→ cmTranscription integra STT en el historial (Conn.Messages).');
  WriteLn('  Tras la transcripción, puedes cambiar ChatMode a cmConversation');
  WriteLn('  y llamar AddMessageAndRun() — el LLM verá el texto transcripto.');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
var
  AudioPath   : string;
  TieneApiKey : Boolean;
  TieneAudio  : Boolean;
begin
  WriteLn('=== MakerAI — Capítulo 11: Voz (STT y TTS) ===');
  WriteLn('Requisito: OPENAI_API_KEY en variables de entorno.');
  WriteLn('Demos STT: un archivo de audio MP3/WAV en ', AUDIO_PATH);

  // Verificar disponibilidad
  TieneApiKey := OpenAiApiKeyDisponible;
  AudioPath   := EncontrarAudio;
  TieneAudio  := AudioPath <> '';

  WriteLn('');
  WriteLn('  Estado:');
  if TieneApiKey then
    WriteLn('  [OK] OPENAI_API_KEY detectado')
  else
    WriteLn('  [--] OPENAI_API_KEY no configurado → todos los demos se omitirán');

  if TieneAudio then
    WriteLn('  [OK] Audio encontrado: ', ExtractFileName(AudioPath))
  else
  begin
    WriteLn('  [--] Audio no encontrado (', AUDIO_PATH, ')');
    WriteLn('       Los demos 1, 3 y 4 se omitirán.');
    WriteLn('       Ejecuta el Demo 2 primero para generar un MP3,');
    WriteLn('       luego cópialo a la ruta esperada.');
  end;

  if not TieneApiKey then
  begin
    WriteLn('');
    WriteLn('  Configura OPENAI_API_KEY y vuelve a ejecutar.');
    Exit;
  end;

  // ── Demos STT (requieren archivo de audio) ───────────────────────────────
  if TieneAudio then
    Demo1_STT_Whisper(AudioPath)
  else
  begin
    WriteLn('');
    WriteLn('--- Demo 1 omitido (audio no encontrado) ---');
  end;

  // ── Demo TTS (siempre disponible si hay API key) ─────────────────────────
  Demo2_TTS_OpenAiAudio;

  // ── Pipeline completo (requiere audio) ───────────────────────────────────
  if TieneAudio then
    Demo3_PipelineCompleto(AudioPath)
  else
  begin
    WriteLn('');
    WriteLn('--- Demo 3 omitido (audio no encontrado) ---');
    WriteLn('  Tip: usa el MP3 generado por Demo 2 como entrada STT.');
  end;

  // ── cmTranscription (requiere audio) ─────────────────────────────────────
  if TieneAudio then
    Demo4_cmTranscription(AudioPath)
  else
  begin
    WriteLn('');
    WriteLn('--- Demo 4 omitido (audio no encontrado) ---');
  end;

  WriteLn('');
  WriteLn(StringOfChar('=', 62));
  WriteLn('  Demo completado.');
  WriteLn('  Archivos de audio generados:');
  if TFile.Exists(OUTPUT_TTS_DEMO2)    then WriteLn('    ', OUTPUT_TTS_DEMO2);
  if TFile.Exists(OUTPUT_TTS_PIPELINE) then WriteLn('    ', OUTPUT_TTS_PIPELINE);
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
