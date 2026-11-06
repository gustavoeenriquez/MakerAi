program WhisperDemo;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI -- Capitulo 12: Speech-to-Text: Whisper y proveedores alternativos
// =============================================================================
// Demuestra la transcripcion batch de audio con TAIWhisper y TAiOpenAiAudio.
// Todos los demos usan el mismo archivo de audio de entrada.
//
//   Demo 1 -- TAIWhisper + OpenAI (whisper-1) -- caso base
//     Transcripcion directa: cargar stream, llamar Transcription().
//     La API key se toma de OPENAI_API_KEY; el modelo por defecto es whisper-1.
//
//   Demo 2 -- TAIWhisper + Groq (whisper-large-v3-turbo)
//     Identico al Demo 1. Solo cambia Url y ApiKey.
//     Groq ejecuta Whisper en LPU: hasta 216x tiempo real.
//     Requiere GROQ_API_KEY configurado.
//
//   Demo 3 -- TAIWhisper + whisper.cpp local (privacidad total)
//     Identico al Demo 1. Solo cambia Url y ApiKey (puede ser cualquier string).
//     Requiere el servidor whisper.cpp corriendo en WHISPER_LOCAL_URL.
//     Si el servidor no esta disponible, el demo muestra instrucciones y continua.
//
//   Demo 4 -- Timestamps por palabra con verbose_json
//     ResponseFormat = 'verbose_json' + timestamp_granularities = 'word'.
//     Solo disponible con whisper-1 y proveedores Whisper clasicos.
//     Parsea el JSON de respuesta y muestra [tiempo] palabra por palabra.
//
//   Demo 5 -- TAiOpenAiAudio + gpt-4o-transcribe + logprobs
//     Componente moderno. TranscriptionLogprobs = True solicita la confianza
//     por token. Tokens con Exp(logprob) < 0.8 se marcan como inciertos [?token?].
//
//   Demo 6 -- Diarizacion con gpt-4o-transcribe-diarize
//     Identifica quien hablo que fragmento. Sin Python, sin GPU, en la nube.
//     Parsea Result.RawJson -> 'segments' -> speaker + text + start.
//
// Requisitos:
//   Demos 1, 4, 5, 6:  OPENAI_API_KEY en variables de entorno
//   Demo 2:            GROQ_API_KEY en variables de entorno
//   Demo 3:            Servidor whisper.cpp en WHISPER_LOCAL_URL
//   Todos:             Un archivo de audio en AUDIO_PATH (MP3, WAV, M4A, OGG...)
//
// Para conseguir un archivo de audio de prueba:
//   Ejecuta primero el Demo 2 del capitulo 11 (TTS) que genera un MP3,
//   o graba cualquier nota de voz desde tu telefono y copiala a AUDIO_PATH.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Math,
  System.Generics.Collections,
  WinApi.Windows,
  uMakerAi.Core,
  uMakerAi.Whisper,
  uMakerAi.OpenAI.Audio;

const
  // Ruta del archivo de audio de entrada.
  // Formatos soportados: mp3, mp4, m4a, ogg, wav, webm, flac.
  AUDIO_PATH = 'C:\Users\Public\Documents\sample.mp3';

  // URL del servidor whisper.cpp local (Demo 3).
  // Iniciar con: whisper-server -m models/ggml-turbo.bin -l es --port 8080
  WHISPER_LOCAL_URL = 'http://localhost:8080/v1/';

// =============================================================================

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

function EncontrarAudio: string;
var
  RutaLocal: string;
  Exts: array[0..5] of string;
  i: Integer;
  Archivos: TArray<string>;
begin
  if TFile.Exists(AUDIO_PATH) then
    Exit(AUDIO_PATH);

  // Buscar en el directorio del ejecutable
  Exts[0] := 'mp3';  Exts[1] := 'wav';  Exts[2] := 'ogg';
  Exts[3] := 'm4a';  Exts[4] := 'webm'; Exts[5] := 'mp4';
  for i := 0 to High(Exts) do
  begin
    RutaLocal := ExtractFilePath(ParamStr(0)) + 'sample.' + Exts[i];
    if TFile.Exists(RutaLocal) then
      Exit(RutaLocal);
  end;

  Archivos := TDirectory.GetFiles(
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
//  Demo 1 -- TAIWhisper + OpenAI (caso base)
//
//  Transcription(Stream, NombreArchivo, HintPrompt): String
//  El HintPrompt mejora la precision en terminologia tecnica o nombres propios.
//  Ejemplo: 'Esta es una llamada medica' mejora 'glomerulonefritis'.
// =============================================================================
procedure Demo1_OpenAI(const AudioPath: string);
var
  Whisper: TAIWhisper;
  Stream : TMemoryStream;
  Texto  : string;
begin
  Separador('Demo 1 -- TAIWhisper + OpenAI (whisper-1)');

  Whisper := TAIWhisper.Create(nil);
  Stream  := TMemoryStream.Create;
  try
    Whisper.ApiKey         := '@OPENAI_API_KEY';  // default
    Whisper.Model          := 'whisper-1';
    Whisper.Languaje       := 'es';
    Whisper.ResponseFormat := 'text';

    Stream.LoadFromFile(AudioPath);
    WriteLn('  Archivo : ', ExtractFileName(AudioPath));
    WriteLn('  Tamano  : ', Stream.Size div 1024, ' KB');
    WriteLn('');

    Write('  Transcribiendo (whisper-1)... ');
    Texto := Whisper.Transcription(Stream, ExtractFileName(AudioPath), '');
    WriteLn('OK');
    WriteLn('');
    WriteLn('  Resultado:');
    WriteLn('  ', Texto);
  finally
    Stream.Free;
    Whisper.Free;
  end;

  WriteLn('');
  WriteLn('-> Patron: TAIWhisper.Transcription(Stream, NombreArchivo, HintPrompt).');
  WriteLn('   Cambia solo Url + ApiKey para apuntar a Groq o whisper.cpp.');
end;

// =============================================================================
//  Demo 2 -- El mismo codigo con Groq (hasta 216x tiempo real)
//
//  El codigo es identico al Demo 1. Solo cambian Url y ApiKey.
//  Groq usa LPU (Language Processing Unit) para correr Whisper a maxima
//  velocidad: 60 minutos de audio se transcriben en ~13 segundos.
// =============================================================================
procedure Demo2_Groq(const AudioPath: string);
var
  Whisper: TAIWhisper;
  Stream : TMemoryStream;
  Texto  : string;
begin
  Separador('Demo 2 -- TAIWhisper + Groq (whisper-large-v3-turbo)');

  Whisper := TAIWhisper.Create(nil);
  Stream  := TMemoryStream.Create;
  try
    // *** Unica diferencia respecto al Demo 1: Url y ApiKey ***
    Whisper.Url    := 'https://api.groq.com/openai/v1/';
    Whisper.ApiKey := '@GROQ_API_KEY';
    Whisper.Model  := 'whisper-large-v3-turbo';  // 216x tiempo real
    Whisper.Languaje       := 'es';
    Whisper.ResponseFormat := 'text';

    Stream.LoadFromFile(AudioPath);
    WriteLn('  Archivo : ', ExtractFileName(AudioPath));
    WriteLn('');

    Write('  Transcribiendo (whisper-large-v3-turbo en Groq LPU)... ');
    Texto := Whisper.Transcription(Stream, ExtractFileName(AudioPath), '');
    WriteLn('OK');
    WriteLn('');
    WriteLn('  Resultado:');
    WriteLn('  ', Texto);
  finally
    Stream.Free;
    Whisper.Free;
  end;

  WriteLn('');
  WriteLn('-> whisper-large-v3-turbo: calidad proxima a large-v3, 216x mas rapido.');
  WriteLn('   Tier gratuito Groq: hasta 2 horas de audio por hora de reloj.');
end;

// =============================================================================
//  Demo 3 -- El mismo codigo con whisper.cpp local (sin internet)
//
//  whisper.cpp expone la misma API HTTP que OpenAI. Sin datos que salgan
//  de tu maquina. Ideal para entornos con restricciones de privacidad.
//
//  Para iniciar el servidor local (Windows):
//    whisper-server.exe -m models\ggml-turbo.bin -l es --port 8080
//
//  El demo detecta si el servidor no esta disponible y muestra instrucciones.
// =============================================================================
procedure Demo3_Local(const AudioPath: string);
var
  Whisper: TAIWhisper;
  Stream : TMemoryStream;
  Texto  : string;
begin
  Separador('Demo 3 -- TAIWhisper + whisper.cpp local (privacidad total)');
  WriteLn('  Servidor esperado en: ', WHISPER_LOCAL_URL);
  WriteLn('');

  Whisper := TAIWhisper.Create(nil);
  Stream  := TMemoryStream.Create;
  try
    // *** Unica diferencia: Url local; ApiKey puede ser cualquier string ***
    Whisper.Url    := WHISPER_LOCAL_URL;
    Whisper.ApiKey := 'local';  // whisper.cpp no valida la clave
    Whisper.Model  := 'turbo';  // nombre del modelo cargado en el servidor
    Whisper.Languaje       := 'es';
    Whisper.ResponseFormat := 'text';

    Stream.LoadFromFile(AudioPath);

    Write('  Transcribiendo (whisper.cpp local)... ');
    try
      Texto := Whisper.Transcription(Stream, ExtractFileName(AudioPath), '');
      WriteLn('OK');
      WriteLn('');
      WriteLn('  Resultado:');
      WriteLn('  ', Texto);
    except
      on E: Exception do
      begin
        WriteLn('SERVIDOR NO DISPONIBLE');
        WriteLn('');
        WriteLn('  Para ejecutar este demo:');
        WriteLn('  1. Descargar whisper.cpp: https://github.com/ggml-org/whisper.cpp');
        WriteLn('  2. Compilar: cmake -B build && cmake --build build --config Release');
        WriteLn('  3. Descargar modelo: bash models/download-ggml-model.sh turbo');
        WriteLn('  4. Iniciar servidor:');
        WriteLn('       whisper-server.exe -m models\ggml-turbo.bin -l es --port 8080');
        WriteLn('');
        WriteLn('  El codigo Delphi no cambia. Solo la URL.');
      end;
    end;
  finally
    Stream.Free;
    Whisper.Free;
  end;

  WriteLn('');
  WriteLn('-> Sin datos que salgan de tu maquina. Sin costo por peticion.');
  WriteLn('   Cuantizacion int8 (ggml-turbo-q5_0.bin): tamano ~600 MB.');
end;

// =============================================================================
//  Demo 4 -- Timestamps por palabra con verbose_json
//
//  ResponseFormat = 'verbose_json' devuelve el texto completo mas un array
//  'words' con el tiempo de inicio de cada palabra. Util para:
//    - Karaoke / sincronizacion de subtitulos
//    - Resaltar palabras mientras se reproduce el audio
//    - Detectar pausas largas en el discurso
//
//  Solo disponible con whisper-1 (y Groq whisper-large-v3).
//  Los modelos gpt-4o-transcribe no soportan verbose_json.
// =============================================================================
procedure Demo4_Timestamps(const AudioPath: string);
var
  Whisper : TAIWhisper;
  Stream  : TMemoryStream;
  JSON    : string;
  JObj    : TJSONObject;
  JWords  : TJSONArray;
  JW      : TJSONObject;
  i       : Integer;
  Palabra : string;
  Inicio  : Double;
begin
  Separador('Demo 4 -- Timestamps por palabra (verbose_json)');

  Whisper := TAIWhisper.Create(nil);
  Stream  := TMemoryStream.Create;
  try
    Whisper.ApiKey                  := '@OPENAI_API_KEY';
    Whisper.Model                   := 'whisper-1';
    Whisper.Languaje                := 'es';
    Whisper.ResponseFormat          := 'verbose_json';
    Whisper.timestamp_granularities := 'word';

    Stream.LoadFromFile(AudioPath);
    WriteLn('  Pidiendo timestamps por palabra...');
    WriteLn('');

    Write('  Transcribiendo... ');
    JSON := Whisper.Transcription(Stream, ExtractFileName(AudioPath), '');
    WriteLn('OK');
    WriteLn('');

    JObj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
    if not Assigned(JObj) then
    begin
      WriteLn('  ERROR: respuesta no es JSON valido.');
      Exit;
    end;
    try
      // Texto completo
      WriteLn('  Texto completo:');
      WriteLn('  ', JObj.GetValue<string>('text'));
      WriteLn('');

      // Timestamps por palabra
      JWords := JObj.GetValue<TJSONArray>('words');
      if Assigned(JWords) and (JWords.Count > 0) then
      begin
        WriteLn('  Palabras con timestamp (primeras 20):');
        for i := 0 to Min(JWords.Count - 1, 19) do
        begin
          JW     := JWords.Items[i] as TJSONObject;
          Inicio  := JW.GetValue<Double>('start');
          Palabra := JW.GetValue<string>('word');
          WriteLn(Format('    [%5.2fs]  %s', [Inicio, Trim(Palabra)]));
        end;
        if JWords.Count > 20 then
          WriteLn(Format('    ... (%d palabras en total)', [JWords.Count]));
      end
      else
        WriteLn('  (sin palabras en respuesta — verifica que el audio tenga voz)');
    finally
      JObj.Free;
    end;
  finally
    Stream.Free;
    Whisper.Free;
  end;

  WriteLn('');
  WriteLn('-> verbose_json tambien contiene "segments" con timestamps por frase.');
  WriteLn('   Groq whisper-large-v3 tambien soporta verbose_json.');
  WriteLn('   gpt-4o-transcribe NO soporta verbose_json (se degrada a json).');
end;

// =============================================================================
//  Demo 5 -- TAiOpenAiAudio + gpt-4o-transcribe + logprobs
//
//  gpt-4o-transcribe tiene WER de 2.46% (casi 4x mejor que whisper-1).
//  Con TranscriptionLogprobs=True, la API devuelve la probabilidad de cada
//  token. Esto permite marcar palabras de baja confianza para revision humana.
//
//  Prob = Exp(logprob):
//    Prob = 1.0  -> certeza total
//    Prob = 0.8  -> buena confianza (umbral comun)
//    Prob = 0.5  -> mitad de probabilidad
//    Prob < 0.3  -> probable error de transcripcion
// =============================================================================
procedure Demo5_Logprobs(const AudioPath: string);
var
  Audio  : TAiOpenAiAudio;
  MF     : TAiMediaFile;
  Res    : TTranscriptionResult;
  JToken : TJSONObject;
  Token  : string;
  LogP   : Double;
  Prob   : Double;
  i      : Integer;
begin
  Separador('Demo 5 -- TAiOpenAiAudio + gpt-4o-transcribe + logprobs');

  Audio := TAiOpenAiAudio.Create(nil);
  MF    := TAiMediaFile.Create;
  try
    Audio.ApiKey                 := '@OPENAI_API_KEY';
    Audio.TranscriptionModel     := tmGpt4oTranscribe;  // WER 2.46%
    Audio.TranscriptionLanguage  := 'es';
    Audio.TranscriptionLogprobs  := True;               // pedir confianza por token

    MF.LoadFromFile(AudioPath);
    WriteLn('  Modelo  : gpt-4o-transcribe (WER 2.46%)');
    WriteLn('  Logprobs: True (confianza por token)');
    WriteLn('');

    Write('  Transcribiendo... ');
    Res := Audio.Transcribe(MF);
    try
      WriteLn('OK');
      WriteLn('');
      WriteLn('  Texto completo:');
      WriteLn('  ', Res.Text);
      WriteLn('');

      if Assigned(Res.Logprobs) and (Res.Logprobs.Count > 0) then
      begin
        WriteLn('  Analisis de confianza (? = Prob < 0.8):');
        Write('  ');
        for i := 0 to Res.Logprobs.Count - 1 do
        begin
          JToken := Res.Logprobs.Items[i] as TJSONObject;
          Token  := JToken.GetValue<string>('token');
          LogP   := JToken.GetValue<Double>('logprob');
          Prob   := Exp(LogP);

          if Prob < 0.8 then
            Write('[?' + Token + '?]')  // baja confianza
          else
            Write(Token);               // alta confianza
        end;
        WriteLn;
        WriteLn('');
        WriteLn(Format('  Total tokens: %d', [Res.Logprobs.Count]));
      end
      else
        WriteLn('  (logprobs no devueltos -- verifica que el modelo sea gpt-4o-transcribe)');
    finally
      Res.Free;
    end;
  finally
    MF.Free;
    Audio.Free;
  end;

  WriteLn('');
  WriteLn('-> Logprob = ln(probabilidad). Exp(logprob) da la probabilidad 0..1.');
  WriteLn('   Umbral recomendado: 0.8. Tokens bajo ese umbral merecen revision.');
  WriteLn('   Solo disponible con gpt-4o-transcribe y gpt-4o-mini-transcribe.');
end;

// =============================================================================
//  Demo 6 -- Diarizacion: quien hablo que
//
//  gpt-4o-transcribe-diarize identifica los distintos hablantes y asocia
//  cada fragmento a un speaker. Sin Python, sin GPU, en la nube de OpenAI.
//
//  Ideal para:
//    - Actas de reuniones ("SPEAKER_0 propuso..., SPEAKER_1 respondio...")
//    - Entrevistas y podcasts
//    - Call centers (cliente vs. agente)
//
//  Nota: requiere audio con dos o mas voces distintas para que la
//  diarizacion sea visible en la salida.
// =============================================================================
procedure Demo6_Diarizacion(const AudioPath: string);
var
  Audio : TAiOpenAiAudio;
  MF    : TAiMediaFile;
  Res   : TTranscriptionResult;
  JSegs : TJSONArray;
  JSeg  : TJSONObject;
  i     : Integer;
  Speaker, Texto: string;
  Inicio: Double;
begin
  Separador('Demo 6 -- Diarizacion (gpt-4o-transcribe-diarize)');

  Audio := TAiOpenAiAudio.Create(nil);
  MF    := TAiMediaFile.Create;
  try
    Audio.ApiKey                        := '@OPENAI_API_KEY';
    Audio.TranscriptionModel            := tmGpt4oDiarize;
    Audio.TranscriptionResponseFormat   := trfDiarizedJson;
    Audio.TranscriptionLanguage         := 'es';

    MF.LoadFromFile(AudioPath);
    WriteLn('  Modelo  : gpt-4o-transcribe-diarize');
    WriteLn('  Formato : diarized_json');
    WriteLn('');

    Write('  Transcribiendo con identificacion de hablantes... ');
    Res := Audio.Transcribe(MF);
    try
      WriteLn('OK');
      WriteLn('');
      WriteLn('  Transcripcion completa:');
      WriteLn('  ', Res.Text);
      WriteLn('');

      if Assigned(Res.RawJson) then
      begin
        JSegs := Res.RawJson.GetValue('segments') as TJSONArray;  // nil-safe (non-generic)
        if Assigned(JSegs) and (JSegs.Count > 0) then
        begin
          WriteLn('  Segmentos por hablante:');
          for i := 0 to JSegs.Count - 1 do
          begin
            JSeg    := JSegs.Items[i] as TJSONObject;
            Speaker := JSeg.GetValue<string>('speaker');
            Texto   := JSeg.GetValue<string>('text');
            Inicio  := JSeg.GetValue<Double>('start');
            WriteLn(Format('    [%5.1fs]  %-12s %s', [Inicio, Speaker + ':', Trim(Texto)]));
          end;
        end
        else
        begin
          WriteLn('  (un solo hablante detectado o audio demasiado corto)');
          WriteLn('  Tip: usa un audio con dos o mas voces distintas para ver');
          WriteLn('       la diarizacion en accion.');
        end;
      end;
    finally
      Res.Free;
    end;
  finally
    MF.Free;
    Audio.Free;
  end;

  WriteLn('');
  WriteLn('-> Los labels SPEAKER_0, SPEAKER_1... son automaticos.');
  WriteLn('   Para renombrarlos, mapea Speaker -> nombre real en tu aplicacion.');
  WriteLn('   Alternativa open source: WhisperX + pyannote (ver seccion 12.6).');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
var
  AudioPath     : string;
  TieneOpenAI   : Boolean;
  TieneGroq     : Boolean;
begin
  SetConsoleOutputCP(65001);  // UTF-8 en consola Windows
  WriteLn('=== MakerAI -- Capitulo 12: Whisper y STT Batch ===');
  WriteLn('Demos: 6 formas de transcribir audio en Delphi.');
  WriteLn('');

  // Verificar recursos disponibles
  AudioPath   := EncontrarAudio;
  TieneOpenAI := GetEnvironmentVariable('OPENAI_API_KEY') <> '';
  TieneGroq   := GetEnvironmentVariable('GROQ_API_KEY') <> '';

  WriteLn('  Estado:');
  if TieneOpenAI then
    WriteLn('  [OK] OPENAI_API_KEY detectado')
  else
    WriteLn('  [--] OPENAI_API_KEY no configurado -- demos 1,4,5,6 omitidos');

  if TieneGroq then
    WriteLn('  [OK] GROQ_API_KEY detectado')
  else
    WriteLn('  [--] GROQ_API_KEY no configurado -- demo 2 omitido');

  if AudioPath <> '' then
    WriteLn('  [OK] Audio encontrado: ', ExtractFileName(AudioPath))
  else
  begin
    WriteLn('  [--] Audio no encontrado en: ', AUDIO_PATH);
    WriteLn('       Copia un archivo MP3/WAV a esa ruta y vuelve a ejecutar.');
    WriteLn('       O ejecuta Demo 2 del cap11 (TTS) para generar uno.');
    WriteLn('');
    WriteLn('Presiona Enter para salir...');
    ReadLn;
    Exit;
  end;

  // Demos con OpenAI (1, 4, 5, 6)
  if TieneOpenAI then
  begin
    Demo1_OpenAI(AudioPath);
    Demo4_Timestamps(AudioPath);
    Demo5_Logprobs(AudioPath);
    Demo6_Diarizacion(AudioPath);
  end
  else
  begin
    WriteLn('');
    WriteLn('--- Demos 1, 4, 5, 6 omitidos (OPENAI_API_KEY no configurado) ---');
  end;

  // Demo con Groq (2)
  if TieneGroq then
    Demo2_Groq(AudioPath)
  else
  begin
    WriteLn('');
    WriteLn('--- Demo 2 omitido (GROQ_API_KEY no configurado) ---');
    WriteLn('    Registrate gratis en console.groq.com para obtener tu clave.');
  end;

  // Demo local (3) -- siempre se intenta, maneja el error internamente
  Demo3_Local(AudioPath);

  WriteLn('');
  WriteLn(StringOfChar('=', 62));
  WriteLn('  Demos completados.');
  WriteLn('  Cap.12 cubre tambien Realtime STT (WebSocket).');
  WriteLn('  Ver: Demos/Console/Demos09-Realtime/01-RealtimeSTT/');
  WriteLn(StringOfChar('=', 62));
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.ClassName, ' -- ', E.Message);
  end;
  WriteLn('');
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
