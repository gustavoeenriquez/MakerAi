program ChatTools;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — Capítulo 7: ChatTools — Superpoderes para Cualquier Modelo
// =============================================================================
// Demuestra el sistema ChatTools de MakerAI: el mecanismo que permite dar
// capacidades adicionales a modelos que no las tienen nativamente.
//
// Concepto central: Gap = SessionCaps - ModelCaps
//   Cuando el modelo no tiene una capacidad que la sesión necesita,
//   MakerAI busca un ChatTool asignado para cubrir esa brecha.
//   El modelo nunca sabe que está recibiendo ayuda externa.
//
// Demuestra:
//   - Demo 1: Bridge de visión — Groq (texto puro) "ve" imágenes via Ollama
//             gemma3:4b describe la imagen → Groq razona sobre la descripción
//   - Demo 2: Uso directo del VisionTool — sin pasar por el LLM
//             TAiOllamaVisionTool.DescribeFromFile() como llamada directa
//   - Demo 3: Patrón Camino A/B — cómo fluye la información en ChatTools
//
// Prerequisitos:
//   GROQ_API_KEY  → variable de entorno (para el LLM principal en Demo 1)
//   Ollama        → corriendo en localhost:11434 con modelo gemma3:4b
//                   Instalar: https://ollama.com
//                   Descargar modelo: ollama pull gemma3:4b
//   El Demo 3 es conceptual y no requiere servicios externos.
// =============================================================================

uses
  System.SysUtils,
  System.IOUtils,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Groq,       // DriverName = 'Groq' — LLM principal (solo texto)
  uMakerAi.Ollama.Vision;   // TAiOllamaVisionTool — bridge de visión via Ollama

const
  // LLM principal — texto puro, ModelCaps = []  (sin visión nativa)
  LLM_DRIVER  = 'Groq';
  LLM_MODEL   = 'llama-3.3-70b-versatile';
  LLM_API_KEY = '@GROQ_API_KEY';

  // VisionTool — modelo de visión en Ollama
  OLLAMA_URL   = 'http://localhost:11434/';
  VISION_MODEL = 'gemma3:4b';   // Alternativas: 'llava:latest', 'moondream:latest'

  // Imagen de prueba (presente en todas las instalaciones de Windows)
  IMG_PATH = 'C:\Windows\Web\Wallpaper\Windows\img0.jpg';

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

// =============================================================================
//  Verifica si Ollama está corriendo en el puerto local.
// =============================================================================
function OllamaDisponible: Boolean;
var
  Client: THTTPClient;
begin
  Result := False;
  Client := THTTPClient.Create;
  try
    try
      Client.Get(OLLAMA_URL);
      Result := True;
    except
      Result := False;
    end;
  finally
    Client.Free;
  end;
end;

// =============================================================================
//  Demo 1 — Bridge de visión: Groq (texto puro) + Ollama (visión)
//
//  Groq llama-3.3-70b-versatile no tiene capacidad de visión (ModelCaps = []).
//  Al asignar TAiOllamaVisionTool, MakerAI activa el bridge automáticamente:
//
//    1. App envía: imagen + "describe 3 elementos visuales"
//    2. MakerAI detecta: Groq no puede ver imágenes (Gap)
//    3. VisionTool llama a gemma3:4b en Ollama:
//       → gemma3 describe la imagen en texto
//    4. MakerAI inyecta la descripción como contexto para Groq:
//       → "Descripción de la imagen: [texto de gemma3]"
//    5. Groq razona sobre la descripción y responde
//
//  Groq nunca recibió la imagen directamente. Solo texto.
// =============================================================================
procedure Demo1_VisionBridge;
var
  Conn      : TAiChatConnection;
  VisionTool: TAiOllamaVisionTool;
  MF        : TAiMediaFile;
  Files     : TAiMediaFilesArray;
  R         : string;
begin
  Separador('Demo 1 — Bridge de visión (Groq + Ollama)');

  if not TFile.Exists(IMG_PATH) then
  begin
    WriteLn('  Imagen no encontrada: ', IMG_PATH);
    WriteLn('  Ajusta IMG_PATH a una imagen JPEG en tu sistema.');
    Exit;
  end;

  WriteLn('  LLM principal: ', LLM_DRIVER, ' / ', LLM_MODEL, '  [ModelCaps = []]');
  WriteLn('  VisionTool   : Ollama / ', VISION_MODEL, '         [ve la imagen]');
  WriteLn('  Imagen       : ', TPath.GetFileName(IMG_PATH));
  WriteLn('');

  // El VisionTool: describe imágenes usando gemma3:4b en Ollama
  VisionTool := TAiOllamaVisionTool.Create(nil);
  VisionTool.Url   := OLLAMA_URL;
  VisionTool.Model := VISION_MODEL;
  // Prompt que se envía al modelo de visión (en español)
  VisionTool.Prompt := 'Describe esta imagen en detalle en español.';

  // La conexión principal con Groq — texto puro
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := LLM_DRIVER;
    Conn.Model      := LLM_MODEL;
    Conn.Params.Values['ApiKey']       := LLM_API_KEY;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '512';
    // SessionCaps indica que esta sesión necesita visión.
    // Gap = [cap_Image] - [] = [cap_Image] → MakerAI activa el VisionTool bridge.
    Conn.Params.Values['SessionCaps']  := '[cap_Image]';

    // Asignar el bridge: Groq ahora "ve" a través de Ollama
    Conn.VisionTool := VisionTool;

    Conn.SystemPrompt.Text :=
      'Eres un asistente de análisis visual. Recibirás descripciones en texto ' +
      'de imágenes y responderás preguntas sobre ellas. Responde en español, conciso.';

    // Cargar la imagen en un TAiMediaFile
    // NOTA: AddMessageAndRun toma ownership de los TAiMediaFile en el array.
    // No llamar MF.Free después — el mensaje interno los libera.
    MF := TAiMediaFile.Create;
    MF.LoadFromFile(IMG_PATH);
    SetLength(Files, 1);
    Files[0] := MF;

    // Enviar imagen + pregunta al LLM
    // El bridge se activa automáticamente: gemma3 describe → Groq razona
    Write('  Procesando (gemma3 describe imagen, Groq razona)... ');
    R := Conn.AddMessageAndRun(
      'Describe 3 elementos visuales principales que observas en la imagen.',
      'user', Files);
    WriteLn('OK');
    WriteLn('');
    WriteLn('  Respuesta de Groq (razonó sobre descripción de gemma3):');
    WriteLn('  ', R);

  finally
    Conn.Free;
    VisionTool.Free;
  end;

  WriteLn('');
  WriteLn('→ Groq nunca recibió la imagen directamente.');
  WriteLn('  Recibió la descripción en texto generada por gemma3:4b en Ollama.');
  WriteLn('  Cambiar de VisionTool = cambiar VisionTool.Model. Nada más.');
end;

// =============================================================================
//  Demo 2 — Uso directo del VisionTool (sin LLM)
//
//  Los ChatTools son componentes independientes que pueden usarse directamente,
//  sin necesidad de configurar un TAiChatConnection completo.
//
//  TAiOllamaVisionTool.DescribeFromFile() es un método de clase que:
//    - Crea una conexión temporal a Ollama
//    - Envía la imagen al modelo de visión
//    - Devuelve la descripción como string
//    - Cierra la conexión
//
//  Útil cuando solo necesitas describir una imagen sin conversación.
// =============================================================================
procedure Demo2_DirectTool;
var
  Descripcion: string;
begin
  Separador('Demo 2 — Uso directo del VisionTool (sin LLM)');

  if not TFile.Exists(IMG_PATH) then
  begin
    WriteLn('  Imagen no encontrada: ', IMG_PATH);
    Exit;
  end;

  WriteLn('  Llamando TAiOllamaVisionTool.DescribeFromFile() directamente...');
  WriteLn('  (Sin TAiChatConnection — el VisionTool opera de forma independiente)');
  WriteLn('');

  // Método de clase: no necesita Create/Free, llama directamente al modelo
  Write('  Describiendo imagen con gemma3:4b... ');
  try
    Descripcion := TAiOllamaVisionTool.DescribeFromFile(
      IMG_PATH,
      'Describe brevemente esta imagen en español (máximo 2 oraciones).',
      OLLAMA_URL);
    WriteLn('OK');
    WriteLn('');
    WriteLn('  Descripcion de gemma3:4b:');
    WriteLn('  ', Descripcion);
  except
    on E: Exception do
    begin
      WriteLn('ERROR');
      WriteLn('  ', E.Message);
    end;
  end;

  WriteLn('');
  WriteLn('→ Los ChatTools son componentes reutilizables e independientes.');
  WriteLn('  Puedes llamarlos directamente en cualquier parte de tu código.');
end;

// =============================================================================
//  Demo 3 — Camino A vs Camino B (conceptual)
//
//  Cuando un ChatTool procesa un archivo, tiene dos caminos para comunicar
//  el resultado. La elección determina si el LLM ve el resultado o no.
//
//  Este demo no requiere servicios externos — es una explicación ilustrada.
// =============================================================================
procedure Demo3_CaminoAvsB;
begin
  Separador('Demo 3 — Patron Camino A vs Camino B');

  WriteLn('  Cuando un ChatTool procesa un archivo, elige cómo comunicar');
  WriteLn('  el resultado. Esta decisión define completamente el flujo de datos.');
  WriteLn('');
  WriteLn('  CAMINO A — resultado va al LLM (herramienta de comprension):');
  WriteLn('  ─────────────────────────────────────────────────────────────');
  WriteLn('');
  WriteLn('    aMediaFile.Transcription := TextoExtraido;  // LLM verá esto');
  WriteLn('    aMediaFile.Procesado      := True;           // señal al orquestador');
  WriteLn('');
  WriteLn('    Flujo: Imagen ──> VisionTool describe ──> texto en Transcription');
  WriteLn('                                               ──> LLM recibe ──> razona');
  WriteLn('');
  WriteLn('    Herramientas Camino A (comprensión de entrada):');
  WriteLn('      VisionTool  → imagen  convierte a descripción de texto');
  WriteLn('      SpeechTool  → audio   convierte a transcripción de texto');
  WriteLn('      PdfTool     → PDF     convierte a texto extraído');
  WriteLn('');
  WriteLn('  CAMINO B — resultado va al usuario (herramienta de generacion):');
  WriteLn('  ─────────────────────────────────────────────────────────────');
  WriteLn('');
  WriteLn('    MFAudio := TAiMediaFile.Create;');
  WriteLn('    MFAudio.LoadFromStream(''respuesta.mp3'', AudioStream);');
  WriteLn('    ResMsg.AddMediaFile(MFAudio);   // va directo al usuario');
  WriteLn('');
  WriteLn('    Flujo: Texto LLM ──> TTSTool genera audio ──> MP3');
  WriteLn('                                                 ──> usuario recibe');
  WriteLn('                                   (el LLM nunca vio el audio)');
  WriteLn('');
  WriteLn('    Herramientas Camino B (generación de salida):');
  WriteLn('      SpeechTool  → texto  convierte a audio generado (MP3/WAV)');
  WriteLn('      ImageTool   → prompt convierte a imagen generada (PNG/JPG)');
  WriteLn('      VideoTool   → prompt convierte a video generado (MP4)');
  WriteLn('      ReportTool  → datos  convierte a PDF generado');
  WriteLn('');
  WriteLn('  Tabla de decision:');
  WriteLn('  ┌─────────────────┬──────────────┬──────────────────────────┐');
  WriteLn('  │ Tool            │ Tipo         │ LLM ve el resultado?     │');
  WriteLn('  ├─────────────────┼──────────────┼──────────────────────────┤');
  WriteLn('  │ VisionTool      │ Comprension  │ Si — razona la descripc. │');
  WriteLn('  │ SpeechTool (STT)│ Comprension  │ Si — razona la transcrip.│');
  WriteLn('  │ PdfTool         │ Comprension  │ Si — razona el contenido │');
  WriteLn('  │ SpeechTool (TTS)│ Generacion   │ No — audio va al usuario │');
  WriteLn('  │ ImageTool       │ Generacion   │ No — imagen va al usuario│');
  WriteLn('  │ VideoTool       │ Generacion   │ No — video va al usuario │');
  WriteLn('  └─────────────────┴──────────────┴──────────────────────────┘');
  WriteLn('');
  WriteLn('→ Pregunta clave: ¿necesitas que el LLM procese el resultado?');
  WriteLn('  Si  → Camino A: aMediaFile.Transcription + Procesado = True');
  WriteLn('  No  → Camino B: ResMsg.AddMediaFile(archivoGenerado)');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
var
  OllamaOK: Boolean;
begin
  WriteLn('=== MakerAI — Capítulo 7: ChatTools ===');
  WriteLn('Gap = SessionCaps - ModelCaps -> VisionTool cubre la brecha');

  OllamaOK := OllamaDisponible;
  WriteLn('');

  if OllamaOK then
    WriteLn('[OK] Ollama detectado en ', OLLAMA_URL)
  else
  begin
    WriteLn('[!!] Ollama no detectado en ', OLLAMA_URL);
    WriteLn('     Demos 1 y 2 requieren Ollama con gemma3:4b.');
    WriteLn('     Para instalar: https://ollama.com');
    WriteLn('     Para el modelo: ollama pull gemma3:4b');
    WriteLn('     Solo se ejecutara el Demo 3 (conceptual, sin servicios externos).');
  end;

  if OllamaOK then
  begin
    Demo1_VisionBridge;
    Demo2_DirectTool;
  end;

  Demo3_CaminoAvsB;

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
