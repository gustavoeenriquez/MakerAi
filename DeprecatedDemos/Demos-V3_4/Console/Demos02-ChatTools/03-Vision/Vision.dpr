program Vision;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 02-ChatTools / 03-Vision
// =============================================================================
// Envía imágenes al LLM para que las describa, analice o extraiga información.
//
// Conceptos que cubre:
//   - Adjuntar imágenes con TAiMediaFile.LoadFromFile()
//   - Pasar mediafiles en AddMessageAndRun()
//   - Activar capacidad de visión (ChatMediaSupports)
//   - Diferencia entre modelos con visión nativa vs. bridge automático
//   - Casos de uso: descripción, OCR, análisis técnico, comparación
//
// Proveedores con visión nativa:
//   Claude, OpenAI (GPT-4V), Gemini → usan la imagen directamente
// Proveedores sin visión nativa:
//   Ollama (gemma3:4b), Groq → MakerAI activa un bridge de OCR automático
//
// Imágenes de prueba: crea archivos de imagen en la misma carpeta o ajusta
// la ruta IMG_PATH a una imagen existente en tu sistema.
// =============================================================================

uses
  System.SysUtils,
  System.IOUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Ollama;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

  // Ajusta estas rutas a imágenes reales en tu sistema
  IMG_SCREENSHOT = 'C:\Windows\Web\Wallpaper\Windows\img0.jpg';  // Wallpaper por defecto
  IMG_CUSTOM     = '';  // Deja vacío si no tienes otra imagen

// =============================================================================
//  Envía una imagen con una pregunta y muestra la respuesta
// =============================================================================
procedure AskAboutImage(Conn: TAiChatConnection;
                        const ImagePath, Question, SectionTitle: String);
var
  MediaFile : TAiMediaFile;
  MediaList : TAiMediaFilesArray;
  Response  : String;
begin
  if not TFile.Exists(ImagePath) then
  begin
    Writeln(Format('  [%s] — Imagen no encontrada: %s', [SectionTitle, ImagePath]));
    Writeln('  Ajusta IMG_SCREENSHOT a una imagen existente en tu sistema.');
    Exit;
  end;

  Writeln(StringOfChar('-', 60));
  Writeln(Format('  %s', [SectionTitle]));
  Writeln(Format('  Imagen  : %s', [TPath.GetFileName(ImagePath)]));
  Writeln(Format('  Pregunta: %s', [Question]));
  Writeln;

  Conn.Messages.Clear;

  MediaFile := TAiMediaFile.Create;
  try
    MediaFile.LoadFromFile(ImagePath);
    SetLength(MediaList, 1);
    MediaList[0] := MediaFile;

    Response := Conn.AddMessageAndRun(Question, 'user', MediaList);
    Writeln('  Respuesta:');
    Writeln('  ', Response);
  finally
    MediaFile.Free;
  end;
  Writeln;
end;

// =============================================================================
//  DEMO principal
// =============================================================================
procedure RunDemo;
var
  Conn    : TAiChatConnection;
  ImgPath : String;
begin
  Writeln('=== Vision ===');
  Writeln('Driver: ', DRIVER, ' / Model: ', MODEL);
  Writeln;

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;

    Conn.Params.Values['ApiKey']            := API_KEY;
    Conn.Params.Values['Asynchronous']      := 'False';
    Conn.Params.Values['Max_Tokens']        := '1024';

    // Activar capacidad de visión + PDF
    // Tfc_Image = imágenes, Tfc_Pdf = documentos PDF
    Conn.Params.Values['NativeInputFiles']  := '[Tfc_Image]';
    Conn.Params.Values['ChatMediaSupports'] := '[Tcm_Image]';

    Conn.SystemPrompt.Text :=
      'Eres un asistente experto en análisis de imágenes. ' +
      'Describe lo que ves de forma detallada y precisa. Responde en español.';

    // Determinar imagen a usar
    ImgPath := IMG_SCREENSHOT;
    if (IMG_CUSTOM <> '') and TFile.Exists(IMG_CUSTOM) then
      ImgPath := IMG_CUSTOM;

    // ── Demo 1: descripción general ──────────────────────────────────────
    AskAboutImage(Conn, ImgPath,
      'Describe esta imagen en detalle: ¿qué ves? colores, objetos, contexto.',
      'Descripción general');

    // ── Demo 2: análisis específico ──────────────────────────────────────
    AskAboutImage(Conn, ImgPath,
      '¿Qué colores predominan en esta imagen? Lista los 3 más importantes.',
      'Análisis de colores');

    // ── Demo 3: pregunta concreta ────────────────────────────────────────
    AskAboutImage(Conn, ImgPath,
      '¿Hay texto visible en la imagen? Si sí, transcríbelo exactamente.',
      'Extracción de texto (OCR)');

    // ── Demo 4: imagen personalizada si se proporcionó ───────────────────
    if (IMG_CUSTOM <> '') and TFile.Exists(IMG_CUSTOM) then
      AskAboutImage(Conn, IMG_CUSTOM,
        'Analiza esta imagen desde el punto de vista técnico o artístico.',
        'Análisis imagen personalizada');

    Writeln('Nota: Prueba cambiando DRIVER a "Gemini" o "OpenAI" para');
    Writeln('comparar cómo describe la misma imagen cada modelo.');

  finally
    Conn.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
