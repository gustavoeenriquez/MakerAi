program PDFChat;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 02-ChatTools / 04-PDF
// =============================================================================
// Envía documentos PDF al LLM para que los lea, resuma y responda preguntas
// sobre su contenido.
//
// Conceptos que cubre:
//   - Adjuntar PDFs igual que imágenes (TAiMediaFile.LoadFromFile)
//   - Activar Tfc_Pdf en NativeInputFiles y Tcm_Pdf en ChatMediaSupports
//   - Extracción de información estructurada de documentos
//   - Q&A sobre el contenido del PDF
//   - Resumen automático
//   - Proveedores con soporte nativo: Claude, Gemini
//
// Nota: Si no tienes un PDF disponible, el demo crea uno de prueba.
// =============================================================================

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Gemini;

const
  DRIVER  = 'Claude';
  MODEL   = 'claude-haiku-4-5-20251001';
  API_KEY = '@CLAUDE_API_KEY';

  // Ruta a tu PDF de prueba — ajusta según tu sistema
  // Si el archivo no existe, el demo informa cómo obtener uno
  PDF_PATH = 'C:\Users\Public\Documents\sample.pdf';

  PDF_FALLBACK_URL = 'https://www.w3.org/WAI/WCAG21/wcag21.pdf';

// =============================================================================
//  Envía un PDF con una pregunta
// =============================================================================
function AskAboutPDF(Conn: TAiChatConnection;
                     const PdfPath, Question: String): String;
var
  MediaFile : TAiMediaFile;
  MediaList : TAiMediaFilesArray;
begin
  Conn.Messages.Clear;

  MediaFile := TAiMediaFile.Create;
  try
    MediaFile.LoadFromFile(PdfPath);
    SetLength(MediaList, 1);
    MediaList[0] := MediaFile;

    Result := Conn.AddMessageAndRun(Question, 'user', MediaList);
  finally
    MediaFile.Free;
  end;
end;

// =============================================================================
//  DEMO principal
// =============================================================================
procedure RunDemo;
var
  Conn    : TAiChatConnection;
  PdfPath : String;
  Response: String;
begin
  Writeln('=== PDFChat ===');
  Writeln('Driver: ', DRIVER, ' / Model: ', MODEL);
  Writeln;

  // Verificar que existe el PDF
  PdfPath := PDF_PATH;

  // Buscar un PDF del sistema si no existe el configurado
  if not TFile.Exists(PdfPath) then
  begin
    Writeln('PDF de prueba no encontrado en: ', PdfPath);
    Writeln;
    Writeln('Para ejecutar este demo necesitas un archivo PDF.');
    Writeln('Opciones:');
    Writeln('  1. Edita PDF_PATH en el código con la ruta a tu PDF');
    Writeln('  2. Descarga uno de prueba: ', PDF_FALLBACK_URL);
    Writeln('  3. Usa cualquier PDF que tengas en tu sistema');
    Writeln;

    // Buscar cualquier PDF en Documents
    var Files := TDirectory.GetFiles(
      TPath.GetDocumentsPath, '*.pdf', TSearchOption.soAllDirectories);

    if Length(Files) > 0 then
    begin
      PdfPath := Files[0];
      Writeln(Format('PDF encontrado automáticamente: %s', [PdfPath]));
      Writeln;
    end
    else
    begin
      Writeln('No se encontró ningún PDF. Demo cancelado.');
      Exit;
    end;
  end;

  Writeln(Format('PDF a analizar: %s', [TPath.GetFileName(PdfPath)]));
  Writeln(Format('Tamaño       : %.1f KB', [TFile.GetSize(PdfPath) / 1024]));
  Writeln;

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;

    Conn.Params.Values['ApiKey']            := API_KEY;
    Conn.Params.Values['Asynchronous']      := 'False';
    Conn.Params.Values['Max_Tokens']        := '2048';
    Conn.Params.Values['ResponseTimeOut']   := '120000';

    // Activar soporte de PDF
    Conn.Params.Values['NativeInputFiles']  := '[Tfc_Pdf]';
    Conn.Params.Values['ChatMediaSupports'] := '[Tcm_Pdf]';

    Conn.SystemPrompt.Text :=
      'Eres un asistente especializado en análisis de documentos. ' +
      'Lee el documento adjunto con atención y responde con precisión. ' +
      'Responde en español.';

    // ── Demo 1: Resumen ejecutivo ─────────────────────────────────────────
    Writeln(StringOfChar('-', 60));
    Writeln('1. Resumen ejecutivo del documento');
    Response := AskAboutPDF(Conn, PdfPath,
      'Por favor proporciona un resumen ejecutivo de este documento en ' +
      'máximo 5 puntos clave.');
    Writeln(Response);
    Writeln;

    // ── Demo 2: Preguntas específicas ─────────────────────────────────────
    Writeln(StringOfChar('-', 60));
    Writeln('2. Preguntas específicas sobre el contenido');

    Response := AskAboutPDF(Conn, PdfPath,
      '¿Cuáles son los temas principales que trata este documento? ' +
      'Lista solo los títulos o secciones que encuentres.');
    Writeln(Response);
    Writeln;

    // ── Demo 3: Extracción de datos ───────────────────────────────────────
    Writeln(StringOfChar('-', 60));
    Writeln('3. Extracción de datos estructurados');

    Response := AskAboutPDF(Conn, PdfPath,
      'Extrae en formato JSON los siguientes datos del documento: ' +
      '{"titulo": "", "autor": "", "fecha": "", "paginas_aproximadas": 0, ' +
      '"idioma": ""}. Si no encuentras algún dato, pon null.');
    Writeln(Response);

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
