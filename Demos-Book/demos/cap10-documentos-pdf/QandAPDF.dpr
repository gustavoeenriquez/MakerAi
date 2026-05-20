program QandAPDF;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — Capítulo 10: Procesamiento de Documentos PDF
// =============================================================================
// Demuestra dos estrategias complementarias para trabajar con documentos:
//
//   Estrategia A — PDF nativo con Gemini (requiere GEMINI_API_KEY):
//     gemini-2.5-flash recibe el PDF directamente (ModelCaps incluye cap_Pdf).
//     El modelo procesa texto, tablas e imágenes del PDF íntegramente.
//
//   Estrategia B — Texto embebido con Ollama (100% local, sin API key):
//     Ollama no tiene soporte nativo de PDF → leemos el texto y lo enviamos
//     como contexto en el SystemPrompt. Privacidad total: el documento
//     nunca sale de tu máquina.
//
// Demos:
//   1. PDF nativo con Gemini — resumen ejecutivo del documento
//   2. Multi-turno sobre PDF — el modelo recuerda el documento en el historial
//   3. Extracción JSON desde PDF — datos estructurados para procesar en Delphi
//   4. Ollama + contexto de texto — Q&A local sin soporte PDF nativo
//
// Requisitos:
//   Demos 1-3: GEMINI_API_KEY en variables de entorno
//              Un archivo .pdf accesible (ajusta PDF_PATH o el path en ejecución)
//   Demo 4   : Ollama en localhost:11434 con gemma3:4b
//              Instalar: https://ollama.com  |  ollama pull gemma3:4b
// =============================================================================

uses
  System.SysUtils,
  System.IOUtils,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Gemini,    // DriverName = 'Gemini' — se auto-registra al importar
  uMakerAi.Chat.Ollama;    // DriverName = 'Ollama' — se auto-registra al importar

const
  GEMINI_DRIVER = 'Gemini';
  GEMINI_MODEL  = 'gemini-3-flash-preview';

  OLLAMA_DRIVER = 'Ollama';
  OLLAMA_MODEL  = 'gemma3:4b';
  OLLAMA_URL    = 'http://localhost:11434/';

  // Ruta al PDF de prueba para Demos 1-3.
  // Si el archivo no existe, esos demos se omiten con instrucciones.
  // Puedes usar cualquier PDF: un contrato, factura, manual, informe...
  PDF_PATH = 'C:\Users\Public\Documents\sample.pdf';

// =============================================================================
procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

// =============================================================================
//  Verifica que Ollama esté corriendo en localhost.
// =============================================================================
function OllamaActivo: Boolean;
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
//  Verifica que la variable de entorno GEMINI_API_KEY esté configurada.
// =============================================================================
function GeminiApiKeyDisponible: Boolean;
begin
  Result := GetEnvironmentVariable('GEMINI_API_KEY') <> '';
end;

// =============================================================================
//  Busca un PDF en PDF_PATH y en el directorio actual.
//  Devuelve la ruta si lo encuentra, cadena vacía si no.
// =============================================================================
function EncontrarPDF: string;
var
  RutaLocal: string;
begin
  if TFile.Exists(PDF_PATH) then
    Exit(PDF_PATH);

  // Buscar también en el directorio del ejecutable
  RutaLocal := ExtractFilePath(ParamStr(0)) + 'sample.pdf';
  if TFile.Exists(RutaLocal) then
    Exit(RutaLocal);

  // Buscar cualquier PDF en el directorio actual
  var Archivos := TDirectory.GetFiles(
    ExtractFilePath(ParamStr(0)), '*.pdf', TSearchOption.soTopDirectoryOnly);
  if Length(Archivos) > 0 then
    Exit(Archivos[0]);

  Result := '';
end;

// =============================================================================
//  Crea la conexión con Gemini 2.5 Flash para procesamiento de PDF.
//  gemini-2.5-flash tiene ModelCaps que incluye cap_Pdf — soporte nativo.
//  El llamador es responsable de liberar con Free.
// =============================================================================
function CrearConexionGemini: TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := GEMINI_DRIVER;
  Result.Model      := GEMINI_MODEL;
  Result.Params.Values['ApiKey']       := '@GEMINI_API_KEY';
  Result.Params.Values['Asynchronous'] := 'False';
  Result.Params.Values['Max_Tokens']   := '1024';
  Result.SystemPrompt.Text :=
    'Eres un asistente especializado en análisis de documentos. ' +
    'Basa tus respuestas ÚNICAMENTE en el contenido del documento adjunto. ' +
    'Si la información no está en el documento, dilo claramente. ' +
    'Responde en español de forma concisa y precisa.';
end;

// =============================================================================
//  Crea la conexión con Ollama (gemma3:4b) para Q&A local.
//  El llamador es responsable de liberar con Free.
// =============================================================================
function CrearConexionOllama: TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := OLLAMA_DRIVER;
  Result.Model      := OLLAMA_MODEL;
  Result.Params.Values['Asynchronous'] := 'False';  // Ollama default es True
  Result.Params.Values['Max_Tokens']   := '512';
  Result.SystemPrompt.Text :=
    'Eres un asistente útil. Responde en español de forma concisa y precisa.';
end;

// =============================================================================
//  Demo 1 — PDF nativo con Gemini
//
//  El patrón base de PDF con MakerAI es idéntico al de imágenes:
//    1. Crear TAiMediaFile y cargar el PDF con LoadFromFile
//    2. MF.FileCategory queda en fcDocument automáticamente
//    3. Pasarlo en AddMessageAndRun — Gemini lo procesa nativo
//
//  MakerAI detecta que Gemini tiene cap_Pdf y envía el archivo directamente.
//  No se necesita configuración adicional.
// =============================================================================
procedure Demo1_PdfNativoGemini(const PdfPath: string; Conn: TAiChatConnection);
var
  MF   : TAiMediaFile;
  Files: TAiMediaFilesArray;
  R    : string;
begin
  Separador('Demo 1 — PDF nativo con Gemini (Estrategia A)');

  MF := TAiMediaFile.Create;
  MF.LoadFromFile(PdfPath);
  WriteLn('  Archivo: ', MF.FileName);
  WriteLn('  Tipo   : ', MF.MimeType, '  (', MF.Bytes div 1024, ' KB)');
  WriteLn('');

  SetLength(Files, 1);
  Files[0] := MF;

  Conn.NewChat;
  Write('  Enviando PDF a Gemini... ');
  R := Conn.AddMessageAndRun(
    'Lee este documento y dame un resumen ejecutivo con: ' +
    '(1) tema principal, (2) puntos clave, (3) conclusión o acción requerida. ' +
    'Máximo 5 líneas en total.',
    'user', Files);
  WriteLn('OK');
  WriteLn('');
  WriteLn('  Resumen:');
  WriteLn('  ', R);

  WriteLn('');
  WriteLn('→ Patrón base: MF.LoadFromFile(pdf) + AddMessageAndRun([MF]).');
  WriteLn('  Gemini 2.5 Flash procesa el PDF íntegro (ModelCaps=[cap_Pdf]).');
  WriteLn('  Claude funciona igual — cambia DriverName a "Claude".');
end;

// =============================================================================
//  Demo 2 — Multi-turno sobre un PDF
//
//  Una vez adjuntado en el primer mensaje, el modelo "recuerda" el documento
//  en los turnos siguientes sin necesidad de reenviarlo.
//  Esto permite una sesión de Q&A natural y eficiente.
// =============================================================================
procedure Demo2_MultiturnoGemini(const PdfPath: string; Conn: TAiChatConnection);
var
  MF   : TAiMediaFile;
  Files: TAiMediaFilesArray;
  R    : string;
begin
  Separador('Demo 2 — Q&A multi-turno sobre PDF (el modelo recuerda)');

  MF := TAiMediaFile.Create;
  MF.LoadFromFile(PdfPath);
  SetLength(Files, 1);
  Files[0] := MF;

  Conn.NewChat;

  // Turno 1: enviamos el PDF con la primera pregunta
  Write('  [Turno 1] Enviando PDF... ');
  R := Conn.AddMessageAndRun(
    'He adjuntado un documento. Confirma que lo recibiste y di de qué se trata en una oración.',
    'user', Files);
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);
  WriteLn('  [Historial: ', Conn.Messages.Count, ' mensajes]');

  // Turno 2: segunda pregunta SIN reenviar el PDF
  WriteLn('');
  Write('  [Turno 2] Pregunta sin reenviar PDF... ');
  R := Conn.AddMessageAndRun(
    '¿Cuáles son los tres puntos más importantes del documento?',
    'user', []);   // ← sin PDF en el array
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);
  WriteLn('  [Historial: ', Conn.Messages.Count, ' mensajes]');

  // Turno 3: pregunta de detalle
  WriteLn('');
  Write('  [Turno 3] Pregunta de detalle... ');
  R := Conn.AddMessageAndRun(
    '¿Hay fechas, montos o cifras importantes? Menciona las más relevantes.',
    'user', []);
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);
  WriteLn('  [Historial: ', Conn.Messages.Count, ' mensajes]');

  WriteLn('');
  WriteLn('→ El PDF se adjunta UNA sola vez (Turno 1).');
  WriteLn('  Turnos 2 y 3 pasan [] — el modelo lo recuerda en el historial.');
  WriteLn('  Conn.Messages.Count crece con cada turno: system + user + assistant.');
end;

// =============================================================================
//  Demo 3 — Extracción JSON estructurada desde PDF
//
//  Forzar respuesta JSON permite integración directa con la aplicación:
//  parsear con TJSONObject, guardar en base de datos, etc.
//  Útil para facturas, contratos, formularios, informes.
// =============================================================================
procedure Demo3_ExtraccionJSON(const PdfPath: string; Conn: TAiChatConnection);
var
  MF    : TAiMediaFile;
  Files : TAiMediaFilesArray;
  R     : string;
  OldSP : string;
begin
  Separador('Demo 3 — Extracción estructurada (JSON) desde PDF');

  MF := TAiMediaFile.Create;
  MF.LoadFromFile(PdfPath);
  SetLength(Files, 1);
  Files[0] := MF;

  Conn.NewChat;

  // Guardar y reemplazar el SystemPrompt para forzar JSON
  OldSP := Conn.SystemPrompt.Text;
  Conn.SystemPrompt.Text :=
    'Eres un extractor de datos de documentos. ' +
    'Responde ÚNICAMENTE con JSON válido. Sin texto adicional fuera del JSON. ' +
    'Si un campo no aparece en el documento, usa null.';

  Write('  Extrayendo datos estructurados... ');
  R := Conn.AddMessageAndRun(
    'Analiza este documento y devuelve un JSON con este esquema exacto: ' +
    '{"tipo_documento": "...", ' +
    '"fecha": "YYYY-MM-DD o null", ' +
    '"partes": ["nombre1", "nombre2"], ' +
    '"monto_total": 0.0, ' +
    '"moneda": "MXN/USD/EUR o null", ' +
    '"resumen": "una oración"}',
    'user', Files);
  WriteLn('OK');
  WriteLn('');
  WriteLn('  JSON extraído:');
  WriteLn('  ', R);

  // Restaurar SystemPrompt para demos siguientes
  Conn.SystemPrompt.Text := OldSP;

  WriteLn('');
  WriteLn('→ SystemPrompt + esquema JSON = datos parseables.');
  WriteLn('  En Delphi: TJSONObject.ParseJSONValue(R) para procesar el resultado.');
  WriteLn('  Haiku (Claude) o Flash (Gemini) son las opciones más económicas para lotes.');
end;

// =============================================================================
//  Demo 4 — Ollama + texto embebido (Estrategia B, 100% local)
//
//  Ollama no tiene soporte nativo de PDF (sin cap_Pdf).
//  Alternativa práctica: extraer el texto del PDF con Delphi y enviarlo
//  como contexto en el SystemPrompt. El modelo hace Q&A sobre ese texto.
//
//  Para este demo usamos un documento de muestra interno (factura ficticia).
//  En producción: usar iTextSharp, PdfiumLib, o TAiNativePdfTool
//  (disponible en el repo AiMaker.ChatTools) para la extracción real del texto.
// =============================================================================
procedure Demo4_OllamaTextoLocal(Conn: TAiChatConnection);
var
  Texto: string;
  R    : string;
begin
  Separador('Demo 4 — Ollama + Texto Embebido (Estrategia B, 100% local)');

  WriteLn('  Ollama no procesa PDF nativamente — pero podemos incrustar');
  WriteLn('  el texto extraído directamente en el SystemPrompt.');
  WriteLn('  Resultado: Q&A sobre el documento sin que salga de tu máquina.');
  WriteLn('');

  // Documento de muestra: factura ficticia
  // En producción este texto vendría de un lector de PDF:
  //   - TAiNativePdfTool (AiMaker.ChatTools): extractor 100% Delphi + Skia
  //   - PdfiumLib (componente Delphi open source)
  //   - System.PDF.PDFium (Delphi 12+ en beta)
  Texto :=
    'FACTURA ELECTRÓNICA No. FE-2025-0142' + sLineBreak +
    'Fecha de emisión: 10 de marzo de 2025' + sLineBreak +
    sLineBreak +
    'PROVEEDOR: Distribuidora Norte S.A. de C.V.' + sLineBreak +
    'RFC: DNO820315AB8' + sLineBreak +
    'Dirección: Calle Reforma 450, Col. Centro, CDMX 06040' + sLineBreak +
    sLineBreak +
    'CLIENTE: Tech Solutions Monterrey S.A.' + sLineBreak +
    'RFC: TSM970612CD4' + sLineBreak +
    'Dirección: Av. Constitución 1200, Monterrey, N.L. 64000' + sLineBreak +
    sLineBreak +
    'CONCEPTOS:' + sLineBreak +
    '  1. Laptop Dell XPS 15 (cantidad: 3)        $ 45,000.00' + sLineBreak +
    '  2. Monitor LG 27 pulgadas 4K (cantidad: 5) $ 18,750.00' + sLineBreak +
    '  3. Switch Cisco 24 puertos (cantidad: 1)   $  8,200.00' + sLineBreak +
    '  4. Instalación y configuración             $  3,500.00' + sLineBreak +
    sLineBreak +
    'SUBTOTAL:                                    $ 75,450.00' + sLineBreak +
    'IVA (16%):                                   $ 12,072.00' + sLineBreak +
    'TOTAL A PAGAR:                               $ 87,522.00' + sLineBreak +
    sLineBreak +
    'Forma de pago: Transferencia bancaria 30 días' + sLineBreak +
    'Banco: BBVA  CLABE: 012345678901234567' + sLineBreak +
    sLineBreak +
    'Notas: Incluye garantía de 12 meses en equipos.' + sLineBreak +
    'Fecha de instalación acordada: 15 de marzo de 2025 a las 9:00 hrs.' + sLineBreak +
    'Contacto proveedor: ventas@distribnorte.com.mx  Tel: 55-1234-5678';

  WriteLn('  Documento: Factura FE-2025-0142 (muestra interna)');
  WriteLn('  Contenido: ', Length(Texto), ' caracteres');
  WriteLn('');

  // El texto del documento va en el SystemPrompt
  // El modelo recibe el contenido completo como parte de su contexto inicial
  Conn.NewChat;
  Conn.SystemPrompt.Text :=
    'Eres un asistente especializado en análisis de documentos.' + sLineBreak +
    'Basa tus respuestas ÚNICAMENTE en el siguiente contenido.' + sLineBreak +
    'Si la información no está en el documento, di: "No se menciona en el documento".' + sLineBreak +
    'Responde en español de forma concisa.' + sLineBreak +
    sLineBreak +
    '=== DOCUMENTO ===' + sLineBreak +
    Texto + sLineBreak +
    '=== FIN DEL DOCUMENTO ===';

  // Turno 1: identificación del documento
  Write('  [Turno 1] Identificando documento... ');
  R := Conn.AddMessageAndRun(
    '¿De qué tipo de documento se trata y cuáles son los datos principales?',
    'user', []);
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);

  // Turno 2: cifras financieras
  WriteLn('');
  Write('  [Turno 2] Datos financieros... ');
  R := Conn.AddMessageAndRun(
    '¿Cuál es el total a pagar y de cuánto es el IVA?',
    'user', []);
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);

  // Turno 3: desglose de productos
  WriteLn('');
  Write('  [Turno 3] Productos adquiridos... ');
  R := Conn.AddMessageAndRun(
    'Lista todos los productos con sus cantidades y precios unitarios.',
    'user', []);
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);

  WriteLn('');
  WriteLn('→ Patrón Estrategia B: texto del documento en SystemPrompt.');
  WriteLn('  Sin cap_Pdf nativo — pero efectivo y 100% local con Ollama.');
  WriteLn('  Para automatizar la extracción de texto desde PDF real:');
  WriteLn('    TAiNativePdfTool (AiMaker.ChatTools)  — extractor puro Delphi');
  WriteLn('    PdfiumLib  — binding Delphi a PDFium (código abierto)');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
var
  ConnGemini : TAiChatConnection;
  ConnOllama : TAiChatConnection;
  PdfPath    : string;
  TieneGemini: Boolean;
  TienePDF   : Boolean;
  TieneOllama: Boolean;
begin
  WriteLn('=== MakerAI — Capítulo 10: Procesamiento de Documentos PDF ===');
  WriteLn('Estrategia A: PDF nativo con Gemini (ModelCaps=[cap_Pdf])');
  WriteLn('Estrategia B: Texto embebido con Ollama (100% local)');

  // Verificar disponibilidad de cada recurso
  TieneGemini := GeminiApiKeyDisponible;
  PdfPath     := EncontrarPDF;
  TienePDF    := PdfPath <> '';
  TieneOllama := OllamaActivo;

  WriteLn('');
  WriteLn('  Estado:');
  if TieneGemini then
    WriteLn('  [OK] GEMINI_API_KEY detectado')
  else
    WriteLn('  [--] GEMINI_API_KEY no configurado → Demos 1-3 se omitirán');

  if TienePDF then
    WriteLn('  [OK] PDF encontrado: ', ExtractFileName(PdfPath))
  else
    WriteLn('  [--] PDF no encontrado (PDF_PATH=', PDF_PATH, ') → Demos 1-3 se omitirán');

  if TieneOllama then
    WriteLn('  [OK] Ollama activo en ', OLLAMA_URL, ' — Demo 4 disponible')
  else
    WriteLn('  [--] Ollama no detectado en ', OLLAMA_URL, ' → Demo 4 se omitirá');

  // ── Demos con Gemini (Estrategia A) ─────────────────────────────────────
  if TieneGemini and TienePDF then
  begin
    ConnGemini := CrearConexionGemini;
    try
      Demo1_PdfNativoGemini(PdfPath, ConnGemini);
      Demo2_MultiturnoGemini(PdfPath, ConnGemini);
      Demo3_ExtraccionJSON(PdfPath, ConnGemini);
    finally
      ConnGemini.Free;
    end;
  end
  else if not TieneGemini or not TienePDF then
  begin
    WriteLn('');
    WriteLn('--- Demos 1-3 omitidos (Gemini / PDF no disponibles) ---');
    if not TieneGemini then
      WriteLn('  Para habilitar: configura la variable de entorno GEMINI_API_KEY');
    if not TienePDF then
      WriteLn('  Para habilitar: coloca un .pdf en ', PDF_PATH);
  end;

  // ── Demo con Ollama (Estrategia B) ──────────────────────────────────────
  if TieneOllama then
  begin
    ConnOllama := CrearConexionOllama;
    try
      Demo4_OllamaTextoLocal(ConnOllama);
    finally
      ConnOllama.Free;
    end;
  end
  else
  begin
    WriteLn('');
    WriteLn('--- Demo 4 omitido (Ollama no disponible) ---');
    WriteLn('  Para habilitar: ollama serve  |  ollama pull gemma3:4b');
  end;

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
