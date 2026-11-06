program MediaFile;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — Capítulo 8: TAiMediaFile — El Sobre Universal de Archivos
// =============================================================================
// Demuestra las capacidades de TAiMediaFile: la abstracción que permite
// enviar imágenes, audio, PDF y cualquier archivo al mismo código
// independientemente del provider.
//
// Demuestra:
//   - Demo 1: Las 4 formas de carga y su metadata automática.
//             LoadFromFile, LoadFromBase64, LoadFromStream, LoadFromUrl.
//   - Demo 2: Categorías y funciones utilitarias globales.
//             GetContentCategory, GetMimeTypeFromFileName,
//             GetFileExtensionFromMimeType, StreamToBase64.
//   - Demo 3: Serialización completa — ToJsonObject / LoadFromJsonObject.
//             Preserva IdFile/CloudName para reutilizar archivos ya subidos.
//   - Demo 4: Prompt adaptativo según el tipo de archivo detectado.
//             El mismo código elige el prompt correcto para cada FileCategory.
//             Incluye una llamada real a Groq con un archivo de texto.
//
// Los Demos 1, 2 y 3 no requieren ninguna API key (solo operaciones locales).
// El Demo 4 usa GROQ_API_KEY para demostrar el envío de un archivo de texto.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.TypInfo,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Groq;   // Para el Demo 4

const
  DRIVER  = 'Groq';
  MODEL   = 'llama-3.3-70b-versatile';
  API_KEY = '@GROQ_API_KEY';

  // Imagen siempre disponible en Windows
  IMG_PATH  = 'C:\Windows\Web\Wallpaper\Windows\img0.jpg';

  // Archivo de texto siempre disponible en Windows (para Demo 1/2)
  TXT_PATH      = 'C:\Windows\win.ini';
  // Archivo .txt creado por Demo 4 (extensión .txt → Tfc_Text)
  DEMO4_TXT     = '.\demo_cap08_config.txt';

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

procedure MostrarMF(const Etiqueta: string; MF: TAiMediaFile);
begin
  WriteLn('  [', Etiqueta, ']');
  WriteLn('    FileName     : ', MF.FileName);
  WriteLn('    FileCategory : ', GetEnumName(TypeInfo(TAiFileCategory),
                                             Ord(MF.FileCategory)));
  WriteLn('    MimeType     : ', MF.MimeType);
  WriteLn('    Bytes        : ', MF.Bytes);
  WriteLn('    Base64 len   : ', Length(MF.Base64), ' chars');
end;

// =============================================================================
//  Demo 1 — Las 4 formas de cargar un TAiMediaFile
//
//  Todas las formas producen el mismo resultado: un TAiMediaFile con
//  FileName, FileCategory, MimeType y Bytes correctamente rellenos.
//  El tipo se detecta automáticamente por la extensión del nombre de archivo.
// =============================================================================
procedure Demo1_FormasDeCarga;
var
  MF     : TAiMediaFile;
  Stream : TMemoryStream;
  Base64 : string;
begin
  Separador('Demo 1 — Las 4 formas de cargar un TAiMediaFile');

  // ── Forma 1: LoadFromFile ─────────────────────────────────────────────────
  WriteLn('  Forma 1: LoadFromFile (la más común)');
  if TFile.Exists(IMG_PATH) then
  begin
    MF := TAiMediaFile.Create;
    try
      MF.LoadFromFile(IMG_PATH);
      MostrarMF('LoadFromFile', MF);
      Base64 := MF.Base64;   // guardar para Forma 3
    finally
      MF.Free;
    end;
  end
  else
    WriteLn('  Imagen no encontrada: ', IMG_PATH);

  // ── Forma 2: LoadFromBase64 ───────────────────────────────────────────────
  WriteLn('');
  WriteLn('  Forma 2: LoadFromBase64 (desde datos recibidos de otra API)');
  if Base64 <> '' then
  begin
    MF := TAiMediaFile.Create;
    try
      MF.LoadFromBase64('imagen_recibida.jpg', Base64);
      MostrarMF('LoadFromBase64', MF);
    finally
      MF.Free;
    end;
  end;

  // ── Forma 3: LoadFromStream ───────────────────────────────────────────────
  WriteLn('');
  WriteLn('  Forma 3: LoadFromStream (desde TMemoryStream en memoria)');
  if TFile.Exists(TXT_PATH) then
  begin
    Stream := TMemoryStream.Create;
    try
      Stream.LoadFromFile(TXT_PATH);
      Stream.Position := 0;

      MF := TAiMediaFile.Create;
      try
        MF.LoadFromStream('configuracion.txt', Stream);
        MostrarMF('LoadFromStream', MF);
      finally
        MF.Free;
      end;
    finally
      Stream.Free;
    end;
  end;

  // ── Forma 4: LoadFromUrl (lazy loading) ──────────────────────────────────
  WriteLn('');
  WriteLn('  Forma 4: LoadFromUrl (la descarga es lazy — ocurre al acceder a Content)');
  WriteLn('  Ejemplo de uso (sin descarga real en este demo):');
  WriteLn('');
  WriteLn('    MF.LoadFromUrl(''https://ejemplo.com/contrato.pdf'');');
  WriteLn('    // FileName = "contrato.pdf", FileCategory = Tfc_Pdf');
  WriteLn('    // Content aún no descargado — descarga lazy al primer acceso');
  WriteLn('    Respuesta := AiConn.AddMessageAndRun(''Resume...'', ''user'', [MF]);');
  WriteLn('    // MakerAI accede a Content al construir el request → descarga ocurre aquí');

  WriteLn('');
  WriteLn('→ Las 4 formas producen el mismo TAiMediaFile.');
  WriteLn('  El tipo se detecta automáticamente por la extensión del nombre.');
end;

// =============================================================================
//  Demo 2 — Funciones utilitarias globales
//
//  MakerAI expone tres funciones de uMakerAi.Core para inspeccionar tipos
//  de archivo sin necesidad de crear un TAiMediaFile.
// =============================================================================
procedure Demo2_Utilitarios;
type
  TExtRec = record Ext, Desc: string; end;
const
  EXTENSIONES: array[0..11] of TExtRec = (
    (Ext: '.jpg';  Desc: 'imagen JPEG'),
    (Ext: '.png';  Desc: 'imagen PNG'),
    (Ext: '.mp3';  Desc: 'audio MP3'),
    (Ext: '.mp4';  Desc: 'video MP4'),
    (Ext: '.pdf';  Desc: 'documento PDF'),
    (Ext: '.docx'; Desc: 'Word'),
    (Ext: '.xlsx'; Desc: 'Excel'),
    (Ext: '.txt';  Desc: 'texto plano'),
    (Ext: '.py';   Desc: 'Python'),
    (Ext: '.zip';  Desc: 'comprimido'),
    (Ext: '.glb';  Desc: '3D/CAD'),
    (Ext: '.epub'; Desc: 'ebook')
  );
  MIMES: array[0..5] of string = (
    'image/jpeg', 'audio/mpeg', 'video/mp4',
    'application/pdf', 'text/plain', 'image/webp'
  );
var
  Rec : TExtRec;
  Mime: string;
  Cat : TAiFileCategory;
begin
  Separador('Demo 2 — Funciones utilitarias globales (uMakerAi.Core)');

  WriteLn('  GetContentCategory(extension) — categoria de archivo:');
  WriteLn('');
  WriteLn('  Extension  Categoria                MimeType');
  WriteLn('  ' + StringOfChar('-', 58));
  for Rec in EXTENSIONES do
  begin
    Cat := GetContentCategory(Rec.Ext);
    WriteLn(Format('  %-10s %-24s %s',
      [Rec.Ext,
       GetEnumName(TypeInfo(TAiFileCategory), Ord(Cat)),
       GetMimeTypeFromFileName(Rec.Ext)]));
  end;

  WriteLn('');
  WriteLn('  GetFileExtensionFromMimeType(mime) — extension desde MIME type:');
  WriteLn('');
  for Mime in MIMES do
    WriteLn(Format('  %-40s -> .%s', [Mime, GetFileExtensionFromMimeType(Mime)]));

  WriteLn('');
  WriteLn('  StreamToBase64(stream) — convierte TMemoryStream a Base64 limpio:');
  if TFile.Exists(TXT_PATH) then
  begin
    var St := TMemoryStream.Create;
    try
      St.LoadFromFile(TXT_PATH);
      St.Position := 0;
      var B64 := StreamToBase64(St);
      WriteLn('  (', TPath.GetFileName(TXT_PATH), ' → Base64 de ', Length(B64), ' chars)');
      WriteLn('  Los primeros 60 chars: ', Copy(B64, 1, 60), '...');
    finally
      St.Free;
    end;
  end;

  WriteLn('');
  WriteLn('→ Estas funciones trabajan solo con extensiones/MIME types.');
  WriteLn('  No necesitan leer el archivo — son resoluciones por tabla.');
end;

// =============================================================================
//  Demo 3 — Serialización: ToJsonObject / LoadFromJsonObject
//
//  TAiMediaFile puede exportar e importar su estado completo como JSON.
//  El caso de uso clave: guardar IdFile/CloudName para reutilizar un archivo
//  ya subido al provider en una sesión futura sin tener que re-subirlo.
// =============================================================================
procedure Demo3_Serializacion;
const
  JSON_ESTADO = '.\estado_mediafile.json';
var
  MF       : TAiMediaFile;
  MF2      : TAiMediaFile;
  JObj     : TJSONObject;
  JStr     : string;
  JCargado : TJSONObject;
begin
  Separador('Demo 3 — Serialización (ToJsonObject / LoadFromJsonObject)');

  if not TFile.Exists(IMG_PATH) then
  begin
    WriteLn('  Imagen no encontrada: ', IMG_PATH);
    Exit;
  end;

  // Simular un archivo que ya fue subido a un provider (IdFile asignado)
  MF := TAiMediaFile.Create;
  try
    MF.LoadFromFile(IMG_PATH);

    // Simular que el provider asignó un IdFile (lo haría automáticamente en uso real)
    MF.IdFile := 'file-abc12345xyz';   // OpenAI
    MF.Detail := 'high';               // Calidad análisis OpenAI

    // Exportar estado completo a JSON
    JObj := MF.ToJsonObject;
    try
      JStr := JObj.Format;
      TFile.WriteAllText(JSON_ESTADO, JStr, TEncoding.UTF8);
      WriteLn('  Estado guardado en: ', JSON_ESTADO);
      WriteLn('  Campos en el JSON: FileName, MimeType, Base64, IdFile, Detail...');

      // Mostrar fragmento del JSON (primeros 300 chars)
      var Fragmento := Copy(JStr, 1, 300);
      WriteLn('');
      WriteLn('  Fragmento del JSON:');
      WriteLn('  ', Fragmento, '...');
    finally
      JObj.Free;
    end;
  finally
    MF.Free;
  end;

  WriteLn('');

  // Restaurar desde JSON en una "nueva sesión"
  WriteLn('  Restaurando desde JSON (simula nueva sesión)...');
  MF2 := TAiMediaFile.Create;
  try
    JCargado := TJSONObject.ParseJSONValue(
      TFile.ReadAllText(JSON_ESTADO, TEncoding.UTF8)) as TJSONObject;
    try
      MF2.LoadFromJsonObject(JCargado);
    finally
      JCargado.Free;
    end;

    WriteLn('  Estado restaurado:');
    WriteLn('    FileName : ', MF2.FileName);
    WriteLn('    MimeType : ', MF2.MimeType);
    WriteLn('    Bytes    : ', MF2.Bytes);
    WriteLn('    IdFile   : ', MF2.IdFile, '  ← el provider no re-sube el archivo');
    WriteLn('    Detail   : ', MF2.Detail);
  finally
    MF2.Free;
  end;

  // Limpiar archivo temporal
  if TFile.Exists(JSON_ESTADO) then
    TFile.Delete(JSON_ESTADO);

  WriteLn('');
  WriteLn('→ Con IdFile/CloudName restaurados, MakerAI usa el archivo ya');
  WriteLn('  subido sin necesidad de re-subirlo. Ahorra tiempo y ancho de banda.');
end;

// =============================================================================
//  Demo 4 — Prompt adaptativo por tipo de archivo + envío a Groq
//
//  El mismo código elige el prompt correcto según MF.FileCategory.
//  En este demo, enviamos un archivo .ini (Tfc_Text) a Groq para demostrar
//  el flujo completo: carga → detección de tipo → envío → respuesta.
// =============================================================================
procedure Demo4_PromptAdaptativo;
const
  TXT_CONTENIDO =
    'Configuracion de la aplicacion MiApp v2.5.3' + #13#10 +
    'modo_oscuro=true' + #13#10 +
    'idioma=es' + #13#10 +
    'max_usuarios=100' + #13#10 +
    'servidor=api.miapp.com' + #13#10 +
    'puerto=8443' + #13#10 +
    'timeout_ms=30000';

  function ElegirPrompt(MF: TAiMediaFile): string;
  begin
    case MF.FileCategory of
      Tfc_Image:
        Result := 'Describe esta imagen con detalle: objetos, colores, contexto.';
      Tfc_Audio:
        Result := 'Transcribe y resume el contenido de este audio.';
      Tfc_Video:
        Result := 'Describe qué ocurre en este video, escena por escena.';
      Tfc_Pdf,
      Tfc_Document:
        Result := 'Resume los puntos clave de este documento en 5 puntos.';
      Tfc_CalcSheet:
        Result := 'Analiza los datos de esta hoja de cálculo y da estadísticas.';
      Tfc_Text,
      Tfc_Web,
      Tfc_ExtractTextFile:
        Result := 'Analiza el contenido de este archivo de texto y explica su propósito.';
    else
      Result := 'Analiza el contenido de este archivo y describe lo que encuentres.';
    end;
  end;

var
  MF    : TAiMediaFile;
  Conn  : TAiChatConnection;
  Files : TAiMediaFilesArray;
  Prompt: string;
  R     : string;
begin
  Separador('Demo 4 — Prompt adaptativo por tipo + envio a Groq');

  // Crear archivo .txt temporal con contenido conocido
  TFile.WriteAllText(DEMO4_TXT, TXT_CONTENIDO, TEncoding.UTF8);

  MF := TAiMediaFile.Create;
  MF.LoadFromFile(DEMO4_TXT);

    WriteLn('  Archivo    : ', MF.FileName);
    WriteLn('  Categoria  : ', GetEnumName(TypeInfo(TAiFileCategory), Ord(MF.FileCategory)));
    WriteLn('  MimeType   : ', MF.MimeType);
    WriteLn('  Bytes      : ', MF.Bytes);

    Prompt := ElegirPrompt(MF);
    WriteLn('');
    WriteLn('  Prompt elegido automaticamente:');
    WriteLn('  "', Prompt, '"');
    WriteLn('');

    // Enviar al LLM
    Conn := TAiChatConnection.Create(nil);
    try
      Conn.DriverName := DRIVER;
      Conn.Model      := MODEL;
      Conn.Params.Values['ApiKey']       := API_KEY;
      Conn.Params.Values['Asynchronous'] := 'False';
      Conn.Params.Values['Max_Tokens']   := '300';

      Conn.SystemPrompt.Text :=
        'Eres un asistente que analiza archivos. Responde en español, conciso.';

      // Para modelos texto-only (Groq, DeepSeek, etc.) el contenido de archivos
    // de texto va inline en el prompt — no como attachment binario.
    // Para modelos multimodales (Gemini, Claude, OpenAI) usarías Files[0] := MF.
    var FileContent := TFile.ReadAllText(DEMO4_TXT, TEncoding.UTF8);
    var PromptFinal := Prompt + #13#10#13#10 +
      '--- Contenido del archivo ---' + #13#10 + FileContent;

    Write('  Enviando a Groq (contenido inline)... ');
    try
      R := Conn.AddMessageAndRun(PromptFinal, 'user', []);
      WriteLn('OK');
      WriteLn('');
      WriteLn('  Respuesta de Groq:');
      WriteLn('  ', R);
    except
      on E: Exception do
      begin
        WriteLn('ERROR');
        WriteLn('  ', E.Message);
      end;
    end;

    MF.Free;  // MF no fue pasado a AddMessageAndRun — liberación normal
  finally
    Conn.Free;
  end;

  // Limpiar archivo temporal
  if TFile.Exists(DEMO4_TXT) then
    TFile.Delete(DEMO4_TXT);

  WriteLn('');
  WriteLn('→ El mismo código funciona con cualquier tipo de archivo.');
  WriteLn('  Cambia TXT_PATH por una imagen, PDF o audio (con el provider correcto).');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
begin
  WriteLn('=== MakerAI — Capítulo 8: TAiMediaFile ===');
  WriteLn('El sobre universal: la misma clase para imagen, audio, PDF, texto...');

  Demo1_FormasDeCarga;
  Demo2_Utilitarios;
  Demo3_Serializacion;
  Demo4_PromptAdaptativo;

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
