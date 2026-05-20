program Vision;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — Capítulo 9: Visión Artificial con Ollama (100% local)
// =============================================================================
// Demuestra el análisis de imágenes usando modelos multimodales en Ollama.
// La imagen nunca sale de tu máquina — privacidad total, sin API key.
//
// Modelo: gemma3:4b (visión nativa, ModelCaps = [cap_Image])
//   → ollama pull gemma3:4b
//   Alternativas: 'llava:latest', 'llama3.2-vision:latest', 'moondream:latest'
//
// Demuestra:
//   - Demo 1: Descripción general de una imagen — el patrón base de visión.
//   - Demo 2: OCR — extracción de texto visible en la imagen.
//   - Demo 3: Extracción estructurada — pedir JSON con elementos detectados.
//   - Demo 4: Conversación multi-turno con imagen — el modelo recuerda la
//             imagen en turnos siguientes sin re-enviarla.
//   - Demo 5: Comparación de dos imágenes — múltiples archivos en un mensaje.
//
// Prerequisito:
//   Ollama corriendo en localhost:11434 con modelo gemma3:4b.
//   Instalar: https://ollama.com
//   Modelo:   ollama pull gemma3:4b
// =============================================================================

uses
  System.SysUtils,
  System.IOUtils,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Ollama;   // DriverName = 'Ollama'

const
  DRIVER = 'Ollama';
  MODEL  = 'gemma3:4b';   // Vision + texto. Alternativas: llava:latest

  OLLAMA_URL = 'http://localhost:11434/';

  // Wallpaper de Windows — presente en todas las instalaciones
  IMG1 = 'C:\Windows\Web\Wallpaper\Windows\img0.jpg';

  // Segunda imagen para el Demo 5 (comparación)
  // Si no existe, el demo busca automáticamente otra imagen en el sistema
  IMG2 = 'C:\Windows\Web\Wallpaper\Windows\img0.jpg';  // reemplazar si tienes otra

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

// =============================================================================
//  Verifica que Ollama esté corriendo.
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
//  Crea y configura la conexión Ollama con gemma3:4b.
//  El llamador es responsable de liberar con Free.
// =============================================================================
function CrearConexion: TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := DRIVER;
  Result.Model      := MODEL;
  Result.Params.Values['Asynchronous'] := 'False';  // Ollama default es True
  Result.Params.Values['Max_Tokens']   := '512';
  Result.SystemPrompt.Text :=
    'Eres un asistente experto en análisis visual. ' +
    'Describe lo que ves de forma clara y concisa. Responde en español.';
end;

// =============================================================================
//  Demo 1 — Descripción general
//
//  El patrón base de visión con MakerAI:
//    1. Crear TAiMediaFile y cargar la imagen
//    2. Pasarlo como tercer parámetro de AddMessageAndRun
//    3. El modelo lo recibe y genera una descripción
//
//  gemma3:4b tiene ModelCaps=[cap_Image] → visión nativa, sin bridge.
// =============================================================================
procedure Demo1_Descripcion(Conn: TAiChatConnection);
var
  MF   : TAiMediaFile;
  Files: TAiMediaFilesArray;
  R    : string;
begin
  Separador('Demo 1 — Descripcion general de la imagen');

  MF := TAiMediaFile.Create;
  MF.LoadFromFile(IMG1);
  WriteLn('  Imagen: ', MF.FileName);
  WriteLn('  Tipo  : ', MF.MimeType, '  (', MF.Bytes div 1024, ' KB)');
  WriteLn('');

  SetLength(Files, 1);
  Files[0] := MF;

  Conn.NewChat;
  Write('  Describiendo imagen... ');
  R := Conn.AddMessageAndRun(
    'Describe esta imagen en detalle: ¿qué ves? objetos, colores, ambiente.',
    'user', Files);
  WriteLn('OK');
  WriteLn('');
  WriteLn('  Descripcion:');
  WriteLn('  ', R);

  WriteLn('');
  WriteLn('→ Patron base: MF.LoadFromFile + AddMessageAndRun([MF]).');
  WriteLn('  gemma3:4b procesa la imagen de forma nativa (ModelCaps=[cap_Image]).');
end;

// =============================================================================
//  Demo 2 — OCR: extracción de texto visible
//
//  Los modelos de visión pueden leer texto en imágenes como si fuera OCR.
//  Para documentos escaneados, capturas de pantalla con texto, señales, etc.
// =============================================================================
procedure Demo2_OCR(Conn: TAiChatConnection);
var
  MF   : TAiMediaFile;
  Files: TAiMediaFilesArray;
  R    : string;
begin
  Separador('Demo 2 — OCR: extraccion de texto visible');

  MF := TAiMediaFile.Create;
  MF.LoadFromFile(IMG1);

  SetLength(Files, 1);
  Files[0] := MF;

  Conn.NewChat;
  Write('  Extrayendo texto visible... ');
  R := Conn.AddMessageAndRun(
    '¿Hay texto visible en esta imagen? ' +
    'Si sí, transcríbelo exactamente. ' +
    'Si no hay texto, responde: "Sin texto visible."',
    'user', Files);
  WriteLn('OK');
  WriteLn('');
  WriteLn('  Texto encontrado:');
  WriteLn('  ', R);

  WriteLn('');
  WriteLn('→ Los modelos multimodales hacen OCR inteligente: entienden contexto,');
  WriteLn('  posicion y formato del texto, no solo los caracteres.');
end;

// =============================================================================
//  Demo 3 — Extracción estructurada (JSON)
//
//  Pedir al modelo que devuelva los datos en formato JSON estructurado.
//  Útil para integración directa con aplicaciones: parsear con TJSONObject.
// =============================================================================
procedure Demo3_JSON(Conn: TAiChatConnection);
var
  MF   : TAiMediaFile;
  Files: TAiMediaFilesArray;
  R    : string;
begin
  Separador('Demo 3 — Extraccion estructurada (respuesta JSON)');

  MF := TAiMediaFile.Create;
  MF.LoadFromFile(IMG1);

  SetLength(Files, 1);
  Files[0] := MF;

  Conn.NewChat;
  Conn.SystemPrompt.Text :=
    'Eres un sistema de análisis visual. ' +
    'Responde ÚNICAMENTE con JSON válido. Sin texto adicional fuera del JSON.';

  Write('  Extrayendo datos estructurados... ');
  R := Conn.AddMessageAndRun(
    'Analiza esta imagen y devuelve un JSON con este formato exacto: ' +
    '{"escena": "descripcion breve", ' +
    '"colores_principales": ["color1", "color2", "color3"], ' +
    '"objetos": ["obj1", "obj2"], ' +
    '"hay_texto": false, ' +
    '"ambiente": "interior|exterior|abstracto"}',
    'user', Files);
  WriteLn('OK');
  WriteLn('');
  WriteLn('  JSON devuelto:');
  WriteLn('  ', R);

  // Restaurar system prompt para los demos siguientes
  Conn.SystemPrompt.Text :=
    'Eres un asistente experto en análisis visual. ' +
    'Describe lo que ves de forma clara y concisa. Responde en español.';

  WriteLn('');
  WriteLn('→ JSON en SystemPrompt + ejemplo de formato = respuesta parseable.');
  WriteLn('  Combina con TJSONObject.ParseJSONValue() para procesar en Delphi.');
end;

// =============================================================================
//  Demo 4 — Conversación multi-turno con imagen
//
//  Una vez enviada la imagen en el primer turno, el modelo "recuerda" su
//  contenido en los turnos siguientes. No hace falta reenviar la imagen.
//  Esto permite una conversación natural sobre la misma imagen.
// =============================================================================
procedure Demo4_MultiTurno(Conn: TAiChatConnection);
var
  MF   : TAiMediaFile;
  Files: TAiMediaFilesArray;
  R    : string;
begin
  Separador('Demo 4 — Conversacion multi-turno sobre la imagen');

  MF := TAiMediaFile.Create;
  MF.LoadFromFile(IMG1);

  SetLength(Files, 1);
  Files[0] := MF;

  Conn.NewChat;

  // Turno 1: enviar la imagen con la primera pregunta
  Write('  [Turno 1] Enviando imagen... ');
  R := Conn.AddMessageAndRun(
    'Mira esta imagen. ¿Cuál es el elemento más prominente que ves?',
    'user', Files);
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);
  WriteLn('  [Historial: ', Conn.Messages.Count, ' mensajes]');

  // Turno 2: segunda pregunta SIN reenviar la imagen
  WriteLn('');
  Write('  [Turno 2] Pregunta de seguimiento (sin reenviar imagen)... ');
  R := Conn.AddMessageAndRun(
    '¿Qué colores predominan en esa área?',
    'user', []);  // ← sin imagen en el array
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);
  WriteLn('  [Historial: ', Conn.Messages.Count, ' mensajes]');

  // Turno 3: otra pregunta de contexto
  WriteLn('');
  Write('  [Turno 3] Otra pregunta de contexto... ');
  R := Conn.AddMessageAndRun(
    '¿Qué estado de ánimo o sensación transmite esta imagen?',
    'user', []);
  WriteLn('OK');
  WriteLn('  Respuesta: ', R);
  WriteLn('  [Historial: ', Conn.Messages.Count, ' mensajes]');

  WriteLn('');
  WriteLn('→ La imagen se envía UNA sola vez (Turno 1).');
  WriteLn('  Los Turnos 2 y 3 usan el historial para mantener el contexto.');
  WriteLn('  El orquestador no re-envía la imagen — ahorra tokens y tiempo.');
end;

// =============================================================================
//  Demo 5 — Dos imágenes en un mismo mensaje (comparación)
//
//  MakerAI permite enviar múltiples TAiMediaFile en una sola llamada.
//  El modelo recibe ambas imágenes y puede compararlas, contrastarlas
//  o analizarlas de forma conjunta.
// =============================================================================
procedure Demo5_DosImagenes(Conn: TAiChatConnection);
var
  MF1, MF2  : TAiMediaFile;
  Files     : TAiMediaFilesArray;
  R         : string;
  Img2Path  : string;
begin
  Separador('Demo 5 — Comparacion de dos imagenes');

  // Buscar una segunda imagen en el sistema si IMG2 es igual a IMG1
  Img2Path := IMG2;
  if SameText(Img2Path, IMG1) then
  begin
    // Buscar en carpetas de wallpapers de Windows
    var Candidatos := TDirectory.GetFiles(
      'C:\Windows\Web', '*.jpg', TSearchOption.soAllDirectories);
    for var Ruta in Candidatos do
    begin
      if not SameText(Ruta, IMG1) then
      begin
        Img2Path := Ruta;
        Break;
      end;
    end;
  end;

  if SameText(Img2Path, IMG1) then
  begin
    WriteLn('  Solo se encontró una imagen diferente.');
    WriteLn('  Para este demo, ajusta IMG2 a otra imagen en tu sistema.');
    WriteLn('  (Mostrando el patrón con la misma imagen dos veces)');
    WriteLn('');
  end;

  MF1 := TAiMediaFile.Create;
  MF2 := TAiMediaFile.Create;
  MF1.LoadFromFile(IMG1);
  MF2.LoadFromFile(Img2Path);

  WriteLn('  Imagen 1: ', MF1.FileName);
  WriteLn('  Imagen 2: ', MF2.FileName);
  WriteLn('');

  // Enviar las dos imágenes en el mismo mensaje
  SetLength(Files, 2);
  Files[0] := MF1;
  Files[1] := MF2;

  Conn.NewChat;
  Write('  Comparando dos imagenes... ');
  R := Conn.AddMessageAndRun(
    'Analiza estas dos imágenes. ' +
    '¿Qué tienen en común? ¿Cuáles son las diferencias principales? ' +
    'Responde en 3-4 oraciones.',
    'user', Files);
  WriteLn('OK');
  WriteLn('');
  WriteLn('  Analisis comparativo:');
  WriteLn('  ', R);

  WriteLn('');
  WriteLn('→ Patron multi-imagen: SetLength(Files, N) + asignar cada MF.');
  WriteLn('  El modelo recibe ambas imágenes en el mismo contexto de mensaje.');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
var
  Conn: TAiChatConnection;
begin
  WriteLn('=== MakerAI — Capítulo 9: Visión con Ollama (100% local) ===');
  WriteLn('Modelo: ', MODEL, '  [ModelCaps = cap_Image — visión nativa]');
  WriteLn('La imagen nunca sale de tu máquina.');

  if not OllamaActivo then
  begin
    WriteLn('');
    WriteLn('[!!] Ollama no detectado en ', OLLAMA_URL);
    WriteLn('     Instalar : https://ollama.com');
    WriteLn('     Modelo   : ollama pull gemma3:4b');
    WriteLn('     Iniciar  : ollama serve');
    Exit;
  end;

  WriteLn('');
  WriteLn('[OK] Ollama detectado. Usando ', DRIVER, ' / ', MODEL);

  if not TFile.Exists(IMG1) then
  begin
    WriteLn('');
    WriteLn('[!!] Imagen de prueba no encontrada: ', IMG1);
    WriteLn('     Ajusta la constante IMG1 a una imagen JPEG en tu sistema.');
    Exit;
  end;

  Conn := CrearConexion;
  try
    Demo1_Descripcion(Conn);
    Demo2_OCR(Conn);
    Demo3_JSON(Conn);
    Demo4_MultiTurno(Conn);
    Demo5_DosImagenes(Conn);
  finally
    Conn.Free;
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
