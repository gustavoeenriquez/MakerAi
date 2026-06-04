program SmartDispatchDemo;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI -- Capitulo 16: SmartDispatch -- IA con Herramientas sin Function Calling
// =============================================================================
//
//   Demo 1 -- Routing basico con herramientas simuladas
//             5 casos: CHAT, IMAGEGEN, WEBSEARCH, CHAT (calculo), AMBIGUO
//             Solo requiere Ollama local corriendo en http://localhost:11434
//
//   Demo 2 -- Cuatro herramientas en el dispatcher
//             IMAGE + VIDEO + TTS + WEBSEARCH activas simultaneamente
//             El prompt de despacho se construye dinamicamente segun las
//             herramientas asignadas a la conexion
//
//   Demo 3 -- Hibrido: modelo local Ollama + herramienta real DALL-E (opcional)
//             El modelo local clasifica la solicitud (sin costo de API)
//             DALL-E genera la imagen real (requiere OPENAI_API_KEY)
//
// Requisitos:
//   Ollama corriendo en http://localhost:11434
//   Modelo gemma3:4b instalado (o cambiar MODEL por otro modelo de instrucciones)
//   OPENAI_API_KEY configurado como variable de entorno (solo para Demo 3)
//
// IMPORTANTE: cmSmartDispatch requiere Asynchronous=False
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Winapi.Windows,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Ollama,
  uMakerAi.OpenAi.Dalle;

const
  OLLAMA_URL = 'http://localhost:11434/';
  MODEL      = 'gemma3:4b';    // Modelo recomendado: mejor seguimiento de instrucciones que gemma4:e4b

// =============================================================================
//  Herramientas simuladas (mock) para validar routing sin llamadas a APIs reales
//
//  Cada mock imprime el argumento que recibio del dispatcher para que se vea
//  que el Pase 1 reescribio la solicitud antes de ejecutar la herramienta.
// =============================================================================

type
  TMockImageTool = class(TAiImageToolBase)
  protected
    procedure ExecuteImageGeneration(const APrompt: string;
      ResMsg, AskMsg: TAiChatMessage); override;
  end;

  TMockVideoTool = class(TAiVideoToolBase)
  protected
    procedure ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage); override;
  end;

  TMockSpeechTool = class(TAiSpeechToolBase)
  protected
    procedure ExecuteSpeechGeneration(const AText: string;
      ResMsg, AskMsg: TAiChatMessage); override;
  end;

  TMockWebSearchTool = class(TAiWebSearchToolBase)
  protected
    procedure ExecuteSearch(const AQuery: string;
      ResMsg, AskMsg: TAiChatMessage); override;
  end;

procedure TMockImageTool.ExecuteImageGeneration(const APrompt: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  WriteLn('    >> [IMAGEGEN] prompt reescrito: "', APrompt, '"');
  ResMsg.Prompt := '[imagen generada] ' + APrompt;
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

procedure TMockVideoTool.ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage);
begin
  // ExecuteVideoGeneration no recibe APrompt -- el prompt esta en AskMsg.Prompt
  WriteLn('    >> [VIDEOGEN] solicitud original: "', AskMsg.Prompt, '"');
  ResMsg.Prompt := '[video generado] ' + AskMsg.Prompt;
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

procedure TMockSpeechTool.ExecuteSpeechGeneration(const AText: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  WriteLn('    >> [TTS] texto a leer: "', Copy(AText, 1, 60), '"');
  ResMsg.Prompt := '[audio generado] ' + AText;
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

procedure TMockWebSearchTool.ExecuteSearch(const AQuery: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  WriteLn('    >> [WEBSEARCH] consulta optimizada: "', AQuery, '"');
  ResMsg.Prompt := '[resultado de busqueda] ' + AQuery;
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

// =============================================================================

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

procedure Probar(Conn: TAiChatConnection; const Etiqueta, Mensaje: string);
var
  Resp: string;
begin
  Write('  [', Etiqueta, '] "', Copy(Mensaje, 1, 48), '"');
  try
    Resp := Conn.AddMessageAndRun(Mensaje, 'user', []);
    WriteLn;
    WriteLn('    Respuesta: ', Copy(Resp, 1, 80));
  except
    on E: Exception do
      WriteLn('  ERROR: ', E.ClassName, ': ', E.Message);
  end;
end;

// =============================================================================
//  Demo 1 -- Routing basico con herramientas simuladas
//
//  Demuestra el flujo en dos pasos de cmSmartDispatch:
//    Pase 1 (silencioso): el modelo elige [IMAGEGEN], [WEBSEARCH] o [CHAT]
//    Pase 2             : el framework ejecuta la herramienta correspondiente
//
//  Los mock tools imprimen el argumento recibido del dispatcher -- ahi se ve
//  que el Pase 1 reescribio la solicitud en ingles y en forma mas precisa.
//  No requiere API keys: solo Ollama local.
// =============================================================================
procedure Demo1_RoutingBasico;
var
  Conn      : TAiChatConnection;
  ImageTool : TMockImageTool;
  SearchTool: TMockWebSearchTool;
begin
  Separador('Demo 1 -- Routing basico con herramientas simuladas');
  WriteLn('  Driver: Ollama / ', MODEL);
  WriteLn('  Tools : ImageTool [IMAGEGEN] + WebSearchTool [WEBSEARCH]');
  WriteLn('  Los mock tools muestran el prompt reescrito por el dispatcher');
  WriteLn('');

  ImageTool  := TMockImageTool.Create(nil);
  SearchTool := TMockWebSearchTool.Create(nil);
  Conn       := TAiChatConnection.Create(nil);
  try
    Conn.ImageTool     := ImageTool;
    Conn.WebSearchTool := SearchTool;

    Conn.DriverName                    := 'Ollama';
    Conn.Params.Values['URL']          := OLLAMA_URL;
    Conn.Params.Values['Model']        := MODEL;
    Conn.Params.Values['Asynchronous'] := 'False';   // REQUERIDO
    Conn.Params.Values['Max_Tokens']   := '128';

    Conn.ChatMode          := cmSmartDispatch;
    Conn.SystemPrompt.Text := 'Eres un asistente util. Responde siempre en espanol.';

    // [CHAT] -- pregunta directa de conversacion
    Probar(Conn, 'CHAT esperado    ', 'Cual es la capital de Francia?');

    // [IMAGEGEN] -- solicitud clara de generacion de imagen
    Probar(Conn, 'IMAGEGEN esperado', 'Genera una imagen de un gato con sombrero de cowboy');

    // [WEBSEARCH] -- solicitud de informacion reciente o busqueda web
    Probar(Conn, 'WEBSEARCH esperado', 'Busca noticias recientes sobre inteligencia artificial');

    // [CHAT] -- calculo matematico (no necesita herramientas)
    Probar(Conn, 'CHAT esperado    ', 'Cuanto es 15 multiplicado por 7?');

    // Ambiguo: el modelo decide entre [IMAGEGEN] y [CHAT]
    Probar(Conn, 'AMBIGUO          ', 'Muestrame como se ve un arcoiris');

    WriteLn('');
    WriteLn('-> El dispatcher reescribe las solicitudes en ingles para mayor precision.');
    WriteLn('   [CHAT] devuelve directamente la respuesta del Pase 1 (sin Pase 2).');
    WriteLn('   Las herramientas reciben el prompt ya optimizado por el Pase 1.');
  finally
    Conn.Free;
    ImageTool.Free;
    SearchTool.Free;
  end;
end;

// =============================================================================
//  Demo 2 -- Cuatro herramientas: el prompt de despacho es dinamico
//
//  BuildSmartDispatchPrompt incluye solo las etiquetas de las herramientas
//  que estan asignadas. Con las 4 herramientas activas el modelo dispone de:
//    [IMAGEGEN]  [VIDEOGEN]  [TTS]  [WEBSEARCH]  [CHAT]
//
//  Diferencia importante con Demo 1:
//    ExecuteVideoGeneration NO tiene parametro APrompt (a diferencia de
//    ExecuteImageGeneration y ExecuteSpeechGeneration). El prompt original
//    del usuario esta disponible en AskMsg.Prompt.
// =============================================================================
procedure Demo2_CuatroHerramientas;
var
  Conn      : TAiChatConnection;
  ImageTool : TMockImageTool;
  VideoTool : TMockVideoTool;
  SpeechTool: TMockSpeechTool;
  SearchTool: TMockWebSearchTool;
begin
  Separador('Demo 2 -- Cuatro herramientas: prompt de despacho dinamico');
  WriteLn('  Driver: Ollama / ', MODEL);
  WriteLn('  Tools : IMAGE + VIDEO + TTS + WEBSEARCH (5 etiquetas disponibles)');
  WriteLn('');

  ImageTool  := TMockImageTool.Create(nil);
  VideoTool  := TMockVideoTool.Create(nil);
  SpeechTool := TMockSpeechTool.Create(nil);
  SearchTool := TMockWebSearchTool.Create(nil);
  Conn       := TAiChatConnection.Create(nil);
  try
    Conn.ImageTool     := ImageTool;
    Conn.VideoTool     := VideoTool;
    Conn.SpeechTool    := SpeechTool;
    Conn.WebSearchTool := SearchTool;

    Conn.DriverName                    := 'Ollama';
    Conn.Params.Values['URL']          := OLLAMA_URL;
    Conn.Params.Values['Model']        := MODEL;
    Conn.Params.Values['Asynchronous'] := 'False';   // REQUERIDO
    Conn.Params.Values['Max_Tokens']   := '128';

    Conn.ChatMode          := cmSmartDispatch;
    Conn.SystemPrompt.Text := 'Eres un asistente util. Responde en espanol.';

    Probar(Conn, 'IMAGEGEN  ', 'Dibuja un paisaje montanoso al atardecer');
    Probar(Conn, 'VIDEOGEN  ', 'Crea un video corto de olas en la playa');
    Probar(Conn, 'TTS       ', 'Lee en voz alta: Bienvenido a MakerAI');
    Probar(Conn, 'WEBSEARCH ', 'Que noticias importantes hay hoy?');
    Probar(Conn, 'CHAT      ', 'Explica en una frase que es la inteligencia artificial');

    WriteLn('');
    WriteLn('-> Con 4 tools asignadas, el dispatcher conoce las 5 etiquetas.');
    WriteLn('   Si se desasigna una herramienta, su etiqueta desaparece del prompt.');
    WriteLn('   ExecuteVideoGeneration NO tiene APrompt: usa AskMsg.Prompt directamente.');
  finally
    Conn.Free;
    ImageTool.Free;
    VideoTool.Free;
    SpeechTool.Free;
    SearchTool.Free;
  end;
end;

// =============================================================================
//  Demo 3 -- Hibrido: modelo local Ollama (clasifica) + DALL-E directo (genera)
//
//  Patron en dos pasos EXPLICITOS:
//    Paso 1 -- Ollama clasifica la solicitud y reescribe el prompt en ingles
//              Herramienta capturadora: captura el prompt reescrito sin generar imagen
//    Paso 2 -- TAiDalle.Generate() usa el prompt capturado para generar la imagen real
//
//  Por que dos pasos explícitos?
//    TAiDalleImageTool es internamente asíncrono (TTask.Run). En cmSmartDispatch
//    (que requiere Asynchronous=False) la imagen llegaría después de que
//    AddMessageAndRun retorna, dando resultado vacío. Separar los pasos es más
//    claro y evita el conflicto sync/async.
// =============================================================================

type
  TCapturingImageTool = class(TAiImageToolBase)
  public
    CapturedPrompt: string;
  protected
    procedure ExecuteImageGeneration(const APrompt: string;
      ResMsg, AskMsg: TAiChatMessage); override;
  end;

procedure TCapturingImageTool.ExecuteImageGeneration(const APrompt: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  CapturedPrompt := APrompt;   // solo captura; la generacion real va en el Paso 2
  ResMsg.Prompt  := '[clasificado] ' + APrompt;
  ResMsg.Role    := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

procedure Demo3_Hibrido;
var
  Conn      : TAiChatConnection;
  CapTool   : TCapturingImageTool;
  Dalle     : TAiDalle;
  DalleImg  : TAiDalleImage;
  OutPath   : string;
begin
  Separador('Demo 3 -- Hibrido: Ollama (clasificador) + gpt-image-1 (generador)');
  WriteLn('  Clasificador: Ollama / ', MODEL, '  (sin costo de API)');
  WriteLn('  Generador   : gpt-image-1  (OpenAI, llamada directa)');
  WriteLn('');

  CapTool := TCapturingImageTool.Create(nil);
  Conn    := TAiChatConnection.Create(nil);
  try
    Conn.ImageTool                     := CapTool;
    Conn.DriverName                    := 'Ollama';
    Conn.Params.Values['URL']          := OLLAMA_URL;
    Conn.Params.Values['Model']        := MODEL;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '128';
    Conn.ChatMode          := cmSmartDispatch;
    Conn.SystemPrompt.Text := 'Eres un asistente visual. Genera imagenes cuando se soliciten.';

    WriteLn('  Solicitud: "Genera una imagen de un robot leyendo un libro de Delphi"');
    WriteLn('');
    Write('  Paso 1 -- Ollama clasifica... ');
    Conn.AddMessageAndRun(
      'Genera una imagen de un robot leyendo un libro de Delphi', 'user', []);
    WriteLn('OK');
  finally
    Conn.Free;
    // CapTool.Free: se libera despues de usar CapturedPrompt
  end;

  if CapTool.CapturedPrompt <> '' then
  begin
    WriteLn('  Prompt reescrito: "', CapTool.CapturedPrompt, '"');
    WriteLn('');
    Write('  Paso 2 -- gpt-image-1 genera la imagen... ');
    Dalle := TAiDalle.Create(nil);
    try
      Dalle.ApiKey       := '@OPENAI_API_KEY';
      Dalle.Model        := imGptImage1;
      Dalle.Quality      := iqMedium;
      Dalle.OutputFormat := ifPng;
      DalleImg := Dalle.Generate(CapTool.CapturedPrompt, '', is1024x1024);
      if Assigned(DalleImg) and (DalleImg.Image <> nil) then
      begin
        WriteLn('OK');
        OutPath := TPath.Combine(TPath.GetTempPath, 'cap16_demo3_robot.png');
        DalleImg.Image.Position := 0;
        DalleImg.Image.SaveToFile(OutPath);
        WriteLn('  Imagen guardada: ', OutPath,
                '  (', DalleImg.Image.Size div 1024, ' KB)');
      end
      else
        WriteLn('SIN RESULTADO');
    finally
      Dalle.Free;
    end;
  end
  else
  begin
    WriteLn('  [NOTA] El modelo eligio [CHAT] en lugar de [IMAGEGEN].');
    WriteLn('  Intenta con una solicitud mas explicita: "dibuja" o "crea imagen de..."');
  end;
  CapTool.Free;

  WriteLn('');
  WriteLn('-> El modelo local solo produce ~10-30 tokens (clasificacion + reescritura).');
  WriteLn('   gpt-image-1 recibe el prompt ya optimizado en ingles por el dispatcher.');
  WriteLn('   Este patron separa la clasificacion barata de la generacion costosa.');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
var
  TieneOpenAI: Boolean;
begin
  SetConsoleOutputCP(65001);
  WriteLn('=== MakerAI -- Capitulo 16: SmartDispatch ===');
  WriteLn('Demos: routing inteligente para modelos sin function calling');
  WriteLn('');
  WriteLn('  Driver local : Ollama -- ', OLLAMA_URL);
  WriteLn('  Modelo       : ', MODEL);
  WriteLn('  IMPORTANTE   : cmSmartDispatch requiere Asynchronous=False');
  WriteLn('');

  TieneOpenAI := GetEnvironmentVariable('OPENAI_API_KEY') <> '';

  WriteLn('  Estado del entorno:');
  WriteLn('  Ollama -- si esta corriendo en ', OLLAMA_URL, ' los demos 1 y 2 funcionan');
  if TieneOpenAI then WriteLn('  [OK] OPENAI_API_KEY detectado -- Demo 3 activo')
  else            WriteLn('  [--] OPENAI_API_KEY no configurado -- Demo 3 omitido');

  try Demo1_RoutingBasico;
  except on E: Exception do WriteLn('  [Demo 1 ERROR] ', E.ClassName, ': ', E.Message); end;

  try Demo2_CuatroHerramientas;
  except on E: Exception do WriteLn('  [Demo 2 ERROR] ', E.ClassName, ': ', E.Message); end;

  if TieneOpenAI then
  begin
    try Demo3_Hibrido;
    except on E: Exception do WriteLn('  [Demo 3 ERROR] ', E.ClassName, ': ', E.Message); end;
  end
  else
    WriteLn(#13#10'--- Demo 3 omitido (OPENAI_API_KEY no configurado) ---');

  WriteLn('');
  WriteLn(StringOfChar('=', 62));
  WriteLn('  Demos completados.');
  WriteLn('  Ver cap16-smartdispatch.md para la documentacion completa.');
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
