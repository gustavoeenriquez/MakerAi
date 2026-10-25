program PuentesLadera;

{
  Capitulo 37 - Los demas puentes: cambiar una pieza sin tocar la app.

  El hueco de capacidades (SessionCaps - ModelCaps) se cubre con un puente: un componente que
  implementa una interfaz de uMakerAi.Chat.Tools (IAiSpeechTool, IAiVisionTool, IAiWebSearchTool...)
  y se asigna en ChatTools. El libro uso casi siempre el de OpenAI. Aqui, con el
  MISMO chat (deepseek-flash, que no oye, no ve y no busca), se cambia la pieza:
    --voz        la nota de voz de la clienta transcrita por OpenAI, ElevenLabs y Whisper; el chat
                 contesta con uno y con otro; la respuesta de Lucia dicha por ElevenLabs y por OpenAI
    --vision     la foto de la bolsa descrita por un modelo LOCAL (Ollama, gemma4) como VisionTool;
                 OCR local (deepseek-ocr) de un recorte de pantalla
    --web        busqueda web de OpenAI como WebSearchTool de un modelo que no busca
    --embeddings el mismo texto por cuatro proveedores (OpenAI, Mistral, Cohere, Ollama local):
                 dimensiones, tiempo y la misma comparacion de significado
  Sin argumentos corre los cuatro. Gemini (voz, web) queda anotado: la clave de esta maquina no
  es valida; el video (Veo) esta en el capitulo 31. Cafe La Ladera: empresa, productos y precios ficticios.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  System.Diagnostics,
  System.Net.HttpClient,
  Winapi.Windows,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.OpenAI.Audio.Tool,
  uMakerAi.ElevenLabs.Speech,
  uMakerAi.Whisper,
  uMakerAi.Ollama.Vision,
  uMakerAi.Ollama.Ocr,
  uMakerAi.OpenAI.WebSearch,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.Embeddings.Mistral,
  uMakerAi.Embeddings.Cohere,
  uMakerAi.Embeddings.Ollama;

function Tiene(const AArg: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AArg) then
      Exit(True);
end;

function Carpeta(const ASub: string): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), ASub);
  ForceDirectories(Result);
end;

function Media(const ANombre: string): string;
begin
  Result := TPath.Combine(Carpeta('media'), ANombre);
end;

procedure Titulo(const S: string);
begin
  Writeln;
  Writeln('== ' + S + ' ' + StringOfChar('=', 78 - Length(S)));
end;

function Recortar(const S: string; AMax: Integer = 160): string;
begin
  Result := StringReplace(Trim(S), sLineBreak, ' ', [rfReplaceAll]);
  if Length(Result) > AMax then
    Result := Copy(Result, 1, AMax) + '…';
end;

// Ollama puede ser local o de otra maquina de la red (OLLAMA_URL); aqui, un DGX Spark de la LAN
function OllamaUrl: string;
begin
  Result := GetEnvironmentVariable('OLLAMA_URL');
  if Result = '' then
    Result := 'http://localhost:11434/';
end;

type
  TOyente = class
    procedure Error(Sender: TObject; const AErrorMsg: string; E: Exception; const AResponse: IHTTPResponse);
  end;

procedure TOyente.Error(Sender: TObject; const AErrorMsg: string; E: Exception; const AResponse: IHTTPResponse);
begin
  Writeln('  [error] ' + Copy(StringReplace(AErrorMsg, sLineBreak, ' ', [rfReplaceAll]), 1, 200));
end;

var
  Oyente: TOyente;

// Un chat que NO oye, NO ve y NO busca: el hueco lo cubre el puente que se le ponga
function NuevoChat: TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := 'DeepSeek';
  Result.Model := 'deepseek-flash';
  Result.Params.Values['ApiKey'] := '@DEEPSEEK_API_KEY';
  Result.Params.Values['Asynchronous'] := 'False';
  Result.AiChat.ResponseTimeOut := 300000;
  Result.OnError := Oyente.Error;      // lo que falle en un puente se ve, no se traga
end;

procedure Preguntar(Conn: TAiChatConnection; const APregunta, AArchivo, ARotulo: string);
var
  MF: TAiMediaFile;
  Reloj: TStopwatch;
  R: string;
begin
  Conn.NewChat;
  Reloj := TStopwatch.StartNew;
  if AArchivo <> '' then
  begin
    MF := TAiMediaFile.Create;    // es del mensaje: no se libera aqui
    MF.LoadFromFile(AArchivo);
    R := Conn.AddMessageAndRun(APregunta, 'user', [MF]);
  end
  else
    R := Conn.AddMessageAndRun(APregunta, 'user', []);
  if R = '' then
    R := 'ERROR: ' + Conn.LastError;
  Writeln(Format('  %s (%.1f s): %s', [ARotulo, Reloj.ElapsedMilliseconds / 1000, Recortar(R, 300)]));
end;

{ ---------------------------------------------------------------------------- A: voz }

procedure Voz;
var
  OpenAi: TAiOpenAiSpeechTool;
  Eleven: TAiElevenLabsSpeechTool;
  Whisper: TAIWhisper;
  MF: TAiMediaFile;
  Res, Ask: TAiChatMessage;
  Reloj: TStopwatch;
  Conn: TAiChatConnection;
const
  FRASE = '¡Claro que sí, Marta! Te mando dos bolsas de Huila Reserva a Pitalito; con el envío quedan en 81.000 pesos.';
begin
  Titulo('A. Voz: la misma nota, tres oídos; la misma frase, dos voces');
  OpenAi := TAiOpenAiSpeechTool.Create(nil);
  Eleven := TAiElevenLabsSpeechTool.Create(nil);
  Whisper := TAIWhisper.Create(nil);
  MF := TAiMediaFile.Create;
  Res := TAiChatMessage.Create('', 'assistant');
  Ask := TAiChatMessage.Create('', 'user');
  try
    MF.LoadFromFile(Media('nota.wav'));
    // 1) Los puentes, llamados directo (sin chat) por su interfaz: cada uno deja el texto en MF.Transcription
    Reloj := TStopwatch.StartNew;
    (OpenAi as IAiSpeechTool).ExecuteTranscription(MF, Res, Ask);
    Writeln(Format('  OpenAI gpt-transcribe  (%.1f s): «%s»', [Reloj.ElapsedMilliseconds / 1000, Recortar(MF.Transcription)]));
    MF.Transcription := '';
    Reloj := TStopwatch.StartNew;
    try
      (Eleven as IAiSpeechTool).ExecuteTranscription(MF, Res, Ask);
      Writeln(Format('  ElevenLabs %s   (%.1f s): «%s»', [Eleven.SpeechToTextModel, Reloj.ElapsedMilliseconds / 1000, Recortar(MF.Transcription)]));
    except
      on E: Exception do
        Writeln('  ElevenLabs: ERROR ' + Recortar(E.Message));
    end;
    MF.Transcription := '';
    Reloj := TStopwatch.StartNew;
    try
      (Whisper as IAiSpeechTool).ExecuteTranscription(MF, Res, Ask);    // whisper-1 por el API de OpenAI (o un servidor Whisper propio con Url)
      Writeln(Format('  TAIWhisper whisper-1   (%.1f s): «%s»', [Reloj.ElapsedMilliseconds / 1000, Recortar(MF.Transcription)]));
    except
      on E: Exception do
        Writeln('  TAIWhisper: ERROR ' + Recortar(E.Message));
    end;
    MF.Transcription := '';
    Writeln('  Gemini (TAiGeminiSpeechTool): [PENDIENTE: la clave de Gemini de esta máquina no es válida]');

    // 2) El mismo chat, con un oido y con otro: solo cambia ChatTools.SpeechTool
    Conn := NuevoChat;
    try
      Conn.SessionCaps := [cap_Audio];                       // el hueco: deepseek-flash no oye
      Conn.ChatTools.SpeechTool := Eleven;
      Preguntar(Conn, '¿Qué me pide la clienta en esta nota de voz? Resume en una línea.', Media('nota.wav'), 'deepseek-flash + ElevenLabs');
      Conn.ChatTools.SpeechTool := OpenAi;
      Preguntar(Conn, '¿Qué me pide la clienta en esta nota de voz? Resume en una línea.', Media('nota.wav'), 'deepseek-flash + OpenAI    ');
    finally
      Conn.Free;
    end;

    // 3) La voz de Lucia por dos proveedores (ExecuteSpeechGeneration deja el audio en Res.MediaFiles)
    Reloj := TStopwatch.StartNew;
    try
      (Eleven as IAiSpeechTool).ExecuteSpeechGeneration(FRASE, Res, Ask);
      if Res.MediaFiles.Count > 0 then
      begin
        Res.MediaFiles[Res.MediaFiles.Count - 1].SaveToFile(TPath.Combine(Carpeta('salida'), 'lucia-elevenlabs.mp3'));
        Writeln(Format('  ElevenLabs %s, voz %s (%.1f s): salida\lucia-elevenlabs.mp3, %d KB', [Eleven.ModelId, Eleven.VoiceId,
          Reloj.ElapsedMilliseconds / 1000, Res.MediaFiles[Res.MediaFiles.Count - 1].Bytes div 1024]));
      end;
    except
      on E: Exception do
        Writeln('  ElevenLabs TTS: ERROR ' + Recortar(E.Message));
    end;
    Reloj := TStopwatch.StartNew;
    (OpenAi as IAiSpeechTool).ExecuteSpeechGeneration(FRASE, Res, Ask);
    if Res.MediaFiles.Count > 0 then
    begin
      Res.MediaFiles[Res.MediaFiles.Count - 1].SaveToFile(TPath.Combine(Carpeta('salida'), 'lucia-openai' +
        ExtractFileExt(Res.MediaFiles[Res.MediaFiles.Count - 1].filename)));
      Writeln(Format('  OpenAI gpt-4o-mini-tts (%.1f s): salida\lucia-openai%s, %d KB', [Reloj.ElapsedMilliseconds / 1000,
        ExtractFileExt(Res.MediaFiles[Res.MediaFiles.Count - 1].filename), Res.MediaFiles[Res.MediaFiles.Count - 1].Bytes div 1024]));
    end;
  finally
    Res.Free;
    Ask.Free;
    MF.Free;
    OpenAi.Free;
    Eleven.Free;
    Whisper.Free;
  end;
end;

{ ---------------------------------------------------------------------------- B: vision local }

procedure Vision;
var
  Ojo: TAiOllamaVisionTool;
  Ocr: TAiOllamaOcrTool;
  Conn: TAiChatConnection;
  MF: TAiMediaFile;
  Res, Ask: TAiChatMessage;
  Reloj: TStopwatch;
begin
  Titulo('B. Visión y OCR locales (Ollama): el modelo no ve, la máquina sí');
  Ojo := TAiOllamaVisionTool.Create(nil);
  Ocr := TAiOllamaOcrTool.Create(nil);
  try
    Ojo.Url := OllamaUrl;
    Ojo.Model := 'gemma4:e4b';                  // un modelo con vision, en el Ollama de OllamaUrl
    Ojo.Timeout := 300000;
    Ocr.Url := OllamaUrl;
    Ocr.Timeout := 300000;                      // la primera vez carga el modelo
    Ocr.Prompt := 'Free OCR.';                  // por Ollama, el prompt de grounding (el default) devuelve tokens y tablas rotas
    Writeln('  Ollama en ' + OllamaUrl);
    Conn := NuevoChat;
    try
      Conn.SessionCaps := [cap_Image];          // el hueco: deepseek-flash no ve
      Conn.ChatTools.VisionTool := Ojo;         // gemma4 describe la foto; el chat recibe texto
      Preguntar(Conn, '¿Qué producto aparece en esta foto y en qué estado está? Contesta en dos líneas.', Media('bolsa.png'),
        'deepseek-flash + gemma4:e4b (Ollama)');
    finally
      Conn.Free;
    end;
    // OCR directo, sin chat: una pantalla a texto, en una maquina propia
    MF := TAiMediaFile.Create;
    Res := TAiChatMessage.Create('', 'assistant');
    Ask := TAiChatMessage.Create('', 'user');     // sin prompt: el del tool (<|grounding|>Convert the document to markdown)
    try
      MF.LoadFromFile(Media('guia-andina.png'));    // la pantalla de Andina Despachos del capitulo 35
      Reloj := TStopwatch.StartNew;
      try
        (Ocr as IAiVisionTool).ExecuteImageDescription(MF, Res, Ask);
        Writeln(Format('  OCR %s (%.1f s): «%s»', [Ocr.Model, Reloj.ElapsedMilliseconds / 1000, Recortar(MF.Transcription, 260)]));
      except
        on E: Exception do
          Writeln(Format('  OCR %s: ERROR %s', [Ocr.Model, Recortar(E.Message, 160)]));
      end;
    finally
      MF.Free;
      Res.Free;
      Ask.Free;
    end;
  finally
    Ojo.Free;
    Ocr.Free;
  end;
end;

{ ---------------------------------------------------------------------------- C: web }

procedure Web;
var
  Buscador: TAiOpenAiWebSearchTool;
  Conn: TAiChatConnection;
begin
  Titulo('C. Búsqueda web: un modelo que no busca, con el buscador de otro');
  Buscador := TAiOpenAiWebSearchTool.Create(nil);
  Conn := NuevoChat;
  try
    // Directo, sin chat: el puente devuelve texto con citas (ResMsg.Citations)
    var Res := TAiChatMessage.Create('', 'assistant');
    var Ask := TAiChatMessage.Create('', 'user');
    var Reloj := TStopwatch.StartNew;
    try
      (Buscador as IAiWebSearchTool).ExecuteSearch('precio interno de referencia carga de café 125 kg Colombia hoy Federación Nacional de Cafeteros', Res, Ask);
      Writeln(Format('  búsqueda directa %s (%.1f s, %d citas): %s', [Buscador.Model, Reloj.ElapsedMilliseconds / 1000, Res.Citations.Count, Recortar(Res.Prompt, 220)]));
      if Res.Citations.Count > 0 then
        Writeln('    fuente: ' + Res.Citations[0].Sources[0].DataSource.Url);
    finally
      Res.Free;
      Ask.Free;
    end;
    Conn.SessionCaps := [cap_WebSearch];                    // el hueco: deepseek-flash no tiene web
    Conn.ChatTools.WebSearchTool := Buscador;               // OpenAI busca; DeepSeek redacta con lo encontrado
    Preguntar(Conn, '¿Cuál es el precio interno de referencia de la carga de café de 125 kg en Colombia hoy, según la Federación Nacional de Cafeteros? Una línea con la fecha.',
      '', 'deepseek-flash + búsqueda OpenAI (' + Buscador.Model + ')');
    Writeln('  Gemini (TAiGeminiWebSearchTool, grounding): [PENDIENTE: clave de Gemini no válida]');
  finally
    Conn.Free;
    Buscador.Free;
  end;
end;

{ ---------------------------------------------------------------------------- E: embeddings }

procedure Embeddings;
const
  T1 = 'Huila Reserva: café de Pitalito con notas de panela y frutos rojos';
  T2 = 'un café dulce, con sabor a fruta';
  T3 = 'guía de despacho de la transportadora a Cali';
  DRIVERS: array [0 .. 3] of string = ('OpenAi', 'Mistral', 'Cohere', 'Ollama');
var
  Emb: TAiEmbeddingConnection;
  D: string;
  V1, V2, V3: TAiEmbeddingData;
  Reloj: TStopwatch;
begin
  Titulo('E. Embeddings: el mismo significado por cuatro proveedores');
  Writeln('  T1 = «' + T1 + '»  T2 = «' + T2 + '»  T3 = «' + T3 + '»');
  Emb := TAiEmbeddingConnection.Create(nil);
  try
    for D in DRIVERS do
    begin
      try
        Emb.DriverName := D;                      // el conector crea el driver; la clave sale de @<PROVEEDOR>_API_KEY
        if D = 'Ollama' then
        begin
          Emb.Model := 'nomic-embed-text';        // sin clave; OJO: Model recarga los parametros del catalogo
          Emb.Url := OllamaUrl;                   //   (incluida la Url), asi que la Url va DESPUES del modelo
        end;
        Reloj := TStopwatch.StartNew;
        V1 := Emb.CreateEmbedding(T1, 'ladera');
        V2 := Emb.CreateEmbedding(T2, 'ladera');
        V3 := Emb.CreateEmbedding(T3, 'ladera');
        Writeln(Format('  %-8s %-26s %5d dims · %4d ms los tres · cos(T1,T2)=%.3f  cos(T1,T3)=%.3f', [D, Emb.Model, Length(V1),
          Reloj.ElapsedMilliseconds, TAiEmbeddingsCore.CosineSimilarity(V1, V2), TAiEmbeddingsCore.CosineSimilarity(V1, V3)]));
      except
        on E: Exception do
          Writeln(Format('  %-8s ERROR %s', [D, Recortar(E.Message, 120)]));
      end;
    end;
    Writeln('  Qwen (text-embedding-v4): [sin DASHSCOPE_API_KEY en esta máquina]');
  finally
    Emb.Free;
  end;
end;

begin
  SetConsoleOutputCP(CP_UTF8);
  Oyente := TOyente.Create;
  try
    if (ParamCount = 0) or Tiene('--voz') then
      Voz;
    if (ParamCount = 0) or Tiene('--vision') then
      Vision;
    if (ParamCount = 0) or Tiene('--web') then
      Web;
    if (ParamCount = 0) or Tiene('--embeddings') then
      Embeddings;
  except
    on E: Exception do
    begin
      Writeln('ERROR: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
  Oyente.Free;
end.
