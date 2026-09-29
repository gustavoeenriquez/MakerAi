program QwenShowcase;

// =============================================================================
// DEMO 089 - Qwen (Alibaba Model Studio) en MakerAI
// =============================================================================
// Un recorrido por todo lo que MakerAI hace con Qwen usando una sola API key.
// Cada seccion se puede correr sola:
//
//   chat        sincrono, streaming, razonamiento con presupuesto, herramientas
//   vision      generar una imagen -> editarla -> describirla
//   voz         TTS -> transcripcion con contexto -> audio entendido por omni
//   traduccion  qwen-mt-plus con glosario y dominio, dos idiomas destino
//   rag         embeddings (ranking por similitud) + reranker con instruccion
//   realtime    LLM -> voz en tiempo real y traduccion simultanea por el
//               conector universal, con DriverParams
//   --video     texto a video con Wan, 2 s a 720p (cuesta mas: solo si se pide)
//   --voces     clonar una voz, hablar con ella y borrarla (idem)
//
// Uso:  QwenShowcase.exe                    todas menos video y voces
//       QwenShowcase.exe vision voz         solo esas
//       QwenShowcase.exe --video --voces    agrega las opcionales
//
// Requiere DASHSCOPE_API_KEY (region internacional, Singapur). Los archivos
// generados quedan en la carpeta 'salida' del demo.
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Math,
  System.StrUtils,
  System.SyncObjs,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,   // registra los drivers, entre ellos 'Qwen'
  uMakerAi.Chat.Qwen,
  uMakerAi.Tools.Functions,
  uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Realtime,
  uMakerAi.Realtime.AiConnection,
  uMakerAi.Qwen.Rerank in '..\..\Source\Tools\uMakerAi.Qwen.Rerank.pas',
  uMakerAi.Qwen.Voices in '..\..\Source\Tools\uMakerAi.Qwen.Voices.pas',
  uMakerAi.Realtime.Qwen in '..\..\Source\Realtime\uMakerAi.Realtime.Qwen.pas',
  uMakerAi.Realtime.QwenTTS in '..\..\Source\Realtime\uMakerAi.Realtime.QwenTTS.pas';

type
  // Los eventos del framework son 'of object': los manejadores viven en una clase
  TDemo = class
  public
    Fin: TEvent;
    Texto: string;
    Error: string;
    Herramienta: string;
    // Realtime
    Listo, Terminado: Boolean;
    Audio: TMemoryStream;
    Original, Traduccion: string;
    PrimerAudioMs: Integer;
    T0: Cardinal;
    TTS: TAiQwenRealtimeTTS;
    constructor Create;
    destructor Destroy; override;
    procedure Reiniciar;
    // Chat
    procedure OnDelta(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure OnFin(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string; Exception: Exception; const AResponse: IHTTPResponse);
    procedure OnCambio(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string;
      ToolCall: TAiToolsFunction; var Handled: Boolean);
    // LLM -> voz: cada fragmento del chat va directo al TTS en tiempo real
    procedure OnDeltaAVoz(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure OnFinAVoz(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    // Realtime
    procedure OnListo(Sender: TObject);
    procedure OnAudio(Sender: TObject; const AData: TBytes);
    procedure OnTerminado(Sender: TObject);
    procedure OnOriginal(Sender: TObject; const Transcript, ItemId: string);
    procedure OnTraduccion(Sender: TObject; const AText: string);
    procedure OnRtError(Sender: TObject; const ErrorMsg, ErrorCode: string);
  end;

var
  Demo: TDemo;
  Salida: string;
  Secciones: TStringList;

{ TDemo }

constructor TDemo.Create;
begin
  Fin := TEvent.Create(nil, True, False, '');
  Audio := TMemoryStream.Create;
end;

destructor TDemo.Destroy;
begin
  Fin.Free;
  Audio.Free;
  inherited;
end;

procedure TDemo.Reiniciar;
begin
  Fin.ResetEvent;
  Texto := '';
  Error := '';
  Herramienta := '';
  Listo := False;
  Terminado := False;
  Audio.Clear;
  Original := '';
  Traduccion := '';
  PrimerAudioMs := -1;
  T0 := TThread.GetTickCount;
end;

procedure TDemo.OnDelta(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  if aRole = 'assistant' then
    Write(aText); // el texto aparece a medida que llega
end;

procedure TDemo.OnFin(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  Texto := aText;
  Fin.SetEvent;
end;

procedure TDemo.OnError(Sender: TObject; const ErrorMsg: string; Exception: Exception; const AResponse: IHTTPResponse);
begin
  Error := ErrorMsg;
  Fin.SetEvent;
end;

procedure TDemo.OnCambio(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  // Una funcion local de verdad devolveria la tasa del dia
  Herramienta := ToolCall.Arguments;
  ToolCall.Response := '{"moneda":"USD","tasa_cop":4012.35,"fecha":"2026-09-28"}';
  Handled := True;
end;

procedure TDemo.OnDeltaAVoz(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  if aRole = 'assistant' then
  begin
    Write(aText);
    TTS.AppendText(aText);
  end;
end;

procedure TDemo.OnFinAVoz(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  TTS.Finish; // sintetiza lo pendiente; OnFinished llega cuando termina
end;

procedure TDemo.OnListo(Sender: TObject);
begin
  Listo := True;
end;

procedure TDemo.OnAudio(Sender: TObject; const AData: TBytes);
begin
  if PrimerAudioMs < 0 then
    PrimerAudioMs := TThread.GetTickCount - T0;
  if Length(AData) > 0 then
    Audio.WriteBuffer(AData[0], Length(AData));
end;

procedure TDemo.OnTerminado(Sender: TObject);
begin
  Terminado := True;
end;

procedure TDemo.OnOriginal(Sender: TObject; const Transcript, ItemId: string);
begin
  Original := Transcript;
end;

procedure TDemo.OnTraduccion(Sender: TObject; const AText: string);
begin
  Traduccion := AText;
  Terminado := True;
end;

procedure TDemo.OnRtError(Sender: TObject; const ErrorMsg, ErrorCode: string);
begin
  Error := ErrorCode + ': ' + ErrorMsg;
end;

// -----------------------------------------------------------------------------
// Utilidades
// -----------------------------------------------------------------------------

procedure Titulo(const S: string);
begin
  Writeln;
  Writeln('=== ', S, ' ', StringOfChar('=', Max(0, 70 - Length(S))));
end;

// Los eventos llegan por TThread.Queue: en consola hay que drenar la cola
procedure Esperar(ACond: TFunc<Boolean>; AMs: Integer = 60000);
var
  T0: Cardinal;
begin
  T0 := TThread.GetTickCount;
  while not ACond() and (TThread.GetTickCount - T0 < Cardinal(AMs)) do
    CheckSynchronize(20);
end;

function Conexion(const AModel: string; AAsync: Boolean = False): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := 'Qwen';
  Result.Model := AModel; // despues de DriverName: cambiar de driver lo vacia
  Result.Params.Values['Asynchronous'] := BoolToStr(AAsync, True);
  Result.OnReceiveDataEnd := Demo.OnFin;
  Result.OnError := Demo.OnError;
  Demo.Reiniciar;
end;

function Ultimo(C: TAiChatConnection): TAiChatMessage;
begin
  Result := C.GetLastMessage;
end;

// Guarda PCM16 mono como WAV reproducible
procedure GuardarWav(const AArchivo: string; APcm: TMemoryStream; ASampleRate: Integer);
var
  F: TFileStream;
  DataSize, ByteRate: Cardinal;
  procedure W32(V: Cardinal); begin F.WriteBuffer(V, 4); end;
  procedure W16(V: Word); begin F.WriteBuffer(V, 2); end;
  procedure Tag(const S: AnsiString); begin F.WriteBuffer(S[1], 4); end;
begin
  DataSize := APcm.Size;
  ByteRate := ASampleRate * 2;
  F := TFileStream.Create(AArchivo, fmCreate);
  try
    Tag('RIFF'); W32(36 + DataSize); Tag('WAVE');
    Tag('fmt '); W32(16); W16(1); W16(1); W32(ASampleRate); W32(ByteRate); W16(2); W16(16);
    Tag('data'); W32(DataSize);
    APcm.Position := 0;
    F.CopyFrom(APcm, DataSize);
  finally
    F.Free;
  end;
end;

function Quiere(const ASeccion: string): Boolean;
begin
  Result := Secciones.IndexOf(ASeccion) >= 0;
end;

function Corto(const S: string; N: Integer = 160): string;
begin
  Result := StringReplace(Trim(S), sLineBreak, ' ', [rfReplaceAll]);
  if Length(Result) > N then
    Result := Copy(Result, 1, N) + '...';
end;

// -----------------------------------------------------------------------------
// chat
// -----------------------------------------------------------------------------

procedure SeccionChat;
var
  C: TAiChatConnection;
  F: TAiFunctions;
  Fn: TFunctionActionItem;
begin
  Titulo('CHAT');

  // 1. Sincrono: Qwen razona por defecto en el API; el driver lo apaga si el
  //    modelo no tiene cap_Reasoning (rapido y sin pagar tokens de razonar)
  C := Conexion('qwen3.8-flash');
  try
    Writeln('1. Sincrono (qwen3.8-flash):');
    Writeln('   ', Corto(C.AddMessageAndRun('Que es la partida doble? Responde en una frase.', 'user', [])));
  finally
    C.Free;
  end;

  // 2. Streaming: el texto se imprime a medida que llega
  C := Conexion('qwen3.8-flash', True);
  try
    C.OnReceiveData := Demo.OnDelta;
    Write('2. Streaming: ');
    C.AddMessageAndRun('Nombra tres cuentas del activo corriente, separadas por comas.', 'user', []);
    Esperar(function: Boolean begin Result := Demo.Fin.WaitFor(0) = wrSignaled end);
    Writeln;
  finally
    C.Free;
  end;

  // 3. Razonamiento: cap_Reasoning lo activa y ThinkingLevel fija el presupuesto
  C := Conexion('qwen3.8-flash');
  try
    C.Params.Values['ModelCaps'] := '[cap_Image, cap_Reasoning]';
    C.Params.Values['ThinkingLevel'] := 'tlLow'; // thinking_budget = 1024 tokens
    Writeln('3. Razonamiento (tlLow):');
    Writeln('   ', Corto(C.AddMessageAndRun('Si vendo con 19% de IVA a 119.000, cual es la base gravable? Solo el numero.', 'user', [])));
    Writeln(Format('   razonamiento: %d caracteres', [Length(Ultimo(C).ReasoningContent)]));
  finally
    C.Free;
  end;

  // 4. Herramientas: el modelo pide la funcion, MakerAI la ejecuta y sigue
  F := TAiFunctions.Create(nil);
  C := Conexion('qwen3.8-flash');
  try
    Fn := F.Functions.Add;
    Fn.FunctionName := 'tasa_de_cambio';
    Fn.Description.Text := 'Tasa de cambio del dia de una moneda a pesos colombianos';
    with Fn.Parameters.Add do
    begin
      Name := 'moneda';
      ParamType := ptString;
      Required := True;
      Description.Text := 'Codigo ISO de la moneda, p.ej. USD';
    end;
    Fn.OnAction := Demo.OnCambio;
    C.AiFunctions := F;
    Writeln('4. Herramientas:');
    Writeln('   ', Corto(C.AddMessageAndRun('Cuantos pesos colombianos son 100 dolares hoy?', 'user', [])));
    Writeln('   la funcion se llamo con: ', Demo.Herramienta);
  finally
    C.Free;
    F.Free;
  end;
end;

// -----------------------------------------------------------------------------
// vision: generar -> editar -> describir
// -----------------------------------------------------------------------------

procedure SeccionVision;
var
  C: TAiChatConnection;
  M: TAiMediaFile;
  Faro, FaroVerde: string;
begin
  Titulo('VISION');
  Faro := TPath.Combine(Salida, 'faro.png');
  FaroVerde := TPath.Combine(Salida, 'faro_verde.png');

  // 1. Generar: un modelo de imagen tiene cap_GenImage en SessionCaps
  C := Conexion('qwen-image-2.0');
  try
    C.AddMessageAndRun('Un faro rojo y blanco sobre rocas al atardecer, acuarela', 'user', []);
    if Ultimo(C).MediaFiles.Count > 0 then
    begin
      Ultimo(C).MediaFiles[0].SaveToFile(Faro);
      Writeln('1. Imagen generada: ', Faro);
    end
    else
      Writeln('1. Sin imagen: ', C.AiChat.LastError);
  finally
    C.Free;
  end;
  if not TFile.Exists(Faro) then Exit;

  // 2. Editar: la imagen adjunta al prompt es la entrada; sin 'size' conserva la proporcion
  C := Conexion('qwen-image-edit-plus');
  M := TAiMediaFile.Create;
  try
    M.LoadFromfile(Faro);
    C.AddMessageAndRun('Convierte el faro en verde y blanco, sin cambiar nada mas', 'user', [M]);
    if Ultimo(C).MediaFiles.Count > 0 then
    begin
      Ultimo(C).MediaFiles[0].SaveToFile(FaroVerde);
      Writeln('2. Imagen editada:  ', FaroVerde);
    end
    else
      Writeln('2. Sin imagen: ', C.AiChat.LastError);
  finally
    C.Free;
  end;
  if not TFile.Exists(FaroVerde) then Exit;

  // 3. Describir con un modelo de chat con vision
  C := Conexion('qwen3.8-flash');
  M := TAiMediaFile.Create;
  try
    M.LoadFromfile(FaroVerde);
    Writeln('3. Descripcion: ', Corto(C.AddMessageAndRun('De que colores es el faro? Una frase.', 'user', [M])));
  finally
    C.Free;
  end;
end;

// -----------------------------------------------------------------------------
// voz: TTS -> transcripcion -> omni
// -----------------------------------------------------------------------------

function GenerarVoz(const ATexto, AArchivo: string): Boolean;
var
  C: TAiChatConnection;
begin
  C := Conexion('qwen3-tts-flash');
  try
    C.TtsParams.Voice := 'Cherry';
    C.TtsParams.Language := 'es';
    C.AddMessageAndRun(ATexto, 'user', []);
    Result := Ultimo(C).MediaFiles.Count > 0;
    if Result then
      Ultimo(C).MediaFiles[0].SaveToFile(AArchivo)
    else
      Writeln('   Sin audio: ', C.AiChat.LastError);
  finally
    C.Free;
  end;
end;

procedure SeccionVoz;
var
  C: TAiChatConnection;
  M: TAiMediaFile;
  Wav: string;
begin
  Titulo('VOZ');
  Wav := TPath.Combine(Salida, 'voz.wav');

  // 1. Texto a voz
  if GenerarVoz('Hola, esta es una prueba de voz de MakerAI desde Delphi.', Wav) then
    Writeln('1. Voz generada: ', Wav)
  else
    Exit;

  // 2. Transcripcion: en cmTranscription el prompt es contexto para el reconocedor
  //    (nombres propios, jerga), no una pregunta
  C := Conexion('qwen3-asr-flash');
  M := TAiMediaFile.Create;
  try
    C.ChatMode := cmTranscription;
    M.LoadFromfile(Wav);
    Writeln('2. Transcripcion: ', Corto(C.AddMessageAndRun('MakerAI, Delphi', 'user', [M])));
  finally
    C.Free;
  end;

  // 3. Un modelo omni entiende el audio directamente
  C := Conexion('qwen3.8-omni-flash');
  M := TAiMediaFile.Create;
  try
    M.LoadFromfile(Wav);
    Writeln('3. Omni: ', Corto(C.AddMessageAndRun('Que dice el audio y en que idioma esta?', 'user', [M])));
  finally
    C.Free;
  end;
end;

// -----------------------------------------------------------------------------
// traduccion con qwen-mt
// -----------------------------------------------------------------------------

procedure SeccionTraduccion;
const
  TEXTO = 'La caja menor se registra en la cuenta 110510 del PUC y el IVA descontable en la 2408.';
var
  C: TAiChatConnection;
begin
  Titulo('TRADUCCION');
  C := Conexion('qwen-mt-plus');
  try
    // Idioma destino y dominio por Params; el glosario es una lista en el driver
    C.Params.Values['TranslateTo'] := 'English';
    C.Params.Values['TranslateDomain'] := 'Colombian accounting, chart of accounts (PUC)';
    (C.AiChat as TAiQwenChat).TranslateTerms.Text := 'IVA descontable=deductible VAT';
    Writeln('Original: ', TEXTO);
    Writeln('English:  ', Corto(C.AddMessageAndRun(TEXTO, 'user', [])));
    C.Params.Values['TranslateTo'] := 'Japanese';
    Writeln('Japanese: (se guarda en salida\traduccion_ja.txt; la consola no muestra japones)');
    TFile.WriteAllText(TPath.Combine(Salida, 'traduccion_ja.txt'),
      C.AddMessageAndRun(TEXTO, 'user', []), TEncoding.UTF8);
  finally
    C.Free;
  end;
end;

// -----------------------------------------------------------------------------
// rag: embeddings + reranker
// -----------------------------------------------------------------------------

function Coseno(const A, B: TAiEmbeddingData): Double;
var
  I: Integer;
  AB, AA, BB: Double;
begin
  AB := 0; AA := 0; BB := 0;
  for I := 0 to Min(High(A), High(B)) do
  begin
    AB := AB + A[I] * B[I];
    AA := AA + A[I] * A[I];
    BB := BB + B[I] * B[I];
  end;
  if (AA = 0) or (BB = 0) then Exit(0);
  Result := AB / (Sqrt(AA) * Sqrt(BB));
end;

procedure SeccionRag;
const
  CONSULTA = 'Donde se registra la caja menor?';
  PASAJES: array[0..3] of string = (
    'La caja menor se registra en la cuenta 110510 del PUC.',
    'El IVA descontable va en la cuenta 2408.',
    'Los bancos nacionales usan la subcuenta 111005.',
    'Receta de arepas con queso.');
var
  E: TAiEmbeddingConnection;
  R: TAiQwenRAGReranker;
  Q: TAiEmbeddingData;
  Scores: TArray<Double>;
  Textos: TArray<string>;
  I: Integer;
begin
  Titulo('RAG');
  Writeln('Consulta: ', CONSULTA);

  // 1. Embeddings: similitud coseno de cada pasaje con la consulta
  E := TAiEmbeddingConnection.Create(nil);
  try
    E.DriverName := 'Qwen'; // text-embedding-v4, 1024 dimensiones
    Q := Copy(E.CreateEmbedding(CONSULTA, 'user'));
    Writeln(Format('1. Embeddings (%s, %d dimensiones):', [E.AiEmbeddings.Model, Length(Q)]));
    for I := 0 to High(PASAJES) do
      Writeln(Format('   %.3f  %s', [Coseno(Q, E.CreateEmbedding(PASAJES[I], 'user')), PASAJES[I]]));
  finally
    E.Free;
  end;

  // 2. Reranker: puntaje de relevancia 0..1 por pasaje (se enchufa en TAiRAGVector.Reranker)
  R := TAiQwenRAGReranker.Create(nil);
  try
    R.Instruct := 'Given an accounting question, retrieve the chart-of-accounts passage that answers it';
    SetLength(Textos, Length(PASAJES));
    for I := 0 to High(PASAJES) do
      Textos[I] := PASAJES[I];
    Scores := R.Score(CONSULTA, Textos);
    Writeln('2. Reranker (qwen3-rerank):');
    for I := 0 to High(PASAJES) do
      Writeln(Format('   %.3f  %s', [Scores[I], PASAJES[I]]));
  finally
    R.Free;
  end;
end;

// -----------------------------------------------------------------------------
// realtime: LLM -> voz, y traduccion simultanea por el conector universal
// -----------------------------------------------------------------------------

procedure SeccionRealtime;
var
  TTS: TAiQwenRealtimeTTS;
  C: TAiChatConnection;
  RT: TAiRealtimeConnection;
  Wav, Pcm: TBytes;
  Chunk, Silencio: TBytes;
  I: Integer;
  VozWav: string;
begin
  Titulo('REALTIME');

  // 1. LLM -> voz: cada fragmento que escribe el modelo va directo al TTS, que
  //    empieza a hablar antes de que el modelo termine
  TTS := TAiQwenRealtimeTTS.Create(nil);
  try
    TTS.Voice := 'Cherry';
    TTS.Language := 'es';
    TTS.OnSessionReady := Demo.OnListo;
    TTS.OnAudioChunk := Demo.OnAudio;
    TTS.OnFinished := Demo.OnTerminado;
    Demo.Reiniciar;
    Demo.TTS := TTS;
    TTS.Connect;
    Esperar(function: Boolean begin Result := Demo.Listo end, 15000);
    if not Demo.Listo then
    begin
      Writeln('1. El TTS en tiempo real no se conecto');
      Exit;
    end;
    C := Conexion('qwen3.8-flash', True);
    try
      C.OnReceiveData := Demo.OnDeltaAVoz;
      C.OnReceiveDataEnd := Demo.OnFinAVoz;
      Demo.T0 := TThread.GetTickCount;
      Write('1. LLM -> voz: ');
      C.AddMessageAndRun('Explica en dos frases cortas que es una cuenta contable.', 'user', []);
      Esperar(function: Boolean begin Result := Demo.Terminado end);
      Writeln;
      GuardarWav(TPath.Combine(Salida, 'llm_voz.wav'), Demo.Audio, 24000);
      Writeln(Format('   primer audio a los %d ms de la pregunta; %.1f s de voz en salida\llm_voz.wav',
        [Demo.PrimerAudioMs, Demo.Audio.Size / 48000]));
    finally
      C.Free;
    end;
  finally
    TTS.Free;
  end;

  // 2. Traduccion simultanea por TAiRealtimeConnection: el mismo codigo sirve
  //    con otros drivers; lo propio de Qwen va en DriverParams
  VozWav := TPath.Combine(Salida, 'voz.wav');
  if not TFile.Exists(VozWav) and not GenerarVoz('Hola, esta es una prueba de voz de MakerAI desde Delphi.', VozWav) then
    Exit;
  Wav := TFile.ReadAllBytes(VozWav);
  Pcm := Copy(Wav, 44, MaxInt); // WAV PCM16 24 kHz: se salta la cabecera
  RT := TAiRealtimeConnection.Create(nil);
  try
    RT.DriverName := 'QwenTranslate';
    RT.DriverParams.Values['TargetLanguage'] := 'en';
    RT.DriverParams.Values['Voice'] := 'Tina';
    RT.InputSampleRate := 24000; // el conector remuestrea a los 16 kHz del modelo
    RT.OnSessionReady := Demo.OnListo;
    RT.OnTranscriptCompleted := Demo.OnOriginal;
    RT.OnAssistantText := Demo.OnTraduccion;
    RT.OnAudioChunk := Demo.OnAudio;
    RT.OnError := Demo.OnRtError;
    Demo.Reiniciar;
    RT.Connect;
    Esperar(function: Boolean begin Result := Demo.Listo or (Demo.Error <> '') end, 15000);
    if not Demo.Listo then
    begin
      Writeln('2. La traduccion en tiempo real no se conecto: ', Demo.Error);
      Exit;
    end;
    // Se envia como si fuera un microfono: bloques de 100 ms y luego silencio
    // (este modelo cierra el turno tras ~2.5 s sin voz)
    I := 0;
    while I < Length(Pcm) do
    begin
      Chunk := Copy(Pcm, I, 4800);
      RT.SendAudioChunk(Chunk);
      Inc(I, 4800);
      Esperar(function: Boolean begin Result := False end, 100);
    end;
    SetLength(Silencio, 4800);
    FillChar(Silencio[0], Length(Silencio), 0);
    for I := 1 to 30 do
    begin
      RT.SendAudioChunk(Silencio);
      Esperar(function: Boolean begin Result := False end, 100);
    end;
    Esperar(function: Boolean begin Result := Demo.Terminado end, 30000);
    RT.Disconnect;
    Writeln('2. Traduccion simultanea (QwenTranslate):');
    Writeln('   escuchado: ', Demo.Original);
    Writeln('   traducido: ', Demo.Traduccion);
    GuardarWav(TPath.Combine(Salida, 'traduccion_en.wav'), Demo.Audio, 24000);
    Writeln(Format('   voz traducida: %.1f s en salida\traduccion_en.wav', [Demo.Audio.Size / 48000]));
  finally
    RT.Free;
  end;
end;

// -----------------------------------------------------------------------------
// opcionales
// -----------------------------------------------------------------------------

procedure SeccionVideo;
var
  C: TAiChatConnection;
  Archivo: string;
begin
  Titulo('VIDEO (Wan)');
  Archivo := TPath.Combine(Salida, 'video.mp4');
  C := Conexion('wan2.6-t2v');
  try
    C.VideoParams.Params.Values['duration'] := '2'; // 720p por defecto en el driver
    Writeln('Generando 2 s de video (tarda ~1 minuto)...');
    C.AddMessageAndRun('Olas rompiendo contra un faro al atardecer', 'user', []);
    if Ultimo(C).MediaFiles.Count > 0 then
    begin
      Ultimo(C).MediaFiles[0].SaveToFile(Archivo);
      Writeln('Video con audio: ', Archivo);
    end
    else
      Writeln('Sin video: ', C.AiChat.LastError);
  finally
    C.Free;
  end;
end;

procedure SeccionVoces;
var
  V: TAiQwenVoices;
  Id, Muestra, Archivo: string;
  C: TAiChatConnection;
begin
  Titulo('VOCES PROPIAS');
  // Clonar solo voces con permiso de su dueno. Aqui la muestra es la voz sintetica del TTS.
  Muestra := TPath.Combine(Salida, 'voz.wav');
  if not TFile.Exists(Muestra) and not GenerarVoz('Hola, esta es una prueba de voz de MakerAI desde Delphi.', Muestra) then
    Exit;
  V := TAiQwenVoices.Create(nil);
  try
    Id := V.CloneVoice(Muestra, 'demo089', 'es');
    Writeln('Voz clonada: ', Id);
    try
      C := Conexion('qwen3-tts-flash'); // el driver cambia al modelo que exige la voz
      try
        C.TtsParams.Voice := Id;
        C.AddMessageAndRun('Esta es la voz clonada, hablando desde el demo de MakerAI.', 'user', []);
        if Ultimo(C).MediaFiles.Count > 0 then
        begin
          Archivo := TPath.Combine(Salida, 'voz_clonada.wav');
          Ultimo(C).MediaFiles[0].SaveToFile(Archivo);
          Writeln('Audio con la voz clonada: ', Archivo);
        end;
      finally
        C.Free;
      end;
    finally
      V.DeleteVoice(Id); // las voces propias quedan en la cuenta hasta borrarlas
      Writeln('Voz borrada de la cuenta');
    end;
  finally
    V.Free;
  end;
end;

// -----------------------------------------------------------------------------

var
  I: Integer;
begin
  ExitCode := 0;
  if GetEnvironmentVariable('DASHSCOPE_API_KEY') = '' then
  begin
    Writeln('Falta la variable de entorno DASHSCOPE_API_KEY');
    ExitCode := 2;
    Exit;
  end;
  Salida := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\salida'));
  ForceDirectories(Salida);

  Secciones := TStringList.Create;
  Demo := TDemo.Create;
  try
    for I := 1 to ParamCount do
      Secciones.Add(LowerCase(ParamStr(I)).Replace('--', ''));
    // Sin secciones (o solo las opcionales): todas las baratas
    var SoloOpcionales := True;
    for var LSec in Secciones do
      if (LSec <> 'video') and (LSec <> 'voces') then
        SoloOpcionales := False;
    if SoloOpcionales then
      Secciones.AddStrings(['chat', 'vision', 'voz', 'traduccion', 'rag', 'realtime']);
    try
      if Quiere('chat') then SeccionChat;
      if Quiere('vision') then SeccionVision;
      if Quiere('voz') then SeccionVoz;
      if Quiere('traduccion') then SeccionTraduccion;
      if Quiere('rag') then SeccionRag;
      if Quiere('realtime') then SeccionRealtime;
      if Quiere('video') then SeccionVideo;
      if Quiere('voces') then SeccionVoces;
    except
      on E: Exception do
      begin
        Writeln('ERROR: ', E.ClassName, ': ', E.Message);
        ExitCode := 2;
      end;
    end;
    Writeln;
    Writeln('Archivos generados en: ', Salida);
  finally
    Demo.Free;
    Secciones.Free;
  end;
end.
