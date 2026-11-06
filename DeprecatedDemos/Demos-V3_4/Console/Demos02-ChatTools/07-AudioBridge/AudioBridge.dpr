program AudioBridge;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 02-ChatTools / 07-AudioBridge
// =============================================================================
// Demuestra los modos de entrada de audio con gemma4 (Ollama local):
//
//   ESCENARIO 1 — Audio nativo (cap_Audio en ModelCaps → sin bridge)
//     gemma4:e4b / gemma4:e2b tienen cap_Audio nativo registrado.
//     Gap = SessionCaps − ModelCaps = [] → el audio se envía directamente
//     al modelo vía la API de Ollama, sin pasar por Gemini.
//
//   ESCENARIO 2 — STT Bridge (forzar ModelCaps=[] → Gemini transcribe → gemma4 responde)
//     Sobreescribir ModelCaps=[] en tiempo de ejecución produce
//     Gap=[cap_Audio] → Fase 1 activa TAiGeminiSpeechTool.ExecuteTranscription.
//     El audio se transcribe con Gemini 2.0-flash; gemma4 recibe texto plano.
//     Útil cuando Ollama no acepta audio en la API aunque el modelo lo soporte.
//
//   ESCENARIO 3 — TTS directo (texto → Gemini TTS → WAV)
//     Usa TAiGeminiSpeechTool.GenerateSpeech() directamente.
//
//   ESCENARIO 4 — Pipeline completo (audio → STT bridge → gemma4 → TTS)
//
// Requisitos:
//   - Ollama corriendo en http://localhost:11434 con gemma4:e4b y gemma4:e2b
//   - Variable de entorno GEMINI_API_KEY con una clave válida
//   - Archivo de audio WAV/MP3/OGG/M4A (primer argumento o se pide interactivo)
//
// Uso:
//   AudioBridge.exe [ruta_audio] [pregunta]
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Net.HttpClient,
  TypInfo,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Ollama,
  uMakerAi.Gemini.Speech;

// ---------------------------------------------------------------------------
const
  OLLAMA_URL  = 'http://localhost:11434/';
  E4B_MODEL   = 'gemma4:e4b';
  E2B_MODEL   = 'gemma4:e2b';
  GEMINI_KEY  = '@GEMINI_API_KEY';
  TTS_VOICE   = 'Puck';
  SEP         = '------------------------------------------------------------';

// ---------------------------------------------------------------------------
type
  TErrorCapture = class
  private
    FMsg: String;
    FCode: Integer;
  public
    procedure OnError(Sender: TObject; const ErrorMsg: string;
      AEx: Exception; const AResp: IHTTPResponse);
    function HasError: Boolean;
    procedure Clear;
    property Msg: String read FMsg;
    property Code: Integer read FCode;
  end;

procedure TErrorCapture.OnError(Sender: TObject; const ErrorMsg: string;
  AEx: Exception; const AResp: IHTTPResponse);
begin
  FMsg  := ErrorMsg;
  FCode := 0;
  if Assigned(AResp) then FCode := AResp.StatusCode;
end;

function TErrorCapture.HasError: Boolean;
begin
  Result := FMsg <> '';
end;

procedure TErrorCapture.Clear;
begin
  FMsg := '';
  FCode := 0;
end;

// ---------------------------------------------------------------------------
procedure PrintResult(const Title, Text: String);
begin
  Writeln;
  Writeln('=== ', Title, ' ===');
  if Text = '' then
    Writeln('  [SIN RESULTADO]')
  else
    Writeln(Text);
end;

procedure PrintError(const EC: TErrorCapture);
begin
  Writeln('  [ERROR] ', EC.Msg);
  if EC.Code > 0 then
    Writeln('  HTTP ', EC.Code);
end;

// ---------------------------------------------------------------------------
// Crea una conexión Ollama.
//   ForceSttBridge=True  → ModelCaps=[] fuerza el bridge Gemini STT
//   ForceSttBridge=False → ModelCaps del registro (gemma4 incluye cap_Audio nativo)
// ---------------------------------------------------------------------------
function MakeOllamaConn(const AModel: String;
  ForceSttBridge: Boolean;
  EC: TErrorCapture;
  SpeechTool: TAiGeminiSpeechTool): TAiChatConnection;
var
  Conn: TAiChatConnection;
begin
  Conn := TAiChatConnection.Create(nil);

  // IMPORTANTE: SpeechTool y OnError se asignan ANTES de DriverName.
  // DriverName dispara SetupChatFromDriver → ApplyParamsToChat, que copia
  // Conn.FChatTools al FChat interno. Si SpeechTool se asigna después,
  // FChat.FChatTools.FSpeechTool queda nil y el STT bridge falla.
  Conn.SpeechTool := SpeechTool;
  Conn.OnError := EC.OnError;

  Conn.DriverName := 'Ollama';
  Conn.Params.Values['URL']          := OLLAMA_URL;
  Conn.Params.Values['Model']        := AModel;
  Conn.Params.Values['Asynchronous'] := 'False';
  Conn.Params.Values['Max_Tokens']   := '1024';
  if ForceSttBridge then
  begin
    // Gap=[cap_Audio] → Fase 1 activa TAiGeminiSpeechTool.ExecuteTranscription
    Conn.Params.Values['ModelCaps']    := '[]';
    Conn.Params.Values['SessionCaps']  := '[cap_Audio]';
    Conn.Params.Values['Tool_Active']  := 'False';
  end
  else
  begin
    // gemma4 registrado con cap_Audio nativo → se usan valores del registro
    // Nota: Ollama no serializa audio en la API → el modelo recibirá texto vacío
    Conn.Params.Values['Tool_Active']  := 'True';
  end;
  Result := Conn;
end;

// ===========================================================================
// ESCENARIO 1: Audio nativo — gemma4 recibe el archivo directamente
// ===========================================================================
function RunNativeAudio(const AModel, AAudioFile, AQuestion: String;
  SpeechTool: TAiGeminiSpeechTool): String;
var
  Conn      : TAiChatConnection;
  AudioFile : TAiMediaFile;
  EC        : TErrorCapture;
begin
  Result := '';
  Writeln;
  Writeln('>>> ESCENARIO 1 — Audio nativo');
  Writeln('    Modelo  : ', AModel, ' (cap_Audio en ModelCaps → sin STT bridge)');
  Writeln('    Audio   : ', AAudioFile);
  Writeln('    Pregunta: ', AQuestion);
  Writeln(SEP);

  EC   := TErrorCapture.Create;
  Conn := MakeOllamaConn(AModel, False, EC, SpeechTool);
  try
    // ModelCaps del registro incluye cap_Audio → Gap = [] → audio va directo
    // a Ollama sin pasar por Gemini
    AudioFile := TAiMediaFile.Create;
    AudioFile.LoadFromFile(AAudioFile);
    Writeln('  Cargado: ', AudioFile.Content.Size, ' bytes  MIME: ', AudioFile.MimeType);
    Writeln('  Nota: Ollama serializa audio como imagen en la API multimodal.');
    Writeln('        Si el modelo responde "adjunta el audio", la API de Ollama');
    Writeln('        no soporta audio nativo — usar STT bridge (Escenario 2).');
    Writeln('  Enviando audio directamente a ', AModel, ' via Ollama...');

    Result := Conn.AddMessageAndRun(AQuestion, 'user', [AudioFile]);

    if EC.HasError then
      PrintError(EC)
    else
      PrintResult('Respuesta de ' + AModel + ' (nativo)', Result);
  finally
    Conn.Free;
    EC.Free;
  end;
end;

// ===========================================================================
// ESCENARIO 2: STT Bridge — Gemini transcribe → gemma4 responde en texto
// ===========================================================================
function RunSTTBridge(const AModel, AAudioFile, AQuestion: String;
  SpeechTool: TAiGeminiSpeechTool): String;
var
  Conn      : TAiChatConnection;
  AudioFile : TAiMediaFile;
  EC        : TErrorCapture;
begin
  Result := '';
  Writeln;
  Writeln('>>> ESCENARIO 2 — STT Bridge (Gemini transcribe → ', AModel, ')');
  Writeln('    ModelCaps=[]  /  SessionCaps=[cap_Audio]  →  Gap=[cap_Audio]');
  Writeln('    Audio   : ', AAudioFile);
  Writeln('    Pregunta: ', AQuestion);
  Writeln(SEP);

  EC   := TErrorCapture.Create;
  Conn := MakeOllamaConn(AModel, True, EC, SpeechTool);
  try
    // Gap=[cap_Audio] activa:
    //   Fase 1 → ExecuteTranscription(Gemini 2.0-flash) → AskMsg.Prompt aumentado
    //   Fase 3 → InternalRunCompletions → gemma4 recibe solo texto
    AudioFile := TAiMediaFile.Create;
    AudioFile.LoadFromFile(AAudioFile);
    Writeln('  Cargado: ', AudioFile.Content.Size, ' bytes  MIME: ', AudioFile.MimeType);
    Writeln('  Transcribiendo con Gemini y enviando texto a ', AModel, '...');

    Result := Conn.AddMessageAndRun(AQuestion, 'user', [AudioFile]);

    if EC.HasError then
      PrintError(EC)
    else
      PrintResult('Respuesta de ' + AModel + ' (STT bridge)', Result);
  finally
    Conn.Free;
    EC.Free;
  end;
end;

// ===========================================================================
// ESCENARIO 3: TTS Directo — texto → Gemini TTS → archivo WAV
// ===========================================================================
procedure RunTTSDirect(const AText, AOutFile: String);
var
  Audio: TAiMediaFile;
begin
  Writeln;
  Writeln('>>> ESCENARIO 3 — TTS Directo (Gemini TTS → WAV)');
  Writeln('    Texto : ', Copy(AText, 1, 80), IfThen(Length(AText) > 80, '...', ''));
  Writeln('    Salida: ', AOutFile);
  Writeln('    Voz   : ', TTS_VOICE);
  Writeln(SEP);
  Writeln('  Generando audio con Gemini TTS...');

  Audio := TAiGeminiSpeechTool.GenerateSpeech(
    GetEnvironmentVariable('GEMINI_API_KEY'),
    AText, TTS_VOICE, nil, nil, nil);

  if Assigned(Audio) then
  begin
    try
      Audio.Content.SaveToFile(AOutFile);
      Writeln('  Guardado: ', AOutFile, '  (', Audio.Content.Size, ' bytes)');
    finally
      Audio.Free;
    end;
  end
  else
    Writeln('  [ERROR] Gemini TTS no devolvió audio. Verifica GEMINI_API_KEY.');
end;

// ===========================================================================
// ESCENARIO 4: Pipeline completo — audio → STT bridge → gemma4 → TTS
// ===========================================================================
procedure RunFullPipeline(const AModel, AAudioFile, AQuestion: String;
  SpeechTool: TAiGeminiSpeechTool);
var
  ModelResponse : String;
  OutFile       : String;
begin
  Writeln;
  Writeln('>>> ESCENARIO 4 — Pipeline completo (audio → STT → ', AModel, ' → TTS)');
  Writeln(SEP);

  ModelResponse := RunSTTBridge(AModel, AAudioFile, AQuestion, SpeechTool);

  if ModelResponse = '' then
  begin
    Writeln('  [SKIP] No hay respuesta del LLM para convertir a audio.');
    Exit;
  end;

  OutFile := ExtractFilePath(ParamStr(0)) + 'pipeline_respuesta.wav';
  RunTTSDirect(ModelResponse, OutFile);
end;

// ===========================================================================
// MAIN
// ===========================================================================
var
  AudioPath, Question : String;
  SpeechTool          : TAiGeminiSpeechTool;
  HasAudio            : Boolean;
begin
  Writeln('=== MakerAI — AudioBridge Demo (gemma4) ===');
  Writeln('Modelos Ollama : ', E4B_MODEL, '  /  ', E2B_MODEL);
  Writeln('Bridge STT     : TAiGeminiSpeechTool (gemini-2.0-flash)');
  Writeln('TTS            : TAiGeminiSpeechTool (gemini-2.5-flash-preview-tts)');
  Writeln;

  try
    SpeechTool := TAiGeminiSpeechTool.Create(nil);
    try
      SpeechTool.ApiKey              := GEMINI_KEY;
      SpeechTool.TranscriptionModel  := 'gemini-2.0-flash';
      SpeechTool.TranscriptionPrompt := 'Transcribe este audio con precisión. ' +
                                        'Si es español, transcribe en español.';
      SpeechTool.Model               := 'gemini-2.5-flash-preview-tts';
      SpeechTool.Voice               := TTS_VOICE;

      // -- Ruta del archivo de audio -------------------------------------------
      HasAudio := False;
      if ParamCount >= 1 then
        AudioPath := Trim(ParamStr(1))
      else
      begin
        Write('Ruta del archivo de audio (Enter para omitir): ');
        Readln(AudioPath);
        AudioPath := Trim(AudioPath);
      end;

      if AudioPath <> '' then
      begin
        if not FileExists(AudioPath) then
          raise Exception.CreateFmt('Archivo no encontrado: "%s"', [AudioPath]);
        HasAudio := True;
      end;

      // -- Pregunta ------------------------------------------------------------
      if ParamCount >= 2 then
        Question := ParamStr(2)
      else if HasAudio then
        Question := 'Resume brevemente el contenido del audio.'
      else
        Question := '';

      // -----------------------------------------------------------------------
      // Escenario 3: TTS (siempre disponible, no requiere audio de entrada)
      // -----------------------------------------------------------------------
      RunTTSDirect(
        'Hola! Soy gemma4, un modelo de lenguaje local corriendo en Ollama. ' +
        'Estoy siendo probado con el sistema de puentes de audio de MakerAI.',
        ExtractFilePath(ParamStr(0)) + 'tts_demo.wav');

      // -----------------------------------------------------------------------
      // Escenarios con audio
      // -----------------------------------------------------------------------
      if HasAudio then
      begin
        Writeln;
        Writeln('=== PRUEBAS CON AUDIO: ', ExtractFileName(AudioPath), ' ===');

        // -- Escenario 1: Audio nativo con e4b y e2b --
        RunNativeAudio(E4B_MODEL, AudioPath, Question, SpeechTool);
        RunNativeAudio(E2B_MODEL, AudioPath, Question, SpeechTool);

        // -- Escenario 2: STT Bridge con e4b y e2b --
        RunSTTBridge(E4B_MODEL, AudioPath, Question, SpeechTool);
        RunSTTBridge(E2B_MODEL, AudioPath, Question, SpeechTool);

        // -- Escenario 4: Pipeline completo con e4b --
        RunFullPipeline(E4B_MODEL, AudioPath, Question, SpeechTool);
      end
      else
      begin
        Writeln;
        Writeln(SEP);
        Writeln('INFO: No se proporcionó archivo de audio.');
        Writeln('      Los Escenarios 1, 2 y 4 requieren un archivo de audio.');
        Writeln('      Pasa la ruta como primer argumento:');
        Writeln('        AudioBridge.exe D:\ruta\audio.wav');
      end;

    finally
      SpeechTool.Free;
    end;

  except
    on E: Exception do
    begin
      Writeln;
      Writeln('FATAL: ', E.ClassName, ': ', E.Message);
    end;
  end;

  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
