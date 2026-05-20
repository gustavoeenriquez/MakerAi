program NativeTranscription;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 10-STT / 01-NativeTranscription
// =============================================================================
// Prueba la transcripci?n nativa de audio (InternalRunNativeTranscription) para
// los tres drivers que implementaron el override en mayo 2026:
//
//   - Groq     : whisper-large-v3 / whisper-large-v3-turbo
//                (endpoint OpenAI-compatible: /audio/transcriptions)
//   - Mistral  : voxtral-mini-latest / voxtral-small-latest
//                (endpoint OpenAI-compatible: /audio/transcriptions)
//   - Cohere   : cohere-transcribe-03-2026
//                (endpoint /v2/audio/transcriptions; language obligatorio)
//
// Como referencia/baseline se prueba tambi?n OpenAI whisper-1.
//
// Configuraci?n clave:
//   ChatMode = cmTranscription  -->  RunNew va directo a InternalRunTranscription
//                                    sin pasar por InternalRunCompletions.
//   Sin SpeechTool asignado     -->  InternalRunTranscription llama al override
//                                    nativo del driver (no al tool externo).
//   OnError captura errores     -->  RunNew swallows exceptions internamente
//                                    y los reporta v?a el evento OnError.
//
// Uso:
//   NativeTranscription.exe [ruta_audio]
//
// Formatos soportados: WAV, MP3, OGG, M4A, FLAC, WEBM (seg?n provider).
// Variables de entorno: OPENAI_API_KEY  GROQ_API_KEY  MISTRAL_API_KEY  COHERE_API_KEY
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

// ---------------------------------------------------------------------------
const
  SEP  = '------------------------------------------------------------';
  WIDE = '============================================================';

// ---------------------------------------------------------------------------
// TErrorCapture — captura el OnError de TAiChatConnection
// RunNew atrapa todas las exceptions internamente y las redirige a OnError.
// ---------------------------------------------------------------------------
type
  TErrorCapture = class
  private
    FMsg: String;
  public
    procedure OnError(Sender: TObject; const ErrorMsg: string;
      AEx: Exception; const AResp: IHTTPResponse);
    function HasError: Boolean;
    procedure Clear;
    property Msg: String read FMsg;
  end;

procedure TErrorCapture.OnError(Sender: TObject; const ErrorMsg: string;
  AEx: Exception; const AResp: IHTTPResponse);
begin
  FMsg := ErrorMsg;
end;

function TErrorCapture.HasError: Boolean;
begin
  Result := FMsg <> '';
end;

procedure TErrorCapture.Clear;
begin
  FMsg := '';
end;

// ---------------------------------------------------------------------------
// TTestResult — registro de resultados para el resumen final
// ---------------------------------------------------------------------------
type
  TTestResult = record
    Driver : String;
    Model  : String;
    Tag    : String; // escenario corto (ej: 'lang=es', 'vacio')
    OK     : Boolean;
    Output : String; // primeros 120 chars de la transcripci?n o el error
  end;

var
  GResults : array of TTestResult;

procedure AddResult(const ADriver, AModel, ATag: String; AOK: Boolean;
  const AOutput: String);
var
  R: TTestResult;
begin
  R.Driver := ADriver;
  R.Model  := AModel;
  R.Tag    := ATag;
  R.OK     := AOK;
  R.Output := Copy(AOutput, 1, 120);
  SetLength(GResults, Length(GResults) + 1);
  GResults[High(GResults)] := R;
end;

// ---------------------------------------------------------------------------
// TestTranscription
//   AResponseFormat : '' = default del driver | 'json' | 'verbose_json' | 'text'
//   ALanguage       : '' = no forzar  | 'es' | 'en' | ...
// ---------------------------------------------------------------------------
function TestTranscription(const ADriver, AModel, AAudioFile,
  ALanguage, AResponseFormat: String): Boolean;
var
  Conn : TAiChatConnection;
  EC   : TErrorCapture;
  MF   : TAiMediaFile;
  Resp : String;
  Tag  : String;
begin
  Result := False;
  Tag    := '';
  if ALanguage <> '' then Tag := Tag + 'lang=' + ALanguage + ' ';
  if AResponseFormat <> '' then Tag := Tag + 'fmt=' + AResponseFormat;
  Tag := Trim(Tag);

  Write('  ', ADriver, '/', AModel);
  if Tag <> '' then Write(' [', Tag, ']');
  Write(' ... ');

  EC   := TErrorCapture.Create;
  Conn := TAiChatConnection.Create(nil);
  try
    // --- Configuraci?n b?sica ---
    // IMPORTANTE: OnError se asigna ANTES de DriverName, igual que AudioBridge.
    Conn.OnError                       := EC.OnError;
    Conn.DriverName                    := ADriver;
    Conn.Params.Values['Model']        := AModel;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.ChatMode                      := cmTranscription;
    // Sin SpeechTool: el driver cae en InternalRunNativeTranscription

    // --- Par?metros de transcripci?n ---
    if ALanguage <> '' then
      Conn.TranscriptionParams.Language := ALanguage;
    if AResponseFormat <> '' then
      Conn.TranscriptionParams.ResponseFormat := AResponseFormat;

    // --- Archivo de audio ---
    // AddMessageAndRun transfiere la propiedad de MF al historial interno.
    // Conn.Free libera el historial y con ?l todos los TAiMediaFile.
    MF := TAiMediaFile.Create;
    MF.LoadFromFile(AAudioFile);

    // --- Llamada ---
    Resp := Conn.AddMessageAndRun('', 'user', [MF]);

    if EC.HasError then
    begin
      Writeln('FAIL');
      Writeln('    ! ', Copy(EC.Msg, 1, 300));
      AddResult(ADriver, AModel, Tag, False, EC.Msg);
    end
    else if Resp = '' then
    begin
      Writeln('VACIA');
      AddResult(ADriver, AModel, Tag, False, '[respuesta vacia]');
    end
    else
    begin
      Writeln('OK');
      Writeln('    > ', Copy(Resp, 1, 100));
      AddResult(ADriver, AModel, Tag, True, Resp);
      Result := True;
    end;

  finally
    Conn.Free;
    EC.Free;
  end;
end;

// ---------------------------------------------------------------------------
// TestErrorCase: archivo vac?o — debe reportar error v?a OnError
// RunNew atrapa la exception y llama OnError (no re-raise).
// ---------------------------------------------------------------------------
procedure TestErrorCase(const ADriver, AModel: String);
var
  Conn : TAiChatConnection;
  EC   : TErrorCapture;
  MF   : TAiMediaFile;
  Resp : String;
begin
  Write('  ', ADriver, '/', AModel, ' [archivo vacio] ... ');

  EC   := TErrorCapture.Create;
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.OnError                       := EC.OnError;
    Conn.DriverName                    := ADriver;
    Conn.Params.Values['Model']        := AModel;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.ChatMode                      := cmTranscription;

    // MF vac?o (sin LoadFromFile). RunNew detecta Content.Size=0 y llama OnError.
    MF   := TAiMediaFile.Create;
    Resp := Conn.AddMessageAndRun('', 'user', [MF]);

    if EC.HasError then
      Writeln('OK (error capturado: ', Copy(EC.Msg, 1, 60), ')')
    else if Resp = '' then
      Writeln('OK (respuesta vacia — validaci?n previa al driver)')
    else
      Writeln('SIN ERROR (inesperado — respuesta: "', Copy(Resp, 1, 40), '")');
  finally
    Conn.Free;
    EC.Free;
  end;
end;

// ---------------------------------------------------------------------------
// MAIN
// ---------------------------------------------------------------------------
var
  AudioPath : String;
  HasAudio  : Boolean;
  I         : Integer;
  OK, Total : Integer;
begin
  Writeln(WIDE);
  Writeln('  MakerAI — 10-STT / 01-NativeTranscription');
  Writeln(WIDE);
  Writeln;
  Writeln('Drivers: OpenAI (baseline), Groq, Mistral, Cohere');
  Writeln('Hook:    InternalRunNativeTranscription override');
  Writeln('Ruta:    ChatMode=cmTranscription → InternalRunTranscription → override');
  Writeln;

  // --- Ruta del archivo de audio ---
  HasAudio := False;
  if ParamCount >= 1 then
    AudioPath := Trim(ParamStr(1))
  else
  begin
    Write('Ruta del archivo de audio (WAV/MP3/OGG, Enter para omitir): ');
    Readln(AudioPath);
    AudioPath := Trim(AudioPath);
  end;

  if AudioPath <> '' then
  begin
    if not FileExists(AudioPath) then
    begin
      Writeln('ERROR: Archivo no encontrado: "', AudioPath, '"');
      Write('Presiona Enter...');
      Readln;
      Halt(1);
    end;
    HasAudio := True;
    Writeln('Audio: ', ExtractFileName(AudioPath),
            '  (', TFile.GetSize(AudioPath), ' bytes)');
    Writeln;
  end;

  // ===========================================================================
  // BLOQUE 0: Caso de error — archivo vac?o
  // Verifica que el driver detecta el problema y reporta v?a OnError.
  // ===========================================================================
  Writeln(SEP);
  Writeln('BLOQUE 0: Manejo de error (archivo sin contenido)');
  Writeln(SEP);
  TestErrorCase('OpenAI',  'whisper-1');
  TestErrorCase('Groq',    'whisper-large-v3');
  TestErrorCase('Mistral', 'voxtral-mini-latest');
  TestErrorCase('Cohere',  'cohere-transcribe-03-2026');
  Writeln;

  if not HasAudio then
  begin
    Writeln(SEP);
    Writeln('INFO: Sin archivo de audio — bloques 1-4 omitidos.');
    Writeln('      Pasa la ruta como argumento:');
    Writeln('        NativeTranscription.exe D:\ruta\audio.wav');
    Writeln(SEP);
  end
  else
  begin
    // =========================================================================
    // BLOQUE 1: OpenAI — baseline (funcionaba antes del PR)
    // =========================================================================
    Writeln(SEP);
    Writeln('BLOQUE 1: OpenAI (baseline)');
    Writeln(SEP);
    TestTranscription('OpenAI', 'whisper-1', AudioPath, '',   '');
    TestTranscription('OpenAI', 'whisper-1', AudioPath, 'en', 'verbose_json');
    Writeln;

    // =========================================================================
    // BLOQUE 2: Groq — whisper OpenAI-compatible
    // =========================================================================
    Writeln(SEP);
    Writeln('BLOQUE 2: Groq (whisper — OpenAI-compatible)');
    Writeln(SEP);
    TestTranscription('Groq', 'whisper-large-v3',       AudioPath, '',   '');
    TestTranscription('Groq', 'whisper-large-v3',       AudioPath, 'en', 'verbose_json');
    TestTranscription('Groq', 'whisper-large-v3-turbo', AudioPath, '',   '');
    Writeln;

    // =========================================================================
    // BLOQUE 3: Mistral — Voxtral OpenAI-compatible
    // =========================================================================
    Writeln(SEP);
    Writeln('BLOQUE 3: Mistral (Voxtral — OpenAI-compatible)');
    Writeln(SEP);
    TestTranscription('Mistral', 'voxtral-mini-latest', AudioPath, '',   '');
    TestTranscription('Mistral', 'voxtral-mini-latest', AudioPath, 'es', 'verbose_json');
    TestTranscription('Mistral', 'voxtral-mini-latest', AudioPath, 'en', 'text');
    Writeln;

    // =========================================================================
    // BLOQUE 4: Cohere
    // Diferencias: language obligatorio (driver usa 'en' por defecto),
    // response_format no soportado (se ignora en el override — no debe explotar).
    // =========================================================================
    Writeln(SEP);
    Writeln('BLOQUE 4: Cohere (language obligatorio, sin response_format)');
    Writeln(SEP);
    // Sin language → override usa 'en' autom?ticamente
    TestTranscription('Cohere', 'cohere-transcribe-03-2026', AudioPath, '',   '');
    // Language expl?cito
    TestTranscription('Cohere', 'cohere-transcribe-03-2026', AudioPath, 'en', '');
    TestTranscription('Cohere', 'cohere-transcribe-03-2026', AudioPath, 'es', '');
    // response_format ignorado por Cohere — el override no lo env?a; no debe fallar
    TestTranscription('Cohere', 'cohere-transcribe-03-2026', AudioPath, 'en', 'verbose_json');
    Writeln;
  end;

  // ===========================================================================
  // RESUMEN FINAL
  // ===========================================================================
  Writeln(WIDE);
  Writeln('RESUMEN');
  Writeln(WIDE);
  OK    := 0;
  Total := Length(GResults);
  for I := 0 to Total - 1 do
  begin
    if GResults[I].OK then
    begin
      Write('  [OK]   ', GResults[I].Driver, '/', GResults[I].Model);
      if GResults[I].Tag <> '' then Write(' [', GResults[I].Tag, ']');
      Writeln;
      Inc(OK);
    end
    else
    begin
      Write('  [FAIL] ', GResults[I].Driver, '/', GResults[I].Model);
      if GResults[I].Tag <> '' then Write(' [', GResults[I].Tag, ']');
      Writeln(' — ', GResults[I].Output);
    end;
  end;
  Writeln;
  Writeln('Resultado: ', OK, '/', Total, ' transcripciones OK');
  Writeln(WIDE);

  Writeln;
  Write('Presiona Enter para salir...');
  Readln;
end.
