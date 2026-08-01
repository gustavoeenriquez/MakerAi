// Demo de TRADUCTOR BIDIRECCIONAL en tiempo real (consola)
// =========================================================
//
// Captura simultaneamente:
//   [ELLOS] el audio que el sistema REPRODUCE (WASAPI loopback) -> lo que dice
//           el otro lado de la videollamada (Zoom, Meet, Teams, YouTube...)
//   [YO]    el microfono (WASAPI) -> lo que dices tu
//
// Cada flujo se envia a un Realtime STT de OpenAI (VAD en el servidor) y cada
// transcripcion completada se traduce con TAiChatConnection.
//
//   loopback --> TAiAudioCapture --> TAiOpenAiRealtimeSTT --+
//                                                           +--> TAiChatConnection --> consola
//   microfono -> TAiAudioCapture --> TAiOpenAiRealtimeSTT --+
//
// Requisitos:
//   - Variable de entorno OPENAI_API_KEY (para STT y, por defecto, traduccion).
//   - AURICULARES recomendados: con altavoces el microfono captura tambien el
//     audio remoto y se duplican las transcripciones (eco acustico).
//
// El traductor es intercambiable: cambia TRANSLATOR_DRIVER / TRANSLATOR_MODEL /
// TRANSLATOR_URL para usar Ollama u otro proveedor (p.ej. un servidor local).

program TranslatorDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Threading,
  Winapi.Windows,
  uMakerAi.Utils.AudioCapture,
  uMakerAi.Realtime,
  uMakerAi.Realtime.OpenAI,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.OpenAi; // importa y auto-registra el driver 'OpenAi' (v3.4)

const
  // ==========================================================================
  // CONFIGURACION
  // ==========================================================================

  // A que idioma traducir cada lado
  TARGET_LANG_REMOTE = 'espanol'; // lo que dice el otro lado -> espanol
  TARGET_LANG_LOCAL = 'ingles'; // lo que digo yo -> ingles

  // Pista de idioma para el STT (ISO-639-1: 'en', 'es'... vacio = autodetectar)
  STT_LANG_REMOTE = '';
  STT_LANG_LOCAL = '';

  // Proveedor de traduccion (cualquier driver registrado de MakerAI).
  // Para Ollama local: DRIVER='Ollama', MODEL='gpt-oss:20b', URL='http://192.168.3.121:11434/'
  TRANSLATOR_DRIVER = 'OpenAi';
  TRANSLATOR_MODEL = 'gpt-4o-mini';
  TRANSLATOR_URL = ''; // vacio = URL por defecto del driver
  TRANSLATOR_APIKEY = '@OPENAI_API_KEY';

  STT_APIKEY = '@OPENAI_API_KEY';
  // Modelo de la sesion Realtime. OJO: el default del driver
  // (gpt-4o-realtime-preview) fue retirado por OpenAI.
  STT_MODEL = 'gpt-realtime-2.1';

var
  GConsoleLock: TCriticalSection;

procedure SafeWriteLn(const aText: string);
begin
  GConsoleLock.Enter;
  try
    WriteLn(aText);
  finally
    GConsoleLock.Leave;
  end;
end;

type
  // Un "lado" del traductor: captura + STT + traduccion
  TTranslatorSide = class
  private
    FTag: string; // '[ELLOS]' / '[YO]'
    FTargetLang: string;
    FClosing: Boolean;
    FCapture: TAiAudioCapture;
    FSTT: TAiOpenAiRealtimeSTT;
    FChat: TAiChatConnection;
    FChatLock: TCriticalSection;
    procedure SttSessionReady(Sender: TObject);
    procedure SttTranscriptCompleted(Sender: TObject; const Transcript: string; const ItemId: string);
    procedure SttError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure CaptureError(Sender: TObject; const ErrorMessage: string);
    procedure Translate(const aText: string);
  public
    constructor Create(aSource: TAiAudioSource; const aTag, aTargetLang, aSttLanguage: string);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

constructor TTranslatorSide.Create(aSource: TAiAudioSource; const aTag, aTargetLang, aSttLanguage: string);
begin
  inherited Create;
  FTag := aTag;
  FTargetLang := aTargetLang;
  FClosing := False;
  FChatLock := TCriticalSection.Create;

  // --- Traductor (un chat por lado, asi las llamadas no se cruzan) ---
  FChat := TAiChatConnection.Create(nil);
  FChat.DriverName := TRANSLATOR_DRIVER;
  FChat.Model := TRANSLATOR_MODEL;
  FChat.Params.Values['Asynchronous'] := 'False';
  if TRANSLATOR_APIKEY <> '' then
    FChat.Params.Values['ApiKey'] := TRANSLATOR_APIKEY;
  if TRANSLATOR_URL <> '' then
    FChat.Params.Values['Url'] := TRANSLATOR_URL;

  // --- STT en tiempo real (VAD del servidor decide inicio/fin de frase) ---
  FSTT := TAiOpenAiRealtimeSTT.Create(nil);
  FSTT.ApiKey := STT_APIKEY;
  FSTT.Model := STT_MODEL;
  // gpt-live-transcribe (ago 2026): mejor WER con ruido de fondo, acentos y
  // terminologia; acepta contexto del dominio (prompt + idiomas esperados)
  FSTT.TranscriptionModel := otmGptLiveTranscribe;
  FSTT.TranscriptionPrompt := 'Llamada de trabajo traducida en tiempo real';
  FSTT.Language := aSttLanguage;
  if aSttLanguage = '' then
  begin
    // Autodeteccion guiada: los dos idiomas que puede traer la llamada
    FSTT.Languages.Add('en');
    FSTT.Languages.Add('es');
  end;
  FSTT.OnSessionReady := SttSessionReady;
  FSTT.OnTranscriptCompleted := SttTranscriptCompleted;
  FSTT.OnError := SttError;

  // --- Captura de audio (loopback o microfono) ---
  FCapture := TAiAudioCapture.Create(nil);
  FCapture.Source := aSource;
  FCapture.OutputSampleRate := 16000;
  FCapture.OutputChannels := 1;
  FCapture.RealtimeSTT := FSTT; // cada chunk PCM16 se reenvia al STT
  FCapture.OnError := CaptureError;
end;

destructor TTranslatorSide.Destroy;
begin
  Stop;
  // Esperar a que ninguna tarea de traduccion este usando FChat
  FChatLock.Enter;
  try
    FClosing := True;
  finally
    FChatLock.Leave;
  end;
  FCapture.Free;
  FSTT.Free;
  FChat.Free;
  FChatLock.Free;
  inherited;
end;

procedure TTranslatorSide.Start;
begin
  FSTT.Connect; // asincrono: cuando conecte, los chunks empiezan a fluir
  FCapture.Active := True;
end;

procedure TTranslatorSide.Stop;
begin
  FClosing := True;
  if Assigned(FCapture) then
    FCapture.Active := False;
  if Assigned(FSTT) and FSTT.IsConnected then
    FSTT.Disconnect;
end;

procedure TTranslatorSide.SttSessionReady(Sender: TObject);
begin
  SafeWriteLn(FTag + ' STT conectado y listo.');
end;

procedure TTranslatorSide.SttError(Sender: TObject; const ErrorMsg, ErrorCode: string);
begin
  SafeWriteLn(Format('%s ERROR STT [%s]: %s', [FTag, ErrorCode, ErrorMsg]));
end;

procedure TTranslatorSide.CaptureError(Sender: TObject; const ErrorMessage: string);
begin
  SafeWriteLn(FTag + ' ERROR captura: ' + ErrorMessage);
end;

procedure TTranslatorSide.SttTranscriptCompleted(Sender: TObject; const Transcript: string; const ItemId: string);
var
  Text: string;
begin
  Text := Trim(Transcript);
  if Text = '' then
    Exit;
  SafeWriteLn(Format('%s %s', [FTag, Text]));
  Translate(Text);
end;

procedure TTranslatorSide.Translate(const aText: string);
begin
  // Traducir en un task para no bloquear el bucle de eventos de la consola
  TTask.Run(
    procedure
    var
      Prompt, Res: string;
    begin
      try
        FChatLock.Enter;
        try
          if FClosing then
            Exit;
          FChat.NewChat; // traduccion sin estado: cada frase es independiente
          Prompt := Format('Traduce al %s el siguiente texto. Responde UNICAMENTE con la traduccion, ' +
            'sin comentarios ni comillas:'#10'%s', [FTargetLang, aText]);
          Res := FChat.AddMessageAndRun(Prompt, 'user', []);
        finally
          FChatLock.Leave;
        end;
        SafeWriteLn(Format('%s    -> %s', [FTag, Trim(Res)]));
      except
        on E: Exception do
          SafeWriteLn(Format('%s ERROR traduciendo: %s', [FTag, E.Message]));
      end;
    end);
end;

// ============================================================================

var
  Remote, Local: TTranslatorSide;
  StopRequested: Boolean;

begin
  SetConsoleOutputCP(CP_UTF8); // tildes y caracteres especiales en consola
  StopRequested := False;
  GConsoleLock := TCriticalSection.Create;
  try
    try
      WriteLn('=== Traductor bidireccional en tiempo real (MakerAI) ===');
      WriteLn;
      WriteLn('  [ELLOS] audio del sistema (loopback)  -> ' + TARGET_LANG_REMOTE);
      WriteLn('  [YO]    microfono                     -> ' + TARGET_LANG_LOCAL);
      WriteLn;
      WriteLn('  Recomendado usar AURICULARES (evita que el microfono capture');
      WriteLn('  el audio remoto y se dupliquen las transcripciones).');
      WriteLn;

      if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
      begin
        WriteLn('ERROR: la variable de entorno OPENAI_API_KEY no esta definida.');
        WriteLn('Definela y vuelve a ejecutar.');
        Exit;
      end;

      Remote := TTranslatorSide.Create(asLoopback, '[ELLOS]', TARGET_LANG_REMOTE, STT_LANG_REMOTE);
      Local := TTranslatorSide.Create(asMicrophone, '[YO]   ', TARGET_LANG_LOCAL, STT_LANG_LOCAL);
      try
        Remote.Start;
        Local.Start;

        WriteLn('Traduciendo... pulsa ENTER para terminar.');
        WriteLn;

        // ReadLn en un task; el hilo principal procesa los eventos TThread.Queue
        TTask.Run(
          procedure
          begin
            ReadLn;
            StopRequested := True;
          end);

        while not StopRequested do
          CheckSynchronize(50);

        WriteLn('Deteniendo...');
        Remote.Stop;
        Local.Stop;
        CheckSynchronize(200); // drenar eventos pendientes
      finally
        Remote.Free;
        Local.Free;
      end;
    except
      on E: Exception do
        WriteLn(E.ClassName, ': ', E.Message);
    end;
  finally
    GConsoleLock.Free;
  end;

end.
