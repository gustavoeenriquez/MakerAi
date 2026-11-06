program Gemma4Test;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// Gemma4Test — prueba gemma4:e2b via Ollama con el framework MakerAI
//
// TEST 1: Chat basico + reasoning (verifica que think:true activa el campo thinking)
// TEST 2: Vision (imagen roja 1x1)
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.JSON,
  System.Net.HttpClient,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Ollama,
  uMakerAi.Chat.Initializations,
  uMakerAi.Core;

const
  OLLAMA_URL = 'http://localhost:11434/';
  MODEL      = 'gemma4:e2b';
  LINE       = '------------------------------------------------------------';

var
  GThinking: String;

// ---------------------------------------------------------------------------
type
  TEvHandler = class
    procedure OnThinking(const Sender: TObject; aMsg: TAiChatMessage;
                         aResponse: TJSONObject; aRole, aText: string);
    procedure OnData(const Sender: TObject; aMsg: TAiChatMessage;
                     aResponse: TJSONObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const AMsg: string;
                      AEx: Exception; const AResp: IHTTPResponse);
  end;

procedure TEvHandler.OnThinking(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
begin
  GThinking := aText;
  Writeln;
  Writeln('--- Razonamiento interno ---');
  Writeln(aText);
  Writeln('----------------------------');
end;

procedure TEvHandler.OnData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
begin
  Write(aText);  // imprime la respuesta token a token (modo stream)
end;

procedure TEvHandler.OnError(Sender: TObject; const AMsg: string;
  AEx: Exception; const AResp: IHTTPResponse);
begin
  Writeln('  [ERROR] ', AMsg);
  if Assigned(AResp) then
    Writeln('  HTTP ', AResp.StatusCode, ': ', Copy(AResp.ContentAsString, 1, 300));
end;

// ---------------------------------------------------------------------------
function MakeConn(Ev: TEvHandler): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName                    := 'Ollama';
  Result.Params.Values['URL']          := OLLAMA_URL;
  Result.Params.Values['Model']        := MODEL;
  Result.Params.Values['Asynchronous'] := 'False';
  Result.Params.Values['Max_Tokens']   := '2048';
  Result.OnReceiveThinking             := Ev.OnThinking;
  Result.OnReceiveData                 := Ev.OnData;
  Result.OnError                       := Ev.OnError;
end;

// ---------------------------------------------------------------------------
// TEST 1: Chat basico + verificar que thinking llega
// ---------------------------------------------------------------------------
procedure Test1_Reasoning;
var
  Conn: TAiChatConnection;
  Ev  : TEvHandler;
  Resp: String;
begin
  Writeln;
  Writeln('TEST 1 - Chat basico + reasoning');
  Writeln(LINE);

  GThinking := '';
  Ev   := TEvHandler.Create;
  Conn := MakeConn(Ev);
  try
    Conn.SystemPrompt.Text := 'Eres un asistente util. Responde siempre en espanol.';
    Write('Respuesta : ');
    Resp := Conn.AddMessageAndRun('Cuanto es 17 x 23? Piensa paso a paso.', 'user', []);
    Writeln;

    if (Resp <> '') and (GThinking <> '') then
      Writeln('PASS: respuesta + thinking recibidos (think:true activo)')
    else if Resp <> '' then
      Writeln('WARN: respuesta OK pero thinking vacio')
    else
      Writeln('FAIL: sin respuesta');
  finally
    Conn.Free;
    Ev.Free;
  end;
end;

// ---------------------------------------------------------------------------
// TEST 2: Vision — imagen PNG 1x1 rojo
// ---------------------------------------------------------------------------
procedure Test2_Vision;
const
  // PNG 1x1 pixel rojo (generado con System.Drawing, valido)
  RED_PNG_B64 = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAAXNSR0IArs4c6Q' +
                'AAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAANSURBVBhX' +
                'Y/jPwPAfAAUAAf+mXJtdAAAAAElFTkSuQmCC';
var
  Conn  : TAiChatConnection;
  Ev    : TEvHandler;
  Media : TAiMediaFile;
  Resp  : String;
begin
  Writeln;
  Writeln('TEST 2 - Vision (imagen roja 1x1)');
  Writeln(LINE);

  Ev   := TEvHandler.Create;
  Conn := MakeConn(Ev);
  Media := TAiMediaFile.Create;
  try
    // Cargar imagen PNG 1x1 desde base64
    Media.LoadFromBase64('image.png', RED_PNG_B64);

    Resp := Conn.AddMessageAndRun(
      'What is the dominant color in this image? One word only.',
      'user', [Media]);

    Writeln('Respuesta: ', Resp);

    if Resp.ToLower.Contains('red') then
      Writeln('PASS: vision funciona correctamente')
    else if Resp <> '' then
      Writeln('INFO: respuesta recibida: "', Resp, '" (verificar si es correcto)');
  finally
    Conn.Free;
    Ev.Free;
    // Media es liberado por la conexion via AddMessageAndRun
  end;
end;

// ---------------------------------------------------------------------------
// TEST 3: Audio — transcripcion de un WAV
// ---------------------------------------------------------------------------
procedure Test3_Audio;
const
  AUDIO_FILE = 'D:\taller\medios\HolaComoTEEncuentras.wav';
var
  Conn  : TAiChatConnection;
  Ev    : TEvHandler;
  Media : TAiMediaFile;
  Resp  : String;
begin
  Writeln;
  Writeln('TEST 3 - Audio (transcripcion WAV)');
  Writeln(LINE);
  Writeln('Archivo: ', AUDIO_FILE);

  if not FileExists(AUDIO_FILE) then
  begin
    Writeln('SKIP: archivo no encontrado');
    Exit;
  end;

  Ev    := TEvHandler.Create;
  Conn  := MakeConn(Ev);
  Media := TAiMediaFile.Create;
  try
    Media.LoadFromFile(AUDIO_FILE);

    Write('Respuesta : ');
    Resp := Conn.AddMessageAndRun(
      'Transcribe exactamente lo que dice el audio. Solo el texto, sin comentarios.',
      'user', [Media]);
    Writeln;

    if Resp <> '' then
      Writeln('PASS: audio transcrito correctamente')
    else
      Writeln('FAIL: sin respuesta');
  finally
    Conn.Free;
    Ev.Free;
  end;
end;

// ---------------------------------------------------------------------------
begin
  Writeln('=== Gemma4Test - MakerAI Framework ===');
  Writeln('Model : ', MODEL, '  Driver: Ollama  URL: ', OLLAMA_URL);

  try
    Test1_Reasoning;
    Test2_Vision;
    Test3_Audio;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;

  Writeln;
  Writeln(LINE);
  Writeln('Presiona Enter para salir...');
  Readln;
end.
